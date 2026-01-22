const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
const process = std.process;
const StaticStringSet = std.StaticStringMap(void);

const backend = @import("backend");
const Assembly = backend.Assembly;
const IR = backend.Ir;
const Object = backend.Object;

const CodeGen = @import("CodeGen/CodeGen.zig");
const Compilation = @import("Basic/Compilation.zig");
const Diagnostics = @import("Basic/Diagnostics.zig");
const GCCVersion = @import("Driver/GCCVersion.zig");
const LangOpts = @import("Basic/LangOpts.zig");
const Lexer = @import("Lexer/Lexer.zig");
const Preprocessor = @import("Lexer/Preprocessor.zig");
const Source = @import("Basic/Source.zig");
const Target = @import("Basic/Target.zig");
const Toolchain = @import("Toolchain.zig");
const Tree = @import("AST/AST.zig");

const AsmCodeGenFn = fn (target: std.Target, tree: *const Tree) Compilation.Error!Assembly;

const PicRelatedOptions = StaticStringSet.initComptime(.{
    .{"-fpic"},
    .{"-fno-pic"},
    .{"-fPIC"},
    .{"-fno-PIC"},
    .{"-fpie"},
    .{"-fno-pie"},
    .{"-fPIE"},
    .{"-fno-PIE"},
});

pub const Linker = enum {
    ld,
    bfd,
    gold,
    lld,
    mold,
};

const Driver = @This();

comp: *Compilation,
diagnostics: *Diagnostics,

inputs: std.ArrayList(Source) = .empty,
linkObjects: std.ArrayList([]const u8) = .empty,
outputName: ?[]const u8 = null,
sysroot: ?[]const u8 = null,
resourceDir: ?[]const u8 = null,
systemDefines: Compilation.SystemDefinesMode = .IncludeSystemDefines,
tempFileCount: u32 = 0,
/// If false, do not emit line directives in -E mode
lineCommands: bool = true,
/// If true, use `#line <num>` instead of `# <num>` for line directives
useLineDirectives: bool = false,

// debug options
onlyPreprocess: bool = false,
onlySyntax: bool = false,
onlyCompile: bool = false,
onlyPreprocessAndCompile: bool = false,
dumpPP: bool = false,
dumpAst: bool = false,
dumpIR: bool = false,
dumpTokens: bool = false,
dumpRawTokens: bool = false,
dumpLinkerArgs: bool = false,
color: ?bool = true,
nobuiltininc: bool = false,
nostdinc: bool = false,
nostdlibinc: bool = false,
desiredPicLevel: ?backend.CodeGenOptions.PicLevel = null,
desiredPieLevel: ?backend.CodeGenOptions.PicLevel = null,
appleKext: bool = false,
mkernel: bool = false,
mabicalls: ?bool = null,
dynamicNopic: ?bool = null,
ropi: bool = false,
rwpi: bool = false,
cmodel: std.builtin.CodeModel = .default,
debugDumpLetters: packed struct(u3) {
    d: bool = false,
    m: bool = false,
    n: bool = false,

    /// According to GCC, specifying letters whose behavior conflicts is undefined.
    /// We follow clang in that `-dM` always takes precedence over `-dD`
    pub fn getPreprocessorDumpMode(self: @This()) Preprocessor.DumpMode {
        if (self.m) return .MacrosOnly;
        if (self.d) return .MacrosAndResult;
        if (self.n) return .MacroNamesAndResult;
        return .ResultOnly;
    }
} = .{},

/// name of the zinc executable
zincName: []const u8 = "",

/// Value of --triple= passed via CLI
rawTargetTriple: ?[]const u8 = null,
/// Non-optimizing assembly backend is currently selected by passing `-O0`
useAssemblyBackend: bool = false,

// linker options
useLinker: ?[]const u8 = null,
linkerPath: ?[]const u8 = null,
nodefaultlibs: bool = false,
nolibc: bool = false,
nostartfiles: bool = false,
nostdlib: bool = false,
pie: ?bool = null,
rdynamic: bool = false,
relocatable: bool = false,
rtlib: ?[]const u8 = null,
shared: bool = false,
sharedLibgcc: bool = false,
static: bool = false,
staticLibgcc: bool = false,
staticPie: bool = false,
strip: bool = false,
unwindlib: ?[]const u8 = null,

pub fn deinit(d: *Driver) void {
    for (d.linkObjects.items[d.linkObjects.items.len - d.tempFileCount ..]) |obj| {
        std.fs.deleteFileAbsolute(obj) catch {};
        d.comp.gpa.free(obj);
    }
    d.inputs.deinit(d.comp.gpa);
    d.linkObjects.deinit(d.comp.gpa);
    d.* = undefined;
}

const usage =
    \\Usage {s}: [options] file..
    \\
    \\General Options:
    \\  --help      Print this message
    \\  --version   Print Zinc version
    \\ 
    \\Compile Options:
    \\  -c, --compile           Only run preprocess, compile, and assemble steps
    \\  -dM                     Output #define directives for all the macros defined during the execution of the preprocessor
    \\  -dD                     Like -dM except that it outputs both the #define directives and the result of preprocessing
    \\  -dN                     Like -dD, but emit only the macro names, not their expansions.
    \\  -D <macro>=<value>      Define <macro> to <value> (defaults to 1)
    \\  -E                      Only run the preprocessor 
    \\  -fapple-kext            Use Apple's kernel extensions ABI
    \\  -fchar8_t               Enable char8_t (enabled by default in C23 and later)
    \\  -fno-char8_t            Disable char8_t (disabled by default for pre-C23)
    \\  -fcolor-diagnostics     Enable colors in diagnostics
    \\  -fno-color-diagnostics  Disable colors in diagnostics
    \\  -fcommon                Place uninitialized global variables in a common block
    \\  -fno-common             Place uninitialized global variables in the BSS section of the object file
    \\  -fdeclspec              Enable support for __declspec attributes
    \\  -fno-declspec           Disable support for __declspec attributes
    \\  -ffp-eval-method=[source|double|extended]
    \\                          Evaluation method to use for floating-point arithmetic
    \\  -ffreestanding          Compilation in a freestanding environment
    \\  -fgnuc-version=<value>  Controls value of __GNUC__ and related macros. Set to 0 or empty to disable them.
    \\  -fgnu-inline-asm        Enable GNU style inline asm (default: enabled)
    \\  -fno-gnu-inline-asm     Disable GNU style inline asm
    \\  -fhosted                Compilation in a hosted environment
    \\  -fms-extensions         Enable support for Microsoft extensions
    \\  -fno-ms-extensions      Disable support for Microsoft extensions
    \\  -fdollars-in-identifiers        
    \\                          Allow '$' in identifiers(default)
    \\  -fno-dollars-in-identifiers     
    \\                          Disallow '$' in identifiers
    \\  -g                      Generate debug information
    \\  -fshort-enums           Use the narrowest possible integer type for enums.
    \\  -fno-short-enums        Use "int" as the tag type for enums.
    \\  -fsigned-char           "char" is signed
    \\  -fno-signed-char        "char" is unsigned
    \\  -fsyntax-only           Only run the preprocessor parser, and semantic analysis stages
    \\  -funsigned-char         "char" is unsigned
    \\  -fno-unsigned-char      "char" is signed
    \\  -fuse-line-directives   Use `#line <num>` linemarkers in preprocessed output
    \\  -fno-use-line-directives
    \\                          Use `# <num>` linemarkers in preprocessed output
    \\  -fmacro-backtrace-limit=<limit>
    \\                          Set limit on how many macro expansion traces are shown in errors (default 6)
    \\  -fnative-half-type      Use the native half type for __fp16 instead of promoting to float
    \\  -fnative-half-arguments-and-returns
    \\                          Allow half-precision function arguments and return values
    \\  -fpic                   Generate position-independent code (PIC) suitable for use in a shared library, if supported for the target machine
    \\  -fPIC                   Similar to -fpic but avoid any limit on the size of the global offset table
    \\  -fpie                   Similar to -fpic, but the generated position-independent code can only be linked into executables
    \\  -fPIE                   Similar to -fPIC, but the generated position-independent code can only be linked into executables
    \\  -frwpi                  Generate read-write position independent code (ARM only)
    \\  -fno-rwpi               Disable generate read-write position independent code (ARM only).
    \\  -fropi                  Generate read-only position independent code (ARM only)
    \\  -fno-ropi               Disable generate read-only position independent code (ARM only). 
    \\  -I <dir>                Add directory to include search path
    \\  -isystem  <dir>         Add directory to system include search path
    \\  --embed-dir=<dir>       Set directory for embedded files
    \\  --emulate=[clang|gcc|msvc]
    \\                          Select which C compiler to emulate (default clang)
    \\  -mabicalls              Enable SVR4-style position-independent code (Mips only)
    \\  -mno-abicalls           Disable SVR4-style position-independent code (Mips only)
    \\  -mcmodel=<code-model>   Generate code for the given code model
    \\  -mkernel                Enable kernel development mode
    \\  -nobuiltininc           Do not search the compiler's builtin directory for include files
    \\  -nostdinc, --no-standard-includes
    \\                          Do not search the standard system directories or compiler builtin directories for include files.
    \\  -nostdlibinc            Do not search the standard system directories for include files, but do search compiler builtin include directories
    \\  -o <file>               Write output to <file>
    \\  -P, --no-line-commands  Disable linemarker output in -E mode
    \\  -pedantic               Warn on language extensions
    \\  -pedantic-errors        Error on language extensions
    \\  -resource-dir=<dir>     Set directory for compiler resource files
    \\  --rtlib=<arg>           Compiler runtime library to use (libgcc or compiler-rt)
    \\  -std=<standard>         Specify language standard
    \\  -S, --assemble          Only run preprocess and compilation step
    \\ --target=<value>         Generate code for the given target
    \\  -U <macro>              Undefine <macro>
    \\  -undef                  Do not predefine any system-specific macros. Standard predefined macros remain defined.
    \\  -w                      Ignore all warnings
    \\  -Wall                   Enable all warnings
    \\  -Werror                 Treat all warnings as errors
    \\  -Werror=<warning>       Treat warning as error
    \\  -W<warning>             Enable the specified warning
    \\  -Wno-<warning>          Disable the specified warning
    \\
    \\Link options:
    \\  -fuse-ld=[bfd|gold|lld|mold]
    \\                          Use specific linker
    \\  -nodefaultlibs          Do not use the standard system libraries when linking.
    \\  -nolibc                 Do not use the C library or system libraries tightly coupled with it when linking.
    \\  -nostdlib               Do not use the standard system startup files or libraries when linking
    \\  -nostartfiles           Do not use the standard system startup files when linking.
    \\  -pie                    Produce a dynamically linked position independent executable on targets that support it.
    \\  --ld-path=<path>        Use linker specified by <path>
    \\  -r                      Produce a relocatable object as output.
    \\  -rdynamic               Pass the flag -export-dynamic to the ELF linker, on targets that support it.
    \\  -s                      Remove all symbol table and relocation information from the executable.
    \\  -shared-libgcc          On systems that provide libgcc as a shared library, force the use of the shared version
    \\  -static                 On systems that support dynamic linking, this overrides -pie and prevents linking with the shared libraries.
    \\  -static-libgcc          On systems that provide libgcc as a shared library, force the use of the static version
    \\  -static-pie             Produce a static position independent executable on targets that support it.
    \\  --unwindlib=<arg>       Unwind library to use ("none", "libgcc", or "libunwind") If not specified, will match runtime library
    \\
    \\Debug options:
    \\  -dump-pp               Dump preprocessor state
    \\  -dump-ast              Dump produced AST to stdout
    \\  -dump-ir               Dump ir to stdout
    \\  -dump-tokens           Run preprocessor, dump internal rep of tokens to stdout 
    \\  -dump-raw-tokens       Lex file in raw mode and dump raw tokens to stdout
    \\  -dump-linker-args      Dump linker arguments to stdout
    \\
    \\
;

fn option(arg: []const u8, name: []const u8) ?[]const u8 {
    if (std.mem.startsWith(u8, arg, name) and arg.len > name.len)
        return arg[name.len..];
    return null;
}

pub fn parseArgs(
    d: *Driver,
    stdOut: *std.Io.Writer,
    macroBuffer: *std.ArrayList(u8),
    args: []const []const u8,
) (Compilation.Error || std.Io.Writer.Error)!bool {
    var commentArg: []const u8 = "";
    var hosted: ?bool = null;
    var gnucVersion: []const u8 = "4.2.1"; // default value set by clang
    var picArg: []const u8 = "";
    var declspecAttrs: ?bool = null;
    var msExtensions: ?bool = null;

    var i: usize = 1;
    while (i < args.len) : (i += 1) {
        const arg = args[i];
        if (std.mem.startsWith(u8, arg, "-") and arg.len > 1) {
            if (std.mem.eql(u8, arg, "--help")) {
                try stdOut.print(usage, .{args[0]});
                try stdOut.flush();
                return true;
            } else if (std.mem.eql(u8, arg, "--version")) {
                try stdOut.writeAll(@import("backend").VersionStr ++ "\n");
                try stdOut.flush();
                return true;
            } else if (std.mem.startsWith(u8, arg, "-D")) {
                var macro = arg["-D".len..];
                if (macro.len == 0) {
                    i += 1;
                    if (i >= args.len) {
                        try d.err("expected argument after -D", .{});
                        continue;
                    }
                    macro = args[i];
                }

                var value: []const u8 = "1";
                if (std.mem.indexOfScalar(u8, macro, '=')) |some| {
                    value = macro[some + 1 ..];
                    macro = macro[0..some];
                }
                try macroBuffer.print(d.comp.gpa, "#define {s} {s}\n", .{ macro, value });
            } else if (std.mem.startsWith(u8, arg, "-U")) {
                var macro = arg["-U".len..];
                if (macro.len == 0) {
                    i += 1;
                    if (i >= args.len) {
                        try d.err("expected argument after -U", .{});
                        continue;
                    }
                    macro = args[i];
                }
                try macroBuffer.print(d.comp.gpa, "#undef {s} \n", .{macro});
            } else if (mem.eql(u8, arg, "-O")) {
                d.comp.codegenOptions.optimizationLevel = .@"0";
            } else if (mem.startsWith(u8, arg, "-O")) {
                d.comp.codegenOptions.optimizationLevel = backend.CodeGenOptions.OptimizationLevel.fromString(arg["-O".len..]) orelse {
                    try d.err("invalid optimization level '{s}'", .{arg});
                    continue;
                };
                d.useAssemblyBackend = d.comp.codegenOptions.optimizationLevel == .@"0";
            } else if (mem.eql(u8, arg, "-undef")) {
                d.systemDefines = .NoSystemDefines;
            } else if (std.mem.eql(u8, arg, "-c") or std.mem.eql(u8, arg, "--compile")) {
                d.onlyCompile = true;
            } else if (mem.eql(u8, arg, "-dD")) {
                d.debugDumpLetters.d = true;
            } else if (mem.eql(u8, arg, "-dM")) {
                d.debugDumpLetters.m = true;
            } else if (mem.eql(u8, arg, "-dN")) {
                d.debugDumpLetters.n = true;
            } else if (std.mem.eql(u8, arg, "-E")) {
                d.onlyPreprocess = true;
            } else if (std.mem.eql(u8, arg, "-P") or std.mem.eql(u8, arg, "--no-line-commands")) {
                d.lineCommands = false;
            } else if (std.mem.eql(u8, arg, "-fuse-line-directives")) {
                d.useLineDirectives = true;
            } else if (std.mem.eql(u8, arg, "-fno-use-line-directives")) {
                d.useLineDirectives = false;
            } else if (mem.eql(u8, arg, "-fapple-kext")) {
                d.appleKext = true;
            } else if (option(arg, "-mcmodel=")) |cmodel| {
                d.cmodel = std.meta.stringToEnum(std.builtin.CodeModel, cmodel) orelse
                    return d.fatal("unsupported machine code model: '{s}'", .{arg});
            } else if (mem.eql(u8, arg, "-mkernel")) {
                d.mkernel = true;
            } else if (mem.eql(u8, arg, "-mdynamic-no-pic")) {
                d.dynamicNopic = true;
            } else if (mem.eql(u8, arg, "-mabicalls")) {
                d.mabicalls = true;
            } else if (mem.eql(u8, arg, "-mno-abicalls")) {
                d.mabicalls = false;
            } else if (mem.eql(u8, arg, "-fchar8_t")) {
                d.comp.langOpts.hasChar8tOverride = true;
            } else if (mem.eql(u8, arg, "-fno-char8_t")) {
                d.comp.langOpts.hasChar8tOverride = false;
            } else if (std.mem.eql(u8, arg, "-fcolor-diagnostics")) {
                d.color = true;
            } else if (std.mem.eql(u8, arg, "-fno-color-diagnostics")) {
                d.color = false;
            } else if (mem.eql(u8, arg, "-fcommon")) {
                d.comp.codegenOptions.common = true;
            } else if (mem.eql(u8, arg, "-fno-common")) {
                d.comp.codegenOptions.common = false;
            } else if (std.mem.eql(u8, arg, "-fshort-enums")) {
                d.comp.langOpts.shortEnums = true;
            } else if (std.mem.eql(u8, arg, "-fno-short-enums")) {
                d.comp.langOpts.shortEnums = false;
            } else if (mem.eql(u8, arg, "-fsigned-char")) {
                d.comp.langOpts.setCharSignedness(.signed);
            } else if (mem.eql(u8, arg, "-fno-signed-char")) {
                d.comp.langOpts.setCharSignedness(.unsigned);
            } else if (mem.eql(u8, arg, "-funsigned-char")) {
                d.comp.langOpts.setCharSignedness(.unsigned);
            } else if (mem.eql(u8, arg, "-fno-unsigned-char")) {
                d.comp.langOpts.setCharSignedness(.signed);
            } else if (std.mem.eql(u8, arg, "-fdeclspec")) {
                declspecAttrs = true;
            } else if (std.mem.eql(u8, arg, "-fno-declspec")) {
                declspecAttrs = false;
            } else if (mem.eql(u8, arg, "-fgnu-inline-asm")) {
                d.comp.langOpts.gnuAsm = true;
            } else if (mem.eql(u8, arg, "-fno-gnu-inline-asm")) {
                d.comp.langOpts.gnuAsm = false;
            } else if (mem.eql(u8, arg, "-ffreestanding")) {
                hosted = false;
            } else if (mem.eql(u8, arg, "-fhosted")) {
                hosted = true;
            } else if (std.mem.eql(u8, arg, "-fms-extensions")) {
                msExtensions = true;
                try d.diagnostics.set("microsoft", .off);
            } else if (std.mem.eql(u8, arg, "-fno-ms-extensions")) {
                msExtensions = false;
                try d.diagnostics.set("microsoft", .warning);
            } else if (std.mem.eql(u8, arg, "-fdollars-in-identifiers")) {
                d.comp.langOpts.dollarsInIdentifiers = true;
            } else if (std.mem.eql(u8, arg, "-fno-dollars-in-identifiers")) {
                d.comp.langOpts.dollarsInIdentifiers = false;
            } else if (mem.eql(u8, arg, "-g")) {
                d.comp.codegenOptions.debug = true;
            } else if (mem.eql(u8, arg, "-g0")) {
                d.comp.codegenOptions.debug = false;
            } else if (option(arg, "-fmacro-backtrace-limit=")) |limitStr| {
                var limit = std.fmt.parseInt(u32, limitStr, 10) catch {
                    try d.err("-fmacro-backtrace-limit takes a number argument", .{});
                    continue;
                };
                if (limit == 0) limit = std.math.maxInt(u32);
                d.diagnostics.macroBacktraceLimit = limit;
            } else if (std.mem.eql(u8, arg, "-fnative-half-type")) {
                d.comp.langOpts.useNativeHalfType = true;
            } else if (std.mem.eql(u8, arg, "-fnative-half-arguments-and-returns")) {
                d.comp.langOpts.allowHalfArgsAndReturns = true;
            } else if (PicRelatedOptions.has(arg)) {
                picArg = arg;
            } else if (mem.eql(u8, arg, "-fropi")) {
                d.ropi = true;
            } else if (mem.eql(u8, arg, "-fno-ropi")) {
                d.ropi = false;
            } else if (mem.eql(u8, arg, "-frwpi")) {
                d.rwpi = true;
            } else if (mem.eql(u8, arg, "-fno-rwpi")) {
                d.rwpi = false;
            } else if (std.mem.startsWith(u8, arg, "-I")) {
                var path = arg["-I".len..];
                if (path.len == 0) {
                    i += 1;
                    if (i >= args.len) {
                        try d.err("expected argument after -I", .{});
                        continue;
                    }
                    path = args[i];
                }
                try d.comp.includeDirs.append(d.comp.gpa, path);
            } else if (std.mem.eql(u8, arg, "-fsyntax-only")) {
                d.onlySyntax = true;
            } else if (std.mem.eql(u8, arg, "-fno-syntax-only")) {
                d.onlySyntax = false;
            } else if (std.mem.eql(u8, arg, "-fgnuc-version=")) {
                gnucVersion = "0";
            } else if (option(arg, "-fgnuc-version=")) |version| {
                gnucVersion = version;
            } else if (std.mem.startsWith(u8, arg, "-isystem")) {
                var path = arg["-isystem".len..];
                if (path.len == 0) {
                    i += 1;
                    if (i >= args.len) {
                        try d.err("expected argument after -isystem", .{});
                        continue;
                    }
                    path = args[i];
                }
                try d.comp.systemIncludeDirs.append(d.comp.gpa, path);
            } else if (option(arg, "--emulate=")) |compilerStr| {
                const compiler = std.meta.stringToEnum(LangOpts.Compiler, compilerStr) orelse {
                    try d.err("invalid compiler '{s}'", .{arg});
                    continue;
                };
                d.comp.langOpts.setEmulatedCompiler(compiler);
                switch (d.comp.langOpts.emulate) {
                    .clang => try d.diagnostics.set("clang", .off),
                    .gcc => try d.diagnostics.set("gnu", .off),
                    .msvc => try d.diagnostics.set("microsoft", .off),
                }
            } else if (option(arg, "-ffp-eval-method=")) |fpMethodStr| {
                const fpEvalMethod = std.meta.stringToEnum(LangOpts.FPEvalMethod, fpMethodStr) orelse .indeterminate;
                if (fpEvalMethod == .indeterminate) {
                    try d.err("unsupported argument '{s}' to option '-ffp-eval-method='; expected 'source', 'double', or 'extended'", .{fpMethodStr});
                    continue;
                }
                d.comp.langOpts.setFpEvalMethod(fpEvalMethod);
            } else if (std.mem.startsWith(u8, arg, "-o")) {
                var filename = arg["-o".len..];
                if (filename.len == 0) {
                    i += 1;
                    if (i >= args.len) {
                        try d.err("expected argument after -o", .{});
                        continue;
                    }
                    filename = args[i];
                }
                d.outputName = filename;
            } else if (option(arg, "--sysroot=")) |sysroot| {
                d.sysroot = sysroot;
            } else if (option(arg, "--embed-dir=")) |embDir| {
                try d.comp.embedDirs.append(d.comp.gpa, embDir);
            } else if (mem.eql(u8, arg, "-resource-dir")) {
                i += 1;
                if (i >= args.len) {
                    try d.err("expected argument after -resource-dir", .{});
                    continue;
                }
                d.resourceDir = args[i];
            } else if (mem.eql(u8, arg, "-pedantic")) {
                d.diagnostics.state.extensions = .warning;
            } else if (mem.eql(u8, arg, "-pedantic-errors")) {
                d.diagnostics.state.extensions = .@"error";
            } else if (mem.eql(u8, arg, "-w")) {
                d.diagnostics.state.ignoreWarnings = true;
            } else if (option(arg, "--rtlib=")) |rtlib| {
                if (mem.eql(u8, rtlib, "compiler-rt") or mem.eql(u8, rtlib, "libgcc") or mem.eql(u8, rtlib, "platform")) {
                    d.rtlib = rtlib;
                } else {
                    try d.err("invalid runtime library name '{s}'", .{rtlib});
                }
            } else if (mem.eql(u8, arg, "-Wno-fatal-errors")) {
                d.diagnostics.state.fatalErrors = false;
            } else if (mem.eql(u8, arg, "-Wfatal-errors")) {
                d.diagnostics.state.fatalErrors = true;
            } else if (mem.eql(u8, arg, "-Wno-everything")) {
                d.diagnostics.state.enableAllWarnings = false;
            } else if (mem.eql(u8, arg, "-Weverything")) {
                d.diagnostics.state.enableAllWarnings = true;
            } else if (mem.eql(u8, arg, "-Werror")) {
                d.diagnostics.state.errorWarnings = true;
            } else if (mem.eql(u8, arg, "-Wno-error")) {
                d.diagnostics.state.errorWarnings = false;
            } else if (std.mem.eql(u8, arg, "-Wall")) {
                // d.diagnostics.setAll(.warning);
            } else if (option(arg, "-Werror=")) |errName| {
                try d.diagnostics.set(errName, .@"error");
            } else if (option(arg, "-Wno-error=")) |errName| {
                // TODO this should not set to warning if the option has not been specified.
                try d.diagnostics.set(errName, .warning);
            } else if (option(arg, "-Wno-")) |errName| {
                try d.diagnostics.set(errName, .off);
            } else if (option(arg, "-W")) |errName| {
                try d.diagnostics.set(errName, .warning);
            } else if (option(arg, "-std=")) |standard| {
                d.comp.langOpts.setStandard(standard) catch
                    try d.err("invalid standard '{s}'", .{arg});
            } else if (std.mem.eql(u8, arg, "-S") or std.mem.eql(u8, arg, "--assemble")) {
                d.onlyPreprocessAndCompile = true;
            } else if (mem.eql(u8, arg, "-target")) {
                i += 1;
                if (i >= args.len) {
                    try d.err("expected argument after -target", .{});
                    continue;
                }
                d.rawTargetTriple = args[i];
            } else if (option(arg, "--target=")) |triple| {
                d.rawTargetTriple = triple;
            } else if (std.mem.eql(u8, arg, "-dump-pp")) {
                d.dumpPP = true;
            } else if (std.mem.eql(u8, arg, "-dump-ast")) {
                d.dumpAst = true;
            } else if (std.mem.eql(u8, arg, "-dump-ir")) {
                d.dumpIR = true;
            } else if (std.mem.eql(u8, arg, "-dump-tokens")) {
                d.dumpTokens = true;
            } else if (std.mem.eql(u8, arg, "-dump-raw-tokens")) {
                d.dumpRawTokens = true;
            } else if (std.mem.eql(u8, arg, "-dump-linker-args")) {
                d.dumpLinkerArgs = true;
            } else if (mem.eql(u8, arg, "-C") or mem.eql(u8, arg, "--comments")) {
                d.comp.langOpts.preserveComments = true;
                commentArg = arg;
            } else if (mem.eql(u8, arg, "-CC") or mem.eql(u8, arg, "--comments-in-macros")) {
                d.comp.langOpts.preserveComments = true;
                d.comp.langOpts.preserveCommentsInMacros = true;
                commentArg = arg;
            } else if (option(arg, "-fuse-ld=")) |linkerName| {
                d.useLinker = linkerName;
            } else if (std.mem.eql(u8, arg, "-fuse-ld=")) {
                d.useLinker = null;
            } else if (option(arg, "--ld-path=")) |linkerPath| {
                d.linkerPath = linkerPath;
            } else if (mem.eql(u8, arg, "-r")) {
                d.relocatable = true;
            } else if (mem.eql(u8, arg, "-shared")) {
                d.shared = true;
            } else if (mem.eql(u8, arg, "-shared-libgcc")) {
                d.sharedLibgcc = true;
            } else if (mem.eql(u8, arg, "-static")) {
                d.static = true;
            } else if (mem.eql(u8, arg, "-static-libgcc")) {
                d.staticLibgcc = true;
            } else if (mem.eql(u8, arg, "-static-pie")) {
                d.staticPie = true;
            } else if (mem.eql(u8, arg, "-pie")) {
                d.pie = true;
            } else if (mem.eql(u8, arg, "-no-pie") or mem.eql(u8, arg, "-nopie")) {
                d.pie = false;
            } else if (mem.eql(u8, arg, "-rdynamic")) {
                d.rdynamic = true;
            } else if (mem.eql(u8, arg, "-s")) {
                d.strip = true;
            } else if (mem.eql(u8, arg, "-nodefaultlibs")) {
                d.nodefaultlibs = true;
            } else if (mem.eql(u8, arg, "-nolibc")) {
                d.nolibc = true;
            } else if (mem.eql(u8, arg, "-nobuiltininc")) {
                d.nobuiltininc = true;
            } else if (mem.eql(u8, arg, "-nostdinc") or mem.eql(u8, arg, "--no-standard-includes")) {
                d.nostdinc = true;
            } else if (mem.eql(u8, arg, "-nostdlibinc")) {
                d.nostdlibinc = true;
            } else if (mem.eql(u8, arg, "-nostdlib")) {
                d.nostdlib = true;
            } else if (mem.eql(u8, arg, "-nostartfiles")) {
                d.nostartfiles = true;
            } else if (option(arg, "--unwindlib=")) |unwindlib| {
                const validUnwindlibs: [5][]const u8 = .{ "", "none", "platform", "libunwind", "libgcc" };
                for (validUnwindlibs) |name| {
                    if (mem.eql(u8, name, unwindlib)) {
                        d.unwindlib = unwindlib;
                        break;
                    }
                } else {
                    try d.err("invalid unwind library name  '{s}'", .{unwindlib});
                }
            } else {
                try d.warn("unknown argument '{s}'", .{arg});
            }
        } else if (std.mem.endsWith(u8, arg, ".o") or std.mem.endsWith(u8, arg, ".obj")) {
            try d.linkObjects.append(d.comp.gpa, arg);
        } else {
            const file = d.addSource(arg) catch |er| {
                return d.fatal("unable to add source file '{s}': {s}", .{ arg, errorDescription(er) });
            };
            try d.inputs.append(d.comp.gpa, file);
        }
    }

    if (d.rawTargetTriple) |triple| triple: {
        const query = std.Target.Query.parse(.{ .arch_os_abi = triple }) catch {
            try d.err("invalid target '{s}'", .{triple});
            d.rawTargetTriple = null;
            break :triple;
        };
        const target = std.zig.system.resolveTargetQuery(d.comp.io, query) catch |e| {
            return d.fatal("unable to resolve target: {s}", .{errorDescription(e)});
        };
        d.comp.target = .fromZigTarget(target);
        d.comp.langOpts.setEmulatedCompiler(Target.systemCompiler(&d.comp.target));
        switch (d.comp.langOpts.emulate) {
            .clang => try d.diagnostics.set("clang", .off),
            .gcc => try d.diagnostics.set("gnu", .off),
            .msvc => try d.diagnostics.set("microsoft", .off),
        }
    }

    if (d.comp.langOpts.preserveComments and !d.onlyPreprocess)
        return d.fatal("invalid argument '{s}' only allowed with '-E'", .{commentArg});

    if (hosted) |isHosted| {
        if (isHosted and d.shared) {
            if (d.comp.target.os.tag == .freestanding) {
                return d.fatal("Cannot use freestanding target with `-fhosted`", .{});
            }
        } else {
            d.comp.target.os.tag = .freestanding;
        }
    }

    const version = GCCVersion.parse(gnucVersion);
    if (version.major == -1) {
        return d.fatal("invalid value '{0s}' in '-fgnuc-version={0s}'", .{gnucVersion});
    }
    d.comp.langOpts.gnucVersion = version.toUnsigned();

    const picLevel, const isPie = try d.getPICMode(picArg);
    d.comp.codegenOptions.picLevel = picLevel;
    d.comp.codegenOptions.isPie = isPie;

    if (declspecAttrs) |some| d.comp.langOpts.declSpecAttrs = some;
    if (msExtensions) |some| d.comp.langOpts.setMSExtensions(some);

    return false;
}

fn addSource(d: *Driver, path: []const u8) !Source {
    if (std.mem.eql(u8, "-", path)) {
        return d.comp.addSourceFromFile(.stdin(), "<stdin>", .User);
    }
    return d.comp.addSourceFromPath(path);
}

pub fn err(d: *Driver, fmt: []const u8, args: anytype) Compilation.Error!void {
    var sf = std.heap.stackFallback(1024, d.comp.gpa);
    var allocating: std.Io.Writer.Allocating = .init(sf.get());
    defer allocating.deinit();

    Diagnostics.formatArgs(&allocating.writer, fmt, args) catch return error.OutOfMemory;
    try d.diagnostics.add(.{ .kind = .@"error", .text = allocating.written(), .location = null });
}

pub fn warn(d: *Driver, fmt: []const u8, args: anytype) Compilation.Error!void {
    var sf = std.heap.stackFallback(1024, d.comp.gpa);
    var allocating: std.Io.Writer.Allocating = .init(sf.get());
    defer allocating.deinit();

    Diagnostics.formatArgs(&allocating.writer, fmt, args) catch return error.OutOfMemory;
    try d.diagnostics.add(.{ .kind = .warning, .text = allocating.written(), .location = null });
}

pub fn unsupportedOptionForTarget(d: *Driver, target: *const Target, opt: []const u8) Compilation.Error!void {
    try d.err(
        "unsupported option '{s}' for target '{s}-{s}-{s}'",
        .{ opt, @tagName(target.cpu.arch), @tagName(target.os.tag), @tagName(target.abi) },
    );
}

pub fn fatal(d: *Driver, comptime fmt: []const u8, args: anytype) error{ FatalError, OutOfMemory } {
    var sf = std.heap.stackFallback(1024, d.comp.gpa);
    var allocating: std.Io.Writer.Allocating = .init(sf.get());
    defer allocating.deinit();

    Diagnostics.formatArgs(&allocating.writer, fmt, args) catch return error.OutOfMemory;
    try d.diagnostics.add(.{ .kind = .@"fatal error", .text = allocating.written(), .location = null });
    unreachable;
}

pub fn printDiagnosticsStats(d: *Driver) void {
    const warnings = d.diagnostics.warnings;
    const errors = d.diagnostics.errors;

    const ws: []const u8 = if (warnings == 1) "" else "s";
    const es: []const u8 = if (errors == 1) "" else "s";
    if (errors != 0 and warnings != 0) {
        std.debug.print("{d} warning{s} and {d} error{s} generated.\n", .{ warnings, ws, errors, es });
    } else if (warnings != 0) {
        std.debug.print("{d} warning{s} generated.\n", .{ warnings, ws });
    } else if (errors != 0) {
        std.debug.print("{d} error{s} generated.\n", .{ errors, es });
    }
}

pub fn detectConfig(d: *Driver, file: std.fs.File) std.Io.tty.Config {
    if (d.color == true) return .escape_codes;
    if (d.color == false) return .no_color;

    if (file.supportsAnsiEscapeCodes()) return .escape_codes;
    if (@import("builtin").os.tag == .windows and file.isTty()) {
        var info: std.os.windows.CONSOLE_SCREEN_BUFFER_INFO = undefined;
        if (std.os.windows.kernel32.GetConsoleScreenBufferInfo(file.handle, &info) != std.os.windows.TRUE) {
            return .no_color;
        }
        return .{
            .windows_api = .{
                .handle = file.handle,
                .reset_attributes = info.wAttributes,
            },
        };
    }

    return .no_color;
}

pub fn errorDescription(e: anyerror) []const u8 {
    return switch (e) {
        error.OutOfMemory => "ran out of memory",
        error.FileNotFound => "file not found",
        error.IsDir => "is a directory",
        error.NotDir => "is not a directory",
        error.NotOpenForReading => "file is not open for reading",
        error.NotOpenForWriting => "file is not open for writing",
        error.InvalidUtf8 => "input is not valid UTF-8",
        error.FileBusy => "file is busy",
        error.NameTooLong => "file name is too long",
        error.AccessDenied => "access denied",
        error.FileTooBig => "file is too big",
        error.ProcessFdQuotaExceeded, error.SystemFdQuotaExceeded => "ran out of file descriptors",
        error.SystemResources => "ran out of system resources",
        error.FatalError => "a fatal error occurred",
        error.Unexpected => "an unexpected error occurred",
        else => @errorName(e),
    };
}

/// The entry point of the Zinc compiler.
/// **MAY call `exit` if `fast_exit` is set.**
pub fn main(
    d: *Driver,
    tc: *Toolchain,
    args: []const []const u8,
    comptime fastExit: bool,
    asmGenFn: ?AsmCodeGenFn,
) Compilation.Error!void {
    const userDefinedMacros = macros: {
        var macroBuffer: std.ArrayList(u8) = .empty;
        defer macroBuffer.deinit(d.comp.gpa);

        var stdoutBuffer: [256]u8 = undefined;
        var stdout = std.fs.File.stdout().writer(&stdoutBuffer);
        if (parseArgs(d, &stdout.interface, &macroBuffer, args) catch |er| switch (er) {
            error.WriteFailed => return d.fatal("failed to write to stdout: {s}", .{errorDescription(er)}),
            error.OutOfMemory => return error.OutOfMemory,
            error.FatalError => return error.FatalError,
        }) return;

        if (macroBuffer.items.len > std.math.maxInt(u32)) {
            return d.fatal("user provided macro source exceeded max size", .{});
        }

        const contents = try macroBuffer.toOwnedSlice(d.comp.gpa);
        errdefer d.comp.gpa.free(contents);

        break :macros try d.comp.addSourceFromOwnedBuffer("<command line>", contents, .User);
    };

    const linking = !(d.onlyPreprocess or d.onlySyntax or d.onlyCompile or d.onlyPreprocessAndCompile);

    if (d.inputs.items.len == 0) {
        return d.fatal("no input files", .{});
    } else if ((d.inputs.items.len != 1 and d.outputName != null) and !linking) {
        return d.fatal("cannot specify -o when generating multiple output files", .{});
    }

    if (!linking)
        for (d.linkObjects.items) |obj|
            try d.err("{s}: linker input file unused because linking not done", .{obj});

    tc.discover() catch |er| switch (er) {
        error.OutOfMemory => return error.OutOfMemory,
        error.TooManyMultilibs => return d.fatal("found more than one multilib with the same priority", .{}),
    };

    tc.defineSystemIncludes() catch |er| switch (er) {
        error.OutOfMemory => return error.OutOfMemory,
        error.ZincIncludeNotFound => return d.fatal("unable to find Zinc builtin headers", .{}),
    };

    const builtinMacros = d.comp.generateBuiltinMacros(d.systemDefines) catch |er| switch (er) {
        error.FileTooBig => return d.fatal("builtin macro source exceeded max size", .{}),
        else => |e| return e,
    };

    if (fastExit and d.inputs.items.len == 1) {
        try d.processSource(tc, d.inputs.items[0], builtinMacros, userDefinedMacros, fastExit);
        unreachable;
    }

    for (d.inputs.items) |source| {
        try d.processSource(tc, source, builtinMacros, userDefinedMacros, fastExit, asmGenFn);
    }

    if (d.diagnostics.errors != 0) {
        if (fastExit) d.exitWithCleanup(1);
        return;
    }

    if (linking)
        try d.invokeLinker(tc, fastExit);

    if (fastExit)
        std.process.exit(0);
}

fn getRandomFilename(d: *Driver, buffer: *[std.fs.max_name_bytes]u8, extension: []const u8) ![]const u8 {
    const randomBytesCount = 12;
    const subPathLen = comptime std.fs.base64_encoder.calcSize(randomBytesCount);

    var randomBytes: [randomBytesCount]u8 = undefined;
    std.crypto.random.bytes(&randomBytes);
    var randomName: [subPathLen]u8 = undefined;
    _ = std.fs.base64_encoder.encode(&randomName, &randomBytes);

    const fmtTemplate = "/tmp/{s}{s}";
    const fmtArgs = .{
        @as([]const u8, &randomName),
        extension,
    };
    return std.fmt.bufPrint(buffer, fmtTemplate, fmtArgs) catch return d.fatal("Filename too long for filesystem: " ++ fmtTemplate, fmtArgs);
}

/// If it's used, buf will either hold a filename or `/tmp/<12 random bytes with base-64 encoding>.<extension>`
/// both of which should fit into max_name_bytes for all systems
fn getOutFileName(d: *Driver, source: Source, buffer: *[std.fs.max_name_bytes]u8) ![]const u8 {
    if (d.onlyCompile or d.onlyPreprocessAndCompile) {
        const fmtTemplate = "{s}{s}";
        const fmtArgs = .{
            std.fs.path.stem(source.path),
            if (d.onlyPreprocessAndCompile) ".s" else d.comp.target.ofmt.fileExt(d.comp.target.cpu.arch),
        };
        return d.outputName orelse
            std.fmt.bufPrint(buffer, fmtTemplate, fmtArgs) catch return d.fatal("Filename too long for filesystem: " ++ fmtTemplate, fmtArgs);
    }

    return d.getRandomFilename(buffer, d.comp.target.ofmt.fileExt(d.comp.target.cpu.arch));
}

fn invokeAssembler(d: *Driver, tc: *Toolchain, inputPath: []const u8, outputPath: []const u8) !void {
    var assemblerPathBuffer: [std.fs.max_path_bytes]u8 = undefined;
    const assemblerPath = try tc.getAssemblerPath(&assemblerPathBuffer);
    const argv = [_][]const u8{ assemblerPath, inputPath, "-o", outputPath };

    var child = std.process.Child.init(&argv, d.comp.gpa);
    // TODO handle better
    child.stdin_behavior = .Inherit;
    child.stdout_behavior = .Inherit;
    child.stderr_behavior = .Inherit;

    const term = child.spawnAndWait() catch |er| {
        return d.fatal("unable to spawn linker: {s}", .{errorDescription(er)});
    };
    switch (term) {
        .Exited => |code| if (code != 0) {
            const e = d.fatal("assembler exited with an error code", .{});
            return e;
        },
        else => {
            const e = d.fatal("assembler crashed", .{});
            return e;
        },
    }
}

fn processSource(
    d: *Driver,
    tc: *Toolchain,
    source: Source,
    builtinMacro: Source,
    userDefinedMacros: Source,
    comptime fastExit: bool,
    asmGenFn: ?AsmCodeGenFn,
) !void {
    d.comp.generatedBuffer.items.len = 0;
    const prevErrors = d.comp.diagnostics.errors;

    var pp = try Preprocessor.initDefault(d.comp);
    defer pp.deinit();

    if (d.comp.langOpts.msExtensions)
        d.comp.msCwdSourceId = source.id;

    const dumpNode = d.debugDumpLetters.getPreprocessorDumpMode();

    if (d.dumpPP) pp.verbose = true;
    if (d.onlyPreprocess) {
        pp.preserveWhitespace = true;
        if (d.lineCommands)
            pp.linemarkers = if (d.useLineDirectives) .LineDirectives else .NumericDirectives;

        switch (dumpNode) {
            .MacrosAndResult, .MacroNamesAndResult => pp.storeMacroTokens = true,
            .ResultOnly, .MacrosOnly => {},
        }
    }

    if (d.dumpRawTokens)
        _ = try pp.tokenize(source)
    else
        try pp.preprocessSources(&.{ source, builtinMacro, userDefinedMacros });

    if (d.onlyPreprocess) {
        d.printDiagnosticsStats();

        if (d.diagnostics.errors != prevErrors) {
            if (fastExit) std.process.exit(1); // not linking, no need for cleanup
            return;
        }

        const file = if (d.outputName) |some|
            std.fs.cwd().createFile(some, .{}) catch |er|
                return d.fatal("unable to create output file '{s}': {s}", .{ some, errorDescription(er) })
        else
            std.fs.File.stdout();
        defer if (d.outputName != null) file.close();

        var fileBuffer: [4096]u8 = undefined;
        var fileWriter = file.writer(&fileBuffer);

        pp.prettyPrintTokens(&fileWriter.interface, dumpNode) catch
            return d.fatal("unable to write result: {s}", .{errorDescription(fileWriter.err.?)});

        if (fastExit)
            std.process.exit(0); // Not linking, no need for clean up.

        return;
    }

    if (d.dumpTokens or d.dumpRawTokens) {
        const locs = pp.tokens.items(.loc);
        var lexer = Lexer{
            .buffer = d.comp.getSource(locs[0].id).buffer,
            .langOpts = d.comp.langOpts,
            .index = locs[0].byteOffset,
            .source = .generated,
        };
        for (pp.tokens.items(.id), 0..) |*tok, i| {
            const loc = locs[i];
            const res = if (d.dumpTokens) lexer.nextNoWsAndNewLine() else lexer.next();
            const s = pp.comp.getSource(loc.id);
            std.debug.print("{d:^5} {s:<15} [line: {d}, col: {d}, range:<{d}, {d}>], info=`{s}`\n", .{
                i,
                @tagName(tok.*),
                loc.line,
                s.getLineCol(locs[i]).col,
                res.start,
                res.end,
                if (tok.* == .NewLine) "nl" else lexer.buffer[res.start..res.end],
            });
        }
        return;
    }

    var tree = try pp.parse();
    defer tree.deinit();

    if (d.dumpAst) {
        // for (tree.nodes.items(.tag), 0..) |nodeTag, i|
        //     std.debug.print("{d}: {s}\n", .{ i, @tagName(nodeTag) });

        var stdoutBuffer: [4096]u8 = undefined;
        var stdout = std.fs.File.stdout().writer(&stdoutBuffer);
        tree.dump(d.detectConfig(stdout.file), &stdout.interface) catch {};
    }

    d.printDiagnosticsStats();

    // do not compile if there were errors
    if (d.diagnostics.errors != prevErrors) {
        if (fastExit) d.exitWithCleanup(1);
        return; // Don't compile if there were errors
    }

    if (d.onlySyntax) {
        if (fastExit) std.process.exit(0); // Not linking, no need for clean up.
        return;
    }

    if (d.comp.target.ofmt != .elf or d.comp.target.cpu.arch != .x86_64) {
        return d.fatal(
            "unsupported target {s}-{s}-{s}, currently only x86-64 elf is supported",
            .{ @tagName(d.comp.target.cpu.arch), @tagName(d.comp.target.os.tag), @tagName(d.comp.target.abi) },
        );
    }

    var nameBuffer: [std.fs.max_name_bytes]u8 = undefined;
    const outFileName = try d.getOutFileName(source, &nameBuffer);

    if (d.useAssemblyBackend) {
        const asmFn = asmGenFn orelse
            return d.fatal("Assembly codegen not supported", .{});

        const assembly = try asmFn(d.comp.target.toZigTarget(), &tree);
        defer assembly.deinit(d.comp.gpa);

        if (d.onlyPreprocessAndCompile) {
            const outFile = d.comp.cwd.createFile(outFileName, .{}) catch |er|
                return d.fatal("unable to create output file '{s}': {s}", .{ outFileName, errorDescription(er) });
            defer outFile.close();

            assembly.writeToFile(outFile) catch |er|
                return d.fatal("unable to write to output file '{s}': {s}", .{ outFileName, errorDescription(er) });
            if (fastExit) std.process.exit(0); // Not linking, no need for cleanup.
            return;
        }

        // write to assembly outfile name
        // then assemble to outfile name
        var assemblyNameBuffer: [std.fs.max_name_bytes]u8 = undefined;
        const assemblyOutfileName = try d.getRandomFilename(&assemblyNameBuffer, ".s");
        const outFile = d.comp.cwd.createFile(assemblyOutfileName, .{}) catch |er|
            return d.fatal("unable to create output file '{s}': {s}", .{ assemblyOutfileName, errorDescription(er) });
        defer outFile.close();

        assembly.writeToFile(outFile) catch |er|
            return d.fatal("unable to write to output file '{s}': {s}", .{ assemblyOutfileName, errorDescription(er) });
        try d.invokeAssembler(tc, assemblyOutfileName, outFileName);
        if (d.onlyCompile) {
            if (fastExit) std.process.exit(0); // Not linking, no need for cleanup.
            return;
        }
    } else {
        var ir = try tree.genIR();
        defer ir.deinit(d.comp.gpa);

        if (d.dumpIR) {
            var stdoutBuffer: [4096]u8 = undefined;
            var stdout = std.fs.File.stdout().writer(&stdoutBuffer);
            ir.dump(d.comp.gpa, d.detectConfig(stdout.file), &stdout.interface) catch {};
        }

        var renderErrorList: IR.Renderer.ErrorList = .{};
        defer {
            for (renderErrorList.values()) |msg| d.comp.gpa.free(msg);
            renderErrorList.deinit(d.comp.gpa);
        }

        var obj = ir.render(d.comp.gpa, d.comp.target.toZigTarget(), &renderErrorList) catch |e| switch (e) {
            error.OutOfMemory => return error.OutOfMemory,
            error.LowerFail => {
                return d.fatal(
                    "unable to render Ir to machine code: {s}",
                    .{renderErrorList.values()[0]},
                );
            },
        };
        defer obj.deinit();

        // If it's used, name_buf will either hold a filename or `/tmp/<12 random bytes with base-64 encoding>.<extension>`
        // both of which should fit into MAX_NAME_BYTES for all systems
        const outFile = std.fs.cwd().createFile(outFileName, .{}) catch |er|
            return d.fatal("unable to create output file '{s}': {s}", .{ outFileName, errorDescription(er) });
        defer outFile.close();

        var fileBuffer: [4096]u8 = undefined;
        var fileWriter = outFile.writer(&fileBuffer);
        obj.finish(&fileWriter.interface) catch
            return d.fatal("could not output to object file '{s}': {s}", .{ outFileName, errorDescription(fileWriter.err.?) });
    }

    if (d.onlyCompile or d.onlyPreprocessAndCompile) {
        if (fastExit) std.process.exit(0); // Not linking, no need clean up.
        return;
    }

    try d.linkObjects.ensureUnusedCapacity(d.comp.gpa, 1);
    d.linkObjects.appendAssumeCapacity(try d.comp.gpa.dupe(u8, outFileName));
    d.tempFileCount += 1;

    if (fastExit)
        try d.invokeLinker(tc, fastExit);
}

fn printLinkerArgs(w: *std.Io.Writer, items: []const []const u8) !void {
    for (items, 0..) |item, i| {
        if (i > 0) try w.writeByte(' ');
        try w.print("\"{f}\"", .{std.zig.fmtString(item)});
    }
    try w.writeByte('\n');
    try w.flush();
}

fn invokeLinker(d: *Driver, tc: *Toolchain, comptime fastExit: bool) Compilation.Error!void {
    const gpa = d.comp.gpa;
    var argv: std.ArrayList([]const u8) = .empty;
    defer argv.deinit(gpa);

    var linkerPathBuffer: [std.fs.max_path_bytes]u8 = undefined;
    const linkerPath = try tc.getLinkerPath(&linkerPathBuffer);
    try argv.append(gpa, linkerPath);

    try tc.buildLinkerArgs(&argv);

    if (d.dumpLinkerArgs) {
        var stdoutBuffer: [4096]u8 = undefined;
        var stdout = std.fs.File.stdout().writer(&stdoutBuffer);
        printLinkerArgs(&stdout.interface, argv.items) catch {
            return d.fatal("unable to dump linker args: {s}", .{errorDescription(stdout.err.?)});
        };
    }

    var child = std.process.Child.init(argv.items, gpa);
    // TODO handle better
    child.stdin_behavior = .Inherit;
    child.stdout_behavior = .Inherit;
    child.stderr_behavior = .Inherit;

    const term = child.spawnAndWait() catch |er| {
        return d.fatal("unable to spawn linker: {s}", .{errorDescription(er)});
    };
    switch (term) {
        .Exited => |code| if (code != 0) {
            const e = d.fatal("linker exited with an error code", .{});
            if (fastExit) d.exitWithCleanup(code);
            return e;
        },
        else => {
            const e = d.fatal("linker crashed", .{});
            if (fastExit) d.exitWithCleanup(1);
            return e;
        },
    }

    if (fastExit)
        d.exitWithCleanup(0);
}

fn exitWithCleanup(d: *Driver, code: u8) noreturn {
    for (d.linkObjects.items[d.linkObjects.items.len - d.tempFileCount ..]) |obj|
        std.fs.deleteFileAbsolute(obj) catch {};
    std.process.exit(code);
}

/// Parses the various -fpic/-fPIC/-fpie/-fPIE arguments.
/// Then, smooshes them together with platform defaults, to decide whether
/// this compile should be using PIC mode or not.
/// Returns a tuple of ( backend.CodeGenOptions.PicLevel, IsPIE).
pub fn getPICMode(d: *Driver, lastpic: []const u8) Compilation.Error!struct { backend.CodeGenOptions.PicLevel, bool } {
    const eqlIgnoreCase = std.ascii.eqlIgnoreCase;

    const target = &d.comp.target;
    const linker = d.useLinker orelse @import("system-defaults").linker;
    const isBfdLinker = eqlIgnoreCase(linker, "bfd");

    const isPieDefault = switch (target.isPIEDefault()) {
        .Yes => true,
        .No => false,
        .DependsOnLinker => if (isBfdLinker)
            target.cpu.arch == .x86_64 // CrossWindows
        else
            false, //MSVC
    };
    const isPicDefault = switch (target.isPICdefault()) {
        .Yes => true,
        .No => false,
        .DependsOnLinker => if (isBfdLinker)
            target.cpu.arch == .x86_64
        else
            (target.cpu.arch == .x86_64 or target.cpu.arch == .aarch64),
    };

    var pie: bool = isPieDefault;
    var pic: bool = pie or isPicDefault;
    // The Darwin/MachO default to use PIC does not apply when using -static.
    if (target.ofmt == .macho and d.static) {
        pic, pie = .{ false, false };
    }
    var isPicLevelTwo = pic;

    const kernelOrKext: bool = d.mkernel or d.appleKext;

    // Android-specific defaults for PIC/PIE
    if (target.abi.isAndroid()) {
        switch (target.cpu.arch) {
            .arm,
            .armeb,
            .thumb,
            .thumbeb,
            .aarch64,
            .mips,
            .mipsel,
            .mips64,
            .mips64el,
            => pic = true, // "-fpic"

            .x86, .x86_64 => {
                pic = true; // "-fPIC"
                isPicLevelTwo = true;
            },
            else => {},
        }
    }

    // OHOS-specific defaults for PIC/PIE
    if (target.abi == .ohos and target.cpu.arch == .aarch64)
        pic = true;

    // OpenBSD-specific defaults for PIE
    if (target.os.tag == .openbsd) {
        switch (target.cpu.arch) {
            .arm, .aarch64, .mips64, .mips64el, .x86, .x86_64 => isPicLevelTwo = false, // "-fpie"
            .powerpc, .sparc64 => isPicLevelTwo = true, // "-fPIE"
            else => {},
        }
    }

    // The last argument relating to either PIC or PIE wins, and no
    // other argument is used. If the last argument is any flavor of the
    // '-fno-...' arguments, both PIC and PIE are disabled. Any PIE
    // option implicitly enables PIC at the same level.
    if (target.os.tag == .windows and !target.isMinGW() and
        (eqlIgnoreCase(lastpic, "-fpic") or eqlIgnoreCase(lastpic, "-fpie"))) // -fpic/-fPIC, -fpie/-fPIE
    {
        try d.unsupportedOptionForTarget(target, lastpic);
        if (target.cpu.arch == .x86_64)
            return .{ .two, false };
        return .{ .none, false };
    }

    // Check whether the tool chain trumps the PIC-ness decision. If the PIC-ness
    // is forced, then neither PIC nor PIE flags will have no effect.
    const forced = switch (target.isPICDefaultForced()) {
        .Yes => true,
        .No => false,
        .DependsOnLinker => if (isBfdLinker) target.cpu.arch == .x86_64 else target.cpu.arch == .aarch64 or target.cpu.arch == .x86_64,
    };
    if (!forced) {
        // -fpic/-fPIC, -fpie/-fPIE
        if (eqlIgnoreCase(lastpic, "-fpic") or eqlIgnoreCase(lastpic, "-fpie")) {
            pie = eqlIgnoreCase(lastpic, "-fpie");
            pic = pie or eqlIgnoreCase(lastpic, "-fpic");
            isPicLevelTwo = mem.eql(u8, lastpic, "-fPIE") or mem.eql(u8, lastpic, "-fPIC");
        } else {
            pic, pie = .{ false, false };
            if (target.isPS()) {
                if (d.cmodel != .kernel) {
                    pic = true;
                    try d.warn(
                        "option '{s}' was ignored by the {s} toolchain, using '-fPIC'",
                        .{ lastpic, if (target.os.tag == .ps4) "PS4" else "PS5" },
                    );
                }
            }
        }
    }

    if (pic and (target.os.tag.isDarwin() or target.isPS())) {
        isPicLevelTwo = isPicLevelTwo or isPicDefault;
    }

    // This kernel flags are a trump-card: they will disable PIC/PIE
    // generation, independent of the argument order.
    if (kernelOrKext and
        (!(target.os.tag != .ios) or (target.os.isAtLeast(.ios, .{ .major = 6, .minor = 0, .patch = 0 }) orelse false)) and
        !(target.os.tag != .watchos) and
        !(target.os.tag != .driverkit))
    {
        pie, pic = .{ false, false };
    }

    if (d.dynamicNopic == true) {
        if (!target.os.tag.isDarwin()) {
            try d.unsupportedOptionForTarget(target, "-mdynamic-no-pic");
        }
        pic = isPicDefault or forced;
        return .{ if (pic) .two else .none, false };
    }

    const embedderPiSupported = target.cpu.arch.isArm();
    if (!embedderPiSupported) {
        if (d.ropi) try d.unsupportedOptionForTarget(target, "-fropi");
        if (d.rwpi) try d.unsupportedOptionForTarget(target, "-frwpi");
    }

    // ROPI and RWPI are not compatible with PIC or PIE.
    if ((d.ropi or d.rwpi) and (pic or pie)) {
        try d.err("embedded and GOT-based position independence are incompatible", .{});
    }

    if (target.cpu.arch.isMIPS()) {
        // When targeting the N64 ABI, PIC is the default, except in the case
        // when the -mno-abicalls option is used. In that case we exit
        // at next check regardless of PIC being set below.
        // TODO: implement incomplete!!
        if (target.cpu.arch.isMIPS64())
            pic = true;

        // When targettng MIPS with -mno-abicalls, it's always static.
        if (d.mabicalls == false)
            return .{ .none, false };

        // Unlike other architectures, MIPS, even with -fPIC/-mxgot/multigot,
        // does not use PIC level 2 for historical reasons.
        isPicLevelTwo = false;
    }

    if (pic) return .{ if (isPicLevelTwo) .two else .one, pie };
    return .{ .none, false };
}
