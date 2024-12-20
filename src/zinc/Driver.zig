const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
const process = std.process;
const backend = @import("backend");
const IR = backend.Ir;
const Object = backend.Object;
const Util = backend.Util;
const CodeGen = @import("CodeGen/CodeGen.zig");
const Compilation = @import("Basic/Compilation.zig");
const LangOpts = @import("Basic/LangOpts.zig");
const Lexer = @import("Lexer/Lexer.zig");
const Preprocessor = @import("Lexer/Preprocessor.zig");
const Source = @import("Basic/Source.zig");
const Toolchain = @import("Toolchain.zig");
const Target = @import("Basic/Target.zig");

const Driver = @This();

pub const Linker = enum {
    ld,
    bfd,
    gold,
    lld,
    mold,
};

comp: *Compilation,
inputs: std.ArrayListUnmanaged(Source) = .{},
linkObjects: std.ArrayListUnmanaged([]const u8) = .{},
outputName: ?[]const u8 = null,
sysroot: ?[]const u8 = null,
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

/// name of the zinc executable
zincName: []const u8 = "",

/// Value of --triple= passed via CLI
rawTargetTriple: ?[]const u8 = null,

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
    \\  -h, --help      Print this message.
    \\  -v, --version   Print Zinc version.
    \\ 
    \\Compile Options:
    \\  -c, --compile           Only run preprocess, compile, and assemble steps
    \\  -D <macro>=<value>      Define <macro> to <value> (defaults to 1)
    \\  -E                      Only run the preprocessor 
    \\  -fchar8_t               Enable char8_t (enabled by default in C23 and later)
    \\  -fno-char8_t            Disable char8_t (disabled by default for pre-C23)
    \\  -fcolor-diagnostics     Enable colors in diagnostics
    \\  -fno-color-diagnostics  Disable colors in diagnostics
    \\  -fdeclspec              Enable support for __declspec attributes
    \\  -fno-declspec           Disable support for __declspec attributes
    \\  -fgnu-inline-asm        Enable GNU style inline asm (default: enabled)
    \\  -fno-gnu-inline-asm     Disable GNU style inline asm
    \\  -fms-extensions         Enable support for Microsoft extensions
    \\  -fno-ms-extensions      Disable support for Microsoft extensions
    \\  -fdollars-in-identifiers        
    \\                          Allow '$' in identifiers(default)
    \\  -fno-dollars-in-identifiers     
    \\                          Disallow '$' in identifiers
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
    \\  -I <dir>                Add directory to include search path
    \\  -isystem                Add directory to system include search path
    \\  --emulate=[clang|gcc|msvc]
    \\                          Select which C compiler to emulate (default clang)
    \\  -o <file>               Write output to <file>
    \\  -P, --no-line-commands  Disable linemarker output in -E mode
    \\  -pedantic               Warn on language extensions
    \\  --rtlib=<arg>           Compiler runtime library to use (libgcc or compiler-rt)
    \\  -std=<standard>         Specify language standard
    \\  -S, --assemble          Only run preprocess and compilation step
    \\ --target=<value>         Generate code for the given target
    \\  -U <macro>              Undefine <macro>
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
    stdOut: anytype,
    macroBuffer: anytype,
    args: [][]const u8,
) !bool {
    var colorSetting: enum { on, off, unset } = .unset;

    var commentArg: []const u8 = "";
    var i: usize = 1;
    while (i < args.len) : (i += 1) {
        const arg = args[i];
        if (std.mem.startsWith(u8, arg, "-") and arg.len > 1) {
            if (std.mem.eql(u8, arg, "-h") or std.mem.eql(u8, arg, "--help")) {
                stdOut.print(usage, .{args[0]}) catch |er| {
                    return d.fatal("unable to print usage: {s}", .{Util.errorDescription(er)});
                };
                return true;
            } else if (std.mem.eql(u8, arg, "-v") or std.mem.eql(u8, arg, "--version")) {
                stdOut.writeAll(@import("backend").VersionStr ++ "\n") catch |er| {
                    return d.fatal("unable to print version: {s}", .{Util.errorDescription(er)});
                };
                return true;
            } else if (std.mem.startsWith(u8, arg, "-D")) {
                var macro = arg["-D".len..];
                if (macro.len == 0) {
                    i += 1;
                    if (i >= args.len) {
                        try d.err("expected argument after -D");
                        continue;
                    }
                    macro = args[i];
                }

                var value: []const u8 = "1";
                if (std.mem.indexOfScalar(u8, macro, '=')) |some| {
                    value = macro[some + 1 ..];
                    macro = macro[0..some];
                }
                try macroBuffer.print("#define {s} {s}\n", .{ macro, value });
            } else if (std.mem.startsWith(u8, arg, "-U")) {
                var macro = arg["-U".len..];
                if (macro.len == 0) {
                    i += 1;
                    if (i >= args.len) {
                        try d.err("expected argument after -U");
                        continue;
                    }
                    macro = args[i];
                }
                try macroBuffer.print("#undef {s} \n", .{macro});
            } else if (std.mem.eql(u8, arg, "-c") or std.mem.eql(u8, arg, "--compile")) {
                d.onlyCompile = true;
            } else if (std.mem.eql(u8, arg, "-E")) {
                d.onlyPreprocess = true;
            } else if (std.mem.eql(u8, arg, "-P") or std.mem.eql(u8, arg, "--no-line-commands")) {
                d.lineCommands = false;
            } else if (std.mem.eql(u8, arg, "-fuse-line-directives")) {
                d.useLineDirectives = true;
            } else if (std.mem.eql(u8, arg, "-fno-use-line-directives")) {
                d.useLineDirectives = false;
            } else if (mem.eql(u8, arg, "-fchar8_t")) {
                d.comp.langOpts.hasChar8tOverride = true;
            } else if (mem.eql(u8, arg, "-fno-char8_t")) {
                d.comp.langOpts.hasChar8tOverride = false;
            } else if (std.mem.eql(u8, arg, "-fcolor-diagnostics")) {
                colorSetting = .on;
            } else if (std.mem.eql(u8, arg, "-fno-color-diagnostics")) {
                colorSetting = .off;
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
                d.comp.langOpts.declSpecAttrs = true;
            } else if (std.mem.eql(u8, arg, "-fno-declspec")) {
                d.comp.langOpts.declSpecAttrs = false;
            } else if (mem.eql(u8, arg, "-fgnu-inline-asm")) {
                d.comp.langOpts.gnuAsm = true;
            } else if (mem.eql(u8, arg, "-fno-gnu-inline-asm")) {
                d.comp.langOpts.gnuAsm = false;
            } else if (std.mem.eql(u8, arg, "-fms-extensions")) {
                d.comp.langOpts.enableMSExtensions();
            } else if (std.mem.eql(u8, arg, "-fno-ms-extensions")) {
                d.comp.langOpts.disableMSExtensions();
            } else if (std.mem.eql(u8, arg, "-fdollars-in-identifiers")) {
                d.comp.langOpts.dollarsInIdentifiers = true;
            } else if (std.mem.eql(u8, arg, "-fno-dollars-in-identifiers")) {
                d.comp.langOpts.dollarsInIdentifiers = false;
            } else if (option(arg, "-fmacro-backtrace-limit=")) |limitStr| {
                var limit = std.fmt.parseInt(u32, limitStr, 10) catch {
                    try d.err("-fmacro-backtrace-limit takes a number argument");
                    continue;
                };
                if (limit == 0) limit = std.math.maxInt(u32);
                d.comp.diagnostics.macroBacktraceLimit = limit;
            } else if (std.mem.startsWith(u8, arg, "-I")) {
                var path = arg["-I".len..];
                if (path.len == 0) {
                    i += 1;
                    if (i >= args.len) {
                        try d.err("expected argument after -I");
                        continue;
                    }
                    path = args[i];
                }
                try d.comp.includeDirs.append(d.comp.gpa, path);
            } else if (std.mem.eql(u8, arg, "-fsyntax-only")) {
                d.onlySyntax = true;
            } else if (std.mem.eql(u8, arg, "-fno-syntax-only")) {
                d.onlySyntax = false;
            } else if (std.mem.startsWith(u8, arg, "-isystem")) {
                var path = arg["-isystem".len..];
                if (path.len == 0) {
                    i += 1;
                    if (i >= args.len) {
                        try d.err("expected argument after -isystem");
                        continue;
                    }
                    path = args[i];
                }
                try d.comp.systemIncludeDirs.append(d.comp.gpa, path);
            } else if (option(arg, "--emulate=")) |compilerStr| {
                const compiler = std.meta.stringToEnum(LangOpts.Compiler, compilerStr) orelse {
                    try d.comp.addDiagnostic(.{ .tag = .cli_invalid_emulate, .extra = .{ .str = arg } }, &.{});
                    continue;
                };
                d.comp.langOpts.setEmulatedCompiler(compiler);
            } else if (std.mem.startsWith(u8, arg, "-o")) {
                var filename = arg["-o".len..];
                if (filename.len == 0) {
                    i += 1;
                    if (i >= args.len) {
                        try d.err("expected argument after -o");
                        continue;
                    }
                    filename = args[i];
                }
                d.outputName = filename;
            } else if (option(arg, "--sysroot=")) |sysroot| {
                d.sysroot = sysroot;
            } else if (std.mem.eql(u8, arg, "-pedantic")) {
                d.comp.diagnostics.options.pedantic = .warning;
            } else if (option(arg, "--rtlib=")) |rtlib| {
                if (mem.eql(u8, rtlib, "compiler-rt") or mem.eql(u8, rtlib, "libgcc") or mem.eql(u8, rtlib, "platform")) {
                    d.rtlib = rtlib;
                } else {
                    try d.comp.addDiagnostic(.{ .tag = .invalid_rtlib, .extra = .{ .str = rtlib } }, &.{});
                }
            } else if (std.mem.eql(u8, arg, "-Wall")) {
                d.comp.diagnostics.setAll(.warning);
            } else if (std.mem.eql(u8, arg, "-Werror")) {
                d.comp.diagnostics.setAll(.@"error");
            } else if (std.mem.eql(u8, arg, "-Wfatal-errors")) {
                d.comp.diagnostics.fatalErrors = true;
            } else if (std.mem.eql(u8, arg, "-Wno-fatal-errors")) {
                d.comp.diagnostics.fatalErrors = false;
            } else if (option(arg, "-Werror=")) |errName| {
                try d.comp.diagnostics.set(errName, .@"error");
            } else if (option(arg, "-Wno-")) |errName| {
                try d.comp.diagnostics.set(errName, .off);
            } else if (option(arg, "-W")) |errName| {
                try d.comp.diagnostics.set(errName, .warning);
            } else if (option(arg, "-std=")) |standard| {
                d.comp.langOpts.setStandard(standard) catch
                    try d.comp.addDiagnostic(.{ .tag = .cli_invalid_standard, .extra = .{ .str = arg } }, &.{});
            } else if (std.mem.eql(u8, arg, "-S") or std.mem.eql(u8, arg, "--assemble")) {
                d.onlyPreprocessAndCompile = true;
            } else if (option(arg, "--target=")) |triple| {
                const query = std.Target.Query.parse(.{ .arch_os_abi = triple }) catch {
                    try d.comp.addDiagnostic(.{ .tag = .cli_invalid_target, .extra = .{ .str = arg } }, &.{});
                    continue;
                };
                const target = std.zig.system.resolveTargetQuery(query) catch |e| {
                    return d.fatal("unable to resolve target: {s}", .{Util.errorDescription(e)});
                };
                d.comp.target = target;
                d.comp.langOpts.setEmulatedCompiler(Target.systemCompiler(d.comp.target));
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
                    try d.comp.addDiagnostic(.{ .tag = .invalid_unwindlib, .extra = .{ .str = unwindlib } }, &.{});
                }
            } else {
                try d.comp.addDiagnostic(.{ .tag = .cli_unknown_arg, .extra = .{ .str = arg } }, &.{});
            }
        } else if (std.mem.endsWith(u8, arg, ".o") or std.mem.endsWith(u8, arg, ".obj")) {
            try d.linkObjects.append(d.comp.gpa, arg);
        } else {
            const file = d.addSource(arg) catch |er| {
                return d.fatal("unable to add source file '{s}': {s}", .{ arg, Util.errorDescription(er) });
            };
            try d.inputs.append(d.comp.gpa, file);
        }
    }

    d.comp.diagnostics.color = switch (colorSetting) {
        .on => true,
        .off => false,
        .unset => Util.fileSupportsColor(std.io.getStdOut()) and !std.process.hasEnvVarConstant("NO_COLOR"),
    };

    if (d.comp.langOpts.preserveComments and !d.onlyPreprocess)
        return d.fatal("invalid argument '{s}' only allowed with '-E'", .{commentArg});

    return false;
}

fn addSource(d: *Driver, path: []const u8) !Source {
    if (std.mem.eql(u8, "-", path)) {
        const stdin = std.io.getStdIn().reader();
        const input = try stdin.readAllAlloc(d.comp.gpa, std.math.maxInt(u32));
        defer d.comp.gpa.free(input);
        return d.comp.addSourceFromBuffer("<stdin>", input);
    }
    return d.comp.addSourceFromPath(path);
}

pub fn err(d: *Driver, msg: []const u8) !void {
    try d.comp.addDiagnostic(.{ .tag = .cli_error, .extra = .{ .str = msg } }, &.{});
}

pub fn fatal(d: *Driver, comptime fmt: []const u8, args: anytype) error{FatalError} {
    d.comp.renderErrors();
    return d.comp.diagnostics.fatalNoSrc(fmt, args);
}

/// The entry point of the Zinc compiler.
/// **MAY call `exit` if `fast_exit` is set.**
pub fn main(d: *Driver, tc: *Toolchain, args: [][]const u8, comptime fastExit: bool) !void {
    var macroBuffer = std.ArrayList(u8).init(d.comp.gpa);
    defer macroBuffer.deinit();

    const stdOut = std.io.getStdOut().writer();
    if (try parseArgs(d, stdOut, macroBuffer.writer(), args))
        return;

    const linking = !(d.onlyPreprocess or d.onlySyntax or d.onlyCompile or d.onlyPreprocessAndCompile);

    if (d.inputs.items.len == 0) {
        return d.fatal("no input files", .{});
    } else if ((d.inputs.items.len != 1 and d.outputName != null) and !linking) {
        return d.fatal("cannot specify -o when generating multiple output files", .{});
    }

    if (!linking)
        for (d.linkObjects.items) |obj|
            try d.comp.addDiagnostic(.{ .tag = .cli_unused_link_object, .extra = .{ .str = obj } }, &.{});

    d.comp.defineSystemIncludes(d.zincName) catch |er| switch (er) {
        error.OutOfMemory => return error.OutOfMemory,
        error.ZincIncludeNotFound => return d.fatal("unable to find Zinc builtin headers", .{}),
    };

    const builtinMacros = try d.comp.generateBuiltinMacros();
    const userDefinedMacros = try d.comp.addSourceFromBuffer("<command line>", macroBuffer.items);

    if (fastExit and d.inputs.items.len == 1) {
        processSource(tc, d.inputs.items[0], builtinMacros, userDefinedMacros, fastExit) catch |e| switch (e) {
            error.FatalError => {
                d.comp.renderErrors();
                d.exitWithCleanup(1);
            },
            else => |er| return er,
        };
        unreachable;
    }

    for (d.inputs.items) |source| {
        d.processSource(tc, source, builtinMacros, userDefinedMacros, fastExit) catch |e| switch (e) {
            error.FatalError => d.comp.renderErrors(),
            else => |er| return er,
        };
    }

    if (d.comp.diagnostics.errors != 0) {
        if (fastExit) d.exitWithCleanup(1);
        return;
    }

    if (linking)
        try d.invokeLinker(tc, fastExit);

    if (fastExit)
        std.process.exit(0);
}

fn processSource(
    d: *Driver,
    tc: *Toolchain,
    source: Source,
    builtinMacro: Source,
    userDefinedMacros: Source,
    comptime fastExit: bool,
) !void {
    d.comp.generatedBuffer.items.len = 0;
    var pp = try Preprocessor.initDefault(d.comp);
    defer pp.deinit();

    if (d.comp.langOpts.msExtensions)
        d.comp.msCwdSourceId = source.id;

    if (d.dumpPP) pp.verbose = true;
    if (d.onlyPreprocess) {
        pp.preserveWhitespace = true;
        if (d.lineCommands)
            pp.linemarkers = if (d.useLineDirectives) .LineDirectives else .NumericDirectives;
    }

    if (d.dumpRawTokens)
        _ = try pp.tokenize(source)
    else
        try pp.preprocessSources(&.{ source, builtinMacro, userDefinedMacros });

    if (d.onlyPreprocess) {
        d.comp.renderErrors();

        if (d.comp.diagnostics.errors != 0) {
            if (fastExit) std.process.exit(1); // not linking, no need for cleanup
            return;
        }

        const file = if (d.outputName) |some|
            std.fs.cwd().createFile(some, .{}) catch |er|
                return d.fatal("unable to create output file '{s}': {s}", .{ some, Util.errorDescription(er) })
        else
            std.io.getStdOut();
        defer if (d.outputName != null) file.close();

        var bufWriter = std.io.bufferedWriter(file.writer());
        pp.prettyPrintTokens(bufWriter.writer()) catch |er|
            return d.fatal("unable to write result: {s}", .{Util.errorDescription(er)});

        bufWriter.flush() catch |er|
            return d.fatal("unable to write result: {s}", .{Util.errorDescription(er)});

        if (fastExit)
            std.process.exit(0); // Not linking, no need for clean up.

        return;
    }

    if (d.dumpTokens or d.dumpRawTokens) {
        const locs = pp.tokens.items(.loc);
        var lexer = Lexer{
            .buffer = d.comp.getSource(locs[0].id).buffer,
            .comp = d.comp,
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
        for (tree.nodes.items(.tag), 0..) |nodeTag, i|
            std.debug.print("{d}: {s}\n", .{ i, @tagName(nodeTag) });

        const stdout = std.io.getStdOut();
        const color = d.comp.diagnostics.color and Util.fileSupportsColor(stdout);

        var buffWriter = std.io.bufferedWriter(stdout.writer());
        tree.dump(color, buffWriter.writer()) catch {};
        buffWriter.flush() catch {};
    }

    const prevErrors = d.comp.diagnostics.errors;
    d.comp.renderErrors();

    // do not compile if there were errors
    if (d.comp.diagnostics.errors != prevErrors) {
        if (fastExit)
            exitWithCleanup(1);
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

    var ir = try tree.genIR();
    defer ir.deinit(d.comp.gpa);

    if (d.dumpIR) {
        const stdout = std.io.getStdOut();
        var bufferWriter = std.io.bufferedWriter(stdout.writer());
        const color = d.comp.diagnostics.color and Util.fileSupportsColor(stdout);

        ir.dump(d.comp.gpa, color, bufferWriter.writer()) catch {};
        bufferWriter.flush() catch {};
    }

    var renderErrors: IR.Renderer.ErrorList = .{};
    defer renderErrors.deinit(d.comp.gpa);

    var obj = ir.render(d.comp.gpa, d.comp.target, &renderErrors) catch |e| switch (e) {
        error.OutOfMemory => return error.OutOfMemory,
        error.LowerFail => {
            return d.fatal(
                "unable to render Ir to machine code: {s}",
                .{renderErrors.values()[0]},
            );
        },
    };
    defer obj.deinit();

    // If it's used, name_buf will either hold a filename or `/tmp/<12 random bytes with base-64 encoding>.<extension>`
    // both of which should fit into MAX_NAME_BYTES for all systems
    var nameBuffer: [std.fs.max_name_bytes]u8 = undefined;

    const outFileName = if (d.onlyCompile) blk: {
        const fmtTemplate = "{s}{s}";
        const fmtArgs = .{
            std.fs.path.stem(source.path),
            d.comp.target.ofmt.fileExt(d.comp.target.cpu.arch),
        };
        break :blk d.outputName orelse std.fmt.bufPrint(&nameBuffer, fmtTemplate, fmtArgs) catch return d.fatal("Filename too long for filesystem: " ++ fmtTemplate, fmtArgs);
    } else blk: {
        const randomBytesCount = 12;
        const subPathLen = comptime std.fs.base64_encoder.calcSize(randomBytesCount);

        var randomBytes: [randomBytesCount]u8 = undefined;
        std.crypto.random.bytes(&randomBytes);
        var randomName: [subPathLen]u8 = undefined;
        _ = std.fs.base64_encoder.encode(&randomName, &randomBytes);

        const fmtTemplate = "/tmp/{s}{s}";
        const fmtArgs = .{
            randomName,
            d.comp.target.ofmt.fileExt(d.comp.target.cpu.arch),
        };
        break :blk std.fmt.bufPrint(&nameBuffer, fmtTemplate, fmtArgs) catch return d.fatal("Filename too long for filesystem: " ++ fmtTemplate, fmtArgs);
    };

    const outFile = std.fs.cwd().createFile(outFileName, .{}) catch |er|
        return d.fatal("unable to create output file '{s}': {s}", .{ outFileName, Util.errorDescription(er) });
    defer outFile.close();

    obj.finish(outFile) catch |er|
        return d.fatal("could not output to object file '{s}': {s}", .{ outFileName, Util.errorDescription(er) });

    if (d.onlyCompile) {
        if (fastExit)
            std.process.exit(0); // Not linking, no need clean up.
        return;
    }

    try d.linkObjects.ensureUnusedCapacity(d.comp.gpa, 1);
    d.linkObjects.appendAssumeCapacity(try d.comp.gpa.dupe(u8, outFileName));
    d.tempFileCount += 1;

    if (fastExit)
        try invokeLinker(tc, fastExit);
}

fn printLinkerArgs(items: []const []const u8) !void {
    const stdout = std.io.getStdOut().writer();
    for (items, 0..) |item, i| {
        if (i > 0) try stdout.writeByte(' ');
        try stdout.print("\"{}\"", .{std.zig.fmtEscapes(item)});
    }
    try stdout.writeByte('\n');
}

fn invokeLinker(d: *Driver, tc: *Toolchain, comptime fastExit: bool) !void {
    try tc.discover();

    var argv = std.ArrayList([]const u8).init(d.comp.gpa);
    defer argv.deinit();

    var linkerPathBuffer: [std.fs.max_path_bytes]u8 = undefined;
    const linkerPath = try tc.getLinkerPath(&linkerPathBuffer);
    try argv.append(linkerPath);

    try tc.buildLinkerArgs(&argv);

    if (d.dumpLinkerArgs) {
        printLinkerArgs(argv.items) catch |er|
            return d.fatal("unable to dump linker args: {s}", .{Util.errorDescription(er)});
    }

    var child = std.process.Child.init(argv.items, d.comp.gpa);
    // TODO handle better
    child.stdin_behavior = .Inherit;
    child.stdout_behavior = .Inherit;
    child.stderr_behavior = .Inherit;

    const term = child.spawnAndWait() catch |er| {
        return d.fatal("unable to spawn linker: {s}", .{Util.errorDescription(er)});
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
