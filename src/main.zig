const std = @import("std");
const builtin = @import("builtin");
const Compilation = @import("Basic/Compilation.zig");
const Codegen = @import("CodeGen/Codegen.zig");
const Source = @import("Basic/Source.zig");
const Preprocessor = @import("Lexer/Preprocessor.zig");
const Lexer = @import("Lexer/Lexer.zig");
const Parser = @import("Parser/Parser.zig");
const LangOpts = @import("Basic/LangOpts.zig");

var GeneralPurposeAllocator = std.heap.GeneralPurposeAllocator(.{}){};

pub fn main() u8 {
    const gpa = if (builtin.link_libc) std.heap.raw_c_allocator else GeneralPurposeAllocator.allocator();
    defer if (!builtin.link_libc) {
        _ = GeneralPurposeAllocator.deinit();
    };

    var arenaInstance = std.heap.ArenaAllocator.init(gpa);
    defer arenaInstance.deinit();

    const arena = arenaInstance.allocator();
    const args = std.process.argsAlloc(arena) catch {
        std.debug.print("Out of Memory\n", .{});
        return 1;
    };

    var comp = Compilation.init(gpa);
    defer comp.deinit();

    comp.addDefaultPragmaHandlers() catch |er| switch (er) {
        error.OutOfMemory => {
            std.debug.print("Out of Memory\n", .{});
            return 1;
        },
    };

    if (comp.target.abi == .msvc or comp.target.os.tag == .windows) {
        comp.langOpts.setEmulatedCompiler(.msvc);
    }

    mainExtra(&comp, args) catch |er| switch (er) {
        error.OutOfMemory => {
            std.debug.print("Out of Memory\n", .{});
            return 1;
        },
        error.StreamTooLong => {
            std.debug.print("Stream too long\n", .{});
            return 1;
        },
        error.FatalError => comp.renderErrors(),
        else => return 1,
    };

    return @intFromBool(comp.diag.errors != 0);
}

const usage =
    \\Usage {s}: [options] file..
    \\
    \\General Options:
    \\  -h, --help      Print this message.
    \\  -v, --version   Print ZCC version.
    \\ 
    \\Compile Options:
    \\  -c                      Only run preprocess, compile, and assemble steps
    \\  -D <macro>=<value>      Define <macro> to <value> (defaults to 1)
    \\  -E                      Only run the preprocessor
    \\  -fcolor-diagnostics     Enable colors in diagnostics
    \\  -fno-color-diagnostics  Disable colors in diagnostics
    \\  -fdeclspec              Enable support for __declspec attributes
    \\  -fno-declspec           Disable support for __declspec attributes
    \\  -fms-extensions         Enable support for Microsoft extensions
    \\  -fno-ms-extensions      Disable support for Microsoft extensions
    \\  -fdollars-in-identifiers        
    \\                          Allow '$' in identifiers(default)
    \\  -fno-dollars-in-identifiers     
    \\                          Disallow '$' in identifiers
    \\  -fshort-enums           Use the narrowest possible integer type for enums.
    \\  -fno-short-enums        Use "int" as the tag type for enums.
    \\  -fmacro-backtrace-limit=<limit>
    \\                          Set limit on how many macro expansion traces are shown in errors (default 6)
    \\  -I <dir>                Add directory to include search path
    \\  -isystem                Add directory to system include search path
    \\  --emulate=[clang|gcc|msvc]
    \\                          Select which C compiler to emulate (default clang)
    \\  -o <file>               Write output to <file>
    \\  -pedantic               Warn on language extensions
    \\  -std=<standard>         Specify language standard
    \\ --target=<value>         Generate code for the given target
    \\  -U <macro>              Undefine <macro>
    \\  -Wall                   Enable all warnings
    \\  -Werror                 Treat all warnings as errors
    \\  -Werror=<warning>       Treat warning as error
    \\  -W<warning>             Enable the specified warning
    \\  -Wno-<warning>          Disable the specified warning
    \\
    \\Debug options:
    \\  -dump-ast              Dump produced AST to stdout
    \\  -dump-tokens           Run preprocessor, dump internal rep of tokens to stdout 
    \\  -dump-raw-tokens        Lex file in raw mode and dump raw tokens to stdout
    \\
;

pub fn parseArgs(
    comp: *Compilation,
    stdOut: anytype,
    sources: *std.ArrayList(Source),
    macroBuffer: anytype,
    args: [][]const u8,
) !bool {
    var i: usize = 1;
    while (i < args.len) : (i += 1) {
        const arg = args[i];
        if (std.mem.startsWith(u8, arg, "-")) {
            if (std.mem.eql(u8, arg, "-h") or std.mem.eql(u8, arg, "--help")) {
                stdOut.print(usage, .{args[0]}) catch |er| {
                    return fatal(comp, "{s} when trying to print usage", .{@errorName(er)});
                };
                return true;
            } else if (std.mem.eql(u8, arg, "-v") or std.mem.eql(u8, arg, "--version")) {
                stdOut.writeAll(@import("zcc.zig").VersionStr ++ "\n") catch |er| {
                    return fatal(comp, "{s} when trying to print version", .{@errorName(er)});
                };
                return true;
            } else if (std.mem.startsWith(u8, arg, "-D")) {
                var macro = arg["-D".len..];
                if (macro.len == 0) {
                    i += 1;
                    if (i >= args.len) {
                        try err(comp, "expected argument after -D");
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
                    if (i >= args.len)
                        return comp.diag.fatalNoSrc("expected argument after -U", .{});
                    macro = args[i];
                }

                try macroBuffer.print("#undef {s} \n", .{macro});
            } else if (std.mem.eql(u8, arg, "-c")) {
                comp.onlyCompile = true;
            } else if (std.mem.eql(u8, arg, "-E")) {
                comp.onlyPreprocess = true;
            } else if (std.mem.eql(u8, arg, "-fcolor-diagnostics")) {
                comp.diag.color = true;
            } else if (std.mem.eql(u8, arg, "-fno-color-diagnostics")) {
                comp.diag.color = false;
            } else if (std.mem.eql(u8, arg, "-fshort-enums")) {
                comp.langOpts.shortEnums = true;
            } else if (std.mem.eql(u8, arg, "-fno-short-enums")) {
                comp.langOpts.shortEnums = false;
            } else if (std.mem.eql(u8, arg, "-fdeclspec")) {
                comp.langOpts.declSpecAttrs = true;
            } else if (std.mem.eql(u8, arg, "-fno-declspec")) {
                comp.langOpts.declSpecAttrs = false;
            } else if (std.mem.eql(u8, arg, "-fms-extensions")) {
                comp.langOpts.enableMSExtensions();
            } else if (std.mem.eql(u8, arg, "-fno-ms-extensions")) {
                comp.langOpts.disableMSExtensions();
            } else if (std.mem.eql(u8, arg, "-fdollars-in-identifiers")) {
                comp.langOpts.dollarsInIdentifiers = true;
            } else if (std.mem.eql(u8, arg, "-fno-dollars-in-identifiers")) {
                comp.langOpts.dollarsInIdentifiers = false;
            } else if (std.mem.startsWith(u8, arg, "-fmacro-backtrace-limit=")) {
                const limitStr = arg["-fmacro-backtrace-limit=".len..];
                var limit = std.fmt.parseInt(u32, limitStr, 10) catch {
                    try err(comp, "-fmacro-backtrace-limit takes a number argument");
                    continue;
                };
                if (limit == 0) limit = std.math.maxInt(u32);
                comp.diag.macroBacktraceLimit = limit;
            } else if (std.mem.startsWith(u8, arg, "-I")) {
                var path = arg["-I".len..];
                if (path.len == 0) {
                    i += 1;
                    if (i >= args.len) {
                        try err(comp, "expected argument after -I");
                        continue;
                    }
                    path = args[i];
                }
                try comp.includeDirs.append(path);
            } else if (std.mem.startsWith(u8, arg, "-isystem")) {
                var path = arg["-isystem".len..];
                if (path.len == 0) {
                    i += 1;
                    if (i >= args.len) {
                        try err(comp, "expected argument after -isystem");
                        continue;
                    }
                    path = args[i];
                }
                try comp.systemIncludeDirs.append(path);
            } else if (std.mem.startsWith(u8, arg, "--emulate=")) {
                const compilerStr = arg["--emulate=".len..];
                const compiler = std.meta.stringToEnum(LangOpts.Compiler, compilerStr) orelse {
                    try comp.diag.add(.{ .tag = .cli_invalid_emulate, .extra = .{ .str = arg } }, &.{});
                    continue;
                };
                comp.langOpts.setEmulatedCompiler(compiler);
            } else if (std.mem.startsWith(u8, arg, "-o")) {
                var filename = arg["-o".len..];
                if (filename.len == 0) {
                    i += 1;
                    if (i >= args.len) {
                        try err(comp, "expected argument after -o");
                        continue;
                    }
                    filename = args[i];
                }
                comp.outputName = filename;
            } else if (std.mem.eql(u8, arg, "-pedantic")) {
                comp.diag.options.pedantic = .warning;
            } else if (std.mem.eql(u8, arg, "-Wall")) {
                comp.diag.setAll(.warning);
            } else if (std.mem.eql(u8, arg, "-Werror")) {
                comp.diag.setAll(.@"error");
            } else if (std.mem.eql(u8, arg, "-Wfatal-errors")) {
                comp.diag.fatalErrors = true;
            } else if (std.mem.eql(u8, arg, "-Wno-fatal-errors")) {
                comp.diag.fatalErrors = false;
            } else if (std.mem.startsWith(u8, arg, "-Werror=")) {
                const option = arg["-Werror=".len..];
                try comp.diag.set(option, .@"error");
            } else if (std.mem.startsWith(u8, arg, "-Wno-")) {
                const option = arg["-Wno-".len..];
                try comp.diag.set(option, .off);
            } else if (std.mem.startsWith(u8, arg, "-W")) {
                const option = arg["-W".len..];
                try comp.diag.set(option, .warning);
            } else if (std.mem.startsWith(u8, arg, "-std=")) {
                const standard = arg["-std=".len..];
                comp.langOpts.setStandard(standard) catch
                    try comp.diag.add(.{ .tag = .cli_invalid_standard, .extra = .{ .str = arg } }, &.{});
            } else if (std.mem.startsWith(u8, arg, "--target=")) {
                const triple = arg["--target=".len..];
                const query = std.Target.Query.parse(.{ .arch_os_abi = triple }) catch {
                    return comp.diag.fatalNoSrc("Invalid target '{s}'", .{triple});
                };
                const target = std.zig.system.resolveTargetQuery(query) catch |e| {
                    return comp.diag.fatalNoSrc("unable to resolve target: {s}", .{@errorName(e)});
                };
                comp.target = target;
                if (comp.target.abi == .msvc or comp.target.os.tag == .windows) {
                    comp.langOpts.setEmulatedCompiler(.msvc);
                }
            } else if (std.mem.eql(u8, arg, "-dump-ast")) {
                comp.dumpAst = true;
            } else if (std.mem.eql(u8, arg, "-dump-tokens")) {
                comp.dumpTokens = true;
            } else if (std.mem.eql(u8, arg, "-dump-raw-tokens")) {
                comp.dumpRawTokens = true;
            } else {
                try comp.diag.add(.{ .tag = .cli_unknown_arg, .extra = .{ .str = arg } }, &.{});
            }
        } else {
            const file = comp.addSourceFromPath(arg) catch |er| {
                return fatal(comp, "{s}", .{@errorName(er)});
            };
            try sources.append(file);
        }
    }

    return false;
}

fn mainExtra(comp: *Compilation, args: [][]const u8) !void {
    comp.defineSystemIncludes() catch |er| switch (er) {
        error.OutOfMemory => return error.OutOfMemory,
        error.SelfExeNotFound => return comp.diag.fatalNoSrc("could not find ZCC executable path", .{}),
        error.ZccIncludeNotFound => return comp.diag.fatalNoSrc("could not find ZCC builtin headers", .{}),
    };

    var sourceFiles = std.ArrayList(Source).init(comp.gpa);
    var macroBuffer = std.ArrayList(u8).init(comp.gpa);

    defer {
        sourceFiles.deinit();
        macroBuffer.deinit();
    }

    const stdOut = std.io.getStdOut().writer();
    if (try parseArgs(comp, stdOut, &sourceFiles, macroBuffer.writer(), args))
        return;

    if (sourceFiles.items.len == 0) {
        return fatal(comp, "no input files", .{});
    } else if (sourceFiles.items.len != 1 and comp.outputName != null) {
        return fatal(comp, "cannot specify -o when generating multiple output files", .{});
    }

    const builtinMacros = try comp.generateBuiltinMacros();
    const userDefinedMacros = try comp.addSourceFromBuffer("<command line>", macroBuffer.items);

    for (sourceFiles.items) |source| {
        processSource(comp, source, builtinMacros, userDefinedMacros) catch |e| switch (e) {
            error.OutOfMemory => return error.OutOfMemory,
            error.FatalError => comp.renderErrors(),
        };
    }
}

fn err(comp: *Compilation, msg: []const u8) !void {
    try comp.diag.add(.{ .tag = .cli_error, .extra = .{ .str = msg } }, &.{});
}

fn fatal(comp: *Compilation, comptime fmt: []const u8, args: anytype) error{FatalError} {
    comp.renderErrors();
    return comp.diag.fatalNoSrc(fmt, args);
}

fn processSource(comp: *Compilation, source: Source, builtinMacro: Source, userDefinedMacros: Source) !void {
    comp.generatedBuffer.items.len = 0;
    var pp = Preprocessor.init(comp);
    defer pp.deinit();

    try pp.addBuiltinMacros();

    _ = try pp.preprocess(builtinMacro);
    _ = try pp.preprocess(userDefinedMacros);
    if (comp.dumpRawTokens) {
        _ = try pp.tokenize(source);
    } else {
        const eof = try pp.preprocess(source);
        try pp.tokens.append(pp.comp.gpa, eof);
    }

    if (comp.onlyPreprocess) {
        comp.renderErrors();

        const file = if (comp.outputName) |some|
            std.fs.cwd().createFile(some, .{}) catch |er|
                return fatal(comp, "{s} when trying to create output file", .{@errorName(er)})
        else
            std.io.getStdOut();
        defer if (comp.outputName != null) file.close();

        var bufWriter = std.io.bufferedWriter(file.writer());
        pp.prettyPrintTokens(file.writer()) catch |er|
            return fatal(comp, "{s} when trying to print tokens", .{@errorName(er)});

        return bufWriter.flush() catch |er|
            fatal(comp, "{s} when trying to print tokens", .{@errorName(er)});
    }

    if (comp.dumpTokens or comp.dumpRawTokens) {
        const locs = pp.tokens.items(.loc);
        var lexer = Lexer{
            .buffer = comp.getSource(locs[0].id).buffer,
            .comp = comp,
            .index = locs[0].byteOffset,
            .source = .generated,
        };
        for (pp.tokens.items(.id), 0..) |*tok, i| {
            const loc = locs[i];
            const res = if (comp.dumpTokens) lexer.nextNoWsAndNewLine() else lexer.next();
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

    var tree = try Parser.parse(&pp);
    defer tree.deinit();

    if (comp.dumpAst) {
        var buffWriter = std.io.bufferedWriter(std.io.getStdOut().writer());
        tree.dump(buffWriter.writer()) catch {};
        buffWriter.flush() catch {};
    }

    const prevErrors = comp.diag.errors;
    comp.renderErrors();

    // do not compile if there were errors
    if (comp.diag.errors != prevErrors)
        return;

    if (comp.target.ofmt != .elf or comp.target.cpu.arch != .x86_64) {
        return fatal(
            comp,
            "unsupported target {s}-{s}-{s}, currently only x86-64 elf is supported",
            .{ @tagName(comp.target.cpu.arch), @tagName(comp.target.os.tag), @tagName(comp.target.abi) },
        );
    }

    const obj = try Codegen.generateTree(comp, tree);
    defer obj.deinit();

    const basename = std.fs.path.basename(source.path);
    const outFileName = comp.outputName orelse try std.fmt.allocPrint(comp.gpa, "{s}{s}", .{
        basename[0 .. basename.len - std.fs.path.extension(source.path).len],
        comp.target.ofmt.fileExt(comp.target.cpu.arch),
    });
    defer if (comp.outputName == null) comp.gpa.free(outFileName);

    const outFile = std.fs.cwd().createFile(outFileName, .{}) catch |er|
        return fatal(comp, "could not create output file '{s}': {s}", .{ outFileName, @errorName(er) });
    defer outFile.close();

    obj.finish(outFile) catch |er|
        return fatal(comp, "could output to object file '{s}': {s}", .{ outFileName, @errorName(er) });

    if (comp.onlyCompile) return;

    // TODO invoke linker
}

test "simple test" {
    _ = @import("Lexer/Lexer.zig");
    _ = @import("Lexer/Preprocessor.zig");
    _ = @import("Basic/Diagnostics.zig");
    _ = @import("Parser/Parser.zig");
    _ = @import("Lexer/Pragma.zig");
    _ = @import("Parser/InitList.zig");
    _ = @import("Basic/Source.zig");
    _ = @import("Basic/LangOpts.zig");
    _ = @import("Basic/Compilation.zig");
    _ = @import("AST/AST.zig");
    _ = @import("AST/Type.zig");
}
