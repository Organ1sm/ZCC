const std = @import("std");
const builtin = @import("builtin");
const Compilation = @import("Basic/Compilation.zig");
const Codegen = @import("CodeGen/Codegen.zig");
const Source = @import("Basic/Source.zig");
const Preprocessor = @import("Lexer/Preprocessor.zig");
const Lexer = @import("Lexer/Lexer.zig");
const Parser = @import("Parser/Parser.zig");
const LangOpts = @import("Basic/LangOpts.zig");
const Util = @import("Basic/Util.zig");

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

    const fastExit = @import("builtin").mode != .Debug;

    var comp = Compilation.init(gpa);
    defer comp.deinit();

    comp.addDefaultPragmaHandlers() catch |er| switch (er) {
        error.OutOfMemory => {
            std.debug.print("Out of Memory\n", .{});
            if (fastExit) std.process.exit(1);
            return 1;
        },
    };

    comp.langOpts.setEmulatedCompiler(comp.systemCompiler());

    mainExtra(&comp, args) catch |er| switch (er) {
        error.OutOfMemory => {
            std.debug.print("Out of Memory\n", .{});
            if (fastExit) std.process.exit(1);
            return 1;
        },
        error.StreamTooLong => {
            std.debug.print("maximum file size exceeded\n", .{});
            if (fastExit) std.process.exit(1);
            return 1;
        },
        error.FatalError => {
            comp.renderErrors();
            if (fastExit) std.process.exit(1);
            return 1;
        },
        else => return 1,
    };

    if (fastExit) std.process.exit(@intFromBool(comp.diag.errors != 0));
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
    \\  -S                      Only run preprocess and compilation step
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
    \\  --ld-path=<path>        Use linker specified by <path>
    \\
    \\Debug options:
    \\  -dump-pp               Dump preprocessor state
    \\  -dump-ast              Dump produced AST to stdout
    \\  -dump-tokens           Run preprocessor, dump internal rep of tokens to stdout 
    \\  -dump-raw-tokens        Lex file in raw mode and dump raw tokens to stdout
    \\
;

fn option(arg: []const u8, name: []const u8) ?[]const u8 {
    if (std.mem.startsWith(u8, arg, name) and arg.len > name.len)
        return arg[name.len..];
    return null;
}

pub fn parseArgs(
    comp: *Compilation,
    stdOut: anytype,
    sources: *std.ArrayList(Source),
    linkObjects: *std.ArrayList([]const u8),
    macroBuffer: anytype,
    args: [][]const u8,
) !bool {
    var colorSetting: enum { on, off, unset } = .unset;

    var i: usize = 1;
    while (i < args.len) : (i += 1) {
        const arg = args[i];
        if (std.mem.startsWith(u8, arg, "-") and arg.len > 1) {
            if (std.mem.eql(u8, arg, "-h") or std.mem.eql(u8, arg, "--help")) {
                stdOut.print(usage, .{args[0]}) catch |er| {
                    return fatal(comp, "unable to print usage: {s}", .{Util.errorDescription(er)});
                };
                return true;
            } else if (std.mem.eql(u8, arg, "-v") or std.mem.eql(u8, arg, "--version")) {
                stdOut.writeAll(@import("zcc.zig").VersionStr ++ "\n") catch |er| {
                    return fatal(comp, "unable to print version: {s}", .{Util.errorDescription(er)});
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
                colorSetting = .on;
            } else if (std.mem.eql(u8, arg, "-fno-color-diagnostics")) {
                colorSetting = .off;
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
            } else if (option(arg, "-fmacro-backtrace-limit=")) |limitStr| {
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
            } else if (option(arg, "--emulate=")) |compilerStr| {
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
            } else if (option(arg, "-Werror=")) |errName| {
                try comp.diag.set(errName, .@"error");
            } else if (option(arg, "-Wno-")) |errName| {
                try comp.diag.set(errName, .off);
            } else if (option(arg, "-W")) |errName| {
                try comp.diag.set(errName, .warning);
            } else if (option(arg, "-std=")) |standard| {
                comp.langOpts.setStandard(standard) catch
                    try comp.diag.add(.{ .tag = .cli_invalid_standard, .extra = .{ .str = arg } }, &.{});
            } else if (std.mem.startsWith(u8, arg, "-S")) {
                comp.onlyPreprocessAndCompile = true;
            } else if (option(arg, "--target=")) |triple| {
                const query = std.Target.Query.parse(.{ .arch_os_abi = triple }) catch {
                    return comp.diag.fatalNoSrc("Invalid target '{s}'", .{triple});
                };
                const target = std.zig.system.resolveTargetQuery(query) catch |e| {
                    return comp.diag.fatalNoSrc("unable to resolve target: {s}", .{@errorName(e)});
                };
                comp.target = target;
                comp.langOpts.setEmulatedCompiler(comp.systemCompiler());
            } else if (std.mem.eql(u8, arg, "-dump-pp")) {
                comp.dumpPP = true;
            } else if (std.mem.eql(u8, arg, "-dump-ast")) {
                comp.dumpAst = true;
            } else if (std.mem.eql(u8, arg, "-dump-tokens")) {
                comp.dumpTokens = true;
            } else if (std.mem.eql(u8, arg, "-dump-raw-tokens")) {
                comp.dumpRawTokens = true;
            } else if (option(arg, "-fuse-ld=")) |linkerName| {
                comp.useLinker = std.meta.stringToEnum(Compilation.Linker, linkerName) orelse {
                    try comp.diag.add(.{ .tag = .cli_unknown_linker, .extra = .{ .str = arg } }, &.{});
                    continue;
                };
            } else if (option(arg, "--ld-path=")) |linkerPath| {
                comp.linkerPath = linkerPath;
            } else {
                try comp.diag.add(.{ .tag = .cli_unknown_arg, .extra = .{ .str = arg } }, &.{});
            }
        } else if (std.mem.endsWith(u8, arg, ".o") or std.mem.endsWith(u8, arg, ".obj")) {
            try linkObjects.append(arg);
        } else {
            const file = addSource(comp, arg) catch |er| {
                return fatal(comp, "unable to add source file '{s}': {s}", .{ arg, Util.errorDescription(er) });
            };
            try sources.append(file);
        }
    }

    comp.diag.color = switch (colorSetting) {
        .on => true,
        .off => false,
        .unset => Util.fileSupportsColor(std.io.getStdOut()) and !std.process.hasEnvVarConstant("NO_COLOR"),
    };

    return false;
}

fn mainExtra(comp: *Compilation, args: [][]const u8) !void {
    var sourceFiles = std.ArrayList(Source).init(comp.gpa);
    var macroBuffer = std.ArrayList(u8).init(comp.gpa);
    var linkObjects = std.ArrayList([]const u8).init(comp.gpa);

    defer {
        sourceFiles.deinit();
        macroBuffer.deinit();
        linkObjects.deinit();
    }

    const stdOut = std.io.getStdOut().writer();
    if (try parseArgs(comp, stdOut, &sourceFiles, &linkObjects, macroBuffer.writer(), args))
        return;

    const linking = !(comp.onlyPreprocess or comp.onlyCompile or comp.onlyPreprocessAndCompile);
    var tempFileCount: u32 = 0;
    defer if (linking) for (linkObjects.items[linkObjects.items.len - tempFileCount ..]) |obj| {
        std.fs.deleteFileAbsolute(obj) catch {};
        comp.gpa.free(obj);
    };

    if (sourceFiles.items.len == 0) {
        return fatal(comp, "no input files", .{});
    } else if ((sourceFiles.items.len != 1 and comp.outputName != null) and !linking) {
        return fatal(comp, "cannot specify -o when generating multiple output files", .{});
    }

    if (!linking)
        for (linkObjects.items) |obj|
            try comp.diag.add(.{ .tag = .cli_unused_link_object, .extra = .{ .str = obj } }, &.{});

    if (linking and comp.linkerPath == null)
        comp.linkerPath = comp.getLinkerPath();

    comp.defineSystemIncludes() catch |er| switch (er) {
        error.OutOfMemory => return error.OutOfMemory,
        error.SelfExeNotFound => return fatal(comp, "unable to find ZCC executable path", .{}),
        error.ZccIncludeNotFound => return fatal(comp, "unable to find ZCC builtin headers", .{}),
    };

    const builtinMacros = try comp.generateBuiltinMacros();
    const userDefinedMacros = try comp.addSourceFromBuffer("<command line>", macroBuffer.items);

    const fastExit = @import("builtin").mode != .Debug;
    if (fastExit and sourceFiles.items.len == 1) {
        processSource(comp, sourceFiles.items[0], &linkObjects, &tempFileCount, builtinMacros, userDefinedMacros, fastExit) catch |e| switch (e) {
            error.OutOfMemory => return error.OutOfMemory,
            error.FatalError => {
                comp.renderErrors();
                exitWithCleanup(linkObjects.items, tempFileCount, 1);
            },
        };
        unreachable;
    }

    for (sourceFiles.items) |source| {
        processSource(comp, source, &linkObjects, &tempFileCount, builtinMacros, userDefinedMacros, fastExit) catch |e| switch (e) {
            error.OutOfMemory => return error.OutOfMemory,
            error.FatalError => comp.renderErrors(),
        };
    }

    if (comp.diag.errors != 0) {
        if (fastExit) exitWithCleanup(linkObjects.items, tempFileCount, 1);
        return;
    }

    if (linking) {
        try invokeLinker(comp, linkObjects.items, tempFileCount);
        if (fastExit)
            exitWithCleanup(linkObjects.items, tempFileCount, 0);
    }

    if (fastExit)
        std.process.exit(0);
}

fn addSource(comp: *Compilation, path: []const u8) !Source {
    return comp.addSourceFromPath(path) catch |er| switch (er) {
        error.FileNotFound => {
            if (std.mem.eql(u8, "-", path)) {
                const stdin = std.io.getStdIn().reader();
                const input = try stdin.readAllAlloc(comp.gpa, std.math.maxInt(u32));
                defer comp.gpa.free(input);
                return comp.addSourceFromBuffer("<stdin>", input);
            }
            return er;
        },
        else => return er,
    };
}

fn err(comp: *Compilation, msg: []const u8) !void {
    try comp.diag.add(.{ .tag = .cli_error, .extra = .{ .str = msg } }, &.{});
}

fn fatal(comp: *Compilation, comptime fmt: []const u8, args: anytype) error{FatalError} {
    comp.renderErrors();
    return comp.diag.fatalNoSrc(fmt, args);
}

fn processSource(
    comp: *Compilation,
    source: Source,
    linkObjects: *std.ArrayList([]const u8),
    tempFileCount: *u32,
    builtinMacro: Source,
    userDefinedMacros: Source,
    comptime fastExit: bool,
) !void {
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
                return fatal(comp, "unable to create output file '{s}': {s}", .{ some, Util.errorDescription(er) })
        else
            std.io.getStdOut();
        defer if (comp.outputName != null) file.close();

        var bufWriter = std.io.bufferedWriter(file.writer());
        pp.prettyPrintTokens(bufWriter.writer()) catch |er|
            return fatal(comp, "unable to write result: {s}", .{Util.errorDescription(er)});

        bufWriter.flush() catch |er|
            return fatal(comp, "unable to write result: {s}", .{Util.errorDescription(er)});

        if (fastExit)
            std.process.exit(0); // Not linking, no need for clean up.

        return;
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
        const stdout = std.io.getStdOut();
        const color = comp.diag.color and Util.fileSupportsColor(stdout);

        var buffWriter = std.io.bufferedWriter(stdout.writer());
        tree.dump(color, buffWriter.writer()) catch {};
        buffWriter.flush() catch {};
    }

    const prevErrors = comp.diag.errors;
    comp.renderErrors();

    // do not compile if there were errors
    if (comp.diag.errors != prevErrors) {
        if (fastExit)
            exitWithCleanup(linkObjects.items, tempFileCount.*, 1);
        return; // Don't compile if there were errors
    }

    if (comp.target.ofmt != .elf or comp.target.cpu.arch != .x86_64) {
        return fatal(
            comp,
            "unsupported target {s}-{s}-{s}, currently only x86-64 elf is supported",
            .{ @tagName(comp.target.cpu.arch), @tagName(comp.target.os.tag), @tagName(comp.target.abi) },
        );
    }

    const obj = try Codegen.generateTree(comp, tree);
    defer obj.deinit();

    const outFileName = if (comp.onlyCompile) blk: {
        const basename = std.fs.path.basename(source.path);
        break :blk comp.outputName orelse try std.fmt.allocPrint(comp.gpa, "{s}{s}", .{
            basename[0 .. basename.len - std.fs.path.extension(source.path).len],
            comp.target.ofmt.fileExt(comp.target.cpu.arch),
        });
    } else blk: {
        const randomBytesCount = 12;
        const subPathLen = comptime std.fs.base64_encoder.calcSize(randomBytesCount);

        var randomBytes: [randomBytesCount]u8 = undefined;
        std.crypto.random.bytes(&randomBytes);
        var randomName: [subPathLen]u8 = undefined;
        _ = std.fs.base64_encoder.encode(&randomName, &randomBytes);

        break :blk try std.fmt.allocPrint(comp.gpa, "/tmp/{s}{s}", .{
            randomName, comp.target.ofmt.fileExt(comp.target.cpu.arch),
        });
    };
    defer if (comp.onlyCompile) comp.gpa.free(outFileName);

    const outFile = std.fs.cwd().createFile(outFileName, .{}) catch |er|
        return fatal(comp, "unable to create output file '{s}': {s}", .{ outFileName, Util.errorDescription(er) });
    defer outFile.close();

    obj.finish(outFile) catch |er|
        return fatal(comp, "could output to object file '{s}': {s}", .{ outFileName, Util.errorDescription(er) });

    if (comp.onlyCompile) {
        if (fastExit)
            std.process.exit(0); // Not linking, no need clean up.
        return;
    }

    try linkObjects.append(outFileName);
    tempFileCount.* += 1;
    if (fastExit) {
        try invokeLinker(comp, linkObjects.items, tempFileCount.*);
        exitWithCleanup(linkObjects, tempFileCount.*, 0);
    }
}

fn invokeLinker(comp: *Compilation, linkObjects: []const []const u8, tempFileCount: u32) !void {
    const argsLen = 1 // linker name
    + 2 // -o output
    + 2 // -dynamic-linker <path>
    + 1 // -lc
    + 1 // -L/lib
    + 1 // -L/usr/lib
    + 1 // Scrt1.0
    + linkObjects.len;

    var argv = try std.ArrayList([]const u8).initCapacity(comp.gpa, argsLen);
    defer argv.deinit();

    argv.appendAssumeCapacity(comp.linkerPath.?);
    argv.appendAssumeCapacity("-o");
    argv.appendAssumeCapacity(comp.outputName orelse "a.out");
    argv.appendAssumeCapacity("-dynamic-linker");
    argv.appendAssumeCapacity(comp.target.standardDynamicLinkerPath().get().?);
    argv.appendAssumeCapacity("-L/lib");
    argv.appendAssumeCapacity("-L/usr/lib");
    argv.appendAssumeCapacity("-lc");
    argv.appendAssumeCapacity("/usr/lib/Scrt1.o"); // TODO very bad
    argv.appendSliceAssumeCapacity(linkObjects);

    var child = std.ChildProcess.init(argv.items, comp.gpa);
    // TODO handle better
    child.stdin_behavior = .Inherit;
    child.stdout_behavior = .Inherit;
    child.stderr_behavior = .Inherit;

    const term = child.spawnAndWait() catch |er| {
        return fatal(comp, "unable to spawn linker: {s}", .{Util.errorDescription(er)});
    };
    switch (term) {
        .Exited => |code| if (code != 0) exitWithCleanup(linkObjects, tempFileCount, code),
        else => std.process.abort(),
    }

    if (@import("builtin").mode != .Debug)
        exitWithCleanup(linkObjects, tempFileCount, 1);
}

fn exitWithCleanup(linkObjects: []const []const u8, tempFileCount: u32, code: u8) noreturn {
    for (linkObjects[linkObjects.len - tempFileCount ..]) |obj|
        std.fs.deleteFileAbsolute(obj) catch {};

    std.process.exit(code);
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
