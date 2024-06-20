const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
const process = std.process;
const Codegen = @import("CodeGen/Codegen.zig");
const Compilation = @import("Basic/Compilation.zig");
const LangOpts = @import("Basic/LangOpts.zig");
const Lexer = @import("Lexer/Lexer.zig");
const Preprocessor = @import("Lexer/Preprocessor.zig");
const Parser = @import("Parser/Parser.zig");
const Source = @import("Basic/Source.zig");
const Util = @import("Basic/Util.zig");
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
tempFileCount: u32 = 0,

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

// linker options
useLinker: Linker = .ld,
linkerPath: ?[]const u8 = null,

/// Full Path to the zcc executable
zccName: []const u8 = "",

pub fn deinit(d: *Driver) void {
    const linking = !(d.onlyPreprocess or d.onlySyntax or d.onlyCompile or d.onlyPreprocessAndCompile);
    if (linking) for (d.linkObjects.items[d.linkObjects.items.len - d.tempFileCount ..]) |obj| {
        std.fs.deleteFileAbsolute(obj) catch {};
        d.comp.gpa.free(obj);
    };
    d.inputs.deinit(d.comp.gpa);
    d.linkObjects.deinit(d.comp.gpa);
    d.* = undefined;
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
    \\  -fsyntax-only           Only run the preprocessor parser, and semantic analysis stages
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
    \\  -dump-ir               Dump ir to stdout
    \\  -dump-tokens           Run preprocessor, dump internal rep of tokens to stdout 
    \\  -dump-raw-tokens       Lex file in raw mode and dump raw tokens to stdout
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
                stdOut.writeAll(@import("zcc.zig").VersionStr ++ "\n") catch |er| {
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
            } else if (std.mem.eql(u8, arg, "-c")) {
                d.onlyCompile = true;
            } else if (std.mem.eql(u8, arg, "-E")) {
                d.onlyPreprocess = true;
            } else if (std.mem.eql(u8, arg, "-fcolor-diagnostics")) {
                colorSetting = .on;
            } else if (std.mem.eql(u8, arg, "-fno-color-diagnostics")) {
                colorSetting = .off;
            } else if (std.mem.eql(u8, arg, "-fshort-enums")) {
                d.comp.langOpts.shortEnums = true;
            } else if (std.mem.eql(u8, arg, "-fno-short-enums")) {
                d.comp.langOpts.shortEnums = false;
            } else if (std.mem.eql(u8, arg, "-fdeclspec")) {
                d.comp.langOpts.declSpecAttrs = true;
            } else if (std.mem.eql(u8, arg, "-fno-declspec")) {
                d.comp.langOpts.declSpecAttrs = false;
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
                try d.comp.includeDirs.append(path);
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
                try d.comp.systemIncludeDirs.append(path);
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
            } else if (std.mem.eql(u8, arg, "-pedantic")) {
                d.comp.diagnostics.options.pedantic = .warning;
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
            } else if (std.mem.startsWith(u8, arg, "-S")) {
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
            } else if (option(arg, "-fuse-ld=")) |linkerName| {
                d.useLinker = std.meta.stringToEnum(Linker, linkerName) orelse {
                    try d.comp.addDiagnostic(.{ .tag = .cli_unknown_linker, .extra = .{ .str = arg } }, &.{});
                    continue;
                };
            } else if (option(arg, "--ld-path=")) |linkerPath| {
                d.linkerPath = linkerPath;
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

fn err(d: *Driver, msg: []const u8) !void {
    try d.comp.addDiagnostic(.{ .tag = .cli_error, .extra = .{ .str = msg } }, &.{});
}

fn fatal(d: *Driver, comptime fmt: []const u8, args: anytype) error{FatalError} {
    d.comp.renderErrors();
    return d.comp.diagnostics.fatalNoSrc(fmt, args);
}

pub fn main(d: *Driver, args: [][]const u8) !void {
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

    if (linking and d.linkerPath == null)
        d.linkerPath = d.getLinkerPath();

    d.comp.defineSystemIncludes(d.zccName) catch |er| switch (er) {
        error.OutOfMemory => return error.OutOfMemory,
        error.ZccIncludeNotFound => return d.fatal("unable to find ZCC builtin headers", .{}),
    };

    const builtinMacros = try d.comp.generateBuiltinMacros();
    const userDefinedMacros = try d.comp.addSourceFromBuffer("<command line>", macroBuffer.items);

    const fastExit = @import("builtin").mode != .Debug;
    if (fastExit and d.inputs.items.len == 1) {
        processSource(d.inputs.items[0], builtinMacros, userDefinedMacros, fastExit) catch |e| switch (e) {
            error.OutOfMemory => return error.OutOfMemory,
            error.FatalError => {
                d.comp.renderErrors();
                d.exitWithCleanup(1);
            },
        };
        unreachable;
    }

    for (d.inputs.items) |source| {
        d.processSource(source, builtinMacros, userDefinedMacros, fastExit) catch |e| switch (e) {
            error.OutOfMemory => return error.OutOfMemory,
            error.FatalError => d.comp.renderErrors(),
        };
    }

    if (d.comp.diagnostics.errors != 0) {
        if (fastExit) d.exitWithCleanup(1);
        return;
    }

    if (linking) {
        try d.invokeLinker();
        if (fastExit)
            d.exitWithCleanup(0);
    }

    if (fastExit)
        std.process.exit(0);
}

fn processSource(
    d: *Driver,
    source: Source,
    builtinMacro: Source,
    userDefinedMacros: Source,
    comptime fastExit: bool,
) !void {
    d.comp.generatedBuffer.items.len = 0;
    var pp = Preprocessor.init(d.comp);
    defer pp.deinit();

    if (d.dumpPP) pp.verbose = true;
    if (d.onlyPreprocess) pp.preserveWhitespace = true;
    try pp.addBuiltinMacros();

    _ = try pp.preprocess(builtinMacro);
    _ = try pp.preprocess(userDefinedMacros);
    if (d.dumpRawTokens) {
        _ = try pp.tokenize(source);
    } else {
        const eof = try pp.preprocess(source);
        try pp.tokens.append(pp.comp.gpa, eof);
    }

    if (d.onlyPreprocess) {
        d.comp.renderErrors();

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

    var tree = try Parser.parse(&pp);
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

    if (d.dumpIR)
        try @import("CodeGen/CodeGen.zig").generateTree(d.comp, tree);

    const obj = try Codegen.generateTree(d.comp, tree);
    defer obj.deinit();

    const outFileName = if (d.onlyCompile) blk: {
        const basename = std.fs.path.basename(source.path);
        break :blk d.outputName orelse try std.fmt.allocPrint(d.comp.gpa, "{s}{s}", .{
            basename[0 .. basename.len - std.fs.path.extension(source.path).len],
            d.comp.target.ofmt.fileExt(d.comp.target.cpu.arch),
        });
    } else blk: {
        const randomBytesCount = 12;
        const subPathLen = comptime std.fs.base64_encoder.calcSize(randomBytesCount);

        var randomBytes: [randomBytesCount]u8 = undefined;
        std.crypto.random.bytes(&randomBytes);
        var randomName: [subPathLen]u8 = undefined;
        _ = std.fs.base64_encoder.encode(&randomName, &randomBytes);

        break :blk try std.fmt.allocPrint(d.comp.gpa, "/tmp/{s}{s}", .{
            randomName, d.comp.target.ofmt.fileExt(d.comp.target.cpu.arch),
        });
    };
    defer if (d.onlyCompile) d.comp.gpa.free(outFileName);

    const outFile = std.fs.cwd().createFile(outFileName, .{}) catch |er|
        return d.fatal("unable to create output file '{s}': {s}", .{ outFileName, Util.errorDescription(er) });
    defer outFile.close();

    obj.finish(outFile) catch |er|
        return d.fatal("could output to object file '{s}': {s}", .{ outFileName, Util.errorDescription(er) });

    if (d.onlyCompile) {
        if (fastExit)
            std.process.exit(0); // Not linking, no need clean up.
        return;
    }

    try d.linkObjects.append(d.comp.gpa, outFileName);
    d.tempFileCount += 1;
    if (fastExit) {
        try invokeLinker();
        d.exitWithCleanup(0);
    }
}

fn invokeLinker(d: *Driver) !void {
    const argsLen = 1 // linker name
    + 2 // -o output
    + 2 // -dynamic-linker <path>
    + 1 // -lc
    + 1 // -L/lib
    + 1 // -L/usr/lib
    + 1 // Scrt1.0
    + d.linkObjects.items.len;

    var argv = try std.ArrayList([]const u8).initCapacity(d.comp.gpa, argsLen);
    defer argv.deinit();

    argv.appendAssumeCapacity(d.linkerPath.?);
    argv.appendAssumeCapacity("-o");
    argv.appendAssumeCapacity(d.outputName orelse "a.out");
    argv.appendAssumeCapacity("-dynamic-linker");
    argv.appendAssumeCapacity(d.comp.target.standardDynamicLinkerPath().get().?);
    argv.appendAssumeCapacity("-L/lib");
    argv.appendAssumeCapacity("-L/usr/lib");
    argv.appendAssumeCapacity("-lc");
    argv.appendAssumeCapacity("/usr/lib/Scrt1.o"); // TODO very bad
    argv.appendSliceAssumeCapacity(d.linkObjects.items);

    var child = std.process.Child.init(argv.items, d.comp.gpa);
    // TODO handle better
    child.stdin_behavior = .Inherit;
    child.stdout_behavior = .Inherit;
    child.stderr_behavior = .Inherit;

    const term = child.spawnAndWait() catch |er| {
        return d.fatal("unable to spawn linker: {s}", .{Util.errorDescription(er)});
    };
    switch (term) {
        .Exited => |code| if (code != 0) d.exitWithCleanup(code),
        else => std.process.abort(),
    }

    if (@import("builtin").mode != .Debug)
        d.exitWithCleanup(1);
}

fn exitWithCleanup(d: *Driver, code: u8) noreturn {
    for (d.linkObjects.items[d.linkObjects.items.len - d.tempFileCount ..]) |obj|
        std.fs.deleteFileAbsolute(obj) catch {};
    std.process.exit(code);
}

pub fn getLinkerPath(d: *Driver) []const u8 {
    // TODO extremely incomplete
    return switch (d.useLinker) {
        .ld => "/usr/bin/ld",
        .bfd => "/usr/bin/ld.bfd",
        .gold => "/usr/bin/ld.gold",
        .lld => "/usr/bin/ld.lld",
        .mold => "/usr/bin/ld.mold",
    };
}
