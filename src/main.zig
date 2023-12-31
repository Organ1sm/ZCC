const std = @import("std");
const builtin = @import("builtin");
const Compilation = @import("Basic/Compilation.zig");
const Codegen = @import("CodeGen/Codegen.zig");
const Source = @import("Basic/Source.zig");
const Preprocessor = @import("Lexer/Preprocessor.zig");
const Parser = @import("Parser/Parser.zig");

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

    comp.addDefaultPragmaHandlers() catch |err| switch (err) {
        error.OutOfMemory => {
            std.debug.print("Out of Memory\n", .{});
            return 1;
        },
    };

    handleArgs(&comp, args) catch |err| switch (err) {
        error.OutOfMemory => {
            std.debug.print("Out of Memory\n", .{});
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
    \\  -o <file>               Write output to <file>
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
    \\  --dump-ast              Dump produced AST to stdout
    \\
    \\
;

fn handleArgs(comp: *Compilation, args: [][]const u8) !void {
    comp.defineSystemIncludes() catch |err| switch (err) {
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
    var i: usize = 1;
    while (i < args.len) : (i += 1) {
        const arg = args[i];
        if (std.mem.startsWith(u8, arg, "-")) {
            if (std.mem.eql(u8, arg, "-h") or std.mem.eql(u8, arg, "--help")) {
                return stdOut.print(usage, .{args[0]}) catch |err| {
                    return comp.diag.fatalNoSrc("{s} when trying to print usage", .{@errorName(err)});
                };
            } else if (std.mem.eql(u8, arg, "-v") or std.mem.eql(u8, arg, "--version")) {
                return stdOut.writeAll(@import("zcc.zig").VersionStr ++ "\n") catch |err| {
                    return comp.diag.fatalNoSrc("{s} when trying to print version", .{@errorName(err)});
                };
            } else if (std.mem.eql(u8, arg, "-c")) {
                comp.onlyCompile = true;
            } else if (std.mem.startsWith(u8, arg, "-D")) {
                var macro = arg["-D".len..];
                if (macro.len == 0) {
                    i += 1;
                    if (i >= args.len)
                        return comp.diag.fatalNoSrc("expected argument after -D", .{});

                    macro = args[i];
                }

                var value: []const u8 = "1";
                if (std.mem.indexOfScalar(u8, macro, '=')) |some| {
                    value = macro[some + 1 ..];
                    macro = macro[0..some];
                }

                try macroBuffer.writer().print("#define {s} {s}\n", .{ macro, value });
            } else if (std.mem.startsWith(u8, arg, "-U")) {
                var macro = arg["-U".len..];
                if (macro.len == 0) {
                    i += 1;
                    if (i >= args.len)
                        return comp.diag.fatalNoSrc("expected argument after -U", .{});

                    macro = args[i];
                }

                try macroBuffer.writer().print("#undef {s} \n", .{macro});
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
            } else if (std.mem.eql(u8, arg, "-fdollars-in-identifiers")) {
                comp.langOpts.dollarsInIdentifiers = true;
            } else if (std.mem.eql(u8, arg, "-fno-dollars-in-identifiers")) {
                comp.langOpts.dollarsInIdentifiers = false;
            } else if (std.mem.startsWith(u8, arg, "-fmacro-backtrace-limit=")) {
                const limitStr = arg["-fmacro-backtrace-limit=".len..];
                var limit = std.fmt.parseInt(u32, limitStr, 10) catch
                    return comp.diag.fatalNoSrc("-fmacro-backtrace-limit takes a number argument", .{});

                if (limit == 0) limit = std.math.maxInt(u32);
                comp.diag.macroBacktraceLimit = limit;
            } else if (std.mem.startsWith(u8, arg, "-I")) {
                var path = arg["-I".len..];
                if (path.len == 0) {
                    i += 1;
                    if (i >= args.len)
                        return comp.diag.fatalNoSrc("expected argument after -I", .{});

                    path = args[i];
                }
                try comp.includeDirs.append(path);
            } else if (std.mem.startsWith(u8, arg, "-isystem")) {
                var path = arg["-isystem".len..];
                if (path.len == 0) {
                    i += 1;
                    if (i >= args.len)
                        return comp.diag.fatalNoSrc("expected argument after -isystem", .{});

                    path = args[i];
                }
                try comp.systemIncludeDirs.append(path);
            } else if (std.mem.startsWith(u8, arg, "-o")) {
                var filename = arg["-o".len..];
                if (filename.len == 0) {
                    i += 1;
                    if (i >= args.len)
                        return comp.diag.fatalNoSrc("expected argument after -o", .{});

                    filename = args[i];
                }
                comp.outputName = filename;
            } else if (std.mem.eql(u8, arg, "-Wall")) {
                comp.diag.setAll(.warning);
            } else if (std.mem.eql(u8, arg, "-Werror")) {
                comp.diag.setAll(.@"error");
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
                comp.langOpts.setStandard(standard) catch return comp.diag.fatalNoSrc("Invalid standard '{s}'", .{standard});
            } else if (std.mem.startsWith(u8, arg, "--target=")) {
                const triple = arg["--target=".len..];
                const cross = std.zig.CrossTarget.parse(.{ .arch_os_abi = triple }) catch
                    return comp.diag.fatalNoSrc("Invalid target '{s}'", .{triple});
                comp.target = cross.toTarget(); // TODO deprecated
            } else if (std.mem.eql(u8, arg, "--dump-ast")) {
                comp.dumpAst = true;
            } else {
                try stdOut.print(usage, .{args[0]});
                return std.debug.print("unknown command: {s}", .{arg});
            }
        } else {
            const file = comp.addSource(arg) catch |err| return comp.diag.fatalNoSrc("{s}", .{@errorName(err)});
            try sourceFiles.append(file);
        }
    }

    if (sourceFiles.items.len == 0) {
        return comp.diag.fatalNoSrc("no input files", .{});
    } else if (sourceFiles.items.len != 1 and comp.outputName != null) {
        return comp.diag.fatalNoSrc("cannot specify -o when generating multiple output files", .{});
    }

    const builtinMacros = try comp.generateBuiltinMacros();
    const userDefinedMacros = blk: {
        const dupedPath = try comp.gpa.dupe(u8, "<command line>");
        errdefer comp.gpa.free(dupedPath);

        const contents = try macroBuffer.toOwnedSlice();
        errdefer comp.gpa.free(contents);

        const source = Source{
            .id = @as(Source.ID, @enumFromInt(comp.sources.count() + 2)),
            .path = dupedPath,
            .buffer = contents,
        };
        try comp.sources.put(dupedPath, source);
        break :blk source;
    };

    for (sourceFiles.items) |source| {
        processSource(comp, source, builtinMacros, userDefinedMacros) catch |e| switch (e) {
            error.OutOfMemory => return error.OutOfMemory,
            error.FatalError => comp.renderErrors(),
        };
    }
}

fn processSource(comp: *Compilation, source: Source, builtinMacro: Source, userDefinedMacros: Source) !void {
    comp.generatedBuffer.items.len = 0;
    var pp = Preprocessor.init(comp);
    defer pp.deinit();

    try pp.addBuiltinMacros();

    _ = try pp.preprocess(builtinMacro);
    _ = try pp.preprocess(userDefinedMacros);
    const eof = try pp.preprocess(source);
    try pp.tokens.append(pp.compilation.gpa, eof);

    if (comp.onlyPreprocess) {
        comp.renderErrors();

        const file = if (comp.outputName) |some|
            std.fs.cwd().createFile(some, .{}) catch |err|
                return comp.diag.fatalNoSrc("{s} when trying to create output file", .{@errorName(err)})
        else
            std.io.getStdOut();
        defer if (comp.outputName != null) file.close();

        return pp.prettyPrintTokens(file.writer()) catch |err|
            comp.diag.fatalNoSrc("{s} when trying to print tokens", .{@errorName(err)});
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
        return comp.diag.fatalNoSrc(
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

    const outFile = std.fs.cwd().createFile(outFileName, .{}) catch |err|
        return comp.diag.fatalNoSrc("could not create output file '{s}': {s}", .{ outFileName, @errorName(err) });
    defer outFile.close();

    obj.finish(outFile) catch |err|
        return comp.diag.fatalNoSrc("could output to object file '{s}': {s}", .{ outFileName, @errorName(err) });

    if (comp.onlyCompile) return;

    // TODO invoke linker
}

test "simple test" {
    _ = @import("Lexer/Lexer.zig");
    _ = @import("Lexer/Preprocessor.zig");
    _ = @import("Lexer/Pragma.zig");
    _ = @import("Parser/InitList.zig");
    _ = @import("Basic/Source.zig");
    _ = @import("Basic/LangOpts.zig");
    _ = @import("Basic/Compilation.zig");
    _ = @import("AST/AST.zig");
    _ = @import("AST/Type.zig");
    _ = @import("CodeGen/Codegen.zig");
}
