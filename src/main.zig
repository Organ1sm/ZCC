const std = @import("std");
const builtin = @import("builtin");

const Compilation = @import("Basic/Compilation.zig");
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
    \\Feature Options:
    \\  -E                      Only run the preprocessor
    \\  -fcolor-diagnostics     Enable colors in diagnostics
    \\  -fno-color-diagnostics  Disable colors in diagnostics
    \\  -I <dir>                Add directory to include search path
    \\  -isystem                Add directory to system include search path
    \\  -Wall                   Enable all warnings
    \\  -Werror                 Treat all warnings as errors
    \\  -Werror=<warning>       Treat warning as error
    \\  -W<warning>             Enable the specified warning
    \\  -Wno-<warning>          Disable the specified warning
    \\
    \\
;

fn handleArgs(comp: *Compilation, args: [][]const u8) !void {
    try comp.systemIncludeDirs.append("/usr/include");

    var sourceFiles = std.ArrayList(Source).init(comp.gpa);
    defer sourceFiles.deinit();

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
                return stdOut.writeAll(@import("Basic/Info.zig").VersionStr ++ "\n") catch |err| {
                    return comp.diag.fatalNoSrc("{s} when trying to print version", .{@errorName(err)});
                };
            } else if (std.mem.eql(u8, arg, "-E")) {
                comp.onlyPreprocess = true;
            } else if (std.mem.eql(u8, arg, "-fcolor-diagnostics")) {
                comp.diag.color = true;
            } else if (std.mem.eql(u8, arg, "-fno-color-diagnostics")) {
                comp.diag.color = false;
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

    for (sourceFiles.items) |source| {
        processSource(comp, source) catch |e| switch (e) {
            error.OutOfMemory => return error.OutOfMemory,
            error.FatalError => {},
        };

        comp.renderErrors();
    }
}

fn processSource(comp: *Compilation, source: Source) !void {
    var pp = Preprocessor.init(comp);
    defer pp.deinit();

    try pp.preprocess(source);
    if (comp.onlyPreprocess) {
        comp.renderErrors();

        const file = if (comp.outputName) |some|
            std.fs.cwd().createFile(some, .{}) catch |err|
                return comp.diag.fatalNoSrc("{s} when trying to print tokens", .{@errorName(err)})
        else
            std.io.getStdOut();
        defer if (comp.outputName != null) file.close();

        return pp.prettyPrintTokens(file.writer()) catch |err|
            comp.diag.fatalNoSrc("{s} when trying to print tokens", .{@errorName(err)});
    }

    var tree = try Parser.parse(&pp);
    defer tree.deinit();

    comp.renderErrors();

    tree.dump(std.io.getStdOut().writer()) catch {};
}

test "simple test" {
    _ = @import("Lexer/Lexer.zig");
    _ = @import("Lexer/Preprocessor.zig");
    _ = @import("Basic/Source.zig");
    _ = @import("Basic/Compilation.zig");
    _ = @import("AST/AST.zig");
    _ = @import("AST/Type.zig");
}
