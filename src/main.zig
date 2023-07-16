const std = @import("std");
const builtin = @import("builtin");

const Compilation = @import("Compilation.zig");
const Source = @import("Source.zig");
const Preprocessor = @import("Preprocessor.zig");
const Parser = @import("Parser.zig");

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
        std.process.exit(1);
    };

    handleArgs(gpa, args) catch |err| switch (err) {
        error.OutOfMemory => {
            std.debug.print("Out of Memory\n", .{});
            return 1;
        },
        error.FatalError => return 1,
        else => return 1,
    };

    return 0;
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
    \\  -Wall                   Enable all warnings
    \\  -Werror                 Treat all warnings as errors
    \\  -Werror=<warning>       Treat warning as error
    \\  -W<warning>             Enable the specified warning
    \\  -Wno-<warning>          Disable the specified warning
    \\
    \\
;

fn handleArgs(gpa: std.mem.Allocator, args: [][]const u8) !void {
    var comp = Compilation.init(gpa);
    defer comp.deinit();

    var sourceFiles = std.ArrayList(Source).init(gpa);
    defer sourceFiles.deinit();

    const stdOut = std.io.getStdOut().writer();
    for (args[1..]) |arg| {
        if (std.mem.startsWith(u8, arg, "-")) {
            if (std.mem.eql(u8, arg, "-h") or std.mem.eql(u8, arg, "--help")) {
                return stdOut.print(usage, .{args[0]}) catch |err| {
                    return comp.diag.fatalNoSrc("{s} when trying to print usage", .{@errorName(err)});
                };
            } else if (std.mem.eql(u8, arg, "-v") or std.mem.eql(u8, arg, "--version")) {
                return stdOut.writeAll(@import("Info.zig").VersionStr ++ "\n") catch |err| {
                    return comp.diag.fatalNoSrc("{s} when trying to print version", .{@errorName(err)});
                };
            } else if (std.mem.eql(u8, arg, "-fcolor-diagnostics")) {
                comp.diag.color = true;
            } else if (std.mem.eql(u8, arg, "-fno-color-diagnostics")) {
                comp.diag.color = false;
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
    }

    for (sourceFiles.items) |source| {
        var pp = Preprocessor.init(&comp);
        defer pp.deinit();

        var parser = Parser{
            .pp = &pp,
            .tokens = pp.tokens.items,
        };

        pp.preprocess(source) catch |e| switch (e) {
            error.OutOfMemory => return error.OutOfMemory,
            error.FatalError => {
                comp.renderErrors();
                comp.diag.list.items.len = 0;
                continue;
            },
        };

        parser.parse() catch |e| switch (e) {
            error.OutOfMemory => return error.OutOfMemory,
            error.ParsingFailed, error.FatalError => {},
        };

    }
}

test "simple test" {
    _ = @import("Lexer.zig");
    _ = @import("Preprocessor.zig");
    _ = @import("Source.zig");
    _ = @import("Compilation.zig");
    _ = @import("AST.zig");
    _ = @import("Type.zig");
}
