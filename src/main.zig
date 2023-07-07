const std = @import("std");
const builtin = @import("builtin");

var GeneralPurposeAllocator = std.heap.GeneralPurposeAllocator(.{}){};

pub fn main() !void {
    const gpa = if (builtin.link_libc) std.heap.raw_c_allocator else GeneralPurposeAllocator.allocator();
    defer if (!builtin.link_libc) {
        _ = GeneralPurposeAllocator.deinit();
    };

    var arenaInstance = std.heap.ArenaAllocator.init(gpa);
    defer arenaInstance.deinit();

    const arena = arenaInstance.allocator();

    const args = std.process.argsAlloc(arena) catch {
        std.debug.print("Out of Memory", .{});
        std.process.exit(1);
    };

    handleArgs(gpa, args) catch |err| {
        fail("{s}", .{@errorName(err)});
        std.process.exit(1);
    };
}

const usage =
    \\Usage {s}: [options] file..
    \\
    \\Options:
    \\  -h, --help      Print this message.
    \\  -v, --version   Print ZCC version.
    \\
    \\
;

fn fail(comptime msg: []const u8, args: anytype) void {
    std.debug.print("error: " ++ msg ++ "\n", args);
}

fn handleArgs(gpa: std.mem.Allocator, args: [][]const u8) !void {
    var sourceFiles = std.ArrayList([]const u8).init(gpa);
    defer sourceFiles.deinit();

    const stdOut = std.io.getStdOut().writer();
    for (args[1..]) |arg| {
        if (std.mem.startsWith(u8, arg, "-")) {
            if (std.mem.eql(u8, arg, "-h") or std.mem.eql(u8, arg, "--help")) {
                return stdOut.print(usage, .{args[0]});
            } else if (std.mem.eql(u8, arg, "-v") or std.mem.eql(u8, arg, "--version")) {
                return stdOut.writeAll(@import("Info.zig").VersionStr ++ "\n");
            } else {
                try stdOut.print(usage, .{args[0]});
                return std.debug.print("unknown command: {s}", .{arg});
            }
        } else {
            try sourceFiles.append(arg);
        }
    }

    if (sourceFiles.items.len == 0) {
        return fail("no input files", .{});
    }
}

test "simple test" {
    _ = @import("Lexer.zig");
}
