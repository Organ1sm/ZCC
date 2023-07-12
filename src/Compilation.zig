const std = @import("std");
const builtin = @import("builtin");
const Source = @import("Source.zig");

const Allocator = std.mem.Allocator;

const Compilation = @This();

gpa: Allocator,
sources: std.StringArrayHashMap(Source),
color: bool,

pub fn init(gpa: Allocator) Compilation {
    return .{
        .gpa = gpa,
        .sources = std.StringArrayHashMap(Source).init(gpa),
        .color = std.io.getStdErr().supportsAnsiEscapeCodes(),
    };
}

pub fn deinit(compilation: *Compilation) void {
    for (compilation.sources.values()) |source| {
        compilation.gpa.free(source.path);
        compilation.gpa.free(source.buffer);
    }

    compilation.sources.deinit();
}

pub fn printErrStart(comp: *Compilation, path: []const u8, lcs: Source.LCS) void {
    if (builtin.os.tag == .windows or !comp.color) {
        std.debug.print("{s}:{d}:{d}: error: ", .{ path, lcs.line, lcs.col });
    } else {
        const RED = "\x1b[31;1m";
        const WHITE = "\x1b[37;1m";

        std.debug.print(WHITE ++ "{s}:{d}:{d}: " ++ RED ++ "error: " ++ WHITE, .{
            path,
            lcs.line,
            lcs.col,
        });
    }
}

pub fn PrintErrEnd(comp: *Compilation, lcs: Source.LCS) void {
    if (builtin.os.tag == .windows or !comp.color) {
        std.debug.print("\n{s}\n", .{lcs.str});
        std.debug.print("{s: >[1]}^\n", .{ "", lcs.col - 1 });
    } else {
        const GREEN = "\x1b[32;1m";
        const RESET = "\x1b[0m";

        std.debug.print("\n" ++ RESET ++ "{s}\n", .{lcs.str});
        std.debug.print("{s: >[1]}" ++ GREEN ++ "^" ++ RESET ++ "\n", .{ "", lcs.col - 1 });
    }
}

pub fn addSource(compilation: *Compilation, path: []const u8) !Source {
    if (compilation.sources.get(path)) |some| return some;

    const file = try std.fs.cwd().openFile(path, .{});
    defer file.close();

    const dupedPath = try compilation.gpa.dupe(u8, path);
    errdefer compilation.gpa.free(dupedPath);

    const contents = try file.reader().readAllAlloc(compilation.gpa, std.math.maxInt(u32));
    errdefer compilation.gpa.free(contents);

    const source = Source{
        .id = @enumFromInt(@as(u15, @intCast(compilation.sources.count()))),
        .path = dupedPath,
        .buffer = contents,
    };

    try compilation.sources.put(dupedPath, source);

    return source;
}
