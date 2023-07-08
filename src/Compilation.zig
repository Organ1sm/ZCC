const std = @import("std");
const Source = @import("Source.zig");

const Allocator = std.mem.Allocator;

const Compilation = @This();

gpa: *Allocator,
sources: std.StringHashMap(Source),

pub fn init(gpa: *Allocator) Compilation {
    return .{
        .gpa = gpa,
        .source = std.StringHashMap(Source).init(gpa),
    };
}

pub fn deinit(compilation: *Compilation) void {
    var it = compilation.sources.iterator();
    while (it.next()) |source| {
        compilation.gpa.free(source.value.path);
        compilation.gpa.free(source.value.buffer);
    }

    compilation.sources.deinit();
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
        .id = @intCast(Source.id),
        .path = dupedPath,
        .buffer = contents,
    };

    try compilation.sources.put(dupedPath, source);

    return source;
}
