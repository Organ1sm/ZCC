const std = @import("std");
const builtin = @import("builtin");
const Source = @import("Source.zig");
const Diagnostics = @import("Diagnostics.zig");
const Token = @import("Token.zig").Token;

const Allocator = std.mem.Allocator;

const Compilation = @This();
pub const Error = error{
    /// A fatal error has ocurred and compilation has stopped.
    FatalError,
} || Allocator.Error;

gpa: Allocator,
sources: std.StringArrayHashMap(Source),
diag: Diagnostics,
includeDirs: std.ArrayList([]const u8),
systemIncludeDirs: std.ArrayList([]const u8),

pub fn init(gpa: Allocator) Compilation {
    return .{
        .gpa = gpa,
        .sources = std.StringArrayHashMap(Source).init(gpa),
        .diag = Diagnostics.init(gpa),
        .includeDirs = std.ArrayList([]const u8).init(gpa),
        .systemIncludeDirs = std.ArrayList([]const u8).init(gpa),
    };
}

pub fn deinit(compilation: *Compilation) void {
    for (compilation.sources.values()) |source| {
        compilation.gpa.free(source.path);
        compilation.gpa.free(source.buffer);
    }

    compilation.sources.deinit();
    compilation.diag.deinit();
    compilation.includeDirs.deinit();
    compilation.systemIncludeDirs.deinit();
}

pub fn getSource(comp: *Compilation, id: Source.ID) Source {
    var src = comp.sources.values()[id.index()];
    if (id.isGenerated())
        src.id.markGenerated();

    return src;
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

pub fn findInclude(comp: *Compilation, token: Token, filename: []const u8, searchWord: bool) !Source {
    var pathBuff: [std.fs.MAX_PATH_BYTES]u8 = undefined;
    var fib = std.heap.FixedBufferAllocator.init(&pathBuff);

    if (searchWord) blk: {
        const source = comp.getSource(token.source);
        const path = if (std.fs.path.dirname(source.path)) |some|
            std.fs.path.join(fib.allocator(), &.{ some, filename }) catch break :blk
        else
            filename;

        if (comp.addSource(path)) |some|
            return some
        else |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            else => {},
        }
    }

    for (comp.includeDirs.items) |dir| {
        fib.end_index = 0;
        const path = std.fs.path.join(fib.allocator(), &.{ dir, filename }) catch continue;

        if (comp.addSource(path)) |some|
            return some
        else |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            else => {},
        }
    }

    for (comp.systemIncludeDirs.items) |dir| {
        fib.end_index = 0;
        const path = std.fs.path.join(fib.allocator(), &.{ dir, filename }) catch continue;

        if (comp.addSource(path)) |some|
            return some
        else |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            else => {},
        }
    }

    return comp.fatal(token, "'{s}' not found", .{filename});
}

pub fn fatal(comp: *Compilation, token: Token, comptime fmt: []const u8, args: anytype) Error {
    const source = comp.getSource(token.source);
    const lcs = source.lineColString(token.loc.start);

    return comp.diag.fatal(source.path, lcs, fmt, args);
}

pub const fatalNoSrc = Diagnostics.fatalNoSrc;
pub const renderErrors = Diagnostics.render;
