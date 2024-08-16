const std = @import("std");
const mem = std.mem;
const builtin = @import("builtin");
const isWindows = builtin.os.tag == .windows;

fn findProgramByNameFake(allocator: std.mem.Allocator, name: []const u8, buf: []u8) ?[]const u8 {
    _ = buf;
    _ = name;
    _ = allocator;
    @panic("TODO");
}

fn canExecuteFake(paths: []const []const u8, path: []const u8) bool {
    _ = path;
    _ = paths;
    @setCold(true);
    @panic("TODO");
}

fn existsFake(paths: []const []const u8, path: []const u8) bool {
    var buf: [std.fs.max_path_bytes]u8 = undefined;
    var fib = std.heap.FixedBufferAllocator.init(&buf);
    const resolved = std.fs.path.resolvePosix(fib.allocator(), &.{path}) catch return false;
    for (paths) |fakepath| {
        if (mem.eql(u8, fakepath, resolved)) return true;
    }
    return false;
}

fn canExecutePosix(path: []const u8) bool {
    std.posix.access(path, std.posix.X_OK) catch return false;
    // Todo: ensure path is not a directory
    return true;
}

/// TODO
fn canExecuteWindows(path: []const u8) bool {
    _ = path;
    return true;
}

/// TODO
fn findProgramByNameWindows(allocator: std.mem.Allocator, name: []const u8, buf: []u8) ?[]const u8 {
    _ = buf;
    _ = name;
    _ = allocator;
    return null;
}

/// TODO: does WASI need special handling?
fn findProgramByNamePosix(name: []const u8, buf: []u8) ?[]const u8 {
    if (mem.indexOfScalar(u8, name, '/') != null) {
        @memcpy(buf[0..name.len], name);
        return buf[0..name.len];
    }

    const pathEnv = std.posix.getenvZ("PATH") orelse return null;
    var fib = std.heap.FixedBufferAllocator.init(buf);

    var it = mem.tokenizeScalar(u8, pathEnv, ':');
    while (it.next()) |pathDir| {
        defer fib.reset();
        const fullPath = std.fs.path.join(fib.allocator(), &.{ pathDir, name }) catch continue;
        if (canExecutePosix(fullPath)) return fullPath;
    }

    return null;
}

pub const Filesystem = union(enum) {
    real: void,
    fake: []const []const u8,

    pub fn exists(fs: Filesystem, path: []const u8) bool {
        switch (fs) {
            .real => {
                std.fs.cwd().access(path, .{}) catch return false;
                return true;
            },
            .fake => |paths| return existsFake(paths, path),
        }
    }

    pub fn joinedExists(fs: Filesystem, parts: []const []const u8) bool {
        var buf: [std.fs.max_path_bytes]u8 = undefined;
        var fib = std.heap.FixedBufferAllocator.init(&buf);
        const joined = std.fs.path.join(fib.allocator(), parts) catch return false;
        return fs.exists(joined);
    }

    pub fn canExecute(fs: Filesystem, path: []const u8) bool {
        return switch (fs) {
            .real => if (isWindows) canExecuteWindows(path) else canExecutePosix(path),
            .fake => |paths| canExecuteFake(paths, path),
        };
    }

    /// Search for an executable named `name` using platform-specific logic
    /// If it's found, write the full path to `buf` and return a slice of it
    /// Otherwise retun null
    pub fn findProgramByName(fs: Filesystem, allocator: std.mem.Allocator, name: []const u8, buf: []u8) ?[]const u8 {
        std.debug.assert(name.len > 0);
        return switch (fs) {
            .real => if (isWindows) findProgramByNameWindows(allocator, name, buf) else findProgramByNamePosix(name, buf),
            .fake => findProgramByNameFake(allocator, name, buf),
        };
    }
};

test "Fake filesystem" {
    const fs: Filesystem = .{ .fake = &.{
        "/usr/bin",
    } };
    try std.testing.expect(fs.exists("/usr/bin"));
    try std.testing.expect(fs.exists("/usr/bin/foo/.."));
    try std.testing.expect(!fs.exists("/usr/bin/bar"));
}
