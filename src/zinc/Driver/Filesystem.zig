const std = @import("std");
const mem = std.mem;
const builtin = @import("builtin");
const isWindows = builtin.os.tag == .windows;

fn findProgramByNameFake(entries: []const Filesystem.Entry, name: []const u8, path: ?[]const u8, buf: []u8) ?[]const u8 {
    @branchHint(.cold);
    if (mem.indexOfScalar(u8, name, '/') != null) {
        @memcpy(buf[0..name.len], name);
        return buf[0..name.len];
    }

    const pathEnv = path orelse return null;
    var fib = std.heap.FixedBufferAllocator.init(buf);

    var it = mem.tokenizeScalar(u8, pathEnv, std.fs.path.delimiter);
    while (it.next()) |pathDir| {
        defer fib.reset();
        const fullPath = std.fs.path.join(fib.allocator(), &.{ pathDir, name }) catch continue;
        if (canExecuteFake(entries, fullPath)) return fullPath;
    }
    return null;
}

fn canExecuteFake(entries: []const Filesystem.Entry, path: []const u8) bool {
    @branchHint(.cold);
    for (entries) |entry| {
        if (mem.eql(u8, entry.path, path))
            return entry.executable;
    }
    return false;
}

fn existsFake(entries: []const Filesystem.Entry, path: []const u8) bool {
    @branchHint(.cold);
    var buf: [std.fs.max_path_bytes]u8 = undefined;
    var fib = std.heap.FixedBufferAllocator.init(&buf);
    const resolved = std.fs.path.resolvePosix(fib.allocator(), &.{path}) catch return false;
    for (entries) |entry| {
        if (mem.eql(u8, entry.path, resolved)) return true;
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
fn findProgramByNameWindows(allocator: std.mem.Allocator, name: []const u8, path: ?[]const u8, buf: []u8) ?[]const u8 {
    _ = path;
    _ = buf;
    _ = name;
    _ = allocator;
    return null;
}

/// TODO: does WASI need special handling?
fn findProgramByNamePosix(name: []const u8, path: ?[]const u8, buf: []u8) ?[]const u8 {
    if (mem.indexOfScalar(u8, name, '/') != null) {
        @memcpy(buf[0..name.len], name);
        return buf[0..name.len];
    }

    const pathEnv = path orelse return null;
    var fib = std.heap.FixedBufferAllocator.init(buf);

    var it = mem.tokenizeScalar(u8, pathEnv, std.fs.path.delimiter);
    while (it.next()) |pathDir| {
        defer fib.reset();
        const fullPath = std.fs.path.join(fib.allocator(), &.{ pathDir, name }) catch continue;
        if (canExecutePosix(fullPath)) return fullPath;
    }

    return null;
}

pub const Filesystem = union(enum) {
    real: void,
    fake: []const Entry,

    const Entry = struct {
        path: []const u8,
        executable: bool = false,
    };

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
            .fake => |entries| canExecuteFake(entries, path),
        };
    }

    /// Search for an executable named `name` using platform-specific logic
    /// If it's found, write the full path to `buf` and return a slice of it
    /// Otherwise retun null
    pub fn findProgramByName(fs: Filesystem, allocator: std.mem.Allocator, name: []const u8, path: ?[]const u8, buf: []u8) ?[]const u8 {
        std.debug.assert(name.len > 0);
        return switch (fs) {
            .real => if (isWindows) findProgramByNameWindows(allocator, name, path, buf) else findProgramByNamePosix(name, path, buf),
            .fake => |entries| findProgramByNameFake(entries, name, path, buf),
        };
    }
};

test "Fake filesystem" {
    const fs: Filesystem = .{ .fake = &.{
        .{ .path = "/usr/bin" },
    } };
    try std.testing.expect(fs.exists("/usr/bin"));
    try std.testing.expect(fs.exists("/usr/bin/foo/.."));
    try std.testing.expect(!fs.exists("/usr/bin/bar"));
}
