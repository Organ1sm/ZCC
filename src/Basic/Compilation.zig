const std = @import("std");
const builtin = @import("builtin");
const Source = @import("Source.zig");
const Diagnostics = @import("../Basic/Diagnostics.zig");
const Token = @import("../Lexer/Token.zig").Token;

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
outputName: ?[]const u8 = null,
target: std.Target = builtin.target,
onlyPreprocess: bool = false,

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

/// Generate builtin macros that will be available to each source file.
pub fn generateBuiltinMacros(comp: *Compilation) !Source {
    var buf = std.ArrayList(u8).init(comp.gpa);
    defer buf.deinit();

    try buf.appendSlice(
        \\#define __VERSION__ "Aro 
    ++ @import("../Basic/Info.zig").VersionStr ++ "\"\n" ++
        \\#define __STDC__ 1
        \\#define __STDC_HOSTED__ 1
        \\#define __STDC_VERSION__ 201710L
        \\#define __STDC_NO_ATOMICS__ 1
        \\#define __STDC_NO_COMPLEX__ 1
        \\#define __STDC_NO_THREADS__ 1
        \\#define __STDC_NO_VLA__ 1
        \\
    );

    switch (comp.target.os.tag) {
        .linux => try buf.appendSlice(
            \\#define unix 1
            \\#define __unix 1
            \\#define __unix__ 1
            \\#define linux 1
            \\#define __linux 1
            \\#define __linux__ 1
            \\
        ),
        .windows => if (comp.target.ptrBitWidth() == 32)
            try buf.appendSlice("#define _WIN32 1\n")
        else
            try buf.appendSlice(
                \\#define _WIN32 1
                \\#define _WIN64 1
                \\
            ),
        else => {},
    }

    switch (comp.target.cpu.arch) {
        .x86_64 => try buf.appendSlice(
            \\#define __amd64__ 1
            \\#define __amd64 1
            \\#define __x86_64 1
            \\#define __x86_64__ 1
            \\
        ),
        else => {},
    }

    try buf.appendSlice(if (comp.target.cpu.arch.endian() == .Little)
        \\#define __ORDER_LITTLE_ENDIAN__ 1234
        \\#define __ORDER_BIG_ENDIAN__ 4321
        \\#define __ORDER_PDP_ENDIAN__ 3412
        \\#define __BYTE_ORDER__ __ORDER_LITTLE_ENDIAN__
        \\#define __LITTLE_ENDIAN__ 1
        \\
    else
        \\#define __ORDER_LITTLE_ENDIAN__ 1234
        \\#define __ORDER_BIG_ENDIAN__ 4321
        \\#define __ORDER_PDP_ENDIAN__ 3412
        \\#define __BYTE_ORDER__ __ORDER_BIG_ENDIAN__;
        \\#define __BIG_ENDIAN__ 1
        \\
    );

    const duped_path = try comp.gpa.dupe(u8, "<builtin>");
    errdefer comp.gpa.free(duped_path);

    const contents = try buf.toOwnedSlice();
    errdefer comp.gpa.free(contents);

    const source = Source{
        .id = @as(Source.ID, @enumFromInt(comp.sources.count() + 2)),
        .path = duped_path,
        .buffer = contents,
    };
    try comp.sources.put(duped_path, source);
    return source;
}

pub fn getSource(comp: *Compilation, id: Source.ID) Source {
    return comp.sources.values()[@intFromEnum(id) - 2];
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
        .id = @as(Source.ID, @enumFromInt(compilation.sources.count() + 2)),
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
    const lcs = source.lineColString(token.start);

    return comp.diag.fatal(source.path, lcs, fmt, args);
}

pub const renderErrors = Diagnostics.render;
