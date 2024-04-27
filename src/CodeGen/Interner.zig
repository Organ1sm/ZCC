const std = @import("std");
const Allocator = std.mem.Allocator;
const assert = std.debug.assert;
const Value = @import("../AST/Value.zig");

const Interner = @This();

map: std.ArrayHashMapUnmanaged(Key, void, KeyContext, false) = .{},

const KeyContext = struct {
    pub fn eql(_: @This(), a: Key, b: Key, _: usize) bool {
        return b.eql(a);
    }

    pub fn hash(_: @This(), a: Key) u32 {
        return a.hash();
    }
};

pub const Key = union(enum) {
    int: u16,
    float: u16,
    ptr,
    noreturn,
    void,
    func,
    array: struct {
        len: u64,
        child: Ref,
    },
    vector: struct {
        len: u32,
        child: Ref,
    },
    value: Value,

    pub fn hash(key: Key) u32 {
        var hasher = std.hash.Wyhash.init(0);
        switch (key) {
            .value => |val| {
                std.hash.autoHash(&hasher, val.tag);
                switch (val.tag) {
                    .unavailable => unreachable,
                    .int => std.hash.autoHash(&hasher, val.data.int),
                    .float => std.hash.autoHash(&hasher, @as(u64, @bitCast(val.data.float))),
                    .array => @panic("TODO"),
                    .bytes => std.hash.autoHashStrat(&hasher, val.data.bytes, .Shallow),
                }
            },
            inline else => |a_info| {
                std.hash.autoHash(&hasher, a_info);
            },
        }
        return @truncate(hasher.final());
    }

    pub fn eql(lhs: Key, rhs: Key) bool {
        const KeyTag = std.meta.Tag(Key);
        const lhsTag: KeyTag = lhs;
        const rhsTag: KeyTag = rhs;
        if (lhsTag != rhsTag) return false;
        switch (lhs) {
            .value => |lhsInfo| {
                const rhsInfo = rhs.value;
                if (lhsInfo.tag != rhsInfo.tag) return false;
                switch (lhsInfo.tag) {
                    .unavailable => unreachable,
                    .int => return lhsInfo.data.int == rhsInfo.data.int,
                    .float => return lhsInfo.data.float == rhsInfo.data.float,
                    .array => @panic("TODO"),
                    .bytes => return std.mem.eql(u8, lhsInfo.data.bytes, rhsInfo.data.bytes),
                }
            },
            inline else => |a_info, tag| {
                const b_info = @field(rhs, @tagName(tag));
                return std.meta.eql(a_info, b_info);
            },
        }
    }
};

pub const Ref = enum(u32) {
    const max = std.math.maxInt(u32);

    ptr = max - 0,
    noreturn = max - 1,
    void = max - 2,
    i1 = max - 3,
    i8 = max - 4,
    i16 = max - 5,
    i32 = max - 6,
    i64 = max - 7,
    i128 = max - 8,
    f16 = max - 9,
    f32 = max - 10,
    f64 = max - 11,
    f80 = max - 12,
    f128 = max - 13,
    func = max - 14,
    _,
};

pub fn deinit(ip: *Interner, gpa: Allocator) void {
    ip.map.deinit(gpa);
}

pub fn put(ip: *Interner, gpa: Allocator, key: Key) !Ref {
    switch (key) {
        .int => |bits| switch (bits) {
            1 => return .i1,
            8 => return .i8,
            16 => return .i16,
            32 => return .i32,
            64 => return .i64,
            128 => return .i128,
            else => {},
        },
        .float => |bits| switch (bits) {
            16 => return .f16,
            32 => return .f32,
            64 => return .f64,
            80 => return .f80,
            128 => return .f128,
            else => unreachable,
        },
        .ptr => return .ptr,
        .func => return .func,
        .noreturn => return .noreturn,
        .void => return .void,
        else => {},
    }
    const gop = try ip.map.getOrPut(gpa, key);
    return @enumFromInt(gop.index);
}

pub fn get(ip: Interner, ref: Ref) Key {
    switch (ref) {
        .ptr => return .ptr,
        .func => return .func,
        .noreturn => return .noreturn,
        .void => return .void,
        .i1 => return .{ .int = 1 },
        .i8 => return .{ .int = 8 },
        .i16 => return .{ .int = 16 },
        .i32 => return .{ .int = 32 },
        .i64 => return .{ .int = 64 },
        .i128 => return .{ .int = 128 },
        .f16 => return .{ .float = 16 },
        .f32 => return .{ .float = 32 },
        .f64 => return .{ .float = 64 },
        .f80 => return .{ .float = 80 },
        .f128 => return .{ .float = 128 },
        else => {},
    }
    return ip.map.keys()[@intFromEnum(ref)];
}
