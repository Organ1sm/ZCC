const std = @import("std");
const mem = std.mem;

const Compilation = @import("../Basic/Compilation.zig");

const StringInterner = @This();

pub const StringId = enum(u32) {
    empty = std.math.maxInt(u32),
    _,

    pub fn lookup(id: StringId, comp: *const Compilation) []const u8 {
        if (id == .empty) return "";
        return comp.stringInterner.table.keys()[@intFromEnum(id)];
    }

    pub fn lookupExtra(id: StringId, si: StringInterner) []const u8 {
        if (id == .empty) return "";
        return si.table.keys()[@intFromEnum(id)];
    }
};

table: std.StringArrayHashMapUnmanaged(void) = .empty,

pub fn deinit(self: *StringInterner, allocator: mem.Allocator) void {
    self.table.deinit(allocator);
    self.* = undefined;
}

/// Intern externally owned string.
pub fn intern(si: *StringInterner, allocator: mem.Allocator, str: []const u8) !StringId {
    if (str.len == 0) return .empty;

    const gop = try si.table.getOrPut(allocator, str);
    return @enumFromInt(gop.index);
}
