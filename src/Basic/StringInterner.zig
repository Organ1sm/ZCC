const std = @import("std");
const mem = std.mem;

const StringInterner = @This();

const StringToIdMap = std.StringHashMapUnmanaged(StringId);

pub const StringId = enum(u32) {
    empty,
    _,
};

pub const TypeMapper = struct {
    const LookupSpeed = enum {
        fast, // Fast lookups use an array for direct index access.
        slow, // Slow lookups use a map for key-value association.
    };

    data: union(LookupSpeed) {
        fast: []const []const u8, // Array of strings for fast lookup by StringId.
        slow: *const StringToIdMap, // Pointer to a map for slow but flexible lookups.
    },

    /// lookup returns the string associated with the given StringId.
    /// If the StringId is empty, it returns an empty string.
    /// For fast lookups, it directly indexes into the array.
    /// For slow lookups, it iterates over the map to find the associated string.
    pub fn lookup(self: TypeMapper, stringId: StringInterner.StringId) []const u8 {
        if (stringId == .empty) return "";
        switch (self.data) {
            .fast => |arr| return arr[@intFromEnum(stringId)], // Direct index access for fast lookup.
            .slow => |map| {
                var it = map.iterator();
                while (it.next()) |entry| {
                    if (entry.value_ptr.* == stringId)
                        return entry.key_ptr.*;
                }
                unreachable; // It's unreachable because all StringIds should have an associated string.
            },
        }
    }

    pub fn deinit(self: TypeMapper, allocator: mem.Allocator) void {
        switch (self.data) {
            .slow => {},
            .fast => |arr| allocator.free(arr),
        }
    }
};

stringTable: StringToIdMap = .{},
nextId: StringId = @enumFromInt(@intFromEnum(StringId.empty) + 1),

pub fn deinit(self: *StringInterner, allocator: mem.Allocator) void {
    self.stringTable.deinit(allocator);
}

pub fn intern(self: *StringInterner, allocator: mem.Allocator, str: []const u8) !StringId {
    if (str.len == 0) return .empty;

    const gop = try self.stringTable.getOrPut(allocator, str);
    if (gop.found_existing) return gop.value_ptr.*;

    defer self.nextId = @enumFromInt(@intFromEnum(self.nextId) + 1);
    gop.value_ptr.* = self.nextId;
    return self.nextId;
}

/// deinit for the returned TypeMapper is a no-op and does not need to be called
pub fn getSlowTypeMapper(self: *const StringInterner) TypeMapper {
    return TypeMapper{ .data = .{ .slow = &self.stringTable } };
}

/// Caller must call `deinit` on the returned TypeMapper
pub fn getFastTypeMapper(self: *const StringInterner, allocator: mem.Allocator) !TypeMapper {
    var strings = try allocator.alloc([]const u8, @intFromEnum(self.nextId));
    var it = self.stringTable.iterator();

    strings[0] = "";
    while (it.next()) |entry|
        strings[@intFromEnum(entry.value_ptr.*)] = entry.key_ptr.*;

    return TypeMapper{ .data = .{ .fast = strings } };
}
