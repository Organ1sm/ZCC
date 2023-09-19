const std = @import("std");
const DiagnosticTag = @import("Diagnostics.zig").Tag;

const LangOpts = @This();

const Standard = enum {
    /// ISO C 1990
    c89,
    /// ISO C 1990 with amendment 1
    iso9899,
    /// ISO C 1990 with GNU extensions
    gnu89,
    /// ISO C 1999
    c99,
    /// ISO C 1999 with GNU extensions
    gnu99,
    /// ISO C 2011
    c11,
    /// ISO C 2011 with GNU extensions
    gnu11,
    /// ISO C 2017
    c17,
    /// ISO C 2017 with GNU extensions
    gnu17,
    /// Working Draft for ISO C2x
    c2x,
    /// Working Draft for ISO C2x with GNU extensions
    gnu2x,

    const NameMap = std.ComptimeStringMap(Standard, .{
        .{ "c89", .c89 },                .{ "c90", .c89 },          .{ "iso9899:1990", .c89 },
        .{ "iso9899:199409", .iso9899 }, .{ "gnu89", .gnu89 },      .{ "gnu90", .gnu89 },
        .{ "c99", .c99 },                .{ "iso9899:1999", .c99 }, .{ "gnu99", .gnu99 },
        .{ "c11", .c11 },                .{ "iso9899:2011", .c11 }, .{ "gnu11", .gnu11 },
        .{ "c17", .c17 },                .{ "iso9899:2017", .c17 }, .{ "c18", .c17 },
        .{ "iso9899:2018", .c17 },       .{ "gnu17", .gnu17 },      .{ "gnu18", .gnu17 },
        .{ "c2x", .c2x },                .{ "gnu2x", .gnu2x },
    });

    pub fn atLeast(self: Standard, other: Standard) bool {
        return @intFromEnum(self) >= @intFromEnum(other);
    }

    pub fn isGNU(standard: Standard) bool {
        return switch (standard) {
            .gnu89, .gnu99, .gnu11, .gnu17, .gnu2x => true,
            else => false,
        };
    }
};

standard: Standard = .gnu17,

pub fn setStandard(self: *LangOpts, name: []const u8) error{InvalidStandard}!void {
    self.standard = Standard.NameMap.get(name) orelse return error.InvalidStandard;
}

pub fn suppress(langopts: LangOpts, tag: DiagnosticTag) bool {
    return switch (tag) {
        .static_assert_missing_message => langopts.standard.atLeast(.c2x),
        .alignof_expr => langopts.standard.isGNU(),
        else => false,
    };
}
