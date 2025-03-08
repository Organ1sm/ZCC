const std = @import("std");
const mem = std.mem;

pub const Prefix = enum(u8) {
    binary = 2,
    octal = 8,
    decimal = 10,
    hex = 16,

    /// Checks if a given character `c` is allowed as a digit for the specified `prefix`.
    pub fn digitAllowed(prefix: Prefix, c: u8) bool {
        return switch (c) {
            '0', '1' => true,
            '2'...'7' => prefix != .binary,
            '8'...'9' => prefix == .decimal or prefix == .hex,
            'a'...'f', 'A'...'F' => prefix == .hex,
            else => false,
        };
    }

    pub fn fromString(buf: []const u8) Prefix {
        if (buf.len == 1) return .decimal;

        // tokenizer enforces that first byte is a decimal digit or period
        switch (buf[0]) {
            '.', '1'...'9' => return .decimal,
            '0' => {},
            else => unreachable,
        }

        switch (buf[1]) {
            'x', 'X' => return if (buf.len == 2) .decimal else .hex,
            'b', 'B' => return if (buf.len == 2) .decimal else .binary,
            else => {
                if (mem.indexOfAny(u8, buf, "eE.")) |_| {
                    // This is a decimal floating point number that happens to start with zero
                    return .decimal;
                } else if (Suffix.fromString(buf[1..], .int)) |_| {
                    // This is `0` with a valid suffix
                    return .decimal;
                } else {
                    return .octal;
                }
            },
        }
    }

    /// Length of this prefix as a string
    pub fn stringLen(prefix: Prefix) usize {
        return switch (prefix) {
            .binary => 2,
            .octal => 1,
            .decimal => 0,
            .hex => 2,
        };
    }
};

pub const Suffix = enum {
    // zig fmt: off

    // int and imaginary int
    None, I,

    // unsigned real integers
    U, UL, ULL,

    // unsigned imaginary integers
    IU, IUL, IULL,

    // long or long double, real and imaginary
    L, IL,

    // long long and imaginary long long
    LL, ILL,

    // float and imaginary float
    F, IF,

    // _Float16
    F16,

    // Imaginary _BitInt
    IWB, IUWB,

    // _BitInt
    WB, UWB,

    // zig fmt: on

    const Tuple = struct { Suffix, []const []const u8 };

    const IntSuffixes = &[_]Tuple{
        .{ .U, &.{"U"} },
        .{ .L, &.{"L"} },
        .{ .WB, &.{"WB"} },
        .{ .UL, &.{ "U", "L" } },
        .{ .UWB, &.{ "U", "WB" } },
        .{ .LL, &.{"LL"} },
        .{ .ULL, &.{ "U", "LL" } },

        .{ .I, &.{"I"} },

        .{ .IWB, &.{ "I", "WB" } },
        .{ .IU, &.{ "I", "U" } },
        .{ .IL, &.{ "I", "L" } },
        .{ .IUL, &.{ "I", "U", "L" } },
        .{ .IUWB, &.{ "I", "U", "WB" } },
        .{ .ILL, &.{ "I", "LL" } },
        .{ .IULL, &.{ "I", "U", "LL" } },
    };

    const FloatSuffixes = &[_]Tuple{
        .{ .F, &.{"F"} },
        .{ .F16, &.{"F16"} },
        .{ .L, &.{"L"} },

        .{ .I, &.{"I"} },
        .{ .IL, &.{ "I", "L" } },
        .{ .IF, &.{ "I", "F" } },
    };

    pub fn fromString(buf: []const u8, suffixKind: enum { int, float }) ?Suffix {
        if (buf.len == 0) return .None;

        const suffixes = switch (suffixKind) {
            .float => FloatSuffixes,
            .int => IntSuffixes,
        };
        var scratch: [3]u8 = undefined;
        top: for (suffixes) |candidate| {
            const tag = candidate[0];
            const parts = candidate[1];
            var len: usize = 0;
            for (parts) |part| len += part.len;
            if (len != buf.len) continue;

            for (parts) |part| {
                const lower = std.ascii.lowerString(&scratch, part);
                if (mem.indexOf(u8, buf, part) == null and mem.indexOf(u8, buf, lower) == null)
                    continue :top;
            }
            return tag;
        }
        return null;
    }

    pub fn isImaginary(suffix: Suffix) bool {
        return switch (suffix) {
            .I, .IL, .IF, .IU, .IUL, .ILL, .IULL, .IWB, .IUWB => true,
            .None, .L, .F16, .F, .U, .UL, .LL, .ULL, .WB, .UWB => false,
        };
    }

    pub fn isSignedInteger(suffix: Suffix) bool {
        return switch (suffix) {
            .None, .L, .LL, .I, .IL, .ILL, .WB, .IWB => true,
            .U, .UL, .ULL, .IU, .IUL, .IULL, .UWB, .IUWB => false,
            .F, .IF, .F16 => unreachable,
        };
    }

    pub fn signedness(suffix: Suffix) std.builtin.Signedness {
        return if (suffix.isSignedInteger()) .signed else .unsigned;
    }

    pub fn isBitInt(suffix: Suffix) bool {
        return switch (suffix) {
            .WB, .UWB, .IWB, .IUWB => true,
            else => false,
        };
    }
};
