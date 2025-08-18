const std = @import("std");

const AST = @import("../AST/AST.zig");
const TokenIndex = AST.TokenIndex;
const Compilation = @import("../Basic/Compilation.zig");
const QualType = @import("../AST/TypeStore.zig").QualType;
const Value = @import("../AST/Value.zig");

pub const Switch = @This();

comp: *const Compilation,
default: ?TokenIndex = null,
ranges: std.ArrayList(Range) = .empty,
qt: QualType,

pub const Range = struct {
    first: Value,
    last: Value,
    token: TokenIndex,
};

pub fn add(s: *Switch, first: Value, last: Value, token: TokenIndex) !?Range {
    for (s.ranges.items) |range| {
        if (last.compare(.gte, range.first, s.comp) and
            first.compare(.lte, range.last, s.comp))
        {
            return range; // They overlap.
        }
    }
    try s.ranges.append(s.comp.gpa, .{
        .first = first,
        .last = last,
        .token = token,
    });
    return null;
}
