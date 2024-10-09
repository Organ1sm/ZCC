const std = @import("std");
const AST = @import("../AST/AST.zig");
const Type = @import("../AST/Type.zig");
const Compilation = @import("../Basic/Compilation.zig");
const Value = @import("../AST/Value.zig");
const Parser = @import("../Parser/Parser.zig");

const TokenIndex = AST.TokenIndex;
const NodeIndex = AST.NodeIndex;

pub const Switch = @This();

p: *Parser,
default: ?TokenIndex = null,
ranges: std.ArrayList(Range),
type: Type,

pub const Range = struct {
    first: Value,
    last: Value,
    token: TokenIndex,
};

pub fn add(self: *Switch, first: Value, last: Value, token: TokenIndex) !?Range {
    for (self.ranges.items) |range| {
        if (last.compare(.gte, range.first, self.p) and first.compare(.lte, range.last, self.p)) {
            return range; // They overlap.
        }
    }
    try self.ranges.append(.{
        .first = first,
        .last = last,
        .token = token,
    });
    return null;
}
