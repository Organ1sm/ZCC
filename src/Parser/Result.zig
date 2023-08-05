const std = @import("std");
const Type = @import("../AST/Type.zig");
const AST = @import("../AST/AST.zig");
const AstTag = @import("../AST/AstTag.zig").Tag;
const Parser = @import("Parser.zig");

const NodeIndex = AST.NodeIndex;
const TokenIndex = AST.TokenIndex;
const Error = Parser.Error;

const Result = @This();

node: NodeIndex = .none,
ty: Type = .{ .specifier = .Int },
value: union(enum) {
    unsigned: u64,
    signed: i64,
    unavailable,
} = .unavailable,

pub fn getBool(res: Result) bool {
    return switch (res.value) {
        .signed => |v| v != 0,
        .unsigned => |v| v != 0,
        .unavailable => unreachable,
    };
}

pub fn expect(res: Result, p: *Parser) Error!void {
    if (res.node == .none) {
        try p.errToken(.expected_expr, p.index);
        return error.ParsingFailed;
    }
}

pub fn bin(lhs: *Result, p: *Parser, tag: AstTag, rhs: Result) !void {
    lhs.node = try p.addNode(.{
        .tag = tag,
        .type = lhs.ty,
        .data = .{ .BinaryExpr = .{ .lhs = lhs.node, .rhs = rhs.node } },
    });
}

pub fn un(operand: *Result, p: *Parser, tag: AstTag) Error!Result {
    _ = tag;
    operand.node = try p.addNode(.{
        .tag = .CastExpr,
        .type = operand.ty,
        .data = .{ .UnaryExpr = operand.node },
    });
    return operand.*;
}

pub fn coerce(res: Result, p: *Parser, destType: Type) !Result {
    _ = p;
    _ = destType;
    var casted = res;
    // var curType = res.ty;

    // if (casted.data == .lVal) {
    //     curType.qual.@"const" = false;
    //     casted = try node(p, .{
    //         .tag = .LValueToRValue,
    //         .type = curType,
    //         .data = .{ .first = try casted.toNode(p) },
    //     });
    // }

    // if (destType.specifier == .Pointer and curType.isArray()) {
    //     casted = try node(p, .{
    //         .tag = .ArrayToPointer,
    //         .type = destType,
    //         .data = .{ .first = try casted.toNode(p) },
    //     });
    // }
    return casted;
}

/// Return true if both are same type
pub fn adjustTypes(a: *Result, b: *Result, p: *Parser) !bool {
    const aIsUnsigned = a.ty.isUnsignedInt(p.pp.compilation);
    const bIsUnsigned = b.ty.isUnsignedInt(p.pp.compilation);

    if (aIsUnsigned != bIsUnsigned) {}

    if (a.value != .unavailable and b.value != .unavailable)
        return true;

    try a.saveValue(p);
    try b.saveValue(p);
    return false;
}

fn saveValue(res: Result, p: *Parser) !void {
    std.debug.assert(!p.wantConst);
    if (res.value == .unavailable) return;

    switch (p.nodes.items(.tag)[@intFromEnum(res.node)]) {
        .IntLiteral => return,
        else => {},
    }

    switch (res.value) {
        .unsigned => |v| try p.valueMap.put(res.node, v),
        .signed => |v| try p.valueMap.put(res.node, @as(u64, @bitCast(v))),
        .unavailable => {},
    }
}

pub fn hash(res: Result) u64 {
    var val: i64 = undefined;
    switch (res.value) {
        .unsigned => |v| return std.hash.Wyhash.hash(0, std.mem.asBytes(&v)), // doesn't matter we only want a hash
        .signed => |v| return std.hash.Wyhash.hash(0, std.mem.asBytes(&v)),
        .unavailable => unreachable,
    }
    return std.hash.Wyhash.hash(0, std.mem.asBytes(&val));
}

pub fn eql(a: Result, b: Result) bool {
    return a.compare(.eq, b);
}

pub fn compare(a: Result, op: std.math.CompareOperator, b: Result) bool {
    switch (a.value) {
        .unsigned => |val| return std.math.compare(val, op, b.value.unsigned),
        .signed => |val| return std.math.compare(val, op, b.value.signed),
        .unavailable => unreachable,
    }
}

pub fn mul(a: *Result, token: TokenIndex, b: Result, p: *Parser) !void {
    const size = a.ty.sizeof(p.pp.compilation);

    var isOverflow = false;
    switch (a.value) {
        .unsigned => |*v| {
            switch (size) {
                1, 2 => unreachable, // upcasted to int,
                4 => {
                    const ov = @mulWithOverflow(@as(u32, @truncate(v.*)), @as(u32, @truncate(b.value.unsigned)));
                    isOverflow = ov[1] == 1;
                    v.* = ov[0];
                },
                8 => {
                    const ov = @mulWithOverflow(v.*, b.value.unsigned);
                    isOverflow = ov[1] == 1;
                    v.* = ov[0];
                },
                else => unreachable,
            }

            if (isOverflow)
                try p.errExtra(.overflow_signed, token, .{ .unsigned = v.* });
        },

        .signed => |*v| {
            switch (size) {
                1, 2 => unreachable, // upcasted to int
                4 => {
                    const ov = @mulWithOverflow(@as(i32, @truncate(v.*)), @as(i32, @truncate(b.value.signed)));
                    isOverflow = ov[1] == 1;
                    v.* = ov[0];
                },
                8 => {
                    const ov = @mulWithOverflow(v.*, b.value.signed);
                    isOverflow = ov[1] == 1;
                    v.* = ov[0];
                },
                else => unreachable,
            }
            if (isOverflow)
                try p.errExtra(.overflow_signed, token, .{ .signed = v.* });
        },

        .unavailable => unreachable,
    }
}

pub fn add(a: *Result, tok: TokenIndex, b: Result, p: *Parser) !void {
    const size = a.ty.sizeof(p.pp.compilation);
    var isOverflow = false;
    switch (a.value) {
        .unsigned => |*v| {
            switch (size) {
                1, 2 => unreachable, // upcasted to int
                4 => {
                    const ov = @addWithOverflow(@as(u32, @truncate(v.*)), @as(u32, @truncate(b.value.unsigned)));
                    isOverflow = ov[1] == 1;
                    v.* = ov[0];
                },
                8 => {
                    const ov = @addWithOverflow(v.*, b.value.unsigned);
                    isOverflow = ov[1] == 1;
                    v.* = ov[0];
                },
                else => unreachable,
            }

            if (isOverflow)
                try p.errExtra(.overflow_unsigned, tok, .{ .unsigned = v.* });
        },

        .signed => |*v| {
            switch (size) {
                1 => unreachable, // upcasted to int
                2 => unreachable, // upcasted to int
                4 => {
                    const ov = @addWithOverflow(@as(i32, @truncate(v.*)), @as(i32, @truncate(b.value.signed)));
                    isOverflow = ov[1] == 1;
                    v.* = ov[0];
                },
                8 => {
                    const ov = @addWithOverflow(v.*, b.value.signed);
                    isOverflow = ov[1] == 1;
                    v.* = ov[0];
                },
                else => unreachable,
            }

            if (isOverflow)
                try p.errExtra(.overflow_signed, tok, .{ .signed = v.* });
        },

        .unavailable => unreachable,
    }
}

pub fn sub(a: *Result, tok: TokenIndex, b: Result, p: *Parser) !void {
    const size = a.ty.sizeof(p.pp.compilation);
    var isOverflow = false;
    switch (a.value) {
        .unsigned => |*v| {
            switch (size) {
                1 => unreachable, // upcasted to int
                2 => unreachable, // upcasted to int
                4 => {
                    const ov = @subWithOverflow(@as(u32, @truncate(v.*)), @as(u32, @truncate(b.value.unsigned)));
                    isOverflow = ov[1] == 1;
                    v.* = ov[0];
                },
                8 => {
                    const ov = @subWithOverflow(v.*, b.value.unsigned);
                    isOverflow = ov[1] == 1;
                    v.* = ov[0];
                },
                else => unreachable,
            }
            if (isOverflow)
                try p.errExtra(.overflow_unsigned, tok, .{ .unsigned = v.* });
        },

        .signed => |*v| {
            switch (size) {
                1 => unreachable, // upcasted to int
                2 => unreachable, // upcasted to int
                4 => {
                    const ov = @subWithOverflow(@as(i32, @truncate(v.*)), @as(i32, @truncate(b.value.signed)));
                    isOverflow = ov[1] == 1;
                    v.* = ov[0];
                },
                8 => {
                    const ov = @subWithOverflow(v.*, b.value.signed);
                    isOverflow = ov[1] == 1;
                    v.* = ov[0];
                },
                else => unreachable,
            }

            if (isOverflow)
                try p.errExtra(.overflow_signed, tok, .{ .signed = v.* });
        },

        .unavailable => unreachable,
    }
}
