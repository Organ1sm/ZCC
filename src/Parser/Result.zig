const std = @import("std");
const Type = @import("../AST/Type.zig");
const AST = @import("../AST/AST.zig");
const Parser = @import("Parser.zig");

const NodeIndex = AST.NodeIndex;
const TokenIndex = AST.TokenIndex;
const Error = Parser.Error;

const Result = @This();

ty: Type = .{ .specifier = .Int },
data: union(enum) {
    none,
    unsigned: u64,
    signed: i64,
    lVal: NodeIndex,
    node: NodeIndex,
} = .none,

pub fn getBool(res: Result) bool {
    return switch (res.data) {
        .signed => |v| v != 0,
        .unsigned => |v| v != 0,

        .node, .none, .lVal => unreachable,
    };
}

pub fn expect(res: Result, p: *Parser) Error!void {
    if (res.data == .none) {
        try p.errToken(.expected_expr, p.index);
        return error.ParsingFailed;
    }
}

pub fn node(p: *Parser, n: AST.Node) !Result {
    const index = try p.addNode(n);
    return Result{ .ty = n.type, .data = .{ .lVal = index } };
}

pub fn leftValue(p: *Parser, n: AST.Node) !Result {
    const index = try p.addNode(n);
    return Result{ .ty = n.type, .data = .{ .lVal = index } };
}

pub fn toNode(res: Result, p: *Parser) !NodeIndex {
    var parts: [2]TokenIndex = undefined;
    switch (res.data) {
        .none => return 0,
        .node, .lVal => |n| return n,
        .signed => |v| parts = @as([2]TokenIndex, @bitCast(v)),
        .unsigned => |v| parts = @as([2]TokenIndex, @bitCast(v)),
    }

    return p.addNode(.{
        .tag = .IntLiteral,
        .type = res.ty,
        .first = parts[0],
        .second = parts[1],
    });
}

pub fn coerce(res: Result, p: *Parser, destType: Type) !Result {
    var casted = res;
    var curType = res.ty;

    if (casted.data == .lVal) {
        curType.qual.@"const" = false;
        casted = try node(p, .{
            .tag = .LValueToRValue,
            .type = curType,
            .first = try casted.toNode(p),
        });
    }

    if (destType.specifier == .Pointer and curType.isArray()) {
        casted = try node(p, .{
            .tag = .ArrayToPointer,
            .type = destType,
            .first = try casted.toNode(p),
        });
    }
    return casted;
}

/// Return true if both are same type
pub fn adjustTypes(a: *Result, b: *Result, p: *Parser) !bool {
    const aIsUnsigned = a.ty.isUnsignedInt(p.pp.compilation);
    const bIsUnsigned = b.ty.isUnsignedInt(p.pp.compilation);

    if (aIsUnsigned != bIsUnsigned) {}

    return (a.data == .unsigned or a.data == .signed) and (b.data == .unsigned or b.data == .signed);
}

pub fn hash(res: Result) u64 {
    var val: i64 = undefined;
    switch (res.data) {
        .unsigned => |v| val = @as(i64, @bitCast(v)), // doesn't matter we only want a hash
        .signed => |v| val = v,
        .none, .node, .lVal => unreachable,
    }
    return std.hash.Wyhash.hash(0, std.mem.asBytes(&val));
}

pub fn eql(a: Result, b: Result) bool {
    return a.compare(.eq, b);
}

pub fn compare(a: Result, op: std.math.CompareOperator, b: Result) bool {
    switch (a.data) {
        .unsigned => |val| return std.math.compare(val, op, b.data.unsigned),
        .signed => |val| return std.math.compare(val, op, b.data.signed),
        .none, .node, .lVal => unreachable,
    }
}

pub fn mul(a: *Result, token: TokenIndex, b: Result, p: *Parser) !void {
    const size = a.ty.sizeof(p.pp.compilation);

    var isOverflow = false;
    switch (a.data) {
        .unsigned => |*v| {
            switch (size) {
                1, 2 => unreachable, // upcasted to int,
                4 => {
                    const ov = @mulWithOverflow(@as(u32, @truncate(v.*)), @as(u32, @truncate(b.data.unsigned)));
                    isOverflow = ov[1] == 1;
                    v.* = ov[0];
                },
                8 => {
                    const ov = @mulWithOverflow(v.*, b.data.unsigned);
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
                    const ov = @mulWithOverflow(@as(i32, @truncate(v.*)), @as(i32, @truncate(b.data.signed)));
                    isOverflow = ov[1] == 1;
                    v.* = ov[0];
                },
                8 => {
                    const ov = @mulWithOverflow(v.*, b.data.signed);
                    isOverflow = ov[1] == 1;
                    v.* = ov[0];
                },
                else => unreachable,
            }
            if (isOverflow)
                try p.errExtra(.overflow_signed, token, .{ .signed = v.* });
        },

        .none, .node, .lVal => unreachable,
    }
}

pub fn add(a: *Result, tok: TokenIndex, b: Result, p: *Parser) !void {
    const size = a.ty.sizeof(p.pp.compilation);
    var isOverflow = false;
    switch (a.data) {
        .unsigned => |*v| {
            switch (size) {
                1, 2 => unreachable, // upcasted to int
                4 => {
                    const ov = @addWithOverflow(@as(u32, @truncate(v.*)), @as(u32, @truncate(b.data.unsigned)));
                    isOverflow = ov[1] == 1;
                    v.* = ov[0];
                },
                8 => {
                    const ov = @addWithOverflow(v.*, b.data.unsigned);
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
                    const ov = @addWithOverflow(@as(i32, @truncate(v.*)), @as(i32, @truncate(b.data.signed)));
                    isOverflow = ov[1] == 1;
                    v.* = ov[0];
                },
                8 => {
                    const ov = @addWithOverflow(v.*, b.data.signed);
                    isOverflow = ov[1] == 1;
                    v.* = ov[0];
                },
                else => unreachable,
            }

            if (isOverflow)
                try p.errExtra(.overflow_signed, tok, .{ .signed = v.* });
        },
        .none, .node, .lVal => unreachable,
    }
}

pub fn sub(a: *Result, tok: TokenIndex, b: Result, p: *Parser) !void {
    const size = a.ty.sizeof(p.pp.compilation);
    var isOverflow = false;
    switch (a.data) {
        .unsigned => |*v| {
            switch (size) {
                1 => unreachable, // upcasted to int
                2 => unreachable, // upcasted to int
                4 => {
                    const ov = @subWithOverflow(@as(u32, @truncate(v.*)), @as(u32, @truncate(b.data.unsigned)));
                    isOverflow = ov[1] == 1;
                    v.* = ov[0];
                },
                8 => {
                    const ov = @subWithOverflow(v.*, b.data.unsigned);
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
                    const ov = @subWithOverflow(@as(i32, @truncate(v.*)), @as(i32, @truncate(b.data.signed)));
                    isOverflow = ov[1] == 1;
                    v.* = ov[0];
                },
                8 => {
                    const ov = @subWithOverflow(v.*, b.data.signed);
                    isOverflow = ov[1] == 1;
                    v.* = ov[0];
                },
                else => unreachable,
            }

            if (isOverflow)
                try p.errExtra(.overflow_signed, tok, .{ .signed = v.* });
        },
        .none, .node, .lVal => unreachable,
    }
}
