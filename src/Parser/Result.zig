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

pub fn asU64(res: Result) u64 {
    return switch (res.value) {
        .signed => |v| @as(u64, @bitCast(v)),
        .unsigned => |v| v,
        .unavailable => unreachable,
    };
}

pub fn expect(res: Result, p: *Parser) Error!void {
    if (p.inMacro) {
        if (res.value == .unavailable) {
            try p.errToken(.expected_expr, p.index);
            return error.ParsingFailed;
        }
        return;
    }

    try res.saveValue(p);
    if (res.node == .none) {
        try p.errToken(.expected_expr, p.index);
        return error.ParsingFailed;
    }
}

pub fn empty(res: Result, p: *Parser) bool {
    if (p.inMacro)
        return res.value == .unavailable;
    return res.node == .none;
}

pub fn maybeWarnUnused(res: Result, p: *Parser, exprStart: TokenIndex) Error!void {
    if (res.ty.specifier == .Void or res.node == .none)
        return;

    switch (p.nodes.items(.tag)[@intFromEnum(res.node)]) {
        .Invalid,
        .AssignExpr,
        .MulAssignExpr,
        .DivAssignExpr,
        .ModAssignExpr,
        .AddAssignExpr,
        .SubAssignExpr,
        .ShlAssignExpr,
        .ShrAssignExpr,
        .BitAndAssignExpr,
        .BitXorAssignExpr,
        .BitOrAssignExpr,
        .CallExpr,
        .CallExprOne,
        .PreIncExpr,
        .PreDecExpr,
        .PostIncExpr,
        .PostDecExpr,
        => return,

        else => {},
    }

    try p.errToken(.unused_value, exprStart);
}

pub fn bin(lhs: *Result, p: *Parser, tag: AstTag, rhs: Result) !void {
    lhs.node = try p.addNode(.{
        .tag = tag,
        .type = lhs.ty,
        .data = .{ .BinaryExpr = .{ .lhs = lhs.node, .rhs = rhs.node } },
    });
}

pub fn un(operand: *Result, p: *Parser, tag: AstTag) Error!void {
    operand.node = try p.addNode(.{
        .tag = tag,
        .type = operand.ty,
        .data = .{ .UnaryExpr = operand.node },
    });
}

/// Return true if both are same type
/// Adjust types for binary operation, returns true if the result can and should be evaluated.
pub fn adjustTypes(a: *Result, token: TokenIndex, b: *Result, p: *Parser, kind: enum {
    integer,
    arithmetic,
    booleanLogic,
    relational,
    equality,
    conditional,
    add,
    sub,
}) !bool {
    try a.lvalConversion(p);
    try b.lvalConversion(p);

    const aIsInt = a.ty.isInt();
    const bIsInt = b.ty.isInt();

    if (aIsInt and bIsInt) {
        try a.usualArithmeticConversion(b, p);
        return a.shouldEval(b, p);
    }

    if (kind == .integer)
        return a.invalidBinTy(token, b, p);

    const aIsFloat = a.ty.isFloat();
    const bIsFloat = b.ty.isFloat();
    const aIsArithmetic = aIsInt or aIsFloat;
    const bIsArithmetic = bIsInt or bIsFloat;
    if (aIsArithmetic and bIsArithmetic) {
        // <, <=, >, >= only work on real types
        if (kind == .relational and (!a.ty.isReal() or !b.ty.isReal()))
            return a.invalidBinTy(token, b, p);

        try a.usualArithmeticConversion(b, p);
        return a.shouldEval(b, p);
    }

    if (kind == .arithmetic)
        return a.invalidBinTy(token, b, p);

    const aIsPtr = a.ty.specifier == .Pointer;
    const bIsPtr = b.ty.specifier == .Pointer;
    const aIsScalar = aIsArithmetic or aIsPtr;
    const bIsScalar = bIsArithmetic or bIsPtr;
    switch (kind) {
        .booleanLogic => {
            if (!aIsScalar or !bIsScalar) return a.invalidBinTy(token, b, p);

            // Do integer promotions but nothing else
            if (aIsInt) try a.intCast(p, a.ty.integerPromotion(p.pp.compilation));
            if (bIsInt) try b.intCast(p, b.ty.integerPromotion(p.pp.compilation));
            return a.shouldEval(b, p);
        },

        .relational, .equality => {
            // comparisons between floats and pointes not allowed
            if (!aIsScalar or !bIsScalar or (aIsFloat and bIsPtr) or (bIsFloat and aIsPtr))
                return a.invalidBinTy(token, b, p);

            // TODO print types
            if (aIsInt or bIsInt) try p.errToken(.comparison_ptr_int, token);
            if (aIsPtr and bIsPtr) {
                if (!a.ty.eql(b.ty, false)) try p.errToken(.comparison_distinct_ptr, token);
            } else if (aIsPtr) {
                try b.ptrCast(p, a.ty);
            } else {
                std.debug.assert(bIsPtr);
                try a.ptrCast(p, b.ty);
            }

            return a.shouldEval(b, p);
        },

        .conditional => {
            // doesn't matter what we return here, as the result is ignored
            if (a.ty.specifier == .Void or b.ty.specifier == .Void) {
                try a.toVoid(p);
                try b.toVoid(p);
                return true;
            }
            // TODO struct/record and pointers
            return a.invalidBinTy(token, b, p);
        },

        .add => {
            // if both aren't arithmetic one should be pointer and the other an integer
            if (aIsPtr == bIsPtr or aIsInt == bIsInt)
                return a.invalidBinTy(token, b, p);

            // Do integer promotions but nothing else
            if (aIsInt) try a.intCast(p, a.ty.integerPromotion(p.pp.compilation));
            if (bIsInt) try b.intCast(p, b.ty.integerPromotion(p.pp.compilation));
            return a.shouldEval(b, p);
        },

        .sub => {
            // if both aren't arithmetic then either both should be pointers or just a
            if (!aIsPtr or !(bIsPtr or bIsInt)) return a.invalidBinTy(token, b, p);

            if (aIsPtr and bIsPtr) {
                // TODO print types
                if (!a.ty.eql(b.ty, false)) try p.errToken(.incompatible_pointers, token);
                a.ty = Type.ptrDiffT(p.pp.compilation);
            }

            // Do integer promotion on b if needed
            if (bIsInt) try b.intCast(p, b.ty.integerPromotion(p.pp.compilation));
            return a.shouldEval(b, p);
        },

        else => return a.invalidBinTy(token, b, p),
    }

    return a.shouldEval(b, p);
}

pub fn lvalConversion(res: *Result, p: *Parser) Error!void {
    if (res.ty.isFunc()) {
        var elemType = try p.arena.create(Type);
        elemType.* = res.ty;

        res.ty.specifier = .Pointer;
        res.ty.data = .{ .subType = elemType };
        try res.un(p, .FunctionToPointer);
    } else if (res.ty.isArray()) {
        var elemType = try p.arena.create(Type);
        elemType.* = res.ty.getElemType();

        res.ty.specifier = .Pointer;
        res.ty.data = .{ .subType = elemType };
        try res.un(p, .ArrayToPointer);
    } else if (!p.inMacro and AST.isLValue(p.nodes.slice(), res.node)) {
        res.ty.qual = .{};
        try res.un(p, .LValueToRValue);
    }
}

pub fn boolCast(res: *Result, p: *Parser, boolType: Type) Error!void {
    if (res.ty.specifier == .Pointer) {
        res.ty = boolType;
        try res.un(p, .PointerToBool);
    } else if (res.ty.isInt() and res.ty.specifier != .Bool) {
        res.ty = boolType;
        try res.un(p, .IntToBool);
    } else if (res.ty.isFloat()) {
        res.ty = boolType;
        try res.un(p, .FloatToBool);
    }
}

pub fn intCast(res: *Result, p: *Parser, intType: Type) Error!void {
    if (res.ty.specifier == .Bool) {
        res.ty = intType;
        try res.un(p, .BoolToInt);
    } else if (!res.ty.eql(intType, true)) {
        res.ty = intType;
        try res.un(p, .IntCast);
    }

    const isUnsigned = intType.isUnsignedInt(p.pp.compilation);
    if (isUnsigned and res.value == .signed) {
        const copy = res.value.signed;
        res.value = .{ .unsigned = @bitCast(copy) };
    } else if (!isUnsigned and res.value == .unsigned) {
        const copy = res.value.unsigned;
        res.value = .{ .signed = @bitCast(copy) };
    }
}

pub fn floatCast(res: *Result, p: *Parser, floatType: Type) Error!void {
    if (res.ty.specifier == .Bool) {
        res.ty = floatType;
        try res.un(p, .BoolToFloat);
    } else if (res.ty.isInt()) {
        res.ty = floatType;
        try res.un(p, .IntToFloat);
    } else if (!res.ty.eql(floatType, true)) {
        res.ty = floatType;
        try res.un(p, .FloatCast);
    }
}

pub fn ptrCast(res: *Result, p: *Parser, ptrType: Type) Error!void {
    if (res.ty.specifier == .Bool) {
        res.ty = ptrType;
        try res.un(p, .BoolToPointer);
    } else if (res.ty.isInt()) {
        res.ty = ptrType;
        try res.un(p, .IntToPointer);
    }
}

pub fn toVoid(res: *Result, p: *Parser) Error!void {
    if (res.ty.specifier != .Void) {
        res.ty = .{ .specifier = .Void };
        res.node = try p.addNode(.{
            .tag = .ToVoid,
            .type = res.ty,
            .data = .{ .UnaryExpr = res.node },
        });
    }
}

fn usualArithmeticConversion(a: *Result, b: *Result, p: *Parser) Error!void {
    // if either is a float cast to that type
    if (Type.eitherLongDouble(a.ty, b.ty)) |ty| {
        try a.floatCast(p, ty);
        try b.floatCast(p, ty);
        return;
    }
    if (Type.eitherDouble(a.ty, b.ty)) |ty| {
        try a.floatCast(p, ty);
        try b.floatCast(p, ty);
        return;
    }
    if (Type.eitherFloat(a.ty, b.ty)) |ty| {
        try a.floatCast(p, ty);
        try b.floatCast(p, ty);
        return;
    }

    // Do integer promotion on both operands
    const aPromoted = a.ty.integerPromotion(p.pp.compilation);
    const bPromoted = b.ty.integerPromotion(p.pp.compilation);
    if (aPromoted.eql(bPromoted, true)) {
        // cast to promoted type
        try a.intCast(p, aPromoted);
        try b.intCast(p, aPromoted);
        return;
    }

    const aIsUnsigned = aPromoted.isUnsignedInt(p.pp.compilation);
    const bIsUnsigned = bPromoted.isUnsignedInt(p.pp.compilation);
    if (aIsUnsigned == bIsUnsigned) {
        // cast to greater signed or unsigned type
        const resSpecifier = @max(@intFromEnum(aPromoted.specifier), @intFromEnum(bPromoted.specifier));
        const resType = Type{ .specifier = @as(Type.Specifier, @enumFromInt(resSpecifier)) };
        try a.intCast(p, resType);
        try b.intCast(p, resType);
        return;
    }

    // cast to the unsigned type with greater rank
    if (aIsUnsigned and (@intFromEnum(aPromoted.specifier) >= @intFromEnum(bPromoted.specifier))) {
        try a.intCast(p, aPromoted);
        try b.intCast(p, aPromoted);
        return;
    } else {
        std.debug.assert(bIsUnsigned and (@intFromEnum(bPromoted.specifier) >= @intFromEnum(aPromoted.specifier)));
        try a.intCast(p, bPromoted);
        try b.intCast(p, bPromoted);
    }
}

fn invalidBinTy(a: *Result, tok: TokenIndex, b: *Result, p: *Parser) Error!bool {
    // TODO print a and b types
    _ = a;
    _ = b;
    try p.errToken(.invalid_bin_types, tok);
    return false;
}

fn shouldEval(a: *Result, b: *Result, p: *Parser) Error!bool {
    if (p.noEval) return false;
    if (a.value != .unavailable and b.value != .unavailable)
        return true;

    try a.saveValue(p);
    try b.saveValue(p);
    return p.noEval;
}

fn saveValue(res: Result, p: *Parser) !void {
    std.debug.assert(!p.inMacro);
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
    const size = a.ty.sizeof(p.pp.compilation).?;

    var isOverflow = false;
    switch (a.value) {
        .unsigned => |*v| {
            switch (size) {
                1, 2 => unreachable, // promoted to int,
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
                1, 2 => unreachable, // promoted to int
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
    const size = a.ty.sizeof(p.pp.compilation).?;
    var isOverflow = false;
    switch (a.value) {
        .unsigned => |*v| {
            switch (size) {
                1, 2 => unreachable, // promoted to int
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
                1, 2 => unreachable, // promoted to int
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
    const size = a.ty.sizeof(p.pp.compilation).?;
    var isOverflow = false;
    switch (a.value) {
        .unsigned => |*v| {
            switch (size) {
                1, 2 => unreachable, // promoted to int
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
                1, 2 => unreachable, // promoted to int
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
