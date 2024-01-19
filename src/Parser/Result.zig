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

pub fn isZero(res: Result) bool {
    return switch (res.value) {
        .signed => |v| v == 0,
        .unsigned => |v| v == 0,
        .unavailable => false,
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

pub fn maybeWarnUnused(res: Result, p: *Parser, exprStart: TokenIndex, errStart: usize) Error!void {
    if (res.ty.is(.Void) or res.node == .none)
        return;

    // don't warn about unused result if the expression contained errors besides other unused results
    var i = errStart;
    while (i < p.pp.compilation.diag.list.items.len) : (i += 1) {
        if (p.pp.compilation.diag.list.items[i].tag != .unused_value) return;
    }

    var curNode = res.node;
    while (true)
        switch (p.nodes.items(.tag)[@intFromEnum(curNode)]) {
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

            .StmtExpr => {
                const body = p.nodes.items(.data)[@intFromEnum(curNode)].unExpr;
                switch (p.nodes.items(.tag)[@intFromEnum(body)]) {
                    .CompoundStmtTwo => {
                        const bodyStmt = p.nodes.items(.data)[@intFromEnum(body)].binExpr;
                        curNode = if (bodyStmt.rhs != .none) bodyStmt.rhs else bodyStmt.lhs;
                    },
                    .CompoundStmt => {
                        const data = p.nodes.items(.data)[@intFromEnum(body)];
                        curNode = p.data.items[data.range.end - 1];
                    },
                    else => unreachable,
                }
            },

            .CommaExpr => curNode = p.nodes.items(.data)[@intFromEnum(curNode)].binExpr.rhs,
            .ParenExpr => curNode = p.nodes.items(.data)[@intFromEnum(curNode)].unExpr,

            else => break,
        };

    try p.errToken(.unused_value, exprStart);
}

pub fn bin(lhs: *Result, p: *Parser, tag: AstTag, rhs: Result) !void {
    lhs.node = try p.addNode(.{
        .tag = tag,
        .type = lhs.ty,
        .data = .{ .binExpr = .{ .lhs = lhs.node, .rhs = rhs.node } },
    });
}

pub fn un(operand: *Result, p: *Parser, tag: AstTag) Error!void {
    operand.node = try p.addNode(.{
        .tag = tag,
        .type = operand.ty,
        .data = .{ .unExpr = operand.node },
    });
}

pub fn qualCast(res: *Result, p: *Parser, elemType: *Type) Error!void {
    res.ty = .{
        .data = .{ .subType = elemType },
        .specifier = .Pointer,
    };
    try res.un(p, .QualCast);
}

pub fn adjustCondExprPtrs(a: *Result, tok: TokenIndex, b: *Result, p: *Parser) !bool {
    std.debug.assert(a.ty.isPointer() and b.ty.isPointer());

    const aElem = a.ty.getElemType();
    const bElem = b.ty.getElemType();
    if (aElem.eql(bElem, true)) return true;

    var adjustedElemType = try p.arena.create(Type);
    adjustedElemType.* = aElem;

    const hasVoidStarBranch = a.ty.isVoidStar() or b.ty.isVoidStar();
    const onlyQualsDiffer = aElem.eql(bElem, false);
    const pointersCompatible = onlyQualsDiffer or hasVoidStarBranch;

    if (!pointersCompatible or hasVoidStarBranch) {
        if (!pointersCompatible)
            try p.errStr(.pointer_mismatch, tok, try p.typePairStrExtra(a.ty, " and ", b.ty));

        adjustedElemType.* = .{ .specifier = .Void };
    }
    if (pointersCompatible) {
        adjustedElemType.qual = aElem.qual.mergeCVQualifiers(bElem.qual);
    }
    if (!adjustedElemType.eql(aElem, true)) try a.qualCast(p, adjustedElemType);
    if (!adjustedElemType.eql(bElem, true)) try b.qualCast(p, adjustedElemType);
    return true;
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

    const aIsPtr = a.ty.isPointer();
    const bIsPtr = b.ty.isPointer();
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

            if ((aIsInt or bIsInt) and !(a.isZero() or b.isZero())) {
                try p.errStr(.comparison_ptr_int, token, try p.typePairStr(a.ty, b.ty));
            } else if (aIsPtr and bIsPtr) {
                if (!a.ty.isVoidStar() and !b.ty.isVoidStar() and !a.ty.eql(b.ty, false))
                    try p.errStr(.comparison_distinct_ptr, token, try p.typePairStr(a.ty, b.ty));
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
            if (a.ty.is(.Void) or b.ty.is(.Void)) {
                try a.toVoid(p);
                try b.toVoid(p);
                return true;
            }

            if ((aIsPtr and bIsInt) or (aIsInt and bIsPtr)) {
                if (a.isZero() or b.isZero()) {
                    try a.nullCast(p, b.ty);
                    try b.nullCast(p, a.ty);
                    return true;
                }

                const intType = if (aIsInt) a else b;
                const ptrType = if (aIsPtr) a else b;

                try p.errStr(.implicit_int_to_ptr, token, try p.typePairStrExtra(intType.ty, " to ", ptrType.ty));
                try intType.ptrCast(p, ptrType.ty);
                return true;
            }

            if (aIsPtr and bIsPtr)
                return a.adjustCondExprPtrs(token, b, p);

            if (a.ty.isRecord() and b.ty.isRecord() and a.ty.eql(b.ty, false))
                return true;

            return a.invalidBinTy(token, b, p);
        },

        .add => {
            // if both aren't arithmetic one should be pointer and the other an integer
            if (aIsPtr == bIsPtr or aIsInt == bIsInt)
                return a.invalidBinTy(token, b, p);

            // Do integer promotions but nothing else
            if (aIsInt) try a.intCast(p, a.ty.integerPromotion(p.pp.compilation));
            if (bIsInt) try b.intCast(p, b.ty.integerPromotion(p.pp.compilation));

            // The result type is the type of the pointer operand
            if (aIsInt) a.ty = b.ty else b.ty = a.ty;
            return a.shouldEval(b, p);
        },

        .sub => {
            // if both aren't arithmetic then either both should be pointers or just a
            if (!aIsPtr or !(bIsPtr or bIsInt)) return a.invalidBinTy(token, b, p);

            if (aIsPtr and bIsPtr) {
                if (!a.ty.eql(b.ty, false))
                    try p.errStr(.incompatible_pointers, token, try p.typePairStr(a.ty, b.ty));
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
        const elemType = try p.arena.create(Type);
        elemType.* = res.ty;
        res.ty.specifier = .Pointer;
        res.ty.data = .{ .subType = elemType };
        res.ty.alignment = 0;
        try res.un(p, .FunctionToPointer);
    } else if (res.ty.isArray()) {
        res.ty.decayArray();
        res.ty.alignment = 0;
        try res.un(p, .ArrayToPointer);
    } else if (!p.inMacro and AST.isLValue(p.nodes.slice(), p.data.items, p.valueMap, res.node)) {
        res.ty.qual = .{};
        res.ty.alignment = 0;
        try res.un(p, .LValueToRValue);
    }
}

pub fn boolCast(res: *Result, p: *Parser, boolType: Type) Error!void {
    if (res.ty.isPointer()) {
        res.ty = boolType;
        try res.un(p, .PointerToBool);
    } else if (res.ty.isInt() and !res.ty.is(.Bool)) {
        res.ty = boolType;
        try res.un(p, .IntToBool);
    } else if (res.ty.isFloat()) {
        res.ty = boolType;
        try res.un(p, .FloatToBool);
    }
}

pub fn intCast(res: *Result, p: *Parser, intType: Type) Error!void {
    if (res.ty.is(.Bool)) {
        res.ty = intType;
        try res.un(p, .BoolToInt);
    } else if (res.ty.isPointer()) {
        res.ty = intType;
        try res.un(p, .PointerToInt);
    } else if (res.ty.isFloat()) {
        res.ty = intType;
        try res.un(p, .FloatToInt);
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
    if (res.ty.is(.Bool)) {
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
    if (res.ty.is(.Bool)) {
        res.ty = ptrType;
        try res.un(p, .BoolToPointer);
    } else if (res.ty.isInt()) {
        res.ty = ptrType;
        try res.un(p, .IntToPointer);
    }
}

pub fn toVoid(res: *Result, p: *Parser) Error!void {
    if (!res.ty.is(.Void)) {
        res.ty = .{ .specifier = .Void };
        res.node = try p.addNode(.{
            .tag = .ToVoid,
            .type = res.ty,
            .data = .{ .unExpr = res.node },
        });
    }
}

pub fn nullCast(res: *Result, p: *Parser, ptrType: Type) Error!void {
    if (!res.isZero()) return;
    res.ty = ptrType;
    try res.un(p, .NullToPointer);
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
        const resType = Type{ .specifier = @enumFromInt(resSpecifier) };
        try a.intCast(p, resType);
        try b.intCast(p, resType);
        return;
    }

    // cast to the unsigned type with greater rank
    const aLarger = @intFromEnum(aPromoted.specifier) > @intFromEnum(bPromoted.specifier);
    const bLarger = @intFromEnum(bPromoted.specifier) > @intFromEnum(bPromoted.specifier);
    if (aIsUnsigned) {
        const target = if (aLarger) aPromoted else bPromoted;
        try a.intCast(p, target);
        try b.intCast(p, target);
        return;
    } else {
        std.debug.assert(bIsUnsigned);
        const target = if (bLarger) bPromoted else aPromoted;
        try a.intCast(p, target);
        try b.intCast(p, target);
    }
}

fn invalidBinTy(a: *Result, tok: TokenIndex, b: *Result, p: *Parser) Error!bool {
    try p.errStr(.invalid_bin_types, tok, try p.typePairStr(a.ty, b.ty));
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

pub fn saveValue(res: *Result, p: *Parser) !void {
    std.debug.assert(!p.inMacro);
    switch (res.value) {
        .unsigned => |v| try p.valueMap.put(res.node, v),
        .signed => |v| try p.valueMap.put(res.node, @as(u64, @bitCast(v))),
        .unavailable => return,
    }

    res.value = .unavailable;
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
                    const sum, const overflowed = @addWithOverflow(@as(u32, @truncate(v.*)), @as(u32, @truncate(b.value.unsigned)));
                    isOverflow = (overflowed == 1);
                    v.* = sum;
                },
                8 => {
                    const sum, const overflowed = @addWithOverflow(v.*, b.value.unsigned);
                    isOverflow = (overflowed == 1);
                    v.* = sum;
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
                    const sum, const overflowed = @addWithOverflow(@as(i32, @truncate(v.*)), @as(i32, @truncate(b.value.signed)));
                    isOverflow = (overflowed == 1);
                    v.* = sum;
                },
                8 => {
                    const sum, const overflowed = @addWithOverflow(v.*, b.value.signed);
                    isOverflow = (overflowed == 1);
                    v.* = sum;
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
