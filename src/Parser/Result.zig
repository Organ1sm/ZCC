const std = @import("std");
const Type = @import("../AST/Type.zig");
const AST = @import("../AST/AST.zig");
const Value = @import("../AST/Value.zig");
const AstTag = @import("../AST/AstTag.zig").Tag;
const Parser = @import("Parser.zig");

const NodeIndex = AST.NodeIndex;
const TokenIndex = AST.TokenIndex;
const Error = Parser.Error;

const Result = @This();

node: NodeIndex = .none,
ty: Type = Type.Int,
value: Value = .{},

pub fn expect(res: Result, p: *Parser) Error!void {
    if (p.inMacro) {
        if (res.value.tag == .unavailable) {
            try p.errToken(.expected_expr, p.tokenIdx);
            return error.ParsingFailed;
        }
        return;
    }

    if (res.node == .none) {
        try p.errToken(.expected_expr, p.tokenIdx);
        return error.ParsingFailed;
    }
}

pub fn empty(res: Result, p: *Parser) bool {
    if (p.inMacro)
        return res.value.tag == .unavailable;
    return res.node == .none;
}

pub fn maybeWarnUnused(res: Result, p: *Parser, exprStart: TokenIndex, errStart: usize) Error!void {
    if (res.ty.is(.Void) or res.node == .none)
        return;

    // don't warn about unused result if the expression contained errors besides other unused results
    for (p.comp.diag.list.items[errStart..]) |errItem| {
        if (errItem.tag != .unused_value) return;
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
            .PreIncExpr,
            .PreDecExpr,
            .PostIncExpr,
            .PostDecExpr,
            => return,

            .CallExprOne => {
                const fnPtr = p.nodes.items(.data)[@intFromEnum(curNode)].binExpr.lhs;
                const fnTy = p.nodes.items(.type)[@intFromEnum(fnPtr)].getElemType();
                if (fnTy.hasAttribute(.nodiscard)) try p.errStr(.nodiscard_unused, exprStart, "TODO get name");
                if (fnTy.hasAttribute(.warn_unused_result)) try p.errStr(.warn_unused_result, exprStart, "TODO get name");
                return;
            },

            .CallExpr => {
                const fnPtr = p.data.items[p.nodes.items(.data)[@intFromEnum(curNode)].range.start];
                const fnTy = p.nodes.items(.type)[@intFromEnum(fnPtr)].getElemType();
                if (fnTy.hasAttribute(.nodiscard)) try p.errStr(.nodiscard_unused, exprStart, "TODO get name");
                if (fnTy.hasAttribute(.warn_unused_result)) try p.errStr(.warn_unused_result, exprStart, "TODO get name");
                return;
            },

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

pub fn implicitCast(operand: *Result, p: *Parser, kind: AST.CastKind) Error!void {
    operand.node = try p.addNode(.{
        .tag = .ImplicitCast,
        .type = operand.ty,
        .data = .{ .cast = .{ .operand = operand.node, .kind = kind } },
    });
}

pub fn adjustCondExprPtrs(a: *Result, tok: TokenIndex, b: *Result, p: *Parser) !bool {
    std.debug.assert(a.ty.isPointer() and b.ty.isPointer());

    const aElem = a.ty.getElemType();
    const bElem = b.ty.getElemType();
    if (aElem.eql(bElem, p.comp, true))
        return true;

    var adjustedElemType = try p.arena.create(Type);
    adjustedElemType.* = aElem;

    const hasVoidStarBranch = a.ty.isVoidStar() or b.ty.isVoidStar();
    const onlyQualsDiffer = aElem.eql(bElem, p.comp, false);
    const pointersCompatible = onlyQualsDiffer or hasVoidStarBranch;

    if (!pointersCompatible or hasVoidStarBranch) {
        if (!pointersCompatible)
            try p.errStr(.pointer_mismatch, tok, try p.typePairStrExtra(a.ty, " and ", b.ty));

        adjustedElemType.* = Type.Void;
    }
    if (pointersCompatible) {
        adjustedElemType.qual = aElem.qual.mergeCVQualifiers(bElem.qual);
    }

    if (!adjustedElemType.eql(aElem, p.comp, true)) {
        a.ty = .{
            .data = .{ .subType = adjustedElemType },
            .specifier = .Pointer,
        };
        try a.implicitCast(p, .Bitcast);
    }

    if (!adjustedElemType.eql(bElem, p.comp, true)) {
        b.ty = .{
            .data = .{ .subType = adjustedElemType },
            .specifier = .Pointer,
        };
        try b.implicitCast(p, .Bitcast);
    }
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

    const aVec = a.ty.is(.Vector);
    const bVec = b.ty.is(.Vector);
    if (aVec and bVec) {
        if (a.ty.eql(b.ty, p.comp, false))
            return a.shouldEval(b, p);
        return a.invalidBinTy(token, b, p);
    } else if (aVec) {
        if (b.coerceExtra(p, a.ty.getElemType(), token, .testCoerce)) {
            try b.saveValue(p);
            try b.implicitCast(p, .VectorSplat);
            return a.shouldEval(b, p);
        } else |err| switch (err) {
            error.CoercionFailed => return a.invalidBinTy(token, b, p),
            else => |e| return e,
        }
    } else if (bVec) {
        if (a.coerceExtra(p, b.ty.getElemType(), token, .testCoerce)) {
            try a.saveValue(p);
            try a.implicitCast(p, .VectorSplat);
            return a.shouldEval(b, p);
        } else |err| switch (err) {
            error.CoercionFailed => return a.invalidBinTy(token, b, p),
            else => |e| return e,
        }
    }

    const aIsInt = a.ty.isInt();
    const bIsInt = b.ty.isInt();

    if (aIsInt and bIsInt) {
        try a.usualArithmeticConversion(b, p, token);
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

        try a.usualArithmeticConversion(b, p, token);
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
            if (aIsInt) try a.intCast(p, a.ty.integerPromotion(p.comp), token);
            if (bIsInt) try b.intCast(p, b.ty.integerPromotion(p.comp), token);
            return a.shouldEval(b, p);
        },

        .relational, .equality => {
            // comparisons between floats and pointes not allowed
            if (!aIsScalar or !bIsScalar or (aIsFloat and bIsPtr) or (bIsFloat and aIsPtr))
                return a.invalidBinTy(token, b, p);

            if ((aIsInt or bIsInt) and !(a.value.isZero() or b.value.isZero())) {
                try p.errStr(.comparison_ptr_int, token, try p.typePairStr(a.ty, b.ty));
            } else if (aIsPtr and bIsPtr) {
                if (!a.ty.isVoidStar() and !b.ty.isVoidStar() and !a.ty.eql(b.ty, p.comp, false))
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
                if (a.value.isZero() or b.value.isZero()) {
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

            if (a.ty.isRecord() and b.ty.isRecord() and a.ty.eql(b.ty, p.comp, false))
                return true;

            return a.invalidBinTy(token, b, p);
        },

        .add => {
            // if both aren't arithmetic one should be pointer and the other an integer
            if (aIsPtr == bIsPtr or aIsInt == bIsInt)
                return a.invalidBinTy(token, b, p);

            // Do integer promotions but nothing else
            if (aIsInt) try a.intCast(p, a.ty.integerPromotion(p.comp), token);
            if (bIsInt) try b.intCast(p, b.ty.integerPromotion(p.comp), token);

            // The result type is the type of the pointer operand
            if (aIsInt) a.ty = b.ty else b.ty = a.ty;
            return a.shouldEval(b, p);
        },

        .sub => {
            // if both aren't arithmetic then either both should be pointers or just a
            if (!aIsPtr or !(bIsPtr or bIsInt)) return a.invalidBinTy(token, b, p);

            if (aIsPtr and bIsPtr) {
                if (!a.ty.eql(b.ty, p.comp, false))
                    try p.errStr(.incompatible_pointers, token, try p.typePairStr(a.ty, b.ty));
                a.ty = p.comp.types.ptrdiff;
            }

            // Do integer promotion on b if needed
            if (bIsInt) try b.intCast(p, b.ty.integerPromotion(p.comp), token);
            return a.shouldEval(b, p);
        },

        else => return a.invalidBinTy(token, b, p),
    }

    return a.shouldEval(b, p);
}

/// Perform l-value to r-value conversion and decay functions and arrays to pointers.
/// This function modifies `res` to ensure it's a valid r-value by decaying array types to pointers,
/// converting functions to pointers, and stripping qualifiers from l-value types to convert them to r-values.
/// @param res The Result struct representing the expression to convert.
/// @param p The Parser instance containing the context for the conversion.
/// @return Error!void The function returns an error if any of the conversion steps fail.
pub fn lvalConversion(res: *Result, p: *Parser) Error!void {
    // Convert a function type to a pointer to the function.
    if (res.ty.isFunc()) {
        const elemType = try p.arena.create(Type);
        elemType.* = res.ty;
        res.ty.specifier = .Pointer;
        res.ty.data = .{ .subType = elemType };
        try res.implicitCast(p, .FunctionToPointer);
        // Decay an array type to a pointer to its first element.
    } else if (res.ty.isArray()) {
        res.value.tag = .unavailable;
        res.ty.decayArray();
        try res.implicitCast(p, .ArrayToPointer);
        // Perform l-value to r-value conversion if the type is an l-value and we are not in a macro.
    } else if (!p.inMacro and AST.isLValue(p.nodes.slice(), p.data.items, p.valueMap, res.node)) {
        res.ty.qual = .{};
        // Update the AST to reflect the l-value to r-value conversion.
        try res.implicitCast(p, .LValToRVal);
    }
}

pub fn boolCast(res: *Result, p: *Parser, boolType: Type, tok: TokenIndex) Error!void {
    if (res.ty.isPointer()) {
        res.value.toBool();
        res.ty = boolType;
        try res.implicitCast(p, .PointerToBool);
    } else if (res.ty.isInt() and !res.ty.is(.Bool)) {
        res.value.toBool();
        res.ty = boolType;
        try res.implicitCast(p, .IntToBool);
    } else if (res.ty.isFloat()) {
        const oldValue = res.value;
        const valueChangeKind = res.value.floatToInt(res.ty, boolType, p.comp);
        try res.floatToIntWarning(p, boolType, oldValue, valueChangeKind, tok);
        if (!res.ty.isReal()) {
            res.ty = res.ty.makeReal();
            try res.implicitCast(p, .ComplexFloatToReal);
        }
        res.ty = boolType;
        try res.implicitCast(p, .FloatToBool);
    }
}

/// Perform an integer cast on the result's value.
/// This function handles casting from boolean, pointer, float, and integer types to an integer type.
/// If the original type is a float, it will also check for potential issues with the conversion.
/// If the integer type has an incomplete size, the parsing will fail.
/// @param res      The result object to cast.
/// @param p        The parser object.
/// @param intType  The integer type to cast to.
/// @param tok      The token index at which the cast occurs.
/// @return Error   Returns an error if casting fails or the result type has an incomplete size.
pub fn intCast(res: *Result, p: *Parser, intType: Type, tok: TokenIndex) Error!void {
    if (intType.hasIncompleteSize())
        return error.ParsingFailed;

    // Cast from boolean to integer.
    if (res.ty.is(.Bool)) {
        res.ty = intType.makeReal();
        try res.implicitCast(p, .BoolToInt);
        if (!intType.isReal()) {
            res.ty = intType;
            try res.implicitCast(p, .RealToComplexInt);
        }
    }

    // Cast from pointer to integer.
    else if (res.ty.isPointer()) {
        res.ty = intType.makeReal();
        try res.implicitCast(p, .PointerToInt);
        if (!intType.isReal()) {
            res.ty = intType;
            try res.implicitCast(p, .RealToComplexInt);
        }
    }

    // Cast from floating point to integer.
    else if (res.ty.isFloat()) {
        const oldValue = res.value;
        // Check for the kind of value change that will occur during the cast.
        const valueChangeKind = res.value.floatToInt(res.ty, intType, p.comp);
        // Warn if there are issues with the float to int conversion.
        try res.floatToIntWarning(p, intType, oldValue, valueChangeKind, tok);

        const oldReal = res.ty.isReal();
        const newReal = intType.isReal();
        if (oldReal and newReal) {
            res.ty = intType;
            try res.implicitCast(p, .FloatToInt);
        } else if (oldReal) {
            res.ty = intType.makeReal();
            try res.implicitCast(p, .FloatToInt);
            res.ty = intType;
            try res.implicitCast(p, .RealToComplexInt);
        } else if (newReal) {
            res.ty = res.ty.makeReal();
            try res.implicitCast(p, .ComplexFloatToReal);
            res.ty = intType;
            try res.implicitCast(p, .FloatToInt);
        } else {
            res.ty = intType;
            try res.implicitCast(p, .ComplexFloatToComplexInt);
        }
    }

    // Cast between integer types.
    else if (!res.ty.eql(intType, p.comp, true)) {
        res.value.intCast(res.ty, intType, p.comp);
        const oldReal = res.ty.isReal();
        const newReal = intType.isReal();
        if (oldReal and newReal) {
            res.ty = intType;
            try res.implicitCast(p, .IntCast);
        } else if (oldReal) {
            res.ty = intType.makeReal();
            try res.implicitCast(p, .IntCast);
            res.ty = intType;
            try res.implicitCast(p, .RealToComplexInt);
        } else if (newReal) {
            res.ty = res.ty.makeReal();
            try res.implicitCast(p, .ComplexIntToReal);
            res.ty = intType;
            try res.implicitCast(p, .IntCast);
        } else {
            res.ty = intType;
            try res.implicitCast(p, .ComplexIntCast);
        }
    }
}

fn floatToIntWarning(res: *Result, p: *Parser, intTy: Type, oldValue: Value, changeKind: Value.FloatToIntChangeKind, tok: TokenIndex) !void {
    switch (changeKind) {
        .none => return p.errStr(.float_to_int, tok, try p.typePairStrExtra(res.ty, " to ", intTy)),
        .outOfRange => return p.errStr(.float_out_of_range, tok, try p.typePairStrExtra(res.ty, " to ", intTy)),
        .overflow => return p.errStr(.float_overflow_conversion, tok, try p.typePairStrExtra(res.ty, " to ", intTy)),
        .nonZeroToZero => return p.errStr(.float_zero_conversion, tok, try p.floatValueChangedStr(res, oldValue.getFloat(f64), intTy)),
        .valueChanged => return p.errStr(.float_value_changed, tok, try p.floatValueChangedStr(res, oldValue.getFloat(f64), intTy)),
    }
}

pub fn floatCast(res: *Result, p: *Parser, floatType: Type) Error!void {
    if (res.ty.is(.Bool)) {
        res.value.intToFloat(res.ty, floatType, p.comp);
        res.ty = floatType.makeReal();
        try res.implicitCast(p, .BoolToFloat);
        if (!floatType.isReal()) {
            res.ty = floatType;
            try res.implicitCast(p, .RealToComplexFloat);
        }
    } else if (res.ty.isInt()) {
        res.value.intToFloat(res.ty, floatType, p.comp);
        const old_real = res.ty.isReal();
        const new_real = floatType.isReal();
        if (old_real and new_real) {
            res.ty = floatType;
            try res.implicitCast(p, .IntToFloat);
        } else if (old_real) {
            res.ty = floatType.makeReal();
            try res.implicitCast(p, .IntToFloat);
            res.ty = floatType;
            try res.implicitCast(p, .RealToComplexFloat);
        } else if (new_real) {
            res.ty = res.ty.makeReal();
            try res.implicitCast(p, .ComplexIntToReal);
            res.ty = floatType;
            try res.implicitCast(p, .IntToFloat);
        } else {
            res.ty = floatType;
            try res.implicitCast(p, .ComplexIntToComplexFloat);
        }
    } else if (!res.ty.eql(floatType, p.comp, true)) {
        res.value.floatCast(res.ty, floatType, p.comp);
        const oldReal = res.ty.isReal();
        const newReal = floatType.isReal();
        if (oldReal and newReal) {
            res.ty = floatType;
            try res.implicitCast(p, .FloatCast);
        } else if (oldReal) {
            res.ty = floatType.makeReal();
            try res.implicitCast(p, .FloatCast);
            res.ty = floatType;
            try res.implicitCast(p, .RealToComplexFloat);
        } else if (newReal) {
            res.ty = res.ty.makeReal();
            try res.implicitCast(p, .ComplexFloatToReal);
            res.ty = floatType;
            try res.implicitCast(p, .FloatCast);
        } else {
            res.ty = floatType;
            try res.implicitCast(p, .ComplexFloatCast);
        }
    }
}

pub fn ptrCast(res: *Result, p: *Parser, ptrType: Type) Error!void {
    if (res.ty.is(.Bool)) {
        res.ty = ptrType;
        try res.implicitCast(p, .BoolToPointer);
    } else if (res.ty.isInt()) {
        res.value.intCast(res.ty, ptrType, p.comp);
        res.ty = ptrType;
        try res.implicitCast(p, .IntToPointer);
    }
}

pub fn toVoid(res: *Result, p: *Parser) Error!void {
    if (!res.ty.is(.Void)) {
        res.ty = Type.Void;
        try res.implicitCast(p, .ToVoid);
    }
}

pub fn nullCast(res: *Result, p: *Parser, ptrType: Type) Error!void {
    if (!res.value.isZero()) return;
    res.ty = ptrType;
    try res.implicitCast(p, .NullToPointer);
}

fn usualArithmeticConversion(lhs: *Result, rhs: *Result, p: *Parser, tok: TokenIndex) Error!void {
    // if either is a float cast to that type
    if (lhs.ty.isFloat() or rhs.ty.isFloat()) {
        const floatTypes = [6][2]Type.Specifier{
            .{ .ComplexLongDouble, .LongDouble },
            .{ .ComplexFloat128, .Float128 },
            .{ .ComplexFloat80, .Float80 },
            .{ .ComplexDouble, .Double },
            .{ .ComplexFloat, .Float },
            .{ .ComplexFP16, .FP16 },
        };

        const lhsSpec = lhs.ty.canonicalize(.standard).specifier;
        const rhsSpec = rhs.ty.canonicalize(.standard).specifier;

        if (p.comp.target.c_type_bit_size(.longdouble) == 128) {
            if (try lhs.floatConversion(rhs, lhsSpec, rhsSpec, p, floatTypes[0]))
                return;
        }

        if (try lhs.floatConversion(rhs, lhsSpec, rhsSpec, p, floatTypes[1]))
            return;

        if (p.comp.target.c_type_bit_size(.longdouble) == 80) {
            if (try lhs.floatConversion(rhs, lhsSpec, rhsSpec, p, floatTypes[0]))
                return;
        }

        if (try lhs.floatConversion(rhs, lhsSpec, rhsSpec, p, floatTypes[2]))
            return;

        if (p.comp.target.c_type_bit_size(.longdouble) == 64) {
            if (try lhs.floatConversion(rhs, lhsSpec, rhsSpec, p, floatTypes[0])) return;
        }

        if (try lhs.floatConversion(rhs, lhsSpec, rhsSpec, p, floatTypes[3])) return;
        if (try lhs.floatConversion(rhs, lhsSpec, rhsSpec, p, floatTypes[4])) return;
        if (try lhs.floatConversion(rhs, lhsSpec, rhsSpec, p, floatTypes[5])) return;
    }

    // Do integer promotion on both operands
    const lhsPromoted = lhs.ty.integerPromotion(p.comp);
    const rhsPromoted = rhs.ty.integerPromotion(p.comp);
    if (lhsPromoted.eql(rhsPromoted, p.comp, true)) {
        // cast to promoted type
        try lhs.intCast(p, lhsPromoted, tok);
        try rhs.intCast(p, lhsPromoted, tok);
        return;
    }

    const lhsIsUnsigned = lhsPromoted.isUnsignedInt(p.comp);
    const rhsIsUnsigned = rhsPromoted.isUnsignedInt(p.comp);
    if (lhsIsUnsigned == rhsIsUnsigned) {
        // cast to greater signed or unsigned type
        const resSpecifier = @max(@intFromEnum(lhsPromoted.specifier), @intFromEnum(rhsPromoted.specifier));
        const resType = Type{ .specifier = @enumFromInt(resSpecifier) };
        try lhs.intCast(p, resType, tok);
        try rhs.intCast(p, resType, tok);
        return;
    }

    // cast to the unsigned type with greater rank
    const lhsLarger = @intFromEnum(lhsPromoted.specifier) > @intFromEnum(rhsPromoted.specifier);
    const rhsLarger = @intFromEnum(rhsPromoted.specifier) > @intFromEnum(lhsPromoted.specifier);
    if (lhsIsUnsigned) {
        const target = if (lhsLarger) lhsPromoted else rhsPromoted;
        try lhs.intCast(p, target, tok);
        try rhs.intCast(p, target, tok);
        return;
    } else {
        std.debug.assert(rhsIsUnsigned);
        const target = if (rhsLarger) rhsPromoted else lhsPromoted;
        try lhs.intCast(p, target, tok);
        try rhs.intCast(p, target, tok);
    }
}
fn floatConversion(
    lhs: *Result,
    rhs: *Result,
    lhsSpec: Type.Specifier,
    rhsSpec: Type.Specifier,
    p: *Parser,
    pair: [2]Type.Specifier,
) !bool {
    if (lhsSpec == pair[0] or lhsSpec == pair[1] or
        rhsSpec == pair[0] or rhsSpec == pair[1])
    {
        const bothReal = lhs.ty.isReal() and rhs.ty.isReal();
        const resSpec = pair[@intFromBool(bothReal)];
        const ty = Type{ .specifier = resSpec };
        try lhs.floatCast(p, ty);
        try rhs.floatCast(p, ty);
        return true;
    }
    return false;
}

fn invalidBinTy(a: *Result, tok: TokenIndex, b: *Result, p: *Parser) Error!bool {
    try p.errStr(.invalid_bin_types, tok, try p.typePairStr(a.ty, b.ty));
    a.value.tag = .unavailable;
    a.value.tag = .unavailable;
    return false;
}

fn shouldEval(a: *Result, b: *Result, p: *Parser) Error!bool {
    if (p.noEval) return false;
    if (a.value.tag != .unavailable and b.value.tag != .unavailable)
        return true;

    try a.saveValue(p);
    try b.saveValue(p);
    return p.noEval;
}

/// Saves value and replaces it with `.unavailable`.
pub fn saveValue(res: *Result, p: *Parser) !void {
    std.debug.assert(!p.inMacro);
    if (res.value.tag == .unavailable) return;

    if (!p.inMacro)
        try p.valueMap.put(res.node, res.value);
    res.value.tag = .unavailable;
}

pub fn castType(res: *Result, p: *Parser, to: Type, tok: TokenIndex) !void {
    var castKind: AST.CastKind = undefined;
    if (to.is(.Void)) {
        // everything can cast to void
        castKind = .ToVoid;
        res.value.tag = .unavailable;
    } else if (res.value.isZero() and to.isPointer()) {
        castKind = .NullToPointer;
    } else if (to.isScalar()) cast: {
        const oldFloat = res.ty.isFloat();
        const newFloat = to.isFloat();

        if (newFloat and res.ty.isPointer()) {
            try p.errStr(.invalid_cast_to_float, tok, try p.typeStr(to));
            return error.ParsingFailed;
        } else if (oldFloat and to.isPointer()) {
            try p.errStr(.invalid_cast_to_pointer, tok, try p.typeStr(res.ty));
            return error.ParsingFailed;
        }
        const oldReal = res.ty.isReal();
        const newReal = to.isReal();

        if (to.eql(res.ty, p.comp, false)) {
            castKind = .NoOP;
        } else if (to.is(.Bool)) {
            if (res.ty.isPointer()) {
                castKind = .PointerToBool;
            } else if (res.ty.isInt()) {
                if (!oldReal) {
                    res.ty = res.ty.makeReal();
                    try res.implicitCast(p, .ComplexIntToReal);
                }
                castKind = .IntToBool;
            } else if (oldFloat) {
                if (!oldReal) {
                    res.ty = res.ty.makeReal();
                    try res.implicitCast(p, .ComplexFloatToReal);
                }
                castKind = .FloatToBool;
            }
        } else if (to.isInt()) {
            if (res.ty.is(.Bool)) {
                if (!newReal) {
                    res.ty = to.makeReal();
                    try res.implicitCast(p, .BoolToInt);
                    castKind = .RealToComplexInt;
                } else {
                    castKind = .BoolToInt;
                }
            } else if (res.ty.isInt()) {
                if (oldReal and newReal) {
                    castKind = .IntCast;
                } else if (oldReal) {
                    res.ty = to.makeReal();
                    try res.implicitCast(p, .IntCast);
                    castKind = .RealToComplexInt;
                } else if (newReal) {
                    res.ty = res.ty.makeReal();
                    try res.implicitCast(p, .ComplexIntToReal);
                    castKind = .IntCast;
                } else {
                    castKind = .ComplexIntCast;
                }
            } else if (res.ty.isPointer()) {
                if (!newReal) {
                    res.ty = to.makeReal();
                    try res.implicitCast(p, .PointerToInt);
                    castKind = .RealToComplexInt;
                } else {
                    castKind = .PointerToInt;
                }
            } else if (oldReal and newReal) {
                castKind = .FloatToInt;
            } else if (oldReal) {
                res.ty = to.makeReal();
                try res.implicitCast(p, .FloatToInt);
                castKind = .RealToComplexInt;
            } else if (newReal) {
                res.ty = res.ty.makeReal();
                try res.implicitCast(p, .ComplexFloatToReal);
                castKind = .FloatToInt;
            } else {
                castKind = .ComplexFloatToComplexInt;
            }
        } else if (to.isPointer()) {
            if (res.ty.isArray())
                castKind = .ArrayToPointer
            else if (res.ty.isPointer())
                castKind = .Bitcast
            else if (res.ty.isFunc())
                castKind = .FunctionToPointer
            else if (res.ty.is(.Bool))
                castKind = .BoolToPointer
            else if (res.ty.isInt()) {
                if (!oldReal) {
                    res.ty = res.ty.makeReal();
                    try res.implicitCast(p, .ComplexIntToReal);
                }
                castKind = .IntToPointer;
            }
        } else if (newFloat) {
            if (res.ty.is(.Bool)) {
                if (!newReal) {
                    res.ty = to.makeReal();
                    try res.implicitCast(p, .BoolToFloat);
                    castKind = .RealToComplexFloat;
                } else {
                    castKind = .BoolToFloat;
                }
            } else if (res.ty.isInt()) {
                if (oldReal and newReal) {
                    castKind = .IntToFloat;
                } else if (oldReal) {
                    res.ty = to.makeReal();
                    try res.implicitCast(p, .IntToFloat);
                    castKind = .RealToComplexFloat;
                } else if (newReal) {
                    res.ty = res.ty.makeReal();
                    try res.implicitCast(p, .ComplexIntToReal);
                    castKind = .IntToFloat;
                } else {
                    castKind = .ComplexIntToComplexFloat;
                }
            } else if (oldReal and newReal) {
                castKind = .FloatCast;
            } else if (oldReal) {
                res.ty = to.makeReal();
                try res.implicitCast(p, .FloatCast);
                castKind = .RealToComplexFloat;
            } else if (newReal) {
                res.ty = res.ty.makeReal();
                try res.implicitCast(p, .ComplexFloatToReal);
                castKind = .FloatCast;
            } else {
                castKind = .ComplexFloatCast;
            }
        }

        if (res.value.tag == .unavailable) break :cast;

        const oldInt = res.ty.isInt() or res.ty.isPointer();
        const newInt = to.isInt() or to.isPointer();
        if (to.is(.Bool)) {
            res.value.toBool();
        } else if (oldFloat and newInt) {
            // Explicit cast, no conversion warning
            _ = res.value.floatToInt(res.ty, to, p.comp);
        } else if (newFloat and oldInt) {
            res.value.intToFloat(res.ty, to, p.comp);
        } else if (newFloat and oldFloat) {
            res.value.floatCast(res.ty, to, p.comp);
        } else if (oldInt and newInt) {
            res.value.intCast(res.ty, to, p.comp);
        }
    } else {
        try p.errStr(.invalid_cast_type, tok, try p.typeStr(to));
        return error.ParsingFailed;
    }

    if (to.containAnyQual()) try p.errStr(.qual_cast, tok, try p.typeStr(to));
    if (to.isInt() and res.ty.isPointer() and to.sizeCompare(res.ty, p.comp) == .lt) {
        try p.errStr(.cast_to_smaller_int, tok, try p.typePairStrExtra(to, " from ", res.ty));
    }

    res.ty = to;
    res.ty.qual = .{};
    res.node = try p.addNode(.{
        .tag = .ExplicitCast,
        .type = res.ty,
        .data = .{ .cast = .{ .operand = res.node, .kind = castKind } },
    });
}

/// Check if the integer value represented by `res` fits within the type bounds of `ty`.
/// This function compares the `res` value against the maximum and minimum values that
/// can be represented by the type `ty`.
/// @param res   The result object containing the value to be checked.
/// @param p     A pointer to the Parser object.
/// @param ty    The type within which the value should fit.
/// @return      Returns true if the value fits within the type bounds, false otherwise.
pub fn intFitsInType(res: Result, p: *Parser, ty: Type) bool {
    const maxInt = Value.int(ty.maxInt(p.comp));
    const minInt = Value.int(ty.minInt(p.comp));

    return res.value.compare(.lte, maxInt, res.ty, p.comp) and
        (res.ty.isUnsignedInt(p.comp) or res.value.compare(.gte, minInt, res.ty, p.comp));
}

const CoerceContext = union(enum) {
    assign,
    init,
    ret,
    arg: TokenIndex,
    testCoerce,

    fn note(ctx: CoerceContext, p: *Parser) !void {
        switch (ctx) {
            .arg => |tok| try p.errToken(.parameter_here, tok),
            .testCoerce => unreachable,
            else => {},
        }
    }

    fn typePairStr(ctx: CoerceContext, p: *Parser, dest_ty: Type, src_ty: Type) ![]const u8 {
        switch (ctx) {
            .assign, .init => return p.typePairStrExtra(dest_ty, " from incompatible type ", src_ty),
            .ret => return p.typePairStrExtra(src_ty, " from a function with incompatible result type ", dest_ty),
            .arg => return p.typePairStrExtra(src_ty, " to parameter of incompatible type ", dest_ty),
            .testCoerce => unreachable,
        }
    }
};

/// Perform assignment-like coercion to `dest_ty`.
pub fn coerce(res: *Result, p: *Parser, destTy: Type, tok: TokenIndex, ctx: CoerceContext) !void {
    return res.coerceExtra(p, destTy, tok, ctx) catch |er| switch (er) {
        error.CoercionFailed => unreachable,
        else => |e| return e,
    };
}

const Stage1Limitation = Error || error{CoercionFailed};

fn coerceExtra(res: *Result, p: *Parser, destTy: Type, tok: TokenIndex, ctx: CoerceContext) Stage1Limitation!void {
    // Subject of the coercion does not need to be qualified.
    var unqualTy = destTy.canonicalize(.standard);
    unqualTy.qual = .{};
    if (unqualTy.is(.Bool)) {
        if (res.ty.isScalar()) {
            // this is ridiculous but it's what clang does
            try res.boolCast(p, unqualTy, tok);
            return;
        }
    } else if (unqualTy.isInt()) {
        if (res.ty.isInt() or res.ty.isFloat()) {
            try res.intCast(p, unqualTy, tok);
            return;
        } else if (res.ty.isPointer()) {
            if (ctx == .testCoerce)
                return error.CoercionFailed;
            try p.errStr(.implicit_ptr_to_int, tok, try p.typePairStrExtra(res.ty, " to ", destTy));
            try ctx.note(p);
            try res.intCast(p, unqualTy, tok);
            return;
        }
    } else if (unqualTy.isFloat()) {
        if (res.ty.isInt() or res.ty.isFloat()) {
            try res.floatCast(p, unqualTy);
            return;
        }
    } else if (unqualTy.isPointer()) {
        if (res.value.isZero()) {
            try res.nullCast(p, destTy);
            return;
        } else if (res.ty.isInt()) {
            if (ctx == .testCoerce)
                return error.CoercionFailed;
            try p.errStr(.implicit_int_to_ptr, tok, try p.typePairStrExtra(res.ty, " to ", destTy));
            try ctx.note(p);
            try res.ptrCast(p, unqualTy);
            return;
        } else if (res.ty.isVoidStar() or unqualTy.isVoidStar() or unqualTy.eql(res.ty, p.comp, true)) {
            return; // ok
        } else if (unqualTy.eql(res.ty, p.comp, false)) {
            if (!unqualTy.getElemType().qual.hasQuals(res.ty.getElemType().qual)) {
                try p.errStr(switch (ctx) {
                    .assign => .ptr_assign_discards_quals,
                    .init => .ptr_init_discards_quals,
                    .ret => .ptr_ret_discards_quals,
                    .arg => .ptr_arg_discards_quals,
                    .testCoerce => return error.CoercionFailed,
                }, tok, try ctx.typePairStr(p, destTy, res.ty));
            }
            try res.ptrCast(p, unqualTy);
            return;
        } else if (res.ty.isPointer()) {
            try p.errStr(switch (ctx) {
                .assign => .incompatible_ptr_assign,
                .init => .incompatible_ptr_init,
                .ret => .incompatible_return,
                .arg => .incompatible_arg,
                .testCoerce => return error.CoercionFailed,
            }, tok, try ctx.typePairStr(p, destTy, res.ty));
            try ctx.note(p);
            try res.ptrCast(p, unqualTy);
            return;
        }
    } else if (unqualTy.isRecord()) {
        if (unqualTy.eql(res.ty, p.comp, false)) {
            return; // ok
        }

        if (ctx == .arg) {
            if (unqualTy.get(.Union)) |unionTy| {
                if (destTy.hasAttribute(.transparent_union)) transparent_union: {
                    res.coerceExtra(p, unionTy.data.record.fields[0].ty, tok, .testCoerce) catch |err| switch (err) {
                        error.CoercionFailed => break :transparent_union,
                        else => |e| return e,
                    };
                    res.node = try p.addNode(.{
                        .tag = .UnionInitExpr,
                        .type = destTy,
                        .data = .{ .unionInit = .{ .fieldIndex = 0, .node = res.node } },
                    });
                    res.ty = destTy;
                    return;
                }
            }
        } else if (unqualTy.is(.Vector)) {
            if (unqualTy.eql(res.ty, p.comp, false))
                return; //ok
        }
    } else {
        if (ctx == .assign and (unqualTy.isArray() or unqualTy.isFunc())) {
            try p.errToken(.not_assignable, tok);
            return;
        } else if (ctx == .testCoerce) {
            return error.CoercionFailed;
        }
        // This case should not be possible and an error should have already been emitted but we
        // might still have attempted to parse further so return error.ParsingFailed here to stop.
        return error.ParsingFailed;
    }

    try p.errStr(switch (ctx) {
        .assign => .incompatible_assign,
        .init => .incompatible_init,
        .ret => .incompatible_return,
        .arg => .incompatible_arg,
        .testCoerce => return error.CoercionFailed,
    }, tok, try ctx.typePairStr(p, destTy, res.ty));
    try ctx.note(p);
}
