const std = @import("std");
const assert = std.debug.assert;
const Type = @import("../AST/Type.zig");
const AST = @import("../AST/AST.zig");
const Diagnostics = @import("../Basic/Diagnostics.zig");
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

// validate result is valid
pub fn expect(res: Result, p: *Parser) Error!void {
    if (p.inMacro) {
        if (res.value.isUnavailable()) {
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
        return res.value.isUnavailable();
    return res.node == .none;
}

pub fn maybeWarnUnused(res: Result, p: *Parser, exprStart: TokenIndex, errStart: usize) Error!void {
    if (res.ty.is(.Void) or res.node == .none)
        return;

    // don't warn about unused result if the expression contained errors besides other unused results
    for (p.comp.diagnostics.list.items[errStart..]) |errItem| {
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

pub fn boolRes(lhs: *Result, p: *Parser, tag: AstTag, rhs: Result) !void {
    if (lhs.value.tag == .nullptrTy)
        lhs.value = Value.int(0);

    if (!lhs.ty.isInvalid())
        lhs.ty = Type.Int;

    return lhs.bin(p, tag, rhs);
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

pub fn adjustCondExprPtrs(lhs: *Result, tok: TokenIndex, rhs: *Result, p: *Parser) !bool {
    assert(lhs.ty.isPointer() and rhs.ty.isPointer());

    const lhsElem = lhs.ty.getElemType();
    const rhsElem = rhs.ty.getElemType();
    if (lhsElem.eql(rhsElem, p.comp, true))
        return true;

    var adjustedElemType = try p.arena.create(Type);
    adjustedElemType.* = lhsElem;

    const hasVoidStarBranch = lhs.ty.isVoidStar() or rhs.ty.isVoidStar();
    const onlyQualsDiffer = lhsElem.eql(rhsElem, p.comp, false);
    const pointersCompatible = onlyQualsDiffer or hasVoidStarBranch;

    if (!pointersCompatible or hasVoidStarBranch) {
        if (!pointersCompatible)
            try p.errStr(.pointer_mismatch, tok, try p.typePairStrExtra(lhs.ty, " and ", rhs.ty));
        adjustedElemType.* = Type.Void;
    }

    if (pointersCompatible)
        adjustedElemType.qual = lhsElem.qual.mergeCVQualifiers(rhsElem.qual);

    if (!adjustedElemType.eql(lhsElem, p.comp, true)) {
        lhs.ty = .{
            .data = .{ .subType = adjustedElemType },
            .specifier = .Pointer,
        };
        try lhs.implicitCast(p, .Bitcast);
    }

    if (!adjustedElemType.eql(rhsElem, p.comp, true)) {
        rhs.ty = .{
            .data = .{ .subType = adjustedElemType },
            .specifier = .Pointer,
        };
        try rhs.implicitCast(p, .Bitcast);
    }
    return true;
}

/// Return true if both are same type
/// Adjust types for binary operation, returns true if the result can and should be evaluated.
pub fn adjustTypes(lhs: *Result, token: TokenIndex, rhs: *Result, p: *Parser, kind: enum {
    integer,
    arithmetic,
    booleanLogic,
    relational,
    equality,
    conditional,
    add,
    sub,
}) !bool {
    if (rhs.ty.isInvalid()) {
        try lhs.saveValue(p);
        lhs.ty = Type.Invalid;
    }

    if (lhs.ty.isInvalid())
        return false;

    try lhs.lvalConversion(p);
    try rhs.lvalConversion(p);

    const lhsIsVec = lhs.ty.is(.Vector);
    const rhsIsVec = rhs.ty.is(.Vector);
    if (lhsIsVec and rhsIsVec) {
        if (lhs.ty.eql(rhs.ty, p.comp, false))
            return lhs.shouldEval(rhs, p);
        return lhs.invalidBinTy(token, rhs, p);
    } else if (lhsIsVec or rhsIsVec) {
        const vecOperand = if (lhsIsVec) lhs else rhs;
        const scalarOperand = if (lhsIsVec) rhs else lhs;
        if (scalarOperand.coerceExtra(p, vecOperand.ty.getElemType(), token, .testCoerce)) {
            try scalarOperand.saveValue(p);
            try scalarOperand.implicitCast(p, .VectorSplat);
            return lhs.shouldEval(rhs, p);
        } else |err| switch (err) {
            error.CoercionFailed => return lhs.invalidBinTy(token, rhs, p),
            else => |e| return e,
        }
    }

    const lhsIsInt = lhs.ty.isInt();
    const rhsIsInt = rhs.ty.isInt();
    if (lhsIsInt and rhsIsInt) {
        try lhs.usualArithmeticConversion(rhs, p, token);
        return lhs.shouldEval(rhs, p);
    }

    if (kind == .integer)
        return lhs.invalidBinTy(token, rhs, p);

    const lhsIsFloat = lhs.ty.isFloat();
    const rhsIsFloat = rhs.ty.isFloat();
    const lhsIsArithmetic = lhsIsInt or lhsIsFloat;
    const rhsIsArithmetic = rhsIsInt or rhsIsFloat;
    if (lhsIsArithmetic and rhsIsArithmetic) {
        // <, <=, >, >= only work on real types
        if (kind == .relational and (!lhs.ty.isReal() or !rhs.ty.isReal()))
            return lhs.invalidBinTy(token, rhs, p);

        try lhs.usualArithmeticConversion(rhs, p, token);
        return lhs.shouldEval(rhs, p);
    }

    if (kind == .arithmetic)
        return lhs.invalidBinTy(token, rhs, p);

    const lhsIsNullptr = lhs.ty.is(.NullPtrTy);
    const rhsIsNullptr = rhs.ty.is(.NullPtrTy);
    const lhsIsPtr = lhs.ty.isPointer();
    const rhsIsPtr = rhs.ty.isPointer();
    const lhsIsScalar = lhsIsArithmetic or lhsIsPtr;
    const rhsIsScalar = rhsIsArithmetic or rhsIsPtr;
    switch (kind) {
        .booleanLogic => {
            if (!(lhsIsScalar or lhsIsNullptr) or !(rhsIsScalar or rhsIsNullptr))
                return lhs.invalidBinTy(token, rhs, p);

            // Do integer promotions but nothing else
            if (lhsIsInt) try lhs.intCast(p, lhs.ty.integerPromotion(p.comp), token);
            if (rhsIsInt) try rhs.intCast(p, rhs.ty.integerPromotion(p.comp), token);
            return lhs.shouldEval(rhs, p);
        },

        .relational, .equality => {
            if (kind == .equality and (lhsIsNullptr or rhsIsNullptr)) {
                if (lhsIsNullptr and rhsIsNullptr)
                    return lhs.shouldEval(rhs, p);

                const nullPtrRes = if (lhsIsNullptr) lhs else rhs;
                const otherRes = if (lhsIsNullptr) rhs else lhs;
                if (otherRes.ty.isPointer()) {
                    try nullPtrRes.nullCast(p, otherRes.ty);
                    return otherRes.shouldEval(nullPtrRes, p);
                } else if (otherRes.value.isZero()) {
                    otherRes.value = .{ .tag = .nullptrTy };
                    try otherRes.nullCast(p, nullPtrRes.ty);
                    return otherRes.shouldEval(nullPtrRes, p);
                }
                return lhs.invalidBinTy(token, rhs, p);
            }

            // comparisons between floats and pointes not allowed
            if (!lhsIsScalar or !rhsIsScalar or (lhsIsFloat and rhsIsPtr) or (rhsIsFloat and lhsIsPtr))
                return lhs.invalidBinTy(token, rhs, p);

            if ((lhsIsInt or rhsIsInt) and !(lhs.value.isZero() or rhs.value.isZero())) {
                try p.errStr(.comparison_ptr_int, token, try p.typePairStr(lhs.ty, rhs.ty));
            } else if (lhsIsPtr and rhsIsPtr) {
                if (!lhs.ty.isVoidStar() and !rhs.ty.isVoidStar() and !lhs.ty.eql(rhs.ty, p.comp, false))
                    try p.errStr(.comparison_distinct_ptr, token, try p.typePairStr(lhs.ty, rhs.ty));
            } else if (lhsIsPtr) {
                try rhs.ptrCast(p, lhs.ty);
            } else {
                assert(rhsIsPtr);
                try lhs.ptrCast(p, rhs.ty);
            }

            return lhs.shouldEval(rhs, p);
        },

        .conditional => {
            // doesn't matter what we return here, as the result is ignored
            if (lhs.ty.is(.Void) or rhs.ty.is(.Void)) {
                try lhs.toVoid(p);
                try rhs.toVoid(p);
                return true;
            }

            if (lhsIsNullptr and rhsIsNullptr)
                return true;

            if ((lhsIsPtr and rhsIsInt) or (lhsIsInt and rhsIsPtr)) {
                if (lhs.value.isZero() or rhs.value.isZero()) {
                    try lhs.nullCast(p, rhs.ty);
                    try rhs.nullCast(p, lhs.ty);
                    return true;
                }

                const intType = if (lhsIsInt) lhs else rhs;
                const ptrType = if (lhsIsPtr) lhs else rhs;

                try p.errStr(.implicit_int_to_ptr, token, try p.typePairStrExtra(intType.ty, " to ", ptrType.ty));
                try intType.ptrCast(p, ptrType.ty);
                return true;
            }

            if (lhsIsPtr and rhsIsPtr)
                return lhs.adjustCondExprPtrs(token, rhs, p);

            if ((lhsIsPtr and rhsIsNullptr) or (lhsIsNullptr and rhsIsPtr)) {
                const nullPtrRes = if (lhsIsNullptr) lhs else rhs;
                const ptrRes = if (lhsIsNullptr) rhs else lhs;
                try nullPtrRes.nullCast(p, ptrRes.ty);
                return true;
            }

            if (lhs.ty.isRecord() and rhs.ty.isRecord() and lhs.ty.eql(rhs.ty, p.comp, false))
                return true;

            return lhs.invalidBinTy(token, rhs, p);
        },

        .add => {
            // if both aren't arithmetic one should be pointer and the other an integer
            if (lhsIsPtr == rhsIsPtr or lhsIsInt == rhsIsInt)
                return lhs.invalidBinTy(token, rhs, p);

            // Do integer promotions but nothing else
            if (lhsIsInt) try lhs.intCast(p, lhs.ty.integerPromotion(p.comp), token);
            if (rhsIsInt) try rhs.intCast(p, rhs.ty.integerPromotion(p.comp), token);

            // The result type is the type of the pointer operand
            if (lhsIsInt) lhs.ty = rhs.ty else rhs.ty = lhs.ty;
            return lhs.shouldEval(rhs, p);
        },

        .sub => {
            // if both aren't arithmetic then either both should be pointers or just a
            if (!lhsIsPtr or !(rhsIsPtr or rhsIsInt)) return lhs.invalidBinTy(token, rhs, p);

            if (lhsIsPtr and rhsIsPtr) {
                if (!lhs.ty.eql(rhs.ty, p.comp, false))
                    try p.errStr(.incompatible_pointers, token, try p.typePairStr(lhs.ty, rhs.ty));
                lhs.ty = p.comp.types.ptrdiff;
            }

            // Do integer promotion on b if needed
            if (rhsIsInt) try rhs.intCast(p, rhs.ty.integerPromotion(p.comp), token);
            return lhs.shouldEval(rhs, p);
        },

        else => return lhs.invalidBinTy(token, rhs, p),
    }

    return lhs.shouldEval(rhs, p);
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
    }
    // Decay an array type to a pointer to its first element.
    else if (res.ty.isArray()) {
        res.value.tag = .unavailable;
        res.ty.decayArray();
        try res.implicitCast(p, .ArrayToPointer);
    }
    // Perform l-value to r-value conversion
    else if (!p.inMacro and p.tempTree().isLValue(res.node)) {
        res.ty.qual = .{};
        try res.implicitCast(p, .LValToRVal);
    }
}

pub fn boolCast(res: *Result, p: *Parser, boolType: Type, tok: TokenIndex) Error!void {
    if (res.ty.isArray()) {
        if (res.value.tag == .bytes)
            try p.errStr(.string_literal_to_bool, tok, try p.typePairStrExtra(res.ty, " to ", boolType))
        else
            try p.errStr(.array_address_to_bool, tok, p.getTokenText(tok));

        try res.lvalConversion(p);
        res.value = Value.int(1);
        res.ty = boolType;
        try res.implicitCast(p, .PointerToBool);
    } else if (res.ty.isPointer()) {
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
            const realIntTy = intType.makeReal();
            if (!res.ty.eql(realIntTy, p.comp, false)) {
                res.ty = realIntTy;
                try res.implicitCast(p, .IntCast);
            }
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
    }
    // src type is int type
    else if (res.ty.isInt()) {
        res.value.intToFloat(res.ty, floatType, p.comp);
        const oldReal = res.ty.isReal();
        const newReal = floatType.isReal();
        if (oldReal and newReal) {
            res.ty = floatType;
            try res.implicitCast(p, .IntToFloat);
        } else if (oldReal) {
            res.ty = floatType.makeReal();
            try res.implicitCast(p, .IntToFloat);
            res.ty = floatType;
            try res.implicitCast(p, .RealToComplexFloat);
        } else if (newReal) {
            res.ty = res.ty.makeReal();
            try res.implicitCast(p, .ComplexIntToReal);
            res.ty = floatType;
            try res.implicitCast(p, .IntToFloat);
        } else {
            res.ty = floatType;
            try res.implicitCast(p, .ComplexIntToComplexFloat);
        }
    }
    // src type is not equal float type
    else if (!res.ty.eql(floatType, p.comp, true)) {
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

/// converts a bool or integer to a pointer
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

/// converts pointer to one with a different child type
fn ptrChildTypeCast(res: *Result, p: *Parser, ptrTy: Type) Error!void {
    res.ty = ptrTy;
    return res.implicitCast(p, .Bitcast);
}

pub fn toVoid(res: *Result, p: *Parser) Error!void {
    if (!res.ty.is(.Void)) {
        res.ty = Type.Void;
        try res.implicitCast(p, .ToVoid);
    }
}

pub fn nullCast(res: *Result, p: *Parser, ptrType: Type) Error!void {
    if (!res.value.isZero() and !res.ty.is(.NullPtrTy)) return;
    res.ty = ptrType;
    try res.implicitCast(p, .NullToPointer);
}

/// Attempts to perform a cast on the result's value using the usual conversion rules.
/// The usual conversion rules are as follows:
///   - If the result's type is an integer, then perform an integer promotion on the result's type.
///
/// Arguments:
///   - res: The result that should be cast.
///   - p: The parser that this result is being parsed with.
///   - tok: The token index of the token that this result is being cast to.
///
/// Returns:
///   - The cast value if the cast was successful. Otherwise, an error.
pub fn usualUnaryConversion(res: *Result, p: *Parser, tok: TokenIndex) Error!void {
    if (res.ty.isInt())
        try res.intCast(p, res.ty.integerPromotion(p.comp), tok);
}

fn usualArithmeticConversion(lhs: *Result, rhs: *Result, p: *Parser, tok: TokenIndex) Error!void {
    try lhs.usualUnaryConversion(p, tok);
    try rhs.usualUnaryConversion(p, tok);

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

        if (p.comp.target.c_type_bit_size(.longdouble) == 64)
            if (try lhs.floatConversion(rhs, lhsSpec, rhsSpec, p, floatTypes[0]))
                return;

        if (try lhs.floatConversion(rhs, lhsSpec, rhsSpec, p, floatTypes[3])) return;
        if (try lhs.floatConversion(rhs, lhsSpec, rhsSpec, p, floatTypes[4])) return;
        if (try lhs.floatConversion(rhs, lhsSpec, rhsSpec, p, floatTypes[5])) return;
    }

    if (lhs.ty.eql(rhs.ty, p.comp, true)) {
        // cast to promoted type
        try lhs.intCast(p, lhs.ty, tok);
        try rhs.intCast(p, rhs.ty, tok);
        return;
    }

    const targetTy = lhs.ty.integerConversion(rhs.ty, p.comp);
    if (!targetTy.isReal()) {
        try lhs.saveValue(p);
        try rhs.saveValue(p);
    }
    try lhs.intCast(p, targetTy, tok);
    try rhs.intCast(p, targetTy, tok);
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

fn invalidBinTy(lhs: *Result, tok: TokenIndex, rhs: *Result, p: *Parser) Error!bool {
    try p.errStr(.invalid_bin_types, tok, try p.typePairStr(lhs.ty, rhs.ty));
    lhs.value.tag = .unavailable;
    lhs.value.tag = .unavailable;
    lhs.ty = Type.Invalid;
    return false;
}

/// Return true if the result of the expression should be evaluated.
fn shouldEval(lhs: *Result, rhs: *Result, p: *Parser) Error!bool {
    if (p.noEval) return false;
    if (!lhs.value.isUnavailable() and !rhs.value.isUnavailable())
        return true;

    try lhs.saveValue(p);
    try rhs.saveValue(p);
    return p.noEval;
}

/// Saves value and replaces it with `.unavailable`.
pub fn saveValue(res: *Result, p: *Parser) !void {
    assert(!p.inMacro);
    if (res.value.isUnavailable() or res.value.tag == .nullptrTy)
        return;

    if (!p.inMacro)
        try p.valueMap.put(res.node, res.value);

    res.value.tag = .unavailable;
}

pub fn castType(res: *Result, p: *Parser, to: Type, tok: TokenIndex) !void {
    var explicitCastKind: AST.CastKind = undefined;
    if (to.is(.Void)) {
        // everything can cast to void
        explicitCastKind = .ToVoid;
        res.value.tag = .unavailable;
    } else if (to.is(.NullPtrTy)) {
        if (res.ty.is(.NullPtrTy)) {
            explicitCastKind = .NoOP;
        } else {
            try p.errStr(.invalid_object_cast, tok, try p.typePairStrExtra(res.ty, " to ", to));
            return error.ParsingFailed;
        }
    } else if (res.ty.is(.NullPtrTy)) {
        if (to.is(.Bool)) {
            try res.nullCast(p, res.ty);
            res.value.toBool();
            res.ty = Type.Bool;
            try res.implicitCast(p, .PointerToBool);
            try res.saveValue(p);
        } else if (to.isPointer()) {
            try res.nullCast(p, to);
        } else {
            try p.errStr(.invalid_object_cast, tok, try p.typePairStrExtra(res.ty, " to ", to));
            return error.ParsingFailed;
        }
        explicitCastKind = .NoOP;
    } else if (res.value.isZero() and to.isPointer()) {
        explicitCastKind = .NullToPointer;
    } else if (to.isScalar()) cast: {
        const oldIsFloat = res.ty.isFloat();
        const newIsFloat = to.isFloat();

        if (newIsFloat and res.ty.isPointer()) {
            try p.errStr(.invalid_cast_to_float, tok, try p.typeStr(to));
            return error.ParsingFailed;
        } else if (oldIsFloat and to.isPointer()) {
            try p.errStr(.invalid_cast_to_pointer, tok, try p.typeStr(res.ty));
            return error.ParsingFailed;
        }

        const oldIsInt = res.ty.isInt();
        const oldIsReal = res.ty.isReal();
        const newIsReal = to.isReal();
        const oldIsComplex = !oldIsReal;
        const newIsComplex = !newIsReal;

        if (to.eql(res.ty, p.comp, false)) {
            explicitCastKind = .NoOP;
        } else if (to.is(.Bool)) {
            if (res.ty.isPointer()) {
                explicitCastKind = .PointerToBool;
            } else if (oldIsInt or oldIsFloat) {
                if (oldIsComplex) {
                    res.ty = res.ty.makeReal();
                    try res.implicitCast(p, if (oldIsInt) .ComplexIntToReal else .ComplexFloatToReal);
                }
                explicitCastKind = if (oldIsInt) .IntToBool else .FloatToBool;
            }
        } else if (to.isInt()) {
            if (res.ty.is(.Bool)) {
                if (newIsComplex) {
                    res.ty = to.makeReal();
                    try res.implicitCast(p, .BoolToInt);
                }
                explicitCastKind = if (newIsReal) .BoolToInt else .RealToComplexInt;
            } else if (res.ty.isInt()) {
                const needIntToRealCast = oldIsComplex and newIsReal;
                const needRealToComplexIntCast = oldIsReal and newIsComplex;

                if (needIntToRealCast) {
                    res.ty = res.ty.makeReal();
                    try res.implicitCast(p, .ComplexIntToReal);
                } else if (needRealToComplexIntCast) {
                    res.ty = to.makeReal();
                    try res.implicitCast(p, .IntCast);
                }

                explicitCastKind = if ((oldIsReal and newIsReal) or needIntToRealCast)
                    .IntCast
                else if (needRealToComplexIntCast)
                    .RealToComplexInt
                else
                    .ComplexIntCast;
            } else if (res.ty.isPointer()) {
                if (newIsComplex) {
                    res.ty = to.makeReal();
                    try res.implicitCast(p, .PointerToInt);
                    explicitCastKind = .RealToComplexInt;
                } else {
                    explicitCastKind = .PointerToInt;
                }
            } else if (oldIsReal and newIsReal) {
                explicitCastKind = .FloatToInt;
            } else if (oldIsReal) {
                res.ty = to.makeReal();
                try res.implicitCast(p, .FloatToInt);
                explicitCastKind = .RealToComplexInt;
            } else if (newIsReal) {
                res.ty = res.ty.makeReal();
                try res.implicitCast(p, .ComplexFloatToReal);
                explicitCastKind = .FloatToInt;
            } else {
                explicitCastKind = .ComplexFloatToComplexInt;
            }
        } else if (to.isPointer()) {
            if (res.ty.isArray())
                explicitCastKind = .ArrayToPointer
            else if (res.ty.isPointer())
                explicitCastKind = .Bitcast
            else if (res.ty.isFunc())
                explicitCastKind = .FunctionToPointer
            else if (res.ty.is(.Bool))
                explicitCastKind = .BoolToPointer
            else if (res.ty.isInt()) {
                if (oldIsComplex) {
                    res.ty = res.ty.makeReal();
                    try res.implicitCast(p, .ComplexIntToReal);
                }
                explicitCastKind = .IntToPointer;
            }
        } else if (newIsFloat) {
            if (res.ty.is(.Bool)) {
                if (newIsComplex) {
                    res.ty = to.makeReal();
                    try res.implicitCast(p, .BoolToFloat);
                    explicitCastKind = .RealToComplexFloat;
                } else {
                    explicitCastKind = .BoolToFloat;
                }
            } else if (res.ty.isInt()) {
                if (oldIsReal and newIsReal) {
                    explicitCastKind = .IntToFloat;
                } else if (oldIsReal) {
                    res.ty = to.makeReal();
                    try res.implicitCast(p, .IntToFloat);
                    explicitCastKind = .RealToComplexFloat;
                } else if (newIsReal) {
                    res.ty = res.ty.makeReal();
                    try res.implicitCast(p, .ComplexIntToReal);
                    explicitCastKind = .IntToFloat;
                } else {
                    explicitCastKind = .ComplexIntToComplexFloat;
                }
            } else if (oldIsReal and newIsReal) {
                explicitCastKind = .FloatCast;
            } else if (oldIsReal) {
                res.ty = to.makeReal();
                try res.implicitCast(p, .FloatCast);
                explicitCastKind = .RealToComplexFloat;
            } else if (newIsReal) {
                res.ty = res.ty.makeReal();
                try res.implicitCast(p, .ComplexFloatToReal);
                explicitCastKind = .FloatCast;
            } else {
                explicitCastKind = .ComplexFloatCast;
            }
        }

        if (res.value.isUnavailable()) break :cast;

        const oldInt = res.ty.isInt() or res.ty.isPointer();
        const newInt = to.isInt() or to.isPointer();
        if (to.is(.Bool)) {
            res.value.toBool();
        } else if (oldIsFloat and newInt) {
            // Explicit cast, no conversion warning
            _ = res.value.floatToInt(res.ty, to, p.comp);
        } else if (newIsFloat and oldInt) {
            res.value.intToFloat(res.ty, to, p.comp);
        } else if (newIsFloat and oldIsFloat) {
            res.value.floatCast(res.ty, to, p.comp);
        } else if (oldInt and newInt) {
            if (to.hasIncompleteSize()) {
                try p.errStr(.cast_to_incomplete_type, tok, try p.typeStr(to));
                return error.ParsingFailed;
            }
            res.value.intCast(res.ty, to, p.comp);
        }
    } else if (to.get(.Union)) |unionTy| {
        if (unionTy.data.record.hasFieldOfType(res.ty, p.comp)) {
            explicitCastKind = .UnionCast;
            try p.errToken(.gnu_union_cast, tok);
        } else {
            if (unionTy.data.record.isIncomplete())
                try p.errStr(.cast_to_incomplete_type, tok, try p.typeStr(to))
            else
                try p.errStr(.invalid_union_cast, tok, try p.typeStr(res.ty));
            return error.ParsingFailed;
        }
    } else {
        if (to.is(.AutoType))
            try p.errToken(.invalid_cast_to_auto_type, tok)
        else
            try p.errStr(.invalid_cast_type, tok, try p.typeStr(to));
        return error.ParsingFailed;
    }

    if (to.containAnyQual())
        try p.errStr(.qual_cast, tok, try p.typeStr(to));
    if (to.isInt() and res.ty.isPointer() and to.sizeCompare(res.ty, p.comp) == .lt)
        try p.errStr(.cast_to_smaller_int, tok, try p.typePairStrExtra(to, " from ", res.ty));

    res.ty = to;
    res.ty.qual = .{};
    res.node = try p.addNode(.{
        .tag = .ExplicitCast,
        .type = res.ty,
        .data = .{ .cast = .{ .operand = res.node, .kind = explicitCastKind } },
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
    if (res.ty.isInvalid() or destTy.isInvalid()) {
        res.ty = Type.Invalid;
        return;
    }
    return res.coerceExtra(p, destTy, tok, ctx) catch |er| switch (er) {
        error.CoercionFailed => unreachable,
        else => |e| return e,
    };
}

fn coerceExtra(
    res: *Result,
    p: *Parser,
    destTy: Type,
    tok: TokenIndex,
    ctx: CoerceContext,
) (Error || error{CoercionFailed})!void {
    // Subject of the coercion does not need to be qualified.
    var unqualTy = destTy.canonicalize(.standard);
    unqualTy.qual = .{};
    if (unqualTy.is(.NullPtrTy)) {
        if (res.ty.is(.NullPtrTy)) return;
    }
    // dest type is bool
    else if (unqualTy.is(.Bool)) {
        if (res.ty.isScalar() and !res.ty.is(.NullPtrTy)) {
            // this is ridiculous but it's what clang does
            try res.boolCast(p, unqualTy, tok);
            return;
        }
    }
    // dest type is int
    else if (unqualTy.isInt()) {
        if (res.ty.isArithmetic()) {
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
    }
    // dest type is float
    else if (unqualTy.isFloat()) {
        if (res.ty.isArithmetic()) {
            try res.floatCast(p, unqualTy);
            return;
        }
    }
    // dest type is pointer
    else if (unqualTy.isPointer()) {
        if (res.value.isZero() or res.ty.is(.NullPtrTy)) {
            try res.nullCast(p, destTy);
            return;
        } else if (res.ty.isInt() and res.ty.isReal()) {
            if (ctx == .testCoerce)
                return error.CoercionFailed;
            try p.errStr(.implicit_int_to_ptr, tok, try p.typePairStrExtra(res.ty, " to ", destTy));
            try ctx.note(p);
            try res.ptrCast(p, unqualTy);
            return;
        } else if (res.ty.isVoidStar() or unqualTy.eql(res.ty, p.comp, true)) {
            return; // ok
        } else if (unqualTy.isVoidStar() and res.ty.isPointer() or (res.ty.isInt() and res.ty.isReal())) {
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
                .arg => .incompatible_ptr_arg,
                .testCoerce => return error.CoercionFailed,
            }, tok, try ctx.typePairStr(p, destTy, res.ty));
            try ctx.note(p);
            try res.ptrChildTypeCast(p, unqualTy);
            return;
        }
    }
    // dest type is record
    else if (unqualTy.isRecord()) {
        if (unqualTy.eql(res.ty, p.comp, false))
            return; // ok

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
    }
    // other type
    else {
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

    try p.errStr(
        switch (ctx) {
            .assign => .incompatible_assign,
            .init => .incompatible_init,
            .ret => .incompatible_return,
            .arg => .incompatible_arg,
            .testCoerce => return error.CoercionFailed,
        },
        tok,
        try ctx.typePairStr(p, destTy, res.ty),
    );
    try ctx.note(p);
}
