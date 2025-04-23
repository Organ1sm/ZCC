const std = @import("std");
const assert = std.debug.assert;
const Type = @import("../AST/Type.zig");
const AST = @import("../AST/AST.zig");
const Diagnostics = @import("../Basic/Diagnostics.zig");
const Value = @import("../AST/Value.zig");
const Parser = @import("Parser.zig");

const Node = AST.Node;
const TokenIndex = AST.TokenIndex;
const Error = Parser.Error;

const Result = @This();

node: Node.Index,
ty: Type = Type.Int,
value: Value = .{},

pub fn str(res: Result, p: *Parser) ![]const u8 {
    switch (res.value.optRef) {
        .none => return "(none)",
        .null => return "nullptr_t",
        else => {},
    }

    const stringsTop = p.strings.items.len;
    defer p.strings.items.len = stringsTop;

    try res.value.print(res.ty, p.comp, p.strings.writer());
    return try p.comp.diagnostics.arena.allocator().dupe(u8, p.strings.items[stringsTop..]);
}

pub fn maybeWarnUnused(res: Result, p: *Parser, exprStart: TokenIndex, errStart: usize) Error!void {
    if (res.ty.is(.Void))
        return;

    // don't warn about unused result if the expression contained errors besides other unused results
    for (p.comp.diagnostics.list.items[errStart..]) |errItem| {
        if (errItem.tag != .unused_value) return;
    }

    var curNode = res.node;
    while (true)
        switch (curNode.get(&p.tree)) {
            .assignExpr,
            .mulAssignExpr,
            .divAssignExpr,
            .modAssignExpr,
            .addAssignExpr,
            .subAssignExpr,
            .shlAssignExpr,
            .shrAssignExpr,
            .bitAndAssignExpr,
            .bitXorAssignExpr,
            .bitOrAssignExpr,
            .preIncExpr,
            .preDecExpr,
            .postIncExpr,
            .postDecExpr,
            => return,

            .callExpr => |call| {
                const callInfo = p.tree.callableResultUsage(call.callee) orelse return;
                if (callInfo.nodiscard) try p.errStr(.nodiscard_unused, exprStart, p.getTokenText(callInfo.token));
                if (callInfo.warnUnusedResult) try p.errStr(.warn_unused_result, exprStart, p.getTokenText(callInfo.token));
                return;
            },

            .stmtExpr => |stmtExpr| {
                const compound = stmtExpr.operand.get(&p.tree).compoundStmt;
                curNode = compound.body[compound.body.len - 1];
            },

            .commaExpr => |comma| curNode = comma.rhs,
            .parenExpr => |grouped| curNode = grouped.operand,

            else => break,
        };

    try p.errToken(.unused_value, exprStart);
}

pub fn boolRes(lhs: *Result, p: *Parser, tag: std.meta.Tag(Node), rhs: Result, token: TokenIndex) !void {
    if (lhs.value.isNull()) lhs.value = Value.zero;
    if (!lhs.ty.isInvalid()) lhs.ty = Type.Int;

    return lhs.bin(p, tag, rhs, token);
}

pub fn bin(lhs: *Result, p: *Parser, rtTag: std.meta.Tag(Node), rhs: Result, token: TokenIndex) !void {
    const binData: Node.Binary = .{
        .opToken = token,
        .lhs = lhs.node,
        .rhs = rhs.node,
        .type = lhs.ty,
    };
    switch (rtTag) {
        // zig fmt: off
            inline .commaExpr, .assignExpr, .mulAssignExpr, .divAssignExpr,
            .modAssignExpr, .addAssignExpr, .subAssignExpr, .shlAssignExpr,
            .shrAssignExpr, .bitAndAssignExpr, .bitXorAssignExpr,
            .bitOrAssignExpr, .boolOrExpr, .boolAndExpr, .bitOrExpr,
            .bitXorExpr, .bitAndExpr, .equalExpr, .notEqualExpr,
            .lessThanExpr, .lessThanEqualExpr, .greaterThanExpr,
            .greaterThanEqualExpr, .shlExpr, .shrExpr, .addExpr,
            .subExpr, .mulExpr, .divExpr, .modExpr,
            // zig fmt: on
        => |tag| lhs.node = try p.addNode(@unionInit(Node, @tagName(tag), binData)),
        else => unreachable,
    }
}

pub fn un(operand: *Result, p: *Parser, rtTag: std.meta.Tag(Node), token: TokenIndex) Error!void {
    const unData: Node.Unary = .{
        .opToken = token,
        .operand = operand.node,
        .type = operand.ty,
    };
    switch (rtTag) {
        // zig fmt: off
            inline .addrOfExpr, .derefExpr, .plusExpr, .negateExpr,
            .bitNotExpr, .boolNotExpr, .preIncExpr, .preDecExpr,
            .imagExpr, .realExpr, .postIncExpr,.postDecExpr,
            .parenExpr, .stmtExpr, .imaginaryLiteral, 
            // zig fmt: on
        => |tag| operand.node = try p.addNode(@unionInit(Node, @tagName(tag), unData)),
        else => unreachable,
    }
}

pub fn implicitCast(operand: *Result, p: *Parser, kind: Node.Cast.Kind, token: TokenIndex) Error!void {
    operand.node = try p.addNode(.{
        .cast = .{
            .lparen = token,
            .kind = kind,
            .operand = operand.node,
            .type = operand.ty,
            .implicit = true,
        },
    });
}

pub fn adjustCondExprPtrs(lhs: *Result, token: TokenIndex, rhs: *Result, p: *Parser) !bool {
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
            try p.errStr(.pointer_mismatch, token, try p.typePairStrExtra(lhs.ty, " and ", rhs.ty));
        adjustedElemType.* = Type.Void;
    }

    if (pointersCompatible)
        adjustedElemType.qual = lhsElem.qual.mergeCVQualifiers(rhsElem.qual);

    if (!adjustedElemType.eql(lhsElem, p.comp, true)) {
        lhs.ty = .{
            .data = .{ .subType = adjustedElemType },
            .specifier = .Pointer,
        };
        try lhs.implicitCast(p, .Bitcast, token);
    }

    if (!adjustedElemType.eql(rhsElem, p.comp, true)) {
        rhs.ty = .{
            .data = .{ .subType = adjustedElemType },
            .specifier = .Pointer,
        };
        try rhs.implicitCast(p, .Bitcast, token);
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

    try lhs.lvalConversion(p, token);
    try rhs.lvalConversion(p, token);

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
            try scalarOperand.implicitCast(p, .VectorSplat, token);
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
                    try nullPtrRes.nullCast(p, otherRes.ty, token);
                    return otherRes.shouldEval(nullPtrRes, p);
                } else if (otherRes.value.isZero(p.comp)) {
                    otherRes.value = Value.null;
                    try otherRes.nullCast(p, nullPtrRes.ty, token);
                    return otherRes.shouldEval(nullPtrRes, p);
                }
                return lhs.invalidBinTy(token, rhs, p);
            }

            // comparisons between floats and pointes not allowed
            if (!lhsIsScalar or !rhsIsScalar or (lhsIsFloat and rhsIsPtr) or (rhsIsFloat and lhsIsPtr))
                return lhs.invalidBinTy(token, rhs, p);

            if ((lhsIsInt or rhsIsInt) and !(lhs.value.isZero(p.comp) or rhs.value.isZero(p.comp))) {
                try p.errStr(.comparison_ptr_int, token, try p.typePairStr(lhs.ty, rhs.ty));
            } else if (lhsIsPtr and rhsIsPtr) {
                if (!lhs.ty.isVoidStar() and !rhs.ty.isVoidStar() and !lhs.ty.eql(rhs.ty, p.comp, false))
                    try p.errStr(.comparison_distinct_ptr, token, try p.typePairStr(lhs.ty, rhs.ty));
            } else if (lhsIsPtr) {
                try rhs.ptrCast(p, lhs.ty, token);
            } else {
                assert(rhsIsPtr);
                try lhs.ptrCast(p, rhs.ty, token);
            }

            return lhs.shouldEval(rhs, p);
        },

        .conditional => {
            // doesn't matter what we return here, as the result is ignored
            if (lhs.ty.is(.Void) or rhs.ty.is(.Void)) {
                try lhs.toVoid(p, token);
                try rhs.toVoid(p, token);
                return true;
            }

            if (lhsIsNullptr and rhsIsNullptr)
                return true;

            if ((lhsIsPtr and rhsIsInt) or (lhsIsInt and rhsIsPtr)) {
                if (lhs.value.isZero(p.comp) or rhs.value.isZero(p.comp)) {
                    try lhs.nullCast(p, rhs.ty, token);
                    try rhs.nullCast(p, lhs.ty, token);
                    return true;
                }

                const intType = if (lhsIsInt) lhs else rhs;
                const ptrType = if (lhsIsPtr) lhs else rhs;

                try p.errStr(.implicit_int_to_ptr, token, try p.typePairStrExtra(intType.ty, " to ", ptrType.ty));
                try intType.ptrCast(p, ptrType.ty, token);
                return true;
            }

            if (lhsIsPtr and rhsIsPtr)
                return lhs.adjustCondExprPtrs(token, rhs, p);

            if ((lhsIsPtr and rhsIsNullptr) or (lhsIsNullptr and rhsIsPtr)) {
                const nullPtrRes = if (lhsIsNullptr) lhs else rhs;
                const ptrRes = if (lhsIsNullptr) rhs else lhs;
                try nullPtrRes.nullCast(p, ptrRes.ty, token);
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
pub fn lvalConversion(res: *Result, p: *Parser, token: TokenIndex) Error!void {
    // Convert a function type to a pointer to the function.
    if (res.ty.isFunc()) {
        const elemType = try p.arena.create(Type);
        elemType.* = res.ty;
        res.ty.specifier = .Pointer;
        res.ty.data = .{ .subType = elemType };
        try res.implicitCast(p, .FunctionToPointer, token);
    }
    // Decay an array type to a pointer to its first element.
    else if (res.ty.isArray()) {
        res.value = .{};
        res.ty.decayArray();
        try res.implicitCast(p, .ArrayToPointer, token);
    }
    // Perform l-value to r-value conversion
    else if (!p.inMacro and p.tree.isLValue(res.node)) {
        res.ty.qual = .{};
        try res.implicitCast(p, .LValToRVal, token);
    }
}

pub fn boolCast(res: *Result, p: *Parser, boolType: Type, tok: TokenIndex) Error!void {
    if (res.ty.isArray()) {
        if (res.value.is(.bytes, p.comp))
            try p.errStr(.string_literal_to_bool, tok, try p.typePairStrExtra(res.ty, " to ", boolType))
        else
            try p.errStr(.array_address_to_bool, tok, p.getTokenText(tok));

        try res.lvalConversion(p, tok);
        res.value = Value.one;
        res.ty = boolType;
        try res.implicitCast(p, .PointerToBool, tok);
    } else if (res.ty.isPointer()) {
        res.value.boolCast(p.comp);
        res.ty = boolType;
        try res.implicitCast(p, .PointerToBool, tok);
    } else if (res.ty.isInt() and !res.ty.is(.Bool)) {
        res.value.boolCast(p.comp);
        res.ty = boolType;
        try res.implicitCast(p, .IntToBool, tok);
    } else if (res.ty.isFloat()) {
        const oldValue = res.value;
        const valueChangeKind = try res.value.floatToInt(boolType, p.comp);
        try res.floatToIntWarning(p, boolType, oldValue, valueChangeKind, tok);
        if (!res.ty.isReal()) {
            res.ty = res.ty.makeReal();
            try res.implicitCast(p, .ComplexFloatToReal, tok);
        }
        res.ty = boolType;
        try res.implicitCast(p, .FloatToBool, tok);
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
pub fn intCast(res: *Result, p: *Parser, intType: Type, token: TokenIndex) Error!void {
    if (intType.hasIncompleteSize())
        return error.ParsingFailed;

    // Cast from boolean to integer.
    if (res.ty.is(.Bool)) {
        res.ty = intType.makeReal();
        try res.implicitCast(p, .BoolToInt, token);
        if (!intType.isReal()) {
            res.ty = intType;
            try res.implicitCast(p, .RealToComplexInt, token);
        }
    }

    // Cast from pointer to integer.
    else if (res.ty.isPointer()) {
        res.value = .{};
        res.ty = intType.makeReal();
        try res.implicitCast(p, .PointerToInt, token);
        if (!intType.isReal()) {
            res.ty = intType;
            try res.implicitCast(p, .RealToComplexInt, token);
        }
    }

    // Cast from floating point to integer.
    else if (res.ty.isFloat()) {
        const oldValue = res.value;
        const valueChangeKind = try res.value.floatToInt(intType, p.comp);
        try res.floatToIntWarning(p, intType, oldValue, valueChangeKind, token);

        const oldReal = res.ty.isReal();
        const newReal = intType.isReal();
        if (oldReal and newReal) {
            res.ty = intType;
            try res.implicitCast(p, .FloatToInt, token);
        } else if (oldReal) {
            res.ty = intType.makeReal();
            try res.implicitCast(p, .FloatToInt, token);
            res.ty = intType;
            try res.implicitCast(p, .RealToComplexInt, token);
        } else if (newReal) {
            res.ty = res.ty.makeReal();
            try res.implicitCast(p, .ComplexFloatToReal, token);
            res.ty = intType;
            try res.implicitCast(p, .FloatToInt, token);
        } else {
            res.ty = intType;
            try res.implicitCast(p, .ComplexFloatToComplexInt, token);
        }
    }

    // Cast between integer types.
    else if (!res.ty.eql(intType, p.comp, true)) {
        try res.value.intCast(intType, p.comp);
        const oldReal = res.ty.isReal();
        const newReal = intType.isReal();
        if (oldReal and newReal) {
            res.ty = intType;
            try res.implicitCast(p, .IntCast, token);
        } else if (oldReal) {
            const realIntTy = intType.makeReal();
            if (!res.ty.eql(realIntTy, p.comp, false)) {
                res.ty = realIntTy;
                try res.implicitCast(p, .IntCast, token);
            }
            res.ty = intType;
            try res.implicitCast(p, .RealToComplexInt, token);
        } else if (newReal) {
            res.ty = res.ty.makeReal();
            try res.implicitCast(p, .ComplexIntToReal, token);
            res.ty = intType;
            try res.implicitCast(p, .IntCast, token);
        } else {
            res.ty = intType;
            try res.implicitCast(p, .ComplexIntCast, token);
        }
    }
}

fn floatToIntWarning(
    res: *Result,
    p: *Parser,
    intTy: Type,
    oldValue: Value,
    changeKind: Value.FloatToIntChangeKind,
    tok: TokenIndex,
) !void {
    switch (changeKind) {
        .none => return p.errStr(.float_to_int, tok, try p.typePairStrExtra(res.ty, " to ", intTy)),
        .outOfRange => return p.errStr(.float_out_of_range, tok, try p.typePairStrExtra(res.ty, " to ", intTy)),
        .overflow => return p.errStr(.float_overflow_conversion, tok, try p.typePairStrExtra(res.ty, " to ", intTy)),
        .nonZeroToZero => return p.errStr(.float_zero_conversion, tok, try p.floatValueChangedStr(res, oldValue, intTy)),
        .valueChanged => return p.errStr(.float_value_changed, tok, try p.floatValueChangedStr(res, oldValue, intTy)),
    }
}

pub fn floatCast(res: *Result, p: *Parser, floatType: Type, token: TokenIndex) Error!void {
    if (res.ty.is(.Bool)) {
        try res.value.intToFloat(floatType, p.comp);
        res.ty = floatType.makeReal();
        try res.implicitCast(p, .BoolToFloat, token);
        if (!floatType.isReal()) {
            res.ty = floatType;
            try res.implicitCast(p, .RealToComplexFloat, token);
        }
    }
    // src type is int type
    else if (res.ty.isInt()) {
        try res.value.intToFloat(floatType, p.comp);
        const oldReal = res.ty.isReal();
        const newReal = floatType.isReal();
        if (oldReal and newReal) {
            res.ty = floatType;
            try res.implicitCast(p, .IntToFloat, token);
        } else if (oldReal) {
            res.ty = floatType.makeReal();
            try res.implicitCast(p, .IntToFloat, token);
            res.ty = floatType;
            try res.implicitCast(p, .RealToComplexFloat, token);
        } else if (newReal) {
            res.ty = res.ty.makeReal();
            try res.implicitCast(p, .ComplexIntToReal, token);
            res.ty = floatType;
            try res.implicitCast(p, .IntToFloat, token);
        } else {
            res.ty = floatType;
            try res.implicitCast(p, .ComplexIntToComplexFloat, token);
        }
    }
    // src type is not equal float type
    else if (!res.ty.eql(floatType, p.comp, true)) {
        try res.value.floatCast(floatType, p.comp);
        const oldReal = res.ty.isReal();
        const newReal = floatType.isReal();
        if (oldReal and newReal) {
            res.ty = floatType;
            try res.implicitCast(p, .FloatCast, token);
        } else if (oldReal) {
            if (res.ty.floatRank() != floatType.floatRank()) {
                res.ty = floatType.makeReal();
                try res.implicitCast(p, .FloatCast, token);
            }
            res.ty = floatType;
            try res.implicitCast(p, .RealToComplexFloat, token);
        } else if (newReal) {
            res.ty = res.ty.makeReal();
            try res.implicitCast(p, .ComplexFloatToReal, token);
            if (res.ty.floatRank() != floatType.floatRank()) {
                res.ty = floatType;
                try res.implicitCast(p, .FloatCast, token);
            }
        } else {
            res.ty = floatType;
            try res.implicitCast(p, .ComplexFloatCast, token);
        }
    }
}

/// converts a bool or integer to a pointer
pub fn ptrCast(res: *Result, p: *Parser, ptrType: Type, token: TokenIndex) Error!void {
    if (res.ty.is(.Bool)) {
        res.ty = ptrType;
        try res.implicitCast(p, .BoolToPointer, token);
    } else if (res.ty.isInt()) {
        try res.value.intCast(ptrType, p.comp);
        res.ty = ptrType;
        try res.implicitCast(p, .IntToPointer, token);
    }
}

/// converts pointer to one with a different child type
fn ptrChildTypeCast(res: *Result, p: *Parser, ptrTy: Type, token: TokenIndex) Error!void {
    res.ty = ptrTy;
    return res.implicitCast(p, .Bitcast, token);
}

pub fn toVoid(res: *Result, p: *Parser, token: TokenIndex) Error!void {
    if (!res.ty.is(.Void)) {
        res.ty = Type.Void;
        try res.implicitCast(p, .ToVoid, token);
    }
}

pub fn nullCast(res: *Result, p: *Parser, ptrType: Type, token: TokenIndex) Error!void {
    if (!res.value.isZero(p.comp) and !res.ty.is(.NullPtrTy)) return;
    res.ty = ptrType;
    try res.implicitCast(p, .NullToPointer, token);
}

/// Attempts to perform a cast on the result's value using the usual conversion rules.
/// The usual conversion rules are as follows:
///   - If the result's type is an integer, then perform an integer promotion on the result's type.
///
/// Arguments:
///   - res: The result that should be cast.
///   - p: The parser that this result is being parsed with.
///   - token: The token index of the token that this result is being cast to.
///
/// Returns:
///   - The cast value if the cast was successful. Otherwise, an error.
pub fn usualUnaryConversion(res: *Result, p: *Parser, token: TokenIndex) Error!void {
    if (res.ty.is(.FP16) and !p.comp.langOpts.useNativeHalfType) {
        return res.floatCast(p, Type.Float, token);
    }

    if (res.ty.isInt() and !p.inMacro) {
        if (p.tree.bitfieldWidth(res.node, true)) |width| {
            if (res.ty.bitfieldPromotion(p.comp, width)) |promotedTy|
                return res.intCast(p, promotedTy, token);
        }
        return res.intCast(p, res.ty.integerPromotion(p.comp), token);
    }
}

fn usualArithmeticConversion(lhs: *Result, rhs: *Result, p: *Parser, token: TokenIndex) Error!void {
    try lhs.usualUnaryConversion(p, token);
    try rhs.usualUnaryConversion(p, token);

    // if either is a float cast to that type
    if (lhs.ty.isFloat() or rhs.ty.isFloat()) {
        const floatTypes = [6][2]Type.Specifier{
            .{ .ComplexLongDouble, .LongDouble },
            .{ .ComplexFloat128, .Float128 },
            .{ .ComplexDouble, .Double },
            .{ .ComplexFloat, .Float },
            // No `_Complex __fp16` type
            .{ .Invalid, .FP16 },
            .{ .ComplexFloat16, .Float16 },
        };

        const lhsSpec = lhs.ty.canonicalize(.standard).specifier;
        const rhsSpec = rhs.ty.canonicalize(.standard).specifier;

        for (floatTypes) |ft| {
            if (try lhs.floatConversion(rhs, lhsSpec, rhsSpec, p, ft, token)) return;
        }
        unreachable;
    }

    if (lhs.ty.eql(rhs.ty, p.comp, true)) {
        // cast to promoted type
        try lhs.intCast(p, lhs.ty, token);
        try rhs.intCast(p, rhs.ty, token);
        return;
    }

    const targetTy = lhs.ty.integerConversion(rhs.ty, p.comp);
    if (!targetTy.isReal()) {
        try lhs.saveValue(p);
        try rhs.saveValue(p);
    }
    try lhs.intCast(p, targetTy, token);
    try rhs.intCast(p, targetTy, token);
}

fn floatConversion(
    lhs: *Result,
    rhs: *Result,
    lhsSpec: Type.Specifier,
    rhsSpec: Type.Specifier,
    p: *Parser,
    pair: [2]Type.Specifier,
    token: TokenIndex,
) !bool {
    if (lhsSpec == pair[0] or lhsSpec == pair[1] or
        rhsSpec == pair[0] or rhsSpec == pair[1])
    {
        const bothReal = lhs.ty.isReal() and rhs.ty.isReal();
        const resSpec = pair[@intFromBool(bothReal)];
        const ty = Type{ .specifier = resSpec };
        try lhs.floatCast(p, ty, token);
        try rhs.floatCast(p, ty, token);
        return true;
    }
    return false;
}

fn invalidBinTy(lhs: *Result, tok: TokenIndex, rhs: *Result, p: *Parser) Error!bool {
    try p.errStr(.invalid_bin_types, tok, try p.typePairStr(lhs.ty, rhs.ty));
    lhs.value = .{};
    rhs.value = .{};
    lhs.ty = Type.Invalid;
    return false;
}

/// Return true if the result of the expression should be evaluated.
fn shouldEval(lhs: *Result, rhs: *Result, p: *Parser) Error!bool {
    if (p.noEval) return false;
    if (!lhs.value.isNone() and !rhs.value.isNone())
        return true;

    try lhs.saveValue(p);
    try rhs.saveValue(p);
    return p.noEval;
}

/// Saves value and replaces it with `.unavailable`.
pub fn saveValue(res: *Result, p: *Parser) !void {
    assert(!p.inMacro);
    if (res.value.isNone() or res.value.isNull())
        return;

    if (!p.inMacro)
        try p.tree.valueMap.put(p.gpa, res.node, res.value);

    res.value = .{};
}
/// Saves value without altering the result.
pub fn putValue(res: *const Result, p: *Parser) !void {
    if (res.value.isNone() or res.value.isNull()) return;
    if (!p.inMacro) try p.tree.valueMap.put(p.gpa, res.node, res.value);
}

pub fn castType(res: *Result, p: *Parser, to: Type, operandToken: TokenIndex, lparen: TokenIndex) !void {
    var explicitCastKind: Node.Cast.Kind = undefined;
    if (to.is(.Void)) {
        // everything can cast to void
        explicitCastKind = .ToVoid;
        res.value = .{};
    } else if (to.is(.NullPtrTy)) {
        if (res.ty.is(.NullPtrTy)) {
            explicitCastKind = .NoOP;
        } else {
            try p.errStr(.invalid_object_cast, lparen, try p.typePairStrExtra(res.ty, " to ", to));
            return error.ParsingFailed;
        }
    } else if (res.ty.is(.NullPtrTy)) {
        if (to.is(.Bool)) {
            try res.nullCast(p, res.ty, lparen);
            res.value.boolCast(p.comp);
            res.ty = Type.Bool;
            try res.implicitCast(p, .PointerToBool, lparen);
            try res.saveValue(p);
        } else if (to.isPointer()) {
            try res.nullCast(p, to, lparen);
        } else {
            try p.errStr(.invalid_object_cast, lparen, try p.typePairStrExtra(res.ty, " to ", to));
            return error.ParsingFailed;
        }
        explicitCastKind = .NoOP;
    } else if (res.value.isZero(p.comp) and to.isPointer()) {
        explicitCastKind = .NullToPointer;
    } else if (to.isScalar()) cast: {
        const oldIsFloat = res.ty.isFloat();
        const newIsFloat = to.isFloat();

        if (newIsFloat and res.ty.isPointer()) {
            try p.errStr(.invalid_cast_to_float, lparen, try p.typeStr(to));
            return error.ParsingFailed;
        } else if (oldIsFloat and to.isPointer()) {
            try p.errStr(.invalid_cast_to_pointer, lparen, try p.typeStr(res.ty));
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
                    try res.implicitCast(
                        p,
                        if (oldIsInt) .ComplexIntToReal else .ComplexFloatToReal,
                        lparen,
                    );
                }
                explicitCastKind = if (oldIsInt) .IntToBool else .FloatToBool;
            }
        } else if (to.isInt()) {
            if (res.ty.is(.Bool)) {
                if (newIsComplex) {
                    res.ty = to.makeReal();
                    try res.implicitCast(p, .BoolToInt, lparen);
                }
                explicitCastKind = if (newIsReal) .BoolToInt else .RealToComplexInt;
            } else if (res.ty.isInt()) {
                const needIntToRealCast = oldIsComplex and newIsReal;
                const needRealToComplexIntCast = oldIsReal and newIsComplex;

                if (needIntToRealCast) {
                    res.ty = res.ty.makeReal();
                    try res.implicitCast(p, .ComplexIntToReal, lparen);
                } else if (needRealToComplexIntCast) {
                    res.ty = to.makeReal();
                    try res.implicitCast(p, .IntCast, lparen);
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
                    try res.implicitCast(p, .PointerToInt, lparen);
                    explicitCastKind = .RealToComplexInt;
                } else {
                    explicitCastKind = .PointerToInt;
                    res.value = .{};
                }
            } else if (oldIsReal and newIsReal) {
                explicitCastKind = .FloatToInt;
            } else if (oldIsReal) {
                res.ty = to.makeReal();
                try res.implicitCast(p, .FloatToInt, lparen);
                explicitCastKind = .RealToComplexInt;
            } else if (newIsReal) {
                res.ty = res.ty.makeReal();
                try res.implicitCast(p, .ComplexFloatToReal, lparen);
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
                    try res.implicitCast(p, .ComplexIntToReal, lparen);
                }
                explicitCastKind = .IntToPointer;
            } else {
                try p.errStr(.cond_expr_type, operandToken, try p.typeStr(res.ty));
                return error.ParsingFailed;
            }
        } else if (newIsFloat) {
            if (res.ty.is(.Bool)) {
                if (newIsComplex) {
                    res.ty = to.makeReal();
                    try res.implicitCast(p, .BoolToFloat, lparen);
                    explicitCastKind = .RealToComplexFloat;
                } else {
                    explicitCastKind = .BoolToFloat;
                }
            } else if (res.ty.isInt()) {
                if (oldIsReal and newIsReal) {
                    explicitCastKind = .IntToFloat;
                } else if (oldIsReal) {
                    res.ty = to.makeReal();
                    try res.implicitCast(p, .IntToFloat, lparen);
                    explicitCastKind = .RealToComplexFloat;
                } else if (newIsReal) {
                    res.ty = res.ty.makeReal();
                    try res.implicitCast(p, .ComplexIntToReal, lparen);
                    explicitCastKind = .IntToFloat;
                } else {
                    explicitCastKind = .ComplexIntToComplexFloat;
                }
            } else if (oldIsReal and newIsReal) {
                explicitCastKind = .FloatCast;
            } else if (oldIsReal) {
                res.ty = to.makeReal();
                try res.implicitCast(p, .FloatCast, lparen);
                explicitCastKind = .RealToComplexFloat;
            } else if (newIsReal) {
                res.ty = res.ty.makeReal();
                try res.implicitCast(p, .ComplexFloatToReal, lparen);
                explicitCastKind = .FloatCast;
            } else {
                explicitCastKind = .ComplexFloatCast;
            }
        }

        if (res.value.isNone()) break :cast;

        const oldInt = res.ty.isInt() or res.ty.isPointer();
        const newInt = to.isInt() or to.isPointer();
        if (to.is(.Bool)) {
            res.value.boolCast(p.comp);
        } else if (oldIsFloat and newInt) {
            // Explicit cast, no conversion warning
            _ = try res.value.floatToInt(to, p.comp);
        } else if (newIsFloat and oldInt) {
            try res.value.intToFloat(to, p.comp);
        } else if (newIsFloat and oldIsFloat) {
            try res.value.floatCast(to, p.comp);
        } else if (oldInt and newInt) {
            if (to.hasIncompleteSize()) {
                try p.errStr(.cast_to_incomplete_type, lparen, try p.typeStr(to));
                return error.ParsingFailed;
            }
            try res.value.intCast(to, p.comp);
        }
    } else if (to.get(.Union)) |unionTy| {
        if (unionTy.data.record.hasFieldOfType(res.ty, p.comp)) {
            explicitCastKind = .UnionCast;
            try p.errToken(.gnu_union_cast, lparen);
        } else {
            if (unionTy.data.record.isIncomplete())
                try p.errStr(.cast_to_incomplete_type, lparen, try p.typeStr(to))
            else
                try p.errStr(.invalid_union_cast, lparen, try p.typeStr(res.ty));
            return error.ParsingFailed;
        }
    } else {
        if (to.is(.AutoType))
            try p.errToken(.invalid_cast_to_auto_type, lparen)
        else
            try p.errStr(.invalid_cast_type, lparen, try p.typeStr(to));
        return error.ParsingFailed;
    }

    if (to.containAnyQual())
        try p.errStr(.qual_cast, lparen, try p.typeStr(to));
    if (to.isInt() and res.ty.isPointer() and to.sizeCompare(res.ty, p.comp) == .lt)
        try p.errStr(.cast_to_smaller_int, lparen, try p.typePairStrExtra(to, " from ", res.ty));

    res.ty = to;
    res.ty.qual = .{};
    res.node = try p.addNode(.{
        .cast = .{
            .lparen = lparen,
            .type = res.ty,
            .operand = res.node,
            .kind = explicitCastKind,
            .implicit = false,
        },
    });
}

/// Check if the integer value represented by `res` fits within the type bounds of `ty`.
/// This function compares the `res` value against the maximum and minimum values that
/// can be represented by the type `ty`.
/// @param res   The result object containing the value to be checked.
/// @param p     A pointer to the Parser object.
/// @param ty    The type within which the value should fit.
/// @return      Returns true if the value fits within the type bounds, false otherwise.
pub fn intFitsInType(res: Result, p: *Parser, ty: Type) !bool {
    const maxInt = try Value.int(ty.maxInt(p.comp), p.comp);
    const minInt = try Value.int(ty.minInt(p.comp), p.comp);

    return res.value.compare(.lte, maxInt, p.comp) and
        (res.ty.isUnsignedInt(p.comp) or res.value.compare(.gte, minInt, p.comp));
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
            try res.floatCast(p, unqualTy, tok);
            return;
        }
    }
    // dest type is pointer
    else if (unqualTy.isPointer()) {
        if (res.value.isZero(p.comp) or res.ty.is(.NullPtrTy)) {
            try res.nullCast(p, destTy, tok);
            return;
        } else if (res.ty.isInt() and res.ty.isReal()) {
            if (ctx == .testCoerce)
                return error.CoercionFailed;
            try p.errStr(.implicit_int_to_ptr, tok, try p.typePairStrExtra(res.ty, " to ", destTy));
            try ctx.note(p);
            try res.ptrCast(p, unqualTy, tok);
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
            try res.ptrCast(p, unqualTy, tok);
            return;
        } else if (res.ty.isPointer()) {
            const differentSignOnly = unqualTy.getElemType().sameRankDifferentSign(res.ty.getElemType(), p.comp);
            try p.errStr(switch (ctx) {
                .assign => ([2]Diagnostics.Tag{ .incompatible_ptr_assign, .incompatible_ptr_assign_sign })[@intFromBool(differentSignOnly)],
                .init => ([2]Diagnostics.Tag{ .incompatible_ptr_init, .incompatible_ptr_init_sign })[@intFromBool(differentSignOnly)],
                .ret => ([2]Diagnostics.Tag{ .incompatible_return, .incompatible_return_sign })[@intFromBool(differentSignOnly)],
                .arg => ([2]Diagnostics.Tag{ .incompatible_ptr_arg, .incompatible_ptr_arg_sign })[@intFromBool(differentSignOnly)],
                .testCoerce => return error.CoercionFailed,
            }, tok, try ctx.typePairStr(p, destTy, res.ty));
            try ctx.note(p);
            try res.ptrChildTypeCast(p, unqualTy, tok);
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
                        .unionInitExpr = .{
                            .fieldIndex = 0,
                            .initializer = res.node,
                            .lbraceToken = tok,
                            .unionType = destTy,
                        },
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
