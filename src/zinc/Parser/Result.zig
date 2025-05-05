const std = @import("std");
const assert = std.debug.assert;

const AST = @import("../AST/AST.zig");
const Diagnostics = @import("../Basic/Diagnostics.zig");
const Value = @import("../AST/Value.zig");
const Parser = @import("Parser.zig");
const TypeStore = @import("../AST/TypeStore.zig");
const QualType = TypeStore.QualType;

const Node = AST.Node;
const TokenIndex = AST.TokenIndex;
const Error = Parser.Error;

const Result = @This();

node: Node.Index,
qt: QualType = .int,
value: Value = .{},

pub fn str(res: Result, p: *Parser) ![]const u8 {
    switch (res.value.optRef) {
        .none => return "(none)",
        .null => return "nullptr_t",
        else => {},
    }

    const stringsTop = p.strings.items.len;
    defer p.strings.items.len = stringsTop;

    try res.value.print(res.qt, p.comp, p.strings.writer());
    return try p.comp.diagnostics.arena.allocator().dupe(u8, p.strings.items[stringsTop..]);
}

pub fn maybeWarnUnused(res: Result, p: *Parser, exprStart: TokenIndex, errStart: usize) Error!void {
    if (res.qt.is(p.comp, .void))
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
    if (lhs.value.isNull()) lhs.value = .zero;
    if (!lhs.qt.isInvalid()) lhs.qt = .int;

    return lhs.bin(p, tag, rhs, token);
}

pub fn bin(lhs: *Result, p: *Parser, rtTag: std.meta.Tag(Node), rhs: Result, token: TokenIndex) !void {
    const binData: Node.Binary = .{
        .opToken = token,
        .lhs = lhs.node,
        .rhs = rhs.node,
        .qt = lhs.qt,
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
        .qt = operand.qt,
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
            .qt = operand.qt,
            .implicit = true,
        },
    });
}

pub fn adjustCondExprPtrs(lhs: *Result, token: TokenIndex, rhs: *Result, p: *Parser) !bool {
    assert(lhs.qt.isPointer() and rhs.qt.isPointer());

    const lhsElem = lhs.qt.childType(p.comp);
    const rhsElem = rhs.qt.childType(p.comp);
    if (lhsElem.eql(rhsElem, p.comp))
        return true;

    var adjustedElemType = lhsElem;

    const hasVoidStarBranch = lhs.qt.isVoidStar() or rhs.qt.isVoidStar();
    const onlyQualsDiffer = lhsElem.eql(rhsElem, p.comp);
    const pointersCompatible = onlyQualsDiffer or hasVoidStarBranch;

    if (!pointersCompatible or hasVoidStarBranch) {
        if (!pointersCompatible)
            try p.errStr(.pointer_mismatch, token, try p.typePairStrExtra(lhs.qt, " and ", rhs.qt));
        adjustedElemType = .void;
    }

    if (pointersCompatible) {
        adjustedElemType.@"const" = lhsElem.@"const" or rhsElem.@"const";
        adjustedElemType.@"volatile" = lhsElem.@"volatile" or rhsElem.@"volatile";
    }

    if (!adjustedElemType.eql(lhsElem, p.comp)) {
        lhs.qt = .{
            .data = .{ .subType = adjustedElemType },
            .specifier = .Pointer,
        };
        try lhs.implicitCast(p, .Bitcast, token);
    }

    if (!adjustedElemType.eql(rhsElem, p.comp)) {
        rhs.qt = .{
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
    if (rhs.qt.isInvalid()) {
        try lhs.saveValue(p);
        lhs.qt = .invalid;
    }

    if (lhs.qt.isInvalid())
        return false;

    try lhs.lvalConversion(p, token);
    try rhs.lvalConversion(p, token);

    const lhsIsVec = lhs.qt.is(p.comp, .vector);
    const rhsIsVec = rhs.qt.is(p.comp, .vector);
    if (lhsIsVec and rhsIsVec) {
        if (lhs.qt.eql(rhs.ty, p.comp))
            return lhs.shouldEval(rhs, p);
        return lhs.invalidBinTy(token, rhs, p);
    } else if (lhsIsVec or rhsIsVec) {
        const vecOperand = if (lhsIsVec) lhs else rhs;
        const scalarOperand = if (lhsIsVec) rhs else lhs;
        if (scalarOperand.coerceExtra(p, vecOperand.qt.childType(p.comp), token, .testCoerce)) {
            try scalarOperand.saveValue(p);
            try scalarOperand.implicitCast(p, .VectorSplat, token);
            return lhs.shouldEval(rhs, p);
        } else |err| switch (err) {
            error.CoercionFailed => return lhs.invalidBinTy(token, rhs, p),
            else => |e| return e,
        }
    }

    const lhsIsInt = lhs.qt.isInt();
    const rhsIsInt = rhs.qt.isInt();
    if (lhsIsInt and rhsIsInt) {
        try lhs.usualArithmeticConversion(rhs, p, token);
        return lhs.shouldEval(rhs, p);
    }

    if (kind == .integer)
        return lhs.invalidBinTy(token, rhs, p);

    const lhsIsFloat = lhs.qt.isFloat(p.comp);
    const rhsIsFloat = rhs.qt.isFloat(p.comp);
    const lhsIsArithmetic = lhsIsInt or lhsIsFloat;
    const rhsIsArithmetic = rhsIsInt or rhsIsFloat;
    if (lhsIsArithmetic and rhsIsArithmetic) {
        // <, <=, >, >= only work on real types
        if (kind == .relational and (!lhs.qt.isReal() or !rhs.qt.isReal()))
            return lhs.invalidBinTy(token, rhs, p);

        try lhs.usualArithmeticConversion(rhs, p, token);
        return lhs.shouldEval(rhs, p);
    }

    if (kind == .arithmetic)
        return lhs.invalidBinTy(token, rhs, p);

    const lhsIsNullptr = lhs.qt.is(.NullPtrTy);
    const rhsIsNullptr = rhs.qt.is(.NullPtrTy);
    const lhsIsPtr = lhs.qt.isPointer();
    const rhsIsPtr = rhs.qt.isPointer();
    const lhsIsScalar = lhsIsArithmetic or lhsIsPtr;
    const rhsIsScalar = rhsIsArithmetic or rhsIsPtr;
    switch (kind) {
        .booleanLogic => {
            if (!(lhsIsScalar or lhsIsNullptr) or !(rhsIsScalar or rhsIsNullptr))
                return lhs.invalidBinTy(token, rhs, p);

            // Do integer promotions but nothing else
            if (lhsIsInt) try lhs.castToInt(p, lhs.qt.integerPromotion(p.comp), token);
            if (rhsIsInt) try rhs.castToInt(p, rhs.qt.integerPromotion(p.comp), token);
            return lhs.shouldEval(rhs, p);
        },

        .relational, .equality => {
            if (kind == .equality and (lhsIsNullptr or rhsIsNullptr)) {
                if (lhsIsNullptr and rhsIsNullptr)
                    return lhs.shouldEval(rhs, p);

                const nullPtrRes = if (lhsIsNullptr) lhs else rhs;
                const otherRes = if (lhsIsNullptr) rhs else lhs;
                if (otherRes.qt.isPointer()) {
                    try nullPtrRes.nullCast(p, otherRes.qt, token);
                    return otherRes.shouldEval(nullPtrRes, p);
                } else if (otherRes.value.isZero(p.comp)) {
                    otherRes.value = .null;
                    try otherRes.nullToPointer(p, nullPtrRes.qt, token);
                    return otherRes.shouldEval(nullPtrRes, p);
                }
                return lhs.invalidBinTy(token, rhs, p);
            }

            // comparisons between floats and pointes not allowed
            if (!lhsIsScalar or !rhsIsScalar or (lhsIsFloat and rhsIsPtr) or (rhsIsFloat and lhsIsPtr))
                return lhs.invalidBinTy(token, rhs, p);

            if ((lhsIsInt or rhsIsInt) and !(lhs.value.isZero(p.comp) or rhs.value.isZero(p.comp))) {
                try p.errStr(.comparison_ptr_int, token, try p.typePairStr(lhs.qt, rhs.qt));
            } else if (lhsIsPtr and rhsIsPtr) {
                if (!lhs.qt.isVoidStar() and !rhs.qt.isVoidStar() and !lhs.qt.eql(rhs.qt, p.comp))
                    try p.errStr(.comparison_distinct_ptr, token, try p.typePairStr(lhs.qt, rhs.qt));
            } else if (lhsIsPtr) {
                try rhs.castToPointer(p, lhs.qt, token);
            } else {
                assert(rhsIsPtr);
                try lhs.castToPointer(p, rhs.qt, token);
            }

            return lhs.shouldEval(rhs, p);
        },

        .conditional => {
            // doesn't matter what we return here, as the result is ignored
            if (lhs.qt.is(.Void) or rhs.qt.is(.Void)) {
                try lhs.castToVoid(p, token);
                try rhs.castToVoid(p, token);
                return true;
            }

            if (lhsIsNullptr and rhsIsNullptr)
                return true;

            if ((lhsIsPtr and rhsIsInt) or (lhsIsInt and rhsIsPtr)) {
                if (lhs.value.isZero(p.comp) or rhs.value.isZero(p.comp)) {
                    try lhs.nullToPointer(p, rhs.qt, token);
                    try rhs.nullToPointer(p, lhs.qt, token);
                    return true;
                }

                const intType = if (lhsIsInt) lhs else rhs;
                const ptrType = if (lhsIsPtr) lhs else rhs;

                try p.errStr(.implicit_int_to_ptr, token, try p.typePairStrExtra(intType.qt, " to ", ptrType.qt));
                try intType.castToPointer(p, ptrType.qt, token);
                return true;
            }

            if (lhsIsPtr and rhsIsPtr)
                return lhs.adjustCondExprPtrs(token, rhs, p);

            if ((lhsIsPtr and rhsIsNullptr) or (lhsIsNullptr and rhsIsPtr)) {
                const nullPtrRes = if (lhsIsNullptr) lhs else rhs;
                const ptrRes = if (lhsIsNullptr) rhs else lhs;
                try intType.nullToPointer(p, ptrRes.ty, token);
                return true;
            }

            if (lhs.qt.isRecord() and rhs.qt.isRecord() and lhs.qt.eql(rhs.qt, p.comp, false))
                return true;

            return lhs.invalidBinTy(token, rhs, p);
        },

        .add => {
            // if both aren't arithmetic one should be pointer and the other an integer
            if (lhsIsPtr == rhsIsPtr or lhsIsInt == rhsIsInt)
                return lhs.invalidBinTy(token, rhs, p);

            // Do integer promotions but nothing else
            if (lhsIsInt) try lhs.castToInt(p, lhs.qt.integerPromotion(p.comp), token);
            if (rhsIsInt) try rhs.castToInt(p, rhs.qt.integerPromotion(p.comp), token);

            // The result type is the type of the pointer operand
            if (lhsIsInt) lhs.qt = rhs.qt else rhs.qt = lhs.qt;
            return lhs.shouldEval(rhs, p);
        },

        .sub => {
            // if both aren't arithmetic then either both should be pointers or just a
            if (!lhsIsPtr or !(rhsIsPtr or rhsIsInt)) return lhs.invalidBinTy(token, rhs, p);

            if (lhsIsPtr and rhsIsPtr) {
                if (!lhs.qt.eql(rhs.ty, p.comp))
                    try p.errStr(.incompatible_pointers, token, try p.typePairStr(lhs.qt, rhs.qt));
                lhs.qt = p.comp.typeStore.ptrdiff;
            }

            // Do integer promotion on b if needed
            if (rhsIsInt) try rhs.castToInt(p, rhs.qt.integerPromotion(p.comp), token);
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
    if (res.qt.is(p.comp, .func)) {
        res.qt = try res.qt.decay(p.comp);
        try res.implicitCast(p, .FunctionToPointer, token);
    }
    // Decay an array type to a pointer to its first element.
    else if (res.ty.isArray()) {
        res.value = .{};
        res.qt = try res.qt.decay(p.comp);
        try res.implicitCast(p, .ArrayToPointer, token);
    }
    // Perform l-value to r-value conversion
    else if (!p.inMacro and p.tree.isLValue(res.node)) {
        res.qt = res.qt.unqualified();
        try res.implicitCast(p, .LValToRVal, token);
    }
}

pub fn castToBool(res: *Result, p: *Parser, boolType: QualType, tok: TokenIndex) Error!void {
    const srcSK = res.qt.scalarKind(p.comp);
    if (srcSK.isPointer()) {
        res.value.boolCast(p.comp);
        res.qt = boolType;
        try res.implicitCast(p, .PointerToBool, tok);
    } else if (srcSK.isInt() and srcSK != .Bool) {
        res.value.boolCast(p.comp);
        if (!srcSK.isReal()) {
            res.qt = res.qt.toReal(p.comp);
            try res.implicitCast(p, .ComplexFloatToReal, tok);
        }
        res.qt = boolType;
        try res.implicitCast(p, .IntToBool, tok);
    } else if (srcSK.isFloat()) {
        const oldValue = res.value;
        const valueChangeKind = try res.value.floatToInt(boolType, p.comp);
        try res.floatToIntWarning(p, boolType, oldValue, valueChangeKind, tok);
        if (!srcSK.isReal()) {
            res.qt = res.qt.toReal(p.comp);
            try res.implicitCast(p, .ComplexFloatToReal, tok);
        }
        res.qt = boolType;
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
pub fn castToInt(res: *Result, p: *Parser, intQt: QualType, token: TokenIndex) Error!void {
    const srcSK = res.qt.scalarKind(p.comp);
    const destSK = intQt.scalarKind(p.comp);

    if (srcSK == .Bool) {
        try res.implicitCast(p, .BoolToInt, token);
        if (!destSK.isReal()) {
            res.qt = intQt;
            try res.implicitCast(p, .RealToComplexInt, token);
        }
    }
    // Cast from floating point to integer.
    else if (res.qt.isFloat(p.comp)) {
        const oldValue = res.val;
        const valueChangeKind = try res.value.floatToInt(intQt, p.comp);

        try res.floatToIntWarning(p, intQt, oldValue, valueChangeKind, token);

        if (srcSK.isReal() and destSK.isReal()) {
            res.qt = intQt;
            try res.implicitCast(p, .FloatToInt, token);
        } else if (srcSK.isReal()) {
            res.qt = intQt.toReal(p.comp);
            try res.implicitCast(p, .FloatToInt, token);
            res.qt = intQt;
            try res.implicitCast(p, .RealToComplexInt, token);
        } else if (destSK.isReal()) {
            res.qt = res.qt.toReal(p.comp);
            try res.implicitCast(p, .ComplexFloatToReal, token);
            res.qt = intQt;
            try res.implicitCast(p, .FloatToInt, token);
        } else {
            res.qt = intQt;
            try res.implicitCast(p, .ComplexFloatToComplexInt, token);
        }
    } else if (!res.qt.eql(intQt, p.comp)) {
        const oldValue = res.value;
        const valueChangeKind = try res.value.intCast(intQt, p.comp);
        switch (valueChangeKind) {
            .none => {},
            .truncated => try p.errStr(.int_value_changed, token, try p.floatValueChangedStr(res, oldValue, intQt)),
            .si => try p.errStr(.sign_conversion, token, try p.typePairStrExtra(res.qt, " to ", intQt)),
        }

        if (srcSK.isReal() and destSK.isReal()) {
            res.qt = intQt;
            try res.implicitCast(p, .int_cast, token);
        } else if (srcSK.isReal()) {
            const realIntQt = intQt.toReal(p.comp);
            if (!res.qt.eql(realIntQt, p.comp)) {
                res.qt = realIntQt;
                try res.implicitCast(p, .int_cast, token);
            }
            res.qt = intQt;
            try res.implicitCast(p, .RealToComplexInt, token);
        } else if (destSK.isReal()) {
            res.qt = res.qt.toReal(p.comp);
            try res.implicitCast(p, .ComplexIntToReal, token);
            res.qt = intQt;
            try res.implicitCast(p, .IntCast, token);
        } else {
            res.qt = intQt;
            try res.implicitCast(p, .ComplexIntCast, token);
        }
    }
}

fn floatToIntWarning(
    res: *Result,
    p: *Parser,
    intTy: QualType,
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

pub fn castToFloat(res: *Result, p: *Parser, floatQt: QualType, token: TokenIndex) Error!void {
    const srcSK = res.qt.scalarKind(p.comp);
    const destSK = floatQt.scalarKind(p.comp);

    if (srcSK == .Bool) {
        try res.value.intToFloat(floatQt, p.comp);
        res.qt = floatQt.toReal(p.comp);
        try res.implicitCast(p, .BoolToFloat, token);
        if (!destSK.isReal()) {
            res.qt = floatQt;
            try res.implicitCast(p, .RealToComplexFloat, token);
        }
    } else if (srcSK.isInt()) {
        try res.value.intToFloat(floatQt, p.comp);
        if (srcSK.isReal() and destSK.isReal()) {
            res.qt = floatQt;
            try res.implicitCast(p, .IntToFloat, token);
        } else if (srcSK.isReal()) {
            res.qt = floatQt.toReal(p.comp);
            try res.implicitCast(p, .IntToFloat, token);
            res.qt = floatQt;
            try res.implicitCast(p, .RealToComplexFloat, token);
        } else if (destSK.isReal()) {
            res.qt = res.qt.toReal(p.comp);
            try res.implicitCast(p, .complex_int_to_real, token);
            res.qt = floatQt;
            try res.implicitCast(p, .IntToFloat, token);
        } else {
            res.qt = floatQt;
            try res.implicitCast(p, .ComplexIntToComplexFloat, token);
        }
    } else if (!res.qt.eql(floatQt, p.comp)) {
        try res.value.floatCast(floatQt, p.comp);
        if (srcSK.isReal() and destSK.isReal()) {
            res.qt = floatQt;
            try res.implicitCast(p, .FloatCast, token);
        } else if (srcSK.isReal()) {
            if (res.qt.floatRank(p.comp) != floatQt.floatRank(p.comp)) {
                res.qt = floatQt.toReal(p.comp);
                try res.implicitCast(p, .FloatCast, token);
            }
            res.qt = floatQt;
            try res.implicitCast(p, .RealToComplexFloat, token);
        } else if (destSK.isReal()) {
            res.qt = res.qt.toReal(p.comp);
            try res.implicitCast(p, .ComplexFloatToReal, token);
            if (res.qt.floatRank(p.comp) != floatQt.floatRank(p.comp)) {
                res.qt = floatQt;
                try res.implicitCast(p, .FloatCast, token);
            }
        } else {
            res.qt = floatQt;
            try res.implicitCast(p, .ComplexFloatCast, token);
        }
    }
}

/// converts a bool or integer to a pointer
pub fn castToPointer(res: *Result, p: *Parser, ptrTy: QualType, token: TokenIndex) Error!void {
    const resSK = res.qt.scalarKind(p.comp);
    if (resSK == .Bool) {
        res.qt = ptrTy;
        try res.implicitCast(p, .BoolToPointer, token);
    } else if (resSK.isInt()) {
        _ = try res.value.intCast(ptrTy, p.comp);
        res.qt = ptrTy;
        try res.implicitCast(p, .IntToPointer, token);
    }
}

fn castToVoid(res: *Result, p: *Parser, tok: TokenIndex) Error!void {
    if (!res.qt.is(.void)) {
        res.qt = .void;
        try res.implicitCast(p, .to_void, tok);
    }
}

fn nullToPointer(res: *Result, p: *Parser, ptrTy: QualType, token: TokenIndex) Error!void {
    if (!res.qt.is(p.comp, .nullptrTy) and !res.value.isZero(p.comp)) return;
    res.qt = ptrTy;
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
    if (res.qt.is(.fp16) and !p.comp.langOpts.useNativeHalfType) {
        return res.castToFloat(p, .float, token);
    }
    if (res.qt.isInt(p.comp) and !p.in_macro) {
        if (p.tree.bitfieldWidth(res.node, true)) |width| {
            if (res.qt.bitfieldPromotion(p.comp, width)) |promotionTy| {
                return res.castToInt(p, promotionTy, token);
            }
        }
        return res.castToInt(p, res.qt.promoteInt(p.comp), token);
    }
}

fn usualArithmeticConversion(lhs: *Result, rhs: *Result, p: *Parser, token: TokenIndex) Error!void {
    try lhs.usualUnaryConversion(p, token);
    try rhs.usualUnaryConversion(p, token);

    // if either is a float cast to that type
    if (lhs.qt.isFloat(p.comp) or rhs.qt.isFloat(p.comp)) {
        const floatTypes = [6][2]QualType{
            .{ .ComplexFloat128, .Float128 },
            .{ .complexlongDouble, .LongDouble },
            .{ .ComplexDouble, .Double },
            .{ .ComplexFloat, .Float },
            // No `_Complex __fp16` type
            .{ .Invalid, .FP16 },
            .{ .ComplexFloat16, .Float16 },
        };

        const lhsSpec = lhs.qt.canonicalize(.standard).specifier;
        const rhsSpec = rhs.qt.canonicalize(.standard).specifier;

        for (floatTypes) |ft| {
            if (try lhs.floatConversion(rhs, lhsSpec, rhsSpec, p, ft, token)) return;
        }
        unreachable;
    }

    if (lhs.qt.eql(rhs.ty, p.comp, true)) {
        // cast to promoted type
        try lhs.castToInt(p, lhs.ty, token);
        try rhs.castToInt(p, rhs.ty, token);
        return;
    }

    const targetTy = lhs.qt.integerConversion(rhs.ty, p.comp);
    if (!targetTy.isReal()) {
        try lhs.saveValue(p);
        try rhs.saveValue(p);
    }
    try lhs.castToInt(p, targetTy, token);
    try rhs.castToInt(p, targetTy, token);
}

fn floatConversion(
    lhs: *Result,
    rhs: *Result,
    lhsSpec: QualType,
    rhsSpec: QualType,
    p: *Parser,
    pair: [2]QualType,
    token: TokenIndex,
) !bool {
    if (lhsSpec == pair[0] or lhsSpec == pair[1] or
        rhsSpec == pair[0] or rhsSpec == pair[1])
    {
        const bothReal = lhs.qt.isReal() and rhs.qt.isReal();
        const resSpec = pair[@intFromBool(bothReal)];
        const ty = QualType{ .specifier = resSpec };
        try lhs.castToFloat(p, ty, token);
        try rhs.castToFloat(p, ty, token);
        return true;
    }
    return false;
}

fn invalidBinTy(lhs: *Result, tok: TokenIndex, rhs: *Result, p: *Parser) Error!bool {
    try p.errStr(.invalid_bin_types, tok, try p.typePairStr(lhs.qt, rhs.qt));
    lhs.value = .{};
    rhs.value = .{};
    lhs.qt = .invalid;
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

pub fn castType(res: *Result, p: *Parser, destQt: QualType, operandToken: TokenIndex, lparen: TokenIndex) !void {
    var explicitCK: Node.Cast.Kind = undefined;

    const destSK = destQt.scalarKind(p.comp);
    const srcSK = res.qt.scalarKind(p.comp);

    if (destQt.is(p.comp, .void)) {
        // everything can cast to void
        explicitCK = .ToVoid;
        res.value = .{};
    } else if (destSK == .NullptrTy) {
        if (srcSK == .NullptrTy) {
            explicitCK = .NoOP;
        } else {
            try p.errStr(.invalid_object_cast, lparen, try p.typePairStrExtra(res.qt, " to ", destQt));
            return error.ParsingFailed;
        }
    } else if (srcSK == .NullptrTy) {
        if (destSK == .Bool) {
            try res.nullToPointer(p, res.qt, lparen);
            res.value.boolCast(p.comp);
            res.qt = .bool;
            try res.implicitCast(p, .PointerToBool, lparen);
            try res.saveValue(p);
        } else if (destQt.isPointer()) {
            try res.nullToPointer(p, destQt, lparen);
        } else {
            try p.errStr(.invalid_object_cast, lparen, try p.typePairStrExtra(res.qt, " to ", destQt));
            return error.ParsingFailed;
        }
        explicitCK = .NoOP;
    } else if (res.value.isZero(p.comp) and destSK.isPointer()) {
        explicitCK = .NullToPointer;
    } else if (destSK != .None) cast: {
        if (destSK.isFloat() and srcSK.isPointer()) {
            try p.errStr(.invalid_cast_to_float, lparen, try p.typeStr(destQt));
            return error.ParsingFailed;
        } else if (srcSK.isFloat() and destSK.isPointer()) {
            try p.errStr(.invalid_cast_to_pointer, lparen, try p.typeStr(res.qt));
            return error.ParsingFailed;
        }

        if (destQt.eql(res.qt, p.comp)) {
            explicitCK = .NoOP;
        }
        // dest type is bool
        else if (destSK == .Bool) {
            if (srcSK.isPointer()) {
                explicitCK = .PointerToBool;
            } else if (srcSK.isInt()) {
                if (!srcSK.isReal()) {
                    res.qt = res.qt.toReal(p.comp);
                    try res.implicitCast(p, .ComplexIntToReal, lparen);
                }
            } else if (srcSK.isFloat()) {
                if (!srcSK.isReal()) {
                    res.qt = res.qt.toReal(p.comp);
                    try res.implicitCast(p, .ComplexFloatToReal, lparen);
                }
                explicitCK = .FloatToBool;
            }
        }
        // dest type is int
        else if (destQt.isInt()) {
            if (res.qt.is(.bool)) {
                if (!destSK.isReal()) {
                    res.ty = destQt.toReal(p.comp);
                    try res.implicitCast(p, .BoolToInt, lparen);
                    explicitCK = .RealToComplexInt;
                } else {
                    explicitCK = .BoolToInt;
                }
            } else if (srcSK.isInt()) {
                if (srcSK.isReal() and destSK.isReal()) {
                    explicitCK = .IntCast;
                } else if (srcSK.isReal()) {
                    res.qt = destQt.toReal(p.comp);
                    try res.implicitCast(p, .IntCast, lparen);
                    explicitCK = .RealToComplexInt;
                } else if (destSK.isReal()) {
                    res.qt = res.qt.toReal(p.comp);
                    try res.implicitCast(p, .ComplexIntToReal, lparen);
                    explicitCK = .IntCast;
                } else {
                    explicitCK = .ComplexIntCast;
                }
            } else if (srcSK.isPointer()) {
                if (!destSK.isReal()) {
                    res.ty = destQt.toReal(p.comp);
                    try res.implicitCast(p, .PointerToInt, lparen);
                    explicitCK = .RealToComplexInt;
                } else {
                    explicitCK = .PointerToInt;
                    res.value = .{};
                }
            } else if (srcSK.isReal() and destSK.isReal()) {
                explicitCK = .FloatToInt;
            } else if (srcSK.isReal()) {
                res.ty = destQt.toReal(p.comp);
                try res.implicitCast(p, .FloatToInt, lparen);
                explicitCK = .RealToComplexInt;
            } else if (destSK.isReal()) {
                res.qt = res.qt.toReal(p.comp);
                try res.implicitCast(p, .ComplexFloatToReal, lparen);
                explicitCK = .FloatToInt;
            } else {
                explicitCK = .ComplexFloatToComplexInt;
            }
        } else if (destSK.isPointer()) {
            if (srcSK.isPointer()) {
                explicitCK = .Bitcast;
            } else if (srcSK.isInt()) {
                if (!srcSK.isReal()) {
                    res.qt = res.qt.toReal(p.comp);
                    try res.implicitCast(p, .ComplexIntToReal, lparen);
                }
                explicitCK = .IntToPointer;
            } else if (srcSK == .bool) {
                explicitCK = .BoolToPointer;
            } else if (res.qt.is(p.comp, .array)) {
                explicitCK = .ArrayToPointer;
            } else if (res.qt.is(p.comp, .func)) {
                explicitCK = .FunctionToPointer;
            } else {
                try p.errStr(.cond_expr_type, operandToken, try p.typeStr(res.qt));
                return error.ParsingFailed;
            }
        } else if (destSK.isFloat()) {
            if (srcSK == .Bool) {
                if (!destSK.isReal()) {
                    res.ty = destQt.toReal(p.comp);
                    try res.implicitCast(p, .BoolToFloat, lparen);
                    explicitCK = .RealToComplexFloat;
                } else {
                    explicitCK = .BoolToFloat;
                }
            } else if (srcSK.isInt()) {
                if (srcSK.isReal() and destSK.isReal()) {
                    explicitCK = .IntToFloat;
                } else if (srcSK.isReal()) {
                    res.qt = destQt.toReal(p.comp);
                    try res.implicitCast(p, .IntToFloat, lparen);
                    explicitCK = .RealToComplexFloat;
                } else if (destSK.isReal()) {
                    res.qt = res.qt.toReal(p.comp);
                    try res.implicitCast(p, .ComplexIntToReal, lparen);
                    explicitCK = .IntToFloat;
                } else {
                    explicitCK = .ComplexIntToComplexFloat;
                }
            } else if (srcSK.isReal() and destSK.isReal()) {
                explicitCK = .FloatCast;
            } else if (srcSK.isReal()) {
                res.qt = destQt.toReal(p.comp);
                try res.implicitCast(p, .FloatCast, lparen);
                explicitCK = .RealToComplexFloat;
            } else if (destSK.isReal()) {
                res.qt = res.qt.toReal(p.comp);
                try res.implicitCast(p, .ComplexFloatToReal, lparen);
                explicitCK = .FloatCast;
            } else {
                explicitCK = .ComplexFloatCast;
            }
        }

        if (res.value.isNone()) break :cast;

        const oldInt = srcSK.isInt() or srcSK.isPointer();
        const newInt = destSK.isInt() or destSK.isPointer();
        if (destQt.is(.Bool)) {
            res.value.boolCast(p.comp);
        } else if (srcSK.isFloat() and newInt) {
            // Explicit cast, no conversion warning
            _ = try res.value.floatToInt(destQt, p.comp);
        } else if (destSK.isFloat() and oldInt) {
            try res.value.intToFloat(destQt, p.comp);
        } else if (destSK.isFloat() and srcSK.isFloat()) {
            try res.value.floatCast(destQt, p.comp);
        } else if (oldInt and newInt) {
            if (destQt.sizeofOrNull(p.comp) == null) {
                try p.errStr(.cast_to_incomplete_type, lparen, try p.typeStr(destQt));
                return error.ParsingFailed;
            }
            try res.value.intCast(destQt, p.comp);
        }
    } else if (destQt.get(p.comp, .Union)) |unionTy| {
        if (unionTy.layout == null) {
            try p.errStr(.cast_to_incomplete_type, lparen, try p.typeStr(destQt));
            return error.ParsingFailed;
        }

        for (unionTy.fields) |field| {
            if (field.qt.eql(res.qt, p.comp)) {
                explicitCK = .UnionCast;
                try p.errToken(.gnu_union_cast, lparen);
                break;
            }
        } else {
            try p.errStr(.invalid_union_cast, lparen, try p.typeStr(res.qt));
            return error.ParsingFailed;
        }
    } else {
        try p.errStr(.invalid_cast_type, lparen, try p.typeStr(destQt));
        return error.ParsingFailed;
    }

    if (destQt.isQualified())
        try p.errStr(.qual_cast, lparen, try p.typeStr(destQt));

    if (destQt.isInt() and srcSK.isPointer() and destQt.sizeCompare(res.qt, p.comp) == .lt)
        try p.errStr(.cast_to_smaller_int, lparen, try p.typePairStrExtra(destQt, " from ", res.qt));

    res.qt = destQt;
    res.node = try p.addNode(.{
        .cast = .{
            .lparen = lparen,
            .qt = res.qt,
            .operand = res.node,
            .kind = explicitCK,
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
pub fn intFitsInType(res: Result, p: *Parser, ty: QualType) !bool {
    const maxInt = try Value.maxInt(ty, p.comp);
    const minInt = try Value.minInt(ty, p.comp);

    return res.value.compare(.lte, maxInt, p.comp) and
        (res.qt.isUnsignedInt(p.comp) or res.value.compare(.gte, minInt, p.comp));
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

    fn typePairStr(ctx: CoerceContext, p: *Parser, destTy: QualType, srcTy: QualType) ![]const u8 {
        switch (ctx) {
            .assign, .init => return p.typePairStrExtra(destTy, " from incompatible type ", srcTy),
            .ret => return p.typePairStrExtra(srcTy, " from a function with incompatible result type ", destTy),
            .arg => return p.typePairStrExtra(srcTy, " to parameter of incompatible type ", destTy),
            .testCoerce => unreachable,
        }
    }
};

/// Perform assignment-like coercion to `dest_ty`.
pub fn coerce(res: *Result, p: *Parser, destTy: QualType, tok: TokenIndex, ctx: CoerceContext) !void {
    if (res.qt.isInvalid() or destTy.isInvalid()) {
        res.qt = .invalid;
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
    destQt: QualType,
    tok: TokenIndex,
    ctx: CoerceContext,
) (Error || error{CoercionFailed})!void {
    // Subject of the coercion does not need to be qualified.
    var destUnqual = destQt.unqualified();
    if (destUnqual.is(.NullPtrTy)) {
        if (res.qt.is(.NullPtrTy)) return;
    }
    // dest type is bool
    else if (destUnqual.is(.Bool)) {
        if (res.qt.isScalar() and !res.qt.is(.NullPtrTy)) {
            // this is ridiculous but it's what clang does
            try res.castToBool(p, destUnqual, tok);
            return;
        }
    }
    // dest type is int
    else if (destUnqual.isInt(p.comp)) {
        if (res.qt.isInt(p.comp) or res.qt.isFloat(p.comp)) {
            try res.castToInt(p, destUnqual, tok);
            return;
        } else if (res.qt.isPointer()) {
            if (ctx == .testCoerce)
                return error.CoercionFailed;
            try p.errStr(.implicit_ptr_to_int, tok, try p.typePairStrExtra(res.ty, " to ", destQt));
            try ctx.note(p);
            try res.castToInt(p, destUnqual, tok);
            return;
        }
    }
    // dest type is float
    else if (destUnqual.isFloat()) {
        if (res.qt.isInt(p.comp) or res.qt.isFloat(p.comp)) {
            try res.castToFloat(p, destUnqual, tok);
            return;
        }
    }
    // dest type is pointer
    else if (destUnqual.isPointer()) {
        if (res.value.isZero(p.comp) or res.qt.is(.NullPtrTy)) {
            try res.nullToPointer(p, destQt, tok);
            return;
        } else if (res.qt.isInt() and res.qt.isReal()) {
            if (ctx == .testCoerce)
                return error.CoercionFailed;
            try p.errStr(.implicit_int_to_ptr, tok, try p.typePairStrExtra(res.ty, " to ", destQt));
            try ctx.note(p);
            try res.castToPointer(p, destUnqual, tok);
            return;
        } else if (res.qt.isVoidStar() or destUnqual.eql(res.ty, p.comp)) {
            return; // ok
        } else if (destUnqual.isVoidStar() and res.qt.isPointer() or (res.qt.isInt() and res.qt.isReal())) {
            return; // ok
        } else if (destUnqual.eql(res.ty, p.comp, false)) {
            if (!destUnqual.childType(p.comp).qual.hasQuals(res.ty.childType(p.comp).qual)) {
                try p.errStr(switch (ctx) {
                    .assign => .ptr_assign_discards_quals,
                    .init => .ptr_init_discards_quals,
                    .ret => .ptr_ret_discards_quals,
                    .arg => .ptr_arg_discards_quals,
                    .testCoerce => return error.CoercionFailed,
                }, tok, try ctx.typePairStr(p, destQt, res.qt));
            }
            try res.castToPointer(p, destUnqual, tok);
            return;
        } else if (res.qt.isPointer()) {
            const differentSignOnly = destUnqual.childType(p.comp).sameRankDifferentSign(res.qt.childType(p.comp), p.comp);
            try p.errStr(switch (ctx) {
                .assign => ([2]Diagnostics.Tag{ .incompatible_ptr_assign, .incompatible_ptr_assign_sign })[@intFromBool(differentSignOnly)],
                .init => ([2]Diagnostics.Tag{ .incompatible_ptr_init, .incompatible_ptr_init_sign })[@intFromBool(differentSignOnly)],
                .ret => ([2]Diagnostics.Tag{ .incompatible_return, .incompatible_return_sign })[@intFromBool(differentSignOnly)],
                .arg => ([2]Diagnostics.Tag{ .incompatible_ptr_arg, .incompatible_ptr_arg_sign })[@intFromBool(differentSignOnly)],
                .testCoerce => return error.CoercionFailed,
            }, tok, try ctx.typePairStr(p, destQt, res.ty));
            try ctx.note(p);

            res.qt = destUnqual;
            return res.implicitCast(p, .bitcat, tok);
        }
    }
    // dest type is record
    else if (destUnqual.isRecord()) {
        if (destUnqual.eql(res.ty, p.comp, false))
            return; // ok

        if (ctx == .arg) {
            if (destUnqual.get(.Union)) |unionTy| {
                if (destQt.hasAttribute(.transparent_union)) transparent_union: {
                    res.coerceExtra(p, unionTy.data.record.fields[0].ty, tok, .testCoerce) catch |err| switch (err) {
                        error.CoercionFailed => break :transparent_union,
                        else => |e| return e,
                    };
                    res.node = try p.addNode(.{
                        .unionInitExpr = .{
                            .fieldIndex = 0,
                            .initializer = res.node,
                            .lbraceToken = tok,
                            .unionQt = destQt,
                        },
                    });
                    res.ty = destQt;
                    return;
                }
            }
        } else if (destUnqual.is(.Vector)) {
            if (destUnqual.eql(res.qt, p.comp, false))
                return; //ok
        }
    }
    // other type
    else {
        if (ctx == .assign and (destUnqual.isArray() or destUnqual.isFunc())) {
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
        try ctx.typePairStr(p, destQt, res.qt),
    );
    try ctx.note(p);
}
