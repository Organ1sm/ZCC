const std = @import("std");
const assert = std.debug.assert;

const AST = @import("../AST/AST.zig");
const TokenIndex = AST.TokenIndex;
const Node = AST.Node;
const Diagnostics = @import("../Basic/Diagnostics.zig");
const Parser = @import("Parser.zig");
const Error = Parser.Error;
const TypeStore = @import("../AST/TypeStore.zig");
const QualType = TypeStore.QualType;
const Value = @import("../AST/Value.zig");

const Result = @This();

node: Node.Index,
qt: QualType = .int,
value: Value = .{},

pub fn maybeWarnUnused(res: Result, p: *Parser, exprStart: TokenIndex, prevTotal: usize) Error!void {
    if (res.qt.is(p.comp, .void) or res.qt.isInvalid()) return;

    // don't warn about unused result if the expression contained errors besides other unused results
    if (p.diagnostics.total != prevTotal) return; // TODO improve
    // for (p.comp.diagnostics.list.items[errStart..]) |errItem| {
    //     if (errItem.tag != .unused_value) return;
    // }

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
                const callInfoTokenStr = p.getTokenText(callInfo.token);
                if (callInfo.nodiscard) try p.err(.nodiscard_unused, exprStart, .{callInfoTokenStr});
                if (callInfo.warnUnusedResult) try p.err(.warn_unused_result, exprStart, .{callInfoTokenStr});
                return;
            },

            .builtinCallExpr => |call| {
                const expanded = p.comp.builtins.lookup(p.getTokenText(call.builtinToken));
                const attributes = expanded.builtin.properties.attributes;
                if (attributes.pure) try p.err(.builtin_unused, call.builtinToken, .{"pure"});
                if (attributes.@"const") try p.err(.builtin_unused, call.builtinToken, .{"const"});
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

    try p.err(.unused_value, exprStart, .{});
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
            .parenExpr, .stmtExpr, .imaginaryLiteral, .compoundAssignDummyExpr,
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
    assert(lhs.qt.isPointer(p.comp) and rhs.qt.isPointer(p.comp));
    const gpa = p.comp.gpa;

    const lhsElem = lhs.qt.childType(p.comp);
    const rhsElem = rhs.qt.childType(p.comp);
    if (lhsElem.eqlQualified(rhsElem, p.comp)) return true;

    const hasVoidPtrBranch = lhs.qt.scalarKind(p.comp) == .VoidPointer or rhs.qt.scalarKind(p.comp) == .VoidPointer;
    const onlyQualsDiffer = lhsElem.eql(rhsElem, p.comp);
    const pointersCompatible = onlyQualsDiffer or hasVoidPtrBranch;

    var adjustedElemQt = lhsElem;
    if (!pointersCompatible or hasVoidPtrBranch) {
        if (!pointersCompatible)
            try p.err(.pointer_mismatch, token, .{ lhs.qt, rhs.qt });
        adjustedElemQt = .void;
    }

    if (pointersCompatible) {
        adjustedElemQt.@"const" = lhsElem.@"const" or rhsElem.@"const";
        adjustedElemQt.@"volatile" = lhsElem.@"volatile" or rhsElem.@"volatile";
    }

    if (!adjustedElemQt.eqlQualified(lhsElem, p.comp)) {
        lhs.qt = try p.comp.typeStore.put(gpa, .{ .pointer = .{
            .child = adjustedElemQt,
            .decayed = null,
        } });
        try lhs.implicitCast(p, .Bitcast, token);
    }

    if (!adjustedElemQt.eqlQualified(rhsElem, p.comp)) {
        rhs.qt = try p.comp.typeStore.put(gpa, .{ .pointer = .{
            .child = adjustedElemQt,
            .decayed = null,
        } });
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
        if (lhs.qt.eql(rhs.qt, p.comp))
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

    const lhsSK = lhs.qt.scalarKind(p.comp);
    const rhsSK = rhs.qt.scalarKind(p.comp);
    if (lhsSK.isInt() and rhsSK.isInt()) {
        try lhs.usualArithmeticConversion(rhs, p, token);
        return lhs.shouldEval(rhs, p);
    }

    if (kind == .integer)
        return lhs.invalidBinTy(token, rhs, p);

    if (lhsSK.isArithmetic() and rhsSK.isArithmetic()) {
        // <, <=, >, >= only work on real types
        if (kind == .relational and (!lhsSK.isReal() or !rhsSK.isReal()))
            return lhs.invalidBinTy(token, rhs, p);

        try lhs.usualArithmeticConversion(rhs, p, token);
        return lhs.shouldEval(rhs, p);
    }

    if (kind == .arithmetic)
        return lhs.invalidBinTy(token, rhs, p);

    switch (kind) {
        .booleanLogic => {
            if (!(lhsSK != .None or lhsSK == .NullptrTy) or
                !(rhsSK != .None or rhsSK == .NullptrTy))
            {
                return lhs.invalidBinTy(token, rhs, p);
            }

            // Do integer promotions but nothing else
            if (lhsSK.isInt()) try lhs.castToInt(p, lhs.qt.promoteInt(p.comp), token);
            if (rhsSK.isInt()) try rhs.castToInt(p, rhs.qt.promoteInt(p.comp), token);
            return lhs.shouldEval(rhs, p);
        },

        .relational, .equality => {
            if (kind == .equality and (lhsSK == .NullptrTy or rhsSK == .NullptrTy)) {
                if (lhsSK == .NullptrTy and rhsSK == .NullptrTy)
                    return lhs.shouldEval(rhs, p);

                const nullPtrRes = if (lhsSK == .NullptrTy) lhs else rhs;
                const otherRes = if (lhsSK == .NullptrTy) rhs else lhs;

                if (otherRes.qt.isPointer(p.comp)) {
                    try nullPtrRes.nullToPointer(p, otherRes.qt, token);
                    return otherRes.shouldEval(nullPtrRes, p);
                } else if (otherRes.value.isZero(p.comp)) {
                    otherRes.value = .null;
                    try otherRes.nullToPointer(p, nullPtrRes.qt, token);
                    return otherRes.shouldEval(nullPtrRes, p);
                }
                return lhs.invalidBinTy(token, rhs, p);
            }

            // comparisons between floats and pointes not allowed
            if (lhsSK == .None or rhsSK == .None or
                (lhsSK.isFloat() and rhsSK.isPointer()) or
                (rhsSK.isFloat() and lhsSK.isPointer()))
            {
                return lhs.invalidBinTy(token, rhs, p);
            }

            if (lhsSK == .NullptrTy or rhsSK == .NullptrTy) return lhs.invalidBinTy(token, rhs, p);

            if ((lhsSK.isInt() or rhsSK.isInt()) and !(lhs.value.isZero(p.comp) or rhs.value.isZero(p.comp))) {
                try p.err(.comparison_ptr_int, token, .{ lhs.qt, rhs.qt });
            } else if (lhsSK.isPointer() and rhsSK.isPointer()) {
                if (lhsSK != .VoidPointer and rhsSK != .VoidPointer) {
                    const lhsElem = lhs.qt.childType(p.comp);
                    const rhsElem = rhs.qt.childType(p.comp);
                    if (!lhsElem.eql(rhsElem, p.comp)) {
                        try p.err(.comparison_distinct_ptr, token, .{ lhs.qt, rhs.qt });
                        try rhs.castToPointer(p, lhs.qt, token);
                    }
                } else if (lhsSK == .VoidPointer) {
                    try rhs.castToPointer(p, lhs.qt, token);
                } else if (rhsSK == .VoidPointer) {
                    try lhs.castToPointer(p, rhs.qt, token);
                }
            } else if (lhsSK.isPointer()) {
                try rhs.castToPointer(p, lhs.qt, token);
            } else {
                assert(rhsSK.isPointer());
                try lhs.castToPointer(p, rhs.qt, token);
            }

            return lhs.shouldEval(rhs, p);
        },

        .conditional => {
            // doesn't matter what we return here, as the result is ignored
            if (lhs.qt.is(p.comp, .void) or rhs.qt.is(p.comp, .void)) {
                try lhs.castToVoid(p, token);
                try rhs.castToVoid(p, token);
                return true;
            }

            if (lhsSK == .NullptrTy and rhsSK == .NullptrTy)
                return true;

            if ((lhsSK.isPointer() and rhsSK.isInt()) or (lhsSK.isInt() and rhsSK.isPointer())) {
                if (lhs.value.isZero(p.comp) or rhs.value.isZero(p.comp)) {
                    try lhs.nullToPointer(p, rhs.qt, token);
                    try rhs.nullToPointer(p, lhs.qt, token);
                    return true;
                }

                const intType = if (lhsSK.isInt()) lhs else rhs;
                const ptrType = if (lhsSK.isPointer()) lhs else rhs;

                try p.err(.implicit_int_to_ptr, token, .{ intType.qt, ptrType.qt });
                try intType.castToPointer(p, ptrType.qt, token);
                return true;
            }

            if (lhsSK.isPointer() and rhsSK.isPointer())
                return lhs.adjustCondExprPtrs(token, rhs, p);

            if ((lhsSK.isPointer() and rhsSK == .NullptrTy) or (lhsSK == .NullptrTy and rhsSK.isPointer())) {
                const nullPtrRes = if (lhsSK == .NullptrTy) lhs else rhs;
                const ptrRes = if (lhsSK == .NullptrTy) rhs else lhs;
                try nullPtrRes.nullToPointer(p, ptrRes.qt, token);
                return true;
            }

            if (lhsSK.isPointer() and rhsSK.isPointer()) return lhs.adjustCondExprPtrs(token, rhs, p);

            if (lhs.qt.getRecord(p.comp) != null and rhs.qt.getRecord(p.comp) != null and lhs.qt.eql(rhs.qt, p.comp))
                return true;

            return lhs.invalidBinTy(token, rhs, p);
        },

        .add => {
            // if both aren't arithmetic one should be pointer and the other an integer
            if (lhsSK.isPointer() == rhsSK.isPointer() or lhsSK.isInt() == rhsSK.isInt())
                return lhs.invalidBinTy(token, rhs, p);

            if (lhsSK == .VoidPointer or rhsSK == .VoidPointer)
                try p.err(.gnu_pointer_arith, token, .{});

            if (lhsSK == .NullptrTy) try lhs.nullToPointer(p, .voidPointer, token);
            if (rhsSK == .NullptrTy) try rhs.nullToPointer(p, .voidPointer, token);

            // Do integer promotions but nothing else
            if (lhsSK.isInt()) try lhs.castToInt(p, lhs.qt.promoteInt(p.comp), token);
            if (rhsSK.isInt()) try rhs.castToInt(p, rhs.qt.promoteInt(p.comp), token);

            // The result type is the type of the pointer operand
            if (lhsSK.isInt()) lhs.qt = rhs.qt else rhs.qt = lhs.qt;
            return lhs.shouldEval(rhs, p);
        },

        .sub => {
            // if both aren't arithmetic then either both should be pointers or just a
            if (!lhsSK.isPointer() or !(rhsSK.isPointer() or rhsSK.isInt())) return lhs.invalidBinTy(token, rhs, p);

            if (lhsSK == .VoidPointer)
                try p.err(.gnu_pointer_arith, token, .{});

            if (lhsSK == .NullptrTy) try lhs.nullToPointer(p, .voidPointer, token);
            if (rhsSK == .NullptrTy) try rhs.nullToPointer(p, .voidPointer, token);

            if (lhsSK.isPointer() and rhsSK.isPointer()) {
                const lhsChildQt = lhs.qt.get(p.comp, .pointer).?.child;
                const rhsChildQt = rhs.qt.get(p.comp, .pointer).?.child;

                if (!lhsChildQt.eql(rhsChildQt, p.comp))
                    try p.err(.incompatible_pointers, token, .{ lhs.qt, rhs.qt });

                if (lhs.qt.childType(p.comp).sizeofOrNull(p.comp) orelse 1 == 0)
                    try p.err(.subtract_pointers_zero_elem_size, token, .{lhs.qt.childType(p.comp)});

                lhs.qt = p.comp.typeStore.ptrdiff;
            }

            // Do integer promotion on b if needed
            if (rhsSK.isInt()) try rhs.castToInt(p, rhs.qt.promoteInt(p.comp), token);
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
    else if (res.qt.is(p.comp, .array)) {
        res.value = try p.pointerValue(res.node, .zero);
        res.qt = try res.qt.decay(p.comp);
        try res.implicitCast(p, .ArrayToPointer, token);
    }
    // Perform l-value to r-value conversion
    else if (!p.inMacro and p.tree.isLValue(res.node)) {
        res.qt = res.qt.unqualified();
        try res.implicitCast(p, .LValToRVal, token);
    }
}

pub fn castToBool(res: *Result, p: *Parser, boolQt: QualType, tok: TokenIndex) Error!void {
    if (res.qt.isInvalid()) return;
    std.debug.assert(!boolQt.isInvalid());

    const srcSK = res.qt.scalarKind(p.comp);
    if (res.qt.is(p.comp, .array)) {
        if (res.value.is(.bytes, p.comp)) {
            try p.err(.string_literal_to_bool, tok, .{ res.qt, boolQt });
        } else {
            try p.err(.array_address_to_bool, tok, .{p.getTokenText(tok)});
        }
        try res.lvalConversion(p, tok);
        res.value = .one;
        res.qt = boolQt;
        try res.implicitCast(p, .PointerToBool, tok);
    } else if (srcSK.isPointer()) {
        res.value.boolCast(p.comp);
        res.qt = boolQt;
        try res.implicitCast(p, .PointerToBool, tok);
    } else if (srcSK.isInt() and srcSK != .Bool) {
        res.value.boolCast(p.comp);
        if (!srcSK.isReal()) {
            res.qt = res.qt.toReal(p.comp);
            try res.implicitCast(p, .ComplexFloatToReal, tok);
        }
        res.qt = boolQt;
        try res.implicitCast(p, .IntToBool, tok);
    } else if (srcSK.isFloat()) {
        const oldValue = res.value;
        const valueChangeKind = try res.value.floatToInt(boolQt, p.comp);
        try res.floatToIntWarning(p, boolQt, oldValue, valueChangeKind, tok);
        if (!srcSK.isReal()) {
            res.qt = res.qt.toReal(p.comp);
            try res.implicitCast(p, .ComplexFloatToReal, tok);
        }
        res.qt = boolQt;
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
    if (res.qt.isInvalid()) return;
    std.debug.assert(!intQt.isInvalid());
    if (intQt.hasIncompleteSize(p.comp)) {
        return error.ParsingFailed; // Cast to incomplete enum, diagnostic already issued
    }

    const srcSK = res.qt.scalarKind(p.comp);
    const destSK = intQt.scalarKind(p.comp);

    if (srcSK == .Bool) {
        res.qt = intQt.toReal(p.comp);
        try res.implicitCast(p, .BoolToInt, token);
        if (!destSK.isReal()) {
            res.qt = intQt;
            try res.implicitCast(p, .RealToComplexInt, token);
        }
    }

    // Cast from pointer to integer
    else if (srcSK.isPointer()) {
        res.value = .{};
        res.qt = intQt.toReal(p.comp);
        try res.implicitCast(p, .PointerToInt, token);
        if (!destSK.isReal()) {
            res.qt = intQt;
            try res.implicitCast(p, .RealToComplexInt, token);
        }
    }

    // Cast from floating point to integer.
    else if (res.qt.isFloat(p.comp)) {
        const oldValue = res.value;
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
    }

    // Cast from integer to integer
    else if (!res.qt.eql(intQt, p.comp)) {
        const oldValue = res.value;
        const valueChangeKind = try res.value.intCast(intQt, p.comp);
        switch (valueChangeKind) {
            .none => {},
            .truncated => try p.errValueChanged(.int_value_changed, token, res.*, oldValue, intQt),
            .signChanged => try p.err(.sign_conversion, token, .{ res.qt, intQt }),
        }

        if (srcSK.isReal() and destSK.isReal()) {
            res.qt = intQt;
            try res.implicitCast(p, .IntCast, token);
        } else if (srcSK.isReal()) {
            const realIntQt = intQt.toReal(p.comp);
            if (!res.qt.eql(realIntQt, p.comp)) {
                res.qt = realIntQt;
                try res.implicitCast(p, .IntCast, token);
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
    res: Result,
    p: *Parser,
    intQt: QualType,
    oldValue: Value,
    changeKind: Value.FloatToIntChangeKind,
    tok: TokenIndex,
) !void {
    switch (changeKind) {
        .none => return p.err(.float_to_int, tok, .{ res.qt, intQt }),
        .outOfRange => return p.err(.float_out_of_range, tok, .{ res.qt, intQt }),
        .overflow => return p.err(.float_overflow_conversion, tok, .{ res.qt, intQt }),
        .nonZeroToZero => return p.errValueChanged(.float_zero_conversion, tok, res, oldValue, intQt),
        .valueChanged => return p.errValueChanged(.float_value_changed, tok, res, oldValue, intQt),
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
    }
    // from int to float
    else if (srcSK.isInt()) {
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
            try res.implicitCast(p, .ComplexIntToReal, token);
            res.qt = floatQt;
            try res.implicitCast(p, .IntToFloat, token);
        } else {
            res.qt = floatQt;
            try res.implicitCast(p, .ComplexIntToComplexFloat, token);
        }
    }
    // from float to float
    else if (!res.qt.eql(floatQt, p.comp)) {
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
pub fn castToPointer(res: *Result, p: *Parser, ptrQt: QualType, token: TokenIndex) Error!void {
    const srcSK = res.qt.scalarKind(p.comp);
    if (srcSK == .Bool) {
        res.qt = ptrQt;
        try res.implicitCast(p, .BoolToPointer, token);
    } else if (srcSK.isInt()) {
        _ = try res.value.intCast(ptrQt, p.comp);
        res.qt = ptrQt;
        try res.implicitCast(p, .IntToPointer, token);
    } else if (srcSK.isPointer() and !res.qt.eql(ptrQt, p.comp)) {
        res.qt = ptrQt;
        try res.implicitCast(p, .Bitcast, token);
    }
}

fn castToVoid(res: *Result, p: *Parser, tok: TokenIndex) Error!void {
    if (!res.qt.is(p.comp, .void)) {
        res.qt = .void;
        try res.implicitCast(p, .ToVoid, tok);
    }
}

pub fn nullToPointer(res: *Result, p: *Parser, ptrQt: QualType, token: TokenIndex) Error!void {
    if (!res.qt.is(p.comp, .nullptrTy) and !res.value.isZero(p.comp)) return;
    res.value = .null;
    res.qt = ptrQt;
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
    if (res.qt.isInvalid()) return;
    if (!p.comp.langOpts.useNativeHalfType) {
        if (res.qt.get(p.comp, .float)) |floatTy| {
            if (floatTy == .FP16) {
                return res.castToFloat(p, .float, token);
            }
        }
    }
    if (res.qt.isInt(p.comp) and !p.inMacro) {
        if (p.tree.bitfieldWidth(res.node, true)) |width| {
            if (res.qt.promoteBitfield(p.comp, width)) |promotionTy| {
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
    const lhsIsFloat = lhs.qt.isFloat(p.comp);
    const rhsIsFloat = rhs.qt.isFloat(p.comp);
    if (lhsIsFloat and rhsIsFloat) {
        const lhsIsComplex = lhs.qt.is(p.comp, .complex);
        const rhsIsComplex = rhs.qt.is(p.comp, .complex);

        const resQt = if (lhs.qt.floatRank(p.comp) > rhs.qt.floatRank(p.comp))
            (if (!lhsIsComplex and rhsIsComplex) try lhs.qt.toComplex(p.comp) else lhs.qt)
        else
            (if (!rhsIsComplex and lhsIsComplex) try rhs.qt.toComplex(p.comp) else rhs.qt);

        try lhs.castToFloat(p, resQt, token);
        try rhs.castToFloat(p, resQt, token);
        return;
    } else if (lhsIsFloat) {
        try rhs.castToFloat(p, lhs.qt, token);
        return;
    } else if (rhsIsFloat) {
        try lhs.castToFloat(p, rhs.qt, token);
        return;
    }

    if (lhs.qt.eql(rhs.qt, p.comp)) {
        // cast to promoted type
        try lhs.castToInt(p, lhs.qt, token);
        try rhs.castToInt(p, rhs.qt, token);
        return;
    }

    const lhsReal = lhs.qt.toReal(p.comp);
    const rhsReal = rhs.qt.toReal(p.comp);

    const typeOrder = lhs.qt.intRankOrder(rhs.qt, p.comp);
    const lhsIsSigned = lhs.qt.signedness(p.comp) == .signed;
    const rhsIsSigned = rhs.qt.signedness(p.comp) == .signed;

    var targetQt: QualType = .invalid;
    if (lhsIsSigned == rhsIsSigned) {
        // If both have the same sign, use higher-rank type.
        targetQt = switch (typeOrder) {
            .lt => rhs.qt,
            .eq, .gt => lhsReal,
        };
    } else if (typeOrder != if (lhsIsSigned) std.math.Order.gt else std.math.Order.lt) {
        // Only one is signed; and the unsigned type has rank >= the signed type
        // Use the unsigned type
        targetQt = if (rhsIsSigned) lhsReal else rhsReal;
    } else if (lhsReal.bitSizeof(p.comp) != rhsReal.bitSizeof(p.comp)) {
        // Signed type is higher rank and sizes are not equal
        // Use the signed type
        targetQt = if (lhsIsSigned) lhsReal else rhsReal;
    } else {
        // Signed type is higher rank but same size as unsigned type
        // e.g. `long` and `unsigned` on x86-linux-gnu
        // Use unsigned version of the signed type
        targetQt = if (lhsIsSigned) try lhsReal.makeIntUnsigned(p.comp) else try rhsReal.makeIntUnsigned(p.comp);
    }

    if (lhs.qt.is(p.comp, .complex) or rhs.qt.is(p.comp, .complex)) {
        targetQt = try targetQt.toComplex(p.comp);
    }

    if (targetQt.is(p.comp, .complex)) {
        // TODO implement complex int values
        try lhs.saveValue(p);
        try rhs.saveValue(p);
    }
    try lhs.castToInt(p, targetQt, token);
    try rhs.castToInt(p, targetQt, token);
}

fn invalidBinTy(lhs: *Result, tok: TokenIndex, rhs: *Result, p: *Parser) Error!bool {
    try p.err(.invalid_bin_types, tok, .{ lhs.qt, rhs.qt });
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
        try p.tree.valueMap.put(p.comp.gpa, res.node, res.value);

    res.value = .{};
}

/// Saves value without altering the result.
pub fn putValue(res: *const Result, p: *Parser) !void {
    if (res.value.isNone() or res.value.isNull()) return;
    if (!p.inMacro) try p.tree.valueMap.put(p.comp.gpa, res.node, res.value);
}

pub fn castType(res: *Result, p: *Parser, destQt: QualType, operandToken: TokenIndex, lparen: TokenIndex) !void {
    if (res.qt.isInvalid()) {
        res.value = .{};
        return;
    } else if (destQt.isInvalid()) {
        res.value = .{};
        res.qt = .invalid;
        return;
    }

    var explicitCK: Node.Cast.Kind = undefined;

    const destSK = destQt.scalarKind(p.comp);
    const srcSK = res.qt.scalarKind(p.comp);

    if (destQt.is(p.comp, .void)) {
        // everything can cast to void
        explicitCK = .ToVoid;
        res.value = .{};
    } else if (res.qt.is(p.comp, .void)) {
        try p.err(.invalid_cast_operand_type, operandToken, .{res.qt});
        return error.ParsingFailed;
    } else if (destSK == .NullptrTy) {
        res.value = .{};
        if (srcSK == .NullptrTy) {
            explicitCK = .NoOP;
        } else {
            try p.err(.invalid_object_cast, lparen, .{ res.qt, destQt });
            return error.ParsingFailed;
        }
    } else if (srcSK == .NullptrTy) {
        if (destSK == .Bool) {
            try res.nullToPointer(p, res.qt, lparen);
            res.value.boolCast(p.comp);
            res.qt = .bool;
            try res.implicitCast(p, .PointerToBool, lparen);
            try res.saveValue(p);
        } else if (destSK.isPointer()) {
            try res.nullToPointer(p, destQt, lparen);
        } else {
            try p.err(.invalid_object_cast, lparen, .{ res.qt, destQt });
            return error.ParsingFailed;
        }
        explicitCK = .NoOP;
    } else if (res.value.isZero(p.comp) and destSK.isPointer()) {
        explicitCK = .NullToPointer;
    } else if (destSK != .None) cast: {
        if (destSK.isFloat() and srcSK.isPointer()) {
            try p.err(.invalid_cast_to_float, lparen, .{destQt});
            return error.ParsingFailed;
        } else if (srcSK.isFloat() and destSK.isPointer()) {
            try p.err(.invalid_cast_to_pointer, lparen, .{res.qt});
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
                explicitCK = .IntToBool;
            } else if (srcSK.isFloat()) {
                if (!srcSK.isReal()) {
                    res.qt = res.qt.toReal(p.comp);
                    try res.implicitCast(p, .ComplexFloatToReal, lparen);
                }
                explicitCK = .FloatToBool;
            }
        }
        // dest type is int
        else if (destSK.isInt()) {
            if (srcSK == .Bool) {
                if (!destSK.isReal()) {
                    res.qt = destQt.toReal(p.comp);
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
                res.value = .{};
                if (!destSK.isReal()) {
                    res.qt = destQt.toReal(p.comp);
                    try res.implicitCast(p, .PointerToInt, lparen);
                    explicitCK = .RealToComplexInt;
                } else {
                    explicitCK = .PointerToInt;
                }
            } else if (srcSK.isReal() and destSK.isReal()) {
                explicitCK = .FloatToInt;
            } else if (srcSK.isReal()) {
                res.qt = destQt.toReal(p.comp);
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
            } else if (srcSK == .Bool) {
                explicitCK = .BoolToPointer;
            } else if (res.qt.is(p.comp, .array)) {
                explicitCK = .ArrayToPointer;
            } else if (res.qt.is(p.comp, .func)) {
                explicitCK = .FunctionToPointer;
            } else {
                try p.err(.invalid_cast_operand_type, operandToken, .{res.qt});
                return error.ParsingFailed;
            }
        } else if (destSK.isFloat()) {
            if (srcSK == .Bool) {
                if (!destSK.isReal()) {
                    res.qt = destQt.toReal(p.comp);
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

        const srcIsInt = srcSK.isInt() or srcSK.isPointer();
        const destIsInt = destSK.isInt() or destSK.isPointer();
        if (destSK == .Bool) {
            res.value.boolCast(p.comp);
        } else if (srcSK.isFloat() and destIsInt) {
            if (destQt.hasIncompleteSize(p.comp)) {
                try p.err(.cast_to_incomplete_type, lparen, .{destQt});
                return error.ParsingFailed;
            }
            // Explicit cast, no conversion warning
            _ = try res.value.floatToInt(destQt, p.comp);
        } else if (destSK.isFloat() and srcIsInt) {
            try res.value.intToFloat(destQt, p.comp);
        } else if (destSK.isFloat() and srcSK.isFloat()) {
            try res.value.floatCast(destQt, p.comp);
        } else if (srcIsInt and destIsInt) {
            if (destQt.hasIncompleteSize(p.comp)) {
                try p.err(.cast_to_incomplete_type, lparen, .{destQt});
                return error.ParsingFailed;
            }
            _ = try res.value.intCast(destQt, p.comp);
        }
    } else if (destQt.get(p.comp, .@"union")) |unionTy| {
        if (unionTy.layout == null) {
            try p.err(.cast_to_incomplete_type, lparen, .{destQt});
            return error.ParsingFailed;
        }

        for (unionTy.fields) |field| {
            if (field.qt.eql(res.qt, p.comp)) {
                explicitCK = .UnionCast;
                try p.err(.gnu_union_cast, lparen, .{});
                break;
            }
        } else {
            try p.err(.invalid_union_cast, lparen, .{res.qt});
            return error.ParsingFailed;
        }
    } else {
        try p.err(.invalid_cast_type, lparen, .{destQt});
        return error.ParsingFailed;
    }

    if (destQt.isQualified())
        try p.err(.qual_cast, lparen, .{destQt});

    if (destSK.isInt() and srcSK.isPointer() and destQt.sizeCompare(res.qt, p.comp) == .lt)
        try p.err(.cast_to_smaller_int, lparen, .{ destQt, res.qt });

    res.qt = destQt.unqualified();
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
        (res.qt.isUnsigned(p.comp) or res.value.compare(.gte, minInt, p.comp));
}

const CoerceContext = union(enum) {
    assign,
    init,
    ret,
    arg: TokenIndex,
    testCoerce,

    fn note(ctx: CoerceContext, p: *Parser) !void {
        switch (ctx) {
            .arg => |tok| try p.err(.parameter_here, tok, .{}),
            .testCoerce => unreachable,
            else => {},
        }
    }
};

/// Perform assignment-like coercion to `dest_ty`.
pub fn coerce(res: *Result, p: *Parser, destTy: QualType, tok: TokenIndex, ctx: CoerceContext) !void {
    if (destTy.isInvalid()) {
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
    const srcOriginalQt = res.qt;
    switch (ctx) {
        .init, .ret, .assign => try res.lvalConversion(p, tok),
        else => {},
    }
    if (res.qt.isInvalid()) return;

    const destUnqual = destQt.unqualified();
    const srcSK = res.qt.scalarKind(p.comp);
    const destSK = destUnqual.scalarKind(p.comp);

    if (destQt.is(p.comp, .vector) and res.qt.is(p.comp, .vector)) {
        if (destUnqual.eql(res.qt, p.comp)) return; // ok
        if (destUnqual.sizeCompare(res.qt, p.comp) == .eq) {
            res.qt = destUnqual;
            return res.implicitCast(p, .Bitcast, tok);
        }
    } else if (destSK == .NullptrTy) {
        if (srcSK == .NullptrTy) return;
    }
    // dest type is bool
    else if (destSK == .Bool) {
        if (srcSK != .None and srcSK != .NullptrTy) {
            // this is ridiculous but it's what clang does
            try res.castToBool(p, destUnqual, tok);
            return;
        }
    }
    // dest type is int
    else if (destSK.isInt()) {
        if (srcSK.isArithmetic()) {
            try res.castToInt(p, destUnqual, tok);
            return;
        } else if (srcSK.isPointer()) {
            if (ctx == .testCoerce) return error.CoercionFailed;
            try p.err(.implicit_ptr_to_int, tok, .{ srcOriginalQt, destUnqual });
            try ctx.note(p);
            try res.castToInt(p, destUnqual, tok);
            return;
        }
    }
    // dest type is float
    else if (destSK.isFloat()) {
        if (srcSK.isArithmetic()) {
            try res.castToFloat(p, destUnqual, tok);
            return;
        }
    }
    // dest type is pointer
    else if (destSK.isPointer()) {
        if (res.value.isZero(p.comp) or srcSK == .NullptrTy) {
            try res.nullToPointer(p, destUnqual, tok);
            return;
        } else if (srcSK.isInt() and srcSK.isReal()) {
            if (ctx == .testCoerce) return error.CoercionFailed;
            try p.err(.implicit_int_to_ptr, tok, .{ srcOriginalQt, destUnqual });
            try ctx.note(p);
            try res.castToPointer(p, destUnqual, tok);
            return;
        } else if (srcSK == .VoidPointer or destUnqual.eql(res.qt, p.comp)) {
            return res.castToPointer(p, destUnqual, tok);
        } else if (destSK == .VoidPointer and srcSK.isPointer() or (srcSK.isInt() and srcSK.isReal())) {
            return res.castToPointer(p, destUnqual, tok);
        } else if (srcSK.isPointer()) {
            const srcChild = res.qt.childType(p.comp);
            const destChild = destUnqual.childType(p.comp);

            if (srcChild.eql(destChild, p.comp)) {
                if ((srcChild.@"const" and !destChild.@"const") or
                    (srcChild.@"volatile" and !destChild.@"volatile") or
                    (srcChild.restrict and !destChild.restrict))
                {
                    try p.err(switch (ctx) {
                        .assign => .ptr_assign_discards_quals,
                        .init => .ptr_init_discards_quals,
                        .ret => .ptr_ret_discards_quals,
                        .arg => .ptr_arg_discards_quals,
                        .testCoerce => return error.CoercionFailed,
                    }, tok, .{ destQt, srcOriginalQt });
                }
                try res.castToPointer(p, destUnqual, tok);
                return;
            }
            const differentSignOnly = srcChild.sameRankDifferentSign(destChild, p.comp);
            switch (ctx) {
                .assign => try p.err(if (differentSignOnly) .incompatible_ptr_assign_sign else .incompatible_ptr_assign, tok, .{ destQt, srcOriginalQt }),
                .init => try p.err(if (differentSignOnly) .incompatible_ptr_init_sign else .incompatible_ptr_init, tok, .{ destQt, srcOriginalQt }),
                .ret => try p.err(if (differentSignOnly) .incompatible_return_sign else .incompatible_return, tok, .{ srcOriginalQt, destQt }),
                .arg => try p.err(if (differentSignOnly) .incompatible_ptr_arg_sign else .incompatible_ptr_arg, tok, .{ srcOriginalQt, destQt }),
                .testCoerce => return error.CoercionFailed,
            }
            try ctx.note(p);

            res.qt = destUnqual;
            return res.implicitCast(p, .Bitcast, tok);
        }
    }
    // dest type is record
    else if (destUnqual.getRecord(p.comp) != null) {
        if (destUnqual.eql(res.qt, p.comp))
            return; // ok

        if (ctx == .arg) {
            if (destUnqual.get(p.comp, .@"union")) |unionTy| {
                if (destUnqual.hasAttribute(p.comp, .transparent_union)) transparent_union: {
                    res.coerceExtra(p, unionTy.fields[0].qt, tok, .testCoerce) catch |err| switch (err) {
                        error.CoercionFailed => break :transparent_union,
                        else => |e| return e,
                    };
                    res.node = try p.addNode(.{
                        .unionInitExpr = .{
                            .fieldIndex = 0,
                            .initializer = res.node,
                            .lbraceToken = tok,
                            .unionQt = destUnqual,
                        },
                    });
                    res.qt = destUnqual;
                    return;
                }
            }
        } else if (destUnqual.is(p.comp, .vector)) {
            if (destUnqual.eql(res.qt, p.comp))
                return; //ok
        }
    }
    // other type
    else {
        if (ctx == .assign and (destUnqual.is(p.comp, .array) or destUnqual.is(p.comp, .func))) {
            try p.err(.not_assignable, tok, .{});
            return;
        } else if (ctx == .testCoerce) {
            return error.CoercionFailed;
        }
        // This case should not be possible and an error should have already been emitted but we
        // might still have attempted to parse further so return error.ParsingFailed here to stop.
        return error.ParsingFailed;
    }

    switch (ctx) {
        .assign => try p.err(.incompatible_assign, tok, .{ destUnqual, res.qt }),
        .init => try p.err(.incompatible_init, tok, .{ destUnqual, res.qt }),
        .ret => try p.err(.incompatible_return, tok, .{ res.qt, destUnqual }),
        .arg => try p.err(.incompatible_arg, tok, .{ res.qt, destUnqual }),
        .testCoerce => return error.CoercionFailed,
    }
    try ctx.note(p);
}
