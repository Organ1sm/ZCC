const std = @import("std");
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const backend = @import("backend");
const IR = backend.Ir;
const IRBuilder = IR.Builder;
const Inst = IR.Inst;
const Interner = backend.Interner;
const Compilation = @import("../Basic/Compilation.zig");
const Tree = @import("../AST/AST.zig");
const TreeTag = @import("../AST/AstTag.zig").Tag;
const Node = Tree.Node;
const Type = @import("../AST/Type.zig");
const Value = @import("../AST/Value.zig");
const StringInterner = @import("../Basic/StringInterner.zig");
const StringId = StringInterner.StringId;

const CodeGen = @This();

const WipSwitch = struct {
    cases: Cases = .{},
    default: ?IR.Ref = null,
    size: u64,

    const Cases = std.MultiArrayList(struct {
        value: Interner.Ref,
        label: IR.Ref,
    });
};

const Symbol = struct {
    name: StringId,
    value: IR.Ref,
};

const Error = Compilation.Error;

tree: *const Tree,
comp: *Compilation,
builder: IRBuilder,
wipSwitch: *WipSwitch = undefined,
symbols: std.ArrayListUnmanaged(Symbol) = .{},
retNodes: std.ArrayListUnmanaged(Inst.Phi.Input) = .{},
phiNodes: std.ArrayListUnmanaged(Inst.Phi.Input) = .{},
recordElemBuffer: std.ArrayListUnmanaged(Interner.Ref) = .{},
recordCache: std.AutoHashMapUnmanaged(*Type.Record, Interner.Ref) = .{},

condDummyTy: ?Interner.Ref = null,
boolInvert: bool = false,
boolEndLabel: IR.Ref = .none,
condDummyRef: IR.Ref = undefined,
continueLabel: IR.Ref = undefined,
breakLabel: IR.Ref = undefined,
returnLabel: IR.Ref = undefined,

fn fail(c: *CodeGen, comptime fmt: []const u8, args: anytype) error{ FatalError, OutOfMemory } {
    try c.comp.diagnostics.list.append(c.comp.gpa, .{
        .tag = .cli_error,
        .kind = .@"fatal error",
        .extra = .{ .str = try std.fmt.allocPrint(c.comp.diagnostics.arena.allocator(), fmt, args) },
    });
    return error.FatalError;
}

pub fn generateIR(tree: *const Tree) Compilation.Error!IR {
    const gpa = tree.comp.gpa;
    var c: CodeGen = .{
        .builder = .{
            .gpa = tree.comp.gpa,
            .arena = std.heap.ArenaAllocator.init(gpa),
            .interner = &tree.comp.interner,
        },
        .tree = tree,
        .comp = tree.comp,
    };
    defer c.symbols.deinit(gpa);
    defer c.retNodes.deinit(gpa);
    defer c.phiNodes.deinit(gpa);
    defer c.recordElemBuffer.deinit(gpa);
    defer c.recordCache.deinit(gpa);
    defer c.builder.deinit();

    for (tree.rootDecls.items) |decl| {
        c.builder.arena.deinit();
        c.builder.arena = std.heap.ArenaAllocator.init(gpa);

        switch (decl.get(c.tree)) {
            .staticAssert,
            .typedef,
            .structDecl,
            .unionDecl,
            .enumDecl,
            => {},

            .fnProto,
            => {},

            .fnDef => |def| c.genFn(def) catch |err| switch (err) {
                error.FatalError => return error.FatalError,
                error.OutOfMemory => return error.OutOfMemory,
            },

            .variable => |variable| c.genVar(variable) catch |err| switch (err) {
                error.FatalError => return error.FatalError,
                error.OutOfMemory => return error.OutOfMemory,
            },
            else => unreachable,
        }
    }
    return c.builder.finish();
}

fn genType(c: *CodeGen, baseTy: Type) !Interner.Ref {
    var key: Interner.Key = undefined;
    const ty = baseTy.canonicalize(.standard);
    switch (ty.specifier) {
        .Void => return .void,
        .Bool => return .i1,
        .Struct => {
            if (c.recordCache.get(ty.data.record)) |some| return some;

            const elemBufferTop = c.recordElemBuffer.items.len;
            defer c.recordElemBuffer.items.len = elemBufferTop;

            for (ty.data.record.fields) |field| {
                if (!field.isRegularField())
                    return c.fail("TODO lower struct bitfields", .{});

                // TODO handle padding bits
                const fieldRef = try c.genType(field.ty);
                try c.recordElemBuffer.append(c.builder.gpa, fieldRef);
            }

            return c.builder.interner.put(c.builder.gpa, .{
                .recordTy = c.recordElemBuffer.items[elemBufferTop..],
            });
        },

        .Union => {
            return c.fail("TODO lower union types", .{});
        },

        else => {},
    }

    if (ty.isPointer())
        return .ptr;

    if (ty.isFunc())
        return .func;

    if (!ty.isReal())
        return c.fail("TODO lower complex types", .{});

    if (ty.isInt()) {
        const bits = ty.bitSizeof(c.comp).?;
        key = .{ .intTy = @intCast(bits) };
    } else if (ty.isFloat()) {
        const bits = ty.bitSizeof(c.comp).?;
        key = .{ .floatTy = @intCast(bits) };
    } else if (ty.isArray()) {
        const elem = try c.genType(ty.getElemType());
        key = .{ .arrayTy = .{ .child = elem, .len = ty.arrayLen().? } };
    } else if (ty.isVector()) {
        const elem = try c.genType(ty.getElemType());
        key = .{ .vectorTy = .{ .child = elem, .len = @intCast(ty.data.array.len) } };
    } else if (ty.is(.NullPtrTy)) {
        return c.fail("TODO lower nullptr_t", .{});
    }

    return c.builder.interner.put(c.builder.gpa, key);
}

fn genFn(c: *CodeGen, def: Node.FnDef) Error!void {
    const name = c.tree.getTokenSlice(def.nameToken);
    const funcTy = def.type.canonicalize(.standard);
    c.retNodes.items.len = 0;

    try c.builder.startFn();

    const params = funcTy.getParams();
    // Generate parameters
    for (params) |param| {
        // TODO handle calling convention here
        const arg = try c.builder.addArg(try c.genType(param.ty));
        const size: u32 = @intCast(param.ty.sizeof(c.comp).?); // TODO add error in parser
        const @"align" = param.ty.alignof(c.comp);
        const alloc = try c.builder.addAlloc(size, @"align");

        try c.builder.addStore(alloc, arg);
        try c.symbols.append(c.comp.gpa, .{ .name = param.name, .value = alloc });
    }

    // Generate body
    c.returnLabel = try c.builder.makeLabel("return");
    try c.genStmt(def.bpdy);

    // Relocate returns
    if (c.retNodes.items.len != 1)
        try c.builder.startBlock(c.returnLabel);
    // try c.builder.body.append(c.builder.gpa, c.returnLabel);
    if (c.retNodes.items.len == 0) {
        _ = try c.builder.addInst(.Ret, .{ .un = .none }, .noreturn);
    } else if (c.retNodes.items.len == 1) {
        c.builder.body.items.len -= 1;
        _ = try c.builder.addInst(.Ret, .{ .un = c.retNodes.items[0].value }, .noreturn);
    } else {
        const phi = try c.builder.addPhi(c.retNodes.items, try c.genType(funcTy.getReturnType()));
        _ = try c.builder.addInst(.Ret, .{ .un = phi }, .noreturn);
    }
    try c.builder.finishFn(name);
}

fn addUn(c: *CodeGen, tag: Inst.Tag, operand: IR.Ref, ty: Type) !IR.Ref {
    return c.builder.addInst(tag, .{ .un = operand }, try c.genType(ty));
}

fn addBin(c: *CodeGen, tag: Inst.Tag, lhs: IR.Ref, rhs: IR.Ref, ty: Type) !IR.Ref {
    return c.builder.addInst(tag, .{ .bin = .{ .lhs = lhs, .rhs = rhs } }, try c.genType(ty));
}

fn addBranch(c: *CodeGen, cond: IR.Ref, trueLabel: IR.Ref, falseLabel: IR.Ref) !void {
    if (trueLabel == c.boolEndLabel) {
        if (falseLabel == c.boolEndLabel) {
            try c.phiNodes.append(c.comp.gpa, .{ .label = c.builder.currentLabel, .value = cond });
            return;
        }
        try c.addBoolPhi(!c.boolInvert);
    }

    if (falseLabel == c.boolEndLabel)
        try c.addBoolPhi(c.boolInvert);

    return c.builder.addBranch(cond, trueLabel, falseLabel);
}

fn addBoolPhi(c: *CodeGen, value: bool) !void {
    const val = try c.builder.addConstant((try Value.int(@intFromBool(value), c.comp)).ref(), .i1);
    try c.phiNodes.append(c.comp.gpa, .{ .label = c.builder.currentLabel, .value = val });
}

fn genStmt(c: *CodeGen, node: Node.Index) Error!void {
    _ = try c.genExpr(node);
}

fn genExpr(c: *CodeGen, nodeIdx: Node.Index) Error!IR.Ref {
    if (c.tree.valueMap.get(nodeIdx)) |val|
        return c.builder.addConstant(val.ref(), try c.genType(nodeIdx.type(c.tree)));

    const node = nodeIdx.get(c.tree);
    switch (node) {
        .enumerationRef,
        .boolLiteral,
        .intLiteral,
        .charLiteral,
        .FloatLiteral,
        .imaginaryLiteral,
        .stringLiteralExpr,
        => unreachable, // These should have an entry in value_map.

        .fnDef,
        => unreachable,

        .staticAssert,
        .fnProto,
        .typeDef,
        .structDecl,
        .unionDecl,
        .enumDecl,
        .enumField,
        .recordField,
        .structForwardDecl,
        .unionForwardDecl,
        .enumForwardDecl,
        .nullStmt,
        => {},

        .variable => |variable| {
            if (variable.@"extern" or variable.static) {
                try c.genVar(variable);
                return .none;
            }
            const size: u32 = @intCast(variable.type.sizeof(c.comp).?);
            const @"align" = variable.type.alignof(c.comp);
            const alloc = try c.builder.addAlloc(size, @"align");
            const name = try StringInterner.intern(c.comp, c.tree.getTokenSlice(variable.nameToken));

            try c.symbols.append(c.comp.gpa, .{ .name = name, .value = alloc });

            if (variable.initializer) |init|
                try c.genInitializer(alloc, variable.type, init);
        },

        .labeledStmt => |labeled| {
            const label = try c.builder.makeLabel("label");
            try c.builder.startBlock(label);
            try c.genStmt(labeled.body);
        },

        .compoundStmt => |compound| {
            const oldSymLen = c.symbols.items.len;
            c.symbols.items.len = oldSymLen;

            for (compound.body) |stmt|
                try c.genStmt(stmt);
        },

        .ifStmt => |@"if"| {
            const thenLabel = try c.builder.makeLabel("if.then");
            const elseBody = @"if".elseBody orelse {
                const endLabel = try c.builder.makeLabel("if.end");
                try c.genBoolExpr(@"if".cond, thenLabel, endLabel);

                try c.builder.startBlock(thenLabel);
                try c.genStmt(@"if".thenBody);
                try c.builder.startBlock(endLabel);
                return .none;
            };

            const elseLabel = try c.builder.makeLabel("if.else");
            const endLabel = try c.builder.makeLabel("if.end");

            try c.genBoolExpr(@"if".cond, thenLabel, elseLabel);

            try c.builder.startBlock(thenLabel);
            try c.genStmt(@"if".thenBody); // then
            try c.builder.addJump(endLabel);

            try c.builder.startBlock(elseLabel);
            try c.genStmt(elseBody); // else

            try c.builder.startBlock(endLabel);
        },

        .switchStmt => |@"switch"| {
            var wipSwitch: WipSwitch = .{
                .size = @"switch".cond.type(c.tree).sizeof(c.comp).?,
            };
            defer wipSwitch.cases.deinit(c.builder.gpa);

            const oldWipSwitch = c.wipSwitch;
            defer c.wipSwitch = oldWipSwitch;
            c.wipSwitch = &wipSwitch;

            const oldBreakLabel = c.breakLabel;
            defer c.breakLabel = oldBreakLabel;

            const endRef = try c.builder.makeLabel("switch.end");
            c.breakLabel = endRef;

            const cond = try c.genExpr(@"switch".cond);
            const switchIndex = c.builder.instructions.len;
            _ = try c.builder.addInst(.Switch, undefined, .noreturn);

            try c.genStmt(@"switch".body); // body

            const defaultRef = wipSwitch.default orelse endRef;
            try c.builder.startBlock(endRef);

            const a = c.builder.arena.allocator();
            const switchData = try a.create(Inst.Switch);
            switchData.* = .{
                .target = cond,
                .casesLen = @intCast(wipSwitch.cases.len),
                .caseVals = (try a.dupe(Interner.Ref, wipSwitch.cases.items(.value))).ptr,
                .caseLabels = (try a.dupe(IR.Ref, wipSwitch.cases.items(.label))).ptr,
                .default = defaultRef,
            };
            c.builder.instructions.items(.data)[switchIndex] = .{ .@"switch" = switchData };
        },

        .caseStmt => |case| {
            if (case.end != null) return c.fail("TODO CodeGen.genStmt case range\n", .{});
            const val = c.tree.valueMap.get(case.start).?;
            const label = try c.builder.makeLabel("case");
            try c.builder.startBlock(label);
            try c.wipSwitch.cases.append(c.builder.gpa, .{
                .value = val.ref(),
                .label = label,
            });
            try c.genStmt(case.body);
        },

        .defaultStmt => |default| {
            const defaultLabel = try c.builder.makeLabel("default");
            try c.builder.startBlock(defaultLabel);
            c.wipSwitch.default = defaultLabel;
            try c.genStmt(default.body);
        },

        .whileStmt => |@"while"| {
            const oldBreakLabel = c.breakLabel;
            const oldContinueLabel = c.continueLabel;

            defer {
                c.breakLabel = oldBreakLabel;
                c.continueLabel = oldContinueLabel;
            }

            const condLabel = try c.builder.makeLabel("while.cond");
            const thenLabel = try c.builder.makeLabel("while.then");
            const endLabel = try c.builder.makeLabel("while.end");

            c.continueLabel = condLabel;
            c.breakLabel = endLabel;

            try c.builder.startBlock(condLabel);
            try c.genBoolExpr(@"while".cond, thenLabel, endLabel);

            try c.builder.startBlock(thenLabel);
            try c.genStmt(@"while".body);
            try c.builder.addJump(condLabel);

            try c.builder.startBlock(endLabel);
        },

        .doWhileStmt => |doWhile| {
            const oldBreakLabel = c.breakLabel;
            const oldContinueLabel = c.continueLabel;

            defer {
                c.breakLabel = oldBreakLabel;
                c.continueLabel = oldContinueLabel;
            }

            const thenLabel = try c.builder.makeLabel("do.then");
            const condLabel = try c.builder.makeLabel("do.cond");
            const endLabel = try c.builder.makeLabel("do.end");

            c.continueLabel = condLabel;
            c.breakLabel = endLabel;

            try c.builder.startBlock(thenLabel);
            try c.genStmt(doWhile.body);

            try c.builder.startBlock(condLabel);
            try c.genBoolExpr(doWhile.cond, thenLabel, endLabel);

            try c.builder.startBlock(endLabel);
        },

        .forStmt => |@"for"| {
            const oldBreakLabel = c.breakLabel;
            const oldContinueLabel = c.continueLabel;

            defer {
                c.breakLabel = oldBreakLabel;
                c.continueLabel = oldContinueLabel;
            }

            const cond = @"for".cond orelse {
                const thenLabel = try c.builder.makeLabel("for.then");
                const endLabel = try c.builder.makeLabel("for.end");

                c.continueLabel = thenLabel;
                c.breakLabel = endLabel;

                try c.builder.startBlock(thenLabel);
                try c.genStmt(@"for".body);
                if (@"for".incr) |incr| {
                    _ = try c.genExpr(incr);
                }
                try c.builder.addJump(thenLabel);
                try c.builder.startBlock(endLabel);

                return .none;
            };

            const thenLabel = try c.builder.makeLabel("for.then");
            var condLabel = thenLabel;
            const contLabel = try c.builder.makeLabel("for.cont");
            const endLabel = try c.builder.makeLabel("for.end");

            c.continueLabel = contLabel;
            c.breakLabel = endLabel;

            condLabel = try c.builder.makeLabel("for.cond");
            try c.builder.startBlock(condLabel);
            try c.genBoolExpr(cond, thenLabel, endLabel);

            try c.builder.startBlock(thenLabel);
            try c.genStmt(@"for".body);
            if (@"for".incr) |incr|
                _ = try c.genExpr(incr);

            try c.builder.addJump(condLabel);
            try c.builder.startBlock(endLabel);
        },

        .continueStmt => try c.builder.addJump(c.continueLabel),
        .breakStmt => try c.builder.addJump(c.breakLabel),

        .returnStmt => |@"return"| {
            if (@"return".expr) |expr| {
                const operand = try c.genExpr(expr);
                try c.retNodes.append(c.comp.gpa, .{ .value = operand, .label = c.builder.currentLabel });
            }
            try c.builder.addJump(c.returnLabel);
        },

        .implicitReturn => |implicitRet| {
            if (implicitRet.zero) {
                const operand = try c.builder.addConstant(.zero, try c.genType(implicitRet.returnType));
                try c.retNodes.append(c.comp.gpa, .{ .value = operand, .label = c.builder.currentLabel });
            }
            // No need to emit a jump since implicit_return is always the last instruction.
        },

        .gotoStmt,
        .computedGotoStmt,
        .nullptrLiteral,
        => return c.fail("TODO CodeGen.genStmt {}\n", .{@tagName(node)}),

        .commaExpr => |bin| {
            _ = try c.genExpr(bin.lhs);
            return c.genExpr(bin.rhs);
        },

        .assignExpr => |bin| {
            const rhs = try c.genExpr(bin.rhs);
            const lhs = try c.genLval(bin.lhs);
            try c.builder.addStore(lhs, rhs);
            return rhs;
        },

        .mulAssignExpr => |bin| return c.genCompoundAssign(bin, .Mul),
        .divAssignExpr => |bin| return c.genCompoundAssign(bin, .Div),
        .modAssignExpr => |bin| return c.genCompoundAssign(bin, .Mod),
        .addAssignExpr => |bin| return c.genCompoundAssign(bin, .Add),
        .subAssignExpr => |bin| return c.genCompoundAssign(bin, .Sub),
        .shlAssignExpr => |bin| return c.genCompoundAssign(bin, .BitShl),
        .shrAssignExpr => |bin| return c.genCompoundAssign(bin, .BitShr),
        .bitAndAssignExpr => |bin| return c.genCompoundAssign(bin, .BitAnd),
        .bitXorAssignExpr => |bin| return c.genCompoundAssign(bin, .BitXor),
        .bitOrAssignExpr => |bin| return c.genCompoundAssign(bin, .BitOr),

        .bitOrExpr => |bin| return c.genBinOp(bin, .BitOr),
        .bitXorExpr => |bin| return c.genBinOp(bin, .BitXor),
        .bitAndExpr => |bin| return c.genBinOp(bin, .BitAnd),

        .equalExpr => |bin| {
            const cmp = try c.genComparison(node, .CmpEQ);
            return c.addUn(.Zext, cmp, bin.type);
        },

        .notEqualExpr => |bin| {
            const cmp = try c.genComparison(node, .CmpNE);
            return c.addUn(.Zext, cmp, bin.type);
        },

        .lessThanExpr => |bin| {
            const cmp = try c.genComparison(node, .CmpLT);
            return c.addUn(.Zext, cmp, bin.type);
        },

        .lessThanEqualExpr => |bin| {
            const cmp = try c.genComparison(node, .CmpLTE);
            return c.addUn(.Zext, cmp, bin.type);
        },

        .greaterThanExpr => |bin| {
            const cmp = try c.genComparison(node, .CmpGT);
            return c.addUn(.Zext, cmp, bin.type);
        },

        .greaterThanEqualExpr => |bin| {
            const cmp = try c.genComparison(node, .CmpGTE);
            return c.addUn(.Zext, cmp, bin.type);
        },

        .shlExpr => |bin| return c.genBinOp(bin, .BitShl),
        .shrExpr => |bin| return c.genBinOp(bin, .BitShr),

        .addExpr => |bin| {
            if (bin.type.isPointer()) {
                const lhsTy = bin.lhs.type(c.tree);
                if (lhsTy.isPointer()) {
                    const ptr = try c.genExpr(bin.lhs);
                    const offset = try c.genExpr(bin.rhs);
                    return c.genPtrArithmetic(ptr, offset, bin.rhs.type(c.tree), bin.type);
                } else {
                    const offset = try c.genExpr(bin.lhs);
                    const ptr = try c.genExpr(bin.rhs);
                    const offsetTy = lhsTy;
                    return c.genPtrArithmetic(ptr, offset, offsetTy, bin.type);
                }
            }
            return c.genBinOp(bin, .Add);
        },

        .subExpr => |bin| {
            if (bin.type.isPointer()) {
                const ptr = try c.genExpr(bin.lhs);
                const offset = try c.genExpr(bin.rhs);
                return c.genPtrArithmetic(ptr, offset, bin.rhs.type(c.tree), bin.type);
            }
            return c.genBinOp(node, .Sub);
        },

        .mulExpr => |bin| return c.genBinOp(bin, .Mul),
        .divExpr => |bin| return c.genBinOp(bin, .Div),
        .modExpr => |bin| return c.genBinOp(bin, .Mod),

        .addrOfExpr => |un| return try c.genLval(un.operand),
        .derefExpr => |un| {
            const operandNode = un.operand.get(c.tree);
            if (operandNode == .ImplicitCast and
                operandNode.implicitCast.kind == .FunctionToPointer)
                return c.genExpr(un.operand);

            const operand = try c.genLval(un.operand);
            return c.addUn(.Load, operand, un.type);
        },

        .plusExpr => |un| return c.genExpr(un.operand),
        .negateExpr => |un| {
            const zero = try c.builder.addConstant(.zero, try c.genType(un.type));
            const operand = try c.genExpr(un.operand);
            return c.addBin(.Sub, zero, operand, un.type);
        },

        .bitNotExpr => |un| {
            const operand = try c.genExpr(un.operand);
            return c.addUn(.BitNot, operand, un.type);
        },
        .boolNotExpr => |un| {
            const zero = try c.builder.addConstant(.zero, try c.genType(un.type));
            const operand = try c.genExpr(un.operand);
            return c.addBin(.CmpNE, zero, operand, un.type);
        },

        .preIncExpr => |un| {
            const operand = try c.genLval(un.operand);
            const val = try c.addUn(.Load, operand, un.type);
            const one = try c.builder.addConstant(.one, try c.genType(un.type));
            const plusOne = try c.addBin(.Add, val, one, un.type);
            try c.builder.addStore(operand, plusOne);
            return plusOne;
        },
        .preDecExpr => |un| {
            const operand = try c.genLval(un.operand);
            const val = try c.addUn(.Load, operand, un.type);
            const one = try c.builder.addConstant(.one, try c.genType(un.type));
            const decOne = try c.addBin(.Sub, val, one, un.type);
            try c.builder.addStore(operand, decOne);
            return decOne;
        },

        .postIncExpr => |un| {
            const operand = try c.genLval(un.operand);
            const val = try c.addUn(.Load, operand, un.type);
            const one = try c.builder.addConstant(.one, try c.genType(un.type));
            const plusOne = try c.addBin(.Add, val, one, un.type);
            try c.builder.addStore(operand, plusOne);
            return val;
        },
        .postDecExpr => |un| {
            const operand = try c.genLval(un.operand);
            const val = try c.addUn(.Load, operand, un.type);
            const one = try c.builder.addConstant(.one, try c.genType(un.type));
            const decOne = try c.addBin(.Sub, val, one, un.type);
            try c.builder.addStore(operand, decOne);
            return val;
        },

        .parenExpr => |un| return c.genExpr(un.operand),
        .declRefExpr => unreachable, // Lval expression.
        .explicitCast, .implicitCast => |cast| switch (cast.kind) {
            .NoOP => return c.genExpr(cast.operand),
            .ToVoid => {
                _ = try c.genExpr(cast.operand);
                return .none;
            },

            .LValToRVal => {
                const operand = try c.genLval(cast.operand);
                return c.addUn(.Load, operand, cast.type);
            },

            .FunctionToPointer, .ArrayToPointer => {
                return c.genLval(cast.operand);
            },

            .IntCast => {
                const operand = try c.genExpr(cast.operand);
                const srcTy = cast.operand.type(c.tree);
                const srcBits = srcTy.bitSizeof(c.comp).?;
                const destBits = cast.type.bitSizeof(c.comp).?;
                if (srcBits == destBits) {
                    return operand;
                } else if (srcBits < destBits) {
                    if (srcTy.isUnsignedInt(c.comp))
                        return c.addUn(.Zext, operand, cast.type)
                    else
                        return c.addUn(.Sext, operand, cast.type);
                } else {
                    return c.addUn(.Trunc, operand, cast.type);
                }
            },

            .BoolToInt => {
                const operand = try c.genExpr(cast.operand);
                return c.addUn(.Zext, operand, cast.type);
            },

            .PointerToBool, .IntToBool, .FloatToBool => {
                const lhs = try c.genExpr(cast.operand);
                const rhs = try c.builder.addConstant(.zero, try c.genType(cast.type));
                return c.builder.addInst(.CmpNE, .{ .bin = .{ .lhs = lhs, .rhs = rhs } }, .i1);
            },

            .Bitcast,
            .FloatCast,
            .PointerToInt,
            .IntToPointer,
            .BoolToFloat,
            .BoolToPointer,
            .IntToFloat,
            .FloatToInt,
            .ComplexIntToComplexFloat,
            .ComplexFloatToComplexInt,
            .ComplexIntCast,
            .ComplexIntToReal,
            .RealToComplexInt,
            .ComplexFloatCast,
            .ComplexFloatToReal,
            .RealToComplexFloat,
            .NullToPointer,
            .UnionCast,
            .VectorSplat,
            => return c.fail("TODO CodeGen gen CastKind {}\n", .{cast.kind}),
        },

        .binaryCondExpr => |conditional| {
            if (c.tree.valueMap.get(conditional.cond)) |cond| {
                if (cond.toBool(c.comp)) {
                    c.condDummyRef = try c.genExpr(conditional.cond);
                    return c.genExpr(conditional.thenExpr); // then
                } else {
                    return c.genExpr(conditional.thenExpr); // else
                }
            }

            const thenLabel = try c.builder.makeLabel("ternary.then");
            const elseLabel = try c.builder.makeLabel("ternary.else");
            const endLabel = try c.builder.makeLabel("ternary.end");
            const condTy = conditional.cond.type(c.tree);
            {
                const oldCondDummyTy = c.condDummyTy;
                defer c.condDummyTy = oldCondDummyTy;
                c.condDummyTy = try c.genType(condTy);

                try c.genBoolExpr(conditional.cond, thenLabel, elseLabel);
            }

            try c.builder.startBlock(thenLabel);
            if (c.builder.instructions.items(.type)[@intFromEnum(c.condDummyRef)] == .i1)
                c.condDummyRef = try c.addUn(.Zext, c.condDummyRef, condTy);

            const thenVal = try c.genExpr(conditional.thenExpr); // then
            try c.builder.addJump(endLabel);
            const thenExit = c.builder.currentLabel;

            try c.builder.startBlock(elseLabel);
            const elseVal = try c.genExpr(conditional.elseExpr); // else
            const elseExit = c.builder.currentLabel;

            try c.builder.startBlock(endLabel);

            var phiBuffer: [2]Inst.Phi.Input = .{
                .{ .value = thenVal, .label = thenExit },
                .{ .value = elseVal, .label = elseExit },
            };
            return c.builder.addPhi(&phiBuffer, try c.genType(conditional.type));
        },

        .condDummyExpr => return c.condDummyRef,

        .condExpr => |conditional| {
            if (c.tree.valueMap.get(conditional.cond)) |cond| {
                if (cond.toBool(c.comp)) {
                    return c.genExpr(conditional.thenExpr); // then
                } else {
                    return c.genExpr(conditional.elseExpr); // else
                }
            }

            const thenLabel = try c.builder.makeLabel("ternary.then");
            const elseLabel = try c.builder.makeLabel("ternary.else");
            const endLabel = try c.builder.makeLabel("ternary.end");

            try c.genBoolExpr(conditional.cond, thenLabel, elseLabel);

            try c.builder.startBlock(thenLabel);
            const thenVal = try c.genExpr(conditional.thenExpr); // then
            try c.builder.addJump(endLabel);
            const thenExit = c.builder.currentLabel;

            try c.builder.startBlock(elseLabel);
            const elseVal = try c.genExpr(conditional.elseExpr); // else
            const elseExit = c.builder.currentLabel;

            try c.builder.startBlock(endLabel);

            var phiBuffer: [2]Inst.Phi.Input = .{
                .{ .value = thenVal, .label = thenExit },
                .{ .value = elseVal, .label = elseExit },
            };
            return c.builder.addPhi(&phiBuffer, try c.genType(conditional.type));
        },

        .callExpr => |call| return c.genCall(call),

        .boolOrExpr => |bin| {
            if (c.tree.valueMap.get(bin.lhs)) |lhs| {
                if (!lhs.toBool(c.comp))
                    return c.builder.addConstant(.zero, try c.genType(bin.type));

                return c.genExpr(bin.rhs);
            }

            const falseLabel = try c.builder.makeLabel("bool_false");
            const exitLabel = try c.builder.makeLabel("bool_exit");

            const oldBoolEndLabel = c.boolEndLabel;
            defer c.boolEndLabel = oldBoolEndLabel;
            c.boolEndLabel = exitLabel;

            const phiNodesTop = c.phiNodes.items.len;
            defer c.phiNodes.items.len = phiNodesTop;

            try c.genBoolExpr(bin.lhs, exitLabel, falseLabel);

            try c.builder.startBlock(falseLabel);
            try c.genBoolExpr(bin.rhs, exitLabel, exitLabel);

            try c.builder.startBlock(exitLabel);

            const phi = try c.builder.addPhi(c.phiNodes.items[phiNodesTop..], .i1);
            return c.addUn(.Zext, phi, bin.type);
        },

        .boolAndExpr => |bin| {
            if (c.tree.valueMap.get(bin.lhs)) |lhs| {
                if (!lhs.toBool(c.comp))
                    return c.builder.addConstant(.zero, try c.genType(bin.type));

                return c.genExpr(bin.rhs);
            }
            const trueLabel = try c.builder.makeLabel("bool_true");
            const exitLabel = try c.builder.makeLabel("bool_exit");

            const oldBoolEndLabel = c.boolEndLabel;
            defer c.boolEndLabel = oldBoolEndLabel;
            c.boolEndLabel = exitLabel;

            const phiNodesTop = c.phiNodes.items.len;
            defer c.phiNodes.items.len = phiNodesTop;

            try c.genBoolExpr(bin.lhs, trueLabel, exitLabel);

            try c.builder.startBlock(trueLabel);
            try c.genBoolExpr(bin.rhs, exitLabel, exitLabel);

            try c.builder.startBlock(exitLabel);

            const phi = try c.builder.addPhi(c.phiNodes.items[phiNodesTop..], .i1);
            return c.addUn(.Zext, phi, bin.type);
        },

        .builtinChooseExpr => |conditional| {
            const cond = c.tree.valueMap.get(conditional.cond).?;
            if (cond.toBool(c.comp))
                return c.genExpr(conditional.thenExpr)
            else
                return c.genExpr(conditional.elseExpr);
        },

        .genericExpr => |generic| {
            const chosen = generic.chosen.get(c.tree);
            switch (chosen) {
                .genericAssociationExpr => |assoc| {
                    return c.genExpr(assoc.expr);
                },
                .genericDefaultExpr => |default| {
                    return c.genExpr(default.epxr);
                },
                else => unreachable,
            }
        },

        .genericAssociationExpr, .genericDefaultExpr => unreachable,

        .stmtExpr => |un| {
            const compoundStmt = un.operand.get(c.tree).compoundStmt;

            const oldSymLen = c.symbols.items.len;
            c.symbols.items.len = oldSymLen;

            for (compoundStmt.body[0 .. compoundStmt.body.len - 1]) |stmt|
                try c.genStmt(stmt);
            return c.genExpr(compoundStmt.body[compoundStmt.body.len - 1]);
        },

        .AddrOfLabel,
        .ImagExpr,
        .RealExpr,
        .BuiltinCallExprOne,
        .BuiltinCallExpr,
        .SizeOfExpr,
        => return c.fail("TODO CodeGen.genExpr {}\n", .{@tagName(node)}),

        else => unreachable, // Not an expression.
    }

    return .none;
}

fn genLval(c: *CodeGen, nodeIndex: Node.Index) Error!IR.Ref {
    assert(c.tree.isLValue(nodeIndex));

    const node = nodeIndex.get(c.tree);
    switch (node) {
        .stringLiteralExpr => {
            const val = c.tree.valueMap.get(nodeIndex).?;
            return c.builder.addConstant(val.ref(), .ptr);
        },

        .parenExpr => |un| return c.genLval(un.operand),

        .declRefExpr => |declRef| {
            const slice = c.tree.getTokenSlice(declRef.nameToken);
            const name = try StringInterner.intern(c.comp, slice);
            var i = c.symbols.items.len;
            while (i > 0) {
                i -= 1;
                if (c.symbols.items[i].name == name) {
                    return c.symbols.items[i].value;
                }
            }

            const dupedName = try c.builder.arena.allocator().dupeZ(u8, slice);
            const ref: IR.Ref = @enumFromInt(c.builder.instructions.len);
            try c.builder.instructions.append(
                c.builder.gpa,
                .{ .tag = .Symbol, .data = .{ .label = dupedName }, .type = .ptr },
            );
            return ref;
        },

        .derefExpr => |un| return c.genExpr(un.operand),

        .compoundLiteralExpr => |literal| {
            if (literal.static or literal.thread_local) {
                return c.fail("TODO CodeGen.compound_literal_expr static or thread_local\n", .{});
            }
            const size: u32 = @intCast(literal.type.sizeof(c.comp).?); // TODO add error in parser
            const @"align" = literal.type.alignof(c.comp);
            const alloc = try c.builder.addAlloc(size, @"align");
            try c.genInitializer(alloc, literal.type, literal.initializer);
            return alloc;
        },

        .builtinChooseExpr => |conditional| {
            const cond = c.tree.valueMap.get(conditional.cond).?;
            if (cond.toBool(c.comp))
                return c.genLval(conditional.thenExpr)
            else
                return c.genLval(conditional.elseExpr);
        },

        .memberAccessExpr,
        .memberAccessPtrExpr,
        .arrayAccessExpr,
        => return c.fail("TODO CodeGen.genLval {}\n", .{@tagName(node)}),

        else => unreachable, // not an lvalue expression.
    }
}

fn genBoolExpr(c: *CodeGen, base: Node.Index, trueLabel: IR.Ref, falseLabel: IR.Ref) Error!void {
    var node = base;
    while (true) {
        switch (node.get(c.tree)) {
            .parenExpr => |un| node = un.operand,
            else => break,
        }
    }

    switch (node.get(c.tree)) {
        .boolOrExpr => |bin| {
            if (c.tree.valueMap.get(bin.lhs)) |lhs| {
                if (lhs.toBool(c.comp)) {
                    if (trueLabel == c.boolEndLabel) {
                        return c.addBoolPhi(!c.boolInvert);
                    }
                    return c.builder.addJump(trueLabel);
                }
                return c.genBoolExpr(bin.rhs, trueLabel, falseLabel);
            }

            const newFalseLabel = try c.builder.makeLabel("bool_false");
            try c.genBoolExpr(bin.lhs, trueLabel, newFalseLabel);
            try c.builder.startBlock(newFalseLabel);

            if (c.condDummyTy) |ty| c.condDummyRef = try c.builder.addConstant(.one, ty);
            return c.genBoolExpr(bin.rhs, trueLabel, falseLabel);
        },

        .boolAndExpr => |bin| {
            if (c.tree.valueMap.get(bin.lhs)) |lhs| {
                if (!lhs.toBool(c.comp)) {
                    if (falseLabel == c.boolEndLabel) {
                        return c.addBoolPhi(c.boolInvert);
                    }
                    return c.builder.addJump(falseLabel);
                }
                return c.genBoolExpr(bin.rhs, trueLabel, falseLabel);
            }

            const newTrueLabel = try c.builder.makeLabel("bool_true");
            try c.genBoolExpr(bin.lhs, newTrueLabel, falseLabel);
            try c.builder.startBlock(newTrueLabel);

            if (c.condDummyTy) |ty| c.condDummyRef = try c.builder.addConstant(.one, ty);
            return c.genBoolExpr(bin.rhs, trueLabel, falseLabel);
        },

        .boolNotExpr => |un| {
            c.boolInvert = !c.boolInvert;
            defer c.boolInvert = !c.boolInvert;

            if (c.condDummyTy) |ty| c.condDummyRef = try c.builder.addConstant(.zero, ty);
            return c.genBoolExpr(un.operand, trueLabel, falseLabel);
        },

        .equalExpr => |bin| {
            const cmp = try c.genComparison(bin, .CmpEQ);
            if (c.condDummyTy != null) c.condDummyRef = cmp;
            return c.addBranch(cmp, trueLabel, falseLabel);
        },

        .notEqualExpr => |bin| {
            const cmp = try c.genComparison(bin, .CmpNE);
            if (c.condDummyTy != null) c.condDummyRef = cmp;
            return c.addBranch(cmp, trueLabel, falseLabel);
        },

        .lessThanExpr => |bin| {
            const cmp = try c.genComparison(bin, .CmpLT);
            if (c.condDummyTy != null) c.condDummyRef = cmp;
            return c.addBranch(cmp, trueLabel, falseLabel);
        },

        .lessThanEqualExpr => |bin| {
            const cmp = try c.genComparison(bin, .CmpLTE);
            if (c.condDummyTy != null) c.condDummyRef = cmp;
            return c.addBranch(cmp, trueLabel, falseLabel);
        },

        .greaterThanExpr => |bin| {
            const cmp = try c.genComparison(bin, .CmpGT);
            if (c.condDummyTy != null) c.condDummyRef = cmp;
            return c.addBranch(cmp, trueLabel, falseLabel);
        },

        .greaterThanEqualExpr => |bin| {
            const cmp = try c.genComparison(bin, .CmpGTE);
            if (c.condDummyTy != null) c.condDummyRef = cmp;
            return c.addBranch(cmp, trueLabel, falseLabel);
        },

        .explicitCast, .implicitCast => |cast| switch (cast.kind) {
            .BoolToInt => {
                const operand = try c.genExpr(cast.operand);
                if (c.condDummyTy != null) c.condDummyRef = operand;
                return c.addBranch(operand, trueLabel, falseLabel);
            },
            else => {},
        },

        .binaryCondExpr => |conditional| {
            if (c.tree.valueMap.get(conditional.cond)) |cond| {
                if (cond.toBool(c.comp)) {
                    return c.genBoolExpr(conditional.thenExpr, trueLabel, falseLabel); // then
                } else {
                    return c.genBoolExpr(conditional.elseExpr, trueLabel, falseLabel); // else
                }
            }

            const newFalseLabel = try c.builder.makeLabel("ternary.else");
            try c.genBoolExpr(conditional.cond, trueLabel, newFalseLabel);

            try c.builder.startBlock(newFalseLabel);
            if (c.condDummyTy) |ty| c.condDummyRef = try c.builder.addConstant(.one, ty);
            return c.genBoolExpr(conditional.elseExpr, trueLabel, falseLabel); // else
        },

        .condExpr => |conditional| {
            if (c.tree.valueMap.get(conditional.cond)) |cond| {
                if (cond.toBool(c.comp)) {
                    return c.genBoolExpr(conditional.thenExpr, trueLabel, falseLabel); // then
                } else {
                    return c.genBoolExpr(conditional.elseExpr, trueLabel, falseLabel); // else
                }
            }

            const newTrueLabel = try c.builder.makeLabel("ternary.then");
            const newFalseLabel = try c.builder.makeLabel("ternary.else");
            try c.genBoolExpr(conditional.cond, newTrueLabel, newFalseLabel);

            try c.builder.startBlock(newTrueLabel);
            try c.genBoolExpr(conditional.thenExpr, trueLabel, falseLabel); // then
            try c.builder.startBlock(newFalseLabel);
            if (c.condDummyTy) |ty| c.condDummyRef = try c.builder.addConstant(.one, ty);
            return c.genBoolExpr(conditional.elseExpr, trueLabel, falseLabel); // else
        },

        else => {},
    }

    if (c.tree.valueMap.get(node)) |value| {
        if (value.toBool(c.comp)) {
            if (trueLabel == c.boolEndLabel) {
                return c.addBoolPhi(!c.boolInvert);
            }
            return c.builder.addJump(trueLabel);
        } else {
            if (falseLabel == c.boolEndLabel) {
                return c.addBoolPhi(c.boolInvert);
            }
            return c.builder.addJump(falseLabel);
        }
    }

    // Assume int operand.
    const lhs = try c.genExpr(node);
    const rhs = try c.builder.addConstant(.zero, try c.genType(node.type(c.tree)));
    const cmp = try c.builder.addInst(.CmpNE, .{ .bin = .{ .lhs = lhs, .rhs = rhs } }, .i1);
    if (c.condDummyTy != null) c.condDummyRef = cmp;
    try c.addBranch(cmp, trueLabel, falseLabel);
}

fn genCall(c: *CodeGen, call: Node.Call) Error!IR.Ref {
    // Detect direct calls.
    const fnRef = blk: {
        const callee = call.callee.get(c.tre);
        if (callee != .ImplicitCast or callee.implicitCast.kind != .FunctionToPointer)
            break :blk try c.genExpr(call.callee);

        var cur = callee.implicitCast.operand;
        while (true) switch (cur.get(c.tree)) {
            .parenExpr, .addrOfExpr, .derefExpr => |un| cur = un.operand,

            .implicitCast => |cast| {
                if (cast.kind != .FunctionToPointer) {
                    break :blk try c.genExpr(call.callee);
                }
                cur = cast.operand;
            },

            .declRefExpr => |declRef| {
                const slice = c.tree.getTokenSlice(declRef.nameToken);
                const name = try StringInterner.intern(c.comp, slice);
                var i = c.symbols.items.len;
                while (i > 0) {
                    i -= 1;
                    if (c.symbols.items[i].name == name) {
                        break :blk try c.genExpr(call.callee);
                    }
                }

                const dupedName = try c.builder.arena.allocator().dupeZ(u8, slice);
                const ref: IR.Ref = @enumFromInt(c.builder.instructions.len);
                try c.builder.instructions.append(
                    c.builder.gpa,
                    .{ .tag = .Symbol, .data = .{ .label = dupedName }, .type = .ptr },
                );
                break :blk ref;
            },
            else => break :blk try c.genExpr(call.callee),
        };
    };

    const args = try c.builder.arena.allocator().alloc(IR.Ref, call.args.len);
    for (call.args, args) |node, *arg| {
        // TODO handle calling convention here
        arg.* = try c.genExpr(node);
    }

    // TODO handle variadic call
    const callInst = try c.builder.arena.allocator().create(Inst.Call);
    callInst.* = .{
        .func = fnRef,
        .argsLen = @intCast(args.len),
        .argsPtr = args.ptr,
    };
    return c.builder.addInst(.Call, .{ .call = callInst }, try c.genType(call.type));
}

fn genCompoundAssign(c: *CodeGen, bin: Node.BinaryExpr, tag: Inst.Tag) Error!IR.Ref {
    const rhs = try c.genExpr(bin.rhs);
    const lhs = try c.genLval(bin.lhs);
    const res = try c.addBin(tag, lhs, rhs, bin.type);

    try c.builder.addStore(lhs, res);
    return res;
}

fn genBinOp(c: *CodeGen, bin: Node.BinaryExpr, tag: Inst.Tag) Error!IR.Ref {
    const lhs = try c.genExpr(bin.lhs);
    const rhs = try c.genExpr(bin.rhs);
    return c.addBin(tag, lhs, rhs, bin.type);
}

fn genComparison(c: *CodeGen, bin: Node.BinaryExpr, tag: Inst.Tag) Error!IR.Ref {
    const lhs = try c.genExpr(bin.lhs);
    const rhs = try c.genExpr(bin.rhs);

    return c.builder.addInst(tag, .{ .bin = .{ .lhs = lhs, .rhs = rhs } }, .i1);
}

fn genPtrArithmetic(c: *CodeGen, ptr: IR.Ref, offset: IR.Ref, offsetTy: Type, ty: Type) Error!IR.Ref {
    // TODO consider adding a getelemptr instruction
    const size = ty.getElemType().sizeof(c.comp).?;
    if (size == 1)
        return c.builder.addInst(.Add, .{ .bin = .{ .lhs = ptr, .rhs = offset } }, try c.genType(ty));

    const sizeInst = try c.builder.addConstant((try Value.int(size, c.comp)).ref(), try c.genType(offsetTy));
    const offsetInst = try c.addBin(.Mul, offset, sizeInst, offsetTy);
    return c.addBin(.Add, ptr, offsetInst, offsetTy);
}

fn genInitializer(c: *CodeGen, ptr: IR.Ref, destTy: Type, initializer: Node.Index) Error!void {
    const node = initializer.get(c.tree);
    switch (node) {
        .arrayInitExpr,
        .structInitExpr,
        .unionInitExpr,
        .arrayFillerExpr,
        .defaultInitExpr,
        => return c.fail("TODO CodeGen.genInitializer {}\n", .{@tagName(node)}),

        .stringLiteralExpr => {
            const val = c.tree.valueMap.get(initializer).?;
            const strPtr = try c.builder.addConstant(val.ref(), .ptr);
            if (destTy.isArray()) {
                return c.fail("TODO memcpy\n", .{});
            } else {
                try c.builder.addStore(ptr, strPtr);
            }
        },

        else => {
            const res = try c.genExpr(initializer);
            try c.builder.addStore(ptr, res);
        },
    }
}

fn genVar(c: *CodeGen, _: Node.Variable) Error!void {
    return c.fail("TODO CodeGen.genVar\n", .{});
}
