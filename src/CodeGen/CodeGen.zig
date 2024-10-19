const std = @import("std");
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const Compilation = @import("../Basic/Compilation.zig");
const Tree = @import("../AST/AST.zig");
const TreeTag = @import("../AST/AstTag.zig").Tag;
const NodeIndex = Tree.NodeIndex;
const IR = @import("IR.zig");
const IRBuilder = IR.Builder;
const Inst = IR.Inst;
const Type = @import("../AST/Type.zig");
const Interner = @import("Interner.zig");
const Value = @import("../AST/Value.zig");
const StringId = @import("../Basic/StringInterner.zig").StringId;

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

tree: Tree,
comp: *Compilation,
builder: IRBuilder,
nodeTag: []const TreeTag,
nodeData: []const Tree.Node.Data,
nodeTy: []const Type,
wipSwitch: *WipSwitch = undefined,
symbols: std.ArrayListUnmanaged(Symbol) = .{},
retNodes: std.ArrayListUnmanaged(IR.Inst.Phi.Input) = .{},
phiNodes: std.ArrayListUnmanaged(IR.Inst.Phi.Input) = .{},
recordElemBuffer: std.ArrayListUnmanaged(Interner.Ref) = .{},

condDummyTy: ?Interner.Ref = null,
boolInvert: bool = false,
boolEndLabel: IR.Ref = .none,
condDummyRef: IR.Ref = undefined,
continueLabel: IR.Ref = undefined,
breakLabel: IR.Ref = undefined,
returnLabel: IR.Ref = undefined,

pub fn generateTree(comp: *Compilation, tree: Tree) Compilation.Error!void {
    var c = CodeGen{
        .builder = .{
            .gpa = comp.gpa,
            .arena = std.heap.ArenaAllocator.init(comp.gpa),
        },
        .tree = tree,
        .comp = comp,
        .nodeTag = tree.nodes.items(.tag),
        .nodeData = tree.nodes.items(.data),
        .nodeTy = tree.nodes.items(.type),
    };
    defer c.symbols.deinit(c.comp.gpa);
    defer c.retNodes.deinit(c.comp.gpa);
    defer c.phiNodes.deinit(c.comp.gpa);
    defer c.recordElemBuffer.deinit(c.comp.gpa);
    defer c.builder.deinit();

    const nodeTags = tree.nodes.items(.tag);
    for (tree.rootDecls) |decl| {
        c.builder.arena.deinit();
        c.builder.arena = std.heap.ArenaAllocator.init(comp.gpa);

        switch (nodeTags[@intFromEnum(decl)]) {
            .StaticAssert,
            .TypeDef,
            .StructDeclTwo,
            .UnionDeclTwo,
            .EnumDeclTwo,
            .StructDecl,
            .UnionDecl,
            .EnumDecl,
            => {},

            .FnProto,
            .StaticFnProto,
            .InlineFnProto,
            .InlineStaticFnProto,
            .ExternVar,
            .ThreadlocalExternVar,
            => {},

            .FnDef,
            .StaticFnDef,
            .InlineFnDef,
            .InlineStaticFnDef,
            => c.genFn(decl) catch |err| switch (err) {
                error.FatalError => return error.FatalError,
                error.OutOfMemory => return error.OutOfMemory,
            },

            .Var,
            .StaticVar,
            .ThreadlocalVar,
            .ThreadlocalStaticVar,
            => c.genVar(decl) catch |err| switch (err) {
                error.FatalError => return error.FatalError,
                error.OutOfMemory => return error.OutOfMemory,
            },
            else => unreachable,
        }
    }
}

fn genType(c: *CodeGen, baseTy: Type) !Interner.Ref {
    var key: Interner.Key = undefined;
    const ty = baseTy.canonicalize(.standard);
    switch (ty.specifier) {
        .Void => return .void,
        .Bool => return .i1,
        .Struct => {
            key = .{
                .recordTy = .{
                    .userPtr = ty.data.record,
                    .elements = undefined, // Not needed for hash lookup.
                },
            };

            if (c.builder.pool.has(key)) |some|
                return some;

            const elemBufferTop = c.recordElemBuffer.items.len;
            defer c.recordElemBuffer.items.len = elemBufferTop;

            for (ty.data.record.fields) |field| {
                if (!field.isRegularField())
                    return c.comp.diagnostics.fatalNoSrc("TODO lower struct bitfields", .{});

                // TODO handle padding bits
                const fieldRef = try c.genType(field.ty);
                try c.recordElemBuffer.append(c.builder.gpa, fieldRef);
            }

            key.recordTy.elements = try c.builder.arena.allocator().dupe(Interner.Ref, c.recordElemBuffer.items[elemBufferTop..]);
            return c.builder.pool.put(c.builder.gpa, key);
        },

        .Union => {
            return c.comp.diagnostics.fatalNoSrc("TODO lower union types", .{});
        },

        else => {},
    }

    if (ty.isPointer())
        return .ptr;

    if (ty.isFunc())
        return .func;

    if (!ty.isReal())
        return c.comp.diagnostics.fatalNoSrc("TODO lower complex types", .{});

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
        return c.comp.diagnostics.fatalNoSrc("TODO lower nullptr_t", .{});
    }

    return c.builder.pool.put(c.builder.gpa, key);
}

fn genFn(c: *CodeGen, decl: NodeIndex) Error!void {
    const name = c.tree.getTokenSlice(c.nodeData[@intFromEnum(decl)].decl.name);
    const funcTy = c.nodeTy[@intFromEnum(decl)].canonicalize(.standard);
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
    try c.genStmt(c.nodeData[@intFromEnum(decl)].decl.node);

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

    var res = IR{
        .pool = c.builder.pool,
        .instructions = c.builder.instructions,
        .arena = c.builder.arena.state,
        .body = c.builder.body,
        .strings = c.tree.strings,
    };

    res.dump(c.builder.gpa, name, c.comp.diagnostics.color, std.io.getStdOut().writer()) catch {};
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
    const val = try c.builder.addConstant(Value.int(@intFromBool(value)), .i1);
    try c.phiNodes.append(c.comp.gpa, .{ .label = c.builder.currentLabel, .value = val });
}

fn genStmt(c: *CodeGen, node: NodeIndex) Error!void {
    _ = try c.genExpr(node);
}

fn genExpr(c: *CodeGen, node: NodeIndex) Error!IR.Ref {
    assert(node != .none);

    const ty = c.nodeTy[@intFromEnum(node)];
    if (c.tree.valueMap.get(node)) |val|
        return c.builder.addConstant(val, try c.genType(ty));

    const data = c.nodeData[@intFromEnum(node)];
    const tag = c.nodeTag[@intFromEnum(node)];

    switch (tag) {
        .EnumerationRef,
        .BoolLiteral,
        .IntLiteral,
        .CharLiteral,
        .FloatLiteral,
        .DoubleLiteral,
        .ImaginaryLiteral,
        .StringLiteralExpr,
        => unreachable, // These should have an entry in value_map.

        .Invalid,
        .FnDef,
        .StaticFnDef,
        .InlineFnDef,
        .InlineStaticFnDef,
        .ThreadlocalVar,
        => unreachable,

        .StaticAssert,
        .FnProto,
        .StaticFnProto,
        .InlineFnProto,
        .InlineStaticFnProto,
        .ExternVar,
        .ThreadlocalExternVar,
        .TypeDef,
        .StructDeclTwo,
        .UnionDeclTwo,
        .EnumDeclTwo,
        .StructDecl,
        .UnionDecl,
        .EnumDecl,
        .EnumFieldDecl,
        .RecordFieldDecl,
        .IndirectRecordFieldDecl,
        .StructForwardDecl,
        .UnionForwardDecl,
        .EnumForwardDecl,
        .NullStmt,
        => {},

        .StaticVar,
        .ImplicitStaticVar,
        .ThreadlocalStaticVar,
        => try c.genVar(node),

        .Var => {
            const size: u32 = @intCast(ty.sizeof(c.comp).?);
            const @"align" = ty.alignof(c.comp);
            const alloc = try c.builder.addAlloc(size, @"align");
            const name = try c.comp.intern(c.tree.getTokenSlice(data.decl.name));

            try c.symbols.append(c.comp.gpa, .{ .name = name, .value = alloc });

            if (data.decl.node != .none)
                try c.genInitializer(alloc, ty, data.decl.node);
        },

        .LabeledStmt => {
            const label = try c.builder.makeLabel("label");
            try c.builder.startBlock(label);
            try c.genStmt(data.decl.node);
        },

        .CompoundStmtTwo => {
            const oldSymLen = c.symbols.items.len;
            c.symbols.items.len = oldSymLen;

            if (data.binExpr.lhs != .none) try c.genStmt(data.binExpr.lhs);
            if (data.binExpr.rhs != .none) try c.genStmt(data.binExpr.rhs);
        },

        .CompoundStmt => {
            const oldSymLen = c.symbols.items.len;
            c.symbols.items.len = oldSymLen;

            for (c.tree.data[data.range.start..data.range.end]) |stmt|
                try c.genStmt(stmt);
        },

        .IfThenElseStmt => {
            const thenLabel = try c.builder.makeLabel("if.then");
            const elseLabel = try c.builder.makeLabel("if.else");
            const endLabel = try c.builder.makeLabel("if.end");

            try c.genBoolExpr(data.if3.cond, thenLabel, elseLabel);

            try c.builder.startBlock(thenLabel);
            try c.genStmt(c.tree.data[data.if3.body]); // then
            try c.builder.addJump(endLabel);

            try c.builder.startBlock(elseLabel);
            try c.genStmt(c.tree.data[data.if3.body + 1]); // else

            try c.builder.startBlock(endLabel);
        },

        .IfThenStmt => {
            const thenLabel = try c.builder.makeLabel("if.then");
            const endLabel = try c.builder.makeLabel("if.end");

            try c.genBoolExpr(data.binExpr.lhs, thenLabel, endLabel);

            try c.builder.startBlock(thenLabel);
            try c.genStmt(data.binExpr.rhs); // then
            try c.builder.startBlock(endLabel);
        },

        .SwitchStmt => {
            var wipSwitch = WipSwitch{
                .size = c.nodeTy[@intFromEnum(data.binExpr.lhs)].sizeof(c.comp).?,
            };
            defer wipSwitch.cases.deinit(c.builder.gpa);

            const oldWipSwitch = c.wipSwitch;
            defer c.wipSwitch = oldWipSwitch;
            c.wipSwitch = &wipSwitch;

            const oldBreakLabel = c.breakLabel;
            defer c.breakLabel = oldBreakLabel;

            const endRef = try c.builder.makeLabel("switch.end");
            c.breakLabel = endRef;

            const cond = try c.genExpr(data.binExpr.lhs);
            const switchIndex = c.builder.instructions.len;
            _ = try c.builder.addInst(.Switch, undefined, .noreturn);

            try c.genStmt(data.binExpr.rhs); // body

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

        .CaseStmt => {
            const val = c.tree.valueMap.get(data.binExpr.lhs).?;
            const label = try c.builder.makeLabel("case");
            try c.builder.startBlock(label);
            try c.wipSwitch.cases.append(c.builder.gpa, .{
                .value = try c.builder.pool.put(c.builder.gpa, .{ .value = val }),
                .label = label,
            });
            try c.genStmt(data.binExpr.rhs);
        },

        .DefaultStmt => {
            const default = try c.builder.makeLabel("default");
            try c.builder.startBlock(default);
            c.wipSwitch.default = default;
            try c.genStmt(data.unExpr);
        },

        .WhileStmt => {
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
            try c.genBoolExpr(data.binExpr.lhs, thenLabel, endLabel);

            try c.builder.startBlock(thenLabel);
            try c.genStmt(data.binExpr.rhs);
            try c.builder.addJump(condLabel);

            try c.builder.startBlock(endLabel);
        },

        .DoWhileStmt => {
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
            try c.genStmt(data.binExpr.rhs);

            try c.builder.startBlock(condLabel);
            try c.genBoolExpr(data.binExpr.lhs, thenLabel, endLabel);

            try c.builder.startBlock(endLabel);
        },

        .ForDeclStmt => {
            const oldBreakLabel = c.breakLabel;
            const oldContinueLabel = c.continueLabel;

            defer {
                c.breakLabel = oldBreakLabel;
                c.continueLabel = oldContinueLabel;
            }

            const forDecl = data.forDecl(c.tree);
            for (forDecl.decls) |decl|
                try c.genStmt(decl);

            const thenLabel = try c.builder.makeLabel("for.then");
            var condLabel = thenLabel;
            const contLabel = try c.builder.makeLabel("for.cont");
            const endLabel = try c.builder.makeLabel("for.end");

            c.continueLabel = contLabel;
            c.breakLabel = endLabel;

            if (forDecl.cond != .none) {
                condLabel = try c.builder.makeLabel("for.cond");
                try c.builder.startBlock(condLabel);
                try c.genBoolExpr(forDecl.cond, thenLabel, endLabel);
            }

            try c.builder.startBlock(thenLabel);
            try c.genStmt(forDecl.body);
            if (forDecl.incr != .none)
                _ = try c.genExpr(forDecl.incr);

            try c.builder.addJump(condLabel);
            try c.builder.startBlock(endLabel);
        },

        .ForEverStmt => {
            const oldBreakLabel = c.breakLabel;
            const oldContinueLabel = c.continueLabel;

            defer {
                c.breakLabel = oldBreakLabel;
                c.continueLabel = oldContinueLabel;
            }

            const thenLabel = try c.builder.makeLabel("for.then");
            const endLabel = try c.builder.makeLabel("for.end");

            c.continueLabel = thenLabel;
            c.breakLabel = endLabel;

            try c.builder.startBlock(thenLabel);
            try c.genStmt(data.unExpr);
            try c.builder.startBlock(endLabel);
        },

        .ForStmt => {
            const oldBreakLabel = c.breakLabel;
            const oldContinueLabel = c.continueLabel;

            defer {
                c.breakLabel = oldBreakLabel;
                c.continueLabel = oldContinueLabel;
            }

            const forStmt = data.forStmt(c.tree);
            if (forStmt.init != .none)
                _ = try c.genExpr(forStmt.init);

            const thenLabel = try c.builder.makeLabel("for.then");
            var condLabel = thenLabel;
            const contLabel = try c.builder.makeLabel("for.cont");
            const endLabel = try c.builder.makeLabel("for.end");

            c.continueLabel = contLabel;
            c.breakLabel = endLabel;

            if (forStmt.cond != .none) {
                condLabel = try c.builder.makeLabel("for.cond");
                try c.builder.startBlock(condLabel);
                try c.genBoolExpr(forStmt.cond, thenLabel, endLabel);
            }

            try c.builder.startBlock(thenLabel);
            try c.genStmt(forStmt.body);
            if (forStmt.incr != .none)
                _ = try c.genExpr(forStmt.incr);

            try c.builder.addJump(condLabel);
            try c.builder.startBlock(endLabel);
        },

        .ContinueStmt => try c.builder.addJump(c.continueLabel),
        .BreakStmt => try c.builder.addJump(c.breakLabel),

        .ReturnStmt => {
            if (data.unExpr != .none) {
                const operand = try c.genExpr(data.unExpr);
                try c.retNodes.append(c.comp.gpa, .{ .value = operand, .label = c.builder.currentLabel });
            }
            try c.builder.addJump(c.returnLabel);
        },

        .ImplicitReturn => {
            if (data.returnZero) {
                const operand = try c.builder.addConstant(Value.int(0), try c.genType(ty));
                try c.retNodes.append(c.comp.gpa, .{ .value = operand, .label = c.builder.currentLabel });
            }
            // No need to emit a jump since implicit_return is always the last instruction.
        },

        .CaseRangeStmt,
        .GotoStmt,
        .ComputedGotoStmt,
        .NullPtrLiteral,
        => return c.comp.diagnostics.fatalNoSrc("TODO CodeGen.genStmt {}\n", .{c.nodeTag[@intFromEnum(node)]}),

        .CommaExpr => {
            _ = try c.genExpr(data.binExpr.lhs);
            return c.genExpr(data.binExpr.rhs);
        },

        .AssignExpr => {
            const rhs = try c.genExpr(data.binExpr.rhs);
            const lhs = try c.genLval(data.binExpr.lhs);
            try c.builder.addStore(lhs, rhs);
            return rhs;
        },

        .MulAssignExpr => return c.genCompoundAssign(node, .Mul),
        .DivAssignExpr => return c.genCompoundAssign(node, .Div),
        .ModAssignExpr => return c.genCompoundAssign(node, .Mod),
        .AddAssignExpr => return c.genCompoundAssign(node, .Add),
        .SubAssignExpr => return c.genCompoundAssign(node, .Sub),
        .ShlAssignExpr => return c.genCompoundAssign(node, .BitShl),
        .ShrAssignExpr => return c.genCompoundAssign(node, .BitShr),
        .BitAndAssignExpr => return c.genCompoundAssign(node, .BitAnd),
        .BitXorAssignExpr => return c.genCompoundAssign(node, .BitXor),
        .BitOrAssignExpr => return c.genCompoundAssign(node, .BitOr),

        .BitOrExpr => return c.genBinOp(node, .BitOr),
        .BitXorExpr => return c.genBinOp(node, .BitXor),
        .BitAndExpr => return c.genBinOp(node, .BitAnd),

        .EqualExpr => {
            const cmp = try c.genComparison(node, .CmpEQ);
            return c.addUn(.Zext, cmp, ty);
        },

        .NotEqualExpr => {
            const cmp = try c.genComparison(node, .CmpNE);
            return c.addUn(.Zext, cmp, ty);
        },

        .LessThanExpr => {
            const cmp = try c.genComparison(node, .CmpLT);
            return c.addUn(.Zext, cmp, ty);
        },

        .LessThanEqualExpr => {
            const cmp = try c.genComparison(node, .CmpLTE);
            return c.addUn(.Zext, cmp, ty);
        },

        .GreaterThanExpr => {
            const cmp = try c.genComparison(node, .CmpGT);
            return c.addUn(.Zext, cmp, ty);
        },

        .GreaterThanEqualExpr => {
            const cmp = try c.genComparison(node, .CmpGTE);
            return c.addUn(.Zext, cmp, ty);
        },

        .ShlExpr => return c.genBinOp(node, .BitShl),
        .ShrExpr => return c.genBinOp(node, .BitShr),

        .AddExpr => {
            if (ty.isPointer()) {
                const lhsTy = c.nodeTy[@intFromEnum(data.binExpr.lhs)];
                if (lhsTy.isPointer()) {
                    const ptr = try c.genExpr(data.binExpr.lhs);
                    const offset = try c.genExpr(data.binExpr.rhs);
                    const offsetTy = c.nodeTy[@intFromEnum(data.binExpr.rhs)];
                    return c.genPtrArithmetic(ptr, offset, offsetTy, ty);
                } else {
                    const offset = try c.genExpr(data.binExpr.lhs);
                    const ptr = try c.genExpr(data.binExpr.rhs);
                    const offsetTy = lhsTy;
                    return c.genPtrArithmetic(ptr, offset, offsetTy, ty);
                }
            }
            return c.genBinOp(node, .Add);
        },

        .SubExpr => {
            if (ty.isPointer()) {
                const ptr = try c.genExpr(data.binExpr.lhs);
                const offset = try c.genExpr(data.binExpr.rhs);
                const offsetTy = c.nodeTy[@intFromEnum(data.binExpr.rhs)];
                return c.genPtrArithmetic(ptr, offset, offsetTy, ty);
            }
            return c.genBinOp(node, .Sub);
        },

        .MulExpr => return c.genBinOp(node, .Mul),
        .DivExpr => return c.genBinOp(node, .Div),
        .ModExpr => return c.genBinOp(node, .Mod),

        .AddrOfExpr => return try c.genLval(data.unExpr),
        .DerefExpr => {
            const unData = c.nodeData[@intFromEnum(data.unExpr)];
            if (c.nodeTag[@intFromEnum(data.unExpr)] == .ImplicitCast and
                unData.cast.kind == .FunctionToPointer)
                return c.genExpr(data.unExpr);

            const operand = try c.genLval(data.unExpr);
            return c.addUn(.Load, operand, ty);
        },

        .PlusExpr => return c.genExpr(data.unExpr),
        .NegateExpr => {
            const zero = try c.builder.addConstant(Value.int(0), try c.genType(ty));
            const operand = try c.genExpr(data.unExpr);
            return c.addBin(.Sub, zero, operand, ty);
        },

        .BitNotExpr => {
            const operand = try c.genExpr(data.unExpr);
            return c.addUn(.BitNot, operand, ty);
        },
        .BoolNotExpr => {
            const zero = try c.builder.addConstant(Value.int(0), try c.genType(ty));
            const operand = try c.genExpr(data.unExpr);
            return c.addBin(.CmpNE, zero, operand, ty);
        },

        .PreIncExpr => {
            const operand = try c.genLval(data.unExpr);
            const val = try c.addUn(.Load, operand, ty);
            const one = try c.builder.addConstant(Value.int(1), try c.genType(ty));
            const plusOne = try c.addBin(.Add, val, one, ty);
            try c.builder.addStore(operand, plusOne);
            return plusOne;
        },
        .PreDecExpr => {
            const operand = try c.genLval(data.unExpr);
            const val = try c.addUn(.Load, operand, ty);
            const one = try c.builder.addConstant(Value.int(1), try c.genType(ty));
            const decOne = try c.addBin(.Sub, val, one, ty);
            try c.builder.addStore(operand, decOne);
            return decOne;
        },

        .PostIncExpr => {
            const operand = try c.genLval(data.unExpr);
            const val = try c.addUn(.Load, operand, ty);
            const one = try c.builder.addConstant(Value.int(1), try c.genType(ty));
            const plusOne = try c.addBin(.Add, val, one, ty);
            try c.builder.addStore(operand, plusOne);
            return val;
        },
        .PostDecExpr => {
            const operand = try c.genLval(data.unExpr);
            const val = try c.addUn(.Load, operand, ty);
            const one = try c.builder.addConstant(Value.int(1), try c.genType(ty));
            const decOne = try c.addBin(.Sub, val, one, ty);
            try c.builder.addStore(operand, decOne);
            return val;
        },

        .ParenExpr => return c.genExpr(data.unExpr),
        .DeclRefExpr => unreachable, // Lval expression.
        .ExplicitCast, .ImplicitCast => switch (data.cast.kind) {
            .NoOP => return c.genExpr(data.cast.operand),
            .ToVoid => {
                _ = try c.genExpr(data.cast.operand);
                return .none;
            },

            .LValToRVal => {
                const operand = try c.genLval(data.cast.operand);
                return c.addUn(.Load, operand, ty);
            },

            .FunctionToPointer, .ArrayToPointer => {
                return c.genLval(data.cast.operand);
            },

            .IntCast => {
                const operand = try c.genExpr(data.cast.operand);
                const srcTy = c.nodeTy[@intFromEnum(data.cast.operand)];
                const srcBits = srcTy.bitSizeof(c.comp).?;
                const destBits = ty.bitSizeof(c.comp).?;
                if (srcBits == destBits) {
                    return operand;
                } else if (srcBits < destBits) {
                    if (srcTy.isUnsignedInt(c.comp))
                        return c.addUn(.Zext, operand, ty)
                    else
                        return c.addUn(.Sext, operand, ty);
                } else {
                    return c.addUn(.Trunc, operand, ty);
                }
            },

            .BoolToInt => {
                const operand = try c.genExpr(data.cast.operand);
                return c.addUn(.Zext, operand, ty);
            },

            .PointerToBool, .IntToBool, .FloatToBool => {
                const lhs = try c.genExpr(data.cast.operand);
                const rhs = try c.builder.addConstant(Value.int(0), try c.genType(c.nodeTy[@intFromEnum(node)]));
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
            => return c.comp.diagnostics.fatalNoSrc("TODO CodeGen gen CastKind {}\n", .{data.cast.kind}),
        },

        .BinaryCondExpr => {
            if (c.tree.valueMap.get(data.if3.cond)) |cond| {
                if (cond.getBool()) {
                    c.condDummyRef = try c.genExpr(data.if3.cond);
                    return c.genExpr(c.tree.data[data.if3.body]); // then
                } else {
                    return c.genExpr(c.tree.data[data.if3.body + 1]); // else
                }
            }

            const thenLabel = try c.builder.makeLabel("ternary.then");
            const elseLabel = try c.builder.makeLabel("ternary.else");
            const endLabel = try c.builder.makeLabel("ternary.end");
            const condTy = c.nodeTy[@intFromEnum(data.if3.cond)];
            {
                const oldCondDummyTy = c.condDummyTy;
                defer c.condDummyTy = oldCondDummyTy;
                c.condDummyTy = try c.genType(condTy);

                try c.genBoolExpr(data.if3.cond, thenLabel, elseLabel);
            }

            try c.builder.startBlock(thenLabel);
            if (c.builder.instructions.items(.type)[@intFromEnum(c.condDummyRef)] == .i1)
                c.condDummyRef = try c.addUn(.Zext, c.condDummyRef, condTy);

            const thenVal = try c.genExpr(c.tree.data[data.if3.body]); // then
            try c.builder.addJump(endLabel);
            const thenExit = c.builder.currentLabel;

            try c.builder.startBlock(elseLabel);
            const elseVal = try c.genExpr(c.tree.data[data.if3.body + 1]); // else
            const elseExit = c.builder.currentLabel;

            try c.builder.startBlock(endLabel);

            var phiBuffer: [2]IR.Inst.Phi.Input = .{
                .{ .value = thenVal, .label = thenExit },
                .{ .value = elseVal, .label = elseExit },
            };
            return c.builder.addPhi(&phiBuffer, try c.genType(ty));
        },

        .CondDummyExpr => return c.condDummyRef,

        .CondExpr => {
            if (c.tree.valueMap.get(data.if3.cond)) |cond| {
                if (cond.getBool()) {
                    return c.genExpr(c.tree.data[data.if3.body]); // then
                } else {
                    return c.genExpr(c.tree.data[data.if3.body + 1]); // else
                }
            }

            const thenLabel = try c.builder.makeLabel("ternary.then");
            const elseLabel = try c.builder.makeLabel("ternary.else");
            const endLabel = try c.builder.makeLabel("ternary.end");

            try c.genBoolExpr(data.if3.cond, thenLabel, elseLabel);

            try c.builder.startBlock(thenLabel);
            const thenVal = try c.genExpr(c.tree.data[data.if3.body]); // then
            try c.builder.addJump(endLabel);
            const thenExit = c.builder.currentLabel;

            try c.builder.startBlock(elseLabel);
            const elseVal = try c.genExpr(c.tree.data[data.if3.body + 1]); // else
            const elseExit = c.builder.currentLabel;

            try c.builder.startBlock(endLabel);

            var phiBuffer: [2]IR.Inst.Phi.Input = .{
                .{ .value = thenVal, .label = thenExit },
                .{ .value = elseVal, .label = elseExit },
            };
            return c.builder.addPhi(&phiBuffer, try c.genType(ty));
        },

        .CallExprOne => if (data.binExpr.rhs == .none) {
            return c.genCall(data.binExpr.lhs, &.{}, ty);
        } else {
            return c.genCall(data.binExpr.lhs, &.{data.binExpr.rhs}, ty);
        },

        .CallExpr => {
            return c.genCall(c.tree.data[data.range.start], c.tree.data[data.range.start + 1 .. data.range.end], ty);
        },

        .BoolOrExpr => {
            if (c.tree.valueMap.get(data.binExpr.lhs)) |lhs| {
                const cond = lhs.getBool();
                if (!cond)
                    return c.builder.addConstant(Value.int(1), try c.genType(ty));

                return c.genExpr(data.binExpr.rhs);
            }

            const falseLabel = try c.builder.makeLabel("bool_false");
            const exitLabel = try c.builder.makeLabel("bool_exit");

            const oldBoolEndLabel = c.boolEndLabel;
            defer c.boolEndLabel = oldBoolEndLabel;
            c.boolEndLabel = exitLabel;

            const phiNodesTop = c.phiNodes.items.len;
            defer c.phiNodes.items.len = phiNodesTop;

            try c.genBoolExpr(data.binExpr.lhs, exitLabel, falseLabel);

            try c.builder.startBlock(falseLabel);
            try c.genBoolExpr(data.binExpr.rhs, exitLabel, exitLabel);

            try c.builder.startBlock(exitLabel);

            const phi = try c.builder.addPhi(c.phiNodes.items[phiNodesTop..], .i1);
            return c.addUn(.Zext, phi, ty);
        },

        .BoolAndExpr => {
            if (c.tree.valueMap.get(data.binExpr.lhs)) |lhs| {
                const cond = lhs.getBool();
                if (!cond)
                    return c.builder.addConstant(Value.int(0), try c.genType(ty));

                return c.genExpr(data.binExpr.rhs);
            }
            const trueLabel = try c.builder.makeLabel("bool_true");
            const exitLabel = try c.builder.makeLabel("bool_exit");

            const oldBoolEndLabel = c.boolEndLabel;
            defer c.boolEndLabel = oldBoolEndLabel;
            c.boolEndLabel = exitLabel;

            const phiNodesTop = c.phiNodes.items.len;
            defer c.phiNodes.items.len = phiNodesTop;

            try c.genBoolExpr(data.binExpr.lhs, trueLabel, exitLabel);

            try c.builder.startBlock(trueLabel);
            try c.genBoolExpr(data.binExpr.rhs, exitLabel, exitLabel);

            try c.builder.startBlock(exitLabel);

            const phi = try c.builder.addPhi(c.phiNodes.items[phiNodesTop..], .i1);
            return c.addUn(.Zext, phi, ty);
        },

        .BuiltinChooseExpr => {
            const cond = c.tree.valueMap.get(data.if3.cond).?;
            if (cond.getBool())
                return c.genExpr(c.tree.data[data.if3.body])
            else
                return c.genExpr(c.tree.data[data.if3.body + 1]);
        },

        .GenericExprOne => {
            const index = @intFromEnum(data.binExpr.rhs);
            switch (c.nodeTag[index]) {
                .GenericAssociationExpr, .GenericDefaultExpr => {
                    return c.genExpr(c.nodeData[index].unExpr);
                },
                else => unreachable,
            }
        },

        .GenericExpr => {
            const index = @intFromEnum(c.tree.data[data.range.start + 1]);
            switch (c.nodeTag[index]) {
                .GenericAssociationExpr, .GenericDefaultExpr => {
                    return c.genExpr(c.nodeData[index].unExpr);
                },
                else => unreachable,
            }
        },

        .GenericAssociationExpr, .GenericDefaultExpr => unreachable,

        .StmtExpr => switch (c.nodeTag[@intFromEnum(data.unExpr)]) {
            .CompoundStmtTwo => {
                const oldSymLen = c.symbols.items.len;
                c.symbols.items.len = oldSymLen;

                const stmtData = c.nodeData[@intFromEnum(data.unExpr)];
                if (stmtData.binExpr.rhs == .none)
                    return c.genExpr(stmtData.binExpr.lhs);
                try c.genStmt(stmtData.binExpr.lhs);
                return c.genExpr(stmtData.binExpr.rhs);
            },

            .CompoundStmt => {
                const oldSymLen = c.symbols.items.len;
                c.symbols.items.len = oldSymLen;

                const stmtData = c.nodeData[@intFromEnum(data.unExpr)];
                for (c.tree.data[stmtData.range.start .. stmtData.range.end - 1]) |stmt|
                    try c.genStmt(stmt);
                return c.genExpr(c.tree.data[stmtData.range.end]);
            },

            else => unreachable,
        },

        .AddrOfLabel,
        .ImagExpr,
        .RealExpr,
        .BuiltinCallExprOne,
        .BuiltinCallExpr,
        .SizeOfExpr,
        => return c.comp.diagnostics.fatalNoSrc("TODO CodeGen.genExpr {}\n", .{c.nodeTag[@intFromEnum(node)]}),

        else => unreachable, // Not an expression.
    }

    return .none;
}

fn genLval(c: *CodeGen, node: NodeIndex) Error!IR.Ref {
    assert(node != .none);
    assert(c.tree.isLValue(node));

    const data = c.nodeData[@intFromEnum(node)];
    switch (c.nodeTag[@intFromEnum(node)]) {
        .StringLiteralExpr => {
            const val = c.tree.valueMap.get(node).?;
            return c.builder.addConstant(val, .ptr);
        },

        .ParenExpr => return c.genLval(data.unExpr),

        .DeclRefExpr => {
            const slice = c.tree.getTokenSlice(data.declRef);
            const name = try c.comp.intern(slice);
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

        .DerefExpr => return c.genExpr(data.unExpr),

        .CompoundLiteralExpr => {
            const ty = c.nodeTy[@intFromEnum(node)];
            const size: u32 = @intCast(ty.sizeof(c.comp).?); // TODO add error in parser
            const @"align" = ty.alignof(c.comp);
            const alloc = try c.builder.addAlloc(size, @"align");
            try c.genInitializer(alloc, ty, data.unExpr);
            return alloc;
        },

        .BuiltinChooseExpr => {
            const cond = c.tree.valueMap.get(data.if3.cond).?;
            if (cond.getBool())
                return c.genLval(c.tree.data[data.if3.body])
            else
                return c.genLval(c.tree.data[data.if3.body + 1]);
        },

        .MemberAccessExpr,
        .MemberAccessPtrExpr,
        .ArrayAccessExpr,
        => return c.comp.diagnostics.fatalNoSrc("TODO CodeGen.genLval {}\n", .{c.nodeTag[@intFromEnum(node)]}),

        else => unreachable, // not an lvalue expression.
    }
}

fn genBoolExpr(c: *CodeGen, base: NodeIndex, trueLabel: IR.Ref, falseLabel: IR.Ref) Error!void {
    var node = base;
    while (true) {
        switch (c.nodeTag[@intFromEnum(node)]) {
            .ParenExpr => node = c.nodeData[@intFromEnum(node)].unExpr,
            else => break,
        }
    }

    const data = c.nodeData[@intFromEnum(node)];
    const tag = c.nodeTag[@intFromEnum(node)];
    switch (tag) {
        .BoolOrExpr => {
            if (c.tree.valueMap.get(data.binExpr.lhs)) |lhs| {
                const cond = lhs.getBool();
                if (cond) {
                    if (trueLabel == c.boolEndLabel) {
                        return c.addBoolPhi(!c.boolInvert);
                    }
                    return c.builder.addJump(trueLabel);
                }
                return c.genBoolExpr(data.binExpr.rhs, trueLabel, falseLabel);
            }

            const newFalseLabel = try c.builder.makeLabel("bool_false");
            try c.genBoolExpr(data.binExpr.lhs, trueLabel, newFalseLabel);
            try c.builder.startBlock(newFalseLabel);

            if (c.condDummyTy) |ty| c.condDummyRef = try c.builder.addConstant(Value.int(1), ty);
            return c.genBoolExpr(data.binExpr.rhs, trueLabel, falseLabel);
        },

        .BoolAndExpr => {
            if (c.tree.valueMap.get(data.binExpr.lhs)) |lhs| {
                const cond = lhs.getBool();
                if (!cond) {
                    if (falseLabel == c.boolEndLabel) {
                        return c.addBoolPhi(c.boolInvert);
                    }
                    return c.builder.addJump(falseLabel);
                }
                return c.genBoolExpr(data.binExpr.rhs, trueLabel, falseLabel);
            }

            const newTrueLabel = try c.builder.makeLabel("bool_true");
            try c.genBoolExpr(data.binExpr.lhs, newTrueLabel, falseLabel);
            try c.builder.startBlock(newTrueLabel);

            if (c.condDummyTy) |ty| c.condDummyRef = try c.builder.addConstant(Value.int(1), ty);
            return c.genBoolExpr(data.binExpr.rhs, trueLabel, falseLabel);
        },

        .BoolNotExpr => {
            c.boolInvert = !c.boolInvert;
            defer c.boolInvert = !c.boolInvert;

            if (c.condDummyTy) |ty| c.condDummyRef = try c.builder.addConstant(Value.int(0), ty);
            return c.genBoolExpr(data.unExpr, trueLabel, falseLabel);
        },

        .EqualExpr => {
            const cmp = try c.genComparison(node, .CmpEQ);
            if (c.condDummyTy != null) c.condDummyRef = cmp;
            return c.addBranch(cmp, trueLabel, falseLabel);
        },

        .NotEqualExpr => {
            const cmp = try c.genComparison(node, .CmpNE);
            if (c.condDummyTy != null) c.condDummyRef = cmp;
            return c.addBranch(cmp, trueLabel, falseLabel);
        },

        .LessThanExpr => {
            const cmp = try c.genComparison(node, .CmpLT);
            if (c.condDummyTy != null) c.condDummyRef = cmp;
            return c.addBranch(cmp, trueLabel, falseLabel);
        },

        .LessThanEqualExpr => {
            const cmp = try c.genComparison(node, .CmpLTE);
            if (c.condDummyTy != null) c.condDummyRef = cmp;
            return c.addBranch(cmp, trueLabel, falseLabel);
        },

        .GreaterThanExpr => {
            const cmp = try c.genComparison(node, .CmpGT);
            if (c.condDummyTy != null) c.condDummyRef = cmp;
            return c.addBranch(cmp, trueLabel, falseLabel);
        },

        .GreaterThanEqualExpr => {
            const cmp = try c.genComparison(node, .CmpGTE);
            if (c.condDummyTy != null) c.condDummyRef = cmp;
            return c.addBranch(cmp, trueLabel, falseLabel);
        },

        .ExplicitCast, .ImplicitCast => switch (data.cast.kind) {
            .BoolToInt => {
                const operand = try c.genExpr(data.cast.operand);
                if (c.condDummyTy != null) c.condDummyRef = operand;
                return c.addBranch(operand, trueLabel, falseLabel);
            },
            else => {},
        },

        .BinaryCondExpr => {
            if (c.tree.valueMap.get(data.if3.cond)) |cond| {
                if (cond.getBool()) {
                    return c.genBoolExpr(c.tree.data[data.if3.body], trueLabel, falseLabel); // then
                } else {
                    return c.genBoolExpr(c.tree.data[data.if3.body + 1], trueLabel, falseLabel); // else
                }
            }

            const newFalseLabel = try c.builder.makeLabel("ternary.else");
            try c.genBoolExpr(data.if3.cond, trueLabel, newFalseLabel);

            try c.builder.startBlock(newFalseLabel);
            if (c.condDummyTy) |ty| c.condDummyRef = try c.builder.addConstant(Value.int(1), ty);
            return c.genBoolExpr(c.tree.data[data.if3.body + 1], trueLabel, falseLabel); // else
        },

        .CondExpr => {
            if (c.tree.valueMap.get(data.if3.cond)) |cond| {
                if (cond.getBool()) {
                    return c.genBoolExpr(c.tree.data[data.if3.body], trueLabel, falseLabel); // then
                } else {
                    return c.genBoolExpr(c.tree.data[data.if3.body + 1], trueLabel, falseLabel); // else
                }
            }

            const newTrueLabel = try c.builder.makeLabel("ternary.then");
            const newFalseLabel = try c.builder.makeLabel("ternary.else");
            try c.genBoolExpr(data.if3.cond, newTrueLabel, newFalseLabel);

            try c.builder.startBlock(newTrueLabel);
            try c.genBoolExpr(c.tree.data[data.if3.body], trueLabel, falseLabel); // then
            try c.builder.startBlock(newFalseLabel);
            if (c.condDummyTy) |ty| c.condDummyRef = try c.builder.addConstant(Value.int(1), ty);
            return c.genBoolExpr(c.tree.data[data.if3.body + 1], trueLabel, falseLabel); // else
        },

        else => {},
    }

    if (c.tree.valueMap.get(node)) |value| {
        if (value.getBool()) {
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
    const rhs = try c.builder.addConstant(Value.int(0), try c.genType(c.nodeTy[@intFromEnum(node)]));
    const cmp = try c.builder.addInst(.CmpNE, .{ .bin = .{ .lhs = lhs, .rhs = rhs } }, .i1);
    if (c.condDummyTy != null) c.condDummyRef = cmp;
    try c.addBranch(cmp, trueLabel, falseLabel);
}

fn genCall(c: *CodeGen, fnNode: NodeIndex, argNodes: []const NodeIndex, ty: Type) Error!IR.Ref {
    // Detect direct calls.
    const fnRef = blk: {
        const data = c.nodeData[@intFromEnum(fnNode)];
        if (c.nodeTag[@intFromEnum(fnNode)] != .ImplicitCast or data.cast.kind != .FunctionToPointer)
            break :blk try c.genExpr(fnNode);

        var cur = @intFromEnum(data.cast.operand);
        while (true) switch (c.nodeTag[cur]) {
            .ParenExpr, .AddrOfExpr, .DerefExpr => {
                cur = @intFromEnum(c.nodeData[cur].unExpr);
            },

            .ImplicitCast => {
                const cast = c.nodeData[cur].cast;
                if (cast.kind != .FunctionToPointer) {
                    break :blk try c.genExpr(fnNode);
                }
                cur = @intFromEnum(cast.operand);
            },

            .DeclRefExpr => {
                const slice = c.tree.getTokenSlice(c.nodeData[cur].declRef);
                const name = try c.comp.intern(slice);
                var i = c.symbols.items.len;
                while (i > 0) {
                    i -= 1;
                    if (c.symbols.items[i].name == name) {
                        break :blk try c.genExpr(fnNode);
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
            else => break :blk try c.genExpr(fnNode),
        };
    };

    const args = try c.builder.arena.allocator().alloc(IR.Ref, argNodes.len);
    for (argNodes, args) |node, *arg| {
        // TODO handle calling convention here
        arg.* = try c.genExpr(node);
    }
    // TODO handle variadic call
    const call = try c.builder.arena.allocator().create(Inst.Call);
    call.* = .{
        .func = fnRef,
        .argsLen = @intCast(args.len),
        .argsPtr = args.ptr,
    };
    return c.builder.addInst(.Call, .{ .call = call }, try c.genType(ty));
}

fn genCompoundAssign(c: *CodeGen, node: NodeIndex, tag: Inst.Tag) Error!IR.Ref {
    const bin = c.nodeData[@intFromEnum(node)].binExpr;
    const ty = c.nodeTy[@intFromEnum(node)];
    const rhs = try c.genExpr(bin.rhs);
    const lhs = try c.genLval(bin.lhs);
    const res = try c.addBin(tag, lhs, rhs, ty);

    try c.builder.addStore(lhs, res);
    return res;
}

fn genBinOp(c: *CodeGen, node: NodeIndex, tag: Inst.Tag) Error!IR.Ref {
    const bin = c.nodeData[@intFromEnum(node)].binExpr;
    const ty = c.nodeTy[@intFromEnum(node)];
    const lhs = try c.genExpr(bin.lhs);
    const rhs = try c.genExpr(bin.rhs);
    return c.addBin(tag, lhs, rhs, ty);
}

fn genComparison(c: *CodeGen, node: NodeIndex, tag: Inst.Tag) Error!IR.Ref {
    const bin = c.nodeData[@intFromEnum(node)].binExpr;
    const lhs = try c.genExpr(bin.lhs);
    const rhs = try c.genExpr(bin.rhs);

    return c.builder.addInst(tag, .{ .bin = .{ .lhs = lhs, .rhs = rhs } }, .i1);
}

fn genPtrArithmetic(c: *CodeGen, ptr: IR.Ref, offset: IR.Ref, offsetTy: Type, ty: Type) Error!IR.Ref {
    // TODO consider adding a getelemptr instruction
    const size = ty.getElemType().sizeof(c.comp).?;
    if (size == 1)
        return c.builder.addInst(.Add, .{ .bin = .{ .lhs = ptr, .rhs = offset } }, try c.genType(ty));

    const sizeInst = try c.builder.addConstant(Value.int(size), try c.genType(offsetTy));
    const offsetInst = try c.addBin(.Mul, offset, sizeInst, offsetTy);
    return c.addBin(.Add, ptr, offsetInst, offsetTy);
}

fn genInitializer(c: *CodeGen, ptr: IR.Ref, destTy: Type, initializer: NodeIndex) Error!void {
    assert(initializer != .none);
    switch (c.nodeTag[@intFromEnum(initializer)]) {
        .ArrayInitExprTwo,
        .ArrayInitExpr,
        .StructInitExprTwo,
        .StructInitExpr,
        .UnionInitExpr,
        .ArrayFillerExpr,
        .DefaultInitExpr,
        => return c.comp.diagnostics.fatalNoSrc("TODO CodeGen.genInitializer {}\n", .{c.nodeTag[@intFromEnum(initializer)]}),

        .StringLiteralExpr => {
            const val = c.tree.valueMap.get(initializer).?;
            const strPtr = try c.builder.addConstant(val, .ptr);
            if (destTy.isArray()) {
                return c.comp.diagnostics.fatalNoSrc("TODO memcpy\n", .{});
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
fn genVar(c: *CodeGen, _: NodeIndex) Error!void {
    return c.comp.diagnostics.fatalNoSrc("TODO CodeGen.genVar\n", .{});
}
