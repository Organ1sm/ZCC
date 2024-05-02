const std = @import("std");
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
condDummyRef: IR.Ref = undefined,
symbols: std.ArrayListUnmanaged(Symbol) = .{},
retNodes: std.ArrayListUnmanaged(IR.Inst.Phi.Input) = .{},
continueLabel: IR.Ref = undefined,
breakLabel: IR.Ref = undefined,
returnLabel: IR.Ref = undefined,
currentLabel: IR.Ref = undefined,

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
    defer c.symbols.deinit(c.builder.gpa);
    defer c.retNodes.deinit(c.comp.gpa);
    defer c.builder.deinit();

    const nodeTags = tree.nodes.items(.tag);
    for (tree.rootDecls) |decl| {
        c.builder.arena.deinit();
        c.builder.arena = std.heap.ArenaAllocator.init(comp.gpa);
        c.builder.instructions.len = 0;
        c.builder.body.items.len = 0;

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
        else => {},
    }

    if (ty.isPointer())
        return .ptr;

    if (ty.isFunc())
        return .func;

    if (!ty.isReal())
        @panic("TODO lower complex types");

    if (ty.isInt()) {
        const bits = ty.bitSizeof(c.comp).?;
        key = .{ .int = @intCast(bits) };
    } else if (ty.isFloat()) {
        const bits = ty.bitSizeof(c.comp).?;
        key = .{ .float = @intCast(bits) };
    } else if (ty.isArray()) {
        const elem = try c.genType(ty.getElemType());
        key = .{ .array = .{ .child = elem, .len = ty.arrayLen().? } };
    } else if (ty.isVector()) {
        const elem = try c.genType(ty.getElemType());
        key = .{ .vector = .{ .child = elem, .len = @intCast(ty.data.array.len) } };
    }

    return c.builder.pool.put(c.builder.gpa, key);
}

fn genFn(c: *CodeGen, decl: NodeIndex) Error!void {
    const name = c.tree.getTokenSlice(c.nodeData[@intFromEnum(decl)].decl.name);
    const funcTy = c.nodeTy[@intFromEnum(decl)].canonicalize(.standard);
    c.builder.allocCount = 0;
    c.retNodes.items.len = 0;

    // reserve space for arg instructions
    const params = funcTy.getParams();
    try c.builder.instructions.ensureUnusedCapacity(c.builder.gpa, params.len);
    c.builder.instructions.len = params.len;

    // Generate parameters
    for (params, 0..) |param, i| {
        // TODO handle calling convention here
        const arg: IR.Ref = @enumFromInt(i);
        c.builder.instructions.set(i, .{
            .tag = .Arg,
            .data = .{ .arg = @intCast(i) },
            .type = try c.genType(param.ty),
        });

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
    if (c.retNodes.items.len == 0) {
        _ = try c.builder.addInst(.Ret, .{ .un = .none }, .noreturn);
    } else if (c.retNodes.items.len == 1) {
        c.builder.body.items.len -= 1;
        _ = try c.builder.addInst(.Ret, .{ .un = c.retNodes.items[0].value }, .noreturn);
    } else {
        try c.startBlock(c.returnLabel);
        const phi = try c.builder.addPhi(c.retNodes.items, try c.genType(funcTy.getReturnType()));
        _ = try c.builder.addInst(.Ret, .{ .un = phi }, .noreturn);
    }

    var res = IR{
        .pool = c.builder.pool,
        .instructions = c.builder.instructions,
        .arena = c.builder.arena.state,
        .body = c.builder.body,
    };

    res.dump(name, c.comp.diag.color, std.io.getStdOut().writer()) catch {};
}

fn addUn(c: *CodeGen, tag: Inst.Tag, operand: IR.Ref, ty: Type) !IR.Ref {
    return c.builder.addInst(tag, .{ .un = operand }, try c.genType(ty));
}

fn addBin(c: *CodeGen, tag: Inst.Tag, lhs: IR.Ref, rhs: IR.Ref, ty: Type) !IR.Ref {
    return c.builder.addInst(tag, .{ .bin = .{ .lhs = lhs, .rhs = rhs } }, try c.genType(ty));
}

fn startBlock(c: *CodeGen, label: IR.Ref) !void {
    try c.builder.body.append(c.builder.gpa, label);
    c.currentLabel = label;
}

fn genStmt(c: *CodeGen, node: NodeIndex) Error!void {
    std.debug.assert(node != .none);
    const ty = c.nodeTy[@intFromEnum(node)];
    const data = c.nodeData[@intFromEnum(node)];
    const tag = c.nodeTag[@intFromEnum(node)];

    switch (tag) {
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

            if (data.decl.node != .none) {
                const res = try c.genExpr(data.decl.node);
                try c.builder.addStore(alloc, res);
            }
        },

        .LabeledStmt => {
            const label = try c.builder.makeLabel("label");
            try c.startBlock(label);
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

            {
                c.builder.branch = .{
                    .trueLabel = thenLabel,
                    .falseLabel = elseLabel,
                };
                defer c.builder.branch = null;
                try c.genBoolExpr(data.if3.cond);
            }

            try c.startBlock(thenLabel);
            try c.genStmt(c.tree.data[data.if3.body]); // then
            try c.builder.addJump(endLabel);

            try c.startBlock(elseLabel);
            try c.genStmt(c.tree.data[data.if3.body + 1]); // else

            try c.startBlock(endLabel);
        },

        .IfThenStmt => {
            const thenLabel = try c.builder.makeLabel("if.then");
            const endLabel = try c.builder.makeLabel("if.end");

            {
                c.builder.branch = .{
                    .trueLabel = thenLabel,
                    .falseLabel = endLabel,
                };
                defer c.builder.branch = null;
                try c.genBoolExpr(data.binExpr.lhs);
            }
            try c.startBlock(thenLabel);
            try c.genStmt(data.binExpr.rhs); // then
            try c.startBlock(endLabel);
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
            try c.startBlock(endRef);

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
            try c.startBlock(label);
            try c.wipSwitch.cases.append(c.builder.gpa, .{
                .value = try c.builder.pool.put(c.builder.gpa, .{ .value = val }),
                .label = label,
            });
            try c.genStmt(data.binExpr.rhs);
        },

        .DefaultStmt => {
            const default = try c.builder.makeLabel("default");
            try c.startBlock(default);
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

            try c.startBlock(condLabel);
            {
                c.builder.branch = .{
                    .trueLabel = thenLabel,
                    .falseLabel = endLabel,
                };
                defer c.builder.branch = null;
                try c.genBoolExpr(data.binExpr.lhs);
            }

            try c.startBlock(thenLabel);
            try c.genStmt(data.binExpr.rhs);
            try c.builder.addJump(condLabel);
            try c.startBlock(endLabel);
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

            try c.startBlock(thenLabel);
            try c.genStmt(data.binExpr.rhs);
            try c.startBlock(condLabel);
            {
                c.builder.branch = .{
                    .trueLabel = thenLabel,
                    .falseLabel = endLabel,
                };
                defer c.builder.branch = null;
                try c.genBoolExpr(data.binExpr.lhs);
            }
            try c.startBlock(endLabel);
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
                try c.startBlock(condLabel);

                c.builder.branch = .{
                    .trueLabel = thenLabel,
                    .falseLabel = endLabel,
                };
                defer c.builder.branch = null;
                try c.genBoolExpr(forDecl.cond);
            }

            try c.startBlock(thenLabel);
            try c.genStmt(forDecl.body);
            if (forDecl.incr != .none)
                _ = try c.genExpr(forDecl.incr);

            try c.builder.addJump(condLabel);
            try c.startBlock(endLabel);
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

            try c.startBlock(thenLabel);
            try c.genStmt(data.unExpr);
            try c.startBlock(endLabel);
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
                try c.startBlock(condLabel);

                c.builder.branch = .{
                    .trueLabel = thenLabel,
                    .falseLabel = endLabel,
                };
                defer c.builder.branch = null;
                try c.genBoolExpr(forStmt.cond);
            }

            try c.startBlock(thenLabel);
            try c.genStmt(forStmt.body);
            if (forStmt.incr != .none) {
                _ = try c.genExpr(forStmt.incr);
            }
            try c.builder.addJump(condLabel);
            try c.startBlock(endLabel);
        },

        .ContinueStmt => try c.builder.addJump(c.continueLabel),
        .BreakStmt => try c.builder.addJump(c.breakLabel),

        .ReturnStmt => {
            if (data.unExpr != .none) {
                const operand = try c.genExpr(data.unExpr);
                try c.retNodes.append(c.comp.gpa, .{ .value = operand, .label = c.currentLabel });
            }
            try c.builder.addJump(c.returnLabel);
        },

        .ImplicitReturn => {
            if (data.returnZero) {
                const operand = try c.builder.addConstant(Value.int(0), try c.genType(ty));
                try c.retNodes.append(c.comp.gpa, .{ .value = operand, .label = c.currentLabel });
            }
            // No need to emit a jump since implicit_return is always the last instruction.
        },

        .CaseRangeStmt,
        .GotoStmt,
        .ComputedGotoStmt,
        => return c.comp.diag.fatalNoSrc("TODO CodeGen.genStmt {}\n", .{c.nodeTag[@intFromEnum(node)]}),

        else => _ = try c.genExpr(node),
    }
}

fn genExpr(c: *CodeGen, node: NodeIndex) Error!IR.Ref {
    std.debug.assert(node != .none);

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
            .ToVoid => unreachable, // Not an expression.

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
            => return c.comp.diag.fatalNoSrc("TODO CodeGen gen CastKind {}\n", .{data.cast.kind}),
        },

        .BinaryCondExpr => {
            const cond = try c.genExpr(data.if3.cond);
            const then = then: {
                const oldCondDummyRef = c.condDummyRef;
                defer c.condDummyRef = oldCondDummyRef;

                c.condDummyRef = cond;

                break :then try c.genExpr(c.tree.data[data.if3.body]);
            };

            const @"else" = try c.genExpr(c.tree.data[data.if3.body + 1]);

            const branch = try c.builder.arena.allocator().create(Inst.Branch);
            branch.* = .{ .cond = cond, .then = then, .@"else" = @"else" };

            // TODO can't use select here
            return c.builder.addInst(.Select, .{ .branch = branch }, try c.genType(ty));
        },

        .CondDummyExpr => return c.condDummyRef,

        .CondExpr => {
            const cond = try c.genExpr(data.if3.cond);
            const then = try c.genExpr(c.tree.data[data.if3.body]);
            const @"else" = try c.genExpr(c.tree.data[data.if3.body + 1]);

            const branch = try c.builder.arena.allocator().create(Inst.Branch);
            branch.* = .{ .cond = cond, .then = then, .@"else" = @"else" };
            // TODO can't use select here
            return c.builder.addInst(.Select, .{ .branch = branch }, try c.genType(ty));
        },

        .CallExprOne => if (data.binExpr.rhs == .none) {
            return c.genCall(data.binExpr.lhs, &.{}, ty);
        } else {
            return c.genCall(data.binExpr.lhs, &.{data.binExpr.rhs}, ty);
        },

        .CallExpr => {
            return c.genCall(c.tree.data[data.range.start], c.tree.data[data.range.start + 1 .. data.range.end], ty);
        },

        .BoolOrExpr,
        .BoolAndExpr,
        .AddrOfLabel,
        .ImagExpr,
        .RealExpr,
        .ArrayAccessExpr,
        .BuiltinCallExprOne,
        .BuiltinCallExpr,
        .BuiltinChooseExpr,
        .MemberAccessExpr,
        .MemberAccessPtrExpr,
        .SizeOfExpr,
        .AlignOfExpr,
        .GenericExprOne,
        .GenericExpr,
        .GenericAssociationExpr,
        .GenericDefaultExpr,
        .StmtExpr,
        .ArrayInitExprTwo,
        .ArrayInitExpr,
        .StructInitExprTwo,
        .StructInitExpr,
        .UnionInitExpr,
        .CompoundLiteralExpr,
        .ArrayFillerExpr,
        .DefaultInitExpr,
        => return c.comp.diag.fatalNoSrc("TODO CodeGen.genExpr {}\n", .{c.nodeTag[@intFromEnum(node)]}),

        else => unreachable, // Not an expression.
    }
}

fn genLval(c: *CodeGen, node: NodeIndex) Error!IR.Ref {
    std.debug.assert(node != .none);
    std.debug.assert(Tree.isLValue(c.tree.nodes, c.tree.data, c.tree.valueMap, node));

    const data = c.nodeData[@intFromEnum(node)];
    switch (c.nodeTag[@intFromEnum(node)]) {
        .StringLiteralExpr => {
            const val = c.tree.valueMap.get(node).?.data.bytes;

            // TODO generate anonymous global
            const name = try std.fmt.allocPrintZ(c.builder.arena.allocator(), "\"{}\"", .{std.fmt.fmtSliceEscapeLower(val)});
            return c.builder.addInst(.Symbol, .{ .label = name }, .ptr);
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
            return c.builder.addInst(.Symbol, .{ .label = dupedName }, .ptr);
        },

        .DerefExpr => return c.genExpr(data.unExpr),
        else => return c.comp.diag.fatalNoSrc("TODO CodeGen.genLval {}\n", .{c.nodeTag[@intFromEnum(node)]}),
    }
}

fn genBoolExpr(c: *CodeGen, base: NodeIndex) Error!void {
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
                if (cond)
                    return c.builder.addJump(c.builder.branch.?.trueLabel);

                return c.genBoolExpr(data.binExpr.rhs);
            }

            const oldBranch = c.builder.branch;
            defer c.builder.branch = oldBranch;

            const falseLabel = try c.builder.makeLabel("bool_or.false");
            c.builder.branch = .{
                .trueLabel = c.builder.branch.?.trueLabel,
                .falseLabel = falseLabel,
            };

            try c.genBoolExpr(data.binExpr.lhs);
            try c.startBlock(falseLabel);
            c.builder.branch = .{
                .trueLabel = c.builder.branch.?.trueLabel,
                .falseLabel = oldBranch.?.falseLabel,
            };
            return c.genBoolExpr(data.binExpr.rhs);
        },

        .BoolAndExpr => {
            if (c.tree.valueMap.get(data.binExpr.lhs)) |lhs| {
                const cond = lhs.getBool();
                if (!cond)
                    return c.builder.addJump(c.builder.branch.?.falseLabel);

                return c.genBoolExpr(data.binExpr.rhs);
            }

            const oldBranch = c.builder.branch;
            defer c.builder.branch = oldBranch;

            const trueLabel = try c.builder.makeLabel("bool_and.true");
            c.builder.branch = .{
                .trueLabel = trueLabel,
                .falseLabel = c.builder.branch.?.falseLabel,
            };

            try c.genBoolExpr(data.binExpr.lhs);
            try c.startBlock(trueLabel);
            c.builder.branch = .{
                .trueLabel = oldBranch.?.trueLabel,
                .falseLabel = c.builder.branch.?.falseLabel,
            };
            return c.genBoolExpr(data.binExpr.rhs);
        },

        .BoolNotExpr => {
            const oldBranch = c.builder.branch;
            defer c.builder.branch = oldBranch;

            c.builder.branch = .{
                .trueLabel = c.builder.branch.?.falseLabel,
                .falseLabel = c.builder.branch.?.trueLabel,
            };
            return c.genBoolExpr(data.unExpr);
        },

        .EqualExpr => {
            const cmp = try c.genComparison(node, .CmpEQ);
            return c.builder.addBranch(cmp);
        },

        .NotEqualExpr => {
            const cmp = try c.genComparison(node, .CmpNE);
            return c.builder.addBranch(cmp);
        },

        .LessThanExpr => {
            const cmp = try c.genComparison(node, .CmpLT);
            return c.builder.addBranch(cmp);
        },

        .LessThanEqualExpr => {
            const cmp = try c.genComparison(node, .CmpLTE);
            return c.builder.addBranch(cmp);
        },

        .GreaterThanExpr => {
            const cmp = try c.genComparison(node, .CmpGT);
            return c.builder.addBranch(cmp);
        },

        .GreaterThanEqualExpr => {
            const cmp = try c.genComparison(node, .CmpGTE);
            return c.builder.addBranch(cmp);
        },

        .ExplicitCast, .ImplicitCast => switch (data.cast.kind) {
            .BoolToInt => {
                const operand = try c.genExpr(data.cast.operand);
                return c.builder.addBranch(operand);
            },
            else => {},
        },

        else => {},
    }

    // Assume int operand.
    const lhs = try c.genExpr(node);
    const rhs = try c.builder.addConstant(Value.int(0), try c.genType(c.nodeTy[@intFromEnum(node)]));
    const cmp = try c.builder.addInst(.CmpNE, .{ .bin = .{ .lhs = lhs, .rhs = rhs } }, .i1);
    try c.builder.addBranch(cmp);
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

                break :blk try c.builder.addInst(.Symbol, .{ .label = try c.builder.arena.allocator().dupeZ(u8, slice) }, .func);
            },
            else => break :blk try c.genExpr(fnNode),
        };
    };

    const args = try c.builder.arena.allocator().alloc(IR.Ref, argNodes.len);
    for (argNodes, 0..) |node, i| {
        // TODO handle calling convention here
        args[i] = try c.genExpr(node);
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

fn genVar(c: *CodeGen, _: NodeIndex) Error!void {
    return c.comp.diag.fatalNoSrc("TODO CodeGen.genVar\n", .{});
}
