const std = @import("std");
const Allocator = std.mem.Allocator;
const Compilation = @import("../Basic/Compilation.zig");
const Tree = @import("../AST/AST.zig");
const TreeTag = @import("../AST/AstTag.zig").Tag;
const NodeIndex = Tree.NodeIndex;
const IR = @import("IR.zig");
const IRBuilder = IR.Builder;
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
        val: Interner.Ref,
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
    defer c.symbols.deinit(c.builder.gpa);

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
    c.returnLabel = try c.builder.addLabel("return");
    try c.genStmt(c.nodeData[@intFromEnum(decl)].decl.node);

    // Relocate returns
    try c.builder.body.append(c.builder.gpa, c.returnLabel);
    _ = try c.builder.addInst(.Ret, undefined, .noreturn);

    var res = IR{
        .pool = c.builder.pool,
        .instructions = c.builder.instructions,
        .arena = c.builder.arena.state,
        .body = c.builder.body,
    };

    res.dump(name, c.comp.diag.color, std.io.getStdOut().writer()) catch {};
}

fn addUn(c: *CodeGen, tag: IR.Inst.Tag, operand: IR.Ref, ty: Type) !IR.Ref {
    return c.builder.addInst(tag, .{ .un = operand }, try c.genType(ty));
}

fn addBin(c: *CodeGen, tag: IR.Inst.Tag, lhs: IR.Ref, rhs: IR.Ref, ty: Type) !IR.Ref {
    return c.builder.addInst(tag, .{ .bin = .{ .lhs = lhs, .rhs = rhs } }, try c.genType(ty));
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

        .ReturnStmt => {
            if (data.unExpr != .none) {
                const operand = try c.genExpr(data.unExpr);
                _ = try c.builder.addInst(.RetValue, .{ .un = operand }, .void);
            }
            try c.builder.addJump(c.returnLabel);
        },

        .ImplicitReturn => {
            if (data.returnZero) {
                const operand = try c.builder.addConstant(Value.int(0), try c.genType(ty));
                _ = try c.builder.addInst(.RetValue, .{ .un = operand }, .void);
            }
            // No need to emit a jump since implicit_return is always the last instruction.
        },

        else => _ = try c.genExpr(node),
    }
}

fn genExpr(c: *CodeGen, node: NodeIndex) Error!IR.Ref {
    std.debug.assert(node != .none);

    const ty = c.nodeTy[@intFromEnum(node)];
    if (c.tree.valueMap.get(node)) |val|
        return c.builder.addConstant(val, try c.genType(ty));

    const data = c.nodeData[@intFromEnum(node)];
    switch (c.nodeTag[@intFromEnum(node)]) {
        .EnumerationRef,
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
            const operand = try c.genExpr(data.unExpr);
            const val = try c.addUn(.Load, operand, ty);
            const one = try c.builder.addConstant(Value.int(1), try c.genType(ty));
            const plusOne = try c.addBin(.Add, val, one, ty);
            try c.builder.addStore(operand, plusOne);
            return plusOne;
        },
        .PreDecExpr => {
            const operand = try c.genExpr(data.unExpr);
            const val = try c.addUn(.Load, operand, ty);
            const one = try c.builder.addConstant(Value.int(1), try c.genType(ty));
            const decOne = try c.addBin(.Sub, val, one, ty);
            try c.builder.addStore(operand, decOne);
            return decOne;
        },

        .PostIncExpr => {
            const operand = try c.genExpr(data.unExpr);
            const val = try c.addUn(.Load, operand, ty);
            const one = try c.builder.addConstant(Value.int(1), try c.genType(ty));
            const plusOne = try c.addBin(.Add, val, one, ty);
            try c.builder.addStore(operand, plusOne);
            return val;
        },
        .PostDecExpr => {
            const operand = try c.genExpr(data.unExpr);
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

            const branch = try c.builder.arena.allocator().create(IR.Inst.Branch);
            branch.* = .{ .cond = cond, .then = then, .@"else" = @"else" };

            // TODO can't use select here
            return c.builder.addInst(.Select, .{ .branch = branch }, try c.genType(ty));
        },

        .CondDummyExpr => return c.condDummyRef,

        .CondExpr => {
            const cond = try c.genExpr(data.if3.cond);
            const then = try c.genExpr(c.tree.data[data.if3.body]);
            const @"else" = try c.genExpr(c.tree.data[data.if3.body + 1]);

            const branch = try c.builder.arena.allocator().create(IR.Inst.Branch);
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

fn genBoolExpr(_: *CodeGen, _: NodeIndex) Error!void {}

fn genCall(c: *CodeGen, _: NodeIndex, _: []const NodeIndex, _: Type) Error!IR.Ref {
    return c.comp.diag.fatalNoSrc("TODO CodeGen.genCall\n", .{});
}

fn genCompoundAssign(c: *CodeGen, node: NodeIndex, tag: IR.Inst.Tag) Error!IR.Ref {
    const bin = c.nodeData[@intFromEnum(node)].binExpr;
    const ty = c.nodeTy[@intFromEnum(node)];
    const rhs = try c.genExpr(bin.rhs);
    const lhs = try c.genLval(bin.lhs);
    const res = try c.addBin(tag, lhs, rhs, ty);

    try c.builder.addStore(lhs, res);
    return res;
}

fn genBinOp(c: *CodeGen, node: NodeIndex, tag: IR.Inst.Tag) Error!IR.Ref {
    const bin = c.nodeData[@intFromEnum(node)].binExpr;
    const ty = c.nodeTy[@intFromEnum(node)];
    const lhs = try c.genExpr(bin.lhs);
    const rhs = try c.genExpr(bin.rhs);
    return c.addBin(tag, lhs, rhs, ty);
}

fn genComparison(c: *CodeGen, node: NodeIndex, tag: IR.Inst.Tag) Error!IR.Ref {
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
