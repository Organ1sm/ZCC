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

fn genStmt(_: *CodeGen, _: NodeIndex) Error!void {}

fn genExpr(_: *CodeGen, _: NodeIndex) Error!IR.Ref {}

fn genLval(_: *CodeGen, _: NodeIndex) Error!IR.Ref {}

fn genBoolExpr(_: *CodeGen, _: NodeIndex) Error!void {}

fn genCall(_: *CodeGen, _: NodeIndex, _: []const NodeIndex, _: Type) Error!IR.Ref {}

fn genCompoundAssign(_: *CodeGen, _: NodeIndex, _: IR.Inst.Tag) Error!IR.Ref {}

fn genBinOp(_: *CodeGen, _: NodeIndex, _: IR.Inst.Tag) Error!IR.Ref {}

fn genComparison(_: *CodeGen, _: NodeIndex, _: IR.Inst.Tag) Error!IR.Ref {}

fn genPtrArithmetic(_: *CodeGen, _: IR.Ref, _: IR.Ref, _: Type, _: Type) Error!IR.Ref {}

fn genVar(_: *CodeGen, _: NodeIndex) Error!void {}
