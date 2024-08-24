const std = @import("std");
const Compilation = @import("../Basic/Compilation.zig");
const Tree = @import("../AST/AST.zig");
const TreeTag = @import("../AST/AstTag.zig").Tag;
const NodeIndex = Tree.NodeIndex;
const Object = @import("../Object/Object.zig");
const x86_64 = @import("Arch/x86_64.zig");

const Codegen = @This();

comp: *Compilation,
tree: Tree,
obj: *Object,
nodeTag: []const TreeTag,
nodeData: []const Tree.Node.Data,

pub const Error = Compilation.Error || error{CodegenFailed};

/// Generate tree to an object file.
/// Caller is responsible for flushing and freeing the returned object.
pub fn generateTree(comp: *Compilation, tree: Tree) Compilation.Error!*Object {
    var c = Codegen{
        .comp = comp,
        .tree = tree,
        .obj = try Object.create(comp),
        .nodeTag = tree.nodes.items(.tag),
        .nodeData = tree.nodes.items(.data),
    };

    errdefer c.obj.deinit();

    const nodeTags = tree.nodes.items(.tag);
    for (tree.rootDecls) |decl| {
        switch (nodeTags[@intFromEnum(decl)]) {
            // these produce no code
            .StaticAssert,
            .TypeDef,
            .StructDeclTwo,
            .UnionDeclTwo,
            .EnumDeclTwo,
            .StructDecl,
            .UnionDecl,
            .EnumDecl,
            .StructForwardDecl,
            .UnionForwardDecl,
            .EnumForwardDecl,
            => {},

            // define symbol
            .FnProto,
            .StaticFnProto,
            .InlineFnProto,
            .InlineStaticFnProto,
            .ExternVar,
            .ThreadlocalExternVar,
            => {
                const name = c.tree.getTokenSlice(c.nodeData[@intFromEnum(decl)].decl.name);
                _ = try c.obj.declareSymbol(.undefined, name, .strong, .external, 0, 0);
            },

            // function definition
            .FnDef,
            .StaticFnDef,
            .InlineFnDef,
            .InlineStaticFnDef,
            => c.genFn(decl) catch |err| switch (err) {
                error.FatalError => return error.FatalError,
                error.OutOfMemory => return error.OutOfMemory,
                error.CodegenFailed => continue,
            },

            .Var,
            .StaticVar,
            .ThreadlocalVar,
            .ThreadlocalStaticVar,
            .ImplicitStaticVar,
            => c.genVar(decl) catch |err| switch (err) {
                error.FatalError => return error.FatalError,
                error.OutOfMemory => return error.OutOfMemory,
                error.CodegenFailed => continue,
            },

            // TOOD
            .FileScopeAsm => {},

            else => unreachable,
        }
    }

    return c.obj;
}

fn genFn(c: *Codegen, decl: NodeIndex) Error!void {
    const section: Object.Section = .func;
    const data = try c.obj.getSection(section);
    const startLen = data.items.len;
    switch (c.comp.target.cpu.arch) {
        .x86_64 => try x86_64.genFn(c, decl, data),
        else => unreachable,
    }
    const name = c.tree.getTokenSlice(c.nodeData[@intFromEnum(decl)].decl.name);
    _ = try c.obj.declareSymbol(section, name, .strong, .func, startLen, data.items.len - startLen);
}

fn genVar(c: *Codegen, decl: NodeIndex) Error!void {
    switch (c.comp.target.cpu.arch) {
        .x86_64 => try x86_64.genVar(c, decl),
        else => unreachable,
    }
}
