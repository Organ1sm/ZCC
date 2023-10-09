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

pub fn generateTree(comp: *Compilation, tree: Tree) Compilation.Error!void {
    var c = Codegen{
        .comp = comp,
        .tree = tree,
        .obj = try Object.create(comp),
        .nodeTag = tree.nodes.items(.tag),
        .nodeData = tree.nodes.items(.data),
    };

    defer c.obj.deinit();

    const nodeTags = tree.nodes.items(.tag);
    for (tree.rootDecls) |decl| {
        switch (nodeTags[@intFromEnum(decl)]) {
            // these produce no code
            .StaticAssert, .TypeDef => {},

            // no work needed
            .FnProto,
            .StaticFnProto,
            .InlineFnProto,
            .InlineStaticFnProto,
            .NoreturnFnProto,
            .NoreturnStaticFnProto,
            .NoreturnInlineFnProto,
            .NoreturnInlineStaticFnProto,
            .ExternVar,
            .ThreadlocalExternVar,
            => {},

            // function definition
            .FnDef,
            .StaticFnDef,
            .InlineFnDef,
            .InlineStaticFnDef,
            .NoreturnFnDef,
            .NoreturnStaticFnDef,
            .NoreturnInlineFnDef,
            .NoreturnInlineStaticFnDef,
            => c.genFn(decl) catch |err| switch (err) {
                error.FatalError => return error.FatalError,
                error.OutOfMemory => return error.OutOfMemory,
                error.CodegenFailed => continue,
            },

            .Var,
            .StaticVar,
            .ThreadlocalVar,
            .ThreadlocalStaticVar,
            => c.genVar(decl) catch |err| switch (err) {
                error.FatalError => return error.FatalError,
                error.OutOfMemory => return error.OutOfMemory,
                error.CodegenFailed => continue,
            },

            else => unreachable,
        }
    }

    const outFileName = comp.outputName orelse "a.o";
    const outFile = std.fs.cwd().createFile(outFileName, .{}) catch |err|
        return comp.diag.fatalNoSrc("could not create output file '{s}': {s}", .{ outFileName, @errorName(err) });
    defer outFile.close();
    c.obj.finish(outFile) catch |err|
        return comp.diag.fatalNoSrc("could output to object file '{s}': {s}", .{ outFileName, @errorName(err) });
}

fn genFn(c: *Codegen, decl: NodeIndex) Error!void {
    const section: []const u8 = "text";
    const data = try c.obj.getSection(section);
    const startLen = data.items.len;
    switch (c.comp.target.cpu.arch) {
        .x86_64 => try x86_64.genFn(c, decl, data),
        else => return c.comp.diag.fatalNoSrc("implement genFn for target {}\n", .{c.comp.target.cpu.arch}),
    }
    const name = c.tree.tokSlice(c.nodeData[@intFromEnum(decl)].Declaration.name);
    try c.obj.declareSymbol(section, name, .Strong, .func, startLen, data.items.len - startLen);
}

fn genVar(c: *Codegen, decl: NodeIndex) Error!void {
    switch (c.comp.target.cpu.arch) {
        .x86_64 => try x86_64.genVar(c, decl),
        else => return c.comp.diag.fatalNoSrc("implement genVar for target {}\n", .{c.comp.target.cpu.arch}),
    }
}
