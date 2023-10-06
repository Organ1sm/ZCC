const std = @import("std");
const Compilation = @import("../Basic/Compilation.zig");
const Source = @import("../Basic/Source.zig");
const Tree = @import("../AST/AST.zig");
const NodeIndex = Tree.NodeIndex;
const x86_64 = @import("../Arch/x86_64.zig");

const Codegen = @This();

comp: *Compilation,
text: std.ArrayList(u8),
rodata: std.ArrayList(u8),

pub const Error = Compilation.Error || error{CodegenFailed};

pub fn generateTree(comp: *Compilation, tree: Tree) Compilation.Error!void {
    var c = Codegen{
        .comp = comp,
        .text = std.ArrayList(u8).init(comp.gpa),
        .rodata = std.ArrayList(u8).init(comp.gpa),
    };
    defer {
        c.text.deinit();
        c.rodata.deinit();
    }

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
            => c.genFn() catch |err| switch (err) {
                error.FatalError => return error.FatalError,
                error.OutOfMemory => return error.OutOfMemory,
                error.CodegenFailed => continue,
            },

            .Var,
            .StaticVar,
            .ThreadlocalVar,
            .ThreadlocalStaticVar,
            => c.genVar() catch |err| switch (err) {
                error.FatalError => return error.FatalError,
                error.OutOfMemory => return error.OutOfMemory,
                error.CodegenFailed => continue,
            },

            else => unreachable,
        }
    }
}

fn genFn(c: *Codegen) Error!void {
    switch (c.comp.target.cpu.arch) {
        .x86_64 => try x86_64.genFn(c),
        else => return c.comp.diag.fatalNoSrc("implement genFn for target {}\n", .{c.comp.target.cpu.arch}),
    }
}

fn genVar(c: *Codegen) Error!void {
    switch (c.comp.target.cpu.arch) {
        .x86_64 => try x86_64.genVar(c),
        else => return c.comp.diag.fatalNoSrc("implement genVar for target {}\n", .{c.comp.target.cpu.arch}),
    }
}
