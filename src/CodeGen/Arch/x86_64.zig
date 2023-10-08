const std = @import("std");
const Tree = @import("../../AST/AST.zig");
const Codegen = @import("../../CodeGen/Codegen.zig");
const x86_64 = @import("deps").codegen.x86_64;
const Register = x86_64.Register;
const RegisterManager = @import("deps").RegisterManager;

const Fn = @This();

registerManager: RegisterManager(Fn, Register, &x86_64.CalleePreservedRegs) = .{},
data: *std.ArrayListUnmanaged(u8),
gpa: std.mem.Allocator,
c: *Codegen,

pub fn deinit(func: *Fn) void {
    func.* = undefined;
}

pub fn genFn(c: *Codegen, decl: Tree.NodeIndex, data: *std.ArrayListUnmanaged(u8)) Codegen.Error!void {
    var func = Fn{
        .data = data,
        .gpa = c.comp.gpa,
        .c = c,
    };
    defer func.deinit();

    // function prologue
    try func.data.appendSlice(func.gpa, &.{ 0x55, 0x48, 0x89, 0xe5 });
    try func.genNode(c.nodeData[@intFromEnum(decl)].Declaration.node);
    // all functions are guaranteed to end in a return statement so no extra work required here
}

pub fn spillInst(f: *Fn, reg: Register, inst: u32) !void {
    _ = inst;
    _ = reg;
    _ = f;
}

fn genNode(func: *Fn, node: Tree.NodeIndex) Codegen.Error!void {
    const data = func.c.nodeData[@intFromEnum(node)];
    switch (func.c.nodeTag[@intFromEnum(node)]) {
        .CompoundStmtTwo => {
            if (data.BinaryExpr.lhs != .none) try func.genNode(data.BinaryExpr.lhs);
            if (data.BinaryExpr.rhs != .none) try func.genNode(data.BinaryExpr.rhs);
        },

        .CompoundStmt => for (func.c.tree.data[data.range.start..data.range.end]) |stmt| {
            try func.genNode(stmt);
        },

        .CallExprOne => if (data.BinaryExpr.rhs != .none)
            try func.genCall(data.BinaryExpr.lhs, &.{data.BinaryExpr.rhs})
        else
            try func.genCall(data.BinaryExpr.lhs, &.{}),

        .CallExpr => try func.genCall(func.c.tree.data[data.range.start], func.c.tree.data[data.range.start + 1 .. data.range.end]),
        .FunctionToPointer => try func.genNode(data.UnaryExpr), // no-op
        .ArrayToPointer => try func.genNode(data.UnaryExpr), // no-op
        .DeclRefExpr => {},
        .ReturnStmt => {
            //TODO: generate return value
            try func.data.appendSlice(func.gpa, &.{ 0x48, 0x83, 0xc4, 0x10, 0x5d, 0xc3 });
        },
        .IntLiteral => {},
        .StringLiteralExpr => {},
        else => return func.c.comp.diag.fatalNoSrc("TODO x86_64 genNode {}\n", .{func.c.nodeTag[@intFromEnum(node)]}),
    }
}

fn genCall(func: *Fn, lhs: Tree.NodeIndex, args: []const Tree.NodeIndex) Codegen.Error!void {
    _ = args;
    _ = lhs;
    _ = func;
}

pub fn genVar(c: *Codegen, decl: Tree.NodeIndex) Codegen.Error!void {
    _ = c;
    _ = decl;
}
