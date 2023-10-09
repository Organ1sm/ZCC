const std = @import("std");
const Tree = @import("../../AST/AST.zig");
const Codegen = @import("../../CodeGen/Codegen.zig");
const x86_64 = @import("deps").codegen.x86_64;
const Register = x86_64.Register;
const RegisterManager = @import("deps").RegisterManager;

const Fn = @This();

const Value = union(enum) {
    symbol: []const u8,
    immediate: u64,
    register: Register,
    none,
};

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
    try func.data.appendSlice(func.gpa, &.{
        0x55, // push rbp
        0x48,
        0x89,
        0xe5, // mov rbp, rsp
    });
    _ = try func.genNode(c.nodeData[@intFromEnum(decl)].Declaration.node);
    // all functions are guaranteed to end in a return statement so no extra work required here
}

pub fn spillInst(f: *Fn, reg: Register, inst: u32) !void {
    _ = inst;
    _ = reg;
    _ = f;
}

fn genNode(func: *Fn, node: Tree.NodeIndex) Codegen.Error!Value {
    if (func.c.tree.valueMap.get(node)) |some|
        return Value{ .immediate = some };

    const data = func.c.nodeData[@intFromEnum(node)];
    switch (func.c.nodeTag[@intFromEnum(node)]) {
        .CompoundStmtTwo => {
            if (data.BinaryExpr.lhs != .none) _ = try func.genNode(data.BinaryExpr.lhs);
            if (data.BinaryExpr.rhs != .none) _ = try func.genNode(data.BinaryExpr.rhs);
            return Value{ .none = {} };
        },

        .CompoundStmt => {
            for (func.c.tree.data[data.range.start..data.range.end]) |stmt|
                _ = try func.genNode(stmt);
            return Value{ .none = {} };
        },

        .CallExprOne => if (data.BinaryExpr.rhs != .none)
            return func.genCall(data.BinaryExpr.lhs, &.{data.BinaryExpr.rhs})
        else
            return func.genCall(data.BinaryExpr.lhs, &.{}),

        .CallExpr => return func.genCall(func.c.tree.data[data.range.start], func.c.tree.data[data.range.start + 1 .. data.range.end]),

        .FunctionToPointer,
        .ArrayToPointer,
        => return func.genNode(data.UnaryExpr), // no-op

        .DeclRefExpr => {
            // TODO locals and arguments
            return Value{ .symbol = func.c.tree.tokSlice(data.DeclarationRef) };
        },

        .ReturnStmt => {
            // TODO gen return value
            try func.data.appendSlice(func.gpa, &.{ 0x31, 0xc0 }); // xor eax
            try func.data.appendSlice(func.gpa, &.{
                0x5d, // pop rbp
                0xc3, // ret
            });
            return Value{ .none = {} };
        },

        .IntLiteral => return Value{ .immediate = data.Int },
        .StringLiteralExpr => {
            const strBytes = func.c.tree.strings[data.String.index..][0..data.String.len];
            const section = try func.c.obj.getSection(.strings);
            const start = section.items.len;
            try section.appendSlice(func.gpa, strBytes);
            const symbolName = try func.c.obj.declareSymbol(.strings, null, .Internal, .variable, start, strBytes.len);
            return Value{ .symbol = symbolName };
        },

        else => return func.c.comp.diag.fatalNoSrc("TODO x86_64 genNode {}\n", .{func.c.nodeTag[@intFromEnum(node)]}),
    }
}

fn genCall(func: *Fn, lhs: Tree.NodeIndex, args: []const Tree.NodeIndex) Codegen.Error!Value {
    _ = try func.genNode(lhs);
    for (args) |arg|
        _ = try func.genNode(arg);

    return Value{ .none = {} };
}

pub fn genVar(c: *Codegen, decl: Tree.NodeIndex) Codegen.Error!void {
    _ = c;
    _ = decl;
}
