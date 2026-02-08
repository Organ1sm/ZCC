const std = @import("std");
const Allocator = std.mem.Allocator;
const assert = std.debug.assert;

const zinc = @import("zinc");
const Assembly = zinc.Assembly;
const Compilation = zinc.Compilation;
const Node = Tree.Node;
const Source = zinc.Source;
const Tree = zinc.Tree;
const QualType = zinc.QualType;
const Value = zinc.Value;

const AsmCodeGen = @This();
const Error = zinc.Compilation.Error;

tree: *const Tree,
comp: *Compilation,
text: *std.Io.Writer,
data: *std.Io.Writer,

const StorageUnit = enum(u8) {
    byte = 8,
    short = 16,
    long = 32,
    quad = 64,

    fn trunc(self: StorageUnit, val: u64) u64 {
        return switch (self) {
            .byte => @as(u8, @truncate(val)),
            .short => @as(u16, @truncate(val)),
            .long => @as(u32, @truncate(val)),
            .quad => val,
        };
    }
};

fn serializeInt(value: u64, storageUnit: StorageUnit, w: *std.Io.Writer) !void {
    try w.print("  .{s}  0x{x}\n", .{ @tagName(storageUnit), storageUnit.trunc(value) });
}

fn serializeFloat(comptime T: type, value: T, w: *std.Io.Writer) !void {
    switch (T) {
        f128 => {
            const bytes = std.mem.asBytes(&value);
            const first = std.mem.bytesToValue(u64, bytes[0..8]);
            try serializeInt(first, .quad, w);
            const second = std.mem.bytesToValue(u64, bytes[8..16]);
            return serializeInt(second, .quad, w);
        },
        f80 => {
            const bytes = std.mem.asBytes(&value);
            const first = std.mem.bytesToValue(u64, bytes[0..8]);
            try serializeInt(first, .quad, w);
            const second = std.mem.bytesToValue(u16, bytes[8..10]);
            try serializeInt(second, .short, w);
            return w.writeAll("  .zero 6\n");
        },
        else => {
            const size = @bitSizeOf(T);
            const storageUnit = std.meta.intToEnum(StorageUnit, size) catch unreachable;
            const IntTy = @Int(.unsigned, size);
            const intValue: IntTy = @bitCast(value);
            return serializeInt(intValue, storageUnit, w);
        },
    }
}

pub fn todo(c: *AsmCodeGen, msg: []const u8, token: Tree.TokenIndex) Error {
    const loc: Source.Location = c.tree.tokens.items(.loc)[token];

    var sf = std.heap.stackFallback(1024, c.comp.gpa);
    const allocator = sf.get();
    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);

    try buf.print(allocator, "TODO: {s}", .{msg});
    try c.comp.diagnostics.add(.{
        .text = buf.items,
        .kind = .@"error",
        .location = loc.expand(c.comp),
    });
    return error.FatalError;
}

fn emitAggregate(c: *AsmCodeGen, qt: QualType, node: Node.Index) !void {
    _ = qt;
    return c.todo("Codegen aggregates", node.tok(c.tree));
}

fn emitSingleValue(c: *AsmCodeGen, qt: QualType, node: Node.Index) !void {
    const value = c.tree.valueMap.get(node) orelse return;
    const bitSize = qt.bitSizeof(c.comp);
    const sk = qt.scalarKind(c.comp);
    if (!sk.isReal()) {
        return c.todo("Codegen _Complex values", node.tok(c.tree));
    } else if (sk.isInt()) {
        const storageUnit = std.meta.intToEnum(StorageUnit, bitSize) catch return c.todo("Codegen _BitInt values", node.tok(c.tree));
        try c.data.print("  .{s} ", .{@tagName(storageUnit)});
        _ = try value.print(qt, c.comp, c.data);
        try c.data.writeByte('\n');
    } else if (sk.isFloat()) {
        switch (bitSize) {
            16 => return serializeFloat(f16, value.toFloat(f16, c.comp), c.data),
            32 => return serializeFloat(f32, value.toFloat(f32, c.comp), c.data),
            64 => return serializeFloat(f64, value.toFloat(f64, c.comp), c.data),
            80 => return serializeFloat(f80, value.toFloat(f80, c.comp), c.data),
            128 => return serializeFloat(f128, value.toFloat(f128, c.comp), c.data),
            else => unreachable,
        }
    } else if (sk.isPointer()) {
        return c.todo("Codegen pointer", node.tok(c.tree));
    } else if (qt.is(c.comp, .array)) {
        // Todo:
        //  Handle truncated initializers e.g. char x[3] = "hello";
        //  Zero out remaining bytes if initializer is shorter than storage capacity
        //  Handle non-char strings
        const bytes = value.toBytes(c.comp);
        const directive = if (bytes.len > bitSize / 8) "ascii" else "string";
        try c.data.print("  .{s} ", .{directive});
        try Value.printString(bytes, qt, c.comp, c.data);

        try c.data.writeByte('\n');
    } else unreachable;
}

fn emitValue(c: *AsmCodeGen, qt: QualType, node: Node.Index) !void {
    switch (node.get(c.tree)) {
        .arrayInitExpr,
        .structInitExpr,
        .unionInitExpr,
        => return c.todo("Codegen multiple inits", node.tok(c.tree)),
        else => return c.emitSingleValue(qt, node),
    }
}

pub fn genAsm(tree: *const Tree) Error!Assembly {
    var data: std.Io.Writer.Allocating = .init(tree.comp.gpa);
    defer data.deinit();

    var text: std.Io.Writer.Allocating = .init(tree.comp.gpa);
    defer text.deinit();

    var codegen: AsmCodeGen = .{
        .tree = tree,
        .comp = tree.comp,
        .text = &text.writer,
        .data = &data.writer,
    };

    codegen.genDecls() catch |err| switch (err) {
        error.WriteFailed => return error.OutOfMemory,
        error.OutOfMemory => return error.OutOfMemory,
        error.FatalError => return error.FatalError,
    };

    const textSlice = try text.toOwnedSlice();
    errdefer tree.comp.gpa.free(textSlice);
    const dataSlice = try data.toOwnedSlice();
    return .{
        .text = textSlice,
        .data = dataSlice,
    };
}

fn genDecls(c: *AsmCodeGen) !void {
    if (c.tree.comp.codegenOptions.debug != .strip) {
        const sources = c.tree.comp.sources.values();
        for (sources) |source| {
            try c.data.print("  .file {d} \"{s}\"\n", .{ @intFromEnum(source.id) - 1, source.path });
        }
    }

    for (c.tree.rootDecls.items) |decl| {
        switch (decl.get(c.tree)) {
            .staticAssert,
            .typedef,
            .structDecl,
            .unionDecl,
            .enumDecl,
            => {},

            .function => |func| {
                if (func.body == null) continue;
                try c.genFn(func);
            },

            .variable => |variable| try c.genVar(variable),

            else => unreachable,
        }
    }
    try c.text.writeAll("  .section  .note.GNU-stack,\"\",@progbits\n");
}

fn genFn(c: *AsmCodeGen, func: Node.Function) !void {
    return c.todo("Codegen functions", func.nameToken);
}

fn genVar(c: *AsmCodeGen, variable: Node.Variable) !void {
    const comp = c.comp;
    const qt = variable.qt;

    const isTentative = variable.initializer == null;
    const size = qt.sizeofOrNull(comp) orelse blk: {
        // tentative array definition assumed to have one element
        std.debug.assert(isTentative and qt.is(c.comp, .array));
        break :blk qt.childType(c.comp).sizeof(comp);
    };

    const name = c.tree.tokenSlice(variable.nameToken);
    const natAlign = qt.alignof(comp);
    const alignment = if (qt.is(c.comp, .array) and size >= 16) @max(16, natAlign) else natAlign;

    if (variable.storageClass == .static) {
        try c.data.print("  .local \"{s}\"\n", .{name});
    } else {
        try c.data.print("  .globl \"{s}\"\n", .{name});
    }

    if (isTentative and comp.codegenOptions.common) {
        try c.data.print("  .comm \"{s}\", {d}, {d}\n", .{ name, size, alignment });
        return;
    }
    if (variable.initializer) |init| {
        if (variable.threadLocal and comp.codegenOptions.dataSections) {
            try c.data.print("  .section .tdata.\"{s}\",\"awT\",@progbits\n", .{name});
        } else if (variable.threadLocal) {
            try c.data.writeAll("  .section .tdata,\"awT\",@progbits\n");
        } else if (comp.codegenOptions.dataSections) {
            try c.data.print("  .section .data.\"{s}\",\"aw\",@progbits\n", .{name});
        } else {
            try c.data.writeAll("  .data\n");
        }

        try c.data.print("  .type \"{s}\", @object\n", .{name});
        try c.data.print("  .size \"{s}\", {d}\n", .{ name, size });
        try c.data.print("  .align {d}\n", .{alignment});
        try c.data.print("\"{s}\":\n", .{name});
        try c.emitValue(qt, init);
        return;
    }
    if (variable.threadLocal and comp.codegenOptions.dataSections) {
        try c.data.print("  .section .tbss.\"{s}\",\"awT\",@nobits\n", .{name});
    } else if (variable.threadLocal) {
        try c.data.writeAll("  .section .tbss,\"awT\",@nobits\n");
    } else if (comp.codegenOptions.dataSections) {
        try c.data.print("  .section .bss.\"{s}\",\"aw\",@nobits\n", .{name});
    } else {
        try c.data.writeAll("  .bss\n");
    }
    try c.data.print("  .align {d}\n", .{alignment});
    try c.data.print("\"{s}\":\n", .{name});
    try c.data.print("  .zero {d}\n", .{size});
}
