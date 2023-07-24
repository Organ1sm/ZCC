const std = @import("std");
const Parser = @import("../Parser/Parser.zig");
const Tree = @import("AST.zig");
const Builder = @import("TypeBuilder.zig").Builder;

const Type = @This();

const NodeIndex = @import("AST.zig").NodeIndex;

pub const Qualifiers = packed struct {
    @"const": bool = false,
    atomic: bool = false,
    @"volatile": bool = false,
    restrict: bool = false,

    pub fn any(quals: Qualifiers) bool {
        return quals.@"const" or quals.restrict or quals.@"volatile" or quals.atomic;
    }

    pub fn dump(quals: Qualifiers, w: anytype) !void {
        if (quals.@"const") try w.writeAll(" const ");
        if (quals.atomic) try w.writeAll(" _Atomic ");
        if (quals.@"volatile") try w.writeAll("volatile ");
        if (quals.restrict) try w.writeAll(" restrict ");
    }
};
pub const Function = struct {
    returnType: Type,
    paramTypes: []NodeIndex,
};

pub const Array = struct {
    len: u64,
    elem: Type,
};

data: union {
    subType: *Type,
    func: *Function,
    array: *Array,
    node: NodeIndex,
    none: void,
} = .{ .none = {} },

qual: Qualifiers = .{},
specifier: Specifier,
alignment: u32 = 0,

pub const Specifier = enum {
    Void,
    Bool,

    // integers
    Char,
    SChar,
    UChar,
    Short,
    UShort,
    Int,
    UInt,
    Long,
    ULong,
    LongLong,
    ULongLong,

    // floating point numbers
    Float,
    Double,
    LongDouble,
    ComplexFloat,
    ComplexDouble,
    ComplexLongDouble,

    // data.SubType
    Pointer,
    Atomic,
    // data.func
    Func,
    VarArgsFunc,

    // data.array
    Array,
    StaticArray,

    // data.node
    Struct,
    Union,
    Enum,
};

pub fn isCallable(ty: Type) ?Type {
    return switch (ty.specifier) {
        .Func, .VarArgsFunc => ty,
        .Pointer => ty.data.subType.isCallable(),
        else => null,
    };
}

pub fn isFunc(ty: Type) bool {
    return switch (ty.specifier) {
        .Func, .VarArgsFunc => true,
        else => false,
    };
}

pub fn isArray(ty: Type) bool {
    return switch (ty.specifier) {
        .Array, .StaticArray => true,
        else => false,
    };
}

pub fn combine(inner: Type, outer: Type, p: *Parser) !Type {
    switch (inner.specifier) {
        .Pointer => {
            var res = inner;
            res.data.subType.* = outer;
            return res;
        },
        .Array, .StaticArray => return p.todo("combine array"),
        .Func, .VarArgsFunc => return p.todo("combine func"),
        else => return outer,
    }
}

pub fn dump(ty: Type, tree: Tree, w: anytype) @TypeOf(w).Error!void {
    switch (ty.specifier) {
        .Pointer => {
            try ty.data.subType.dump(tree, w);
            try w.writeAll("*");
            try ty.qual.dump(w);
        },
        .Atomic => {
            try w.writeAll("_Atomic");
            try ty.data.subType.dump(tree, w);
            try w.writeAll(")");
            try ty.qual.dump(w);
        },
        .Func, .VarArgsFunc => {
            try ty.data.func.returnType.dump(tree, w);
            try w.writeAll(" (");
            for (ty.data.func.paramTypes, 0..) |param, i| {
                if (i != 0) try w.writeAll(", ");
                try tree.nodes.items(.type)[param].dump(tree, w);
                const nameToken = tree.nodes.items(.first)[param];
                if (tree.tokens[nameToken].id == .Identifier) {
                    try w.print(" {s}", .{tree.tokSlice(nameToken)});
                }
            }

            if (ty.specifier == .VarArgsFunc) {
                if (ty.data.func.paramTypes.len != 0) try w.writeAll(", ");
                try w.writeAll("...");
            }
            try w.writeAll(")");
            try ty.qual.dump(w);
        },
        .Array, .StaticArray => {
            try ty.data.array.elem.dump(tree, w);
            try w.print("[{d}", .{ty.data.array.len});
            try ty.qual.dump(w);
            if (ty.specifier == .StaticArray) try w.writeAll(" static");
            try w.writeAll("]");
        },
        else => {
            try w.writeAll(Builder.fromType(ty).toString());
            try ty.qual.dump(w);
        },
    }
}
