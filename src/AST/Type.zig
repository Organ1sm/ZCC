const std = @import("std");
const Parser = @import("../Parser/Parser.zig");
const Tree = @import("AST.zig");
const Builder = @import("TypeBuilder.zig").Builder;

const Type = @This();

const NodeIndex = Tree.NodeIndex;
const TokenIndex = Tree.TokenIndex;

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
    /// int foo(int bar, char baz) and int (void)
    Func,

    /// int foo(int bar, char baz, ...)
    VarArgsFunc,

    /// int foo(bar, baz) and int foo()
    /// is also var args, but give warnings
    OldStyleFunc,

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
        .Func, .VarArgsFunc, .OldStyleFunc => ty,
        .Pointer => ty.data.subType.isCallable(),
        else => null,
    };
}

pub fn isFunc(ty: Type) bool {
    return switch (ty.specifier) {
        .Func, .VarArgsFunc, .OldStyleFunc => true,
        else => false,
    };
}

pub fn isArray(ty: Type) bool {
    return switch (ty.specifier) {
        .Array, .StaticArray => true,
        else => false,
    };
}

pub fn combine(inner: *Type, outer: Type, p: *Parser, sourceToken: TokenIndex) Parser.Error!void {
    switch (inner.specifier) {
        .Pointer => return inner.data.subType.combine(outer, p, sourceToken),
        .Array, .StaticArray => return p.errStr(.todo, sourceToken, "combine array"),
        .Func, .VarArgsFunc, .OldStyleFunc => {
            try inner.data.func.returnType.combine(outer, p, sourceToken);
            switch (inner.data.func.returnType.specifier) {
                .Func, .VarArgsFunc, .OldStyleFunc => return p.errToken(.func_cannot_return_func, sourceToken),
                .Array, .StaticArray => return p.errToken(.func_cannot_return_array, sourceToken),
                else => {},
            }
        },
        else => inner.* = outer,
    }
}

pub fn dump(ty: Type, tree: Tree, w: anytype) @TypeOf(w).Error!void {
    try ty.qual.dump(w);
    switch (ty.specifier) {
        .Pointer => {
            try w.writeAll("*");
            try ty.data.subType.dump(tree, w);
        },

        .Atomic => {
            try w.writeAll("_Atomic");
            try ty.data.subType.dump(tree, w);
            try w.writeAll(")");
        },

        .Func, .VarArgsFunc, .OldStyleFunc => {
            try w.writeAll("fn (");
            for (ty.data.func.paramTypes, 0..) |param, i| {
                if (i != 0) try w.writeAll(", ");
                const nameToken = tree.nodes.items(.first)[param];
                if (tree.tokens[nameToken].id == .Identifier) {
                    try w.print("{s}: ", .{tree.tokSlice(nameToken)});
                }

                try tree.nodes.items(.type)[param].dump(tree, w);
            }

            if (ty.specifier == .VarArgsFunc) {
                if (ty.data.func.paramTypes.len != 0) try w.writeAll(", ");
                try w.writeAll("...");
            }

            try w.writeAll(") ");
            try ty.data.func.returnType.dump(tree, w);
        },
        .Array, .StaticArray => {
            try w.writeAll("[");
            if (ty.specifier == .StaticArray) try w.writeAll("static ");
            try w.print("{d}]", .{ty.data.array.len});
            try ty.data.array.elem.dump(tree, w);
        },
        else => {
            try w.writeAll(Builder.fromType(ty).toString());
        },
    }
}
