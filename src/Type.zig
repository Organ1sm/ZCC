const std = @import("std");
const Parser = @import("Parser.zig");
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

    pub fn dump(quals: Qualifiers) void {
        if (quals.@"const") std.debug.print(" const", .{});
        if (quals.atomic) std.debug.print(" _Atomic", .{});
        if (quals.@"volatile") std.debug.print(" volatile", .{});
        if (quals.restrict) std.debug.print(" restrict", .{});
    }
};
pub const Function = struct {
    returnType: Type,
    paramTypes: []NodeIndex,
};

pub const Array = struct {
    qual: Qualifiers,
    len: u64,
    static: bool,
    elem: *Type,
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

pub fn dump(ty: Type, tree: Tree) void {
    switch (ty.specifier) {
        .Pointer => {
            ty.data.subType.dump(tree);
            std.debug.print("*", .{});
            ty.qual.dump();
        },

        .Atomic => {
            std.debug.print("_Atomic", .{});
            ty.data.subType.dump(tree);
            std.debug.print(")", .{});
            ty.qual.dump();
        },

        .Func, .VarArgsFunc => {
            ty.data.func.returnType.dump(tree);
            std.debug.print(" (", .{});
            for (ty.data.func.paramTypes, 0..) |param, i| {
                if (i != 0) std.debug.print(", ", .{});

                tree.nodes.items(.type)[param].dump(tree);

                const nameToken = tree.nodes.items(.first)[param];
                if (tree.tokens[nameToken].id == .Identifier) {
                    std.debug.print(" {s}", .{tree.tokSlice(nameToken)});
                }
            }
            if (ty.specifier == .VarArgsFunc) {
                if (ty.data.func.paramTypes.len != 0)
                    std.debug.print(", ", .{});
                std.debug.print("...", .{});
            }
            std.debug.print(")", .{});
            ty.qual.dump();
        },

        .Array, .StaticArray => {
            ty.data.array.elem.dump(tree);

            std.debug.print("[{d}", .{ty.data.array.len});

            ty.qual.dump();
            if (ty.specifier == .StaticArray)
                std.debug.print(" static", .{});

            std.debug.print(")", .{});
        },
        else => {
            std.debug.print("{s}", .{Builder.fromType(ty).toString()});
            ty.qual.dump();
        },
    }
}
