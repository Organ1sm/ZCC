const std = @import("std");
const Parser = @import("../Parser/Parser.zig");
const Tree = @import("AST.zig");
const Compilation = @import("../Basic/Compilation.zig");
const Builder = @import("TypeBuilder.zig").Builder;

const Type = @This();

const NodeIndex = Tree.NodeIndex;
const TokenIndex = Tree.TokenIndex;

data: union {
    subType: *Type,
    func: *Function,
    array: *Array,
    vla: *VLA,
    @"enum": *Enum,
    record: *Record,
    none: void,
} = .{ .none = {} },

qual: Qualifiers = .{},
specifier: Specifier,
alignment: u32 = 0,

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
    params: []Param,

    pub const Param = struct {
        name: []const u8,
        ty: Type,
        register: bool,
    };
};

pub const Array = struct {
    len: u64,
    elem: Type,
};

pub const VLA = struct {
    expr: NodeIndex,
    elem: Type,
};

pub const Enum = struct {
    name: []const u8,
    tagType: Type,
    fields: []Field,

    pub const Field = struct {
        name: []const u8,
        ty: Type,
        value: u64,
    };
};

pub const Record = struct {
    name: []const u8,
    fields: []Field,
    size: u32,
    alignment: u32,

    pub const Field = struct {
        name: []const u8,
        ty: Type,
        bitWidth: u32,
    };
};

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
    UnspecifiedVariableLenArray,

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
    IncompleteArray,
    VariableLenArray,

    IncompleteStruct,
    IncompleteUnion,
    IncompleteEnum,

    // data.record
    Struct,
    Union,

    // data.enum
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
        .Array, .StaticArray, .IncompleteArray, .VariableLenArray, .UnspecifiedVariableLenArray => true,
        else => false,
    };
}

pub fn isUnsignedInt(ty: Type, comp: *Compilation) bool {
    _ = comp;
    return switch (ty.specifier) {
        .UChar, .UShort, .UInt, .ULong, .ULongLong => return true,
        else => false,
    };
}

pub fn isEnumOrRecord(ty: Type) bool {
    return switch (ty.specifier) {
        .Enum, .Struct, .Union => true,
        else => false,
    };
}

pub fn getElemType(ty: Type) Type {
    return switch (ty.specifier) {
        .Pointer, .UnspecifiedVariableLenArray => ty.data.subType.*,
        .Array, .StaticArray, .IncompleteArray => ty.data.array.elem,
        .VariableLenArray => ty.data.array.elem,
        else => unreachable,
    };
}

pub fn wideChar(p: *Parser) Type {
    _ = p;
    return .{ .specifier = .Int };
}

pub fn hasIncompleteSize(ty: Type) bool {
    return switch (ty.specifier) {
        .Void,
        .IncompleteArray,
        .IncompleteStruct,
        .IncompleteUnion,
        .IncompleteEnum,
        => true,

        else => false,
    };
}

/// Size of type as reported by sizeof
pub fn sizeof(ty: Type, comp: *Compilation) ?u64 {
    // TODO get target from compilation
    return switch (ty.specifier) {
        .VariableLenArray,
        .UnspecifiedVariableLenArray,
        .IncompleteArray,
        => unreachable,

        .Func,
        .VarArgsFunc,
        .OldStyleFunc,
        .Void,
        .Bool,
        => 1,

        .Char, .SChar, .UChar => 1,
        .Short, .UShort => 2,
        .Int, .UInt => 4,

        .Long, .ULong, .LongLong, .ULongLong => switch (comp.target.os.tag) {
            .linux,
            .macos,
            .freebsd,
            .netbsd,
            .dragonfly,
            .openbsd,
            .wasi,
            .emscripten,
            => comp.target.ptrBitWidth() >> 3,

            .windows, .uefi => 32,

            else => 32,
        },
        .Float => 4,
        .Double => 8,
        .LongDouble => 16,
        .ComplexFloat => 8,
        .ComplexDouble => 16,
        .ComplexLongDouble => 32,
        .Pointer, .StaticArray => comp.target.ptrBitWidth() >> 3,
        .Array => ty.data.subType.sizeof(comp).? * ty.data.array.len,
        .Struct, .Union => ty.data.record.size,
        .Enum => ty.data.@"enum".tagType.sizeof(comp),

        .IncompleteStruct,
        .IncompleteUnion,
        .IncompleteEnum,
        => null,
    };
}

pub fn combine(inner: *Type, outer: Type, p: *Parser, sourceToken: TokenIndex) Parser.Error!void {
    switch (inner.specifier) {
        .Pointer => return inner.data.subType.combine(outer, p, sourceToken),

        .UnspecifiedVariableLenArray => return p.todo("combine [*] array"),

        .Array, .StaticArray, .IncompleteArray => {
            try inner.data.array.elem.combine(outer, p, sourceToken);

            if (inner.data.array.elem.hasIncompleteSize()) return p.errToken(.array_incomplete_elem, sourceToken);
            if (inner.data.array.elem.isFunc()) return p.errToken(.array_func_elem, sourceToken);
            if (inner.data.array.elem.specifier == .StaticArray and inner.isArray()) return p.errToken(.static_non_outermost_array, sourceToken);
            if (inner.data.array.elem.qual.any() and inner.isArray()) return p.errToken(.qualifier_non_outermost_array, sourceToken);
        },

        .VariableLenArray => {
            try inner.data.vla.elem.combine(outer, p, sourceToken);

            if (inner.data.vla.elem.hasIncompleteSize()) return p.errToken(.array_incomplete_elem, sourceToken);
            if (inner.data.vla.elem.isFunc()) return p.errToken(.array_func_elem, sourceToken);
            if (inner.data.vla.elem.qual.any() and inner.isArray()) return p.errToken(.qualifier_non_outermost_array, sourceToken);
        },

        .Func, .VarArgsFunc, .OldStyleFunc => {
            try inner.data.func.returnType.combine(outer, p, sourceToken);
            if (inner.data.func.returnType.isFunc()) return p.errToken(.func_cannot_return_func, sourceToken);
            if (inner.data.func.returnType.isArray()) return p.errToken(.func_cannot_return_array, sourceToken);
        },
        else => inner.* = outer,
    }
}

/// Useful for debugging, too noisy to be enabled by default.
const DumpDetailedContainers = false;

pub fn dump(ty: Type, w: anytype) @TypeOf(w).Error!void {
    try ty.qual.dump(w);
    switch (ty.specifier) {
        .Pointer => {
            try w.writeAll("*");
            try ty.data.subType.dump(w);
        },

        .Func, .VarArgsFunc, .OldStyleFunc => {
            try w.writeAll("fn (");
            for (ty.data.func.params, 0..) |param, i| {
                if (i != 0) try w.writeAll(", ");
                if (param.register)
                    try w.writeAll("register ");

                if (param.name.len != 0)
                    try w.print("{s}: ", .{param.name});
                try param.ty.dump(w);
            }

            if (ty.specifier != .Func) {
                if (ty.data.func.params.len != 0) try w.writeAll(", ");
                try w.writeAll("...");
            }

            try w.writeAll(") ");
            try ty.data.func.returnType.dump(w);
        },

        .Array, .StaticArray => {
            try w.writeAll("[");
            if (ty.specifier == .StaticArray) try w.writeAll("static ");
            try w.print("{d}]", .{ty.data.array.len});
            try ty.data.array.elem.dump(w);
        },

        .IncompleteArray => {
            try w.writeAll("[]");
            try ty.data.array.elem.dump(w);
        },

        .Enum => {
            try w.print("enum {s}", .{ty.data.@"enum".name});
            if (DumpDetailedContainers)
                try dumpEnum(ty.data.@"enum", w);
        },

        .Struct => {
            try w.print("struct {s}", .{ty.data.record.name});
            if (DumpDetailedContainers)
                try dumpRecord(ty.data.record, w);
        },

        .Union => {
            try w.print("union {s}", .{ty.data.record.name});
            if (DumpDetailedContainers)
                try dumpRecord(ty.data.record, w);
        },

        else => try w.writeAll(Builder.fromType(ty).toString()),
    }

    if (ty.alignment != 0)
        try w.print(" _Alignas({d})", .{ty.alignment});
}

fn dumpEnum(@"enum": *Enum, w: anytype) @TypeOf(w).Error!void {
    try w.writeAll(" {");
    for (@"enum".fields) |field| {
        try w.print(" {s} = {d},", .{ field.name, field.value });
    }
    try w.writeAll(" }");
}

fn dumpRecord(record: *Record, w: anytype) @TypeOf(w).Error!void {
    try w.writeAll(" {");
    for (record.fields) |field| {
        try w.writeByte(' ');
        try field.ty.dump(w);
        try w.print(" {s}: {d};", .{ field.name, field.bit_width });
    }
    try w.writeAll(" }");
}
