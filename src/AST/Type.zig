const std = @import("std");
const Parser = @import("../Parser/Parser.zig");
const Tree = @import("AST.zig");
const Compilation = @import("../Basic/Compilation.zig");
const TypeBuilder = @import("TypeBuilder.zig");

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

/// user requested alignment, to get type alignment use `alignof`
alignment: u29 = 0,

pub const Qualifiers = packed struct {
    @"const": bool = false,
    atomic: bool = false,
    @"volatile": bool = false,

    // for function parameters only, stored here since it fits in the padding
    register: bool = false,
    restrict: bool = false,

    pub fn any(quals: Qualifiers) bool {
        return quals.@"const" or quals.restrict or quals.@"volatile" or quals.atomic;
    }

    pub fn dump(quals: Qualifiers, w: anytype) !void {
        if (quals.@"const") try w.writeAll("const ");
        if (quals.atomic) try w.writeAll("_Atomic ");
        if (quals.@"volatile") try w.writeAll("volatile ");
        if (quals.restrict) try w.writeAll("restrict ");
        if (quals.register) try w.writeAll("register ");
    }

    /// Merge the const/volatile/ qualifiers, used by type resolution
    /// of the conditional operator
    pub fn mergeCVQualifiers(a: Qualifiers, b: Qualifiers) Qualifiers {
        return .{
            .@"const" = a.@"const" or b.@"const",
            .@"volatile" = a.@"volatile" or b.@"volatile",
        };
    }

    /// Merge all qualifiers, used by typeof()
    pub fn mergeAllQualifiers(a: Qualifiers, b: Qualifiers) Qualifiers {
        return .{
            .@"const" = a.@"const" or b.@"const",
            .atomic = a.atomic or b.atomic,
            .@"volatile" = a.@"volatile" or b.@"volatile",
            .restrict = a.restrict or b.restrict,
            .register = a.register or b.register,
        };
    }

    pub const Builder = struct {
        @"const": ?TokenIndex = null,
        atomic: ?TokenIndex = null,
        @"volatile": ?TokenIndex = null,
        restrict: ?TokenIndex = null,

        pub fn finish(b: Qualifiers.Builder, p: *Parser, ty: *Type) !void {
            if (ty.specifier != .Pointer and b.restrict != null) {
                try p.errStr(.restrict_non_pointer, b.restrict.?, try p.typeStr(ty.*));
            }
            if (b.atomic) |some| {
                if (ty.isArray()) try p.errStr(.atomic_array, some, try p.typeStr(ty.*));
                if (ty.isFunc()) try p.errStr(.atomic_func, some, try p.typeStr(ty.*));
                if (ty.hasIncompleteSize()) try p.errStr(.atomic_incomplete, some, try p.typeStr(ty.*));
            }

            ty.qual = .{
                .@"const" = b.@"const" != null,
                .atomic = b.atomic != null,
                .@"volatile" = b.@"volatile" != null,
                .restrict = b.restrict != null,
            };
        }
    };
};

pub const Function = struct {
    returnType: Type,
    params: []Param,

    pub const Param = struct {
        name: []const u8,
        ty: Type,
        nameToken: TokenIndex,
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

    pub fn isIncomplete(e: Enum) bool {
        return e.fields.len == std.math.maxInt(usize);
    }

    pub fn create(allocator: std.mem.Allocator, name: []const u8) !*Enum {
        var e = try allocator.create(Enum);
        e.name = name;
        e.fields.len = std.math.maxInt(usize);
        return e;
    }
};

pub const Record = struct {
    name: []const u8,
    fields: []Field,
    size: u64,
    alignment: u29,

    pub const Field = struct {
        name: []const u8,
        ty: Type,
        bitWidth: u32,
    };

    pub fn isIncomplete(r: Record) bool {
        return r.fields.len == std.math.maxInt(usize);
    }

    pub fn create(allocator: std.mem.Allocator, name: []const u8) !*Record {
        var r = try allocator.create(Record);
        r.name = name;
        r.fields.len = std.math.maxInt(usize);
        return r;
    }
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
    DecayedUnspecifiedVariableLenArray,

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
    DecayedArray,
    StaticArray,
    DecayedStaticArray,
    IncompleteArray,
    DecayedIncompleteArray,
    VariableLenArray,
    DecayedVariableLenArray,

    // data.record
    Struct,
    Union,

    // data.enum
    Enum,
};

pub fn isCallable(ty: Type) ?Type {
    return switch (ty.specifier) {
        .Func, .VarArgsFunc, .OldStyleFunc => ty,
        .Pointer => if (ty.data.subType.isFunc()) ty.data.subType.* else null,
        else => null,
    };
}

pub fn isFunc(ty: Type) bool {
    return switch (ty.specifier) {
        .Func, .VarArgsFunc, .OldStyleFunc => true,
        else => false,
    };
}

pub fn isPointer(ty: Type) bool {
    return switch (ty.specifier) {
        .Pointer,
        .DecayedArray,
        .DecayedStaticArray,
        .DecayedIncompleteArray,
        .DecayedVariableLenArray,
        .DecayedUnspecifiedVariableLenArray,
        => true,
        else => false,
    };
}

pub fn isArray(ty: Type) bool {
    return switch (ty.specifier) {
        .Array, .StaticArray, .IncompleteArray, .VariableLenArray, .UnspecifiedVariableLenArray => true,
        else => false,
    };
}

pub fn isInt(ty: Type) bool {
    return switch (ty.specifier) {
        .Enum,
        .Bool,
        .Char,
        .SChar,
        .UChar,
        .Short,
        .UShort,
        .Int,
        .UInt,
        .Long,
        .ULong,
        .LongLong,
        .ULongLong,
        => true,
        else => false,
    };
}

pub fn isFloat(ty: Type) bool {
    return switch (ty.specifier) {
        .Float,
        .Double,
        .LongDouble,
        .ComplexFloat,
        .ComplexDouble,
        .ComplexLongDouble,
        => true,
        else => false,
    };
}

pub fn isReal(ty: Type) bool {
    return switch (ty.specifier) {
        .ComplexFloat, .ComplexDouble, .ComplexLongDouble => false,
        else => true,
    };
}

// is the 'void *'
pub fn isVoidStar(ty: Type) bool {
    return switch (ty.specifier) {
        .Pointer => ty.data.subType.specifier == .Void,
        else => false,
    };
}

pub fn isUnsignedInt(ty: Type, comp: *const Compilation) bool {
    return switch (ty.specifier) {
        .Char => return getCharSignedness(comp) == .unsigned,
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

pub fn isRecord(ty: Type) bool {
    return switch (ty.specifier) {
        .Struct, .Union => true,
        else => false,
    };
}

pub fn getElemType(ty: Type) Type {
    return switch (ty.specifier) {
        .Pointer,
        .UnspecifiedVariableLenArray,
        .DecayedUnspecifiedVariableLenArray,
        => ty.data.subType.*,

        .Array,
        .StaticArray,
        .IncompleteArray,
        .DecayedArray,
        .DecayedStaticArray,
        .DecayedIncompleteArray,
        => ty.data.array.elem,

        .VariableLenArray, .DecayedVariableLenArray => ty.data.vla.elem,
        else => unreachable,
    };
}

pub fn eitherLongDouble(a: Type, b: Type) ?Type {
    if (a.specifier == .LongDouble or a.specifier == .ComplexLongDouble) return a;
    if (b.specifier == .LongDouble or b.specifier == .ComplexLongDouble) return b;
    return null;
}

pub fn eitherDouble(a: Type, b: Type) ?Type {
    if (a.specifier == .Double or a.specifier == .ComplexDouble) return a;
    if (b.specifier == .Double or b.specifier == .ComplexDouble) return b;
    return null;
}

pub fn eitherFloat(a: Type, b: Type) ?Type {
    if (a.specifier == .Float or a.specifier == .ComplexFloat) return a;
    if (b.specifier == .Float or b.specifier == .ComplexFloat) return b;
    return null;
}

pub fn integerPromotion(ty: Type, comp: *Compilation) Type {
    var specifier = ty.specifier;
    if (specifier == .Enum)
        specifier = ty.data.@"enum".tagType.specifier;

    return .{
        .specifier = switch (specifier) {
            .Bool, .Char, .SChar, .UChar, .Short => .Int,
            .UShort => if (ty.sizeof(comp).? == sizeof(.{ .specifier = .Int }, comp)) Specifier.UInt else Specifier.Int,
            .Int => .Int,
            .UInt => .UInt,
            .Long => .Long,
            .ULong => .ULong,
            .LongLong => .LongLong,
            .ULongLong => .ULongLong,
            else => unreachable, // not an integer type
        },
    };
}

pub fn wideChar(comp: *Compilation) Type {
    const os = comp.target.os.tag;
    return switch (comp.target.cpu.arch) {
        .xcore => .{ .specifier = .UChar },
        .ve => .{ .specifier = .UInt },

        .arm, .armeb, .thumb, .thumbeb => .{
            .specifier = if (os != .windows and os != .netbsd and os != .openbsd) .UInt else .Int,
        },

        .aarch64, .aarch64_be, .aarch64_32 => .{
            .specifier = if (!os.isDarwin() and os != .netbsd) .UInt else .Int,
        },

        .x86_64, .x86 => .{ .specifier = if (os == .windows) .UShort else .Int },
        else => .{ .specifier = .Int },
    };
}

pub fn ptrDiffT(comp: *Compilation) Type {
    if (comp.target.os.tag == .windows and comp.target.ptrBitWidth() == 64)
        return .{ .specifier = .LongLong };

    return switch (comp.target.ptrBitWidth()) {
        32 => .{ .specifier = .Int },
        64 => .{ .specifier = .Long },
        else => unreachable,
    };
}

pub fn sizeT(comp: *Compilation) Type {
    if (comp.target.os.tag == .windows and comp.target.ptrBitWidth() == 64)
        return .{ .specifier = .ULongLong };

    return switch (comp.target.ptrBitWidth()) {
        32 => .{ .specifier = .UInt },
        64 => .{ .specifier = .ULong },
        else => unreachable,
    };
}

pub fn hasIncompleteSize(ty: Type) bool {
    return switch (ty.specifier) {
        .Void,
        .IncompleteArray,
        => true,

        .Enum => ty.data.@"enum".isIncomplete(),
        .Struct => ty.data.record.isIncomplete(),

        else => false,
    };
}

pub fn hasUnboundVLA(ty: Type) bool {
    var cur = ty;
    while (true) {
        switch (cur.specifier) {
            .UnspecifiedVariableLenArray,
            .DecayedUnspecifiedVariableLenArray,
            => return true,

            .Array,
            .StaticArray,
            .IncompleteArray,
            .VariableLenArray,
            .DecayedArray,
            .DecayedStaticArray,
            .DecayedIncompleteArray,
            .DecayedVariableLenArray,
            => cur = cur.getElemType(),

            else => return false,
        }
    }
}

const FieldAndIndex = struct { f: Record.Field, i: usize };
pub fn getField(ty: Type, name: []const u8) ?FieldAndIndex {
    // TODO deal with anonymous struct
    switch (ty.specifier) {
        .Struct => {
            std.debug.assert(!ty.data.record.isIncomplete());
            for (ty.data.record.fields, 0..) |f, i| {
                if (std.mem.eql(u8, name, f.name)) return FieldAndIndex{ .f = f, .i = i };
            }
        },
        .Union => {
            std.debug.assert(!ty.data.record.isIncomplete());
            for (ty.data.record.fields, 0..) |f, i| {
                if (std.mem.eql(u8, name, f.name)) return FieldAndIndex{ .f = f, .i = i };
            }
        },
        else => unreachable,
    }
    return null;
}

pub fn maxInt(ty: Type, comp: *const Compilation) u64 {
    std.debug.assert(ty.isInt());
    return switch (ty.sizeof(comp).?) {
        1 => if (ty.isUnsignedInt(comp)) @as(u64, std.math.maxInt(u8)) else std.math.maxInt(i8),
        2 => if (ty.isUnsignedInt(comp)) @as(u64, std.math.maxInt(u16)) else std.math.maxInt(i16),
        4 => if (ty.isUnsignedInt(comp)) @as(u64, std.math.maxInt(u32)) else std.math.maxInt(i32),
        8 => if (ty.isUnsignedInt(comp)) @as(u64, std.math.maxInt(u64)) else std.math.maxInt(i64),
        else => unreachable,
    };
}

pub fn getCharSignedness(comp: *const Compilation) std.builtin.Signedness {
    switch (comp.target.cpu.arch) {
        .aarch64,
        .aarch64_32,
        .aarch64_be,
        .arm,
        .armeb,
        .thumb,
        .thumbeb,
        => return if (comp.target.os.tag.isDarwin() or comp.target.os.tag == .windows) .signed else .unsigned,

        .powerpc, .powerpc64 => return if (comp.target.os.tag.isDarwin()) .signed else .unsigned,
        .powerpc64le,
        .s390x,
        .xcore,
        .arc,
        => return .unsigned,

        else => return .signed,
    }
}

/// Size of type as reported by sizeof
pub fn sizeof(ty: Type, comp: *const Compilation) ?u64 {
    // TODO get target from compilation
    return switch (ty.specifier) {
        .VariableLenArray,
        .UnspecifiedVariableLenArray,
        .IncompleteArray,
        => return null,

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

            .windows, .uefi => 4,

            else => 4,
        },

        .Float => 4,
        .Double => 8,
        .LongDouble => 16,
        .ComplexFloat => 8,
        .ComplexDouble => 16,
        .ComplexLongDouble => 32,

        .Pointer,
        .StaticArray,
        .DecayedArray,
        .DecayedStaticArray,
        .DecayedIncompleteArray,
        .DecayedVariableLenArray,
        .DecayedUnspecifiedVariableLenArray,
        => comp.target.ptrBitWidth() >> 3,

        .Array => ty.data.array.elem.sizeof(comp).? * ty.data.array.len,
        .Struct, .Union => if (ty.data.record.isIncomplete()) null else ty.data.record.size,
        .Enum => if (ty.data.@"enum".isIncomplete()) null else ty.data.@"enum".tagType.sizeof(comp),
    };
}

/// Get the alignment of a type
pub fn alignof(ty: Type, comp: *Compilation) u29 {
    if (ty.alignment != 0)
        return ty.alignment;

    // TODO get target from compilation
    return switch (ty.specifier) {
        .UnspecifiedVariableLenArray => unreachable, // must be bound in function definition
        .VariableLenArray, .IncompleteArray => ty.getElemType().alignof(comp),
        .Func, .VarArgsFunc, .OldStyleFunc => 4, // TODO check target
        .Char, .SChar, .UChar, .Void, .Bool => 1,
        .Short, .UShort => 2,
        .Int, .UInt => 4,
        .Long, .ULong => switch (comp.target.os.tag) {
            .linux,
            .macos,
            .freebsd,
            .netbsd,
            .dragonfly,
            .openbsd,
            .wasi,
            .emscripten,
            => comp.target.ptrBitWidth() >> 3,
            .windows, .uefi => 4,
            else => 4,
        },

        .LongLong, .ULongLong => 8,
        .Float, .ComplexFloat => 4,
        .Double, .ComplexDouble => 8,
        .LongDouble, .ComplexLongDouble => 16,

        .Pointer,
        .StaticArray,
        .DecayedArray,
        .DecayedStaticArray,
        .DecayedIncompleteArray,
        .DecayedVariableLenArray,
        .DecayedUnspecifiedVariableLenArray,
        => comp.target.ptrBitWidth() >> 3,

        .Array => ty.data.array.elem.alignof(comp),
        .Struct, .Union => if (ty.data.record.isIncomplete()) 0 else ty.data.record.alignment,
        .Enum => if (ty.data.@"enum".isIncomplete()) 0 else ty.data.@"enum".tagType.alignof(comp),
    };
}

pub fn eql(a: Type, b: Type, checkQualifiers: bool) bool {
    if (a.alignment != b.alignment) return false;
    if (a.isPointer()) {
        if (!b.isPointer()) return false;
    } else if (a.isFunc()) {
        if (!b.isFunc()) return false;
    } else if (a.isArray()) {
        if (!b.isArray()) return false;
    } else if (a.specifier != b.specifier) return false;

    if (a.qual.atomic != b.qual.atomic) return false;

    if (checkQualifiers) {
        if (a.qual.@"const" != b.qual.@"const") return false;
        if (a.qual.@"volatile" != b.qual.@"volatile") return false;
    }

    switch (a.specifier) {
        .Pointer,
        .DecayedArray,
        .DecayedIncompleteArray,
        .DecayedVariableLenArray,
        .DecayedStaticArray,
        .DecayedUnspecifiedVariableLenArray,
        => if (!a.getElemType().eql(b.getElemType(), checkQualifiers)) return false,

        .Func,
        .VarArgsFunc,
        .OldStyleFunc,
        => {
            // TODO validate this
            if (a.data.func.params.len != b.data.func.params.len) return false;
            if (!a.data.func.returnType.eql(b.data.func.returnType, checkQualifiers)) return false;
            for (a.data.func.params, 0..) |param, i| {
                if (!param.ty.eql(b.data.func.params[i].ty, checkQualifiers)) return false;
            }
        },

        .Array,
        .StaticArray,
        .IncompleteArray,
        => {
            if (a.data.array.len != b.data.array.len) return false;
            if (!a.data.array.elem.eql(b.data.array.elem, checkQualifiers)) return false;
        },

        .VariableLenArray => if (!a.data.vla.elem.eql(b.data.vla.elem, checkQualifiers)) return false,

        .Union, .Struct => if (a.data.record != b.data.record) return false,
        .Enum => if (a.data.@"enum" != b.data.@"enum") return false,

        else => {},
    }

    return true;
}

pub fn decayArray(ty: *Type) void {
    ty.specifier = @as(Type.Specifier, @enumFromInt(@intFromEnum(ty.specifier) + 1));
}

pub fn combine(inner: *Type, outer: Type, p: *Parser, sourceToken: TokenIndex) Parser.Error!void {
    switch (inner.specifier) {
        .Pointer => return inner.data.subType.combine(outer, p, sourceToken),

        .UnspecifiedVariableLenArray => {
            try inner.data.subType.combine(outer, p, sourceToken);

            const elemType = inner.data.subType.*;
            if (elemType.hasIncompleteSize()) try p.errStr(.array_incomplete_elem, sourceToken, try p.typeStr(elemType));
            if (elemType.isFunc()) try p.errToken(.array_func_elem, sourceToken);
            if (elemType.qual.any() and elemType.isArray()) try p.errToken(.qualifier_non_outermost_array, sourceToken);
        },

        .Array, .StaticArray, .IncompleteArray => {
            try inner.data.array.elem.combine(outer, p, sourceToken);

            const elemType = inner.data.array.elem;
            if (elemType.hasIncompleteSize()) try p.errStr(.array_incomplete_elem, sourceToken, try p.typeStr(elemType));
            if (elemType.isFunc()) try p.errToken(.array_func_elem, sourceToken);
            if (elemType.specifier == .StaticArray and elemType.isArray()) try p.errToken(.static_non_outermost_array, sourceToken);
            if (elemType.qual.any() and elemType.isArray()) try p.errToken(.qualifier_non_outermost_array, sourceToken);
        },

        .VariableLenArray => {
            try inner.data.vla.elem.combine(outer, p, sourceToken);

            const elemType = inner.data.vla.elem;
            if (elemType.hasIncompleteSize()) try p.errStr(.array_incomplete_elem, sourceToken, try p.typeStr(elemType));
            if (elemType.isFunc()) try p.errToken(.array_func_elem, sourceToken);
            if (elemType.qual.any() and elemType.isArray()) try p.errToken(.qualifier_non_outermost_array, sourceToken);
        },

        .Func, .VarArgsFunc, .OldStyleFunc => {
            try inner.data.func.returnType.combine(outer, p, sourceToken);
            if (inner.data.func.returnType.isFunc()) try p.errToken(.func_cannot_return_func, sourceToken);
            if (inner.data.func.returnType.isArray()) try p.errToken(.func_cannot_return_array, sourceToken);
        },

        .DecayedArray,
        .DecayedStaticArray,
        .DecayedIncompleteArray,
        .DecayedVariableLenArray,
        .DecayedUnspecifiedVariableLenArray,
        => unreachable,

        else => inner.* = outer,
    }
}

/// Print type in C style
pub fn print(ty: Type, w: anytype) @TypeOf(w).Error!void {
    _ = try ty.printPrologue(w);
    try ty.printEpilogue(w);
}

/// return true if `ty` is simple
fn printPrologue(ty: Type, w: anytype) @TypeOf(w).Error!bool {
    if (ty.qual.atomic) {
        var nonAtomicType = ty;
        nonAtomicType.qual.atomic = false;
        try w.writeAll("_Atomic(");
        try nonAtomicType.print(w);
        try w.writeAll(")");
        return true;
    }

    switch (ty.specifier) {
        .Pointer,
        .DecayedArray,
        .DecayedStaticArray,
        .DecayedIncompleteArray,
        .DecayedVariableLenArray,
        .DecayedUnspecifiedVariableLenArray,
        => {
            const elemType = ty.getElemType();
            const simple = try elemType.printPrologue(w);
            if (simple) try w.writeByte(' ');
            if (elemType.isFunc() or elemType.isArray()) try w.writeByte('(');
            try w.writeByte('*');
            try ty.qual.dump(w);
            if (ty.alignment != 0) try w.print(" _Alignas({d})", .{ty.alignment});
            return false;
        },

        .Func, .VarArgsFunc, .OldStyleFunc => {
            const retType = ty.data.func.returnType;
            const simple = try retType.printPrologue(w);
            if (simple) try w.writeByte(' ');
            return false;
        },

        .Array,
        .StaticArray,
        .IncompleteArray,
        .UnspecifiedVariableLenArray,
        .VariableLenArray,
        => {
            const elemType = ty.getElemType();
            const simple = try elemType.printPrologue(w);
            if (simple) try w.writeByte(' ');
            return false;
        },

        else => {},
    }
    try ty.qual.dump(w);
    if (ty.alignment != 0)
        try w.print(" _Alignas({d})", .{ty.alignment});

    switch (ty.specifier) {
        .Enum => try w.print("enum {s}", .{ty.data.@"enum".name}),
        .Struct => try w.print("struct {s}", .{ty.data.record.name}),
        .Union => try w.print("union {s}", .{ty.data.record.name}),
        else => try w.writeAll(TypeBuilder.fromType(ty).toString().?),
    }

    return true;
}

fn printEpilogue(ty: Type, w: anytype) @TypeOf(w).Error!void {
    if (ty.qual.atomic) return;
    switch (ty.specifier) {
        .Pointer,
        .DecayedArray,
        .DecayedStaticArray,
        .DecayedIncompleteArray,
        .DecayedVariableLenArray,
        .DecayedUnspecifiedVariableLenArray,
        => {
            const elemType = ty.getElemType();
            if (elemType.isFunc() or elemType.isArray()) try w.writeByte(')');
            try elemType.printEpilogue(w);
        },

        .Func, .VarArgsFunc, .OldStyleFunc => {
            try w.writeByte('(');
            for (ty.data.func.params, 0..) |param, i| {
                if (i != 0) try w.writeAll(", ");
                _ = try param.ty.printPrologue(w);
                if (param.name.len != 0) try w.writeAll(param.name);
                try param.ty.printEpilogue(w);
            }
            if (ty.specifier != .Func) {
                if (ty.data.func.params.len != 0) try w.writeAll(", ");
                try w.writeAll("...");
            } else if (ty.data.func.params.len == 0) {
                try w.writeAll("void");
            }
            try w.writeByte(')');
            try ty.data.func.returnType.printEpilogue(w);
        },

        .Array, .StaticArray => {
            try w.writeByte('[');
            if (ty.specifier == .StaticArray) try w.writeAll("static ");
            try ty.qual.dump(w);
            if (ty.alignment != 0) try w.print(" _Alignas({d})", .{ty.alignment});
            try w.print("{d}]", .{ty.data.array.len});
            try ty.data.array.elem.printEpilogue(w);
        },

        .IncompleteArray => {
            try w.writeByte('[');
            try ty.qual.dump(w);
            if (ty.alignment != 0) try w.print(" _Alignas({d})", .{ty.alignment});
            try w.writeByte(']');
            try ty.data.array.elem.printEpilogue(w);
        },

        .UnspecifiedVariableLenArray => {
            try w.writeByte('[');
            try ty.qual.dump(w);
            if (ty.alignment != 0) try w.print(" _Alignas({d})", .{ty.alignment});
            try w.writeAll("*]");
            try ty.data.subType.printEpilogue(w);
        },

        .VariableLenArray => {
            try w.writeByte('[');
            try ty.qual.dump(w);
            if (ty.alignment != 0) try w.print(" _Alignas({d})", .{ty.alignment});
            try w.writeAll("<expr>]");
            try ty.data.vla.elem.printEpilogue(w);
        },

        else => {},
    }
}

/// Useful for debugging, too noisy to be enabled by default.
const DumpDetailedContainers = false;

pub fn dump(ty: Type, w: anytype) @TypeOf(w).Error!void {
    try ty.qual.dump(w);
    if (ty.alignment != 0)
        try w.print("_Alignas({d})", .{ty.alignment});

    switch (ty.specifier) {
        .Pointer => {
            try w.writeAll("*");
            try ty.data.subType.dump(w);
        },

        .Func, .VarArgsFunc, .OldStyleFunc => {
            try w.writeAll("fn (");
            for (ty.data.func.params, 0..) |param, i| {
                if (i != 0)
                    try w.writeAll(", ");
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

        .Array, .StaticArray, .DecayedArray, .DecayedStaticArray => {
            if (ty.specifier == .DecayedArray or ty.specifier == .DecayedStaticArray) try w.writeByte('d');
            try w.writeAll("[");
            if (ty.specifier == .StaticArray or ty.specifier == .DecayedStaticArray) try w.writeAll("static ");
            try w.print("{d}]", .{ty.data.array.len});
            try ty.data.array.elem.dump(w);
        },

        .IncompleteArray, .DecayedIncompleteArray => {
            if (ty.specifier == .DecayedIncompleteArray) try w.writeByte('d');
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

        .UnspecifiedVariableLenArray, .DecayedUnspecifiedVariableLenArray => {
            if (ty.specifier == .DecayedUnspecifiedVariableLenArray) try w.writeByte('d');
            try w.writeAll("[*]");
            try ty.data.subType.dump(w);
        },

        .VariableLenArray, .DecayedVariableLenArray => {
            if (ty.specifier == .DecayedVariableLenArray) try w.writeByte('d');
            try w.writeAll("[<expr>]");
            try ty.data.vla.elem.dump(w);
        },

        else => try w.writeAll(TypeBuilder.fromType(ty).toString().?),
    }
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
