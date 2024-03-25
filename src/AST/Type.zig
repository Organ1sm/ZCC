const std = @import("std");
const Parser = @import("../Parser/Parser.zig");
const Tree = @import("AST.zig");
const Compilation = @import("../Basic/Compilation.zig");
const TypeBuilder = @import("TypeBuilder.zig");
const Attribute = @import("../Lexer/Attribute.zig");

const Type = @This();

const NodeIndex = Tree.NodeIndex;
const TokenIndex = Tree.TokenIndex;

data: union {
    subType: *Type,
    func: *Function,
    array: *Array,
    expr: *Expr,
    @"enum": *Enum,
    record: *Record,
    attributed: *Attributed,
    none: void,
} = .{ .none = {} },

qual: Qualifiers = .{},
specifier: Specifier,

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

    /// checks if a has all the qualifiers of b
    pub fn hasQuals(a: Qualifiers, b: Qualifiers) bool {
        if (b.@"const" and !a.@"const") return false;
        if (b.@"volatile" and !a.@"volatile") return false;
        if (b.atomic and !a.atomic) return false;
        return true;
    }

    /// register is a storage class and not actually a qualifier
    /// so it is not preserved by typeof()
    pub fn inheritFromTypeof(quals: Qualifiers) Qualifiers {
        var res = quals;
        res.register = false;
        return res;
    }

    pub const Builder = struct {
        @"const": ?TokenIndex = null,
        atomic: ?TokenIndex = null,
        @"volatile": ?TokenIndex = null,
        restrict: ?TokenIndex = null,

        pub fn finish(b: Qualifiers.Builder, p: *Parser, ty: *Type) !void {
            if (!ty.is(.Pointer) and b.restrict != null) {
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

pub const Expr = struct {
    node: NodeIndex,
    ty: Type,
};

pub const Attributed = struct {
    attributes: []Attribute,
    base: Type, // base type

    fn create(allocator: std.mem.Allocator, base: Type, attributes: []const Attribute) !*Attributed {
        const attrType = try allocator.create(Attributed);
        errdefer allocator.destroy(attrType);

        const existingAttrs = base.getAttributes();
        var allAttrs = try allocator.alloc(Attribute, existingAttrs.len + attributes.len);
        @memcpy(allAttrs[0..existingAttrs.len], existingAttrs);
        @memcpy(allAttrs[existingAttrs.len..], attributes);

        attrType.* = .{
            .attributes = allAttrs,
            .base = base,
        };
        return attrType;
    }
};

pub const Enum = struct {
    name: []const u8,
    tagType: Type,
    fields: []Field,

    pub const Field = struct {
        name: []const u8,
        ty: Type,
        nameToken: TokenIndex,
        node: NodeIndex,
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
        /// zero for anonymous fields
        nameToken: TokenIndex = 0,
        bitWidth: u32 = 0,

        pub fn isAnonymousRecord(f: Field) bool {
            return f.nameToken == 0 and f.ty.isRecord();
        }
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
    // data.expr
    VariableLenArray,
    DecayedVariableLenArray,

    // data.record
    Struct,
    Union,

    // data.enum
    Enum,

    /// typeof(typename)
    TypeofType,
    /// decayed array created with typeof(typename)
    DecayedTypeofType,

    /// typeof(expression)
    TypeofExpr,
    /// decayed array created with typeof(expression)
    DecayedTypeofExpr,
    /// data.attributed
    Attributed,
    /// special type used to implement __builtin_va_start
    SpecialVaStart,
};

/// Determine if type matches the given specifier, recursing into typeof
/// types if necessary.
pub fn is(ty: Type, specifier: Specifier) bool {
    std.debug.assert(specifier != .TypeofExpr and specifier != .TypeofType);
    return ty.get(specifier) != null;
}

pub fn withAttributes(self: Type, allocator: std.mem.Allocator, attributes: []const Attribute) !Type {
    if (attributes.len == 0)
        return self;

    const attributedType = try Type.Attributed.create(allocator, self, attributes);
    return Type{ .specifier = .Attributed, .data = .{ .attributed = attributedType } };
}

pub fn isCallable(ty: Type) ?Type {
    return switch (ty.specifier) {
        .Func, .VarArgsFunc, .OldStyleFunc => ty,
        .Pointer => if (ty.data.subType.isFunc()) ty.data.subType.* else null,
        .TypeofType => ty.data.subType.isCallable(),
        .TypeofExpr => ty.data.expr.ty.isCallable(),
        .Attributed => ty.data.attributed.base.isCallable(),
        else => null,
    };
}

pub fn isFunc(ty: Type) bool {
    return switch (ty.specifier) {
        .Func, .VarArgsFunc, .OldStyleFunc => true,
        .TypeofType => ty.data.subType.isFunc(),
        .TypeofExpr => ty.data.expr.ty.isFunc(),
        .Attributed => ty.data.attributed.base.isFunc(),
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
        .DecayedTypeofType,
        .DecayedTypeofExpr,
        => true,

        .TypeofType => ty.data.subType.isPointer(),
        .TypeofExpr => ty.data.expr.ty.isPointer(),
        .Attributed => ty.data.attributed.base.isPointer(),

        else => false,
    };
}

pub fn isArray(ty: Type) bool {
    return switch (ty.specifier) {
        .Array, .StaticArray, .IncompleteArray, .VariableLenArray, .UnspecifiedVariableLenArray => true,
        .TypeofType => ty.data.subType.isArray(),
        .TypeofExpr => ty.data.expr.ty.isArray(),
        .Attributed => ty.data.attributed.base.isArray(),
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

        .TypeofType => ty.data.subType.isInt(),
        .TypeofExpr => ty.data.expr.ty.isInt(),
        .Attributed => ty.data.attributed.base.isInt(),

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

        .TypeofType => ty.data.subType.isFloat(),
        .TypeofExpr => ty.data.expr.ty.isFloat(),
        .Attributed => ty.data.attributed.base.isFloat(),

        else => false,
    };
}

pub fn isReal(ty: Type) bool {
    return switch (ty.specifier) {
        .ComplexFloat, .ComplexDouble, .ComplexLongDouble => false,

        .TypeofType => ty.data.subType.isReal(),
        .TypeofExpr => ty.data.expr.ty.isReal(),
        .Attributed => ty.data.attributed.base.isReal(),

        else => true,
    };
}

pub fn isTypeof(ty: Type) bool {
    return switch (ty.specifier) {
        .TypeofType, .TypeofExpr, .DecayedTypeofType, .DecayedTypeofExpr => true,
        else => false,
    };
}

pub fn isConst(ty: Type) bool {
    return switch (ty.specifier) {
        .TypeofType, .DecayedTypeofType => ty.qual.@"const" or ty.data.subType.isConst(),
        .TypeofExpr, .DecayedTypeofExpr => ty.qual.@"const" or ty.data.expr.ty.isConst(),
        .Attributed => ty.data.attributed.base.isConst(),
        else => ty.qual.@"const",
    };
}

// is the 'void *'
pub fn isVoidStar(ty: Type) bool {
    return switch (ty.specifier) {
        .Pointer => ty.data.subType.is(.Void),
        .TypeofType => ty.data.subType.isVoidStar(),
        .TypeofExpr => ty.data.expr.ty.isVoidStar(),
        .Attributed => ty.data.attributed.base.isVoidStar(),

        else => false,
    };
}

pub fn isUnsignedInt(ty: Type, comp: *const Compilation) bool {
    return switch (ty.specifier) {
        .Char => return getCharSignedness(comp) == .unsigned,
        .UChar, .UShort, .UInt, .ULong, .ULongLong, .Bool => return true,
        .TypeofType => ty.data.subType.isUnsignedInt(comp),
        .TypeofExpr => ty.data.expr.ty.isUnsignedInt(comp),
        .Attributed => ty.data.attributed.base.isUnsignedInt(comp),
        else => false,
    };
}

pub fn isEnumOrRecord(ty: Type) bool {
    return switch (ty.specifier) {
        .Enum, .Struct, .Union => true,
        .TypeofType => ty.data.subType.isEnumOrRecord(),
        .TypeofExpr => ty.data.expr.ty.isEnumOrRecord(),
        .Attributed => ty.data.attributed.base.isEnumOrRecord(),
        else => false,
    };
}

pub fn isRecord(ty: Type) bool {
    return switch (ty.specifier) {
        .Struct, .Union => true,
        .TypeofType => ty.data.subType.isRecord(),
        .TypeofExpr => ty.data.expr.ty.isRecord(),
        .Attributed => ty.data.attributed.base.isRecord(),
        else => false,
    };
}

pub fn isAnonymousRecord(ty: Type) bool {
    return switch (ty.specifier) {
        // anonymous records can be recognized by their names which are in
        // the format "(anonymous TAG at path:line:col)".
        .Struct, .Union => ty.data.record.name[0] == '(',
        .TypeofType => ty.data.subType.isAnonymousRecord(),
        .TypeofExpr => ty.data.expr.ty.isAnonymousRecord(),
        .Attributed => ty.data.attributed.base.isAnonymousRecord(),
        else => false,
    };
}

/// Canonicalizes the input type based on the specified qualifier handling option.
///
///* Canonicalize a possibly-typeof() type. If the type is not a typeof() type, simply
///* return it. Otherwise, determine the actual qualified type.
///* The `qualHhandling` parameter can be used to return the full set of qualifiers
///* added by typeof() operations, which is useful when determining the elemType of
///* arrays and pointers.
///
/// # Arguments
/// * `ty` - The input type to be canonicalized
/// * `qualHandling` - The qualifier handling option, which can be either `standard` or `preserve_quals`
///
/// # Returns
/// The canonicalized type after processing qualifiers and special cases related to `typeof` and `decay` operations.
pub fn canonicalize(ty: Type, qualHandling: enum { standard, preserve_quals }) Type {
    var cur = ty;
    if (cur.specifier == .Attributed)
        cur = cur.data.attributed.base;
    if (!cur.isTypeof()) return cur;

    var qual = cur.qual;
    while (true) {
        switch (cur.specifier) {
            .TypeofType => cur = cur.data.subType.*,
            .TypeofExpr => cur = cur.data.expr.ty,

            .DecayedTypeofType => {
                cur = cur.data.subType.*;
                cur.decayArray();
            },

            .DecayedTypeofExpr => {
                cur = cur.data.expr.ty;
                cur.decayArray();
            },

            else => break,
        }
        qual = qual.mergeAllQualifiers(cur.qual);
    }

    if ((cur.isArray() or cur.isPointer()) and qualHandling == .standard)
        cur.qual = .{}
    else
        cur.qual = qual;

    return cur;
}

pub fn get(ty: *const Type, specifier: Specifier) ?*const Type {
    std.debug.assert(specifier != .TypeofType and specifier != .TypeofExpr);
    return switch (ty.specifier) {
        .TypeofType => ty.data.subType.get(specifier),
        .TypeofExpr => ty.data.expr.ty.get(specifier),
        .Attributed => ty.data.attributed.base.get(specifier),
        else => if (ty.specifier == specifier) ty else null,
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

        .VariableLenArray,
        .DecayedVariableLenArray,
        => ty.data.expr.ty,

        .TypeofType,
        .DecayedTypeofType,
        .TypeofExpr,
        .DecayedTypeofExpr,
        .Attributed,
        => {
            const unwrapped = ty.canonicalize(.preserve_quals);
            var elem = unwrapped.getElemType();
            elem.qual = elem.qual.mergeAllQualifiers(unwrapped.qual);
            return elem;
        },

        else => unreachable,
    };
}

pub fn getReturnType(ty: Type) Type {
    return switch (ty.specifier) {
        .Func, .VarArgsFunc, .OldStyleFunc => ty.data.func.returnType,
        .TypeofType, .DecayedTypeofType => ty.data.subType.getReturnType(),
        .TypeofExpr, .DecayedTypeofExpr => ty.data.expr.ty.getReturnType(),
        .Attributed => ty.data.attributed.base.getReturnType(),
        else => unreachable,
    };
}

/// This function looks into the specified type and retrieves the parameters
/// based on the type's specifier.
/// @param ty The Type containing the specifier and relevant data.
/// @return A slice of Function.Param representing the function parameters.
///         If the type is not a function or typeof expression/typeof type,
///         the function is unreachable.
pub fn getParams(ty: Type) []Function.Param {
    return switch (ty.specifier) {
        .Func, .VarArgsFunc, .OldStyleFunc => ty.data.func.params,
        .TypeofType, .DecayedTypeofType => ty.data.subType.getParams(),
        .TypeofExpr, .DecayedTypeofExpr => ty.data.expr.ty.getParams(),
        .Attributed => ty.data.attributed.base.getParams(),
        else => unreachable,
    };
}

pub fn arrayLen(ty: Type) ?usize {
    return switch (ty.specifier) {
        .Array,
        .StaticArray,
        .DecayedArray,
        .DecayedStaticArray,
        => ty.data.array.len,

        .TypeofType, .DecayedTypeofType => ty.data.subType.arrayLen(),
        .TypeofExpr, .DecayedTypeofExpr => ty.data.expr.ty.arrayLen(),
        .Attributed => ty.data.attributed.base.arrayLen(),
        else => null,
    };
}

pub fn containAnyQual(ty: Type) bool {
    return switch (ty.specifier) {
        .TypeofType => ty.qual.any() or ty.data.subType.containAnyQual(),
        .TypeofExpr => ty.qual.any() or ty.data.expr.ty.containAnyQual(),
        else => ty.qual.any(),
    };
}

pub fn integerPromotion(ty: Type, comp: *Compilation) Type {
    var specifier = ty.specifier;
    if (specifier == .Enum) {
        // promote incomplete enums to int type
        if (ty.hasIncompleteSize())
            return .{ .specifier = .Int };
        specifier = ty.data.@"enum".tagType.specifier;
    }

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

            .TypeofType => return ty.data.subType.integerPromotion(comp),
            .TypeofExpr => return ty.data.expr.ty.integerPromotion(comp),
            .Attributed => return ty.data.attributed.base.integerPromotion(comp),

            else => unreachable, // not an integer type
        },
    };
}

pub fn hasIncompleteSize(ty: Type) bool {
    return switch (ty.specifier) {
        .Void,
        .IncompleteArray,
        => true,

        .Enum => ty.data.@"enum".isIncomplete(),
        .Struct, .Union => ty.data.record.isIncomplete(),

        .Array, .StaticArray => ty.data.array.elem.hasIncompleteSize(),

        .TypeofType => ty.data.subType.hasIncompleteSize(),
        .TypeofExpr => ty.data.expr.ty.hasIncompleteSize(),
        .Attributed => ty.data.attributed.base.hasIncompleteSize(),

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

            .TypeofType,
            .DecayedTypeofType,
            => cur = cur.data.subType.*,

            .TypeofExpr,
            .DecayedTypeofExpr,
            => cur = cur.data.expr.ty,

            .Attributed => cur = cur.data.attributed.base,

            else => return false,
        }
    }
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

/// Check if the given type `ty` has a field with the specified `name`
pub fn hasField(ty: Type, name: []const u8) bool {
    switch (ty.specifier) {
        .Struct => {
            std.debug.assert(!ty.data.record.isIncomplete());
            for (ty.data.record.fields) |f| {
                if (f.isAnonymousRecord() and f.ty.hasField(name)) return true;
                if (std.mem.eql(u8, name, f.name)) return true;
            }
        },
        .Union => {
            std.debug.assert(!ty.data.record.isIncomplete());
            for (ty.data.record.fields) |f| {
                if (f.isAnonymousRecord() and f.ty.hasField(name)) return true;
                if (std.mem.eql(u8, name, f.name)) return true;
            }
        },
        .TypeofType => return ty.data.subType.hasField(name),
        .TypeofExpr => return ty.data.expr.ty.hasField(name),
        .Attributed => return ty.data.attributed.base.hasField(name),
        else => unreachable,
    }
    return false;
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
        .DecayedTypeofType,
        .DecayedTypeofExpr,
        => comp.target.ptrBitWidth() >> 3,

        .Array => ty.data.array.elem.sizeof(comp).? * ty.data.array.len,
        .Struct, .Union => if (ty.data.record.isIncomplete()) null else ty.data.record.size,
        .Enum => if (ty.data.@"enum".isIncomplete()) null else ty.data.@"enum".tagType.sizeof(comp),

        .TypeofType => ty.data.subType.sizeof(comp),
        .TypeofExpr => ty.data.expr.ty.sizeof(comp),

        .Attributed => ty.data.attributed.base.sizeof(comp),
        else => unreachable,
    };
}

pub fn bitSizeof(ty: Type, comp: *Compilation) ?u64 {
    return switch (ty.specifier) {
        .Bool => 1,
        .TypeofType, .DecayedTypeofType => ty.data.subType.bitSizeof(comp),
        .TypeofExpr, .DecayedTypeofExpr => ty.data.expr.ty.bitSizeof(comp),
        .Attributed => ty.data.attributed.base.bitSizeof(comp),
        else => 8 * (ty.sizeof(comp) orelse return null),
    };
}

/// Get the alignment of a type
pub fn alignof(ty: Type, comp: *const Compilation) u29 {
    if (ty.requestedAlignment(comp)) |requested|
        return requested;

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

        .TypeofType, .DecayedTypeofType => ty.data.subType.alignof(comp),
        .TypeofExpr, .DecayedTypeofExpr => ty.data.expr.ty.alignof(comp),

        .Attributed => ty.data.attributed.base.alignof(comp),

        else => unreachable,
    };
}

fn requestedAlignment(ty: Type, comp: *const Compilation) ?u29 {
    return switch (ty.specifier) {
        .TypeofType, .DecayedTypeofType => ty.data.subType.requestedAlignment(comp),
        .TypeofExpr, .DecayedTypeofExpr => ty.data.expr.ty.requestedAlignment(comp),
        .Attributed => {
            var maxRequested: ?u29 = null;
            for (ty.data.attributed.attributes) |attribute| {
                if (attribute.tag != .aligned) continue;
                const requested: u29 = if (attribute.args.aligned.alignment) |alignment|
                    alignment.requested
                else
                    comp.defaultAlignment();

                if (maxRequested == null or maxRequested.? < requested) {
                    maxRequested = requested;
                }
            }
            return maxRequested;
        },
        else => null,
    };
}

pub fn eql(aParam: Type, bParam: Type, comp: *const Compilation, checkQualifiers: bool) bool {
    const a = aParam.canonicalize(.standard);
    const b = bParam.canonicalize(.standard);

    if (a.alignof(comp) != b.alignof(comp))
        return false;

    if (a.isPointer()) {
        if (!b.isPointer()) return false;
    } else if (a.isFunc()) {
        if (!b.isFunc()) return false;
    } else if (a.isArray()) {
        if (!b.isArray()) return false;
    } else if (a.specifier != b.specifier) return false;

    if (a.qual.atomic != b.qual.atomic)
        return false;

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
        => if (!aParam.getElemType().eql(bParam.getElemType(), comp, checkQualifiers))
            return false,

        .Func,
        .VarArgsFunc,
        .OldStyleFunc,
        => {
            // TODO validate this
            if (a.data.func.params.len != b.data.func.params.len) return false;

            // return type cannot have qualifiers
            if (!a.data.func.returnType.eql(b.data.func.returnType, comp, false))
                return false;

            for (a.data.func.params, 0..) |param, i| {
                var aUnqual = param.ty;
                aUnqual.qual.@"const" = false;
                aUnqual.qual.@"volatile" = false;

                var bUnqual = b.data.func.params[i].ty;
                bUnqual.qual.@"const" = false;
                bUnqual.qual.@"volatile" = false;

                if (!aUnqual.eql(bUnqual, comp, checkQualifiers))
                    return false;
            }
        },

        .Array,
        .StaticArray,
        .IncompleteArray,
        => {
            if (!std.meta.eql(a.arrayLen(), b.arrayLen()))
                return false;
            if (!a.getElemType().eql(b.getElemType(), comp, checkQualifiers))
                return false;
        },

        .VariableLenArray => if (!a.getElemType().eql(b.getElemType(), comp, checkQualifiers)) return false,

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

        .UnspecifiedVariableLenArray => try inner.data.subType.combine(outer, p, sourceToken),

        .Array,
        .StaticArray,
        .IncompleteArray,
        => try inner.data.array.elem.combine(outer, p, sourceToken),

        .VariableLenArray => try inner.data.expr.ty.combine(outer, p, sourceToken),

        .Func,
        .VarArgsFunc,
        .OldStyleFunc,
        => try inner.data.func.returnType.combine(outer, p, sourceToken),

        // type should not be able to decay before combine
        .DecayedArray,
        .DecayedStaticArray,
        .DecayedIncompleteArray,
        .DecayedVariableLenArray,
        .DecayedUnspecifiedVariableLenArray,
        .DecayedTypeofType,
        .DecayedTypeofExpr,
        => unreachable,

        else => inner.* = outer,
    }
}

pub fn validateCombinedType(ty: Type, p: *Parser, sourceToken: TokenIndex) Parser.Error!void {
    switch (ty.specifier) {
        .Pointer => return ty.data.subType.validateCombinedType(p, sourceToken),

        .UnspecifiedVariableLenArray,
        .VariableLenArray,
        .Array,
        .StaticArray,
        .IncompleteArray,
        => {
            const elemType = ty.getElemType();
            if (elemType.hasIncompleteSize()) {
                try p.errStr(.array_incomplete_elem, sourceToken, try p.typeStr(elemType));
                return error.ParsingFailed;
            }

            if (elemType.isFunc()) {
                try p.errToken(.array_func_elem, sourceToken);
                return error.ParsingFailed;
            }

            if (elemType.specifier == .StaticArray and elemType.isArray())
                try p.errToken(.static_non_outermost_array, sourceToken);

            if (elemType.containAnyQual() and elemType.isArray())
                try p.errToken(.qualifier_non_outermost_array, sourceToken);
        },

        .Func, .VarArgsFunc, .OldStyleFunc => {
            const returnType = &ty.data.func.returnType;
            if (returnType.isArray())
                try p.errToken(.func_cannot_return_array, sourceToken);

            if (returnType.isFunc())
                try p.errToken(.func_cannot_return_func, sourceToken);

            if (returnType.qual.@"const") {
                try p.errStr(.qual_on_ret_type, sourceToken, "const");
                returnType.qual.@"const" = false;
            }

            if (returnType.qual.@"volatile") {
                try p.errStr(.qual_on_ret_type, sourceToken, "volatile");
                returnType.qual.@"volatile" = false;
            }

            if (returnType.qual.atomic) {
                try p.errStr(.qual_on_ret_type, sourceToken, "atomic");
                returnType.qual.atomic = false;
            }
        },

        .TypeofType, .DecayedTypeofType => return ty.data.subType.validateCombinedType(p, sourceToken),
        .TypeofExpr, .DecayedTypeofExpr => return ty.data.expr.ty.validateCombinedType(p, sourceToken),
        .Attributed => return ty.data.attributed.base.validateCombinedType(p, sourceToken),
        else => {},
    }
}

/// Return the attribute with the specified tag from the given type.
/// If the type is a TypeofType or TypeofExpr, the search is performed on
/// the subtype or the type of the expression, respectively.
///
/// If the type is Attributed, it iterates over its attributes to find a match.
/// Returns null if the attribute is not present or the type does not support attributes.
///
/// @param ty The Type from which to retrieve the attribute.
/// @param tag The attribute tag to search for.
/// @return The attribute with the given tag if it exists, or null otherwise.
pub fn getAttribute(ty: Type, comptime tag: Attribute.Tag) ?Attribute.ArgumentsForTag(tag) {
    switch (ty.specifier) {
        .TypeofType => return ty.data.subType.getAttribute(tag),
        .TypeofExpr => return ty.data.expr.ty.getAttribute(tag),

        .Attributed => {
            for (ty.data.attributed.attributes) |attr| {
                if (attr.tag == tag)
                    return @field(attr.args, @tagName(tag));
            }
            return null;
        },

        else => return null,
    }
}

pub fn getAttributes(ty: Type) []const Attribute {
    return switch (ty.specifier) {
        .Attributed => ty.data.attributed.attributes,
        .TypeofType, .DecayedTypeofType => ty.data.subType.getAttributes(),
        .TypeofExpr, .DecayedTypeofExpr => ty.data.expr.ty.getAttributes(),
        else => &.{},
    };
}

/// Print type in C style
pub fn print(ty: Type, w: anytype) @TypeOf(w).Error!void {
    _ = try ty.printPrologue(w);
    try ty.printEpilogue(w);
}

pub fn printNamed(ty: Type, name: []const u8, w: anytype) @TypeOf(w).Error!void {
    const simple = try ty.printPrologue(w);
    if (simple) try w.writeByte(' ');
    try w.writeAll(name);
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
        .DecayedTypeofType,
        .DecayedTypeofExpr,
        => {
            const elemType = ty.getElemType();
            const simple = try elemType.printPrologue(w);
            if (simple) try w.writeByte(' ');
            if (elemType.isFunc() or elemType.isArray()) try w.writeByte('(');
            try w.writeByte('*');
            try ty.qual.dump(w);
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

        .TypeofType,
        .TypeofExpr,
        => {
            const actual = ty.canonicalize(.standard);
            return actual.printPrologue(w);
        },

        .Attributed => {
            const actual = ty.canonicalize(.standard);
            return actual.printPrologue(w);
        },

        else => {},
    }
    try ty.qual.dump(w);

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
        .DecayedTypeofType,
        .DecayedTypeofExpr,
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
                try param.ty.printEpilogue(w);
            }
            if (!ty.is(.Func)) {
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
            if (ty.is(.StaticArray)) try w.writeAll("static ");
            try ty.qual.dump(w);
            try w.print("{d}]", .{ty.data.array.len});
            try ty.data.array.elem.printEpilogue(w);
        },

        .IncompleteArray => {
            try w.writeByte('[');
            try ty.qual.dump(w);
            try w.writeByte(']');
            try ty.data.array.elem.printEpilogue(w);
        },

        .UnspecifiedVariableLenArray => {
            try w.writeByte('[');
            try ty.qual.dump(w);
            try w.writeAll("*]");
            try ty.data.subType.printEpilogue(w);
        },

        .VariableLenArray => {
            try w.writeByte('[');
            try ty.qual.dump(w);
            try w.writeAll("<expr>]");
            try ty.data.expr.ty.printEpilogue(w);
        },

        else => {},
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
                if (i != 0)
                    try w.writeAll(", ");
                if (param.name.len != 0)
                    try w.print("{s}: ", .{param.name});
                try param.ty.dump(w);
            }

            if (!ty.isFunc()) {
                if (ty.data.func.params.len != 0) try w.writeAll(", ");
                try w.writeAll("...");
            }

            try w.writeAll(") ");
            try ty.data.func.returnType.dump(w);
        },

        .Array, .StaticArray, .DecayedArray, .DecayedStaticArray => {
            if (ty.is(.DecayedArray) or ty.is(.DecayedStaticArray)) try w.writeByte('d');
            try w.writeAll("[");
            if (ty.is(.StaticArray) or ty.is(.DecayedStaticArray)) try w.writeAll("static ");
            try w.print("{d}]", .{ty.data.array.len});
            try ty.data.array.elem.dump(w);
        },

        .IncompleteArray, .DecayedIncompleteArray => {
            if (ty.is(.DecayedIncompleteArray)) try w.writeByte('d');
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
            if (ty.is(.DecayedUnspecifiedVariableLenArray)) try w.writeByte('d');
            try w.writeAll("[*]");
            try ty.data.subType.dump(w);
        },

        .VariableLenArray, .DecayedVariableLenArray => {
            if (ty.is(.DecayedVariableLenArray)) try w.writeByte('d');
            try w.writeAll("[<expr>]");
            try ty.data.expr.ty.dump(w);
        },

        .TypeofType, .DecayedTypeofType => {
            try w.writeAll("typeof(");
            try ty.data.subType.dump(w);
            try w.writeAll(")");
        },

        .TypeofExpr, .DecayedTypeofExpr => {
            try w.writeAll("typeof(<expr>: ");
            try ty.data.expr.ty.dump(w);
            try w.writeAll(")");
        },

        .Attributed => {
            try w.writeAll("attributed(");
            try ty.data.attributed.base.dump(w);
            try w.writeAll(")");
        },

        .SpecialVaStart => try w.writeAll("(var start param)"),
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
