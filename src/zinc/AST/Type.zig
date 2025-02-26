const std = @import("std");
const assert = std.debug.assert;
const Parser = @import("../Parser/Parser.zig");
const Tree = @import("AST.zig");
const Compilation = @import("../Basic/Compilation.zig");
const TypeBuilder = @import("TypeBuilder.zig");
const Attribute = @import("../Lexer/Attribute.zig");
const StringInterner = @import("../Basic/StringInterner.zig");
const StringId = StringInterner.StringId;
const Target = @import("../Basic/Target.zig");
const LangOpts = @import("../Basic/LangOpts.zig");

const Type = @This();

const Node = Tree.Node;
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
    int: struct {
        bits: u16,
        signedness: std.builtin.Signedness,
    },
} = .{ .none = {} },

qual: Qualifiers = .{},
specifier: Specifier,
decayed: bool = false,
/// typedef name, if any
name: StringId = .empty,

pub const Invalid = create(.Invalid);
pub const Void = create(.Void);
pub const Pointer = create(.Pointer);
pub const NullptrTy = create(.NullPtrTy);

pub const Bool = create(.Bool);
pub const Char = create(.Char);
pub const UChar = create(.UChar);
pub const SChar = create(.SChar);
pub const Short = create(.Short);
pub const UShort = create(.UShort);
pub const Int = create(.Int);
pub const UInt = create(.UInt);
pub const ULong = create(.ULong);
pub const Long = create(.Long);
pub const LongLong = create(.LongLong);
pub const ULongLong = create(.ULongLong);
pub const Int128 = create(.Int128);
pub const UInt128 = create(.UInt128);

pub const Float = create(.Float);
pub const Double = create(.Double);
pub const LongDouble = create(.LongDouble);
pub const ComplexFloat = create(.ComplexFloat);
pub const ComplexDouble = create(.ComplexDouble);
pub const ComplexLongDouble = create(.ComplexLongDouble);

pub inline fn create(specifier: Specifier) Type {
    return .{ .specifier = specifier };
}

pub const Qualifiers = packed struct {
    @"const": bool = false,
    atomic: bool = false,
    @"volatile": bool = false,
    restrict: bool = false,

    pub fn any(quals: Qualifiers) bool {
        return quals.@"const" or quals.restrict or quals.@"volatile" or quals.atomic;
    }

    pub fn dump(quals: Qualifiers, w: anytype) !void {
        if (quals.@"const") try w.writeAll("const ");
        if (quals.atomic) try w.writeAll("_Atomic ");
        if (quals.@"volatile") try w.writeAll("volatile ");
        if (quals.restrict) try w.writeAll("restrict ");
    }

    /// Merge the const/volatile/ qualifiers, used by type resolution
    /// of the conditional operator
    pub fn mergeCVQualifiers(a: Qualifiers, b: Qualifiers) Qualifiers {
        return .{
            .@"const" = a.@"const" or b.@"const",
            .@"volatile" = a.@"volatile" or b.@"volatile",
        };
    }

    /// remove cv qualifiers
    pub fn removeCVQualifiers(qual: *Qualifiers) void {
        qual.@"const" = false;
        qual.@"volatile" = false;
    }

    /// Merge all qualifiers, used by typeof()
    pub fn mergeAllQualifiers(a: Qualifiers, b: Qualifiers) Qualifiers {
        return .{
            .@"const" = a.@"const" or b.@"const",
            .atomic = a.atomic or b.atomic,
            .@"volatile" = a.@"volatile" or b.@"volatile",
            .restrict = a.restrict or b.restrict,
        };
    }

    /// checks if a has all the qualifiers of b
    pub fn hasQuals(a: Qualifiers, b: Qualifiers) bool {
        if (b.@"const" and !a.@"const") return false;
        if (b.@"volatile" and !a.@"volatile") return false;
        if (b.atomic and !a.atomic) return false;
        return true;
    }

    pub const Builder = struct {
        @"const": ?TokenIndex = null,
        atomic: ?TokenIndex = null,
        @"volatile": ?TokenIndex = null,
        restrict: ?TokenIndex = null,

        pub fn finish(b: Qualifiers.Builder, p: *Parser, ty: *Type) !void {
            if (!ty.is(.Pointer) and b.restrict != null)
                try p.errStr(.restrict_non_pointer, b.restrict.?, try p.typeStr(ty.*));

            // validate atomic
            if (b.atomic) |some| {
                if (ty.isArray()) try p.errStr(.atomic_array, some, try p.typeStr(ty.*));
                if (ty.isFunc()) try p.errStr(.atomic_func, some, try p.typeStr(ty.*));
                if (ty.hasIncompleteSize()) try p.errStr(.atomic_incomplete, some, try p.typeStr(ty.*));
            }

            if (b.@"const" != null) ty.qual.@"const" = true;
            if (b.atomic != null) ty.qual.atomic = true;
            if (b.@"volatile" != null) ty.qual.@"volatile" = true;
            if (b.restrict != null) ty.qual.restrict = true;
        }
    };
};

pub const Function = struct {
    returnType: Type,
    params: []Param,

    pub const Param = struct {
        ty: Type,
        name: StringId,
        nameToken: TokenIndex,
        node: Node.OptIndex,
    };

    fn eql(
        lhs: *const Function,
        rhs: *const Function,
        lhsSpec: Specifier,
        rhsSpec: Specifier,
        comp: *const Compilation,
    ) bool {
        if (!lhs.returnType.eql(rhs.returnType, comp, false))
            return false;

        if (lhs.params.len != rhs.params.len) {
            if (lhsSpec == .OldStyleFunc or rhsSpec == .OldStyleFunc) {
                const maybeHasParams = if (lhsSpec == .OldStyleFunc) rhs else lhs;
                for (maybeHasParams.params) |param| {
                    if (param.ty.undergoesDefaultArgPromotion(comp))
                        return false;
                }
                return true;
            }
            return false;
        }

        // Check if both types are functions or neither is a function
        if ((lhsSpec == .Func) != (rhsSpec == .Func))
            return false;

        for (lhs.params, rhs.params) |lhsParam, rhsParam| {
            var lhsParamUnqual = lhsParam.ty;
            lhsParamUnqual.qual.removeCVQualifiers();

            var rhsParamUnqual = rhsParam.ty;
            rhsParamUnqual.qual.removeCVQualifiers();

            if (!lhsParamUnqual.eql(rhsParamUnqual, comp, true))
                return false;
        }

        return true;
    }
};

pub const Array = struct {
    len: u64,
    elem: Type,
};

pub const Expr = struct {
    node: Node.Index,
    ty: Type,
};

pub const Attributed = struct {
    attributes: []Attribute,
    base: Type, // base type

    pub fn create(
        allocator: std.mem.Allocator,
        base: Type,
        existingAttrs: []const Attribute,
        attributes: []const Attribute,
    ) !*Attributed {
        const attrType = try allocator.create(Attributed);
        errdefer allocator.destroy(attrType);

        var allAttrs = try allocator.alloc(Attribute, existingAttrs.len + attributes.len);
        @memcpy(allAttrs[0..existingAttrs.len], existingAttrs);
        @memcpy(allAttrs[existingAttrs.len..], attributes);

        attrType.* = .{ .attributes = allAttrs, .base = base };
        return attrType;
    }
};

pub const Enum = struct {
    fields: []Field,
    tagType: Type,
    name: StringId,
    fixed: bool,

    pub const Field = struct {
        ty: Type,
        name: StringId,
        nameToken: TokenIndex,
        init: Node.OptIndex,
    };

    pub fn isIncomplete(e: Enum) bool {
        return e.fields.len == std.math.maxInt(usize);
    }

    pub fn create(allocator: std.mem.Allocator, name: StringId, fixedTy: ?Type) !*Enum {
        var e = try allocator.create(Enum);
        e.name = name;
        e.fields.len = std.math.maxInt(usize);
        if (fixedTy) |some| e.tagType = some;
        e.fixed = (fixedTy != null);
        return e;
    }
};

pub const TypeLayout = struct {
    /// The size of the type in bits.
    ///
    /// This is the value returned by `sizeof` in C
    /// (but in bits instead of bytes). This is a multiple of `pointer_alignment_bits`.
    sizeBits: u64,
    /// The alignment of the type, in bits, when used as a field in a record.
    ///
    /// This is usually the value returned by `_Alignof` in C, but there are some edge
    /// cases in GCC where `_Alignof` returns a smaller value.
    fieldAlignmentBits: u32,
    /// The alignment, in bits, of valid pointers to this type.
    /// `size_bits` is a multiple of this value.
    pointerAlignmentBits: u32,
    /// The required alignment of the type in bits.
    ///
    /// This value is only used by MSVC targets. It is 8 on all other
    /// targets. On MSVC targets, this value restricts the effects of `#pragma pack` except
    /// in some cases involving bit-fields.
    requiredAlignmentBits: u32,
};

pub const FieldLayout = struct {
    /// `offset_bits` and `size_bits` should both be INVALID if and only if the field
    /// is an unnamed bitfield. There is no way to reference an unnamed bitfield in C, so
    /// there should be no way to observe these values. If it is used, this value will
    /// maximize the chance that a safety-checked overflow will occur.
    const INVALID = std.math.maxInt(u64);
    /// The offset of the struct, in bits, from the start of the struct.
    offsetBits: u64 = INVALID,
    /// The size, in bits, of the field.
    /// For bit-fields, this is the width of the field.
    sizeBits: u64 = INVALID,

    pub fn isUnnamed(self: FieldLayout) bool {
        return self.offsetBits == INVALID and self.sizeBits == INVALID;
    }
};

pub const Record = struct {
    fields: []Field,
    typeLayout: TypeLayout,
    /// If this is null, none of the fields have attributes
    /// Otherwise, it's a pointer to N items (where N == number of fields)
    /// and the item at index i is the attributes for the field at index i
    fieldAttributes: ?[*][]const Attribute,
    name: StringId,

    pub const Field = struct {
        ty: Type,
        name: StringId,
        /// zero for anonymous fields
        nameToken: TokenIndex = 0,
        bitWidth: ?u32 = null,
        layout: FieldLayout = .{ .offsetBits = 0, .sizeBits = 0 },

        pub fn isNamed(f: *const Field) bool {
            return f.nameToken != 0;
        }

        pub fn isAnonymousRecord(f: Field) bool {
            return !f.isNamed() and f.ty.isRecord();
        }

        /// false for bitfields
        pub fn isRegularField(f: *const Field) bool {
            return f.bitWidth == null;
        }

        /// bit width as specified in the C source. Asserts that `f` is a bitfield.
        pub fn specifiedBitWidth(f: *const Field) u32 {
            return f.bitWidth.?;
        }
    };

    pub fn isIncomplete(r: Record) bool {
        return r.fields.len == std.math.maxInt(usize);
    }

    pub fn hasFieldOfType(self: *const Record, ty: Type, comp: *const Compilation) bool {
        if (self.isIncomplete()) return false;

        for (self.fields) |f| {
            if (ty.eql(f.ty, comp, false)) return true;
        }

        return false;
    }

    pub fn hasField(self: *const Record, name: StringId) bool {
        std.debug.assert(!self.isIncomplete());
        for (self.fields) |f| {
            if (f.isAnonymousRecord() and f.ty.getRecord().?.hasField(name)) return true;
            if (name == f.name) return true;
        }
        return false;
    }

    pub fn create(allocator: std.mem.Allocator, name: StringId) !*Record {
        var r = try allocator.create(Record);
        r.name = name;
        r.fields.len = std.math.maxInt(usize);
        r.fieldAttributes = null;
        r.typeLayout = .{
            .sizeBits = 8,
            .fieldAlignmentBits = 8,
            .pointerAlignmentBits = 8,
            .requiredAlignmentBits = 8,
        };
        return r;
    }
};

pub const Specifier = enum {
    /// A NaN-like poison value
    Invalid,

    /// GNU auto type
    /// This is a placeholder specifier - it must be replaced by the actual type specifier (determined by the initializer)
    AutoType,
    /// c23 auto
    C23Auto,

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
    Int128,
    UInt128,
    ComplexChar,
    ComplexSChar,
    ComplexUChar,
    ComplexShort,
    ComplexUShort,
    ComplexInt,
    ComplexUInt,
    ComplexLong,
    ComplexULong,
    ComplexLongLong,
    ComplexULongLong,
    ComplexInt128,
    ComplexUInt128,

    // data.int
    BitInt,
    ComplexBitInt,

    // floating point numbers
    FP16,
    Float,
    Double,
    LongDouble,
    Float80,
    Float128,
    ComplexFP16,
    ComplexFloat,
    ComplexDouble,
    ComplexLongDouble,
    ComplexFloat80,
    ComplexFloat128,

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
    Vector,

    // data.expr
    VariableLenArray,

    // data.record
    Struct,
    Union,
    // data.enum
    Enum,

    /// typeof(typename)
    TypeofType,

    /// typeof(expression)
    TypeofExpr,

    /// data.attributed
    Attributed,

    /// special type used to implement __builtin_va_start
    SpecialVaStart,

    /// C23 nullptr_t
    NullPtrTy,
};

/// Whether the type is promoted if used as a variadic argument or
/// as an argument to a function with no prototype
fn undergoesDefaultArgPromotion(ty: Type, comp: *const Compilation) bool {
    return switch (ty.specifier) {
        .Bool => true,
        .Char, .UChar, .SChar => true,
        .Short, .UShort => true,
        .Enum => if (comp.langOpts.emulate == .clang) ty.data.@"enum".isIncomplete() else false,
        .Float => true,

        .TypeofType => ty.data.subType.undergoesDefaultArgPromotion(comp),
        .TypeofExpr => ty.data.expr.ty.undergoesDefaultArgPromotion(comp),
        .Attributed => ty.data.attributed.base.undergoesDefaultArgPromotion(comp),
        else => false,
    };
}

/// Determine if type matches the given specifier, recursing into typeof
/// types if necessary.
pub fn is(ty: Type, specifier: Specifier) bool {
    assert(specifier != .TypeofExpr and specifier != .TypeofType);
    return ty.get(specifier) != null;
}

/// create a new type by applying the specified attributes to the given type.
pub fn withAttributes(self: Type, allocator: std.mem.Allocator, attributes: []const Attribute) !Type {
    if (attributes.len == 0)
        return self;

    const attributedType = try Type.Attributed.create(allocator, self, self.getAttributes(), attributes);
    return Type{
        .specifier = .Attributed,
        .data = .{ .attributed = attributedType },
        .decayed = self.decayed,
    };
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
        .Pointer => true,

        .Array,
        .StaticArray,
        .IncompleteArray,
        .VariableLenArray,
        .UnspecifiedVariableLenArray,
        => ty.isDecayed(),

        .TypeofType => ty.isDecayed() or ty.data.subType.isPointer(),
        .TypeofExpr => ty.isDecayed() or ty.data.expr.ty.isPointer(),
        .Attributed => ty.isDecayed() or ty.data.attributed.base.isPointer(),

        else => false,
    };
}

pub fn isArray(ty: Type) bool {
    return switch (ty.specifier) {
        .Array, .StaticArray, .IncompleteArray, .VariableLenArray, .UnspecifiedVariableLenArray => !ty.isDecayed(),
        .TypeofType => !ty.isDecayed() and ty.data.subType.isArray(),
        .TypeofExpr => !ty.isDecayed() and ty.data.expr.ty.isArray(),
        .Attributed => !ty.isDecayed() and ty.data.attributed.base.isArray(),
        else => false,
    };
}

pub fn isVector(ty: Type) bool {
    return ty.specifier == .Vector;
}

pub fn isScalar(ty: Type) bool {
    return ty.isInt() or isScalarNonInt(ty);
}

/// To avoid calling isInt() twice for allowable loop/if controlling expressions
pub fn isScalarNonInt(ty: Type) bool {
    return ty.isFloat() or ty.isPointer() or ty.is(.NullPtrTy);
}

pub fn isDecayed(ty: Type) bool {
    return ty.decayed;
}

pub fn isInvalid(ty: Type) bool {
    return ty.specifier == .Invalid;
}

pub fn isChar(ty: Type) bool {
    return switch (ty.specifier) {
        .Char, .SChar, .UChar => true,
        .TypeofType => ty.data.subType.isChar(),
        .TypeofExpr => ty.data.expr.ty.isChar(),
        .Attributed => ty.data.attributed.base.isChar(),
        else => false,
    };
}

pub fn isArithmetic(ty: Type) bool {
    return ty.isInt() or ty.isFloat();
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
        .Int128,
        .UInt128,
        .ComplexChar,
        .ComplexSChar,
        .ComplexUChar,
        .ComplexShort,
        .ComplexUShort,
        .ComplexInt,
        .ComplexUInt,
        .ComplexLong,
        .ComplexULong,
        .ComplexLongLong,
        .ComplexULongLong,
        .ComplexInt128,
        .ComplexUInt128,
        .BitInt,
        .ComplexBitInt,
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
        .FP16,
        .Float80,
        .Float128,
        .ComplexFP16,
        .ComplexFloat80,
        .ComplexFloat128,
        => true,

        .TypeofType => ty.data.subType.isFloat(),
        .TypeofExpr => ty.data.expr.ty.isFloat(),
        .Attributed => ty.data.attributed.base.isFloat(),

        else => false,
    };
}

pub fn isReal(ty: Type) bool {
    return switch (ty.specifier) {
        .ComplexFloat,
        .ComplexDouble,
        .ComplexLongDouble,
        .ComplexFP16,
        .ComplexFloat80,
        .ComplexFloat128,
        .ComplexChar,
        .ComplexSChar,
        .ComplexUChar,
        .ComplexShort,
        .ComplexUShort,
        .ComplexInt,
        .ComplexUInt,
        .ComplexLong,
        .ComplexULong,
        .ComplexLongLong,
        .ComplexULongLong,
        .ComplexInt128,
        .ComplexUInt128,
        .ComplexBitInt,
        => false,

        .TypeofType => ty.data.subType.isReal(),
        .TypeofExpr => ty.data.expr.ty.isReal(),
        .Attributed => ty.data.attributed.base.isReal(),

        else => true,
    };
}

pub fn isComplex(ty: Type) bool {
    return switch (ty.specifier) {
        .ComplexFloat,
        .ComplexDouble,
        .ComplexLongDouble,
        .ComplexFP16,
        .ComplexFloat80,
        .ComplexFloat128,
        .ComplexChar,
        .ComplexSChar,
        .ComplexUChar,
        .ComplexShort,
        .ComplexUShort,
        .ComplexInt,
        .ComplexUInt,
        .ComplexLong,
        .ComplexULong,
        .ComplexLongLong,
        .ComplexULongLong,
        .ComplexInt128,
        .ComplexUInt128,
        .ComplexBitInt,
        => true,

        .TypeofType => ty.data.subType.isComplex(),
        .TypeofExpr => ty.data.expr.ty.isComplex(),
        .Attributed => ty.data.attributed.base.isComplex(),

        else => false,
    };
}

pub fn isTypeof(ty: Type) bool {
    return switch (ty.specifier) {
        .TypeofType, .TypeofExpr => true,
        else => false,
    };
}

pub fn isConst(ty: Type) bool {
    return switch (ty.specifier) {
        .TypeofType => ty.qual.@"const" or ty.data.subType.isConst(),
        .TypeofExpr => ty.qual.@"const" or ty.data.expr.ty.isConst(),
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
    return ty.signedness(comp) == .unsigned;
}

pub fn signedness(ty: Type, comp: *const Compilation) std.builtin.Signedness {
    return switch (ty.specifier) {
        .Char, .ComplexChar => return comp.getCharSignedness(),

        .UChar,
        .UShort,
        .UInt,
        .ULong,
        .ULongLong,
        .Bool,
        .ComplexUChar,
        .ComplexUShort,
        .ComplexUInt,
        .ComplexULong,
        .ComplexULongLong,
        .ComplexUInt128,
        => .unsigned,

        .BitInt, .ComplexBitInt => return ty.data.int.signedness,

        .TypeofType => ty.data.subType.signedness(comp),
        .TypeofExpr => ty.data.expr.ty.signedness(comp),
        .Attributed => ty.data.attributed.base.signedness(comp),

        else => .signed,
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

pub fn isAnonymousRecord(ty: Type, comp: *const Compilation) bool {
    return switch (ty.specifier) {
        // anonymous records can be recognized by their names which are in
        // the format "(anonymous TAG at path:line:col)".
        .Struct, .Union => {
            const mapper = comp.stringInterner.getSlowTypeMapper();
            return mapper.lookup(ty.data.record.name)[0] == '(';
        },
        .TypeofType => ty.data.subType.isAnonymousRecord(comp),
        .TypeofExpr => ty.data.expr.ty.isAnonymousRecord(comp),
        .Attributed => ty.data.attributed.base.isAnonymousRecord(comp),
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
    if (cur.specifier == .Attributed) {
        cur = cur.data.attributed.base;
        cur.decayed = ty.decayed;
    }
    if (!cur.isTypeof())
        return cur;

    var qual = cur.qual;
    while (true) {
        switch (cur.specifier) {
            .TypeofType => cur = cur.data.subType.*,
            .TypeofExpr => cur = cur.data.expr.ty,
            else => break,
        }
        qual = qual.mergeAllQualifiers(cur.qual);
    }

    if ((cur.isArray() or cur.isPointer()) and qualHandling == .standard)
        cur.qual = .{}
    else
        cur.qual = qual;

    cur.decayed = ty.decayed;
    return cur;
}

pub fn get(ty: *const Type, specifier: Specifier) ?*const Type {
    assert(specifier != .TypeofType and specifier != .TypeofExpr);
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
        => ty.data.subType.*,

        .Array,
        .StaticArray,
        .IncompleteArray,
        .Vector,
        => ty.data.array.elem,

        .VariableLenArray => ty.data.expr.ty,

        .TypeofType, .TypeofExpr => {
            const unwrapped = ty.canonicalize(.preserve_quals);
            var elem = unwrapped.getElemType();
            elem.qual = elem.qual.mergeAllQualifiers(unwrapped.qual);
            return elem;
        },

        .Attributed => ty.data.attributed.base.getElemType(),

        .ComplexFloat,
        .ComplexDouble,
        .ComplexLongDouble,
        .ComplexFP16,
        .ComplexFloat80,
        .ComplexFloat128,
        .ComplexChar,
        .ComplexSChar,
        .ComplexUChar,
        .ComplexShort,
        .ComplexUShort,
        .ComplexInt,
        .ComplexUInt,
        .ComplexLong,
        .ComplexULong,
        .ComplexLongLong,
        .ComplexULongLong,
        .ComplexInt128,
        .ComplexUInt128,
        .ComplexBitInt,
        => ty.makeReal(),

        .Invalid => Type.Invalid,
        else => unreachable,
    };
}

pub fn getReturnType(ty: Type) Type {
    return switch (ty.specifier) {
        .Func, .VarArgsFunc, .OldStyleFunc => ty.data.func.returnType,
        .TypeofType => ty.data.subType.getReturnType(),
        .TypeofExpr => ty.data.expr.ty.getReturnType(),
        .Attributed => ty.data.attributed.base.getReturnType(),
        .Invalid => Type.Invalid,
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
        .TypeofType => ty.data.subType.getParams(),
        .TypeofExpr => ty.data.expr.ty.getParams(),
        .Attributed => ty.data.attributed.base.getParams(),
        .Invalid => &.{},
        else => unreachable,
    };
}

pub fn arrayLen(ty: Type) ?u64 {
    return switch (ty.specifier) {
        .Array, .StaticArray => ty.data.array.len,
        .TypeofType => ty.data.subType.arrayLen(),
        .TypeofExpr => ty.data.expr.ty.arrayLen(),
        .Attributed => ty.data.attributed.base.arrayLen(),
        else => null,
    };
}

/// Complex numbers are scalars but they can be initialized with a 2-element initList
pub fn expectedInitListSize(ty: Type) ?u64 {
    return if (ty.isComplex()) 2 else ty.arrayLen();
}

pub fn containAnyQual(ty: Type) bool {
    return switch (ty.specifier) {
        .TypeofType => ty.qual.any() or ty.data.subType.containAnyQual(),
        .TypeofExpr => ty.qual.any() or ty.data.expr.ty.containAnyQual(),
        else => ty.qual.any(),
    };
}

/// Promote an integer type to the smallest type that can represent it without loss.
pub fn integerPromotion(ty: Type, comp: *Compilation) Type {
    var specifier = ty.specifier;
    switch (specifier) {
        .Enum => {
            if (ty.hasIncompleteSize())
                return Type.Int;
            specifier = ty.data.@"enum".tagType.specifier;
        },
        .BitInt, .ComplexBitInt => return .{ .specifier = specifier, .data = ty.data },
        else => {},
    }

    return .{
        .specifier = switch (specifier) {
            .Bool, .Char, .SChar, .UChar, .Short => .Int,
            .UShort => if (ty.sizeof(comp).? == sizeof(Type.Int, comp)) .UInt else .Int,

            .Int,
            .UInt,
            .Long,
            .ULong,
            .LongLong,
            .ULongLong,
            .Int128,
            .UInt128,
            .ComplexChar,
            .ComplexSChar,
            .ComplexUChar,
            .ComplexShort,
            .ComplexUShort,
            .ComplexInt,
            .ComplexUInt,
            .ComplexLong,
            .ComplexULong,
            .ComplexLongLong,
            .ComplexULongLong,
            .ComplexInt128,
            .ComplexUInt128,
            .BitInt,
            .ComplexBitInt,
            => specifier,

            .TypeofType => return ty.data.subType.integerPromotion(comp),
            .TypeofExpr => return ty.data.expr.ty.integerPromotion(comp),
            .Attributed => return ty.data.attributed.base.integerPromotion(comp),

            .Invalid => .Invalid,
            else => unreachable, // not an integer type
        },
    };
}

pub fn hasIncompleteSize(ty: Type) bool {
    if (ty.isDecayed())
        return false;

    return switch (ty.specifier) {
        .Void, .IncompleteArray => true,
        .Enum => ty.data.@"enum".isIncomplete() and !ty.data.@"enum".fixed,
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
            .UnspecifiedVariableLenArray => return true,

            .Array,
            .StaticArray,
            .IncompleteArray,
            .VariableLenArray,
            => cur = cur.getElemType(),

            .TypeofType => cur = cur.data.subType.*,
            .TypeofExpr => cur = cur.data.expr.ty,
            .Attributed => cur = cur.data.attributed.base,

            else => return false,
        }
    }
}

pub fn maxInt(ty: Type, comp: *const Compilation) u64 {
    assert(ty.isInt());
    return switch (ty.sizeof(comp).?) {
        1 => if (ty.isUnsignedInt(comp)) @as(u64, std.math.maxInt(u8)) else std.math.maxInt(i8),
        2 => if (ty.isUnsignedInt(comp)) @as(u64, std.math.maxInt(u16)) else std.math.maxInt(i16),
        4 => if (ty.isUnsignedInt(comp)) @as(u64, std.math.maxInt(u32)) else std.math.maxInt(i32),
        8 => if (ty.isUnsignedInt(comp)) @as(u64, std.math.maxInt(u64)) else std.math.maxInt(i64),
        else => unreachable,
    };
}

pub fn minInt(ty: Type, comp: *const Compilation) i64 {
    assert(ty.isInt());
    if (ty.isUnsignedInt(comp)) return 0;
    return switch (ty.sizeof(comp).?) {
        1 => std.math.minInt(i8),
        2 => std.math.minInt(i16),
        4 => std.math.minInt(i32),
        8 => std.math.minInt(i64),
        else => unreachable,
    };
}

/// Check if the given type `ty` has a field with the specified `name`
pub fn hasField(ty: Type, name: StringId) bool {
    return ty.getRecord().?.hasField(name);
}

const TypeSizeOrder = enum {
    lt,
    gt,
    eq,
    indeterminate,
};

pub fn sizeCompare(lhs: Type, rhs: Type, comp: *Compilation) TypeSizeOrder {
    const lhsSize = lhs.sizeof(comp) orelse return .indeterminate;
    const rhsSize = rhs.sizeof(comp) orelse return .indeterminate;
    return switch (std.math.order(lhsSize, rhsSize)) {
        .lt => .lt,
        .gt => .gt,
        .eq => .eq,
    };
}

/// Size of type as reported by sizeof
pub fn sizeof(ty: Type, comp: *const Compilation) ?u64 {
    if (ty.isPointer())
        return comp.target.ptrBitWidth() / 8;

    return switch (ty.specifier) {
        .AutoType, .C23Auto => unreachable,
        .VariableLenArray, .UnspecifiedVariableLenArray => null,

        .IncompleteArray => return if (comp.langOpts.emulate == .msvc) @as(?u64, 0) else null,

        .Func,
        .VarArgsFunc,
        .OldStyleFunc,
        .Void,
        .Bool,
        => 1,

        .Char, .SChar, .UChar => 1,
        .Short => comp.target.cTypeByteSize(.short),
        .UShort => comp.target.cTypeByteSize(.ushort),
        .Int => comp.target.cTypeByteSize(.int),
        .UInt => comp.target.cTypeByteSize(.uint),
        .Long => comp.target.cTypeByteSize(.long),
        .ULong => comp.target.cTypeByteSize(.ulong),
        .LongLong => comp.target.cTypeByteSize(.longlong),
        .ULongLong => comp.target.cTypeByteSize(.ulonglong),
        .LongDouble => comp.target.cTypeByteSize(.longdouble),

        .Int128, .UInt128 => 16,

        .FP16 => 2,
        .Float => comp.target.cTypeByteSize(.float),
        .Double => comp.target.cTypeByteSize(.double),
        .Float80 => 16,
        .Float128 => 16,

        .BitInt => return std.mem.alignForward(u64, (ty.data.int.bits + 7) / 8, ty.alignof(comp)),

        .ComplexChar,
        .ComplexSChar,
        .ComplexUChar,
        .ComplexShort,
        .ComplexUShort,
        .ComplexInt,
        .ComplexUInt,
        .ComplexLong,
        .ComplexULong,
        .ComplexLongLong,
        .ComplexULongLong,
        .ComplexInt128,
        .ComplexUInt128,
        .ComplexFP16,
        .ComplexFloat,
        .ComplexDouble,
        .ComplexLongDouble,
        .ComplexFloat80,
        .ComplexFloat128,
        .ComplexBitInt,
        => return 2 * ty.makeReal().sizeof(comp).?,

        .Pointer => unreachable,
        .StaticArray, .NullPtrTy => comp.target.ptrBitWidth() / 8,

        .Array, .Vector => {
            const size = ty.data.array.elem.sizeof(comp) orelse return null;
            const arraySize = size * ty.data.array.len;
            if (comp.langOpts.emulate == .msvc) {
                // msvc ignores array type alignment.
                // Since the size might not be a multiple of the field
                // alignment, the address of the second element might not be properly aligned
                // for the field alignment. A flexible array has size 0. See test case 0018.
                return arraySize;
            } else {
                return std.mem.alignForward(u64, arraySize, ty.alignof(comp));
            }
        },

        .Struct, .Union => if (ty.data.record.isIncomplete()) null else @as(u64, ty.data.record.typeLayout.sizeBits / 8),
        .Enum => if (ty.data.@"enum".isIncomplete() and !ty.data.@"enum".fixed) null else ty.data.@"enum".tagType.sizeof(comp),

        .TypeofType => ty.data.subType.sizeof(comp),
        .TypeofExpr => ty.data.expr.ty.sizeof(comp),
        .Attributed => ty.data.attributed.base.sizeof(comp),

        .Invalid => null,
        else => unreachable,
    };
}

pub fn bitSizeof(ty: Type, comp: *const Compilation) ?u64 {
    return switch (ty.specifier) {
        .Bool => if (comp.langOpts.emulate == .msvc) @as(u64, 8) else 1,
        .TypeofType => ty.data.subType.bitSizeof(comp),
        .TypeofExpr => ty.data.expr.ty.bitSizeof(comp),
        .Attributed => ty.data.attributed.base.bitSizeof(comp),
        .BitInt => return ty.data.int.bits,
        .LongDouble => comp.target.cTypeBitSize(.longdouble),
        .Float80 => return 80,
        else => 8 * (ty.sizeof(comp) orelse return null),
    };
}

pub fn alignable(ty: Type) bool {
    return ty.isArray() or !ty.hasIncompleteSize() or ty.is(.Void);
}

/// Get the alignment of a type
pub fn alignof(ty: Type, comp: *const Compilation) u29 {
    // don't return the attribute for records
    // layout has already accounted for requested alignment
    if (ty.requestedAlignment(comp)) |requested| {
        // gcc does not respect alignment on enums
        if (ty.get(.Enum)) |tyEnum| {
            if (comp.langOpts.emulate == .gcc)
                return tyEnum.alignof(comp);
        } else if (ty.getRecord()) |rec| {
            if (ty.hasIncompleteSize()) return 0;
            const computed: u29 = @intCast(@divExact(rec.typeLayout.fieldAlignmentBits, 8));
            return @max(requested, computed);
        } else if (comp.langOpts.emulate == .msvc) {
            const typeAlign = ty.data.attributed.base.alignof(comp);
            return @max(requested, typeAlign);
        }
        return requested;
    }

    return switch (ty.specifier) {
        .Invalid => unreachable,
        .AutoType, .C23Auto => unreachable,

        .UnspecifiedVariableLenArray,
        .VariableLenArray,
        .IncompleteArray,
        .Array,
        .Vector,
        => if (ty.isPointer()) switch (comp.target.cpu.arch) {
            .avr => 1,
            else => comp.target.ptrBitWidth() / 8,
        } else ty.getElemType().alignof(comp),

        .Func, .VarArgsFunc, .OldStyleFunc => Target.defaultFunctionAlignment(comp.target),
        .Char, .SChar, .UChar, .Void, .Bool => 1,

        .ComplexChar,
        .ComplexSChar,
        .ComplexUChar,
        .ComplexShort,
        .ComplexUShort,
        .ComplexInt,
        .ComplexUInt,
        .ComplexLong,
        .ComplexULong,
        .ComplexLongLong,
        .ComplexULongLong,
        .ComplexInt128,
        .ComplexUInt128,
        .ComplexFP16,
        .ComplexFloat,
        .ComplexDouble,
        .ComplexLongDouble,
        .ComplexFloat80,
        .ComplexFloat128,
        .ComplexBitInt,
        => return ty.makeReal().alignof(comp),

        .Short => comp.target.cTypeAlignment(.short),
        .UShort => comp.target.cTypeAlignment(.ushort),
        .Int => comp.target.cTypeAlignment(.int),
        .UInt => comp.target.cTypeAlignment(.uint),
        .Long => comp.target.cTypeAlignment(.long),
        .ULong => comp.target.cTypeAlignment(.ulong),
        .LongLong => comp.target.cTypeAlignment(.longlong),
        .ULongLong => comp.target.cTypeAlignment(.ulonglong),

        .BitInt => {
            // https://www.open-std.org/jtc1/sc22/wg14/www/docs/n2709.pdf
            // _BitInt(N) types align with existing calling conventions. They have the same size and alignment as the
            // smallest basic type that can contain them. Types that are larger than __int64_t are conceptually treated
            // as struct of register size chunks. The number of chunks is the smallest number that can contain the type.
            if (ty.data.int.bits > 64) return 8;
            const basicType = comp.intLeastN(ty.data.int.bits, ty.data.int.signedness);
            return basicType.alignof(comp);
        },

        .Int128,
        .UInt128,
        => if (comp.target.cpu.arch == .s390x and
            comp.target.os.tag == .linux and
            comp.target.isGnu()) 8 else 16,

        .FP16 => 2,
        .Float => comp.target.cTypeAlignment(.float),
        .Double => comp.target.cTypeAlignment(.double),
        .LongDouble => comp.target.cTypeAlignment(.longdouble),

        .Float80, .Float128 => 16,

        .Pointer,
        .StaticArray,
        .NullPtrTy,
        => switch (comp.target.cpu.arch) {
            .avr => 1,
            else => comp.target.ptrBitWidth() / 8,
        },

        .Struct, .Union => if (ty.data.record.isIncomplete()) 0 else @intCast(ty.data.record.typeLayout.fieldAlignmentBits / 8),
        .Enum => if (ty.data.@"enum".isIncomplete() and !ty.data.@"enum".fixed) 0 else ty.data.@"enum".tagType.alignof(comp),

        .TypeofType => ty.data.subType.alignof(comp),
        .TypeofExpr => ty.data.expr.ty.alignof(comp),
        .Attributed => ty.data.attributed.base.alignof(comp),

        else => unreachable,
    };
}

pub fn enumIsPacked(ty: Type, comp: *const Compilation) bool {
    assert(ty.is(.Enum));
    return comp.langOpts.shortEnums or Target.packAllEnums(comp.target) or ty.hasAttribute(.@"packed");
}

pub fn getName(ty: Type) StringId {
    return switch (ty.specifier) {
        .TypeofType => if (ty.name == .empty) ty.data.subType.getName() else ty.name,
        .TypeofExpr => if (ty.name == .empty) ty.data.expr.ty.getName() else ty.name,
        .Attributed => if (ty.name == .empty) ty.data.attributed.base.getName() else ty.name,
        else => ty.name,
    };
}

pub fn requestedAlignment(ty: Type, comp: *const Compilation) ?u29 {
    return switch (ty.specifier) {
        .TypeofType => ty.data.subType.requestedAlignment(comp),
        .TypeofExpr => ty.data.expr.ty.requestedAlignment(comp),
        .Attributed => annotationAlignment(comp, ty.data.attributed.attributes),
        else => null,
    };
}

/// Determines the maximum alignment requested by a set of attributes.
/// This function iterates over each attribute in the given array and checks if it is an 'aligned'
/// attribute. If so, it gets the requested alignment value for that attribute. It then determines
/// the maximum alignment requested among all 'aligned' attributes.
/// If no 'aligned' attributes are present, the function returns null.
/// @param comp A pointer to the Compilation context.
/// @param attrs An optional array of Attribute structures to inspect.
/// @return The maximum requested alignment as u29, or null if not applicable.
pub fn annotationAlignment(comp: *const Compilation, attrs: ?[]const Attribute) ?u29 {
    // If attrs is null, exit early as there are no attributes to process.
    const a = attrs orelse return null;

    // Initialize maxRequested to null, to be updated with the max alignment found.
    var maxRequested: ?u29 = null;

    for (a) |attribute| {
        // Skip any attribute that is not tagged as 'aligned'.
        if (attribute.tag != .aligned) continue;

        // Get the requested alignment from the attribute, or use the default if not specified.
        const requested = if (attribute.args.aligned.alignment) |alignment|
            alignment.requested
        else
            Target.defaultAlignment(comp.target);

        if (maxRequested == null or maxRequested.? < requested)
            maxRequested = requested;
    }

    return maxRequested;
}

/// Returns a canonicalized version of the given type without cv qualifiers.
pub fn stripCVTypeQuals(ty: Type) Type {
    //  unwrap typeof-types when checking compatibility
    var tyUnqual = ty.canonicalize(.standard);
    tyUnqual.qual.removeCVQualifiers();
    return tyUnqual;
}

pub fn eql(lhsParam: Type, rhsParam: Type, comp: *const Compilation, checkQualifiers: bool) bool {
    const lhs = lhsParam.canonicalize(.standard);
    const rhs = rhsParam.canonicalize(.standard);

    if (lhs.specifier == .Invalid or rhs.specifier == .Invalid)
        return false;

    if (lhs.alignof(comp) != rhs.alignof(comp))
        return false;

    if (lhs.isPointer()) {
        if (!rhs.isPointer()) return false;
    } else if (lhs.isFunc()) {
        if (!rhs.isFunc()) return false;
    } else if (lhs.isArray()) {
        if (!rhs.isArray()) return false;
    } else if (lhs.specifier != rhs.specifier)
        return false;

    if (lhs.qual.atomic != rhs.qual.atomic)
        return false;

    if (checkQualifiers) {
        if (lhs.qual.@"const" != rhs.qual.@"const") return false;
        if (lhs.qual.@"volatile" != rhs.qual.@"volatile") return false;
    }

    if (lhs.isPointer())
        return lhsParam.getElemType().eql(rhsParam.getElemType(), comp, checkQualifiers);

    switch (lhs.specifier) {
        .Pointer => unreachable,

        .Func,
        .VarArgsFunc,
        .OldStyleFunc,
        => if (!lhs.data.func.eql(rhs.data.func, lhs.specifier, rhs.specifier, comp)) return false,

        .Array,
        .StaticArray,
        .IncompleteArray,
        .Vector,
        => {
            const lhsLen = lhs.arrayLen();
            const rhsLen = rhs.arrayLen();
            if (lhsLen == null or rhsLen == null) {
                // At least one array is incomplete; only check child type for equality
            } else if (lhsLen.? != rhsLen.?) {
                return false;
            }

            if (!lhs.getElemType().eql(rhs.getElemType(), comp, false))
                return false;
        },

        .VariableLenArray => if (!lhs.getElemType().eql(rhs.getElemType(), comp, checkQualifiers)) return false,

        .Union, .Struct => if (lhs.data.record != rhs.data.record) return false,
        .Enum => if (lhs.data.@"enum" != rhs.data.@"enum") return false,

        .BitInt, .ComplexBitInt => return (lhs.data.int.bits == rhs.data.int.bits) and (lhs.data.int.signedness == rhs.data.int.signedness),

        else => {},
    }

    return true;
}

pub fn decayArray(ty: *Type) void {
    assert(ty.isArray());
    ty.decayed = true;
}

pub fn originalTypeOfDecayedArray(ty: Type) Type {
    assert(ty.isDecayed());
    var copy = ty;
    copy.decayed = false;
    return copy;
}

/// Rank for integer conversions, ignoring domain (complex vs real)
/// Asserts that ty is an integer type
pub fn integerRank(ty: Type, comp: *const Compilation) usize {
    const real = ty.makeReal();
    return @intCast(switch (real.specifier) {
        .BitInt => @as(u64, real.data.int.bits) << 3,

        .Bool => 1 + (ty.bitSizeof(comp).? << 3),
        .Char, .SChar, .UChar => 2 + (ty.bitSizeof(comp).? << 3),
        .Short, .UShort => 3 + (ty.bitSizeof(comp).? << 3),
        .Int, .UInt => 4 + (ty.bitSizeof(comp).? << 3),
        .Long, .ULong => 5 + (ty.bitSizeof(comp).? << 3),
        .LongLong, .ULongLong => 6 + (ty.bitSizeof(comp).? << 3),
        .Int128, .UInt128 => 7 + (ty.bitSizeof(comp).? << 3),

        else => unreachable,
    });
}

/// Returns true if `lhs` and `rhs` are integer types that differ only in sign
pub fn sameRankDifferentSign(lhs: Type, rhs: Type, comp: *const Compilation) bool {
    if (!lhs.isInt() or !rhs.isInt()) return false;
    if (lhs.integerRank(comp) != rhs.integerRank(comp)) return false;
    return lhs.isUnsignedInt(comp) != rhs.isUnsignedInt(comp);
}

pub fn makeReal(ty: Type) Type {
    // TODO discards attributed/typeof
    var base = ty.canonicalize(.standard);
    switch (base.specifier) {
        .ComplexFP16,
        .ComplexFloat,
        .ComplexDouble,
        .ComplexLongDouble,
        .ComplexFloat80,
        .ComplexFloat128,
        => {
            base.specifier = @enumFromInt(@intFromEnum(base.specifier) - 6);
            return base;
        },

        .ComplexBitInt => {
            base.specifier = .BitInt;
            return base;
        },

        .ComplexChar,
        .ComplexSChar,
        .ComplexUChar,
        .ComplexShort,
        .ComplexUShort,
        .ComplexInt,
        .ComplexUInt,
        .ComplexLong,
        .ComplexULong,
        .ComplexLongLong,
        .ComplexULongLong,
        .ComplexInt128,
        .ComplexUInt128,
        => {
            base.specifier = @enumFromInt(@intFromEnum(base.specifier) - 13);
            return base;
        },

        else => return ty,
    }
}

pub fn makeComplex(ty: Type) Type {
    // TODO discards attributed/typeof
    var base = ty.canonicalize(.standard);
    switch (base.specifier) {
        .FP16,
        .Float,
        .Double,
        .LongDouble,
        .Float80,
        .Float128,
        => {
            base.specifier = @enumFromInt(@intFromEnum(base.specifier) + 6);
            return base;
        },

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
        .Int128,
        .UInt128,
        => {
            base.specifier = @enumFromInt(@intFromEnum(base.specifier) + 13);
            return base;
        },

        .BitInt => {
            base.specifier = .ComplexBitInt;
            return base;
        },

        else => return ty,
    }
}

/// Combines types recursively in the order they were parsed, uses `.void` specifier as a sentinel value.
pub fn combine(inner: *Type, outer: Type) Parser.Error!void {
    switch (inner.specifier) {
        .Pointer => return inner.data.subType.combine(outer),

        .UnspecifiedVariableLenArray => {
            assert(!inner.isDecayed());
            try inner.data.subType.combine(outer);
        },

        .Array, .StaticArray, .IncompleteArray => {
            assert(!inner.isDecayed());
            try inner.data.array.elem.combine(outer);
        },

        .VariableLenArray => {
            assert(!inner.isDecayed());
            try inner.data.expr.ty.combine(outer);
        },

        .Func,
        .VarArgsFunc,
        .OldStyleFunc,
        => try inner.data.func.returnType.combine(outer),

        .TypeofType, .TypeofExpr => assert(!inner.isDecayed()),

        .Void, .Invalid => inner.* = outer,
        else => unreachable,
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

        .TypeofType => return ty.data.subType.validateCombinedType(p, sourceToken),
        .TypeofExpr => return ty.data.expr.ty.validateCombinedType(p, sourceToken),
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
        .TypeofType => ty.data.subType.getAttributes(),
        .TypeofExpr => ty.data.expr.ty.getAttributes(),
        else => &.{},
    };
}

pub fn getRecord(ty: Type) ?*const Type.Record {
    return switch (ty.specifier) {
        .Attributed => ty.data.attributed.base.getRecord(),
        .TypeofType => ty.data.subType.getRecord(),
        .TypeofExpr => ty.data.expr.ty.getRecord(),
        .Struct, .Union => ty.data.record,
        else => null,
    };
}

pub fn hasAttribute(ty: Type, tag: Attribute.Tag) bool {
    for (ty.getAttributes()) |attr| {
        if (attr.tag == tag) return true;
    }
    return false;
}

pub fn compareIntegerRanks(lhs: Type, rhs: Type, comp: *const Compilation) std.math.Order {
    assert(lhs.isInt() and rhs.isInt());
    if (lhs.eql(rhs, comp, false)) return .eq;

    const lhsUnsigned = lhs.isUnsignedInt(comp);
    const rhsUnsigned = rhs.isUnsignedInt(comp);

    const lhsRank = lhs.integerRank(comp);
    const rhsRank = rhs.integerRank(comp);
    if (lhsUnsigned == rhsUnsigned)
        return std.math.order(lhsRank, rhsRank);

    if (lhsUnsigned) {
        if (lhsRank >= rhsRank) return .gt;
        return .lt;
    }

    assert(rhsUnsigned);
    if (rhsRank >= lhsRank) return .lt;
    return .gt;
}

fn realIntegerConversion(lhs: Type, rhs: Type, comp: *const Compilation) Type {
    assert(lhs.isReal() and rhs.isReal());

    const typeOrder = lhs.compareIntegerRanks(rhs, comp);
    const lhsSigned = !lhs.isUnsignedInt(comp);
    const rhsSigned = !rhs.isUnsignedInt(comp);
    if (lhsSigned == rhsSigned) {
        // If both have the same sign, use higher-rank type.
        return switch (typeOrder) {
            .lt => rhs,
            .eq, .gt => lhs,
        };
    } else if (typeOrder != if (lhsSigned) std.math.Order.gt else std.math.Order.lt) {
        // Only one is signed; and the unsigned type has rank >= the signed type
        // Use the unsigned type
        return if (rhsSigned) lhs else rhs;
    } else if (lhs.bitSizeof(comp).? != rhs.bitSizeof(comp).?) {
        // Signed type is higher rank and sizes are not equal
        // Use the signed type
        return if (lhsSigned) lhs else rhs;
    } else {
        // Signed type is higher rank but same size as unsigned type
        // e.g. `long` and `unsigned` on x86-linux-gnu
        // Use unsigned version of the signed type
        return if (lhsSigned) lhs.makeIntegerUnsigned() else rhs.makeIntegerUnsigned();
    }
}

pub fn makeIntegerUnsigned(ty: Type) Type {
    // TODO discards attributed/typeof
    var base = ty.canonicalize(.standard);
    switch (base.specifier) {
        // zig fmt: off
        .UChar, .UShort, .UInt, .ULong, .ULongLong, .UInt128,
        .ComplexUChar, .ComplexUShort, .ComplexUInt, .ComplexULong, .ComplexULongLong, .ComplexUInt128,
        => return ty,
        // zig fmt: on

        .Char, .ComplexChar => {
            base.specifier = @enumFromInt(@intFromEnum(base.specifier) + 2);
            return base;
        },

        // zig fmt: off
        .SChar, .Short, .Int, .Long, .LongLong, .Int128,
        .ComplexSChar, .ComplexShort, .ComplexInt, .ComplexLong, .ComplexLongLong, .ComplexInt128 => {
            base.specifier = @enumFromInt(@intFromEnum(base.specifier) + 1);
            return base;
        },
        // zig fmt: on

        .BitInt, .ComplexBitInt => {
            base.data.int.signedness = .unsigned;
            return base;
        },

        else => unreachable,
    }
}

/// Find the common type of a and b for binary operations
pub fn integerConversion(lhs: Type, rhs: Type, comp: *const Compilation) Type {
    const lhsReal = lhs.isReal();
    const rhsReal = rhs.isReal();
    const targetTy = lhs.makeReal().realIntegerConversion(rhs.makeReal(), comp);
    return if (lhsReal and rhsReal) targetTy else targetTy.makeComplex();
}

/// printf format modifier
pub fn formatModifier(ty: Type) []const u8 {
    return switch (ty.specifier) {
        .SChar, .UChar => "hh",
        .Short, .UShort => "h",
        .Int, .UInt => "",
        .Long, .ULong => "l",
        .LongLong, .ULongLong => "ll",
        else => unreachable,
    };
}

/// Suffix for integer values of this type
pub fn intValueSuffix(ty: Type, comp: *const Compilation) []const u8 {
    return switch (ty.specifier) {
        .SChar, .Short, .Int => "",
        .Long => "L",
        .LongLong => "LL",
        .UChar, .Char => {
            if (ty.specifier == .Char and comp.getCharSignedness() == .signed) return "";
            // Only 8-bit char supported currently;
            // TODO: handle platforms with 16-bit int + 16-bit char
            assert(ty.sizeof(comp).? == 1);
            return "";
        },
        .UShort => {
            if (ty.sizeof(comp).? < Int.sizeof(comp).?)
                return "";
            return "U";
        },
        .UInt => "U",
        .ULong => "UL",
        .ULongLong => "ULL",
        else => unreachable, // not integer
    };
}

/// Print type in C style
pub fn print(ty: Type, mapper: StringInterner.TypeMapper, langOpts: LangOpts, w: anytype) @TypeOf(w).Error!void {
    _ = try ty.printPrologue(mapper, langOpts, w);
    try ty.printEpilogue(mapper, langOpts, w);
}

pub fn printNamed(
    ty: Type,
    name: []const u8,
    mapper: StringInterner.TypeMapper,
    langOpts: LangOpts,
    w: anytype,
) @TypeOf(w).Error!void {
    const simple = try ty.printPrologue(mapper, langOpts, w);
    if (simple) try w.writeByte(' ');
    try w.writeAll(name);
    try ty.printEpilogue(mapper, langOpts, w);
}

const StringGetter = fn (TokenIndex) []const u8;

/// return true if `ty` is simple
fn printPrologue(ty: Type, mapper: StringInterner.TypeMapper, langOpts: LangOpts, w: anytype) @TypeOf(w).Error!bool {
    if (ty.qual.atomic) {
        var nonAtomicType = ty;
        nonAtomicType.qual.atomic = false;
        try w.writeAll("_Atomic(");
        try nonAtomicType.print(mapper, langOpts, w);
        try w.writeAll(")");
        return true;
    }

    if (ty.isPointer()) {
        const elemType = ty.getElemType();
        const simple = try elemType.printPrologue(mapper, langOpts, w);
        if (simple) try w.writeByte(' ');
        if (elemType.isFunc() or elemType.isArray()) try w.writeByte('(');
        try w.writeByte('*');
        try ty.qual.dump(w);
        return false;
    }

    switch (ty.specifier) {
        .Pointer => unreachable,
        .Func, .VarArgsFunc, .OldStyleFunc => {
            const retType = ty.data.func.returnType;
            const simple = try retType.printPrologue(mapper, langOpts, w);
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
            const simple = try elemType.printPrologue(mapper, langOpts, w);
            if (simple) try w.writeByte(' ');
            return false;
        },

        .TypeofType,
        .TypeofExpr,
        => {
            const actual = ty.canonicalize(.standard);
            return actual.printPrologue(mapper, langOpts, w);
        },

        .Attributed => {
            const actual = ty.canonicalize(.standard);
            return actual.printPrologue(mapper, langOpts, w);
        },

        else => {},
    }
    try ty.qual.dump(w);

    switch (ty.specifier) {
        .Enum => if (ty.data.@"enum".fixed) {
            try w.print("enum {s}: ", .{mapper.lookup(ty.data.@"enum".name)});
            try ty.data.@"enum".tagType.dump(mapper, langOpts, w);
        } else {
            try w.print("enum {s}", .{mapper.lookup(ty.data.@"enum".name)});
        },

        .Struct => try w.print("struct {s}", .{mapper.lookup(ty.data.record.name)}),
        .Union => try w.print("union {s}", .{mapper.lookup(ty.data.record.name)}),

        .Vector => {
            const len = ty.data.array.len;
            const elem_ty = ty.data.array.elem;
            try w.print("__attribute__((__vector_size__({d} * sizeof(", .{len});
            _ = try elem_ty.printPrologue(mapper, langOpts, w);
            try w.writeAll(")))) ");
            _ = try elem_ty.printPrologue(mapper, langOpts, w);
            try w.print(" (vector of {d} '", .{len});
            _ = try elem_ty.printPrologue(mapper, langOpts, w);
            try w.writeAll("' values)");
        },

        .BitInt => try w.print("{s} _BitInt({d})", .{ @tagName(ty.data.int.signedness), ty.data.int.bits }),
        .ComplexBitInt => try w.print("_Complex {s} _BitInt({d})", .{ @tagName(ty.data.int.signedness), ty.data.int.bits }),

        else => try w.writeAll(TypeBuilder.fromType(ty).toString(langOpts).?),
    }

    return true;
}

fn printEpilogue(ty: Type, mapper: StringInterner.TypeMapper, langOpts: LangOpts, w: anytype) @TypeOf(w).Error!void {
    if (ty.qual.atomic) return;

    if (ty.isPointer()) {
        const elemType = ty.getElemType();
        if (elemType.isFunc() or elemType.isArray()) try w.writeByte(')');
        try elemType.printEpilogue(mapper, langOpts, w);
        return;
    }

    switch (ty.specifier) {
        .Pointer => unreachable,
        .Func, .VarArgsFunc, .OldStyleFunc => {
            try w.writeByte('(');
            for (ty.data.func.params, 0..) |param, i| {
                if (i != 0) try w.writeAll(", ");
                _ = try param.ty.printPrologue(mapper, langOpts, w);
                try param.ty.printEpilogue(mapper, langOpts, w);
            }
            if (!ty.is(.Func)) {
                if (ty.data.func.params.len != 0) try w.writeAll(", ");
                try w.writeAll("...");
            } else if (ty.data.func.params.len == 0) {
                try w.writeAll("void");
            }
            try w.writeByte(')');
            try ty.data.func.returnType.printEpilogue(mapper, langOpts, w);
        },

        .Array, .StaticArray => {
            try w.writeByte('[');
            if (ty.is(.StaticArray)) try w.writeAll("static ");
            try ty.qual.dump(w);
            try w.print("{d}]", .{ty.data.array.len});
            try ty.data.array.elem.printEpilogue(mapper, langOpts, w);
        },

        .IncompleteArray => {
            try w.writeByte('[');
            try ty.qual.dump(w);
            try w.writeByte(']');
            try ty.data.array.elem.printEpilogue(mapper, langOpts, w);
        },

        .UnspecifiedVariableLenArray => {
            try w.writeByte('[');
            try ty.qual.dump(w);
            try w.writeAll("*]");
            try ty.data.subType.printEpilogue(mapper, langOpts, w);
        },

        .VariableLenArray => {
            try w.writeByte('[');
            try ty.qual.dump(w);
            try w.writeAll("<expr>]");
            try ty.data.expr.ty.printEpilogue(mapper, langOpts, w);
        },

        .TypeofType, .TypeofExpr => {
            const actual = ty.canonicalize(.standard);
            try actual.printEpilogue(mapper, langOpts, w);
        },

        .Attributed => {
            const actual = ty.canonicalize(.standard);
            try actual.printEpilogue(mapper, langOpts, w);
        },

        else => {},
    }
}

/// Useful for debugging, too noisy to be enabled by default.
const DumpDetailedContainers = false;

pub fn dump(ty: Type, mapper: StringInterner.TypeMapper, langOpts: LangOpts, w: anytype) @TypeOf(w).Error!void {
    try ty.qual.dump(w);

    switch (ty.specifier) {
        .Invalid => try w.writeAll("invalid"),
        .Pointer => {
            try w.writeAll("*");
            try ty.data.subType.dump(mapper, langOpts, w);
        },

        .Func, .VarArgsFunc, .OldStyleFunc => {
            if (ty.specifier == .OldStyleFunc)
                try w.writeAll("kr (")
            else
                try w.writeAll("fn (");
            for (ty.data.func.params, 0..) |param, i| {
                if (i != 0)
                    try w.writeAll(", ");
                if (param.name != .empty)
                    try w.print("{s}: ", .{mapper.lookup(param.name)});
                try param.ty.dump(mapper, langOpts, w);
            }

            if (!ty.isFunc()) {
                if (ty.data.func.params.len != 0) try w.writeAll(", ");
                try w.writeAll("...");
            }

            try w.writeAll(") ");
            try ty.data.func.returnType.dump(mapper, langOpts, w);
        },

        .Array, .StaticArray => {
            if (ty.isDecayed()) try w.writeAll("*d");
            try w.writeAll("[");
            if (ty.is(.StaticArray)) try w.writeAll("static ");
            try w.print("{d}]", .{ty.data.array.len});
            try ty.data.array.elem.dump(mapper, langOpts, w);
        },

        .Vector => {
            try w.print("vector(({d}, ", .{ty.data.array.len});
            try ty.data.array.elem.dump(mapper, langOpts, w);
            try w.writeAll(")");
        },

        .IncompleteArray => {
            if (ty.isDecayed()) try w.writeAll("*d");
            try w.writeAll("[]");
            try ty.data.array.elem.dump(mapper, langOpts, w);
        },

        .Enum => {
            const enumTy = ty.data.@"enum";
            if (enumTy.isIncomplete() and !enumTy.fixed) {
                try w.print("enum {s}", .{mapper.lookup(enumTy.name)});
            } else {
                try w.print("enum {s}: ", .{mapper.lookup(enumTy.name)});
                try enumTy.tagType.dump(mapper, langOpts, w);
            }

            if (DumpDetailedContainers)
                try dumpEnum(enumTy, mapper, w);
        },

        .Struct => {
            try w.print("struct {s}", .{mapper.lookup(ty.data.record.name)});
            if (DumpDetailedContainers)
                try dumpRecord(ty.data.record, mapper, langOpts, w);
        },

        .Union => {
            try w.print("union {s}", .{mapper.lookup(ty.data.record.name)});
            if (DumpDetailedContainers)
                try dumpRecord(ty.data.record, mapper, langOpts, w);
        },

        .UnspecifiedVariableLenArray => {
            if (ty.isDecayed()) try w.writeAll("*d");
            try w.writeAll("[*]");
            try ty.data.subType.dump(mapper, langOpts, w);
        },

        .VariableLenArray => {
            if (ty.isDecayed()) try w.writeAll("*d");
            try w.writeAll("[<expr>]");
            try ty.data.expr.ty.dump(mapper, langOpts, w);
        },

        .TypeofType => {
            try w.writeAll("typeof(");
            try ty.data.subType.dump(mapper, langOpts, w);
            try w.writeAll(")");
        },

        .TypeofExpr => {
            try w.writeAll("typeof(<expr>: ");
            try ty.data.expr.ty.dump(mapper, langOpts, w);
            try w.writeAll(")");
        },

        .Attributed => {
            if (ty.isDecayed()) try w.writeAll("*d:");
            try w.writeAll("attributed(");
            try ty.data.attributed.base.dump(mapper, langOpts, w);
            try w.writeAll(")");
        },

        .SpecialVaStart => try w.writeAll("(var start param)"),

        .BitInt => try w.print("{s} _BitInt({d})", .{ @tagName(ty.data.int.signedness), ty.data.int.bits }),
        .ComplexBitInt => try w.print("_Complex {s} _BitInt({d})", .{ @tagName(ty.data.int.signedness), ty.data.int.bits }),

        else => try w.writeAll(TypeBuilder.fromType(ty).toString(langOpts).?),
    }
}

fn dumpEnum(@"enum": *Enum, mapper: StringInterner.TypeMapper, w: anytype) @TypeOf(w).Error!void {
    try w.writeAll(" {");
    for (@"enum".fields) |field|
        try w.print(" {s} = {d},", .{ mapper.lookup(field.name), field.value });
    try w.writeAll(" }");
}

fn dumpRecord(record: *Record, mapper: StringInterner.TypeMapper, langOpts: LangOpts, w: anytype) @TypeOf(w).Error!void {
    try w.writeAll(" {");
    for (record.fields) |field| {
        try w.writeByte(' ');
        try field.ty.dump(mapper, langOpts, w);
        try w.print(" {s}: {d};", .{ mapper.lookup(field.name), field.bitWidth });
    }
    try w.writeAll(" }");
}
