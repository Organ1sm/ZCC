const Type = @import("Type.zig");
const TokenIndex = @import("AST.zig").TokenIndex;
const NodeIndex = @import("AST.zig").NodeIndex;
const Parser = @import("../Parser/Parser.zig");
const Compilation = @import("../Basic/Compilation.zig");
const Qualifiers = @import("Type.zig").Qualifiers;
const Target = @import("../Basic/Target.zig");
const LangOpts = @import("../Basic/LangOpts.zig");

const TypeBuilder = @This();

complexToken: ?TokenIndex = null,
bitIntToken: ?TokenIndex = null,
autoTypeToken: ?TokenIndex = null,
typedef: ?struct {
    token: TokenIndex,
    type: Type,
} = null,

specifier: Specifier = .None,
qual: Qualifiers.Builder = .{},
typeof: ?Type = null,
/// When true an error is returned instead of adding a diagnostic message.
/// Used for trying to combine typedef types.
errorOnInvalid: bool = false,

pub const Specifier = union(enum) {
    None,
    Void,
    /// GNU __auto_type extension
    AutoType,
    /// c23 auto,
    C23Auto,
    NullPtrTy,
    Bool,
    Char,
    SChar,
    UChar,
    ComplexChar,
    ComplexSChar,
    ComplexUChar,

    Unsigned,
    Signed,
    Short,
    SShort,
    UShort,
    ShortInt,
    SShortInt,
    UShortInt,
    Int,
    SInt,
    UInt,
    Long,
    SLong,
    ULong,
    LongInt,
    SLongInt,
    ULongInt,
    LongLong,
    SLongLong,
    ULongLong,
    LongLongInt,
    SLongLongInt,
    ULongLongInt,
    Int128,
    SInt128,
    UInt128,
    ComplexUnsigned,
    ComplexSigned,
    ComplexShort,
    ComplexSShort,
    ComplexUShort,
    ComplexShortInt,
    ComplexSShortInt,
    ComplexUShortInt,
    ComplexInt,
    ComplexSInt,
    ComplexUInt,
    ComplexLong,
    ComplexSLong,
    ComplexULong,
    ComplexLongInt,
    ComplexSLongInt,
    ComplexULongInt,
    ComplexLongLong,
    ComplexSLongLong,
    ComplexULongLong,
    ComplexLongLongInt,
    ComplexSLongLongInt,
    ComplexULongLongInt,
    ComplexInt128,
    ComplexSInt128,
    ComplexUInt128,

    BitInt: u64,
    SBitInt: u64,
    UBitInt: u64,
    ComplexBitInt: u64,
    ComplexSBitInt: u64,
    ComplexUBitInt: u64,

    FP16,
    Float16,
    Float,
    Double,
    LongDouble,
    Float80,
    Float128,
    Complex,
    ComplexFP16,
    ComplexFloat16,
    ComplexFloat,
    ComplexDouble,
    ComplexLongDouble,
    ComplexFloat80,
    ComplexFloat128,

    Pointer: *Type,
    UnspecifiedVariableLenArray: *Type,
    DecayedUnspecifiedVariableLenArray: *Type,
    Func: *Type.Function,
    VarArgsFunc: *Type.Function,
    OldStyleFunc: *Type.Function,
    Array: *Type.Array,
    DecayedArray: *Type.Array,
    StaticArray: *Type.Array,
    DecayedStaticArray: *Type.Array,
    IncompleteArray: *Type.Array,
    DecayedIncompleteArray: *Type.Array,
    Vector: *Type.Array,

    VariableLenArray: *Type.Expr,
    DecayedVariableLenArray: *Type.Expr,

    Struct: *Type.Record,
    Union: *Type.Record,
    Enum: *Type.Enum,

    TypeofType: *Type,
    DecayedTypeofType: *Type,
    TypeofExpr: *Type.Expr,
    DecayedTypeofExpr: *Type.Expr,

    Attributed: *Type.Attributed,
    DecayedAttributed: *Type.Attributed,

    pub fn toString(spec: Specifier, langOpts: LangOpts) ?[]const u8 {
        return switch (spec) {
            .None => unreachable,

            .AutoType => "__auto_type",
            .C23Auto => "auto",

            .Void => "void",
            .NullPtrTy => "nullptr_t",
            .Bool => if (langOpts.standard.atLeast(.c23)) "bool" else "_Bool",

            .Char => "char",
            .SChar => "signed char",
            .UChar => "unsigned char",

            .Unsigned => "unsigned",
            .Signed => "signed",

            .Short => "short",
            .UShort => "unsigned short",
            .SShort => "signed short",

            .ShortInt => "short int",
            .SShortInt => "signed short int",
            .UShortInt => "unsigned short int",
            .Int => "int",
            .SInt => "signed int",
            .UInt => "unsigned int",

            .Long => "long",
            .SLong => "signed long",
            .ULong => "unsigned long",
            .LongInt => "long int",
            .SLongInt => "signed long int",
            .ULongInt => "unsigned long int",
            .LongLong => "long long",
            .SLongLong => "signed long long",
            .ULongLong => "unsigned long long",
            .LongLongInt => "long long int",
            .SLongLongInt => "signed long long int",
            .ULongLongInt => "unsigned long long int",
            .Int128 => "__int128",
            .UInt128 => "unsigned __int128",
            .SInt128 => "signed __int128",

            .ComplexChar => "_Complex char",
            .ComplexSChar => "_Complex signed char",
            .ComplexUChar => "_Complex unsigned char",
            .ComplexUnsigned => "_Complex unsigned",
            .ComplexSigned => "_Complex signed",
            .ComplexShort => "_Complex short",
            .ComplexUShort => "_Complex unsigned short",
            .ComplexSShort => "_Complex signed short",
            .ComplexShortInt => "_Complex short int",
            .ComplexSShortInt => "_Complex signed short int",
            .ComplexUShortInt => "_Complex unsigned short int",
            .ComplexInt => "_Complex int",
            .ComplexSInt => "_Complex signed int",
            .ComplexUInt => "_Complex unsigned int",
            .ComplexLong => "_Complex long",
            .ComplexSLong => "_Complex signed long",
            .ComplexULong => "_Complex unsigned long",
            .ComplexLongInt => "_Complex long int",
            .ComplexSLongInt => "_Complex signed long int",
            .ComplexULongInt => "_Complex unsigned long int",
            .ComplexLongLong => "_Complex long long",
            .ComplexSLongLong => "_Complex signed long long",
            .ComplexULongLong => "_Complex unsigned long long",
            .ComplexLongLongInt => "_Complex long long int",
            .ComplexSLongLongInt => "_Complex signed long long int",
            .ComplexULongLongInt => "_Complex unsigned long long int",
            .ComplexInt128 => "_Complex __int128",
            .ComplexSInt128 => "_Complex signed __int128",
            .ComplexUInt128 => "_Complex unsigned __int128",

            .FP16 => "__fp16",
            .Float16 => "_Float16",
            .Float => "float",
            .Double => "double",
            .LongDouble => "long double",
            .Float80 => "__float80",
            .Float128 => "__float128",
            .Complex => "_Complex",
            .ComplexFP16 => "__Complex __fp16",
            .ComplexFloat16 => "_Complex _Float16",
            .ComplexFloat => "_Complex float",
            .ComplexDouble => "_Complex double",
            .ComplexLongDouble => "_Complex long double",
            .ComplexFloat80 => "_Complex __float80",
            .ComplexFloat128 => "_Complex __float128",

            .Attributed => |attr| TypeBuilder.fromType(attr.base).toString(langOpts),

            else => null,
        };
    }
};

/// Finishes the parsing of a type and constructs the corresponding `Type` structure.
/// This function is called when the parsing of a type is to be completed,
/// with potential handling of typedefs, qualifiers, and array types.
///
/// Arguments:
/// - `b`: The builder context containing information gathered during parsing.
/// - `p`: The parser context to report errors and allocate memory.
///
/// Returns:
/// - `Type` structure representing the parsed type on success.
/// - `Parser.Error` indicating the type of parsing error on failure.
pub fn finish(b: @This(), p: *Parser) Parser.Error!Type {
    var ty = Type{ .specifier = undefined };
    if (b.typedef) |typedef| {
        ty = typedef.type;
        if (ty.isArray()) {
            var elem = ty.getElemType();
            try b.qual.finish(p, &elem);
            // TODO this really should be easier
            switch (ty.specifier) {
                .Array, .StaticArray, .IncompleteArray => {
                    const old = ty.data.array;
                    ty.data.array = try p.arena.create(Type.Array);
                    ty.data.array.* = .{
                        .len = old.len,
                        .elem = elem,
                    };
                },
                .VariableLenArray, .UnspecifiedVariableLenArray => {
                    const old = ty.data.expr;
                    ty.data.expr = try p.arena.create(Type.Expr);
                    ty.data.expr.* = .{
                        .node = old.node,
                        .ty = elem,
                    };
                },
                .TypeofExpr => {}, // TODO handle
                .TypeofType => {}, // TODO handle
                .Attributed => {}, // TODO handle
                else => unreachable,
            }
            return ty;
        }
        try b.qual.finish(p, &ty);
        return ty;
    }

    switch (b.specifier) {
        Specifier.None => {
            if (b.typeof) |typeof| {
                ty = typeof;
            } else {
                ty.specifier = .Int;
                if (p.comp.langOpts.standard.atLeast(.c23))
                    try p.err(.missing_type_specifier_c23)
                else
                    try p.err(.missing_type_specifier);
            }
        },

        // nullptr_t can only be accessed via typeof(nullptr)
        Specifier.NullPtrTy => unreachable,

        Specifier.AutoType => ty.specifier = .AutoType,
        Specifier.C23Auto => ty.specifier = .C23Auto,
        Specifier.Void => ty.specifier = .Void,
        Specifier.Bool => ty.specifier = .Bool,
        Specifier.Char => ty.specifier = .Char,
        Specifier.SChar => ty.specifier = .SChar,
        Specifier.UChar => ty.specifier = .UChar,
        Specifier.ComplexChar => ty.specifier = .ComplexChar,
        Specifier.ComplexSChar => ty.specifier = .ComplexSChar,
        Specifier.ComplexUChar => ty.specifier = .ComplexUChar,

        Specifier.Unsigned => ty.specifier = .UInt,
        Specifier.Signed => ty.specifier = .Int,

        Specifier.ShortInt, Specifier.SShortInt, Specifier.Short, Specifier.SShort => ty.specifier = .Short,
        Specifier.UShort, Specifier.UShortInt => ty.specifier = .UShort,

        Specifier.Int, .SInt => ty.specifier = .Int,
        Specifier.UInt => ty.specifier = .UInt,

        Specifier.Long, Specifier.SLong, Specifier.LongInt, Specifier.SLongInt => ty.specifier = .Long,
        Specifier.ULong, Specifier.ULongInt => ty.specifier = .ULong,

        Specifier.LongLong, Specifier.SLongLong, Specifier.LongLongInt, Specifier.SLongLongInt => ty.specifier = .LongLong,
        Specifier.ULongLong, Specifier.ULongLongInt => ty.specifier = .ULongLong,
        Specifier.Int128, Specifier.SInt128 => ty.specifier = .Int128,
        Specifier.UInt128 => ty.specifier = .UInt128,

        Specifier.ComplexUnsigned => ty.specifier = .ComplexUInt,
        Specifier.ComplexSigned => ty.specifier = .ComplexInt,

        Specifier.ComplexShortInt,
        Specifier.ComplexSShortInt,
        Specifier.ComplexShort,
        Specifier.ComplexSShort,
        => ty.specifier = .ComplexShort,

        Specifier.ComplexUShort, Specifier.ComplexUShortInt => ty.specifier = .ComplexUShort,
        Specifier.ComplexInt, Specifier.ComplexSInt => ty.specifier = .ComplexInt,
        Specifier.ComplexUInt => ty.specifier = .ComplexUInt,

        Specifier.ComplexLong,
        Specifier.ComplexSLong,
        Specifier.ComplexLongInt,
        Specifier.ComplexSLongInt,
        => ty.specifier = .ComplexLong,

        Specifier.ComplexULong, Specifier.ComplexULongInt => ty.specifier = .ComplexULong,

        Specifier.ComplexLongLong,
        Specifier.ComplexSLongLong,
        Specifier.ComplexLongLongInt,
        Specifier.ComplexSLongLongInt,
        => ty.specifier = .ComplexLongLong,

        Specifier.ComplexULongLong,
        Specifier.ComplexULongLongInt,
        => ty.specifier = .ComplexULongLong,

        Specifier.ComplexInt128, Specifier.ComplexSInt128 => ty.specifier = .ComplexInt128,
        Specifier.ComplexUInt128 => ty.specifier = .ComplexUInt128,

        Specifier.BitInt,
        Specifier.SBitInt,
        Specifier.UBitInt,
        Specifier.ComplexBitInt,
        Specifier.ComplexUBitInt,
        Specifier.ComplexSBitInt,
        => |bits| {
            const complexStr = if (b.complexToken != null) "_Complex " else "";
            const unsigned = b.specifier == .UBitInt or b.specifier == .ComplexUBitInt;
            if (unsigned) {
                if (bits < 1) {
                    try p.errStr(.unsigned_bit_int_too_small, b.bitIntToken.?, complexStr);
                    return Type.Invalid;
                }
            } else {
                if (bits < 2) {
                    try p.errStr(.signed_bit_int_too_small, b.bitIntToken.?, complexStr);
                    return Type.Invalid;
                }
            }

            if (bits > Compilation.BitIntMaxBits) {
                try p.errStr(if (unsigned) .unsigned_bit_int_too_big else .signed_bit_int_too_big, b.bitIntToken.?, complexStr);
                return Type.Invalid;
            }

            ty.specifier = if (b.complexToken != null) .ComplexBitInt else .BitInt;
            ty.data = .{
                .int = .{
                    .signedness = if (unsigned) .unsigned else .signed,
                    .bits = @intCast(bits),
                },
            };
        },

        Specifier.FP16 => ty.specifier = .FP16,
        Specifier.Float16 => ty.specifier = .Float16,
        Specifier.Float => ty.specifier = .Float,
        Specifier.Double => ty.specifier = .Double,
        Specifier.LongDouble => ty.specifier = .LongDouble,
        Specifier.ComplexFP16 => ty.specifier = .ComplexFP16,
        Specifier.ComplexFloat16 => ty.specifier = .ComplexFloat16,
        Specifier.Float80 => ty.specifier = .Float80,
        Specifier.Float128 => ty.specifier = .Float128,
        Specifier.ComplexFloat => ty.specifier = .ComplexFloat,
        Specifier.ComplexDouble => ty.specifier = .ComplexDouble,
        Specifier.ComplexLongDouble => ty.specifier = .ComplexLongDouble,
        Specifier.ComplexFloat80 => ty.specifier = .ComplexFloat80,
        Specifier.ComplexFloat128 => ty.specifier = .ComplexFloat128,
        Specifier.Complex => {
            try p.errToken(.plain_complex, p.tokenIdx - 1);
            ty.specifier = .ComplexDouble;
        },

        Specifier.Pointer => |data| {
            ty.specifier = .Pointer;
            ty.data = .{ .subType = data };
        },

        Specifier.UnspecifiedVariableLenArray,
        Specifier.DecayedUnspecifiedVariableLenArray,
        => |data| {
            ty.specifier = .UnspecifiedVariableLenArray;
            ty.data = .{ .subType = data };
            ty.decayed = (b.specifier == .DecayedUnspecifiedVariableLenArray);
        },

        Specifier.Func => |data| {
            ty.specifier = .Func;
            ty.data = .{ .func = data };
        },

        Specifier.VarArgsFunc => |data| {
            ty.specifier = .VarArgsFunc;
            ty.data = .{ .func = data };
        },

        Specifier.OldStyleFunc => |data| {
            ty.specifier = .OldStyleFunc;
            ty.data = .{ .func = data };
        },

        Specifier.Array, Specifier.DecayedArray => |data| {
            ty.specifier = .Array;
            ty.data = .{ .array = data };
            ty.decayed = (b.specifier == .DecayedArray);
        },

        Specifier.StaticArray, Specifier.DecayedStaticArray => |data| {
            ty.specifier = .StaticArray;
            ty.data = .{ .array = data };
            ty.decayed = (b.specifier == .DecayedStaticArray);
        },

        Specifier.IncompleteArray, Specifier.DecayedIncompleteArray => |data| {
            ty.specifier = .IncompleteArray;
            ty.data = .{ .array = data };
            ty.decayed = (b.specifier == .DecayedIncompleteArray);
        },

        Specifier.Vector => |data| {
            ty.specifier = .Vector;
            ty.data = .{ .array = data };
        },

        Specifier.VariableLenArray, Specifier.DecayedVariableLenArray => |data| {
            ty.specifier = .VariableLenArray;
            ty.data = .{ .expr = data };
            ty.decayed = (b.specifier == .DecayedVariableLenArray);
        },

        Specifier.Struct => |data| {
            ty.specifier = .Struct;
            ty.data = .{ .record = data };
        },

        Specifier.Union => |data| {
            ty.specifier = .Union;
            ty.data = .{ .record = data };
        },

        Specifier.Enum => |data| {
            ty.specifier = .Enum;
            ty.data = .{ .@"enum" = data };
        },

        Specifier.TypeofType, Specifier.DecayedTypeofType => |data| {
            ty.specifier = .TypeofType;
            ty.data = .{ .subType = data };
            ty.decayed = (b.specifier == .DecayedTypeofType);
        },

        Specifier.TypeofExpr, Specifier.DecayedTypeofExpr => |data| {
            ty.specifier = .TypeofExpr;
            ty.data = .{ .expr = data };
            ty.decayed = (b.specifier == .DecayedTypeofExpr);
        },

        Specifier.Attributed, Specifier.DecayedAttributed => |data| {
            ty.specifier = .Attributed;
            ty.data = .{ .attributed = data };
            ty.decayed = (b.specifier == .DecayedAttributed);
        },
    }

    if (!ty.isReal() and ty.isInt())
        if (b.complexToken) |tok| try p.errToken(.complex_int, tok);

    try b.qual.finish(p, &ty);
    return ty;
}

fn cannotCombine(b: @This(), p: *Parser, sourceToken: TokenIndex) !void {
    if (b.errorOnInvalid)
        return error.CannotCombine;

    const tyString = b.specifier.toString(p.comp.langOpts) orelse try p.typeStr(try b.finish(p));
    try p.errExtra(.cannot_combine_spec, sourceToken, .{ .str = tyString });

    if (b.typedef) |some|
        try p.errStr(.spec_from_typedef, some.token, try p.typeStr(some.type));
}

fn duplicateSpec(b: *@This(), p: *Parser, sourceToken: TokenIndex, spec: []const u8) !void {
    if (b.errorOnInvalid)
        return error.CannotCombine;
    if (p.comp.langOpts.emulate != .clang)
        return b.cannotCombine(p, sourceToken);
    try p.errStr(.duplicate_declspec, p.tokenIdx, spec);
}

pub fn combineFromTypeof(b: *@This(), p: *Parser, new: Type, sourceToken: TokenIndex) Compilation.Error!void {
    if (b.typeof != null) return p.errStr(.cannot_combine_spec, sourceToken, "typeof");
    if (b.specifier != .None) return p.errStr(.invalid_typeof, sourceToken, b.specifier.toString(p.comp.langOpts).?);

    const inner = switch (new.specifier) {
        .TypeofType => new.data.subType.*,
        .TypeofExpr => new.data.expr.ty,
        .NullPtrTy => new, // typeof(nullptr) is special-cased to be an unwrapped typeof-expr
        else => unreachable,
    };

    b.typeof = switch (inner.specifier) {
        .Attributed => inner.data.attributed.base,
        else => new,
    };
}

/// Try to combine type from typedef, returns true if successful.
pub fn combineTypedef(b: *@This(), p: *Parser, typedefType: Type, nameToken: TokenIndex) bool {
    b.errorOnInvalid = true;
    defer b.errorOnInvalid = false;

    const newSpec = fromType(typedefType);
    b.combineExtra(p, newSpec, 0) catch |err| switch (err) {
        error.FatalError => unreachable, // we do not add any diagnostics
        error.OutOfMemory => unreachable, // we do not add any diagnostics
        error.ParsingFailed => unreachable, // we do not add any diagnostics
        error.CannotCombine => return false,
    };
    b.typedef = .{ .token = nameToken, .type = typedefType };
    return true;
}

pub fn combine(b: *@This(), p: *Parser, new: Specifier, sourceToken: TokenIndex) !void {
    b.combineExtra(p, new, sourceToken) catch |er| switch (er) {
        error.CannotCombine => unreachable,
        else => |e| return e,
    };
}

fn combineExtra(b: *@This(), p: *Parser, new: Specifier, sourceToken: TokenIndex) !void {
    if (b.typeof != null) {
        if (b.errorOnInvalid)
            return error.CannotCombine;
        try p.errStr(.invalid_typeof, sourceToken, new.toString(p.comp.langOpts).?);
    }

    switch (new) {
        .Complex => b.complexToken = sourceToken,
        .BitInt => b.bitIntToken = sourceToken,
        .AutoType => b.autoTypeToken = sourceToken,
        else => {},
    }

    if (new == .Int128 and !Target.hasInt128(p.comp.target))
        try p.errStr(.type_not_supported_on_target, sourceToken, "__int128");

    switch (new) {
        .Signed => b.specifier = switch (b.specifier) {
            .None => .Signed,
            .Char => .SChar,
            .Short => .SShort,
            .ShortInt => .SShortInt,
            .Int => .SInt,
            .Long => .SLong,
            .LongInt => .SLongInt,
            .LongLong => .SLongLong,
            .LongLongInt => .SLongLongInt,
            .Int128 => .SInt128,

            .BitInt => |bits| .{ .SBitInt = bits },

            .Complex => .ComplexSigned,
            .ComplexChar => .ComplexSChar,
            .ComplexShort => .ComplexSShort,
            .ComplexShortInt => .ComplexSShortInt,
            .ComplexInt => .ComplexSInt,
            .ComplexLong => .ComplexSLong,
            .ComplexLongInt => .ComplexSLongInt,
            .ComplexLongLong => .ComplexSLongLong,
            .ComplexLongLongInt => .ComplexSLongLongInt,
            .ComplexInt128 => .ComplexSInt128,
            .ComplexBitInt => |bits| .{ .ComplexSBitInt = bits },

            .Signed,
            .SShort,
            .SShortInt,
            .SInt,
            .SLong,
            .SLongInt,
            .SLongLong,
            .SLongLongInt,
            .SInt128,
            .SBitInt,
            .ComplexSChar,
            .ComplexSigned,
            .ComplexSShort,
            .ComplexSShortInt,
            .ComplexSInt,
            .ComplexSLong,
            .ComplexSLongInt,
            .ComplexSLongLong,
            .ComplexSLongLongInt,
            .ComplexSInt128,
            .ComplexSBitInt,
            => return b.duplicateSpec(p, sourceToken, "signed"),
            else => return b.cannotCombine(p, sourceToken),
        },

        .Unsigned => b.specifier = switch (b.specifier) {
            .None => .Unsigned,
            .Char => .UChar,
            .Short => .UShort,
            .ShortInt => .UShortInt,
            .Int => .UInt,
            .Long => .ULong,
            .LongInt => .ULongInt,
            .LongLong => .ULongLong,
            .LongLongInt => .ULongLongInt,
            .Int128 => .UInt128,
            .BitInt => |bits| .{ .UBitInt = bits },
            .Complex => .ComplexUnsigned,
            .ComplexChar => .ComplexUChar,
            .ComplexShort => .ComplexUShort,
            .ComplexShortInt => .ComplexUShortInt,
            .ComplexInt => .ComplexUInt,
            .ComplexLong => .ComplexULong,
            .ComplexLongInt => .ComplexULongInt,
            .ComplexLongLong => .ComplexULongLong,
            .ComplexLongLongInt => .ComplexULongLongInt,
            .ComplexInt128 => .ComplexUInt128,
            .ComplexBitInt => |bits| .{ .ComplexUBitInt = bits },
            .Unsigned,
            .UShort,
            .UShortInt,
            .UInt,
            .ULong,
            .ULongInt,
            .ULongLong,
            .ULongLongInt,
            .UInt128,
            .UBitInt,
            .ComplexUChar,
            .ComplexUnsigned,
            .ComplexUShort,
            .ComplexUShortInt,
            .ComplexUInt,
            .ComplexULong,
            .ComplexULongInt,
            .ComplexULongLong,
            .ComplexULongLongInt,
            .ComplexUInt128,
            .ComplexUBitInt,
            => return b.duplicateSpec(p, sourceToken, "unsigned"),
            else => return b.cannotCombine(p, sourceToken),
        },

        .Char => b.specifier = switch (b.specifier) {
            .None => .Char,
            .Unsigned => .UChar,
            .Signed => .SChar,
            .Complex => .ComplexChar,
            .ComplexSigned => .ComplexSChar,
            .ComplexUnsigned => .ComplexUChar,
            else => return b.cannotCombine(p, sourceToken),
        },

        .Short => b.specifier = switch (b.specifier) {
            .None => .Short,
            .Unsigned => .UShort,
            .Signed => .SShort,
            .Int => .ShortInt,
            .SInt => .SShortInt,
            .ULong => .UShortInt,
            .Complex => .ComplexShort,
            .ComplexUnsigned => .ComplexUShort,
            .ComplexSigned => .ComplexSShort,

            else => return b.cannotCombine(p, sourceToken),
        },

        .Int => b.specifier = switch (b.specifier) {
            .None => .Int,
            .Signed => .SInt,
            .Unsigned => .UInt,
            .Short => .ShortInt,
            .SShort => .SShortInt,
            .UShort => .UShortInt,
            .Long => .LongInt,
            .SLong => .SLongInt,
            .ULong => .ULongInt,
            .LongLong => .LongLongInt,
            .SLongLong => .SLongLongInt,
            .ULongLong => .ULongLongInt,
            .Complex => .ComplexInt,
            .ComplexSigned => .ComplexSInt,
            .ComplexUnsigned => .ComplexUInt,
            .ComplexShort => .ComplexShortInt,
            .ComplexSShort => .ComplexSShortInt,
            .ComplexUShort => .ComplexUShortInt,
            .ComplexLong => .ComplexLongInt,
            .ComplexSLong => .ComplexSLongInt,
            .ComplexULong => .ComplexULongInt,
            .ComplexLongLong => .ComplexLongLongInt,
            .ComplexSLongLong => .ComplexSLongLongInt,
            .ComplexULongLong => .ComplexULongLongInt,
            else => return b.cannotCombine(p, sourceToken),
        },

        .Long => b.specifier = switch (b.specifier) {
            .None => .Long,
            .Long => .LongLong,
            .Unsigned => .ULong,
            .Signed => .Long,
            .Int => .LongInt,
            .SInt => .SLongInt,
            .ULong => .ULongLong,
            .Complex => .ComplexLong,
            .ComplexSigned => .ComplexSLong,
            .ComplexUnsigned => .ComplexULong,
            .ComplexLong => .ComplexLongLong,
            .ComplexSLong => .ComplexSLongLong,
            .ComplexULong => .ComplexULongLong,
            else => return b.cannotCombine(p, sourceToken),
        },

        .Int128 => b.specifier = switch (b.specifier) {
            .None => .Int128,
            .Unsigned => .UInt128,
            .Signed => .SInt128,
            .Complex => .ComplexInt128,
            .ComplexSigned => .ComplexSInt128,
            .ComplexUnsigned => .ComplexUInt128,
            else => return b.cannotCombine(p, sourceToken),
        },

        .BitInt => b.specifier = switch (b.specifier) {
            .None => .{ .BitInt = new.BitInt },
            .Unsigned => .{ .UBitInt = new.BitInt },
            .Signed => .{ .SBitInt = new.BitInt },
            .Complex => .{ .ComplexBitInt = new.BitInt },
            .ComplexSigned => .{ .ComplexSBitInt = new.BitInt },
            .ComplexUnsigned => .{ .ComplexUBitInt = new.BitInt },
            else => return b.cannotCombine(p, sourceToken),
        },

        .AutoType => b.specifier = switch (b.specifier) {
            .None => .AutoType,
            else => return b.cannotCombine(p, sourceToken),
        },

        .C23Auto => b.specifier = switch (b.specifier) {
            .None => .C23Auto,
            else => return b.cannotCombine(p, sourceToken),
        },

        .FP16 => b.specifier = switch (b.specifier) {
            .None => .FP16,
            .Complex => .ComplexFP16,
            else => return b.cannotCombine(p, sourceToken),
        },

        .Float16 => b.specifier = switch (b.specifier) {
            .None => .Float16,
            .Complex => .ComplexFloat16,
            else => return b.cannotCombine(p, sourceToken),
        },

        .Float => b.specifier = switch (b.specifier) {
            .None => .Float,
            .Complex => .ComplexFloat,
            else => return b.cannotCombine(p, sourceToken),
        },

        .Double => b.specifier = switch (b.specifier) {
            .None => .Double,
            .Long => .LongDouble,
            .Complex => .ComplexDouble,
            .ComplexLong => .ComplexLongDouble,

            else => return b.cannotCombine(p, sourceToken),
        },

        .Float80 => b.specifier = switch (b.specifier) {
            .None => .Float80,
            .Complex => .ComplexFloat80,
            else => return b.cannotCombine(p, sourceToken),
        },

        .Float128 => b.specifier = switch (b.specifier) {
            .None => .Float128,
            .Complex => .ComplexFloat128,
            else => return b.cannotCombine(p, sourceToken),
        },

        .Complex => b.specifier = switch (b.specifier) {
            .None => .Complex,
            .FP16 => .ComplexFP16,
            .Float16 => .ComplexFloat16,
            .Float => .ComplexFloat,
            .Double => .ComplexDouble,
            .LongDouble => .ComplexLongDouble,
            .Float80 => .ComplexFloat80,
            .Float128 => .ComplexFloat128,

            .Char => .ComplexChar,
            .SChar => .ComplexSChar,
            .UChar => .ComplexUChar,
            .Unsigned => .ComplexUnsigned,
            .Signed => .ComplexSigned,
            .Short => .ComplexShort,
            .SShort => .ComplexSShort,
            .UShort => .ComplexUShort,
            .ShortInt => .ComplexShortInt,
            .SShortInt => .ComplexSShortInt,
            .UShortInt => .ComplexUShortInt,
            .Int => .ComplexInt,
            .SInt => .ComplexSInt,
            .UInt => .ComplexUInt,
            .Long => .ComplexLong,
            .SLong => .ComplexSLong,
            .ULong => .ComplexULong,
            .LongInt => .ComplexLongInt,
            .SLongInt => .ComplexSLongInt,
            .ULongInt => .ComplexULongInt,
            .LongLong => .ComplexLongLong,
            .SLongLong => .ComplexSLongLong,
            .ULongLong => .ComplexULongLong,
            .LongLongInt => .ComplexLongLongInt,
            .SLongLongInt => .ComplexSLongLongInt,
            .ULongLongInt => .ComplexULongLongInt,
            .Int128 => .ComplexInt128,
            .SInt128 => .ComplexSInt128,
            .UInt128 => .ComplexUInt128,

            .BitInt => |bits| .{ .ComplexBitInt = bits },
            .SBitInt => |bits| .{ .ComplexSBitInt = bits },
            .UBitInt => |bits| .{ .ComplexUBitInt = bits },

            .Complex,
            .ComplexFP16,
            .ComplexFloat16,
            .ComplexFloat,
            .ComplexDouble,
            .ComplexLongDouble,
            .ComplexFloat80,
            .ComplexFloat128,
            .ComplexChar,
            .ComplexSChar,
            .ComplexUChar,
            .ComplexUnsigned,
            .ComplexSigned,
            .ComplexShort,
            .ComplexSShort,
            .ComplexUShort,
            .ComplexShortInt,
            .ComplexSShortInt,
            .ComplexUShortInt,
            .ComplexInt,
            .ComplexSInt,
            .ComplexUInt,
            .ComplexLong,
            .ComplexSLong,
            .ComplexULong,
            .ComplexLongInt,
            .ComplexSLongInt,
            .ComplexULongInt,
            .ComplexLongLong,
            .ComplexSLongLong,
            .ComplexULongLong,
            .ComplexLongLongInt,
            .ComplexSLongLongInt,
            .ComplexULongLongInt,
            .ComplexInt128,
            .ComplexSInt128,
            .ComplexUInt128,
            .ComplexBitInt,
            .ComplexSBitInt,
            .ComplexUBitInt,
            => return b.duplicateSpec(p, sourceToken, "_Complex"),
            else => return b.cannotCombine(p, sourceToken),
        },

        else => switch (b.specifier) {
            .None => b.specifier = new,
            else => return b.cannotCombine(p, sourceToken),
        },
    }
}

pub fn fromType(ty: Type) Specifier {
    return switch (ty.specifier) {
        .Void => Specifier.Void,
        .AutoType => Specifier.AutoType,
        .C23Auto => Specifier.C23Auto,
        .NullPtrTy => Specifier.NullPtrTy,
        .Bool => Specifier.Bool,
        .Char => Specifier.Char,
        .SChar => Specifier.SChar,
        .UChar => Specifier.UChar,
        .Short => Specifier.Short,
        .UShort => Specifier.UShort,
        .Int => Specifier.Int,
        .UInt => Specifier.UInt,
        .Long => Specifier.Long,
        .ULong => Specifier.ULong,
        .LongLong => Specifier.LongLong,
        .ULongLong => Specifier.ULongLong,
        .Int128 => Specifier.Int128,
        .UInt128 => Specifier.UInt128,

        .BitInt => if (ty.data.int.signedness == .unsigned)
            return .{ .UBitInt = ty.data.int.bits }
        else
            return .{ .BitInt = ty.data.int.bits },

        .ComplexChar => Specifier.ComplexChar,
        .ComplexSChar => Specifier.ComplexSChar,
        .ComplexUChar => Specifier.ComplexUChar,
        .ComplexShort => Specifier.ComplexShort,
        .ComplexUShort => Specifier.ComplexUShort,
        .ComplexInt => Specifier.ComplexInt,
        .ComplexUInt => Specifier.ComplexUInt,
        .ComplexLong => Specifier.ComplexLong,
        .ComplexULong => Specifier.ComplexULong,
        .ComplexLongLong => Specifier.ComplexLongLong,
        .ComplexULongLong => Specifier.ComplexULongLong,
        .ComplexInt128 => Specifier.ComplexInt128,
        .ComplexUInt128 => Specifier.ComplexUInt128,

        .ComplexBitInt => if (ty.data.int.signedness == .unsigned)
            return .{ .ComplexUBitInt = ty.data.int.bits }
        else
            return .{ .ComplexBitInt = ty.data.int.bits },

        .FP16 => Specifier.FP16,
        .Float16 => Specifier.Float16,
        .Float => Specifier.Float,
        .Double => Specifier.Double,
        .LongDouble => Specifier.LongDouble,
        .Float80 => Specifier.Float80,
        .Float128 => Specifier.Float128,

        .ComplexFP16 => Specifier.ComplexFP16,
        .ComplexFloat16 => Specifier.ComplexFloat16,
        .ComplexFloat => Specifier.ComplexFloat,
        .ComplexDouble => Specifier.ComplexDouble,
        .ComplexLongDouble => Specifier.ComplexLongDouble,
        .ComplexFloat80 => Specifier.ComplexFloat80,
        .ComplexFloat128 => Specifier.ComplexFloat128,

        .Pointer => .{ .Pointer = ty.data.subType },

        .UnspecifiedVariableLenArray => if (ty.isDecayed())
            .{ .DecayedUnspecifiedVariableLenArray = ty.data.subType }
        else
            .{ .UnspecifiedVariableLenArray = ty.data.subType },

        .Func => .{ .Func = ty.data.func },
        .VarArgsFunc => .{ .VarArgsFunc = ty.data.func },
        .OldStyleFunc => .{ .OldStyleFunc = ty.data.func },

        .Array => if (ty.isDecayed()) .{ .DecayedArray = ty.data.array } else .{ .Array = ty.data.array },
        .StaticArray => if (ty.isDecayed())
            .{ .DecayedStaticArray = ty.data.array }
        else
            .{ .StaticArray = ty.data.array },

        .IncompleteArray => if (ty.isDecayed())
            .{ .DecayedIncompleteArray = ty.data.array }
        else
            .{ .IncompleteArray = ty.data.array },

        .Vector => .{ .Vector = ty.data.array },
        .VariableLenArray => if (ty.isDecayed())
            .{ .DecayedVariableLenArray = ty.data.expr }
        else
            .{ .VariableLenArray = ty.data.expr },

        .Struct => .{ .Struct = ty.data.record },
        .Union => .{ .Union = ty.data.record },
        .Enum => .{ .Enum = ty.data.@"enum" },

        .TypeofType => if (ty.isDecayed())
            .{ .DecayedTypeofType = ty.data.subType }
        else
            .{ .TypeofType = ty.data.subType },

        .TypeofExpr => if (ty.isDecayed())
            .{ .DecayedTypeofExpr = ty.data.expr }
        else
            .{ .TypeofExpr = ty.data.expr },

        .Attributed => if (ty.isDecayed())
            .{ .DecayedAttributed = ty.data.attributed }
        else
            .{ .Attributed = ty.data.attributed },

        else => unreachable,
    };
}
