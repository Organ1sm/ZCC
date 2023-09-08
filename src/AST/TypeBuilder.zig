const Type = @import("Type.zig");
const TokenIndex = @import("AST.zig").TokenIndex;
const NodeIndex = @import("AST.zig").NodeIndex;
const Parser = @import("../Parser/Parser.zig");
const Compilation = @import("../Basic/Compilation.zig");
const Qualifiers = @import("Type.zig").Qualifiers;

const TypeBuilder = @This();

typedef: ?struct {
    token: TokenIndex,
    type: Type,
} = null,

specifier: @This().Specifier = .None,
qual: Qualifiers.Builder = .{},

pub const Specifier = union(enum) {
    None,
    Void,
    Bool,
    Char,
    SChar,
    UChar,

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

    Float,
    Double,
    LongDouble,
    Complex,
    ComplexLong,
    ComplexFloat,
    ComplexDouble,
    ComplexLongDouble,

    Pointer: *Type,
    UnspecifiedVariableLenArray: *Type,
    Func: *Type.Function,
    VarArgsFunc: *Type.Function,
    OldStyleFunc: *Type.Function,
    Array: *Type.Array,
    StaticArray: *Type.Array,
    IncompleteArray: *Type.Array,
    VariableLenArray: *Type.VLA,

    Struct: *Type.Record,
    Union: *Type.Record,
    Enum: *Type.Enum,

    pub fn toString(spec: Specifier) ?[]const u8 {
        return switch (spec) {
            Specifier.None => unreachable,

            Specifier.Void => "void",
            Specifier.Bool => "_Bool",

            Specifier.Char => "char",
            Specifier.SChar => "signed char",
            Specifier.UChar => "unsigned char",

            Specifier.Unsigned => "unsigned",
            Specifier.Signed => "signed",

            Specifier.Short => "short",
            Specifier.UShort => "unsigned short",
            Specifier.SShort => "signed short",

            Specifier.ShortInt => "short int",
            Specifier.SShortInt => "signed short int",
            Specifier.UShortInt => "unsigned short int",
            Specifier.Int => "int",
            Specifier.SInt => "signed int",
            Specifier.UInt => "unsigned int",

            Specifier.Long => "long",
            Specifier.SLong => "signed long",
            Specifier.ULong => "unsigned long",
            Specifier.LongInt => "long int",
            Specifier.SLongInt => "signed long int",
            Specifier.ULongInt => "unsigned long int",
            Specifier.LongLong => "long long",
            Specifier.SLongLong => "signed long long",
            Specifier.ULongLong => "unsigned long long",
            Specifier.LongLongInt => "long long int",
            Specifier.SLongLongInt => "signed long long int",
            Specifier.ULongLongInt => "unsigned long long int",

            Specifier.Float => "float",
            Specifier.Double => "double",
            Specifier.LongDouble => "long double",
            Specifier.Complex => "_Complex",
            Specifier.ComplexFloat => "_Complex float",
            Specifier.ComplexLong => "_Complex long",
            Specifier.ComplexDouble => "_Complex double",
            Specifier.ComplexLongDouble => "_Complex long double",

            else => null,
        };
    }
};

pub fn finish(b: @This(), p: *Parser, ty: *Type) Parser.Error!void {
    switch (b.specifier) {
        Specifier.None => {
            ty.specifier = .Int;
            try p.err(.missing_type_specifier);
        },

        Specifier.Void => ty.specifier = .Void,
        Specifier.Bool => ty.specifier = .Bool,
        Specifier.Char => ty.specifier = .Char,
        Specifier.SChar => ty.specifier = .SChar,
        Specifier.UChar => ty.specifier = .UChar,

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

        Specifier.Float => ty.specifier = .Float,
        Specifier.Double => ty.specifier = .Double,
        Specifier.LongDouble => ty.specifier = .LongDouble,
        Specifier.ComplexFloat => ty.specifier = .ComplexFloat,
        Specifier.ComplexDouble => ty.specifier = .ComplexDouble,
        Specifier.ComplexLongDouble => ty.specifier = .ComplexLongDouble,
        Specifier.Complex, Specifier.ComplexLong => {
            try p.errExtra(.type_is_invalid, p.index, .{ .str = b.specifier.toString().? });
            return error.ParsingFailed;
        },

        Specifier.Pointer => |data| {
            ty.specifier = .Pointer;
            ty.data = .{ .subType = data };
        },

        .UnspecifiedVariableLenArray => |data| {
            ty.specifier = .UnspecifiedVariableLenArray;
            ty.data = .{ .subType = data };
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

        Specifier.Array => |data| {
            ty.specifier = .Array;
            ty.data = .{ .array = data };
        },

        Specifier.StaticArray => |data| {
            ty.specifier = .StaticArray;
            ty.data = .{ .array = data };
        },

        Specifier.IncompleteArray => |data| {
            ty.specifier = .IncompleteArray;
            ty.data = .{ .array = data };
        },

        Specifier.VariableLenArray => |data| {
            ty.specifier = .VariableLenArray;
            ty.data = .{ .vla = data };
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
    }

    try b.qual.finish(p, ty);
}

pub fn cannotCombine(b: @This(), p: *Parser, sourceToken: TokenIndex) Compilation.Error!void {
    var prevType: Type = .{ .specifier = undefined };
    b.finish(p, &prevType) catch unreachable;
    try p.errExtra(
        .cannot_combine_spec,
        sourceToken,
        .{ .str = try p.typeStr(prevType) },
    );

    if (b.typedef) |some|
        try p.errStr(.spec_from_typedef, some.token, try p.typeStr(some.type));
}

pub fn combine(b: *@This(), p: *Parser, new: Specifier, sourceToken: TokenIndex) Compilation.Error!void {
    switch (new) {
        Specifier.Signed => b.specifier = switch (b.specifier) {
            Specifier.None => Specifier.Signed,
            Specifier.Char => Specifier.SChar,
            Specifier.Short => Specifier.SShort,
            Specifier.ShortInt => Specifier.SShortInt,
            Specifier.Int => Specifier.SInt,
            Specifier.Long => Specifier.SLong,
            Specifier.LongInt => Specifier.SLongInt,
            Specifier.LongLong => Specifier.SLongLong,
            Specifier.LongLongInt => Specifier.SLongLongInt,

            Specifier.SShort,
            Specifier.SShortInt,
            Specifier.SInt,
            Specifier.SLong,
            Specifier.SLongInt,
            Specifier.SLongLong,
            Specifier.SLongLongInt,
            => return p.errStr(.duplicate_declspec, p.index, "signed"),

            else => return b.cannotCombine(p, sourceToken),
        },

        Specifier.Unsigned => b.specifier = switch (b.specifier) {
            Specifier.None => Specifier.Unsigned,
            Specifier.Char => Specifier.UChar,
            Specifier.Short => Specifier.UShort,
            Specifier.ShortInt => Specifier.UShortInt,
            Specifier.Int => Specifier.UInt,
            Specifier.Long => Specifier.ULong,
            Specifier.LongInt => Specifier.ULongInt,
            Specifier.LongLong => Specifier.ULongLong,
            Specifier.LongLongInt => Specifier.ULongLongInt,

            Specifier.UShort,
            Specifier.UShortInt,
            Specifier.UInt,
            Specifier.ULong,
            Specifier.ULongInt,
            Specifier.ULongLong, //
            Specifier.ULongLongInt,
            => return p.errStr(.duplicate_declspec, p.index, "unsigned"),

            else => return b.cannotCombine(p, sourceToken),
        },

        Specifier.Char => b.specifier = switch (b.specifier) {
            Specifier.None => Specifier.Char,
            Specifier.Unsigned => Specifier.UChar,
            Specifier.Signed => Specifier.SChar,
            Specifier.Char, Specifier.SChar, Specifier.UChar => return p.errStr(.duplicate_declspec, p.index, "char"),

            else => return b.cannotCombine(p, sourceToken),
        },

        Specifier.Int => b.specifier = switch (b.specifier) {
            Specifier.None => Specifier.Int,
            Specifier.Signed => Specifier.SInt,
            Specifier.Unsigned => Specifier.UInt,
            Specifier.Short => Specifier.ShortInt,
            Specifier.SShort => Specifier.SShortInt,
            Specifier.UShort => Specifier.UShortInt,
            Specifier.Long => Specifier.LongInt,
            Specifier.SLong => Specifier.SLongInt,
            Specifier.ULong => Specifier.ULongInt,
            Specifier.LongLong => Specifier.LongLongInt,
            Specifier.SLongLong => Specifier.SLongLongInt,
            Specifier.ULongLong => Specifier.ULongLongInt,

            Specifier.Int,
            Specifier.SInt,
            Specifier.UInt,
            Specifier.ShortInt,
            Specifier.SShortInt,
            Specifier.UShortInt,
            Specifier.LongInt,
            Specifier.SLongInt,
            Specifier.ULongInt,
            Specifier.LongLongInt,
            Specifier.SLongLongInt,
            Specifier.ULongLongInt,
            => return p.errStr(.duplicate_declspec, p.index, "int"),

            else => return b.cannotCombine(p, sourceToken),
        },

        Specifier.Long => b.specifier = switch (b.specifier) {
            Specifier.None => Specifier.Long,
            Specifier.Long => Specifier.LongLong,
            Specifier.Unsigned => Specifier.ULong,
            Specifier.Signed => Specifier.Long,
            Specifier.Int => Specifier.LongInt,
            Specifier.SInt => Specifier.SLongInt,
            Specifier.ULong => Specifier.ULongLong,
            Specifier.LongLong, Specifier.ULongLong => return p.errStr(.duplicate_declspec, p.index, "long"),

            else => return b.cannotCombine(p, sourceToken),
        },

        Specifier.Float => b.specifier = switch (b.specifier) {
            Specifier.None => Specifier.Float,
            Specifier.Complex => Specifier.ComplexFloat,

            Specifier.ComplexFloat, Specifier.Float => return p.errStr(.duplicate_declspec, p.index, "float"),
            else => return b.cannotCombine(p, sourceToken),
        },

        Specifier.Double => b.specifier = switch (b.specifier) {
            Specifier.None => Specifier.Double,
            Specifier.Long => Specifier.LongDouble,
            Specifier.Complex => Specifier.ComplexDouble,
            Specifier.ComplexLong => Specifier.ComplexLongDouble,

            Specifier.LongDouble,
            Specifier.ComplexLongDouble,
            Specifier.ComplexDouble,
            Specifier.Double,
            => return p.errStr(.duplicate_declspec, p.index, "double"),

            else => return b.cannotCombine(p, sourceToken),
        },

        Specifier.Complex => b.specifier = switch (b.specifier) {
            Specifier.None => Specifier.Complex,
            Specifier.Long => Specifier.ComplexLong,
            Specifier.Float => Specifier.ComplexFloat,
            Specifier.Double => Specifier.ComplexDouble,
            Specifier.LongDouble => Specifier.ComplexLongDouble,

            Specifier.Complex,
            Specifier.ComplexLong,
            Specifier.ComplexFloat,
            Specifier.ComplexDouble, //
            Specifier.ComplexLongDouble,
            => return p.errStr(.duplicate_declspec, p.index, "_Complex"),

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
        .Float => Specifier.Float,
        .Double => Specifier.Double,
        .LongDouble => Specifier.LongDouble,
        .ComplexFloat => Specifier.ComplexFloat,
        .ComplexDouble => Specifier.ComplexDouble,
        .ComplexLongDouble => Specifier.ComplexLongDouble,

        .Pointer => .{ .Pointer = ty.data.subType },
        .UnspecifiedVariableLenArray => .{ .UnspecifiedVariableLenArray = ty.data.subType },
        .Func => .{ .Func = ty.data.func },
        .VarArgsFunc => .{ .VarArgsFunc = ty.data.func },
        .OldStyleFunc => .{ .OldStyleFunc = ty.data.func },
        .Array => .{ .Array = ty.data.array },
        .StaticArray => .{ .StaticArray = ty.data.array },
        .IncompleteArray => .{ .IncompleteArray = ty.data.array },
        .VariableLenArray => .{ .VariableLenArray = ty.data.vla },
        .Struct => .{ .Struct = ty.data.record },
        .Union => .{ .Union = ty.data.record },
        .Enum => .{ .Enum = ty.data.@"enum" },
    };
}
