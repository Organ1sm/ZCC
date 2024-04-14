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
typeof: ?Type = null,
/// When true an error is returned instead of adding a diagnostic message.
/// Used for trying to combine typedef types.
errorOnInvalid: bool = false,

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
    Int128,
    SInt128,
    UInt128,

    FP16,
    Float,
    Double,
    LongDouble,
    Float80,
    Float128,
    Complex,
    ComplexFP16,
    ComplexLong,
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
            Specifier.Int128 => "__int128",
            Specifier.UInt128 => "unsigned __int128",
            Specifier.SInt128 => "signed __int128",

            Specifier.FP16 => "__fp16",
            Specifier.Float => "float",
            Specifier.Double => "double",
            Specifier.LongDouble => "long double",
            Specifier.Float80 => "__float80",
            Specifier.Float128 => "__float128",
            Specifier.Complex => "_Complex",
            Specifier.ComplexFP16 => "__Complex __fp16",
            Specifier.ComplexFloat => "_Complex float",
            Specifier.ComplexLong => "_Complex long",
            Specifier.ComplexDouble => "_Complex double",
            Specifier.ComplexLongDouble => "_Complex long double",
            Specifier.ComplexFloat80 => "_Complex __float80",
            Specifier.ComplexFloat128 => "_Complex __float128",

            Specifier.Attributed => |attr| TypeBuilder.fromType(attr.base).toString(),

            else => null,
        };
    }
};

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
                try p.err(.missing_type_specifier);
            }
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
        Specifier.Int128, Specifier.SInt128 => ty.specifier = .Int128,
        Specifier.UInt128 => ty.specifier = .UInt128,

        Specifier.FP16 => ty.specifier = .FP16,
        Specifier.Float => ty.specifier = .Float,
        Specifier.Double => ty.specifier = .Double,
        Specifier.LongDouble => ty.specifier = .LongDouble,
        Specifier.ComplexFP16 => ty.specifier = .ComplexFP16,
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
        Specifier.ComplexLong => {
            try p.errExtra(.type_is_invalid, p.tokenIdx, .{ .str = b.specifier.toString().? });
            return error.ParsingFailed;
        },

        Specifier.Pointer => |data| {
            ty.specifier = .Pointer;
            ty.data = .{ .subType = data };
        },

        Specifier.UnspecifiedVariableLenArray => |data| {
            ty.specifier = .UnspecifiedVariableLenArray;
            ty.data = .{ .subType = data };
        },

        Specifier.DecayedUnspecifiedVariableLenArray => |data| {
            ty.specifier = .DecayedUnspecifiedVariableLenArray;
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

        Specifier.DecayedArray => |data| {
            ty.specifier = .DecayedArray;
            ty.data = .{ .array = data };
        },

        Specifier.StaticArray => |data| {
            ty.specifier = .StaticArray;
            ty.data = .{ .array = data };
        },

        Specifier.DecayedStaticArray => |data| {
            ty.specifier = .DecayedStaticArray;
            ty.data = .{ .array = data };
        },

        Specifier.IncompleteArray => |data| {
            ty.specifier = .IncompleteArray;
            ty.data = .{ .array = data };
        },

        Specifier.Vector => |data| {
            ty.specifier = .Vector;
            ty.data = .{ .array = data };
        },

        Specifier.DecayedIncompleteArray => |data| {
            ty.specifier = .DecayedIncompleteArray;
            ty.data = .{ .array = data };
        },

        Specifier.VariableLenArray => |data| {
            ty.specifier = .VariableLenArray;
            ty.data = .{ .expr = data };
        },

        Specifier.DecayedVariableLenArray => |data| {
            ty.specifier = .DecayedVariableLenArray;
            ty.data = .{ .expr = data };
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

        Specifier.TypeofType => |data| {
            ty.specifier = .TypeofType;
            ty.data = .{ .subType = data };
        },

        Specifier.DecayedTypeofType => |data| {
            ty.specifier = .DecayedTypeofType;
            ty.data = .{ .subType = data };
        },

        Specifier.TypeofExpr => |data| {
            ty.specifier = .TypeofExpr;
            ty.data = .{ .expr = data };
        },

        Specifier.DecayedTypeofExpr => |data| {
            ty.specifier = .DecayedTypeofExpr;
            ty.data = .{ .expr = data };
        },

        Specifier.Attributed => |data| {
            ty.specifier = .Attributed;
            ty.data = .{ .attributed = data };
        },
    }

    try b.qual.finish(p, &ty);

    return ty;
}

fn cannotCombine(b: @This(), p: *Parser, sourceToken: TokenIndex) !void {
    if (b.errorOnInvalid)
        return error.CannotCombine;

    const tyString = b.specifier.toString() orelse try p.typeStr(try b.finish(p));
    try p.errExtra(
        .cannot_combine_spec,
        sourceToken,
        .{ .str = tyString },
    );

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
    if (b.specifier != .None) return p.errStr(.invalid_typeof, sourceToken, b.specifier.toString().?);

    const inner = switch (new.specifier) {
        .TypeofType => new.data.subType.*,
        .TypeofExpr => new.data.expr.ty,
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
        try p.errStr(.invalid_typeof, sourceToken, new.toString().?);
    }

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

            .Signed,
            .SShort,
            .SShortInt,
            .SInt,
            .SLong,
            .SLongInt,
            .SLongLong,
            .SLongLongInt,
            .SInt128,
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
            .Unsigned,
            .UShort,
            .UShortInt,
            .UInt,
            .ULong,
            .ULongInt,
            .ULongLong, //
            .ULongLongInt,
            .UInt128,
            => return b.duplicateSpec(p, sourceToken, "unsigned"),
            else => return b.cannotCombine(p, sourceToken),
        },

        .Char => b.specifier = switch (b.specifier) {
            .None => .Char,
            .Unsigned => .UChar,
            .Signed => .SChar,
            else => return b.cannotCombine(p, sourceToken),
        },

        .Short => b.specifier = switch (b.specifier) {
            .None => .Short,
            .Unsigned => .UShort,
            .Signed => .SShort,
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
            else => return b.cannotCombine(p, sourceToken),
        },

        .Int128 => b.specifier = switch (b.specifier) {
            .None => .Int128,
            .Unsigned => .UInt128,
            .Signed => .SInt128,
            else => return b.cannotCombine(p, sourceToken),
        },

        .FP16 => b.specifier = switch (b.specifier) {
            .None => .FP16,
            .Complex => .ComplexFP16,
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
            .Long => .ComplexLong,
            .FP16 => .ComplexFP16,
            .Float => .ComplexFloat,
            .Double => .ComplexDouble,
            .LongDouble => .ComplexLongDouble,
            .Float80 => .ComplexFloat80,
            .Float128 => .ComplexFloat128,

            .Complex,
            .ComplexFP16,
            .ComplexLong,
            .ComplexFloat,
            .ComplexDouble,
            .ComplexLongDouble,
            .ComplexFloat80,
            .ComplexFloat128,
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
        .FP16 => Specifier.FP16,
        .Float => Specifier.Float,
        .Double => Specifier.Double,
        .LongDouble => Specifier.LongDouble,
        .Float80 => Specifier.Float80,
        .Float128 => Specifier.Float128,

        .ComplexFP16 => Specifier.ComplexFP16,
        .ComplexFloat => Specifier.ComplexFloat,
        .ComplexDouble => Specifier.ComplexDouble,
        .ComplexLongDouble => Specifier.ComplexLongDouble,
        .ComplexFloat80 => Specifier.ComplexFloat80,
        .ComplexFloat128 => Specifier.ComplexFloat128,

        .Pointer => .{ .Pointer = ty.data.subType },
        .UnspecifiedVariableLenArray => .{ .UnspecifiedVariableLenArray = ty.data.subType },
        .DecayedUnspecifiedVariableLenArray => .{ .DecayedUnspecifiedVariableLenArray = ty.data.subType },
        .Func => .{ .Func = ty.data.func },
        .VarArgsFunc => .{ .VarArgsFunc = ty.data.func },
        .OldStyleFunc => .{ .OldStyleFunc = ty.data.func },
        .Array => .{ .Array = ty.data.array },
        .DecayedArray => .{ .DecayedArray = ty.data.array },
        .StaticArray => .{ .StaticArray = ty.data.array },
        .DecayedStaticArray => .{ .DecayedStaticArray = ty.data.array },
        .IncompleteArray => .{ .IncompleteArray = ty.data.array },
        .DecayedIncompleteArray => .{ .DecayedIncompleteArray = ty.data.array },
        .Vector => .{ .Vector = ty.data.array },
        .VariableLenArray => .{ .VariableLenArray = ty.data.expr },
        .DecayedVariableLenArray => .{ .DecayedVariableLenArray = ty.data.expr },
        .Struct => .{ .Struct = ty.data.record },
        .Union => .{ .Union = ty.data.record },
        .Enum => .{ .Enum = ty.data.@"enum" },

        .TypeofType => .{ .TypeofType = ty.data.subType },
        .DecayedTypeofType => .{ .DecayedTypeofType = ty.data.subType },
        .TypeofExpr => .{ .TypeofExpr = ty.data.expr },
        .DecayedTypeofExpr => .{ .DecayedTypeofExpr = ty.data.expr },

        .Attributed => .{ .Attributed = ty.data.attributed },

        else => unreachable,
    };
}
