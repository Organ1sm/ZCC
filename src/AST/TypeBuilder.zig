const Type = @import("Type.zig");
const TokenIndex = @import("AST.zig").TokenIndex;
const NodeIndex = @import("AST.zig").NodeIndex;
const Parser = @import("../Parser/Parser.zig");

const TypeBuilder = @This();

pub const Builder = struct {
    typedef: ?struct {
        token: TokenIndex,
        spec: []const u8,
    } = null,

    kind: Kind = .None,

    pub const Kind = union(enum) {
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
        Atomic: *Type,
        Func: *Type.Function,
        VarArgsFunc: *Type.Function,
        OldStyleFunc: *Type.Function,
        Array: *Type.Array,
        StaticArray: *Type.Array,

        Struct: NodeIndex,
        Union: NodeIndex,
        Enum: NodeIndex,

        pub fn toString(spec: Kind) []const u8 {
            return switch (spec) {
                Kind.None => unreachable,

                Kind.Void => "void",
                Kind.Bool => "_Bool",

                Kind.Char => "char",
                Kind.SChar => "signed char",
                Kind.UChar => "unsigned char",

                Kind.Unsigned => "unsigned",
                Kind.Signed => "signed",

                Kind.Short => "short",
                Kind.UShort => "unsigned short",
                Kind.SShort => "signed short",

                Kind.ShortInt => "short int",
                Kind.SShortInt => "signed short int",
                Kind.UShortInt => "unsigned short int",
                Kind.Int => "int",
                Kind.SInt => "signed int",
                Kind.UInt => "unsigned int",

                Kind.Long => "long",
                Kind.SLong => "signed long",
                Kind.ULong => "unsigned long",
                Kind.LongInt => "long int",
                Kind.SLongInt => "signed long int",
                Kind.ULongInt => "unsigned long int",
                Kind.LongLong => "long long",
                Kind.SLongLong => "signed long long",
                Kind.ULongLong => "unsigned long long",
                Kind.LongLongInt => "long long int",
                Kind.SLongLongInt => "signed long long int",
                Kind.ULongLongInt => "unsigned long long int",

                Kind.Float => "float",
                Kind.Double => "double",
                Kind.LongDouble => "long double",
                Kind.Complex => "_Complex",
                Kind.ComplexFloat => "_Complex float",
                Kind.ComplexLong => "_Complex long",
                Kind.ComplexDouble => "_Complex double",
                Kind.ComplexLongDouble => "_Complex long double",

                Kind.Pointer => "pointer",
                Kind.Atomic => "atomic",
                Kind.Func, Kind.VarArgsFunc, Kind.OldStyleFunc => "function",
                Kind.Array, Kind.StaticArray => "array",
                Kind.Struct => "struct",
                Kind.Union => "union",
                Kind.Enum => "enum",
            };
        }
    };

    pub fn finish(spec: Builder, p: *Parser, ty: *Type) Parser.Error!void {
        ty.specifier = switch (spec.kind) {
            Kind.None => {
                ty.specifier = .Int;
                return p.err(.missing_type_specifier);
            },

            Kind.Void => .Void,
            Kind.Bool => .Bool,
            Kind.Char => .Char,
            Kind.SChar => .SChar,
            Kind.UChar => .UChar,

            Kind.Unsigned => .UInt,
            Kind.Signed => .Int,

            Kind.ShortInt, Kind.SShortInt, Kind.Short, Kind.SShort => .Short,
            Kind.UShort, Kind.UShortInt => .UShort,

            Kind.Int, .SInt => .Int,
            Kind.UInt => .UInt,

            Kind.Long, Kind.SLong, Kind.LongInt, Kind.SLongInt => .Long,
            Kind.ULong, Kind.ULongInt => .ULong,

            Kind.LongLong, Kind.SLongLong, Kind.LongLongInt, Kind.SLongLongInt => .LongLong,
            Kind.ULongLong, Kind.ULongLongInt => .ULongLong,

            Kind.Float => .Float,
            Kind.Double => .Double,
            Kind.LongDouble => .LongDouble,
            Kind.ComplexFloat => .ComplexFloat,
            Kind.ComplexDouble => .ComplexDouble,
            Kind.ComplexLongDouble => .ComplexLongDouble,
            Kind.Complex, Kind.ComplexLong => {
                const token = p.getCurrToken();
                try p.pp.compilation.diag.add(.{
                    .tag = .type_is_invalid,
                    .sourceId = token.source,
                    .locStart = token.loc.start,
                    .extra = .{ .str = spec.kind.toString() },
                });

                return error.ParsingFailed;
            },

            Kind.Atomic => |data| {
                ty.specifier = .Atomic;
                ty.data = .{ .subType = data };
                return;
            },

            Kind.Pointer => |data| {
                ty.specifier = .Pointer;
                ty.data = .{ .subType = data };
                return;
            },

            Kind.Func => |data| {
                ty.specifier = .Func;
                ty.data = .{ .func = data };
                return;
            },

            Kind.VarArgsFunc => |data| {
                ty.specifier = .VarArgsFunc;
                ty.data = .{ .func = data };
                return;
            },

            Kind.OldStyleFunc => |data| {
                ty.specifier = .OldStyleFunc;
                ty.data = .{ .func = data };
                return;
            },

            Kind.Array => |data| {
                ty.specifier = .Array;
                ty.data = .{ .array = data };
                return;
            },

            Kind.StaticArray => |data| {
                ty.specifier = .StaticArray;
                ty.data = .{ .array = data };
                return;
            },

            Kind.Struct => |data| {
                ty.specifier = .Struct;
                ty.data = .{ .node = data };
                return;
            },

            Kind.Union => |data| {
                ty.specifier = .Union;
                ty.data = .{ .node = data };
                return;
            },

            Kind.Enum => |data| {
                ty.specifier = .Enum;
                ty.data = .{ .node = data };
                return;
            },
        };
    }

    pub fn cannotCombine(spec: Builder, p: *Parser) Parser.Error {
        const token = p.getCurrToken();

        try p.pp.compilation.diag.add(.{
            .tag = .cannot_combine_spec,
            .sourceId = token.source,
            .locStart = token.loc.start,
            .extra = .{ .str = spec.kind.toString() },
        });

        if (spec.typedef) |some|
            try p.errStr(.spec_from_typedef, some.token, some.spec);

        return error.ParsingFailed;
    }

    pub fn combine(spec: *Builder, p: *Parser, new: Kind) Parser.Error!void {
        switch (new) {
            Kind.Void,
            Kind.Bool,
            Kind.Enum,
            Kind.Struct,
            Kind.Union,
            Kind.Pointer,
            Kind.Array,
            Kind.StaticArray, //
            Kind.Func,
            Kind.OldStyleFunc,
            Kind.VarArgsFunc,
            => switch (spec.kind) {
                .None => spec.kind = new,
                else => return spec.cannotCombine(p),
            },

            Kind.Atomic => return p.todo("atomic type"),
            Kind.Signed => spec.kind = switch (spec.kind) {
                Kind.None => Kind.Signed,
                Kind.Char => Kind.SChar,
                Kind.Short => Kind.SShort,
                Kind.ShortInt => Kind.SShortInt,
                Kind.Int => Kind.SInt,
                Kind.Long => Kind.SLong,
                Kind.LongInt => Kind.SLongInt,
                Kind.LongLong => Kind.SLongLong,
                Kind.LongLongInt => Kind.SLongLongInt,

                Kind.SShort,
                Kind.SShortInt,
                Kind.SInt,
                Kind.SLong,
                Kind.SLongInt,
                Kind.SLongLong,
                Kind.SLongLongInt,
                => return p.errStr(.duplicate_declspec, p.index, "signed"),

                else => return spec.cannotCombine(p),
            },

            Kind.Unsigned => spec.kind = switch (spec.kind) {
                Kind.None => Kind.Unsigned,
                Kind.Char => Kind.UChar,
                Kind.Short => Kind.UShort,
                Kind.ShortInt => Kind.UShortInt,
                Kind.Int => Kind.UInt,
                Kind.Long => Kind.ULong,
                Kind.LongInt => Kind.ULongInt,
                Kind.LongLong => Kind.ULongLong,
                Kind.LongLongInt => Kind.ULongLongInt,

                Kind.UShort,
                Kind.UShortInt,
                Kind.UInt,
                Kind.ULong,
                Kind.ULongInt,
                Kind.ULongLong, //
                Kind.ULongLongInt,
                => return p.errStr(.duplicate_declspec, p.index, "unsigned"),

                else => return spec.cannotCombine(p),
            },

            Kind.Char => spec.kind = switch (spec.kind) {
                Kind.None => Kind.Char,
                Kind.Unsigned => Kind.UChar,
                Kind.Signed => Kind.SChar,
                Kind.Char, Kind.SChar, Kind.UChar => return p.errStr(.duplicate_declspec, p.index, "char"),

                else => return spec.cannotCombine(p),
            },

            Kind.Int => spec.kind = switch (spec.kind) {
                Kind.None => Kind.Int,
                Kind.Signed => Kind.SInt,
                Kind.Unsigned => Kind.UInt,
                Kind.Short => Kind.ShortInt,
                Kind.SShort => Kind.SShortInt,
                Kind.UShort => Kind.UShortInt,
                Kind.Long => Kind.LongInt,
                Kind.SLong => Kind.SLongInt,
                Kind.ULong => Kind.ULongInt,
                Kind.LongLong => Kind.LongLongInt,
                Kind.SLongLong => Kind.SLongLongInt,
                Kind.ULongLong => Kind.ULongLongInt,

                Kind.Int,
                Kind.SInt,
                Kind.UInt,
                Kind.ShortInt,
                Kind.SShortInt,
                Kind.UShortInt,
                Kind.LongInt,
                Kind.SLongInt,
                Kind.ULongInt,
                Kind.LongLongInt,
                Kind.SLongLongInt,
                Kind.ULongLongInt,
                => return p.errStr(.duplicate_declspec, p.index, "int"),

                else => return spec.cannotCombine(p),
            },

            Kind.Long => spec.kind = switch (spec.kind) {
                Kind.None => Kind.Long,
                Kind.Long => Kind.LongLong,
                Kind.Unsigned => Kind.ULong,
                Kind.Signed => Kind.Long,
                Kind.Int => Kind.LongInt,
                Kind.SInt => Kind.SLongInt,
                Kind.ULong => Kind.ULongLong,
                Kind.LongLong, Kind.ULongLong => return p.errStr(.duplicate_declspec, p.index, "long"),

                else => return spec.cannotCombine(p),
            },

            Kind.Float => spec.kind = switch (spec.kind) {
                Kind.None => Kind.Float,
                Kind.Complex => Kind.ComplexFloat,

                Kind.ComplexFloat, Kind.Float => return p.errStr(.duplicate_declspec, p.index, "float"),
                else => return spec.cannotCombine(p),
            },

            Kind.Double => spec.kind = switch (spec.kind) {
                Kind.None => Kind.Double,
                Kind.Long => Kind.LongDouble,
                Kind.Complex => Kind.ComplexDouble,
                Kind.ComplexLong => Kind.ComplexLongDouble,

                Kind.LongDouble,
                Kind.ComplexLongDouble,
                Kind.ComplexDouble,
                Kind.Double,
                => return p.errStr(.duplicate_declspec, p.index, "double"),

                else => return spec.cannotCombine(p),
            },

            Kind.Complex => spec.kind = switch (spec.kind) {
                Kind.None => Kind.Complex,
                Kind.Long => Kind.ComplexLong,
                Kind.Float => Kind.ComplexFloat,
                Kind.Double => Kind.ComplexDouble,
                Kind.LongDouble => Kind.ComplexLongDouble,

                Kind.Complex,
                Kind.ComplexLong,
                Kind.ComplexFloat,
                Kind.ComplexDouble, //
                Kind.ComplexLongDouble,
                => return p.errStr(.duplicate_declspec, p.index, "_Complex"),

                else => return spec.cannotCombine(p),
            },

            else => unreachable,
        }
    }

    pub fn fromType(ty: Type) Kind {
        return switch (ty.specifier) {
            .Void => Kind.Void,
            .Bool => Kind.Bool,
            .Char => Kind.Char,
            .SChar => Kind.SChar,
            .UChar => Kind.UChar,
            .Short => Kind.Short,
            .UShort => Kind.UShort,
            .Int => Kind.Int,
            .UInt => Kind.UInt,
            .Long => Kind.Long,
            .ULong => Kind.ULong,
            .LongLong => Kind.LongLong,
            .ULongLong => Kind.ULongLong,
            .Float => Kind.Float,
            .Double => Kind.Double,
            .LongDouble => Kind.LongDouble,
            .ComplexFloat => Kind.ComplexFloat,
            .ComplexDouble => Kind.ComplexDouble,
            .ComplexLongDouble => Kind.ComplexLongDouble,

            .Pointer => .{ .Pointer = ty.data.subType },
            .Atomic => .{ .Atomic = ty.data.subType },
            .Func => .{ .Func = ty.data.func },
            .VarArgsFunc => .{ .VarArgsFunc = ty.data.func },
            .OldStyleFunc => .{ .OldStyleFunc = ty.data.func },
            .Array => .{ .Array = ty.data.array },
            .StaticArray => .{ .StaticArray = ty.data.array },
            .Struct => .{ .Struct = ty.data.node },
            .Union => .{ .Union = ty.data.node },
            .Enum => .{ .Enum = ty.data.node },
        };
    }
};