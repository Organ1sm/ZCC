const Type = @import("Type.zig");
const NodeIndex = @import("AST.zig").NodeIndex;
const Parser = @import("Parser.zig");

const TypeBuilder = @This();

pub const Builder = union(enum) {
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
    Array: *Type.Array,
    StaticArray: *Type.Array,

    Struct: NodeIndex,
    Union: NodeIndex,
    Enum: NodeIndex,

    pub fn toString(spec: Builder) []const u8 {
        return switch (spec) {
            Builder.None => unreachable,

            Builder.Void => "void",
            Builder.Bool => "_Bool",

            Builder.Char => "char",
            Builder.SChar => "signed char",
            Builder.UChar => "unsigned char",

            Builder.Unsigned => "unsigned",
            Builder.Signed => "signed",

            Builder.Short => "short",
            Builder.UShort => "unsigned short",
            Builder.SShort => "signed short",

            Builder.ShortInt => "short int",
            Builder.SShortInt => "signed short int",
            Builder.UShortInt => "unsigned short int",
            Builder.Int => "int",
            Builder.SInt => "signed int",
            Builder.UInt => "unsigned int",

            Builder.Long => "long",
            Builder.SLong => "signed long",
            Builder.ULong => "unsigned long",
            Builder.LongInt => "long int",
            Builder.SLongInt => "signed long int",
            Builder.ULongInt => "unsigned long int",
            Builder.LongLong => "long long",
            Builder.SLongLong => "signed long long",
            Builder.ULongLong => "unsigned long long",
            Builder.LongLongInt => "long long int",
            Builder.SLongLongInt => "signed long long int",
            Builder.ULongLongInt => "unsigned long long int",

            Builder.Float => "float",
            Builder.Double => "double",
            Builder.LongDouble => "long double",
            Builder.Complex => "_Complex",
            Builder.ComplexFloat => "_Complex float",
            Builder.ComplexLong => "_Complex long",
            Builder.ComplexDouble => "_Complex double",
            Builder.ComplexLongDouble => "_Complex long double",

            Builder.Pointer => "pointer",
            Builder.Atomic => "atomic",
            Builder.Func, Builder.VarArgsFunc => "function",
            Builder.Array, Builder.StaticArray => "array",
            Builder.Struct => "struct",
            Builder.Union => "union",
            Builder.Enum => "enum",
        };
    }

    pub fn finish(spec: Builder, p: *Parser, ty: *Type) Parser.Error!void {
        ty.specifier = switch (spec) {
            Builder.None => {
                ty.specifier = .Int;
                return p.err(.missing_type_specifier);
            },

            Builder.Void => .Void,
            Builder.Bool => .Bool,
            Builder.Char => .Char,
            Builder.SChar => .SChar,
            Builder.UChar => .UChar,

            Builder.Unsigned => .UInt,
            Builder.Signed => .Int,

            Builder.ShortInt, Builder.SShortInt, Builder.Short, Builder.SShort => .Short,
            Builder.UShort, Builder.UShortInt => .UShort,

            Builder.Int, .SInt => .Int,
            Builder.UInt => .UInt,

            Builder.Long, Builder.SLong, Builder.LongInt, Builder.SLongInt => .Long,
            Builder.ULong, Builder.ULongInt => .ULong,

            Builder.LongLong, Builder.SLongLong, Builder.LongLongInt, Builder.SLongLongInt => .LongLong,
            Builder.ULongLong, Builder.ULongLongInt => .ULongLong,

            Builder.Float => .Float,
            Builder.Double => .Double,
            Builder.LongDouble => .LongDouble,
            Builder.ComplexFloat => .ComplexFloat,
            Builder.ComplexDouble => .ComplexDouble,
            Builder.ComplexLongDouble => .ComplexLongDouble,
            Builder.Complex, Builder.ComplexLong => {
                const token = p.getCurrToken();
                try p.pp.compilation.diag.add(.{
                    .tag = .type_is_invalid,
                    .sourceId = token.source,
                    .locStart = token.loc.start,
                    .extra = .{ .str = spec.toString() },
                });

                return error.ParsingFailed;
            },

            Builder.Atomic => |data| {
                ty.specifier = .Atomic;
                ty.data = .{ .subType = data };
                return;
            },

            Builder.Pointer => |data| {
                ty.specifier = .Pointer;
                ty.data = .{ .subType = data };
                return;
            },

            Builder.Func => |data| {
                ty.specifier = .Func;
                ty.data = .{ .func = data };
                return;
            },

            Builder.VarArgsFunc => |data| {
                ty.specifier = .VarArgsFunc;
                ty.data = .{ .func = data };
                return;
            },

            Builder.Array => |data| {
                ty.specifier = .Array;
                ty.data = .{ .array = data };
                return;
            },

            Builder.StaticArray => |data| {
                ty.specifier = .StaticArray;
                ty.data = .{ .array = data };
                return;
            },

            Builder.Struct => |data| {
                ty.specifier = .Struct;
                ty.data = .{ .node = data };
                return;
            },

            Builder.Union => |data| {
                ty.specifier = .Union;
                ty.data = .{ .node = data };
                return;
            },

            Builder.Enum => |data| {
                ty.specifier = .Enum;
                ty.data = .{ .node = data };
                return;
            },
        };
    }

    pub fn cannotCombine(spec: *Builder, p: *Parser) Parser.Error {
        const token = p.getCurrToken();

        try p.pp.compilation.diag.add(.{
            .tag = .cannot_combine_spec,
            .sourceId = token.source,
            .locStart = token.loc.start,
            .extra = .{ .str = spec.toString() },
        });

        return error.ParsingFailed;
    }

    pub fn combine(spec: *Builder, p: *Parser, new: Builder) Parser.Error!void {
        switch (new) {
            Builder.Void,
            Builder.Bool,
            Builder.Enum,
            Builder.Struct,
            Builder.Union,
            Builder.Pointer,
            Builder.Array,
            Builder.StaticArray, //
            Builder.Func,
            Builder.VarArgsFunc,
            => switch (spec.*) {
                .None => spec.* = new,
                else => return spec.cannotCombine(p),
            },

            Builder.Atomic => return p.todo("atomic type"),
            Builder.Signed => spec.* = switch (spec.*) {
                Builder.None => Builder.Signed,
                Builder.Char => Builder.SChar,
                Builder.Short => Builder.SShort,
                Builder.ShortInt => Builder.SShortInt,
                Builder.Int => Builder.SInt,
                Builder.Long => Builder.SLong,
                Builder.LongInt => Builder.SLongInt,
                Builder.LongLong => Builder.SLongLong,
                Builder.LongLongInt => Builder.SLongLongInt,

                Builder.SShort,
                Builder.SShortInt,
                Builder.SInt,
                Builder.SLong,
                Builder.SLongInt,
                Builder.SLongLong,
                Builder.SLongLongInt,
                => return p.errStr(.duplicate_declspec, p.index, "signed"),

                else => return spec.cannotCombine(p),
            },

            Builder.Unsigned => spec.* = switch (spec.*) {
                Builder.None => Builder.Unsigned,
                Builder.Char => Builder.UChar,
                Builder.Short => Builder.UShort,
                Builder.ShortInt => Builder.UShortInt,
                Builder.Int => Builder.UInt,
                Builder.Long => Builder.ULong,
                Builder.LongInt => Builder.ULongInt,
                Builder.LongLong => Builder.ULongLong,
                Builder.LongLongInt => Builder.ULongLongInt,

                Builder.UShort,
                Builder.UShortInt,
                Builder.UInt,
                Builder.ULong,
                Builder.ULongInt,
                Builder.ULongLong, //
                Builder.ULongLongInt,
                => return p.errStr(.duplicate_declspec, p.index, "unsigned"),

                else => return spec.cannotCombine(p),
            },

            Builder.Char => spec.* = switch (spec.*) {
                Builder.None => Builder.Char,
                Builder.Unsigned => Builder.UChar,
                Builder.Signed => Builder.SChar,
                Builder.Char, Builder.SChar, Builder.UChar => return p.errStr(.duplicate_declspec, p.index, "char"),

                else => return spec.cannotCombine(p),
            },

            Builder.Int => spec.* = switch (spec.*) {
                Builder.None => Builder.Int,
                Builder.Signed => Builder.SInt,
                Builder.Unsigned => Builder.UInt,
                Builder.Short => Builder.ShortInt,
                Builder.SShort => Builder.SShortInt,
                Builder.UShort => Builder.UShortInt,
                Builder.Long => Builder.LongInt,
                Builder.SLong => Builder.SLongInt,
                Builder.ULong => Builder.ULongInt,
                Builder.LongLong => Builder.LongLongInt,
                Builder.SLongLong => Builder.SLongLongInt,
                Builder.ULongLong => Builder.ULongLongInt,

                Builder.Int,
                Builder.SInt,
                Builder.UInt,
                Builder.ShortInt,
                Builder.SShortInt,
                Builder.UShortInt,
                Builder.LongInt,
                Builder.SLongInt,
                Builder.ULongInt,
                Builder.LongLongInt,
                Builder.SLongLongInt,
                Builder.ULongLongInt,
                => return p.errStr(.duplicate_declspec, p.index, "int"),

                else => return spec.cannotCombine(p),
            },

            Builder.Long => spec.* = switch (spec.*) {
                Builder.None => Builder.Long,
                Builder.Long => Builder.LongLong,
                Builder.Unsigned => Builder.ULong,
                Builder.Signed => Builder.Long,
                Builder.Int => Builder.LongInt,
                Builder.SInt => Builder.SLongInt,
                Builder.ULong => Builder.ULongLong,
                Builder.LongLong, Builder.ULongLong => return p.errStr(.duplicate_declspec, p.index, "long"),

                else => return spec.cannotCombine(p),
            },

            Builder.Float => spec.* = switch (spec.*) {
                Builder.None => Builder.Float,
                Builder.Complex => Builder.ComplexFloat,

                Builder.ComplexFloat, Builder.Float => return p.errStr(.duplicate_declspec, p.index, "float"),
                else => return spec.cannotCombine(p),
            },

            Builder.Double => spec.* = switch (spec.*) {
                Builder.None => Builder.Double,
                Builder.Long => Builder.LongDouble,
                Builder.Complex => Builder.ComplexDouble,
                Builder.ComplexLong => Builder.ComplexLongDouble,

                Builder.LongDouble,
                Builder.ComplexLongDouble,
                Builder.ComplexDouble,
                Builder.Double,
                => return p.errStr(.duplicate_declspec, p.index, "double"),

                else => return spec.cannotCombine(p),
            },

            Builder.Complex => spec.* = switch (spec.*) {
                Builder.None => Builder.Complex,
                Builder.Long => Builder.ComplexLong,
                Builder.Float => Builder.ComplexFloat,
                Builder.Double => Builder.ComplexDouble,
                Builder.LongDouble => Builder.ComplexLongDouble,

                Builder.Complex,
                Builder.ComplexLong,
                Builder.ComplexFloat,
                Builder.ComplexDouble, //
                Builder.ComplexLongDouble,
                => return p.errStr(.duplicate_declspec, p.index, "_Complex"),

                else => return spec.cannotCombine(p),
            },

            else => unreachable,
        }
    }

    pub fn fromType(ty: Type) Builder {
        return switch (ty.specifier) {
            .Void => Builder.Void,
            .Bool => Builder.Bool,
            .Char => Builder.Char,
            .SChar => Builder.SChar,
            .UChar => Builder.UChar,
            .Short => Builder.Short,
            .UShort => Builder.UShort,
            .Int => Builder.Int,
            .UInt => Builder.UInt,
            .Long => Builder.Long,
            .ULong => Builder.ULong,
            .LongLong => Builder.LongLong,
            .ULongLong => Builder.ULongLong,
            .Float => Builder.Float,
            .Double => Builder.Double,
            .LongDouble => Builder.LongDouble,
            .ComplexFloat => Builder.ComplexFloat,
            .ComplexDouble => Builder.ComplexDouble,
            .ComplexLongDouble => Builder.ComplexLongDouble,

            .Pointer => .{ .Pointer = ty.data.subType },
            .Atomic => .{ .Atomic = ty.data.subType },
            .Func => .{ .Func = ty.data.func },
            .VarArgsFunc => .{ .VarArgsFunc = ty.data.func },
            .Array => .{ .Array = ty.data.array },
            .StaticArray => .{ .StaticArray = ty.data.array },
            .Struct => .{ .Struct = ty.data.node },
            .Union => .{ .Union = ty.data.node },
            .Enum => .{ .Enum = ty.data.node },
        };
    }
};
