const std = @import("std");
const Type = @This();

pub const Qualifiers = struct {
    @"const": bool = false,
    atomic: bool = false,
    @"volatile": bool = false,
    restrict: bool = false,
};

pub const Specifier = union(enum) {
    // defaulted to int type
    None,

    /// default to int
    Unsigned,
    Signed,

    /// default to ComplexDouble
    Complex,

    /// default to ComplexLongDouble
    ComplexLong,

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

    Pointer: struct {
        qual: Qualifiers,
        elem: *Type,
    },

    Function: struct {
        returnType: *Type,
        paramTypes: []Type,
    },

    /// Decays to pointer
    Array: struct {
        len: u64,
        static: bool,
        elem: *Type,
    },

    incomplete_enum,
    incomplete_struct,
    incomplete_union,

    typedef: *Type,
    @"struct": Record,
    @"union": Record,
    @"enum": struct {},

    pub const Record = struct {
        fields: []Field,

        pub const Field = struct {
            type: Type,
            name: []const u8,
        };
    };

    pub fn toString(spec: Specifier) []const u8 {
        return switch (spec) {
            .None => unreachable,
            .Unsigned => "unsigned",
            .Signed => "signed",
            .Complex => "_Complex",
            .ComplexLong => "_Complex long",

            .Void => "void",
            .Bool => "_Bool",
            .Char => "char",
            .SChar => "signed char",
            .UChar => "unsigned char",
            .Short => "short",
            .UShort => "unsigned short",
            .Int => "int",
            .UInt => "unsigned int",
            .Long => "long",
            .ULong => "unsigned long",
            .LongLong => "long long",
            .ULongLong => "unsigned long long",
            .Float => "float",
            .Double => "double",
            .LongDouble => "long double",
            .ComplexFloat => "_Complex float",
            .ComplexDouble => "_Complex double",
            .ComplexLongDouble => "_Complex long double",

            else => "TODO Specifier string",
        };
    }
};

qual: Qualifiers = .{},
specifier: Specifier = .None,
alignment: u32 = 0,
