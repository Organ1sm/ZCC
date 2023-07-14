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

    Bool,

    // integers
    Char,
    Schar,
    Uchar,
    Short,
    Ushort,
    Int,
    Uint,
    Long,
    Ulong,
    LongLong,
    UlongLong,

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
};

qual: Qualifiers = .{},
specifier: Specifier = .none,
alignment: u32 = 0,
