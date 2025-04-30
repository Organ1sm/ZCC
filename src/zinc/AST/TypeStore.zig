pub const std = @import("std");

const Attribute = @import("../Lexer//Attribute.zig");
const Compilation = @import("../Basic/Compilation.zig");
const LangOpts = @import("../Basic/LangOpts.zig");
const RecordLayout = @import("../Basic/RecordLayout.zig");
const Parser = @import("../Parser/Parser.zig");
const StringInterner = @import("../Basic/StringInterner.zig");
const StringId = StringInterner.StringId;
const TargetUtil = @import("../Basic/Target.zig");
const Tree = @import("AST.zig");
const Node = Tree.Node;
const TokenIndex = Tree.TokenIndex;

const Repr = struct {
    tag: Tag,
    /// If a Type has a child type it is stored in data[0].
    data: [2]u32,

    pub const Tag = enum(u8) {
        Complex,
        BitInt,
        Atomic,
        Func,
        FuncVariadic,
        FuncOldStyle,
        FuncZero,
        FuncVariadicZero,
        FuncOldStyleZero,
        FuncOne,
        FuncVariadicOne,
        FuncOldStyleOne,
        Pointer,
        PointerDecayed,
        ArrayIncomplete,
        ArrayFixed,
        ArrayStatic,
        ArrayVariable,
        ArrayUnspecifiedVariable,
        Vector,
        Struct,
        StructIncomplete,
        Union,
        UnionIncomplete,
        Enum,
        EnumFixed,
        EnumIncomplete,
        Typeof,
        TypeofExpr,
        Typedef,
        Attributed,
        AttributedOne,
    };
};

const Index = enum(u29) {
    /// A NaN-like poison value
    Invalid = std.math.maxInt(u29) - 0,
    /// GNU auto type
    /// This is a placeholder specifier - it must be replaced by the actual type specifier (determined by the initializer)
    AutoType = std.math.maxInt(u29) - 1,
    /// C23 auto, behaves like auto_type
    C23Auto = std.math.maxInt(u29) - 2,
    Void = std.math.maxInt(u29) - 3,
    Bool = std.math.maxInt(u29) - 4,
    NullptrTy = std.math.maxInt(u29) - 5,
    Char = std.math.maxInt(u29) - 6,
    SChar = std.math.maxInt(u29) - 7,
    UChar = std.math.maxInt(u29) - 8,
    Short = std.math.maxInt(u29) - 9,
    UShort = std.math.maxInt(u29) - 10,
    Int = std.math.maxInt(u29) - 11,
    UInt = std.math.maxInt(u29) - 12,
    Long = std.math.maxInt(u29) - 13,
    ULong = std.math.maxInt(u29) - 14,
    LongLong = std.math.maxInt(u29) - 15,
    ULongLong = std.math.maxInt(u29) - 16,
    Int128 = std.math.maxInt(u29) - 17,
    UInt128 = std.math.maxInt(u29) - 18,
    FP16 = std.math.maxInt(u29) - 19,
    Float16 = std.math.maxInt(u29) - 20,
    Float = std.math.maxInt(u29) - 21,
    Double = std.math.maxInt(u29) - 22,
    LongDouble = std.math.maxInt(u29) - 23,
    Float128 = std.math.maxInt(u29) - 24,
    VoidPointer = std.math.maxInt(u29) - 25,
    CharPointer = std.math.maxInt(u29) - 26,
    IntPointer = std.math.maxInt(u29) - 27,
    _,
};

const TypeStore = @This();

pub const QualType = packed struct(u32) {
    @"const": bool = false,
    @"volatile": bool = false,
    restrict: bool = false,

    _index: Index,

    pub const invalid: QualType = .{ ._index = .Invalid };
    pub const autoType: QualType = .{ ._index = .AutoType };
    pub const c23Auto: QualType = .{ ._index = .C23Auto };
    pub const @"void": QualType = .{ ._index = .Void };
    pub const @"bool": QualType = .{ ._index = .Bool };
    pub const nullptrTy: QualType = .{ ._index = .NullptrTy };
    pub const char: QualType = .{ ._index = .Char };
    pub const schar: QualType = .{ ._index = .SChar };
    pub const uchar: QualType = .{ ._index = .UChar };
    pub const short: QualType = .{ ._index = .Short };
    pub const ushort: QualType = .{ ._index = .UShort };
    pub const int: QualType = .{ ._index = .Int };
    pub const uint: QualType = .{ ._index = .UInt };
    pub const long: QualType = .{ ._index = .Long };
    pub const ulong: QualType = .{ ._index = .ULong };
    pub const longlong: QualType = .{ ._index = .LongLong };
    pub const ulonglong: QualType = .{ ._index = .ULongLong };
    pub const int128: QualType = .{ ._index = .Int128 };
    pub const uint128: QualType = .{ ._index = .UInt128 };
    pub const fp16: QualType = .{ ._index = .FP16 };
    pub const float16: QualType = .{ ._index = .Float16 };
    pub const float: QualType = .{ ._index = .Float };
    pub const double: QualType = .{ ._index = .Double };
    pub const longDouble: QualType = .{ ._index = .LongDouble };
    pub const float128: QualType = .{ ._index = .Float128 };
    pub const voidPointer: QualType = .{ ._index = .VoidPointer };
    pub const charPointer: QualType = .{ ._index = .CharPointer };
    pub const intPointer: QualType = .{ ._index = .IntPointer };

    pub fn isInvalid(qt: QualType) bool {
        return qt._index == .Invalid;
    }

    pub fn @"type"(qt: QualType, comp: *const Compilation) Type {
        switch (qt._index) {
            .Invalid => unreachable,
            .AutoType => unreachable,
            .C23Auto => unreachable,
            .Void => return .void,
            .Bool => return .bool,
            .NullptrTy => return .nullptrTy,
            .Char => return .{ .int = .Char },
            .SChar => return .{ .int = .SChar },
            .UChar => return .{ .int = .UChar },
            .Short => return .{ .int = .Short },
            .UShort => return .{ .int = .UShort },
            .Int => return .{ .int = .Int },
            .UInt => return .{ .int = .UInt },
            .Long => return .{ .int = .Long },
            .ULong => return .{ .int = .ULong },
            .LongLong => return .{ .int = .LongLong },
            .ULongLong => return .{ .int = .ULongLong },
            .Int128 => return .{ .int = .Int128 },
            .UInt128 => return .{ .int = .UInt128 },
            .FP16 => return .{ .float = .FP16 },
            .Float16 => return .{ .float = .Float16 },
            .Float => return .{ .float = .Float },
            .Double => return .{ .float = .Double },
            .LongDouble => return .{ .float = .LongDouble },
            .Float128 => return .{ .float = .Float128 },
            .VoidPointer => return .{ .pointer = .{ .child = .void, .decayed = null } },
            .CharPointer => return .{ .pointer = .{ .child = .char, .decayed = null } },
            .IntPointer => return .{ .pointer = .{ .child = .int, .decayed = null } },
            else => {},
        }

        const ts = comp.typeStore;
        const extra = ts.extra.items;
        const repr = ts.types.get(@intFromEnum(qt._index));
        return switch (repr.tag) {
            .Complex => .{ .complex = @bitCast(repr.data[0]) },
            .Atomic => .{ .atomic = @bitCast(repr.data[0]) },
            .BitInt => .{
                .bitInt = .{
                    .bits = @intCast(repr.data[0]),
                    .signedness = @enumFromInt(repr.data[1]),
                },
            },

            .Func,
            .FuncVariadic,
            .FuncOldStyle,
            .FuncZero,
            .FuncVariadicZero,
            .FuncOldStyleZero,
            .FuncOne,
            .FuncVariadicOne,
            .FuncOldStyleOne,
            => @panic("TODO"),

            .Pointer => .{
                .pointer = .{
                    .child = @bitCast(repr.data[0]),
                    .decayed = null,
                },
            },
            .PointerDecayed => .{
                .pointer = .{
                    .child = @bitCast(repr.data[0]),
                    .decayed = @bitCast(repr.data[1]),
                },
            },
            .ArrayIncomplete => .{
                .array = .{
                    .elem = @bitCast(repr.data[0]),
                    .len = .incomplete,
                },
            },
            .ArrayFixed => .{
                .array = .{
                    .elem = @bitCast(repr.data[0]),
                    .len = .{ .fixed = @bitCast(ts.extra.items[repr.data[1]..][0..2].*) },
                },
            },
            .ArrayStatic => .{
                .array = .{
                    .elem = @bitCast(repr.data[0]),
                    .len = .{ .static = @bitCast(ts.extra.items[repr.data[1]..][0..2].*) },
                },
            },
            .ArrayVariable => .{
                .array = .{
                    .elem = @bitCast(repr.data[0]),
                    .len = .{ .variable = @enumFromInt(repr.data[1]) },
                },
            },
            .ArrayUnspecifiedVariable => .{
                .array = .{
                    .elem = @bitCast(repr.data[0]),
                    .len = .unspecifiedVariable,
                },
            },
            .Vector => .{
                .vector = .{
                    .elem = @bitCast(repr.data[0]),
                    .len = repr.data[1],
                },
            },
            .Struct, .Union => {
                const layoutSize = 6;
                const layout = @as(*Type.Record.Layout, @alignCast(@ptrCast(extra[repr.data[1]..][0..layoutSize]))).*;
                const fieldsLen = extra[repr.data[1] + layoutSize];
                const fields = extra[repr.data[1] + layoutSize + 1 ..][0..fieldsLen];

                const record: Type.Record = .{
                    .name = @enumFromInt(repr.data[0]),
                    .layout = layout,
                    .fields = std.mem.bytesAsSlice(Type.Record.Field, std.mem.sliceAsBytes(fields)),
                };
                return switch (repr.tag) {
                    .Struct => .{ .@"struct" = record },
                    .Union => .{ .@"union" = record },
                    else => unreachable,
                };
            },
            .StructIncomplete => .{
                .@"struct" = .{
                    .name = @enumFromInt(repr.data[0]),
                    .layout = null,
                    .fields = &.{},
                },
            },
            .UnionIncomplete => .{
                .@"union" = .{
                    .name = @enumFromInt(repr.data[0]),
                    .layout = null,
                    .fields = &.{},
                },
            },
            .Enum, .EnumFixed => {
                return .{
                    .@"enum" = .{
                        .name = @enumFromInt(extra[repr.data[1]]),
                        .tag = @bitCast(repr.data[0]),
                        .fixed = repr.tag == .EnumFixed,
                        .fields = std.mem.bytesAsSlice(Type.Enum.Field, std.mem.sliceAsBytes(extra[repr.data[1] + 1 ..][0 .. repr.data[1] + 2])),
                    },
                };
            },
            .EnumIncomplete => .{
                .@"enum" = .{
                    .name = @enumFromInt(repr.data[0]),
                    .tag = null,
                    .fixed = false,
                    .fields = &.{},
                },
            },
            .Typeof => .{
                .typeof = .{
                    .base = @bitCast(repr.data[0]),
                    .expr = null,
                },
            },
            .TypeofExpr => .{
                .typeof = .{
                    .base = @bitCast(repr.data[0]),
                    .expr = @enumFromInt(repr.data[1]),
                },
            },
            .Typedef => .{ .typedef = .{
                .base = @bitCast(repr.data[0]),
                .name = @enumFromInt(repr.data[1]),
            } },
            .Attributed => .{
                .attributed = .{
                    .base = @bitCast(repr.data[0]),
                    .attributes = ts.attributes.items[extra[repr.data[1]]..][0..extra[repr.data[1] + 1]],
                },
            },
            .AttributedOne => .{
                .attributed = .{
                    .base = @bitCast(repr.data[0]),
                    .attributes = ts.attributes.items[repr.data[1]..][0..1],
                },
            },
        };
    }

    pub fn base(qt: QualType, comp: *const Compilation) struct { type: Type, qt: QualType } {
        var cur = qt;
        while (true) switch (cur.type(comp)) {
            .typeof => |typeof| cur = typeof.base,
            .typedef => |typedef| cur = typedef.base,
            .attributed => |attributed| cur = attributed.base,
            else => |ty| return .{ .type = ty, .qt = cur },
        };
    }

    pub fn get(qt: QualType, comp: *const Compilation, comptime tag: std.meta.Tag(Type)) ?@FieldType(Type, @tagName(tag)) {
        comptime std.debug.assert(tag != .typeof and tag != .attributed and tag != .typedef);
        const baseType = qt.base(comp).type;
        if (baseType == tag) return @field(baseType, @tagName(tag));
        return null;
    }

    pub fn is(qt: QualType, comp: *const Compilation, comptime tag: std.meta.Tag(Type)) bool {
        return qt.get(comp, tag) != null;
    }

    pub fn isUnsignedInt(qt: QualType, comp: *const Compilation) bool {
        return qt.signedness(comp) == .unsigned;
    }

    pub fn childType(qt: QualType, comp: *const Compilation) QualType {
        if (qt._index == .Invalid) return .invalid;
        return switch (qt.base(comp).type) {
            .complex => |complex| complex,
            .func => |func| func.returnType,
            .pointer => |pointer| pointer.child,
            .array => |array| array.elem,
            .vector => |vector| vector.elem,
            .@"enum" => |@"enum"| @"enum".tag.?,
            else => unreachable,
        };
    }

    pub fn arrayLen(qt: QualType, comp: *Compilation) ?u64 {
        const arrayType = switch (qt.base(comp).type) {
            .array => |array| array,
            .pointer => |pointer| blk: {
                const decayed = pointer.decayed orelse return null;
                break :blk decayed.get(comp, .array) orelse return null;
            },
            else => return null,
        };
        switch (arrayType.len) {
            .fixed, .static => |len| return len,
            else => return null,
        }
    }

    pub fn sizeof(qt: QualType, comp: *const Compilation) u64 {
        _ = qt;
        _ = comp;
        @panic("TODO");
    }

    pub fn sizeofOrNull(qt: QualType, comp: *const Compilation) ?u64 {
        _ = qt;
        _ = comp;
        @panic("TODO");
    }

    pub fn bitSizeof(qt: QualType, comp: *const Compilation) u64 {
        _ = qt;
        _ = comp;
        @panic("TODO");
    }

    pub fn bitSizeofOrNull(qt: QualType, comp: *const Compilation) ?u64 {
        _ = qt;
        _ = comp;
        @panic("TODO");
    }

    pub fn signedness(qt: QualType, comp: *const Compilation) std.builtin.Signedness {
        _ = qt;
        _ = comp;
        @panic("TODO");
    }

    pub fn alignof(qt: QualType, comp: *const Compilation) u32 {
        _ = qt;
        _ = comp;
        @panic("TODO");
    }

    /// Suffix for integer values of this type
    pub fn intValueSuffix(qt: QualType, comp: *const Compilation) []const u8 {
        return switch (qt.get(comp, .int).?) {
            .Short, .Int => "",
            .Long => "L",
            .LongLong => "LL",
            .SChar, .UChar, .Char => {
                // Only 8-bit char supported currently;
                // TODO: handle platforms with 16-bit int + 16-bit char
                std.debug.assert(qt.sizeof(comp) == 1);
                return "";
            },
            .UShort => {
                if (qt.sizeof(comp) < int.sizeof(comp))
                    return "";
                return "U";
            },
            .UInt => "U",
            .ULong => "UL",
            .ULongLong => "ULL",
            else => unreachable, // TODO
        };
    }

    /// printf format modifier
    pub fn formatModifier(qt: QualType, comp: *const Compilation) []const u8 {
        return switch (qt.get(comp, .int).?) {
            .SChar, .UChar => "hh",
            .Short, .UShort => "h",
            .Int, .UInt => "",
            .Long, .ULong => "l",
            .LongLong, .ULongLong => "ll",
            else => unreachable, // TODO
        };
    }

    /// Make real int type unsigned.
    /// Discards attributes.
    pub fn makeIntUnsigned(qt: QualType, comp: *Compilation) !QualType {
        switch (qt.base(comp).type) {
            .int => |kind| switch (kind) {
                .Char => return .uchar,
                .SChar => return .uchar,
                .UChar => return .uchar,
                .Short => return .ushort,
                .UShort => return .ushort,
                .Int => return .uint,
                .UInt => return .uint,
                .Long => return .ulong,
                .ULong => return .ulong,
                .LongLong => return .ulonglong,
                .ULongLong => return .ulonglong,
                .Int128 => return .uint128,
                .UInt128 => return .uint128,
            },
            .bitInt => |bitInt| {
                return try comp.typeStore.put(comp.gpa, .{
                    .bitInt = .{
                        .signedness = .unsigned,
                        .bits = bitInt.bits,
                    },
                });
            },
            else => unreachable,
        }
    }

    pub const ScalarKind = enum {
        Enum,
        Bool,
        Int,
        Float,
        Pointer,
        ComplexInt,
        ComplexFloat,
        None,

        pub fn isInt(sk: ScalarKind) bool {
            return switch (sk) {
                .Bool, .Enum, .Int, .ComplexInt => true,
                else => false,
            };
        }

        pub fn isFloat(sk: ScalarKind) bool {
            return switch (sk) {
                .Float, .ComplexFloat => true,
                else => false,
            };
        }

        pub fn isReal(sk: ScalarKind) bool {
            return switch (sk) {
                .ComplexInt, .ComplexFloat => false,
                else => true,
            };
        }

        pub fn isArithmetic(sk: ScalarKind) bool {
            return switch (sk) {
                .Bool, .Enum, .Int, .ComplexInt, .Float, .ComplexFloat => true,
                else => false,
            };
        }
    };

    pub fn scalarKind(qt: QualType, comp: *const Compilation) ScalarKind {
        loop: switch (qt.base(comp).type) {
            .bool => return .Bool,
            .int => return .Int,
            .float => return .Float,
            .pointer => return .Pointer,
            .@"enum" => return .Enum,
            .complex => |complex| switch (complex.base(comp).type) {
                .int => return .ComplexInt,
                .float => return .ComplexFloat,
                else => unreachable,
            },
            .atomic => |atomic| continue :loop atomic.base(comp).type,
            else => return .none,
        }
    }

    pub fn hasAttribute(qt: QualType, comp: *const Compilation, tag: Attribute.Tag) bool {
        var it = Attribute.Iterator.initType(qt, comp);
        while (it.next()) |item| {
            const attr, _ = item;
            if (attr.tag == tag) return true;
        }
        return false;
    }

    pub fn requestedAlignment(qt: QualType, comp: *const Compilation) ?u32 {
        _ = qt;
        _ = comp;
        @panic("TODO");
    }

    pub fn annotationAlignment(comp: *const Compilation, attrs: Attribute.Iterator) ?u32 {
        var it = attrs;
        var maxRequested: ?u32 = null;
        var lastAlignedIndex: ?usize = null;
        while (it.next()) |item| {
            const attribute, const index = item;
            if (attribute.tag != .aligned) continue;
            if (lastAlignedIndex) |alignedIndex| {
                // once we recurse into a new type, after an `aligned` attribute was found, we're done
                if (index <= alignedIndex) break;
            }
            lastAlignedIndex = index;
            const requested = if (attribute.args.aligned.alignment) |alignment| alignment.requested else TargetUtil.defaultAlignment(comp.target);
            if (maxRequested == null or maxRequested.? < requested) {
                maxRequested = requested;
            }
        }
        return maxRequested;
    }

    pub fn print(qt: QualType, comp: *const Compilation, w: anytype) @TypeOf(w).Error!void {
        _ = qt;
        _ = comp;
        @panic("TODO");
    }

    pub fn dump(qt: QualType, comp: *const Compilation, w: anytype) @TypeOf(w).Error!void {
        _ = qt;
        _ = comp;
        @panic("TODO");
    }
};

pub const Type = union(enum) {
    void,
    bool,
    /// C23 nullptr_t
    nullptrTy,

    int: Int,
    float: Float,
    complex: QualType,
    bitInt: BitInt,
    atomic: QualType,

    func: Func,
    pointer: Pointer,
    array: Array,
    vector: Vector,

    @"struct": Record,
    @"union": Record,
    @"enum": Enum,

    typeof: TypeOf,
    typedef: TypeDef,
    attributed: Attributed,

    pub const Int = enum {
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

        pub fn bits(int: Int, comp: *const Compilation) u16 {
            return switch (int) {
                .Char => comp.target.cTypeBitSize(.char),
                .SChar => comp.target.cTypeBitSize(.char),
                .UChar => comp.target.cTypeBitSize(.char),
                .Short => comp.target.cTypeBitSize(.short),
                .UShort => comp.target.cTypeBitSize(.ushort),
                .Int => comp.target.cTypeBitSize(.int),
                .UInt => comp.target.cTypeBitSize(.uint),
                .Long => comp.target.cTypeBitSize(.long),
                .ULong => comp.target.cTypeBitSize(.ulong),
                .LongLong => comp.target.cTypeBitSize(.longlong),
                .ULongLong => comp.target.cTypeBitSize(.ulonglong),
                .Int128 => 128,
                .UInt128 => 128,
            };
        }
    };

    pub const Float = enum {
        FP16,
        Float16,
        Float,
        Double,
        LongDouble,
        Float128,

        pub fn bits(float: Float, comp: *const Compilation) u16 {
            return switch (float) {
                .FP16 => 16,
                .Float16 => 16,
                .Float => comp.target.cTypeBitSize(.float),
                .Double => comp.target.cTypeBitSize(.double),
                .LongDouble => comp.target.cTypeBitSize(.longdouble),
                .Float128 => 128,
            };
        }
    };

    pub const BitInt = struct {
        /// Must be >= 1 if unsigned and >= 2 if signed
        bits: u16,
        signedness: std.builtin.Signedness,
    };

    pub const Func = struct {
        returnType: QualType,
        kind: enum {
            /// int foo(int bar, char baz) and int (void)
            Normal,
            /// int foo(int bar, char baz, ...)
            Variadic,
            /// int foo(bar, baz) and int foo()
            /// is also var args, but we can give warnings about incorrect amounts of parameters
            OldStyle,
        },
        params: []const Param,

        pub const Param = extern struct {
            qt: QualType,
            name: StringId,
            node: Node.OptIndex,
        };
    };

    pub const Pointer = struct {
        child: QualType,
        decayed: ?QualType,
    };

    pub const Array = struct {
        elem: QualType,
        len: union(enum) {
            incomplete,
            fixed: u64,
            static: u64,
            variable: Node.Index,
            unspecifiedVariable,
        },
    };

    pub const Vector = struct {
        elem: QualType,
        len: u32,
    };

    pub const Record = struct {
        name: StringId,
        layout: ?Layout = null,
        fields: []const Field,

        pub const Field = extern struct {
            qt: QualType,
            name: StringId,
            /// zero for anonymous fields
            nameToken: TokenIndex = 0,
            bitWidth: enum(u32) {
                null = std.math.maxInt(u32),
                _,

                pub fn unpack(width: @This()) ?u32 {
                    if (width == .null) return null;
                    return @intFromEnum(width);
                }
            } = .null,
            layout: Field.Layout = .{ .offsetBits = 0, .sizeBits = 0 },
            _attr_index: u32 = 0,
            _attr_len: u32 = 0,

            pub fn attributes(field: Field, comp: *const Compilation) []const Attribute {
                return comp.typeStore.attributes.items[field._attr_index..][0..field._attr_len];
            }

            pub const Layout = extern struct {
                /// `offset_bits` and `size_bits` should both be INVALID if and only if the field
                /// is an unnamed bitfield. There is no way to reference an unnamed bitfield in C, so
                /// there should be no way to observe these values. If it is used, this value will
                /// maximize the chance that a safety-checked overflow will occur.
                const INVALID = std.math.maxInt(u64);

                /// The offset of the field, in bits, from the start of the struct.
                offsetBits: u64 align(4) = INVALID,
                /// The size, in bits, of the field.
                ///
                /// For bit-fields, this is the width of the field.
                sizeBits: u64 align(4) = INVALID,

                pub fn isUnnamed(self: Field.Layout) bool {
                    return self.offsetBits == INVALID and self.sizeBits == INVALID;
                }
            };
        };

        pub const Layout = extern struct {
            /// The size of the type in bits.
            ///
            /// This is the value returned by `sizeof` in C
            /// (but in bits instead of bytes). This is a multiple of `pointer_alignment_bits`.
            sizeBits: u64 align(4),
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
    };

    pub const Enum = struct {
        tag: ?QualType,
        fixed: bool,
        name: StringId,
        fields: []const Field,

        pub const Field = extern struct {
            type: QualType,
            name: StringId,
            nameToken: TokenIndex,
            node: Node.Index,
        };
    };

    pub const TypeOf = struct {
        base: QualType,
        expr: ?Node.Index,
    };

    pub const TypeDef = struct {
        base: QualType,
        name: StringId,
    };

    pub const Attributed = struct {
        base: QualType,
        attributes: []const Attribute,
    };
};

types: std.MultiArrayList(Repr) = .empty,
extra: std.ArrayListUnmanaged(u32) = .empty,
attributes: std.ArrayListUnmanaged(Attribute) = .empty,

wchar: QualType = .invalid,
uintLeast16Ty: QualType = .invalid,
uintLeast32Ty: QualType = .invalid,
ptrdiff: QualType = .invalid,
size: QualType = .invalid,
vaList: QualType = .invalid,
pidTy: QualType = .invalid,
nsConstantString: QualType = .invalid,
file: QualType = .invalid,
jmpBuf: QualType = .invalid,
sigJmpBuf: QualType = .invalid,
ucontextTy: QualType = .invalid,
intmax: QualType = .invalid,
intptr: QualType = .invalid,
int16: QualType = .invalid,
int64: QualType = .invalid,

pub fn deinit(ts: *TypeStore, gpa: std.mem.Allocator) void {
    ts.types.deinit(gpa);
    ts.extra.deinit(gpa);
    ts.attributes.deinit(gpa);
    ts.* = undefined;
}

pub fn put(ts: *TypeStore, gpa: std.mem.Allocator, ty: Type) !QualType {
    return .{ ._index = try ts.putExtra(gpa, ty) };
}

pub fn putExtra(ts: *TypeStore, gpa: std.mem.Allocator, ty: Type) !Index {
    switch (ty) {
        .void => return .Void,
        .bool => return .Bool,
        .nullptrTy => return .NullptrTy,
        .int => |int| switch (int) {
            .Char => return .Char,
            .SChar => return .SChar,
            .UChar => return .UChar,
            .Short => return .Short,
            .UShort => return .UShort,
            .Int => return .Int,
            .UInt => return .UInt,
            .Long => return .Long,
            .ULong => return .ULong,
            .LongLong => return .LongLong,
            .ULongLong => return .ULongLong,
            .Int128 => return .Int128,
            .UInt128 => return .UInt128,
        },
        .float => |float| switch (float) {
            .FP16 => return .FP16,
            .Float16 => return .Float16,
            .Float => return .Float,
            .Double => return .Double,
            .LongDouble => return .LongDouble,
            .Float128 => return .Float128,
        },
        else => {},
    }
    const index = try ts.types.addOne(gpa);
    try ts.set(gpa, ty, index);
    return @enumFromInt(index);
}

pub fn set(ts: *TypeStore, gpa: std.mem.Allocator, ty: Type, index: usize) !void {
    var repr: Repr = undefined;
    switch (ty) {
        .void => unreachable,
        .bool => unreachable,
        .nullptrTy => unreachable,
        .int => unreachable,
        .float => unreachable,
        .complex => |complex| {
            repr.tag = .Complex;
            repr.data[0] = @bitCast(complex);
        },
        .bitInt => |bitInt| {
            repr.tag = .BitInt;
            repr.data[0] = bitInt.bits;
            repr.data[1] = @intFromEnum(bitInt.signedness);
        },
        .atomic => |atomic| {
            repr.tag = .Atomic;
            std.debug.assert(!atomic.@"const" and !atomic.@"volatile");
            repr.data[0] = @bitCast(atomic);
        },
        .func => |func| {
            repr.data[0] = @bitCast(func.returnType);

            const extraIndex: u32 = @intCast(ts.extra.items.len);
            repr.data[1] = extraIndex;
            if (func.params.len > 1) {
                try ts.extra.append(gpa, @intCast(func.params.len));
            }

            const paramSize = 3;
            comptime std.debug.assert(@sizeOf(Type.Func.Param) == @sizeOf(u32) * paramSize);

            try ts.extra.ensureUnusedCapacity(gpa, func.params.len * paramSize);
            for (func.params) |*param| {
                const casted: *const [paramSize]u32 = @ptrCast(param);
                ts.extra.appendSliceAssumeCapacity(casted);
            }

            repr.tag = switch (func.kind) {
                .Normal => switch (func.params.len) {
                    0 => .FuncZero,
                    1 => .FuncOne,
                    else => .Func,
                },
                .Variadic => switch (func.params.len) {
                    0 => .FuncVariadicZero,
                    1 => .FuncVariadicOne,
                    else => .FuncVariadic,
                },
                .OldStyle => switch (func.params.len) {
                    0 => .FuncVariadicZero,
                    1 => .FuncVariadicOne,
                    else => .FuncVariadic,
                },
            };
        },
        .pointer => |pointer| {
            repr.data[0] = @bitCast(pointer.child);
            if (pointer.decayed) |array| {
                repr.tag = .PointerDecayed;
                repr.data[1] = @bitCast(array);
            } else {
                repr.tag = .Pointer;
            }
        },
        .array => |array| {
            repr.data[0] = @bitCast(array.elem);

            const extraIndex: u32 = @intCast(ts.extra.items.len);
            switch (array.len) {
                .incomplete => {
                    repr.tag = .ArrayIncomplete;
                },
                .fixed => |len| {
                    repr.tag = .ArrayFixed;
                    repr.data[1] = extraIndex;
                    try ts.extra.appendSlice(gpa, &@as([2]u32, @bitCast(len)));
                },
                .static => |len| {
                    repr.tag = .ArrayStatic;
                    repr.data[1] = extraIndex;
                    try ts.extra.appendSlice(gpa, &@as([2]u32, @bitCast(len)));
                },
                .variable => |expr| {
                    repr.tag = .ArrayVariable;
                    repr.data[1] = @intFromEnum(expr);
                },
                .unspecifiedVariable => {
                    repr.tag = .ArrayUnspecifiedVariable;
                },
            }
        },
        .vector => |vector| {
            repr.tag = .Vector;
            repr.data[0] = @bitCast(vector.elem);
            repr.data[1] = vector.len;
        },
        .@"struct", .@"union" => |record| record: {
            repr.data[0] = @intFromEnum(record.name);
            const layout = record.layout orelse {
                std.debug.assert(record.fields.len == 0);
                repr.tag = switch (ty) {
                    .@"struct" => .StructIncomplete,
                    .@"union" => .UnionIncomplete,
                    else => unreachable,
                };
                break :record;
            };
            repr.tag = switch (ty) {
                .@"struct" => .Struct,
                .@"union" => .Union,
                else => unreachable,
            };

            const extraIndex: u32 = @intCast(ts.extra.items.len);
            repr.data[1] = extraIndex;

            const layoutSize = 5;
            comptime std.debug.assert(@sizeOf(Type.Record.Layout) == @sizeOf(u32) * layoutSize);
            const fieldSize = 10;
            comptime std.debug.assert(@sizeOf(Type.Record.Field) == @sizeOf(u32) * fieldSize);
            try ts.extra.ensureUnusedCapacity(gpa, record.fields.len * fieldSize + layoutSize + 1);

            const castedLayout: *const [layoutSize]u32 = @ptrCast(&layout);
            ts.extra.appendSliceAssumeCapacity(castedLayout);
            ts.extra.appendAssumeCapacity(@intCast(record.fields.len));

            for (record.fields) |*field| {
                const casted: *const [fieldSize]u32 = @ptrCast(field);
                ts.extra.appendSliceAssumeCapacity(casted);
            }
        },
        .@"enum" => |@"enum"| @"enum": {
            const tagTy = @"enum".tag orelse {
                std.debug.assert(@"enum".fields.len == 0);
                repr.tag = .EnumIncomplete;
                repr.data[0] = @intFromEnum(@"enum".name);
                break :@"enum";
            };
            repr.data[0] = @bitCast(tagTy);
            repr.tag = if (@"enum".fixed) .EnumFixed else .Enum;

            const extraIndex: u32 = @intCast(ts.extra.items.len);
            repr.data[1] = extraIndex;

            const fieldSize = 4;
            comptime std.debug.assert(@sizeOf(Type.Enum.Field) == @sizeOf(u32) * fieldSize);
            try ts.extra.ensureUnusedCapacity(gpa, @"enum".fields.len * fieldSize + 1 + 1);

            ts.extra.appendAssumeCapacity(@intFromEnum(@"enum".name));
            ts.extra.appendAssumeCapacity(@intCast(@"enum".fields.len));

            for (@"enum".fields) |*field| {
                const casted: *const [fieldSize]u32 = @ptrCast(field);
                ts.extra.appendSliceAssumeCapacity(casted);
            }
        },
        .typeof => |typeof| {
            repr.data[0] = @bitCast(typeof.base);
            if (typeof.expr) |some| {
                repr.tag = .TypeofExpr;
                repr.data[1] = @intFromEnum(some);
            } else {
                repr.tag = .Typeof;
            }
        },
        .typedef => |typedef| {
            repr.tag = .Typedef;
            repr.data[0] = @bitCast(typedef.base);
            repr.data[1] = @intFromEnum(typedef.name);
        },
        .attributed => |attributed| {
            repr.data[0] = @bitCast(attributed.base);

            const attrIndex: u32 = @intCast(ts.attributes.items.len);
            const attrCount: u32 = @intCast(attributed.attributes.len);
            try ts.attributes.appendSlice(gpa, attributed.attributes);
            if (attrCount > 1) {
                repr.tag = .Attributed;
                const extraIndex: u32 = @intCast(ts.extra.items.len);
                repr.data[0] = extraIndex;
                try ts.extra.appendSlice(gpa, &.{ attrIndex, attrCount });
            } else {
                repr.tag = .AttributedOne;
                repr.data[0] = attrIndex;
            }
        },
    }
    ts.types.set(index, repr);
}

pub fn initNamedTypes(ts: *TypeStore, comp: *Compilation) !void {
    const os = comp.target.os.tag;
    ts.wchar = switch (comp.target.cpu.arch) {
        .xcore => .uchar,
        .ve, .msp430 => .uint,
        .arm, .armeb, .thumb, .thumbeb => if (os != .windows and os != .netbsd and os != .openbsd) .uint else .int,
        .aarch64, .aarch64_be => if (!os.isDarwin() and os != .netbsd) .uint else .int,
        .x86_64, .x86 => if (os == .windows) .ushort else .int,
        else => .int,
    };

    const ptrWidth = comp.target.ptrBitWidth();
    ts.ptrdiff = if (os == .windows and ptrWidth == 64)
        .longlong
    else switch (ptrWidth) {
        16 => .int,
        32 => .int,
        64 => .long,
        else => unreachable,
    };

    ts.size = if (os == .windows and ptrWidth == 64)
        .ulonglong
    else switch (ptrWidth) {
        16 => .uint,
        32 => .uint,
        64 => .ulong,
        else => unreachable,
    };

    ts.pidTy = switch (os) {
        .haiku => .long,
        // Todo: pid_t is required to "a signed integer type"; are there any systems
        // on which it is `short int`?
        else => .int,
    };

    ts.intmax = TargetUtil.intMaxType(comp.target);
    ts.intptr = TargetUtil.intPtrType(comp.target);
    ts.int16 = TargetUtil.int16Type(comp.target);
    ts.int64 = TargetUtil.int64Type(comp.target);
    ts.uintLeast16Ty = comp.intLeastN(16, .unsigned);
    ts.uintLeast32Ty = comp.intLeastN(32, .unsigned);

    ts.nsConstantString = try ts.generateNsConstantStringType(comp);
    ts.vaList = try ts.generateVaListType(comp);
}

fn generateNsConstantStringType(ts: *TypeStore, comp: *Compilation) !QualType {
    const constIntPtr: QualType = .{ .@"const" = true, ._index = .IntPointer };
    const constCharPtr: QualType = .{ .@"const" = true, ._index = .CharPointer };

    var record: Type.Record = .{
        .name = try comp.internString("__NSConstantString_tag"),
        .layout = null,
        .fields = &.{},
    };
    const qt = try ts.put(comp.gpa, .{ .@"struct" = record });

    var fields: [4]Type.Record.Field = .{
        .{ .name = try comp.internString("isa"), .qt = constIntPtr },
        .{ .name = try comp.internString("flags"), .qt = .int },
        .{ .name = try comp.internString("str"), .qt = constCharPtr },
        .{ .name = try comp.internString("length"), .qt = .long },
    };
    record.fields = &fields;
    record.layout = RecordLayout.compute(&fields, qt, comp, null) catch unreachable;
    try ts.set(comp.gpa, .{ .@"struct" = record }, @intFromEnum(qt._index));

    return qt;
}

fn generateVaListType(ts: *TypeStore, comp: *Compilation) !QualType {
    const Kind = enum { aarch64_va_list, x86_64_va_list };
    const kind: Kind = switch (comp.target.cpu.arch) {
        .aarch64 => switch (comp.target.os.tag) {
            .windows => return .charPointer,
            .ios, .macos, .tvos, .watchos => return .charPointer,
            else => .aarch64_va_list,
        },
        .sparc, .wasm32, .wasm64, .bpfel, .bpfeb, .riscv32, .riscv64, .avr, .spirv32, .spirv64 => return .voidPointer,
        .powerpc => switch (comp.target.os.tag) {
            .ios, .macos, .tvos, .watchos, .aix => return .charPointer,
            else => return .void, // unknown
        },
        .x86, .msp430 => return .charPointer,
        .x86_64 => switch (comp.target.os.tag) {
            .windows => return .charPointer,
            else => .x86_64_va_list,
        },
        else => return .void, // unknown
    };

    switch (kind) {
        .aarch64_va_list => {
            var record: Type.Record = .{
                .name = try comp.internString("__va_list_tag"),
                .layout = null,
                .fields = &.{},
            };
            const qt = try ts.put(comp.gpa, .{ .@"struct" = record });

            var fields: [5]Type.Record.Field = .{
                .{ .name = try comp.internString("__stack"), .qt = .voidPointer },
                .{ .name = try comp.internString("__gr_top"), .qt = .voidPointer },
                .{ .name = try comp.internString("__vr_top"), .qt = .voidPointer },
                .{ .name = try comp.internString("__gr_offs"), .qt = .int },
                .{ .name = try comp.internString("__vr_offs"), .qt = .int },
            };
            record.fields = &fields;
            record.layout = RecordLayout.compute(&fields, qt, comp, null) catch unreachable;
            try ts.set(comp.gpa, .{ .@"struct" = record }, @intFromEnum(qt._index));

            return qt;
        },
        .x86_64_va_list => {
            var record: Type.Record = .{
                .name = try comp.internString("__va_list_tag"),
                .layout = null,
                .fields = &.{},
            };
            const qt = try ts.put(comp.gpa, .{ .@"struct" = record });

            var fields: [4]Type.Record.Field = .{
                .{ .name = try comp.internString("gp_offset"), .qt = .uint },
                .{ .name = try comp.internString("fp_offset"), .qt = .uint },
                .{ .name = try comp.internString("overflow_arg_area"), .qt = .voidPointer },
                .{ .name = try comp.internString("reg_save_area"), .qt = .voidPointer },
            };
            record.fields = &fields;
            record.layout = RecordLayout.compute(&fields, qt, comp, null) catch unreachable;
            try ts.set(comp.gpa, .{ .@"struct" = record }, @intFromEnum(qt._index));

            return qt;
        },
    }
}

pub const Builder = struct {
    parser: *Parser,

    @"const": ?TokenIndex = null,
    atomic: ?TokenIndex = null,
    @"volatile": ?TokenIndex = null,
    restrict: ?TokenIndex = null,

    complexToken: ?TokenIndex = null,
    bitIntToken: ?TokenIndex = null,
    typedef: bool = false,
    typeof: bool = false,

    type: Specifier = .none,
    /// When true an error is returned instead of adding a diagnostic message.
    /// Used for trying to combine typedef types.
    errorOnInvalid: bool = false,

    pub const Specifier = union(enum) {
        None,
        Void,
        /// GNU __auto_type extension
        AutoType,
        C23Auto,
        NullptrTy,
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
        ComplexSshort,
        ComplexUshort,
        ComplexShortInt,
        ComplexSshortInt,
        ComplexUshortInt,
        ComplexInt,
        ComplexSint,
        ComplexUint,
        ComplexLong,
        ComplexSlong,
        ComplexUlong,
        ComplexLongInt,
        ComplexSlongInt,
        ComplexUlongInt,
        ComplexLongLong,
        ComplexSlongLong,
        ComplexUlongLong,
        ComplexLongLongInt,
        ComplexSlongLongInt,
        ComplexUlongLongInt,
        ComplexInt128,
        ComplexSint128,
        ComplexUint128,
        BitInt: u64,
        SBitInt: u64,
        UBitInt: u64,
        ComplexBitInt: u64,
        ComplexSbitInt: u64,
        ComplexUbitInt: u64,

        FP16,
        Float16,
        Float,
        Double,
        LongDouble,
        Float128,
        Complex,
        ComplexFloat16,
        ComplexFloat,
        ComplexDouble,
        ComplexLongDouble,
        ComplexFloat128,

        // Any not simply constructed from specifier keywords.
        other: QualType,

        pub fn str(spec: Builder.Specifier, langopts: LangOpts) ?[]const u8 {
            return switch (spec) {
                .None => unreachable,
                .Void => "void",
                .AutoType => "__auto_type",
                .C23Auto => "auto",
                .NullptrTy => "nullptr_t",
                .Bool => if (langopts.standard.atLeast(.c23)) "bool" else "_Bool",
                .Char => "char",
                .SChar => "signed char",
                .UChar => "unsigned char",
                .Unsigned => "unsigned",
                .signed => "signed",
                .short => "short",
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
                .SInt128 => "signed __int128",
                .UInt128 => "unsigned __int128",
                .ComplexChar => "_Complex char",
                .ComplexSChar => "_Complex signed char",
                .ComplexUChar => "_Complex unsigned char",
                .ComplexUnsigned => "_Complex unsigned",
                .ComplexSigned => "_Complex signed",
                .ComplexShort => "_Complex short",
                .ComplexUshort => "_Complex unsigned short",
                .ComplexSshort => "_Complex signed short",
                .ComplexShortInt => "_Complex short int",
                .ComplexSshortInt => "_Complex signed short int",
                .ComplexUshortInt => "_Complex unsigned short int",
                .ComplexInt => "_Complex int",
                .ComplexSint => "_Complex signed int",
                .ComplexUint => "_Complex unsigned int",
                .ComplexLong => "_Complex long",
                .ComplexSlong => "_Complex signed long",
                .ComplexUlong => "_Complex unsigned long",
                .ComplexLongInt => "_Complex long int",
                .ComplexSlongInt => "_Complex signed long int",
                .ComplexUlongInt => "_Complex unsigned long int",
                .ComplexLongLong => "_Complex long long",
                .ComplexSlongLong => "_Complex signed long long",
                .ComplexUlongLong => "_Complex unsigned long long",
                .ComplexLongLongInt => "_Complex long long int",
                .ComplexSlongLongInt => "_Complex signed long long int",
                .ComplexUlongLongInt => "_Complex unsigned long long int",
                .ComplexInt128 => "_Complex __int128",
                .ComplexSint128 => "_Complex signed __int128",
                .ComplexUint128 => "_Complex unsigned __int128",

                .FP16 => "__fp16",
                .Float16 => "_Float16",
                .Float => "float",
                .Double => "double",
                .LongDouble => "long double",
                .Float128 => "__float128",
                .Complex => "_Complex",
                .ComplexFloat16 => "_Complex _Float16",
                .ComplexFloat => "_Complex float",
                .ComplexDouble => "_Complex double",
                .ComplexLongDouble => "_Complex long double",
                .ComplexFloat128 => "_Complex __float128",

                else => null,
            };
        }
    };

    pub fn finish(b: Builder) Parser.Error!QualType {
        const qt: QualType = switch (b.type) {
            .None => blk: {
                if (b.parser.comp.langOpts.standard.atLeast(.c23)) {
                    try b.parser.err(.missing_type_specifier_c23);
                } else {
                    try b.parser.err(.missing_type_specifier);
                }
                break :blk .int;
            },
            .void => .void,
            .AutoType => .autoType,
            .C23Auto => .c23Auto,
            .NullptrTy => unreachable, // nullptr_t can only be accessed via typeof(nullptr)
            .Bool => .bool,
            .Char => .char,
            .SChar => .schar,
            .UChar => .uchar,

            .Unsigned => .uint,
            .Signed => .int,
            .ShortInt, .SShortInt, .Short, .SShort => .short,
            .UShort, .UShortInt => .ushort,
            .Int, .SInt => .int,
            .UInt => .uint,
            .Long, .SLong, .LongInt, .SLongInt => .long,
            .ULong, .ULongInt => .ulong,
            .LongLong, .SLongLong, .LongLongInt, .SLongLongInt => .longlong,
            .ULongLong, .ULongLongInt => .ulonglong,
            .Int128, .SInt128 => .int128,
            .UInt128 => .uint128,

            .ComplexChar,
            .ComplexSChar,
            .ComplexUChar,
            .ComplexUnsigned,
            .ComplexSigned,
            .ComplexShortInt,
            .ComplexSshortInt,
            .ComplexShort,
            .ComplexSshort,
            .ComplexUshort,
            .ComplexUshortInt,
            .ComplexInt,
            .ComplexSint,
            .ComplexUint,
            .ComplexLong,
            .ComplexSlong,
            .ComplexLongInt,
            .ComplexSlongInt,
            .ComplexUlong,
            .ComplexUlongInt,
            .ComplexLongLong,
            .ComplexSlongLong,
            .ComplexLongLongInt,
            .ComplexSlongLongInt,
            .ComplexUlongLong,
            .ComplexUlongLongInt,
            .ComplexInt128,
            .ComplexSint128,
            .ComplexUint128,
            => blk: {
                const baseQt: QualType = switch (b.type) {
                    .ComplexChar => .char,
                    .ComplexSChar => .schar,
                    .ComplexUChar => .uchar,
                    .ComplexUnsigned => .uint,
                    .ComplexSigned => .int,
                    .ComplexShortInt, .ComplexSshortInt, .ComplexShort, .ComplexSshort => .short,
                    .ComplexUshort, .ComplexUshortInt => .ushort,
                    .ComplexInt, .ComplexSint => .int,
                    .ComplexUint => .uint,
                    .ComplexLong, .ComplexSlong, .ComplexLongInt, .ComplexSlongInt => .long,
                    .ComplexUlong, .ComplexUlongInt => .ulong,
                    .ComplexLongLong, .ComplexSlongLong, .ComplexLongLongInt, .ComplexSlongLongInt => .longlong,
                    .ComplexUlongLong, .ComplexUlongLongInt => .ulonglong,
                    .ComplexInt128, .ComplexSint128 => .int128,
                    .ComplexUint128 => .uint128,
                    else => unreachable,
                };
                if (b.complexToken) |tok| try b.parser.errToken(.complex_int, tok);
                break :blk try baseQt.toComplex(b.parser.comp);
            },

            .BitInt, .SBitInt, .UBitInt, .ComplexBitInt, .ComplexUbitInt, .ComplexSbitInt => |bits| blk: {
                const unsigned = b.type == .UBitInt or b.type == .ComplexUbitInt;
                const complex = b.type == .ComplexBitInt or b.type == .ComplexUbitInt or b.type == .ComplexSbitInt;
                const complexStr = if (complex) "_Complex " else "";

                if (unsigned) {
                    if (bits < 1) {
                        try b.parser.errStr(.unsigned_bit_int_too_small, b.bitIntToken.?, complexStr);
                        return .invalid;
                    }
                } else {
                    if (bits < 2) {
                        try b.parser.errStr(.signed_bit_int_too_small, b.bitIntToken.?, complexStr);
                        return .invalid;
                    }
                }
                if (bits > Compilation.BitIntMaxBits) {
                    try b.parser.errStr(if (unsigned) .unsigned_bit_int_too_big else .signed_bit_int_too_big, b.bitIntToken.?, complexStr);
                    return .invalid;
                }
                if (b.complexToken) |tok| try b.parser.errToken(.complex_int, tok);

                const qt = try b.parser.comp.typeStore.put(b.parser.gpa, .{
                    .bitInt = .{
                        .signedness = if (unsigned) .unsigned else .signed,
                        .bits = @intCast(bits),
                    },
                });
                break :blk if (complex) try qt.toComplex(b.parser.comp) else qt;
            },

            .FP16 => .fp16,
            .Float16 => .float16,
            .Float => .float,
            .Double => .double,
            .LongDouble => .longDouble,
            .Float128 => .float128,

            .ComplexFloat16,
            .ComplexFloat,
            .ComplexDouble,
            .ComplexLongDouble,
            .ComplexFloat128,
            .Complex,
            => blk: {
                const baseQt: QualType = switch (b.type) {
                    .ComplexFloat16 => .float16,
                    .ComplexFloat => .float,
                    .ComplexDouble => .double,
                    .ComplexLongDouble => .longDouble,
                    .ComplexFloat128 => .float128,
                    .Complex => .double,
                    else => unreachable,
                };
                if (b.type == .complex) try b.parser.errToken(.plain_complex, b.parser.tokenIdx - 1);
                break :blk try baseQt.toComplex(b.parser.comp);
            },

            .other => |qt| qt,
        };
        return b.finishQuals(qt);
    }

    pub fn finishQuals(b: Builder, qt: QualType) !QualType {
        var resultQt = qt;
        if (b.@"const" != null) resultQt.@"const" = true;
        if (b.@"volatile" != null) resultQt.@"volatile" = true;

        if (b.restrict) |restrictToken| {
            switch (qt.base(b.parser.comp).type) {
                .array, .pointer => resultQt.restrict = true,
                else => {
                    try b.parser.errStr(.restrict_non_pointer, restrictToken, try b.parser.typeStr(qt));
                },
            }
        }

        if (b.atomic) |atomicToken| {
            _ = atomicToken;
            // if (qt.isArray()) try b.parser.errStr(.atomic_array, atomic_tok, try b.parser.typeStr(qt));
            // if (qt.isFunc()) try b.parser.errStr(.atomic_func, atomic_tok, try b.parser.typeStr(qt));
            // if (qt.hasIncompleteSize()) try b.parser.errStr(.atomic_incomplete, atomic_tok, try b.parser.typeStr(qt));
            // TODO erro if quals
            // TODO make atomic
        }
        return resultQt;
    }

    fn cannotCombine(b: Builder, sourceToken: TokenIndex) !void {
        const typeStr = b.type.str(b.parser.comp.langOpts) orelse try b.parser.typeStr(try b.finish());
        try b.parser.errExtra(.cannot_combine_spec, sourceToken, .{ .str = typeStr });
    }

    fn duplicateSpec(b: *Builder, sourceToken: TokenIndex, spec: []const u8) !void {
        if (b.parser.comp.langOpts.emulate != .clang) return b.cannotCombine(sourceToken);
        try b.parser.errStr(.duplicate_decl_spec, b.parser.tokenIdx, spec);
    }

    pub fn combineFromTypeof(b: *Builder, new: QualType, sourceToken: TokenIndex) Compilation.Error!void {
        if (b.typedef) return b.parser.errStr(.cannot_combine_spec, sourceToken, "type-name");
        if (b.typeof) return b.parser.errStr(.cannot_combine_spec, sourceToken, "typeof");
        if (b.type != .None) return b.parser.errStr(.cannot_combine_with_typeof, sourceToken, @tagName(b.type));
        b.type = .{ .other = new };
    }

    /// Try to combine type from typedef, returns true if successful.
    pub fn combineTypedef(b: *Builder, typedefQt: QualType) bool {
        if (b.type != .none) return false;

        b.typedef = true;
        b.type = .{ .other = typedefQt };
        return true;
    }

    pub fn combine(b: *Builder, new: Builder.Specifier, sourceToken: TokenIndex) !void {
        if (b.typeof)
            try b.parser.errStr(.cannot_combine_with_typeof, sourceToken, @tagName(new));

        if (b.typedef)
            try b.parser.errStr(.cannot_combine_spec, sourceToken, "type-name");

        switch (new) {
            .Complex => b.complexToken = sourceToken,
            .BitInt => b.bitIntToken = sourceToken,
            else => {},
        }

        if (new == .Int128 and !TargetUtil.hasInt128(b.parser.comp.target)) {
            try b.parser.errStr(.type_not_supported_on_target, sourceToken, "__int128");
        }

        b.type = switch (new) {
            else => switch (b.type) {
                .None => new,
                else => return b.cannotCombine(sourceToken),
            },
            .Signed => switch (b.type) {
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
                .ComplexShort => .ComplexSshort,
                .ComplexShortInt => .ComplexSshortInt,
                .ComplexInt => .ComplexSint,
                .ComplexLong => .ComplexSlong,
                .ComplexLongInt => .ComplexSlongInt,
                .ComplexLongLong => .ComplexSlongLong,
                .ComplexLongLongInt => .ComplexSlongLongInt,
                .ComplexInt128 => .ComplexSint128,
                .ComplexBitInt => |bits| .{ .ComplexSbitInt = bits },

                .Signed,
                .SChar,
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
                .ComplexSshort,
                .ComplexSshortInt,
                .ComplexSint,
                .ComplexSlong,
                .ComplexSlongInt,
                .ComplexSlongLong,
                .ComplexSlongLongInt,
                .ComplexSint128,
                .ComplexSbitInt,
                => return b.duplicateSpec(sourceToken, "signed"),
                else => return b.cannotCombine(sourceToken),
            },

            .Unsigned => switch (b.type) {
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
                .ComplexShort => .ComplexUshort,
                .ComplexShortInt => .ComplexUshortInt,
                .ComplexInt => .ComplexUint,
                .ComplexLong => .ComplexUlong,
                .ComplexLongInt => .ComplexUlongInt,
                .ComplexLongLong => .ComplexUlongLong,
                .ComplexLongLongInt => .ComplexUlongLongInt,
                .ComplexInt128 => .ComplexUint128,
                .ComplexBitInt => |bits| .{ .ComplexUbitInt = bits },

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
                .ComplexUshort,
                .ComplexUshortInt,
                .ComplexUint,
                .ComplexUlong,
                .ComplexUlongInt,
                .ComplexUlongLong,
                .ComplexUlongLongInt,
                .ComplexUint128,
                .ComplexUbitInt,
                => return b.duplicateSpec(sourceToken, "unsigned"),
                else => return b.cannotCombine(sourceToken),
            },

            .Char => switch (b.type) {
                .None => .Char,
                .Unsigned => .SChar,
                .Signed => .Char,
                .Complex => .ComplexChar,
                .ComplexSigned => .SChar,
                .ComplexUnsigned => .UChar,
                else => return b.cannotCombine(sourceToken),
            },

            .Short => switch (b.type) {
                .None => .Short,
                .Unsigned => .UShort,
                .Signed => .SShort,
                .Int => .ShortInt,
                .SInt => .SShortInt,
                .UInt => .UShortInt,
                .Complex => .ComplexShort,
                .ComplexSigned => .SShort,
                .ComplexUnsigned => .UShort,
                else => return b.cannotCombine(sourceToken),
            },

            .Int => switch (b.type) {
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
                .ComplexSigned => .ComplexSint,
                .ComplexUnsigned => .ComplexUint,
                .ComplexShort => .ComplexShortInt,
                .ComplexSshort => .ComplexSshortInt,
                .ComplexUshort => .ComplexUshortInt,
                .ComplexLong => .ComplexLongInt,
                .ComplexSlong => .ComplexSlongInt,
                .ComplexUlong => .ComplexUlongInt,
                .ComplexLongLong => .ComplexLongLongInt,
                .ComplexSlongLong => .ComplexSlongLongInt,
                .ComplexUlongLong => .ComplexUlongLongInt,
                else => return b.cannotCombine(sourceToken),
            },

            .Long => switch (b.type) {
                .None => .Long,
                .Double => .LongDouble,
                .Unsigned => .ULong,
                .Signed => .Long,
                .Int => .LongInt,
                .SInt => .SLongInt,
                .Long => .LongLong,
                .SLong => .SLongLong,
                .ULong => .ULongLong,
                .Complex => .ComplexLong,
                .ComplexSigned => .ComplexSlong,
                .ComplexUnsigned => .ComplexUlong,
                .ComplexLong => .ComplexLongLong,
                .ComplexSlong => .ComplexSlongLong,
                .ComplexUlong => .ComplexUlongLong,
                .ComplexDouble => .ComplexLongDouble,
                else => return b.cannotCombine(sourceToken),
            },

            .Int128 => switch (b.type) {
                .None => .Int128,
                .Unsigned => .UInt128,
                .Signed => .SInt128,
                .Complex => .ComplexInt128,
                .ComplexSigned => .ComplexSint128,
                .ComplexUnsigned => .ComplexUint128,
                else => return b.cannotCombine(sourceToken),
            },

            .BitInt => switch (b.type) {
                .None => .{ .BitInt = new.BitInt },
                .Unsigned => .{ .UBitInt = new.BitInt },
                .Signed => .{ .SBitInt = new.BitInt },
                .Complex => .{ .ComplexBitInt = new.BitInt },
                .ComplexSigned => .{ .ComplexSbitInt = new.BitInt },
                .ComplexUnsigned => .{ .ComplexUbitInt = new.BitInt },
                else => return b.cannotCombine(sourceToken),
            },

            .AutoType => switch (b.type) {
                .None => .AutoType,
                else => return b.cannotCombine(sourceToken),
            },
            .C23Auto => switch (b.type) {
                .None => .C23Auto,
                else => return b.cannotCombine(sourceToken),
            },
            .FP16 => switch (b.type) {
                .None => .FP16,
                else => return b.cannotCombine(sourceToken),
            },
            .Float16 => switch (b.type) {
                .None => .Float16,
                .Complex => .ComplexFloat16,
                else => return b.cannotCombine(sourceToken),
            },
            .Float => switch (b.type) {
                .None => .Float,
                .Complex => .ComplexFloat,
                else => return b.cannotCombine(sourceToken),
            },
            .Double => switch (b.type) {
                .None => .Double,
                .Long => .LongDouble,
                .Complex => .ComplexDouble,
                .ComplexLong => .ComplexLongDouble,
                else => return b.cannotCombine(sourceToken),
            },
            .Float128 => switch (b.type) {
                .None => .Float128,
                .Complex => .ComplexFloat128,
                else => return b.cannotCombine(sourceToken),
            },
            .Complex => switch (b.type) {
                .None => .Complex,
                .Float16 => .ComplexFloat16,
                .Float => .ComplexFloat,
                .Double => .ComplexDouble,
                .LongDouble => .ComplexLongDouble,
                .Float128 => .ComplexFloat128,
                .Char => .ComplexChar,
                .SChar => .ComplexSChar,
                .UChar => .ComplexUChar,
                .Unsigned => .ComplexUnsigned,
                .Signed => .ComplexSigned,
                .Short => .ComplexShort,
                .SShort => .ComplexSshort,
                .UShort => .ComplexUshort,
                .ShortInt => .ComplexShortInt,
                .SShortInt => .ComplexSshortInt,
                .UShortInt => .ComplexUshortInt,
                .Int => .ComplexInt,
                .SInt => .ComplexSint,
                .UInt => .ComplexUint,
                .Long => .ComplexLong,
                .SLong => .ComplexSlong,
                .ULong => .ComplexUlong,
                .LongInt => .ComplexLongInt,
                .SLongInt => .ComplexSlongInt,
                .ULongInt => .ComplexUlongInt,
                .LongLong => .ComplexLongLong,
                .SLongLong => .ComplexSlongLong,
                .ULongLong => .ComplexUlongLong,
                .LongLongInt => .ComplexLongLongInt,
                .SLongLongInt => .ComplexSlongLongInt,
                .ULongLongInt => .ComplexUlongLongInt,
                .Int128 => .ComplexInt128,
                .SInt128 => .ComplexSint128,
                .UInt128 => .ComplexUint128,

                .BitInt => |bits| .{ .ComplexBitInt = bits },
                .SBitInt => |bits| .{ .ComplexSbitInt = bits },
                .UBitInt => |bits| .{ .ComplexUbitInt = bits },

                .Complex,
                .ComplexFloat,
                .ComplexDouble,
                .ComplexLongDouble,
                .ComplexFloat128,
                .ComplexChar,
                .ComplexSChar,
                .ComplexUchar,
                .ComplexUnsigned,
                .ComplexSigned,
                .ComplexShort,
                .ComplexSshort,
                .ComplexUshort,
                .ComplexShortInt,
                .ComplexSshortInt,
                .ComplexUshortInt,
                .ComplexInt,
                .ComplexSint,
                .ComplexUint,
                .ComplexLong,
                .ComplexSlong,
                .ComplexUlong,
                .ComplexLongInt,
                .ComplexSlongInt,
                .ComplexUlongInt,
                .ComplexLongLong,
                .ComplexSlongLong,
                .ComplexUlongLong,
                .ComplexLongLongInt,
                .ComplexSlongLongInt,
                .ComplexUlongLongInt,
                .ComplexInt128,
                .ComplexSint128,
                .ComplexUint128,
                .ComplexBitInt,
                .ComplexSbitInt,
                .ComplexUbitInt,
                => return b.duplicateSpec(sourceToken, "_Complex"),
                else => return b.cannotCombine(sourceToken),
            },
        };
    }

    pub fn fromType(comp: *const Compilation, qt: QualType) Builder.Specifier {
        return switch (qt.base(comp).type) {
            .void => .Void,
            .nullptrTy => .NullptrTy,
            .bool => .Bool,
            .int => |int| switch (int) {
                .Char => .Char,
                .SChar => .SChar,
                .UChar => .UChar,
                .Short => .Short,
                .UShort => .UShort,
                .Int => .Int,
                .UInt => .UInt,
                .Long => .Long,
                .ULong => .ULong,
                .LongLong => .LongLong,
                .ULongLong => .ULongLong,
                .Int128 => .Int128,
                .UInt128 => .UInt128,
            },

            .bitInt => |bitInt| if (bitInt.signedness == .unsigned) {
                return .{ .UBitInt = bitInt.bits };
            } else {
                return .{ .BitInt = bitInt.bits };
            },

            .float => |float| switch (float) {
                .FP16 => .FP16,
                .Float16 => .Float16,
                .Float => .Float,
                .Double => .Double,
                .LongDouble => .LongDouble,
                .Float128 => .Float128,
            },
            .complex => |complex| switch (complex.base(comp).type) {
                .int => |int| switch (int) {
                    .Char => .ComplexChar,
                    .SChar => .ComplexSChar,
                    .UChar => .ComplexUChar,
                    .Short => .ComplexShort,
                    .UShort => .ComplexUshort,
                    .Int => .ComplexInt,
                    .UInt => .ComplexUint,
                    .Long => .ComplexLong,
                    .ULong => .ComplexUlong,
                    .LongLong => .ComplexLongLong,
                    .ULongLong => .ComplexUlongLong,
                    .Int128 => .ComplexInt128,
                    .UInt128 => .ComplexUint128,
                },
                .bitInt => |bitInt| if (bitInt.signedness == .unsigned) {
                    return .{ .ComplexUbitInt = bitInt.bits };
                } else {
                    return .{ .ComplexBitInt = bitInt.bits };
                },
                .float => |float| switch (float) {
                    .FP16 => unreachable,
                    .Float16 => .Float16,
                    .Float => .Float,
                    .Double => .Double,
                    .LongDouble => .LongDouble,
                    .Float128 => .Float128,
                },
                else => unreachable,
            },
            else => .{ .other = qt },
        };
    }
};
