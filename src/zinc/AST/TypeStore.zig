pub const std = @import("std");

const Attribute = @import("../Lexer//Attribute.zig");
const Compilation = @import("../Basic/Compilation.zig");
const LangOpts = @import("../Basic/LangOpts.zig");
const Parser = @import("../Parser/Parser.zig");
const RecordLayout = @import("../Basic/RecordLayout.zig");
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
        EnumIncompleteFixed,
        Typeof,
        TypeofExpr,
        Typedef,
        Attributed,
        AttributedOne,
    };
};

const Index = enum(u29) {
    /// A NaN-like poison value
    /// Can only be nested in function types.
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
    /// Special type used when combining declarators.
    DeclaratorCombine = std.math.maxInt(u29) - 28,
    _,
};

const TypeStore = @This();

/// bit: 31                           3 2 1 0
///   [        _index (29bit)     ][r][v][c]
///                                │  │  │
///                                │  │  └─ const
///                                │  └── volatile
///                                └── restrict
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

    pub fn isAutoType(qt: QualType) bool {
        return qt._index == .AutoType;
    }

    pub fn isC23Auto(qt: QualType) bool {
        return qt._index == .C23Auto;
    }

    pub fn isQualified(qt: QualType) bool {
        return qt.@"const" or qt.@"volatile" or qt.restrict;
    }

    pub fn unqualified(qt: QualType) QualType {
        return .{ ._index = qt._index };
    }

    pub fn withQualifiers(target: QualType, qualsFrom: QualType) QualType {
        return .{
            ._index = target._index,
            .@"const" = qualsFrom.@"const",
            .@"volatile" = qualsFrom.@"volatile",
            .restrict = qualsFrom.restrict,
        };
    }

    pub fn @"type"(qt: QualType, comp: *const Compilation) Type {
        switch (qt._index) {
            .Invalid => unreachable,
            .AutoType => unreachable,
            .C23Auto => unreachable,
            .DeclaratorCombine => unreachable,
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
        // std.debug.print("repr tag: {}\n", .{repr.tag});
        return switch (repr.tag) {
            .Complex => .{ .complex = @bitCast(repr.data[0]) },
            .Atomic => .{ .atomic = @bitCast(repr.data[0]) },
            .BitInt => .{
                .bitInt = .{
                    .bits = @intCast(repr.data[0]),
                    .signedness = @enumFromInt(repr.data[1]),
                },
            },

            .FuncZero => .{
                .func = .{
                    .returnType = @bitCast(repr.data[0]),
                    .kind = .Normal,
                    .params = &.{},
                },
            },
            .FuncVariadicZero => .{
                .func = .{
                    .returnType = @bitCast(repr.data[0]),
                    .kind = .Variadic,
                    .params = &.{},
                },
            },

            .FuncOldStyleZero => .{
                .func = .{
                    .returnType = @bitCast(repr.data[0]),
                    .kind = .OldStyle,
                    .params = &.{},
                },
            },

            .FuncOne,
            .FuncVariadicOne,
            .FuncOldStyleOne,
            .Func,
            .FuncVariadic,
            .FuncOldStyle,
            => {
                const paramSize = 4;
                comptime std.debug.assert(@sizeOf(Type.Func.Param) == @sizeOf(u32) * paramSize);

                const paramsLen = switch (repr.tag) {
                    .FuncOne, .FuncVariadicOne, .FuncOldStyleOne => 1,
                    .Func, .FuncVariadic, .FuncOldStyle => extra[repr.data[1]],
                    else => unreachable,
                };
                const extraParams = extra[repr.data[1] + @intFromBool(paramsLen > 1) ..][0 .. paramsLen * paramSize];

                return .{ .func = .{
                    .returnType = @bitCast(repr.data[0]),
                    .kind = switch (repr.tag) {
                        .FuncOne, .Func => .Normal,
                        .FuncVariadicOne, .FuncVariadic => .Variadic,
                        .FuncOldStyleOne, .FuncOldStyle => .OldStyle,
                        else => unreachable,
                    },
                    .params = std.mem.bytesAsSlice(Type.Func.Param, std.mem.sliceAsBytes(extraParams)),
                } };
            },

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
                const layoutSize = 5;
                const fieldSize = 10;
                comptime std.debug.assert(@sizeOf(Type.Record.Layout) == @sizeOf(u32) * layoutSize);
                comptime std.debug.assert(@sizeOf(Type.Record.Field) == @sizeOf(u32) * fieldSize);

                const layout = @as(*Type.Record.Layout, @ptrCast(extra[repr.data[1] + 1 ..][0..layoutSize])).*;
                const fieldsLen = extra[repr.data[1] + layoutSize + 1];
                const extraFields = extra[repr.data[1] + layoutSize + 2 ..][0 .. fieldsLen * fieldSize];

                const record: Type.Record = .{
                    .name = @enumFromInt(repr.data[0]),
                    .declNode = @enumFromInt(extra[repr.data[1]]),
                    .layout = layout,
                    .fields = std.mem.bytesAsSlice(Type.Record.Field, std.mem.sliceAsBytes(extraFields)),
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
                    .declNode = @enumFromInt(repr.data[1]),
                    .layout = null,
                    .fields = &.{},
                },
            },
            .UnionIncomplete => .{
                .@"union" = .{
                    .name = @enumFromInt(repr.data[0]),
                    .declNode = @enumFromInt(repr.data[1]),
                    .layout = null,
                    .fields = &.{},
                },
            },
            .Enum, .EnumFixed => {
                const fieldSize = 3;
                comptime std.debug.assert(@sizeOf(Type.Enum.Field) == @sizeOf(u32) * fieldSize);

                const fieldsLen = extra[repr.data[1] + 2];
                const extraFields = extra[repr.data[1] + 3 ..][0 .. fieldsLen * fieldSize];

                return .{
                    .@"enum" = .{
                        .name = @enumFromInt(extra[repr.data[1]]),
                        .declNode = @enumFromInt(extra[repr.data[1] + 1]),
                        .tag = @bitCast(repr.data[0]),
                        .fixed = repr.tag == .EnumFixed,
                        .incomplete = false,
                        .fields = std.mem.bytesAsSlice(Type.Enum.Field, std.mem.sliceAsBytes(extraFields)),
                    },
                };
            },
            .EnumIncomplete => .{
                .@"enum" = .{
                    .tag = null,
                    .name = @enumFromInt(repr.data[0]),
                    .declNode = @enumFromInt(repr.data[1]),
                    .incomplete = true,
                    .fixed = false,
                    .fields = &.{},
                },
            },
            .EnumIncompleteFixed => .{
                .@"enum" = .{
                    .tag = @bitCast(repr.data[0]),
                    .name = @enumFromInt(comp.typeStore.extra.items[repr.data[1]]),
                    .declNode = @enumFromInt(comp.typeStore.extra.items[repr.data[1] + 1]),
                    .incomplete = true,
                    .fixed = true,
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
                .name = @enumFromInt(comp.typeStore.extra.items[repr.data[1]]),
                .declNode = @enumFromInt(comp.typeStore.extra.items[repr.data[1] + 1]),
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

        switch (qt._index) {
            .Invalid, .AutoType, .C23Auto => return null,
            else => {},
        }

        const baseType = qt.base(comp).type;
        if (baseType == tag) return @field(baseType, @tagName(tag));
        return null;
    }

    pub fn getRecord(qt: QualType, comp: *const Compilation) ?Type.Record {
        return switch (qt.base(comp).type) {
            .@"struct", .@"union" => |record| record,
            else => null,
        };
    }

    pub fn is(qt: QualType, comp: *const Compilation, comptime tag: std.meta.Tag(Type)) bool {
        return qt.get(comp, tag) != null;
    }

    pub fn isUnsigned(qt: QualType, comp: *const Compilation) bool {
        return qt.signedness(comp) == .unsigned;
    }

    pub fn childType(qt: QualType, comp: *const Compilation) QualType {
        if (qt.isInvalid()) return .invalid;
        return switch (qt.base(comp).type) {
            .complex => |complex| complex,
            .pointer => |pointer| pointer.child,
            .array => |array| array.elem,
            .vector => |vector| vector.elem,
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

    pub const TypeSizeOrder = enum { lt, gt, eq, indeterminate };

    pub fn sizeCompare(lhs: QualType, rhs: QualType, comp: *const Compilation) TypeSizeOrder {
        const lhsSize = lhs.sizeofOrNull(comp) orelse return .indeterminate;
        const rhsSize = rhs.sizeofOrNull(comp) orelse return .indeterminate;
        return switch (std.math.order(lhsSize, rhsSize)) {
            .lt => .lt,
            .gt => .gt,
            .eq => .eq,
        };
    }

    /// Size of a type as reported by the sizeof operator.
    pub fn sizeof(qt: QualType, comp: *const Compilation) u64 {
        return qt.sizeofOrNull(comp).?;
    }

    /// Size of a type as reported by the sizeof operator.
    /// Returns null for incomplete types.
    pub fn sizeofOrNull(qt: QualType, comp: *const Compilation) ?u64 {
        if (qt.isInvalid()) return null;
        return loop: switch (qt.base(comp).type) {
            .void => 1,
            .bool => 1,
            .func => 1,
            .nullptrTy, .pointer => comp.target.ptrBitWidth() / 8,
            .int => |intTy| intTy.bits(comp) / 8,
            .float => |floatTy| floatTy.bits(comp) / 8,
            .complex => |complex| complex.sizeofOrNull(comp),
            .bitInt => |bitInt| {
                return std.mem.alignForward(u64, (@as(u32, bitInt.bits) + 7) / 8, qt.alignof(comp));
            },
            .atomic => |atomic| atomic.sizeofOrNull(comp),
            .vector => |vector| {
                const elem_size = vector.elem.sizeofOrNull(comp) orelse return null;
                return elem_size * vector.len;
            },
            .array => |array| {
                const len = switch (array.len) {
                    .variable, .unspecifiedVariable => return null,
                    .incomplete => {
                        return if (comp.langOpts.emulate == .msvc) 0 else null;
                    },
                    .fixed, .static => |len| len,
                };
                const elemSize = array.elem.sizeofOrNull(comp) orelse return null;
                const arrSize = elemSize * len;
                if (comp.langOpts.emulate == .msvc) {
                    // msvc ignores array type alignment.
                    // Since the size might not be a multiple of the field
                    // alignment, the address of the second element might not be properly aligned
                    // for the field alignment. A flexible array has size 0. See test case 0018.
                    return arrSize;
                } else {
                    return std.mem.alignForward(u64, arrSize, qt.alignof(comp));
                }
            },
            .@"struct", .@"union" => |record| {
                const layout = record.layout orelse return null;
                return layout.sizeBits / 8;
            },
            .@"enum" => |enumTy| {
                const tag = enumTy.tag orelse return null;
                continue :loop tag.base(comp).type;
            },
            .typeof => unreachable,
            .typedef => unreachable,
            .attributed => unreachable,
        };
    }

    /// Size of type in bits as it would have in a bitfield.
    pub fn bitSizeof(qt: QualType, comp: *const Compilation) u64 {
        return qt.bitSizeofOrNull(comp).?;
    }

    pub fn bitSizeofOrNull(qt: QualType, comp: *const Compilation) ?u64 {
        if (qt.isInvalid()) return null;
        return loop: switch (qt.base(comp).type) {
            .bool => if (comp.langOpts.emulate == .msvc) 8 else 1,
            .bitInt => |bitInt| bitInt.bits,
            .float => |floatTy| floatTy.bits(comp),
            .int => |intTy| intTy.bits(comp),
            .nullptrTy, .pointer => comp.target.ptrBitWidth(),
            .atomic => |atomic| continue :loop atomic.base(comp).type,
            .complex => |complex| {
                const childSize = complex.bitSizeofOrNull(comp) orelse return null;
                return childSize * 2;
            },
            else => 8 * (qt.sizeofOrNull(comp) orelse return null),
        };
    }

    pub fn hasIncompleteSize(qt: QualType, comp: *const Compilation) bool {
        if (qt.isInvalid()) return false;
        return switch (qt.base(comp).type) {
            .void => true,
            .array => |array| array.len == .incomplete,
            .@"enum" => |enumTy| enumTy.incomplete and !enumTy.fixed,
            .@"struct", .@"union" => |record| record.layout == null,
            else => false,
        };
    }

    pub fn signedness(qt: QualType, comp: *const Compilation) std.builtin.Signedness {
        return loop: switch (qt.base(comp).type) {
            .complex => |complex| continue :loop complex.base(comp).type,
            .atomic => |atomic| continue :loop atomic.base(comp).type,
            .bool => .unsigned,
            .bitInt => |bitInt| bitInt.signedness,
            .int => |intTy| switch (intTy) {
                .Char => comp.getCharSignedness(),
                .SChar, .Short, .Int, .Long, .LongLong, .Int128 => .signed,
                .UChar, .UShort, .UInt, .ULong, .ULongLong, .UInt128 => .unsigned,
            },
            // Pointer values are signed.
            .pointer, .nullptrTy => .signed,
            .@"enum" => .signed,
            else => unreachable,
        };
    }

    /// Size of a type as reported by the alignof operator.
    pub fn alignof(qt: QualType, comp: *const Compilation) u32 {
        if (qt.requestedAlignment(comp)) |requested| request: {
            if (qt.is(comp, .@"enum")) {
                if (comp.langOpts.emulate == .gcc) {
                    // gcc does not respect alignment on enums
                    break :request;
                }
            } else if (qt.getRecord(comp)) |recordTy| {
                const layout = recordTy.layout orelse return 0;

                // don't return the attribute for records
                // layout has already accounted for requested alignment
                const computed = @divExact(layout.fieldAlignmentBits, 8);
                return @max(requested, computed);
            } else if (comp.langOpts.emulate == .msvc) {
                const typeAlign = qt.base(comp).qt.alignof(comp);
                return @max(requested, typeAlign);
            }
            return requested;
        }

        return loop: switch (qt.base(comp).type) {
            .void => 1,
            .bool => 1,
            .int => |intTy| switch (intTy) {
                .Char,
                .SChar,
                .UChar,
                => 1,

                .Short => comp.target.cTypeAlignment(.short),
                .UShort => comp.target.cTypeAlignment(.ushort),
                .Int => comp.target.cTypeAlignment(.int),
                .UInt => comp.target.cTypeAlignment(.uint),

                .Long => comp.target.cTypeAlignment(.long),
                .ULong => comp.target.cTypeAlignment(.ulong),
                .LongLong => comp.target.cTypeAlignment(.longlong),
                .ULongLong => comp.target.cTypeAlignment(.ulonglong),
                .Int128, .UInt128 => if (comp.target.cpu.arch == .s390x and comp.target.os.tag == .linux and comp.target.abi.isGnu()) 8 else 16,
            },
            .float => |floatTy| switch (floatTy) {
                .Float => comp.target.cTypeAlignment(.float),
                .Double => comp.target.cTypeAlignment(.double),
                .LongDouble => comp.target.cTypeAlignment(.longdouble),
                .FP16, .Float16 => 2,
                .Float128 => 16,
            },
            .bitInt => |bitInt| {
                // https://www.open-std.org/jtc1/sc22/wg14/www/docs/n2709.pdf
                // _BitInt(N) types align with existing calling conventions. They have the same size and alignment as the
                // smallest basic type that can contain them. Types that are larger than __int64_t are conceptually treated
                // as struct of register size chunks. The number of chunks is the smallest number that can contain the type.
                if (bitInt.bits > 64) return 8;
                const basicType = comp.intLeastN(bitInt.bits, bitInt.signedness);
                return basicType.alignof(comp);
            },
            .atomic => |atomic| continue :loop atomic.base(comp).type,
            .complex => |complex| continue :loop complex.base(comp).type,

            .pointer, .nullptrTy => switch (comp.target.cpu.arch) {
                .avr => 1,
                else => comp.target.ptrBitWidth() / 8,
            },

            .func => TargetUtil.defaultFunctionAlignment(&comp.target),

            .array => |array| continue :loop array.elem.base(comp).type,
            .vector => |vector| continue :loop vector.elem.base(comp).type,

            .@"struct", .@"union" => |record| {
                const layout = record.layout orelse return 0;
                return layout.fieldAlignmentBits / 8;
            },
            .@"enum" => |enumTy| {
                const tag = enumTy.tag orelse return 0;
                continue :loop tag.base(comp).type;
            },
            .typeof => unreachable,
            .typedef => unreachable,
            .attributed => unreachable,
        };
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

    pub fn toReal(qt: QualType, comp: *const Compilation) QualType {
        return switch (qt.base(comp).type) {
            .complex => |complex| complex,
            else => qt,
        };
    }

    pub fn toComplex(qt: QualType, comp: *Compilation) !QualType {
        if (std.debug.runtime_safety) {
            switch (qt.base(comp).type) {
                .complex => unreachable,
                .float => |floatTy| if (floatTy == .FP16) unreachable,
                .int, .bitInt => {},
                else => unreachable,
            }
        }
        return comp.typeStore.put(comp.gpa, .{ .complex = qt });
    }

    pub fn decay(qt: QualType, comp: *Compilation) !QualType {
        if (qt.isInvalid()) return .invalid;
        switch (qt.base(comp).type) {
            .array => |arrayTy| {
                // Copy const and volatile to the element
                var elemQt = arrayTy.elem;
                elemQt.@"const" = qt.@"const" or elemQt.@"const";
                elemQt.@"volatile" = qt.@"volatile" or elemQt.@"volatile";

                var pointerTy = try comp.typeStore.put(comp.gpa, .{ .pointer = .{
                    .child = elemQt,
                    .decayed = qt,
                } });

                // .. and restrict to the pointer.
                pointerTy.restrict = qt.restrict or arrayTy.elem.restrict;
                return pointerTy;
            },
            .func => |funcTy| {
                if (funcTy.returnType.isInvalid())
                    return .invalid;

                for (funcTy.params) |param| {
                    if (param.qt.isInvalid()) {
                        return .invalid;
                    }
                }

                return comp.typeStore.put(comp.gpa, .{ .pointer = .{
                    .child = qt,
                    .decayed = null,
                } });
            },
            else => return qt,
        }
    }

    /// Rank for floating point conversions, ignoring domain (complex vs real)
    /// Asserts that ty is a floating point type
    pub fn floatRank(qt: QualType, comp: *const Compilation) usize {
        return loop: switch (qt.base(comp).type) {
            .float => |floatTy| switch (floatTy) {
                // TODO: bfloat16 => 0
                .Float16 => 1,
                .FP16 => 2,
                .Float => 3,
                .Double => 4,
                .LongDouble => 5,
                .Float128 => 6,
                // TODO: ibm128 => 7
            },
            .complex => |complex| continue :loop complex.base(comp).type,
            .atomic => |atomic| continue :loop atomic.base(comp).type,
            else => unreachable,
        };
    }

    /// Rank for integer conversions, ignoring domain (complex vs real)
    /// Asserts that ty is an integer type
    pub fn intRank(qt: QualType, comp: *const Compilation) usize {
        return loop: switch (qt.base(comp).type) {
            .bitInt => |bitInt| @as(usize, bitInt.bits) * 8,
            .bool => 1 + @as(usize, (QualType.bool.bitSizeof(comp) * 8)),
            .int => |intTy| switch (intTy) {
                .Char, .SChar, .UChar => 2 + (intTy.bits(comp) * 8),
                .Short, .UShort => 3 + (intTy.bits(comp) * 8),
                .Int, .UInt => 4 + (intTy.bits(comp) * 8),
                .Long, .ULong => 5 + (intTy.bits(comp) * 8),
                .LongLong, .ULongLong => 6 + (intTy.bits(comp) * 8),
                .Int128, .UInt128 => 7 + (intTy.bits(comp) * 8),
            },
            .complex => |complex| continue :loop complex.base(comp).type,
            .atomic => |atomic| continue :loop atomic.base(comp).type,
            .@"enum" => |enumTy| continue :loop enumTy.tag.?.base(comp).type,
            else => unreachable,
        };
    }

    pub fn intRankOrder(a: QualType, b: QualType, comp: *const Compilation) std.math.Order {
        std.debug.assert(a.isInt(comp) and b.isInt(comp));

        const aUnsigned = a.signedness(comp) == .unsigned;
        const bUnsigned = b.signedness(comp) == .unsigned;

        const aRank = a.intRank(comp);
        const bRank = b.intRank(comp);
        if (aUnsigned == bUnsigned)
            return std.math.order(aRank, bRank);

        if (aUnsigned) {
            if (aRank >= bRank) return .gt;
            return .lt;
        }

        std.debug.assert(bUnsigned);
        if (bRank >= aRank) return .lt;
        return .gt;
    }

    /// Returns true if `lhs` and `rhs` are integer types that differ only in sign
    pub fn sameRankDifferentSign(lhs: QualType, rhs: QualType, comp: *const Compilation) bool {
        if (!lhs.isInt(comp) or !rhs.isInt(comp)) return false;
        if (lhs.hasIncompleteSize(comp) or rhs.hasIncompleteSize(comp)) return false;
        if (lhs.intRank(comp) != rhs.intRank(comp)) return false;
        return lhs.signedness(comp) != rhs.signedness(comp);
    }

    pub fn promoteInt(qt: QualType, comp: *const Compilation) QualType {
        return loop: switch (qt.base(comp).type) {
            .bool => return .int,
            .@"enum" => |enumTy| if (enumTy.tag) |tag| continue :loop tag.base(comp).type else return .int,
            .bitInt => return qt,
            .complex => return qt, // Assume complex integer type
            .int => |intTy| switch (intTy) {
                .Char, .SChar, .UChar, .Short => .int,
                .UShort => if (Type.IntType.UChar.bits(comp) == Type.IntType.Int.bits(comp)) .uint else .int,
                else => return qt,
            },
            .atomic => |atomic| continue :loop atomic.base(comp).type,
            else => unreachable, // Not an integer type
        };
    }

    /// Promote a bitfield. If `int` can hold all the values of the underlying field,
    /// promote to int. Otherwise, promote to unsigned int
    /// Returns null if no promotion is necessary
    pub fn promoteBitfield(qt: QualType, comp: *const Compilation, width: u32) ?QualType {
        const typeSizeBits = qt.bitSizeof(comp);

        // Note: GCC and clang will promote `long: 3` to int even though the C standard does not allow this
        if (width < typeSizeBits) {
            return .int;
        }

        if (width == typeSizeBits) {
            return if (qt.signedness(comp) == .unsigned) .uint else .int;
        }

        return null;
    }

    pub const ScalarKind = enum {
        Enum,
        Bool,
        Int,
        Float,
        Pointer,
        NullptrTy,
        VoidPointer,
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

        pub fn isPointer(sk: ScalarKind) bool {
            return switch (sk) {
                .Pointer, .VoidPointer => true,
                else => false,
            };
        }

        /// Equivalent to ( isInt() or isFloat() )
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
            .int, .bitInt => return .Int,
            .float => return .Float,
            .nullptrTy => return .NullptrTy,
            .pointer => |pointer| switch (pointer.child.base(comp).type) {
                .void => return .VoidPointer,
                else => return .Pointer,
            },
            .@"enum" => return .Enum,
            .complex => |complex| switch (complex.base(comp).type) {
                .int, .bitInt => return .ComplexInt,
                .float => return .ComplexFloat,
                else => unreachable,
            },
            .atomic => |atomic| continue :loop atomic.base(comp).type,
            else => return .None,
        }
    }

    // Prefer calling scalarKind directly if checking multiple kinds.
    pub fn isInt(qt: QualType, comp: *const Compilation) bool {
        return qt.scalarKind(comp).isInt();
    }

    pub fn isRealInt(qt: QualType, comp: *const Compilation) bool {
        const sk = qt.scalarKind(comp);
        return sk.isInt() and sk.isReal();
    }

    // Prefer calling scalarKind directly if checking multiple kinds.
    pub fn isFloat(qt: QualType, comp: *const Compilation) bool {
        return qt.scalarKind(comp).isFloat();
    }

    // Prefer calling scalarKind directly if checking multiple kinds.
    pub fn isPointer(qt: QualType, comp: *const Compilation) bool {
        return qt.scalarKind(comp).isPointer();
    }

    pub fn eqlQualified(aQualTy: QualType, bQualTy: QualType, comp: *const Compilation) bool {
        if (aQualTy.@"const" != bQualTy.@"const") return false;
        if (aQualTy.@"volatile" != bQualTy.@"volatile") return false;
        if (aQualTy.restrict != bQualTy.restrict) return false;

        return aQualTy.eql(bQualTy, comp);
    }

    pub fn eql(aQualTy: QualType, bQualTy: QualType, comp: *const Compilation) bool {
        if (aQualTy.isInvalid() or bQualTy.isInvalid()) return false;
        if (aQualTy._index == bQualTy._index) return true;

        const aTypeQt = aQualTy.base(comp);
        const bTypeQt = bQualTy.base(comp);
        const aType = aTypeQt.type;
        const bType = bTypeQt.type;

        // Alignment check also guards against comparing incomplete enums to ints.
        if (aTypeQt.qt.alignof(comp) != bTypeQt.qt.alignof(comp)) return false;
        if (aType == .@"enum" and bType != .@"enum") {
            return aType.@"enum".tag.?.eql(bQualTy, comp);
        } else if (aType != .@"enum" and bType == .@"enum") {
            return bType.@"enum".tag.?.eql(aQualTy, comp);
        }

        if (std.meta.activeTag(aType) != bType) return false;

        switch (aType) {
            .void => return true,
            .bool => return true,
            .nullptrTy => return true,
            .int => |aInt| return aInt == bType.int,
            .float => |aFloat| return aFloat == bType.float,

            .complex => |aComplex| {
                const bComplex = bType.complex;
                // Complex child type cannot be qualified.
                return aComplex.eql(bComplex, comp);
            },

            .bitInt => |aBitInt| {
                const bBitInt = bType.bitInt;
                if (aBitInt.bits != bBitInt.bits) return false;
                if (aBitInt.signedness != bBitInt.signedness) return false;
                return true;
            },
            .atomic => |aAtomic| {
                const bAtomic = bType.atomic;
                // Atomic child type cannot be qualified.
                return aAtomic.eql(bAtomic, comp);
            },
            .func => |aFunc| {
                const bFunc = bType.func;

                // Function return type cannot be qualified.
                if (!aFunc.returnType.eql(bFunc.returnType, comp)) return false;

                if (aFunc.params.len == 0 and bFunc.params.len == 0)
                    return (aFunc.kind == .Variadic) == (bFunc.kind == .Variadic);

                if (aFunc.params.len != bFunc.params.len) {
                    if (aFunc.kind == .OldStyle and bFunc.kind == .OldStyle) return true;
                    if (aFunc.kind == .OldStyle or bFunc.kind == .OldStyle) {
                        const maybeHasParams = if (aFunc.kind == .OldStyle) bFunc else aFunc;

                        // Check if any args undergo default argument promotion.
                        for (maybeHasParams.params) |param| {
                            switch (param.qt.base(comp).type) {
                                .bool => return false,
                                .int => |intTy| switch (intTy) {
                                    .Char, .UChar, .SChar => return false,
                                    else => {},
                                },
                                .float => |floatTy| if (floatTy != .Double) return false,
                                .@"enum" => |enumTy| {
                                    if (comp.langOpts.emulate == .clang and enumTy.incomplete) return false;
                                },
                                else => {},
                            }
                        }
                        return true;
                    }
                    return false;
                }

                if ((aFunc.kind == .Normal) != (bFunc.kind == .Normal)) return false;

                for (aFunc.params, bFunc.params) |aParam, bParam| {
                    // Function parameters cannot be qualified.
                    if (!aParam.qt.eql(bParam.qt, comp)) return false;
                }
                return true;
            },
            .pointer => |aPointer| {
                const bPointer = bType.pointer;
                return aPointer.child.eqlQualified(bPointer.child, comp);
            },
            .array => |aArray| {
                const bArray = bType.array;
                const aLen = switch (aArray.len) {
                    .fixed, .static => |len| len,
                    else => null,
                };
                const bLen = switch (bArray.len) {
                    .fixed, .static => |len| len,
                    else => null,
                };
                if (aLen != null and bLen != null) {
                    return aLen.? == bLen.?;
                }

                // Array element qualifiers are ignored.
                return aArray.elem.eql(bArray.elem, comp);
            },
            .vector => |aVector| {
                const bVector = bType.vector;
                if (aVector.len != bVector.len) return false;

                // Vector elemnent qualifiers are checked.
                return aVector.elem.eqlQualified(bVector.elem, comp);
            },
            .@"struct", .@"union", .@"enum" => return aTypeQt.qt._index == bTypeQt.qt._index,

            .typeof => unreachable, // Never returned from base()
            .typedef => unreachable, // Never returned from base()
            .attributed => unreachable, // Never returned from base()
        }
    }

    pub fn getAttribute(qt: QualType, comp: *const Compilation, comptime tag: Attribute.Tag) ?Attribute.ArgumentsForTag(tag) {
        if (tag == .aligned) @compileError("use requestedAlignment");
        var it = Attribute.Iterator.initType(qt, comp);
        while (it.next()) |item| {
            const attribute, _ = item;
            if (attribute.tag == tag) return @field(attribute.args, @tagName(tag));
        }
        return null;
    }

    pub fn hasAttribute(qt: QualType, comp: *const Compilation, tag: Attribute.Tag) bool {
        var it = Attribute.Iterator.initType(qt, comp);
        while (it.next()) |item| {
            const attr, _ = item;
            if (attr.tag == tag) return true;
        }
        return false;
    }

    pub fn alignable(qt: QualType, comp: *const Compilation) bool {
        if (qt.isInvalid()) return true; // avoid redunant error.
        const baseType = qt.base(comp);
        return switch (baseType.type) {
            .array, .void => false,
            else => !baseType.qt.hasIncompleteSize(comp),
        };
    }

    pub fn requestedAlignment(qt: QualType, comp: *const Compilation) ?u32 {
        return annotationAlignment(comp, Attribute.Iterator.initType(qt, comp));
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
            const requested = if (attribute.args.aligned.alignment) |alignment| alignment.requested else TargetUtil.defaultAlignment(&comp.target);
            if (maxRequested == null or maxRequested.? < requested) {
                maxRequested = requested;
            }
        }
        return maxRequested;
    }

    pub fn enumIsPacked(qt: QualType, comp: *const Compilation) bool {
        std.debug.assert(qt.is(comp, .@"enum"));
        return comp.langOpts.shortEnums or TargetUtil.packAllEnums(&comp.target) or qt.hasAttribute(comp, .@"packed");
    }

    pub fn shouldDesugar(qt: QualType, comp: *const Compilation) bool {
        loop: switch (qt.type(comp)) {
            .attributed => |attributed| continue :loop attributed.base.type(comp),
            .pointer => |pointer| continue :loop pointer.child.type(comp),
            .func => |func| {
                for (func.params) |param| {
                    if (param.qt.shouldDesugar(comp)) return true;
                }
                continue :loop func.returnType.type(comp);
            },
            .typeof => return true,
            .typedef => |typedef| return !typedef.base.is(comp, .nullptrTy),
            else => return false,
        }
    }

    pub fn print(qt: QualType, comp: *const Compilation, w: *std.Io.Writer) std.Io.Writer.Error!void {
        if (qt.isC23Auto()) {
            try w.writeAll("auto");
            return;
        }
        _ = try qt.printPrologue(comp, false, w);
        try qt.printEpilogue(comp, false, w);
    }

    pub fn printNamed(
        qt: QualType,
        name: []const u8,
        comp: *const Compilation,
        w: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        if (qt.isC23Auto()) {
            try w.print("auto {s}", .{name});
            return;
        }
        const simple = try qt.printPrologue(comp, false, w);
        if (simple) try w.writeByte(' ');
        try w.writeAll(name);
        try qt.printEpilogue(comp, false, w);
    }

    pub fn printDesugared(qt: QualType, comp: *const Compilation, w: *std.Io.Writer) std.Io.Writer.Error!void {
        _ = try qt.printPrologue(comp, true, w);
        try qt.printEpilogue(comp, true, w);
    }

    fn printPrologue(qt: QualType, comp: *const Compilation, desugar: bool, w: *std.Io.Writer) std.Io.Writer.Error!bool {
        loop: switch (qt.type(comp)) {
            .pointer => |pointer| {
                const simple = try pointer.child.printPrologue(comp, desugar, w);
                if (simple) try w.writeByte(' ');
                switch (pointer.child.base(comp).type) {
                    .func, .array => try w.writeByte('('),
                    else => {},
                }
                try w.writeByte('*');
                if (qt.@"const") try w.writeAll("const");
                if (qt.@"volatile") {
                    if (qt.@"const") try w.writeByte(' ');
                    try w.writeAll("volatile");
                }
                if (qt.restrict) {
                    if (qt.@"const" or qt.@"volatile") try w.writeByte(' ');
                    try w.writeAll("restrict");
                }
                return false;
            },
            .func => |func| {
                const simple = try func.returnType.printPrologue(comp, desugar, w);
                if (simple) try w.writeByte(' ');
                return false;
            },
            .array => |array| {
                const simple = try array.elem.printPrologue(comp, desugar, w);
                if (simple) try w.writeByte(' ');
                return false;
            },
            .typeof => |typeof| if (desugar) {
                continue :loop typeof.base.type(comp);
            } else {
                try w.writeAll("typeof(");
                try typeof.base.print(comp, w);
                try w.writeAll(")");
                return true;
            },
            .typedef => |typedef| if (desugar) {
                continue :loop typedef.base.type(comp);
            } else {
                try w.writeAll(typedef.name.lookup(comp));
                return true;
            },
            .attributed => |attributed| continue :loop attributed.base.type(comp),
            else => {},
        }
        if (qt.@"const") try w.writeAll("const ");
        if (qt.@"volatile") try w.writeAll("volatile ");

        switch (qt.base(comp).type) {
            .pointer => unreachable,
            .func => unreachable,
            .array => unreachable,
            .typeof => unreachable,
            .typedef => unreachable,
            .attributed => unreachable,

            .void => try w.writeAll("void"),
            .bool => try w.writeAll(if (comp.langOpts.standard.atLeast(.c23)) "bool" else "_Bool"),
            .nullptrTy => try w.writeAll("nullptr_t"),
            .int => |intType| switch (intType) {
                .Char => try w.writeAll("char"),
                .SChar => try w.writeAll("signed char"),
                .UChar => try w.writeAll("unsigned char"),
                .Short => try w.writeAll("short"),
                .UShort => try w.writeAll("unsigned short"),
                .Int => try w.writeAll("int"),
                .UInt => try w.writeAll("unsigned int"),
                .Long => try w.writeAll("long"),
                .ULong => try w.writeAll("unsigned long"),
                .LongLong => try w.writeAll("long long"),
                .ULongLong => try w.writeAll("unsigned long long"),
                .Int128 => try w.writeAll("__int128"),
                .UInt128 => try w.writeAll("unsigned __int128"),
            },
            .bitInt => |bitInt| try w.print("{s} _BitInt({d})", .{ @tagName(bitInt.signedness), bitInt.bits }),
            .float => |floatTy| switch (floatTy) {
                .FP16 => try w.writeAll("__fp16"),
                .Float16 => try w.writeAll("_Float16"),
                .Float => try w.writeAll("float"),
                .Double => try w.writeAll("double"),
                .LongDouble => try w.writeAll("long double"),
                .Float128 => try w.writeAll("__float128"),
            },
            .complex => |complex| {
                try w.writeAll("_Complex ");
                _ = try complex.printPrologue(comp, desugar, w);
            },
            .atomic => |atomic| {
                try w.writeAll("_Atomic(");
                _ = try atomic.printPrologue(comp, desugar, w);
                try atomic.printEpilogue(comp, desugar, w);
                try w.writeAll(")");
            },

            .vector => |vector| {
                try w.print("__attribute__((__vector_size__({d} * sizeof(", .{vector.len});
                _ = try vector.elem.printPrologue(comp, desugar, w);
                try w.writeAll(")))) ");
                _ = try vector.elem.printPrologue(comp, desugar, w);
            },

            .@"struct" => |structTy| try w.print("struct {s}", .{structTy.name.lookup(comp)}),
            .@"union" => |unionTy| try w.print("union {s}", .{unionTy.name.lookup(comp)}),
            .@"enum" => |enumTy| if (enumTy.fixed) {
                try w.print("enum {s}: ", .{enumTy.name.lookup(comp)});
                _ = try enumTy.tag.?.printPrologue(comp, desugar, w);
            } else {
                try w.print("enum {s}", .{enumTy.name.lookup(comp)});
            },
        }
        return true;
    }

    fn printEpilogue(qt: QualType, comp: *const Compilation, desugar: bool, w: *std.Io.Writer) std.Io.Writer.Error!void {
        loop: switch (qt.type(comp)) {
            .pointer => |pointer| {
                switch (pointer.child.base(comp).type) {
                    .func, .array => try w.writeByte(')'),
                    else => {},
                }
                continue :loop pointer.child.type(comp);
            },
            .func => |func| {
                try w.writeByte('(');
                for (func.params, 0..) |param, i| {
                    if (i != 0) try w.writeAll(", ");
                    _ = try param.qt.printPrologue(comp, desugar, w);
                    try param.qt.printEpilogue(comp, desugar, w);
                }
                if (func.kind != .Normal) {
                    if (func.params.len != 0) try w.writeAll(", ");
                    try w.writeAll("...");
                } else if (func.params.len == 0 and !comp.langOpts.standard.atLeast(.c23)) {
                    try w.writeAll("void");
                }
                try w.writeByte(')');
                continue :loop func.returnType.type(comp);
            },
            .array => |array| {
                try w.writeByte('[');
                switch (array.len) {
                    .fixed, .static => |len| try w.print("{d}", .{len}),
                    .incomplete => {},
                    .unspecifiedVariable => try w.writeByte('*'),
                    .variable => try w.writeAll("<expr>"),
                }

                const static = array.len == .static;
                if (static) try w.writeAll("static");
                if (qt.@"const") {
                    if (static) try w.writeByte(' ');
                    try w.writeAll("const");
                }
                if (qt.@"volatile") {
                    if (static or qt.@"const") try w.writeByte(' ');
                    try w.writeAll("volatile");
                }
                if (qt.restrict) {
                    if (static or qt.@"const" or qt.@"volatile") try w.writeByte(' ');
                    try w.writeAll("restrict");
                }
                try w.writeByte(']');

                continue :loop array.elem.type(comp);
            },
            .attributed => |attributed| continue :loop attributed.base.type(comp),
            else => {},
        }
    }

    pub fn dump(qt: QualType, comp: *const Compilation, w: *std.Io.Writer) std.Io.Writer.Error!void {
        if (qt.@"const") try w.writeAll("const ");
        if (qt.@"volatile") try w.writeAll("volatile ");
        if (qt.restrict) try w.writeAll("restrict ");
        if (qt.isInvalid()) return w.writeAll("invalid");
        switch (qt.type(comp)) {
            .pointer => |pointer| {
                if (pointer.decayed) |decayed| {
                    try w.writeAll("decayed *");
                    try decayed.dump(comp, w);
                } else {
                    try w.writeAll("*");
                    try pointer.child.dump(comp, w);
                }
            },
            .func => |func| {
                if (func.kind == .OldStyle)
                    try w.writeAll("kr (")
                else
                    try w.writeAll("fn (");

                for (func.params, 0..) |param, i| {
                    if (i != 0) try w.writeAll(", ");
                    if (param.name != .empty) try w.print("{s}: ", .{param.name.lookup(comp)});
                    try param.qt.dump(comp, w);
                }
                if (func.kind != .Normal) {
                    if (func.params.len != 0) try w.writeAll(", ");
                    try w.writeAll("...");
                }
                try w.writeAll(") ");
                try func.returnType.dump(comp, w);
            },
            .array => |array| {
                switch (array.len) {
                    .fixed => |len| try w.print("[{d}]", .{len}),
                    .static => |len| try w.print("[static {d}]", .{len}),
                    .incomplete => try w.writeAll("[]"),
                    .unspecifiedVariable => try w.writeAll("[*]"),
                    .variable => try w.writeAll("[<expr>]"),
                }
                try array.elem.dump(comp, w);
            },
            .vector => |vector| {
                try w.print("vector({d}, ", .{vector.len});
                try vector.elem.dump(comp, w);
                try w.writeAll(")");
            },
            .typeof => |typeof| {
                try w.writeAll("typeof(");
                if (typeof.expr != null) try w.writeAll("<expr>: ");
                try typeof.base.dump(comp, w);
                try w.writeAll(")");
            },
            .attributed => |attributed| {
                try w.writeAll("attributed(");
                try attributed.base.dump(comp, w);
                try w.writeAll(")");
            },
            .typedef => |typedef| {
                try w.writeAll(typedef.name.lookup(comp));
                try w.writeAll(": ");
                try typedef.base.dump(comp, w);
            },
            .@"enum" => |enumTy| {
                try w.print("enum {s}: ", .{enumTy.name.lookup(comp)});
                if (enumTy.tag) |some| {
                    try some.dump(comp, w);
                } else {
                    try w.writeAll("<incomplete>");
                }
            },
            else => try qt.unqualified().print(comp, w),
        }
    }
};

pub const Type = union(enum) {
    void,
    bool,
    /// C23 nullptr_t
    nullptrTy,

    int: IntType,
    float: FloatType,
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

    pub const IntType = enum {
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

        pub fn bits(int: IntType, comp: *const Compilation) u16 {
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

    pub const FloatType = enum {
        FP16,
        Float16,
        Float,
        Double,
        LongDouble,
        Float128,

        pub fn bits(float: FloatType, comp: *const Compilation) u16 {
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
            nameToken: TokenIndex,
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
        declNode: Node.Index,
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

        pub fn isAnonymous(record: Record, comp: *const Compilation) bool {
            // anonymous records can be recognized by their names which are in
            // the format "(anonymous TAG at path:line:col)".
            return record.name.lookup(comp)[0] == '(';
        }

        pub fn hasField(record: Record, comp: *const Compilation, name: StringId) bool {
            std.debug.assert(record.layout != null);
            std.debug.assert(name != .empty);
            for (record.fields) |field| {
                if (name == field.name) return true;
                if (field.nameToken == 0) if (field.qt.getRecord(comp)) |fieldRecordTy| {
                    if (fieldRecordTy.hasField(comp, name)) return true;
                };
            }
            return false;
        }
    };

    pub const Enum = struct {
        /// null if the enum is incomplete and not fixed
        tag: ?QualType,
        fixed: bool,
        incomplete: bool,
        name: StringId,
        declNode: Node.Index,
        fields: []const Field,

        pub const Field = extern struct {
            qt: QualType,
            name: StringId,
            nameToken: TokenIndex,
        };
    };

    pub const TypeOf = struct {
        base: QualType,
        expr: ?Node.Index,
    };

    pub const TypeDef = struct {
        base: QualType,
        name: StringId,
        declNode: Node.Index,
    };

    pub const Attributed = struct {
        base: QualType,
        attributes: []const Attribute,
    };
};

types: std.MultiArrayList(Repr) = .empty,
extra: std.ArrayList(u32) = .empty,
attributes: std.ArrayList(Attribute) = .empty,
annoNameArena: std.heap.ArenaAllocator.State = .{},

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
    ts.annoNameArena.promote(gpa).deinit();
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

            const paramSize = 4;
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
                    0 => .FuncOldStyleZero,
                    1 => .FuncOldStyleOne,
                    else => .FuncOldStyle,
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
                repr.data[1] = @intFromEnum(record.declNode);
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
            try ts.extra.ensureUnusedCapacity(gpa, record.fields.len * fieldSize + layoutSize + 2);

            ts.extra.appendAssumeCapacity(@intFromEnum(record.declNode));
            const castedLayout: *const [layoutSize]u32 = @ptrCast(&layout);
            ts.extra.appendSliceAssumeCapacity(castedLayout);
            ts.extra.appendAssumeCapacity(@intCast(record.fields.len));

            for (record.fields) |*field| {
                const casted: *const [fieldSize]u32 = @ptrCast(field);
                ts.extra.appendSliceAssumeCapacity(casted);
            }
        },
        .@"enum" => |@"enum"| @"enum": {
            if (@"enum".incomplete) {
                std.debug.assert(@"enum".fields.len == 0);
                if (@"enum".fixed) {
                    repr.tag = .EnumIncompleteFixed;
                    repr.data[0] = @bitCast(@"enum".tag.?);
                    repr.data[1] = @intCast(ts.extra.items.len);
                    try ts.extra.appendSlice(gpa, &.{
                        @intFromEnum(@"enum".name),
                        @intFromEnum(@"enum".declNode),
                    });
                } else {
                    repr.tag = .EnumIncomplete;
                    repr.data[0] = @intFromEnum(@"enum".name);
                    repr.data[1] = @intFromEnum(@"enum".declNode);
                }
                break :@"enum";
            }
            repr.tag = if (@"enum".fixed) .EnumFixed else .Enum;
            repr.data[0] = @bitCast(@"enum".tag.?);

            const extraIndex: u32 = @intCast(ts.extra.items.len);
            repr.data[1] = extraIndex;

            const fieldSize = 3;
            comptime std.debug.assert(@sizeOf(Type.Enum.Field) == @sizeOf(u32) * fieldSize);
            try ts.extra.ensureUnusedCapacity(gpa, @"enum".fields.len * fieldSize + 3);

            ts.extra.appendAssumeCapacity(@intFromEnum(@"enum".name));
            ts.extra.appendAssumeCapacity(@intFromEnum(@"enum".declNode));
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
            repr.data[1] = @intCast(ts.extra.items.len);
            try ts.extra.appendSlice(gpa, &.{
                @intFromEnum(typedef.name),
                @intFromEnum(typedef.declNode),
            });
        },
        .attributed => |attributed| {
            repr.data[0] = @bitCast(attributed.base);

            const attrIndex: u32 = @intCast(ts.attributes.items.len);
            const attrCount: u32 = @intCast(attributed.attributes.len);
            try ts.attributes.appendSlice(gpa, attributed.attributes);
            if (attrCount > 1) {
                repr.tag = .Attributed;
                const extraIndex: u32 = @intCast(ts.extra.items.len);
                repr.data[1] = extraIndex;
                try ts.extra.appendSlice(gpa, &.{ attrIndex, attrCount });
            } else {
                repr.tag = .AttributedOne;
                repr.data[1] = attrIndex;
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

    const target = &comp.target;
    ts.intmax = TargetUtil.intMaxType(target);
    ts.intptr = TargetUtil.intPtrType(target);
    ts.int16 = TargetUtil.int16Type(target);
    ts.int64 = TargetUtil.int64Type(target);
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
        .declNode = undefined, //TODO
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
    const Kind = enum {
        aarch64_va_list,
        arm_va_list,
        hexagon_va_list,
        powerpc_va_list,
        s390x_va_list,
        x86_64_va_list,
        xtensa_va_list,
    };

    const kind: Kind = switch (comp.target.cpu.arch) {
        .amdgcn,
        .msp430,
        .nvptx,
        .nvptx64,
        .powerpc64,
        .powerpc64le,
        .x86,
        => return .charPointer,

        .arc,
        .avr,
        .bpfel,
        .bpfeb,
        .csky,
        .lanai,
        .loongarch32,
        .loongarch64,
        .m68k,
        .mips,
        .mipsel,
        .mips64,
        .mips64el,
        .riscv32,
        .riscv32be,
        .riscv64,
        .riscv64be,
        .sparc,
        .sparc64,
        .spirv32,
        .spirv64,
        .ve,
        .wasm32,
        .wasm64,
        .xcore,
        => return .voidPointer,

        .aarch64, .aarch64_be => switch (comp.target.os.tag) {
            .driverkit, .ios, .macos, .tvos, .visionos, .watchos, .windows => return .charPointer,
            else => .aarch64_va_list,
        },

        .arm, .armeb, .thumb, .thumbeb => .arm_va_list,

        .hexagon => if (comp.target.abi.isMusl())
            .hexagon_va_list
        else
            return .charPointer,

        .powerpc, .powerpcle => .powerpc_va_list,

        .x86_64 => switch (comp.target.os.tag) {
            .uefi, .windows => return .charPointer,
            else => .x86_64_va_list,
        },
        .xtensa => .xtensa_va_list,
        else => return .void, // unknown
    };

    const structQt = switch (kind) {
        .aarch64_va_list => blk: {
            var record: Type.Record = .{
                .name = try comp.internString("__va_list_tag"),
                .declNode = undefined, //TODO
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

            break :blk qt;
        },
        .arm_va_list => blk: {
            var record: Type.Record = .{
                .name = try comp.internString("__va_list_tag"),
                .declNode = undefined, // TODO
                .layout = null,
                .fields = &.{},
            };
            const qt = try ts.put(comp.gpa, .{ .@"struct" = record });

            var fields: [1]Type.Record.Field = .{
                .{ .name = try comp.internString("__ap"), .qt = .voidPointer },
            };
            record.fields = &fields;
            record.layout = RecordLayout.compute(&fields, qt, comp, null) catch unreachable;
            try ts.set(comp.gpa, .{ .@"struct" = record }, @intFromEnum(qt._index));

            break :blk qt;
        },
        .hexagon_va_list => blk: {
            var record: Type.Record = .{
                .name = try comp.internString("__va_list_tag"),
                .declNode = undefined, // TODO
                .layout = null,
                .fields = &.{},
            };
            const qt = try ts.put(comp.gpa, .{ .@"struct" = record });

            var fields: [4]Type.Record.Field = .{
                .{ .name = try comp.internString("__gpr"), .qt = .long },
                .{ .name = try comp.internString("__fpr"), .qt = .long },
                .{ .name = try comp.internString("__overflow_arg_area"), .qt = .voidPointer },
                .{ .name = try comp.internString("__reg_save_area"), .qt = .voidPointer },
            };
            record.fields = &fields;
            record.layout = RecordLayout.compute(&fields, qt, comp, null) catch unreachable;
            try ts.set(comp.gpa, .{ .@"struct" = record }, @intFromEnum(qt._index));

            break :blk qt;
        },
        .powerpc_va_list => blk: {
            var record: Type.Record = .{
                .name = try comp.internString("__va_list_tag"),
                .declNode = undefined, // TODO
                .layout = null,
                .fields = &.{},
            };
            const qt = try ts.put(comp.gpa, .{ .@"struct" = record });

            var fields: [5]Type.Record.Field = .{
                .{ .name = try comp.internString("gpr"), .qt = .uchar },
                .{ .name = try comp.internString("fpr"), .qt = .uchar },
                .{ .name = try comp.internString("reserved"), .qt = .ushort },
                .{ .name = try comp.internString("overflow_arg_area"), .qt = .voidPointer },
                .{ .name = try comp.internString("reg_save_area"), .qt = .voidPointer },
            };
            record.fields = &fields;
            record.layout = RecordLayout.compute(&fields, qt, comp, null) catch unreachable;
            try ts.set(comp.gpa, .{ .@"struct" = record }, @intFromEnum(qt._index));

            break :blk qt;
        },
        .s390x_va_list => blk: {
            var record: Type.Record = .{
                .name = try comp.internString("__va_list_tag"),
                .declNode = undefined, // TODO
                .layout = null,
                .fields = &.{},
            };
            const qt = try ts.put(comp.gpa, .{ .@"struct" = record });

            var fields: [3]Type.Record.Field = .{
                .{ .name = try comp.internString("__current_saved_reg_area_pointer"), .qt = .voidPointer },
                .{ .name = try comp.internString("__saved_reg_area_end_pointer"), .qt = .voidPointer },
                .{ .name = try comp.internString("__overflow_area_pointer"), .qt = .voidPointer },
            };
            record.fields = &fields;
            record.layout = RecordLayout.compute(&fields, qt, comp, null) catch unreachable;
            try ts.set(comp.gpa, .{ .@"struct" = record }, @intFromEnum(qt._index));

            break :blk qt;
        },
        .x86_64_va_list => blk: {
            var record: Type.Record = .{
                .name = try comp.internString("__va_list_tag"),
                .declNode = undefined, //TODO
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

            break :blk qt;
        },
        .xtensa_va_list => blk: {
            var record: Type.Record = .{
                .name = try comp.internString("__va_list_tag"),
                .declNode = undefined, // TODO
                .layout = null,
                .fields = &.{},
            };
            const qt = try ts.put(comp.gpa, .{ .@"struct" = record });

            var fields: [3]Type.Record.Field = .{
                .{ .name = try comp.internString("__va_stk"), .qt = .intPointer },
                .{ .name = try comp.internString("__va_reg"), .qt = .intPointer },
                .{ .name = try comp.internString("__va_ndx"), .qt = .int },
            };
            record.fields = &fields;
            record.layout = RecordLayout.compute(&fields, qt, comp, null) catch unreachable;
            try ts.set(comp.gpa, .{ .@"struct" = record }, @intFromEnum(qt._index));

            break :blk qt;
        },
    };

    return ts.put(comp.gpa, .{ .array = .{
        .elem = structQt,
        .len = .{ .fixed = 1 },
    } });
}

pub const Builder = struct {
    parser: *Parser,

    @"const": ?TokenIndex = null,
    /// _Atomic
    atomic: ?TokenIndex = null,
    @"volatile": ?TokenIndex = null,
    restrict: ?TokenIndex = null,

    unaligned: ?TokenIndex = null,
    nullability: union(enum) {
        none,
        nonnull: TokenIndex,
        nullable: TokenIndex,
        nullableResult: TokenIndex,
        nullUnspecified: TokenIndex,
    } = .none,

    complexToken: ?TokenIndex = null,
    bitIntToken: ?TokenIndex = null,
    typedef: bool = false,
    typeof: bool = false,
    /// _Atomic(type)
    atomicType: ?TokenIndex = null,

    type: Specifier = .None,
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

        pub fn toString(spec: Builder.Specifier, langopts: LangOpts) ?[]const u8 {
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
                    try b.parser.err(.missing_type_specifier_c23, b.parser.tokenIdx, .{});
                } else {
                    try b.parser.err(.missing_type_specifier, b.parser.tokenIdx, .{});
                }
                break :blk .int;
            },
            .Void => .void,
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
                if (b.complexToken) |tok| try b.parser.err(.complex_int, tok, .{});
                break :blk try baseQt.toComplex(b.parser.comp);
            },

            .BitInt, .SBitInt, .UBitInt, .ComplexBitInt, .ComplexUbitInt, .ComplexSbitInt => |bits| blk: {
                const unsigned = b.type == .UBitInt or b.type == .ComplexUbitInt;
                const complex = b.type == .ComplexBitInt or b.type == .ComplexUbitInt or b.type == .ComplexSbitInt;
                const complexStr = if (complex) "_Complex " else "";

                if (unsigned) {
                    if (bits < 1) {
                        try b.parser.err(.unsigned_bit_int_too_small, b.bitIntToken.?, .{complexStr});
                        return .invalid;
                    }
                } else {
                    if (bits < 2) {
                        try b.parser.err(.signed_bit_int_too_small, b.bitIntToken.?, .{complexStr});
                        return .invalid;
                    }
                }
                if (bits > Compilation.BitIntMaxBits) {
                    try b.parser.err(if (unsigned) .unsigned_bit_int_too_big else .signed_bit_int_too_big, b.bitIntToken.?, .{complexStr});
                    return .invalid;
                }
                if (b.complexToken) |tok| try b.parser.err(.complex_int, tok, .{});

                const qt = try b.parser.comp.typeStore.put(b.parser.comp.gpa, .{
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
                if (b.type == .Complex) try b.parser.err(.plain_complex, b.parser.tokenIdx - 1, .{});
                break :blk try baseQt.toComplex(b.parser.comp);
            },

            .other => |qt| qt,
        };
        return b.finishQuals(qt);
    }

    pub fn finishQuals(b: Builder, qt: QualType) !QualType {
        if (qt.isInvalid()) return .invalid;

        const parser = b.parser;
        const comp = parser.comp;
        const gpa = comp.gpa;

        var resultQt = qt;
        if (b.atomicType orelse b.atomic) |atomicToken| {
            if (resultQt.isAutoType()) return parser.todo("_Atomic __auto_type");
            if (resultQt.isC23Auto()) {
                try b.parser.err(.atomic_auto, atomicToken, .{});
                return .invalid;
            }
            if (resultQt.hasIncompleteSize(comp)) {
                try parser.err(.atomic_incomplete, atomicToken, .{qt});
                return .invalid;
            }
            switch (resultQt.base(comp).type) {
                .array => {
                    try parser.err(.atomic_array, atomicToken, .{qt});
                    return .invalid;
                },
                .func => {
                    try parser.err(.atomic_func, atomicToken, .{qt});
                    return .invalid;
                },
                .atomic => {
                    try parser.err(.atomic_atomic, atomicToken, .{qt});
                    return .invalid;
                },
                .complex => {
                    try parser.err(.atomic_complex, atomicToken, .{qt});
                    return .invalid;
                },
                else => {
                    resultQt = try comp.typeStore.put(gpa, .{ .atomic = resultQt });
                },
            }
        }

        const isPtr = qt.isAutoType() or qt.isC23Auto() or qt.base(comp).type == .pointer;

        if (b.unaligned != null and !isPtr) {
            resultQt = (try comp.typeStore.put(gpa, .{
                .attributed = .{
                    .base = resultQt,
                    .attributes = &.{.{
                        .tag = .unaligned,
                        .args = .{ .unaligned = .{} },
                        .syntax = .keyword,
                    }},
                },
            })).withQualifiers(resultQt);
        }

        switch (b.nullability) {
            .none => {},
            .nonnull, .nullable, .nullableResult, .nullUnspecified => |token| {
                if (!isPtr) {
                    try b.parser.err(.invalid_nullability, token, .{qt});
                }
            },
        }

        if (b.@"const" != null) resultQt.@"const" = true;
        if (b.@"volatile" != null) resultQt.@"volatile" = true;

        if (b.restrict) |restrictToken| {
            if (resultQt.isAutoType()) return parser.todo("restrict __auto_type");
            if (resultQt.isC23Auto()) {
                try parser.err(.restrict_non_pointer, restrictToken, .{});
                return resultQt;
            }
            switch (qt.base(comp).type) {
                .array, .pointer => resultQt.restrict = true,
                else => {
                    try parser.err(.restrict_non_pointer, restrictToken, .{qt});
                },
            }
        }

        return resultQt;
    }

    fn cannotCombine(b: Builder, sourceToken: TokenIndex) !void {
        if (b.type.toString(b.parser.comp.langOpts)) |some| {
            return b.parser.err(.cannot_combine_spec, sourceToken, .{some});
        }
        try b.parser.err(.cannot_combine_spec_qt, sourceToken, .{try b.finish()});
    }

    fn duplicateSpec(b: *Builder, sourceToken: TokenIndex, spec: []const u8) !void {
        if (b.parser.comp.langOpts.emulate != .clang) return b.cannotCombine(sourceToken);
        try b.parser.err(.duplicate_decl_spec, b.parser.tokenIdx, .{spec});
    }

    pub fn combineFromTypeof(b: *Builder, new: QualType, sourceToken: TokenIndex) Compilation.Error!void {
        if (b.atomicType != null) return b.parser.err(.cannot_combine_spec, sourceToken, .{"_Atomic"});
        if (b.typedef) return b.parser.err(.cannot_combine_spec, sourceToken, .{"type-name"});
        if (b.typeof) return b.parser.err(.cannot_combine_spec, sourceToken, .{"typeof"});
        if (b.type != .None) return b.parser.err(
            .cannot_combine_with_typeof,
            sourceToken,
            .{if (b.type.toString(b.parser.comp.langOpts)) |str| str else @tagName(b.type)},
        );
        b.typeof = true;
        b.type = .{ .other = new };
    }

    pub fn combineAtomic(b: *Builder, baseQt: QualType, sourceToken: TokenIndex) !void {
        if (b.atomicType != null) return b.parser.err(.cannot_combine_spec, sourceToken, .{"_Atomic"});
        if (b.typedef) return b.parser.err(.cannot_combine_spec, sourceToken, .{"type-name"});
        if (b.typeof) return b.parser.err(.cannot_combine_spec, sourceToken, .{"typeof"});

        const newSpec = TypeStore.Builder.fromType(b.parser.comp, baseQt);
        try b.combine(newSpec, sourceToken);

        b.atomicType = sourceToken;
    }

    /// Try to combine type from typedef, returns true if successful.
    pub fn combineTypedef(b: *Builder, typedefQt: QualType) bool {
        if (b.type != .None) return false;

        b.typedef = true;
        b.type = .{ .other = typedefQt };
        return true;
    }

    pub fn combine(b: *Builder, new: Builder.Specifier, sourceToken: TokenIndex) !void {
        if (b.typeof)
            return b.parser.err(
                .cannot_combine_with_typeof,
                sourceToken,
                .{if (new.toString(b.parser.comp.langOpts)) |str| str else @tagName(new)},
            );

        if (b.atomicType != null)
            return b.parser.err(.cannot_combine_spec, sourceToken, .{"_Atomic"});

        if (b.typedef)
            return b.parser.err(.cannot_combine_spec, sourceToken, .{"type-name"});

        if (b.type == .other and b.type.other.isInvalid()) return;

        switch (new) {
            .Complex => b.complexToken = sourceToken,
            .BitInt => b.bitIntToken = sourceToken,
            else => {},
        }

        if (new == .Int128 and !TargetUtil.hasInt128(&b.parser.comp.target)) {
            try b.parser.err(.type_not_supported_on_target, sourceToken, .{"__int128"});
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
                .Unsigned => .UChar,
                .Signed => .SChar,
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

            .LongLong => switch (b.type) {
                .None => .LongLong,
                .Unsigned => .ULongLong,
                .Signed => .SLongLong,
                .Int => .LongLongInt,
                .SInt => .SLongLongInt,
                .Long => .LongLong,
                .SLong => .SLongLong,
                .ULong => .ULongLong,
                .Complex => .ComplexLong,
                .ComplexSigned => .ComplexSlongLong,
                .ComplexUnsigned => .ComplexUlongLong,
                .ComplexLong => .ComplexLongLong,
                .ComplexSlong => .ComplexSlongLong,
                .ComplexUlong => .ComplexUlongLong,
                .LongLong,
                .ULongLong,
                .ULongLongInt,
                .ComplexLongLong,
                .ComplexUlongLong,
                .ComplexUlongLongInt,
                => return b.duplicateSpec(sourceToken, "long"),
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
                .ComplexUChar,
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
                    .Float16 => .ComplexFloat16,
                    .Float => .ComplexFloat,
                    .Double => .ComplexDouble,
                    .LongDouble => .ComplexLongDouble,
                    .Float128 => .ComplexFloat128,
                },
                else => unreachable,
            },
            else => .{ .other = qt },
        };
    }
};
