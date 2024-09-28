const std = @import("std");
const Allocator = std.mem.Allocator;
const assert = std.debug.assert;
const Value = @import("../AST/Value.zig");
const BigIntConst = std.math.big.int.Const;
const BigIntMutable = std.math.big.int.Mutable;
const Hash = std.hash.Wyhash;

const Interner = @This();

map: std.AutoArrayHashMapUnmanaged(void, void) = .{},
items: std.MultiArrayList(struct {
    tag: Tag,
    data: u32,
}) = .{},
extra: std.ArrayListUnmanaged(u32) = .{},
strings: std.ArrayListUnmanaged(u8) = .{},

const KeyAdapter = struct {
    interner: *const Interner,

    pub fn eql(adapter: KeyAdapter, a: Key, b_void: void, b_map_index: usize) bool {
        _ = b_void;
        return adapter.interner.get(@as(Ref, @enumFromInt(b_map_index))).eql(a);
    }

    pub fn hash(adapter: KeyAdapter, a: Key) u32 {
        _ = adapter;
        return a.hash();
    }
};

pub const Key = union(enum) {
    intTy: u16,
    floatTy: u16,
    ptrTy,
    noreturnTy,
    voidTy,
    funcTy,
    arrayTy: struct {
        len: u64,
        child: Ref,
    },
    vectorTy: struct {
        len: u32,
        child: Ref,
    },
    /// may not be zero
    null,
    int: union(enum) {
        u64: u64,
        i64: i64,
        bigInt: BigIntConst,
    },
    float: Float,
    bytes: []const u8,
    record: struct {
        /// Pointer to user data, value used for hash and equality check.
        userPtr: *anyopaque,
        elements: []const Ref,
    },

    pub const Float = union(enum) {
        f16: f16,
        f32: f32,
        f64: f64,
        f80: f80,
        f128: f128,
    };

    pub fn hash(key: Key) u32 {
        var hasher = Hash.init(0);
        const tag = std.meta.activeTag(key);
        std.hash.autoHash(&hasher, tag);
        switch (key) {
            .bytes => |bytes| {
                hasher.update(bytes);
            },
            .record => |info| {
                std.hash.autoHash(&hasher, @intFromPtr(info.userPtr));
            },
            .float => @panic("TODO"),
            .int => @panic("TODO"),
            inline else => |a_info| {
                std.hash.autoHash(&hasher, a_info);
            },
        }
        return @truncate(hasher.final());
    }

    pub fn eql(lhs: Key, rhs: Key) bool {
        const KeyTag = std.meta.Tag(Key);
        const lhsTag: KeyTag = lhs;
        const rhsTag: KeyTag = rhs;
        if (lhsTag != rhsTag) return false;
        switch (lhs) {
            .record => |lhsInfo| {
                return lhsInfo.userPtr == rhs.record.userPtr;
            },
            .bytes => |lhsBytes| {
                const rhsBytes = rhs.bytes;
                return std.mem.eql(u8, lhsBytes, rhsBytes);
            },
            inline else => |lhsInfo, tag| {
                const rhsInfo = @field(rhs, @tagName(tag));
                return std.meta.eql(lhsInfo, rhsInfo);
            },
        }
    }

    fn toRef(key: Key) ?Ref {
        switch (key) {
            .intTy => |bits| switch (bits) {
                1 => return .i1,
                8 => return .i8,
                16 => return .i16,
                32 => return .i32,
                64 => return .i64,
                128 => return .i128,
                else => {},
            },
            .floatTy => |bits| switch (bits) {
                16 => return .f16,
                32 => return .f32,
                64 => return .f64,
                80 => return .f80,
                128 => return .f128,
                else => unreachable,
            },
            .ptrTy => return .ptr,
            .funcTy => return .func,
            .noreturnTy => return .noreturn,
            .voidTy => return .void,
            .int => |repr| switch (repr) {
                inline .u64, .i64 => |u| switch (u) {
                    0 => return .zero,
                    1 => return .one,
                    else => {},
                },
                .bigInt => |data| if (data.eqlZero()) return .zero,
            },
            .null => return .null,
            else => {},
        }
        return null;
    }
};

pub const Ref = enum(u32) {
    ptr,
    noreturn,
    void,
    i1,
    i8,
    i16,
    i32,
    i64,
    i128,
    f16,
    f32,
    f64,
    f80,
    f128,
    func,
    zero,
    one,
    null,
    _,
};

pub const Tag = enum(u8) {
    /// `data` is `u16`
    intTy,
    /// `data` is `u16`
    floatTy,
    /// `data` is index to `Array`
    arrayTy,
    /// `data` is index to `Vector`
    vectorTy,
    /// `data` is `u32`
    u32,
    /// `data` is `i32`
    i32,
    /// `data` is `Int`
    intPositive,
    /// `data` is `Int`
    intNegative,
    /// `data` is `f16`
    f16,
    /// `data` is `f32`
    f32,
    /// `data` is `F64`
    f64,
    /// `data` is `F80`
    f80,
    /// `data` is `F128`
    f128,
    /// `data` is `Bytes`
    bytes,
    /// `data` is `Record`
    record,

    pub const Array = struct {
        len0: u32,
        len1: u32,
        child: Ref,

        pub fn getLen(a: Array) u64 {
            return (PackedU64{
                .a = a.len0,
                .b = a.len1,
            }).get();
        }
    };

    pub const Vector = struct {
        len: u32,
        child: Ref,
    };

    pub const Int = struct {
        limbs_index: u32,
        limbs_len: u32,

        /// Big enough to fit any non-BigInt value
        pub const BigIntSpace = struct {
            /// The +1 is headroom so that operations such as incrementing once
            /// or decrementing once are possible without using an allocator.
            limbs: [(@sizeOf(u64) / @sizeOf(std.math.big.Limb)) + 1]std.math.big.Limb,
        };
    };

    pub const F64 = struct {
        piece0: u32,
        piece1: u32,

        pub fn get(self: F64) f64 {
            const int_bits = @as(u64, self.piece0) | (@as(u64, self.piece1) << 32);
            return @bitCast(int_bits);
        }

        fn pack(val: f64) F64 {
            const bits = @as(u64, @bitCast(val));
            return .{
                .piece0 = @as(u32, @truncate(bits)),
                .piece1 = @as(u32, @truncate(bits >> 32)),
            };
        }
    };

    pub const F80 = struct {
        piece0: u32,
        piece1: u32,
        piece2: u32, // u16 part, top bits

        pub fn get(self: F80) f80 {
            const int_bits = @as(u80, self.piece0) |
                (@as(u80, self.piece1) << 32) |
                (@as(u80, self.piece2) << 64);
            return @bitCast(int_bits);
        }

        fn pack(val: f80) F80 {
            const bits = @as(u80, @bitCast(val));
            return .{
                .piece0 = @as(u32, @truncate(bits)),
                .piece1 = @as(u32, @truncate(bits >> 32)),
                .piece2 = @as(u16, @truncate(bits >> 64)),
            };
        }
    };

    pub const F128 = struct {
        piece0: u32,
        piece1: u32,
        piece2: u32,
        piece3: u32,

        pub fn get(self: F128) f128 {
            const int_bits = @as(u128, self.piece0) |
                (@as(u128, self.piece1) << 32) |
                (@as(u128, self.piece2) << 64) |
                (@as(u128, self.piece3) << 96);
            return @bitCast(int_bits);
        }

        fn pack(val: f128) F128 {
            const bits = @as(u128, @bitCast(val));
            return .{
                .piece0 = @as(u32, @truncate(bits)),
                .piece1 = @as(u32, @truncate(bits >> 32)),
                .piece2 = @as(u32, @truncate(bits >> 64)),
                .piece3 = @as(u32, @truncate(bits >> 96)),
            };
        }
    };

    pub const Bytes = struct {
        stringsIndex: u32,
        len: u32,
    };

    pub const Record = struct {
        ptr0: u32,
        ptr1: u32,
        elements_len: u32,
        // trailing
        // [elements_len]Ref

        pub fn getPtr(r: Record) *anyopaque {
            return @ptrFromInt((PackedU64{
                .a = r.ptr0,
                .b = r.ptr1,
            }).get());
        }
    };
};

pub const PackedU64 = packed struct(u64) {
    a: u32,
    b: u32,

    pub fn get(x: PackedU64) u64 {
        return @bitCast(x);
    }

    pub fn init(x: u64) PackedU64 {
        return @bitCast(x);
    }
};

pub fn deinit(self: *Interner, gpa: Allocator) void {
    self.map.deinit(gpa);
    self.items.deinit(gpa);
    self.extra.deinit(gpa);
    self.strings.deinit(gpa);
}

pub fn put(self: *Interner, gpa: Allocator, key: Key) !Ref {
    if (key.toRef()) |some|
        return some;
    const adapter = KeyAdapter{ .interner = self };
    const gop = try self.map.getOrPutAdapted(gpa, key, adapter);
    return @enumFromInt(gop.index);
}

pub fn has(self: *Interner, key: Key) ?Ref {
    if (key.toRef()) |some|
        return some;

    if (self.map.getIndex(key)) |index|
        return @enumFromInt(index);

    return null;
}

pub fn get(ip: Interner, ref: Ref) Key {
    switch (ref) {
        .ptr => return .ptr,
        .func => return .func,
        .noreturn => return .noreturn,
        .void => return .void,
        .i1 => return .{ .int = 1 },
        .i8 => return .{ .int = 8 },
        .i16 => return .{ .int = 16 },
        .i32 => return .{ .int = 32 },
        .i64 => return .{ .int = 64 },
        .i128 => return .{ .int = 128 },
        .f16 => return .{ .float = 16 },
        .f32 => return .{ .float = 32 },
        .f64 => return .{ .float = 64 },
        .f80 => return .{ .float = 80 },
        .f128 => return .{ .float = 128 },
        else => {},
    }
    return ip.map.keys()[@intFromEnum(ref)];
}
