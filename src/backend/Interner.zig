const std = @import("std");
const Allocator = std.mem.Allocator;
const assert = std.debug.assert;
const BigIntConst = std.math.big.int.Const;
const BigIntMutable = std.math.big.int.Mutable;
const Hash = std.hash.Wyhash;
const Limb = std.math.big.Limb;

const Interner = @This();

map: std.AutoArrayHashMapUnmanaged(void, void) = .{},
items: std.MultiArrayList(struct {
    tag: Tag,
    data: u32,
}) = .{},
extra: std.ArrayListUnmanaged(u32) = .{},
limbs: std.ArrayListUnmanaged(Limb) = .{},
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
    complexTy: u16,
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
    recordTy: []const Ref,
    /// may not be zero
    null,
    int: union(enum) {
        u64: u64,
        i64: i64,
        bigInt: BigIntConst,

        pub fn toBigInt(repr: @This(), space: *Tag.Int.BigIntSpace) BigIntConst {
            return switch (repr) {
                .bigInt => |x| x,
                inline .u64, .i64 => |x| BigIntMutable.init(&space.limbs, x).toConst(),
            };
        }
    },
    float: Float,
    complex: Complex,
    bytes: []const u8,

    pub const Float = union(enum) {
        f16: f16,
        f32: f32,
        f64: f64,
        f80: f80,
        f128: f128,
    };

    pub const Complex = union(enum) {
        cf16: [2]f16,
        cf32: [2]f32,
        cf64: [2]f64,
        cf80: [2]f80,
        cf128: [2]f128,
    };

    pub fn hash(key: Key) u32 {
        var hasher = Hash.init(0);
        const tag = std.meta.activeTag(key);
        std.hash.autoHash(&hasher, tag);
        switch (key) {
            .bytes => |bytes| {
                hasher.update(bytes);
            },
            .recordTy => |elems| for (elems) |elem| {
                std.hash.autoHash(&hasher, elem);
            },
            .float => |repr| switch (repr) {
                inline else => |data| std.hash.autoHash(
                    &hasher,
                    @as(std.meta.Int(.unsigned, @bitSizeOf(@TypeOf(data))), @bitCast(data)),
                ),
            },
            .complex => |repr| switch (repr) {
                inline else => |data| std.hash.autoHash(
                    &hasher,
                    @as(std.meta.Int(.unsigned, @bitSizeOf(@TypeOf(data))), @bitCast(data)),
                ),
            },
            .int => |repr| {
                var space: Tag.Int.BigIntSpace = undefined;
                const big = repr.toBigInt(&space);
                std.hash.autoHash(&hasher, big.positive);
                for (big.limbs) |limb| std.hash.autoHash(&hasher, limb);
            },
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
            .recordTy => |lhsElems| {
                const rhsElems = rhs.recordTy;
                if (lhsElems.len != rhsElems.len) return false;
                for (lhsElems, rhsElems) |lhsElem, rhsElem| {
                    if (lhsElem != rhsElem) return false;
                }
                return true;
            },
            .bytes => |lhsBytes| {
                const rhsBytes = rhs.bytes;
                return std.mem.eql(u8, lhsBytes, rhsBytes);
            },
            .int => |lhsRepr| {
                var lhsSpace: Tag.Int.BigIntSpace = undefined;
                var rhsSpace: Tag.Int.BigIntSpace = undefined;

                const lhsBig = lhsRepr.toBigInt(&lhsSpace);
                const rhsBig = rhs.int.toBigInt(&rhsSpace);

                return lhsBig.eql(rhsBig);
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
            .complexTy => |bits| switch (bits) {
                16 => return .cf16,
                32 => return .cf32,
                64 => return .cf64,
                80 => return .cf80,
                128 => return .cf128,
                else => unreachable,
            },
            .ptrTy => return .ptr,
            .funcTy => return .func,
            .noreturnTy => return .noreturn,
            .voidTy => return .void,
            .int => |repr| {
                var space: Tag.Int.BigIntSpace = undefined;
                const big = repr.toBigInt(&space);
                if (big.eqlZero()) return .zero;
                const bigOne = BigIntConst{ .limbs = &.{1}, .positive = true };
                if (big.eql(bigOne)) return .one;
            },
            .float => |repr| switch (repr) {
                inline else => |data| {
                    if (std.math.isPositiveZero(data)) return .zero;
                    if (data == 1) return .one;
                },
            },
            .null => return .null,
            else => {},
        }
        return null;
    }
};

pub const Ref = enum(u32) {
    const max = std.math.maxInt(u32);

    ptr = max - 1,
    noreturn = max - 2,
    void = max - 3,
    i1 = max - 4,
    i8 = max - 5,
    i16 = max - 6,
    i32 = max - 7,
    i64 = max - 8,
    i128 = max - 9,
    f16 = max - 10,
    f32 = max - 11,
    f64 = max - 12,
    f80 = max - 13,
    f128 = max - 14,
    func = max - 15,
    zero = max - 16,
    one = max - 17,
    null = max - 18,
    cf16 = max - 19,
    cf32 = max - 20,
    cf64 = max - 21,
    cf80 = max - 22,
    cf128 = max - 23,
    _,
};

pub const OptRef = enum(u32) {
    const max = std.math.maxInt(u32);

    none = max - 0,
    ptr = max - 1,
    noreturn = max - 2,
    void = max - 3,
    i1 = max - 4,
    i8 = max - 5,
    i16 = max - 6,
    i32 = max - 7,
    i64 = max - 8,
    i128 = max - 9,
    f16 = max - 10,
    f32 = max - 11,
    f64 = max - 12,
    f80 = max - 13,
    f128 = max - 14,
    func = max - 15,
    zero = max - 16,
    one = max - 17,
    null = max - 18,
    cf16 = max - 19,
    cf32 = max - 20,
    cf64 = max - 21,
    cf80 = max - 22,
    cf128 = max - 23,
    _,
};

pub const Tag = enum(u8) {
    /// `data` is `u16`
    intTy,
    /// `data` is `u16`
    floatTy,
    /// `data` is `u16`
    complexTy,
    /// `data` is index to `Array`
    arrayTy,
    /// `data` is index to `Vector`
    vectorTy,
    /// `data` is `u32`
    u32,
    /// `data` is `i32`
    i32,
    /// `data` is `Int`
    intPostive,
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
    /// `data` is `CF16`
    cf16,
    /// `data` is `CF32`
    cf32,
    /// `data` is `CF64`
    cf64,
    /// `data` is `CF80`
    cf80,
    /// `data` is `CF128`
    cf128,
    /// `data` is `Bytes`
    bytes,
    /// `data` is `Record`
    recordTy,

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
        limbsIndex: u32,
        limbsLen: u32,

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
            const intBits = @as(u64, self.piece0) | (@as(u64, self.piece1) << 32);
            return @bitCast(intBits);
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

    pub const CF16 = struct {
        piece0: u32,

        pub fn get(self: CF16) [2]f16 {
            const real: f16 = @bitCast(@as(u16, @truncate(self.piece0 >> 16)));
            const imag: f16 = @bitCast(@as(u16, @truncate(self.piece0)));
            return .{ real, imag };
        }

        fn pack(val: [2]f16) CF16 {
            const real: u16 = @bitCast(val[0]);
            const imag: u16 = @bitCast(val[1]);
            return .{ .piece0 = (@as(u32, real) << 16) | @as(u32, imag) };
        }
    };

    pub const CF32 = struct {
        piece0: u32,
        piece1: u32,

        pub fn get(self: CF32) [2]f32 {
            return .{
                @bitCast(self.piece0),
                @bitCast(self.piece1),
            };
        }

        fn pack(val: [2]f32) CF32 {
            return .{
                .piece0 = @bitCast(val[0]),
                .piece1 = @bitCast(val[1]),
            };
        }
    };

    pub const CF64 = struct {
        piece0: u32,
        piece1: u32,
        piece2: u32,
        piece3: u32,

        pub fn get(self: CF64) [2]f64 {
            return .{
                (F64{ .piece0 = self.piece0, .piece1 = self.piece1 }).get(),
                (F64{ .piece0 = self.piece2, .piece1 = self.piece3 }).get(),
            };
        }

        fn pack(val: [2]f64) CF64 {
            const real = F64.pack(val[0]);
            const imag = F64.pack(val[1]);
            return .{
                .piece0 = real.piece0,
                .piece1 = real.piece1,
                .piece2 = imag.piece0,
                .piece3 = imag.piece1,
            };
        }
    };

    /// TODO pack into 5 pieces
    pub const CF80 = struct {
        piece0: u32,
        piece1: u32,
        piece2: u32, // u16 part, top bits
        piece3: u32,
        piece4: u32,
        piece5: u32, // u16 part, top bits

        pub fn get(self: CF80) [2]f80 {
            return .{
                (F80{ .piece0 = self.piece0, .piece1 = self.piece1, .piece2 = self.piece2 }).get(),
                (F80{ .piece0 = self.piece3, .piece1 = self.piece4, .piece2 = self.piece5 }).get(),
            };
        }

        fn pack(val: [2]f80) CF80 {
            const real = F80.pack(val[0]);
            const imag = F80.pack(val[1]);
            return .{
                .piece0 = real.piece0,
                .piece1 = real.piece1,
                .piece2 = real.piece2,
                .piece3 = imag.piece0,
                .piece4 = imag.piece1,
                .piece5 = imag.piece2,
            };
        }
    };

    pub const CF128 = struct {
        piece0: u32,
        piece1: u32,
        piece2: u32,
        piece3: u32,
        piece4: u32,
        piece5: u32,
        piece6: u32,
        piece7: u32,

        pub fn get(self: CF128) [2]f128 {
            return .{
                (F128{ .piece0 = self.piece0, .piece1 = self.piece1, .piece2 = self.piece2, .piece3 = self.piece3 }).get(),
                (F128{ .piece0 = self.piece4, .piece1 = self.piece5, .piece2 = self.piece6, .piece3 = self.piece7 }).get(),
            };
        }

        fn pack(val: [2]f128) CF128 {
            const real = F128.pack(val[0]);
            const imag = F128.pack(val[1]);
            return .{
                .piece0 = real.piece0,
                .piece1 = real.piece1,
                .piece2 = real.piece2,
                .piece3 = real.piece3,
                .piece4 = imag.piece0,
                .piece5 = imag.piece1,
                .piece6 = imag.piece2,
                .piece7 = imag.piece3,
            };
        }
    };

    pub const Bytes = struct {
        stringsIndex: u32,
        len: u32,
    };

    pub const Record = struct {
        elementsLen: u32,
        // trailing
        // [elements_len]Ref
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
    self.limbs.deinit(gpa);
    self.strings.deinit(gpa);
}

pub fn put(self: *Interner, gpa: Allocator, key: Key) !Ref {
    if (key.toRef()) |some|
        return some;

    const adapter = KeyAdapter{ .interner = self };
    const gop = try self.map.getOrPutAdapted(gpa, key, adapter);
    if (gop.found_existing) return @enumFromInt(gop.index);

    try self.items.ensureUnusedCapacity(gpa, 1);

    switch (key) {
        .intTy => |bits| self.items.appendAssumeCapacity(.{ .tag = .intTy, .data = bits }),
        .floatTy => |bits| self.items.appendAssumeCapacity(.{ .tag = .floatTy, .data = bits }),
        .complexTy => |bits| self.items.appendAssumeCapacity(.{ .tag = .complexTy, .data = bits }),

        .arrayTy => |info| {
            const splitLen = PackedU64.init(info.len);
            self.items.appendAssumeCapacity(.{
                .tag = .arrayTy,
                .data = try self.addExtra(gpa, Tag.Array{
                    .len0 = splitLen.a,
                    .len1 = splitLen.b,
                    .child = info.child,
                }),
            });
        },

        .vectorTy => |info| {
            self.items.appendAssumeCapacity(.{
                .tag = .vectorTy,
                .data = try self.addExtra(gpa, Tag.Vector{
                    .len = info.len,
                    .child = info.child,
                }),
            });
        },

        .int => |repr| int: {
            var space: Tag.Int.BigIntSpace = undefined;
            const big = repr.toBigInt(&space);
            switch (repr) {
                .u64 => |data| if (std.math.cast(u32, data)) |small| {
                    self.items.appendAssumeCapacity(.{ .tag = .u32, .data = small });
                    break :int;
                },
                .i64 => |data| if (std.math.cast(i32, data)) |small| {
                    self.items.appendAssumeCapacity(.{ .tag = .i32, .data = @bitCast(small) });
                    break :int;
                },
                .bigInt => |data| {
                    if (data.fitsInTwosComp(.unsigned, 32)) {
                        self.items.appendAssumeCapacity(.{
                            .tag = .u32,
                            .data = data.to(u32) catch unreachable,
                        });
                        break :int;
                    } else if (data.fitsInTwosComp(.signed, 32)) {
                        self.items.appendAssumeCapacity(.{
                            .tag = .i32,
                            .data = @bitCast(data.to(i32) catch unreachable),
                        });
                        break :int;
                    }
                },
            }

            const limbsIndex: u32 = @intCast(self.limbs.items.len);
            try self.limbs.appendSlice(gpa, big.limbs);
            self.items.appendAssumeCapacity(.{
                .tag = if (big.positive) .intPostive else .intNegative,
                .data = try self.addExtra(gpa, Tag.Int{
                    .limbsIndex = limbsIndex,
                    .limbsLen = @intCast(big.limbs.len),
                }),
            });
        },

        .float => |repr| switch (repr) {
            .f16 => |data| self.items.appendAssumeCapacity(.{
                .tag = .f16,
                .data = @as(u16, @bitCast(data)),
            }),
            .f32 => |data| self.items.appendAssumeCapacity(.{
                .tag = .f32,
                .data = @as(u32, @bitCast(data)),
            }),
            .f64 => |data| self.items.appendAssumeCapacity(.{
                .tag = .f64,
                .data = try self.addExtra(gpa, Tag.F64.pack(data)),
            }),
            .f80 => |data| self.items.appendAssumeCapacity(.{
                .tag = .f80,
                .data = try self.addExtra(gpa, Tag.F80.pack(data)),
            }),
            .f128 => |data| self.items.appendAssumeCapacity(.{
                .tag = .f128,
                .data = try self.addExtra(gpa, Tag.F128.pack(data)),
            }),
        },

        .complex => |repr| switch (repr) {
            .cf16 => |data| self.items.appendAssumeCapacity(.{
                .tag = .cf16,
                .data = try self.addExtra(gpa, Tag.CF16.pack(data)),
            }),
            .cf32 => |data| self.items.appendAssumeCapacity(.{
                .tag = .cf32,
                .data = try self.addExtra(gpa, Tag.CF32.pack(data)),
            }),
            .cf64 => |data| self.items.appendAssumeCapacity(.{
                .tag = .cf64,
                .data = try self.addExtra(gpa, Tag.CF64.pack(data)),
            }),
            .cf80 => |data| self.items.appendAssumeCapacity(.{
                .tag = .cf80,
                .data = try self.addExtra(gpa, Tag.CF80.pack(data)),
            }),
            .cf128 => |data| self.items.appendAssumeCapacity(.{
                .tag = .cf128,
                .data = try self.addExtra(gpa, Tag.CF128.pack(data)),
            }),
        },

        .bytes => |bytes| {
            const stringsIndex: u32 = @intCast(self.strings.items.len);
            try self.strings.appendSlice(gpa, bytes);
            self.items.appendAssumeCapacity(.{
                .tag = .bytes,
                .data = try self.addExtra(gpa, Tag.Bytes{
                    .stringsIndex = stringsIndex,
                    .len = @intCast(bytes.len),
                }),
            });
        },

        .recordTy => |elems| {
            try self.extra.ensureUnusedCapacity(gpa, @typeInfo(Tag.Record).@"struct".fields.len + elems.len);
            self.items.appendAssumeCapacity(.{
                .tag = .recordTy,
                .data = self.addExtraAssumeCapacity(Tag.Record{
                    .elementsLen = @intCast(elems.len),
                }),
            });
            self.extra.appendSliceAssumeCapacity(@ptrCast(elems));
        },

        .ptrTy,
        .noreturnTy,
        .voidTy,
        .funcTy,
        .null,
        => unreachable,
    }

    return @enumFromInt(gop.index);
}

fn addExtra(self: *Interner, gpa: Allocator, extra: anytype) Allocator.Error!u32 {
    const fields = @typeInfo(@TypeOf(extra)).@"struct".fields;
    try self.extra.ensureUnusedCapacity(gpa, fields.len);
    return self.addExtraAssumeCapacity(extra);
}

fn addExtraAssumeCapacity(self: *Interner, extra: anytype) u32 {
    const result = @as(u32, @intCast(self.extra.items.len));
    inline for (@typeInfo(@TypeOf(extra)).@"struct".fields) |field| {
        self.extra.appendAssumeCapacity(switch (field.type) {
            Ref => @intFromEnum(@field(extra, field.name)),
            u32 => @field(extra, field.name),
            else => @compileError("bad field type: " ++ @typeName(field.type)),
        });
    }
    return result;
}

pub fn get(self: *const Interner, ref: Ref) Key {
    switch (ref) {
        .ptr => return .ptrTy,
        .func => return .funcTy,
        .noreturn => return .noreturnTy,
        .void => return .voidTy,
        .i1 => return .{ .intTy = 1 },
        .i8 => return .{ .intTy = 8 },
        .i16 => return .{ .intTy = 16 },
        .i32 => return .{ .intTy = 32 },
        .i64 => return .{ .intTy = 64 },
        .i128 => return .{ .intTy = 128 },
        .f16 => return .{ .floatTy = 16 },
        .f32 => return .{ .floatTy = 32 },
        .f64 => return .{ .floatTy = 64 },
        .f80 => return .{ .floatTy = 80 },
        .f128 => return .{ .floatTy = 128 },
        .zero => return .{ .int = .{ .u64 = 0 } },
        .one => return .{ .int = .{ .u64 = 1 } },
        .null => return .null,
        .cf16 => return .{ .complexTy = 16 },
        .cf32 => return .{ .complexTy = 32 },
        .cf64 => return .{ .complexTy = 64 },
        .cf80 => return .{ .complexTy = 80 },
        else => {},
    }

    const item = self.items.get(@intFromEnum(ref));
    const data = item.data;
    return switch (item.tag) {
        .intTy => .{ .intTy = @intCast(data) },
        .floatTy => .{ .floatTy = @intCast(data) },
        .complexTy => .{ .complexTy = @intCast(data) },
        .arrayTy => {
            const arrayType = self.extraData(Tag.Array, data);
            return .{
                .arrayTy = .{
                    .len = arrayType.getLen(),
                    .child = arrayType.child,
                },
            };
        },

        .vectorTy => {
            const vectorType = self.extraData(Tag.Vector, data);
            return .{
                .vectorTy = .{
                    .len = vectorType.len,
                    .child = vectorType.child,
                },
            };
        },

        .u32 => .{ .int = .{ .u64 = data } },
        .i32 => .{ .int = .{ .i64 = @as(i32, @bitCast(data)) } },

        .intNegative, .intPostive => {
            const intInfo = self.extraData(Tag.Int, data);
            const limbs = self.limbs.items[intInfo.limbsIndex..][0..intInfo.limbsLen];
            return .{
                .int = .{
                    .bigInt = .{
                        .positive = item.tag == .intPostive,
                        .limbs = limbs,
                    },
                },
            };
        },

        .f16 => .{ .float = .{ .f16 = @bitCast(@as(u16, @intCast(data))) } },
        .f32 => .{ .float = .{ .f32 = @bitCast(data) } },
        .f64 => {
            const float = self.extraData(Tag.F64, data);
            return .{ .float = .{ .f64 = float.get() } };
        },
        .f80 => {
            const float = self.extraData(Tag.F80, data);
            return .{ .float = .{ .f80 = float.get() } };
        },
        .f128 => {
            const float = self.extraData(Tag.F128, data);
            return .{ .float = .{ .f128 = float.get() } };
        },

        .cf16 => {
            const components = self.extraData(Tag.CF16, data);
            return .{ .complex = .{ .cf16 = components.get() } };
        },
        .cf32 => {
            const components = self.extraData(Tag.CF32, data);
            return .{ .complex = .{ .cf32 = components.get() } };
        },
        .cf64 => {
            const components = self.extraData(Tag.CF64, data);
            return .{ .complex = .{ .cf64 = components.get() } };
        },
        .cf80 => {
            const components = self.extraData(Tag.CF80, data);
            return .{ .complex = .{ .cf80 = components.get() } };
        },
        .cf128 => {
            const components = self.extraData(Tag.CF128, data);
            return .{ .complex = .{ .cf128 = components.get() } };
        },

        .bytes => {
            const bytes = self.extraData(Tag.Bytes, data);
            return .{ .bytes = self.strings.items[bytes.stringsIndex..][0..bytes.len] };
        },

        .recordTy => {
            const extra = self.extraDataTrail(Tag.Record, data);
            return .{
                .recordTy = @ptrCast(self.extra.items[extra.end..][0..extra.data.elementsLen]),
            };
        },
    };
}

fn extraData(interner: *const Interner, comptime T: type, index: usize) T {
    return interner.extraDataTrail(T, index).data;
}

fn extraDataTrail(interner: *const Interner, comptime T: type, index: usize) struct { data: T, end: u32 } {
    var result: T = undefined;
    const fields = @typeInfo(T).@"struct".fields;
    inline for (fields, 0..) |field, field_i| {
        const int32 = interner.extra.items[field_i + index];
        @field(result, field.name) = switch (field.type) {
            Ref => @enumFromInt(int32),
            u32 => int32,
            else => @compileError("bad field type: " ++ @typeName(field.type)),
        };
    }
    return .{
        .data = result,
        .end = @intCast(index + fields.len),
    };
}
