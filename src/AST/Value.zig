const std = @import("std");
const assert = std.debug.assert;
const Compilation = @import("../Basic/Compilation.zig");
const Type = @import("Type.zig");
const Interner = @import("../CodeGen/Interner.zig");
const BigIntSpace = Interner.Tag.Int.BigIntSpace;
const BigIntConst = std.math.big.int.Const;
const BigIntMutable = std.math.big.int.Mutable;

const Value = @This();

pub const Context = struct {
    interner: *const Interner,
    comp: *const Compilation,
    mutable: if (std.debug.runtime_safety) bool else void = if (std.debug.runtime_safety) false else {},

    pub fn intern(ctx: Context, k: Interner.Key) !Value {
        if (std.debug.runtime_safety) assert(ctx.mutable);
        const r = try @constCast(ctx.interner).put(ctx.comp.gpa, k);
        return .{ .optRef = @enumFromInt(@intFromEnum(r)) };
    }
};

pub const ByteRange = struct {
    start: u32,
    end: u32,

    pub fn len(self: ByteRange) u32 {
        return self.end - self.start;
    }

    pub fn trim(self: ByteRange, amount: u32) ByteRange {
        std.debug.assert(self.start <= self.end - amount);
        return .{ .start = self.start, .end = self.end - amount };
    }

    pub fn slice(
        self: ByteRange,
        all_bytes: []const u8,
        comptime size: Compilation.CharUnitSize,
    ) []const size.Type() {
        switch (size) {
            inline else => |sz| {
                const aligned: []align(@alignOf(sz.Type())) const u8 = @alignCast(all_bytes[self.start..self.end]);
                return std.mem.bytesAsSlice(sz.Type(), aligned);
            },
        }
    }

    pub fn dumpString(range: ByteRange, ty: Type, comp: *const Compilation, strings: []const u8, w: anytype) !void {
        const size: Compilation.CharUnitSize = @enumFromInt(ty.getElemType().sizeof(comp).?);
        const withoutNull = range.trim(@intFromEnum(size));
        switch (size) {
            inline .@"1", .@"2" => |sz| {
                const data_slice = withoutNull.slice(strings, sz);
                const formatter = if (sz == .@"1") std.zig.fmtEscapes(data_slice) else std.unicode.fmtUtf16Le(data_slice);
                try w.print("\"{}\"", .{formatter});
            },
            .@"4" => {
                try w.writeByte('"');
                const data_slice = withoutNull.slice(strings, .@"4");
                var buf: [4]u8 = undefined;
                for (data_slice) |item| {
                    if (item <= std.math.maxInt(u21) and std.unicode.utf8ValidCodepoint(@intCast(item))) {
                        const codepoint: u21 = @intCast(item);
                        const written = std.unicode.utf8Encode(codepoint, &buf) catch unreachable;
                        try w.print("{s}", .{buf[0..written]});
                    } else {
                        try w.print("\\x{x}", .{item});
                    }
                }
                try w.writeByte('"');
            },
        }
    }
};

pub const OptRef = enum(u32) {
    none = std.math.maxInt(u32),
    _,
};

optRef: OptRef = .none,

/// The current kind of value that `Value` is representing.
tag: Tag = .unavailable,

/// The content of the value, which varies based on what `tag` is set to.
data: union {
    none: void,
    int: u64, // Used to store integer, boolean, and pointer values as u64.
    float: f64,
    bytes: ByteRange,
} = .{ .none = {} },

/// Defines the possible types of values that the `Value` can be tagged as.
const Tag = enum {
    unavailable, // Value is not available or uninitialized.
    nullptrTy,
    int, // `int` is used to store integer, boolean and pointer values.
    float,
    bytes,
};

pub fn isUnavailable(v: Value) bool {
    return v.tag == .unavailable;
}

pub fn isNumeric(v: Value) bool {
    return switch (v.tag) {
        .int, .float => true,
        else => false,
    };
}

pub fn isZero(v: Value) bool {
    return v.ref() == .zero;
}

pub fn ref(v: Value) Interner.Ref {
    std.debug.assert(v.optRef != .none);
    return @enumFromInt(@intFromEnum(v.optRef));
}

pub const zero = Value{ .optRef = @enumFromInt(@intFromEnum(Interner.Ref.zero)) };
pub const one = Value{ .optRef = @enumFromInt(@intFromEnum(Interner.Ref.one)) };

/// Number of bits needed to hold `v`.
/// Asserts that `v` is not negative
pub fn minUnsignedBits(v: Value, ctx: Context) usize {
    var space: BigIntSpace = undefined;
    const big = v.toBigInt(&space, ctx);
    assert(big.positive);
    return big.bitCountAbs();
}

test "minUnsignedBits" {
    const Test = struct {
        fn checkIntBits(ctx: Context, v: u64, expected: usize) !void {
            const val = try ctx.intern(.{ .int = .{ .u64 = v } });
            try std.testing.expectEqual(expected, val.minUnsignedBits(ctx));
        }
    };

    var comp = Compilation.init(std.testing.allocator);
    defer comp.deinit();
    const targetQuery = try std.Target.Query.parse(.{ .arch_os_abi = "x86_64-linux-gnu" });
    comp.target = try std.zig.system.resolveTargetQuery(targetQuery);

    var interner: Interner = .{};
    defer interner.deinit(comp.gpa);

    const ctx: Context = .{
        .comp = &comp,
        .interner = &interner,
        .mutable = if (std.debug.runtime_safety) true else {},
    };

    try Test.checkIntBits(ctx, .int, 0, 0);
    try Test.checkIntBits(ctx, .int, 1, 1);
    try Test.checkIntBits(ctx, .int, 2, 2);
    try Test.checkIntBits(ctx, .int, std.math.maxInt(i8), 7);
    try Test.checkIntBits(ctx, .int, std.math.maxInt(u8), 8);
    try Test.checkIntBits(ctx, .int, std.math.maxInt(i16), 15);
    try Test.checkIntBits(ctx, .int, std.math.maxInt(u16), 16);
    try Test.checkIntBits(ctx, .int, std.math.maxInt(i32), 31);
    try Test.checkIntBits(ctx, .uint, std.math.maxInt(u32), 32);
    try Test.checkIntBits(ctx, .long, std.math.maxInt(i64), 63);
    try Test.checkIntBits(ctx, .ulong, std.math.maxInt(u64), 64);
    try Test.checkIntBits(ctx, .long_long, std.math.maxInt(i64), 63);
    try Test.checkIntBits(ctx, .ulong_long, std.math.maxInt(u64), 64);
}

/// Minimum number of bits needed to represent `v` in 2's complement notation
/// Asserts that `v` is negative.
pub fn minSignedBits(v: Value, ctx: Context) usize {
    var space: BigIntSpace = undefined;
    const big = v.toBigInt(&space, ctx);
    assert(!big.positive);
    return big.bitCountTwosComp();
}

test "minSignedBits" {
    const Test = struct {
        fn checkIntBits(ctx: Context, v: i64, expected: usize) !void {
            const val = try ctx.intern(.{ .int = .{ .i64 = v } });
            try std.testing.expectEqual(expected, val.minSignedBits(ctx));
        }
    };

    var comp = Compilation.init(std.testing.allocator);
    defer comp.deinit();
    const targetQuery = try std.Target.Query.parse(.{ .arch_os_abi = "x86_64-linux-gnu" });
    comp.target = try std.zig.system.resolveTargetQuery(targetQuery);

    var interner: Interner = .{};
    defer interner.deinit(comp.gpa);

    const ctx: Context = .{
        .comp = &comp,
        .interner = &interner,
        .mutable = if (std.debug.runtime_safety) true else {},
    };

    try Test.checkIntBits(ctx, -1, 1);
    try Test.checkIntBits(ctx, -2, 2);
    try Test.checkIntBits(ctx, -10, 5);
    try Test.checkIntBits(ctx, -101, 8);
    try Test.checkIntBits(ctx, std.math.minInt(i8), 8);
    try Test.checkIntBits(ctx, std.math.minInt(i16), 16);
    try Test.checkIntBits(ctx, std.math.minInt(i32), 32);
    try Test.checkIntBits(ctx, std.math.minInt(i64), 64);
}

pub const FloatToIntChangeKind = enum {
    ///value did not change,
    none,
    /// floating point number too small or large for destination integer type
    outOfRange,
    /// tried to convert a NaN or Infinity
    overflow,
    /// fractional value was converted to zero
    nonZeroToZero,
    /// fractional part truncated
    valueChanged,
};

/// Converts a floating-point value to an integer value, handling edge cases and indicating the kind of change that occurred.
/// @param FloatTy            The type of floating-point number (e.g., f32 or f64).
/// @param intTySignedness    The signedness (std.builtin.Signedness) of the target integer type.
/// @param intTySize          The size (in bytes) of the target integer type.
/// @param v                  A pointer to the Value struct holding the floating-point number to be converted.
/// @return                   A FloatToIntChangeKind indicating the result of the conversion (e.g., none, outOfRange, nonZeroToZero, valueChanged).
fn floatToIntExtra(comptime FloatTy: type, intTySignedness: std.builtin.Signedness, intTySize: u16, v: *Value) FloatToIntChangeKind {
    const floatVal = v.getFloat(FloatTy);
    const wasZero = floatVal == 0;
    const hadFraction = std.math.modf(floatVal).fpart != 0;

    // Iterate through all combinations of signedness and byte count.
    switch (intTySignedness) {
        inline else => |signedness| switch (intTySize) {
            inline 1, 2, 4, 8 => |bytecount| {
                // Define the type of integer to cast to based on the signedness and size.
                const IntTy = std.meta.Int(signedness, bytecount * 8);

                // Perform a lossy cast of the floating-point value to the target integer type.
                const intVal = std.math.lossyCast(IntTy, floatVal);
                // Store the resulting integer value in the Value struct.
                v.* = int(intVal);

                // Check for special cases and return the appropriate FloatToIntChangeKind.
                // Case when the float was not zero but the result is zero.
                if (!wasZero and v.isZero()) return FloatToIntChangeKind.nonZeroToZero;
                // Case when the value is out of the range representable by IntTy.
                if (floatVal <= std.math.minInt(IntTy) or floatVal >= std.math.maxInt(IntTy)) return FloatToIntChangeKind.outOfRange;
                // Case when the float had a fractional part, meaning the value has changed.
                if (hadFraction) return FloatToIntChangeKind.valueChanged;
                // If none of the special cases apply, no significant change occurred.
                return FloatToIntChangeKind.none;
            },
            else => unreachable,
        },
    }
}

/// Converts the stored value from a float to an integer.
/// `.unavailable` value remains unchanged.
pub fn floatToInt(v: *Value, oldTy: Type, newTy: Type, comp: *Compilation) FloatToIntChangeKind {
    assert(oldTy.isFloat());
    if (v.isUnavailable())
        return FloatToIntChangeKind.none;

    if (newTy.is(.Bool)) {
        const wasZero = v.isZero();
        const wasOne = v.getFloat(f64) == 1.0;
        v.toBool();

        if (wasZero or wasOne)
            return FloatToIntChangeKind.none;

        return FloatToIntChangeKind.valueChanged;
    } else if (newTy.isUnsignedInt(comp) and v.data.float < 0) {
        v.* = int(0);
        return FloatToIntChangeKind.outOfRange;
    } else if (!std.math.isFinite(v.data.float)) {
        v.tag = .unavailable;
        return FloatToIntChangeKind.overflow;
    }

    const oldSize = oldTy.sizeof(comp).?;
    const newSize: u16 = @intCast(newTy.sizeof(comp).?);
    if (newTy.isUnsignedInt(comp))
        switch (oldSize) {
            1 => unreachable, // promoted to int
            2 => unreachable, // promoted to int
            4 => return floatToIntExtra(f32, .unsigned, newSize, v),
            8 => return floatToIntExtra(f64, .unsigned, newSize, v),
            else => unreachable,
        }
    else switch (oldSize) {
        1 => unreachable, // promoted to int
        2 => unreachable, // promoted to int
        4 => return floatToIntExtra(f32, .signed, newSize, v),
        8 => return floatToIntExtra(f64, .signed, newSize, v),
        else => unreachable,
    }
}

/// Converts the stored value from an integer to a float.
/// `.none` value remains unchanged.
pub fn intToFloat(v: *Value, destTy: Type, ctx: Context) !void {
    if (v.optRef == .none) return;
    const bits = destTy.bitSizeof(ctx.comp).?;
    return switch (ctx.interner.get(v.ref()).int) {
        inline .u64, .i64 => |data| {
            const f: Interner.Key.Float = switch (bits) {
                16 => .{ .f16 = @floatFromInt(data) },
                32 => .{ .f32 = @floatFromInt(data) },
                64 => .{ .f64 = @floatFromInt(data) },
                80 => .{ .f80 = @floatFromInt(data) },
                128 => .{ .f128 = @floatFromInt(data) },
                else => unreachable,
            };
            v.* = try ctx.intern(.{ .float = f });
        },

        .bigInt => |data| {
            const bigFloat = bigIntToFloat(data.limbs, data.positive);
            const f: Interner.Key.Float = switch (bits) {
                16 => .{ .f16 = @floatCast(bigFloat) },
                32 => .{ .f32 = @floatCast(bigFloat) },
                64 => .{ .f64 = @floatCast(bigFloat) },
                80 => .{ .f80 = @floatCast(bigFloat) },
                128 => .{ .f128 = @floatCast(bigFloat) },
                else => unreachable,
            };
            v.* = try ctx.intern(.{ .float = f });
        },
    };
}

/// Truncates or extends bits based on type.
/// oldTy is only used for size.
pub fn intCast(v: *Value, oldTy: Type, newTy: Type, comp: *Compilation) void {
    if (v.isUnavailable()) return;

    if (newTy.is(.Bool))
        return v.toBool();

    if (!oldTy.isUnsignedInt(comp)) {
        const size = newTy.sizeof(comp).?;
        switch (size) {
            1 => v.* = int(@as(u8, @truncate(@as(u64, @bitCast(v.signExtend(oldTy, comp)))))),
            2 => v.* = int(@as(u16, @truncate(@as(u64, @bitCast(v.signExtend(oldTy, comp)))))),
            4 => v.* = int(@as(u32, @truncate(@as(u64, @bitCast(v.signExtend(oldTy, comp)))))),
            8 => return,
            else => unreachable,
        }
    }
}

/// Converts the stored value from an integer to a float.
/// `.unavailable` value remains unchanged.
pub fn floatCast(v: *Value, oldTy: Type, newTy: Type, comp: *Compilation) void {
    assert(oldTy.isFloat() and newTy.isFloat());
    if (v.isUnavailable()) return;

    const size = newTy.sizeof(comp).?;
    if (!newTy.isReal() or size > 8) {
        v.tag = .unavailable;
    } else if (size == 32) {
        v.* = float(@as(f32, @floatCast(v.data.float)));
    }
}

pub fn toBigInt(val: Value, space: *BigIntSpace, ctx: Context) BigIntConst {
    return switch (ctx.interner.get(val.ref()).int) {
        inline .u64, .i64 => |x| BigIntMutable.init(&space.limbs, x).toConst(),
        .bigInt => |b| b,
    };
}

pub fn toFloat(v: Value, comptime T: type, ctx: Context) T {
    return switch (ctx.interner.get(v.ref())) {
        .int => |repr| switch (repr) {
            inline .u64, .i64 => |data| @floatFromInt(data),
            .bigInt => |data| @floatCast(bigIntToFloat(data.limbs, data.positive)),
        },

        .float => |repr| switch (repr) {
            inline else => |data| @floatCast(data),
        },

        else => unreachable,
    };
}

fn bigIntToFloat(limbs: []const std.math.big.Limb, positive: bool) f128 {
    if (limbs.len == 0) return 0;

    const base = std.math.maxInt(std.math.big.Limb) + 1;
    var result: f128 = 0;
    var i: usize = limbs.len;
    while (i != 0) {
        i -= 1;
        const limb: f128 = @as(f128, @floatFromInt(limbs[i]));
        result = @mulAdd(f128, base, result, limb);
    }

    return if (positive) result else -result;
}

/// Converts value to zero or one;
pub fn boolCast(v: *Value) void {
    v.* = fromBool(v.toBool());
}

pub fn fromBool(b: bool) Value {
    return if (b) one else zero;
}

pub fn toBool(v: Value) bool {
    return v.ref() != .zero;
}

pub fn toInt(v: Value, comptime T: type, ctx: Context) ?T {
    if (v.optRef == .none) return null;
    if (ctx.interner.get(v.ref()) != .int) return null;

    var space: BigIntSpace = undefined;
    const bigInt = v.toBigInt(&space, ctx);
    return bigInt.to(T) catch null;
}

pub fn add(res: *Value, lhs: Value, rhs: Value, ty: Type, ctx: Context) !bool {
    const bits = ty.bitSizeof(ctx.comp).?;
    if (ty.isFloat()) {
        const f: Interner.Key.Float = switch (bits) {
            16 => .{ .f16 = lhs.toFloat(f16, ctx) + rhs.toFloat(f16, ctx) },
            32 => .{ .f32 = lhs.toFloat(f32, ctx) + rhs.toFloat(f32, ctx) },
            64 => .{ .f64 = lhs.toFloat(f64, ctx) + rhs.toFloat(f64, ctx) },
            80 => .{ .f80 = lhs.toFloat(f80, ctx) + rhs.toFloat(f80, ctx) },
            128 => .{ .f128 = lhs.toFloat(f128, ctx) + rhs.toFloat(f128, ctx) },
            else => unreachable,
        };
        res.* = try ctx.intern(.{ .float = f });
        return false;
    } else {
        var lhsSpace: BigIntSpace = undefined;
        var rhsSpace: BigIntSpace = undefined;
        const lhsBigInt = lhs.toBigInt(&lhsSpace, ctx);
        const rhsBigInt = rhs.toBigInt(&rhsSpace, ctx);

        const limbs = try ctx.comp.gpa.alloc(
            std.math.big.Limb,
            std.math.big.int.calcTwosCompLimbCount(bits),
        );
        defer ctx.comp.gpa.free(limbs);

        var result = BigIntMutable{ .limbs = limbs, .positive = undefined, .len = undefined };
        const overflowed = result.addWrap(lhsBigInt, rhsBigInt, ty.signedness(ctx.comp), bits);
        res.* = .{ .ref = try ctx.interner.put(ctx.comp.gpa, .{ .int = .{ .bigInt = result.toConst() } }) };
        return overflowed;
    }
}

pub fn sub(res: *Value, lhs: Value, rhs: Value, ty: Type, ctx: Context) !bool {
    const bits = ty.bitSizeof(ctx.comp).?;
    if (ty.isFloat()) {
        const f: Interner.Key.Float = switch (bits) {
            16 => .{ .f16 = lhs.toFloat(f16, ctx) - rhs.toFloat(f16, ctx) },
            32 => .{ .f32 = lhs.toFloat(f32, ctx) - rhs.toFloat(f32, ctx) },
            64 => .{ .f64 = lhs.toFloat(f64, ctx) - rhs.toFloat(f64, ctx) },
            80 => .{ .f80 = lhs.toFloat(f80, ctx) - rhs.toFloat(f80, ctx) },
            128 => .{ .f128 = lhs.toFloat(f128, ctx) - rhs.toFloat(f128, ctx) },
            else => unreachable,
        };
        res.* = try ctx.intern(.{ .float = f });
        return false;
    } else {
        var lhsSpace: BigIntSpace = undefined;
        var rhsSpace: BigIntSpace = undefined;
        const lhsBigInt = lhs.toBigInt(&lhsSpace, ctx);
        const rhsBigInt = rhs.toBigInt(&rhsSpace, ctx);

        const limbs = try ctx.comp.gpa.alloc(std.math.big.Limb, std.math.big.int.calcTwosCompLimbCount(bits));
        defer ctx.comp.gpa.free(limbs);

        var result = BigIntMutable{ .limbs = limbs, .positive = undefined, .len = undefined };
        const overflowed = result.subWrap(lhsBigInt, rhsBigInt, ty.signedness(ctx.comp), bits);
        res.* = .{ .ref = try ctx.interner.put(ctx.comp.gpa, .{ .int = .{ .bigInt = result.toConst() } }) };
        return overflowed;
    }
}

pub fn mul(res: *Value, lhs: Value, rhs: Value, ty: Type, ctx: Context) !bool {
    const bits = ty.bitSizeof(ctx.comp).?;
    if (ty.isFloat()) {
        const f: Interner.Key.Float = switch (bits) {
            16 => .{ .f16 = lhs.toFloat(f16, ctx) * rhs.toFloat(f16, ctx) },
            32 => .{ .f32 = lhs.toFloat(f32, ctx) * rhs.toFloat(f32, ctx) },
            64 => .{ .f64 = lhs.toFloat(f64, ctx) * rhs.toFloat(f64, ctx) },
            80 => .{ .f80 = lhs.toFloat(f80, ctx) * rhs.toFloat(f80, ctx) },
            128 => .{ .f128 = lhs.toFloat(f128, ctx) * rhs.toFloat(f128, ctx) },
            else => unreachable,
        };
        res.* = try ctx.intern(.{ .float = f });
        return false;
    } else {
        var lhsSpace: BigIntSpace = undefined;
        var rhsSpace: BigIntSpace = undefined;
        const lhsBigInt = lhs.toBigInt(&lhsSpace, ctx);
        const rhsBigInt = rhs.toBigInt(&rhsSpace, ctx);

        const limbs = try ctx.comp.gpa.alloc(std.math.big.Limb, lhsBigInt.limbs.len + rhsBigInt.limbs.len);
        defer ctx.comp.gpa.free(limbs);

        var result = BigIntMutable{ .limbs = limbs, .positive = undefined, .len = undefined };

        const limbsBuffer = try ctx.comp.gpa.alloc(
            std.math.big.Limb,
            std.math.big.int.calcMulLimbsBufferLen(lhsBigInt.limbs.len, rhsBigInt.limbs.len, 1),
        );
        defer ctx.comp.gpa.free(limbsBuffer);

        result.mul(lhsBigInt, rhsBigInt, limbsBuffer, ctx.comp.gpa);

        const signedness = ty.signedness(ctx.comp);
        const overflowed = !result.toConst().fitsInTwosComp(signedness, bits);
        if (overflowed) {
            result.truncate(result.toConst(), signedness, bits);
        }

        res.* = try ctx.intern(.{ .int = .{ .bigInt = result.toConst() } });
        return overflowed;
    }
}

/// caller guarantees rhs != 0
pub fn div(res: *Value, lhs: Value, rhs: Value, ty: Type, ctx: Context) !bool {
    const bits = ty.bitSizeof(ctx.comp).?;
    if (ty.isFloat()) {
        const f: Interner.Key.Float = switch (bits) {
            16 => .{ .f16 = lhs.toFloat(f16, ctx) / rhs.toFloat(f16, ctx) },
            32 => .{ .f32 = lhs.toFloat(f32, ctx) / rhs.toFloat(f32, ctx) },
            64 => .{ .f64 = lhs.toFloat(f64, ctx) / rhs.toFloat(f64, ctx) },
            80 => .{ .f80 = lhs.toFloat(f80, ctx) / rhs.toFloat(f80, ctx) },
            128 => .{ .f128 = lhs.toFloat(f128, ctx) / rhs.toFloat(f128, ctx) },
            else => unreachable,
        };
        res.* = try ctx.intern(.{ .float = f });
        return false;
    } else {
        var lhsSpace: BigIntSpace = undefined;
        var rhsSpace: BigIntSpace = undefined;
        const lhsBigInt = lhs.toBigInt(&lhsSpace, ctx);
        const rhsBigInt = rhs.toBigInt(&rhsSpace, ctx);

        const limbs = try ctx.comp.gpa.alloc(std.math.big.Limb, lhsBigInt.limbs.len + rhsBigInt.limbs.len);
        defer ctx.comp.gpa.free(limbs);

        const limbsQ = try ctx.comp.gpa.alloc(std.math.big.Limb, lhsBigInt.limbs.len);
        defer ctx.comp.gpa.free(limbsQ);

        const limbsR = try ctx.comp.gpa.alloc(std.math.big.Limb, rhsBigInt.limbs.len);
        defer ctx.comp.gpa.free(limbsR);

        var result = BigIntMutable{ .limbs = limbs, .positive = undefined, .len = undefined };
        var resultQ = BigIntMutable{ .limbs = limbsQ, .positive = undefined, .len = undefined };
        var resultR = BigIntMutable{ .limbs = limbsR, .positive = undefined, .len = undefined };

        const limbsBuffer = try ctx.comp.gpa.alloc(
            std.math.big.Limb,
            std.math.big.int.calcDivLimbsBufferLen(lhsBigInt.limbs.len, rhsBigInt.limbs.len),
        );
        defer ctx.comp.gpa.free(limbsBuffer);

        resultQ.divTrunc(&resultR, lhsBigInt, rhsBigInt, limbsBuffer);

        res.* = try ctx.comp.intern(.{ .int = .{ .bitInt = result.toConst() } });
        return !resultQ.toConst().fitsInTwosComp(ty.signedness(ctx.comp), bits);
    }
}

pub fn bitOr(lhs: Value, rhs: Value, ctx: Context) !Value {
    var lhsSpace: BigIntSpace = undefined;
    var rhsSpace: BigIntSpace = undefined;
    const lhsBigInt = lhs.toBigInt(&lhsSpace, ctx);
    const rhsBigInt = rhs.toBigInt(&rhsSpace, ctx);

    const limbs = try ctx.comp.gpa.alloc(std.math.big.Limb, @max(lhsBigInt.limbs.len, rhsBigInt.limbs.len));
    defer ctx.comp.gpa.free(limbs);

    var result = BigIntMutable{ .limbs = limbs, .positive = undefined, .len = undefined };
    result.bitOr(lhsBigInt, rhsBigInt);

    return ctx.comp.intern(.{ .int = .{ .bigInt = result.toConst() } });
}

pub fn bitXor(lhs: Value, rhs: Value, ctx: Context) !Value {
    var lhsSpace: BigIntSpace = undefined;
    var rhsSpace: BigIntSpace = undefined;
    const lhsBigInt = lhs.toBigInt(&lhsSpace, ctx);
    const rhsBigInt = rhs.toBigInt(&rhsSpace, ctx);

    const limbs = try ctx.comp.gpa.alloc(std.math.big.Limb, @max(lhsBigInt.limbs.len, rhsBigInt.limbs.len));
    defer ctx.comp.gpa.free(limbs);

    var result = BigIntMutable{ .limbs = limbs, .positive = undefined, .len = undefined };
    result.bitXor(lhsBigInt, rhsBigInt);

    return ctx.intern(.{ .int = .{ .bigInt = result.toConst() } });
}

pub fn bitAnd(lhs: Value, rhs: Value, ctx: Context) !Value {
    var lhsSpace: BigIntSpace = undefined;
    var rhsSpace: BigIntSpace = undefined;
    const lhsBigInt = lhs.toBigInt(&lhsSpace, ctx);
    const rhsBigInt = rhs.toBigInt(&rhsSpace, ctx);

    const limbs = try ctx.comp.gpa.alloc(std.math.big.Limb, @max(lhsBigInt.limbs.len, rhsBigInt.limbs.len));
    defer ctx.comp.gpa.free(limbs);

    var result = BigIntMutable{ .limbs = limbs, .positive = undefined, .len = undefined };
    result.bitAnd(lhsBigInt, rhsBigInt);

    return ctx.intern(.{ .int = .{ .bigInt = result.toConst() } });
}

pub fn bitNot(val: Value, ty: Type, ctx: Context) !Value {
    const bits = ty.bitSizeof(ctx.comp).?;
    var valSpace: Value.BigIntSpace = undefined;
    const valBigInt = val.toBigInt(&valSpace, ctx);

    const limbs = try ctx.comp.gpa.alloc(std.math.big.Limb, std.math.big.int.calcTwosCompLimbCount(bits));
    defer ctx.comp.gpa.free(limbs);

    var result = BigIntMutable{ .limbs = limbs, .positive = undefined, .len = undefined };
    result.bitNotWrap(valBigInt, ty.signedness(ctx.comp), bits);

    return ctx.intern(.{ .int = .{ .bigInt = result.toConst() } });
}

pub fn compare(lhs: Value, op: std.math.CompareOperator, rhs: Value, ctx: *Context) bool {
    const lhsKey = ctx.interner.get(lhs.ref());
    const rhsKey = ctx.interner.get(rhs.ref());
    if (lhsKey == .float or rhsKey == .float) {
        const lhsF128 = lhs.toFloat(f128, ctx);
        const rhsF128 = rhs.toFloat(f128, ctx);
        return std.math.compare(lhsF128, op, rhsF128);
    }

    var lhsBigIntSpace: BigIntSpace = undefined;
    var rhsBigIntSpace: BigIntSpace = undefined;
    const lhsBigInt = lhs.toBigInt(&lhsBigIntSpace, ctx);
    const rhsBigInt = rhs.toBigInt(&rhsBigIntSpace, ctx);
    return lhsBigInt.order(rhsBigInt).compare(op);
}

pub fn hash(v: Value) u64 {
    switch (v.tag) {
        .unavailable => unreachable,
        .int => return std.hash.Wyhash.hash(0, std.mem.asBytes(&v.data.int)),
        else => @panic("TODO"),
    }
}

pub fn print(v: Value, ctx: Context, w: anytype) @TypeOf(w).Error!void {
    const key = ctx.interner.get(v.ref());
    switch (key) {
        .null => return w.writeAll("nullptr_t"),
        .int => |repr| switch (repr) {
            inline else => |x| return w.print("{d}", .{x}),
        },
        .float => |repr| switch (repr) {
            inline else => |x| return w.print("{d}", .{@as(f64, @floatCast(x))}),
        },
        .bytes => |b| return std.zig.fmt.stringEscape(b, "", .{}, w),
        else => unreachable, // not a value
    }
}
