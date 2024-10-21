const std = @import("std");
const assert = std.debug.assert;
const Compilation = @import("../Basic/Compilation.zig");
const Type = @import("Type.zig");
const Interner = @import("../CodeGen/Interner.zig");
const BigIntSpace = Interner.Tag.Int.BigIntSpace;
const BigIntConst = std.math.big.int.Const;
const BigIntMutable = std.math.big.int.Mutable;
const TargetUtil = @import("../Basic/Target.zig");

const Value = @This();

optRef: Interner.OptRef = .none,

pub const zero = Value{ .optRef = .zero };
pub const one = Value{ .optRef = .one };
pub const @"null" = Value{ .optRef = .null };

pub fn intern(comp: *Compilation, k: Interner.Key) !Value {
    const r = try comp.interner.put(comp.gpa, k);
    return .{ .optRef = @enumFromInt(@intFromEnum(r)) };
}

pub fn isNone(v: Value) bool {
    return v.optRef == .none;
}

pub fn isNumeric(v: Value, comp: *Compilation) bool {
    const key = comp.interner.get(v.ref());
    return switch (key) {
        .int, .float => true,
        else => false,
    };
}

pub fn isZero(v: Value, comp: *const Compilation) bool {
    if (v.isNone()) return false;
    switch (v.ref()) {
        .zero => return true,
        .one => return false,
        .null => return TargetUtil.nullRepr(comp.target) == 0,
        else => {},
    }

    const key = comp.interner.get(v.ref());
    switch (key) {
        .float => |repr| switch (repr) {
            inline else => |data| return data == 0,
        },
        .int => |repr| switch (repr) {
            inline .i64, .u64 => |data| return data == 0,
            .bigInt => |data| return data.eqlZero(),
        },
        .bytes => return false,
        else => unreachable,
    }
}

pub fn int(i: anytype, comp: *Compilation) !Value {
    const info = @typeInfo(@TypeOf(i));
    if (info == .comptime_int or info.int.signedness == .unsigned) {
        return intern(comp, .{ .int = .{ .u64 = i } });
    } else {
        return intern(comp, .{ .int = .{ .i64 = i } });
    }
}

pub fn ref(v: Value) Interner.Ref {
    std.debug.assert(v.optRef != .none);
    return @enumFromInt(@intFromEnum(v.optRef));
}

pub fn is(v: Value, tag: std.meta.Tag(Interner.Key), comp: *const Compilation) bool {
    if (v.optRef == .none) return false;
    return comp.interner.get(v.ref()) == tag;
}

/// Number of bits needed to hold `v`.
/// Asserts that `v` is not negative
pub fn minUnsignedBits(v: Value, comp: *const Compilation) usize {
    var space: BigIntSpace = undefined;
    const big = v.toBigInt(&space, comp);
    assert(big.positive);
    return big.bitCountAbs();
}

test "minUnsignedBits" {
    const Test = struct {
        fn checkIntBits(comp: *const Compilation, v: u64, expected: usize) !void {
            const val = try intern(comp, .{ .int = .{ .u64 = v } });
            try std.testing.expectEqual(expected, val.minUnsignedBits(comp));
        }
    };

    var comp = Compilation.init(std.testing.allocator);
    defer comp.deinit();
    const targetQuery = try std.Target.Query.parse(.{ .arch_os_abi = "x86_64-linux-gnu" });
    comp.target = try std.zig.system.resolveTargetQuery(targetQuery);

    try Test.checkIntBits(comp, .int, 0, 0);
    try Test.checkIntBits(comp, .int, 1, 1);
    try Test.checkIntBits(comp, .int, 2, 2);
    try Test.checkIntBits(comp, .int, std.math.maxInt(i8), 7);
    try Test.checkIntBits(comp, .int, std.math.maxInt(u8), 8);
    try Test.checkIntBits(comp, .int, std.math.maxInt(i16), 15);
    try Test.checkIntBits(comp, .int, std.math.maxInt(u16), 16);
    try Test.checkIntBits(comp, .int, std.math.maxInt(i32), 31);
    try Test.checkIntBits(comp, .uint, std.math.maxInt(u32), 32);
    try Test.checkIntBits(comp, .long, std.math.maxInt(i64), 63);
    try Test.checkIntBits(comp, .ulong, std.math.maxInt(u64), 64);
    try Test.checkIntBits(comp, .long_long, std.math.maxInt(i64), 63);
    try Test.checkIntBits(comp, .ulong_long, std.math.maxInt(u64), 64);
}

/// Minimum number of bits needed to represent `v` in 2's complement notation
/// Asserts that `v` is negative.
pub fn minSignedBits(v: Value, comp: *const Compilation) usize {
    var space: BigIntSpace = undefined;
    const big = v.toBigInt(&space, comp);
    assert(!big.positive);
    return big.bitCountTwosComp();
}

test "minSignedBits" {
    const Test = struct {
        fn checkIntBits(comp: *const Compilation, v: i64, expected: usize) !void {
            const val = try intern(comp, .{ .int = .{ .i64 = v } });
            try std.testing.expectEqual(expected, val.minSignedBits(comp));
        }
    };

    var comp = Compilation.init(std.testing.allocator);
    defer comp.deinit();
    const targetQuery = try std.Target.Query.parse(.{ .arch_os_abi = "x86_64-linux-gnu" });
    comp.target = try std.zig.system.resolveTargetQuery(targetQuery);

    try Test.checkIntBits(comp, -1, 1);
    try Test.checkIntBits(comp, -2, 2);
    try Test.checkIntBits(comp, -10, 5);
    try Test.checkIntBits(comp, -101, 8);
    try Test.checkIntBits(comp, std.math.minInt(i8), 8);
    try Test.checkIntBits(comp, std.math.minInt(i16), 16);
    try Test.checkIntBits(comp, std.math.minInt(i32), 32);
    try Test.checkIntBits(comp, std.math.minInt(i64), 64);
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

/// Converts the stored value from a float to an integer.
/// `.unavailable` value remains unchanged.
pub fn floatToInt(v: *Value, destTy: Type, comp: *Compilation) !FloatToIntChangeKind {
    if (v.optRef == .none) return .none;

    const floatVal = v.toFloat(f128, comp);
    const wasZero = floatVal == 0;

    if (destTy.is(.Bool)) {
        const wasOne = floatVal == 1.0;
        v.* = fromBool(!wasZero);
        if (wasZero or wasOne) return .none;
        return .valueChanged;
    } else if (destTy.isUnsignedInt(comp) and v.compare(.lt, zero, comp)) {
        v.* = zero;
        return .outOfRange;
    }

    const hadFraction = @rem(floatVal, 1) != 0;
    const isNegative = std.math.signbit(floatVal);
    const floored = @floor(@abs(floatVal));

    var rational = try std.math.big.Rational.init(comp.gpa);
    defer rational.q.deinit();
    rational.setFloat(f128, floored) catch |err| switch (err) {
        error.NonFiniteFloat => {
            v.* = .{};
            return .overflow;
        },
        error.OutOfMemory => return error.OutOfMemory,
    };

    // The float is reduced in rational.setFloat, so we assert that denominator is equal to one
    const bigOne = BigIntConst{ .limbs = &.{1}, .positive = true };
    assert(rational.q.toConst().eqlAbs(bigOne));

    if (isNegative) {
        rational.negate();
    }

    const signedness = destTy.signedness(comp);
    const bits = destTy.bitSizeof(comp).?;

    // rational.p.truncate(rational.p.toConst(), signedness: Signedness, bit_count: usize)
    const fits = rational.p.fitsInTwosComp(signedness, bits);
    try rational.p.truncate(&rational.p, signedness, bits);
    v.* = try intern(comp, .{ .int = .{ .bigInt = rational.p.toConst() } });

    if (!wasZero and v.isZero(comp)) return .nonZeroToZero;
    if (!fits) return .outOfRange;
    if (hadFraction) return .valueChanged;
    return .none;
}

/// Converts the stored value from an integer to a float.
/// `.none` value remains unchanged.
pub fn intToFloat(v: *Value, destTy: Type, comp: *Compilation) !void {
    if (v.optRef == .none) return;
    const bits = destTy.bitSizeof(comp).?;
    return switch (comp.interner.get(v.ref()).int) {
        inline .u64, .i64 => |data| {
            const f: Interner.Key.Float = switch (bits) {
                16 => .{ .f16 = @floatFromInt(data) },
                32 => .{ .f32 = @floatFromInt(data) },
                64 => .{ .f64 = @floatFromInt(data) },
                80 => .{ .f80 = @floatFromInt(data) },
                128 => .{ .f128 = @floatFromInt(data) },
                else => unreachable,
            };
            v.* = try intern(comp, .{ .float = f });
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
            v.* = try intern(comp, .{ .float = f });
        },
    };
}

/// `.none` value remains unchanged.
pub fn intCast(v: *Value, destTy: Type, comp: *Compilation) !void {
    if (v.optRef == .none) return;

    const bits = destTy.bitSizeof(comp).?;
    var space: BigIntSpace = undefined;
    const big = v.toBigInt(&space, comp);

    const limbs = try comp.gpa.alloc(
        std.math.big.Limb,
        std.math.big.int.calcTwosCompLimbCount(bits),
    );
    defer comp.gpa.free(limbs);

    var result = std.math.big.int.Mutable{ .limbs = limbs, .positive = undefined, .len = undefined };
    result.truncate(big, destTy.signedness(comp), bits);

    v.* = try intern(comp, .{ .int = .{ .bigInt = result.toConst() } });
}

/// `.none` value remains unchanged.
pub fn floatCast(v: *Value, dest_ty: Type, comp: *Compilation) !void {
    if (v.optRef == .none) return;
    const bits = dest_ty.bitSizeof(comp).?;
    const f: Interner.Key.Float = switch (bits) {
        16 => .{ .f16 = v.toFloat(f16, comp) },
        32 => .{ .f32 = v.toFloat(f32, comp) },
        64 => .{ .f64 = v.toFloat(f64, comp) },
        80 => .{ .f80 = v.toFloat(f80, comp) },
        128 => .{ .f128 = v.toFloat(f128, comp) },
        else => unreachable,
    };
    v.* = try intern(comp, .{ .float = f });
}

pub fn toBigInt(val: Value, space: *BigIntSpace, comp: *const Compilation) BigIntConst {
    return switch (comp.interner.get(val.ref()).int) {
        inline .u64, .i64 => |x| BigIntMutable.init(&space.limbs, x).toConst(),
        .bigInt => |b| b,
    };
}

pub fn toFloat(v: Value, comptime T: type, comp: *const Compilation) T {
    return switch (comp.interner.get(v.ref())) {
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
/// `.none` value remains unchanged.
pub fn boolCast(v: *Value, comp: *const Compilation) void {
    if (v.isNone()) return;
    v.* = fromBool(v.toBool(comp));
}

pub fn fromBool(b: bool) Value {
    return if (b) one else zero;
}

pub fn toBool(v: Value, comp: *const Compilation) bool {
    return !v.isZero(comp);
}

pub fn toInt(v: Value, comptime T: type, comp: *const Compilation) ?T {
    if (v.optRef == .none) return null;
    if (comp.interner.get(v.ref()) != .int) return null;

    var space: BigIntSpace = undefined;
    const bigInt = v.toBigInt(&space, comp);
    return bigInt.to(T) catch null;
}

pub fn add(res: *Value, lhs: Value, rhs: Value, ty: Type, comp: *Compilation) !bool {
    const bits = ty.bitSizeof(comp).?;
    if (ty.isFloat()) {
        const f: Interner.Key.Float = switch (bits) {
            16 => .{ .f16 = lhs.toFloat(f16, comp) + rhs.toFloat(f16, comp) },
            32 => .{ .f32 = lhs.toFloat(f32, comp) + rhs.toFloat(f32, comp) },
            64 => .{ .f64 = lhs.toFloat(f64, comp) + rhs.toFloat(f64, comp) },
            80 => .{ .f80 = lhs.toFloat(f80, comp) + rhs.toFloat(f80, comp) },
            128 => .{ .f128 = lhs.toFloat(f128, comp) + rhs.toFloat(f128, comp) },
            else => unreachable,
        };
        res.* = try intern(comp, .{ .float = f });
        return false;
    } else {
        var lhsSpace: BigIntSpace = undefined;
        var rhsSpace: BigIntSpace = undefined;
        const lhsBigInt = lhs.toBigInt(&lhsSpace, comp);
        const rhsBigInt = rhs.toBigInt(&rhsSpace, comp);

        const limbs = try comp.gpa.alloc(
            std.math.big.Limb,
            std.math.big.int.calcTwosCompLimbCount(bits),
        );
        defer comp.gpa.free(limbs);

        var result = BigIntMutable{ .limbs = limbs, .positive = undefined, .len = undefined };
        const overflowed = result.addWrap(lhsBigInt, rhsBigInt, ty.signedness(comp), bits);
        res.* = try intern(comp, .{ .int = .{ .bigInt = result.toConst() } });
        return overflowed;
    }
}

pub fn sub(res: *Value, lhs: Value, rhs: Value, ty: Type, comp: *Compilation) !bool {
    const bits = ty.bitSizeof(comp).?;
    if (ty.isFloat()) {
        const f: Interner.Key.Float = switch (bits) {
            16 => .{ .f16 = lhs.toFloat(f16, comp) - rhs.toFloat(f16, comp) },
            32 => .{ .f32 = lhs.toFloat(f32, comp) - rhs.toFloat(f32, comp) },
            64 => .{ .f64 = lhs.toFloat(f64, comp) - rhs.toFloat(f64, comp) },
            80 => .{ .f80 = lhs.toFloat(f80, comp) - rhs.toFloat(f80, comp) },
            128 => .{ .f128 = lhs.toFloat(f128, comp) - rhs.toFloat(f128, comp) },
            else => unreachable,
        };
        res.* = try intern(comp, .{ .float = f });
        return false;
    } else {
        var lhsSpace: BigIntSpace = undefined;
        var rhsSpace: BigIntSpace = undefined;
        const lhsBigInt = lhs.toBigInt(&lhsSpace, comp);
        const rhsBigInt = rhs.toBigInt(&rhsSpace, comp);

        const limbs = try comp.gpa.alloc(
            std.math.big.Limb,
            std.math.big.int.calcTwosCompLimbCount(bits),
        );
        defer comp.gpa.free(limbs);

        var result = BigIntMutable{ .limbs = limbs, .positive = undefined, .len = undefined };
        const overflowed = result.subWrap(lhsBigInt, rhsBigInt, ty.signedness(comp), bits);
        res.* = try intern(comp, .{ .int = .{ .bigInt = result.toConst() } });
        return overflowed;
    }
}

pub fn mul(res: *Value, lhs: Value, rhs: Value, ty: Type, comp: *Compilation) !bool {
    const bits = ty.bitSizeof(comp).?;
    if (ty.isFloat()) {
        const f: Interner.Key.Float = switch (bits) {
            16 => .{ .f16 = lhs.toFloat(f16, comp) * rhs.toFloat(f16, comp) },
            32 => .{ .f32 = lhs.toFloat(f32, comp) * rhs.toFloat(f32, comp) },
            64 => .{ .f64 = lhs.toFloat(f64, comp) * rhs.toFloat(f64, comp) },
            80 => .{ .f80 = lhs.toFloat(f80, comp) * rhs.toFloat(f80, comp) },
            128 => .{ .f128 = lhs.toFloat(f128, comp) * rhs.toFloat(f128, comp) },
            else => unreachable,
        };
        res.* = try intern(comp, .{ .float = f });
        return false;
    } else {
        var lhsSpace: BigIntSpace = undefined;
        var rhsSpace: BigIntSpace = undefined;
        const lhsBigInt = lhs.toBigInt(&lhsSpace, comp);
        const rhsBigInt = rhs.toBigInt(&rhsSpace, comp);

        const limbs = try comp.gpa.alloc(std.math.big.Limb, lhsBigInt.limbs.len + rhsBigInt.limbs.len);
        defer comp.gpa.free(limbs);

        var result = BigIntMutable{ .limbs = limbs, .positive = undefined, .len = undefined };

        const limbsBuffer = try comp.gpa.alloc(
            std.math.big.Limb,
            std.math.big.int.calcMulLimbsBufferLen(lhsBigInt.limbs.len, rhsBigInt.limbs.len, 1),
        );
        defer comp.gpa.free(limbsBuffer);

        result.mul(lhsBigInt, rhsBigInt, limbsBuffer, comp.gpa);

        const signedness = ty.signedness(comp);
        const overflowed = !result.toConst().fitsInTwosComp(signedness, bits);
        if (overflowed) {
            result.truncate(result.toConst(), signedness, bits);
        }

        res.* = try intern(comp, .{ .int = .{ .bigInt = result.toConst() } });
        return overflowed;
    }
}

/// caller guarantees rhs != 0
pub fn div(res: *Value, lhs: Value, rhs: Value, ty: Type, comp: *Compilation) !bool {
    const bits = ty.bitSizeof(comp).?;
    if (ty.isFloat()) {
        const f: Interner.Key.Float = switch (bits) {
            16 => .{ .f16 = lhs.toFloat(f16, comp) / rhs.toFloat(f16, comp) },
            32 => .{ .f32 = lhs.toFloat(f32, comp) / rhs.toFloat(f32, comp) },
            64 => .{ .f64 = lhs.toFloat(f64, comp) / rhs.toFloat(f64, comp) },
            80 => .{ .f80 = lhs.toFloat(f80, comp) / rhs.toFloat(f80, comp) },
            128 => .{ .f128 = lhs.toFloat(f128, comp) / rhs.toFloat(f128, comp) },
            else => unreachable,
        };
        res.* = try intern(comp, .{ .float = f });
        return false;
    } else {
        var lhsSpace: BigIntSpace = undefined;
        var rhsSpace: BigIntSpace = undefined;
        const lhsBigInt = lhs.toBigInt(&lhsSpace, comp);
        const rhsBigInt = rhs.toBigInt(&rhsSpace, comp);

        const limbs = try comp.gpa.alloc(std.math.big.Limb, lhsBigInt.limbs.len + rhsBigInt.limbs.len);
        defer comp.gpa.free(limbs);

        const limbsQ = try comp.gpa.alloc(std.math.big.Limb, lhsBigInt.limbs.len);
        defer comp.gpa.free(limbsQ);

        const limbsR = try comp.gpa.alloc(std.math.big.Limb, rhsBigInt.limbs.len);
        defer comp.gpa.free(limbsR);

        var result = BigIntMutable{ .limbs = limbs, .positive = undefined, .len = undefined };
        var resultQ = BigIntMutable{ .limbs = limbsQ, .positive = undefined, .len = undefined };
        var resultR = BigIntMutable{ .limbs = limbsR, .positive = undefined, .len = undefined };

        const limbsBuffer = try comp.gpa.alloc(
            std.math.big.Limb,
            std.math.big.int.calcDivLimbsBufferLen(lhsBigInt.limbs.len, rhsBigInt.limbs.len),
        );
        defer comp.gpa.free(limbsBuffer);

        resultQ.divTrunc(&resultR, lhsBigInt, rhsBigInt, limbsBuffer);

        res.* = try intern(comp, .{ .int = .{ .bigInt = result.toConst() } });
        return !resultQ.toConst().fitsInTwosComp(ty.signedness(comp), bits);
    }
}

/// caller guarantees rhs != 0
/// caller guarantees lhs != std.math.minInt(T) OR rhs != -1
pub fn rem(lhs: Value, rhs: Value, ty: Type, comp: *Compilation) !Value {
    var lhsSpace: BigIntSpace = undefined;
    var rhsSpace: BigIntSpace = undefined;
    const lhsBigInt = lhs.toBigInt(&lhsSpace, comp);
    const rhsBigInt = rhs.toBigInt(&rhsSpace, comp);

    const signedness = ty.signedness(comp);
    if (signedness == .signed) {
        var spaces: [3]BigIntSpace = undefined;
        const minVal = BigIntMutable.init(&spaces[0].limbs, ty.minInt(comp)).toConst();
        const negative = BigIntMutable.init(&spaces[1].limbs, -1).toConst();
        const bigOne = BigIntMutable.init(&spaces[2].limbs, 1).toConst();
        if (lhsBigInt.eql(minVal) and rhsBigInt.eql(negative)) {
            return .{};
        } else if (rhsBigInt.order(bigOne).compare(.lt)) {
            // lhs - @divTrunc(lhs, rhs) * rhs
            var tmp: Value = undefined;
            _ = try tmp.div(lhs, rhs, ty, comp);
            _ = try tmp.mul(tmp, rhs, ty, comp);
            _ = try tmp.sub(lhs, tmp, ty, comp);
            return tmp;
        }
    }

    const limbsQ = try comp.gpa.alloc(std.math.big.Limb, lhsBigInt.limbs.len);
    defer comp.gpa.free(limbsQ);
    var resultQ = BigIntMutable{ .limbs = limbsQ, .positive = undefined, .len = undefined };

    const limbsR = try comp.gpa.alloc(std.math.big.Limb, rhsBigInt.limbs.len);
    defer comp.gpa.free(limbsR);
    var resultR = BigIntMutable{ .limbs = limbsR, .positive = undefined, .len = undefined };

    const limbsBuffer = try comp.gpa.alloc(
        std.math.big.Limb,
        std.math.big.int.calcDivLimbsBufferLen(lhsBigInt.limbs.len, rhsBigInt.limbs.len),
    );
    defer comp.gpa.free(limbsBuffer);

    resultQ.divTrunc(&resultR, lhsBigInt, rhsBigInt, limbsBuffer);
    return try intern(comp, .{ .int = .{ .bigInt = resultR.toConst() } });
}

pub fn bitOr(lhs: Value, rhs: Value, comp: *Compilation) !Value {
    var lhsSpace: BigIntSpace = undefined;
    var rhsSpace: BigIntSpace = undefined;
    const lhsBigInt = lhs.toBigInt(&lhsSpace, comp);
    const rhsBigInt = rhs.toBigInt(&rhsSpace, comp);

    const limbs = try comp.gpa.alloc(std.math.big.Limb, @max(lhsBigInt.limbs.len, rhsBigInt.limbs.len));
    defer comp.gpa.free(limbs);

    var result = BigIntMutable{ .limbs = limbs, .positive = undefined, .len = undefined };
    result.bitOr(lhsBigInt, rhsBigInt);

    return intern(comp, .{ .int = .{ .bigInt = result.toConst() } });
}

pub fn bitXor(lhs: Value, rhs: Value, comp: *Compilation) !Value {
    var lhsSpace: BigIntSpace = undefined;
    var rhsSpace: BigIntSpace = undefined;
    const lhsBigInt = lhs.toBigInt(&lhsSpace, comp);
    const rhsBigInt = rhs.toBigInt(&rhsSpace, comp);

    const limbs = try comp.gpa.alloc(std.math.big.Limb, @max(lhsBigInt.limbs.len, rhsBigInt.limbs.len));
    defer comp.gpa.free(limbs);

    var result = BigIntMutable{ .limbs = limbs, .positive = undefined, .len = undefined };
    result.bitXor(lhsBigInt, rhsBigInt);

    return intern(comp, .{ .int = .{ .bigInt = result.toConst() } });
}

pub fn bitAnd(lhs: Value, rhs: Value, comp: *Compilation) !Value {
    var lhsSpace: BigIntSpace = undefined;
    var rhsSpace: BigIntSpace = undefined;
    const lhsBigInt = lhs.toBigInt(&lhsSpace, comp);
    const rhsBigInt = rhs.toBigInt(&rhsSpace, comp);

    const limbs = try comp.gpa.alloc(std.math.big.Limb, @max(lhsBigInt.limbs.len, rhsBigInt.limbs.len));
    defer comp.gpa.free(limbs);

    var result = BigIntMutable{ .limbs = limbs, .positive = undefined, .len = undefined };
    result.bitAnd(lhsBigInt, rhsBigInt);

    return intern(comp, .{ .int = .{ .bigInt = result.toConst() } });
}

pub fn bitNot(val: Value, ty: Type, comp: *Compilation) !Value {
    const bits = ty.bitSizeof(comp).?;
    var valSpace: Value.BigIntSpace = undefined;
    const valBigInt = val.toBigInt(&valSpace, comp);

    const limbs = try comp.gpa.alloc(std.math.big.Limb, std.math.big.int.calcTwosCompLimbCount(bits));
    defer comp.gpa.free(limbs);

    var result = BigIntMutable{ .limbs = limbs, .positive = undefined, .len = undefined };
    result.bitNotWrap(valBigInt, ty.signedness(comp), bits);

    return intern(comp, .{ .int = .{ .bigInt = result.toConst() } });
}

pub fn shl(res: *Value, lhs: Value, rhs: Value, ty: Type, comp: *Compilation) !bool {
    var lhsSpace: Value.BigIntSpace = undefined;
    const lhsBigInt = lhs.toBigInt(&lhsSpace, comp);
    const shift = rhs.toInt(usize, comp) orelse std.math.maxInt(usize);

    const bits = ty.bitSizeof(comp).?;
    if (shift > bits) {
        if (lhsBigInt.positive) {
            res.* = try intern(comp, .{ .int = .{ .u64 = ty.maxInt(comp) } });
        } else {
            res.* = try intern(comp, .{ .int = .{ .i64 = ty.minInt(comp) } });
        }
        return true;
    }

    const limbs = try comp.gpa.alloc(
        std.math.big.Limb,
        lhsBigInt.limbs.len + (shift / (@sizeOf(std.math.big.Limb) * 8)) + 1,
    );
    defer comp.gpa.free(limbs);

    var result = BigIntMutable{ .limbs = limbs, .positive = undefined, .len = undefined };
    result.shiftLeft(lhsBigInt, shift);

    const signedness = ty.signedness(comp);
    const overflowed = !result.toConst().fitsInTwosComp(signedness, bits);
    if (overflowed)
        result.truncate(result.toConst(), signedness, bits);

    res.* = try intern(comp, .{ .int = .{ .bigInt = result.toConst() } });
    return overflowed;
}

pub fn shr(lhs: Value, rhs: Value, ty: Type, comp: *Compilation) !Value {
    var lhsSpace: Value.BigIntSpace = undefined;
    const lhsBigInt = lhs.toBigInt(&lhsSpace, comp);
    const shift = rhs.toInt(usize, comp) orelse return zero;

    const resultLimbs = lhsBigInt.limbs.len -| (shift / (@sizeOf(std.math.big.Limb) * 8));
    if (resultLimbs == 0) {
        // The shift is enough to remove all the bits from the number, which means the
        // result is 0 or -1 depending on the sign.
        if (lhsBigInt.positive) {
            return zero;
        } else {
            return intern(comp, .{ .int = .{ .i64 = -1 } });
        }
    }

    const bits = ty.bitSizeof(comp).?;
    const limbs = try comp.gpa.alloc(
        std.math.big.Limb,
        std.math.big.int.calcTwosCompLimbCount(bits),
    );
    defer comp.gpa.free(limbs);

    var result = BigIntMutable{ .limbs = limbs, .positive = undefined, .len = undefined };
    result.shiftRight(lhsBigInt, shift);

    return intern(comp, .{ .int = .{ .bigInt = result.toConst() } });
}

pub fn compare(lhs: Value, op: std.math.CompareOperator, rhs: Value, comp: *Compilation) bool {
    if (op == .eq) {
        return lhs.optRef == rhs.optRef;
    } else if (lhs.optRef == rhs.optRef) {
        return std.math.Order.eq.compare(op);
    }

    const lhsKey = comp.interner.get(lhs.ref());
    const rhsKey = comp.interner.get(rhs.ref());
    if (lhsKey == .float or rhsKey == .float) {
        const lhsF128 = lhs.toFloat(f128, comp);
        const rhsF128 = rhs.toFloat(f128, comp);
        return std.math.compare(lhsF128, op, rhsF128);
    }

    var lhsBigIntSpace: BigIntSpace = undefined;
    var rhsBigIntSpace: BigIntSpace = undefined;
    const lhsBigInt = lhs.toBigInt(&lhsBigIntSpace, comp);
    const rhsBigInt = rhs.toBigInt(&rhsBigIntSpace, comp);

    return lhsBigInt.order(rhsBigInt).compare(op);
}

pub fn hash(v: Value) u64 {
    switch (v.tag) {
        .unavailable => unreachable,
        .int => return std.hash.Wyhash.hash(0, std.mem.asBytes(&v.data.int)),
        else => @panic("TODO"),
    }
}

pub fn print(v: Value, comp: *Compilation, w: anytype) @TypeOf(w).Error!void {
    const key = comp.interner.get(v.ref());
    switch (key) {
        .null => return w.writeAll("nullptr_t"),
        .int => |repr| switch (repr) {
            inline else => |x| return w.print("{d}", .{x}),
        },
        .float => |repr| switch (repr) {
            inline else => |x| return w.print("{d}", .{@as(f64, @floatCast(x))}),
        },
        .bytes => |b| return printString(b, Type.Char, comp, w),
        else => unreachable, // not a value
    }
}

pub fn printString(bytes: []const u8, elemTy: Type, comp: *Compilation, w: anytype) @TypeOf(w).Error!void {
    const size: Compilation.CharUnitSize = @enumFromInt(elemTy.sizeof(comp).?);
    const withoutNull = bytes[0 .. bytes.len - @intFromEnum(size)];
    switch (size) {
        inline .@"1", .@"2" => |sz| {
            const dataSlice: []const sz.Type() = @alignCast(std.mem.bytesAsSlice(sz.Type(), withoutNull));
            const formatter = if (sz == .@"1") std.zig.fmtEscapes(dataSlice) else std.unicode.fmtUtf16Le(dataSlice);
            try w.print("\"{}\"", .{formatter});
        },
        .@"4" => {
            try w.writeByte('"');
            const dataSlice = std.mem.bytesAsSlice(u32, withoutNull);
            var buf: [4]u8 = undefined;
            for (dataSlice) |item| {
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
