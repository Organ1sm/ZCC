const std = @import("std");
const assert = std.debug.assert;
const BigIntConst = std.math.big.int.Const;
const BigIntMutable = std.math.big.int.Mutable;

const backend = @import("backend");
const Interner = backend.Interner;
const BigIntSpace = Interner.Tag.Int.BigIntSpace;

const AnnexG = @import("../AnnexG.zig");
const Compilation = @import("../Basic/Compilation.zig");
const TargetUtil = @import("../Basic/Target.zig");
const QualType = @import("TypeStore.zig").QualType;

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

pub fn isNull(v: Value) bool {
    return v.optRef == .null;
}

pub fn isNumeric(v: Value, comp: *Compilation) bool {
    if (v.isNone()) return false;
    return switch (comp.interner.get(v.ref())) {
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
        .complex => |repr| switch (repr) {
            inline else => |data| return data[0] == 0.0 and data[1] == 0.0,
        },
        .bytes => return false,
        .pointer => return false,
        else => unreachable,
    }
}

pub fn is(v: Value, tag: std.meta.Tag(Interner.Key), comp: *const Compilation) bool {
    if (v.isNone()) return false;
    return comp.interner.get(v.ref()) == tag;
}

pub fn isArithmetic(v: Value, comp: *const Compilation) bool {
    if (v.isNone()) return false;
    return switch (comp.interner.get(v.ref())) {
        .int, .float, .complex => true,
        else => false,
    };
}

pub fn int(i: anytype, comp: *Compilation) !Value {
    const info = @typeInfo(@TypeOf(i));
    if (info == .comptime_int or info.int.signedness == .unsigned) {
        return intern(comp, .{ .int = .{ .u64 = i } });
    } else {
        return intern(comp, .{ .int = .{ .i64 = i } });
    }
}

pub fn pointer(r: Interner.Key.Pointer, comp: *Compilation) !Value {
    return intern(comp, .{ .pointer = r });
}

pub fn ref(v: Value) Interner.Ref {
    std.debug.assert(v.optRef != .none);
    return @enumFromInt(@intFromEnum(v.optRef));
}

pub fn fromRef(r: Interner.Ref) Value {
    return .{ .optRef = @enumFromInt(@intFromEnum(r)) };
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
        fn checkIntBits(comp: *Compilation, v: u64, expected: usize) !void {
            const val = try intern(comp, .{ .int = .{ .u64 = v } });
            try std.testing.expectEqual(expected, val.minUnsignedBits(comp));
        }
    };

    var arenaState: std.heap.ArenaAllocator = .init(std.testing.allocator);
    defer arenaState.deinit();
    const arena = arenaState.allocator();

    var comp = Compilation.init(std.testing.allocator, arena, undefined, std.fs.cwd());
    defer comp.deinit();

    const targetQuery = try std.Target.Query.parse(.{ .arch_os_abi = "x86_64-linux-gnu" });
    comp.target = try std.zig.system.resolveTargetQuery(targetQuery);

    try Test.checkIntBits(&comp, 0, 0);
    try Test.checkIntBits(&comp, 1, 1);
    try Test.checkIntBits(&comp, 2, 2);
    try Test.checkIntBits(&comp, std.math.maxInt(i8), 7);
    try Test.checkIntBits(&comp, std.math.maxInt(u8), 8);
    try Test.checkIntBits(&comp, std.math.maxInt(i16), 15);
    try Test.checkIntBits(&comp, std.math.maxInt(u16), 16);
    try Test.checkIntBits(&comp, std.math.maxInt(i32), 31);
    try Test.checkIntBits(&comp, std.math.maxInt(u32), 32);
    try Test.checkIntBits(&comp, std.math.maxInt(i64), 63);
    try Test.checkIntBits(&comp, std.math.maxInt(u64), 64);
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
        fn checkIntBits(comp: *Compilation, v: i64, expected: usize) !void {
            const val = try intern(comp, .{ .int = .{ .i64 = v } });
            try std.testing.expectEqual(expected, val.minSignedBits(comp));
        }
    };
    var arenaState: std.heap.ArenaAllocator = .init(std.testing.allocator);
    defer arenaState.deinit();
    const arena = arenaState.allocator();

    var comp = Compilation.init(std.testing.allocator, arena, undefined, std.fs.cwd());
    defer comp.deinit();

    const targetQuery = try std.Target.Query.parse(.{ .arch_os_abi = "x86_64-linux-gnu" });
    comp.target = try std.zig.system.resolveTargetQuery(targetQuery);

    try Test.checkIntBits(&comp, -1, 1);
    try Test.checkIntBits(&comp, -2, 2);
    try Test.checkIntBits(&comp, -10, 5);
    try Test.checkIntBits(&comp, -101, 8);
    try Test.checkIntBits(&comp, std.math.minInt(i8), 8);
    try Test.checkIntBits(&comp, std.math.minInt(i16), 16);
    try Test.checkIntBits(&comp, std.math.minInt(i32), 32);
    try Test.checkIntBits(&comp, std.math.minInt(i64), 64);
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
pub fn floatToInt(v: *Value, destTy: QualType, comp: *Compilation) !FloatToIntChangeKind {
    if (v.isNone()) return .none;

    const floatVal = v.toFloat(f128, comp);
    const wasZero = floatVal == 0;

    if (destTy.is(comp, .bool)) {
        const wasOne = floatVal == 1.0;
        v.* = fromBool(!wasZero);
        if (wasZero or wasOne) return .none;
        return .valueChanged;
    } else if (destTy.isUnsigned(comp) and floatVal < 0) {
        v.* = zero;
        return .outOfRange;
    } else if (!std.math.isFinite(floatVal)) {
        v.* = .{};
        return .overflow;
    }

    const signedness = destTy.signedness(comp);
    const bits: usize = @intCast(destTy.bitSizeof(comp));

    var bigInt: BigIntMutable = .{
        .limbs = try comp.gpa.alloc(std.math.big.Limb, @max(
            std.math.big.int.calcLimbLen(floatVal),
            std.math.big.int.calcTwosCompLimbCount(bits),
        )),
        .len = undefined,
        .positive = undefined,
    };
    defer comp.gpa.free(bigInt.limbs);

    const hadFraction = switch (bigInt.setFloat(floatVal, .trunc)) {
        .inexact => true,
        .exact => false,
    };

    const fits = bigInt.toConst().fitsInTwosComp(signedness, bits);
    v.* = try intern(comp, .{ .int = .{ .bigInt = bigInt.toConst() } });
    bigInt.truncate(bigInt.toConst(), signedness, bits);

    if (!wasZero and v.isZero(comp)) return .nonZeroToZero;
    if (!fits) return .outOfRange;
    if (hadFraction) return .valueChanged;
    return .none;
}

/// Converts the stored value from an integer to a float.
/// `.none` value remains unchanged.
pub fn intToFloat(v: *Value, destTy: QualType, comp: *Compilation) !void {
    if (v.isNone()) return;

    if (destTy.is(comp, .complex)) {
        const bits = destTy.bitSizeof(comp);
        const cf: Interner.Key.Complex = switch (bits) {
            32 => .{ .cf16 = .{ v.toFloat(f16, comp), 0 } },
            64 => .{ .cf32 = .{ v.toFloat(f32, comp), 0 } },
            128 => .{ .cf64 = .{ v.toFloat(f64, comp), 0 } },
            160 => .{ .cf80 = .{ v.toFloat(f80, comp), 0 } },
            256 => .{ .cf128 = .{ v.toFloat(f128, comp), 0 } },
            else => unreachable,
        };
        v.* = try intern(comp, .{ .complex = cf });
        return;
    }
    const bits = destTy.bitSizeof(comp);
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

pub const IntCastChangeKind = enum {
    ///value did not change,
    none,
    /// Trucation occurred (e.g. i32 to i16)
    truncated,
    /// Sign conversion occurred(e.g. i32 to u32)
    signChanged,
};

/// `.none` value remains unchanged.
pub fn intCast(v: *Value, destTy: QualType, comp: *Compilation) !IntCastChangeKind {
    if (v.isNone()) return .none;

    const key = comp.interner.get(v.ref());
    if (key == .pointer) return .none;

    const destBits: usize = @intCast(destTy.bitSizeof(comp));
    const destSigned = !destTy.isUnsigned(comp);

    var space: BigIntSpace = undefined;
    const big = key.toBigInt(&space);

    const valueBits = big.bitCountTwosComp();

    const srcSigned = !big.positive;
    const signChange = srcSigned != destSigned;

    const limbs = try comp.gpa.alloc(
        std.math.big.Limb,
        std.math.big.int.calcTwosCompLimbCount(@max(valueBits, destBits)),
    );
    defer comp.gpa.free(limbs);

    var result = std.math.big.int.Mutable{ .limbs = limbs, .positive = undefined, .len = undefined };
    result.truncate(big, destTy.signedness(comp), destBits);

    v.* = try intern(comp, .{ .int = .{ .bigInt = result.toConst() } });

    const truncationOccurred = valueBits > destBits;
    if (truncationOccurred) {
        return .truncated;
    } else if (signChange) {
        return .signChanged;
    } else {
        return .none;
    }
}

pub fn imag(v: Value, comptime T: type, comp: *const Compilation) T {
    return switch (comp.interner.get(v.ref())) {
        .int => 0.0,
        .float => 0.0,
        .complex => |repr| switch (repr) {
            inline else => |components| return @floatCast(components[1]),
        },
        else => unreachable,
    };
}

/// Converts the stored value to a float of the specified type
/// `.none` value remains unchanged.
pub fn floatCast(v: *Value, destTy: QualType, comp: *Compilation) !void {
    if (v.isNone()) return;

    const bits = destTy.bitSizeof(comp);
    if (destTy.is(comp, .complex)) {
        const cf: Interner.Key.Complex = switch (bits) {
            32 => .{ .cf16 = .{ v.toFloat(f16, comp), v.imag(f16, comp) } },
            64 => .{ .cf32 = .{ v.toFloat(f32, comp), v.imag(f32, comp) } },
            128 => .{ .cf64 = .{ v.toFloat(f64, comp), v.imag(f64, comp) } },
            160 => .{ .cf80 = .{ v.toFloat(f80, comp), v.imag(f80, comp) } },
            256 => .{ .cf128 = .{ v.toFloat(f128, comp), v.imag(f128, comp) } },
            else => unreachable,
        };
        v.* = try intern(comp, .{ .complex = cf });
    } else {
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
}

fn toBigInt(val: Value, space: *BigIntSpace, comp: *const Compilation) BigIntConst {
    return comp.interner.get(val.ref()).toBigInt(space);
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
        .complex => |repr| switch (repr) {
            inline else => |components| @floatCast(components[0]),
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

pub fn realPart(v: Value, comp: *Compilation) !Value {
    if (v.isNone()) return v;
    return switch (comp.interner.get(v.ref())) {
        .int, .float => v,
        .complex => |repr| Value.intern(comp, switch (repr) {
            .cf16 => |components| .{ .float = .{ .f16 = components[0] } },
            .cf32 => |components| .{ .float = .{ .f32 = components[0] } },
            .cf64 => |components| .{ .float = .{ .f64 = components[0] } },
            .cf80 => |components| .{ .float = .{ .f80 = components[0] } },
            .cf128 => |components| .{ .float = .{ .f128 = components[0] } },
        }),
        else => unreachable,
    };
}

pub fn imaginaryPart(v: Value, comp: *Compilation) !Value {
    if (v.isNone()) return v;
    return switch (comp.interner.get(v.ref())) {
        .int, .float => Value.zero,
        .complex => |repr| Value.intern(comp, switch (repr) {
            .cf16 => |components| .{ .float = .{ .f16 = components[1] } },
            .cf32 => |components| .{ .float = .{ .f32 = components[1] } },
            .cf64 => |components| .{ .float = .{ .f64 = components[1] } },
            .cf80 => |components| .{ .float = .{ .f80 = components[1] } },
            .cf128 => |components| .{ .float = .{ .f128 = components[1] } },
        }),
        else => unreachable,
    };
}

const IsInfKind = enum(i32) {
    negative = -1,
    finite = 0,
    positive = 1,
    unknown = std.math.maxInt(i32),
};

pub fn isInfSign(v: Value, comp: *const Compilation) IsInfKind {
    if (v.isNone()) return .unknown;
    return switch (comp.interner.get(v.ref())) {
        .float => |repr| switch (repr) {
            inline else => |data| if (std.math.isPositiveInf(data)) .positive else if (std.math.isNegativeInf(data)) .negative else .finite,
        },
        else => .unknown,
    };
}

pub fn isInf(v: Value, comp: *const Compilation) bool {
    if (v.isNone()) return false;
    return switch (comp.interner.get(v.ref())) {
        .float => |repr| switch (repr) {
            inline else => |data| std.math.isInf(data),
        },
        .complex => |repr| switch (repr) {
            inline else => |components| std.math.isInf(components[0]) or std.math.isInf(components[1]),
        },
        else => false,
    };
}

pub fn isNan(v: Value, comp: *const Compilation) bool {
    if (v.isNone()) return false;
    return switch (comp.interner.get(v.ref())) {
        .float => |repr| switch (repr) {
            inline else => |data| std.math.isNan(data),
        },
        .complex => |repr| switch (repr) {
            inline else => |components| std.math.isNan(components[0]) or std.math.isNan(components[1]),
        },
        else => false,
    };
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
    if (v.isNone()) return null;

    const key = comp.interner.get(v.ref());
    if (key != .int) return null;

    var space: BigIntSpace = undefined;
    const bigInt = key.toBigInt(&space);
    return bigInt.to(T) catch null;
}

const ComplexOp = enum {
    add,
    sub,
};

fn complexAddSub(lhs: Value, rhs: Value, comptime T: type, op: ComplexOp, comp: *Compilation) !Value {
    const resRe = switch (op) {
        .add => lhs.toFloat(T, comp) + rhs.toFloat(T, comp),
        .sub => lhs.toFloat(T, comp) - rhs.toFloat(T, comp),
    };
    const resIm = switch (op) {
        .add => lhs.imag(T, comp) + rhs.imag(T, comp),
        .sub => lhs.imag(T, comp) - rhs.imag(T, comp),
    };

    return switch (T) {
        f16 => intern(comp, .{ .complex = .{ .cf16 = .{ resRe, resIm } } }),
        f32 => intern(comp, .{ .complex = .{ .cf32 = .{ resRe, resIm } } }),
        f64 => intern(comp, .{ .complex = .{ .cf64 = .{ resRe, resIm } } }),
        f80 => intern(comp, .{ .complex = .{ .cf80 = .{ resRe, resIm } } }),
        f128 => intern(comp, .{ .complex = .{ .cf128 = .{ resRe, resIm } } }),
        else => unreachable,
    };
}

pub fn add(res: *Value, lhs: Value, rhs: Value, qt: QualType, comp: *Compilation) !bool {
    const bits: usize = @intCast(qt.bitSizeof(comp));
    const scalarKind = qt.scalarKind(comp);
    if (scalarKind.isFloat()) {
        if (scalarKind == .ComplexFloat) {
            res.* = switch (bits) {
                32 => try complexAddSub(lhs, rhs, f16, .add, comp),
                64 => try complexAddSub(lhs, rhs, f32, .add, comp),
                128 => try complexAddSub(lhs, rhs, f64, .add, comp),
                160 => try complexAddSub(lhs, rhs, f80, .add, comp),
                256 => try complexAddSub(lhs, rhs, f128, .add, comp),
                else => unreachable,
            };
            return false;
        }
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
    }

    const lhsKey = comp.interner.get(lhs.ref());
    const rhsKey = comp.interner.get(rhs.ref());
    if (lhsKey == .pointer or rhsKey == .pointer) {
        const rel, const index = if (lhsKey == .pointer)
            .{ lhsKey.pointer, rhs }
        else
            .{ rhsKey.pointer, lhs };

        const elemSize = try int(qt.childType(comp).sizeofOrNull(comp) orelse 1, comp);
        var totalOffset: Value = undefined;

        const mulOverflow = try totalOffset.mul(elemSize, index, comp.typeStore.ptrdiff, comp);
        const oldOffset = fromRef(rel.offset);
        const addOverflow = try totalOffset.add(totalOffset, oldOffset, comp.typeStore.ptrdiff, comp);

        _ = try totalOffset.intCast(comp.typeStore.ptrdiff, comp);
        res.* = try pointer(.{ .node = rel.node, .offset = totalOffset.ref() }, comp);
        return mulOverflow or addOverflow;
    }

    var lhsSpace: BigIntSpace = undefined;
    var rhsSpace: BigIntSpace = undefined;
    const lhsBigInt = lhsKey.toBigInt(&lhsSpace);
    const rhsBigInt = rhsKey.toBigInt(&rhsSpace);

    const limbs = try comp.gpa.alloc(
        std.math.big.Limb,
        std.math.big.int.calcTwosCompLimbCount(bits),
    );
    defer comp.gpa.free(limbs);

    var result = BigIntMutable{ .limbs = limbs, .positive = undefined, .len = undefined };
    const overflowed = result.addWrap(lhsBigInt, rhsBigInt, qt.signedness(comp), bits);
    res.* = try intern(comp, .{ .int = .{ .bigInt = result.toConst() } });
    return overflowed;
}

pub fn negate(res: *Value, val: Value, qt: QualType, comp: *Compilation) !bool {
    return res.sub(zero, val, qt, undefined, comp);
}

pub fn decrement(res: *Value, val: Value, qt: QualType, comp: *Compilation) !bool {
    return res.sub(val, one, qt, undefined, comp);
}

/// elemSize is only used when subtracting two pointers, so we can scale the result by the size of the element type
pub fn sub(res: *Value, lhs: Value, rhs: Value, qt: QualType, elemSize: u64, comp: *Compilation) !bool {
    const bits: usize = @intCast(qt.bitSizeof(comp));
    const scalarKind = qt.scalarKind(comp);
    if (scalarKind.isFloat()) {
        if (scalarKind == .ComplexFloat) {
            res.* = switch (bits) {
                32 => try complexAddSub(lhs, rhs, f16, .sub, comp),
                64 => try complexAddSub(lhs, rhs, f32, .sub, comp),
                128 => try complexAddSub(lhs, rhs, f64, .sub, comp),
                160 => try complexAddSub(lhs, rhs, f80, .sub, comp),
                256 => try complexAddSub(lhs, rhs, f128, .sub, comp),
                else => unreachable,
            };
            return false;
        }
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
    }

    const lhsKey = comp.interner.get(lhs.ref());
    const rhsKey = comp.interner.get(rhs.ref());
    if (lhsKey == .pointer and rhsKey == .pointer) {
        const lhsReloc = lhsKey.pointer;
        const rhsReloc = rhsKey.pointer;

        if (lhsReloc.node != rhsReloc.node) {
            res.* = .{};
            return false;
        }

        const lhsOffset = fromRef(lhsReloc.offset);
        const rhsOffset = fromRef(rhsReloc.offset);

        const overflowed = try res.sub(lhsOffset, rhsOffset, comp.typeStore.ptrdiff, undefined, comp);
        const rhsSize = try int(elemSize, comp);

        _ = try res.div(res.*, rhsSize, comp.typeStore.ptrdiff, comp);
        return overflowed;
    } else if (lhsKey == .pointer) {
        const rel = lhsKey.pointer;
        const lhsSize = try int(elemSize, comp);

        var totalOffset: Value = undefined;
        const mulOverflow = try totalOffset.mul(lhsSize, rhs, comp.typeStore.ptrdiff, comp);
        const oldOffset = fromRef(rel.offset);
        const addOverflow = try totalOffset.sub(oldOffset, totalOffset, comp.typeStore.ptrdiff, undefined, comp);

        _ = try totalOffset.intCast(comp.typeStore.ptrdiff, comp);
        res.* = try pointer(.{ .node = rel.node, .offset = totalOffset.ref() }, comp);

        return mulOverflow or addOverflow;
    }

    var lhsSpace: BigIntSpace = undefined;
    var rhsSpace: BigIntSpace = undefined;
    const lhsBigInt = lhsKey.toBigInt(&lhsSpace);
    const rhsBigInt = rhsKey.toBigInt(&rhsSpace);

    const limbs = try comp.gpa.alloc(
        std.math.big.Limb,
        std.math.big.int.calcTwosCompLimbCount(bits),
    );
    defer comp.gpa.free(limbs);

    var result = BigIntMutable{ .limbs = limbs, .positive = undefined, .len = undefined };
    const overflowed = result.subWrap(lhsBigInt, rhsBigInt, qt.signedness(comp), bits);
    res.* = try intern(comp, .{ .int = .{ .bigInt = result.toConst() } });
    return overflowed;
}

pub fn mul(res: *Value, lhs: Value, rhs: Value, qt: QualType, comp: *Compilation) !bool {
    const bits: usize = @intCast(qt.bitSizeof(comp));
    const scalarKind = qt.scalarKind(comp);
    if (scalarKind.isFloat()) {
        if (scalarKind == .ComplexFloat) {
            const cf: Interner.Key.Complex = switch (bits) {
                32 => .{ .cf16 = AnnexG.complexFloatMul(f16, lhs.toFloat(f16, comp), lhs.imag(f16, comp), rhs.toFloat(f16, comp), rhs.imag(f16, comp)) },
                64 => .{ .cf32 = AnnexG.complexFloatMul(f32, lhs.toFloat(f32, comp), lhs.imag(f32, comp), rhs.toFloat(f32, comp), rhs.imag(f32, comp)) },
                128 => .{ .cf64 = AnnexG.complexFloatMul(f64, lhs.toFloat(f64, comp), lhs.imag(f64, comp), rhs.toFloat(f64, comp), rhs.imag(f64, comp)) },
                160 => .{ .cf80 = AnnexG.complexFloatMul(f80, lhs.toFloat(f80, comp), lhs.imag(f80, comp), rhs.toFloat(f80, comp), rhs.imag(f80, comp)) },
                256 => .{ .cf128 = AnnexG.complexFloatMul(f128, lhs.toFloat(f128, comp), lhs.imag(f128, comp), rhs.toFloat(f128, comp), rhs.imag(f128, comp)) },
                else => unreachable,
            };
            res.* = try intern(comp, .{ .complex = cf });
            return false;
        }

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

        const signedness = qt.signedness(comp);
        const overflowed = !result.toConst().fitsInTwosComp(signedness, bits);
        if (overflowed) {
            result.truncate(result.toConst(), signedness, bits);
        }

        res.* = try intern(comp, .{ .int = .{ .bigInt = result.toConst() } });
        return overflowed;
    }
}

/// caller guarantees rhs != 0
pub fn div(res: *Value, lhs: Value, rhs: Value, qt: QualType, comp: *Compilation) !bool {
    const bits: usize = @intCast(qt.bitSizeof(comp));
    const scalarKind = qt.scalarKind(comp);
    if (scalarKind.isFloat()) {
        if (scalarKind == .ComplexFloat) {
            const cf: Interner.Key.Complex = switch (bits) {
                32 => .{ .cf16 = AnnexG.complexFloatDiv(f16, lhs.toFloat(f16, comp), lhs.imag(f16, comp), rhs.toFloat(f16, comp), rhs.imag(f16, comp)) },
                64 => .{ .cf32 = AnnexG.complexFloatDiv(f32, lhs.toFloat(f32, comp), lhs.imag(f32, comp), rhs.toFloat(f32, comp), rhs.imag(f32, comp)) },
                128 => .{ .cf64 = AnnexG.complexFloatDiv(f64, lhs.toFloat(f64, comp), lhs.imag(f64, comp), rhs.toFloat(f64, comp), rhs.imag(f64, comp)) },
                160 => .{ .cf80 = AnnexG.complexFloatDiv(f80, lhs.toFloat(f80, comp), lhs.imag(f80, comp), rhs.toFloat(f80, comp), rhs.imag(f80, comp)) },
                256 => .{ .cf128 = AnnexG.complexFloatDiv(f128, lhs.toFloat(f128, comp), lhs.imag(f128, comp), rhs.toFloat(f128, comp), rhs.imag(f128, comp)) },
                else => unreachable,
            };
            res.* = try intern(comp, .{ .complex = cf });
            return false;
        }
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

        const limbsQ = try comp.gpa.alloc(std.math.big.Limb, lhsBigInt.limbs.len);
        defer comp.gpa.free(limbsQ);

        const limbsR = try comp.gpa.alloc(std.math.big.Limb, rhsBigInt.limbs.len);
        defer comp.gpa.free(limbsR);

        var resultQ = BigIntMutable{ .limbs = limbsQ, .positive = undefined, .len = undefined };
        var resultR = BigIntMutable{ .limbs = limbsR, .positive = undefined, .len = undefined };

        const limbsBuffer = try comp.gpa.alloc(
            std.math.big.Limb,
            std.math.big.int.calcDivLimbsBufferLen(lhsBigInt.limbs.len, rhsBigInt.limbs.len),
        );
        defer comp.gpa.free(limbsBuffer);

        resultQ.divTrunc(&resultR, lhsBigInt, rhsBigInt, limbsBuffer);

        res.* = try intern(comp, .{ .int = .{ .bigInt = resultQ.toConst() } });
        return !resultQ.toConst().fitsInTwosComp(qt.signedness(comp), bits);
    }
}

/// caller guarantees rhs != 0
/// caller guarantees lhs != std.math.minInt(T) OR rhs != -1
pub fn rem(lhs: Value, rhs: Value, qt: QualType, comp: *Compilation) !Value {
    var lhsSpace: BigIntSpace = undefined;
    var rhsSpace: BigIntSpace = undefined;
    const lhsBigInt = lhs.toBigInt(&lhsSpace, comp);
    const rhsBigInt = rhs.toBigInt(&rhsSpace, comp);

    const signedness = qt.signedness(comp);
    if (signedness == .signed) {
        var spaces: [2]BigIntSpace = undefined;
        const minVal = try Value.minInt(qt, comp);
        const negative = BigIntMutable.init(&spaces[0].limbs, -1).toConst();
        const bigOne = BigIntMutable.init(&spaces[1].limbs, 1).toConst();
        if (lhs.compare(.eq, minVal, comp) and rhsBigInt.eql(negative)) {
            return .{};
        } else if (rhsBigInt.order(bigOne).compare(.lt)) {
            // lhs - @divTrunc(lhs, rhs) * rhs
            var tmp: Value = undefined;
            _ = try tmp.div(lhs, rhs, qt, comp);
            _ = try tmp.mul(tmp, rhs, qt, comp);
            _ = try tmp.sub(lhs, tmp, qt, undefined, comp);
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

    const extra = @intFromBool(lhsBigInt.positive != rhsBigInt.positive);
    const limbs = try comp.gpa.alloc(
        std.math.big.Limb,
        @max(lhsBigInt.limbs.len, rhsBigInt.limbs.len) + extra,
    );
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

    const limbCount = if (lhsBigInt.positive and rhsBigInt.positive)
        @min(lhsBigInt.limbs.len, rhsBigInt.limbs.len)
    else if (lhsBigInt.positive)
        lhsBigInt.limbs.len
    else if (rhsBigInt.positive)
        rhsBigInt.limbs.len
    else
        @max(lhsBigInt.limbs.len, rhsBigInt.limbs.len) + 1;

    const limbs = try comp.gpa.alloc(std.math.big.Limb, limbCount);
    defer comp.gpa.free(limbs);

    var result = BigIntMutable{ .limbs = limbs, .positive = undefined, .len = undefined };
    result.bitAnd(lhsBigInt, rhsBigInt);

    return intern(comp, .{ .int = .{ .bigInt = result.toConst() } });
}

pub fn bitNot(val: Value, qt: QualType, comp: *Compilation) !Value {
    const bits: usize = @intCast(qt.bitSizeof(comp));
    var valSpace: Value.BigIntSpace = undefined;
    const valBigInt = val.toBigInt(&valSpace, comp);

    const limbs = try comp.gpa.alloc(std.math.big.Limb, std.math.big.int.calcTwosCompLimbCount(bits));
    defer comp.gpa.free(limbs);

    var result = BigIntMutable{ .limbs = limbs, .positive = undefined, .len = undefined };
    result.bitNotWrap(valBigInt, qt.signedness(comp), bits);

    return intern(comp, .{ .int = .{ .bigInt = result.toConst() } });
}

pub fn shl(res: *Value, lhs: Value, rhs: Value, qt: QualType, comp: *Compilation) !bool {
    var lhsSpace: Value.BigIntSpace = undefined;
    const lhsBigInt = lhs.toBigInt(&lhsSpace, comp);
    const shift = rhs.toInt(usize, comp) orelse std.math.maxInt(usize);

    const bits: usize = @intCast(qt.bitSizeof(comp));
    if (shift > bits) {
        if (lhsBigInt.positive) {
            res.* = try Value.maxInt(qt, comp);
        } else {
            res.* = try Value.minInt(qt, comp);
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

    const signedness = qt.signedness(comp);
    const overflowed = !result.toConst().fitsInTwosComp(signedness, bits);
    if (overflowed)
        result.truncate(result.toConst(), signedness, bits);

    res.* = try intern(comp, .{ .int = .{ .bigInt = result.toConst() } });
    return overflowed;
}

pub fn shr(lhs: Value, rhs: Value, qt: QualType, comp: *Compilation) !Value {
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

    const bits: usize = @intCast(qt.bitSizeof(comp));
    const limbs = try comp.gpa.alloc(
        std.math.big.Limb,
        std.math.big.int.calcTwosCompLimbCount(bits),
    );
    defer comp.gpa.free(limbs);

    var result = BigIntMutable{ .limbs = limbs, .positive = undefined, .len = undefined };
    result.shiftRight(lhsBigInt, shift);

    return intern(comp, .{ .int = .{ .bigInt = result.toConst() } });
}

pub fn complexConj(val: Value, qt: QualType, comp: *Compilation) !Value {
    const bits = qt.bitSizeof(comp);
    const cf: Interner.Key.Complex = switch (bits) {
        32 => .{ .cf16 = .{ val.toFloat(f16, comp), -val.imag(f16, comp) } },
        64 => .{ .cf32 = .{ val.toFloat(f32, comp), -val.imag(f32, comp) } },
        128 => .{ .cf64 = .{ val.toFloat(f64, comp), -val.imag(f64, comp) } },
        160 => .{ .cf80 = .{ val.toFloat(f80, comp), -val.imag(f80, comp) } },
        256 => .{ .cf128 = .{ val.toFloat(f128, comp), -val.imag(f128, comp) } },
        else => unreachable,
    };
    return intern(comp, .{ .complex = cf });
}

fn shallowCompare(lhs: Value, op: std.math.CompareOperator, rhs: Value) ?bool {
    if (op == .eq) {
        return lhs.optRef == rhs.optRef;
    } else if (lhs.optRef == rhs.optRef) {
        return std.math.Order.eq.compare(op);
    }
    return null;
}

pub fn compare(lhs: Value, op: std.math.CompareOperator, rhs: Value, comp: *const Compilation) bool {
    if (lhs.shallowCompare(op, rhs)) |val| return val;

    const lhsKey = comp.interner.get(lhs.ref());
    const rhsKey = comp.interner.get(rhs.ref());
    if (lhsKey == .float or rhsKey == .float) {
        const lhsF128 = lhs.toFloat(f128, comp);
        const rhsF128 = rhs.toFloat(f128, comp);
        return std.math.compare(lhsF128, op, rhsF128);
    }

    if (lhsKey == .complex or rhsKey == .complex) {
        assert(op == .neq);
        const realEq = std.math.compare(lhs.toFloat(f128, comp), .eq, rhs.toFloat(f128, comp));
        const imagEq = std.math.compare(lhs.imag(f128, comp), .eq, rhs.imag(f128, comp));
        return !realEq or !imagEq;
    }

    var lhsBigIntSpace: BigIntSpace = undefined;
    var rhsBigIntSpace: BigIntSpace = undefined;
    const lhsBigInt = lhs.toBigInt(&lhsBigIntSpace, comp);
    const rhsBigInt = rhs.toBigInt(&rhsBigIntSpace, comp);

    return lhsBigInt.order(rhsBigInt).compare(op);
}

/// Returns null for values that cannot be compared at compile time (e.g. `&x < &y`) for globals `x` and `y`.
pub fn comparePointers(lhs: Value, op: std.math.CompareOperator, rhs: Value, comp: *const Compilation) ?bool {
    if (lhs.shallowCompare(op, rhs)) |val| return val;

    const lhsKey = comp.interner.get(lhs.ref());
    const rhsKey = comp.interner.get(rhs.ref());

    if (lhsKey == .pointer and rhsKey == .pointer) {
        const lhsReloc = lhsKey.pointer;
        const rhsReloc = rhsKey.pointer;
        switch (op) {
            .eq => if (lhsReloc.node != rhsReloc.node) return false,
            .neq => if (lhsReloc.node != rhsReloc.node) return true,
            else => if (lhsReloc.node != rhsReloc.node) return null,
        }

        const lhsOffset = fromRef(lhsReloc.offset);
        const rhsOffset = fromRef(rhsReloc.offset);
        return lhsOffset.compare(op, rhsOffset, comp);
    }
    return null;
}

pub fn hash(v: Value) u64 {
    switch (v.tag) {
        .unavailable => unreachable,
        .int => return std.hash.Wyhash.hash(0, std.mem.asBytes(&v.data.int)),
        else => @panic("TODO"),
    }
}

fn twosCompIntLimit(limit: std.math.big.int.TwosCompIntLimit, qt: QualType, comp: *Compilation) !Value {
    const signedness = qt.signedness(comp);
    if (limit == .min and signedness == .unsigned) return Value.zero;

    const magBits: usize = @intCast(qt.bitSizeof(comp));
    switch (magBits) {
        inline 8, 16, 32, 64 => |bits| {
            if (limit == .min) return Value.int(@as(i64, std.math.minInt(std.meta.Int(.signed, bits))), comp);
            return switch (signedness) {
                inline else => |sign| Value.int(std.math.maxInt(std.meta.Int(sign, bits)), comp),
            };
        },
        else => {},
    }

    const signBits = @intFromBool(signedness == .signed);
    const totalBits = magBits + signBits;

    const limbs = try comp.gpa.alloc(
        std.math.big.Limb,
        std.math.big.int.calcTwosCompLimbCount(totalBits),
    );
    defer comp.gpa.free(limbs);

    var result: BigIntMutable = .{ .limbs = limbs, .positive = undefined, .len = undefined };
    result.setTwosCompIntLimit(limit, signedness, magBits);
    return Value.intern(comp, .{ .int = .{ .bigInt = result.toConst() } });
}

pub fn minInt(qt: QualType, comp: *Compilation) !Value {
    return twosCompIntLimit(.min, qt, comp);
}

pub fn maxInt(qt: QualType, comp: *Compilation) !Value {
    return twosCompIntLimit(.max, qt, comp);
}

const NestedPrint = union(enum) {
    pointer: struct {
        node: u32,
        offset: Value,
    },
};

pub fn printPointer(offset: Value, base: []const u8, comp: *const Compilation, w: anytype) @TypeOf(w).Error!void {
    try w.writeByte('&');
    try w.writeAll(base);
    if (!offset.isZero(comp)) {
        const maybeNested = try offset.print(comp.typeStore.ptrdiff, comp, w);
        std.debug.assert(maybeNested == null);
    }
}

pub fn print(v: Value, qt: QualType, comp: *const Compilation, w: anytype) @TypeOf(w).Error!?NestedPrint {
    if (qt.is(comp, .bool)) {
        try w.writeAll(if (v.isZero(comp)) "false" else "true");
        return null;
    }

    const key = comp.interner.get(v.ref());
    switch (key) {
        .null => try w.writeAll("nullptr_t"),
        .int => |repr| switch (repr) {
            inline else => |x| try w.print("{d}", .{x}),
        },
        .float => |repr| switch (repr) {
            .f16 => |x| try w.print("{d}", .{@round(@as(f64, @floatCast(x)) * 1000) / 1000}),
            .f32 => |x| try w.print("{d}", .{@round(@as(f64, @floatCast(x)) * 1000000) / 1000000}),
            inline else => |x| try w.print("{d}", .{@as(f64, @floatCast(x))}),
        },
        .bytes => |b| try printString(b, qt, comp, w),
        .complex => |repr| switch (repr) {
            .cf32 => |components| try w.print("{d} + {d}i", .{
                @round(@as(f64, @floatCast(components[0])) * 1000000) / 1000000,
                @round(@as(f64, @floatCast(components[1])) * 1000000) / 1000000,
            }),
            inline else => |components| try w.print("{d} + {d}i", .{
                @as(f64, @floatCast(components[0])),
                @as(f64, @floatCast(components[1])),
            }),
        },
        .pointer => |ptr| return .{ .pointer = .{ .node = ptr.node, .offset = fromRef(ptr.offset) } },
        else => unreachable, // not a value
    }
    return null;
}

pub fn printString(
    bytes: []const u8,
    qt: QualType,
    comp: *const Compilation,
    w: anytype,
) @TypeOf(w).Error!void {
    const size: Compilation.CharUnitSize = @enumFromInt(qt.childType(comp).sizeof(comp));
    const withoutNull = bytes[0 .. bytes.len - @intFromEnum(size)];
    try w.writeByte('"');
    switch (size) {
        .@"1" => try w.print("{}", .{std.zig.fmtEscapes(withoutNull)}),
        .@"2" => {
            var items: [2]u16 = undefined;
            var i: usize = 0;
            while (i < withoutNull.len) {
                @memcpy(std.mem.sliceAsBytes(items[0..1]), withoutNull[i..][0..2]);
                i += 2;
                const isSurrogate = std.unicode.utf16IsHighSurrogate(items[0]);
                if (isSurrogate and i < withoutNull.len) {
                    @memcpy(std.mem.sliceAsBytes(items[1..2]), withoutNull[i..][0..2]);
                    if (std.unicode.utf16DecodeSurrogatePair(&items)) |decoded| {
                        i += 2;
                        try w.print("{u}", .{decoded});
                    } else |_| {
                        try w.print("\\x{x}", .{items[0]});
                    }
                } else if (isSurrogate) {
                    try w.print("\\x{x}", .{items[0]});
                } else {
                    try w.print("{u}", .{items[0]});
                }
            }
        },
        .@"4" => {
            var item: [1]u32 = undefined;
            const dataSlice = std.mem.sliceAsBytes(item[0..1]);
            for (0..@divExact(withoutNull.len, 4)) |n| {
                @memcpy(dataSlice, withoutNull[n * 4 ..][0..4]);
                if (item[0] <= std.math.maxInt(u21) and std.unicode.utf8ValidCodepoint(@intCast(item[0]))) {
                    const codepoint: u21 = @intCast(item[0]);
                    try w.print("{u}", .{codepoint});
                } else {
                    try w.print("\\x{x}", .{item[0]});
                }
            }
        },
    }
    try w.writeByte('"');
}
