const std = @import("std");
const assert = std.debug.assert;
const Compilation = @import("../Basic/Compilation.zig");
const Type = @import("Type.zig");
const Interner = @import("../CodeGen/Interner.zig");
const BigIntSpace = Interner.Tag.Int.BigIntSpace;
const Parser = @import("../Parser/Parser.zig");
const BigIntConst = std.math.big.int.Const;
const BigIntMutable = std.math.big.int.Mutable;

const Value = @This();

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

pub fn ref(v: Value) Interner.Ref {
    std.debug.assert(v.optRef != .none);
    return @enumFromInt(@intFromEnum(v.optRef));
}

pub fn zero(v: Value) Value {
    return switch (v.tag) {
        .int => int(0),
        .float => float(0),
        else => unreachable,
    };
}

pub fn one(v: Value) Value {
    return switch (v.tag) {
        .int => int(1),
        .float => float(1),
        else => unreachable,
    };
}

/// Converts a comptime integer or runtime unsigned integer to a Value.
/// If the input is a signed runtime integer, it is cast to i64 before storing.
/// @param v  The integer value to convert.
/// @return   A Value struct with the tag set to .int and data containing the integer.
pub fn int(v: anytype) Value {
    const info = @typeInfo(@TypeOf(v));

    // Check if v is a comptime integer or unsigned runtime integer
    if (info == .comptime_int or info.int.signedness == .unsigned) {
        // Directly store the integer in the Value
        return .{ .tag = .int, .data = .{ .int = v } };
    } else {
        // Cast signed runtime integer to i64 before storing
        return .{ .tag = .int, .data = .{ .int = @bitCast(@as(i64, v)) } };
    }
}

pub fn float(v: anytype) Value {
    return .{ .tag = .float, .data = .{ .float = v } };
}

pub fn bytes(start: u32, end: u32) Value {
    return .{ .tag = .bytes, .data = .{ .bytes = .{ .start = start, .end = end } } };
}

/// Performs a sign extension on the given value `v` based on its type `oldTy`.
/// This function extends the sign bit of an integer to a larger integer type to preserve the value's sign.
/// @param v       The value whose sign is to be extended.
/// @param oldTy   The original type of the value `v` before sign extension.
/// @param comp    A pointer to the Compilation context.
/// @return i64    The sign-extended integer as a 64-bit integer.
pub fn signExtend(v: Value, oldTy: Type, comp: *Compilation) i64 {
    const size = oldTy.sizeof(comp).?;

    return switch (size) {
        1 => v.getInt(i8),
        2 => v.getInt(i16),
        4 => v.getInt(i32),
        8 => v.getInt(i64),
        else => unreachable,
    };
}

/// Number of bits needed to hold `v` which is of type `ty`.
/// Asserts that `v` is not negative
pub fn minUnsignedBits(v: Value, ty: Type, comp: *const Compilation) usize {
    assert(v.compare(.gte, Value.int(0), ty, comp));
    return switch (ty.sizeof(comp).?) {
        1 => 8 - @as(u8, @clz(v.getInt(u8))),
        2 => 16 - @as(u16, @clz(v.getInt(u16))),
        4 => 32 - @as(u32, @clz(v.getInt(u32))),
        8 => 64 - @as(u64, @clz(v.getInt(u64))),
        else => unreachable,
    };
}

test "minUnsignedBits" {
    const Test = struct {
        fn checkIntBits(comp: *const Compilation, specifier: Type.Specifier, v: u64, expected: usize) !void {
            const val = Value.int(v);
            try std.testing.expectEqual(expected, val.minUnsignedBits(.{ .specifier = specifier }, comp));
        }
    };

    var comp = Compilation.init(std.testing.allocator);
    defer comp.deinit();
    const targetQuery = try std.Target.Query.parse(.{ .arch_os_abi = "x86_64-linux-gnu" });
    comp.target = try std.zig.system.resolveTargetQuery(targetQuery);

    try Test.checkIntBits(&comp, .Int, 0, 0);
    try Test.checkIntBits(&comp, .Int, 1, 1);
    try Test.checkIntBits(&comp, .Int, 2, 2);
    try Test.checkIntBits(&comp, .Int, std.math.maxInt(i8), 7);
    try Test.checkIntBits(&comp, .Int, std.math.maxInt(u8), 8);
    try Test.checkIntBits(&comp, .Int, std.math.maxInt(i16), 15);
    try Test.checkIntBits(&comp, .Int, std.math.maxInt(u16), 16);
    try Test.checkIntBits(&comp, .Int, std.math.maxInt(i32), 31);
    try Test.checkIntBits(&comp, .UInt, std.math.maxInt(u32), 32);
    try Test.checkIntBits(&comp, .Long, std.math.maxInt(i64), 63);
    try Test.checkIntBits(&comp, .ULong, std.math.maxInt(u64), 64);
    try Test.checkIntBits(&comp, .LongLong, std.math.maxInt(i64), 63);
    try Test.checkIntBits(&comp, .ULongLong, std.math.maxInt(u64), 64);
}

/// Minimum number of bits needed to represent `v` in 2's complement notation
/// Asserts that `v` is negative.
pub fn minSignedBits(v: Value, ty: Type, comp: *const Compilation) usize {
    assert(v.compare(.lt, Value.int(0), ty, comp));
    return switch (ty.sizeof(comp).?) {
        1 => 8 - @clz(~v.getInt(u8)) + 1,
        2 => 16 - @clz(~v.getInt(u16)) + 1,
        4 => 32 - @clz(~v.getInt(u32)) + 1,
        8 => 64 - @clz(~v.getInt(u64)) + 1,
        else => unreachable,
    };
}

test "minSignedBits" {
    const Test = struct {
        fn checkIntBits(comp: *const Compilation, specifier: Type.Specifier, v: i64, expected: usize) !void {
            const val = Value.int(v);
            try std.testing.expectEqual(expected, val.minSignedBits(.{ .specifier = specifier }, comp));
        }
    };

    var comp = Compilation.init(std.testing.allocator);
    defer comp.deinit();
    const targetQuery = try std.Target.Query.parse(.{ .arch_os_abi = "x86_64-linux-gnu" });
    comp.target = try std.zig.system.resolveTargetQuery(targetQuery);

    for ([_]Type.Specifier{ .Int, .Long, .LongLong }) |specifier| {
        try Test.checkIntBits(&comp, specifier, -1, 1);
        try Test.checkIntBits(&comp, specifier, -2, 2);
        try Test.checkIntBits(&comp, specifier, -10, 5);
        try Test.checkIntBits(&comp, specifier, -101, 8);

        try Test.checkIntBits(&comp, specifier, std.math.minInt(i8), 8);
        try Test.checkIntBits(&comp, specifier, std.math.minInt(i16), 16);
        try Test.checkIntBits(&comp, specifier, std.math.minInt(i32), 32);
    }

    try Test.checkIntBits(&comp, .Long, std.math.minInt(i64), 64);
    try Test.checkIntBits(&comp, .LongLong, std.math.minInt(i64), 64);
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
/// `.unavailable` value remains unchanged.
pub fn intToFloat(v: *Value, oldTy: Type, newTy: Type, comp: *Compilation) void {
    assert(oldTy.isInt());
    if (v.isUnavailable()) return;

    if (!newTy.isReal() or newTy.sizeof(comp).? > 8) {
        v.tag = .unavailable;
    } else if (oldTy.isUnsignedInt(comp)) {
        const val: f64 = @floatFromInt(v.data.int);
        v.* = float(val);
    } else {
        const convertVal: i64 = @bitCast(v.data.int);
        const val: f64 = @floatFromInt(convertVal);
        v.* = float(val);
    }
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

/// Truncates data.int to one bit
pub fn toBool(v: *Value) void {
    if (v.isUnavailable()) return;
    const res = v.getBool();
    v.* = int(@intFromBool(res));
}

pub fn isZero(v: Value) bool {
    return switch (v.tag) {
        .unavailable => false,
        .nullptrTy => false,
        .int => v.data.int == 0,
        .float => v.data.float == 0,
        .bytes => false,
    };
}

pub fn getBool(v: Value) bool {
    return switch (v.tag) {
        .unavailable => unreachable,
        .nullptrTy => false,
        .int => v.data.int != 0,
        .float => v.data.float != 0,
        .bytes => false,
    };
}

/// Returns the integer value of `v` as type `T`.
///
/// If `T` is `u64`, this function simply returns the stored integer value.
/// If `T` is an unsigned integer type, this function truncates the stored value to the target type.
/// If `T` is a signed integer type, this function sign-extends the stored value to the target type.
///
/// @param v  The `Value` to get the integer value from.
/// @param T  The type to cast the value to.
///
/// @return   The value of `v` as type `T`.
pub fn getInt(v: Value, comptime T: type) T {
    if (T == u64) return v.data.int;
    return if (@typeInfo(T).int.signedness == .unsigned)
        @as(T, @truncate(v.data.int))
    else
        @as(T, @truncate(@as(i64, @bitCast(v.data.int))));
}

pub fn getFloat(v: Value, comptime T: type) T {
    if (T == f64) return v.data.float;
    return @floatCast(v.data.float);
}

pub fn add(res: *Value, lhs: Value, rhs: Value, ty: Type, p: *Parser) !bool {
    const bits = ty.bitSizeof(p.comp).?;
    if (ty.isFloat()) {
        const f: Interner.Key.Float = switch (bits) {
            16 => .{ .f16 = lhs.toFloat(f16, p) + rhs.toFloat(f16, p) },
            32 => .{ .f32 = lhs.toFloat(f32, p) + rhs.toFloat(f32, p) },
            64 => .{ .f64 = lhs.toFloat(f64, p) + rhs.toFloat(f64, p) },
            80 => .{ .f80 = lhs.toFloat(f80, p) + rhs.toFloat(f80, p) },
            128 => .{ .f128 = lhs.toFloat(f128, p) + rhs.toFloat(f128, p) },
            else => unreachable,
        };
        res.* = try p.intern(.{ .float = f });
        return false;
    } else {
        var lhsSpace: BigIntSpace = undefined;
        var rhsSpace: BigIntSpace = undefined;
        const lhsBigInt = lhs.toBigInt(&lhsSpace, p);
        const rhsBigInt = rhs.toBigInt(&rhsSpace, p);

        const limbs = try p.gpa.alloc(
            std.math.big.Limb,
            std.math.big.int.calcTwosCompLimbCount(bits),
        );
        defer p.gpa.free(limbs);

        var result = BigIntMutable{ .limbs = limbs, .positive = undefined, .len = undefined };
        const overflowed = result.addWrap(lhsBigInt, rhsBigInt, ty.signedness(p.comp), bits);
        res.* = .{ .ref = try p.interner.put(p.gpa, .{ .int = .{ .bigInt = result.toConst() } }) };
        return overflowed;
    }
}

pub fn sub(res: *Value, lhs: Value, rhs: Value, ty: Type, p: *Parser) !bool {
    const bits = ty.bitSizeof(p.comp).?;
    if (ty.isFloat()) {
        const f: Interner.Key.Float = switch (bits) {
            16 => .{ .f16 = lhs.toFloat(f16, p) - rhs.toFloat(f16, p) },
            32 => .{ .f32 = lhs.toFloat(f32, p) - rhs.toFloat(f32, p) },
            64 => .{ .f64 = lhs.toFloat(f64, p) - rhs.toFloat(f64, p) },
            80 => .{ .f80 = lhs.toFloat(f80, p) - rhs.toFloat(f80, p) },
            128 => .{ .f128 = lhs.toFloat(f128, p) - rhs.toFloat(f128, p) },
            else => unreachable,
        };
        res.* = try p.intern(.{ .float = f });
        return false;
    } else {
        var lhsSpace: BigIntSpace = undefined;
        var rhsSpace: BigIntSpace = undefined;
        const lhsBigInt = lhs.toBigInt(&lhsSpace, p);
        const rhsBigInt = rhs.toBigInt(&rhsSpace, p);

        const limbs = try p.gpa.alloc(std.math.big.Limb, std.math.big.int.calcTwosCompLimbCount(bits));
        defer p.gpa.free(limbs);

        var result = BigIntMutable{ .limbs = limbs, .positive = undefined, .len = undefined };
        const overflowed = result.subWrap(lhsBigInt, rhsBigInt, ty.signedness(p.comp), bits);
        res.* = .{ .ref = try p.interner.put(p.gpa, .{ .int = .{ .bigInt = result.toConst() } }) };
        return overflowed;
    }
}

pub fn mul(res: *Value, lhs: Value, rhs: Value, ty: Type, p: *Parser) !bool {
    const bits = ty.bitSizeof(p.comp).?;
    if (ty.isFloat()) {
        const f: Interner.Key.Float = switch (bits) {
            16 => .{ .f16 = lhs.toFloat(f16, p) * rhs.toFloat(f16, p) },
            32 => .{ .f32 = lhs.toFloat(f32, p) * rhs.toFloat(f32, p) },
            64 => .{ .f64 = lhs.toFloat(f64, p) * rhs.toFloat(f64, p) },
            80 => .{ .f80 = lhs.toFloat(f80, p) * rhs.toFloat(f80, p) },
            128 => .{ .f128 = lhs.toFloat(f128, p) * rhs.toFloat(f128, p) },
            else => unreachable,
        };
        res.* = try p.intern(.{ .float = f });
        return false;
    } else {
        var lhsSpace: BigIntSpace = undefined;
        var rhsSpace: BigIntSpace = undefined;
        const lhsBigInt = lhs.toBigInt(&lhsSpace, p);
        const rhsBigInt = rhs.toBigInt(&rhsSpace, p);

        const limbs = try p.gpa.alloc(std.math.big.Limb, lhsBigInt.limbs.len + rhsBigInt.limbs.len);
        defer p.gpa.free(limbs);

        var result = BigIntMutable{ .limbs = limbs, .positive = undefined, .len = undefined };

        const limbsBuffer = try p.gpa.alloc(
            std.math.big.Limb,
            std.math.big.int.calcMulLimbsBufferLen(lhsBigInt.limbs.len, rhsBigInt.limbs.len, 1),
        );
        defer p.gpa.free(limbsBuffer);

        result.mul(lhsBigInt, rhsBigInt, limbsBuffer, p.gpa);

        const signedness = ty.signedness(p.comp);
        const overflowed = !result.toConst().fitsInTwosComp(signedness, bits);
        if (overflowed) {
            result.truncate(result.toConst(), signedness, bits);
        }

        res.* = try p.intern(.{ .int = .{ .bigInt = result.toConst() } });
        return overflowed;
    }
}

/// caller guarantees rhs != 0
pub fn div(res: *Value, lhs: Value, rhs: Value, ty: Type, p: *Parser) !bool {
    const bits = ty.bitSizeof(p.comp).?;
    if (ty.isFloat()) {
        const f: Interner.Key.Float = switch (bits) {
            16 => .{ .f16 = lhs.toFloat(f16, p) / rhs.toFloat(f16, p) },
            32 => .{ .f32 = lhs.toFloat(f32, p) / rhs.toFloat(f32, p) },
            64 => .{ .f64 = lhs.toFloat(f64, p) / rhs.toFloat(f64, p) },
            80 => .{ .f80 = lhs.toFloat(f80, p) / rhs.toFloat(f80, p) },
            128 => .{ .f128 = lhs.toFloat(f128, p) / rhs.toFloat(f128, p) },
            else => unreachable,
        };
        res.* = try p.intern(.{ .float = f });
        return false;
    } else {
        var lhsSpace: BigIntSpace = undefined;
        var rhsSpace: BigIntSpace = undefined;
        const lhsBigInt = lhs.toBigInt(&lhsSpace, p);
        const rhsBigInt = rhs.toBigInt(&rhsSpace, p);

        const limbs = try p.gpa.alloc(std.math.big.Limb, lhsBigInt.limbs.len + rhsBigInt.limbs.len);
        defer p.gpa.free(limbs);

        var result = BigIntMutable{ .limbs = limbs, .positive = undefined, .len = undefined };

        const limbsQ = try p.gpa.alloc(std.math.big.Limb, lhsBigInt.limbs.len);
        defer p.gpa.free(limbsQ);
        var resultQ = BigIntMutable{ .limbs = limbsQ, .positive = undefined, .len = undefined };

        const limbsR = try p.gpa.alloc(std.math.big.Limb, rhsBigInt.limbs.len);
        defer p.gpa.free(limbsR);
        var resultR = BigIntMutable{ .limbs = limbsR, .positive = undefined, .len = undefined };

        const limbs_buffer = try p.gpa.alloc(
            std.math.big.Limb,
            std.math.big.int.calcDivLimbsBufferLen(lhsBigInt.limbs.len, rhsBigInt.limbs.len),
        );
        defer p.gpa.free(limbs_buffer);

        resultQ.divTrunc(&resultR, lhsBigInt, rhsBigInt, limbs_buffer);

        res.* = try p.intern(.{ .int = .{ .bitInt = result.toConst() } });
        return !resultQ.toConst().fitsInTwosComp(ty.signedness(p.comp), bits);
    }
}

pub fn bitOr(lhs: Value, rhs: Value, p: *Parser) !Value {
    var lhsSpace: BigIntSpace = undefined;
    var rhsSpace: BigIntSpace = undefined;
    const lhsBigInt = lhs.toBigInt(&lhsSpace, p);
    const rhsBigInt = rhs.toBigInt(&rhsSpace, p);

    const limbs = try p.gpa.alloc(std.math.big.Limb, @max(lhsBigInt.limbs.len, rhsBigInt.limbs.len));
    defer p.gpa.free(limbs);

    var result = BigIntMutable{ .limbs = limbs, .positive = undefined, .len = undefined };
    result.bitOr(lhsBigInt, rhsBigInt);

    return p.intern(.{ .int = .{ .bigInt = result.toConst() } });
}

pub fn bitXor(lhs: Value, rhs: Value, p: *Parser) !Value {
    var lhsSpace: BigIntSpace = undefined;
    var rhsSpace: BigIntSpace = undefined;
    const lhsBigInt = lhs.toBigInt(&lhsSpace, p);
    const rhsBigInt = rhs.toBigInt(&rhsSpace, p);

    const limbs = try p.gpa.alloc(std.math.big.Limb, @max(lhsBigInt.limbs.len, rhsBigInt.limbs.len));
    defer p.gpa.free(limbs);

    var result = BigIntMutable{ .limbs = limbs, .positive = undefined, .len = undefined };
    result.bitXor(lhsBigInt, rhsBigInt);

    return p.intern(.{ .int = .{ .bigInt = result.toConst() } });
}

pub fn bitAnd(lhs: Value, rhs: Value, p: *Parser) !Value {
    var lhsSpace: BigIntSpace = undefined;
    var rhsSpace: BigIntSpace = undefined;
    const lhsBigInt = lhs.toBigInt(&lhsSpace, p);
    const rhsBigInt = rhs.toBigInt(&rhsSpace, p);

    const limbs = try p.gpa.alloc(std.math.big.Limb, @max(lhsBigInt.limbs.len, rhsBigInt.limbs.len));
    defer p.gpa.free(limbs);

    var result = BigIntMutable{ .limbs = limbs, .positive = undefined, .len = undefined };
    result.bitAnd(lhsBigInt, rhsBigInt);

    return p.intern(.{ .int = .{ .bigInt = result.toConst() } });
}

pub fn bitNot(val: Value, ty: Type, p: *Parser) !Value {
    const bits = ty.bitSizeof(p.comp).?;
    var valSpace: Value.BigIntSpace = undefined;
    const valBigInt = val.toBigInt(&valSpace, p);

    const limbs = try p.gpa.alloc(std.math.big.Limb, std.math.big.int.calcTwosCompLimbCount(bits));
    defer p.gpa.free(limbs);

    var result = BigIntMutable{ .limbs = limbs, .positive = undefined, .len = undefined };
    result.bitNotWrap(valBigInt, ty.signedness(p.comp), bits);

    return p.intern(.{ .int = .{ .bigInt = result.toConst() } });
}

pub fn compare(a: Value, op: std.math.CompareOperator, b: Value, ty: Type, comp: *const Compilation) bool {
    assert(a.tag == b.tag);
    if (a.tag == .nullptrTy) {
        return switch (op) {
            .eq => true,
            .neq => false,
            else => unreachable,
        };
    }
    const S = struct {
        inline fn doICompare(comptime T: type, aa: Value, opp: std.math.CompareOperator, bb: Value) bool {
            const a_val = aa.getInt(T);
            const b_val = bb.getInt(T);
            return std.math.compare(a_val, opp, b_val);
        }
        inline fn doFCompare(comptime T: type, aa: Value, opp: std.math.CompareOperator, bb: Value) bool {
            const a_val = aa.getFloat(T);
            const b_val = bb.getFloat(T);
            return std.math.compare(a_val, opp, b_val);
        }
    };
    const size = ty.sizeof(comp).?;
    switch (a.tag) {
        .unavailable => return true,
        .int => if (ty.isUnsignedInt(comp)) switch (size) {
            1 => return S.doICompare(u8, a, op, b),
            2 => return S.doICompare(u16, a, op, b),
            4 => return S.doICompare(u32, a, op, b),
            8 => return S.doICompare(u64, a, op, b),
            else => unreachable,
        } else switch (size) {
            1 => return S.doICompare(i8, a, op, b),
            2 => return S.doICompare(i16, a, op, b),
            4 => return S.doICompare(i32, a, op, b),
            8 => return S.doICompare(i64, a, op, b),
            else => unreachable,
        },
        .float => switch (size) {
            4 => return S.doFCompare(f32, a, op, b),
            8 => return S.doFCompare(f64, a, op, b),
            else => unreachable,
        },
        else => @panic("TODO"),
    }
    return false;
}

pub fn hash(v: Value) u64 {
    switch (v.tag) {
        .unavailable => unreachable,
        .int => return std.hash.Wyhash.hash(0, std.mem.asBytes(&v.data.int)),
        else => @panic("TODO"),
    }
}

pub fn dump(v: Value, ty: Type, comp: *Compilation, strings: []const u8, w: anytype) !void {
    switch (v.tag) {
        .unavailable => try w.writeAll("unavailable"),
        .int => if (ty.is(.Bool) and comp.langOpts.standard.atLeast(.c23)) {
            try w.print("{s}", .{if (v.isZero()) "false" else "true"});
        } else if (ty.isUnsignedInt(comp)) {
            try w.print("{d}", .{v.data.int});
        } else {
            try w.print("{d}", .{v.signExtend(ty, comp)});
        },
        .bytes => try v.data.bytes.dumpString(ty, comp, strings, w),
        // std.fmt does @as instead of @floatCast
        .float => try w.print("{d}", .{@as(f64, @floatCast(v.data.float))}),
        else => try w.print("({s})", .{@tagName(v.tag)}),
    }
}
