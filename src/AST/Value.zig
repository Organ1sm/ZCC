const std = @import("std");
const assert = std.debug.assert;
const Compilation = @import("../Basic/Compilation.zig");
const Type = @import("Type.zig");

const Value = @This();

/// The current kind of value that `Value` is representing.
tag: Tag = .unavailable,

/// The content of the value, which varies based on what `tag` is set to.
data: union {
    none: void, // Represents no value.
    int: u64, // Used to store integer, boolean, and pointer values as u64.
    float: f64, // Used to store floating point values as f64.
    array: []Value, // An array of `Value` items.
    bytes: []u8, // Used to store raw bytes.
} = .{ .none = {} },

/// Defines the possible types of values that the `Value` can be tagged as.
const Tag = enum {
    unavailable, // Value is not available or uninitialized.
    /// `int` is used to store integer, boolean and pointer values.
    int,
    float, // For floating-point values.
    array, // For arrays of values.
    bytes, // For raw byte data.
};

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
    if (info == .ComptimeInt or info.Int.signedness == .unsigned) {
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

pub fn bytes(v: anytype) Value {
    return .{ .tag = .bytes, .data = .{ .bytes = v } };
}

/// Performs a sign extension on the given value `v` based on its type `oldTy`.
/// This function extends the sign bit of an integer to a larger integer type to preserve the value's sign.
/// @param v       The value whose sign is to be extended.
/// @param oldTy   The original type of the value `v` before sign extension.
/// @param comp    A pointer to the Compilation context.
/// @return i64    The sign-extended integer as a 64-bit integer.
pub fn signExtend(v: Value, oldTy: Type, comp: *Compilation) i64 {
    // Determine the size of the old type to perform the correct sign extension.
    const size = oldTy.sizeof(comp).?;

    // Perform sign extension based on the determined size.
    return switch (size) {
        // If the size is 4 bytes, interpret `v` as a 32-bit integer and return it.
        4 => v.getInt(i32),
        // If the size is 8 bytes, interpret `v` as a 64-bit integer and return it.
        8 => v.getInt(i64),
        // If the size is neither 4 nor 8 bytes, the situation is not handled; this is an unreachable state.
        else => unreachable,
    };
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
    inline for ([_]std.builtin.Signedness{ .signed, .unsigned }) |signedness| {
        inline for ([_]u16{ 1, 2, 4, 8 }) |bytecount| {
            // When the current signedness and byte count match the target, perform the conversion.
            if (signedness == intTySignedness and bytecount == intTySize) {
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
            }
        }
    }
    // This point should be unreachable because all valid paths return from within the loop.
    unreachable;
}

/// Converts the stored value from a float to an integer.
/// `.unavailable` value remains unchanged.
pub fn floatToInt(v: *Value, oldTy: Type, newTy: Type, comp: *Compilation) FloatToIntChangeKind {
    assert(oldTy.isFloat());
    if (v.tag == .unavailable)
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
    if (v.tag == .unavailable) return;
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
    if (v.tag == .unavailable) return;

    if (newTy.is(.Bool))
        return v.toBool();

    if (!oldTy.isUnsignedInt(comp)) {
        const size = newTy.sizeof(comp).?;
        switch (size) {
            1 => v.* = int(@as(u8, @bitCast(v.getInt(i8)))),
            2 => v.* = int(@as(u16, @bitCast(v.getInt(i16)))),
            4 => v.* = int(@as(u32, @bitCast(v.getInt(i32)))),
            8 => return,
            else => unreachable,
        }
    }
}

/// Converts the stored value from an integer to a float.
/// `.unavailable` value remains unchanged.
pub fn floatCast(v: *Value, oldTy: Type, newTy: Type, comp: *Compilation) void {
    assert(oldTy.isFloat() and newTy.isFloat());
    if (v.tag == .unavailable) return;
    const size = newTy.sizeof(comp).?;
    if (!newTy.isReal() or size > 8) {
        v.tag = .unavailable;
    } else if (size == 32) {
        v.* = float(@as(f32, @floatCast(v.data.float)));
    }
}

/// Truncates data.int to one bit
pub fn toBool(v: *Value) void {
    if (v.tag == .unavailable) return;
    const res = v.getBool();
    v.* = int(@intFromBool(res));
}

pub fn isZero(v: Value) bool {
    return switch (v.tag) {
        .unavailable => false,
        .int => v.data.int == 0,
        .float => v.data.float == 0,
        .array => false,
        .bytes => false,
    };
}

pub fn getBool(v: Value) bool {
    return switch (v.tag) {
        .unavailable => unreachable,
        .int => v.data.int != 0,
        .float => v.data.float != 0,
        .array => false,
        .bytes => false,
    };
}

pub fn getInt(v: Value, comptime T: type) T {
    if (T == u64) return v.data.int;
    return if (@typeInfo(T).Int.signedness == .unsigned)
        @as(T, @truncate(v.data.int))
    else
        @as(T, @truncate(@as(i64, @bitCast(v.data.int))));
}

pub fn getFloat(v: Value, comptime T: type) T {
    if (T == f64) return v.data.float;
    return @floatCast(v.data.float);
}

const bin_overflow = struct {
    inline fn addInt(comptime T: type, out: *Value, a: Value, b: Value) bool {
        const a_val = a.getInt(T);
        const b_val = b.getInt(T);
        var c: T = undefined;
        c, const overflow = @addWithOverflow(a_val, b_val);
        out.* = int(c);

        return overflow == 1;
    }

    inline fn addFloat(comptime T: type, aa: Value, bb: Value) Value {
        const a_val = aa.getFloat(T);
        const b_val = bb.getFloat(T);
        return float(a_val + b_val);
    }

    inline fn subInt(comptime T: type, out: *Value, a: Value, b: Value) bool {
        const a_val = a.getInt(T);
        const b_val = b.getInt(T);
        var c: T = undefined;
        c, const overflow = @subWithOverflow(a_val, b_val);
        out.* = int(c);

        return overflow == 1;
    }

    inline fn subFloat(comptime T: type, aa: Value, bb: Value) Value {
        const a_val = aa.getFloat(T);
        const b_val = bb.getFloat(T);
        return float(a_val - b_val);
    }

    inline fn mulInt(comptime T: type, out: *Value, a: Value, b: Value) bool {
        const a_val = a.getInt(T);
        const b_val = b.getInt(T);
        var c: T = undefined;
        c, const overflow = @mulWithOverflow(a_val, b_val);
        out.* = int(c);
        return overflow == 1;
    }

    inline fn mulFloat(comptime T: type, aa: Value, bb: Value) Value {
        const a_val = aa.getFloat(T);
        const b_val = bb.getFloat(T);
        return float(a_val * b_val);
    }

    const FT = fn (*Value, Value, Value, Type, *Compilation) bool;
    fn getOp(intFunc: anytype, floatFunc: anytype) FT {
        return struct {
            fn op(res: *Value, a: Value, b: Value, ty: Type, comp: *Compilation) bool {
                const size = ty.sizeof(comp).?;
                if (@TypeOf(floatFunc) != @TypeOf(null) and ty.isFloat()) {
                    res.* = switch (size) {
                        4 => floatFunc(f32, a, b),
                        8 => floatFunc(f64, a, b),
                        else => unreachable,
                    };
                    return false;
                }

                if (ty.isUnsignedInt(comp)) switch (size) {
                    1 => unreachable, // promoted to int
                    2 => unreachable, // promoted to int
                    4 => return intFunc(u32, res, a, b),
                    8 => return intFunc(u64, res, a, b),
                    else => unreachable,
                } else switch (size) {
                    1 => unreachable, // promoted to int
                    2 => unreachable, // promoted to int
                    4 => return intFunc(i32, res, a, b),
                    8 => return intFunc(i64, res, a, b),
                    else => unreachable,
                }
            }
        }.op;
    }
};

pub const add = bin_overflow.getOp(bin_overflow.addInt, bin_overflow.addFloat);
pub const sub = bin_overflow.getOp(bin_overflow.subInt, bin_overflow.subFloat);
pub const mul = bin_overflow.getOp(bin_overflow.mulInt, bin_overflow.mulFloat);

const bin_ops = struct {
    inline fn divInt(comptime T: type, aa: Value, bb: Value) Value {
        const a_val = aa.getInt(T);
        const b_val = bb.getInt(T);
        return int(@divTrunc(a_val, b_val));
    }

    inline fn divFloat(comptime T: type, aa: Value, bb: Value) Value {
        const a_val = aa.getFloat(T);
        const b_val = bb.getFloat(T);
        return float(a_val / b_val);
    }

    inline fn remInt(comptime T: type, a: Value, b: Value) Value {
        const a_val = a.getInt(T);
        const b_val = b.getInt(T);

        if (@typeInfo(T).Int.signedness == .signed) {
            if (a_val == std.math.minInt(T) and b_val == -1) {
                return Value{ .tag = .unavailable, .data = .{ .none = {} } };
            } else {
                if (b_val > 0) return int(@rem(a_val, b_val));
                return int(a_val - @divTrunc(a_val, b_val) * b_val);
            }
        } else {
            return int(a_val % b_val);
        }
    }

    inline fn orInt(comptime T: type, a: Value, b: Value) Value {
        const a_val = a.getInt(T);
        const b_val = b.getInt(T);
        return int(a_val | b_val);
    }

    inline fn xorInt(comptime T: type, a: Value, b: Value) Value {
        const a_val = a.getInt(T);
        const b_val = b.getInt(T);
        return int(a_val ^ b_val);
    }

    inline fn andInt(comptime T: type, a: Value, b: Value) Value {
        const a_val = a.getInt(T);
        const b_val = b.getInt(T);
        return int(a_val & b_val);
    }

    inline fn shl(comptime T: type, a: Value, b: Value) Value {
        const ShiftT = std.math.Log2Int(T);
        const info = @typeInfo(T).Int;
        const UT = std.meta.Int(.unsigned, info.bits);
        const b_val = b.getInt(T);

        if (b_val > std.math.maxInt(ShiftT)) {
            return if (info.signedness == .unsigned)
                int(@as(UT, std.math.maxInt(UT)))
            else
                int(@as(T, std.math.minInt(T)));
        }
        const amt: ShiftT = @truncate(@as(UT, @bitCast(b_val)));
        const a_val = a.getInt(T);
        return int(a_val << amt);
    }

    inline fn shr(comptime T: type, a: Value, b: Value) Value {
        const ShiftT = std.math.Log2Int(T);
        const UT = std.meta.Int(.unsigned, @typeInfo(T).Int.bits);

        const b_val = b.getInt(T);
        if (b_val > std.math.maxInt(ShiftT)) return Value.int(0);

        const amt: ShiftT = @truncate(@as(UT, @bitCast(b_val)));
        const a_val = a.getInt(T);
        return int(a_val >> amt);
    }

    const FT = fn (Value, Value, Type, *Compilation) Value;
    fn getOp(intFunc: anytype, floatFunc: anytype) FT {
        return struct {
            fn op(a: Value, b: Value, ty: Type, comp: *Compilation) Value {
                const size = ty.sizeof(comp).?;
                if (@TypeOf(floatFunc) != @TypeOf(null) and ty.isFloat()) {
                    switch (size) {
                        4 => return floatFunc(f32, a, b),
                        8 => return floatFunc(f64, a, b),
                        else => unreachable,
                    }
                }

                if (ty.isUnsignedInt(comp)) switch (size) {
                    1 => unreachable, // promoted to int
                    2 => unreachable, // promoted to int
                    4 => return intFunc(u32, a, b),
                    8 => return intFunc(u64, a, b),
                    else => unreachable,
                } else switch (size) {
                    1 => unreachable, // promoted to int
                    2 => unreachable, // promoted to int
                    4 => return intFunc(i32, a, b),
                    8 => return intFunc(i64, a, b),
                    else => unreachable,
                }
            }
        }.op;
    }
};

/// caller guarantees rhs != 0
pub const div = bin_ops.getOp(bin_ops.divInt, bin_ops.divFloat);
/// caller guarantees rhs != 0
/// caller guarantees lhs != std.math.minInt(T) OR rhs != -1
pub const rem = bin_ops.getOp(bin_ops.remInt, null);

pub const bitOr = bin_ops.getOp(bin_ops.orInt, null);
pub const bitXor = bin_ops.getOp(bin_ops.xorInt, null);
pub const bitAnd = bin_ops.getOp(bin_ops.andInt, null);

pub const shl = bin_ops.getOp(bin_ops.shl, null);
pub const shr = bin_ops.getOp(bin_ops.shr, null);

pub fn bitNot(v: Value, ty: Type, comp: *Compilation) Value {
    const size = ty.sizeof(comp).?;
    var out: Value = undefined;
    if (ty.isUnsignedInt(comp)) switch (size) {
        1 => unreachable, // promoted to int
        2 => unreachable, // promoted to int
        4 => out = int(~v.getInt(u32)),
        8 => out = int(~v.getInt(u64)),
        else => unreachable,
    } else switch (size) {
        1 => unreachable, // promoted to int
        2 => unreachable, // promoted to int
        4 => out = int(~v.getInt(i32)),
        8 => out = int(~v.getInt(i64)),
        else => unreachable,
    }
    return out;
}

pub fn compare(a: Value, op: std.math.CompareOperator, b: Value, ty: Type, comp: *Compilation) bool {
    assert(a.tag == b.tag);
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
            1 => unreachable, // promoted to int
            2 => unreachable, // promoted to int
            4 => return S.doICompare(u32, a, op, b),
            8 => return S.doICompare(u64, a, op, b),
            else => unreachable,
        } else switch (size) {
            1 => unreachable, // promoted to int
            2 => unreachable, // promoted to int
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

pub fn dump(v: Value, ty: Type, comp: *Compilation, w: anytype) !void {
    switch (v.tag) {
        .unavailable => try w.writeAll("unavailable"),
        .int => if (ty.isUnsignedInt(comp))
            try w.print("{d}", .{v.data.int})
        else {
            try w.print("{d}", .{v.signExtend(ty, comp)});
        },
        // std.fmt does @as instead of @floatCast
        .float => try w.print("{d}", .{@as(f64, @floatCast(v.data.float))}),
        else => try w.print("({s})", .{@tagName(v.tag)}),
    }
}
