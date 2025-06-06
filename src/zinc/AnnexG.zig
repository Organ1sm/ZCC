//! Complex arithmetic algorithms from C99 Annex G

const std = @import("std");

const copysign = std.math.copysign;
const inf = std.math.inf;
const nan = std.math.nan;
const ilogb = std.math.ilogb;
const isInf = std.math.isInf;
const isNan = std.math.isNan;
const isFinite = std.math.isFinite;
const isPositiveZero = std.math.isPositiveZero;
const scalbn = std.math.scalbn;

/// computes floating point z*w where a_param, b_param are real, imaginary parts of z and c_param, d_param are real, imaginary parts of w
pub fn complexFloatMul(comptime T: type, aParam: T, bParam: T, cParam: T, dParam: T) [2]T {
    var a = aParam;
    var b = bParam;
    var c = cParam;
    var d = dParam;

    const ac = a * c;
    const bd = b * d;
    const ad = a * d;
    const bc = b * c;
    var x = ac - bd;
    var y = ad + bc;
    if (isNan(x) and isNan(y)) {
        var recalc = false;
        if (isInf(a) or isInf(b)) {
            // lhs infinite
            // Box the infinity and change NaNs in the other factor to 0
            a = copysign(if (isInf(a)) @as(T, 1.0) else @as(T, 0.0), a);
            b = copysign(if (isInf(b)) @as(T, 1.0) else @as(T, 0.0), b);
            if (isNan(c)) c = copysign(@as(T, 0.0), c);
            if (isNan(d)) d = copysign(@as(T, 0.0), d);
            recalc = true;
        }
        if (isInf(c) or isInf(d)) {
            // rhs infinite
            // Box the infinity and change NaNs in the other factor to 0
            c = copysign(if (isInf(c)) @as(T, 1.0) else @as(T, 0.0), c);
            d = copysign(if (isInf(d)) @as(T, 1.0) else @as(T, 0.0), d);
            if (isNan(a)) a = copysign(@as(T, 0.0), a);
            if (isNan(b)) b = copysign(@as(T, 0.0), b);
            recalc = true;
        }
        if (!recalc and (isInf(ac) or isInf(bd) or isInf(ad) or isInf(bc))) {
            // Recover infinities from overflow by changing NaN's to 0
            if (isNan(a)) a = copysign(@as(T, 0.0), a);
            if (isNan(b)) b = copysign(@as(T, 0.0), b);
            if (isNan(c)) c = copysign(@as(T, 0.0), c);
            if (isNan(d)) d = copysign(@as(T, 0.0), d);
        }
        if (recalc) {
            x = inf(T) * (a * c - b * d);
            y = inf(T) * (a * d + b * c);
        }
    }
    return .{ x, y };
}

/// computes floating point z / w where a_param, b_param are real, imaginary parts of z and c_param, d_param are real, imaginary parts of w
pub fn complexFloatDiv(comptime T: type, aParam: T, bParam: T, cParam: T, dParam: T) [2]T {
    var a = aParam;
    var b = bParam;
    var c = cParam;
    var d = dParam;
    var denomLogb: i32 = 0;

    const maxCD = @max(@abs(c), @abs(d));
    if (isFinite(maxCD)) {
        if (maxCD == 0) {
            denomLogb = std.math.minInt(i32) + 1;
            c = 0;
            d = 0;
        } else {
            denomLogb = ilogb(maxCD);
            c = scalbn(c, -denomLogb);
            d = scalbn(d, -denomLogb);
        }
    }
    const denom = c * c + d * d;
    var x = scalbn((a * c + b * d) / denom, -denomLogb);
    var y = scalbn((b * c - a * d) / denom, -denomLogb);
    if (isNan(x) and isNan(y)) {
        if (isPositiveZero(denom) and (!isNan(a) or !isNan(b))) {
            x = copysign(inf(T), c) * a;
            y = copysign(inf(T), c) * b;
        } else if ((isInf(a) or isInf(b)) and isFinite(c) and isFinite(d)) {
            a = copysign(if (isInf(a)) @as(T, 1.0) else @as(T, 0.0), a);
            b = copysign(if (isInf(b)) @as(T, 1.0) else @as(T, 0.0), b);
            x = inf(T) * (a * c + b * d);
            y = inf(T) * (b * c - a * d);
        } else if (isInf(maxCD) and isFinite(a) and isFinite(b)) {
            c = copysign(if (isInf(c)) @as(T, 1.0) else @as(T, 0.0), c);
            d = copysign(if (isInf(d)) @as(T, 1.0) else @as(T, 0.0), d);
            x = 0.0 * (a * c + b * d);
            y = 0.0 * (b * c - a * d);
        }
    }
    return .{ x, y };
}

test complexFloatMul {
    // Naive algorithm would produce NaN + NaNi instead of inf + NaNi
    const result = complexFloatMul(f64, inf(f64), std.math.nan(f64), 2, 0);
    try std.testing.expect(isInf(result[0]));
    try std.testing.expect(isNan(result[1]));
}

test complexFloatDiv {
    // Naive algorithm would produce NaN + NaNi instead of inf + NaNi
    var result = complexFloatDiv(f64, inf(f64), std.math.nan(f64), 2, 0);
    try std.testing.expect(isInf(result[0]));
    try std.testing.expect(isNan(result[1]));

    result = complexFloatDiv(f64, 2.0, 2.0, 0.0, 0.0);
    try std.testing.expect(isInf(result[0]));
    try std.testing.expect(isInf(result[1]));
}
