const std = @import("std");
const Codegen = @import("../CodeGen/Codegen.zig");
const x86_64 = @import("deps").codegen.x86_64;

const Fn = @This();

pub fn genFn(c: *Codegen) Codegen.Error!void {
    _ = c;
}

pub fn genVar(c: *Codegen) Codegen.Error!void {
    _ = c;
}

