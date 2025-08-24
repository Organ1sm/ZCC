const std = @import("std");

const zinc = @import("zinc");
pub const Error = zinc.Compilation.Error || error{CodegenFailed};

pub const x86_64 = @import("assemblyBackend/x86_64.zig");

pub fn genAsm(target: std.Target, tree: *const zinc.Tree) Error!zinc.Assembly {
    return switch (target.cpu.arch) {
        .x86_64 => x86_64.genAsm(tree),
        else => std.debug.panic("genAsm not implemented: {s}", .{@tagName(target.cpu.arch)}),
    };
}
