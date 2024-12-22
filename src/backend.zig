pub const Ir = @import("backend/IR.zig");
pub const Object = @import("backend/Object.zig");
pub const Interner = @import("backend/Interner.zig");

pub const CallingConvention = enum {
    C,
    stdcall,
    thiscall,
    vectorcall,
};

pub const VersionStr = @import("build-options").version_str;
pub const version = @import("std").SemanticVersion.parse(VersionStr) catch unreachable;
