pub const Assembly = @import("backend/Assembly.zig");
pub const CodeGenOptions = @import("backend/CodeGenOptions.zig");
pub const Interner = @import("backend/Interner.zig");
pub const Ir = @import("backend/IR.zig");
pub const Object = @import("backend/Object.zig");

pub const CallingConvention = enum {
    C,
    stdcall,
    thiscall,
    vectorcall,
};

pub const VersionStr = @import("build-options").version_str;
pub const Version = @import("std").SemanticVersion.parse(VersionStr) catch unreachable;
