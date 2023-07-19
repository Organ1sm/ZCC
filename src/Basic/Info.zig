pub const VersionStr = "0.0.1-dev";
pub const Version = @import("std").SemanticVersion.parse(VersionStr) catch unreachable;
