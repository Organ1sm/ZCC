pub const arch = struct {
    pub const x86_64 = struct {
        pub const abi = @import("Arch/x86_64/abi.zig");
        pub const bits = @import("Arch/x86_64/bits.zig");
    };
};
pub const RegisterManager = @import("RegisterManager.zig").RegisterManager;
