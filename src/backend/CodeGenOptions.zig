/// place uninitialized global variables in a common block
common: bool,
/// Place each function into its own section in the output file if the target supports arbitrary sections
funcSections: bool,
/// Place each data item into its own section in the output file if the target supports arbitrary sections
dataSections: bool,
picLevel: PicLevel,
/// Generate position-independent code that can only be linked into executables
isPie: bool,

pub const PicLevel = enum(u8) {
    /// Do not generate position-independent code
    none = 0,
    /// Generate position-independent code (PIC) suitable for use in a shared library, if supported for the target machine.
    one = 1,
    /// If supported for the target machine, emit position-independent code, suitable for dynamic linking and avoiding
    /// any limit on the size of the global offset table.
    two = 2,
};

pub const default: @This() = .{
    .common = false,
    .funcSections = false,
    .dataSections = false,
    .picLevel = .none,
    .isPie = false,
};
