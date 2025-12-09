const std = @import("std");
const Abi = std.Target.Abi;
const Cpu = std.Target.Cpu;
const mem = std.mem;
const Os = std.Target.Os;
const testing = std.testing;

const LangOpts = @import("LangOpts.zig");
const QualType = @import("../AST/TypeStore.zig").QualType;
const TargetSet = @import("../Builtins/Properties.zig").TargetSet;

/// intmax_t for this target
pub fn intMaxType(target: *const std.Target) QualType {
    switch (target.cpu.arch) {
        .aarch64,
        .aarch64_be,
        .sparc64,
        => if (target.os.tag != .openbsd) return .long,

        .bpfel,
        .bpfeb,
        .loongarch64,
        .riscv64,
        .powerpc64,
        .powerpc64le,
        .ve,
        => return .long,

        .x86_64 => switch (target.os.tag) {
            .windows, .openbsd => {},
            else => switch (target.abi) {
                .gnux32, .muslx32 => {},
                else => return .long,
            },
        },

        else => {},
    }
    return .longlong;
}

/// intptr_t for this target
pub fn intPtrType(target: *const std.Target) QualType {
    if (target.os.tag == .haiku) return .long;

    switch (target.cpu.arch) {
        .aarch64, .aarch64_be => switch (target.os.tag) {
            .windows => return .longlong,
            else => {},
        },

        .msp430,
        .csky,
        .loongarch32,
        .riscv32,
        .xcore,
        .hexagon,
        .m68k,
        .spirv32,
        .arc,
        .avr,
        => return .int,

        .sparc => switch (target.os.tag) {
            .netbsd, .openbsd => {},
            else => return .int,
        },

        .powerpc, .powerpcle => switch (target.os.tag) {
            .linux, .freebsd, .netbsd => return .int,
            else => {},
        },

        // 32-bit x86 Darwin, OpenBSD, and RTEMS use long (the default); others use int
        .x86 => switch (target.os.tag) {
            .openbsd, .rtems => {},
            else => if (!target.os.tag.isDarwin()) return .int,
        },

        .x86_64 => switch (target.os.tag) {
            .windows => return .longlong,
            else => switch (target.abi) {
                .gnux32, .muslx32 => return .int,
                else => {},
            },
        },

        else => {},
    }

    return .long;
}

/// int16_t for this target
pub fn int16Type(target: *const std.Target) QualType {
    return switch (target.cpu.arch) {
        .avr => .int,
        else => .short,
    };
}

/// int64_t for this target
pub fn int64Type(target: *const std.Target) QualType {
    switch (target.cpu.arch) {
        .loongarch64,
        .ve,
        .riscv64,
        .powerpc64,
        .powerpc64le,
        .bpfel,
        .bpfeb,
        => return .long,

        .sparc64 => return intMaxType(target),

        .x86, .x86_64 => if (!target.os.tag.isDarwin()) return intMaxType(target),

        .aarch64,
        .aarch64_be,
        => if (!target.os.tag.isDarwin() and target.os.tag != .openbsd and target.os.tag != .windows)
            return .long,

        else => {},
    }
    return .longlong;
}

pub fn float80Type(target: *const std.Target) ?QualType {
    switch (target.cpu.arch) {
        .x86, .x86_64 => return .longDouble,
        else => {},
    }
    return null;
}

/// This function returns 1 if function alignment is not observable or settable.
pub fn defaultFunctionAlignment(target: *const std.Target) u8 {
    return switch (target.cpu.arch) {
        .arm, .armeb => 4,
        .aarch64, .aarch64_be => 4,
        .sparc, .sparc64 => 4,
        .riscv64 => 2,
        else => 1,
    };
}

/// Determines whether the current compilation target supports
/// Thread Local Storage (TLS) or not.
///
/// On Darwin(Apple) platforms, it depends on the macOS version.
/// For other platforms, it depends on the CPU architecture.
///
/// Returns true if the target supports TLS, false otherwise.
pub fn isTlsSupported(target: *const std.Target) bool {
    if (target.os.tag.isDarwin()) {
        var supported = false;
        switch (target.os.tag) {
            .macos => supported = !(target.os.isAtLeast(.macos, .{ .major = 10, .minor = 7, .patch = 0 }) orelse false),
            else => {},
        }
        return supported;
    }
    return switch (target.cpu.arch) {
        .bpfel, .bpfeb, .msp430, .nvptx, .nvptx64, .x86, .arm, .armeb, .thumb, .thumbeb => false,
        else => true,
    };
}

pub fn isLP64(target: *const std.Target) bool {
    return target.cTypeBitSize(.int) == 32 and target.ptrBitWidth() == 64;
}

pub fn isKnownWindowsMSVCEnvironment(target: *const std.Target) bool {
    return target.os.tag == .windows and target.abi == .msvc;
}

pub fn isWindowsMSVCEnvironment(target: *const std.Target) bool {
    return target.os.tag == .windows and (target.abi == .msvc or target.abi == .none);
}

pub fn isMinGW(target: *const std.Target) bool {
    return target.os.tag == .windows and target.abi.isGnu();
}

pub fn isPS(target: *const std.Target) bool {
    return (target.os.tag == .ps4 or target.os.tag == .ps5) and target.cpu.arch == .x86_64;
}

pub fn isAbi(target: *const std.Target, query: []const u8) bool {
    var buf: [64]u8 = undefined;
    const lower = toLower(query, &buf) orelse return false;
    if (std.meta.stringToEnum(Abi, lower)) |some| {
        if (some == .none and target.os.tag == .maccatalyst) {
            // Clang thinks maccatalyst has macabi
            return false;
        }
        return target.abi == some;
    }
    if (mem.eql(u8, lower, "macabi")) {
        return target.os.tag == .maccatalyst;
    }
    return false;
}

fn toLower(src: []const u8, dest: []u8) ?[]const u8 {
    if (src.len > dest.len) return null;
    for (src, dest[0..src.len]) |a, *b| {
        b.* = std.ascii.toLower(a);
    }
    return dest[0..src.len];
}

/// Determines whether to ignore the alignment requirements for non-zero-sized
/// bitfield types for the given target architecture and operating system.
pub fn ignoreNonZeroSizedBitfieldTypeAlignment(target: *const std.Target) bool {
    switch (target.cpu.arch) {
        .avr => return true,
        .arm => {
            if (std.Target.arm.featureSetHas(target.cpu.features, .has_v7)) {
                switch (target.os.tag) {
                    .ios => return true,
                    else => return false,
                }
            }
        },
        else => return false,
    }
    return false;
}

pub fn ignoreZeroSizedBitfieldTypeAlignment(target: *const std.Target) bool {
    switch (target.cpu.arch) {
        .avr => return true,
        else => return false,
    }
}

pub fn minZeroWidthBitfieldAlignment(target: *const std.Target) ?u29 {
    switch (target.cpu.arch) {
        .avr => return 8,
        .arm => {
            if (std.Target.arm.featureSetHas(target.cpu.features, .has_v7)) {
                switch (target.os.tag) {
                    .ios => return 32,
                    else => return null,
                }
            } else return null;
        },
        else => return null,
    }
}

/// Determines whether the presence of an unnamed field in a struct affects
/// the alignment of the struct for the given target architecture and operating system.
pub fn unnamedFieldAffectsAlignment(target: *const std.Target) bool {
    switch (target.cpu.arch) {
        .aarch64 => {
            if (target.os.tag.isDarwin() or target.os.tag == .windows) return false;
            return true;
        },
        .armeb => {
            if (std.Target.arm.featureSetHas(target.cpu.features, .has_v7)) {
                if (std.Target.Abi.default(target.cpu.arch, target.os.tag) == .eabi) return true;
            }
        },
        .arm => return true,
        .avr => return true,
        .thumb => {
            if (target.os.tag == .windows) return false;
            return true;
        },
        else => return false,
    }
    return false;
}

pub fn packAllEnums(target: *const std.Target) bool {
    return switch (target.cpu.arch) {
        .hexagon => true,
        else => false,
    };
}

/// Default alignment (in bytes) for __attribute__((aligned)) when no alignment is specified
pub fn defaultAlignment(target: *const std.Target) u29 {
    switch (target.cpu.arch) {
        .avr => return 1,
        .arm => if (target.abi.isAndroid() or target.os.tag == .ios) return 16 else return 8,
        .sparc => if (std.Target.sparc.featureSetHas(target.cpu.features, .v9)) return 16 else return 8,
        .mips, .mipsel => switch (target.abi) {
            .none, .gnuabi64 => return 16,
            else => return 8,
        },
        .s390x, .armeb, .thumbeb, .thumb => return 8,
        else => return 16,
    }
}

pub fn systemCompiler(target: *const std.Target) LangOpts.Compiler {
    if (target.abi.isAndroid() or
        target.os.tag.isBSD() or
        target.os.tag == .fuchsia or
        target.os.tag == .illumos or
        target.os.tag == .haiku or
        target.cpu.arch == .hexagon)
    {
        return .clang;
    }

    if (target.os.tag == .uefi)
        return .msvc;

    // this is before windows to grab WindowsGnu
    if (target.abi.isGnu() or target.os.tag == .linux)
        return .gcc;

    if (target.os.tag == .windows)
        return .msvc;

    if (target.cpu.arch == .avr)
        return .gcc;

    return .clang;
}

pub fn hasInt128(target: *const std.Target) bool {
    if (target.cpu.arch == .wasm32) return true;
    return target.ptrBitWidth() >= 64;
}

pub fn hasHalfPrecisionFloatABI(target: *const std.Target) bool {
    return switch (target.cpu.arch) {
        .thumb, .thumbeb, .arm, .aarch64 => true,
        else => false,
    };
}

pub fn hasFloat128(target: *const std.Target) bool {
    if (target.cpu.arch.isWasm()) return true;
    if (target.os.tag.isDarwin()) return false;
    if (target.cpu.arch.isPowerPC()) return std.Target.powerpc.featureSetHas(target.cpu.features, .float128);
    return switch (target.os.tag) {
        .dragonfly,
        .haiku,
        .linux,
        .openbsd,
        .illumos,
        => target.cpu.arch.isX86(),
        else => false,
    };
}

pub const FPSemantics = enum {
    None,
    IEEEHalf,
    BFloat,
    IEEESingle,
    IEEEDouble,
    IEEEQuad,
    /// Minifloat 5-bit exponent 2-bit mantissa
    E5M2,
    /// Minifloat 4-bit exponent 3-bit mantissa
    E4M3,
    x87ExtendedDouble,
    IBMExtendedDouble,

    /// Only intended for generating float.h macros for the preprocessor
    pub fn forType(ty: std.Target.CType, target: *const std.Target) FPSemantics {
        std.debug.assert(ty == .float or ty == .double or ty == .longdouble);
        return switch (target.cTypeBitSize(ty)) {
            32 => .IEEESingle,
            64 => .IEEEDouble,
            80 => .x87ExtendedDouble,
            128 => switch (target.cpu.arch) {
                .powerpc, .powerpcle, .powerpc64, .powerpc64le => .IBMExtendedDouble,
                else => .IEEEQuad,
            },
            else => unreachable,
        };
    }

    pub fn halfPrecisionType(target: *const std.Target) ?FPSemantics {
        switch (target.cpu.arch) {
            .aarch64,
            .aarch64_be,
            .arm,
            .armeb,
            .hexagon,
            .riscv32,
            .riscv64,
            .spirv32,
            .spirv64,
            => return .IEEEHalf,
            .x86, .x86_64 => if (std.Target.x86.featureSetHas(target.cpu.features, .sse2)) return .IEEEHalf,
            else => {},
        }
        return null;
    }

    pub fn chooseValue(self: FPSemantics, comptime T: type, values: [6]T) T {
        return switch (self) {
            .IEEEHalf => values[0],
            .IEEESingle => values[1],
            .IEEEDouble => values[2],
            .x87ExtendedDouble => values[3],
            .IBMExtendedDouble => values[4],
            .IEEEQuad => values[5],
            else => unreachable,
        };
    }
};

/// Value of the `-m` flag for `ld` for this target
pub fn ldEmulationOption(target: *const std.Target, armEndianness: ?std.builtin.Endian) ?[]const u8 {
    return switch (target.cpu.arch) {
        .arm,
        .armeb,
        .thumb,
        .thumbeb,
        => switch (armEndianness orelse target.cpu.arch.endian()) {
            .little => "armelf_linux_eabi",
            .big => "armelfb_linux_eabi",
        },
        .aarch64 => "aarch64linux",
        .aarch64_be => "aarch64linuxb",
        .csky => "cskyelf_linux",
        .loongarch32 => "elf32loongarch",
        .loongarch64 => "elf64loongarch",
        .mips => "elf32btsmip",
        .mips64 => switch (target.abi) {
            .gnuabin32, .muslabin32 => "elf32btsmipn32",
            else => "elf64btsmip",
        },
        .mips64el => switch (target.abi) {
            .gnuabin32, .muslabin32 => "elf32ltsmipn32",
            else => "elf64ltsmip",
        },
        .mipsel => "elf32ltsmip",
        .powerpc => if (target.os.tag == .linux) "elf32ppclinux" else "elf32ppc",
        .powerpc64 => "elf64ppc",
        .powerpc64le => "elf64lppc",
        .powerpcle => if (target.os.tag == .linux) "elf32lppclinux" else "elf32lppc",
        .riscv32 => "elf32lriscv",
        .riscv64 => "elf64lriscv",
        .sparc => "elf32_sparc",
        .sparc64 => "elf64_sparc",
        .ve => "elf64ve",
        .x86 => "elf_i386",
        .x86_64 => switch (target.abi) {
            .gnux32, .muslx32 => "elf32_x86_64",
            else => "elf_x86_64",
        },
        else => null,
    };
}

pub fn get32BitArchVariant(target: *const std.Target) ?std.Target {
    var copy = target.*;
    switch (target.cpu.arch) {
        .alpha,
        .amdgcn,
        .avr,
        .bpfeb,
        .bpfel,
        .kvx,
        .msp430,
        .s390x,
        .ve,
        => return null,

        .arc,
        .arceb,
        .arm,
        .armeb,
        .csky,
        .hexagon,
        .hppa,
        .kalimba,
        .lanai,
        .loongarch32,
        .m68k,
        .microblaze,
        .microblazeel,
        .mips,
        .mipsel,
        .nvptx,
        .or1k,
        .powerpc,
        .powerpcle,
        .propeller,
        .riscv32,
        .riscv32be,
        .sh,
        .sheb,
        .sparc,
        .spirv32,
        .thumb,
        .thumbeb,
        .wasm32,
        .x86,
        .xcore,
        .xtensa,
        .xtensaeb,
        => {}, // Already 32 bit

        .aarch64 => copy.cpu.arch = .arm,
        .aarch64_be => copy.cpu.arch = .armeb,
        .hppa64 => copy.cpu.arch = .hppa,
        .loongarch64 => copy.cpu.arch = .loongarch32,
        .mips64 => copy.cpu.arch = .mips,
        .mips64el => copy.cpu.arch = .mipsel,
        .nvptx64 => copy.cpu.arch = .nvptx,
        .powerpc64 => copy.cpu.arch = .powerpc,
        .powerpc64le => copy.cpu.arch = .powerpcle,
        .riscv64 => copy.cpu.arch = .riscv32,
        .riscv64be => copy.cpu.arch = .riscv32be,
        .sparc64 => copy.cpu.arch = .sparc,
        .spirv64 => copy.cpu.arch = .spirv32,
        .wasm64 => copy.cpu.arch = .wasm32,
        .x86_16 => copy.cpu.arch = .x86,
        .x86_64 => copy.cpu.arch = .x86,
    }
    return copy;
}

pub fn get64BitArchVariant(target: *const std.Target) ?std.Target {
    var copy = target.*;
    switch (target.cpu.arch) {
        .arc,
        .arceb,
        .avr,
        .csky,
        .hexagon,
        .kalimba,
        .lanai,
        .m68k,
        .microblaze,
        .microblazeel,
        .msp430,
        .or1k,
        .propeller,
        .sh,
        .sheb,
        .xcore,
        .xtensa,
        .xtensaeb,
        => return null,

        .aarch64_be,
        .aarch64,
        .alpha,
        .amdgcn,
        .bpfeb,
        .bpfel,
        .hppa64,
        .kvx,
        .loongarch64,
        .mips64,
        .mips64el,
        .nvptx64,
        .powerpc64,
        .powerpc64le,
        .riscv64,
        .riscv64be,
        .s390x,
        .sparc64,
        .spirv64,
        .ve,
        .wasm64,
        .x86_64,
        => {}, // Already 64 bit

        .arm => copy.cpu.arch = .aarch64,
        .armeb => copy.cpu.arch = .aarch64_be,
        .hppa => copy.cpu.arch = .hppa64,
        .loongarch32 => copy.cpu.arch = .loongarch64,
        .mips => copy.cpu.arch = .mips64,
        .mipsel => copy.cpu.arch = .mips64el,
        .nvptx => copy.cpu.arch = .nvptx64,
        .powerpc => copy.cpu.arch = .powerpc64,
        .powerpcle => copy.cpu.arch = .powerpc64le,
        .riscv32 => copy.cpu.arch = .riscv64,
        .riscv32be => copy.cpu.arch = .riscv64be,
        .sparc => copy.cpu.arch = .sparc64,
        .spirv32 => copy.cpu.arch = .spirv64,
        .thumb => copy.cpu.arch = .aarch64,
        .thumbeb => copy.cpu.arch = .aarch64_be,
        .wasm32 => copy.cpu.arch = .wasm64,
        .x86 => copy.cpu.arch = .x86_64,
        .x86_16 => copy.cpu.arch = .x86_64,
    }
    return copy;
}

/// Adapted from Zig's src/codegen/llvm.zig
pub fn toLLVMTriple(target: *const std.Target, buf: []u8) []const u8 {
    // 64 bytes is assumed to be large enough to hold any target triple; increase if necessary
    std.debug.assert(buf.len >= 64);

    var writer: std.Io.Writer = .fixed(buf);

    const llvmArch = switch (target.cpu.arch) {
        .aarch64 => if (target.abi == .ilp32) "aarch64_32" else "aarch64",
        .aarch64_be => "aarch64_be",
        .amdgcn => "amdgcn",
        .arc => "arc",
        .arm => "arm",
        .armeb => "armeb",
        .avr => "avr",
        .bpfeb => "bpfeb",
        .bpfel => "bpfel",
        .csky => "csky",
        .hexagon => "hexagon",
        .lanai => "lanai",
        .loongarch32 => "loongarch32",
        .loongarch64 => "loongarch64",
        .m68k => "m68k",
        .mips => "mips",
        .mips64 => "mips64",
        .mips64el => "mips64el",
        .mipsel => "mipsel",
        .msp430 => "msp430",
        .nvptx => "nvptx",
        .nvptx64 => "nvptx64",
        .powerpc => "powerpc",
        .powerpc64 => "powerpc64",
        .powerpc64le => "powerpc64le",
        .powerpcle => "powerpcle",
        .riscv32 => "riscv32",
        .riscv32be => "riscv32be",
        .riscv64 => "riscv64",
        .riscv64be => "riscv64be",
        .s390x => "s390x",
        .sparc => "sparc",
        .sparc64 => "sparc64",
        .spirv32 => "spirv32",
        .spirv64 => "spirv64",
        .thumb => "thumb",
        .thumbeb => "thumbeb",
        .ve => "ve",
        .wasm32 => "wasm32",
        .wasm64 => "wasm64",
        .x86 => "i386",
        .x86_64 => "x86_64",
        .xcore => "xcore",
        .xtensa => "xtensa",
        // Note: these are not supported in LLVM; this is the Zig arch name
        .alpha => "alpha",
        .arceb => "arceb",
        .hppa => "hppa",
        .hppa64 => "hppa64",
        .kalimba => "kalimba",
        .kvx => "kvx",
        .microblaze => "microblaze",
        .microblazeel => "microblazeel",
        .or1k => "or1k",
        .propeller => "propeller",
        .sh => "sh",
        .sheb => "sheb",
        .x86_16 => "i86",
        .xtensaeb => "xtensaeb",
    };
    writer.writeAll(llvmArch) catch unreachable;
    writer.writeByte('-') catch unreachable;

    const llvm_os = switch (target.os.tag) {
        .amdhsa => "amdhsa",
        .amdpal => "amdpal",
        .contiki => "contiki",
        .cuda => "cuda",
        .dragonfly => "dragonfly",
        .driverkit => "driverkit",
        .emscripten => "emscripten",
        .freebsd => "freebsd",
        .freestanding => "unknown",
        .fuchsia => "fuchsia",
        .haiku => "haiku",
        .hermit => "hermit",
        .hurd => "hurd",
        .illumos => "illumos",
        .ios, .maccatalyst => "ios",
        .linux => "linux",
        .macos => "macosx",
        .managarm => "managarm",
        .mesa3d => "mesa3d",
        .netbsd => "netbsd",
        .nvcl => "nvcl",
        .openbsd => "openbsd",
        .ps3 => "lv2",
        .ps4 => "ps4",
        .ps5 => "ps5",
        .rtems => "rtems",
        .serenity => "serenity",
        .tvos => "tvos",
        .uefi => "windows",
        .visionos => "xros",
        .vulkan => "vulkan",
        .wasi => "wasi",
        .watchos => "watchos",
        .windows => "windows",

        .@"3ds",
        .opencl,
        .opengl,
        .other,
        .plan9,
        .vita,
        => "unknown",
    };
    writer.writeAll(llvm_os) catch unreachable;

    if (target.os.tag.isDarwin()) {
        const minVersion = target.os.version_range.semver.min;
        writer.print("{d}.{d}.{d}", .{
            minVersion.major,
            minVersion.minor,
            minVersion.patch,
        }) catch unreachable;
    }
    writer.writeByte('-') catch unreachable;

    const LLVMAbi = switch (target.abi) {
        .none => if (target.os.tag == .maccatalyst) "macabi" else "unknown",
        .ilp32 => "unknown",

        .android => "android",
        .androideabi => "androideabi",
        .eabi => "eabi",
        .eabihf => "eabihf",
        .gnu => "gnu",
        .gnuabi64 => "gnuabi64",
        .gnuabin32 => "gnuabin32",
        .gnueabi => "gnueabi",
        .gnueabihf => "gnueabihf",
        .gnuf32 => "gnuf32",
        .gnusf => "gnusf",
        .gnux32 => "gnux32",
        .itanium => "itanium",
        .msvc => "msvc",
        .musl => "musl",
        .muslabi64 => "muslabi64",
        .muslabin32 => "muslabin32",
        .musleabi => "musleabi",
        .musleabihf => "musleabihf",
        .muslf32 => "muslf32",
        .muslsf => "muslsf",
        .muslx32 => "muslx32",
        .ohos => "ohos",
        .ohoseabi => "ohoseabi",
        .simulator => "simulator",
    };
    writer.writeAll(LLVMAbi) catch unreachable;
    return writer.buffered();
}

pub const DefaultPIStatus = enum { Yes, No, DependsOnLinker };

pub fn isPIEDefault(target: *const std.Target) DefaultPIStatus {
    return switch (target.os.tag) {
        .haiku,

        .maccatalyst,
        .macos,
        .ios,
        .tvos,
        .watchos,
        .visionos,
        .driverkit,

        .dragonfly,
        .netbsd,
        .freebsd,
        .illumos,

        .cuda,
        .amdhsa,
        .amdpal,
        .mesa3d,

        .ps4,
        .ps5,

        .hurd,
        => .No,

        .openbsd,
        .fuchsia,
        => .Yes,

        .linux => {
            if (target.abi == .ohos)
                return .Yes;

            switch (target.cpu.arch) {
                .ve => return .No,
                else => return if (target.os.tag == .linux or target.abi.isAndroid() or target.abi.isMusl()) .Yes else .No,
            }
        },

        .windows => {
            if (target.isMinGW())
                return .No;

            if (target.abi == .itanium)
                return if (target.cpu.arch == .x86_64) .Yes else .No;

            if (target.abi == .msvc or target.abi == .none)
                return .DependsOnLinker;

            return .No;
        },

        else => {
            switch (target.cpu.arch) {
                .hexagon => {
                    // CLANG_DEFAULT_PIE_ON_LINUX
                    return if (target.os.tag == .linux or target.abi.isAndroid() or target.abi.isMusl()) .Yes else .No;
                },

                else => return .No,
            }
        },
    };
}

pub fn isPICdefault(target: *const std.Target) DefaultPIStatus {
    return switch (target.os.tag) {
        .haiku,

        .maccatalyst,
        .macos,
        .ios,
        .tvos,
        .watchos,
        .visionos,
        .driverkit,

        .amdhsa,
        .amdpal,
        .mesa3d,

        .ps4,
        .ps5,
        => .Yes,

        .fuchsia,
        .cuda,
        => .No,

        .dragonfly,
        .openbsd,
        .netbsd,
        .freebsd,
        .illumos,
        .hurd,
        => {
            return switch (target.cpu.arch) {
                .mips64, .mips64el => .Yes,
                else => .No,
            };
        },

        .linux => {
            if (target.abi == .ohos)
                return .No;

            return switch (target.cpu.arch) {
                .mips64, .mips64el => .Yes,
                else => .No,
            };
        },

        .windows => {
            if (target.isMinGW())
                return if (target.cpu.arch == .x86_64 or target.cpu.arch == .aarch64) .Yes else .No;

            if (target.abi == .itanium)
                return if (target.cpu.arch == .x86_64) .Yes else .No;

            if (target.abi == .msvc or target.abi == .none)
                return .DependsOnLinker;

            if (target.ofmt == .macho)
                return .Yes;

            return switch (target.cpu.arch) {
                .x86_64, .mips64, .mips64el => .Yes,
                else => .No,
            };
        },

        else => {
            if (target.ofmt == .macho)
                return .Yes;

            return switch (target.cpu.arch) {
                .mips64, .mips64el => .Yes,
                else => .No,
            };
        },
    };
}

pub fn isPICDefaultForced(target: *const std.Target) DefaultPIStatus {
    return switch (target.os.tag) {
        .amdhsa, .amdpal, .mesa3d => .Yes,

        .haiku,
        .dragonfly,
        .openbsd,
        .netbsd,
        .freebsd,
        .illumos,
        .cuda,
        .ps4,
        .ps5,
        .hurd,
        .linux,
        .fuchsia,
        => .No,

        .windows => {
            if (target.isMinGW())
                return .Yes;

            if (target.abi == .itanium)
                return if (target.cpu.arch == .x86_64) .Yes else .No;

            // if (bfd) return target.cpu.arch == .x86_64 else target.cpu.arch == .x86_64 or target.cpu.arch == .aarch64;
            if (target.abi == .msvc or target.abi == .none)
                return .DependsOnLinker;

            if (target.ofmt == .macho)
                return if (target.cpu.arch == .aarch64 or target.cpu.arch == .x86_64) .Yes else .No;

            return if (target.cpu.arch == .x86_64) .Yes else .No;
        },

        .maccatalyst,
        .macos,
        .ios,
        .tvos,
        .watchos,
        .visionos,
        .driverkit,
        => if (target.cpu.arch == .x86_64 or target.cpu.arch == .aarch64) .Yes else .No,

        else => {
            return switch (target.cpu.arch) {
                .hexagon,
                .lanai,
                .avr,
                .riscv32,
                .riscv64,
                .csky,
                .xcore,
                .wasm32,
                .wasm64,
                .ve,
                .spirv32,
                .spirv64,
                => .No,

                .msp430 => .Yes,

                else => {
                    if (target.ofmt == .macho)
                        return if (target.cpu.arch == .aarch64 or target.cpu.arch == .x86_64) .Yes else .No;
                    return .No;
                },
            };
        },
    };
}

test "alignment functions - smoke test" {
    const linux: std.Target.Os = .{ .tag = .linux, .version_range = .{ .none = {} } };
    const x86_64_target: std.Target = .{
        .abi = .default(.x86_64, linux.tag),
        .cpu = std.Target.Cpu.Model.generic(.x86_64).toCpu(.x86_64),
        .os = linux,
        .ofmt = .elf,
    };

    try std.testing.expect(isTlsSupported(&x86_64_target));
    try std.testing.expect(!ignoreNonZeroSizedBitfieldTypeAlignment(&x86_64_target));
    try std.testing.expect(minZeroWidthBitfieldAlignment(&x86_64_target) == null);
    try std.testing.expect(!unnamedFieldAffectsAlignment(&x86_64_target));
    try std.testing.expect(defaultAlignment(&x86_64_target) == 16);
    try std.testing.expect(!packAllEnums(&x86_64_target));
    try std.testing.expect(systemCompiler(&x86_64_target) == .gcc);
}

test "target size/align tests" {
    var comp: @import("Compilation.zig") = undefined;

    const linux: std.Target.Os = .{ .tag = .linux, .version_range = .{ .none = {} } };
    const x86_target: std.Target = .{
        .abi = .default(.x86, linux.tag),
        .cpu = std.Target.Cpu.Model.generic(.x86).toCpu(.x86),
        .os = linux,
        .ofmt = .elf,
    };
    comp.target = x86_target;

    const tt: QualType = .longlong;

    try std.testing.expectEqual(@as(u64, 8), tt.sizeof(&comp));
    try std.testing.expectEqual(@as(u64, 4), tt.alignof(&comp));
}

/// The canonical integer representation of nullptr_t.
pub fn nullRepr(_: *const std.Target) u64 {
    return 0;
}

pub fn builtinEnabled(target: *const std.Target, enabledFor: TargetSet) bool {
    var it = enabledFor.iterator();
    while (it.next()) |val| {
        switch (val) {
            .basic => return true,
            .x86_64 => if (target.cpu.arch == .x86_64) return true,
            .aarch64 => if (target.cpu.arch == .aarch64) return true,
            .arm => if (target.cpu.arch == .arm) return true,
            .ppc => switch (target.cpu.arch) {
                .powerpc, .powerpc64, .powerpc64le => return true,
                else => {},
            },
            else => {
                // Todo: handle other target predicates
            },
        }
    }
    return false;
}

pub fn defaultFpEvalMethod(target: *const std.Target) LangOpts.FPEvalMethod {
    switch (target.cpu.arch) {
        .x86, .x86_64 => {
            if (target.ptrBitWidth() == 32 and target.os.tag == .netbsd) {
                if (target.os.version_range.semver.min.order(.{ .major = 6, .minor = 99, .patch = 26 }) != .gt) {
                    // NETBSD <= 6.99.26 on 32-bit x86 defaults to double
                    return .double;
                }
            }
            if (std.Target.x86.featureSetHas(target.cpu.features, .sse)) {
                return .source;
            }
            return .extended;
        },
        else => {},
    }
    return .source;
}
