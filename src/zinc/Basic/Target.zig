const std = @import("std");

const LangOpts = @import("LangOpts.zig");
const TargetSet = @import("../Builtins/Properties.zig").TargetSet;
const QualType = @import("../AST/TypeStore.zig").QualType;
const backend = @import("backend");

/// intmax_t for this target
pub fn intMaxType(target: std.Target) QualType {
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
pub fn intPtrType(target: std.Target) QualType {
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
pub fn int16Type(target: std.Target) QualType {
    return switch (target.cpu.arch) {
        .avr => .int,
        else => .short,
    };
}

/// int64_t for this target
pub fn int64Type(target: std.Target) QualType {
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

pub fn float80Type(target: std.Target) ?QualType {
    switch (target.cpu.arch) {
        .x86, .x86_64 => return .longDouble,
        else => {},
    }
    return null;
}

/// This function returns 1 if function alignment is not observable or settable.
pub fn defaultFunctionAlignment(target: std.Target) u8 {
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
pub fn isTlsSupported(target: std.Target) bool {
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

pub fn isLP64(target: std.Target) bool {
    return target.cTypeBitSize(.int) == 32 and target.ptrBitWidth() == 64;
}

pub fn isKnownWindowsMSVCEnvironment(target: std.Target) bool {
    return target.os.tag == .windows and target.abi == .msvc;
}

pub fn isWindowsMSVCEnvironment(target: std.Target) bool {
    return target.os.tag == .windows and (target.abi == .msvc or target.abi == .none);
}

pub fn isCygwinMinGW(target: std.Target) bool {
    return target.os.tag == .windows and (target.abi == .gnu or target.abi == .cygnus);
}

/// Determines whether to ignore the alignment requirements for non-zero-sized
/// bitfield types for the given target architecture and operating system.
pub fn ignoreNonZeroSizedBitfieldTypeAlignment(target: std.Target) bool {
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

pub fn ignoreZeroSizedBitfieldTypeAlignment(target: std.Target) bool {
    switch (target.cpu.arch) {
        .avr => return true,
        else => return false,
    }
}

pub fn minZeroWidthBitfieldAlignment(target: std.Target) ?u29 {
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
pub fn unnamedFieldAffectsAlignment(target: std.Target) bool {
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

pub fn packAllEnums(target: std.Target) bool {
    return switch (target.cpu.arch) {
        .hexagon => true,
        else => false,
    };
}

/// Default alignment (in bytes) for __attribute__((aligned)) when no alignment is specified
pub fn defaultAlignment(target: std.Target) u29 {
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

pub fn systemCompiler(target: std.Target) LangOpts.Compiler {
    if (target.abi.isAndroid() or
        target.os.tag.isBSD() or
        target.os.tag == .fuchsia or
        target.os.tag == .solaris or
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

pub fn hasInt128(target: std.Target) bool {
    if (target.cpu.arch == .wasm32) return true;
    return target.ptrBitWidth() >= 64;
}

pub fn hasHalfPrecisionFloatABI(target: std.Target) bool {
    return switch (target.cpu.arch) {
        .thumb, .thumbeb, .arm, .aarch64 => true,
        else => false,
    };
}

pub fn hasFloat128(target: std.Target) bool {
    if (target.cpu.arch.isWasm()) return true;
    if (target.os.tag.isDarwin()) return false;
    if (target.cpu.arch.isPowerPC()) return std.Target.powerpc.featureSetHas(target.cpu.features, .float128);
    return switch (target.os.tag) {
        .dragonfly,
        .haiku,
        .linux,
        .openbsd,
        .solaris,
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
    pub fn forType(ty: std.Target.CType, target: std.Target) FPSemantics {
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

    pub fn halfPrecisionType(target: std.Target) ?FPSemantics {
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
pub fn ldEmulationOption(target: std.Target, armEndianness: ?std.builtin.Endian) ?[]const u8 {
    return switch (target.cpu.arch) {
        .x86 => "elf_i386",
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
        .m68k => "m68kelf",
        .powerpc => if (target.os.tag == .linux) "elf32ppclinux" else "elf32ppc",
        .powerpcle => if (target.os.tag == .linux) "elf32lppclinux" else "elf32lppc",
        .powerpc64 => "elf64ppc",
        .powerpc64le => "elf64lppc",
        .riscv32 => "elf32lriscv",
        .riscv64 => "elf64lriscv",
        .sparc => "elf32_sparc",
        .sparc64 => "elf64_sparc",
        .loongarch32 => "elf32loongarch",
        .loongarch64 => "elf64loongarch",
        .mips => "elf32btsmip",
        .mipsel => "elf32ltsmip",
        .mips64 => switch (target.abi) {
            .gnuabin32, .muslabin32 => "elf32btsmipn32",
            else => "elf64btsmip",
        },
        .mips64el => switch (target.abi) {
            .gnuabin32, .muslabin32 => "elf32ltsmipn32",
            else => "elf64ltsmip",
        },
        .x86_64 => switch (target.abi) {
            .gnux32, .muslx32 => "elf32_x86_64",
            else => "elf_x86_64",
        },
        .ve => "elf64ve",
        .csky => "cskyelf_linux",
        else => null,
    };
}

pub fn get32BitArchVariant(target: std.Target) ?std.Target {
    var copy = target;
    switch (target.cpu.arch) {
        .amdgcn,
        .avr,
        .msp430,
        .ve,
        .bpfel,
        .bpfeb,
        .s390x,
        => return null,

        .arc,
        .arm,
        .armeb,
        .csky,
        .hexagon,
        .m68k,
        .mips,
        .mipsel,
        .powerpc,
        .powerpcle,
        .riscv32,
        .sparc,
        .thumb,
        .thumbeb,
        .x86,
        .xcore,
        .nvptx,
        .kalimba,
        .lanai,
        .wasm32,
        .spirv32,
        .loongarch32,
        .xtensa,
        .propeller,
        .or1k,
        => {}, // Already 32 bit

        .aarch64 => copy.cpu.arch = .arm,
        .aarch64_be => copy.cpu.arch = .armeb,
        .nvptx64 => copy.cpu.arch = .nvptx,
        .wasm64 => copy.cpu.arch = .wasm32,
        .spirv64 => copy.cpu.arch = .spirv32,
        .loongarch64 => copy.cpu.arch = .loongarch32,
        .mips64 => copy.cpu.arch = .mips,
        .mips64el => copy.cpu.arch = .mipsel,
        .powerpc64 => copy.cpu.arch = .powerpc,
        .powerpc64le => copy.cpu.arch = .powerpcle,
        .riscv64 => copy.cpu.arch = .riscv32,
        .sparc64 => copy.cpu.arch = .sparc,
        .x86_64 => copy.cpu.arch = .x86,
    }
    return copy;
}

pub fn get64BitArchVariant(target: std.Target) ?std.Target {
    var copy = target;
    switch (target.cpu.arch) {
        .arc,
        .avr,
        .csky,
        .hexagon,
        .kalimba,
        .lanai,
        .m68k,
        .msp430,
        .xcore,
        .xtensa,
        .propeller,
        .or1k,
        => return null,

        .aarch64,
        .aarch64_be,
        .amdgcn,
        .bpfeb,
        .bpfel,
        .nvptx64,
        .wasm64,
        .spirv64,
        .loongarch64,
        .mips64,
        .mips64el,
        .powerpc64,
        .powerpc64le,
        .riscv64,
        .s390x,
        .sparc64,
        .ve,
        .x86_64,
        => {}, // Already 64 bit

        .arm => copy.cpu.arch = .aarch64,
        .armeb => copy.cpu.arch = .aarch64_be,
        .loongarch32 => copy.cpu.arch = .loongarch64,
        .mips => copy.cpu.arch = .mips64,
        .mipsel => copy.cpu.arch = .mips64el,
        .nvptx => copy.cpu.arch = .nvptx64,
        .powerpc => copy.cpu.arch = .powerpc64,
        .powerpcle => copy.cpu.arch = .powerpc64le,
        .riscv32 => copy.cpu.arch = .riscv64,
        .sparc => copy.cpu.arch = .sparc64,
        .spirv32 => copy.cpu.arch = .spirv64,
        .thumb => copy.cpu.arch = .aarch64,
        .thumbeb => copy.cpu.arch = .aarch64_be,
        .wasm32 => copy.cpu.arch = .wasm64,
        .x86 => copy.cpu.arch = .x86_64,
    }
    return copy;
}

/// Adapted from Zig's src/codegen/llvm.zig
pub fn toLLVMTriple(target: std.Target, buf: []u8) []const u8 {
    // 64 bytes is assumed to be large enough to hold any target triple; increase if necessary
    std.debug.assert(buf.len >= 64);

    var stream = std.io.fixedBufferStream(buf);
    const writer = stream.writer();

    const llvm_arch = switch (target.cpu.arch) {
        .arm => "arm",
        .armeb => "armeb",
        .aarch64 => if (target.abi == .ilp32) "aarch64_32" else "aarch64",
        .aarch64_be => "aarch64_be",
        .arc => "arc",
        .avr => "avr",
        .bpfel => "bpfel",
        .bpfeb => "bpfeb",
        .csky => "csky",
        .hexagon => "hexagon",
        .loongarch32 => "loongarch32",
        .loongarch64 => "loongarch64",
        .m68k => "m68k",
        .mips => "mips",
        .mipsel => "mipsel",
        .mips64 => "mips64",
        .mips64el => "mips64el",
        .msp430 => "msp430",
        .powerpc => "powerpc",
        .powerpcle => "powerpcle",
        .powerpc64 => "powerpc64",
        .powerpc64le => "powerpc64le",
        .amdgcn => "amdgcn",
        .riscv32 => "riscv32",
        .riscv64 => "riscv64",
        .sparc => "sparc",
        .sparc64 => "sparc64",
        .s390x => "s390x",
        .thumb => "thumb",
        .thumbeb => "thumbeb",
        .x86 => "i386",
        .x86_64 => "x86_64",
        .xcore => "xcore",
        .xtensa => "xtensa",
        .nvptx => "nvptx",
        .nvptx64 => "nvptx64",
        .spirv32 => "spirv32",
        .spirv64 => "spirv64",
        .kalimba => "kalimba",
        .lanai => "lanai",
        .wasm32 => "wasm32",
        .wasm64 => "wasm64",
        .ve => "ve",
        .propeller => "propeller",
        .or1k => "or1k",
    };
    writer.writeAll(llvm_arch) catch unreachable;
    writer.writeByte('-') catch unreachable;

    const llvm_os = switch (target.os.tag) {
        .freestanding => "unknown",
        .dragonfly => "dragonfly",
        .freebsd => "freebsd",
        .fuchsia => "fuchsia",
        .linux => "linux",
        .ps3 => "lv2",
        .netbsd => "netbsd",
        .openbsd => "openbsd",
        .solaris => "solaris",
        .illumos => "illumos",
        .windows => "windows",
        .zos => "zos",
        .haiku => "haiku",
        .rtems => "rtems",
        .aix => "aix",
        .cuda => "cuda",
        .nvcl => "nvcl",
        .amdhsa => "amdhsa",
        .ps4 => "ps4",
        .ps5 => "ps5",
        .mesa3d => "mesa3d",
        .contiki => "contiki",
        .amdpal => "amdpal",
        .hermit => "hermit",
        .hurd => "hurd",
        .wasi => "wasi",
        .emscripten => "emscripten",
        .uefi => "windows",
        .macos => "macosx",
        .ios => "ios",
        .tvos => "tvos",
        .watchos => "watchos",
        .driverkit => "driverkit",
        .visionos => "xros",
        .serenity => "serenity",
        .opencl,
        .opengl,
        .vulkan,
        .plan9,
        .other,
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
        .none, .ilp32 => "unknown",
        .gnu => "gnu",
        .gnuabin32 => "gnuabin32",
        .gnuabi64 => "gnuabi64",
        .gnueabi => "gnueabi",
        .gnueabihf => "gnueabihf",
        .gnuf32 => "gnuf32",
        .gnusf => "gnusf",
        .gnux32 => "gnux32",
        .code16 => "code16",
        .eabi => "eabi",
        .eabihf => "eabihf",
        .android => "android",
        .androideabi => "androideabi",
        .musl => "musl",
        .muslabin32 => "muslabin32",
        .muslabi64 => "muslabi64",
        .musleabi => "musleabi",
        .musleabihf => "musleabihf",
        .muslf32 => "muslf32",
        .muslsf => "muslsf",
        .muslx32 => "muslx32",
        .msvc => "msvc",
        .itanium => "itanium",
        .cygnus => "cygnus",
        .simulator => "simulator",
        .macabi => "macabi",
        .ohos => "openhos",
        .ohoseabi => "ohoseabi",
    };
    writer.writeAll(LLVMAbi) catch unreachable;
    return stream.getWritten();
}

/// This currently just returns the desired settings without considering target defaults / requirements
pub fn getPICMode(
    target: std.Target,
    desiredPicLevel: ?backend.CodeGenOptions.PicLevel,
    wantsPie: ?bool,
) struct { backend.CodeGenOptions.PicLevel, bool } {
    _ = target;
    return .{ desiredPicLevel orelse .none, wantsPie orelse false };
}

test "alignment functions - smoke test" {
    const linux: std.Target.Os = .{ .tag = .linux, .version_range = .{ .none = {} } };
    const x86_64_target: std.Target = .{
        .abi = std.Target.Abi.default(.x86_64, linux.tag),
        .cpu = std.Target.Cpu.Model.generic(.x86_64).toCpu(.x86_64),
        .os = linux,
        .ofmt = .elf,
    };

    try std.testing.expect(isTlsSupported(x86_64_target));
    try std.testing.expect(!ignoreNonZeroSizedBitfieldTypeAlignment(x86_64_target));
    try std.testing.expect(minZeroWidthBitfieldAlignment(x86_64_target) == null);
    try std.testing.expect(!unnamedFieldAffectsAlignment(x86_64_target));
    try std.testing.expect(defaultAlignment(x86_64_target) == 16);
    try std.testing.expect(!packAllEnums(x86_64_target));
    try std.testing.expect(systemCompiler(x86_64_target) == .gcc);
}

test "target size/align tests" {
    var comp: @import("Compilation.zig") = undefined;

    const linux: std.Target.Os = .{ .tag = .linux, .version_range = .{ .none = {} } };
    const x86_target: std.Target = .{
        .abi = std.Target.Abi.default(.x86, linux.tag),
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
pub fn nullRepr(_: std.Target) u64 {
    return 0;
}

pub fn builtinEnabled(target: std.Target, enabled_for: TargetSet) bool {
    var it = enabled_for.iterator();
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

pub fn defaultFpEvalMethod(target: std.Target) LangOpts.FPEvalMethod {
    if (target.os.tag == .aix) return .double;
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
