const std = @import("std");

const Driver = @import("../Driver.zig");
const GCCVersion = @import("GCCVersion.zig");
const Multilib = @import("Multilib.zig");
const SystemDefaults = @import("system-defaults");
const TargetUtil = @import("../Basic/Target.zig");
const Toolchain = @import("../Toolchain.zig");

const GCCDetector = @This();

isValid: bool = false,
installPath: []const u8 = "",
parentLibPath: []const u8 = "",
version: GCCVersion = .{},
gccTriple: []const u8 = "",
selected: Multilib = .{},
biarchSibling: ?Multilib = null,

pub fn deinit(self: *GCCDetector) void {
    if (!self.isValid) return;
}

pub fn appendToolPath(self: *const GCCDetector, tc: *Toolchain) !void {
    if (!self.isValid) return;
    return tc.addPathFromComponents(&.{
        self.parentLibPath,
        "..",
        self.gccTriple,
        "bin",
    }, .program);
}

fn addDefaultGCCPrefixes(prefixes: *std.ArrayListUnmanaged([]const u8), tc: *const Toolchain) !void {
    const sysroot = tc.getSysroot();
    const target = tc.getTarget();
    if (sysroot.len == 0 and target.os.tag == .linux and tc.filesystem.exists("/opt/rh")) {
        prefixes.appendAssumeCapacity("/opt/rh/gcc-toolset-12/root/usr");
        prefixes.appendAssumeCapacity("/opt/rh/gcc-toolset-11/root/usr");
        prefixes.appendAssumeCapacity("/opt/rh/gcc-toolset-10/root/usr");
        prefixes.appendAssumeCapacity("/opt/rh/devtoolset-12/root/usr");
        prefixes.appendAssumeCapacity("/opt/rh/devtoolset-11/root/usr");
        prefixes.appendAssumeCapacity("/opt/rh/devtoolset-10/root/usr");
        prefixes.appendAssumeCapacity("/opt/rh/devtoolset-9/root/usr");
        prefixes.appendAssumeCapacity("/opt/rh/devtoolset-8/root/usr");
        prefixes.appendAssumeCapacity("/opt/rh/devtoolset-7/root/usr");
        prefixes.appendAssumeCapacity("/opt/rh/devtoolset-6/root/usr");
        prefixes.appendAssumeCapacity("/opt/rh/devtoolset-4/root/usr");
        prefixes.appendAssumeCapacity("/opt/rh/devtoolset-3/root/usr");
        prefixes.appendAssumeCapacity("/opt/rh/devtoolset-2/root/usr");
    }
    if (sysroot.len == 0) {
        prefixes.appendAssumeCapacity("/usr");
    } else {
        var usrPath = try tc.driver.comp.arena.alloc(u8, 4 + sysroot.len);
        @memcpy(usrPath[0..4], "/usr");
        @memcpy(usrPath[4..], sysroot);
        prefixes.appendAssumeCapacity(usrPath);
    }
}

fn collectLibDirsAndTriples(
    tc: *Toolchain,
    libDirs: *std.ArrayListUnmanaged([]const u8),
    tripleAliases: *std.ArrayListUnmanaged([]const u8),
    biarchLibDirs: *std.ArrayListUnmanaged([]const u8),
    biarchTripleAliases: *std.ArrayListUnmanaged([]const u8),
) !void {
    const AArch64LibDirs: [2][]const u8 = .{ "/lib64", "/lib" };
    const AArch64Triples: [4][]const u8 = .{ "aarch64-none-linux-gnu", "aarch64-linux-gnu", "aarch64-redhat-linux", "aarch64-suse-linux" };
    const AArch64beLibDirs: [1][]const u8 = .{"/lib"};
    const AArch64beTriples: [2][]const u8 = .{ "aarch64_be-none-linux-gnu", "aarch64_be-linux-gnu" };

    const ARMLibDirs: [1][]const u8 = .{"/lib"};
    const ARMTriples: [1][]const u8 = .{"arm-linux-gnueabi"};
    const ARMHFTriples: [4][]const u8 = .{ "arm-linux-gnueabihf", "armv7hl-redhat-linux-gnueabi", "armv6hl-suse-linux-gnueabi", "armv7hl-suse-linux-gnueabi" };

    const AVRLibDirs: [1][]const u8 = .{"/lib"};
    const AVRTriples: [1][]const u8 = .{"avr"};

    const CSKYLibDirs: [1][]const u8 = .{"/lib"};
    const CSKYTriples: [3][]const u8 = .{ "csky-linux-gnuabiv2", "csky-linux-uclibcabiv2", "csky-elf-noneabiv2" };

    const ARMebLibDirs: [1][]const u8 = .{"/lib"};
    const ARMebTriples: [1][]const u8 = .{"armeb-linux-gnueabi"};
    const ARMebHFTriples: [2][]const u8 = .{ "armeb-linux-gnueabihf", "armebv7hl-redhat-linux-gnueabi" };

    const X86_64LibDirs: [2][]const u8 = .{ "/lib64", "/lib" };
    const X86_64Triples: [11][]const u8 = .{
        "x86_64-linux-gnu",       "x86_64-unknown-linux-gnu",
        "x86_64-pc-linux-gnu",    "x86_64-redhat-linux6E",
        "x86_64-redhat-linux",    "x86_64-suse-linux",
        "x86_64-manbo-linux-gnu", "x86_64-linux-gnu",
        "x86_64-slackware-linux", "x86_64-unknown-linux",
        "x86_64-amazon-linux",
    };
    const X32Triples: [2][]const u8 = .{ "x86_64-linux-gnux32", "x86_64-pc-linux-gnux32" };
    const X32LibDirs: [2][]const u8 = .{ "/libx32", "/lib" };
    const X86LibDirs: [2][]const u8 = .{ "/lib32", "/lib" };
    const X86Triples: [9][]const u8 = .{
        "i586-linux-gnu",      "i686-linux-gnu",        "i686-pc-linux-gnu",
        "i386-redhat-linux6E", "i686-redhat-linux",     "i386-redhat-linux",
        "i586-suse-linux",     "i686-montavista-linux", "i686-gnu",
    };

    const LoongArch64LibDirs: [2][]const u8 = .{ "/lib64", "/lib" };
    const LoongArch64Triples: [2][]const u8 = .{ "loongarch64-linux-gnu", "loongarch64-unknown-linux-gnu" };

    const M68kLibDirs: [1][]const u8 = .{"/lib"};
    const M68kTriples: [3][]const u8 = .{ "m68k-linux-gnu", "m68k-unknown-linux-gnu", "m68k-suse-linux" };

    const MIPSLibDirs: [2][]const u8 = .{ "/libo32", "/lib" };
    const MIPSTriples: [5][]const u8 = .{
        "mips-linux-gnu",        "mips-mti-linux",
        "mips-mti-linux-gnu",    "mips-img-linux-gnu",
        "mipsisa32r6-linux-gnu",
    };
    const MIPSELLibDirs: [2][]const u8 = .{ "/libo32", "/lib" };
    const MIPSELTriples: [3][]const u8 = .{ "mipsel-linux-gnu", "mips-img-linux-gnu", "mipsisa32r6el-linux-gnu" };

    const MIPS64LibDirs: [2][]const u8 = .{ "/lib64", "/lib" };
    const MIPS64Triples: [6][]const u8 = .{
        "mips64-linux-gnu",      "mips-mti-linux-gnu",
        "mips-img-linux-gnu",    "mips64-linux-gnuabi64",
        "mipsisa64r6-linux-gnu", "mipsisa64r6-linux-gnuabi64",
    };
    const MIPS64ELLibDirs: [2][]const u8 = .{ "/lib64", "/lib" };
    const MIPS64ELTriples: [6][]const u8 = .{
        "mips64el-linux-gnu",      "mips-mti-linux-gnu",
        "mips-img-linux-gnu",      "mips64el-linux-gnuabi64",
        "mipsisa64r6el-linux-gnu", "mipsisa64r6el-linux-gnuabi64",
    };

    const MIPSN32LibDirs: [1][]const u8 = .{"/lib32"};
    const MIPSN32Triples: [2][]const u8 = .{ "mips64-linux-gnuabin32", "mipsisa64r6-linux-gnuabin32" };
    const MIPSN32ELLibDirs: [1][]const u8 = .{"/lib32"};
    const MIPSN32ELTriples: [2][]const u8 = .{ "mips64el-linux-gnuabin32", "mipsisa64r6el-linux-gnuabin32" };

    const MSP430LibDirs: [1][]const u8 = .{"/lib"};
    const MSP430Triples: [1][]const u8 = .{"msp430-elf"};

    const PPCLibDirs: [2][]const u8 = .{ "/lib32", "/lib" };
    const PPCTriples: [5][]const u8 = .{
        "powerpc-linux-gnu",    "powerpc-unknown-linux-gnu",   "powerpc-linux-gnuspe",
        // On 32-bit PowerPC systems running SUSE Linux, gcc is configured as a
        // 64-bit compiler which defaults to "-m32", hence "powerpc64-suse-linux".
        "powerpc64-suse-linux", "powerpc-montavista-linuxspe",
    };
    const PPCLELibDirs: [2][]const u8 = .{ "/lib32", "/lib" };
    const PPCLETriples: [3][]const u8 = .{ "powerpcle-linux-gnu", "powerpcle-unknown-linux-gnu", "powerpcle-linux-musl" };

    const PPC64LibDirs: [2][]const u8 = .{ "/lib64", "/lib" };
    const PPC64Triples: [4][]const u8 = .{
        "powerpc64-linux-gnu",  "powerpc64-unknown-linux-gnu",
        "powerpc64-suse-linux", "ppc64-redhat-linux",
    };
    const PPC64LELibDirs: [2][]const u8 = .{ "/lib64", "/lib" };
    const PPC64LETriples: [5][]const u8 = .{
        "powerpc64le-linux-gnu",      "powerpc64le-unknown-linux-gnu",
        "powerpc64le-none-linux-gnu", "powerpc64le-suse-linux",
        "ppc64le-redhat-linux",
    };

    const RISCV32LibDirs: [2][]const u8 = .{ "/lib32", "/lib" };
    const RISCV32Triples: [3][]const u8 = .{ "riscv32-unknown-linux-gnu", "riscv32-linux-gnu", "riscv32-unknown-elf" };
    const RISCV64LibDirs: [2][]const u8 = .{ "/lib64", "/lib" };
    const RISCV64Triples: [3][]const u8 = .{
        "riscv64-unknown-linux-gnu",
        "riscv64-linux-gnu",
        "riscv64-unknown-elf",
    };

    const SPARCv8LibDirs: [2][]const u8 = .{ "/lib32", "/lib" };
    const SPARCv8Triples: [2][]const u8 = .{ "sparc-linux-gnu", "sparcv8-linux-gnu" };
    const SPARCv9LibDirs: [2][]const u8 = .{ "/lib64", "/lib" };
    const SPARCv9Triples: [2][]const u8 = .{ "sparc64-linux-gnu", "sparcv9-linux-gnu" };

    const SystemZLibDirs: [2][]const u8 = .{ "/lib64", "/lib" };
    const SystemZTriples: [5][]const u8 = .{
        "s390x-linux-gnu",  "s390x-unknown-linux-gnu", "s390x-ibm-linux-gnu",
        "s390x-suse-linux", "s390x-redhat-linux",
    };

    const target = tc.getTarget();
    if (target.os.tag == .solaris) {
        // TODO
        return;
    }

    if (target.abi.isAndroid()) {
        const AArch64AndroidTriples: [1][]const u8 = .{"aarch64-linux-android"};
        const ARMAndroidTriples: [1][]const u8 = .{"arm-linux-androideabi"};
        const X86AndroidTriples: [1][]const u8 = .{"i686-linux-android"};
        const X86_64AndroidTriples: [1][]const u8 = .{"x86_64-linux-android"};

        switch (target.cpu.arch) {
            .aarch64 => {
                libDirs.appendSliceAssumeCapacity(&AArch64LibDirs);
                tripleAliases.appendSliceAssumeCapacity(&AArch64AndroidTriples);
            },
            .arm,
            .thumb,
            => {
                libDirs.appendSliceAssumeCapacity(&ARMLibDirs);
                tripleAliases.appendSliceAssumeCapacity(&ARMAndroidTriples);
            },
            .mipsel => {},
            .mips64el => {},
            .x86_64 => {
                libDirs.appendSliceAssumeCapacity(&X86_64LibDirs);
                tripleAliases.appendSliceAssumeCapacity(&X86_64AndroidTriples);
                biarchLibDirs.appendSliceAssumeCapacity(&X86LibDirs);
                biarchTripleAliases.appendSliceAssumeCapacity(&X86AndroidTriples);
            },
            .x86 => {
                libDirs.appendSliceAssumeCapacity(&X86LibDirs);
                tripleAliases.appendSliceAssumeCapacity(&X86AndroidTriples);
                biarchLibDirs.appendSliceAssumeCapacity(&X86_64LibDirs);
                biarchTripleAliases.appendSliceAssumeCapacity(&X86_64AndroidTriples);
            },
            else => {},
        }
        return;
    }

    switch (target.cpu.arch) {
        .aarch64 => {
            libDirs.appendSliceAssumeCapacity(&AArch64LibDirs);
            tripleAliases.appendSliceAssumeCapacity(&AArch64Triples);
            biarchLibDirs.appendSliceAssumeCapacity(&AArch64LibDirs);
            biarchTripleAliases.appendSliceAssumeCapacity(&AArch64Triples);
        },
        .aarch64_be => {
            libDirs.appendSliceAssumeCapacity(&AArch64beLibDirs);
            tripleAliases.appendSliceAssumeCapacity(&AArch64beTriples);
            biarchLibDirs.appendSliceAssumeCapacity(&AArch64beLibDirs);
            biarchTripleAliases.appendSliceAssumeCapacity(&AArch64beTriples);
        },
        .arm, .thumb => {
            libDirs.appendSliceAssumeCapacity(&ARMLibDirs);
            if (target.abi == .gnueabihf)
                tripleAliases.appendSliceAssumeCapacity(&ARMHFTriples)
            else
                tripleAliases.appendSliceAssumeCapacity(&ARMTriples);
        },
        .armeb, .thumbeb => {
            libDirs.appendSliceAssumeCapacity(&ARMebLibDirs);
            if (target.abi == .gnueabihf)
                tripleAliases.appendSliceAssumeCapacity(&ARMebHFTriples)
            else
                tripleAliases.appendSliceAssumeCapacity(&ARMebTriples);
        },
        .avr => {
            libDirs.appendSliceAssumeCapacity(&AVRLibDirs);
            tripleAliases.appendSliceAssumeCapacity(&AVRTriples);
        },
        .csky => {
            libDirs.appendSliceAssumeCapacity(&CSKYLibDirs);
            tripleAliases.appendSliceAssumeCapacity(&CSKYTriples);
        },
        .x86_64 => {
            if (target.abi == .gnux32 or target.abi == .muslx32) {
                libDirs.appendSliceAssumeCapacity(&X32LibDirs);
                tripleAliases.appendSliceAssumeCapacity(&X32Triples);
                biarchLibDirs.appendSliceAssumeCapacity(&X86_64LibDirs);
                biarchTripleAliases.appendSliceAssumeCapacity(&X86_64Triples);
            } else {
                libDirs.appendSliceAssumeCapacity(&X86_64LibDirs);
                tripleAliases.appendSliceAssumeCapacity(&X86_64Triples);
                biarchLibDirs.appendSliceAssumeCapacity(&X32LibDirs);
                biarchTripleAliases.appendSliceAssumeCapacity(&X32Triples);
            }
            biarchLibDirs.appendSliceAssumeCapacity(&X86LibDirs);
            biarchTripleAliases.appendSliceAssumeCapacity(&X86Triples);
        },
        .x86 => {
            libDirs.appendSliceAssumeCapacity(&X86LibDirs);
            tripleAliases.appendSliceAssumeCapacity(&X86Triples);
            biarchLibDirs.appendSliceAssumeCapacity(&X86_64LibDirs);
            biarchTripleAliases.appendSliceAssumeCapacity(&X86_64Triples);
            biarchLibDirs.appendSliceAssumeCapacity(&X32LibDirs);
            biarchTripleAliases.appendSliceAssumeCapacity(&X32Triples);
        },
        .loongarch64 => {
            libDirs.appendSliceAssumeCapacity(&LoongArch64LibDirs);
            tripleAliases.appendSliceAssumeCapacity(&LoongArch64Triples);
        },
        .m68k => {
            libDirs.appendSliceAssumeCapacity(&M68kLibDirs);
            tripleAliases.appendSliceAssumeCapacity(&M68kTriples);
        },
        .mips => {
            libDirs.appendSliceAssumeCapacity(&MIPSLibDirs);
            tripleAliases.appendSliceAssumeCapacity(&MIPSTriples);

            biarchLibDirs.appendSliceAssumeCapacity(&MIPS64LibDirs);
            biarchLibDirs.appendSliceAssumeCapacity(&MIPSN32ELLibDirs);
            biarchTripleAliases.appendSliceAssumeCapacity(&MIPS64Triples);
            biarchTripleAliases.appendSliceAssumeCapacity(&MIPSN32ELTriples);
        },
        .mipsel => {
            libDirs.appendSliceAssumeCapacity(&MIPSELLibDirs);
            tripleAliases.appendSliceAssumeCapacity(&MIPSELTriples);
            tripleAliases.appendSliceAssumeCapacity(&MIPSTriples);

            biarchLibDirs.appendSliceAssumeCapacity(&MIPS64ELLibDirs);
            biarchLibDirs.appendSliceAssumeCapacity(&MIPSN32ELLibDirs);
            biarchTripleAliases.appendSliceAssumeCapacity(&MIPS64ELTriples);
            biarchTripleAliases.appendSliceAssumeCapacity(&MIPSN32ELTriples);
        },
        .mips64 => {
            libDirs.appendSliceAssumeCapacity(&MIPS64LibDirs);
            tripleAliases.appendSliceAssumeCapacity(&MIPS64Triples);

            biarchLibDirs.appendSliceAssumeCapacity(&MIPSLibDirs);
            biarchLibDirs.appendSliceAssumeCapacity(&MIPSN32LibDirs);
            biarchTripleAliases.appendSliceAssumeCapacity(&MIPSTriples);
            biarchTripleAliases.appendSliceAssumeCapacity(&MIPSN32Triples);
        },
        .mips64el => {
            libDirs.appendSliceAssumeCapacity(&MIPS64ELLibDirs);
            tripleAliases.appendSliceAssumeCapacity(&MIPS64ELTriples);

            biarchLibDirs.appendSliceAssumeCapacity(&MIPSELLibDirs);
            biarchLibDirs.appendSliceAssumeCapacity(&MIPSN32ELLibDirs);
            biarchTripleAliases.appendSliceAssumeCapacity(&MIPSELTriples);
            biarchTripleAliases.appendSliceAssumeCapacity(&MIPSN32ELTriples);
            biarchTripleAliases.appendSliceAssumeCapacity(&MIPSTriples);
        },
        .msp430 => {
            libDirs.appendSliceAssumeCapacity(&MSP430LibDirs);
            tripleAliases.appendSliceAssumeCapacity(&MSP430Triples);
        },
        .powerpc => {
            libDirs.appendSliceAssumeCapacity(&PPCLibDirs);
            tripleAliases.appendSliceAssumeCapacity(&PPCTriples);
            biarchLibDirs.appendSliceAssumeCapacity(&PPC64LibDirs);
            biarchTripleAliases.appendSliceAssumeCapacity(&PPC64Triples);
        },
        .powerpcle => {
            libDirs.appendSliceAssumeCapacity(&PPCLELibDirs);
            tripleAliases.appendSliceAssumeCapacity(&PPCLETriples);
            biarchLibDirs.appendSliceAssumeCapacity(&PPC64LELibDirs);
            biarchTripleAliases.appendSliceAssumeCapacity(&PPC64LETriples);
        },
        .powerpc64 => {
            libDirs.appendSliceAssumeCapacity(&PPC64LibDirs);
            tripleAliases.appendSliceAssumeCapacity(&PPC64Triples);
            biarchLibDirs.appendSliceAssumeCapacity(&PPCLibDirs);
            biarchTripleAliases.appendSliceAssumeCapacity(&PPCTriples);
        },
        .powerpc64le => {
            libDirs.appendSliceAssumeCapacity(&PPC64LELibDirs);
            tripleAliases.appendSliceAssumeCapacity(&PPC64LETriples);
            biarchLibDirs.appendSliceAssumeCapacity(&PPCLELibDirs);
            biarchTripleAliases.appendSliceAssumeCapacity(&PPCLETriples);
        },
        .riscv32 => {
            libDirs.appendSliceAssumeCapacity(&RISCV32LibDirs);
            tripleAliases.appendSliceAssumeCapacity(&RISCV32Triples);
            biarchLibDirs.appendSliceAssumeCapacity(&RISCV64LibDirs);
            biarchTripleAliases.appendSliceAssumeCapacity(&RISCV64Triples);
        },
        .riscv64 => {
            libDirs.appendSliceAssumeCapacity(&RISCV64LibDirs);
            tripleAliases.appendSliceAssumeCapacity(&RISCV64Triples);
            biarchLibDirs.appendSliceAssumeCapacity(&RISCV32LibDirs);
            biarchTripleAliases.appendSliceAssumeCapacity(&RISCV32Triples);
        },
        .sparc => {
            libDirs.appendSliceAssumeCapacity(&SPARCv8LibDirs);
            tripleAliases.appendSliceAssumeCapacity(&SPARCv8Triples);
            biarchLibDirs.appendSliceAssumeCapacity(&SPARCv9LibDirs);
            biarchTripleAliases.appendSliceAssumeCapacity(&SPARCv9Triples);
        },
        .sparc64 => {
            libDirs.appendSliceAssumeCapacity(&SPARCv9LibDirs);
            tripleAliases.appendSliceAssumeCapacity(&SPARCv9Triples);
            biarchLibDirs.appendSliceAssumeCapacity(&SPARCv8LibDirs);
            biarchTripleAliases.appendSliceAssumeCapacity(&SPARCv8Triples);
        },
        .s390x => {
            libDirs.appendSliceAssumeCapacity(&SystemZLibDirs);
            tripleAliases.appendSliceAssumeCapacity(&SystemZTriples);
        },
        else => {},
    }
}

pub fn discover(self: *GCCDetector, tc: *Toolchain) !void {
    var pathBuffer: [std.fs.max_path_bytes]u8 = undefined;
    var fib = std.heap.FixedBufferAllocator.init(&pathBuffer);

    const target = tc.getTarget();
    const biVariantTarget = if (target.ptrBitWidth() == 32)
        TargetUtil.get64BitArchVariant(target)
    else
        TargetUtil.get32BitArchVariant(target);

    var candidateLibDirsBuffer: [16][]const u8 = undefined;
    var candidateLibDirs = std.ArrayListUnmanaged([]const u8).initBuffer(&candidateLibDirsBuffer);

    var candidateTripleAliasesBuffer: [16][]const u8 = undefined;
    var candidateTripleAliases = std.ArrayListUnmanaged([]const u8).initBuffer(&candidateTripleAliasesBuffer);

    var candidateBiarchLibDirsBuffer: [16][]const u8 = undefined;
    var candidateBiarchLibDirs = std.ArrayListUnmanaged([]const u8).initBuffer(&candidateBiarchLibDirsBuffer);

    var candidateBiarchTripleAliasesBuffer: [20][]const u8 = undefined;
    var candidateBiarchTripleAliases = std.ArrayListUnmanaged([]const u8).initBuffer(&candidateBiarchTripleAliasesBuffer);

    try collectLibDirsAndTriples(
        tc,
        &candidateLibDirs,
        &candidateTripleAliases,
        &candidateBiarchLibDirs,
        &candidateBiarchTripleAliases,
    );

    var targetBuffer: [64]u8 = undefined;
    const tripleStr = TargetUtil.toLLVMTriple(target, &targetBuffer);
    candidateTripleAliases.appendAssumeCapacity(tripleStr);

    //   // Also include the multiarch variant if it's different.
    var biarchBuffer: [64]u8 = undefined;
    if (biVariantTarget) |biarchTarget| {
        const biarchTripleStr = TargetUtil.toLLVMTriple(biarchTarget, &biarchBuffer);
        if (!std.mem.eql(u8, biarchTripleStr, tripleStr))
            candidateTripleAliases.appendAssumeCapacity(biarchTripleStr);
    }

    var prefixesBuffer: [16][]const u8 = undefined;
    var prefixes = std.ArrayListUnmanaged([]const u8).initBuffer(&prefixesBuffer);
    const gccToolchainDir = getGccToolchainDir(tc);
    if (gccToolchainDir.len != 0) {
        const adjusted = if (gccToolchainDir[gccToolchainDir.len - 1] == '/')
            gccToolchainDir[0 .. gccToolchainDir.len - 1]
        else
            gccToolchainDir;
        prefixes.appendAssumeCapacity(adjusted);
    } else {
        const sysroot = tc.getSysroot();
        if (sysroot.len > 0) {
            prefixes.appendAssumeCapacity(sysroot);
            try addDefaultGCCPrefixes(&prefixes, tc);
        }

        if (sysroot.len == 0)
            try addDefaultGCCPrefixes(&prefixes, tc);

        // TODO: Special-case handling for Gentoo
    }

    const v0 = GCCVersion.parse("0.0.0");
    for (prefixes.items) |prefix| {
        if (!tc.filesystem.exists(prefix)) continue;

        for (candidateLibDirs.items) |suffix| {
            defer fib.reset();
            const libDir = std.fs.path.join(fib.allocator(), &.{ prefix, suffix }) catch continue;
            if (!tc.filesystem.exists(libDir)) continue;

            const gccDirExists = tc.filesystem.joinedExists(&.{ libDir, "/gcc" });
            const gccCrossDirExists = tc.filesystem.joinedExists(&.{ libDir, "/gcc-cross" });

            try self.scanLibDirForGCCTriple(tc, target, libDir, tripleStr, false, gccDirExists, gccCrossDirExists);
            for (candidateTripleAliases.items) |candidate| {
                try self.scanLibDirForGCCTriple(tc, target, libDir, candidate, false, gccDirExists, gccCrossDirExists);
            }
        }

        for (candidateBiarchLibDirs.items) |suffix| {
            const libDir = std.fs.path.join(fib.allocator(), &.{ prefix, suffix }) catch continue;
            if (!tc.filesystem.exists(libDir)) continue;

            const gccDirExists = tc.filesystem.joinedExists(&.{ libDir, "/gcc" });
            const gccCrossDirExists = tc.filesystem.joinedExists(&.{ libDir, "/gcc-cross" });

            for (candidateBiarchTripleAliases.items) |candidate| {
                try self.scanLibDirForGCCTriple(tc, target, libDir, candidate, true, gccDirExists, gccCrossDirExists);
            }
        }

        if (self.version.order(v0) == .gt)
            break;
    }
}

fn findBiarchMultilibs(
    tc: *const Toolchain,
    result: *Multilib.Detected,
    target: std.Target,
    path: [2][]const u8,
    needsBiArchSuffix: bool,
) !bool {
    const suff64 = if (target.os.tag == .solaris)
        switch (target.cpu.arch) {
            .x86, .x86_64 => "/amd64",
            .sparc => "/sparcv9",
            else => "/64",
        }
    else
        "/64";

    const alt64 = Multilib.init(suff64, suff64, &.{ "-m32", "+m64", "-mx32" });
    const alt32 = Multilib.init("/32", "/32", &.{ "+m32", "-m64", "-mx32" });
    const altX32 = Multilib.init("/x32", "/x32", &.{ "-m32", "-m64", "+mx32" });

    const multilibFilter = Multilib.Filter{
        .base = path,
        .file = "crtbegin.o",
    };

    const Want = enum {
        want32,
        want64,
        wantx32,
    };
    const isX32 = target.abi == .gnux32 or target.abi == .muslx32;
    const targetPtrWidth = target.ptrBitWidth();
    const want: Want = if (targetPtrWidth == 32 and multilibFilter.exists(alt32, tc.filesystem))
        .want64
    else if (targetPtrWidth == 64 and isX32 and multilibFilter.exists(altX32, tc.filesystem))
        .want64
    else if (targetPtrWidth == 64 and !isX32 and multilibFilter.exists(alt64, tc.filesystem))
        .want32
    else if (targetPtrWidth == 32)
        if (needsBiArchSuffix) .want64 else .want32
    else if (isX32)
        if (needsBiArchSuffix) .want64 else .wantx32
    else if (needsBiArchSuffix) .want32 else .want64;

    const default = switch (want) {
        .want32 => Multilib.init("", "", &.{ "+m32", "-m64", "-mx32" }),
        .want64 => Multilib.init("", "", &.{ "-m32", "+m64", "-mx32" }),
        .wantx32 => Multilib.init("", "", &.{ "-m32", "-m64", "+mx32" }),
    };

    result.multilibBuffer[result.multilibCount..][0..4].* = .{
        default,
        alt64,
        alt32,
        altX32,
    };
    result.multilibCount += 4;
    result.filter(multilibFilter, tc.filesystem);

    return result.select(&.{
        if (targetPtrWidth == 64 and !isX32) "+m64" else "-m64",
        if (targetPtrWidth == 32) "+m32" else "-m32",
        if (targetPtrWidth == 64 and isX32) "+mx32" else "-mx32",
    });
}

fn scanGCCForMultilibs(
    self: *GCCDetector,
    tc: *const Toolchain,
    target: std.Target,
    path: [2][]const u8,
    needsBiarchSuffix: bool,
) !bool {
    var detected: Multilib.Detected = .{};
    if (target.cpu.arch == .csky) {
        // TODO
    } else if (target.cpu.arch.isMIPS()) {
        // TODO
    } else if (target.cpu.arch.isRISCV()) {
        // TODO
    } else if (target.cpu.arch == .msp430) {
        // TODO
    } else if (target.cpu.arch == .avr) {
        // No multilibs
    } else if (!try findBiarchMultilibs(tc, &detected, target, path, needsBiarchSuffix)) {
        return false;
    }
    self.selected = detected.selected;
    self.biarchSibling = detected.biarchSibling;
    return true;
}

fn scanLibDirForGCCTriple(
    self: *GCCDetector,
    tc: *const Toolchain,
    target: std.Target,
    libDir: []const u8,
    candidateTriple: []const u8,
    needsBiarchSuffix: bool,
    gccDirExists: bool,
    gccCrossDirExists: bool,
) !void {
    var pathBuffer: [std.fs.max_path_bytes]u8 = undefined;
    var fib = std.heap.FixedBufferAllocator.init(&pathBuffer);
    const arena = tc.driver.comp.arena;

    for (0..2) |i| {
        if (i == 0 and !gccDirExists) continue;
        if (i == 1 and !gccCrossDirExists) continue;
        defer fib.reset();

        const base: []const u8 = if (i == 0) "gcc" else "gcc-cross";

        var libSuffixBuffer: [64]u8 = undefined;
        var suffixBufferLib = std.heap.FixedBufferAllocator.init(&libSuffixBuffer);

        const libSuffix = std.fs.path.join(suffixBufferLib.allocator(), &.{ base, candidateTriple }) catch continue;
        const dirname = std.fs.path.join(fib.allocator(), &.{ libDir, libSuffix }) catch continue;

        var parentDir = tc.filesystem.openDir(dirname) catch continue;
        defer parentDir.close();

        var it = parentDir.iterate();
        while (it.next() catch continue) |entry| {
            if (entry.kind != .directory) continue;

            const versionText = entry.name;
            const candidateVersion = GCCVersion.parse(versionText);
            if (candidateVersion.major != -1) {
                // TODO: cache path so we're not repeatedly scanning
            }

            if (candidateVersion.isLessThan(4, 1, 1, ""))
                continue;

            switch (candidateVersion.order(self.version)) {
                .lt, .eq => continue,
                .gt => {},
            }

            if (!try self.scanGCCForMultilibs(tc, target, .{ dirname, versionText }, needsBiarchSuffix)) continue;

            self.version = candidateVersion;
            self.gccTriple = try arena.dupe(u8, candidateTriple);
            self.installPath = try std.fs.path.join(arena, &.{ libDir, libSuffix, versionText });
            self.parentLibPath = try std.fs.path.join(arena, &.{ self.installPath, "..", "..", ".." });
            self.isValid = true;
        }
    }
}

fn getGccToolchainDir(tc: *const Toolchain) []const u8 {
    const sysroot = tc.getSysroot();
    if (sysroot.len != 0) return "";
    return SystemDefaults.gccInstallPrefix;
}
