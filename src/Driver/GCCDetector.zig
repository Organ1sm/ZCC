const std = @import("std");
const Driver = @import("../Driver.zig");
const Toolchain = @import("../Toolchain.zig");
const TargetUtil = @import("../Basic/Target.zig");
const SystemDefaults = @import("system-defaults");
const GCCVersion = @import("GCCVersion.zig");
const Multilib = @import("Multilib.zig");
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

fn addDefaultGCCPrefixes(prefixes: *PathPrefixes, tc: *const Toolchain) !void {
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
        var usrPath = try tc.arena.alloc(u8, 4 + sysroot.len);
        @memcpy(usrPath[0..4], "/usr");
        @memcpy(usrPath[4..], sysroot);
        prefixes.appendAssumeCapacity(usrPath);
    }
}

const PathPrefixes = std.BoundedArray([]const u8, 16);

fn collectLibDirsAndTriples(
    self: *GCCDetector,
    tc: *Toolchain,
    libDirs: *PathPrefixes,
    tripleAliases: *PathPrefixes,
    biarchLibDirs: *PathPrefixes,
    biarchTripleAliases: *PathPrefixes,
) !void {
    _ = self;

    const AArch64LibDirs: [2][]const u8 = .{ "/lib64", "/lib" };
    const AArch64Triples: [4][]const u8 = .{ "aarch64-none-linux-gnu", "aarch64-linux-gnu", "aarch64-redhat-linux", "aarch64-suse-linux" };
    const AArch64beLibDirs: [1][]const u8 = .{"/lib"};
    const AArch64beTriples: [2][]const u8 = .{ "aarch64_be-none-linux-gnu", "aarch64_be-linux-gnu" };

    const ARMLibDirs: [1][]const u8 = .{"/lib"};
    const ARMTriples: [1][]const u8 = .{"arm-linux-gnueabi"};
    const ARMHFTriples: [4][]const u8 = .{ "arm-linux-gnueabihf", "armv7hl-redhat-linux-gnueabi", "armv6hl-suse-linux-gnueabi", "armv7hl-suse-linux-gnueabi" };

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

    const RISCV32LibDirs: [2][]const u8 = .{ "/lib32", "/lib" };
    _ = RISCV32LibDirs;
    const RISCV32Triples: [3][]const u8 = .{ "riscv32-unknown-linux-gnu", "riscv32-linux-gnu", "riscv32-unknown-elf" };
    _ = RISCV32Triples;
    const RISCV64LibDirs: [2][]const u8 = .{ "/lib64", "/lib" };
    _ = RISCV64LibDirs;
    const RISCV64Triples: [3][]const u8 = .{
        "riscv64-unknown-linux-gnu",
        "riscv64-linux-gnu",
        "riscv64-unknown-elf",
    };
    _ = RISCV64Triples;

    const SystemZLibDirs: [2][]const u8 = .{ "/lib64", "/lib" };
    _ = SystemZLibDirs;
    const SystemZTriples: [5][]const u8 = .{
        "s390x-linux-gnu",  "s390x-unknown-linux-gnu", "s390x-ibm-linux-gnu",
        "s390x-suse-linux", "s390x-redhat-linux",
    };
    _ = SystemZTriples;

    const target = tc.getTarget();
    if (target.os.tag == .solaris) {
        // TODO
        return;
    }

    if (target.isAndroid()) {
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
        .avr => @panic("TODO"),
        .csky => @panic("TODO"),
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
            // MCU toolchain is 32 bit only and its triple alias is TargetTriple
            // itself, which will be appended below.
            if (target.os.tag != .elfiamcu) {
                tripleAliases.appendSliceAssumeCapacity(&X86Triples);
                biarchLibDirs.appendSliceAssumeCapacity(&X86_64LibDirs);
                biarchTripleAliases.appendSliceAssumeCapacity(&X86_64Triples);
                biarchLibDirs.appendSliceAssumeCapacity(&X32LibDirs);
                biarchTripleAliases.appendSliceAssumeCapacity(&X32Triples);
            }
        },
        .loongarch64 => @panic("TODO"),
        .m68k => @panic("TODO"),
        .mips => @panic("TODO"),
        .mipsel => @panic("TODO"),
        .mips64 => @panic("TODO"),
        .mips64el => @panic("TODO"),
        .msp430 => @panic("TODO"),
        .powerpc => @panic("TODO"),
        .powerpcle => @panic("TODO"),
        .powerpc64 => @panic("TODO"),
        .powerpc64le => @panic("TODO"),
        .riscv32 => @panic("TODO"),
        .riscv64 => @panic("TODO"),
        .sparc => @panic("TODO"),
        .sparc64 => @panic("TODO"),
        .s390x => @panic("TODO"),
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
    _ = biVariantTarget;

    var candidateLibDirs: PathPrefixes = .{};
    var candidateBiarchLibDirs: PathPrefixes = .{};
    var candidateTripleAliases: PathPrefixes = .{};
    var candidateBiarchTripleAliases: PathPrefixes = .{};

    try self.collectLibDirsAndTriples(
        tc,
        &candidateLibDirs,
        &candidateTripleAliases,
        &candidateBiarchLibDirs,
        &candidateBiarchTripleAliases,
    );

    var targetBuffer: std.BoundedArray(u8, 32) = .{};
    try TargetUtil.toLLVMTriple(targetBuffer.writer(), target);
    const tripleStr = targetBuffer.constSlice();
    candidateTripleAliases.appendAssumeCapacity(tripleStr);

    //   // Also include the multiarch variant if it's different.
    //   if (TargetTriple.str() != BiarchTriple.str())
    //     BiarchTripleAliases.push_back(BiarchTriple.str());

    var prefixes: PathPrefixes = .{};
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
    for (prefixes.constSlice()) |prefix| {
        if (!tc.filesystem.exists(prefix)) continue;

        for (candidateLibDirs.constSlice()) |suffix| {
            defer fib.reset();
            const libDir = std.fs.path.join(fib.allocator(), &.{ prefix, suffix }) catch continue;
            if (!tc.filesystem.exists(libDir)) continue;

            const gccDirExists = tc.filesystem.joinedExists(&.{ libDir, "/gcc" });
            const gccCrossDirExists = tc.filesystem.joinedExists(&.{ libDir, "/gcc-cross" });

            try self.scanLibDirForGCCTriple(tc, target, libDir, tripleStr, false, gccDirExists, gccCrossDirExists);
            for (candidateTripleAliases.constSlice()) |candidate| {
                try self.scanLibDirForGCCTriple(tc, target, libDir, candidate, false, gccDirExists, gccCrossDirExists);
            }
        }

        for (candidateBiarchLibDirs.constSlice()) |suffix| {
            const libDir = std.fs.path.join(fib.allocator(), &.{ prefix, suffix }) catch continue;
            if (!tc.filesystem.exists(libDir)) continue;

            const gccDirExists = tc.filesystem.joinedExists(&.{ libDir, "/gcc" });
            const gccCrossDirExists = tc.filesystem.joinedExists(&.{ libDir, "/gcc-cross" });

            for (candidateBiarchTripleAliases.constSlice()) |candidate| {
                try self.scanLibDirForGCCTriple(tc, target, libDir, candidate, true, gccDirExists, gccCrossDirExists);
            }
        }

        if (self.version.order(v0) == .gt)
            break;
    }
}

fn findBiarchMultilibs(
    self: *GCCDetector,
    tc: *const Toolchain,
    result: *Multilib.Detected,
    target: std.Target,
    path: [2][]const u8,
    needsBiArchSuffix: bool,
) !bool {
    const suff64 = if (target.os.tag == .solaris) switch (target.cpu.arch) {
        .x86, .x86_64 => "/amd64",
        .sparc => "/sparcv9",
        else => "/64",
    } else "/64";

    _ = self;
    const alt64 = Multilib.init(suff64, suff64, &.{ "-m32", "+m64", "-mx32" });
    const alt32 = Multilib.init("/32", "/32", &.{ "+m32", "-m64", "-mx32" });
    const altX32 = Multilib.init("/x32", "/x32", &.{ "-m32", "-m64", "+mx32" });

    const multilibFilter = Multilib.Filter{
        .base = path,
        .file = if (target.os.tag == .elfiamcu) "libgcc.a" else "crtbegin.o",
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
    result.multilibs.appendSliceAssumeCapacity(&.{
        default,
        alt64,
        alt32,
        altX32,
    });
    result.filter(multilibFilter, tc.filesystem);

    var flags: Multilib.Flags = .{};
    flags.appendAssumeCapacity(if (targetPtrWidth == 64 and !isX32) "+m64" else "-m64");
    flags.appendAssumeCapacity(if (targetPtrWidth == 32) "+m32" else "-m32");
    flags.appendAssumeCapacity(if (targetPtrWidth == 64 and isX32) "+mx32" else "-mx32");

    const success = result.select(flags);
    if (!success) return false;

    return true;
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
    } else if (!try self.findBiarchMultilibs(tc, &detected, target, path, needsBiarchSuffix)) {
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
    var path_buf: [std.fs.max_path_bytes]u8 = undefined;
    var fib = std.heap.FixedBufferAllocator.init(&path_buf);
    for (0..2) |i| {
        if (i == 0 and !gccDirExists) continue;
        if (i == 1 and !gccCrossDirExists) continue;
        defer fib.reset();

        const base: []const u8 = if (i == 0) "gcc" else "gcc-cross";

        var libSuffixBuffer: std.BoundedArray(u8, 64) = .{};
        libSuffixBuffer.appendSliceAssumeCapacity(base);
        libSuffixBuffer.appendAssumeCapacity(std.fs.path.sep);
        libSuffixBuffer.appendSliceAssumeCapacity(candidateTriple);

        const libSuffix = libSuffixBuffer.constSlice();
        const dirname = std.fs.path.join(fib.allocator(), &.{ libDir, libSuffix }) catch continue;
        var parentDir = std.fs.cwd().openDir(dirname, .{ .access_sub_paths = false, .iterate = true }) catch continue;
        defer parentDir.close();

        var it = parentDir.iterate();
        while (try it.next()) |entry| {
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
            self.gccTriple = try tc.arena.dupe(u8, candidateTriple);
            self.installPath = try std.fs.path.join(tc.arena, &.{ libDir, libSuffix, versionText });
            self.parentLibPath = try std.fs.path.join(tc.arena, &.{ self.installPath, "..", "..", ".." });
            self.isValid = true;
        }
    }
}

fn getGccToolchainDir(tc: *const Toolchain) []const u8 {
    const sysroot = tc.getSysroot();
    if (sysroot.len != 0) return "";
    return SystemDefaults.gccInstallPrefix;
}
