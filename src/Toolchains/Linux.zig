const std = @import("std");
const mem = std.mem;
const Compilation = @import("../Basic/Compilation.zig");
const Toolchain = @import("../Toolchain.zig");
const Driver = @import("../Driver.zig");
const Distro = @import("../Driver/Distro.zig");
const GCCDetector = @import("../Driver/GCCDetector.zig");
const TargetUtil = @import("../Basic/Target.zig");
const SystemDefaults = @import("system-defaults");

const Linux = @This();

distro: Distro.Tag = .unknown,
extraOpts: std.ArrayListUnmanaged([]const u8) = .{},
gccDetector: GCCDetector = .{},

pub fn discover(self: *Linux, tc: *Toolchain) !void {
    self.distro = Distro.detect(tc.getTarget(), tc.filesystem);
    try self.gccDetector.discover(tc);
    tc.selectedMultilib = self.gccDetector.selected;

    try self.gccDetector.appendToolPath(tc);
    try self.buildExtraOpts(tc);
    try self.findPaths(tc);
}

fn buildExtraOpts(self: *Linux, tc: *const Toolchain) !void {
    const gpa = tc.driver.comp.gpa;
    const target = tc.getTarget();
    const isAndroid = target.isAndroid();

    if (self.distro.isAlpine() or isAndroid) {
        try self.extraOpts.ensureUnusedCapacity(gpa, 2);
        self.extraOpts.appendAssumeCapacity("-z");
        self.extraOpts.appendAssumeCapacity("now");
    }

    if (self.distro.isOpenSUSE() or self.distro.isUbuntu() or self.distro.isAlpine() or isAndroid) {
        try self.extraOpts.ensureUnusedCapacity(gpa, 2);
        self.extraOpts.appendAssumeCapacity("-z");
        self.extraOpts.appendAssumeCapacity("relro");
    }

    if (target.cpu.arch.isARM() or target.cpu.arch.isAARCH64() or isAndroid) {
        try self.extraOpts.ensureUnusedCapacity(gpa, 2);
        self.extraOpts.appendAssumeCapacity("-z");
        self.extraOpts.appendAssumeCapacity("max-page-size=4096");
    }

    if (target.cpu.arch == .arm or target.cpu.arch == .thumb)
        try self.extraOpts.append(gpa, "-X");

    if (!target.cpu.arch.isMIPS() and target.cpu.arch != .hexagon) {
        const hash_style = if (isAndroid) .both else self.distro.getHashStyle();
        try self.extraOpts.append(gpa, switch (hash_style) {
            inline else => |tag| "--hash-style=" ++ @tagName(tag),
        });
    }

    if (SystemDefaults.enableLinkerBuildId)
        try self.extraOpts.append(gpa, "--build-id");
}

fn addMultiLibPaths(self: *Linux, tc: *Toolchain, sysroot: []const u8, osLibDir: []const u8) !void {
    if (!self.gccDetector.isValid) return;
    const gccTriple = self.gccDetector.gccTriple;
    const libPath = self.gccDetector.parentLibPath;

    // Add lib/gcc/$triple/$version, with an optional /multilib suffix.
    try tc.addPathIfExists(&.{ self.gccDetector.installPath, tc.selectedMultilib.gccSuffix }, .file);

    // Add lib/gcc/$triple/$libdir
    // For GCC built with --enable-version-specific-runtime-libs.
    try tc.addPathIfExists(&.{ self.gccDetector.installPath, "..", osLibDir }, .file);

    try tc.addPathIfExists(&.{ libPath, "..", gccTriple, "lib", "..", osLibDir, tc.selectedMultilib.osSuffix }, .file);

    // If the GCC installation we found is inside of the sysroot, we want to
    // prefer libraries installed in the parent prefix of the GCC installation.
    // It is important to *not* use these paths when the GCC installation is
    // outside of the system root as that can pick up unintended libraries.
    // This usually happens when there is an external cross compiler on the
    // host system, and a more minimal sysroot available that is the target of
    // the cross. Note that GCC does include some of these directories in some
    // configurations but this seems somewhere between questionable and simply
    // a bug.
    if (mem.startsWith(u8, libPath, sysroot))
        try tc.addPathIfExists(&.{ libPath, "..", osLibDir }, .file);
}

fn addMultiArchPaths(self: *Linux, tc: *Toolchain) !void {
    if (!self.gccDetector.isValid) return;
    const libPath = self.gccDetector.parentLibPath;
    const gccTriple = self.gccDetector.gccTriple;
    const multilib = self.gccDetector.selected;
    try tc.addPathIfExists(&.{ libPath, "..", gccTriple, "lib", multilib.osSuffix }, .file);
}

/// TODO: Very incomplete
fn findPaths(self: *Linux, tc: *Toolchain) !void {
    const target = tc.getTarget();
    const sysroot = tc.getSysroot();

    const osLibDir = getOSLibDir(target);
    const multiarchTriple = getMultiarchTriple(target);

    try self.addMultiLibPaths(tc, sysroot, osLibDir);

    try tc.addPathIfExists(&.{ sysroot, "/lib", multiarchTriple }, .file);
    try tc.addPathIfExists(&.{ sysroot, "/lib", "..", osLibDir }, .file);

    if (target.isAndroid()) {
        // TODO
    }

    try tc.addPathIfExists(&.{ sysroot, "/usr", "lib", multiarchTriple }, .file);
    try tc.addPathIfExists(&.{ sysroot, "/usr", "lib", "..", osLibDir }, .file);
    try tc.addPathIfExists(&.{ sysroot, "/lib" }, .file);
    try tc.addPathIfExists(&.{ sysroot, "/usr", "lib" }, .file);
}

pub fn deinit(self: *Linux, allocator: std.mem.Allocator) void {
    self.extraOpts.deinit(allocator);
}

fn isPIEDefault(self: *const Linux) bool {
    _ = self;
    return false;
}

fn getPIE(self: *const Linux, d: *const Driver) bool {
    if (d.shared or d.static or d.relocatable or d.staticPie)
        return false;
    return d.pie orelse self.isPIEDefault();
}

fn getStaticPIE(self: *const Linux, d: *Driver) !bool {
    _ = self;
    if (d.staticPie and d.pie != null)
        try d.err("cannot specify 'nopie' along with 'static-pie'");
    return d.staticPie;
}

fn getStatic(self: *const Linux, d: *const Driver) bool {
    _ = self;
    return d.static and !d.staticPie;
}

pub fn getDefaultLinker(self: *const Linux, target: std.Target) []const u8 {
    _ = self;
    if (target.isAndroid())
        return "ld.lld";
    return "ld";
}

pub fn buildLinkerArgs(self: *const Linux, tc: *const Toolchain, argv: *std.ArrayList([]const u8)) Compilation.Error!void {
    const d = tc.driver;
    const target = tc.getTarget();

    const isPie = self.getPIE(d);
    const isStaticPie = try self.getStaticPIE(d);
    const isStatic = self.getStatic(d);
    const isAndroid = target.isAndroid();
    const isIamcu = target.os.tag == .elfiamcu;

    if (isPie) try argv.append("-pie");

    if (isStaticPie)
        try argv.appendSlice(&.{ "-static", "-pie", "--no-dynamic-linker", "-z", "text" });

    if (d.rdynamic) try argv.append("-export-dynamic");
    if (d.strip) try argv.append("-s");

    try argv.appendSlice(self.extraOpts.items);
    try argv.append("--eh-frame-hdr");

    // Todo: Driver should parse `-EL`/`-EB` for arm to set endianness for arm targets
    if (TargetUtil.ldEmulationOption(target, null)) |emulation| {
        try argv.appendSlice(&.{ "-m", emulation });
    } else {
        try d.err("Unknown target triple");
        return;
    }

    if (target.cpu.arch.isRISCV())
        try argv.append("-X");

    if (d.shared)
        try argv.append("-shared");

    if (isStatic) {
        try argv.append("-static");
    } else {
        if (d.rdynamic)
            try argv.append("-export-dynamic");

        if (!d.shared and !isStaticPie and !d.relocatable) {
            const dynamicLinker = target.standardDynamicLinkerPath();
            // todo: check for --dyld-prefix
            if (dynamicLinker.get()) |path| {
                try argv.appendSlice(&.{ "-dynamic-linker", try tc.arena.dupe(u8, path) });
            } else {
                try d.err("Could not find dynamic linker path");
            }
        }
    }

    try argv.appendSlice(&.{ "-o", d.outputName orelse "a.out" });

    if (!d.nostdlib and !d.nostartfiles and !d.relocatable) {
        if (!isAndroid and !isIamcu) {
            if (!d.shared) {
                const crt1 = if (isPie)
                    "Scrt1.o"
                else if (isStaticPie)
                    "rcrt1.o"
                else
                    "crt1.o";
                try argv.append(try tc.getFilePath(crt1));
            }
            try argv.append(try tc.getFilePath("crti.o"));
        }
    }

    // TODO add -L opts
    // TODO add -u opts

    try tc.addFilePathLibArgs(argv);

    // TODO handle LTO

    try argv.appendSlice(d.linkObjects.items);

    if (!d.nostdlib and !d.relocatable) {
        if (!d.nodefaultlibs) {
            if (isStatic or isStaticPie)
                try argv.append("--start-group");

            // TODO: add pthread if needed
            if (!d.nolibc)
                try argv.append("-lc");

            if (isIamcu)
                try argv.append("-lgloss");

            if (isStatic or isStaticPie)
                try argv.append("--end-group");

            if (isIamcu) {
                try argv.ensureUnusedCapacity(3);
                argv.appendAssumeCapacity("--as-needed");
                argv.appendAssumeCapacity("-lsoftfp");
                argv.appendAssumeCapacity("--no-as-needed");
            }
        }

        if (!d.nostartfiles and !isIamcu) {
            // TODO: handle CRT begin/end files
            if (!isAndroid)
                try argv.append(try tc.getFilePath("crtn.o"));
        }
    }

    // TODO add -T args
}

fn getMultiarchTriple(target: std.Target) []const u8 {
    const isAndroid = target.isAndroid();

    return switch (target.cpu.arch) {
        .aarch64 => if (isAndroid) "aarch64-linux-android" else "aarch64-linux-gnu",
        .aarch64_be => "aarch64_be-linux-gnu",
        .x86 => if (isAndroid) "i686-linux-android" else "i386-linux-gnu",
        .x86_64 => if (isAndroid) "x86_64-linux-android" else if (target.abi == .gnux32) "x86_64-linux-gnux32" else "x86_64-linux-gnu",

        // TODO: expand this
        else => "",
    };
}

fn getOSLibDir(target: std.Target) []const u8 {
    switch (target.cpu.arch) {
        .x86,
        .powerpc,
        .powerpcle,
        .sparc,
        => return "lib32",
        else => {},
    }

    if (target.cpu.arch == .x86_64 and (target.abi == .gnux32 or target.abi == .muslx32))
        return "libx32";

    if (target.cpu.arch == .riscv32)
        return "lib32";

    if (target.ptrBitWidth() == 32)
        return "lib";

    return "lib64";
}
