const std = @import("std");
const mem = std.mem;

const Compilation = @import("../Basic/Compilation.zig");
const Distro = @import("../Driver/Distro.zig");
const Driver = @import("../Driver.zig");
const GCCDetector = @import("../Driver/GCCDetector.zig");
const SystemDefaults = @import("system-defaults");
const TargetUtil = @import("../Basic/Target.zig");
const Toolchain = @import("../Toolchain.zig");

const Linux = @This();

distro: Distro.Tag = .unknown,
extraOpts: std.ArrayList([]const u8) = .empty,
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
    const isAndroid = target.abi.isAndroid();

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

    if ((target.cpu.arch.isArm() and !target.cpu.arch.isThumb()) or target.cpu.arch.isAARCH64() or isAndroid) {
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

    var output: [64]u8 = undefined;
    const osLibDir = getOSLibDir(target);
    const multiarchTriple = getMultiarchTriple(target) orelse TargetUtil.toLLVMTriple(target, &output);

    try self.addMultiLibPaths(tc, sysroot, osLibDir);

    try tc.addPathIfExists(&.{ sysroot, "/lib", multiarchTriple }, .file);
    try tc.addPathIfExists(&.{ sysroot, "/lib", "..", osLibDir }, .file);

    if (target.abi.isAndroid()) {
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
        try d.err("cannot specify 'nopie' along with 'static-pie'", .{});
    return d.staticPie;
}

fn getStatic(self: *const Linux, d: *const Driver) bool {
    _ = self;
    return d.static and !d.staticPie;
}

pub fn getDefaultLinker(self: *const Linux, target: std.Target) []const u8 {
    _ = self;
    if (target.abi.isAndroid())
        return "ld.lld";
    return "ld";
}

pub fn buildLinkerArgs(self: *const Linux, tc: *const Toolchain, argv: *std.ArrayList([]const u8)) Compilation.Error!void {
    const d = tc.driver;
    const gpa = d.comp.gpa;
    const target = tc.getTarget();

    const isPie = self.getPIE(d);
    const isStaticPie = try self.getStaticPIE(d);
    const isStatic = self.getStatic(d);
    const isAndroid = target.abi.isAndroid();
    const isVe = target.cpu.arch == .ve;
    const hasCrtBeginEndFiles = target.abi != .none; // TODO: clang checks for MIPS vendor

    if (isPie) try argv.append(gpa, "-pie");

    if (isStaticPie)
        try argv.appendSlice(gpa, &.{ "-static", "-pie", "--no-dynamic-linker", "-z", "text" });

    if (d.rdynamic) try argv.append(gpa, "-export-dynamic");
    if (d.strip) try argv.append(gpa, "-s");

    try argv.appendSlice(gpa, self.extraOpts.items);
    try argv.append(gpa, "--eh-frame-hdr");

    // Todo: Driver should parse `-EL`/`-EB` for arm to set endianness for arm targets
    if (TargetUtil.ldEmulationOption(target, null)) |emulation| {
        try argv.appendSlice(gpa, &.{ "-m", emulation });
    } else {
        try d.err("Unknown target triple", .{});
        return;
    }

    if (target.cpu.arch.isRISCV())
        try argv.append(gpa, "-X");

    if (d.shared)
        try argv.append(gpa, "-shared");

    if (isStatic) {
        try argv.append(gpa, "-static");
    } else {
        if (d.rdynamic)
            try argv.append(gpa, "-export-dynamic");

        if (!d.shared and !isStaticPie and !d.relocatable) {
            const dynamicLinker = target.standardDynamicLinkerPath();
            // todo: check for --dyld-prefix
            if (dynamicLinker.get()) |path| {
                try argv.appendSlice(gpa, &.{ "-dynamic-linker", try d.comp.arena.dupe(u8, path) });
            } else {
                try d.err("Could not find dynamic linker path", .{});
            }
        }
    }

    try argv.appendSlice(gpa, &.{ "-o", d.outputName orelse "a.out" });

    if (!d.nostdlib and !d.nostartfiles and !d.relocatable) {
        if (!isAndroid) {
            if (!d.shared) {
                const crt1 = if (isPie)
                    "Scrt1.o"
                else if (isStaticPie)
                    "rcrt1.o"
                else
                    "crt1.o";
                try argv.append(gpa, try tc.getFilePath(crt1));
            }
            try argv.append(gpa, try tc.getFilePath("crti.o"));
        }

        if (isVe)
            try argv.appendSlice(gpa, &.{ "-z", "max-page-size=0x4000000" });

        if (hasCrtBeginEndFiles) {
            var path: []const u8 = "";
            if (tc.getRuntimeLibKind() == .compiler_rt and !isAndroid) {
                const crtBegin = try tc.getCompilerRt("crtbegin", .object);
                if (tc.filesystem.exists(crtBegin))
                    path = crtBegin;
            }

            if (path.len == 0) {
                const crtBegin = if (tc.driver.shared)
                    if (isAndroid) "crtbegin_so.o" else "crtbeginS.o"
                else if (isStatic)
                    if (isAndroid) "crtbegin_static.o" else "crtbeginT.o"
                else if (isPie or isStaticPie)
                    if (isAndroid) "crtbegin_dynamic.o" else "crtbeginS.o"
                else if (isAndroid) "crtbegin_dynamic.o" else "crtbegin.o";

                path = try tc.getFilePath(crtBegin);
            }
            try argv.append(gpa, path);
        }
    }

    // TODO add -L opts
    // TODO add -u opts

    try tc.addFilePathLibArgs(argv);

    // TODO handle LTO

    try argv.appendSlice(gpa, d.linkObjects.items);

    if (!d.nostdlib and !d.relocatable) {
        if (!d.nodefaultlibs) {
            if (isStatic or isStaticPie)
                try argv.append(gpa, "--start-group");

            try tc.addRuntimeLibs(argv);

            // TODO: add pthread if needed
            if (!d.nolibc)
                try argv.append(gpa, "-lc");

            if (isStatic or isStaticPie)
                try argv.append(gpa, "--end-group")
            else
                try tc.addRuntimeLibs(argv);
        }

        if (!d.nostartfiles) {
            if (hasCrtBeginEndFiles) {
                var path: []const u8 = "";
                if (tc.getRuntimeLibKind() == .compiler_rt and !isAndroid) {
                    const crtEnd = try tc.getCompilerRt("crtend", .object);
                    if (tc.filesystem.exists(crtEnd))
                        path = crtEnd;
                }

                if (path.len == 0) {
                    const crtEnd = if (d.shared)
                        if (isAndroid) "crtend_so.o" else "crtendS.o"
                    else if (isPie or isStaticPie)
                        if (isAndroid) "crtend_android.o" else "crtendS.o"
                    else if (isAndroid) "crtend_android.o" else "crtend.o";
                    path = try tc.getFilePath(crtEnd);
                }

                try argv.append(gpa, path);
            }

            if (!isAndroid)
                try argv.append(gpa, try tc.getFilePath("crtn.o"));
        }
    }

    // TODO add -T args
}

fn getMultiarchTriple(target: std.Target) ?[]const u8 {
    const isAndroid = target.abi.isAndroid();
    const isMipsR6 = std.Target.mips.featureSetHas(target.cpu.features, .mips32r6);

    return switch (target.cpu.arch) {
        .aarch64 => if (isAndroid) "aarch64-linux-android" else "aarch64-linux-gnu",
        .aarch64_be => "aarch64_be-linux-gnu",
        .x86 => if (isAndroid) "i686-linux-android" else "i386-linux-gnu",
        .x86_64 => if (isAndroid) "x86_64-linux-android" else if (target.abi == .gnux32) "x86_64-linux-gnux32" else "x86_64-linux-gnu",
        .m68k => "m68k-linux-gnu",
        .mips => if (isMipsR6) "mipsisa32r6-linux-gnu" else "mips-linux-gnu",
        .mipsel => if (isAndroid) "mipsel-linux-android" else if (isMipsR6) "mipsisa32r6el-linux-gnu" else "mipsel-linux-gnu",
        .powerpcle => "powerpcle-linux-gnu",
        .powerpc64 => "powerpc64-linux-gnu",
        .powerpc64le => "powerpc64le-linux-gnu",
        .riscv64 => "riscv64-linux-gnu",
        .sparc => "sparc-linux-gnu",
        .sparc64 => "sparc64-linux-gnu",
        .s390x => "s390x-linux-gnu",

        // TODO: expand this
        else => null,
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

pub fn defineSystemIncludes(self: *const Linux, tc: *const Toolchain) !void {
    if (tc.driver.nostdinc) return;

    const comp = tc.driver.comp;
    const target = tc.getTarget();

    // musl prefers /usr/include before builtin includes, so musl targets will add builtins
    // at the end of this function (unless disabled with nostdlibinc)
    if (!tc.driver.nobuiltininc and (!target.abi.isMusl() or tc.driver.nostdlibinc)) {
        try comp.addBuiltinIncludeDir(tc.driver.zincName);
    }

    if (tc.driver.nostdlibinc) return;

    const sysroot = tc.getSysroot();
    const localInclude = try std.fs.path.join(comp.gpa, &.{ sysroot, "/usr/local/include" });
    defer comp.gpa.free(localInclude);
    try comp.addSystemIncludeDir(localInclude);

    if (self.gccDetector.isValid) {
        const gccIncludePath = try std.fs.path.join(comp.gpa, &.{
            self.gccDetector.parentLibPath,
            "..",
            self.gccDetector.gccTriple,
            "include",
        });
        defer comp.gpa.free(gccIncludePath);
        try comp.addSystemIncludeDir(gccIncludePath);
    }

    if (getMultiarchTriple(target)) |triple| {
        const joined = try std.fs.path.join(comp.gpa, &.{ sysroot, "/usr/include", triple });
        defer comp.gpa.free(joined);
        if (tc.filesystem.exists(joined)) {
            try comp.addSystemIncludeDir(joined);
        }
    }

    if (target.os.tag == .rtems) return;

    try comp.addSystemIncludeDir("/include");
    try comp.addSystemIncludeDir("/usr/include");

    std.debug.assert(!tc.driver.nostdlibinc);
    if (!tc.driver.nobuiltininc and target.abi.isMusl()) {
        try comp.addBuiltinIncludeDir(tc.driver.zincName);
    }
}

test Linux {
    if (@import("builtin").os.tag == .windows) return error.SkipZigTest;

    const gpa = std.testing.allocator;
    var arenaInstance = std.heap.ArenaAllocator.init(gpa);
    defer arenaInstance.deinit();

    const arena = arenaInstance.allocator();

    var comp = Compilation.init(gpa, arena, undefined, std.fs.cwd());
    defer comp.deinit();

    comp.environment = .{
        .path = "/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin",
    };
    defer comp.environment = .{};

    const rawTriple = "x86_64-linux-gnu";
    const targetQuery = try std.Target.Query.parse(.{ .arch_os_abi = rawTriple });
    comp.target = try std.zig.system.resolveTargetQuery(targetQuery);
    comp.langOpts.setEmulatedCompiler(.gcc);

    var driver: Driver = .{ .comp = &comp, .diagnostics = undefined };
    defer driver.deinit();
    driver.rawTargetTriple = rawTriple;

    const linkObj = try driver.comp.gpa.dupe(u8, "/tmp/foo.o");
    try driver.linkObjects.append(driver.comp.gpa, linkObj);
    driver.tempFileCount += 1;

    var toolchain: Toolchain = .{ .driver = &driver, .filesystem = .{ .fake = &.{
        .{ .path = "/tmp" },
        .{ .path = "/usr" },
        .{ .path = "/usr/lib64" },
        .{ .path = "/usr/bin" },
        .{ .path = "/usr/bin/ld", .executable = true },
        .{ .path = "/lib" },
        .{ .path = "/lib/x86_64-linux-gnu" },
        .{ .path = "/lib/x86_64-linux-gnu/crt1.o" },
        .{ .path = "/lib/x86_64-linux-gnu/crti.o" },
        .{ .path = "/lib/x86_64-linux-gnu/crtn.o" },
        .{ .path = "/lib64" },
        .{ .path = "/usr/lib" },
        .{ .path = "/usr/lib/gcc" },
        .{ .path = "/usr/lib/gcc/x86_64-linux-gnu" },
        .{ .path = "/usr/lib/gcc/x86_64-linux-gnu/9" },
        .{ .path = "/usr/lib/gcc/x86_64-linux-gnu/9/crtbegin.o" },
        .{ .path = "/usr/lib/gcc/x86_64-linux-gnu/9/crtend.o" },
        .{ .path = "/usr/lib/x86_64-linux-gnu" },
        .{ .path = "/etc/lsb-release", .contents = 
        \\DISTRIB_ID=Ubuntu
        \\DISTRIB_RELEASE=20.04
        \\DISTRIB_CODENAME=focal
        \\DISTRIB_DESCRIPTION="Ubuntu 20.04.6 LTS"
        \\
        },
    } } };
    defer toolchain.deinit();

    try toolchain.discover();

    var argv: std.ArrayList([]const u8) = .empty;
    defer argv.deinit(gpa);

    var linkerPathBuffer: [std.fs.max_name_bytes]u8 = undefined;
    const linkerPath = try toolchain.getLinkerPath(&linkerPathBuffer);
    try argv.append(gpa, linkerPath);

    try toolchain.buildLinkerArgs(&argv);

    const expected = [_][]const u8{
        "/usr/bin/ld",
        "-z",
        "relro",
        "--hash-style=gnu",
        "--eh-frame-hdr",
        "-m",
        "elf_x86_64",
        "-dynamic-linker",
        "/lib64/ld-linux-x86-64.so.2",
        "-o",
        "a.out",
        "/lib/x86_64-linux-gnu/crt1.o",
        "/lib/x86_64-linux-gnu/crti.o",
        "/usr/lib/gcc/x86_64-linux-gnu/9/crtbegin.o",
        "-L/usr/lib/gcc/x86_64-linux-gnu/9",
        "-L/usr/lib/gcc/x86_64-linux-gnu/9/../../../../lib64",
        "-L/lib/x86_64-linux-gnu",
        "-L/lib/../lib64",
        "-L/usr/lib/x86_64-linux-gnu",
        "-L/usr/lib/../lib64",
        "-L/lib",
        "-L/usr/lib",
        linkObj,
        "-lgcc",
        "--as-needed",
        "-lgcc_s",
        "--no-as-needed",
        "-lc",
        "-lgcc",
        "--as-needed",
        "-lgcc_s",
        "--no-as-needed",
        "/usr/lib/gcc/x86_64-linux-gnu/9/crtend.o",
        "/lib/x86_64-linux-gnu/crtn.o",
    };

    try std.testing.expectEqual(expected.len, argv.items.len);
    for (expected, argv.items) |expectedItem, actualItem| {
        try std.testing.expectEqualStrings(expectedItem, actualItem);
    }
}
