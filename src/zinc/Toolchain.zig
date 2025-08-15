const std = @import("std");
const mem = std.mem;

const Compilation = @import("Basic/Compilation.zig");
const Driver = @import("Driver.zig");
const Filesystem = @import("Driver/Filesystem.zig").Filesystem;
const Linux = @import("Toolchains/Linux.zig");
const Multilib = @import("Driver/Multilib.zig");
const SystemDefaults = @import("system-defaults");
const TargetUtil = @import("Basic/Target.zig");

const Toolchain = @This();

pub const PathList = std.ArrayListUnmanaged([]const u8);

pub const RuntimeLibKind = enum {
    compiler_rt,
    libgcc,
};

pub const FileKind = enum {
    object,
    static,
    shared,
};

pub const LibGCCKind = enum {
    unspecified,
    static,
    shared,
};

pub const UnwindLibKind = enum {
    none,
    compiler_rt,
    libgcc,
};

const Inner = union(enum) {
    uninitialized,
    linux: Linux,
    unknown: void,

    fn deinit(self: *Inner, allocator: mem.Allocator) void {
        switch (self.*) {
            .linux => |*linux| linux.deinit(allocator),
            .uninitialized, .unknown => {},
        }
    }
};

filesystem: Filesystem,
driver: *Driver,

/// The list of toolchain specific path prefixes to search for libraries.
libaryPaths: PathList = .{},

/// The list of toolchain specific path prefixes to search for files.
filePaths: PathList = .{},

/// The list of toolchain specific path prefixes to search for programs.
programPaths: PathList = .{},

selectedMultilib: Multilib = .{},

inner: Inner = .{ .uninitialized = {} },

pub fn getTarget(tc: *const Toolchain) std.Target {
    return tc.driver.comp.target;
}

fn getDefaultLinker(tc: *const Toolchain) []const u8 {
    return switch (tc.inner) {
        .uninitialized => unreachable,
        .linux => |linux| linux.getDefaultLinker(tc.getTarget()),
        .unknown => "ld",
    };
}

/// Call this after driver has finished parsing command line arguments to find the toolchain
pub fn discover(tc: *Toolchain) !void {
    if (tc.inner != .uninitialized) return;

    const target = tc.getTarget();
    tc.inner = switch (target.os.tag) {
        .linux => if (target.cpu.arch == .hexagon)
            .{ .unknown = {} } // TODO
        else if (target.cpu.arch.isMIPS())
            .{ .unknown = {} } // TODO
        else if (target.cpu.arch.isPowerPC())
            .{ .unknown = {} } // TODO
        else if (target.cpu.arch == .ve)
            .{ .unknown = {} } // TODO
        else
            .{ .linux = .{} },
        else => .{ .unknown = {} }, // TODO
    };
    return switch (tc.inner) {
        .uninitialized => unreachable,
        .linux => |*linux| linux.discover(tc),
        .unknown => {},
    };
}

pub fn deinit(tc: *Toolchain) void {
    const gpa = tc.driver.comp.gpa;
    tc.inner.deinit(gpa);
    tc.libaryPaths.deinit(gpa);
    tc.programPaths.deinit(gpa);
    tc.filePaths.deinit(gpa);
}

/// Write linker path to `buf` and return a slice of it
pub fn getLinkerPath(tc: *const Toolchain, buf: []u8) ![]const u8 {
    // --ld-path= takes precedence over -fuse-ld= and specifies the executable
    // name. -B, COMPILER_PATH and PATH are consulted if the value does not
    // contain a path component separator.
    // -fuse-ld=lld can be used with --ld-path= to indicate that the binary
    // that --ld-path= points to is lld.
    const useLinker = tc.driver.useLinker orelse SystemDefaults.linker;

    if (tc.driver.linkerPath) |ldPath| {
        var path = ldPath;
        if (path.len > 0) {
            if (std.fs.path.dirname(path) == null)
                path = tc.getProgramPath(path, buf);

            if (tc.filesystem.canExecute(path))
                return path;
        }
        return tc.driver.fatal(
            "invalid linker name in argument '--ld-path={s}'",
            .{path},
        );
    }

    // If we're passed -fuse-ld= with no argument, or with the argument ld,
    // then use whatever the default system linker is.
    if (useLinker.len == 0 or mem.eql(u8, useLinker, "ld")) {
        const default = tc.getDefaultLinker();
        if (std.fs.path.isAbsolute(default)) return default;
        return tc.getProgramPath(default, buf);
    }

    // Extending -fuse-ld= to an absolute or relative path is unexpected. Checking
    // for the linker flavor is brittle. In addition, prepending "ld." or "ld64."
    // to a relative path is surprising. This is more complex due to priorities
    // among -B, COMPILER_PATH and PATH. --ld-path= should be used instead.
    if (mem.indexOfScalar(u8, useLinker, '/') != null)
        try tc.driver.comp.diagnostics.add(.{
            .text = "'-fuse-ld=' taking a path is deprecated; use '--ld-path=' instead",
            .kind = .off,
            .opt = .@"fuse-ld-path",
            .location = null,
        });

    if (std.fs.path.isAbsolute(useLinker)) {
        if (tc.filesystem.canExecute(useLinker))
            return useLinker;
    } else {
        var linkerName = try std.ArrayList(u8).initCapacity(tc.driver.comp.gpa, 5 + useLinker.len); // "ld64." ++ use_linker
        defer linkerName.deinit();

        if (tc.getTarget().os.tag.isDarwin())
            linkerName.appendSliceAssumeCapacity("ld64.")
        else
            linkerName.appendSliceAssumeCapacity("ld.");

        linkerName.appendSliceAssumeCapacity(useLinker);
        const linkerPath = tc.getProgramPath(linkerName.items, buf);
        if (tc.filesystem.canExecute(linkerPath)) {
            return linkerPath;
        }
    }

    if (tc.driver.useLinker) |linker| {
        return tc.driver.fatal(
            "invalid linker name in argument '-fuse-ld={s}'",
            .{linker},
        );
    }
    const defaultLinker = tc.getDefaultLinker();
    return tc.getProgramPath(defaultLinker, buf);
}

/// If an explicit target is provided, also check the prefixed tool-specific name
/// TODO: this isn't exactly right since our target names don't necessarily match up
/// with GCC's.
/// For example the Zig target `arm-freestanding-eabi` would need the `arm-none-eabi` tools
fn possibleProgramNames(
    rawTriple: ?[]const u8,
    name: []const u8,
    buf: *[64]u8,
    possibleNameBuffer: *[2][]const u8,
) []const []const u8 {
    var i: u32 = 0;
    if (rawTriple) |triple| {
        if (std.fmt.bufPrint(buf, "{s}-{s}", .{ triple, name })) |res| {
            possibleNameBuffer[i] = res;
            i += 1;
        } else |_| {}
    }

    possibleNameBuffer[i] = name;
    return possibleNameBuffer[0..i];
}

/// Add toolchain `file_paths` to argv as `-L` arguments
pub fn addFilePathLibArgs(tc: *const Toolchain, argv: *std.ArrayList([]const u8)) !void {
    try argv.ensureUnusedCapacity(tc.filePaths.items.len);

    var bytesNeeded: usize = 0;
    for (tc.filePaths.items) |path| {
        bytesNeeded += path.len + 2; // +2 for `-L`
    }
    var bytes = try tc.driver.comp.arena.alloc(u8, bytesNeeded);
    var index: usize = 0;
    for (tc.filePaths.items) |path| {
        @memcpy(bytes[index..][0..2], "-L");
        @memcpy(bytes[index + 2 ..][0..path.len], path);
        argv.appendAssumeCapacity(bytes[index..][0 .. path.len + 2]);
        index += path.len + 2;
    }
}

/// Search for an executable called `name` or `{triple}-{name} in programPaths and the $PATH environment variable
/// If not found there, just use `name`
/// Writes the result to `buf` and returns a slice of it
fn getProgramPath(tc: *const Toolchain, name: []const u8, buf: []u8) []const u8 {
    var pathBuffer: [std.fs.max_path_bytes]u8 = undefined;
    var fib = std.heap.FixedBufferAllocator.init(&pathBuffer);

    var toolSpecificBuffer: [64]u8 = undefined;
    var possibleNameBuffer: [2][]const u8 = undefined;
    const possibleNames = possibleProgramNames(tc.driver.rawTargetTriple, name, &toolSpecificBuffer, &possibleNameBuffer);

    for (possibleNames) |toolName| {
        for (tc.programPaths.items) |programPath| {
            defer fib.reset();

            const candidate = std.fs.path.join(fib.allocator(), &.{ programPath, toolName }) catch continue;
            if (tc.filesystem.canExecute(candidate) and candidate.len <= buf.len) {
                @memcpy(buf[0..candidate.len], candidate);
                return buf[0..candidate.len];
            }
        }
        return tc.filesystem.findProgramByName(tc.driver.comp.gpa, name, tc.driver.comp.environment.path, buf) orelse continue;
    }

    @memcpy(buf[0..name.len], name);
    return buf[0..name.len];
}

pub fn getSysroot(tc: *const Toolchain) []const u8 {
    return tc.driver.sysroot orelse SystemDefaults.sysroot;
}

/// Search for `name` in a variety of places
/// TODO: cache results based on `name` so we're not repeatedly allocating the same strings?
pub fn getFilePath(tc: *const Toolchain, name: []const u8) ![]const u8 {
    var pathBuffer: [std.fs.max_path_bytes]u8 = undefined;
    var fib = std.heap.FixedBufferAllocator.init(&pathBuffer);
    const allocator = fib.allocator();

    const arena = tc.driver.comp.arena;

    const sysroot = tc.getSysroot();

    // todo check resource dir
    // todo check compiler RT path

    const zincDir = std.fs.path.dirname(tc.driver.zincName) orelse "";
    const candidate = try std.fs.path.join(allocator, &.{ zincDir, "..", name });
    if (tc.filesystem.exists(candidate))
        return arena.dupe(u8, candidate);

    if (tc.searchPaths(&fib, sysroot, tc.libaryPaths.items, name)) |path|
        return arena.dupe(u8, path);

    if (tc.searchPaths(&fib, sysroot, tc.filePaths.items, name)) |path|
        return try arena.dupe(u8, path);

    return name;
}

/// Search a list of `path_prefixes` for the existence `name`
/// Assumes that `fba` is a fixed-buffer allocator, so does not free joined path candidates
fn searchPaths(
    tc: *const Toolchain,
    fib: *std.heap.FixedBufferAllocator,
    sysroot: []const u8,
    pathPrefixes: []const []const u8,
    name: []const u8,
) ?[]const u8 {
    for (pathPrefixes) |path| {
        if (path.len == 0) continue;

        const candidate = if (path[0] == '=')
            std.fs.path.join(fib.allocator(), &.{ sysroot, path[1..], name }) catch continue
        else
            std.fs.path.join(fib.allocator(), &.{ path, name }) catch continue;

        if (tc.filesystem.exists(candidate))
            return candidate;
    }
    return null;
}

const PathKind = enum {
    library,
    file,
    program,
};

/// Join `components` into a path. If the path exists, dupe it into the Compilation arena and
/// add it to the specified path list.
pub fn addPathIfExists(tc: *Toolchain, components: []const []const u8, destKind: PathKind) !void {
    var pathBuffer: [std.fs.max_path_bytes]u8 = undefined;
    var fib = std.heap.FixedBufferAllocator.init(&pathBuffer);

    const candidate = try std.fs.path.join(fib.allocator(), components);

    if (tc.filesystem.exists(candidate)) {
        const duped = try tc.driver.comp.arena.dupe(u8, candidate);
        const dest = switch (destKind) {
            .library => &tc.libaryPaths,
            .file => &tc.filePaths,
            .program => &tc.programPaths,
        };
        try dest.append(tc.driver.comp.gpa, duped);
    }
}

/// Join `components` using the Compilation arena and add the resulting path to `dest_kind`. Does not check
/// whether the path actually exists
pub fn addPathFromComponents(tc: *Toolchain, components: []const []const u8, destKind: PathKind) !void {
    const fullPath = try std.fs.path.join(tc.driver.comp.arena, components);
    const dest = switch (destKind) {
        .library => &tc.libaryPaths,
        .file => &tc.filePaths,
        .program => &tc.programPaths,
    };
    try dest.append(tc.driver.comp.gpa, fullPath);
}

/// Add linker args to `argv`. Does not add path to linker executable as first item;
/// that must be handled separately
/// Items added to `argv` will be string literals or owned by `tc.driver.comp.driver`
/// so they must not be individually freed
pub fn buildLinkerArgs(tc: *Toolchain, argv: *std.ArrayList([]const u8)) !void {
    return switch (tc.inner) {
        .uninitialized => unreachable,
        .linux => |*linux| linux.buildLinkerArgs(tc, argv),
        .unknown => @panic("This toolchain does not support linking yet"),
    };
}

fn getDefaultRuntimeLibKind(tc: *const Toolchain) RuntimeLibKind {
    if (tc.getTarget().abi.isAndroid())
        return .compiler_rt;
    return .libgcc;
}

pub fn getRuntimeLibKind(tc: *const Toolchain) RuntimeLibKind {
    const libname = tc.driver.rtlib orelse SystemDefaults.rtlib;

    if (mem.eql(u8, libname, "compiler-rt"))
        return .compiler_rt
    else if (mem.eql(u8, libname, "libgcc"))
        return .libgcc
    else
        return tc.getDefaultRuntimeLibKind();
}

/// TODO
pub fn getCompilerRt(tc: *const Toolchain, component: []const u8, fileKind: FileKind) ![]const u8 {
    _ = fileKind;
    _ = component;
    _ = tc;
    return "";
}

fn getLibGCCKind(tc: *const Toolchain) LibGCCKind {
    const target = tc.getTarget();

    if (tc.driver.staticLibgcc or tc.driver.static or tc.driver.staticPie or target.abi.isAndroid())
        return .static;

    if (tc.driver.sharedLibgcc)
        return .shared;

    return .unspecified;
}

fn getUnwindLibKind(tc: *const Toolchain) !UnwindLibKind {
    const libname = tc.driver.unwindlib orelse SystemDefaults.unwindlib;
    if (libname.len == 0 or mem.eql(u8, libname, "platform")) {
        switch (tc.getRuntimeLibKind()) {
            .compiler_rt => {
                const target = tc.getTarget();
                if (target.abi.isAndroid() or target.os.tag == .aix)
                    return .compiler_rt
                else
                    return .none;
            },
            .libgcc => return .libgcc,
        }
    } else if (mem.eql(u8, libname, "none")) {
        return .none;
    } else if (mem.eql(u8, libname, "libgcc")) {
        return .libgcc;
    } else if (mem.eql(u8, libname, "libunwind")) {
        if (tc.getRuntimeLibKind() == .libgcc)
            try tc.driver.err("--rtlib=libgcc requires --unwindlib=libgcc", .{});

        return .compiler_rt;
    } else {
        unreachable;
    }
}

fn getAsNeededOption(isSolaris: bool, needed: bool) []const u8 {
    if (isSolaris) {
        return if (needed) "-zignore" else "-zrecord";
    } else {
        return if (needed) "--as-needed" else "--no-as-needed";
    }
}

fn addLibGCC(tc: *const Toolchain, argv: *std.ArrayList([]const u8)) !void {
    const libgccKind = tc.getLibGCCKind();
    if (libgccKind == .static or libgccKind == .unspecified)
        try argv.append("-lgcc");

    try tc.addUnwindLibrary(argv);

    if (libgccKind == .shared)
        try argv.append("-lgcc");
}

fn addUnwindLibrary(tc: *const Toolchain, argv: *std.ArrayList([]const u8)) !void {
    const unw = try tc.getUnwindLibKind();
    const target = tc.getTarget();
    const isAndroid = target.abi.isAndroid();
    if ((isAndroid and unw == .libgcc) or
        target.ofmt == .wasm or
        TargetUtil.isWindowsMSVCEnvironment(target) or
        unw == .none) return;

    const lgk = tc.getLibGCCKind();
    const asNeeded = (lgk == .unspecified) and !isAndroid and !TargetUtil.isCygwinMinGW(target) and target.os.tag != .aix;
    if (asNeeded)
        try argv.append(getAsNeededOption(target.os.tag == .solaris, true));

    switch (unw) {
        .none => return,
        .libgcc => if (lgk == .static) try argv.append("-lgcc_eh") else try argv.append("-lgcc_s"),
        .compiler_rt => if (target.os.tag == .aix) {
            if (lgk != .static)
                try argv.append("-lunwind");
        } else if (lgk == .static) {
            try argv.append("-l:libunwind.a");
        } else if (lgk == .shared) {
            if (TargetUtil.isCygwinMinGW(target))
                try argv.append("-l:libunwind.dll.a")
            else
                try argv.append("-l:libunwind.so");
        } else {
            try argv.append("-lunwind");
        },
    }

    if (asNeeded)
        try argv.append(getAsNeededOption(target.os.tag == .solaris, false));
}

pub fn addRuntimeLibs(tc: *const Toolchain, argv: *std.ArrayList([]const u8)) !void {
    const target = tc.getTarget();
    const rlt = tc.getRuntimeLibKind();
    switch (rlt) {
        .compiler_rt => {
            // TODO
        },
        .libgcc => {
            if (TargetUtil.isKnownWindowsMSVCEnvironment(target)) {
                const rtlibStr = tc.driver.rtlib orelse SystemDefaults.rtlib;
                if (!mem.eql(u8, rtlibStr, "platform"))
                    try tc.driver.err("unsupported runtime library 'libgcc' for platform 'MSVC'", .{});
            } else {
                try tc.addLibGCC(argv);
            }
        },
    }

    if (target.abi.isAndroid() and !tc.driver.static and !tc.driver.staticPie)
        try argv.append("-ldl");
}

pub fn defineSystemIncludes(tc: *Toolchain) !void {
    return switch (tc.inner) {
        .uninitialized => unreachable,
        .linux => |*linux| linux.defineSystemIncludes(tc),
        .unknown => {
            if (tc.driver.nostdinc) return;

            const comp = tc.driver.comp;
            if (!tc.driver.nobuiltininc) {
                try comp.addBuiltinIncludeDir(tc.driver.zincName);
            }

            if (!tc.driver.nostdlibinc) {
                try comp.addSystemIncludeDir("/usr/include");
            }
        },
    };
}
