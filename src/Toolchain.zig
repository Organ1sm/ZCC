const std = @import("std");
const mem = std.mem;
const Driver = @import("Driver.zig");
const Compilation = @import("Basic/Compilation.zig");
const util = @import("Basic/Util.zig");
const SystemDefaults = @import("system-defaults");
const Linux = @import("Toolchains/Linux.zig");
const Multilib = @import("Driver/Multilib.zig");
const Filesystem = @import("Driver/Filesystem.zig").Filesystem;

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

const Inner = union(enum) {
    linux: Linux,
    unknown: void,

    fn deinit(self: *Inner, allocator: mem.Allocator) void {
        switch (self.*) {
            .linux => |*linux| linux.deinit(allocator),
            .unknown => {},
        }
    }
};

filesystem: Filesystem = .{ .real = {} },
driver: *Driver,
arena: mem.Allocator,

/// The list of toolchain specific path prefixes to search for libraries.
libaryPaths: PathList = .{},

/// The list of toolchain specific path prefixes to search for files.
filePaths: PathList = .{},

/// The list of toolchain specific path prefixes to search for programs.
programPaths: PathList = .{},

selectedMultilib: Multilib = .{},

inner: Inner = .{ .unknown = {} },

pub fn getTarget(self: *const Toolchain) std.Target {
    return self.driver.comp.target;
}

fn getDefaultLinker(self: *const Toolchain) []const u8 {
    return switch (self.inner) {
        .linux => |linux| linux.getDefaultLinker(self.getTarget()),
        .unknown => "ld",
    };
}

/// Call this after driver has finished parsing command line arguments to find the toolchain
pub fn discover(self: *Toolchain) !void {
    const target = self.getTarget();
    self.inner = switch (target.os.tag) {
        .elfiamcu,
        .linux,
        => if (target.cpu.arch == .hexagon)
            .{ .unknown = {} } // TODO
        else if (target.cpu.arch.isMIPS())
            .{ .unknown = {} } // TODO
        else if (target.cpu.arch.isPPC())
            .{ .unknown = {} } // TODO
        else if (target.cpu.arch == .ve)
            .{ .unknown = {} } // TODO
        else
            .{ .linux = .{} },
        else => .{ .unknown = {} }, // TODO
    };
    return switch (self.inner) {
        .linux => |*linux| linux.discover(self),
        .unknown => {},
    };
}

pub fn deinit(self: *Toolchain) void {
    const gpa = self.driver.comp.gpa;
    self.inner.deinit(gpa);
    self.libaryPaths.deinit(gpa);
    self.programPaths.deinit(gpa);
    self.filePaths.deinit(gpa);
}

/// Write linker path to `buf` and return a slice of it
pub fn getLinkerPath(self: *const Toolchain, buf: []u8) ![]const u8 {
    // --ld-path= takes precedence over -fuse-ld= and specifies the executable
    // name. -B, COMPILER_PATH and PATH are consulted if the value does not
    // contain a path component separator.
    // -fuse-ld=lld can be used with --ld-path= to indicate that the binary
    // that --ld-path= points to is lld.
    const useLinker = self.driver.useLinker orelse SystemDefaults.linker;

    if (self.driver.linkerPath) |ld_path| {
        var path = ld_path;
        if (path.len > 0) {
            if (std.fs.path.dirname(path) == null)
                path = self.getProgramPath(path, buf);

            if (self.filesystem.canExecute(path))
                return path;
        }
        return self.driver.fatal(
            "invalid linker name in argument '--ld-path={s}'",
            .{path},
        );
    }

    // If we're passed -fuse-ld= with no argument, or with the argument ld,
    // then use whatever the default system linker is.
    if (useLinker.len == 0 or mem.eql(u8, useLinker, "ld")) {
        const default = self.getDefaultLinker();
        if (std.fs.path.isAbsolute(default)) return default;
        return self.getProgramPath(default, buf);
    }

    // Extending -fuse-ld= to an absolute or relative path is unexpected. Checking
    // for the linker flavor is brittle. In addition, prepending "ld." or "ld64."
    // to a relative path is surprising. This is more complex due to priorities
    // among -B, COMPILER_PATH and PATH. --ld-path= should be used instead.
    if (mem.indexOfScalar(u8, useLinker, '/') != null)
        try self.driver.comp.addDiagnostic(.{ .tag = .fuse_ld_path }, &.{});

    if (std.fs.path.isAbsolute(useLinker)) {
        if (self.filesystem.canExecute(useLinker))
            return useLinker;
    } else {
        var linkerName = try std.ArrayList(u8).initCapacity(self.driver.comp.gpa, 5 + useLinker.len); // "ld64." ++ use_linker
        defer linkerName.deinit();

        if (self.getTarget().isDarwin())
            linkerName.appendSliceAssumeCapacity("ld64.")
        else
            linkerName.appendSliceAssumeCapacity("ld.");

        linkerName.appendSliceAssumeCapacity(useLinker);
        const linkerPath = self.getProgramPath(linkerName.items, buf);
        if (self.filesystem.canExecute(linkerPath)) {
            return linkerPath;
        }
    }

    if (self.driver.useLinker) |linker| {
        return self.driver.fatal(
            "invalid linker name in argument '-fuse-ld={s}'",
            .{linker},
        );
    }
    const defaultLinker = self.getDefaultLinker();
    return self.getProgramPath(defaultLinker, buf);
}

/// If an explicit target is provided, also check the prefixed tool-specific name
/// TODO: this isn't exactly right since our target names don't necessarily match up
/// with GCC's.
/// For example the Zig target `arm-freestanding-eabi` would need the `arm-none-eabi` tools
fn possibleProgramNames(rawTriple: ?[]const u8, name: []const u8, buf: *[64]u8) std.BoundedArray([]const u8, 2) {
    var possibleNames: std.BoundedArray([]const u8, 2) = .{};
    if (rawTriple) |triple| {
        if (std.fmt.bufPrint(buf, "{s}-{s}", .{ triple, name })) |res| {
            possibleNames.appendAssumeCapacity(res);
        } else |_| {}
    }

    possibleNames.appendAssumeCapacity(name);
    return possibleNames;
}

/// Add toolchain `file_paths` to argv as `-L` arguments
pub fn addFilePathLibArgs(tc: *const Toolchain, argv: *std.ArrayList([]const u8)) !void {
    try argv.ensureUnusedCapacity(tc.filePaths.items.len);

    var bytesNeeded: usize = 0;
    for (tc.filePaths.items) |path| {
        bytesNeeded += path.len + 2; // +2 for `-L`
    }
    var bytes = try tc.arena.alloc(u8, bytesNeeded);
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
    const possibleNames = possibleProgramNames(tc.driver.rawTargetTriple, name, &toolSpecificBuffer);

    for (possibleNames.constSlice()) |toolName| {
        for (tc.programPaths.items) |programPath| {
            defer fib.reset();

            const candidate = std.fs.path.join(fib.allocator(), &.{ programPath, toolName }) catch continue;
            if (tc.filesystem.canExecute(candidate) and candidate.len <= buf.len) {
                @memcpy(buf[0..candidate.len], candidate);
                return buf[0..candidate.len];
            }
        }
        return tc.filesystem.findProgramByName(tc.driver.comp.gpa, name, buf) orelse continue;
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

    const sysroot = tc.getSysroot();

    // todo check resource dir
    // todo check compiler RT path

    const candidate = try std.fs.path.join(allocator, &.{ tc.driver.zccDir, "..", name });
    if (tc.filesystem.exists(candidate))
        return tc.arena.dupe(u8, candidate);

    fib.reset();
    if (tc.searchPaths(allocator, sysroot, tc.libaryPaths.items, name)) |path| {
        return tc.arena.dupe(u8, path);
    }

    fib.reset();
    if (tc.searchPaths(allocator, sysroot, tc.filePaths.items, name)) |path| {
        return try tc.arena.dupe(u8, path);
    }

    return name;
}

/// Search a list of `path_prefixes` for the existence `name`
/// Assumes that `fba` is a fixed-buffer allocator, so does not free joined path candidates
fn searchPaths(
    tc: *const Toolchain,
    fba: mem.Allocator,
    sysroot: []const u8,
    pathPrefixes: []const []const u8,
    name: []const u8,
) ?[]const u8 {
    for (pathPrefixes) |path| {
        if (path.len == 0) continue;

        const candidate = if (path[0] == '=')
            std.fs.path.join(fba, &.{ sysroot, path[1..], name }) catch continue
        else
            std.fs.path.join(fba, &.{ path, name }) catch continue;

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

/// Join `components` into a path. If the path exists, dupe it into the toolchain arena and
/// add it to the specified path list.
pub fn addPathIfExists(self: *Toolchain, components: []const []const u8, destKind: PathKind) !void {
    var pathBuffer: [std.fs.max_path_bytes]u8 = undefined;
    var fib = std.heap.FixedBufferAllocator.init(&pathBuffer);

    const candidate = try std.fs.path.join(fib.allocator(), components);

    if (self.filesystem.exists(candidate)) {
        const duped = try self.arena.dupe(u8, candidate);
        const dest = switch (destKind) {
            .library => &self.libaryPaths,
            .file => &self.filePaths,
            .program => &self.programPaths,
        };
        try dest.append(self.driver.comp.gpa, duped);
    }
}

/// Join `components` using the toolchain arena and add the resulting path to `dest_kind`. Does not check
/// whether the path actually exists
pub fn addPathFromComponents(self: *Toolchain, components: []const []const u8, destKind: PathKind) !void {
    const fullPath = try std.fs.path.join(self.arena, components);
    const dest = switch (destKind) {
        .library => &self.libaryPaths,
        .file => &self.filePaths,
        .program => &self.programPaths,
    };
    try dest.append(self.driver.comp.gpa, fullPath);
}

/// Add linker args to `argv`. Does not add path to linker executable as first item;
/// that must be handled separately
/// Items added to `argv` will be string literals or owned by `tc.arena`
/// so they must not be individually freed
pub fn buildLinkerArgs(self: *Toolchain, argv: *std.ArrayList([]const u8)) !void {
    return switch (self.inner) {
        .linux => |*linux| linux.buildLinkerArgs(self, argv),
        .unknown => @panic("This toolchain does not support linking yet"),
    };
}

fn getDefaultRuntimeLibKind(self: *const Toolchain) RuntimeLibKind {
    if (self.getTarget().isAndroid())
        return .compiler_rt;
    return .libgcc;
}

pub fn getRuntimeLibType(self: *const Toolchain) RuntimeLibKind {
    const libname = self.driver.rtlib orelse SystemDefaults.rtlib;

    if (mem.eql(u8, libname, "compiler-rt"))
        return .compiler_rt
    else if (mem.eql(u8, libname, "libgcc"))
        return .libgcc
    else
        return self.getDefaultRuntimeLibKind();
}

/// TODO
pub fn getCompilerRt(tc: *const Toolchain, component: []const u8, fileKind: FileKind) ![]const u8 {
    _ = fileKind;
    _ = component;
    _ = tc;
    return "";
}
