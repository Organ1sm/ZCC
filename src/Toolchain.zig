const std = @import("std");
const mem = std.mem;
const Driver = @import("Driver.zig");
const Compilation = @import("Basic/Compilation.zig");
const util = @import("Basic/Util.zig");
const SystemDefaults = @import("system-defaults");
const Linux = @import("Toolchains/Linux.zig");

const Toolchain = @This();

pub const PathList = std.ArrayListUnmanaged([]const u8);

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

driver: *Driver,
arena: mem.Allocator,

/// The list of toolchain specific path prefixes to search for libraries.
libaryPaths: PathList = .{},

/// The list of toolchain specific path prefixes to search for files.
filePaths: PathList = .{},

/// The list of toolchain specific path prefixes to search for programs.
programPaths: PathList = .{},

inner: Inner = .{ .unknown = {} },

pub fn getTarget(self: *const Toolchain) std.Target {
    return self.driver.comp.target;
}

fn getDefaultLinker(self: *const Toolchain) []const u8 {
    _ = self;
    return "ld";
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

    for (self.libaryPaths.items) |item| {
        gpa.free(item);
    }
    self.libaryPaths.deinit(gpa);

    for (self.filePaths.items) |item| {
        gpa.free(item);
    }
    self.filePaths.deinit(gpa);

    for (self.programPaths.items) |item| {
        gpa.free(item);
    }
    self.programPaths.deinit(gpa);
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

            if (util.canExecute(path))
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
        if (util.canExecute(useLinker))
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
        if (util.canExecute(linkerPath)) {
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

fn getProgramPath(toolchain: *const Toolchain, name: []const u8, buf: []u8) []const u8 {
    _ = toolchain;
    _ = buf;
    return name;
}

const PathStackSize = 128;
const PathAllocator = std.heap.StackFallbackAllocator(PathStackSize);

pub fn getFilePath(toolchain: *const Toolchain, name: []const u8) ![]const u8 {
    const d = toolchain.driver;

    var stack_fb = std.heap.stackFallback(PathStackSize, d.comp.gpa);
    var allocator = stack_fb.get();

    // todo check resource dir
    // todo check compiler RT path

    const candidate = try std.fs.path.join(allocator, &.{ d.zccDir, "..", name });
    defer allocator.free(candidate);
    if (util.exists(candidate))
        return toolchain.arena.dupe(u8, candidate);

    if (try searchPaths(&stack_fb, toolchain.libaryPaths.items, name)) |path| {
        defer allocator.free(path);
        return toolchain.arena.dupe(u8, path);
    }

    if (try searchPaths(&stack_fb, toolchain.filePaths.items, name)) |path| {
        defer allocator.free(path);
        return try toolchain.arena.dupe(u8, path);
    }

    return name;
}

/// find path
fn searchPaths(pathAllocator: *PathAllocator, paths: []const []const u8, name: []const u8) !?[]const u8 {
    for (paths) |path| {
        if (path.len == 0) continue;

        const allocator = pathAllocator.get(); // resets underlying fixed buffer
        const candidate = try std.fs.path.join(allocator, &.{ path, name });

        if (util.exists(candidate)) {
            const duped = try pathAllocator.fallback_allocator.dupe(u8, candidate);
            return duped;
        }
        allocator.free(candidate);
    }
    return null;
}

const PathKind = enum {
    library,
    file,
    program,
};

pub fn addPathIfExists(self: *Toolchain, components: []const []const u8, destKind: PathKind) !void {
    var stackFb = std.heap.stackFallback(PathStackSize, self.driver.comp.gpa);
    var allocator = stackFb.get();

    const candidate = try std.fs.path.join(allocator, components);
    defer allocator.free(candidate);

    if (util.exists(candidate)) {
        const gpa = self.driver.comp.gpa;
        const duped = try gpa.dupe(u8, candidate);
        errdefer gpa.free(duped);

        const dest = switch (destKind) {
            .library => &self.libaryPaths,
            .file => &self.filePaths,
            .program => &self.programPaths,
        };
        try dest.append(gpa, duped);
    }
}

pub fn buildLinkerArgs(self: *Toolchain, argv: *std.ArrayList([]const u8)) !void {
    return switch (self.inner) {
        .linux => |*linux| linux.buildLinkerArgs(self, argv),
        .unknown => {},
    };
}
