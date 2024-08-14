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
            if (util.canExecute(candidate) and candidate.len <= buf.len) {
                @memcpy(buf[0..candidate.len], candidate);
                return buf[0..candidate.len];
            }
        }
        // todo: check $PATH
    }

    @memcpy(buf[0..name.len], name);
    return buf[0..name.len];
}

/// Search for `name` in a variety of places
/// TODO: cache results based on `name` so we're not repeatedly allocating the same strings?
pub fn getFilePath(tc: *const Toolchain, name: []const u8) ![]const u8 {
    var pathBuffer: [std.fs.max_path_bytes]u8 = undefined;
    var fib = std.heap.FixedBufferAllocator.init(&pathBuffer);
    const allocator = fib.allocator();

    // todo check resource dir
    // todo check compiler RT path

    const candidate = try std.fs.path.join(allocator, &.{ tc.driver.zccDir, "..", name });
    if (util.exists(candidate))
        return tc.arena.dupe(u8, candidate);

    fib.reset();
    if (searchPaths(allocator, tc.driver.sysroot, tc.libaryPaths.items, name)) |path| {
        return tc.arena.dupe(u8, path);
    }

    fib.reset();
    if (searchPaths(allocator, tc.driver.sysroot, tc.filePaths.items, name)) |path| {
        return try tc.arena.dupe(u8, path);
    }

    return name;
}

/// Search a list of `path_prefixes` for the existence `name`
/// Assumes that `fba` is a fixed-buffer allocator, so does not free joined path candidates
fn searchPaths(
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

        if (util.exists(candidate))
            return candidate;
    }
    return null;
}

const PathKind = enum {
    library,
    file,
    program,
};

/// Join `components` into a path. If the path exists, add it to the specified path list.
pub fn addPathIfExists(self: *Toolchain, components: []const []const u8, destKind: PathKind) !void {
    var path_buf: [std.fs.max_path_bytes]u8 = undefined;
    var fib = std.heap.FixedBufferAllocator.init(&path_buf);

    const candidate = try std.fs.path.join(fib.allocator(), components);

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
        .unknown => @panic("This toolchain does not support linking yet"),
    };
}
