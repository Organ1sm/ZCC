/// Compilation holds onto the entire state of the Program
/// and declared symbols as well and house all compiler operation
const std = @import("std");
const builtin = @import("builtin");
const Source = @import("Source.zig");
const Diagnostics = @import("../Basic/Diagnostics.zig");
const Token = @import("../Lexer/Token.zig").Token;
const LangOpts = @import("LangOpts.zig");
const Type = @import("../AST/Type.zig");

const Allocator = std.mem.Allocator;
const EpochSeconds = std.time.epoch.EpochSeconds;

const Compilation = @This();
pub const Error = error{
    /// A fatal error has ocurred and compilation has stopped.
    FatalError,
} || Allocator.Error;

gpa: Allocator,
sources: std.StringArrayHashMap(Source),
diag: Diagnostics,
includeDirs: std.ArrayList([]const u8),
systemIncludeDirs: std.ArrayList([]const u8),
outputName: ?[]const u8 = null,
builtinHeaderPath: ?[]u8 = null,
target: std.Target = builtin.target,
onlyPreprocess: bool = false,
onlyCompile: bool = false,
dumpAst: bool = false,
langOpts: LangOpts = .{},

pub fn init(gpa: Allocator) Compilation {
    return .{
        .gpa = gpa,
        .sources = std.StringArrayHashMap(Source).init(gpa),
        .diag = Diagnostics.init(gpa),
        .includeDirs = std.ArrayList([]const u8).init(gpa),
        .systemIncludeDirs = std.ArrayList([]const u8).init(gpa),
    };
}

pub fn deinit(compilation: *Compilation) void {
    for (compilation.sources.values()) |source| {
        compilation.gpa.free(source.path);
        compilation.gpa.free(source.buffer);
    }

    compilation.sources.deinit();
    compilation.diag.deinit();
    compilation.includeDirs.deinit();
    compilation.systemIncludeDirs.deinit();
    if (compilation.builtinHeaderPath) |some| compilation.gpa.free(some);
}

/// Dec 31 9999 23:59:59
const MaxTimestamp = 253402300799;

fn generateDateAndTime(w: anytype) !void {
    const timestamp = std.math.clamp(std.time.timestamp(), 0, std.math.maxInt(i64));
    const epochSeconds = EpochSeconds{ .secs = @as(u64, @intCast(timestamp)) };
    const epochDay = epochSeconds.getEpochDay();
    const daySeconds = epochSeconds.getDaySeconds();
    const yearDay = epochDay.calculateYearDay();
    const monthDay = yearDay.calculateMonthDay();

    const MonthNames = [_][]const u8{ "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" };
    std.debug.assert(std.time.epoch.Month.jan.numeric() == 1);

    const monthName = MonthNames[monthDay.month.numeric() - 1];
    try w.print("#define __DATE__ \"{s} {d: >2} {d}\"\n", .{
        monthName,
        monthDay.day_index + 1,
        yearDay.year,
    });
    try w.print("#define __TIME__ \"{d:0>2}:{d:0>2}:{d:0>2}\"\n", .{
        daySeconds.getHoursIntoDay(),
        daySeconds.getMinutesIntoHour(),
        daySeconds.getSecondsIntoMinute(),
    });

    const day_names = [_][]const u8{ "Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun" };
    // days since Thu Oct 1 1970
    const day_name = day_names[(epochDay.day + 3) % 7];
    try w.print("#define __TIMESTAMP__ \"{s} {s} {d: >2} {d:0>2}:{d:0>2}:{d:0>2} {d}\"\n", .{
        day_name,
        monthName,
        monthDay.day_index + 1,
        daySeconds.getHoursIntoDay(),
        daySeconds.getMinutesIntoHour(),
        daySeconds.getSecondsIntoMinute(),
        yearDay.year,
    });
}

/// Generate builtin macros that will be available to each source file.
pub fn generateBuiltinMacros(comp: *Compilation) !Source {
    var buf = std.ArrayList(u8).init(comp.gpa);
    defer buf.deinit();

    const w = buf.writer();

    // Standard macros
    try w.writeAll(
        \\#define __VERSION__ "Zcc 
    ++ @import("../Basic/Info.zig").VersionStr ++ "\"\n" ++
        \\#define __Zcc__
        \\#define __STDC__ 1
        \\#define __STDC_HOSTED__ 1
        \\#define __STDC_NO_ATOMICS__ 1
        \\#define __STDC_NO_COMPLEX__ 1
        \\#define __STDC_NO_THREADS__ 1
        \\#define __STDC_NO_VLA__ 1
        \\
    );

    if (comp.langOpts.standard.StdCVersionMacro()) |stdcVersion|
        try w.print("#define __STDC_VERSION__ {s}\n", .{stdcVersion});

    switch (comp.target.os.tag) {
        .linux => try buf.appendSlice(
            \\#define linux 1
            \\#define __linux 1
            \\#define __linux__ 1
            \\
        ),
        .windows => if (comp.target.ptrBitWidth() == 32)
            try w.writeAll(
                \\#define WIN32 1
                \\#define _WIN32 1
                \\#define __WIN32 1
                \\#define __WIN32__ 1
                \\
            )
        else
            try w.writeAll(
                \\#define WIN32 1
                \\#define WIN64 1
                \\#define _WIN32 1
                \\#define _WIN64 1
                \\#define __WIN32 1
                \\#define __WIN64 1
                \\#define __WIN32__ 1
                \\#define __WIN64__ 1
                \\
            ),
        .freebsd => try w.print("#define __FreeBSD__ {d}\n", .{comp.target.os.version_range.semver.min.major}),
        .netbsd => try w.writeAll("#define __NetBSD__ 1\n"),
        .openbsd => try w.writeAll("#define __OpenBSD__ 1\n"),
        .dragonfly => try w.writeAll("#define __DragonFly__ 1\n"),
        .solaris => try w.writeAll(
            \\#define sun 1
            \\#define __sun 1
            \\
        ),
        .macos => try w.writeAll(
            \\#define __APPLE__ 1
            \\#define __MACH__ 1
            \\
        ),
        else => {},
    }

    if (comp.target.abi == .android) {
        try w.writeAll("#define __ANDROID__ 1\n");
    }

    switch (comp.target.cpu.arch) {
        .x86_64 => try w.writeAll(
            \\#define __amd64__ 1
            \\#define __amd64 1
            \\#define __x86_64 1
            \\#define __x86_64__ 1
            \\
        ),

        .x86 => try w.writeAll(
            \\#define i386 1
            \\#define __i386 1
            \\#define __i386__ 1
            \\
        ),

        .mips,
        .mipsel,
        .mips64,
        .mips64el,
        => try w.writeAll(
            \\#define __mips__ 1
            \\#define mips 1
            \\
        ),

        .powerpc,
        .powerpcle,
        => try w.writeAll(
            \\#define __powerpc__ 1
            \\#define __POWERPC__ 1
            \\#define __ppc__ 1
            \\#define __PPC__ 1
            \\#define _ARCH_PPC 1
            \\
        ),

        .powerpc64,
        .powerpc64le,
        => try w.writeAll(
            \\#define __powerpc 1
            \\#define __powerpc__ 1
            \\#define __powerpc64__ 1
            \\#define __POWERPC__ 1
            \\#define __ppc__ 1
            \\#define __ppc64__ 1
            \\#define __PPC__ 1
            \\#define __PPC64__ 1
            \\#define _ARCH_PPC 1
            \\#define _ARCH_PPC64 1
            \\
        ),

        .sparc64 => try w.writeAll(
            \\#define __sparc__ 1
            \\#define __sparc 1
            \\#define __sparc_v9__ 1
            \\
        ),

        .sparc, .sparcel => try w.writeAll(
            \\#define __sparc__ 1
            \\#define __sparc 1
            \\
        ),

        .arm, .armeb => try w.writeAll(
            \\#define __arm__ 1
            \\#define __arm 1
            \\
        ),

        .thumb, .thumbeb => try w.writeAll(
            \\#define __arm__ 1
            \\#define __arm 1
            \\#define __thumb__ 1
            \\
        ),

        .aarch64, .aarch64_be => try w.writeAll("#define __aarch64__ 1\n"),
        else => {},
    }

    if (comp.target.os.tag != .windows) switch (comp.target.ptrBitWidth()) {
        64 => try w.writeAll(
            \\#define _LP64 1
            \\#define __LP64__ 1
            \\
        ),
        32 => try w.writeAll("#define _ILP32 1\n"),
        else => {},
    };

    try w.writeAll(
        \\#define __ORDER_LITTLE_ENDIAN__ 1234
        \\#define __ORDER_BIG_ENDIAN__ 4321
        \\#define __ORDER_PDP_ENDIAN__ 3412
        \\
    );

    if (comp.target.cpu.arch.endian() == .Little)
        try w.writeAll(
            \\#define __BYTE_ORDER__ __ORDER_LITTLE_ENDIAN__
            \\#define __LITTLE_ENDIAN__ 1
            \\
        )
    else
        try w.writeAll(
            \\#define __BYTE_ORDER__ __ORDER_BIG_ENDIAN__;
            \\#define __BIG_ENDIAN__ 1
            \\
        );

    // timestamps
    try generateDateAndTime(w);

    //types
    if (Type.getCharSignedness(comp) == .unsigned)
        try w.writeAll("#define __CHAR_UNSIGNED__ 1\n");
    try w.writeAll("#define __CHAR_BIT__ 8\n");

    // int maxs
    try comp.generateIntMax(w, "__SCHAR_MAX__", .{ .specifier = .SChar });
    try comp.generateIntMax(w, "__SHRT_MAX__", .{ .specifier = .Short });
    try comp.generateIntMax(w, "__INT_MAX__", .{ .specifier = .Int });
    try comp.generateIntMax(w, "__LONG_MAX__", .{ .specifier = .Long });
    try comp.generateIntMax(w, "__LONG_LONG_MAX__", .{ .specifier = .LongLong });
    try comp.generateIntMax(w, "__WCHAR_MAX__", Type.wideChar(comp));
    // try comp.generateIntMax(w, "__WINT_MAX__", Type.wideChar(comp));
    // try comp.generateIntMax(w, "__INTMAX_MAX__", Type.wideChar(comp));
    try comp.generateIntMax(w, "__SIZE_MAX__", Type.sizeT(comp));
    // try comp.generateIntMax(w, "__UINTMAX_MAX__", Type.wideChar(comp));
    try comp.generateIntMax(w, "__PTRDIFF_MAX__", Type.ptrDiffT(comp));
    // try comp.generateIntMax(w, "__INTPTR_MAX__", Type.wideChar(comp));
    // try comp.generateIntMax(w, "__UINTPTR_MAX__", Type.sizeT(comp));

    // sizeof types
    try comp.generateSizeofType(w, "__SIZEOF_FLOAT__", .{ .specifier = .Float });
    try comp.generateSizeofType(w, "__SIZEOF_DOUBLE__", .{ .specifier = .Double });
    try comp.generateSizeofType(w, "__SIZEOF_LONG_DOUBLE__", .{ .specifier = .LongDouble });
    try comp.generateSizeofType(w, "__SIZEOF_SHORT__", .{ .specifier = .Short });
    try comp.generateSizeofType(w, "__SIZEOF_INT__", .{ .specifier = .Int });
    try comp.generateSizeofType(w, "__SIZEOF_LONG__", .{ .specifier = .Long });
    try comp.generateSizeofType(w, "__SIZEOF_LONG_LONG__", .{ .specifier = .LongLong });
    try comp.generateSizeofType(w, "__SIZEOF_POINTER__", .{ .specifier = .Pointer });
    try comp.generateSizeofType(w, "__SIZEOF_PTRDIFF_T__", Type.ptrDiffT(comp));
    try comp.generateSizeofType(w, "__SIZEOF_SIZE_T__", Type.sizeT(comp));
    try comp.generateSizeofType(w, "__SIZEOF_WCHAR_T__", Type.wideChar(comp));
    // try comp.generateSizeofType(w, "__SIZEOF_WINT_T__", .{ .specifier = .Pointer });

    // various int types
    try generateTypeMacro(w, "__PTRDIFF_TYPE__", Type.ptrDiffT(comp));
    try generateTypeMacro(w, "__SIZE_TYPE__", Type.sizeT(comp));
    try generateTypeMacro(w, "__WCHAR_TYPE__", Type.wideChar(comp));

    const dupedPath = try comp.gpa.dupe(u8, "<builtin>");
    errdefer comp.gpa.free(dupedPath);

    const contents = try buf.toOwnedSlice();
    errdefer comp.gpa.free(contents);

    const source = Source{
        .id = @as(Source.ID, @enumFromInt(comp.sources.count() + 2)),
        .path = dupedPath,
        .buffer = contents,
    };
    try comp.sources.put(dupedPath, source);
    return source;
}

fn generateTypeMacro(w: anytype, name: []const u8, ty: Type) !void {
    try w.print("#define {s} ", .{name});
    try ty.print(w);
    try w.writeByte('\n');
}

fn generateIntMax(comp: *Compilation, w: anytype, name: []const u8, ty: Type) !void {
    const bitCount = @as(u8, @intCast(ty.sizeof(comp).? * 8));
    const unsigned = ty.isUnsignedInt(comp);
    const max = if (bitCount == 128)
        @as(u128, if (unsigned) std.math.maxInt(u128) else std.math.maxInt(i128))
    else
        ty.maxInt(comp);
    try w.print("#define {s} {d}\n", .{ name, max });
}

fn generateSizeofType(comp: *Compilation, w: anytype, name: []const u8, ty: Type) !void {
    try w.print("#define {s} {d}\n", .{ name, ty.sizeof(comp).? });
}

/// Define the system header file include directories for Zcc
///
/// This function will search the directory containing the executable,
/// and recursively go upwards to find a directory containing stddef.h
/// in an `include` directory. This will be set as the builtin header
/// include path.
///
/// The default '/usr/include' path will also be added in Linux. All found system
/// header include directories will be appended to `comp.systemIncludeDirs`.
///
/// Params:
/// - comp: The compilation global state
///
/// Errors:
/// - SelfExeNotFound: Failed to obtain self executable path
/// - ZccIncludeNotFound: Did not find system include directory
pub fn defineSystemIncludes(comp: *Compilation) !void {
    var buf: [std.fs.MAX_PATH_BYTES]u8 = undefined;
    var searchPath: []const u8 = std.fs.selfExePath(&buf) catch return error.SelfExeNotFound;

    while (std.fs.path.dirname(searchPath)) |dirname| : (searchPath = dirname) {
        var baseDir = std.fs.cwd().openDir(dirname, .{}) catch continue;
        defer baseDir.close();

        baseDir.access("include/stddef.h", .{}) catch continue;
        const path = try std.fs.path.join(comp.gpa, &.{ dirname, "include" });
        comp.builtinHeaderPath = path;
        try comp.systemIncludeDirs.append(path);
        break;
    } else return error.ZccIncludeNotFound;

    try comp.systemIncludeDirs.append("/usr/include");
}

pub fn getSource(comp: *Compilation, id: Source.ID) Source {
    return comp.sources.values()[@intFromEnum(id) - 2];
}

/// Add a source file to the compilation.
///
/// This will read the given file path and add it as a Source object
/// to the compilation. The contents are loaded into the allocator.
///
/// The path string is duplicated. The Source is added to the compilation
/// sources map, keyed by the path.
///
/// If the source already exists, it will be returned immediately.
///
/// Params:
///     compilation: The Compilation instance
///     path: The file path to add
///
/// Returns: The Source object added
///
/// Errors:
///     FileOpenError: If the file failed to open
///     FileReadError: If reading the file contents failed
///     OutOfMemory: If allocation failed
pub fn addSource(compilation: *Compilation, path: []const u8) !Source {
    if (compilation.sources.get(path)) |some| return some;

    const file = try std.fs.cwd().openFile(path, .{});
    defer file.close();

    const dupedPath = try compilation.gpa.dupe(u8, path);
    errdefer compilation.gpa.free(dupedPath);

    const contents = try file.reader().readAllAlloc(compilation.gpa, std.math.maxInt(u32));
    errdefer compilation.gpa.free(contents);

    const source = Source{
        .id = @as(Source.ID, @enumFromInt(compilation.sources.count() + 2)),
        .path = dupedPath,
        .buffer = contents,
    };

    try compilation.sources.put(dupedPath, source);

    return source;
}

pub fn findInclude(comp: *Compilation, token: Token, filename: []const u8, searchWord: bool) !Source {
    var pathBuff: [std.fs.MAX_PATH_BYTES]u8 = undefined;
    var fib = std.heap.FixedBufferAllocator.init(&pathBuff);

    if (searchWord) blk: {
        const source = comp.getSource(token.source);
        const path = if (std.fs.path.dirname(source.path)) |some|
            std.fs.path.join(fib.allocator(), &.{ some, filename }) catch break :blk
        else
            filename;

        if (comp.addSource(path)) |some|
            return some
        else |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            else => {},
        }
    }

    for (comp.includeDirs.items) |dir| {
        fib.end_index = 0;
        const path = std.fs.path.join(fib.allocator(), &.{ dir, filename }) catch continue;

        if (comp.addSource(path)) |some|
            return some
        else |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            else => {},
        }
    }

    for (comp.systemIncludeDirs.items) |dir| {
        fib.end_index = 0;
        const path = std.fs.path.join(fib.allocator(), &.{ dir, filename }) catch continue;

        if (comp.addSource(path)) |some|
            return some
        else |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            else => {},
        }
    }

    return comp.fatal(token, "'{s}' not found", .{filename});
}

pub fn fatal(comp: *Compilation, token: Token, comptime fmt: []const u8, args: anytype) Error {
    const source = comp.getSource(token.source);
    const lcs = source.getLineColString(token.start);

    return comp.diag.fatal(source.path, lcs, fmt, args);
}

pub fn addDiagnostic(comp: *Compilation, msg: Diagnostics.Message) Error!void {
    if (comp.langOpts.suppress(msg.tag))
        return;
    return comp.diag.add(msg);
}

pub const renderErrors = Diagnostics.render;
