/// Compilation holds onto the entire state of the Program
/// and declared symbols as well and house all compiler operation
const std = @import("std");
const builtin = @import("builtin");
const Source = @import("Source.zig");
const Diagnostics = @import("../Basic/Diagnostics.zig");
const Builtins = @import("Builtins.zig");
const Token = @import("../Lexer/Token.zig").Token;
const LangOpts = @import("LangOpts.zig");
const Type = @import("../AST/Type.zig");
const Pragma = @import("../Lexer/Pragma.zig");

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
pragmaHandlers: std.StringArrayHashMapUnmanaged(*Pragma) = .{},
onlyPreprocess: bool = false,
onlyCompile: bool = false,
dumpAst: bool = false,
dumpTokens: bool = false,
dumpRawTokens: bool = false,
langOpts: LangOpts = .{},
generatedBuffer: std.ArrayList(u8),
builtins: Builtins = .{},
types: struct {
    wchar: Type,
    ptrdiff: Type,
    size: Type,
    vaList: Type,
} = undefined,

pub fn init(gpa: Allocator) Compilation {
    return .{
        .gpa = gpa,
        .sources = std.StringArrayHashMap(Source).init(gpa),
        .diag = Diagnostics.init(gpa),
        .includeDirs = std.ArrayList([]const u8).init(gpa),
        .systemIncludeDirs = std.ArrayList([]const u8).init(gpa),
        .generatedBuffer = std.ArrayList(u8).init(gpa),
    };
}

pub fn deinit(comp: *Compilation) void {
    for (comp.pragmaHandlers.values()) |pragma|
        pragma.deinit(pragma, comp);
    for (comp.sources.values()) |source| {
        comp.gpa.free(source.path);
        comp.gpa.free(source.buffer);
        comp.gpa.free(source.spliceLocs);
    }

    comp.sources.deinit();
    comp.diag.deinit();
    comp.includeDirs.deinit();
    comp.systemIncludeDirs.deinit();
    comp.pragmaHandlers.deinit(comp.gpa);
    if (comp.builtinHeaderPath) |some|
        comp.gpa.free(some);
    comp.generatedBuffer.deinit();
    comp.builtins.deinit(comp.gpa);
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
    try comp.generateBuiltinTypes();
    comp.builtins = try Builtins.create(comp);

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

    if (comp.target.cpu.arch.endian() == .little)
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
    try comp.generateIntMax(w, "__WCHAR_MAX__", comp.types.wchar);
    // try comp.generateIntMax(w, "__WINT_MAX__", Type.wideChar(comp));
    // try comp.generateIntMax(w, "__INTMAX_MAX__", Type.wideChar(comp));
    try comp.generateIntMax(w, "__SIZE_MAX__", comp.types.size);
    // try comp.generateIntMax(w, "__UINTMAX_MAX__", Type.wideChar(comp));
    try comp.generateIntMax(w, "__PTRDIFF_MAX__", comp.types.ptrdiff);
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
    try comp.generateSizeofType(w, "__SIZEOF_PTRDIFF_T__", comp.types.ptrdiff);
    try comp.generateSizeofType(w, "__SIZEOF_SIZE_T__", comp.types.size);
    try comp.generateSizeofType(w, "__SIZEOF_WCHAR_T__", comp.types.wchar);
    // try comp.generateSizeofType(w, "__SIZEOF_WINT_T__", .{ .specifier = .Pointer });

    // various int types
    try generateTypeMacro(w, "__PTRDIFF_TYPE__", comp.types.ptrdiff);
    try generateTypeMacro(w, "__SIZE_TYPE__", comp.types.size);
    try generateTypeMacro(w, "__WCHAR_TYPE__", comp.types.wchar);

    return comp.addSourceFromBuffer("<builtin>", buf.items);
}

fn generateTypeMacro(w: anytype, name: []const u8, ty: Type) !void {
    try w.print("#define {s} ", .{name});
    try ty.print(w);
    try w.writeByte('\n');
}

fn generateBuiltinTypes(comp: *Compilation) !void {
    const os = comp.target.os.tag;
    const wchar: Type = switch (comp.target.cpu.arch) {
        .xcore => .{ .specifier = .UChar },
        .ve => .{ .specifier = .UInt },
        .arm, .armeb, .thumb, .thumbeb => .{
            .specifier = if (os != .windows and os != .netbsd and os != .openbsd) .UInt else .Int,
        },
        .aarch64, .aarch64_be, .aarch64_32 => .{
            .specifier = if (!os.isDarwin() and os != .netbsd) .UInt else .Int,
        },
        .x86_64,
        .x86,
        => .{ .specifier = if (os == .windows) .UShort else .Int },
        else => .{ .specifier = .Int },
    };

    const ptrdiff = if (os == .windows and comp.target.ptrBitWidth() == 64)
        Type{ .specifier = .LongLong }
    else switch (comp.target.ptrBitWidth()) {
        32 => Type{ .specifier = .Int },
        64 => Type{ .specifier = .Long },
        else => unreachable,
    };

    const size = if (os == .windows and comp.target.ptrBitWidth() == 64)
        Type{ .specifier = .ULongLong }
    else switch (comp.target.ptrBitWidth()) {
        32 => Type{ .specifier = .UInt },
        64 => Type{ .specifier = .ULong },
        else => unreachable,
    };

    const vaList = try comp.generateVaListType();
    comp.types = .{
        .wchar = wchar,
        .ptrdiff = ptrdiff,
        .size = size,
        .vaList = vaList,
    };
}

fn generateVaListType(comp: *Compilation) !Type {
    const Kind = enum { char_ptr, void_ptr, aarch64_va_list, x86_64_va_list };
    const kind: Kind = switch (comp.target.cpu.arch) {
        .aarch64 => switch (comp.target.os.tag) {
            .windows => @as(Kind, .char_ptr),
            .ios, .macos, .tvos, .watchos => .char_ptr,
            else => .aarch64_va_list,
        },
        .sparc, .wasm32, .wasm64, .bpfel, .bpfeb, .riscv32, .riscv64, .avr, .spirv32, .spirv64 => .void_ptr,
        .powerpc => switch (comp.target.os.tag) {
            .ios, .macos, .tvos, .watchos, .aix => @as(Kind, .char_ptr),
            else => return Type{ .specifier = .Void }, // unknown
        },
        .x86 => .char_ptr,
        .x86_64 => switch (comp.target.os.tag) {
            .windows => @as(Kind, .char_ptr),
            else => .x86_64_va_list,
        },
        else => return Type{ .specifier = .Void }, // unknown
    };

    // TODO this might be bad?
    const arena = comp.diag.arena.allocator();

    var ty: Type = undefined;
    switch (kind) {
        .char_ptr => ty = .{ .specifier = .Char },
        .void_ptr => ty = .{ .specifier = .Void },
        .aarch64_va_list => {
            const recordType = try arena.create(Type.Record);
            recordType.* = .{
                .name = "__va_list_tag",
                .fields = try arena.alloc(Type.Record.Field, 5),
                .size = 32,
                .alignment = 8,
            };
            const voidType = try arena.create(Type);
            voidType.* = .{ .specifier = .Void };
            const voidPtr = Type{ .specifier = .Pointer, .data = .{ .subType = voidType } };
            recordType.fields[0] = .{ .name = "__stack", .ty = voidPtr };
            recordType.fields[1] = .{ .name = "__gr_top", .ty = voidPtr };
            recordType.fields[2] = .{ .name = "__vr_top", .ty = voidPtr };
            recordType.fields[3] = .{ .name = "__gr_offs", .ty = .{ .specifier = .Int } };
            recordType.fields[4] = .{ .name = "__vr_offs", .ty = .{ .specifier = .Int } };
            ty = .{ .specifier = .Struct, .data = .{ .record = recordType } };
        },
        .x86_64_va_list => {
            const recordType = try arena.create(Type.Record);
            recordType.* = .{
                .name = "__va_list_tag",
                .fields = try arena.alloc(Type.Record.Field, 4),
                .size = 24,
                .alignment = 8,
            };
            const voidType = try arena.create(Type);
            voidType.* = .{ .specifier = .Void };
            const voidPtr = Type{ .specifier = .Pointer, .data = .{ .subType = voidType } };
            recordType.fields[0] = .{ .name = "gp_offset", .ty = .{ .specifier = .UInt } };
            recordType.fields[1] = .{ .name = "fp_offset", .ty = .{ .specifier = .UInt } };
            recordType.fields[2] = .{ .name = "overflow_arg_area", .ty = voidPtr };
            recordType.fields[3] = .{ .name = "reg_save_area", .ty = voidPtr };
            ty = .{ .specifier = .Struct, .data = .{ .record = recordType } };
        },
    }

    if (kind == .char_ptr or kind == .void_ptr) {
        const elemType = try arena.create(Type);
        elemType.* = ty;
        ty = Type{ .specifier = .Pointer, .data = .{ .subType = elemType } };
    } else {
        const arrayType = try arena.create(Type.Array);
        arrayType.* = .{ .len = 1, .elem = ty };
        ty = Type{ .specifier = .Array, .data = .{ .array = arrayType } };
    }

    return ty;
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

pub fn getSource(comp: *const Compilation, id: Source.ID) Source {
    if (id == .generated) return .{
        .path = "<scratch space>",
        .buffer = comp.generatedBuffer.items,
        .id = .generated,
        .spliceLocs = &.{},
    };
    return comp.sources.values()[@intFromEnum(id) - 2];
}

/// Creates a Source from the contents of `reader` and adds it to the Compilation
/// Performs newline splicing, line-ending normalization to '\n', and UTF-8 validation.
/// caller retains ownership of `path`
/// `expected_size` will be allocated to hold the contents of `reader` and *must* be at least
/// as large as the entire contents of `reader`.
/// To add a pre-existing buffer as a Source, see addSourceFromBuffer
/// To add a file's contents given its path, see addSourceFromPath
pub fn addSourceFromReader(comp: *Compilation, reader: anytype, path: []const u8, expectedSize: u32) !Source {
    var contents = try comp.gpa.alloc(u8, expectedSize);
    errdefer comp.gpa.free(contents);

    const dupedPath = try comp.gpa.dupe(u8, path);
    errdefer comp.gpa.free(dupedPath);

    var spliceList = std.ArrayList(u32).init(comp.gpa);
    defer spliceList.deinit();

    const sourceId: Source.ID = @enumFromInt(comp.sources.count() + 2);

    var i: u32 = 0;
    var backslashLoc: u32 = undefined;
    var state: enum { start, back_slash, cr, back_slash_cr, trailing_ws } = .start;
    var line: u32 = 1;

    while (true) {
        const byte = reader.readByte() catch |err| switch (err) {
            error.EndOfStream => break,
            else => |e| return e,
        };
        contents[i] = byte;

        switch (byte) {
            '\r' => {
                switch (state) {
                    .start, .cr => {
                        line += 1;
                        state = .cr;
                        contents[i] = '\n';
                        i += 1;
                    },
                    .back_slash, .trailing_ws, .back_slash_cr => {
                        i = backslashLoc;
                        try spliceList.append(i);
                        if (state == .trailing_ws) {
                            try comp.diag.add(.{
                                .tag = .backslash_newline_escape,
                                .loc = .{ .id = sourceId, .byteOffset = i, .line = line },
                            }, &.{});
                        }
                        state = if (state == .back_slash_cr) .cr else .back_slash_cr;
                    },
                }
            },
            '\n' => {
                switch (state) {
                    .start => {
                        line += 1;
                        i += 1;
                    },
                    .cr, .back_slash_cr => {},
                    .back_slash, .trailing_ws => {
                        i = backslashLoc;
                        if (state == .back_slash or state == .trailing_ws) {
                            try spliceList.append(i);
                        }
                        if (state == .trailing_ws) {
                            try comp.diag.add(.{
                                .tag = .backslash_newline_escape,
                                .loc = .{ .id = sourceId, .byteOffset = i, .line = line },
                            }, &.{});
                        }
                    },
                }
                state = .start;
            },
            '\\' => {
                backslashLoc = i;
                state = .back_slash;
                i += 1;
            },
            '\t', '\x0B', '\x0C', ' ' => {
                switch (state) {
                    .start, .trailing_ws => {},
                    .cr, .back_slash_cr => state = .start,
                    .back_slash => state = .trailing_ws,
                }
                i += 1;
            },
            else => {
                i += 1;
                state = .start;
            },
        }
    }

    const spliceLocs = try spliceList.toOwnedSlice();
    errdefer comp.gpa.free(spliceLocs);

    if (i != contents.len)
        contents = try comp.gpa.realloc(contents, i);

    var source = Source{
        .id = sourceId,
        .path = dupedPath,
        .buffer = contents,
        .spliceLocs = spliceLocs,
    };

    source.checkUtf8();
    try comp.sources.put(path, source);
    return source;
}

/// Caller retains ownership of `path` and `buffer`.
pub fn addSourceFromBuffer(comp: *Compilation, path: []const u8, buffer: []const u8) !Source {
    if (comp.sources.get(path)) |some| return some;

    if (@as(u64, buffer.len) > std.math.maxInt(u32))
        return error.StreamTooLong;

    const size = std.math.cast(u32, buffer.len) orelse return error.StreamTooLong;

    var stream = std.io.fixedBufferStream(buffer);
    const reader = stream.reader();

    return comp.addSourceFromReader(reader, path, size);
}

/// Add a source file to the compilation accord to the given path.
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
///     FileNotFound: If not found the file
pub fn addSourceFromPath(comp: *Compilation, path: []const u8) !Source {
    if (comp.sources.get(path)) |some| return some;

    if (std.mem.indexOfScalar(u8, path, 0) != null)
        return error.FileNotFound;

    const file = try std.fs.cwd().openFile(path, .{});
    defer file.close();

    const size = std.math.cast(u32, try file.getEndPos()) orelse
        return error.StreamTooLong;

    var bufferReader = std.io.bufferedReader(file.reader());
    const reader = bufferReader.reader();

    return comp.addSourceFromReader(reader, path, size);
}

pub const IncludeDirIterator = struct {
    /// A reference to the Compilation instance.
    comp: *const Compilation,
    /// An optional Source.ID representing the current working directory's source.
    cwdSourceID: ?Source.ID,
    /// Index tracking the next include directory to iterate over.
    includeDirsIndex: usize = 0,
    /// Index tracking the next system include directory to iterate over.
    sysIncludeDirsIndex: usize = 0,
    /// nextWithFile will use this to hold the full path for its return value
    /// not required if `nextWithFile` is not used
    pathBuffer: []u8 = undefined,

    /// Retrieves the next include directory path.
    /// If a current working directory source ID is set, it returns its directory path
    /// and clears the source ID. Then it iterates over the include directories and system
    /// include directories of the Compilation instance until all are visited.
    /// @return  The next directory path or null if there are no more directories.
    fn next(self: *IncludeDirIterator) ?[]const u8 {
        // If cwdSourceID is set, return the directory of the corresponding source and unset it.
        if (self.cwdSourceID) |sourceID| {
            self.cwdSourceID = null;
            const path = self.comp.getSource(sourceID).path;
            return std.fs.path.dirname(path) orelse ".";
        }

        // Iterate over the include directories and return the next one if available.
        while (self.includeDirsIndex < self.comp.includeDirs.items.len) {
            defer self.includeDirsIndex += 1;
            return self.comp.includeDirs.items[self.includeDirsIndex];
        }

        // Iterate over the system include directories and return the next one if available.
        while (self.sysIncludeDirsIndex < self.comp.systemIncludeDirs.items.len) {
            defer self.sysIncludeDirsIndex += 1;
            return self.comp.systemIncludeDirs.items[self.sysIncludeDirsIndex];
        }

        // If no more directories are left, return null.
        return null;
    }

    /// Return value is invalidated by subsequent calls to nextWithFile
    fn nextWithFile(self: *IncludeDirIterator, filename: []const u8) ?[]const u8 {
        var fib = std.heap.FixedBufferAllocator.init(self.pathBuffer);
        while (self.next()) |dir| : (fib.end_index = 0) {
            const path = std.fs.path.join(fib.allocator(), &.{ dir, filename }) catch continue;
            return path;
        }
        return null;
    }
};

pub fn findInclude(comp: *Compilation, filename: []const u8, cwdSourceID: ?Source.ID) !?Source {
    var pathBuffer: [std.fs.MAX_PATH_BYTES]u8 = undefined;
    var it = IncludeDirIterator{
        .comp = comp,
        .cwdSourceID = cwdSourceID,
        .pathBuffer = &pathBuffer,
    };

    while (it.nextWithFile(filename)) |path| {
        if (comp.addSourceFromPath(path)) |some|
            return some
        else |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            else => {},
        }
    }

    return null;
}

pub fn addPragmaHandler(comp: *Compilation, name: []const u8, handler: *Pragma) Allocator.Error!void {
    try comp.pragmaHandlers.putNoClobber(comp.gpa, name, handler);
}

pub fn addDefaultPragmaHandlers(comp: *Compilation) Allocator.Error!void {
    const GCC = @import("../Lexer/Pragmas/gcc.zig");
    var gcc = try GCC.init(comp.gpa);

    const Once = @import("../Lexer/Pragmas/once.zig");
    var once = try Once.init(comp.gpa);

    const Message = @import("../Lexer/Pragmas/message.zig");
    var message = try Message.init(comp.gpa);

    errdefer {
        gcc.deinit(gcc, comp);
        once.deinit(once, comp);
        message.deinit(message, comp);
    }

    try comp.addPragmaHandler("GCC", gcc);
    try comp.addPragmaHandler("once", once);
    try comp.addPragmaHandler("message", message);
}

pub fn getPragma(comp: *Compilation, name: []const u8) ?*Pragma {
    return comp.pragmaHandlers.get(name);
}

const PragmaEvent = enum {
    BeforePreprocess,
    BeforeParse,
    AfterParse,
};

pub fn pragmaEvent(comp: *Compilation, event: PragmaEvent) void {
    for (comp.pragmaHandlers.values()) |pragma| {
        const maybeFunc = switch (event) {
            .BeforePreprocess => pragma.beforePreprocess,
            .BeforeParse => pragma.beforeParse,
            .AfterParse => pragma.afterParse,
        };
        if (maybeFunc) |func| func(pragma, comp);
    }
}

pub const renderErrors = Diagnostics.render;

/// Determines whether the current compilation target supports
/// Thread Local Storage (TLS) or not.
///
/// On Darwin(Apple) platforms, it depends on the macOS version.
/// For other platforms, it depends on the CPU architecture.
///
/// Returns true if the target supports TLS, false otherwise.
pub fn isTlsSupported(comp: *Compilation) bool {
    if (comp.target.isDarwin()) {
        var supported = false;
        switch (comp.target.os.tag) {
            .macos => supported = !(comp.target.os.isAtLeast(.macos, .{ .major = 10, .minor = 7, .patch = 0 }) orelse false),
            else => {},
        }
        return supported;
    }
    return switch (comp.target.cpu.arch) {
        .tce, .tcele, .bpfel, .bpfeb, .msp430, .nvptx, .nvptx64, .x86, .arm, .armeb, .thumb, .thumbeb => false,
        else => true,
    };
}

/// Default alignment (in bytes) for __attribute__((aligned)) when no alignment is specified
pub fn defaultAlignment(comp: *const Compilation) u29 {
    switch (comp.target.cpu.arch) {
        .avr => return 1,

        .arm,
        .armeb,
        .thumb,
        .thumbeb,
        => switch (comp.target.abi) {
            .gnueabi, .gnueabihf, .eabi, .eabihf, .musleabi, .musleabihf => return 8,
            else => {},
        },
        else => {},
    }
    return 16;
}

test "addSourceFromReader" {
    const Test = struct {
        fn addSourceFromReader(str: []const u8, expected: []const u8, warningCount: u32, splices: []const u32) !void {
            var comp = Compilation.init(std.testing.allocator);
            defer comp.deinit();

            var stream = std.io.fixedBufferStream(str);
            const reader = stream.reader();
            const source = try comp.addSourceFromReader(reader, "path", @intCast(str.len));

            try std.testing.expectEqualStrings(expected, source.buffer);
            try std.testing.expectEqual(warningCount, @as(u32, @intCast(comp.diag.list.items.len)));
            try std.testing.expectEqualSlices(u32, splices, source.spliceLocs);
        }

        fn withAllocationFailures(allocator: std.mem.Allocator) !void {
            var comp = Compilation.init(allocator);
            defer comp.deinit();

            _ = try comp.addSourceFromBuffer("path", "spliced\\\nbuffer\n");
            _ = try comp.addSourceFromBuffer("path", "non-spliced buffer\n");
        }
    };
    try Test.addSourceFromReader("ab\\\nc", "abc", 0, &.{2});
    try Test.addSourceFromReader("ab\\\rc", "abc", 0, &.{2});
    try Test.addSourceFromReader("ab\\\r\nc", "abc", 0, &.{2});
    try Test.addSourceFromReader("ab\\ \nc", "abc", 1, &.{2});
    try Test.addSourceFromReader("ab\\\t\nc", "abc", 1, &.{2});
    try Test.addSourceFromReader("ab\\                     \t\nc", "abc", 1, &.{2});
    try Test.addSourceFromReader("ab\\\r \nc", "ab \nc", 0, &.{2});
    try Test.addSourceFromReader("ab\\\\\nc", "ab\\c", 0, &.{3});
    try Test.addSourceFromReader("ab\\   \r\nc", "abc", 1, &.{2});
    try Test.addSourceFromReader("ab\\ \\\nc", "ab\\ c", 0, &.{4});
    try Test.addSourceFromReader("ab\\\r\\\nc", "abc", 0, &.{ 2, 2 });
    try Test.addSourceFromReader("ab\\  \rc", "abc", 1, &.{2});
    try Test.addSourceFromReader("ab\\", "ab\\", 0, &.{});
    try Test.addSourceFromReader("ab\\\\", "ab\\\\", 0, &.{});
    try Test.addSourceFromReader("ab\\ ", "ab\\ ", 0, &.{});
    try Test.addSourceFromReader("ab\\\n", "ab", 0, &.{2});
    try Test.addSourceFromReader("ab\\\r\n", "ab", 0, &.{2});
    try Test.addSourceFromReader("ab\\\r", "ab", 0, &.{2});

    // carriage return normalization
    try Test.addSourceFromReader("ab\r", "ab\n", 0, &.{});
    try Test.addSourceFromReader("ab\r\r", "ab\n\n", 0, &.{});
    try Test.addSourceFromReader("ab\r\r\n", "ab\n\n", 0, &.{});
    try Test.addSourceFromReader("ab\r\r\n\r", "ab\n\n\n", 0, &.{});
    try Test.addSourceFromReader("\r\\", "\n\\", 0, &.{});
    try Test.addSourceFromReader("\\\r\\", "\\", 0, &.{0});

    try std.testing.checkAllAllocationFailures(std.testing.allocator, Test.withAllocationFailures, .{});
}

test "addSourceFromReader - exhaustive check for carriage return elimination" {
    const alphabet = [_]u8{ '\r', '\n', ' ', '\\', 'a' };
    const alen = alphabet.len;
    var buffer: [alphabet.len]u8 = [1]u8{alphabet[0]} ** alen;

    var comp = Compilation.init(std.testing.allocator);
    defer comp.deinit();

    var sourceCount: u32 = 0;

    while (true) {
        const source = try comp.addSourceFromBuffer(&buffer, &buffer);
        sourceCount += 1;
        try std.testing.expect(std.mem.indexOfScalar(u8, source.buffer, '\r') == null);

        if (std.mem.allEqual(u8, &buffer, alphabet[alen - 1]))
            break;

        var idx = std.mem.indexOfScalar(u8, &alphabet, buffer[buffer.len - 1]).?;
        buffer[buffer.len - 1] = alphabet[(idx + 1) % alen];
        var j = buffer.len - 1;
        while (j > 0) : (j -= 1) {
            idx = std.mem.indexOfScalar(u8, &alphabet, buffer[j - 1]).?;
            if (buffer[j] == alphabet[0]) buffer[j - 1] = alphabet[(idx + 1) % alen] else break;
        }
    }
    try std.testing.expect(sourceCount == std.math.powi(usize, alen, alen) catch unreachable);
}
