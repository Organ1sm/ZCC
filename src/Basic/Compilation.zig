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
const StringInterner = @import("../Basic/StringInterner.zig");
const RecordLayout = @import("RecordLayout.zig");

const Allocator = std.mem.Allocator;
const EpochSeconds = std.time.epoch.EpochSeconds;

const Compilation = @This();

pub const Linker = enum {
    ld,
    bfd,
    gold,
    lld,
    mold,
};

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
target: std.Target = builtin.target,
pragmaHandlers: std.StringArrayHashMapUnmanaged(*Pragma) = .{},
onlyPreprocess: bool = false,
onlySyntax: bool = false,
onlyCompile: bool = false,
onlyPreprocessAndCompile: bool = false,
dumpPP: bool = false,
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

stringInterner: StringInterner = .{},

// linker options
useLinker: Linker = .ld,
linkerPath: ?[]const u8 = null,

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
    for (comp.systemIncludeDirs.items) |path|
        comp.gpa.free(path);
    comp.systemIncludeDirs.deinit();
    comp.pragmaHandlers.deinit(comp.gpa);
    comp.generatedBuffer.deinit();
    comp.builtins.deinit(comp.gpa);
    comp.stringInterner.deinit(comp.gpa);
}

pub fn intern(comp: *Compilation, str: []const u8) !StringInterner.StringId {
    return comp.stringInterner.intern(comp.gpa, str);
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
    const day_name = day_names[@intCast((epochDay.day + 3) % 7)];
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

    const ptrWidth = comp.target.ptrBitWidth();

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
        .windows => if (ptrWidth == 32)
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

        .msp430 => try w.writeAll(
            \\#define MSP430 1
            \\#define __MSP430__ 1
            \\
        ),
        else => {},
    }

    if (comp.target.os.tag != .windows) switch (ptrWidth) {
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
            \\#define __BYTE_ORDER__ __ORDER_BIG_ENDIAN__
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
    try comp.generateIntMax(w, "__SCHAR_MAX__", Type.SChar);
    try comp.generateIntMax(w, "__SHRT_MAX__", Type.Short);
    try comp.generateIntMax(w, "__INT_MAX__", Type.Int);
    try comp.generateIntMax(w, "__LONG_MAX__", Type.Long);
    try comp.generateIntMax(w, "__LONG_LONG_MAX__", Type.LongLong);
    try comp.generateIntMax(w, "__WCHAR_MAX__", comp.types.wchar);
    // try comp.generateIntMax(w, "__WINT_MAX__", Type.wideChar(comp));
    // try comp.generateIntMax(w, "__INTMAX_MAX__", Type.wideChar(comp));
    try comp.generateIntMax(w, "__SIZE_MAX__", comp.types.size);
    // try comp.generateIntMax(w, "__UINTMAX_MAX__", Type.wideChar(comp));
    try comp.generateIntMax(w, "__PTRDIFF_MAX__", comp.types.ptrdiff);
    // try comp.generateIntMax(w, "__INTPTR_MAX__", Type.wideChar(comp));
    // try comp.generateIntMax(w, "__UINTPTR_MAX__", Type.sizeT(comp));

    // sizeof types
    try comp.generateSizeofType(w, "__SIZEOF_FLOAT__", Type.Float);
    try comp.generateSizeofType(w, "__SIZEOF_DOUBLE__", Type.Double);
    try comp.generateSizeofType(w, "__SIZEOF_LONG_DOUBLE__", Type.LongDouble);
    try comp.generateSizeofType(w, "__SIZEOF_SHORT__", Type.Short);
    try comp.generateSizeofType(w, "__SIZEOF_INT__", Type.Int);
    try comp.generateSizeofType(w, "__SIZEOF_LONG__", Type.Long);
    try comp.generateSizeofType(w, "__SIZEOF_LONG_LONG__", Type.LongLong);
    try comp.generateSizeofType(w, "__SIZEOF_POINTER__", Type.Pointer);
    try comp.generateSizeofType(w, "__SIZEOF_PTRDIFF_T__", comp.types.ptrdiff);
    try comp.generateSizeofType(w, "__SIZEOF_SIZE_T__", comp.types.size);
    try comp.generateSizeofType(w, "__SIZEOF_WCHAR_T__", comp.types.wchar);
    // try comp.generateSizeofType(w, "__SIZEOF_WINT_T__", Type.Pointer);

    // various int types
    const mapper = comp.stringInterner.getSlowTypeMapper();
    try generateTypeMacro(w, mapper, "__PTRDIFF_TYPE__", comp.types.ptrdiff);
    try generateTypeMacro(w, mapper, "__SIZE_TYPE__", comp.types.size);
    try generateTypeMacro(w, mapper, "__WCHAR_TYPE__", comp.types.wchar);

    return comp.addSourceFromBuffer("<builtin>", buf.items);
}

fn generateTypeMacro(w: anytype, mapper: StringInterner.TypeMapper, name: []const u8, ty: Type) !void {
    try w.print("#define {s} ", .{name});
    try ty.print(mapper, w);
    try w.writeByte('\n');
}

fn generateBuiltinTypes(comp: *Compilation) !void {
    const os = comp.target.os.tag;
    const wchar = switch (comp.target.cpu.arch) {
        .xcore => Type.UChar,
        .ve, .msp430 => Type.UInt,
        .arm, .armeb, .thumb, .thumbeb => if (os != .windows and os != .netbsd and os != .openbsd) Type.UInt else Type.Int,
        .aarch64, .aarch64_be, .aarch64_32 => if (!os.isDarwin() and os != .netbsd) Type.UInt else Type.Int,
        .x86_64, .x86 => if (os == .windows) Type.UShort else Type.Int,
        else => Type.Int,
    };

    const ptrdiff = if (os == .windows and comp.target.ptrBitWidth() == 64)
        Type.LongLong
    else switch (comp.target.ptrBitWidth()) {
        16 => Type.Int,
        32 => Type.Int,
        64 => Type.Long,
        else => unreachable,
    };

    const size = if (os == .windows and comp.target.ptrBitWidth() == 64)
        Type.ULongLong
    else switch (comp.target.ptrBitWidth()) {
        16 => Type.UInt,
        32 => Type.UInt,
        64 => Type.ULong,
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
            else => return Type.Void, // unknown
        },
        .x86, .msp430 => .char_ptr,
        .x86_64 => switch (comp.target.os.tag) {
            .windows => @as(Kind, .char_ptr),
            else => .x86_64_va_list,
        },
        else => return Type.Void, // unknown
    };

    // TODO this might be bad?
    const arena = comp.diag.arena.allocator();

    var ty: Type = undefined;
    switch (kind) {
        .char_ptr => ty = Type.Char,
        .void_ptr => ty = Type.Void,
        .aarch64_va_list => {
            const recordType = try arena.create(Type.Record);
            recordType.* = .{
                .name = try comp.intern("__va_list_tag"),
                .fields = try arena.alloc(Type.Record.Field, 5),
                .fieldAttributes = null,
                .typeLayout = undefined,
            };
            const voidType = try arena.create(Type);
            voidType.* = Type.Void;
            const voidPtr = Type{ .specifier = .Pointer, .data = .{ .subType = voidType } };
            recordType.fields[0] = .{ .name = try comp.intern("__stack"), .ty = voidPtr };
            recordType.fields[1] = .{ .name = try comp.intern("__gr_top"), .ty = voidPtr };
            recordType.fields[2] = .{ .name = try comp.intern("__vr_top"), .ty = voidPtr };
            recordType.fields[3] = .{ .name = try comp.intern("__gr_offs"), .ty = Type.Int };
            recordType.fields[4] = .{ .name = try comp.intern("__vr_offs"), .ty = Type.Int };
            ty = .{ .specifier = .Struct, .data = .{ .record = recordType } };
            RecordLayout.compute(ty, comp, null);
        },
        .x86_64_va_list => {
            const recordType = try arena.create(Type.Record);
            recordType.* = .{
                .name = try comp.intern("__va_list_tag"),
                .fields = try arena.alloc(Type.Record.Field, 4),
                .fieldAttributes = null,
                .typeLayout = undefined, // compute below
            };
            const voidType = try arena.create(Type);
            voidType.* = Type.Void;
            const voidPtr = Type{ .specifier = .Pointer, .data = .{ .subType = voidType } };
            recordType.fields[0] = .{ .name = try comp.intern("gp_offset"), .ty = Type.UInt };
            recordType.fields[1] = .{ .name = try comp.intern("fp_offset"), .ty = Type.UInt };
            recordType.fields[2] = .{ .name = try comp.intern("overflow_arg_area"), .ty = voidPtr };
            recordType.fields[3] = .{ .name = try comp.intern("reg_save_area"), .ty = voidPtr };
            ty = .{ .specifier = .Struct, .data = .{ .record = recordType } };
            RecordLayout.compute(ty, comp, null);
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

pub fn nextLargestIntSameSign(comp: *const Compilation, ty: Type) ?Type {
    std.debug.assert(ty.isInt());
    const specifiers = if (ty.isUnsignedInt(comp))
        [_]Type.Specifier{ .Short, .Int, .Long, .LongLong }
    else
        [_]Type.Specifier{ .UShort, .UInt, .ULong, .ULongLong };
    const size = ty.sizeof(comp).?;
    for (specifiers) |specifier| {
        const candidate = Type.create(specifier);
        if (candidate.sizeof(comp).? > size) return candidate;
    }
    return null;
}

/// If `enum E { ... }` syntax has a fixed underlying integer type regardless of the presence of
/// __attribute__((packed)) or the range of values of the corresponding enumerator constants,
/// specify it here.
/// TODO: likely incomplete
pub fn fixedEnumTagSpecifier(comp: *const Compilation) ?Type.Specifier {
    switch (comp.langOpts.emulate) {
        .msvc => return .Int,
        .clang => if (comp.target.os.tag == .windows) return .Int,
        .gcc => {},
    }
    return null;
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
        errdefer comp.gpa.free(path);
        try comp.systemIncludeDirs.append(path);

        break;
    } else return error.ZccIncludeNotFound;

    if (comp.target.os.tag == .linux) {
        var fib = std.heap.FixedBufferAllocator.init(&buf);
        const tripleStr = try comp.target.linuxTriple(fib.allocator());
        const multiarchPath = try std.fs.path.join(fib.allocator(), &.{ "/usr/include", tripleStr });

        if (!std.meta.isError(std.fs.accessAbsolute(multiarchPath, .{}))) {
            const duped = try comp.gpa.dupe(u8, multiarchPath);
            errdefer comp.gpa.free(duped);
            try comp.systemIncludeDirs.append(duped);
        }
    }

    const usrInclude = try comp.gpa.dupe(u8, "/usr/include");
    errdefer comp.gpa.free(usrInclude);
    try comp.systemIncludeDirs.append(usrInclude);
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
    var state: enum {
        beginning_of_file,
        bom1,
        bom2,
        start,
        back_slash,
        cr,
        back_slash_cr,
        trailing_ws,
    } = .beginning_of_file;
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
                    .start, .cr, .beginning_of_file => {
                        state = .start;
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
                    .bom1, .bom2 => break, //invalid utf-8
                }
            },
            '\n' => {
                switch (state) {
                    .start, .beginning_of_file => {
                        state = .start;
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
                    .bom1, .bom2 => break,
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
                    .beginning_of_file => state = .start,
                    .cr, .back_slash_cr => state = .start,
                    .back_slash => state = .trailing_ws,
                    .bom1, .bom2 => break,
                }
                i += 1;
            },
            '\xEF' => {
                i += 1;
                state = switch (state) {
                    .beginning_of_file => .bom1,
                    else => .start,
                };
            },
            '\xBB' => {
                i += 1;
                state = switch (state) {
                    .bom1 => .bom2,
                    else => .start,
                };
            },
            '\xBF' => {
                switch (state) {
                    .bom2 => i = 0, // rewind and overwrite the BOM
                    else => i += 1,
                }
                state = .start;
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
    try comp.sources.put(dupedPath, source);
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

    /// Advance the iterator until it finds an include directory that matches
    /// the directory which contains `source`.
    fn skipUntilDirMatch(self: *IncludeDirIterator, source: Source.ID) void {
        const path = self.comp.getSource(source).path;
        const includerPath = std.fs.path.dirname(path) orelse ".";
        while (self.next()) |dir| {
            if (std.mem.eql(u8, includerPath, dir)) break;
        }
    }
};

fn getCwdSourceID(includerTokenSource: Source.ID, includeType: IncludeType, which: WhichInclude) ?Source.ID {
    return switch (includeType) {
        .Quotes => switch (which) {
            .First => includerTokenSource,
            .Next => null,
        },

        .AngleBrackets => null,
    };
}

pub fn hasInclude(
    comp: *const Compilation,
    filename: []const u8,
    includerTokenSource: Source.ID, // include token belong to which source
    includeType: IncludeType, // angle bracket or quotes
    which: WhichInclude, // __has_include or __has_include_next
) bool {
    const cwd = std.fs.cwd();
    if (std.fs.path.isAbsolute(filename)) {
        if (which == .Next) return false;
        return !std.meta.isError(cwd.access(filename, .{}));
    }

    const cwdSourceID = getCwdSourceID(includerTokenSource, includeType, which);
    var pathBuffer: [std.fs.MAX_PATH_BYTES]u8 = undefined;
    var it = IncludeDirIterator{
        .comp = comp,
        .cwdSourceID = cwdSourceID,
        .pathBuffer = &pathBuffer,
    };

    if (which == .Next)
        it.skipUntilDirMatch(includerTokenSource);

    while (it.nextWithFile(filename)) |path| {
        if (!std.meta.isError(cwd.access(path, .{}))) return true;
    }
    return false;
}

pub const WhichInclude = enum {
    First,
    Next,
};

pub const IncludeType = enum {
    Quotes, // `"`
    AngleBrackets, // `<`
};

pub fn findInclude(
    comp: *Compilation,
    filename: []const u8,
    includeTokenSource: Source.ID, // include token belong to which source
    includeType: IncludeType, // angle bracket or quotes
    which: WhichInclude, // include or include_next
) !?Source {
    var pathBuffer: [std.fs.MAX_PATH_BYTES]u8 = undefined;

    if (std.fs.path.isAbsolute(filename)) {
        if (which == .Next) return null;
        return if (comp.addSourceFromPath(filename)) |some|
            return some
        else |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            else => null,
        };
    }

    const cwdSourceID = getCwdSourceID(includeTokenSource, includeType, which);
    var it = IncludeDirIterator{
        .comp = comp,
        .cwdSourceID = cwdSourceID,
        .pathBuffer = &pathBuffer,
    };

    if (which == .Next)
        it.skipUntilDirMatch(includeTokenSource);

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

    const Pack = @import("../Lexer/Pragmas/pack.zig");
    var pack = try Pack.init(comp.gpa);

    errdefer {
        gcc.deinit(gcc, comp);
        once.deinit(once, comp);
        message.deinit(message, comp);
        pack.deinit(pack, comp);
    }

    try comp.addPragmaHandler("GCC", gcc);
    try comp.addPragmaHandler("once", once);
    try comp.addPragmaHandler("message", message);
    try comp.addPragmaHandler("pack", pack);
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

/// Determines whether to ignore the alignment requirements for non-zero-sized
/// bitfield types for the given target architecture and operating system.
pub fn ignoreNonZeroSizedBitfieldTypeAlignment(comp: *const Compilation) bool {
    switch (comp.target.cpu.arch) {
        .avr => return true,
        .arm => {
            if (std.Target.arm.featureSetHas(comp.target.cpu.features, .has_v7)) {
                switch (comp.target.os.tag) {
                    .ios => return true,
                    else => return false,
                }
            }
        },
        else => return false,
    }
    return false;
}

pub fn ignoreZeroSizedBitfieldTypeAlignment(comp: *const Compilation) bool {
    switch (comp.target.cpu.arch) {
        .avr => return true,
        else => return false,
    }
}

pub fn minZeroWidthBitfieldAlignment(comp: *const Compilation) ?u29 {
    switch (comp.target.cpu.arch) {
        .avr => return 8,
        .arm => {
            if (std.Target.arm.featureSetHas(comp.target.cpu.features, .has_v7)) {
                switch (comp.target.os.tag) {
                    .ios => return 32,
                    else => return null,
                }
            } else return null;
        },
        else => return null,
    }
}

/// Determines whether the presence of an unnamed field in a struct affects
/// the alignment of the struct for the given target architecture and operating system.
pub fn unnamedFieldAffectsAlignment(comp: *const Compilation) bool {
    switch (comp.target.cpu.arch) {
        .aarch64 => {
            if (comp.target.isDarwin() or comp.target.os.tag == .windows) return false;
            return true;
        },
        .armeb => {
            if (std.Target.arm.featureSetHas(comp.target.cpu.features, .has_v7)) {
                if (std.Target.Abi.default(comp.target.cpu.arch, comp.target.os) == .eabi) return true;
            }
        },
        .arm => return true,
        .avr => return true,
        .thumb => {
            if (comp.target.os.tag == .windows) return false;
            return true;
        },
        else => return false,
    }
    return false;
}

pub fn packAllEnums(comp: *const Compilation) bool {
    return switch (comp.target.cpu.arch) {
        .hexagon => true,
        else => false,
    };
}

/// Default alignment (in bytes) for __attribute__((aligned)) when no alignment is specified
pub fn defaultAlignment(comp: *const Compilation) u29 {
    switch (comp.target.cpu.arch) {
        .avr => return 1,
        .arm => if (comp.target.isAndroid() or comp.target.os.tag == .ios) return 16 else return 8,
        .sparc => if (std.Target.sparc.featureSetHas(comp.target.cpu.features, .v9)) return 16 else return 8,
        .mips, .mipsel => switch (comp.target.abi) {
            .none, .gnuabi64 => return 16,
            else => return 8,
        },
        .s390x, .armeb, .thumbeb, .thumb => return 8,
        else => return 16,
    }
}

pub fn systemCompiler(comp: *const Compilation) LangOpts.Compiler {
    const target = comp.target;
    if (target.isDarwin() or
        target.isAndroid() or
        target.isBSD() or
        target.os.tag == .fuchsia or
        target.os.tag == .solaris or
        target.os.tag == .haiku or
        target.cpu.arch == .hexagon)
    {
        return .clang;
    }

    if (target.os.tag == .uefi)
        return .msvc;

    // this is before windows to grab WindowsGnu
    if (target.abi.isGnu() or target.os.tag == .linux)
        return .gcc;

    if (target.os.tag == .windows)
        return .msvc;

    if (target.cpu.arch == .avr)
        return .gcc;

    return .clang;
}

pub fn getLinkerPath(comp: *Compilation) []const u8 {
    // TODO extremely incomplete
    return switch (comp.useLinker) {
        .ld => "/usr/bin/ld",
        .bfd => "/usr/bin/ld.bfd",
        .gold => "/usr/bin/ld.gold",
        .lld => "/usr/bin/ld.lld",
        .mold => "/usr/bin/ld.mold",
    };
}

pub fn hasInt128(comp: *const Compilation) bool {
    if (comp.target.cpu.arch == .wasm32) return true;
    return comp.target.ptrBitWidth() >= 64;
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
test "alignment functions - smoke test" {
    var comp = Compilation.init(std.testing.allocator);
    defer comp.deinit();

    const x86 = std.Target.Cpu.Arch.x86_64;
    comp.target.cpu = std.Target.Cpu.baseline(x86);
    comp.target.os = std.Target.Os.Tag.defaultVersionRange(.linux, x86);
    comp.target.abi = std.Target.Abi.default(x86, comp.target.os);

    try std.testing.expect(comp.isTlsSupported());
    try std.testing.expect(!comp.ignoreNonZeroSizedBitfieldTypeAlignment());
    try std.testing.expect(comp.minZeroWidthBitfieldAlignment() == null);
    try std.testing.expect(!comp.unnamedFieldAffectsAlignment());
    try std.testing.expect(comp.defaultAlignment() == 16);
    try std.testing.expect(!comp.packAllEnums());
    try std.testing.expect(comp.systemCompiler() == .gcc);

    const arm = std.Target.Cpu.Arch.arm;
    comp.target.cpu = std.Target.Cpu.baseline(arm);
    comp.target.os = std.Target.Os.Tag.defaultVersionRange(.ios, arm);
    comp.target.abi = std.Target.Abi.default(arm, comp.target.os);

    try std.testing.expect(!comp.isTlsSupported());
    try std.testing.expect(comp.ignoreNonZeroSizedBitfieldTypeAlignment());
    try std.testing.expectEqual(@as(?u29, 32), comp.minZeroWidthBitfieldAlignment());
    try std.testing.expect(comp.unnamedFieldAffectsAlignment());
    try std.testing.expect(comp.defaultAlignment() == 16);
    try std.testing.expect(!comp.packAllEnums());
    try std.testing.expect(comp.systemCompiler() == .clang);
}

test "target size/align tests" {
    var comp = Compilation.init(std.testing.allocator);
    defer comp.deinit();

    const x86 = std.Target.Cpu.Arch.x86;
    comp.target.cpu.arch = x86;
    comp.target.cpu.model = &std.Target.x86.cpu.i586;
    comp.target.os = std.Target.Os.Tag.defaultVersionRange(.linux, x86);
    comp.target.abi = std.Target.Abi.gnu;

    const tt: Type = Type.LongLong;

    try std.testing.expectEqual(@as(u64, 8), tt.sizeof(&comp).?);
    try std.testing.expectEqual(@as(u64, 4), tt.alignof(&comp));
}

test "ignore BOM at beginning of file" {
    const BOM = "\xEF\xBB\xBF";

    const Test = struct {
        fn run(buf: []const u8) !void {
            var comp = Compilation.init(std.testing.allocator);
            defer comp.deinit();

            var buff = std.io.fixedBufferStream(buf);
            const source = try comp.addSourceFromReader(buff.reader(), "file.c", @intCast(buf.len));
            const expectedOutput = if (std.mem.startsWith(u8, buf, BOM)) buf[BOM.len..] else buf;
            try std.testing.expectEqualStrings(expectedOutput, source.buffer);
        }
    };

    try Test.run(BOM);
    try Test.run(BOM ++ "x");
    try Test.run("x" ++ BOM);
    try Test.run(BOM ++ " ");
    try Test.run(BOM ++ "\n");
    try Test.run(BOM ++ "\\");

    try Test.run(BOM[0..1] ++ "x");
    try Test.run(BOM[0..2] ++ "x");
    try Test.run(BOM[1..] ++ "x");
    try Test.run(BOM[2..] ++ "x");
}
