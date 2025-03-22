/// Compilation holds onto the entire state of the Program
/// and declared symbols as well and house all compiler operation
const std = @import("std");
const assert = std.debug.assert;
const Source = @import("Source.zig");
const Diagnostics = @import("../Basic/Diagnostics.zig");
const Builtins = @import("../Builtins.zig");
const Builtin = Builtins.Builtin;
const Token = @import("../Lexer/Token.zig").Token;
const LangOpts = @import("LangOpts.zig");
const Type = @import("../AST/Type.zig");
const Pragma = @import("../Lexer/Pragma.zig");
const StringInterner = @import("../Basic/StringInterner.zig");
const RecordLayout = @import("RecordLayout.zig");
const Target = @import("Target.zig");
const Interner = @import("backend").Interner;

const Allocator = std.mem.Allocator;
const EpochSeconds = std.time.epoch.EpochSeconds;

const Compilation = @This();

pub const Error = error{
    /// A fatal error has ocurred and compilation has stopped.
    FatalError,
} || Allocator.Error;

pub const BitIntMaxBits = std.math.maxInt(u16);
const PathBufferStackLimit = 1024;

/// Environment variables used during compilation / linking.
pub const Environment = struct {
    /// Directory to use for temporary files
    /// TODO: not implemented yet
    tmpdir: ?[]const u8 = null,

    /// PATH environment variable used to search for programs
    path: ?[]const u8 = null,

    /// Directories to try when searching for subprograms.
    /// TODO: not implemented yet
    compilerPath: ?[]const u8 = null,

    /// Directories to try when searching for special linker files, if compiling for the native target
    /// TODO: not implemented yet
    libraryPath: ?[]const u8 = null,

    /// List of directories to be searched as if specified with -I, but after any paths given with -I options on the command line
    /// Used regardless of the language being compiled
    /// TODO: not implemented yet
    cpath: ?[]const u8 = null,

    /// List of directories to be searched as if specified with -I, but after any paths given with -I options on the command line
    /// Used if the language being compiled is C
    /// TODO: not implemented yet
    cIncludePath: ?[]const u8 = null,

    /// UNIX timestamp to be used instead of the current date and time in the __DATE__ and __TIME__ macros
    sourceDateEpoch: ?[]const u8 = null,

    /// Load all of the environment variables using the std.process API.
    /// Do not use if using zinc as a shared library on Linux without libc
    /// See https://github.com/ziglang/zig/issues/4524
    pub fn loadAll(allocator: std.mem.Allocator) !Environment {
        var env: Environment = .{};
        errdefer env.deinit(allocator);

        inline for (@typeInfo(@TypeOf(env)).@"struct".fields) |field| {
            std.debug.assert(@field(env, field.name) == null);

            var envVarBuffer: [field.name.len]u8 = undefined;
            const envVarName = std.ascii.upperString(&envVarBuffer, field.name);
            const val: ?[]const u8 = std.process.getEnvVarOwned(allocator, envVarName) catch |err| switch (err) {
                error.OutOfMemory => |e| return e,
                error.EnvironmentVariableNotFound => null,
                error.InvalidWtf8 => null,
            };
            @field(env, field.name) = val;
        }

        return env;
    }

    /// Use this only if environment slices were allocated with `allocator` (such as via `loadAll`)
    pub fn deinit(self: *Environment, allocator: std.mem.Allocator) void {
        inline for (@typeInfo(@TypeOf(self.*)).@"struct".fields) |field| {
            if (@field(self, field.name)) |slice| {
                allocator.free(slice);
            }
        }
        self.* = undefined;
    }
};

gpa: Allocator,
diagnostics: Diagnostics,

environment: Environment = .{},
sources: std.StringArrayHashMapUnmanaged(Source) = .{},
includeDirs: std.ArrayListUnmanaged([]const u8) = .{},
systemIncludeDirs: std.ArrayListUnmanaged([]const u8) = .{},
target: std.Target = @import("builtin").target,
pragmaHandlers: std.StringArrayHashMapUnmanaged(*Pragma) = .{},
langOpts: LangOpts = .{},
generatedBuffer: std.ArrayListUnmanaged(u8) = .{},
builtins: Builtins = .{},
types: struct {
    wchar: Type = undefined,
    uintLeast16Ty: Type = undefined,
    uintLeast32Ty: Type = undefined,
    ptrdiff: Type = undefined,
    size: Type = undefined,
    vaList: Type = undefined,
    pidTy: Type = undefined,
    nsConstantString: struct {
        ty: Type = undefined,
        record: Type.Record = undefined,
        fields: [4]Type.Record.Field = undefined,
        intTy_: Type = .{ .specifier = .Int, .qual = .{ .@"const" = true } },
        charTy_: Type = .{ .specifier = .Char, .qual = .{ .@"const" = true } },
    } = .{},
    file: Type = Type.Invalid,
    jmpBuffer: Type = Type.Invalid,
    sigjmpBuffer: Type = Type.Invalid,
    ucontextTy: Type = Type.Invalid,
    intmax: Type = Type.Invalid,
    intptr: Type = Type.Invalid,
    int16: Type = Type.Invalid,
    int64: Type = Type.Invalid,
} = undefined,

stringInterner: StringInterner = .{},
interner: Interner = .{},
msCwdSourceId: ?Source.ID = null,

pub fn init(gpa: Allocator) Compilation {
    return .{
        .gpa = gpa,
        .diagnostics = Diagnostics.init(gpa),
    };
}

/// Initialize Compilation with default environment,
/// pragma handlers and emulation mode set to target.
pub fn initDefault(gpa: Allocator) !Compilation {
    var comp: Compilation = .{
        .gpa = gpa,
        .environment = try Environment.loadAll(gpa),
        .diagnostics = Diagnostics.init(gpa),
    };
    errdefer comp.deinit();

    try comp.addDefaultPragmaHandlers();
    comp.langOpts.setEmulatedCompiler(Target.systemCompiler(comp.target));

    return comp;
}

pub fn deinit(comp: *Compilation) void {
    for (comp.pragmaHandlers.values()) |pragma|
        pragma.deinit(pragma, comp);

    for (comp.sources.values()) |source| {
        comp.gpa.free(source.path);
        comp.gpa.free(source.buffer);
        comp.gpa.free(source.spliceLocs);
    }

    comp.sources.deinit(comp.gpa);
    comp.diagnostics.deinit();
    comp.includeDirs.deinit(comp.gpa);
    for (comp.systemIncludeDirs.items) |path|
        comp.gpa.free(path);
    comp.systemIncludeDirs.deinit(comp.gpa);
    comp.pragmaHandlers.deinit(comp.gpa);
    comp.generatedBuffer.deinit(comp.gpa);
    comp.builtins.deinit(comp.gpa);
    comp.stringInterner.deinit(comp.gpa);
    comp.interner.deinit(comp.gpa);
    comp.environment.deinit(comp.gpa);
}

pub fn internString(comp: *Compilation, str: []const u8) !StringInterner.StringId {
    return comp.stringInterner.intern(comp.gpa, str);
}

/// Dec 31 9999 23:59:59
const MaxTimestamp = 253402300799;

pub fn getSourceEpoch(self: *const Compilation, max: i64) !?i64 {
    const provided = self.environment.sourceDateEpoch orelse return null;
    const parsed = std.fmt.parseInt(i64, provided, 10) catch return error.InvalidEpoch;
    if (parsed < 0 or parsed > max)
        return error.InvalidEpoch;
    return parsed;
}

fn getTimeStamp(comp: *Compilation) !u47 {
    const provided: ?i64 = comp.getSourceEpoch(MaxTimestamp) catch blk: {
        try comp.addDiagnostic(.{
            .tag = .invalid_source_epoch,
            .loc = .{ .id = .unused, .byteOffset = 0, .line = 0 },
        }, &.{});
        break :blk null;
    };
    const timestamp = provided orelse std.time.timestamp();
    return @intCast(std.math.clamp(timestamp, 0, MaxTimestamp));
}

fn generateDateAndTime(w: anytype, timestamp: u47) !void {
    const epochSeconds = EpochSeconds{ .secs = timestamp };
    const epochDay = epochSeconds.getEpochDay();
    const daySeconds = epochSeconds.getDaySeconds();
    const yearDay = epochDay.calculateYearDay();
    const monthDay = yearDay.calculateMonthDay();

    const MonthNames = [_][]const u8{ "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" };
    assert(std.time.epoch.Month.jan.numeric() == 1);

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

/// Which set of system defines to generate via generateBuiltinMacros
pub const SystemDefinesMode = enum {
    /// Only define macros required by the C standard (date/time macros and those beginning with `__STDC`)
    NoSystemDefines,
    /// Define the standard set of system macros
    IncludeSystemDefines,
};

/// Generate builtin macros that will be available to each source file.
pub fn generateSystemDefines(comp: *Compilation, w: anytype) !void {
    const ptrWidth = comp.target.ptrBitWidth();

    switch (comp.target.os.tag) {
        .linux => try w.writeAll(
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

    if (comp.target.abi.isAndroid()) {
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

        .sparc => try w.writeAll(
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

    // atomics
    try w.writeAll(
        \\#define __ATOMIC_RELAXED 0
        \\#define __ATOMIC_CONSUME 1
        \\#define __ATOMIC_ACQUIRE 2
        \\#define __ATOMIC_RELEASE 3
        \\#define __ATOMIC_ACQ_REL 4
        \\#define __ATOMIC_SEQ_CST 5
        \\
    );

    //types
    if (comp.getCharSignedness() == .unsigned) try w.writeAll("#define __CHAR_UNSIGNED__ 1\n");
    try w.writeAll("#define __CHAR_BIT__ 8\n");

    // int maxs
    try comp.generateIntWidth(w, "BOOL", Type.Bool);
    try comp.generateIntMaxAndWidth(w, "SCHAR", Type.SChar);
    try comp.generateIntMaxAndWidth(w, "SHRT", Type.Short);
    try comp.generateIntMaxAndWidth(w, "INT", Type.Int);
    try comp.generateIntMaxAndWidth(w, "LONG", Type.Long);
    try comp.generateIntMaxAndWidth(w, "LONG_LONG", Type.LongLong);
    try comp.generateIntMaxAndWidth(w, "WCHAR", comp.types.wchar);
    // try comp.generateIntMax(w, "WINT", comp.types.wchar);
    try comp.generateIntMaxAndWidth(w, "INTMAX", comp.types.intmax);
    try comp.generateIntMaxAndWidth(w, "SIZE", comp.types.size);
    try comp.generateIntMaxAndWidth(w, "UINTMAX", comp.types.intmax.makeIntegerUnsigned());
    try comp.generateIntMaxAndWidth(w, "PTRDIFF", comp.types.ptrdiff);
    try comp.generateIntMaxAndWidth(w, "INTPTR", comp.types.intptr);
    try comp.generateIntMaxAndWidth(w, "UINTPTR", comp.types.intptr.makeIntegerUnsigned());

    // int widths
    try w.print("#define __BITINT_MAXWIDTH__ {d}\n", .{BitIntMaxBits});

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
    try comp.generateTypeMacro(w, "__INTPTR_TYPE__", comp.types.intptr);
    try comp.generateTypeMacro(w, "__UINTPTR_TYPE__", comp.types.intptr.makeIntegerUnsigned());

    try comp.generateTypeMacro(w, "__INTMAX_TYPE__", comp.types.intmax);
    try comp.generateSuffixMacro("__INTMAX", w, comp.types.intptr);

    try comp.generateTypeMacro(w, "__UINTMAX_TYPE__", comp.types.intmax.makeIntegerUnsigned());
    try comp.generateSuffixMacro("__UINTMAX", w, comp.types.intptr.makeIntegerUnsigned());

    try comp.generateTypeMacro(w, "__PTRDIFF_TYPE__", comp.types.ptrdiff);
    try comp.generateTypeMacro(w, "__SIZE_TYPE__", comp.types.size);
    try comp.generateTypeMacro(w, "__WCHAR_TYPE__", comp.types.wchar);

    try comp.generateExactWidthTypes(w);
    try comp.generateFastAndLeastWidthTypes(w);

    if (Target.FPSemantics.halfPrecisionType(comp.target)) |half| {
        try generateFloatMacros(w, "FLT16", half, "F16");
    }
    try generateFloatMacros(w, "FLT", Target.FPSemantics.forType(.float, comp.target), "F");
    try generateFloatMacros(w, "DBL", Target.FPSemantics.forType(.double, comp.target), "");
    try generateFloatMacros(w, "LDBL", Target.FPSemantics.forType(.longdouble, comp.target), "L");

    // TODO: clang treats __FLT_EVAL_METHOD__ as a special-cased macro because evaluating it within a scope
    // where `#pragma clang fp eval_method(X)` has been called produces an error diagnostic.
    const fltEvalMethod = comp.langOpts.fpEvalMethod orelse Target.defaultFpEvalMethod(comp.target);
    try w.print("#define __FLT_EVAL_METHOD__ {d}\n", .{@intFromEnum(fltEvalMethod)});

    try w.writeAll(
        \\#define __FLT_RADIX__ 2
        \\#define __DECIMAL_DIG__ __LDBL_DECIMAL_DIG__
        \\
    );
}

/// Generate builtin macros that will be available to each source file.
pub fn generateBuiltinMacros(comp: *Compilation, systemDefinesMode: SystemDefinesMode) !Source {
    try comp.generateBuiltinTypes();

    var buf = std.ArrayList(u8).init(comp.gpa);
    defer buf.deinit();

    if (systemDefinesMode == .IncludeSystemDefines) {
        try buf.appendSlice(
            \\#define __VERSION__ "Zinc 
        ++ @import("backend").VersionStr ++ "\"\n" ++
            \\#define __Zinc__
            \\  
        );
    }

    // Standard macros
    try buf.appendSlice(
        \\#define __STDC__ 1
        \\#define __STDC_HOSTED__ 1
        \\#define __STDC_NO_ATOMICS__ 1
        \\#define __STDC_NO_COMPLEX__ 1
        \\#define __STDC_NO_THREADS__ 1
        \\#define __STDC_NO_VLA__ 1
        \\#define __STDC_UTF_16__ 1
        \\#define __STDC_UTF_32__ 1
        \\
    );

    if (comp.langOpts.standard.StdCVersionMacro()) |stdcVersion| {
        try buf.appendSlice("#define __STDC_VERSION__ ");
        try buf.appendSlice(stdcVersion);
        try buf.append('\n');
    }

    // timestamps
    const timestamp = try comp.getTimeStamp();
    try generateDateAndTime(buf.writer(), timestamp);

    if (systemDefinesMode == .IncludeSystemDefines) {
        try comp.generateSystemDefines(buf.writer());
    }

    return comp.addSourceFromBuffer("<builtin>", buf.items);
}

fn generateFloatMacros(w: anytype, prefix: []const u8, semantics: Target.FPSemantics, ext: []const u8) !void {
    const denormMin = semantics.chooseValue(
        []const u8,
        .{
            "5.9604644775390625e-8",
            "1.40129846e-45",
            "4.9406564584124654e-324",
            "3.64519953188247460253e-4951",
            "4.94065645841246544176568792868221e-324",
            "6.47517511943802511092443895822764655e-4966",
        },
    );
    const digits = semantics.chooseValue(i32, .{ 3, 6, 15, 18, 31, 33 });
    const decimalDigits = semantics.chooseValue(i32, .{ 5, 9, 17, 21, 33, 36 });
    const epsilon = semantics.chooseValue(
        []const u8,
        .{
            "9.765625e-4",
            "1.19209290e-7",
            "2.2204460492503131e-16",
            "1.08420217248550443401e-19",
            "4.94065645841246544176568792868221e-324",
            "1.92592994438723585305597794258492732e-34",
        },
    );
    const mantissaDigits = semantics.chooseValue(i32, .{ 11, 24, 53, 64, 106, 113 });

    const min10Exp = semantics.chooseValue(i32, .{ -4, -37, -307, -4931, -291, -4931 });
    const max10Exp = semantics.chooseValue(i32, .{ 4, 38, 308, 4932, 308, 4932 });

    const minExp = semantics.chooseValue(i32, .{ -13, -125, -1021, -16381, -968, -16381 });
    const maxExp = semantics.chooseValue(i32, .{ 16, 128, 1024, 16384, 1024, 16384 });

    const min = semantics.chooseValue(
        []const u8,
        .{
            "6.103515625e-5",
            "1.17549435e-38",
            "2.2250738585072014e-308",
            "3.36210314311209350626e-4932",
            "2.00416836000897277799610805135016e-292",
            "3.36210314311209350626267781732175260e-4932",
        },
    );
    const max = semantics.chooseValue(
        []const u8,
        .{
            "6.5504e+4",
            "3.40282347e+38",
            "1.7976931348623157e+308",
            "1.18973149535723176502e+4932",
            "1.79769313486231580793728971405301e+308",
            "1.18973149535723176508575932662800702e+4932",
        },
    );

    var buf: [32]u8 = undefined;
    var fba = std.heap.FixedBufferAllocator.init(&buf);
    var defPrefix = std.ArrayList(u8).initCapacity(fba.allocator(), 3 + prefix.len) catch unreachable; // fib
    defPrefix.appendSliceAssumeCapacity("__");
    defPrefix.appendSliceAssumeCapacity(prefix);
    defPrefix.appendSliceAssumeCapacity("_");

    try w.print("#define {s}DENORM_MIN__ {s}{s}\n", .{ defPrefix.items, denormMin, ext });
    try w.print("#define {s}HAS_DENORM__\n", .{defPrefix.items});
    try w.print("#define {s}DIG__ {d}\n", .{ defPrefix.items, digits });
    try w.print("#define {s}DECIMAL_DIG__ {d}\n", .{ defPrefix.items, decimalDigits });

    try w.print("#define {s}EPSILON__ {s}{s}\n", .{ defPrefix.items, epsilon, ext });
    try w.print("#define {s}HAS_INFINITY__\n", .{defPrefix.items});
    try w.print("#define {s}HAS_QUIET_NAN__\n", .{defPrefix.items});
    try w.print("#define {s}MANT_DIG__ {d}\n", .{ defPrefix.items, mantissaDigits });

    try w.print("#define {s}MAX_10_EXP__ {d}\n", .{ defPrefix.items, max10Exp });
    try w.print("#define {s}MAX_EXP__ {d}\n", .{ defPrefix.items, maxExp });
    try w.print("#define {s}MAX__ {s}{s}\n", .{ defPrefix.items, max, ext });

    try w.print("#define {s}MIN_10_EXP__ ({d})\n", .{ defPrefix.items, min10Exp });
    try w.print("#define {s}MIN_EXP__ ({d})\n", .{ defPrefix.items, minExp });
    try w.print("#define {s}MIN__ {s}{s}\n", .{ defPrefix.items, min, ext });
}

fn generateTypeMacro(comp: *const Compilation, w: anytype, name: []const u8, ty: Type) !void {
    try w.print("#define {s} ", .{name});
    try ty.print(comp, w);
    try w.writeByte('\n');
}

fn generateBuiltinTypes(comp: *Compilation) !void {
    const os = comp.target.os.tag;
    const wchar = switch (comp.target.cpu.arch) {
        .xcore => Type.UChar,
        .ve, .msp430 => Type.UInt,
        .arm, .armeb, .thumb, .thumbeb => if (os != .windows and os != .netbsd and os != .openbsd) Type.UInt else Type.Int,
        .aarch64, .aarch64_be => if (!os.isDarwin() and os != .netbsd) Type.UInt else Type.Int,
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

    const pidTy: Type = switch (os) {
        .haiku => Type.Long,
        else => Type.Int,
    };

    const intmax = Target.intMaxType(comp.target);
    const intptr = Target.intPtrType(comp.target);
    const int16 = Target.int16Type(comp.target);
    const int64 = Target.int64Type(comp.target);

    comp.types = .{
        .wchar = wchar,
        .ptrdiff = ptrdiff,
        .size = size,
        .vaList = vaList,
        .pidTy = pidTy,
        .intmax = intmax,
        .intptr = intptr,
        .int16 = int16,
        .int64 = int64,
        .uintLeast16Ty = comp.intLeastN(16, .unsigned),
        .uintLeast32Ty = comp.intLeastN(32, .unsigned),
    };

    try comp.generateNsConstantStringType();
}

fn generateNsConstantStringType(comp: *Compilation) !void {
    comp.types.nsConstantString.record = .{
        .name = try comp.internString("__NSConstantString_tag"),
        .fields = &comp.types.nsConstantString.fields,
        .fieldAttributes = null,
        .typeLayout = undefined,
    };
    const constIntPtr = Type{ .specifier = .Pointer, .data = .{ .subType = &comp.types.nsConstantString.intTy_ } };
    const constCharPtr = Type{ .specifier = .Pointer, .data = .{ .subType = &comp.types.nsConstantString.charTy_ } };

    comp.types.nsConstantString.fields[0] = .{ .name = try comp.internString("isa"), .ty = constIntPtr };
    comp.types.nsConstantString.fields[1] = .{ .name = try comp.internString("flags"), .ty = Type.Int };
    comp.types.nsConstantString.fields[2] = .{ .name = try comp.internString("str"), .ty = constCharPtr };
    comp.types.nsConstantString.fields[3] = .{ .name = try comp.internString("length"), .ty = Type.Long };
    comp.types.nsConstantString.ty = .{ .specifier = .Struct, .data = .{ .record = &comp.types.nsConstantString.record } };
    RecordLayout.compute(&comp.types.nsConstantString.record, comp.types.nsConstantString.ty, comp, null);
}

pub fn hasHalfPrecisionFloatABI(comp: *const Compilation) bool {
    return comp.langOpts.allowHalfArgsAndReturns or Target.hasHalfPrecisionFloatABI(comp.target);
}

pub fn hasFloat128(comp: *const Compilation) bool {
    return Target.hasFloat128(comp.target);
}

pub fn float80Type(comp: *const Compilation) ?Type {
    if (comp.langOpts.emulate != .gcc) return null;
    return Target.float80Type(comp.target);
}

fn intSize(comp: *const Compilation, specifier: Type.Specifier) u64 {
    const ty = Type{ .specifier = specifier };
    return ty.sizeof(comp).?;
}

fn generateFastOrLeastType(
    comp: *Compilation,
    bits: usize,
    kind: enum { least, fast },
    signedness: std.builtin.Signedness,
    w: anytype,
) !void {
    const ty = comp.intLeastN(bits, signedness); // defining the fast types as the least types is permitted

    var prefix: std.BoundedArray(u8, 32) = .{};
    const baseName = switch (signedness) {
        .signed => "__INT_",
        .unsigned => "__UINT_",
    };
    const kindStr = switch (kind) {
        .fast => "FAST",
        .least => "LEAST",
    };

    prefix.writer().print("{s}{s}{d}", .{ baseName, kindStr, bits }) catch return error.OutOfMemory;

    {
        const len = prefix.len;
        defer prefix.resize(len) catch unreachable; // restoring previous size
        prefix.appendSliceAssumeCapacity("_TYPE__");
        try comp.generateTypeMacro(w, prefix.constSlice(), ty);
    }

    switch (signedness) {
        .signed => try comp.generateIntMaxAndWidth(w, prefix.constSlice()[2..], ty),
        .unsigned => try comp.generateIntMax(w, prefix.constSlice()[2..], ty),
    }
    try comp.generateFmt(prefix.constSlice(), w, ty);
}

fn generateFastAndLeastWidthTypes(comp: *Compilation, w: anytype) !void {
    const sizes = [_]usize{ 8, 16, 32, 64 };
    for (sizes) |size| {
        try comp.generateFastOrLeastType(size, .least, .signed, w);
        try comp.generateFastOrLeastType(size, .least, .unsigned, w);
        try comp.generateFastOrLeastType(size, .fast, .signed, w);
        try comp.generateFastOrLeastType(size, .fast, .unsigned, w);
    }
}

fn generateExactWidthTypes(comp: *const Compilation, w: anytype) !void {
    try comp.generateExactWidthType(w, .SChar);

    if (comp.intSize(.Short) > comp.intSize(.Char))
        try comp.generateExactWidthType(w, .Short);

    if (comp.intSize(.Int) > comp.intSize(.Short))
        try comp.generateExactWidthType(w, .Int);

    if (comp.intSize(.Long) > comp.intSize(.Int))
        try comp.generateExactWidthType(w, .Long);

    if (comp.intSize(.LongLong) > comp.intSize(.Long))
        try comp.generateExactWidthType(w, .LongLong);

    try comp.generateExactWidthType(w, .UChar);
    try comp.generateExactWidthIntMax(w, .UChar);
    try comp.generateExactWidthIntMax(w, .SChar);

    if (comp.intSize(.Short) > comp.intSize(.Char)) {
        try comp.generateExactWidthType(w, .UShort);
        try comp.generateExactWidthIntMax(w, .UShort);
        try comp.generateExactWidthIntMax(w, .Short);
    }

    if (comp.intSize(.Int) > comp.intSize(.Short)) {
        try comp.generateExactWidthType(w, .UInt);
        try comp.generateExactWidthIntMax(w, .UInt);
        try comp.generateExactWidthIntMax(w, .Int);
    }

    if (comp.intSize(.Long) > comp.intSize(.Int)) {
        try comp.generateExactWidthType(w, .ULong);
        try comp.generateExactWidthIntMax(w, .ULong);
        try comp.generateExactWidthIntMax(w, .Long);
    }

    if (comp.intSize(.Long) > comp.intSize(.Long)) {
        try comp.generateExactWidthType(w, .ULongLong);
        try comp.generateExactWidthIntMax(w, .ULongLong);
        try comp.generateExactWidthIntMax(w, .LongLong);
    }
}

fn generateFmt(comp: *const Compilation, prefix: []const u8, w: anytype, ty: Type) !void {
    const unsigned = ty.isUnsignedInt(comp);
    const modifier = ty.formatModifier();
    const formats = if (unsigned) "ouxX" else "di";
    for (formats) |c|
        try w.print("#define {s}_FMT{c}__ \"{s}{c}\"\n", .{ prefix, c, modifier, c });
}

fn generateSuffixMacro(comp: *const Compilation, prefix: []const u8, w: anytype, ty: Type) !void {
    return w.print("#define {s}_C_SUFFIX__ {s}\n", .{ prefix, ty.intValueSuffix(comp) });
}

/// Generate the following for ty:
///     Name macro (e.g. #define __UINT32_TYPE__ unsigned int)
///     Format strings (e.g. #define __UINT32_FMTu__ "u")
///     Suffix macro (e.g. #define __UINT32_C_SUFFIX__ U)
fn generateExactWidthType(comp: *const Compilation, w: anytype, specifier: Type.Specifier) !void {
    var ty = Type{ .specifier = specifier };
    const width = 8 * ty.sizeof(comp).?;
    const unsigned = ty.isUnsignedInt(comp);

    if (width == 16) {
        ty = if (unsigned) comp.types.int16.makeIntegerUnsigned() else comp.types.int16;
    } else if (width == 64) {
        ty = if (unsigned) comp.types.int64.makeIntegerUnsigned() else comp.types.int64;
    }

    var prefix = std.BoundedArray(u8, 16).init(0) catch unreachable;
    prefix.writer().print("{s}{d}", .{ if (unsigned) "__UINT" else "__INT", width }) catch unreachable;

    {
        const len = prefix.len;
        defer prefix.resize(len) catch unreachable; // restoring previous size
        prefix.appendSliceAssumeCapacity("_TYPE__");
        try comp.generateTypeMacro(w, prefix.constSlice(), ty);
    }

    try comp.generateFmt(prefix.constSlice(), w, ty);
    try comp.generateSuffixMacro(prefix.constSlice(), w, ty);
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
    const arena = comp.diagnostics.arena.allocator();

    var ty: Type = undefined;
    switch (kind) {
        .char_ptr => ty = Type.Char,
        .void_ptr => ty = Type.Void,
        .aarch64_va_list => {
            const recordType = try arena.create(Type.Record);
            recordType.* = .{
                .name = try comp.internString("__va_list_tag"),
                .fields = try arena.alloc(Type.Record.Field, 5),
                .fieldAttributes = null,
                .typeLayout = undefined,
            };
            const voidType = try arena.create(Type);
            voidType.* = Type.Void;
            const voidPtr = Type{ .specifier = .Pointer, .data = .{ .subType = voidType } };
            recordType.fields[0] = .{ .name = try comp.internString("__stack"), .ty = voidPtr };
            recordType.fields[1] = .{ .name = try comp.internString("__gr_top"), .ty = voidPtr };
            recordType.fields[2] = .{ .name = try comp.internString("__vr_top"), .ty = voidPtr };
            recordType.fields[3] = .{ .name = try comp.internString("__gr_offs"), .ty = Type.Int };
            recordType.fields[4] = .{ .name = try comp.internString("__vr_offs"), .ty = Type.Int };
            ty = .{ .specifier = .Struct, .data = .{ .record = recordType } };
            RecordLayout.compute(recordType, ty, comp, null);
        },
        .x86_64_va_list => {
            const recordType = try arena.create(Type.Record);
            recordType.* = .{
                .name = try comp.internString("__va_list_tag"),
                .fields = try arena.alloc(Type.Record.Field, 4),
                .fieldAttributes = null,
                .typeLayout = undefined, // compute below
            };
            const voidType = try arena.create(Type);
            voidType.* = Type.Void;
            const voidPtr = Type{ .specifier = .Pointer, .data = .{ .subType = voidType } };
            recordType.fields[0] = .{ .name = try comp.internString("gp_offset"), .ty = Type.UInt };
            recordType.fields[1] = .{ .name = try comp.internString("fp_offset"), .ty = Type.UInt };
            recordType.fields[2] = .{ .name = try comp.internString("overflow_arg_area"), .ty = voidPtr };
            recordType.fields[3] = .{ .name = try comp.internString("reg_save_area"), .ty = voidPtr };
            ty = .{ .specifier = .Struct, .data = .{ .record = recordType } };
            RecordLayout.compute(recordType, ty, comp, null);
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

fn generateIntMax(comp: *const Compilation, w: anytype, name: []const u8, ty: Type) !void {
    const bitCount = @as(u8, @intCast(ty.sizeof(comp).? * 8));
    const unsigned = ty.isUnsignedInt(comp);
    const max = if (bitCount == 128)
        @as(u128, if (unsigned) std.math.maxInt(u128) else std.math.maxInt(i128))
    else
        ty.maxInt(comp);
    try w.print("#define __{s}_MAX__ {d}{s}\n", .{ name, max, ty.intValueSuffix(comp) });
}

fn generateExactWidthIntMax(comp: *const Compilation, w: anytype, specifier: Type.Specifier) !void {
    var ty = Type{ .specifier = specifier };
    const bitCount: u8 = @intCast(ty.sizeof(comp).? * 8);
    const unsigned = ty.isUnsignedInt(comp);

    if (bitCount == 64)
        ty = if (unsigned) comp.types.int64.makeIntegerUnsigned() else comp.types.int64;

    var name = std.BoundedArray(u8, 6).init(0) catch unreachable;
    name.writer().print("{s}{d}", .{ if (unsigned) "UINT" else "INT", bitCount }) catch return error.OutOfMemory;

    return comp.generateIntMax(w, name.constSlice(), ty);
}

fn generateIntWidth(comp: *Compilation, w: anytype, name: []const u8, ty: Type) !void {
    try w.print("#define __{s}_WIDTH__ {d}\n", .{ name, 8 * ty.sizeof(comp).? });
}

fn generateIntMaxAndWidth(comp: *Compilation, w: anytype, name: []const u8, ty: Type) !void {
    try comp.generateIntMax(w, name, ty);
    try comp.generateIntWidth(w, name, ty);
}

fn generateSizeofType(comp: *Compilation, w: anytype, name: []const u8, ty: Type) !void {
    try w.print("#define {s} {d}\n", .{ name, ty.sizeof(comp).? });
}

pub fn nextLargestIntSameSign(comp: *const Compilation, ty: Type) ?Type {
    assert(ty.isInt());
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

/// Smallest integer type with at least N bits
pub fn intLeastN(comp: *const Compilation, bits: usize, signedness: std.builtin.Signedness) Type {
    const target = comp.target;
    // WebAssembly and Darwin use `long long` for `int_least64_t` and `int_fast64_t`.
    if (bits == 64 and (target.os.tag.isDarwin() or target.cpu.arch.isWasm()))
        return if (signedness == .signed) Type.LongLong else Type.ULongLong;

    // AVR uses int for int_least16_t and int_fast16_t.
    if (bits == 16 and target.cpu.arch == .avr)
        return if (signedness == .signed) Type.Int else Type.UInt;

    const candidates = switch (signedness) {
        .signed => &[_]Type.Specifier{ .SChar, .Short, .Int, .Long, .LongLong },
        .unsigned => &[_]Type.Specifier{ .UChar, .UShort, .UInt, .ULong, .ULongLong },
    };

    for (candidates) |specifier| {
        const ty: Type = .{ .specifier = specifier };
        if (ty.sizeof(comp).? * 8 >= bits) return ty;
    } else unreachable;
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

pub fn getCharSignedness(comp: *const Compilation) std.builtin.Signedness {
    return comp.langOpts.charSignednessOverride orelse comp.target.charSignedness();
}

/// Define the system header file include directories for Zinc
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
/// - ZincIncludeNotFound: Did not find system include directory
pub fn defineSystemIncludes(comp: *Compilation, zincDir: []const u8) !void {
    var stackFallback = std.heap.stackFallback(PathBufferStackLimit, comp.gpa);
    const allocator = stackFallback.get();
    var searchPath = zincDir;

    while (std.fs.path.dirname(searchPath)) |dirname| : (searchPath = dirname) {
        var baseDir = std.fs.cwd().openDir(dirname, .{}) catch continue;
        defer baseDir.close();

        baseDir.access("include/stddef.h", .{}) catch continue;

        const path = try std.fs.path.join(comp.gpa, &.{ dirname, "include" });
        errdefer comp.gpa.free(path);
        try comp.systemIncludeDirs.append(comp.gpa, path);

        break;
    } else return error.ZincIncludeNotFound;

    if (comp.target.os.tag == .linux) {
        const tripleStr = try comp.target.linuxTriple(allocator);
        defer allocator.free(tripleStr);

        const multiarchPath = try std.fs.path.join(allocator, &.{ "/usr/include", tripleStr });
        defer allocator.free(multiarchPath);

        if (!std.meta.isError(std.fs.accessAbsolute(multiarchPath, .{}))) {
            const duped = try comp.gpa.dupe(u8, multiarchPath);
            errdefer comp.gpa.free(duped);
            try comp.systemIncludeDirs.append(comp.gpa, duped);
        }
    }

    const usrInclude = try comp.gpa.dupe(u8, "/usr/include");
    errdefer comp.gpa.free(usrInclude);
    try comp.systemIncludeDirs.append(comp.gpa, usrInclude);
}

pub fn getSource(comp: *const Compilation, id: Source.ID) Source {
    if (id == .generated) return .{
        .path = "<scratch space>",
        .buffer = comp.generatedBuffer.items,
        .id = .generated,
        .spliceLocs = &.{},
        .kind = .User,
    };
    return comp.sources.values()[@intFromEnum(id) - 2];
}

/// Creates a Source from the contents of `reader` and adds it to the Compilation
pub fn addSourceFromReader(comp: *Compilation, reader: anytype, path: []const u8, kind: Source.Kind) !Source {
    const contents = try reader.readAllAlloc(comp.gpa, std.math.maxInt(u32));
    errdefer comp.gpa.free(contents);
    return comp.addSourceFromOwnedBuffer(contents, path, kind);
}

/// Creates a Source from `buf` and adds it to the Compilation
/// Performs newline splicing and line-ending normalization to '\n'
/// `buf` will be modified and the allocation will be resized if newline splicing
/// or line-ending changes happen.
/// caller retains ownership of `path`
/// To add the contents of an arbitrary reader as a Source, see addSourceFromReader
/// To add a file's contents given its path, see addSourceFromPath
pub fn addSourceFromOwnedBuffer(
    comp: *Compilation,
    buffer: []u8,
    path: []const u8,
    kind: Source.Kind,
) !Source {
    try comp.sources.ensureUnusedCapacity(comp.gpa, 1);

    var contents = buffer;

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

    for (contents) |byte| {
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
                            try comp.addDiagnostic(.{
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
                            try comp.addDiagnostic(.{
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

    if (i != contents.len) contents = try comp.gpa.realloc(contents, i);
    errdefer @compileError("errdefers in callers would possibly free the realloced slice using the original len");

    const source = Source{
        .id = sourceId,
        .path = dupedPath,
        .buffer = contents,
        .spliceLocs = spliceLocs,
        .kind = kind,
    };

    comp.sources.putAssumeCapacityNoClobber(dupedPath, source);
    return source;
}

/// Caller retains ownership of `path` and `buffer`.
/// Dupes the source buffer; if it is acceptable to modify the source buffer and possibly resize
/// the allocation, please use `addSourceFromOwnedBuffer`
pub fn addSourceFromBuffer(comp: *Compilation, path: []const u8, buffer: []const u8) !Source {
    if (comp.sources.get(path)) |some| return some;
    if (@as(u64, buffer.len) > std.math.maxInt(u32)) return error.StreamTooLong;

    const contents = try comp.gpa.dupe(u8, buffer);
    errdefer comp.gpa.free(contents);

    return comp.addSourceFromOwnedBuffer(contents, path, .User);
}

pub fn addSourceFromPath(comp: *Compilation, path: []const u8) !Source {
    return comp.addSourceFromPathExtra(path, .User);
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
pub fn addSourceFromPathExtra(comp: *Compilation, path: []const u8, kind: Source.Kind) !Source {
    if (comp.sources.get(path)) |some| return some;

    if (std.mem.indexOfScalar(u8, path, 0) != null)
        return error.FileNotFound;

    const file = try std.fs.cwd().openFile(path, .{});
    defer file.close();

    const contents = file.readToEndAlloc(comp.gpa, std.math.maxInt(u32)) catch |err| switch (err) {
        error.FileTooBig => return error.StreamTooLong,
        else => |e| return e,
    };
    errdefer comp.gpa.free(contents);

    return comp.addSourceFromOwnedBuffer(contents, path, kind);
}

pub const IncludeDirIterator = struct {
    comp: *const Compilation,
    /// An optional Source.ID representing the current working directory's source.
    cwdSourceID: ?Source.ID,
    /// Index tracking the next include directory to iterate over.
    includeDirsIndex: usize = 0,
    /// Index tracking the next system include directory to iterate over.
    sysIncludeDirsIndex: usize = 0,
    triedMSCwd: bool = false,

    const FoundSource = struct {
        path: []const u8,
        kind: Source.Kind,
    };

    /// Retrieves the next include directory path.
    /// If a current working directory source ID is set, it returns its directory path
    /// and clears the source ID. Then it iterates over the include directories and system
    /// include directories of the Compilation instance until all are visited.
    /// @return  The next directory path or null if there are no more directories.
    fn next(self: *IncludeDirIterator) ?FoundSource {
        // If cwdSourceID is set, return the directory of the corresponding source and unset it.
        if (self.cwdSourceID) |sourceID| {
            self.cwdSourceID = null;
            const path = self.comp.getSource(sourceID).path;
            return .{ .path = std.fs.path.dirname(path) orelse ".", .kind = .User };
        }

        // Iterate over the include directories and return the next one if available.
        if (self.includeDirsIndex < self.comp.includeDirs.items.len) {
            defer self.includeDirsIndex += 1;
            return .{ .path = self.comp.includeDirs.items[self.includeDirsIndex], .kind = .System };
        }

        // Iterate over the system include directories and return the next one if available.
        if (self.sysIncludeDirsIndex < self.comp.systemIncludeDirs.items.len) {
            defer self.sysIncludeDirsIndex += 1;
            return .{ .path = self.comp.systemIncludeDirs.items[self.sysIncludeDirsIndex], .kind = .User };
        }

        if (self.comp.msCwdSourceId) |sourceId| {
            if (self.triedMSCwd) return null;
            self.triedMSCwd = true;
            const path = self.comp.getSource(sourceId).path;
            return .{ .path = std.fs.path.dirname(path) orelse ".", .kind = .User };
        }

        // If no more directories are left, return null.
        return null;
    }

    /// Returned value's path field must be freed by allocator
    fn nextWithFile(self: *IncludeDirIterator, filename: []const u8, allocator: Allocator) !?FoundSource {
        while (self.next()) |found| {
            const path = try std.fs.path.join(allocator, &.{ found.path, filename });
            if (self.comp.langOpts.msExtensions) {
                for (path) |*c| {
                    if (c.* == '\\') c.* = '/';
                }
            }
            return .{ .path = path, .kind = found.kind };
        }
        return null;
    }

    /// Advance the iterator until it finds an include directory that matches
    /// the directory which contains `source`.
    fn skipUntilDirMatch(self: *IncludeDirIterator, source: Source.ID) void {
        const path = self.comp.getSource(source).path;
        const includerPath = std.fs.path.dirname(path) orelse ".";
        while (self.next()) |found| {
            if (std.mem.eql(u8, includerPath, found.path)) break;
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
) !bool {
    const cwd = std.fs.cwd();
    if (std.fs.path.isAbsolute(filename)) {
        if (which == .Next) return false;
        return !std.meta.isError(cwd.access(filename, .{}));
    }

    const cwdSourceID = getCwdSourceID(includerTokenSource, includeType, which);
    var it = IncludeDirIterator{ .comp = comp, .cwdSourceID = cwdSourceID };

    if (which == .Next)
        it.skipUntilDirMatch(includerTokenSource);

    var stackFallback = std.heap.stackFallback(PathBufferStackLimit, comp.gpa);
    const sfAllocator = stackFallback.get();

    while (try it.nextWithFile(filename, sfAllocator)) |found| {
        defer sfAllocator.free(found.path);
        if (!std.meta.isError(cwd.access(found.path, .{}))) return true;
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

fn getFileContents(comp: *Compilation, path: []const u8, limit: ?u32) ![]const u8 {
    if (std.mem.indexOfScalar(u8, path, 0) != null)
        return error.FileNotFound;

    const file = try std.fs.cwd().openFile(path, .{});
    defer file.close();

    var buffer = std.ArrayList(u8).init(comp.gpa);
    defer buffer.deinit();

    const max = limit orelse std.math.maxInt(u32);
    file.reader().readAllArrayList(&buffer, max) catch |e| switch (e) {
        error.StreamTooLong => if (limit == null) return e,
        else => return e,
    };

    return buffer.toOwnedSlice();
}

pub fn findEmbed(
    comp: *Compilation,
    filename: []const u8,
    includerTokenSource: Source.ID,
    /// angle bracket vs quotes
    includeType: IncludeType,
    limit: ?u32,
) !?[]const u8 {
    if (std.fs.path.isAbsolute(filename)) {
        return if (comp.getFileContents(filename, limit)) |some|
            some
        else |err| switch (err) {
            error.OutOfMemory => |e| return e,
            else => null,
        };
    }

    const cwdSourceId = switch (includeType) {
        .Quotes => includerTokenSource,
        .AngleBrackets => null,
    };
    var it = IncludeDirIterator{ .comp = comp, .cwdSourceID = cwdSourceId };

    var stackFallback = std.heap.stackFallback(PathBufferStackLimit, comp.gpa);
    const sfAllocator = stackFallback.get();

    while (try it.nextWithFile(filename, sfAllocator)) |found| {
        defer sfAllocator.free(found.path);
        if (comp.getFileContents(found.path, limit)) |some|
            return some
        else |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            else => {},
        }
    }
    return null;
}

pub fn findInclude(
    comp: *Compilation,
    filename: []const u8,
    includeToken: Token, // include token belong to which source
    includeType: IncludeType, // angle bracket or quotes
    which: WhichInclude, // include or include_next
) !?Source {
    if (std.fs.path.isAbsolute(filename)) {
        if (which == .Next) return null;
        return if (comp.addSourceFromPath(filename)) |some|
            return some
        else |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            else => null,
        };
    }

    const cwdSourceID = getCwdSourceID(includeToken.source, includeType, which);
    var it = IncludeDirIterator{ .comp = comp, .cwdSourceID = cwdSourceID };

    if (which == .Next)
        it.skipUntilDirMatch(includeToken.source);

    var stackFallback = std.heap.stackFallback(PathBufferStackLimit, comp.gpa);
    const sfAllocator = stackFallback.get();

    while (try it.nextWithFile(filename, sfAllocator)) |found| {
        defer sfAllocator.free(found.path);
        if (comp.addSourceFromPathExtra(found.path, found.kind)) |some| {
            if (it.triedMSCwd) {
                try comp.addDiagnostic(.{
                    .tag = .ms_search_rule,
                    .extra = .{ .str = some.path },
                    .loc = .{ .id = includeToken.source, .byteOffset = includeToken.start, .line = includeToken.line },
                }, &.{});
            }
            return some;
        } else |err| switch (err) {
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

pub fn hasBuiltin(comp: *const Compilation, name: []const u8) bool {
    if (std.mem.eql(u8, name, "__builtin_va_arg") or
        std.mem.eql(u8, name, "__builtin_choose_expr") or
        std.mem.eql(u8, name, "__builtin_bitoffsetof") or
        std.mem.eql(u8, name, "__builtin_offsetof") or
        std.mem.eql(u8, name, "__builtin_types_compatible_p")) return true;

    const builtin = Builtin.fromName(name) orelse return false;
    return comp.hasBuiltinFunction(builtin);
}

pub fn hasBuiltinFunction(comp: *const Compilation, builtin: Builtin) bool {
    if (!Target.builtinEnabled(comp.target, builtin.properties.target_set)) return false;

    switch (builtin.properties.language) {
        .all_languages => return true,
        .all_ms_languages => return comp.langOpts.emulate == .msvc,
        .gnu_lang, .all_gnu_languages => return comp.langOpts.standard.isGNU(),
    }
}

pub const CharUnitSize = enum(u32) {
    @"1" = 1,
    @"2" = 2,
    @"4" = 4,

    pub fn Type(comptime self: CharUnitSize) type {
        return switch (self) {
            .@"1" => u8,
            .@"2" => u16,
            .@"4" => u32,
        };
    }
};

pub const addDiagnostic = Diagnostics.add;

test "addSourceFromReader" {
    const Test = struct {
        fn addSourceFromReader(str: []const u8, expected: []const u8, warningCount: u32, splices: []const u32) !void {
            var comp = Compilation.init(std.testing.allocator);
            defer comp.deinit();

            var stream = std.io.fixedBufferStream(str);
            const reader = stream.reader();
            const source = try comp.addSourceFromReader(reader, "path", .User);

            try std.testing.expectEqualStrings(expected, source.buffer);
            try std.testing.expectEqual(warningCount, @as(u32, @intCast(comp.diagnostics.list.items.len)));
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

test "ignore BOM at beginning of file" {
    const BOM = "\xEF\xBB\xBF";

    const Test = struct {
        fn run(buf: []const u8) !void {
            var comp = Compilation.init(std.testing.allocator);
            defer comp.deinit();

            var buff = std.io.fixedBufferStream(buf);
            const source = try comp.addSourceFromReader(buff.reader(), "file.c", .User);
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
