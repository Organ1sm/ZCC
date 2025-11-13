/// Compilation holds onto the entire state of the Program
/// and declared symbols as well and house all compiler operation
const std = @import("std");
const assert = std.debug.assert;
const Io = std.Io;

const backend = @import("backend");
const Interner = backend.Interner;
const CodeGenOptions = backend.CodeGenOptions;

const Builtins = @import("../Builtins.zig");
const Builtin = Builtins.Builtin;
const Diagnostics = @import("../Basic/Diagnostics.zig");
const LangOpts = @import("LangOpts.zig");
const Lexer = @import("../Lexer/Lexer.zig");
const Pragma = @import("../Lexer/Pragma.zig");
const Source = @import("Source.zig");
const StringInterner = @import("../Basic/StringInterner.zig");
const RecordLayout = @import("RecordLayout.zig");
const Target = @import("Target.zig");
const Token = @import("../Lexer/Token.zig").Token;
const TypeStore = @import("../AST/TypeStore.zig");
const Type = TypeStore.Type;
const QualType = TypeStore.QualType;

const Allocator = std.mem.Allocator;
const EpochSeconds = std.time.epoch.EpochSeconds;

const Compilation = @This();

pub const Error = error{
    /// A fatal error has ocurred and compilation has stopped.
    FatalError,
} || Allocator.Error;

pub const AddSourceError = Error || error{FileTooBig};

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

    /// UNIX timestamp to be used instead of the current date and time in the __DATE__ and __TIME__ macros, and instead of the
    /// file modification time in the __TIMESTAMP__ macro
    sourceDateEpoch: ?[]const u8 = null,

    pub const SourceEpoch = union(enum) {
        /// Represents system time when aro is invoked; used for __DATE__ and __TIME__ macros
        system: u64,
        /// Represents a user-provided time (typically via the SOURCE_DATE_EPOCH environment variable)
        /// used for __DATE__, __TIME__, and __TIMESTAMP__
        provided: u64,

        pub const default: @This() = .{ .provided = 0 };
    };

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

    pub fn sourceEpoch(self: *const Environment, io: Io) !SourceEpoch {
        const max_timestamp = 253402300799; // Dec 31 9999 23:59:59

        if (self.sourceDateEpoch) |epoch| {
            const parsed = std.fmt.parseInt(u64, epoch, 10) catch return error.InvalidEpoch;
            if (parsed > max_timestamp) return error.InvalidEpoch;
            return .{ .provided = parsed };
        } else {
            const timestamp = try Io.Clock.real.now(io);
            const seconds = std.math.cast(u64, timestamp.toSeconds()) orelse return error.InvalidEpoch;
            return .{ .system = std.math.clamp(seconds, 0, max_timestamp) };
        }
    }
};

gpa: Allocator,
/// Allocations in this arena live all the way until `Compilation.deinit`.
arena: Allocator,
io: Io,
cwd: std.fs.Dir,
diagnostics: *Diagnostics,

codegenOptions: CodeGenOptions = .default,
environment: Environment = .{},
sources: std.StringArrayHashMapUnmanaged(Source) = .empty,
/// Allocated into `gpa`, but keys are externally managed.
includeDirs: std.ArrayList([]const u8) = .empty,
/// Allocated into `gpa`, but keys are externally managed.
systemIncludeDirs: std.ArrayList([]const u8) = .empty,
target: std.Target = @import("builtin").target,
pragmaHandlers: std.StringArrayHashMapUnmanaged(*Pragma) = .empty,
langOpts: LangOpts = .{},
generatedBuffer: std.ArrayList(u8) = .empty,
builtins: Builtins = .{},

stringInterner: StringInterner = .{},
interner: Interner = .{},
typeStore: TypeStore = .{},

/// If this is not null, the directory containing the specified Source will be searched for includes
/// Used by MS extensions which allow searching for includes relative to the directory of the main source file.
msCwdSourceId: ?Source.ID = null,

pub fn init(gpa: Allocator, arena: Allocator, io: Io, diags: *Diagnostics, cwd: std.fs.Dir) Compilation {
    return .{
        .gpa = gpa,
        .arena = arena,
        .io = io,
        .diagnostics = diags,
        .cwd = cwd,
    };
}

/// Initialize Compilation with default environment,
/// pragma handlers and emulation mode set to target.
pub fn initDefault(gpa: Allocator, arena: Allocator, io: Io, diags: *Diagnostics, cwd: std.fs.Dir) !Compilation {
    var comp: Compilation = .{
        .gpa = gpa,
        .arena = arena,
        .io = io,
        .environment = try Environment.loadAll(gpa),
        .diagnostics = diags,
        .cwd = cwd,
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
    comp.includeDirs.deinit(comp.gpa);
    comp.systemIncludeDirs.deinit(comp.gpa);
    comp.pragmaHandlers.deinit(comp.gpa);
    comp.generatedBuffer.deinit(comp.gpa);
    comp.builtins.deinit(comp.gpa);
    comp.stringInterner.deinit(comp.gpa);
    comp.interner.deinit(comp.gpa);
    comp.environment.deinit(comp.gpa);
    comp.typeStore.deinit(comp.gpa);
    comp.* = undefined;
}

pub fn internString(comp: *Compilation, str: []const u8) !StringInterner.StringId {
    return comp.stringInterner.intern(comp.gpa, str);
}

/// Which set of system defines to generate via generateBuiltinMacros
pub const SystemDefinesMode = enum {
    /// Only define macros required by the C standard (date/time macros and those beginning with `__STDC`)
    NoSystemDefines,
    /// Define the standard set of system macros
    IncludeSystemDefines,
};

/// Generate builtin macros that will be available to each source file.
pub fn generateSystemDefines(comp: *Compilation, w: *Io.Writer) !void {
    const define = struct {
        fn define(_w: *Io.Writer, name: []const u8) !void {
            try _w.print("#define {s} 1\n", .{name});
        }
    }.define;
    const defineStd = struct {
        fn defineStd(_w: *Io.Writer, name: []const u8, isGnu: bool) !void {
            if (isGnu) try define(_w, name);

            try _w.print(
                \\#define __{s} 1
                \\#define __{s}__ 1
                \\
            , .{ name, name });
        }
    }.defineStd;
    const ptrWidth = comp.target.ptrBitWidth();
    const isGnu = comp.langOpts.standard.isGNU();

    if (comp.langOpts.gnucVersion > 0) {
        try w.print("#define __GNUC__ {d}\n", .{comp.langOpts.gnucVersion / 10_000});
        try w.print("#define __GNUC_MINOR__ {d}\n", .{comp.langOpts.gnucVersion / 100 % 100});
        try w.print("#define __GNUC_PATCHLEVEL__ {d}\n", .{comp.langOpts.gnucVersion % 100});
    }

    switch (comp.target.os.tag) {
        .linux => try defineStd(w, "linux", isGnu),
        .windows => {
            try define(w, "_WIN32");
            if (ptrWidth == 64)
                try define(w, "_WIN64");

            if (comp.target.abi.isGnu()) {
                try defineStd(w, "WIN32", isGnu);
                try defineStd(w, "WINNT", isGnu);
                if (ptrWidth == 64) {
                    try defineStd(w, "WIN64", isGnu);
                }
                try define(w, "__MSVCRT__");
                try define(w, "__MINGW32__");
            } else if (comp.target.abi == .cygnus) {
                try define(w, "__CYGWIN__");
                if (ptrWidth == 64) {
                    try define(w, "__CYGWIN64__");
                } else {
                    try define(w, "__CYGWIN32__");
                }
            }

            if (comp.target.abi.isGnu() or comp.target.abi == .cygnus) {
                // MinGW and Cygwin define __declspec(a) to __attribute((a)).
                // Like Clang we make the define no op if -fdeclspec is enabled.
                if (comp.langOpts.declSpecAttrs) {
                    try w.writeAll("#define __declspec __declspec\n");
                } else {
                    try w.writeAll("#define __declspec(a) __attribute__((a))\n");
                }

                if (!comp.langOpts.msExtensions) {
                    // Provide aliases for the calling convention keywords.
                    for ([_][]const u8{ "cdecl", "stdcall", "fastcall", "thiscall" }) |keyword| {
                        try w.print(
                            \\#define _{[0]s} __attribute__((__{[0]s}__))
                            \\#define __{[0]s} __attribute__((__{[0]s}__))
                            \\
                        , .{keyword});
                    }
                }
            }
        },
        .uefi => try define(w, "__UEFI__"),
        .freebsd => try w.print("#define __FreeBSD__ {d}\n", .{comp.target.os.version_range.semver.min.major}),
        .netbsd => try define(w, "__NetBSD__"),
        .openbsd => try define(w, "__OpenBSD__"),
        .dragonfly => try define(w, "__DragonFly__"),
        .illumos => {
            try defineStd(w, "sun", isGnu);
            try define(w, "__illumos__");
        },

        .macos,
        .tvos,
        .ios,
        .driverkit,
        .visionos,
        .watchos,
        => try define(w, "__APPLE__"),

        .wasi => try define(w, "__wasi__"),
        .emscripten => try define(w, "__EMSCRIPTEN__"),
        .@"3ds" => try define(w, "__3DS__"),
        .vita => try define(w, "__vita__"),
        else => {},
    }

    // unix and other additional os macros
    switch (comp.target.os.tag) {
        .freebsd,
        .netbsd,
        .openbsd,
        .dragonfly,
        .linux,
        .haiku,
        .hurd,
        .illumos,
        .emscripten,
        => try defineStd(w, "unix", isGnu),

        .windows => if (comp.target.abi.isGnu() or comp.target.abi == .cygnus) {
            try defineStd(w, "unix", isGnu);
        },
        else => {},
    }

    if (comp.target.abi.isAndroid()) {
        try w.writeAll("#define __ANDROID__ 1\n");
    }

    switch (comp.target.cpu.arch) {
        .x86_64 => {
            try define(w, "__amd64__");
            try define(w, "__amd64");
            try define(w, "__x86_64__");
            try define(w, "__x86_64");

            if (comp.target.os.tag == .windows and comp.target.abi == .msvc) {
                try w.writeAll(
                    \\#define _M_X64 100
                    \\#define _M_AMD64 100
                    \\
                );
            }
        },

        .x86 => {
            try defineStd(w, "i3286", isGnu);

            if (comp.target.os.tag == .windows and comp.target.abi == .msvc) {
                try w.print("#define _M_IX86 {d}\n", .{blk: {
                    if (comp.target.cpu.model == &std.Target.x86.cpu.i386) break :blk 300;
                    if (comp.target.cpu.model == &std.Target.x86.cpu.i486) break :blk 400;
                    if (comp.target.cpu.model == &std.Target.x86.cpu.i586) break :blk 500;
                    break :blk @as(u32, 600);
                }});
            }
        },

        .mips,
        .mipsel,
        .mips64,
        .mips64el,
        => {
            try define(w, "__mips__");
            try define(w, "_mips");
        },

        .powerpc,
        .powerpcle,
        => {
            try define(w, "__powerpc__");
            try define(w, "__POWERPC__");
            try define(w, "__ppc__");
            try define(w, "__PPC__");
            try define(w, "_ARCH_PPC");
        },

        .powerpc64,
        .powerpc64le,
        => {
            try define(w, "__powerpc");
            try define(w, "__powerpc__");
            try define(w, "__powerpc64__");
            try define(w, "__POWERPC__");
            try define(w, "__ppc__");
            try define(w, "__ppc64__");
            try define(w, "__PPC__");
            try define(w, "__PPC64__");
            try define(w, "_ARCH_PPC");
            try define(w, "_ARCH_PPC64");
        },

        .sparc64 => {
            try defineStd(w, "sparc", isGnu);
            try define(w, "__sparc_v9__");
            try define(w, "__arch64__");
            if (comp.target.os.tag != .illumos) {
                try define(w, "__sparc64__");
                try define(w, "__sparc_v9__");
                try define(w, "__sparcv9__");
            }
        },

        .sparc => {
            try defineStd(w, "sparc", isGnu);
            if (comp.target.os.tag == .illumos) {
                try define(w, "__sparcv8");
            }
        },

        .arm, .armeb, .thumb, .thumbeb => {
            try define(w, "__arm__");
            try define(w, "__arm");
            if (comp.target.cpu.arch.isThumb()) {
                try define(w, "__thumb__");
            }
        },
        .aarch64, .aarch64_be => {
            try define(w, "__aarch64__");
            if (comp.target.os.tag == .macos) {
                try define(w, "__AARCH64_SIMD__");
                if (ptrWidth == 32) {
                    try define(w, "__ARM64_ARCH_8_32__");
                } else {
                    try define(w, "__ARM64_ARCH_8__");
                }
                try define(w, "__ARM_NEON__");
                try define(w, "__arm64");
                try define(w, "__arm64__");
            }
            if (comp.target.os.tag == .windows and comp.target.abi == .msvc) {
                try w.writeAll("#define _M_ARM64 100\n");
            }
        },

        .msp430 => {
            try define(w, "MSP430");
            try define(w, "__MSP430__");
        },
        else => {},
    }

    if (ptrWidth == 64 and comp.target.cTypeBitSize(.long) == 32) {
        try define(w, "_LP64");
        try define(w, "__LP64__");
    } else if (ptrWidth == 32 and comp.target.cTypeBitSize(.long) == 32 and
        comp.target.cTypeBitSize(.int) == 32)
    {
        try define(w, "_ILP32");
        try define(w, "__ILP32__");
    }

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

    switch (comp.target.ofmt) {
        .elf => try define(w, "__ELF__"),
        .macho => try define(w, "__MACH__"),
        else => {},
    }

    if (comp.target.os.tag.isDarwin()) {
        try w.writeAll(
            \\#define __nonnull _Nonnull
            \\#define __null_unspecified _Null_unspecified
            \\#define __nullable _Nullable
            \\
        );
    }

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

    // TODO: Set these to target-specific constants depending on backend capabilities
    // For now they are just set to the "may be lock-free" value
    try w.writeAll(
        \\#define __ATOMIC_BOOL_LOCK_FREE 1
        \\#define __ATOMIC_CHAR_LOCK_FREE 1
        \\#define __ATOMIC_CHAR16_T_LOCK_FREE 1
        \\#define __ATOMIC_CHAR32_T_LOCK_FREE 1
        \\#define __ATOMIC_WCHAR_T_LOCK_FREE 1
        \\#define __ATOMIC_SHORT_LOCK_FREE 1
        \\#define __ATOMIC_INT_LOCK_FREE 1
        \\#define __ATOMIC_LONG_LOCK_FREE 1
        \\#define __ATOMIC_LLONG_LOCK_FREE 1
        \\#define __ATOMIC_POINTER_LOCK_FREE 1
        \\
    );
    if (comp.langOpts.hasChar8_t()) {
        try w.writeAll("#define __ATOMIC_CHAR8_T_LOCK_FREE 1\n");
    }

    //types
    if (comp.getCharSignedness() == .unsigned) try w.writeAll("#define __CHAR_UNSIGNED__ 1\n");
    try w.writeAll("#define __CHAR_BIT__ 8\n");

    // int maxs
    try comp.generateIntWidth(w, "BOOL", .bool);
    try comp.generateIntMaxAndWidth(w, "SCHAR", .schar);
    try comp.generateIntMaxAndWidth(w, "SHRT", .short);
    try comp.generateIntMaxAndWidth(w, "INT", .int);
    try comp.generateIntMaxAndWidth(w, "LONG", .long);
    try comp.generateIntMaxAndWidth(w, "LONG_LONG", .longlong);
    try comp.generateIntMaxAndWidth(w, "WCHAR", comp.typeStore.wchar);
    // try comp.generateIntMax(w, "WINT", comp.typeStore.wchar);
    try comp.generateIntMaxAndWidth(w, "INTMAX", comp.typeStore.intmax);
    try comp.generateIntMaxAndWidth(w, "SIZE", comp.typeStore.size);
    try comp.generateIntMaxAndWidth(w, "UINTMAX", try comp.typeStore.intmax.makeIntUnsigned(comp));
    try comp.generateIntMaxAndWidth(w, "PTRDIFF", comp.typeStore.ptrdiff);
    try comp.generateIntMaxAndWidth(w, "INTPTR", comp.typeStore.intptr);
    try comp.generateIntMaxAndWidth(w, "UINTPTR", try comp.typeStore.intptr.makeIntUnsigned(comp));

    // int widths
    try w.print("#define __BITINT_MAXWIDTH__ {d}\n", .{BitIntMaxBits});

    // sizeof types
    try comp.generateSizeofType(w, "__SIZEOF_FLOAT__", .float);
    try comp.generateSizeofType(w, "__SIZEOF_DOUBLE__", .double);
    try comp.generateSizeofType(w, "__SIZEOF_LONG_DOUBLE__", .longDouble);
    try comp.generateSizeofType(w, "__SIZEOF_SHORT__", .short);
    try comp.generateSizeofType(w, "__SIZEOF_INT__", .int);
    try comp.generateSizeofType(w, "__SIZEOF_LONG__", .long);
    try comp.generateSizeofType(w, "__SIZEOF_LONG_LONG__", .longlong);
    try comp.generateSizeofType(w, "__SIZEOF_POINTER__", .voidPointer);
    try comp.generateSizeofType(w, "__SIZEOF_PTRDIFF_T__", comp.typeStore.ptrdiff);
    try comp.generateSizeofType(w, "__SIZEOF_SIZE_T__", comp.typeStore.size);
    try comp.generateSizeofType(w, "__SIZEOF_WCHAR_T__", comp.typeStore.wchar);
    // try comp.generateSizeofType(w, "__SIZEOF_WINT_T__", .voidPointer);

    if (Target.hasInt128(comp.target)) {
        try comp.generateSizeofType(w, "__SIZEOF_INT128__", .int128);
    }

    // various int types
    try comp.generateTypeMacro(w, "__INTPTR_TYPE__", comp.typeStore.intptr);
    try comp.generateTypeMacro(w, "__UINTPTR_TYPE__", try comp.typeStore.intptr.makeIntUnsigned(comp));

    try comp.generateTypeMacro(w, "__INTMAX_TYPE__", comp.typeStore.intmax);
    try comp.generateSuffixMacro("__INTMAX", w, comp.typeStore.intptr);

    try comp.generateTypeMacro(w, "__UINTMAX_TYPE__", try comp.typeStore.intmax.makeIntUnsigned(comp));
    try comp.generateSuffixMacro("__UINTMAX", w, try comp.typeStore.intptr.makeIntUnsigned(comp));

    try comp.generateTypeMacro(w, "__PTRDIFF_TYPE__", comp.typeStore.ptrdiff);
    try comp.generateTypeMacro(w, "__SIZE_TYPE__", comp.typeStore.size);
    try comp.generateTypeMacro(w, "__WCHAR_TYPE__", comp.typeStore.wchar);
    try comp.generateTypeMacro(w, "__CHAR16_TYPE__", comp.typeStore.uintLeast16Ty);
    try comp.generateTypeMacro(w, "__CHAR32_TYPE__", comp.typeStore.uintLeast32Ty);

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

    switch (comp.codegenOptions.picLevel) {
        .none => {},
        .one, .two => {
            try w.print(
                \\#define __pic__ {0d}
                \\#define __PIC__ {0d}
                \\
            , .{@intFromEnum(comp.codegenOptions.picLevel)});
            if (comp.codegenOptions.isPie) {
                try w.print(
                    \\#define __pie__ {0d}
                    \\#define __PIE__ {0d}
                    \\
                , .{@intFromEnum(comp.codegenOptions.picLevel)});
            }
        },
    }
}

/// Generate builtin macros that will be available to each source file.
pub fn generateBuiltinMacros(comp: *Compilation, systemDefinesMode: SystemDefinesMode) AddSourceError!Source {
    try comp.typeStore.initNamedTypes(comp);

    var allocating: Io.Writer.Allocating = try .initCapacity(comp.gpa, 2 << 13);
    defer allocating.deinit();

    comp.writeBuiltinMacros(systemDefinesMode, &allocating.writer) catch |err| switch (err) {
        error.WriteFailed, error.OutOfMemory => return error.OutOfMemory,
    };

    if (allocating.written().len > std.math.maxInt(u32)) return error.FileTooBig;

    const contents = try allocating.toOwnedSlice();
    errdefer comp.gpa.free(contents);
    return comp.addSourceFromOwnedBuffer("<builtin>", contents, .User);
}

fn writeBuiltinMacros(comp: *Compilation, systemDefinesMode: SystemDefinesMode, w: *Io.Writer) !void {
    if (systemDefinesMode == .IncludeSystemDefines) {
        try w.writeAll(
            \\#define __VERSION__ "Zinc 
        ++ @import("backend").VersionStr ++ "\"\n" ++
            \\#define __Zinc__
            \\  
        );
    }

    try w.writeAll("#define __STDC__ 1\n");
    try w.print("#define __STDC_HOSTED__ {d}\n", .{@intFromBool(comp.target.os.tag != .freestanding)});

    // Standard macros
    try w.writeAll(
        \\#define __STDC_NO_COMPLEX__ 1
        \\#define __STDC_NO_THREADS__ 1
        \\#define __STDC_NO_VLA__ 1
        \\#define __STDC_UTF_16__ 1
        \\#define __STDC_UTF_32__ 1
        \\#define __STDC_EMBED_NOT_FOUND__ 0
        \\#define __STDC_EMBED_FOUND__ 1
        \\#define __STDC_EMBED_EMPTY__ 2
        \\
    );

    if (comp.langOpts.standard.StdCVersionMacro()) |stdcVersion| {
        try w.writeAll("#define __STDC_VERSION__ ");
        try w.writeAll(stdcVersion);
        try w.writeByte('\n');
    }

    if (systemDefinesMode == .IncludeSystemDefines) {
        try comp.generateSystemDefines(w);
    }
}

fn generateFloatMacros(w: *Io.Writer, prefix: []const u8, semantics: Target.FPSemantics, ext: []const u8) !void {
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

    try w.print("#define __{s}_DENORM_MIN__ {s}{s}\n", .{ prefix, denormMin, ext });
    try w.print("#define __{s}_HAS_DENORM__\n", .{prefix});
    try w.print("#define __{s}_DIG__ {d}\n", .{ prefix, digits });
    try w.print("#define __{s}_DECIMAL_DIG__ {d}\n", .{ prefix, decimalDigits });

    try w.print("#define __{s}_EPSILON__ {s}{s}\n", .{ prefix, epsilon, ext });
    try w.print("#define __{s}_HAS_INFINITY__\n", .{prefix});
    try w.print("#define __{s}_HAS_QUIET_NAN__\n", .{prefix});
    try w.print("#define __{s}_MANT_DIG__ {d}\n", .{ prefix, mantissaDigits });

    try w.print("#define __{s}_MAX_10_EXP__ {d}\n", .{ prefix, max10Exp });
    try w.print("#define __{s}_MAX_EXP__ {d}\n", .{ prefix, maxExp });
    try w.print("#define __{s}_MAX__ {s}{s}\n", .{ prefix, max, ext });

    try w.print("#define __{s}_MIN_10_EXP__ ({d})\n", .{ prefix, min10Exp });
    try w.print("#define __{s}_MIN_EXP__ ({d})\n", .{ prefix, minExp });
    try w.print("#define __{s}_MIN__ {s}{s}\n", .{ prefix, min, ext });
}

fn generateTypeMacro(comp: *const Compilation, w: *Io.Writer, name: []const u8, qt: QualType) !void {
    try w.print("#define {s} ", .{name});
    try qt.print(comp, w);
    try w.writeByte('\n');
}

pub fn hasHalfPrecisionFloatABI(comp: *const Compilation) bool {
    return comp.langOpts.allowHalfArgsAndReturns or Target.hasHalfPrecisionFloatABI(comp.target);
}

pub fn hasFloat128(comp: *const Compilation) bool {
    return Target.hasFloat128(comp.target);
}

pub fn float80Type(comp: *const Compilation) ?QualType {
    if (comp.langOpts.emulate != .gcc) return null;
    return Target.float80Type(comp.target);
}

fn generateFastOrLeastType(
    comp: *Compilation,
    bits: usize,
    kind: enum { least, fast },
    signedness: std.builtin.Signedness,
    w: *Io.Writer,
) !void {
    const ty = comp.intLeastN(bits, signedness); // defining the fast types as the least types is permitted

    var buffer: [32]u8 = undefined;
    const suffix = "_TYPE__";
    const baseName = switch (signedness) {
        .signed => "__INT_",
        .unsigned => "__UINT_",
    };
    const kindStr = switch (kind) {
        .fast => "FAST",
        .least => "LEAST",
    };

    const full = std.fmt.bufPrint(&buffer, "{s}{s}{d}{s}", .{
        baseName, kindStr, bits, suffix,
    }) catch unreachable;

    try comp.generateTypeMacro(w, full, ty);

    // remove "__" and "_TYPE__"
    const prefix = full[2 .. full.len - suffix.len];
    switch (signedness) {
        .signed => try comp.generateIntMaxAndWidth(w, prefix, ty),
        .unsigned => try comp.generateIntMax(w, prefix, ty),
    }
    try comp.generateFmt(prefix, w, ty);
}

fn generateFastAndLeastWidthTypes(comp: *Compilation, w: *Io.Writer) !void {
    const sizes = [_]usize{ 8, 16, 32, 64 };
    for (sizes) |size| {
        try comp.generateFastOrLeastType(size, .least, .signed, w);
        try comp.generateFastOrLeastType(size, .least, .unsigned, w);
        try comp.generateFastOrLeastType(size, .fast, .signed, w);
        try comp.generateFastOrLeastType(size, .fast, .unsigned, w);
    }
}

fn generateExactWidthTypes(comp: *Compilation, w: *Io.Writer) !void {
    try comp.generateExactWidthType(w, .schar);

    if (QualType.short.sizeof(comp) > QualType.char.sizeof(comp))
        try comp.generateExactWidthType(w, .short);

    if (QualType.int.sizeof(comp) > QualType.short.sizeof(comp))
        try comp.generateExactWidthType(w, .int);

    if (QualType.long.sizeof(comp) > QualType.int.sizeof(comp))
        try comp.generateExactWidthType(w, .long);

    if (QualType.longlong.sizeof(comp) > QualType.long.sizeof(comp))
        try comp.generateExactWidthType(w, .longlong);

    try comp.generateExactWidthType(w, .uchar);
    try comp.generateExactWidthIntMax(w, .uchar);
    try comp.generateExactWidthIntMax(w, .schar);

    if (QualType.short.sizeof(comp) > QualType.char.sizeof(comp)) {
        try comp.generateExactWidthType(w, .ushort);
        try comp.generateExactWidthIntMax(w, .ushort);
        try comp.generateExactWidthIntMax(w, .short);
    }

    if (QualType.int.sizeof(comp) > QualType.short.sizeof(comp)) {
        try comp.generateExactWidthType(w, .uint);
        try comp.generateExactWidthIntMax(w, .uint);
        try comp.generateExactWidthIntMax(w, .int);
    }

    if (QualType.long.sizeof(comp) > QualType.int.sizeof(comp)) {
        try comp.generateExactWidthType(w, .ulong);
        try comp.generateExactWidthIntMax(w, .ulong);
        try comp.generateExactWidthIntMax(w, .long);
    }

    if (QualType.longlong.sizeof(comp) > QualType.long.sizeof(comp)) {
        try comp.generateExactWidthType(w, .ulonglong);
        try comp.generateExactWidthIntMax(w, .ulonglong);
        try comp.generateExactWidthIntMax(w, .longlong);
    }
}

fn generateFmt(comp: *const Compilation, prefix: []const u8, w: *Io.Writer, qt: QualType) !void {
    const unsigned = qt.isUnsigned(comp);
    const modifier = qt.formatModifier(comp);
    const formats = if (unsigned) "ouxX" else "di";
    for (formats) |c|
        try w.print("#define {s}_FMT{c}__ \"{s}{c}\"\n", .{ prefix, c, modifier, c });
}

fn generateSuffixMacro(comp: *const Compilation, prefix: []const u8, w: *Io.Writer, qt: QualType) !void {
    return w.print("#define {s}_C_SUFFIX__ {s}\n", .{ prefix, qt.intValueSuffix(comp) });
}

/// Generate the following for ty:
///     Name macro (e.g. #define __UINT32_TYPE__ unsigned int)
///     Format strings (e.g. #define __UINT32_FMTu__ "u")
///     Suffix macro (e.g. #define __UINT32_C_SUFFIX__ U)
fn generateExactWidthType(comp: *Compilation, w: *Io.Writer, originalQt: QualType) !void {
    var qt = originalQt;
    const width = qt.sizeof(comp) * 8;
    const unsigned = qt.isUnsigned(comp);

    if (width == 16) {
        qt = if (unsigned) try comp.typeStore.int16.makeIntUnsigned(comp) else comp.typeStore.int16;
    } else if (width == 64) {
        qt = if (unsigned) try comp.typeStore.int64.makeIntUnsigned(comp) else comp.typeStore.int64;
    }

    var buffer: [16]u8 = undefined;
    const suffix = "_TYPE__";
    const full = std.fmt.bufPrint(&buffer, "{s}{d}{s}", .{
        if (unsigned) "__UINT" else "__INT", width, suffix,
    }) catch unreachable;

    try comp.generateTypeMacro(w, full, qt);

    // remove "_TYPE__"
    const prefix = full[0 .. full.len - suffix.len];
    try comp.generateFmt(prefix, w, qt);
    try comp.generateSuffixMacro(prefix, w, qt);
}

fn generateIntMax(comp: *const Compilation, w: *Io.Writer, name: []const u8, qt: QualType) !void {
    const unsigned = qt.isUnsigned(comp);
    const max: u128 = switch (qt.bitSizeof(comp)) {
        8 => if (unsigned) std.math.maxInt(u8) else std.math.maxInt(i8),
        16 => if (unsigned) std.math.maxInt(u16) else std.math.maxInt(i16),
        32 => if (unsigned) std.math.maxInt(u32) else std.math.maxInt(i32),
        64 => if (unsigned) std.math.maxInt(u64) else std.math.maxInt(i64),
        128 => if (unsigned) std.math.maxInt(u128) else std.math.maxInt(i128),
        else => unreachable,
    };
    try w.print("#define __{s}_MAX__ {d}{s}\n", .{ name, max, qt.intValueSuffix(comp) });
}

/// Largest value that can be stored in wchar_t
pub fn wcharMax(comp: *const Compilation) u32 {
    const unsigned = comp.typeStore.wchar.signedness(comp) == .unsigned;
    return switch (comp.typeStore.wchar.bitSizeof(comp)) {
        8 => if (unsigned) std.math.maxInt(u8) else std.math.maxInt(i8),
        16 => if (unsigned) std.math.maxInt(u16) else std.math.maxInt(i16),
        32 => if (unsigned) std.math.maxInt(u32) else std.math.maxInt(i32),
        else => unreachable,
    };
}

fn generateExactWidthIntMax(comp: *Compilation, w: *Io.Writer, orginalQt: QualType) !void {
    var qt = orginalQt;
    const bitCount: u8 = @intCast(qt.sizeof(comp) * 8);
    const unsigned = qt.isUnsigned(comp);

    if (bitCount == 64)
        qt = if (unsigned) try comp.typeStore.int64.makeIntUnsigned(comp) else comp.typeStore.int64;

    var nameBuffer: [6]u8 = undefined;
    const name = std.fmt.bufPrint(&nameBuffer, "{s}{d}", .{
        if (unsigned) "UINT" else "INT", bitCount,
    }) catch unreachable;

    return comp.generateIntMax(w, name, qt);
}

fn generateIntWidth(comp: *Compilation, w: *Io.Writer, name: []const u8, qt: QualType) !void {
    try w.print("#define __{s}_WIDTH__ {d}\n", .{ name, 8 * qt.sizeof(comp) });
}

fn generateIntMaxAndWidth(comp: *Compilation, w: *Io.Writer, name: []const u8, qt: QualType) !void {
    try comp.generateIntMax(w, name, qt);
    try comp.generateIntWidth(w, name, qt);
}

fn generateSizeofType(comp: *Compilation, w: *Io.Writer, name: []const u8, qt: QualType) !void {
    try w.print("#define {s} {d}\n", .{ name, qt.sizeof(comp) });
}

pub fn nextLargestIntSameSign(comp: *const Compilation, qt: QualType) ?QualType {
    assert(qt.isInt(comp));
    const candidates: [4]QualType = if (qt.isUnsigned(comp))
        .{ .short, .int, .long, .longlong }
    else
        .{ .ushort, .uint, .ulong, .ulonglong };
    const size = qt.sizeof(comp);
    for (candidates) |candidate| {
        if (candidate.sizeof(comp) > size) return candidate;
    }
    return null;
}

/// Smallest integer type with at least N bits
pub fn intLeastN(comp: *const Compilation, bits: usize, signedness: std.builtin.Signedness) QualType {
    const target = comp.target;
    // WebAssembly and Darwin use `long long` for `int_least64_t` and `int_fast64_t`.
    if (bits == 64 and (target.os.tag.isDarwin() or target.cpu.arch.isWasm()))
        return if (signedness == .signed) .longlong else .ulonglong;

    // AVR uses int for int_least16_t and int_fast16_t.
    if (bits == 16 and target.cpu.arch == .avr)
        return if (signedness == .signed) .int else .uint;

    const candidates: [5]QualType = switch (signedness) {
        .signed => .{ .schar, .short, .int, .long, .longlong },
        .unsigned => .{ .uchar, .ushort, .uint, .ulong, .ulonglong },
    };

    for (candidates) |qt| {
        if (qt.bitSizeof(comp) >= bits) return qt;
    } else unreachable;
}

/// Maximum size of an array, in bytes
pub fn maxArrayBytes(comp: *const Compilation) u64 {
    const maxBits = @min(61, comp.target.ptrBitWidth());
    return (@as(u64, 1) << @truncate(maxBits)) - 1;
}

/// If `enum E { ... }` syntax has a fixed underlying integer type regardless of the presence of
/// __attribute__((packed)) or the range of values of the corresponding enumerator constants,
/// specify it here.
/// TODO: likely incomplete
pub fn fixedEnumTagType(comp: *const Compilation) ?QualType {
    switch (comp.langOpts.emulate) {
        .msvc => return .int,
        .clang => if (comp.target.os.tag == .windows) return .int,
        .gcc => {},
    }
    return null;
}

pub fn getCharSignedness(comp: *const Compilation) std.builtin.Signedness {
    return comp.langOpts.charSignednessOverride orelse comp.target.cCharSignedness();
}

/// Add built-in zinc headers directory to system include paths
pub fn addBuiltinIncludeDir(comp: *Compilation, zincDir: []const u8) !void {
    const gpa = comp.gpa;
    const arena = comp.arena;
    try comp.systemIncludeDirs.ensureUnusedCapacity(gpa, 1);

    var searchPath = zincDir;
    while (std.fs.path.dirname(searchPath)) |dirname| : (searchPath = dirname) {
        var baseDir = std.fs.cwd().openDir(dirname, .{}) catch continue;
        defer baseDir.close();

        baseDir.access("include/stddef.h", .{}) catch continue;
        comp.systemIncludeDirs.appendAssumeCapacity(try std.fs.path.join(arena, &.{ dirname, "include" }));

        break;
    } else return error.ZincIncludeNotFound;
}

pub fn addSystemIncludeDir(comp: *Compilation, path: []const u8) !void {
    try comp.systemIncludeDirs.append(comp.gpa, try comp.arena.dupe(u8, path));
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

/// Creates a Source from `buf` and adds it to the Compilation
/// Performs newline splicing and line-ending normalization to '\n'
/// `buf` will be modified and the allocation will be resized if newline splicing
/// or line-ending changes happen.
/// caller retains ownership of `path`
/// To add a file's contents given its path, see addSourceFromPath
pub fn addSourceFromOwnedBuffer(
    comp: *Compilation,
    path: []const u8,
    buffer: []u8,
    kind: Source.Kind,
) !Source {
    assert(buffer.len <= std.math.maxInt(u32));
    try comp.sources.ensureUnusedCapacity(comp.gpa, 1);

    var contents = buffer;

    const dupedPath = try comp.gpa.dupe(u8, path);
    errdefer comp.gpa.free(dupedPath);

    var spliceList: std.ArrayList(u32) = .empty;
    defer spliceList.deinit(comp.gpa);

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
                        try spliceList.append(comp.gpa, i);
                        if (state == .trailing_ws) {
                            try comp.addNewlineEscapeError(path, buffer, spliceList.items, i, line);
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
                            try spliceList.append(comp.gpa, i);
                        }
                        if (state == .trailing_ws) {
                            try comp.addNewlineEscapeError(path, buffer, spliceList.items, i, line);
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

    const spliceLocs = try spliceList.toOwnedSlice(comp.gpa);
    errdefer comp.gpa.free(spliceLocs);

    if (i != contents.len) {
        var list: std.ArrayList(u8) = .{
            .items = contents[0..i],
            .capacity = contents.len,
        };
        contents = try list.toOwnedSlice(comp.gpa);
    }
    errdefer @compileError("errdefers in callers would possibly free the realloced slice using the original len");

    const source: Source = .{
        .id = sourceId,
        .path = dupedPath,
        .buffer = contents,
        .spliceLocs = spliceLocs,
        .kind = kind,
    };

    comp.sources.putAssumeCapacityNoClobber(dupedPath, source);
    return source;
}

fn addNewlineEscapeError(
    comp: *Compilation,
    path: []const u8,
    buffer: []const u8,
    spliceLocs: []const u32,
    byteOffset: u32,
    line: u32,
) !void {

    // Temporary source for getting the location for errors.
    var tempSource: Source = .{
        .path = path,
        .buffer = buffer,
        .id = undefined,
        .kind = undefined,
        .spliceLocs = spliceLocs,
    };

    const diagnostic: Diagnostic = .backslash_newline_escape;
    var loc = tempSource.getLineCol(.{ .id = undefined, .byteOffset = byteOffset, .line = line });
    loc.line = loc.line[0 .. loc.line.len - 1];
    loc.width += 1;
    loc.col += 1;

    try comp.diagnostics.add(.{
        .text = diagnostic.fmt,
        .kind = diagnostic.kind,
        .opt = diagnostic.opt,
        .location = loc,
    });
}

/// Caller retains ownership of `path` and `buffer`.
/// Dupes the source buffer; if it is acceptable to modify the source buffer and possibly resize
/// the allocation, please use `addSourceFromOwnedBuffer`
pub fn addSourceFromBuffer(comp: *Compilation, path: []const u8, buffer: []const u8) !Source {
    if (comp.sources.get(path)) |some| return some;
    if (@as(u64, buffer.len) > std.math.maxInt(u32)) return error.FileTooBig;

    const contents = try comp.gpa.dupe(u8, buffer);
    errdefer comp.gpa.free(contents);

    return comp.addSourceFromOwnedBuffer(path, contents, .User);
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

    const file = try comp.cwd.openFile(path, .{});
    defer file.close();

    return comp.addSourceFromFile(file, path, kind);
}

pub fn addSourceFromFile(comp: *Compilation, file: std.fs.File, path: []const u8, kind: Source.Kind) !Source {
    const contents = try comp.getFileContents(file, .unlimited);
    errdefer comp.gpa.free(contents);
    return comp.addSourceFromOwnedBuffer(path, contents, kind);
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
    if (std.mem.indexOfScalar(u8, filename, 0) != null) return false;
    if (std.fs.path.isAbsolute(filename)) {
        if (which == .Next) return false;
        return !std.meta.isError(comp.cwd.access(filename, .{}));
    }

    const cwdSourceID = getCwdSourceID(includerTokenSource, includeType, which);
    var it = IncludeDirIterator{ .comp = comp, .cwdSourceID = cwdSourceID };

    if (which == .Next)
        it.skipUntilDirMatch(includerTokenSource);

    var stackFallback = std.heap.stackFallback(PathBufferStackLimit, comp.gpa);
    const sfAllocator = stackFallback.get();

    while (try it.nextWithFile(filename, sfAllocator)) |found| {
        defer sfAllocator.free(found.path);
        if (!std.meta.isError(comp.cwd.access(found.path, .{}))) return true;
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

fn getPathContents(comp: *Compilation, path: []const u8, limit: std.Io.Limit) ![]u8 {
    if (std.mem.indexOfScalar(u8, path, 0) != null) {
        return error.FileNotFound;
    }

    const file = try comp.cwd.openFile(path, .{});
    defer file.close();
    return comp.getFileContents(file, limit);
}

fn getFileContents(comp: *Compilation, file: std.fs.File, limit: std.Io.Limit) ![]u8 {
    var fileBuffer: [4096]u8 = undefined;
    var fileReader = file.reader(comp.io, &fileBuffer);

    var allocating: std.Io.Writer.Allocating = .init(comp.gpa);
    defer allocating.deinit();

    if (fileReader.getSize()) |size| {
        const limitedSize = limit.minInt64(size);
        if (limitedSize > std.math.maxInt(u32)) return error.FileTooBig;
        try allocating.ensureUnusedCapacity(limitedSize);
    } else |_| {}

    var remaining = limit.min(.limited(std.math.maxInt(u32)));
    while (remaining.nonzero()) {
        const n = fileReader.interface.stream(&allocating.writer, remaining) catch |err| switch (err) {
            error.EndOfStream => return allocating.toOwnedSlice(),
            error.WriteFailed => return error.OutOfMemory,
            error.ReadFailed => return fileReader.err.?,
        };
        remaining = remaining.subtract(n).?;
    }
    if (limit == .unlimited) return error.FileTooBig;
    return allocating.toOwnedSlice();
}

pub fn findEmbed(
    comp: *Compilation,
    filename: []const u8,
    includerTokenSource: Source.ID,
    /// angle bracket vs quotes
    includeType: IncludeType,
    limit: Io.Limit,
) !?[]u8 {
    if (std.fs.path.isAbsolute(filename)) {
        if (comp.getPathContents(filename, limit)) |some| {
            errdefer comp.gpa.free(some);
            return some;
        } else |err| switch (err) {
            error.OutOfMemory => |e| return e,
            else => return null,
        }
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
        if (comp.getPathContents(found.path, limit)) |some| {
            errdefer comp.gpa.free(some);
            return some;
        } else |err| switch (err) {
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
                const diagnostic: Diagnostic = .ms_search_rule;
                try comp.diagnostics.add(.{
                    .text = diagnostic.fmt,
                    .kind = diagnostic.kind,
                    .opt = diagnostic.opt,
                    .extension = diagnostic.extension,
                    .location = (Source.Location{
                        .id = includeToken.source,
                        .byteOffset = includeToken.start,
                        .line = includeToken.line,
                    }).expand(comp),
                });
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

pub fn locSlice(comp: *const Compilation, loc: Source.Location) []const u8 {
    var tempLexer = Lexer{
        .buffer = comp.getSource(loc.id).buffer,
        .langOpts = comp.langOpts,
        .index = loc.byteOffset,
        .source = .generated,
    };

    const token = tempLexer.next();
    return tempLexer.buffer[token.start..token.end];
}

pub fn getSourceMTimeUncached(comp: *const Compilation, sourceId: Source.ID) ?u64 {
    const source = comp.getSource(sourceId);
    if (comp.cwd.statFile(source.path)) |stat| {
        return std.math.cast(u64, stat.mtime.toSeconds());
    } else |_| {
        return null;
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

pub const Diagnostic = struct {
    fmt: []const u8,
    kind: Diagnostics.Message.Kind,
    opt: ?Diagnostics.Option = null,
    extension: bool = false,

    pub const backslash_newline_escape: Diagnostic = .{
        .fmt = "backslash and newline separated by space",
        .kind = .warning,
        .opt = .@"backslash-newline-escape",
    };

    pub const ms_search_rule: Diagnostic = .{
        .fmt = "#include resolved using non-portable Microsoft search rules as: {s}",
        .kind = .warning,
        .opt = .@"microsoft-include",
        .extension = true,
    };

    pub const ctrl_z_eof: Diagnostic = .{
        .fmt = "treating Ctrl-Z as end-of-file is a Microsoft extension",
        .kind = .off,
        .opt = .@"microsoft-end-of-file",
        .extension = true,
    };
};

test "addSourceFromBuffer" {
    const Test = struct {
        fn addSourceFromBuffer(str: []const u8, expected: []const u8, warningCount: u32, splices: []const u32) !void {
            const allocator = std.testing.allocator;
            const io = std.testing.io;

            var arena: std.heap.ArenaAllocator = .init(allocator);
            defer arena.deinit();

            var diagnostics: Diagnostics = .{ .output = .ignore };
            var comp = Compilation.init(allocator, arena.allocator(), io, &diagnostics, std.fs.cwd());
            defer comp.deinit();

            const source = try comp.addSourceFromBuffer("path", str);

            try std.testing.expectEqualStrings(expected, source.buffer);
            try std.testing.expectEqual(warningCount, @as(u32, @intCast(diagnostics.warnings)));
            try std.testing.expectEqualSlices(u32, splices, source.spliceLocs);
        }

        fn withAllocationFailures(allocator: std.mem.Allocator) !void {
            var arena: std.heap.ArenaAllocator = .init(std.testing.allocator);
            defer arena.deinit();

            var diagnostics: Diagnostics = .{ .output = .ignore };
            var comp = Compilation.init(allocator, arena.allocator(), std.testing.io, &diagnostics, std.fs.cwd());
            defer comp.deinit();

            _ = try comp.addSourceFromBuffer("path", "spliced\\\nbuffer\n");
            _ = try comp.addSourceFromBuffer("path", "non-spliced buffer\n");
        }
    };
    try Test.addSourceFromBuffer("ab\\\nc", "abc", 0, &.{2});
    try Test.addSourceFromBuffer("ab\\\rc", "abc", 0, &.{2});
    try Test.addSourceFromBuffer("ab\\\r\nc", "abc", 0, &.{2});
    try Test.addSourceFromBuffer("ab\\ \nc", "abc", 1, &.{2});
    try Test.addSourceFromBuffer("ab\\\t\nc", "abc", 1, &.{2});
    try Test.addSourceFromBuffer("ab\\                     \t\nc", "abc", 1, &.{2});
    try Test.addSourceFromBuffer("ab\\\r \nc", "ab \nc", 0, &.{2});
    try Test.addSourceFromBuffer("ab\\\\\nc", "ab\\c", 0, &.{3});
    try Test.addSourceFromBuffer("ab\\   \r\nc", "abc", 1, &.{2});
    try Test.addSourceFromBuffer("ab\\ \\\nc", "ab\\ c", 0, &.{4});
    try Test.addSourceFromBuffer("ab\\\r\\\nc", "abc", 0, &.{ 2, 2 });
    try Test.addSourceFromBuffer("ab\\  \rc", "abc", 1, &.{2});
    try Test.addSourceFromBuffer("ab\\", "ab\\", 0, &.{});
    try Test.addSourceFromBuffer("ab\\\\", "ab\\\\", 0, &.{});
    try Test.addSourceFromBuffer("ab\\ ", "ab\\ ", 0, &.{});
    try Test.addSourceFromBuffer("ab\\\n", "ab", 0, &.{2});
    try Test.addSourceFromBuffer("ab\\\r\n", "ab", 0, &.{2});
    try Test.addSourceFromBuffer("ab\\\r", "ab", 0, &.{2});

    // carriage return normalization
    try Test.addSourceFromBuffer("ab\r", "ab\n", 0, &.{});
    try Test.addSourceFromBuffer("ab\r\r", "ab\n\n", 0, &.{});
    try Test.addSourceFromBuffer("ab\r\r\n", "ab\n\n", 0, &.{});
    try Test.addSourceFromBuffer("ab\r\r\n\r", "ab\n\n\n", 0, &.{});
    try Test.addSourceFromBuffer("\r\\", "\n\\", 0, &.{});
    try Test.addSourceFromBuffer("\\\r\\", "\\", 0, &.{0});

    try std.testing.checkAllAllocationFailures(std.testing.allocator, Test.withAllocationFailures, .{});
}

test "addSourceFromBuffer - exhaustive check for carriage return elimination" {
    const allocator = std.testing.allocator;
    const io = std.testing.io;

    var arena: std.heap.ArenaAllocator = .init(allocator);
    defer arena.deinit();

    const alphabet = [_]u8{ '\r', '\n', ' ', '\\', 'a' };
    const alen = alphabet.len;
    var buffer: [alen]u8 = @splat(alphabet[0]);

    var diagnostics: Diagnostics = .{ .output = .ignore };
    var comp = Compilation.init(allocator, arena.allocator(), io, &diagnostics, std.fs.cwd());
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
        fn run(arena: Allocator, buf: []const u8) !void {
            var diagnostics: Diagnostics = .{ .output = .ignore };
            var comp = Compilation.init(std.testing.allocator, arena, std.testing.io, &diagnostics, std.fs.cwd());
            defer comp.deinit();

            const source = try comp.addSourceFromBuffer("file.c", buf);
            const expectedOutput = if (std.mem.startsWith(u8, buf, BOM)) buf[BOM.len..] else buf;
            try std.testing.expectEqualStrings(expectedOutput, source.buffer);
        }
    };

    var arenaState: std.heap.ArenaAllocator = .init(std.testing.allocator);
    defer arenaState.deinit();
    const arena = arenaState.allocator();

    try Test.run(arena, BOM);
    try Test.run(arena, BOM ++ "x");
    try Test.run(arena, "x" ++ BOM);
    try Test.run(arena, BOM ++ " ");
    try Test.run(arena, BOM ++ "\n");
    try Test.run(arena, BOM ++ "\\");

    try Test.run(arena, BOM[0..1] ++ "x");
    try Test.run(arena, BOM[0..2] ++ "x");
    try Test.run(arena, BOM[1..] ++ "x");
    try Test.run(arena, BOM[2..] ++ "x");
}
