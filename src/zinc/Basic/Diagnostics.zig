const std = @import("std");
const Allocator = std.mem.Allocator;

const Compilation = @import("Compilation.zig");
const LangOpts = @import("../Basic/LangOpts.zig");
const Source = @import("Source.zig");
const TokenType = @import("TokenType.zig").TokenType;

pub const Message = struct {
    kind: Kind,
    text: []const u8,
    opt: ?Option = null,
    extension: bool = false,
    location: ?Source.ExpandedLocation,

    effectiveKind: Kind = .off,

    pub const Kind = enum {
        off,
        note,
        warning,
        @"error",
        @"fatal error",
    };

    pub fn write(
        msg: Message,
        w: *std.Io.Writer,
        config: std.Io.tty.Config,
        details: bool,
    ) std.Io.tty.Config.SetColorError!void {
        try config.setColor(w, .bold);
        if (msg.location) |loc| {
            try w.print("{s}:{d}:{d}: ", .{ loc.path, loc.lineNo, loc.col });
        }
        switch (msg.effectiveKind) {
            .@"fatal error", .@"error" => try config.setColor(w, .bright_red),
            .note => try config.setColor(w, .bright_cyan),
            .warning => try config.setColor(w, .bright_magenta),
            .off => unreachable,
        }
        try w.print("{s}: ", .{@tagName(msg.effectiveKind)});

        try config.setColor(w, .white);
        try w.writeAll(msg.text);
        if (msg.opt) |some| {
            if (msg.effectiveKind == .@"error" and msg.kind != .@"error") {
                try w.print(" [-Werror,-W{s}]", .{@tagName(some)});
            } else if (msg.effectiveKind != .note) {
                try w.print(" [-W{s}]", .{@tagName(some)});
            }
        } else if (msg.extension) {
            if (msg.effectiveKind == .@"error") {
                try w.writeAll(" [-Werror,-Wpedantic]");
            } else if (msg.effectiveKind != msg.kind) {
                try w.writeAll(" [-Wpedantic]");
            }
        }

        if (!details or msg.location == null) {
            try w.writeAll("\n");
            try config.setColor(w, .reset);
        } else {
            const loc = msg.location.?;
            const trailer = if (loc.endWithSplice) "\\ " else "";
            try config.setColor(w, .reset);
            try w.print("\n{s}{s}\n", .{ loc.line, trailer });
            try w.splatByteAll(' ', loc.width);
            try config.setColor(w, .bold);
            try config.setColor(w, .bright_green);
            try w.writeAll("^\n");
            try config.setColor(w, .reset);
        }
        try w.flush();
    }
};

pub const Option = enum {
    @"unsupported-pragma",
    @"c99-extensions",
    @"implicit-int",
    @"duplicate-decl-specifier",
    @"missing-declaration",
    @"extern-initializer",
    @"implicit-function-declaration",
    @"unused-value",
    @"unreachable-code",
    @"unknown-warning-option",
    @"gnu-empty-struct",
    @"gnu-alignof-expression",
    @"macro-redefined",
    @"generic-qual-type",
    multichar,
    @"pointer-integer-compare",
    @"compare-distinct-pointer-types",
    @"literal-conversion",
    @"cast-qualifiers",
    @"array-bounds",
    @"int-conversion",
    @"pointer-type-mismatch",
    @"c23-extensions",
    @"incompatible-pointer-types",
    @"excess-initializers",
    @"division-by-zero",
    @"initializer-overrides",
    @"incompatible-pointer-types-discards-qualifiers",
    @"unknown-attributes",
    @"ignored-attributes",
    @"builtin-macro-redefined",
    @"gnu-label-as-value",
    @"malformed-warning-check",
    @"#pragma-messages",
    @"newline-eof",
    @"empty-translation-unit",
    @"implicitly-unsigned-literal",
    @"c99-compat",
    @"unicode-zero-width",
    @"unicode-homoglyph",
    unicode,
    @"return-type",
    @"dollar-in-identifier-extension",
    @"unknown-pragmas",
    @"predefined-identifier-outside-function",
    @"many-braces-around-scalar-init",
    uninitialized,
    @"gnu-statement-expression",
    @"gnu-imaginary-constant",
    @"gnu-complex-integer",
    @"ignored-qualifiers",
    @"integer-overflow",
    @"extra-semi",
    @"gnu-binary-literal",
    @"variadic-macros",
    varargs,
    @"#warnings",
    @"deprecated-declarations",
    @"backslash-newline-escape",
    @"pointer-to-int-cast",
    @"gnu-case-range",
    @"c++-compat",
    vla,
    @"float-overflow-conversion",
    @"float-zero-conversion",
    @"float-conversion",
    @"gnu-folding-constant",
    undef,
    @"ignored-pragmas",
    @"gnu-include-next",
    @"include-next-outside-header",
    @"include-next-absolute-path",
    @"enum-too-large",
    @"fixed-enum-extension",
    @"designated-init",
    @"attribute-warning",
    @"invalid-noreturn",
    @"zero-length-array",
    @"old-style-flexible-struct",
    @"gnu-zero-variadic-macro-arguments",
    @"main-return-type",
    @"expansion-to-defined",
    @"bit-int-extension",
    @"keyword-macro",
    @"pointer-arith",
    @"sizeof-array-argument",
    @"pre-c23-compat",
    @"pointer-bool-conversion",
    @"string-conversion",
    @"gnu-auto-type",
    @"gnu-pointer-arith",
    @"gnu-union-cast",
    @"pointer-sign",
    @"fuse-ld-path",
    @"language-extension-token",
    @"complex-component-init",
    @"microsoft-include",
    @"microsoft-end-of-file",
    @"invalid-source-encoding",
    @"four-char-constants",
    @"unknown-escape-sequence",
    @"invalid-pp-token",
    @"deprecated-non-prototype",
    @"duplicate-embed-param",
    @"unsupported-embed-param",
    @"unused-result",
    normalized,
    @"shift-count-negative",
    @"shift-count-overflow",
    @"constant-conversion",
    @"sign-conversion",
    @"address-of-packed-member",
    nonnull,
    @"atomic-access",
    @"gnu-designator",
    @"empty-body",
    @"nullability-extension",
    nullability,
    @"microsoft-flexible-array",
    @"out-of-scope-function",

    /// GNU extensions
    pub const gnu = [_]Option{
        .@"gnu-empty-struct",
        .@"gnu-alignof-expression",
        .@"gnu-label-as-value",
        .@"gnu-statement-expression",
        .@"gnu-imaginary-constant",
        .@"gnu-complex-integer",
        .@"gnu-binary-literal",
        .@"gnu-case-range",
        .@"gnu-folding-constant",
        .@"gnu-include-next",
        .@"gnu-zero-variadic-macro-arguments",
        .@"gnu-auto-type",
        .@"gnu-pointer-arith",
        .@"gnu-union-cast",
        .@"gnu-designator",
        .@"zero-length-array",
    };

    /// Clang extensions
    pub const clang = [_]Option{
        .@"fixed-enum-extension",
        .@"bit-int-extension",
        .@"nullability-extension",
    };

    /// Microsoft extensions
    pub const microsoft = [_]Option{
        .@"microsoft-end-of-file",
        .@"microsoft-include",
        .@"microsoft-flexible-array",
    };

    pub const extra = [_]Option{
        .@"initializer-overrides",
        .@"ignored-qualifiers",
        .@"initializer-overrides",
        .@"expansion-to-defined",
        .@"fuse-ld-path",
    };

    pub const implicit = [_]Option{
        .@"implicit-int",
        .@"implicit-function-declaration",
    };

    pub const unused = [_]Option{
        .@"unused-value",
        .@"unused-result",
    };

    pub const most = implicit ++ unused ++ [_]Option{
        .@"initializer-overrides",
        .@"ignored-qualifiers",
        .@"initializer-overrides",
        .multichar,
        .@"return-type",
        .@"sizeof-array-argument",
        .uninitialized,
        .@"unknown-pragmas",
    };

    pub const all = most ++ [_]Option{
        .nonnull,
        .@"unreachable-code",
        .@"malformed-warning-check",
    };
};

pub const State = struct {
    // Treat all errors as fatal, set by -Wfatal-errors
    fatalErrors: bool = false,
    // Treat all warnings as errors, set by -Werror
    errorWarnings: bool = false,
    /// Enable all warnings, set by -Weverything
    enableAllWarnings: bool = false,
    /// Ignore all warnings, set by -w
    ignoreWarnings: bool = false,
    /// How to treat extension diagnostics, set by -Wpedantic
    extensions: Message.Kind = .off,
    /// How to treat individual options, set by -W<name>
    options: std.EnumMap(Option, Message.Kind) = .{},
};

const Diagnostics = @This();

output: union(enum) {
    toWriter: struct {
        writer: *std.Io.Writer,
        color: std.Io.tty.Config,
    },
    toList: struct {
        messages: std.ArrayListUnmanaged(Message) = .empty,
        arena: std.heap.ArenaAllocator,
    },
    ignore,
},

/// Force usage of color in output.
color: ?bool = null,
/// Include line of code in output.
details: bool = true,

state: State = .{},
/// Amount of error or fatal error messages that have been sent to `output`.
errors: u32 = 0,
/// Amount of warnings that have been sent to `output`.
warnings: u32 = 0,
// Total amount of diagnostics messages sent to `output`.
total: u32 = 0,
macroBacktraceLimit: u32 = 6,
/// If `effectiveKind` causes us to skip a diagnostic, this is temporarily set to
/// `true` to signal that associated notes should also be skipped.
hideNotes: bool = false,

pub fn deinit(d: *Diagnostics) void {
    switch (d.output) {
        .ignore => {},
        .toWriter => {},
        .toList => |*list| {
            list.messages.deinit(list.arena.child_allocator);
            list.arena.deinit();
        },
    }
}

/// Used by the __has_warning builtin macro.
pub fn warningExists(name: []const u8) bool {
    if (std.mem.eql(u8, name, "pedantic")) return true;
    inline for (comptime std.meta.declarations(Option)) |group| {
        if (std.mem.eql(u8, name, group.name)) return true;
    }
    return std.meta.stringToEnum(Option, name) != null;
}

pub fn set(d: *Diagnostics, name: []const u8, to: Message.Kind) Compilation.Error!void {
    if (std.mem.eql(u8, name, "pedantic")) {
        d.state.extensions = to;
        return;
    }
    if (std.meta.stringToEnum(Option, name)) |option| {
        d.state.options.put(option, to);
        return;
    }

    inline for (comptime std.meta.declarations(Option)) |group| {
        if (std.mem.eql(u8, name, group.name)) {
            for (@field(Option, group.name)) |option| {
                d.state.options.put(option, to);
            }
            return;
        }
    }

    var buf: [256]u8 = undefined;
    const slice = std.fmt.bufPrint(&buf, "unknown warning '{s}'", .{name}) catch &buf;

    try d.add(.{
        .text = slice,
        .kind = .warning,
        .opt = .@"unknown-warning-option",
        .location = null,
    });
}

pub fn effectiveKind(d: *Diagnostics, message: anytype) Message.Kind {
    if (d.hideNotes and message.kind == .note) return .off;

    // -w disregards explicit kind set with -W<name>
    if (d.state.ignoreWarnings and message.kind == .warning) {
        d.hideNotes = true;
        return .off;
    }

    var kind = message.kind;

    // Get explicit kind set by -W<name>=
    var setExplicit = false;
    if (message.opt) |option| {
        if (d.state.options.get(option)) |explicit| {
            kind = explicit;
            setExplicit = true;
        }
    }

    // Use extension diagnostic behavior if not set explicitly.
    if (message.extension and !setExplicit) {
        kind = @enumFromInt(@max(@intFromEnum(kind), @intFromEnum(d.state.extensions)));
    }

    // Make diagnostic a warning if -Weverything is set.
    if (kind == .off and d.state.enableAllWarnings) kind = .warning;

    // Upgrade warnigns to errors if -Werror is set
    if (kind == .warning and d.state.errorWarnings) kind = .@"error";

    // Upgrade errors to fatal errors if -Wfatal-errors is set
    if (kind == .@"error" and d.state.fatalErrors) kind = .@"fatal error";

    if (kind == .off) d.hideNotes = true;

    return kind;
}

pub fn add(d: *Diagnostics, msg: Message) Compilation.Error!void {
    var copy = msg;
    copy.effectiveKind = d.effectiveKind(msg);

    if (copy.effectiveKind == .off) return;
    try d.addMessage(copy);
    if (copy.effectiveKind == .@"fatal error") return error.FatalError;
}

pub fn addWithLocation(
    d: *Diagnostics,
    comp: *const Compilation,
    msg: Message,
    expansionLocs: []const Source.Location,
    noteMsgLoc: bool,
) Compilation.Error!void {
    var copy = msg;
    copy.effectiveKind = d.effectiveKind(msg);
    if (copy.effectiveKind == .off) return;
    if (expansionLocs.len != 0) copy.location = expansionLocs[expansionLocs.len - 1].expand(comp);
    try d.addMessage(copy);

    if (expansionLocs.len != 0) {
        // Add macro backtrace notes in reverse order omitting from the middle if needed.
        var i = expansionLocs.len - 1;
        const half = d.macroBacktraceLimit / 2;
        const limit = if (i < d.macroBacktraceLimit) 0 else i - half;
        while (i > limit) {
            i -= 1;
            try d.addMessage(.{
                .kind = .note,
                .effectiveKind = .note,
                .text = "expanded from here",
                .location = expansionLocs[i].expand(comp),
            });
        }
        if (limit != 0) {
            var buf: [256]u8 = undefined;
            try d.addMessage(.{
                .kind = .note,
                .effectiveKind = .note,
                .text = std.fmt.bufPrint(
                    &buf,
                    "(skipping {d} expansions in backtrace; use -fmacro-backtrace-limit=0 to see all)",
                    .{expansionLocs.len - d.macroBacktraceLimit},
                ) catch unreachable,
                .location = null,
            });
            i = half -| 1;
            while (i > 0) {
                i -= 1;
                try d.addMessage(.{
                    .kind = .note,
                    .effectiveKind = .note,
                    .text = "expanded from here",
                    .location = expansionLocs[i].expand(comp),
                });
            }
        }

        if (noteMsgLoc) {
            try d.addMessage(.{
                .kind = .note,
                .effectiveKind = .note,
                .text = "expanded from here",
                .location = msg.location.?,
            });
        }
    }
    if (copy.kind == .@"fatal error") return error.FatalError;
}

pub fn formatArgs(w: *std.Io.Writer, fmt: []const u8, args: anytype) std.Io.Writer.Error!void {
    var i: usize = 0;
    inline for (std.meta.fields(@TypeOf(args))) |arg_info| {
        const arg = @field(args, arg_info.name);
        i += switch (@TypeOf(arg)) {
            []const u8 => try formatString(w, fmt[i..], arg),
            else => switch (@typeInfo(@TypeOf(arg))) {
                .int, .comptime_int => try Diagnostics.formatInt(w, fmt[i..], arg),
                .pointer => try Diagnostics.formatString(w, fmt[i..], arg),
                else => comptime unreachable,
            },
        };
    }
    try w.writeAll(fmt[i..]);
}

pub fn templateIndex(w: *std.Io.Writer, fmt: []const u8, template: []const u8) std.Io.Writer.Error!usize {
    const i = std.mem.indexOf(u8, fmt, template) orelse {
        if (@import("builtin").mode == .Debug) {
            std.debug.panic("template `{s}` not found in format string `{s}`", .{ template, fmt });
        }
        try w.print("template `{s}` not found in format string `{s}` (this is a bug in arocc)", .{ template, fmt });
        return 0;
    };
    try w.writeAll(fmt[0..i]);
    return i + template.len;
}

pub fn formatString(w: *std.Io.Writer, fmt: []const u8, str: []const u8) std.Io.Writer.Error!usize {
    const i = templateIndex(w, fmt, "{s}");
    try w.writeAll(str);
    return i;
}

pub fn formatInt(w: *std.Io.Writer, fmt: []const u8, int: anytype) std.Io.Writer.Error!usize {
    const i = templateIndex(w, fmt, "{d}");
    try w.printInt(int, 10, .lower, .{});
    return i;
}

fn addMessage(d: *Diagnostics, msg: Message) Compilation.Error!void {
    std.debug.assert(msg.effectiveKind != .off);
    switch (msg.effectiveKind) {
        .off => unreachable,
        .@"error", .@"fatal error" => d.errors += 1,
        .warning => d.warnings += 1,
        .note => {},
    }
    d.total += 1;
    d.hideNotes = false;

    switch (d.output) {
        .ignore => {},
        .toWriter => |writer| {
            var config = writer.color;
            if (d.color == false) config = .no_color;
            if (d.color == true and config == .no_color) config = .escape_codes;
            msg.write(writer.writer, config, d.details) catch {
                return error.FatalError;
            };
        },
        .toList => |*list| {
            const arena = list.arena.allocator();
            try list.messages.append(list.arena.child_allocator, .{
                .kind = msg.kind,
                .effectiveKind = msg.effectiveKind,
                .text = try arena.dupe(u8, msg.text),
                .opt = msg.opt,
                .extension = msg.extension,
                .location = msg.location,
            });
        },
    }
}
