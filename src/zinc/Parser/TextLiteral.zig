const std = @import("std");
const mem = std.mem;

const Compilation = @import("../Basic/Compilation.zig");
const Diagnostics = @import("../Basic/Diagnostics.zig");
const TokenType = @import("../Basic//TokenType.zig").TokenType;
const QualType = @import("../AST/TypeStore.zig").QualType;
const Source = @import("../Basic/Source.zig");

pub const Item = union(enum) {
    /// decoded hex or character escape
    value: u32,
    /// validated unicode codepoint
    codepoint: u21,
    /// CharLiteral in the sourct text is not utf8 encoded
    improperlyEncoded: []const u8,
    /// 1 or more unescaped bytes
    utf8Text: std.unicode.Utf8View,
};

pub const Kind = enum {
    char,
    wide,
    utf8,
    utf16,
    utf32,
    /// Errorr kind that halts parsing
    unterminated,

    pub fn classify(id: TokenType, context: enum { StringLiteral, CharLiteral }) ?Kind {
        return switch (context) {
            .StringLiteral => switch (id) {
                .StringLiteral => .char,
                .StringLiteralWide => .wide,
                .StringLiteralUTF_8 => .utf8,
                .StringLiteralUTF_16 => .utf16,
                .StringLiteralUTF_32 => .utf32,
                .UnterminatedStringLiteral => .unterminated,
                else => null,
            },
            .CharLiteral => switch (id) {
                .CharLiteral => .char,
                .CharLiteralWide => .wide,
                .CharLiteralUTF_8 => .utf8,
                .CharLiteralUTF_16 => .utf16,
                .CharLiteralUTF_32 => .utf32,
                else => null,
            },
        };
    }

    /// Largest unicode codepoint that can be represented by this character kind
    /// May be smaller than the largest value that can be represented.
    /// For example u8 char literals may only specify 0-127 via literals or
    /// character escapes, but may specify up to \xFF via hex escapes.
    pub fn maxCodepoint(kind: Kind, comp: *const Compilation) u21 {
        return @intCast(switch (kind) {
            .char => std.math.maxInt(u7),
            .wide => @min(0x10FFFF, comp.wcharMax()),
            .utf8 => std.math.maxInt(u7),
            .utf16 => std.math.maxInt(u16),
            .utf32 => 0x10FFFF,
            .unterminated => unreachable,
        });
    }

    /// Largest integer that can be represented by this character kind
    pub fn maxInt(kind: Kind, comp: *const Compilation) u32 {
        return @intCast(switch (kind) {
            .char, .utf8 => std.math.maxInt(u8),
            .wide => comp.wcharMax(),
            .utf16 => std.math.maxInt(u16),
            .utf32 => std.math.maxInt(u32),
            .unterminated => unreachable,
        });
    }

    /// Should only be called for string literals. Determines the result kind of two adjacent string
    /// literals
    pub fn concat(self: Kind, other: Kind) !Kind {
        if (self == .unterminated or other == .unterminated) return .unterminated;
        if (self == other) return self; // can always concat with own kind
        if (self == .char) return other; // char + X -> X
        if (other == .char) return self; // X + char -> X
        return error.CannotConcat;
    }

    pub fn contentSlice(kind: Kind, delimited: []const u8) []const u8 {
        const end = delimited.len - 1; // remove trailing quote
        return switch (kind) {
            .char => delimited[1..end],
            .wide => delimited[2..end],
            .utf8 => delimited[3..end],
            .utf16 => delimited[2..end],
            .utf32 => delimited[2..end],
            .unterminated => unreachable,
        };
    }

    /// The C type of a character literal of this kind
    pub fn charLiteralType(kind: Kind, comp: *const Compilation) QualType {
        return switch (kind) {
            .char => .int,
            .wide => comp.typeStore.wchar,
            .utf8 => .uchar,
            .utf16 => comp.typeStore.uintLeast16Ty,
            .utf32 => comp.typeStore.uintLeast32Ty,
            .unterminated => unreachable,
        };
    }

    /// The size of a character unit for a string literal of this kind
    pub fn charUnitSize(kind: Kind, comp: *const Compilation) Compilation.CharUnitSize {
        return switch (kind) {
            .char => .@"1",
            .wide => switch (comp.typeStore.wchar.sizeof(comp)) {
                2 => .@"2",
                4 => .@"4",
                else => unreachable,
            },
            .utf8 => .@"1",
            .utf16 => .@"2",
            .utf32 => .@"4",
            .unterminated => unreachable,
        };
    }

    /// Required alignment within zinc (on compiler host) for writing to Interner.strings.
    pub fn internalStorageAlignment(kind: Kind, comp: *const Compilation) usize {
        return switch (kind.charUnitSize(comp)) {
            inline else => |size| @alignOf(size.Type()),
        };
    }

    pub fn elementType(kind: Kind, comp: *const Compilation) QualType {
        return switch (kind) {
            .unterminated => unreachable,
            .char => .char,
            .utf8 => if (comp.langOpts.hasChar8_t()) .uchar else .char,
            else => kind.charLiteralType(comp),
        };
    }
};

pub const Ascii = struct {
    value: u7,

    pub fn init(value: anytype) Ascii {
        return .{ .value = @intCast(value) };
    }

    pub fn format(ctx: Ascii, w: anytype, fmtStr: []const u8) !usize {
        const i = std.mem.indexOf(u8, fmtStr, "{c}").?;
        try w.writeAll(fmtStr[0..i]);

        if (std.ascii.isPrint(ctx.value)) {
            try w.writeByte(ctx.value);
        } else {
            var buf: [3]u8 = undefined;
            const str = std.fmt.bufPrint(&buf, "x{x}", .{std.fmt.fmtSliceHexLower(&.{ctx.value})}) catch unreachable;
            try w.writeAll(str);
        }
        return i;
    }
};

pub const Parser = struct {
    comp: *const Compilation,
    literal: []const u8,
    i: usize = 0,
    kind: Kind,
    maxCodepoint: u21,
    loc: Source.Location,
    expansionLocs: []const Source.Location,
    /// We only want to issue a max of 1 error per char literal
    errored: bool = false,

    fn prefixLen(self: *const Parser) usize {
        return switch (self.kind) {
            .unterminated => unreachable,
            .char => 0,
            .utf8 => 2,
            .wide, .utf16, .utf32 => 1,
        };
    }

    const Diagnostic = struct {
        fmt: []const u8,
        kind: Diagnostics.Message.Kind,
        opt: ?Diagnostics.Option = null,
        extension: bool = false,

        pub const illegal_char_encoding_error: Diagnostic = .{
            .fmt = "illegal character encoding in character literal",
            .kind = .@"error",
        };

        pub const illegal_char_encoding_warning: Diagnostic = .{
            .fmt = "illegal character encoding in character literal",
            .kind = .warning,
            .opt = .@"invalid-source-encoding",
        };

        pub const missing_hex_escape: Diagnostic = .{
            .fmt = "\\{c} used with no following hex digits",
            .kind = .@"error",
        };

        pub const escape_sequence_overflow: Diagnostic = .{
            .fmt = "escape sequence out of range",
            .kind = .@"error",
        };

        pub const incomplete_universal_character: Diagnostic = .{
            .fmt = "incomplete universal character name",
            .kind = .@"error",
        };

        pub const invalid_universal_character: Diagnostic = .{
            .fmt = "invalid universal character",
            .kind = .@"error",
        };

        pub const char_too_large: Diagnostic = .{
            .fmt = "character too large for enclosing character literal type",
            .kind = .@"error",
        };

        pub const ucn_basic_char_error: Diagnostic = .{
            .fmt = "character '{c}' cannot be specified by a universal character name",
            .kind = .@"error",
        };

        pub const ucn_basic_char_warning: Diagnostic = .{
            .fmt = "specifying character '{c}' with a universal character name is incompatible with C standards before C23",
            .kind = .off,
            .opt = .@"pre-c23-compat",
        };

        pub const ucn_control_char_error: Diagnostic = .{
            .fmt = "universal character name refers to a control character",
            .kind = .@"error",
        };

        pub const ucn_control_char_warning: Diagnostic = .{
            .fmt = "universal character name referring to a control character is incompatible with C standards before C23",
            .kind = .off,
            .opt = .@"pre-c23-compat",
        };

        pub const c89_ucn_in_literal: Diagnostic = .{
            .fmt = "universal character names are only valid in C99 or later",
            .kind = .warning,
            .opt = .unicode,
        };

        const non_standard_escape_char: Diagnostic = .{
            .fmt = "use of non-standard escape character '\\{c}'",
            .kind = .off,
            .extension = true,
        };

        pub const unknown_escape_sequence: Diagnostic = .{
            .fmt = "unknown escape sequence '\\{c}'",
            .kind = .warning,
            .opt = .@"unknown-escape-sequence",
        };

        pub const four_char_char_literal: Diagnostic = .{
            .fmt = "multi-character character constant",
            .opt = .@"four-char-constants",
            .kind = .off,
        };

        pub const multichar_literal_warning: Diagnostic = .{
            .fmt = "multi-character character constant",
            .kind = .warning,
            .opt = .multichar,
        };

        pub const invalid_multichar_literal: Diagnostic = .{
            .fmt = "{s} character literals may not contain multiple characters",
            .kind = .@"error",
        };

        pub const char_lit_too_wide: Diagnostic = .{
            .fmt = "character constant too long for its type",
            .kind = .warning,
        };

        // pub const wide_multichar_literal: Diagnostic = .{
        //     .fmt = "extraneous characters in character constant ignored",
        //     .kind = .warning,
        // };
    };

    pub fn err(p: *Parser, diagnostic: Diagnostic, args: anytype) !void {
        if (p.errored) return;
        defer p.errored = true;
        try p.warn(diagnostic, args);
    }

    pub fn warn(p: *Parser, diagnostic: Diagnostic, args: anytype) !void {
        if (p.errored) return;
        if (p.comp.diagnostics.effectiveKind(diagnostic) == .off) return;

        var sf = std.heap.stackFallback(1024, p.comp.gpa);
        var buf = std.ArrayList(u8).init(sf.get());
        defer buf.deinit();

        try formatArgs(buf.writer(), diagnostic.fmt, args);

        try p.comp.diagnostics.addWithLocation(p.comp, .{
            .kind = diagnostic.kind,
            .text = buf.items,
            .opt = diagnostic.opt,
            .extension = diagnostic.extension,
            .location = p.loc.expand(p.comp),
        }, p.expansionLocs, true);
    }

    fn formatArgs(w: anytype, fmt: []const u8, args: anytype) !void {
        var i: usize = 0;
        inline for (std.meta.fields(@TypeOf(args))) |arg_info| {
            const arg = @field(args, arg_info.name);
            i += switch (@TypeOf(arg)) {
                []const u8 => try Diagnostics.formatString(w, fmt[i..], arg),
                Ascii => try arg.format(w, fmt[i..]),
                else => switch (@typeInfo(@TypeOf(arg))) {
                    .int, .comptime_int => try Diagnostics.formatInt(w, fmt[i..], arg),
                    .pointer => try Diagnostics.formatString(w, fmt[i..], arg),
                    else => unreachable,
                },
            };
        }
        try w.writeAll(fmt[i..]);
    }

    pub fn next(p: *Parser) !?Item {
        if (p.i >= p.literal.len) return null;

        const start = p.i;
        if (p.literal[start] != '\\') {
            p.i = mem.indexOfScalarPos(u8, p.literal, start + 1, '\\') orelse p.literal.len;
            const unescapedSlice = p.literal[start..p.i];

            const view = std.unicode.Utf8View.init(unescapedSlice) catch {
                if (p.kind != .char) {
                    try p.err(.illegal_char_encoding_error, .{});
                    return null;
                }
                try p.warn(.illegal_char_encoding_warning, .{});
                return .{ .improperlyEncoded = p.literal[start..p.i] };
            };
            return .{ .utf8Text = view };
        }

        switch (p.literal[start + 1]) {
            'u', 'U' => return try p.parseUnicodeEscape(),
            else => return try p.parseEscapedChar(),
        }
    }

    fn parseUnicodeEscape(p: *Parser) !?Item {
        const start = p.i;

        std.debug.assert(p.literal[p.i] == '\\');

        const kind = p.literal[p.i + 1];
        std.debug.assert(kind == 'u' or kind == 'U');

        p.i += 2;
        if (p.i >= p.literal.len or !std.ascii.isHex(p.literal[p.i])) {
            try p.err(.missing_hex_escape, .{Ascii.init(kind)});
            return null;
        }
        const expected_len: usize = if (kind == 'u') 4 else 8;
        var overflowed = false;
        var count: usize = 0;
        var value: u32 = 0;

        for (p.literal[p.i..], 0..) |c, i| {
            if (i == expected_len) break;

            const char = std.fmt.charToDigit(c, 16) catch break;

            value, const overflow = @shlWithOverflow(value, 4);
            overflowed = overflowed or overflow != 0;
            value |= char;
            count += 1;
        }
        p.i += expected_len;

        if (overflowed) {
            p.loc.byteOffset += @intCast(start + p.prefixLen());
            try p.err(.escape_sequence_overflow, .{});
            return null;
        }

        if (count != expected_len) {
            try p.err(.incomplete_universal_character, .{});
            return null;
        }

        if (value > std.math.maxInt(u21) or !std.unicode.utf8ValidCodepoint(@intCast(value))) {
            p.loc.byteOffset += @intCast(start + p.prefixLen());
            try p.err(.invalid_universal_character, .{});
            return null;
        }

        if (value > p.maxCodepoint) {
            try p.err(.char_too_large, .{});
            return null;
        }

        if (value < 0xA0 and (value != '$' and value != '@' and value != '`')) {
            const isError = !p.comp.langOpts.standard.atLeast(.c23);
            if (value >= 0x20 and value <= 0x7F) {
                if (isError) {
                    try p.err(.ucn_basic_char_error, .{Ascii.init(value)});
                } else if (!p.comp.langOpts.standard.atLeast(.c23)) {
                    try p.warn(.ucn_basic_char_warning, .{Ascii.init(value)});
                }
            } else {
                if (isError) {
                    try p.err(.ucn_control_char_error, .{});
                } else if (!p.comp.langOpts.standard.atLeast(.c23)) {
                    try p.warn(.ucn_control_char_warning, .{});
                }
            }
        }

        if (!p.comp.langOpts.standard.atLeast(.c99))
            try p.warn(.c89_ucn_in_literal, .{});

        return .{ .codepoint = @intCast(value) };
    }

    fn parseEscapedChar(p: *Parser) !Item {
        p.i += 1;
        const c = p.literal[p.i];
        defer if (c != 'x' and (c < '0' or c > '7')) {
            p.i += 1;
        };

        switch (c) {
            '\n' => unreachable, // removed by line splicing
            '\r' => unreachable, // removed by line splicing
            '\'', '\"', '\\', '?' => return .{ .value = c },
            'n' => return .{ .value = '\n' },
            'r' => return .{ .value = '\r' },
            't' => return .{ .value = '\t' },
            'a' => return .{ .value = 0x07 },
            'b' => return .{ .value = 0x08 },
            'e', 'E' => {
                p.loc.byteOffset += @intCast(p.i);
                try p.warn(.non_standard_escape_char, .{Ascii.init(c)});
                return .{ .value = 0x1B };
            },
            'f' => return .{ .value = 0x0C },
            'v' => return .{ .value = 0x0B },
            'x' => return .{ .value = try p.parseNumberEscape(.hex) },
            '0'...'7' => return .{ .value = try p.parseNumberEscape(.octal) },
            'u', 'U' => unreachable, // handled by parseUnicodeEscape
            '(', '{', '[', '%' => {
                p.loc.byteOffset += @intCast(p.i);
                try p.warn(.non_standard_escape_char, .{Ascii.init(c)});
                return .{ .value = c };
            },
            else => {
                p.loc.byteOffset += @intCast(p.i);
                try p.warn(.unknown_escape_sequence, .{Ascii.init(c)});
                return .{ .value = c };
            },
        }
    }

    fn parseNumberEscape(p: *Parser, base: EscapeBase) !u32 {
        var val: u32 = 0;
        var count: usize = 0;
        var overflowed = false;
        const start = p.i;
        defer p.i += count;

        const slice = switch (base) {
            .octal => p.literal[p.i..@min(p.literal.len, p.i + 3)], // max 3 chars
            .hex => blk: {
                p.i += 1;
                break :blk p.literal[p.i..];
            },
        };

        for (slice) |c| {
            const char = std.fmt.charToDigit(c, @intFromEnum(base)) catch break;
            val, const overflow = @shlWithOverflow(val, base.log2());
            if (overflow != 0) overflowed = true;
            val += char;
            count += 1;
        }

        if (overflowed or val > p.kind.maxInt(p.comp)) {
            p.loc.byteOffset += @intCast(start + p.prefixLen());
            try p.err(.escape_sequence_overflow, .{});
            return 0;
        }

        if (count == 0) {
            std.debug.assert(base == .hex);
            try p.err(.missing_hex_escape, .{Ascii.init('c')});
        }

        return val;
    }
};

const EscapeBase = enum(u8) {
    octal = 8,
    hex = 16,

    fn log2(base: EscapeBase) u4 {
        return switch (base) {
            .octal => 3,
            .hex => 4,
        };
    }
};
