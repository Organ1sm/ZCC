const std = @import("std");
const Compilation = @import("../Basic/Compilation.zig");
const Diagnostics = @import("../Basic/Diagnostics.zig");
const TokenType = @import("../Basic//TokenType.zig").TokenType;
const Type = @import("../AST/Type.zig");
const LangOpts = @import("../Basic/LangOpts.zig");
const mem = std.mem;

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
            .wide => @min(0x10FFFF, comp.types.wchar.maxInt(comp)),
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
            .wide => comp.types.wchar.maxInt(comp),
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
    pub fn charLiteralType(kind: Kind, comp: *const Compilation) Type {
        return switch (kind) {
            .char => Type.Int,
            .wide => comp.types.wchar,
            .utf8 => Type.UChar,
            .utf16 => comp.types.uintLeast16Ty,
            .utf32 => comp.types.uintLeast32Ty,
            .unterminated => unreachable,
        };
    }

    /// The size of a character unit for a string literal of this kind
    pub fn charUnitSize(kind: Kind, comp: *const Compilation) Compilation.CharUnitSize {
        return switch (kind) {
            .char => .@"1",
            .wide => switch (comp.types.wchar.sizeof(comp).?) {
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

    /// Required alignment within zcc (on compiler host); not on compilation target
    pub fn internalStorageAlignment(kind: Kind, comp: *const Compilation) usize {
        return switch (kind.charUnitSize(comp)) {
            inline else => |size| @alignOf(size.Type()),
        };
    }

    pub fn elementType(kind: Kind, comp: *const Compilation) Type {
        return switch (kind) {
            .unterminated => unreachable,
            .char => Type.Char,
            .utf8 => if (comp.langOpts.hasChar8_t()) Type.UChar else Type.Char,
            else => kind.charLiteralType(comp),
        };
    }
};

const CharDiagnostics = struct {
    tag: Diagnostics.Tag,
    extra: Diagnostics.Message.Extra,
};

pub const Parser = struct {
    literal: []const u8,
    i: usize = 0,
    kind: Kind,
    maxCodepoint: u21,
    /// We only want to issue a max of 1 error per char literal
    errored: bool = false,
    errors: std.BoundedArray(CharDiagnostics, 4) = .{},
    comp: *const Compilation,

    pub fn init(literal: []const u8, kind: Kind, maxCodepoint: u21, comp: *const Compilation) Parser {
        return .{
            .literal = literal,
            .comp = comp,
            .kind = kind,
            .maxCodepoint = maxCodepoint,
        };
    }

    fn prefixLen(self: *const Parser) usize {
        return switch (self.kind) {
            .unterminated => unreachable,
            .char => 0,
            .utf8 => 2,
            .wide, .utf16, .utf32 => 1,
        };
    }

    pub fn err(self: *Parser, tag: Diagnostics.Tag, extra: Diagnostics.Message.Extra) void {
        if (self.errored) return;
        self.errored = true;
        const diagnostic = .{ .tag = tag, .extra = extra };
        self.errors.append(diagnostic) catch {
            _ = self.errors.pop();
            self.errors.append(diagnostic) catch unreachable;
        };
    }

    pub fn warn(self: *Parser, tag: Diagnostics.Tag, extra: Diagnostics.Message.Extra) void {
        if (self.errored) return;
        self.errors.append(.{ .tag = tag, .extra = extra }) catch {};
    }

    pub fn next(self: *Parser) ?Item {
        if (self.i >= self.literal.len) return null;

        const start = self.i;
        if (self.literal[start] != '\\') {
            self.i = mem.indexOfScalarPos(u8, self.literal, start + 1, '\\') orelse self.literal.len;
            const unescapedSlice = self.literal[start..self.i];

            const view = std.unicode.Utf8View.init(unescapedSlice) catch {
                if (self.kind != .char) {
                    self.err(.illegal_char_encoding_error, .{ .none = {} });
                    return null;
                }
                self.warn(.illegal_char_encoding_warning, .{ .none = {} });
                return .{ .improperlyEncoded = self.literal[start..self.i] };
            };
            return .{ .utf8Text = view };
        }

        switch (self.literal[start + 1]) {
            'u', 'U' => return self.parseUnicodeEscape(),
            else => return self.parseEscapedChar(),
        }
    }

    fn parseUnicodeEscape(self: *Parser) ?Item {
        const start = self.i;

        std.debug.assert(self.literal[self.i] == '\\');

        const kind = self.literal[self.i + 1];
        std.debug.assert(kind == 'u' or kind == 'U');

        self.i += 2;
        if (self.i >= self.literal.len or !std.ascii.isHex(self.literal[self.i])) {
            self.err(.missing_hex_escape, .{ .ascii = @intCast(kind) });
            return null;
        }
        const expectedLen: usize = if (kind == 'u') 4 else 8;
        var overflowed = false;
        var count: usize = 0;
        var val: u32 = 0;

        for (self.literal[self.i..], 0..) |c, i| {
            if (i == expectedLen) break;

            const char = std.fmt.charToDigit(c, 16) catch {
                break;
            };

            val, const overflow = @shlWithOverflow(val, 4);
            overflowed = overflowed or overflow != 0;
            val |= char;
            count += 1;
        }
        self.i += expectedLen;

        if (overflowed) {
            self.err(.escape_sequence_overflow, .{ .unsigned = start + self.prefixLen() });
            return null;
        }

        if (count != expectedLen) {
            self.err(.incomplete_universal_character, .{ .none = {} });
            return null;
        }

        if (val > std.math.maxInt(u21) or !std.unicode.utf8ValidCodepoint(@intCast(val))) {
            self.err(.invalid_universal_character, .{ .unsigned = start + self.prefixLen() });
            return null;
        }

        if (val > self.maxCodepoint)
            self.err(.char_too_large, .{ .none = {} });

        if (val < 0xA0 and (val != '$' and val != '@' and val != '`')) {
            const isError = !self.comp.langOpts.standard.atLeast(.c2x);
            if (val >= 0x20 and val <= 0x7F) {
                if (isError)
                    self.err(.ucn_basic_char_error, .{ .ascii = @intCast(val) })
                else
                    self.warn(.ucn_basic_char_warning, .{ .ascii = @intCast(val) });
            } else {
                if (isError)
                    self.err(.ucn_control_char_error, .{ .none = {} })
                else
                    self.warn(.ucn_control_char_warning, .{ .none = {} });
            }
        }

        self.warn(.c89_ucn_in_literal, .{ .none = {} });
        return .{ .codepoint = @intCast(val) };
    }

    fn parseEscapedChar(self: *Parser) Item {
        self.i += 1;
        const c = self.literal[self.i];
        defer if (c != 'x' and (c < '0' or c > '7')) {
            self.i += 1;
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
                self.warn(.non_standard_escape_char, .{ .invalidEscape = .{ .char = c, .offset = @intCast(self.i) } });
                return .{ .value = 0x1B };
            },
            'f' => return .{ .value = 0x0C },
            'v' => return .{ .value = 0x0B },
            'x' => return .{ .value = self.parseNumberEscape(.hex) },
            '0'...'7' => return .{ .value = self.parseNumberEscape(.octal) },
            'u', 'U' => unreachable, // handled by parseUnicodeEscape
            '(', '{', '[', '%' => {
                self.warn(.non_standard_escape_char, .{ .invalidEscape = .{ .char = c, .offset = @intCast(self.i) } });
                return .{ .value = c };
            },
            else => {
                self.warn(.unknown_escape_sequence, .{ .invalidEscape = .{ .char = c, .offset = @intCast(self.i) } });
                return .{ .value = c };
            },
        }
    }

    fn parseNumberEscape(self: *Parser, base: EscapeBase) u32 {
        var val: u32 = 0;
        var count: usize = 0;
        var overflowed = false;
        const start = self.i;
        defer self.i += count;

        const slice = switch (base) {
            .octal => self.literal[self.i..@min(self.literal.len, self.i + 3)], // max 3 chars
            .hex => blk: {
                self.i += 1;
                break :blk self.literal[self.i..];
            },
        };

        for (slice) |c| {
            const char = std.fmt.charToDigit(c, @intFromEnum(base)) catch break;
            val, const overflow = @shlWithOverflow(val, base.log2());
            if (overflow != 0) overflowed = true;
            val += char;
            count += 1;
        }

        if (overflowed or val > self.kind.maxInt(self.comp)) {
            self.err(.escape_sequence_overflow, .{ .unsigned = start + self.prefixLen() });
            return 0;
        }

        if (count == 0) {
            std.debug.assert(base == .hex);
            self.err(.missing_hex_escape, .{ .ascii = 'x' });
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
