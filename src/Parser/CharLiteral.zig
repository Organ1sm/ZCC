const std = @import("std");
const Diagnostics = @import("../Basic/Diagnostics.zig");
const LangOpts = @import("../Basic/LangOpts.zig");
const mem = std.mem;

pub const Item = union(enum) {
    /// unicode escape
    codepoint: u21,
    /// hex/octal escape
    value: u32,
    /// CharLiteral in the sourct text is not utf8 encoded
    improperlyEncoded: []const u8,
    /// 1 or more unescaped bytes
    utf8Text: std.unicode.Utf8View,

    const replacement: Item = .{ .value = 0xFFFD };
};

const CharDiagnostics = struct {
    tag: Diagnostics.Tag,
    extra: Diagnostics.Message.Extra,
};
pub const Parser = struct {
    literal: []const u8,
    i: usize = 0,
    /// We only want to issue a max of 1 error per char literal
    errored: bool = false,
    errors: std.BoundedArray(CharDiagnostics, 4) = .{},
    standard: LangOpts.Standard,

    pub fn init(literal: []const u8, standard: LangOpts.Standard) Parser {
        const start = mem.indexOfScalar(u8, literal, '\'').? + 1; // trim leading quote + specifier if any
        return .{
            .literal = literal[start .. literal.len - 1], // trim trailing quote
            .i = 0,
            .standard = standard,
        };
    }

    fn err(self: *Parser, tag: Diagnostics.Tag, extra: Diagnostics.Message.Extra) void {
        if (self.errored) return;
        self.errored = true;
        self.errors.append(.{ .tag = tag, .extra = extra }) catch {};
    }

    fn warn(self: *Parser, tag: Diagnostics.Tag, extra: Diagnostics.Message.Extra) void {
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
                return .{ .improperlyEncoded = self.literal[start..self.i] };
            };
            return .{ .utf8Text = view };
        }

        switch (self.literal[start + 1]) {
            'u', 'U' => return self.parseUnicodeEscape(),
            else => return self.parseEscapedChar(),
        }
    }

    fn parseUnicodeEscape(self: *Parser) Item {
        const start = self.i;

        std.debug.assert(self.literal[self.i] == '\\');

        const kind = self.literal[self.i + 1];
        std.debug.assert(kind == 'u' or kind == 'U');

        self.i += 2;
        if (self.i >= self.literal.len or !std.ascii.isHex(self.literal[self.i])) {
            self.err(.non_hex_ucn, .{ .ascii = @intCast(kind) });
            return Item.replacement;
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
            self.err(.escape_sequence_overflow, .{ .unsigned = start });
            return Item.replacement;
        }

        if (count != expectedLen) {
            self.err(.incomplete_universal_character, .{ .none = {} });
            return Item.replacement;
        }

        if (val > std.math.maxInt(u21) or !std.unicode.utf8ValidCodepoint(@intCast(val))) {
            self.err(.invalid_universal_character, .{ .unsigned = start });
            return Item.replacement;
        }

        if (val < 0xA0 and (val != '$' and val != '@' and val != '`')) {
            const isError = !self.standard.atLeast(.c2x);
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
        defer self.i += 1;

        switch (self.literal[self.i]) {
            '\n' => unreachable, // removed by line splicing
            '\r' => unreachable, // removed by line splicing
            '\'', '\"', '\\', '?' => |c| return .{ .value = c },
            'n' => return .{ .value = '\n' },
            'r' => return .{ .value = '\r' },
            't' => return .{ .value = '\t' },
            'a' => return .{ .value = 0x07 },
            'b' => return .{ .value = 0x08 },
            'e' => {
                self.warn(.non_standard_escape_char, .{ .unsigned = self.i });
                return .{ .codepoint = 0x1B };
            },
            'f' => return .{ .value = 0x0C },
            'v' => return .{ .value = 0x0B },
            'x' => return .{ .value = self.parseHexEscape() },
            '0'...'7' => return .{ .value = self.parseOctalEscape() },
            'u', 'U' => unreachable, // handled by parseUnicodeEscape
            else => unreachable,
        }
    }

    fn parseHexEscape(self: *Parser) u32 {
        var val: u32 = 0;
        var count: usize = 0;
        var overflowed = false;
        defer self.i += count;

        for (self.literal[self.i + 1 ..]) |c| {
            const char = std.fmt.charToDigit(c, 16) catch break;
            val, const overflow = @shlWithOverflow(val, 4);
            if (overflow != 0) overflowed = true;
            val += char;
            count += 1;
        }
        if (overflowed) {
            std.debug.print("overflowed!\n", .{});
        }
        return val;
    }

    fn parseOctalEscape(self: *Parser) u32 {
        var val: u32 = 0;
        var count: usize = 0;
        defer self.i += count - 1;

        for (self.literal[self.i..], 0..) |c, i| {
            if (i == 3) break;
            const char = std.fmt.charToDigit(c, 8) catch break;
            val <<= 3;
            val += char;
            count += 1;
        }
        return val;
    }
};
