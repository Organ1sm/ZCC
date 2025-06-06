//! This module provides functions for classifying characters according to
//! various C standards. All classification routines *do not* consider
//! characters from the basic character set; it is assumed those will be
//! checked separately
//! isXidStart and isXidContinue are adapted from https://github.com/dtolnay/unicode-ident

const assert = @import("std").debug.assert;
const tables = @import("IdentfierTables.zig");

/// C11 Standard Annex D
/// see: https://www.open-std.org/jtc1/sc22/wg14/www/docs/n1548.pdf Annex D appendix.
pub fn isC11IdChar(codepoint: u21) bool {
    assert(codepoint > 0x7F);
    return switch (codepoint) {
        // 1
        0x00A8,
        0x00AA,
        0x00AD,
        0x00AF,
        0x00B2...0x00B5,
        0x00B7...0x00BA,
        0x00BC...0x00BE,
        0x00C0...0x00D6,
        0x00D8...0x00F6,
        0x00F8...0x00FF,

        // 2
        0x0100...0x167F,
        0x1681...0x180D,
        0x180F...0x1FFF,

        // 3
        0x200B...0x200D,
        0x202A...0x202E,
        0x203F...0x2040,
        0x2054,
        0x2060...0x206F,

        // 4
        0x2070...0x218F,
        0x2460...0x24FF,
        0x2776...0x2793,
        0x2C00...0x2DFF,
        0x2E80...0x2FFF,

        // 5
        0x3004...0x3007,
        0x3021...0x302F,
        0x3031...0x303F,

        // 6
        0x3040...0xD7FF,

        // 7
        0xF900...0xFD3D,
        0xFD40...0xFDCF,
        0xFDF0...0xFE44,
        0xFE47...0xFFFD,

        // 8
        0x10000...0x1FFFD,
        0x20000...0x2FFFD,
        0x30000...0x3FFFD,
        0x40000...0x4FFFD,
        0x50000...0x5FFFD,
        0x60000...0x6FFFD,
        0x70000...0x7FFFD,
        0x80000...0x8FFFD,
        0x90000...0x9FFFD,
        0xA0000...0xAFFFD,
        0xB0000...0xBFFFD,
        0xC0000...0xCFFFD,
        0xD0000...0xDFFFD,
        0xE0000...0xEFFFD,
        => true,
        else => false,
    };
}

/// C99 Standard Annex D
/// see: https://www.dii.uchile.cl/~daespino/files/Iso_C_1999_definition.pdf Annex D appendix.
pub fn isC99IdChar(codepoint: u21) bool {
    assert(codepoint > 0x7F);
    return switch (codepoint) {
        // Latin
        0x00AA,
        0x00BA,
        0x00C0...0x00D6,
        0x00D8...0x00F6,
        0x00F8...0x01F5,
        0x01FA...0x0217,
        0x0250...0x02A8,
        0x1E00...0x1E9B,
        0x1EA0...0x1EF9,
        0x207F,

        // Greek
        0x0386,
        0x0388...0x038A,
        0x038C,
        0x038E...0x03A1,
        0x03A3...0x03CE,
        0x03D0...0x03D6,
        0x03DA,
        0x03DC,
        0x03DE,
        0x03E0,
        0x03E2...0x03F3,
        0x1F00...0x1F15,
        0x1F18...0x1F1D,
        0x1F20...0x1F45,
        0x1F48...0x1F4D,
        0x1F50...0x1F57,
        0x1F59,
        0x1F5B,
        0x1F5D,
        0x1F5F...0x1F7D,
        0x1F80...0x1FB4,
        0x1FB6...0x1FBC,
        0x1FC2...0x1FC4,
        0x1FC6...0x1FCC,
        0x1FD0...0x1FD3,
        0x1FD6...0x1FDB,
        0x1FE0...0x1FEC,
        0x1FF2...0x1FF4,
        0x1FF6...0x1FFC,

        // Cyrillic
        0x0401...0x040C,
        0x040E...0x044F,
        0x0451...0x045C,
        0x045E...0x0481,
        0x0490...0x04C4,
        0x04C7...0x04C8,
        0x04CB...0x04CC,
        0x04D0...0x04EB,
        0x04EE...0x04F5,
        0x04F8...0x04F9,

        // Armenian
        0x0531...0x0556,
        0x0561...0x0587,

        // Hebrew
        0x05B0...0x05B9,
        0x05BB...0x05BD,
        0x05BF,
        0x05C1...0x05C2,
        0x05D0...0x05EA,
        0x05F0...0x05F2,

        // Arabic
        0x0621...0x063A,
        0x0640...0x0652,
        0x0670...0x06B7,
        0x06BA...0x06BE,
        0x06C0...0x06CE,
        0x06D0...0x06DC,
        0x06E5...0x06E8,
        0x06EA...0x06ED,

        // Devanagari
        0x0901...0x0903,
        0x0905...0x0939,
        0x093E...0x094D,
        0x0950...0x0952,
        0x0958...0x0963,

        // Bengali
        0x0981...0x0983,
        0x0985...0x098C,
        0x098F...0x0990,
        0x0993...0x09A8,
        0x09AA...0x09B0,
        0x09B2,
        0x09B6...0x09B9,
        0x09BE...0x09C4,
        0x09C7...0x09C8,
        0x09CB...0x09CD,
        0x09DC...0x09DD,
        0x09DF...0x09E3,
        0x09F0...0x09F1,

        // Gurmukhi
        0x0A02,
        0x0A05...0x0A0A,
        0x0A0F...0x0A10,
        0x0A13...0x0A28,
        0x0A2A...0x0A30,
        0x0A32...0x0A33,
        0x0A35...0x0A36,
        0x0A38...0x0A39,
        0x0A3E...0x0A42,
        0x0A47...0x0A48,
        0x0A4B...0x0A4D,
        0x0A59...0x0A5C,
        0x0A5E,
        0x0A74,

        // Gujarati
        0x0A81...0x0A83,
        0x0A85...0x0A8B,
        0x0A8D,
        0x0A8F...0x0A91,
        0x0A93...0x0AA8,
        0x0AAA...0x0AB0,
        0x0AB2...0x0AB3,
        0x0AB5...0x0AB9,
        0x0ABD...0x0AC5,
        0x0AC7...0x0AC9,
        0x0ACB...0x0ACD,
        0x0AD0,
        0x0AE0,

        // Oriya
        0x0B01...0x0B03,
        0x0B05...0x0B0C,
        0x0B0F...0x0B10,
        0x0B13...0x0B28,
        0x0B2A...0x0B30,
        0x0B32...0x0B33,
        0x0B36...0x0B39,
        0x0B3E...0x0B43,
        0x0B47...0x0B48,
        0x0B4B...0x0B4D,
        0x0B5C...0x0B5D,
        0x0B5F...0x0B61,

        // Tamil
        0x0B82...0x0B83,
        0x0B85...0x0B8A,
        0x0B8E...0x0B90,
        0x0B92...0x0B95,
        0x0B99...0x0B9A,
        0x0B9C,
        0x0B9E...0x0B9F,
        0x0BA3...0x0BA4,
        0x0BA8...0x0BAA,
        0x0BAE...0x0BB5,
        0x0BB7...0x0BB9,
        0x0BBE...0x0BC2,
        0x0BC6...0x0BC8,
        0x0BCA...0x0BCD,

        // Telugu
        0x0C01...0x0C03,
        0x0C05...0x0C0C,
        0x0C0E...0x0C10,
        0x0C12...0x0C28,
        0x0C2A...0x0C33,
        0x0C35...0x0C39,
        0x0C3E...0x0C44,
        0x0C46...0x0C48,
        0x0C4A...0x0C4D,
        0x0C60...0x0C61,

        // Kannada
        0x0C82...0x0C83,
        0x0C85...0x0C8C,
        0x0C8E...0x0C90,
        0x0C92...0x0CA8,
        0x0CAA...0x0CB3,
        0x0CB5...0x0CB9,
        0x0CBE...0x0CC4,
        0x0CC6...0x0CC8,
        0x0CCA...0x0CCD,
        0x0CDE,
        0x0CE0...0x0CE1,

        // Malayalam
        0x0D02...0x0D03,
        0x0D05...0x0D0C,
        0x0D0E...0x0D10,
        0x0D12...0x0D28,
        0x0D2A...0x0D39,
        0x0D3E...0x0D43,
        0x0D46...0x0D48,
        0x0D4A...0x0D4D,
        0x0D60...0x0D61,

        // Thai (excluding digits 0x0E50...0x0E59; originally 0x0E01...0x0E3A and 0x0E40...0x0E5B
        0x0E01...0x0E3A,
        0x0E40...0x0E4F,
        0x0E5A...0x0E5B,

        // Lao
        0x0E81...0x0E82,
        0x0E84,
        0x0E87...0x0E88,
        0x0E8A,
        0x0E8D,
        0x0E94...0x0E97,
        0x0E99...0x0E9F,
        0x0EA1...0x0EA3,
        0x0EA5,
        0x0EA7,
        0x0EAA...0x0EAB,
        0x0EAD...0x0EAE,
        0x0EB0...0x0EB9,
        0x0EBB...0x0EBD,
        0x0EC0...0x0EC4,
        0x0EC6,
        0x0EC8...0x0ECD,
        0x0EDC...0x0EDD,

        // Tibetan
        0x0F00,
        0x0F18...0x0F19,
        0x0F35,
        0x0F37,
        0x0F39,
        0x0F3E...0x0F47,
        0x0F49...0x0F69,
        0x0F71...0x0F84,
        0x0F86...0x0F8B,
        0x0F90...0x0F95,
        0x0F97,
        0x0F99...0x0FAD,
        0x0FB1...0x0FB7,
        0x0FB9,

        // Georgian
        0x10A0...0x10C5,
        0x10D0...0x10F6,

        // Hiragana
        0x3041...0x3093,
        0x309B...0x309C,

        // Katakana
        0x30A1...0x30F6,
        0x30FB...0x30FC,

        // Bopomofo
        0x3105...0x312C,

        // CJK Unified Ideographs
        0x4E00...0x9FA5,

        // Hangul
        0xAC00...0xD7A3,

        // Digits
        0x0660...0x0669,
        0x06F0...0x06F9,
        0x0966...0x096F,
        0x09E6...0x09EF,
        0x0A66...0x0A6F,
        0x0AE6...0x0AEF,
        0x0B66...0x0B6F,
        0x0BE7...0x0BEF,
        0x0C66...0x0C6F,
        0x0CE6...0x0CEF,
        0x0D66...0x0D6F,
        0x0E50...0x0E59,
        0x0ED0...0x0ED9,
        0x0F20...0x0F33,

        // Special characters
        0x00B5,
        0x00B7,
        0x02B0...0x02B8,
        0x02BB,
        0x02BD...0x02C1,
        0x02D0...0x02D1,
        0x02E0...0x02E4,
        0x037A,
        0x0559,
        0x093D,
        0x0B3D,
        0x1FBE,
        0x203F...0x2040,
        0x2102,
        0x2107,
        0x210A...0x2113,
        0x2115,
        0x2118...0x211D,
        0x2124,
        0x2126,
        0x2128,
        0x212A...0x2131,
        0x2133...0x2138,
        0x2160...0x2182,
        0x3005...0x3007,
        0x3021...0x3029,
        => true,
        else => false,
    };
}

/// C11 standard Annex D
pub fn isC11DisallowedInitialIdChar(codepoint: u21) bool {
    assert(codepoint > 0x7F);
    return switch (codepoint) {
        0x0300...0x036F,
        0x1DC0...0x1DFF,
        0x20D0...0x20FF,
        0xFE20...0xFE2F,
        => true,
        else => false,
    };
}

/// These are "digit" characters; C99 disallows them as the first
/// character of an identifier
pub fn isC99DisallowedInitialIDChar(codepoint: u21) bool {
    assert(codepoint > 0x7F);
    return switch (codepoint) {
        0x0660...0x0669,
        0x06F0...0x06F9,
        0x0966...0x096F,
        0x09E6...0x09EF,
        0x0A66...0x0A6F,
        0x0AE6...0x0AEF,
        0x0B66...0x0B6F,
        0x0BE7...0x0BEF,
        0x0C66...0x0C6F,
        0x0CE6...0x0CEF,
        0x0D66...0x0D6F,
        0x0E50...0x0E59,
        0x0ED0...0x0ED9,
        0x0F20...0x0F33,
        => true,
        else => false,
    };
}

pub fn isInvisible(codepoint: u21) bool {
    assert(codepoint > 0x7F);
    return switch (codepoint) {
        0x00ad, // SOFT HYPHEN
        0x200b, // ZERO WIDTH SPACE
        0x200c, // ZERO WIDTH NON-JOINER
        0x200d, // ZERO WIDTH JOINER
        0x2060, // WORD JOINER
        0x2061, // FUNCTION APPLICATION
        0x2062, // INVISIBLE TIMES
        0x2063, // INVISIBLE SEPARATOR
        0x2064, // INVISIBLE PLUS
        0xfeff, // ZERO WIDTH NO-BREAK SPACE
        => true,
        else => false,
    };
}

/// Checks for identifier characters which resemble non-identifier characters
pub fn homoglyph(codepoint: u21) ?[]const u8 {
    assert(codepoint > 0x7F);
    return switch (codepoint) {
        0x01c3 => "!", // LATIN LETTER RETROFLEX CLICK
        0x037e => ";", // GREEK QUESTION MARK
        0x2212 => "-", // MINUS SIGN
        0x2215 => "/", // DIVISION SLASH
        0x2216 => "\\", // SET MINUS
        0x2217 => "*", // ASTERISK OPERATOR
        0x2223 => "|", // DIVIDES
        0x2227 => "^", // LOGICAL AND
        0x2236 => ":", // RATIO
        0x223c => "~", // TILDE OPERATOR
        0xa789 => ":", // MODIFIER LETTER COLON
        0xff01 => "!", // FULLWIDTH EXCLAMATION MARK
        0xff03 => "#", // FULLWIDTH NUMBER SIGN
        0xff04 => "$", // FULLWIDTH DOLLAR SIGN
        0xff05 => "%", // FULLWIDTH PERCENT SIGN
        0xff06 => "&", // FULLWIDTH AMPERSAND
        0xff08 => "(", // FULLWIDTH LEFT PARENTHESIS
        0xff09 => ")", // FULLWIDTH RIGHT PARENTHESIS
        0xff0a => "*", // FULLWIDTH ASTERISK
        0xff0b => "+", // FULLWIDTH ASTERISK
        0xff0c => ",", // FULLWIDTH COMMA
        0xff0d => "-", // FULLWIDTH HYPHEN-MINUS
        0xff0e => ".", // FULLWIDTH FULL STOP
        0xff0f => "/", // FULLWIDTH SOLIDUS
        0xff1a => ":", // FULLWIDTH COLON
        0xff1b => ";", // FULLWIDTH SEMICOLON
        0xff1c => "<", // FULLWIDTH LESS-THAN SIGN
        0xff1d => "=", // FULLWIDTH EQUALS SIGN
        0xff1e => ">", // FULLWIDTH GREATER-THAN SIGN
        0xff1f => "?", // FULLWIDTH QUESTION MARK
        0xff20 => "@", // FULLWIDTH COMMERCIAL AT
        0xff3b => "[", // FULLWIDTH LEFT SQUARE BRACKET
        0xff3c => "\\", // FULLWIDTH REVERSE SOLIDUS
        0xff3d => "]", // FULLWIDTH RIGHT SQUARE BRACKET
        0xff3e => "^", // FULLWIDTH CIRCUMFLEX ACCENT
        0xff5b => "{", // FULLWIDTH LEFT CURLY BRACKET
        0xff5c => "|", // FULLWIDTH VERTICAL LINE
        0xff5d => "}", // FULLWIDTH RIGHT CURLY BRACKET
        0xff5e => "~", // FULLWIDTH TILDE
        else => null,
    };
}

pub fn isXidStart(c: u21) bool {
    assert(c > 0x7F);
    const idx = c / 8 / tables.chunk;
    const chunk: usize = if (idx < tables.trie_start.len) tables.trie_start[idx] else 0;
    const offset = chunk * tables.chunk / 2 + c / 8 % tables.chunk;
    return (tables.leaf[offset] >> (@as(u3, @intCast(c % 8)))) & 1 != 0;
}

pub fn isXidContinue(c: u21) bool {
    assert(c > 0x7F);
    const idx = c / 8 / tables.chunk;
    const chunk: usize = if (idx < tables.trie_continue.len) tables.trie_continue[idx] else 0;
    const offset = chunk * tables.chunk / 2 + c / 8 % tables.chunk;
    return (tables.leaf[offset] >> (@as(u3, @intCast(c % 8)))) & 1 != 0;
}

test "isXidStart / isXidContinue panic check" {
    const std = @import("std");
    for (0x80..0x110000) |i| {
        const c: u21 = @intCast(i);
        if (std.unicode.utf8ValidCodepoint(c)) {
            _ = isXidStart(c);
            _ = isXidContinue(c);
        }
    }
}

test isXidStart {
    const std = @import("std");
    try std.testing.expect(!isXidStart('᠑'));
    try std.testing.expect(!isXidStart('™'));
    try std.testing.expect(!isXidStart('£'));
    try std.testing.expect(!isXidStart('\u{1f914}')); // 🤔
}

test isXidContinue {
    const std = @import("std");
    try std.testing.expect(isXidContinue('᠑'));
    try std.testing.expect(!isXidContinue('™'));
    try std.testing.expect(!isXidContinue('£'));
    try std.testing.expect(!isXidContinue('\u{1f914}')); // 🤔
}
