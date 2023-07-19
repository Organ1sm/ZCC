const std = @import("std");
const TokenType = @import("../Basic/TokenType.zig").TokenType;
const Token = @import("Token.zig").Token;
const Source = @import("../Basic/Source.zig");
const Lexer = @This();

buffer: []const u8 = undefined,
index: u32 = 0,
source: Source.ID,

const State = enum {
    start,
    cr,
    back_slash,
    back_slash_cr,
    u,
    u8,
    U,
    L,
    string_literal,
    char_literal_start,
    char_literal,
    escape_sequence,
    cr_escape,
    octal_escape,
    hex_escape,
    unicode_escape,
    identifier,
    equal,
    bang,
    pipe,
    percent,
    asterisk,
    plus,
    angle_bracket_left,
    angle_bracket_angle_bracket_left,
    angle_bracket_right,
    angle_bracket_angle_bracket_right,
    caret,
    period,
    period2,
    minus,
    slash,
    ampersand,
    hash,
    line_comment,
    multi_line_comment,
    multi_line_comment_asterisk,
    zero,
    integer_literal_oct,
    integer_literal_binary,
    integer_literal_hex,
    integer_literal,
    integer_suffix,
    integer_suffix_u,
    integer_suffix_l,
    integer_suffix_ll,
    integer_suffix_ul,
    float_fraction,
    float_fraction_hex,
    float_exponent,
    float_exponent_digits,
    float_suffix,
};

pub fn next(self: *Lexer) Token {
    var state: State = .start;
    var start = self.index;
    var id: TokenType = .Eof;
    var string = false;
    var counter: u32 = 0;

    while (self.index < self.buffer.len) : (self.index += 1) {
        const c = self.buffer[self.index];
        switch (state) {
            .start => switch (c) {
                '\n' => {
                    id = .NewLine;
                    self.index += 1;
                    break;
                },

                '\r' => state = .cr,

                '"' => {
                    id = .StringLiteral;
                    state = .string_literal;
                },

                '\'' => {
                    id = .CharLiteral;
                    state = .char_literal_start;
                },

                'u' => state = .u,
                'U' => state = .U,
                'L' => state = .L,
                'a'...'t', 'v'...'z', 'A'...'K', 'M'...'T', 'V'...'Z', '_', '$' => state = .identifier,

                '=' => state = .equal,
                '!' => state = .bang,
                '|' => state = .pipe,

                '(' => {
                    id = .LParen;
                    self.index += 1;
                    break;
                },
                ')' => {
                    id = .RParen;
                    self.index += 1;
                    break;
                },
                '[' => {
                    id = .LBracket;
                    self.index += 1;
                    break;
                },
                ']' => {
                    id = .RBracket;
                    self.index += 1;
                    break;
                },
                '{' => {
                    id = .LBrace;
                    self.index += 1;
                    break;
                },
                '}' => {
                    id = .RBrace;
                    self.index += 1;
                    break;
                },

                ';' => {
                    id = .Semicolon;
                    self.index += 1;
                    break;
                },
                ',' => {
                    id = .Comma;
                    self.index += 1;
                    break;
                },
                '?' => {
                    id = .QuestionMark;
                    self.index += 1;
                    break;
                },
                ':' => {
                    id = .Colon;
                    self.index += 1;
                    break;
                },
                '~' => {
                    id = .Tilde;
                    self.index += 1;
                    break;
                },

                '%' => state = .percent,
                '*' => state = .asterisk,
                '+' => state = .plus,
                '<' => state = .angle_bracket_left,
                '>' => state = .angle_bracket_right,
                '^' => state = .caret,
                '.' => state = .period,
                '-' => state = .minus,
                '/' => state = .slash,
                '&' => state = .ampersand,
                '#' => state = .hash,
                '0' => state = .zero,
                '1'...'9' => state = .integer_literal,
                '\\' => state = .back_slash,
                '\t', '\x0B', '\x0C', ' ' => start = self.index + 1,
                else => {
                    id = .Invalid;
                    self.index += 1;
                    break;
                },
            },

            .cr => switch (c) {
                '\n' => {
                    id = .NewLine;
                    self.index += 1;
                    break;
                },
                else => {
                    id = .Invalid;
                    break;
                },
            },

            .back_slash => switch (c) {
                '\n' => {
                    start = self.index + 1;
                    state = .start;
                },
                '\r' => state = .back_slash_cr,
                '\t', '\x0B', '\x0C', ' ' => {
                    // TODO warn
                },
                else => {
                    id = .Invalid;
                    break;
                },
            },

            .back_slash_cr => switch (c) {
                '\n' => {
                    start = self.index + 1;
                    state = .start;
                },
                else => {
                    id = .Invalid;
                    break;
                },
            },

            .u => switch (c) {
                '8' => {
                    state = .u8;
                },
                '\'' => {
                    id = .CharLiteralUTF_16;
                    state = .char_literal_start;
                },
                '\"' => {
                    id = .StringLiteralUTF_16;
                    state = .string_literal;
                },
                else => {
                    self.index -= 1;
                    state = .identifier;
                },
            },

            .u8 => switch (c) {
                '\"' => {
                    id = .StringLiteralUTF_8;
                    state = .string_literal;
                },
                else => {
                    self.index -= 1;
                    state = .identifier;
                },
            },

            .U => switch (c) {
                '\'' => {
                    id = .CharLiteralUTF_32;
                    state = .char_literal_start;
                },
                '\"' => {
                    id = .StringLiteralUTF_32;
                    state = .string_literal;
                },
                else => {
                    self.index -= 1;
                    state = .identifier;
                },
            },

            .L => switch (c) {
                '\'' => {
                    id = .CharLiteralWide;
                    state = .char_literal_start;
                },
                '\"' => {
                    id = .StringLiteralWide;
                    state = .string_literal;
                },
                else => {
                    self.index -= 1;
                    state = .identifier;
                },
            },

            .string_literal => switch (c) {
                '\\' => {
                    string = true;
                    state = .escape_sequence;
                },
                '"' => {
                    self.index += 1;
                    break;
                },
                '\n', '\r' => {
                    id = .Invalid;
                    break;
                },
                else => {},
            },

            .char_literal_start => switch (c) {
                '\\' => {
                    string = false;
                    state = .escape_sequence;
                },

                '\'', '\n' => {
                    id = .Invalid;
                    break;
                },
                else => {
                    state = .char_literal;
                },
            },

            .char_literal => switch (c) {
                '\\' => {
                    string = false;
                    state = .escape_sequence;
                },
                '\'' => {
                    self.index += 1;
                    break;
                },
                '\n' => {
                    id = .Invalid;
                    break;
                },
                else => {},
            },

            .escape_sequence => switch (c) {
                '\'', '"', '?', '\\', 'a', 'b', 'f', 'n', 'r', 't', 'v', '\n' => {
                    state = if (string) .string_literal else .char_literal;
                },
                '\r' => state = .cr_escape,
                '0'...'7' => {
                    counter = 1;
                    state = .octal_escape;
                },
                'x' => state = .hex_escape,
                'u' => {
                    counter = 4;
                    state = .octal_escape;
                },
                'U' => {
                    counter = 8;
                    state = .octal_escape;
                },
                else => {
                    id = .Invalid;
                    break;
                },
            },

            .cr_escape => switch (c) {
                '\n' => {
                    state = if (string) .string_literal else .char_literal;
                },
                else => {
                    break;
                },
            },

            .octal_escape => switch (c) {
                '0'...'7' => {
                    counter += 1;
                    if (counter == 3) {
                        state = if (string) .string_literal else .char_literal;
                    }
                },
                else => {
                    self.index -= 1;
                    state = if (string) .string_literal else .char_literal;
                },
            },

            .hex_escape => switch (c) {
                '0'...'9', 'a'...'f', 'A'...'F' => {},
                else => {
                    self.index -= 1;
                    state = if (string) .string_literal else .char_literal;
                },
            },

            .unicode_escape => switch (c) {
                '0'...'9', 'a'...'f', 'A'...'F' => {
                    counter -= 1;
                    if (counter == 0) {
                        state = if (string) .string_literal else .char_literal;
                    }
                },
                else => {
                    if (counter != 0) {
                        id = .Invalid;
                        break;
                    }
                    self.index -= 1;
                    state = if (string) .string_literal else .char_literal;
                },
            },

            .identifier => switch (c) {
                'a'...'z', 'A'...'Z', '_', '0'...'9', '$' => {},
                else => {
                    id = Token.keywords.get(self.buffer[start..self.index]) orelse TokenType.Identifier;
                    break;
                },
            },

            .equal => switch (c) {
                '=' => {
                    id = .EqualEqual;
                    self.index += 1;
                    break;
                },
                else => {
                    id = .Equal;
                    break;
                },
            },

            .bang => switch (c) {
                '=' => {
                    id = .BangEqual;
                    self.index += 1;
                    break;
                },
                else => {
                    id = .Bang;
                    break;
                },
            },

            .pipe => switch (c) {
                '=' => {
                    id = .PipeEqual;
                    self.index += 1;
                    break;
                },
                '|' => {
                    id = .PipePipe;
                    self.index += 1;
                    break;
                },
                else => {
                    id = .Pipe;
                    break;
                },
            },

            .percent => switch (c) {
                '=' => {
                    id = .PercentEqual;
                    self.index += 1;
                    break;
                },
                else => {
                    id = .Percent;
                    break;
                },
            },
            .asterisk => switch (c) {
                '=' => {
                    id = .AsteriskEqual;
                    self.index += 1;
                    break;
                },
                else => {
                    id = .Asterisk;
                    break;
                },
            },

            .plus => switch (c) {
                '=' => {
                    id = .PlusEqual;
                    self.index += 1;
                    break;
                },
                '+' => {
                    id = .PlusPlus;
                    self.index += 1;
                    break;
                },
                else => {
                    id = .Plus;
                    break;
                },
            },
            .angle_bracket_left => switch (c) {
                '<' => {
                    state = .angle_bracket_angle_bracket_left;
                },
                '=' => {
                    id = .AngleBracketLeftEqual;
                    self.index += 1;
                    break;
                },
                else => {
                    id = .AngleBracketLeft;
                    break;
                },
            },

            .angle_bracket_angle_bracket_left => switch (c) {
                '=' => {
                    id = .AngleBracketAngleBracketLeftEqual;
                    self.index += 1;
                    break;
                },
                else => {
                    id = .AngleBracketAngleBracketLeft;
                    break;
                },
            },

            .angle_bracket_right => switch (c) {
                '>' => {
                    state = .angle_bracket_angle_bracket_right;
                },
                '=' => {
                    id = .AngleBracketRightEqual;
                    self.index += 1;
                    break;
                },
                else => {
                    id = .AngleBracketRight;
                    break;
                },
            },

            .angle_bracket_angle_bracket_right => switch (c) {
                '=' => {
                    id = .AngleBracketAngleBracketRightEqual;
                    self.index += 1;
                    break;
                },
                else => {
                    id = .AngleBracketAngleBracketRight;
                    break;
                },
            },

            .caret => switch (c) {
                '=' => {
                    id = .CaretEqual;
                    self.index += 1;
                    break;
                },
                else => {
                    id = .Caret;
                    break;
                },
            },

            .period => switch (c) {
                '.' => {
                    state = .period2;
                },
                '0'...'9' => {
                    state = .float_fraction;
                },
                else => {
                    id = .Period;
                    break;
                },
            },

            .period2 => switch (c) {
                '.' => {
                    id = .Ellipsis;
                    self.index += 1;
                    break;
                },
                else => {
                    id = .Period;
                    self.index -= 1;
                    break;
                },
            },

            .minus => switch (c) {
                '>' => {
                    id = .Arrow;
                    self.index += 1;
                    break;
                },
                '=' => {
                    id = .MinusEqual;
                    self.index += 1;
                    break;
                },
                '-' => {
                    id = .MinusMinus;
                    self.index += 1;
                    break;
                },
                else => {
                    id = .Minus;
                    break;
                },
            },

            .ampersand => switch (c) {
                '&' => {
                    id = .AmpersandAmpersand;
                    self.index += 1;
                    break;
                },
                '=' => {
                    id = .AmpersandEqual;
                    self.index += 1;
                    break;
                },
                else => {
                    id = .Ampersand;
                    break;
                },
            },
            .hash => switch (c) {
                '#' => {
                    id = .HashHash;
                    self.index += 1;
                    break;
                },
                else => {
                    id = .Hash;
                    break;
                },
            },

            .slash => switch (c) {
                '/' => {
                    state = .line_comment;
                },
                '*' => {
                    state = .multi_line_comment;
                },
                '=' => {
                    id = .SlashEqual;
                    self.index += 1;
                    break;
                },
                else => {
                    id = .Slash;
                    break;
                },
            },

            .line_comment => switch (c) {
                '\n' => {
                    self.index -= 1;
                    state = .start;
                },
                else => {},
            },
            .multi_line_comment => switch (c) {
                '*' => state = .multi_line_comment_asterisk,
                else => {},
            },
            .multi_line_comment_asterisk => switch (c) {
                '/' => state = .start,
                else => state = .multi_line_comment,
            },

            .zero => switch (c) {
                '0'...'9' => state = .integer_literal_oct,
                'b', 'B' => state = .integer_literal_binary,
                'x', 'X' => state = .integer_literal_hex,
                '.' => state = .float_fraction,
                else => {
                    state = .integer_suffix;
                    self.index -= 1;
                },
            },

            .integer_literal_oct => switch (c) {
                '0'...'7' => {},
                else => {
                    state = .integer_suffix;
                    self.index -= 1;
                },
            },
            .integer_literal_binary => switch (c) {
                '0', '1' => {},
                else => {
                    state = .integer_suffix;
                    self.index -= 1;
                },
            },
            .integer_literal_hex => switch (c) {
                '0'...'9', 'a'...'f', 'A'...'F' => {},
                '.' => state = .float_fraction_hex,
                'p', 'P' => state = .float_exponent,
                else => {
                    state = .integer_suffix;
                    self.index -= 1;
                },
            },
            .integer_literal => switch (c) {
                '0'...'9' => {},
                '.' => state = .float_fraction,
                'e', 'E' => state = .float_exponent,
                else => {
                    state = .integer_suffix;
                    self.index -= 1;
                },
            },
            .integer_suffix => switch (c) {
                'u', 'U' => state = .integer_suffix_u,
                'l', 'L' => state = .integer_suffix_l,
                else => {
                    id = .IntegerLiteral;
                    break;
                },
            },
            .integer_suffix_u => switch (c) {
                'l', 'L' => state = .integer_suffix_ul,
                else => {
                    id = .IntegerLiteral_U;
                    break;
                },
            },
            .integer_suffix_l => switch (c) {
                'l', 'L' => state = .integer_suffix_ll,
                'u', 'U' => {
                    id = .IntegerLiteral_LU;
                    self.index += 1;
                    break;
                },
                else => {
                    id = .IntegerLiteral_L;
                    break;
                },
            },
            .integer_suffix_ll => switch (c) {
                'u', 'U' => {
                    id = .IntegerLiteral_LLU;
                    self.index += 1;
                    break;
                },
                else => {
                    id = .IntegerLiteral_LL;
                    break;
                },
            },
            .integer_suffix_ul => switch (c) {
                'l', 'L' => {
                    id = .IntegerLiteral_LLU;
                    self.index += 1;
                    break;
                },
                else => {
                    id = .IntegerLiteral_LU;
                    break;
                },
            },

            .float_fraction => switch (c) {
                '0'...'9' => {},
                'e', 'E' => state = .float_exponent,
                else => {
                    self.index -= 1;
                    state = .float_suffix;
                },
            },
            .float_fraction_hex => switch (c) {
                '0'...'9', 'a'...'f', 'A'...'F' => {},
                'p', 'P' => state = .float_exponent,
                else => {
                    id = .Invalid;
                    break;
                },
            },
            .float_exponent => switch (c) {
                '+', '-' => state = .float_exponent_digits,
                else => {
                    self.index -= 1;
                    state = .float_exponent_digits;
                },
            },
            .float_exponent_digits => switch (c) {
                '0'...'9' => counter += 1,
                else => {
                    if (counter == 0) {
                        id = .Invalid;
                        break;
                    }
                    self.index -= 1;
                    state = .float_suffix;
                },
            },
            .float_suffix => switch (c) {
                'l', 'L' => {
                    id = .FloatLiteral_L;
                    self.index += 1;
                    break;
                },
                'f', 'F' => {
                    id = .FloatLiteral_F;
                    self.index += 1;
                    break;
                },
                else => {
                    id = .FloatLiteral;
                    break;
                },
            },
        }
    } else if (self.index == self.buffer.len) {
        switch (state) {
            .start, .line_comment => {},
            .u, .u8, .U, .L, .identifier => {
                id = Token.keywords.get(self.buffer[start..self.index]) orelse TokenType.Identifier;
            },

            .cr,
            .back_slash,
            .back_slash_cr,
            .period2,
            .string_literal,
            .char_literal_start,
            .char_literal,
            .escape_sequence,
            .cr_escape,
            .octal_escape,
            .hex_escape,
            .unicode_escape,
            .multi_line_comment,
            .multi_line_comment_asterisk,
            .float_exponent,
            => id = TokenType.Invalid,

            .float_exponent_digits => id = if (counter == 0) TokenType.Invalid else TokenType.FloatLiteral,
            .float_fraction,
            .float_fraction_hex,
            => id = TokenType.FloatLiteral,

            .integer_literal_oct,
            .integer_literal_binary,
            .integer_literal_hex,
            .integer_literal,
            .integer_suffix,
            .zero,
            => id = TokenType.IntegerLiteral,

            .integer_suffix_u => id = TokenType.IntegerLiteral_U,
            .integer_suffix_l => id = TokenType.IntegerLiteral_L,
            .integer_suffix_ll => id = TokenType.IntegerLiteral_LL,
            .integer_suffix_ul => id = TokenType.IntegerLiteral_LU,

            .float_suffix => id = TokenType.FloatLiteral,
            .equal => id = TokenType.Equal,
            .bang => id = TokenType.Bang,
            .minus => id = TokenType.Minus,
            .slash => id = TokenType.Slash,
            .ampersand => id = TokenType.Ampersand,
            .hash => id = TokenType.Hash,
            .period => id = TokenType.Period,
            .pipe => id = TokenType.Pipe,
            .angle_bracket_angle_bracket_right => id = TokenType.AngleBracketAngleBracketRight,
            .angle_bracket_right => id = TokenType.AngleBracketRight,
            .angle_bracket_angle_bracket_left => id = TokenType.AngleBracketAngleBracketLeft,
            .angle_bracket_left => id = TokenType.AngleBracketLeft,
            .plus => id = TokenType.Plus,
            .percent => id = TokenType.Percent,
            .caret => id = TokenType.Caret,
            .asterisk => id = TokenType.Asterisk,
        }
    }

    return .{
        .id = id,
        .loc = .{
            .start = start,
            .end = self.index,
        },
        .source = self.source,
    };
}

fn expectTokens(source: []const u8, expected: []const TokenType) void {
    var lexer = Lexer{
        .buffer = source,
        .source = undefined,
    };

    for (expected) |expectedTokenId| {
        const token = lexer.next();
        if (!std.meta.eql(token.id, expectedTokenId)) {
            std.debug.panic("expected {s}, found {s}\n", .{ @tagName(expectedTokenId), @tagName(token.id) });
        }
    }

    const lastToken = lexer.next();
    std.testing.expect(lastToken.id == .Eof) catch std.debug.print("lastToken not EOF", .{});
}

test "operators" {
    expectTokens(
        \\ ! != | || |= = ==
        \\ ( ) { } [ ] . .. ...
        \\ ^ ^= + ++ += - -- -=
        \\ * *= % %= -> : ; / /=
        \\ , & && &= ? < <= <<
        \\  <<= > >= >> >>= ~ # ##
        \\
    , &.{
        .Bang,
        .BangEqual,
        .Pipe,
        .PipePipe,
        .PipeEqual,
        .Equal,
        .EqualEqual,
        .NewLine,
        .LParen,
        .RParen,
        .LBrace,
        .RBrace,
        .LBracket,
        .RBracket,
        .Period,
        .Period,
        .Period,
        .Ellipsis,
        .NewLine,
        .Caret,
        .CaretEqual,
        .Plus,
        .PlusPlus,
        .PlusEqual,
        .Minus,
        .MinusMinus,
        .MinusEqual,
        .NewLine,
        .Asterisk,
        .AsteriskEqual,
        .Percent,
        .PercentEqual,
        .Arrow,
        .Colon,
        .Semicolon,
        .Slash,
        .SlashEqual,
        .NewLine,
        .Comma,
        .Ampersand,
        .AmpersandAmpersand,
        .AmpersandEqual,
        .QuestionMark,
        .AngleBracketLeft,
        .AngleBracketLeftEqual,
        .AngleBracketAngleBracketLeft,
        .NewLine,
        .AngleBracketAngleBracketLeftEqual,
        .AngleBracketRight,
        .AngleBracketRightEqual,
        .AngleBracketAngleBracketRight,
        .AngleBracketAngleBracketRightEqual,
        .Tilde,
        .Hash,
        .HashHash,
        .NewLine,
    });
}

test "keywords" {
    expectTokens(
        \\auto break case char const continue default do 
        \\double else enum extern float for goto if int 
        \\long register return short signed sizeof static 
        \\struct switch typedef union unsigned void volatile 
        \\while _Bool _Complex _Imaginary inline restrict _Alignas 
        \\_Alignof _Atomic _Generic _Noreturn _Static_assert _Thread_local 
        \\
    , &.{
        .KeywordAuto,
        .KeywordBreak,
        .KeywordCase,
        .KeywordChar,
        .KeywordConst,
        .KeywordContinue,
        .KeywordDefault,
        .KeywordDo,
        .NewLine,
        .KeywordDouble,
        .KeywordElse,
        .KeywordEnum,
        .KeywordExtern,
        .KeywordFloat,
        .KeywordFor,
        .KeywordGoto,
        .KeywordIf,
        .KeywordInt,
        .NewLine,
        .KeywordLong,
        .KeywordRegister,
        .KeywordReturn,
        .KeywordShort,
        .KeywordSigned,
        .KeywordSizeof,
        .KeywordStatic,
        .NewLine,
        .KeywordStruct,
        .KeywordSwitch,
        .KeywordTypedef,
        .KeywordUnion,
        .KeywordUnsigned,
        .KeywordVoid,
        .KeywordVolatile,
        .NewLine,
        .KeywordWhile,
        .KeywordBool,
        .KeywordComplex,
        .KeywordImaginary,
        .KeywordInline,
        .KeywordRestrict,
        .KeywordAlignas,
        .NewLine,
        .KeywordAlignof,
        .KeywordAtomic,
        .KeywordGeneric,
        .KeywordNoreturn,
        .KeywordStaticAssert,
        .KeywordThreadLocal,
        .NewLine,
    });
}

test "preprocessor directives" {
    expectTokens(
        \\#include
        \\#define
        \\#ifdef
        \\#ifndef
        \\#error
        \\#pragma
        \\
    , &.{
        .Hash, .KeywordInclude, .NewLine,
        .Hash, .KeywordDefine,  .NewLine,
        .Hash, .KeywordIfdef,   .NewLine,
        .Hash, .KeywordIfndef,  .NewLine,
        .Hash, .KeywordError,   .NewLine,
        .Hash, .KeywordPragma,  .NewLine,
    });
}

test "line continuation" {
    expectTokens(
        \\#define foo \
        \\  bar
        \\"foo\
        \\ bar"
        \\#define "foo"
        \\ "bar"
        \\#define "foo" \
        \\ "bar"
    , &.{
        .Hash,
        .KeywordDefine,
        .Identifier,
        .Identifier,
        .NewLine,
        .StringLiteral,
        .NewLine,
        .Hash,
        .KeywordDefine,
        .StringLiteral,
        .NewLine,
        .StringLiteral,
        .NewLine,
        .Hash,
        .KeywordDefine,
        .StringLiteral,
        .StringLiteral,
    });
}

test "string prefix" {
    expectTokens(
        \\"foo"
        \\u"foo"
        \\u8"foo"
        \\U"foo"
        \\L"foo"
        \\'foo'
        \\u'foo'
        \\U'foo'
        \\L'foo'
        \\
    , &.{
        .StringLiteral,       .NewLine,
        .StringLiteralUTF_16, .NewLine,
        .StringLiteralUTF_8,  .NewLine,
        .StringLiteralUTF_32, .NewLine,
        .StringLiteralWide,   .NewLine,
        .CharLiteral,         .NewLine,
        .CharLiteralUTF_16,   .NewLine,
        .CharLiteralUTF_32,   .NewLine,
        .CharLiteralWide,     .NewLine,
    });
}

test "num suffixes" {
    expectTokens(
        \\ 1.0f 1.0L 1.0 .0 1.
        \\ 0l 0lu 0ll 0llu 0
        \\ 1u 1ul 1ull 1
        \\
    , &.{
        .FloatLiteral_F,
        .FloatLiteral_L,
        .FloatLiteral,
        .FloatLiteral,
        .FloatLiteral,
        .NewLine,
        .IntegerLiteral_L,
        .IntegerLiteral_LU,
        .IntegerLiteral_LL,
        .IntegerLiteral_LLU,
        .IntegerLiteral,
        .NewLine,
        .IntegerLiteral_U,
        .IntegerLiteral_LU,
        .IntegerLiteral_LLU,
        .IntegerLiteral,
        .NewLine,
    });
}
