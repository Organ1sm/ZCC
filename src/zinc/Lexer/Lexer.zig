const std = @import("std");

const Compilation = @import("../Basic/Compilation.zig");
const LangOpts = @import("../Basic/LangOpts.zig");
const Source = @import("../Basic/Source.zig");
const TokenType = @import("../Basic/TokenType.zig").TokenType;

const UCNKind = enum(u8) {
    /// Just `\`
    none,
    /// \u or \U followed by an insufficient number of hex digits
    incomplete,
    /// `\uxxxx`
    hex4 = 5,
    /// `\Uxxxxxxxx`
    hex8 = 8,

    /// In the classification phase we don't not care if the escape represents a valid
    /// universal character name
    fn classify(buffer: []const u8) UCNKind {
        std.debug.assert(buffer[0] == '\\');
        if (buffer.len == 1) return .none;

        switch (buffer[1]) {
            'u' => {
                if (buffer.len < 6) return .incomplete;
                for (buffer[2..6]) |c| {
                    if (!std.ascii.isHex(c)) return .incomplete;
                }
                return .hex4;
            },
            'U' => {
                if (buffer.len < 10) return .incomplete;
                for (buffer[2..10]) |c| {
                    if (!std.ascii.isHex(c)) return .incomplete;
                }
                return .hex8;
            },
            else => return .none,
        }
    }
};

pub const Token = struct {
    /// Token classification type (keyword, identifier etc.)
    id: TokenType,

    /// Source file ID where this token is from
    source: Source.ID,

    /// Starting byte offset of token in source file
    start: u32 = 0,

    /// Ending byte offset of token in source file
    end: u32 = 0,

    /// Line number where this token appears
    line: u32 = 0,

    pub inline fn is(self: Token, kind: TokenType) bool {
        return self.id == kind;
    }

    pub inline fn isNot(self: Token, kind: TokenType) bool {
        return self.id != kind;
    }

    pub fn isOneOf(self: Token, kinds: anytype) bool {
        inline for (kinds) |k| {
            if (self.id == k) {
                return true;
            }
        }
        return false;
    }

    /// Maps a token string to a TokenType based on keywords
    /// and current language standard.
    ///
    /// Special cases like inline, restrict have different keyword
    /// status depending on standards. These are checked before
    /// mapping.
    ///
    /// Returns final TokenType for further processing.
    pub fn getTokenId(langOpts: LangOpts, str: []const u8) TokenType {
        const kw = AllKeywords.get(str) orelse return .Identifier;

        // Retrieve the language standard from the Compilation context
        const standard = langOpts.standard;
        return switch (kw) {
            .KeywordInline => if (standard.isGNU() or standard.atLeast(.c99)) kw else .Identifier,
            .KeywordRestrict => if (standard.atLeast(.c99)) kw else .Identifier,
            .KeywordTypeof => if (standard.isGNU() or standard.atLeast(.c23)) kw else .Identifier,
            .KeywordGccAsm => if (standard.isGNU()) kw else .Identifier,

            .KeywordC23Alignas,
            .KeywordC23Alignof,
            .KeywordC23Bool,
            .KeywordC23StaticAssert,
            .KeywordC23ThreadLocal,
            .KeywordConstexpr,
            .KeywordTrue,
            .KeywordFalse,
            .KeywordNullptr,
            .KeywordElifdef,
            .KeywordElifndef,
            .KeywordTypeofUnqual,
            => if (standard.atLeast(.c23)) kw else .Identifier,

            .KeywordDeclSpec => if (langOpts.declSpecAttrs) kw else .Identifier,

            .KeywordMSInt64_,
            .KeywordMSInt64__,
            .KeywordMSInt32_,
            .KeywordMSInt32__,
            .KeywordMSInt16_,
            .KeywordMSInt16__,
            .KeywordMSInt8_,
            .KeywordMSInt8__,
            .KeywordStdCall2,
            .KeywordThisCall2,
            .KeywordVectorCall2,
            .KeywordFastcall2,
            .KeywordCdecl2,
            .KeywordForceInline,
            .KeywordForceInline2,
            .KeywordUnaligned,
            .KeywordUnaligned2,
            => if (langOpts.msExtensions) kw else .Identifier,
            else => kw,
        };
    }

    pub const AllKeywords = std.StaticStringMap(TokenType).initComptime(.{
        .{ "auto", .KeywordAuto },
        .{ "enum", .KeywordEnum },
        .{ "union", .KeywordUnion },
        .{ "struct", .KeywordStruct },
        .{ "unsigned", .KeywordUnsigned },
        .{ "signed", .KeywordSigned },
        .{ "__signed", .KeywordSigned1 },
        .{ "__signed__", .KeywordSigned2 },
        .{ "void", .KeywordVoid },
        .{ "char", .KeywordChar },
        .{ "short", .KeywordShort },
        .{ "int", .KeywordInt },
        .{ "long", .KeywordLong },
        .{ "float", .KeywordFloat },
        .{ "double", .KeywordDouble },

        .{ "for", .KeywordFor },
        .{ "if", .KeywordIf },
        .{ "else", .KeywordElse },
        .{ "do", .KeywordDo },
        .{ "while", .KeywordWhile },
        .{ "switch", .KeywordSwitch },
        .{ "default", .KeywordDefault },
        .{ "goto", .KeywordGoto },
        .{ "continue", .KeywordContinue },
        .{ "break", .KeywordBreak },
        .{ "case", .KeywordCase },
        .{ "return", .KeywordReturn },

        .{ "auto", .KeywordAuto },
        .{ "static", .KeywordStatic },
        .{ "const", .KeywordConst },
        .{ "extern", .KeywordExtern },
        .{ "volatile", .KeywordVolatile },
        .{ "register", .KeywordRegister },

        .{ "sizeof", .KeywordSizeof },
        .{ "typedef", .KeywordTypedef },
        .{ "__typeof", .KeywordTypeof1 },
        .{ "__typeof__", .KeywordTypeof2 },

        // ISO C99
        .{ "_Bool", .KeywordBool },
        .{ "_Complex", .KeywordComplex },
        .{ "_Imaginary", .KeywordImaginary },
        .{ "inline", .KeywordInline },
        .{ "restrict", .KeywordRestrict },

        // ISO C11
        .{ "_Alignas", .KeywordAlignas },
        .{ "_Alignof", .KeywordAlignof },
        .{ "_Atomic", .KeywordAtomic },
        .{ "_Generic", .KeywordGeneric },
        .{ "_Noreturn", .KeywordNoreturn },
        .{ "_Static_assert", .KeywordStaticAssert },
        .{ "_Thread_local", .KeywordThreadLocal },

        // ISO C23
        .{ "_BitInt", .KeywordBitInt },
        .{ "alignas", .KeywordC23Alignas },
        .{ "alignof", .KeywordC23Alignof },
        .{ "bool", .KeywordC23Bool },
        .{ "static_assert", .KeywordC23StaticAssert },
        .{ "thread_local", .KeywordC23ThreadLocal },
        .{ "constexpr", .KeywordConstexpr },
        .{ "true", .KeywordTrue },
        .{ "false", .KeywordFalse },
        .{ "nullptr", .KeywordNullptr },
        .{ "typeof_unqual", .KeywordTypeofUnqual },

        // Preprocessor directives
        .{ "include", .KeywordInclude },
        .{ "include_next", .KeywordIncludeNext },
        .{ "embed", .KeywordEmbed },
        .{ "define", .KeywordDefine },
        .{ "defined", .KeywordDefined },
        .{ "undef", .KeywordUndef },
        .{ "ifdef", .KeywordIfdef },
        .{ "ifndef", .KeywordIfndef },
        .{ "elif", .KeywordElIf },
        .{ "elifdef", .KeywordElifdef },
        .{ "elifndef", .KeywordElifndef },
        .{ "endif", .KeywordEndIf },
        .{ "error", .KeywordError },
        .{ "warning", .KeywordWarning },
        .{ "pragma", .KeywordPragma },
        .{ "line", .KeywordLine },
        .{ "__VA_ARGS__", .KeywordVarArgs },
        .{ "__VA_OPT__", .KeywordVarOpt },
        .{ "__func__", .MacroFunc },
        .{ "__FUNCTION__", .MacroFunction },
        .{ "__PRETTY_FUNCTION__", .MacroPrettyFunc },

        // gcc keywords
        .{ "__auto_type", .KeywordAutoType },
        .{ "__const", .KeywordGccConst1 },
        .{ "__const__", .KeywordGccConst2 },
        .{ "__inline", .KeywordGccInline1 },
        .{ "__inline__", .KeywordGccInline2 },
        .{ "__volatile", .KeywordGccVolatile1 },
        .{ "__volatile__", .KeywordGccVolatile2 },
        .{ "__restrict", .KeywordGccRestrict1 },
        .{ "__restrict__", .KeywordGccRestrict2 },
        .{ "__alignof", .KeywordGccAlignof1 },
        .{ "__alignof__", .KeywordGccAlignof2 },
        .{ "typeof", .KeywordTypeof },
        .{ "__extension__", .KeywordGccExtension },
        .{ "asm", .KeywordGccAsm },
        .{ "__asm", .KeywordGccAsm1 },
        .{ "__asm__", .KeywordGccAsm2 },
        .{ "__attribute", .KeywordAttribute1 },
        .{ "__attribute__", .KeywordAttribute2 },
        .{ "_Float128", .KeywordFloat128_ },
        .{ "__float128", .KeywordFloat128__ },
        .{ "__int128", .KeywordInt128 },
        .{ "__imag", .KeywordImag1 },
        .{ "__imag__", .KeywordImag2 },
        .{ "__real", .KeywordReal1 },
        .{ "__real__", .KeywordReal2 },
        .{ "_Float16", .KeywordFloat16 },

        // clang keywords
        .{ "__fp16", .KeywordFp16 },

        // MS keywords
        .{ "__declspec", .KeywordDeclSpec },
        .{ "_int64", .KeywordMSInt64_ },
        .{ "__int64", .KeywordMSInt64__ },
        .{ "_int32", .KeywordMSInt32_ },
        .{ "__int32", .KeywordMSInt32__ },
        .{ "_int16", .KeywordMSInt16_ },
        .{ "__int16", .KeywordMSInt16__ },
        .{ "_int8", .KeywordMSInt8_ },
        .{ "__int8", .KeywordMSInt8__ },
        .{ "__stdcall", .KeywordStdCall },
        .{ "_stdcall", .KeywordStdCall2 },
        .{ "__thiscall", .KeywordThisCall },
        .{ "_thiscall", .KeywordThisCall2 },
        .{ "__vectorcall", .KeywordVectorCall },
        .{ "_vectorcall", .KeywordVectorCall2 },
        .{ "__fastcall", .KeywordFastcall },
        .{ "_fastcall", .KeywordFastcall2 },
        .{ "_regcall", .KeywordRegcall },
        .{ "__cdecl", .KeywordCdecl },
        .{ "_cdecl", .KeywordCdecl2 },
        .{ "__forceinline", .KeywordForceInline },
        .{ "_forceinline", .KeywordForceInline2 },
        .{ "__unaligned", .KeywordUnaligned },
        .{ "_unaligned", .KeywordUnaligned2 },

        // Type nullability
        .{ "_Nonnull", .KeywordNonnull },
        .{ "_Nullable", .KeywordNullable },
        .{ "_Nullable_result", .KeywordNullableResult },
        .{ "_Null_unspecified", .KeywordNullUnspecified },

        // gcc builtins
        .{ "__builtin_choose_expr", .BuiltinChooseExpr },
        .{ "__builtin_va_arg", .BuiltinVaArg },
        .{ "__builtin_offsetof", .BuiltinOffsetof },
        .{ "__builtin_bitoffsetof", .BuiltinBitOffsetof },
        .{ "__builtin_types_compatible_p", .BuiltinTypesCompatibleP },
    });
};

const Lexer = @This();

/// Input buffer containing source code
buffer: []const u8 = undefined,
/// Current tokenize position
index: u32 = 0,
/// Source file ID
source: Source.ID,
langOpts: LangOpts,
/// current line number
line: u32 = 1,

const State = enum {
    start,
    whitespace,
    u,
    u8,
    U,
    L,
    string_literal,
    char_literal_start,
    char_literal,
    char_escape_sequence,
    string_escape_sequence,
    identifier,
    extended_identifier,
    equal,
    bang,
    pipe,
    colon,
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
    multi_line_comment_done,
    pp_num,
    pp_num_exponent,
    pp_num_digit_separator,
};

pub fn next(self: *Lexer) Token {
    var state: State = .start;
    var start = self.index;
    var id: TokenType = .Eof;

    while (self.index < self.buffer.len) : (self.index += 1) {
        const c = self.buffer[self.index];
        switch (state) {
            .start => switch (c) {
                '\n' => {
                    id = .NewLine;
                    self.index += 1;
                    self.line += 1;
                    break;
                },

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

                '\\' => {
                    const ucnKind = UCNKind.classify(self.buffer[self.index..]);
                    switch (ucnKind) {
                        .none => {
                            self.index += 1;
                            id = .Invalid;
                            break;
                        },

                        .incomplete => {
                            self.index += 1;
                            id = .IncompleteUcn;
                            break;
                        },

                        .hex4, .hex8 => {
                            self.index += @intFromEnum(ucnKind);
                            id = .ExtendedIdentifier;
                            state = .extended_identifier;
                        },
                    }
                },
                'a'...'t', 'v'...'z', 'A'...'K', 'M'...'T', 'V'...'Z', '_' => state = .identifier,

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
                ':' => if (self.langOpts.standard.atLeast(.c23)) {
                    state = .colon;
                } else {
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
                '0'...'9' => state = .pp_num,
                '\t', '\x0B', '\x0C', ' ' => state = .whitespace,
                '$' => if (self.langOpts.dollarsInIdentifiers) {
                    state = .extended_identifier;
                } else {
                    id = .Invalid;
                    self.index += 1;
                    break;
                },
                0x1A => if (self.langOpts.msExtensions) { // ctrl-z
                    id = .Eof;
                    break;
                } else {
                    id = .Invalid;
                    self.index += 1;
                    break;
                },
                0x80...0xFF => state = .extended_identifier,
                else => {
                    id = .Invalid;
                    self.index += 1;
                    break;
                },
            },

            .whitespace => switch (c) {
                '\t', '\x0B', '\x0C', ' ' => {},
                else => {
                    id = .WhiteSpace;
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
                '\'' => {
                    id = .CharLiteralUTF_8;
                    state = .char_literal_start;
                },
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
                    state = .string_escape_sequence;
                },
                '"' => {
                    self.index += 1;
                    break;
                },
                '\n' => {
                    id = .UnterminatedStringLiteral;
                    break;
                },
                '\r' => unreachable,
                else => {},
            },

            .char_literal_start => switch (c) {
                '\\' => {
                    state = .char_escape_sequence;
                },
                '\'' => {
                    id = .EmptyCharLiteral;
                    self.index += 1;
                    break;
                },
                '\n' => {
                    id = .UnterminatedCharLiteral;
                    break;
                },
                else => {
                    state = .char_literal;
                },
            },

            .char_literal => switch (c) {
                '\\' => {
                    state = .char_escape_sequence;
                },
                '\'' => {
                    self.index += 1;
                    break;
                },
                '\n' => {
                    id = .UnterminatedCharLiteral;
                    break;
                },
                else => {},
            },

            .char_escape_sequence => switch (c) {
                '\r', '\n' => {
                    id = .UnterminatedCharLiteral;
                },
                else => state = .char_literal,
            },

            .string_escape_sequence => switch (c) {
                '\n', '\r' => {
                    id = .UnterminatedStringLiteral;
                },
                else => state = .string_literal,
            },

            .identifier, .extended_identifier => switch (c) {
                'a'...'z', 'A'...'Z', '_', '0'...'9' => {},
                '$' => if (self.langOpts.dollarsInIdentifiers) {
                    state = .extended_identifier;
                } else {
                    id = if (state == .identifier) Token.getTokenId(self.langOpts, self.buffer[start..self.index]) else .ExtendedIdentifier;
                    break;
                },
                0x80...0xFF => state = .extended_identifier,

                '\\' => {
                    const ucnKind = UCNKind.classify(self.buffer[self.index..]);
                    switch (ucnKind) {
                        .none, .incomplete => {
                            id = if (state == .identifier) Token.getTokenId(self.langOpts, self.buffer[start..self.index]) else .ExtendedIdentifier;
                            break;
                        },

                        .hex4, .hex8 => {
                            self.index += @intFromEnum(ucnKind);
                            state = .extended_identifier;
                        },
                    }
                },

                else => {
                    id = if (state == .identifier) Token.getTokenId(self.langOpts, self.buffer[start..self.index]) else .ExtendedIdentifier;
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

            .colon => switch (c) {
                ':' => {
                    id = .ColonColon;
                    self.index += 1;
                    break;
                },
                else => {
                    id = .Colon;
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
                '<' => state = .angle_bracket_angle_bracket_left,
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
                '>' => state = .angle_bracket_angle_bracket_right,
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
                '.' => state = .period2,
                '0'...'9' => state = .pp_num,
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
                '/' => state = .line_comment,
                '*' => state = .multi_line_comment,
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
                    if (self.langOpts.preserveComments) {
                        id = .Comment;
                        break;
                    }
                    self.index -= 1;
                    state = .start;
                },
                else => {},
            },
            .multi_line_comment => switch (c) {
                '*' => state = .multi_line_comment_asterisk,
                '\n' => self.line += 1,
                else => {},
            },
            .multi_line_comment_asterisk => switch (c) {
                '/' => {
                    if (self.langOpts.preserveComments) {
                        self.index += 1;
                        id = .Comment;
                        break;
                    }
                    state = .multi_line_comment_done;
                },
                '\n' => {
                    self.line += 1;
                    state = .multi_line_comment;
                },
                '*' => {},
                else => state = .multi_line_comment,
            },
            .multi_line_comment_done => switch (c) {
                '\n' => {
                    start = self.index + 1;
                    id = .NewLine;
                    self.index += 1;
                    self.line += 1;
                    break;
                },
                '\r' => unreachable,
                '\t', '\x0B', '\x0C', ' ' => {
                    start = self.index;
                    state = .whitespace;
                },
                else => {
                    id = .WhiteSpace;
                    break;
                },
            },

            .pp_num => switch (c) {
                'a'...'d',
                'A'...'D',
                'f'...'o',
                'F'...'O',
                'q'...'z',
                'Q'...'Z',
                '0'...'9',
                '_',
                '.',
                => {},
                'e', 'E', 'p', 'P' => state = .pp_num_exponent,
                '\'' => if (self.langOpts.standard.atLeast(.c23)) {
                    state = .pp_num_digit_separator;
                } else {
                    id = .PPNumber;
                    break;
                },
                else => {
                    id = .PPNumber;
                    break;
                },
            },

            .pp_num_exponent => switch (c) {
                'a'...'o',
                'q'...'z',
                'A'...'O',
                'Q'...'Z',
                '0'...'9',
                '_',
                '.',
                '+',
                '-',
                => state = .pp_num,
                'p', 'P' => {},
                else => {
                    id = .PPNumber;
                    break;
                },
            },

            .pp_num_digit_separator => switch (c) {
                'a'...'d',
                'A'...'D',
                'f'...'o',
                'F'...'O',
                'q'...'z',
                'Q'...'Z',
                '0'...'9',
                '_',
                => state = .pp_num,
                else => {
                    self.index -= 1;
                    id = .PPNumber;
                    break;
                },
            },
        }
    } else if (self.index == self.buffer.len) {
        switch (state) {
            .start, .line_comment => {},
            .u, .u8, .U, .L, .identifier => id = Token.getTokenId(self.langOpts, self.buffer[start..self.index]),
            .extended_identifier => id = TokenType.ExtendedIdentifier,

            .whitespace,
            .multi_line_comment_done,
            => id = TokenType.WhiteSpace,

            .period2 => {
                self.index -= 1;
                id = .Period;
            },

            .multi_line_comment,
            .multi_line_comment_asterisk,
            => id = TokenType.UnterminatedComment,

            .char_escape_sequence, .char_literal_start, .char_literal => id = TokenType.UnterminatedCharLiteral,
            .string_escape_sequence, .string_literal => id = TokenType.UnterminatedStringLiteral,

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
            .colon => id = .Colon,
            .percent => id = TokenType.Percent,
            .caret => id = TokenType.Caret,
            .asterisk => id = TokenType.Asterisk,

            .pp_num, .pp_num_exponent, .pp_num_digit_separator => id = TokenType.PPNumber,
        }
    }

    return .{
        .id = id,
        .start = start,
        .end = self.index,
        .line = self.line,
        .source = self.source,
    };
}

pub fn nextNoWs(self: *Lexer) Token {
    return self.nextNoSpecificTokens(std.EnumSet(TokenType).initMany(&[_]TokenType{ .Comment, .WhiteSpace }));
}

pub fn nextNoWsComments(self: *Lexer) Token {
    return self.nextNoSpecificTokens(std.EnumSet(TokenType).initOne(.WhiteSpace));
}

pub fn nextNoWsAndNewLine(self: *Lexer) Token {
    return self.nextNoSpecificTokens(std.EnumSet(TokenType).initMany(&[_]TokenType{ .NewLine, .WhiteSpace }));
}

pub fn nextNoSpecificTokens(self: *Lexer, skipTokens: std.EnumSet(TokenType)) Token {
    var token = self.next();
    while (skipTokens.contains(token.id))
        token = self.next();
    return token;
}

/// Try to tokenize a '::' even if not supported by the current language standard.
pub fn colonColon(self: *Lexer) Token {
    var tok = self.nextNoWs();
    if (tok.id == .Colon and self.index < self.buffer.len and self.buffer[self.index] == ':') {
        self.index += 1;
        tok.id = .ColonColon;
    }
    return tok;
}

fn expectTokens(contents: []const u8, expectedTokens: []const TokenType) !void {
    return expectTokensExtra(contents, expectedTokens, null);
}

fn expectTokensExtra(contents: []const u8, expected: []const TokenType, standard: ?LangOpts.Standard) !void {
    const allocator = std.testing.allocator;
    const io = std.testing.io;

    var arena: std.heap.ArenaAllocator = .init(allocator);
    defer arena.deinit();

    var comp = Compilation.init(allocator, arena.allocator(), io, undefined, std.fs.cwd());
    defer comp.deinit();

    if (standard) |provided|
        comp.langOpts.standard = provided;

    const source = try comp.addSourceFromBuffer("path", contents);
    var lexer = Lexer{
        .buffer = source.buffer,
        .source = source.id,
        .langOpts = comp.langOpts,
    };

    var i: usize = 0;
    while (i < expected.len) {
        const token = lexer.next();
        if (token.is(.WhiteSpace)) continue;
        const expectedTokenId = expected[i];
        i += 1;
        if (!std.meta.eql(token.id, expectedTokenId)) {
            std.debug.panic("expected {s}, found {s}\n", .{ @tagName(expectedTokenId), @tagName(token.id) });
        }
    }

    const lastToken = lexer.next();
    std.testing.expect(lastToken.is(.Eof)) catch std.debug.print("lastToken not EOF", .{});
}

test "extended identifiers" {
    try expectTokens("𝓪𝓻𝓸𝓬𝓬", &.{.ExtendedIdentifier});
    try expectTokens("u𝓪𝓻𝓸𝓬𝓬", &.{.ExtendedIdentifier});
    try expectTokens("u8𝓪𝓻𝓸𝓬𝓬", &.{.ExtendedIdentifier});
    try expectTokens("U𝓪𝓻𝓸𝓬𝓬", &.{.ExtendedIdentifier});
    try expectTokens("L𝓪𝓻𝓸𝓬𝓬", &.{.ExtendedIdentifier});
    try expectTokens("1™", &.{ .PPNumber, .ExtendedIdentifier });
    try expectTokens("1.™", &.{ .PPNumber, .ExtendedIdentifier });
    try expectTokens("..™", &.{ .Period, .Period, .ExtendedIdentifier });
    try expectTokens("0™", &.{ .PPNumber, .ExtendedIdentifier });
    try expectTokens("0b\u{E0000}", &.{ .PPNumber, .ExtendedIdentifier });
    try expectTokens("0b0\u{E0000}", &.{ .PPNumber, .ExtendedIdentifier });
    try expectTokens("01\u{E0000}", &.{ .PPNumber, .ExtendedIdentifier });
    try expectTokens("010\u{E0000}", &.{ .PPNumber, .ExtendedIdentifier });
    try expectTokens("0x\u{E0000}", &.{ .PPNumber, .ExtendedIdentifier });
    try expectTokens("0x0\u{E0000}", &.{ .PPNumber, .ExtendedIdentifier });
    try expectTokens("\"\\0\u{E0000}\"", &.{.StringLiteral});
    try expectTokens("\"\\x\u{E0000}\"", &.{.StringLiteral});
    try expectTokens("\"\\u\u{E0000}\"", &.{.StringLiteral});
    try expectTokens("1e\u{E0000}", &.{ .PPNumber, .ExtendedIdentifier });
    try expectTokens("1e1\u{E0000}", &.{ .PPNumber, .ExtendedIdentifier });
}

test "operators" {
    try expectTokens(
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
    try expectTokens(
        \\auto __auto_type break case char const continue default do 
        \\double else enum extern float for goto if int 
        \\long register return short signed sizeof static 
        \\struct switch typedef union unsigned void volatile 
        \\while _Bool _Complex _Imaginary inline restrict _Alignas 
        \\_Alignof _Atomic _Generic _Noreturn _Static_assert _Thread_local 
        \\__attribute __attribute__
        \\
    , &.{
        .KeywordAuto,
        .KeywordAutoType,
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
        .KeywordAttribute1,
        .KeywordAttribute2,
        .NewLine,
    });
}

test "preprocessor directives" {
    try expectTokens(
        \\#include
        \\#include_next
        // \\#embed
        \\#define
        \\#ifdef
        \\#ifndef
        \\#error
        \\#pragma
        \\
    , &.{
        .Hash, .KeywordInclude,     .NewLine,
        .Hash, .KeywordIncludeNext, .NewLine,
        // .Hash, .KeywordEmbed,       .NewLine,
        .Hash, .KeywordDefine,      .NewLine,
        .Hash, .KeywordIfdef,       .NewLine,
        .Hash, .KeywordIfndef,      .NewLine,
        .Hash, .KeywordError,       .NewLine,
        .Hash, .KeywordPragma,      .NewLine,
    });
}

test "line continuation" {
    try expectTokens(
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
    try expectTokens(
        \\"foo"
        \\u"foo"
        \\u8"foo"
        \\U"foo"
        \\L"foo"
        \\'foo'
        \\u8'A'
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
        .CharLiteralUTF_8,    .NewLine,
        .CharLiteralUTF_16,   .NewLine,
        .CharLiteralUTF_32,   .NewLine,
        .CharLiteralWide,     .NewLine,
    });
}

test "num suffixes" {
    try expectTokens(
        \\ 1.0f 1.0L 1.0 .0 1. 0x1p0f 0X1p0
        \\ 0l 0lu 0ll 0llu 0
        \\ 1u 1ul 1ull 1
        \\ 1.0i 1.0I
        \\ 1.0if 1.0If 1.0fi 1.0fI
        \\ 1.0il 1.0Il 1.0li 1.0lI
        \\
    , &.{
        .PPNumber,
        .PPNumber,
        .PPNumber,
        .PPNumber,
        .PPNumber,
        .PPNumber,
        .PPNumber,
        .NewLine,
        .PPNumber,
        .PPNumber,
        .PPNumber,
        .PPNumber,
        .PPNumber,
        .NewLine,
        .PPNumber,
        .PPNumber,
        .PPNumber,
        .PPNumber,
        .NewLine,
        .PPNumber,
        .PPNumber,
        .NewLine,
        .PPNumber,
        .PPNumber,
        .PPNumber,
        .PPNumber,
        .NewLine,
        .PPNumber,
        .PPNumber,
        .PPNumber,
        .PPNumber,
        .NewLine,
    });
}

test "comments" {
    try expectTokens(
        \\//foo
        \\#foo
    , &.{
        .NewLine,
        .Hash,
        .Identifier,
    });
}

test "C23 keywords" {
    try expectTokensExtra("true false alignas alignof bool static_assert thread_local nullptr typeof_unqual", &.{ .KeywordTrue, .KeywordFalse, .KeywordC23Alignas, .KeywordC23Alignof, .KeywordC23Bool, .KeywordC23StaticAssert, .KeywordC23ThreadLocal, .KeywordNullptr, .KeywordTypeofUnqual }, .c23);
}

test "Universal character names" {
    try expectTokens("\\", &.{.Invalid});
    try expectTokens("\\g", &.{ .Invalid, .Identifier });
    try expectTokens("\\u", &.{ .IncompleteUcn, .Identifier });
    try expectTokens("\\ua", &.{ .IncompleteUcn, .Identifier });
    try expectTokens("\\U9", &.{ .IncompleteUcn, .Identifier });
    try expectTokens("\\ug", &.{ .IncompleteUcn, .Identifier });
    try expectTokens("\\uag", &.{ .IncompleteUcn, .Identifier });

    try expectTokens("\\ ", &.{ .Invalid, .Eof });
    try expectTokens("\\g ", &.{ .Invalid, .Identifier, .Eof });
    try expectTokens("\\u ", &.{ .IncompleteUcn, .Identifier, .Eof });
    try expectTokens("\\ua ", &.{ .IncompleteUcn, .Identifier, .Eof });
    try expectTokens("\\U9 ", &.{ .IncompleteUcn, .Identifier, .Eof });
    try expectTokens("\\ug ", &.{ .IncompleteUcn, .Identifier, .Eof });
    try expectTokens("\\uag ", &.{ .IncompleteUcn, .Identifier, .Eof });

    try expectTokens("a\\", &.{ .Identifier, .Invalid });
    try expectTokens("a\\g", &.{ .Identifier, .Invalid, .Identifier });
    try expectTokens("a\\u", &.{ .Identifier, .IncompleteUcn, .Identifier });
    try expectTokens("a\\ua", &.{ .Identifier, .IncompleteUcn, .Identifier });
    try expectTokens("a\\U9", &.{ .Identifier, .IncompleteUcn, .Identifier });
    try expectTokens("a\\ug", &.{ .Identifier, .IncompleteUcn, .Identifier });
    try expectTokens("a\\uag", &.{ .Identifier, .IncompleteUcn, .Identifier });

    try expectTokens("a\\ ", &.{ .Identifier, .Invalid, .Eof });
    try expectTokens("a\\g ", &.{ .Identifier, .Invalid, .Identifier, .Eof });
    try expectTokens("a\\u ", &.{ .Identifier, .IncompleteUcn, .Identifier, .Eof });
    try expectTokens("a\\ua ", &.{ .Identifier, .IncompleteUcn, .Identifier, .Eof });
    try expectTokens("a\\U9 ", &.{ .Identifier, .IncompleteUcn, .Identifier, .Eof });
    try expectTokens("a\\ug ", &.{ .Identifier, .IncompleteUcn, .Identifier, .Eof });
    try expectTokens("a\\uag ", &.{ .Identifier, .IncompleteUcn, .Identifier, .Eof });
}

test "Lexer fuzz test" {
    const Context = struct {
        fn testOne(_: @This(), inputBytes: []const u8) anyerror!void {
            const allocator = std.testing.allocator;
            const io = std.testing.io;

            var arena: std.heap.ArenaAllocator = .init(allocator);
            defer arena.deinit();

            var comp = Compilation.init(allocator, arena.allocator(), io, undefined, std.fs.cwd());
            defer comp.deinit();

            const source = try comp.addSourceFromBuffer("fuzz.c", inputBytes);

            var lexer: Lexer = .{
                .buffer = source.buffer,
                .source = source.id,
                .langOpts = comp.langOpts,
            };

            while (true) {
                const prevIndex = lexer.index;
                const token = lexer.next();

                if (token.is(.Eof)) break;
                try std.testing.expect(prevIndex < lexer.index);
            }
        }
    };

    return std.testing.fuzz(Context{}, Context.testOne, .{});
}
