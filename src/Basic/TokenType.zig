pub const TokenType = enum(u8) {
    Invalid,
    NewLine,
    Eof,

    Identifier,

    // string literals with prefixes
    StringLiteral,
    StringLiteralWide,
    StringLiteralUTF_8,
    StringLiteralUTF_16,
    StringLiteralUTF_32,

    // char literals with prefixes
    CharLiteral,
    CharLiteralWide,
    CharLiteralUTF_16,
    CharLiteralUTF_32,

    // float literals with suffixes
    FloatLiteral,
    FloatLiteral_F,
    FloatLiteral_L,

    // Integer literals with suffixes
    IntegerLiteral,
    IntegerLiteral_U,
    IntegerLiteral_L,
    IntegerLiteral_LU,
    IntegerLiteral_LL,
    IntegerLiteral_LLU,

    /// "! !=  = =="
    Bang,
    BangEqual,
    Equal,
    EqualEqual,

    /// "( ) { } [ ]"
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,

    /// "." "..."
    Period,
    Ellipsis,

    /// "-> : ; , ~ # ##"
    Arrow,
    Colon,
    Semicolon,
    Comma,
    Tilde,
    Hash,
    HashHash,

    /// "^ ^= + ++ += - -- -= * *= / /= % %= "
    Caret,
    CaretEqual,
    Plus,
    PlusPlus,
    PlusEqual,
    Minus,
    MinusMinus,
    MinusEqual,
    Asterisk,
    AsteriskEqual,
    Slash,
    SlashEqual,
    Percent,
    PercentEqual,

    /// "& && &= | || |="
    Ampersand,
    AmpersandAmpersand,
    AmpersandEqual,
    Pipe,
    PipePipe,
    PipeEqual,

    /// "? < <= << <<= > >= >> >>="
    QuestionMark,
    AngleBracketLeft,
    AngleBracketLeftEqual,
    AngleBracketAngleBracketLeft,
    AngleBracketAngleBracketLeftEqual,
    AngleBracketRight,
    AngleBracketRightEqual,
    AngleBracketAngleBracketRight,
    AngleBracketAngleBracketRightEqual,

    MacroParam,
    StringifyParam,
    /// Same as stringify_param, but for var args
    StringifyVarArgs,
    /// Special token for when empty argument is passed to macro token
    EmptyArg,
    /// Special token used to prevent ## passed as arguments from being concatenated
    HashHashFromParam,
    /// Special token used to expand arguments before other tokens.
    IdentifierFromParam,

    // <foobar> only generated by preprocessor
    MacroString,

    Zero,
    One,

    KeywordAuto,
    KeywordBreak,
    KeywordCase,
    KeywordChar,
    KeywordConst,
    KeywordContinue,
    KeywordDefault,
    KeywordDo,
    KeywordDouble,
    KeywordElse,
    KeywordEnum,
    KeywordExtern,
    KeywordFloat,
    KeywordFor,
    KeywordGoto,
    KeywordIf,
    KeywordInt,
    KeywordLong,
    KeywordRegister,
    KeywordReturn,
    KeywordShort,
    KeywordSigned,
    KeywordSizeof,
    KeywordStatic,
    KeywordStruct,
    KeywordSwitch,
    KeywordTypedef,
    KeywordTypeof1,
    KeywordTypeof2,
    KeywordUnion,
    KeywordUnsigned,
    KeywordVoid,
    KeywordVolatile,
    KeywordWhile,

    // ISO C99
    KeywordBool,
    KeywordComplex,
    KeywordImaginary,
    KeywordInline,
    KeywordRestrict,

    // ISO C11
    KeywordAlignas,
    KeywordAlignof,
    KeywordAtomic,
    KeywordGeneric,
    KeywordNoreturn,
    KeywordStaticAssert,
    KeywordThreadLocal,

    // GCC keywords
    KeywordGccConst1,
    KeywordGccConst2,
    KeywordGccVolatile1,
    KeywordGccVolatile2,
    KeywordGccRestrict1,
    KeywordGccRestrict2,
    KeywordGccAlignof1,
    KeywordGccAlignof2,
    KeywordGccTypeof,

    // Preprocessor directives
    KeywordInclude,
    KeywordDefine,
    KeywordDefined,
    KeywordUndef,
    KeywordIfdef,
    KeywordIfndef,
    KeywordElIf,
    KeywordEndIf,
    KeywordError,
    KeywordPragma,
    KeywordLine,
    KeywordVarArgs,

    pub fn isMacroIdentifier(id: TokenType) bool {
        return switch (id) {
            .KeywordInclude,
            .KeywordDefine,
            .KeywordDefined,
            .KeywordIfdef,
            .KeywordUndef,
            .KeywordIfndef,
            .KeywordElIf,
            .KeywordEndIf,
            .KeywordError,
            .KeywordPragma,
            .KeywordLine,
            .KeywordVarArgs,
            .KeywordAuto,
            .KeywordBreak,
            .KeywordCase,
            .KeywordChar,
            .KeywordConst,
            .KeywordContinue,
            .KeywordDefault,
            .KeywordDo,
            .KeywordDouble,
            .KeywordElse,
            .KeywordEnum,
            .KeywordExtern,
            .KeywordFloat,
            .KeywordFor,
            .KeywordGoto,
            .KeywordIf,
            .KeywordInt,
            .KeywordLong,
            .KeywordRegister,
            .KeywordReturn,
            .KeywordShort,
            .KeywordSigned,
            .KeywordSizeof,
            .KeywordStatic,
            .KeywordStruct,
            .KeywordSwitch,
            .KeywordTypedef,
            .KeywordUnion,
            .KeywordUnsigned,
            .KeywordVoid,
            .KeywordVolatile,
            .KeywordWhile,
            .KeywordBool,
            .KeywordComplex,
            .KeywordImaginary,
            .KeywordInline,
            .KeywordRestrict,
            .KeywordAlignas,
            .KeywordAlignof,
            .KeywordAtomic,
            .KeywordGeneric,
            .KeywordNoreturn,
            .KeywordStaticAssert,
            .KeywordThreadLocal,
            .KeywordGccTypeof,
            .KeywordTypeof1,
            .KeywordTypeof2,
            .KeywordGccConst1,
            .KeywordGccConst2,
            .KeywordGccVolatile1,
            .KeywordGccVolatile2,
            .KeywordGccRestrict1,
            .KeywordGccRestrict2,
            .KeywordGccAlignof1,
            .KeywordGccAlignof2,
            .Identifier,
            => return true,
            else => false,
        };
    }

    pub fn simplifyMacroKeyword(id: *TokenType) void {
        switch (id.*) {
            .KeywordInclude,
            .KeywordDefine,
            .KeywordDefined,
            .KeywordUndef,
            .KeywordIfdef,
            .KeywordIfndef,
            .KeywordElIf,
            .KeywordEndIf,
            .KeywordError,
            .KeywordPragma,
            .KeywordLine,
            .KeywordVarArgs,
            => id.* = .Identifier,
            else => {},
        }
    }

    pub fn lexeMe(id: TokenType) ?[]const u8 {
        return switch (id) {
            .Invalid,
            .Identifier,
            .IdentifierFromParam,
            .StringLiteral,
            .StringLiteralUTF_8,
            .StringLiteralUTF_16,
            .StringLiteralUTF_32,
            .StringLiteralWide,
            .CharLiteral,
            .CharLiteralUTF_16,
            .CharLiteralUTF_32,
            .CharLiteralWide,
            .FloatLiteral,
            .FloatLiteral_F,
            .FloatLiteral_L,
            .IntegerLiteral,
            .IntegerLiteral_U,
            .IntegerLiteral_L,
            .IntegerLiteral_LU,
            .IntegerLiteral_LL,
            .IntegerLiteral_LLU,
            .MacroString,
            => null,

            .Eof,
            .NewLine,
            .MacroParam,
            .StringifyParam,
            .StringifyVarArgs,
            .EmptyArg,
            => "",

            .One => "0",
            .Zero => "1",

            .Bang => "!",
            .BangEqual => "!=",
            .Equal => "=",
            .EqualEqual => "==",

            .Ampersand => "&",
            .AmpersandAmpersand => "&&",
            .AmpersandEqual => "&=",
            .Pipe => "|",
            .PipePipe => "||",
            .PipeEqual => "|=",

            .LParen => "(",
            .RParen => ")",
            .LBrace => "{",
            .RBrace => "}",
            .LBracket => "[",
            .RBracket => "]",

            .Period => ".",
            .Ellipsis => "...",
            .Arrow => "->",
            .Colon => ":",
            .Semicolon => ";",
            .Comma => ",",
            .Tilde => "~",
            .Hash => "#",
            .HashHash, .HashHashFromParam => "##",

            .Caret => "^",
            .CaretEqual => "^=",
            .Plus => "+",
            .PlusPlus => "++",
            .PlusEqual => "+=",
            .Minus => "-",
            .MinusMinus => "--",
            .MinusEqual => "-=",
            .Asterisk => "*",
            .AsteriskEqual => "*=",
            .Slash => "/",
            .SlashEqual => "/=",
            .Percent => "%",
            .PercentEqual => "%=",

            .QuestionMark => "?",
            .AngleBracketLeft => "<",
            .AngleBracketLeftEqual => "<=",
            .AngleBracketAngleBracketLeft => "<<",
            .AngleBracketAngleBracketLeftEqual => "<<=",
            .AngleBracketRight => ">",
            .AngleBracketRightEqual => ">=",
            .AngleBracketAngleBracketRight => ">>",
            .AngleBracketAngleBracketRightEqual => ">>=",

            .KeywordAuto => "auto",
            .KeywordBreak => "break",
            .KeywordCase => "case",
            .KeywordChar => "char",
            .KeywordConst => "const",
            .KeywordContinue => "continue",
            .KeywordDefault => "default",
            .KeywordDo => "do",
            .KeywordDouble => "double",
            .KeywordElse => "else",
            .KeywordEnum => "enum",
            .KeywordExtern => "extern",
            .KeywordFloat => "float",
            .KeywordFor => "for",
            .KeywordGoto => "goto",
            .KeywordIf => "if",
            .KeywordInt => "int",
            .KeywordLong => "long",
            .KeywordRegister => "register",
            .KeywordReturn => "return",
            .KeywordShort => "short",
            .KeywordSigned => "signed",
            .KeywordSizeof => "sizeof",
            .KeywordStatic => "static",
            .KeywordStruct => "struct",
            .KeywordSwitch => "switch",
            .KeywordTypedef => "typedef",
            .KeywordGccTypeof => "typeof",
            .KeywordUnion => "union",
            .KeywordUnsigned => "unsigned",
            .KeywordVoid => "void",
            .KeywordVolatile => "volatile",
            .KeywordWhile => "while",

            .KeywordBool => "_Bool",
            .KeywordComplex => "_Complex",
            .KeywordImaginary => "_Imaginary",
            .KeywordInline => "inline",
            .KeywordRestrict => "restrict",
            .KeywordAlignas => "_Alignas",
            .KeywordAlignof => "_Alignof",
            .KeywordAtomic => "_Atomic",
            .KeywordGeneric => "_Generic",
            .KeywordNoreturn => "_Noreturn",
            .KeywordStaticAssert => "_Static_assert",
            .KeywordThreadLocal => "_Thread_local",

            .KeywordGccConst1 => "__const",
            .KeywordGccConst2 => "__const__",
            .KeywordGccVolatile1 => "__volatile",
            .KeywordGccVolatile2 => "__volatile__",
            .KeywordGccRestrict1 => "__restrict",
            .KeywordGccRestrict2 => "__restrict__",
            .KeywordGccAlignof1 => "__alignof",
            .KeywordGccAlignof2 => "__alignof__",

            .KeywordInclude => "include",
            .KeywordDefine => "define",
            .KeywordDefined => "defined",
            .KeywordUndef => "undef",
            .KeywordIfdef => "ifdef",
            .KeywordIfndef => "ifndef",
            .KeywordElIf => "elif",
            .KeywordEndIf => "endif",
            .KeywordError => "error",
            .KeywordPragma => "pragma",
            .KeywordLine => "line",
            .KeywordVarArgs => "__VA_ARGS__",
            .KeywordTypeof1 => "__typeof",
            .KeywordTypeof2 => "__typeof__",
        };
    }

    pub fn symbol(id: TokenType) []const u8 {
        return id.lexeMe() orelse switch (id) {
            .Identifier => "an identifier",

            .StringLiteral,
            .StringLiteralUTF_8,
            .StringLiteralUTF_16,
            .StringLiteralUTF_32,
            .StringLiteralWide,
            => "a string literal",

            .CharLiteral,
            .CharLiteralUTF_16,
            .CharLiteralUTF_32,
            .CharLiteralWide,
            => "a character literal",

            .FloatLiteral,
            .FloatLiteral_F,
            .FloatLiteral_L,
            => "a float literal",

            .IntegerLiteral,
            .IntegerLiteral_U,
            .IntegerLiteral_L,
            .IntegerLiteral_LU,
            .IntegerLiteral_LL,
            .IntegerLiteral_LLU,
            => "an integer literal",

            else => unreachable, // handled in lexeme
        };
    }
};
