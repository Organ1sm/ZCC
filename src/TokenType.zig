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
    StringLiteralUtf_32,

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
    Asteriskequal,
    Slash,
    SlashEqual,
    Percent,
    PercentEqual,

    /// "& && &= | || |="
    Ampersand,
    AmpersandAmpersand,
    Ampersandequal,
    Pipe,
    PipePipe,
    PipeEqual,

    /// "? < <= << <<= > >= >> >>="
    Questionmark,
    AngleBracketleft,
    AngleBracketleftEqual,
    AngleBracketanglebracketleft,
    AngleBracketanglebracketleftEqual,
    AngleBracketright,
    AngleBracketrightequal,
    AngleBracketanglebracketright,
    AngleBracketanglebracketrightequal,

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

    // Preprocessor directives
    KeywordInclude,
    KeywordDefine,
    KeywordIfdef,
    KeywordIfndef,
    KeywordError,
    KeywordPragma,
};
