pub const TokenType = enum(u8) {
    Invalid,
    NewLine,
    WhiteSpace,
    Eof,

    /// identifier containing solely basic character set characters
    Identifier,
    /// identifier with at least one extended character
    ExtendedIdentifier,

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

    // imaginary literals
    ImaginaryLiteral,
    ImaginaryLiteral_F,
    ImaginaryLiteral_L,

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
    ColonColon,
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

    /// Special token to speed up preprocessing, `loc.end` will be an index to the param list.
    MacroParam,
    /// Special token to signal that the argument must be replaced without expansion (e.g. in concatenation)
    MacroParamNoExpand,
    /// Special token to speed up preprocessing, `loc.end` will be an index to the param list.
    StringifyParam,
    /// Same as stringify_param, but for var args
    StringifyVarArgs,
    /// Special macro whitespace, always equal to a single space
    MacroWS,
    /// Special token for implementing __has_attribute
    MacroParamHasAttribute,
    /// Special token for implementing __has_warning
    MacroParamHasWarning,
    /// Special token for implementing __has_feature
    MacroParamHasFeature,
    /// Special token for implementing __has_extension
    MacroParamHasExtension,
    /// Special token for implementing __has_builtin
    MacroParamHasBuiltin,
    /// Special token for implementing __has_include
    MacroParamHasInclude,
    /// Special token for implementing __has_include_next
    MacroParamHasIncludeNext,
    /// Special token for implementing __is_identifier
    MacroParamIsIdentifier,
    /// Special token for implementing __FILE__
    MacroFile,
    /// Special token for implementing __LINE__
    MacroLine,
    /// Special token for implementing __COUNTER__
    MacroCounter,
    /// Special token for implementing _Pragma
    MacroParamPragmaOperator,
    /// Special identifier for implementing __func__
    MacroFunc,
    /// Special identifier for implementing __FUNCTION__
    MacroFunction,
    /// Special identifier for implementing __PRETTY_FUNCTION__
    MacroPrettyFunc,

    // <foobar> only generated by preprocessor
    MacroString,

    /// Integer literal tokens generated by preprocessor.
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
    KeywordGccInline1,
    KeywordGccInline2,
    KeywordGccVolatile1,
    KeywordGccVolatile2,
    KeywordGccRestrict1,
    KeywordGccRestrict2,
    KeywordGccAlignof1,
    KeywordGccAlignof2,
    KeywordGccTypeof,
    KeywordAttribute1,
    KeywordAttribute2,
    KeywordGccExtension,
    KeywordGccAsm,
    KeywordGccAsm1,
    KeywordGccAsm2,
    KeywordFloat80,
    KeywordFloat128,
    KeywordInt128,
    KeywordImag1,
    KeywordImag2,
    KeywordReal1,
    KeywordReal2,

    // clang keywords
    KeywordFp16,

    // gcc builtins
    BuiltinChooseExpr,
    BuiltinVaArg,
    BuiltinOffsetof,

    // MS extension
    KeywordDeclSpec,
    KeywordMSInt64_,
    KeywordMSInt64__,
    KeywordMSInt32_,
    KeywordMSInt32__,
    KeywordMSInt16_,
    KeywordMSInt16__,
    KeywordMSInt8_,
    KeywordMSInt8__,

    // Preprocessor directives
    KeywordInclude,
    KeywordIncludeNext,
    KeywordDefine,
    KeywordDefined,
    KeywordUndef,
    KeywordIfdef,
    KeywordIfndef,
    KeywordElIf,
    KeywordEndIf,
    KeywordError,
    KeywordWarning,
    KeywordPragma,
    KeywordLine,
    KeywordVarArgs,

    /// This function checks if the provided `id` matches any of the predefined macro-related token types,
    /// and returns `true` if it does, indicating that it is a macro identifier.
    ///
    /// # Arguments
    /// - `id`: The `TokenType` to be checked for macro identifier status.
    ///
    /// # Returns
    /// Returns `true` if the `id` corresponds to a macro identifier, otherwise returns `false`.
    ///
    /// # Examples
    /// ```
    ///     const tokenType = .KeywordDefine;
    ///     const isMacro = isMacroIdentifier(tokenType);
    ///     assert(isMacro); // This assertion will pass since .KeywordDefine is a macro identifier.
    /// ```
    pub fn isMacroIdentifier(id: TokenType) bool {
        return switch (id) {
            .KeywordInclude,
            .KeywordIncludeNext,
            .KeywordDefine,
            .KeywordDefined,
            .KeywordIfdef,
            .KeywordUndef,
            .KeywordIfndef,
            .KeywordElIf,
            .KeywordEndIf,
            .KeywordError,
            .KeywordWarning,
            .KeywordPragma,
            .KeywordLine,
            .KeywordVarArgs,
            .MacroFunc,
            .MacroFunction,
            .MacroPrettyFunc,
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
            .KeywordGccInline1,
            .KeywordGccInline2,
            .KeywordGccVolatile1,
            .KeywordGccVolatile2,
            .KeywordGccRestrict1,
            .KeywordGccRestrict2,
            .KeywordGccAlignof1,
            .KeywordGccAlignof2,
            .KeywordGccExtension,
            .BuiltinChooseExpr,
            .BuiltinVaArg,
            .BuiltinOffsetof,
            .KeywordAttribute1,
            .KeywordAttribute2,
            .Identifier,
            .ExtendedIdentifier,
            .KeywordGccAsm,
            .KeywordGccAsm1,
            .KeywordGccAsm2,
            .KeywordFloat80,
            .KeywordFloat128,
            .KeywordInt128,
            .KeywordFp16,
            .KeywordDeclSpec,
            .KeywordMSInt64_,
            .KeywordMSInt64__,
            .KeywordMSInt32_,
            .KeywordMSInt32__,
            .KeywordMSInt16_,
            .KeywordMSInt16__,
            .KeywordMSInt8_,
            .KeywordMSInt8__,
            => return true,
            else => false,
        };
    }

    /// Simplifies a macro-related keyword to `.Identifier`.
    ///
    /// # Arguments
    /// - `id`: A pointer to the `TokenType` to be simplified.
    ///
    /// # Note
    /// This function is designed to simplify the processing of macro-related keywords,
    /// treating them uniformly as identifiers.
    pub fn simplifyMacroKeyword(id: *TokenType) void {
        switch (id.*) {
            .KeywordInclude,
            .KeywordIncludeNext,
            .KeywordDefine,
            .KeywordDefined,
            .KeywordUndef,
            .KeywordIfdef,
            .KeywordIfndef,
            .KeywordElIf,
            .KeywordEndIf,
            .KeywordError,
            .KeywordWarning,
            .KeywordPragma,
            .KeywordLine,
            .KeywordVarArgs,
            => id.* = .Identifier,
            else => {},
        }
    }

    pub fn getTokenText(id: TokenType) ?[]const u8 {
        return switch (id) {
            .Invalid,
            .Identifier,
            .ExtendedIdentifier,
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
            .ImaginaryLiteral,
            .ImaginaryLiteral_F,
            .ImaginaryLiteral_L,
            .IntegerLiteral,
            .IntegerLiteral_U,
            .IntegerLiteral_L,
            .IntegerLiteral_LU,
            .IntegerLiteral_LL,
            .IntegerLiteral_LLU,
            .MacroString,
            .WhiteSpace,
            => null,

            .Eof,
            .NewLine,
            .MacroParam,
            .MacroParamNoExpand,
            .MacroParamHasAttribute,
            .MacroParamHasWarning,
            .MacroParamHasFeature,
            .MacroParamHasExtension,
            .MacroParamHasBuiltin,
            .MacroParamHasInclude,
            .MacroParamHasIncludeNext,
            .MacroParamIsIdentifier,
            .MacroFile,
            .MacroLine,
            .MacroCounter,
            .MacroParamPragmaOperator,
            .StringifyParam,
            .StringifyVarArgs,
            => "",

            .MacroWS => " ",

            .One => "1",
            .Zero => "0",

            .MacroFunc => "__func__",
            .MacroFunction => "__FUNCTION__",
            .MacroPrettyFunc => "__PRETTY_FUNCTION__",

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
            .ColonColon => "::",
            .Semicolon => ";",
            .Comma => ",",
            .Tilde => "~",
            .Hash => "#",
            .HashHash => "##",

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
            .KeywordGccInline1 => "__inline",
            .KeywordGccInline2 => "__inline__",
            .KeywordGccVolatile1 => "__volatile",
            .KeywordGccVolatile2 => "__volatile__",
            .KeywordGccRestrict1 => "__restrict",
            .KeywordGccRestrict2 => "__restrict__",
            .KeywordGccAlignof1 => "__alignof",
            .KeywordGccAlignof2 => "__alignof__",
            .KeywordGccExtension => "__extension__",
            .KeywordGccAsm => "asm",
            .KeywordGccAsm1 => "__asm",
            .KeywordGccAsm2 => "__asm__",
            .KeywordFloat80 => "__float80",
            .KeywordFloat128 => "__float128",
            .KeywordInt128 => "__int128",
            .KeywordImag1 => "__imag",
            .KeywordImag2 => "__imag__",
            .KeywordReal1 => "__real",
            .KeywordReal2 => "__real__",
            .KeywordFp16 => "__fp16",
            .KeywordMSInt64_ => "_int64",
            .KeywordMSInt64__ => "__int64",
            .KeywordMSInt32_ => "_int32",
            .KeywordMSInt32__ => "__int32",
            .KeywordMSInt16_ => "_int16",
            .KeywordMSInt16__ => "__int16",
            .KeywordMSInt8_ => "_int8",
            .KeywordMSInt8__ => "__int8",
            .KeywordDeclSpec => "__declspec",

            .KeywordInclude => "include",
            .KeywordIncludeNext => "include_next",
            .KeywordDefine => "define",
            .KeywordDefined => "defined",
            .KeywordUndef => "undef",
            .KeywordIfdef => "ifdef",
            .KeywordIfndef => "ifndef",
            .KeywordElIf => "elif",
            .KeywordEndIf => "endif",
            .KeywordError => "error",
            .KeywordWarning => "warning",
            .KeywordPragma => "pragma",
            .KeywordLine => "line",
            .KeywordVarArgs => "__VA_ARGS__",
            .KeywordTypeof1 => "__typeof",
            .KeywordTypeof2 => "__typeof__",
            .BuiltinChooseExpr => "__builtin_choose_expr",
            .BuiltinVaArg => "__builtin_va_arg",
            .BuiltinOffsetof => "__builtin_offsetof",
            .KeywordAttribute1 => "__attribute",
            .KeywordAttribute2 => "__attribute__",
        };
    }

    pub fn symbol(id: TokenType) []const u8 {
        return switch (id) {
            .Identifier,
            .ExtendedIdentifier,
            .MacroFunc,
            .MacroFunction,
            .MacroPrettyFunc,
            .BuiltinChooseExpr,
            .BuiltinVaArg,
            .BuiltinOffsetof,
            => "an identifier",

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

            .ImaginaryLiteral,
            .ImaginaryLiteral_F,
            .ImaginaryLiteral_L,
            => "an imaginary literal",

            .IntegerLiteral,
            .IntegerLiteral_U,
            .IntegerLiteral_L,
            .IntegerLiteral_LU,
            .IntegerLiteral_LL,
            .IntegerLiteral_LLU,
            => "an integer literal",

            else => id.getTokenText().?, // handled in getTokenText();
        };
    }

    /// tokens that can start an expression parsed by Preprocessor.expr
    /// Note: that eof, rparen, and string literals cannot actually start a
    /// preprocessor expression, but we include them here so that a nicer
    /// error message can be generated by the parser.
    pub fn validPreprocessorExprStart(id: TokenType) bool {
        return switch (id) {
            .Eof,
            .RParen,
            .StringLiteral,
            .StringLiteralUTF_8,
            .StringLiteralUTF_16,
            .StringLiteralUTF_32,
            .StringLiteralWide,

            .IntegerLiteral,
            .IntegerLiteral_U,
            .IntegerLiteral_L,
            .IntegerLiteral_LU,
            .IntegerLiteral_LL,
            .IntegerLiteral_LLU,
            .FloatLiteral,
            .FloatLiteral_F,
            .FloatLiteral_L,
            .ImaginaryLiteral,
            .ImaginaryLiteral_F,
            .ImaginaryLiteral_L,
            .CharLiteral,
            .CharLiteralUTF_16,
            .CharLiteralUTF_32,
            .CharLiteralWide,
            .LParen,
            .Plus,
            .Minus,
            .Tilde,
            .Bang,
            .Identifier,
            .ExtendedIdentifier,
            .One,
            .Zero,
            => true,
            else => false,
        };
    }
};
