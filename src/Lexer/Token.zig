const std = @import("std");
const TokenType = @import("../Basic/TokenType.zig").TokenType;
const Source = @import("../Basic/Source.zig");
const LangOpts = @import("../Basic/LangOpts.zig");
const Compilation = @import("../Basic/Compilation.zig");
const CharInfo = @import("../Basic/CharInfo.zig");

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

    /// Maps a token string to a TokenType based on keywords
    /// and current language standard.
    ///
    /// Special cases like inline, restrict have different keyword
    /// status depending on standards. These are checked before
    /// mapping.
    ///
    /// Returns final TokenType for further processing.
    pub fn getTokenId(comp: *const Compilation, str: []const u8) TokenType {
        const kw = AllKeywords.get(str) orelse return .Identifier;

        // Retrieve the language standard from the Compilation context
        const standard = comp.langOpts.standard;
        return switch (kw) {
            .KeywordInline => if (standard.isGNU() or standard.atLeast(.c99)) kw else .Identifier,
            .KeywordRestrict => if (standard.atLeast(.c99)) kw else .Identifier,
            .KeywordGccTypeof => if (standard.isGNU()) kw else .Identifier,
            .KeywordGccAsm => if (standard.isGNU()) kw else .Identifier,
            .KeywordDeclSpec => if (comp.langOpts.msExtensions) kw else .Identifier,
            else => kw,
        };
    }

    pub const AllKeywords = std.ComptimeStringMap(TokenType, .{
        .{ "auto", auto: {
            @setEvalBranchQuota(3000);
            break :auto .KeywordAuto;
        } },
        .{ "enum", .KeywordEnum },
        .{ "union", .KeywordUnion },
        .{ "struct", .KeywordStruct },

        .{ "unsigned", .KeywordUnsigned },
        .{ "signed", .KeywordSigned },
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

        // Preprocessor directives
        .{ "include", .KeywordInclude },
        .{ "include_next", .KeywordIncludeNext },
        .{ "define", .KeywordDefine },
        .{ "defined", .KeywordDefined },
        .{ "undef", .KeywordUndef },
        .{ "ifdef", .KeywordIfdef },
        .{ "ifndef", .KeywordIfndef },
        .{ "elif", .KeywordElIf },
        .{ "endif", .KeywordEndIf },
        .{ "error", .KeywordError },
        .{ "warning", .KeywordWarning },
        .{ "pragma", .KeywordPragma },
        .{ "line", .KeywordLine },
        .{ "__VA_ARGS__", .KeywordVarArgs },
        .{ "__func__", .MacroFunc },
        .{ "__FUNCTION__", .MacroFunction },
        .{ "__PRETTY_FUNCTION__", .MacroPrettyFunc },

        // gcc keywords
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
        .{ "typeof", .KeywordGccTypeof },
        .{ "__extension__", .KeywordGccExtension },
        .{ "asm", .KeywordGccAsm },
        .{ "__asm", .KeywordGccAsm1 },
        .{ "__asm__", .KeywordGccAsm2 },
        .{ "__attribute", .KeywordAttribute1 },
        .{ "__attribute__", .KeywordAttribute2 },

        // MS keywords
        .{ "__declspec", .KeywordDeclSpec },

        // gcc builtins
        .{ "__builtin_choose_expr", .BuiltinChooseExpr },
        .{ "__builtin_va_arg", .BuiltinVaArg },
        .{ "__builtin_offsetof", .BuiltinOffsetof },
    });
};
