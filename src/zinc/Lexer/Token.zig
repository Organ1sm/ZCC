const std = @import("std");

const CharInfo = @import("../Basic/CharInfo.zig");
const Compilation = @import("../Basic/Compilation.zig");
const LangOpts = @import("../Basic/LangOpts.zig");
const Source = @import("../Basic/Source.zig");
const TokenType = @import("../Basic/TokenType.zig").TokenType;

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
