const std = @import("std");
const TokenType = @import("../Basic/TokenType.zig").TokenType;
const Source = @import("../Basic/Source.zig");
const LangOpts = @import("../Basic/LangOpts.zig");
const Compilation = @import("../Basic/Compilation.zig");
const CharInfo = @import("../Basic/CharInfo.zig");

pub const Token = struct {
    id: TokenType,
    source: Source.ID,
    start: u32,
    end: u32,

    /// double underscore and underscore + capital letter identifiers
    /// belong to the implementation namespace, so we always convert them
    /// to keywords.
    /// TODO: add `.keyword_asm` here as GNU extension once that is supported.
    pub fn getTokenId(comp: *const Compilation, str: []const u8) TokenType {
        const kw = AllKeywords.get(str) orelse return .Identifier;
        const standard = comp.langOpts.standard;
        return switch (kw) {
            .KeywordInline => if (standard.isGNU() or standard.atLeast(.c99)) kw else .Identifier,
            .KeywordRestrict => if (standard.atLeast(.c99)) kw else .Identifier,
            .KeywordGccTypeof => if (standard.isGNU()) kw else .Identifier,
            else => kw,
        };
    }

    /// Check if codepoint may appear in specified context
    /// does not check basic character set chars because the tokenizer handles them separately to keep the common
    /// case on the fast path
    pub fn mayAppearInIdent(comp: *const Compilation, codepoint: u21, where: enum { start, inside }) bool {
        return switch (where) {
            .start => if (comp.langOpts.standard.atLeast(.c11))
                CharInfo.isC11IdChar(codepoint) and !CharInfo.isC11DisallowedInitialIdChar(codepoint)
            else
                CharInfo.isC99IdChar(codepoint) and !CharInfo.isC99DisallowedInitialIDChar(codepoint),
            .inside => if (comp.langOpts.standard.atLeast(.c11))
                CharInfo.isC11IdChar(codepoint)
            else
                CharInfo.isC99IdChar(codepoint),
        };
    }

    pub const AllKeywords = std.ComptimeStringMap(TokenType, .{
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
        .{ "define", .KeywordDefine },
        .{ "defined", .KeywordDefined },
        .{ "undef", .KeywordUndef },
        .{ "ifdef", .KeywordIfdef },
        .{ "ifndef", .KeywordIfndef },
        .{ "elif", .KeywordElIf },
        .{ "endif", .KeywordEndIf },
        .{ "error", .KeywordError },
        .{ "pragma", .KeywordPragma },
        .{ "line", .KeywordLine },
        .{ "__VA_ARGS__", .KeywordVarArgs },

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

        // gcc builtins
        .{ "__builtin_choose_expr", .BuiltinChooseExpr },
        .{ "__attribute", .KeywordAttribute1 },
        .{ "__attribute__", .KeywordAttribute2 },
    });
};
