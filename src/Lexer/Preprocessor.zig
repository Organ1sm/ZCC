const std = @import("std");
const RawToken = @import("../Lexer/Token.zig").Token;
const TokenType = @import("../Basic/TokenType.zig").TokenType;
const Compilation = @import("../Basic/Compilation.zig");
const Source = @import("../Basic/Source.zig");
const Lexer = @import("Lexer.zig");
const Parser = @import("../Parser/Parser.zig");
const Diagnostics = @import("../Basic/Diagnostics.zig");
const Token = @import("../AST/AST.zig").Token;
const AttrTag = @import("Attribute.zig").Tag;

const Allocator = std.mem.Allocator;
const Error = Compilation.Error;
const assert = std.debug.assert;

const Preprocessor = @This();
const DefineMap = std.StringHashMap(Macro);
const RawTokenList = std.ArrayList(RawToken);
const MaxIncludeDepth = 200;

const Macro = struct {
    /// Parameters of the function type macro
    params: []const []const u8,
    /// Token constituting the macro body
    tokens: []const RawToken,
    /// If the function type macro has variable number of arguments
    varArgs: bool,
    /// Is a funtion type macro
    isFunc: bool,
    /// Is a predefined macro
    isBuiltin: bool = false,
    /// Location of macro in the source
    /// `byteOffset` and `line` are used to define the range of tokens included in the macro.
    loc: Source.Location,

    fn eql(a: Macro, b: Macro, pp: *Preprocessor) bool {
        if (a.tokens.len != b.tokens.len)
            return false;

        if (a.isBuiltin != b.isBuiltin)
            return false;

        for (a.tokens, 0..) |t, i|
            if (!tokEql(pp, t, b.tokens[i]))
                return false;

        if (a.isFunc and b.isFunc) {
            if (a.varArgs != b.varArgs) return false;
            if (a.params.len != b.params.len) return false;
            for (a.params, 0..) |p, i|
                if (!std.mem.eql(u8, p, b.params[i]))
                    return false;
        }

        return true;
    }

    fn tokEql(pp: *Preprocessor, a: RawToken, b: RawToken) bool {
        return std.mem.eql(u8, pp.tokSliceSafe(a), pp.tokenSlice(b));
    }
};

compilation: *Compilation,
arena: std.heap.ArenaAllocator,
defines: DefineMap,
tokens: Token.List = .{},
generated: std.ArrayList(u8),
tokenBuffer: RawTokenList,
charBuffer: std.ArrayList(u8),
includeDepth: u8 = 0,
poisonedIdentifiers: std.StringHashMap(void),
/// used to implement __COUNTER__ Macro
counter: u32 = 0,
expansionSourceLoc: Source.Location = undefined,
/// Counter that is incremented each time preprocess() is called
/// Can be used to distinguish multiple preprocessings of the same file
preprocessCount: u32 = 0,
/// Memory is retained to avoid allocation on every single token.
topExpansionBuffer: ExpandBuffer,

const FeatureCheckMacros = struct {
    const args = [1][]const u8{"X"};
    const hasAttribute = [1]RawToken{makeFeatCheckMacro(.MacroParamHasAttribute)};
    const hasWarning = [1]RawToken{makeFeatCheckMacro(.MacroParamHasWarning)};
    const isIdentifier = [1]RawToken{makeFeatCheckMacro(.MacroParamIsIdentifier)};
    const file = [1]RawToken{makeFeatCheckMacro(.MacroFile)};
    const line = [1]RawToken{makeFeatCheckMacro(.MacroLine)};
    const counter = [1]RawToken{makeFeatCheckMacro(.MacroCounter)};

    fn makeFeatCheckMacro(id: TokenType) RawToken {
        return .{
            .id = id,
            .source = .generated,
        };
    }
};

pub fn addBuiltinMacro(pp: *Preprocessor, name: []const u8, isFunc: bool, tokens: []const RawToken) !void {
    try pp.defines.put(name, .{
        .params = &FeatureCheckMacros.args,
        .tokens = tokens,
        .varArgs = false,
        .isFunc = isFunc,
        .loc = .{ .id = .generated },
        .isBuiltin = true,
    });
}

pub fn addBuiltinMacros(pp: *Preprocessor) !void {
    try pp.addBuiltinMacro("__has_attribute", true, &FeatureCheckMacros.hasAttribute);
    try pp.addBuiltinMacro("__has_warning", true, &FeatureCheckMacros.hasWarning);
    try pp.addBuiltinMacro("__is_identifier", true, &FeatureCheckMacros.isIdentifier);
    try pp.addBuiltinMacro("__FILE__", false, &FeatureCheckMacros.file);
    try pp.addBuiltinMacro("__LINE__", false, &FeatureCheckMacros.line);
    try pp.addBuiltinMacro("__COUNTER__", false, &FeatureCheckMacros.counter);
}

pub fn init(comp: *Compilation) Preprocessor {
    const pp = Preprocessor{
        .compilation = comp,
        .arena = std.heap.ArenaAllocator.init(comp.gpa),
        .defines = DefineMap.init(comp.gpa),
        .generated = std.ArrayList(u8).init(comp.gpa),
        .tokenBuffer = RawTokenList.init(comp.gpa),
        .charBuffer = std.ArrayList(u8).init(comp.gpa),
        .poisonedIdentifiers = std.StringHashMap(void).init(comp.gpa),
        .topExpansionBuffer = ExpandBuffer.init(comp.gpa),
    };

    comp.pragmaEvent(.BeforePreprocess);
    return pp;
}

pub fn deinit(pp: *Preprocessor) void {
    pp.defines.deinit();
    for (pp.tokens.items(.expansionLocs)) |loc| {
        Token.free(loc, pp.compilation.gpa);
    }
    pp.tokens.deinit(pp.compilation.gpa);
    pp.arena.deinit();
    pp.generated.deinit();
    pp.tokenBuffer.deinit();
    pp.charBuffer.deinit();
    pp.poisonedIdentifiers.deinit();
    pp.topExpansionBuffer.deinit();
}

/// Preprocess a source file, returns eof token.
pub fn preprocess(pp: *Preprocessor, source: Source) Error!Token {
    if (source.invalidUTF8Loc) |loc| {
        try pp.compilation.diag.add(.{
            .tag = .invalid_utf8,
            .loc = loc,
        }, &.{});
        return error.FatalError;
    }

    pp.preprocessCount += 1;
    var lexer = Lexer{
        .buffer = source.buffer,
        .source = source.id,
        .comp = pp.compilation,
    };

    const TrailingWhiteSpace = " \r\t\x0B\x0C";

    // Estimate how many new tokens this source will contain.
    const estimatedTokenCount = source.buffer.len / 8;
    try pp.tokens.ensureTotalCapacity(pp.compilation.gpa, pp.tokens.len + estimatedTokenCount);

    var ifLevel: u8 = 0;
    var ifKind = std.PackedIntArray(u2, 256).init([1]u2{0} ** 256);
    const untilElse = 0;
    const untilEndIf = 1;
    const untilEndIfSeenElse = 2;

    var startOfLine = true;
    while (true) {
        var token = lexer.next();
        switch (token.id) {
            .Hash => if (startOfLine) {
                const directive = lexer.nextNoWhiteSpace();
                switch (directive.id) {
                    .KeywordError => {
                        const start = lexer.index;
                        while (lexer.index < lexer.buffer.len) : (lexer.index += 1) {
                            if (lexer.buffer[lexer.index] == '\n') break;
                        }

                        // #error message
                        var message = lexer.buffer[start..lexer.index];
                        message = std.mem.trim(u8, message, TrailingWhiteSpace);

                        try pp.compilation.diag.add(.{
                            .tag = .error_directive,
                            .loc = .{ .id = token.source, .byteOffset = token.start, .line = directive.line },
                            .extra = .{ .str = message },
                        }, &.{});
                    },

                    .KeywordIf => {
                        const ov = @addWithOverflow(ifLevel, 1);
                        if (ov[1] != 0)
                            return pp.fatal(directive, "too many #if nestings", .{});

                        ifLevel = ov[0];

                        if (try pp.expr(&lexer)) {
                            ifKind.set(ifLevel, untilEndIf);
                        } else {
                            ifKind.set(ifLevel, untilElse);
                            try pp.skip(&lexer, .untilElse);
                        }
                    },

                    .KeywordIfdef => {
                        const ov = @addWithOverflow(ifLevel, 1);
                        if (ov[1] != 0)
                            return pp.fatal(directive, "too many #if nestings", .{});

                        ifLevel = ov[0];

                        const macroName = try pp.expectMacroName(&lexer) orelse continue;
                        try pp.expectNewLine(&lexer);

                        if (pp.defines.get(macroName) != null) {
                            ifKind.set(ifLevel, untilEndIf);
                        } else {
                            ifKind.set(ifLevel, untilElse);
                            try pp.skip(&lexer, .untilElse);
                        }
                    },

                    .KeywordIfndef => {
                        const ov = @addWithOverflow(ifLevel, 1);
                        if (ov[1] != 0)
                            return pp.fatal(directive, "too many #if nestings", .{});

                        ifLevel = ov[0];

                        const macroName = try pp.expectMacroName(&lexer) orelse continue;
                        try pp.expectNewLine(&lexer);

                        if (pp.defines.get(macroName) == null) {
                            ifKind.set(ifLevel, untilEndIf);
                        } else {
                            ifKind.set(ifLevel, untilElse);
                            try pp.skip(&lexer, .untilElse);
                        }
                    },

                    .KeywordUndef => {
                        const macro_name = (try pp.expectMacroName(&lexer)) orelse continue;
                        _ = pp.defines.remove(macro_name);
                        try pp.expectNewLine(&lexer);
                    },

                    .KeywordElIf => {
                        if (ifLevel == 0) {
                            try pp.addError(directive, .elif_without_if);
                            ifLevel += 1;
                            ifKind.set(ifLevel, untilElse);
                        }

                        switch (ifKind.get(ifLevel)) {
                            untilElse => if (try pp.expr(&lexer)) {
                                ifKind.set(ifLevel, untilEndIf);
                            } else {
                                try pp.skip(&lexer, .untilElse);
                            },

                            untilEndIf => try pp.skip(&lexer, .untilEndIf),
                            untilEndIfSeenElse => {
                                try pp.addError(directive, .elif_after_else);
                                skipToNewLine(&lexer);
                            },
                            else => unreachable,
                        }
                    },

                    .KeywordElse => {
                        try pp.expectNewLine(&lexer);

                        if (ifLevel == 0) {
                            try pp.addError(directive, .else_without_if);
                            continue;
                        }

                        switch (ifKind.get(ifLevel)) {
                            untilElse => ifKind.set(ifLevel, untilEndIfSeenElse),
                            untilEndIf => try pp.skip(&lexer, .untilEndIfSeenElse),
                            untilEndIfSeenElse => {
                                try pp.addError(directive, .else_after_else);
                                skipToNewLine(&lexer);
                            },
                            else => unreachable,
                        }
                    },

                    .KeywordEndIf => {
                        try pp.expectNewLine(&lexer);
                        if (ifLevel == 0) {
                            try pp.addError(directive, .else_without_if);
                            continue;
                        }
                        ifLevel -= 1;
                    },

                    .KeywordDefine => try pp.define(&lexer),
                    .KeywordInclude => try pp.include(&lexer),
                    .KeywordPragma => pp.pragma(&lexer, directive) catch |err| switch (err) {
                        // this cannot occur in the main file so it is guaranteed to be ignored
                        error.StopPreprocessing => return @as(Token, undefined),
                        else => |e| return e,
                    },

                    .KeywordLine => {
                        const digits = lexer.nextNoWhiteSpace();
                        if (digits.id != .IntegerLiteral)
                            try pp.addError(digits, .line_simple_digit);

                        if (digits.id == .Eof or digits.id == .NewLine)
                            continue;

                        const name = lexer.nextNoWhiteSpace();
                        if (name.id == .Eof or name.id == .NewLine)
                            continue;

                        if (name.id != .StringLiteral)
                            try pp.addError(name, .line_invalid_filename);

                        try pp.expectNewLine(&lexer);
                    },
                    .IntegerLiteral => {
                        // # number "file" flags
                        const name = lexer.nextNoWhiteSpace();
                        if (name.id == .Eof or name.id == .NewLine) continue;
                        if (name.id != .StringLiteral) try pp.addError(name, .line_invalid_filename);

                        const flag1 = lexer.nextNoWhiteSpace();
                        if (flag1.id == .Eof or flag1.id == .NewLine) continue;
                        const flag2 = lexer.nextNoWhiteSpace();
                        if (flag2.id == .Eof or flag2.id == .NewLine) continue;
                        const flag3 = lexer.nextNoWhiteSpace();
                        if (flag3.id == .Eof or flag3.id == .NewLine) continue;
                        const flag4 = lexer.nextNoWhiteSpace();
                        if (flag4.id == .Eof or flag4.id == .NewLine) continue;
                        try pp.expectNewLine(&lexer);
                    },
                    .NewLine => {},
                    .Eof => {
                        if (ifLevel != 0)
                            try pp.addError(directive, .unterminated_conditional_directive);
                        return tokenFromRaw(directive);
                    },
                    else => {
                        try pp.addError(token, .invalid_preprocessing_directive);
                        try pp.expectNewLine(&lexer);
                    },
                }
            },

            .WhiteSpace => if (pp.compilation.onlyPreprocess) try pp.tokens.append(pp.compilation.gpa, tokenFromRaw(token)),
            .NewLine => {
                startOfLine = true;
                if (pp.compilation.onlyPreprocess)
                    try pp.tokens.append(pp.compilation.gpa, tokenFromRaw(token));
            },

            .Eof => {
                if (ifLevel != 0)
                    try pp.addError(token, .unterminated_conditional_directive);
                if (source.buffer.len > 0 and source.buffer[source.buffer.len - 1] != '\n')
                    try pp.addError(token, .newline_eof);

                return tokenFromRaw(token);
            },

            else => {
                if (token.id.isMacroIdentifier() and pp.poisonedIdentifiers.get(pp.tokSliceSafe(token)) != null)
                    try pp.addError(token, .poisoned_identifier);

                // add the token to the buffer do any necessary expansions
                startOfLine = false;
                token.id.simplifyMacroKeyword();
                try pp.expandMacro(&lexer, token);
            },
        }
    }
}

pub fn tokSliceSafe(pp: *Preprocessor, token: RawToken) []const u8 {
    if (token.id.lexeMe()) |some| return some;

    std.debug.assert(token.source != .generated);
    return pp.compilation.getSource(token.source).buffer[token.start..token.end];
}

// Returned slice is invalidated when generated is updated.
pub fn tokenSlice(pp: *Preprocessor, token: RawToken) []const u8 {
    if (token.id.lexeMe()) |some| return some;

    if (token.source == .generated) {
        return pp.generated.items[token.start..token.end];
    } else {
        const source = pp.compilation.getSource(token.source);
        return source.buffer[token.start..token.end];
    }
}

/// Convert a token from the Tokenizer into a token used by the parser.
fn tokenFromRaw(raw: RawToken) Token {
    return .{
        .id = raw.id,
        .loc = .{
            .id = raw.source,
            .byteOffset = raw.start,
            .line = raw.line,
        },
    };
}

fn addError(pp: *Preprocessor, raw: RawToken, tag: Diagnostics.Tag) !void {
    try pp.compilation.diag.add(.{
        .tag = tag,
        .loc = .{
            .id = raw.source,
            .byteOffset = raw.start,
            .line = raw.line,
        },
    }, &.{});
}

fn fatal(pp: *Preprocessor, raw: RawToken, comptime fmt: []const u8, args: anytype) Compilation.Error {
    const source = pp.compilation.getSource(raw.source);
    const lineAndCol = source.getLineCol(raw.start);
    return pp.compilation.diag.fatal(source.path, lineAndCol.line, raw.line, lineAndCol.col, fmt, args);
}

/// Consume next token, error if it is not an identifier.
fn expectMacroName(pp: *Preprocessor, lexer: *Lexer) Error!?[]const u8 {
    const macroName = lexer.nextNoWhiteSpace();
    if (!macroName.id.isMacroIdentifier()) {
        try pp.addError(macroName, .macro_name_missing);
        skipToNewLine(lexer);
        return null;
    }

    return pp.tokSliceSafe(macroName);
}

/// Skip until after a newline, error if extra tokens before it.
fn expectNewLine(pp: *Preprocessor, lexer: *Lexer) Error!void {
    var sentErr = false;
    while (true) {
        const token = lexer.next();
        if (token.id == .NewLine or token.id == .Eof)
            return;
        if (token.id == .WhiteSpace)
            continue;

        if (!sentErr) {
            sentErr = true;
            try pp.addError(token, .extra_tokens_directive_end);
        }
    }
}

/// Consume all tokens until a newline and parse the result into a boolean.
fn expr(pp: *Preprocessor, lexer: *Lexer) Error!bool {
    const start = pp.tokens.len;
    defer {
        for (pp.tokens.items(.expansionLocs)[start..]) |loc|
            Token.free(loc, pp.compilation.gpa);
        pp.tokens.len = start;
    }

    while (true) {
        var token = lexer.next();
        switch (token.id) {
            .NewLine, .Eof => {
                if (pp.tokens.len == start) {
                    try pp.addError(token, .expected_value_in_expr);
                    try pp.expectNewLine(lexer);
                    return false;
                }

                token.id = .Eof;
                try pp.tokens.append(pp.compilation.gpa, tokenFromRaw(token));
                break;
            },
            .KeywordDefined => {
                const first = lexer.nextNoWhiteSpace();
                const macroToken = if (first.id == .LParen) lexer.next() else first;

                if (!macroToken.id.isMacroIdentifier())
                    try pp.addError(macroToken, .macro_name_missing);

                if (first.id == .LParen) {
                    const rParen = lexer.next();
                    if (rParen.id != .RParen) {
                        try pp.addError(rParen, .closing_paren);
                        try pp.addError(first, .to_match_paren);
                    }
                }
                token.id = if (pp.defines.get(pp.tokSliceSafe(macroToken)) != null) .One else .Zero;
            },

            .WhiteSpace => continue,
            else => {},
        }

        try pp.expandMacro(lexer, token);
    }

    if (!pp.tokens.items(.id)[start].validPreprocessorExprStart()) {
        const token = pp.tokens.get(0);
        try pp.compilation.diag.add(.{
            .tag = .invalid_preproc_expr_start,
            .loc = token.loc,
        }, token.expansionSlice());
        return false;
    }

    for (pp.tokens.items(.id)[start..], 0..) |*tok, i| {
        switch (tok.*) {
            .StringLiteral,
            .StringLiteralUTF_8,
            .StringLiteralUTF_16,
            .StringLiteralUTF_32,
            .StringLiteralWide,
            => {
                const t = pp.tokens.get(i);
                try pp.compilation.diag.add(.{
                    .tag = .string_literal_in_pp_expr,
                    .loc = t.loc,
                }, t.expansionSlice());
                return false;
            },

            .FloatLiteral,
            .FloatLiteral_F,
            .FloatLiteral_L,
            => {
                const t = pp.tokens.get(i);
                try pp.compilation.diag.add(.{
                    .tag = .float_literal_in_pp_expr,
                    .loc = t.loc,
                }, t.expansionSlice());
                return false;
            },

            .PlusPlus,
            .MinusMinus,
            .PlusEqual,
            .MinusEqual,
            .AsteriskEqual,
            .SlashEqual,
            .PercentEqual,
            .AngleBracketAngleBracketLeftEqual,
            .AngleBracketAngleBracketRightEqual,
            .AmpersandEqual,
            .CaretEqual,
            .PipeEqual,
            .LBracket,
            .RBracket,
            .LBrace,
            .RBrace,
            .Ellipsis,
            .Semicolon,
            .Hash,
            .HashHash,
            .Equal,
            .Arrow,
            .Period,
            => {
                const t = pp.tokens.get(i);
                try pp.compilation.diag.add(.{
                    .tag = .invalid_preproc_operator,
                    .loc = t.loc,
                }, t.expansionSlice());
                return false;
            },

            else => if (tok.isMacroIdentifier()) {
                tok.* = .Zero; // undefined macro
            },
        }
    }

    var parser = Parser{
        .pp = pp,
        .tokenIds = pp.tokens.items(.id),
        .index = @intCast(start),
        .arena = pp.arena.allocator(),
        .inMacro = true,
        .scopes = undefined,
        .data = undefined,
        .labels = undefined,
        .strings = undefined,
        .valueMap = undefined,
        .declBuffer = undefined,
        .listBuffer = undefined,
        .paramBuffer = undefined,
        .enumBuffer = undefined,
        .recordBuffer = undefined,
        .attrBuffer = undefined,
    };

    return parser.macroExpr();
}

fn skip(
    pp: *Preprocessor,
    lexer: *Lexer,
    cont: enum { untilElse, untilEndIf, untilEndIfSeenElse },
) Error!void {
    var ifsSeen: u32 = 0;
    var lineStart = true;
    while (lexer.index < lexer.buffer.len) {
        if (lineStart) {
            const savedLexer = lexer.*;
            const hash = lexer.nextNoWhiteSpace();

            if (hash.id == .NewLine) continue;
            lineStart = false;
            if (hash.id != .Hash) continue;

            const directive = lexer.nextNoWhiteSpace();
            switch (directive.id) {
                .KeywordElse => {
                    if (ifsSeen != 0)
                        continue;

                    if (cont == .untilEndIfSeenElse) {
                        try pp.addError(directive, .else_after_else);
                        continue;
                    }

                    lexer.* = savedLexer;
                    return;
                },

                .KeywordElIf => {
                    if (ifsSeen != 0 or cont == .untilEndIf)
                        continue;

                    if (cont == .untilEndIfSeenElse) {
                        try pp.addError(directive, .elif_after_else);
                        continue;
                    }

                    lexer.* = savedLexer;
                    return;
                },

                .KeywordEndIf => {
                    if (ifsSeen == 0) {
                        lexer.* = savedLexer;
                        return;
                    }
                    ifsSeen -= 1;
                },
                .KeywordIf, .KeywordIfdef, .KeywordIfndef => ifsSeen += 1,
                else => {},
            }
        } else if (lexer.buffer[lexer.index] == '\n') {
            lineStart = true;
            lexer.index += 1;
            lexer.line += 1;
        } else {
            lineStart = false;
            lexer.index += 1;
        }
    } else {
        const eof = lexer.next();
        return pp.addError(eof, .unterminated_conditional_directive);
    }
}

// Skip until newline, ignore other tokens.
fn skipToNewLine(lexer: *Lexer) void {
    while (true) {
        const token = lexer.next();
        if (token.id == .NewLine or token.id == .Eof) return;
    }
}

const ExpandBuffer = std.ArrayList(Token);
const MacroArguments = std.ArrayList([]const Token);

fn deinitMacroArguments(allocator: Allocator, args: *const MacroArguments) void {
    for (args.items) |item| {
        for (item) |token|
            Token.free(token.expansionLocs, allocator);
        allocator.free(item);
    }
    args.deinit();
}

fn expandObjMacro(pp: *Preprocessor, simpleMacro: *const Macro) Error!ExpandBuffer {
    var buff = ExpandBuffer.init(pp.compilation.gpa);
    try buff.ensureTotalCapacity(simpleMacro.tokens.len);

    var i: usize = 0;
    while (i < simpleMacro.tokens.len) : (i += 1) {
        const raw = simpleMacro.tokens[i];
        const token = tokenFromRaw(raw);
        switch (raw.id) {
            .HashHash => {
                var rhs = tokenFromRaw(simpleMacro.tokens[i + 1]);
                i += 1;
                while (rhs.id == .WhiteSpace) {
                    rhs = tokenFromRaw(simpleMacro.tokens[i + 1]);
                    i += 1;
                }
                try pp.pasteTokens(&buff, &.{rhs});
            },
            .WhiteSpace => if (pp.compilation.onlyPreprocess) buff.appendAssumeCapacity(token),
            .MacroFile => {
                const start = pp.generated.items.len;
                const source = pp.compilation.getSource(pp.expansionSourceLoc.id);
                try pp.generated.writer().print("\"{s}\"\n", .{source.path});

                buff.appendAssumeCapacity(try pp.makeGeneratedToken(start, .StringLiteral, token));
            },
            .MacroLine => {
                const start = pp.generated.items.len;
                try pp.generated.writer().print("{d}\n", .{pp.expansionSourceLoc.line});

                buff.appendAssumeCapacity(try pp.makeGeneratedToken(start, .IntegerLiteral, token));
            },
            .MacroCounter => {
                defer pp.counter += 1;
                const start = pp.generated.items.len;
                try pp.generated.writer().print("{d}\n", .{pp.counter});

                buff.appendAssumeCapacity(try pp.makeGeneratedToken(start, .IntegerLiteral, token));
            },
            else => buff.appendAssumeCapacity(token),
        }
    }

    return buff;
}

/// Join a possibly-parenthesized series of string literal tokens into a single string without
/// leading or trailing quotes. The returned slice is invalidated if pp.char_buf changes.
/// Returns error.ExpectedStringLiteral if parentheses are not balanced, a non-string-literal
/// is encountered, or if no string literals are encountered
/// TODO: destringize (replace all '\\' with a single `\` and all '\"' with a '"')
fn pasteStringsUnsafe(pp: *Preprocessor, toks: []const Token) ![]const u8 {
    const charBufferTop = pp.charBuffer.items.len;
    defer pp.charBuffer.items.len = charBufferTop;

    var unwrapped = toks;
    if (toks.len >= 2 and toks[0].id == .LParen and toks[toks.len - 1].id == .RParen)
        unwrapped = toks[1 .. toks.len - 1];
    if (unwrapped.len == 0)
        return error.ExpectedStringLiteral;

    for (unwrapped) |tok| {
        if (tok.id == .WhiteSpace) continue;
        if (tok.id != .StringLiteral) return error.ExpectedStringLiteral;
        const str = pp.expandedSlice(tok);
        try pp.charBuffer.appendSlice(str[1 .. str.len - 1]);
    }
    return pp.charBuffer.items[charBufferTop..];
}

fn handleBuiltinMacro(pp: *Preprocessor, builtin: TokenType, paramTokens: []const Token) Error!TokenType {
    switch (builtin) {
        .MacroParamHasAttribute => {
            var invalid: ?Token = null;
            var identifier: ?Token = null;
            for (paramTokens) |tok| switch (tok.id) {
                .Identifier, .ExtendedIdentifier => {
                    if (identifier) |_| invalid = tok else identifier = tok;
                },
                .WhiteSpace => continue,
                else => invalid = tok,
            };
            if (identifier == null and invalid == null) invalid = paramTokens[0];
            if (invalid) |some| {
                try pp.compilation.diag.add(
                    .{ .tag = .feature_check_requires_identifier, .loc = some.loc },
                    some.expansionSlice(),
                );
                return .Zero;
            }
            const attrName = pp.expandedSlice(identifier.?);
            return if (AttrTag.fromString(attrName) == null) .Zero else .One;
        },

        .MacroParamHasWarning => {
            const actualParam = pp.pasteStringsUnsafe(paramTokens) catch |er| switch (er) {
                error.ExpectedStringLiteral => {
                    try pp.compilation.diag.add(.{
                        .tag = .expected_str_literal_in,
                        .loc = paramTokens[0].loc,
                        .extra = .{ .str = "__has_warning" },
                    }, paramTokens[0].expansionSlice());
                    return .Zero;
                },
                else => |e| return e,
            };

            if (!std.mem.startsWith(u8, actualParam, "-W")) {
                try pp.compilation.diag.add(.{
                    .tag = .malformed_warning_check,
                    .loc = paramTokens[0].loc,
                    .extra = .{ .str = "__has_warning" },
                }, paramTokens[0].expansionSlice());
                return .Zero;
            }

            const warningName = actualParam[2..];
            return if (Diagnostics.warningExists(warningName)) .One else .Zero;
        },

        .MacroParamIsIdentifier => {
            var invalid: ?Token = null;
            var identifier: ?Token = null;
            for (paramTokens) |tok| switch (tok.id) {
                .WhiteSpace => continue,
                else => {
                    if (identifier) |_| invalid = tok else identifier = tok;
                },
            };
            if (identifier == null and invalid == null) invalid = paramTokens[0];
            if (invalid) |some| {
                try pp.compilation.diag.add(.{
                    .tag = .missing_tok_builtin,
                    .loc = some.loc,
                    .extra = .{ .expectedTokenId = .RParen },
                }, some.expansionSlice());
                return .Zero;
            }

            const id = identifier.?.id;
            return if (id == .Identifier or id == .ExtendedIdentifier) .One else .Zero;
        },

        else => unreachable,
    }
}

fn expandFuncMacro(
    pp: *Preprocessor,
    loc: Source.Location,
    funcMacro: *const Macro,
    args: *const MacroArguments,
    expandedArgs: *const MacroArguments,
) Error!ExpandBuffer {
    var buf = ExpandBuffer.init(pp.compilation.gpa);
    try buf.ensureTotalCapacity(funcMacro.tokens.len);

    var expandedVarArguments = ExpandBuffer.init(pp.compilation.gpa);
    var varArguments = ExpandBuffer.init(pp.compilation.gpa);

    defer {
        expandedVarArguments.deinit();
        varArguments.deinit();
    }

    if (funcMacro.varArgs) {
        var i: usize = funcMacro.params.len;
        while (i < expandedArgs.items.len) : (i += 1) {
            try varArguments.appendSlice(args.items[i]);
            try expandedVarArguments.appendSlice(expandedArgs.items[i]);
            if (i != expandedArgs.items.len - 1) {
                const comma = Token{
                    .id = .Comma,
                    .loc = .{ .id = .generated },
                };
                try varArguments.append(comma);
                try expandedVarArguments.append(comma);
            }
        }
    }

    // token concatenation and expansion phase
    var tokenIdx: usize = 0;
    while (tokenIdx < funcMacro.tokens.len) : (tokenIdx += 1) {
        const raw = funcMacro.tokens[tokenIdx];
        switch (raw.id) {
            .HashHash => {
                while (tokenIdx + 1 < funcMacro.tokens.len) {
                    const rawNext = funcMacro.tokens[tokenIdx + 1];
                    tokenIdx += 1;
                    const next = switch (rawNext.id) {
                        .WhiteSpace => continue,
                        .HashHash => continue,
                        .MacroParam, .MacroParamNoExpand => args.items[rawNext.end],
                        .KeywordVarArgs => varArguments.items,
                        else => &[1]Token{tokenFromRaw(rawNext)},
                    };
                    try pp.pasteTokens(&buf, next);
                    if (next.len != 0) break;
                }
            },

            .MacroParamNoExpand => {
                const slice = args.items[raw.end];
                const rawLoc = Source.Location{ .id = raw.source, .byteOffset = raw.start, .line = raw.line };
                try bufCopyTokens(&buf, slice, &.{rawLoc});
            },

            .MacroParam => {
                const arg = expandedArgs.items[raw.end];
                const rawLoc = Source.Location{ .id = raw.source, .byteOffset = raw.start, .line = raw.line };
                try bufCopyTokens(&buf, arg, &.{rawLoc});
            },

            .KeywordVarArgs => {
                const rawLoc = Source.Location{ .id = raw.source, .byteOffset = raw.start, .line = raw.line };
                try bufCopyTokens(&buf, expandedVarArguments.items, &.{rawLoc});
            },

            .StringifyParam, .StringifyVarArgs => {
                const arg = if (raw.id == .StringifyVarArgs)
                    varArguments.items
                else
                    args.items[raw.end];

                pp.charBuffer.clearRetainingCapacity();

                try pp.charBuffer.append('"');
                var wsState: enum { start, need, notNeed } = .start;
                for (arg) |tok| {
                    if (tok.id == .NewLine) continue;
                    if (tok.id == .WhiteSpace) {
                        if (wsState == .start) continue;
                        wsState = .need;
                        continue;
                    }
                    if (wsState == .need)
                        try pp.charBuffer.append(' ');
                    wsState = .notNeed;
                    for (pp.expandedSlice(tok)) |c| {
                        if (c == '"')
                            try pp.charBuffer.appendSlice("\\\"")
                        else if (c == '\\')
                            try pp.charBuffer.appendSlice("\\\\")
                        else
                            try pp.charBuffer.append(c);
                    }
                }
                try pp.charBuffer.appendSlice("\"\n");

                const start = pp.generated.items.len;
                try pp.generated.appendSlice(pp.charBuffer.items);

                try buf.append(try pp.makeGeneratedToken(start, .StringLiteral, tokenFromRaw(raw)));
            },

            .MacroParamHasAttribute,
            .MacroParamHasWarning,
            .MacroParamIsIdentifier,
            => {
                const arg = expandedArgs.items[0];
                if (arg.len == 0) {
                    const extra = Diagnostics.Message.Extra{ .arguments = .{ .expected = 1, .actual = 0 } };
                    try pp.compilation.diag.add(.{ .tag = .expected_arguments, .loc = loc, .extra = extra }, &.{});
                    try buf.append(.{ .id = .Zero, .loc = loc });
                    break;
                }

                const resultId = try pp.handleBuiltinMacro(raw.id, arg);
                try buf.append(.{
                    .id = resultId,
                    .loc = loc,
                });
            },

            .WhiteSpace => if (pp.compilation.onlyPreprocess)
                try buf.append(tokenFromRaw(raw)),

            else => try buf.append(tokenFromRaw(raw)),
        }
    }

    return buf;
}

fn shouldExpand(tok: Token, macro: *Macro) bool {
    if (tok.loc.id == macro.loc.id and
        tok.loc.byteOffset >= macro.loc.byteOffset and
        tok.loc.byteOffset <= macro.loc.line)
        return false;
    for (tok.expansionSlice()) |loc| {
        if (loc.id == macro.loc.id and
            loc.byteOffset >= macro.loc.byteOffset and
            loc.byteOffset <= macro.loc.line)
            return false;
    }
    return true;
}

fn bufCopyTokens(buf: *ExpandBuffer, tokens: []const Token, src: []const Source.Location) !void {
    try buf.ensureUnusedCapacity(tokens.len);
    for (tokens) |tok| {
        var copy = try tok.dupe(buf.allocator);
        try copy.addExpansionLocation(buf.allocator, src);
        buf.appendAssumeCapacity(copy);
    }
}

fn nextBufToken(
    pp: *Preprocessor,
    lexer: *Lexer,
    buf: *ExpandBuffer,
    startIdx: *usize,
    endIdx: *usize,
    extendbuffer: bool,
) Error!Token {
    startIdx.* += 1;
    if (startIdx.* == buf.items.len and startIdx.* >= endIdx.*) {
        if (extendbuffer) {
            const rawToken = lexer.next();
            if (rawToken.id.isMacroIdentifier() and pp.poisonedIdentifiers.get(pp.tokSliceSafe(rawToken)) != null)
                try pp.addError(rawToken, .poisoned_identifier);
            const newToken = tokenFromRaw(rawToken);
            endIdx.* += 1;
            try buf.append(newToken);
            return newToken;
        } else {
            return Token{ .id = .Eof, .loc = .{ .id = .generated } };
        }
    } else {
        return buf.items[startIdx.*];
    }
}

fn collectMacroFuncArguments(
    pp: *Preprocessor,
    lexer: *Lexer,
    buf: *ExpandBuffer,
    startIdx: *usize,
    endIdx: *usize,
    extendBuffer: bool,
    isBuiltin: bool,
) Error!(?MacroArguments) {
    const nameToken = buf.items[startIdx.*];
    const savedLexer = lexer.*;
    const oldEnd = endIdx.*;

    while (true) {
        const token = try nextBufToken(pp, lexer, buf, startIdx, endIdx, extendBuffer);

        switch (token.id) {
            .NewLine, .WhiteSpace => {},
            .LParen => break,
            else => {
                if (isBuiltin) {
                    try pp.compilation.diag.add(.{
                        .tag = .missing_tok_builtin,
                        .loc = token.loc,
                        .extra = .{ .expectedTokenId = .LParen },
                    }, token.expansionSlice());
                }
                // Not a macro function call, go over normal identifier, rewind
                lexer.* = savedLexer;
                endIdx.* = oldEnd;
                return null;
            },
        }
    }

    // collect the arguments.
    var parens: u32 = 0;
    var args = MacroArguments.init(pp.compilation.gpa);
    errdefer deinitMacroArguments(pp.compilation.gpa, &args);

    var curArgument = std.ArrayList(Token).init(pp.compilation.gpa);
    defer curArgument.deinit();

    const done = false;
    while (!done) {
        const tok = try nextBufToken(pp, lexer, buf, startIdx, endIdx, extendBuffer);
        switch (tok.id) {
            .Comma => {
                if (parens == 0)
                    try args.append(try curArgument.toOwnedSlice())
                else
                    try curArgument.append(try tok.dupe(pp.compilation.gpa));
            },

            .LParen => {
                try curArgument.append(try tok.dupe(pp.compilation.gpa));
                parens += 1;
            },

            .RParen => {
                if (parens == 0) {
                    try args.append(try curArgument.toOwnedSlice());
                    break;
                } else {
                    try curArgument.append(try tok.dupe(pp.compilation.gpa));
                    parens -= 1;
                }
            },

            .Eof => {
                deinitMacroArguments(pp.compilation.gpa, &args);
                lexer.* = savedLexer;
                endIdx.* = oldEnd;
                try pp.compilation.diag.add(
                    .{ .tag = .unterminated_macro_arg_list, .loc = nameToken.loc },
                    nameToken.expansionSlice(),
                );
                return null;
            },

            else => {
                try curArgument.append(try tok.dupe(pp.compilation.gpa));
            },
        }
    }

    return args;
}

fn expandMacroExhaustive(
    pp: *Preprocessor,
    lexer: *Lexer,
    buf: *ExpandBuffer,
    startIdx: usize,
    endIdx: usize,
    extendBuffer: bool,
) Error!void {
    var movingEndIdx = endIdx;
    var advanceIdx: usize = 0;
    // rescan loop
    var doRescan = true;
    while (doRescan) {
        doRescan = false;
        // expansion loop
        var idx: usize = startIdx + advanceIdx;
        //std.debug.print("Scanning ", .{});
        //try pp.debugTokenBuf(buf.items[start_idx+advance_index .. moving_end_idx]);
        while (idx < movingEndIdx) {
            const macroToken = buf.items[idx];
            const macroEntry = pp.defines.getPtr(pp.expandedSlice(macroToken));
            if (macroEntry == null or !shouldExpand(macroToken, macroEntry.?)) {
                idx += 1;
                continue;
            }
            if (macroEntry) |macro| macroHandler: {
                if (macro.isFunc) {
                    var macroScanIdx = idx;
                    // to be saved in case this doesn't turn out to be a call
                    const args = (try pp.collectMacroFuncArguments(
                        lexer,
                        buf,
                        &macroScanIdx,
                        &movingEndIdx,
                        extendBuffer,
                        macro.isBuiltin,
                    )) orelse {
                        idx += 1;
                        break :macroHandler;
                    };
                    defer {
                        for (args.items) |item| {
                            pp.compilation.gpa.free(item);
                        }
                        args.deinit();
                    }

                    var argsCount = @as(u32, @intCast(args.items.len));
                    // if the macro has zero arguments g() args_count is still 1
                    if (argsCount == 1 and macro.params.len == 0)
                        argsCount = 0;

                    // Validate argument count.
                    const extra = Diagnostics.Message.Extra{ .arguments = .{
                        .expected = @as(u32, @intCast(macro.params.len)),
                        .actual = argsCount,
                    } };
                    if (macro.varArgs and argsCount < macro.params.len) {
                        try pp.compilation.diag.add(
                            .{ .tag = .expected_at_least_arguments, .loc = buf.items[idx].loc, .extra = extra },
                            buf.items[idx].expansionSlice(),
                        );
                        idx += 1;
                        continue;
                    }

                    if (!macro.varArgs and argsCount != macro.params.len) {
                        try pp.compilation.diag.add(
                            .{ .tag = .expected_arguments, .loc = buf.items[idx].loc, .extra = extra },
                            buf.items[idx].expansionSlice(),
                        );
                        idx += 1;
                        continue;
                    }

                    //std.debug.print("Expanding func: {s}\n", .{pp.expandedSlice(buf.items[idx])});
                    var expandedArgs = MacroArguments.init(pp.compilation.gpa);
                    defer deinitMacroArguments(pp.compilation.gpa, &expandedArgs);
                    try expandedArgs.ensureTotalCapacity(args.items.len);
                    for (args.items) |arg| {
                        var expandBuffer = ExpandBuffer.init(pp.compilation.gpa);
                        try expandBuffer.appendSlice(arg);

                        try pp.expandMacroExhaustive(lexer, &expandBuffer, 0, expandBuffer.items.len, false);
                        expandedArgs.appendAssumeCapacity(try expandBuffer.toOwnedSlice());
                    }

                    var res = try pp.expandFuncMacro(macroToken.loc, macro, &args, &expandedArgs);
                    defer res.deinit();

                    const macroExpansionLocs = macroToken.expansionSlice();
                    for (res.items) |*tok| {
                        try tok.addExpansionLocation(pp.compilation.gpa, &.{macroToken.loc});
                        try tok.addExpansionLocation(pp.compilation.gpa, macroExpansionLocs);
                    }

                    const count = macroScanIdx - idx + 1;
                    for (buf.items[idx .. idx + count]) |tok|
                        Token.free(tok.expansionLocs, pp.compilation.gpa);
                    try buf.replaceRange(idx, count, res.items);
                    // TODO: moving_end_idx += res.items.len - (macro_scan_idx-idx+1)
                    // doesn't work when the RHS is negative (unsigned!)
                    movingEndIdx = movingEndIdx + res.items.len - count;
                    idx += res.items.len;
                    doRescan = true;
                } else {
                    const res = try pp.expandObjMacro(macro);
                    defer res.deinit();

                    const macroExpansionLocs = macroToken.expansionSlice();
                    for (res.items) |*tok| {
                        try tok.addExpansionLocation(pp.compilation.gpa, &.{macroToken.loc});
                        try tok.addExpansionLocation(pp.compilation.gpa, macroExpansionLocs);
                    }

                    Token.free(buf.items[idx].expansionLocs, pp.compilation.gpa);

                    try buf.replaceRange(idx, 1, res.items);
                    idx += res.items.len;
                    movingEndIdx = movingEndIdx + res.items.len - 1;
                    doRescan = true;
                }
            }
            if (idx - startIdx == advanceIdx + 1 and !doRescan) {
                advanceIdx += 1;
                //std.debug.print("Advancing start index by {}\n", .{advanceIdx});
            }
        } // end of replacement phase
    }
    // end of scanning phase

    // trim excess buffer
    buf.items.len = movingEndIdx;
}

/// Try to expand a macro after a possible candidate has been read from the `tokenizer`
/// into the `raw` token passed as argument
fn expandMacro(pp: *Preprocessor, lexer: *Lexer, raw: RawToken) Error!void {
    const sourceToken = tokenFromRaw(raw);
    if (!raw.id.isMacroIdentifier())
        return pp.tokens.append(pp.compilation.gpa, sourceToken);

    pp.topExpansionBuffer.items.len = 0;
    try pp.topExpansionBuffer.append(sourceToken);
    pp.expansionSourceLoc = sourceToken.loc;

    try pp.expandMacroExhaustive(lexer, &pp.topExpansionBuffer, 0, 1, true);
    try pp.tokens.ensureUnusedCapacity(pp.compilation.gpa, pp.topExpansionBuffer.items.len);
    for (pp.topExpansionBuffer.items) |*tok| {
        if (tok.id == .WhiteSpace and !pp.compilation.onlyPreprocess) {
            Token.free(tok.expansionLocs, pp.compilation.gpa);
            continue;
        }
        pp.tokens.appendAssumeCapacity(tok.*);
    }
}

// TODO there are like 5 tokSlice functions, can we combine them somehow.
pub fn expandedSlice(pp: *Preprocessor, token: Token) []const u8 {
    if (token.id.lexeMe()) |some|
        return some;

    var lexer = Lexer{
        .buffer = if (token.loc.id == .generated)
            pp.generated.items
        else
            pp.compilation.getSource(token.loc.id).buffer,

        .comp = pp.compilation,
        .index = token.loc.byteOffset,
        .source = .generated,
    };

    if (token.id == .MacroString) {
        while (true) : (lexer.index += 1) {
            if (lexer.buffer[lexer.index] == '>') break;
        }
        return lexer.buffer[token.loc.byteOffset .. lexer.index + 1];
    }

    const res = lexer.next();
    return lexer.buffer[res.start..res.end];
}

/// Concat two tokens and add the result to pp.generated
fn pasteTokens(pp: *Preprocessor, lhsTokens: *ExpandBuffer, rhsTokens: []const Token) Error!void {
    const lhs = while (lhsTokens.popOrNull()) |lhs| {
        if (lhs.id == .WhiteSpace)
            Token.free(lhs.expansionLocs, pp.compilation.gpa)
        else
            break lhs;
    } else {
        return bufCopyTokens(lhsTokens, rhsTokens, &.{});
    };

    var rhsRest: u32 = 1;
    const rhs = for (rhsTokens) |rhs| {
        if (rhs.id != .WhiteSpace) break rhs;
        rhsRest += 1;
    } else {
        return lhsTokens.appendAssumeCapacity(lhs);
    };
    defer Token.free(lhs.expansionLocs, pp.compilation.gpa);

    const start = pp.generated.items.len;
    const end = start + pp.expandedSlice(lhs).len + pp.expandedSlice(rhs).len;
    try pp.generated.ensureTotalCapacity(end + 1); // +1 for a newline

    // We cannot use the same slices here since they might be invalidated by `ensureCapacity`
    pp.generated.appendSliceAssumeCapacity(pp.expandedSlice(lhs));
    pp.generated.appendSliceAssumeCapacity(pp.expandedSlice(rhs));
    pp.generated.appendAssumeCapacity('\n');

    // Try to tokenize the result.
    var lexer = Lexer{
        .buffer = pp.generated.items,
        .comp = pp.compilation,
        .index = @as(u32, @intCast(start)),
        .source = .generated,
    };

    const pastedToken = lexer.nextNoWhiteSpace();
    const next = lexer.nextNoWhiteSpace().id;

    if (next != .NewLine and next != .Eof) {
        try pp.compilation.diag.add(.{
            .tag = .pasting_formed_invalid,
            .loc = .{ .id = lhs.loc.id, .byteOffset = lhs.loc.byteOffset },
            .extra = .{ .str = try pp.arena.allocator().dupe(u8, pp.generated.items[start..end]) },
        }, lhs.expansionSlice());
    }
    try lhsTokens.append(try pp.makeGeneratedToken(start, pastedToken.id, lhs));
    try bufCopyTokens(lhsTokens, rhsTokens[rhsRest..], &.{});
}

fn makeGeneratedToken(pp: *Preprocessor, start: usize, id: TokenType, source: Token) !Token {
    var pastedToken = Token{
        .id = id,
        .loc = .{
            .id = .generated,
            .byteOffset = @as(u32, @intCast(start)),
        },
    };
    try pastedToken.addExpansionLocation(pp.compilation.gpa, &.{source.loc});
    try pastedToken.addExpansionLocation(pp.compilation.gpa, source.expansionSlice());
    return pastedToken;
}

/// Defines a new macro and warns  if it  is a duplicate
fn defineMacro(pp: *Preprocessor, nameToken: RawToken, macro: Macro) Error!void {
    const name = pp.tokSliceSafe(nameToken);
    const gop = try pp.defines.getOrPut(name);
    if (gop.found_existing and !gop.value_ptr.eql(macro, pp)) {
        try pp.compilation.diag.add(.{
            .tag = if (gop.value_ptr.isBuiltin) .builtin_macro_redefined else .macro_redefined,
            .loc = .{ .id = nameToken.source, .byteOffset = nameToken.start, .line = nameToken.line },
            .extra = .{ .str = name },
        }, &.{});
    }

    gop.value_ptr.* = macro;
}

/// Handle #define directive
fn define(pp: *Preprocessor, lexer: *Lexer) Error!void {
    // get the macro name and validate.
    const macroName = lexer.nextNoWhiteSpace();
    if (macroName.id == .KeywordDefined) {
        try pp.addError(macroName, .defined_as_macro_name);
        return skipToNewLine(lexer);
    }

    if (!macroName.id.isMacroIdentifier()) {
        try pp.addError(macroName, .macro_name_must_be_identifier);
        return skipToNewLine(lexer);
    }

    var first = lexer.next();
    switch (first.id) {
        .NewLine, .Eof => return pp.defineMacro(macroName, .{
            .params = undefined,
            .tokens = undefined,
            .varArgs = false,
            .isFunc = false,
            .loc = undefined,
        }),
        .WhiteSpace => first = lexer.next(),
        .LParen => return pp.defineFunc(lexer, macroName, first),
        else => try pp.addError(first, .whitespace_after_macro_name),
    }

    if (first.id == .HashHash) {
        try pp.addError(first, .hash_hash_at_start);
        return skipToNewLine(lexer);
    }
    first.id.simplifyMacroKeyword();

    pp.tokenBuffer.items.len = 0;

    var endIdx: u32 = undefined;
    // Collect the token body and validate any ## found.
    var token = first;
    while (true) {
        token.id.simplifyMacroKeyword();
        switch (token.id) {
            .HashHash => {
                const next = lexer.nextNoWhiteSpace();
                switch (next.id) {
                    .NewLine, .Eof => {
                        try pp.addError(token, .hash_hash_at_end);
                        return;
                    },
                    .HashHash => {
                        try pp.addError(next, .hash_hash_at_end);
                        return;
                    },
                    else => {},
                }
                try pp.tokenBuffer.append(token);
                try pp.tokenBuffer.append(next);
            },
            .NewLine, .Eof => {
                endIdx = token.start;
                break;
            },
            else => try pp.tokenBuffer.append(token),
        }
        token = lexer.next();
    }

    const list = try pp.arena.allocator().dupe(RawToken, pp.tokenBuffer.items);
    try pp.defineMacro(macroName, .{
        .loc = .{ .id = macroName.source, .byteOffset = macroName.start, .line = endIdx },
        .tokens = list,
        .params = undefined,
        .isFunc = false,
        .varArgs = false,
    });
}

/// Handle a function like #define directive
fn defineFunc(pp: *Preprocessor, lexer: *Lexer, macroName: RawToken, lParen: RawToken) Error!void {
    assert(macroName.id.isMacroIdentifier());
    var params = std.ArrayList([]const u8).init(pp.compilation.gpa);
    defer params.deinit();

    // parse the parameter list
    var varArgs = false;
    var startIdx: u32 = undefined;
    while (true) {
        var token = lexer.next();
        if (token.id == .WhiteSpace) continue;
        if (token.id == .RParen) {
            startIdx = token.end;
            break;
        }

        if (params.items.len != 0) {
            if (token.id != .Comma) {
                try pp.addError(token, .invalid_token_param_list);
                return skipToNewLine(lexer);
            } else token = lexer.nextNoWhiteSpace();
        }

        if (token.id == .Eof)
            return pp.addError(token, .unterminated_macro_param_list);

        if (token.id == .Ellipsis) {
            varArgs = true;
            const rParen = lexer.nextNoWhiteSpace();
            if (rParen.id != .RParen) {
                try pp.addError(rParen, .missing_paren_param_list);
                try pp.addError(lParen, .to_match_paren);
                return skipToNewLine(lexer);
            }

            break;
        }

        if (!token.id.isMacroIdentifier()) {
            try pp.addError(token, .invalid_token_param_list);
            return skipToNewLine(lexer);
        }

        try params.append(pp.tokSliceSafe(token));
    }

    var endIdx: u32 = undefined;
    // Collect the body tokens and validate # and ##'s found.
    pp.tokenBuffer.items.len = 0;
    tokenLoop: while (true) {
        var token = lexer.next();
        switch (token.id) {
            .NewLine, .Eof => {
                endIdx = token.start;
                break;
            },
            .WhiteSpace => if (pp.tokenBuffer.items.len != 0) try pp.tokenBuffer.append(token),
            .Hash => {
                const param = lexer.nextNoWhiteSpace();
                blk: {
                    if (varArgs and param.id == .KeywordVarArgs) {
                        token.id = .StringifyVarArgs;
                        try pp.tokenBuffer.append(token);
                        continue :tokenLoop;
                    }

                    if (!param.id.isMacroIdentifier())
                        break :blk;

                    const s = pp.tokSliceSafe(param);
                    for (params.items, 0..) |p, i| {
                        if (std.mem.eql(u8, p, s)) {
                            token.id = .StringifyParam;
                            token.end = @intCast(i);
                            try pp.tokenBuffer.append(token);

                            continue :tokenLoop;
                        }
                    }
                }

                try pp.addError(param, .hash_not_followed_param);
                return skipToNewLine(lexer);
            },

            .HashHash => {
                // if "##" appear at the beginning, the token buffer is still empty in this case
                // emit error
                if (pp.tokenBuffer.items.len == 0) {
                    try pp.addError(token, .hash_hash_at_start);
                    return skipToNewLine(lexer);
                }

                const savedLexer = lexer.*;
                const next = lexer.nextNoWhiteSpace();
                if (next.id == .NewLine or next.id == .Eof) {
                    try pp.addError(token, .hash_hash_at_end);
                    return;
                }

                lexer.* = savedLexer;
                // convert the previous token to .macro_param_no_expand if it was .macro_param
                if (pp.tokenBuffer.items[pp.tokenBuffer.items.len - 1].id == .MacroParam) {
                    pp.tokenBuffer.items[pp.tokenBuffer.items.len - 1].id = .MacroParamNoExpand;
                }
                try pp.tokenBuffer.append(token);
            },

            else => {
                if (varArgs and token.id == .KeywordVarArgs) {
                    // do nothing
                } else if (token.id.isMacroIdentifier()) {
                    token.id.simplifyMacroKeyword();
                    const s = pp.tokSliceSafe(token);
                    for (params.items, 0..) |param, i| {
                        if (std.mem.eql(u8, param, s)) {
                            // NOTE: it doesn't matter to assign .macro_param_no_expand
                            // here in case a ## was the previous token, because
                            // ## processing will eat this token with the same semantics
                            token.id = .MacroParam;
                            token.end = @intCast(i);
                            break;
                        }
                    }
                }

                try pp.tokenBuffer.append(token);
            },
        }
    }

    const paramList = try pp.arena.allocator().dupe([]const u8, params.items);
    const tokenList = try pp.arena.allocator().dupe(RawToken, pp.tokenBuffer.items);
    try pp.defineMacro(macroName, .{
        .isFunc = true,
        .params = paramList,
        .varArgs = varArgs,
        .tokens = tokenList,
        .loc = .{ .id = macroName.source, .byteOffset = macroName.start, .line = endIdx },
    });
}

fn include(pp: *Preprocessor, lexer: *Lexer) Error!void {
    const newSource = pp.findIncludeSource(lexer) catch |er| switch (er) {
        error.InvalidInclude => return,
        else => |e| return e,
    };

    pp.includeDepth += 1;
    defer pp.includeDepth -= 1;
    if (pp.includeDepth > MaxIncludeDepth)
        return;

    _ = try pp.preprocess(newSource);
}

/// Handle a pragma directive
fn pragma(pp: *Preprocessor, lexer: *Lexer, pragmaToken: RawToken) !void {
    const nameToken = lexer.nextNoWhiteSpace();
    if (nameToken.id == .NewLine or nameToken.id == .Eof)
        return;

    const name = pp.tokenSlice(nameToken);

    try pp.tokens.append(pp.compilation.gpa, tokenFromRaw(pragmaToken));
    const pragmaStart = @as(u32, @intCast(pp.tokens.len));
    try pp.tokens.append(pp.compilation.gpa, tokenFromRaw(nameToken));

    while (true) {
        const nextToken = lexer.next();
        if (nextToken.id == .WhiteSpace) continue;
        if (nextToken.id == .Eof) {
            try pp.tokens.append(pp.compilation.gpa, .{
                .id = .NewLine,
                .loc = .{ .id = .generated },
            });
            break;
        }
        try pp.tokens.append(pp.compilation.gpa, tokenFromRaw(nextToken));
        if (nextToken.id == .NewLine)
            break;
    }

    if (pp.compilation.getPragma(name)) |prag| unknown: {
        return prag.preprocessorCB(pp, pragmaStart) catch |err| switch (err) {
            error.UnknownPragma => break :unknown,
            else => |e| return e,
        };
    }
    return pp.compilation.diag.add(.{
        .tag = .unsupported_pragma,
        .loc = .{ .id = nameToken.source, .byteOffset = nameToken.start, .line = nameToken.line },
        .extra = .{ .str = name },
    }, &.{});
}

fn findIncludeSource(pp: *Preprocessor, lexer: *Lexer) !Source {
    const start = pp.tokens.len;
    defer pp.tokens.len = start;

    var first = lexer.nextNoWhiteSpace();
    if (first.id == .AngleBracketLeft) to_end: {
        while (lexer.index < lexer.buffer.len) : (lexer.index += 1) {
            switch (lexer.buffer[lexer.index]) {
                '>' => {
                    lexer.index += 1;
                    first.end = lexer.index;
                    first.id = .MacroString;
                    break :to_end;
                },
                '\n' => break,
                else => {},
            }
        }

        try pp.compilation.diag.add(.{ .tag = .header_str_closing, .loc = .{ .id = first.source, .byteOffset = first.start } }, &.{});
        try pp.addError(first, .header_str_match);
    }

    // Try expand if the argument is a macro
    try pp.expandMacro(lexer, first);

    // check that we actually got a string
    const fileNameTK = pp.tokens.get(start);
    switch (fileNameTK.id) {
        .StringLiteral, .MacroString => {},
        else => {
            try pp.addError(first, .expected_filename);
            try pp.expectNewLine(lexer);
            return error.InvalidInclude;
        },
    }

    // error on the extra tokens.
    const newLine = lexer.nextNoWhiteSpace();
    if ((newLine.id != .NewLine and newLine.id != .Eof) or pp.tokens.len > start + 1) {
        skipToNewLine(lexer);
        try pp.addError(first, .extra_tokens_directive_end);
    }

    // check for empty filename
    const tkSlice = pp.expandedSlice(fileNameTK);
    if (tkSlice.len < 3) {
        try pp.addError(first, .empty_filename);
        return error.InvalidInclude;
    }

    // find the file
    const filename = tkSlice[1 .. tkSlice.len - 1];
    return (try pp.compilation.findInclude(first, filename, fileNameTK.id == .StringLiteral)) orelse
        pp.fatal(first, "'{s}' not found", .{filename});
}

/// pretty print tokens and try to preserve whitespace
pub fn prettyPrintTokens(pp: *Preprocessor, w: anytype) !void {
    var i: u32 = 0;
    while (true) : (i += 1) {
        var cur: Token = pp.tokens.get(i);
        switch (cur.id) {
            .Eof => break,
            .NewLine => try w.writeAll("\n"),
            .KeywordPragma => {
                const pragmaName = pp.expandedSlice(pp.tokens.get(i + 1));
                const endIdx = std.mem.indexOfScalarPos(TokenType, pp.tokens.items(.id), i, .NewLine) orelse i + 1;
                const pragmaLen = @as(u32, @intCast(endIdx)) - i;

                if (pp.compilation.getPragma(pragmaName)) |prag| {
                    if (!prag.shouldPreserveTokens(pp, i + 1)) {
                        i += pragmaLen;
                        cur = pp.tokens.get(i);
                        continue;
                    }
                }
                try w.writeAll("#pragma");
                i += 1;
                while (true) : (i += 1) {
                    cur = pp.tokens.get(i);
                    if (cur.id == .NewLine) {
                        try w.writeByte('\n');
                        break;
                    }
                    try w.writeByte(' ');
                    const slice = pp.expandedSlice(cur);
                    try w.writeAll(slice);
                }
            },

            else => {
                const slice = pp.expandedSlice(cur);
                try w.writeAll(slice);
            },
        }
    }
}

// For debug Preprocessor
fn printDefineMap(pp: *Preprocessor) !void {
    var kiter = pp.defines.keyIterator();
    var k = kiter.next();
    while (k != null) : (k = kiter.next()) {
        const valptr = pp.defines.getPtr(k.?.*);
        std.debug.print("{s}: [{*}]\n", .{ k.?.*, valptr });
    }
    std.debug.print("\n", .{});
}

fn debugTokenBuf(pp: *Preprocessor, buf: []const Token) !void {
    var i: u64 = 0;
    while (i < buf.len) : (i += 1) {
        const slice = pp.expandedSlice(buf[i]);
        if (std.mem.eql(u8, slice, " ")) {
            std.debug.print("⎵", .{});
        } else if (std.mem.eql(u8, slice, "")) {
            std.debug.print("({})", .{buf[i].id});
        } else {
            std.debug.print("{s} ", .{slice});
        }
    }
    std.debug.print("[{} tokens]\n", .{buf.len});
}

fn addTestSource(pp: *Preprocessor, path: []const u8, content: []const u8) !Source {
    const dupedPath = try pp.compilation.gpa.dupe(u8, path);
    const dupedContent = try pp.compilation.gpa.dupe(u8, content);

    errdefer {
        pp.compilation.gpa.free(dupedPath);
        pp.compilation.gpa.free(dupedContent);
    }

    const source = Source{
        .id = @as(Source.Id, @enumFromInt(pp.compilation.sources.count() + 2)),
        .path = dupedPath,
        .buf = dupedContent,
    };

    try pp.compilation.sources.put(dupedPath, source);
    return source;
}

test "Preserve pragma tokens sometimes" {
    const allocator = std.testing.allocator;
    const Test = struct {
        fn runPreprocessor(source_text: []const u8) ![]const u8 {
            var buf = std.ArrayList(u8).init(allocator);
            defer buf.deinit();

            var comp = Compilation.init(allocator);
            defer comp.deinit();
            comp.onlyPreprocess = true;

            try comp.addDefaultPragmaHandlers();

            var pp = Preprocessor.init(&comp);
            defer pp.deinit();

            const test_runner_macros = blk: {
                const duped_path = try allocator.dupe(u8, "<test_runner>");
                errdefer comp.gpa.free(duped_path);

                const contents = try allocator.dupe(u8, source_text);
                errdefer comp.gpa.free(contents);

                const source = Source{
                    .id = @as(Source.ID, @enumFromInt(comp.sources.count() + 2)),
                    .path = duped_path,
                    .buffer = contents,
                };
                try comp.sources.put(duped_path, source);
                break :blk source;
            };

            const eof = try pp.preprocess(test_runner_macros);
            try pp.tokens.append(pp.compilation.gpa, eof);

            try pp.prettyPrintTokens(buf.writer());
            return allocator.dupe(u8, buf.items);
        }

        fn check(source_text: []const u8, expected: []const u8) !void {
            const output = try runPreprocessor(source_text);
            defer allocator.free(output);

            try std.testing.expectEqualStrings(expected, output);
        }
    };

    const preserve_gcc_diagnostic =
        \\#pragma GCC diagnostic error "-Wnewline-eof"
        \\#pragma GCC warning error "-Wnewline-eof"
        \\int x;
        \\#pragma GCC ignored error "-Wnewline-eof"
        \\
    ;
    try Test.check(preserve_gcc_diagnostic, preserve_gcc_diagnostic);

    const omit_once =
        \\#pragma once
        \\int x;
        \\#pragma once
        \\
    ;
    try Test.check(omit_once, "int x;\n");

    const omit_poison =
        \\#pragma GCC poison foobar
        \\
    ;
    try Test.check(omit_poison, "");
}
