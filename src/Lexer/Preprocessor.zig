const std = @import("std");
const RawToken = @import("../Lexer/Token.zig").Token;
const TokenType = @import("../Basic/TokenType.zig").TokenType;
const Compilation = @import("../Basic/Compilation.zig");
const Source = @import("../Basic/Source.zig");
const Lexer = @import("Lexer.zig");
const Parser = @import("../Parser/Parser.zig");
const Diagnostics = @import("../Basic/Diagnostics.zig");
const Token = @import("../AST/AST.zig").Token;
const Attribute = @import("Attribute.zig");
const Features = @import("Features.zig");

const Allocator = std.mem.Allocator;
const Error = Compilation.Error;
const assert = std.debug.assert;

const Preprocessor = @This();
const DefineMap = std.StringHashMap(Macro);
const RawTokenList = std.ArrayList(RawToken);
const MaxIncludeDepth = 200;

/// Errors that can be returned when expanding a macro.
/// error.UnknownPragma can occur within Preprocessor.pragma() but
/// it is handled there and doesn't escape that function
const MacroError = Error || error{StopPreprocessing};

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
        return std.mem.eql(u8, pp.getTokenSlice(a), pp.getTokenSlice(b));
    }
};

comp: *Compilation,
arena: std.heap.ArenaAllocator,
defines: DefineMap,
tokens: Token.List = .{},
tokenBuffer: RawTokenList,
charBuffer: std.ArrayList(u8),
includeDepth: u8 = 0,
generatedLine: u32 = 1,
addExpansionNL: u32 = 0,
poisonedIdentifiers: std.StringHashMap(void),
/// used to implement __COUNTER__ Macro
counter: u32 = 0,
expansionSourceLoc: Source.Location = undefined,
/// Counter that is incremented each time preprocess() is called
/// Can be used to distinguish multiple preprocessings of the same file
preprocessCount: u32 = 0,
/// Memory is retained to avoid allocation on every single token.
topExpansionBuffer: ExpandBuffer,

const BuiltinMacros = struct {
    const args = [1][]const u8{"X"};
    const hasAttribute = [1]RawToken{makeFeatCheckMacro(.MacroParamHasAttribute)};
    const hasWarning = [1]RawToken{makeFeatCheckMacro(.MacroParamHasWarning)};
    const hasFeature = [1]RawToken{makeFeatCheckMacro(.MacroParamHasFeature)};
    const hasExtension = [1]RawToken{makeFeatCheckMacro(.MacroParamHasExtension)};
    const hasBuiltin = [1]RawToken{makeFeatCheckMacro(.MacroParamHasBuiltin)};
    const isIdentifier = [1]RawToken{makeFeatCheckMacro(.MacroParamIsIdentifier)};
    const file = [1]RawToken{makeFeatCheckMacro(.MacroFile)};
    const line = [1]RawToken{makeFeatCheckMacro(.MacroLine)};
    const counter = [1]RawToken{makeFeatCheckMacro(.MacroCounter)};
    const pragmaOperator = [1]RawToken{makeFeatCheckMacro(.MacroParamPragmaOperator)};

    fn makeFeatCheckMacro(id: TokenType) RawToken {
        return .{
            .id = id,
            .source = .generated,
        };
    }
};

pub fn addBuiltinMacro(pp: *Preprocessor, name: []const u8, isFunc: bool, tokens: []const RawToken) !void {
    try pp.defines.put(name, .{
        .params = &BuiltinMacros.args,
        .tokens = tokens,
        .varArgs = false,
        .isFunc = isFunc,
        .loc = .{ .id = .generated },
        .isBuiltin = true,
    });
}

pub fn addBuiltinMacros(pp: *Preprocessor) !void {
    try pp.addBuiltinMacro("__has_attribute", true, &BuiltinMacros.hasAttribute);
    try pp.addBuiltinMacro("__has_warning", true, &BuiltinMacros.hasWarning);
    try pp.addBuiltinMacro("__has_feature", true, &BuiltinMacros.hasFeature);
    try pp.addBuiltinMacro("__has_extension", true, &BuiltinMacros.hasExtension);
    try pp.addBuiltinMacro("__has_builtin", true, &BuiltinMacros.hasBuiltin);
    try pp.addBuiltinMacro("__is_identifier", true, &BuiltinMacros.isIdentifier);
    try pp.addBuiltinMacro("_Pragma", true, &BuiltinMacros.pragmaOperator);
    try pp.addBuiltinMacro("__FILE__", false, &BuiltinMacros.file);
    try pp.addBuiltinMacro("__LINE__", false, &BuiltinMacros.line);
    try pp.addBuiltinMacro("__COUNTER__", false, &BuiltinMacros.counter);
}

pub fn init(comp: *Compilation) Preprocessor {
    const pp = Preprocessor{
        .comp = comp,
        .arena = std.heap.ArenaAllocator.init(comp.gpa),
        .defines = DefineMap.init(comp.gpa),
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
        Token.free(loc, pp.comp.gpa);
    }
    pp.tokens.deinit(pp.comp.gpa);
    pp.arena.deinit();
    pp.tokenBuffer.deinit();
    pp.charBuffer.deinit();
    pp.poisonedIdentifiers.deinit();
    pp.topExpansionBuffer.deinit();
}

/// Preprocess a source file, returns eof token.
pub fn preprocess(pp: *Preprocessor, source: Source) Error!Token {
    return pp.preprocessExtra(source) catch |err| switch (err) {
        // This cannot occur in the main file and is handled in `include`.
        error.StopPreprocessing => unreachable,
        else => |e| return e,
    };
}

fn preprocessExtra(pp: *Preprocessor, source: Source) MacroError!Token {
    pp.preprocessCount += 1;
    var lexer = Lexer{
        .buffer = source.buffer,
        .source = source.id,
        .comp = pp.comp,
    };

    // Estimate how many new tokens this source will contain.
    const estimatedTokenCount = source.buffer.len / 8;
    try pp.tokens.ensureTotalCapacity(pp.comp.gpa, pp.tokens.len + estimatedTokenCount);

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
                    .KeywordError, .KeywordWarning => {
                        pp.topExpansionBuffer.items.len = 0;

                        const charTop = pp.charBuffer.items.len;
                        defer pp.charBuffer.items.len = charTop;

                        while (true) {
                            token = lexer.next();
                            if (token.id == .NewLine or token.id == .Eof) break;
                            if (token.id == .WhiteSpace) token.id = .MacroWS;
                            try pp.topExpansionBuffer.append(tokenFromRaw(token));
                        }

                        try pp.stringify(pp.topExpansionBuffer.items);
                        const slice = pp.charBuffer.items[charTop + 1 .. pp.charBuffer.items.len - 2];
                        const message = try pp.comp.diag.arena.allocator().dupe(u8, slice);

                        try pp.comp.diag.add(.{
                            .tag = if (directive.id == .KeywordError) .error_directive else .warning_directive,
                            .loc = .{ .id = token.source, .byteOffset = directive.start, .line = directive.line },
                            .extra = .{ .str = message },
                        }, &.{});
                    },

                    .KeywordIf => {
                        const sum, const overflowed = @addWithOverflow(ifLevel, 1);
                        if (overflowed != 0)
                            return pp.fatal(directive, "too many #if nestings", .{});

                        ifLevel = sum;

                        if (try pp.expr(&lexer)) {
                            ifKind.set(ifLevel, untilEndIf);
                        } else {
                            ifKind.set(ifLevel, untilElse);
                            try pp.skip(&lexer, .untilElse);
                        }
                    },

                    .KeywordIfdef => {
                        const sum, const overflowed = @addWithOverflow(ifLevel, 1);
                        if (overflowed != 0)
                            return pp.fatal(directive, "too many #if nestings", .{});

                        ifLevel = sum;

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
                        const sum, const overflowed = @addWithOverflow(ifLevel, 1);
                        if (overflowed != 0)
                            return pp.fatal(directive, "too many #if nestings", .{});

                        ifLevel = sum;

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
                        const macroName = (try pp.expectMacroName(&lexer)) orelse continue;
                        _ = pp.defines.remove(macroName);
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
                    .KeywordPragma => try pp.pragma(&lexer, directive, null, &.{}),

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
                        skipToNewLine(&lexer);
                    },
                }
            },

            .WhiteSpace => if (pp.comp.onlyPreprocess) try pp.tokens.append(pp.comp.gpa, tokenFromRaw(token)),
            .NewLine => {
                startOfLine = true;
                if (pp.comp.onlyPreprocess)
                    try pp.tokens.append(pp.comp.gpa, tokenFromRaw(token));
            },

            .Eof => {
                if (ifLevel != 0)
                    try pp.addError(token, .unterminated_conditional_directive);
                if (source.buffer.len > 0 and source.buffer[source.buffer.len - 1] != '\n')
                    try pp.addError(token, .newline_eof);

                return tokenFromRaw(token);
            },

            else => {
                if (token.id.isMacroIdentifier() and pp.poisonedIdentifiers.get(pp.getTokenSlice(token)) != null)
                    try pp.addError(token, .poisoned_identifier);

                // add the token to the buffer do any necessary expansions
                startOfLine = false;
                try pp.expandMacro(&lexer, token);
            },
        }
    }
}

/// Tokenize a file without any preprocessing, returns eof token.
pub fn tokenize(pp: *Preprocessor, source: Source) Error!Token {
    var tokenizer = Lexer{
        .buffer = source.buffer,
        .comp = pp.comp,
        .source = source.id,
    };

    // Estimate how many new tokens this source will contain.
    const EstimatedTokenCount = source.buffer.len / 8;
    try pp.tokens.ensureTotalCapacity(pp.comp.gpa, pp.tokens.len + EstimatedTokenCount);

    while (true) {
        const tok = tokenizer.next();
        if (tok.id == .Eof) return tokenFromRaw(tok);
        try pp.tokens.append(pp.comp.gpa, tokenFromRaw(tok));
    }
}

/// Get raw token source string.
/// Returned slice is invalidated when comp.generatedBuffer is updated.
pub fn getTokenSlice(pp: *Preprocessor, token: RawToken) []const u8 {
    if (token.id.getTokenText()) |some|
        return some;

    const source = pp.comp.getSource(token.source);
    return source.buffer[token.start..token.end];
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
    try pp.comp.diag.add(.{
        .tag = tag,
        .loc = .{
            .id = raw.source,
            .byteOffset = raw.start,
            .line = raw.line,
        },
    }, &.{});
}

fn fatal(pp: *Preprocessor, raw: RawToken, comptime fmt: []const u8, args: anytype) Compilation.Error {
    const source = pp.comp.getSource(raw.source);
    const lineAndCol = source.getLineCol(.{ .id = raw.source, .line = raw.line, .byteOffset = raw.start });
    return pp.comp.diag.fatal(source.path, lineAndCol.line, raw.line, lineAndCol.col, fmt, args);
}

/// Consume next token, error if it is not an identifier.
fn expectMacroName(pp: *Preprocessor, lexer: *Lexer) Error!?[]const u8 {
    const macroName = lexer.nextNoWhiteSpace();
    if (!macroName.id.isMacroIdentifier()) {
        try pp.addError(macroName, .macro_name_missing);
        skipToNewLine(lexer);
        return null;
    }

    return pp.getTokenSlice(macroName);
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
fn expr(pp: *Preprocessor, lexer: *Lexer) MacroError!bool {
    const start = pp.tokens.len;
    defer {
        for (pp.tokens.items(.expansionLocs)[start..]) |loc|
            Token.free(loc, pp.comp.gpa);
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
                try pp.tokens.append(pp.comp.gpa, tokenFromRaw(token));
                break;
            },
            .KeywordDefined => {
                const first = lexer.nextNoWhiteSpace();
                const macroToken = if (first.id == .LParen) lexer.next() else first;

                // validate the macro name
                if (!macroToken.id.isMacroIdentifier())
                    try pp.addError(macroToken, .macro_name_missing);

                if (first.id == .LParen) {
                    const rParen = lexer.next();
                    if (rParen.id != .RParen) {
                        try pp.addError(rParen, .closing_paren);
                        try pp.addError(first, .to_match_paren);
                    }
                }
                token.id = if (pp.defines.get(pp.getTokenSlice(macroToken)) != null) .One else .Zero;
            },

            .WhiteSpace => continue,
            else => {},
        }

        try pp.expandMacro(lexer, token);
    }

    if (!pp.tokens.items(.id)[start].validPreprocessorExprStart()) {
        const token = pp.tokens.get(start);
        try pp.comp.diag.add(.{
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
                const t = pp.tokens.get(start + i);
                try pp.comp.diag.add(.{
                    .tag = .string_literal_in_pp_expr,
                    .loc = t.loc,
                }, t.expansionSlice());
                return false;
            },

            .FloatLiteral,
            .FloatLiteral_F,
            .FloatLiteral_L,
            .ImaginaryLiteral,
            .ImaginaryLiteral_F,
            .ImaginaryLiteral_L,
            => {
                const t = pp.tokens.get(start + i);
                try pp.comp.diag.add(.{
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
                const t = pp.tokens.get(start + i);
                try pp.comp.diag.add(.{
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
        .tokenIdx = @intCast(start),
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

/// Skip until #else #elif #endif, return last directive token id.
/// Also skips nested #if ... #endifs.
fn skip(
    pp: *Preprocessor,
    lexer: *Lexer,
    cont: enum { untilElse, untilEndIf, untilEndIfSeenElse },
) Error!void {
    // ifsSeen tracks the level of nested #if/#ifdef/#ifndef
    // directives. It is incremented when entering a conditional
    // block and decremented when exiting #endif.
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
    var buff = ExpandBuffer.init(pp.comp.gpa);
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
            .WhiteSpace => if (pp.comp.onlyPreprocess) buff.appendAssumeCapacity(token),
            .MacroFile => {
                const start = pp.comp.generatedBuffer.items.len;
                const source = pp.comp.getSource(pp.expansionSourceLoc.id);
                try pp.comp.generatedBuffer.writer().print("\"{s}\"\n", .{source.path});

                buff.appendAssumeCapacity(try pp.makeGeneratedToken(start, .StringLiteral, token));
            },
            .MacroLine => {
                const start = pp.comp.generatedBuffer.items.len;
                const source = pp.comp.getSource(pp.expansionSourceLoc.id);
                try pp.comp.generatedBuffer.writer().print("{d}\n", .{source.physicalLine(pp.expansionSourceLoc)});

                buff.appendAssumeCapacity(try pp.makeGeneratedToken(start, .IntegerLiteral, token));
            },
            .MacroCounter => {
                defer pp.counter += 1;
                const start = pp.comp.generatedBuffer.items.len;
                try pp.comp.generatedBuffer.writer().print("{d}\n", .{pp.counter});

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
        if (tok.id == .MacroWS) continue;
        if (tok.id != .StringLiteral) return error.ExpectedStringLiteral;
        const str = pp.expandedSlice(tok);
        try pp.charBuffer.appendSlice(str[1 .. str.len - 1]);
    }
    return pp.charBuffer.items[charBufferTop..];
}

/// Handle the _Pragma operator (implemented as a builtin macro)
fn pragmaOperator(pp: *Preprocessor, argToken: Token, operatorLoc: Source.Location) !void {
    const argSlice = pp.expandedSlice(argToken);
    const content = argSlice[1 .. argSlice.len - 1];
    const directive = "#pragma ";

    pp.charBuffer.clearRetainingCapacity();
    const totalLen = directive.len + content.len + 1; // destringify can never grow the string, + 1 for newline
    try pp.charBuffer.ensureUnusedCapacity(totalLen);
    pp.charBuffer.appendSliceAssumeCapacity(directive);
    pp.destringify(content);
    pp.charBuffer.appendAssumeCapacity('\n');

    const start = pp.comp.generatedBuffer.items.len;
    try pp.comp.generatedBuffer.appendSlice(pp.charBuffer.items);
    var tempLexer = Lexer{
        .buffer = pp.comp.generatedBuffer.items,
        .comp = pp.comp,
        .index = @intCast(start),
        .source = .generated,
        .line = pp.generatedLine,
    };
    pp.generatedLine += 1;

    const hashToken = tempLexer.next();
    std.debug.assert(hashToken.id == .Hash);

    const pragmaToken = tempLexer.next();
    std.debug.assert(pragmaToken.id == .KeywordPragma);

    try pp.pragma(&tempLexer, pragmaToken, operatorLoc, argToken.expansionSlice());
}

/// Inverts the output of the preprocessor stringify (#) operation
/// (except all whitespace is condensed to a single space)
/// writes output to pp.char_buf; assumes capacity is sufficient
/// backslash backslash -> backslash
/// backslash doublequote -> doublequote
/// All other characters remain the same
fn destringify(pp: *Preprocessor, str: []const u8) void {
    var state: enum { start, backslachSeen } = .start;
    for (str) |c| {
        switch (c) {
            '\\' => {
                if (state == .backslachSeen) pp.charBuffer.appendAssumeCapacity(c);
                state = if (state == .start) .backslachSeen else .start;
            },
            else => {
                if (state == .backslachSeen and c != '"') pp.charBuffer.appendAssumeCapacity('\\');
                pp.charBuffer.appendAssumeCapacity(c);
                state = .start;
            },
        }
    }
}

/// Stringify `tokens` into pp.charBuffer.
/// See https://gcc.gnu.org/onlinedocs/gcc-11.2.0/cpp/Stringizing.html#Stringizing
fn stringify(pp: *Preprocessor, tokens: []const Token) !void {
    try pp.charBuffer.append('"');

    var wsState: enum { start, need, notNeeded } = .start;
    for (tokens) |tok| {
        if (tok.id == .MacroWS) {
            if (wsState == .start) continue;
            wsState = .need;
            continue;
        }
        if (wsState == .need) try pp.charBuffer.append(' ');
        wsState = .notNeeded;

        // backslashes not inside strings are not escaped
        const isString = switch (tok.id) {
            .StringLiteral,
            .StringLiteralUTF_8,
            .StringLiteralUTF_16,
            .StringLiteralUTF_32,
            .StringLiteralWide,
            .CharLiteral,
            .CharLiteralUTF_16,
            .CharLiteralUTF_32,
            .CharLiteralWide,
            => true,
            else => false,
        };

        for (pp.expandedSlice(tok)) |c| {
            if (c == '"')
                try pp.charBuffer.appendSlice("\\\"")
            else if (c == '\\' and isString)
                try pp.charBuffer.appendSlice("\\\\")
            else
                try pp.charBuffer.append(c);
        }
    }
    try pp.charBuffer.appendSlice("\"\n");
}

fn handleBuiltinMacro(
    pp: *Preprocessor,
    builtin: TokenType,
    paramTokens: []const Token,
    srcLoc: Source.Location,
) Error!bool {
    switch (builtin) {
        .MacroParamHasAttribute,
        .MacroParamHasFeature,
        .MacroParamHasExtension,
        .MacroParamHasBuiltin,
        => {
            var invalid: ?Token = null;
            var identifier: ?Token = null;
            for (paramTokens) |tok| switch (tok.id) {
                .Identifier,
                .ExtendedIdentifier,
                .BuiltinChooseExpr,
                .BuiltinVaArg,
                => {
                    if (identifier) |_| invalid = tok else identifier = tok;
                },
                .MacroWS => continue,
                else => {
                    invalid = tok;
                    break;
                },
            };
            if (identifier == null and invalid == null) invalid = .{ .id = .Eof, .loc = srcLoc };
            if (invalid) |some| {
                try pp.comp.diag.add(
                    .{ .tag = .feature_check_requires_identifier, .loc = some.loc },
                    some.expansionSlice(),
                );
                return false;
            }

            const identifierStr = pp.expandedSlice(identifier.?);

            return switch (builtin) {
                .MacroParamHasAttribute => Attribute.fromString(.gnu, null, identifierStr) != null,
                .MacroParamHasFeature => Features.hasFeature(pp.comp, identifierStr),
                .MacroParamHasExtension => Features.hasExtension(pp.comp, identifierStr),
                .MacroParamHasBuiltin => pp.comp.builtins.hasBuiltin(identifierStr),
                else => unreachable,
            };
        },

        .MacroParamHasWarning => {
            const actualParam = pp.pasteStringsUnsafe(paramTokens) catch |er| switch (er) {
                error.ExpectedStringLiteral => {
                    try pp.comp.diag.add(.{
                        .tag = .expected_str_literal_in,
                        .loc = paramTokens[0].loc,
                        .extra = .{ .str = "__has_warning" },
                    }, paramTokens[0].expansionSlice());
                    return false;
                },
                else => |e| return e,
            };

            if (!std.mem.startsWith(u8, actualParam, "-W")) {
                try pp.comp.diag.add(.{
                    .tag = .malformed_warning_check,
                    .loc = paramTokens[0].loc,
                    .extra = .{ .str = "__has_warning" },
                }, paramTokens[0].expansionSlice());
                return false;
            }

            const warningName = actualParam[2..];
            return Diagnostics.warningExists(warningName);
        },

        .MacroParamIsIdentifier => {
            var invalid: ?Token = null;
            var identifier: ?Token = null;
            for (paramTokens) |tok| switch (tok.id) {
                .MacroWS => continue,
                else => {
                    if (identifier) |_| invalid = tok else identifier = tok;
                },
            };
            if (identifier == null and invalid == null) invalid = .{ .id = .Eof, .loc = srcLoc };
            if (invalid) |some| {
                try pp.comp.diag.add(.{
                    .tag = .missing_tok_builtin,
                    .loc = some.loc,
                    .extra = .{ .expectedTokenId = .RParen },
                }, some.expansionSlice());
                return false;
            }

            const id = identifier.?.id;
            return id == .Identifier or id == .ExtendedIdentifier;
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
) MacroError!ExpandBuffer {
    var buf = ExpandBuffer.init(pp.comp.gpa);
    try buf.ensureTotalCapacity(funcMacro.tokens.len);
    errdefer buf.deinit();

    var expandedVarArguments = ExpandBuffer.init(pp.comp.gpa);
    var varArguments = ExpandBuffer.init(pp.comp.gpa);

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
                        .MacroWS => continue,
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
                try pp.stringify(arg);

                const start = pp.comp.generatedBuffer.items.len;
                try pp.comp.generatedBuffer.appendSlice(pp.charBuffer.items);

                try buf.append(try pp.makeGeneratedToken(start, .StringLiteral, tokenFromRaw(raw)));
            },

            .MacroParamHasAttribute,
            .MacroParamHasWarning,
            .MacroParamHasFeature,
            .MacroParamHasExtension,
            .MacroParamIsIdentifier,
            .MacroParamHasBuiltin,
            => {
                const arg = expandedArgs.items[0];
                const result = if (arg.len == 0) blk: {
                    const extra = Diagnostics.Message.Extra{ .arguments = .{ .expected = 1, .actual = 0 } };
                    try pp.comp.diag.add(.{ .tag = .expected_arguments, .loc = loc, .extra = extra }, &.{});
                    try buf.append(.{ .id = .Zero, .loc = loc });
                    break :blk false;
                } else try pp.handleBuiltinMacro(raw.id, arg, loc);

                const start = pp.comp.generatedBuffer.items.len;
                try pp.comp.generatedBuffer.writer().print("{}\n", .{@intFromBool(result)});
                try buf.append(try pp.makeGeneratedToken(start, .IntegerLiteral, tokenFromRaw(raw)));
            },

            .MacroParamPragmaOperator => {
                const paramTokens = expandedArgs.items[0];
                // Clang and GCC require exactly one token (so, no parentheses or string pasting)
                // even though their error messages indicate otherwise. Ours is slightly more
                // descriptive.
                var invalid: ?Token = null;
                var string: ?Token = null;
                for (paramTokens) |tok| switch (tok.id) {
                    .StringLiteral => {
                        if (string) |_| invalid = tok else string = tok;
                    },
                    .MacroWS => continue,
                    else => {
                        invalid = tok;
                        break;
                    },
                };
                if (string == null and invalid == null) invalid = .{ .loc = loc, .id = .Eof };
                if (invalid) |some| try pp.comp.diag.add(
                    .{ .tag = .pragma_operator_string_literal, .loc = some.loc },
                    some.expansionSlice(),
                ) else try pp.pragmaOperator(string.?, loc);
            },

            else => try buf.append(tokenFromRaw(raw)),
        }
    }

    return buf;
}

fn shouldExpand(tok: Token, macro: *Macro) bool {
    // macro.loc.line contains the macros end index
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
            if (rawToken.id.isMacroIdentifier() and pp.poisonedIdentifiers.get(pp.getTokenSlice(rawToken)) != null)
                try pp.addError(rawToken, .poisoned_identifier);

            if (rawToken.id == .NewLine)
                pp.addExpansionNL += 1;

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
            .NewLine, .WhiteSpace, .MacroWS => {},
            .LParen => break,
            else => {
                if (isBuiltin) {
                    try pp.comp.diag.add(.{
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
    var args = MacroArguments.init(pp.comp.gpa);
    errdefer deinitMacroArguments(pp.comp.gpa, &args);

    var curArgument = std.ArrayList(Token).init(pp.comp.gpa);
    defer curArgument.deinit();

    while (true) {
        const tok = try nextBufToken(pp, lexer, buf, startIdx, endIdx, extendBuffer);
        switch (tok.id) {
            .Comma => {
                if (parens == 0)
                    try args.append(try curArgument.toOwnedSlice())
                else
                    try curArgument.append(try tok.dupe(pp.comp.gpa));
            },

            .LParen => {
                try curArgument.append(try tok.dupe(pp.comp.gpa));
                parens += 1;
            },

            .RParen => {
                if (parens == 0) {
                    try args.append(try curArgument.toOwnedSlice());
                    break;
                } else {
                    try curArgument.append(try tok.dupe(pp.comp.gpa));
                    parens -= 1;
                }
            },

            .Eof => {
                deinitMacroArguments(pp.comp.gpa, &args);
                lexer.* = savedLexer;
                endIdx.* = oldEnd;
                try pp.comp.diag.add(
                    .{ .tag = .unterminated_macro_arg_list, .loc = nameToken.loc },
                    nameToken.expansionSlice(),
                );
                return null;
            },

            .NewLine, .WhiteSpace => {
                try curArgument.append(.{ .id = .MacroWS, .loc = .{ .id = .generated } });
            },

            else => {
                try curArgument.append(try tok.dupe(pp.comp.gpa));
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
) MacroError!void {
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
                            pp.comp.gpa.free(item);
                        }
                        args.deinit();
                    }

                    var argsCount = @as(u32, @intCast(args.items.len));
                    // if the macro has zero arguments g() args_count is still 1
                    if (argsCount == 1 and macro.params.len == 0)
                        argsCount = 0;

                    // Validate argument count.
                    const extra = Diagnostics.Message.Extra{ .arguments = .{
                        .expected = @intCast(macro.params.len),
                        .actual = argsCount,
                    } };
                    if (macro.varArgs and argsCount < macro.params.len) {
                        try pp.comp.diag.add(
                            .{ .tag = .expected_at_least_arguments, .loc = buf.items[idx].loc, .extra = extra },
                            buf.items[idx].expansionSlice(),
                        );
                        idx += 1;
                        continue;
                    }

                    if (!macro.varArgs and argsCount != macro.params.len) {
                        try pp.comp.diag.add(
                            .{ .tag = .expected_arguments, .loc = buf.items[idx].loc, .extra = extra },
                            buf.items[idx].expansionSlice(),
                        );
                        idx += 1;
                        continue;
                    }

                    //std.debug.print("Expanding func: {s}\n", .{pp.expandedSlice(buf.items[idx])});
                    var expandedArgs = MacroArguments.init(pp.comp.gpa);
                    defer deinitMacroArguments(pp.comp.gpa, &expandedArgs);
                    try expandedArgs.ensureTotalCapacity(args.items.len);
                    for (args.items) |arg| {
                        var expandBuffer = ExpandBuffer.init(pp.comp.gpa);
                        try expandBuffer.appendSlice(arg);

                        try pp.expandMacroExhaustive(lexer, &expandBuffer, 0, expandBuffer.items.len, false);
                        expandedArgs.appendAssumeCapacity(try expandBuffer.toOwnedSlice());
                    }

                    var res = try pp.expandFuncMacro(macroToken.loc, macro, &args, &expandedArgs);
                    defer res.deinit();

                    const macroExpansionLocs = macroToken.expansionSlice();
                    for (res.items) |*tok| {
                        try tok.addExpansionLocation(pp.comp.gpa, &.{macroToken.loc});
                        try tok.addExpansionLocation(pp.comp.gpa, macroExpansionLocs);
                    }

                    const count = macroScanIdx - idx + 1;
                    for (buf.items[idx .. idx + count]) |tok|
                        Token.free(tok.expansionLocs, pp.comp.gpa);
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
                        try tok.addExpansionLocation(pp.comp.gpa, &.{macroToken.loc});
                        try tok.addExpansionLocation(pp.comp.gpa, macroExpansionLocs);
                    }

                    Token.free(buf.items[idx].expansionLocs, pp.comp.gpa);

                    try buf.replaceRange(idx, 1, res.items);
                    idx += res.items.len;
                    movingEndIdx = movingEndIdx + res.items.len - 1;
                    doRescan = true;
                }
            }
            if (idx - startIdx == advanceIdx + 1 and !doRescan) {
                advanceIdx += 1;
            }
        } // end of replacement phase
    }
    // end of scanning phase

    // trim excess buffer
    for (buf.items[movingEndIdx..]) |item| {
        Token.free(item.expansionLocs, pp.comp.gpa);
    }
    buf.items.len = movingEndIdx;
}

/// Try to expand a macro after a possible candidate has been read from the `tokenizer`
/// into the `raw` token passed as argument
fn expandMacro(pp: *Preprocessor, lexer: *Lexer, raw: RawToken) MacroError!void {
    var sourceToken = tokenFromRaw(raw);
    if (!raw.id.isMacroIdentifier()) {
        sourceToken.id.simplifyMacroKeyword();
        return pp.tokens.append(pp.comp.gpa, sourceToken);
    }

    pp.topExpansionBuffer.items.len = 0;
    try pp.topExpansionBuffer.append(sourceToken);
    pp.expansionSourceLoc = sourceToken.loc;

    try pp.expandMacroExhaustive(lexer, &pp.topExpansionBuffer, 0, 1, true);
    try pp.tokens.ensureUnusedCapacity(pp.comp.gpa, pp.topExpansionBuffer.items.len);
    for (pp.topExpansionBuffer.items) |*tok| {
        if (tok.id == .MacroWS and !pp.comp.onlyPreprocess) {
            Token.free(tok.expansionLocs, pp.comp.gpa);
            continue;
        }
        tok.id.simplifyMacroKeyword();
        pp.tokens.appendAssumeCapacity(tok.*);
    }

    if (pp.comp.onlyPreprocess) {
        try pp.tokens.ensureUnusedCapacity(pp.comp.gpa, pp.addExpansionNL);
        while (pp.addExpansionNL > 0) : (pp.addExpansionNL -= 1) {
            pp.tokens.appendAssumeCapacity(.{ .id = .NewLine, .loc = .{ .id = .generated } });
        }
    }
}

/// Get expanded token source string.
pub fn expandedSlice(pp: *Preprocessor, token: Token) []const u8 {
    if (token.id.getTokenText()) |some|
        return some;

    var lexer = Lexer{
        .buffer = pp.comp.getSource(token.loc.id).buffer,
        .comp = pp.comp,
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
        if (lhs.id == .MacroWS)
            Token.free(lhs.expansionLocs, pp.comp.gpa)
        else
            break lhs;
    } else {
        return bufCopyTokens(lhsTokens, rhsTokens, &.{});
    };

    var rhsRest: u32 = 1;
    const rhs = for (rhsTokens) |rhs| {
        if (rhs.id != .MacroWS) break rhs;
        rhsRest += 1;
    } else {
        return lhsTokens.appendAssumeCapacity(lhs);
    };
    defer Token.free(lhs.expansionLocs, pp.comp.gpa);

    const start = pp.comp.generatedBuffer.items.len;
    const end = start + pp.expandedSlice(lhs).len + pp.expandedSlice(rhs).len;
    try pp.comp.generatedBuffer.ensureTotalCapacity(end + 1); // +1 for a newline

    // We cannot use the same slices here since they might be invalidated by `ensureCapacity`
    pp.comp.generatedBuffer.appendSliceAssumeCapacity(pp.expandedSlice(lhs));
    pp.comp.generatedBuffer.appendSliceAssumeCapacity(pp.expandedSlice(rhs));
    pp.comp.generatedBuffer.appendAssumeCapacity('\n');

    // Try to tokenize the result.
    var lexer = Lexer{
        .buffer = pp.comp.generatedBuffer.items,
        .comp = pp.comp,
        .index = @intCast(start),
        .source = .generated,
    };

    const pastedToken = lexer.nextNoWhiteSpace();
    const next = lexer.nextNoWhiteSpace().id;

    if (next != .NewLine and next != .Eof) {
        try pp.comp.diag.add(.{
            .tag = .pasting_formed_invalid,
            .loc = lhs.loc,
            .extra = .{ .str = try pp.comp.diag.arena.allocator().dupe(u8, pp.comp.generatedBuffer.items[start..end]) },
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
            .byteOffset = @intCast(start),
            .line = pp.generatedLine,
        },
    };
    pp.generatedLine += 1;
    try pastedToken.addExpansionLocation(pp.comp.gpa, &.{source.loc});
    try pastedToken.addExpansionLocation(pp.comp.gpa, source.expansionSlice());
    return pastedToken;
}

/// Defines a new macro and warns  if it  is a duplicate
fn defineMacro(pp: *Preprocessor, nameToken: RawToken, macro: Macro) Error!void {
    const name = pp.getTokenSlice(nameToken);
    const gop = try pp.defines.getOrPut(name);
    if (gop.found_existing and !gop.value_ptr.eql(macro, pp)) {
        try pp.comp.diag.add(.{
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

    // Clear the token buffer
    // Safe to use since we can only be in one directive at a time.
    pp.tokenBuffer.items.len = 0;

    var needWS = false;
    // Collect the token body and validate any ## found.
    var token = first;
    const endIdx = while (true) {
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
            .NewLine, .Eof => break token.start,
            .WhiteSpace => needWS = true,
            else => {
                if (token.id != .WhiteSpace and needWS) {
                    needWS = false;
                    try pp.tokenBuffer.append(.{ .id = .MacroWS, .source = .generated });
                }
                try pp.tokenBuffer.append(token);
            },
        }
        token = lexer.next();
    } else unreachable;

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
    var params = std.ArrayList([]const u8).init(pp.comp.gpa);
    defer params.deinit();

    // parse the parameter list
    var gnuVarArgs: []const u8 = ""; // gnu-named varargs
    var varArgs = false;
    const startIdx = while (true) {
        var token = lexer.nextNoWhiteSpace();
        if (token.id == .RParen) break token.end;

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

            break rParen.end;
        }

        if (!token.id.isMacroIdentifier()) {
            try pp.addError(token, .invalid_token_param_list);
            return skipToNewLine(lexer);
        }

        try params.append(pp.getTokenSlice(token));

        token = lexer.nextNoWhiteSpace();
        if (token.id == .Ellipsis) {
            try pp.addError(token, .gnu_va_macro);
            gnuVarArgs = params.pop();
            const rParen = lexer.nextNoWhiteSpace();
            if (rParen.id != .RParen) {
                try pp.addError(lParen, .missing_paren_param_list);
                try pp.addError(lParen, .to_match_paren);
                return skipToNewLine(lexer);
            }
            break rParen.end;
        } else if (token.id == .RParen) {
            break token.end;
        } else if (token.id != .Comma) {
            try pp.addError(token, .expected_comma_param_list);
            return skipToNewLine(lexer);
        }
    } else unreachable;

    var needWS = false;
    // Collect the body tokens and validate # and ##'s found.
    // Clear the token buffer
    // Safe to use since we can only be in one directive at a time.
    pp.tokenBuffer.items.len = 0;
    const endIdx = tokenLoop: while (true) {
        var token = lexer.next();
        switch (token.id) {
            .NewLine, .Eof => break token.start,
            .WhiteSpace => needWS = pp.tokenBuffer.items.len != 0,
            .Hash => {
                if (token.id != .WhiteSpace and needWS) {
                    needWS = false;
                    try pp.tokenBuffer.append(.{ .id = .MacroWS, .source = .generated });
                }
                const param = lexer.nextNoWhiteSpace();
                blk: {
                    if (varArgs and param.id == .KeywordVarArgs) {
                        token.id = .StringifyVarArgs;
                        try pp.tokenBuffer.append(token);
                        continue :tokenLoop;
                    }

                    if (!param.id.isMacroIdentifier())
                        break :blk;

                    const s = pp.getTokenSlice(param);
                    if (std.mem.eql(u8, s, gnuVarArgs)) {
                        token.id = .StringifyVarArgs;
                        try pp.tokenBuffer.append(token);
                        continue :tokenLoop;
                    }
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
                needWS = false;
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
                if (token.id != .WhiteSpace and needWS) {
                    needWS = false;
                    try pp.tokenBuffer.append(.{ .id = .MacroWS, .source = .generated });
                }
                if (varArgs and token.id == .KeywordVarArgs) {
                    // do nothing
                } else if (token.id.isMacroIdentifier()) {
                    token.id.simplifyMacroKeyword();
                    const s = pp.getTokenSlice(token);
                    if (std.mem.eql(u8, gnuVarArgs, s)) {
                        token.id = .KeywordVarArgs;
                    } else {
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
                }

                try pp.tokenBuffer.append(token);
            },
        }
    } else unreachable;

    const paramList = try pp.arena.allocator().dupe([]const u8, params.items);
    const tokenList = try pp.arena.allocator().dupe(RawToken, pp.tokenBuffer.items);
    try pp.defineMacro(macroName, .{
        .isFunc = true,
        .params = paramList,
        .varArgs = varArgs or gnuVarArgs.len != 0,
        .tokens = tokenList,
        .loc = .{ .id = macroName.source, .byteOffset = startIdx, .line = endIdx },
    });
}

fn include(pp: *Preprocessor, lexer: *Lexer) MacroError!void {
    const newSource = pp.findIncludeSource(lexer) catch |er| switch (er) {
        error.InvalidInclude => return,
        else => |e| return e,
    };

    pp.includeDepth += 1;
    defer pp.includeDepth -= 1;
    if (pp.includeDepth > MaxIncludeDepth)
        return;

    _ = pp.preprocessExtra(newSource) catch |err| switch (err) {
        error.StopPreprocessing => {},
        else => |e| return e,
    };
}

/// tokens that are part of a pragma directive can happen in 3 ways:
///     1. directly in the text via `#pragma ...`
///     2. Via a string literal argument to `_Pragma`
///     3. Via a stringified macro argument which is used as an argument to `_Pragma`
/// operator_loc: Location of `_Pragma`; null if this is from #pragma
/// arg_locs: expansion locations of the argument to _Pragma. empty if #pragma or a raw string literal was used
fn makePragmaToken(pp: *Preprocessor, raw: RawToken, operatorLoc: ?Source.Location, argLocs: []const Source.Location) !Token {
    var tok = tokenFromRaw(raw);
    if (operatorLoc) |loc|
        try tok.addExpansionLocation(pp.comp.gpa, &.{loc});

    try tok.addExpansionLocation(pp.comp.gpa, argLocs);
    return tok;
}

/// Handle a pragma directive
fn pragma(
    pp: *Preprocessor,
    lexer: *Lexer,
    pragmaToken: RawToken,
    operatorLoc: ?Source.Location,
    argLocs: []const Source.Location,
) !void {
    const nameToken = lexer.nextNoWhiteSpace();
    if (nameToken.id == .NewLine or nameToken.id == .Eof)
        return;

    const name = pp.getTokenSlice(nameToken);

    try pp.tokens.append(pp.comp.gpa, try pp.makePragmaToken(pragmaToken, operatorLoc, argLocs));
    const pragmaStart = @as(u32, @intCast(pp.tokens.len));

    const pragmaNameToken = try pp.makePragmaToken(nameToken, operatorLoc, argLocs);
    try pp.tokens.append(pp.comp.gpa, pragmaNameToken);

    while (true) {
        const nextToken = lexer.next();
        if (nextToken.id == .WhiteSpace) continue;
        if (nextToken.id == .Eof) {
            try pp.tokens.append(pp.comp.gpa, .{
                .id = .NewLine,
                .loc = .{ .id = .generated },
            });
            break;
        }
        try pp.tokens.append(pp.comp.gpa, try pp.makePragmaToken(nextToken, operatorLoc, argLocs));
        if (nextToken.id == .NewLine)
            break;
    }

    if (pp.comp.getPragma(name)) |prag| unknown: {
        return prag.preprocessorCB(pp, pragmaStart) catch |err| switch (err) {
            error.UnknownPragma => break :unknown,
            else => |e| return e,
        };
    }
    return pp.comp.diag.add(.{
        .tag = .unknown_pragma,
        .loc = pragmaNameToken.loc,
    }, pragmaNameToken.expansionSlice());
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

        try pp.comp.diag.add(.{ .tag = .header_str_closing, .loc = .{ .id = first.source, .byteOffset = first.start } }, &.{});
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
    return (try pp.comp.findInclude(first, filename, fileNameTK.id == .StringLiteral)) orelse
        pp.fatal(first, "'{s}' not found", .{filename});
}

/// pretty print tokens and try to preserve whitespace
pub fn prettyPrintTokens(pp: *Preprocessor, w: anytype) !void {
    var i: u32 = 0;
    while (true) : (i += 1) {
        var cur: Token = pp.tokens.get(i);
        switch (cur.id) {
            .Eof => {
                if (pp.tokens.len > 1 and pp.tokens.items(.id)[i - 1] != .NewLine)
                    try w.writeByte('\n');
                break;
            },
            .NewLine => try w.writeAll("\n"),
            .KeywordPragma => {
                const pragmaName = pp.expandedSlice(pp.tokens.get(i + 1));
                const endIdx = std.mem.indexOfScalarPos(TokenType, pp.tokens.items(.id), i, .NewLine) orelse i + 1;
                const pragmaLen = @as(u32, @intCast(endIdx)) - i;

                if (pp.comp.getPragma(pragmaName)) |prag| {
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

            .WhiteSpace => {
                var slice = pp.expandedSlice(cur);
                while (std.mem.indexOfScalar(u8, slice, '\n')) |some| {
                    try w.writeByte('\n');
                    slice = slice[some + 1 ..];
                }
                for (slice) |_|
                    try w.writeByte(' ');
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

            const test_runner_macros = try comp.addSourceFromBuffer("<test_runner>", source_text);
            const eof = try pp.preprocess(test_runner_macros);
            try pp.tokens.append(pp.comp.gpa, eof);

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

test "destringify" {
    const allocator = std.testing.allocator;
    const Test = struct {
        fn testDestringify(pp: *Preprocessor, stringified: []const u8, destringified: []const u8) !void {
            pp.charBuffer.clearRetainingCapacity();
            try pp.charBuffer.ensureUnusedCapacity(stringified.len);
            pp.destringify(stringified);
            try std.testing.expectEqualStrings(destringified, pp.charBuffer.items);
        }
    };
    var comp = Compilation.init(allocator);
    defer comp.deinit();
    var pp = Preprocessor.init(&comp);
    defer pp.deinit();

    try Test.testDestringify(&pp, "hello\tworld\n", "hello\tworld\n");
    try Test.testDestringify(&pp,
        \\ \"FOO BAR BAZ\"
    ,
        \\ "FOO BAR BAZ"
    );
    try Test.testDestringify(&pp,
        \\ \\t\\n
        \\
    ,
        \\ \t\n
        \\
    );
}
