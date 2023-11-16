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
const assert = std.debug.assert;

const Preprocessor = @This();
const DefineMap = std.StringHashMap(Macro);
const RawTokenList = std.ArrayList(RawToken);
const MaxIncludeDepth = 200;

const Error = Compilation.Error;

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
pragmaOnce: std.AutoHashMap(Source.ID, void),
tokenBuffer: RawTokenList,
charBuffer: std.ArrayList(u8),
includeDepth: u8 = 0,

const FeatureCheckMacros = struct {
    const args = [1][]const u8{"X"};
    const hasAttribute = [1]RawToken{makeFeatCheckMacro(.MacroParamHasAttribute)};
    const hasWarning = [1]RawToken{makeFeatCheckMacro(.MacroParamHasWarning)};
    const isIdentifier = [1]RawToken{makeFeatCheckMacro(.MacroParamIsIdentifier)};

    fn makeFeatCheckMacro(id: TokenType) RawToken {
        return .{
            .id = id,
            .source = .generated,
            .start = 0,
            .end = 0,
        };
    }
};

pub fn addBuiltinMacro(pp: *Preprocessor, name: []const u8, tokens: []const RawToken) !void {
    try pp.defines.put(name, .{
        .params = &FeatureCheckMacros.args,
        .tokens = tokens,
        .varArgs = false,
        .isFunc = true,
        .loc = .{ .id = .generated },
        .isBuiltin = true,
    });
}

pub fn addBuiltinMacros(pp: *Preprocessor) !void {
    try pp.addBuiltinMacro("__has_attribute", &FeatureCheckMacros.hasAttribute);
    try pp.addBuiltinMacro("__has_warning", &FeatureCheckMacros.hasWarning);
    try pp.addBuiltinMacro("__is_identifier", &FeatureCheckMacros.isIdentifier);
}

pub fn init(comp: *Compilation) Preprocessor {
    return .{
        .compilation = comp,
        .arena = std.heap.ArenaAllocator.init(comp.gpa),
        .defines = DefineMap.init(comp.gpa),
        .generated = std.ArrayList(u8).init(comp.gpa),
        .pragmaOnce = std.AutoHashMap(Source.ID, void).init(comp.gpa),
        .tokenBuffer = RawTokenList.init(comp.gpa),
        .charBuffer = std.ArrayList(u8).init(comp.gpa),
    };
}

pub fn deinit(pp: *Preprocessor) void {
    pp.defines.deinit();
    pp.tokens.deinit(pp.compilation.gpa);
    pp.arena.deinit();
    pp.generated.deinit();
    pp.pragmaOnce.deinit();
    pp.tokenBuffer.deinit();
    pp.charBuffer.deinit();
}

pub fn preprocess(pp: *Preprocessor, source: Source) Error!void {
    const initialOptions = pp.compilation.diag.options;
    defer pp.compilation.diag.options = initialOptions;

    var lexer = Lexer{
        .buffer = source.buffer,
        .source = source.id,
        .comp = pp.compilation,
    };

    const TrailingWhiteSpace = " \r\t\x0B\x0C";

    // Estimate how many new tokens this source will contain.
    const estimatedTokenCount = source.buffer.len / 8;
    try pp.tokens.ensureUnusedCapacity(pp.compilation.gpa, pp.tokens.len + estimatedTokenCount);

    var ifLevel: u8 = 0;
    var ifKind = std.PackedIntArray(u2, 256).init([1]u2{0} ** 256);
    var pragmaState: PragmaState = .{};
    const untilElse = 0;
    const untilEndIf = 1;
    const untilEndIfSeenElse = 2;

    var startOfLine = true;
    while (true) {
        var token = lexer.next();
        switch (token.id) {
            .Hash => if (startOfLine) {
                const directive = lexer.next();
                switch (directive.id) {
                    .KeywordError => {
                        const start = lexer.index;
                        while (lexer.index < lexer.buffer.len) : (lexer.index += 1) {
                            if (lexer.buffer[lexer.index] == '\n') break;
                        }

                        var slice = lexer.buffer[start..lexer.index];
                        slice = std.mem.trim(u8, slice, TrailingWhiteSpace);

                        try pp.compilation.addDiagnostic(.{
                            .tag = .error_directive,
                            .loc = .{ .id = token.source, .byteOffset = token.start },
                            .extra = .{ .str = slice },
                        });
                    },

                    .KeywordIf => {
                        const ov = @addWithOverflow(ifLevel, 1);
                        if (ov[1] != 0)
                            return pp.compilation.fatal(directive, "too many #if nestings", .{});

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
                            return pp.compilation.fatal(directive, "too many #if nestings", .{});

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
                            return pp.compilation.fatal(directive, "too many #if nestings", .{});

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

                    .KeywordDefine => try pp.define(&lexer),
                    .KeywordInclude => try pp.include(&lexer),
                    .KeywordPragma => pp.pragma(&lexer, token, &pragmaState) catch |er| switch (er) {
                        error.PragmaOnce => return,
                        else => |e| return e,
                    },

                    .KeywordUndef => {
                        const macro_name = (try pp.expectMacroName(&lexer)) orelse continue;
                        _ = pp.defines.remove(macro_name);
                        try pp.expectNewLine(&lexer);
                    },

                    .KeywordElIf => {
                        if (ifLevel == 0) {
                            try pp.addError(directive, .else_without_if);
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

                    .KeywordLine => {
                        const digits = lexer.next();
                        if (digits.id != .IntegerLiteral)
                            try pp.addError(digits, .line_simple_digit);

                        if (digits.id == .Eof or digits.id == .NewLine)
                            continue;

                        const name = lexer.next();
                        if (name.id == .Eof or name.id == .NewLine)
                            continue;

                        if (name.id != .StringLiteral)
                            try pp.addError(name, .line_invalid_filename);

                        try pp.expectNewLine(&lexer);
                    },

                    .NewLine => {},
                    .Eof => {
                        if (ifLevel != 0)
                            try pp.addError(directive, .unterminated_conditional_directive);
                        return;
                    },
                    else => {
                        try pp.addError(token, .invalid_preprocessing_directive);
                        try pp.expectNewLine(&lexer);
                    },
                }
            },

            .NewLine => startOfLine = true,
            .Eof => {
                if (ifLevel != 0)
                    try pp.addError(token, .unterminated_conditional_directive);
                return;
            },

            else => {
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
        },
    };
}

fn addError(pp: *Preprocessor, raw: RawToken, tag: Diagnostics.Tag) !void {
    try pp.compilation.addDiagnostic(.{
        .tag = tag,
        .loc = .{
            .id = raw.source,
            .byteOffset = raw.start,
        },
    });
}

/// Consume next token, error if it is not an identifier.
fn expectMacroName(pp: *Preprocessor, lexer: *Lexer) Error!?[]const u8 {
    const macroName = lexer.next();
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

        if (!sentErr) {
            sentErr = true;
            try pp.addError(token, .extra_tokens_directive_end);
        }
    }
}

/// Consume all tokens until a newline and parse the result into a boolean.
fn expr(pp: *Preprocessor, lexer: *Lexer) Error!bool {
    const start = pp.tokens.len;
    defer pp.tokens.len = start;

    while (true) {
        var token = lexer.next();
        if (token.id == .NewLine or token.id == .Eof) {
            if (pp.tokens.len == start) {
                try pp.addError(token, .expected_value_in_expr);
                try pp.expectNewLine(lexer);
                return false;
            }

            token.id = .Eof;
            try pp.tokens.append(pp.compilation.gpa, tokenFromRaw(token));
            break;
        } else if (token.id == .KeywordDefined) {
            const first = lexer.next();
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

            if (pp.defines.get(pp.tokSliceSafe(macroToken)) != null) {
                token.id = .One;
            } else {
                token.id = .Zero;
            }
        }

        try pp.expandMacro(lexer, token);
    }

    for (pp.tokens.items(.id)[start..], 0..) |*tok, i| {
        switch (tok.*) {
            .StringLiteral,
            .StringLiteralUTF_8,
            .StringLiteralUTF_16,
            .StringLiteralUTF_32,
            .StringLiteralWide,
            => {
                try pp.compilation.addDiagnostic(.{
                    .tag = .string_literal_in_pp_expr,
                    .loc = pp.tokens.items(.loc)[i],
                });
                return false;
            },

            .FloatLiteral,
            .FloatLiteral_F,
            .FloatLiteral_L,
            => {
                try pp.compilation.addDiagnostic(.{
                    .tag = .float_literal_in_pp_expr,
                    .loc = pp.tokens.items(.loc)[i],
                });
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

fn skip(pp: *Preprocessor, lexer: *Lexer, cont: enum { untilElse, untilEndIf, untilEndIfSeenElse }) Error!void {
    var ifsSeen: u32 = 0;
    var lineStart = true;

    while (lexer.index < lexer.buffer.len) {
        if (lineStart) {
            const dirStart = lexer.index;
            const hash = lexer.next();

            if (hash.id == .NewLine) continue;
            lineStart = false;
            if (hash.id != .Hash) continue;

            const directive = lexer.next();
            switch (directive.id) {
                .KeywordElse => {
                    if (ifsSeen != 0)
                        continue;

                    if (cont == .untilEndIfSeenElse) {
                        try pp.addError(directive, .else_after_else);
                        continue;
                    }

                    lexer.index = dirStart;
                    return;
                },

                .KeywordElIf => {
                    if (ifsSeen != 0 or cont == .untilEndIf)
                        continue;

                    if (cont == .untilEndIfSeenElse) {
                        try pp.addError(directive, .elif_after_else);
                        continue;
                    }

                    lexer.index = dirStart;
                    return;
                },

                .KeywordEndIf => {
                    if (ifsSeen == 0) {
                        lexer.index = dirStart;
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

fn expandObjMacro(pp: *Preprocessor, simpleMacro: *const Macro) Error!ExpandBuffer {
    var buff = ExpandBuffer.init(pp.compilation.gpa);
    try buff.ensureTotalCapacity(simpleMacro.tokens.len);

    var i: usize = 0;
    while (i < simpleMacro.tokens.len) : (i += 1) {
        const raw = simpleMacro.tokens[i];
        if (raw.id == .HashHash) {
            const lhs = buff.pop();
            const rhs = tokenFromRaw(simpleMacro.tokens[i + 1]);
            i += 1;
            buff.appendAssumeCapacity(try pp.pasteTokens(lhs, rhs));
        } else {
            buff.appendAssumeCapacity(tokenFromRaw(raw));
        }
    }

    return buff;
}

/// Join a series of string literal tokens into a single string without
/// leading or trailing quotes.
/// The returned slice is invalidated if pp.charBuffer changes.
fn pasteStringsUnsafe(pp: *Preprocessor, comptime TokType: type, toks: []const TokType) ![]const u8 {
    if (toks.len == 0) return error.ExpectedStringLiteral;
    const charBufferTop = pp.charBuffer.items.len;
    defer pp.charBuffer.items.len = charBufferTop;

    for (toks) |tok| {
        if (tok.id != .StringLiteral) return error.ExpectedStringLiteral;
        const str = switch (TokType) {
            Token => pp.expandedSlice(tok),
            RawToken => pp.tokenSlice(tok),
            else => unreachable,
        };
        try pp.charBuffer.appendSlice(str[1 .. str.len - 1]);
    }
    return pp.charBuffer.items[charBufferTop..];
}

fn handleBuiltinMacro(pp: *Preprocessor, builtin: TokenType, paramTokens: []const Token) Error!TokenType {
    switch (builtin) {
        .MacroParamHasAttribute => {
            if (paramTokens.len != 1 or paramTokens[0].id != .Identifier) {
                try pp.compilation.addDiagnostic(.{ .tag = .feature_check_requires_identifier, .loc = paramTokens[0].loc });
                return .Zero;
            }
            const attrName = pp.expandedSlice(paramTokens[0]);
            return if (AttrTag.fromString(attrName) == null) .Zero else .One;
        },

        .MacroParamHasWarning => {
            const actualParam = pp.pasteStringsUnsafe(Token, paramTokens) catch |er| switch (er) {
                error.ExpectedStringLiteral => {
                    const extra = Diagnostics.Message.Extra{ .str = "__has_warning" };
                    try pp.compilation.addDiagnostic(.{ .tag = .expected_str_literal_in, .loc = paramTokens[0].loc, .extra = extra });
                    return .Zero;
                },
                else => |e| return e,
            };

            if (!std.mem.startsWith(u8, actualParam, "-W")) {
                try pp.compilation.addDiagnostic(.{ .tag = .malformed_warning_check, .loc = paramTokens[0].loc });
                return .Zero;
            }

            const warningName = actualParam[2..];
            return if (Diagnostics.warningExists(warningName)) .One else .Zero;
        },

        .MacroParamIsIdentifier => {
            if (paramTokens.len > 1) {
                const extra = Diagnostics.Message.Extra{ .tokenId = .{ .expected = .RParen, .actual = paramTokens[1].id } };
                try pp.compilation.addDiagnostic(.{ .tag = .missing_tok_builtin, .loc = paramTokens[1].loc, .extra = extra });
                return .Zero;
            }
            return if (paramTokens[0].id == .Identifier) .One else .Zero;
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
                    .loc = .{
                        .id = .generated,
                    },
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
                const rawNext = funcMacro.tokens[tokenIdx + 1];
                const placeHolderToken = Token{
                    .id = .EmptyArg,
                    .loc = .{
                        .id = rawNext.source,
                        .byteOffset = rawNext.start,
                    },
                };

                const prev = buf.pop();
                var next = switch (rawNext.id) {
                    .MacroParam, .MacroParamNoExpand => args.items[rawNext.end],
                    .KeywordVarArgs => varArguments.items,
                    else => &[1]Token{tokenFromRaw(rawNext)},
                };
                next = if (next.len > 0) next else &[1]Token{placeHolderToken};

                var pastedToken = try pp.pasteTokens(prev, next[0]);
                try buf.append(pastedToken);
                try buf.appendSlice(next[1..]);
                // skip next token
                tokenIdx += 1;
            },

            .MacroParamNoExpand => {
                const placeholderToken = Token{ .id = .EmptyArg, .loc = .{ .id = raw.source, .byteOffset = raw.start } };
                var slice = switch (raw.id) {
                    .MacroParamNoExpand => args.items[raw.end],
                    .KeywordVarArgs => varArguments.items,
                    else => &[1]Token{tokenFromRaw(raw)},
                };
                slice = if (slice.len > 0) slice else &[1]Token{placeholderToken};

                try buf.appendSlice(slice);
            },

            .MacroParam => {
                const arg = expandedArgs.items[raw.end];
                if (arg.len == 0) {
                    // needed for the following token pasting phase
                    try buf.append(.{ .id = .EmptyArg, .loc = .{ .id = raw.source, .byteOffset = raw.start } });
                } else {
                    for (arg) |tok| {
                        try buf.ensureTotalCapacity(buf.items.len + arg.len);
                        buf.appendAssumeCapacity(tok);
                    }
                }
            },

            .KeywordVarArgs => {
                try buf.ensureTotalCapacity(buf.items.len + expandedVarArguments.items.len);
                buf.appendSliceAssumeCapacity(expandedVarArguments.items);
            },

            .StringifyParam, .StringifyVarArgs => {
                const arg = if (raw.id == .StringifyVarArgs)
                    varArguments.items
                else
                    args.items[raw.end];

                pp.charBuffer.clearRetainingCapacity();

                // TODO pretty print these
                try pp.charBuffer.append('"');
                for (arg) |tok| {
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

                try buf.append(.{
                    .id = .StringLiteral,
                    .loc = .{ // location of token slice in the generated buffer
                        .id = .generated,
                        .byteOffset = @as(u32, @intCast(start)),
                    },
                });
            },

            .MacroParamHasAttribute,
            .MacroParamHasWarning,
            .MacroParamIsIdentifier,
            => {
                const arg = expandedArgs.items[0];
                if (arg.len == 0) {
                    const extra = Diagnostics.Message.Extra{ .arguments = .{ .expected = 1, .actual = 0 } };
                    try pp.compilation.addDiagnostic(.{ .tag = .expected_arguments, .loc = loc, .extra = extra });
                    try buf.append(.{ .id = .Zero, .loc = loc });
                    break;
                }

                const resultId = try pp.handleBuiltinMacro(raw.id, arg);
                try buf.append(.{
                    .id = resultId,
                    .loc = loc,
                });
            },

            else => {
                try buf.append(tokenFromRaw(raw));
            },
        }
    }

    return buf;
}

fn shouldExpand(tok: Token, macro: *Macro) bool {
    const macroLoc = macro.*.loc;
    var maybeLoc = tok.loc.next;
    while (maybeLoc) |loc| {
        if (loc.id == macroLoc.id and loc.byteOffset == macroLoc.byteOffset)
            return false;
        maybeLoc = loc.next;
    }

    return true;
}

fn nextBufToken(lexer: *Lexer, buf: *ExpandBuffer, startIdx: *usize, endIdx: *usize, extendbuffer: bool) Error!Token {
    startIdx.* += 1;
    if (startIdx.* == buf.items.len and startIdx.* == endIdx.*) {
        if (extendbuffer) {
            const newToken = tokenFromRaw(lexer.next());
            endIdx.* += 1;
            try buf.append(newToken);
            return newToken;
        } else {
            return Token{
                .id = .Eof,
                .loc = .{ .id = .generated },
            };
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
    const initialLexerIdx = lexer.index;
    const oldEnd = endIdx.*;

    while (true) {
        const token = try nextBufToken(lexer, buf, startIdx, endIdx, extendBuffer);

        switch (token.id) {
            .NewLine => {},
            .LParen => break,
            else => {
                if (isBuiltin) {
                    const extra = Diagnostics.Message.Extra{ .tokenId = .{ .expected = .LParen, .actual = token.id } };
                    try pp.compilation.addDiagnostic(.{ .tag = .missing_tok_builtin, .loc = token.loc, .extra = extra });
                }
                // Not a macro function call, go over normal identifier, rewind
                lexer.index = initialLexerIdx;
                endIdx.* = oldEnd;
                return null;
            },
        }
    }

    // collect the arguments.
    var parens: u32 = 0;
    var args = MacroArguments.init(pp.compilation.gpa);
    errdefer args.deinit();

    var curArgument = std.ArrayList(Token).init(pp.compilation.gpa);
    defer curArgument.deinit();

    var done = false;
    while (!done) {
        var tok = try nextBufToken(lexer, buf, startIdx, endIdx, extendBuffer);
        switch (tok.id) {
            .Comma => {
                if (parens == 0)
                    try args.append(try curArgument.toOwnedSlice())
                else
                    try curArgument.append(tok);
            },

            .LParen => {
                try curArgument.append(tok);
                parens += 1;
            },

            .RParen => {
                if (parens == 0) {
                    try args.append(try curArgument.toOwnedSlice());
                    break;
                } else {
                    try curArgument.append(tok);
                    parens -= 1;
                }
            },

            .Eof => {
                lexer.index = initialLexerIdx;
                endIdx.* = oldEnd;
                try pp.compilation.addDiagnostic(.{ .tag = .unterminated_macro_arg_list, .loc = nameToken.loc });
                return null;
            },

            else => {
                try curArgument.append(tok);
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
        // last phase before rescan: remove placeholder tokens
        // NOTE: only do this if there were expansion (i.e. do_rescan is true)
        if (doRescan) {
            var i: usize = startIdx;
            while (i < buf.items.len) {
                const tok = &buf.items[i];
                switch (tok.id) {
                    .EmptyArg => {
                        _ = buf.orderedRemove(i);
                        movingEndIdx -= 1;
                    },
                    else => i += 1,
                }
            }
        }

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
                    const args = (try pp.collectMacroFuncArguments(lexer, buf, &macroScanIdx, &movingEndIdx, extendBuffer, macro.isBuiltin)) orelse {
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
                    const extra = Diagnostics.Message.Extra{ .arguments = .{ .expected = @as(u32, @intCast(macro.params.len)), .actual = argsCount } };
                    if (macro.varArgs and argsCount < macro.params.len) {
                        try pp.compilation.addDiagnostic(.{ .tag = .expected_at_least_arguments, .loc = buf.items[idx].loc, .extra = extra });
                        idx += 1;
                        continue;
                    }

                    if (!macro.varArgs and argsCount != macro.params.len) {
                        try pp.compilation.addDiagnostic(.{ .tag = .expected_arguments, .loc = buf.items[idx].loc, .extra = extra });
                        idx += 1;
                        continue;
                    }

                    //std.debug.print("Expanding func: {s}\n", .{pp.expandedSlice(buf.items[idx])});
                    var expandedArgs = MacroArguments.init(pp.compilation.gpa);
                    defer expandedArgs.deinit();
                    try expandedArgs.ensureTotalCapacity(args.items.len);
                    for (args.items) |arg| {
                        var expandBuffer = ExpandBuffer.init(pp.compilation.gpa);
                        try expandBuffer.appendSlice(arg);

                        try pp.expandMacroExhaustive(lexer, &expandBuffer, 0, expandBuffer.items.len, false);
                        expandedArgs.appendAssumeCapacity(try expandBuffer.toOwnedSlice());
                    }

                    var res = try pp.expandFuncMacro(macroToken.loc, macro, &args, &expandedArgs);
                    defer res.deinit();
                    for (expandedArgs.items) |arg| {
                        pp.compilation.gpa.free(arg);
                    }
                    var expansionLoc = macro.loc;
                    expansionLoc.next = buf.items[idx].loc.next;
                    for (res.items) |*tok| {
                        if (buf.items[idx].loc.next) |ln| {
                            try pp.markExpandedFrom(tok, ln.*);
                        }
                        try pp.markExpandedFrom(tok, macro.loc);
                    }

                    try buf.replaceRange(idx, macroScanIdx - idx + 1, res.items);
                    // TODO: moving_end_idx += res.items.len - (macro_scan_idx-idx+1)
                    // doesn't work when the RHS is negative (unsigned!)
                    movingEndIdx = movingEndIdx + res.items.len - (macroScanIdx - idx + 1);
                    idx += res.items.len;
                    doRescan = true;
                } else {
                    const res = try pp.expandObjMacro(macro);
                    defer res.deinit();

                    var expansionLoc = macro.loc;
                    expansionLoc.next = buf.items[idx].loc.next;
                    for (res.items) |*tok| {
                        if (buf.items[idx].loc.next) |ln| {
                            try pp.markExpandedFrom(tok, ln.*);
                        }
                        try pp.markExpandedFrom(tok, macro.loc);
                    }

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
    buf.shrinkAndFree(movingEndIdx);
}

/// Try to expand a macro after a possible candidate has been read from the `tokenizer`
/// into the `raw` token passed as argument
fn expandMacro(pp: *Preprocessor, lexer: *Lexer, raw: RawToken) Error!void {
    var buf = ExpandBuffer.init(pp.compilation.gpa);
    defer buf.deinit();

    try buf.append(tokenFromRaw(raw));
    try pp.expandMacroExhaustive(lexer, &buf, 0, 1, true);
    //std.debug.print("Result: ", .{});
    //try pp.debugTokenBuf(buf.items);
    try pp.tokens.ensureTotalCapacity(pp.compilation.gpa, pp.tokens.len + buf.items.len);
    for (buf.items) |*r| {
        pp.tokens.appendAssumeCapacity(r.*);
    }
}

// mark that this token has been expanded from `loc`
fn markExpandedFrom(pp: *Preprocessor, token: *Token, loc: Source.Location) !void {
    if (loc.id == .generated) return;
    const newLoc = try pp.arena.allocator().create(Source.Location);
    newLoc.* = loc;
    newLoc.next = token.loc.next;
    token.loc.next = newLoc;
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
fn pasteTokens(pp: *Preprocessor, lhs: Token, rhs: Token) Error!Token {
    if (lhs.id == .EmptyArg)
        return rhs
    else if (rhs.id == .EmptyArg)
        return lhs;

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

    const pastedToken = lexer.next();
    const next = lexer.next().id;

    if (next != .NewLine and next != .Eof) {
        try pp.compilation.addDiagnostic(.{
            .tag = .pasting_formed_invalid,
            .loc = .{ .id = lhs.loc.id, .byteOffset = lhs.loc.byteOffset },
            .extra = .{ .str = try pp.arena.allocator().dupe(u8, pp.generated.items[start..end]) },
        });
    }

    return Token{
        .id = pastedToken.id,
        .loc = .{
            .id = .generated,
            .byteOffset = @intCast(start),
        },
    };
}

/// Defines a new macro and warns  if it  is a duplicate
fn defineMacro(pp: *Preprocessor, nameToken: RawToken, macro: Macro) Error!void {
    const name = pp.tokSliceSafe(nameToken);
    const gop = try pp.defines.getOrPut(name);
    if (gop.found_existing and !gop.value_ptr.eql(macro, pp)) {
        try pp.compilation.addDiagnostic(.{
            .tag = if (gop.value_ptr.isBuiltin) .builtin_macro_redefined else .macro_redefined,
            .loc = .{ .id = nameToken.source, .byteOffset = nameToken.start },
            .extra = .{ .str = name },
        });
    }

    gop.value_ptr.* = macro;
}

/// Handle #define directive
fn define(pp: *Preprocessor, lexer: *Lexer) Error!void {
    // get the macro name and validate.
    const macroName = lexer.next();
    if (macroName.id == .KeywordDefined) {
        try pp.addError(macroName, .defined_as_macro_name);
        return skipToNewLine(lexer);
    }

    if (!macroName.id.isMacroIdentifier()) {
        try pp.addError(macroName, .macro_name_must_be_identifier);
        return skipToNewLine(lexer);
    }

    var first = lexer.next();
    first.id.simplifyMacroKeyword();

    if (first.id == .NewLine or first.id == .Eof) {
        return pp.defineMacro(macroName, .{
            .params = undefined,
            .tokens = undefined,
            .varArgs = false,
            .isFunc = false,
            .loc = undefined,
        });
    } else if (first.start == macroName.end) {
        if (first.id == .LParen)
            return pp.defineFunc(lexer, macroName, first);
        try pp.addError(first, .whitespace_after_macro_name);
    } else if (first.id == .HashHash) {
        try pp.addError(first, .hash_hash_at_start);
    }

    pp.tokenBuffer.items.len = 0;
    try pp.tokenBuffer.append(first);

    while (true) {
        var token = lexer.next();
        token.id.simplifyMacroKeyword();
        switch (token.id) {
            .HashHash => {
                const next = lexer.next();
                if (next.id == .NewLine or next.id == .Eof) {
                    try pp.addError(token, .hash_hash_at_end);
                    break;
                }

                try pp.tokenBuffer.append(token);
                try pp.tokenBuffer.append(next);
            },
            .NewLine, .Eof => break,
            else => try pp.tokenBuffer.append(token),
        }
    }

    const list = try pp.arena.allocator().dupe(RawToken, pp.tokenBuffer.items);
    try pp.defineMacro(macroName, .{
        .loc = .{ .id = macroName.source, .byteOffset = macroName.start },
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
    while (true) {
        var token = lexer.next();
        if (token.id == .RParen)
            break;

        if (params.items.len != 0) {
            if (token.id != .Comma)
                try pp.addError(token, .invalid_token_param_list)
            else
                token = lexer.next();
        }

        if (token.id == .Eof)
            return pp.addError(token, .unterminated_macro_param_list);

        if (token.id == .Ellipsis) {
            varArgs = true;
            const rParen = lexer.next();
            if (rParen.id != .RParen) {
                try pp.addError(rParen, .missing_paren_param_list);
                try pp.addError(lParen, .to_match_paren);
            }

            break;
        }

        if (!token.id.isMacroIdentifier()) {
            try pp.addError(token, .invalid_token_param_list);
            continue;
        }

        try params.append(pp.tokSliceSafe(token));
    }

    // Collect the body tokens and validate # and ##'s found.
    pp.tokenBuffer.items.len = 0;
    tokenLoop: while (true) {
        var token = lexer.next();
        switch (token.id) {
            .NewLine, .Eof => break,
            .Hash => {
                const param = lexer.next();
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
                token = param;
            },

            .HashHash => {
                const start = lexer.index;
                const next = lexer.next();

                if (next.id == .NewLine or next.id == .Eof) {
                    try pp.addError(token, .hash_hash_at_end);
                    continue;
                }

                lexer.index = start;
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
        .loc = .{ .id = macroName.source, .byteOffset = macroName.start },
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

    try pp.preprocess(newSource);
}

const Pragmas = enum {
    once,
    GCC,
    clang,

    const PragmaGCC = enum {
        warning,
        @"error",
        diagnostic,

        const Diagnostics = enum {
            ignored,
            warning,
            @"error",
            fatal,
            push,
            pop,
        };
    };
};
const PragmaState = struct {
    SeenPragmaOnce: bool = false,
};

fn gccDiagnostic(pp: *Preprocessor, pragmaTokens: []const RawToken) !bool {
    if (pragmaTokens.len == 0) return false;
    if (std.meta.stringToEnum(Pragmas.PragmaGCC.Diagnostics, pp.tokenSlice(pragmaTokens[0]))) |diagnostic| {
        switch (diagnostic) {
            .ignored, .warning, .@"error", .fatal => {
                const text = pp.pasteStringsUnsafe(RawToken, pragmaTokens[1..]) catch |err| switch (err) {
                    error.ExpectedStringLiteral => return false,
                    else => |e| return e,
                };
                if (!std.mem.startsWith(u8, text, "-W")) return false;
                const new_kind = switch (diagnostic) {
                    .ignored => Diagnostics.Kind.off,
                    .warning => Diagnostics.Kind.warning,
                    .@"error" => Diagnostics.Kind.@"error",
                    .fatal => Diagnostics.Kind.@"fatal error",
                    else => unreachable,
                };

                try pp.compilation.diag.set(text[2..], new_kind);
                return true;
            },
            .push, .pop => {},
        }
    }
    return false;
}

/// Handle a GCC pragma. Return true if the pragma is recognized (even if there are errors)
/// return false if the pragma is unknown
fn gccPragma(pp: *Preprocessor, pragmaTokens: []const RawToken) !bool {
    if (pragmaTokens.len == 0) return false;
    if (std.meta.stringToEnum(Pragmas.PragmaGCC, pp.tokenSlice(pragmaTokens[0]))) |gcc_pragma| {
        switch (gcc_pragma) {
            .warning, .@"error" => {
                const text = pp.pasteStringsUnsafe(RawToken, pragmaTokens[1..]) catch |er| switch (er) {
                    error.ExpectedStringLiteral => {
                        const extra = Diagnostics.Message.Extra{ .str = @tagName(gcc_pragma) };
                        try pp.compilation.addDiagnostic(.{ .tag = .pragma_requires_string_literal, .loc = tokenFromRaw(pragmaTokens[0]).loc, .extra = extra });
                        return true;
                    },
                    else => |e| return e,
                };
                const extra = Diagnostics.Message.Extra{ .str = try pp.arena.allocator().dupe(u8, text) };
                const diagnosticTag: Diagnostics.Tag = if (gcc_pragma == .warning) .pragma_warning_message else .pragma_error_message;
                try pp.compilation.addDiagnostic(.{ .tag = diagnosticTag, .loc = tokenFromRaw(pragmaTokens[0]).loc, .extra = extra });
                return true;
            },
            .diagnostic => if (try pp.gccDiagnostic(pragmaTokens[1..])) return true,
        }
    }
    return false;
}

/// Handle a pragma directive
fn pragma(pp: *Preprocessor, lexer: *Lexer, tok: RawToken, pragmaState: *PragmaState) !void {
    const tokenBuffStart = pp.tokenBuffer.items.len;
    defer pp.tokenBuffer.items.len = tokenBuffStart;

    while (true) {
        const nextToken = lexer.next();
        if (nextToken.id == .NewLine) break;
        try pp.tokenBuffer.append(nextToken);
    }
    const pragmaTokens = pp.tokenBuffer.items[tokenBuffStart..];
    if (pragmaTokens.len > 0) {
        if (std.meta.stringToEnum(Pragmas, pp.tokenSlice(pragmaTokens[0]))) |pragmaType| {
            switch (pragmaType) {
                .once => {
                    const prev = try pp.pragmaOnce.fetchPut(lexer.source, {});
                    if (prev != null and !pragmaState.SeenPragmaOnce) {
                        return error.PragmaOnce;
                    } else {
                        pragmaState.SeenPragmaOnce = true;
                    }
                    return;
                },
                .GCC => if (try pp.gccPragma(pragmaTokens[1..])) return,
                .clang => {},
            }
        }
    }
    const str = if (pragmaTokens.len == 0) "" else lexer.buffer[pragmaTokens[0].start..pragmaTokens[pragmaTokens.len - 1].end];
    try pp.compilation.addDiagnostic(.{
        .tag = .unsupported_pragma,
        .loc = .{ .id = tok.source, .byteOffset = tok.start },
        .extra = .{ .str = str },
    });
}

fn findIncludeSource(pp: *Preprocessor, lexer: *Lexer) !Source {
    const start = pp.tokens.len;
    defer pp.tokens.len = start;

    var first = lexer.next();
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

        try pp.compilation.addDiagnostic(.{ .tag = .header_str_closing, .loc = .{ .id = first.source, .byteOffset = first.start } });
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
    const newLine = lexer.next();
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
    return pp.compilation.findInclude(first, filename, fileNameTK.id == .StringLiteral);
}

/// pretty print tokens and try to preserve whitespace
pub fn prettyPrintTokens(pp: *Preprocessor, w: anytype) !void {
    var i: usize = 0;
    var cur: Token = pp.tokens.get(i);
    while (true) {
        if (cur.id == .Eof) break;

        const slice = pp.expandedSlice(cur);
        try w.writeAll(slice);

        i += 1;
        const next = pp.tokens.get(i);
        if (next.id == .Eof) {
            try w.writeByte('\n');
        } else if (next.loc.next != null or next.loc.id == .generated) {
            // next was expanded from a macro
            try w.writeByte(' ');
        } else if (next.loc.id == cur.loc.id) {
            try w.writeByte(' ');
        } else {
            // next was included from another file
            try w.writeByte('\n');
        }
        cur = next;
    }
}

fn printInBetween(slice: []const u8, w: anytype) !void {
    var inBetween = slice;
    while (true) {
        if (std.mem.indexOfScalar(u8, inBetween, '#') orelse std.mem.indexOf(u8, inBetween, "//")) |some| {
            try w.writeAll(inBetween[0..some]);
            inBetween = inBetween[some..];
            const nl = std.mem.indexOfScalar(u8, inBetween, '\n') orelse inBetween.len;
            inBetween = inBetween[nl..];
        } else if (std.mem.indexOf(u8, inBetween, "/*")) |some| {
            try w.writeAll(inBetween[0..some]);
            inBetween = inBetween[some..];
            const nl = std.mem.indexOf(u8, inBetween, "*/") orelse inBetween.len;
            inBetween = inBetween[nl + 2 ..];
        } else break;
    }
    try w.writeAll(inBetween);
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
