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
gpa: std.mem.Allocator,
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
/// Map from Source.ID to macro name in the `#ifndef` condition which guards the source, if any
includeGuards: std.AutoHashMapUnmanaged(Source.ID, []const u8) = .{},

/// Dump current state to stderr
verbose: bool = false,
preserveWhitespace: bool = false,

const BuiltinMacros = struct {
    const args = [1][]const u8{"X"};
    const hasAttribute = [1]RawToken{makeFeatCheckMacro(.MacroParamHasAttribute)};
    const hasWarning = [1]RawToken{makeFeatCheckMacro(.MacroParamHasWarning)};
    const hasFeature = [1]RawToken{makeFeatCheckMacro(.MacroParamHasFeature)};
    const hasExtension = [1]RawToken{makeFeatCheckMacro(.MacroParamHasExtension)};
    const hasBuiltin = [1]RawToken{makeFeatCheckMacro(.MacroParamHasBuiltin)};
    const hasInclude = [1]RawToken{makeFeatCheckMacro(.MacroParamHasInclude)};
    const hasIncludeNext = [1]RawToken{makeFeatCheckMacro(.MacroParamHasIncludeNext)};
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
    try pp.defines.putNoClobber(name, .{
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
    try pp.addBuiltinMacro("__has_include", true, &BuiltinMacros.hasInclude);
    try pp.addBuiltinMacro("__has_include_next", true, &BuiltinMacros.hasIncludeNext);
    try pp.addBuiltinMacro("__is_identifier", true, &BuiltinMacros.isIdentifier);
    try pp.addBuiltinMacro("_Pragma", true, &BuiltinMacros.pragmaOperator);
    try pp.addBuiltinMacro("__FILE__", false, &BuiltinMacros.file);
    try pp.addBuiltinMacro("__LINE__", false, &BuiltinMacros.line);
    try pp.addBuiltinMacro("__COUNTER__", false, &BuiltinMacros.counter);
}

pub fn init(comp: *Compilation) Preprocessor {
    const pp = Preprocessor{
        .comp = comp,
        .gpa = comp.gpa,
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
        Token.free(loc, pp.gpa);
    }
    pp.tokens.deinit(pp.gpa);
    pp.arena.deinit();
    pp.tokenBuffer.deinit();
    pp.charBuffer.deinit();
    pp.poisonedIdentifiers.deinit();
    pp.topExpansionBuffer.deinit();
    pp.includeGuards.deinit(pp.gpa);
}

/// Return the name of the #ifndef guard macro that starts a source, if any.
fn findIncludeGuard(pp: *Preprocessor, source: Source) ?[]const u8 {
    var lexer = Lexer{
        .buffer = source.buffer,
        .comp = pp.comp,
        .source = source.id,
    };

    var hash = lexer.nextNoWhiteSpace();
    while (hash.id == .NewLine)
        hash = lexer.nextNoWhiteSpace();
    if (hash.id != .Hash) return null;

    const ifndef = lexer.nextNoWhiteSpace();
    if (ifndef.id != .KeywordIfndef) return null;

    const guard = lexer.nextNoWhiteSpace();
    if (guard.id != .Identifier) return null;

    return pp.getTokenSlice(guard);
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
    var guardName = pp.findIncludeGuard(source);
    pp.preprocessCount += 1;
    var lexer = Lexer{
        .buffer = source.buffer,
        .source = source.id,
        .comp = pp.comp,
    };

    // Estimate how many new tokens this source will contain.
    const estimatedTokenCount = source.buffer.len / 8;
    try pp.tokens.ensureTotalCapacity(pp.gpa, pp.tokens.len + estimatedTokenCount);

    var ifLevel: u8 = 0;
    var ifKind = std.PackedIntArray(u2, 256).init([1]u2{0} ** 256);
    const untilElse = 0;
    const untilEndIf = 1;
    const untilEndIfSeenElse = 2;

    var startOfLine = true;
    while (true) {
        var token = lexer.next();
        switch (token.id) {
            .Hash => if (!startOfLine) try pp.tokens.append(pp.gpa, tokenFromRaw(token)) else {
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
                            if (pp.verbose)
                                pp.verboseLog(directive, "entering then branch of #if", .{});
                        } else {
                            ifKind.set(ifLevel, untilElse);
                            try pp.skip(&lexer, .untilElse);
                            if (pp.verbose)
                                pp.verboseLog(directive, "entering else branch of #if", .{});
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
                            if (pp.verbose)
                                pp.verboseLog(directive, "entering then branch of #ifdef", .{});
                        } else {
                            ifKind.set(ifLevel, untilElse);
                            try pp.skip(&lexer, .untilElse);
                            if (pp.verbose)
                                pp.verboseLog(directive, "entering else branch of #ifdef", .{});
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
                        } else if (ifLevel == 1) {
                            guardName = null;
                        }

                        switch (ifKind.get(ifLevel)) {
                            untilElse => if (try pp.expr(&lexer)) {
                                ifKind.set(ifLevel, untilEndIf);
                                if (pp.verbose)
                                    pp.verboseLog(directive, "entering then branch of #elif", .{});
                            } else {
                                try pp.skip(&lexer, .untilElse);
                                if (pp.verbose)
                                    pp.verboseLog(directive, "entering else branch of #elif", .{});
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
                        } else if (ifLevel == 1) {
                            guardName = null;
                        }

                        switch (ifKind.get(ifLevel)) {
                            untilElse => {
                                ifKind.set(ifLevel, untilEndIfSeenElse);
                                if (pp.verbose)
                                    pp.verboseLog(directive, "#else branch here", .{});
                            },
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
                            guardName = null;
                            try pp.addError(directive, .else_without_if);
                            continue;
                        } else if (ifLevel == 1) {
                            const savedLexer = lexer;
                            defer lexer = savedLexer;

                            var next = lexer.nextNoWhiteSpace();
                            while (next.id == .NewLine) : (next = lexer.nextNoWhiteSpace()) {}
                            if (next.id != .Eof) guardName = null;
                        }
                        ifLevel -= 1;
                    },

                    .KeywordDefine => try pp.define(&lexer),
                    .KeywordInclude => try pp.include(&lexer, .First),
                    .KeywordIncludeNext => {
                        try pp.comp.diag.add(.{
                            .tag = .include_next,
                            .loc = .{ .id = token.source, .byteOffset = directive.start, .line = directive.line },
                        }, &.{});
                        if (pp.includeDepth == 0) {
                            try pp.comp.diag.add(.{
                                .tag = .include_next_outside_header,
                                .loc = .{ .id = token.source, .byteOffset = directive.start, .line = directive.line },
                            }, &.{});
                            try pp.include(&lexer, .First);
                        } else {
                            try pp.include(&lexer, .Next);
                        }
                    },
                    .KeywordPragma => try pp.pragma(&lexer, directive, null, &.{}),

                    .KeywordLine => {
                        const digits = lexer.nextNoWhiteSpace();
                        if (digits.id != .PPNumber)
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

                    .PPNumber => {
                        // # number "file" flags
                        // TODO: validate that the pp_num token is solely digits
                        // if not, emit `GNU line marker directive requires a simple digit sequence`
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

            .WhiteSpace => if (pp.preserveWhitespace) try pp.tokens.append(pp.gpa, tokenFromRaw(token)),
            .NewLine => {
                startOfLine = true;
                if (pp.preserveWhitespace)
                    try pp.tokens.append(pp.gpa, tokenFromRaw(token));
            },

            .Eof => {
                if (ifLevel != 0)
                    try pp.addError(token, .unterminated_conditional_directive);
                if (source.buffer.len > 0 and source.buffer[source.buffer.len - 1] != '\n')
                    try pp.addError(token, .newline_eof);
                if (guardName) |name| {
                    if (try pp.includeGuards.fetchPut(pp.gpa, source.id, name)) |prev| {
                        assert(std.mem.eql(u8, name, prev.value));
                    }
                }
                return tokenFromRaw(token);
            },

            else => {
                if (token.id.isMacroIdentifier() and pp.poisonedIdentifiers.get(pp.getTokenSlice(token)) != null)
                    try pp.addError(token, .poisoned_identifier);

                if (std.mem.eql(u8, lexer.buffer[token.start..token.end], "__has_include")) {
                    token.id.simplifyMacroKeyword();
                    try pp.comp.diag.add(.{
                        .tag = .preprocessing_directive_only,
                        .loc = .{ .id = token.source, .byteOffset = token.start, .line = token.line },
                        .extra = .{ .str = lexer.buffer[token.start..token.end] },
                    }, &.{});
                }

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
    try pp.tokens.ensureTotalCapacity(pp.gpa, pp.tokens.len + EstimatedTokenCount);

    while (true) {
        const tok = tokenizer.next();
        if (tok.id == .Eof) return tokenFromRaw(tok);
        try pp.tokens.append(pp.gpa, tokenFromRaw(tok));
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

fn verboseLog(pp: *Preprocessor, raw: RawToken, comptime fmt: []const u8, args: anytype) void {
    const source = pp.comp.getSource(raw.source);
    const lineCol = source.getLineCol(.{ .id = raw.source, .line = raw.line, .byteOffset = raw.start });

    const stderr = std.io.getStdErr().writer();
    var buffWriter = std.io.bufferedWriter(stderr);
    const writer = buffWriter.writer();
    defer buffWriter.flush() catch {};
    writer.print("{s}:{d}:{d}: ", .{ source.path, lineCol.lineNO, lineCol.col }) catch return;
    writer.print(fmt, args) catch return;
    writer.writeByte('\n') catch return;
    writer.writeAll(lineCol.line) catch return;
    writer.writeByte('\n') catch return;
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
        for (pp.topExpansionBuffer.items) |token|
            Token.free(token.expansionLocs, pp.gpa);
        pp.tokens.len = start;
    }

    pp.topExpansionBuffer.items.len = 0;
    const eof = while (true) {
        const token = lexer.next();
        switch (token.id) {
            .NewLine, .Eof => break token,
            .WhiteSpace => if (pp.topExpansionBuffer.items.len == 0) continue,
            else => {},
        }

        try pp.topExpansionBuffer.append(tokenFromRaw(token));
    } else unreachable;

    if (pp.topExpansionBuffer.items.len != 0) {
        pp.expansionSourceLoc = pp.topExpansionBuffer.items[0].loc;
        try pp.expandMacroExhaustive(lexer, &pp.topExpansionBuffer, 0, pp.topExpansionBuffer.items.len, false, .Expr);
    }

    for (pp.topExpansionBuffer.items) |token| {
        if (token.id == .MacroWS) continue;
        if (!token.id.validPreprocessorExprStart()) {
            try pp.comp.diag.add(.{
                .tag = .invalid_preproc_expr_start,
                .loc = token.loc,
            }, token.expansionSlice());

            return false;
        }
        break;
    } else {
        try pp.addError(eof, .expected_value_in_expr);
        return false;
    }

    // validate the tokens in the expression
    try pp.tokens.ensureUnusedCapacity(pp.gpa, pp.topExpansionBuffer.items.len);
    var i: usize = 0;
    const items = pp.topExpansionBuffer.items;
    while (i < items.len) : (i += 1) {
        var token = items[i];
        switch (token.id) {
            .StringLiteral,
            .StringLiteralUTF_8,
            .StringLiteralUTF_16,
            .StringLiteralUTF_32,
            .StringLiteralWide,
            => {
                try pp.comp.diag.add(.{
                    .tag = .string_literal_in_pp_expr,
                    .loc = token.loc,
                }, token.expansionSlice());
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
                try pp.comp.diag.add(.{
                    .tag = .invalid_preproc_operator,
                    .loc = token.loc,
                }, token.expansionSlice());
                return false;
            },

            .MacroWS, .WhiteSpace => continue,

            else => if (token.id.isMacroIdentifier()) {
                if (token.id == .KeywordDefined) {
                    const tokenConsumed = try pp.handleKeywordDefined(&token, items[i + 1 ..], eof);
                    i += tokenConsumed;
                } else {
                    try pp.comp.diag.add(.{
                        .tag = .undefined_macro,
                        .loc = token.loc,
                        .extra = .{ .str = pp.expandedSlice(token) },
                    }, token.expansionSlice());

                    if (i + 1 < pp.topExpansionBuffer.items.len and
                        pp.topExpansionBuffer.items[i + 1].id == .LParen)
                    {
                        try pp.comp.diag.add(.{
                            .tag = .fn_macro_undefined,
                            .loc = token.loc,
                            .extra = .{ .str = pp.expandedSlice(token) },
                        }, token.expansionSlice());
                        return false;
                    }

                    token.id = .Zero; // undefined macro
                }
            },
        }
        pp.tokens.appendAssumeCapacity(token);
    }

    try pp.tokens.append(pp.gpa, .{
        .id = .Eof,
        .loc = tokenFromRaw(eof).loc,
    });

    var parser = Parser{
        .pp = pp,
        .comp = pp.comp,
        .gpa = pp.gpa,
        .tokenIds = pp.tokens.items(.id),
        .tokenIdx = @intCast(start),
        .arena = pp.arena.allocator(),
        .inMacro = true,
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
        .fieldAttrBuffer = undefined,
        .stringsIds = undefined,
    };

    return parser.macroExpr();
}

/// Turns macro_tok from .keyword_defined into .zero or .one depending on whether the argument is defined
/// Returns the number of tokens consumed
fn handleKeywordDefined(pp: *Preprocessor, macroToken: *Token, tokens: []const Token, eof: RawToken) !usize {
    std.debug.assert(macroToken.id == .KeywordDefined);
    var it = TokenIterator.init(tokens);
    const first = it.nextNoWS() orelse {
        try pp.addError(eof, .macro_name_missing);
        return it.i;
    };
    switch (first.id) {
        .LParen => {},
        else => {
            if (!first.id.isMacroIdentifier()) {
                try pp.comp.diag.add(.{
                    .tag = .macro_name_must_be_identifier,
                    .loc = first.loc,
                    .extra = .{ .str = pp.expandedSlice(first) },
                }, first.expansionSlice());
            }
            macroToken.id = if (pp.defines.contains(pp.expandedSlice(first))) .One else .Zero;
            return it.i;
        },
    }
    const second = it.nextNoWS() orelse {
        try pp.addError(eof, .macro_name_missing);
        return it.i;
    };
    if (!second.id.isMacroIdentifier()) {
        try pp.comp.diag.add(.{
            .tag = .macro_name_must_be_identifier,
            .loc = second.loc,
        }, second.expansionSlice());
        return it.i;
    }
    macroToken.id = if (pp.defines.contains(pp.expandedSlice(second))) .One else .Zero;

    const last = it.nextNoWS();
    if (last == null or last.?.id != .RParen) {
        const tok = last orelse tokenFromRaw(eof);
        try pp.comp.diag.add(.{
            .tag = .closing_paren,
            .loc = tok.loc,
        }, tok.expansionSlice());
        try pp.comp.diag.add(.{
            .tag = .to_match_paren,
            .loc = first.loc,
        }, first.expansionSlice());
    }

    return it.i;
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

fn removePlaceMarkers(buf: *ExpandBuffer) void {
    var i: usize = buf.items.len -% 1;
    while (i < buf.items.len) : (i -%= 1) {
        if (buf.items[i].id == .PlaceMarker) {
            const placemarker = buf.orderedRemove(i);
            Token.free(placemarker.expansionLocs, buf.allocator);
        }
    }
}

fn deinitMacroArguments(allocator: Allocator, args: *const MacroArguments) void {
    for (args.items) |item| {
        for (item) |token|
            Token.free(token.expansionLocs, allocator);
        allocator.free(item);
    }
    args.deinit();
}

fn expandObjMacro(pp: *Preprocessor, simpleMacro: *const Macro) Error!ExpandBuffer {
    var buff = ExpandBuffer.init(pp.gpa);
    errdefer buff.deinit();
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
            .WhiteSpace => if (pp.preserveWhitespace) buff.appendAssumeCapacity(token),
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

                buff.appendAssumeCapacity(try pp.makeGeneratedToken(start, .PPNumber, token));
            },
            .MacroCounter => {
                defer pp.counter += 1;
                const start = pp.comp.generatedBuffer.items.len;
                try pp.comp.generatedBuffer.writer().print("{d}\n", .{pp.counter});

                buff.appendAssumeCapacity(try pp.makeGeneratedToken(start, .PPNumber, token));
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
    if (pp.charBuffer.items[pp.charBuffer.items.len - 1] == '\\') {
        const tok = tokens[tokens.len - 1];
        try pp.comp.diag.add(.{
            .tag = .invalid_pp_stringify_escape,
            .loc = tok.loc,
        }, tok.expansionSlice());
        pp.charBuffer.items.len -= 1; // remove `\`
    }

    try pp.charBuffer.appendSlice("\"\n");
}

fn reconstructIncludeString(pp: *Preprocessor, paramTokens: []const Token) !?[]const u8 {
    const charTop = pp.charBuffer.items.len;
    defer pp.charBuffer.items.len = charTop;

    // Trim leading/trailing whitespace
    var begin: usize = 0;
    var end = paramTokens.len;
    while (begin < end and paramTokens[begin].id == .MacroWS) begin += 1;
    while (end > begin and paramTokens[end - 1].id == .MacroWS) end -= 1;
    const params = paramTokens[begin..end];

    if (params.len == 0) {
        try pp.comp.diag.add(.{
            .tag = .expected_filename,
            .loc = paramTokens[0].loc,
        }, paramTokens[0].expansionSlice());
        return null;
    }

    // no string pasting
    if (params[0].id == .StringLiteral and params.len > 1) {
        try pp.comp.diag.add(.{
            .tag = .closing_paren,
            .loc = params[1].loc,
        }, params[1].expansionSlice());
        return null;
    }

    for (params) |tok| {
        const str = pp.expandedSliceExtra(tok, .PreserveMacroWS);
        try pp.charBuffer.appendSlice(str);
    }

    const includeStr = pp.charBuffer.items[charTop..];
    if (includeStr.len < 3) {
        try pp.comp.diag.add(.{
            .tag = .empty_filename,
            .loc = params[0].loc,
        }, params[0].expansionSlice());
        return null;
    }

    switch (includeStr[0]) {
        '<' => {
            if (includeStr[includeStr.len - 1] != '>') {
                // Ugly hack to find out where the '>' should go, since we don't have the closing ')' location
                const start = params[0].loc;
                try pp.comp.diag.add(.{
                    .tag = .header_str_closing,
                    .loc = .{ .id = start.id, .byteOffset = start.byteOffset + @as(u32, @intCast(includeStr.len)) + 1, .line = start.line },
                }, params[0].expansionSlice());

                try pp.comp.diag.add(.{
                    .tag = .header_str_match,
                    .loc = params[0].loc,
                }, params[0].expansionSlice());
                return null;
            }
            return includeStr;
        },

        '"' => return includeStr,

        else => {
            try pp.comp.diag.add(.{
                .tag = .expected_filename,
                .loc = params[0].loc,
            }, params[0].expansionSlice());
            return null;
        },
    }
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
            for (paramTokens) |tok| {
                if (tok.id == .MacroWS) continue;
                if (!tok.id.isMacroIdentifier()) {
                    invalid = tok;
                    break;
                }

                if (identifier) |_| invalid = tok else identifier = tok;
            }
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

        .MacroParamHasInclude, .MacroParamHasIncludeNext => {
            const includeStr = (try pp.reconstructIncludeString(paramTokens)) orelse return false;
            const includeType: Compilation.IncludeType = switch (includeStr[0]) {
                '"' => .Quotes,
                '<' => .AngleBrackets,
                else => unreachable,
            };
            const filename = includeStr[1 .. includeStr.len - 1];

            if (builtin == .MacroParamHasInclude or pp.includeDepth == 0) {
                if (builtin == .MacroParamHasIncludeNext) {
                    try pp.comp.diag.add(.{
                        .tag = .include_next_outside_header,
                        .loc = srcLoc,
                    }, &.{});
                }
                return pp.comp.hasInclude(filename, srcLoc.id, includeType, .First);
            }
            return pp.comp.hasInclude(filename, srcLoc.id, includeType, .Next);
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
    var buf = ExpandBuffer.init(pp.gpa);
    try buf.ensureTotalCapacity(funcMacro.tokens.len);
    errdefer buf.deinit();

    var expandedVarArguments = ExpandBuffer.init(pp.gpa);
    var varArguments = ExpandBuffer.init(pp.gpa);

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
                        .MacroParam, .MacroParamNoExpand => if (args.items[rawNext.end].len > 0)
                            args.items[rawNext.end]
                        else
                            &[1]Token{tokenFromRaw(.{ .id = .PlaceMarker, .source = .generated })},
                        .KeywordVarArgs => varArguments.items,
                        else => &[1]Token{tokenFromRaw(rawNext)},
                    };
                    try pp.pasteTokens(&buf, next);
                    if (next.len != 0) break;
                }
            },

            .MacroParamNoExpand => {
                const slice = if (args.items[raw.end].len > 0)
                    args.items[raw.end]
                else
                    &[1]Token{tokenFromRaw(.{ .id = .PlaceMarker, .source = .generated })};
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
            .MacroParamHasBuiltin,
            .MacroParamHasInclude,
            .MacroParamHasIncludeNext,
            .MacroParamIsIdentifier,
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
                try buf.append(try pp.makeGeneratedToken(start, .PPNumber, tokenFromRaw(raw)));
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

            // GNU extension
            .Comma => {
                if (tokenIdx + 2 < funcMacro.tokens.len and funcMacro.tokens[tokenIdx + 1].id == .HashHash) {
                    const hashhash = funcMacro.tokens[tokenIdx + 1];
                    var maybeVaArgs = funcMacro.tokens[tokenIdx + 2];
                    var consumed: usize = 2;
                    if (maybeVaArgs.id == .MacroWS and tokenIdx + 3 < funcMacro.tokens.len) {
                        consumed = 3;
                        maybeVaArgs = funcMacro.tokens[tokenIdx + 3];
                    }

                    if (maybeVaArgs.id == .KeywordVarArgs) {
                        // GNU extension: `, ##__VA_ARGS__` deletes the comma if __VA_ARGS__ is empty
                        tokenIdx += consumed;
                        if (funcMacro.params.len == expandedArgs.items.len) {
                            // Empty __VA_ARGS__, drop the comma
                            try pp.addError(hashhash, .comma_deletion_va_args);
                        } else if (funcMacro.params.len == 0 and expandedArgs.items.len == 1 and expandedArgs.items[0].len == 0) {
                            // Ambiguous whether this is "empty __VA_ARGS__" or "__VA_ARGS__ omitted"
                            if (pp.comp.langOpts.standard.isGNU()) {
                                // GNU standard, drop the comma
                                try pp.addError(hashhash, .comma_deletion_va_args);
                            } else {
                                // C standard, retain the comma
                                try buf.append(tokenFromRaw(raw));
                            }
                        } else {
                            try buf.append(tokenFromRaw(raw));
                            if (expandedVarArguments.items.len > 0 or varArguments.items.len == funcMacro.params.len) {
                                try pp.addError(hashhash, .comma_deletion_va_args);
                            }
                            const rawLoc = Source.Location{
                                .id = maybeVaArgs.source,
                                .byteOffset = maybeVaArgs.start,
                                .line = maybeVaArgs.line,
                            };
                            try bufCopyTokens(&buf, expandedVarArguments.items, &.{rawLoc});
                        }
                        continue;
                    }
                }
                // Regular comma, no token pasting with __VA_ARGS__
                try buf.append(tokenFromRaw(raw));
            },
            else => try buf.append(tokenFromRaw(raw)),
        }
    }
    removePlaceMarkers(&buf);

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
    if (tok.flags.expansionDisabled)
        return false;
    return true;
}

fn bufCopyTokens(buf: *ExpandBuffer, tokens: []const Token, src: []const Source.Location) !void {
    try buf.ensureUnusedCapacity(tokens.len);
    for (tokens) |tok| {
        var copy = try tok.dupe(buf.allocator);
        errdefer Token.free(copy.expansionLocs, buf.allocator);
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
) !MacroArguments {
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
                        .tag = .missing_lparen_after_builtin,
                        .loc = nameToken.loc,
                        .extra = .{ .str = pp.expandedSlice(nameToken) },
                    }, token.expansionSlice());
                }
                // Not a macro function call, go over normal identifier, rewind
                lexer.* = savedLexer;
                endIdx.* = oldEnd;
                return error.MissLParen;
            },
        }
    }

    // collect the arguments.
    var parens: u32 = 0;
    var args = MacroArguments.init(pp.gpa);
    errdefer deinitMacroArguments(pp.gpa, &args);

    var curArgument = std.ArrayList(Token).init(pp.gpa);
    defer curArgument.deinit();

    while (true) {
        var tok = try nextBufToken(pp, lexer, buf, startIdx, endIdx, extendBuffer);
        tok.flags.isMacroArg = true;
        switch (tok.id) {
            .Comma => {
                if (parens == 0) {
                    const owned = try curArgument.toOwnedSlice();
                    errdefer pp.gpa.free(owned);
                    try args.append(owned);
                } else {
                    const duped = try tok.dupe(pp.gpa);
                    errdefer Token.free(duped.expansionLocs, pp.gpa);
                    try curArgument.append(duped);
                }
            },

            .LParen => {
                const duped = try tok.dupe(pp.gpa);
                errdefer Token.free(duped.expansionLocs, pp.gpa);
                try curArgument.append(duped);
                parens += 1;
            },

            .RParen => {
                if (parens == 0) {
                    const owned = try curArgument.toOwnedSlice();
                    errdefer pp.gpa.free(owned);
                    try args.append(owned);
                    break;
                } else {
                    const duped = try tok.dupe(pp.gpa);
                    errdefer Token.free(duped.expansionLocs, pp.gpa);
                    try curArgument.append(duped);
                    parens -= 1;
                }
            },

            .Eof => {
                {
                    const owned = try curArgument.toOwnedSlice();
                    errdefer pp.gpa.free(owned);
                    try args.append(owned);
                }

                lexer.* = savedLexer;
                try pp.comp.diag.add(
                    .{ .tag = .unterminated_macro_arg_list, .loc = nameToken.loc },
                    nameToken.expansionSlice(),
                );

                return error.Unterminated;
            },

            .NewLine, .WhiteSpace => {
                try curArgument.append(.{ .id = .MacroWS, .loc = tok.loc });
            },

            else => {
                const duped = try tok.dupe(pp.gpa);
                errdefer Token.free(duped.expansionLocs, pp.gpa);
                try curArgument.append(duped);
            },
        }
    }

    return args;
}

fn removeExpandedTokens(
    pp: *Preprocessor,
    buf: *ExpandBuffer,
    start: usize,
    len: usize,
    movingEndIdx: *usize,
) !void {
    for (buf.items[start .. start + len]) |tok|
        Token.free(tok.expansionLocs, pp.gpa);
    try buf.replaceRange(start, len, &.{});
    movingEndIdx.* -|= len;
}

/// The behavior of `defined` depends on whether we are in a preprocessor
/// expression context (#if or #elif) or not.
/// In a non-expression context it's just an identifier. Within a preprocessor
/// expression it is a unary operator or one-argument function.
const EvalContext = enum {
    Expr,
    NonExpr,
};

/// Helper for safely iterating over a slice of tokens while skipping whitespace
const TokenIterator = struct {
    toks: []const Token,
    i: usize,

    fn init(toks: []const Token) TokenIterator {
        return .{ .toks = toks, .i = 0 };
    }

    fn nextNoWS(self: *TokenIterator) ?Token {
        while (self.i < self.toks.len) : (self.i += 1) {
            const tok = self.toks[self.i];
            if (tok.id == .WhiteSpace or tok.id == .MacroWS) continue;

            self.i += 1;
            return tok;
        }
        return null;
    }
};

fn expandMacroExhaustive(
    pp: *Preprocessor,
    lexer: *Lexer,
    buf: *ExpandBuffer,
    startIdx: usize,
    endIdx: usize,
    extendBuffer: bool,
    evalCtx: EvalContext,
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
            if (macroToken.id == .KeywordDefined and evalCtx == .Expr) {
                idx += 1;
                var it = TokenIterator.init(buf.items[idx..movingEndIdx]);
                if (it.nextNoWS()) |tok| {
                    switch (tok.id) {
                        .LParen => {
                            _ = it.nextNoWS(); // eat (what should be) identifier
                            _ = it.nextNoWS(); // eat (what should be) r paren
                        },
                        .Identifier, .ExtendedIdentifier => {},
                        else => {},
                    }
                }
                idx += it.i;
                continue;
            }

            const macroEntry = pp.defines.getPtr(pp.expandedSlice(macroToken));
            if (macroEntry == null or !shouldExpand(macroToken, macroEntry.?)) {
                idx += 1;
                continue;
            }
            if (macroEntry) |macro| macroHandler: {
                if (macro.isFunc) {
                    var macroScanIdx = idx;
                    // to be saved in case this doesn't turn out to be a call
                    const args = pp.collectMacroFuncArguments(
                        lexer,
                        buf,
                        &macroScanIdx,
                        &movingEndIdx,
                        extendBuffer,
                        macro.isBuiltin,
                    ) catch |err| switch (err) {
                        error.MissLParen => {
                            if (!buf.items[idx].flags.isMacroArg)
                                buf.items[idx].flags.expansionDisabled = true;
                            idx += 1;
                            break :macroHandler;
                        },
                        error.Unterminated => {
                            if (pp.comp.langOpts.emulate == .gcc)
                                idx += 1;
                            try pp.removeExpandedTokens(buf, idx, macroScanIdx - idx, &movingEndIdx);
                            break :macroHandler;
                        },
                        else => |e| return e,
                    };

                    defer {
                        for (args.items) |item|
                            pp.gpa.free(item);
                        args.deinit();
                    }

                    var argsCount = @as(u32, @intCast(args.items.len));
                    // if the macro has zero arguments g() args_count is still 1
                    // an empty token list g() and a whitespace-only token list g(    )
                    // counts as zero arguments for the purposes of argument-count validation
                    if (argsCount == 1 and macro.params.len == 0) {
                        for (args.items[0]) |tok| {
                            if (tok.id != .MacroWS) break;
                        } else {
                            argsCount = 0;
                        }
                    }

                    // Validate argument count.
                    const extra = Diagnostics.Message.Extra{
                        .arguments = .{
                            .expected = @intCast(macro.params.len),
                            .actual = argsCount,
                        },
                    };
                    if (macro.varArgs and argsCount < macro.params.len) {
                        try pp.comp.diag.add(
                            .{ .tag = .expected_at_least_arguments, .loc = buf.items[idx].loc, .extra = extra },
                            buf.items[idx].expansionSlice(),
                        );
                        idx += 1;
                        try pp.removeExpandedTokens(buf, idx, macroScanIdx - idx + 1, &movingEndIdx);
                        continue;
                    }

                    if (!macro.varArgs and argsCount != macro.params.len) {
                        try pp.comp.diag.add(
                            .{ .tag = .expected_arguments, .loc = buf.items[idx].loc, .extra = extra },
                            buf.items[idx].expansionSlice(),
                        );
                        idx += 1;
                        try pp.removeExpandedTokens(buf, idx, macroScanIdx - idx + 1, &movingEndIdx);
                        continue;
                    }

                    //std.debug.print("Expanding func: {s}\n", .{pp.expandedSlice(buf.items[idx])});
                    var expandedArgs = MacroArguments.init(pp.gpa);
                    defer deinitMacroArguments(pp.gpa, &expandedArgs);
                    try expandedArgs.ensureTotalCapacity(args.items.len);
                    for (args.items) |arg| {
                        var expandBuffer = ExpandBuffer.init(pp.gpa);
                        errdefer expandBuffer.deinit();
                        try expandBuffer.appendSlice(arg);

                        try pp.expandMacroExhaustive(lexer, &expandBuffer, 0, expandBuffer.items.len, false, evalCtx);
                        expandedArgs.appendAssumeCapacity(try expandBuffer.toOwnedSlice());
                    }

                    var res = try pp.expandFuncMacro(macroToken.loc, macro, &args, &expandedArgs);
                    defer res.deinit();
                    const tokensAdded = res.items.len;

                    const macroExpansionLocs = macroToken.expansionSlice();
                    for (res.items) |*tok| {
                        try tok.addExpansionLocation(pp.gpa, &.{macroToken.loc});
                        try tok.addExpansionLocation(pp.gpa, macroExpansionLocs);
                    }

                    const tokensRemoved = macroScanIdx - idx + 1;
                    for (buf.items[idx .. idx + tokensRemoved]) |tok| Token.free(tok.expansionLocs, pp.gpa);
                    try buf.replaceRange(idx, tokensRemoved, res.items);

                    movingEndIdx += tokensAdded;
                    // Overflow here means that we encountered an unterminated argument list
                    // while expanding the body of this macro.
                    movingEndIdx -|= tokensRemoved;
                    idx += tokensAdded;
                    doRescan = true;
                } else {
                    const res = try pp.expandObjMacro(macro);
                    defer res.deinit();

                    const macroExpansionLocs = macroToken.expansionSlice();
                    var incrementIdxBy = res.items.len;
                    for (res.items, 0..) |*tok, i| {
                        tok.flags.isMacroArg = macroToken.flags.isMacroArg;
                        try tok.addExpansionLocation(pp.gpa, &.{macroToken.loc});
                        try tok.addExpansionLocation(pp.gpa, macroExpansionLocs);
                        if (tok.id == .KeywordDefined and evalCtx == .Expr) {
                            try pp.comp.diag.add(.{
                                .tag = .expansion_to_defined,
                                .loc = tok.loc,
                            }, tok.expansionSlice());
                        }
                        if (i < incrementIdxBy and (tok.id == .KeywordDefined or pp.defines.contains(pp.expandedSlice(tok.*)))) {
                            incrementIdxBy = i;
                        }
                    }

                    Token.free(buf.items[idx].expansionLocs, pp.gpa);

                    try buf.replaceRange(idx, 1, res.items);
                    idx += incrementIdxBy;
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
    for (buf.items[movingEndIdx..]) |item|
        Token.free(item.expansionLocs, pp.gpa);
    buf.items.len = movingEndIdx;
}

/// Try to expand a macro after a possible candidate has been read from the `tokenizer`
/// into the `raw` token passed as argument
fn expandMacro(pp: *Preprocessor, lexer: *Lexer, raw: RawToken) MacroError!void {
    var sourceToken = tokenFromRaw(raw);
    if (!raw.id.isMacroIdentifier()) {
        sourceToken.id.simplifyMacroKeyword();
        return pp.tokens.append(pp.gpa, sourceToken);
    }

    pp.topExpansionBuffer.items.len = 0;
    try pp.topExpansionBuffer.append(sourceToken);
    pp.expansionSourceLoc = sourceToken.loc;

    try pp.expandMacroExhaustive(lexer, &pp.topExpansionBuffer, 0, 1, true, .NonExpr);
    try pp.tokens.ensureUnusedCapacity(pp.gpa, pp.topExpansionBuffer.items.len);
    for (pp.topExpansionBuffer.items) |*tok| {
        if (tok.id == .MacroWS and !pp.preserveWhitespace) {
            Token.free(tok.expansionLocs, pp.gpa);
            continue;
        }
        tok.id.simplifyMacroKeywordExtra(true);
        pp.tokens.appendAssumeCapacity(tok.*);
    }

    if (pp.preserveWhitespace) {
        try pp.tokens.ensureUnusedCapacity(pp.gpa, pp.addExpansionNL);
        while (pp.addExpansionNL > 0) : (pp.addExpansionNL -= 1) {
            pp.tokens.appendAssumeCapacity(.{ .id = .NewLine, .loc = .{ .id = .generated } });
        }
    }
}

/// Get expanded token source string.
pub fn expandedSlice(pp: *Preprocessor, tok: Token) []const u8 {
    return pp.expandedSliceExtra(tok, .SingleMacroWS);
}

pub fn expandedSliceExtra(pp: *Preprocessor, token: Token, macroWSHandling: enum { SingleMacroWS, PreserveMacroWS }) []const u8 {
    if (token.id.getTokenText()) |some|
        if (!(token.id == .MacroWS and macroWSHandling == .PreserveMacroWS))
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
            Token.free(lhs.expansionLocs, pp.gpa)
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
    defer Token.free(lhs.expansionLocs, pp.gpa);

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
    const pastedId = if (lhs.id == .PlaceMarker and rhs.id == .PlaceMarker)
        .PlaceMarker
    else
        pastedToken.id;
    try lhsTokens.append(try pp.makeGeneratedToken(start, pastedId, lhs));
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
    try pastedToken.addExpansionLocation(pp.gpa, &.{source.loc});
    try pastedToken.addExpansionLocation(pp.gpa, source.expansionSlice());
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

    if (pp.verbose)
        pp.verboseLog(nameToken, "macro {s} defined", .{name});

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
    var params = std.ArrayList([]const u8).init(pp.gpa);
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

fn include(pp: *Preprocessor, lexer: *Lexer, which: Compilation.WhichInclude) MacroError!void {
    const first = lexer.nextNoWhiteSpace();
    const newSource = pp.findIncludeSource(lexer, first, which) catch |er| switch (er) {
        error.InvalidInclude => return,
        else => |e| return e,
    };

    pp.includeDepth += 1;
    defer pp.includeDepth -= 1;
    if (pp.includeDepth > MaxIncludeDepth) {
        try pp.comp.diag.add(.{
            .tag = .too_many_includes,
            .loc = .{ .id = first.source, .byteOffset = first.start, .line = first.line },
        }, &.{});
        return error.StopPreprocessing;
    }

    if (pp.includeGuards.get(newSource.id)) |guard| {
        if (pp.defines.contains(guard))
            return;
    }

    if (pp.verbose)
        pp.verboseLog(first, "include file {s}", .{newSource.path});

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
        try tok.addExpansionLocation(pp.gpa, &.{loc});

    try tok.addExpansionLocation(pp.gpa, argLocs);
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

    try pp.tokens.append(pp.gpa, try pp.makePragmaToken(pragmaToken, operatorLoc, argLocs));
    const pragmaStart = @as(u32, @intCast(pp.tokens.len));

    const pragmaNameToken = try pp.makePragmaToken(nameToken, operatorLoc, argLocs);
    try pp.tokens.append(pp.gpa, pragmaNameToken);

    while (true) {
        const nextToken = lexer.next();
        if (nextToken.id == .WhiteSpace) continue;
        if (nextToken.id == .Eof) {
            try pp.tokens.append(pp.gpa, .{
                .id = .NewLine,
                .loc = .{ .id = .generated },
            });
            break;
        }
        try pp.tokens.append(pp.gpa, try pp.makePragmaToken(nextToken, operatorLoc, argLocs));
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

fn findIncludeFilenameToken(
    pp: *Preprocessor,
    firstToken: RawToken,
    lexer: *Lexer,
    trailingTokenBehavior: enum { IgnoreTrailingTokens, expectNlEof },
) !Token {
    const start = pp.tokens.len;
    defer pp.tokens.len = start;

    var first = firstToken;
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

        try pp.comp.diag.add(.{
            .tag = .header_str_closing,
            .loc = .{
                .id = first.source,
                .byteOffset = lexer.index,
                .line = first.line,
            },
        }, &.{});
        try pp.addError(first, .header_str_match);
    }

    // Try expand if the argument is a macro
    try pp.expandMacro(lexer, first);

    // check that we actually got a string
    const filenameToken = pp.tokens.get(start);
    switch (filenameToken.id) {
        .StringLiteral, .MacroString => {},
        else => {
            try pp.addError(first, .expected_filename);
            try pp.expectNewLine(lexer);
            return error.InvalidInclude;
        },
    }

    // error on the extra tokens.
    switch (trailingTokenBehavior) {
        .expectNlEof => {
            const newLine = lexer.nextNoWhiteSpace();
            if ((newLine.id != .NewLine and newLine.id != .Eof) or pp.tokens.len > start + 1) {
                skipToNewLine(lexer);
                try pp.addError(first, .extra_tokens_directive_end);
            }
        },
        .IgnoreTrailingTokens => {},
    }

    return filenameToken;
}

fn findIncludeSource(
    pp: *Preprocessor,
    lexer: *Lexer,
    first: RawToken,
    which: Compilation.WhichInclude,
) !Source {
    const filenameToken = try pp.findIncludeFilenameToken(first, lexer, .expectNlEof);

    // check for empty filename
    const tkSlice = pp.expandedSlice(filenameToken);
    if (tkSlice.len < 3) {
        try pp.addError(first, .empty_filename);
        return error.InvalidInclude;
    }

    // find the file
    const filename = tkSlice[1 .. tkSlice.len - 1];
    const includeType: Compilation.IncludeType = switch (filenameToken.id) {
        .StringLiteral => .Quotes,
        .MacroString => .AngleBrackets,
        else => unreachable,
    };
    return (try pp.comp.findInclude(filename, first.source, includeType, which)) orelse
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

            try comp.addDefaultPragmaHandlers();

            var pp = Preprocessor.init(&comp);
            defer pp.deinit();

            pp.preserveWhitespace = true;

            const test_runner_macros = try comp.addSourceFromBuffer("<test_runner>", source_text);
            const eof = try pp.preprocess(test_runner_macros);
            try pp.tokens.append(pp.gpa, eof);

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

test "Include guards" {
    const Test = struct {
        /// This is here so that when #elifdef / #elifndef are added we don't forget
        /// to test that they don't accidentally break include guard detection
        fn pairsWithIfndef(tokenID: TokenType) bool {
            return switch (tokenID) {
                .KeywordElIf,
                .KeywordElse,
                => true,

                .KeywordInclude,
                .KeywordIncludeNext,
                .KeywordDefine,
                .KeywordDefined,
                .KeywordUndef,
                .KeywordIfdef,
                .KeywordIfndef,
                .KeywordError,
                .KeywordWarning,
                .KeywordPragma,
                .KeywordLine,
                .KeywordEndIf,
                => false,
                else => unreachable,
            };
        }

        fn skippable(tokenID: TokenType) bool {
            return switch (tokenID) {
                .KeywordDefined, .KeywordVarArgs, .KeywordEndIf => true,
                else => false,
            };
        }

        fn testIncludeGuard(
            allocator: std.mem.Allocator,
            comptime template: []const u8,
            tokenID: TokenType,
            expectedGuards: u32,
        ) !void {
            var comp = Compilation.init(allocator);
            defer comp.deinit();
            var pp = Preprocessor.init(&comp);
            defer pp.deinit();

            const path = try std.fs.path.join(allocator, &.{ ".", "bar.h" });
            defer allocator.free(path);

            _ = try comp.addSourceFromBuffer(path, "int bar = 5;\n");

            var buf = std.ArrayList(u8).init(allocator);
            defer buf.deinit();

            var writer = buf.writer();
            switch (tokenID) {
                .KeywordInclude, .KeywordIncludeNext => try writer.print(template, .{ tokenID.getTokenText().?, " \"bar.h\"" }),
                .KeywordDefine, .KeywordUndef => try writer.print(template, .{ tokenID.getTokenText().?, " BAR" }),
                .KeywordIfndef, .KeywordIfdef => try writer.print(template, .{ tokenID.getTokenText().?, " BAR\n#endif" }),
                else => try writer.print(template, .{ tokenID.getTokenText().?, "" }),
            }

            const source = try comp.addSourceFromBuffer("test.h", buf.items);
            _ = try pp.preprocess(source);

            try std.testing.expectEqual(expectedGuards, pp.includeGuards.count());
        }
    };

    const tags = std.meta.tags(TokenType);
    for (tags) |tag| {
        if (Test.skippable(tag)) continue;
        var copy = tag;
        copy.simplifyMacroKeyword();
        if (copy != tag or tag == .KeywordElse) {
            const insideIfndefTemplate =
                \\//Leading comment (should be ignored)
                \\
                \\#ifndef FOO
                \\#{s}{s}
                \\#endif
            ;
            const expectedGuards: u32 = if (Test.pairsWithIfndef(tag)) 0 else 1;
            try Test.testIncludeGuard(std.testing.allocator, insideIfndefTemplate, tag, expectedGuards);

            const outsideIfndefTemplate =
                \\#ifndef FOO
                \\#endif
                \\#{s}{s}
            ;
            try Test.testIncludeGuard(std.testing.allocator, outsideIfndefTemplate, tag, 0);
        }
    }
}
