const std = @import("std");
const Token = @import("Token.zig");
const TokenType = @import("TokenType.zig");
const Compilation = @import("Compilation.zig");
const Source = @import("Source.zig");
const Lexer = @import("Lexer.zig");

const Allocator = std.mem.Allocator;
const assert = std.debug.assert;

const Preprocessor = @This();
const DefineMap = std.StringHashMap(Marco);
const TokenList = std.ArrayList(Token);

const Marco = union(enum) {
    empty,

    func: struct {
        /// Parameters of the function type macro
        params: []const []const u8,

        /// Token constituting the macro body
        tokens: []const Token,
    },

    simple: []const Token,
};

compilation: *Compilation,
arena: std.heap.ArenaAllocator,
defines: DefineMap,
tokens: TokenList,

pub fn init(comp: *Compilation) Preprocessor {
    return .{
        .comp = comp,
        .arena = std.heap.ArenaAllocator.init(comp.gpa),
        .defines = DefineMap.init(comp.gpa),
        .tokens = TokenList.init(comp.gpa),
    };
}

pub fn deinit(pp: *Preprocessor) void {
    pp.defines.deinit();
    pp.tokens.deinit();
    pp.arena.deinit();
}

const Error = Allocator.Error || error{PreProcessorFailed};

pub fn preprocess(pp: *Preprocessor, source: Source) !void {
    var lexer = Lexer{
        .buffer = source.buffer,
        .source = source,
    };

    // Estimate how many new tokens this source will contain.
    const estimatedTokenCount = source.buffer.len / 8;
    try pp.tokens.ensureTotalCapacity(pp.tokens.items.len + estimatedTokenCount);

    const last = try pp.preprocessInternal(&lexer, .untilEof);
    assert(last == .Eof);
}

fn preprocessInternal(pp: *Preprocessor, lexer: *Lexer, cont: enum { untilEof, untilElse, untilEndIf }) Error!TokenType {
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

                        const loc: Source.SourceLocation = .{
                            .start = start,
                            .end = lexer.index,
                        };

                        var slice = lexer.source.slice(loc);
                        slice = std.mem.trim(u8, slice, "\t\x0B\x0C");

                        return pp.fail(lexer.source, slice, loc);
                    },

                    .KeywordIf => try pp.expandConditional(lexer, try pp.expandBoolExpr(lexer)),
                    .KeywordIfdef => {
                        const macroName = try pp.expectMacroName(lexer);
                        try pp.expectNewLine(lexer, false);
                        try pp.expandConditional(lexer, pp.defines.get(macroName) != null);
                    },

                    .KeywordIfndef => {
                        const macroName = try pp.expectMacroName(lexer);
                        try pp.expectNewLine(lexer, false);
                        try pp.expandConditional(lexer, pp.defines.get(macroName) != null);
                    },

                    .KeywordDefine => pp.define(lexer),
                    .KeywordInclude => return pp.fail(lexer.source, "TODO include directive", directive.loc),
                    .KeywordPragma => return pp.fail(lexer.source, "TODO pragma directive", directive.loc),
                    .KeywordUndef => {
                        const macro_name = try pp.expectMacroName(lexer);
                        _ = pp.defines.remove(macro_name);
                        try pp.expectNewLine(lexer, true);
                    },

                    .KeywordElIf => {
                        if (cont == .untilEof) return pp.fail(lexer.source, "#elif without #if", directive.loc);
                        if (cont == .untilEndIf) return pp.fail(lexer.source, "#elif after #else", directive.loc);
                        return directive.id;
                    },

                    .KeywordElse => {
                        if (cont == .untilEof) return pp.fail(lexer.source, "#else without #if", directive.loc);
                        if (cont == .untilEndIf) return pp.fail(lexer.source, "#else after #else", directive.loc);
                        return directive.id;
                    },

                    .KeywordEndif => {
                        if (cont == .untilEof) return pp.fail(lexer.source, "#endif without #if", directive.loc);
                        return directive.id;
                    },

                    .NewLine => {},
                    .Eof => {
                        if (cont != .untilEof) return pp.fail(lexer.source, "unterminated conditional directive", directive.loc);
                        try pp.tokens.append(token);
                        return directive.id;
                    },
                    else => return pp.fail(lexer.source, "invalid preprocessing directive", directive.loc),
                }
            },

            .NewLine => startOfLine = true,
            .Eof => {
                if (cont != .untilEof) return pp.fail(lexer.source, "unterminated conditional directive", token.loc);
                try pp.tokens.append(token);
                return token.id;
            },

            else => {
                startOfLine = false;
                if (token.id.isMacroIdentifier()) {
                    try pp.expandMacro(lexer, token, &pp.tokens);
                    token.id = .Identifier;
                }
                try pp.tokens.append(token);
            },
        }
    }
}

pub fn fail(pp: *Preprocessor, source: Source, msg: []const u8, token: Token) Error {
    const line_col = source.lineCol(token.loc);
    std.debug.print("{s}:{d}:{d}: error: {s}\n", .{ source.path, line_col.line, line_col.col, msg });
    return error.PreProcessingFailed;
}

fn expectMacroName(pp: *Preprocessor, lexer: *Lexer) Error![]const u8 {
    var macroName = lexer.next();
    if (!macroName.id.isMacroIdentifier())
        return pp.fail(lexer.source, "macro name missing", macroName.loc);

    return lexer.source.slice(macroName.loc);
}

fn expectNewLine(pp: *Preprocessor, lexer: *Lexer, allowEof: bool) Error!void {
    const token = lexer.next();

    if (token.id == .Eof) {
        if (!allowEof) return pp.fail(lexer.source, "unterminated conditional directive", lexer.loc);
        return;
    }

    if (token.id != .NewLine)
        return pp.fail(lexer.source, "extra tokens at end of macro directive", token.loc);
}

fn expandBoolExpr(pp: *Preprocessor, lexer: *Lexer) Error!bool {
    const token = lexer.next();
    return fail(pp, lexer.source, "TODO marco bool condition", token.loc);
}

/// handle one level of #if ... #endif
fn expandConditional(pp: *Preprocessor, lexer: *Lexer, firstCond: bool) Error!void {
    var cond = firstCond;
    while (true) {
        if (cond) {
            const directive = try pp.preprocessInternal(lexer, .untilElse);
            switch (directive) {
                .KeywordEndif => return pp.expectNewLine(lexer, true),
                .KeywordElse => {},
                .KeywordElIf => {
                    cond = try pp.expandBoolExpr(lexer);
                    continue;
                },

                else => unreachable,
            }
        }

        const directive = try pp.skip(lexer);
        switch (directive) {
            .KeywordEndif => return pp.expectNewLine(lexer, true),
            .KeywordElse => {
                _ = try pp.preprocessInternal(lexer, .untilEndIf);
                return;
            },
            .KeywordElIf => {
                cond = try pp.expandBoolExpr(lexer);
            },
            else => unreachable,
        }
    }
}

fn skip(pp: *Preprocessor, lexer: *Lexer) Error!TokenType {
    var ifsSeen: u32 = 0;
    var lineStart = true;

    while (lexer.index < lexer.buffer.len) : (lexer.index += 1) {
        if (lineStart) {
            lineStart = false;

            var hash = lexer.next();
            while (hash.id == .NewLine)
                hash = lexer.next();

            if (hash.id != .Hash)
                continue;

            const directive = lexer.next();
            switch (directive.id) {
                .KeywordElse, .KeywordElIf => return directive.id,
                .KeywordEndif => {
                    if (ifsSeen == 0) return directive.id;
                    ifsSeen -= 1;
                },
                .KeywordIf, .KeywordIfdef, .KeywordIfndef => ifsSeen += 1,
                else => {},
            }
        } else if (lexer.buffer[lexer.index] == '\n') {
            lineStart = true;
        } else {
            lineStart = false;
        }
    } else {
        return pp.fail(lexer.source, "unterminated conditional directive", .{ .start = lexer.index - 1, .end = lexer.index });
    }
}

fn expandMacro(pp: *Preprocessor, lexer: *Lexer, name: Token, tokens: *TokenList) Error!void {
    if (pp.defines.get(lexer.source.slice(name.loc))) |some| switch (some) {
        .empty => {},
        .simple => |macroTokens| try tokens.appendSlice(macroTokens),
        .func => return pp.fail(lexer.source, "TODO func macro expansion", name.loc),
    } else {
        try tokens.append(name);
    }
}

/// Handle #define directive
fn define(pp: *Preprocessor, lexer: *Lexer) Error!void {
    var macroName = lexer.next();
    if (!macroName.id.isMacroIdentifier())
        return pp.fail(lexer.source, "macro name must be an identifier", macroName.loc);

    const nameStr = lexer.source.slice(macroName.loc);
    const first = lexer.next();

    if (first.id == .NewLine or first.id == .Eof) {
        _ = try pp.defines.put(nameStr, .empty);
        return;
    } else if (first.loc.start == macroName.loc.end) {
        if (first.id == .LParen)
            return pp.defineFunc(lexer, macroName);

        return pp.fail(lexer.source, "ISO C99 requires whitespace after the macro name", first.loc);
    } else if (first.id == .HashHash) {
        return pp.fail(lexer.sourc, "'##' cannot appear at start of macro expansion", first.loc);
    }
}

/// Handle a function like #define directive
fn defineFunc(pp: *Preprocessor, lexer: *Lexer, macroName: Token) Error!void {
    var params = std.ArrayList([]const u8).init(pp.compilation.gpa);
    defer params.deinit();

    while (true) {
        var token = lexer.next();

        if (token.id == .RParen) break;
        if (token.id.isMacroIdentifier())
            try params.append(lexer.source.slice(token.loc))
        else
            return pp.fail(lexer.source, "invaild token in macro parameter list", token.loc);
    }

    var tokens = TokenList.init(pp.compilation.gpa);
    defer tokens.deinit();

    while (true) {
        var token = lexer.next();
        switch (token.id) {
            .NewLine, .Eof => break,
            else => try tokens.append(token),
        }
    }

    const paramList = try pp.arena.allocator.dupe([]const u8, params.items);
    const tokenList = try pp.arena.allocator.dupe(Token, tokens.items);
    const nameStr = lexer.source.slice(macroName.loc);
    _ = try pp.defines.put(nameStr, .{ .func = .{ .params = paramList, .tokens = tokenList } });
}