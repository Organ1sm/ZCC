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

const Marco = union(enum) {
    empty,
    func: struct {
        args: []const []const u8,
        tokens: []const Token,
    },
    simple: []const Token,
};

compilation: *Compilation,
arena: std.heap.ArenaAllocator,
defines: DefineMap,
tokens: std.ArrayList(Token),

pub fn init(comp: *Compilation) Preprocessor {
    return .{
        .comp = comp,
        .arena = std.heap.ArenaAllocator.init(comp.gpa),
        .defines = DefineMap.init(comp.gpa),
        .tokens = std.ArrayList(Token).init(comp.gpa),
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
                    .KeywordError => return pp.fail(lexer.source, "TODO error directive", directive.loc),
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

                    .KeywordDefine => return pp.fail(lexer.source, "TODO define directive", directive.loc),
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
                startOfLine = true;
            },

            .NewLine => startOfLine = true,
            .Eof => {
                if (cont != .untilEof) return pp.fail(lexer.source, "unterminated conditional directive", token.loc);
                try pp.tokens.append(token);
                return token.id;
            },

            else => {
                startOfLine = false;
                if (token.id.isIdentifier()) {
                    if (pp.defines.get(lexer.source.slice(token.loc))) |some| switch (some) {
                        .empty => continue,
                        .simple => |toks| {
                            try pp.tokens.appendSlice(toks);
                            continue;
                        },
                        .func => pp.fail(lexer.source, "TODO func macro expansion.", token.loc),
                    };

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
    const macroName = lexer.next();
    if (!macroName.id.isIdentifier())
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
