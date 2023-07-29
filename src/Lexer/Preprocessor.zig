const std = @import("std");
const RawToken = @import("../Lexer/Token.zig").Token;
const TokenType = @import("../Basic/TokenType.zig").TokenType;
const Compilation = @import("../Basic/Compilation.zig");
const Source = @import("../Basic/Source.zig");
const Lexer = @import("Lexer.zig");
const Parser = @import("../Parser/Parser.zig");
const Diagnostics = @import("../Basic/Diagnostics.zig");
const Token = @import("../AST/AST.zig").Token;

const Allocator = std.mem.Allocator;
const assert = std.debug.assert;

const Preprocessor = @This();
const DefineMap = std.StringHashMap(Macro);
const RawTokenList = std.ArrayList(RawToken);
const MaxIncludeDepth = 200;

const Error = Compilation.Error;

const Macro = union(enum) {
    /// #define Foo
    empty,

    /// #define Foo Foo
    self,

    /// #define Add a + b
    simple: struct {
        tokens: []const RawToken,
        loc: Source.Location,
    },

    /// #define Add(a, b) ((a)+(b))
    func: Func,

    const Func = struct {
        /// Parameters of the function type macro
        params: []const []const u8,

        /// Token constituting the macro body
        tokens: []const RawToken,

        varArgs: bool,
        loc: Source.Location,
    };
};

compilation: *Compilation,
arena: std.heap.ArenaAllocator,
defines: DefineMap,
tokens: Token.List = .{},
generated: std.ArrayList(u8),
pragmaOnce: std.AutoHashMap(Source.ID, void),
tokenBuffer: RawTokenList,
charBuffer: std.ArrayList(u8),

// It is safe to have pointers to entries of defines since it
// cannot be modified while we are expanding a macro.
expansionLog: std.AutoHashMap(DefineMap.Entry, void),
includeDepth: u8 = 0,

pub fn init(comp: *Compilation) Preprocessor {
    return .{
        .compilation = comp,
        .arena = std.heap.ArenaAllocator.init(comp.gpa),
        .defines = DefineMap.init(comp.gpa),
        .generated = std.ArrayList(u8).init(comp.gpa),
        .pragmaOnce = std.AutoHashMap(Source.ID, void).init(comp.gpa),
        .tokenBuffer = RawTokenList.init(comp.gpa),
        .charBuffer = std.ArrayList(u8).init(comp.gpa),
        .expansionLog = std.AutoHashMap(DefineMap.Entry, void).init(comp.gpa),
    };
}

pub fn deinit(pp: *Preprocessor) void {
    pp.defines.deinit();
    pp.tokens.deinit(pp.compilation.gpa);
    pp.arena.deinit();
    pp.generated.deinit();
    pp.pragmaOnce.deinit();
    pp.tokenBuffer.deinit();
    pp.expansionLog.deinit();
}

pub fn preprocess(pp: *Preprocessor, source: Source) Error!void {
    var lexer = Lexer{
        .buffer = source.buffer,
        .source = source.id,
    };

    // Estimate how many new tokens this source will contain.
    const estimatedTokenCount = source.buffer.len / 8;
    try pp.tokens.ensureTotalCapacity(pp.compilation.gpa, pp.tokens.len + estimatedTokenCount);

    var ifLevel: u8 = 0;
    var ifKind = std.packed_int_array.PackedIntArray(u2, 256).init([1]u2{0} ** 256);
    var seenPragmaOnce = false;
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
                        slice = std.mem.trim(u8, slice, "\t\x0B\x0C");

                        try pp.compilation.diag.add(.{
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
                    .KeywordPragma => {
                        const start = lexer.index;
                        while (lexer.index < lexer.buffer.len) : (lexer.index += 1) {
                            if (lexer.buffer[lexer.index] == '\n')
                                break;
                        }

                        var slice = lexer.buffer[start..lexer.index];
                        slice = std.mem.trim(u8, slice, "\t\x0B\x0C");

                        if (std.mem.eql(u8, slice, "once")) {
                            const prev = try pp.pragmaOnce.fetchPut(lexer.source, {});
                            if (prev != null and !seenPragmaOnce) {
                                return;
                            } else {
                                seenPragmaOnce = true;
                            }
                        } else {
                            try pp.compilation.diag.add(.{
                                .tag = .unsupported_pragma,
                                .loc = .{ .id = token.source, .byteOffset = token.start },
                                .extra = .{ .str = slice },
                            });
                        }
                    },
                    .KeywordUndef => {
                        const macro_name = (try pp.expectMacroName(&lexer)) orelse continue;
                        _ = pp.defines.remove(macro_name);
                        try pp.expectNewLine(&lexer);
                    },

                    .KeywordElIf => {
                        if (ifLevel == 0) {
                            try pp.err(directive, .else_without_if);
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
                                try pp.err(directive, .elif_after_else);
                                skipToNewLine(&lexer);
                            },
                            else => unreachable,
                        }
                    },

                    .KeywordElse => {
                        try pp.expectNewLine(&lexer);

                        if (ifLevel == 0) {
                            try pp.err(directive, .else_without_if);
                            continue;
                        }

                        switch (ifKind.get(ifLevel)) {
                            untilElse => ifKind.set(ifLevel, untilEndIfSeenElse),
                            untilEndIf => try pp.skip(&lexer, .untilEndIfSeenElse),
                            untilEndIfSeenElse => {
                                try pp.err(directive, .else_after_else);
                                skipToNewLine(&lexer);
                            },
                            else => unreachable,
                        }
                    },

                    .KeywordEndIf => {
                        try pp.expectNewLine(&lexer);
                        if (ifLevel == 0) {
                            try pp.err(directive, .else_without_if);
                            continue;
                        }

                        ifLevel -= 1;
                    },

                    .KeywordLine => {
                        const digits = lexer.next();
                        if (digits.id != .IntegerLiteral)
                            try pp.err(digits, .line_simple_digit);

                        if (digits.id == .Eof or digits.id == .NewLine)
                            continue;

                        const name = lexer.next();
                        if (name.id == .Eof or name.id == .NewLine)
                            continue;

                        if (name.id != .StringLiteral)
                            try pp.err(name, .line_invalid_filename);

                        try pp.expectNewLine(&lexer);
                    },

                    .NewLine => {},
                    .Eof => {
                        if (ifLevel != 0)
                            try pp.err(directive, .unterminated_conditional_directive);

                        return pp.tokens.append(pp.compilation.gpa, tokenFromRaw(directive));
                    },
                    else => {
                        try pp.err(token, .invalid_preprocessing_directive);
                        try pp.expectNewLine(&lexer);
                    },
                }
            },

            .NewLine => startOfLine = true,
            .Eof => {
                if (ifLevel != 0)
                    try pp.err(token, .unterminated_conditional_directive);

                return pp.tokens.append(pp.compilation.gpa, tokenFromRaw(token));
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

fn tokSliceSafe(pp: *Preprocessor, token: RawToken) []const u8 {
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

fn err(pp: *Preprocessor, raw: RawToken, tag: Diagnostics.Tag) !void {
    try pp.compilation.diag.add(.{
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
        try pp.err(macroName, .macro_name_missing);
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
            try pp.err(token, .extra_tokens_directive_end);
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
                try pp.err(token, .expected_value_in_expr);
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
                try pp.err(macroToken, .macro_name_missing);

            if (first.id == .LParen) {
                const rParen = lexer.next();
                if (rParen.id != .RParen) {
                    try pp.err(rParen, .closing_paren);
                    try pp.err(first, .to_match_paren);
                }
            }

            if (pp.defines.get(pp.tokSliceSafe(macroToken))) |_| {
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
                try pp.compilation.diag.add(.{
                    .tag = .string_literal_in_pp_expr,
                    .loc = pp.tokens.items(.loc)[i],
                });
                return false;
            },

            .FloatLiteral,
            .FloatLiteral_F,
            .FloatLiteral_L,
            => {
                try pp.compilation.diag.add(.{
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
        .currDeclList = undefined,
        .scopes = undefined,
        .data = undefined,
        .labels = undefined,
    };

    const res = parser.constExpr() catch |e| switch (e) {
        error.OutOfMemory => return error.OutOfMemory,
        error.FatalError => return error.FatalError,
        error.ParsingFailed => return false,
    };
    return res.getBool();
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
                        try pp.err(directive, .else_after_else);
                        continue;
                    }

                    lexer.index = dirStart;
                    return;
                },

                .KeywordElIf => {
                    if (ifsSeen != 0 or cont == .untilEndIf)
                        continue;

                    if (cont == .untilEndIfSeenElse) {
                        try pp.err(directive, .elif_after_else);
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
        return pp.err(eof, .unterminated_conditional_directive);
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

fn expandMacro(pp: *Preprocessor, lexer: *Lexer, raw: RawToken) Error!void {
    if (pp.defines.getEntry(pp.tokSliceSafe(raw))) |some| switch (some.value_ptr.*) {
        .empty => return,
        .self => {},
        .simple => {
            pp.expansionLog.clearRetainingCapacity();
            var buffer = ExpandBuffer.init(pp.compilation.gpa);
            defer buffer.deinit();

            // add the token to the buffer and expand it
            try buffer.append(tokenFromRaw(raw));
            var start: usize = 0;
            try pp.expandExtra(&buffer, &start);

            // Add the result tokens to the token list and mark that they were expanded.
            try pp.tokens.ensureTotalCapacity(pp.compilation.gpa, pp.tokens.len + buffer.items.len);
            const loc = Source.Location{ .id = raw.source, .byteOffset = raw.start };
            for (buffer.items) |*r| {
                try pp.markExpandedFrom(r, loc);
                pp.tokens.appendAssumeCapacity(r.*);
            }

            return;
        },

        .func => |macro| blk: {
            const start = lexer.index;
            const lp = lexer.next();

            if (lp.id != .LParen) {
                lexer.index = start;
                break :blk;
            }

            pp.expansionLog.clearRetainingCapacity();
            var buffer = ExpandBuffer.init(pp.compilation.gpa);
            defer buffer.deinit();

            // collect the macro name and arguments into a new buffer
            try buffer.append(tokenFromRaw(raw));
            try buffer.append(tokenFromRaw(lp));

            while (true) {
                const token = lexer.next();
                switch (token.id) {
                    .NewLine => continue,
                    .Eof => {
                        try pp.err(token, .unterminated_macro_arg_list);
                        return;
                    },
                    .RParen => {
                        try buffer.append(tokenFromRaw(token));
                        break;
                    },

                    else => try buffer.append(tokenFromRaw(token)),
                }
            }

            var startIdx: usize = 0;
            // Mark that we have seen this macro.
            try pp.expansionLog.putNoClobber(some, {});
            try pp.expandFunc(&buffer, &startIdx, macro);

            // add the result tokens to the token list and mark they were expanded,
            try pp.tokens.ensureTotalCapacity(pp.compilation.gpa, pp.tokens.len + buffer.items.len);
            const loc = Source.Location{ .id = raw.source, .byteOffset = raw.start };
            for (buffer.items) |*r| {
                try pp.markExpandedFrom(r, loc);
                pp.tokens.appendAssumeCapacity(r.*);
            }

            return;
        },
    };

    // Not a macro, continue as usual.
    try pp.tokens.append(pp.compilation.gpa, tokenFromRaw(raw));
}

// mark that this token has been expanded from `loc`
fn markExpandedFrom(pp: *Preprocessor, token: *Token, loc: Source.Location) !void {
    const newLoc = try pp.arena.allocator().create(Source.Location);
    newLoc.* = loc;
    newLoc.next = token.loc.next;
    token.loc.next = newLoc;
}

/// Try to expand a macro in the `source` buffer at `start_index`.
fn expandExtra(pp: *Preprocessor, source: *ExpandBuffer, start: *usize) Error!void {
    if (pp.defines.getEntry(pp.expandedSlice(source.items[start.*]))) |some| {
        if (pp.expansionLog.get(some)) |_| {
            // If we have already expanded this macro, do not recursively expand it.
            start.* += 1;
            return;
        }

        // Mark that we have seen this macro.
        try pp.expansionLog.putNoClobber(some, {});

        switch (some.value_ptr.*) {
            .empty => _ = source.orderedRemove(start.*), // Simply remove the token.
            .self => start.* += 1, // Just go over the token.
            .simple => |macro| {
                _ = source.orderedRemove(start.*);
                var buffer = ExpandBuffer.init(pp.compilation.gpa);
                defer buffer.deinit();
                try buffer.ensureTotalCapacity(macro.tokens.len);

                // add all of the macros tokens to the new buffer handing any concats
                var i: usize = 0;
                while (i < macro.tokens.len) : (i += 1) {
                    const raw = macro.tokens[i];
                    if (raw.id == .HashHash) {
                        _ = buffer.pop();
                        const lhs = tokenFromRaw(macro.tokens[i - 1]);
                        const rhs = tokenFromRaw(macro.tokens[i + 1]);
                        i += 1;

                        buffer.appendAssumeCapacity(try pp.pasteTokens(lhs, rhs));
                    } else {
                        buffer.appendAssumeCapacity(tokenFromRaw(raw));
                    }
                }

                // Try to expand the result tokens.
                i = 0;
                while (i < buffer.items.len) {
                    if (buffer.items[i].id.isMacroIdentifier()) {
                        try pp.expandExtra(&buffer, &i);
                    } else {
                        i += 1;
                    }
                }

                // Mark all the tokens before adding them to the source buffer.
                for (buffer.items) |*tok| try pp.markExpandedFrom(tok, macro.loc);
                try source.insertSlice(start.*, buffer.items);
                start.* += buffer.items.len;
            },

            .func => |macro| return pp.expandFunc(source, start, macro),
        }
    } else {
        start.* += 1;
    }
}

/// Try to expand a function like macro in the source buffer at start location.
fn expandFunc(pp: *Preprocessor, source: *ExpandBuffer, startIdx: *usize, macro: Macro.Func) Error!void {
    const nameTK = source.items[startIdx.*];
    const lparenIdx = startIdx.* + 1;
    if (source.items.len <= lparenIdx or source.items[lparenIdx].id != .LParen) {
        // Not a macro function call, go over normal identifier.
        startIdx.* += 1;
        return;
    }

    // collect the arguments.
    // `args_count` starts with 1 since whitespace counts as an argument.
    var argsCount: u32 = 1;
    const args = for (source.items[lparenIdx..], 0..) |tok, i| {
        switch (tok.id) {
            .Comma => argsCount += 1,
            .RParen => break source.items[lparenIdx + 1 .. i + 1],
            else => {},
        }
    } else {
        try pp.compilation.diag.add(.{ .tag = .unterminated_macro_arg_list, .loc = nameTK.loc });
        startIdx.* += 1;
        return;
    };

    // Validate argument count.
    const extra = Diagnostics.Message.Extra{ .arguments = .{ .expected = @as(u32, @intCast(macro.params.len)), .actual = argsCount } };
    if (macro.varArgs and argsCount < macro.params.len) {
        try pp.compilation.diag.add(.{ .tag = .expected_at_least_arguments, .loc = nameTK.loc, .extra = extra });
        startIdx.* += 1;
        return;
    } else if (argsCount != macro.params.len) {
        try pp.compilation.diag.add(.{ .tag = .expected_arguments, .loc = nameTK.loc, .extra = extra });
        startIdx.* += 1;
        return;
    }

    var buf = ExpandBuffer.init(pp.compilation.gpa);
    defer buf.deinit();
    try buf.ensureTotalCapacity(macro.tokens.len);

    // 1. Stringification and 2. Parameter replacement
    var tokenIdx: usize = 0;
    while (tokenIdx < macro.tokens.len) : (tokenIdx += 1) {
        const raw = macro.tokens[tokenIdx];
        switch (raw.id) {
            .StringifyParam => {
                const targetArg = argSlice(args, raw.end);
                pp.charBuffer.items.len = 0; // Safe since we can only be stringifying one parameter at a time.

                // TODO pretty print these
                try pp.charBuffer.append('"');
                for (targetArg, 0..) |a, i| {
                    if (i != 0) try pp.charBuffer.append(' ');
                    for (pp.expandedSlice(a)) |c| {
                        if (c == '"')
                            try pp.charBuffer.appendSlice("\\\"")
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

            .MacroParam => {
                const targetArg = argSlice(args, raw.end);
                // TODO mark originating from params
                if (targetArg.len == 0)
                    // This is needed so that we can properly do token pasting.
                    try buf.append(.{ .id = .EmptyArg, .loc = .{ .id = raw.source, .byteOffset = raw.start } })
                else
                    try buf.appendSlice(targetArg);
            },
            else => try buf.append(tokenFromRaw(raw)),
        }
    }

    // 3. Concatenation
    tokenIdx = 0;
    while (tokenIdx < buf.items.len) : (tokenIdx += 1) {
        switch (buf.items[tokenIdx].id) {
            .HashHash => {
                // TODO ## originating from args should not be concatenated
                const prev = buf.items[tokenIdx - 1];
                const next = buf.items[tokenIdx + 1];

                buf.items[tokenIdx - 1] = try pp.pasteTokens(prev, next);
                std.mem.copyBackwards(Token, buf.items[tokenIdx..], buf.items[tokenIdx + 2 ..]);
                buf.items.len -= 2;
                tokenIdx -= 1;
            },
            else => {},
        }
    }

    // TODO
    // 4. Expand tokens from parameters
    // tok_i = 0;
    // while (tok_i < expanded.items.len) : (tok_i += 1) {
    // }

    // 5. Expand resulting tokens
    tokenIdx = 0;
    while (tokenIdx < buf.items.len) {
        if (buf.items[tokenIdx].id == .EmptyArg) {
            _ = buf.orderedRemove(tokenIdx);
            continue;
        }
        if (buf.items[tokenIdx].id.isMacroIdentifier()) {
            try pp.expandExtra(&buf, &tokenIdx);
        } else {
            tokenIdx += 1;
        }
    }
    // Mark all the tokens before adding them to the source buffer.
    for (buf.items) |*tok|
        try pp.markExpandedFrom(tok, macro.loc);

    // Move tokens after the call out of the way.
    const inputLen = args.len + 3; // +3 for identifier, ( and )
    if (inputLen >= buf.items.len) {
        std.mem.copy(Token, source.items[startIdx.* + buf.items.len ..], source.items[startIdx.* + inputLen ..]);
        source.items.len -= inputLen - buf.items.len;
    } else {
        try source.ensureTotalCapacity(source.items.len + buf.items.len - inputLen);
        const start_len = source.items.len;
        source.items.len = source.capacity;
        std.mem.copyBackwards(Token, source.items[startIdx.* + buf.items.len ..], source.items[startIdx.* + inputLen .. start_len]);
    }
    // Insert resulting tokens to the source
    std.mem.copy(Token, source.items[startIdx.*..], buf.items);
    startIdx.* += buf.items.len;
}

// get argument at index from a list of tokens.
fn argSlice(args: []const Token, index: u32) []const Token {
    // TODO this is a mess
    var commasSeen: usize = 0;
    var i: usize = 0;
    while (i < args.len) : (i += 1) {
        if (args[i].id == .Comma) {
            if (index == 0) return args[0..i];
            commasSeen += 1;
            continue;
        }
        if (commasSeen == index) for (args[i..], 0..) |a_2, j| {
            if (a_2.id == .Comma) {
                return args[i..][0..j];
            }
        } else return args[i..];
    } else return args[i..];
    unreachable;
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
        .index = @as(u32, @intCast(start)),
        .source = .generated,
    };

    const pastedToken = lexer.next();
    const next = lexer.next().id;

    if (next != .NewLine and next != .Eof) {
        try pp.compilation.diag.add(.{
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

/// Handle #define directive
fn define(pp: *Preprocessor, lexer: *Lexer) Error!void {
    // get the macro name and validate.
    const macroName = lexer.next();
    if (macroName.id == .KeywordDefined) {
        try pp.err(macroName, .defined_as_macro_name);
        return skipToNewLine(lexer);
    }

    if (!macroName.id.isMacroIdentifier()) {
        try pp.err(macroName, .macro_name_must_be_identifier);
        return skipToNewLine(lexer);
    }

    const nameStr = pp.tokSliceSafe(macroName);
    var first = lexer.next();
    first.id.simplifyMacroKeyword();

    if (first.id == .NewLine or first.id == .Eof) {
        _ = try pp.defines.put(nameStr, .empty);
        return;
    } else if (first.start == macroName.end) {
        if (first.id == .LParen)
            return pp.defineFunc(lexer, macroName, first);

        try pp.err(first, .whitespace_after_macro_name);
    } else if (first.id == .HashHash) {
        try pp.err(first, .hash_hash_at_start);
    }

    {
        const start = lexer.index;
        const second = lexer.next();
        if (second.id == .NewLine or second.id == .Eof) {
            if (std.mem.eql(u8, pp.tokSliceSafe(first), nameStr)) {
                _ = try pp.defines.put(nameStr, .self);
                return;
            }
        }

        lexer.index = start;
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
                    try pp.err(token, .hash_hash_at_end);
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
    _ = try pp.defines.put(nameStr, .{ .simple = .{
        .loc = .{ .id = macroName.source, .byteOffset = macroName.start },
        .tokens = list,
    } });
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
                try pp.err(token, .invalid_token_param_list)
            else
                token = lexer.next();
        }

        if (token.id == .Eof)
            return pp.err(token, .unterminated_macro_param_list);

        if (token.id == .Ellipsis) {
            varArgs = true;
            const rParen = lexer.next();
            if (rParen.id != .RParen) {
                try pp.err(rParen, .missing_paren_param_list);
                try pp.err(lParen, .to_match_paren);
            }

            break;
        }

        if (!token.id.isMacroIdentifier()) {
            try pp.err(token, .invalid_token_param_list);
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

                try pp.err(param, .hash_not_followed_param);
                token = param;
            },

            .HashHash => {
                const start = lexer.index;
                const next = lexer.next();

                if (next.id == .NewLine or next.id == .Eof) {
                    try pp.err(token, .hash_hash_at_end);
                    continue;
                }

                lexer.index = start;
                try pp.tokenBuffer.append(token);
            },

            else => {
                if (token.id.isMacroIdentifier()) {
                    token.id.simplifyMacroKeyword();
                    const s = pp.tokSliceSafe(token);
                    for (params.items, 0..) |param, i| {
                        if (std.mem.eql(u8, param, s)) {
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
    const nameStr = pp.tokSliceSafe(macroName);
    _ = try pp.defines.put(nameStr, .{ .func = .{
        .params = paramList,
        .varArgs = varArgs,
        .tokens = tokenList,
        .loc = .{ .id = macroName.source, .byteOffset = macroName.start },
    } });
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
    pp.tokens.len -= 1; // remove eof
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

        try pp.compilation.diag.add(.{ .tag = .header_str_closing, .loc = .{ .id = first.source, .byteOffset = first.start } });
        try pp.err(first, .header_str_match);
    }

    // Try expand if the argument is a macro
    try pp.expandMacro(lexer, first);

    // check that we actually got a string
    const fileNameTK = pp.tokens.get(start);
    switch (fileNameTK.id) {
        .StringLiteral, .MacroString => {},
        else => {
            try pp.err(first, .expected_filename);
            try pp.expectNewLine(lexer);

            return error.InvalidInclude;
        },
    }

    // error on the extra tokens.
    const newLine = lexer.next();
    if ((newLine.id != .NewLine and newLine.id != .Eof) or pp.tokens.len > start + 1) {
        skipToNewLine(lexer);
        try pp.err(first, .extra_tokens_directive_end);
    }

    // check for empty filename
    const tkSlice = pp.expandedSlice(fileNameTK);
    if (tkSlice.len < 3) {
        try pp.err(first, .empty_filename);
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
            const source = pp.compilation.getSource(cur.loc.id).buffer;
            const curEnd = cur.loc.byteOffset + slice.len;
            try pp.printInBetween(source[curEnd..next.loc.byteOffset], w);
        } else {
            // next was included from another file
            try w.writeByte('\n');
        }
        cur = next;
    }
}

fn printInBetween(pp: *Preprocessor, slice: []const u8, w: anytype) !void {
    _ = pp;
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

fn expectTokens(buffer: []const u8, expectedTokens: []const TokenType) void {
    var comp = Compilation.init(std.testing.allocator);
    defer comp.deinit();

    const source = Source{
        .buffer = buffer,
        .id = @enumFromInt(2),
        .path = "<test-buffer>",
    };

    comp.sources.putNoClobber(source.path, source) catch unreachable;
    defer comp.sources.clearAndFree();

    var pp = Preprocessor.init(&comp);
    defer pp.deinit();

    pp.preprocess(source) catch unreachable;
    std.testing.expect(comp.renderErrors() == 0) catch std.debug.print("nothing TODO", .{});

    for (expectedTokens, 0..) |expectedTokenId, i| {
        const actual = pp.tokens.items(.id)[i];
        if (!std.meta.eql(actual, expectedTokenId)) {
            std.debug.panic("expected {s}, found {s}\n", .{ @tagName(expectedTokenId), @tagName(actual) });
        }
    }
    const lastToken = pp.tokens.items(.id)[expectedTokens.len];
    std.testing.expect(lastToken == .Eof) catch std.debug.print("", .{});
}

fn expectStr(buffer: []const u8, expected: []const u8) void {
    var comp = Compilation.init(std.testing.allocator);
    defer comp.deinit();

    const source = Source{
        .buffer = buffer,
        .id = @as(Source.ID, @enumFromInt(2)),
        .path = "<test-buf>",
    };

    comp.sources.putNoClobber(source.path, source) catch unreachable;
    defer comp.sources.clearAndFree();

    var pp = Preprocessor.init(&comp);
    defer pp.deinit();

    pp.preprocess(source) catch unreachable;
    std.testing.expect(comp.renderErrors() == 0) catch std.debug.print("nothing TODO", .{});

    var actual = std.ArrayList(u8).init(std.testing.allocator);
    defer actual.deinit();

    for (pp.tokens.items(.id), 0..) |id, i| {
        if (id == .Eof) break;

        if (i != 0) actual.append(' ') catch unreachable;

        actual.appendSlice(pp.expandedSlice(pp.tokens.get(i))) catch unreachable;
    }

    std.testing.expectEqualStrings(expected, actual.items) catch std.debug.print("nothing TODO", .{});
}

test "ifdef" {
    expectTokens(
        \\#define FOO
        \\#ifdef FOO
        \\long
        \\#else
        \\int
        \\#endif
    , &.{.KeywordLong});

    expectTokens(
        \\#define BAR
        \\#ifdef FOO
        \\long
        \\#else
        \\int
        \\#endif
    , &.{.KeywordInt});
}

test "define undefine" {
    expectTokens(
        \\#define FOO 1
        \\#undef FOO
    , &.{});
}

test "recursive object macro" {
    expectStr(
        \\#define y x
        \\#define x y
        \\x
    , "x");

    expectStr(
        \\#define x x
        \\x
    , "x");
}

test "object macro expansion" {
    expectTokens(
        \\#define x a
        \\x
        \\#define a 1
        \\x
    , &.{ .Identifier, .IntegerLiteral });
    expectTokens(
        \\#define x define
        \\x
    , &.{.Identifier});
}

test "object macro token pasting" {
    expectStr(
        \\#define x a##1
        \\x
        \\#define a 1
        \\x
    , "a1 a1");
}

test "#if constant expression" {
    expectStr(
        \\#if defined FOO
        \\void
        \\#elif !defined(BAR)
        \\long
        \\#endif
    , "long");
}

test "function macro expansion" {
    expectStr(
        \\#define HE HI
        \\#define LLO _THERE
        \\#define HELLO "HI THERE"
        \\#define CAT(a,b) a##b
        \\#define XCAT(a,b) CAT(a,b)
        \\#define CALL(fn) fn(HE,LLO)
        \\CAT(HE, LLO)
        \\//XCAT(HE, LLO) TODO HI_THERE
        \\CALL(CAT)
    , "\"HI THERE\" \"HI THERE\"");
}
