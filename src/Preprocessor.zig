const std = @import("std");
const Token = @import("Token.zig").Token;
const TokenType = @import("TokenType.zig").TokenType;
const Compilation = @import("Compilation.zig");
const Source = @import("Source.zig");
const Lexer = @import("Lexer.zig");
const Parser = @import("Parser.zig");

const Allocator = std.mem.Allocator;
const assert = std.debug.assert;

const Preprocessor = @This();
const DefineMap = std.StringHashMap(Marco);
const TokenList = std.ArrayList(Token);

const Error = Allocator.Error || error{PreprocessingFailed};

const Marco = union(enum) {
    /// #define Foo
    empty,

    /// #define Foo Foo
    self,

    /// #define Add a + b
    simple: []const Token,

    /// #define Add(a, b) ((a)+(b))
    func: struct {
        /// Parameters of the function type macro
        params: []const []const u8,

        /// Token constituting the macro body
        tokens: []const Token,

        varArgs: bool,
    },
};

compilation: *Compilation,
arena: std.heap.ArenaAllocator,
defines: DefineMap,
tokens: TokenList,
generated: std.ArrayList(u8),
pragmaOnce: std.AutoHashMap(Source.ID, void),
tokenBuffer: TokenList,

pub fn init(comp: *Compilation) Preprocessor {
    return .{
        .compilation = comp,
        .arena = std.heap.ArenaAllocator.init(comp.gpa),
        .defines = DefineMap.init(comp.gpa),
        .tokens = TokenList.init(comp.gpa),
        .generated = std.ArrayList(u8).init(comp.gpa),
        .pragmaOnce = std.AutoHashMap(Source.ID, void).init(comp.gpa),
        .tokenBuffer = TokenList.init(comp.gpa),
    };
}

pub fn deinit(pp: *Preprocessor) void {
    pp.defines.deinit();
    pp.tokens.deinit();
    pp.arena.deinit();
    pp.generated.deinit();
    pp.pragmaOnce.deinit();
    pp.tokenBuffer.deinit();
}

pub fn preprocess(pp: *Preprocessor, source: Source) !void {
    var lexer = Lexer{
        .buffer = source.buffer,
        .source = source.id,
    };

    // Estimate how many new tokens this source will contain.
    const estimatedTokenCount = source.buffer.len / 8;
    try pp.tokens.ensureTotalCapacity(pp.tokens.items.len + estimatedTokenCount);

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

                        return pp.fail(slice, directive);
                    },

                    .KeywordIf => {
                        const ov = @addWithOverflow(ifLevel, 1);
                        if (ov[1] != 0)
                            return pp.fail("too many #if nestings", directive);

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
                            return pp.fail("too many #ifdef nestings", directive);

                        ifLevel = ov[0];

                        const macroName = try pp.expectMacroName(&lexer);
                        try pp.expectNewLine(&lexer, false);

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
                            return pp.fail("too many #ifndef nestings", directive);

                        ifLevel = ov[0];

                        const macroName = try pp.expectMacroName(&lexer);
                        try pp.expectNewLine(&lexer, false);

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
                            // todo:
                        }
                    },
                    .KeywordUndef => {
                        const macro_name = try pp.expectMacroName(&lexer);
                        _ = pp.defines.remove(macro_name);
                        try pp.expectNewLine(&lexer, true);
                    },

                    .KeywordElIf => {
                        if (ifLevel == 0)
                            return pp.fail("#elif without #if", directive);

                        switch (ifKind.get(ifLevel)) {
                            untilElse => if (try pp.expr(&lexer)) {
                                ifKind.set(ifLevel, untilEndIf);
                            } else {
                                try pp.skip(&lexer, .untilElse);
                            },

                            untilEndIf => try pp.skip(&lexer, .untilEndIf),
                            untilEndIfSeenElse => return pp.fail("#elif after #else", directive),
                            else => unreachable,
                        }
                    },

                    .KeywordElse => {
                        if (ifLevel == 0)
                            return pp.fail("#else without #if", directive);

                        try pp.expectNewLine(&lexer, false);

                        switch (ifKind.get(ifLevel)) {
                            untilElse => ifKind.set(ifLevel, untilEndIfSeenElse),
                            untilEndIf => try pp.skip(&lexer, .untilEndIfSeenElse),
                            untilEndIfSeenElse => return pp.fail("#else after #else", directive),
                            else => unreachable,
                        }
                    },

                    .KeywordEndIf => {
                        if (ifLevel == 0)
                            return pp.fail("#else without #if", directive);

                        try pp.expectNewLine(&lexer, true);
                        ifLevel -= 1;
                    },

                    .KeywordLine => {
                        const digits = lexer.next();
                        if (digits.id != .IntegerLiteral)
                            return pp.fail("#line directive requires a simple digit sequence", digits);

                        const name = lexer.next();
                        if (name.id == .Eof or name.id == .NewLine)
                            continue;

                        if (name.id != .StringLiteral)
                            return pp.fail("invalid filename for #line directive", name);

                        try pp.expectNewLine(&lexer, true);
                    },

                    .NewLine => {},
                    .Eof => {
                        if (ifLevel != 0)
                            return pp.fail("unterminated conditional directive", directive);

                        try pp.tokens.append(directive);
                        return;
                    },
                    else => return pp.fail("invalid preprocessing directive", directive),
                }
            },

            .NewLine => startOfLine = true,
            .Eof => {
                if (ifLevel != 0)
                    return pp.fail("unterminated conditional directive", token);

                try pp.tokens.append(token);
                return;
            },

            else => {
                startOfLine = false;
                if (token.id.isMacroIdentifier()) {
                    token.id.simplifyMacroKeyword();
                    try pp.expandMacro(&lexer, token, &pp.tokens);
                } else {
                    try pp.tokens.append(token);
                }
            },
        }
    }
}

fn tokSliceSafe(pp: *Preprocessor, token: Token) []const u8 {
    if (token.id.lexeMe()) |some| return some;

    assert(!token.source.isGenerated());

    const sourceBuf = pp.compilation.sources.values()[token.source.index()].buffer;
    return sourceBuf[token.loc.start..token.loc.end];
}

// Returned slice is invalidated when generated is updated.
pub fn tokSlice(pp: *Preprocessor, token: Token) []const u8 {
    if (token.id.lexeMe()) |some| return some;
    if (token.source.isGenerated()) {
        return pp.generated.items[token.loc.start..token.loc.end];
    } else {
        const source = pp.compilation.getSource(token.source);
        return source.buffer[token.loc.start..token.loc.end];
    }
}

fn failFmt(pp: *Preprocessor, token: Token, comptime fmt: []const u8, args: anytype) Error {
    const source = pp.compilation.getSource(token.source);
    const lcs = source.lineColString(token.loc);

    pp.compilation.printErrStart(source.path, lcs);
    std.debug.print(fmt, args);
    pp.compilation.printErrEnd(lcs);

    return error.PreprocessingFailed;
}

fn fail(pp: *Preprocessor, msg: []const u8, token: Token) Error {
    return pp.failFmt(token, "{s}", .{msg});
}

fn expectMacroName(pp: *Preprocessor, lexer: *Lexer) Error![]const u8 {
    const macroName = lexer.next();
    if (!macroName.id.isMacroIdentifier())
        return pp.fail("macro name missing", macroName);

    return pp.tokSliceSafe(macroName);
}

fn expectNewLine(pp: *Preprocessor, lexer: *Lexer, allowEof: bool) Error!void {
    const token = lexer.next();

    if (token.id == .Eof) {
        if (!allowEof)
            return pp.fail("unterminated conditional directive", token);

        return;
    }

    if (token.id != .NewLine)
        return pp.fail("extra tokens at end of macro directive", token);
}

fn expr(pp: *Preprocessor, lexer: *Lexer) Error!bool {
    pp.tokenBuffer.items.len = 0;

    while (true) {
        var token = lexer.next();
        if (token.id == .NewLine or token.id == .Eof) {
            if (pp.tokenBuffer.items.len == 0)
                return pp.fail("expected value in expression", token);

            token.id = .Eof;
            try pp.tokenBuffer.append(token);

            break;
        } else if (token.id == .KeywordDefined) {
            const first = lexer.next();
            const macroToken = if (first.id == .LParen) lexer.next() else first;

            if (!macroToken.id.isMacroIdentifier())
                return pp.fail("macro name missing", macroToken);

            if (first.id == .LParen) {
                const rParen = lexer.next();
                if (rParen.id != .RParen)
                    return pp.fail("expected closing ')'", rParen);
            }

            if (pp.defines.get(pp.tokSliceSafe(macroToken))) |_| {
                token.id = .One;
            } else {
                token.id = .Zero;
            }
        } else if (token.id.isMacroIdentifier()) {
            try pp.expandMacro(lexer, token, &pp.tokenBuffer);
            continue;
        }

        try pp.tokenBuffer.append(token);
    }

    for (pp.tokenBuffer.items) |*tok| {
        switch (tok.id) {
            .StringLiteral,
            .StringLiteralUTF_8,
            .StringLiteralUTF_16,
            .StringLiteralUTF_32,
            .StringLiteralWide,
            => return pp.fail("string literal in preprocessor expression", tok.*),

            .FloatLiteral,
            .FloatLiteral_F,
            .FloatLiteral_L,
            => return pp.fail("floating point literal in preprocessor expression", tok.*),
            else => if (tok.id.isMacroIdentifier()) {
                tok.id = .Zero;
            },
        }
    }

    var parser = Parser{
        .pp = pp,
        .tokens = pp.tokenBuffer.items,
    };

    const res = parser.constExpr() catch |err| switch (err) {
        error.OutOfMemory => unreachable, // TODO verify this
        error.ParsingFailed => return error.PreprocessingFailed,
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

                    if (cont == .untilEndIfSeenElse)
                        return pp.fail("#else after #else", directive);

                    lexer.index = dirStart;
                    return;
                },

                .KeywordElIf => {
                    if (ifsSeen != 0 or cont == .untilEndIf)
                        continue;

                    if (cont == .untilEndIfSeenElse)
                        return pp.fail("#elif after #else", directive);

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
        return pp.fail("unterminated conditional directive", eof);
    }
}

fn expandMacro(pp: *Preprocessor, lexer: *Lexer, token: Token, tokens: *TokenList) Error!void {
    const name = pp.tokSliceSafe(token);
    return pp.expandExtra(lexer, name, token, tokens);
}

fn expandExtra(pp: *Preprocessor, lexer: *Lexer, origName: []const u8, arg: Token, tokens: *TokenList) Error!void {
    if (pp.defines.get(pp.tokSlice(arg))) |some| switch (some) {
        .empty => {},
        .self => try tokens.append(arg),
        .simple => |macroTokens| {
            for (macroTokens) |token| {
                if (token.id.isMacroIdentifier() and !std.mem.eql(u8, pp.tokSlice(token), origName))
                    try pp.expandExtra(lexer, origName, token, tokens)
                else
                    try tokens.append(token);
            }
        },

        .func => return pp.fail("TODO func macro expansion", arg),
    } else {
        try tokens.append(arg);
    }
}

/// Handle #define directive
fn define(pp: *Preprocessor, lexer: *Lexer) Error!void {
    const macroName = lexer.next();
    if (macroName.id == .KeywordDefined)
        return pp.fail("'defined' cannot be used as macro name", macroName);

    if (!macroName.id.isMacroIdentifier())
        return pp.fail("macro name must be an identifier", macroName);

    const nameStr = pp.tokSliceSafe(macroName);
    var first = lexer.next();
    first.id.simplifyMacroKeyword();

    if (first.id == .NewLine or first.id == .Eof) {
        _ = try pp.defines.put(nameStr, .empty);
        return;
    } else if (first.loc.start == macroName.loc.end) {
        if (first.id == .LParen)
            return pp.defineFunc(lexer, macroName);

        return pp.fail("ISO C99 requires whitespace after the macro name", first);
    } else if (first.id == .HashHash) {
        return pp.fail("'##' cannot appear at start of macro expansion", first);
    }

    {
        const start = lexer.index;
        const second = lexer.next();
        if (second.id == .NewLine or second.id == .Eof) {
            if (std.mem.eql(u8, pp.tokSlice(first), nameStr)) {
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
                const prev = pp.tokenBuffer.pop();
                const next = lexer.next();
                if (next.id == .NewLine or next.id == .Eof)
                    return pp.fail("'##' cannot appear at end of macro expansion", next);

                const nextSlice = pp.tokSliceSafe(next);

                const start = pp.generated.items.len;
                const end = pp.tokSlice(prev).len + nextSlice.len;
                try pp.generated.ensureTotalCapacity(end);

                pp.generated.appendSliceAssumeCapacity(pp.tokSlice(prev));
                pp.generated.appendSliceAssumeCapacity(nextSlice);

                var tempSource = lexer.source;
                tempSource.markGenerated();
                var tempLexer = Lexer{ .buffer = pp.generated.items, .index = @as(u32, @intCast(start)), .source = tempSource };
                const pastedToken = tempLexer.next();

                if (tempLexer.next().id != .Eof) {
                    return pp.failFmt(token, "pasting formed '{s}, an invalid preprocessing token'", .{pp.generated.items[start..end]});
                }

                try pp.tokenBuffer.append(pastedToken);
            },
            .NewLine, .Eof => break,
            else => try pp.tokenBuffer.append(token),
        }
    }

    const list = try pp.arena.allocator().dupe(Token, pp.tokenBuffer.items);
    _ = try pp.defines.put(nameStr, .{ .simple = list });
}

/// Handle a function like #define directive
fn defineFunc(pp: *Preprocessor, lexer: *Lexer, macroName: Token) Error!void {
    var params = std.ArrayList([]const u8).init(pp.compilation.gpa);
    defer params.deinit();

    var varArgs = false;
    while (true) {
        const token = lexer.next();

        if (token.id == .RParen) break;
        if (token.id == .Ellipsis) {
            varArgs = true;
            const rParen = lexer.next();
            if (rParen.id != .RParen)
                return pp.fail("missing ')' in macro parameter list", rParen);

            break;
        }

        if (!token.id.isMacroIdentifier())
            return pp.fail("invaild token in macro parameter list", token);

        try params.append(pp.tokSliceSafe(token));

        const next = lexer.next();
        if (next.id == .RParen)
            break;

        if (next.id != .Comma) // TODO should not allow name (a,)
            return pp.fail("invaild token in macro parameter list", next);
    }

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
                            token.loc.end = @intCast(i);
                            try pp.tokenBuffer.append(token);

                            continue :tokenLoop;
                        }
                    }
                }

                return pp.fail("'#' is not followed by a macro parameter", param);
            },
            .HashHash => {
                const start = lexer.index;
                const next = lexer.next();

                if (next.id == .NewLine or next.id != .Eof)
                    return pp.fail("## cannot appear at start of macro expansion", token);

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
                            token.loc.end = @intCast(i);
                            break;
                        }
                    }
                }

                try pp.tokenBuffer.append(token);
            },
        }
    }

    const paramList = try pp.arena.allocator().dupe([]const u8, params.items);
    const tokenList = try pp.arena.allocator().dupe(Token, pp.tokenBuffer.items);
    const nameStr = pp.tokSliceSafe(macroName);
    _ = try pp.defines.put(nameStr, .{ .func = .{ .params = paramList, .varArgs = varArgs, .tokens = tokenList } });
}

fn include(pp: *Preprocessor, lexer: *Lexer) Error!void {
    pp.tokenBuffer.items.len = 0;

    var first = lexer.next();
    if (first.id == .AngleBracketLeft) to_end: {
        while (lexer.index < lexer.buffer.len) : (lexer.index += 1) {
            switch (lexer.buffer[lexer.index]) {
                '>' => {
                    lexer.index += 1;
                    first.loc.end = lexer.index;
                    first.id = .MacroString;
                    break :to_end;
                },
                '\n' => break,
                else => {},
            }
        }

        return pp.fail("expected '>'", first);
    }

    try pp.expandMacro(lexer, first, &pp.tokenBuffer);

    const fileNameTK = if (pp.tokenBuffer.items.len == 0) first else pp.tokenBuffer.items[0];
    switch (fileNameTK.id) {
        .StringLiteral, .MacroString => {},
        else => return pp.fail("expected \"FILENAME\" or <FILENAME>", first),
    }

    const newLine = lexer.next();
    if ((newLine.id != .NewLine and newLine.id != .Eof) or pp.tokenBuffer.items.len > 1)
        return pp.fail("extra tokens at end of macro directive", first);

    const tokenSlice = pp.tokSlice(fileNameTK);
    if (tokenSlice.len < 3)
        return pp.fail("empty  filename", first);

    const filename = tokenSlice[1 .. tokenSlice.len - 1];
    const newSource = pp.compilation.findInclude(filename, fileNameTK.id == .StringLiteral) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        else => return pp.failFmt(first, "'{s}' not found", .{filename}),
    };

    return pp.preprocess(newSource);
}

fn expectTokens(buffer: []const u8, expectedTokens: []const TokenType) void {
    var comp = Compilation.init(std.testing.allocator);
    defer comp.deinit();

    const source = Source{
        .buffer = buffer,
        .id = @enumFromInt(0),
        .path = "<test-buffer>",
    };

    comp.sources.putNoClobber(source.path, source) catch unreachable;
    defer comp.sources.clearAndFree();

    var pp = Preprocessor.init(&comp);
    defer pp.deinit();

    pp.preprocess(source) catch unreachable;

    for (expectedTokens, 0..) |expectedTokenId, i| {
        const token = pp.tokens.items[i];
        if (!std.meta.eql(token.id, expectedTokenId)) {
            std.debug.panic("expected {s}, found {s}\n", .{ @tagName(expectedTokenId), @tagName(token.id) });
        }
    }
    const lastToken = pp.tokens.items[expectedTokens.len];
    std.testing.expect(lastToken.id == .Eof) catch std.debug.print("", .{});
}

fn expectStr(buffer: []const u8, expected: []const u8) void {
    var comp = Compilation.init(std.testing.allocator);
    defer comp.deinit();

    const source = Source{
        .buffer = buffer,
        .id = @as(Source.ID, @enumFromInt(0)),
        .path = "<test-buf>",
    };

    comp.sources.putNoClobber(source.path, source) catch unreachable;
    defer comp.sources.clearAndFree();

    var pp = Preprocessor.init(&comp);
    defer pp.deinit();

    pp.preprocess(source) catch unreachable;

    var actual = std.ArrayList(u8).init(std.testing.allocator);
    defer actual.deinit();

    for (pp.tokens.items, 0..) |token, i| {
        if (token.id == .Eof) break;

        if (i != 0) actual.append(' ') catch unreachable;

        actual.appendSlice(pp.tokSlice(token)) catch unreachable;
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
