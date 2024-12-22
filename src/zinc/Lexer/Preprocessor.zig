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
const DefineMap = std.StringHashMapUnmanaged(Macro);
const RawTokenList = std.ArrayList(RawToken);
const MaxIncludeDepth = 200;

/// Errors that can be returned when expanding a macro.
/// error.UnknownPragma can occur within Preprocessor.pragma() but
/// it is handled there and doesn't escape that function
const MacroError = Error || error{StopPreprocessing};

const IfContext = struct {
    const Backing = u2;
    const Nesting = enum(Backing) {
        untilElse,
        untilEndIf,
        untilEndIfSeenElse,
    };

    const bufferSizeBits = @bitSizeOf(Backing) * 256;
    kind: [bufferSizeBits / std.mem.byte_size_in_bits]u8,
    level: u8,

    fn get(self: *const IfContext) Nesting {
        return @enumFromInt(std.mem.readPackedIntNative(Backing, &self.kind, @as(usize, self.level) * 2));
    }

    fn set(self: *IfContext, context: Nesting) void {
        std.mem.writePackedIntNative(Backing, &self.kind, @as(usize, self.level) * 2, @intFromEnum(context));
    }

    fn increment(self: *IfContext) bool {
        self.level, const overflowed = @addWithOverflow(self.level, 1);
        return overflowed != 0;
    }

    fn decrement(self: *IfContext) void {
        self.level -= 1;
    }

    /// Initialize `kind` to an invalid value since it is an error to read the kind before setting it.
    /// Doing so will trigger safety-checked undefined behavior in `IfContext.get`
    const default: IfContext = .{ .kind = @splat(0xFF), .level = 0 };
};

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
    start: u32,
    end: u32,

    fn eql(a: Macro, b: Macro, pp: *Preprocessor) bool {
        if (a.tokens.len != b.tokens.len)
            return false;

        if (a.isBuiltin != b.isBuiltin)
            return false;

        for (a.tokens, b.tokens) |aToken, bToken|
            if (!tokEql(pp, aToken, bToken))
                return false;

        if (a.isFunc and b.isFunc) {
            if (a.varArgs != b.varArgs) return false;
            if (a.params.len != b.params.len) return false;
            for (a.params, b.params) |aParam, bParam|
                if (!std.mem.eql(u8, aParam, bParam))
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
defines: DefineMap = .{},
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

/// linemarker tokens. Must be .none unless in -E mode (parser does not handle linemarkers)
linemarkers: LineMarkers = .None,

pub const parse = Parser.parse;

pub const LineMarkers = enum {
    /// No linemarker tokens. Required setting if parser will run
    None,
    /// #line <num> "filename"
    LineDirectives,
    /// # <num> "filename" flags
    NumericDirectives,
};

const BuiltinMacros = struct {
    const args = [1][]const u8{"X"};
    const hasAttribute = makeFeatCheckMacro(.MacroParamHasAttribute);
    const hasCAttribute = makeFeatCheckMacro(.MacroParamHasCAttribute);
    const hasDeclspecAttribute = makeFeatCheckMacro(.MacroParamHasDeclspecAttribute);
    const hasWarning = makeFeatCheckMacro(.MacroParamHasWarning);
    const hasFeature = makeFeatCheckMacro(.MacroParamHasFeature);
    const hasExtension = makeFeatCheckMacro(.MacroParamHasExtension);
    const hasBuiltin = makeFeatCheckMacro(.MacroParamHasBuiltin);
    const hasInclude = makeFeatCheckMacro(.MacroParamHasInclude);
    const hasIncludeNext = makeFeatCheckMacro(.MacroParamHasIncludeNext);
    const hasEmbed = makeFeatCheckMacro(.MacroParamHasEmbed);
    const isIdentifier = makeFeatCheckMacro(.MacroParamIsIdentifier);
    const file = makeFeatCheckMacro(.MacroFile);
    const line = makeFeatCheckMacro(.MacroLine);
    const counter = makeFeatCheckMacro(.MacroCounter);
    const pragmaOperator = makeFeatCheckMacro(.MacroParamPragmaOperator);

    inline fn makeFeatCheckMacro(id: TokenType) [1]RawToken {
        return [1]RawToken{.{ .id = id, .source = .generated }};
    }
};

pub fn addBuiltinMacro(pp: *Preprocessor, name: []const u8, isFunc: bool, tokens: []const RawToken) !void {
    try pp.defines.putNoClobber(pp.gpa, name, .{
        .params = &BuiltinMacros.args,
        .tokens = tokens,
        .varArgs = false,
        .isFunc = isFunc,
        .loc = .{ .id = .generated },
        .start = 0,
        .end = 0,
        .isBuiltin = true,
    });
}

pub fn addBuiltinMacros(pp: *Preprocessor) !void {
    try pp.addBuiltinMacro("__has_attribute", true, &BuiltinMacros.hasAttribute);
    try pp.addBuiltinMacro("__has_c_attribute", true, &BuiltinMacros.hasCAttribute);
    try pp.addBuiltinMacro("__has_declspec_attribute", true, &BuiltinMacros.hasDeclspecAttribute);
    try pp.addBuiltinMacro("__has_warning", true, &BuiltinMacros.hasWarning);
    try pp.addBuiltinMacro("__has_feature", true, &BuiltinMacros.hasFeature);
    try pp.addBuiltinMacro("__has_extension", true, &BuiltinMacros.hasExtension);
    try pp.addBuiltinMacro("__has_builtin", true, &BuiltinMacros.hasBuiltin);
    try pp.addBuiltinMacro("__has_include", true, &BuiltinMacros.hasInclude);
    try pp.addBuiltinMacro("__has_include_next", true, &BuiltinMacros.hasIncludeNext);
    try pp.addBuiltinMacro("__has_embed", true, &BuiltinMacros.hasEmbed);
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
        .tokenBuffer = RawTokenList.init(comp.gpa),
        .charBuffer = std.ArrayList(u8).init(comp.gpa),
        .poisonedIdentifiers = std.StringHashMap(void).init(comp.gpa),
        .topExpansionBuffer = ExpandBuffer.init(comp.gpa),
    };

    comp.pragmaEvent(.BeforePreprocess);
    return pp;
}

/// Initialize Preprocessor with builtin macros.
pub fn initDefault(comp: *Compilation) !Preprocessor {
    var pp = init(comp);
    errdefer pp.deinit();

    try pp.addBuiltinMacros();
    return pp;
}

pub fn deinit(pp: *Preprocessor) void {
    pp.defines.deinit(pp.gpa);
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
    while (hash.is(.NewLine))
        hash = lexer.nextNoWhiteSpace();
    if (hash.isNot(.Hash)) return null;

    const ifndef = lexer.nextNoWhiteSpace();
    if (ifndef.isNot(.KeywordIfndef)) return null;

    const guard = lexer.nextNoWhiteSpace();
    if (guard.isNot(.Identifier)) return null;

    return pp.getTokenSlice(guard);
}

pub fn addIncludeStart(pp: *Preprocessor, source: Source) !void {
    if (pp.linemarkers == .None) return;
    try pp.tokens.append(pp.gpa, .{
        .id = .IncludeStart,
        .loc = .{
            .id = source.id,
            .byteOffset = std.math.maxInt(u32),
            .line = 0,
        },
    });
}

pub fn addIncludeResume(pp: *Preprocessor, source: Source.ID, offset: u32, line: u32) !void {
    if (pp.linemarkers == .None) return;
    try pp.tokens.append(pp.gpa, .{
        .id = .IncludeResume,
        .loc = .{
            .id = source,
            .byteOffset = offset,
            .line = line,
        },
    });
}

/// Preprocess a compilation unit of sources into a parsable list of tokens.
pub fn preprocessSources(pp: *Preprocessor, sources: []const Source) Error!void {
    assert(sources.len > 1);

    const first = sources[0];
    try pp.addIncludeStart(first);
    for (sources[1..]) |header| {
        try pp.addIncludeStart(header);
        _ = try pp.preprocess(header);
    }
    try pp.addIncludeResume(first.id, 0, 0);
    const eof = try pp.preprocess(first);
    try pp.tokens.append(pp.comp.gpa, eof);
}

/// Preprocess a source file, returns eof token.
pub fn preprocess(pp: *Preprocessor, source: Source) Error!Token {
    const eof = pp.preprocessExtra(source) catch |err| switch (err) {
        // This cannot occur in the main file and is handled in `include`.
        error.StopPreprocessing => unreachable,
        else => |e| return e,
    };
    try eof.checkMsEof(source, pp.comp);
    return eof;
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

    var ifContext: IfContext = .default;

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
                            if (token.isOneOf(.{ .NewLine, .Eof })) break;
                            if (token.is(.WhiteSpace)) token.id = .MacroWS;
                            try pp.topExpansionBuffer.append(tokenFromRaw(token));
                        }

                        try pp.stringify(pp.topExpansionBuffer.items);
                        const slice = pp.charBuffer.items[charTop + 1 .. pp.charBuffer.items.len - 2];
                        const message = try pp.comp.diagnostics.arena.allocator().dupe(u8, slice);

                        try pp.comp.addDiagnostic(.{
                            .tag = if (directive.is(.KeywordError)) .error_directive else .warning_directive,
                            .loc = .{ .id = token.source, .byteOffset = directive.start, .line = directive.line },
                            .extra = .{ .str = message },
                        }, &.{});
                    },

                    .KeywordIf => {
                        const overflowed = ifContext.increment();
                        if (overflowed)
                            return pp.fatal(directive, "too many #if nestings", .{});

                        if (try pp.expr(&lexer)) {
                            ifContext.set(.untilEndIf);
                            if (pp.verbose)
                                pp.verboseLog(directive, "entering then branch of #if", .{});
                        } else {
                            ifContext.set(.untilElse);
                            try pp.skip(&lexer, .untilElse);
                            if (pp.verbose)
                                pp.verboseLog(directive, "entering else branch of #if", .{});
                        }
                    },

                    .KeywordIfdef => {
                        const overflowed = ifContext.increment();
                        if (overflowed)
                            return pp.fatal(directive, "too many #if nestings", .{});

                        const macroName = try pp.expectMacroName(&lexer) orelse continue;
                        try pp.expectNewLine(&lexer);

                        if (pp.defines.get(macroName) != null) {
                            ifContext.set(.untilEndIf);
                            if (pp.verbose)
                                pp.verboseLog(directive, "entering then branch of #ifdef", .{});
                        } else {
                            ifContext.set(.untilElse);
                            try pp.skip(&lexer, .untilElse);
                            if (pp.verbose)
                                pp.verboseLog(directive, "entering else branch of #ifdef", .{});
                        }
                    },

                    .KeywordIfndef => {
                        const overflowed = ifContext.increment();
                        if (overflowed)
                            return pp.fatal(directive, "too many #if nestings", .{});

                        const macroName = try pp.expectMacroName(&lexer) orelse continue;
                        try pp.expectNewLine(&lexer);

                        if (pp.defines.get(macroName) == null) {
                            ifContext.set(.untilEndIf);
                        } else {
                            ifContext.set(.untilElse);
                            try pp.skip(&lexer, .untilElse);
                        }
                    },

                    .KeywordUndef => {
                        const macroName = (try pp.expectMacroName(&lexer)) orelse continue;
                        _ = pp.defines.remove(macroName);
                        try pp.expectNewLine(&lexer);
                    },

                    .KeywordElIf => {
                        if (ifContext.level == 0) {
                            try pp.addError(directive, .elif_without_if);
                            _ = ifContext.increment();
                            ifContext.set(.untilElse);
                        } else if (ifContext.level == 1) {
                            guardName = null;
                        }

                        switch (ifContext.get()) {
                            .untilElse => if (try pp.expr(&lexer)) {
                                ifContext.set(.untilEndIf);
                                if (pp.verbose)
                                    pp.verboseLog(directive, "entering then branch of #elif", .{});
                            } else {
                                try pp.skip(&lexer, .untilElse);
                                if (pp.verbose)
                                    pp.verboseLog(directive, "entering else branch of #elif", .{});
                            },

                            .untilEndIf => try pp.skip(&lexer, .untilEndIf),
                            .untilEndIfSeenElse => {
                                try pp.addError(directive, .elif_after_else);
                                skipToNewLine(&lexer);
                            },
                        }
                    },

                    .KeywordElse => {
                        try pp.expectNewLine(&lexer);

                        if (ifContext.level == 0) {
                            try pp.addError(directive, .else_without_if);
                            continue;
                        } else if (ifContext.level == 1) {
                            guardName = null;
                        }

                        switch (ifContext.get()) {
                            .untilElse => {
                                ifContext.set(.untilEndIfSeenElse);
                                if (pp.verbose)
                                    pp.verboseLog(directive, "#else branch here", .{});
                            },
                            .untilEndIf => try pp.skip(&lexer, .untilEndIfSeenElse),
                            .untilEndIfSeenElse => {
                                try pp.addError(directive, .else_after_else);
                                skipToNewLine(&lexer);
                            },
                        }
                    },

                    .KeywordEndIf => {
                        try pp.expectNewLine(&lexer);
                        if (ifContext.level == 0) {
                            guardName = null;
                            try pp.addError(directive, .else_without_if);
                            continue;
                        } else if (ifContext.level == 1) {
                            const savedLexer = lexer;
                            defer lexer = savedLexer;

                            var next = lexer.nextNoWhiteSpace();
                            while (next.is(.NewLine)) : (next = lexer.nextNoWhiteSpace()) {}
                            if (next.isNot(.Eof)) guardName = null;
                        }
                        ifContext.decrement();
                    },

                    .KeywordDefine => try pp.define(&lexer),
                    .KeywordInclude => {
                        try pp.include(&lexer, .First);
                        continue;
                    },
                    .KeywordIncludeNext => {
                        try pp.comp.addDiagnostic(.{
                            .tag = .include_next,
                            .loc = .{ .id = token.source, .byteOffset = directive.start, .line = directive.line },
                        }, &.{});
                        if (pp.includeDepth == 0) {
                            try pp.comp.addDiagnostic(.{
                                .tag = .include_next_outside_header,
                                .loc = .{ .id = token.source, .byteOffset = directive.start, .line = directive.line },
                            }, &.{});
                            try pp.include(&lexer, .First);
                        } else {
                            try pp.include(&lexer, .Next);
                        }
                    },

                    .KeywordEmbed => try pp.embed(&lexer),
                    .KeywordPragma => {
                        try pp.pragma(&lexer, directive, null, &.{});
                        continue;
                    },

                    .KeywordLine => {
                        const digits = lexer.nextNoWhiteSpace();
                        if (digits.isNot(.PPNumber))
                            try pp.addError(digits, .line_simple_digit);

                        if (digits.isOneOf(.{ .Eof, .NewLine }))
                            continue;

                        const name = lexer.nextNoWhiteSpace();
                        if (name.isOneOf(.{ .Eof, .NewLine }))
                            continue;

                        if (name.isNot(.StringLiteral))
                            try pp.addError(name, .line_invalid_filename);

                        try pp.expectNewLine(&lexer);
                    },

                    .PPNumber => {
                        // # number "file" flags
                        // TODO: validate that the pp_num token is solely digits
                        // if not, emit `GNU line marker directive requires a simple digit sequence`
                        const name = lexer.nextNoWhiteSpace();
                        if (name.isOneOf(.{ .Eof, .NewLine })) continue;
                        if (name.isNot(.NewLine)) try pp.addError(name, .line_invalid_filename);

                        const flag1 = lexer.nextNoWhiteSpace();
                        if (flag1.isOneOf(.{ .Eof, .NewLine })) continue;
                        const flag2 = lexer.nextNoWhiteSpace();
                        if (flag2.isOneOf(.{ .Eof, .NewLine })) continue;
                        const flag3 = lexer.nextNoWhiteSpace();
                        if (flag3.isOneOf(.{ .Eof, .NewLine })) continue;
                        const flag4 = lexer.nextNoWhiteSpace();
                        if (flag4.isOneOf(.{ .Eof, .NewLine })) continue;
                        try pp.expectNewLine(&lexer);
                    },
                    .NewLine => {},
                    .Eof => {
                        if (ifContext.level != 0)
                            try pp.addError(directive, .unterminated_conditional_directive);
                        return tokenFromRaw(directive);
                    },
                    else => {
                        try pp.addError(token, .invalid_preprocessing_directive);
                        skipToNewLine(&lexer);
                    },
                }
                if (pp.preserveWhitespace) {
                    token.id = .NewLine;
                    try pp.tokens.append(pp.gpa, tokenFromRaw(token));
                }
            },

            .WhiteSpace => if (pp.preserveWhitespace) try pp.tokens.append(pp.gpa, tokenFromRaw(token)),
            .NewLine => {
                startOfLine = true;
                if (pp.preserveWhitespace)
                    try pp.tokens.append(pp.gpa, tokenFromRaw(token));
            },

            .Eof => {
                if (ifContext.level != 0)
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

                switch (token.id) {
                    .UnterminatedStringLiteral => try pp.addError(token, .unterminated_string_literal_warning),
                    .UnterminatedCharLiteral => try pp.addError(token, .unterminated_char_literal_warning),
                    .EmptyCharLiteral => try pp.addError(token, .empty_char_literal_warning),
                    .UnterminatedComment => {
                        try pp.addError(token, .unterminated_comment);
                        continue;
                    },
                    else => {},
                }

                if (std.mem.eql(u8, lexer.buffer[token.start..token.end], "__has_include")) {
                    token.id.simplifyMacroKeyword();
                    try pp.comp.addDiagnostic(.{
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
    assert(pp.linemarkers == .None);
    assert(pp.preserveWhitespace == false);
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
        if (tok.is(.Eof)) return tokenFromRaw(tok);
        try pp.tokens.append(pp.gpa, tokenFromRaw(tok));
    }
}

/// Get raw token source string.
/// Returned slice is invalidated when comp.generatedBuffer is updated.
pub fn getTokenSlice(pp: *Preprocessor, token: RawToken) []const u8 {
    if (token.id.lexeme()) |some|
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
    try pp.comp.addDiagnostic(.{
        .tag = tag,
        .loc = .{
            .id = raw.source,
            .byteOffset = raw.start,
            .line = raw.line,
        },
    }, &.{});
}

fn errStr(pp: *Preprocessor, tok: Token, tag: Diagnostics.Tag, str: []const u8) !void {
    try pp.comp.addDiagnostic(.{
        .tag = tag,
        .loc = tok.loc,
        .extra = .{ .str = str },
    }, tok.expansionSlice());
}

fn fatal(pp: *Preprocessor, raw: RawToken, comptime fmt: []const u8, args: anytype) Compilation.Error {
    try pp.comp.diagnostics.list.append(pp.gpa, .{
        .tag = .cli_error,
        .kind = .@"fatal error",
        .extra = .{ .str = try std.fmt.allocPrint(pp.comp.diagnostics.arena.allocator(), fmt, args) },
        .loc = .{
            .id = raw.source,
            .byteOffset = raw.start,
            .line = raw.line,
        },
    });
    return error.FatalError;
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
        if (token.isOneOf(.{ .Eof, .NewLine }))
            return;
        if (token.is(.WhiteSpace))
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
        if (token.is(.MacroWS)) continue;
        if (!token.id.validPreprocessorExprStart()) {
            try pp.comp.addDiagnostic(.{
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
                try pp.comp.addDiagnostic(.{
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
                try pp.comp.addDiagnostic(.{
                    .tag = .invalid_preproc_operator,
                    .loc = token.loc,
                }, token.expansionSlice());
                return false;
            },

            .MacroWS, .WhiteSpace => continue,

            .KeywordFalse => token.id = .Zero,
            .KeywordTrue => token.id = .One,

            else => if (token.id.isMacroIdentifier()) {
                if (token.is(.KeywordDefined)) {
                    const tokenConsumed = try pp.handleKeywordDefined(&token, items[i + 1 ..], eof);
                    i += tokenConsumed;
                } else {
                    try pp.errStr(token, .undefined_macro, pp.expandedSlice(token));

                    if (i + 1 < pp.topExpansionBuffer.items.len and
                        pp.topExpansionBuffer.items[i + 1].is(.LParen))
                    {
                        try pp.errStr(token, .fn_macro_undefined, pp.expandedSlice(token));
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
        .strings = std.ArrayList(u8).init(pp.comp.gpa),

        .data = undefined,
        .labels = undefined,
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
    defer parser.strings.deinit();

    return parser.macroExpr();
}

/// Turns macro_tok from .keyword_defined into .zero or .one depending on whether the argument is defined
/// Returns the number of tokens consumed
fn handleKeywordDefined(pp: *Preprocessor, macroToken: *Token, tokens: []const Token, eof: RawToken) !usize {
    assert(macroToken.is(.KeywordDefined));
    var it = TokenIterator.init(tokens);
    const first = it.nextNoWS() orelse {
        try pp.addError(eof, .macro_name_missing);
        return it.i;
    };
    switch (first.id) {
        .LParen => {},
        else => {
            if (!first.id.isMacroIdentifier())
                try pp.errStr(first, .macro_name_must_be_identifier, pp.expandedSlice(first));
            macroToken.id = if (pp.defines.contains(pp.expandedSlice(first))) .One else .Zero;
            return it.i;
        },
    }
    const second = it.nextNoWS() orelse {
        try pp.addError(eof, .macro_name_missing);
        return it.i;
    };
    if (!second.id.isMacroIdentifier()) {
        try pp.comp.addDiagnostic(.{
            .tag = .macro_name_must_be_identifier,
            .loc = second.loc,
        }, second.expansionSlice());
        return it.i;
    }
    macroToken.id = if (pp.defines.contains(pp.expandedSlice(second))) .One else .Zero;

    const last = it.nextNoWS();
    if (last == null or last.?.isNot(.RParen)) {
        const tok = last orelse tokenFromRaw(eof);
        try pp.comp.addDiagnostic(.{
            .tag = .closing_paren,
            .loc = tok.loc,
        }, tok.expansionSlice());
        try pp.comp.addDiagnostic(.{
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

            if (hash.is(.NewLine)) continue;
            lineStart = false;
            if (hash.isNot(.Hash)) continue;

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
            if (pp.preserveWhitespace) {
                try pp.tokens.append(pp.gpa, .{
                    .id = .NewLine,
                    .loc = .{ .id = lexer.source, .line = lexer.line },
                });
            }
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
        if (token.isOneOf(.{ .NewLine, .Eof })) return;
    }
}

const ExpandBuffer = std.ArrayList(Token);
const MacroArguments = std.ArrayList([]const Token);

fn removePlaceMarkers(buf: *ExpandBuffer) void {
    var i: usize = buf.items.len -% 1;
    while (i < buf.items.len) : (i -%= 1) {
        if (buf.items[i].is(.PlaceMarker)) {
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
                while (true) {
                    if (rhs.is(.WhiteSpace)) {
                        rhs = tokenFromRaw(simpleMacro.tokens[i + 1]);
                        i += 1;
                    } else if (rhs.is(.Comment) and !pp.comp.langOpts.preserveCommentsInMacros) {
                        rhs = tokenFromRaw(simpleMacro.tokens[i + 1]);
                        i += 1;
                    } else break;
                }
                try pp.pasteTokens(&buff, &.{rhs});
            },
            .WhiteSpace => if (pp.preserveWhitespace) buff.appendAssumeCapacity(token),
            .MacroFile => {
                const start = pp.comp.generatedBuffer.items.len;
                const source = pp.comp.getSource(pp.expansionSourceLoc.id);

                const w = pp.comp.generatedBuffer.writer(pp.gpa);
                try w.print("\"{s}\"\n", .{source.path});

                buff.appendAssumeCapacity(try pp.makeGeneratedToken(start, .StringLiteral, token));
            },
            .MacroLine => {
                const start = pp.comp.generatedBuffer.items.len;
                const source = pp.comp.getSource(pp.expansionSourceLoc.id);

                const w = pp.comp.generatedBuffer.writer(pp.gpa);
                try w.print("{d}\n", .{source.physicalLine(pp.expansionSourceLoc)});

                buff.appendAssumeCapacity(try pp.makeGeneratedToken(start, .PPNumber, token));
            },
            .MacroCounter => {
                defer pp.counter += 1;
                const start = pp.comp.generatedBuffer.items.len;

                const w = pp.comp.generatedBuffer.writer(pp.gpa);
                try w.print("{d}\n", .{pp.counter});

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
    if (toks.len >= 2 and toks[0].is(.LParen) and toks[toks.len - 1].is(.RParen))
        unwrapped = toks[1 .. toks.len - 1];
    if (unwrapped.len == 0)
        return error.ExpectedStringLiteral;

    for (unwrapped) |tok| {
        if (tok.is(.MacroWS)) continue;
        if (tok.isNot(.StringLiteral)) return error.ExpectedStringLiteral;
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
    try pp.comp.generatedBuffer.appendSlice(pp.gpa, pp.charBuffer.items);
    var tempLexer = Lexer{
        .buffer = pp.comp.generatedBuffer.items,
        .comp = pp.comp,
        .index = @intCast(start),
        .source = .generated,
        .line = pp.generatedLine,
    };
    pp.generatedLine += 1;

    const hashToken = tempLexer.next();
    assert(hashToken.is(.Hash));

    const pragmaToken = tempLexer.next();
    assert(pragmaToken.is(.KeywordPragma));

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
        if (tok.is(.MacroWS)) {
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
        try pp.comp.addDiagnostic(.{
            .tag = .invalid_pp_stringify_escape,
            .loc = tok.loc,
        }, tok.expansionSlice());
        pp.charBuffer.items.len -= 1; // remove `\`
    }

    try pp.charBuffer.appendSlice("\"\n");
}

fn reconstructIncludeString(pp: *Preprocessor, paramTokens: []const Token, embedArgs: ?*[]const Token) !?[]const u8 {
    const charTop = pp.charBuffer.items.len;
    defer pp.charBuffer.items.len = charTop;

    // Trim leading/trailing whitespace
    var begin: usize = 0;
    var end = paramTokens.len;
    while (begin < end and paramTokens[begin].is(.MacroWS)) begin += 1;
    while (end > begin and paramTokens[end - 1].is(.MacroWS)) end -= 1;
    const params = paramTokens[begin..end];

    if (params.len == 0) {
        try pp.comp.addDiagnostic(.{
            .tag = .expected_filename,
            .loc = paramTokens[0].loc,
        }, paramTokens[0].expansionSlice());
        return null;
    }

    // no string pasting
    if (embedArgs == null and params[0].is(.StringLiteral) and params.len > 1) {
        try pp.comp.addDiagnostic(.{
            .tag = .closing_paren,
            .loc = params[1].loc,
        }, params[1].expansionSlice());
        return null;
    }

    for (params, 0..) |tok, i| {
        const str = pp.expandedSliceExtra(tok, .PreserveMacroWS);
        try pp.charBuffer.appendSlice(str);
        if (embedArgs) |some| {
            if ((i == 0 and tok.is(.StringLiteral)) or tok.is(.AngleBracketRight)) {
                some.* = params[i + 1 ..];
                break;
            }
        }
    }

    const includeStr = pp.charBuffer.items[charTop..];
    if (includeStr.len < 3) {
        try pp.comp.addDiagnostic(.{
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
                try pp.comp.addDiagnostic(.{
                    .tag = .header_str_closing,
                    .loc = .{ .id = start.id, .byteOffset = start.byteOffset + @as(u32, @intCast(includeStr.len)) + 1, .line = start.line },
                }, params[0].expansionSlice());

                try pp.comp.addDiagnostic(.{
                    .tag = .header_str_match,
                    .loc = params[0].loc,
                }, params[0].expansionSlice());
                return null;
            }
            return includeStr;
        },

        '"' => return includeStr,

        else => {
            try pp.comp.addDiagnostic(.{
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
        .MacroParamHasDeclspecAttribute,
        .MacroParamHasFeature,
        .MacroParamHasExtension,
        .MacroParamHasBuiltin,
        => {
            var invalid: ?Token = null;
            var identifier: ?Token = null;
            for (paramTokens) |tok| {
                if (tok.isOneOf(.{ .MacroWS, .Comment })) continue;
                if (!tok.id.isMacroIdentifier()) {
                    invalid = tok;
                    break;
                }

                if (identifier) |_| invalid = tok else identifier = tok;
            }
            if (identifier == null and invalid == null) invalid = .{ .id = .Eof, .loc = srcLoc };
            if (invalid) |some| {
                try pp.comp.addDiagnostic(
                    .{ .tag = .feature_check_requires_identifier, .loc = some.loc },
                    some.expansionSlice(),
                );
                return false;
            }

            const identifierStr = pp.expandedSlice(identifier.?);

            return switch (builtin) {
                .MacroParamHasAttribute => Attribute.fromString(.gnu, null, identifierStr) != null,
                .MacroParamHasDeclspecAttribute => {
                    return if (pp.comp.langOpts.declSpecAttrs)
                        Attribute.fromString(.declspec, null, identifierStr) != null
                    else
                        false;
                },
                .MacroParamHasFeature => Features.hasFeature(pp.comp, identifierStr),
                .MacroParamHasExtension => Features.hasExtension(pp.comp, identifierStr),
                .MacroParamHasBuiltin => pp.comp.builtins.hasBuiltin(identifierStr),
                else => unreachable,
            };
        },

        .MacroParamHasWarning => {
            const actualParam = pp.pasteStringsUnsafe(paramTokens) catch |er| switch (er) {
                error.ExpectedStringLiteral => {
                    try pp.errStr(paramTokens[0], .expected_str_literal_in, "__has_warning");
                    return false;
                },
                else => |e| return e,
            };

            if (!std.mem.startsWith(u8, actualParam, "-W")) {
                try pp.errStr(paramTokens[0], .malformed_warning_check, "__has_warning");
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
                .Comment => continue,
                else => {
                    if (identifier) |_| invalid = tok else identifier = tok;
                },
            };
            if (identifier == null and invalid == null) invalid = .{ .id = .Eof, .loc = srcLoc };
            if (invalid) |some| {
                try pp.comp.addDiagnostic(.{
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
            const includeStr = (try pp.reconstructIncludeString(paramTokens, null)) orelse return false;
            const includeType: Compilation.IncludeType = switch (includeStr[0]) {
                '"' => .Quotes,
                '<' => .AngleBrackets,
                else => unreachable,
            };
            const filename = includeStr[1 .. includeStr.len - 1];

            if (builtin == .MacroParamHasInclude or pp.includeDepth == 0) {
                if (builtin == .MacroParamHasIncludeNext) {
                    try pp.comp.addDiagnostic(.{
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

                    var vaoptBuffer = ExpandBuffer.init(pp.gpa);
                    defer vaoptBuffer.deinit();

                    const next = switch (rawNext.id) {
                        .MacroWS => continue,
                        .HashHash => continue,
                        .Comment => if (!pp.comp.langOpts.preserveCommentsInMacros)
                            continue
                        else
                            &[1]Token{tokenFromRaw(rawNext)},
                        .MacroParam, .MacroParamNoExpand => if (args.items[rawNext.end].len > 0)
                            args.items[rawNext.end]
                        else
                            &[1]Token{tokenFromRaw(.{ .id = .PlaceMarker, .source = .generated })},
                        .KeywordVarArgs => varArguments.items,
                        .KeywordVarOpt => blk: {
                            try pp.expandVaOpt(&vaoptBuffer, rawNext, varArguments.items.len != 0);
                            if (varArguments.items.len == 0) break;
                            break :blk vaoptBuffer.items;
                        },
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

            .KeywordVarOpt => {
                try pp.expandVaOpt(&buf, raw, varArguments.items.len != 0);
            },

            .StringifyParam, .StringifyVarArgs => {
                const arg = if (raw.is(.StringifyVarArgs))
                    varArguments.items
                else
                    args.items[raw.end];

                pp.charBuffer.clearRetainingCapacity();
                try pp.stringify(arg);

                const start = pp.comp.generatedBuffer.items.len;
                try pp.comp.generatedBuffer.appendSlice(pp.gpa, pp.charBuffer.items);

                try buf.append(try pp.makeGeneratedToken(start, .StringLiteral, tokenFromRaw(raw)));
            },

            .MacroParamHasAttribute,
            .MacroParamHasDeclspecAttribute,
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
                    try pp.comp.addDiagnostic(.{ .tag = .expected_arguments, .loc = loc, .extra = extra }, &.{});
                    try buf.append(.{ .id = .Zero, .loc = loc });
                    break :blk false;
                } else try pp.handleBuiltinMacro(raw.id, arg, loc);

                const start = pp.comp.generatedBuffer.items.len;
                const w = pp.comp.generatedBuffer.writer(pp.gpa);

                try w.print("{}\n", .{@intFromBool(result)});
                try buf.append(try pp.makeGeneratedToken(start, .PPNumber, tokenFromRaw(raw)));
            },

            .MacroParamHasCAttribute => {
                const arg = expandedArgs.items[0];
                const notFound = "0\n";
                const result = if (arg.len == 0) blk: {
                    const extra = Diagnostics.Message.Extra{ .arguments = .{ .expected = 1, .actual = 0 } };
                    try pp.comp.addDiagnostic(.{ .tag = .expected_arguments, .loc = loc, .extra = extra }, &.{});
                    break :blk notFound;
                } else res: {
                    var invalid: ?Token = null;
                    var vendorIdent: ?Token = null;
                    var coloncolon: ?Token = null;
                    var attrIdent: ?Token = null;
                    for (arg) |tok| {
                        if (tok.isOneOf(.{ .MacroWS, .Comment })) continue;
                        if (tok.is(.ColonColon)) {
                            if (coloncolon != null or attrIdent == null) {
                                invalid = tok;
                                break;
                            }
                            vendorIdent = attrIdent;
                            attrIdent = null;
                            coloncolon = tok;
                            continue;
                        }
                        if (!tok.id.isMacroIdentifier()) {
                            invalid = tok;
                            break;
                        }
                        if (attrIdent) |_| {
                            invalid = tok;
                            break;
                        } else attrIdent = tok;
                    }

                    if (vendorIdent != null and attrIdent == null) {
                        invalid = vendorIdent;
                    } else if (attrIdent == null and invalid == null) {
                        invalid = .{ .id = .Eof, .loc = loc };
                    }

                    if (invalid) |some| {
                        try pp.comp.addDiagnostic(
                            .{ .tag = .feature_check_requires_identifier, .loc = some.loc },
                            some.expansionSlice(),
                        );
                        break :res notFound;
                    }

                    if (vendorIdent) |some| {
                        const vendorStr = pp.expandedSlice(some);
                        const attrStr = pp.expandedSlice(attrIdent.?);
                        const exists = Attribute.fromString(.gnu, vendorStr, attrStr) != null;

                        const start = pp.comp.generatedBuffer.items.len;
                        try pp.comp.generatedBuffer.appendSlice(pp.gpa, if (exists) "1\n" else "0\n");
                        try buf.append(try pp.makeGeneratedToken(start, .PPNumber, tokenFromRaw(raw)));
                        continue;
                    }

                    if (!pp.comp.langOpts.standard.atLeast(.c23)) break :res notFound;

                    const attrs = std.StaticStringMap([]const u8).initComptime(.{
                        .{ "deprecated", "201904L\n" },
                        .{ "fallthrough", "201904L\n" },
                        .{ "maybe_unused", "201904L\n" },
                        .{ "nodiscard", "202003L\n" },
                        .{ "noreturn", "202202L\n" },
                        .{ "_Noreturn", "202202L\n" },
                        .{ "unsequenced", "202207L\n" },
                        .{ "reproducible", "202207L\n" },
                    });

                    const attrStr = Attribute.normalize(pp.expandedSlice(attrIdent.?));
                    break :res attrs.get(attrStr) orelse notFound;
                };
                const start = pp.comp.generatedBuffer.items.len;
                try pp.comp.generatedBuffer.appendSlice(pp.gpa, result);
                try buf.append(try pp.makeGeneratedToken(start, .PPNumber, tokenFromRaw(raw)));
            },

            .MacroParamHasEmbed => {
                const arg = expandedArgs.items[0];
                const notFound = "0\n";
                const result = if (arg.len == 0) blk: {
                    const extra = Diagnostics.Message.Extra{ .arguments = .{ .expected = 1, .actual = 0 } };
                    try pp.comp.addDiagnostic(.{ .tag = .expected_arguments, .loc = loc, .extra = extra }, &.{});
                    break :blk notFound;
                } else res: {
                    var embedArgs: []const Token = &.{};
                    const includeStr = (try pp.reconstructIncludeString(arg, &embedArgs)) orelse
                        break :res notFound;

                    var prev = tokenFromRaw(raw);
                    prev.id = .Eof;
                    var it: struct {
                        i: u32 = 0,
                        slice: []const Token,
                        prev: Token,
                        fn next(it: *@This()) Token {
                            while (it.i < it.slice.len) switch (it.slice[it.i].id) {
                                .MacroWS, .WhiteSpace => it.i += 1,
                                else => break,
                            } else return it.prev;
                            defer it.i += 1;

                            it.prev = it.slice[it.i];
                            it.prev.id = .Eof;
                            return it.slice[it.i];
                        }
                    } = .{ .slice = embedArgs, .prev = prev };

                    while (true) {
                        const paramFirst = it.next();
                        if (paramFirst.is(.Eof)) break;
                        if (paramFirst.isNot(.Identifier)) {
                            try pp.comp.addDiagnostic(
                                .{ .tag = .malformed_embed_param, .loc = paramFirst.loc },
                                paramFirst.expansionSlice(),
                            );
                            continue;
                        }

                        const charTop = pp.charBuffer.items.len;
                        defer pp.charBuffer.items.len = charTop;

                        const maybeColon = it.next();
                        const param = switch (maybeColon.id) {
                            .ColonColon => blk: {
                                // vendor::param
                                const param = it.next();
                                if (param.isNot(.Identifier)) {
                                    try pp.comp.addDiagnostic(
                                        .{ .tag = .malformed_embed_param, .loc = param.loc },
                                        param.expansionSlice(),
                                    );
                                    continue;
                                }

                                const lparen = it.next();
                                if (lparen.isNot(.LParen)) {
                                    try pp.comp.addDiagnostic(
                                        .{ .tag = .malformed_embed_param, .loc = lparen.loc },
                                        lparen.expansionSlice(),
                                    );
                                    continue;
                                }
                                break :blk "doesn't exist";
                            },
                            .LParen => Attribute.normalize(pp.expandedSlice(paramFirst)),
                            else => {
                                try pp.comp.addDiagnostic(
                                    .{ .tag = .malformed_embed_param, .loc = maybeColon.loc },
                                    maybeColon.expansionSlice(),
                                );
                                continue;
                            },
                        };

                        var argCount: u32 = 0;
                        var firstArg: Token = undefined;
                        while (true) {
                            const next = it.next();
                            if (next.is(.Eof)) {
                                try pp.comp.addDiagnostic(
                                    .{ .tag = .malformed_embed_limit, .loc = paramFirst.loc },
                                    paramFirst.expansionSlice(),
                                );
                                break;
                            }
                            if (next.is(.RParen)) break;

                            argCount += 1;
                            if (argCount == 1) firstArg = next;
                        }

                        if (std.mem.eql(u8, param, "limit")) {
                            if (argCount != 1) {
                                try pp.comp.addDiagnostic(
                                    .{ .tag = .malformed_embed_limit, .loc = paramFirst.loc },
                                    paramFirst.expansionSlice(),
                                );
                                continue;
                            }
                            if (firstArg.isNot(.PPNumber)) {
                                try pp.comp.addDiagnostic(
                                    .{ .tag = .malformed_embed_limit, .loc = paramFirst.loc },
                                    paramFirst.expansionSlice(),
                                );
                                continue;
                            }
                            _ = std.fmt.parseInt(u32, pp.expandedSlice(firstArg), 10) catch {
                                break :res notFound;
                            };
                        } else if (!std.mem.eql(u8, param, "prefix") and !std.mem.eql(u8, param, "suffix") and
                            !std.mem.eql(u8, param, "if_empty"))
                        {
                            break :res notFound;
                        }
                    }

                    const includeType: Compilation.IncludeType = switch (includeStr[0]) {
                        '"' => .Quotes,
                        '<' => .AngleBrackets,
                        else => unreachable,
                    };
                    const filename = includeStr[1 .. includeStr.len - 1];
                    const contents = (try pp.comp.findEmbed(filename, arg[0].loc.id, includeType, 1)) orelse
                        break :res notFound;

                    defer pp.comp.gpa.free(contents);
                    break :res if (contents.len != 0) "1\n" else "2\n";
                };

                const start = pp.comp.generatedBuffer.items.len;
                try pp.comp.generatedBuffer.appendSlice(pp.comp.gpa, result);
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
                    .Comment => continue,
                    else => {
                        invalid = tok;
                        break;
                    },
                };
                if (string == null and invalid == null) invalid = .{ .loc = loc, .id = .Eof };
                if (invalid) |some| try pp.comp.addDiagnostic(
                    .{ .tag = .pragma_operator_string_literal, .loc = some.loc },
                    some.expansionSlice(),
                ) else try pp.pragmaOperator(string.?, loc);
            },

            // GNU extension
            .Comma => {
                if (tokenIdx + 2 < funcMacro.tokens.len and funcMacro.tokens[tokenIdx + 1].is(.HashHash)) {
                    const hashhash = funcMacro.tokens[tokenIdx + 1];
                    var maybeVaArgs = funcMacro.tokens[tokenIdx + 2];
                    var consumed: usize = 2;
                    if (maybeVaArgs.is(.MacroWS) and tokenIdx + 3 < funcMacro.tokens.len) {
                        consumed = 3;
                        maybeVaArgs = funcMacro.tokens[tokenIdx + 3];
                    }

                    if (maybeVaArgs.is(.KeywordVarArgs)) {
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

fn expandVaOpt(
    pp: *Preprocessor,
    buf: *ExpandBuffer,
    raw: RawToken,
    shouldExpa: bool,
) !void {
    if (!shouldExpa) return;

    const source = pp.comp.getSource(raw.source);
    var lexer: Lexer = .{
        .buffer = source.buffer,
        .index = raw.start,
        .source = raw.source,
        .comp = pp.comp,
        .line = raw.line,
    };
    while (lexer.index < raw.end) {
        const tok = lexer.next();
        try buf.append(tokenFromRaw(tok));
    }
}

fn shouldExpand(tok: Token, macro: *Macro) bool {
    if (tok.loc.id == macro.loc.id and
        tok.loc.byteOffset >= macro.start and
        tok.loc.byteOffset <= macro.end)
        return false;

    for (tok.expansionSlice()) |loc| {
        if (loc.id == macro.loc.id and
            loc.byteOffset >= macro.start and
            loc.byteOffset <= macro.end)
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

            if (rawToken.is(.NewLine))
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
                if (isBuiltin)
                    try pp.errStr(nameToken, .missing_lparen_after_builtin, pp.expandedSlice(nameToken));
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
                try pp.comp.addDiagnostic(
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
            if (tok.isOneOf(.{ .WhiteSpace, .MacroWS })) continue;

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
            if (macroToken.is(.KeywordDefined) and evalCtx == .Expr) {
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
                            if (tok.isNot(.MacroWS)) break;
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
                        try pp.comp.addDiagnostic(
                            .{ .tag = .expected_at_least_arguments, .loc = buf.items[idx].loc, .extra = extra },
                            buf.items[idx].expansionSlice(),
                        );
                        idx += 1;
                        try pp.removeExpandedTokens(buf, idx, macroScanIdx - idx + 1, &movingEndIdx);
                        continue;
                    }

                    if (!macro.varArgs and argsCount != macro.params.len) {
                        try pp.comp.addDiagnostic(
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
                        if (tok.is(.KeywordDefined) and evalCtx == .Expr) {
                            try pp.comp.addDiagnostic(.{
                                .tag = .expansion_to_defined,
                                .loc = tok.loc,
                            }, tok.expansionSlice());
                        }
                        if (i < incrementIdxBy and (tok.is(.KeywordDefined) or pp.defines.contains(pp.expandedSlice(tok.*)))) {
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
        if (tok.is(.MacroWS) and !pp.preserveWhitespace) {
            Token.free(tok.expansionLocs, pp.gpa);
            continue;
        }
        if (tok.is(.Comment) and !pp.comp.langOpts.preserveCommentsInMacros) {
            Token.free(tok.expansionLocs, pp.gpa);
            continue;
        }
        tok.id.simplifyMacroKeywordExtra(true);
        pp.tokens.appendAssumeCapacity(tok.*);
    }

    if (pp.preserveWhitespace) {
        try pp.tokens.ensureUnusedCapacity(pp.gpa, pp.addExpansionNL);
        while (pp.addExpansionNL > 0) : (pp.addExpansionNL -= 1) {
            pp.tokens.appendAssumeCapacity(.{ .id = .NewLine, .loc = .{
                .id = lexer.source,
                .line = lexer.line,
            } });
        }
    }
}

/// Get expanded token source string.
pub fn expandedSlice(pp: *Preprocessor, tok: Token) []const u8 {
    return pp.expandedSliceExtra(tok, .SingleMacroWS);
}

pub fn expandedSliceExtra(
    pp: *Preprocessor,
    token: Token,
    macroWSHandling: enum { SingleMacroWS, PreserveMacroWS },
) []const u8 {
    if (token.id.lexeme()) |some|
        if (!(token.is(.MacroWS) and macroWSHandling == .PreserveMacroWS))
            return some;

    var lexer = Lexer{
        .buffer = pp.comp.getSource(token.loc.id).buffer,
        .comp = pp.comp,
        .index = token.loc.byteOffset,
        .source = .generated,
    };

    if (token.is(.MacroString)) {
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
        if ((pp.comp.langOpts.preserveCommentsInMacros and lhs.is(.Comment)) or !lhs.isOneOf(.{ .MacroWS, .Comment }))
            break lhs;
        Token.free(lhs.expansionLocs, pp.gpa);
    } else {
        return bufCopyTokens(lhsTokens, rhsTokens, &.{});
    };

    var rhsRest: u32 = 1;
    const rhs = for (rhsTokens) |rhs| {
        if ((pp.comp.langOpts.preserveCommentsInMacros and rhs.is(.Comment)) or !rhs.isOneOf(.{ .MacroWS, .Comment }))
            break rhs;
        rhsRest += 1;
    } else {
        return lhsTokens.appendAssumeCapacity(lhs);
    };
    defer Token.free(lhs.expansionLocs, pp.gpa);

    const start = pp.comp.generatedBuffer.items.len;
    const end = start + pp.expandedSlice(lhs).len + pp.expandedSlice(rhs).len;
    try pp.comp.generatedBuffer.ensureTotalCapacity(pp.gpa, end + 1); // +1 for a newline

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

    const pastedToken = lexer.nextNoWhiteSpaceComments();
    const next = lexer.nextNoWhiteSpaceComments();
    const pastedId = if (lhs.is(.PlaceMarker) and rhs.is(.PlaceMarker))
        .PlaceMarker
    else
        pastedToken.id;
    try lhsTokens.append(try pp.makeGeneratedToken(start, pastedId, lhs));
    if (!next.isOneOf(.{ .NewLine, .Eof })) {
        try pp.errStr(
            lhs,
            .pasting_formed_invalid,
            try pp.comp.diagnostics.arena.allocator().dupe(u8, pp.comp.generatedBuffer.items[start..end]),
        );
        try lhsTokens.append(tokenFromRaw(next));
    }
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
    const gop = try pp.defines.getOrPut(pp.gpa, name);
    if (gop.found_existing and !gop.value_ptr.eql(macro, pp)) {
        const tag: Diagnostics.Tag = if (gop.value_ptr.isBuiltin) .builtin_macro_redefined else .macro_redefined;
        const start = pp.comp.diagnostics.list.items.len;
        try pp.comp.addDiagnostic(.{
            .tag = tag,
            .loc = .{ .id = nameToken.source, .byteOffset = nameToken.start, .line = nameToken.line },
            .extra = .{ .str = name },
        }, &.{});

        if (!gop.value_ptr.isBuiltin and pp.comp.diagnostics.list.items.len != start) {
            try pp.comp.addDiagnostic(.{
                .tag = .previous_definition,
                .loc = gop.value_ptr.loc,
            }, &.{});
        }
    }

    if (pp.verbose)
        pp.verboseLog(nameToken, "macro {s} defined", .{name});

    gop.value_ptr.* = macro;
}

/// Handle #define directive
fn define(pp: *Preprocessor, lexer: *Lexer) Error!void {
    // get the macro name and validate.
    const macroName = lexer.nextNoWhiteSpace();
    if (macroName.is(.KeywordDefined)) {
        try pp.addError(macroName, .defined_as_macro_name);
        return skipToNewLine(lexer);
    }

    if (!macroName.id.isMacroIdentifier()) {
        try pp.addError(macroName, .macro_name_must_be_identifier);
        return skipToNewLine(lexer);
    }

    var macroNameTokenID = macroName.id;
    macroNameTokenID.simplifyMacroKeyword();
    switch (macroNameTokenID) {
        .Identifier, .ExtendedIdentifier => {},
        else => if (macroNameTokenID.isMacroIdentifier()) {
            try pp.addError(macroName, .keyword_macro);
        },
    }

    var first = lexer.next();
    switch (first.id) {
        .NewLine, .Eof => return pp.defineMacro(macroName, .{
            .params = &.{},
            .tokens = &.{},
            .varArgs = false,
            .isFunc = false,
            .loc = tokenFromRaw(macroName).loc,
            .start = 0,
            .end = 0,
        }),
        .WhiteSpace => first = lexer.next(),
        .LParen => return pp.defineFunc(lexer, macroName, first),
        else => try pp.addError(first, .whitespace_after_macro_name),
    }

    if (first.is(.HashHash)) {
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
                const next = lexer.nextNoWhiteSpaceComments();
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
            .Comment => if (pp.comp.langOpts.preserveCommentsInMacros) {
                if (needWS) {
                    needWS = false;
                    try pp.tokenBuffer.append(.{ .id = .MacroWS, .source = .generated });
                }
                try pp.tokenBuffer.append(token);
            },
            .WhiteSpace => needWS = true,
            .UnterminatedStringLiteral => {
                try pp.addError(token, .unterminated_string_literal_warning);
                try pp.tokenBuffer.append(token);
            },
            .UnterminatedCharLiteral => {
                try pp.addError(token, .unterminated_char_literal_warning);
                try pp.tokenBuffer.append(token);
            },
            .EmptyCharLiteral => {
                try pp.addError(token, .empty_char_literal_warning);
                try pp.tokenBuffer.append(token);
            },
            .UnterminatedComment => try pp.addError(token, .unterminated_comment),
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
        .loc = tokenFromRaw(macroName).loc,
        .start = first.start,
        .end = endIdx,
        .tokens = list,
        .params = undefined,
        .isFunc = false,
        .varArgs = false,
    });
}

/// Handle an #embed directive
/// embedDirective : ("FileName" | <Filename>) embedParam*
/// embedParam : Identifier (:: Identifier)? '(' <tokens> ')'
fn embed(pp: *Preprocessor, lexer: *Lexer) MacroError!void {
    const first = lexer.nextNoWhiteSpace();
    const fileNameToken = pp.findIncludeFilenameToken(first, lexer, .IgnoreTrailingTokens) catch |er| switch (er) {
        error.InvalidInclude => return,
        else => |e| return e,
    };

    // Check for empty filename.
    const tokSlice = pp.expandedSliceExtra(fileNameToken, .SingleMacroWS);
    if (tokSlice.len < 3) {
        try pp.addError(first, .empty_filename);
        return;
    }

    const filename = tokSlice[1 .. tokSlice.len - 1];
    const includeType: Compilation.IncludeType = switch (fileNameToken.id) {
        .StringLiteral => .Quotes,
        .MacroString => .AngleBrackets,
        else => unreachable,
    };

    const Range = struct {
        start: u32,
        end: u32,

        fn expand(optRange: ?@This(), pp_: *Preprocessor, lexer_: *Lexer) !void {
            const range = optRange orelse return;
            const slice = pp_.tokenBuffer.items[range.start..range.end];
            for (slice) |token|
                try pp_.expandMacro(lexer_, token);
        }
    };

    pp.tokenBuffer.items.len = 0;

    var limit: ?u32 = null;
    var prefix: ?Range = null;
    var suffix: ?Range = null;
    var ifEmpty: ?Range = null;
    while (true) {
        const paramFirst = lexer.nextNoWhiteSpace();
        switch (paramFirst.id) {
            .NewLine, .Eof => break,
            .Identifier => {},
            else => {
                try pp.addError(paramFirst, .malformed_embed_param);
                continue;
            },
        }

        const charTop = pp.charBuffer.items.len;
        defer pp.charBuffer.items.len = charTop;

        const maybeColon = lexer.colonColon();
        const param = switch (maybeColon.id) {
            .ColonColon => blk: {
                // vendor::param
                const param = lexer.nextNoWhiteSpace();
                if (param.id != .Identifier) {
                    try pp.addError(param, .malformed_embed_param);
                    continue;
                }
                const lparen = lexer.nextNoWhiteSpace();
                if (lparen.id != .LParen) {
                    try pp.addError(lparen, .malformed_embed_param);
                    continue;
                }
                try pp.charBuffer.appendSlice(Attribute.normalize(pp.getTokenSlice(paramFirst)));
                try pp.charBuffer.appendSlice("::");
                try pp.charBuffer.appendSlice(Attribute.normalize(pp.getTokenSlice(param)));
                break :blk pp.charBuffer.items;
            },
            .LParen => Attribute.normalize(pp.getTokenSlice(paramFirst)),
            else => {
                try pp.addError(maybeColon, .malformed_embed_param);
                continue;
            },
        };

        const start: u32 = @intCast(pp.tokenBuffer.items.len);
        while (true) {
            const next = lexer.nextNoWhiteSpace();
            if (next.id == .RParen) break;
            try pp.tokenBuffer.append(next);
        }
        const end: u32 = @intCast(pp.tokenBuffer.items.len);

        if (std.mem.eql(u8, param, "limit")) {
            if (limit != null) {
                try pp.errStr(tokenFromRaw(paramFirst), .duplicate_embed_param, "limit");
                continue;
            }

            if (start + 1 != end) {
                try pp.addError(paramFirst, .malformed_embed_limit);
                continue;
            }

            const limitToken = pp.tokenBuffer.items[start];
            if (limitToken.id != .PPNumber) {
                try pp.addError(paramFirst, .malformed_embed_limit);
                continue;
            }

            limit = std.fmt.parseInt(u32, pp.getTokenSlice(limitToken), 10) catch {
                try pp.addError(limitToken, .malformed_embed_limit);
                continue;
            };
            pp.tokenBuffer.items.len = start;
        } else if (std.mem.eql(u8, param, "prefix")) {
            if (prefix != null) {
                try pp.errStr(tokenFromRaw(paramFirst), .duplicate_embed_param, "prefix");
                continue;
            }
            prefix = .{ .start = start, .end = end };
        } else if (std.mem.eql(u8, param, "suffix")) {
            if (suffix != null) {
                try pp.errStr(tokenFromRaw(paramFirst), .duplicate_embed_param, "suffix");
                continue;
            }
            suffix = .{ .start = start, .end = end };
        } else if (std.mem.eql(u8, param, "if_empty")) {
            if (ifEmpty != null) {
                try pp.errStr(tokenFromRaw(paramFirst), .duplicate_embed_param, "if_empty");
                continue;
            }
            ifEmpty = .{ .start = start, .end = end };
        } else {
            try pp.errStr(
                tokenFromRaw(paramFirst),
                .unsupported_embed_param,
                try pp.comp.diagnostics.arena.allocator().dupe(u8, param),
            );
            pp.tokenBuffer.items.len = start;
        }
    }

    const embedBytes = (try pp.comp.findEmbed(filename, first.source, includeType, limit)) orelse
        return pp.fatal(first, "'{s}' not found", .{filename});
    defer pp.comp.gpa.free(embedBytes);

    try Range.expand(prefix, pp, lexer);
    if (embedBytes.len == 0) {
        try Range.expand(ifEmpty, pp, lexer);
        try Range.expand(suffix, pp, lexer);
        return;
    }

    try pp.tokens.ensureUnusedCapacity(pp.comp.gpa, 2 * embedBytes.len - 1); // N bytes and N-1 commas

    // TODO: We currently only support systems with CHAR_BIT == 8
    // If the target's CHAR_BIT is not 8, we need to write out correctly-sized embed_bytes
    // and correctly account for the target's endianness
    const writer = pp.comp.generatedBuffer.writer(pp.gpa);

    {
        const byte = embedBytes[0];
        const start = pp.comp.generatedBuffer.items.len;
        try writer.print("{d}", .{byte});
        pp.tokens.appendAssumeCapacity(try pp.makeGeneratedToken(start, .EmbedByte, fileNameToken));
    }

    for (embedBytes[1..]) |byte| {
        const start = pp.comp.generatedBuffer.items.len;
        try writer.print(",{d}", .{byte});
        pp.tokens.appendAssumeCapacity(.{ .id = .Comma, .loc = .{ .id = .generated, .byteOffset = @intCast(start) } });
        pp.tokens.appendAssumeCapacity(try pp.makeGeneratedToken(start + 1, .EmbedByte, fileNameToken));
    }
    try pp.comp.generatedBuffer.append(pp.gpa, '\n');

    try Range.expand(suffix, pp, lexer);
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
        if (token.is(.RParen)) break token.end;

        if (token.is(.Eof))
            return pp.addError(token, .unterminated_macro_param_list);

        if (token.is(.Ellipsis)) {
            varArgs = true;
            const rParen = lexer.nextNoWhiteSpace();
            if (rParen.isNot(.RParen)) {
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
        if (token.is(.Ellipsis)) {
            try pp.addError(token, .gnu_va_macro);
            gnuVarArgs = params.pop();
            const rParen = lexer.nextNoWhiteSpace();
            if (rParen.id != .RParen) {
                try pp.addError(lParen, .missing_paren_param_list);
                try pp.addError(lParen, .to_match_paren);
                return skipToNewLine(lexer);
            }
            break rParen.end;
        } else if (token.is(.RParen)) {
            break token.end;
        } else if (token.isNot(.Comma)) {
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
            .Comment => if (!pp.comp.langOpts.preserveCommentsInMacros) continue else {
                if (needWS) {
                    needWS = false;
                    try pp.tokenBuffer.append(.{ .id = .MacroWS, .source = .generated });
                }
                try pp.tokenBuffer.append(token);
            },
            .Hash => {
                if (token.isNot(.WhiteSpace) and needWS) {
                    needWS = false;
                    try pp.tokenBuffer.append(.{ .id = .MacroWS, .source = .generated });
                }
                const param = lexer.nextNoWhiteSpace();
                blk: {
                    if (varArgs and param.is(.KeywordVarArgs)) {
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
                const next = lexer.nextNoWhiteSpaceComments();
                if (next.isOneOf(.{ .NewLine, .Eof })) {
                    try pp.addError(token, .hash_hash_at_end);
                    return;
                }

                lexer.* = savedLexer;
                // convert the previous token to .macro_param_no_expand if it was .macro_param
                if (pp.tokenBuffer.items[pp.tokenBuffer.items.len - 1].is(.MacroParam)) {
                    pp.tokenBuffer.items[pp.tokenBuffer.items.len - 1].id = .MacroParamNoExpand;
                }
                try pp.tokenBuffer.append(token);
            },

            .UnterminatedStringLiteral => {
                try pp.addError(token, .unterminated_string_literal_warning);
                try pp.tokenBuffer.append(token);
            },

            .UnterminatedCharLiteral => {
                try pp.addError(token, .unterminated_char_literal_warning);
                try pp.tokenBuffer.append(token);
            },

            .EmptyCharLiteral => {
                try pp.addError(token, .empty_char_literal_warning);
                try pp.tokenBuffer.append(token);
            },

            .UnterminatedComment => try pp.addError(token, .unterminated_comment),

            else => {
                if (token.id != .WhiteSpace and needWS) {
                    needWS = false;
                    try pp.tokenBuffer.append(.{ .id = .MacroWS, .source = .generated });
                }

                if (varArgs and token.is(.KeywordVarArgs)) {
                    // do nothing
                } else if (varArgs and token.is(.KeywordVarOpt)) {
                    const optLparen = lexer.next();
                    if (!optLparen.is(.LParen)) {
                        try pp.addError(optLparen, .va_opt_lparen);
                        return skipToNewLine(lexer);
                    }
                    token.start = optLparen.end;

                    var parens: u32 = 0;
                    while (true) {
                        const optToken = lexer.next();
                        switch (optToken.id) {
                            .LParen => parens += 1,
                            .RParen => if (parens == 0) {
                                break;
                            } else {
                                parens -= 1;
                            },
                            .NewLine, .Eof => {
                                try pp.addError(optToken, .va_opt_rparen);
                                try pp.addError(optLparen, .to_match_paren);
                                return skipToNewLine(lexer);
                            },
                            .WhiteSpace => {},
                            else => token.end = optToken.end,
                        }
                    }
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
        .loc = tokenFromRaw(macroName).loc,
        .start = startIdx,
        .end = endIdx,
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
        try pp.comp.addDiagnostic(.{
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

    const tokenStart = pp.tokens.len;
    try pp.addIncludeStart(newSource);
    const eof = pp.preprocessExtra(newSource) catch |err| switch (err) {
        error.StopPreprocessing => {
            for (pp.tokens.items(.expansionLocs)[tokenStart..]) |loc| Token.free(loc, pp.gpa);
            pp.tokens.len = tokenStart;
            return;
        },
        else => |e| return e,
    };
    try eof.checkMsEof(newSource, pp.comp);

    if (pp.preserveWhitespace and pp.tokens.items(.id)[pp.tokens.len - 1] != .NewLine) {
        try pp.tokens.append(pp.gpa, .{ .id = .NewLine, .loc = .{ .id = lexer.source, .line = lexer.line } });
    }

    if (pp.linemarkers == .None) return;
    var next = first;
    while (true) {
        var tmp = lexer.*;
        next = tmp.nextNoWhiteSpace();
        if (next.id != .NewLine) break;
        lexer.* = tmp;
    }

    try pp.addIncludeResume(next.source, next.end, next.line);
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
    if (nameToken.isOneOf(.{ .NewLine, .Eof }))
        return;

    const name = pp.getTokenSlice(nameToken);

    try pp.tokens.append(pp.gpa, try pp.makePragmaToken(pragmaToken, operatorLoc, argLocs));
    const pragmaStart = @as(u32, @intCast(pp.tokens.len));

    const pragmaNameToken = try pp.makePragmaToken(nameToken, operatorLoc, argLocs);
    try pp.tokens.append(pp.gpa, pragmaNameToken);

    while (true) {
        const nextToken = lexer.next();
        if (nextToken.is(.WhiteSpace)) continue;
        if (nextToken.is(.Eof)) {
            try pp.tokens.append(pp.gpa, .{
                .id = .NewLine,
                .loc = .{ .id = .generated },
            });
            break;
        }
        try pp.tokens.append(pp.gpa, try pp.makePragmaToken(nextToken, operatorLoc, argLocs));
        if (nextToken.is(.NewLine))
            break;
    }

    if (pp.comp.getPragma(name)) |prag| unknown: {
        return prag.preprocessorCB(pp, pragmaStart) catch |err| switch (err) {
            error.UnknownPragma => break :unknown,
            else => |e| return e,
        };
    }
    return pp.comp.addDiagnostic(.{
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
    if (first.is(.AngleBracketLeft)) to_end: {
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

        try pp.comp.addDiagnostic(.{
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
    const tkSlice = pp.expandedSliceExtra(filenameToken, .SingleMacroWS);
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
    return (try pp.comp.findInclude(filename, first, includeType, which)) orelse
        pp.fatal(first, "'{s}' not found", .{filename});
}

fn printLinemarker(
    pp: *Preprocessor,
    w: anytype,
    lineNO: u32,
    source: Source,
    start_resume: enum(u8) { start, @"resume", none },
) !void {
    try w.writeByte('#');
    if (pp.linemarkers == .LineDirectives) try w.writeAll("line");
    // lineNo is 0 indexed.
    try w.print(" {d} \"", .{lineNO + 1});
    for (source.path) |byte| switch (byte) {
        '\n' => try w.writeAll("\\n"),
        '\r' => try w.writeAll("\\r"),
        '\t' => try w.writeAll("\\t"),
        '\\' => try w.writeAll("\\\\"),
        '"' => try w.writeAll("\\\""),
        ' ', '!', '#'...'&', '('...'[', ']'...'~' => try w.writeByte(byte),
        // Use hex escapes for any non-ASCII/unprintable characters.
        // This ensures that the parsed version of this string will end up
        // containing the same bytes as the input regardless of encoding.
        else => {
            try w.writeAll("\\x");
            try std.fmt.formatInt(byte, 16, .lower, .{ .width = 2, .fill = '0' }, w);
        },
    };
    try w.writeByte('"');
    if (pp.linemarkers == .NumericDirectives) {
        switch (start_resume) {
            .none => {},
            .start => try w.writeAll(" 1"),
            .@"resume" => try w.writeAll(" 2"),
        }

        switch (source.kind) {
            .User => {},
            .System => try w.writeAll(" 3"),
            .ExternCSystem => try w.writeAll(" 3 4"),
        }
    }
    try w.writeByte('\n');
}

// After how many empty lines are needed to replace them with linemarkers.
const CollapseNewlines = 8;

/// pretty print tokens and try to preserve whitespace
pub fn prettyPrintTokens(pp: *Preprocessor, w: anytype) !void {
    const tokenIds = pp.tokens.items(.id);

    var lastNewline = true;
    var i: u32 = 0;
    outer: while (true) : (i += 1) {
        var cur: Token = pp.tokens.get(i);
        switch (cur.id) {
            .Eof => {
                if (!lastNewline) try w.writeByte('\n');
                return;
            },

            .NewLine => {
                var newlines: u32 = 0;
                for (tokenIds[i..], i..) |id, j| {
                    if (id == .NewLine) {
                        newlines += 1;
                    } else if (id == .Eof) {
                        if (!lastNewline) try w.writeByte('\n');
                        return;
                    } else if (id != .WhiteSpace) {
                        if (pp.linemarkers == .None) {
                            if (newlines < 2) break;
                        } else if (newlines < CollapseNewlines) {
                            break;
                        }

                        i = @intCast((j - 1) - @intFromBool(tokenIds[j - 1] == .WhiteSpace));
                        if (!lastNewline) try w.writeAll("\n");

                        if (pp.linemarkers != .None) {
                            const next = pp.tokens.get(i);
                            const source = pp.comp.getSource(next.loc.id);
                            const lineCol = source.getLineCol(next.loc);
                            try printLinemarker(pp, w, lineCol.lineNO, source, .none);
                            lastNewline = true;
                        }
                        continue :outer;
                    }
                }
                lastNewline = true;
                try w.writeAll("\n");
            },
            .KeywordPragma => {
                const pragmaName = pp.expandedSlice(pp.tokens.get(i + 1));
                const endIdx = std.mem.indexOfScalarPos(TokenType, tokenIds, i, .NewLine) orelse i + 1;
                const pragmaLen = @as(u32, @intCast(endIdx)) - i;

                if (pp.comp.getPragma(pragmaName)) |prag| {
                    if (!prag.shouldPreserveTokens(pp, i + 1)) {
                        try w.writeByte('\n');
                        i += pragmaLen;
                        cur = pp.tokens.get(i);
                        continue;
                    }
                }
                try w.writeAll("#pragma");
                i += 1;
                while (true) : (i += 1) {
                    cur = pp.tokens.get(i);
                    if (cur.is(.NewLine)) {
                        try w.writeByte('\n');
                        lastNewline = true;
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
                    if (pp.linemarkers != .None) try w.writeByte('\n');
                    slice = slice[some + 1 ..];
                }
                for (slice) |_|
                    try w.writeByte(' ');
                lastNewline = false;
            },

            .IncludeStart => {
                const source = pp.comp.getSource(cur.loc.id);
                try pp.printLinemarker(w, 0, source, .start);
                lastNewline = true;
            },

            .IncludeResume => {
                const source = pp.comp.getSource(cur.loc.id);
                const lineCol = source.getLineCol(cur.loc);
                if (!lastNewline) try w.writeAll("\n");

                try pp.printLinemarker(w, lineCol.lineNO, source, .@"resume");
                lastNewline = true;
            },

            else => {
                const slice = pp.expandedSlice(cur);
                try w.writeAll(slice);
                lastNewline = false;
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
            assert(pp.linemarkers == .None);

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
    try Test.check(omit_once, "\nint x;\n\n");

    const omit_poison =
        \\#pragma GCC poison foobar
        \\
    ;
    try Test.check(omit_poison, "\n");
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
                .KeywordEmbed,
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
                .KeywordDefined, .KeywordVarArgs, .KeywordVarOpt, .KeywordEndIf => true,
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
                .KeywordInclude, .KeywordIncludeNext => try writer.print(template, .{ tokenID.lexeme().?, " \"bar.h\"" }),
                .KeywordDefine, .KeywordUndef => try writer.print(template, .{ tokenID.lexeme().?, " BAR" }),
                .KeywordIfndef, .KeywordIfdef => try writer.print(template, .{ tokenID.lexeme().?, " BAR\n#endif" }),
                else => try writer.print(template, .{ tokenID.lexeme().?, "" }),
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
