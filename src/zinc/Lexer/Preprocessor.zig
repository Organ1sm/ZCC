const std = @import("std");
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;

const Attribute = @import("Attribute.zig");
const Compilation = @import("../Basic/Compilation.zig");
const Error = Compilation.Error;
const SourceEpoch = Compilation.Environment.SourceEpoch;
const Diagnostics = @import("../Basic/Diagnostics.zig");
const Features = @import("Features.zig");
const HideSet = @import("HideSet.zig");
const Lexer = @import("Lexer.zig");
const Parser = @import("../Parser/Parser.zig");
const RawToken = @import("../Lexer/Lexer.zig").Token;
const Source = @import("../Basic/Source.zig");
const TextLiteral = @import("../Parser/TextLiteral.zig");
const TokenType = @import("../Basic/TokenType.zig").TokenType;
const Tree = @import("../AST/AST.zig");
const Token = Tree.Token;
const TokenWithExpansionLocs = Tree.TokenWithExpansionLocs;

const DefineMap = std.StringArrayHashMapUnmanaged(Macro);
const RawTokenList = std.ArrayList(RawToken);
const MaxIncludeDepth = 200;

const ExpansionEntry = struct {
    idx: Tree.TokenIndex,
    locs: [*]Source.Location,
};

const TokenState = struct {
    tokensLen: usize,
    expansionEntriesLen: usize,
};

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

pub const Macro = struct {
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

const Preprocessor = @This();

comp: *Compilation,
diagnostics: *Diagnostics,

arena: std.heap.ArenaAllocator,
defines: DefineMap = .empty,
/// Do not directly mutate this; must be kept in sync with `tokens`
expansionEntries: std.MultiArrayList(ExpansionEntry) = .empty,
/// Do not directly mutate this;
/// use addToken / addTokenAssumeCapacity / ensureTotalTokenCapacity / ensureUnusedTokenCapacity
tokens: Token.List = .empty,
tokenBuffer: RawTokenList = .empty,
charBuffer: std.ArrayList(u8) = .empty,
includeDepth: u8 = 0,
generatedLine: u32 = 1,
addExpansionNL: u32 = 0,
poisonedIdentifiers: std.StringHashMapUnmanaged(void) = .empty,
/// used to implement __COUNTER__ Macro
counter: u32 = 0,
expansionSourceLoc: Source.Location = undefined,
/// Counter that is incremented each time preprocess() is called
/// Can be used to distinguish multiple preprocessings of the same file
preprocessCount: u32 = 0,
/// Memory is retained to avoid allocation on every single token.
topExpansionBuffer: ExpandBuffer = .empty,
/// Map from Source.ID to macro name in the `#ifndef` condition which guards the source, if any
includeGuards: std.AutoHashMapUnmanaged(Source.ID, []const u8) = .empty,

/// Store `keyword-define` and `keyword-undef` tokens.
/// Used to implement preprocessor debug dump options
/// Must be false unless in -E mode (parser does not handle those token types)
storeMacroTokens: bool = false,

/// Dump current state to stderr
verbose: bool = false,
preserveWhitespace: bool = false,

/// linemarker tokens. Must be .none unless in -E mode (parser does not handle linemarkers)
linemarkers: LineMarkers = .None,

hideSet: HideSet,

/// Epoch used for __DATE__, __TIME__, and possibly __TIMESTAMP__
sourceEpoch: SourceEpoch,
mtimes: std.AutoHashMapUnmanaged(Source.ID, u64) = .empty,

pub const parse = Parser.parse;

pub const LineMarkers = enum {
    /// No linemarker tokens. Required setting if parser will run
    None,
    /// #line <num> "filename"
    LineDirectives,
    /// # <num> "filename" flags
    NumericDirectives,
};

pub fn addBuiltinMacro(pp: *Preprocessor, name: []const u8, isFunc: bool, comptime paramTokenTy: TokenType) !void {
    try pp.defines.putNoClobber(pp.comp.gpa, name, .{
        .params = &[1][]const u8{"X"},
        .tokens = &[1]RawToken{.{ .id = paramTokenTy, .source = .generated }},
        .varArgs = false,
        .isFunc = isFunc,
        .loc = .{ .id = .generated },
        .isBuiltin = true,
    });
}

pub fn addBuiltinMacros(pp: *Preprocessor) !void {
    try pp.addBuiltinMacro("__has_attribute", true, .MacroParamHasAttribute);
    try pp.addBuiltinMacro("__has_c_attribute", true, .MacroParamHasCAttribute);
    try pp.addBuiltinMacro("__has_declspec_attribute", true, .MacroParamHasDeclspecAttribute);
    try pp.addBuiltinMacro("__has_warning", true, .MacroParamHasWarning);
    try pp.addBuiltinMacro("__has_feature", true, .MacroParamHasFeature);
    try pp.addBuiltinMacro("__has_extension", true, .MacroParamHasExtension);
    try pp.addBuiltinMacro("__has_builtin", true, .MacroParamHasBuiltin);
    try pp.addBuiltinMacro("__has_include", true, .MacroParamHasInclude);
    try pp.addBuiltinMacro("__has_include_next", true, .MacroParamHasIncludeNext);
    try pp.addBuiltinMacro("__has_embed", true, .MacroParamHasEmbed);
    try pp.addBuiltinMacro("__is_identifier", true, .MacroParamIsIdentifier);
    try pp.addBuiltinMacro("_Pragma", true, .MacroParamPragmaOperator);

    if (pp.comp.langOpts.msExtensions) {
        try pp.addBuiltinMacro("__identifier", true, .MacroParamMsIdentifier);
        try pp.addBuiltinMacro("__pragma", true, .MacroParamMsPragma);
    }

    try pp.addBuiltinMacro("__FILE__", false, .MacroFile);
    try pp.addBuiltinMacro("__LINE__", false, .MacroLine);
    try pp.addBuiltinMacro("__COUNTER__", false, .MacroCounter);
    try pp.addBuiltinMacro("__DATE__", false, .MacroDate);
    try pp.addBuiltinMacro("__TIME__", false, .MacroTime);
    try pp.addBuiltinMacro("__TIMESTAMP__", false, .MacroTimestamp);
}

pub fn init(comp: *Compilation, sourceEpoch: SourceEpoch) Preprocessor {
    const pp = Preprocessor{
        .comp = comp,
        .diagnostics = comp.diagnostics,
        .arena = .init(comp.gpa),
        .hideSet = .{ .comp = comp },
        .sourceEpoch = sourceEpoch,
    };

    comp.pragmaEvent(.BeforePreprocess);
    return pp;
}

/// Initialize Preprocessor with builtin macros.
pub fn initDefault(comp: *Compilation) !Preprocessor {
    const sourceEpoch: SourceEpoch = comp.environment.sourceEpoch(comp.io) catch |er| switch (er) {
        error.InvalidEpoch, error.UnsupportedClock, error.Unexpected => blk: {
            const diagnostic: Diagnostic = .invalid_source_epoch;
            try comp.diagnostics.add(.{ .text = diagnostic.fmt, .kind = diagnostic.kind, .opt = diagnostic.opt, .location = null });
            break :blk .default;
        },
    };

    var pp = init(comp, sourceEpoch);
    errdefer pp.deinit();

    try pp.addBuiltinMacros();
    return pp;
}

pub fn deinit(pp: *Preprocessor) void {
    const gpa = pp.comp.gpa;

    pp.defines.deinit(gpa);
    pp.tokens.deinit(gpa);
    pp.arena.deinit();
    pp.tokenBuffer.deinit(gpa);
    pp.charBuffer.deinit(gpa);
    pp.poisonedIdentifiers.deinit(gpa);
    pp.topExpansionBuffer.deinit(gpa);
    pp.includeGuards.deinit(gpa);
    pp.hideSet.deinit();
    pp.mtimes.deinit(gpa);
    for (pp.expansionEntries.items(.locs)) |locs| TokenWithExpansionLocs.free(locs, gpa);
    pp.expansionEntries.deinit(gpa);

    pp.* = undefined;
}

/// Free buffers that are not needed after preprocessing
fn clearBuffers(pp: *Preprocessor) void {
    const gpa = pp.comp.gpa;
    pp.tokenBuffer.clearAndFree(gpa);
    pp.charBuffer.clearAndFree(gpa);
    pp.topExpansionBuffer.clearAndFree(gpa);
    pp.hideSet.clearAndFree();
}

fn mTime(pp: *Preprocessor, sourceID: Source.ID) !u64 {
    const gop = try pp.mtimes.getOrPut(pp.comp.gpa, sourceID);
    if (!gop.found_existing) {
        gop.value_ptr.* = pp.comp.getSourceMTimeUncached(sourceID) orelse 0;
    }
    return gop.value_ptr.*;
}

pub fn expansionSlice(pp: *Preprocessor, tok: Tree.TokenIndex) []Source.Location {
    const S = struct {
        fn orderTokenIndex(lhs: Tree.TokenIndex, rhs: Tree.TokenIndex) std.math.Order {
            return std.math.order(lhs, rhs);
        }
    };

    const indices = pp.expansionEntries.items(.idx);
    const idx = std.sort.binarySearch(Tree.TokenIndex, indices, tok, S.orderTokenIndex) orelse return &.{};
    const locs = pp.expansionEntries.items(.locs)[idx];
    var i: usize = 0;
    while (locs[i].id != .unused) : (i += 1) {}
    return locs[0..i];
}

/// Return the name of the #ifndef guard macro that starts a source, if any.
fn findIncludeGuard(pp: *Preprocessor, source: Source) ?[]const u8 {
    var lexer = Lexer{
        .buffer = source.buffer,
        .langOpts = pp.comp.langOpts,
        .source = source.id,
    };

    var hash = lexer.nextNoWs();
    while (hash.is(.NewLine))
        hash = lexer.nextNoWs();
    if (hash.isNot(.Hash)) return null;

    const ifndef = lexer.nextNoWs();
    if (ifndef.isNot(.KeywordIfndef)) return null;

    const guard = lexer.nextNoWs();
    if (guard.isNot(.Identifier)) return null;

    return pp.getTokenSlice(guard);
}

pub fn addIncludeStart(pp: *Preprocessor, source: Source) !void {
    if (pp.linemarkers == .None) return;
    try pp.addToken(.{
        .id = .IncludeStart,
        .loc = .{
            .id = source.id,
            .byteOffset = std.math.maxInt(u32),
            .line = 1,
        },
    });
}

pub fn addIncludeResume(pp: *Preprocessor, source: Source.ID, offset: u32, line: u32) !void {
    if (pp.linemarkers == .None) return;
    try pp.addToken(.{
        .id = .IncludeResume,
        .loc = .{
            .id = source,
            .byteOffset = offset,
            .line = line,
        },
    });
}

fn invalidTokenDiagnostic(tokType: TokenType) Diagnostic {
    return switch (tokType) {
        .UnterminatedStringLiteral => .unterminated_string_literal_warning,
        .UnterminatedCharLiteral => .unterminated_char_literal_warning,
        .EmptyCharLiteral => .empty_char_literal_warning,
        else => unreachable,
    };
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
    try pp.addIncludeResume(first.id, 0, 1);
    const eof = try pp.preprocess(first);
    try pp.addToken(eof);
    pp.clearBuffers();
}

/// Preprocess a source file, returns eof token.
pub fn preprocess(pp: *Preprocessor, source: Source) Error!TokenWithExpansionLocs {
    const eof = pp.preprocessExtra(source) catch |er| switch (er) {
        // This cannot occur in the main file and is handled in `include`.
        error.StopPreprocessing => unreachable,
        else => |e| return e,
    };
    try eof.checkMsEof(source, pp.comp);
    return eof;
}

fn preprocessExtra(pp: *Preprocessor, source: Source) MacroError!TokenWithExpansionLocs {
    const gpa = pp.comp.gpa;
    var guardName = pp.findIncludeGuard(source);

    pp.preprocessCount += 1;
    var lexer = Lexer{
        .buffer = source.buffer,
        .source = source.id,
        .langOpts = pp.comp.langOpts,
    };

    // Estimate how many new tokens this source will contain.
    const estimatedTokenCount = source.buffer.len / 8;
    try pp.ensureTotalTokenCapacity(pp.tokens.len + estimatedTokenCount);

    var ifContext: IfContext = .default;

    var startOfLine = true;
    while (true) {
        var token = lexer.next();
        switch (token.id) {
            .Hash => if (!startOfLine) try pp.addToken(tokenFromRaw(token)) else {
                const directive = lexer.nextNoWs();
                const directiveLoc: Source.Location = .{ .id = token.source, .byteOffset = directive.start, .line = directive.line };
                switch (directive.id) {
                    .KeywordError, .KeywordWarning => {
                        pp.topExpansionBuffer.items.len = 0;

                        const charTop = pp.charBuffer.items.len;
                        defer pp.charBuffer.items.len = charTop;

                        while (true) {
                            token = lexer.next();
                            if (token.isOneOf(.{ .NewLine, .Eof })) break;
                            if (token.is(.WhiteSpace)) token.id = .MacroWS;
                            try pp.topExpansionBuffer.append(gpa, tokenFromRaw(token));
                        }

                        try pp.stringify(pp.topExpansionBuffer.items);
                        const slice = pp.charBuffer.items[charTop + 1 .. pp.charBuffer.items.len - 2];

                        try pp.err(
                            directiveLoc,
                            if (directive.is(.KeywordError)) .error_directive else .warning_directive,
                            .{slice},
                        );
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
                        if (pp.storeMacroTokens) try pp.addToken(tokenFromRaw(directive));
                        _ = pp.defines.orderedRemove(macroName);
                        try pp.expectNewLine(&lexer);
                    },

                    .KeywordElIf => {
                        if (ifContext.level == 0) {
                            try pp.err(directive, .elif_without_if, .{});
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
                                try pp.err(directive, .elif_after_else, .{});
                                skipToNewLine(&lexer);
                            },
                        }
                    },

                    .KeywordElifdef => {
                        if (ifContext.level == 0) {
                            try pp.err(directive, .elifdef_without_if, .{});
                            _ = ifContext.increment();
                            ifContext.set(.untilElse);
                        } else if (ifContext.level == 1) {
                            guardName = null;
                        }

                        switch (ifContext.get()) {
                            .untilElse => {
                                const macroName = try pp.expectMacroName(&lexer);
                                if (macroName == null) {
                                    ifContext.set(.untilElse);
                                    try pp.skip(&lexer, .untilElse);
                                    if (pp.verbose)
                                        pp.verboseLog(directive, "entering then branch of #elifdef", .{});
                                } else {
                                    try pp.expectNewLine(&lexer);
                                    if (pp.defines.get(macroName.?) != null) {
                                        ifContext.set(.untilEndIf);
                                        if (pp.verbose)
                                            pp.verboseLog(directive, "entering else branch of #elifdef", .{});
                                    } else {
                                        ifContext.set(.untilElse);
                                        try pp.skip(&lexer, .untilElse);
                                        if (pp.verbose)
                                            pp.verboseLog(directive, "entering then branch of #elifdef", .{});
                                    }
                                }
                            },

                            .untilEndIf => try pp.skip(&lexer, .untilEndIf),
                            .untilEndIfSeenElse => {
                                try pp.err(directive, .elifdef_after_else, .{});
                                skipToNewLine(&lexer);
                            },
                        }
                    },

                    .KeywordElifndef => {
                        if (ifContext.level == 0) {
                            try pp.err(directive, .elifndef_without_if, .{});
                            _ = ifContext.increment();
                            ifContext.set(.untilElse);
                        } else if (ifContext.level == 1) {
                            guardName = null;
                        }

                        switch (ifContext.get()) {
                            .untilElse => {
                                const macroName = try pp.expectMacroName(&lexer);
                                if (macroName == null) {
                                    ifContext.set(.untilElse);
                                    try pp.skip(&lexer, .untilElse);
                                    if (pp.verbose)
                                        pp.verboseLog(directive, "entering then branch of #elifndef", .{});
                                } else {
                                    try pp.expectNewLine(&lexer);
                                    if (pp.defines.get(macroName.?) == null) {
                                        ifContext.set(.untilEndIf);
                                        if (pp.verbose)
                                            pp.verboseLog(directive, "entering else branch of #elifndef", .{});
                                    } else {
                                        ifContext.set(.untilElse);
                                        try pp.skip(&lexer, .untilElse);
                                        if (pp.verbose)
                                            pp.verboseLog(directive, "entering then branch of #elifndef", .{});
                                    }
                                }
                            },
                            .untilEndIf => try pp.skip(&lexer, .untilEndIf),
                            .untilEndIfSeenElse => {
                                try pp.err(directive, .elifndef_after_else, .{});
                                skipToNewLine(&lexer);
                            },
                        }
                    },

                    .KeywordElse => {
                        try pp.expectNewLine(&lexer);

                        if (ifContext.level == 0) {
                            try pp.err(directive, .else_without_if, .{});
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
                                try pp.err(directive, .else_after_else, .{});
                                skipToNewLine(&lexer);
                            },
                        }
                    },

                    .KeywordEndIf => {
                        try pp.expectNewLine(&lexer);
                        if (ifContext.level == 0) {
                            guardName = null;
                            try pp.err(directive, .else_without_if, .{});
                            continue;
                        } else if (ifContext.level == 1) {
                            const savedLexer = lexer;
                            defer lexer = savedLexer;

                            var next = lexer.nextNoWs();
                            while (next.is(.NewLine)) : (next = lexer.nextNoWs()) {}
                            if (next.isNot(.Eof)) guardName = null;
                        }
                        ifContext.decrement();
                    },

                    .KeywordDefine => try pp.define(&lexer, directive),
                    .KeywordInclude => {
                        try pp.include(&lexer, .First);
                        continue;
                    },
                    .KeywordIncludeNext => {
                        try pp.err(directiveLoc, .include_next, .{});
                        if (pp.includeDepth == 0) {
                            try pp.err(directiveLoc, .include_next_outside_header, .{});
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
                        const digits = lexer.nextNoWs();
                        if (digits.isNot(.PPNumber))
                            try pp.err(digits, .line_simple_digit, .{});

                        if (digits.isOneOf(.{ .Eof, .NewLine }))
                            continue;

                        const name = lexer.nextNoWs();
                        if (name.isOneOf(.{ .Eof, .NewLine }))
                            continue;

                        if (name.isNot(.StringLiteral))
                            try pp.err(name, .line_invalid_filename, .{});

                        try pp.expectNewLine(&lexer);
                    },

                    .PPNumber => {
                        // # number "file" flags
                        // TODO: validate that the pp_num token is solely digits
                        // if not, emit `GNU line marker directive requires a simple digit sequence`
                        const name = lexer.nextNoWs();
                        if (name.isOneOf(.{ .Eof, .NewLine })) continue;
                        if (name.isNot(.NewLine)) try pp.err(name, .line_invalid_filename, .{});

                        const flag1 = lexer.nextNoWs();
                        if (flag1.isOneOf(.{ .Eof, .NewLine })) continue;
                        const flag2 = lexer.nextNoWs();
                        if (flag2.isOneOf(.{ .Eof, .NewLine })) continue;
                        const flag3 = lexer.nextNoWs();
                        if (flag3.isOneOf(.{ .Eof, .NewLine })) continue;
                        const flag4 = lexer.nextNoWs();
                        if (flag4.isOneOf(.{ .Eof, .NewLine })) continue;
                        try pp.expectNewLine(&lexer);
                    },
                    .NewLine => {},
                    .Eof => {
                        if (ifContext.level != 0)
                            try pp.err(directive, .unterminated_conditional_directive, .{});
                        return tokenFromRaw(directive);
                    },
                    else => {
                        try pp.err(token, .invalid_preprocessing_directive, .{});
                        skipToNewLine(&lexer);
                    },
                }
                if (pp.preserveWhitespace) {
                    token.id = .NewLine;
                    try pp.addToken(tokenFromRaw(token));
                }
            },

            .WhiteSpace => if (pp.preserveWhitespace) try pp.addToken(tokenFromRaw(token)),
            .NewLine => {
                startOfLine = true;
                if (pp.preserveWhitespace)
                    try pp.addToken(tokenFromRaw(token));
            },

            .Eof => {
                if (ifContext.level != 0)
                    try pp.err(token, .unterminated_conditional_directive, .{});
                if (source.buffer.len > 0 and source.buffer[source.buffer.len - 1] != '\n')
                    try pp.err(token, .newline_eof, .{});
                if (guardName) |name| {
                    if (try pp.includeGuards.fetchPut(gpa, source.id, name)) |prev| {
                        assert(std.mem.eql(u8, name, prev.value));
                    }
                }
                return tokenFromRaw(token);
            },

            .UnterminatedStringLiteral, .UnterminatedCharLiteral, .EmptyCharLiteral => |tag| {
                startOfLine = false;
                try pp.err(token, invalidTokenDiagnostic(tag), .{});
                try pp.expandMacro(&lexer, token);
            },

            .UnterminatedComment => try pp.err(token, .unterminated_comment, .{}),

            else => {
                if (token.id.isMacroIdentifier() and pp.poisonedIdentifiers.get(pp.getTokenSlice(token)) != null)
                    try pp.err(token, .poisoned_identifier, .{});

                const tokenStr = lexer.buffer[token.start..token.end];
                if (std.mem.eql(u8, tokenStr, "__has_include")) {
                    token.id.simplifyMacroKeyword();
                    try pp.err(token, .preprocessing_directive_only, .{tokenStr});
                }

                // add the token to the buffer do any necessary expansions
                startOfLine = false;
                try pp.expandMacro(&lexer, token);
            },
        }
    }
}

/// Tokenize a file without any preprocessing, returns eof token.
pub fn tokenize(pp: *Preprocessor, source: Source) Error!TokenWithExpansionLocs {
    assert(pp.linemarkers == .None);
    assert(pp.preserveWhitespace == false);
    var tokenizer = Lexer{
        .buffer = source.buffer,
        .langOpts = pp.comp.langOpts,
        .source = source.id,
    };

    // Estimate how many new tokens this source will contain.
    const EstimatedTokenCount = source.buffer.len / 8;
    try pp.ensureTotalTokenCapacity(pp.tokens.len + EstimatedTokenCount);

    while (true) {
        const tok = tokenizer.next();
        if (tok.is(.Eof)) return tokenFromRaw(tok);
        try pp.addToken(tokenFromRaw(tok));
    }
}

/// Get raw token source string.
/// Returned slice is invalidated when comp.generatedBuffer is updated.
pub fn getTokenSlice(pp: *const Preprocessor, token: anytype) []const u8 {
    if (token.id.lexeme()) |some|
        return some;

    const source = pp.comp.getSource(token.source);
    return source.buffer[token.start..token.end];
}

/// Convert a token from the Lexer into a token used by the parser.
fn tokenFromRaw(raw: RawToken) TokenWithExpansionLocs {
    return .{
        .id = raw.id,
        .loc = .{
            .id = raw.source,
            .byteOffset = raw.start,
            .line = raw.line,
        },
    };
}

pub const Diagnostic = @import("Diagnostic.zig");

fn err(pp: *Preprocessor, location: anytype, diagnostic: Diagnostic, args: anytype) Compilation.Error!void {
    if (pp.diagnostics.effectiveKind(diagnostic) == .off) return;

    var sf = std.heap.stackFallback(1024, pp.comp.gpa);
    var allocating: std.Io.Writer.Allocating = .init(sf.get());
    defer allocating.deinit();

    Diagnostics.formatArgs(&allocating.writer, diagnostic.fmt, args) catch return error.OutOfMemory;
    try pp.diagnostics.addWithLocation(pp.comp, .{
        .kind = diagnostic.kind,
        .text = allocating.written(),
        .opt = diagnostic.opt,
        .extension = diagnostic.extension,
        .location = switch (@TypeOf(location)) {
            RawToken => (Source.Location{
                .id = location.source,
                .byteOffset = location.start,
                .line = location.line,
            }).expand(pp.comp),
            TokenWithExpansionLocs, *TokenWithExpansionLocs => location.loc.expand(pp.comp),
            Source.Location => location.expand(pp.comp),
            else => @compileError("invalid token type " ++ @typeName(@TypeOf(location))),
        },
    }, switch (@TypeOf(location)) {
        RawToken => &.{},
        TokenWithExpansionLocs, *TokenWithExpansionLocs => location.expansionSlice(),
        Source.Location => &.{},
        else => @compileError("invalid token type"),
    }, true);
}

fn fatal(pp: *Preprocessor, raw: RawToken, comptime fmt: []const u8, args: anytype) Compilation.Error {
    var sf = std.heap.stackFallback(1024, pp.comp.gpa);
    var allocating: std.Io.Writer.Allocating = .init(sf.get());
    defer allocating.deinit();

    Diagnostics.formatArgs(&allocating.writer, fmt, args) catch return error.OutOfMemory;
    try pp.diagnostics.add(.{
        .kind = .@"fatal error",
        .text = allocating.written(),
        .location = (Source.Location{
            .id = raw.source,
            .byteOffset = raw.start,
            .line = raw.line,
        }).expand(pp.comp),
    });
    unreachable;
}

fn fatalNotFound(pp: *Preprocessor, tok: TokenWithExpansionLocs, filename: []const u8) Compilation.Error {
    const old = pp.comp.diagnostics.state.fatalErrors;
    pp.comp.diagnostics.state.fatalErrors = true;
    defer pp.comp.diagnostics.state.fatalErrors = old;

    var sf = std.heap.stackFallback(1024, pp.comp.gpa);
    const allocator = sf.get();

    var buf: std.ArrayList(u8) = .empty;
    defer buf.deinit(allocator);

    try buf.print(allocator, "'{s}' not found", .{filename});
    try pp.diagnostics.addWithLocation(pp.comp, .{
        .kind = .@"fatal error",
        .text = buf.items,
        .location = tok.loc.expand(pp.comp),
    }, tok.expansionSlice(), true);
    unreachable; // should've returned FatalError
}

fn verboseLog(pp: *Preprocessor, raw: RawToken, comptime fmt: []const u8, args: anytype) void {
    @branchHint(.cold);
    const source = pp.comp.getSource(raw.source);
    const lineCol = source.getLineCol(.{ .byteOffset = raw.start, .line = raw.line, .id = raw.source });

    var stderrBuffer: [4096]u8 = undefined;
    var stderr = std.fs.File.stderr().writer(&stderrBuffer);
    const w = &stderr.interface;

    w.print("{s}:{d}:{d}: ", .{ source.path, lineCol.lineNo, lineCol.col }) catch return;
    w.print(fmt, args) catch return;
    w.writeByte('\n') catch return;
    w.writeAll(lineCol.line) catch return;
    w.writeByte('\n') catch return;
    w.flush() catch return;
}

/// Consume next token, error if it is not an identifier.
fn expectMacroName(pp: *Preprocessor, lexer: *Lexer) Error!?[]const u8 {
    const macroName = lexer.nextNoWs();
    if (!macroName.id.isMacroIdentifier()) {
        try pp.err(macroName, .macro_name_missing, .{});
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
        if (token.isOneOf(.{ .WhiteSpace, .Comment }))
            continue;

        if (!sentErr) {
            sentErr = true;
            try pp.err(token, .extra_tokens_directive_end, .{});
        }
    }
}

fn getTokenState(pp: *const Preprocessor) TokenState {
    return .{
        .tokensLen = pp.tokens.len,
        .expansionEntriesLen = pp.expansionEntries.len,
    };
}

fn restoreTokenState(pp: *Preprocessor, state: TokenState) void {
    pp.tokens.len = state.tokensLen;
    pp.expansionEntries.len = state.expansionEntriesLen;
}

/// Consume all tokens until a newline and parse the result into a boolean.
fn expr(pp: *Preprocessor, lexer: *Lexer) MacroError!bool {
    const gpa = pp.comp.gpa;
    const tokenState = pp.getTokenState();
    defer {
        for (pp.topExpansionBuffer.items) |token|
            TokenWithExpansionLocs.free(token.expansionLocs, gpa);
        pp.restoreTokenState(tokenState);
    }

    pp.topExpansionBuffer.items.len = 0;
    const eof = while (true) {
        const token = lexer.next();
        switch (token.id) {
            .NewLine, .Eof => break token,
            .WhiteSpace => if (pp.topExpansionBuffer.items.len == 0) continue,
            else => {},
        }

        try pp.topExpansionBuffer.append(gpa, tokenFromRaw(token));
    } else unreachable;

    if (pp.topExpansionBuffer.items.len != 0) {
        pp.expansionSourceLoc = pp.topExpansionBuffer.items[0].loc;
        pp.hideSet.clearRetainingCapacity();
        try pp.expandMacroExhaustive(lexer, &pp.topExpansionBuffer, 0, pp.topExpansionBuffer.items.len, false, .Expr);
    }

    for (pp.topExpansionBuffer.items) |token| {
        if (token.is(.MacroWS)) continue;
        if (!token.id.validPreprocessorExprStart()) {
            try pp.err(token, .invalid_preproc_expr_start, .{});

            return false;
        }
        break;
    } else {
        try pp.err(eof, .expected_value_in_expr, .{});
        return false;
    }

    // validate the tokens in the expression
    try pp.ensureUnusedTokenCapacity(pp.topExpansionBuffer.items.len);
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
                try pp.err(token, .string_literal_in_pp_expr, .{});
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
                try pp.err(token, .invalid_preproc_operator, .{});
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
                    try pp.err(token, .undefined_macro, .{pp.expandedSlice(token)});

                    if (i + 1 < pp.topExpansionBuffer.items.len and
                        pp.topExpansionBuffer.items[i + 1].is(.LParen))
                    {
                        try pp.err(token, .fn_macro_undefined, .{pp.expandedSlice(token)});
                        return false;
                    }

                    token.id = .Zero; // undefined macro
                }
            },
        }
        pp.addTokenAssumeCapacity(try pp.unescapeUcn(token));
    }

    try pp.addToken(.{
        .id = .Eof,
        .loc = tokenFromRaw(eof).loc,
    });

    var parser: Parser = .{
        .pp = pp,
        .comp = pp.comp,
        .diagnostics = pp.diagnostics,
        .tokenIds = pp.tokens.items(.id),
        .tokenIdx = @intCast(tokenState.tokensLen),
        .inMacro = true,

        .tree = undefined,
        .labels = undefined,
        .declBuffer = undefined,
        .listBuffer = undefined,
        .paramBuffer = undefined,
        .enumBuffer = undefined,
        .recordBuffer = undefined,
        .attrBuffer = undefined,
        .stringsIds = undefined,
    };
    defer parser.strings.deinit(gpa);

    return parser.macroExpr();
}

/// Turns macro_tok from .keyword_defined into .zero or .one depending on whether the argument is defined
/// Returns the number of tokens consumed
fn handleKeywordDefined(
    pp: *Preprocessor,
    macroToken: *TokenWithExpansionLocs,
    tokens: []const TokenWithExpansionLocs,
    eof: RawToken,
) !usize {
    assert(macroToken.is(.KeywordDefined));
    var it = TokenIterator.init(tokens);
    const first = it.nextNoWS() orelse {
        try pp.err(eof, .macro_name_missing, .{});
        return it.i;
    };
    switch (first.id) {
        .LParen => {},
        else => {
            if (!first.id.isMacroIdentifier())
                try pp.err(first, .macro_name_must_be_identifier, .{});
            macroToken.id = if (pp.defines.contains(pp.expandedSlice(first))) .One else .Zero;
            return it.i;
        },
    }
    const second = it.nextNoWS() orelse {
        try pp.err(eof, .macro_name_missing, .{});
        return it.i;
    };
    if (!second.id.isMacroIdentifier()) {
        try pp.err(second, .macro_name_must_be_identifier, .{});
        return it.i;
    }
    macroToken.id = if (pp.defines.contains(pp.expandedSlice(second))) .One else .Zero;

    const last = it.nextNoWS();
    if (last == null or last.?.isNot(.RParen)) {
        const tok = last orelse tokenFromRaw(eof);
        try pp.err(tok, .closing_paren, .{});
        try pp.err(first, .to_match_paren, .{});
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
            const hash = lexer.nextNoWs();

            if (hash.is(.NewLine)) continue;
            lineStart = false;
            if (hash.isNot(.Hash)) continue;

            const directive = lexer.nextNoWs();
            switch (directive.id) {
                .KeywordElse => {
                    if (ifsSeen != 0) continue;
                    if (cont == .untilEndIfSeenElse) {
                        try pp.err(directive, .else_after_else, .{});
                        continue;
                    }
                    lexer.* = savedLexer;
                    return;
                },

                .KeywordElIf => {
                    if (ifsSeen != 0 or cont == .untilEndIf) continue;
                    if (cont == .untilEndIfSeenElse) {
                        try pp.err(directive, .elif_after_else, .{});
                        continue;
                    }
                    lexer.* = savedLexer;
                    return;
                },

                .KeywordElifdef => {
                    if (ifsSeen != 0 or cont == .untilEndIf) continue;
                    if (cont == .untilEndIfSeenElse) {
                        try pp.err(directive, .elifdef_after_else, .{});
                        continue;
                    }
                    lexer.* = savedLexer;
                    return;
                },

                .KeywordElifndef => {
                    if (ifsSeen != 0 or cont == .untilEndIf) continue;
                    if (cont == .untilEndIfSeenElse) {
                        try pp.err(directive, .elifndef_after_else, .{});
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
                try pp.addToken(.{
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
        return pp.err(eof, .unterminated_conditional_directive, .{});
    }
}

// Skip until newline, ignore other tokens.
fn skipToNewLine(lexer: *Lexer) void {
    while (true) {
        const token = lexer.next();
        if (token.isOneOf(.{ .NewLine, .Eof })) return;
    }
}

const ExpandBuffer = std.ArrayList(TokenWithExpansionLocs);

fn removePlaceMarkers(gpa: Allocator, buf: *ExpandBuffer) void {
    var i: usize = buf.items.len -% 1;
    while (i < buf.items.len) : (i -%= 1) {
        if (buf.items[i].is(.PlaceMarker)) {
            const placemarker = buf.orderedRemove(i);
            TokenWithExpansionLocs.free(placemarker.expansionLocs, gpa);
        }
    }
}

const MacroArguments = std.ArrayList([]const TokenWithExpansionLocs);
fn deinitMacroArguments(gpa: Allocator, args: *MacroArguments) void {
    for (args.items) |item| {
        for (item) |token|
            TokenWithExpansionLocs.free(token.expansionLocs, gpa);
        gpa.free(item);
    }
    args.deinit(gpa);
}

fn expandObjMacro(pp: *Preprocessor, simpleMacro: *const Macro) Error!ExpandBuffer {
    const gpa = pp.comp.gpa;

    var buffer: ExpandBuffer = .empty;
    errdefer buffer.deinit(gpa);

    if (simpleMacro.tokens.len == 0) {
        try buffer.append(gpa, .{ .id = .PlaceMarker, .loc = .{ .id = .generated } });
        return buffer;
    }
    try buffer.ensureTotalCapacity(gpa, simpleMacro.tokens.len);

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
                try pp.pasteTokens(&buffer, &.{rhs});
            },
            .WhiteSpace => if (pp.preserveWhitespace) buffer.appendAssumeCapacity(token),
            .MacroFile => {
                const start = pp.comp.generatedBuffer.items.len;
                const source = pp.comp.getSource(pp.expansionSourceLoc.id);

                try pp.comp.generatedBuffer.print(gpa, "\"{f}\"\n", .{fmtEscapes(source.path)});

                buffer.appendAssumeCapacity(try pp.makeGeneratedToken(start, .StringLiteral, token));
            },
            .MacroLine => {
                const start = pp.comp.generatedBuffer.items.len;
                const source = pp.comp.getSource(pp.expansionSourceLoc.id);

                try pp.comp.generatedBuffer.print(gpa, "{d}\n", .{source.physicalLine(pp.expansionSourceLoc)});

                buffer.appendAssumeCapacity(try pp.makeGeneratedToken(start, .PPNumber, token));
            },
            .MacroCounter => {
                defer pp.counter += 1;
                const start = pp.comp.generatedBuffer.items.len;

                try pp.comp.generatedBuffer.print(gpa, "{d}\n", .{pp.counter});

                buffer.appendAssumeCapacity(try pp.makeGeneratedToken(start, .PPNumber, token));
            },
            .MacroDate, .MacroTime => {
                const start = pp.comp.generatedBuffer.items.len;
                const timestamp = switch (pp.sourceEpoch) {
                    .system, .provided => |ts| ts,
                };
                try pp.writeDateTimeStamp(.fromTokId(raw.id), timestamp);
                buffer.appendAssumeCapacity(try pp.makeGeneratedToken(start, .StringLiteral, token));
            },
            .MacroTimestamp => {
                const start = pp.comp.generatedBuffer.items.len;
                const timestamp = switch (pp.sourceEpoch) {
                    .provided => |ts| ts,
                    .system => try pp.mTime(pp.expansionSourceLoc.id),
                };

                try pp.writeDateTimeStamp(.fromTokId(raw.id), timestamp);
                buffer.appendAssumeCapacity(try pp.makeGeneratedToken(start, .StringLiteral, token));
            },
            else => buffer.appendAssumeCapacity(token),
        }
    }

    return buffer;
}

const DateTimeStampKind = enum {
    date,
    time,
    timestamp,

    fn fromTokId(tokenTy: TokenType) DateTimeStampKind {
        return switch (tokenTy) {
            .MacroDate => .date,
            .MacroTime => .time,
            .MacroTimestamp => .timestamp,
            else => unreachable,
        };
    }
};

fn writeDateTimeStamp(pp: *Preprocessor, kind: DateTimeStampKind, timestamp: u64) !void {
    std.debug.assert(std.time.epoch.Month.jan.numeric() == 1);

    const gpa = pp.comp.gpa;

    const epochSeconds = std.time.epoch.EpochSeconds{ .secs = timestamp };
    const epochDay = epochSeconds.getEpochDay();
    const daySeconds = epochSeconds.getDaySeconds();
    const yearDay = epochDay.calculateYearDay();
    const monthDay = yearDay.calculateMonthDay();

    const dayNames = [_][]const u8{ "Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun" };
    const monthNames = [_][]const u8{ "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" };
    const dayName = dayNames[@intCast((epochDay.day + 3) % 7)];
    const monthName = monthNames[monthDay.month.numeric() - 1];

    switch (kind) {
        .date => {
            try pp.comp.generatedBuffer.print(gpa, "\"{s} {d: >2} {d}\"", .{
                monthName,
                monthDay.day_index + 1,
                yearDay.year,
            });
        },
        .time => {
            try pp.comp.generatedBuffer.print(gpa, "\"{d:0>2}:{d:0>2}:{d:0>2}\"", .{
                daySeconds.getHoursIntoDay(),
                daySeconds.getMinutesIntoHour(),
                daySeconds.getSecondsIntoMinute(),
            });
        },
        .timestamp => {
            try pp.comp.generatedBuffer.print(gpa, "\"{s} {s} {d: >2} {d:0>2}:{d:0>2}:{d:0>2} {d}\"", .{
                dayName,
                monthName,
                monthDay.day_index + 1,
                daySeconds.getHoursIntoDay(),
                daySeconds.getMinutesIntoHour(),
                daySeconds.getSecondsIntoMinute(),
                yearDay.year,
            });
        },
    }
}

/// Join a possibly-parenthesized series of string literal tokens into a single string without
/// leading or trailing quotes. The returned slice is invalidated if pp.char_buf changes.
/// Returns error.ExpectedStringLiteral if parentheses are not balanced, a non-string-literal
/// is encountered, or if no string literals are encountered
/// TODO: destringize (replace all '\\' with a single `\` and all '\"' with a '"')
fn pasteStringsUnsafe(pp: *Preprocessor, toks: []const TokenWithExpansionLocs) ![]const u8 {
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
        try pp.charBuffer.appendSlice(pp.comp.gpa, str[1 .. str.len - 1]);
    }
    return pp.charBuffer.items[charBufferTop..];
}

/// Handle the _Pragma operator (implemented as a builtin macro)
fn pragmaOperator(pp: *Preprocessor, argToken: TokenWithExpansionLocs, operatorLoc: Source.Location) !void {
    const argSlice = pp.expandedSlice(argToken);
    const content = argSlice[1 .. argSlice.len - 1];
    const directive = "#pragma ";
    const gpa = pp.comp.gpa;

    pp.charBuffer.clearRetainingCapacity();
    const totalLen = directive.len + content.len + 1; // destringify can never grow the string, + 1 for newline
    try pp.charBuffer.ensureUnusedCapacity(gpa, totalLen);
    pp.charBuffer.appendSliceAssumeCapacity(directive);
    pp.destringify(content);
    pp.charBuffer.appendAssumeCapacity('\n');

    const start = pp.comp.generatedBuffer.items.len;
    try pp.comp.generatedBuffer.appendSlice(gpa, pp.charBuffer.items);
    var tempLexer = Lexer{
        .buffer = pp.comp.generatedBuffer.items,
        .langOpts = pp.comp.langOpts,
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

/// Handle Microsoft __pragma operator
fn msPragmaOperator(pp: *Preprocessor, pragmaToken: TokenWithExpansionLocs, args: []const TokenWithExpansionLocs) !void {
    if (args.len == 0) {
        try pp.err(pragmaToken, .unknown_pragma, .{});
        return;
    }

    {
        var copy = try pragmaToken.dupe(pp.comp.gpa);
        copy.id = .KeywordPragma;
        try pp.addToken(copy);
    }

    const pragmaStart: u32 = @intCast(pp.tokens.len);
    for (args) |tok| {
        switch (tok.id) {
            .MacroWS, .Comment => continue,
            else => try pp.addToken(try tok.dupe(pp.comp.gpa)),
        }
    }
    try pp.addToken(.{ .id = .NewLine, .loc = .{ .id = .generated } });

    const name = pp.expandedSlice(pp.tokens.get(pragmaStart));
    if (pp.comp.getPragma(name)) |prag| unknown: {
        return prag.preprocessorCB(pp, pragmaStart) catch |er| switch (er) {
            error.UnknownPragma => break :unknown,
            else => |e| return e,
        };
    }

    try pp.err(args[0], .unknown_pragma, .{});
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
fn stringify(pp: *Preprocessor, tokens: []const TokenWithExpansionLocs) !void {
    const gpa = pp.comp.gpa;
    try pp.charBuffer.append(gpa, '"');

    var wsState: enum { start, need, notNeeded } = .start;
    for (tokens) |tok| {
        if (tok.is(.MacroWS)) {
            if (wsState == .start) continue;
            wsState = .need;
            continue;
        }
        if (wsState == .need) try pp.charBuffer.append(gpa, ' ');
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
                try pp.charBuffer.appendSlice(gpa, "\\\"")
            else if (c == '\\' and isString)
                try pp.charBuffer.appendSlice(gpa, "\\\\")
            else
                try pp.charBuffer.append(gpa, c);
        }
    }

    try pp.charBuffer.ensureUnusedCapacity(gpa, 2);
    if (pp.charBuffer.items[pp.charBuffer.items.len - 1] != '\\') {
        pp.charBuffer.appendSliceAssumeCapacity("\"\n");
        return;
    }
    pp.charBuffer.appendAssumeCapacity('"');
    var lexer: Lexer = .{
        .buffer = pp.charBuffer.items,
        .index = 0,
        .source = .generated,
        .langOpts = pp.comp.langOpts,
        .line = 0,
    };

    const item = lexer.next();
    if (item.id == .UnterminatedStringLiteral) {
        const tok = tokens[tokens.len - 1];
        try pp.err(tok, .invalid_pp_stringify_escape, .{});
        pp.charBuffer.items.len -= 2; // erase unpaired backslash and appended end quote
        pp.charBuffer.appendAssumeCapacity('"');
    }

    pp.charBuffer.appendAssumeCapacity('\n');
}

fn reconstructIncludeString(
    pp: *Preprocessor,
    paramTokens: []const TokenWithExpansionLocs,
    embedArgs: ?*[]const TokenWithExpansionLocs,
    first: TokenWithExpansionLocs,
) !?[]const u8 {
    if (paramTokens.len == 0) {
        try pp.err(first, .expected_filename, .{});
        return null;
    }

    const charTop = pp.charBuffer.items.len;
    defer pp.charBuffer.items.len = charTop;

    // Trim leading/trailing whitespace
    var begin: usize = 0;
    var end = paramTokens.len;
    while (begin < end and paramTokens[begin].is(.MacroWS)) begin += 1;
    while (end > begin and paramTokens[end - 1].is(.MacroWS)) end -= 1;
    const params = paramTokens[begin..end];

    if (params.len == 0) {
        try pp.err(first, .expected_filename, .{});
        return null;
    }

    // no string pasting
    if (embedArgs == null and params[0].is(.StringLiteral) and params.len > 1) {
        try pp.err(params[1], .closing_paren, .{});
        return null;
    }

    for (params, 0..) |tok, i| {
        const str = pp.expandedSliceExtra(tok, .PreserveMacroWS);
        try pp.charBuffer.appendSlice(pp.comp.gpa, str);
        if (embedArgs) |some| {
            if ((i == 0 and tok.is(.StringLiteral)) or tok.is(.AngleBracketRight)) {
                some.* = params[i + 1 ..];
                break;
            }
        }
    }

    const includeStr = pp.charBuffer.items[charTop..];
    if (includeStr.len < 3) {
        if (includeStr.len == 0) {
            try pp.err(first, .expected_filename, .{});
            return null;
        }
        try pp.err(params[0], .empty_filename, .{});
        return null;
    }

    switch (includeStr[0]) {
        '<' => {
            if (includeStr[includeStr.len - 1] != '>') {
                // Ugly hack to find out where the '>' should go, since we don't have the closing ')' location
                var closing = params[0];
                closing.loc.byteOffset += @as(u32, @intCast(includeStr.len)) + 1;
                try pp.err(closing, .header_str_closing, .{});

                try pp.err(params[0], .header_str_match, .{});
                return null;
            }
            return includeStr;
        },

        '"' => return includeStr,

        else => {
            try pp.err(params[0], .expected_filename, .{});
            return null;
        },
    }
}

fn handleBuiltinMacro(
    pp: *Preprocessor,
    builtin: TokenType,
    paramTokens: []const TokenWithExpansionLocs,
    srcLoc: Source.Location,
) Error!bool {
    switch (builtin) {
        .MacroParamHasAttribute,
        .MacroParamHasDeclspecAttribute,
        .MacroParamHasFeature,
        .MacroParamHasExtension,
        .MacroParamHasBuiltin,
        => {
            var invalid: ?TokenWithExpansionLocs = null;
            var identifier: ?TokenWithExpansionLocs = null;
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
                try pp.err(some, .feature_check_requires_identifier, .{});
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
                .MacroParamHasBuiltin => pp.comp.hasBuiltin(identifierStr),
                else => unreachable,
            };
        },

        .MacroParamHasWarning => {
            const actualParam = pp.pasteStringsUnsafe(paramTokens) catch |er| switch (er) {
                error.ExpectedStringLiteral => {
                    try pp.err(paramTokens[0], .expected_str_literal_in, .{"__has_warning"});
                    return false;
                },
                else => |e| return e,
            };

            if (!std.mem.startsWith(u8, actualParam, "-W")) {
                try pp.err(paramTokens[0], .malformed_warning_check, .{"__has_warning"});
                return false;
            }

            const warningName = actualParam[2..];
            return Diagnostics.warningExists(warningName);
        },

        .MacroParamIsIdentifier => {
            var invalid: ?TokenWithExpansionLocs = null;
            var identifier: ?TokenWithExpansionLocs = null;
            for (paramTokens) |tok| switch (tok.id) {
                .MacroWS => continue,
                .Comment => continue,
                else => {
                    if (identifier) |_| invalid = tok else identifier = tok;
                },
            };
            if (identifier == null and invalid == null) invalid = .{ .id = .Eof, .loc = srcLoc };
            if (invalid) |some| {
                try pp.err(some, .builtin_missing_r_paren, .{"builtin feature-check macro"});
                return false;
            }

            const id = identifier.?.id;
            return id == .Identifier or id == .ExtendedIdentifier;
        },

        .MacroParamHasInclude, .MacroParamHasIncludeNext => {
            const includeStr = (try pp.reconstructIncludeString(paramTokens, null, paramTokens[0])) orelse return false;
            const includeType: Compilation.IncludeType = switch (includeStr[0]) {
                '"' => .Quotes,
                '<' => .AngleBrackets,
                else => unreachable,
            };
            const filename = includeStr[1 .. includeStr.len - 1];

            if (builtin == .MacroParamHasInclude or pp.includeDepth == 0) {
                if (builtin == .MacroParamHasIncludeNext)
                    try pp.err(srcLoc, .include_next_outside_header, .{});
                return pp.comp.hasInclude(filename, srcLoc.id, includeType, .First);
            }
            return pp.comp.hasInclude(filename, srcLoc.id, includeType, .Next);
        },

        else => unreachable,
    }
}

/// Treat whitespace-only paste arguments as empty
fn getPasteArgs(args: []const TokenWithExpansionLocs) []const TokenWithExpansionLocs {
    for (args) |tok| {
        if (tok.id != .MacroWS) return args;
    }
    return &[1]TokenWithExpansionLocs{.{
        .id = .PlaceMarker,
        .loc = .{ .id = .generated, .byteOffset = 0, .line = 0 },
    }};
}

fn expandFuncMacro(
    pp: *Preprocessor,
    macroToken: TokenWithExpansionLocs,
    funcMacro: *const Macro,
    args: *const MacroArguments,
    expandedArgs: *const MacroArguments,
    hideSetArg: HideSet.Index,
) MacroError!ExpandBuffer {
    const gpa = pp.comp.gpa;
    var hideset = hideSetArg;

    var buf: ExpandBuffer = .empty;
    errdefer buf.deinit(gpa);

    try buf.ensureTotalCapacity(gpa, funcMacro.tokens.len);

    var expandedVarArguments: ExpandBuffer = .empty;
    var varArguments: ExpandBuffer = .empty;

    defer {
        expandedVarArguments.deinit(gpa);
        varArguments.deinit(gpa);
    }

    if (funcMacro.varArgs) {
        var i: usize = funcMacro.params.len;
        while (i < expandedArgs.items.len) : (i += 1) {
            try varArguments.appendSlice(gpa, args.items[i]);
            try expandedVarArguments.appendSlice(gpa, expandedArgs.items[i]);
            if (i != expandedArgs.items.len - 1) {
                const comma: TokenWithExpansionLocs = .{
                    .id = .Comma,
                    .loc = .{ .id = .generated },
                };
                try varArguments.append(gpa, comma);
                try expandedVarArguments.append(gpa, comma);
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

                    var vaoptBuffer: ExpandBuffer = .empty;
                    defer vaoptBuffer.deinit(gpa);

                    const next = switch (rawNext.id) {
                        .MacroWS => continue,
                        .HashHash => continue,
                        .Comment => if (!pp.comp.langOpts.preserveCommentsInMacros)
                            continue
                        else
                            &[1]TokenWithExpansionLocs{tokenFromRaw(rawNext)},
                        .MacroParam, .MacroParamNoExpand => getPasteArgs(args.items[rawNext.end]),
                        .KeywordVarArgs => varArguments.items,
                        .KeywordVarOpt => blk: {
                            try pp.expandVaOpt(&vaoptBuffer, rawNext, varArguments.items.len != 0);
                            if (varArguments.items.len == 0) break;
                            break :blk vaoptBuffer.items;
                        },
                        else => &[1]TokenWithExpansionLocs{tokenFromRaw(rawNext)},
                    };
                    try pp.pasteTokens(&buf, next);
                    if (next.len != 0) break;
                }
            },

            .MacroParamNoExpand => {
                if (tokenIdx + 1 < funcMacro.tokens.len and funcMacro.tokens[tokenIdx + 1].id == .HashHash)
                    hideset = pp.hideSet.get(tokenFromRaw(funcMacro.tokens[tokenIdx + 1]).loc);

                const slice = getPasteArgs(args.items[raw.end]);
                const rawLoc = Source.Location{ .id = raw.source, .byteOffset = raw.start, .line = raw.line };
                try bufCopyTokens(gpa, &buf, slice, &.{rawLoc});
            },

            .MacroParam => {
                if (tokenIdx + 1 < funcMacro.tokens.len and funcMacro.tokens[tokenIdx + 1].id == .HashHash)
                    hideset = pp.hideSet.get(tokenFromRaw(funcMacro.tokens[tokenIdx + 1]).loc);

                const arg = expandedArgs.items[raw.end];
                const rawLoc = Source.Location{ .id = raw.source, .byteOffset = raw.start, .line = raw.line };
                try bufCopyTokens(gpa, &buf, arg, &.{rawLoc});
            },

            .KeywordVarArgs => {
                const rawLoc = Source.Location{ .id = raw.source, .byteOffset = raw.start, .line = raw.line };
                try bufCopyTokens(gpa, &buf, expandedVarArguments.items, &.{rawLoc});
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
                try pp.comp.generatedBuffer.appendSlice(gpa, pp.charBuffer.items);

                try buf.append(gpa, try pp.makeGeneratedToken(start, .StringLiteral, tokenFromRaw(raw)));
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
                    try pp.err(macroToken, .expected_arguments, .{ 1, 0 });
                    try buf.append(gpa, .{ .id = .Zero, .loc = macroToken.loc });
                    break :blk false;
                } else try pp.handleBuiltinMacro(raw.id, arg, macroToken.loc);

                const start = pp.comp.generatedBuffer.items.len;
                try pp.comp.generatedBuffer.print(gpa, "{}\n", .{@intFromBool(result)});
                try buf.append(gpa, try pp.makeGeneratedToken(start, .PPNumber, tokenFromRaw(raw)));
            },

            .MacroParamHasCAttribute => {
                const arg = expandedArgs.items[0];
                const notFound = "0\n";
                const result = if (arg.len == 0) blk: {
                    try pp.err(macroToken, .expected_arguments, .{ 1, 0 });
                    break :blk notFound;
                } else res: {
                    var invalid: ?TokenWithExpansionLocs = null;
                    var vendorIdent: ?TokenWithExpansionLocs = null;
                    var coloncolon: ?TokenWithExpansionLocs = null;
                    var attrIdent: ?TokenWithExpansionLocs = null;
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
                        invalid = .{ .id = .Eof, .loc = macroToken.loc };
                    }

                    if (invalid) |some| {
                        try pp.err(some, .feature_check_requires_identifier, .{});
                        break :res notFound;
                    }

                    if (vendorIdent) |some| {
                        const vendorStr = pp.expandedSlice(some);
                        const attrStr = pp.expandedSlice(attrIdent.?);
                        const exists = Attribute.fromString(.gnu, vendorStr, attrStr) != null;

                        const start = pp.comp.generatedBuffer.items.len;
                        try pp.comp.generatedBuffer.appendSlice(gpa, if (exists) "1\n" else "0\n");
                        try buf.append(gpa, try pp.makeGeneratedToken(start, .PPNumber, tokenFromRaw(raw)));
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
                try pp.comp.generatedBuffer.appendSlice(gpa, result);
                try buf.append(gpa, try pp.makeGeneratedToken(start, .PPNumber, tokenFromRaw(raw)));
            },

            .MacroParamHasEmbed => {
                const arg = expandedArgs.items[0];
                const notFound = "0\n";
                const result = if (arg.len == 0) blk: {
                    try pp.err(macroToken, .expected_arguments, .{ 1, 0 });
                    break :blk notFound;
                } else res: {
                    var embedArgs: []const TokenWithExpansionLocs = &.{};
                    const includeStr = (try pp.reconstructIncludeString(arg, &embedArgs, arg[0])) orelse
                        break :res notFound;

                    var prev = tokenFromRaw(raw);
                    prev.id = .Eof;
                    var it: struct {
                        i: u32 = 0,
                        slice: []const TokenWithExpansionLocs,
                        prev: TokenWithExpansionLocs,
                        fn next(it: *@This()) TokenWithExpansionLocs {
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
                            try pp.err(paramFirst, .malformed_embed_param, .{});
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
                                    try pp.err(param, .malformed_embed_param, .{});
                                    continue;
                                }

                                const lparen = it.next();
                                if (lparen.isNot(.LParen)) {
                                    try pp.err(lparen, .malformed_embed_param, .{});
                                    continue;
                                }
                                break :blk "doesn't exist";
                            },
                            .LParen => Attribute.normalize(pp.expandedSlice(paramFirst)),
                            else => {
                                try pp.err(maybeColon, .malformed_embed_param, .{});
                                continue;
                            },
                        };

                        var argCount: u32 = 0;
                        var firstArg: TokenWithExpansionLocs = undefined;
                        while (true) {
                            const next = it.next();
                            if (next.is(.Eof)) {
                                try pp.err(paramFirst, .malformed_embed_limit, .{});
                                break;
                            }
                            if (next.is(.RParen)) break;

                            argCount += 1;
                            if (argCount == 1) firstArg = next;
                        }

                        if (std.mem.eql(u8, param, "limit")) {
                            if (argCount != 1) {
                                try pp.err(paramFirst, .malformed_embed_limit, .{});
                                continue;
                            }
                            if (firstArg.isNot(.PPNumber)) {
                                try pp.err(paramFirst, .malformed_embed_limit, .{});
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
                    const contents = (try pp.comp.findEmbed(filename, arg[0].loc.id, includeType, .limited(1))) orelse
                        break :res notFound;

                    defer gpa.free(contents);
                    break :res if (contents.len != 0) "1\n" else "2\n";
                };

                const start = pp.comp.generatedBuffer.items.len;
                try pp.comp.generatedBuffer.appendSlice(gpa, result);
                try buf.append(gpa, try pp.makeGeneratedToken(start, .PPNumber, tokenFromRaw(raw)));
            },

            .MacroParamPragmaOperator => {
                // Clang and GCC require exactly one token (so, no parentheses or string pasting)
                // even though their error messages indicate otherwise. Ours is slightly more
                // descriptive.
                var invalid: ?TokenWithExpansionLocs = null;
                var string: ?TokenWithExpansionLocs = null;
                for (expandedArgs.items[0]) |tok| {
                    switch (tok.id) {
                        .StringLiteral => {
                            if (string) |_| {
                                invalid = tok;
                                break;
                            }
                            string = tok;
                        },
                        .MacroWS => continue,
                        .Comment => continue,
                        else => {
                            invalid = tok;
                            break;
                        },
                    }
                }
                if (string == null and invalid == null) invalid = macroToken;
                if (invalid) |some|
                    try pp.err(some, .pragma_operator_string_literal, .{})
                else
                    try pp.pragmaOperator(string.?, macroToken.loc);
            },

            .MacroParamMsIdentifier => blk: {
                // Expect '__identifier' '(' macro-identifier ')'
                var ident: ?TokenWithExpansionLocs = null;
                for (expandedArgs.items[0]) |tok| {
                    switch (tok.id) {
                        .MacroWS => continue,
                        .Comment => continue,
                        else => {},
                    }
                    if (ident) |_| {
                        try pp.err(tok, .builtin_missing_r_paren, .{"identifier"});
                        break :blk;
                    } else if (tok.id.isMacroIdentifier()) {
                        ident = tok;
                    } else {
                        try pp.err(tok, .cannot_convert_to_identifier, .{tok.id.symbol()});
                        break :blk;
                    }
                }
                if (ident) |*some| {
                    some.id = .Identifier;
                    try buf.append(gpa, some.*);
                } else {
                    try pp.err(macroToken, .expected_identifier, .{});
                }
            },

            .MacroParamMsPragma => try pp.msPragmaOperator(macroToken, expandedArgs.items[0]),

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
                            try pp.err(hashhash, .comma_deletion_va_args, .{});
                        } else if (funcMacro.params.len == 0 and expandedArgs.items.len == 1 and expandedArgs.items[0].len == 0) {
                            // Ambiguous whether this is "empty __VA_ARGS__" or "__VA_ARGS__ omitted"
                            if (pp.comp.langOpts.standard.isGNU()) {
                                // GNU standard, drop the comma
                                try pp.err(hashhash, .comma_deletion_va_args, .{});
                            } else {
                                // C standard, retain the comma
                                try buf.append(gpa, tokenFromRaw(raw));
                            }
                        } else {
                            try buf.append(gpa, tokenFromRaw(raw));
                            if (expandedVarArguments.items.len > 0 or varArguments.items.len == funcMacro.params.len) {
                                try pp.err(hashhash, .comma_deletion_va_args, .{});
                            }
                            const rawLoc = Source.Location{
                                .id = maybeVaArgs.source,
                                .byteOffset = maybeVaArgs.start,
                                .line = maybeVaArgs.line,
                            };
                            try bufCopyTokens(gpa, &buf, expandedVarArguments.items, &.{rawLoc});
                        }
                        continue;
                    }
                }
                // Regular comma, no token pasting with __VA_ARGS__
                try buf.append(gpa, tokenFromRaw(raw));
            },
            else => try buf.append(gpa, tokenFromRaw(raw)),
        }
    }
    removePlaceMarkers(gpa, &buf);

    const macroExpansionLocs = macroToken.expansionSlice();
    for (buf.items) |*tok| {
        try tok.addExpansionLocation(gpa, &.{macroToken.loc});
        try tok.addExpansionLocation(gpa, macroExpansionLocs);
        const tokenHideList = pp.hideSet.get(tok.loc);
        const newHideList = try pp.hideSet.@"union"(tokenHideList, hideset);
        try pp.hideSet.put(tok.loc, newHideList);
    }

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
        .langOpts = pp.comp.langOpts,
        .line = raw.line,
    };
    while (lexer.index < raw.end) {
        const tok = lexer.next();
        try buf.append(pp.comp.gpa, tokenFromRaw(tok));
    }
}

fn bufCopyTokens(
    gpa: Allocator,
    buf: *ExpandBuffer,
    tokens: []const TokenWithExpansionLocs,
    src: []const Source.Location,
) !void {
    try buf.ensureUnusedCapacity(gpa, tokens.len);
    for (tokens) |tok| {
        var copy = try tok.dupe(gpa);
        errdefer TokenWithExpansionLocs.free(copy.expansionLocs, gpa);
        try copy.addExpansionLocation(gpa, src);
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
) Error!TokenWithExpansionLocs {
    startIdx.* += 1;
    if (startIdx.* == buf.items.len and startIdx.* >= endIdx.*) {
        if (extendbuffer) {
            const rawToken = lexer.next();
            if (rawToken.id.isMacroIdentifier() and pp.poisonedIdentifiers.get(pp.getTokenSlice(rawToken)) != null)
                try pp.err(rawToken, .poisoned_identifier, .{});

            if (rawToken.is(.NewLine))
                pp.addExpansionNL += 1;

            const newToken = tokenFromRaw(rawToken);
            endIdx.* += 1;
            try buf.append(pp.comp.gpa, newToken);
            return newToken;
        } else {
            return .{ .id = .Eof, .loc = .{ .id = .generated } };
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
    rparen: *TokenWithExpansionLocs,
) !MacroArguments {
    const gpa = pp.comp.gpa;
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
                    try pp.err(nameToken, .missing_lparen_after_builtin, .{pp.expandedSlice(nameToken)});
                // Not a macro function call, go over normal identifier, rewind
                lexer.* = savedLexer;
                endIdx.* = oldEnd;
                return error.MissLParen;
            },
        }
    }

    // collect the arguments.
    var parens: u32 = 0;
    var args: MacroArguments = .empty;
    errdefer deinitMacroArguments(gpa, &args);

    var curArgument: std.ArrayList(TokenWithExpansionLocs) = .empty;
    defer curArgument.deinit(gpa);

    while (true) {
        var tok = try nextBufToken(pp, lexer, buf, startIdx, endIdx, extendBuffer);
        tok.flags.isMacroArg = true;
        switch (tok.id) {
            .Comma => {
                if (parens == 0) {
                    const owned = try curArgument.toOwnedSlice(gpa);
                    errdefer gpa.free(owned);
                    try args.append(gpa, owned);
                } else {
                    const duped = try tok.dupe(gpa);
                    errdefer TokenWithExpansionLocs.free(duped.expansionLocs, gpa);
                    try curArgument.append(gpa, duped);
                }
            },

            .LParen => {
                const duped = try tok.dupe(gpa);
                errdefer TokenWithExpansionLocs.free(duped.expansionLocs, gpa);
                try curArgument.append(gpa, duped);
                parens += 1;
            },

            .RParen => {
                if (parens == 0) {
                    const owned = try curArgument.toOwnedSlice(gpa);
                    errdefer gpa.free(owned);
                    try args.append(gpa, owned);
                    rparen.* = tok;
                    break;
                } else {
                    const duped = try tok.dupe(gpa);
                    errdefer TokenWithExpansionLocs.free(duped.expansionLocs, gpa);
                    try curArgument.append(gpa, duped);
                    parens -= 1;
                }
            },

            .Eof => {
                {
                    const owned = try curArgument.toOwnedSlice(gpa);
                    errdefer gpa.free(owned);
                    try args.append(gpa, owned);
                }

                lexer.* = savedLexer;
                try pp.err(nameToken, .unterminated_macro_arg_list, .{});

                return error.Unterminated;
            },

            .NewLine, .WhiteSpace => {
                try curArgument.append(gpa, .{ .id = .MacroWS, .loc = tok.loc });
            },

            else => {
                const duped = try tok.dupe(gpa);
                errdefer TokenWithExpansionLocs.free(duped.expansionLocs, gpa);
                try curArgument.append(gpa, duped);
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
    const gpa = pp.comp.gpa;
    for (buf.items[start .. start + len]) |tok|
        TokenWithExpansionLocs.free(tok.expansionLocs, gpa);
    try buf.replaceRange(gpa, start, len, &.{});
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
    toks: []const TokenWithExpansionLocs,
    i: usize,

    fn init(toks: []const TokenWithExpansionLocs) TokenIterator {
        return .{ .toks = toks, .i = 0 };
    }

    fn nextNoWS(self: *TokenIterator) ?TokenWithExpansionLocs {
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
    const gpa = pp.comp.gpa;
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

            if (!macroToken.id.isMacroIdentifier() or macroToken.flags.expansionDisabled) {
                idx += 1;
                continue;
            }

            const expanded = pp.expandedSlice(macroToken);
            const macro = pp.defines.getPtr(expanded) orelse {
                idx += 1;
                continue;
            };
            const macroHideList = pp.hideSet.get(macroToken.loc);
            if (pp.hideSet.contains(macroHideList, expanded)) {
                idx += 1;
                continue;
            }

            macroHandler: {
                if (macro.isFunc) {
                    var rparen: TokenWithExpansionLocs = undefined;
                    var macroScanIdx = idx;
                    // to be saved in case this doesn't turn out to be a call
                    var args = pp.collectMacroFuncArguments(
                        lexer,
                        buf,
                        &macroScanIdx,
                        &movingEndIdx,
                        extendBuffer,
                        macro.isBuiltin,
                        &rparen,
                    ) catch |er| switch (er) {
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

                    assert(rparen.id == .RParen);

                    var freeArgExpansionLocs = false;
                    defer {
                        for (args.items) |item| {
                            if (freeArgExpansionLocs) {
                                for (item) |tok|
                                    TokenWithExpansionLocs.free(tok.expansionLocs, gpa);
                            }
                            gpa.free(item);
                        }
                        args.deinit(gpa);
                    }

                    const rparenHideList = pp.hideSet.get(rparen.loc);
                    var hs = try pp.hideSet.intersection(macroHideList, rparenHideList);
                    hs = try pp.hideSet.prepend(macroToken.loc, hs);

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
                    if (macro.varArgs and argsCount < macro.params.len) {
                        freeArgExpansionLocs = true;
                        try pp.err(buf.items[idx], .expected_at_least_arguments, .{ macro.params.len, argsCount });
                        idx += 1;
                        try pp.removeExpandedTokens(buf, idx, macroScanIdx - idx + 1, &movingEndIdx);
                        continue;
                    }

                    if (!macro.varArgs and argsCount != macro.params.len) {
                        freeArgExpansionLocs = true;
                        try pp.err(buf.items[idx], .expected_arguments, .{ macro.params.len, argsCount });
                        idx += 1;
                        try pp.removeExpandedTokens(buf, idx, macroScanIdx - idx + 1, &movingEndIdx);
                        continue;
                    }

                    //std.debug.print("Expanding func: {s}\n", .{pp.expandedSlice(buf.items[idx])});
                    var expandedArgs: MacroArguments = .empty;
                    defer deinitMacroArguments(gpa, &expandedArgs);
                    try expandedArgs.ensureTotalCapacity(gpa, args.items.len);
                    for (args.items) |arg| {
                        var expandBuffer: ExpandBuffer = .empty;
                        errdefer expandBuffer.deinit(gpa);
                        try expandBuffer.appendSlice(gpa, arg);

                        try pp.expandMacroExhaustive(lexer, &expandBuffer, 0, expandBuffer.items.len, false, evalCtx);
                        expandedArgs.appendAssumeCapacity(try expandBuffer.toOwnedSlice(gpa));
                    }

                    var res = try pp.expandFuncMacro(macroToken, macro, &args, &expandedArgs, hs);
                    defer res.deinit(gpa);
                    const tokensAdded = res.items.len;

                    const tokensRemoved = macroScanIdx - idx + 1;
                    for (buf.items[idx .. idx + tokensRemoved]) |tok|
                        TokenWithExpansionLocs.free(tok.expansionLocs, gpa);
                    try buf.replaceRange(gpa, idx, tokensRemoved, res.items);

                    movingEndIdx += tokensAdded;
                    // Overflow here means that we encountered an unterminated argument list
                    // while expanding the body of this macro.
                    movingEndIdx -|= tokensRemoved;
                    idx += tokensAdded;
                    doRescan = true;
                } else {
                    var res = try pp.expandObjMacro(macro);
                    defer res.deinit(gpa);

                    const hs = try pp.hideSet.prepend(macroToken.loc, macroHideList);
                    const macroExpansionLocs = macroToken.expansionSlice();
                    var incrementIdxBy = res.items.len;
                    for (res.items, 0..) |*tok, i| {
                        tok.flags.isMacroArg = macroToken.flags.isMacroArg;
                        try tok.addExpansionLocation(gpa, &.{macroToken.loc});
                        try tok.addExpansionLocation(gpa, macroExpansionLocs);

                        const tokenHideList = pp.hideSet.get(tok.loc);
                        const newHideList = try pp.hideSet.@"union"(tokenHideList, hs);
                        try pp.hideSet.put(tok.loc, newHideList);

                        if (tok.is(.KeywordDefined) and evalCtx == .Expr) {
                            if (macro.isFunc)
                                try pp.err(tok, .expansion_to_defined_func, .{})
                            else
                                try pp.err(tok, .expansion_to_defined_obj, .{});
                        }
                        if (i < incrementIdxBy and (tok.is(.KeywordDefined) or pp.defines.contains(pp.expandedSlice(tok.*)))) {
                            incrementIdxBy = i;
                        }
                    }

                    TokenWithExpansionLocs.free(buf.items[idx].expansionLocs, gpa);

                    try buf.replaceRange(gpa, idx, 1, res.items);
                    idx += incrementIdxBy;
                    movingEndIdx = movingEndIdx + res.items.len - 1;
                    doRescan = true;
                }
            }
            if (idx - startIdx == advanceIdx + 1 and !doRescan) {
                advanceIdx += 1;
            }
        } // end of replacement phase
    } // end of scanning phase

    // trim excess buffer
    for (buf.items[movingEndIdx..]) |item|
        TokenWithExpansionLocs.free(item.expansionLocs, gpa);
    buf.items.len = movingEndIdx;
}

fn unescapeUcn(pp: *Preprocessor, tok: TokenWithExpansionLocs) !TokenWithExpansionLocs {
    const comp = pp.comp;
    const gpa = comp.gpa;
    switch (tok.id) {
        .IncompleteUcn => {
            @branchHint(.cold);
            try pp.err(tok, .incomplete_ucn, .{});
        },
        .ExtendedIdentifier => {
            @branchHint(.cold);
            const identifier = pp.expandedSlice(tok);
            if (std.mem.indexOfScalar(u8, identifier, '\\') != null) {
                @branchHint(.cold);
                const start = comp.generatedBuffer.items.len;
                try comp.generatedBuffer.ensureUnusedCapacity(gpa, identifier.len + 1);
                var identifierParser: TextLiteral.Parser = .{
                    .comp = comp,
                    .literal = pp.expandedSlice(tok), // re-expand since previous line may have caused a reallocation, invalidating `identifier`
                    .kind = .utf8,
                    .maxCodepoint = 0x10ffff,
                    .loc = tok.loc,
                    .expansionLocs = tok.expansionSlice(),
                    .diagnoseIncorrectEncoding = false,
                };

                while (try identifierParser.next()) |decoded| {
                    switch (decoded) {
                        .value => unreachable, // validated by tokenizer
                        .codepoint => |c| {
                            var buf: [4]u8 = undefined;
                            const written = std.unicode.utf8Encode(c, &buf) catch unreachable;
                            comp.generatedBuffer.appendSliceAssumeCapacity(buf[0..written]);
                        },
                        .improperlyEncoded => |bytes| {
                            comp.generatedBuffer.appendSliceAssumeCapacity(bytes);
                        },
                        .utf8Text => |view| {
                            comp.generatedBuffer.appendSliceAssumeCapacity(view.bytes);
                        },
                    }
                }

                comp.generatedBuffer.appendAssumeCapacity('\n');
                defer TokenWithExpansionLocs.free(tok.expansionLocs, gpa);
                return pp.makeGeneratedToken(start, .ExtendedIdentifier, tok);
            }
        },
        else => {},
    }
    return tok;
}

/// Try to expand a macro after a possible candidate has been read from the `lexer`
/// into the `raw` token passed as argument
fn expandMacro(pp: *Preprocessor, lexer: *Lexer, raw: RawToken) MacroError!void {
    var sourceToken = tokenFromRaw(raw);
    if (!raw.id.isMacroIdentifier()) {
        sourceToken.id.simplifyMacroKeyword();
        return pp.addToken(sourceToken);
    }

    const gpa = pp.comp.gpa;

    pp.topExpansionBuffer.items.len = 0;
    try pp.topExpansionBuffer.append(gpa, sourceToken);
    pp.expansionSourceLoc = sourceToken.loc;

    pp.hideSet.clearRetainingCapacity();
    try pp.expandMacroExhaustive(lexer, &pp.topExpansionBuffer, 0, 1, true, .NonExpr);
    try pp.ensureUnusedTokenCapacity(pp.topExpansionBuffer.items.len);
    for (pp.topExpansionBuffer.items) |*tok| {
        if (tok.is(.MacroWS) and !pp.preserveWhitespace) {
            TokenWithExpansionLocs.free(tok.expansionLocs, gpa);
            continue;
        }
        if (tok.is(.Comment) and !pp.comp.langOpts.preserveCommentsInMacros) {
            TokenWithExpansionLocs.free(tok.expansionLocs, gpa);
            continue;
        }
        if (tok.id == .PlaceMarker) {
            TokenWithExpansionLocs.free(tok.expansionLocs, gpa);
            continue;
        }
        tok.id.simplifyMacroKeywordExtra(true);
        pp.addTokenAssumeCapacity(try pp.unescapeUcn(tok.*));
    }

    if (pp.preserveWhitespace) {
        try pp.ensureUnusedTokenCapacity(pp.addExpansionNL);
        while (pp.addExpansionNL > 0) : (pp.addExpansionNL -= 1) {
            pp.addTokenAssumeCapacity(.{ .id = .NewLine, .loc = .{
                .id = lexer.source,
                .line = lexer.line,
            } });
        }
    }
}

/// Get expanded token source string.
pub fn expandedSlice(pp: *const Preprocessor, tok: anytype) []const u8 {
    return pp.expandedSliceExtra(tok, .SingleMacroWS);
}

pub fn expandedSliceExtra(
    pp: *const Preprocessor,
    token: anytype,
    macroWSHandling: enum { SingleMacroWS, PreserveMacroWS },
) []const u8 {
    if (token.id.lexeme()) |some|
        if (!(token.is(.MacroWS) and macroWSHandling == .PreserveMacroWS))
            return some;

    var lexer = Lexer{
        .buffer = pp.comp.getSource(token.loc.id).buffer,
        .langOpts = pp.comp.langOpts,
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
fn pasteTokens(pp: *Preprocessor, lhsTokens: *ExpandBuffer, rhsTokens: []const TokenWithExpansionLocs) Error!void {
    const gpa = pp.comp.gpa;
    const lhs = while (lhsTokens.pop()) |lhs| {
        if ((pp.comp.langOpts.preserveCommentsInMacros and lhs.is(.Comment)) or !lhs.isOneOf(.{ .MacroWS, .Comment }))
            break lhs;
        TokenWithExpansionLocs.free(lhs.expansionLocs, gpa);
    } else {
        return bufCopyTokens(gpa, lhsTokens, rhsTokens, &.{});
    };

    var rhsRest: u32 = 1;
    const rhs = for (rhsTokens) |rhs| {
        if ((pp.comp.langOpts.preserveCommentsInMacros and rhs.is(.Comment)) or !rhs.isOneOf(.{ .MacroWS, .Comment }))
            break rhs;
        rhsRest += 1;
    } else {
        return lhsTokens.appendAssumeCapacity(lhs);
    };
    defer TokenWithExpansionLocs.free(lhs.expansionLocs, gpa);

    const start = pp.comp.generatedBuffer.items.len;
    const end = start + pp.expandedSlice(lhs).len + pp.expandedSlice(rhs).len;
    try pp.comp.generatedBuffer.ensureTotalCapacity(gpa, end + 1); // +1 for a newline

    // We cannot use the same slices here since they might be invalidated by `ensureCapacity`
    pp.comp.generatedBuffer.appendSliceAssumeCapacity(pp.expandedSlice(lhs));
    pp.comp.generatedBuffer.appendSliceAssumeCapacity(pp.expandedSlice(rhs));
    pp.comp.generatedBuffer.appendAssumeCapacity('\n');

    // Try to tokenize the result.
    var lexer = Lexer{
        .buffer = pp.comp.generatedBuffer.items,
        .langOpts = pp.comp.langOpts,
        .index = @intCast(start),
        .source = .generated,
    };

    const pastedToken = lexer.nextNoWsComments();
    const next = lexer.nextNoWsComments();
    const pastedId = if (lhs.is(.PlaceMarker) and rhs.is(.PlaceMarker))
        .PlaceMarker
    else
        pastedToken.id;
    try lhsTokens.append(gpa, try pp.makeGeneratedToken(start, pastedId, lhs));
    if (!next.isOneOf(.{ .NewLine, .Eof })) {
        try pp.err(lhs, .pasting_formed_invalid, .{pp.comp.generatedBuffer.items[start..end]});
        try lhsTokens.append(gpa, tokenFromRaw(next));
    }
    try bufCopyTokens(gpa, lhsTokens, rhsTokens[rhsRest..], &.{});
}

fn makeGeneratedToken(pp: *Preprocessor, start: usize, id: TokenType, source: TokenWithExpansionLocs) !TokenWithExpansionLocs {
    const gpa = pp.comp.gpa;
    var pastedToken = TokenWithExpansionLocs{
        .id = id,
        .loc = .{
            .id = .generated,
            .byteOffset = @intCast(start),
            .line = pp.generatedLine,
        },
    };
    pp.generatedLine += 1;
    try pastedToken.addExpansionLocation(gpa, &.{source.loc});
    try pastedToken.addExpansionLocation(gpa, source.expansionSlice());
    return pastedToken;
}

/// Defines a new macro and warns  if it  is a duplicate
fn defineMacro(pp: *Preprocessor, defineToken: RawToken, nameToken: TokenWithExpansionLocs, macro: Macro) Error!void {
    const name = pp.expandedSlice(nameToken);
    const gop = try pp.defines.getOrPut(pp.comp.gpa, name);
    if (gop.found_existing and !gop.value_ptr.eql(macro, pp)) {
        const loc = nameToken.loc;
        const prevTotal = pp.diagnostics.total;
        if (gop.value_ptr.isBuiltin) {
            try pp.err(loc, .builtin_macro_redefined, .{});
        } else {
            try pp.err(loc, .macro_redefined, .{name});
        }

        if (!gop.value_ptr.isBuiltin and pp.diagnostics.total != prevTotal) {
            try pp.err(gop.value_ptr.loc, .previous_definition, .{});
        }
    }

    if (pp.verbose) {
        const raw: RawToken = .{
            .id = nameToken.id,
            .source = nameToken.loc.id,
            .start = nameToken.loc.byteOffset,
            .end = nameToken.loc.byteOffset,
            .line = nameToken.loc.line,
        };
        pp.verboseLog(raw, "macro {s} defined", .{name});
    }

    if (pp.storeMacroTokens)
        try pp.addToken(tokenFromRaw(defineToken));

    gop.value_ptr.* = macro;
}

/// Handle #define directive
fn define(pp: *Preprocessor, lexer: *Lexer, defineToken: RawToken) Error!void {
    const gpa = pp.comp.gpa;
    // get the macro name and validate.
    const escapeMacroName = lexer.nextNoWs();
    if (escapeMacroName.is(.KeywordDefined)) {
        try pp.err(escapeMacroName, .defined_as_macro_name, .{});
        return skipToNewLine(lexer);
    }

    const macroName = try pp.unescapeUcn(tokenFromRaw(escapeMacroName));
    defer TokenWithExpansionLocs.free(macroName.expansionLocs, gpa);

    if (!macroName.id.isMacroIdentifier()) {
        try pp.err(macroName, .macro_name_must_be_identifier, .{});
        return skipToNewLine(lexer);
    }

    var macroNameTokenID = macroName.id;
    macroNameTokenID.simplifyMacroKeyword();
    switch (macroNameTokenID) {
        .Identifier, .ExtendedIdentifier => {},
        else => if (macroNameTokenID.isMacroIdentifier() and
            !std.mem.eql(u8, pp.comp.getSource(lexer.source).path, "<builtin>"))
        {
            try pp.err(macroName, .keyword_macro, .{});
        },
    }

    var first = lexer.next();
    switch (first.id) {
        .NewLine, .Eof => return pp.defineMacro(defineToken, macroName, .{
            .params = &.{},
            .tokens = &.{},
            .varArgs = false,
            .isFunc = false,
            .loc = macroName.loc,
        }),
        .WhiteSpace => first = lexer.next(),
        .LParen => return pp.defineFunc(lexer, defineToken, macroName, first),
        else => try pp.err(first, .whitespace_after_macro_name, .{}),
    }

    if (first.is(.HashHash)) {
        try pp.err(first, .hash_hash_at_start, .{});
        return skipToNewLine(lexer);
    }
    first.id.simplifyMacroKeyword();

    // Clear the token buffer
    // Safe to use since we can only be in one directive at a time.
    pp.tokenBuffer.items.len = 0;

    var needWS = false;
    // Collect the token body and validate any ## found.
    var token = first;
    while (true) {
        token.id.simplifyMacroKeyword();
        switch (token.id) {
            .HashHash => {
                const next = lexer.nextNoWsComments();
                switch (next.id) {
                    .NewLine, .Eof => {
                        try pp.err(token, .hash_hash_at_end, .{});
                        return;
                    },
                    .HashHash => {
                        try pp.err(next, .hash_hash_at_end, .{});
                        return;
                    },
                    else => {},
                }
                try pp.tokenBuffer.append(gpa, token);
                try pp.tokenBuffer.append(gpa, next);
            },
            .NewLine, .Eof => break,
            .Comment => if (pp.comp.langOpts.preserveCommentsInMacros) {
                if (needWS) {
                    needWS = false;
                    try pp.tokenBuffer.append(gpa, .{ .id = .MacroWS, .source = .generated });
                }
                try pp.tokenBuffer.append(gpa, token);
            },
            .WhiteSpace => needWS = true,
            .UnterminatedStringLiteral, .UnterminatedCharLiteral, .EmptyCharLiteral => |tag| {
                try pp.err(token, invalidTokenDiagnostic(tag), .{});
                try pp.tokenBuffer.append(gpa, token);
            },
            .UnterminatedComment => try pp.err(token, .unterminated_comment, .{}),

            else => {
                if (token.is(.IncompleteUcn)) {
                    @branchHint(.cold);
                    try pp.err(token, .incomplete_ucn, .{});
                }
                if (token.id != .WhiteSpace and needWS) {
                    needWS = false;
                    try pp.tokenBuffer.append(gpa, .{ .id = .MacroWS, .source = .generated });
                }
                try pp.tokenBuffer.append(gpa, token);
            },
        }
        token = lexer.next();
    }

    const list = try pp.arena.allocator().dupe(RawToken, pp.tokenBuffer.items);
    try pp.defineMacro(defineToken, macroName, .{
        .loc = macroName.loc,
        .tokens = list,
        .params = &.{},
        .isFunc = false,
        .varArgs = false,
    });
}

/// Handle an #embed directive
/// embedDirective : ("FileName" | <Filename>) embedParam*
/// embedParam : Identifier (:: Identifier)? '(' <tokens> ')'
fn embed(pp: *Preprocessor, lexer: *Lexer) MacroError!void {
    const gpa = pp.comp.gpa;
    const first = lexer.nextNoWs();
    const fileNameToken = pp.findIncludeFilenameToken(first, lexer, .IgnoreTrailingTokens) catch |er| switch (er) {
        error.InvalidInclude => return,
        else => |e| return e,
    };
    defer TokenWithExpansionLocs.free(fileNameToken.expansionLocs, gpa);

    // Check for empty filename.
    const tokSlice = pp.expandedSliceExtra(fileNameToken, .SingleMacroWS);
    if (tokSlice.len < 3) {
        try pp.err(first, .empty_filename, .{});
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

    var limit: ?std.Io.Limit = null;
    var prefix: ?Range = null;
    var suffix: ?Range = null;
    var ifEmpty: ?Range = null;
    while (true) {
        const paramFirst = lexer.nextNoWs();
        switch (paramFirst.id) {
            .NewLine, .Eof => break,
            .Identifier => {},
            else => {
                try pp.err(paramFirst, .malformed_embed_param, .{});
                continue;
            },
        }

        const charTop = pp.charBuffer.items.len;
        defer pp.charBuffer.items.len = charTop;

        const maybeColon = lexer.colonColon();
        const param = switch (maybeColon.id) {
            .ColonColon => blk: {
                // vendor::param
                const param = lexer.nextNoWs();
                if (param.id != .Identifier) {
                    try pp.err(param, .malformed_embed_param, .{});
                    continue;
                }
                const lparen = lexer.nextNoWs();
                if (lparen.id != .LParen) {
                    try pp.err(lparen, .malformed_embed_param, .{});
                    continue;
                }

                const vendor = Attribute.normalize(pp.getTokenSlice(paramFirst));
                const paramName = Attribute.normalize(pp.getTokenSlice(param));

                try pp.charBuffer.ensureUnusedCapacity(gpa, vendor.len + 2 + paramName.len);
                pp.charBuffer.appendSliceAssumeCapacity(Attribute.normalize(vendor));
                pp.charBuffer.appendSliceAssumeCapacity("::");
                pp.charBuffer.appendSliceAssumeCapacity(paramName);

                break :blk pp.charBuffer.items;
            },
            .LParen => Attribute.normalize(pp.getTokenSlice(paramFirst)),
            else => {
                try pp.err(maybeColon, .malformed_embed_param, .{});
                continue;
            },
        };

        const start: u32 = @intCast(pp.tokenBuffer.items.len);
        while (true) {
            const next = lexer.nextNoWs();
            if (next.id == .RParen) break;
            try pp.tokenBuffer.append(gpa, next);
        }
        const end: u32 = @intCast(pp.tokenBuffer.items.len);

        if (std.mem.eql(u8, param, "limit")) {
            if (limit != null) {
                try pp.err(tokenFromRaw(paramFirst), .duplicate_embed_param, .{"limit"});
                continue;
            }

            if (start + 1 != end) {
                try pp.err(paramFirst, .malformed_embed_limit, .{});
                continue;
            }

            const limitToken = pp.tokenBuffer.items[start];
            if (limitToken.id != .PPNumber) {
                try pp.err(paramFirst, .malformed_embed_limit, .{});
                continue;
            }

            limit = .limited(std.fmt.parseInt(u32, pp.getTokenSlice(limitToken), 10) catch {
                try pp.err(limitToken, .malformed_embed_limit, .{});
                continue;
            });
            pp.tokenBuffer.items.len = start;
        } else if (std.mem.eql(u8, param, "prefix")) {
            if (prefix != null) {
                try pp.err(tokenFromRaw(paramFirst), .duplicate_embed_param, .{"prefix"});
                continue;
            }
            prefix = .{ .start = start, .end = end };
        } else if (std.mem.eql(u8, param, "suffix")) {
            if (suffix != null) {
                try pp.err(tokenFromRaw(paramFirst), .duplicate_embed_param, .{"suffix"});
                continue;
            }
            suffix = .{ .start = start, .end = end };
        } else if (std.mem.eql(u8, param, "if_empty")) {
            if (ifEmpty != null) {
                try pp.err(tokenFromRaw(paramFirst), .duplicate_embed_param, .{"if_empty"});
                continue;
            }
            ifEmpty = .{ .start = start, .end = end };
        } else {
            try pp.err(tokenFromRaw(paramFirst), .unsupported_embed_param, .{param});
            pp.tokenBuffer.items.len = start;
        }
    }

    const embedBytes = (try pp.comp.findEmbed(filename, first.source, includeType, limit orelse .unlimited)) orelse
        return pp.fatalNotFound(fileNameToken, filename);
    defer gpa.free(embedBytes);

    try Range.expand(prefix, pp, lexer);
    if (embedBytes.len == 0) {
        try Range.expand(ifEmpty, pp, lexer);
        try Range.expand(suffix, pp, lexer);
        return;
    }

    try pp.ensureUnusedTokenCapacity(2 * embedBytes.len - 1); // N bytes and N-1 commas

    // TODO: We currently only support systems with CHAR_BIT == 8
    // If the target's CHAR_BIT is not 8, we need to write out correctly-sized embed_bytes
    // and correctly account for the target's endianness

    {
        const byte = embedBytes[0];
        const start = pp.comp.generatedBuffer.items.len;
        try pp.comp.generatedBuffer.print(gpa, "{d}", .{byte});
        pp.addTokenAssumeCapacity(try pp.makeGeneratedToken(start, .EmbedByte, fileNameToken));
    }

    for (embedBytes[1..]) |byte| {
        const start = pp.comp.generatedBuffer.items.len;
        try pp.comp.generatedBuffer.print(gpa, ",{d}", .{byte});
        pp.addTokenAssumeCapacity(.{ .id = .Comma, .loc = .{ .id = .generated, .byteOffset = @intCast(start) } });
        pp.addTokenAssumeCapacity(try pp.makeGeneratedToken(start + 1, .EmbedByte, fileNameToken));
    }
    try pp.comp.generatedBuffer.append(gpa, '\n');

    try Range.expand(suffix, pp, lexer);
}

/// Handle a function like #define directive
fn defineFunc(
    pp: *Preprocessor,
    lexer: *Lexer,
    defineToken: RawToken,
    macroName: TokenWithExpansionLocs,
    lParen: RawToken,
) Error!void {
    assert(macroName.id.isMacroIdentifier());

    const gpa = pp.comp.gpa;
    var params: std.ArrayList([]const u8) = .empty;
    defer params.deinit(gpa);

    // parse the parameter list
    var gnuVarArgs: []const u8 = ""; // gnu-named varargs
    var varArgs = false;
    while (true) {
        var token = lexer.nextNoWs();
        if (token.is(.RParen)) break;

        if (token.is(.Eof))
            return pp.err(token, .unterminated_macro_param_list, .{});

        if (token.is(.Ellipsis)) {
            varArgs = true;
            const rParen = lexer.nextNoWs();
            if (rParen.isNot(.RParen)) {
                try pp.err(rParen, .missing_paren_param_list, .{});
                try pp.err(lParen, .to_match_paren, .{});
                return skipToNewLine(lexer);
            }

            break;
        }

        if (!token.id.isMacroIdentifier()) {
            try pp.err(token, .invalid_token_param_list, .{});
            return skipToNewLine(lexer);
        }

        try params.append(gpa, pp.getTokenSlice(token));

        token = lexer.nextNoWs();
        if (token.is(.Ellipsis)) {
            try pp.err(token, .gnu_va_macro, .{});
            gnuVarArgs = params.pop().?;
            const rParen = lexer.nextNoWs();
            if (rParen.id != .RParen) {
                try pp.err(lParen, .missing_paren_param_list, .{});
                try pp.err(lParen, .to_match_paren, .{});
                return skipToNewLine(lexer);
            }
            break;
        } else if (token.is(.RParen)) {
            break;
        } else if (token.isNot(.Comma)) {
            try pp.err(token, .expected_comma_param_list, .{});
            return skipToNewLine(lexer);
        }
    } else unreachable;

    var needWS = false;
    // Collect the body tokens and validate # and ##'s found.
    // Clear the token buffer
    // Safe to use since we can only be in one directive at a time.
    pp.tokenBuffer.items.len = 0;
    tokenLoop: while (true) {
        var token = lexer.next();
        switch (token.id) {
            .NewLine, .Eof => break,
            .WhiteSpace => needWS = pp.tokenBuffer.items.len != 0,
            .Comment => if (!pp.comp.langOpts.preserveCommentsInMacros) continue else {
                if (needWS) {
                    needWS = false;
                    try pp.tokenBuffer.append(gpa, .{ .id = .MacroWS, .source = .generated });
                }
                try pp.tokenBuffer.append(gpa, token);
            },
            .Hash => {
                if (token.isNot(.WhiteSpace) and needWS) {
                    needWS = false;
                    try pp.tokenBuffer.append(gpa, .{ .id = .MacroWS, .source = .generated });
                }
                const param = lexer.nextNoWs();
                blk: {
                    if (varArgs and param.is(.KeywordVarArgs)) {
                        token.id = .StringifyVarArgs;
                        try pp.tokenBuffer.append(gpa, token);
                        continue :tokenLoop;
                    }

                    if (!param.id.isMacroIdentifier())
                        break :blk;

                    const s = pp.getTokenSlice(param);
                    if (std.mem.eql(u8, s, gnuVarArgs)) {
                        token.id = .StringifyVarArgs;
                        try pp.tokenBuffer.append(gpa, token);
                        continue :tokenLoop;
                    }
                    for (params.items, 0..) |p, i| {
                        if (std.mem.eql(u8, p, s)) {
                            token.id = .StringifyParam;
                            token.end = @intCast(i);
                            try pp.tokenBuffer.append(gpa, token);

                            continue :tokenLoop;
                        }
                    }
                }

                try pp.err(param, .hash_not_followed_param, .{});
                return skipToNewLine(lexer);
            },

            .HashHash => {
                needWS = false;
                // if "##" appear at the beginning, the token buffer is still empty in this case
                // emit error
                if (pp.tokenBuffer.items.len == 0) {
                    try pp.err(token, .hash_hash_at_start, .{});
                    return skipToNewLine(lexer);
                }

                const savedLexer = lexer.*;
                const next = lexer.nextNoWsComments();
                if (next.isOneOf(.{ .NewLine, .Eof })) {
                    try pp.err(token, .hash_hash_at_end, .{});
                    return;
                }

                lexer.* = savedLexer;
                // convert the previous token to .macro_param_no_expand if it was .macro_param
                if (pp.tokenBuffer.items[pp.tokenBuffer.items.len - 1].is(.MacroParam)) {
                    pp.tokenBuffer.items[pp.tokenBuffer.items.len - 1].id = .MacroParamNoExpand;
                }
                try pp.tokenBuffer.append(gpa, token);
            },

            .UnterminatedStringLiteral, .UnterminatedCharLiteral, .EmptyCharLiteral => |tag| {
                try pp.err(token, invalidTokenDiagnostic(tag), .{});
                try pp.tokenBuffer.append(gpa, token);
            },

            .UnterminatedComment => try pp.err(token, .unterminated_comment, .{}),

            else => {
                if (token.id != .WhiteSpace and needWS) {
                    needWS = false;
                    try pp.tokenBuffer.append(gpa, .{ .id = .MacroWS, .source = .generated });
                }

                if (varArgs and token.is(.KeywordVarArgs)) {
                    // do nothing
                } else if (varArgs and token.is(.KeywordVarOpt)) {
                    const optLparen = lexer.next();
                    if (!optLparen.is(.LParen)) {
                        try pp.err(optLparen, .va_opt_lparen, .{});
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
                                try pp.err(optToken, .va_opt_rparen, .{});
                                try pp.err(optLparen, .to_match_paren, .{});
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

                try pp.tokenBuffer.append(gpa, token);
            },
        }
    }

    const paramList = try pp.arena.allocator().dupe([]const u8, params.items);
    const tokenList = try pp.arena.allocator().dupe(RawToken, pp.tokenBuffer.items);
    try pp.defineMacro(defineToken, macroName, .{
        .isFunc = true,
        .params = paramList,
        .varArgs = varArgs or gnuVarArgs.len != 0,
        .tokens = tokenList,
        .loc = macroName.loc,
    });
}

fn include(pp: *Preprocessor, lexer: *Lexer, which: Compilation.WhichInclude) MacroError!void {
    const first = lexer.nextNoWs();
    const newSource = pp.findIncludeSource(lexer, first, which) catch |er| switch (er) {
        error.InvalidInclude => return,
        else => |e| return e,
    };

    const gpa = pp.comp.gpa;

    pp.includeDepth += 1;
    defer pp.includeDepth -= 1;
    if (pp.includeDepth > MaxIncludeDepth) {
        const loc: Source.Location = .{ .id = first.source, .byteOffset = first.start, .line = first.line };
        try pp.err(loc, .too_many_includes, .{});
        return error.StopPreprocessing;
    }

    if (pp.includeGuards.get(newSource.id)) |guard| {
        if (pp.defines.contains(guard))
            return;
    }

    if (pp.verbose)
        pp.verboseLog(first, "include file {s}", .{newSource.path});

    const tokenState = pp.getTokenState();
    try pp.addIncludeStart(newSource);
    const eof = pp.preprocessExtra(newSource) catch |er| switch (er) {
        error.StopPreprocessing => {
            for (pp.expansionEntries.items(.locs)[tokenState.expansionEntriesLen..]) |loc|
                TokenWithExpansionLocs.free(loc, gpa);
            pp.restoreTokenState(tokenState);
            return;
        },
        else => |e| return e,
    };
    try eof.checkMsEof(newSource, pp.comp);

    if (pp.preserveWhitespace and pp.tokens.items(.id)[pp.tokens.len - 1] != .NewLine) {
        try pp.addToken(.{ .id = .NewLine, .loc = .{ .id = lexer.source, .line = lexer.line } });
    }

    if (pp.linemarkers == .None) return;
    var next = first;
    while (true) {
        var tmp = lexer.*;
        next = tmp.nextNoWs();
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
fn makePragmaToken(
    pp: *Preprocessor,
    raw: RawToken,
    operatorLoc: ?Source.Location,
    argLocs: []const Source.Location,
) !TokenWithExpansionLocs {
    const gpa = pp.comp.gpa;
    var tok = tokenFromRaw(raw);
    if (operatorLoc) |loc|
        try tok.addExpansionLocation(gpa, &.{loc});

    try tok.addExpansionLocation(gpa, argLocs);
    return tok;
}

pub fn addToken(pp: *Preprocessor, tokenArg: TokenWithExpansionLocs) !void {
    const gpa = pp.comp.gpa;
    const tok = try pp.unescapeUcn(tokenArg);
    if (tok.expansionLocs) |expansionLocs| {
        try pp.expansionEntries.append(gpa, .{ .idx = @intCast(pp.tokens.len), .locs = expansionLocs });
    }
    try pp.tokens.append(gpa, .{ .id = tok.id, .loc = tok.loc });
}

pub fn addTokenAssumeCapacity(pp: *Preprocessor, tok: TokenWithExpansionLocs) void {
    if (tok.expansionLocs) |expansionLocs| {
        pp.expansionEntries.appendAssumeCapacity(.{ .idx = @intCast(pp.tokens.len), .locs = expansionLocs });
    }
    pp.tokens.appendAssumeCapacity(.{ .id = tok.id, .loc = tok.loc });
}

pub fn ensureTotalTokenCapacity(pp: *Preprocessor, capacity: usize) !void {
    const gpa = pp.comp.gpa;
    try pp.tokens.ensureTotalCapacity(gpa, capacity);
    try pp.expansionEntries.ensureTotalCapacity(gpa, capacity);
}

pub fn ensureUnusedTokenCapacity(pp: *Preprocessor, capacity: usize) !void {
    const gpa = pp.comp.gpa;
    try pp.tokens.ensureUnusedCapacity(gpa, capacity);
    try pp.expansionEntries.ensureUnusedCapacity(gpa, capacity);
}

/// Handle a pragma directive
fn pragma(
    pp: *Preprocessor,
    lexer: *Lexer,
    pragmaToken: RawToken,
    operatorLoc: ?Source.Location,
    argLocs: []const Source.Location,
) !void {
    const nameToken = lexer.nextNoWs();
    if (nameToken.isOneOf(.{ .NewLine, .Eof }))
        return;

    const name = pp.getTokenSlice(nameToken);

    try pp.addToken(try pp.makePragmaToken(pragmaToken, operatorLoc, argLocs));
    const pragmaStart = @as(u32, @intCast(pp.tokens.len));

    const pragmaNameToken = try pp.makePragmaToken(nameToken, operatorLoc, argLocs);
    try pp.addToken(pragmaNameToken);

    while (true) {
        const nextToken = lexer.next();
        if (nextToken.is(.WhiteSpace)) continue;
        if (nextToken.is(.Eof)) {
            try pp.addToken(.{
                .id = .NewLine,
                .loc = .{ .id = .generated },
            });
            break;
        }
        try pp.addToken(try pp.makePragmaToken(nextToken, operatorLoc, argLocs));
        if (nextToken.is(.NewLine))
            break;
    }

    if (pp.comp.getPragma(name)) |prag| unknown: {
        return prag.preprocessorCB(pp, pragmaStart) catch |er| switch (er) {
            error.UnknownPragma => break :unknown,
            else => |e| return e,
        };
    }

    try pp.err(pragmaNameToken, .unknown_pragma, .{});
}

fn findIncludeFilenameToken(
    pp: *Preprocessor,
    firstToken: RawToken,
    lexer: *Lexer,
    trailingTokenBehavior: enum { IgnoreTrailingTokens, expectNlEof },
) !TokenWithExpansionLocs {
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

        const loc: Source.Location = .{ .id = first.source, .byteOffset = lexer.index, .line = first.line };
        try pp.err(loc, .header_str_closing, .{});
        try pp.err(first, .header_str_match, .{});
    }

    const sourceToken = tokenFromRaw(first);
    const filenameToken, const expandedTrailing = switch (sourceToken.id) {
        .StringLiteral, .MacroString => .{ sourceToken, false },
        else => expanded: {
            const gpa = pp.comp.gpa;
            pp.topExpansionBuffer.items.len = 0;
            defer for (pp.topExpansionBuffer.items) |tok|
                TokenWithExpansionLocs.free(tok.expansionLocs, gpa);
            try pp.topExpansionBuffer.append(gpa, sourceToken);
            pp.expansionSourceLoc = sourceToken.loc;

            try pp.expandMacroExhaustive(lexer, &pp.topExpansionBuffer, 0, 1, true, .NonExpr);
            var trailingTokens: []const TokenWithExpansionLocs = &.{};
            const includeStr = (try pp.reconstructIncludeString(pp.topExpansionBuffer.items, &trailingTokens, tokenFromRaw(first))) orelse {
                try pp.expectNewLine(lexer);
                return error.InvalidInclude;
            };
            const start = pp.comp.generatedBuffer.items.len;
            try pp.comp.generatedBuffer.appendSlice(gpa, includeStr);

            break :expanded .{ try pp.makeGeneratedToken(start, switch (includeStr[0]) {
                '"' => .StringLiteral,
                '<' => .MacroString,
                else => unreachable,
            }, pp.topExpansionBuffer.items[0]), trailingTokens.len != 0 };
        },
    };

    // error on the extra tokens.
    switch (trailingTokenBehavior) {
        .expectNlEof => {
            const newLine = lexer.nextNoWs();
            if ((newLine.id != .NewLine and newLine.id != .Eof) or expandedTrailing) {
                skipToNewLine(lexer);
                try pp.err(filenameToken, .extra_tokens_directive_end, .{});
            }
        },
        .IgnoreTrailingTokens => if (expandedTrailing) {
            try pp.err(filenameToken, .extra_tokens_directive_end, .{});
        },
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
    defer TokenWithExpansionLocs.free(filenameToken.expansionLocs, pp.comp.gpa);

    // check for empty filename
    const tkSlice = pp.expandedSliceExtra(filenameToken, .SingleMacroWS);
    if (tkSlice.len < 3) {
        try pp.err(first, .empty_filename, .{});
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
        pp.fatalNotFound(filenameToken, filename);
}

fn printLinemarker(
    pp: *Preprocessor,
    w: *std.Io.Writer,
    lineNO: u32,
    source: Source,
    start_resume: enum(u8) { start, @"resume", none },
) !void {
    try w.writeByte('#');
    if (pp.linemarkers == .LineDirectives) try w.writeAll("line");
    try w.print(" {d} \"{f}\"", .{ lineNO, fmtEscapes(source.path) });

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

pub const DumpMode = enum {
    ResultOnly,
    /// Output only #define directives for all the macros defined during the execution of the preprocessor
    /// Only macros which are still defined at the end of preprocessing are printed.
    /// Only the most recent definition is printed
    /// Defines are printed in arbitrary order
    MacrosOnly,
    /// Standard preprocessor output; but additionally output #define's and #undef's for macros as they are encountered
    MacrosAndResult,
    /// Same as macros_and_result, except only the macro name is printed for #define's
    MacroNamesAndResult,
};

/// Pretty-print the macro define or undef at location `loc`.
/// We re-tokenize the directive because we are printing a macro that may have the same name as one in
/// `pp.defines` but a different definition (due to being #undef'ed and then redefined)
fn prettyPrintMacro(
    pp: *Preprocessor,
    w: *std.Io.Writer,
    loc: Source.Location,
    parts: enum { NameOnly, NameAndBody },
) !void {
    const source = pp.comp.getSource(loc.id);
    var lexer: Lexer = .{
        .buffer = source.buffer,
        .langOpts = pp.comp.langOpts,
        .source = source.id,
        .index = loc.byteOffset,
    };

    // avoid printing multiple whitespace if /* */ comments are within the macro def
    var prevWs = false;
    // do not print comments before the name token is seen.
    var sawName = false;
    while (true) {
        const tok = lexer.next();
        switch (tok.id) {
            .Comment => {
                if (sawName) {
                    prevWs = false;
                    try w.print("{s}", .{pp.getTokenSlice(tok)});
                }
            },
            .NewLine, .Eof => break,
            .WhiteSpace => {
                if (!prevWs) {
                    try w.writeByte(' ');
                    prevWs = true;
                }
            },
            else => {
                prevWs = false;
                try w.print("{s}", .{pp.getTokenSlice(tok)});
            },
        }
        if (tok.id == .Identifier or tok.id == .ExtendedIdentifier) {
            if (parts == .NameOnly) break;
            sawName = true;
        }
    }
}

fn prettyPrintMacrosOnly(pp: *Preprocessor, w: *std.Io.Writer) !void {
    for (pp.defines.values()) |macro| {
        if (macro.isBuiltin) continue;

        try w.writeAll("#define ");
        try pp.prettyPrintMacro(w, macro.loc, .NameAndBody);
        try w.writeByte('\n');
    }
}

/// pretty print tokens and try to preserve whitespace
pub fn prettyPrintTokens(pp: *Preprocessor, w: *std.Io.Writer, macroDumpNode: DumpMode) !void {
    if (macroDumpNode == .MacrosOnly) return pp.prettyPrintMacrosOnly(w);

    const tokenIds = pp.tokens.items(.id);

    var lastNewline = true;
    var i: u32 = 0;
    outer: while (true) : (i += 1) {
        var cur: Token = pp.tokens.get(i);
        switch (cur.id) {
            .Eof => {
                if (!lastNewline) try w.writeByte('\n');
                try w.flush();
                return;
            },

            .NewLine => {
                var newlines: u32 = 0;
                for (tokenIds[i..], i..) |id, j| {
                    if (id == .NewLine) {
                        newlines += 1;
                    } else if (id == .Eof) {
                        if (!lastNewline) try w.writeByte('\n');
                        try w.flush();
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
                            try printLinemarker(pp, w, lineCol.lineNo, source, .none);
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
                try pp.printLinemarker(w, 1, source, .start);
                lastNewline = true;
            },

            .IncludeResume => {
                const source = pp.comp.getSource(cur.loc.id);
                const lineCol = source.getLineCol(cur.loc);
                if (!lastNewline) try w.writeAll("\n");

                try pp.printLinemarker(w, lineCol.lineNo, source, .@"resume");
                lastNewline = true;
            },

            .KeywordDefine, .KeywordUndef => {
                switch (macroDumpNode) {
                    .MacrosAndResult, .MacroNamesAndResult => {
                        try w.writeByte('#');
                        try pp.prettyPrintMacro(
                            w,
                            cur.loc,
                            if (macroDumpNode == .MacrosAndResult) .NameAndBody else .NameOnly,
                        );
                    },
                    .ResultOnly => unreachable, // pp.storeMacroTokens should be false for standard
                    .MacrosOnly => unreachable, // handled by prettyPrintMacroOnly()
                }
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

/// Like `std.zig.fmtEscapes`, but for C strings. Hex escapes are used for any
/// non-ASCII/unprintable bytes to ensure that the string bytes do not change if
/// the encoding of the file is not UTF-8.
fn fmtEscapes(bytes: []const u8) FmtEscapes {
    return .{ .bytes = bytes };
}

const FmtEscapes = struct {
    bytes: []const u8,
    pub fn format(ctx: FmtEscapes, w: *std.Io.Writer) !void {
        for (ctx.bytes) |byte| switch (byte) {
            '\n' => try w.writeAll("\\n"),
            '\r' => try w.writeAll("\\r"),
            '\t' => try w.writeAll("\\t"),
            '\\' => try w.writeAll("\\\\"),
            '"' => try w.writeAll("\\\""),
            ' ', '!', '#'...'&', '('...'[', ']'...'~' => try w.writeByte(byte),
            // Use hex escapes for any non-ASCII/unprintable characters.
            // This ensures that the parsed version of this string will end up
            // containing the same bytes as the input regardless of encoding.
            else => try w.print("\\x{x:0>2}", .{byte}),
        };
    }
};

test "Preserve pragma tokens sometimes" {
    const gpa = std.testing.allocator;
    const io = std.testing.io;
    const Test = struct {
        fn runPreprocessor(sourceText: []const u8) ![]const u8 {
            var arena: std.heap.ArenaAllocator = .init(gpa);
            defer arena.deinit();

            var diagnostics: Diagnostics = .{ .output = .ignore };
            var comp = Compilation.init(gpa, arena.allocator(), io, &diagnostics, std.fs.cwd());
            defer comp.deinit();

            try comp.addDefaultPragmaHandlers();

            var pp = Preprocessor.init(&comp, .default);
            defer pp.deinit();

            pp.preserveWhitespace = true;
            assert(pp.linemarkers == .None);

            const testRunnerMacros = try comp.addSourceFromBuffer("<test_runner>", sourceText);
            const eof = try pp.preprocess(testRunnerMacros);
            try pp.addToken(eof);

            var allocating: std.Io.Writer.Allocating = .init(gpa);
            defer allocating.deinit();

            try pp.prettyPrintTokens(&allocating.writer, .ResultOnly);
            return allocating.toOwnedSlice();
        }

        fn check(sourceText: []const u8, expected: []const u8) !void {
            const output = try runPreprocessor(sourceText);
            defer gpa.free(output);

            try std.testing.expectEqualStrings(expected, output);
        }
    };

    const preserceGccDiag =
        \\#pragma GCC diagnostic error "-Wnewline-eof"
        \\#pragma GCC warning error "-Wnewline-eof"
        \\int x;
        \\#pragma GCC ignored error "-Wnewline-eof"
        \\
    ;
    try Test.check(preserceGccDiag, preserceGccDiag);

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
    const gpa = std.testing.allocator;
    const Test = struct {
        fn testDestringify(pp: *Preprocessor, stringified: []const u8, destringified: []const u8) !void {
            pp.charBuffer.clearRetainingCapacity();
            try pp.charBuffer.ensureUnusedCapacity(gpa, stringified.len);
            pp.destringify(stringified);
            try std.testing.expectEqualStrings(destringified, pp.charBuffer.items);
        }
    };

    var arena: std.heap.ArenaAllocator = .init(gpa);
    defer arena.deinit();

    var diagnostics: Diagnostics = .{ .output = .ignore };
    var comp = Compilation.init(gpa, arena.allocator(), std.testing.io, &diagnostics, std.fs.cwd());
    defer comp.deinit();

    var pp = Preprocessor.init(&comp, .default);
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
                .KeywordElifdef,
                .KeywordElifndef,
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
            gpa: std.mem.Allocator,
            comptime template: []const u8,
            tokenID: TokenType,
            expectedGuards: u32,
        ) !void {
            var arenaState: std.heap.ArenaAllocator = .init(gpa);
            defer arenaState.deinit();
            const arena = arenaState.allocator();

            var diagnostics: Diagnostics = .{ .output = .ignore };
            var comp = Compilation.init(gpa, arena, std.testing.io, &diagnostics, std.fs.cwd());
            defer comp.deinit();

            var pp = Preprocessor.init(&comp, .default);
            defer pp.deinit();

            const path = try std.fs.path.join(arena, &.{ ".", "bar.h" });

            _ = try comp.addSourceFromBuffer(path, "int bar = 5;\n");

            var buf: std.ArrayList(u8) = .empty;
            defer buf.deinit(gpa);

            switch (tokenID) {
                .KeywordInclude, .KeywordIncludeNext => try buf.print(gpa, template, .{ tokenID.lexeme().?, " \"bar.h\"" }),
                .KeywordDefine, .KeywordUndef => try buf.print(gpa, template, .{ tokenID.lexeme().?, " BAR" }),
                .KeywordIfndef,
                .KeywordIfdef,
                .KeywordElifdef,
                .KeywordElifndef,
                => try buf.print(gpa, template, .{ tokenID.lexeme().?, " BAR\n#endif" }),
                else => try buf.print(gpa, template, .{ tokenID.lexeme().?, "" }),
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
