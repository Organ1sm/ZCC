const std = @import("std");
const Compilation = @import("../Basic/Compilation.zig");
const Source = @import("../Basic/Source.zig");
const TokenType = @import("../Basic/TokenType.zig").TokenType;
const Lexer = @import("../Lexer/Lexer.zig");
const Preprocessor = @import("../Lexer/Preprocessor.zig");
const AST = @import("../AST/AST.zig");
const AstTag = @import("../AST/AstTag.zig").Tag;
const Type = @import("../AST/Type.zig");
const TypeBuilder = @import("../AST/TypeBuilder.zig");
const Diagnostics = @import("../Basic/Diagnostics.zig");
const DeclSpec = @import("../AST/DeclSpec.zig");
const SymbolStack = @import("../Sema/SymbolStack.zig");
const Switch = @import("../Sema/Switch.zig");
const Result = @import("Result.zig");
const InitList = @import("InitList.zig");
const Attribute = @import("../Lexer/Attribute.zig");
const CharInfo = @import("../Basic/CharInfo.zig");
const Value = @import("../AST/Value.zig");

const Token = AST.Token;
const TokenIndex = AST.TokenIndex;
const NodeIndex = AST.NodeIndex;
const Allocator = std.mem.Allocator;
const NodeList = std.ArrayList(NodeIndex);

const Parser = @This();
pub const Error = Compilation.Error || error{ParsingFailed};

/// An attribute that has been parsed but not yet validated in its context
const TentativeAttribute = struct {
    attr: Attribute,
    tok: TokenIndex,
};

/// How the parser handles const int decl references when it is expecting an integer
/// constant expression.
const ConstDeclFoldingMode = enum {
    /// fold const decls as if they were literals
    FoldConstDecls,
    /// fold const decls as if they were literals and issue GNU extension diagnostic
    GNUFoldingExtension,
    /// fold const decls as if they were literals and issue VLA diagnostic
    GNUVLAFoldingExtension,
    /// folding const decls is prohibited; return an unavailable value
    NoConstDeclFolding,
};

// values from pp
pp: *Preprocessor,
tokenIds: []const TokenType,
tokenIdx: u32 = 0,

// value of incomplete AST
arena: Allocator,
nodes: AST.Node.List = .{},
data: NodeList,
strings: std.ArrayList(u8),
valueMap: AST.ValueMap,

// buffers used during compilation
symStack: SymbolStack = .{},
labels: std.ArrayList(Label),
listBuffer: NodeList,
declBuffer: NodeList,
paramBuffer: std.ArrayList(Type.Function.Param),
enumBuffer: std.ArrayList(Type.Enum.Field),
recordBuffer: std.ArrayList(Type.Record.Field),
attrBuffer: std.MultiArrayList(TentativeAttribute) = .{},

// configuration
noEval: bool = false,
inMacro: bool = false,
extensionSuppressd: bool = false,
containsAddresssOfLabel: bool = false,
labelCount: u32 = 0,
constDeclFolding: ConstDeclFoldingMode = .FoldConstDecls,
/// location of first computed goto in function currently being parsed
/// if a computed goto is used, the function must contain an
/// address-of-label expression (tracked with ContainsAddressOfLabel)
computedGotoTok: ?TokenIndex = null,

/// Various variables that are different for each function.
func: struct {
    /// null if not in function, will always be plain func, varargs func or OldStyleFunc
    type: ?Type = null,
    name: TokenIndex = 0,
    ident: ?Result = null,
    prettyIdent: ?Result = null,
} = .{},

/// Various variables that are different for each record.
record: struct {
    // invalid means we're not parsing a record
    kind: TokenType = .Invalid,
    flexibleField: ?TokenIndex = null,
    start: usize = 0,

    fn addField(r: @This(), p: *Parser, token: TokenIndex) Error!void {
        const name = p.getTokenSlice(token);
        var i = p.recordMembers.items.len;
        while (i > r.start) {
            i -= 1;
            if (std.mem.eql(u8, p.recordMembers.items[i].name, name)) {
                try p.errStr(.duplicate_member, token, name);
                try p.errToken(.previous_definition, p.recordMembers.items[i].token);
                break;
            }
        }
        try p.recordMembers.append(p.pp.comp.gpa, .{ .name = name, .token = token });
    }

    fn addFieldsFromAnonymous(r: @This(), p: *Parser, ty: Type) Error!void {
        for (ty.data.record.fields) |f| {
            if (f.isAnonymousRecord()) {
                try r.addFieldsFromAnonymous(p, f.ty.canonicalize(.standard));
            } else if (f.nameToken != 0) {
                try r.addField(p, f.nameToken);
            }
        }
    }
} = .{},

recordMembers: std.ArrayListUnmanaged(struct { token: TokenIndex, name: []const u8 }) = .{},
@"switch": ?*Switch = null,
inLoop: bool = false,

const Label = union(enum) {
    unresolvedGoto: TokenIndex,
    label: TokenIndex,
};

fn checkIdentifierCodepointWarnings(comp: *Compilation, codepoint: u21, loc: Source.Location) Compilation.Error!bool {
    std.debug.assert(codepoint >= 0x80);

    const errStart = comp.diag.list.items.len;

    if (!CharInfo.isC99IdChar(codepoint)) {
        try comp.diag.add(.{
            .tag = .c99_compat,
            .loc = loc,
        }, &.{});
    }
    if (CharInfo.isInvisible(codepoint)) {
        try comp.diag.add(.{
            .tag = .unicode_zero_width,
            .loc = loc,
            .extra = .{ .actualCodePoint = codepoint },
        }, &.{});
    }
    if (CharInfo.homoglyph(codepoint)) |resembles| {
        try comp.diag.add(.{
            .tag = .unicode_homoglyph,
            .loc = loc,
            .extra = .{ .codePoints = .{ .actual = codepoint, .resembles = resembles } },
        }, &.{});
    }
    return comp.diag.list.items.len != errStart;
}

/// Issues diagnostics for the current extended identifier token
/// Return value indicates whether the token should be considered an identifier
/// true means consider the token to actually be an identifier
/// false means it is not
fn validateExtendedIdentifier(p: *Parser) !bool {
    std.debug.assert(p.getCurrToken() == .ExtendedIdentifier);

    const slice = p.getTokenSlice(p.tokenIdx);
    const view = std.unicode.Utf8View.init(slice) catch {
        try p.errToken(.invalid_utf8, p.tokenIdx);
        return error.FatalError;
    };
    var it = view.iterator();

    var validIdentifier = true;
    var warned = false;
    var len: usize = 0;
    var invalidChar: u21 = undefined;
    var loc = p.pp.tokens.items(.loc)[p.tokenIdx];

    const standard = p.pp.comp.langOpts.standard;
    while (it.nextCodepoint()) |codepoint| {
        defer {
            len += 1;
            loc.byteOffset += std.unicode.utf8CodepointSequenceLength(codepoint) catch unreachable;
        }
        if (codepoint == '$') {
            warned = true;
            try p.pp.comp.diag.add(.{
                .tag = .dollar_in_identifier_extension,
                .loc = loc,
            }, &.{});
        }

        if (codepoint <= 0x7F) continue;
        if (!validIdentifier) continue;

        const allowed = standard.codepointAllowedInIdentifier(codepoint, len == 0);
        if (!allowed) {
            invalidChar = codepoint;
            validIdentifier = false;
            continue;
        }

        if (!warned)
            warned = try checkIdentifierCodepointWarnings(p.pp.comp, codepoint, loc);
    }

    if (!validIdentifier) {
        if (len == 1) {
            try p.errExtra(.unexpected_character, p.tokenIdx, .{ .actualCodePoint = invalidChar });
            return false;
        } else {
            try p.errExtra(.invalid_identifier_start_char, p.tokenIdx, .{ .actualCodePoint = invalidChar });
        }
    }

    return true;
}

fn eatIdentifier(p: *Parser) !?TokenIndex {
    switch (p.getCurrToken()) {
        .Identifier => {},
        .ExtendedIdentifier => {
            if (!try p.validateExtendedIdentifier()) {
                p.tokenIdx += 1;
                return null;
            }
        },
        else => return null,
    }
    p.tokenIdx += 1;

    // Handle illegal '$' characters in identifiers
    if (!p.pp.comp.langOpts.dollarsInIdentifiers) {
        if (p.getCurrToken() == .Invalid and p.getTokenSlice(p.tokenIdx)[0] == '$') {
            try p.err(.dollars_in_identifiers);
            p.tokenIdx += 1;
            return error.ParsingFailed;
        }
    }

    return p.tokenIdx - 1;
}

fn expectIdentifier(p: *Parser) Error!TokenIndex {
    const actual = p.getCurrToken();
    if (actual != .Identifier and actual != .ExtendedIdentifier)
        return p.errExpectedToken(.Identifier, actual);

    return (try p.eatIdentifier()) orelse unreachable;
}

fn eat(p: *Parser, expected: TokenType) ?TokenIndex {
    std.debug.assert(expected != .Identifier and expected != .ExtendedIdentifier); // use eatIdentifier
    if (p.getCurrToken() == expected) {
        defer p.tokenIdx += 1;
        return p.tokenIdx;
    } else return null;
}

pub fn getCurrToken(p: *Parser) TokenType {
    return p.lookAhead(0);
}

pub fn lookAhead(p: *Parser, n: u32) TokenType {
    std.debug.assert(p.tokenIdx + n < p.tokenIds.len);
    return p.tokenIds[p.tokenIdx + n];
}

fn expectToken(p: *Parser, expected: TokenType) Error!TokenIndex {
    std.debug.assert(expected != .Identifier and expected != .ExtendedIdentifier); // use eatIdentifier
    const actual = p.getCurrToken();
    if (actual != expected)
        return p.errExpectedToken(expected, actual);

    defer p.tokenIdx += 1;
    return p.tokenIdx;
}

fn expectClosing(p: *Parser, opening: TokenIndex, id: TokenType) Error!void {
    _ = p.expectToken(id) catch |e| {
        if (e == error.ParsingFailed) {
            try p.errToken(switch (id) {
                .RParen => .to_match_paren,
                .RBrace => .to_match_brace,
                .RBracket => .to_match_bracket,
                else => unreachable,
            }, opening);
        }
        return e;
    };
}

fn errOverflow(p: *Parser, op_tok: TokenIndex, res: Result) !void {
    if (res.ty.isUnsignedInt(p.pp.comp)) {
        try p.errExtra(.overflow_unsigned, op_tok, .{ .unsigned = res.value.data.int });
    } else {
        try p.errExtra(.overflow_signed, op_tok, .{ .signed = res.value.signExtend(res.ty, p.pp.comp) });
    }
}

pub fn getTokenSlice(p: *Parser, index: TokenIndex) []const u8 {
    if (p.tokenIds[index].getTokenText()) |some|
        return some;

    const loc = p.pp.tokens.items(.loc)[index];
    var lexer = Lexer{
        .buffer = p.pp.comp.getSource(loc.id).buffer,
        .comp = p.pp.comp,
        .index = loc.byteOffset,
        .source = .generated,
    };

    const res = lexer.next();
    return lexer.buffer[res.start..res.end];
}

pub fn errExpectedToken(p: *Parser, expected: TokenType, actual: TokenType) Error {
    switch (actual) {
        .Invalid => try p.errExtra(.expected_invalid, p.tokenIdx, .{ .expectedTokenId = expected }),
        .Eof => try p.errExtra(.expected_eof, p.tokenIdx, .{ .expectedTokenId = expected }),
        else => try p.errExtra(
            .expected_token,
            p.tokenIdx,
            .{
                .tokenId = .{
                    .expected = expected,
                    .actual = actual,
                },
            },
        ),
    }
    return error.ParsingFailed;
}

pub fn errStr(p: *Parser, tag: Diagnostics.Tag, index: TokenIndex, str: []const u8) Compilation.Error!void {
    @setCold(true);
    return p.errExtra(tag, index, .{ .str = str });
}

pub fn errExtra(p: *Parser, tag: Diagnostics.Tag, index: TokenIndex, extra: Diagnostics.Message.Extra) Compilation.Error!void {
    @setCold(true);
    const token = p.pp.tokens.get(index);
    var loc = token.loc;

    if (index != 0 and token.id == .Eof) {
        const prev = p.pp.tokens.get(index - 1);
        loc = prev.loc;
        loc.byteOffset += @intCast(p.getTokenSlice(index - 1).len);
    }

    try p.pp.comp.diag.add(.{
        .tag = tag,
        .loc = loc,
        .extra = extra,
    }, token.expansionSlice());
}

pub fn errToken(p: *Parser, tag: Diagnostics.Tag, index: TokenIndex) Compilation.Error!void {
    @setCold(true);
    return p.errExtra(tag, index, .{ .none = {} });
}

pub fn err(p: *Parser, tag: Diagnostics.Tag) Compilation.Error!void {
    @setCold(true);
    return p.errToken(tag, p.tokenIdx);
}

pub fn todo(p: *Parser, msg: []const u8) Error {
    try p.errStr(.todo, p.tokenIdx, msg);
    return error.ParsingFailed;
}

pub fn ignoredAttrString(p: *Parser, attr: Attribute.Tag, context: Attribute.ParseContext) ![]const u8 {
    const stringTop = p.strings.items.len;
    defer p.strings.items.len = stringTop;

    try p.strings.writer().print("Attribute '{s}' ignored in {s} context", .{ @tagName(attr), @tagName(context) });
    return try p.pp.comp.diag.arena.allocator().dupe(u8, p.strings.items[stringTop..]);
}

pub fn typeStr(p: *Parser, ty: Type) ![]const u8 {
    if (TypeBuilder.fromType(ty).toString()) |str| return str;
    const stringsTop = p.strings.items.len;
    defer p.strings.items.len = stringsTop;

    try ty.print(p.strings.writer());
    return try p.pp.comp.diag.arena.allocator().dupe(u8, p.strings.items[stringsTop..]);
}

pub fn typePairStr(p: *Parser, a: Type, b: Type) ![]const u8 {
    return p.typePairStrExtra(a, " and ", b);
}

pub fn typePairStrExtra(p: *Parser, a: Type, msg: []const u8, b: Type) ![]const u8 {
    const stringsTop = p.strings.items.len;
    defer p.strings.items.len = stringsTop;

    try p.strings.append('\'');
    try a.print(p.strings.writer());
    try p.strings.append('\'');
    try p.strings.appendSlice(msg);
    try p.strings.append('\'');
    try b.print(p.strings.writer());
    try p.strings.append('\'');
    return try p.pp.comp.diag.arena.allocator().dupe(u8, p.strings.items[stringsTop..]);
}

pub fn floatValueChangedStr(p: *Parser, res: *Result, oldValue: f64, intTy: Type) ![]const u8 {
    const stringsTop = p.strings.items.len;
    defer p.strings.items.len = stringsTop;

    var w = p.strings.writer();
    const str = try p.typePairStrExtra(res.ty, " to ", intTy);
    try w.writeAll(str);
    const isZero = res.value.isZero();
    const nonZeroStr: []const u8 = if (isZero) "non-zero " else "";
    if (intTy.is(.Bool)) {
        try w.print(" changes {s}value from {d} to {}", .{ nonZeroStr, oldValue, res.value.getBool() });
    } else if (intTy.isUnsignedInt(p.pp.comp)) {
        try w.print(" changes {s}value from {d} to {d}", .{ nonZeroStr, oldValue, res.value.getInt(u64) });
    } else {
        try w.print(" changes {s}value from {d} to {d}", .{ nonZeroStr, oldValue, res.value.getInt(i64) });
    }

    return try p.pp.comp.diag.arena.allocator().dupe(u8, p.strings.items[stringsTop..]);
}

/// Check for deprecated or unavailable attributes on a type and report them.
/// If the type has an 'unavailable' attribute, it reports an error for both
/// the usage and declaration tokens, then aborts parsing.
/// If the type has a 'deprecated' attribute, it reports a warning for both
/// the usage and declaration tokens.
///
/// @param p The parser instance.
/// @param ty The type to check for attributes.
/// @param usageToken The token index where the type is used.
/// @param declToken The token index where the type is declared.
/// @return Returns error.ParsingFailed if the type is unavailable, otherwise void.
fn checkDeprecatedUnavailable(p: *Parser, ty: Type, usageToken: TokenIndex, declToken: TokenIndex) !void {
    // Check if the type has an 'unavailable' attribute and report it
    if (ty.getAttribute(.unavailable)) |unavailable| {
        try p.errDeprecated(.unavailable, usageToken, unavailable.msg);
        try p.errStr(.unavailable_note, unavailable.__name_token, p.getTokenSlice(declToken));
        return error.ParsingFailed; // Abort parsing due to 'unavailable' type
    }
    // Check if the type has a 'deprecated' attribute and report it
    else if (ty.getAttribute(.deprecated)) |deprecated| {
        try p.errDeprecated(.deprecated_declarations, usageToken, deprecated.msg);
        try p.errStr(.deprecated_note, deprecated.__name_token, p.getTokenSlice(declToken));
    }
}

/// Reports deprecated or unavailable usage of code based on the diagnostic tag.
/// It constructs an error message and then calls `errStr` to handle the error.
///
/// @param p         The parser instance containing state and utilities for parsing.
/// @param tag       The diagnostic tag indicating the type of deprecation.
/// @param tokenIdx  The index of the token related to the deprecation.
/// @param msg       Optional message providing additional information about the deprecation.
/// @return          An error indicating that the parsing should be aborted on failure.
fn errDeprecated(p: *Parser, tag: Diagnostics.Tag, tokenIdx: TokenIndex, msg: ?[]const u8) Compilation.Error!void {
    const stringsTop = p.strings.items.len;
    defer p.strings.items.len = stringsTop;

    const w = p.strings.writer();
    try w.print("'{s}' is ", .{p.getTokenSlice(tokenIdx)});
    // Determine the reason for deprecation based on the provided tag.
    const reason: []const u8 = switch (tag) {
        .unavailable => "unavailable", // The feature is not available.
        .deprecated_declarations => "deprecated", // The feature is deprecated.
        else => unreachable, // Other cases should be unreachable.
    };

    // Write the reason to the buffer.
    try w.writeAll(reason);
    // If a custom message is provided, append it to the buffer.
    if (msg) |m| {
        try w.print(": {s}", .{m}); // Append the custom message.
    }

    // Duplicate the constructed string from the buffer and prepare the error message.
    const str = try p.pp.comp.diag.arena.allocator().dupe(u8, p.strings.items[stringsTop..]);
    // Report the error with the constructed message.
    return p.errStr(tag, tokenIdx, str);
}

pub fn addNode(p: *Parser, node: AST.Node) Allocator.Error!NodeIndex {
    if (p.inMacro)
        return .none;

    const res = p.nodes.len;
    try p.nodes.append(p.pp.comp.gpa, node);

    return @enumFromInt(res);
}

fn addList(p: *Parser, nodes: []const NodeIndex) Allocator.Error!AST.Range {
    if (p.inMacro)
        return AST.Range{ .start = 0, .end = 0 };
    const start: u32 = @intCast(p.data.items.len);
    try p.data.appendSlice(nodes);
    const end: u32 = @intCast(p.data.items.len);

    return AST.Range{ .start = start, .end = end };
}

fn findLabel(p: *Parser, name: []const u8) ?TokenIndex {
    for (p.labels.items) |item| {
        switch (item) {
            .label => |l| if (std.mem.eql(u8, p.getTokenSlice(l), name)) return l,
            .unresolvedGoto => {},
        }
    }
    return null;
}

fn nodeIs(p: *Parser, node: NodeIndex, tag: AstTag) bool {
    return p.getNode(node, tag) != null;
}

fn getNode(p: *Parser, node: NodeIndex, tag: AstTag) ?NodeIndex {
    var cur = node;
    const tags = p.nodes.items(.tag);
    const data = p.nodes.items(.data);
    while (true) {
        const curTag = tags[@intFromEnum(cur)];
        if (curTag == .ParenExpr) {
            cur = data[@intFromEnum(cur)].unExpr;
        } else if (curTag == tag) {
            return cur;
        } else {
            return null;
        }
    }
}

fn pragma(p: *Parser) Compilation.Error!bool {
    var foundPragma = false;
    while (p.eat(.KeywordPragma)) |_| {
        foundPragma = true;
        const nameToken = p.tokenIdx;
        const name = p.getTokenSlice(nameToken);
        const endIdx = std.mem.indexOfScalarPos(TokenType, p.tokenIds, p.tokenIdx, .NewLine).?;
        const pragmaLen = @as(TokenIndex, @intCast(endIdx)) - p.tokenIdx;
        defer p.tokenIdx += pragmaLen + 1; // skip past .nl as well

        if (p.pp.comp.getPragma(name)) |prag| {
            try prag.parserCB(p, p.tokenIdx);
        }
    }
    return foundPragma;
}

/// root : (decl | inline assembly ';' | static-assert-declaration)*
pub fn parse(pp: *Preprocessor) Compilation.Error!AST {
    pp.comp.pragmaEvent(.BeforeParse);

    var arena = std.heap.ArenaAllocator.init(pp.comp.gpa);
    errdefer arena.deinit();

    var p = Parser{
        .pp = pp,
        .arena = arena.allocator(),
        .tokenIds = pp.tokens.items(.id),
        .data = NodeList.init(pp.comp.gpa),
        .labels = std.ArrayList(Label).init(pp.comp.gpa),
        .strings = std.ArrayList(u8).init(pp.comp.gpa),
        .valueMap = AST.ValueMap.init(pp.comp.gpa),
        .listBuffer = NodeList.init(pp.comp.gpa),
        .declBuffer = NodeList.init(pp.comp.gpa),
        .paramBuffer = std.ArrayList(Type.Function.Param).init(pp.comp.gpa),
        .enumBuffer = std.ArrayList(Type.Enum.Field).init(pp.comp.gpa),
        .recordBuffer = std.ArrayList(Type.Record.Field).init(pp.comp.gpa),
    };

    //bind p to the symbol stack for simplify symbol stack api
    p.symStack.p = &p;

    defer {
        p.symStack.deinit(pp.comp.gpa);
        p.data.deinit();
        p.labels.deinit();
        p.listBuffer.deinit();
        p.declBuffer.deinit();
        p.paramBuffer.deinit();
        p.enumBuffer.deinit();
        p.recordBuffer.deinit();
        p.recordMembers.deinit(pp.comp.gpa);
        p.attrBuffer.deinit(pp.comp.gpa);
    }

    errdefer {
        p.nodes.deinit(pp.comp.gpa);
        p.strings.deinit();
        p.valueMap.deinit();
    }

    _ = try p.addNode(.{ .tag = .Invalid, .type = undefined, .data = undefined });
    {
        const ty = &pp.comp.types.vaList;
        try p.symStack.defineTypedef("__builtin_va_list", ty.*, 0, .none);
        if (ty.isArray())
            ty.decayArray();
    }

    while (p.eat(.Eof) == null) {
        if (try p.pragma())
            continue;

        if (try p.parseOrNextDecl(parseStaticAssert))
            continue;

        if (try p.parseOrNextDecl(parseDeclaration))
            continue;

        if (p.eat(.KeywordGccExtension)) |_| {
            const saveExtension = p.extensionSuppressd;
            defer p.extensionSuppressd = saveExtension;

            p.extensionSuppressd = true;

            if (try p.parseOrNextDecl(parseDeclaration))
                continue;
            switch (p.getCurrToken()) {
                .Semicolon => p.tokenIdx += 1,
                .KeywordStaticAssert,
                .KeywordPragma,
                .KeywordGccExtension,
                .KeywordGccAsm,
                .KeywordGccAsm1,
                .KeywordGccAsm2,
                => {},
                else => try p.err(.expected_external_decl),
            }
            continue;
        }

        if (p.parseAssembly(.global) catch |er| switch (er) {
            error.ParsingFailed => {
                p.nextExternDecl();
                continue;
            },
            else => |e| return e,
        }) |_| continue;

        if (p.eat(.Semicolon)) |tok| {
            try p.errToken(.extra_semi, tok);
            continue;
        }

        try p.err(.expected_external_decl);
        p.tokenIdx += 1;
    }

    const rootDecls = try p.declBuffer.toOwnedSlice();
    if (rootDecls.len == 0)
        try p.errToken(.empty_translation_unit, p.tokenIdx - 1);

    const data = try p.data.toOwnedSlice();
    errdefer pp.comp.gpa.free(data);

    pp.comp.pragmaEvent(.AfterParse);

    return AST{
        .comp = pp.comp,
        .tokens = pp.tokens.slice(),
        .arena = arena,
        .generated = pp.comp.generatedBuffer.items,
        .nodes = p.nodes.toOwnedSlice(),
        .data = data,
        .rootDecls = rootDecls,
        .strings = try p.strings.toOwnedSlice(),
        .valueMap = p.valueMap,
    };
}

fn parseOrNextDecl(p: *Parser, comptime func: fn (*Parser) Error!bool) Compilation.Error!bool {
    return func(p) catch |er| switch (er) {
        error.ParsingFailed => {
            p.nextExternDecl();
            return true;
        },
        else => |e| return e,
    };
}

fn nextExternDecl(p: *Parser) void {
    var parens: u32 = 0;
    while (true) : (p.tokenIdx += 1) {
        switch (p.getCurrToken()) {
            .LParen, .LBrace, .LBracket => parens += 1,
            .RParen, .RBrace, .RBracket => if (parens != 0) {
                parens -= 1;
            },

            .KeywordTypedef,
            .KeywordExtern,
            .KeywordStatic,
            .KeywordAuto,
            .KeywordRegister,
            .KeywordThreadLocal,
            .KeywordInline,
            .KeywordGccInline1,
            .KeywordGccInline2,
            .KeywordNoreturn,
            .KeywordVoid,
            .KeywordBool,
            .KeywordChar,
            .KeywordShort,
            .KeywordInt,
            .KeywordLong,
            .KeywordSigned,
            .KeywordUnsigned,
            .KeywordFloat,
            .KeywordDouble,
            .KeywordComplex,
            .KeywordAtomic,
            .KeywordEnum,
            .KeywordStruct,
            .KeywordUnion,
            .KeywordAlignas,
            .KeywordGccTypeof,
            .KeywordGccExtension,
            .KeywordTypeof1,
            .KeywordTypeof2,
            .Identifier,
            .ExtendedIdentifier,
            => if (parens == 0) return,
            .KeywordPragma => p.skipToPragmaSentinel(),
            .Eof => return,
            .Semicolon => if (parens == 0) {
                p.tokenIdx += 1;
                return;
            },
            else => {},
        }
    }
}

fn skipToPragmaSentinel(p: *Parser) void {
    while (true) : (p.tokenIdx += 1) {
        if (p.getCurrToken() == .NewLine) return;
        if (p.getCurrToken() == .Eof) {
            p.tokenIdx -= 1;
            return;
        }
    }
}

fn skipTo(p: *Parser, id: TokenType) void {
    var parens: u32 = 0;
    while (true) : (p.tokenIdx += 1) {
        if (p.getCurrToken() == id and parens == 0) {
            p.tokenIdx += 1;
            return;
        }
        switch (p.getCurrToken()) {
            .LParen, .LBrace, .LBracket => parens += 1,
            .RParen, .RBrace, .RBracket => if (parens != 0) {
                parens -= 1;
            },

            .KeywordPragma => p.skipToPragmaSentinel(),
            .Eof => return,
            else => {},
        }
    }
}

pub fn withAttributes(p: *Parser, ty: Type, start: usize) !Type {
    const attrs = p.attrBuffer.items(.attr)[start..];
    return ty.withAttributes(p.arena, attrs);
}

/// declaration
///  : declaration-specifiers init-declarator-list? ';'
///  | attribute-specifier declaration-specifiers init-declarator-list? ';'
///  | static-assert-declaration
///  | declaration-specifiers declarator decl* compoundStmt
///
/// init-declarator-list
///  : init-declarator (',' init-declarator)*
fn parseDeclaration(p: *Parser) Error!bool {
    _ = try p.pragma();
    const firstTokenIndex = p.tokenIdx;
    const attrBufferTop = p.attrBuffer.len;
    defer p.attrBuffer.len = attrBufferTop;

    try p.parseAttrSpec();

    var declSpec = if (try p.parseDeclSpec(false)) |some| some else blk: {
        if (p.func.type != null) {
            p.tokenIdx = firstTokenIndex;
            return false;
        }

        switch (p.tokenIds[firstTokenIndex]) {
            .Asterisk, .LParen, .Identifier => {},
            else => if (p.tokenIdx != firstTokenIndex) {
                try p.err(.expected_ident_or_l_paren);
                return error.ParsingFailed;
            } else return false,
        }

        var spec: TypeBuilder = .{};
        break :blk DeclSpec{ .type = try spec.finish(p, p.attrBuffer.len) };
    };

    if (declSpec.noreturn) |token| {
        const attr = Attribute{ .tag = .noreturn, .args = .{ .noreturn = {} } };
        try p.attrBuffer.append(p.pp.comp.gpa, .{ .attr = attr, .tok = token });
    }

    try declSpec.warnIgnoredAttrs(p, attrBufferTop);
    var ID = (try p.parseInitDeclarator(&declSpec)) orelse {
        // eat ';'
        _ = try p.expectToken(.Semicolon);
        if (declSpec.type.is(.Enum) or (declSpec.type.isRecord() and !declSpec.type.isAnonymousRecord() and !declSpec.type.isTypeof()))
            return true;

        try p.errToken(.missing_declaration, firstTokenIndex);
        return true;
    };

    ID.d.type = try p.withAttributes(ID.d.type, attrBufferTop);
    try p.validateAlignas(ID.d.type, null);

    // check for funtion definition
    if (ID.d.funcDeclarator != null and
        ID.initializer.node == .none and
        ID.d.type.isFunc())
    fndef: {
        switch (p.getCurrToken()) {
            .Comma, .Semicolon => break :fndef,
            .LBrace => {},
            else => {
                if (ID.d.oldTypeFunc == null) {
                    try p.err(.expected_fn_body);
                    return true;
                }
            },
        }

        if (p.func.type != null)
            try p.err(.func_not_in_root);

        const node = try p.addNode(undefined); // reserve space
        try p.symStack.defineSymbol(ID.d.type, ID.d.name, node, .{});

        const func = p.func;
        defer p.func = func;
        p.func = .{ .type = ID.d.type, .name = ID.d.name };

        try p.symStack.pushScope();
        defer p.symStack.popScope();

        // collect old style parameters
        if (ID.d.oldTypeFunc != null) {
            const paramBufferTop = p.paramBuffer.items.len;
            defer p.paramBuffer.items.len = paramBufferTop;

            // ensure attributed specifier is not lost for old-style functions
            const attrs = ID.d.type.getAttributes();
            var baseTy = if (ID.d.type.specifier == .Attributed) ID.d.type.getElemType() else ID.d.type;
            baseTy.specifier = .Func;
            ID.d.type = try baseTy.withAttributes(p.arena, attrs);

            paramLoop: while (true) {
                const paramDeclSpec = (try p.parseDeclSpec(true)) orelse break;
                if (p.eat(.Semicolon)) |semi| {
                    try p.errToken(.missing_declaration, semi);
                    continue :paramLoop;
                }

                while (true) {
                    var d = (try p.declarator(paramDeclSpec.type, .normal)) orelse {
                        try p.errToken(.missing_declaration, firstTokenIndex);
                        _ = try p.expectToken(.Semicolon);
                        continue :paramLoop;
                    };

                    if (d.type.hasIncompleteSize() and !d.type.is(.Void))
                        try p.errStr(.parameter_incomplete_ty, d.name, try p.typeStr(d.type));

                    if (d.type.isFunc()) {
                        const elemType = try p.arena.create(Type);
                        elemType.* = d.type;
                        d.type = Type{
                            .specifier = .Pointer,
                            .data = .{ .subType = elemType },
                        };
                    } else if (d.type.isArray()) {
                        // param declared as arrays are converted to pointers
                        d.type.decayArray();
                    } else if (d.type.is(.Void)) {
                        try p.errToken(.invalid_void_param, d.name);
                    }

                    // find and correct parameter types
                    // TODO check for missing declaration and redefinition
                    const name = p.getTokenSlice(d.name);
                    for (ID.d.type.getParams()) |*param| {
                        if (std.mem.eql(u8, param.name, name)) {
                            param.ty = d.type;
                            break;
                        }
                    } else {
                        try p.errStr(.parameter_missing, d.name, name);
                    }

                    // bypass redefinition check to avoid duplicate errors
                    try p.symStack.appendSymbol(.{
                        .kind = .definition,
                        .name = name,
                        .token = d.name,
                        .type = d.type,
                        .value = .{},
                    });
                    if (p.eat(.Comma) == null) break;
                }

                _ = try p.expectToken(.Semicolon);
            }
        } else {
            for (ID.d.type.getParams()) |param| {
                if (param.ty.hasUnboundVLA())
                    try p.errToken(.unbound_vla, param.nameToken);
                if (param.ty.hasIncompleteSize() and !param.ty.is(.Void))
                    try p.errStr(.parameter_incomplete_ty, param.nameToken, try p.typeStr(param.ty));
                if (param.name.len == 0) {
                    try p.errToken(.omitting_parameter_name, param.nameToken);
                    continue;
                }

                // bypass redefinition check to avoid duplicate errors
                try p.symStack.appendSymbol(.{
                    .kind = .definition,
                    .name = param.name,
                    .token = param.nameToken,
                    .type = param.ty,
                    .value = .{},
                });
            }
        }

        const body = (try p.parseCompoundStmt(true, null)) orelse {
            std.debug.assert(ID.d.oldTypeFunc != null);
            try p.err(.expected_fn_body);
            return true;
        };
        p.nodes.set(@intFromEnum(node), .{
            .type = ID.d.type,
            .tag = try declSpec.validateFnDef(p),
            .data = .{ .decl = .{ .name = ID.d.name, .node = body } },
        });
        try p.declBuffer.append(node);

        // check gotos
        if (func.type == null) {
            for (p.labels.items) |item| {
                if (item == .unresolvedGoto)
                    try p.errStr(.undeclared_label, item.unresolvedGoto, p.getTokenSlice(item.unresolvedGoto));

                if (p.computedGotoTok) |gotoToken| {
                    if (!p.containsAddresssOfLabel)
                        try p.errToken(.invalid_computed_goto, gotoToken);
                }

                p.labels.items.len = 0;
                p.labelCount = 0;
                p.containsAddresssOfLabel = false;
                p.computedGotoTok = null;
            }
        }

        return true;
    }

    // Declare all variable/typedef declarators.
    while (true) {
        if (ID.d.oldTypeFunc) |tokenIdx|
            try p.errToken(.invalid_old_style_params, tokenIdx);

        const tag = try declSpec.validate(p, &ID.d.type, ID.initializer.node != .none);
        // const attrs = p.attrBuffer.items(.attr)[attrBufferTop..];
        // ID.d.type = try ID.d.type.withAttributes(p.arena, attrs);

        const node = try p.addNode(.{
            .type = ID.d.type,
            .tag = tag,
            .data = .{ .decl = .{ .name = ID.d.name, .node = ID.initializer.node } },
        });
        try p.declBuffer.append(node);

        if (declSpec.storageClass == .typedef) {
            try p.symStack.defineTypedef(p.getTokenSlice(ID.d.name), ID.d.type, ID.d.name, node);
        } else if (ID.initializer.node != .none or
            (declSpec.storageClass != .@"extern" and p.func.type != null))
        {
            try p.symStack.defineSymbol(
                ID.d.type,
                ID.d.name,
                node,
                if (ID.d.type.isConst()) ID.initializer.value else .{},
            );
        } else {
            try p.symStack.declareSymbol(ID.d.type, ID.d.name, node);
        }

        if (p.eat(.Comma) == null)
            break;

        ID = (try p.parseInitDeclarator(&declSpec)) orelse {
            try p.err(.expected_ident_or_l_paren);
            continue;
        };
    }

    _ = try p.expectToken(.Semicolon);
    return true;
}

/// static-assert-declaration
///  : (`_Static_assert` | `static_assert`) '(' constExpr ',' StringLiteral+ ')' ';'
fn parseStaticAssert(p: *Parser) Error!bool {
    const curToken = p.eat(.KeywordStaticAssert) orelse return false;
    const lp = try p.expectToken(.LParen);
    const resToken = p.tokenIdx;
    const res = try p.parseConstExpr(.NoConstDeclFolding);

    const str = if (p.eat(.Comma) != null)
        switch (p.getCurrToken()) {
            .StringLiteral,
            .StringLiteralUTF_8,
            .StringLiteralUTF_16,
            .StringLiteralUTF_32,
            .StringLiteralWide,
            => try p.parseStringLiteral(),

            else => {
                try p.err(.expected_str_literal);
                return error.ParsingFailed;
            },
        }
    else
        Result{};

    try p.expectClosing(lp, .RParen);
    _ = try p.expectToken(.Semicolon);
    if (str.node == .none)
        try p.errToken(.static_assert_missing_message, curToken);

    if (res.value.tag == .unavailable) {
        // an unavailable sizeof expression is already a compile error, so we don't emit
        // another error for an invalid _Static_assert condition. This matches the behavior
        // of gcc/clang
        if (!p.nodeIs(res.node, .SizeOfExpr))
            try p.errToken(.static_assert_not_constant, resToken);
    } else if (!res.value.getBool()) {
        if (str.node != .none) {
            var buffer = std.ArrayList(u8).init(p.pp.comp.gpa);
            defer buffer.deinit();

            const data = str.value.data.bytes;
            try buffer.ensureUnusedCapacity(data.len);
            try AST.dumpString(
                data,
                p.nodes.items(.tag)[@intFromEnum(str.node)],
                buffer.writer(),
            );

            try p.errStr(
                .static_assert_failure_message,
                curToken,
                try p.pp.comp.diag.arena.allocator().dupe(u8, buffer.items),
            );
        } else try p.errToken(.static_assert_failure, curToken);
    }

    const node = try p.addNode(.{
        .tag = .StaticAssert,
        .data = .{
            .binExpr = .{
                .lhs = res.node,
                .rhs = str.node,
            },
        },
    });

    try p.declBuffer.append(node);
    return true;
}

/// typeof
///   : `typeof` '(' typeName ')'
///   | `typeof` '(' expr ')'
fn typeof(p: *Parser) Error!?Type {
    switch (p.getCurrToken()) {
        .KeywordGccTypeof, .KeywordTypeof1, .KeywordTypeof2 => p.tokenIdx += 1,
        else => return null,
    }

    const lp = try p.expectToken(.LParen);
    if (try p.parseTypeName()) |ty| {
        try p.expectClosing(lp, .RParen);
        const typeofType = try p.arena.create(Type);
        typeofType.* = .{
            .data = ty.data,
            .qual = ty.qual.inheritFromTypeof(),
            .specifier = ty.specifier,
        };

        return Type{
            .data = .{ .subType = typeofType },
            .specifier = .TypeofType,
        };
    }

    const typeofExpr = try p.parseNoEval(parseExpr);
    try typeofExpr.expect(p);
    try p.expectClosing(lp, .RParen);

    const inner = try p.arena.create(Type.Expr);
    inner.* = .{
        .node = typeofExpr.node,
        .ty = .{
            .data = typeofExpr.ty.data,
            .qual = typeofExpr.ty.qual.inheritFromTypeof(),
            .specifier = typeofExpr.ty.specifier,
        },
    };

    return Type{
        .data = .{ .expr = inner },
        .specifier = .TypeofExpr,
    };
}

/// declaration-Specifier
///  : storageClass-specifier
///  | type-specifier
///  | type-qualifier
///  | func-specifier
///  | align-specifier
///
/// storageClass-specifier:
///  : `typedef`
///  | `extern`
///  | `static`
///  | `threadlocal`
///  | `auto`
///  | `register`
fn parseDeclSpec(p: *Parser, isParam: bool) Error!?DeclSpec {
    var d: DeclSpec = .{ .type = .{ .specifier = undefined } };
    var spec: TypeBuilder = .{};

    const attrBufferTop = p.attrBuffer.len;
    defer p.attrBuffer.len = attrBufferTop;

    const start = p.tokenIdx;
    while (true) {
        if (try p.parseTypeSpec(&spec))
            continue;

        const token = p.getCurrToken();
        switch (token) {
            .KeywordTypedef,
            .KeywordExtern,
            .KeywordStatic,
            .KeywordAuto,
            .KeywordRegister,
            => {
                if (d.storageClass != .none) {
                    try p.errStr(.multiple_storage_class, p.tokenIdx, @tagName(d.storageClass));

                    return error.ParsingFailed;
                }

                if (d.threadLocal != null) {
                    switch (token) {
                        .KeywordTypedef,
                        .KeywordAuto,
                        .KeywordRegister,
                        => try p.errStr(.cannot_combine_spec, p.tokenIdx, token.getTokenText().?),

                        else => {},
                    }
                }

                switch (token) {
                    .KeywordTypedef => d.storageClass = .{ .typedef = p.tokenIdx },
                    .KeywordExtern => d.storageClass = .{ .@"extern" = p.tokenIdx },
                    .KeywordStatic => d.storageClass = .{ .static = p.tokenIdx },
                    .KeywordAuto => d.storageClass = .{ .auto = p.tokenIdx },
                    .KeywordRegister => d.storageClass = .{ .register = p.tokenIdx },
                    else => unreachable,
                }
            },

            .KeywordThreadLocal => {
                if (d.threadLocal != null) {
                    try p.errStr(.duplicate_declspec, p.tokenIdx, "_Thread_local");
                }

                switch (d.storageClass) {
                    .@"extern", .none, .static => {},
                    else => try p.errStr(.cannot_combine_spec, p.tokenIdx, @tagName(d.storageClass)),
                }

                d.threadLocal = p.tokenIdx;
            },

            .KeywordInline, .KeywordGccInline1, .KeywordGccInline2 => {
                if (d.@"inline" != null) {
                    try p.errStr(.duplicate_declspec, p.tokenIdx, "inline");
                }

                d.@"inline" = p.tokenIdx;
            },

            .KeywordNoreturn => {
                if (d.noreturn != null) {
                    try p.errStr(.duplicate_declspec, p.tokenIdx, "_Noreturn");
                }

                d.noreturn = p.tokenIdx;
            },
            else => break,
        }

        p.tokenIdx += 1;
    }

    if (p.tokenIdx == start)
        return null;

    d.type = try spec.finish(p, attrBufferTop);
    if (isParam)
        try p.validateAlignas(d.type, .alignas_on_param);

    return d;
}

fn validateAlignas(p: *Parser, ty: Type, tag: ?Diagnostics.Tag) !void {
    const base = ty.canonicalize(.standard);
    const defaultAlign = base.alignof(p.pp.comp);
    for (ty.getAttributes()) |attr| {
        if (attr.tag != .aligned) continue;
        if (attr.args.aligned.alignment) |alignment| {
            if (!alignment.alignas) continue;

            const alignToken = attr.args.aligned.__name_token;
            if (tag) |t|
                try p.errToken(t, alignToken);

            if (ty.isFunc()) {
                try p.errToken(.alignas_on_func, alignToken);
            } else if (alignment.requested < defaultAlign) {
                try p.errExtra(.minimum_alignment, alignToken, .{ .unsigned = defaultAlign });
            }
        }
    }
}

/// attribute
///  : attrIdentifier
///  | attrIdentifier '(' identifier ')'
///  | attrIdentifier '(' identifier (',' expr)+ ')'
///  | attrIdentifier '(' (expr (',' expr)*)? ')'
fn attribute(p: *Parser, kind: Attribute.Kind, namespace: ?[]const u8) Error!?TentativeAttribute {
    const nameToken = p.tokenIdx;
    switch (p.getCurrToken()) {
        .KeywordConst, .KeywordGccConst1, .KeywordGccConst2 => p.tokenIdx += 1,
        else => _ = try p.expectIdentifier(),
    }

    const name = p.getTokenSlice(nameToken);
    const attr = Attribute.fromString(kind, namespace, name) orelse {
        const tag: Diagnostics.Tag = if (kind == .declspec) .declspec_attr_not_supported else .unknown_attribute;
        try p.errStr(tag, nameToken, name);
        if (p.eat(.LParen)) |_| p.skipTo(.RParen);
        return null;
    };
    const requiredCount = Attribute.requiredArgCount(attr);
    var arguments = Attribute.initArguments(attr, nameToken);
    var argIdx: u32 = 0;

    switch (p.getCurrToken()) {
        .Comma, .RParen => {}, // will be consumed in attributeList

        .LParen => blk: {
            p.tokenIdx += 1;
            if (p.eat(.RParen)) |_|
                break :blk;

            if (Attribute.wantsIdentEnum(attr)) {
                if (try p.eatIdentifier()) |ident| {
                    if (Attribute.diagnoseIdent(attr, &arguments, p.getTokenSlice(ident))) |msg| {
                        try p.errExtra(msg.tag, ident, msg.extra);
                        p.skipTo(.RParen);
                        return error.ParsingFailed;
                    }
                } else {
                    try p.errExtra(.attribute_requires_identifier, nameToken, .{ .str = name });
                    return error.ParsingFailed;
                }
            } else {
                const argStart = p.tokenIdx;
                var firstExpr = try p.parseAssignExpr();
                try firstExpr.expect(p);
                if (p.diagnose(attr, &arguments, argIdx, firstExpr)) |msg| {
                    try p.errExtra(msg.tag, argStart, msg.extra);
                    p.skipTo(.RParen);
                    return error.ParsingFailed;
                }
            }
            argIdx += 1;
            while (p.eat(.RParen) == null) : (argIdx += 1) {
                _ = try p.expectToken(.Comma);

                const argStart = p.tokenIdx;
                var argExpr = try p.parseAssignExpr();
                try argExpr.expect(p);
                if (p.diagnose(attr, &arguments, argIdx, argExpr)) |msg| {
                    try p.errExtra(msg.tag, argStart, msg.extra);
                    p.skipTo(.RParen);
                    return error.ParsingFailed;
                }
            }
        },
        else => {},
    }
    if (argIdx < requiredCount) {
        try p.errExtra(
            .attribute_not_enough_args,
            nameToken,
            .{ .attrArgCount = .{ .attribute = attr, .expected = requiredCount } },
        );
        return error.ParsingFailed;
    }
    return TentativeAttribute{ .attr = .{ .tag = attr, .args = arguments }, .tok = nameToken };
}

fn diagnose(p: *Parser, attr: Attribute.Tag, arguments: *Attribute.Arguments, argIdx: u32, res: Result) ?Diagnostics.Message {
    if (Attribute.wantsAlignment(attr, argIdx))
        return Attribute.diagnoseAlignment(attr, arguments, argIdx, res.value, res.ty, p.pp.comp);

    const node = p.nodes.get(@intFromEnum(res.node));
    return Attribute.diagnose(attr, arguments, argIdx, res.value, node);
}

/// attribute-list : (attribute (',' attribute)*)?
fn parseGNUAttrList(p: *Parser) Error!void {
    if (p.getCurrToken() == .RParen)
        return;

    if (try p.attribute(.gnu, null)) |attr| {
        try p.attrBuffer.append(p.pp.comp.gpa, attr);
    }

    while (p.getCurrToken() != .RParen) {
        _ = try p.expectToken(.Comma);
        if (try p.attribute(.gnu, null)) |attr| {
            try p.attrBuffer.append(p.pp.comp.gpa, attr);
        }
    }
}

fn parseC23AttrList(p: *Parser) Error!void {
    while (p.getCurrToken() != .RBracket) { // ']'
        const namespaceTok = try p.expectIdentifier();
        var namespace: ?[]const u8 = null;
        if (p.eat(.ColonColon)) |_| {
            namespace = p.getTokenSlice(namespaceTok);
        } else {
            p.tokenIdx -= 1;
        }
        if (try p.attribute(.c23, namespace)) |attr|
            try p.attrBuffer.append(p.pp.comp.gpa, attr);
        _ = p.eat(.Comma);
    }
}

fn parseMSVCAttrList(p: *Parser) Error!void {
    while (p.getCurrToken() != .RParen) {
        if (try p.attribute(.declspec, null)) |attr|
            try p.attrBuffer.append(p.pp.comp.gpa, attr);
        _ = p.eat(.Comma);
    }
}

fn c23Attribute(p: *Parser) !bool {
    if (!p.pp.comp.langOpts.standard.atLeast(.c2x)) return false;
    const bracket1 = p.eat(.LBracket) orelse return false;
    const bracket2 = p.eat(.LBracket) orelse {
        p.tokenIdx -= 1;
        return false;
    };

    try p.parseC23AttrList();

    _ = try p.expectClosing(bracket2, .RBracket);
    _ = try p.expectClosing(bracket1, .RBracket);

    return true;
}

fn msvcAttribute(p: *Parser) !bool {
    const declspecTok = p.eat(.KeywordDeclSpec) orelse return false;
    if (!p.pp.comp.langOpts.declSpecAttrs) {
        try p.errToken(.declspec_not_enabled, declspecTok);
        return error.ParsingFailed;
    }

    const lparen = try p.expectToken(.LParen);
    try p.parseMSVCAttrList();
    _ = try p.expectClosing(lparen, .RParen);

    return false;
}

fn gnuAttribute(p: *Parser) !bool {
    switch (p.getCurrToken()) {
        .KeywordAttribute1, .KeywordAttribute2 => p.tokenIdx += 1,
        else => return false,
    }
    const paren1 = try p.expectToken(.LParen);
    const paren2 = try p.expectToken(.LParen);

    try p.parseGNUAttrList();

    _ = try p.expectClosing(paren2, .RParen);
    _ = try p.expectClosing(paren1, .RParen);
    return true;
}

/// alignAs : keyword_alignas '(' (typeName | constExpr ) ')'
fn alignAs(p: *Parser) !bool {
    const alignToken = p.eat(.KeywordAlignas) orelse return false;
    const lparen = try p.expectToken(.LParen);
    if (try p.parseTypeName()) |innerTy| {
        const alignment = Attribute.Alignment{ .requested = innerTy.alignof(p.pp.comp), .alignas = true };
        const attr = Attribute{ .tag = .aligned, .args = .{ .aligned = .{ .alignment = alignment, .__name_token = alignToken } } };
        try p.attrBuffer.append(p.pp.comp.gpa, .{ .attr = attr, .tok = alignToken });
    } else {
        const arg_start = p.tokenIdx;
        const res = try p.parseConstExpr(.NoConstDeclFolding);
        if (!res.value.isZero()) {
            var args = Attribute.initArguments(.aligned, alignToken);
            if (p.diagnose(.aligned, &args, 0, res)) |msg| {
                try p.errExtra(msg.tag, arg_start, msg.extra);
                p.skipTo(.RParen);
                return error.ParsingFailed;
            }
            args.aligned.alignment.?.node = res.node;
            args.aligned.alignment.?.alignas = true;
            try p.attrBuffer.append(p.pp.comp.gpa, .{ .attr = .{ .tag = .aligned, .args = args }, .tok = alignToken });
        }
    }
    try p.expectClosing(lparen, .RParen);
    return true;
}

/// attribute-specifier : (keyword_attrbute '( '(' attribute-list ')' ')')*
fn parseAttrSpec(p: *Parser) Error!void {
    while (true) {
        if (try p.alignAs()) continue;
        if (try p.gnuAttribute()) continue;
        if (try p.c23Attribute()) continue;
        if (try p.msvcAttribute()) continue;
        break;
    }
}

const InitDeclarator = struct { d: Declarator, initializer: Result = .{} };

/// init-declarator : declarator assembly? attribute-specifier? ('=' initializer)?
fn parseInitDeclarator(p: *Parser, declSpec: *DeclSpec) Error!?InitDeclarator {
    var ID = InitDeclarator{ .d = (try p.declarator(declSpec.type, .normal)) orelse return null };
    _ = try p.parseAssembly(.declLable);
    try p.parseAttrSpec(); //if (ID.d.type.isFunc()) .function else .variable

    if (p.eat(.Equal)) |eq| init: {
        if (declSpec.storageClass == .typedef or ID.d.funcDeclarator != null)
            try p.errToken(.illegal_initializer, eq)
        else if (ID.d.type.is(.VariableLenArray))
            try p.errToken(.vla_init, eq)
        else if (declSpec.storageClass == .@"extern") {
            try p.err(.extern_initializer);
            declSpec.storageClass = .none;
        }

        if (ID.d.type.hasIncompleteSize() and !ID.d.type.isArray()) {
            try p.errStr(.variable_incomplete_ty, ID.d.name, try p.typeStr(ID.d.type));
            return error.ParsingFailed;
        }

        try p.symStack.pushScope();
        defer p.symStack.popScope();

        try p.symStack.declareSymbol(ID.d.type, ID.d.name, .none);

        var initListExpr = try p.initializer(ID.d.type);
        ID.initializer = initListExpr;
        // int j [] = c; // c -> *int
        if (!initListExpr.ty.isArray()) break :init;
        if (ID.d.type.specifier == .IncompleteArray) {
            ID.d.type.data.array.len = initListExpr.ty.arrayLen() orelse break :init;
            ID.d.type.specifier = .Array;
        } else if (ID.d.type.is(.IncompleteArray)) {
            const attrs = ID.d.type.getAttributes();
            const arrayType = try p.arena.create(Type.Array);
            arrayType.* = .{
                .elem = ID.d.type.getElemType(),
                .len = initListExpr.ty.arrayLen().?,
            };
            const ty = Type{
                .specifier = .Array,
                .data = .{ .array = arrayType },
            };
            ID.d.type = try ty.withAttributes(p.arena, attrs);
        }
    }

    const name = ID.d.name;
    if (declSpec.storageClass != .typedef and ID.d.type.hasIncompleteSize()) incomplete: {
        const specifier = ID.d.type.canonicalize(.standard).specifier;
        if (declSpec.storageClass == .@"extern") switch (specifier) {
            .Struct, .Union, .Enum => break :incomplete,
            .IncompleteArray => {
                ID.d.type.decayArray();
                break :incomplete;
            },
            else => {},
        };

        // if there was an initializer expression it must have contained an error
        if (ID.initializer.node != .none)
            break :incomplete;
        try p.errStr(.variable_incomplete_ty, name, try p.typeStr(ID.d.type));
        return ID;
    }

    return ID;
}

/// type-specifier
///  : `void`
///  | `char`
///  | `short`
///  | `int`
///  | `long`
///  | `float`
///  | `double`
///  | `signed`
///  | `unsigned`
///  | `bool`
///  | `_Complex`
///  | atomic-type-specifier
///  | record-specifier
///  | enum-sepcifier
///  | typedef-name
///  | typeof-specifier
/// atomic-type-specifier
///   : keyword-atomic '(' typeName ')'
/// align-specifier
///   : keyword-alignas '(' typeName ')'
///   | keyword-alignas '(' constExpr ')'
fn parseTypeSpec(p: *Parser, ty: *TypeBuilder) Error!bool {
    const start = p.tokenIdx;
    while (true) {
        try p.parseAttrSpec(); // .typedef
        if (try p.typeof()) |innerType| {
            try ty.combineFromTypeof(p, innerType, start);
            continue;
        }

        if (try p.parseTypeQual(&ty.qual))
            continue;

        switch (p.getCurrToken()) {
            .KeywordVoid => try ty.combine(p, .Void, p.tokenIdx),
            .KeywordBool => try ty.combine(p, .Bool, p.tokenIdx),
            .KeywordChar => try ty.combine(p, .Char, p.tokenIdx),
            .KeywordShort => try ty.combine(p, .Short, p.tokenIdx),
            .KeywordInt => try ty.combine(p, .Int, p.tokenIdx),
            .KeywordLong => try ty.combine(p, .Long, p.tokenIdx),
            .KeywordSigned => try ty.combine(p, .Signed, p.tokenIdx),
            .KeywordUnsigned => try ty.combine(p, .Unsigned, p.tokenIdx),
            .KeywordFloat => try ty.combine(p, .Float, p.tokenIdx),
            .KeywordDouble => try ty.combine(p, .Double, p.tokenIdx),
            .KeywordComplex => try ty.combine(p, .Complex, p.tokenIdx),

            .KeywordAtomic => {
                const atomicToken = p.tokenIdx;
                p.tokenIdx += 1;
                const lp = p.eat(.LParen) orelse {
                    // _Atomic qualifier not _Atomic(typeName)
                    p.tokenIdx = atomicToken;
                    break;
                };
                const innerType = (try p.parseTypeName()) orelse {
                    try p.err(.expected_type);
                    return error.ParsingFailed;
                };

                try p.expectClosing(lp, .RParen);

                const newSpec = TypeBuilder.fromType(innerType);
                try ty.combine(p, newSpec, atomicToken);

                if (ty.qual.atomic != null)
                    try p.errStr(.duplicate_declspec, atomicToken, "atomic")
                else
                    ty.qual.atomic = atomicToken;

                continue;
            },

            .KeywordEnum => {
                const tagToken = p.tokenIdx;
                try ty.combine(p, .{ .Enum = try p.parseEnumSpec() }, tagToken);
                continue;
            },

            .KeywordStruct => {
                const tagToken = p.tokenIdx;
                try ty.combine(p, .{ .Struct = try p.parseRecordSpecifier() }, tagToken);
                continue;
            },

            .KeywordUnion => {
                const tagToken = p.tokenIdx;
                try ty.combine(p, .{ .Union = try p.parseRecordSpecifier() }, tagToken);
                continue;
            },

            .Identifier, .ExtendedIdentifier => {
                if (ty.typedef != null)
                    break;
                const typedef = (try p.symStack.findTypedef(p.tokenIdx, ty.specifier != .None)) orelse break;
                if (!ty.combineTypedef(p, typedef.type, typedef.token))
                    break;
            },
            else => break,
        }

        if (try p.eatIdentifier()) |_| {} else p.tokenIdx += 1;
    }

    return p.tokenIdx != start;
}

fn getAnonymousName(p: *Parser, kindToken: TokenIndex) ![]const u8 {
    const loc = p.pp.tokens.items(.loc)[kindToken];
    const source = p.pp.comp.getSource(loc.id);
    const lineAndCol = source.getLineCol(loc);

    const kindStr = switch (p.tokenIds[kindToken]) {
        .KeywordStruct,
        .KeywordUnion,
        .KeywordEnum,
        => p.getTokenSlice(kindToken),
        else => "record field",
    };

    return std.fmt.allocPrint(
        p.arena,
        "(anonymous {s} at {s}:{d}:{d})",
        .{ kindStr, source.path, lineAndCol.lineNO, lineAndCol.col },
    );
}

/// record-specifier
///  : StructOrUnion identifier? { record-declaration-list }
///  | StructOrUnion identifier
/// record-declaration-list
///  : record-declaration+
/// StructOrUnion
///  : 'struct'
///  | 'union'
fn parseRecordSpecifier(p: *Parser) Error!*Type.Record {
    const kindToken = p.tokenIdx;
    const isStruct = p.tokenIds[kindToken] == .KeywordStruct;
    p.tokenIdx += 1;

    const attrBufferTop = p.attrBuffer.len;
    defer p.attrBuffer.len = attrBufferTop;

    try p.parseAttrSpec(); // .record

    const maybeIdent = try p.eatIdentifier();
    const lb = p.eat(.LBrace) orelse {
        const ident = maybeIdent orelse {
            try p.err(.ident_or_l_brace);
            return error.ParsingFailed;
        };

        // check if this is a reference to a previous type
        if (try p.symStack.findTag(p.tokenIds[kindToken], ident)) |prev| {
            return prev.type.data.record;
        } else {
            // this is a forward declaration, create a new record type.
            const recordType = try Type.Record.create(p.arena, p.getTokenSlice(ident));
            const ty = try p.withAttributes(.{
                .specifier = if (isStruct) .Struct else .Union,
                .data = .{ .record = recordType },
            }, attrBufferTop);

            try p.symStack.symbols.append(p.pp.comp.gpa, .{
                .kind = if (isStruct) .@"struct" else .@"union",
                .name = recordType.name,
                .token = ident,
                .type = ty,
                .value = .{},
            });
            return recordType;
        }
    };

    // Get forward declared type or create a new one
    var defined = false;
    const recordType: *Type.Record = if (maybeIdent) |ident| recordTy: {
        if (try p.symStack.defineTag(p.tokenIds[kindToken], ident)) |prev| {
            if (!prev.type.hasIncompleteSize()) {
                // if the record isn't incomplete, this is a redefinition
                try p.errStr(.redefinition, ident, p.getTokenSlice(ident));
                try p.errToken(.previous_definition, prev.token);
            } else {
                defined = true;
                break :recordTy prev.type.get(if (isStruct) .Struct else .Union).?.data.record;
            }
        }
        break :recordTy try Type.Record.create(p.arena, p.getTokenSlice(ident));
    } else try Type.Record.create(p.arena, try p.getAnonymousName(kindToken));

    const ty = try p.withAttributes(.{
        .specifier = if (isStruct) .Struct else .Union,
        .data = .{ .record = recordType },
    }, attrBufferTop);

    // declare a symbol for the type
    if (maybeIdent != null and !defined) {
        try p.symStack.appendSymbol(.{
            .kind = if (isStruct) .@"struct" else .@"union",
            .name = p.getTokenSlice(maybeIdent.?),
            .token = maybeIdent.?,
            .type = ty,
            .value = .{},
        });
    }

    // reserve space for this record
    try p.declBuffer.append(.none);
    const declBufferTop = p.declBuffer.items.len;
    const recordBufferTop = p.recordBuffer.items.len;

    const oldRecord = p.record;
    const oldMembers = p.recordMembers.items.len;

    errdefer p.declBuffer.items.len = declBufferTop - 1;

    defer {
        p.declBuffer.items.len = declBufferTop;
        p.recordBuffer.items.len = recordBufferTop;
        p.record = oldRecord;
        p.recordMembers.items.len = oldMembers;
    }

    p.record = .{
        .kind = p.tokenIds[kindToken],
        .start = p.recordMembers.items.len,
    };

    try p.parseRecordDecls();
    if (p.record.flexibleField) |some| {
        if (p.recordBuffer.items[recordBufferTop..].len == 1 and isStruct)
            try p.errToken(.flexible_in_empty, some);
    }

    for (p.recordBuffer.items[recordBufferTop..]) |field| {
        if (field.ty.hasIncompleteSize()) break;
    } else {
        recordType.fields = try p.arena.dupe(Type.Record.Field, p.recordBuffer.items[recordBufferTop..]);
        recordType.size = 1;
        recordType.alignment = 1;
    }

    if (p.recordBuffer.items.len == recordBufferTop) {
        try p.errStr(.empty_record, kindToken, p.getTokenSlice(kindToken));
        try p.errStr(.empty_record_size, kindToken, p.getTokenSlice(kindToken));
    }

    try p.expectClosing(lb, .RBrace);
    try p.parseAttrSpec(); // .record

    // finish by creating a node
    var node: AST.Node = .{
        .tag = if (isStruct) .StructDeclTwo else .UnionDeclTwo,
        .type = ty,
        .data = .{ .binExpr = .{ .lhs = .none, .rhs = .none } },
    };
    const recordDLS = p.declBuffer.items[declBufferTop..];
    switch (recordDLS.len) {
        0 => {},
        1 => node.data = .{ .binExpr = .{ .lhs = recordDLS[0], .rhs = .none } },
        2 => node.data = .{ .binExpr = .{ .lhs = recordDLS[0], .rhs = recordDLS[1] } },
        else => {
            node.tag = if (isStruct) .StructDecl else .UnionDecl;
            node.data = .{ .range = try p.addList(recordDLS) };
        },
    }

    p.declBuffer.items[declBufferTop - 1] = try p.addNode(node);
    return recordType;
}

/// record-declarations
///  : record-declaration
///
/// record-declaration
///  : specifier-qualifier-list (record-declarator (',' record-declarator)*)? ';'
///  | static-assert-declaration ';'
fn parseRecordDecls(p: *Parser) Error!void {
    while (true) {
        if (try p.pragma()) continue;
        if (try p.parseOrNextDecl(parseStaticAssert)) continue;
        if (p.eat(.KeywordGccExtension)) |_| {
            const saveExtension = p.extensionSuppressd;
            defer p.extensionSuppressd = saveExtension;
            p.extensionSuppressd = true;

            if (try p.parseOrNextDecl(parseRecordDeclarator))
                continue;

            try p.err(.expected_type);
            p.nextExternDecl();
            continue;
        }

        if (try p.parseOrNextDecl(parseRecordDeclarator))
            continue;
        break;
    }
}

/// record-declarator : declarator (':' constant-expression)?
fn parseRecordDeclarator(p: *Parser) Error!bool {
    const attrBuffTop = p.attrBuffer.len;
    defer p.attrBuffer.len = attrBuffTop;
    const baseType = (try p.parseSpecQuals()) orelse return false;

    while (true) {
        const thisDeclTop = p.attrBuffer.len;
        defer p.attrBuffer.len = thisDeclTop;

        try p.parseAttrSpec(); //.record
        // 0 means unnamed
        var nameToken: TokenIndex = 0;
        var ty = baseType;
        var bitsNode: NodeIndex = .none;
        var bits: u32 = 0;
        const firstToken = p.tokenIdx;
        if (try p.declarator(ty, .record)) |d| {
            nameToken = d.name;
            ty = d.type;
        }

        try p.parseAttrSpec(); // .record
        ty = try p.withAttributes(ty, attrBuffTop);

        if (p.eat(.Colon)) |_| bits: {
            const bitsToken = p.tokenIdx;
            const res = try p.parseConstExpr(.GNUFoldingExtension);
            if (!ty.isInt()) {
                try p.errStr(.non_int_bitfield, firstToken, try p.typeStr(ty));
                break :bits;
            }

            if (res.value.tag == .unavailable) {
                try p.errToken(.expected_integer_constant_expr, bitsToken);
                break :bits;
            } else if (res.value.compare(.lt, Value.int(0), res.ty, p.pp.comp)) {
                try p.errExtra(.negative_bitwidth, firstToken, .{
                    .signed = res.value.signExtend(res.ty, p.pp.comp),
                });
                break :bits;
            }

            // incomplete size error is reported later
            const bitSize = ty.bitSizeof(p.pp.comp) orelse break :bits;
            if (res.value.compare(.gt, Value.int(bitSize), res.ty, p.pp.comp)) {
                try p.errToken(.bitfield_too_big, nameToken);
                break :bits;
            } else if (res.value.isZero() and nameToken != 0) {
                try p.errToken(.zero_width_named_field, nameToken);
                break :bits;
            }

            bits = res.value.getInt(u32);
            bitsNode = res.node;
        }

        if (nameToken == 0 and bitsNode == .none) unnamed: {
            // don't allow incompelete size fields in anonymous record.
            if (ty.is(.Enum) or ty.hasIncompleteSize()) break :unnamed;
            if (ty.isRecord() and ty.data.record.name[0] == '(') {
                // An anonymous record appears as indirect fields on the parent
                try p.recordBuffer.append(.{
                    .name = try p.getAnonymousName(firstToken),
                    .ty = ty,
                    .bitWidth = 0,
                });
                const node = try p.addNode(.{
                    .tag = .IndirectRecordFieldDecl,
                    .type = ty,
                    .data = undefined,
                });
                try p.declBuffer.append(node);
                try p.record.addFieldsFromAnonymous(p, ty);
                break; // must be followed by a semicolon
            }
            try p.err(.missing_declaration);
        } else {
            try p.recordBuffer.append(.{
                .name = if (nameToken != 0) p.getTokenSlice(nameToken) else try p.getAnonymousName(firstToken),
                .ty = ty,
                .nameToken = nameToken,
                .bitWidth = bits,
            });

            if (nameToken != 0)
                try p.record.addField(p, nameToken);
            const node = try p.addNode(.{
                .tag = .RecordFieldDecl,
                .type = ty,
                .data = .{ .decl = .{ .name = nameToken, .node = bitsNode } },
            });
            try p.declBuffer.append(node);
        }

        if (ty.isFunc()) {
            try p.errToken(.func_field, firstToken);
        } else if (ty.is(.VariableLenArray)) {
            try p.errToken(.vla_field, firstToken);
        } else if (ty.is(.IncompleteArray)) {
            if (p.record.kind == .KeywordUnion)
                try p.errToken(.flexible_in_union, firstToken);
            if (p.record.flexibleField) |some|
                try p.errToken(.flexible_non_final, some);

            p.record.flexibleField = firstToken;
        } else if (ty.hasIncompleteSize()) {
            try p.errStr(.field_incomplete_ty, firstToken, try p.typeStr(ty));
        } else if (p.record.flexibleField) |some| {
            if (some != firstToken)
                try p.errToken(.flexible_non_final, some);
        }
        if (p.eat(.Comma) == null) break;
    }
    _ = try p.expectToken(.Semicolon);
    return true;
}

fn checkAlignasUsage(p: *Parser, tag: Diagnostics.Tag, attrBufferStart: usize) !void {
    var i = attrBufferStart;
    while (i < p.attrBuffer.len) : (i += 1) {
        const tentativeAttr = p.attrBuffer.get(i);
        if (tentativeAttr.attr.tag != .aligned) continue;
        if (tentativeAttr.attr.args.aligned.alignment) |alignment| {
            if (alignment.alignas)
                try p.errToken(tag, tentativeAttr.tok);
        }
    }
}

// specifier-qualifier-list : (type-specifier | type-qualifier | align-specifier)+
fn parseSpecQuals(p: *Parser) Error!?Type {
    const attrBufferTop = p.attrBuffer.len;
    defer p.attrBuffer.len = attrBufferTop;

    var spec: TypeBuilder = .{};
    if (try p.parseTypeSpec(&spec)) {
        const ty = try spec.finish(p, attrBufferTop);
        try p.validateAlignas(ty, .align_ignored);
        return ty;
    }

    return null;
}

/// enum-specifier
///  : `enum` identifier? { enumerator (',' enumerator)? ',') }
///  | `enum` identifier
fn parseEnumSpec(p: *Parser) Error!*Type.Enum {
    const enumTK = p.tokenIdx;
    p.tokenIdx += 1;

    const attrBufferTop = p.attrBuffer.len;
    defer p.attrBuffer.len = attrBufferTop;

    try p.parseAttrSpec(); //.record

    const maybeID = try p.eatIdentifier();
    const lb = p.eat(.LBrace) orelse {
        const ident = maybeID orelse {
            try p.err(.ident_or_l_brace);
            return error.ParsingFailed;
        };

        // check if this is a reference to a previous type
        if (try p.symStack.findTag(.KeywordEnum, ident)) |prev| {
            return prev.type.data.@"enum";
        } else {
            // this is a forward declaration, create a new enum type
            const enumType = try Type.Enum.create(p.arena, p.getTokenSlice(ident));
            const ty = try p.withAttributes(
                .{ .specifier = .Enum, .data = .{ .@"enum" = enumType } },
                attrBufferTop,
            );
            try p.symStack.appendSymbol(.{
                .kind = .@"enum",
                .name = enumType.name,
                .token = ident,
                .type = ty,
                .value = .{},
            });
            return enumType;
        }
    };

    // Get forward declared type or create a new one
    var defined = false;
    const enumType: *Type.Enum = if (maybeID) |ident| enumTy: {
        if (try p.symStack.findTag(.KeywordEnum, ident)) |prev| {
            if (!prev.type.hasIncompleteSize()) {
                // if the enum isn't incomplete, this is a redefinition
                try p.errStr(.redefinition, ident, p.getTokenSlice(ident));
                try p.errToken(.previous_definition, prev.token);
            } else {
                defined = true;
                break :enumTy prev.type.get(.Enum).?.data.@"enum";
            }
        }
        break :enumTy try Type.Enum.create(p.arena, p.getTokenSlice(ident));
    } else try Type.Enum.create(p.arena, try p.getAnonymousName(enumTK));

    const ty = try p.withAttributes(
        .{
            .specifier = .Enum,
            .data = .{ .@"enum" = enumType },
        },
        attrBufferTop,
    );

    if (maybeID != null and !defined) {
        try p.symStack.appendSymbol(.{
            .kind = .@"enum",
            .name = p.getTokenSlice(maybeID.?),
            .type = ty,
            .token = maybeID.?,
            .value = .{},
        });
    }

    // reserve space for this enum
    try p.declBuffer.append(.none);
    const declBufferTop = p.declBuffer.items.len;
    const listBufferTop = p.listBuffer.items.len;
    const enumBufferTop = p.enumBuffer.items.len;
    errdefer p.declBuffer.items.len = declBufferTop - 1;

    defer {
        p.declBuffer.items.len = declBufferTop;
        p.listBuffer.items.len = listBufferTop;
        p.enumBuffer.items.len = enumBufferTop;
    }

    var e = Enumerator.init(p);
    while (try p.enumerator(&e)) |fieldAndNode| {
        try p.enumBuffer.append(fieldAndNode.field);
        try p.listBuffer.append(fieldAndNode.node);
        if (p.eat(.Comma) == null) break;
    }

    enumType.fields = try p.arena.dupe(Type.Enum.Field, p.enumBuffer.items[enumBufferTop..]);
    enumType.tagType = e.res.ty;

    if (p.enumBuffer.items.len == enumBufferTop)
        try p.err(.empty_enum);

    try p.expectClosing(lb, .RBrace);
    try p.parseAttrSpec(); //.record

    // finish by creating a node
    var node: AST.Node = .{
        .tag = .EnumDeclTwo,
        .type = ty,
        .data = .{ .binExpr = .{ .lhs = .none, .rhs = .none } },
    };

    const fieldNodes = p.listBuffer.items[listBufferTop..];
    switch (fieldNodes.len) {
        0 => {},
        1 => node.data = .{ .binExpr = .{ .lhs = fieldNodes[0], .rhs = .none } },
        2 => node.data = .{ .binExpr = .{ .lhs = fieldNodes[0], .rhs = fieldNodes[1] } },
        else => {
            node.tag = .EnumDecl;
            node.data = .{ .range = try p.addList(fieldNodes) };
        },
    }

    p.declBuffer.items[declBufferTop - 1] = try p.addNode(node);
    return enumType;
}

const Enumerator = struct {
    res: Result,

    fn init(p: *Parser) Enumerator {
        return .{ .res = .{
            .ty = .{ .specifier = if (p.pp.comp.langOpts.shortEnums) .SChar else .Int },
            .value = Value.int(0),
        } };
    }

    /// Increment enumerator value adjusting type if needed.
    fn incr(e: *Enumerator, p: *Parser) !void {
        e.res.node = .none;
        _ = e.res.value.add(e.res.value, Value.int(0), e.res.ty, p.pp.comp);
        // TODO adjust type if value does not fit current
    }

    /// Set enumerator value to specified value, adjusting type if needed.
    fn set(e: *Enumerator, _: *Parser, res: Result) !void {
        e.res = res;
        // TODO adjust res type to try to fit with the previous type
    }
};

const EnumFieldAndNode = struct { field: Type.Enum.Field, node: NodeIndex };

/// enumerator : identifier ('=' constant-expression)
fn enumerator(p: *Parser, e: *Enumerator) Error!?EnumFieldAndNode {
    _ = try p.pragma();
    const nameToken = try p.eatIdentifier() orelse {
        if (p.getCurrToken() == .RBrace) return null;
        try p.err(.expected_identifier);
        p.skipTo(.RBrace);
        return error.ParsingFailed;
    };

    const attrBufferTop = p.attrBuffer.len;
    defer p.attrBuffer.len = attrBufferTop;

    try p.parseAttrSpec();

    if (p.eat(.Equal)) |_| {
        const specified = try p.parseConstExpr(.GNUFoldingExtension);
        if (specified.value.tag == .unavailable) {
            try p.errToken(.enum_val_unavailable, nameToken + 2);
            try e.incr(p);
        } else {
            try e.set(p, specified);
        }
    } else {
        try e.incr(p);
    }

    var res = e.res;
    res.ty = try p.withAttributes(res.ty, attrBufferTop);

    try p.symStack.defineEnumeration(res.ty, nameToken);
    const node = try p.addNode(.{
        .tag = .EnumFieldDecl,
        .type = res.ty,
        .data = .{
            .decl = .{
                .name = nameToken,
                .node = res.node,
            },
        },
    });

    return EnumFieldAndNode{
        .field = .{
            .name = p.getTokenSlice(nameToken),
            .ty = res.ty,
            .nameToken = nameToken,
            .node = res.node,
        },
        .node = node,
    };
}

/// atomic-type-specifier : keyword_atomic '(' type-name ')'
/// type-qualifier
/// : keyword_const
/// | keyword_restrict
/// | keyword_volatile
/// | keyword_atomic
fn parseTypeQual(p: *Parser, b: *Type.Qualifiers.Builder) Error!bool {
    var any = false;

    while (true) {
        switch (p.getCurrToken()) {
            .KeywordRestrict,
            .KeywordGccRestrict1,
            .KeywordGccRestrict2,
            => {
                if (b.restrict != null)
                    try p.errStr(.duplicate_declspec, p.tokenIdx, "restrict")
                else
                    b.restrict = p.tokenIdx;
            },

            .KeywordConst,
            .KeywordGccConst1,
            .KeywordGccConst2,
            => {
                if (b.@"const" != null)
                    try p.errStr(.duplicate_declspec, p.tokenIdx, "const")
                else
                    b.@"const" = p.tokenIdx;
            },

            .KeywordVolatile, .KeywordGccVolatile1, .KeywordGccVolatile2 => {
                if (b.@"volatile" != null)
                    try p.errStr(.duplicate_declspec, p.tokenIdx, "volatile")
                else
                    b.@"volatile" = p.tokenIdx;
            },

            .KeywordAtomic => {
                // _Atomic(typeName) instead of just _Atomic
                if (p.tokenIds[p.tokenIdx + 1] == .LParen) break;
                if (b.atomic != null)
                    try p.errStr(.duplicate_declspec, p.tokenIdx, "atomic")
                else
                    b.atomic = p.tokenIdx;
            },

            else => break,
        }
        p.tokenIdx += 1;
        any = true;
    }

    return any;
}

const Declarator = struct {
    name: TokenIndex, //
    type: Type,
    funcDeclarator: ?TokenIndex = null,
    oldTypeFunc: ?TokenIndex = null,
};

const DeclaratorKind = enum { normal, abstract, param, record };

/// declarator: pointer? direct-declarator
/// abstract-declarator
///  : pointer
///  : pointer? direct-abstract-declarator
fn declarator(p: *Parser, baseType: Type, kind: DeclaratorKind) Error!?Declarator {
    const start = p.tokenIdx;
    var d = Declarator{ .name = 0, .type = try p.parsePointer(baseType) };

    const attrBufferTop = p.attrBuffer.len;
    defer p.attrBuffer.len = attrBufferTop;

    const maybeIdent = p.tokenIdx;
    if (kind != .abstract and (try p.eatIdentifier()) != null) {
        d.name = maybeIdent;
        const combineToken = p.tokenIdx;
        d.type = try p.directDeclarator(d.type, &d, kind);
        try d.type.validateCombinedType(p, combineToken);
        d.type = try p.withAttributes(d.type, attrBufferTop);
        return d;
    } else if (p.eat(.LParen)) |lp| blk: {
        var res = (try p.declarator(.{ .specifier = .Void }, kind)) orelse {
            p.tokenIdx = lp;
            break :blk;
        };

        try p.expectClosing(lp, .RParen);
        const suffixStart = p.tokenIdx;
        const outer = try p.directDeclarator(d.type, &d, kind);

        try res.type.combine(outer, p, res.funcDeclarator orelse suffixStart);
        try res.type.validateCombinedType(p, suffixStart);
        res.oldTypeFunc = d.oldTypeFunc;
        return res;
    }

    const expectedIdent = p.tokenIdx;

    d.type = try p.directDeclarator(d.type, &d, kind);
    if (kind == .normal and !d.type.isEnumOrRecord()) {
        try p.errToken(.expected_ident_or_l_paren, expectedIdent);
        return error.ParsingFailed;
    }

    d.type = try p.withAttributes(d.type, attrBufferTop);

    if (start == p.tokenIdx)
        return null;

    try d.type.validateCombinedType(p, expectedIdent);

    return d;
}

/// direct-declarator
///  : identifier attribute-specifier-sequence?
///  | ( declarator )?
///  | array-declarator attribute-specifier-sequence?
///  | function-declarator attribute-specifier-sequence?
///
/// array-declarator
///  : '[' type-qualifier-list* assignExpr? ']' direct-declarator?
///  | '[' `static` type-qualifier-list* assign-expression ']' direct-declarator?
///  | '[' type-qualifier-list+ `static` assign-expression ']' direct-declarator?
///  | '[' type-qualifier-list* '*' ']' direct-declarator?
///
/// function-declarator
///  | '(' param-decls ')' direct-declarator?
///  | '(' identifier-list? ')' direct-declarator?
///
/// identifier-list : identifier (',' identifier)*
///
/// direct-abstract-declarator
///  : '[' type-qualifier-list* assign-expression? ']'
///  | '[' `static` type-qualifier-list* assign-expression ']'
///  | '[' type-qualifier-list+ `static` assign-expression ']'
///  | '[' '*' ']'
///  | '(' param-decls? ')'
fn directDeclarator(p: *Parser, baseType: Type, d: *Declarator, kind: DeclaratorKind) Error!Type {
    try p.parseAttrSpec();
    if (p.eat(.LBracket)) |lb| {
        var resType = Type{ .specifier = .Pointer };
        var qualsBuilder = Type.Qualifiers.Builder{};
        var gotQuals = try p.parseTypeQual(&qualsBuilder);
        var static = p.eat(.KeywordStatic);

        if (static != null and !gotQuals)
            gotQuals = try p.parseTypeQual(&qualsBuilder);

        var star = p.eat(.Asterisk);
        const sizeToken = p.tokenIdx;

        const constDeclFolding = p.constDeclFolding;
        p.constDeclFolding = .GNUVLAFoldingExtension;
        const size = if (star) |_| Result{} else try p.parseAssignExpr();
        p.constDeclFolding = constDeclFolding;

        try p.expectClosing(lb, .RBracket);

        if (star != null and static != null) {
            try p.errToken(.invalid_static_star, static.?);
        }

        if (kind != .param) {
            if (static != null)
                try p.errToken(.static_non_param, lb)
            else if (gotQuals)
                try p.errToken(.array_qualifiers, lb);

            if (star) |some|
                try p.errToken(.star_non_param, some);

            static = null;
            qualsBuilder = .{};
            star = null;
        } else {
            try qualsBuilder.finish(p, &resType);
        }

        if (static) |_|
            try size.expect(p);

        const outer = try p.directDeclarator(baseType, d, kind);
        var maxBits = p.pp.comp.target.ptrBitWidth();
        if (maxBits > 61) maxBits = 61;

        // `outer` is validated later so it may be invalid here
        const outerSize = if (outer.hasIncompleteSize()) 1 else outer.sizeof(p.pp.comp);
        const maxBytes = (@as(u64, 1) << @as(u6, @truncate(maxBits))) - 1;
        const maxElems = maxBytes / @max(1, outerSize orelse 1);

        if (!size.ty.isInt()) {
            try p.errStr(.array_size_non_int, sizeToken, try p.typeStr(size.ty));
            return error.ParsingFailed;
        }

        if (size.value.tag == .unavailable) {
            if (size.node != .none) {
                try p.errToken(.vla, sizeToken);
                if (p.func.type == null and kind != .param and p.record.kind == .Invalid)
                    try p.errToken(.variable_len_array_file_scope, d.name);

                const exprType = try p.arena.create(Type.Expr);
                exprType.ty = .{ .specifier = .Void };
                exprType.node = size.node;
                resType.data = .{ .expr = exprType };
                resType.specifier = .VariableLenArray;

                if (static) |some|
                    try p.errToken(.useless_static, some);
            } else if (star) |_| {
                const elemType = try p.arena.create(Type);
                elemType.* = .{ .specifier = .Void };
                resType.data = .{ .subType = elemType };
                resType.specifier = .UnspecifiedVariableLenArray;
            } else {
                const arrayType = try p.arena.create(Type.Array);
                arrayType.elem = .{ .specifier = .Void };
                arrayType.len = 0;
                resType.data = .{ .array = arrayType };
                resType.specifier = .IncompleteArray;
            }
        } else {
            var sizeValue = size.value;
            const size_t = p.pp.comp.types.size;

            if (sizeValue.compare(.lt, Value.int(0), size_t, p.pp.comp))
                try p.errToken(.negative_array_size, lb);

            const arrayType = try p.arena.create(Type.Array);
            arrayType.elem = .{ .specifier = .Void };
            if (sizeValue.compare(.gt, Value.int(maxElems), size_t, p.pp.comp)) {
                try p.errToken(.array_too_large, lb);
                arrayType.len = maxElems;
            } else {
                arrayType.len = sizeValue.getInt(u64);
            }
            resType.data = .{ .array = arrayType };
            resType.specifier = .Array;
        }

        try resType.combine(outer, p, lb);
        return resType;
    } else if (p.eat(.LParen)) |lp| {
        d.funcDeclarator = lp;

        const funcType = try p.arena.create(Type.Function);
        funcType.params = &.{};
        funcType.returnType.specifier = .Void;
        var specifier: Type.Specifier = .Func;

        if (p.eat(.Ellipsis)) |_| {
            try p.err(.param_before_var_args);
            try p.expectClosing(lp, .RParen);
            var resType = Type{ .specifier = .Func, .data = .{ .func = funcType } };

            const outer = try p.directDeclarator(baseType, d, kind);
            try resType.combine(outer, p, lp);
            return resType;
        }

        if (try p.parseParamDecls()) |params| {
            funcType.params = params;
            if (p.eat(.Ellipsis)) |_|
                specifier = .VarArgsFunc;
        } else if (p.getCurrToken() == .RParen) {
            specifier = .OldStyleFunc;
        } else if (p.getCurrToken() == .Identifier or p.getCurrToken() == .ExtendedIdentifier) {
            d.oldTypeFunc = p.tokenIdx;

            const paramBufferTop = p.paramBuffer.items.len;
            try p.symStack.pushScope();

            defer {
                p.paramBuffer.items.len = paramBufferTop;
                p.symStack.popScope();
            }

            specifier = .OldStyleFunc;
            while (true) {
                const nameToken = try p.expectIdentifier();
                try p.symStack.defineParam(undefined, nameToken);
                try p.paramBuffer.append(.{
                    .name = p.getTokenSlice(nameToken),
                    .nameToken = nameToken,
                    .ty = .{ .specifier = .Int },
                });

                if (p.eat(.Comma) == null) break;
            }

            funcType.params = try p.arena.dupe(Type.Function.Param, p.paramBuffer.items[paramBufferTop..]);
        } else {
            try p.err(.expected_param_decl);
        }

        try p.expectClosing(lp, .RParen);
        var resType = Type{
            .specifier = specifier,
            .data = .{ .func = funcType },
        };

        const outer = try p.directDeclarator(baseType, d, kind);
        try resType.combine(outer, p, lp);
        return resType;
    } else {
        return baseType;
    }
}

/// pointer : '*' type-qualifier-list* pointer?
fn parsePointer(p: *Parser, baseType: Type) Error!Type {
    var ty = baseType;
    while (p.eat(.Asterisk)) |_| {
        const elemType = try p.arena.create(Type);
        elemType.* = ty;
        ty = Type{
            .specifier = .Pointer,
            .data = .{ .subType = elemType },
        };

        var qualsBuilder = Type.Qualifiers.Builder{};
        _ = try p.parseTypeQual(&qualsBuilder);
        try qualsBuilder.finish(p, &ty);
    }

    return ty;
}

/// param-decls : param-decl (',' param-decl)* (',' '...')
/// paramDecl : decl-specifier (declarator | abstract-declarator)
fn parseParamDecls(p: *Parser) Error!?[]Type.Function.Param {
    const paramBufferTop = p.paramBuffer.items.len;

    try p.symStack.pushScope();

    defer {
        p.paramBuffer.items.len = paramBufferTop;
        p.symStack.popScope();
    }

    while (true) {
        const paramDeclSpec = if (try p.parseDeclSpec(true)) |some|
            some
        else if (p.paramBuffer.items.len == paramBufferTop)
            return null
        else blk: {
            var spec: TypeBuilder = .{};
            break :blk DeclSpec{ .type = try spec.finish(p, p.attrBuffer.len) };
        };

        var nameToken: TokenIndex = 0;
        const firstToken = p.tokenIdx;
        var paramType = paramDeclSpec.type;
        if (try p.declarator(paramDeclSpec.type, .param)) |some| {
            if (some.oldTypeFunc) |tokenIdx|
                try p.errToken(.invalid_old_style_params, tokenIdx);

            // parse attributes at end of formal parameters
            const attrBufferTop = p.attrBuffer.len;
            defer p.attrBuffer.len = attrBufferTop;

            try p.parseAttrSpec();

            nameToken = some.name;
            paramType = try p.withAttributes(some.type, attrBufferTop);

            if (some.name != 0)
                try p.symStack.defineParam(paramType, nameToken);
        }

        if (paramType.isFunc()) {
            // params declared as functions are converted to function pointers
            const elemType = try p.arena.create(Type);
            elemType.* = paramType;
            paramType = Type{
                .specifier = .Pointer,
                .data = .{ .subType = elemType },
            };
        } else if (paramType.isArray()) {
            // params declared as array are converted to pointers
            paramType.decayArray();
        } else if (paramType.is(.Void)) {
            // validate void parameters
            if (p.paramBuffer.items.len == paramBufferTop) {
                if (p.getCurrToken() != .RParen) {
                    try p.err(.void_only_param);
                    if (paramType.containAnyQual())
                        try p.err(.void_param_qualified);
                    return error.ParsingFailed;
                }
                return &[0]Type.Function.Param{};
            }

            try p.err(.void_must_be_first_param);

            return error.ParsingFailed;
        }

        try paramDeclSpec.validateParam(p, &paramType);
        try p.paramBuffer.append(.{
            .name = if (nameToken == 0) "" else p.getTokenSlice(nameToken),
            .nameToken = if (nameToken == 0) firstToken else nameToken,
            .ty = paramType,
        });

        if (p.eat(.Comma) == null)
            break;

        if (p.getCurrToken() == .Ellipsis)
            break;
    }

    return try p.arena.dupe(Type.Function.Param, p.paramBuffer.items[paramBufferTop..]);
}

/// type-name : specifier-qualifier-list+ abstract-declarator
fn parseTypeName(p: *Parser) Error!?Type {
    const ty = (try p.parseSpecQuals()) orelse return null;
    if (try p.declarator(ty, .abstract)) |some| {
        if (some.oldTypeFunc) |tokenIdx|
            try p.errToken(.invalid_old_style_params, tokenIdx);

        return some.type;
    } else return ty;
}

/// initializer
///  : assign-expression
///  : braced-initializer
///
/// braced-initializer
///  | '{' initializerItems '}'
pub fn initializer(p: *Parser, initType: Type) Error!Result {
    // fast path for non-braced initializers
    if (p.getCurrToken() != .LBrace) {
        const token = p.tokenIdx;
        var res = try p.parseAssignExpr();
        try res.expect(p);
        if (try p.coerceArrayInit(&res, token, initType))
            return res;

        try p.coerceInit(&res, token, initType);
        return res;
    }

    var il: InitList = .{};
    defer il.deinit(p.pp.comp.gpa);

    _ = try p.initializerItem(&il, initType);

    const res = try p.convertInitList(il, initType);
    var resType = p.nodes.items(.type)[@intFromEnum(res)];
    resType.qual = initType.qual;
    return Result{ .ty = resType, .node = res };
}

/// initializerItems : designation? initializer (',' designation? initializer)* ','?
/// designation : designator-list '='
/// designator-list:
///  : designator designator-list designator
/// designator
///  : '[' constant-expression ']'
///  | '.' identifier
pub fn initializerItem(p: *Parser, il: *InitList, initType: Type) Error!bool {
    const lb = p.eat(.LBrace) orelse {
        const token = p.tokenIdx;
        var res = try p.parseAssignExpr();
        if (res.empty(p))
            return false;

        const arr = try p.coerceArrayInit(&res, token, initType);
        if (!arr)
            try p.coerceInit(&res, token, initType);

        if (il.tok != 0) {
            try p.errToken(.initializer_overrides, token);
            try p.errToken(.previous_initializer, il.tok);
        }
        il.node = res.node;
        il.tok = token;
        return true;
    };

    const isScalar = initType.isInt() or initType.isFloat() or initType.isPointer();
    if (p.eat(.RBrace)) |_| {
        if (isScalar)
            try p.errToken(.empty_scalar_init, lb);

        if (il.tok != 0) {
            try p.errToken(.initializer_overrides, lb);
            try p.errToken(.previous_initializer, il.tok);
        }
        il.node = .none;
        il.tok = lb;
        return true;
    }

    var count: u64 = 0;
    var warnedExcess = false;
    const isStrInit = false;
    var indexHint: ?usize = null;
    while (true) : (count += 1) {
        errdefer p.skipTo(.RBrace);

        const firstToken = p.tokenIdx;
        var curType = initType;
        var curIL = il;
        var designation = false;
        var curIndexHint: ?usize = null;
        while (true) {
            if (p.eat(.LBracket)) |lbr| {
                if (!curType.isArray()) {
                    try p.errStr(.invalid_array_designator, lbr, try p.typeStr(curType));
                    return error.ParsingFailed;
                }

                const exprToken = p.tokenIdx;
                const indexRes = try p.parseConstExpr(.GNUFoldingExtension);
                try p.expectClosing(lbr, .RBracket);

                if (indexRes.value.tag == .unavailable) {
                    try p.errToken(.expected_integer_constant_expr, exprToken);
                    return error.ParsingFailed;
                } else if (indexRes.value.compare(.lt, indexRes.value.zero(), indexRes.ty, p.pp.comp)) {
                    try p.errExtra(.negative_array_designator, lb + 1, .{
                        .signed = indexRes.value.signExtend(indexRes.ty, p.pp.comp),
                    });
                    return error.ParsingFailed;
                }

                const maxLen = curType.arrayLen() orelse std.math.maxInt(usize);
                if (indexRes.value.data.int >= maxLen) {
                    try p.errExtra(.oob_array_designator, lbr + 1, .{ .unsigned = indexRes.value.data.int });
                    return error.ParsingFailed;
                }

                const checked = indexRes.value.getInt(u64);
                curIndexHint = curIndexHint orelse checked;

                curIL = try curIL.find(p.pp.comp.gpa, checked);
                curType = curType.getElemType();
                designation = true;
            } else if (p.eat(.Period)) |period| {
                const fieldName = p.getTokenSlice(try p.expectIdentifier());
                curType = curType.canonicalize(.standard);
                if (!curType.isRecord()) {
                    try p.errStr(.invalid_field_designator, period, try p.typeStr(curType));
                    return error.ParsingFailed;
                }

                // TODO check if union already has field set
                outer: while (true) {
                    for (curType.data.record.fields, 0..) |f, i| {
                        if (f.isAnonymousRecord()) {
                            // Recurse into anonymous field if it has a field by the name.
                            if (!f.ty.hasField(fieldName)) continue;
                            curType = f.ty.canonicalize(.standard);
                            curIL = try il.find(p.pp.comp.gpa, i);
                            curIndexHint = curIndexHint orelse i;
                            continue :outer;
                        }

                        if (std.mem.eql(u8, fieldName, f.name)) {
                            curIL = try curIL.find(p.pp.comp.gpa, i);
                            curType = f.ty;
                            curIndexHint = curIndexHint orelse i;
                            break :outer;
                        }
                    }
                    unreachable; // we already checked that the starting type has this field
                }

                designation = true;
            } else break;
        }

        if (designation) indexHint = null;
        defer indexHint = curIndexHint orelse null;

        if (designation)
            _ = try p.expectToken(.Equal);

        var saw = false;

        if (isStrInit and p.isStringInit(initType)) {
            var tempIL = InitList{};
            defer tempIL.deinit(p.pp.comp.gpa);
            saw = try p.initializerItem(&tempIL, .{ .specifier = .Void });
        } else if (count == 0 and p.isStringInit(initType)) {
            saw = try p.initializerItem(il, initType);
        } else if (isScalar and count != 0) {
            // discard further scalars
            var tempIL = InitList{};
            defer tempIL.deinit(p.pp.comp.gpa);
            saw = try p.initializerItem(&tempIL, .{ .specifier = .Void });
        } else if (p.getCurrToken() == .LBrace) {
            if (designation) {
                // designation overrides previous value, let existing mechanism handle it
                saw = try p.initializerItem(curIL, curType);
            } else if (try p.findAggregateInitializer(&curIL, &curType, &indexHint)) {
                saw = try p.initializerItem(curIL, curType);
            } else {
                // discard further values
                var tempIL = InitList{};
                defer tempIL.deinit(p.pp.comp.gpa);

                saw = try p.initializerItem(curIL, curType);
                saw = try p.initializerItem(&tempIL, .{ .specifier = .Void });
                if (!warnedExcess)
                    try p.errToken(if (initType.isArray()) .excess_array_init else .excess_struct_init, firstToken);
                warnedExcess = true;
            }
        } else if (indexHint != null and try p.findScalarInitializerAt(&curIL, &curType, &indexHint.?)) {
            saw = try p.initializerItem(curIL, curType);
        } else if (try p.findScalarInitializer(&curIL, &curType)) {
            saw = try p.initializerItem(curIL, curType);
        } else if (designation) {
            // designation overrides previous value, let existing mechanism handle it
            saw = try p.initializerItem(curIL, curType);
        } else {
            // discard further values
            var tempIL = InitList{};
            defer tempIL.deinit(p.pp.comp.gpa);
            saw = try p.initializerItem(&tempIL, .{ .specifier = .Void });
            if (!warnedExcess and saw)
                try p.errToken(if (initType.isArray()) .excess_array_init else .excess_struct_init, firstToken);
            warnedExcess = true;
        }

        if (!saw) {
            if (designation) {
                try p.err(.expected_expr);
                return error.ParsingFailed;
            }
            break;
        } else if (count == 1) {
            if (isStrInit) try p.errToken(.excess_str_init, firstToken);
            if (isScalar) try p.errToken(.excess_scalar_init, firstToken);
        }

        if (p.eat(.Comma) == null) break;
    }
    try p.expectClosing(lb, .RBrace);

    if (isScalar or isStrInit)
        return true;

    if (il.tok != 0) {
        try p.errToken(.initializer_overrides, lb);
        try p.errToken(.previous_initializer, il.tok);
    }
    il.node = .none;
    il.tok = lb;
    return true;
}

/// returns true if the value is unused
fn findScalarInitializerAt(p: *Parser, il: **InitList, ty: *Type, startIdx: *usize) Error!bool {
    if (ty.isArray()) {
        startIdx.* += 1;

        const arrType = ty.*;
        const elemCount = arrType.arrayLen() orelse std.math.maxInt(usize);
        if (elemCount == 0) {
            if (p.getCurrToken() != .LBrace) {
                try p.err(.empty_aggregate_init_braces);
                return error.ParsingFailed;
            }
            return false;
        }

        const elemType = arrType.getElemType();
        const arrIL = il.*;
        if (startIdx.* < elemCount) {
            ty.* = elemType;
            il.* = try arrIL.find(p.pp.comp.gpa, startIdx.*);
            _ = try p.findScalarInitializer(il, ty);
            return true;
        }
        return false;
    } else if (ty.get(.Struct)) |structType| {
        startIdx.* += 1;
        const fieldCount = structType.data.record.fields.len;
        if (fieldCount == 0) {
            if (p.getCurrToken() != .LBrace) {
                try p.err(.empty_aggregate_init_braces);
                return error.ParsingFailed;
            }
            return false;
        }

        const structIL = il.*;
        if (startIdx.* < fieldCount) {
            const field = structType.data.record.fields[startIdx.*];
            ty.* = field.ty;
            il.* = try structIL.find(p.pp.comp.gpa, startIdx.*);
            _ = try p.findScalarInitializer(il, ty);
            return true;
        }
        return false;
    } else if (ty.get(.Union)) |_| {
        return false;
    }
    return il.*.node == .none;
}

/// Returns true if the value is unused.
fn findScalarInitializer(p: *Parser, il: **InitList, ty: *Type) Error!bool {
    if (ty.isArray()) {
        var index = il.*.list.items.len;
        if (index != 0) index = il.*.list.items[index - 1].index;

        const arrayType = ty.*;
        const elemCount = arrayType.arrayLen() orelse std.math.maxInt(usize);
        if (elemCount == 0) {
            if (p.getCurrToken() != .LBrace) {
                try p.err(.empty_aggregate_init_braces);
                return error.ParsingFailed;
            }
            return false;
        }
        const elemType = arrayType.getElemType();
        const arrayIL = il.*;
        while (index < elemCount) : (index += 1) {
            ty.* = elemType;
            il.* = try arrayIL.find(p.pp.comp.gpa, index);
            if (try p.findScalarInitializer(il, ty))
                return true;
        }
        return false;
    } else if (ty.get(.Struct)) |structType| {
        var index = il.*.list.items.len;
        if (index != 0) index = il.*.list.items[index - 1].index + 1;

        const fieldCount = structType.data.record.fields.len;
        if (fieldCount == 0) {
            if (p.getCurrToken() == .LBrace) {
                try p.err(.empty_aggregate_init_braces);
                return error.ParsingFailed;
            }
            return false;
        }
        const structIL = il.*;
        while (index < fieldCount) : (index += 1) {
            const field = structType.data.record.fields[index];
            ty.* = field.ty;
            il.* = try structIL.find(p.pp.comp.gpa, index);
            if (try p.findScalarInitializer(il, ty))
                return true;
        }
        return false;
    } else if (ty.get(.Union)) |unionType| {
        if (unionType.data.record.fields.len == 0) {
            if (p.getCurrToken() == .LBrace) {
                try p.err(.empty_aggregate_init_braces);
                return error.ParsingFailed;
            }
            return false;
        }
        ty.* = unionType.data.record.fields[0].ty;
        il.* = try il.*.find(p.pp.comp.gpa, 0);
        if (try p.findScalarInitializer(il, ty))
            return true;
        return false;
    }
    return il.*.node == .none;
}

fn findAggregateInitializer(p: *Parser, il: **InitList, ty: *Type, startIdx: *?usize) Error!bool {
    if (ty.isArray()) {
        var index = il.*.list.items.len;
        if (index != 0) index = il.*.list.items[index - 1].index + 1;
        if (startIdx.*) |*some| {
            some.* += 1;
            index = some.*;
        }

        const arrType = ty.*;
        const elemCount = arrType.arrayLen() orelse std.math.maxInt(usize);
        const elemType = arrType.getElemType();
        if (index < elemCount) {
            ty.* = elemType;
            il.* = try il.*.find(p.pp.comp.gpa, index);
            return true;
        }
        return false;
    } else if (ty.get(.Struct)) |structType| {
        var index = il.*.list.items.len;
        if (index != 0) index = il.*.list.items[index - 1].index + 1;
        if (startIdx.*) |*some| {
            some.* += 1;
            index = some.*;
        }

        const fieldCount = structType.data.record.fields.len;
        if (index < fieldCount) {
            ty.* = structType.data.record.fields[index].ty;
            il.* = try il.*.find(p.pp.comp.gpa, index);
            return true;
        }
        return false;
    } else if (ty.get(.Union)) |unionType| {
        if (startIdx.*) |_| return false; // overrides
        if (unionType.data.record.fields.len == 0)
            return false;
        ty.* = unionType.data.record.fields[0].ty;
        il.* = try il.*.find(p.pp.comp.gpa, 0);
        return true;
    } else {
        try p.err(.too_many_scalar_init_braces);
        return il.*.node == .none;
    }
}

fn coerceArrayInit(p: *Parser, item: *Result, token: TokenIndex, target: Type) !bool {
    if (!target.isArray())
        return false;

    const isStrLiteral = p.nodeIs(item.node, .StringLiteralExpr);
    if (!isStrLiteral and !p.nodeIs(item.node, .CompoundLiteralExpr)) {
        try p.errToken(.array_init_str, token);
        return true; // do not do further coercion
    }

    const targetSpec = target.getElemType().canonicalize(.standard).specifier;
    const itemSpec = item.ty.getElemType().canonicalize(.standard).specifier;
    const compatible = target.getElemType().eql(item.ty.getElemType(), p.pp.comp, false) or
        (isStrLiteral and itemSpec == .Char and (targetSpec == .UChar or targetSpec == .SChar));

    if (!compatible) {
        const eMsg = " with array of type ";
        try p.errStr(.incompatible_array_init, token, try p.typePairStrExtra(target, eMsg, item.ty));
        return true; // do not do further coercion
    }

    if (target.get(.Array)) |arrayType| {
        std.debug.assert(item.ty.is(.Array));
        const len = item.ty.arrayLen().?;
        const arrayLen = arrayType.arrayLen().?;
        if (isStrLiteral) {
            // the null byte of a string can be dropped
            if (len - 1 > arrayLen)
                try p.errToken(.str_init_too_long, token);
        } else if (len > arrayLen) {
            try p.errStr(
                .arr_init_too_long,
                token,
                try p.typePairStrExtra(target, " with array of type ", item.ty),
            );
        }
    }
    return true;
}

fn coerceInit(p: *Parser, item: *Result, token: TokenIndex, target: Type) !void {
    // Do not do type coercion on excess items
    if (target.is(.Void))
        return;

    // item does not need to be qualified
    var unqualType = target.canonicalize(.standard);
    unqualType.qual = .{};
    const eMsg = " from incompatible type ";
    try item.lvalConversion(p);
    if (unqualType.is(.Bool)) {
        // this is ridiculous but it's what clang does
        if (item.ty.isInt() or item.ty.isFloat() or item.ty.isPointer()) {
            try item.boolCast(p, unqualType, token);
        } else {
            try p.errStr(.incompatible_init, token, try p.typePairStrExtra(target, eMsg, item.ty));
        }
    } else if (unqualType.isInt()) {
        if (item.ty.isInt() or item.ty.isFloat()) {
            try item.intCast(p, unqualType, token);
        } else if (item.ty.isPointer()) {
            try p.errStr(.implicit_ptr_to_int, token, try p.typePairStrExtra(item.ty, " to ", target));
            try item.intCast(p, unqualType, token);
        } else {
            try p.errStr(.incompatible_init, token, try p.typePairStrExtra(target, eMsg, item.ty));
        }
    } else if (unqualType.isFloat()) {
        if (item.ty.isInt() or item.ty.isFloat()) {
            try item.floatCast(p, unqualType);
        } else {
            try p.errStr(.incompatible_init, token, try p.typePairStrExtra(target, eMsg, item.ty));
        }
    } else if (unqualType.isPointer()) {
        if (item.value.isZero()) {
            try item.nullCast(p, target);
        } else if (item.ty.isInt()) {
            try p.errStr(.implicit_int_to_ptr, token, try p.typePairStrExtra(item.ty, " to ", target));
            try item.ptrCast(p, unqualType);
        } else if (item.ty.isPointer()) {
            if (!item.ty.isVoidStar() and !unqualType.isVoidStar() and !unqualType.eql(item.ty, p.pp.comp, false)) {
                try p.errStr(.incompatible_ptr_init, token, try p.typePairStrExtra(target, eMsg, item.ty));
                try item.ptrCast(p, unqualType);
            } else if (!unqualType.eql(item.ty, p.pp.comp, true)) {
                if (!unqualType.getElemType().qual.hasQuals(item.ty.getElemType().qual))
                    try p.errStr(.ptr_init_discards_quals, token, try p.typePairStrExtra(target, eMsg, item.ty));

                try item.ptrCast(p, unqualType);
            }
        } else {
            try p.errStr(.incompatible_init, token, try p.typePairStrExtra(target, eMsg, item.ty));
        }
    } else if (unqualType.isRecord()) {
        if (!unqualType.eql(item.ty, p.pp.comp, false))
            try p.errStr(.incompatible_init, token, try p.typePairStrExtra(target, eMsg, item.ty));
    } else if (unqualType.isArray() or unqualType.isFunc()) {
        // we have already issued an error for this
    } else {
        try p.errStr(.incompatible_init, token, try p.typePairStrExtra(target, eMsg, item.ty));
    }
}

fn isStringInit(p: *Parser, ty: Type) bool {
    if (!ty.isArray() or !ty.getElemType().isInt())
        return false;

    var i = p.tokenIdx;
    while (true) : (i += 1) {
        switch (p.tokenIds[i]) {
            .LParen => {},

            .StringLiteral,
            .StringLiteralUTF_16,
            .StringLiteralUTF_8,
            .StringLiteralUTF_32,
            .StringLiteralWide,
            => return true,

            else => return false,
        }
    }
}

/// Convert InitList into an AST
fn convertInitList(p: *Parser, il: InitList, initType: Type) Error!NodeIndex {
    if (initType.isInt() or initType.isFloat() or initType.isPointer()) {
        if (il.node == .none) {
            return p.addNode(.{
                .tag = .DefaultInitExpr,
                .type = initType,
                .data = undefined,
            });
        }
        return il.node;
    } else if (initType.is(.VariableLenArray)) {
        return error.ParsingFailed; // vla invalid, reported earlier
    } else if (initType.isArray()) {
        if (il.node != .none)
            return il.node;

        const listBuffTop = p.listBuffer.items.len;
        defer p.listBuffer.items.len = listBuffTop;

        const elemType = initType.getElemType();
        const maxItems = initType.arrayLen() orelse std.math.maxInt(usize);
        var start: u64 = 0;
        for (il.list.items) |*init| {
            if (init.index > start) {
                const elem = try p.addNode(.{
                    .tag = .ArrayFillerExpr,
                    .type = elemType,
                    .data = .{ .int = init.index - start },
                });
                try p.listBuffer.append(elem);
            }
            start = init.index + 1;

            const elem = try p.convertInitList(init.list, elemType);
            try p.listBuffer.append(elem);
        }

        var arrInitNode: AST.Node = .{
            .tag = .ArrayInitExprTwo,
            .type = initType,
            .data = .{ .binExpr = .{ .lhs = .none, .rhs = .none } },
        };

        if (initType.specifier == .IncompleteArray) {
            arrInitNode.type.specifier = .Array;
            arrInitNode.type.data.array.len = start;
        } else if (initType.is(.IncompleteArray)) {
            const arrayType = try p.arena.create(Type.Array);
            arrayType.* = .{
                .elem = initType.getElemType(),
                .len = start,
            };
            arrInitNode.type = .{
                .specifier = .Array,
                .data = .{ .array = arrayType },
            };
            const attrs = initType.getAttributes();
            arrInitNode.type = try arrInitNode.type.withAttributes(p.arena, attrs);
        } else if (start < maxItems) {
            const elem = try p.addNode(.{
                .tag = .ArrayFillerExpr,
                .type = elemType,
                .data = .{ .int = maxItems - start },
            });
            try p.listBuffer.append(elem);
        }

        const items = p.listBuffer.items[listBuffTop..];
        switch (items.len) {
            0 => {},
            1 => arrInitNode.data.binExpr.lhs = items[0],
            2 => arrInitNode.data.binExpr = .{ .lhs = items[0], .rhs = items[1] },
            else => {
                arrInitNode.tag = .ArrayInitExpr;
                arrInitNode.data = .{ .range = try p.addList(items) };
            },
        }
        return try p.addNode(arrInitNode);
    } else if (initType.get(.Struct)) |structType| {
        std.debug.assert(!structType.hasIncompleteSize());

        const listBuffTop = p.listBuffer.items.len;
        defer p.listBuffer.items.len = listBuffTop;

        var initIndex: usize = 0;
        for (structType.data.record.fields, 0..) |f, i| {
            if (initIndex < il.list.items.len and il.list.items[initIndex].index == i) {
                const item = try p.convertInitList(il.list.items[initIndex].list, f.ty);
                try p.listBuffer.append(item);
                initIndex += 1;
            } else {
                const item = try p.addNode(.{ .tag = .DefaultInitExpr, .type = f.ty, .data = undefined });
                try p.listBuffer.append(item);
            }
        }

        var structInitNode: AST.Node = .{
            .tag = .StructInitExprTwo,
            .type = initType,
            .data = .{ .binExpr = .{ .lhs = .none, .rhs = .none } },
        };
        const items = p.listBuffer.items[listBuffTop..];
        switch (items.len) {
            0 => {},
            1 => structInitNode.data.binExpr.lhs = items[0],
            2 => structInitNode.data.binExpr = .{ .lhs = items[0], .rhs = items[1] },
            else => {
                structInitNode.tag = .StructInitExpr;
                structInitNode.data = .{ .range = try p.addList(items) };
            },
        }
        return try p.addNode(structInitNode);
    } else if (initType.get(.Union)) |unionType| {
        var unionInitNode: AST.Node = .{
            .tag = .UnionInitExpr,
            .type = initType,
            .data = .{ .unionInit = .{ .fieldIndex = 0, .node = .none } },
        };
        if (unionType.data.record.fields.len == 0) {
            // do nothing for empty unions
        } else if (il.list.items.len == 0) {
            unionInitNode.data.unionInit.node = try p.addNode(.{
                .tag = .DefaultInitExpr,
                .type = initType,
                .data = undefined,
            });
        } else {
            const init = il.list.items[0];
            const fieldType = unionType.data.record.fields[init.index].ty;
            unionInitNode.data.unionInit = .{
                .fieldIndex = @truncate(init.index),
                .node = try p.convertInitList(init.list, fieldType),
            };
        }
        return try p.addNode(unionInitNode);
    } else {
        return error.ParsingFailed; // initializer target is invalid, reported earily.
    }
}

/// assembly : keyword_asm asm-qualifier* '(' asm-string ')'
fn parseAssembly(p: *Parser, kind: enum { global, declLable, stmt }) Error!?NodeIndex {
    const asmToken = p.tokenIdx;
    switch (p.getCurrToken()) {
        .KeywordGccAsm, .KeywordGccAsm1, .KeywordGccAsm2 => p.tokenIdx += 1,
        else => return null,
    }

    var @"volatile" = false;
    var @"inline" = false;
    var goto = false;
    while (true) : (p.tokenIdx += 1) switch (p.getCurrToken()) {
        .KeywordVolatile, .KeywordGccVolatile1, .KeywordGccVolatile2 => {
            if (kind != .stmt) try p.errStr(.meaningless_asm_qual, p.tokenIdx, "volatile");
            if (@"volatile") try p.errStr(.duplicate_asm_qual, p.tokenIdx, "volatile");
            @"volatile" = true;
        },
        .KeywordInline, .KeywordGccInline1, .KeywordGccInline2 => {
            if (kind != .stmt) try p.errStr(.meaningless_asm_qual, p.tokenIdx, "inline");
            if (@"inline") try p.errStr(.duplicate_asm_qual, p.tokenIdx, "inline");
            @"inline" = true;
        },
        .KeywordGoto => {
            if (kind != .stmt) try p.errStr(.meaningless_asm_qual, p.tokenIdx, "goto");
            if (goto) try p.errStr(.duplicate_asm_qual, p.tokenIdx, "goto");
            goto = true;
        },
        else => break,
    };

    const lparen = try p.expectToken(.LParen);
    switch (kind) {
        .declLable => {
            const str = (try p.parseAsmString()).value.data.bytes;
            const attr = Attribute{ .tag = .asm_label, .args = .{ .asm_label = .{ .name = str[0 .. str.len - 1] } } };
            try p.attrBuffer.append(p.pp.comp.gpa, .{ .attr = attr, .tok = asmToken });
        },
        .global => _ = try p.parseAsmString(),
        .stmt => return p.todo("assembly statements"),
    }
    try p.expectClosing(lparen, .RParen);

    if (kind != .declLable)
        _ = try p.expectToken(.Semicolon);
    return .none;
}

/// Same as stringLiteral but errors on unicode and wide string literals
fn parseAsmString(p: *Parser) Error!Result {
    var i = p.tokenIdx;
    while (true) : (i += 1) switch (p.tokenIds[i]) {
        .StringLiteral => {},
        .StringLiteralUTF_8, .StringLiteralUTF_16, .StringLiteralUTF_32 => {
            try p.errStr(.invalid_asm_str, p.tokenIdx, "unicode");
            return error.ParsingFailed;
        },
        .StringLiteralWide => {
            try p.errStr(.invalid_asm_str, p.tokenIdx, "wide");
            return error.ParsingFailed;
        },
        else => break,
    };
    return try p.parseStringLiteral();
}

// ====== statements ======

/// statement
///  : labeled-statement
///  | unlabled-statement
/// unlabled-statement:
///  : expression-statement
///  | attribute-specifier-sequence? primary-block
///  | attribute-specifier-sequence? jump-statement
/// expression-statement
///  : expression? ';'
///  | attribute-specifier-sequence expression ';'
/// primary-block
///  : compound-statement
///  | selection-statement
///  | iteration-statement
/// selection-statement
///  : if-statement
///  | switch-statement
/// iteration-statement
///  : while-statement
///  | do-while-statement
///  | for-statement
/// jump-statement
///  : goto-statement;
///  | `continue` ';'
///  | `break` ';'
///  | return-statement
fn parseStmt(p: *Parser) Error!NodeIndex {
    if (try p.parseLabeledStmt()) |some|
        return some;

    if (try p.parseCompoundStmt(false, null)) |some|
        return some;

    if (p.eat(.KeywordIf)) |_|
        return p.parseIfStmt();

    if (p.eat(.KeywordSwitch)) |_|
        return p.parseSwitchStmt();

    if (p.eat(.KeywordWhile)) |_|
        return p.parseWhileStmt();

    if (p.eat(.KeywordDo)) |_|
        return p.parseDoWhileStmt();

    if (p.eat(.KeywordFor)) |_|
        return p.parseForStmt();

    if (p.eat(.KeywordGoto)) |gotoToken|
        return p.parseGotoStmt(gotoToken);

    if (p.eat(.KeywordContinue)) |cont| {
        if (!p.inLoop)
            try p.errToken(.continue_not_in_loop, cont);
        _ = try p.expectToken(.Semicolon);

        return try p.addNode(.{ .tag = .ContinueStmt, .data = undefined });
    }

    if (p.eat(.KeywordBreak)) |br| {
        if (!p.inLoop and p.@"switch" == null)
            try p.errToken(.break_not_in_loop_or_switch, br);

        _ = try p.expectToken(.Semicolon);
        return try p.addNode(.{ .tag = .BreakStmt, .data = undefined });
    }

    if (try p.parseReturnStmt()) |some|
        return some;

    const exprStart = p.tokenIdx;
    const errStart = p.pp.comp.diag.list.items.len;

    const e = try p.parseExpr();
    if (e.node != .none) {
        _ = try p.expectToken(.Semicolon);
        try e.maybeWarnUnused(p, exprStart, errStart);
        return e.node;
    }

    const attrBufferTop = p.attrBuffer.len;
    defer p.attrBuffer.len = attrBufferTop;
    try p.parseAttrSpec(); // .statement

    if (p.eat(.Semicolon)) |_| {
        var nullNode: AST.Node = .{ .tag = .NullStmt, .data = undefined };
        nullNode.type = try p.withAttributes(nullNode.type, attrBufferTop);

        if (nullNode.type.getAttribute(.fallthrough) != null) {
            // TODO: this condition is not completely correct; the last statement of a compound
            // statement is also valid if it precedes a switch label (so intervening '}' are ok,
            // but only if they close a compound statement)
            if (p.getCurrToken() != .KeywordCase and p.getCurrToken() != .KeywordDefault)
                try p.errToken(.invalid_fallthrough, exprStart);
        }

        return p.addNode(nullNode);
    }

    try p.err(.expected_stmt);
    return error.ParsingFailed;
}

/// if-statement
///  : `if` '(' expression ')' statement
///  | `if` '(' expression ')' statement `else` statement
fn parseIfStmt(p: *Parser) Error!NodeIndex {
    const lp = try p.expectToken(.LParen);
    const condToken = p.tokenIdx;
    var cond = try p.parseExpr();

    try cond.expect(p);
    try cond.lvalConversion(p);
    if (cond.ty.isInt())
        try cond.intCast(p, cond.ty.integerPromotion(p.pp.comp), condToken)
    else if (!cond.ty.isFloat() and !cond.ty.isPointer())
        try p.errStr(.statement_scalar, lp + 1, try p.typeStr(cond.ty));

    try cond.saveValue(p);
    try p.expectClosing(lp, .RParen);

    const then = try p.parseStmt();
    const elseTK = if (p.eat(.KeywordElse)) |_| try p.parseStmt() else .none;

    if (then != .none and elseTK != .none) {
        return try p.addNode(.{
            .tag = .IfThenElseStmt,
            .data = .{
                .if3 = .{
                    .cond = cond.node,
                    .body = (try p.addList(&.{ then, elseTK })).start,
                },
            },
        });
    } else if (then == .none and elseTK != .none) {
        return try p.addNode(.{
            .tag = .IfElseStmt,
            .data = .{ .binExpr = .{ .lhs = cond.node, .rhs = elseTK } },
        });
    } else return try p.addNode(.{
        .tag = .IfThenStmt,
        .data = .{ .binExpr = .{ .lhs = cond.node, .rhs = then } },
    });
}

/// for-statement
///  : `for` '(' expression? ';' expression? ';' expression? ')' statement
///  | `for` '(' declaration expression? ';' expression? ')' statement
fn parseForStmt(p: *Parser) Error!NodeIndex {
    try p.symStack.pushScope();
    const declBufferTop = p.declBuffer.items.len;

    defer {
        p.declBuffer.items.len = declBufferTop;
        p.symStack.popScope();
    }

    const lp = try p.expectToken(.LParen);
    const gotDecl = try p.parseDeclaration();

    // for-init
    const initStart = p.tokenIdx;
    var errStart = p.pp.comp.diag.list.items.len;
    var init = if (!gotDecl) try p.parseExpr() else Result{};
    try init.saveValue(p);
    try init.maybeWarnUnused(p, initStart, errStart);

    if (!gotDecl)
        _ = try p.expectToken(.Semicolon);

    // cond
    const condToken = p.tokenIdx;
    var cond = try p.parseExpr();
    if (cond.node != .none) {
        try cond.lvalConversion(p);
        if (cond.ty.isInt())
            try cond.intCast(p, cond.ty.integerPromotion(p.pp.comp), condToken)
        else if (!cond.ty.isFloat() and !cond.ty.isPointer())
            try p.errStr(.statement_scalar, lp + 1, try p.typeStr(cond.ty));
    }
    try cond.saveValue(p);
    _ = try p.expectToken(.Semicolon);

    // increment
    const incrStart = p.tokenIdx;
    errStart = p.pp.comp.diag.list.items.len;
    var incr = try p.parseExpr();
    try incr.maybeWarnUnused(p, incrStart, errStart);
    try incr.saveValue(p);
    try p.expectClosing(lp, .RParen);

    const body = body: {
        const oldLoop = p.inLoop;
        p.inLoop = true;
        defer p.inLoop = oldLoop;
        break :body try p.parseStmt();
    };

    if (gotDecl) {
        const start = (try p.addList(p.declBuffer.items[declBufferTop..])).start;
        const end = (try p.addList(&.{ cond.node, incr.node, body })).end;

        return try p.addNode(.{
            .tag = .ForDeclStmt,
            .data = .{ .range = .{ .start = start, .end = end } },
        });
    } else if (init.node == .none and cond.node == .none and incr.node == .none) {
        return try p.addNode(.{
            .tag = .ForEverStmt,
            .data = .{ .unExpr = body },
        });
    } else {
        return try p.addNode(.{
            .tag = .ForStmt,
            .data = .{
                .if3 = .{
                    .cond = body,
                    .body = (try p.addList(&.{ init.node, cond.node, incr.node })).start,
                },
            },
        });
    }
}

/// while-statement : `while` '(' expression ')' statement ';'
fn parseWhileStmt(p: *Parser) Error!NodeIndex {
    const lp = try p.expectToken(.LParen);
    const condToken = p.tokenIdx;
    var cond = try p.parseExpr();

    try cond.expect(p);
    try cond.lvalConversion(p);
    if (cond.ty.isInt())
        try cond.intCast(p, cond.ty.integerPromotion(p.pp.comp), condToken)
    else if (!cond.ty.isFloat() and !cond.ty.isPointer())
        try p.errStr(.statement_scalar, lp + 1, try p.typeStr(cond.ty));

    try cond.saveValue(p);
    try p.expectClosing(lp, .RParen);

    const body = body: {
        const oldLoop = p.inLoop;
        p.inLoop = true;
        defer p.inLoop = oldLoop;
        break :body try p.parseStmt();
    };

    return try p.addNode(.{
        .tag = .WhileStmt,
        .data = .{ .binExpr = .{ .rhs = cond.node, .lhs = body } },
    });
}

/// do-while-statement : `do` statement `while` '(' expression ')' ';'
fn parseDoWhileStmt(p: *Parser) Error!NodeIndex {
    const body = body: {
        const oldLoop = p.inLoop;
        p.inLoop = true;
        defer p.inLoop = oldLoop;
        break :body try p.parseStmt();
    };

    _ = try p.expectToken(.KeywordWhile);
    const lp = try p.expectToken(.LParen);
    const condToken = p.tokenIdx;
    var cond = try p.parseExpr();

    try cond.expect(p);
    try cond.lvalConversion(p);
    if (cond.ty.isInt())
        try cond.intCast(p, cond.ty.integerPromotion(p.pp.comp), condToken)
    else if (!cond.ty.isFloat() and !cond.ty.isPointer())
        try p.errStr(.statement_scalar, lp + 1, try p.typeStr(cond.ty));

    try cond.saveValue(p);
    try p.expectClosing(lp, .RParen);

    _ = try p.expectToken(.Semicolon);
    return try p.addNode(.{
        .tag = .WhileStmt,
        .data = .{ .binExpr = .{ .rhs = cond.node, .lhs = body } },
    });
}

/// goto-statement : `goto` ( identifier | ( '*' expr)) ';'
fn parseGotoStmt(p: *Parser, gotoToken: TokenIndex) Error!NodeIndex {
    if (p.eat(.Asterisk)) |_| {
        const expr = p.tokenIdx;
        var e = try p.parseExpr();
        try e.expect(p);
        try e.lvalConversion(p);
        p.computedGotoTok = p.computedGotoTok orelse gotoToken;
        if (!e.ty.isPointer()) {
            if (!e.ty.isInt()) {
                try p.errStr(.incompatible_param, expr, try p.typeStr(e.ty));
                return error.ParsingFailed;
            }

            const elemType = try p.arena.create(Type);
            elemType.* = .{ .specifier = .Void, .qual = .{ .@"const" = true } };
            const resultType = Type{
                .specifier = .Pointer,
                .data = .{ .subType = elemType },
            };

            if (e.value.isZero()) {
                try e.nullCast(p, resultType);
            } else {
                try p.errStr(.implicit_int_to_ptr, expr, try p.typePairStrExtra(e.ty, " to ", resultType));
                try e.ptrCast(p, resultType);
            }
        }

        try e.un(p, .ComputedGotoStmt);
        _ = try p.expectToken(.Semicolon);
        return e.node;
    }

    const nameToken = try p.expectIdentifier();
    const str = p.getTokenSlice(nameToken);
    if (p.findLabel(str) == null) {
        try p.labels.append(.{ .unresolvedGoto = nameToken });
    }
    _ = try p.expectToken(.Semicolon);
    return try p.addNode(.{
        .tag = .GotoStmt,
        .data = .{ .declRef = nameToken },
    });
}

/// switch-statement : `switch` '(' expression ')' statement
fn parseSwitchStmt(p: *Parser) Error!NodeIndex {
    const lp = try p.expectToken(.LParen);
    const condToken = p.tokenIdx;
    var cond = try p.parseExpr();

    try cond.expect(p);
    try cond.lvalConversion(p);
    if (cond.ty.isInt())
        try cond.intCast(p, cond.ty.integerPromotion(p.pp.comp), condToken)
    else
        try p.errStr(.statement_int, lp + 1, try p.typeStr(cond.ty));

    try cond.saveValue(p);
    try p.expectClosing(lp, .RParen);

    const oldSwitch = p.@"switch";
    var @"switch" = Switch{
        .ranges = std.ArrayList(Switch.Range).init(p.pp.comp.gpa),
        .type = cond.ty,
    };
    p.@"switch" = &@"switch";

    defer {
        @"switch".ranges.deinit();
        p.@"switch" = oldSwitch;
    }

    const body = try p.parseStmt();

    return try p.addNode(.{
        .tag = .SwitchStmt,
        .data = .{ .binExpr = .{ .lhs = cond.node, .rhs = body } },
    });
}

/// case-statement : case constant-expression ':'
fn parseCaseStmt(p: *Parser, caseToken: u32) Error!?NodeIndex {
    const firstItem = try p.parseConstExpr(.GNUFoldingExtension);
    const ellipsis = p.tokenIdx; // `...`
    const secondItem = if (p.eat(.Ellipsis) != null) blk: {
        try p.errToken(.gnu_switch_range, ellipsis);
        break :blk try p.parseConstExpr(.GNUFoldingExtension);
    } else null;

    _ = try p.expectToken(.Colon);

    if (p.@"switch") |some| check: {
        if (some.type.hasIncompleteSize()) // error already reported for incomplete size
            break :check;

        const first = firstItem.value;
        const last = if (secondItem) |second| second.value else first;
        if (first.tag == .unavailable) {
            try p.errToken(.case_val_unavailable, caseToken + 1);
            break :check;
        } else if (last.tag == .unavailable) {
            try p.errToken(.case_val_unavailable, ellipsis + 1);
            break :check;
        } else if (last.compare(.lt, first, some.type, p.pp.comp)) {
            try p.errToken(.empty_case_range, caseToken + 1);
            break :check;
        }

        // TODO cast to target type
        const prev = (try some.add(p.pp.comp, first, last, caseToken + 1)) orelse break :check;
        // TODO: check which value was already handled
        if (some.type.isUnsignedInt(p.pp.comp)) {
            try p.errExtra(.duplicate_switch_case_unsigned, caseToken + 1, .{
                .unsigned = first.data.int,
            });
        } else {
            try p.errExtra(.duplicate_switch_case_signed, caseToken + 1, .{
                .signed = first.signExtend(some.type, p.pp.comp),
            });
        }
        try p.errToken(.previous_case, prev.token);
    } else {
        try p.errStr(.case_not_in_switch, caseToken, "case");
    }

    const s = try p.parseStmt();
    if (secondItem) |some| return try p.addNode(.{
        .tag = .CaseRangeStmt,
        .data = .{
            .if3 = .{
                .cond = s,
                .body = (try p.addList(&.{ firstItem.node, some.node })).start,
            },
        },
    }) else return try p.addNode(.{
        .tag = .CaseStmt,
        .data = .{
            .binExpr = .{
                .lhs = firstItem.node,
                .rhs = s,
            },
        },
    });
}

/// default-statement : `default` ':'
fn parseDefaultStmt(p: *Parser, defaultToken: u32) Error!?NodeIndex {
    _ = try p.expectToken(.Colon);
    const s = try p.parseStmt();

    const node = try p.addNode(.{
        .tag = .DefaultStmt,
        .data = .{ .unExpr = s },
    });

    const @"switch" = p.@"switch" orelse {
        try p.errStr(.case_not_in_switch, defaultToken, "default");
        return node;
    };

    if (@"switch".default) |previous| {
        try p.errToken(.multiple_default, defaultToken);
        try p.errToken(.previous_case, previous);
    } else {
        @"switch".default = defaultToken;
    }

    return node;
}

/// labeledStmt
/// : identifier ':' statement
/// | case-statement
/// | default-statement
fn parseLabeledStmt(p: *Parser) Error!?NodeIndex {
    if ((p.getCurrToken() == .Identifier or
        p.getCurrToken() == .ExtendedIdentifier) and
        p.lookAhead(1) == .Colon)
    {
        const nameToken = p.expectIdentifier() catch unreachable;
        const str = p.getTokenSlice(nameToken);
        if (p.findLabel(str)) |some| {
            try p.errStr(.duplicate_label, nameToken, str);
            try p.errStr(.previous_label, some, str);
        } else {
            p.labelCount += 1;
            try p.labels.append(.{ .label = nameToken });

            var i: usize = 0;
            while (i < p.labels.items.len) {
                if (p.labels.items[i] == .unresolvedGoto and
                    std.mem.eql(u8, p.getTokenSlice(p.labels.items[i].unresolvedGoto), str))
                    _ = p.labels.swapRemove(i)
                else
                    i += 1;
            }
        }

        p.tokenIdx += 1;

        const attrBufferTop = p.attrBuffer.len;
        defer p.attrBuffer.len = attrBufferTop;

        try p.parseAttrSpec(); // .label

        return try p.addNode(.{
            .tag = .LabeledStmt,
            .data = .{ .decl = .{ .name = nameToken, .node = try p.parseStmt() } },
        });
    } else if (p.eat(.KeywordCase)) |case| {
        return p.parseCaseStmt(case);
    } else if (p.eat(.KeywordDefault)) |default| {
        return p.parseDefaultStmt(default);
    } else return null;
}

const StmtExprState = struct {
    lastExprToken: TokenIndex = 0,
    lastExprRes: Result = .{ .ty = .{ .specifier = .Void } },
};

/// compound-statement
/// : '{' ( decl | GccExtensionDecl | static-assert-declaration | statememt)* '}'
fn parseCompoundStmt(p: *Parser, isFnBody: bool, stmtExprState: ?*StmtExprState) Error!?NodeIndex {
    const lBrace = p.eat(.LBrace) orelse return null;
    const declBufferTop = p.declBuffer.items.len;

    // the parameters of a function are in the same scope as the body
    if (!isFnBody)
        try p.symStack.pushScope();

    defer {
        p.declBuffer.items.len = declBufferTop;
        if (!isFnBody) p.symStack.popScope();
    }

    var noreturnIdx: ?TokenIndex = null;
    var noreturnLabelCount: u32 = 0;

    while (p.eat(.RBrace) == null) : (_ = try p.pragma()) {
        if (stmtExprState) |state| state.* = .{};
        if (try p.parseOrNextStmt(parseStaticAssert, lBrace))
            continue;

        if (try p.parseOrNextStmt(parseDeclaration, lBrace))
            continue;

        if (p.eat(.KeywordGccExtension)) |ext| {
            const saveExtension = p.extensionSuppressd;
            defer p.extensionSuppressd = saveExtension;
            p.extensionSuppressd = true;

            if (p.parseDeclaration() catch |e| switch (e) {
                error.ParsingFailed => {
                    try p.nextStmt(lBrace);
                    continue;
                },
                else => |er| return er,
            }) continue;
            p.tokenIdx = ext;
        }

        const stmtToken = p.tokenIdx;
        const s = p.parseStmt() catch |er| switch (er) {
            error.ParsingFailed => {
                try p.nextStmt(lBrace);
                continue;
            },
            else => |e| return e,
        };

        if (s == .none) continue;
        if (stmtExprState) |state| {
            state.* = .{
                .lastExprToken = stmtToken,
                .lastExprRes = .{
                    .node = s,
                    .ty = p.nodes.items(.type)[@intFromEnum(s)],
                },
            };
        }
        try p.declBuffer.append(s);

        if (noreturnIdx == null and p.nodeIsNoreturn(s)) {
            noreturnIdx = p.tokenIdx;
            noreturnLabelCount = p.labelCount;
        }

        switch (p.nodes.items(.tag)[@intFromEnum(s)]) {
            .CaseStmt, .DefaultStmt, .LabeledStmt => noreturnIdx = null,
            else => {},
        }
    }

    if (noreturnIdx) |some| {
        if (noreturnLabelCount == p.labelCount and some != p.tokenIdx - 1)
            try p.errToken(.unreachable_code, some);
    }

    if (isFnBody and (p.declBuffer.items.len == declBufferTop or !p.nodeIsNoreturn(p.declBuffer.items[p.declBuffer.items.len - 1]))) {
        if (!p.func.type.?.getReturnType().is(.Void))
            try p.errStr(.func_does_not_return, p.tokenIdx - 1, p.getTokenSlice(p.func.name));

        try p.declBuffer.append(try p.addNode(.{
            .tag = .ImplicitReturn,
            .type = p.func.type.?.getReturnType(),
            .data = undefined,
        }));
    }

    if (isFnBody) {
        if (p.func.ident) |some| try p.declBuffer.insert(declBufferTop, some.node);
        if (p.func.prettyIdent) |some| try p.declBuffer.insert(declBufferTop, some.node);
    }

    var node: AST.Node = .{
        .tag = .CompoundStmtTwo,
        .data = .{ .binExpr = .{ .lhs = .none, .rhs = .none } },
    };
    const statements = p.declBuffer.items[declBufferTop..];
    switch (statements.len) {
        0 => {},
        1 => node.data = .{ .binExpr = .{ .lhs = statements[0], .rhs = .none } },
        2 => node.data = .{ .binExpr = .{ .lhs = statements[0], .rhs = statements[1] } },
        else => {
            node.tag = .CompoundStmt;
            node.data = .{ .range = try p.addList(statements) };
        },
    }

    return try p.addNode(node);
}

/// return-statement : `return` expression? ';'
fn parseReturnStmt(p: *Parser) Error!?NodeIndex {
    const retToken = p.eat(.KeywordReturn) orelse return null;
    const eToken = p.tokenIdx;

    var expr = try p.parseExpr();
    _ = try p.expectToken(.Semicolon);
    const returnType = p.func.type.?.getReturnType();

    if (expr.node == .none) {
        if (!returnType.is(.Void))
            try p.errStr(.func_should_return, retToken, p.getTokenSlice(p.func.name));
        return try p.addNode(.{ .tag = .ReturnStmt, .data = .{ .unExpr = expr.node } });
    } else if (returnType.is((.Void))) {
        try p.errStr(.void_func_returns_value, eToken, p.getTokenSlice(p.func.name));
        return try p.addNode(.{ .tag = .ReturnStmt, .data = .{ .unExpr = expr.node } });
    }

    try expr.lvalConversion(p);
    // Return type conversion is done as if it was assignment
    if (returnType.is(.Bool)) {
        // this is ridiculous but it's what clang does
        if (expr.ty.isInt() or expr.ty.isFloat() or expr.ty.isPointer()) {
            try expr.boolCast(p, returnType, eToken);
        } else {
            try p.errStr(.incompatible_return, eToken, try p.typeStr(expr.ty));
        }
    } else if (returnType.isInt()) {
        if (expr.ty.isInt() or expr.ty.isFloat()) {
            try expr.intCast(p, returnType, eToken);
        } else if (expr.ty.isPointer()) {
            try p.errStr(.implicit_ptr_to_int, eToken, try p.typePairStrExtra(expr.ty, " to ", returnType));
            try expr.intCast(p, returnType, eToken);
        } else {
            try p.errStr(.incompatible_return, eToken, try p.typeStr(expr.ty));
        }
    } else if (returnType.isFloat()) {
        if (expr.ty.isInt() or expr.ty.isFloat()) {
            try expr.floatCast(p, returnType);
        } else {
            try p.errStr(.incompatible_return, eToken, try p.typeStr(expr.ty));
        }
    } else if (returnType.isPointer()) {
        if (expr.value.isZero()) {
            try expr.nullCast(p, returnType);
        } else if (expr.ty.isInt()) {
            try p.errStr(.implicit_int_to_ptr, eToken, try p.typePairStrExtra(expr.ty, " to ", returnType));
            try expr.intCast(p, returnType, eToken);
        } else if (!expr.ty.isVoidStar() and !returnType.isVoidStar() and !returnType.eql(expr.ty, p.pp.comp, false)) {
            try p.errStr(.incompatible_return, eToken, try p.typeStr(expr.ty));
        }
    } else if (returnType.isRecord()) { // enum.isInt() == true
        if (!returnType.eql(expr.ty, p.pp.comp, false)) {
            try p.errStr(.incompatible_return, eToken, try p.typeStr(expr.ty));
        }
    } else {
        unreachable;
    }

    try expr.saveValue(p);
    return try p.addNode(.{ .tag = .ReturnStmt, .data = .{ .unExpr = expr.node } });
}

fn nodeIsNoreturn(p: *Parser, node: NodeIndex) bool {
    switch (p.nodes.items(.tag)[@intFromEnum(node)]) {
        .BreakStmt, .ContinueStmt, .ReturnStmt => return true,
        .IfThenElseStmt => {
            const data = p.data.items[p.nodes.items(.data)[@intFromEnum(node)].if3.body..];
            return p.nodeIsNoreturn(data[0]) and p.nodeIsNoreturn(data[1]);
        },

        .CompoundStmtTwo => {
            const data = p.nodes.items(.data)[@intFromEnum(node)];
            if (data.binExpr.rhs != .none) return p.nodeIsNoreturn(data.binExpr.rhs);
            if (data.binExpr.lhs != .none) return p.nodeIsNoreturn(data.binExpr.lhs);
            return false;
        },

        .CompoundStmt => {
            const data = p.nodes.items(.data)[@intFromEnum(node)];
            return p.nodeIsNoreturn(p.data.items[data.range.end - 1]);
        },

        .LabeledStmt => {
            const data = p.nodes.items(.data)[@intFromEnum(node)];
            return p.nodeIsNoreturn(data.decl.node);
        },

        else => return false,
    }
}

fn parseOrNextStmt(p: *Parser, comptime func: fn (*Parser) Error!bool, lbrace: TokenIndex) !bool {
    return func(p) catch |er| switch (er) {
        error.ParsingFailed => {
            try p.nextStmt(lbrace);
            return true;
        },
        else => |e| return e,
    };
}

fn nextStmt(p: *Parser, lBrace: TokenIndex) !void {
    var parens: u32 = 0;
    while (p.tokenIdx < p.tokenIds.len) : (p.tokenIdx += 1) {
        switch (p.getCurrToken()) {
            .LParen, .LBrace, .LBracket => parens += 1,
            .RParen, .RBracket => if (parens != 0) {
                parens -= 1;
            },

            .RBrace => if (parens == 0)
                return
            else {
                parens -= 1;
            },

            .Semicolon,
            .KeywordFor,
            .KeywordWhile,
            .KeywordDo,
            .KeywordIf,
            .KeywordGoto,
            .KeywordSwitch,
            .KeywordCase,
            .KeywordDefault,
            .KeywordContinue,
            .KeywordBreak,
            .KeywordReturn,
            .KeywordTypedef,
            .KeywordExtern,
            .KeywordStatic,
            .KeywordAuto,
            .KeywordRegister,
            .KeywordThreadLocal,
            .KeywordInline,
            .KeywordGccInline1,
            .KeywordGccInline2,
            .KeywordNoreturn,
            .KeywordVoid,
            .KeywordBool,
            .KeywordChar,
            .KeywordShort,
            .KeywordInt,
            .KeywordLong,
            .KeywordSigned,
            .KeywordUnsigned,
            .KeywordFloat,
            .KeywordDouble,
            .KeywordComplex,
            .KeywordAtomic,
            .KeywordEnum,
            .KeywordStruct,
            .KeywordUnion,
            .KeywordAlignas,
            .KeywordTypeof1,
            .KeywordTypeof2,
            .KeywordGccTypeof,
            .KeywordGccExtension,
            => if (parens == 0) return,
            .KeywordPragma => p.skipToPragmaSentinel(),
            else => {},
        }
    }
    // so  we can consume d eof
    p.tokenIdx -= 1;
    try p.expectClosing(lBrace, .RBrace);
    unreachable;
}

/////////////////////////////////////////////////////////////////////////////////////////////////
/////                               Expression                                               ///
///////////////////////////////////////////////////////////////////////////////////////////////
pub fn macroExpr(p: *Parser) Compilation.Error!bool {
    const res = p.parseCondExpr() catch |e| switch (e) {
        error.OutOfMemory => return error.OutOfMemory,
        error.FatalError => return error.FatalError,
        error.ParsingFailed => return false,
    };

    if (res.value.tag == .unavailable) {
        try p.errToken(.expected_expr, p.tokenIdx);
        return false;
    }

    return res.value.getBool();
}

/// expression : assign-expression (',' assign-expression)*
fn parseExpr(p: *Parser) Error!Result {
    var exprStartIdx = p.tokenIdx;
    var errStart = p.pp.comp.diag.list.items.len;
    var lhs = try p.parseAssignExpr();

    if (p.getCurrToken() == .Comma)
        try lhs.expect(p);

    while (p.eat(.Comma)) |_| {
        try lhs.maybeWarnUnused(p, exprStartIdx, errStart);
        exprStartIdx = p.tokenIdx;
        errStart = p.pp.comp.diag.list.items.len;

        const rhs = try p.parseAssignExpr();
        try rhs.expect(p);
        lhs.value = rhs.value;
        lhs.ty = rhs.ty;
        try lhs.bin(p, .CommaExpr, rhs);
    }

    return lhs;
}

fn tokToTag(p: *Parser, token: TokenIndex) AstTag {
    return switch (p.tokenIds[token]) {
        .Equal => .AssignExpr,
        .AsteriskEqual => .MulAssignExpr,
        .SlashEqual => .DivAssignExpr,
        .PercentEqual => .ModAssignExpr,
        .PlusEqual => .AddAssignExpr,
        .MinusEqual => .SubAssignExpr,
        .AngleBracketAngleBracketLeftEqual => .ShlAssignExpr,
        .AngleBracketAngleBracketRightEqual => .ShrAssignExpr,
        .AmpersandEqual => .BitAndAssignExpr,
        .CaretEqual => .BitXorAssignExpr,
        .PipeEqual => .BitOrAssignExpr,
        .EqualEqual => .EqualExpr,
        .BangEqual => .NotEqualExpr,
        .AngleBracketLeft => .LessThanExpr,
        .AngleBracketLeftEqual => .LessThanEqualExpr,
        .AngleBracketRight => .GreaterThanExpr,
        .AngleBracketRightEqual => .GreaterThanEqualExpr,
        .AngleBracketAngleBracketLeft => .ShlExpr,
        .AngleBracketAngleBracketRight => .ShrExpr,
        .Plus => .AddExpr,
        .Minus => .SubExpr,
        .Asterisk => .MulExpr,
        .Slash => .DivExpr,
        .Percent => .ModExpr,
        else => unreachable,
    };
}

/// assign-expression
///  : conditional-expression
///  | unary-expression assignment-operator  assign-expression
/// assignment-operator
///  : ('=' | '*=' | '/=' | '%=' | '+=' | '-=' | '<<=' | '>>=' | '&=' | '^=' | '|=')
fn parseAssignExpr(p: *Parser) Error!Result {
    var lhs = try p.parseCondExpr();
    if (lhs.empty(p))
        return lhs;

    const token = p.tokenIdx;
    const eq = p.eat(.Equal);
    const mul = eq orelse p.eat(.AsteriskEqual);
    const div = mul orelse p.eat(.SlashEqual);
    const mod = div orelse p.eat(.PercentEqual);
    const add = mod orelse p.eat(.PlusEqual);
    const sub = add orelse p.eat(.MinusEqual);
    const shl = sub orelse p.eat(.AngleBracketAngleBracketLeftEqual);
    const shr = shl orelse p.eat(.AngleBracketAngleBracketRightEqual);
    const bitAnd = shr orelse p.eat(.AmpersandEqual);
    const bitXor = bitAnd orelse p.eat(.CaretEqual);
    const bitOr = bitXor orelse p.eat(.PipeEqual);

    const tag = p.tokToTag(bitOr orelse return lhs);
    var rhs = try p.parseAssignExpr();
    try rhs.expect(p);
    try rhs.lvalConversion(p);

    var isConst: bool = undefined;
    if (!AST.isLValueExtra(p.nodes.slice(), p.data.items, p.valueMap, lhs.node, &isConst) or isConst) {
        try p.errToken(.not_assignable, token);
        return error.ParsingFailed;
    }

    // adjustTypes will do do lvalue conversion but we do not want that
    var lhsCopy = lhs;
    switch (tag) {
        .AssignExpr => {}, // handle plain assignment separately

        .MulAssignExpr,
        .DivAssignExpr,
        .ModAssignExpr,
        => {
            if (rhs.value.isZero() and lhs.ty.isInt() and rhs.ty.isInt()) {
                switch (tag) {
                    .DivAssignExpr => try p.errStr(.division_by_zero, div.?, "division"),
                    .ModAssignExpr => try p.errStr(.division_by_zero, mod.?, "remainder"),
                    else => {},
                }
            }
            _ = try lhsCopy.adjustTypes(token, &rhs, p, if (tag == .ModAssignExpr) .integer else .arithmetic);
            try lhs.bin(p, tag, rhs);
            return lhs;
        },

        .SubAssignExpr,
        .AddAssignExpr,
        => {
            if (lhs.ty.isPointer() and rhs.ty.isInt()) {
                try rhs.ptrCast(p, lhs.ty);
            } else {
                _ = try lhsCopy.adjustTypes(token, &rhs, p, .arithmetic);
            }
            try lhs.bin(p, tag, rhs);
            return lhs;
        },

        .ShlAssignExpr,
        .ShrAssignExpr,
        .BitAndAssignExpr,
        .BitXorAssignExpr,
        .BitOrAssignExpr,
        => {
            _ = try lhsCopy.adjustTypes(token, &rhs, p, .integer);
            try lhs.bin(p, tag, rhs);
            return lhs;
        },
        else => unreachable,
    }

    // rhs does not need to be qualified
    var unqualType = lhs.ty.canonicalize(.standard);
    unqualType.qual = .{};

    const eMsg = " from incompatible type ";
    if (lhs.ty.is(.Bool)) {
        // this is ridiculous but it's what clang does
        if (rhs.ty.isInt() or rhs.ty.isFloat() or rhs.ty.isPointer()) {
            try rhs.boolCast(p, lhs.ty, token);
        } else {
            try p.errStr(.incompatible_assign, token, try p.typePairStrExtra(lhs.ty, eMsg, rhs.ty));
        }
    } else if (unqualType.isInt()) {
        if (rhs.ty.isInt() or rhs.ty.isFloat()) {
            try rhs.intCast(p, unqualType, token);
        } else if (rhs.ty.isPointer()) {
            try p.errStr(.implicit_ptr_to_int, token, try p.typePairStrExtra(rhs.ty, " to ", lhs.ty));
            try rhs.intCast(p, unqualType, token);
        } else {
            try p.errStr(.incompatible_assign, token, try p.typePairStrExtra(lhs.ty, eMsg, rhs.ty));
        }
    } else if (unqualType.isFloat()) {
        if (rhs.ty.isInt() or rhs.ty.isFloat())
            try rhs.floatCast(p, unqualType)
        else
            try p.errStr(.incompatible_assign, token, try p.typePairStrExtra(lhs.ty, eMsg, rhs.ty));
    } else if (lhs.ty.isPointer()) {
        if (rhs.value.isZero()) {
            try rhs.nullCast(p, lhs.ty);
        } else if (rhs.ty.isInt()) {
            try p.errStr(.implicit_int_to_ptr, token, try p.typePairStrExtra(rhs.ty, " to ", lhs.ty));
            try rhs.ptrCast(p, unqualType);
        } else if (rhs.ty.isPointer()) {
            if (!unqualType.isVoidStar() and !rhs.ty.isVoidStar() and !unqualType.eql(rhs.ty, p.pp.comp, false)) {
                try p.errStr(.incompatible_ptr_assign, token, try p.typePairStrExtra(lhs.ty, eMsg, rhs.ty));
            } else if (!unqualType.eql(rhs.ty, p.pp.comp, true)) {
                if (!unqualType.getElemType().qual.hasQuals(rhs.ty.getElemType().qual))
                    try p.errStr(.ptr_assign_discards_quals, token, try p.typePairStrExtra(lhs.ty, eMsg, rhs.ty));
                try rhs.ptrCast(p, unqualType);
            }
        } else {
            try p.errStr(.incompatible_assign, token, try p.typePairStrExtra(lhs.ty, eMsg, rhs.ty));
        }
    } else if (lhs.ty.isRecord()) {
        if (!unqualType.eql(rhs.ty, p.pp.comp, false))
            try p.errStr(.incompatible_assign, token, try p.typePairStrExtra(lhs.ty, eMsg, rhs.ty));
    } else if (lhs.ty.isArray() or lhs.ty.isFunc()) {
        try p.errToken(.not_assignable, token);
    } else {
        try p.errStr(.incompatible_assign, token, try p.typePairStrExtra(lhs.ty, eMsg, rhs.ty));
    }

    try lhs.bin(p, tag, rhs);
    return lhs;
}

/// const-expression : conditional-expression
fn parseConstExpr(p: *Parser, declFolding: ConstDeclFoldingMode) Error!Result {
    const start = p.tokenIdx;

    const constDeclFolding = p.constDeclFolding;
    defer p.constDeclFolding = constDeclFolding;

    p.constDeclFolding = declFolding;

    const res = try p.parseCondExpr();
    try res.expect(p);

    if (!res.ty.isInt()) {
        try p.errToken(.expected_integer_constant_expr, start);
        return error.ParsingFailed;
    }
    // saveValue sets val to unavailable
    var copy = res;
    try copy.saveValue(p);
    return res;
}

/// conditional-expression : logical-OR-expression ('?' expression? ':' conditional-expression)?
fn parseCondExpr(p: *Parser) Error!Result {
    var cond = try p.logicalOrExpr();
    if (cond.empty(p) or p.eat(.QuestionMark) == null)
        return cond;
    const savedEval = p.noEval;

    // Depending on the value of the condition, avoid  evaluating unreachable
    var thenExpr = blk: {
        defer p.noEval = savedEval;
        if (cond.value.tag != .unavailable and !cond.value.getBool())
            p.noEval = true;

        break :blk try p.parseExpr();
    };
    try thenExpr.expect(p);

    const colon = try p.expectToken(.Colon);

    var elseExpr = blk: {
        defer p.noEval = savedEval;
        if (cond.value.tag != .unavailable and cond.value.getBool())
            p.noEval = true;

        break :blk try p.parseCondExpr();
    };
    try elseExpr.expect(p);

    _ = try thenExpr.adjustTypes(colon, &elseExpr, p, .conditional);

    if (cond.value.tag != .unavailable) {
        cond.value = if (cond.value.getBool()) thenExpr.value else elseExpr.value;
    } else {
        try thenExpr.saveValue(p);
        try elseExpr.saveValue(p);
    }

    cond.ty = thenExpr.ty;
    cond.node = try p.addNode(.{
        .tag = .CondExpr,
        .type = cond.ty,
        .data = .{ .if3 = .{ .cond = cond.node, .body = (try p.addList(&.{ thenExpr.node, elseExpr.node })).start } },
    });

    return cond;
}

/// logical-OR-expression : logical-AND-expression ('||' logical-AND-expression)*
fn logicalOrExpr(p: *Parser) Error!Result {
    var lhs = try p.logicalAndExpr();
    if (lhs.empty(p))
        return lhs;
    const savedEval = p.noEval;
    defer p.noEval = savedEval;

    while (p.eat(.PipePipe)) |token| {
        if (lhs.value.tag != .unavailable and lhs.value.getBool())
            p.noEval = true;

        var rhs = try p.logicalAndExpr();
        try rhs.expect(p);

        if (try lhs.adjustTypes(token, &rhs, p, .booleanLogic)) {
            const res = @intFromBool(lhs.value.getBool() or rhs.value.getBool());
            lhs.value = Value.int(res);
        }

        lhs.ty = .{ .specifier = .Int };
        try lhs.bin(p, .BoolOrExpr, rhs);
    }

    return lhs;
}

/// logical-AND-expression : or-expression ('&&' or-expression)*
fn logicalAndExpr(p: *Parser) Error!Result {
    var lhs = try p.parseOrExpr();
    if (lhs.empty(p))
        return lhs;

    const savedEval = p.noEval;
    defer p.noEval = savedEval;

    while (p.eat(.AmpersandAmpersand)) |token| {
        if (lhs.value.tag != .unavailable and !lhs.value.getBool())
            p.noEval = true;

        var rhs = try p.parseOrExpr();
        try rhs.expect(p);

        if (try lhs.adjustTypes(token, &rhs, p, .booleanLogic)) {
            const res = @intFromBool(lhs.value.getBool() and rhs.value.getBool());
            lhs.value = Value.int(res);
        }

        lhs.ty = .{ .specifier = .Int };
        try lhs.bin(p, .BoolAndExpr, rhs);
    }
    return lhs;
}

/// or-expression : xor-expression ('|' xor-expression)*
fn parseOrExpr(p: *Parser) Error!Result {
    var lhs = try p.parseXORExpr();
    if (lhs.empty(p))
        return lhs;

    while (p.eat(.Pipe)) |token| {
        var rhs = try p.parseXORExpr();
        try rhs.expect(p);

        if (try lhs.adjustTypes(token, &rhs, p, .integer))
            lhs.value = lhs.value.bitOr(rhs.value, lhs.ty, p.pp.comp);

        try lhs.bin(p, .BitOrExpr, rhs);
    }
    return lhs;
}

/// xor-expression : and-expression ('^' and-expression)*
fn parseXORExpr(p: *Parser) Error!Result {
    var lhs = try p.parseAndExpr();
    if (lhs.empty(p))
        return lhs;

    while (p.eat(.Caret)) |token| {
        var rhs = try p.parseAndExpr();
        try rhs.expect(p);

        if (try lhs.adjustTypes(token, &rhs, p, .integer))
            lhs.value = lhs.value.bitXor(rhs.value, lhs.ty, p.pp.comp);

        try lhs.bin(p, .BitXorExpr, rhs);
    }
    return lhs;
}

/// and-expression : equality-expression ('&' equality-expression)*
fn parseAndExpr(p: *Parser) Error!Result {
    var lhs = try p.parseEqExpr();
    if (lhs.empty(p))
        return lhs;

    while (p.eat(.Ampersand)) |token| {
        var rhs = try p.parseEqExpr();
        try rhs.expect(p);

        if (try lhs.adjustTypes(token, &rhs, p, .integer))
            lhs.value = lhs.value.bitAnd(rhs.value, lhs.ty, p.pp.comp);

        try lhs.bin(p, .BitAndExpr, rhs);
    }
    return lhs;
}

/// equality-expression : compare-expression (('==' | '!=') compare-expression)*
fn parseEqExpr(p: *Parser) Error!Result {
    var lhs = try p.parseCompExpr();
    if (lhs.empty(p))
        return lhs;

    while (true) {
        const eq = p.eat(.EqualEqual);
        const ne = eq orelse p.eat(.BangEqual);
        const tag = p.tokToTag(ne orelse break);
        var rhs = try p.parseCompExpr();
        try rhs.expect(p);

        if (try lhs.adjustTypes(ne.?, &rhs, p, .equality)) {
            const op: std.math.CompareOperator = if (tag == .EqualExpr) .eq else .neq;
            const res = lhs.value.compare(op, rhs.value, lhs.ty, p.pp.comp);

            lhs.value = Value.int(@intFromBool(res));
        }

        lhs.ty = .{ .specifier = .Int };
        try lhs.bin(p, tag, rhs);
    }
    return lhs;
}

/// compare-expression : shirt-expression (('<' | '<=' | '>' | '>=') shirt-expression)*
fn parseCompExpr(p: *Parser) Error!Result {
    var lhs = try p.parseShiftExpr();
    if (lhs.empty(p))
        return lhs;

    while (true) {
        const lt = p.eat(.AngleBracketLeft);
        const le = lt orelse p.eat(.AngleBracketLeftEqual);
        const gt = le orelse p.eat(.AngleBracketRight);
        const ge = gt orelse p.eat(.AngleBracketRightEqual);
        const tag = p.tokToTag(ge orelse break);
        var rhs = try p.parseShiftExpr();
        try rhs.expect(p);

        if (try lhs.adjustTypes(ge.?, &rhs, p, .relational)) {
            const op: std.math.CompareOperator = switch (tag) {
                .LessThanExpr => .lt,
                .LessThanEqualExpr => .lte,
                .GreaterThanExpr => .gt,
                .GreaterThanEqualExpr => .gte,
                else => unreachable,
            };

            const res = lhs.value.compare(op, rhs.value, lhs.ty, p.pp.comp);
            lhs.value = Value.int(@intFromBool(res));
        }

        lhs.ty = .{ .specifier = .Int };
        try lhs.bin(p, tag, rhs);
    }

    return lhs;
}

/// shift-expression : add-expression (('<<' | '>>') add-expression)*
fn parseShiftExpr(p: *Parser) Error!Result {
    var lhs = try p.parseAddExpr();
    if (lhs.empty(p))
        return lhs;

    while (true) {
        const shl = p.eat(.AngleBracketAngleBracketLeft);
        const shr = shl orelse p.eat(.AngleBracketAngleBracketRight);
        const tag = p.tokToTag(shr orelse break);
        var rhs = try p.parseAddExpr();
        try rhs.expect(p);

        if (try lhs.adjustTypes(shr.?, &rhs, p, .integer)) {
            if (shl != null) {
                lhs.value = lhs.value.shl(rhs.value, lhs.ty, p.pp.comp);
            } else {
                lhs.value = lhs.value.shr(rhs.value, lhs.ty, p.pp.comp);
            }
        }

        try lhs.bin(p, tag, rhs);
    }
    return lhs;
}

/// add-expression : mul-expression (('+' | '-') mul-expression)*
fn parseAddExpr(p: *Parser) Error!Result {
    var lhs = try p.parseMulExpr();
    if (lhs.empty(p))
        return lhs;

    while (true) {
        const plus = p.eat(.Plus);
        const minus = plus orelse p.eat(.Minus);
        const tag = p.tokToTag(minus orelse break);
        var rhs = try p.parseMulExpr();
        try rhs.expect(p);

        if (try lhs.adjustTypes(minus.?, &rhs, p, if (plus != null) .add else .sub)) {
            if (plus != null) {
                if (lhs.value.add(lhs.value, rhs.value, lhs.ty, p.pp.comp))
                    try p.errOverflow(plus.?, lhs);
            } else {
                if (lhs.value.sub(lhs.value, rhs.value, lhs.ty, p.pp.comp))
                    try p.errOverflow(minus.?, lhs);
            }
        }

        try lhs.bin(p, tag, rhs);
    }
    return lhs;
}

/// mul-expression : cast-expression (('*' | '/' | '%') cast-expression)*´
fn parseMulExpr(p: *Parser) Error!Result {
    var lhs = try p.parseCastExpr();
    if (lhs.empty(p))
        return lhs;

    while (true) {
        const mul = p.eat(.Asterisk);
        const div = mul orelse p.eat(.Slash);
        const percent = div orelse p.eat(.Percent);
        const tag = p.tokToTag(percent orelse break);
        var rhs = try p.parseCastExpr();
        try rhs.expect(p);

        if (rhs.value.isZero() and mul == null and !p.noEval and lhs.ty.isInt() and rhs.ty.isInt()) {
            const errTag: Diagnostics.Tag = if (p.inMacro) .division_by_zero_macro else .division_by_zero;
            lhs.value.tag = .unavailable;
            if (div != null) {
                try p.errStr(errTag, div.?, "division");
            } else {
                try p.errStr(errTag, percent.?, "remainder");
            }

            if (p.inMacro)
                return error.ParsingFailed;
        }

        if (try lhs.adjustTypes(percent.?, &rhs, p, if (tag == .ModExpr) .integer else .arithmetic)) {
            if (mul != null) {
                if (lhs.value.mul(lhs.value, rhs.value, lhs.ty, p.pp.comp))
                    try p.errOverflow(mul.?, lhs);
            } else if (div != null) {
                lhs.value = Value.div(lhs.value, rhs.value, lhs.ty, p.pp.comp);
            } else {
                var res = Value.rem(lhs.value, rhs.value, lhs.ty, p.pp.comp);
                if (res.tag == .unavailable) {
                    if (p.inMacro) {
                        // match clang behavior by defining invalid remainder to be zero in macros
                        res = Value.int(0);
                    } else {
                        try lhs.saveValue(p);
                        try rhs.saveValue(p);
                    }
                }
                lhs.value = res;
            }
        }

        try lhs.bin(p, tag, rhs);
    }

    return lhs;
}

/// This will always be the last message, if present
fn removeUnusedWarningForTok(p: *Parser, lastExprToken: TokenIndex) void {
    if (lastExprToken == 0) return;
    if (p.pp.comp.diag.list.items.len == 0) return;

    const lastExprLoc = p.pp.tokens.items(.loc)[lastExprToken];
    const lastMessage = p.pp.comp.diag.list.items[p.pp.comp.diag.list.items.len - 1];

    if (lastMessage.tag == .unused_value and lastMessage.loc.eql(lastExprLoc)) {
        p.pp.comp.diag.list.items.len = p.pp.comp.diag.list.items.len - 1;
    }
}

/// cast-expression
///  : '(' compoundStmt ')'
///  | '(' typeName ')' cast-expression
///  | '(' typeName ')' '{' initializerItems '}'
///  | __builtin_choose_expr '(' const-expression ',' assign-expression ',' assign-expression ')'
///  | __builtin_va_arg '('  assign-expression ',' typeName ')'
///  | unary-expression
fn parseCastExpr(p: *Parser) Error!Result {
    if (p.eat(.LParen)) |lp| castExpr: {
        if (p.getCurrToken() == .LBrace) {
            try p.err(.gnu_statement_expression);
            if (p.func.type == null) {
                try p.err(.stmt_expr_not_allowed_file_scope);
                return error.ParsingFailed;
            }

            var stmtExprState: StmtExprState = .{};
            const bodyNode = (try p.parseCompoundStmt(false, &stmtExprState)).?; // compoundStmt only returns null if .l_brace isn't the first token
            p.removeUnusedWarningForTok(stmtExprState.lastExprToken);

            var res = Result{
                .node = bodyNode,
                .ty = stmtExprState.lastExprRes.ty,
                .value = stmtExprState.lastExprRes.value,
            };
            try p.expectClosing(lp, .RParen);
            try res.un(p, .StmtExpr);
            return res;
        }
        const ty = (try p.parseTypeName()) orelse
            {
            p.tokenIdx -= 1;
            break :castExpr;
        };
        try p.expectClosing(lp, .RParen);

        if (p.getCurrToken() == .LBrace) {
            // compound literal
            if (ty.isFunc())
                try p.err(.func_init)
            else if (ty.is(.VariableLenArray)) {
                try p.err(.vla_init);
            } else if (ty.hasIncompleteSize() and !ty.is(.IncompleteArray)) {
                try p.errStr(.variable_incomplete_ty, p.tokenIdx, try p.typeStr(ty));
                return error.ParsingFailed;
            }

            var initListExpr = try p.initializer(ty);
            try initListExpr.un(p, .CompoundLiteralExpr);
            return initListExpr;
        }

        var operand = try p.parseCastExpr();
        try operand.expect(p);

        if (ty.is(.Void)) {
            // everything can cast to void
            operand.value.tag = .unavailable;
        } else if (ty.isInt() or ty.isFloat() or ty.isPointer()) cast: {
            try operand.lvalConversion(p);

            const oldFloat = operand.ty.isFloat();
            const newFloat = ty.isFloat();

            if (newFloat and operand.ty.isPointer()) {
                try p.errStr(.invalid_cast_to_float, lp, try p.typeStr(ty));
                return error.ParsingFailed;
            } else if (oldFloat and ty.isPointer()) {
                try p.errStr(.invalid_cast_to_pointer, lp, try p.typeStr(operand.ty));
                return error.ParsingFailed;
            }

            if (operand.value.tag == .unavailable)
                break :cast;

            const oldInt = operand.ty.isInt() or operand.ty.isPointer();
            const newInt = ty.isInt() or ty.isPointer();
            if (ty.is(.Bool)) {
                operand.value.toBool();
            } else if (oldFloat and newInt) {
                _ = operand.value.floatToInt(operand.ty, ty, p.pp.comp); // no warnings for explicit cast
            } else if (newFloat and oldInt) {
                operand.value.intToFloat(operand.ty, ty, p.pp.comp);
            } else if (newFloat and oldFloat) {
                operand.value.floatCast(operand.ty, ty, p.pp.comp);
            }
        } else {
            try p.errStr(.invalid_cast_type, lp, try p.typeStr(operand.ty));
            return error.ParsingFailed;
        }

        if (ty.containAnyQual())
            try p.errStr(.qual_cast, lp, try p.typeStr(ty));

        if (ty.isInt() and operand.ty.isPointer() and ty.sizeCompare(operand.ty, p.pp.comp) == .lt)
            try p.errStr(.cast_to_smaller_int, lp, try p.typePairStrExtra(ty, " from ", operand.ty));

        operand.ty = ty;
        operand.ty.qual = .{};

        try operand.un(p, .CastExpr);
        return operand;
    }

    switch (p.getCurrToken()) {
        .BuiltinChooseExpr => return p.parseBuiltinChooseExpr(),
        .BuiltinVaArg => return p.builtinVaArg(),
        // TODO: other special-cased builtins
        else => {},
    }

    return p.parseUnaryExpr();
}

fn parseBuiltinChooseExpr(p: *Parser) Error!Result {
    p.tokenIdx += 1;
    const lp = try p.expectToken(.LParen);
    const condToken = p.tokenIdx;
    var cond = try p.parseConstExpr(.NoConstDeclFolding);
    if (cond.value.tag == .unavailable) {
        try p.errToken(.builtin_choose_cond, condToken);
        return error.ParsingFailed;
    }

    _ = try p.expectToken(.Comma);

    var thenExpr = if (cond.value.getBool()) try p.parseAssignExpr() else try p.parseNoEval(parseAssignExpr);
    try thenExpr.expect(p);

    _ = try p.expectToken(.Comma);

    var elseExpr = if (!cond.value.getBool()) try p.parseAssignExpr() else try p.parseNoEval(parseAssignExpr);
    try elseExpr.expect(p);

    try p.expectClosing(lp, .RParen);

    if (cond.value.getBool()) {
        cond.value = thenExpr.value;
        cond.ty = thenExpr.ty;
    } else {
        cond.value = elseExpr.value;
        cond.ty = elseExpr.ty;
    }
    cond.node = try p.addNode(.{
        .tag = .BuiltinChooseExpr,
        .type = cond.ty,
        .data = .{ .if3 = .{ .cond = cond.node, .body = (try p.addList(&.{ thenExpr.node, elseExpr.node })).start } },
    });

    return cond;
}

fn builtinVaArg(p: *Parser) Error!Result {
    const builtinToken = p.tokenIdx;
    p.tokenIdx += 1;

    const lp = try p.expectToken(.LParen);
    const vaListToken = p.tokenIdx;
    var vaList = try p.parseAssignExpr();
    try vaList.expect(p);
    try vaList.lvalConversion(p);

    _ = try p.expectToken(.Comma);

    const ty = (try p.parseTypeName()) orelse {
        try p.err(.expected_type);
        return error.ParsingFailed;
    };
    try p.expectClosing(lp, .RParen);

    if (!vaList.ty.eql(p.pp.comp.types.vaList, p.pp.comp, true)) {
        try p.errStr(.incompatible_va_arg, vaListToken, try p.typeStr(vaList.ty));
        return error.ParsingFailed;
    }

    return Result{ .ty = ty, .node = try p.addNode(.{
        .tag = .BuiltinCallExprOne,
        .type = ty,
        .data = .{ .decl = .{ .name = builtinToken, .node = vaList.node } },
    }) };
}

/// unaryExpr
///  : primary-expression suffix-expression*
///  | '&&' identifier
///  | ('&' | '*' | '+' | '-' | '~' | '!' | '++' | '--' | `__extension__`) cast-expression
///  | `sizeof` unary-expression
///  | `sizeof` '(' type-name ')'
///  | alignof '(' type-name ')'
fn parseUnaryExpr(p: *Parser) Error!Result {
    const token = p.tokenIdx;
    switch (p.tokenIds[token]) {
        .Ampersand => {
            if (p.inMacro) {
                try p.err(.invalid_preproc_operator);
                return error.ParsingFailed;
            }
            p.tokenIdx += 1;
            var operand = try p.parseCastExpr();
            try operand.expect(p);

            const slice = p.nodes.slice();
            if (!AST.isLValue(slice, p.data.items, p.valueMap, operand.node)) {
                try p.errToken(.addr_of_rvalue, token);
            }

            if (operand.ty.qual.register)
                try p.errToken(.addr_of_register, token);

            const elemType = try p.arena.create(Type);
            elemType.* = operand.ty;
            operand.ty = Type{
                .specifier = .Pointer,
                .data = .{ .subType = elemType },
            };

            try operand.saveValue(p);
            try operand.un(p, .AddrOfExpr);
            return operand;
        },

        .AmpersandAmpersand => {
            const addressToken = p.tokenIdx;
            p.tokenIdx += 1;
            const nameToken = try p.expectIdentifier();
            try p.errToken(.gnu_label_as_value, addressToken);
            p.containsAddresssOfLabel = true;

            const str = p.getTokenSlice(nameToken);
            if (p.findLabel(str) == null)
                try p.labels.append(.{ .unresolvedGoto = nameToken });

            const elemType = try p.arena.create(Type);
            elemType.* = .{ .specifier = .Void };

            const resultType = Type{ .specifier = .Pointer, .data = .{ .subType = elemType } };
            return Result{
                .node = try p.addNode(.{
                    .tag = .AddrOfLabel,
                    .data = .{ .declRef = nameToken },
                    .type = resultType,
                }),
                .ty = resultType,
            };
        },

        .Asterisk => {
            const asteriskLoc = p.tokenIdx;
            p.tokenIdx += 1;
            var operand = try p.parseCastExpr();
            try operand.expect(p);

            if (operand.ty.isArray() or operand.ty.isPointer()) {
                operand.ty = operand.ty.getElemType();
            } else if (!operand.ty.isFunc()) {
                try p.errToken(.indirection_ptr, token);
            }

            if (operand.ty.hasIncompleteSize() and !operand.ty.is(.Void))
                try p.errStr(.deref_incomplete_ty_ptr, asteriskLoc, try p.typeStr(operand.ty));

            operand.ty.qual = .{};
            try operand.un(p, .DerefExpr);
            return operand;
        },

        .Plus => {
            p.tokenIdx += 1;
            var operand = try p.parseCastExpr();
            try operand.expect(p);
            try operand.lvalConversion(p);

            if (!operand.ty.isInt() and !operand.ty.isFloat())
                try p.errStr(.invalid_argument_un, token, try p.typeStr(operand.ty));

            if (operand.ty.isInt())
                try operand.intCast(p, operand.ty.integerPromotion(p.pp.comp), token);

            return operand;
        },

        .Minus => {
            p.tokenIdx += 1;
            var operand = try p.parseCastExpr();
            try operand.expect(p);
            try operand.lvalConversion(p);

            if (!operand.ty.isInt() and !operand.ty.isFloat())
                try p.errStr(.invalid_argument_un, token, try p.typeStr(operand.ty));

            if (operand.ty.isInt())
                try operand.intCast(p, operand.ty.integerPromotion(p.pp.comp), token);

            if (operand.value.isNumeric()) {
                _ = operand.value.sub(operand.value.zero(), operand.value, operand.ty, p.pp.comp);
            } else {
                operand.value.tag = .unavailable;
            }

            try operand.un(p, .NegateExpr);
            return operand;
        },

        .PlusPlus => {
            p.tokenIdx += 1;
            var operand = try p.parseCastExpr();
            try operand.expect(p);

            if (!operand.ty.isInt() and !operand.ty.isFloat() and !operand.ty.isReal() and !operand.ty.isPointer())
                try p.errStr(.invalid_argument_un, token, try p.typeStr(operand.ty));

            if (!AST.isLValue(p.nodes.slice(), p.data.items, p.valueMap, operand.node) or operand.ty.isConst()) {
                try p.errToken(.not_assignable, token);
                return error.ParsingFailed;
            }

            if (operand.ty.isInt())
                try operand.intCast(p, operand.ty.integerPromotion(p.pp.comp), token);

            if (operand.value.isNumeric()) {
                if (operand.value.add(operand.value, operand.value.one(), operand.ty, p.pp.comp))
                    try p.errOverflow(token, operand);
            } else {
                operand.value.tag = .unavailable;
            }

            try operand.un(p, .PreIncExpr);
            return operand;
        },

        .MinusMinus => {
            p.tokenIdx += 1;
            var operand = try p.parseCastExpr();
            try operand.expect(p);

            if (!operand.ty.isInt() and !operand.ty.isFloat() and !operand.ty.isReal() and !operand.ty.isPointer())
                try p.errStr(.invalid_argument_un, token, try p.typeStr(operand.ty));

            if (!AST.isLValue(p.nodes.slice(), p.data.items, p.valueMap, operand.node) or operand.ty.isConst()) {
                try p.errToken(.not_assignable, token);
                return error.ParsingFailed;
            }

            if (operand.ty.isInt())
                try operand.intCast(p, operand.ty.integerPromotion(p.pp.comp), token);

            if (operand.value.isNumeric()) {
                if (operand.value.sub(operand.value, operand.value.one(), operand.ty, p.pp.comp))
                    try p.errOverflow(token, operand);
            } else {
                operand.value.tag = .unavailable;
            }

            try operand.un(p, .PreDecExpr);
            return operand;
        },

        .Tilde => {
            p.tokenIdx += 1;
            var operand = try p.parseCastExpr();
            try operand.expect(p);
            try operand.lvalConversion(p);

            if (!operand.ty.isInt())
                try p.errStr(.invalid_argument_un, token, try p.typeStr(operand.ty));

            if (operand.ty.isInt()) {
                try operand.intCast(p, operand.ty.integerPromotion(p.pp.comp), token);
                if (operand.value.tag == .int) {
                    operand.value = operand.value.bitNot(operand.ty, p.pp.comp);
                }
            } else {
                operand.value.tag = .unavailable;
            }

            try operand.un(p, .BitNotExpr);
            return operand;
        },

        .Bang => {
            p.tokenIdx += 1;
            var operand = try p.parseCastExpr();
            try operand.expect(p);
            try operand.lvalConversion(p);

            if (!operand.ty.isInt() and !operand.ty.isFloat() and !operand.ty.isPointer())
                try p.errStr(.invalid_argument_un, token, try p.typeStr(operand.ty));

            if (operand.ty.isInt())
                try operand.intCast(p, operand.ty.integerPromotion(p.pp.comp), token);

            if (operand.value.tag == .int) {
                const res = Value.int(@intFromBool(!operand.value.getBool()));
                operand.value = res;
            } else {
                operand.value.tag = .unavailable;
            }

            operand.ty = .{ .specifier = .Int };
            try operand.un(p, .BoolNotExpr);
            return operand;
        },

        .KeywordSizeof => {
            p.tokenIdx += 1;
            const expectedParen = p.tokenIdx;
            var res = Result{};
            if (try p.parseTypeName()) |ty| {
                res.ty = ty;
                try p.errToken(.expected_parens_around_typename, expectedParen);
            } else if (p.eat(.LParen)) |lp| {
                if (try p.parseTypeName()) |ty| {
                    res.ty = ty;
                    try p.expectClosing(lp, .RParen);
                } else {
                    p.tokenIdx = expectedParen;
                    res = try p.parseNoEval(parseUnaryExpr);
                }
            } else {
                res = try p.parseNoEval(parseUnaryExpr);
            }

            if (res.ty.sizeof(p.pp.comp)) |size| {
                res.value = .{ .tag = .int, .data = .{ .int = size } };
            } else {
                res.value.tag = .unavailable;
                try p.errStr(.invalid_sizeof, expectedParen - 1, try p.typeStr(res.ty));
            }

            res.ty = p.pp.comp.types.size;
            try res.un(p, .SizeOfExpr);
            return res;
        },

        .KeywordAlignof, .KeywordGccAlignof1, .KeywordGccAlignof2 => {
            p.tokenIdx += 1;
            const expectedParen = p.tokenIdx;
            var res = Result{};
            if (try p.parseTypeName()) |ty| {
                res.ty = ty;
                try p.errToken(.expected_parens_around_typename, expectedParen);
            } else if (p.eat(.LParen)) |lp| {
                if (try p.parseTypeName()) |ty| {
                    res.ty = ty;
                    try p.expectClosing(lp, .RParen);
                } else {
                    p.tokenIdx = expectedParen;
                    res = try p.parseNoEval(parseUnaryExpr);
                    try p.errToken(.alignof_expr, expectedParen);
                }
            } else {
                res = try p.parseNoEval(parseUnaryExpr);
                try p.errToken(.alignof_expr, expectedParen);
            }

            res.value = Value.int(res.ty.alignof(p.pp.comp));
            res.ty = p.pp.comp.types.size;
            try res.un(p, .AlignOfExpr);
            return res;
        },

        .KeywordGccExtension => {
            p.tokenIdx += 1;
            const savedExtension = p.extensionSuppressd;
            defer p.extensionSuppressd = savedExtension;
            p.extensionSuppressd = true;

            var child = try p.parseCastExpr();
            try child.expect(p);
            return child;
        },

        else => {
            var lhs = try p.parsePrimaryExpr();
            if (lhs.empty(p))
                return lhs;
            while (true) {
                const suffix = try p.parseSuffixExpr(lhs);
                if (suffix.empty(p)) break;
                lhs = suffix;
            }
            return lhs;
        },
    }
}

/// suffix-expression
///  : '[' expression ']'
///  | '(' argument-expression-list? ')'
///  | '.' identifier
///  | '->' identifier
///  | '++'
///  | '--'
fn parseSuffixExpr(p: *Parser, lhs: Result) Error!Result {
    std.debug.assert(!lhs.empty(p));
    switch (p.getCurrToken()) {
        .LBracket => {
            const lb = p.tokenIdx;
            p.tokenIdx += 1;
            var index = try p.parseExpr();
            try index.expect(p);
            try p.expectClosing(lb, .RBracket);

            const lhsType = lhs.ty;
            const rhsType = index.ty;
            var ptr = lhs;
            try ptr.lvalConversion(p);
            try index.lvalConversion(p);
            if (ptr.ty.isPointer()) {
                ptr.ty = ptr.ty.getElemType();
                if (!index.ty.isInt()) try p.errToken(.invalid_index, lb);
                try p.checkArrayBounds(index, lhsType, lb);
            } else if (index.ty.isPointer()) {
                index.ty = index.ty.getElemType();
                if (!ptr.ty.isInt()) try p.errToken(.invalid_index, lb);
                try p.checkArrayBounds(ptr, rhsType, lb);
                std.mem.swap(Result, &ptr, &index);
            } else {
                try p.errToken(.invalid_subscript, lb);
            }

            try ptr.saveValue(p);
            try index.saveValue(p);
            try ptr.bin(p, .ArrayAccessExpr, index);
            return ptr;
        },

        .LParen => return p.parseCallExpr(lhs),

        .Period => {
            p.tokenIdx += 1;
            const name = try p.expectIdentifier();
            return p.fieldAccess(lhs, name, false);
        },

        .Arrow => {
            p.tokenIdx += 1;
            const name = try p.expectIdentifier();
            if (lhs.ty.isArray()) {
                var copy = lhs;
                copy.ty.decayArray();
                try copy.un(p, .ArrayToPointer);
                return p.fieldAccess(copy, name, true);
            }
            return p.fieldAccess(lhs, name, true);
        },

        .PlusPlus => {
            defer p.tokenIdx += 1;
            var operand = lhs;

            if (!operand.ty.isInt() and !operand.ty.isFloat() and !operand.ty.isReal() and !operand.ty.isPointer())
                try p.errStr(.invalid_argument_un, p.tokenIdx, try p.typeStr(operand.ty));

            if (!AST.isLValue(p.nodes.slice(), p.data.items, p.valueMap, operand.node) or operand.ty.isConst()) {
                try p.err(.not_assignable);
                return error.ParsingFailed;
            }

            if (operand.ty.isInt())
                try operand.intCast(p, operand.ty.integerPromotion(p.pp.comp), p.tokenIdx);

            try operand.un(p, .PostIncExpr);
            return operand;
        },

        .MinusMinus => {
            defer p.tokenIdx += 1;
            var operand = lhs;

            if (!operand.ty.isInt() and !operand.ty.isFloat() and !operand.ty.isReal() and !operand.ty.isPointer())
                try p.errStr(.invalid_argument_un, p.tokenIdx, try p.typeStr(operand.ty));

            if (!AST.isLValue(p.nodes.slice(), p.data.items, p.valueMap, operand.node) or operand.ty.isConst()) {
                try p.err(.not_assignable);
                return error.ParsingFailed;
            }

            if (operand.ty.isInt())
                try operand.intCast(p, operand.ty.integerPromotion(p.pp.comp), p.tokenIdx);

            try operand.un(p, .PostDecExpr);
            return operand;
        },

        else => return Result{},
    }
}

fn fieldAccess(
    p: *Parser,
    lhs: Result,
    fieldNameToken: TokenIndex,
    isArrow: bool,
) !Result {
    const exprType = lhs.ty;
    const isPtr = exprType.isPointer();
    const exprBaseType = if (isPtr) exprType.getElemType() else exprType;
    const recordType = exprBaseType.canonicalize(.standard);

    switch (recordType.specifier) {
        .Struct, .Union => {},
        else => {
            try p.errStr(.expected_record_ty, fieldNameToken, try p.typeStr(exprType));
            return error.ParsingFailed;
        },
    }

    if (recordType.hasIncompleteSize()) {
        try p.errStr(.deref_incomplete_ty_ptr, fieldNameToken - 2, try p.typeStr(exprBaseType));
        return error.ParsingFailed;
    }

    if (isArrow and !isPtr) try p.errStr(.member_expr_not_ptr, fieldNameToken, try p.typeStr(exprType));
    if (!isArrow and isPtr) try p.errStr(.member_expr_ptr, fieldNameToken, try p.typeStr(exprType));

    const fieldName = p.getTokenSlice(fieldNameToken);
    if (!recordType.hasField(fieldName)) {
        p.strings.items.len = 0;

        try p.strings.writer().print("'{s}' in '", .{fieldName});
        try exprType.print(p.strings.writer());
        try p.strings.append('\'');

        const duped = try p.pp.comp.diag.arena.allocator().dupe(u8, p.strings.items);
        try p.errStr(.no_such_member, fieldNameToken, duped);
        return error.ParsingFailed;
    }

    return p.fieldAccessExtra(lhs.node, recordType, fieldName, isArrow);
}

/// This function handles extra field access for a given record type.
///
/// # Arguments
/// * `p` - A pointer to the parser
/// * `lhs` - The node index for the left-hand side expression
/// * `recordType` - The type of the record
/// * `fieldName` - The name of the field to access
/// * `isArrow` - Indicates whether the arrow operator is used
///
/// # Returns
/// An Error or Result
fn fieldAccessExtra(
    p: *Parser,
    lhs: NodeIndex,
    recordType: Type,
    fieldName: []const u8,
    isArrow: bool, // is arrow operator
) Error!Result {
    for (recordType.data.record.fields, 0..) |f, i| {
        if (f.isAnonymousRecord()) {
            if (!f.ty.hasField(fieldName)) continue;
            const inner = try p.addNode(.{
                .tag = if (isArrow) .MemberAccessPtrExpr else .MemberAccessExpr,
                .type = f.ty,
                .data = .{ .member = .{ .lhs = lhs, .index = @intCast(i) } },
            });
            return p.fieldAccessExtra(inner, f.ty, fieldName, false);
        }
        if (std.mem.eql(u8, fieldName, f.name))
            return Result{
                .ty = f.ty,
                .node = try p.addNode(.{
                    .tag = if (isArrow) .MemberAccessPtrExpr else .MemberAccessExpr,
                    .type = f.ty,
                    .data = .{ .member = .{ .lhs = lhs, .index = @intCast(i) } },
                }),
            };
    }
    // We already checked that this container has a field by the name.
    unreachable;
}

fn reportParam(p: *Parser, paramToken: TokenIndex, arg: Result, argCount: u32, params: []Type.Function.Param) Error!void {
    try p.errStr(.incompatible_param, paramToken, try p.typeStr(arg.ty));
    try p.errToken(.parameter_here, params[argCount].nameToken);
}

fn parseCallExpr(p: *Parser, lhs: Result) Error!Result {
    const lParen = p.tokenIdx;
    p.tokenIdx += 1;
    const ty = lhs.ty.isCallable() orelse {
        try p.errStr(.not_callable, lParen, try p.typeStr(lhs.ty));
        return error.ParsingFailed;
    };

    const params = ty.getParams();
    var func = lhs;
    try func.lvalConversion(p);

    const listBufferTop = p.listBuffer.items.len;
    defer p.listBuffer.items.len = listBufferTop;
    try p.listBuffer.append(func.node);
    var argCount: u32 = 0;

    const builtinNode = p.getNode(lhs.node, .BuiltinCallExprOne);

    var firstAfter = lParen;
    while (p.eat(.RParen) == null) {
        const paramToken = p.tokenIdx;
        if (argCount == params.len)
            firstAfter = p.tokenIdx;

        var arg = try p.parseAssignExpr();
        try arg.expect(p);
        const rawArgNode = arg.node;
        try arg.lvalConversion(p);
        if (arg.ty.hasIncompleteSize() and !arg.ty.is(.Void))
            return error.ParsingFailed;

        if (argCount >= params.len) {
            if (arg.ty.isInt()) try arg.intCast(p, arg.ty.integerPromotion(p.pp.comp), paramToken);
            if (arg.ty.is(.Float)) try arg.floatCast(p, .{ .specifier = .Double });
            try arg.saveValue(p);
            try p.listBuffer.append(arg.node);
            argCount += 1;

            _ = p.eat(.Comma) orelse {
                try p.expectClosing(lParen, .RParen);
                break;
            };
            continue;
        }

        const paramType = params[argCount].ty;
        if (paramType.is(.SpecialVaStart)) vaStart: {
            const builtinToken = p.nodes.items(.data)[@intFromEnum(builtinNode.?)].decl.name;
            const funcType = p.func.type orelse {
                try p.errToken(.va_start_not_in_func, builtinToken);
                break :vaStart;
            };

            const funcParams = funcType.getParams();
            if (funcType.specifier != .VarArgsFunc or funcParams.len == 0) {
                try p.errToken(.va_start_fixed_args, builtinToken);
                break :vaStart;
            }

            const lastParamName = funcParams[funcType.data.func.params.len - 1].name;
            const declRef = p.getNode(rawArgNode, .DeclRefExpr);
            if (declRef == null or
                !std.mem.eql(u8, p.getTokenSlice(p.nodes.items(.data)[@intFromEnum(declRef.?)].declRef), lastParamName))
            {
                try p.errToken(.va_start_not_last_param, paramToken);
            }
        } else if (paramType.is(.Bool)) {
            // this is ridiculous but it's what clang does
            if (arg.ty.isInt() or arg.ty.isFloat() or arg.ty.isPointer())
                try arg.boolCast(p, paramType, params[argCount].nameToken)
            else
                try p.reportParam(paramToken, arg, argCount, params);
        } else if (paramType.isInt()) {
            if (arg.ty.isInt() or arg.ty.isFloat()) {
                try arg.intCast(p, paramType, paramToken);
            } else if (arg.ty.isPointer()) {
                try p.errStr(
                    .implicit_ptr_to_int,
                    paramToken,
                    try p.typePairStrExtra(arg.ty, " to ", paramType),
                );
                try p.errToken(.parameter_here, params[argCount].nameToken);
                try arg.intCast(p, paramType, paramToken);
            } else {
                try p.reportParam(paramToken, arg, argCount, params);
            }
        } else if (paramType.isFloat()) {
            if (arg.ty.isInt() or arg.ty.isFloat())
                try arg.floatCast(p, paramType)
            else
                try p.reportParam(paramToken, arg, argCount, params);
        } else if (paramType.isPointer()) {
            if (arg.value.isZero()) {
                try arg.nullCast(p, paramType);
            } else if (arg.ty.isInt()) {
                try p.errStr(
                    .implicit_int_to_ptr,
                    paramToken,
                    try p.typePairStrExtra(arg.ty, " to ", paramType),
                );
                try p.errToken(.parameter_here, params[argCount].nameToken);
                try arg.intCast(p, paramType, paramToken);
            } else if (!arg.ty.isVoidStar() and !paramType.isVoidStar() and !paramType.eql(arg.ty, p.pp.comp, false)) {
                try p.reportParam(paramToken, arg, argCount, params);
            }
        } else if (paramType.isRecord()) {
            if (!paramType.eql(arg.ty, p.pp.comp, false)) {
                try p.reportParam(paramToken, arg, argCount, params);
            }
        } else {
            // should be unreachable
            try p.reportParam(paramToken, arg, argCount, params);
        }

        try arg.saveValue(p);
        try p.listBuffer.append(arg.node);
        argCount += 1;
        _ = p.eat(.Comma) orelse {
            try p.expectClosing(lParen, .RParen);
            break;
        };
    }

    const extra = Diagnostics.Message.Extra{
        .arguments = .{
            .expected = @intCast(params.len),
            .actual = @intCast(argCount),
        },
    };
    if (ty.is(.Func) and params.len != argCount)
        try p.errExtra(.expected_arguments, firstAfter, extra);

    if (ty.is(.OldStyleFunc) and params.len != argCount)
        try p.errExtra(.expected_arguments_old, firstAfter, extra);

    if (ty.is(.VarArgsFunc) and argCount < params.len)
        try p.errExtra(.expected_at_least_arguments, firstAfter, extra);

    if (builtinNode) |some| {
        const index = @intFromEnum(some);
        var callNode = p.nodes.get(index);
        defer p.nodes.set(index, callNode);

        const args = p.listBuffer.items[listBufferTop..];
        switch (argCount) {
            0 => {},
            1 => callNode.data.decl.node = args[1], // args[0] == func.node
            else => {
                callNode.tag = .BuiltinCallExpr;
                args[0] = @enumFromInt(callNode.data.decl.name);
                callNode.data = .{ .range = try p.addList(args) };
            },
        }
        return Result{ .node = some, .ty = callNode.type.getReturnType() };
    }

    var callNode: AST.Node = .{
        .tag = .CallExprOne,
        .type = ty.getReturnType(),
        .data = .{ .binExpr = .{ .lhs = lhs.node, .rhs = .none } },
    };

    const args = p.listBuffer.items[listBufferTop..];
    switch (argCount) {
        0 => {},
        1 => callNode.data.binExpr.rhs = args[1], //args[0]  == lhs.node
        else => {
            callNode.tag = .CallExpr;
            callNode.data = .{ .range = try p.addList(args) };
        },
    }
    return Result{ .node = try p.addNode(callNode), .ty = callNode.type };
}

fn checkArrayBounds(p: *Parser, index: Result, arrayType: Type, token: TokenIndex) !void {
    if (index.value.tag == .unavailable) return;
    const len = Value.int(arrayType.arrayLen() orelse return);

    if (index.ty.isUnsignedInt(p.pp.comp)) {
        if (index.value.compare(.gte, len, p.pp.comp.types.size, p.pp.comp))
            try p.errExtra(.array_after, token, .{ .unsigned = index.value.data.int });
    } else {
        if (index.value.compare(.lt, Value.int(0), index.ty, p.pp.comp)) {
            try p.errExtra(.array_before, token, .{
                .signed = index.value.signExtend(index.ty, p.pp.comp),
            });
        } else if (index.value.compare(.gte, len, p.pp.comp.types.size, p.pp.comp)) {
            try p.errExtra(.array_after, token, .{ .unsigned = index.value.data.int });
        }
    }
}

//// primary-expression
////  : identifier
////  | integer-literal
////  | float-literal
////  | imaginary-literal
////  | char-literal
////  | string-literal
////  | '(' expression ')'
////  | generic-selection
fn parsePrimaryExpr(p: *Parser) Error!Result {
    if (p.eat(.LParen)) |lp| {
        var e = try p.parseExpr();
        try e.expect(p);
        try p.expectClosing(lp, .RParen);
        try e.un(p, .ParenExpr);
        return e;
    }

    switch (p.getCurrToken()) {
        .Identifier, .ExtendedIdentifier => {
            const nameToken = p.expectIdentifier() catch unreachable;
            const name = p.getTokenSlice(nameToken);
            if (p.pp.comp.builtins.get(name)) |some| {
                for (p.tokenIds[p.tokenIdx..]) |id| switch (id) {
                    .RParen => {}, // closing grouped expr
                    .LParen => break, // beginning of a call
                    else => {
                        try p.errToken(.builtin_must_be_called, nameToken);
                        return error.ParsingFailed;
                    },
                };
                return Result{
                    .ty = some,
                    .node = try p.addNode(.{
                        .tag = .BuiltinCallExprOne,
                        .type = some,
                        .data = .{ .decl = .{ .name = nameToken, .node = .none } },
                    }),
                };
            }
            if (p.symStack.findSymbol(nameToken)) |sym| {
                try p.checkDeprecatedUnavailable(sym.type, nameToken, sym.token);
                if (sym.value.tag == .int) {
                    switch (p.constDeclFolding) {
                        .GNUFoldingExtension => try p.errToken(.const_decl_folded, nameToken),
                        .GNUVLAFoldingExtension => try p.errToken(.const_decl_folded_vla, nameToken),
                        else => {},
                    }
                }
                return Result{
                    .value = if (p.constDeclFolding == .NoConstDeclFolding) Value{} else sym.value,
                    .ty = sym.type,
                    .node = try p.addNode(.{
                        .tag = if (sym.kind == .enumeration) .EnumerationRef else .DeclRefExpr,
                        .type = sym.type,
                        .data = .{ .declRef = nameToken },
                    }),
                };
            }

            if (p.getCurrToken() == .LParen) {
                // implicitly declare simple functions as like `puts("foo")`;
                // allow implicitly declaring functions before C99 like `puts("foo")`
                if (std.mem.startsWith(u8, name, "__builtin_"))
                    try p.errStr(.unknown_builtin, nameToken, name)
                else
                    try p.errStr(.implicit_func_decl, nameToken, name);

                const funcType = try p.arena.create(Type.Function);
                funcType.* = .{ .returnType = .{ .specifier = .Int }, .params = &.{} };
                const ty: Type = .{ .specifier = .OldStyleFunc, .data = .{ .func = funcType } };
                const node = try p.addNode(.{
                    .type = ty,
                    .tag = .FnProto,
                    .data = .{ .decl = .{ .name = nameToken } },
                });

                try p.declBuffer.append(node);
                try p.symStack.declareSymbol(ty, nameToken, node);

                return Result{
                    .ty = ty,
                    .node = try p.addNode(.{ .tag = .DeclRefExpr, .type = ty, .data = .{ .declRef = nameToken } }),
                };
            }
            try p.errStr(.undeclared_identifier, nameToken, p.getTokenSlice(nameToken));
            return error.ParsingFailed;
        },

        .MacroFunc, .MacroFunction => {
            defer p.tokenIdx += 1;
            var ty: Type = undefined;
            var tok = p.tokenIdx;
            if (p.func.ident) |some| {
                ty = some.ty;
                tok = p.nodes.items(.data)[@intFromEnum(some.node)].decl.name;
            } else if (p.func.type) |_| {
                p.strings.items.len = 0;
                try p.strings.appendSlice(p.getTokenSlice(p.func.name));
                try p.strings.append(0);
                const predef = try p.makePredefinedIdentifier();
                ty = predef.ty;
                p.func.ident = predef;
            } else {
                p.strings.items.len = 0;
                try p.strings.append(0);
                const predef = try p.makePredefinedIdentifier();
                ty = predef.ty;
                p.func.ident = predef;
                try p.declBuffer.append(predef.node);
            }
            if (p.func.type == null)
                try p.err(.predefined_top_level);
            return Result{
                .ty = ty,
                .node = try p.addNode(.{
                    .tag = .DeclRefExpr,
                    .type = ty,
                    .data = .{ .declRef = tok },
                }),
            };
        },

        .MacroPrettyFunc => {
            defer p.tokenIdx += 1;
            var ty: Type = undefined;
            if (p.func.prettyIdent) |some| {
                ty = some.ty;
            } else if (p.func.type) |funcType| {
                p.strings.items.len = 0;
                try Type.printNamed(funcType, p.getTokenSlice(p.func.name), p.strings.writer());
                try p.strings.append(0);
                const predef = try p.makePredefinedIdentifier();
                ty = predef.ty;
                p.func.prettyIdent = predef;
            } else {
                p.strings.items.len = 0;
                try p.strings.appendSlice("top level\x00");
                const predef = try p.makePredefinedIdentifier();
                ty = predef.ty;
                p.func.prettyIdent = predef;
                try p.declBuffer.append(predef.node);
            }

            if (p.func.type == null)
                try p.err(.predefined_top_level);
            return Result{
                .ty = ty,
                .node = try p.addNode(.{
                    .tag = .DeclRefExpr,
                    .type = ty,
                    .data = .{ .declRef = p.tokenIdx },
                }),
            };
        },

        .StringLiteral,
        .StringLiteralUTF_8,
        .StringLiteralUTF_16,
        .StringLiteralUTF_32,
        .StringLiteralWide,
        => return p.parseStringLiteral(),

        .CharLiteral,
        .CharLiteralUTF_16,
        .CharLiteralUTF_32,
        .CharLiteralWide,
        => return p.parseCharLiteral(),

        .FloatLiteral, .ImaginaryLiteral => |tag| {
            defer p.tokenIdx += 1;

            const ty = Type{ .specifier = .Double };
            const dValue = try p.parseFloat(p.tokenIdx, f64);
            var res = Result{
                .ty = ty,
                .node = try p.addNode(.{ .tag = .DoubleLiteral, .type = ty, .data = undefined }),
                .value = Value.float(dValue),
            };

            if (!p.inMacro)
                try p.valueMap.put(res.node, res.value);

            if (tag == .ImaginaryLiteral) {
                try p.err(.gnu_imaginary_constant);
                res.ty = .{ .specifier = .ComplexDouble };
                res.value.tag = .unavailable;
                try res.un(p, .ImaginaryLiteral);
            }
            return res;
        },

        .FloatLiteral_F,
        .ImaginaryLiteral_F,
        => |tag| {
            defer p.tokenIdx += 1;

            const ty = Type{ .specifier = .Float };
            const fValue = try p.parseFloat(p.tokenIdx, f64);
            var res = Result{
                .ty = ty,
                .node = try p.addNode(.{ .tag = .FloatLiteral, .type = ty, .data = undefined }),
                .value = Value.float(fValue),
            };

            if (!p.inMacro)
                try p.valueMap.put(res.node, res.value);

            if (tag == .ImaginaryLiteral_F) {
                try p.err(.gnu_imaginary_constant);
                res.ty = .{ .specifier = .ComplexFloat };
                res.value.tag = .unavailable;
                try res.un(p, .ImaginaryLiteral);
            }
            return res;
        },

        .FloatLiteral_L => return p.todo("long double literals"),

        .ImaginaryLiteral_L => {
            try p.err(.gnu_imaginary_constant);
            return p.todo("long double imaginary literals");
        },

        .Zero => {
            p.tokenIdx += 1;
            var res: Result = .{ .value = Value.int(0) };
            res.node = try p.addNode(.{ .tag = .IntLiteral, .type = res.ty, .data = undefined });
            if (!p.inMacro) try p.valueMap.put(res.node, res.value);
            return res;
        },

        .One => {
            p.tokenIdx += 1;
            var res: Result = .{ .value = Value.int(1) };
            res.node = try p.addNode(.{ .tag = .IntLiteral, .type = res.ty, .data = undefined });
            if (!p.inMacro) try p.valueMap.put(res.node, res.value);
            return res;
        },

        .IntegerLiteral,
        .IntegerLiteral_U,
        .IntegerLiteral_L,
        .IntegerLiteral_LU,
        .IntegerLiteral_LL,
        .IntegerLiteral_LLU,
        => return p.parseIntegerLiteral(),

        .KeywordGeneric => return p.parseGenericSelection(),

        else => return Result{},
    }
}

fn makePredefinedIdentifier(p: *Parser) !Result {
    const slice = p.strings.items;

    const elemType = .{ .specifier = .Char, .qual = .{ .@"const" = true } };
    const arrType = try p.arena.create(Type.Array);
    arrType.* = .{ .elem = elemType, .len = slice.len };
    const ty: Type = .{ .specifier = .Array, .data = .{ .array = arrType } };

    const val = Value.bytes(try p.arena.dupe(u8, slice));
    const strLit = try p.addNode(.{ .tag = .StringLiteralExpr, .type = ty, .data = undefined });
    if (!p.inMacro) try p.valueMap.put(strLit, val);
    return Result{
        .ty = ty,
        .node = try p.addNode(.{
            .tag = .ImplicitStaticVar,
            .type = ty,
            .data = .{
                .decl = .{
                    .name = p.tokenIdx,
                    .node = strLit,
                },
            },
        }),
    };
}

fn parseFloat(p: *Parser, tok: TokenIndex, comptime T: type) Error!T {
    var bytes = p.getTokenSlice(tok);
    switch (p.tokenIds[tok]) {
        .FloatLiteral => {},
        .ImaginaryLiteral, .FloatLiteral_F, .FloatLiteral_L => bytes = bytes[0 .. bytes.len - 1],
        .ImaginaryLiteral_F, .ImaginaryLiteral_L => bytes = bytes[0 .. bytes.len - 2],
        else => unreachable,
    }

    return std.fmt.parseFloat(T, bytes) catch |e| switch (e) {
        error.InvalidCharacter => unreachable, // validated by Tokenizer
    };
}

fn castInt(p: *Parser, val: u64, specs: []const Type.Specifier) Error!Result {
    var res: Result = .{ .value = Value.int(val) };
    for (specs) |spec| {
        const ty = Type{ .specifier = spec };
        const isUnsigned = ty.isUnsignedInt(p.pp.comp);
        const tySize = ty.sizeof(p.pp.comp).?;
        res.ty = ty;

        if (isUnsigned) {
            switch (tySize) {
                2 => if (val <= std.math.maxInt(u16)) break,
                4 => if (val <= std.math.maxInt(u32)) break,
                8 => if (val <= std.math.maxInt(u64)) break,
                else => unreachable,
            }
        } else {
            switch (tySize) {
                2 => if (val <= std.math.maxInt(i16)) break,
                4 => if (val <= std.math.maxInt(i32)) break,
                8 => if (val <= std.math.maxInt(i64)) break,
                else => unreachable,
            }
        }
    } else {
        res.ty = .{ .specifier = .ULongLong };
    }

    res.node = try p.addNode(.{ .tag = .IntLiteral, .type = res.ty, .data = .{ .int = val } });
    if (!p.inMacro)
        try p.valueMap.put(res.node, res.value);
    return res;
}

/// Run a parser function but do not evaluate the result
fn parseNoEval(p: *Parser, comptime func: fn (*Parser) Error!Result) Error!Result {
    const noEval = p.noEval;
    defer p.noEval = noEval;

    p.noEval = true;
    const parsed = try func(p);
    try parsed.expect(p);
    return parsed;
}

//// genericSelection :
//// `_Generic` '(' assign-expression ',' generic-association (',' generic-association)* ')'
//// generic-association
////  : type-name ':' assign-expression
////  | `default` ':' assign-expression
fn parseGenericSelection(p: *Parser) Error!Result {
    p.tokenIdx += 1;
    const lp = try p.expectToken(.LParen);
    const controlling = try p.parseNoEval(parseAssignExpr);
    _ = try p.expectToken(.Comma);

    const listBufferTop = p.listBuffer.items.len;
    defer p.listBuffer.items.len = listBufferTop;

    try p.listBuffer.append(controlling.node);

    var defaultToken: ?TokenIndex = null;
    var chosen: Result = .{};

    while (true) {
        const start = p.tokenIdx;
        if (try p.parseTypeName()) |ty| {
            if (ty.containAnyQual()) {
                try p.errToken(.generic_qual_type, start);
            }

            _ = try p.expectToken(.Colon);
            chosen = try p.parseAssignExpr();
            try chosen.expect(p);
            try chosen.saveValue(p);

            try p.listBuffer.append(try p.addNode(.{
                .tag = .GenericAssociationExpr,
                .type = ty,
                .data = .{ .unExpr = chosen.node },
            }));
        } else if (p.eat(.KeywordDefault)) |tok| {
            if (defaultToken) |prev| {
                try p.errToken(.generic_duplicate_default, tok);
                try p.errToken(.previous_case, prev);
            }

            defaultToken = tok;
            _ = try p.expectToken(.Colon);
            chosen = try p.parseAssignExpr();
            try chosen.expect(p);
            try chosen.saveValue(p);

            try p.listBuffer.append(try p.addNode(.{
                .tag = .GenericDefaultExpr,
                .data = .{ .unExpr = chosen.node },
            }));
        } else {
            if (p.listBuffer.items.len == listBufferTop + 1) {
                try p.err(.expected_type);
                return error.ParsingFailed;
            }
            break;
        }

        if (p.eat(.Comma) == null)
            break;
    }

    try p.expectClosing(lp, .RParen);
    var genericNode: AST.Node = .{
        .tag = .GenericExprOne,
        .type = chosen.ty,
        .data = .{ .binExpr = .{ .lhs = controlling.node, .rhs = chosen.node } },
    };

    const associations = p.listBuffer.items[listBufferTop..];
    if (associations.len > 2) { // associations[0] == controlling.node
        genericNode.tag = .GenericExpr;
        genericNode.data = .{ .range = try p.addList(associations) };
    }

    chosen.node = try p.addNode(genericNode);
    return chosen;
}

fn parseIntegerLiteral(p: *Parser) Error!Result {
    const curToken = p.getCurrToken();
    var slice = p.getTokenSlice(p.tokenIdx);

    defer p.tokenIdx += 1;

    var base: u8 = 10;
    if (std.ascii.startsWithIgnoreCase(slice, "0x")) {
        slice = slice[2..];
        base = 16;
    } else if (std.ascii.startsWithIgnoreCase(slice, "0b")) {
        try p.err(.binary_integer_literal);
        slice = slice[2..];
        base = 2;
    } else if (slice[0] == '0') {
        base = 8;
    }

    switch (curToken) {
        .IntegerLiteral_U, .IntegerLiteral_L => slice = slice[0 .. slice.len - 1],
        .IntegerLiteral_LU, .IntegerLiteral_LL => slice = slice[0 .. slice.len - 2],
        .IntegerLiteral_LLU => slice = slice[0 .. slice.len - 3],
        else => {},
    }

    var value: u64 = 0;
    var overflow = false;
    for (slice) |ch| {
        const digit: u64 = switch (ch) {
            '0'...'9' => ch - '0',
            'A'...'Z' => ch - 'A' + 10,
            'a'...'z' => ch - 'a' + 10,
            else => unreachable,
        };

        if (value != 0) {
            const mulOV = @mulWithOverflow(value, base);
            if (mulOV[1] != 0)
                overflow = true;

            value = mulOV[0];
        }

        const sum, const overflowed = @addWithOverflow(value, digit);
        if (overflowed != 0)
            overflow = true;
        value = sum;
    }

    if (overflow) {
        try p.err(.int_literal_too_big);
        var res: Result = .{ .ty = .{ .specifier = .ULongLong }, .value = Value.int(value) };
        res.node = try p.addNode(.{ .tag = .IntLiteral, .type = res.ty, .data = undefined });
        if (!p.inMacro)
            try p.valueMap.put(res.node, res.value);
        return res;
    }

    switch (curToken) {
        .IntegerLiteral,
        .IntegerLiteral_L,
        .IntegerLiteral_LL,
        => {
            if (value > std.math.maxInt(i64))
                try p.err(.implicitly_unsigned_literal);
        },
        else => {},
    }

    if (base == 10) {
        switch (curToken) {
            .IntegerLiteral => return p.castInt(value, &.{ .Int, .Long, .LongLong }),
            .IntegerLiteral_U => return p.castInt(value, &.{ .UInt, .ULong, .ULongLong }),
            .IntegerLiteral_L => return p.castInt(value, &.{ .Long, .LongLong }),
            .IntegerLiteral_LU => return p.castInt(value, &.{ .ULong, .ULongLong }),
            .IntegerLiteral_LL => return p.castInt(value, &.{.LongLong}),
            .IntegerLiteral_LLU => return p.castInt(value, &.{.ULongLong}),
            else => unreachable,
        }
    } else {
        switch (curToken) {
            .IntegerLiteral => return p.castInt(value, &.{ .Int, .UInt, .Long, .ULong, .LongLong, .ULongLong }),
            .IntegerLiteral_U => return p.castInt(value, &.{ .UInt, .ULong, .ULongLong }),
            .IntegerLiteral_L => return p.castInt(value, &.{ .Long, .ULong, .LongLong, .ULongLong }),
            .IntegerLiteral_LU => return p.castInt(value, &.{ .ULong, .ULongLong }),
            .IntegerLiteral_LL => return p.castInt(value, &.{ .LongLong, .ULongLong }),
            .IntegerLiteral_LLU => return p.castInt(value, &.{.ULongLong}),
            else => unreachable,
        }
    }
}

fn parseCharLiteral(p: *Parser) Error!Result {
    defer p.tokenIdx += 1;
    const ty: Type = switch (p.getCurrToken()) {
        .CharLiteral => .{ .specifier = .Int },
        .CharLiteralWide => p.pp.comp.types.wchar,
        .CharLiteralUTF_16 => .{ .specifier = .UShort },
        .CharLiteralUTF_32 => .{ .specifier = .ULong },
        else => unreachable,
    };

    const max: u32 = switch (p.getCurrToken()) {
        .CharLiteral => std.math.maxInt(u8),
        .CharLiteralWide => std.math.maxInt(u32), // TODO correct
        .CharLiteralUTF_16 => std.math.maxInt(u16),
        .CharLiteralUTF_32 => std.math.maxInt(u32),
        else => unreachable,
    };
    var multichar: u8 = switch (p.getCurrToken()) {
        .CharLiteral => 0,
        .CharLiteralWide => 4,
        .CharLiteralUTF_16 => 2,
        .CharLiteralUTF_32 => 2,
        else => unreachable,
    };

    var val: u32 = 0;
    var overflowReported = false;
    var slice = p.getTokenSlice(p.tokenIdx);
    slice = slice[0 .. slice.len - 1];
    var i = std.mem.indexOf(u8, slice, "\'").? + 1;
    while (i < slice.len) : (i += 1) {
        var c: u32 = slice[i];
        switch (c) {
            '\\' => {
                i += 1;
                switch (slice[i]) {
                    '\n' => i += 1,
                    '\r' => i += 2,
                    '\'', '\"', '\\', '?' => c = slice[i],
                    'n' => c = '\n',
                    'r' => c = '\r',
                    't' => c = '\t',
                    'a' => c = 0x07,
                    'b' => c = 0x08,
                    'e' => {
                        try p.errExtra(.non_standard_escape_char, p.tokenIdx, .{ .unsigned = i - 1 });
                        c = 0x1B;
                    },
                    'f' => c = 0x0C,
                    'v' => c = 0x0B,
                    'x' => c = try p.parseNumberEscape(p.tokenIdx, 16, slice, &i),
                    '0'...'7' => c = try p.parseNumberEscape(p.tokenIdx, 8, slice, &i),
                    'u', 'U' => return p.todo("unicode escapes in char literals"),
                    else => unreachable,
                }
            },
            // These are safe since the source is checked to be valid utf8.
            0b1100_0000...0b1101_1111 => {
                c &= 0b00011111;
                c <<= 6;
                c |= slice[i + 1] & 0b00111111;
                i += 1;
            },
            0b1110_0000...0b1110_1111 => {
                c &= 0b00001111;
                c <<= 6;
                c |= slice[i + 1] & 0b00111111;
                c <<= 6;
                c |= slice[i + 2] & 0b00111111;
                i += 2;
            },
            0b1111_0000...0b1111_0111 => {
                c &= 0b00000111;
                c <<= 6;
                c |= slice[i + 1] & 0b00111111;
                c <<= 6;
                c |= slice[i + 2] & 0b00111111;
                c <<= 6;
                c |= slice[i + 3] & 0b00111111;
                i += 3;
            },
            else => {},
        }

        if (c > max)
            try p.err(.char_too_large);

        switch (multichar) {
            0, 2, 4 => multichar += 1,
            1 => {
                multichar = 99;
                try p.err(.multichar_literal);
            },
            3 => {
                try p.err(.unicode_multichar_literal);
                return error.ParsingFailed;
            },
            5 => {
                try p.err(.wide_multichar_literal);
                val = 0;
                multichar = 6;
            },
            6 => val = 0,
            else => {},
        }
        const mulOV = @mulWithOverflow(val, max);
        if (mulOV[1] != 0 and !overflowReported) {
            try p.errExtra(.char_lit_too_wide, p.tokenIdx, .{ .unsigned = i });
            overflowReported = true;
        }
        val = mulOV[0] + c;
    }

    const res = Result{
        .ty = ty,
        .value = Value.int(val),
        .node = try p.addNode(.{ .tag = .IntLiteral, .type = ty, .data = undefined }),
    };

    if (!p.inMacro)
        try p.valueMap.put(res.node, res.value);
    return res;
}

fn parseStringLiteral(p: *Parser) Error!Result {
    var start = p.tokenIdx;
    var width: ?u8 = null;

    while (true) {
        switch (p.getCurrToken()) {
            .StringLiteral => {},
            .StringLiteralUTF_8 => if (width) |some| {
                if (some != 8) try p.err(.unsupported_str_cat);
            } else {
                width = 8;
            },

            .StringLiteralUTF_16 => if (width) |some| {
                if (some != 16) try p.err(.unsupported_str_cat);
            } else {
                width = 16;
            },

            .StringLiteralUTF_32 => if (width) |some| {
                if (some != 32) try p.err(.unsupported_str_cat);
            } else {
                width = 32;
            },

            .StringLiteralWide => if (width) |some| {
                if (some != 1) try p.err(.unsupported_str_cat);
            } else {
                width = 1;
            },

            else => break,
        }

        p.tokenIdx += 1;
    }

    if (width == null)
        width = 8;

    if (width.? != 8)
        return p.todo("unicode string literals");

    p.strings.items.len = 0;
    while (start < p.tokenIdx) : (start += 1) {
        var slice = p.getTokenSlice(start);
        slice = slice[0 .. slice.len - 1];
        var i = std.mem.indexOf(u8, slice, "\"").? + 1;

        try p.strings.ensureUnusedCapacity(slice.len);
        while (i < slice.len) : (i += 1) {
            switch (slice[i]) {
                '\\' => {
                    i += 1;
                    switch (slice[i]) {
                        '\n' => i += 1,
                        '\r' => i += 2,
                        '\'', '\"', '\\', '?' => |c| p.strings.appendAssumeCapacity(c),
                        'n' => p.strings.appendAssumeCapacity('\n'),
                        'r' => p.strings.appendAssumeCapacity('\r'),
                        't' => p.strings.appendAssumeCapacity('\t'),
                        'a' => p.strings.appendAssumeCapacity(0x07),
                        'b' => p.strings.appendAssumeCapacity(0x08),
                        'e' => {
                            try p.errExtra(.non_standard_escape_char, start, .{ .unsigned = i - 1 });
                            p.strings.appendAssumeCapacity(0x1B);
                        },
                        'f' => p.strings.appendAssumeCapacity(0x0C),
                        'v' => p.strings.appendAssumeCapacity(0x0B),
                        'x' => p.strings.appendAssumeCapacity(try p.parseNumberEscape(start, 16, slice, &i)),
                        '0'...'7' => p.strings.appendAssumeCapacity(try p.parseNumberEscape(start, 8, slice, &i)),

                        'u' => try p.parseUnicodeEscape(start, 4, slice, &i),
                        'U' => try p.parseUnicodeEscape(start, 8, slice, &i),
                        else => unreachable,
                    }
                },
                else => |c| p.strings.appendAssumeCapacity(c),
            }
        }
    }

    try p.strings.append(0);
    const slice = p.strings.items;

    const arrayType = try p.arena.create(Type.Array);
    arrayType.* = .{ .elem = .{ .specifier = .Char }, .len = slice.len };

    var res: Result = .{
        .ty = .{
            .specifier = .Array,
            .data = .{ .array = arrayType },
        },
        .value = Value.bytes(try p.arena.dupe(u8, slice)),
    };

    res.node = try p.addNode(.{ .tag = .StringLiteralExpr, .type = res.ty, .data = undefined });

    if (!p.inMacro)
        try p.valueMap.put(res.node, res.value);

    return res;
}

fn parseNumberEscape(p: *Parser, tok: TokenIndex, base: u8, slice: []const u8, i: *usize) !u8 {
    if (base == 16) i.* += 1; // skip x
    var char: u8 = 0;
    var reported = false;

    while (i.* < slice.len) : (i.* += 1) {
        const val = std.fmt.charToDigit(slice[i.*], base) catch break; // validated by Tokenizer
        const mulOV = @mulWithOverflow(char, base);

        if (mulOV[1] != 0 and !reported) {
            try p.errExtra(.escape_sequence_overflow, tok, .{ .unsigned = i.* });
            reported = true;
        }
        char = mulOV[0] + val;
    }

    i.* -= 1;

    return char;
}

fn parseUnicodeEscape(p: *Parser, tok: TokenIndex, count: u8, slice: []const u8, i: *usize) !void {
    const c = std.fmt.parseInt(u21, slice[i.* + 1 ..][0..count], 16) catch 0x110000; // Validated by tokenizer
    i.* += count + 1;
    if (!std.unicode.utf8ValidCodepoint(c) or (c < 0xa0 and c != '$' and c != '@' and c != '`')) {
        try p.errExtra(.invalid_universal_character, tok, .{ .unsigned = i.* - count - 2 });
        return;
    }
    var buf: [4]u8 = undefined;
    const to_write = std.unicode.utf8Encode(c, &buf) catch unreachable; // validated above
    p.strings.appendSliceAssumeCapacity(buf[0..to_write]);
}
