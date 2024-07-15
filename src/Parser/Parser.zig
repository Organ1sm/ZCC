const std = @import("std");
const big = std.math.big;
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
const Symbol = SymbolStack.Symbol;
const Switch = @import("../Sema/Switch.zig");
const Result = @import("Result.zig");
const InitList = @import("InitList.zig");
const Attribute = @import("../Lexer/Attribute.zig");
const CharInfo = @import("../Basic/CharInfo.zig");
const Value = @import("../AST/Value.zig");
const StringId = @import("../Basic/StringInterner.zig").StringId;
const RecordLayout = @import("../Basic/RecordLayout.zig");
const NumberAffixes = @import("../Lexer/NumberAffixes.zig");
const NumberPrefix = NumberAffixes.Prefix;
const NumberSuffix = NumberAffixes.Suffix;

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
comp: *Compilation,
gpa: Allocator,
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
attrApplicationBuffer: std.ArrayListUnmanaged(Attribute) = .{},
fieldAttrBuffer: std.ArrayList([]const Attribute),

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
    fieldAttrStart: usize = 0,

    fn addField(r: @This(), p: *Parser, name: StringId, token: TokenIndex) Error!void {
        var i = p.recordMembers.items.len;
        while (i > r.start) {
            i -= 1;
            if (p.recordMembers.items[i].name == name) {
                try p.errStr(.duplicate_member, token, p.getTokenText(token));
                try p.errToken(.previous_definition, p.recordMembers.items[i].token);
                break;
            }
        }
        try p.recordMembers.append(p.gpa, .{ .name = name, .token = token });
    }

    fn addFieldsFromAnonymous(r: @This(), p: *Parser, ty: Type) Error!void {
        for (ty.getRecord().?.fields) |f| {
            if (f.isAnonymousRecord()) {
                try r.addFieldsFromAnonymous(p, f.ty.canonicalize(.standard));
            } else if (f.nameToken != 0) {
                try r.addField(p, f.name, f.nameToken);
            }
        }
    }
} = .{},

recordMembers: std.ArrayListUnmanaged(struct { token: TokenIndex, name: StringId }) = .{},
@"switch": ?*Switch = null,
inLoop: bool = false,
/// #pragma pack value
pragmaPack: ?u8 = null,
stringsIds: struct {
    declSpecId: StringId,
    mainId: StringId,
},

const Label = union(enum) {
    unresolvedGoto: TokenIndex,
    label: TokenIndex,
};

fn checkIdentifierCodepointWarnings(comp: *Compilation, codepoint: u21, loc: Source.Location) Compilation.Error!bool {
    std.debug.assert(codepoint >= 0x80);

    const errStart = comp.diagnostics.list.items.len;

    if (!CharInfo.isC99IdChar(codepoint)) {
        try comp.addDiagnostic(.{
            .tag = .c99_compat,
            .loc = loc,
        }, &.{});
    }
    if (CharInfo.isInvisible(codepoint)) {
        try comp.addDiagnostic(.{
            .tag = .unicode_zero_width,
            .loc = loc,
            .extra = .{ .actualCodePoint = codepoint },
        }, &.{});
    }
    if (CharInfo.homoglyph(codepoint)) |resembles| {
        try comp.addDiagnostic(.{
            .tag = .unicode_homoglyph,
            .loc = loc,
            .extra = .{ .codePoints = .{ .actual = codepoint, .resembles = resembles } },
        }, &.{});
    }
    return comp.diagnostics.list.items.len != errStart;
}

/// Issues diagnostics for the current extended identifier token
/// Return value indicates whether the token should be considered an identifier
/// true means consider the token to actually be an identifier
/// false means it is not
fn validateExtendedIdentifier(p: *Parser) !bool {
    std.debug.assert(p.getCurrToken() == .ExtendedIdentifier);

    const slice = p.getTokenText(p.tokenIdx);
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

    const standard = p.comp.langOpts.standard;
    while (it.nextCodepoint()) |codepoint| {
        defer {
            len += 1;
            loc.byteOffset += std.unicode.utf8CodepointSequenceLength(codepoint) catch unreachable;
        }

        if (codepoint == '$') {
            warned = true;
            if (p.comp.langOpts.dollarsInIdentifiers)
                try p.comp.addDiagnostic(.{
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
            warned = try checkIdentifierCodepointWarnings(p.comp, codepoint, loc);
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
    if (!p.comp.langOpts.dollarsInIdentifiers) {
        if (p.getCurrToken() == .Invalid and p.getTokenText(p.tokenIdx)[0] == '$') {
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
    if (res.ty.isUnsignedInt(p.comp))
        try p.errExtra(.overflow_unsigned, op_tok, .{ .unsigned = res.value.data.int })
    else
        try p.errExtra(.overflow_signed, op_tok, .{ .signed = res.value.signExtend(res.ty, p.comp) });
}

pub fn getTokenText(p: *Parser, index: TokenIndex) []const u8 {
    if (p.tokenIds[index].getTokenText()) |some|
        return some;

    const loc = p.pp.tokens.items(.loc)[index];
    var lexer = Lexer{
        .buffer = p.comp.getSource(loc.id).buffer,
        .comp = p.comp,
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
        else => try p.errExtra(.expected_token, p.tokenIdx, .{
            .tokenId = .{
                .expected = expected,
                .actual = actual,
            },
        }),
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
        loc.byteOffset += @intCast(p.getTokenText(index - 1).len);
    }

    try p.comp.addDiagnostic(
        .{ .tag = tag, .loc = loc, .extra = extra },
        token.expansionSlice(),
    );
}

pub fn errToken(p: *Parser, tag: Diagnostics.Tag, index: TokenIndex) Compilation.Error!void {
    @setCold(true);
    return p.errExtra(tag, index, .{ .none = {} });
}

pub fn err(p: *Parser, tag: Diagnostics.Tag) Compilation.Error!void {
    @setCold(true);
    return p.errExtra(tag, p.tokenIdx, .{ .none = {} });
}

pub fn todo(p: *Parser, msg: []const u8) Error {
    try p.errStr(.todo, p.tokenIdx, msg);
    return error.ParsingFailed;
}

pub fn typeStr(p: *Parser, ty: Type) ![]const u8 {
    if (TypeBuilder.fromType(ty).toString(p.comp.langOpts)) |str| return str;
    const stringsTop = p.strings.items.len;
    defer p.strings.items.len = stringsTop;

    const mapper = p.comp.stringInterner.getSlowTypeMapper();
    try ty.print(mapper, p.comp.langOpts, p.strings.writer());
    return try p.comp.diagnostics.arena.allocator().dupe(u8, p.strings.items[stringsTop..]);
}

pub fn typePairStr(p: *Parser, a: Type, b: Type) ![]const u8 {
    return p.typePairStrExtra(a, " and ", b);
}

pub fn typePairStrExtra(p: *Parser, a: Type, msg: []const u8, b: Type) ![]const u8 {
    const stringsTop = p.strings.items.len;
    defer p.strings.items.len = stringsTop;

    try p.strings.append('\'');
    const mapper = p.comp.stringInterner.getSlowTypeMapper();
    try a.print(mapper, p.comp.langOpts, p.strings.writer());
    try p.strings.append('\'');
    try p.strings.appendSlice(msg);
    try p.strings.append('\'');
    try b.print(mapper, p.comp.langOpts, p.strings.writer());
    try p.strings.append('\'');
    return try p.comp.diagnostics.arena.allocator().dupe(u8, p.strings.items[stringsTop..]);
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
    } else if (intTy.isUnsignedInt(p.comp)) {
        try w.print(" changes {s}value from {d} to {d}", .{ nonZeroStr, oldValue, res.value.getInt(u64) });
    } else {
        try w.print(" changes {s}value from {d} to {d}", .{ nonZeroStr, oldValue, res.value.getInt(i64) });
    }

    return try p.comp.diagnostics.arena.allocator().dupe(u8, p.strings.items[stringsTop..]);
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
    if (ty.getAttribute(.@"error")) |@"error"| {
        const stringsTop = p.strings.items.len;
        defer p.strings.items.len = stringsTop;

        const w = p.strings.writer();
        try w.print("call to '{s}' declared with attribute error: {s}", .{ p.getTokenText(@"error".__name_token), @"error".msg });
        const str = try p.comp.diagnostics.arena.allocator().dupe(u8, p.strings.items[stringsTop..]);
        try p.errStr(.error_attribute, usageToken, str);
    }

    if (ty.getAttribute(.warning)) |warning| {
        const stringsTop = p.strings.items.len;
        defer p.strings.items.len = stringsTop;

        const w = p.strings.writer();
        try w.print("call to '{s}' declared with attribute warning: {s}", .{ p.getTokenText(warning.__name_token), warning.msg });
        const str = try p.comp.diagnostics.arena.allocator().dupe(u8, p.strings.items[stringsTop..]);
        try p.errStr(.warning_attribute, usageToken, str);
    }

    // Check if the type has an 'unavailable' attribute and report it
    if (ty.getAttribute(.unavailable)) |unavailable| {
        try p.errDeprecated(.unavailable, usageToken, unavailable.msg);
        try p.errStr(.unavailable_note, unavailable.__name_token, p.getTokenText(declToken));
        return error.ParsingFailed; // Abort parsing due to 'unavailable' type
    }

    // Check if the type has a 'deprecated' attribute and report it
    else if (ty.getAttribute(.deprecated)) |deprecated| {
        try p.errDeprecated(.deprecated_declarations, usageToken, deprecated.msg);
        try p.errStr(.deprecated_note, deprecated.__name_token, p.getTokenText(declToken));
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
    try w.print("'{s}' is ", .{p.getTokenText(tokenIdx)});
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
    const str = try p.comp.diagnostics.arena.allocator().dupe(u8, p.strings.items[stringsTop..]);
    // Report the error with the constructed message.
    return p.errStr(tag, tokenIdx, str);
}

pub fn addNode(p: *Parser, node: AST.Node) Allocator.Error!NodeIndex {
    if (p.inMacro)
        return .none;

    const res = p.nodes.len;
    try p.nodes.append(p.gpa, node);

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
            .label => |l| if (std.mem.eql(u8, p.getTokenText(l), name)) return l,
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

fn getInternString(p: *Parser, tokenIdx: TokenIndex) !StringId {
    const name = p.getTokenText(tokenIdx);
    return p.comp.intern(name);
}

fn pragma(p: *Parser) Compilation.Error!bool {
    var foundPragma = false;
    while (p.eat(.KeywordPragma)) |_| {
        foundPragma = true;
        const nameToken = p.tokenIdx;
        const name = p.getTokenText(nameToken);
        const endIdx = std.mem.indexOfScalarPos(TokenType, p.tokenIds, p.tokenIdx, .NewLine).?;
        const pragmaLen = @as(TokenIndex, @intCast(endIdx)) - p.tokenIdx;
        defer p.tokenIdx += pragmaLen + 1; // skip past .nl as well

        if (p.comp.getPragma(name)) |prag| {
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
        .comp = pp.comp,
        .gpa = pp.comp.gpa,
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
        .fieldAttrBuffer = std.ArrayList([]const Attribute).init(pp.comp.gpa),
        .stringsIds = .{
            .declSpecId = try pp.comp.intern("__declspec"),
            .mainId = try pp.comp.intern("main"),
        },
    };

    errdefer {
        p.nodes.deinit(pp.comp.gpa);
        p.valueMap.deinit();
    }

    defer {
        p.data.deinit();
        p.labels.deinit();
        p.strings.deinit();
        p.symStack.deinit(pp.comp.gpa);
        p.listBuffer.deinit();
        p.declBuffer.deinit();
        p.paramBuffer.deinit();
        p.enumBuffer.deinit();
        p.recordBuffer.deinit();
        p.recordMembers.deinit(pp.comp.gpa);
        p.attrBuffer.deinit(pp.comp.gpa);
        p.attrApplicationBuffer.deinit(pp.comp.gpa);
        std.debug.assert(p.fieldAttrBuffer.items.len == 0);
        p.fieldAttrBuffer.deinit();
    }

    //bind p to the symbol stack for simplify symbol stack api
    p.symStack.p = &p;
    try p.symStack.pushScope();
    defer p.symStack.popScope();

    // NodeIndex 0 must be invalid
    _ = try p.addNode(.{ .tag = .Invalid, .type = undefined, .data = undefined });
    {
        try p.symStack.defineTypedef(try p.comp.intern("__int128_t"), Type.Int128, 0, .none);
        try p.symStack.defineTypedef(try p.comp.intern("__uint128_t"), Type.UInt128, 0, .none);

        const elemTy = try p.arena.create(Type);
        elemTy.* = Type.Char;
        try p.symStack.defineTypedef(try p.comp.intern("__builtin_ms_va_list"), .{
            .specifier = .Pointer,
            .data = .{ .subType = elemTy },
        }, 0, .none);

        const ty = &pp.comp.types.vaList;
        try p.symStack.defineTypedef(try p.comp.intern("__builtin_va_list"), ty.*, 0, .none);
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
                .KeywordC23StaticAssert,
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
    errdefer pp.comp.gpa.free(rootDecls);
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

/// external-declaration : function-definition | declaration
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
            .KeywordC23ThreadLocal,
            .KeywordInline,
            .KeywordGccInline1,
            .KeywordGccInline2,
            .KeywordNoreturn,
            .KeywordVoid,
            .KeywordBool,
            .KeywordC23Bool,
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
            .KeywordC23Alignas,
            .KeywordGccExtension,
            .KeywordTypeof,
            .KeywordTypeof1,
            .KeywordTypeof2,
            .KeywordBitInt,
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

/// declaration
///  : declaration-specifiers init-declarator-list? ';'
///  | attribute-specifier-sequence declaration-specifiers init-declarator-list ';'
///  | static-assert-declaration
///  | attribute-declaration
///
/// declaration-specifiers
///  : declaration-specifier attribute-specifier-sequence?
///  | declaration-specifier declaration-sepcifiers
///
/// init-declarator-list
///  : init-declarator (',' init-declarator)*
fn parseDeclaration(p: *Parser) Error!bool {
    _ = try p.pragma();
    const firstTokenIndex = p.tokenIdx;
    const attrBufferTop = p.attrBuffer.len;
    defer p.attrBuffer.len = attrBufferTop;

    try p.parseAttrSpec();

    var declSpec = if (try p.parseDeclSpec()) |some| some else blk: {
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
        break :blk DeclSpec{ .type = try spec.finish(p) };
    };

    if (declSpec.noreturn) |token| {
        const attr = Attribute{ .tag = .noreturn, .args = .{ .noreturn = {} }, .syntax = .keyword };
        try p.attrBuffer.append(p.gpa, .{ .attr = attr, .tok = token });
    }

    var initDeclarator = (try p.parseInitDeclarator(&declSpec, attrBufferTop)) orelse {
        _ = try p.expectToken(.Semicolon); // eat ';'
        if (declSpec.type.is(.Enum) or
            (declSpec.type.isRecord() and
            !declSpec.type.isAnonymousRecord(p.comp) and
            !declSpec.type.isTypeof()))
        {
            const specifier = declSpec.type.canonicalize(.standard).specifier;
            const attrs = p.attrBuffer.items(.attr)[attrBufferTop..];
            const toks = p.attrBuffer.items(.tok)[attrBufferTop..];
            for (attrs, toks) |attr, tok| {
                try p.errExtra(
                    .ignored_record_attr,
                    tok,
                    .{
                        .ignoredRecordAttr = .{
                            .tag = attr.tag,
                            .specifier = switch (specifier) {
                                .Enum => .@"enum",
                                .Struct => .@"struct",
                                .Union => .@"union",
                                else => unreachable,
                            },
                        },
                    },
                );
            }
            return true;
        }

        try p.errToken(.missing_declaration, firstTokenIndex);
        return true;
    };

    // check for funtion definition
    if (initDeclarator.d.funcDeclarator != null and
        initDeclarator.initializer.node == .none and
        initDeclarator.d.type.isFunc())
    fndef: {
        switch (p.getCurrToken()) {
            .Comma, .Semicolon => break :fndef,
            .LBrace => {},
            else => {
                if (initDeclarator.d.oldTypeFunc == null) {
                    try p.err(.expected_fn_body);
                    return true;
                }
            },
        }

        if (p.func.type != null)
            try p.err(.func_not_in_root);

        const node = try p.addNode(undefined); // reserve space
        const internedDeclaratorName = try p.getInternString(initDeclarator.d.name);
        try p.symStack.defineSymbol(internedDeclaratorName, initDeclarator.d.type, initDeclarator.d.name, node, .{}, false);

        const func = p.func;
        defer p.func = func;
        p.func = .{ .type = initDeclarator.d.type, .name = initDeclarator.d.name };
        if (internedDeclaratorName == p.stringsIds.mainId and
            !initDeclarator.d.type.getReturnType().is(.Int))
            try p.errToken(.main_return_type, initDeclarator.d.name);

        try p.symStack.pushScope();
        defer p.symStack.popScope();

        // collect old style parameters
        if (initDeclarator.d.oldTypeFunc != null) {
            const paramBufferTop = p.paramBuffer.items.len;
            defer p.paramBuffer.items.len = paramBufferTop;

            // ensure attributed specifier is not lost for old-style functions
            const attrs = initDeclarator.d.type.getAttributes();
            var baseTy = if (initDeclarator.d.type.specifier == .Attributed) initDeclarator.d.type.data.attributed.base else initDeclarator.d.type;
            baseTy.specifier = .Func;
            initDeclarator.d.type = try baseTy.withAttributes(p.arena, attrs);

            paramLoop: while (true) {
                const paramDeclSpec = (try p.parseDeclSpec()) orelse break;
                if (p.eat(.Semicolon)) |semi| {
                    try p.errToken(.missing_declaration, semi);
                    continue :paramLoop;
                }

                while (true) {
                    const attrBufferTopDeclarator = p.attrBuffer.len;
                    defer p.attrBuffer.len = attrBufferTopDeclarator;

                    var d = (try p.declarator(paramDeclSpec.type, .normal)) orelse {
                        try p.errToken(.missing_declaration, firstTokenIndex);
                        _ = try p.expectToken(.Semicolon);
                        continue :paramLoop;
                    };

                    try p.parseAttrSpec();

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
                    const name = p.getTokenText(d.name);
                    const internedName = try p.comp.intern(name);
                    for (initDeclarator.d.type.getParams()) |*param| {
                        if (param.name == internedName) {
                            param.ty = d.type;
                            break;
                        }
                    } else {
                        try p.errStr(.parameter_missing, d.name, name);
                    }

                    d.type = try Attribute.applyParameterAttributes(p, d.type, attrBufferTop, .alignas_on_param);

                    // bypass redefinition check to avoid duplicate errors
                    try p.symStack.define(.{
                        .kind = .definition,
                        .name = internedName,
                        .token = d.name,
                        .type = d.type,
                        .value = .{},
                    });
                    if (p.eat(.Comma) == null) break;
                }

                _ = try p.expectToken(.Semicolon);
            }
        } else {
            for (initDeclarator.d.type.getParams()) |param| {
                if (param.ty.hasUnboundVLA())
                    try p.errToken(.unbound_vla, param.nameToken);
                if (param.ty.hasIncompleteSize() and !param.ty.is(.Void))
                    try p.errStr(.parameter_incomplete_ty, param.nameToken, try p.typeStr(param.ty));
                if (param.name == .empty) {
                    try p.errToken(.omitting_parameter_name, param.nameToken);
                    continue;
                }

                // bypass redefinition check to avoid duplicate errors
                try p.symStack.define(.{
                    .kind = .definition,
                    .name = param.name,
                    .token = param.nameToken,
                    .type = param.ty,
                    .value = .{},
                });
            }
        }

        const body = (try p.parseCompoundStmt(true, null)) orelse {
            std.debug.assert(initDeclarator.d.oldTypeFunc != null);
            try p.err(.expected_fn_body);
            return true;
        };
        p.nodes.set(@intFromEnum(node), .{
            .type = initDeclarator.d.type,
            .tag = try declSpec.validateFnDef(p),
            .data = .{ .decl = .{ .name = initDeclarator.d.name, .node = body } },
        });
        try p.declBuffer.append(node);

        // check gotos
        if (func.type == null) {
            for (p.labels.items) |item| {
                if (item == .unresolvedGoto)
                    try p.errStr(.undeclared_label, item.unresolvedGoto, p.getTokenText(item.unresolvedGoto));

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
        if (initDeclarator.d.oldTypeFunc) |tokenIdx|
            try p.errToken(.invalid_old_style_params, tokenIdx);

        const tag = try declSpec.validate(p, &initDeclarator.d.type, initDeclarator.initializer.node != .none);
        const node = try p.addNode(.{
            .type = initDeclarator.d.type,
            .tag = tag,
            .data = .{ .decl = .{ .name = initDeclarator.d.name, .node = initDeclarator.initializer.node } },
        });
        try p.declBuffer.append(node);

        const internedName = try p.getInternString(initDeclarator.d.name);
        if (declSpec.storageClass == .typedef) {
            try p.symStack.defineTypedef(internedName, initDeclarator.d.type, initDeclarator.d.name, node);
        } else if (initDeclarator.initializer.node != .none or
            (declSpec.storageClass != .@"extern" and p.func.type != null))
        {
            // TODO validate global variable/constexpr initializer comptime known
            try p.symStack.defineSymbol(
                internedName,
                initDeclarator.d.type,
                initDeclarator.d.name,
                node,
                if (initDeclarator.d.type.isConst() or declSpec.constexpr != null) initDeclarator.initializer.value else .{},
                declSpec.constexpr != null,
            );
        } else {
            try p.symStack.declareSymbol(internedName, initDeclarator.d.type, initDeclarator.d.name, node);
        }

        if (p.eat(.Comma) == null)
            break;

        initDeclarator = (try p.parseInitDeclarator(&declSpec, attrBufferTop)) orelse {
            try p.err(.expected_ident_or_l_paren);
            continue;
        };
    }

    _ = try p.expectToken(.Semicolon);
    return true;
}

fn staticAssertMessage(p: *Parser, condNode: NodeIndex, message: Result) !?[]const u8 {
    const condTag = p.nodes.items(.tag)[@intFromEnum(condNode)];
    if (condTag != .BuiltinTypesCompatibleP and message.node == .none) return null;

    var buf = std.ArrayList(u8).init(p.gpa);
    defer buf.deinit();

    if (condTag == .BuiltinTypesCompatibleP) {
        const mapper = p.comp.stringInterner.getSlowTypeMapper();
        const data = p.nodes.items(.data)[@intFromEnum(condNode)].binExpr;

        try buf.appendSlice("'__builtin_types_compatible_p(");

        const lhsTy = p.nodes.items(.type)[@intFromEnum(data.lhs)];
        try lhsTy.print(mapper, p.comp.langOpts, buf.writer());
        try buf.appendSlice(", ");

        const rhsTy = p.nodes.items(.type)[@intFromEnum(data.rhs)];
        try rhsTy.print(mapper, p.comp.langOpts, buf.writer());

        try buf.appendSlice(")'");
    }

    if (message.node != .none) {
        std.debug.assert(p.nodes.items(.tag)[@intFromEnum(message.node)] == .StringLiteralExpr);

        if (buf.items.len > 0)
            try buf.append(' ');

        const data = message.value.data.bytes;
        try buf.ensureUnusedCapacity(data.len);
        try AST.dumpString(
            data,
            p.nodes.items(.tag)[@intFromEnum(message.node)],
            buf.writer(),
        );
    }
    return try p.comp.diagnostics.arena.allocator().dupe(u8, buf.items);
}

/// static-assert-declaration
///  : (`_Static_assert` | `static_assert`) '(' integer-const-expression ',' StringLiteral+ ')' ';'
fn parseStaticAssert(p: *Parser) Error!bool {
    const staticAssert = p.eat(.KeywordStaticAssert) orelse p.eat(.KeywordC23StaticAssert) orelse return false;
    const lp = try p.expectToken(.LParen);
    const resToken = p.tokenIdx;
    var res = try p.constExpr(.GNUFoldingExtension);
    const resNode = res.node;

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
    if (str.node == .none) {
        try p.errToken(.static_assert_missing_message, staticAssert);
        try p.errStr(.pre_c2x_compat, staticAssert, "'_Static_assert' with no message");
    }

    // Array will never be zero; a value of zero for a pointer is a null pointer constant
    if ((res.ty.isArray() or res.ty.isPointer()) and !res.value.isZero()) {
        const errStart = p.comp.diagnostics.list.items.len;
        try p.errToken(.const_decl_folded, resToken);
        if (res.ty.isPointer() and errStart != p.comp.diagnostics.list.items.len) {
            // Don't show the note if the .const_decl_folded diagnostic was not added
            try p.errToken(.constant_expression_conversion_not_allowed, resToken);
        }
    }

    try res.boolCast(p, Type.Bool, resToken);
    if (res.value.isUnavailable()) {
        if (!res.ty.isInvalid())
            try p.errToken(.static_assert_not_constant, resToken);
    } else {
        if (!res.value.getBool()) {
            if (try p.staticAssertMessage(resNode, str)) |message|
                try p.errStr(.static_assert_failure_message, staticAssert, message)
            else
                try p.errToken(.static_assert_failure, staticAssert);
        }
    }

    const node = try p.addNode(.{
        .tag = .StaticAssert,
        .data = .{ .binExpr = .{ .lhs = res.node, .rhs = str.node } },
    });
    try p.declBuffer.append(node);
    return true;
}

/// typeof
///   : `typeof` '(' type-name ')'
///   | `typeof` '(' expr ')'
fn typeof(p: *Parser) Error!?Type {
    switch (p.getCurrToken()) {
        .KeywordTypeof, .KeywordTypeof1, .KeywordTypeof2 => p.tokenIdx += 1,
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

        return Type{ .data = .{ .subType = typeofType }, .specifier = .TypeofType };
    }

    const typeofExpr = try p.parseNoEval(parseExpr);
    try typeofExpr.expect(p);
    try p.expectClosing(lp, .RParen);

    if (typeofExpr.ty.is(.NullPtrTy))
        return Type{ .specifier = .NullPtrTy, .qual = typeofExpr.ty.qual.inheritFromTypeof() };

    const inner = try p.arena.create(Type.Expr);
    inner.* = .{
        .node = typeofExpr.node,
        .ty = .{
            .data = typeofExpr.ty.data,
            .qual = typeofExpr.ty.qual.inheritFromTypeof(),
            .specifier = typeofExpr.ty.specifier,
            .decayed = typeofExpr.ty.decayed,
        },
    };

    return Type{
        .data = .{ .expr = inner },
        .specifier = .TypeofExpr,
        .decayed = typeofExpr.ty.decayed,
    };
}

/// declaration-specifier
///  : storage-class-specifier
///  | type-specifier-qualifier
///  | function-specifier
///
/// declaration-specifier
///  : storage-class-specifier
///  | type-specifier
///  | type-qualifier
///  | func-specifier
///  | align-specifier
///
/// storage-class-specifier:
///  | `auto`
///  | `constexpr`
///  | `extern`
///  | `register`
///  | `static`
///  | `threadlocal`
///  : `typedef`
fn parseDeclSpec(p: *Parser) Error!?DeclSpec {
    var d: DeclSpec = .{ .type = .{ .specifier = undefined } };
    var spec: TypeBuilder = .{};

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
                        .KeywordExtern, .KeywordStatic => {},
                        else => try p.errStr(.cannot_combine_spec, p.tokenIdx, token.getTokenText().?),
                    }
                    if (d.constexpr) |tok|
                        try p.errStr(.cannot_combine_spec, p.tokenIdx, p.tokenIds[tok].getTokenText().?);
                }

                if (d.constexpr != null) {
                    switch (token) {
                        .KeywordAuto, .KeywordRegister, .KeywordStatic => {},
                        else => try p.errStr(.cannot_combine_spec, p.tokenIdx, token.getTokenText().?),
                    }
                    if (d.threadLocal) |tok|
                        try p.errStr(.cannot_combine_spec, p.tokenIdx, p.tokenIds[tok].getTokenText().?);
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

            .KeywordThreadLocal, .KeywordC23ThreadLocal => {
                if (d.threadLocal != null)
                    try p.errStr(.duplicate_declspec, p.tokenIdx, token.getTokenText().?);

                if (d.constexpr) |tok|
                    try p.errStr(.cannot_combine_spec, p.tokenIdx, p.tokenIds[tok].getTokenText().?);

                switch (d.storageClass) {
                    .@"extern", .none, .static => {},
                    else => try p.errStr(.cannot_combine_spec, p.tokenIdx, @tagName(d.storageClass)),
                }

                d.threadLocal = p.tokenIdx;
            },

            .KeywordConstexpr => {
                if (d.constexpr != null)
                    try p.errStr(.duplicate_declspec, p.tokenIdx, token.getTokenText().?);

                if (d.threadLocal) |tok|
                    try p.errStr(.cannot_combine_spec, p.tokenIdx, p.tokenIds[tok].getTokenText().?);

                switch (d.storageClass) {
                    .auto, .register, .none, .static => {},
                    else => try p.errStr(.cannot_combine_spec, p.tokenIdx, @tagName(d.storageClass)),
                }

                d.constexpr = p.tokenIdx;
            },

            .KeywordInline, .KeywordGccInline1, .KeywordGccInline2 => {
                if (d.@"inline" != null)
                    try p.errStr(.duplicate_declspec, p.tokenIdx, "inline");
                d.@"inline" = p.tokenIdx;
            },

            .KeywordNoreturn => {
                if (d.noreturn != null)
                    try p.errStr(.duplicate_declspec, p.tokenIdx, "_Noreturn");
                d.noreturn = p.tokenIdx;
            },
            else => break,
        }

        p.tokenIdx += 1;
    }

    if (p.tokenIdx == start)
        return null;

    d.type = try spec.finish(p);
    return d;
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

    const name = p.getTokenText(nameToken);
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
                    if (Attribute.diagnoseIdent(attr, &arguments, p.getTokenText(ident))) |msg| {
                        try p.errExtra(msg.tag, ident, msg.extra);
                        p.skipTo(.RParen);
                        return error.ParsingFailed;
                    }
                } else {
                    try p.errExtra(.attribute_requires_identifier, nameToken, .{ .str = name });
                    return error.ParsingFailed;
                }
            } else {
                // handle first argument
                try p.handleAttrParam(attr, &arguments, argIdx);
            }

            argIdx += 1;
            while (p.eat(.RParen) == null) : (argIdx += 1) {
                // handle next argument
                _ = try p.expectToken(.Comma);
                try p.handleAttrParam(attr, &arguments, argIdx);
            }
        },
        else => {},
    }

    if (argIdx < requiredCount) {
        try p.errExtra(.attribute_not_enough_args, nameToken, .{
            .attrArgCount = .{ .attribute = attr, .expected = requiredCount },
        });
        return error.ParsingFailed;
    }
    return TentativeAttribute{ .attr = .{ .tag = attr, .args = arguments, .syntax = kind.toSyntax() }, .tok = nameToken };
}

fn handleAttrParam(p: *Parser, attr: Attribute.Tag, arguments: *Attribute.Arguments, argIdx: u32) Error!void {
    const argStart = p.tokenIdx;
    var argExpr = try p.parseAssignExpr();
    try argExpr.expect(p);
    if (p.diagnose(attr, arguments, argIdx, argExpr)) |msg| {
        try p.errExtra(msg.tag, argStart, msg.extra);
        p.skipTo(.RParen);
        return error.ParsingFailed;
    }
}

fn diagnose(p: *Parser, attr: Attribute.Tag, arguments: *Attribute.Arguments, argIdx: u32, res: Result) ?Diagnostics.Message {
    if (Attribute.wantsAlignment(attr, argIdx))
        return Attribute.diagnoseAlignment(attr, arguments, argIdx, res.value, res.ty, p.comp);

    const node = p.nodes.get(@intFromEnum(res.node));
    return Attribute.diagnose(attr, arguments, argIdx, res.value, node);
}

fn handleAttr(p: *Parser, format: Attribute.Kind, namespace: ?[]const u8) Error!void {
    if (try p.attribute(format, namespace)) |attr|
        try p.attrBuffer.append(p.gpa, attr);
}

/// attribute-list : (attribute (',' attribute)*)?
fn parseGNUAttrList(p: *Parser) Error!void {
    if (p.getCurrToken() == .RParen)
        return;

    try p.handleAttr(.gnu, null);
    while (p.getCurrToken() != .RParen) {
        _ = try p.expectToken(.Comma);
        try p.handleAttr(.gnu, null);
    }
}

fn parseC23AttrList(p: *Parser) Error!void {
    while (p.getCurrToken() != .RBracket) { // ']'
        const namespaceTok = try p.expectIdentifier();
        var namespace: ?[]const u8 = null;
        if (p.eat(.ColonColon)) |_| // '::'
            namespace = p.getTokenText(namespaceTok)
        else
            p.tokenIdx -= 1;

        try p.handleAttr(.c23, namespace);
        _ = p.eat(.Comma);
    }
}

fn parseMSVCAttrList(p: *Parser) Error!void {
    while (p.getCurrToken() != .RParen) {
        try p.handleAttr(.declspec, null);
        _ = p.eat(.Comma); // ','
    }
}

/// '[[' c23-attribute-list  ']]'
fn c23Attribute(p: *Parser) !bool {
    if (!p.comp.langOpts.standard.atLeast(.c2x)) return false;
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
    _ = p.eat(.KeywordDeclSpec) orelse return false;
    const lparen = try p.expectToken(.LParen);
    try p.parseMSVCAttrList();
    _ = try p.expectClosing(lparen, .RParen);

    return true;
}

/// (__attribute | __attribute__) '((' gnu-attribute-list  '))'
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

fn parseAttrSpec(p: *Parser) Error!void {
    try p.attributeSpecifier(null);
}

/// attribute-specifier : (KW-attribute '( '(' attribute-list ')' ')')*
fn attributeSpecifier(p: *Parser, declaratorName: ?TokenIndex) Error!void {
    while (true) {
        if (try p.gnuAttribute()) continue;
        if (try p.c23Attribute()) continue;

        const maybeDeclspecToken = p.tokenIdx;
        const attrBufferTop = p.attrBuffer.len;

        if (try p.msvcAttribute()) {
            if (declaratorName) |nameToken| {
                try p.errToken(.declspec_not_allowed_after_declarator, maybeDeclspecToken);
                try p.errToken(.declarator_name_tok, nameToken);
                p.attrBuffer.len = attrBufferTop;
            }
            continue;
        }
        break;
    }
}

const InitDeclarator = struct { d: Declarator, initializer: Result = .{} };

/// init-declarator : declarator assembly? attribute-specifier? ('=' initializer)?
fn parseInitDeclarator(p: *Parser, declSpec: *DeclSpec, attrBufferTop: usize) Error!?InitDeclarator {
    const thisAttrBufferTop = p.attrBuffer.len;
    defer p.attrBuffer.len = thisAttrBufferTop;

    var ID = InitDeclarator{ .d = (try p.declarator(declSpec.type, .normal)) orelse return null };

    try p.attributeSpecifier(ID.d.name);
    _ = try p.parseAssembly(.declLable);
    try p.attributeSpecifier(ID.d.name);

    if (declSpec.storageClass == .typedef) {
        ID.d.type = try Attribute.applyTypeAttributes(p, ID.d.type, attrBufferTop, null);
    } else if (ID.d.type.isFunc()) {
        ID.d.type = try Attribute.applyFunctionAttributes(p, ID.d.type, attrBufferTop);
    } else {
        ID.d.type = try Attribute.applyVariableAttributes(p, ID.d.type, attrBufferTop, null);
    }

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

        const internedName = try p.getInternString(ID.d.name);
        try p.symStack.declareSymbol(internedName, ID.d.type, ID.d.name, .none);

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
        if (p.func.type == null and specifier == .IncompleteArray) {
            try p.errStr(.tentative_array, name, try p.typeStr(ID.d.type));
            return ID;
        }
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
///  | `keyword-c23-Bool`
///  | `_Complex`
///  | `_BitInt`  '(' integer-const-expression ')'
///  | atomic-type-specifier
///  | record-specifier
///  | enum-sepcifier
///  | typedef-name
///  | typeof-specifier
///
/// atomic-type-specifier
///   : keyword-atomic '(' typeName ')'
///
/// align-specifier
///   : keyword-alignas '(' typeName ')'
///   | keyword-alignas '(' integer-const-expression ')'
///   | keyword-c23-alignas '(' typeName ')'
///   | keyword-c23-alignas '(' integer-const-expression ')'
fn parseTypeSpec(p: *Parser, ty: *TypeBuilder) Error!bool {
    const start = p.tokenIdx;
    while (true) {
        try p.parseAttrSpec();

        if (try p.typeof()) |innerType| {
            try ty.combineFromTypeof(p, innerType, start);
            continue;
        }

        if (try p.parseTypeQual(&ty.qual))
            continue;

        switch (p.getCurrToken()) {
            .KeywordVoid => try ty.combine(p, .Void, p.tokenIdx),
            .KeywordBool, .KeywordC23Bool => try ty.combine(p, .Bool, p.tokenIdx),
            .KeywordMSInt8_, .KeywordMSInt8__, .KeywordChar => try ty.combine(p, .Char, p.tokenIdx),
            .KeywordMSInt16_, .KeywordMSInt16__, .KeywordShort => try ty.combine(p, .Short, p.tokenIdx),
            .KeywordMSInt32_, .KeywordMSInt32__, .KeywordInt => try ty.combine(p, .Int, p.tokenIdx),
            .KeywordLong => try ty.combine(p, .Long, p.tokenIdx),
            .KeywordMSInt64_, .KeywordMSInt64__ => try ty.combine(p, .LongLong, p.tokenIdx),
            .KeywordInt128 => try ty.combine(p, .Int128, p.tokenIdx),
            .KeywordSigned => try ty.combine(p, .Signed, p.tokenIdx),
            .KeywordUnsigned => try ty.combine(p, .Unsigned, p.tokenIdx),
            .KeywordFp16 => try ty.combine(p, .FP16, p.tokenIdx),
            .KeywordFloat => try ty.combine(p, .Float, p.tokenIdx),
            .KeywordDouble => try ty.combine(p, .Double, p.tokenIdx),
            .KeywordComplex => try ty.combine(p, .Complex, p.tokenIdx),
            .KeywordFloat80 => try ty.combine(p, .Float80, p.tokenIdx),
            .KeywordFloat128 => try ty.combine(p, .Float128, p.tokenIdx),

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

            .KeywordAlignas, .KeywordC23Alignas => {
                const alignToken = p.tokenIdx;
                p.tokenIdx += 1;
                const lparen = try p.expectToken(.LParen);
                const typenameStart = p.tokenIdx;
                if (try p.parseTypeName()) |innerTy| {
                    if (!innerTy.alignable())
                        try p.errStr(.invalid_alignof, typenameStart, try p.typeStr(innerTy));

                    const alignment = Attribute.Alignment{ .requested = innerTy.alignof(p.comp) };
                    try p.attrBuffer.append(p.gpa, .{
                        .attr = .{
                            .tag = .aligned,
                            .args = .{ .aligned = .{ .alignment = alignment, .__name_token = alignToken } },
                            .syntax = .keyword,
                        },
                        .tok = alignToken,
                    });
                } else {
                    const argStart = p.tokenIdx;
                    const res = try p.parseIntegerConstExpr(.NoConstDeclFolding);
                    if (!res.value.isZero()) {
                        var args = Attribute.initArguments(.aligned, alignToken);
                        if (p.diagnose(.aligned, &args, 0, res)) |msg| {
                            try p.errExtra(msg.tag, argStart, msg.extra);
                            p.skipTo(.RParen);
                            return error.ParsingFailed;
                        }
                        args.aligned.alignment.?.node = res.node;
                        try p.attrBuffer.append(p.gpa, .{
                            .attr = .{ .tag = .aligned, .args = args, .syntax = .keyword },
                            .tok = alignToken,
                        });
                    }
                }
                try p.expectClosing(lparen, .RParen);
                continue;
            },

            .KeywordStdCall,
            .KeywordStdCall2,
            .KeywordThisCall,
            .KeywordThisCall2,
            .KeywordVectorCall,
            .KeywordVectorCall2,
            => try p.attrBuffer.append(p.gpa, .{
                .attr = .{
                    .tag = .calling_convention,
                    .args = .{
                        .calling_convention = .{
                            .cc = switch (p.getCurrToken()) {
                                .KeywordStdCall, .KeywordStdCall2 => .stdcall,
                                .KeywordThisCall, .KeywordThisCall2 => .thiscall,
                                .KeywordVectorCall, .KeywordVectorCall2 => .vectorcall,
                                else => unreachable,
                            },
                        },
                    },
                    .syntax = .keyword,
                },
                .tok = p.tokenIdx,
            }),

            .KeywordEnum => {
                const tagToken = p.tokenIdx;
                try ty.combine(p, .{ .Enum = try p.parseEnumSpec() }, tagToken);
                continue;
            },

            .KeywordStruct, .KeywordUnion => {
                const tagToken = p.tokenIdx;
                const recordTy = try p.parseRecordSpec();
                try ty.combine(p, TypeBuilder.fromType(recordTy), tagToken);
                continue;
            },

            .Identifier, .ExtendedIdentifier => {
                var internedName = try p.getInternString(p.tokenIdx);
                var declspecFound = false;
                if (internedName == p.stringsIds.declSpecId) {
                    try p.errToken(.declspec_not_enabled, p.tokenIdx);
                    p.tokenIdx += 1;

                    if (p.eat(.LParen)) |_| {
                        p.skipTo(.RParen);
                        continue;
                    }
                    declspecFound = true;
                }

                if (ty.typedef != null)
                    break;

                if (declspecFound)
                    internedName = try p.getInternString(p.tokenIdx);

                const typedef = (try p.symStack.findTypedef(internedName, p.tokenIdx, ty.specifier != .None)) orelse break;
                if (!ty.combineTypedef(p, typedef.type, typedef.token))
                    break;
            },

            .KeywordBitInt => {
                try p.err(.bit_int);
                const bitIntToken = p.tokenIdx;
                p.tokenIdx += 1;

                const lparen = try p.expectToken(.LParen);
                const res = try p.parseIntegerConstExpr(.GNUFoldingExtension);
                try p.expectClosing(lparen, .RParen);

                var bits: i16 = undefined;
                if (res.value.isUnavailable()) {
                    try p.errToken(.expected_integer_constant_expr, bitIntToken);
                    return error.ParsingFailed;
                } else if (res.value.compare(.lte, Value.int(0), res.ty, p.comp)) {
                    bits = -1;
                } else if (res.value.compare(.gt, Value.int(128), res.ty, p.comp)) {
                    bits = 129;
                } else {
                    bits = res.value.getInt(i16);
                }

                try ty.combine(p, .{ .BitInt = bits }, bitIntToken);
                continue;
            },
            else => break,
        }

        if (try p.eatIdentifier()) |_| {} else p.tokenIdx += 1;
    }

    return p.tokenIdx != start;
}

fn getAnonymousName(p: *Parser, kindToken: TokenIndex) !StringId {
    const loc = p.pp.tokens.items(.loc)[kindToken];
    const source = p.comp.getSource(loc.id);
    const lineAndCol = source.getLineCol(loc);

    const kindStr = switch (p.tokenIds[kindToken]) {
        .KeywordStruct, .KeywordUnion, .KeywordEnum => p.getTokenText(kindToken),
        else => "record field",
    };

    const str = try std.fmt.allocPrint(
        p.arena,
        "(anonymous {s} at {s}:{d}:{d})",
        .{ kindStr, source.path, lineAndCol.lineNO, lineAndCol.col },
    );
    return p.comp.intern(str);
}

/// record-specifier
///  : StructOrUnion identifier? { record-declaration-list }
///  | StructOrUnion identifier
/// record-declaration-list
///  : record-declaration+
/// StructOrUnion
///  : 'struct'
///  | 'union'
fn parseRecordSpec(p: *Parser) Error!Type {
    const startingPragmaPack = p.pragmaPack;
    const kindToken = p.tokenIdx;
    const isStruct = p.tokenIds[kindToken] == .KeywordStruct;
    p.tokenIdx += 1;

    const attrBufferTop = p.attrBuffer.len;
    defer p.attrBuffer.len = attrBufferTop;

    try p.parseAttrSpec();

    const maybeIdent = try p.eatIdentifier();
    const lb = p.eat(.LBrace) orelse {
        const ident = maybeIdent orelse {
            try p.err(.ident_or_l_brace);
            return error.ParsingFailed;
        };

        // check if this is a reference to a previous type
        const internedName = try p.getInternString(ident);
        if (try p.symStack.findTag(internedName, p.tokenIds[kindToken], ident, p.getCurrToken())) |prev| {
            return prev.type;
        } else {
            // this is a forward declaration, create a new record type.
            const recordType = try Type.Record.create(p.arena, internedName);
            var ty = Type{
                .specifier = if (isStruct) .Struct else .Union,
                .data = .{ .record = recordType },
            };
            ty = try Attribute.applyTypeAttributes(p, ty, attrBufferTop, null);

            try p.symStack.define(.{
                .kind = if (isStruct) .@"struct" else .@"union",
                .name = internedName,
                .token = ident,
                .type = ty,
                .value = .{},
            });

            const node = try p.addNode(.{
                .tag = if (isStruct) .StructForwardDecl else .UnionForwardDecl,
                .type = ty,
                .data = undefined,
            });
            try p.declBuffer.append(node);
            return ty;
        }
    };

    var done = false;
    errdefer if (!done) p.skipTo(.RBrace);

    // Get forward declared type or create a new one
    var defined = false;
    const recordType: *Type.Record = if (maybeIdent) |ident| recordTy: {
        const identStr = p.getTokenText(ident);
        const internedName = try p.comp.intern(identStr);
        if (try p.symStack.defineTag(internedName, p.tokenIds[kindToken], ident)) |prev| {
            if (!prev.type.hasIncompleteSize()) {
                // if the record isn't incomplete, this is a redefinition
                try p.errStr(.redefinition, ident, identStr);
                try p.errToken(.previous_definition, prev.token);
            } else {
                defined = true;
                break :recordTy prev.type.get(if (isStruct) .Struct else .Union).?.data.record;
            }
        }
        break :recordTy try Type.Record.create(p.arena, internedName);
    } else try Type.Record.create(p.arena, try p.getAnonymousName(kindToken));

    // Initially create ty as a regular non-attributed type, since attributes for a record
    // can be specified after the closing rbrace, which we haven't encountered yet.
    var ty = Type{
        .specifier = if (isStruct) .Struct else .Union,
        .data = .{ .record = recordType },
    };

    // declare a symbol for the type
    if (maybeIdent != null and !defined) {
        try p.symStack.define(.{
            .kind = if (isStruct) .@"struct" else .@"union",
            .name = recordType.name,
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
    const oldFieldAttrStart = p.fieldAttrBuffer.items.len;

    errdefer p.declBuffer.items.len = declBufferTop - 1;

    defer {
        p.declBuffer.items.len = declBufferTop;
        p.recordBuffer.items.len = recordBufferTop;
        p.record = oldRecord;
        p.recordMembers.items.len = oldMembers;
        p.fieldAttrBuffer.items.len = oldFieldAttrStart;
    }

    p.record = .{
        .kind = p.tokenIds[kindToken],
        .start = p.recordMembers.items.len,
        .fieldAttrStart = p.fieldAttrBuffer.items.len,
    };

    try p.parseRecordDecls();

    if (p.record.flexibleField) |some| {
        if (p.recordBuffer.items[recordBufferTop..].len == 1 and isStruct)
            try p.errToken(.flexible_in_empty, some);
    }

    for (p.recordBuffer.items[recordBufferTop..]) |field| {
        if (field.ty.hasIncompleteSize() and !field.ty.is(.IncompleteArray))
            break;
    } else {
        recordType.fields = try p.arena.dupe(Type.Record.Field, p.recordBuffer.items[recordBufferTop..]);
    }

    if (oldFieldAttrStart < p.fieldAttrBuffer.items.len) {
        const fieldAttrSlice = p.fieldAttrBuffer.items[oldFieldAttrStart..];
        const duped = try p.arena.dupe([]const Attribute, fieldAttrSlice);
        recordType.fieldAttributes = duped.ptr;
    }

    if (p.recordBuffer.items.len == recordBufferTop) {
        try p.errStr(.empty_record, kindToken, p.getTokenText(kindToken));
        try p.errStr(.empty_record_size, kindToken, p.getTokenText(kindToken));
    }

    try p.expectClosing(lb, .RBrace);
    done = true;
    try p.parseAttrSpec();

    const t = Type{
        .specifier = if (isStruct) .Struct else .Union,
        .data = .{ .record = recordType },
    };
    ty = try Attribute.applyTypeAttributes(p, t, attrBufferTop, null);

    if (ty.specifier == .Attributed and maybeIdent != null) {
        const identStr = p.getTokenText(maybeIdent.?);
        const internedName = try p.comp.intern(identStr);
        const ptr = p.symStack.getPtr(internedName, .tags);
        ptr.type = ty;
    }

    if (!ty.hasIncompleteSize()) {
        const pragmaPackValue = switch (p.comp.langOpts.emulate) {
            .clang => startingPragmaPack,
            .gcc => p.pragmaPack,
            .msvc => p.pragmaPack,
        };
        RecordLayout.compute(recordType, ty, p.comp, pragmaPackValue);
    }

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
    return ty;
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

/// record-declarator : declarator (':' integer-constant-expression)?
fn parseRecordDeclarator(p: *Parser) Error!bool {
    const attrBufferTop = p.attrBuffer.len;
    defer p.attrBuffer.len = attrBufferTop;

    const baseType = (try p.parseSpecQuals()) orelse return false;
    try p.parseAttrSpec(); // .record

    while (true) {
        const thisDeclTop = p.attrBuffer.len;
        defer p.attrBuffer.len = thisDeclTop;

        try p.parseAttrSpec();
        // 0 means unnamed
        var nameToken: TokenIndex = 0;
        var ty = baseType;
        var bitsNode: NodeIndex = .none;
        var bits: ?u32 = null;
        const firstToken = p.tokenIdx;
        if (try p.declarator(ty, .record)) |d| {
            nameToken = d.name;
            ty = d.type;
        }

        if (p.eat(.Colon)) |_| bits: {
            const bitsToken = p.tokenIdx;
            const res = try p.parseIntegerConstExpr(.GNUFoldingExtension);
            if (!ty.isInt()) {
                try p.errStr(.non_int_bitfield, firstToken, try p.typeStr(ty));
                break :bits;
            }

            if (res.value.isUnavailable()) {
                try p.errToken(.expected_integer_constant_expr, bitsToken);
                break :bits;
            } else if (res.value.compare(.lt, Value.int(0), res.ty, p.comp)) {
                try p.errExtra(.negative_bitwidth, firstToken, .{
                    .signed = res.value.signExtend(res.ty, p.comp),
                });
                break :bits;
            }

            // incomplete size error is reported later
            const bitSize = ty.bitSizeof(p.comp) orelse break :bits;
            if (res.value.compare(.gt, Value.int(bitSize), res.ty, p.comp)) {
                try p.errToken(.bitfield_too_big, nameToken);
                break :bits;
            } else if (res.value.isZero() and nameToken != 0) {
                try p.errToken(.zero_width_named_field, nameToken);
                break :bits;
            }

            bits = res.value.getInt(u32);
            bitsNode = res.node;
        }

        try p.parseAttrSpec(); // .record
        const toAppend = try Attribute.applyFieldAttributes(p, &ty, attrBufferTop);
        errdefer p.arena.free(toAppend);

        const anyFieldsHaveAttrs = p.fieldAttrBuffer.items.len > p.record.fieldAttrStart;
        if (anyFieldsHaveAttrs) {
            try p.fieldAttrBuffer.append(toAppend);
        } else {
            if (toAppend.len > 0) {
                const preceding = p.recordMembers.items.len - p.record.start;
                if (preceding > 0)
                    try p.fieldAttrBuffer.appendNTimes(&.{}, preceding);
                try p.fieldAttrBuffer.append(toAppend);
            }
        }

        if (nameToken == 0 and bitsNode == .none) unnamed: {
            // don't allow incompelete size fields in anonymous record.
            if (ty.is(.Enum) or ty.hasIncompleteSize()) break :unnamed;
            if (ty.isAnonymousRecord(p.comp)) {
                // An anonymous record appears as indirect fields on the parent
                try p.recordBuffer.append(.{
                    .name = try p.getAnonymousName(firstToken),
                    .ty = ty,
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
            const internedName = if (nameToken != 0) try p.getInternString(nameToken) else try p.getAnonymousName(firstToken);
            try p.recordBuffer.append(.{
                .name = internedName,
                .ty = ty,
                .nameToken = nameToken,
                .bitWidth = bits,
            });

            if (nameToken != 0)
                try p.record.addField(p, internedName, nameToken);

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
            if (p.record.flexibleField) |some| {
                if (p.record.kind == .KeywordStruct)
                    try p.errToken(.flexible_non_final, some);
            }
            p.record.flexibleField = firstToken;
        } else if (ty.hasIncompleteSize()) {
            try p.errStr(.field_incomplete_ty, firstToken, try p.typeStr(ty));
        } else if (p.record.flexibleField) |some| {
            if (some != firstToken and p.record.kind == .KeywordStruct)
                try p.errToken(.flexible_non_final, some);
        }
        if (p.eat(.Comma) == null) break;
    }
    _ = try p.expectToken(.Semicolon);
    return true;
}

// specifier-qualifier-list : (type-specifier | type-qualifier | align-specifier)+
fn parseSpecQuals(p: *Parser) Error!?Type {
    var spec: TypeBuilder = .{};
    if (try p.parseTypeSpec(&spec))
        return try spec.finish(p);
    return null;
}

fn checkEnumFixedTy(p: *Parser, fixedTy: ?Type, identToken: TokenIndex, prev: Symbol) !void {
    const enumTy = prev.type.get(.Enum).?.data.@"enum";
    if (fixedTy) |some| {
        if (!enumTy.fixed) {
            try p.errToken(.enum_prev_nonfixed, identToken);
            try p.errToken(.previous_definition, prev.token);
            return error.ParsingFailed;
        }

        if (!enumTy.tagType.eql(some, p.comp, false)) {
            const str = try p.typePairStrExtra(some, " (was ", enumTy.tagType);
            try p.errStr(.enum_different_explicit_ty, identToken, str);
            try p.errToken(.previous_definition, prev.token);
            return error.ParsingFailed;
        }
    } else if (enumTy.fixed) {
        try p.errToken(.enum_prev_fixed, identToken);
        try p.errToken(.previous_definition, prev.token);
        return error.ParsingFailed;
    }
}

/// enum-specifier
///  : `enum` identifier? (':', type-name)? { enumerator (',' enumerator)? ',') }
///  | `enum` identifier (':', type-name)?
fn parseEnumSpec(p: *Parser) Error!*Type.Enum {
    const enumTK = p.tokenIdx;
    p.tokenIdx += 1;

    const attrBufferTop = p.attrBuffer.len;
    defer p.attrBuffer.len = attrBufferTop;

    try p.parseAttrSpec();

    const maybeID = try p.eatIdentifier();
    const fixedTy = if (p.eat(.Colon)) |colon| fixed: {
        const fixed = (try p.parseTypeName()) orelse {
            if (p.record.kind != .Invalid) {
                // This is a bit field.
                p.tokenIdx -= 1;
                break :fixed null;
            }
            try p.err(.expected_type);
            try p.errToken(.enum_fixed, colon);
            break :fixed null;
        };
        try p.errToken(.enum_fixed, colon);
        break :fixed fixed;
    } else null;

    const lb = p.eat(.LBrace) orelse {
        const ident = maybeID orelse {
            try p.err(.ident_or_l_brace);
            return error.ParsingFailed;
        };

        // check if this is a reference to a previous type
        const internedName = try p.getInternString(ident);
        if (try p.symStack.findTag(internedName, .KeywordEnum, ident, p.getCurrToken())) |prev| {
            try p.checkEnumFixedTy(fixedTy, ident, prev);
            return prev.type.get(.Enum).?.data.@"enum";
        } else {
            // this is a forward declaration, create a new enum type
            const enumType = try Type.Enum.create(p.arena, internedName, fixedTy);
            var ty = Type{ .specifier = .Enum, .data = .{ .@"enum" = enumType } };
            ty = try Attribute.applyTypeAttributes(p, ty, attrBufferTop, null);

            try p.symStack.define(.{
                .kind = .@"enum",
                .name = internedName,
                .token = ident,
                .type = ty,
                .value = .{},
            });

            const node = try p.addNode(.{
                .tag = .EnumForwardDecl,
                .type = ty,
                .data = undefined,
            });
            try p.declBuffer.append(node);
            return enumType;
        }
    };

    var done = false;
    errdefer if (!done) p.skipTo(.RBrace);

    // Get forward declared type or create a new one
    var defined = false;
    const enumType: *Type.Enum = if (maybeID) |ident| enumTy: {
        const identStr = p.getTokenText(ident);
        const internedName = try p.comp.intern(identStr);
        if (try p.symStack.defineTag(internedName, .KeywordEnum, ident)) |prev| {
            const enumTy = prev.type.get(.Enum).?.data.@"enum";
            if (!enumTy.fixed and !enumTy.isIncomplete()) {
                // if the enum isn't incomplete, this is a redefinition
                try p.errStr(.redefinition, ident, identStr);
                try p.errToken(.previous_definition, prev.token);
            } else {
                try p.checkEnumFixedTy(fixedTy, ident, prev);
                defined = true;
                break :enumTy enumTy;
            }
        }
        break :enumTy try Type.Enum.create(p.arena, internedName, fixedTy);
    } else try Type.Enum.create(p.arena, try p.getAnonymousName(enumTK), fixedTy);

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

    var e = Enumerator.init(fixedTy);
    while (try p.enumerator(&e)) |fieldAndNode| {
        try p.enumBuffer.append(fieldAndNode.field);
        try p.listBuffer.append(fieldAndNode.node);
        if (p.eat(.Comma) == null) break;
    }

    if (p.enumBuffer.items.len == enumBufferTop)
        try p.err(.empty_enum);

    try p.expectClosing(lb, .RBrace);
    done = true;
    try p.parseAttrSpec();

    var ty = Type{
        .specifier = .Enum,
        .data = .{ .@"enum" = enumType },
    };
    ty = try Attribute.applyTypeAttributes(p, ty, attrBufferTop, null);

    if (!enumType.fixed) {
        const tagSpecifier = try e.getTypeSpecifier(p, ty.enumIsPacked(p.comp), maybeID orelse enumTK);
        enumType.tagType = .{ .specifier = tagSpecifier };
    }

    const enumFields = p.enumBuffer.items[enumBufferTop..];
    const fieldNodes = p.listBuffer.items[listBufferTop..];

    if (fixedTy == null) {
        for (enumFields, 0..) |*field, i| {
            if (field.ty.eql(Type.Int, p.comp, false)) continue;

            const sym = p.symStack.get(field.name, .vars) orelse continue;
            var res = Result{ .node = field.node, .ty = field.ty, .value = sym.value };
            const destTy = if (p.comp.fixedEnumTagSpecifier()) |some|
                Type{ .specifier = some }
            else if (res.intFitsInType(p, Type.Int))
                Type.Int
            else if (!res.ty.eql(enumType.tagType, p.comp, false))
                enumType.tagType
            else
                continue;

            const symbol = p.symStack.getPtr(field.name, .vars);
            symbol.value.intCast(field.ty, destTy, p.comp);
            symbol.type = destTy;
            p.nodes.items(.type)[@intFromEnum(fieldNodes[i])] = destTy;
            field.ty = destTy;
            res.ty = destTy;

            if (res.node != .none) {
                try res.implicitCast(p, .IntCast);
                field.node = res.node;
                p.nodes.items(.data)[@intFromEnum(fieldNodes[i])].decl.node = res.node;
            }
        }
    }

    enumType.fields = try p.arena.dupe(Type.Enum.Field, enumFields);
    // declare a symbol for the type
    if (maybeID != null and !defined) {
        try p.symStack.define(.{
            .kind = .@"enum",
            .name = enumType.name,
            .type = ty,
            .token = maybeID.?,
            .value = .{},
        });
    }
    // finish by creating a node
    var node: AST.Node = .{
        .tag = .EnumDeclTwo,
        .type = ty,
        .data = .{ .binExpr = .{ .lhs = .none, .rhs = .none } },
    };

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
    // tracks the minimum number of bits required to represent the positive part of the enumeration constant
    numPositiveBits: usize = 0,
    // tracks the minimum number of bits required to represent the negative part of the enumeration constant
    numNegativeBits: usize = 0,
    // whether indicate fixed type
    fixed: bool,

    fn init(fixedTy: ?Type) Enumerator {
        return .{
            .res = .{ .ty = fixedTy orelse Type.Int, .value = .{ .tag = .unavailable } },
            .fixed = (fixedTy != null),
        };
    }

    /// Increment enumerator value adjusting type if needed.
    fn incr(e: *Enumerator, p: *Parser, token: TokenIndex) !void {
        e.res.node = .none;
        const oldVal = e.res.value;
        if (oldVal.isUnavailable()) {
            // First enumerator, set to 0 fits in all types.
            e.res.value = Value.int(0);
            return;
        }

        if (e.res.value.add(e.res.value, Value.int(1), e.res.ty, p.comp)) {
            const byteSize = e.res.ty.sizeof(p.comp).?;
            const bitSize: u8 = @intCast(if (e.res.ty.isUnsignedInt(p.comp)) byteSize * 8 else byteSize * 8 - 1);

            if (e.fixed) {
                try p.errStr(.enum_not_representable_fixed, token, try p.typeStr(e.res.ty));
                return;
            }

            const newTy = if (p.comp.nextLargestIntSameSign(e.res.ty)) |larger| blk: {
                try p.errToken(.enumerator_overflow, token);
                break :blk larger;
            } else blk: {
                try p.errExtra(.enum_not_representable, token, .{ .pow2AsString = bitSize });
                break :blk Type.ULongLong;
            };
            e.res.ty = newTy;
            _ = e.res.value.add(oldVal, Value.int(1), e.res.ty, p.comp);
        }
    }

    /// Set enumerator value to specified value.
    fn set(e: *Enumerator, p: *Parser, res: Result, token: TokenIndex) !void {
        if (res.ty.isInvalid()) return;
        if (e.fixed and !res.ty.eql(e.res.ty, p.comp, false)) {
            if (!res.intFitsInType(p, e.res.ty)) {
                try p.errStr(.enum_not_representable_fixed, token, try p.typeStr(e.res.ty));
                return error.ParsingFailed;
            }
            var copy = res;
            copy.ty = e.res.ty;
            try copy.implicitCast(p, .IntCast);
            e.res = copy;
        } else {
            e.res = res;
            try e.res.intCast(p, e.res.ty.integerPromotion(p.comp), token);
        }
    }

    fn getTypeSpecifier(e: *const Enumerator, p: *Parser, isPacked: bool, token: TokenIndex) !Type.Specifier {
        if (p.comp.fixedEnumTagSpecifier()) |tagSpecifier|
            return tagSpecifier;

        const charWidth = Type.SChar.sizeof(p.comp).? * 8;
        const shortWidth = Type.Short.sizeof(p.comp).? * 8;
        const intWidth = Type.Int.sizeof(p.comp).? * 8;
        if (e.numNegativeBits > 0) {
            if (isPacked and e.numNegativeBits <= charWidth and e.numPositiveBits < charWidth) {
                return .SChar;
            } else if (isPacked and e.numNegativeBits <= shortWidth and e.numPositiveBits < shortWidth) {
                return .Short;
            } else if (e.numNegativeBits <= intWidth and e.numPositiveBits < intWidth) {
                return .Int;
            }

            const longWidth = Type.Long.sizeof(p.comp).? * 8;
            if (e.numNegativeBits <= longWidth and e.numPositiveBits < longWidth)
                return .Long;

            const llongWidth = Type.LongLong.sizeof(p.comp).? * 8;
            if (e.numNegativeBits > llongWidth or e.numPositiveBits >= llongWidth)
                try p.errToken(.enum_too_large, token);
            return .LongLong;
        }

        if (isPacked and e.numPositiveBits <= charWidth) {
            return .UChar;
        } else if (isPacked and e.numPositiveBits <= shortWidth) {
            return .UShort;
        } else if (e.numPositiveBits <= intWidth) {
            return .UInt;
        } else if (e.numPositiveBits <= Type.Long.sizeof(p.comp).? * 8) {
            return .ULong;
        }
        return .ULongLong;
    }
};

const EnumFieldAndNode = struct { field: Type.Enum.Field, node: NodeIndex };

/// enumerator : identifier ('=' integer-constant-expression)
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

    const errStart = p.comp.diagnostics.list.items.len;
    if (p.eat(.Equal)) |_| {
        const specified = try p.parseIntegerConstExpr(.GNUFoldingExtension);
        if (specified.value.isUnavailable()) {
            try p.errToken(.enum_val_unavailable, nameToken + 2);
            try e.incr(p, nameToken);
        } else {
            try e.set(p, specified, nameToken);
        }
    } else {
        try e.incr(p, nameToken);
    }

    var res = e.res;
    res.ty = try Attribute.applyEnumeratorAttributes(p, res.ty, attrBufferTop);

    if (res.ty.isUnsignedInt(p.comp) or res.value.compare(.gte, Value.int(0), res.ty, p.comp))
        e.numPositiveBits = @max(e.numPositiveBits, res.value.minUnsignedBits(res.ty, p.comp))
    else
        e.numNegativeBits = @max(e.numNegativeBits, res.value.minSignedBits(res.ty, p.comp));

    if (errStart == p.comp.diagnostics.list.items.len) {
        // only do these warnings if we didn't already warn about overflow or non-representable values
        if (e.res.value.compare(.lt, Value.int(0), e.res.ty, p.comp)) {
            const value = e.res.value.getInt(i64);
            if (value < Type.Int.minInt(p.comp))
                try p.errExtra(.enumerator_too_small, nameToken, .{ .signed = value });
        } else {
            const value = e.res.value.getInt(u64);
            if (value > Type.Int.maxInt(p.comp))
                try p.errExtra(.enumerator_too_large, nameToken, .{ .unsigned = value });
        }
    }

    const internedName = try p.getInternString(nameToken);
    try p.symStack.defineEnumeration(internedName, res.ty, nameToken, e.res.value);
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
            .name = internedName,
            .ty = res.ty,
            .nameToken = nameToken,
            .node = res.node,
        },
        .node = node,
    };
}

/// atomic-type-specifier : keyword_atomic '(' type-name ')'
/// type-qualifier
/// : keyword-const
/// | keyword-restrict
/// | keyword-volatile
/// | keyword-atomic
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
                if (p.lookAhead(1) == .LParen) break;
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

/// A Declarator represents the information extracted when parsing a declarator.
const Declarator = struct {
    /// the name of the entity.
    name: TokenIndex,
    /// the parsed Type of the declarator.
    type: Type,
    /// This is used to retain additional information needed for function declarations.
    funcDeclarator: ?TokenIndex = null,
    /// for c89 old style function
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

    const maybeIdent = p.tokenIdx;
    if (kind != .abstract and (try p.eatIdentifier()) != null) {
        d.name = maybeIdent;
        const combineToken = p.tokenIdx;
        d.type = try p.directDeclarator(d.type, &d, kind);
        try d.type.validateCombinedType(p, combineToken);
        return d;
    } else if (p.eat(.LParen)) |lp| blk: {
        var res = (try p.declarator(Type.Void, kind)) orelse {
            p.tokenIdx = lp;
            break :blk;
        };

        try p.expectClosing(lp, .RParen);
        const suffixStart = p.tokenIdx;
        const outer = try p.directDeclarator(d.type, &d, kind);

        try res.type.combine(outer);
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
    if (p.eat(.LBracket)) |lb| {
        if (p.getCurrToken() == .LBracket) {
            switch (kind) {
                .normal, .record => if (p.comp.langOpts.standard.atLeast(.c2x)) {
                    p.tokenIdx -= 1;
                    return baseType;
                },
                .param, .abstract => {},
            }
            try p.err(.expected_expr);
            return error.ParsingFailed;
        }

        var resType = Type.Pointer;
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
        var maxBits = p.comp.target.ptrBitWidth();
        if (maxBits > 61) maxBits = 61;

        // `outer` is validated later so it may be invalid here
        const outerSize = outer.sizeof(p.comp);
        const maxBytes = (@as(u64, 1) << @as(u6, @truncate(maxBits))) - 1;
        const maxElems = maxBytes / @max(1, outerSize orelse 1);

        if (!size.ty.isInt()) {
            try p.errStr(.array_size_non_int, sizeToken, try p.typeStr(size.ty));
            return error.ParsingFailed;
        }

        if (size.value.isUnavailable()) {
            if (size.node != .none) {
                try p.errToken(.vla, sizeToken);
                if (p.func.type == null and kind != .param and p.record.kind == .Invalid)
                    try p.errToken(.variable_len_array_file_scope, d.name);

                const exprType = try p.arena.create(Type.Expr);
                exprType.ty = Type.Void;
                exprType.node = size.node;
                resType.data = .{ .expr = exprType };
                resType.specifier = .VariableLenArray;

                if (static) |some|
                    try p.errToken(.useless_static, some);
            } else if (star) |_| {
                const elemType = try p.arena.create(Type);
                elemType.* = Type.Void;
                resType.data = .{ .subType = elemType };
                resType.specifier = .UnspecifiedVariableLenArray;
            } else {
                const arrayType = try p.arena.create(Type.Array);
                arrayType.elem = Type.Void;
                arrayType.len = 0;
                resType.data = .{ .array = arrayType };
                resType.specifier = .IncompleteArray;
            }
        } else {
            var sizeValue = size.value;
            const sizeTy = p.comp.types.size;

            if (sizeValue.isZero()) {
                try p.errToken(.zero_length_array, lb);
            } else if (sizeValue.compare(.lt, Value.int(0), sizeTy, p.comp)) {
                try p.errToken(.negative_array_size, lb);
                return error.ParsingFailed;
            }

            const arrayType = try p.arena.create(Type.Array);
            arrayType.elem = Type.Void;
            if (sizeValue.compare(.gt, Value.int(maxElems), sizeTy, p.comp)) {
                try p.errToken(.array_too_large, lb);
                arrayType.len = maxElems;
            } else {
                arrayType.len = sizeValue.getInt(u64);
            }
            resType.data = .{ .array = arrayType };
            resType.specifier = .Array;
        }

        try resType.combine(outer);
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
            try resType.combine(outer);
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
                const internedName = try p.getInternString(nameToken);
                try p.symStack.defineParam(internedName, undefined, nameToken);
                try p.paramBuffer.append(.{
                    .name = internedName,
                    .nameToken = nameToken,
                    .ty = Type.Int,
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
        try resType.combine(outer);
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
        const attrBufferTop = p.attrBuffer.len;
        defer p.attrBuffer.len = attrBufferTop;

        const paramDeclSpec = if (try p.parseDeclSpec()) |some|
            some
        else if (p.paramBuffer.items.len == paramBufferTop)
            return null
        else blk: {
            var spec: TypeBuilder = .{};
            break :blk DeclSpec{ .type = try spec.finish(p) };
        };

        var nameToken: TokenIndex = 0;
        const firstToken = p.tokenIdx;
        var paramType = paramDeclSpec.type;
        if (try p.declarator(paramDeclSpec.type, .param)) |some| {
            if (some.oldTypeFunc) |tokenIdx|
                try p.errToken(.invalid_old_style_params, tokenIdx);

            try p.parseAttrSpec();

            nameToken = some.name;
            paramType = some.type;
            if (some.name != 0) {
                const internedName = try p.getInternString(nameToken);
                try p.symStack.defineParam(internedName, paramType, nameToken);
            }
        }

        paramType = try Attribute.applyParameterAttributes(p, paramType, attrBufferTop, .alignas_on_param);

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
            .name = if (nameToken == 0) .empty else try p.getInternString(nameToken),
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
    const attrBufferTop = p.attrBuffer.len;
    defer p.attrBuffer.len = attrBufferTop;

    const ty = (try p.parseSpecQuals()) orelse return null;
    if (try p.declarator(ty, .abstract)) |some| {
        if (some.oldTypeFunc) |tokenIdx|
            try p.errToken(.invalid_old_style_params, tokenIdx);
        return try Attribute.applyTypeAttributes(p, some.type, attrBufferTop, .align_ignored);
    }
    return try Attribute.applyTypeAttributes(p, ty, attrBufferTop, .align_ignored);
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
    defer il.deinit(p.gpa);

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
///  : '[' integer-constant-expression ']'
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

    const isScalar = initType.isScalar();
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
    var indexHint: ?u64 = null;
    while (true) : (count += 1) {
        errdefer p.skipTo(.RBrace);

        var firstToken = p.tokenIdx;
        var curType = initType;
        var curIL = il;
        var designation = false;
        var curIndexHint: ?u64 = null;
        while (true) {
            if (p.eat(.LBracket)) |lbr| {
                if (!curType.isArray()) {
                    try p.errStr(.invalid_array_designator, lbr, try p.typeStr(curType));
                    return error.ParsingFailed;
                }

                const exprToken = p.tokenIdx;
                const indexRes = try p.parseIntegerConstExpr(.GNUFoldingExtension);
                try p.expectClosing(lbr, .RBracket);

                if (indexRes.value.isUnavailable()) {
                    try p.errToken(.expected_integer_constant_expr, exprToken);
                    return error.ParsingFailed;
                } else if (indexRes.value.compare(.lt, indexRes.value.zero(), indexRes.ty, p.comp)) {
                    try p.errExtra(.negative_array_designator, lb + 1, .{
                        .signed = indexRes.value.signExtend(indexRes.ty, p.comp),
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

                curIL = try curIL.find(p.gpa, checked);
                curType = curType.getElemType();
                designation = true;
            } else if (p.eat(.Period)) |period| {
                const fieldToken = try p.expectIdentifier();
                const fieldStr = p.getTokenText(fieldToken);
                const fieldName = try p.comp.intern(fieldStr);
                curType = curType.canonicalize(.standard);
                if (!curType.isRecord()) {
                    try p.errStr(.invalid_field_designator, period, try p.typeStr(curType));
                    return error.ParsingFailed;
                } else if (!curType.hasField(fieldName)) {
                    try p.errStr(.no_such_field_designator, period, fieldStr);
                    return error.ParsingFailed;
                }

                // TODO check if union already has field set
                outer: while (true) {
                    for (curType.data.record.fields, 0..) |f, i| {
                        if (f.isAnonymousRecord()) {
                            // Recurse into anonymous field if it has a field by the name.
                            if (!f.ty.hasField(fieldName)) continue;
                            curType = f.ty.canonicalize(.standard);
                            curIL = try il.find(p.gpa, i);
                            curIndexHint = curIndexHint orelse i;
                            continue :outer;
                        }

                        if (fieldName == f.name) {
                            curIL = try curIL.find(p.gpa, i);
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

        if (!designation and curType.hasAttribute(.designated_init))
            try p.err(.designated_init_needed);

        var saw = false;

        if (isStrInit and p.isStringInit(initType)) {
            var tempIL = InitList{};
            defer tempIL.deinit(p.gpa);
            saw = try p.initializerItem(&tempIL, Type.Void);
        } else if (count == 0 and p.isStringInit(initType)) {
            saw = try p.initializerItem(il, initType);
        } else if (isScalar and count != 0) {
            // discard further scalars
            var tempIL = InitList{};
            defer tempIL.deinit(p.gpa);
            saw = try p.initializerItem(&tempIL, Type.Void);
        } else if (p.getCurrToken() == .LBrace) {
            if (designation) {
                // designation overrides previous value, let existing mechanism handle it
                saw = try p.initializerItem(curIL, curType);
            } else if (try p.findAggregateInitializer(&curIL, &curType, &indexHint)) {
                saw = try p.initializerItem(curIL, curType);
            } else {
                // discard further values
                var tempIL = InitList{};
                defer tempIL.deinit(p.gpa);

                saw = try p.initializerItem(curIL, curType);
                saw = try p.initializerItem(&tempIL, Type.Void);
                if (!warnedExcess)
                    try p.errToken(if (initType.isArray()) .excess_array_init else .excess_struct_init, firstToken);
                warnedExcess = true;
            }
        } else sigleItems: {
            firstToken = p.tokenIdx;
            var res = try p.parseAssignExpr();
            saw = !res.empty(p);
            if (!saw) break :sigleItems;

            excess: {
                if (indexHint) |*hint| {
                    if (try p.findScalarInitializerAt(&curIL, &curType, res.ty, firstToken, hint)) break :excess;
                } else if (try p.findScalarInitializer(&curIL, &curType, res.ty, firstToken)) break :excess;

                if (designation) break :excess;
                if (!warnedExcess) try p.errToken(if (initType.isArray()) .excess_array_init else .excess_struct_init, firstToken);
                warnedExcess = true;

                break :sigleItems;
            }

            const arr = try p.coerceArrayInit(&res, firstToken, curType);
            if (!arr) try p.coerceInit(&res, firstToken, curType);
            if (curIL.tok != 0) {
                try p.errToken(.initializer_overrides, firstToken);
                try p.errToken(.previous_initializer, curIL.tok);
            }
            curIL.node = res.node;
            curIL.tok = firstToken;
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
fn findScalarInitializerAt(
    p: *Parser,
    il: **InitList,
    ty: *Type,
    actualTy: Type,
    firstToken: TokenIndex,
    startIdx: *u64,
) Error!bool {
    if (ty.isArray()) {
        if (il.*.node != .none)
            return false;
        startIdx.* += 1;

        const arrType = ty.*;
        const elemCount = arrType.arrayLen() orelse std.math.maxInt(u64);
        if (elemCount == 0) {
            try p.errToken(.empty_aggregate_init_braces, firstToken);
            return error.ParsingFailed;
        }

        const elemType = arrType.getElemType();
        const arrIL = il.*;
        if (startIdx.* < elemCount) {
            ty.* = elemType;
            il.* = try arrIL.find(p.gpa, startIdx.*);
            _ = try p.findScalarInitializer(il, ty, actualTy, firstToken);
            return true;
        }
        return false;
    } else if (ty.get(.Struct)) |structType| {
        if (il.*.node != .none)
            return false;
        startIdx.* += 1;
        const fields = structType.data.record.fields;
        if (fields.len == 0) {
            try p.errToken(.empty_aggregate_init_braces, firstToken);
            return error.ParsingFailed;
        }

        const structIL = il.*;
        if (startIdx.* < fields.len) {
            const field = fields[@intCast(startIdx.*)];
            ty.* = field.ty;
            il.* = try structIL.find(p.gpa, startIdx.*);
            _ = try p.findScalarInitializer(il, ty, actualTy, firstToken);
            return true;
        }
        return false;
    } else if (ty.get(.Union)) |_| {
        return false;
    }
    return il.*.node == .none;
}

/// Returns true if the value is unused.
fn findScalarInitializer(
    p: *Parser,
    il: **InitList,
    ty: *Type,
    actualTy: Type,
    firstToken: TokenIndex,
) Error!bool {
    if (ty.isArray()) {
        if (il.*.node != .none) return false;
        const startIdx = il.*.list.items.len;
        var index = if (startIdx != 0) il.*.list.items[startIdx - 1].index else startIdx;

        const arrayType = ty.*;
        const elemCount = arrayType.arrayLen() orelse std.math.maxInt(u64);
        if (elemCount == 0) {
            try p.errToken(.empty_aggregate_init_braces, firstToken);
            return error.ParsingFailed;
        }

        const elemTy = arrayType.getElemType();
        const arrayIL = il.*;
        while (index < elemCount) : (index += 1) {
            ty.* = elemTy;
            il.* = try arrayIL.find(p.gpa, index);
            if (il.*.node == .none and actualTy.eql(elemTy, p.comp, false))
                return true;
            if (try p.findScalarInitializer(il, ty, actualTy, firstToken))
                return true;
        }
        return false;
    } else if (ty.get(.Struct)) |structType| {
        if (il.*.node != .none) return false;
        if (actualTy.eql(ty.*, p.pp.comp, false)) return true;

        const startIdx = il.*.list.items.len;
        var index = if (startIdx != 0) il.*.list.items[startIdx - 1].index + 1 else startIdx;

        const fields = structType.data.record.fields;
        if (fields.len == 0) {
            try p.errToken(.empty_aggregate_init_braces, firstToken);
            return error.ParsingFailed;
        }
        const structIL = il.*;
        while (index < fields.len) : (index += 1) {
            const field = fields[@intCast(index)];
            ty.* = field.ty;
            il.* = try structIL.find(p.gpa, index);
            if (il.*.node == .none and actualTy.eql(field.ty, p.comp, false))
                return true;
            if (try p.findScalarInitializer(il, ty, actualTy, firstToken))
                return true;
        }
        return false;
    } else if (ty.get(.Union)) |unionType| {
        if (il.*.node != .none)
            return false;
        if (actualTy.eql(ty.*, p.pp.comp, false))
            return true;
        if (unionType.data.record.fields.len == 0) {
            try p.errToken(.empty_aggregate_init_braces, firstToken);
            return error.ParsingFailed;
        }
        ty.* = unionType.data.record.fields[0].ty;
        il.* = try il.*.find(p.gpa, 0);
        if (try p.findScalarInitializer(il, ty, actualTy, firstToken))
            return true;
        return false;
    }
    return il.*.node == .none;
}

fn findAggregateInitializer(p: *Parser, il: **InitList, ty: *Type, startIdx: *?u64) Error!bool {
    if (ty.isArray()) {
        if (il.*.node != .none)
            return false;
        const listIdx = il.*.list.items.len;
        const index = if (startIdx.*) |*some| blk: {
            some.* += 1;
            break :blk some.*;
        } else if (listIdx != 0)
            il.*.list.items[listIdx - 1].index + 1
        else
            listIdx;

        const arrType = ty.*;
        const elemCount = arrType.arrayLen() orelse std.math.maxInt(u64);
        const elemType = arrType.getElemType();
        if (index < elemCount) {
            ty.* = elemType;
            il.* = try il.*.find(p.gpa, index);
            return true;
        }
        return false;
    } else if (ty.get(.Struct)) |structType| {
        if (il.*.node != .none)
            return false;
        const listIdx = il.*.list.items.len;
        const index = if (startIdx.*) |*some| blk: {
            some.* += 1;
            break :blk some.*;
        } else if (listIdx != 0)
            il.*.list.items[listIdx - 1].index + 1
        else
            listIdx;

        const fieldCount = structType.data.record.fields.len;
        if (index < fieldCount) {
            ty.* = structType.data.record.fields[@intCast(index)].ty;
            il.* = try il.*.find(p.gpa, index);
            return true;
        }
        return false;
    } else if (ty.get(.Union)) |unionType| {
        if (il.*.node != .none)
            return false;
        if (startIdx.*) |_| return false; // overrides
        if (unionType.data.record.fields.len == 0)
            return false;
        ty.* = unionType.data.record.fields[0].ty;
        il.* = try il.*.find(p.gpa, 0);
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
    if (!isStrLiteral and !p.nodeIs(item.node, .CompoundLiteralExpr) or !item.ty.isArray()) {
        try p.errToken(.array_init_str, token);
        return true; // do not do further coercion
    }

    const targetSpec = target.getElemType().canonicalize(.standard).specifier;
    const itemSpec = item.ty.getElemType().canonicalize(.standard).specifier;
    const compatible = target.getElemType().eql(item.ty.getElemType(), p.comp, false) or
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

    try item.lvalConversion(p);
    try item.coerce(p, target, token, .init);
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
    if (initType.isScalar()) {
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

        if (il.node != .none)
            return il.node;

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
            const index: u32 = @truncate(init.index);
            const fieldType = unionType.data.record.fields[index].ty;
            unionInitNode.data.unionInit = .{
                .fieldIndex = index,
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
            const args = Attribute.Arguments{ .asm_label = .{ .name = str[0 .. str.len - 1] } };
            const attr = Attribute{ .tag = .asm_label, .args = args, .syntax = .keyword };
            try p.attrBuffer.append(p.gpa, .{ .attr = attr, .tok = asmToken });
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
    const errStart = p.comp.diagnostics.list.items.len;

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
        nullNode.type = try Attribute.applyStatementAttributes(p, nullNode.type, exprStart, attrBufferTop);
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
    try cond.usualUnaryConversion(p, condToken);

    if (!cond.ty.isScalar())
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
    var errStart = p.comp.diagnostics.list.items.len;
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
        try cond.usualUnaryConversion(p, condToken);

        if (!cond.ty.isScalar())
            try p.errStr(.statement_scalar, lp + 1, try p.typeStr(cond.ty));
    }
    try cond.saveValue(p);
    _ = try p.expectToken(.Semicolon);

    // increment
    const incrStart = p.tokenIdx;
    errStart = p.comp.diagnostics.list.items.len;
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
        return try p.addNode(.{ .tag = .ForEverStmt, .data = .{ .unExpr = body } });
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
    try cond.usualUnaryConversion(p, condToken);

    if (!cond.ty.isScalar())
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
    try cond.usualUnaryConversion(p, condToken);

    if (!cond.ty.isScalar())
        try p.errStr(.statement_scalar, lp + 1, try p.typeStr(cond.ty));

    try cond.saveValue(p);
    try p.expectClosing(lp, .RParen);

    _ = try p.expectToken(.Semicolon);
    return try p.addNode(.{ .tag = .WhileStmt, .data = .{ .binExpr = .{ .rhs = cond.node, .lhs = body } } });
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
            const elemType = try p.arena.create(Type);
            elemType.* = .{ .specifier = .Void, .qual = .{ .@"const" = true } };
            const resultType = Type{
                .specifier = .Pointer,
                .data = .{ .subType = elemType },
            };

            if (!e.ty.isInt()) {
                try p.errStr(.incompatible_arg, expr, try p.typePairStrExtra(e.ty, " to parameter of incompatible type ", resultType));
                return error.ParsingFailed;
            }

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
    const str = p.getTokenText(nameToken);
    if (p.findLabel(str) == null) {
        try p.labels.append(.{ .unresolvedGoto = nameToken });
    }
    _ = try p.expectToken(.Semicolon);
    return try p.addNode(.{ .tag = .GotoStmt, .data = .{ .declRef = nameToken } });
}

/// switch-statement : `switch` '(' expression ')' statement
fn parseSwitchStmt(p: *Parser) Error!NodeIndex {
    const lp = try p.expectToken(.LParen);
    const condToken = p.tokenIdx;
    var cond = try p.parseExpr();

    try cond.expect(p);
    try cond.lvalConversion(p);
    try cond.usualUnaryConversion(p, condToken);

    if (!cond.ty.isInt())
        try p.errStr(.statement_int, lp + 1, try p.typeStr(cond.ty));

    try cond.saveValue(p);
    try p.expectClosing(lp, .RParen);

    const oldSwitch = p.@"switch";
    var @"switch" = Switch{
        .ranges = std.ArrayList(Switch.Range).init(p.gpa),
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

/// case-statement : `case` integer-constant-expression ':'
fn parseCaseStmt(p: *Parser, caseToken: u32) Error!?NodeIndex {
    const firstItem = try p.parseIntegerConstExpr(.GNUFoldingExtension);
    const ellipsis = p.tokenIdx; // `...`
    const secondItem = if (p.eat(.Ellipsis) != null) blk: {
        try p.errToken(.gnu_switch_range, ellipsis);
        break :blk try p.parseIntegerConstExpr(.GNUFoldingExtension);
    } else null;

    _ = try p.expectToken(.Colon);

    if (p.@"switch") |some| check: {
        if (some.type.hasIncompleteSize()) // error already reported for incomplete size
            break :check;

        const first = firstItem.value;
        const last = if (secondItem) |second| second.value else first;
        if (first.isUnavailable()) {
            try p.errToken(.case_val_unavailable, caseToken + 1);
            break :check;
        } else if (last.isUnavailable()) {
            try p.errToken(.case_val_unavailable, ellipsis + 1);
            break :check;
        } else if (last.compare(.lt, first, some.type, p.comp)) {
            try p.errToken(.empty_case_range, caseToken + 1);
            break :check;
        }

        // TODO cast to target type
        const prev = (try some.add(p.comp, first, last, caseToken + 1)) orelse break :check;
        // TODO: check which value was already handled
        if (some.type.isUnsignedInt(p.comp)) {
            try p.errExtra(.duplicate_switch_case_unsigned, caseToken + 1, .{
                .unsigned = first.data.int,
            });
        } else {
            const signed: i64 = first.signExtend(some.type, p.comp);
            try p.errExtra(.duplicate_switch_case_signed, caseToken + 1, .{ .signed = signed });
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
        .data = .{ .binExpr = .{ .lhs = firstItem.node, .rhs = s } },
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
        const str = p.getTokenText(nameToken);
        if (p.findLabel(str)) |some| {
            try p.errStr(.duplicate_label, nameToken, str);
            try p.errStr(.previous_label, some, str);
        } else {
            p.labelCount += 1;
            try p.labels.append(.{ .label = nameToken });

            var i: usize = 0;
            while (i < p.labels.items.len) {
                if (p.labels.items[i] == .unresolvedGoto and
                    std.mem.eql(u8, p.getTokenText(p.labels.items[i].unresolvedGoto), str))
                    _ = p.labels.swapRemove(i)
                else
                    i += 1;
            }
        }

        p.tokenIdx += 1;

        const attrBufferTop = p.attrBuffer.len;
        defer p.attrBuffer.len = attrBufferTop;

        try p.parseAttrSpec();

        var labeledStmt = AST.Node{
            .tag = .LabeledStmt,
            .data = .{ .decl = .{ .name = nameToken, .node = try p.parseStmt() } },
        };
        labeledStmt.type = try Attribute.applyLabelAttributes(p, labeledStmt.type, attrBufferTop);
        return try p.addNode(labeledStmt);
    } else if (p.eat(.KeywordCase)) |case| {
        return p.parseCaseStmt(case);
    } else if (p.eat(.KeywordDefault)) |default| {
        return p.parseDefaultStmt(default);
    } else return null;
}

const StmtExprState = struct {
    lastExprToken: TokenIndex = 0,
    lastExprRes: Result = .{ .ty = Type.Void },
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

        if (noreturnIdx == null and p.nodeIsNoreturn(s) == .yes) {
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

    if (isFnBody) {
        const lastNoreturn = if (p.declBuffer.items.len == declBufferTop)
            .no
        else
            p.nodeIsNoreturn(p.declBuffer.items[p.declBuffer.items.len - 1]);

        if (lastNoreturn != .yes) {
            const retTy = p.func.type.?.getReturnType();
            var returnZero = false;
            if (lastNoreturn == .no and !retTy.is(.Void) and !retTy.isArray() and !retTy.isFunc()) {
                const funcName = p.getTokenText(p.func.name);
                const internerName = try p.comp.intern(funcName);
                if (internerName == p.stringsIds.mainId and retTy.is(.Int))
                    returnZero = true
                else
                    try p.errStr(.func_does_not_return, p.tokenIdx - 1, funcName);
            }

            try p.declBuffer.append(try p.addNode(.{
                .tag = .ImplicitReturn,
                .type = p.func.type.?.getReturnType(),
                .data = .{ .returnZero = returnZero },
            }));
        }

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

    if (p.func.type.?.hasAttribute(.noreturn))
        try p.errStr(.invalid_noreturn, eToken, p.getTokenText(p.func.name));

    if (expr.node == .none) {
        if (!returnType.is(.Void))
            try p.errStr(.func_should_return, retToken, p.getTokenText(p.func.name));
        return try p.addNode(.{ .tag = .ReturnStmt, .data = .{ .unExpr = expr.node } });
    } else if (returnType.is((.Void))) {
        try p.errStr(.void_func_returns_value, eToken, p.getTokenText(p.func.name));
        return try p.addNode(.{ .tag = .ReturnStmt, .data = .{ .unExpr = expr.node } });
    }

    try expr.lvalConversion(p);
    try expr.coerce(p, returnType, eToken, .ret);
    try expr.saveValue(p);

    return try p.addNode(.{ .tag = .ReturnStmt, .data = .{ .unExpr = expr.node } });
}

const NoreturnKind = enum { no, yes, complex };

fn nodeIsNoreturn(p: *Parser, node: NodeIndex) NoreturnKind {
    switch (p.nodes.items(.tag)[@intFromEnum(node)]) {
        .BreakStmt, .ContinueStmt, .ReturnStmt => return .yes,
        .IfThenElseStmt => {
            const data = p.data.items[p.nodes.items(.data)[@intFromEnum(node)].if3.body..];
            const thenType = p.nodeIsNoreturn(data[0]);
            const elseType = p.nodeIsNoreturn(data[1]);
            if (thenType == .complex or elseType == .complex) return .complex;
            if (thenType == .yes and elseType == .yes) return .yes;
            return .no;
        },

        .CompoundStmtTwo => {
            const data = p.nodes.items(.data)[@intFromEnum(node)];
            if (data.binExpr.rhs != .none) return p.nodeIsNoreturn(data.binExpr.rhs);
            if (data.binExpr.lhs != .none) return p.nodeIsNoreturn(data.binExpr.lhs);
            return .no;
        },

        .CompoundStmt => {
            const data = p.nodes.items(.data)[@intFromEnum(node)];
            return p.nodeIsNoreturn(p.data.items[data.range.end - 1]);
        },

        .LabeledStmt => {
            const data = p.nodes.items(.data)[@intFromEnum(node)];
            return p.nodeIsNoreturn(data.decl.node);
        },

        .SwitchStmt => {
            const data = p.nodes.items(.data)[@intFromEnum(node)];
            if (data.binExpr.rhs == .none) return .complex;
            if (p.nodeIsNoreturn(data.binExpr.rhs) == .yes) return .yes;
            return .complex;
        },

        else => return .no,
    }
}

pub fn tempTree(p: *Parser) AST {
    return .{
        .nodes = p.nodes.slice(),
        .data = p.data.items,
        .valueMap = p.valueMap,
        .comp = p.comp,
        .arena = undefined,
        .generated = undefined,
        .tokens = undefined,
        .rootDecls = undefined,
    };
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
            .KeywordC23ThreadLocal,
            .KeywordInline,
            .KeywordGccInline1,
            .KeywordGccInline2,
            .KeywordNoreturn,
            .KeywordVoid,
            .KeywordBool,
            .KeywordC23Bool,
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
            .KeywordC23Alignas,
            .KeywordTypeof,
            .KeywordTypeof1,
            .KeywordTypeof2,
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

    if (res.value.isUnavailable()) {
        try p.errToken(.expected_expr, p.tokenIdx);
        return false;
    }

    return res.value.getBool();
}

/// expression : assign-expression (',' assign-expression)*
fn parseExpr(p: *Parser) Error!Result {
    var exprStartIdx = p.tokenIdx;
    var errStart = p.comp.diagnostics.list.items.len;
    var lhs = try p.parseAssignExpr();

    if (p.getCurrToken() == .Comma)
        try lhs.expect(p);

    while (p.eat(.Comma)) |_| {
        try lhs.maybeWarnUnused(p, exprStartIdx, errStart);
        exprStartIdx = p.tokenIdx;
        errStart = p.comp.diagnostics.list.items.len;

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
    if (!p.tempTree().isLValueExtra(lhs.node, &isConst) or isConst) {
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

    try rhs.coerce(p, lhs.ty, token, .assign);
    try lhs.bin(p, tag, rhs);
    return lhs;
}

/// integer-const-expression : const-expression
fn parseIntegerConstExpr(p: *Parser, declFolding: ConstDeclFoldingMode) Error!Result {
    const start = p.tokenIdx;
    const res = try p.constExpr(declFolding);
    if (!res.ty.isInt() and !res.ty.isInvalid()) {
        try p.errToken(.expected_integer_constant_expr, start);
        return error.ParsingFailed;
    }
    return res;
}

// Caller is responsible for issuing a diagnostic if result is invalid/unavailable
// constExpr : condExpr
fn constExpr(p: *Parser, declFolding: ConstDeclFoldingMode) Error!Result {
    const constDeclFolding = p.constDeclFolding;
    defer p.constDeclFolding = constDeclFolding;

    p.constDeclFolding = declFolding;

    const res = try p.parseCondExpr();
    try res.expect(p);
    if (res.ty.isInvalid() or res.value.isUnavailable())
        return res;

    // saveValue sets val to unavailable
    var copy = res;
    try copy.saveValue(p);
    return res;
}

/// conditional-expression : logical-OR-expression ('?' expression? ':' conditional-expression)?
fn parseCondExpr(p: *Parser) Error!Result {
    const condToken = p.tokenIdx;
    var cond = try p.logicalOrExpr();
    if (cond.empty(p) or p.eat(.QuestionMark) == null)
        return cond;

    try cond.lvalConversion(p);
    const savedEval = p.noEval;

    if (!cond.ty.isScalar()) {
        try p.errStr(.cond_expr_type, condToken, try p.typeStr(cond.ty));
        return error.ParsingFailed;
    }

    // Prepare for possible binary conditional expression.
    const maybeColon = p.eat(.Colon); // eat ':'

    // Depending on the value of the condition, avoid  evaluating unreachable
    var thenExpr = blk: {
        defer p.noEval = savedEval;
        if (!cond.value.isUnavailable() and !cond.value.getBool())
            p.noEval = true;

        break :blk try p.parseExpr();
    };
    try thenExpr.expect(p);

    // If we saw a colon then this is a binary conditional expression.
    if (maybeColon) |colon| {
        var condThen = cond;
        condThen.node = try p.addNode(.{ .tag = .CondDummyExpr, .type = cond.ty, .data = .{ .unExpr = cond.node } });
        _ = try condThen.adjustTypes(colon, &thenExpr, p, .conditional);
        cond.ty = thenExpr.ty;
        cond.node = try p.addNode(.{
            .tag = .BinaryCondExpr,
            .type = cond.ty,
            .data = .{ .if3 = .{ .cond = cond.node, .body = (try p.addList(&.{ condThen.node, thenExpr.node })).start } },
        });
        return cond;
    }

    const colon = try p.expectToken(.Colon);

    var elseExpr = blk: {
        defer p.noEval = savedEval;
        if (!cond.value.isUnavailable() and cond.value.getBool())
            p.noEval = true;

        break :blk try p.parseCondExpr();
    };
    try elseExpr.expect(p);

    _ = try thenExpr.adjustTypes(colon, &elseExpr, p, .conditional);

    if (!cond.value.isUnavailable()) {
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
        if (!lhs.value.isUnavailable() and lhs.value.getBool())
            p.noEval = true;

        var rhs = try p.logicalAndExpr();
        try rhs.expect(p);

        if (try lhs.adjustTypes(token, &rhs, p, .booleanLogic)) {
            const res = @intFromBool(lhs.value.getBool() or rhs.value.getBool());
            lhs.value = Value.int(res);
        }

        try lhs.boolRes(p, .BoolOrExpr, rhs);
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
        if (!lhs.value.isUnavailable() and !lhs.value.getBool())
            p.noEval = true;

        var rhs = try p.parseOrExpr();
        try rhs.expect(p);

        if (try lhs.adjustTypes(token, &rhs, p, .booleanLogic)) {
            const res = @intFromBool(lhs.value.getBool() and rhs.value.getBool());
            lhs.value = Value.int(res);
        }

        try lhs.boolRes(p, .BoolAndExpr, rhs);
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
            lhs.value = lhs.value.bitOr(rhs.value, lhs.ty, p.comp);

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
            lhs.value = lhs.value.bitXor(rhs.value, lhs.ty, p.comp);

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
            lhs.value = lhs.value.bitAnd(rhs.value, lhs.ty, p.comp);

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
            const res = lhs.value.compare(op, rhs.value, lhs.ty, p.comp);
            lhs.value = Value.int(@intFromBool(res));
        }

        try lhs.boolRes(p, tag, rhs);
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

            const res = lhs.value.compare(op, rhs.value, lhs.ty, p.comp);
            lhs.value = Value.int(@intFromBool(res));
        }

        lhs.ty = Type.Int;
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
            if (shl != null)
                lhs.value = lhs.value.shl(rhs.value, lhs.ty, p.comp)
            else
                lhs.value = lhs.value.shr(rhs.value, lhs.ty, p.comp);
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

        const lhsTy = lhs.ty;
        if (try lhs.adjustTypes(minus.?, &rhs, p, if (plus != null) .add else .sub)) {
            if (plus != null) {
                if (lhs.value.add(lhs.value, rhs.value, lhs.ty, p.comp))
                    try p.errOverflow(plus.?, lhs);
            } else {
                if (lhs.value.sub(lhs.value, rhs.value, lhs.ty, p.comp))
                    try p.errOverflow(minus.?, lhs);
            }
        }

        if (!lhs.ty.isInvalid() and lhsTy.isPointer() and !lhsTy.isVoidStar() and lhsTy.getElemType().hasIncompleteSize()) {
            try p.errStr(.ptr_arithmetic_incomplete, minus.?, try p.typeStr(lhsTy.getElemType()));
            lhs.ty = Type.Invalid;
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
            if (div != null)
                try p.errStr(errTag, div.?, "division")
            else
                try p.errStr(errTag, percent.?, "remainder");

            if (p.inMacro)
                return error.ParsingFailed;
        }

        if (try lhs.adjustTypes(percent.?, &rhs, p, if (tag == .ModExpr) .integer else .arithmetic)) {
            if (mul != null) {
                if (lhs.value.mul(lhs.value, rhs.value, lhs.ty, p.comp))
                    try p.errOverflow(mul.?, lhs);
            } else if (div != null) {
                lhs.value = Value.div(lhs.value, rhs.value, lhs.ty, p.comp);
            } else {
                var res = Value.rem(lhs.value, rhs.value, lhs.ty, p.comp);
                if (res.isUnavailable()) {
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
    if (p.comp.diagnostics.list.items.len == 0) return;

    const lastExprLoc = p.pp.tokens.items(.loc)[lastExprToken];
    const lastMessage = p.comp.diagnostics.list.items[p.comp.diagnostics.list.items.len - 1];

    if (lastMessage.tag == .unused_value and lastMessage.loc.eql(lastExprLoc))
        p.comp.diagnostics.list.items.len = p.comp.diagnostics.list.items.len - 1;
}

/// cast-expression
///  : '(' compoundStmt ')'
///  | '(' typeName ')' cast-expression
///  | '(' typeName ')' '{' initializerItems '}'
///  | __builtin_choose_expr '(' intger-const-expression ',' assign-expression ',' assign-expression ')'
///  | __builtin_va_arg '(' assign-expression ',' typeName ')'
///  | __builtin_offsetof '(' type-name ',' offsetofMemberDesignator ')'
///  | __builtin_bitoffsetof '(' type-name ',' offsetofMemberDesignator ')'
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
        const ty = (try p.parseTypeName()) orelse {
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

        try operand.lvalConversion(p);
        try operand.castType(p, ty, lp);

        return operand;
    }

    switch (p.getCurrToken()) {
        .BuiltinChooseExpr => return p.parseBuiltinChooseExpr(),
        .BuiltinVaArg => return p.builtinVaArg(),
        .BuiltinOffsetof => return p.builtinOffsetof(false),
        .BuiltinBitOffsetof => return p.builtinOffsetof(true),
        .BuiltinTypesCompatibleP => return p.typesCompatible(),
        // TODO: other special-cased builtins
        else => {},
    }

    return p.parseUnaryExpr();
}

fn typesCompatible(p: *Parser) Error!Result {
    p.tokenIdx += 1;
    const lp = try p.expectToken(.LParen);

    const first = (try p.parseTypeName()) orelse {
        try p.err(.expected_type);
        p.skipTo(.RParen);
        return error.ParsingFailed;
    };
    const lhs = try p.addNode(.{ .tag = .Invalid, .type = first, .data = undefined });

    _ = try p.expectToken(.Comma);

    const second = (try p.parseTypeName()) orelse {
        try p.err(.expected_type);
        p.skipTo(.RParen);
        return error.ParsingFailed;
    };
    const rhs = try p.addNode(.{ .tag = .Invalid, .type = second, .data = undefined });

    try p.expectClosing(lp, .RParen);

    const compatible = first.compatible(second, p.comp);
    const res = Result{
        .value = Value.int(@intFromBool(compatible)),
        .node = try p.addNode(.{
            .tag = .BuiltinTypesCompatibleP,
            .type = Type.Int,
            .data = .{ .binExpr = .{ .lhs = lhs, .rhs = rhs } },
        }),
    };

    try p.valueMap.put(res.node, res.value);
    return res;
}

fn parseBuiltinChooseExpr(p: *Parser) Error!Result {
    p.tokenIdx += 1;
    const lp = try p.expectToken(.LParen);
    const condToken = p.tokenIdx;
    var cond = try p.parseIntegerConstExpr(.NoConstDeclFolding);
    if (cond.value.isUnavailable()) {
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

    if (!vaList.ty.eql(p.comp.types.vaList, p.comp, true)) {
        try p.errStr(.incompatible_va_arg, vaListToken, try p.typeStr(vaList.ty));
        return error.ParsingFailed;
    }

    return Result{
        .ty = ty,
        .node = try p.addNode(.{
            .tag = .BuiltinCallExprOne,
            .type = ty,
            .data = .{ .decl = .{ .name = builtinToken, .node = vaList.node } },
        }),
    };
}

fn builtinOffsetof(p: *Parser, wantsBits: bool) Error!Result {
    const builtinToken = p.tokenIdx;
    p.tokenIdx += 1;

    const lparen = try p.expectToken(.LParen);
    const tyToken = p.tokenIdx;

    const ty = (try p.parseTypeName()) orelse {
        try p.err(.expected_type);
        p.skipTo(.RParen);
        return error.ParsingFailed;
    };

    if (!ty.isRecord()) {
        try p.errStr(.offsetof_ty, tyToken, try p.typeStr(ty));
        p.skipTo(.RParen);
        return error.ParsingFailed;
    } else if (ty.hasIncompleteSize()) {
        try p.errStr(.offsetof_incomplete, tyToken, try p.typeStr(ty));
        p.skipTo(.RParen);
        return error.ParsingFailed;
    }

    _ = try p.expectToken(.Comma);

    const offsetofExpr = try p.offsetofMemberDesignator(ty);

    try p.expectClosing(lparen, .RParen);

    return Result{
        .ty = p.comp.types.size,
        .value = if (offsetofExpr.value.tag == .int and !wantsBits)
            Value.int(offsetofExpr.value.data.int / 8)
        else
            offsetofExpr.value,

        .node = try p.addNode(.{
            .tag = .BuiltinCallExprOne,
            .type = p.comp.types.size,
            .data = .{ .decl = .{ .name = builtinToken, .node = offsetofExpr.node } },
        }),
    };
}

/// offsetofMemberDesignator: Identifier ('.' Identifier | '[' expr ']' )*
fn offsetofMemberDesignator(p: *Parser, baseType: Type) Error!Result {
    errdefer p.skipTo(.RParen);
    const baseFieldNameToken = try p.expectIdentifier();
    const baseFieldName = try p.getInternString(baseFieldNameToken);

    try p.validateFieldAccess(baseType, baseType, baseFieldNameToken, baseFieldName);

    const baseNode = try p.addNode(.{ .tag = .DefaultInitExpr, .type = baseType, .data = undefined });

    var offsetNum: u64 = 0;
    const baseRecordTy = baseType.canonicalize(.standard);
    var lhs = try p.fieldAccessExtra(baseNode, baseRecordTy, baseFieldName, false, &offsetNum);
    var bitOffset = Value.int(offsetNum);

    while (true) switch (p.getCurrToken()) {
        .Period => {
            p.tokenIdx += 1;
            const fieldNameToken = try p.expectIdentifier();
            const fieldName = try p.getInternString(fieldNameToken);

            if (!lhs.ty.isRecord()) {
                try p.errStr(.offsetof_ty, fieldNameToken, try p.typeStr(lhs.ty));
                return error.ParsingFailed;
            }
            try p.validateFieldAccess(lhs.ty, lhs.ty, fieldNameToken, fieldName);
            lhs = try p.fieldAccessExtra(lhs.node, lhs.ty, fieldName, false, &offsetNum);
            if (bitOffset.tag != .unavailable)
                bitOffset = Value.int(offsetNum + bitOffset.getInt(u64));
        },
        .LBracket => {
            const lbracketToken = p.tokenIdx;
            p.tokenIdx += 1;
            var index = try p.parseExpr();
            try index.expect(p);
            _ = try p.expectClosing(lbracketToken, .RBracket);

            if (!lhs.ty.isArray()) {
                try p.errStr(.offsetof_array, lbracketToken, try p.typeStr(lhs.ty));
                return error.ParsingFailed;
            }

            var ptr = lhs;
            try ptr.lvalConversion(p);
            try index.lvalConversion(p);

            if (!index.ty.isInt())
                try p.errToken(.invalid_index, lbracketToken);
            try p.checkArrayBounds(index, lhs, lbracketToken);

            try index.saveValue(p);
            try ptr.bin(p, .ArrayAccessExpr, index);
            lhs = ptr;
        },
        else => break,
    };

    return Result{ .ty = baseType, .value = bitOffset, .node = lhs.node };
}

/// unaryExpr
///  : primary-expression suffix-expression*
///  | '&&' identifier
///  | ('&' | '*' | '+' | '-' | '~' | '!' | '++' | '--' | `__extension__` | `__imag__` | `__real__`) cast-expression
///  | `sizeof` unary-expression
///  | `sizeof` '(' type-name ')'
///  | (`keyword-alignof` | `keyword-c23-alignof`) '(' type-name ')'
fn parseUnaryExpr(p: *Parser) Error!Result {
    const token = p.tokenIdx;
    switch (p.getCurrToken()) {
        .Ampersand => {
            if (p.inMacro) {
                try p.err(.invalid_preproc_operator);
                return error.ParsingFailed;
            }
            p.tokenIdx += 1;
            var operand = try p.parseCastExpr();
            try operand.expect(p);

            const tree = p.tempTree();
            if (p.getNode(operand.node, .MemberAccessExpr) orelse
                p.getNode(operand.node, .MemberAccessPtrExpr)) |memberNode|
            {
                if (tree.isBitField(memberNode))
                    try p.errToken(.addr_of_bitfield, token);
            }

            if (!tree.isLValue(operand.node))
                try p.errToken(.addr_of_rvalue, token);

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

            const str = p.getTokenText(nameToken);
            if (p.findLabel(str) == null)
                try p.labels.append(.{ .unresolvedGoto = nameToken });

            const elemType = try p.arena.create(Type);
            elemType.* = Type.Void;

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

            if (operand.ty.isArray() or operand.ty.isPointer() or operand.ty.isFunc()) {
                try operand.lvalConversion(p);
                operand.ty = operand.ty.getElemType();
            } else {
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

            try operand.usualUnaryConversion(p, token);

            return operand;
        },

        .Minus => {
            p.tokenIdx += 1;
            var operand = try p.parseCastExpr();
            try operand.expect(p);
            try operand.lvalConversion(p);

            if (!operand.ty.isInt() and !operand.ty.isFloat())
                try p.errStr(.invalid_argument_un, token, try p.typeStr(operand.ty));

            try operand.usualUnaryConversion(p, token);
            if (operand.value.isNumeric())
                _ = operand.value.sub(operand.value.zero(), operand.value, operand.ty, p.comp)
            else
                operand.value.tag = .unavailable;

            try operand.un(p, .NegateExpr);
            return operand;
        },

        .PlusPlus => {
            p.tokenIdx += 1;
            var operand = try p.parseCastExpr();
            try operand.expect(p);

            if (!operand.ty.isScalar())
                try p.errStr(.invalid_argument_un, token, try p.typeStr(operand.ty));

            if (!p.tempTree().isLValue(operand.node) or operand.ty.isConst()) {
                try p.errToken(.not_assignable, token);
                return error.ParsingFailed;
            }

            try operand.usualUnaryConversion(p, token);

            if (operand.value.isNumeric()) {
                if (operand.value.add(operand.value, operand.value.one(), operand.ty, p.comp))
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

            if (!operand.ty.isScalar())
                try p.errStr(.invalid_argument_un, token, try p.typeStr(operand.ty));

            if (!p.tempTree().isLValue(operand.node) or operand.ty.isConst()) {
                try p.errToken(.not_assignable, token);
                return error.ParsingFailed;
            }

            try operand.usualUnaryConversion(p, token);

            if (operand.value.isNumeric()) {
                if (operand.value.sub(operand.value, operand.value.one(), operand.ty, p.comp))
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
            try operand.usualUnaryConversion(p, token);

            if (operand.ty.isInt()) {
                try operand.intCast(p, operand.ty.integerPromotion(p.comp), token);
                if (operand.value.tag == .int) {
                    operand.value = operand.value.bitNot(operand.ty, p.comp);
                }
            } else {
                try p.errStr(.invalid_argument_un, token, try p.typeStr(operand.ty));
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

            if (!operand.ty.isScalar())
                try p.errStr(.invalid_argument_un, token, try p.typeStr(operand.ty));

            try operand.usualUnaryConversion(p, token);

            if (operand.value.tag == .int) {
                const res = Value.int(@intFromBool(!operand.value.getBool()));
                operand.value = res;
            } else if (operand.value.tag == .nullptrTy) {
                operand.value = Value.int(1);
            } else {
                if (operand.ty.isDecayed())
                    operand.value = Value.int(0)
                else
                    operand.value.tag = .unavailable;
            }

            operand.ty = Type.Int;
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

            if (res.ty.is(.Void)) {
                try p.errStr(.pointer_arith_void, token, "sizeof");
            } else if (res.ty.isDecayed()) {
                const arrayTy = res.ty.originalTypeOfDecayedArray();
                const errString = try p.typePairStrExtra(res.ty, " instead of ", arrayTy);
                try p.errStr(.sizeof_array_arg, token, errString);
            }

            if (res.ty.sizeof(p.comp)) |size| {
                if (size == 0) try p.errToken(.sizeof_returns_zero, token);
                res.value = Value.int(size);
                res.ty = p.comp.types.size;
            } else {
                res.value.tag = .unavailable;
                if (res.ty.hasIncompleteSize()) {
                    try p.errStr(.invalid_sizeof, expectedParen - 1, try p.typeStr(res.ty));
                    res.ty = Type.Invalid;
                } else {
                    res.ty = p.comp.types.size;
                }
            }
            try res.un(p, .SizeOfExpr);
            return res;
        },

        .KeywordAlignof,
        .KeywordGccAlignof1,
        .KeywordGccAlignof2,
        .KeywordC23Alignof,
        => {
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

            if (res.ty.is(.Void))
                try p.errStr(.pointer_arith_void, token, "alignof");

            if (res.ty.alignable()) {
                res.value = Value.int(res.ty.alignof(p.comp));
                res.ty = p.comp.types.size;
            } else {
                try p.errStr(.invalid_alignof, expectedParen, try p.typeStr(res.ty));
                return error.ParsingFailed;
            }
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

        .KeywordImag1, .KeywordImag2 => {
            const imagToken = p.tokenIdx;
            p.tokenIdx += 1;

            var operand = try p.parseCastExpr();
            try operand.expect(p);
            try operand.lvalConversion(p);
            if (!operand.ty.isInt() and !operand.ty.isFloat()) {
                try p.errStr(.invalid_imag, imagToken, try p.typeStr(operand.ty));
            }
            // convert _Complex F to F
            operand.ty = operand.ty.makeReal();

            try operand.un(p, .ImagExpr);
            return operand;
        },

        .KeywordReal1, .KeywordReal2 => {
            const realToken = p.tokenIdx;
            p.tokenIdx += 1;

            var operand = try p.parseCastExpr();
            try operand.expect(p);
            try operand.lvalConversion(p);
            if (!operand.ty.isInt() and !operand.ty.isFloat()) {
                try p.errStr(.invalid_real, realToken, try p.typeStr(operand.ty));
            }

            operand.ty = operand.ty.makeReal();

            try operand.un(p, .RealExpr);
            return operand;
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

            const arrayBeforeConversion = lhs;
            const indexBeforeConversion = index;
            var ptr = lhs;
            try ptr.lvalConversion(p);
            try index.lvalConversion(p);
            if (ptr.ty.isPointer()) {
                ptr.ty = ptr.ty.getElemType();
                if (!index.ty.isInt()) try p.errToken(.invalid_index, lb);
                try p.checkArrayBounds(indexBeforeConversion, arrayBeforeConversion, lb);
            } else if (index.ty.isPointer()) {
                index.ty = index.ty.getElemType();
                if (!ptr.ty.isInt()) try p.errToken(.invalid_index, lb);
                try p.checkArrayBounds(arrayBeforeConversion, indexBeforeConversion, lb);
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
                try copy.implicitCast(p, .ArrayToPointer);
                return p.fieldAccess(copy, name, true);
            }
            return p.fieldAccess(lhs, name, true);
        },

        .PlusPlus => {
            defer p.tokenIdx += 1;
            var operand = lhs;

            if (!operand.ty.isScalar())
                try p.errStr(.invalid_argument_un, p.tokenIdx, try p.typeStr(operand.ty));

            if (!p.tempTree().isLValue(operand.node) or operand.ty.isConst()) {
                try p.err(.not_assignable);
                return error.ParsingFailed;
            }

            try operand.usualUnaryConversion(p, p.tokenIdx);

            try operand.un(p, .PostIncExpr);
            return operand;
        },

        .MinusMinus => {
            defer p.tokenIdx += 1;
            var operand = lhs;

            if (!operand.ty.isScalar())
                try p.errStr(.invalid_argument_un, p.tokenIdx, try p.typeStr(operand.ty));

            if (!p.tempTree().isLValue(operand.node) or operand.ty.isConst()) {
                try p.err(.not_assignable);
                return error.ParsingFailed;
            }

            try operand.usualUnaryConversion(p, p.tokenIdx);

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

    const fieldName = try p.getInternString(fieldNameToken);
    try p.validateFieldAccess(recordType, exprType, fieldNameToken, fieldName);
    var discard: u64 = 0;
    return p.fieldAccessExtra(lhs.node, recordType, fieldName, isArrow, &discard);
}

fn validateFieldAccess(
    p: *Parser,
    recordType: Type,
    exprType: Type,
    fieldNameToken: TokenIndex,
    fieldName: StringId,
) Error!void {
    if (recordType.hasField(fieldName))
        return;

    p.strings.items.len = 0;

    try p.strings.writer().print("'{s}' in '", .{p.getTokenText(fieldNameToken)});
    const mapper = p.comp.stringInterner.getSlowTypeMapper();
    try exprType.print(mapper, p.comp.langOpts, p.strings.writer());
    try p.strings.append('\'');

    const duped = try p.comp.diagnostics.arena.allocator().dupe(u8, p.strings.items);
    try p.errStr(.no_such_member, fieldNameToken, duped);
    return error.ParsingFailed;
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
    fieldName: StringId,
    isArrow: bool, // is arrow operator
    offsetBits: *u64,
) Error!Result {
    for (recordType.data.record.fields, 0..) |f, i| {
        if (f.isAnonymousRecord()) {
            if (!f.ty.hasField(fieldName)) continue;
            const inner = try p.addNode(.{
                .tag = if (isArrow) .MemberAccessPtrExpr else .MemberAccessExpr,
                .type = f.ty,
                .data = .{ .member = .{ .lhs = lhs, .index = @intCast(i) } },
            });
            const res = p.fieldAccessExtra(inner, f.ty, fieldName, false, offsetBits);
            offsetBits.* += f.layout.offsetBits;
            return res;
        }

        if (fieldName == f.name) {
            offsetBits.* = f.layout.offsetBits;
            return Result{
                .ty = f.ty,
                .node = try p.addNode(.{
                    .tag = if (isArrow) .MemberAccessPtrExpr else .MemberAccessExpr,
                    .type = f.ty,
                    .data = .{ .member = .{ .lhs = lhs, .index = @intCast(i) } },
                }),
            };
        }
    }
    // We already checked that this container has a field by the name.
    unreachable;
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
            if (arg.ty.isInt()) try arg.intCast(p, arg.ty.integerPromotion(p.comp), paramToken);
            if (arg.ty.is(.Float)) try arg.floatCast(p, Type.Double);
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
                try p.getInternString(p.nodes.items(.data)[@intFromEnum(declRef.?)].declRef) != lastParamName)
            {
                try p.errToken(.va_start_not_last_param, paramToken);
            }
        } else {
            try arg.coerce(p, paramType, paramToken, .{ .arg = params[argCount].nameToken });
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
        .data = .{ .binExpr = .{ .lhs = func.node, .rhs = .none } },
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

fn checkArrayBounds(p: *Parser, index: Result, array: Result, token: TokenIndex) !void {
    if (index.value.isUnavailable()) return;
    const arrayLen = array.ty.arrayLen() orelse return;
    if (arrayLen == 0) return;

    if (arrayLen == 1) {
        if (p.getNode(array.node, .MemberAccessExpr) orelse p.getNode(array.node, .MemberAccessPtrExpr)) |node| {
            const data = p.nodes.items(.data)[@intFromEnum(node)];
            var lhs = p.nodes.items(.type)[@intFromEnum(data.member.lhs)];
            if (lhs.get(.Pointer)) |ptr|
                lhs = ptr.data.subType.*;

            if (lhs.is(.Struct)) {
                const record = lhs.getRecord().?;
                if (data.member.index + 1 == record.fields.len) {
                    if (!index.value.isZero()) {
                        try p.errExtra(.old_style_flexible_struct, token, .{
                            .unsigned = index.value.data.int,
                        });
                    }
                    return;
                }
            }
        }
    }

    const len = Value.int(arrayLen);
    if (index.ty.isUnsignedInt(p.comp)) {
        if (index.value.compare(.gte, len, p.comp.types.size, p.comp))
            try p.errExtra(.array_after, token, .{ .unsigned = index.value.data.int });
    } else {
        if (index.value.compare(.lt, Value.int(0), index.ty, p.comp)) {
            const signed: i64 = index.value.signExtend(index.ty, p.comp);
            try p.errExtra(.array_before, token, .{ .signed = signed });
        } else if (index.value.compare(.gte, len, p.comp.types.size, p.comp)) {
            try p.errExtra(.array_after, token, .{ .unsigned = index.value.data.int });
        }
    }
}

/// primary-expression
///  : identifier
///  | keywordTrue or keywordFalse
///  | keywordNullptr
///  | integer-literal
///  | float-literal
///  | imaginary-literal
///  | char-literal
///  | string-literal
///  | '(' expression ')'
///  | generic-selection
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
            const name = p.getTokenText(nameToken);
            const internedName = try p.comp.intern(name);
            if (p.comp.builtins.get(name)) |some| {
                for (p.tokenIds[p.tokenIdx..]) |id| switch (id) {
                    .RParen => {}, // closing grouped expr
                    .LParen => break, // beginning of a call
                    else => {
                        try p.errToken(.builtin_must_be_called, nameToken);
                        return error.ParsingFailed;
                    },
                };
                const node = try p.addNode(.{
                    .tag = .BuiltinCallExprOne,
                    .type = some,
                    .data = .{ .decl = .{ .name = nameToken, .node = .none } },
                });
                return Result{ .ty = some, .node = node };
            }
            if (p.symStack.findSymbol(internedName)) |sym| {
                try p.checkDeprecatedUnavailable(sym.type, nameToken, sym.token);
                if (sym.kind == .constexpr) {
                    const node = try p.addNode(.{
                        .tag = .DeclRefExpr,
                        .type = sym.type,
                        .data = .{ .declRef = nameToken },
                    });
                    return Result{ .value = sym.value, .ty = sym.type, .node = node };
                }

                if (sym.value.tag == .int) {
                    switch (p.constDeclFolding) {
                        .GNUFoldingExtension => try p.errToken(.const_decl_folded, nameToken),
                        .GNUVLAFoldingExtension => try p.errToken(.const_decl_folded_vla, nameToken),
                        else => {},
                    }
                }

                const node = try p.addNode(.{
                    .tag = if (sym.kind == .enumeration) .EnumerationRef else .DeclRefExpr,
                    .type = sym.type,
                    .data = .{ .declRef = nameToken },
                });
                return Result{
                    .value = if (p.constDeclFolding == .NoConstDeclFolding and sym.kind != .enumeration) Value{} else sym.value,
                    .ty = sym.type,
                    .node = node,
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
                funcType.* = .{ .returnType = Type.Int, .params = &.{} };
                const ty: Type = .{ .specifier = .OldStyleFunc, .data = .{ .func = funcType } };
                const node = try p.addNode(.{
                    .type = ty,
                    .tag = .FnProto,
                    .data = .{ .decl = .{ .name = nameToken } },
                });

                try p.declBuffer.append(node);
                try p.symStack.declareSymbol(internedName, ty, nameToken, node);

                return Result{
                    .ty = ty,
                    .node = try p.addNode(.{ .tag = .DeclRefExpr, .type = ty, .data = .{ .declRef = nameToken } }),
                };
            }
            try p.errStr(.undeclared_identifier, nameToken, p.getTokenText(nameToken));
            return error.ParsingFailed;
        },

        .KeywordTrue, .KeywordFalse => |id| {
            p.tokenIdx += 1;
            const numericValue = @intFromBool(id == .KeywordTrue);
            const res = Result{
                .value = Value.int(numericValue),
                .ty = Type.Bool,
                .node = try p.addNode(.{
                    .tag = .BoolLiteral,
                    .type = Type.Bool,
                    .data = .{ .int = numericValue },
                }),
            };
            std.debug.assert(!p.inMacro); // Should have been replaced with .one / .zero
            try p.valueMap.put(res.node, res.value);
            return res;
        },

        .KeywordNullptr => {
            defer p.tokenIdx += 1;
            try p.errStr(.pre_c2x_compat, p.tokenIdx, "'nullptr'");

            return Result{
                .value = .{ .tag = .nullptrTy },
                .ty = Type.NullptrTy,
                .node = try p.addNode(.{
                    .tag = .NullPtrLiteral,
                    .type = Type.NullptrTy,
                    .data = undefined,
                }),
            };
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
                try p.strings.appendSlice(p.getTokenText(p.func.name));
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
                const mapper = p.comp.stringInterner.getSlowTypeMapper();
                p.strings.items.len = 0;
                try Type.printNamed(funcType, p.getTokenText(p.func.name), mapper, p.comp.langOpts, p.strings.writer());
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

        .PPNumber => return p.parsePPNumber(),

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
            .data = .{ .decl = .{ .name = p.tokenIdx, .node = strLit } },
        }),
    };
}

fn parseFloat(p: *Parser, buf: []const u8, suffix: NumberSuffix) !Result {
    switch (suffix) {
        .L => return p.todo("long double literals"),
        .IL => {
            try p.err(.gnu_imaginary_constant);
            return p.todo("long double imaginary literals");
        },

        .None, .I, .F, .IF => {
            const ty = switch (suffix) {
                .None, .I => Type.Double,
                .F, .IF => Type.Float,
                else => unreachable,
            };

            const dValue = std.fmt.parseFloat(f64, buf) catch |er| switch (er) {
                error.InvalidCharacter => return p.todo("c2x digit separators in floats"),
                else => unreachable,
            };

            const tag: AstTag = switch (suffix) {
                .None, .I => .DoubleLiteral,
                .F, .IF => .FloatLiteral,
                else => unreachable,
            };

            var res = Result{
                .ty = ty,
                .node = try p.addNode(.{ .tag = tag, .type = ty, .data = undefined }),
                .value = Value.float(dValue),
            };
            if (suffix.isImaginary()) {
                try p.err(.gnu_imaginary_constant);
                res.ty = switch (suffix) {
                    .I => Type.ComplexDouble,
                    .IF => Type.ComplexFloat,
                    else => unreachable,
                };
                res.value.tag = .unavailable;
                try res.un(p, .ImaginaryLiteral);
            }
            return res;
        },

        else => unreachable,
    }
}

fn getIntegerPart(p: *Parser, buffer: []const u8, prefix: NumberPrefix, tokenIdx: TokenIndex) ![]const u8 {
    if (buffer[0] == '.') return "";

    if (!prefix.digitAllowed(buffer[0])) {
        switch (prefix) {
            .binary => try p.errExtra(.invalid_binary_digit, tokenIdx, .{ .ascii = @intCast(buffer[0]) }),
            .octal => try p.errExtra(.invalid_octal_digit, tokenIdx, .{ .ascii = @intCast(buffer[0]) }),
            .hex => try p.errStr(.invalid_int_suffix, tokenIdx, buffer),
            .decimal => unreachable,
        }
        return error.ParsingFailed;
    }

    for (buffer, 0..) |c, idx| {
        if (idx == 0) continue;
        switch (c) {
            '.' => return buffer[0..idx],

            'p', 'P' => return if (prefix == .hex) buffer[0..idx] else {
                try p.errStr(.invalid_int_suffix, tokenIdx, buffer[idx..]);
                return error.ParsingFailed;
            },

            'e', 'E' => {
                switch (prefix) {
                    .hex => continue,
                    .decimal => return buffer[0..idx],
                    .binary => try p.errExtra(.invalid_binary_digit, tokenIdx, .{ .ascii = @intCast(c) }),
                    .octal => try p.errExtra(.invalid_octal_digit, tokenIdx, .{ .ascii = @intCast(c) }),
                }
                return error.ParsingFailed;
            },

            '0'...'9', 'a'...'d', 'A'...'D', 'f', 'F' => {
                if (!prefix.digitAllowed(c)) {
                    switch (prefix) {
                        .binary => try p.errExtra(.invalid_binary_digit, tokenIdx, .{ .ascii = @intCast(c) }),
                        .octal => try p.errExtra(.invalid_octal_digit, tokenIdx, .{ .ascii = @intCast(c) }),
                        .decimal, .hex => try p.errStr(.invalid_int_suffix, tokenIdx, buffer[idx..]),
                    }
                    return error.ParsingFailed;
                }
            },

            '\'' => {},
            else => return buffer[0..idx],
        }
    }
    return buffer;
}

fn castInt(p: *Parser, val: u64, specs: []const Type.Specifier) Error!Result {
    var res: Result = .{ .value = Value.int(val) };
    for (specs) |spec| {
        const ty = Type{ .specifier = spec };
        const isUnsigned = ty.isUnsignedInt(p.comp);
        const tySize = ty.sizeof(p.comp).?;
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
        res.ty = Type.ULongLong;
    }

    res.node = try p.addNode(.{ .tag = .IntLiteral, .type = res.ty, .data = .{ .int = val } });
    if (!p.inMacro)
        try p.valueMap.put(res.node, res.value);
    return res;
}

fn fixedSizeInt(p: *Parser, base: u8, buf: []const u8, suffix: NumberSuffix, tokenIdx: TokenIndex) !Result {
    var value: u64 = 0;
    var overflow = false;
    for (buf) |ch| {
        const digit: u64 = switch (ch) {
            '0'...'9' => ch - '0',
            'A'...'Z' => ch - 'A' + 10,
            'a'...'z' => ch - 'a' + 10,
            '\'' => continue,
            else => unreachable,
        };

        if (value != 0) {
            const product, const overflowed = @mulWithOverflow(value, base);
            if (overflowed != 0)
                overflow = true;
            value = product;
        }

        const sum, const overflowed = @addWithOverflow(value, digit);
        if (overflowed != 0)
            overflow = true;
        value = sum;
    }

    if (overflow) {
        try p.errToken(.int_literal_too_big, tokenIdx);
        var res: Result = .{ .ty = Type.ULongLong, .value = Value.int(value) };
        res.node = try p.addNode(.{ .tag = .IntLiteral, .type = res.ty, .data = undefined });
        if (!p.inMacro)
            try p.valueMap.put(res.node, res.value);
        return res;
    }

    if (suffix.isSignedInteger() and value > std.math.maxInt(i64))
        try p.errToken(.implicitly_unsigned_literal, tokenIdx);

    return if (base == 10)
        switch (suffix) {
            .None, .I => p.castInt(value, &.{ .Int, .Long, .LongLong }),
            .U, .IU => p.castInt(value, &.{ .UInt, .ULong, .ULongLong }),
            .L, .IL => p.castInt(value, &.{ .Long, .LongLong }),
            .UL, .IUL => p.castInt(value, &.{ .ULong, .ULongLong }),
            .LL, .ILL => p.castInt(value, &.{.LongLong}),
            .ULL, .IULL => p.castInt(value, &.{.ULongLong}),
            else => unreachable,
        }
    else switch (suffix) {
        .None, .I => p.castInt(value, &.{ .Int, .UInt, .Long, .ULong, .LongLong, .ULongLong }),
        .U, .IU => p.castInt(value, &.{ .UInt, .ULong, .ULongLong }),
        .L, .IL => p.castInt(value, &.{ .Long, .ULong, .LongLong, .ULongLong }),
        .UL, .IUL => p.castInt(value, &.{ .ULong, .ULongLong }),
        .LL, .ILL => p.castInt(value, &.{ .LongLong, .ULongLong }),
        .ULL, .IULL => p.castInt(value, &.{.ULongLong}),
        else => unreachable,
    };
}

fn parseInt(
    p: *Parser,
    prefix: NumberPrefix,
    buf: []const u8,
    suffix: NumberSuffix,
    tokenIdx: TokenIndex,
) !Result {
    if (prefix == .binary)
        try p.errToken(.binary_integer_literal, tokenIdx);

    const base = @intFromEnum(prefix);
    var res = if (suffix.isBitInt())
        try p.bitInt(base, buf, suffix, tokenIdx)
    else
        try p.fixedSizeInt(base, buf, suffix, tokenIdx);

    if (suffix.isImaginary()) {
        try p.errToken(.gnu_imaginary_constant, tokenIdx);
        res.ty = res.ty.makeComplex();
        res.value.tag = .unavailable;
        try res.un(p, .ImaginaryLiteral);
    }
    return res;
}
fn bitInt(p: *Parser, base: u8, buf: []const u8, suffix: NumberSuffix, tokenIdx: TokenIndex) Error!Result {
    try p.errStr(.pre_c2x_compat, tokenIdx, "'_BitInt' suffix for literals");
    try p.errToken(.bitint_suffix, tokenIdx);

    var managed = try big.int.Managed.init(p.comp.gpa);
    defer managed.deinit();

    managed.setString(base, buf) catch |e| switch (e) {
        error.InvalidBase => unreachable, // `base` is one of 2, 8, 10, 16
        error.InvalidCharacter => unreachable, // digits validated by Tokenizer
        else => |er| return er,
    };

    const c = managed.toConst();
    const bitsNeeded: std.math.IntFittingRange(0, Compilation.BitIntMaxBits) = blk: {
        const count = @max(1, c.bitCountTwosComp());
        const signBits = @intFromBool(suffix.isSignedInteger());
        const bitsNeeded = count + signBits;

        if (bitsNeeded > Compilation.BitIntMaxBits) {
            const specifier: TypeBuilder.Specifier = switch (suffix) {
                .WB => .{ .BitInt = 0 },
                .UWB => .{ .UBitInt = 0 },
                .IWB => .{ .ComplexBitInt = 0 },
                .IUWB => .{ .ComplexUBitInt = 0 },
                else => unreachable,
            };

            try p.errStr(.bit_int_too_big, tokenIdx, specifier.toString(p.comp.langOpts).?);
            return error.ParsingFailed;
        }

        if (bitsNeeded > 64)
            return p.todo("_BitInt constants > 64 bits");

        break :blk @intCast(bitsNeeded);
    };

    const val = c.to(u64) catch |e| switch (e) {
        error.NegativeIntoUnsigned => unreachable, // unary minus parsed elsewhere; we only see positive integers
        error.TargetTooSmall => unreachable, // Validated above but Todo: handle larger _BitInt
    };

    var res: Result = .{
        .value = Value.int(val),
        .ty = .{
            .specifier = .BitInt,
            .data = .{ .int = .{ .bits = bitsNeeded, .signedness = suffix.signedness() } },
        },
    };
    res.node = try p.addNode(.{ .tag = .IntLiteral, .type = res.ty, .data = undefined });
    if (!p.inMacro) try p.valueMap.put(res.node, res.value);
    return res;
}

fn getFracPart(p: *Parser, buffer: []const u8, prefix: NumberPrefix, tokenIdx: TokenIndex) ![]const u8 {
    if (buffer.len == 0 or buffer[0] != '.') return "";
    std.debug.assert(prefix != .octal);

    if (prefix == .binary) {
        try p.errStr(.invalid_int_suffix, tokenIdx, buffer);
        return error.ParsingFailed;
    }

    for (buffer, 0..) |c, idx| {
        if (idx == 0) continue;
        if (c == '\'') continue;
        if (!prefix.digitAllowed(c)) return buffer[0..idx];
    }

    return buffer;
}

fn getExponent(p: *Parser, buffer: []const u8, prefix: NumberPrefix, tokenIdx: TokenIndex) ![]const u8 {
    if (buffer.len == 0) return "";

    switch (buffer[0]) {
        'e', 'E' => std.debug.assert(prefix == .decimal),
        'p', 'P' => if (prefix != .hex) {
            try p.errStr(.invalid_float_suffix, tokenIdx, buffer);
            return error.ParsingFailed;
        },
        else => return "",
    }

    const end = for (buffer, 0..) |c, idx| {
        if (idx == 0) continue;
        if (idx == 1 and (c == '+' or c == '-')) continue;
        switch (c) {
            '0'...'9' => {},
            '\'' => continue,
            else => break idx,
        }
    } else buffer.len;

    const exponent = buffer[0..end];
    if (std.mem.indexOfAny(u8, exponent, "0123456789") == null) {
        try p.errToken(.exponent_has_no_digits, tokenIdx);
        return error.ParsingFailed;
    }
    return exponent;
}

/// Using an explicit `tokenIdx` parameter instead of `p.tokenIdx` makes it easier
/// to parse numbers in pragma handlers.
pub fn parseNumberToken(p: *Parser, tokenIdx: TokenIndex) !Result {
    const buffer = p.getTokenText(tokenIdx);
    const prefix = NumberPrefix.fromString(buffer);
    const afterPrefix = buffer[prefix.stringLen()..];

    const intPart = try p.getIntegerPart(afterPrefix, prefix, tokenIdx);
    const afterInt = afterPrefix[intPart.len..];

    const frac = try p.getFracPart(afterInt, prefix, tokenIdx);
    const afterFrac = afterInt[frac.len..];

    const exponent = try p.getExponent(afterFrac, prefix, tokenIdx);
    const suffixStr = afterFrac[exponent.len..];
    const isFloat = (exponent.len > 0 or frac.len > 0);
    const suffix = NumberSuffix.fromString(suffixStr, if (isFloat) .float else .int) orelse {
        if (isFloat)
            try p.errStr(.invalid_float_suffix, tokenIdx, suffixStr)
        else
            try p.errStr(.invalid_int_suffix, tokenIdx, suffixStr);
        return error.ParsingFailed;
    };

    if (isFloat) {
        std.debug.assert(prefix == .hex or prefix == .decimal);
        if (prefix == .hex and exponent.len == 0) {
            try p.errToken(.hex_floating_constant_requires_exponent, tokenIdx);
            return error.ParsingFailed;
        }
        const number = buffer[0 .. buffer.len - suffixStr.len];
        return p.parseFloat(number, suffix);
    } else {
        return p.parseInt(prefix, intPart, suffix, tokenIdx);
    }
}

fn parsePPNumber(p: *Parser) Error!Result {
    defer p.tokenIdx += 1;
    const res = try p.parseNumberToken(p.tokenIdx);
    if (p.inMacro) {
        if (res.ty.isFloat() or !res.ty.isReal()) {
            try p.errToken(.float_literal_in_pp_expr, p.tokenIdx);
            return error.ParsingFailed;
        }
    } else {
        try p.valueMap.put(res.node, res.value);
    }
    return res;
}

fn parseCharLiteral(p: *Parser) Error!Result {
    defer p.tokenIdx += 1;
    const ty: Type = switch (p.getCurrToken()) {
        .CharLiteral => Type.Int,
        .CharLiteralWide => p.comp.types.wchar,
        .CharLiteralUTF_16 => Type.UShort,
        .CharLiteralUTF_32 => Type.ULong,
        else => unreachable,
    };

    const max: u32 = switch (p.getCurrToken()) {
        .CharLiteral => std.math.maxInt(u8),
        .CharLiteralWide => @intCast(p.comp.types.wchar.maxInt(p.comp)),
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
    var slice = p.getTokenText(p.tokenIdx);
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

        const product, const overflowed = @mulWithOverflow(val, max);
        if (overflowed != 0 and !overflowReported) {
            try p.errExtra(.char_lit_too_wide, p.tokenIdx, .{ .unsigned = i });
            overflowReported = true;
        }
        val = product + c;
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
        var slice = p.getTokenText(start);
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
    arrayType.* = .{ .elem = Type.Char, .len = slice.len };

    var res: Result = .{
        .ty = .{ .specifier = .Array, .data = .{ .array = arrayType } },
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
        const product, const overflowed = @mulWithOverflow(char, base);
        if (overflowed != 0 and !reported) {
            try p.errExtra(.escape_sequence_overflow, tok, .{ .unsigned = i.* });
            reported = true;
        }
        char = product + val;
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
    const toWrite = std.unicode.utf8Encode(c, &buf) catch unreachable; // validated above
    p.strings.appendSliceAssumeCapacity(buf[0..toWrite]);
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
    const controllingToken = p.tokenIdx;
    const controlling = try p.parseNoEval(parseAssignExpr);
    _ = try p.expectToken(.Comma);

    const listBufferTop = p.listBuffer.items.len;
    defer p.listBuffer.items.len = listBufferTop;

    try p.listBuffer.append(controlling.node);

    const declBufferTop = p.declBuffer.items.len;
    defer p.declBuffer.items.len = declBufferTop;

    var defaultToken: ?TokenIndex = null;
    var default: Result = undefined;
    var chosenToken: TokenIndex = undefined;
    var chosen: Result = .{};

    while (true) {
        const start = p.tokenIdx;
        if (try p.parseTypeName()) |ty| blk: {
            if (ty.isArray()) {
                try p.errToken(.generic_array_type, start);
            } else if (ty.isFunc()) {
                try p.errToken(.generic_func_type, start);
            } else if (ty.containAnyQual()) {
                try p.errToken(.generic_qual_type, start);
            }

            _ = try p.expectToken(.Colon);
            const node = try p.parseAssignExpr();
            try node.expect(p);

            if (ty.eql(controlling.ty, p.comp, false)) {
                if (chosen.node == .none) {
                    chosen = node;
                    chosenToken = start;
                    break :blk;
                }
                try p.errStr(.generic_duplicate, start, try p.typeStr(ty));
                try p.errStr(.generic_duplicate_here, chosenToken, try p.typeStr(ty));
            }

            for (p.listBuffer.items[listBufferTop + 1 ..], p.declBuffer.items[declBufferTop..]) |item, prevToken| {
                const prevTy = p.nodes.items(.type)[@intFromEnum(item)];
                if (prevTy.eql(ty, p.comp, true)) {
                    try p.errStr(.generic_duplicate, start, try p.typeStr(ty));
                    try p.errStr(.generic_duplicate_here, @intFromEnum(prevToken), try p.typeStr(ty));
                }
            }

            try p.listBuffer.append(try p.addNode(.{
                .tag = .GenericAssociationExpr,
                .type = ty,
                .data = .{ .unExpr = node.node },
            }));
            try p.declBuffer.append(@enumFromInt(start));
        } else if (p.eat(.KeywordDefault)) |tok| {
            if (defaultToken) |prev| {
                try p.errToken(.generic_duplicate_default, tok);
                try p.errToken(.previous_case, prev);
            }

            defaultToken = tok;
            _ = try p.expectToken(.Colon);
            default = try p.parseAssignExpr();
            try default.expect(p);
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
    if (chosen.node == .none) {
        if (defaultToken != null) {
            const node = try p.addNode(.{ .tag = .GenericDefaultExpr, .data = .{ .unExpr = default.node } });
            try p.listBuffer.insert(listBufferTop + 1, node);
            chosen = default;
        } else {
            try p.errStr(.generic_no_match, controllingToken, try p.typeStr(controlling.ty));
            return error.ParsingFailed;
        }
    } else {
        const node = try p.addNode(.{ .tag = .GenericAssociationExpr, .data = .{ .unExpr = chosen.node } });
        try p.listBuffer.insert(listBufferTop + 1, node);
        if (defaultToken != null)
            try p.listBuffer.append(try p.addNode(.{ .tag = .GenericDefaultExpr, .data = .{ .unExpr = chosen.node } }));
    }

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
