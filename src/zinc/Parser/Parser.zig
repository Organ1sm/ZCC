const std = @import("std");
const assert = std.debug.assert;
const big = std.math.big;
const Compilation = @import("../Basic/Compilation.zig");
const Source = @import("../Basic/Source.zig");
const TokenType = @import("../Basic/TokenType.zig").TokenType;
const Lexer = @import("../Lexer/Lexer.zig");
const Preprocessor = @import("../Lexer/Preprocessor.zig");
const AST = @import("../AST/AST.zig");
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
const TextLiteral = @import("../Parser/TextLiteral.zig");
const Value = @import("../AST/Value.zig");
const StringId = @import("../Basic/StringInterner.zig").StringId;
const RecordLayout = @import("../Basic/RecordLayout.zig");
const Builtins = @import("../Builtins.zig");
const Builtin = Builtins.Builtin;

const Token = AST.Token;
const NumberPrefix = Token.NumberPrefix;
const NumberSuffix = Token.NumberSuffix;
const TokenIndex = AST.TokenIndex;
const Node = AST.Node;
const Allocator = std.mem.Allocator;
const NodeList = std.ArrayList(Node.Index);

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

tree: AST,
// references tree.arena
arena: Allocator,

// buffers used during compilation
symStack: SymbolStack = .{},
strings: std.ArrayList(u8),
labels: std.ArrayList(Label),
listBuffer: NodeList,
declBuffer: NodeList,
paramBuffer: std.ArrayList(Type.Function.Param),
enumBuffer: std.ArrayList(Type.Enum.Field),
recordBuffer: std.ArrayList(Type.Record.Field),
attrBuffer: std.MultiArrayList(TentativeAttribute) = .{},
attrApplicationBuffer: std.ArrayListUnmanaged(Attribute) = .{},
fieldAttrBuffer: std.ArrayList([]const Attribute),
/// type name -> variable name location for tentative definitions (top-level defs with thus-far-incomplete types)
/// e.g. `struct Foo bar;` where `struct Foo` is not defined yet.
/// The key is the StringId of `Foo` and the value is the TokenIndex of `bar`
/// Items are removed if the type is subsequently completed with a definition.
/// We only store the first tentative definition that uses a given type because this map is only used
/// for issuing an error message, and correcting the first error for a type will fix all of them for that type.
tentativeDefs: std.AutoHashMapUnmanaged(StringId, TokenIndex) = .{},

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
    file: StringId,
    jmpBuf: StringId,
    sigJmpBuf: StringId,
    ucontextTy: StringId,
},

const Label = union(enum) {
    unresolvedGoto: TokenIndex,
    label: TokenIndex,
};

fn checkIdentifierCodepointWarnings(comp: *Compilation, codepoint: u21, loc: Source.Location) Compilation.Error!bool {
    assert(codepoint >= 0x80);

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
    assert(p.currToken() == .ExtendedIdentifier);

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
    switch (p.currToken()) {
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
        if (p.currToken() == .Invalid and p.getTokenText(p.tokenIdx)[0] == '$') {
            try p.err(.dollars_in_identifiers);
            p.tokenIdx += 1;
            return error.ParsingFailed;
        }
    }

    return p.tokenIdx - 1;
}

fn expectIdentifier(p: *Parser) Error!TokenIndex {
    const actual = p.currToken();
    if (actual != .Identifier and actual != .ExtendedIdentifier)
        return p.errExpectedToken(.Identifier, actual);

    return (try p.eatIdentifier()) orelse error.ParsingFailed;
}

fn eat(p: *Parser, expected: TokenType) ?TokenIndex {
    assert(expected != .Identifier and expected != .ExtendedIdentifier); // use eatIdentifier
    if (p.currToken() == expected) {
        defer p.tokenIdx += 1;
        return p.tokenIdx;
    } else return null;
}

pub fn currToken(p: *Parser) TokenType {
    return p.lookAhead(0);
}

pub fn lookAhead(p: *Parser, n: u32) TokenType {
    assert(p.tokenIdx + n < p.tokenIds.len);
    return p.tokenIds[p.tokenIdx + n];
}

fn expectToken(p: *Parser, expected: TokenType) Error!TokenIndex {
    assert(expected != .Identifier and expected != .ExtendedIdentifier); // use eatIdentifier
    const actual = p.currToken();
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

fn errOverflow(p: *Parser, opToken: TokenIndex, res: Result) !void {
    try p.errStr(.overflow, opToken, try res.str(p));
}

pub fn getTokenText(p: *Parser, index: TokenIndex) []const u8 {
    if (p.tokenIds[index].lexeme()) |some|
        return some;

    const loc = p.pp.tokens.items(.loc)[index];
    var lexer = Lexer{
        .buffer = p.comp.getSource(loc.id).buffer,
        .langOpts = p.comp.langOpts,
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
    @branchHint(.cold);
    return p.errExtra(tag, index, .{ .str = str });
}

pub fn errExtra(p: *Parser, tag: Diagnostics.Tag, tokIdx: TokenIndex, extra: Diagnostics.Message.Extra) Compilation.Error!void {
    @branchHint(.cold);
    const token = p.pp.tokens.get(tokIdx);
    var loc = token.loc;

    if (tokIdx != 0 and token.is(.Eof)) {
        const prev = p.pp.tokens.get(tokIdx - 1);
        loc = prev.loc;
        loc.byteOffset += @intCast(p.getTokenText(tokIdx - 1).len);
    }

    try p.comp.addDiagnostic(
        .{ .tag = tag, .loc = loc, .extra = extra },
        p.pp.expansionSlice(tokIdx),
    );
}

pub fn errToken(p: *Parser, tag: Diagnostics.Tag, index: TokenIndex) Compilation.Error!void {
    @branchHint(.cold);
    return p.errExtra(tag, index, .{ .none = {} });
}

pub fn err(p: *Parser, tag: Diagnostics.Tag) Compilation.Error!void {
    @branchHint(.cold);
    return p.errExtra(tag, p.tokenIdx, .{ .none = {} });
}

pub fn todo(p: *Parser, msg: []const u8) Error {
    try p.errStr(.todo, p.tokenIdx, msg);
    return error.ParsingFailed;
}

pub fn removeNull(p: *Parser, str: Value) !Value {
    const stringsTop = p.strings.items.len;
    defer p.strings.items.len = stringsTop;
    {
        const bytes = p.comp.interner.get(str.ref()).bytes;
        try p.strings.appendSlice(bytes[0 .. bytes.len - 1]);
    }
    return Value.intern(p.comp, .{ .bytes = p.strings.items[stringsTop..] });
}

pub fn typeStr(p: *Parser, ty: Type) ![]const u8 {
    if (TypeBuilder.fromType(ty).toString(p.comp.langOpts)) |str| return str;
    const stringsTop = p.strings.items.len;
    defer p.strings.items.len = stringsTop;

    try ty.print(p.comp, p.strings.writer());
    return try p.comp.diagnostics.arena.allocator().dupe(u8, p.strings.items[stringsTop..]);
}

pub fn typePairStr(p: *Parser, a: Type, b: Type) ![]const u8 {
    return p.typePairStrExtra(a, " and ", b);
}

pub fn typePairStrExtra(p: *Parser, a: Type, msg: []const u8, b: Type) ![]const u8 {
    const stringsTop = p.strings.items.len;
    defer p.strings.items.len = stringsTop;

    try p.strings.append('\'');
    try a.print(p.comp, p.strings.writer());
    try p.strings.append('\'');
    try p.strings.appendSlice(msg);
    try p.strings.append('\'');
    try b.print(p.comp, p.strings.writer());
    try p.strings.append('\'');
    return try p.comp.diagnostics.arena.allocator().dupe(u8, p.strings.items[stringsTop..]);
}

pub fn floatValueChangedStr(p: *Parser, res: *Result, oldValue: Value, intTy: Type) ![]const u8 {
    const stringsTop = p.strings.items.len;
    defer p.strings.items.len = stringsTop;

    var w = p.strings.writer();
    const str = try p.typePairStrExtra(res.ty, " to ", intTy);
    try w.writeAll(str);

    try w.writeAll(" changes ");
    if (res.value.isZero(p.comp)) try w.writeAll("non-zero ");
    try w.writeAll("value from ");
    try oldValue.print(res.ty, p.comp, w);
    try w.writeAll(" to ");
    try res.value.print(intTy, p.comp, w);

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
        const msgStr = p.comp.interner.get(@"error".msg.ref()).bytes;
        try w.print("call to '{s}' declared with attribute error: {}", .{
            p.getTokenText(@"error".__name_token), std.zig.fmtEscapes(msgStr),
        });
        try w.print("call to '{s}' declared with attribute error: ", .{p.getTokenText(@"error".__name_token)});
        const str = try p.comp.diagnostics.arena.allocator().dupe(u8, p.strings.items[stringsTop..]);
        try p.errStr(.error_attribute, usageToken, str);
    }

    if (ty.getAttribute(.warning)) |warning| {
        const stringsTop = p.strings.items.len;
        defer p.strings.items.len = stringsTop;

        const w = p.strings.writer();
        const msgStr = p.comp.interner.get(warning.msg.ref()).bytes;
        try w.print("call to '{s}' declared with attribute error: {}", .{
            p.getTokenText(warning.__name_token), std.zig.fmtEscapes(msgStr),
        });
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
fn errDeprecated(p: *Parser, tag: Diagnostics.Tag, tokenIdx: TokenIndex, msg: ?Value) Compilation.Error!void {
    const stringsTop = p.strings.items.len;
    defer p.strings.items.len = stringsTop;

    const w = p.strings.writer();
    try w.print("'{s}' is ", .{p.getTokenText(tokenIdx)});
    const reason: []const u8 = switch (tag) {
        .unavailable => "unavailable",
        .deprecated_declarations => "deprecated",
        else => unreachable,
    };

    try w.writeAll(reason);
    if (msg) |m| {
        const str = p.comp.interner.get(m.ref()).bytes;
        try w.print(": {}", .{std.zig.fmtEscapes(str)});
    }

    const str = try p.comp.diagnostics.arena.allocator().dupe(u8, p.strings.items[stringsTop..]);
    return p.errStr(tag, tokenIdx, str);
}

pub fn addNode(p: *Parser, node: AST.Node) Allocator.Error!Node.Index {
    if (p.inMacro) return undefined;
    return p.tree.addNode(node);
}

fn addList(p: *Parser, nodes: []const Node.Index) Allocator.Error!AST.Range {
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

fn nodeIs(p: *Parser, node: Node.Index, comptime tag: std.meta.Tag(AST.Node)) bool {
    return p.getNode(node, tag) != null;
}

fn getNode(p: *Parser, node: Node.Index, comptime tag: std.meta.Tag(AST.Node)) ?@FieldType(Node, @tagName(tag)) {
    var cur = node;
    while (true) {
        switch (cur.get(&p.tree)) {
            .parenExpr => |un| cur = un.operand,
            tag => |data| return data,
            else => return null,
        }
    }
}

fn getInternString(p: *Parser, token: TokenIndex) !StringId {
    const name = p.getTokenText(token);
    return p.comp.internString(name);
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

/// Issue errors for top-level definitions whose type was never completed.
fn diagnoseIncompleteDefinitions(p: *Parser) !void {
    @branchHint(.cold);

    const errStart = p.comp.diagnostics.list.items.len;
    for (p.declBuffer.items) |declIndex| {
        const node = declIndex.get(&p.tree);
        const forward = switch (node) {
            .structForwardDecl, .unionForwardDecl, .enumForwardDecl => |forward| forward,
            else => continue,
        };

        const declTypeName = if (forward.containerType.getRecord()) |rec|
            rec.name
        else if (forward.containerType.get(.Enum)) |en|
            en.data.@"enum".name
        else
            unreachable;

        const tentativeDefToken = p.tentativeDefs.get(declTypeName) orelse continue;
        const tyStr = try p.typeStr(forward.containerType);
        try p.errStr(.tentative_definition_incomplete, tentativeDefToken, tyStr);
        try p.errStr(.forward_declaration_here, forward.nameOrKindToken, tyStr);
    }

    const errorsAdded = p.comp.diagnostics.list.items.len - errStart;
    assert(errorsAdded == 2 * p.tentativeDefs.count()); // Each tentative def should add an error + note
}

fn addImplicitTypedef(p: *Parser, name: []const u8, ty: Type) !void {
    const start = p.comp.generatedBuffer.items.len;
    try p.comp.generatedBuffer.appendSlice(p.comp.gpa, name);
    try p.comp.generatedBuffer.append(p.comp.gpa, '\n');

    const nameToken: u32 = @intCast(p.pp.tokens.len);
    try p.pp.tokens.append(p.gpa, .{ .id = .Identifier, .loc = .{
        .id = .generated,
        .byteOffset = @intCast(start),
        .line = p.pp.generatedLine,
    } });
    p.pp.generatedLine += 1;

    // Reset in case there was an allocation.
    p.tree.tokens = p.pp.tokens.slice();

    const node = try p.addNode(.{
        .typedef = .{
            .nameToken = nameToken,
            .type = ty,
            .implicit = true,
        },
    });
    try p.symStack.defineTypedef(try p.comp.internString(name), ty, nameToken, node);
    try p.declBuffer.append(node);
}

/// root : (decl | inline assembly ';' | static-assert-declaration)*
pub fn parse(pp: *Preprocessor) Compilation.Error!AST {
    assert(pp.linemarkers == .None);
    pp.comp.pragmaEvent(.BeforeParse);

    var arena = std.heap.ArenaAllocator.init(pp.comp.gpa);
    errdefer arena.deinit();

    var p = Parser{
        .pp = pp,
        .comp = pp.comp,
        .gpa = pp.comp.gpa,
        .arena = undefined,
        .tokenIds = pp.tokens.items(.id),

        .tree = .{
            .comp = pp.comp,
            .tokens = pp.tokens.slice(),
            .arena = .init(pp.comp.gpa),
        },

        .labels = std.ArrayList(Label).init(pp.comp.gpa),
        .strings = std.ArrayList(u8).init(pp.comp.gpa),
        .listBuffer = NodeList.init(pp.comp.gpa),
        .declBuffer = NodeList.init(pp.comp.gpa),
        .paramBuffer = std.ArrayList(Type.Function.Param).init(pp.comp.gpa),
        .enumBuffer = std.ArrayList(Type.Enum.Field).init(pp.comp.gpa),
        .recordBuffer = std.ArrayList(Type.Record.Field).init(pp.comp.gpa),
        .fieldAttrBuffer = std.ArrayList([]const Attribute).init(pp.comp.gpa),
        .stringsIds = .{
            .declSpecId = try pp.comp.internString("__declspec"),
            .mainId = try pp.comp.internString("main"),
            .file = try pp.comp.internString("FILE"),
            .jmpBuf = try pp.comp.internString("jmp_buf"),
            .sigJmpBuf = try pp.comp.internString("sigjmp_buf"),
            .ucontextTy = try pp.comp.internString("ucontext_t"),
        },
    };

    p.arena = p.tree.arena.allocator();
    errdefer p.tree.deinit();

    defer {
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
        p.tentativeDefs.deinit(pp.comp.gpa);
        assert(p.fieldAttrBuffer.items.len == 0);
        p.fieldAttrBuffer.deinit();
    }

    //bind p to the symbol stack for simplify symbol stack api
    p.symStack.p = &p;
    try p.symStack.pushScope();
    defer p.symStack.popScope();

    {
        if (p.comp.langOpts.hasChar8_t())
            try p.addImplicitTypedef("char8_t", Type.UChar);

        try p.addImplicitTypedef("__int128_t", Type.Int128);
        try p.addImplicitTypedef("__uint128_t", Type.UInt128);

        const elemTy = try p.arena.create(Type);
        elemTy.* = Type.Char;
        try p.addImplicitTypedef("__builtin_ms_va_list", .{
            .specifier = .Pointer,
            .data = .{ .subType = elemTy },
        });

        const ty = &pp.comp.types.vaList;
        try p.addImplicitTypedef("__builtin_va_list", ty.*);

        if (ty.isArray()) ty.decayArray();

        try p.addImplicitTypedef("__NSConstantString", pp.comp.types.nsConstantString.ty);

        if (p.comp.float80Type()) |float80Ty| {
            try p.addImplicitTypedef("__float80", float80Ty);
        }
    }

    const implicitTypedefCount = p.declBuffer.items.len;

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

            switch (p.currToken()) {
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
        }) |node| {
            try p.declBuffer.append(node);
            continue;
        }

        if (p.eat(.Semicolon)) |tok| {
            try p.errToken(.extra_semi, tok);
            const empty = try p.addNode(.{ .emptyDecl = .{ .semicolon = tok } });
            try p.declBuffer.append(empty);
            continue;
        }

        try p.err(.expected_external_decl);
        p.tokenIdx += 1;
    }

    if (p.tentativeDefs.count() > 0)
        try p.diagnoseIncompleteDefinitions();

    p.tree.rootDecls = p.declBuffer.moveToUnmanaged();
    if (p.tree.rootDecls.items.len == implicitTypedefCount)
        try p.errToken(.empty_translation_unit, p.tokenIdx - 1);

    pp.comp.pragmaEvent(.AfterParse);

    return p.tree;
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
        switch (p.currToken()) {
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
            .KeywordTypeofUnqual,
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
        if (p.currToken() == .NewLine) return;
        if (p.currToken() == .Eof) {
            p.tokenIdx -= 1;
            return;
        }
    }
}

fn skipTo(p: *Parser, id: TokenType) void {
    var parens: u32 = 0;
    while (true) : (p.tokenIdx += 1) {
        if (p.currToken() == id and parens == 0) {
            p.tokenIdx += 1;
            return;
        }
        switch (p.currToken()) {
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

/// Called after a typedef is defined
fn typedefDefined(p: *Parser, name: StringId, ty: Type) void {
    if (name == p.stringsIds.file) {
        p.comp.types.file = ty;
    } else if (name == p.stringsIds.jmpBuf) {
        p.comp.types.jmpBuffer = ty;
    } else if (name == p.stringsIds.sigJmpBuf) {
        p.comp.types.sigjmpBuffer = ty;
    } else if (name == p.stringsIds.ucontextTy) {
        p.comp.types.ucontextTy = ty;
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
    const firstToken = p.tokenIdx;

    const attrBufferTop = p.attrBuffer.len;
    defer p.attrBuffer.len = attrBufferTop;

    try p.parseAttrSpec();

    var declSpec = if (try p.parseDeclSpec()) |some| some else blk: {
        if (p.func.type != null) {
            p.tokenIdx = firstToken;
            return false;
        }

        switch (p.tokenIds[firstToken]) {
            .Asterisk, .LParen, .Identifier => {},
            else => if (p.tokenIdx != firstToken) {
                try p.err(.expected_ident_or_l_paren);
                return error.ParsingFailed;
            } else return false,
        }

        var spec: TypeBuilder = .{};
        break :blk DeclSpec{ .type = try spec.finish(p) };
    };

    if (declSpec.noreturn) |token| {
        const attr = Attribute{ .tag = .noreturn, .args = .{ .noreturn = .{} }, .syntax = .keyword };
        try p.attrBuffer.append(p.gpa, .{ .attr = attr, .tok = token });
    }

    var declNode = try p.tree.addNode(.{ .emptyDecl = .{ .semicolon = firstToken } });
    var initDeclarator = (try p.parseInitDeclarator(&declSpec, attrBufferTop, declNode)) orelse {
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

        try p.errToken(.missing_declaration, firstToken);
        return true;
    };

    // check for funtion definition
    if (initDeclarator.d.funcDeclarator != null and
        initDeclarator.initializer == null and
        initDeclarator.d.type.isFunc())
    fndef: {
        if (declSpec.autoType) |tokenIdx| {
            try p.errStr(.auto_type_not_allowed, tokenIdx, "function return type");
            return error.ParsingFailed;
        }

        switch (p.currToken()) {
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

        const internedDeclaratorName = try p.getInternString(initDeclarator.d.name);
        try p.symStack.defineSymbol(internedDeclaratorName, initDeclarator.d.type, initDeclarator.d.name, declNode, .{}, false);

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

                    var d = (try p.declarator(paramDeclSpec.type, .param)) orelse {
                        try p.errToken(.missing_declaration, firstToken);
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
                    const internedName = try p.comp.internString(name);
                    for (initDeclarator.d.type.getParams()) |*param| {
                        if (param.name == internedName) {
                            param.ty = d.type;
                            break;
                        }
                    } else {
                        try p.errStr(.parameter_missing, d.name, name);
                    }

                    d.type = try Attribute.applyParameterAttributes(p, d.type, attrBufferTop, .alignas_on_param);

                    try paramDeclSpec.validateParam(p, &d.type);
                    const paramNode = try p.addNode(.{
                        .param = .{
                            .nameToken = d.name,
                            .type = d.type,
                            .storageClass = switch (paramDeclSpec.storageClass) {
                                .none => .auto,
                                .register => .register,
                                else => .auto, // Error reported in `validateParam`
                            },
                        },
                    });

                    // bypass redefinition check to avoid duplicate errors
                    try p.symStack.define(.{
                        .kind = .definition,
                        .name = internedName,
                        .token = d.name,
                        .type = d.type,
                        .value = .{},
                        .node = .pack(paramNode),
                    });
                    if (p.eat(.Comma) == null) break;
                }

                _ = try p.expectToken(.Semicolon);
            }
        } else {
            for (initDeclarator.d.type.getParams()) |param| {
                if (param.ty.hasUnboundVLA())
                    try p.errToken(.unbound_vla, param.nameToken);
                if (param.ty.hasIncompleteSize() and !param.ty.is(.Void) and !param.ty.is(.Invalid))
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
                    .node = param.node,
                });
            }
        }

        const body = (try p.parseCompoundStmt(true, null)) orelse {
            assert(initDeclarator.d.oldTypeFunc != null);
            try p.err(.expected_fn_body);
            return true;
        };

        try declSpec.validateFnDef(p);

        try p.tree.setNode(@intFromEnum(declNode), .{
            .fnDef = .{
                .nameToken = initDeclarator.d.name,
                .@"inline" = declSpec.@"inline" != null,
                .static = declSpec.storageClass == .static,
                .type = initDeclarator.d.type,
                .body = body,
            },
        });
        try p.declBuffer.append(declNode);

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
    var warnedAuto = false;
    while (true) {
        if (initDeclarator.d.oldTypeFunc) |tokenIdx|
            try p.errToken(.invalid_old_style_params, tokenIdx);

        try declSpec.validate(p, &initDeclarator.d.type);
        try p.tree.setNode(@intFromEnum(declNode), if (declSpec.storageClass == .typedef)
            .{
                .typedef = .{
                    .nameToken = initDeclarator.d.name,
                    .type = initDeclarator.d.type,
                    .implicit = false,
                },
            }
        else if (initDeclarator.d.type.isFunc())
            .{
                .fnProto = .{
                    .nameToken = initDeclarator.d.name,
                    .type = initDeclarator.d.type,
                    .static = declSpec.storageClass == .static,
                    .@"inline" = declSpec.@"inline" != null,
                    .definition = null,
                },
            }
        else
            .{
                .variable = .{
                    .nameToken = initDeclarator.d.name,
                    .type = initDeclarator.d.type,
                    .storageClass = switch (declSpec.storageClass) {
                        .auto => .auto,
                        .register => .register,
                        .static => .static,
                        .@"extern" => if (initDeclarator.initializer == null) .@"extern" else .auto,
                        else => .auto, // Error reported in `validate`
                    },
                    .threadLocal = declSpec.threadLocal != null,
                    .implicit = false,
                    .initializer = if (initDeclarator.initializer) |some| some.node else null,
                },
            });
        try p.declBuffer.append(declNode);

        const internedName = try p.getInternString(initDeclarator.d.name);
        if (declSpec.storageClass == .typedef) {
            try p.symStack.defineTypedef(internedName, initDeclarator.d.type, initDeclarator.d.name, declNode);
            p.typedefDefined(internedName, initDeclarator.d.type);
        } else if (initDeclarator.initializer) |init| {
            // TODO validate global variable/constexpr initializer comptime known
            try p.symStack.defineSymbol(
                internedName,
                initDeclarator.d.type,
                initDeclarator.d.name,
                declNode,
                if (initDeclarator.d.type.isConst()) init.value else .{},
                declSpec.constexpr != null,
            );
        } else if (p.func.type != null and declSpec.storageClass != .@"extern") {
            try p.symStack.defineSymbol(internedName, initDeclarator.d.type, initDeclarator.d.name, declNode, .{}, false);
        } else {
            try p.symStack.declareSymbol(internedName, initDeclarator.d.type, initDeclarator.d.name, declNode);
        }

        if (p.eat(.Comma) == null)
            break;

        if (!warnedAuto) {
            if (declSpec.autoType) |tokIdx| {
                try p.errToken(.auto_type_requires_single_declarator, tokIdx);
                warnedAuto = true;
            }
            if (p.comp.langOpts.standard.atLeast(.c23) and declSpec.storageClass == .auto) {
                try p.errToken(.c23_auto_single_declarator, declSpec.storageClass.auto);
                warnedAuto = true;
            }
        }

        declNode = try p.tree.addNode(.{ .emptyDecl = .{ .semicolon = p.tokenIdx - 1 } });
        initDeclarator = (try p.parseInitDeclarator(&declSpec, attrBufferTop, declNode)) orelse {
            try p.err(.expected_ident_or_l_paren);
            continue;
        };
    }

    _ = try p.expectToken(.Semicolon);
    return true;
}

fn staticAssertMessage(p: *Parser, condNode: Node.Index, maybeMessage: ?Result) !?[]const u8 {
    var buf = std.ArrayList(u8).init(p.gpa);
    defer buf.deinit();

    const cond = condNode.get(&p.tree);
    if (cond == .builtinTypesCompatibleP) {
        try buf.appendSlice("'__builtin_types_compatible_p(");

        const lhsTy = cond.builtinTypesCompatibleP.lhs;
        try lhsTy.print(p.comp, buf.writer());
        try buf.appendSlice(", ");

        const rhsTy = cond.builtinTypesCompatibleP.rhs;
        try rhsTy.print(p.comp, buf.writer());

        try buf.appendSlice(")'");
    } else if (maybeMessage == null) return null;

    if (maybeMessage) |message| {
        assert(message.node.get(&p.tree) == .stringLiteralExpr);

        if (buf.items.len > 0)
            try buf.append(' ');

        const bytes = p.comp.interner.get(message.value.ref()).bytes;
        try buf.ensureUnusedCapacity(bytes.len);
        try Value.printString(bytes, message.ty, p.comp, buf.writer());
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
        switch (p.currToken()) {
            .StringLiteral,
            .StringLiteralUTF_8,
            .StringLiteralUTF_16,
            .StringLiteralUTF_32,
            .StringLiteralWide,
            .UnterminatedStringLiteral,
            => try p.parseStringLiteral(),

            else => {
                try p.err(.expected_str_literal);
                return error.ParsingFailed;
            },
        }
    else
        null;

    try p.expectClosing(lp, .RParen);
    _ = try p.expectToken(.Semicolon);
    if (str == null) {
        try p.errToken(.static_assert_missing_message, staticAssert);
        try p.errStr(.pre_c23_compat, staticAssert, "'_Static_assert' with no message");
    }

    // Array will never be zero; a value of zero for a pointer is a null pointer constant
    if ((res.ty.isArray() or res.ty.isPointer()) and !res.value.isZero(p.comp)) {
        const errStart = p.comp.diagnostics.list.items.len;
        try p.errToken(.const_decl_folded, resToken);
        if (res.ty.isPointer() and errStart != p.comp.diagnostics.list.items.len) {
            // Don't show the note if the .const_decl_folded diagnostic was not added
            try p.errToken(.constant_expression_conversion_not_allowed, resToken);
        }
    }

    try res.boolCast(p, Type.Bool, resToken);
    if (res.value.isNone()) {
        if (!res.ty.isInvalid())
            try p.errToken(.static_assert_not_constant, resToken);
    } else {
        if (!res.value.toBool(p.comp)) {
            if (try p.staticAssertMessage(resNode, str)) |message|
                try p.errStr(.static_assert_failure_message, staticAssert, message)
            else
                try p.errToken(.static_assert_failure, staticAssert);
        }
    }

    const node = try p.addNode(.{
        .staticAssert = .{
            .assertToken = staticAssert,
            .cond = res.node,
            .message = if (str) |some| some.node else null,
        },
    });
    try p.declBuffer.append(node);
    return true;
}

/// typeof
///   : `typeof` '(' type-name ')'
///   | `typeof` '(' expr ')'
fn typeof(p: *Parser) Error!?Type {
    var unqual = false;
    switch (p.currToken()) {
        .KeywordTypeof, .KeywordTypeof1, .KeywordTypeof2 => p.tokenIdx += 1,
        .KeywordTypeofUnqual => {
            p.tokenIdx += 1;
            unqual = true;
        },
        else => return null,
    }

    const lp = try p.expectToken(.LParen);
    if (try p.parseTypeName()) |ty| {
        try p.expectClosing(lp, .RParen);
        const typeofType = try p.arena.create(Type);
        typeofType.* = .{
            .data = ty.data,
            .qual = if (unqual) .{} else ty.qual,
            .specifier = ty.specifier,
        };

        return Type{ .data = .{ .subType = typeofType }, .specifier = .TypeofType };
    }

    const typeofExpr = try p.parseNoEval(parseExpr);
    try p.expectClosing(lp, .RParen);

    if (typeofExpr.ty.is(.NullPtrTy))
        return Type{
            .specifier = .NullPtrTy,
            .qual = if (unqual) .{} else typeofExpr.ty.qual,
        };

    const inner = try p.arena.create(Type.Expr);
    inner.* = .{
        .node = typeofExpr.node,
        .ty = .{
            .data = typeofExpr.ty.data,
            .qual = if (unqual) .{} else typeofExpr.ty.qual,
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
/// funcSpec : keyword_inline | keyword_noreturn
fn parseDeclSpec(p: *Parser) Error!?DeclSpec {
    var d: DeclSpec = .{ .type = .{ .specifier = undefined } };
    var spec: TypeBuilder = .{};

    var combinedAuto = !p.comp.langOpts.standard.atLeast(.c23);
    const start = p.tokenIdx;
    while (true) {
        if (!combinedAuto and d.storageClass == .auto) {
            try spec.combine(p, .C23Auto, d.storageClass.auto);
            combinedAuto = true;
        }

        if (try p.parseStorageClassSpec(&d)) continue;
        if (try p.parseTypeSpec(&spec)) continue;

        const id = p.currToken();
        switch (id) {
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
    if (p.tokenIdx == start) return null;

    d.type = try spec.finish(p);
    d.autoType = spec.autoTypeToken;
    return d;
}

/// storage-class-specifier:
///  | `auto`
///  | `constexpr`
///  | `extern`
///  | `register`
///  | `static`
///  | `threadlocal`
///  : `typedef`
fn parseStorageClassSpec(p: *Parser, d: *DeclSpec) Error!bool {
    const start = p.tokenIdx;
    while (true) {
        const token = p.currToken();
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
                        else => try p.errStr(.cannot_combine_spec, p.tokenIdx, token.lexeme().?),
                    }
                    if (d.constexpr) |tok|
                        try p.errStr(.cannot_combine_spec, p.tokenIdx, p.tokenIds[tok].lexeme().?);
                }

                if (d.constexpr != null) {
                    switch (token) {
                        .KeywordAuto, .KeywordRegister, .KeywordStatic => {},
                        else => try p.errStr(.cannot_combine_spec, p.tokenIdx, token.lexeme().?),
                    }
                    if (d.threadLocal) |tok|
                        try p.errStr(.cannot_combine_spec, p.tokenIdx, p.tokenIds[tok].lexeme().?);
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
                    try p.errStr(.duplicate_declspec, p.tokenIdx, token.lexeme().?);

                if (d.constexpr) |tok|
                    try p.errStr(.cannot_combine_spec, p.tokenIdx, p.tokenIds[tok].lexeme().?);

                switch (d.storageClass) {
                    .@"extern", .none, .static => {},
                    else => try p.errStr(.cannot_combine_spec, p.tokenIdx, @tagName(d.storageClass)),
                }

                d.threadLocal = p.tokenIdx;
            },

            .KeywordConstexpr => {
                if (d.constexpr != null)
                    try p.errStr(.duplicate_declspec, p.tokenIdx, token.lexeme().?);

                if (d.threadLocal) |tok|
                    try p.errStr(.cannot_combine_spec, p.tokenIdx, p.tokenIds[tok].lexeme().?);

                switch (d.storageClass) {
                    .auto, .register, .none, .static => {},
                    else => try p.errStr(.cannot_combine_spec, p.tokenIdx, @tagName(d.storageClass)),
                }

                d.constexpr = p.tokenIdx;
            },

            else => break,
        }

        p.tokenIdx += 1;
    }

    return p.tokenIdx != start;
}

/// attribute
///  : attrIdentifier
///  | attrIdentifier '(' identifier ')'
///  | attrIdentifier '(' identifier (',' expr)+ ')'
///  | attrIdentifier '(' (expr (',' expr)*)? ')'
fn attribute(p: *Parser, kind: Attribute.Kind, namespace: ?[]const u8) Error!?TentativeAttribute {
    const nameToken = p.tokenIdx;
    switch (p.currToken()) {
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

    switch (p.currToken()) {
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
    const argExpr = try p.expect(parseAssignExpr);
    if (try p.diagnose(attr, arguments, argIdx, argExpr)) |msg| {
        try p.errExtra(msg.tag, argStart, msg.extra);
        p.skipTo(.RParen);
        return error.ParsingFailed;
    }
}

fn diagnose(p: *Parser, attr: Attribute.Tag, arguments: *Attribute.Arguments, argIdx: u32, res: Result) !?Diagnostics.Message {
    if (Attribute.wantsAlignment(attr, argIdx))
        return Attribute.diagnoseAlignment(attr, arguments, argIdx, res, p);

    return Attribute.diagnose(attr, arguments, argIdx, res, res.node.get(&p.tree), p);
}

fn handleAttr(p: *Parser, format: Attribute.Kind, namespace: ?[]const u8) Error!void {
    if (try p.attribute(format, namespace)) |attr|
        try p.attrBuffer.append(p.gpa, attr);
}

/// attribute-list : (attribute (',' attribute)*)?
fn parseGNUAttrList(p: *Parser) Error!void {
    if (p.currToken() == .RParen)
        return;

    try p.handleAttr(.gnu, null);
    while (p.currToken() != .RParen) {
        _ = try p.expectToken(.Comma);
        try p.handleAttr(.gnu, null);
    }
}

fn parseC23AttrList(p: *Parser) Error!void {
    while (p.currToken() != .RBracket) { // ']'
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
    while (p.currToken() != .RParen) {
        try p.handleAttr(.declspec, null);
        _ = p.eat(.Comma); // ','
    }
}

/// '[[' c23-attribute-list  ']]'
fn c23Attribute(p: *Parser) !bool {
    if (!p.comp.langOpts.standard.atLeast(.c23)) return false;
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
    switch (p.currToken()) {
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

const InitDeclarator = struct { d: Declarator, initializer: ?Result = null };

/// init-declarator : declarator assembly? attribute-specifier? ('=' initializer)?
fn parseInitDeclarator(p: *Parser, declSpec: *DeclSpec, attrBufferTop: usize, declNode: Node.Index) Error!?InitDeclarator {
    const thisAttrBufferTop = p.attrBuffer.len;
    defer p.attrBuffer.len = thisAttrBufferTop;

    var ID = InitDeclarator{ .d = (try p.declarator(declSpec.type, .normal)) orelse return null };

    if (declSpec.type.is(.C23Auto) and !ID.d.type.is(.C23Auto)) {
        try p.errToken(.c23_auto_plain_declarator, declSpec.storageClass.auto);
        return error.ParsingFailed;
    }

    try p.attributeSpecifier(ID.d.name);
    _ = try p.parseAssembly(.declLabel);
    try p.attributeSpecifier(ID.d.name);

    var applyVarAttributes = false;
    if (declSpec.storageClass == .typedef) {
        if (declSpec.autoType) |tokIndex| {
            try p.errStr(.auto_type_not_allowed, tokIndex, "typedef");
            return error.ParsingFailed;
        }
        ID.d.type = try Attribute.applyTypeAttributes(p, ID.d.type, attrBufferTop, null);
    } else if (ID.d.type.isFunc()) {
        ID.d.type = try Attribute.applyFunctionAttributes(p, ID.d.type, attrBufferTop);
    } else {
        applyVarAttributes = true;
    }

    if (p.eat(.Equal)) |eq| init: {
        if (declSpec.storageClass == .typedef or (ID.d.funcDeclarator != null and ID.d.type.isFunc()))
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

        if (p.currToken() == .LBrace and ID.d.type.is(.C23Auto)) {
            try p.errToken(.c23_auto_scalar_init, declSpec.storageClass.auto);
            return error.ParsingFailed;
        }

        try p.symStack.pushScope();
        defer p.symStack.popScope();

        const internedName = try p.getInternString(ID.d.name);
        try p.symStack.declareSymbol(internedName, ID.d.type, ID.d.name, declNode);

        var initListExpr = try p.initializer(ID.d.type);
        ID.initializer = initListExpr;
        // int j [] = c; // c -> *int
        if (!initListExpr.ty.isArray()) break :init;
        if (ID.d.type.specifier == .IncompleteArray) {
            ID.d.type.data.array.len = initListExpr.ty.arrayLen() orelse break :init;
            ID.d.type.specifier = .Array;
        }
    }

    const c23Auto = ID.d.type.is(.C23Auto);
    const name = ID.d.name;
    if (ID.d.type.is(.AutoType) or c23Auto) {
        if (ID.initializer) |some| {
            ID.d.type.specifier = some.ty.specifier;
            ID.d.type.data = some.ty.data;
            ID.d.type.decayed = some.ty.decayed;
        } else {
            ID.d.type = Type.Invalid;
            if (c23Auto)
                try p.errStr(.c23_auto_requires_initializer, declSpec.storageClass.auto, p.getTokenText(name))
            else
                try p.errStr(.auto_type_requires_initializer, name, p.getTokenText(name));
            return ID;
        }
    }

    if (applyVarAttributes)
        ID.d.type = try Attribute.applyVariableAttributes(p, ID.d.type, attrBufferTop, null);

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
        if (ID.initializer != null) break :incomplete;

        if (p.func.type == null) {
            if (specifier == .IncompleteArray) {
                try p.errStr(.tentative_array, name, try p.typeStr(ID.d.type));
                break :incomplete;
            } else if (ID.d.type.getRecord()) |record| {
                _ = try p.tentativeDefs.getOrPutValue(p.gpa, record.name, ID.d.name);
                break :incomplete;
            } else if (ID.d.type.get(.Enum)) |e| {
                _ = try p.tentativeDefs.getOrPutValue(p.gpa, e.data.@"enum".name, ID.d.name);
                break :incomplete;
            }
        }
        try p.errStr(.variable_incomplete_ty, name, try p.typeStr(ID.d.type));
    }

    return ID;
}

/// type-specifier
///  : `void`
///  | `auto_type`
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

        switch (p.currToken()) {
            .KeywordVoid => try ty.combine(p, .Void, p.tokenIdx),
            .KeywordAutoType => {
                try p.errToken(.auto_type_extension, p.tokenIdx);
                try ty.combine(p, .AutoType, p.tokenIdx);
            },
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
            .KeywordFloat16 => try ty.combine(p, .Float16, p.tokenIdx),
            .KeywordFloat => try ty.combine(p, .Float, p.tokenIdx),
            .KeywordDouble => try ty.combine(p, .Double, p.tokenIdx),
            .KeywordComplex => try ty.combine(p, .Complex, p.tokenIdx),
            .KeywordFloat128_, .KeywordFloat128__ => {
                if (!p.comp.hasFloat128())
                    try p.errStr(.type_not_supported_on_target, p.tokenIdx, p.currToken().lexeme().?);
                try ty.combine(p, .Float128, p.tokenIdx);
            },

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
                    if (!res.value.isZero(p.comp)) {
                        var args = Attribute.initArguments(.aligned, alignToken);
                        if (try p.diagnose(.aligned, &args, 0, res)) |msg| {
                            try p.errExtra(msg.tag, argStart, msg.extra);
                            p.skipTo(.RParen);
                            return error.ParsingFailed;
                        }
                        args.aligned.alignment.?.node = .pack(res.node);
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
                            .cc = switch (p.currToken()) {
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
                const enumTy = try p.parseEnumSpec();
                try ty.combine(p, TypeBuilder.fromType(enumTy), tagToken);
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

                var bits: u64 = undefined;
                if (res.value.isNone()) {
                    try p.errToken(.expected_integer_constant_expr, bitIntToken);
                    return error.ParsingFailed;
                } else if (res.value.compare(.lte, Value.zero, p.comp)) {
                    bits = 0;
                } else {
                    bits = res.value.toInt(u64, p.comp) orelse std.math.maxInt(u64);
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
    return p.comp.internString(str);
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
        if (try p.symStack.findTag(internedName, p.tokenIds[kindToken], ident, p.currToken())) |prev| {
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

            const fw: Node.ContainerForwardDecl = .{
                .nameOrKindToken = ident,
                .containerType = ty,
                .definition = null,
            };
            try p.declBuffer.append(try p.addNode(if (isStruct)
                .{ .structForwardDecl = fw }
            else
                .{ .unionForwardDecl = fw }));
            return ty;
        }
    };

    var done = false;
    errdefer if (!done) p.skipTo(.RBrace);

    // Get forward declared type or create a new one
    var defined = false;
    const recordType: *Type.Record = if (maybeIdent) |ident| recordTy: {
        const identStr = p.getTokenText(ident);
        const internedName = try p.comp.internString(identStr);
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
    try p.declBuffer.append(undefined);
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

    const recordDecls = p.declBuffer.items[declBufferTop..];
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
        const internedName = try p.comp.internString(identStr);
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
    const cd: Node.ContainerDecl = .{
        .nameOrKindToken = maybeIdent orelse kindToken,
        .containerType = ty,
        .fields = recordDecls,
    };

    p.declBuffer.items[declBufferTop - 1] = try p.addNode(if (isStruct) .{ .structDecl = cd } else .{ .unionDecl = cd });
    if (p.func.type == null)
        _ = p.tentativeDefs.remove(recordType.name);

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
        if (ty.is(.AutoType)) {
            try p.errStr(.auto_type_not_allowed, p.tokenIdx, if (p.record.kind == .KeywordStruct) "struct member" else "union member");
            ty = Type.Invalid;
        }

        var bitsNode: ?Node.Index = null;
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

            if (res.value.isNone()) {
                try p.errToken(.expected_integer_constant_expr, bitsToken);
                break :bits;
            } else if (res.value.compare(.lt, Value.zero, p.comp)) {
                try p.errStr(.negative_bitwidth, firstToken, try res.str(p));
                break :bits;
            }

            // incomplete size error is reported later
            const bitSize = ty.bitSizeof(p.comp) orelse break :bits;
            const bitsUnchecked = res.value.toInt(u32, p.comp) orelse std.math.maxInt(u32);
            if (bitsUnchecked > bitSize) {
                try p.errToken(.bitfield_too_big, nameToken);
                break :bits;
            } else if (bitsUnchecked == 0 and nameToken != 0) {
                try p.errToken(.zero_width_named_field, nameToken);
                break :bits;
            }

            bits = bitsUnchecked;
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

        if (nameToken == 0 and bits == null) unnamed: {
            // don't allow incompelete size fields in anonymous record.
            if (ty.is(.Enum) or ty.hasIncompleteSize()) break :unnamed;
            if (ty.isAnonymousRecord(p.comp)) {
                // An anonymous record appears as indirect fields on the parent
                try p.recordBuffer.append(.{
                    .name = try p.getAnonymousName(firstToken),
                    .ty = ty,
                });
                const node = try p.addNode(.{
                    .recordField = .{
                        .nameOrFirstToken = nameToken,
                        .type = ty,
                        .bitWidth = null,
                    },
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
                .recordField = .{
                    .nameOrFirstToken = nameToken,
                    .type = ty,
                    .bitWidth = bitsNode,
                },
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
        } else if (ty.hasIncompleteSize() and !ty.is(.Invalid)) {
            try p.errStr(.field_incomplete_ty, firstToken, try p.typeStr(ty));
        } else if (p.record.flexibleField) |some| {
            if (some != firstToken and p.record.kind == .KeywordStruct)
                try p.errToken(.flexible_non_final, some);
        }
        if (p.eat(.Comma) == null) break;
    }

    if (p.eat(.Semicolon) == null) {
        const curToken = p.currToken();
        if (curToken == .RBrace)
            try p.err(.missing_semicolon)
        else
            return p.errExpectedToken(.Semicolon, curToken);
    }

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
fn parseEnumSpec(p: *Parser) Error!Type {
    const enumToken = p.tokenIdx;
    p.tokenIdx += 1;

    const attrBufferTop = p.attrBuffer.len;
    defer p.attrBuffer.len = attrBufferTop;

    try p.parseAttrSpec();

    const maybeIdent = try p.eatIdentifier();
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
        const ident = maybeIdent orelse {
            try p.err(.ident_or_l_brace);
            return error.ParsingFailed;
        };

        // check if this is a reference to a previous type
        const internedName = try p.getInternString(ident);
        if (try p.symStack.findTag(internedName, .KeywordEnum, ident, p.currToken())) |prev| {
            if (p.currToken() == .Semicolon)
                try p.checkEnumFixedTy(fixedTy, ident, prev);
            return prev.type;
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

            try p.declBuffer.append(try p.addNode(.{
                .enumForwardDecl = .{
                    .nameOrKindToken = ident,
                    .containerType = ty,
                    .definition = null,
                },
            }));
            return ty;
        }
    };

    var done = false;
    errdefer if (!done) p.skipTo(.RBrace);

    // Get forward declared type or create a new one
    var defined = false;
    const enumType: *Type.Enum = if (maybeIdent) |ident| enumTy: {
        const identStr = p.getTokenText(ident);
        const internedName = try p.comp.internString(identStr);
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
    } else try Type.Enum.create(p.arena, try p.getAnonymousName(enumToken), fixedTy);

    // reserve space for this enum
    try p.declBuffer.append(undefined);
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

    var ty = Type{ .specifier = .Enum, .data = .{ .@"enum" = enumType } };
    ty = try Attribute.applyTypeAttributes(p, ty, attrBufferTop, null);

    if (!enumType.fixed) {
        const tagSpecifier = try e.getTypeSpecifier(p, ty.enumIsPacked(p.comp), maybeIdent orelse enumToken);
        enumType.tagType = .{ .specifier = tagSpecifier };
    }

    const enumFields = p.enumBuffer.items[enumBufferTop..];
    const fieldNodes = p.listBuffer.items[listBufferTop..];

    if (fixedTy == null) {
        for (enumFields, fieldNodes) |*field, fieldNode| {
            if (field.ty.eql(Type.Int, p.comp, false)) continue;

            const sym = p.symStack.get(field.name, .vars) orelse continue;
            var res: Result = .{ .node = undefined, .ty = field.ty, .value = sym.value };
            const destTy = if (p.comp.fixedEnumTagSpecifier()) |some|
                Type{ .specifier = some }
            else if (try res.intFitsInType(p, Type.Int))
                Type.Int
            else if (!res.ty.eql(enumType.tagType, p.comp, false))
                enumType.tagType
            else
                continue;

            const symbol = p.symStack.getPtr(field.name, .vars);
            try symbol.value.intCast(destTy, p.comp);

            symbol.type = destTy;
            field.ty = destTy;
            res.ty = destTy;

            // Create a new enum_field node with the correct type.
            var newFieldNode = fieldNode.get(&p.tree);
            newFieldNode.enumField.type = destTy;

            if (field.init.unpack()) |some| {
                res.node = some;
                try res.implicitCast(p, .IntCast, some.tok(&p.tree));
                field.init = .pack(res.node);
                newFieldNode.enumField.init = res.node;
            }

            try p.tree.setNode(@intFromEnum(fieldNode), newFieldNode);
        }
    }

    enumType.fields = try p.arena.dupe(Type.Enum.Field, enumFields);
    // declare a symbol for the type
    if (maybeIdent != null and !defined) {
        try p.symStack.define(.{
            .kind = .@"enum",
            .name = enumType.name,
            .type = ty,
            .token = maybeIdent.?,
            .value = .{},
        });
    }
    // finish by creating a node
    const node = try p.addNode(.{
        .enumDecl = .{
            .nameOrKindToken = maybeIdent orelse enumToken,
            .containerType = ty,
            .fields = fieldNodes,
        },
    });
    p.declBuffer.items[declBufferTop - 1] = node;
    if (p.func.type == null)
        _ = p.tentativeDefs.remove(enumType.name);
    return ty;
}

const Enumerator = struct {
    value: Value = .{},
    type: Type,
    // tracks the minimum number of bits required to represent the positive part of the enumeration constant
    numPositiveBits: usize = 0,
    // tracks the minimum number of bits required to represent the negative part of the enumeration constant
    numNegativeBits: usize = 0,
    // whether indicate fixed type
    fixed: bool,

    fn init(fixedTy: ?Type) Enumerator {
        return .{
            .type = fixedTy orelse Type.Int,
            .fixed = (fixedTy != null),
        };
    }

    /// Increment enumerator value adjusting type if needed.
    fn incr(e: *Enumerator, p: *Parser, token: TokenIndex) !void {
        const oldVal = e.value;
        if (oldVal.isNone()) {
            // First enumerator, set to 0 fits in all types.
            e.value = Value.zero;
            return;
        }

        if (try e.value.add(e.value, Value.one, e.type, p.comp)) {
            const byteSize = e.type.sizeof(p.comp).?;
            const bitSize: u8 = @intCast(if (e.type.isUnsignedInt(p.comp)) byteSize * 8 else byteSize * 8 - 1);

            if (e.fixed) {
                try p.errStr(.enum_not_representable_fixed, token, try p.typeStr(e.type));
                return;
            }

            const newTy = if (p.comp.nextLargestIntSameSign(e.type)) |larger| blk: {
                try p.errToken(.enumerator_overflow, token);
                break :blk larger;
            } else blk: {
                try p.errExtra(.enum_not_representable, token, .{ .pow2AsString = bitSize });
                break :blk Type.ULongLong;
            };
            e.type = newTy;
            _ = try e.value.add(oldVal, Value.one, e.type, p.comp);
        }
    }

    /// Set enumerator value to specified value.
    fn set(e: *Enumerator, p: *Parser, res: *Result, token: TokenIndex) !void {
        if (res.ty.isInvalid()) return;
        if (e.fixed and !res.ty.eql(e.type, p.comp, false)) {
            if (!try res.intFitsInType(p, e.type)) {
                try p.errStr(.enum_not_representable_fixed, token, try p.typeStr(e.type));
                return error.ParsingFailed;
            }
            res.ty = e.type;
            try res.implicitCast(p, .IntCast, token);
            e.value = res.value;
        } else {
            try res.intCast(p, res.ty.integerPromotion(p.comp), token);
            e.type = res.ty;
            e.value = res.value;
        }
    }

    fn getTypeSpecifier(e: *const Enumerator, p: *Parser, isPacked: bool, token: TokenIndex) !Type.Specifier {
        if (p.comp.fixedEnumTagSpecifier()) |tagSpecifier|
            return tagSpecifier;

        const charWidth = Type.SChar.sizeof(p.comp).? * 8;
        const shortWidth = Type.Short.sizeof(p.comp).? * 8;
        const intWidth = Type.Int.sizeof(p.comp).? * 8;
        if (e.numNegativeBits > 0) {
            if (isPacked and e.numNegativeBits <= charWidth and e.numPositiveBits < charWidth)
                return .SChar
            else if (isPacked and e.numNegativeBits <= shortWidth and e.numPositiveBits < shortWidth)
                return .Short
            else if (e.numNegativeBits <= intWidth and e.numPositiveBits < intWidth)
                return .Int;

            const longWidth = Type.Long.sizeof(p.comp).? * 8;
            if (e.numNegativeBits <= longWidth and e.numPositiveBits < longWidth)
                return .Long;

            const llongWidth = Type.LongLong.sizeof(p.comp).? * 8;
            if (e.numNegativeBits > llongWidth or e.numPositiveBits >= llongWidth)
                try p.errToken(.enum_too_large, token);
            return .LongLong;
        }

        if (isPacked and e.numPositiveBits <= charWidth)
            return .UChar
        else if (isPacked and e.numPositiveBits <= shortWidth)
            return .UShort
        else if (e.numPositiveBits <= intWidth)
            return .UInt
        else if (e.numPositiveBits <= Type.Long.sizeof(p.comp).? * 8)
            return .ULong;

        return .ULongLong;
    }

    fn str(e: *const Enumerator, p: *Parser) ![]const u8 {
        return (Result{
            .node = undefined, // Result.str does not use the node
            .ty = e.type,
            .value = e.value,
        }).str(p);
    }
};

const EnumFieldAndNode = struct { field: Type.Enum.Field, node: Node.Index };

/// enumerator : identifier ('=' integer-constant-expression)
fn enumerator(p: *Parser, e: *Enumerator) Error!?EnumFieldAndNode {
    _ = try p.pragma();
    const nameToken = try p.eatIdentifier() orelse {
        if (p.currToken() == .RBrace) return null;
        try p.err(.expected_identifier);
        p.skipTo(.RBrace);
        return error.ParsingFailed;
    };

    const attrBufferTop = p.attrBuffer.len;
    defer p.attrBuffer.len = attrBufferTop;

    try p.parseAttrSpec();

    const errStart = p.comp.diagnostics.list.items.len;
    const fieldInit = if (p.eat(.Equal)) |_| blk: {
        var specified = try p.parseIntegerConstExpr(.GNUFoldingExtension);
        if (specified.value.isNone()) {
            try p.errToken(.enum_val_unavailable, nameToken + 2);
            try e.incr(p, nameToken);
            break :blk null;
        } else {
            try e.set(p, &specified, nameToken);
            break :blk specified.node;
        }
    } else blk: {
        try e.incr(p, nameToken);
        break :blk null;
    };

    const attrTy = try Attribute.applyEnumeratorAttributes(p, e.type, attrBufferTop);

    if (attrTy.isUnsignedInt(p.comp) or e.value.compare(.gte, Value.zero, p.comp))
        e.numPositiveBits = @max(e.numPositiveBits, e.value.minUnsignedBits(p.comp))
    else
        e.numNegativeBits = @max(e.numNegativeBits, e.value.minSignedBits(p.comp));

    if (errStart == p.comp.diagnostics.list.items.len) {
        // only do these warnings if we didn't already warn about overflow or non-representable values
        if (e.value.compare(.lt, Value.zero, p.comp)) {
            const minInt = Type.Int.minInt(p.comp);
            const minValue = try Value.int(minInt, p.comp);
            if (e.value.compare(.lt, minValue, p.comp))
                try p.errStr(.enumerator_too_small, nameToken, try e.str(p));
        } else {
            const maxInt = Type.Int.maxInt(p.comp);
            const maxValue = try Value.int(maxInt, p.comp);
            if (e.value.compare(.gt, maxValue, p.comp))
                try p.errStr(.enumerator_too_large, nameToken, try e.str(p));
        }
    }

    const node = try p.addNode(.{
        .enumField = .{
            .nameToken = nameToken,
            .type = attrTy,
            .init = fieldInit,
        },
    });

    try p.tree.valueMap.put(p.gpa, node, e.value);

    const internedName = try p.getInternString(nameToken);
    try p.symStack.defineEnumeration(internedName, attrTy, nameToken, e.value, node);

    return .{
        .field = .{
            .name = internedName,
            .ty = attrTy,
            .nameToken = nameToken,
            .init = .packOpt(fieldInit),
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
        switch (p.currToken()) {
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
    if (baseType.is(.AutoType) and !d.type.is(.AutoType)) {
        try p.errToken(.auto_type_requires_plain_declarator, start);
        return error.ParsingFailed;
    }

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
        if (d.funcDeclarator) |some| res.funcDeclarator = some;
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
        if (p.currToken() == .LBracket) {
            switch (kind) {
                .normal, .record => if (p.comp.langOpts.standard.atLeast(.c23)) {
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
        const optSize = if (star) |_| null else try p.parseAssignExpr();
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

        if (static) |_| _ = try p.expectResult(optSize);

        if (baseType.is(.AutoType)) {
            try p.errStr(.array_of_auto_type, d.name, p.getTokenText(d.name));
            return error.ParsingFailed;
        }

        const outer = try p.directDeclarator(baseType, d, kind);
        var maxBits = p.comp.target.ptrBitWidth();
        if (maxBits > 61) maxBits = 61;

        const maxBytes = (@as(u64, 1) << @as(u6, @truncate(maxBits))) - 1;

        if (optSize != null and !optSize.?.ty.isInt()) {
            try p.errStr(.array_size_non_int, sizeToken, try p.typeStr(optSize.?.ty));
            return error.ParsingFailed;
        }

        if (baseType.is(.C23Auto)) {
            // issue error later.
            return Type.Invalid;
        }

        if (optSize) |size| {
            if (size.value.isNone()) {
                try p.errToken(.vla, sizeToken);
                if (p.func.type == null and kind != .param and p.record.kind == .Invalid)
                    try p.errToken(.variable_len_array_file_scope, d.name);

                const exprType = try p.arena.create(Type.Expr);
                exprType.ty = Type.Void;
                exprType.node = size.node;
                resType.data = .{ .expr = exprType };
                resType.specifier = .VariableLenArray;

                if (static) |some| try p.errToken(.useless_static, some);
            } else {
                // `outer` is validated later so it may be invalid here
                const outerSize = outer.sizeof(p.comp);
                const maxElems = maxBytes / @max(1, outerSize orelse 1);
                if (size.value.isZero(p.comp)) {
                    try p.errToken(.zero_length_array, lb);
                } else if (size.value.compare(.lt, Value.zero, p.comp)) {
                    try p.errToken(.negative_array_size, lb);
                    return error.ParsingFailed;
                }
                const arrayType = try p.arena.create(Type.Array);
                arrayType.elem = Type.Void;
                arrayType.len = size.value.toInt(u64, p.comp) orelse std.math.maxInt(u64);
                if (arrayType.len > maxElems) {
                    try p.errToken(.array_too_large, lb);
                    arrayType.len = maxElems;
                }
                resType.data = .{ .array = arrayType };
                resType.specifier = .Array;
            }
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

        if (try p.parseParamDecls(d)) |params| {
            funcType.params = params;
            if (p.eat(.Ellipsis)) |_| specifier = .VarArgsFunc;
        } else if (p.currToken() == .RParen) {
            specifier = if (p.comp.langOpts.standard.atLeast(.c23)) .Func else .OldStyleFunc;
        } else if (p.currToken() == .Identifier or p.currToken() == .ExtendedIdentifier) {
            d.oldTypeFunc = p.tokenIdx;

            const paramBufferTop = p.paramBuffer.items.len;
            try p.symStack.pushScope();

            defer {
                p.paramBuffer.items.len = paramBufferTop;
                p.symStack.popScope();
            }

            specifier = if (p.comp.langOpts.standard.atLeast(.c23))
                .VarArgsFunc
            else
                .OldStyleFunc;
            while (true) {
                const nameToken = try p.expectIdentifier();
                const internedName = try p.getInternString(nameToken);
                try p.symStack.defineParam(internedName, undefined, nameToken, null);
                try p.paramBuffer.append(.{
                    .name = internedName,
                    .nameToken = nameToken,
                    .ty = Type.Int,
                    .node = .null,
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
fn parseParamDecls(p: *Parser, d: *Declarator) Error!?[]Type.Function.Param {
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
        else if (p.comp.langOpts.standard.atLeast(.c23) and
            (p.currToken() == .Identifier or p.currToken() == .ExtendedIdentifier))
        {
            // handle deprecated K&R style parameters
            const identifier = try p.expectIdentifier();
            try p.errStr(.unknown_type_name, identifier, p.getTokenText(identifier));

            if (d.oldTypeFunc == null)
                d.oldTypeFunc = identifier;

            try p.paramBuffer.append(.{
                .name = try p.comp.internString(p.getTokenText(identifier)),
                .nameToken = identifier,
                .ty = Type.Int,
                .node = .null,
            });

            if (p.eat(.Comma) == null) break;
            if (p.currToken() == .Ellipsis) break;

            continue;
        } else if (p.paramBuffer.items.len == paramBufferTop) {
            return null;
        } else blk: {
            var spec: TypeBuilder = .{};
            break :blk DeclSpec{ .type = try spec.finish(p) };
        };

        var nameToken: TokenIndex = 0;
        var paramNode: Node.OptIndex = .null;
        const firstToken = p.tokenIdx;
        var paramType = paramDeclSpec.type;
        if (try p.declarator(paramDeclSpec.type, .param)) |some| {
            if (some.oldTypeFunc) |tokenIdx|
                try p.errToken(.invalid_old_style_params, tokenIdx);

            try p.parseAttrSpec();

            nameToken = some.name;
            paramType = try Attribute.applyParameterAttributes(p, some.type, attrBufferTop, .alignas_on_param);
            if (some.name != 0) {
                const node = try p.addNode(.{
                    .param = .{
                        .nameToken = nameToken,
                        .type = paramType,
                        .storageClass = switch (paramDeclSpec.storageClass) {
                            .none => .auto,
                            .register => .register,
                            else => .auto, // Error reported in `validateParam`
                        },
                    },
                });
                paramNode = .pack(node);
                const internedName = try p.getInternString(nameToken);
                try p.symStack.defineParam(internedName, paramType, nameToken, node);
            }
        } else {
            paramType = try Attribute.applyParameterAttributes(p, paramType, attrBufferTop, .alignas_on_param);
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
                if (p.currToken() != .RParen) {
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
            .node = paramNode,
        });

        if (p.eat(.Comma) == null) break;
        if (p.currToken() == .Ellipsis) break;
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
    if (p.currToken() != .LBrace) {
        const token = p.tokenIdx;
        var res = try p.expect(parseAssignExpr);
        if (try p.coerceArrayInit(&res, token, initType))
            return res;

        try p.coerceInit(&res, token, initType);
        return res;
    }

    if (initType.is(.AutoType)) {
        try p.err(.auto_type_with_init_list);
        return error.ParsingFailed;
    }

    var il: InitList = .{};
    defer il.deinit(p.gpa);

    _ = try p.initializerItem(&il, initType);

    const res = try p.convertInitList(il, initType);
    var resType = res.type(&p.tree);
    resType.qual = initType.qual;
    return .{ .ty = resType, .node = res };
}

/// initializerItems : designation? initializer (',' designation? initializer)* ','?
/// designation : designator-list '='
/// designator-list: designator designator-list designator
/// designator : '[' integer-constant-expression ']'
///            | '.' identifier
pub fn initializerItem(p: *Parser, il: *InitList, initType: Type) Error!bool {
    const lb = p.eat(.LBrace) orelse {
        const token = p.tokenIdx;
        var res = (try p.parseAssignExpr()) orelse return false;

        const arr = try p.coerceArrayInit(&res, token, initType);
        if (!arr)
            try p.coerceInit(&res, token, initType);

        if (il.tok != 0) {
            try p.errToken(.initializer_overrides, token);
            try p.errToken(.previous_initializer, il.tok);
        }
        il.node = .pack(res.node);
        il.tok = token;
        return true;
    };

    const isScalar = initType.isScalar();
    const isComplex = initType.isComplex();
    const scalarInitsNeeds: usize = if (isComplex) 2 else 1;
    if (p.eat(.RBrace)) |_| {
        if (isScalar)
            try p.errToken(.empty_scalar_init, lb);

        if (il.tok != 0) {
            try p.errToken(.initializer_overrides, lb);
            try p.errToken(.previous_initializer, il.tok);
        }
        il.node = .null;
        il.tok = lb;
        return true;
    }

    var count: u64 = 0;
    var warnedExcess = false;
    var isStrInit = false;
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

                if (indexRes.value.isNone()) {
                    try p.errToken(.expected_integer_constant_expr, exprToken);
                    return error.ParsingFailed;
                } else if (indexRes.value.compare(.lt, Value.zero, p.comp)) {
                    try p.errStr(.negative_array_designator, lb + 1, try indexRes.str(p));
                    return error.ParsingFailed;
                }

                const maxLen = curType.arrayLen() orelse std.math.maxInt(usize);
                const indexInt = indexRes.value.toInt(u64, p.comp) orelse std.math.maxInt(u64);
                if (indexInt >= maxLen) {
                    try p.errStr(.oob_array_designator, lbr + 1, try indexRes.str(p));
                    return error.ParsingFailed;
                }

                curIndexHint = curIndexHint orelse indexInt;
                curIL = try curIL.find(p.gpa, indexInt);
                curType = curType.getElemType();
                designation = true;
            } else if (p.eat(.Period)) |period| {
                const fieldToken = try p.expectIdentifier();
                const fieldStr = p.getTokenText(fieldToken);
                const fieldName = try p.comp.internString(fieldStr);
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
            var tempIL: InitList = .{};
            defer tempIL.deinit(p.gpa);
            saw = try p.initializerItem(&tempIL, Type.Void);
        } else if (count == 0 and p.isStringInit(initType)) {
            isStrInit = true;
            saw = try p.initializerItem(il, initType);
        } else if (isScalar and count >= scalarInitsNeeds) {
            // discard further scalars
            var tempIL: InitList = .{};
            defer tempIL.deinit(p.gpa);
            saw = try p.initializerItem(&tempIL, Type.Void);
        } else if (p.currToken() == .LBrace) {
            if (designation) {
                // designation overrides previous value, let existing mechanism handle it
                saw = try p.initializerItem(curIL, curType);
            } else if (try p.findAggregateInitializer(&curIL, &curType, &indexHint)) {
                saw = try p.initializerItem(curIL, curType);
            } else {
                // discard further values
                var tempIL: InitList = .{};
                defer tempIL.deinit(p.gpa);

                saw = try p.initializerItem(curIL, curType);
                saw = try p.initializerItem(&tempIL, Type.Void);
                if (!warnedExcess)
                    try p.errToken(if (initType.isArray()) .excess_array_init else .excess_struct_init, firstToken);
                warnedExcess = true;
            }
        } else singleItem: {
            firstToken = p.tokenIdx;
            var res = (try p.parseAssignExpr()) orelse {
                saw = false;
                break :singleItem;
            };
            saw = true;

            excess: {
                if (indexHint) |*hint| {
                    if (try p.findScalarInitializerAt(&curIL, &curType, &res, firstToken, hint)) break :excess;
                } else if (try p.findScalarInitializer(&curIL, &curType, &res, firstToken)) break :excess;

                if (designation) break :excess;
                if (!warnedExcess) try p.errToken(if (initType.isArray()) .excess_array_init else .excess_struct_init, firstToken);
                warnedExcess = true;

                break :singleItem;
            }

            const arr = try p.coerceArrayInit(&res, firstToken, curType);
            if (!arr) try p.coerceInit(&res, firstToken, curType);
            if (curIL.tok != 0) {
                try p.errToken(.initializer_overrides, firstToken);
                try p.errToken(.previous_initializer, curIL.tok);
            }
            curIL.node = .pack(res.node);
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
            if (isScalar and !isComplex) try p.errToken(.excess_scalar_init, firstToken);
        } else if (count == 2) {
            if (isScalar and isComplex) try p.errToken(.excess_scalar_init, firstToken);
        }

        if (p.eat(.Comma) == null) break;
    }
    try p.expectClosing(lb, .RBrace);

    if (isComplex and count == 1) // count of 1 means we saw exactly 2 items in the initializer list
        try p.errToken(.complex_component_init, lb);

    if (isScalar or isStrInit)
        return true;

    if (il.tok != 0) {
        try p.errToken(.initializer_overrides, lb);
        try p.errToken(.previous_initializer, il.tok);
    }
    il.node = .null;
    il.tok = lb;
    return true;
}

/// returns true if the value is unused
fn findScalarInitializerAt(
    p: *Parser,
    il: **InitList,
    ty: *Type,
    res: *Result,
    firstToken: TokenIndex,
    startIdx: *u64,
) Error!bool {
    if (ty.isArray()) {
        if (il.*.node != .null) return false;
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
            _ = try p.findScalarInitializer(il, ty, res, firstToken);
            return true;
        }
        return false;
    } else if (ty.get(.Struct)) |structType| {
        if (il.*.node != .null) return false;
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
            _ = try p.findScalarInitializer(il, ty, res, firstToken);
            return true;
        }
        return false;
    } else if (ty.get(.Union)) |_| {
        return false;
    }
    return il.*.node == .null;
}

/// Returns true if the value is unused.
fn findScalarInitializer(
    p: *Parser,
    il: **InitList,
    ty: *Type,
    res: *Result,
    firstToken: TokenIndex,
) Error!bool {
    const actualTy = res.ty;
    if (ty.isArray() or ty.isComplex()) {
        if (il.*.node != .null) return false;
        if (try p.coerceArrayInitExtra(res, firstToken, ty.*, false)) return true;
        const startIdx = il.*.list.items.len;
        var index = if (startIdx != 0) il.*.list.items[startIdx - 1].index else startIdx;

        const arrayType = ty.*;
        const elemCount = arrayType.expectedInitListSize() orelse std.math.maxInt(u64);
        if (elemCount == 0) {
            try p.errToken(.empty_aggregate_init_braces, firstToken);
            return error.ParsingFailed;
        }

        const elemTy = arrayType.getElemType();
        const arrayIL = il.*;
        while (index < elemCount) : (index += 1) {
            ty.* = elemTy;
            il.* = try arrayIL.find(p.gpa, index);
            if (il.*.node == .null and actualTy.eql(elemTy, p.comp, false))
                return true;
            if (try p.findScalarInitializer(il, ty, res, firstToken))
                return true;
        }
        return false;
    } else if (ty.get(.Struct)) |structType| {
        if (il.*.node != .null) return false;
        if (actualTy.eql(ty.*, p.comp, false)) return true;

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
            if (il.*.node == .null and actualTy.eql(field.ty, p.comp, false)) return true;
            if (il.*.node == .null and try p.coerceArrayInitExtra(res, firstToken, ty.*, false)) return true;
            if (try p.findScalarInitializer(il, ty, res, firstToken)) return true;
        }
        return false;
    } else if (ty.get(.Union)) |unionType| {
        if (il.*.node != .null)
            return false;
        if (actualTy.eql(ty.*, p.comp, false))
            return true;
        if (unionType.data.record.fields.len == 0) {
            try p.errToken(.empty_aggregate_init_braces, firstToken);
            return error.ParsingFailed;
        }
        ty.* = unionType.data.record.fields[0].ty;
        il.* = try il.*.find(p.gpa, 0);
        if (try p.coerceArrayInitExtra(res, firstToken, ty.*, false)) return true;
        if (try p.findScalarInitializer(il, ty, res, firstToken)) return true;
        return false;
    }
    return il.*.node == .null;
}

fn findAggregateInitializer(p: *Parser, il: **InitList, ty: *Type, startIdx: *?u64) Error!bool {
    if (ty.isArray()) {
        if (il.*.node != .null) return false;
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
        if (il.*.node != .null)
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
        if (il.*.node != .null)
            return false;
        if (startIdx.*) |_| return false; // overrides
        if (unionType.data.record.fields.len == 0)
            return false;
        ty.* = unionType.data.record.fields[0].ty;
        il.* = try il.*.find(p.gpa, 0);
        return true;
    } else {
        try p.err(.too_many_scalar_init_braces);
        return il.*.node == .null;
    }
}

fn coerceArrayInit(p: *Parser, item: *Result, token: TokenIndex, target: Type) !bool {
    if (!target.isArray()) return false;
    return p.coerceArrayInitExtra(item, token, target, true);
}

fn coerceArrayInitExtra(p: *Parser, item: *Result, token: TokenIndex, target: Type, reportError: bool) !bool {
    const isStrLiteral = p.nodeIs(item.node, .stringLiteralExpr);
    if (!isStrLiteral and !p.nodeIs(item.node, .compoundLiteralExpr) or !item.ty.isArray()) {
        if (!reportError) return false;
        try p.errToken(.array_init_str, token);
        return true; // do not do further coercion
    }

    const targetSpec = target.getElemType().canonicalize(.standard).specifier;
    const itemSpec = item.ty.getElemType().canonicalize(.standard).specifier;
    const compatible = target.getElemType().eql(item.ty.getElemType(), p.comp, false) or
        (isStrLiteral and itemSpec == .Char and (targetSpec == .UChar or targetSpec == .SChar)) or
        (isStrLiteral and itemSpec == .UChar and (targetSpec == .UChar or targetSpec == .SChar or targetSpec == .Char));

    if (!compatible) {
        if (!reportError) return false;
        const eMsg = " with array of type ";
        try p.errStr(.incompatible_array_init, token, try p.typePairStrExtra(target, eMsg, item.ty));
        return true; // do not do further coercion
    }

    if (target.get(.Array)) |arrayType| {
        assert(item.ty.is(.Array));
        const len = item.ty.arrayLen().?;
        const arrayLen = arrayType.arrayLen().?;
        if (isStrLiteral) {
            // the null byte of a string can be dropped
            if (len - 1 > arrayLen and reportError)
                try p.errToken(.str_init_too_long, token);
        } else if (len > arrayLen and reportError) {
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

    const node = item.node;
    try item.lvalConversion(p, token);
    if (target.is(.AutoType)) {
        if (p.getNode(node, .memberAccessExpr) orelse p.getNode(node, .memberAccessPtrExpr)) |access| {
            if (access.isBitFieldWidth(&p.tree) != null) try p.errToken(.auto_type_from_bitfield, token);
        }
        return;
    } else if (target.is(.C23Auto)) {
        return;
    }
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
fn convertInitList(p: *Parser, il: InitList, initType: Type) Error!Node.Index {
    const isComplex = initType.isComplex();
    if (initType.isScalar() and !isComplex) {
        return il.node.unpack() orelse
            try p.addNode(.{
                .defaultInitExpr = .{
                    .lastToken = il.tok,
                    .type = initType,
                },
            });
    } else if (initType.is(.VariableLenArray)) {
        return error.ParsingFailed; // vla invalid, reported earlier
    } else if (initType.isArray() or isComplex) {
        if (il.node.unpack()) |some| return some;

        const listBuffTop = p.listBuffer.items.len;
        defer p.listBuffer.items.len = listBuffTop;

        const elemType = initType.getElemType();
        const maxItems: u64 = initType.expectedInitListSize() orelse std.math.maxInt(usize);
        var start: u64 = 0;
        for (il.list.items) |*init| {
            if (init.index > start) {
                const elem = try p.addNode(.{
                    .arrayFillerExpr = .{
                        .lastToken = p.tokenIdx - 1,
                        .count = init.index - start,
                        .type = elemType,
                    },
                });
                try p.listBuffer.append(elem);
            }
            start = init.index + 1;

            const elem = try p.convertInitList(init.list, elemType);
            try p.listBuffer.append(elem);
        }

        var arrInitTy = initType;
        if (initType.specifier == .IncompleteArray) {
            arrInitTy.specifier = .Array;
            arrInitTy.data.array.len = start;
        } else if (initType.is(.IncompleteArray)) {
            const arrayType = try p.arena.create(Type.Array);
            arrayType.* = .{ .elem = initType.getElemType(), .len = start };
            arrInitTy = .{ .specifier = .Array, .data = .{ .array = arrayType } };
            const attrs = initType.getAttributes();
            arrInitTy = try arrInitTy.withAttributes(p.arena, attrs);
        } else if (start < maxItems) {
            const elem = try p.addNode(.{
                .arrayFillerExpr = .{
                    .lastToken = p.tokenIdx - 1,
                    .count = maxItems - start,
                    .type = elemType,
                },
            });
            try p.listBuffer.append(elem);
        }

        return try p.addNode(.{
            .arrayInitExpr = .{
                .lbraceToken = il.tok,
                .containerType = arrInitTy,
                .items = p.listBuffer.items[listBuffTop..],
            },
        });
    } else if (initType.get(.Struct)) |structType| {
        assert(!structType.hasIncompleteSize());

        if (il.node.unpack()) |some| return some;

        const listBuffTop = p.listBuffer.items.len;
        defer p.listBuffer.items.len = listBuffTop;

        var initIndex: usize = 0;
        for (structType.data.record.fields, 0..) |f, i| {
            if (initIndex < il.list.items.len and il.list.items[initIndex].index == i) {
                const item = try p.convertInitList(il.list.items[initIndex].list, f.ty);
                try p.listBuffer.append(item);
                initIndex += 1;
            } else {
                const item = try p.addNode(.{ .defaultInitExpr = .{ .lastToken = il.tok, .type = f.ty } });
                try p.listBuffer.append(item);
            }
        }

        return try p.addNode(.{
            .structInitExpr = .{
                .lbraceToken = il.tok,
                .containerType = initType,
                .items = p.listBuffer.items[listBuffTop..],
            },
        });
    } else if (initType.get(.Union)) |unionType| {
        if (il.node.unpack()) |some| return some;

        const initNode, const index = if (unionType.data.record.fields.len == 0)
            // do nothing for empty unions
            .{ null, 0 }
        else if (il.list.items.len == 0)
            .{ try p.addNode(.{ .defaultInitExpr = .{ .lastToken = p.tokenIdx - 1, .type = initType } }), 0 }
        else blk: {
            const init = il.list.items[0];
            const index: u32 = @truncate(init.index);
            const fieldType = unionType.data.record.fields[index].ty;
            break :blk .{ try p.convertInitList(init.list, fieldType), index };
        };

        return try p.addNode(.{
            .unionInitExpr = .{
                .fieldIndex = index,
                .initializer = initNode,
                .lbraceToken = il.tok,
                .unionType = initType,
            },
        });
    } else {
        return error.ParsingFailed; // initializer target is invalid, reported earily.
    }
}

fn parseMSVCAsmStmt(p: *Parser) Error!?Node.Index {
    return p.todo("MSVC assembly statements");
}

/// asmOperand : ('[' Identifier ']')? asmStr '(' expr ')'
fn parseAsmOperands(p: *Parser, names: *std.ArrayList(?TokenIndex), constraints: *NodeList, exprs: *NodeList) Error!void {
    if (!p.currToken().isStringLiteral() and p.currToken() != .LBracket) {
        // Empty
        return;
    }

    while (true) {
        if (p.eat(.LBracket)) |lbracket| {
            const ident = (try p.eatIdentifier()) orelse {
                try p.err(.expected_identifier);
                return error.ParsingFailed;
            };
            try names.append(ident);
            try p.expectClosing(lbracket, .RBracket);
        } else {
            try names.append(null);
        }

        const constraint = try p.parseAsmString();
        try constraints.append(constraint.node);

        const lparen = p.eat(.LParen) orelse {
            try p.errExtra(.expected_token, p.tokenIdx, .{ .tokenId = .{ .actual = p.currToken(), .expected = .LParen } });
            return error.ParsingFailed;
        };
        const maybeRes = try p.parseExpr();
        try p.expectClosing(lparen, .RParen);
        const res = try p.expectResult(maybeRes);
        try exprs.append(res.node);
        if (p.eat(.Comma) == null) return;
    }
}

/// gnuAsmStmt
///  : asmStr
///  | asmStr ':' asmOperand*
///  | asmStr ':' asmOperand* ':' asmOperand*
///  | asmStr ':' asmOperand* ':' asmOperand* : asmStr? (',' asmStr)*
///  | asmStr ':' asmOperand* ':' asmOperand* : asmStr? (',' asmStr)* : Identifier (',' Identifier)*
fn parseGNUAsmStmt(
    p: *Parser,
    quals: AST.GNUAssemblyQualifiers,
    asmToken: TokenIndex,
    lparen: TokenIndex,
) Error!Node.Index {
    const asmString = try p.parseAsmString();
    try p.checkAsmStr(asmString.value, lparen);

    if (p.currToken() == .RParen) {
        return p.addNode(.{
            .gnuAsmSimple = .{
                .asmString = asmString.node,
                .asmToken = asmToken,
            },
        });
    }

    const expectedItems = 8; // arbitrarily chosen, most assembly will have fewer than 8 inputs/outputs/constraints/names
    const bytesNeeded = expectedItems * @sizeOf(?TokenIndex) + expectedItems * 3 * @sizeOf(Node.Index);

    var stackFallback = std.heap.stackFallback(bytesNeeded, p.gpa);
    const allocator = stackFallback.get();

    var names = std.ArrayList(?TokenIndex).initCapacity(allocator, expectedItems) catch unreachable;
    var constraints = NodeList.initCapacity(allocator, expectedItems) catch unreachable;
    var exprs = NodeList.initCapacity(allocator, expectedItems) catch unreachable;
    var clobbers = NodeList.initCapacity(allocator, expectedItems) catch unreachable;

    defer {
        names.deinit();
        constraints.deinit();
        exprs.deinit();
        clobbers.deinit();
    }

    // Outputs
    var ateExtraColor = false;
    if (p.eat(.Colon) orelse p.eat(.ColonColon)) |tokenIdx| {
        ateExtraColor = p.tokenIds[tokenIdx] == .ColonColon;
        if (!ateExtraColor) {
            if (p.currToken().isStringLiteral() or p.currToken() == .LBracket) {
                while (true) {
                    try p.parseAsmOperands(&names, &constraints, &exprs);
                    if (p.eat(.Comma) == null) break;
                }
            }
        }
    }

    const numOutputs = names.items.len;

    // Inputs
    if (ateExtraColor or p.currToken() == .Colon or p.currToken() == .ColonColon) {
        if (ateExtraColor) {
            ateExtraColor = false;
        } else {
            ateExtraColor = p.currToken() == .ColonColon;
            p.tokenIdx += 1;
        }

        if (!ateExtraColor) {
            if (p.currToken().isStringLiteral() or p.currToken() == .LBracket) {
                while (true) {
                    try p.parseAsmOperands(&names, &constraints, &exprs);
                    if (p.eat(.Comma) == null) break;
                }
            }
        }
    }

    std.debug.assert(names.items.len == constraints.items.len and constraints.items.len == exprs.items.len);
    const numInputs = names.items.len - numOutputs;
    _ = numInputs;

    // Clobbers
    if (ateExtraColor or p.currToken() == .Colon or p.currToken() == .ColonColon) {
        if (ateExtraColor) {
            ateExtraColor = false;
        } else {
            ateExtraColor = p.currToken() == .ColonColon;
            p.tokenIdx += 1;
        }

        if (!ateExtraColor and p.currToken().isStringLiteral()) {
            while (true) {
                const clobber = try p.parseAsmString();
                try clobbers.append(clobber.node);
                if (p.eat(.Comma) == null) break;
            }
        }
    }

    if (!quals.goto and (p.currToken() != .RParen or ateExtraColor)) {
        try p.errExtra(.expected_token, p.tokenIdx, .{ .tokenId = .{ .actual = p.currToken(), .expected = .RParen } });
        return error.ParsingFailed;
    }

    // Goto labels
    var numLabels: u32 = 0;
    if (ateExtraColor or p.currToken() == .Colon) {
        if (!ateExtraColor) {
            p.tokenIdx += 1;
        }
        while (true) {
            const ident = (try p.eatIdentifier()) orelse {
                try p.err(.expected_identifier);
                return error.ParsingFailed;
            };
            const identStr = p.getTokenText(ident);
            const label = p.findLabel(identStr) orelse blk: {
                try p.labels.append(.{ .unresolvedGoto = ident });
                break :blk ident;
            };
            try names.append(ident);

            const elemTy = try p.arena.create(Type);
            elemTy.* = Type.Void;

            const labelAddrNode = try p.addNode(.{
                .addrOfLabel = .{
                    .labelToken = label,
                    .type = .{ .specifier = .Pointer, .data = .{ .subType = elemTy } },
                },
            });
            try exprs.append(labelAddrNode);

            numLabels += 1;
            if (p.eat(.Comma) == null) break;
        }
    } else if (quals.goto) {
        try p.errExtra(.expected_token, p.tokenIdx, .{ .tokenId = .{ .actual = p.currToken(), .expected = .Colon } });
        return error.ParsingFailed;
    }

    // TODO: validate and insert into AST
    return p.addNode(.{
        .nullStmt = .{
            .semicolonOrRbraceToken = asmToken,
            .type = Type.Void,
        },
    });
}

fn checkAsmStr(p: *Parser, asmString: Value, tok: TokenIndex) !void {
    if (!p.comp.langOpts.allowedGnuAsm()) {
        const str = p.comp.interner.get(asmString.ref()).bytes;
        if (str.len > 1) {
            // Empty string (just a NUll byte) is ok because it does not emit any assembly
            try p.errToken(.gnu_asm_disabled, tok);
        }
    }
}

/// assembly
///  : keyword-asm asmQual* '(' asmStr ')'
///  | keyword-asm asmQual* '(' gnuAsmStmt ')'
///  | keyword-asm msvcAsmStmt
fn parseAssembly(p: *Parser, kind: enum { global, declLabel, stmt }) Error!?Node.Index {
    const asmToken = p.tokenIdx;
    switch (p.currToken()) {
        .KeywordGccAsm => {
            try p.err(.extension_token_used);
            p.tokenIdx += 1;
        },
        .KeywordGccAsm1, .KeywordGccAsm2 => p.tokenIdx += 1,
        else => return null,
    }

    if (!p.currToken().canOpenGCCAsmStmt())
        return p.parseMSVCAsmStmt();

    var quals: AST.GNUAssemblyQualifiers = .{};
    while (true) : (p.tokenIdx += 1) switch (p.currToken()) {
        .KeywordVolatile, .KeywordGccVolatile1, .KeywordGccVolatile2 => {
            if (kind != .stmt) try p.errStr(.meaningless_asm_qual, p.tokenIdx, "volatile");
            if (quals.@"volatile") try p.errStr(.duplicate_asm_qual, p.tokenIdx, "volatile");
            quals.@"volatile" = true;
        },
        .KeywordInline, .KeywordGccInline1, .KeywordGccInline2 => {
            if (kind != .stmt) try p.errStr(.meaningless_asm_qual, p.tokenIdx, "inline");
            if (quals.@"inline") try p.errStr(.duplicate_asm_qual, p.tokenIdx, "inline");
            quals.@"inline" = true;
        },
        .KeywordGoto => {
            if (kind != .stmt) try p.errStr(.meaningless_asm_qual, p.tokenIdx, "goto");
            if (quals.goto) try p.errStr(.duplicate_asm_qual, p.tokenIdx, "goto");
            quals.goto = true;
        },
        else => break,
    };

    const lparen = try p.expectToken(.LParen);
    var resultNode: ?Node.Index = null;
    switch (kind) {
        .declLabel => {
            const asmString = try p.parseAsmString();
            const str = try p.removeNull(asmString.value);

            const args = Attribute.Arguments{ .asm_label = .{ .name = str } };
            const attr = Attribute{ .tag = .asm_label, .args = args, .syntax = .keyword };
            try p.attrBuffer.append(p.gpa, .{ .attr = attr, .tok = asmToken });
        },
        .global => {
            const asmString = try p.parseAsmString();
            try p.checkAsmStr(asmString.value, lparen);
            resultNode = try p.addNode(.{
                .globalAsm = .{
                    .asmToken = asmToken,
                    .asmString = asmString.node,
                },
            });
        },
        .stmt => resultNode = try p.parseGNUAsmStmt(quals, asmToken, lparen),
    }
    try p.expectClosing(lparen, .RParen);

    if (kind != .declLabel)
        _ = try p.expectToken(.Semicolon);
    return resultNode;
}

/// Same as stringLiteral but errors on unicode and wide string literals
fn parseAsmString(p: *Parser) Error!Result {
    var i = p.tokenIdx;
    while (true) : (i += 1) switch (p.tokenIds[i]) {
        .StringLiteral, .UnterminatedStringLiteral => {},
        .StringLiteralUTF_8, .StringLiteralUTF_16, .StringLiteralUTF_32 => {
            try p.errStr(.invalid_asm_str, p.tokenIdx, "unicode");
            return error.ParsingFailed;
        },
        .StringLiteralWide => {
            try p.errStr(.invalid_asm_str, p.tokenIdx, "wide");
            return error.ParsingFailed;
        },
        else => {
            if (i == p.tokenIdx) {
                try p.errStr(.expected_str_literal_in, p.tokenIdx, "asm");
                return error.ParsingFailed;
            }
            break;
        },
    };
    return p.parseStringLiteral();
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
fn parseStmt(p: *Parser) Error!Node.Index {
    if (try p.parseLabeledStmt()) |some|
        return some;

    if (try p.parseCompoundStmt(false, null)) |some|
        return some;

    if (p.eat(.KeywordIf)) |kwIf|
        return p.parseIfStmt(kwIf);

    if (p.eat(.KeywordSwitch)) |kwSwitch|
        return p.parseSwitchStmt(kwSwitch);

    if (p.eat(.KeywordWhile)) |kwWhile|
        return p.parseWhileStmt(kwWhile);

    if (p.eat(.KeywordDo)) |kwDo|
        return p.parseDoWhileStmt(kwDo);

    if (p.eat(.KeywordFor)) |kwFor|
        return p.parseForStmt(kwFor);

    if (p.eat(.KeywordGoto)) |gotoToken|
        return p.parseGotoStmt(gotoToken);

    if (p.eat(.KeywordContinue)) |cont| {
        if (!p.inLoop) try p.errToken(.continue_not_in_loop, cont);
        _ = try p.expectToken(.Semicolon);
        return try p.addNode(.{ .continueStmt = .{ .continueToken = cont } });
    }

    if (p.eat(.KeywordBreak)) |br| {
        if (!p.inLoop and p.@"switch" == null) try p.errToken(.break_not_in_loop_or_switch, br);
        _ = try p.expectToken(.Semicolon);
        return try p.addNode(.{ .breakStmt = .{ .breakToken = br } });
    }

    if (try p.parseReturnStmt()) |some|
        return some;

    if (try p.parseAssembly(.stmt)) |some|
        return some;

    const exprStart = p.tokenIdx;
    const errStart = p.comp.diagnostics.list.items.len;

    if (try p.parseExpr()) |some| {
        _ = try p.expectToken(.Semicolon);
        try some.maybeWarnUnused(p, exprStart, errStart);
        return some.node;
    }

    const attrBufferTop = p.attrBuffer.len;
    defer p.attrBuffer.len = attrBufferTop;
    try p.parseAttrSpec(); // .statement

    if (p.eat(.Semicolon)) |semicolon| {
        return p.addNode(.{
            .nullStmt = .{
                .semicolonOrRbraceToken = semicolon,
                .type = try Attribute.applyStatementAttributes(p, exprStart, attrBufferTop),
            },
        });
    }

    try p.err(.expected_stmt);
    return error.ParsingFailed;
}

/// if-statement
///  : `if` '(' expression ')' statement
///  | `if` '(' expression ')' statement `else` statement
fn parseIfStmt(p: *Parser, kwIf: TokenIndex) Error!Node.Index {
    const lp = try p.expectToken(.LParen);
    const condToken = p.tokenIdx;

    var cond = try p.expect(parseExpr);
    try cond.lvalConversion(p, condToken);
    try cond.usualUnaryConversion(p, condToken);

    if (!cond.ty.isScalar())
        try p.errStr(.statement_scalar, lp + 1, try p.typeStr(cond.ty));

    try cond.saveValue(p);
    try p.expectClosing(lp, .RParen);

    const thenBody = try p.parseStmt();
    const elseBody = if (p.eat(.KeywordElse)) |_| try p.parseStmt() else null;

    return try p.addNode(.{
        .ifStmt = .{
            .ifToken = kwIf,
            .cond = cond.node,
            .thenBody = thenBody,
            .elseBody = elseBody,
        },
    });
}

/// for-statement
///  : `for` '(' expression? ';' expression? ';' expression? ')' statement
///  | `for` '(' declaration expression? ';' expression? ')' statement
fn parseForStmt(p: *Parser, kwFor: TokenIndex) Error!Node.Index {
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
    const init = init: {
        if (gotDecl) break :init null;

        var init = (try p.parseExpr()) orelse break :init null;
        try init.saveValue(p);
        try init.maybeWarnUnused(p, initStart, errStart);
        break :init init.node;
    };

    if (!gotDecl)
        _ = try p.expectToken(.Semicolon);

    // cond
    const cond = cond: {
        if (gotDecl) break :cond null;

        const condToken = p.tokenIdx;
        var cond = (try p.parseExpr()) orelse break :cond null;
        try cond.lvalConversion(p, condToken);
        try cond.usualUnaryConversion(p, condToken);

        if (!cond.ty.isScalar())
            try p.errStr(.statement_scalar, lp + 1, try p.typeStr(cond.ty));

        try cond.saveValue(p);
        break :cond cond.node;
    };

    _ = try p.expectToken(.Semicolon);

    // increment
    const incrStart = p.tokenIdx;
    errStart = p.comp.diagnostics.list.items.len;

    const incr = incr: {
        var incr = (try p.parseExpr()) orelse break :incr null;
        try incr.maybeWarnUnused(p, incrStart, errStart);
        try incr.saveValue(p);
        break :incr incr.node;
    };
    try p.expectClosing(lp, .RParen);

    const body = body: {
        const oldLoop = p.inLoop;
        p.inLoop = true;
        defer p.inLoop = oldLoop;
        break :body try p.parseStmt();
    };

    return p.addNode(.{
        .forStmt = .{
            .forToken = kwFor,
            .init = if (init) |some|
                .{ .expr = some }
            else
                .{ .decls = p.declBuffer.items[declBufferTop..] },
            .cond = cond,
            .incr = incr,
            .body = body,
        },
    });
}

/// while-statement : `while` '(' expression ')' statement ';'
fn parseWhileStmt(p: *Parser, kwWhile: TokenIndex) Error!Node.Index {
    const lp = try p.expectToken(.LParen);
    const condToken = p.tokenIdx;

    var cond = try p.expect(parseExpr);
    try cond.lvalConversion(p, condToken);
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

    return p.addNode(.{
        .whileStmt = .{ .whileToken = kwWhile, .cond = cond.node, .body = body },
    });
}

/// do-while-statement : `do` statement `while` '(' expression ')' ';'
fn parseDoWhileStmt(p: *Parser, kwDo: TokenIndex) Error!Node.Index {
    const body = body: {
        const oldLoop = p.inLoop;
        p.inLoop = true;
        defer p.inLoop = oldLoop;
        break :body try p.parseStmt();
    };

    _ = try p.expectToken(.KeywordWhile);
    const lp = try p.expectToken(.LParen);
    const condToken = p.tokenIdx;

    var cond = try p.expect(parseExpr);
    try cond.lvalConversion(p, condToken);
    try cond.usualUnaryConversion(p, condToken);

    if (!cond.ty.isScalar())
        try p.errStr(.statement_scalar, lp + 1, try p.typeStr(cond.ty));

    try cond.saveValue(p);
    try p.expectClosing(lp, .RParen);

    _ = try p.expectToken(.Semicolon);

    return p.addNode(.{
        .doWhileStmt = .{
            .doToken = kwDo,
            .cond = cond.node,
            .body = body,
        },
    });
}

/// goto-statement : `goto` ( identifier | ( '*' expr)) ';'
fn parseGotoStmt(p: *Parser, gotoToken: TokenIndex) Error!Node.Index {
    if (p.eat(.Asterisk)) |_| {
        const exprToken = p.tokenIdx;
        var gotoExpr = try p.expect(parseExpr);
        try gotoExpr.lvalConversion(p, exprToken);
        p.computedGotoTok = p.computedGotoTok orelse gotoToken;
        if (!gotoExpr.ty.isPointer()) {
            const elemType = try p.arena.create(Type);
            elemType.* = .{ .specifier = .Void, .qual = .{ .@"const" = true } };
            const resultType = Type{
                .specifier = .Pointer,
                .data = .{ .subType = elemType },
            };

            if (!gotoExpr.ty.isInt()) {
                try p.errStr(.incompatible_arg, exprToken, try p.typePairStrExtra(gotoExpr.ty, " to parameter of incompatible type ", resultType));
                return error.ParsingFailed;
            }

            if (gotoExpr.value.isZero(p.comp)) {
                try gotoExpr.nullCast(p, resultType, exprToken);
            } else {
                try p.errStr(.implicit_int_to_ptr, exprToken, try p.typePairStrExtra(gotoExpr.ty, " to ", resultType));
                try gotoExpr.ptrCast(p, resultType, exprToken);
            }
        }

        return p.addNode(.{ .computedGotoStmt = .{ .gotoToken = gotoToken, .expr = gotoExpr.node } });
    }

    const nameToken = try p.expectIdentifier();
    const str = p.getTokenText(nameToken);
    if (p.findLabel(str) == null) {
        try p.labels.append(.{ .unresolvedGoto = nameToken });
    }
    _ = try p.expectToken(.Semicolon);
    return try p.addNode(.{ .gotoStmt = .{ .labelToken = nameToken } });
}

/// switch-statement : `switch` '(' expression ')' statement
fn parseSwitchStmt(p: *Parser, kwSwitch: TokenIndex) Error!Node.Index {
    const lp = try p.expectToken(.LParen);
    const condToken = p.tokenIdx;

    var cond = try p.expect(parseExpr);
    try cond.lvalConversion(p, condToken);
    try cond.usualUnaryConversion(p, condToken);

    if (!cond.ty.isInt())
        try p.errStr(.statement_int, lp + 1, try p.typeStr(cond.ty));

    try cond.saveValue(p);
    try p.expectClosing(lp, .RParen);

    const oldSwitch = p.@"switch";
    var @"switch" = Switch{
        .ranges = std.ArrayList(Switch.Range).init(p.gpa),
        .type = cond.ty,
        .comp = p.comp,
    };
    p.@"switch" = &@"switch";

    defer {
        @"switch".ranges.deinit();
        p.@"switch" = oldSwitch;
    }

    const body = try p.parseStmt();

    return p.addNode(.{
        .switchStmt = .{
            .switchToken = kwSwitch,
            .cond = cond.node,
            .body = body,
        },
    });
}

/// case-statement : `case` integer-constant-expression ':'
fn parseCaseStmt(p: *Parser, caseToken: u32) Error!?Node.Index {
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
        if (first.isNone()) {
            try p.errToken(.case_val_unavailable, caseToken + 1);
            break :check;
        } else if (last.isNone()) {
            try p.errToken(.case_val_unavailable, ellipsis + 1);
            break :check;
        } else if (last.compare(.lt, first, p.comp)) {
            try p.errToken(.empty_case_range, caseToken + 1);
            break :check;
        }

        // TODO cast to target type
        const prev = (try some.add(first, last, caseToken + 1)) orelse break :check;

        try p.errStr(.duplicate_switch_case, caseToken + 1, try firstItem.str(p));
        try p.errToken(.previous_case, prev.token);
    } else {
        try p.errStr(.case_not_in_switch, caseToken, "case");
    }

    return try p.addNode(.{
        .caseStmt = .{
            .caseToken = caseToken,
            .start = firstItem.node,
            .end = if (secondItem) |some| some.node else null,
            .body = try p.labelableStmt(),
        },
    });
}

/// default-statement : `default` ':'
fn parseDefaultStmt(p: *Parser, defaultToken: u32) Error!?Node.Index {
    _ = try p.expectToken(.Colon);

    const node = try p.addNode(.{
        .defaultStmt = .{
            .defaultToken = defaultToken,
            .body = try p.labelableStmt(),
        },
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

fn labelableStmt(p: *Parser) Error!Node.Index {
    if (p.currToken() == .RBrace) {
        try p.err(.label_compound_end);
        return p.addNode(.{ .nullStmt = .{
            .semicolonOrRbraceToken = p.tokenIdx,
            .type = Type.Void,
        } });
    }
    return p.parseStmt();
}

/// labeledStmt
/// : identifier ':' statement
/// | case-statement
/// | default-statement
fn parseLabeledStmt(p: *Parser) Error!?Node.Index {
    if ((p.currToken() == .Identifier or
        p.currToken() == .ExtendedIdentifier) and
        p.lookAhead(1) == .Colon)
    {
        const nameToken = try p.expectIdentifier();
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

        return try p.addNode(.{
            .labeledStmt = .{
                .type = try Attribute.applyLabelAttributes(p, attrBufferTop),
                .labelToken = nameToken,
                .body = try p.labelableStmt(),
            },
        });
    } else if (p.eat(.KeywordCase)) |case| {
        return p.parseCaseStmt(case);
    } else if (p.eat(.KeywordDefault)) |default| {
        return p.parseDefaultStmt(default);
    } else return null;
}

const StmtExprState = struct {
    lastExprToken: TokenIndex = 0,
    lastExprType: Type = .Void,
};

/// compound-statement
/// : '{' ( decl | GccExtensionDecl | static-assert-declaration | statememt)* '}'
fn parseCompoundStmt(p: *Parser, isFnBody: bool, stmtExprState: ?*StmtExprState) Error!?Node.Index {
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

        if (stmtExprState) |state| {
            state.* = .{
                .lastExprToken = stmtToken,
                .lastExprType = s.type(&p.tree),
            };
        }
        try p.declBuffer.append(s);

        if (noreturnIdx == null and p.nodeIsNoreturn(s) == .yes) {
            noreturnIdx = p.tokenIdx;
            noreturnLabelCount = p.labelCount;
        }

        switch (s.get(&p.tree)) {
            .caseStmt, .defaultStmt, .labeledStmt => noreturnIdx = null,
            else => {},
        }
    }

    const rbrace = p.tokenIdx - 1;
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
                const internerName = try p.comp.internString(funcName);
                if (internerName == p.stringsIds.mainId and retTy.is(.Int))
                    returnZero = true
                else
                    try p.errStr(.func_does_not_return, p.tokenIdx - 1, funcName);
            }

            const implicitRet = try p.addNode(.{
                .returnStmt = .{
                    .returnToken = rbrace,
                    .returnType = retTy,
                    .operand = .{ .implicit = returnZero },
                },
            });
            try p.declBuffer.append(implicitRet);
        }

        if (p.func.ident) |some| try p.declBuffer.insert(declBufferTop, some.node);
        if (p.func.prettyIdent) |some| try p.declBuffer.insert(declBufferTop, some.node);
    }

    return try p.addNode(.{
        .compoundStmt = .{
            .lbraceToken = lBrace,
            .body = p.declBuffer.items[declBufferTop..],
        },
    });
}

/// return-statement : `return` expression? ';'
fn parseReturnStmt(p: *Parser) Error!?Node.Index {
    const retToken = p.eat(.KeywordReturn) orelse return null;
    const eToken = p.tokenIdx;

    var retExpr = try p.parseExpr();
    _ = try p.expectToken(.Semicolon);
    const returnType = p.func.type.?.getReturnType();

    if (p.func.type.?.hasAttribute(.noreturn))
        try p.errStr(.invalid_noreturn, eToken, p.getTokenText(p.func.name));

    if (retExpr) |*some| {
        if (returnType.is(.Void)) {
            try p.errStr(.void_func_returns_value, eToken, p.getTokenText(p.func.name));
        } else {
            try some.lvalConversion(p, eToken);
            try some.coerce(p, returnType, eToken, .ret);
            try some.saveValue(p);
        }
    } else if (!returnType.is((.Void))) {
        try p.errStr(.func_should_return, retToken, p.getTokenText(p.func.name));
    }

    return try p.addNode(.{
        .returnStmt = .{
            .returnToken = retToken,
            .returnType = returnType,
            .operand = if (retExpr) |some| .{ .expr = some.node } else .none,
        },
    });
}

const NoreturnKind = enum { no, yes, complex };

fn nodeIsNoreturn(p: *Parser, node: Node.Index) NoreturnKind {
    switch (node.get(&p.tree)) {
        .breakStmt, .continueStmt, .returnStmt => return .yes,
        .ifStmt => |@"if"| {
            const thenType = p.nodeIsNoreturn(@"if".elseBody orelse return .no);
            const elseType = p.nodeIsNoreturn(@"if".thenBody);
            if (thenType == .complex or elseType == .complex) return .complex;
            if (thenType == .yes and elseType == .yes) return .yes;
            return .no;
        },

        .compoundStmt => |compound| {
            for (compound.body) |bodyStmt| {
                const kind = p.nodeIsNoreturn(bodyStmt);
                if (kind != .no) return kind;
            }
            return .no;
        },

        .labeledStmt => |labeled| return p.nodeIsNoreturn(labeled.body),
        .defaultStmt => |default| return p.nodeIsNoreturn(default.body),

        .whileStmt, .doWhileStmt, .forStmt, .switchStmt => return .complex,
        else => return .no,
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
        switch (p.currToken()) {
            .LParen, .LBrace, .LBracket => parens += 1,
            .RParen, .RBracket => if (parens != 0) {
                parens -= 1;
            },

            .RBrace => if (parens == 0)
                return
            else {
                parens -= 1;
            },

            .Semicolon => if (parens == 0) {
                p.tokenIdx += 1;
                return;
            },

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
            .KeywordTypeofUnqual,
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
fn expect(p: *Parser, comptime func: fn (*Parser) Error!?Result) Error!Result {
    return p.expectResult(try func(p));
}

fn expectResult(p: *Parser, res: ?Result) Error!Result {
    return res orelse {
        try p.errToken(.expected_expr, p.tokenIdx);
        return error.ParsingFailed;
    };
}

pub fn macroExpr(p: *Parser) Compilation.Error!bool {
    const res = p.expect(parseCondExpr) catch |e| switch (e) {
        error.OutOfMemory => return error.OutOfMemory,
        error.FatalError => return error.FatalError,
        error.ParsingFailed => return false,
    };

    return res.value.toBool(p.comp);
}

/// expression : assign-expression (',' assign-expression)*
fn parseExpr(p: *Parser) Error!?Result {
    var exprStart = p.tokenIdx;
    var errStart = p.comp.diagnostics.list.items.len;
    var lhs = (try p.parseAssignExpr()) orelse {
        if (p.currToken() == .Comma) _ = try p.expectResult(null);
        return null;
    };

    while (p.eat(.Comma)) |comma| {
        try lhs.maybeWarnUnused(p, exprStart, errStart);
        exprStart = p.tokenIdx;
        errStart = p.comp.diagnostics.list.items.len;

        var rhs = try p.expect(parseAssignExpr);
        try rhs.lvalConversion(p, exprStart);
        lhs.value = rhs.value;
        lhs.ty = rhs.ty;
        try lhs.bin(p, .commaExpr, rhs, comma);
    }

    return lhs;
}

fn eatTag(p: *Parser, id: TokenType) ?std.meta.Tag(Node) {
    if (p.eat(id)) |_| return switch (id) {
        .Equal => .assignExpr,
        .AsteriskEqual => .mulAssignExpr,
        .SlashEqual => .divAssignExpr,
        .PercentEqual => .modAssignExpr,
        .PlusEqual => .addAssignExpr,
        .MinusEqual => .subAssignExpr,
        .AngleBracketAngleBracketLeftEqual => .shlAssignExpr,
        .AngleBracketAngleBracketRightEqual => .shrAssignExpr,
        .AmpersandEqual => .bitAndAssignExpr,
        .CaretEqual => .bitXorAssignExpr,
        .PipeEqual => .bitOrAssignExpr,
        .EqualEqual => .equalExpr,
        .BangEqual => .notEqualExpr,
        .AngleBracketLeft => .lessThanExpr,
        .AngleBracketLeftEqual => .lessThanEqualExpr,
        .AngleBracketRight => .greaterThanExpr,
        .AngleBracketRightEqual => .greaterThanEqualExpr,
        .AngleBracketAngleBracketLeft => .shlExpr,
        .AngleBracketAngleBracketRight => .shrExpr,
        .Plus => .addExpr,
        .Minus => .subExpr,
        .Asterisk => .mulExpr,
        .Slash => .divExpr,
        .Percent => .modExpr,
        else => unreachable,
    } else return null;
}

/// assign-expression
///  : conditional-expression
///  | unary-expression assignment-operator  assign-expression
/// assignment-operator
///  : ('=' | '*=' | '/=' | '%=' | '+=' | '-=' | '<<=' | '>>=' | '&=' | '^=' | '|=')
fn parseAssignExpr(p: *Parser) Error!?Result {
    var lhs = (try p.parseCondExpr()) orelse return null;

    const token = p.tokenIdx;
    const tag = p.eatTag(.Equal) orelse
        p.eatTag(.AsteriskEqual) orelse
        p.eatTag(.SlashEqual) orelse
        p.eatTag(.PercentEqual) orelse
        p.eatTag(.PlusEqual) orelse
        p.eatTag(.MinusEqual) orelse
        p.eatTag(.AngleBracketAngleBracketLeftEqual) orelse
        p.eatTag(.AngleBracketAngleBracketRightEqual) orelse
        p.eatTag(.AmpersandEqual) orelse
        p.eatTag(.CaretEqual) orelse
        p.eatTag(.PipeEqual) orelse return lhs;

    var rhs = try p.expect(parseAssignExpr);
    try rhs.lvalConversion(p, token);

    var isConst: bool = undefined;
    if (!p.tree.isLValueExtra(lhs.node, &isConst) or isConst) {
        try p.errToken(.not_assignable, token);
        return error.ParsingFailed;
    }

    // adjustTypes will do do lvalue conversion but we do not want that
    var lhsCopy = lhs;
    switch (tag) {
        .assignExpr => {}, // handle plain assignment separately

        .mulAssignExpr,
        .divAssignExpr,
        .modAssignExpr,
        => {
            if (rhs.value.isZero(p.comp) and lhs.ty.isInt() and rhs.ty.isInt()) {
                switch (tag) {
                    .divAssignExpr => try p.errStr(.division_by_zero, token, "division"),
                    .modAssignExpr => try p.errStr(.division_by_zero, token, "remainder"),
                    else => {},
                }
            }
            _ = try lhsCopy.adjustTypes(token, &rhs, p, if (tag == .modAssignExpr) .integer else .arithmetic);
            try lhs.bin(p, tag, rhs, token);
            return lhs;
        },

        .subAssignExpr,
        .addAssignExpr,
        => {
            if (lhs.ty.isPointer() and rhs.ty.isInt()) {
                try rhs.ptrCast(p, lhs.ty, token);
            } else {
                _ = try lhsCopy.adjustTypes(token, &rhs, p, .arithmetic);
            }
            try lhs.bin(p, tag, rhs, token);
            return lhs;
        },

        .shlAssignExpr,
        .shrAssignExpr,
        .bitAndAssignExpr,
        .bitXorAssignExpr,
        .bitOrAssignExpr,
        => {
            _ = try lhsCopy.adjustTypes(token, &rhs, p, .integer);
            try lhs.bin(p, tag, rhs, token);
            return lhs;
        },
        else => unreachable,
    }

    try rhs.coerce(p, lhs.ty, token, .assign);
    try lhs.bin(p, tag, rhs, token);
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

    const res = try p.expect(parseCondExpr);
    if (res.ty.isInvalid() or res.value.isNone())
        return res;

    // saveValue sets val to unavailable
    var copy = res;
    try copy.saveValue(p);
    return res;
}

/// conditional-expression : logical-OR-expression ('?' expression? ':' conditional-expression)?
fn parseCondExpr(p: *Parser) Error!?Result {
    const condToken = p.tokenIdx;
    var cond = (try p.logicalOrExpr()) orelse return null;
    if (p.eat(.QuestionMark) == null)
        return cond;

    try cond.lvalConversion(p, condToken);
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
        if (!cond.value.isNone() and !cond.value.toBool(p.comp))
            p.noEval = true;

        break :blk try p.expect(parseExpr);
    };

    // If we saw a colon then this is a binary conditional expression.
    if (maybeColon) |colon| {
        var condThen = cond;
        condThen.node = try p.addNode(.{
            .condDummyExpr = .{
                .opToken = colon,
                .operand = cond.node,
                .type = cond.ty,
            },
        });
        _ = try condThen.adjustTypes(colon, &thenExpr, p, .conditional);
        cond.ty = thenExpr.ty;
        cond.node = try p.addNode(.{
            .binaryCondExpr = .{
                .condToken = condToken,
                .cond = cond.node,
                .thenExpr = condThen.node,
                .elseExpr = thenExpr.node,
                .type = cond.ty,
            },
        });
        return cond;
    }

    const colon = try p.expectToken(.Colon);

    var elseExpr = blk: {
        defer p.noEval = savedEval;
        if (!cond.value.isNone() and cond.value.toBool(p.comp))
            p.noEval = true;

        break :blk try p.expect(parseCondExpr);
    };

    _ = try thenExpr.adjustTypes(colon, &elseExpr, p, .conditional);

    if (!cond.value.isNone()) {
        cond.value = if (cond.value.toBool(p.comp)) thenExpr.value else elseExpr.value;
    } else {
        try thenExpr.saveValue(p);
        try elseExpr.saveValue(p);
    }

    cond.ty = thenExpr.ty;
    cond.node = try p.addNode(.{
        .condExpr = .{
            .condToken = condToken,
            .cond = cond.node,
            .type = cond.ty,
            .thenExpr = thenExpr.node,
            .elseExpr = elseExpr.node,
        },
    });

    return cond;
}

/// logical-OR-expression : logical-AND-expression ('||' logical-AND-expression)*
fn logicalOrExpr(p: *Parser) Error!?Result {
    var lhs = (try p.logicalAndExpr()) orelse return null;

    const savedEval = p.noEval;
    defer p.noEval = savedEval;

    while (p.eat(.PipePipe)) |token| {
        if (!lhs.value.isNone() and lhs.value.toBool(p.comp))
            p.noEval = true;

        var rhs = try p.expect(logicalAndExpr);

        if (try lhs.adjustTypes(token, &rhs, p, .booleanLogic)) {
            const res = lhs.value.toBool(p.comp) or rhs.value.toBool(p.comp);
            lhs.value = Value.fromBool(res);
        }

        try lhs.boolRes(p, .boolOrExpr, rhs, token);
    }

    return lhs;
}

/// logical-AND-expression : or-expression ('&&' or-expression)*
fn logicalAndExpr(p: *Parser) Error!?Result {
    var lhs = (try p.parseOrExpr()) orelse return null;

    const savedEval = p.noEval;
    defer p.noEval = savedEval;

    while (p.eat(.AmpersandAmpersand)) |token| {
        if (!lhs.value.isNone() and !lhs.value.toBool(p.comp))
            p.noEval = true;

        var rhs = try p.expect(parseOrExpr);
        if (try lhs.adjustTypes(token, &rhs, p, .booleanLogic)) {
            const res = lhs.value.toBool(p.comp) and rhs.value.toBool(p.comp);
            lhs.value = Value.fromBool(res);
        }

        try lhs.boolRes(p, .boolAndExpr, rhs, token);
    }
    return lhs;
}

/// or-expression : xor-expression ('|' xor-expression)*
fn parseOrExpr(p: *Parser) Error!?Result {
    var lhs = (try p.parseXORExpr()) orelse return null;

    while (p.eat(.Pipe)) |token| {
        var rhs = try p.expect(parseXORExpr);
        if (try lhs.adjustTypes(token, &rhs, p, .integer))
            lhs.value = try lhs.value.bitOr(rhs.value, p.comp);

        try lhs.bin(p, .bitOrExpr, rhs, token);
    }
    return lhs;
}

/// xor-expression : and-expression ('^' and-expression)*
fn parseXORExpr(p: *Parser) Error!?Result {
    var lhs = (try p.parseAndExpr()) orelse return null;

    while (p.eat(.Caret)) |token| {
        var rhs = try p.expect(parseAndExpr);
        if (try lhs.adjustTypes(token, &rhs, p, .integer))
            lhs.value = try lhs.value.bitXor(rhs.value, p.comp);

        try lhs.bin(p, .bitXorExpr, rhs, token);
    }
    return lhs;
}

/// and-expression : equality-expression ('&' equality-expression)*
fn parseAndExpr(p: *Parser) Error!?Result {
    var lhs = (try p.parseEqExpr()) orelse return null;

    while (p.eat(.Ampersand)) |token| {
        var rhs = try p.expect(parseEqExpr);
        if (try lhs.adjustTypes(token, &rhs, p, .integer))
            lhs.value = try lhs.value.bitAnd(rhs.value, p.comp);

        try lhs.bin(p, .bitAndExpr, rhs, token);
    }
    return lhs;
}

/// equality-expression : compare-expression (('==' | '!=') compare-expression)*
fn parseEqExpr(p: *Parser) Error!?Result {
    var lhs = (try p.parseCompExpr()) orelse return null;

    while (true) {
        const tok = p.tokenIdx;
        const tag = p.eatTag(.EqualEqual) orelse
            p.eatTag(.BangEqual) orelse break;

        var rhs = try p.expect(parseCompExpr);
        if (try lhs.adjustTypes(tok, &rhs, p, .equality)) {
            const op: std.math.CompareOperator = if (tag == .equalExpr) .eq else .neq;
            const res = lhs.value.compare(op, rhs.value, p.comp);
            lhs.value = Value.fromBool(res);
        }

        try lhs.boolRes(p, tag, rhs, tok);
    }
    return lhs;
}

/// compare-expression : shirt-expression (('<' | '<=' | '>' | '>=') shirt-expression)*
fn parseCompExpr(p: *Parser) Error!?Result {
    var lhs = (try p.parseShiftExpr()) orelse return null;

    while (true) {
        const tok = p.tokenIdx;
        const tag = p.eatTag(.AngleBracketLeft) orelse
            p.eatTag(.AngleBracketLeftEqual) orelse
            p.eatTag(.AngleBracketRight) orelse
            p.eatTag(.AngleBracketRightEqual) orelse break;

        var rhs = try p.expect(parseShiftExpr);

        if (try lhs.adjustTypes(tok, &rhs, p, .relational)) {
            const op: std.math.CompareOperator = switch (tag) {
                .lessThanExpr => .lt,
                .lessThanEqualExpr => .lte,
                .greaterThanExpr => .gt,
                .greaterThanEqualExpr => .gte,
                else => unreachable,
            };

            const res = lhs.value.compare(op, rhs.value, p.comp);
            lhs.value = Value.fromBool(res);
        }
        try lhs.boolRes(p, tag, rhs, tok);
    }

    return lhs;
}

/// shift-expression : add-expression (('<<' | '>>') add-expression)*
fn parseShiftExpr(p: *Parser) Error!?Result {
    var lhs = (try p.parseAddExpr()) orelse return null;

    while (true) {
        const tok = p.tokenIdx;
        const tag = p.eatTag(.AngleBracketAngleBracketLeft) orelse
            p.eatTag(.AngleBracketAngleBracketRight) orelse break;

        var rhs = try p.expect(parseAddExpr);

        if (try lhs.adjustTypes(tok, &rhs, p, .integer)) {
            if (tag == .shlExpr) {
                if (try lhs.value.shl(lhs.value, rhs.value, lhs.ty, p.comp))
                    try p.errOverflow(tok, lhs);
            } else {
                lhs.value = try lhs.value.shr(rhs.value, lhs.ty, p.comp);
            }
        }

        try lhs.bin(p, tag, rhs, tok);
    }
    return lhs;
}

/// add-expression : mul-expression (('+' | '-') mul-expression)*
fn parseAddExpr(p: *Parser) Error!?Result {
    var lhs = (try p.parseMulExpr()) orelse return null;

    while (true) {
        const tok = p.tokenIdx;
        const tag = p.eatTag(.Plus) orelse
            p.eatTag(.Minus) orelse break;
        var rhs = try p.expect(parseMulExpr);

        const lhsTy = lhs.ty;
        if (try lhs.adjustTypes(tok, &rhs, p, if (tag == .addExpr) .add else .sub)) {
            if (tag == .addExpr) {
                if (try lhs.value.add(lhs.value, rhs.value, lhs.ty, p.comp))
                    try p.errOverflow(tok, lhs);
            } else {
                if (try lhs.value.sub(lhs.value, rhs.value, lhs.ty, p.comp))
                    try p.errOverflow(tok, lhs);
            }
        }

        if (!lhs.ty.isInvalid() and lhsTy.isPointer() and !lhsTy.isVoidStar() and lhsTy.getElemType().hasIncompleteSize()) {
            try p.errStr(.ptr_arithmetic_incomplete, tok, try p.typeStr(lhsTy.getElemType()));
            lhs.ty = Type.Invalid;
        }

        try lhs.bin(p, tag, rhs, tok);
    }
    return lhs;
}

/// mul-expression : cast-expression (('*' | '/' | '%') cast-expression)*´
fn parseMulExpr(p: *Parser) Error!?Result {
    var lhs = (try p.parseCastExpr()) orelse return null;

    while (true) {
        const tok = p.tokenIdx;
        const tag = p.eatTag(.Asterisk) orelse
            p.eatTag(.Slash) orelse
            p.eatTag(.Percent) orelse break;

        var rhs = try p.expect(parseCastExpr);

        if (rhs.value.isZero(p.comp) and tag != .mulExpr and !p.noEval and lhs.ty.isInt() and rhs.ty.isInt()) {
            const errTag: Diagnostics.Tag = if (p.inMacro) .division_by_zero_macro else .division_by_zero;
            lhs.value = .{};
            if (tag == .divExpr)
                try p.errStr(errTag, tok, "division")
            else
                try p.errStr(errTag, tok, "remainder");

            if (p.inMacro)
                return error.ParsingFailed;
        }

        if (try lhs.adjustTypes(tok, &rhs, p, if (tag == .modExpr) .integer else .arithmetic)) {
            switch (tag) {
                .mulExpr => if (try lhs.value.mul(lhs.value, rhs.value, lhs.ty, p.comp))
                    try p.errOverflow(tok, lhs),
                .divExpr => if (try lhs.value.div(lhs.value, rhs.value, lhs.ty, p.comp))
                    try p.errOverflow(tok, lhs),

                .modExpr => {
                    var res = try Value.rem(lhs.value, rhs.value, lhs.ty, p.comp);
                    if (res.isNone()) {
                        if (p.inMacro) {
                            // match clang behavior by defining invalid remainder to be zero in macros
                            res = Value.zero;
                        } else {
                            try lhs.saveValue(p);
                            try rhs.saveValue(p);
                        }
                    }
                    lhs.value = res;
                },
                else => unreachable,
            }
        }

        try lhs.bin(p, tag, rhs, tok);
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
fn parseCastExpr(p: *Parser) Error!?Result {
    if (p.eat(.LParen)) |lp| castExpr: {
        if (p.currToken() == .LBrace) {
            const tok = p.tokenIdx;
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
                .ty = stmtExprState.lastExprType,
            };
            try p.expectClosing(lp, .RParen);
            try res.un(p, .stmtExpr, tok);
            return res;
        }
        const ty = (try p.parseTypeName()) orelse {
            p.tokenIdx -= 1;
            break :castExpr;
        };
        try p.expectClosing(lp, .RParen);

        if (p.currToken() == .LBrace) {
            // compound literal handled in unexpr.
            p.tokenIdx = lp;
            break :castExpr;
        }

        const operandToken = p.tokenIdx;
        var operand = try p.expect(parseCastExpr);
        try operand.lvalConversion(p, operandToken);
        try operand.castType(p, ty, operandToken, lp);

        return operand;
    }

    switch (p.currToken()) {
        .BuiltinChooseExpr => return try p.parseBuiltinChooseExpr(),
        .BuiltinVaArg => return try p.builtinVaArg(),
        .BuiltinOffsetof => return try p.builtinOffsetof(false),
        .BuiltinBitOffsetof => return try p.builtinOffsetof(true),
        .BuiltinTypesCompatibleP => return try p.typesCompatible(),
        // TODO: other special-cased builtins
        else => {},
    }

    return p.parseUnaryExpr();
}

fn typesCompatible(p: *Parser) Error!Result {
    const builtinToken = p.tokenIdx;
    p.tokenIdx += 1;
    const lp = try p.expectToken(.LParen);

    const lhs = (try p.parseTypeName()) orelse {
        try p.err(.expected_type);
        p.skipTo(.RParen);
        return error.ParsingFailed;
    };
    _ = try p.expectToken(.Comma);

    const rhs = (try p.parseTypeName()) orelse {
        try p.err(.expected_type);
        p.skipTo(.RParen);
        return error.ParsingFailed;
    };

    try p.expectClosing(lp, .RParen);

    var firstUnqual = lhs.canonicalize(.standard);
    firstUnqual.qual.removeCVQualifiers();

    var secondUnqual = rhs.canonicalize(.standard);
    secondUnqual.qual.removeCVQualifiers();

    const compatible = firstUnqual.eql(secondUnqual, p.comp, true);

    const res: Result = .{
        .value = Value.fromBool(compatible),
        .ty = Type.Int,
        .node = try p.addNode(.{
            .builtinTypesCompatibleP = .{
                .builtinToken = builtinToken,
                .lhs = lhs,
                .rhs = rhs,
            },
        }),
    };

    try res.putValue(p);
    return res;
}

fn parseBuiltinChooseExpr(p: *Parser) Error!Result {
    p.tokenIdx += 1;
    const lp = try p.expectToken(.LParen);
    const condToken = p.tokenIdx;
    var cond = try p.parseIntegerConstExpr(.NoConstDeclFolding);
    if (cond.value.isNone()) {
        try p.errToken(.builtin_choose_cond, condToken);
        return error.ParsingFailed;
    }

    _ = try p.expectToken(.Comma);

    const thenExpr = if (cond.value.toBool(p.comp))
        try p.expect(parseAssignExpr)
    else
        try p.parseNoEval(parseAssignExpr);

    _ = try p.expectToken(.Comma);

    const elseExpr = if (!cond.value.toBool(p.comp))
        try p.expect(parseAssignExpr)
    else
        try p.parseNoEval(parseAssignExpr);

    try p.expectClosing(lp, .RParen);

    if (cond.value.toBool(p.comp)) {
        cond.value = thenExpr.value;
        cond.ty = thenExpr.ty;
    } else {
        cond.value = elseExpr.value;
        cond.ty = elseExpr.ty;
    }
    cond.node = try p.addNode(.{
        .builtinChooseExpr = .{
            .condToken = condToken,
            .type = cond.ty,
            .cond = cond.node,
            .thenExpr = thenExpr.node,
            .elseExpr = elseExpr.node,
        },
    });

    return cond;
}

fn builtinVaArg(p: *Parser) Error!Result {
    const builtinToken = p.tokenIdx;
    p.tokenIdx += 1;

    const lp = try p.expectToken(.LParen);
    const vaListToken = p.tokenIdx;
    var vaList = try p.expect(parseAssignExpr);
    try vaList.lvalConversion(p, vaListToken);

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

    return .{
        .ty = ty,
        .node = try p.addNode(.{
            .builtinCallExpr = .{
                .builtinToken = builtinToken,
                .type = ty,
                .args = &.{vaList.node},
            },
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

    const offsetofExpr = try p.offsetofMemberDesignator(ty, wantsBits, builtinToken);

    try p.expectClosing(lparen, .RParen);

    return .{
        .ty = p.comp.types.size,
        .value = offsetofExpr.value,
        .node = try p.addNode(.{
            .builtinCallExpr = .{
                .builtinToken = builtinToken,
                .type = p.comp.types.size,
                .args = &.{offsetofExpr.node},
            },
        }),
    };
}

/// offsetofMemberDesignator: Identifier ('.' Identifier | '[' expr ']' )*
fn offsetofMemberDesignator(
    p: *Parser,
    baseTy: Type,
    wantBits: bool,
    accessToken: TokenIndex,
) Error!Result {
    errdefer p.skipTo(.RParen);
    const baseFieldNameToken = try p.expectIdentifier();
    const baseFieldName = try p.getInternString(baseFieldNameToken);
    const baseRecordTy = baseTy.getRecord().?;

    try p.validateFieldAccess(baseRecordTy, baseTy, baseFieldNameToken, baseFieldName);

    const baseNode = try p.addNode(.{
        .defaultInitExpr = .{
            .lastToken = p.tokenIdx,
            .type = baseTy,
        },
    });

    var curOffset: u64 = 0;
    var lhs = try p.fieldAccessExtra(
        baseNode,
        baseRecordTy,
        baseFieldName,
        false,
        accessToken,
        &curOffset,
    );

    var totalOffset = curOffset;
    while (true) switch (p.currToken()) {
        .Period => {
            p.tokenIdx += 1;
            const fieldNameToken = try p.expectIdentifier();
            const fieldName = try p.getInternString(fieldNameToken);

            const lhsRecordTy = lhs.ty.getRecord() orelse {
                try p.errStr(.offsetof_ty, fieldNameToken, try p.typeStr(lhs.ty));
                return error.ParsingFailed;
            };

            try p.validateFieldAccess(lhsRecordTy, lhs.ty, fieldNameToken, fieldName);
            lhs = try p.fieldAccessExtra(
                lhs.node,
                lhsRecordTy,
                fieldName,
                false,
                accessToken,
                &curOffset,
            );
            totalOffset += curOffset;
        },
        .LBracket => {
            const lbracket = p.tokenIdx;
            p.tokenIdx += 1;
            var index = try p.expect(parseExpr);
            _ = try p.expectClosing(lbracket, .RBracket);

            if (!lhs.ty.isArray()) {
                try p.errStr(.offsetof_array, lbracket, try p.typeStr(lhs.ty));
                return error.ParsingFailed;
            }

            var ptr = lhs;
            try ptr.lvalConversion(p, lbracket);
            try index.lvalConversion(p, lbracket);

            if (!index.ty.isInt())
                try p.errToken(.invalid_index, lbracket);
            try p.checkArrayBounds(index, lhs, lbracket);

            try index.saveValue(p);
            try ptr.bin(p, .arrayAccessExpr, index, lbracket);
            lhs = ptr;
        },
        else => break,
    };

    const val = try Value.int(if (wantBits) totalOffset else totalOffset / 8, p.comp);
    return .{ .ty = baseTy, .value = val, .node = lhs.node };
}

/// unaryExpr
///  : (compoundLiteral | primaryExpr) suffix-expression*
///  | '&&' identifier
///  | ('&' | '*' | '+' | '-' | '~' | '!' | '++' | '--' | `__extension__` | `__imag__` | `__real__`) cast-expression
///  | `sizeof` unary-expression
///  | `sizeof` '(' type-name ')'
///  | (`keyword-alignof` | `keyword-c23-alignof`) '(' type-name ')'
fn parseUnaryExpr(p: *Parser) Error!?Result {
    const token = p.tokenIdx;
    switch (p.currToken()) {
        .Ampersand => {
            if (p.inMacro) {
                try p.err(.invalid_preproc_operator);
                return error.ParsingFailed;
            }
            p.tokenIdx += 1;

            var operand = try p.expect(parseCastExpr);
            if (p.getNode(operand.node, .memberAccessExpr) orelse
                p.getNode(operand.node, .memberAccessPtrExpr)) |access|
            {
                if (access.isBitFieldWidth(&p.tree) != null)
                    try p.errToken(.addr_of_bitfield, token);
            }

            if (!p.tree.isLValue(operand.node))
                try p.errToken(.addr_of_rvalue, token);

            if (p.getNode(operand.node, .declRefExpr)) |declRef| {
                switch (declRef.decl.get(&p.tree)) {
                    .variable => |variable| {
                        if (variable.storageClass == .register)
                            try p.errToken(.addr_of_register, token);
                    },
                    else => {},
                }
            } else if (p.getNode(operand.node, .compoundLiteralExpr)) |literal| {
                switch (literal.storageClass) {
                    .register => try p.errToken(.addr_of_register, token),
                    else => {},
                }
            }

            const elemType = try p.arena.create(Type);
            elemType.* = operand.ty;
            operand.ty = Type{
                .specifier = .Pointer,
                .data = .{ .subType = elemType },
            };

            try operand.saveValue(p);
            try operand.un(p, .addrOfExpr, token);
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
                    .addrOfLabel = .{
                        .labelToken = nameToken,
                        .type = resultType,
                    },
                }),
                .ty = resultType,
            };
        },

        .Asterisk => {
            p.tokenIdx += 1;

            var operand = try p.expect(parseCastExpr);
            if (operand.ty.isArray() or operand.ty.isPointer() or operand.ty.isFunc()) {
                try operand.lvalConversion(p, token);
                operand.ty = operand.ty.getElemType();
            } else {
                try p.errToken(.indirection_ptr, token);
            }

            if (operand.ty.hasIncompleteSize() and !operand.ty.is(.Void))
                try p.errStr(.deref_incomplete_ty_ptr, token, try p.typeStr(operand.ty));

            operand.ty.qual = .{};
            try operand.un(p, .derefExpr, token);
            return operand;
        },

        .Plus => {
            p.tokenIdx += 1;

            var operand = try p.expect(parseCastExpr);
            try operand.lvalConversion(p, token);

            if (!operand.ty.isInt() and !operand.ty.isFloat())
                try p.errStr(.invalid_argument_un, token, try p.typeStr(operand.ty));

            try operand.usualUnaryConversion(p, token);

            return operand;
        },

        .Minus => {
            p.tokenIdx += 1;

            var operand = try p.expect(parseCastExpr);
            try operand.lvalConversion(p, token);

            if (!operand.ty.isInt() and !operand.ty.isFloat())
                try p.errStr(.invalid_argument_un, token, try p.typeStr(operand.ty));

            try operand.usualUnaryConversion(p, token);
            if (operand.value.isNumeric(p.comp))
                _ = try operand.value.sub(Value.zero, operand.value, operand.ty, p.comp)
            else
                operand.value = .{};

            try operand.un(p, .negateExpr, token);
            return operand;
        },

        .PlusPlus => {
            p.tokenIdx += 1;

            var operand = try p.expect(parseCastExpr);
            if (!operand.ty.isScalar())
                try p.errStr(.invalid_argument_un, token, try p.typeStr(operand.ty));
            if (operand.ty.isComplex())
                try p.errStr(.complex_prefix_postfix_op, p.tokenIdx, try p.typeStr(operand.ty));

            if (!p.tree.isLValue(operand.node) or operand.ty.isConst()) {
                try p.errToken(.not_assignable, token);
                return error.ParsingFailed;
            }

            try operand.usualUnaryConversion(p, token);

            if (operand.value.isNumeric(p.comp)) {
                if (try operand.value.add(operand.value, Value.one, operand.ty, p.comp))
                    try p.errOverflow(token, operand);
            } else {
                operand.value = .{};
            }

            try operand.un(p, .preIncExpr, token);
            return operand;
        },

        .MinusMinus => {
            p.tokenIdx += 1;

            var operand = try p.expect(parseCastExpr);
            if (!operand.ty.isScalar())
                try p.errStr(.invalid_argument_un, token, try p.typeStr(operand.ty));
            if (operand.ty.isComplex())
                try p.errStr(.complex_prefix_postfix_op, p.tokenIdx, try p.typeStr(operand.ty));

            if (!p.tree.isLValue(operand.node) or operand.ty.isConst()) {
                try p.errToken(.not_assignable, token);
                return error.ParsingFailed;
            }

            try operand.usualUnaryConversion(p, token);

            if (operand.value.isNumeric(p.comp)) {
                if (try operand.value.sub(operand.value, Value.one, operand.ty, p.comp))
                    try p.errOverflow(token, operand);
            } else {
                operand.value = .{};
            }

            try operand.un(p, .preDecExpr, token);
            return operand;
        },

        .Tilde => {
            p.tokenIdx += 1;

            var operand = try p.expect(parseCastExpr);
            try operand.lvalConversion(p, token);
            try operand.usualUnaryConversion(p, token);

            if (operand.ty.isInt()) {
                try operand.intCast(p, operand.ty.integerPromotion(p.comp), token);
                if (operand.value.is(.int, p.comp)) {
                    operand.value = try operand.value.bitNot(operand.ty, p.comp);
                }
            } else {
                try p.errStr(.invalid_argument_un, token, try p.typeStr(operand.ty));
                operand.value = .{};
            }

            try operand.un(p, .bitNotExpr, token);
            return operand;
        },

        .Bang => {
            p.tokenIdx += 1;

            var operand = try p.expect(parseCastExpr);
            try operand.lvalConversion(p, token);

            if (!operand.ty.isScalar())
                try p.errStr(.invalid_argument_un, token, try p.typeStr(operand.ty));

            try operand.usualUnaryConversion(p, token);

            if (operand.value.is(.int, p.comp)) {
                const res = Value.fromBool(!operand.value.toBool(p.comp));
                operand.value = res;
            } else if (operand.value.isNull()) {
                operand.value = Value.one;
            } else {
                if (operand.ty.isDecayed())
                    operand.value = Value.zero
                else
                    operand.value = .{};
            }

            operand.ty = Type.Int;
            try operand.un(p, .boolNotExpr, token);
            return operand;
        },

        .KeywordSizeof => {
            p.tokenIdx += 1;
            const expectedParen = p.tokenIdx;

            var hasExpr = false;
            var res: Result = .{ .node = undefined }; // check has expr

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
                    hasExpr = true;
                }
            } else {
                res = try p.parseNoEval(parseUnaryExpr);
                hasExpr = true;
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
                res.value = try Value.int(size, p.comp);
                res.ty = p.comp.types.size;
            } else {
                res.value = .{};
                if (res.ty.hasIncompleteSize()) {
                    try p.errStr(.invalid_sizeof, expectedParen - 1, try p.typeStr(res.ty));
                    res.ty = Type.Invalid;
                } else {
                    res.ty = p.comp.types.size;
                }
            }
            res.node = try p.addNode(.{
                .sizeofExpr = .{
                    .opToken = token,
                    .type = res.ty,
                    .expr = if (hasExpr) res.node else null,
                },
            });
            return res;
        },

        .KeywordAlignof,
        .KeywordGccAlignof1,
        .KeywordGccAlignof2,
        .KeywordC23Alignof,
        => {
            p.tokenIdx += 1;
            const expectedParen = p.tokenIdx;

            var hasExpr = false;
            var res: Result = .{ .node = undefined }; // check has expr

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
                    hasExpr = true;
                    try p.errToken(.alignof_expr, expectedParen);
                }
            } else {
                res = try p.parseNoEval(parseUnaryExpr);
                hasExpr = true;

                try p.errToken(.alignof_expr, expectedParen);
            }

            if (res.ty.is(.Void))
                try p.errStr(.pointer_arith_void, token, "alignof");

            if (res.ty.alignable()) {
                res.value = try Value.int(res.ty.alignof(p.comp), p.comp);
                res.ty = p.comp.types.size;
            } else {
                try p.errStr(.invalid_alignof, expectedParen, try p.typeStr(res.ty));
                return error.ParsingFailed;
            }

            res.node = try p.addNode(.{
                .alignofExpr = .{
                    .opToken = token,
                    .type = res.ty,
                    .expr = if (hasExpr) res.node else null,
                },
            });
            return res;
        },

        .KeywordGccExtension => {
            p.tokenIdx += 1;
            const savedExtension = p.extensionSuppressd;
            defer p.extensionSuppressd = savedExtension;

            p.extensionSuppressd = true;
            return try p.expect(parseCastExpr);
        },

        .KeywordImag1, .KeywordImag2 => {
            const imagToken = p.tokenIdx;
            p.tokenIdx += 1;

            var operand = try p.expect(parseCastExpr);
            try operand.lvalConversion(p, token);
            if (!operand.ty.isInt() and !operand.ty.isFloat()) {
                try p.errStr(.invalid_imag, imagToken, try p.typeStr(operand.ty));
            }

            if (operand.ty.isReal()) {
                switch (p.comp.langOpts.emulate) {
                    .msvc => {},
                    .gcc => operand.value = Value.zero,
                    .clang => {
                        if (operand.value.is(.int, p.comp) or operand.value.is(.float, p.comp))
                            operand.value = Value.zero;
                    },
                }
            }

            // convert _Complex F to F
            operand.ty = operand.ty.makeReal();

            try operand.un(p, .imagExpr, token);
            return operand;
        },

        .KeywordReal1, .KeywordReal2 => {
            const realToken = p.tokenIdx;
            p.tokenIdx += 1;

            var operand = try p.expect(parseCastExpr);
            try operand.lvalConversion(p, token);
            if (!operand.ty.isInt() and !operand.ty.isFloat()) {
                try p.errStr(.invalid_real, realToken, try p.typeStr(operand.ty));
            }

            operand.ty = operand.ty.makeReal();

            try operand.un(p, .realExpr, token);
            return operand;
        },

        else => {
            var lhs = (try p.parseCompoundLiteral()) orelse
                (try p.parsePrimaryExpr()) orelse
                return null;

            while (try p.parseSuffixExpr(lhs)) |suffix| {
                lhs = suffix;
            }
            return lhs;
        },
    }
}

/// compoundLiteral
///  : '(' storage-class-spec* type-name ')' '{' initializer-list '}'
///  | '(' storage-class-spec* type-name ')' '{' initializer-list ',' '}'
fn parseCompoundLiteral(p: *Parser) Error!?Result {
    const lparen = p.eat(.LParen) orelse return null;

    var d: DeclSpec = .{ .type = .{ .specifier = undefined } };
    const any = if (p.comp.langOpts.standard.atLeast(.c23))
        try p.parseStorageClassSpec(&d)
    else
        false;

    switch (d.storageClass) {
        .auto, .@"extern", .typedef => |tok| {
            try p.errStr(.invalid_compound_literal_storage_class, tok, @tagName(d.storageClass));
            d.storageClass = .none;
        },
        .register => if (p.func.type == null) try p.err(.illegal_storage_on_global),
        else => {},
    }

    var ty = (try p.parseTypeName()) orelse {
        p.tokenIdx = lparen;
        if (any) {
            try p.err(.expected_type);
            return error.ParsingFailed;
        }
        return null;
    };
    try p.expectClosing(lparen, .RParen);

    if (ty.isFunc()) {
        try p.err(.func_init);
    } else if (ty.is(.VariableLenArray)) {
        try p.err(.vla_init);
    } else if (ty.hasIncompleteSize() and !ty.is(.IncompleteArray)) {
        try p.errStr(.variable_incomplete_ty, p.tokenIdx, try p.typeStr(ty));
        return error.ParsingFailed;
    }

    var initlistExpr = try p.initializer(ty);
    if (d.constexpr) |_| {
        //TODO error if not constexpr
    }

    initlistExpr.node = try p.addNode(.{
        .compoundLiteralExpr = .{
            .lparenToken = lparen,
            .storageClass = switch (d.storageClass) {
                .register => .register,
                .static => .static,
                else => .auto,
            },
            .threadLocal = d.threadLocal != null,
            .initializer = initlistExpr.node,
            .type = initlistExpr.ty,
        },
    });
    return initlistExpr;
}

/// suffix-expression
///  : '[' expression ']'
///  | '(' argument-expression-list? ')'
///  | '.' identifier
///  | '->' identifier
///  | '++'
///  | '--'
fn parseSuffixExpr(p: *Parser, lhs: Result) Error!?Result {
    switch (p.currToken()) {
        .LBracket => {
            const lb = p.tokenIdx;
            p.tokenIdx += 1;
            var index = try p.expect(parseExpr);
            try p.expectClosing(lb, .RBracket);

            const arrayBeforeConversion = lhs;
            const indexBeforeConversion = index;

            var ptr = lhs;
            try ptr.lvalConversion(p, lb);
            try index.lvalConversion(p, lb);

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

            ptr.node = try p.addNode(.{
                .arrayAccessExpr = .{
                    .lbracketToken = lb,
                    .base = ptr.node,
                    .index = index.node,
                    .type = ptr.ty,
                },
            });

            return ptr;
        },

        .LParen => return p.parseCallExpr(lhs),

        .Period => {
            const period = p.tokenIdx;
            p.tokenIdx += 1;
            const name = try p.expectIdentifier();
            return try p.fieldAccess(lhs, name, false, period);
        },

        .Arrow => {
            const arrow = p.tokenIdx;
            p.tokenIdx += 1;
            const name = try p.expectIdentifier();
            if (lhs.ty.isArray()) {
                var copy = lhs;
                copy.ty.decayArray();
                try copy.implicitCast(p, .ArrayToPointer, arrow);
                return try p.fieldAccess(copy, name, true, arrow);
            }
            return try p.fieldAccess(lhs, name, true, arrow);
        },

        .PlusPlus => {
            defer p.tokenIdx += 1;
            var operand = lhs;

            if (!operand.ty.isScalar())
                try p.errStr(.invalid_argument_un, p.tokenIdx, try p.typeStr(operand.ty));

            if (operand.ty.isComplex())
                try p.errStr(.complex_prefix_postfix_op, p.tokenIdx, try p.typeStr(operand.ty));

            if (!p.tree.isLValue(operand.node) or operand.ty.isConst()) {
                try p.err(.not_assignable);
                return error.ParsingFailed;
            }

            try operand.usualUnaryConversion(p, p.tokenIdx);
            try operand.un(p, .postIncExpr, p.tokenIdx);
            return operand;
        },

        .MinusMinus => {
            defer p.tokenIdx += 1;
            var operand = lhs;

            if (!operand.ty.isScalar())
                try p.errStr(.invalid_argument_un, p.tokenIdx, try p.typeStr(operand.ty));

            if (operand.ty.isComplex())
                try p.errStr(.complex_prefix_postfix_op, p.tokenIdx, try p.typeStr(operand.ty));

            if (!p.tree.isLValue(operand.node) or operand.ty.isConst()) {
                try p.err(.not_assignable);
                return error.ParsingFailed;
            }

            try operand.usualUnaryConversion(p, p.tokenIdx);

            try operand.un(p, .postDecExpr, p.tokenIdx);
            return operand;
        },

        else => return null,
    }
}

fn fieldAccess(
    p: *Parser,
    lhs: Result,
    fieldNameToken: TokenIndex,
    isArrow: bool,
    accessToken: TokenIndex,
) !Result {
    const exprType = lhs.ty;
    const isPtr = exprType.isPointer();
    const exprBaseType = if (isPtr) exprType.getElemType() else exprType;
    const recordType = exprBaseType.getRecord() orelse {
        try p.errStr(.expected_record_ty, fieldNameToken, try p.typeStr(exprType));
        return error.ParsingFailed;
    };

    if (recordType.isIncomplete()) {
        try p.errStr(.deref_incomplete_ty_ptr, fieldNameToken - 2, try p.typeStr(exprBaseType));
        return error.ParsingFailed;
    }

    if (isArrow and !isPtr) try p.errStr(.member_expr_not_ptr, fieldNameToken, try p.typeStr(exprType));
    if (!isArrow and isPtr) try p.errStr(.member_expr_ptr, fieldNameToken, try p.typeStr(exprType));

    const fieldName = try p.getInternString(fieldNameToken);
    try p.validateFieldAccess(recordType, exprType, fieldNameToken, fieldName);
    var discard: u64 = 0;
    return p.fieldAccessExtra(lhs.node, recordType, fieldName, isArrow, accessToken, &discard);
}

fn validateFieldAccess(
    p: *Parser,
    recordType: *const Type.Record,
    exprType: Type,
    fieldNameToken: TokenIndex,
    fieldName: StringId,
) Error!void {
    if (recordType.hasField(fieldName))
        return;

    p.strings.items.len = 0;

    try p.strings.writer().print("'{s}' in '", .{p.getTokenText(fieldNameToken)});
    try exprType.print(p.comp, p.strings.writer());
    try p.strings.append('\'');

    const duped = try p.comp.diagnostics.arena.allocator().dupe(u8, p.strings.items);
    try p.errStr(.no_such_member, fieldNameToken, duped);
    return error.ParsingFailed;
}

fn fieldAccessExtra(
    p: *Parser,
    base: Node.Index,
    recordType: *const Type.Record,
    fieldName: StringId,
    isArrow: bool, // is arrow operator
    accessToken: TokenIndex,
    offsetBits: *u64,
) Error!Result {
    for (recordType.fields, 0..) |f, i| {
        if (f.isAnonymousRecord()) {
            if (!f.ty.hasField(fieldName)) continue;

            const access: Node.MemberAccess = .{
                .accessToken = accessToken,
                .type = f.ty,
                .base = base,
                .memberIndex = @intCast(i),
            };
            const inner = try p.addNode(if (isArrow)
                .{ .memberAccessPtrExpr = access }
            else
                .{ .memberAccessExpr = access });

            const res = p.fieldAccessExtra(
                inner,
                f.ty.getRecord().?,
                fieldName,
                false,
                accessToken,
                offsetBits,
            );
            offsetBits.* = f.layout.offsetBits;
            return res;
        }

        if (fieldName == f.name) {
            offsetBits.* = f.layout.offsetBits;
            const access: Node.MemberAccess = .{
                .accessToken = accessToken,
                .type = f.ty,
                .base = base,
                .memberIndex = @intCast(i),
            };

            return .{ .ty = f.ty, .node = try p.addNode(if (isArrow)
                .{ .memberAccessPtrExpr = access }
            else
                .{ .memberAccessExpr = access }) };
        }
    }
    // We already checked that this container has a field by the name.
    unreachable;
}

fn checkVaStartArg(
    p: *Parser,
    builtinToken: TokenIndex,
    firstAfter: TokenIndex,
    paramToken: TokenIndex,
    arg: *Result,
    idx: u32,
) !void {
    assert(idx != 0);
    if (idx > 1) {
        try p.errToken(.closing_paren, firstAfter);
        return error.ParsingFailed;
    }

    var funcTy = p.func.type orelse {
        try p.errToken(.va_start_not_in_func, builtinToken);
        return;
    };
    const funcParams = funcTy.getParams();
    if (funcTy.specifier != .VarArgsFunc or funcParams.len == 0) {
        return p.errToken(.va_start_fixed_args, builtinToken);
    }
    const lastParamName = funcParams[funcParams.len - 1].name;
    const declRef = p.getNode(arg.node, .declRefExpr);
    if (declRef == null or lastParamName != try p.getInternString(declRef.?.nameToken)) {
        try p.errToken(.va_start_not_last_param, paramToken);
    }
}

fn checkComplexArg(
    p: *Parser,
    builtinTok: TokenIndex,
    firstAfter: TokenIndex,
    paramTok: TokenIndex,
    arg: *Result,
    idx: u32,
) !void {
    _ = builtinTok;
    _ = firstAfter;
    if (idx <= 1 and !arg.ty.isFloat()) {
        try p.errStr(.not_floating_type, paramTok, try p.typeStr(arg.ty));
    } else if (idx == 1) {
        const prevIdx = p.listBuffer.items[p.listBuffer.items.len - 1];
        const prevTy = prevIdx.type(&p.tree);
        if (!prevTy.eql(arg.ty, p.comp, false)) {
            try p.errStr(.argument_types_differ, paramTok, try p.typePairStrExtra(prevTy, " vs ", arg.ty));
        }
    }
}

const CallExpr = union(enum) {
    standard: Node.Index,
    builtin: struct {
        builtinToken: TokenIndex,
        tag: Builtin.Tag,
    },

    fn init(p: *Parser, callNode: Node.Index, funcNode: Node.Index) CallExpr {
        if (p.getNode(callNode, .builtinRef)) |builtinRef| {
            const name = p.getTokenText(builtinRef.nameToken);
            const expanded = p.comp.builtins.lookup(name);
            return .{ .builtin = .{ .builtinToken = builtinRef.nameToken, .tag = expanded.builtin.tag } };
        }
        return .{ .standard = funcNode };
    }

    fn shouldPerformLvalConversion(self: CallExpr, argIdx: u32) bool {
        return switch (self) {
            .standard => true,
            .builtin => |builtin| switch (builtin.tag) {
                .__builtin_va_start, .__va_start, .va_start => argIdx != 1,
                else => true,
            },
        };
    }

    fn shouldPromoteVarArg(self: CallExpr, argIdx: u32) bool {
        return switch (self) {
            .standard => true,
            .builtin => |builtin| switch (builtin.tag) {
                .__builtin_va_start, .__va_start, .va_start => argIdx != 1,
                .__builtin_complex => false,
                else => true,
            },
        };
    }

    fn shouldCoerceArg(self: CallExpr, argIdx: u32) bool {
        _ = self;
        _ = argIdx;
        return true;
    }

    fn checkVarArg(self: CallExpr, p: *Parser, firstAfter: TokenIndex, paramToken: TokenIndex, arg: *Result, argIdx: u32) !void {
        if (self == .standard) return;

        const builtinToken = self.builtin.builtinToken;
        switch (self.builtin.tag) {
            .__builtin_va_start, .__va_start, .va_start => return p.checkVaStartArg(builtinToken, firstAfter, paramToken, arg, argIdx),
            .__builtin_complex => return p.checkComplexArg(builtinToken, firstAfter, paramToken, arg, argIdx),
            else => {},
        }
    }

    /// Some functions cannot be expressed as standard C prototypes. For example `__builtin_complex` requires
    /// two arguments of the same real floating point type (e.g. two doubles or two floats). These functions are
    /// encoded as varargs functions with custom typechecking. Since varargs functions do not have a fixed number
    /// of arguments, `paramCountOverride` is used to tell us how many arguments we should actually expect to see for
    /// these custom-typechecked functions.
    fn paramCountOverride(self: CallExpr) ?u32 {
        return switch (self) {
            .standard => null,
            .builtin => |builtin| switch (builtin.tag) {
                .__c11_atomic_thread_fence,
                .__c11_atomic_signal_fence,
                .__c11_atomic_is_lock_free,
                => 1,

                .__builtin_complex,
                .__c11_atomic_load,
                .__c11_atomic_init,
                => 2,

                .__c11_atomic_store,
                .__c11_atomic_exchange,
                .__c11_atomic_fetch_add,
                .__c11_atomic_fetch_sub,
                .__c11_atomic_fetch_or,
                .__c11_atomic_fetch_xor,
                .__c11_atomic_fetch_and,
                .__atomic_fetch_add,
                .__atomic_fetch_sub,
                .__atomic_fetch_and,
                .__atomic_fetch_xor,
                .__atomic_fetch_or,
                .__atomic_fetch_nand,
                .__atomic_add_fetch,
                .__atomic_sub_fetch,
                .__atomic_and_fetch,
                .__atomic_xor_fetch,
                .__atomic_or_fetch,
                .__atomic_nand_fetch,
                => 3,

                .__c11_atomic_compare_exchange_strong,
                .__c11_atomic_compare_exchange_weak,
                => 5,

                .__atomic_compare_exchange,
                .__atomic_compare_exchange_n,
                => 6,

                else => null,
            },
        };
    }

    fn returnType(self: CallExpr, p: *Parser, callableTy: Type) Type {
        return switch (self) {
            .standard => callableTy.getReturnType(),
            .builtin => |builtin| switch (builtin.tag) {
                .__builtin_complex => {
                    if (p.listBuffer.items.len < 1) return Type.Invalid;
                    const lastParam = p.listBuffer.items[p.listBuffer.items.len - 1];
                    return lastParam.type(&p.tree).makeComplex();
                },

                .__c11_atomic_load => {
                    if (p.listBuffer.items.len != 3) return Type.Invalid;
                    const secondParam = p.listBuffer.items[1];
                    const ty = secondParam.type(&p.tree);
                    if (!ty.isPointer()) return Type.Invalid;
                    return ty.getElemType();
                },

                .__c11_atomic_exchange => {
                    if (p.listBuffer.items.len != 4) return Type.Invalid;
                    const secondParam = p.listBuffer.items[2];
                    return secondParam.type(&p.tree);
                },

                .__atomic_fetch_add,
                .__atomic_add_fetch,
                .__c11_atomic_fetch_add,

                .__atomic_fetch_sub,
                .__atomic_sub_fetch,
                .__c11_atomic_fetch_sub,

                .__atomic_fetch_and,
                .__atomic_and_fetch,
                .__c11_atomic_fetch_and,

                .__atomic_fetch_xor,
                .__atomic_xor_fetch,
                .__c11_atomic_fetch_xor,

                .__atomic_fetch_or,
                .__atomic_or_fetch,
                .__c11_atomic_fetch_or,

                .__atomic_fetch_nand,
                .__atomic_nand_fetch,
                .__c11_atomic_fetch_nand,
                => {
                    if (p.listBuffer.items.len != 3) return Type.Invalid;
                    const secondParam = p.listBuffer.items[2];
                    return secondParam.type(&p.tree);
                },

                .__c11_atomic_compare_exchange_strong,
                .__c11_atomic_compare_exchange_weak,
                => {
                    if (p.listBuffer.items.len != 6) return Type.Invalid;
                    const thirdParam = p.listBuffer.items[3];
                    return thirdParam.type(&p.tree);
                },

                .__atomic_compare_exchange,
                .__atomic_compare_exchange_n,
                .__c11_atomic_is_lock_free,
                => Type.Bool,

                else => callableTy.getReturnType(),
            },
        };
    }

    fn finish(self: CallExpr, p: *Parser, ty: Type, listBufferTop: usize, lparen: TokenIndex) Error!?Result {
        const retTy = self.returnType(p, ty);
        const args = p.listBuffer.items[listBufferTop..];
        switch (self) {
            .standard => |funcNode| return .{
                .ty = retTy,
                .node = try p.addNode(.{
                    .callExpr = .{
                        .lparenToken = lparen,
                        .type = retTy,
                        .callee = funcNode,
                        .args = args,
                    },
                }),
            },
            .builtin => |builtin| return .{
                .ty = retTy,
                .node = try p.addNode(.{
                    .builtinCallExpr = .{
                        .builtinToken = builtin.builtinToken,
                        .type = retTy,
                        .args = args,
                    },
                }),
            },
        }
    }
};

fn parseCallExpr(p: *Parser, lhs: Result) Error!?Result {
    const lParen = p.tokenIdx;
    p.tokenIdx += 1;
    const ty = lhs.ty.isCallable() orelse {
        try p.errStr(.not_callable, lParen, try p.typeStr(lhs.ty));
        return error.ParsingFailed;
    };

    const params = ty.getParams();
    var func = lhs;
    try func.lvalConversion(p, lParen);

    const listBufferTop = p.listBuffer.items.len;
    defer p.listBuffer.items.len = listBufferTop;

    var argCount: u32 = 0;
    var firstAfter = lParen;

    const callExpr = CallExpr.init(p, lhs.node, func.node);

    while (p.eat(.RParen) == null) {
        const paramToken = p.tokenIdx;
        if (argCount == params.len)
            firstAfter = p.tokenIdx;

        var arg = try p.expect(parseAssignExpr);

        if (callExpr.shouldPerformLvalConversion(argCount))
            try arg.lvalConversion(p, paramToken);

        if (arg.ty.hasIncompleteSize() and !arg.ty.is(.Void))
            return error.ParsingFailed;

        if (argCount >= params.len) {
            if (callExpr.shouldPromoteVarArg(argCount)) {
                if (arg.ty.isInt()) try arg.intCast(p, arg.ty.integerPromotion(p.comp), paramToken);
                if (arg.ty.is(.Float)) try arg.floatCast(p, Type.Double, paramToken);
            }
            try callExpr.checkVarArg(p, firstAfter, paramToken, &arg, argCount);
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
        if (callExpr.shouldCoerceArg(argCount)) {
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

    const actual: u32 = @intCast(argCount);
    const extra = Diagnostics.Message.Extra{
        .arguments = .{
            .expected = @intCast(params.len),
            .actual = actual,
        },
    };

    if (callExpr.paramCountOverride()) |expected| {
        if (expected != actual)
            try p.errExtra(.expected_arguments, firstAfter, .{ .arguments = .{ .actual = actual, .expected = expected } });
    } else if (ty.is(.Func) and params.len != argCount) {
        try p.errExtra(.expected_arguments, firstAfter, extra);
    } else if (ty.is(.OldStyleFunc) and params.len != argCount) {
        if (params.len == 0)
            try p.errToken(.passing_args_to_kr, firstAfter)
        else
            try p.errExtra(.expected_arguments_old, firstAfter, extra);
    } else if (ty.is(.VarArgsFunc) and argCount < params.len)
        try p.errExtra(.expected_at_least_arguments, firstAfter, extra);

    return callExpr.finish(p, ty, listBufferTop, lParen);
}

fn checkArrayBounds(p: *Parser, index: Result, array: Result, token: TokenIndex) !void {
    if (index.value.isNone()) return;

    const arrayLen = array.ty.arrayLen() orelse return;
    if (arrayLen == 0) return;

    if (arrayLen == 1) {
        if (p.getNode(array.node, .memberAccessExpr) orelse p.getNode(array.node, .memberAccessPtrExpr)) |access| {
            var baseTy = access.base.type(&p.tree);

            if (baseTy.get(.Pointer)) |ptr|
                baseTy = ptr.data.subType.*;

            if (baseTy.is(.Struct)) {
                const record = baseTy.getRecord().?;
                if (access.memberIndex + 1 == record.fields.len) {
                    if (!index.value.isZero(p.comp)) {
                        try p.errStr(.old_style_flexible_struct, token, try index.str(p));
                    }
                    return;
                }
            }
        }
    }

    const indexInt = index.value.toInt(u64, p.comp) orelse std.math.maxInt(u64);
    if (index.ty.isUnsignedInt(p.comp)) {
        if (indexInt >= arrayLen) {
            try p.errStr(.array_after, token, try index.str(p));
        }
    } else {
        if (index.value.compare(.lt, Value.zero, p.comp)) {
            try p.errStr(.array_before, token, try index.str(p));
        } else if (indexInt >= arrayLen) {
            try p.errStr(.array_after, token, try index.str(p));
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
///  | generic-selectioncatch unreachable
fn parsePrimaryExpr(p: *Parser) Error!?Result {
    if (p.eat(.LParen)) |lp| {
        var groupedExpr = try p.expect(parseExpr);
        try p.expectClosing(lp, .RParen);
        try groupedExpr.un(p, .parenExpr, lp);
        return groupedExpr;
    }

    switch (p.currToken()) {
        .Identifier, .ExtendedIdentifier => {
            const nameToken = try p.expectIdentifier();
            const name = p.getTokenText(nameToken);
            const internedName = try p.comp.internString(name);

            if (p.symStack.findSymbol(internedName)) |sym| {
                try p.checkDeprecatedUnavailable(sym.type, nameToken, sym.token);
                if (sym.kind == .constexpr) {
                    return .{
                        .value = sym.value,
                        .ty = sym.type,
                        .node = try p.addNode(.{
                            .declRefExpr = .{
                                .nameToken = nameToken,
                                .type = sym.type,
                                .decl = sym.node.unpack().?,
                            },
                        }),
                    };
                }

                if (sym.value.is(.int, p.comp)) {
                    switch (p.constDeclFolding) {
                        .GNUFoldingExtension => try p.errToken(.const_decl_folded, nameToken),
                        .GNUVLAFoldingExtension => try p.errToken(.const_decl_folded_vla, nameToken),
                        else => {},
                    }
                }

                const dr: Node.DeclRef = .{
                    .nameToken = nameToken,
                    .type = sym.type,
                    .decl = sym.node.unpack().?,
                };
                const node = try p.addNode(if (sym.kind == .enumeration)
                    .{ .enumerationRef = dr }
                else
                    .{ .declRefExpr = dr });

                return Result{
                    .value = if (p.constDeclFolding == .NoConstDeclFolding and sym.kind != .enumeration) Value{} else sym.value,
                    .ty = sym.type,
                    .node = node,
                };
            }

            // check if this is a builtin call
            if (try p.comp.builtins.getOrCreate(p.comp, name, p.arena)) |some| {
                for (p.tokenIds[p.tokenIdx..]) |id| switch (id) {
                    .RParen => {}, // closing grouped expr
                    .LParen => break, // beginning of a call
                    else => {
                        try p.errToken(.builtin_must_be_called, nameToken);
                        return error.ParsingFailed;
                    },
                };

                if (some.builtin.properties.header != .none) {
                    try p.errStr(.implicit_builtin, nameToken, name);
                    try p.errExtra(.implicit_builtin_header_note, nameToken, .{
                        .builtinWithHeader = .{
                            .builtin = some.builtin.tag,
                            .header = some.builtin.properties.header,
                        },
                    });
                }

                return .{
                    .ty = some.ty,
                    .node = try p.addNode(.{
                        .builtinRef = .{
                            .nameToken = nameToken,
                            .type = some.ty,
                        },
                    }),
                };
            }

            // check for unknown builtin or implicit function declaration
            if (p.currToken() == .LParen and !p.comp.langOpts.standard.atLeast(.c23)) {
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
                    .fnProto = .{
                        .nameToken = nameToken,
                        .type = ty,
                        .static = false,
                        .@"inline" = false,
                        .definition = null,
                    },
                });

                try p.declBuffer.append(node);
                try p.symStack.declareSymbol(internedName, ty, nameToken, node);

                return .{
                    .ty = ty,
                    .node = try p.addNode(.{
                        .declRefExpr = .{
                            .nameToken = nameToken,
                            .type = ty,
                            .decl = node,
                        },
                    }),
                };
            }
            try p.errStr(.undeclared_identifier, nameToken, p.getTokenText(nameToken));
            return error.ParsingFailed;
        },

        .KeywordTrue, .KeywordFalse => |id| {
            const tokenIdx = p.tokenIdx;
            p.tokenIdx += 1;
            const res: Result = .{
                .value = Value.fromBool(id == .KeywordTrue),
                .ty = Type.Bool,
                .node = try p.addNode(.{
                    .boolLiteral = .{
                        .type = Type.Bool,
                        .literalToken = tokenIdx,
                    },
                }),
            };
            assert(!p.inMacro); // Should have been replaced with .one / .zero
            try res.putValue(p);
            return res;
        },

        .KeywordNullptr => {
            defer p.tokenIdx += 1;
            try p.errStr(.pre_c23_compat, p.tokenIdx, "'nullptr'");

            return .{
                .value = Value.null,
                .ty = Type.NullptrTy,
                .node = try p.addNode(.{
                    .nullptrLiteral = .{
                        .type = Type.NullptrTy,
                        .literalToken = p.tokenIdx,
                    },
                }),
            };
        },

        .MacroFunc, .MacroFunction => {
            defer p.tokenIdx += 1;
            var ty: Type = undefined;
            var tok = p.tokenIdx;

            if (p.func.ident) |some| {
                ty = some.ty;
                tok = some.node.get(&p.tree).variable.nameToken;
            } else if (p.func.type) |_| {
                const stringsTop = p.strings.items.len;
                defer p.strings.items.len = stringsTop;

                try p.strings.appendSlice(p.getTokenText(p.func.name));
                try p.strings.append(0);

                const predef = try p.makePredefinedIdentifier(stringsTop);
                ty = predef.ty;
                p.func.ident = predef;
            } else {
                const stringsTop = p.strings.items.len;
                defer p.strings.items.len = stringsTop;

                try p.strings.append(0);

                const predef = try p.makePredefinedIdentifier(stringsTop);
                ty = predef.ty;
                p.func.ident = predef;
                try p.declBuffer.append(predef.node);
            }

            if (p.func.type == null)
                try p.err(.predefined_top_level);

            return .{
                .ty = ty,
                .node = try p.addNode(.{
                    .declRefExpr = .{
                        .nameToken = tok,
                        .type = ty,
                        .decl = p.func.ident.?.node,
                    },
                }),
            };
        },

        .MacroPrettyFunc => {
            defer p.tokenIdx += 1;
            var ty: Type = undefined;
            if (p.func.prettyIdent) |some| {
                ty = some.ty;
            } else if (p.func.type) |funcType| {
                const stringsTop = p.strings.items.len;
                defer p.strings.items.len = stringsTop;

                try funcType.printNamed(p.getTokenText(p.func.name), p.comp, p.strings.writer());
                try p.strings.append(0);

                const predef = try p.makePredefinedIdentifier(stringsTop);
                ty = predef.ty;
                p.func.prettyIdent = predef;
            } else {
                const stringsTop = p.strings.items.len;
                defer p.strings.items.len = stringsTop;

                try p.strings.appendSlice("top level\x00");
                const predef = try p.makePredefinedIdentifier(stringsTop);
                ty = predef.ty;
                p.func.prettyIdent = predef;
                try p.declBuffer.append(predef.node);
            }

            if (p.func.type == null)
                try p.err(.predefined_top_level);

            return .{
                .ty = ty,
                .node = try p.addNode(.{
                    .declRefExpr = .{
                        .nameToken = p.tokenIdx,
                        .type = ty,
                        .decl = undefined, //TODO
                    },
                }),
            };
        },

        .StringLiteral,
        .StringLiteralUTF_8,
        .StringLiteralUTF_16,
        .StringLiteralUTF_32,
        .StringLiteralWide,
        .UnterminatedStringLiteral,
        => return try p.parseStringLiteral(),

        .CharLiteral,
        .CharLiteralUTF_8,
        .CharLiteralUTF_16,
        .CharLiteralUTF_32,
        .CharLiteralWide,
        .EmptyCharLiteral,
        .UnterminatedCharLiteral,
        => return try p.parseCharLiteral(),

        .Zero => {
            p.tokenIdx += 1;
            const intTy = if (p.inMacro) p.comp.types.intmax else Type.Int;
            const res: Result = .{
                .value = Value.zero,
                .ty = intTy,
                .node = try p.addNode(.{
                    .intLiteral = .{
                        .type = intTy,
                        .literalToken = p.tokenIdx,
                    },
                }),
            };
            try res.putValue(p);
            return res;
        },

        .One => {
            p.tokenIdx += 1;
            const intTy = if (p.inMacro) p.comp.types.intmax else Type.Int;
            const res: Result = .{
                .value = Value.one,
                .ty = intTy,
                .node = try p.addNode(.{
                    .intLiteral = .{
                        .literalToken = p.tokenIdx,
                        .type = intTy,
                    },
                }),
            };
            try res.putValue(p);
            return res;
        },

        .PPNumber => return try p.parsePPNumber(),

        .EmbedByte => {
            assert(!p.inMacro);
            const loc = p.pp.tokens.items(.loc)[p.tokenIdx];
            p.tokenIdx += 1;

            const buffer = p.comp.getSource(.generated).buffer[loc.byteOffset..];
            var byte: u8 = buffer[0] - '0';
            for (buffer[1..]) |c| {
                if (!std.ascii.isDigit(c)) break;
                byte *= 10;
                byte += c - '0';
            }

            const intTy = Type.Int;
            const res: Result = .{
                .value = try Value.int(byte, p.comp),
                .ty = intTy,
                .node = try p.addNode(.{ .intLiteral = .{ .type = intTy, .literalToken = p.tokenIdx } }),
            };
            try res.putValue(p);

            return res;
        },

        .KeywordGeneric => return p.parseGenericSelection(),

        else => return null,
    }
}

fn makePredefinedIdentifier(p: *Parser, stringsTop: usize) !Result {
    const end: u32 = @intCast(p.strings.items.len);
    const elemType: Type = .{ .specifier = .Char, .qual = .{ .@"const" = true } };

    const arrType = try p.arena.create(Type.Array);
    arrType.* = .{ .elem = elemType, .len = end - stringsTop };
    const ty: Type = .{ .specifier = .Array, .data = .{ .array = arrType } };

    const slice = p.strings.items[stringsTop..];
    const val = try Value.intern(p.comp, .{ .bytes = slice });

    const strLit = try p.addNode(.{ .stringLiteralExpr = .{ .type = ty, .literalToken = p.tokenIdx } });
    if (!p.inMacro) try p.tree.valueMap.put(p.gpa, strLit, val);

    return .{
        .ty = ty,
        .node = try p.addNode(.{
            .variable = .{
                .nameToken = p.tokenIdx,
                .type = ty,
                .storageClass = .static,
                .threadLocal = false,
                .implicit = true,
                .initializer = strLit,
            },
        }),
    };
}

fn parseFloat(p: *Parser, buf: []const u8, suffix: NumberSuffix, tokenIdx: TokenIndex) !Result {
    const ty = switch (suffix) {
        .None, .I => Type.Double,
        .F, .IF => Type.Float,
        .F16, .IF16 => Type.Float16,
        .L, .IL => Type.LongDouble,
        .W, .IW => p.comp.float80Type().?,
        .Q, .IQ, .F128, .IF128 => Type.Float128,
        else => unreachable,
    };

    const val = try Value.intern(p.comp, key: {
        try p.strings.ensureUnusedCapacity(buf.len);

        const stringsTop = p.strings.items.len;
        defer p.strings.items.len = stringsTop;
        for (buf) |c| {
            if (c != '_') p.strings.appendAssumeCapacity(c);
        }

        const float = std.fmt.parseFloat(f128, p.strings.items[stringsTop..]) catch unreachable;
        const bits = ty.bitSizeof(p.comp).?;
        break :key switch (bits) {
            16 => .{ .float = .{ .f16 = @floatCast(float) } },
            32 => .{ .float = .{ .f32 = @floatCast(float) } },
            64 => .{ .float = .{ .f64 = @floatCast(float) } },
            80 => .{ .float = .{ .f80 = @floatCast(float) } },
            128 => .{ .float = .{ .f128 = @floatCast(float) } },
            else => unreachable,
        };
    });

    var res = Result{
        .ty = ty,
        .node = try p.addNode(.{ .floatLiteral = .{ .type = ty, .literalToken = tokenIdx } }),
        .value = val,
    };
    if (suffix.isImaginary()) {
        try p.err(.gnu_imaginary_constant);
        res.ty = switch (suffix) {
            .I => Type.ComplexDouble,
            .IF16 => Type.ComplexFloat16,
            .IF => Type.ComplexFloat,
            .IL => Type.ComplexLongDouble,
            .IW => p.comp.float80Type().?.makeComplex(),
            .IQ, .IF128 => Type.ComplexFloat128,
            else => unreachable,
        };
        res.value = .{}; // TOOD: add complex values
        try res.un(p, .imaginaryLiteral, tokenIdx);
    }
    return res;
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

    var res: Result = .{ .value = try Value.int(value, p.comp), .node = undefined };
    if (overflow) {
        try p.errToken(.int_literal_too_big, tokenIdx);
        res.ty = Type.ULongLong;
        res.node = try p.addNode(.{ .intLiteral = .{ .type = res.ty, .literalToken = tokenIdx } });
        try res.putValue(p);
        return res;
    }

    if (suffix.isSignedInteger() and value > p.comp.types.intmax.maxInt(p.comp))
        try p.errToken(.implicitly_unsigned_literal, tokenIdx);

    const signedSpecs = .{ .Int, .Long, .LongLong };
    const unsignedSpecs = .{ .UInt, .ULong, .ULongLong };
    const signedOctHexSpecs = .{ .Int, .UInt, .Long, .ULong, .LongLong, .ULongLong };
    const specs: []const Type.Specifier = if (suffix.signedness() == .unsigned)
        &unsignedSpecs
    else if (base == 10)
        &signedSpecs
    else
        &signedOctHexSpecs;

    const suffixTy: Type = .{ .specifier = switch (suffix) {
        .None, .I => .Int,
        .U, .IU => .UInt,
        .UL, .IUL => .ULong,
        .ULL, .IULL => .ULongLong,
        .L, .IL => .Long,
        .LL, .ILL => .LongLong,
        else => unreachable,
    } };

    for (specs) |spec| {
        res.ty = Type{ .specifier = spec };
        if (res.ty.compareIntegerRanks(suffixTy, p.comp).compare(.lt)) continue;
        const maxInt = res.ty.maxInt(p.comp);
        if (value <= maxInt) break;
    } else {
        res.ty = Type.ULongLong;
    }

    res.node = try p.addNode(.{ .intLiteral = .{ .type = res.ty, .literalToken = tokenIdx } });
    try res.putValue(p);
    return res;
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
        res.value = .{};
        try res.un(p, .imaginaryLiteral, tokenIdx);
    }
    return res;
}

fn bitInt(p: *Parser, base: u8, buf: []const u8, suffix: NumberSuffix, tokenIdx: TokenIndex) Error!Result {
    try p.errStr(.pre_c23_compat, tokenIdx, "'_BitInt' suffix for literals");
    try p.errToken(.bitint_suffix, tokenIdx);

    var managed = try big.int.Managed.init(p.gpa);
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
        break :blk @intCast(bitsNeeded);
    };

    const intTy: Type = .{
        .specifier = .BitInt,
        .data = .{ .int = .{ .bits = bitsNeeded, .signedness = suffix.signedness() } },
    };

    const res: Result = .{
        .value = try Value.intern(p.comp, .{ .int = .{ .bigInt = c } }),
        .ty = intTy,
        .node = try p.addNode(.{ .intLiteral = .{ .type = intTy, .literalToken = tokenIdx } }),
    };
    try res.putValue(p);
    return res;
}

fn getFracPart(p: *Parser, buffer: []const u8, prefix: NumberPrefix, tokenIdx: TokenIndex) ![]const u8 {
    if (buffer.len == 0 or buffer[0] != '.') return "";
    assert(prefix != .octal);

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
        'e', 'E' => assert(prefix == .decimal),
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

    if (suffix.isFloat80() and p.comp.float80Type() == null) {
        try p.errStr(.invalid_float_suffix, tokenIdx, suffixStr);
        return error.ParsingFailed;
    }

    if (isFloat) {
        assert(prefix == .hex or prefix == .decimal);
        if (prefix == .hex and exponent.len == 0) {
            try p.errToken(.hex_floating_constant_requires_exponent, tokenIdx);
            return error.ParsingFailed;
        }
        const number = buffer[0 .. buffer.len - suffixStr.len];
        return p.parseFloat(number, suffix, tokenIdx);
    } else {
        return p.parseInt(prefix, intPart, suffix, tokenIdx);
    }
}

fn parsePPNumber(p: *Parser) Error!Result {
    defer p.tokenIdx += 1;
    var res = try p.parseNumberToken(p.tokenIdx);
    if (p.inMacro) {
        if (res.ty.isFloat() or !res.ty.isReal()) {
            try p.errToken(.float_literal_in_pp_expr, p.tokenIdx);
            return error.ParsingFailed;
        }
        res.ty = if (res.ty.isUnsignedInt(p.comp)) p.comp.types.intmax.makeIntegerUnsigned() else p.comp.types.intmax;
    } else if (!res.value.isNone()) {
        // TODO add complex values
        try res.putValue(p);
    }
    return res;
}

fn parseCharLiteral(p: *Parser) Error!?Result {
    defer p.tokenIdx += 1;

    const tokenId = p.currToken();
    const charKind = TextLiteral.Kind.classify(tokenId, .CharLiteral) orelse {
        if (tokenId == .EmptyCharLiteral)
            try p.err(.empty_char_literal_error)
        else if (tokenId == .UnterminatedCharLiteral)
            try p.err(.unterminated_char_literal_error)
        else
            unreachable;

        return .{
            .ty = Type.Int,
            .value = Value.zero,
            .node = try p.addNode(.{
                .charLiteral = .{
                    .type = Type.Int,
                    .literalToken = p.tokenIdx,
                },
            }),
        };
    };

    if (charKind == .utf8) try p.err(.u8_char_lit);
    var val: u32 = 0;
    const slice = charKind.contentSlice(p.getTokenText(p.tokenIdx));

    if (slice.len == 1 and std.ascii.isASCII(slice[0])) {
        val = slice[0];
    } else {
        const maxCodepoint = charKind.maxCodepoint(p.comp);
        var CharLiteralParser = TextLiteral.Parser.init(slice, charKind, maxCodepoint, p.comp);
        const maxCharsExpected = 4;

        var stackFallback = std.heap.stackFallback(maxCharsExpected * @sizeOf(u32), p.comp.gpa);
        var chars = std.ArrayList(u32).initCapacity(stackFallback.get(), maxCharsExpected) catch unreachable;
        defer chars.deinit();

        while (CharLiteralParser.next()) |item|
            switch (item) {
                .value => |v| try chars.append(v),
                .codepoint => |c| try chars.append(c),
                .improperlyEncoded => |s| {
                    try chars.ensureUnusedCapacity(s.len);
                    for (s) |c| chars.appendAssumeCapacity(c);
                },
                .utf8Text => |view| {
                    var it = view.iterator();
                    var maxcodepointSeen: u21 = 0;
                    while (it.nextCodepoint()) |c| {
                        maxcodepointSeen = @max(maxcodepointSeen, c);
                        try chars.append(c);
                    }
                    if (maxcodepointSeen > maxCodepoint)
                        CharLiteralParser.err(.char_too_large, .{ .none = {} });
                },
            };

        const isMultichar = chars.items.len > 1;
        if (isMultichar) {
            if (charKind == .char and chars.items.len == 4) {
                CharLiteralParser.warn(.four_char_char_literal, .{ .none = {} });
            } else if (charKind == .char) {
                CharLiteralParser.warn(.multichar_literal_warning, .{ .none = {} });
            } else {
                const kind = switch (charKind) {
                    .wide => "wide",
                    .utf8, .utf16, .utf32 => "Unicode",
                    else => unreachable,
                };
                CharLiteralParser.err(.invalid_multichar_literal, .{ .str = kind });
            }
        }

        var multicharOverflow = false;
        if (charKind == .char and isMultichar) {
            for (chars.items) |item| {
                val, const overflowed = @shlWithOverflow(val, 8);
                multicharOverflow = multicharOverflow or overflowed != 0;
                val += @as(u8, @truncate(item));
            }
        } else if (chars.items.len > 0) {
            val = chars.items[chars.items.len - 1];
        }

        if (multicharOverflow)
            CharLiteralParser.err(.char_lit_too_wide, .{ .none = {} });

        for (CharLiteralParser.errors()) |item|
            try p.errExtra(item.tag, p.tokenIdx, item.extra);
    }

    const ty = charKind.charLiteralType(p.comp);
    // This is the type the literal will have if we're in a macro; macros always operate on intmax_t/uintmax_t values
    const macroTy = if (ty.isUnsignedInt(p.comp) or (charKind == .char and p.comp.getCharSignedness() == .unsigned))
        p.comp.types.intmax.makeIntegerUnsigned()
    else
        p.comp.types.intmax;

    const res = Result{
        .ty = if (p.inMacro) macroTy else ty,
        .value = try Value.int(val, p.comp),
        .node = try p.addNode(.{
            .charLiteral = .{
                .type = ty,
                .literalToken = p.tokenIdx,
            },
        }),
    };

    if (!p.inMacro) try p.tree.valueMap.put(p.gpa, res.node, res.value);
    return res;
}

fn parseStringLiteral(p: *Parser) Error!Result {
    const stringStart = p.tokenIdx;
    var stringEnd = p.tokenIdx;
    var stringKind: TextLiteral.Kind = .char;
    while (TextLiteral.Kind.classify(p.tokenIds[stringEnd], .StringLiteral)) |next| : (stringEnd += 1) {
        stringKind = stringKind.concat(next) catch {
            try p.errToken(.unsupported_str_cat, stringEnd);
            while (p.currToken().isStringLiteral()) : (p.tokenIdx += 1) {}
            return error.ParsingFailed;
        };

        if (stringKind == .unterminated) {
            try p.errToken(.unterminated_string_literal_error, stringEnd);
            p.tokenIdx = stringEnd + 1;
            return error.ParsingFailed;
        }
    }

    assert(stringEnd > p.tokenIdx);

    const charWidth = stringKind.charUnitSize(p.comp);

    const stringsTop = p.strings.items.len;
    defer p.strings.items.len = stringsTop;

    while (p.tokenIdx < stringEnd) : (p.tokenIdx += 1) {
        const thisKind = TextLiteral.Kind.classify(p.currToken(), .StringLiteral).?;
        const slice = thisKind.contentSlice(p.getTokenText(p.tokenIdx));
        var charLiteralParser = TextLiteral.Parser.init(slice, thisKind, 0x10ffff, p.comp);

        try p.strings.ensureUnusedCapacity((slice.len + 1) * @intFromEnum(charWidth));
        while (charLiteralParser.next()) |item| switch (item) {
            .value => |v| {
                switch (charWidth) {
                    .@"1" => p.strings.appendAssumeCapacity(@intCast(v)),
                    .@"2" => {
                        const word: u16 = @intCast(v);
                        p.strings.appendSliceAssumeCapacity(std.mem.asBytes(&word));
                    },
                    .@"4" => p.strings.appendSliceAssumeCapacity(std.mem.asBytes(&v)),
                }
            },
            .codepoint => |c| {
                switch (charWidth) {
                    .@"1" => {
                        var buf: [4]u8 = undefined;
                        const written = std.unicode.utf8Encode(c, &buf) catch unreachable;
                        const encoded = buf[0..written];
                        p.strings.appendSliceAssumeCapacity(encoded);
                    },
                    .@"2" => {
                        var utf16Buffer: [2]u16 = undefined;
                        var utf8Buffer: [4]u8 = undefined;
                        const utf8written = std.unicode.utf8Encode(c, &utf8Buffer) catch unreachable;
                        const utf16Written = std.unicode.utf8ToUtf16Le(&utf16Buffer, utf8Buffer[0..utf8written]) catch unreachable;
                        const bytes = std.mem.sliceAsBytes(utf16Buffer[0..utf16Written]);
                        p.strings.appendSliceAssumeCapacity(bytes);
                    },
                    .@"4" => {
                        const val: u32 = c;
                        p.strings.appendSliceAssumeCapacity(std.mem.asBytes(&val));
                    },
                }
            },
            .improperlyEncoded => |bytes| p.strings.appendSliceAssumeCapacity(bytes),
            .utf8Text => |view| {
                switch (charWidth) {
                    .@"1" => p.strings.appendSliceAssumeCapacity(view.bytes),
                    .@"2" => {
                        var capacitySlice: []align(@alignOf(u16)) u8 = @alignCast(p.strings.unusedCapacitySlice());
                        const destLen = std.mem.alignBackward(usize, capacitySlice.len, 2);
                        const dest = std.mem.bytesAsSlice(u16, capacitySlice[0..destLen]);
                        const wordsWritten = std.unicode.utf8ToUtf16Le(dest, view.bytes) catch unreachable;
                        p.strings.resize(p.strings.items.len + wordsWritten * 2) catch unreachable;
                    },
                    .@"4" => {
                        var it = view.iterator();
                        while (it.nextCodepoint()) |codepoint| {
                            const val: u32 = codepoint;
                            p.strings.appendSliceAssumeCapacity(std.mem.asBytes(&val));
                        }
                    },
                }
            },
        };

        for (charLiteralParser.errors()) |item|
            try p.errExtra(item.tag, p.tokenIdx, item.extra);
    }

    p.strings.appendNTimesAssumeCapacity(0, @intFromEnum(charWidth));
    const slice = p.strings.items[stringsTop..];

    const internedAlign = std.mem.alignForward(
        usize,
        p.comp.interner.strings.items.len,
        stringKind.internalStorageAlignment(p.comp),
    );
    try p.comp.interner.strings.resize(p.gpa, internedAlign);

    const val = try Value.intern(p.comp, .{ .bytes = slice });
    const arrayType: Type = ty: {
        const arrayData = try p.arena.create(Type.Array);
        arrayData.* = .{ .elem = stringKind.elementType(p.comp), .len = @divExact(slice.len, @intFromEnum(charWidth)) };
        break :ty .{ .specifier = .Array, .data = .{ .array = arrayData } };
    };

    const res: Result = .{
        .ty = arrayType,
        .value = val,
        .node = try p.addNode(.{
            .stringLiteralExpr = .{
                .literalToken = stringStart,
                .type = arrayType,
            },
        }),
    };

    try res.putValue(p);
    return res;
}

/// Run a parser function but do not evaluate the result
fn parseNoEval(p: *Parser, comptime func: fn (*Parser) Error!?Result) Error!Result {
    const noEval = p.noEval;
    defer p.noEval = noEval;

    p.noEval = true;

    const parsed = try func(p);
    return p.expectResult(parsed);
}

//// genericSelection :
//// `_Generic` '(' assign-expression ',' generic-association (',' generic-association)* ')'
//// generic-association
////  : type-name ':' assign-expression
////  | `default` ':' assign-expression
fn parseGenericSelection(p: *Parser) Error!?Result {
    const kwGeneric = p.tokenIdx;
    p.tokenIdx += 1;
    const lp = try p.expectToken(.LParen);
    const controllingToken = p.tokenIdx;
    const controlling = try p.parseNoEval(parseAssignExpr);

    var controllingTy = controlling.ty;
    if (controllingTy.isArray()) controllingTy.decayArray();

    _ = try p.expectToken(.Comma);

    const listBufferTop = p.listBuffer.items.len;
    defer p.listBuffer.items.len = listBufferTop;

    const paramBufferTop = p.paramBuffer.items.len;
    defer p.paramBuffer.items.len = paramBufferTop;

    var defaultToken: ?TokenIndex = null;
    var default: Result = undefined;
    var chosenToken: ?TokenIndex = null;
    var chosen: Result = undefined;

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

            const colon = try p.expectToken(.Colon);
            var res = try p.expect(parseAssignExpr);
            res.node = try p.addNode(.{
                .genericAssociationExpr = .{
                    .colonToken = colon,
                    .associationType = ty,
                    .expr = res.node,
                },
            });

            try p.listBuffer.append(res.node);
            try p.paramBuffer.append(.{ .name = undefined, .ty = ty, .nameToken = start, .node = undefined });

            if (ty.eql(controllingTy, p.comp, false)) {
                if (chosenToken == null) {
                    chosen = res;
                    chosenToken = start;
                    break :blk;
                }
            }

            const previousItems = p.paramBuffer.items[0 .. p.paramBuffer.items.len - 1][paramBufferTop..];
            for (previousItems) |prevItem| {
                if (prevItem.ty.eql(ty, p.comp, true)) {
                    try p.errStr(.generic_duplicate, start, try p.typeStr(ty));
                    try p.errStr(.generic_duplicate_here, prevItem.nameToken, try p.typeStr(ty));
                }
            }
        } else if (p.eat(.KeywordDefault)) |tok| {
            _ = try p.expectToken(.Colon);
            var res = try p.expect(parseAssignExpr);
            res.node = try p.addNode(.{
                .genericDefaultExpr = .{
                    .defaultToken = tok,
                    .expr = res.node,
                },
            });

            if (defaultToken) |prev| {
                try p.errToken(.generic_duplicate_default, tok);
                try p.errToken(.previous_case, prev);
            }

            default = res;
            defaultToken = tok;
        } else {
            if (p.listBuffer.items.len == listBufferTop) {
                try p.err(.expected_type);
                return error.ParsingFailed;
            }
            break;
        }

        if (p.eat(.Comma) == null)
            break;
    }

    try p.expectClosing(lp, .RParen);
    if (chosenToken == null) {
        if (defaultToken != null) {
            chosen = default;
        } else {
            try p.errStr(.generic_no_match, controllingToken, try p.typeStr(controllingTy));
            return error.ParsingFailed;
        }
    } else if (defaultToken != null) {
        try p.listBuffer.append(default.node);
    }

    for (p.listBuffer.items[listBufferTop..], listBufferTop..) |item, i| {
        if (item == chosen.node) {
            _ = p.listBuffer.orderedRemove(i);
            break;
        }
    }

    return .{
        .ty = chosen.ty,
        .value = chosen.value,
        .node = try p.addNode(.{
            .genericExpr = .{
                .genericToken = kwGeneric,
                .controlling = controlling.node,
                .chosen = chosen.node,
                .type = chosen.ty,
                .rest = p.listBuffer.items[listBufferTop..],
            },
        }),
    };
}
