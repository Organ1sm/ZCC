const std = @import("std");
const assert = std.debug.assert;
const big = std.math.big;
const Allocator = std.mem.Allocator;
const NodeList = std.ArrayList(Node.Index);

const Compilation = @import("../Basic/Compilation.zig");
const Source = @import("../Basic/Source.zig");
const TokenType = @import("../Basic/TokenType.zig").TokenType;
const Lexer = @import("../Lexer/Lexer.zig");
const Preprocessor = @import("../Lexer/Preprocessor.zig");
const AST = @import("../AST/AST.zig");
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
const evalBuiltin = @import("../Builtins/eval.zig").eval;

const Token = AST.Token;
const NumberPrefix = Token.NumberPrefix;
const NumberSuffix = Token.NumberSuffix;
const TokenIndex = AST.TokenIndex;
const Node = AST.Node;

const TypeStore = @import("../AST/TypeStore.zig");
const Type = TypeStore.Type;
const QualType = TypeStore.QualType;
const TypeBuilder = TypeStore.Builder;

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

// buffers used during compilation
symStack: SymbolStack = .{},
strings: std.ArrayList(u8),
labels: std.ArrayList(Label),
listBuffer: NodeList,
declBuffer: NodeList,
/// Function type parameters, also used for generic selection association
/// duplicate checking.
paramBuffer: std.ArrayList(Type.Func.Param),
/// Enum type fields.
enumBuffer: std.ArrayList(Type.Enum.Field),
/// Record type fields.
recordBuffer: std.ArrayList(Type.Record.Field),
/// Attributes that have been parsed but not yet validated or applied.
attrBuffer: std.MultiArrayList(TentativeAttribute) = .{},
/// Used to store validated attributes before they are applied to types.
attrApplicationBuffer: std.ArrayListUnmanaged(Attribute) = .{},
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

initContext: InitContext = .runtime,

/// Various variables that are different for each function.
func: struct {
    /// null if not in function, will always be plain func
    qt: ?QualType = null,
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

    fn addFieldsFromAnonymous(r: @This(), p: *Parser, recordTy: Type.Record) Error!void {
        for (recordTy.fields) |f| {
            if (f.name == .empty) {
                if (f.qt.getRecord(p.comp)) |rec| {
                    try r.addFieldsFromAnonymous(p, rec);
                }
            } else {
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

pub const InitContext = enum {
    /// inits do not need to be compile-time constants
    runtime,
    /// constexpr variable, could be any scope but inits must be compile-time constants
    constexpr,
    /// static and global variables, inits must be compile-time constants
    static,
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

pub fn typeStr(p: *Parser, qt: QualType) ![]const u8 {
    if (@import("builtin").mode != .Debug) {
        if (qt.isInvalid()) {
            return "Tried to render invalid type - this is an zinc bug.";
        }
    }

    if (TypeBuilder.fromType(p.comp, qt).toString(p.comp.langOpts)) |str| return str;
    const stringsTop = p.strings.items.len;
    defer p.strings.items.len = stringsTop;

    try qt.print(p.comp, p.strings.writer());
    return try p.comp.diagnostics.arena.allocator().dupe(u8, p.strings.items[stringsTop..]);
}

pub fn typePairStr(p: *Parser, a: QualType, b: QualType) ![]const u8 {
    return p.typePairStrExtra(a, " and ", b);
}

pub fn typePairStrExtra(p: *Parser, a: QualType, msg: []const u8, b: QualType) ![]const u8 {
    if (@import("builtin").mode != .Debug) {
        if (a.isInvalid() or b.isInvalid()) {
            return "Tried to render invalid type - this is an zinc bug.";
        }
    }

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

pub fn floatValueChangedStr(p: *Parser, res: *Result, oldValue: Value, intTy: QualType) ![]const u8 {
    const stringsTop = p.strings.items.len;
    defer p.strings.items.len = stringsTop;

    var w = p.strings.writer();
    const str = try p.typePairStrExtra(res.qt, " to ", intTy);
    try w.writeAll(str);

    try w.writeAll(" changes ");
    if (res.value.isZero(p.comp)) try w.writeAll("non-zero ");
    try w.writeAll("value from ");
    try oldValue.print(res.qt, p.comp, w);
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
fn checkDeprecatedUnavailable(p: *Parser, ty: QualType, usageToken: TokenIndex, declToken: TokenIndex) !void {
    if (ty.getAttribute(p.comp, .@"error")) |@"error"| {
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

    if (ty.getAttribute(p.comp, .warning)) |warning| {
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
    if (ty.getAttribute(p.comp, .unavailable)) |unavailable| {
        try p.errDeprecated(.unavailable, usageToken, unavailable.msg);
        try p.errStr(.unavailable_note, unavailable.__name_token, p.getTokenText(declToken));
        return error.ParsingFailed; // Abort parsing due to 'unavailable' type
    }

    // Check if the type has a 'deprecated' attribute and report it
    else if (ty.getAttribute(p.comp, .deprecated)) |deprecated| {
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

pub fn getDecayedStringLiteral(p: *Parser, node: Node.Index) ?Value {
    const cast = p.getNode(node, .cast) orelse return null;
    if (cast.kind != .ArrayToPointer) return null;

    var cur = cast.operand;
    while (true) {
        switch (cur.get(&p.tree)) {
            .parenExpr => |un| cur = un.operand,
            .stringLiteralExpr => return p.tree.valueMap.get(cur),
            else => return null,
        }
    }
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

    for (p.declBuffer.items) |declIndex| {
        const node = declIndex.get(&p.tree);
        const forward = switch (node) {
            .structForwardDecl, .unionForwardDecl, .enumForwardDecl => |forward| forward,
            else => continue,
        };

        const declTypeName = switch (forward.containerQt.base(p.comp).type) {
            .@"struct", .@"union" => |recordTy| recordTy.name,
            .@"enum" => |enumTy| enumTy.name,
            else => unreachable,
        };

        const tentativeDefToken = p.tentativeDefs.get(declTypeName) orelse continue;
        const tyStr = try p.typeStr(forward.containerQt);
        try p.errStr(.tentative_definition_incomplete, tentativeDefToken, tyStr);
        try p.errStr(.forward_declaration_here, forward.nameOrKindToken, tyStr);
    }
}

fn addImplicitTypedef(p: *Parser, name: []const u8, qt: QualType) !void {
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
            .qt = qt,
            .implicit = true,
        },
    });

    const internedName = try p.comp.internString(name);
    const typedefQt = (try p.comp.typeStore.put(p.gpa, .{ .typedef = .{
        .base = qt,
        .name = internedName,
    } })).withQualifiers(qt);
    try p.symStack.defineTypedef(internedName, typedefQt, nameToken, node);
    try p.declBuffer.append(node);
}

/// root : (decl | inline assembly ';' | static-assert-declaration)*
pub fn parse(pp: *Preprocessor) Compilation.Error!AST {
    assert(pp.linemarkers == .None);
    pp.comp.pragmaEvent(.BeforeParse);

    var p = Parser{
        .pp = pp,
        .comp = pp.comp,
        .gpa = pp.comp.gpa,
        .tokenIds = pp.tokens.items(.id),

        .tree = .{
            .comp = pp.comp,
            .tokens = pp.tokens.slice(),
        },

        .labels = .init(pp.comp.gpa),
        .strings = .init(pp.comp.gpa),
        .listBuffer = .init(pp.comp.gpa),
        .declBuffer = .init(pp.comp.gpa),
        .paramBuffer = .init(pp.comp.gpa),
        .enumBuffer = .init(pp.comp.gpa),
        .recordBuffer = .init(pp.comp.gpa),
        .stringsIds = .{
            .declSpecId = try pp.comp.internString("__declspec"),
            .mainId = try pp.comp.internString("main"),
            .file = try pp.comp.internString("FILE"),
            .jmpBuf = try pp.comp.internString("jmp_buf"),
            .sigJmpBuf = try pp.comp.internString("sigjmp_buf"),
            .ucontextTy = try pp.comp.internString("ucontext_t"),
        },
    };

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
    }

    //bind p to the symbol stack for simplify symbol stack api
    p.symStack.p = &p;
    try p.symStack.pushScope();
    defer p.symStack.popScope();

    {
        if (p.comp.langOpts.hasChar8_t())
            try p.addImplicitTypedef("char8_t", .uchar);

        try p.addImplicitTypedef("__int128_t", .int128);
        try p.addImplicitTypedef("__uint128_t", .uint128);
        try p.addImplicitTypedef("__builtin_ms_va_list", .charPointer);

        const vaListQt = pp.comp.typeStore.vaList;
        try p.addImplicitTypedef("__builtin_va_list", vaListQt);

        pp.comp.typeStore.vaList = try vaListQt.decay(pp.comp);

        try p.addImplicitTypedef("__NSConstantString", pp.comp.typeStore.nsConstantString);

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
fn typedefDefined(p: *Parser, name: StringId, ty: QualType) void {
    if (name == p.stringsIds.file) {
        p.comp.typeStore.file = ty;
    } else if (name == p.stringsIds.jmpBuf) {
        p.comp.typeStore.jmpBuf = ty;
    } else if (name == p.stringsIds.sigJmpBuf) {
        p.comp.typeStore.sigJmpBuf = ty;
    } else if (name == p.stringsIds.ucontextTy) {
        p.comp.typeStore.ucontextTy = ty;
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

    var declSpec = (try p.parseDeclSpec()) orelse blk: {
        if (p.func.qt != null) {
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

        var builder: TypeBuilder = .{ .parser = p };
        break :blk DeclSpec{ .qt = try builder.finish() };
    };

    if (declSpec.noreturn) |token| {
        const attr = Attribute{ .tag = .noreturn, .args = .{ .noreturn = .{} }, .syntax = .keyword };
        try p.attrBuffer.append(p.gpa, .{ .attr = attr, .tok = token });
    }

    var declNode = try p.tree.addNode(.{ .emptyDecl = .{ .semicolon = firstToken } });
    var initDeclarator = (try p.parseInitDeclarator(&declSpec, attrBufferTop, declNode)) orelse {
        _ = try p.expectToken(.Semicolon); // eat ';'

        missingDecl: {
            if (declSpec.qt.type(p.comp) == .typeof) {
                break :missingDecl; // we follow GCC and clang's behavior here
            }
            switch (declSpec.qt.base(p.comp).type) {
                .@"enum" => break :missingDecl,
                .@"struct", .@"union" => |recordTy| if (!recordTy.isAnonymous(p.comp)) break :missingDecl,
                else => {},
            }

            try p.errToken(.missing_declaration, firstToken);
            return true;
        }

        const attrs = p.attrBuffer.items(.attr)[attrBufferTop..];
        const toks = p.attrBuffer.items(.tok)[attrBufferTop..];
        for (attrs, toks) |attr, tok| {
            try p.errExtra(
                .ignored_record_attr,
                tok,
                .{
                    .ignoredRecordAttr = .{
                        .tag = attr.tag,
                        .tagKind = switch (declSpec.qt.base(p.comp).type) {
                            .@"enum" => .@"enum",
                            .@"struct" => .@"struct",
                            .@"union" => .@"union",
                            else => unreachable,
                        },
                    },
                },
            );
        }

        try p.errToken(.missing_declaration, firstToken);
        return true;
    };

    // check for funtion definition
    if (initDeclarator.d.declaratorType == .func and initDeclarator.initializer == null) fndef: {
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

        if (p.func.qt != null) try p.err(.func_not_in_root);

        const declaratorName = initDeclarator.d.name;
        const internedDeclaratorName = try p.getInternString(declaratorName);

        const func = p.func;
        p.func = .{ .qt = initDeclarator.d.qt, .name = declaratorName };
        defer p.func = func;

        if (internedDeclaratorName == p.stringsIds.mainId) {
            const funcTy = initDeclarator.d.qt.get(p.comp, .func).?;
            if (funcTy.returnType.get(p.comp, .int)) |intTy| {
                if (intTy == .Int) try p.errToken(.main_return_type, declaratorName);
            }
        }

        try p.symStack.pushScope();
        defer p.symStack.popScope();

        // collect old style parameters
        if (initDeclarator.d.oldTypeFunc != null) {
            const paramBufferTop = p.paramBuffer.items.len;
            defer p.paramBuffer.items.len = paramBufferTop;

            // We cannot refer to the function type here because the pointer to
            // type_store.extra might get invalidated while parsing the param decls.
            const funcQt = initDeclarator.d.qt.base(p.comp).qt;
            const paramsLen = funcQt.get(p.comp, .func).?.params.len;

            const newParams = try p.paramBuffer.addManyAsSlice(paramsLen);
            for (newParams) |*newParam| {
                newParam.name = .empty;
            }

            paramLoop: while (true) {
                const paramDeclSpec = (try p.parseDeclSpec()) orelse break;
                if (p.eat(.Semicolon)) |semi| {
                    try p.errToken(.missing_declaration, semi);
                    continue :paramLoop;
                }

                while (true) {
                    const attrBufferTopDeclarator = p.attrBuffer.len;
                    defer p.attrBuffer.len = attrBufferTopDeclarator;

                    var paramD = (try p.declarator(paramDeclSpec.qt, .param)) orelse {
                        try p.errToken(.missing_declaration, firstToken);
                        _ = try p.expectToken(.Semicolon);
                        continue :paramLoop;
                    };

                    try p.parseAttrSpec();

                    if (paramD.qt.sizeofOrNull(p.comp) == null) {
                        if (paramD.qt.is(p.comp, .void)) {
                            try p.errToken(.invalid_void_param, paramD.name);
                        } else {
                            try p.errStr(.parameter_incomplete_ty, paramD.name, try p.typeStr(paramD.qt));
                        }
                    } else {
                        // Decay params declared as functions or arrays to pointer.
                        paramD.qt = try paramD.qt.decay(p.comp);
                    }

                    const attributedQt = try Attribute.applyParameterAttributes(p, paramD.qt, attrBufferTopDeclarator, .alignas_on_param);

                    try paramDeclSpec.validateParam(p);
                    const paramNode = try p.addNode(.{
                        .param = .{
                            .nameToken = paramD.name,
                            .qt = attributedQt,
                            .storageClass = switch (paramDeclSpec.storageClass) {
                                .none => .auto,
                                .register => .register,
                                else => .auto, // Error reported in `validateParam`
                            },
                        },
                    });

                    const nameStr = p.getTokenText(paramD.name);
                    const internedName = try p.comp.internString(nameStr);
                    try p.symStack.defineParam(internedName, attributedQt, paramD.name, paramNode);

                    // find and correct parameter types
                    for (funcQt.get(p.comp, .func).?.params, newParams) |param, *newParam| {
                        if (param.name == internedName) {
                            newParam.* = .{
                                .qt = attributedQt,
                                .name = param.name,
                                .node = .pack(paramNode),
                                .nameToken = param.nameToken,
                            };
                            break;
                        }
                    } else {
                        try p.errStr(.parameter_missing, paramD.name, nameStr);
                    }

                    if (p.eat(.Comma) == null) break;
                }

                _ = try p.expectToken(.Semicolon);
            }

            const funcTy = funcQt.get(p.comp, .func).?;
            for (funcTy.params, newParams) |param, *newParam| {
                if (newParam.name == .empty) {
                    try p.errStr(.param_not_declared, param.nameToken, param.name.lookup(p.comp));
                    newParam.* = .{
                        .name = param.name,
                        .nameToken = param.nameToken,
                        .node = param.node,
                        .qt = .int,
                    };
                }
            }
            p.func.qt = try p.comp.typeStore.put(p.gpa, .{ .func = .{
                .kind = .Normal,
                .params = newParams,
                .returnType = funcTy.returnType,
            } });
        } else if (initDeclarator.d.qt.get(p.comp, .func)) |funcTy| {
            for (funcTy.params) |param| {
                if (param.name == .empty) {
                    try p.errToken(.omitting_parameter_name, param.nameToken);
                    continue;
                }

                // bypass redefinition check to avoid duplicate errors
                try p.symStack.define(.{
                    .kind = .definition,
                    .name = param.name,
                    .token = param.nameToken,
                    .qt = param.qt,
                    .value = .{},
                    .node = param.node,
                });

                if (param.qt.isInvalid()) continue;

                if (param.qt.get(p.comp, .pointer)) |pointerTy| {
                    if (pointerTy.decayed) |decayedQt| {
                        if (decayedQt.get(p.comp, .array)) |arrayTy| {
                            if (arrayTy.len == .unspecifiedVariable) {
                                try p.errToken(.unbound_vla, param.nameToken);
                            }
                        }
                    }
                }

                if (param.qt.sizeofOrNull(p.comp) == null and !param.qt.is(p.comp, .void))
                    try p.errStr(.parameter_incomplete_ty, param.nameToken, try p.typeStr(param.qt));
            }
        }

        try p.symStack.defineSymbol(internedDeclaratorName, p.func.qt.?, declaratorName, declNode, .{}, false);

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
                .qt = p.func.qt.?,
                .body = body,
            },
        });
        try p.declBuffer.append(declNode);

        // check gotos
        if (func.qt == null) {
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

        try declSpec.validate(p, initDeclarator.d.qt);

        if (declSpec.storageClass == .typedef) {
            try p.tree.setNode(@intFromEnum(declNode), .{
                .typedef = .{
                    .nameToken = initDeclarator.d.name,
                    .qt = initDeclarator.d.qt,
                    .implicit = false,
                },
            });
        } else if (initDeclarator.d.declaratorType == .func or initDeclarator.d.qt.is(p.comp, .func)) {
            try p.tree.setNode(@intFromEnum(declNode), .{
                .fnProto = .{
                    .nameToken = initDeclarator.d.name,
                    .qt = initDeclarator.d.qt,
                    .static = declSpec.storageClass == .static,
                    .@"inline" = declSpec.@"inline" != null,
                    .definition = null,
                },
            });
        } else {
            var nodeQt = initDeclarator.d.qt;
            if (p.func.qt == null) {
                if (nodeQt.get(p.comp, .array)) |arrayTy| {
                    if (arrayTy.len == .incomplete) {
                        // Create tentative array node with fixed type.
                        nodeQt = try p.comp.typeStore.put(p.gpa, .{ .array = .{
                            .elem = arrayTy.elem,
                            .len = .{ .fixed = 1 },
                        } });
                    }
                }
            }
            try p.tree.setNode(@intFromEnum(declNode), .{
                .variable = .{
                    .nameToken = initDeclarator.d.name,
                    .qt = nodeQt,
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
        }
        try p.declBuffer.append(declNode);

        const internedName = try p.getInternString(initDeclarator.d.name);
        if (declSpec.storageClass == .typedef) {
            const typedefQt = (try p.comp.typeStore.put(p.gpa, .{ .typedef = .{
                .base = initDeclarator.d.qt,
                .name = internedName,
            } })).withQualifiers(initDeclarator.d.qt);

            try p.symStack.defineTypedef(internedName, typedefQt, initDeclarator.d.name, declNode);
            p.typedefDefined(internedName, typedefQt);
        } else if (initDeclarator.initializer) |init| {
            // TODO validate global variable/constexpr initializer comptime known
            try p.symStack.defineSymbol(
                internedName,
                initDeclarator.d.qt,
                initDeclarator.d.name,
                declNode,
                if (initDeclarator.d.qt.@"const" or declSpec.constexpr != null) init.value else .{},
                declSpec.constexpr != null,
            );
        } else if (p.func.qt != null and declSpec.storageClass != .@"extern") {
            try p.symStack.defineSymbol(internedName, initDeclarator.d.qt, initDeclarator.d.name, declNode, .{}, false);
        } else {
            try p.symStack.declareSymbol(internedName, initDeclarator.d.qt, initDeclarator.d.name, declNode);
        }

        if (p.eat(.Comma) == null)
            break;

        if (!warnedAuto) {
            if (declSpec.autoType) |tokIdx| {
                try p.errToken(.auto_type_requires_single_declarator, tokIdx);
                warnedAuto = true;
            }
            if (declSpec.c23Auto) |tokIdx| {
                try p.errToken(.c23_auto_single_declarator, tokIdx);
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
        try Value.printString(bytes, message.qt, p.comp, buf.writer());
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

    try res.castToBool(p, .bool, resToken);
    if (res.value.isNone()) {
        if (!res.qt.isInvalid())
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
fn typeof(p: *Parser) Error!?QualType {
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
    if (try p.parseTypeName()) |qt| {
        try p.expectClosing(lp, .RParen);
        if (qt.isInvalid()) return null;

        const typeofQt = try p.comp.typeStore.put(p.gpa, .{ .typeof = .{
            .base = qt,
            .expr = null,
        } });
        return typeofQt.withQualifiers(qt);
    }

    const typeofExpr = try p.parseNoEval(parseExpr);
    try p.expectClosing(lp, .RParen);

    if (typeofExpr.qt.isInvalid()) return null;

    const typeofQt = try p.comp.typeStore.put(p.gpa, .{ .typeof = .{
        .base = typeofExpr.qt,
        .expr = typeofExpr.node,
    } });
    if (unqual) return typeofQt;
    return typeofQt.withQualifiers(typeofExpr.qt);
}

/// declaration-specifier
///  : storage-class-specifier
///  | type-specifier-qualifier
///  | function-specifier
///  | auto-specifier
///
/// declaration-specifier
///  : storage-class-specifier
///  | type-specifier
///  | type-qualifier
///  | func-specifier
///  | align-specifier
///
/// func-specifier : keyword-inline | keyword-noreturn
/// auto-specifier  : keyword-auto-type
///
fn parseDeclSpec(p: *Parser) Error!?DeclSpec {
    var d: DeclSpec = .{ .qt = .invalid };
    var builder: TypeBuilder = .{ .parser = p };

    const start = p.tokenIdx;
    while (true) {
        const id = p.currToken();
        switch (id) {
            .KeywordInline, .KeywordGccInline1, .KeywordGccInline2 => {
                if (d.@"inline" != null)
                    try p.errStr(.duplicate_declspec, p.tokenIdx, "inline");
                d.@"inline" = p.tokenIdx;
                p.tokenIdx += 1;
                continue;
            },

            .KeywordNoreturn => {
                if (d.noreturn != null)
                    try p.errStr(.duplicate_declspec, p.tokenIdx, "_Noreturn");
                d.noreturn = p.tokenIdx;
                p.tokenIdx += 1;
                continue;
            },

            .KeywordAutoType => {
                try p.errToken(.auto_type_extension, p.tokenIdx);
                try builder.combine(.AutoType, p.tokenIdx);
                if (builder.type == .AutoType) d.autoType = p.tokenIdx;
                p.tokenIdx += 1;
                continue;
            },

            .KeywordAuto => if (p.comp.langOpts.standard.atLeast(.c23)) {
                try builder.combine(.C23Auto, p.tokenIdx);
                if (builder.type == .C23Auto) d.c23Auto = p.tokenIdx;
                p.tokenIdx += 1;
                continue;
            },

            else => {},
        }

        if (try p.parseStorageClassSpec(&d)) continue;
        if (try p.parseTypeSpec(&builder)) continue;
        if (p.tokenIdx == start) return null;

        d.qt = try builder.finish();
        return d;
    }
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

    var ID = InitDeclarator{ .d = (try p.declarator(declSpec.qt, .normal)) orelse return null };

    try p.attributeSpecifier(ID.d.name);
    _ = try p.parseAssembly(.declLabel);
    try p.attributeSpecifier(ID.d.name);

    var applyVarAttributes = false;
    if (declSpec.storageClass == .typedef) {
        if (declSpec.autoType) |tokenIdx| {
            try p.errStr(.auto_type_not_allowed, tokenIdx, "typedef");
            ID.d.qt = .invalid;
        } else if (declSpec.c23Auto) |tokenIdx| {
            try p.errStr(.c23_auto_not_allowed, tokenIdx, "typedef");
            ID.d.qt = .invalid;
        } else {
            ID.d.qt = try Attribute.applyTypeAttributes(p, ID.d.qt, attrBufferTop, null);
        }
    } else if (ID.d.qt.is(p.comp, .func)) {
        if (declSpec.autoType) |tokenIdx| {
            try p.errStr(.auto_type_not_allowed, tokenIdx, "function return type");
            ID.d.qt = .invalid;
        } else if (declSpec.c23Auto) |tokenIdx| {
            try p.errStr(.c23_auto_not_allowed, tokenIdx, "function return type");
            ID.d.qt = .invalid;
        } else {
            ID.d.qt = try Attribute.applyFunctionAttributes(p, ID.d.qt, attrBufferTop);
        }
    } else {
        if (ID.d.qt.is(p.comp, .array)) {
            if (declSpec.autoType) |tokenIdx| {
                try p.errStr(.auto_type_array, tokenIdx, p.getTokenText(ID.d.name));
                ID.d.qt = .invalid;
            } else if (declSpec.c23Auto) |tokenIndex| {
                try p.errStr(.c23_auto_array, tokenIndex, p.getTokenText(ID.d.name));
                ID.d.qt = .invalid;
            }
        } else if (ID.d.qt.is(p.comp, .pointer)) {
            if (declSpec.autoType != null or declSpec.c23Auto != null) {
                // TODO this is not a hard error in clang
                try p.errToken(.auto_type_requires_plain_declarator, p.tokenIdx);
                ID.d.qt = .invalid;
            }
        }
        applyVarAttributes = true;
    }

    if (p.eat(.Equal)) |eq| {
        if (declSpec.storageClass == .typedef or (ID.d.declaratorType != .func and ID.d.qt.is(p.comp, .func)))
            try p.errToken(.illegal_initializer, eq)
        else if (ID.d.qt.get(p.comp, .array)) |arrayTy| {
            if (arrayTy.len == .incomplete) try p.errToken(.vla_init, eq);
        } else if (declSpec.storageClass == .@"extern") {
            try p.err(.extern_initializer);
            declSpec.storageClass = .none;
        }

        incomplete: {
            if (ID.d.qt.isInvalid()) break :incomplete;
            if (ID.d.qt.isC23Auto()) break :incomplete;
            if (ID.d.qt.isAutoType()) break :incomplete;
            if (ID.d.qt.sizeofOrNull(p.comp) != null) break :incomplete;
            if (ID.d.qt.get(p.comp, .array)) |arrayTy| {
                if (arrayTy.len == .incomplete) break :incomplete;
            }
            try p.errStr(.variable_incomplete_ty, ID.d.name, try p.typeStr(ID.d.qt));
            return error.ParsingFailed;
        }

        try p.symStack.pushScope();
        defer p.symStack.popScope();

        const internedName = try p.getInternString(ID.d.name);
        try p.symStack.declareSymbol(internedName, ID.d.qt, ID.d.name, declNode);

        const initContext = p.initContext;
        defer p.initContext = initContext;
        p.initContext = declSpec.initContext(p);

        var initListExpr = try p.initializer(ID.d.qt);
        ID.initializer = initListExpr;

        // int j [] = c; // c -> *int
        // Set incomplete array length if possible.
        if (ID.d.qt.get(p.comp, .array)) |baseArrayTy| {
            if (baseArrayTy.len == .incomplete) if (initListExpr.qt.get(p.comp, .array)) |initArrayTy| {
                switch (initArrayTy.len) {
                    .fixed, .static => |len| {
                        ID.d.qt = (try p.comp.typeStore.put(p.gpa, .{ .array = .{
                            .elem = baseArrayTy.elem,
                            .len = .{ .fixed = len },
                        } })).withQualifiers(ID.d.qt);
                    },
                    else => {},
                }
            };
        }
    }

    const name = ID.d.name;
    if (ID.d.qt.isAutoType() or ID.d.qt.isC23Auto()) {
        if (ID.initializer) |some| {
            ID.d.qt = some.qt;
        } else {
            try p.errStr(
                if (ID.d.qt.isC23Auto()) .c23_auto_requires_initializer else .auto_type_requires_initializer,
                name,
                p.getTokenText(name),
            );
            ID.d.qt = .invalid;
            return ID;
        }
    }

    if (applyVarAttributes)
        ID.d.qt = try Attribute.applyVariableAttributes(p, ID.d.qt, attrBufferTop, null);

    incomplete: {
        if (declSpec.storageClass != .typedef) break :incomplete;
        if (ID.d.qt.isInvalid()) break :incomplete;
        if (ID.d.qt.sizeofOrNull(p.comp)) |_| break :incomplete;

        const initType = ID.d.qt.base(p.comp).type;
        if (declSpec.storageClass == .@"extern") switch (initType) {
            .@"struct", .@"union", .@"enum" => break :incomplete,
            .array => |arrayTy| if (arrayTy.len == .incomplete) break :incomplete,
            else => {},
        };

        // if there was an initializer expression it must have contained an error
        if (ID.initializer != null) break :incomplete;

        if (p.func.qt == null) {
            switch (initType) {
                .array => |arrayTy| if (arrayTy.len == .incomplete) {
                    try p.errStr(.tentative_array, name, try p.typeStr(ID.d.qt));
                    break :incomplete;
                },
                .@"struct", .@"union" => |recordTy| {
                    _ = try p.tentativeDefs.getOrPutValue(p.gpa, recordTy.name, ID.d.name);
                    break :incomplete;
                },
                .@"enum" => |enumTy| {
                    _ = try p.tentativeDefs.getOrPutValue(p.gpa, enumTy.name, ID.d.name);
                    break :incomplete;
                },
                else => {},
            }
        }
        try p.errStr(.variable_incomplete_ty, name, try p.typeStr(ID.d.qt));
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
///  | record-specifier
///  | enum-sepcifier
///  | typedef-name
///  | typeof-specifier
///  | keyword-atomic '(' typeName ')'
///  | keyword-alignas '(' typeName ')'
///  | keyword-alignas '(' integer-const-expression ')'
///  | keyword-c23-alignas '(' typeName ')'
///  | keyword-c23-alignas '(' integer-const-expression ')'
fn parseTypeSpec(p: *Parser, builder: *TypeBuilder) Error!bool {
    const start = p.tokenIdx;
    while (true) {
        try p.parseAttrSpec();

        if (try p.typeof()) |typeofQt| {
            try builder.combineFromTypeof(typeofQt, start);
            continue;
        }

        if (try p.parseTypeQual(builder))
            continue;

        switch (p.currToken()) {
            .KeywordVoid => try builder.combine(.Void, p.tokenIdx),
            .KeywordBool, .KeywordC23Bool => try builder.combine(.Bool, p.tokenIdx),
            .KeywordMSInt8_, .KeywordMSInt8__, .KeywordChar => try builder.combine(.Char, p.tokenIdx),
            .KeywordMSInt16_, .KeywordMSInt16__, .KeywordShort => try builder.combine(.Short, p.tokenIdx),
            .KeywordMSInt32_, .KeywordMSInt32__, .KeywordInt => try builder.combine(.Int, p.tokenIdx),
            .KeywordLong => try builder.combine(.Long, p.tokenIdx),
            .KeywordMSInt64_, .KeywordMSInt64__ => try builder.combine(.LongLong, p.tokenIdx),
            .KeywordInt128 => try builder.combine(.Int128, p.tokenIdx),
            .KeywordSigned => try builder.combine(.Signed, p.tokenIdx),
            .KeywordUnsigned => try builder.combine(.Unsigned, p.tokenIdx),
            .KeywordFp16 => try builder.combine(.FP16, p.tokenIdx),
            .KeywordFloat16 => try builder.combine(.Float16, p.tokenIdx),
            .KeywordFloat => try builder.combine(.Float, p.tokenIdx),
            .KeywordDouble => try builder.combine(.Double, p.tokenIdx),
            .KeywordComplex => try builder.combine(.Complex, p.tokenIdx),
            .KeywordFloat128_, .KeywordFloat128__ => {
                if (!p.comp.hasFloat128())
                    try p.errStr(.type_not_supported_on_target, p.tokenIdx, p.currToken().lexeme().?);
                try builder.combine(.Float128, p.tokenIdx);
            },

            .KeywordAtomic => {
                const atomicToken = p.tokenIdx;
                p.tokenIdx += 1;
                const lp = p.eat(.LParen) orelse {
                    // _Atomic qualifier not _Atomic(typeName)
                    p.tokenIdx = atomicToken;
                    break;
                };
                const baseQt = (try p.parseTypeName()) orelse {
                    try p.err(.expected_type);
                    return error.ParsingFailed;
                };

                try p.expectClosing(lp, .RParen);

                const newSpec = TypeBuilder.fromType(p.comp, baseQt);
                try builder.combine(newSpec, atomicToken);

                if (builder.atomic != null)
                    try p.errStr(.duplicate_declspec, atomicToken, "atomic")
                else
                    builder.atomic = atomicToken;

                continue;
            },

            .KeywordAlignas, .KeywordC23Alignas => {
                const alignToken = p.tokenIdx;
                p.tokenIdx += 1;
                const lparen = try p.expectToken(.LParen);
                const typenameStart = p.tokenIdx;
                if (try p.parseTypeName()) |innerQt| {
                    if (!innerQt.alignable(p.comp))
                        try p.errStr(.invalid_alignof, typenameStart, try p.typeStr(innerQt));

                    const alignment = Attribute.Alignment{ .requested = innerQt.alignof(p.comp) };
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
                try builder.combine(TypeBuilder.fromType(p.comp, enumTy), tagToken);
                continue;
            },

            .KeywordStruct, .KeywordUnion => {
                const tagToken = p.tokenIdx;
                const recordTy = try p.parseRecordSpec();
                try builder.combine(TypeBuilder.fromType(p.comp, recordTy), tagToken);
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

                if (declspecFound)
                    internedName = try p.getInternString(p.tokenIdx);

                const typedef = (try p.symStack.findTypedef(internedName, p.tokenIdx, builder.type == .None)) orelse break;
                if (!builder.combineTypedef(typedef.qt))
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
                } else if (res.value.compare(.lte, .zero, p.comp)) {
                    bits = 0;
                } else {
                    bits = res.value.toInt(u64, p.comp) orelse std.math.maxInt(u64);
                }

                try builder.combine(.{ .BitInt = bits }, bitIntToken);
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

    const str = std.fmt.allocPrint(
        p.comp.diagnostics.arena.allocator(), //TODO
        "(anonymous {s} at {s}:{d}:{d})",
        .{ kindStr, source.path, lineAndCol.lineNO, lineAndCol.col },
    ) catch unreachable;
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
fn parseRecordSpec(p: *Parser) Error!QualType {
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
            return prev.qt;
        } else {
            // this is a forward declaration, create a new record type.
            // this is a forward declaration, create a new record type.
            const recordTy: Type.Record = .{
                .name = internedName,
                .layout = null,
                .fields = &.{},
            };
            const recordQt = try p.comp.typeStore.put(p.gpa, if (isStruct)
                .{ .@"struct" = recordTy }
            else
                .{ .@"union" = recordTy });

            const attributedQt = try Attribute.applyTypeAttributes(p, recordQt, attrBufferTop, null);
            try p.symStack.define(.{
                .kind = if (isStruct) .@"struct" else .@"union",
                .name = internedName,
                .token = ident,
                .qt = attributedQt,
                .value = .{},
            });

            const fw: Node.ContainerForwardDecl = .{
                .nameOrKindToken = ident,
                .containerQt = attributedQt,
                .definition = null,
            };
            try p.declBuffer.append(try p.addNode(if (isStruct)
                .{ .structForwardDecl = fw }
            else
                .{ .unionForwardDecl = fw }));
            return attributedQt;
        }
    };

    var done = false;
    errdefer if (!done) p.skipTo(.RBrace);

    // Get forward declared type or create a new one
    var recordType: Type.Record, const qt: QualType = blk: {
        const internedName = if (maybeIdent) |ident| interned: {
            const identStr = p.getTokenText(ident);
            const internedName = try p.comp.internString(identStr);
            if (try p.symStack.defineTag(internedName, p.tokenIds[kindToken], ident)) |prev| {
                const recordTy = prev.qt.getRecord(p.comp).?;
                if (recordTy.layout != null) {
                    // if the record isn't incomplete, this is a redefinition
                    try p.errStr(.redefinition, ident, identStr);
                    try p.errToken(.previous_definition, prev.token);
                } else {
                    break :blk .{ recordTy, prev.qt };
                }
            }
            break :interned internedName;
        } else try p.getAnonymousName(kindToken);

        // Initially create ty as a regular non-attributed type, since attributes for a record
        // can be specified after the closing rbrace, which we haven't encountered yet.
        const recordTy: Type.Record = .{
            .name = internedName,
            .layout = null,
            .fields = &.{},
        };
        const recordQt = try p.comp.typeStore.put(p.gpa, if (isStruct)
            .{ .@"struct" = recordTy }
        else
            .{ .@"union" = recordTy });

        // declare a symbol for the type
        if (maybeIdent != null) {
            try p.symStack.define(.{
                .kind = if (isStruct) .@"struct" else .@"union",
                .name = recordTy.name,
                .token = maybeIdent.?,
                .qt = recordQt,
                .value = .{},
            });
        }

        break :blk .{ recordTy, recordQt };
    };

    // reserve space for this record
    try p.declBuffer.append(undefined);
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

    const fields = p.recordBuffer.items[recordBufferTop..];

    if (p.record.flexibleField) |some| {
        if (p.recordBuffer.items[recordBufferTop..].len == 1 and isStruct)
            try p.errToken(.flexible_in_empty, some);
    }

    if (p.recordBuffer.items.len == recordBufferTop) {
        try p.errStr(.empty_record, kindToken, p.getTokenText(kindToken));
        try p.errStr(.empty_record_size, kindToken, p.getTokenText(kindToken));
    }

    try p.expectClosing(lb, .RBrace);
    done = true;
    try p.parseAttrSpec();

    const attributedQt = try Attribute.applyTypeAttributes(p, qt, attrBufferTop, null);

    // if (ty.specifier == .Attributed and maybeIdent != null) {
    //     const identStr = p.getTokenText(maybeIdent.?);
    //     const internedName = try p.comp.internString(identStr);
    //     const ptr = p.symStack.getPtr(internedName, .tags);
    //     ptr.type = ty;
    // }

    for (fields) |field| {
        if (field.qt.sizeofOrNull(p.comp) == null and !field.qt.is(p.comp, .array))
            break;
    } else {
        const pragmaPackValue = switch (p.comp.langOpts.emulate) {
            .clang => startingPragmaPack,
            .gcc => p.pragmaPack,
            .msvc => p.pragmaPack,
        };

        if (RecordLayout.compute(fields, attributedQt, p.comp, pragmaPackValue)) |layout| {
            recordType.fields = fields;
            recordType.layout = layout;
        } else |er| switch (er) {
            error.Overflow => try p.errStr(.record_too_large, maybeIdent orelse kindToken, try p.typeStr(qt)),
        }

        // Override previous incomplete type.
        const baseType = attributedQt.base(p.comp);
        if (isStruct) {
            std.debug.assert(baseType.type.@"struct".name == recordType.name);
            try p.comp.typeStore.set(p.gpa, .{ .@"struct" = recordType }, @intFromEnum(baseType.qt._index));
        } else {
            std.debug.assert(baseType.type.@"union".name == recordType.name);
            try p.comp.typeStore.set(p.gpa, .{ .@"union" = recordType }, @intFromEnum(baseType.qt._index));
        }
    }

    // finish by creating a node
    const cd: Node.ContainerDecl = .{
        .nameOrKindToken = maybeIdent orelse kindToken,
        .containerQt = attributedQt,
        .fields = p.declBuffer.items[declBufferTop..],
    };

    p.declBuffer.items[declBufferTop - 1] = try p.addNode(if (isStruct) .{ .structDecl = cd } else .{ .unionDecl = cd });
    if (p.func.qt == null)
        _ = p.tentativeDefs.remove(recordType.name);

    return attributedQt;
}

/// record-declarations: (keyword-extension? recordDecl | static-assert-declaration)*
fn parseRecordDecls(p: *Parser) Error!void {
    while (true) {
        if (try p.pragma()) continue;
        if (try p.parseOrNextDecl(parseStaticAssert)) continue;

        if (p.eat(.KeywordGccExtension)) |_| {
            const saveExtension = p.extensionSuppressd;
            defer p.extensionSuppressd = saveExtension;
            p.extensionSuppressd = true;

            if (try p.parseOrNextDecl(parseRecordDecl))
                continue;

            try p.err(.expected_type);
            p.nextExternDecl();
            continue;
        }

        if (try p.parseOrNextDecl(parseRecordDecl))
            continue;
        break;
    }
}

/// record-declaration : type-specifier+ (record-declarator (',' record-declarator)*)?
/// record-declarator : declarator (':' integer-constant-expression)?
fn parseRecordDecl(p: *Parser) Error!bool {
    const attrBufferTop = p.attrBuffer.len;
    defer p.attrBuffer.len = attrBufferTop;

    const baseQt: QualType = blk: {
        const start = p.tokenIdx;
        var builder: TypeStore.Builder = .{ .parser = p };
        while (true) {
            if (try p.parseTypeSpec(&builder)) continue;
            const id = p.currToken();
            switch (id) {
                .KeywordAuto => {
                    if (p.comp.langOpts.standard.atLeast(.c23)) break;

                    try p.errStr(.c23_auto_not_allowed, p.tokenIdx, if (p.record.kind == .KeywordStruct) "struct member" else "union member");
                    try builder.combine(.C23Auto, p.tokenIdx);
                },
                .KeywordAutoType => {
                    try p.errToken(.auto_type_extension, p.tokenIdx);
                    try p.errStr(.auto_type_not_allowed, p.tokenIdx, if (p.record.kind == .KeywordStruct) "struct member" else "union member");
                    try builder.combine(.AutoType, p.tokenIdx);
                },
                else => break,
            }
            p.tokenIdx += 1;
            break;
        }
        if (p.tokenIdx == start) return false;
        break :blk switch (builder.type) {
            .AutoType, .C23Auto => .invalid,
            else => try builder.finish(),
        };
    };
    try p.parseAttrSpec(); // .record

    while (true) {
        const thisDeclTop = p.attrBuffer.len;
        defer p.attrBuffer.len = thisDeclTop;

        try p.parseAttrSpec();
        // 0 means unnamed
        var nameToken: TokenIndex = 0;
        var qt = baseQt;
        var bitsNode: ?Node.Index = null;
        var bits: ?u32 = null;
        const firstToken = p.tokenIdx;
        if (try p.declarator(qt, .record)) |d| {
            nameToken = d.name;
            qt = d.qt;
        }

        if (p.eat(.Colon)) |_| bits: {
            const bitsToken = p.tokenIdx;
            const res = try p.parseIntegerConstExpr(.GNUFoldingExtension);
            if (!qt.isInt(p.comp)) {
                try p.errStr(.non_int_bitfield, firstToken, try p.typeStr(qt));
                break :bits;
            }

            if (res.value.isNone()) {
                try p.errToken(.expected_integer_constant_expr, bitsToken);
                break :bits;
            } else if (res.value.compare(.lt, .zero, p.comp)) {
                try p.errStr(.negative_bitwidth, firstToken, try res.str(p));
                break :bits;
            }

            // incomplete size error is reported later
            const bitSize = qt.bitSizeofOrNull(p.comp) orelse break :bits;
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
        const toAppend = try Attribute.applyFieldAttributes(p, &qt, attrBufferTop);

        const attrIndex: u32 = @intCast(p.comp.typeStore.attributes.items.len);
        const attrLen: u32 = @intCast(toAppend.len);
        try p.comp.typeStore.attributes.appendSlice(p.gpa, toAppend);

        if (nameToken == 0 and bits == null) unnamed: {
            // don't allow incompelete size fields in anonymous record.
            if (qt.sizeofOrNull(p.comp) == null) break :unnamed;
            switch (qt.base(p.comp).type) {
                .@"enum" => break :unnamed,
                .@"struct", .@"union" => |recordTy| if (recordTy.isAnonymous(p.comp)) {
                    // An anonymous record appears as indirect fields on the parent
                    try p.recordBuffer.append(.{
                        .name = try p.getAnonymousName(firstToken),
                        .qt = qt,
                        ._attr_index = attrIndex,
                        ._attr_len = attrLen,
                    });
                    const node = try p.addNode(.{
                        .recordField = .{
                            .nameOrFirstToken = nameToken,
                            .qt = qt,
                            .bitWidth = null,
                        },
                    });
                    try p.declBuffer.append(node);
                    try p.record.addFieldsFromAnonymous(p, recordTy);
                    break; // must be followed by a semicolon
                } else break :unnamed,
                else => {},
            }
            try p.err(.missing_declaration);
        } else {
            const internedName = if (nameToken != 0) try p.getInternString(nameToken) else try p.getAnonymousName(firstToken);
            try p.recordBuffer.append(.{
                .name = internedName,
                .qt = qt,
                .nameToken = nameToken,
                .bitWidth = if (bits) |some| @enumFromInt(some) else .null,
                ._attr_index = attrIndex,
                ._attr_len = attrLen,
            });

            if (nameToken != 0)
                try p.record.addField(p, internedName, nameToken);

            const node = try p.addNode(.{
                .recordField = .{
                    .nameOrFirstToken = nameToken,
                    .qt = qt,
                    .bitWidth = bitsNode,
                },
            });
            try p.declBuffer.append(node);
        }

        if (!qt.isInvalid()) {
            const fieldType = qt.base(p.comp);
            switch (fieldType.type) {
                .func => {
                    try p.errToken(.func_field, firstToken);
                    qt = .invalid;
                },
                .array => |arrayTy| switch (arrayTy.len) {
                    .static, .unspecifiedVariable => unreachable,
                    .variable => {
                        try p.errToken(.vla_field, firstToken);
                        qt = .invalid;
                    },
                    .fixed => {},
                    .incomplete => {
                        if (p.record.kind == .KeywordUnion) {
                            try p.errToken(.flexible_in_union, firstToken);
                            qt = .invalid;
                        }
                        if (p.record.flexibleField) |some| {
                            if (p.record.kind == .KeywordStruct) {
                                try p.errToken(.flexible_non_final, some);
                            }
                        }
                        p.record.flexibleField = firstToken;
                    },
                },
                else => if (fieldType.qt.sizeofOrNull(p.comp) == null) {
                    try p.errStr(.field_incomplete_ty, firstToken, try p.typeStr(qt));
                } else if (p.record.flexibleField) |some| {
                    std.debug.assert(some != firstToken);
                    if (p.record.kind == .KeywordStruct)
                        try p.errToken(.flexible_non_final, some);
                },
            }
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

// specifier-qualifier-list : type-specifier+
fn parseSpecQuals(p: *Parser) Error!?QualType {
    var builder: TypeBuilder = .{ .parser = p };
    if (try p.parseTypeSpec(&builder))
        return try builder.finish();
    return null;
}

fn checkEnumFixedTy(p: *Parser, fixedTy: ?QualType, identToken: TokenIndex, prev: Symbol) !void {
    const enumTy = prev.qt.get(p.comp, .@"enum").?;
    if (fixedTy) |some| {
        if (!enumTy.fixed) {
            try p.errToken(.enum_prev_nonfixed, identToken);
            try p.errToken(.previous_definition, prev.token);
            return error.ParsingFailed;
        }

        if (!enumTy.tag.?.eql(some, p.comp)) {
            const str = try p.typePairStrExtra(some, " (was ", enumTy.tag.?);
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
fn parseEnumSpec(p: *Parser) Error!QualType {
    const enumToken = p.tokenIdx;
    p.tokenIdx += 1;

    const attrBufferTop = p.attrBuffer.len;
    defer p.attrBuffer.len = attrBufferTop;

    try p.parseAttrSpec();

    const maybeIdent = try p.eatIdentifier();
    const fixedQt = if (p.eat(.Colon)) |colon| fixed: {
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
        if (try p.symStack.findTag(internedName, p.tokenIds[enumToken], ident, p.currToken())) |prev| {
            return prev.qt;
        } else {
            // this is a forward declaration
            const enumQt = try p.comp.typeStore.put(p.gpa, .{ .@"enum" = .{
                .name = internedName,
                .tag = fixedQt orelse .int,
                .fixed = fixedQt != null,
                .incomplete = true,
                .fields = &.{},
            } });
            const attributedQt = try Attribute.applyTypeAttributes(p, enumQt, attrBufferTop, null);

            try p.symStack.define(.{
                .kind = .@"enum",
                .name = internedName,
                .token = ident,
                .qt = attributedQt,
                .value = .{},
            });

            try p.declBuffer.append(try p.addNode(.{
                .enumForwardDecl = .{
                    .nameOrKindToken = ident,
                    .containerQt = attributedQt,
                    .definition = null,
                },
            }));
            return attributedQt;
        }
    };

    var done = false;
    errdefer if (!done) p.skipTo(.RBrace);

    // Get forward declared type or create a new one
    var defined = false;
    var enumType: Type.Enum, const qt: QualType = blk: {
        const internedName = if (maybeIdent) |ident| interned: {
            const identStr = p.getTokenText(ident);
            const internedName = try p.comp.internString(identStr);
            if (try p.symStack.defineTag(internedName, p.tokenIds[enumToken], ident)) |prev| {
                const enumTy = prev.qt.get(p.comp, .@"enum").?;
                if (!enumTy.incomplete) {
                    // if the record isn't incomplete, this is a redefinition
                    try p.errStr(.redefinition, ident, identStr);
                    try p.errToken(.previous_definition, prev.token);
                } else {
                    defined = true;
                    break :blk .{ enumTy, prev.qt };
                }
            }
            break :interned internedName;
        } else try p.getAnonymousName(enumToken);

        // Initially create ty as a regular non-attributed type, since attributes for a record
        // can be specified after the closing rbrace, which we haven't encountered yet.
        const enumTy: Type.Enum = .{
            .name = internedName,
            .tag = fixedQt orelse .int,
            .incomplete = true,
            .fixed = fixedQt != null,
            .fields = &.{},
        };
        const enumQt = try p.comp.typeStore.put(p.gpa, .{ .@"enum" = enumTy });
        break :blk .{ enumTy, enumQt };
    };

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

    var e = Enumerator.init(fixedQt);
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

    const attributedQt = try Attribute.applyTypeAttributes(p, qt, attrBufferTop, null);

    if (!enumType.fixed) {
        enumType.tag = try e.getTypeSpecifier(p, attributedQt.enumIsPacked(p.comp), maybeIdent orelse enumToken);
    }

    const enumFields = p.enumBuffer.items[enumBufferTop..];
    const fieldNodes = p.listBuffer.items[listBufferTop..];

    if (fixedQt == null) {
        // Coerce all fields to final type.
        for (enumFields, fieldNodes) |*field, fieldNode| {
            if (field.qt.eql(.int, p.comp)) continue;

            const sym = p.symStack.get(field.name, .vars) orelse continue;
            if (sym.kind != .enumeration) continue; // already an error

            var res: Result = .{ .node = undefined, .qt = field.qt, .value = sym.value };
            const destTy: QualType = if (p.comp.fixedEnumTagType()) |some|
                some
            else if (try res.intFitsInType(p, .int))
                .int
            else if (!res.qt.eql(enumType.tag, p.comp))
                enumType.tag
            else
                continue;

            const symbol = p.symStack.getPtr(field.name, .vars);
            try symbol.value.intCast(destTy, p.comp);

            symbol.qt = destTy;
            field.qt = destTy;
            res.qt = destTy;

            // Create a new enum_field node with the correct type.
            var newFieldNode = fieldNode.get(&p.tree);
            newFieldNode.enumField.qt = destTy;

            if (newFieldNode.enumField.init) |some| {
                res.node = some;
                try res.implicitCast(p, .IntCast, some.tok(&p.tree));
                newFieldNode.enumField.init = res.node;
            }

            try p.tree.setNode(@intFromEnum(fieldNode), newFieldNode);
        }
    }

    { // Override previous incomplete type
        enumType.fields = enumFields;
        enumType.incomplete = false;
        const baseType = attributedQt.base(p.comp);
        std.debug.assert(baseType.type.@"enum".name == enumType.name);
        try p.comp.typeStore.set(p.gpa, .{ .@"enum" = enumType }, @intFromEnum(baseType.qt._index));
    }

    // declare a symbol for the type
    if (maybeIdent != null and !defined) {
        try p.symStack.define(.{
            .kind = .@"enum",
            .name = enumType.name,
            .qt = attributedQt,
            .token = maybeIdent.?,
            .value = .{},
        });
    }
    // finish by creating a node
    const node = try p.addNode(.{
        .enumDecl = .{
            .nameOrKindToken = maybeIdent orelse enumToken,
            .containerQt = attributedQt,
            .fields = fieldNodes,
        },
    });
    p.declBuffer.items[declBufferTop - 1] = node;

    if (p.func.qt == null)
        _ = p.tentativeDefs.remove(enumType.name);
    return attributedQt;
}

const Enumerator = struct {
    value: Value = .{},
    qt: QualType,
    // tracks the minimum number of bits required to represent the positive part of the enumeration constant
    numPositiveBits: usize = 0,
    // tracks the minimum number of bits required to represent the negative part of the enumeration constant
    numNegativeBits: usize = 0,
    // whether indicate fixed type
    fixed: bool,

    fn init(fixedTy: ?QualType) Enumerator {
        return .{
            .qt = fixedTy orelse .int,
            .fixed = (fixedTy != null),
        };
    }

    /// Increment enumerator value adjusting type if needed.
    fn incr(e: *Enumerator, p: *Parser, token: TokenIndex) !void {
        const oldVal = e.value;
        if (oldVal.isNone()) {
            // First enumerator, set to 0 fits in all types.
            e.value = .zero;
            return;
        }

        if (try e.value.add(e.value, .one, e.qt, p.comp)) {
            if (e.fixed) {
                try p.errStr(.enum_not_representable_fixed, token, try p.typeStr(e.qt));
                return;
            }

            if (p.comp.nextLargestIntSameSign(e.qt)) |larger| {
                try p.errToken(.enumerator_overflow, token);
                e.qt = larger;
            } else {
                const byteSize = e.qt.sizeof(p.comp);
                const bitSize: u8 = @intCast(if (e.qt.isUnsignedInt(p.comp)) byteSize * 8 else byteSize * 8 - 1);
                try p.errExtra(.enum_not_representable, token, .{ .pow2AsString = bitSize });
                e.qt = .ulonglong;
            }
            _ = try e.value.add(oldVal, .one, e.qt, p.comp);
        }
    }

    /// Set enumerator value to specified value.
    fn set(e: *Enumerator, p: *Parser, res: *Result, token: TokenIndex) !void {
        if (res.qt.isInvalid()) return;
        if (e.fixed and !res.qt.eql(e.qt, p.comp)) {
            if (!try res.intFitsInType(p, e.qt)) {
                try p.errStr(.enum_not_representable_fixed, token, try p.typeStr(e.qt));
                return error.ParsingFailed;
            }
            res.qt = e.qt;
            try res.implicitCast(p, .IntCast, token);
            e.value = res.value;
        } else {
            try res.castToInt(p, res.qt.promoteInt(p.comp), token);
            e.qt = res.qt;
            e.value = res.value;
        }
    }

    fn getTypeSpecifier(e: *const Enumerator, p: *Parser, isPacked: bool, token: TokenIndex) !QualType {
        if (p.comp.fixedEnumTagType()) |tagSpecifier| return tagSpecifier;

        const charWidth = Type.IntType.Int.bits(p.comp);
        const shortWidth = Type.IntType.Int.bits(p.comp);
        const intWidth = Type.IntType.Int.bits(p.comp);
        if (e.numNegativeBits > 0) {
            if (isPacked and e.numNegativeBits <= charWidth and e.numPositiveBits < charWidth)
                return .schar
            else if (isPacked and e.numNegativeBits <= shortWidth and e.numPositiveBits < shortWidth)
                return .short
            else if (e.numNegativeBits <= intWidth and e.numPositiveBits < intWidth)
                return .int;

            const longWidth = Type.IntType.Long.bits(p.comp);
            if (e.numNegativeBits <= longWidth and e.numPositiveBits < longWidth)
                return .long;

            const llongWidth = Type.IntType.LongLong.bits(p.comp);
            if (e.numNegativeBits > llongWidth or e.numPositiveBits >= llongWidth)
                try p.errToken(.enum_too_large, token);
            return .longlong;
        }

        if (isPacked and e.numPositiveBits <= charWidth)
            return .uchar
        else if (isPacked and e.numPositiveBits <= shortWidth)
            return .ushort
        else if (e.numPositiveBits <= intWidth)
            return .uint
        else if (e.numPositiveBits <= Type.IntType.Long.bits(p.comp))
            return .ulong;

        return .ulonglong;
    }

    fn str(e: *const Enumerator, p: *Parser) ![]const u8 {
        return (Result{
            .node = undefined, // Result.str does not use the node
            .qt = e.qt,
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

    if (e.qt.isUnsignedInt(p.comp) or e.value.compare(.gte, .zero, p.comp))
        e.numPositiveBits = @max(e.numPositiveBits, e.value.minUnsignedBits(p.comp))
    else
        e.numNegativeBits = @max(e.numNegativeBits, e.value.minSignedBits(p.comp));

    if (errStart == p.comp.diagnostics.list.items.len) {
        // only do these warnings if we didn't already warn about overflow or non-representable values
        if (e.value.compare(.lt, .zero, p.comp)) {
            const minValue = try Value.minInt(.int, p.comp);
            if (e.value.compare(.lt, minValue, p.comp))
                try p.errStr(.enumerator_too_small, nameToken, try e.str(p));
        } else {
            const maxValue = try Value.maxInt(.int, p.comp);
            if (e.value.compare(.gt, maxValue, p.comp))
                try p.errStr(.enumerator_too_large, nameToken, try e.str(p));
        }
    }

    const attributedQt = try Attribute.applyEnumeratorAttributes(p, e.qt, attrBufferTop);
    const node = try p.addNode(.{
        .enumField = .{
            .nameToken = nameToken,
            .qt = attributedQt,
            .init = fieldInit,
        },
    });

    try p.tree.valueMap.put(p.gpa, node, e.value);

    const internedName = try p.getInternString(nameToken);
    try p.symStack.defineEnumeration(internedName, attributedQt, nameToken, e.value, node);

    return .{
        .field = .{
            .name = internedName,
            .qt = attributedQt,
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
fn parseTypeQual(p: *Parser, b: *TypeBuilder) Error!bool {
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
    qt: QualType,
    /// This is used to retain additional information needed for function declarations.
    /// for c89 old style function
    oldTypeFunc: ?TokenIndex = null,

    /// What kind of a type did this declarator declare?
    /// Used redundantly with `qt` in case it was set to `.invalid` by `validate`.
    declaratorType: enum { other, func, array, pointer } = .other,

    const Kind = enum { normal, abstract, param, record };

    fn validate(d: *Declarator, p: *Parser, sourceToken: TokenIndex) Parser.Error!void {
        if (!try validateExtra(p, d.qt, sourceToken)) return;
        if (d.declaratorType == .func) return;
        if (d.qt.isAutoType() or d.qt.isC23Auto()) return;
        d.qt = .invalid;
    }

    // Returns true if the type contained invalid or auto types.
    fn validateExtra(p: *Parser, cur: QualType, sourceToken: TokenIndex) Parser.Error!bool {
        if (cur.isInvalid()) return true;
        if (cur.isAutoType()) return true;
        if (cur.isC23Auto()) return true;

        switch (cur.type(p.comp)) {
            .pointer => |pointerTy| return validateExtra(p, pointerTy.child, sourceToken),
            .array => |arrayTy| {
                const elemQt = arrayTy.elem;
                if (try validateExtra(p, elemQt, sourceToken)) return true;

                if (elemQt.sizeofOrNull(p.comp) == null) {
                    try p.errStr(.array_incomplete_elem, sourceToken, try p.typeStr(elemQt));
                    return true;
                }

                if (elemQt.is(p.comp, .func)) {
                    try p.errToken(.array_func_elem, sourceToken);
                    return true;
                }

                if (arrayTy.len == .static and elemQt.is(p.comp, .array))
                    try p.errToken(.static_non_outermost_array, sourceToken);

                if (cur.isQualified() and elemQt.is(p.comp, .array))
                    try p.errToken(.qualifier_non_outermost_array, sourceToken);

                return false;
            },
            .func => |funcTy| {
                const retQt = funcTy.returnType;
                if (try validateExtra(p, retQt, sourceToken)) return true;

                if (retQt.is(p.comp, .array)) try p.errToken(.func_cannot_return_array, sourceToken);
                if (retQt.is(p.comp, .func)) try p.errToken(.func_cannot_return_func, sourceToken);

                if (retQt.@"const") try p.errStr(.qual_on_ret_type, sourceToken, "const");
                if (retQt.@"volatile") try p.errStr(.qual_on_ret_type, sourceToken, "volatile");
                if (retQt.get(p.comp, .float)) |float| {
                    if (float == .FP16 and !p.comp.hasHalfPrecisionFloatABI()) {
                        try p.errStr(.suggest_pointer_for_invalid_fp16, sourceToken, "function return value");
                    }
                }
                return false;
            },
            else => return false,
        }
    }
};

/// declarator: pointer? direct-declarator
/// abstract-declarator
///  : pointer
///  : pointer? direct-abstract-declarator
/// pointer : '*' typeQual* pointer?
fn declarator(p: *Parser, baseQt: QualType, kind: Declarator.Kind) Error!?Declarator {
    const start = p.tokenIdx;
    var d = Declarator{ .name = 0, .qt = baseQt };

    // Parse potential pointer declarators first.
    while (p.eat(.Asterisk)) |_| {
        d.declaratorType = .pointer;
        var builder: TypeBuilder = .{ .parser = p };
        _ = try p.parseTypeQual(&builder);

        const pointer_qt = try p.comp.typeStore.put(p.gpa, .{ .pointer = .{
            .child = d.qt,
            .decayed = null,
        } });
        d.qt = try builder.finishQuals(pointer_qt);
    }

    const maybeIdent = p.tokenIdx;
    if (kind != .abstract and (try p.eatIdentifier()) != null) {
        d.name = maybeIdent;
        const combineToken = p.tokenIdx;
        d.qt = try p.directDeclarator(&d, kind);
        try d.validate(p, combineToken);
        return d;
    } else if (p.eat(.LParen)) |lp| blk: {
        var res = (try p.declarator(.invalid, kind)) orelse {
            p.tokenIdx = lp;
            break :blk;
        };

        try p.expectClosing(lp, .RParen);
        const suffixStart = p.tokenIdx;
        const outer = try p.directDeclarator(&d, kind);

        // Correct the base type now that it is known.
        // If outer is invalid there was no pointer, array or function type.
        if (outer.isInvalid() or res.qt.isInvalid()) {
            res.qt = outer;
        } else {
            var cur = res.qt;
            while (true) {
                switch (cur.type(p.comp)) {
                    .pointer => |pointerTy| if (!pointerTy.child.isInvalid()) {
                        cur = pointerTy.child;
                        continue;
                    },
                    .array => |arrayTy| if (!arrayTy.elem.isInvalid()) {
                        cur = arrayTy.elem;
                        continue;
                    },
                    .func => |funcTy| if (!funcTy.returnType.isInvalid()) {
                        cur = funcTy.returnType;
                        continue;
                    },
                    else => unreachable,
                }
                // Child type is always stored in repr.data[0]
                p.comp.typeStore.types.items(.data)[@intFromEnum(cur._index)][0] = @bitCast(outer);
                break;
            }
        }

        try res.validate(p, suffixStart);
        return res;
    }

    const expectedIdent = p.tokenIdx;

    d.qt = try p.directDeclarator(&d, kind);

    if (kind == .normal and !d.qt.isInvalid()) {
        switch (d.qt.base(p.comp).type) {
            .@"enum", .@"struct", .@"union" => {},
            else => {
                try p.errToken(.expected_ident_or_l_paren, expectedIdent);
                return error.ParsingFailed;
            },
        }
    }

    try d.validate(p, expectedIdent);
    if (start == p.tokenIdx) return null;

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
fn directDeclarator(
    p: *Parser,
    baseDeclarator: *Declarator,
    kind: Declarator.Kind,
) Error!QualType {
    if (p.eat(.LBracket)) |lb| {
        // Check for C23 attribute
        if (p.currToken() == .LBracket) {
            switch (kind) {
                .normal, .record => if (p.comp.langOpts.standard.atLeast(.c23)) {
                    p.tokenIdx -= 1;
                    return baseDeclarator.qt;
                },
                .param, .abstract => {},
            }
            try p.err(.expected_expr);
            return error.ParsingFailed;
        }

        var builder: TypeBuilder = .{ .parser = p };
        var gotQuals = try p.parseTypeQual(&builder);
        var static = p.eat(.KeywordStatic);

        if (static != null and !gotQuals)
            gotQuals = try p.parseTypeQual(&builder);

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
            builder = .{ .parser = p };
            star = null;
        }

        if (static) |_| _ = try p.expectResult(optSize);

        const outer = try p.directDeclarator(baseDeclarator, kind);

        // Set after call to `directDeclarator` since we will return an
        // array type from here
        baseDeclarator.declaratorType = .array;

        if (optSize != null and !optSize.?.qt.isInt(p.comp)) {
            try p.errStr(.array_size_non_int, sizeToken, try p.typeStr(optSize.?.qt));
            return error.ParsingFailed;
        }

        if (optSize) |size| {
            if (size.value.isNone()) {
                try p.errToken(.vla, sizeToken);
                if (p.func.qt == null and kind != .param and p.record.kind == .Invalid)
                    try p.errToken(.variable_len_array_file_scope, baseDeclarator.name);

                const arrayQt = try p.comp.typeStore.put(p.gpa, .{ .array = .{
                    .elem = outer,
                    .len = .{ .variable = size.node },
                } });

                if (static) |some| try p.errToken(.useless_static, some);
                return builder.finishQuals(arrayQt);
            } else {
                if (size.value.isZero(p.comp)) {
                    try p.errToken(.zero_length_array, lb);
                } else if (size.value.compare(.lt, .zero, p.comp)) {
                    try p.errToken(.negative_array_size, lb);
                    return error.ParsingFailed;
                }

                var len = size.value.toInt(u64, p.comp) orelse std.math.maxInt(u64);

                // `outer` is validated later so it may be invalid here
                if (!outer.isInvalid() and !outer.isAutoType() and !outer.isC23Auto()) {
                    const outSize = outer.sizeofOrNull(p.comp) orelse 1;
                    const maxElems = p.comp.maxArrayBytes() / @max(1, outSize);
                    if (len > maxElems) {
                        try p.errToken(.array_too_large, lb);
                        len = maxElems;
                    }
                }

                const arrayQt = try p.comp.typeStore.put(p.gpa, .{ .array = .{
                    .elem = outer,
                    .len = if (static != null)
                        .{ .static = len }
                    else
                        .{ .fixed = len },
                } });
                return builder.finishQuals(arrayQt);
            }
        } else if (star) |_| {
            const arrayQt = try p.comp.typeStore.put(p.gpa, .{ .array = .{
                .elem = outer,
                .len = .unspecifiedVariable,
            } });
            return builder.finishQuals(arrayQt);
        } else {
            const arrayQt = try p.comp.typeStore.put(p.gpa, .{ .array = .{
                .elem = outer,
                .len = .incomplete,
            } });
            return builder.finishQuals(arrayQt);
        }
    } else if (p.eat(.LParen)) |lp| {
        var funcType: Type.Func = .{
            .kind = undefined,
            .returnType = undefined,
            .params = &.{},
        };

        if (p.eat(.Ellipsis)) |_| {
            try p.err(.param_before_var_args);
            try p.expectClosing(lp, .RParen);

            funcType.kind = if (p.comp.langOpts.standard.atLeast(.c23)) .Variadic else .Normal;
            funcType.returnType = try p.directDeclarator(baseDeclarator, kind);
            return p.comp.typeStore.put(p.gpa, .{ .func = funcType });
        }

        // Set here so the call to directDeclarator for the return type
        // doesn't clobber this function type's parameters.
        const paramBufferTop = p.paramBuffer.items.len;
        defer p.paramBuffer.items.len = paramBufferTop;

        if (try p.parseParamDecls(baseDeclarator)) |params| {
            funcType.kind = .Normal;
            funcType.params = params;
            if (p.eat(.Ellipsis)) |_| funcType.kind = .Variadic;
        } else if (p.currToken() == .RParen) {
            funcType.kind = if (p.comp.langOpts.standard.atLeast(.c23)) .Normal else .OldStyle;
        } else if (p.currToken() == .Identifier or p.currToken() == .ExtendedIdentifier) {
            baseDeclarator.oldTypeFunc = p.tokenIdx;

            try p.symStack.pushScope();
            defer p.symStack.popScope();

            funcType.kind = .OldStyle;

            while (true) {
                const nameToken = try p.expectIdentifier();
                const internedName = try p.getInternString(nameToken);
                try p.symStack.defineParam(internedName, undefined, nameToken, null);
                try p.paramBuffer.append(.{
                    .name = internedName,
                    .nameToken = nameToken,
                    .qt = .int,
                    .node = .null,
                });

                if (p.eat(.Comma) == null) break;
            }

            funcType.params = p.paramBuffer.items[paramBufferTop..];
        } else {
            try p.err(.expected_param_decl);
        }

        try p.expectClosing(lp, .RParen);

        funcType.returnType = try p.directDeclarator(baseDeclarator, kind);

        // Set after call to `directDeclarator` since we will return
        // a function type from here.
        baseDeclarator.declaratorType = .func;

        return p.comp.typeStore.put(p.gpa, .{ .func = funcType });
    } else {
        return baseDeclarator.qt;
    }
}

/// param-decls : param-decl (',' param-decl)* (',' '...')
/// paramDecl : decl-specifier (declarator | abstract-declarator)
fn parseParamDecls(p: *Parser, d: *Declarator) Error!?[]Type.Func.Param {
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
                .qt = .int,
                .node = .null,
            });

            if (p.eat(.Comma) == null) break;
            if (p.currToken() == .Ellipsis) break;
            continue;
        } else if (p.paramBuffer.items.len == paramBufferTop) {
            return null;
        } else blk: {
            try p.err(.missing_type_specifier);
            break :blk DeclSpec{ .qt = .int };
        };

        var nameToken: TokenIndex = 0;
        var internedName: StringId = .empty;
        const firstToken = p.tokenIdx;
        var paramQt = paramDeclSpec.qt;

        if (paramDeclSpec.autoType) |tokenIndex| {
            try p.errStr(.auto_type_not_allowed, tokenIndex, "function prototype");
            paramQt = .invalid;
        }

        if (paramDeclSpec.c23Auto) |tokenIndex| {
            try p.errStr(.c23_auto_not_allowed, tokenIndex, "function prototype");
            paramQt = .invalid;
        }

        if (try p.declarator(paramQt, .param)) |some| {
            if (some.oldTypeFunc) |tokenIdx|
                try p.errToken(.invalid_old_style_params, tokenIdx);

            try p.parseAttrSpec();
            nameToken = some.name;
            paramQt = some.qt;
        }

        if (paramQt.is(p.comp, .void)) {
            // validate void parameters
            if (p.paramBuffer.items.len == paramBufferTop) {
                if (p.currToken() != .RParen) {
                    try p.err(.void_only_param);
                    if (paramQt.isQualified()) try p.err(.void_param_qualified);
                    return error.ParsingFailed;
                }
                return &.{};
            }

            try p.err(.void_must_be_first_param);
            return error.ParsingFailed;
        } else {
            // Decay params declared as functions or arrays to pointer.
            paramQt = try paramQt.decay(p.comp);
        }

        try paramDeclSpec.validateParam(p);
        paramQt = try Attribute.applyParameterAttributes(p, paramQt, attrBufferTop, .alignas_on_param);

        if (paramQt.get(p.comp, .float)) |float| {
            if (float == .FP16 and !p.comp.hasHalfPrecisionFloatABI()) {
                try p.errStr(.suggest_pointer_for_invalid_fp16, firstToken, "parameters");
            }
        }

        var paramNode: Node.OptIndex = .null;
        if (nameToken != 0) {
            const node = try p.addNode(.{
                .param = .{
                    .nameToken = nameToken,
                    .qt = paramQt,
                    .storageClass = switch (paramDeclSpec.storageClass) {
                        .none => .auto,
                        .register => .register,
                        else => .auto, // Error reported in `validateParam`
                    },
                },
            });
            paramNode = .pack(node);
            internedName = try p.comp.internString(p.getTokenText(nameToken));
            try p.symStack.defineParam(internedName, paramQt, nameToken, node);
        }

        try p.paramBuffer.append(.{
            .name = internedName,
            .nameToken = if (nameToken == 0) firstToken else nameToken,
            .qt = paramQt,
            .node = paramNode,
        });

        if (p.eat(.Comma) == null) break;
        if (p.currToken() == .Ellipsis) break;
    }

    return p.paramBuffer.items[paramBufferTop..];
}

/// type-name : specifier-qualifier-list+ abstract-declarator
fn parseTypeName(p: *Parser) Error!?QualType {
    const attrBufferTop = p.attrBuffer.len;
    defer p.attrBuffer.len = attrBufferTop;

    const ty = (try p.parseSpecQuals()) orelse return null;
    if (try p.declarator(ty, .abstract)) |some| {
        if (some.oldTypeFunc) |tokenIdx|
            try p.errToken(.invalid_old_style_params, tokenIdx);
        return try Attribute.applyTypeAttributes(p, some.qt, attrBufferTop, .align_ignored);
    }
    return try Attribute.applyTypeAttributes(p, ty, attrBufferTop, .align_ignored);
}

fn complexInitializer(p: *Parser, initTy: QualType) Error!Result {
    assert(p.currToken() == .LBrace);
    assert(initTy.is(p.comp, .complex));

    const realTy = initTy.toReal(p.comp);
    if (realTy.isInt(p.comp))
        return p.todo("Complex integer initializer");

    const lbrace = p.tokenIdx;
    p.tokenIdx += 1;
    try p.errToken(.complex_component_init, lbrace);

    const firstToken = p.tokenIdx;
    var first = try p.expect(parseAssignExpr);
    try p.coerceInit(&first, firstToken, realTy);

    const second = if (p.eat(.Comma)) |_| second: {
        const secondToken = p.tokenIdx;
        var second = (try p.parseAssignExpr()) orelse break :second null;
        try p.coerceInit(&second, secondToken, realTy);
        break :second second;
    } else null;

    var extraToken: ?TokenIndex = null;
    while (p.eat(.Comma)) |_| {
        if (p.currToken() == .RBrace) break;
        extraToken = p.tokenIdx;

        if ((try p.parseAssignExpr()) == null) {
            try p.errToken(.expected_expr, p.tokenIdx);
            p.skipTo(.RBrace);
            return error.ParsingFailed;
        }
    }

    try p.expectClosing(lbrace, .RBrace);
    if (extraToken) |tok|
        try p.errToken(.excess_scalar_init, tok);

    var res: Result = .{
        .node = try p.addNode(.{
            .arrayInitExpr = .{
                .containerQt = initTy,
                .items = if (second) |some| &.{ first.node, some.node } else &.{first.node},
                .lbraceToken = lbrace,
            },
        }),
        .qt = initTy,
    };

    const firstValue = p.tree.valueMap.get(first.node) orelse return res;
    const secondValue = if (second) |some| p.tree.valueMap.get(some.node) orelse return res else Value.zero;
    res.value = try Value.intern(p.comp, switch (realTy.bitSizeof(p.comp)) {
        32 => .{ .complex = .{ .cf32 = .{ firstValue.toFloat(f32, p.comp), secondValue.toFloat(f32, p.comp) } } },
        64 => .{ .complex = .{ .cf64 = .{ firstValue.toFloat(f64, p.comp), secondValue.toFloat(f64, p.comp) } } },
        80 => .{ .complex = .{ .cf80 = .{ firstValue.toFloat(f80, p.comp), secondValue.toFloat(f80, p.comp) } } },
        128 => .{ .complex = .{ .cf128 = .{ firstValue.toFloat(f128, p.comp), secondValue.toFloat(f128, p.comp) } } },
        else => unreachable,
    });
    try res.putValue(p);
    return res;
}

/// initializer
///  : assign-expression
///  : braced-initializer
///
/// braced-initializer
///  | '{' initializerItems '}'
pub fn initializer(p: *Parser, initQt: QualType) Error!Result {
    // fast path for non-braced initializers
    if (p.currToken() != .LBrace) {
        const token = p.tokenIdx;
        var res = try p.expect(parseAssignExpr);
        if (try p.coerceArrayInit(&res, token, initQt))
            return res;

        try p.coerceInit(&res, token, initQt);
        return res;
    }
    // We want to parse the initializer even if the target is
    // invalidly inferred.
    var finalInitQt = initQt;
    if (initQt.isAutoType()) {
        try p.err(.auto_type_with_init_list);
        finalInitQt = .invalid;
    } else if (initQt.isC23Auto()) {
        try p.err(.c23_auto_with_init_list);
        finalInitQt = .invalid;
    }

    if (finalInitQt.is(p.comp, .complex))
        return p.complexInitializer(finalInitQt);

    var il: InitList = .{};
    defer il.deinit(p.gpa);

    _ = try p.initializerItem(&il, finalInitQt);

    const res = try p.convertInitList(il, finalInitQt);
    return .{ .qt = res.qt(&p.tree).withQualifiers(finalInitQt), .node = res };
}

/// initializerItems : designation? initializer (',' designation? initializer)* ','?
/// designation : designator-list '='
/// designator-list: designator designator-list designator
/// designator : '[' integer-constant-expression ']'
///            | '.' identifier
pub fn initializerItem(p: *Parser, il: *InitList, initType: QualType) Error!bool {
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

    const isScalar, const isComplex = blk: {
        if (initType.isInvalid()) break :blk .{ false, false };
        const scalarKind = initType.scalarKind(p.comp);
        break :blk .{ scalarKind != .None, !scalarKind.isReal() };
    };

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
        var curQt = initType;
        var curIL = il;
        var designation = false;
        var curIndexHint: ?u64 = null;
        while (true) {
            if (p.eat(.LBracket)) |lbr| {
                const arrayTy = curQt.get(p.comp, .array) orelse {
                    try p.errStr(.invalid_array_designator, lbr, try p.typeStr(curQt));
                    return error.ParsingFailed;
                };

                const exprToken = p.tokenIdx;
                const indexRes = try p.parseIntegerConstExpr(.GNUFoldingExtension);
                try p.expectClosing(lbr, .RBracket);

                if (indexRes.value.isNone()) {
                    try p.errToken(.expected_integer_constant_expr, exprToken);
                    return error.ParsingFailed;
                } else if (indexRes.value.compare(.lt, .zero, p.comp)) {
                    try p.errStr(.negative_array_designator, lb + 1, try indexRes.str(p));
                    return error.ParsingFailed;
                }

                const maxLen = switch (arrayTy.len) {
                    .fixed, .static => |len| len,
                    else => std.math.maxInt(u64),
                };
                const indexInt = indexRes.value.toInt(u64, p.comp) orelse std.math.maxInt(u64);
                if (indexInt >= maxLen) {
                    try p.errStr(.oob_array_designator, lbr + 1, try indexRes.str(p));
                    return error.ParsingFailed;
                }

                curIndexHint = curIndexHint orelse indexInt;
                curIL = try curIL.find(p.gpa, indexInt);
                curQt = arrayTy.elem;
                designation = true;
            } else if (p.eat(.Period)) |period| {
                const fieldToken = try p.expectIdentifier();
                const fieldStr = p.getTokenText(fieldToken);
                const fieldName = try p.comp.internString(fieldStr);

                const recordTy = curQt.getRecord(p.comp) orelse {
                    try p.errStr(.invalid_field_designator, period, try p.typeStr(curQt));
                    return error.ParsingFailed;
                };
                if (!recordTy.hasField(p.comp, fieldName)) {
                    try p.errStr(.no_such_field_designator, period, fieldStr);
                    return error.ParsingFailed;
                }

                // TODO check if union already has field set
                outer: while (true) {
                    for (recordTy.fields, 0..) |f, i| {
                        if (f.name == .empty) {
                            if (f.qt.getRecord(p.comp)) |rec| {
                                // Recurse into anonymous field if it has a field by the name.
                                if (!rec.hasField(p.comp, fieldName)) continue;

                                curQt = f.qt;
                                curIL = try il.find(p.gpa, i);
                                curIndexHint = curIndexHint orelse i;
                                continue :outer;
                            }
                        }

                        if (fieldName == f.name) {
                            curIL = try curIL.find(p.gpa, i);
                            curQt = f.qt;
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

        if (!designation and curQt.hasAttribute(p.comp, .designated_init))
            try p.err(.designated_init_needed);

        var saw = false;
        if (isStrInit and p.isStringInit()) {
            var tempIL: InitList = .{};
            defer tempIL.deinit(p.gpa);
            saw = try p.initializerItem(&tempIL, .void);
        } else if (count == 0 and p.isStringInit()) {
            isStrInit = true;
            saw = try p.initializerItem(il, initType);
        } else if (isScalar and count >= scalarInitsNeeds) {
            // discard further scalars
            var tempIL: InitList = .{};
            defer tempIL.deinit(p.gpa);
            saw = try p.initializerItem(&tempIL, .void);
        } else if (p.currToken() == .LBrace) {
            if (designation) {
                // designation overrides previous value, let existing mechanism handle it
                saw = try p.initializerItem(curIL, curQt);
            } else if (try p.findAggregateInitializer(&curIL, &curQt, &indexHint)) {
                saw = try p.initializerItem(curIL, curQt);
            } else {
                // discard further values
                var tempIL: InitList = .{};
                defer tempIL.deinit(p.gpa);

                saw = try p.initializerItem(curIL, curQt);
                saw = try p.initializerItem(&tempIL, .void);
                if (!warnedExcess)
                    try p.errToken(if (initType.is(p.comp, .array)) .excess_array_init else .excess_struct_init, firstToken);
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
                    if (try p.findScalarInitializerAt(&curIL, &curQt, &res, firstToken, hint)) break :excess;
                } else if (try p.findScalarInitializer(&curIL, &curQt, &res, firstToken)) break :excess;

                if (designation) break :excess;
                if (!warnedExcess) try p.errToken(if (initType.is(p.comp, .array)) .excess_array_init else .excess_struct_init, firstToken);
                warnedExcess = true;

                break :singleItem;
            }

            const arr = try p.coerceArrayInit(&res, firstToken, curQt);
            if (!arr) try p.coerceInit(&res, firstToken, curQt);
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
            if (isScalar and !isComplex) try p.errToken(.excess_scalar_init, firstToken);
        }

        if (p.eat(.Comma) == null) break;
    }
    try p.expectClosing(lb, .RBrace);

    if (isScalar and isComplex and count == 1) // count of 1 means we saw exactly 2 items in the initializer list
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
    qt: *QualType,
    res: *Result,
    firstToken: TokenIndex,
    startIdx: *u64,
) Error!bool {
    switch (qt.base(p.comp).type) {
        .array => |arrayTy| {
            if (il.*.node != .null) return false;
            startIdx.* += 1;

            const maxLen = switch (arrayTy.len) {
                .fixed, .static => |len| len,
                else => std.math.maxInt(u64),
            };

            if (maxLen == 0) {
                try p.errToken(.empty_aggregate_init_braces, firstToken);
                return error.ParsingFailed;
            }

            const arrIL = il.*;
            if (startIdx.* < maxLen) {
                qt.* = arrayTy.elem;
                il.* = try arrIL.find(p.gpa, startIdx.*);
                _ = try p.findScalarInitializer(il, qt, res, firstToken);
                return true;
            }
            return false;
        },

        .@"struct" => |structTy| {
            if (il.*.node != .null) return false;
            startIdx.* += 1;

            if (structTy.fields.len == 0) {
                try p.errToken(.empty_aggregate_init_braces, firstToken);
                return error.ParsingFailed;
            }

            const structIL = il.*;
            if (startIdx.* < structTy.fields.len) {
                qt.* = structTy.fields[@intCast(startIdx.*)].qt;
                il.* = try structIL.find(p.gpa, startIdx.*);
                _ = try p.findScalarInitializer(il, qt, res, firstToken);
                return true;
            }
            return false;
        },

        .@"union" => return false,
        else => return il.*.node == .null,
    }
}

/// Returns true if the value is unused.
fn findScalarInitializer(
    p: *Parser,
    il: **InitList,
    qt: *QualType,
    res: *Result,
    firstToken: TokenIndex,
) Error!bool {
    const actualQt = res.qt;
    if (qt.isInvalid()) return il.*.node == .null;

    switch (qt.base(p.comp).type) {
        .array => |arrayTy| {
            if (il.*.node != .null) return false;
            if (try p.coerceArrayInitExtra(res, firstToken, qt.*, false)) return true;

            const startIdx = il.*.list.items.len;
            var index = if (startIdx != 0) il.*.list.items[startIdx - 1].index else startIdx;

            const maxLen = switch (arrayTy.len) {
                .fixed, .static => |len| len,
                else => std.math.maxInt(u64),
            };
            if (maxLen == 0) {
                try p.errToken(.empty_aggregate_init_braces, firstToken);
                return error.ParsingFailed;
            }

            const arrayIL = il.*;
            const elemTy = arrayTy.elem;
            while (index < maxLen) : (index += 1) {
                qt.* = elemTy;
                il.* = try arrayIL.find(p.gpa, index);
                if (il.*.node == .null and actualQt.eql(elemTy, p.comp))
                    return true;
                if (try p.findScalarInitializer(il, qt, res, firstToken))
                    return true;
            }
            return false;
        },

        .@"struct" => |structTy| {
            if (il.*.node != .null) return false;
            if (actualQt.eql(qt.*, p.comp)) return true;

            const startIdx = il.*.list.items.len;
            var index = if (startIdx != 0) il.*.list.items[startIdx - 1].index + 1 else startIdx;

            if (structTy.fields.len == 0) {
                try p.errToken(.empty_aggregate_init_braces, firstToken);
                return error.ParsingFailed;
            }

            const structIL = il.*;
            while (index < structTy.fields.len) : (index += 1) {
                const field = structTy.fields[@intCast(index)];
                qt.* = field.qt;
                il.* = try structIL.find(p.gpa, index);

                if (il.*.node == .null and actualQt.eql(field.qt, p.comp)) return true;
                if (il.*.node == .null and try p.coerceArrayInitExtra(res, firstToken, qt.*, false)) return true;
                if (try p.findScalarInitializer(il, qt, res, firstToken)) return true;
            }
            return false;
        },

        .@"union" => |unionTy| {
            if (il.*.node != .null) return false;
            if (actualQt.eql(qt.*, p.comp)) return true;

            if (unionTy.fields.len == 0) {
                try p.errToken(.empty_aggregate_init_braces, firstToken);
                return error.ParsingFailed;
            }

            qt.* = unionTy.fields[0].qt;
            il.* = try il.*.find(p.gpa, 0);

            if (try p.coerceArrayInitExtra(res, firstToken, qt.*, false)) return true;
            if (try p.findScalarInitializer(il, qt, res, firstToken)) return true;
            return false;
        },

        else => return il.*.node == .null,
    }
}

fn findAggregateInitializer(p: *Parser, il: **InitList, qt: *QualType, startIdx: *?u64) Error!bool {
    if (qt.isInvalid()) return il.*.node == .null;
    switch (qt.base(p.comp).type) {
        .array => |arrayTy| {
            if (il.*.node != .null) return false;

            const listIdx = il.*.list.items.len;
            const index = if (startIdx.*) |*some| blk: {
                some.* += 1;
                break :blk some.*;
            } else if (listIdx != 0)
                il.*.list.items[listIdx - 1].index + 1
            else
                listIdx;

            const maxLen = switch (arrayTy.len) {
                .fixed, .static => |len| len,
                else => std.math.maxInt(u64),
            };

            if (index < maxLen) {
                qt.* = arrayTy.elem;
                il.* = try il.*.find(p.gpa, index);
                return true;
            }
            return false;
        },

        .@"struct" => |structTy| {
            if (il.*.node != .null) return false;

            const listIdx = il.*.list.items.len;
            const index = if (startIdx.*) |*some| blk: {
                some.* += 1;
                break :blk some.*;
            } else if (listIdx != 0)
                il.*.list.items[listIdx - 1].index + 1
            else
                listIdx;

            const fieldCount = structTy.fields.len;
            if (index < fieldCount) {
                qt.* = structTy.fields[@intCast(index)].qt;
                il.* = try il.*.find(p.gpa, index);
                return true;
            }
            return false;
        },

        .@"union" => |unionTy| {
            if (il.*.node != .null) return false;
            if (startIdx.*) |_| return false; // overrides
            if (unionTy.fields.len == 0) return false;

            qt.* = unionTy.fields[0].qt;
            il.* = try il.*.find(p.gpa, 0);
            return true;
        },

        else => {
            try p.err(.too_many_scalar_init_braces);
            return il.*.node == .null;
        },
    }
}

fn coerceArrayInit(p: *Parser, item: *Result, token: TokenIndex, target: QualType) !bool {
    return p.coerceArrayInitExtra(item, token, target, true);
}

fn coerceArrayInitExtra(p: *Parser, item: *Result, token: TokenIndex, target: QualType, reportError: bool) !bool {
    if (target.isInvalid()) return false;
    const targetArrayTy = target.get(p.comp, .array) orelse return false;

    const isStrLiteral = p.nodeIs(item.node, .stringLiteralExpr);
    const maybeItemArrayTy = item.qt.get(p.comp, .array);
    if (!isStrLiteral and !p.nodeIs(item.node, .compoundLiteralExpr) or maybeItemArrayTy == null) {
        if (!reportError) return false;
        try p.errToken(.array_init_str, token);
        return true; // do not do further coercion
    }

    const targetElem: QualType = targetArrayTy.elem;
    const itemElem: QualType = maybeItemArrayTy.?.elem;
    const targetInt = targetElem.get(p.comp, .int);
    const itemInt = itemElem.get(p.comp, .int);

    const compatible = targetElem.eql(itemElem, p.comp) or
        (isStrLiteral and itemInt.? == .Char and (targetInt.? == .UChar or targetInt.? == .SChar)) or
        (isStrLiteral and itemInt.? == .UChar and (targetInt.? == .UChar or targetInt.? == .SChar or targetInt.? == .Char));

    if (!compatible) {
        if (!reportError) return false;
        const eMsg = " with array of type ";
        try p.errStr(.incompatible_array_init, token, try p.typePairStrExtra(target, eMsg, item.qt));
        return true; // do not do further coercion
    }

    if (targetArrayTy.len == .fixed) {
        const targetLen = targetArrayTy.len.fixed;
        const itemLen = switch (maybeItemArrayTy.?.len) {
            .fixed, .static => |len| len,
            else => unreachable,
        };

        if (isStrLiteral) {
            // the null byte of a string can be dropped
            if (itemLen - 1 > targetLen and reportError)
                try p.errToken(.str_init_too_long, token);
        } else if (itemLen > targetLen and reportError) {
            try p.errStr(
                .arr_init_too_long,
                token,
                try p.typePairStrExtra(target, " with array of type ", item.qt),
            );
        }
    }
    return true;
}

fn coerceInit(p: *Parser, item: *Result, token: TokenIndex, target: QualType) !void {
    // Do not do type coercion on excess items
    if (target.is(p.comp, .void)) return;

    const node = item.node;
    try item.lvalConversion(p, token);
    if (target.isAutoType() or target.isC23Auto()) {
        if (p.getNode(node, .memberAccessExpr) orelse p.getNode(node, .memberAccessPtrExpr)) |access| {
            if (access.isBitFieldWidth(&p.tree) != null) try p.errToken(.auto_type_from_bitfield, token);
        }
        return;
    }

    try item.coerce(p, target, token, .init);
    if (item.value.isNone()) runtime: {
        const tag: Diagnostics.Tag = switch (p.initContext) {
            .runtime => break :runtime,
            .constexpr => .constexpr_requires_const,
            .static => break :runtime,
        };

        p.initContext = .runtime;
        try p.errToken(tag, token);
    }

    if (target.@"const" or p.initContext == .constexpr) {
        var copy = item.*;
        return copy.saveValue(p);
    }
    return item.saveValue(p);
}

fn isStringInit(p: *Parser) bool {
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
fn convertInitList(p: *Parser, il: InitList, initQt: QualType) Error!Node.Index {
    if (initQt.isInvalid()) {
        return try p.addNode(.{ .defaultInitExpr = .{
            .lastToken = p.tokenIdx,
            .qt = initQt,
        } });
    }

    const scalarKind = initQt.scalarKind(p.comp);
    if (scalarKind != .None and scalarKind.isReal()) {
        return il.node.unpack() orelse
            try p.addNode(.{
                .defaultInitExpr = .{
                    .lastToken = il.tok,
                    .qt = initQt,
                },
            });
    }

    switch (initQt.base(p.comp).type) {
        .array => |arrayTy| {
            if (il.node.unpack()) |some| return some;

            const listBuffTop = p.listBuffer.items.len;
            defer p.listBuffer.items.len = listBuffTop;

            const elemType = initQt.childType(p.comp);
            const maxLen = switch (arrayTy.len) {
                .fixed, .static => |len| len,
                // vla invalid, reported earlier
                .variable => return try p.addNode(.{ .defaultInitExpr = .{
                    .lastToken = p.tokenIdx,
                    .qt = initQt,
                } }),
                else => std.math.maxInt(u64),
            };

            var start: u64 = 0;
            for (il.list.items) |*init| {
                if (init.index > start) {
                    const elem = try p.addNode(.{
                        .arrayFillerExpr = .{
                            .lastToken = p.tokenIdx - 1,
                            .count = init.index - start,
                            .qt = elemType,
                        },
                    });
                    try p.listBuffer.append(elem);
                }
                start = init.index + 1;

                const elem = try p.convertInitList(init.list, elemType);
                try p.listBuffer.append(elem);
            }

            var arrInitTy = initQt;
            if (arrayTy.len == .incomplete) {
                arrInitTy = try p.comp.typeStore.put(p.gpa, .{ .array = .{
                    .elem = arrayTy.elem,
                    .len = .{ .fixed = start },
                } });
            } else if (start < maxLen) {
                const elem = try p.addNode(.{
                    .arrayFillerExpr = .{
                        .lastToken = p.tokenIdx - 1,
                        .count = maxLen - start,
                        .qt = elemType,
                    },
                });
                try p.listBuffer.append(elem);
            }

            return try p.addNode(.{
                .arrayInitExpr = .{
                    .lbraceToken = il.tok,
                    .containerQt = arrInitTy,
                    .items = p.listBuffer.items[listBuffTop..],
                },
            });
        },

        .@"struct" => |structTy| {
            assert(structTy.layout != null);

            if (il.node.unpack()) |some| return some;

            const listBuffTop = p.listBuffer.items.len;
            defer p.listBuffer.items.len = listBuffTop;

            var initIndex: usize = 0;
            for (structTy.fields, 0..) |field, i| {
                if (initIndex < il.list.items.len and il.list.items[initIndex].index == i) {
                    const item = try p.convertInitList(il.list.items[initIndex].list, field.qt);
                    try p.listBuffer.append(item);
                    initIndex += 1;
                } else {
                    const item = try p.addNode(.{ .defaultInitExpr = .{ .lastToken = il.tok, .qt = field.qt } });
                    try p.listBuffer.append(item);
                }
            }

            return try p.addNode(.{
                .structInitExpr = .{
                    .lbraceToken = il.tok,
                    .containerQt = initQt,
                    .items = p.listBuffer.items[listBuffTop..],
                },
            });
        },

        .@"union" => |unionTy| {
            if (il.node.unpack()) |some| return some;

            const initNode, const index = if (unionTy.fields.len == 0)
                // do nothing for empty unions
                .{ null, 0 }
            else if (il.list.items.len == 0)
                .{ try p.addNode(.{ .defaultInitExpr = .{ .lastToken = p.tokenIdx - 1, .qt = initQt } }), 0 }
            else blk: {
                const init = il.list.items[0];
                const index: u32 = @truncate(init.index);
                const fieldQt = unionTy.fields[index].qt;

                break :blk .{ try p.convertInitList(init.list, fieldQt), index };
            };

            return try p.addNode(.{
                .unionInitExpr = .{
                    .fieldIndex = index,
                    .initializer = initNode,
                    .lbraceToken = il.tok,
                    .unionQt = initQt,
                },
            });
        },

        // initializer target is invalid, reported earily.
        // initializer target is invalid, reported earlier
        else => return try p.addNode(.{ .defaultInitExpr = .{
            .lastToken = p.tokenIdx,
            .qt = initQt,
        } }),
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

            const labelAddrNode = try p.addNode(.{
                .addrOfLabel = .{
                    .labelToken = label,
                    .qt = .voidPointer,
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
            .qt = .void,
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
                .qt = try Attribute.applyStatementAttributes(p, exprStart, attrBufferTop),
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

    if (cond.qt.scalarKind(p.comp) == .None)
        try p.errStr(.statement_scalar, lp + 1, try p.typeStr(cond.qt));

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

        if (cond.qt.scalarKind(p.comp) == .None)
            try p.errStr(.statement_scalar, lp + 1, try p.typeStr(cond.qt));

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
    try cond.usualUnaryConversion(p, condToken);

    if (cond.qt.scalarKind(p.comp) == .None)
        try p.errStr(.statement_scalar, lp + 1, try p.typeStr(cond.qt));

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

    if (cond.qt.scalarKind(p.comp) == .None)
        try p.errStr(.statement_scalar, lp + 1, try p.typeStr(cond.qt));

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

        const scalarKind = gotoExpr.qt.scalarKind(p.comp);
        if (!scalarKind.isPointer()) {
            const resultQt: QualType = .{ .@"const" = true, ._index = .VoidPointer };

            if (!scalarKind.isInt()) {
                try p.errStr(.incompatible_arg, exprToken, try p.typePairStrExtra(gotoExpr.qt, " to parameter of incompatible type ", resultQt));
                return error.ParsingFailed;
            }

            if (gotoExpr.value.isZero(p.comp)) {
                try gotoExpr.nullToPointer(p, resultQt, exprToken);
            } else {
                try p.errStr(.implicit_int_to_ptr, exprToken, try p.typePairStrExtra(gotoExpr.qt, " to ", resultQt));
                try gotoExpr.castToPointer(p, resultQt, exprToken);
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

    if (!cond.qt.isInt(p.comp))
        try p.errStr(.statement_int, lp + 1, try p.typeStr(cond.qt));

    try cond.saveValue(p);
    try p.expectClosing(lp, .RParen);

    const oldSwitch = p.@"switch";
    var @"switch" = Switch{
        .ranges = std.ArrayList(Switch.Range).init(p.gpa),
        .qt = cond.qt,
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
        if (some.qt.sizeofOrNull(p.comp) == null) // error already reported for incomplete size
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
        return p.addNode(.{ .nullStmt = .{ .semicolonOrRbraceToken = p.tokenIdx, .qt = .void } });
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
                .qt = try Attribute.applyLabelAttributes(p, attrBufferTop),
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
    lastExprType: QualType = .void,
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
                .lastExprType = s.qt(&p.tree),
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

        const retQt: QualType = if (p.func.qt.?.get(p.comp, .func)) |funcTy| funcTy.returnType else .invalid;
        if (lastNoreturn != .yes and !retQt.isInvalid()) {
            var returnZero = false;
            if (lastNoreturn == .no) switch (retQt.base(p.comp).type) {
                .void => {},
                .func, .array => {},

                else => {
                    const funcName = p.getTokenText(p.func.name);
                    const internerName = try p.comp.internString(funcName);

                    if (internerName == p.stringsIds.mainId) {
                        if (retQt.get(p.comp, .int)) |intTy| {
                            if (intTy == .Int) returnZero = true;
                        }
                    }

                    if (!returnZero)
                        try p.errStr(.func_does_not_return, p.tokenIdx - 1, funcName);
                },
            };

            const implicitRet = try p.addNode(.{
                .returnStmt = .{
                    .returnToken = rbrace,
                    .returnQt = retQt,
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

    const funtQt = p.func.qt.?; // `return` cannot be parsed outside of a function.
    const returnQt: QualType = if (funtQt.get(p.comp, .func)) |funcTy| funcTy.returnType else .invalid;
    const retQtIsVoid = !returnQt.isInvalid() and returnQt.is(p.comp, .void);

    if (funtQt.hasAttribute(p.comp, .noreturn))
        try p.errStr(.invalid_noreturn, eToken, p.getTokenText(p.func.name));

    if (retExpr) |*some| {
        if (retQtIsVoid) {
            try p.errStr(.void_func_returns_value, eToken, p.getTokenText(p.func.name));
        } else {
            try some.lvalConversion(p, eToken);
            try some.coerce(p, returnQt, eToken, .ret);
            try some.saveValue(p);
        }
    } else if (!retQtIsVoid) {
        try p.errStr(.func_should_return, retToken, p.getTokenText(p.func.name));
    }

    return try p.addNode(.{
        .returnStmt = .{
            .returnToken = retToken,
            .returnQt = returnQt,
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
        lhs.qt = rhs.qt;
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
            if (rhs.value.isZero(p.comp) and lhs.qt.isInt(p.comp) and rhs.qt.isInt(p.comp)) {
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
            if (lhs.qt.isPointer(p.comp) and rhs.qt.isInt(p.comp)) {
                try rhs.castToPointer(p, lhs.qt, token);
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

    try rhs.coerce(p, lhs.qt, token, .assign);
    try lhs.bin(p, tag, rhs, token);
    return lhs;
}

/// integer-const-expression : const-expression
fn parseIntegerConstExpr(p: *Parser, declFolding: ConstDeclFoldingMode) Error!Result {
    const start = p.tokenIdx;
    const res = try p.constExpr(declFolding);
    if (!res.qt.isInt(p.comp) and !res.qt.isInvalid()) {
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
    if (res.qt.isInvalid() or res.value.isNone())
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

    if (cond.qt.scalarKind(p.comp) == .None) {
        try p.errStr(.cond_expr_type, condToken, try p.typeStr(cond.qt));
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
                .qt = cond.qt,
            },
        });
        _ = try condThen.adjustTypes(colon, &thenExpr, p, .conditional);
        cond.qt = thenExpr.qt;
        cond.node = try p.addNode(.{
            .binaryCondExpr = .{
                .condToken = condToken,
                .cond = cond.node,
                .thenExpr = condThen.node,
                .elseExpr = thenExpr.node,
                .qt = cond.qt,
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

    cond.qt = thenExpr.qt;
    cond.node = try p.addNode(.{
        .condExpr = .{
            .condToken = condToken,
            .cond = cond.node,
            .qt = cond.qt,
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
                if (try lhs.value.shl(lhs.value, rhs.value, lhs.qt, p.comp) and
                    lhs.qt.signedness(p.comp) != .unsigned) try p.errOverflow(tok, lhs);
            } else {
                lhs.value = try lhs.value.shr(rhs.value, lhs.qt, p.comp);
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

        if (try lhs.adjustTypes(tok, &rhs, p, if (tag == .addExpr) .add else .sub)) {
            if (tag == .addExpr) {
                if (try lhs.value.add(lhs.value, rhs.value, lhs.qt, p.comp) and
                    lhs.qt.signedness(p.comp) != .unsigned) try p.errOverflow(tok, lhs);
            } else {
                if (try lhs.value.sub(lhs.value, rhs.value, lhs.qt, p.comp) and
                    lhs.qt.signedness(p.comp) != .unsigned) try p.errOverflow(tok, lhs);
            }
        } else if (!lhs.qt.isInvalid()) {
            const lhsSK = lhs.qt.scalarKind(p.comp);
            if (lhsSK == .Pointer and lhs.qt.childType(p.comp).sizeofOrNull(p.comp) == null) {
                try p.errStr(.ptr_arithmetic_incomplete, tok, try p.typeStr(lhs.qt.childType(p.comp)));
                lhs.qt = .invalid;
            }
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

        if (rhs.value.isZero(p.comp) and tag != .mulExpr and !p.noEval and lhs.qt.isInt(p.comp) and rhs.qt.isInt(p.comp)) {
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
                .mulExpr => if (try lhs.value.mul(lhs.value, rhs.value, lhs.qt, p.comp) and
                    lhs.qt.signedness(p.comp) != .unsigned) try p.errOverflow(tok, lhs),
                .divExpr => if (try lhs.value.div(lhs.value, rhs.value, lhs.qt, p.comp) and
                    lhs.qt.signedness(p.comp) != .unsigned) try p.errOverflow(tok, lhs),

                .modExpr => {
                    var res = try Value.rem(lhs.value, rhs.value, lhs.qt, p.comp);
                    if (res.isNone()) {
                        if (p.inMacro) {
                            // match clang behavior by defining invalid remainder to be zero in macros
                            res = .zero;
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
            if (p.func.qt == null) {
                try p.err(.stmt_expr_not_allowed_file_scope);
                return error.ParsingFailed;
            }

            var stmtExprState: StmtExprState = .{};
            const bodyNode = (try p.parseCompoundStmt(false, &stmtExprState)).?; // compoundStmt only returns null if .l_brace isn't the first token
            p.removeUnusedWarningForTok(stmtExprState.lastExprToken);

            var res = Result{
                .node = bodyNode,
                .qt = stmtExprState.lastExprType,
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

    const compatible = lhs.eql(rhs, p.comp);
    const res: Result = .{
        .value = Value.fromBool(compatible),
        .qt = .int,
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
        cond.qt = thenExpr.qt;
    } else {
        cond.value = elseExpr.value;
        cond.qt = elseExpr.qt;
    }
    cond.node = try p.addNode(.{
        .builtinChooseExpr = .{
            .condToken = condToken,
            .qt = cond.qt,
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

    if (!vaList.qt.eql(p.comp.typeStore.vaList, p.comp)) {
        try p.errStr(.incompatible_va_arg, vaListToken, try p.typeStr(vaList.qt));
        return error.ParsingFailed;
    }

    return .{
        .qt = ty,
        .node = try p.addNode(.{
            .builtinCallExpr = .{
                .builtinToken = builtinToken,
                .qt = ty,
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

    const operandQt = (try p.parseTypeName()) orelse {
        try p.err(.expected_type);
        p.skipTo(.RParen);
        return error.ParsingFailed;
    };

    const recordTy = operandQt.getRecord(p.comp) orelse {
        try p.errStr(.offsetof_ty, tyToken, try p.typeStr(operandQt));
        p.skipTo(.RParen);
        return error.ParsingFailed;
    };

    if (recordTy.layout == null) {
        try p.errStr(.offsetof_incomplete, tyToken, try p.typeStr(operandQt));
        p.skipTo(.RParen);
        return error.ParsingFailed;
    }

    _ = try p.expectToken(.Comma);

    const offsetofExpr = try p.offsetofMemberDesignator(recordTy, operandQt, wantsBits, builtinToken);

    try p.expectClosing(lparen, .RParen);

    return .{
        .qt = p.comp.typeStore.size,
        .value = offsetofExpr.value,
        .node = try p.addNode(.{
            .builtinCallExpr = .{
                .builtinToken = builtinToken,
                .qt = p.comp.typeStore.size,
                .args = &.{offsetofExpr.node},
            },
        }),
    };
}

/// offsetofMemberDesignator: Identifier ('.' Identifier | '[' expr ']' )*
fn offsetofMemberDesignator(
    p: *Parser,
    baseRecordTy: Type.Record,
    baseQt: QualType,
    wantBits: bool,
    accessToken: TokenIndex,
) Error!Result {
    errdefer p.skipTo(.RParen);
    const baseFieldNameToken = try p.expectIdentifier();
    const baseFieldName = try p.getInternString(baseFieldNameToken);

    try p.validateFieldAccess(baseRecordTy, baseQt, baseFieldNameToken, baseFieldName);

    const baseNode = try p.addNode(.{
        .defaultInitExpr = .{
            .lastToken = p.tokenIdx,
            .qt = baseQt,
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

            const lhsRecordTy = lhs.qt.getRecord(p.comp) orelse {
                try p.errStr(.offsetof_ty, fieldNameToken, try p.typeStr(lhs.qt));
                return error.ParsingFailed;
            };

            try p.validateFieldAccess(lhsRecordTy, lhs.qt, fieldNameToken, fieldName);
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

            if (!lhs.qt.is(p.comp, .array)) {
                try p.errStr(.offsetof_array, lbracket, try p.typeStr(lhs.qt));
                return error.ParsingFailed;
            }

            var ptr = lhs;
            try ptr.lvalConversion(p, lbracket);
            try index.lvalConversion(p, lbracket);

            if (!index.qt.isInt(p.comp))
                try p.errToken(.invalid_index, lbracket);
            try p.checkArrayBounds(index, lhs, lbracket);

            try index.saveValue(p);
            try ptr.bin(p, .arrayAccessExpr, index, lbracket);
            lhs = ptr;
        },
        else => break,
    };

    const val = try Value.int(if (wantBits) totalOffset else totalOffset / 8, p.comp);
    return .{ .qt = baseQt, .value = val, .node = lhs.node };
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

            if (operand.qt.isInvalid()) {
                if (!p.tree.isLValue(operand.node))
                    try p.errToken(.addr_of_rvalue, token);

                operand.qt = try p.comp.typeStore.put(p.gpa, .{
                    .pointer = .{ .child = operand.qt, .decayed = null },
                });
            }

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

            return .{
                .node = try p.addNode(.{ .addrOfLabel = .{ .labelToken = nameToken, .qt = .voidPointer } }),
                .qt = .voidPointer,
            };
        },

        .Asterisk => {
            p.tokenIdx += 1;

            var operand = try p.expect(parseCastExpr);
            switch (operand.qt.base(p.comp).type) {
                .array, .func, .pointer => {
                    try operand.lvalConversion(p, token);
                    operand.qt = operand.qt.childType(p.comp);
                },
                else => {
                    try p.errToken(.indirection_ptr, token);
                },
            }

            if (operand.qt.sizeofOrNull(p.comp) == null and !operand.qt.is(p.comp, .void))
                try p.errStr(.deref_incomplete_ty_ptr, token, try p.typeStr(operand.qt));

            operand.qt = operand.qt.unqualified();
            try operand.un(p, .derefExpr, token);
            return operand;
        },

        .Plus => {
            p.tokenIdx += 1;

            var operand = try p.expect(parseCastExpr);
            try operand.lvalConversion(p, token);

            if (!operand.qt.isInt(p.comp) and !operand.qt.isFloat(p.comp))
                try p.errStr(.invalid_argument_un, token, try p.typeStr(operand.qt));

            try operand.usualUnaryConversion(p, token);

            return operand;
        },

        .Minus => {
            p.tokenIdx += 1;

            var operand = try p.expect(parseCastExpr);
            try operand.lvalConversion(p, token);

            const sk = operand.qt.scalarKind(p.comp);
            if (!sk.isArithmetic())
                try p.errStr(.invalid_argument_un, token, try p.typeStr(operand.qt));

            try operand.usualUnaryConversion(p, token);
            if (operand.value.isArithmetic(p.comp))
                _ = try operand.value.sub(Value.zero, operand.value, operand.qt, p.comp)
            else
                operand.value = .{};

            try operand.un(p, .negateExpr, token);
            return operand;
        },

        .PlusPlus => {
            p.tokenIdx += 1;

            var operand = try p.expect(parseCastExpr);

            const sk = operand.qt.scalarKind(p.comp);
            if (sk == .None)
                try p.errStr(.invalid_argument_un, token, try p.typeStr(operand.qt));
            if (!sk.isReal())
                try p.errStr(.complex_prefix_postfix_op, p.tokenIdx, try p.typeStr(operand.qt));

            if (!p.tree.isLValue(operand.node) or operand.qt.@"const") {
                try p.errToken(.not_assignable, token);
                return error.ParsingFailed;
            }

            try operand.usualUnaryConversion(p, token);

            if (operand.value.isNumeric(p.comp)) {
                if (try operand.value.add(operand.value, .one, operand.qt, p.comp))
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
            const sk = operand.qt.scalarKind(p.comp);
            if (sk == .None)
                try p.errStr(.invalid_argument_un, token, try p.typeStr(operand.qt));
            if (!sk.isReal())
                try p.errStr(.complex_prefix_postfix_op, p.tokenIdx, try p.typeStr(operand.qt));

            if (!p.tree.isLValue(operand.node) or operand.qt.@"const") {
                try p.errToken(.not_assignable, token);
                return error.ParsingFailed;
            }

            try operand.usualUnaryConversion(p, token);

            if (operand.value.isNumeric(p.comp)) {
                if (try operand.value.sub(operand.value, .one, operand.qt, p.comp))
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

            const sk = operand.qt.scalarKind(p.comp);
            if (sk.isInt()) {
                if (operand.value.is(.int, p.comp)) {
                    operand.value = try operand.value.bitNot(operand.qt, p.comp);
                }
            } else if (!sk.isReal()) {
                try p.errStr(.complex_conj, token, try p.typeStr(operand.qt));
                if (operand.value.is(.complex, p.comp))
                    operand.value = try operand.value.complexConj(operand.qt, p.comp);
            } else {
                try p.errStr(.invalid_argument_un, token, try p.typeStr(operand.qt));
                operand.value = .{};
            }

            try operand.un(p, .bitNotExpr, token);
            return operand;
        },

        .Bang => {
            p.tokenIdx += 1;

            var operand = try p.expect(parseCastExpr);
            try operand.lvalConversion(p, token);

            if (operand.qt.scalarKind(p.comp) != .None)
                try p.errStr(.invalid_argument_un, token, try p.typeStr(operand.qt));

            try operand.usualUnaryConversion(p, token);

            if (operand.value.is(.int, p.comp)) {
                const res = Value.fromBool(!operand.value.toBool(p.comp));
                operand.value = res;
            } else if (operand.value.isNull()) {
                operand.value = .one;
            } else {
                operand.value = .{};
                if (operand.qt.get(p.comp, .pointer)) |ptrTy| {
                    if (ptrTy.decayed != null) operand.value = .zero;
                }
            }

            operand.qt = .int;
            try operand.un(p, .boolNotExpr, token);
            return operand;
        },

        .KeywordSizeof => {
            p.tokenIdx += 1;
            const expectedParen = p.tokenIdx;

            var hasExpr = false;
            var res: Result = .{ .node = undefined }; // check has expr

            if (try p.parseTypeName()) |qt| {
                res.qt = qt;
                try p.errToken(.expected_parens_around_typename, expectedParen);
            } else if (p.eat(.LParen)) |lp| {
                if (try p.parseTypeName()) |qt| {
                    res.qt = qt;
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

            const operandQt = res.qt;

            if (res.qt.isInvalid()) {
                res.value = .{};
            } else {
                const baseType = res.qt.base(p.comp);
                switch (baseType.type) {
                    .void => try p.errStr(.pointer_arith_void, token, "sizeof"),
                    .pointer => |ptrTy| if (ptrTy.decayed) |decayedQt| {
                        const errString = try p.typePairStrExtra(res.qt, " instead of ", decayedQt);
                        try p.errStr(.sizeof_array_arg, token, errString);
                    },
                    else => {},
                }

                if (baseType.qt.sizeofOrNull(p.comp)) |size| {
                    if (size == 0) try p.errToken(.sizeof_returns_zero, token);
                    res.value = try Value.int(size, p.comp);
                    res.qt = p.comp.typeStore.size;
                } else {
                    res.value = .{};
                    try p.errStr(.invalid_sizeof, expectedParen - 1, try p.typeStr(res.qt));
                    res.qt = .invalid;
                }
            }

            res.node = try p.addNode(.{
                .sizeofExpr = .{
                    .opToken = token,
                    .qt = res.qt,
                    .expr = if (hasExpr) res.node else null,
                    .operandQt = operandQt,
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

            if (try p.parseTypeName()) |qt| {
                res.qt = qt;
                try p.errToken(.expected_parens_around_typename, expectedParen);
            } else if (p.eat(.LParen)) |lp| {
                if (try p.parseTypeName()) |qt| {
                    res.qt = qt;
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

            const operandQt = res.qt;

            if (res.qt.is(p.comp, .void))
                try p.errStr(.pointer_arith_void, token, "alignof");

            if (res.qt.sizeofOrNull(p.comp) != null) {
                res.value = try Value.int(res.qt.alignof(p.comp), p.comp);
                res.qt = p.comp.typeStore.size;
            } else if (!res.qt.isInvalid()) {
                try p.errStr(.invalid_alignof, expectedParen, try p.typeStr(res.qt));
                return error.ParsingFailed;
            }

            res.node = try p.addNode(.{
                .alignofExpr = .{
                    .opToken = token,
                    .qt = res.qt,
                    .expr = if (hasExpr) res.node else null,
                    .operandQt = operandQt,
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

            const sk = operand.qt.scalarKind(p.comp);
            if (!sk.isArithmetic()) {
                try p.errStr(.invalid_imag, imagToken, try p.typeStr(operand.qt));
            }
            if (!sk.isReal()) {
                operand.value = try operand.value.imaginaryPart(p.comp);
            } else switch (p.comp.langOpts.emulate) {
                .msvc => {},
                .gcc => operand.value = Value.zero,
                .clang => {
                    if (operand.value.is(.int, p.comp) or operand.value.is(.float, p.comp))
                        operand.value = Value.zero;
                },
            }

            // convert _Complex F to F
            operand.qt = operand.qt.toReal(p.comp);
            try operand.un(p, .imagExpr, token);
            return operand;
        },

        .KeywordReal1, .KeywordReal2 => {
            const realToken = p.tokenIdx;
            p.tokenIdx += 1;

            var operand = try p.expect(parseCastExpr);
            try operand.lvalConversion(p, token);
            if (!operand.qt.isInt(p.comp) and !operand.qt.isFloat(p.comp)) {
                try p.errStr(.invalid_real, realToken, try p.typeStr(operand.qt));
            }

            operand.qt = operand.qt.toReal(p.comp);
            operand.value = try operand.value.realPart(p.comp);

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

    var d: DeclSpec = .{ .qt = .invalid };
    const any = if (p.comp.langOpts.standard.atLeast(.c23))
        try p.parseStorageClassSpec(&d)
    else
        false;

    switch (d.storageClass) {
        .auto, .@"extern", .typedef => |tok| {
            try p.errStr(.invalid_compound_literal_storage_class, tok, @tagName(d.storageClass));
            d.storageClass = .none;
        },
        .register => if (p.func.qt == null) try p.err(.illegal_storage_on_global),
        else => {},
    }

    var qt = (try p.parseTypeName()) orelse {
        p.tokenIdx = lparen;
        if (any) {
            try p.err(.expected_type);
            return error.ParsingFailed;
        }
        return null;
    };
    try p.expectClosing(lparen, .RParen);

    switch (qt.base(p.comp).type) {
        .func => try p.err(.func_init),
        .array => |arrayTy| if (arrayTy.len == .variable) try p.err(.vla_init),
        else => if (qt.sizeofOrNull(p.comp) == null) {
            try p.errStr(.variable_incomplete_ty, p.tokenIdx, try p.typeStr(qt));
            return error.ParsingFailed;
        },
    }

    const initContext = p.initContext;
    defer p.initContext = initContext;

    p.initContext = d.initContext(p);

    var initlistExpr = try p.initializer(qt);
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
            .qt = initlistExpr.qt,
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
        .LParen => return try p.parseCallExpr(lhs),

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

            if (ptr.qt.get(p.comp, .pointer)) |ptrTy| {
                ptr.qt = ptrTy.child;
                if (!index.qt.isInt(p.comp)) try p.errToken(.invalid_index, lb);
                try p.checkArrayBounds(indexBeforeConversion, arrayBeforeConversion, lb);
            } else if (index.qt.get(p.comp, .pointer)) |ptrTy| {
                index.qt = ptrTy.child;
                if (!ptr.qt.isInt(p.comp)) try p.errToken(.invalid_index, lb);
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
                    .qt = ptr.qt,
                },
            });

            return ptr;
        },

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
            if (lhs.qt.is(p.comp, .array)) {
                var copy = lhs;
                copy.qt = try copy.qt.decay(p.comp);
                try copy.implicitCast(p, .ArrayToPointer, arrow);
                return try p.fieldAccess(copy, name, true, arrow);
            }
            return try p.fieldAccess(lhs, name, true, arrow);
        },

        .PlusPlus => {
            defer p.tokenIdx += 1;
            var operand = lhs;

            const scalarKind = operand.qt.scalarKind(p.comp);
            if (scalarKind == .None)
                try p.errStr(.invalid_argument_un, p.tokenIdx, try p.typeStr(operand.qt));

            if (!scalarKind.isReal())
                try p.errStr(.complex_prefix_postfix_op, p.tokenIdx, try p.typeStr(operand.qt));

            if (!p.tree.isLValue(operand.node) or operand.qt.@"const") {
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

            const scalarKind = operand.qt.scalarKind(p.comp);
            if (scalarKind == .None)
                try p.errStr(.invalid_argument_un, p.tokenIdx, try p.typeStr(operand.qt));

            if (!scalarKind.isReal())
                try p.errStr(.complex_prefix_postfix_op, p.tokenIdx, try p.typeStr(operand.qt));

            if (!p.tree.isLValue(operand.node) or operand.qt.@"const") {
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
    const exprQt = lhs.qt;
    const isPtr = exprQt.isPointer(p.comp);
    const exprBaseQt = if (isPtr) exprQt.childType(p.comp) else exprQt;
    const recordType = exprBaseQt.getRecord(p.comp) orelse {
        try p.errStr(.expected_record_ty, fieldNameToken, try p.typeStr(exprQt));
        return error.ParsingFailed;
    };

    if (recordType.layout == null) {
        try p.errStr(.deref_incomplete_ty_ptr, fieldNameToken - 2, try p.typeStr(exprBaseQt));
        return error.ParsingFailed;
    }

    if (isArrow and !isPtr) try p.errStr(.member_expr_not_ptr, fieldNameToken, try p.typeStr(exprQt));
    if (!isArrow and isPtr) try p.errStr(.member_expr_ptr, fieldNameToken, try p.typeStr(exprQt));

    const fieldName = try p.getInternString(fieldNameToken);
    try p.validateFieldAccess(recordType, exprQt, fieldNameToken, fieldName);
    var discard: u64 = 0;
    return p.fieldAccessExtra(lhs.node, recordType, fieldName, isArrow, accessToken, &discard);
}

fn validateFieldAccess(
    p: *Parser,
    recordType: Type.Record,
    exprType: QualType,
    fieldNameToken: TokenIndex,
    fieldName: StringId,
) Error!void {
    if (recordType.hasField(p.comp, fieldName)) return;

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
    recordType: Type.Record,
    targetName: StringId,
    isArrow: bool, // is arrow operator
    accessToken: TokenIndex,
    offsetBits: *u64,
) Error!Result {
    for (recordType.fields, 0..) |field, fieldIndex| {
        if (field.name == .empty) {
            if (field.qt.getRecord(p.comp)) |fieldRecordTy| {
                if (!fieldRecordTy.hasField(p.comp, targetName)) continue;

                const access: Node.MemberAccess = .{
                    .accessToken = accessToken,
                    .qt = field.qt,
                    .base = base,
                    .memberIndex = @intCast(fieldIndex),
                };
                const inner = try p.addNode(if (isArrow)
                    .{ .memberAccessPtrExpr = access }
                else
                    .{ .memberAccessExpr = access });

                const ret = p.fieldAccessExtra(inner, fieldRecordTy, targetName, false, accessToken, offsetBits);
                offsetBits.* = field.layout.offsetBits;
                return ret;
            }
        }

        if (targetName == field.name) {
            offsetBits.* = field.layout.offsetBits;
            const access: Node.MemberAccess = .{
                .accessToken = accessToken,
                .qt = field.qt,
                .base = base,
                .memberIndex = @intCast(fieldIndex),
            };

            return .{ .qt = field.qt, .node = try p.addNode(if (isArrow)
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

    var funcQt = p.func.qt orelse {
        try p.errToken(.va_start_not_in_func, builtinToken);
        return;
    };

    const funcTy = funcQt.get(p.comp, .func) orelse return;
    if (funcTy.kind == .Variadic or funcTy.params.len == 0) {
        return p.errToken(.va_start_fixed_args, builtinToken);
    }

    const lastParamName = funcTy.params[funcTy.params.len - 1].name;
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
    if (idx <= 1 and !arg.qt.isFloat(p.comp)) {
        try p.errStr(.not_floating_type, paramTok, try p.typeStr(arg.qt));
    } else if (idx == 1) {
        const prevIdx = p.listBuffer.items[p.listBuffer.items.len - 1];
        const prevQt = prevIdx.qt(&p.tree);
        if (!prevQt.eql(arg.qt, p.comp)) {
            try p.errStr(.argument_types_differ, paramTok, try p.typePairStrExtra(prevQt, " vs ", arg.qt));
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

                .__builtin_complex,
                .__builtin_isinf,
                .__builtin_isinf_sign,
                .__builtin_isnan,
                => false,
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
                .__builtin_isinf,
                .__builtin_isinf_sign,
                .__builtin_isnan,
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

    fn returnType(self: CallExpr, p: *Parser, funcQt: QualType) QualType {
        if (self == .standard)
            return if (funcQt.get(p.comp, .func)) |funcTy| funcTy.returnType else .invalid;

        const builtin = self.builtin;
        const funcTy = funcQt.get(p.comp, .func).?;
        return switch (builtin.tag) {
            .__builtin_complex => {
                if (p.listBuffer.items.len < 1) return .invalid;
                const lastParam = p.listBuffer.items[p.listBuffer.items.len - 1];
                return try lastParam.qt(&p.tree).toComplex(p.comp);
            },

            .__c11_atomic_load => {
                if (p.listBuffer.items.len != 3) return .invalid;
                const firstParam = p.listBuffer.items[1];
                const qt = firstParam.qt(&p.tree);
                if (!qt.scalarKind(p.comp).isPointer()) return .invalid;
                return qt.childType(p.comp);
            },

            .__c11_atomic_exchange => {
                if (p.listBuffer.items.len != 4) return .invalid;
                const secondParam = p.listBuffer.items[2];
                return secondParam.qt(&p.tree);
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
                if (p.listBuffer.items.len != 3) return .invalid;
                const secondParam = p.listBuffer.items[2];
                return secondParam.qt(&p.tree);
            },

            .__c11_atomic_compare_exchange_strong,
            .__c11_atomic_compare_exchange_weak,
            => {
                if (p.listBuffer.items.len != 6) return .invalid;
                const thirdParam = p.listBuffer.items[3];
                return thirdParam.qt(&p.tree);
            },

            .__atomic_compare_exchange,
            .__atomic_compare_exchange_n,
            .__c11_atomic_is_lock_free,
            => .bool,

            else => funcTy.returnType,
        };
    }

    fn finish(self: CallExpr, p: *Parser, funcQt: QualType, listBufferTop: usize, lparen: TokenIndex) Error!Result {
        const returnQt = self.returnType(p, funcQt);
        const args = p.listBuffer.items[listBufferTop..];
        switch (self) {
            .standard => |funcNode| return .{
                .qt = returnQt,
                .node = try p.addNode(.{
                    .callExpr = .{
                        .lparenToken = lparen,
                        .qt = returnQt.unqualified(),
                        .callee = funcNode,
                        .args = args,
                    },
                }),
            },
            .builtin => |builtin| return .{
                .value = try evalBuiltin(builtin.tag, p, args),
                .qt = returnQt,
                .node = try p.addNode(.{
                    .builtinCallExpr = .{
                        .builtinToken = builtin.builtinToken,
                        .qt = returnQt,
                        .args = args,
                    },
                }),
            },
        }
    }
};

fn parseCallExpr(p: *Parser, lhs: Result) Error!Result {
    const lParen = p.tokenIdx;
    p.tokenIdx += 1;

    // We cannot refer to the function type here because the pointer to
    // type_store.extra might get invalidated while parsing args.
    const funcQt, const paramsLen, const funcKind = blk: {
        var baseQt = lhs.qt;
        if (baseQt.get(p.comp, .pointer)) |pointerTy| baseQt = pointerTy.child;
        if (baseQt.isInvalid()) break :blk .{ baseQt, std.math.maxInt(usize), undefined };

        const funcTyQt = baseQt.base(p.comp);
        if (funcTyQt.type != .func) {
            try p.errStr(.not_callable, lParen, try p.typeStr(lhs.qt));
            return error.ParsingFailed;
        }
        break :blk .{ funcTyQt.qt, funcTyQt.type.func.params.len, funcTyQt.type.func.kind };
    };

    var func = lhs;
    try func.lvalConversion(p, lParen);

    const listBufferTop = p.listBuffer.items.len;
    defer p.listBuffer.items.len = listBufferTop;

    var argCount: u32 = 0;
    var firstAfter = lParen;

    const callExpr = CallExpr.init(p, lhs.node, func.node);

    while (p.eat(.RParen) == null) {
        const paramToken = p.tokenIdx;
        if (argCount == paramsLen)
            firstAfter = p.tokenIdx;

        var arg = try p.expect(parseAssignExpr);

        if (callExpr.shouldPerformLvalConversion(argCount))
            try arg.lvalConversion(p, paramToken);

        if (arg.qt.sizeofOrNull(p.comp) == null and !arg.qt.is(p.comp, .void))
            return error.ParsingFailed;

        if (argCount >= paramsLen) {
            if (callExpr.shouldPromoteVarArg(argCount)) switch (arg.qt.base(p.comp).type) {
                .int => try arg.castToInt(p, arg.qt.promoteInt(p.comp), paramToken),
                .float => try arg.castToFloat(p, .double, paramToken),
                else => {},
            };

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

        if (funcQt.get(p.comp, .func)) |funcTy| {
            const param = funcTy.params[argCount];
            if (callExpr.shouldCoerceArg(argCount)) {
                try arg.coerce(p, param.qt, paramToken, .{ .arg = param.nameToken });
            }
        }

        try arg.saveValue(p);
        try p.listBuffer.append(arg.node);
        argCount += 1;
        _ = p.eat(.Comma) orelse {
            try p.expectClosing(lParen, .RParen);
            break;
        };
    }

    if (funcQt.isInvalid()) {
        // Skip argument count checks.
        return try callExpr.finish(p, funcQt, listBufferTop, lParen);
    }

    const actual: u32 = @intCast(argCount);
    const extra = Diagnostics.Message.Extra{
        .arguments = .{
            .expected = @intCast(paramsLen),
            .actual = actual,
        },
    };

    if (callExpr.paramCountOverride()) |expected| {
        if (expected != actual)
            try p.errExtra(.expected_arguments, firstAfter, .{ .arguments = .{ .actual = actual, .expected = expected } });
    } else switch (funcKind) {
        .Normal => if (paramsLen != argCount) {
            try p.errExtra(.expected_arguments, firstAfter, extra);
        },
        .Variadic => if (argCount < paramsLen) {
            try p.errExtra(.expected_at_least_arguments, firstAfter, extra);
        },
        .OldStyle => if (paramsLen != argCount) {
            if (paramsLen == 0)
                try p.errToken(.passing_args_to_kr, firstAfter)
            else
                try p.errExtra(.expected_arguments_old, firstAfter, extra);
        },
    }

    return try callExpr.finish(p, funcQt, listBufferTop, lParen);
}

fn checkArrayBounds(p: *Parser, index: Result, array: Result, token: TokenIndex) !void {
    if (index.value.isNone()) return;

    const arrayLen = array.qt.arrayLen(p.comp) orelse return;
    if (arrayLen == 0) return;

    if (arrayLen == 1) {
        if (p.getNode(array.node, .memberAccessExpr) orelse p.getNode(array.node, .memberAccessPtrExpr)) |access| {
            var baseTy = access.base.qt(&p.tree);

            if (baseTy.get(p.comp, .pointer)) |ptrTy|
                baseTy = ptrTy.child;

            if (baseTy.getRecord(p.comp)) |recordTy| {
                if (access.memberIndex + 1 == recordTy.fields.len) {
                    if (!index.value.isZero(p.comp)) {
                        try p.errStr(.old_style_flexible_struct, token, try index.str(p));
                    }
                    return;
                }
            }
        }
    }

    const indexInt = index.value.toInt(u64, p.comp) orelse std.math.maxInt(u64);
    if (index.qt.isUnsignedInt(p.comp)) {
        if (indexInt >= arrayLen) {
            try p.errStr(.array_after, token, try index.str(p));
        }
    } else {
        if (index.value.compare(.lt, .zero, p.comp)) {
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
                try p.checkDeprecatedUnavailable(sym.qt, nameToken, sym.token);
                if (sym.kind == .constexpr) {
                    return .{
                        .value = sym.value,
                        .qt = sym.qt,
                        .node = try p.addNode(.{
                            .declRefExpr = .{
                                .nameToken = nameToken,
                                .qt = sym.qt,
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
                    .qt = sym.qt,
                    .decl = sym.node.unpack().?,
                };
                const node = try p.addNode(if (sym.kind == .enumeration)
                    .{ .enumerationRef = dr }
                else
                    .{ .declRefExpr = dr });

                return Result{
                    .value = if (p.constDeclFolding == .NoConstDeclFolding and sym.kind != .enumeration) Value{} else sym.value,
                    .qt = sym.qt,
                    .node = node,
                };
            }

            // check if this is a builtin call
            if (try p.comp.builtins.getOrCreate(p.comp, name)) |some| {
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
                    .qt = some.qt,
                    .node = try p.addNode(.{
                        .builtinRef = .{
                            .nameToken = nameToken,
                            .qt = some.qt,
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

                const funcQt = try p.comp.typeStore.put(p.gpa, .{ .func = .{
                    .returnType = .int,
                    .kind = .OldStyle,
                    .params = &.{},
                } });
                const node = try p.addNode(.{
                    .fnProto = .{
                        .nameToken = nameToken,
                        .qt = funcQt,
                        .static = false,
                        .@"inline" = false,
                        .definition = null,
                    },
                });

                try p.declBuffer.append(node);
                try p.symStack.declareSymbol(internedName, funcQt, nameToken, node);

                return .{
                    .qt = funcQt,
                    .node = try p.addNode(.{
                        .declRefExpr = .{
                            .nameToken = nameToken,
                            .qt = funcQt,
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
                .value = .fromBool(id == .KeywordTrue),
                .qt = .bool,
                .node = try p.addNode(.{
                    .boolLiteral = .{
                        .qt = .bool,
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
                .value = .null,
                .qt = .nullptrTy,
                .node = try p.addNode(.{
                    .nullptrLiteral = .{
                        .qt = .nullptrTy,
                        .literalToken = p.tokenIdx,
                    },
                }),
            };
        },

        .MacroFunc, .MacroFunction => {
            defer p.tokenIdx += 1;
            var qt: QualType = undefined;
            var tok = p.tokenIdx;

            if (p.func.ident) |some| {
                qt = some.qt;
                tok = some.node.get(&p.tree).variable.nameToken;
            } else if (p.func.qt) |_| {
                const stringsTop = p.strings.items.len;
                defer p.strings.items.len = stringsTop;

                try p.strings.appendSlice(p.getTokenText(p.func.name));
                try p.strings.append(0);

                const predef = try p.makePredefinedIdentifier(stringsTop);
                qt = predef.qt;
                p.func.ident = predef;
            } else {
                const stringsTop = p.strings.items.len;
                defer p.strings.items.len = stringsTop;

                try p.strings.append(0);

                const predef = try p.makePredefinedIdentifier(stringsTop);
                qt = predef.qt;
                p.func.ident = predef;
                try p.declBuffer.append(predef.node);
            }

            if (p.func.qt == null)
                try p.err(.predefined_top_level);

            return .{
                .qt = qt,
                .node = try p.addNode(.{
                    .declRefExpr = .{
                        .nameToken = tok,
                        .qt = qt,
                        .decl = p.func.ident.?.node,
                    },
                }),
            };
        },

        .MacroPrettyFunc => {
            defer p.tokenIdx += 1;
            var qt: QualType = undefined;
            if (p.func.prettyIdent) |some| {
                qt = some.qt;
            } else if (p.func.qt) |funcQt| {
                const stringsTop = p.strings.items.len;
                defer p.strings.items.len = stringsTop;

                try funcQt.printNamed(p.getTokenText(p.func.name), p.comp, p.strings.writer());
                try p.strings.append(0);

                const predef = try p.makePredefinedIdentifier(stringsTop);
                qt = predef.qt;
                p.func.prettyIdent = predef;
            } else {
                const stringsTop = p.strings.items.len;
                defer p.strings.items.len = stringsTop;

                try p.strings.appendSlice("top level\x00");
                const predef = try p.makePredefinedIdentifier(stringsTop);
                qt = predef.qt;
                p.func.prettyIdent = predef;
                try p.declBuffer.append(predef.node);
            }

            if (p.func.qt == null)
                try p.err(.predefined_top_level);

            return .{
                .qt = qt,
                .node = try p.addNode(.{
                    .declRefExpr = .{
                        .nameToken = p.tokenIdx,
                        .qt = qt,
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
            const intTy: QualType = if (p.inMacro) p.comp.typeStore.intmax else .int;
            const res: Result = .{
                .value = .zero,
                .qt = intTy,
                .node = try p.addNode(.{
                    .intLiteral = .{
                        .qt = intTy,
                        .literalToken = p.tokenIdx,
                    },
                }),
            };
            try res.putValue(p);
            return res;
        },

        .One => {
            p.tokenIdx += 1;
            const intTy: QualType = if (p.inMacro) p.comp.typeStore.intmax else .int;
            const res: Result = .{
                .value = .one,
                .qt = intTy,
                .node = try p.addNode(.{
                    .intLiteral = .{
                        .literalToken = p.tokenIdx,
                        .qt = intTy,
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

            const res: Result = .{
                .value = try Value.int(byte, p.comp),
                .qt = .int,
                .node = try p.addNode(.{ .intLiteral = .{ .qt = .int, .literalToken = p.tokenIdx } }),
            };
            try res.putValue(p);

            return res;
        },

        .KeywordGeneric => return p.parseGenericSelection(),

        else => return null,
    }
}

fn makePredefinedIdentifier(p: *Parser, stringsTop: usize) !Result {
    const arrayQt = try p.comp.typeStore.put(p.gpa, .{ .array = .{
        .elem = .{ .@"const" = true, ._index = .Char },
        .len = .{ .fixed = p.strings.items.len - stringsTop },
    } });

    const slice = p.strings.items[stringsTop..];
    const val = try Value.intern(p.comp, .{ .bytes = slice });

    const strLit = try p.addNode(.{ .stringLiteralExpr = .{ .qt = arrayQt, .literalToken = p.tokenIdx } });
    if (!p.inMacro) try p.tree.valueMap.put(p.gpa, strLit, val);

    return .{
        .qt = arrayQt,
        .node = try p.addNode(.{
            .variable = .{
                .nameToken = p.tokenIdx,
                .qt = arrayQt,
                .storageClass = .static,
                .threadLocal = false,
                .implicit = true,
                .initializer = strLit,
            },
        }),
    };
}

fn parseFloat(p: *Parser, buf: []const u8, suffix: NumberSuffix, tokenIdx: TokenIndex) !Result {
    const qt: QualType = switch (suffix) {
        .None, .I => .double,
        .F, .IF => .float,
        .F16, .IF16 => .float16,
        .L, .IL => .longDouble,
        .W, .IW => p.comp.float80Type().?,
        .Q, .IQ, .F128, .IF128 => .float128,
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
        break :key switch (qt.bitSizeof(p.comp)) {
            16 => .{ .float = .{ .f16 = @floatCast(float) } },
            32 => .{ .float = .{ .f32 = @floatCast(float) } },
            64 => .{ .float = .{ .f64 = @floatCast(float) } },
            80 => .{ .float = .{ .f80 = @floatCast(float) } },
            128 => .{ .float = .{ .f128 = @floatCast(float) } },
            else => unreachable,
        };
    });

    var res = Result{
        .qt = qt,
        .node = try p.addNode(.{ .floatLiteral = .{ .qt = qt, .literalToken = tokenIdx } }),
        .value = val,
    };
    if (suffix.isImaginary()) {
        try p.err(.gnu_imaginary_constant);
        res.qt = try qt.toComplex(p.comp);
        res.value = try Value.intern(p.comp, switch (res.qt.bitSizeof(p.comp)) {
            64 => .{ .complex = .{ .cf32 = .{ 0.0, val.toFloat(f32, p.comp) } } },
            128 => .{ .complex = .{ .cf64 = .{ 0.0, val.toFloat(f64, p.comp) } } },
            160 => .{ .complex = .{ .cf80 = .{ 0.0, val.toFloat(f80, p.comp) } } },
            256 => .{ .complex = .{ .cf128 = .{ 0.0, val.toFloat(f128, p.comp) } } },
            else => unreachable,
        });
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
        res.qt = .ulonglong;
        res.node = try p.addNode(.{ .intLiteral = .{ .qt = res.qt, .literalToken = tokenIdx } });
        try res.putValue(p);
        return res;
    }

    const internedVal = try Value.int(value, p.comp);
    if (suffix.isSignedInteger()) {
        const maxInt = try Value.maxInt(p.comp.typeStore.intmax, p.comp);
        if (internedVal.compare(.gt, maxInt, p.comp)) {
            try p.errToken(.implicitly_unsigned_literal, tokenIdx);
        }
    }

    const qts: []const QualType = if (suffix.signedness() == .unsigned)
        &.{ .uint, .ulong, .ulonglong }
    else if (base == 10)
        &.{ .int, .long, .longlong }
    else
        &.{ .int, .uint, .long, .ulong, .longlong, .ulonglong };

    const suffixQt: QualType = switch (suffix) {
        .None, .I => .int,
        .U, .IU => .uint,
        .UL, .IUL => .ulong,
        .ULL, .IULL => .ulonglong,
        .L, .IL => .long,
        .LL, .ILL => .longlong,
        else => unreachable,
    };

    for (qts) |qt| {
        res.qt = qt;
        if (res.qt.intRankOrder(suffixQt, p.comp).compare(.lt)) continue;
        const maxInt = try Value.maxInt(res.qt, p.comp);
        if (internedVal.compare(.lte, maxInt, p.comp)) break;
    } else {
        res.qt = .ulonglong;
    }

    res.node = try p.addNode(.{ .intLiteral = .{ .qt = res.qt, .literalToken = tokenIdx } });
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
        res.qt = try res.qt.toComplex(p.comp);
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

    const intQt = try p.comp.typeStore.put(p.gpa, .{ .bitInt = .{
        .bits = bitsNeeded,
        .signedness = suffix.signedness(),
    } });
    const res: Result = .{
        .value = try Value.intern(p.comp, .{ .int = .{ .bigInt = c } }),
        .qt = intQt,
        .node = try p.addNode(.{ .intLiteral = .{ .qt = intQt, .literalToken = tokenIdx } }),
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
        const resSK = res.qt.scalarKind(p.comp);
        if (resSK.isFloat() or !resSK.isReal()) {
            try p.errToken(.float_literal_in_pp_expr, p.tokenIdx);
            return error.ParsingFailed;
        }
        res.qt = if (res.qt.isUnsignedInt(p.comp)) try p.comp.typeStore.intmax.makeIntUnsigned(p.comp) else p.comp.typeStore.intmax;
    } else if (!res.value.isNone()) {
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
            .qt = .int,
            .value = .zero,
            .node = try p.addNode(.{ .charLiteral = .{ .qt = .int, .literalToken = p.tokenIdx } }),
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

    const charliteralQt = charKind.charLiteralType(p.comp);
    // This is the type the literal will have if we're in a macro; macros always operate on intmax_t/uintmax_t values
    const macroQt = if (charliteralQt.isUnsignedInt(p.comp) or (charKind == .char and p.comp.getCharSignedness() == .unsigned))
        try p.comp.typeStore.intmax.makeIntUnsigned(p.comp)
    else
        p.comp.typeStore.intmax;

    const res = Result{
        .qt = if (p.inMacro) macroQt else charliteralQt,
        .value = try Value.int(val, p.comp),
        .node = try p.addNode(.{ .charLiteral = .{ .qt = charliteralQt, .literalToken = p.tokenIdx } }),
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
    const arrayQt = try p.comp.typeStore.put(p.gpa, .{ .array = .{
        .elem = stringKind.elementType(p.comp),
        .len = .{ .fixed = @divExact(slice.len, @intFromEnum(charWidth)) },
    } });

    const res: Result = .{
        .qt = arrayQt,
        .value = val,
        .node = try p.addNode(.{
            .stringLiteralExpr = .{
                .literalToken = stringStart,
                .qt = arrayQt,
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

    var controllingQt = controlling.qt;
    if (controllingQt.is(p.comp, .array)) {
        controllingQt = try controllingQt.decay(p.comp);
    }

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
        if (try p.parseTypeName()) |qt| blk: {
            switch (qt.base(p.comp).type) {
                .array => try p.errToken(.generic_array_type, start),
                .func => try p.errToken(.generic_func_type, start),
                else => if (qt.isQualified()) {
                    try p.errToken(.generic_qual_type, start);
                },
            }

            const colon = try p.expectToken(.Colon);
            var res = try p.expect(parseAssignExpr);
            res.node = try p.addNode(.{
                .genericAssociationExpr = .{
                    .colonToken = colon,
                    .associationQt = qt,
                    .expr = res.node,
                },
            });

            try p.listBuffer.append(res.node);
            try p.paramBuffer.append(.{ .name = undefined, .qt = qt, .nameToken = start, .node = undefined });

            if (qt.eql(controllingQt, p.comp)) {
                if (chosenToken == null) {
                    chosen = res;
                    chosenToken = start;
                    break :blk;
                }
            }

            const previousItems = p.paramBuffer.items[0 .. p.paramBuffer.items.len - 1][paramBufferTop..];
            for (previousItems) |prevItem| {
                if (prevItem.qt.eql(qt, p.comp)) {
                    try p.errStr(.generic_duplicate, start, try p.typeStr(qt));
                    try p.errStr(.generic_duplicate_here, prevItem.nameToken, try p.typeStr(qt));
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
            try p.errStr(.generic_no_match, controllingToken, try p.typeStr(controllingQt));
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
        .qt = chosen.qt,
        .value = chosen.value,
        .node = try p.addNode(.{
            .genericExpr = .{
                .genericToken = kwGeneric,
                .controlling = controlling.node,
                .chosen = chosen.node,
                .qt = chosen.qt,
                .rest = p.listBuffer.items[listBufferTop..],
            },
        }),
    };
}
