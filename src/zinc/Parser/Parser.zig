const std = @import("std");
const assert = std.debug.assert;
const big = std.math.big;
const Allocator = std.mem.Allocator;

const AST = @import("../AST/AST.zig");
const Token = AST.Token;
const TokenIndex = AST.TokenIndex;
const NumberPrefix = Token.NumberPrefix;
const NumberSuffix = Token.NumberSuffix;
const Node = AST.Node;
const Attribute = @import("../Lexer/Attribute.zig");
const Builtins = @import("../Builtins.zig");
const Builtin = Builtins.Builtin;
const evalBuiltin = @import("../Builtins/eval.zig").eval;
const CharInfo = @import("../Basic/CharInfo.zig");
const Compilation = @import("../Basic/Compilation.zig");
const DeclSpec = @import("../AST/DeclSpec.zig");
const Diagnostics = @import("../Basic/Diagnostics.zig");
const InitList = @import("InitList.zig");
const Lexer = @import("../Lexer/Lexer.zig");
const Preprocessor = @import("../Lexer/Preprocessor.zig");
const Result = @import("Result.zig");
const RecordLayout = @import("../Basic/RecordLayout.zig");
const SymbolStack = @import("../Sema/SymbolStack.zig");
const Symbol = SymbolStack.Symbol;
const Switch = @import("../Sema/Switch.zig");
const Source = @import("../Basic/Source.zig");
const StringId = @import("../Basic/StringInterner.zig").StringId;
const TargetUtil = @import("../Basic/Target.zig");
const TextLiteral = @import("../Parser/TextLiteral.zig");
const TokenType = @import("../Basic/TokenType.zig").TokenType;
const TypeStore = @import("../AST/TypeStore.zig");
const Type = TypeStore.Type;
const QualType = TypeStore.QualType;
const TypeBuilder = TypeStore.Builder;
const Value = @import("../AST/Value.zig");

pub const Error = Compilation.Error || error{ParsingFailed};

const NodeList = std.ArrayList(Node.Index);

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

const Parser = @This();

// values from pp
pp: *Preprocessor,
comp: *Compilation,
diagnostics: *Diagnostics,
tokenIds: []const TokenType,
tokenIdx: u32 = 0,

/// The AST being constructed.
tree: AST,

// buffers used during compilation
symStack: SymbolStack = .{},
strings: std.array_list.Aligned(u8, .@"4") = .empty,
labels: std.ArrayList(Label) = .empty,
listBuffer: NodeList = .empty,
declBuffer: NodeList = .empty,
/// Function type parameters, also used for generic selection association
/// duplicate checking.
paramBuffer: std.ArrayList(Type.Func.Param) = .empty,
/// Enum type fields.
enumBuffer: std.ArrayList(Type.Enum.Field) = .empty,
/// Record type fields.
recordBuffer: std.ArrayList(Type.Record.Field) = .empty,
/// Attributes that have been parsed but not yet validated or applied.
attrBuffer: std.MultiArrayList(TentativeAttribute) = .empty,
/// Used to store validated attributes before they are applied to types.
attrApplicationBuffer: std.ArrayList(Attribute) = .empty,
/// type name -> variable name location for tentative definitions (top-level defs with thus-far-incomplete types)
/// e.g. `struct Foo bar;` where `struct Foo` is not defined yet.
/// The key is the StringId of `Foo` and the value is the TokenIndex of `bar`
/// Items are removed if the type is subsequently completed with a definition.
/// We only store the first tentative definition that uses a given type because this map is only used
/// for issuing an error message, and correcting the first error for a type will fix all of them for that type.
tentativeDefs: std.AutoHashMapUnmanaged(StringId, TokenIndex) = .empty,

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
/// __auto_type may only be used with a single declarator. Keep track of the name
/// so that it is not used in its own initializer.
autoTypeDeclName: StringId = .empty,

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
                try p.err(.duplicate_member, token, .{p.getTokenText(token)});
                try p.err(.previous_definition, p.recordMembers.items[i].token, .{});
                break;
            }
        }
        try p.recordMembers.append(p.comp.gpa, .{ .name = name, .token = token });
    }

    fn addFieldsFromAnonymous(r: @This(), p: *Parser, recordTy: Type.Record) Error!void {
        for (recordTy.fields) |f| {
            if (f.nameToken == 0) {
                if (f.qt.getRecord(p.comp)) |rec| {
                    try r.addFieldsFromAnonymous(p, rec);
                }
            } else {
                try r.addField(p, f.name, f.nameToken);
            }
        }
    }
} = .{},

recordMembers: std.ArrayList(struct { token: TokenIndex, name: StringId }) = .empty,

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

fn checkIdentifierCodepointWarnings(p: *Parser, codepoint: u21, loc: Source.Location) Compilation.Error!bool {
    assert(codepoint >= 0x80);

    const prevTotal = p.diagnostics.total;
    var sf = std.heap.stackFallback(1024, p.comp.gpa);
    var allocating: std.Io.Writer.Allocating = .init(sf.get());
    defer allocating.deinit();

    if (!CharInfo.isC99IdChar(codepoint)) {
        const diagnostic: Diagnostic = .c99_compat;
        try p.diagnostics.add(.{
            .kind = diagnostic.kind,
            .text = diagnostic.fmt,
            .extension = diagnostic.extension,
            .opt = diagnostic.opt,
            .location = loc.expand(p.comp),
        });
    }

    if (CharInfo.isInvisible(codepoint)) {
        const diagnostic: Diagnostic = .unicode_zero_width;
        p.formatArgs(&allocating.writer, diagnostic.fmt, .{Codepoint.init(codepoint)}) catch return error.OutOfMemory;

        try p.diagnostics.add(.{
            .kind = diagnostic.kind,
            .text = allocating.written(),
            .extension = diagnostic.extension,
            .opt = diagnostic.opt,
            .location = loc.expand(p.comp),
        });
    }

    if (CharInfo.homoglyph(codepoint)) |resembles| {
        const diagnostic: Diagnostic = .unicode_homoglyph;
        p.formatArgs(&allocating.writer, diagnostic.fmt, .{ Codepoint.init(codepoint), resembles }) catch return error.OutOfMemory;

        try p.diagnostics.add(.{
            .kind = diagnostic.kind,
            .text = allocating.written(),
            .extension = diagnostic.extension,
            .opt = diagnostic.opt,
            .location = loc.expand(p.comp),
        });
    }
    return p.diagnostics.total != prevTotal;
}

/// Issues diagnostics for the current extended identifier token
/// Return value indicates whether the token should be considered an identifier
/// true means consider the token to actually be an identifier
/// false means it is not
fn validateExtendedIdentifier(p: *Parser) !bool {
    assert(p.currToken() == .ExtendedIdentifier);

    const slice = p.getTokenText(p.tokenIdx);
    const view = std.unicode.Utf8View.init(slice) catch {
        try p.err(.invalid_utf8, p.tokenIdx, .{});
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
            if (p.comp.langOpts.dollarsInIdentifiers) {
                const diagnostic: Diagnostic = .dollar_in_identifier_extension;
                try p.diagnostics.add(.{
                    .kind = diagnostic.kind,
                    .text = diagnostic.fmt,
                    .extension = diagnostic.extension,
                    .opt = diagnostic.opt,
                    .location = loc.expand(p.comp),
                });
            }
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
            warned = try p.checkIdentifierCodepointWarnings(codepoint, loc);
    }

    if (!validIdentifier) {
        if (len == 1) {
            try p.err(.unexpected_character, p.tokenIdx, .{Codepoint.init(invalidChar)});
            return false;
        } else {
            try p.err(.invalid_identifier_start_char, p.tokenIdx, .{Codepoint.init(invalidChar)});
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
            try p.err(.dollars_in_identifiers, p.tokenIdx, .{});
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
            try p.err(switch (id) {
                .RParen => .to_match_paren,
                .RBrace => .to_match_brace,
                .RBracket => .to_match_bracket,
                else => unreachable,
            }, opening, .{});
        }
        return e;
    };
}

pub const Diagnostic = @import("Diagnostic.zig");

pub fn err(p: *Parser, diagnostic: Diagnostic, tokenIdx: TokenIndex, args: anytype) Compilation.Error!void {
    if (p.extensionSuppressd) {
        if (diagnostic.extension and diagnostic.kind == .off) return;
    }

    if (diagnostic.suppressVersion) |some| if (p.comp.langOpts.standard.atLeast(some)) return;
    if (diagnostic.suppressUnlessVersion) |some| if (!p.comp.langOpts.standard.atLeast(some)) return;
    if (p.diagnostics.effectiveKind(diagnostic) == .off) return;

    var sf = std.heap.stackFallback(1024, p.comp.gpa);
    var allocating: std.Io.Writer.Allocating = .init(sf.get());
    defer allocating.deinit();

    p.formatArgs(&allocating.writer, diagnostic.fmt, args) catch return error.OutOfMemory;

    const tok = p.pp.tokens.get(tokenIdx);
    var loc = tok.loc;
    if (tokenIdx != 0 and tok.is(.Eof)) {
        // if the token is EOF, point at the end of the previous token instead
        const prev = p.pp.tokens.get(tokenIdx - 1);
        loc = prev.loc;
        loc.byteOffset += @intCast(p.getTokenText(tokenIdx - 1).len);
    }

    try p.diagnostics.addWithLocation(p.comp, .{
        .kind = diagnostic.kind,
        .text = allocating.written(),
        .opt = diagnostic.opt,
        .extension = diagnostic.extension,
        .location = loc.expand(p.comp),
    }, p.pp.expansionSlice(tokenIdx), true);
}

fn formatArgs(p: *Parser, w: *std.Io.Writer, fmt: []const u8, args: anytype) !void {
    var i: usize = 0;
    inline for (std.meta.fields(@TypeOf(args))) |argInfo| {
        const arg = @field(args, argInfo.name);
        i += switch (@TypeOf(arg)) {
            []const u8 => try Diagnostics.formatString(w, fmt[i..], arg),
            TokenType => try formatTokenType(w, fmt[i..], arg),
            QualType => try p.formatQualType(w, fmt[i..], arg),
            TextLiteral.Ascii => try arg.format(w, fmt[i..]),
            Result => try p.formatResult(w, fmt[i..], arg),
            *Result => try p.formatResult(w, fmt[i..], arg.*),
            Enumerator, *Enumerator => try p.formatResult(w, fmt[i..], .{
                .node = undefined,
                .value = arg.value,
                .qt = arg.qt,
            }),
            Codepoint => try arg.format(w, fmt[i..]),
            Normalized => try arg.format(w, fmt[i..]),
            Escaped => try arg.format(w, fmt[i..]),
            else => switch (@typeInfo(@TypeOf(arg))) {
                .int, .comptime_int => try Diagnostics.formatInt(w, fmt[i..], arg),
                .pointer => try Diagnostics.formatString(w, fmt[i..], arg),
                else => unreachable,
            },
        };
    }
    try w.writeAll(fmt[i..]);
}

fn formatTokenType(w: *std.Io.Writer, fmt: []const u8, tokenTy: TokenType) !usize {
    const i = Diagnostics.templateIndex(w, fmt, "{tok_id}");
    try w.writeAll(tokenTy.symbol());
    return i;
}

fn formatQualType(p: *Parser, w: *std.Io.Writer, fmt: []const u8, qt: QualType) !usize {
    const i = Diagnostics.templateIndex(w, fmt, "{qt}");
    try w.writeByte('\'');
    try qt.print(p.comp, w);
    try w.writeByte('\'');

    if (qt.isC23Auto()) return i;
    if (qt.get(p.comp, .vector)) |vectorTy| {
        try w.print(" (vector of {d} '", .{vectorTy.len});
        try vectorTy.elem.printDesugared(p.comp, w);
        try w.writeAll("' values)");
    } else if (qt.shouldDesugar(p.comp)) {
        try w.writeAll(" (aka '");
        try qt.printDesugared(p.comp, w);
        try w.writeAll("')");
    }
    return i;
}

fn formatResult(p: *Parser, w: *std.Io.Writer, fmt: []const u8, res: Result) !usize {
    const i = Diagnostics.templateIndex(w, fmt, "{value}");
    switch (res.value.optRef) {
        .none => try w.writeAll("(none)"),
        .null => try w.writeAll("nullptr_t"),
        else => if (try res.value.print(res.qt, p.comp, w)) |nested| switch (nested) {
            .pointer => |ptr| {
                const ptrNode: Node.Index = @enumFromInt(ptr.node);
                const declName = p.tree.tokenSlice(ptrNode.tok(&p.tree));
                try ptr.offset.printPointer(declName, p.comp, w);
            },
        },
    }

    return i;
}

const Normalized = struct {
    str: []const u8,

    fn init(str: []const u8) Normalized {
        return .{ .str = str };
    }

    pub fn format(ctx: Normalized, w: *std.Io.Writer, fmt: []const u8) !usize {
        const i = Diagnostics.templateIndex(w, fmt, "{normalized}");
        var it: std.unicode.Utf8Iterator = .{
            .bytes = ctx.str,
            .i = 0,
        };
        while (it.nextCodepoint()) |codepoint| {
            if (codepoint < 0x7F) {
                try w.writeByte(@intCast(codepoint));
            } else if (codepoint < 0xFFFF) {
                try w.writeAll("\\u");
                try w.printInt(codepoint, 16, .upper, .{
                    .fill = '0',
                    .width = 4,
                });
            } else {
                try w.writeAll("\\U");
                try w.printInt(codepoint, 16, .upper, .{
                    .fill = '0',
                    .width = 8,
                });
            }
        }
        return i;
    }
};

const Codepoint = struct {
    codepoint: u21,

    fn init(codepoint: u21) Codepoint {
        return .{ .codepoint = codepoint };
    }

    pub fn format(ctx: Codepoint, w: *std.Io.Writer, fmt: []const u8) !usize {
        const i = Diagnostics.templateIndex(w, fmt, "{codepoint}");
        try w.print("{X:0>4}", .{ctx.codepoint});
        return i;
    }
};

const Escaped = struct {
    str: []const u8,

    fn init(str: []const u8) Escaped {
        return .{ .str = str };
    }

    pub fn format(ctx: Escaped, w: *std.Io.Writer, fmt: []const u8) !usize {
        const i = Diagnostics.templateIndex(w, fmt, "{s}");
        try std.zig.stringEscape(ctx.str, w);
        return i;
    }
};

pub fn todo(p: *Parser, msg: []const u8) Error {
    try p.err(.todo, p.tokenIdx, .{msg});
    return error.ParsingFailed;
}

pub fn removeNull(p: *Parser, str: Value) !Value {
    const stringsTop = p.strings.items.len;
    defer p.strings.items.len = stringsTop;
    {
        const bytes = p.comp.interner.get(str.ref()).bytes;
        try p.strings.appendSlice(p.comp.gpa, bytes[0 .. bytes.len - 1]);
    }
    return Value.intern(p.comp, .{ .bytes = p.strings.items[stringsTop..] });
}

pub fn errValueChanged(p: *Parser, diagnostic: Diagnostic, tokenIdx: TokenIndex, res: Result, oldValue: Value, intQt: QualType) !void {
    const zeroStr = if (res.value.isZero(p.comp)) "non-zero " else "";
    const oldRes: Result = .{ .node = undefined, .value = oldValue, .qt = res.qt };
    const newRes: Result = .{ .node = undefined, .value = res.value, .qt = intQt };

    try p.err(diagnostic, tokenIdx, .{ res.qt, intQt, zeroStr, oldRes, newRes });
}

fn checkDeprecatedUnavailable(p: *Parser, ty: QualType, usageToken: TokenIndex, declToken: TokenIndex) !void {
    if (ty.getAttribute(p.comp, .@"error")) |@"error"| {
        const msgStr = p.comp.interner.get(@"error".msg.ref()).bytes;
        try p.err(.error_attribute, usageToken, .{ p.getTokenText(@"error".__name_token), std.zig.fmtString(msgStr) });
    }

    if (ty.getAttribute(p.comp, .warning)) |warning| {
        const msgStr = p.comp.interner.get(warning.msg.ref()).bytes;
        try p.err(.warning_attribute, usageToken, .{ p.getTokenText(warning.__name_token), std.zig.fmtString(msgStr) });
    }

    // Check if the type has an 'unavailable' attribute and report it
    if (ty.getAttribute(p.comp, .unavailable)) |unavailable| {
        try p.errDeprecated(.unavailable, usageToken, unavailable.msg);
        try p.err(.unavailable_note, unavailable.__name_token, .{p.getTokenText(declToken)});
        return error.ParsingFailed; // Abort parsing due to 'unavailable' type
    }

    // Check if the type has a 'deprecated' attribute and report it
    else if (ty.getAttribute(p.comp, .deprecated)) |deprecated| {
        try p.errDeprecated(.deprecated_declarations, usageToken, deprecated.msg);
        try p.err(.deprecated_note, deprecated.__name_token, .{p.getTokenText(declToken)});
    }
}

/// Reports deprecated or unavailable usage of code based on the diagnostic tag.
/// It constructs an error message and then calls `errStr` to handle the error.
fn errDeprecated(p: *Parser, diagnostic: Diagnostic, tokenIdx: TokenIndex, msg: ?Value) Compilation.Error!void {
    const colonStr: []const u8 = if (msg != null) ": " else "";
    const msgStr: []const u8 = if (msg) |m| p.comp.interner.get(m.ref()).bytes else "";
    return p.err(diagnostic, tokenIdx, .{ p.getTokenText(tokenIdx), colonStr, Escaped.init(msgStr) });
}

fn errExpectedToken(p: *Parser, expected: TokenType, actual: TokenType) Error {
    switch (actual) {
        .Invalid => try p.err(.expected_invalid, p.tokenIdx, .{expected}),
        .Eof => try p.err(.expected_eof, p.tokenIdx, .{expected}),
        else => try p.err(.expected_token, p.tokenIdx, .{ expected, actual }),
    }
    return error.ParsingFailed;
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

pub fn addNode(p: *Parser, node: AST.Node) Allocator.Error!Node.Index {
    if (p.inMacro) return undefined;
    return p.tree.addNode(node);
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
    var cur = node;
    while (true) {
        switch (cur.get(&p.tree)) {
            .parenExpr => |un| cur = un.operand,
            .stringLiteralExpr => return p.tree.valueMap.get(cur),
            .cast => |cast| switch (cast.kind) {
                .NoOP, .Bitcast, .ArrayToPointer => cur = cast.operand,
                else => return null,
            },
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
        try p.err(.tentative_definition_incomplete, tentativeDefToken, .{forward.containerQt});
        try p.err(.forward_declaration_here, forward.nameOrKindToken, .{forward.containerQt});
    }
}

fn addImplicitTypedef(p: *Parser, name: []const u8, qt: QualType) !void {
    const gpa = p.comp.gpa;
    const start = p.comp.generatedBuffer.items.len;

    try p.comp.generatedBuffer.ensureUnusedCapacity(gpa, name.len + 1);
    p.comp.generatedBuffer.appendSliceAssumeCapacity(name);
    p.comp.generatedBuffer.appendAssumeCapacity('\n');

    const nameToken: u32 = @intCast(p.pp.tokens.len);
    try p.pp.tokens.append(gpa, .{ .id = .Identifier, .loc = .{
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
    const typedefQt = (try p.comp.typeStore.put(gpa, .{ .typedef = .{
        .base = qt,
        .name = internedName,
        .declNode = node,
    } })).withQualifiers(qt);
    try p.symStack.defineTypedef(internedName, typedefQt, nameToken, node);
    try p.declBuffer.append(gpa, node);
}

/// root : (decl | inline assembly ';' | static-assert-declaration)*
pub fn parse(pp: *Preprocessor) Compilation.Error!AST {
    const gpa = pp.comp.gpa;
    assert(pp.linemarkers == .None);
    pp.comp.pragmaEvent(.BeforeParse);

    var p = Parser{
        .pp = pp,
        .comp = pp.comp,
        .diagnostics = pp.diagnostics,
        .tokenIds = pp.tokens.items(.id),

        .tree = .{
            .comp = pp.comp,
            .tokens = pp.tokens.slice(),
        },

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
        p.labels.deinit(gpa);
        p.strings.deinit(gpa);
        p.symStack.deinit(gpa);
        p.listBuffer.deinit(gpa);
        p.declBuffer.deinit(gpa);
        p.paramBuffer.deinit(gpa);
        p.enumBuffer.deinit(gpa);
        p.recordBuffer.deinit(gpa);
        p.recordMembers.deinit(gpa);
        p.attrBuffer.deinit(gpa);
        p.attrApplicationBuffer.deinit(gpa);
        p.tentativeDefs.deinit(gpa);
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
                else => try p.err(.expected_external_decl, p.tokenIdx, .{}),
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
            try p.declBuffer.append(gpa, node);
            continue;
        }

        if (p.eat(.Semicolon)) |tok| {
            try p.err(.extra_semi, tok, .{});
            const empty = try p.addNode(.{ .emptyDecl = .{ .semicolon = tok } });
            try p.declBuffer.append(gpa, empty);
            continue;
        }

        try p.err(.expected_external_decl, p.tokenIdx, .{});
        p.nextExternDecl();
    }

    if (p.tentativeDefs.count() > 0)
        try p.diagnoseIncompleteDefinitions();

    p.tree.rootDecls = p.declBuffer;
    p.declBuffer = .empty;

    if (p.tree.rootDecls.items.len == implicitTypedefCount)
        try p.err(.empty_translation_unit, p.tokenIdx - 1, .{});

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
            .RParen, .RBrace, .RBracket => parens -|= 1,

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
            .KeywordSigned1,
            .KeywordSigned2,
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
    const gpa = p.comp.gpa;

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
            .Asterisk, .LParen => {},
            .Identifier, .ExtendedIdentifier => switch (p.tokenIds[firstToken + 1]) {
                .Identifier, .ExtendedIdentifier => {
                    // The most likely reason for `identifier identifier` is
                    // an unknown type name.
                    try p.err(.unknown_type_name, p.tokenIdx, .{p.getTokenText(p.tokenIdx)});
                    p.tokenIdx += 1;
                    break :blk DeclSpec{ .qt = .invalid };
                },
                else => {},
            },
            else => if (p.tokenIdx != firstToken) {
                try p.err(.expected_ident_or_l_paren, p.tokenIdx, .{});
                return error.ParsingFailed;
            } else return false,
        }

        var builder: TypeBuilder = .{ .parser = p };
        break :blk DeclSpec{ .qt = try builder.finish() };
    };

    if (declSpec.noreturn) |token| {
        const attr = Attribute{ .tag = .noreturn, .args = .{ .noreturn = .{} }, .syntax = .keyword };
        try p.attrBuffer.append(gpa, .{ .attr = attr, .tok = token });
    }

    var declNode = try p.tree.addNode(.{ .emptyDecl = .{ .semicolon = firstToken } });
    var initDeclarator = (try p.parseInitDeclarator(&declSpec, attrBufferTop, declNode)) orelse {
        _ = try p.expectToken(.Semicolon); // eat ';'

        missingDecl: {
            if (declSpec.qt.type(p.comp) == .typeof) {
                try p.err(.missing_declaration, firstToken, .{});
                return true;
            }
            switch (declSpec.qt.base(p.comp).type) {
                .@"enum" => break :missingDecl,
                .@"struct", .@"union" => |recordTy| if (!recordTy.isAnonymous(p.comp)) break :missingDecl,
                else => {},
            }

            try p.err(.missing_declaration, firstToken, .{});
            return true;
        }

        const attrs = p.attrBuffer.items(.attr)[attrBufferTop..];
        const toks = p.attrBuffer.items(.tok)[attrBufferTop..];
        for (attrs, toks) |attr, tok| {
            try p.err(.ignored_record_attr, tok, .{ @tagName(attr.tag), @tagName(declSpec.qt.base(p.comp).type) });
        }
        return true;
    };

    // check for function definition
    if (initDeclarator.d.declaratorType == .func and initDeclarator.initializer == null) fndef: {
        switch (p.currToken()) {
            .Comma, .Semicolon => break :fndef,
            .LBrace => {},
            else => {
                if (initDeclarator.d.oldTypeFunc == null) {
                    try p.err(.expected_fn_body, p.tokenIdx - 1, .{});
                    return true;
                }
            },
        }

        if (p.func.qt != null) try p.err(.func_not_in_root, p.tokenIdx, .{});

        const declaratorName = initDeclarator.d.name;
        const internedDeclaratorName = try p.getInternString(declaratorName);
        try p.symStack.defineSymbol(internedDeclaratorName, initDeclarator.d.qt, initDeclarator.d.name, declNode, .{}, false);

        const func = p.func;
        p.func = .{ .qt = initDeclarator.d.qt, .name = declaratorName };
        defer p.func = func;

        if (internedDeclaratorName == p.stringsIds.mainId) {
            const funcTy = initDeclarator.d.qt.get(p.comp, .func).?;
            const intTy = funcTy.returnType.get(p.comp, .int);
            if (intTy == null or intTy.? != .Int) {
                try p.err(.main_return_type, declaratorName, .{});
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

            const newParams = try p.paramBuffer.addManyAsSlice(gpa, paramsLen);
            for (newParams) |*newParam| {
                newParam.name = .empty;
            }

            paramLoop: while (true) {
                const paramDeclSpec = (try p.parseDeclSpec()) orelse break;
                if (p.eat(.Semicolon)) |semi| {
                    try p.err(.missing_declaration, semi, .{});
                    continue :paramLoop;
                }

                while (true) {
                    const attrBufferTopDeclarator = p.attrBuffer.len;
                    defer p.attrBuffer.len = attrBufferTopDeclarator;

                    var paramD = (try p.declarator(paramDeclSpec.qt, .param)) orelse {
                        try p.err(.missing_declaration, firstToken, .{});
                        _ = try p.expectToken(.Semicolon);
                        continue :paramLoop;
                    };

                    try p.parseAttrSpec();

                    if (paramD.qt.hasIncompleteSize(p.comp)) {
                        if (paramD.qt.is(p.comp, .void)) {
                            try p.err(.invalid_void_param, paramD.name, .{});
                        } else {
                            try p.err(.parameter_incomplete_ty, paramD.name, .{paramD.qt});
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
                        try p.err(.parameter_missing, paramD.name, .{nameStr});
                    }

                    if (p.eat(.Comma) == null) break;
                }

                _ = try p.expectToken(.Semicolon);
            }

            const funcTy = funcQt.get(p.comp, .func).?;
            for (funcTy.params, newParams) |param, *newParam| {
                if (newParam.name == .empty) {
                    try p.err(.param_not_declared, param.nameToken, .{param.name.lookup(p.comp)});
                    newParam.* = .{
                        .name = param.name,
                        .nameToken = param.nameToken,
                        .node = param.node,
                        .qt = .int,
                    };
                }
            }
            // Update the function type to contain the declared parameters.
            p.func.qt = try p.comp.typeStore.put(gpa, .{ .func = .{
                .kind = .Normal,
                .params = newParams,
                .returnType = funcTy.returnType,
            } });
        } else if (initDeclarator.d.qt.get(p.comp, .func)) |funcTy| {
            for (funcTy.params) |param| {
                if (param.name == .empty) {
                    try p.err(.omitting_parameter_name, param.nameToken, .{});
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
                                try p.err(.unbound_vla, param.nameToken, .{});
                            }
                        }
                    }
                }

                if (param.qt.hasIncompleteSize(p.comp) and !param.qt.is(p.comp, .void))
                    try p.err(.parameter_incomplete_ty, param.nameToken, .{param.qt});
            }
        }

        const body = (try p.parseCompoundStmt(true, null)) orelse {
            assert(initDeclarator.d.oldTypeFunc != null);
            try p.err(.expected_fn_body, p.tokenIdx, .{});
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
        try p.declBuffer.append(gpa, declNode);

        // check gotos
        if (func.qt == null) {
            for (p.labels.items) |item| {
                if (item == .unresolvedGoto)
                    try p.err(.undeclared_label, item.unresolvedGoto, .{p.getTokenText(item.unresolvedGoto)});

                if (p.computedGotoTok) |gotoToken| {
                    if (!p.containsAddresssOfLabel)
                        try p.err(.invalid_computed_goto, gotoToken, .{});
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
            try p.err(.invalid_old_style_params, tokenIdx, .{});

        if (declSpec.storageClass == .typedef) {
            try declSpec.validateDecl(p);
            try p.tree.setNode(@intFromEnum(declNode), .{
                .typedef = .{
                    .nameToken = initDeclarator.d.name,
                    .qt = initDeclarator.d.qt,
                    .implicit = false,
                },
            });
        } else if (initDeclarator.d.declaratorType == .func or initDeclarator.d.qt.is(p.comp, .func)) {
            try declSpec.validateFnDecl(p);
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
            try declSpec.validateDecl(p);
            var nodeQt = initDeclarator.d.qt;
            if (p.func.qt == null and declSpec.storageClass != .@"extern") {
                if (nodeQt.get(p.comp, .array)) |arrayTy| {
                    if (arrayTy.len == .incomplete) {
                        // Create tentative array node with fixed type.
                        nodeQt = try p.comp.typeStore.put(gpa, .{ .array = .{
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
        try p.declBuffer.append(gpa, declNode);

        const internedName = try p.getInternString(initDeclarator.d.name);
        if (declSpec.storageClass == .typedef) {
            const typedefQt = if (initDeclarator.d.qt.isInvalid())
                initDeclarator.d.qt
            else
                (try p.comp.typeStore.put(gpa, .{ .typedef = .{
                    .base = initDeclarator.d.qt,
                    .name = internedName,
                    .declNode = declNode,
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
                try p.err(.auto_type_requires_single_declarator, tokIdx, .{});
                warnedAuto = true;
            }
            if (declSpec.c23Auto) |tokIdx| {
                try p.err(.c23_auto_single_declarator, tokIdx, .{});
                warnedAuto = true;
            }
        }

        declNode = try p.tree.addNode(.{ .emptyDecl = .{ .semicolon = p.tokenIdx - 1 } });
        initDeclarator = (try p.parseInitDeclarator(&declSpec, attrBufferTop, declNode)) orelse {
            try p.err(.expected_ident_or_l_paren, p.tokenIdx, .{});
            continue;
        };
    }

    _ = try p.expectToken(.Semicolon);
    return true;
}

fn staticAssertMessage(
    p: *Parser,
    condNode: Node.Index,
    maybeMessage: ?Result,
    allocating: *std.Io.Writer.Allocating,
) !?[]const u8 {
    const w = &allocating.writer;

    const cond = condNode.get(&p.tree);
    if (cond == .builtinTypesCompatibleP) {
        try w.writeAll("'__builtin_types_compatible_p(");

        const lhsTy = cond.builtinTypesCompatibleP.lhs;
        try lhsTy.print(p.comp, w);
        try w.writeAll(", ");

        const rhsTy = cond.builtinTypesCompatibleP.rhs;
        try rhsTy.print(p.comp, w);

        try w.writeAll(")'");
    } else if (maybeMessage == null) return null;

    if (maybeMessage) |message| {
        assert(message.node.get(&p.tree) == .stringLiteralExpr);

        if (allocating.written().len > 0)
            try p.strings.append(p.comp.gpa, ' ');

        const bytes = p.comp.interner.get(message.value.ref()).bytes;
        try Value.printString(bytes, message.qt, p.comp, w);
    }

    return allocating.written();
}

/// static-assert-declaration
///  : (`_Static_assert` | `static_assert`) '(' integer-const-expression ',' StringLiteral+ ')' ';'
fn parseStaticAssert(p: *Parser) Error!bool {
    const gpa = p.comp.gpa;
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
                try p.err(.expected_str_literal, p.tokenIdx, .{});
                return error.ParsingFailed;
            },
        }
    else
        null;

    try p.expectClosing(lp, .RParen);
    _ = try p.expectToken(.Semicolon);
    if (str == null) {
        try p.err(.static_assert_missing_message, staticAssert, .{});
        try p.err(.pre_c23_compat, staticAssert, .{"'_Static_assert' with no message"});
    }

    const isIntExpr = res.qt.isInvalid() or res.qt.isInt(p.comp);
    try res.castToBool(p, .bool, resToken);
    if (!isIntExpr) res.value = .{};

    if (res.value.isNone()) {
        if (!res.qt.isInvalid())
            try p.err(.static_assert_not_constant, resToken, .{});
    } else {
        if (!res.value.toBool(p.comp)) {
            var sf = std.heap.stackFallback(1024, gpa);
            var allocating: std.Io.Writer.Allocating = .init(sf.get());
            defer allocating.deinit();

            if (p.staticAssertMessage(resNode, str, &allocating) catch return error.OutOfMemory) |message|
                try p.err(.static_assert_failure_message, staticAssert, .{message})
            else
                try p.err(.static_assert_failure, staticAssert, .{});
        }
    }

    const node = try p.addNode(.{
        .staticAssert = .{
            .assertToken = staticAssert,
            .cond = res.node,
            .message = if (str) |some| some.node else null,
        },
    });
    try p.declBuffer.append(gpa, node);
    return true;
}

/// typeof
///   : `typeof` '(' type-name ')'
///   | `typeof` '(' expr ')'
fn typeof(p: *Parser) Error!?QualType {
    const gpa = p.comp.gpa;
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

        const typeofQt = try p.comp.typeStore.put(gpa, .{ .typeof = .{
            .base = qt,
            .expr = null,
        } });
        return typeofQt.withQualifiers(qt);
    }

    const typeofExpr = try p.parseNoEval(parseExpr);
    try p.expectClosing(lp, .RParen);

    if (typeofExpr.qt.isInvalid()) return null;

    const typeofQt = try p.comp.typeStore.put(gpa, .{ .typeof = .{
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
                    try p.err(.duplicate_decl_spec, p.tokenIdx, .{"inline"});
                d.@"inline" = p.tokenIdx;
                p.tokenIdx += 1;
                continue;
            },

            .KeywordNoreturn => {
                if (d.noreturn != null)
                    try p.err(.duplicate_decl_spec, p.tokenIdx, .{"_Noreturn"});
                d.noreturn = p.tokenIdx;
                p.tokenIdx += 1;
                continue;
            },

            .KeywordAutoType => {
                try p.err(.auto_type_extension, p.tokenIdx, .{});
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

            .KeywordForceInline, .KeywordForceInline2 => {
                try p.attrBuffer.append(p.comp.gpa, .{
                    .tok = p.tokenIdx,
                    .attr = .{ .tag = .always_inline, .args = .{ .always_inline = .{} }, .syntax = .keyword },
                });
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
                    try p.err(.multiple_storage_class, p.tokenIdx, .{@tagName(d.storageClass)});
                    return error.ParsingFailed;
                }

                if (d.threadLocal != null) {
                    switch (token) {
                        .KeywordExtern, .KeywordStatic => {},
                        else => try p.err(.cannot_combine_spec, p.tokenIdx, .{token.lexeme().?}),
                    }
                    if (d.constexpr) |tok|
                        try p.err(.cannot_combine_spec, p.tokenIdx, .{p.tokenIds[tok].lexeme().?});
                }

                if (d.constexpr != null) {
                    switch (token) {
                        .KeywordAuto, .KeywordRegister, .KeywordStatic => {},
                        else => try p.err(.cannot_combine_spec, p.tokenIdx, .{token.lexeme().?}),
                    }
                    if (d.threadLocal) |tok|
                        try p.err(.cannot_combine_spec, p.tokenIdx, .{p.tokenIds[tok].lexeme().?});
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
                    try p.err(.duplicate_decl_spec, p.tokenIdx, .{token.lexeme().?});

                if (d.constexpr) |tok|
                    try p.err(.cannot_combine_spec, p.tokenIdx, .{p.tokenIds[tok].lexeme().?});

                switch (d.storageClass) {
                    .@"extern", .none, .static => {},
                    else => try p.err(.cannot_combine_spec, p.tokenIdx, .{@tagName(d.storageClass)}),
                }

                d.threadLocal = p.tokenIdx;
            },

            .KeywordConstexpr => {
                if (d.constexpr != null)
                    try p.err(.duplicate_decl_spec, p.tokenIdx, .{token.lexeme().?});

                if (d.threadLocal) |tok|
                    try p.err(.cannot_combine_spec, p.tokenIdx, .{p.tokenIds[tok].lexeme().?});

                switch (d.storageClass) {
                    .auto, .register, .none, .static => {},
                    else => try p.err(.cannot_combine_spec, p.tokenIdx, .{@tagName(d.storageClass)}),
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
    if (!p.currToken().isMacroIdentifier()) {
        return p.errExpectedToken(.Identifier, p.currToken());
    }
    _ = (try p.eatIdentifier()) orelse {
        p.tokenIdx += 1;
    };

    const name = p.getTokenText(nameToken);
    const attr = Attribute.fromString(kind, namespace, name) orelse {
        try p.err(if (kind == .declspec) .declspec_attr_not_supported else .unknown_attribute, nameToken, .{name});
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
                    if (try Attribute.diagnoseIdent(attr, &arguments, ident, p)) {
                        p.skipTo(.RParen);
                        return error.ParsingFailed;
                    }
                } else {
                    try p.err(.attribute_requires_identifier, nameToken, .{name});
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
        try p.err(.attribute_not_enough_args, nameToken, .{ @tagName(attr), requiredCount });
        return error.ParsingFailed;
    }
    return TentativeAttribute{ .attr = .{ .tag = attr, .args = arguments, .syntax = kind.toSyntax() }, .tok = nameToken };
}

fn handleAttrParam(p: *Parser, attr: Attribute.Tag, arguments: *Attribute.Arguments, argIdx: u32) Error!void {
    const argStart = p.tokenIdx;
    const argExpr = try p.expect(parseAssignExpr);
    if (try p.diagnose(attr, arguments, argIdx, argExpr, argStart)) {
        p.skipTo(.RParen);
        return error.ParsingFailed;
    }
}

fn diagnose(p: *Parser, attr: Attribute.Tag, arguments: *Attribute.Arguments, argIdx: u32, res: Result, argStart: TokenIndex) !bool {
    if (Attribute.wantsAlignment(attr, argIdx))
        return Attribute.diagnoseAlignment(attr, arguments, argIdx, res, argStart, p);

    return Attribute.diagnose(attr, arguments, argIdx, res, argStart, res.node.get(&p.tree), p);
}

fn handleAttr(p: *Parser, format: Attribute.Kind, namespace: ?[]const u8) Error!void {
    if (try p.attribute(format, namespace)) |attr|
        try p.attrBuffer.append(p.comp.gpa, attr);
}

/// attribute-list : (attribute (',' attribute)*)?
fn parseGNUAttrList(p: *Parser) Error!void {
    if (p.currToken() == .RParen) return;

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
    return attributeSpecifier(p, null);
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
                try p.err(.declspec_not_allowed_after_declarator, maybeDeclspecToken, .{});
                try p.err(.declarator_name_tok, nameToken, .{});
                p.attrBuffer.len = attrBufferTop;
            }
            continue;
        }
        break;
    }
}

const InitDeclarator = struct { d: Declarator, initializer: ?Result = null };

/// init-declarator : declarator assembly? attribute-specifier? ('=' initializer)?
fn parseInitDeclarator(
    p: *Parser,
    declSpec: *DeclSpec,
    attrBufferTop: usize,
    declNode: Node.Index,
) Error!?InitDeclarator {
    const thisAttrBufferTop = p.attrBuffer.len;
    defer p.attrBuffer.len = thisAttrBufferTop;
    const gpa = p.comp.gpa;

    var ID = InitDeclarator{ .d = (try p.declarator(declSpec.qt, .normal)) orelse return null };

    try p.attributeSpecifier(ID.d.name);
    _ = try p.parseAssembly(.declLabel);
    try p.attributeSpecifier(ID.d.name);

    switch (ID.d.declaratorType) {
        .func => {
            if (declSpec.autoType) |tokenIdx| {
                try p.err(.auto_type_not_allowed, tokenIdx, .{"function return type"});
                ID.d.qt = .invalid;
            } else if (declSpec.c23Auto) |tokenIdx| {
                try p.err(.c23_auto_not_allowed, tokenIdx, .{"function return type"});
                ID.d.qt = .invalid;
            }
        },
        .array => {
            if (declSpec.autoType) |tokenIdx| {
                try p.err(.auto_type_array, tokenIdx, .{p.getTokenText(ID.d.name)});
                ID.d.qt = .invalid;
            } else if (declSpec.c23Auto) |tokenIndex| {
                try p.err(.c23_auto_array, tokenIndex, .{p.getTokenText(ID.d.name)});
                ID.d.qt = .invalid;
            }
        },
        .pointer => {
            if (declSpec.autoType != null or declSpec.c23Auto != null) {
                // TODO this is not a hard error in clang
                try p.err(.auto_type_requires_plain_declarator, p.tokenIdx, .{});
                ID.d.qt = .invalid;
            }
        },
        .other => if (declSpec.storageClass == .typedef) {
            if (declSpec.autoType) |tokenIdx| {
                try p.err(.auto_type_not_allowed, tokenIdx, .{"typedef"});
                ID.d.qt = .invalid;
            } else if (declSpec.c23Auto) |tokenIdx| {
                try p.err(.c23_auto_not_allowed, tokenIdx, .{"typedef"});
                ID.d.qt = .invalid;
            }
        },
    }

    var applyVarAttributes = false;
    if (declSpec.storageClass == .typedef) {
        ID.d.qt = try Attribute.applyTypeAttributes(p, ID.d.qt, attrBufferTop, null);
    } else if (ID.d.declaratorType == .func or ID.d.qt.is(p.comp, .func)) {
        ID.d.qt = try Attribute.applyFunctionAttributes(p, ID.d.qt, attrBufferTop);
    } else {
        applyVarAttributes = true;
    }

    if (p.eat(.Equal)) |eq| {
        if (declSpec.storageClass == .typedef or (ID.d.declaratorType == .func and ID.d.qt.is(p.comp, .func)))
            try p.err(.illegal_initializer, eq, .{})
        else if (ID.d.qt.get(p.comp, .array)) |arrayTy| {
            if (arrayTy.len == .variable) try p.err(.vla_init, eq, .{});
        } else if (declSpec.storageClass == .@"extern") {
            try p.err(.extern_initializer, p.tokenIdx, .{});
            declSpec.storageClass = .none;
        }

        incomplete: {
            if (ID.d.qt.isInvalid()) break :incomplete;
            if (ID.d.qt.isC23Auto()) break :incomplete;
            if (ID.d.qt.isAutoType()) break :incomplete;
            if (!ID.d.qt.hasIncompleteSize(p.comp)) break :incomplete;
            if (ID.d.qt.get(p.comp, .array)) |arrayTy| {
                if (arrayTy.len == .incomplete) break :incomplete;
            }
            try p.err(.variable_incomplete_ty, ID.d.name, .{ID.d.qt});
            ID.d.qt = .invalid;
        }

        try p.symStack.pushScope();
        defer p.symStack.popScope();

        const internedName = try p.getInternString(ID.d.name);
        try p.symStack.declareSymbol(internedName, ID.d.qt, ID.d.name, declNode);

        // TODO this should be a stack of auto type names because of statement expressions.
        if (ID.d.qt.isAutoType() or ID.d.qt.isC23Auto()) {
            p.autoTypeDeclName = internedName;
        }
        defer p.autoTypeDeclName = .empty;

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
                        ID.d.qt = (try p.comp.typeStore.put(gpa, .{ .array = .{
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
            ID.d.qt = some.qt.withQualifiers(ID.d.qt);
        } else {
            if (ID.d.qt.isC23Auto())
                try p.err(.c23_auto_requires_initializer, name, .{})
            else
                try p.err(.auto_type_requires_initializer, name, .{p.getTokenText(name)});
            ID.d.qt = .invalid;
            return ID;
        }
    }

    if (applyVarAttributes)
        ID.d.qt = try Attribute.applyVariableAttributes(p, ID.d.qt, attrBufferTop, null);

    incomplete: {
        if (declSpec.storageClass == .typedef) break :incomplete;
        if (ID.d.qt.isInvalid()) break :incomplete;
        if (!ID.d.qt.hasIncompleteSize(p.comp)) break :incomplete;

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
                    try p.err(.tentative_array, name, .{});
                    break :incomplete;
                },
                .@"struct", .@"union" => |recordTy| {
                    _ = try p.tentativeDefs.getOrPutValue(gpa, recordTy.name, ID.d.name);
                    break :incomplete;
                },
                .@"enum" => |enumTy| {
                    _ = try p.tentativeDefs.getOrPutValue(gpa, enumTy.name, ID.d.name);
                    break :incomplete;
                },
                else => {},
            }
        }
        try p.err(.variable_incomplete_ty, name, .{ID.d.qt});
        ID.d.qt = .invalid;
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

        if (try p.parseTypeQual(builder, true))
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
            .KeywordSigned, .KeywordSigned1, .KeywordSigned2 => try builder.combine(.Signed, p.tokenIdx),
            .KeywordUnsigned => try builder.combine(.Unsigned, p.tokenIdx),
            .KeywordFp16 => try builder.combine(.FP16, p.tokenIdx),
            .KeywordFloat16 => try builder.combine(.Float16, p.tokenIdx),
            .KeywordFloat => try builder.combine(.Float, p.tokenIdx),
            .KeywordDouble => try builder.combine(.Double, p.tokenIdx),
            .KeywordComplex => try builder.combine(.Complex, p.tokenIdx),
            .KeywordFloat128_, .KeywordFloat128__ => {
                if (!p.comp.hasFloat128())
                    try p.err(.type_not_supported_on_target, p.tokenIdx, .{p.currToken().lexeme().?});
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
                    try p.err(.expected_type, p.tokenIdx, .{});
                    return error.ParsingFailed;
                };

                try p.expectClosing(lp, .RParen);

                if (baseQt.isQualified() and !baseQt.isInvalid()) {
                    try p.err(.atomic_qualified, atomicToken, .{baseQt});
                    builder.type = .{ .other = .invalid };
                    continue;
                }

                try builder.combineAtomic(baseQt, atomicToken);
                continue;
            },

            .KeywordAlignas, .KeywordC23Alignas => {
                const alignToken = p.tokenIdx;
                p.tokenIdx += 1;

                const gpa = p.comp.gpa;
                const lparen = try p.expectToken(.LParen);
                const typenameStart = p.tokenIdx;
                if (try p.parseTypeName()) |innerQt| {
                    if (!innerQt.alignable(p.comp))
                        try p.err(.invalid_alignof, typenameStart, .{innerQt});

                    const alignment = Attribute.Alignment{ .requested = innerQt.alignof(p.comp) };
                    try p.attrBuffer.append(gpa, .{
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
                        if (try p.diagnose(.aligned, &args, 0, res, argStart)) {
                            p.skipTo(.RParen);
                            return error.ParsingFailed;
                        }
                        args.aligned.alignment.?.node = .pack(res.node);
                        try p.attrBuffer.append(gpa, .{
                            .attr = .{ .tag = .aligned, .args = args, .syntax = .keyword },
                            .tok = alignToken,
                        });
                    }
                }
                try p.expectClosing(lparen, .RParen);
                continue;
            },

            .KeywordEnum => {
                const tagToken = p.tokenIdx;
                const enumTy = try p.parseEnumSpec();
                try builder.combine(.{ .other = enumTy }, tagToken);
                continue;
            },

            .KeywordStruct, .KeywordUnion => {
                const tagToken = p.tokenIdx;
                const recordTy = try p.parseRecordSpec();
                try builder.combine(.{ .other = recordTy }, tagToken);
                continue;
            },

            .Identifier, .ExtendedIdentifier => {
                var internedName = try p.getInternString(p.tokenIdx);
                var declspecFound = false;
                if (internedName == p.stringsIds.declSpecId) {
                    try p.err(.declspec_not_enabled, p.tokenIdx, .{});
                    p.tokenIdx += 1;

                    if (p.eat(.LParen)) |_| {
                        p.skipTo(.RParen);
                        continue;
                    }
                    declspecFound = true;
                }

                if (declspecFound)
                    internedName = try p.getInternString(p.tokenIdx);

                const typedef = (try p.symStack.findTypedef(internedName, p.tokenIdx, builder.type != .None)) orelse break;
                if (!builder.combineTypedef(typedef.qt))
                    break;
            },

            .KeywordBitInt => {
                try p.err(.bit_int, p.tokenIdx, .{});
                const bitIntToken = p.tokenIdx;
                p.tokenIdx += 1;

                const lparen = try p.expectToken(.LParen);
                const res = try p.parseIntegerConstExpr(.GNUFoldingExtension);
                try p.expectClosing(lparen, .RParen);

                var bits: u64 = undefined;
                if (res.value.isNone()) {
                    try p.err(.expected_integer_constant_expr, bitIntToken, .{});
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

        // consume single token specifier here.
        p.tokenIdx += 1;
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

    var arena = p.comp.typeStore.annoNameArena.promote(p.comp.gpa);
    defer p.comp.typeStore.annoNameArena = arena.state;

    const str = try std.fmt.allocPrint(
        arena.allocator(),
        "(anonymous {s} at {s}:{d}:{d})",
        .{ kindStr, source.path, lineAndCol.lineNo, lineAndCol.col },
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
fn parseRecordSpec(p: *Parser) Error!QualType {
    const gpa = p.comp.gpa;
    const startingPragmaPack = p.pragmaPack;
    const kindToken = p.tokenIdx;
    const isStruct = p.tokenIds[kindToken] == .KeywordStruct;
    p.tokenIdx += 1;

    const attrBufferTop = p.attrBuffer.len;
    defer p.attrBuffer.len = attrBufferTop;

    try p.parseAttrSpec();

    const reservedIndex = try p.tree.nodes.addOne(gpa);

    const maybeIdent = try p.eatIdentifier();
    const lb = p.eat(.LBrace) orelse {
        const ident = maybeIdent orelse {
            try p.err(.ident_or_l_brace, p.tokenIdx, .{});
            return error.ParsingFailed;
        };

        // check if this is a reference to a previous type
        const internedName = try p.getInternString(ident);
        if (try p.symStack.findTag(internedName, p.tokenIds[kindToken], ident, p.currToken())) |prev| {
            return prev.qt;
        } else {
            // this is a forward declaration, create a new record type.
            const recordTy: Type.Record = .{
                .name = internedName,
                .layout = null,
                .declNode = @enumFromInt(reservedIndex),
                .fields = &.{},
            };
            const recordQt = try p.comp.typeStore.put(gpa, if (isStruct)
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
            try p.tree.setNode(reservedIndex, if (isStruct)
                .{ .structForwardDecl = fw }
            else
                .{ .unionForwardDecl = fw });
            try p.declBuffer.append(gpa, @enumFromInt(reservedIndex));
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
                    try p.err(.redefinition, ident, .{identStr});
                    try p.err(.previous_definition, prev.token, .{});
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
            .declNode = @enumFromInt(reservedIndex),
            .layout = null,
            .fields = &.{},
        };
        const recordQt = try p.comp.typeStore.put(gpa, if (isStruct)
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

    try p.declBuffer.append(gpa, @enumFromInt(reservedIndex));
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
        if (fields.len == 1 and isStruct) {
            if (p.comp.langOpts.emulate == .msvc) {
                try p.err(.flexible_in_empty_msvc, some, .{});
            } else {
                try p.err(.flexible_in_empty, some, .{});
            }
        }
    }

    if (p.recordBuffer.items.len == recordBufferTop) {
        try p.err(.empty_record, kindToken, .{p.getTokenText(kindToken)});
        try p.err(.empty_record_size, kindToken, .{p.getTokenText(kindToken)});
    }

    try p.expectClosing(lb, .RBrace);
    done = true;
    try p.parseAttrSpec();

    const anyIncomplete = blk: {
        for (fields) |field| {
            if (field.qt.hasIncompleteSize(p.comp) and !field.qt.is(p.comp, .array)) break :blk true;
        }
        // Set fields and a dummy layout before addign attributes.
        recordType.fields = fields;
        recordType.layout = .{
            .sizeBits = 8,
            .fieldAlignmentBits = 8,
            .pointerAlignmentBits = 8,
            .requiredAlignmentBits = 8,
        };
        recordType.declNode = @enumFromInt(reservedIndex);

        const baseType = qt.base(p.comp);
        if (isStruct) {
            std.debug.assert(baseType.type.@"struct".name == recordType.name);
            try p.comp.typeStore.set(gpa, .{ .@"struct" = recordType }, @intFromEnum(baseType.qt._index));
        } else {
            std.debug.assert(baseType.type.@"union".name == recordType.name);
            try p.comp.typeStore.set(gpa, .{ .@"union" = recordType }, @intFromEnum(baseType.qt._index));
        }
        break :blk false;
    };

    const attributedQt = try Attribute.applyTypeAttributes(p, qt, attrBufferTop, null);

    // Make sure the symbol for this record points to the attributed type.
    if (attributedQt != qt and maybeIdent != null) {
        const identStr = p.getTokenText(maybeIdent.?);
        const internedName = try p.comp.internString(identStr);
        const ptr = p.symStack.getPtr(internedName, .tags);
        ptr.qt = attributedQt;
    }

    if (!anyIncomplete) {
        const pragmaPackValue = switch (p.comp.langOpts.emulate) {
            .clang => startingPragmaPack,
            .gcc => p.pragmaPack,
            .msvc => p.pragmaPack,
        };

        if (RecordLayout.compute(fields, attributedQt, p.comp, pragmaPackValue)) |layout| {
            recordType.fields = fields;
            recordType.layout = layout;
        } else |er| switch (er) {
            error.Overflow => try p.err(.record_too_large, maybeIdent orelse kindToken, .{qt}),
        }

        // Override previous incomplete layout and fields.
        const baseQt = qt.base(p.comp).qt;
        const ts = &p.comp.typeStore;
        var extraIndex = ts.types.items(.data)[@intFromEnum(baseQt._index)][1];

        const layoutSize = 5;
        comptime std.debug.assert(@sizeOf(Type.Record.Layout) == @sizeOf(u32) * layoutSize);
        const fieldSize = 10;
        comptime std.debug.assert(@sizeOf(Type.Record.Field) == @sizeOf(u32) * fieldSize);

        extraIndex += 1; // For declNode
        const castedLayout: *const [layoutSize]u32 = @ptrCast(&recordType.layout);
        ts.extra.items[extraIndex..][0..layoutSize].* = castedLayout.*;
        extraIndex += layoutSize;
        extraIndex += 1; // For field length

        for (recordType.fields) |*field| {
            const casted: *const [fieldSize]u32 = @ptrCast(field);
            ts.extra.items[extraIndex..][0..fieldSize].* = casted.*;
            extraIndex += fieldSize;
        }
    }

    // finish by creating a node
    const cd: Node.ContainerDecl = .{
        .nameOrKindToken = maybeIdent orelse kindToken,
        .containerQt = attributedQt,
        .fields = p.declBuffer.items[declBufferTop..],
    };

    try p.tree.setNode(reservedIndex, if (isStruct) .{ .structDecl = cd } else .{ .unionDecl = cd });
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

            try p.err(.expected_type, p.tokenIdx, .{});
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
    const gpa = p.comp.gpa;

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

                    try p.err(.c23_auto_not_allowed, p.tokenIdx, .{if (p.record.kind == .KeywordStruct) "struct member" else "union member"});
                    try builder.combine(.C23Auto, p.tokenIdx);
                },
                .KeywordAutoType => {
                    try p.err(.auto_type_extension, p.tokenIdx, .{});
                    try p.err(.auto_type_not_allowed, p.tokenIdx, .{if (p.record.kind == .KeywordStruct) "struct member" else "union member"});
                    try builder.combine(.AutoType, p.tokenIdx);
                },
                .Identifier, .ExtendedIdentifier => {
                    if (builder.type != .None) break;
                    try p.err(.unknown_type_name, p.tokenIdx, .{p.getTokenText(p.tokenIdx)});
                    builder.type = .{ .other = .invalid };
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

    var errorOnUnnamed = false;
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
            errorOnUnnamed = true;
        }

        if (p.eat(.Colon)) |_| bits: {
            const bitsToken = p.tokenIdx;
            const res = try p.parseIntegerConstExpr(.GNUFoldingExtension);
            if (!qt.isInt(p.comp)) {
                try p.err(.non_int_bitfield, firstToken, .{qt});
                break :bits;
            }

            if (res.value.isNone()) {
                try p.err(.expected_integer_constant_expr, bitsToken, .{});
                break :bits;
            } else if (res.value.compare(.lt, .zero, p.comp)) {
                try p.err(.negative_bitwidth, firstToken, .{res});
                break :bits;
            }

            // incomplete size error is reported later
            const bitSize = qt.bitSizeofOrNull(p.comp) orelse break :bits;
            const bitsUnchecked = res.value.toInt(u32, p.comp) orelse std.math.maxInt(u32);
            if (bitsUnchecked > bitSize) {
                try p.err(.bitfield_too_big, nameToken, .{});
                break :bits;
            } else if (bitsUnchecked == 0 and nameToken != 0) {
                try p.err(.zero_width_named_field, nameToken, .{});
                break :bits;
            }

            bits = bitsUnchecked;
            bitsNode = res.node;
        }

        try p.parseAttrSpec(); // .record
        const toAppend = try Attribute.applyFieldAttributes(p, &qt, attrBufferTop);

        const attrIndex: u32 = @intCast(p.comp.typeStore.attributes.items.len);
        const attrLen: u32 = @intCast(toAppend.len);
        try p.comp.typeStore.attributes.appendSlice(gpa, toAppend);

        if (nameToken == 0 and bits == null) unnamed: {
            // don't allow incompelete size fields in anonymous record.
            if (!qt.isInvalid()) switch (qt.base(p.comp).type) {
                .@"enum" => break :unnamed,
                .@"struct", .@"union" => |recordTy| if (recordTy.isAnonymous(p.comp)) {
                    // An anonymous record appears as indirect fields on the parent
                    try p.recordBuffer.append(gpa, .{
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
                    try p.declBuffer.append(gpa, node);
                    try p.record.addFieldsFromAnonymous(p, recordTy);
                    break; // must be followed by a semicolon
                },
                else => {},
            };

            if (errorOnUnnamed) {
                try p.err(.expected_member_name, firstToken, .{});
            } else {
                try p.err(.missing_declaration, p.tokenIdx, .{});
            }
            if (p.eat(.Comma) == null) break;
            continue;
        } else {
            const internedName = if (nameToken != 0) try p.getInternString(nameToken) else try p.getAnonymousName(firstToken);
            try p.recordBuffer.append(gpa, .{
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
            try p.declBuffer.append(gpa, node);
        }

        if (!qt.isInvalid()) {
            const fieldType = qt.base(p.comp);
            switch (fieldType.type) {
                .func => {
                    try p.err(.func_field, firstToken, .{});
                    qt = .invalid;
                },
                .array => |arrayTy| switch (arrayTy.len) {
                    .static, .unspecifiedVariable => unreachable,
                    .variable => {
                        try p.err(.vla_field, firstToken, .{});
                        qt = .invalid;
                    },
                    .fixed => {},
                    .incomplete => {
                        if (p.record.kind == .KeywordUnion) {
                            if (p.comp.langOpts.emulate == .msvc) {
                                try p.err(.flexible_in_union_msvc, firstToken, .{});
                            } else {
                                try p.err(.flexible_in_union, firstToken, .{});
                                qt = .invalid;
                            }
                        }
                        if (p.record.flexibleField) |some| {
                            if (p.record.kind == .KeywordStruct) {
                                try p.err(.flexible_non_final, some, .{});
                            }
                        }
                        p.record.flexibleField = firstToken;
                    },
                },
                else => if (fieldType.qt.hasIncompleteSize(p.comp)) {
                    try p.err(.field_incomplete_ty, firstToken, .{qt});
                } else if (p.record.flexibleField) |some| {
                    std.debug.assert(some != firstToken);
                    if (p.record.kind == .KeywordStruct)
                        try p.err(.flexible_non_final, some, .{});
                },
            }
        }

        if (p.eat(.Comma) == null) break;
        errorOnUnnamed = true;
    }

    if (p.eat(.Semicolon) == null) {
        const curToken = p.currToken();
        if (curToken == .RBrace)
            try p.err(.missing_semicolon, p.tokenIdx, .{})
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

fn checkEnumFixedTy(p: *Parser, fixedQt: ?QualType, identToken: TokenIndex, prev: Symbol) !void {
    const enumTy = prev.qt.get(p.comp, .@"enum").?;
    if (fixedQt) |some| {
        if (!enumTy.fixed) {
            try p.err(.enum_prev_nonfixed, identToken, .{});
            try p.err(.previous_definition, prev.token, .{});
            return error.ParsingFailed;
        }

        if (!enumTy.tag.?.eql(some, p.comp)) {
            try p.err(.enum_different_explicit_ty, identToken, .{ some, enumTy.tag.? });
            try p.err(.previous_definition, prev.token, .{});
            return error.ParsingFailed;
        }
    } else if (enumTy.fixed) {
        try p.err(.enum_prev_fixed, identToken, .{});
        try p.err(.previous_definition, prev.token, .{});
        return error.ParsingFailed;
    }
}

/// enum-specifier
///  : `enum` identifier? (':', type-name)? { enumerator (',' enumerator)? ',') }
///  | `enum` identifier (':', type-name)?
fn parseEnumSpec(p: *Parser) Error!QualType {
    const gpa = p.comp.gpa;
    const enumToken = p.tokenIdx;
    p.tokenIdx += 1;

    const attrBufferTop = p.attrBuffer.len;
    defer p.attrBuffer.len = attrBufferTop;

    try p.parseAttrSpec();

    const maybeIdent = try p.eatIdentifier();
    const fixedQt = if (p.eat(.Colon)) |colon| fixed: {
        const tyStart = p.tokenIdx;
        const fixed = (try p.parseSpecQuals()) orelse {
            if (p.record.kind != .Invalid) {
                // This is a bit field.
                p.tokenIdx -= 1;
                break :fixed null;
            }
            try p.err(.expected_type, p.tokenIdx, .{});
            try p.err(.enum_fixed, colon, .{});
            break :fixed null;
        };

        const fixedSK = fixed.scalarKind(p.comp);
        if (fixedSK == .Enum or !fixedSK.isInt()) {
            try p.err(.invalid_type_underlying_enum, tyStart, .{fixed});
            break :fixed null;
        }

        try p.err(.enum_fixed, colon, .{});
        break :fixed fixed;
    } else null;

    const reservedIndex = try p.tree.nodes.addOne(gpa);

    const lb = p.eat(.LBrace) orelse {
        const ident = maybeIdent orelse {
            try p.err(.ident_or_l_brace, p.tokenIdx, .{});
            return error.ParsingFailed;
        };

        // check if this is a reference to a previous type
        const internedName = try p.getInternString(ident);
        if (try p.symStack.findTag(internedName, p.tokenIds[enumToken], ident, p.currToken())) |prev| {
            // only check fixed underlying type in forward declarations and not in references.
            if (p.currToken() == .Semicolon) try p.checkEnumFixedTy(fixedQt, ident, prev);
            return prev.qt;
        } else {
            // this is a forward declaration
            const enumQt = try p.comp.typeStore.put(gpa, .{ .@"enum" = .{
                .name = internedName,
                .tag = fixedQt,
                .fixed = fixedQt != null,
                .incomplete = true,
                .declNode = @enumFromInt(reservedIndex),
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

            try p.declBuffer.append(gpa, try p.addNode(.{
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
                    try p.err(.redefinition, ident, .{identStr});
                    try p.err(.previous_definition, prev.token, .{});
                } else {
                    try p.checkEnumFixedTy(fixedQt, ident, prev);
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
            .declNode = @enumFromInt(reservedIndex),
            .tag = fixedQt,
            .incomplete = true,
            .fixed = fixedQt != null,
            .fields = &.{},
        };
        const enumQt = try p.comp.typeStore.put(gpa, .{ .@"enum" = enumTy });
        break :blk .{ enumTy, enumQt };
    };

    // reserve space for this enum
    try p.declBuffer.append(gpa, @enumFromInt(reservedIndex));
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
        try p.enumBuffer.append(gpa, fieldAndNode.field);
        try p.listBuffer.append(gpa, fieldAndNode.node);
        if (p.eat(.Comma) == null) break;
    }

    if (p.enumBuffer.items.len == enumBufferTop)
        try p.err(.empty_enum, p.tokenIdx, .{});

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
            else if (!res.qt.eql(enumType.tag.?, p.comp))
                enumType.tag.?
            else
                continue;

            const symbol = p.symStack.getPtr(field.name, .vars);
            _ = try symbol.value.intCast(destTy, p.comp);
            try p.tree.valueMap.put(gpa, fieldNode, sym.value);

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
        enumType.declNode = @enumFromInt(reservedIndex);

        const baseType = attributedQt.base(p.comp);
        std.debug.assert(baseType.type.@"enum".name == enumType.name);
        try p.comp.typeStore.set(gpa, .{ .@"enum" = enumType }, @intFromEnum(baseType.qt._index));
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
    try p.tree.setNode(reservedIndex, .{
        .enumDecl = .{
            .nameOrKindToken = maybeIdent orelse enumToken,
            .containerQt = attributedQt,
            .fields = fieldNodes,
        },
    });

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
                try p.err(.enum_not_representable_fixed, token, .{e.qt});
                return;
            }

            if (p.comp.nextLargestIntSameSign(e.qt)) |larger| {
                try p.err(.enumerator_overflow, token, .{});
                e.qt = larger;
            } else {
                const signed = e.qt.signedness(p.comp) == .signed;
                const bitSize = e.qt.bitSizeof(p.comp) - @intFromBool(signed);
                try p.err(.enum_not_representable, token, .{switch (bitSize) {
                    63 => "9223372036854775808",
                    64 => "18446744073709551616",
                    127 => "170141183460469231731687303715884105728",
                    128 => "340282366920938463463374607431768211456",
                    else => unreachable,
                }});
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
                try p.err(.enum_not_representable_fixed, token, .{e.qt});
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

        const charWidth = Type.IntType.SChar.bits(p.comp);
        const shortWidth = Type.IntType.Short.bits(p.comp);
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
                try p.err(.enum_too_large, token, .{});
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
};

const EnumFieldAndNode = struct { field: Type.Enum.Field, node: Node.Index };

/// enumerator : identifier ('=' integer-constant-expression)
fn enumerator(p: *Parser, e: *Enumerator) Error!?EnumFieldAndNode {
    _ = try p.pragma();
    const nameToken = try p.eatIdentifier() orelse {
        if (p.currToken() == .RBrace) return null;
        try p.err(.expected_identifier, p.tokenIdx, .{});
        p.skipTo(.RBrace);
        return error.ParsingFailed;
    };

    const attrBufferTop = p.attrBuffer.len;
    defer p.attrBuffer.len = attrBufferTop;

    try p.parseAttrSpec();

    const prevTotal = p.diagnostics.total;
    const fieldInit = if (p.eat(.Equal)) |_| blk: {
        var specified = try p.parseIntegerConstExpr(.GNUFoldingExtension);
        if (specified.value.isNone()) {
            try p.err(.enum_val_unavailable, nameToken + 2, .{});
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

    if (e.qt.isUnsigned(p.comp) or e.value.compare(.gte, .zero, p.comp))
        e.numPositiveBits = @max(e.numPositiveBits, e.value.minUnsignedBits(p.comp))
    else
        e.numNegativeBits = @max(e.numNegativeBits, e.value.minSignedBits(p.comp));

    if (prevTotal == p.diagnostics.total) {
        // only do these warnings if we didn't already warn about overflow or non-representable values
        if (e.value.compare(.lt, .zero, p.comp)) {
            const minValue = try Value.minInt(.int, p.comp);
            if (e.value.compare(.lt, minValue, p.comp))
                try p.err(.enumerator_too_small, nameToken, .{e});
        } else {
            const maxValue = try Value.maxInt(.int, p.comp);
            if (e.value.compare(.gt, maxValue, p.comp))
                try p.err(.enumerator_too_large, nameToken, .{e});
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

    try p.tree.valueMap.put(p.comp.gpa, node, e.value);

    const internedName = try p.getInternString(nameToken);
    try p.symStack.defineEnumeration(internedName, attributedQt, nameToken, e.value, node);

    return .{
        .field = .{
            .name = internedName,
            .qt = attributedQt,
            .nameToken = nameToken,
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
fn parseTypeQual(p: *Parser, b: *TypeBuilder, allowAttr: bool) Error!bool {
    var any = false;
    while (true) {
        if (allowAttr and try p.msTypeAttribute()) continue;
        if (allowAttr) try p.parseAttrSpec();
        switch (p.currToken()) {
            .KeywordRestrict,
            .KeywordGccRestrict1,
            .KeywordGccRestrict2,
            => {
                if (b.restrict != null)
                    try p.err(.duplicate_decl_spec, p.tokenIdx, .{"restrict"})
                else
                    b.restrict = p.tokenIdx;
            },

            .KeywordConst,
            .KeywordGccConst1,
            .KeywordGccConst2,
            => {
                if (b.@"const" != null)
                    try p.err(.duplicate_decl_spec, p.tokenIdx, .{"const"})
                else
                    b.@"const" = p.tokenIdx;
            },

            .KeywordVolatile, .KeywordGccVolatile1, .KeywordGccVolatile2 => {
                if (b.@"volatile" != null)
                    try p.err(.duplicate_decl_spec, p.tokenIdx, .{"volatile"})
                else
                    b.@"volatile" = p.tokenIdx;
            },

            .KeywordAtomic => {
                // _Atomic(typeName) instead of just _Atomic
                if (p.lookAhead(1) == .LParen) break;
                if (b.atomic != null)
                    try p.err(.duplicate_decl_spec, p.tokenIdx, .{"atomic"})
                else
                    b.atomic = p.tokenIdx;
            },

            .KeywordUnaligned, .KeywordUnaligned2 => {
                if (b.unaligned != null)
                    try p.err(.duplicate_decl_spec, p.tokenIdx, .{"__unaligned"})
                else
                    b.unaligned = p.tokenIdx;
            },

            .KeywordNonnull,
            .KeywordNullable,
            .KeywordNullableResult,
            .KeywordNullUnspecified,
            => |tokenId| {
                const symStr = p.currToken().symbol();
                try p.err(.nullability_extension, p.tokenIdx, .{symStr});

                const new: @FieldType(TypeStore.Builder, "nullability") = switch (tokenId) {
                    .KeywordNonnull => .{ .nonnull = p.tokenIdx },
                    .KeywordNullable => .{ .nullable = p.tokenIdx },
                    .KeywordNullableResult => .{ .nullableResult = p.tokenIdx },
                    .KeywordNullUnspecified => .{ .nullUnspecified = p.tokenIdx },
                    else => unreachable,
                };

                if (std.meta.activeTag(b.nullability) == new) {
                    try p.err(.duplicate_nullability, p.tokenIdx, .{symStr});
                } else switch (b.nullability) {
                    .none => {
                        b.nullability = new;
                        try p.attrBuffer.append(p.comp.gpa, .{
                            .attr = .{
                                .tag = .nullability,
                                .args = .{
                                    .nullability = .{
                                        .kind = switch (tokenId) {
                                            .KeywordNonnull => .nonnull,
                                            .KeywordNullable => .nullable,
                                            .KeywordNullableResult => .nullableResult,
                                            .KeywordNullUnspecified => .unspecified,
                                            else => unreachable,
                                        },
                                    },
                                },
                                .syntax = .keyword,
                            },
                            .tok = p.tokenIdx,
                        });
                    },
                    .nonnull, .nullable, .nullableResult, .nullUnspecified => |prev| {
                        try p.err(.conflicting_nullability, p.tokenIdx, .{ p.currToken(), p.tokenIds[prev] });
                    },
                }
            },

            else => break,
        }
        p.tokenIdx += 1;
        any = true;
    }
    return any;
}

fn msTypeAttribute(p: *Parser) !bool {
    var any = false;
    while (true) {
        switch (p.currToken()) {
            .KeywordStdCall,
            .KeywordStdCall2,
            .KeywordThisCall,
            .KeywordThisCall2,
            .KeywordVectorCall,
            .KeywordVectorCall2,
            .KeywordFastcall,
            .KeywordFastcall2,
            .KeywordRegcall,
            .KeywordCdecl,
            .KeywordCdecl2,
            => {
                try p.attrBuffer.append(p.comp.gpa, .{
                    .attr = .{
                        .tag = .calling_convention,
                        .args = .{
                            .calling_convention = .{
                                .cc = switch (p.currToken()) {
                                    .KeywordStdCall, .KeywordStdCall2 => .stdcall,
                                    .KeywordThisCall, .KeywordThisCall2 => .thiscall,
                                    .KeywordVectorCall, .KeywordVectorCall2 => .vectorcall,
                                    .KeywordFastcall, .KeywordFastcall2 => .fastcall,
                                    .KeywordRegcall => .regcall,
                                    .KeywordCdecl, .KeywordCdecl2 => .c,
                                    else => unreachable,
                                },
                            },
                        },
                        .syntax = .keyword,
                    },
                    .tok = p.tokenIdx,
                });
                any = true;
                p.tokenIdx += 1;
            },
            else => break,
        }
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
        switch (try validateExtra(p, d.qt, sourceToken)) {
            .Normal => return,
            .NestedInvalid => if (d.declaratorType == .func) return,
            .NestedAuto => {
                if (d.declaratorType == .func) return;
                if (d.qt.isAutoType() or d.qt.isC23Auto()) return;
            },
            .DeclaratorCombine => return,
        }
        d.qt = .invalid;
    }

    const ValidationResult = enum {
        NestedInvalid,
        NestedAuto,
        DeclaratorCombine,
        Normal,
    };

    // Returns true if the type contained invalid or auto types.
    fn validateExtra(p: *Parser, cur: QualType, sourceToken: TokenIndex) Parser.Error!ValidationResult {
        if (cur.isInvalid()) return .NestedInvalid;
        if (cur.isAutoType()) return .NestedAuto;
        if (cur.isC23Auto()) return .NestedAuto;
        if (cur._index == .DeclaratorCombine) return .DeclaratorCombine;

        switch (cur.type(p.comp)) {
            .pointer => |pointerTy| return validateExtra(p, pointerTy.child, sourceToken),
            .atomic => |atomicTy| return validateExtra(p, atomicTy, sourceToken),
            .array => |arrayTy| {
                const elemQt = arrayTy.elem;
                const childRes = try validateExtra(p, elemQt, sourceToken);
                if (childRes != .Normal) return childRes;

                if (elemQt.hasIncompleteSize(p.comp)) {
                    try p.err(.array_incomplete_elem, sourceToken, .{elemQt});
                    return .NestedInvalid;
                }

                switch (arrayTy.len) {
                    .fixed, .static => |len| {
                        const elemSize = elemQt.sizeofOrNull(p.comp) orelse 1;
                        const maxElems = p.comp.maxArrayBytes() / @max(1, elemSize);
                        if (len > maxElems) {
                            try p.err(.array_too_large, sourceToken, .{});
                            return .NestedInvalid;
                        }
                    },
                    else => {},
                }

                if (elemQt.is(p.comp, .func)) {
                    try p.err(.array_func_elem, sourceToken, .{});
                    return .NestedInvalid;
                }

                if (elemQt.get(p.comp, .array)) |elemArrayTy| {
                    if (elemArrayTy.len == .static)
                        try p.err(.static_non_outermost_array, sourceToken, .{});

                    if (elemQt.isQualified())
                        try p.err(.qualifier_non_outermost_array, sourceToken, .{});
                }

                return .Normal;
            },
            .func => |funcTy| {
                const retQt = funcTy.returnType;
                const childRes = try validateExtra(p, retQt, sourceToken);
                if (childRes != .Normal) return childRes;

                if (retQt.is(p.comp, .array)) try p.err(.func_cannot_return_array, sourceToken, .{});
                if (retQt.is(p.comp, .func)) try p.err(.func_cannot_return_func, sourceToken, .{});

                if (retQt.@"const") try p.err(.qual_on_ret_type, sourceToken, .{"const"});
                if (retQt.@"volatile") try p.err(.qual_on_ret_type, sourceToken, .{"volatile"});
                if (retQt.get(p.comp, .float)) |float| {
                    if (float == .FP16 and !p.comp.hasHalfPrecisionFloatABI()) {
                        try p.err(.suggest_pointer_for_invalid_fp16, sourceToken, .{"function return value"});
                    }
                }
                return .Normal;
            },
            else => return .Normal,
        }
    }
};

/// declarator: pointer? direct-declarator
///
/// abstract-declarator
///  : pointer
///  | pointer? direct-abstract-declarator
///
/// pointer : '*' type-qualifier * pointer?
fn declarator(p: *Parser, baseQt: QualType, kind: Declarator.Kind) Error!?Declarator {
    var d = Declarator{ .name = 0, .qt = baseQt };

    // Parse potential pointer declarators first.
    while (p.eat(.Asterisk)) |_| {
        d.declaratorType = .pointer;
        var builder: TypeBuilder = .{ .parser = p };
        _ = try p.parseTypeQual(&builder, true);

        const pointerQt = try p.comp.typeStore.put(p.comp.gpa, .{ .pointer = .{
            .child = d.qt,
            .decayed = null,
        } });
        d.qt = try builder.finishQuals(pointerQt);
    }

    const maybeIdent = p.tokenIdx;
    if (kind != .abstract and (try p.eatIdentifier()) != null) {
        d.name = maybeIdent;
        const combineToken = p.tokenIdx;
        d.qt = try p.directDeclarator(&d, kind);
        try d.validate(p, combineToken);
        return d;
    } else if (p.eat(.LParen)) |lp| blk: {
        // c23 abd declspec attributes are not allowed here
        while (try p.gnuAttribute()) {}

        // parse Microsoft-style attributes
        _ = try p.msTypeAttribute();

        const specialMarker: QualType = .{ ._index = .DeclaratorCombine };
        var res = (try p.declarator(specialMarker, kind)) orelse {
            p.tokenIdx = lp;
            break :blk;
        };

        try p.expectClosing(lp, .RParen);
        const suffixStart = p.tokenIdx;
        const outer = try p.directDeclarator(&d, kind);

        // Correct the base type now that it is known.
        // If res.qt is the special marker there was no inner type.
        if (res.qt._index == .DeclaratorCombine) {
            res.qt = outer;
            res.declaratorType = d.declaratorType;
        } else if (outer.isInvalid() or res.qt.isInvalid()) {
            res.qt = outer;
        } else {
            var cur = res.qt;
            while (true) {
                switch (cur.type(p.comp)) {
                    .pointer => |pointerTy| if (pointerTy.child._index != .DeclaratorCombine) {
                        cur = pointerTy.child;
                        continue;
                    },
                    .atomic => |atomicTy| if (atomicTy._index != .DeclaratorCombine) {
                        cur = atomicTy;
                        continue;
                    },
                    .array => |arrayTy| if (arrayTy.elem._index != .DeclaratorCombine) {
                        cur = arrayTy.elem;
                        continue;
                    },
                    .func => |funcTy| if (funcTy.returnType._index != .DeclaratorCombine) {
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

    if (kind == .normal) {
        var cur = d.qt;
        while (true) {
            // QualType.base inlined here because of potential .DeclaratorCombine.
            if (cur._index == .DeclaratorCombine) break;
            switch (cur.type(p.comp)) {
                .typeof => |typeofTy| cur = typeofTy.base,
                .typedef => |typedefTy| cur = typedefTy.base,
                .attributed => |attributedTy| cur = attributedTy.base,
                else => |ty| switch (ty) {
                    .@"enum", .@"struct", .@"union" => break,
                    else => {
                        try p.err(.expected_ident_or_l_paren, expectedIdent, .{});
                        return error.ParsingFailed;
                    },
                },
            }
        }
    }

    try d.validate(p, expectedIdent);
    if (d.qt == baseQt) return null;

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
    const gpa = p.comp.gpa;
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
            try p.err(.expected_expr, p.tokenIdx, .{});
            return error.ParsingFailed;
        }

        var builder: TypeBuilder = .{ .parser = p };
        var gotQuals = try p.parseTypeQual(&builder, false);
        var static = p.eat(.KeywordStatic);

        if (static != null and !gotQuals)
            gotQuals = try p.parseTypeQual(&builder, false);

        var star = p.eat(.Asterisk);
        const sizeToken = p.tokenIdx;

        const constDeclFolding = p.constDeclFolding;
        p.constDeclFolding = .GNUVLAFoldingExtension;
        const optSize = if (star) |_| null else try p.parseAssignExpr();
        p.constDeclFolding = constDeclFolding;

        try p.expectClosing(lb, .RBracket);

        if (star != null and static != null) {
            try p.err(.invalid_static_star, static.?, .{});
        }

        if (kind != .param) {
            if (static != null)
                try p.err(.static_non_param, lb, .{})
            else if (gotQuals)
                try p.err(.array_qualifiers, lb, .{});

            if (star) |some|
                try p.err(.star_non_param, some, .{});

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
            try p.err(.array_size_non_int, sizeToken, .{optSize.?.qt});
            return error.ParsingFailed;
        }

        if (optSize) |size| {
            if (size.value.isNone()) {
                try p.err(.vla, sizeToken, .{});
                if (p.func.qt == null and kind != .param and p.record.kind == .Invalid)
                    try p.err(.variable_len_array_file_scope, baseDeclarator.name, .{});

                const arrayQt = try p.comp.typeStore.put(gpa, .{ .array = .{
                    .elem = outer,
                    .len = .{ .variable = size.node },
                } });

                if (static) |some| try p.err(.useless_static, some, .{});
                return builder.finishQuals(arrayQt);
            } else {
                if (size.value.isZero(p.comp)) {
                    try p.err(.zero_length_array, lb, .{});
                } else if (size.value.compare(.lt, .zero, p.comp)) {
                    try p.err(.negative_array_size, lb, .{});
                    return error.ParsingFailed;
                }

                const len = size.value.toInt(u64, p.comp) orelse std.math.maxInt(u64);
                const arrayQt = try p.comp.typeStore.put(gpa, .{ .array = .{
                    .elem = outer,
                    .len = if (static != null)
                        .{ .static = len }
                    else
                        .{ .fixed = len },
                } });
                return builder.finishQuals(arrayQt);
            }
        } else if (star) |_| {
            const arrayQt = try p.comp.typeStore.put(gpa, .{ .array = .{
                .elem = outer,
                .len = .unspecifiedVariable,
            } });
            return builder.finishQuals(arrayQt);
        } else {
            const arrayQt = try p.comp.typeStore.put(gpa, .{ .array = .{
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
            try p.err(.param_before_var_args, p.tokenIdx, .{});
            try p.expectClosing(lp, .RParen);

            funcType.kind = .Variadic;
            funcType.returnType = try p.directDeclarator(baseDeclarator, kind);

            // Set after call to `directDeclarator` since we will return
            // a function type from here.
            baseDeclarator.declaratorType = .func;
            return p.comp.typeStore.put(gpa, .{ .func = funcType });
        }

        // Set here so the call to directDeclarator for the return type
        // doesn't clobber this function type's parameters.
        const paramBufferTop = p.paramBuffer.items.len;
        defer p.paramBuffer.items.len = paramBufferTop;

        if (try p.parseParamDecls()) |params| {
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
                try p.paramBuffer.append(gpa, .{
                    .name = internedName,
                    .nameToken = nameToken,
                    .qt = .int,
                    .node = .null,
                });

                if (p.eat(.Comma) == null) break;
            }

            funcType.params = p.paramBuffer.items[paramBufferTop..];
        } else {
            try p.err(.expected_param_decl, p.tokenIdx, .{});
        }

        try p.expectClosing(lp, .RParen);

        funcType.returnType = try p.directDeclarator(baseDeclarator, kind);

        // Set after call to `directDeclarator` since we will return
        // a function type from here.
        baseDeclarator.declaratorType = .func;

        return p.comp.typeStore.put(gpa, .{ .func = funcType });
    } else {
        return baseDeclarator.qt;
    }
}

/// param-decls : param-decl (',' param-decl)* (',' '...')
/// paramDecl : decl-specifier (declarator | abstract-declarator)
fn parseParamDecls(p: *Parser) Error!?[]Type.Func.Param {
    try p.symStack.pushScope();
    defer p.symStack.popScope();

    const paramBufferTop = p.paramBuffer.items.len;
    const gpa = p.comp.gpa;

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
            try p.err(.unknown_type_name, identifier, .{p.getTokenText(identifier)});

            try p.paramBuffer.append(gpa, .{
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
            try p.err(.missing_type_specifier, p.tokenIdx, .{});
            break :blk DeclSpec{ .qt = .int };
        };

        var nameToken: TokenIndex = 0;
        var internedName: StringId = .empty;
        const firstToken = p.tokenIdx;
        var paramQt = paramDeclSpec.qt;

        if (paramDeclSpec.autoType) |tokenIndex| {
            try p.err(.auto_type_not_allowed, tokenIndex, .{"function prototype"});
            paramQt = .invalid;
        }

        if (paramDeclSpec.c23Auto) |tokenIndex| {
            try p.err(.c23_auto_not_allowed, tokenIndex, .{"function prototype"});
            paramQt = .invalid;
        }

        if (try p.declarator(paramQt, .param)) |some| {
            if (some.oldTypeFunc) |tokenIdx|
                try p.err(.invalid_old_style_params, tokenIdx, .{});

            try p.parseAttrSpec();
            nameToken = some.name;
            paramQt = some.qt;
        }

        if (paramQt.is(p.comp, .void)) {
            // validate void parameters
            if (p.paramBuffer.items.len == paramBufferTop) {
                if (p.currToken() != .RParen) {
                    try p.err(.void_only_param, p.tokenIdx, .{});
                    if (paramQt.isQualified()) try p.err(.void_param_qualified, p.tokenIdx, .{});
                    return error.ParsingFailed;
                }
                return &.{};
            }

            try p.err(.void_must_be_first_param, p.tokenIdx, .{});
            return error.ParsingFailed;
        } else {
            // Decay params declared as functions or arrays to pointer.
            paramQt = try paramQt.decay(p.comp);
        }

        try paramDeclSpec.validateParam(p);
        paramQt = try Attribute.applyParameterAttributes(p, paramQt, attrBufferTop, .alignas_on_param);

        if (paramQt.get(p.comp, .float)) |float| {
            if (float == .FP16 and !p.comp.hasHalfPrecisionFloatABI()) {
                try p.err(.suggest_pointer_for_invalid_fp16, firstToken, .{"parameters"});
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

        try p.paramBuffer.append(gpa, .{
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
            try p.err(.invalid_old_style_params, tokenIdx, .{});
        return try Attribute.applyTypeAttributes(p, some.qt, attrBufferTop, .align_ignored);
    }
    return try Attribute.applyTypeAttributes(p, ty, attrBufferTop, .align_ignored);
}

/// initializer : assign-expression
///             | braced-initializer
///
/// braced-initializer : '{' initializerItems '}'
pub fn initializer(p: *Parser, initQt: QualType) Error!Result {
    const lbrace = p.eat(.LBrace) orelse {
        // fast path for non-braced initializers
        const token = p.tokenIdx;
        var res = try p.expect(parseAssignExpr);
        if (try p.coerceArrayInit(res, token, initQt))
            return res;

        try p.coerceInit(&res, token, initQt);
        return res;
    };

    // We want to parse the initializer even if the target is
    // invalidly inferred.
    var finalInitQt = initQt;
    if (initQt.isAutoType()) {
        try p.err(.auto_type_with_init_list, lbrace, .{});
        finalInitQt = .invalid;
    } else if (initQt.isC23Auto()) {
        try p.err(.c23_auto_with_init_list, lbrace, .{});
        finalInitQt = .invalid;
    }

    var il: InitList = .{};
    defer il.deinit(p.comp.gpa);

    try p.initializerItem(&il, finalInitQt, lbrace);

    const listNode = try p.convertInitList(il, finalInitQt);
    return .{
        .qt = listNode.qt(&p.tree).withQualifiers(finalInitQt),
        .node = listNode,
        .value = p.tree.valueMap.get(listNode) orelse .{},
    };
}

fn setInitializer(
    p: *Parser,
    il: *InitList,
    initQt: QualType,
    token: TokenIndex,
    res: Result,
) !void {
    var copy = res;

    const arr = try p.coerceArrayInit(copy, token, initQt);
    if (!arr) try p.coerceInit(&copy, token, initQt);

    if (il.tok != 0 and !initQt.isInvalid()) {
        try p.err(.initializer_overrides, token, .{});
        try p.err(.previous_initializer, il.tok, .{});
    }
    il.node = .pack(copy.node);
    il.tok = token;
}

fn setInitializerIfEqual(
    p: *Parser,
    il: *InitList,
    initQt: QualType,
    tok: TokenIndex,
    res: Result,
) !bool {
    if (!res.qt.eql(initQt, p.comp)) return false;
    try p.setInitializer(il, initQt, tok, res);
    return true;
}

const IndexList = std.ArrayListUnmanaged(u64);

/// initializerItems : designation? initializer (',' designation? initializer)* ','?
pub fn initializerItem(
    p: *Parser,
    il: *InitList,
    initQt: QualType,
    lbrace: TokenIndex,
) Error!void {
    const comp = p.comp;
    const gpa = comp.gpa;

    const isScalar = !initQt.isInvalid() and initQt.scalarKind(comp) != .None;

    if (p.eat(.RBrace)) |_| {
        try p.err(.empty_initializer, lbrace, .{});

        if (il.tok != 0 and !initQt.isInvalid()) {
            try p.err(.initializer_overrides, lbrace, .{});
            try p.err(.previous_initializer, il.tok, .{});
        }
        il.node = .null;
        il.tok = lbrace;
        return;
    }

    var indexList: IndexList = .empty;
    defer indexList.deinit(gpa);

    var seenAny = false;
    var warnedExcess = initQt.isInvalid();
    while (true) : (seenAny = true) {
        errdefer p.skipTo(.RBrace);

        const designated = try p.designation(il, initQt, &indexList);
        if (!designated and initQt.hasAttribute(comp, .designated_init)) {
            try p.err(.designated_init_needed, p.tokenIdx, .{});
        }

        const firstToken = p.tokenIdx;
        if (p.eat(.LBrace)) |innerLBrace| {
            if (try p.findBracedInitiailzer(il, initQt, firstToken, &indexList)) |item| {
                if (item.il.tok != 0 and !initQt.isInvalid()) {
                    try p.err(.initializer_overrides, firstToken, .{});
                    try p.err(.previous_initializer, item.il.tok, .{});
                    item.il.deinit(gpa);
                    item.il.* = .{};
                }
                try p.initializerItem(item.il, item.qt, innerLBrace);
            } else {
                // discard further values
                var tempIL: InitList = .{};
                defer tempIL.deinit(gpa);

                try p.initializerItem(&tempIL, .invalid, innerLBrace);
                if (!warnedExcess) try p.err(switch (initQt.base(comp).type) {
                    .array => if (il.node != .null and p.isStringInit(initQt, il.node.unpack().?))
                        .excess_str_init
                    else
                        .excess_array_init,
                    .@"struct" => .excess_struct_init,
                    .@"union" => .excess_union_init,
                    .vector => .excess_vector_init,
                    else => .excess_scalar_init,
                }, firstToken, .{});

                warnedExcess = true;
            }
        } else if (try p.parseAssignExpr()) |res| {
            if (isScalar and il.node != .null) {
                if (!warnedExcess) try p.err(.excess_scalar_init, firstToken, .{});
                warnedExcess = true;
            } else {
                _ = try p.findScalarInitializer(il, initQt, res, firstToken, &warnedExcess, &indexList, 0);
            }
        } else if (designated or (seenAny and p.currToken() != .RBrace)) {
            try p.err(.expected_expr, p.tokenIdx, .{});
        } else break;

        if (p.eat(.Comma) == null) break;
    }
    try p.expectClosing(lbrace, .RBrace);

    if (il.tok == 0) il.tok = lbrace;
}

/// designation : designator-list '='
///
/// designator-list : designator designator-list designator
///
/// designator : '[' integer-constant-expression ']'
///            | '.' identifier
fn designation(
    p: *Parser,
    il: *InitList,
    initQt: QualType,
    indexList: *IndexList,
) !bool {
    switch (p.currToken()) {
        .LBracket, .Period => indexList.items.len = 0,
        else => return false,
    }

    const comp = p.comp;
    const gpa = comp.gpa;

    var curQt = initQt;
    var curIL = il;
    while (true) {
        if (p.eat(.LBracket)) |lbr| {
            const arrayTy = curQt.get(comp, .array) orelse {
                try p.err(.invalid_array_designator, lbr, .{curQt});
                return error.ParsingFailed;
            };

            const exprToken = p.tokenIdx;
            const indexRes = try p.parseIntegerConstExpr(.GNUFoldingExtension);
            try p.expectClosing(lbr, .RBracket);

            if (curQt.isInvalid()) continue;

            if (indexRes.value.isNone()) {
                try p.err(.expected_integer_constant_expr, exprToken, .{});
                return error.ParsingFailed;
            } else if (indexRes.value.compare(.lt, .zero, comp)) {
                try p.err(.negative_array_designator, lbr + 1, .{indexRes});
                return error.ParsingFailed;
            }

            const maxLen = switch (arrayTy.len) {
                .fixed, .static => |len| len,
                else => std.math.maxInt(u64),
            };
            const indexInt = indexRes.value.toInt(u64, comp) orelse std.math.maxInt(u64);
            if (indexInt >= maxLen) {
                try p.err(.oob_array_designator, lbr + 1, .{indexRes});
                return error.ParsingFailed;
            }

            try indexList.append(gpa, indexInt);
            curIL = try curIL.find(gpa, indexInt);
            curQt = arrayTy.elem;
        } else if (p.eat(.Period)) |period| {
            const fieldToken = try p.expectIdentifier();
            if (curQt.isInvalid()) continue;

            const fieldStr = p.getTokenText(fieldToken);
            const targetName = try p.comp.internString(fieldStr);

            var recordTy = curQt.getRecord(comp) orelse {
                try p.err(.invalid_field_designator, period, .{curQt});
                return error.ParsingFailed;
            };

            var fieldIndex: u32 = 0;

            while (fieldIndex < recordTy.fields.len) {
                const f = recordTy.fields[fieldIndex];
                if (f.nameToken == 0) {
                    if (f.qt.getRecord(p.comp)) |rec| {
                        // Recurse into anonymous field if it has a field by the name.
                        if (!rec.hasField(comp, targetName)) continue;

                        try indexList.append(gpa, fieldIndex);
                        curIL = try il.find(gpa, fieldIndex);
                        recordTy = rec;
                        fieldIndex = 0;
                        continue;
                    }
                }

                if (f.name == targetName) {
                    curQt = f.qt;
                    try indexList.append(gpa, fieldIndex);
                    curIL = try curIL.find(gpa, fieldIndex);
                    break;
                }
                fieldIndex += 1;
            } else {
                try p.err(.no_such_field_designator, period, .{fieldStr});
                return error.ParsingFailed;
            }
        } else break;
    }
    if (p.eat(.Equal) == null) {
        try p.err(.gnu_missing_eq_designator, p.tokenIdx, .{});
    }
    return true;
}

/// Returns true if the value is unused.
fn findScalarInitializer(
    p: *Parser,
    il: *InitList,
    qt: QualType,
    res: Result,
    firstToken: TokenIndex,
    warnedExcess: *bool,
    indexList: *IndexList,
    indexListTop: u32,
) Error!bool {
    if (qt.isInvalid()) return false;

    const comp = p.comp;
    const gpa = comp.gpa;

    if (indexList.items.len <= indexListTop) try indexList.append(gpa, 0);
    const index = indexList.items[indexListTop];

    switch (qt.base(p.comp).type) {
        .complex => |complexTy| {
            if (il.node != .null or index >= 2) {
                if (!warnedExcess.*) try p.err(.excess_scalar_init, firstToken, .{});
                warnedExcess.* = true;
                return true;
            }

            if (res.qt.eql(qt, comp) and il.list.items.len == 0) {
                try p.setInitializer(il, qt, firstToken, res);
                return true;
            }

            const elemIL = try il.find(gpa, index);
            if (try p.setInitializerIfEqual(elemIL, complexTy, firstToken, res) or
                try p.findScalarInitializer(
                    elemIL,
                    complexTy,
                    res,
                    firstToken,
                    warnedExcess,
                    indexList,
                    indexListTop + 1,
                ))
            {
                const newIndex = index + 1;
                indexList.items[indexListTop] = newIndex;
                indexList.items.len = indexListTop + 1;
                return newIndex >= 2;
            }

            return false;
        },
        .vector => |vectorTy| {
            if (il.node != .null or index >= vectorTy.len) {
                if (!warnedExcess.*) try p.err(.excess_vector_init, firstToken, .{});
                warnedExcess.* = true;
                return true;
            }

            if (il.list.items.len == 0 and
                (res.qt.eql(qt, p.comp) or (res.qt.is(p.comp, .vector) and res.qt.sizeCompare(qt, p.comp) == .eq)))
            {
                try p.setInitializer(il, qt, firstToken, res);
                return true;
            }

            const elemIL = try il.find(gpa, index);
            if (try p.setInitializerIfEqual(elemIL, vectorTy.elem, firstToken, res) or
                try p.findScalarInitializer(
                    elemIL,
                    vectorTy.elem,
                    res,
                    firstToken,
                    warnedExcess,
                    indexList,
                    indexListTop + 1,
                ))
            {
                const newIndex = index + 1;
                indexList.items[indexListTop] = newIndex;
                indexList.items.len = indexListTop + 1;
                return newIndex >= vectorTy.len;
            }

            return false;
        },
        .array => |arrayTy| {
            const maxLen = switch (arrayTy.len) {
                .fixed, .static => |len| len,
                else => std.math.maxInt(u64),
            };
            if (maxLen == 0) {
                try p.err(.empty_aggregate_init_braces, firstToken, .{});
                return true;
            }

            if (il.node != .null or index >= maxLen) {
                if (!warnedExcess.*) {
                    if (il.node.unpack()) |some|
                        if (p.isStringInit(qt, some)) {
                            try p.err(.excess_str_init, firstToken, .{});
                            warnedExcess.* = true;
                            return true;
                        };
                    try p.err(.excess_array_init, firstToken, .{});
                }
                warnedExcess.* = true;
                return true;
            }

            if (il.list.items.len == 0 and
                p.isStringInit(qt, res.node) and
                try p.coerceArrayInit(res, firstToken, qt))
            {
                try p.setInitializer(il, qt, firstToken, res);
                return true;
            }

            const elemIL = try il.find(gpa, index);
            if (try p.setInitializerIfEqual(elemIL, arrayTy.elem, firstToken, res) or
                try p.findScalarInitializer(
                    elemIL,
                    arrayTy.elem,
                    res,
                    firstToken,
                    warnedExcess,
                    indexList,
                    indexListTop + 1,
                ))
            {
                const newIndex = index + 1;
                indexList.items[indexListTop] = newIndex;
                indexList.items.len = indexListTop + 1;
                return newIndex >= maxLen;
            }

            return false;
        },

        .@"struct" => |structTy| {
            if (structTy.fields.len == 0) {
                try p.err(.empty_aggregate_init_braces, firstToken, .{});
                return true;
            }

            if (il.node != .null or index >= structTy.fields.len) {
                if (!warnedExcess.*) try p.err(.excess_struct_init, firstToken, .{});
                warnedExcess.* = true;
                return true;
            }

            const field = structTy.fields[@intCast(index)];
            const fieldIL = try il.find(gpa, index);
            if (try p.setInitializerIfEqual(fieldIL, field.qt, firstToken, res) or
                try p.findScalarInitializer(
                    fieldIL,
                    field.qt,
                    res,
                    firstToken,
                    warnedExcess,
                    indexList,
                    indexListTop + 1,
                ))
            {
                const newIndex = index + 1;
                indexList.items[indexListTop] = newIndex;
                indexList.items.len = indexListTop + 1;
                return newIndex >= structTy.fields.len;
            }

            return false;
        },

        .@"union" => |unionTy| {
            if (unionTy.fields.len == 0) {
                try p.err(.empty_aggregate_init_braces, firstToken, .{});
                return true;
            }

            if (il.node != .null or il.list.items.len > 1 or
                (il.list.items.len == 1 and il.list.items[0].index != index))
            {
                if (!warnedExcess.*) try p.err(.excess_union_init, firstToken, .{});
                warnedExcess.* = true;
                return true;
            }

            const field = unionTy.fields[@intCast(index)];
            const fieldIL = try il.find(gpa, index);
            if (try p.setInitializerIfEqual(fieldIL, field.qt, firstToken, res) or
                try p.findScalarInitializer(
                    fieldIL,
                    field.qt,
                    res,
                    firstToken,
                    warnedExcess,
                    indexList,
                    indexListTop + 1,
                ))
            {
                const newIndex = index + 1;
                indexList.items[indexListTop] = newIndex;
                indexList.items.len = indexListTop + 1;
            }

            return true;
        },

        else => {
            try p.setInitializer(il, qt, firstToken, res);
            return true;
        },
    }
}

const InitItem = struct { il: *InitList, qt: QualType };

fn findBracedInitiailzer(
    p: *Parser,
    il: *InitList,
    qt: QualType,
    firstToken: TokenIndex,
    indexList: *IndexList,
) Error!?InitItem {
    if (qt.isInvalid()) {
        if (il.node != .null) return .{ .il = il, .qt = qt };
        return null;
    }

    const gpa = p.comp.gpa;

    if (indexList.items.len == 0) try indexList.append(gpa, 0);
    const index = indexList.items[0];

    switch (qt.base(p.comp).type) {
        .complex => |complexTy| {
            if (il.node != .null) return null;

            if (index < 2) {
                indexList.items[0] = index + 1;
                indexList.items.len = 1;
                return .{ .il = try il.find(gpa, index), .qt = complexTy };
            }
        },
        .vector => |vectorTy| {
            if (il.node != .null) return null;

            if (index < vectorTy.len) {
                indexList.items[0] = index + 1;
                indexList.items.len = 1;
                return .{ .il = try il.find(gpa, index), .qt = vectorTy.elem };
            }
        },
        .array => |arrayTy| {
            if (il.*.node != .null) return null;

            const maxLen = switch (arrayTy.len) {
                .fixed, .static => |len| len,
                else => std.math.maxInt(u64),
            };

            if (index < maxLen) {
                indexList.items[0] = index + 1;
                indexList.items.len = 1;
                return .{ .il = try il.find(gpa, index), .qt = arrayTy.elem };
            }
        },
        .@"struct" => |structTy| {
            if (il.node != .null) return null;

            if (index < structTy.fields.len) {
                indexList.items[0] = index + 1;
                indexList.items.len = 1;
                const fieldQt = structTy.fields[@intCast(index)].qt;
                return .{ .il = try il.find(gpa, index), .qt = fieldQt };
            }
        },
        .@"union" => |unionTy| {
            if (il.node != .null) return null;
            if (unionTy.fields.len == 0) return null;

            if (index < unionTy.fields.len) {
                indexList.items[0] = index + 1;
                indexList.items.len = 1;
                const fieldQt = unionTy.fields[@intCast(index)].qt;
                return .{ .il = try il.find(gpa, 0), .qt = fieldQt };
            }
        },

        else => {
            try p.err(.too_many_scalar_init_braces, firstToken, .{});
            if (il.node == .null) return .{ .il = il, .qt = qt };
        },
    }
    return null;
}

fn coerceArrayInit(p: *Parser, item: Result, token: TokenIndex, target: QualType) !bool {
    if (target.isInvalid()) return false;
    const targetArrayTy = target.get(p.comp, .array) orelse return false;

    const isStrLiteral = p.nodeIs(item.node, .stringLiteralExpr);
    const maybeItemArrayTy = item.qt.get(p.comp, .array);
    if (!isStrLiteral and !p.nodeIs(item.node, .compoundLiteralExpr) or maybeItemArrayTy == null) {
        try p.err(.array_init_str, token, .{});
        return true; // do not do further coercion
    }

    const targetElem: QualType = targetArrayTy.elem;
    const itemElem: QualType = maybeItemArrayTy.?.elem;

    // not int; string compat checks below will fail by design
    const targetInt = targetElem.get(p.comp, .int) orelse .Int;
    const itemInt = itemElem.get(p.comp, .int) orelse .Int;

    const compatible = targetElem.eql(itemElem, p.comp) or
        (isStrLiteral and itemInt == .Char and (targetInt == .UChar or targetInt == .SChar)) or
        (isStrLiteral and itemInt == .UChar and (targetInt == .UChar or targetInt == .SChar or targetInt == .Char));

    if (!compatible) {
        try p.err(.incompatible_array_init, token, .{ target, item.qt });
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
            if (itemLen - 1 > targetLen)
                try p.err(.str_init_too_long, token, .{});
        } else if (itemLen > targetLen) {
            try p.err(.arr_init_too_long, token, .{ target, item.qt });
        }
    }
    return true;
}

fn coerceInit(p: *Parser, item: *Result, token: TokenIndex, target: QualType) !void {
    if (target.isInvalid()) return;

    const node = item.node;
    if (target.isAutoType() or target.isC23Auto()) {
        if (p.getNode(node, .memberAccessExpr) orelse p.getNode(node, .memberAccessPtrExpr)) |access| {
            if (access.isBitFieldWidth(&p.tree) != null) try p.err(.auto_type_from_bitfield, token, .{});
        }
        try item.lvalConversion(p, token);
        return;
    }

    try item.coerce(p, target, token, .init);
    if (item.value.isNone()) runtime: {
        const diag: Diagnostic = switch (p.initContext) {
            .runtime => break :runtime,
            .constexpr => .constexpr_requires_const,
            .static => break :runtime,
        };

        p.initContext = .runtime;
        try p.err(diag, token, .{});
    }

    if (target.@"const" or p.initContext == .constexpr) {
        return item.putValue(p);
    }
    return item.saveValue(p);
}

fn isStringInit(p: *Parser, initQt: QualType, node: Node.Index) bool {
    const initArrayTy = initQt.get(p.comp, .array) orelse return false;
    if (!initArrayTy.elem.is(p.comp, .int)) return false;

    return p.nodeIs(node, .stringLiteralExpr);
}

/// Convert InitList into an AST
fn convertInitList(p: *Parser, il: InitList, initQt: QualType) Error!Node.Index {
    if (initQt.isInvalid()) {
        return try p.addNode(.{ .defaultInitExpr = .{
            .lastToken = p.tokenIdx,
            .qt = initQt,
        } });
    }

    if (il.node.unpack()) |some| return some;

    const comp = p.comp;
    const gpa = comp.gpa;
    switch (initQt.base(comp).type) {
        .complex => |complexTy| {
            if (il.list.items.len == 0)
                return try p.addNode(.{ .defaultInitExpr = .{
                    .lastToken = p.tokenIdx - 1,
                    .qt = initQt,
                } });

            const first = try p.convertInitList(il.list.items[0].list, complexTy);
            const second = if (il.list.items.len > 1)
                try p.convertInitList(il.list.items[1].list, complexTy)
            else
                null;

            if (il.list.items.len == 2) {
                try p.err(.complex_component_init, il.tok, .{});
            }

            const node = try p.addNode(.{
                .arrayInitExpr = .{
                    .containerQt = initQt,
                    .items = if (second) |some| &.{ first, some } else &.{first},
                    .lbraceToken = il.tok,
                },
            });
            if (!complexTy.isFloat(comp)) return node;

            const firtstNode = il.list.items[0].list.node.unpack() orelse return node;
            const secondNode = if (il.list.items.len > 1) il.list.items[1].list.node else .null;

            const firstValue = p.tree.valueMap.get(firtstNode) orelse return node;
            const secondValue = if (secondNode.unpack()) |some| p.tree.valueMap.get(some) orelse return node else Value.zero;

            const complexValue = try Value.intern(p.comp, switch (complexTy.bitSizeof(comp)) {
                32 => .{ .complex = .{ .cf32 = .{ firstValue.toFloat(f32, comp), secondValue.toFloat(f32, p.comp) } } },
                64 => .{ .complex = .{ .cf64 = .{ firstValue.toFloat(f64, comp), secondValue.toFloat(f64, p.comp) } } },
                80 => .{ .complex = .{ .cf80 = .{ firstValue.toFloat(f80, comp), secondValue.toFloat(f80, p.comp) } } },
                128 => .{ .complex = .{ .cf128 = .{ firstValue.toFloat(f128, comp), secondValue.toFloat(f128, p.comp) } } },
                else => unreachable,
            });
            try p.tree.valueMap.put(gpa, node, complexValue);
            return node;
        },
        .vector => |vectorTy| {
            const listBufferTop = p.listBuffer.items.len;
            defer p.listBuffer.items.len = listBufferTop;

            const elemTy = initQt.childType(comp);
            const maxLen = vectorTy.len;

            var start: u64 = 0;
            for (il.list.items) |*init| {
                if (init.index > start) {
                    const elem = try p.addNode(.{
                        .arrayFillerExpr = .{
                            .lastToken = p.tokenIdx - 1,
                            .count = init.index - start,
                            .qt = elemTy,
                        },
                    });
                    try p.listBuffer.append(gpa, elem);
                }
                start = init.index + 1;

                const elem = try p.convertInitList(init.list, elemTy);
                try p.listBuffer.append(gpa, elem);
            }

            if (start < maxLen) {
                const elem = try p.addNode(.{
                    .arrayFillerExpr = .{
                        .lastToken = p.tokenIdx - 1,
                        .count = maxLen - start,
                        .qt = elemTy,
                    },
                });
                try p.listBuffer.append(gpa, elem);
            }

            return p.addNode(.{ .arrayInitExpr = .{
                .lbraceToken = il.tok,
                .containerQt = initQt,
                .items = p.listBuffer.items[listBufferTop..],
            } });
        },
        .array => |arrayTy| {
            const listBuffTop = p.listBuffer.items.len;
            defer p.listBuffer.items.len = listBuffTop;

            const elemType = initQt.childType(comp);
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
                    try p.listBuffer.append(gpa, elem);
                }
                start = init.index + 1;

                const elem = try p.convertInitList(init.list, elemType);
                try p.listBuffer.append(gpa, elem);
            }

            const maxElems = comp.maxArrayBytes() / (@max(1, elemType.sizeofOrNull(comp) orelse 1));
            if (start > maxElems) {
                try p.err(.array_too_large, il.tok, .{});
                start = maxElems;
            }

            var arrInitTy = initQt;
            if (arrayTy.len == .incomplete) {
                arrInitTy = try p.comp.typeStore.put(gpa, .{ .array = .{
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
                try p.listBuffer.append(gpa, elem);
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

            const listBuffTop = p.listBuffer.items.len;
            defer p.listBuffer.items.len = listBuffTop;

            var initIndex: usize = 0;
            for (structTy.fields, 0..) |field, i| {
                if (initIndex < il.list.items.len and il.list.items[initIndex].index == i) {
                    const item = try p.convertInitList(il.list.items[initIndex].list, field.qt);
                    try p.listBuffer.append(gpa, item);
                    initIndex += 1;
                } else {
                    const item = try p.addNode(.{ .defaultInitExpr = .{ .lastToken = il.tok, .qt = field.qt } });
                    try p.listBuffer.append(gpa, item);
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
    const gpa = p.comp.gpa;
    if (!p.currToken().isStringLiteral() and p.currToken() != .LBracket) {
        // Empty
        return;
    }

    while (true) {
        if (p.eat(.LBracket)) |lbracket| {
            const ident = (try p.eatIdentifier()) orelse {
                try p.err(.expected_identifier, p.tokenIdx, .{});
                return error.ParsingFailed;
            };
            try names.append(gpa, ident);
            try p.expectClosing(lbracket, .RBracket);
        } else {
            try names.append(gpa, null);
        }

        const constraint = try p.parseAsmString();
        try constraints.append(gpa, constraint.node);

        const lparen = p.eat(.LParen) orelse {
            try p.err(.expected_token, p.tokenIdx, .{ p.currToken(), TokenType.LParen });
            return error.ParsingFailed;
        };
        const maybeRes = try p.parseExpr();
        try p.expectClosing(lparen, .RParen);
        const res = try p.expectResult(maybeRes);
        try exprs.append(gpa, res.node);
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
    const gpa = p.comp.gpa;
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

    var stackFallback = std.heap.stackFallback(bytesNeeded, gpa);
    const allocator = stackFallback.get();

    var names: std.ArrayList(?TokenIndex) = .empty;
    var constraints: NodeList = .empty;
    var clobbers: NodeList = .empty;
    var exprs: NodeList = .empty;

    names.ensureUnusedCapacity(allocator, expectedItems) catch unreachable; // stack allocation already succeeded
    constraints.ensureUnusedCapacity(allocator, expectedItems) catch unreachable; // stack allocation already succeeded
    exprs.ensureUnusedCapacity(allocator, expectedItems) catch unreachable; //stack allocation already succeeded
    clobbers.ensureUnusedCapacity(allocator, expectedItems) catch unreachable; //stack allocation already succeeded

    defer {
        names.deinit(allocator);
        constraints.deinit(allocator);
        exprs.deinit(allocator);
        clobbers.deinit(allocator);
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
                try clobbers.append(allocator, clobber.node);
                if (p.eat(.Comma) == null) break;
            }
        }
    }

    if (!quals.goto and (p.currToken() != .RParen or ateExtraColor)) {
        try p.err(.expected_token, p.tokenIdx, .{ TokenType.RParen, p.currToken() });
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
                try p.err(.expected_identifier, p.tokenIdx, .{});
                return error.ParsingFailed;
            };
            const identStr = p.getTokenText(ident);
            const label = p.findLabel(identStr) orelse blk: {
                try p.labels.append(gpa, .{ .unresolvedGoto = ident });
                break :blk ident;
            };
            try names.append(allocator, ident);

            const labelAddrNode = try p.addNode(.{
                .addrOfLabel = .{
                    .labelToken = label,
                    .qt = .voidPointer,
                },
            });
            try exprs.append(allocator, labelAddrNode);

            numLabels += 1;
            if (p.eat(.Comma) == null) break;
        }
    } else if (quals.goto) {
        try p.err(.expected_token, p.tokenIdx, .{ TokenType.Colon, p.currToken() });
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
            try p.err(.gnu_asm_disabled, tok, .{});
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
            try p.err(.extension_token_used, p.tokenIdx, .{});
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
            if (kind != .stmt) try p.err(.meaningless_asm_qual, p.tokenIdx, .{"volatile"});
            if (quals.@"volatile") try p.err(.duplicate_asm_qual, p.tokenIdx, .{"volatile"});
            quals.@"volatile" = true;
        },
        .KeywordInline, .KeywordGccInline1, .KeywordGccInline2 => {
            if (kind != .stmt) try p.err(.meaningless_asm_qual, p.tokenIdx, .{"inline"});
            if (quals.@"inline") try p.err(.duplicate_asm_qual, p.tokenIdx, .{"inline"});
            quals.@"inline" = true;
        },
        .KeywordGoto => {
            if (kind != .stmt) try p.err(.meaningless_asm_qual, p.tokenIdx, .{"goto"});
            if (quals.goto) try p.err(.duplicate_asm_qual, p.tokenIdx, .{"goto"});
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
            try p.attrBuffer.append(p.comp.gpa, .{ .attr = attr, .tok = asmToken });
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
            try p.err(.invalid_asm_str, p.tokenIdx, .{"unicode"});
            return error.ParsingFailed;
        },
        .StringLiteralWide => {
            try p.err(.invalid_asm_str, p.tokenIdx, .{"wide"});
            return error.ParsingFailed;
        },
        else => {
            if (i == p.tokenIdx) {
                try p.err(.expected_str_literal_in, p.tokenIdx, .{"asm"});
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
        if (!p.inLoop) try p.err(.continue_not_in_loop, cont, .{});
        _ = try p.expectToken(.Semicolon);
        return try p.addNode(.{ .continueStmt = .{ .continueToken = cont } });
    }

    if (p.eat(.KeywordBreak)) |br| {
        if (!p.inLoop and p.@"switch" == null) try p.err(.break_not_in_loop_or_switch, br, .{});
        _ = try p.expectToken(.Semicolon);
        return try p.addNode(.{ .breakStmt = .{ .breakToken = br } });
    }

    if (try p.parseReturnStmt()) |some|
        return some;

    if (try p.parseAssembly(.stmt)) |some|
        return some;

    const exprStart = p.tokenIdx;
    const prevTotal = p.diagnostics.total;

    if (try p.parseExpr()) |some| {
        _ = try p.expectToken(.Semicolon);
        try some.maybeWarnUnused(p, exprStart, prevTotal);
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

    try p.err(.expected_stmt, p.tokenIdx, .{});
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

    if (!cond.qt.isInvalid() and cond.qt.scalarKind(p.comp) == .None)
        try p.err(.statement_scalar, lp + 1, .{cond.qt});

    try cond.saveValue(p);
    try p.expectClosing(lp, .RParen);

    const thenBody = try p.parseStmt();
    const elseBody = if (p.eat(.KeywordElse)) |_| try p.parseStmt() else null;
    if (p.nodeIs(thenBody, .nullStmt) and elseBody == null) {
        const semicolon = thenBody.get(&p.tree).nullStmt.semicolonOrRbraceToken;
        const locs = p.pp.tokens.items(.loc);
        const ifLoc = locs[kwIf];
        const semiLoc = locs[semicolon];
        if (ifLoc.line == semiLoc.line) {
            try p.err(.empty_if_body, semicolon, .{});
            try p.err(.empty_if_body_note, semicolon, .{});
        }
    }

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
    var prevTotal = p.diagnostics.total;
    const init = init: {
        if (gotDecl) break :init null;

        var init = (try p.parseExpr()) orelse break :init null;
        try init.saveValue(p);
        try init.maybeWarnUnused(p, initStart, prevTotal);
        break :init init.node;
    };

    if (!gotDecl)
        _ = try p.expectToken(.Semicolon);

    // cond
    const cond = cond: {
        const condToken = p.tokenIdx;
        var cond = (try p.parseExpr()) orelse break :cond null;
        try cond.lvalConversion(p, condToken);
        try cond.usualUnaryConversion(p, condToken);

        if (!cond.qt.isInvalid() and cond.qt.scalarKind(p.comp) == .None)
            try p.err(.statement_scalar, lp + 1, .{cond.qt});

        try cond.saveValue(p);
        break :cond cond.node;
    };

    _ = try p.expectToken(.Semicolon);

    // increment
    const incrStart = p.tokenIdx;
    prevTotal = p.diagnostics.total;

    const incr = incr: {
        var incr = (try p.parseExpr()) orelse break :incr null;
        try incr.maybeWarnUnused(p, incrStart, prevTotal);
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
            .init = if (declBufferTop == p.declBuffer.items.len)
                .{ .expr = init }
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

    if (!cond.qt.isInvalid() and cond.qt.scalarKind(p.comp) == .None)
        try p.err(.statement_scalar, lp + 1, .{cond.qt});

    try cond.saveValue(p);
    try p.expectClosing(lp, .RParen);

    const body = body: {
        const oldLoop = p.inLoop;
        p.inLoop = true;
        defer p.inLoop = oldLoop;
        break :body try p.parseStmt();
    };

    return p.addNode(.{ .whileStmt = .{ .whileToken = kwWhile, .cond = cond.node, .body = body } });
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

    if (!cond.qt.isInvalid() and cond.qt.scalarKind(p.comp) == .None)
        try p.err(.statement_scalar, lp + 1, .{cond.qt});

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
    const gpa = p.comp.gpa;
    if (p.eat(.Asterisk)) |_| {
        const exprToken = p.tokenIdx;
        var gotoExpr = try p.expect(parseExpr);
        try gotoExpr.lvalConversion(p, exprToken);
        p.computedGotoTok = p.computedGotoTok orelse gotoToken;

        const scalarKind = gotoExpr.qt.scalarKind(p.comp);
        if (!gotoExpr.qt.isInvalid() and !scalarKind.isPointer()) {
            const resultQt = try p.comp.typeStore.put(gpa, .{ .pointer = .{
                .child = .{ .@"const" = true, ._index = .Void },
                .decayed = null,
            } });

            if (!scalarKind.isInt()) {
                try p.err(.incompatible_arg, exprToken, .{ gotoExpr.qt, resultQt });
                return error.ParsingFailed;
            }

            if (gotoExpr.value.isZero(p.comp)) {
                try gotoExpr.nullToPointer(p, resultQt, exprToken);
            } else {
                try p.err(.implicit_int_to_ptr, exprToken, .{ gotoExpr.qt, resultQt });
                try gotoExpr.castToPointer(p, resultQt, exprToken);
            }
        }

        return p.addNode(.{ .computedGotoStmt = .{ .gotoToken = gotoToken, .expr = gotoExpr.node } });
    }

    const nameToken = try p.expectIdentifier();
    const str = p.getTokenText(nameToken);
    if (p.findLabel(str) == null) {
        try p.labels.append(gpa, .{ .unresolvedGoto = nameToken });
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

    if (!cond.qt.isInvalid() and !cond.qt.isInt(p.comp))
        try p.err(.statement_int, lp + 1, .{cond.qt});

    try cond.saveValue(p);
    try p.expectClosing(lp, .RParen);

    const oldSwitch = p.@"switch";
    var @"switch": Switch = .{
        .qt = cond.qt,
        .comp = p.comp,
    };
    p.@"switch" = &@"switch";

    defer {
        @"switch".ranges.deinit(p.comp.gpa);
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
    var firstItem = try p.parseIntegerConstExpr(.GNUFoldingExtension);
    const ellipsis = p.tokenIdx; // `...`
    var secondItem = if (p.eat(.Ellipsis) != null) blk: {
        try p.err(.gnu_switch_range, ellipsis, .{});
        break :blk try p.parseIntegerConstExpr(.GNUFoldingExtension);
    } else null;

    _ = try p.expectToken(.Colon);

    if (p.@"switch") |@"switch"| check: {
        if (@"switch".qt.hasIncompleteSize(p.comp)) // error already reported for incomplete size
            break :check;

        //   Coerce to switch condition type
        try firstItem.coerce(p, @"switch".qt, caseToken + 1, .assign);
        try firstItem.putValue(p);

        if (secondItem) |*item| {
            try item.coerce(p, @"switch".qt, ellipsis + 1, .assign);
            try item.putValue(p);
        }

        const first = firstItem.value;
        const last = if (secondItem) |second| second.value else first;
        if (first.isNone()) {
            try p.err(.case_val_unavailable, caseToken + 1, .{});
            break :check;
        } else if (last.isNone()) {
            try p.err(.case_val_unavailable, ellipsis + 1, .{});
            break :check;
        } else if (last.compare(.lt, first, p.comp)) {
            try p.err(.empty_case_range, caseToken + 1, .{});
            break :check;
        }

        // TODO cast to target type
        const prev = (try @"switch".add(first, last, caseToken + 1)) orelse break :check;

        try p.err(.duplicate_switch_case, caseToken + 1, .{firstItem});
        try p.err(.previous_case, prev.token, .{});
    } else {
        try p.err(.case_not_in_switch, caseToken, .{"case"});
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
        try p.err(.case_not_in_switch, defaultToken, .{"default"});
        return node;
    };

    if (@"switch".default) |previous| {
        try p.err(.multiple_default, defaultToken, .{});
        try p.err(.previous_case, previous, .{});
    } else {
        @"switch".default = defaultToken;
    }

    return node;
}

fn labelableStmt(p: *Parser) Error!Node.Index {
    if (p.currToken() == .RBrace) {
        try p.err(.label_compound_end, p.tokenIdx, .{});
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
            try p.err(.duplicate_label, nameToken, .{str});
            try p.err(.previous_label, some, .{str});
        } else {
            p.labelCount += 1;
            try p.labels.append(p.comp.gpa, .{ .label = nameToken });

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
    defer p.declBuffer.items.len = declBufferTop;

    const gpa = p.comp.gpa;

    // the parameters of a function are in the same scope as the body
    if (!isFnBody) try p.symStack.pushScope();
    defer if (!isFnBody) p.symStack.popScope();

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
        try p.declBuffer.append(gpa, s);

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
            try p.err(.unreachable_code, some, .{});
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
                        try p.err(.func_does_not_return, p.tokenIdx - 1, .{funcName});
                },
            };

            const implicitRet = try p.addNode(.{
                .returnStmt = .{
                    .returnToken = rbrace,
                    .returnQt = retQt,
                    .operand = .{ .implicit = returnZero },
                },
            });
            try p.declBuffer.append(gpa, implicitRet);
        }

        if (p.func.ident) |some| try p.declBuffer.insert(gpa, declBufferTop, some.node);
        if (p.func.prettyIdent) |some| try p.declBuffer.insert(gpa, declBufferTop, some.node);
    }

    return try p.addNode(.{
        .compoundStmt = .{
            .lbraceToken = lBrace,
            .body = p.declBuffer.items[declBufferTop..],
        },
    });
}

pub fn pointerValue(p: *Parser, node: Node.Index, offset: Value) !Value {
    switch (node.get(&p.tree)) {
        .declRefExpr => |declRef| {
            const varName = try p.comp.internString(p.getTokenText(declRef.nameToken));
            const sym = p.symStack.findSymbol(varName) orelse return .{};
            const symNode = sym.node.unpack() orelse return .{};
            return Value.pointer(.{ .node = @intFromEnum(symNode), .offset = offset.ref() }, p.comp);
        },
        .stringLiteralExpr => return p.tree.valueMap.get(node).?,
        else => return .{},
    }
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
        try p.err(.invalid_noreturn, eToken, .{p.getTokenText(p.func.name)});

    if (retExpr) |*some| {
        if (retQtIsVoid) {
            try p.err(.void_func_returns_value, eToken, .{p.getTokenText(p.func.name)});
        } else {
            try some.coerce(p, returnQt, eToken, .ret);
            try some.saveValue(p);
        }
    } else if (!retQtIsVoid) {
        try p.err(.func_should_return, retToken, .{p.getTokenText(p.func.name)});
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
            .KeywordSigned1,
            .KeywordSigned2,
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
        try p.err(.expected_expr, p.tokenIdx, .{});
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
    var prevTotal = p.diagnostics.total;
    var lhs = (try p.parseAssignExpr()) orelse {
        if (p.currToken() == .Comma) _ = try p.expectResult(null);
        return null;
    };

    while (p.eat(.Comma)) |comma| {
        try lhs.maybeWarnUnused(p, exprStart, prevTotal);
        exprStart = p.tokenIdx;
        prevTotal = p.diagnostics.total;

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

fn removeAssignExpr(assignNode: std.meta.Tag(Node)) std.meta.Tag(Node) {
    return switch (assignNode) {
        .mulAssignExpr => .mulExpr,
        .divAssignExpr => .divExpr,
        .modAssignExpr => .modExpr,
        .addAssignExpr => .addExpr,
        .subAssignExpr => .subExpr,
        .shlAssignExpr => .shlExpr,
        .shrAssignExpr => .shrExpr,
        .bitAndAssignExpr => .bitAndExpr,
        .bitXorAssignExpr => .bitXorExpr,
        .bitOrAssignExpr => .bitOrExpr,
        else => unreachable,
    };
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

    var isConst: bool = undefined;
    if (!p.tree.isLValueExtra(lhs.node, &isConst) or isConst) {
        try p.err(.not_assignable, token, .{});
        lhs.qt = .invalid;
    }

    if (tag == .assignExpr) {
        try rhs.coerce(p, lhs.qt, token, .assign);

        try lhs.bin(p, tag, rhs, token);
        return lhs;
    }

    var lhsDummy = blk: {
        var lhsCopy = lhs;
        try lhsCopy.un(p, .compoundAssignDummyExpr, token);
        try lhsCopy.lvalConversion(p, token);
        break :blk lhsCopy;
    };

    switch (tag) {
        .mulAssignExpr,
        .divAssignExpr,
        .modAssignExpr,
        => {
            if (!lhs.qt.isInvalid() and rhs.value.isZero(p.comp) and lhs.qt.isInt(p.comp) and rhs.qt.isInt(p.comp)) {
                switch (tag) {
                    .divAssignExpr => try p.err(.division_by_zero, token, .{"division"}),
                    .modAssignExpr => try p.err(.division_by_zero, token, .{"remainder"}),
                    else => {},
                }
            }
            _ = try lhsDummy.adjustTypes(token, &rhs, p, if (tag == .modAssignExpr) .integer else .arithmetic);
        },

        .subAssignExpr => _ = try lhsDummy.adjustTypes(token, &rhs, p, .sub),
        .addAssignExpr => _ = try lhsDummy.adjustTypes(token, &rhs, p, .add),

        .shlAssignExpr,
        .shrAssignExpr,
        .bitAndAssignExpr,
        .bitXorAssignExpr,
        .bitOrAssignExpr,
        => _ = try lhsDummy.adjustTypes(token, &rhs, p, .integer),

        else => unreachable,
    }
    _ = try lhsDummy.bin(p, removeAssignExpr(tag), rhs, token);
    try lhsDummy.coerce(p, lhs.qt, token, .assign);
    try lhs.bin(p, tag, lhsDummy, token);
    return lhs;
}

/// integer-const-expression : const-expression
fn parseIntegerConstExpr(p: *Parser, declFolding: ConstDeclFoldingMode) Error!Result {
    const start = p.tokenIdx;
    const res = try p.constExpr(declFolding);
    if (!res.qt.isInt(p.comp) and !res.qt.isInvalid()) {
        try p.err(.expected_integer_constant_expr, start, .{});
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

    try res.putValue(p);
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
        try p.err(.cond_expr_type, condToken, .{cond.qt});
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
        } else {
            lhs.value.boolCast(p.comp);
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
        } else {
            lhs.value.boolCast(p.comp);
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
            const res: ?bool = if (lhs.qt.isPointer(p.comp) or rhs.qt.isPointer(p.comp))
                lhs.value.comparePointers(op, rhs.value, p.comp)
            else
                lhs.value.compare(op, rhs.value, p.comp);

            lhs.value = if (res) |val| Value.fromBool(val) else .{};
        } else {
            lhs.value.boolCast(p.comp);
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

            const res = if (lhs.qt.isPointer(p.comp) or rhs.qt.isPointer(p.comp))
                lhs.value.comparePointers(op, rhs.value, p.comp)
            else
                lhs.value.compare(op, rhs.value, p.comp);
            lhs.value = if (res) |val| Value.fromBool(val) else .{};
        } else {
            lhs.value.boolCast(p.comp);
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
                    lhs.qt.signedness(p.comp) != .unsigned) try p.err(.overflow, tok, .{lhs});
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

        // We'll want to check this for invalid pointer arithmetic.
        const originalLhsQt = lhs.qt;

        if (try lhs.adjustTypes(tok, &rhs, p, if (tag == .addExpr) .add else .sub)) {
            const lhsSk = lhs.qt.scalarKind(p.comp);
            if (tag == .addExpr) {
                if (try lhs.value.add(lhs.value, rhs.value, lhs.qt, p.comp)) {
                    if (lhsSk.isPointer()) {
                        const increment = lhs;
                        const ptrBits = p.comp.typeStore.intptr.bitSizeof(p.comp);
                        const elemSize = increment.qt.childType(p.comp).sizeofOrNull(p.comp) orelse 1;
                        const maxElems = p.comp.maxArrayBytes() / elemSize;

                        try p.err(.array_overflow, tok, .{ increment, ptrBits, elemSize * 8, elemSize, maxElems });
                    } else if (lhs.qt.signedness(p.comp) != .unsigned) {
                        try p.err(.overflow, tok, .{lhs});
                    }
                }
            } else {
                const elemSize = if (originalLhsQt.isPointer(p.comp)) originalLhsQt.childType(p.comp).sizeofOrNull(p.comp) orelse 1 else 1;
                if (elemSize == 0 and rhs.qt.isPointer(p.comp)) {
                    lhs.value = .{};
                } else {
                    if (try lhs.value.sub(lhs.value, rhs.value, lhs.qt, elemSize, p.comp) and
                        lhs.qt.signedness(p.comp) != .unsigned)
                    {
                        try p.err(.overflow, tok, .{lhs});
                    }
                }
            }
        }

        if (!lhs.qt.isInvalid()) {
            const lhsSK = originalLhsQt.scalarKind(p.comp);
            if (lhsSK == .Pointer and originalLhsQt.childType(p.comp).hasIncompleteSize(p.comp)) {
                try p.err(.ptr_arithmetic_incomplete, tok, .{originalLhsQt.childType(p.comp)});
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
            lhs.value = .{};
            try p.err(
                if (p.inMacro) .division_by_zero_macro else .division_by_zero,
                tok,
                if (tag == .divExpr) .{"division"} else .{"remainder"},
            );

            if (p.inMacro)
                return error.ParsingFailed;
        }

        if (try lhs.adjustTypes(tok, &rhs, p, if (tag == .modExpr) .integer else .arithmetic)) {
            switch (tag) {
                .mulExpr => if (try lhs.value.mul(lhs.value, rhs.value, lhs.qt, p.comp) and
                    lhs.qt.signedness(p.comp) != .unsigned) try p.err(.overflow, tok, .{lhs}),
                .divExpr => if (try lhs.value.div(lhs.value, rhs.value, lhs.qt, p.comp) and
                    lhs.qt.signedness(p.comp) != .unsigned) try p.err(.overflow, tok, .{lhs}),

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

/// cast-expression
///  : '(' compoundStmt ')'
///  | '(' typeName ')' cast-expression
///  | '(' typeName ')' '{' initializerItems '}'
///  | unary-expression
fn parseCastExpr(p: *Parser) Error!?Result {
    if (p.eat(.LParen)) |lp| castExpr: {
        if (p.currToken() == .LBrace) {
            const tok = p.tokenIdx;
            try p.err(.gnu_statement_expression, p.tokenIdx, .{});
            if (p.func.qt == null) {
                try p.err(.stmt_expr_not_allowed_file_scope, p.tokenIdx, .{});
                return error.ParsingFailed;
            }

            var stmtExprState: StmtExprState = .{};
            const bodyNode = (try p.parseCompoundStmt(false, &stmtExprState)).?; // compoundStmt only returns null if .l_brace isn't the first token

            var res = Result{
                .node = bodyNode,
                .qt = stmtExprState.lastExprType,
            };
            try p.expectClosing(lp, .RParen);
            try res.un(p, .stmtExpr, tok);
            while (try p.parseSuffixExpr(res)) |suffix| {
                res = suffix;
            }
            return res;
        }
        const ty = (try p.parseTypeName()) orelse {
            p.tokenIdx -= 1;
            break :castExpr;
        };
        try p.expectClosing(lp, .RParen);

        if (p.currToken() == .LBrace) {
            // compound literal handled in unexpr.
            var lhs = (try p.parseCompoundLiteral(ty, lp)).?;
            while (try p.parseSuffixExpr(lhs)) |suffix| {
                lhs = suffix;
            }
            return lhs;
        }

        const operandToken = p.tokenIdx;
        var operand = try p.expect(parseCastExpr);
        try operand.lvalConversion(p, operandToken);
        try operand.castType(p, ty, operandToken, lp);

        return operand;
    }

    return p.parseUnaryExpr();
}

/// shufflevector : __builtin_shufflevector '(' assignExpr ',' assignExpr (',' integerConstExpr)* ')'
fn shuffleVector(p: *Parser, builtinToken: TokenIndex) Error!Result {
    const lparen = try p.expectToken(.LParen);

    const firstToken = p.tokenIdx;
    const lhs = try p.expect(parseAssignExpr);
    _ = try p.expectToken(.Comma);
    const secondToken = p.tokenIdx;
    const rhs = try p.expect(parseAssignExpr);

    const comp = p.comp;
    const gpa = comp.gpa;
    const maxIndex: ?Value = blk: {
        if (lhs.qt.isInvalid() or rhs.qt.isInvalid()) break :blk null;
        const lhsVec = lhs.qt.get(comp, .vector) orelse break :blk null;
        const rhsVec = rhs.qt.get(comp, .vector) orelse break :blk null;

        break :blk try Value.int(lhsVec.len + rhsVec.len, comp);
    };
    const negativeOne = try Value.intern(comp, .{ .int = .{ .i64 = -1 } });

    const listBufferTop = p.listBuffer.items.len;
    defer p.listBuffer.items.len = listBufferTop;

    while (p.eat(.Comma)) |_| {
        const indexToken = p.tokenIdx;
        const index = try p.parseIntegerConstExpr(.GNUFoldingExtension);
        try p.listBuffer.append(gpa, index.node);
        if (index.value.compare(.lt, negativeOne, comp)) {
            try p.err(.shufflevector_negative_index, indexToken, .{});
        } else if (maxIndex != null and index.value.compare(.gte, maxIndex.?, comp)) {
            try p.err(.shufflevector_index_too_big, indexToken, .{});
        }
    }

    try p.expectClosing(lparen, .RParen);

    var resQt: QualType = .invalid;
    if (!lhs.qt.isInvalid() and !lhs.qt.is(comp, .vector)) {
        try p.err(.shufflevector_arg, firstToken, .{"first"});
    } else if (!rhs.qt.isInvalid() and !rhs.qt.is(comp, .vector)) {
        try p.err(.shufflevector_arg, secondToken, .{"second"});
    } else if (!lhs.qt.eql(rhs.qt, comp)) {
        try p.err(.shufflevector_same_type, builtinToken, .{});
    } else if (p.listBuffer.items.len == listBufferTop) {
        resQt = lhs.qt;
    } else {
        resQt = try comp.typeStore.put(gpa, .{
            .vector = .{
                .elem = lhs.qt.childType(comp),
                .len = @intCast(p.listBuffer.items.len - listBufferTop),
            },
        });
    }

    return .{
        .qt = resQt,
        .node = try p.addNode(.{
            .builtinShuffleVector = .{
                .builtinToken = builtinToken,
                .qt = resQt,
                .lhs = lhs.node,
                .rhs = rhs.node,
                .indexes = p.listBuffer.items[listBufferTop..],
            },
        }),
    };
}

/// convertvector : __builtin_convertvector '(' assignExpr ',' typeName ')'
fn convertVector(p: *Parser, builtinToken: TokenIndex) Error!Result {
    const lparen = try p.expectToken(.LParen);
    const operand = try p.expect(parseAssignExpr);
    _ = try p.expectToken(.Comma);

    var destQt = (try p.parseTypeName()) orelse {
        try p.err(.expected_type, p.tokenIdx, .{});
        p.skipTo(.RParen);
        return error.ParsingFailed;
    };

    try p.expectClosing(lparen, .RParen);
    if (operand.qt.isInvalid() or operand.qt.isInvalid()) {
        destQt = .invalid;
    } else check: {
        const operandVec = operand.qt.get(p.comp, .vector) orelse {
            try p.err(.convertvector_arg, builtinToken, .{"first"});
            destQt = .invalid;
            break :check;
        };
        const destVec = destQt.get(p.comp, .vector) orelse {
            try p.err(.convertvector_arg, builtinToken, .{"second"});
            destQt = .invalid;
            break :check;
        };
        if (operandVec.len != destVec.len) {
            try p.err(.convertvector_size, builtinToken, .{});
            destQt = .invalid;
        }
    }

    return .{
        .qt = destQt,
        .node = try p.addNode(.{
            .builtinConvertVector = .{
                .builtinToken = builtinToken,
                .destQt = destQt,
                .operand = operand.node,
            },
        }),
    };
}

/// typesCompatible : __builtin_types_compatible_p '(' typeName ',' typeName ')'
fn typesCompatible(p: *Parser, builtinToken: TokenIndex) Error!Result {
    const lp = try p.expectToken(.LParen);

    const lhs = (try p.parseTypeName()) orelse {
        try p.err(.expected_type, p.tokenIdx, .{});
        p.skipTo(.RParen);
        return error.ParsingFailed;
    };
    _ = try p.expectToken(.Comma);

    const rhs = (try p.parseTypeName()) orelse {
        try p.err(.expected_type, p.tokenIdx, .{});
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

/// chooseExpr : __builtin_choose_expr '(' integerConstExpr ',' assignExpr ',' assignExpr ')'
fn builtinChooseExpr(p: *Parser) Error!Result {
    const lp = try p.expectToken(.LParen);
    const condToken = p.tokenIdx;
    var cond = try p.parseIntegerConstExpr(.NoConstDeclFolding);
    if (cond.value.isNone()) {
        try p.err(.builtin_choose_cond, condToken, .{});
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

/// vaStart : __builtin_va_arg '(' assignExpr ',' typeName ')'
fn builtinVaArg(p: *Parser, builtinToken: TokenIndex) Error!Result {
    const lp = try p.expectToken(.LParen);
    const vaListToken = p.tokenIdx;
    var vaList = try p.expect(parseAssignExpr);
    try vaList.lvalConversion(p, vaListToken);

    _ = try p.expectToken(.Comma);

    const ty = (try p.parseTypeName()) orelse {
        try p.err(.expected_type, p.tokenIdx, .{});
        return error.ParsingFailed;
    };

    try p.expectClosing(lp, .RParen);

    if (!vaList.qt.eql(p.comp.typeStore.vaList, p.comp)) {
        try p.err(.incompatible_va_arg, vaListToken, .{vaList.qt});
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

const OffsetKind = enum { Bits, Bytes };

/// offsetof
///  : __builtin_offsetof '(' type-name ',' offsetof-member-designator ')'
///  | __builtin_bitoffsetof '(' type-name ',' offsetof-member-designator ')'
fn builtinOffsetof(p: *Parser, builtinToken: TokenIndex, offsetKind: OffsetKind) Error!Result {
    const lparen = try p.expectToken(.LParen);
    const tyToken = p.tokenIdx;

    const operandQt = (try p.parseTypeName()) orelse {
        try p.err(.expected_type, p.tokenIdx, .{});
        p.skipTo(.RParen);
        return error.ParsingFailed;
    };

    const recordTy = operandQt.getRecord(p.comp) orelse {
        try p.err(.offsetof_ty, tyToken, .{operandQt});
        p.skipTo(.RParen);
        return error.ParsingFailed;
    };

    if (recordTy.layout == null) {
        try p.err(.offsetof_incomplete, tyToken, .{operandQt});
        p.skipTo(.RParen);
        return error.ParsingFailed;
    }

    _ = try p.expectToken(.Comma);

    const offsetofExpr = try p.offsetofMemberDesignator(recordTy, operandQt, offsetKind, builtinToken);

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
    offsetKind: OffsetKind,
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
                try p.err(.offsetof_ty, fieldNameToken, .{lhs.qt});
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
                try p.err(.offsetof_array, lbracket, .{lhs.qt});
                return error.ParsingFailed;
            }

            var ptr = lhs;
            try ptr.lvalConversion(p, lbracket);
            try index.lvalConversion(p, lbracket);

            if (index.qt.isInt(p.comp))
                try p.checkArrayBounds(index, lhs, lbracket)
            else
                try p.err(.invalid_index, lbracket, .{});

            try index.saveValue(p);
            try ptr.bin(p, .arrayAccessExpr, index, lbracket);
            lhs = ptr;
        },
        else => break,
    };

    const val = try Value.int(if (offsetKind == .Bits) totalOffset else totalOffset / 8, p.comp);
    return .{ .qt = baseQt, .value = val, .node = lhs.node };
}

fn computeOffsetExtra(p: *Parser, node: Node.Index, offsetSoFar: *Value) !Value {
    switch (node.get(&p.tree)) {
        .cast => |cast| {
            return switch (cast.kind) {
                .ArrayToPointer, .NoOP, .Bitcast => p.computeOffsetExtra(cast.operand, offsetSoFar),
                .LValToRVal => .{},
                else => unreachable,
            };
        },
        .parenExpr => |un| return p.computeOffsetExtra(un.operand, offsetSoFar),
        .declRefExpr => return p.pointerValue(node, offsetSoFar.*),
        .arrayAccessExpr => |access| {
            const indexValue = p.tree.valueMap.get(access.index) orelse return .{};
            var size = try Value.int(access.qt.sizeof(p.comp), p.comp);
            const mulOverflow = try size.mul(size, indexValue, p.comp.typeStore.ptrdiff, p.comp);

            const addOverflow = try offsetSoFar.add(size, offsetSoFar.*, p.comp.typeStore.ptrdiff, p.comp);
            _ = mulOverflow;
            _ = addOverflow;

            return p.computeOffsetExtra(access.base, offsetSoFar);
        },
        .memberAccessExpr, .memberAccessPtrExpr => |access| {
            var ty = access.base.qt(&p.tree);
            if (ty.isPointer(p.comp)) ty = ty.childType(p.comp);
            const recordTy = ty.getRecord(p.comp).?;

            const fieldOffset = try Value.int(@divExact(recordTy.fields[access.memberIndex].layout.offsetBits, 8), p.comp);
            _ = try offsetSoFar.add(fieldOffset, offsetSoFar.*, p.comp.typeStore.ptrdiff, p.comp);
            return p.computeOffsetExtra(access.base, offsetSoFar);
        },
        else => return .{},
    }
}

/// Compute the offset (in bytes) of an expression from a base pointer.
fn computeOffset(p: *Parser, res: Result) !Value {
    var val: Value = if (res.value.isNone()) .zero else res.value;
    return p.computeOffsetExtra(res.node, &val);
}

/// unaryExpr
///  : (compoundLiteral | primaryExpr) suffix-expression*
///  | '&&' identifier
///  | ('&' | '*' | '+' | '-' | '~' | '!' | '++' | '--' | `__extension__` | `__imag__` | `__real__`) cast-expression
///  | `sizeof` unary-expression
///  | `sizeof` '(' type-name ')'
///  | (`keyword-alignof` | `keyword-c23-alignof`) '(' type-name ')'
fn parseUnaryExpr(p: *Parser) Error!?Result {
    const gpa = p.comp.gpa;
    const token = p.tokenIdx;
    switch (p.currToken()) {
        .Ampersand => {
            if (p.inMacro) {
                try p.err(.invalid_preproc_operator, p.tokenIdx, .{});
                return error.ParsingFailed;
            }

            var addrValue: Value = .{};
            p.tokenIdx += 1;

            var operand = try p.expect(parseCastExpr);
            if (p.getNode(operand.node, .memberAccessExpr) orelse
                p.getNode(operand.node, .memberAccessPtrExpr)) |access|
            {
                if (access.isBitFieldWidth(&p.tree) != null)
                    try p.err(.addr_of_bitfield, token, .{});
            }

            if (!operand.qt.isInvalid()) {
                if (!p.tree.isLValue(operand.node))
                    try p.err(.addr_of_rvalue, token, .{});

                addrValue = try p.computeOffset(operand);

                operand.qt = try p.comp.typeStore.put(gpa, .{
                    .pointer = .{ .child = operand.qt, .decayed = null },
                });
            }

            if (p.getNode(operand.node, .declRefExpr)) |declRef| {
                switch (declRef.decl.get(&p.tree)) {
                    .variable => |variable| {
                        if (variable.storageClass == .register)
                            try p.err(.addr_of_register, token, .{});
                    },
                    else => {},
                }
            } else if (p.getNode(operand.node, .compoundLiteralExpr)) |literal| {
                switch (literal.storageClass) {
                    .register => try p.err(.addr_of_register, token, .{}),
                    else => {},
                }
            }

            try operand.saveValue(p);
            try operand.un(p, .addrOfExpr, token);
            operand.value = addrValue;
            return operand;
        },

        .AmpersandAmpersand => {
            const addressToken = p.tokenIdx;
            p.tokenIdx += 1;
            const nameToken = try p.expectIdentifier();
            try p.err(.gnu_label_as_value, addressToken, .{});
            p.containsAddresssOfLabel = true;

            const str = p.getTokenText(nameToken);
            if (p.findLabel(str) == null)
                try p.labels.append(gpa, .{ .unresolvedGoto = nameToken });

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
                    operand.value = .{};
                },
                else => {
                    try p.err(.indirection_ptr, token, .{});
                },
            }

            if (operand.qt.hasIncompleteSize(p.comp) and !operand.qt.is(p.comp, .void))
                try p.err(.deref_incomplete_ty_ptr, token, .{operand.qt});

            operand.qt = operand.qt.unqualified();
            try operand.un(p, .derefExpr, token);
            return operand;
        },

        .Plus => {
            p.tokenIdx += 1;

            var operand = try p.expect(parseCastExpr);
            try operand.lvalConversion(p, token);

            if (!operand.qt.isInt(p.comp) and !operand.qt.isFloat(p.comp))
                try p.err(.invalid_argument_un, token, .{operand.qt});

            try operand.usualUnaryConversion(p, token);

            return operand;
        },

        .Minus => {
            p.tokenIdx += 1;

            var operand = try p.expect(parseCastExpr);
            try operand.lvalConversion(p, token);

            const sk = operand.qt.scalarKind(p.comp);
            if (!sk.isArithmetic())
                try p.err(.invalid_argument_un, token, .{operand.qt});

            try operand.usualUnaryConversion(p, token);
            if (operand.value.isArithmetic(p.comp))
                _ = try operand.value.negate(operand.value, operand.qt, p.comp)
            else
                operand.value = .{};

            try operand.un(p, .negateExpr, token);
            return operand;
        },

        .PlusPlus => {
            p.tokenIdx += 1;

            var operand = try p.expect(parseCastExpr);

            const sk = operand.qt.scalarKind(p.comp);
            if (sk == .VoidPointer) try p.err(.gnu_pointer_arith, token, .{});
            if (sk == .None) try p.err(.invalid_argument_un, token, .{operand.qt});

            if (!sk.isReal())
                try p.err(.complex_prefix_postfix_op, p.tokenIdx, .{operand.qt});

            if (!p.tree.isLValue(operand.node) or operand.qt.@"const") {
                try p.err(.not_assignable, token, .{});
                return error.ParsingFailed;
            }

            try operand.usualUnaryConversion(p, token);

            if (operand.value.isNumeric(p.comp)) {
                if (try operand.value.add(operand.value, .one, operand.qt, p.comp))
                    try p.err(.overflow, token, .{operand});
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
            if (sk == .VoidPointer) try p.err(.gnu_pointer_arith, token, .{});
            if (sk == .None) try p.err(.invalid_argument_un, token, .{operand.qt});

            if (!sk.isReal())
                try p.err(.complex_prefix_postfix_op, p.tokenIdx, .{operand.qt});

            if (!p.tree.isLValue(operand.node) or operand.qt.@"const") {
                try p.err(.not_assignable, token, .{});
                return error.ParsingFailed;
            }

            try operand.usualUnaryConversion(p, token);

            if (operand.value.isNumeric(p.comp)) {
                if (try operand.value.decrement(operand.value, operand.qt, p.comp))
                    try p.err(.overflow, token, .{operand});
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
                try p.err(.complex_conj, token, .{operand.qt});
                if (operand.value.is(.complex, p.comp))
                    operand.value = try operand.value.complexConj(operand.qt, p.comp);
            } else {
                try p.err(.invalid_argument_un, token, .{operand.qt});
                operand.value = .{};
            }

            try operand.un(p, .bitNotExpr, token);
            return operand;
        },

        .Bang => {
            p.tokenIdx += 1;

            var operand = try p.expect(parseCastExpr);
            try operand.lvalConversion(p, token);

            if (operand.qt.scalarKind(p.comp) == .None)
                try p.err(.invalid_argument_un, token, .{operand.qt});

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
                try p.err(.expected_parens_around_typename, expectedParen, .{});
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
                    .void => try p.err(.pointer_arith_void, token, .{"sizeof"}),
                    .pointer => |ptrTy| if (ptrTy.decayed) |decayedQt| {
                        try p.err(.sizeof_array_arg, token, .{ res.qt, decayedQt });
                    },
                    else => {},
                }

                if (baseType.qt.sizeofOrNull(p.comp)) |size| {
                    if (size == 0 and p.comp.langOpts.emulate == .msvc) try p.err(.sizeof_returns_zero, token, .{});
                    res.value = try Value.int(size, p.comp);
                    res.qt = p.comp.typeStore.size;
                } else {
                    res.value = .{};
                    if (res.qt.hasIncompleteSize(p.comp)) {
                        try p.err(.invalid_sizeof, expectedParen - 1, .{res.qt});
                        res.qt = .invalid;
                    } else {
                        res.qt = p.comp.typeStore.size;
                    }
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
                try p.err(.expected_parens_around_typename, expectedParen, .{});
            } else if (p.eat(.LParen)) |lp| {
                if (try p.parseTypeName()) |qt| {
                    res.qt = qt;
                    try p.expectClosing(lp, .RParen);
                } else {
                    p.tokenIdx = expectedParen;
                    res = try p.parseNoEval(parseUnaryExpr);
                    hasExpr = true;
                    try p.err(.alignof_expr, expectedParen, .{});
                }
            } else {
                res = try p.parseNoEval(parseUnaryExpr);
                hasExpr = true;

                try p.err(.alignof_expr, expectedParen, .{});
            }

            const operandQt = res.qt;

            if (res.qt.is(p.comp, .void))
                try p.err(.pointer_arith_void, token, .{"alignof"});

            if (res.qt.sizeofOrNull(p.comp) != null) {
                res.value = try Value.int(res.qt.alignof(p.comp), p.comp);
                res.qt = p.comp.typeStore.size;
            } else if (!res.qt.isInvalid()) {
                try p.err(.invalid_alignof, expectedParen, .{res.qt});
                res.qt = .invalid;
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
            if (operand.qt.isInvalid()) return operand;

            const sk = operand.qt.scalarKind(p.comp);
            if (!sk.isArithmetic()) {
                try p.err(.invalid_imag, imagToken, .{operand.qt});
            }
            if (!sk.isReal()) {
                operand.value = try operand.value.imaginaryPart(p.comp);
            } else switch (p.comp.langOpts.emulate) {
                .msvc => {},
                .gcc => operand.value = Value.zero,
                .clang => {
                    if (operand.value.is(.int, p.comp) or operand.value.is(.float, p.comp))
                        operand.value = Value.zero
                    else
                        operand.value = .{};
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
            if (operand.qt.isInvalid()) return operand;
            if (!operand.qt.isInt(p.comp) and !operand.qt.isFloat(p.comp)) {
                try p.err(.invalid_real, realToken, .{operand.qt});
            }

            operand.qt = operand.qt.toReal(p.comp);
            operand.value = try operand.value.realPart(p.comp);

            try operand.un(p, .realExpr, token);
            return operand;
        },

        else => {
            var lhs = (try p.parseCompoundLiteral(null, null)) orelse
                (try p.parsePrimaryExpr()) orelse return null;

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
fn parseCompoundLiteral(p: *Parser, optQt: ?QualType, optLparen: ?TokenIndex) Error!?Result {
    const lparen, const d = if (optQt) |some| .{ optLparen.?, DeclSpec{ .qt = some } } else blk: {
        const lparen = p.eat(.LParen) orelse return null;

        var d: DeclSpec = .{ .qt = .invalid };
        const any = if (p.comp.langOpts.standard.atLeast(.c23))
            try p.parseStorageClassSpec(&d)
        else
            false;

        switch (d.storageClass) {
            .auto, .@"extern", .typedef => |tok| {
                try p.err(.invalid_compound_literal_storage_class, tok, .{@tagName(d.storageClass)});
                d.storageClass = .none;
            },
            .register => if (p.func.qt == null) try p.err(.illegal_storage_on_global, p.tokenIdx, .{}),
            else => {},
        }

        d.qt = (try p.parseTypeName()) orelse {
            p.tokenIdx = lparen;
            if (any) {
                try p.err(.expected_type, p.tokenIdx, .{});
                return error.ParsingFailed;
            }
            return null;
        };
        try p.expectClosing(lparen, .RParen);
        break :blk .{ lparen, d };
    };

    var qt = d.qt;
    switch (qt.base(p.comp).type) {
        .func => try p.err(.func_init, p.tokenIdx, .{}),
        .array => |arrayTy| if (arrayTy.len == .variable) try p.err(.vla_init, p.tokenIdx, .{}),
        else => if (qt.hasIncompleteSize(p.comp)) {
            try p.err(.variable_incomplete_ty, p.tokenIdx, .{qt});
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
                if (index.qt.isInt(p.comp))
                    try p.checkArrayBounds(indexBeforeConversion, arrayBeforeConversion, lb)
                else
                    try p.err(.invalid_index, lb, .{});
            } else if (index.qt.get(p.comp, .pointer)) |ptrTy| {
                index.qt = ptrTy.child;
                if (ptr.qt.isInt(p.comp))
                    try p.checkArrayBounds(arrayBeforeConversion, indexBeforeConversion, lb)
                else
                    try p.err(.invalid_index, lb, .{});
                std.mem.swap(Result, &ptr, &index);
            } else if (ptr.qt.get(p.comp, .vector)) |vectorTy| {
                ptr = arrayBeforeConversion;
                ptr.qt = vectorTy.elem;
                if (!index.qt.isInt(p.comp))
                    try p.err(.invalid_index, lb, .{});
            } else {
                try p.err(.invalid_subscript, lb, .{});
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
            if (scalarKind == .VoidPointer) try p.err(.gnu_pointer_arith, p.tokenIdx, .{});
            if (scalarKind == .None) try p.err(.invalid_argument_un, p.tokenIdx, .{operand.qt});

            if (!scalarKind.isReal())
                try p.err(.complex_prefix_postfix_op, p.tokenIdx, .{operand.qt});

            if (!p.tree.isLValue(operand.node) or operand.qt.@"const") {
                try p.err(.not_assignable, p.tokenIdx, .{});
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
            if (scalarKind == .VoidPointer) try p.err(.gnu_pointer_arith, p.tokenIdx, .{});
            if (scalarKind == .None) try p.err(.invalid_argument_un, p.tokenIdx, .{operand.qt});

            if (!scalarKind.isReal())
                try p.err(.complex_prefix_postfix_op, p.tokenIdx, .{operand.qt});

            if (!p.tree.isLValue(operand.node) or operand.qt.@"const") {
                try p.err(.not_assignable, p.tokenIdx, .{});
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
    if (lhs.qt.isInvalid()) {
        const access: Node.MemberAccess = .{
            .accessToken = accessToken,
            .qt = .invalid,
            .base = lhs.node,
            .memberIndex = std.math.maxInt(u32),
        };
        return .{
            .qt = .invalid,
            .node = try p.addNode(if (isArrow)
                .{ .memberAccessPtrExpr = access }
            else
                .{ .memberAccessExpr = access }),
        };
    }

    const exprQt = if (lhs.qt.get(p.comp, .atomic)) |atomic| atomic else lhs.qt;
    const isPtr = exprQt.isPointer(p.comp);
    const exprBaseQt = if (isPtr) exprQt.childType(p.comp) else exprQt;
    const recordQt = if (exprBaseQt.get(p.comp, .atomic)) |atomic| atomic else exprBaseQt;
    const recordType = recordQt.getRecord(p.comp) orelse {
        try p.err(.expected_record_ty, fieldNameToken, .{exprQt});
        return error.ParsingFailed;
    };

    if (recordType.layout == null) {
        std.debug.assert(isPtr);
        try p.err(.deref_incomplete_ty_ptr, fieldNameToken - 2, .{exprBaseQt});
        return error.ParsingFailed;
    }

    if (exprQt != lhs.qt) try p.err(.member_expr_atomic, fieldNameToken, .{lhs.qt});
    if (exprBaseQt != recordQt) try p.err(.member_expr_atomic, fieldNameToken, .{exprBaseQt});

    if (isArrow and !isPtr) try p.err(.member_expr_not_ptr, fieldNameToken, .{exprQt});
    if (!isArrow and isPtr) try p.err(.member_expr_ptr, fieldNameToken, .{exprQt});

    const fieldName = try p.getInternString(fieldNameToken);
    try p.validateFieldAccess(recordType, recordQt, fieldNameToken, fieldName);
    var discard: u64 = 0;
    return p.fieldAccessExtra(lhs.node, recordType, fieldName, isArrow, accessToken, &discard);
}

fn validateFieldAccess(
    p: *Parser,
    recordType: Type.Record,
    recordQt: QualType,
    fieldNameToken: TokenIndex,
    fieldName: StringId,
) Error!void {
    if (recordType.hasField(p.comp, fieldName)) return;
    try p.err(.no_such_member, fieldNameToken, .{ p.getTokenText(fieldNameToken), recordQt });
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
        if (field.nameToken == 0) {
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
        try p.err(.closing_paren, firstAfter, .{});
        return error.ParsingFailed;
    }

    const funcQt = p.func.qt orelse {
        try p.err(.va_start_not_in_func, builtinToken, .{});
        return;
    };

    const funcTy = funcQt.get(p.comp, .func) orelse return;
    if (funcTy.kind != .Variadic or funcTy.params.len == 0) {
        return p.err(.va_start_fixed_args, builtinToken, .{});
    }

    const lastParamName = funcTy.params[funcTy.params.len - 1].name;
    const declRef = p.getNode(arg.node, .declRefExpr);
    if (declRef == null or lastParamName != try p.getInternString(declRef.?.nameToken)) {
        try p.err(.va_start_not_last_param, paramToken, .{});
    }
}

fn checkArithOverflowArg(p: *Parser, builtinToken: TokenIndex, firstAfter: TokenIndex, paramToken: TokenIndex, arg: *Result, idx: u32) !void {
    _ = builtinToken;
    _ = firstAfter;
    if (idx <= 1) {
        const argSk = arg.qt.scalarKind(p.comp);
        if (!argSk.isInt() or !argSk.isReal()) {
            return p.err(.overflow_builtin_requires_int, paramToken, .{arg.qt});
        }
    } else if (idx == 2) {
        if (!arg.qt.isPointer(p.comp)) return p.err(.overflow_result_requires_ptr, paramToken, .{arg.qt});
        const child = arg.qt.childType(p.comp);
        if (child.scalarKind(p.comp) != .Int or child.@"const") return p.err(.overflow_result_requires_ptr, paramToken, .{arg.qt});
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
        try p.err(.not_floating_type, paramTok, .{arg.qt});
    } else if (idx == 1) {
        const prevIdx = p.listBuffer.items[p.listBuffer.items.len - 1];
        const prevQt = prevIdx.qt(&p.tree);
        if (!prevQt.eql(arg.qt, p.comp)) {
            try p.err(.argument_types_differ, paramTok, .{ prevQt, arg.qt });
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
                .__builtin_add_overflow,
                .__builtin_sub_overflow,
                .__builtin_mul_overflow,
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
            .__builtin_add_overflow,
            .__builtin_sub_overflow,
            .__builtin_mul_overflow,
            => return p.checkArithOverflowArg(builtinToken, firstAfter, paramToken, arg, argIdx),
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
                .__builtin_add_overflow,
                .__builtin_sub_overflow,
                .__builtin_mul_overflow,
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

    fn returnType(self: CallExpr, p: *Parser, funcQt: QualType) !QualType {
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
                if (!qt.isPointer(p.comp)) return .invalid;
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
        const returnQt = try self.returnType(p, funcQt);
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
    const gpa = p.comp.gpa;
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
            try p.err(.not_callable, lParen, .{lhs.qt});
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

        if ((arg.qt.hasIncompleteSize(p.comp) and !arg.qt.is(p.comp, .void) or arg.qt.isInvalid()))
            return error.ParsingFailed;

        if (argCount >= paramsLen) {
            if (callExpr.shouldPromoteVarArg(argCount)) switch (arg.qt.base(p.comp).type) {
                .int => |intTy| if (intTy == .Int) try arg.castToInt(p, arg.qt.promoteInt(p.comp), paramToken),
                .float => |floatTy| if (floatTy == .Double) try arg.castToFloat(p, .double, paramToken),
                else => {},
            };

            try callExpr.checkVarArg(p, firstAfter, paramToken, &arg, argCount);
            try arg.saveValue(p);
            try p.listBuffer.append(gpa, arg.node);
            argCount += 1;

            _ = p.eat(.Comma) orelse {
                try p.expectClosing(lParen, .RParen);
                break;
            };
            continue;
        }

        if (funcQt.get(p.comp, .func)) |funcTy| {
            const param = funcTy.params[argCount];
            if (param.qt.get(p.comp, .pointer)) |pointerTy| static_check: {
                const decayedChildQt = pointerTy.decayed orelse break :static_check;
                const paramArrayTy = decayedChildQt.get(p.comp, .array).?;

                if (paramArrayTy.len != .static) break :static_check;

                const paramArrayLen = paramArrayTy.len.static;
                const argArrayLen = arg.qt.arrayLen(p.comp);

                if (argArrayLen != null and argArrayLen.? < paramArrayLen) {
                    try p.err(.array_argument_too_small, paramToken, .{ argArrayLen.?, paramArrayLen });
                    try p.err(.callee_with_static_array, param.nameToken, .{});
                }

                if (arg.value.isZero(p.comp)) {
                    try p.err(.non_null_argument, paramToken, .{});
                    try p.err(.callee_with_static_array, param.nameToken, .{});
                }
            }

            if (callExpr.shouldCoerceArg(argCount)) {
                try arg.coerce(p, param.qt, paramToken, .{ .arg = param.nameToken });
            }
        }

        try arg.saveValue(p);
        try p.listBuffer.append(gpa, arg.node);
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

    if (callExpr.paramCountOverride()) |expected| {
        if (expected != argCount)
            try p.err(.expected_arguments, firstAfter, .{ expected, argCount });
    } else switch (funcKind) {
        .Normal => if (paramsLen != argCount) {
            try p.err(.expected_arguments, firstAfter, .{ paramsLen, argCount });
        },
        .Variadic => if (argCount < paramsLen) {
            try p.err(.expected_at_least_arguments, firstAfter, .{ paramsLen, argCount });
        },
        .OldStyle => if (paramsLen != argCount) {
            if (paramsLen == 0)
                try p.err(.passing_args_to_kr, firstAfter, .{})
            else
                try p.err(.expected_arguments_old, firstAfter, .{ paramsLen, argCount });
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
                        try p.err(.old_style_flexible_struct, token, .{index});
                    }
                    return;
                }
            }
        }
    }

    const indexInt = index.value.toInt(u64, p.comp) orelse std.math.maxInt(u64);
    if (index.qt.isUnsigned(p.comp)) {
        if (indexInt >= arrayLen) {
            try p.err(.array_after, token, .{index});
        }
    } else {
        if (index.value.compare(.lt, .zero, p.comp)) {
            try p.err(.array_before, token, .{index});
        } else if (indexInt >= arrayLen) {
            try p.err(.array_after, token, .{index});
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
///  | generic-selectioncatch
///  | shufflevector
///  | convertvector
///  | typesCompatible
///  | chooseExpr
///  | vaStart
///  | offsetof
fn parsePrimaryExpr(p: *Parser) Error!?Result {
    if (p.eat(.LParen)) |lp| {
        var groupedExpr = try p.expect(parseExpr);
        try p.expectClosing(lp, .RParen);
        try groupedExpr.un(p, .parenExpr, lp);
        return groupedExpr;
    }

    const gpa = p.comp.gpa;
    switch (p.currToken()) {
        .Identifier, .ExtendedIdentifier => {
            const nameToken = try p.expectIdentifier();
            const name = p.getTokenText(nameToken);
            const internedName = try p.comp.internString(name);

            if (internedName == p.autoTypeDeclName) {
                try p.err(.auto_type_self_initialized, nameToken, .{name});
                return error.ParsingFailed;
            }

            if (p.symStack.findSymbol(internedName)) |sym| {
                if (sym.kind == .typedef) {
                    try p.err(.unexpected_type_name, nameToken, .{name});
                    return error.ParsingFailed;
                }
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
                        .GNUFoldingExtension => try p.err(.const_decl_folded, nameToken, .{}),
                        .GNUVLAFoldingExtension => try p.err(.const_decl_folded_vla, nameToken, .{}),
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

                const res: Result = .{
                    .value = if (p.constDeclFolding == .NoConstDeclFolding and sym.kind != .enumeration) Value{} else sym.value,
                    .qt = sym.qt,
                    .node = node,
                };
                try res.putValue(p);
                return res;
            }

            // check if this is a builtin call
            if (try p.comp.builtins.getOrCreate(p.comp, name)) |some| {
                for (p.tokenIds[p.tokenIdx..]) |id| switch (id) {
                    .RParen => {}, // closing grouped expr
                    .LParen => break, // beginning of a call
                    else => {
                        try p.err(.builtin_must_be_called, nameToken, .{});
                        return error.ParsingFailed;
                    },
                };

                if (some.builtin.properties.header != .none) {
                    try p.err(.implicit_builtin, nameToken, .{name});
                    try p.err(.implicit_builtin_header_note, nameToken, .{
                        @tagName(some.builtin.properties.header),
                        Builtin.nameFromTag(some.builtin.tag).span(),
                    });
                }

                switch (some.builtin.tag) {
                    .__builtin_choose_expr => return try p.builtinChooseExpr(),
                    .__builtin_va_arg => return try p.builtinVaArg(nameToken),
                    .__builtin_offsetof => return try p.builtinOffsetof(nameToken, .Bytes),
                    .__builtin_bitoffsetof => return try p.builtinOffsetof(nameToken, .Bits),
                    .__builtin_types_compatible_p => return try p.typesCompatible(nameToken),
                    .__builtin_convertvector => return try p.convertVector(nameToken),
                    .__builtin_shufflevector => return try p.shuffleVector(nameToken),
                    else => {},
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
                    try p.err(.unknown_builtin, nameToken, .{name})
                else
                    try p.err(.implicit_func_decl, nameToken, .{name});

                const funcQt = try p.comp.typeStore.put(gpa, .{ .func = .{
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

                try p.declBuffer.append(gpa, node);
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
            try p.err(.undeclared_identifier, nameToken, .{p.getTokenText(nameToken)});
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
            try p.err(.pre_c23_compat, p.tokenIdx, .{"'nullptr'"});

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

                const name = p.getTokenText(p.func.name);
                try p.strings.ensureUnusedCapacity(gpa, name.len + 1);

                p.strings.appendSliceAssumeCapacity(name);
                p.strings.appendAssumeCapacity(0);

                const predef = try p.makePredefinedIdentifier(p.strings.items[stringsTop..]);
                qt = predef.qt;
                p.func.ident = predef;
            } else {
                const predef = try p.makePredefinedIdentifier("\x00");
                qt = predef.qt;
                p.func.ident = predef;
                try p.declBuffer.append(gpa, predef.node);
            }

            if (p.func.qt == null)
                try p.err(.predefined_top_level, p.tokenIdx, .{});

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
                var sf = std.heap.stackFallback(1024, gpa);
                var allocating: std.Io.Writer.Allocating = .init(sf.get());
                defer allocating.deinit();

                funcQt.printNamed(p.getTokenText(p.func.name), p.comp, &allocating.writer) catch return error.OutOfMemory;
                allocating.writer.writeByte(0) catch return error.OutOfMemory;

                const predef = try p.makePredefinedIdentifier(allocating.written());
                qt = predef.qt;
                p.func.prettyIdent = predef;
            } else {
                const predef = try p.makePredefinedIdentifier("top level\x00");
                qt = predef.qt;
                p.func.prettyIdent = predef;
                try p.declBuffer.append(gpa, predef.node);
            }

            if (p.func.qt == null)
                try p.err(.predefined_top_level, p.tokenIdx, .{});

            return .{
                .qt = qt,
                .node = try p.addNode(.{
                    .declRefExpr = .{
                        .nameToken = p.tokenIdx,
                        .qt = qt,
                        .decl = p.func.prettyIdent.?.node,
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

fn makePredefinedIdentifier(p: *Parser, slice: []const u8) !Result {
    const gpa = p.comp.gpa;
    const arrayQt = try p.comp.typeStore.put(gpa, .{ .array = .{
        .elem = .{ .@"const" = true, ._index = .Char },
        .len = .{ .fixed = slice.len },
    } });

    const val = try Value.intern(p.comp, .{ .bytes = slice });

    const strLit = try p.addNode(.{
        .stringLiteralExpr = .{
            .qt = arrayQt,
            .literalToken = p.tokenIdx,
            .kind = .ascii,
        },
    });
    if (!p.inMacro) try p.tree.valueMap.put(gpa, strLit, val);

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
        try p.strings.ensureUnusedCapacity(p.comp.gpa, buf.len);

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
        try p.err(.gnu_imaginary_constant, p.tokenIdx, .{});
        res.qt = try qt.toComplex(p.comp);
        res.value = try Value.intern(p.comp, switch (res.qt.bitSizeof(p.comp)) {
            32 => .{ .complex = .{ .cf16 = .{ 0.0, val.toFloat(f16, p.comp) } } },
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
            .binary => try p.err(.invalid_binary_digit, tokenIdx, .{TextLiteral.Ascii.init(buffer[0])}),
            .octal => try p.err(.invalid_octal_digit, tokenIdx, .{TextLiteral.Ascii.init(buffer[0])}),
            .hex => try p.err(.invalid_int_suffix, tokenIdx, .{buffer}),
            .decimal => unreachable,
        }
        return error.ParsingFailed;
    }

    for (buffer, 0..) |c, idx| {
        if (idx == 0) continue;
        switch (c) {
            '.' => return buffer[0..idx],

            'p', 'P' => return if (prefix == .hex) buffer[0..idx] else {
                try p.err(.invalid_int_suffix, tokenIdx, .{buffer[idx..]});
                return error.ParsingFailed;
            },

            'e', 'E' => {
                switch (prefix) {
                    .hex => continue,
                    .decimal => return buffer[0..idx],
                    .binary => try p.err(.invalid_binary_digit, tokenIdx, .{TextLiteral.Ascii.init(c)}),
                    .octal => try p.err(.invalid_octal_digit, tokenIdx, .{TextLiteral.Ascii.init(c)}),
                }
                return error.ParsingFailed;
            },

            '0'...'9', 'a'...'d', 'A'...'D', 'f', 'F' => {
                if (!prefix.digitAllowed(c)) {
                    switch (prefix) {
                        .binary => try p.err(.invalid_binary_digit, tokenIdx, .{TextLiteral.Ascii.init(c)}),
                        .octal => try p.err(.invalid_octal_digit, tokenIdx, .{TextLiteral.Ascii.init(c)}),
                        .decimal, .hex => try p.err(.invalid_int_suffix, tokenIdx, .{buffer[idx..]}),
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
        try p.err(.int_literal_too_big, tokenIdx, .{});
        res.qt = .ulonglong;
        res.node = try p.addNode(.{ .intLiteral = .{ .qt = res.qt, .literalToken = tokenIdx } });
        try res.putValue(p);
        return res;
    }

    const internedVal = try Value.int(value, p.comp);
    if (suffix.isSignedInteger() and base == 10) {
        const maxInt = try Value.maxInt(p.comp.typeStore.intmax, p.comp);
        if (internedVal.compare(.gt, maxInt, p.comp)) {
            try p.err(.implicitly_unsigned_literal, tokenIdx, .{});
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
        if (p.comp.langOpts.emulate == .gcc) {
            if (TargetUtil.hasInt128(&p.comp.target))
                res.qt = .int128
            else
                res.qt = .longlong;
        } else {
            res.qt = .ulonglong;
        }
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
        try p.err(.binary_integer_literal, tokenIdx, .{});

    const base = @intFromEnum(prefix);
    var res = if (suffix.isBitInt())
        try p.bitInt(base, buf, suffix, tokenIdx)
    else
        try p.fixedSizeInt(base, buf, suffix, tokenIdx);

    if (suffix.isImaginary()) {
        try p.err(.gnu_imaginary_constant, tokenIdx, .{});
        res.qt = try res.qt.toComplex(p.comp);
        res.value = .{};
        try res.un(p, .imaginaryLiteral, tokenIdx);
    }
    return res;
}

fn bitInt(p: *Parser, base: u8, buf: []const u8, suffix: NumberSuffix, tokenIdx: TokenIndex) Error!Result {
    const gpa = p.comp.gpa;
    try p.err(.pre_c23_compat, tokenIdx, .{"'_BitInt' suffix for literals"});
    try p.err(.bitint_suffix, tokenIdx, .{});

    var managed = try big.int.Managed.init(gpa);
    defer managed.deinit();

    managed.setString(base, buf) catch |e| switch (e) {
        error.InvalidBase => unreachable, // `base` is one of 2, 8, 10, 16
        error.InvalidCharacter => unreachable, // digits validated by Lexer
        else => |er| return er,
    };

    const c = managed.toConst();
    const bitsNeeded: std.math.IntFittingRange(0, Compilation.BitIntMaxBits) = blk: {
        const count = @max(1, c.bitCountTwosComp());
        const signBits = @intFromBool(suffix.isSignedInteger());
        const bitsNeeded = count + signBits;
        break :blk @intCast(bitsNeeded);
    };

    const intQt = try p.comp.typeStore.put(gpa, .{ .bitInt = .{
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
        try p.err(.invalid_int_suffix, tokenIdx, .{buffer});
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
            try p.err(.invalid_float_suffix, tokenIdx, .{buffer});
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
        try p.err(.exponent_has_no_digits, tokenIdx, .{});
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
            try p.err(.invalid_float_suffix, tokenIdx, .{suffixStr})
        else
            try p.err(.invalid_int_suffix, tokenIdx, .{suffixStr});
        return error.ParsingFailed;
    };

    if (suffix.isFloat80() and p.comp.float80Type() == null) {
        try p.err(.invalid_float_suffix, tokenIdx, .{suffixStr});
        return error.ParsingFailed;
    }

    if (isFloat) {
        assert(prefix == .hex or prefix == .decimal);
        if (prefix == .hex and exponent.len == 0) {
            try p.err(.hex_floating_constant_requires_exponent, tokenIdx, .{});
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
            try p.err(.float_literal_in_pp_expr, p.tokenIdx, .{});
            return error.ParsingFailed;
        }
        res.qt = if (res.qt.isUnsigned(p.comp)) try p.comp.typeStore.intmax.makeIntUnsigned(p.comp) else p.comp.typeStore.intmax;
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
            try p.err(.empty_char_literal_error, p.tokenIdx, .{})
        else if (tokenId == .UnterminatedCharLiteral)
            try p.err(.unterminated_char_literal_error, p.tokenIdx, .{})
        else
            unreachable;

        return .{
            .qt = .int,
            .value = .zero,
            .node = try p.addNode(.{ .charLiteral = .{ .qt = .int, .literalToken = p.tokenIdx, .kind = .ascii } }),
        };
    };

    if (charKind == .utf8) try p.err(.u8_char_lit, p.tokenIdx, .{});
    var val: u32 = 0;

    const gpa = p.comp.gpa;
    const slice = charKind.contentSlice(p.getTokenText(p.tokenIdx));

    if (slice.len == 1 and std.ascii.isAscii(slice[0])) {
        val = slice[0];
    } else {
        const maxCodepoint = charKind.maxCodepoint(p.comp);
        var CharLiteralParser: TextLiteral.Parser = .{
            .comp = p.comp,
            .literal = slice,
            .kind = charKind,
            .maxCodepoint = maxCodepoint,
            .loc = p.pp.tokens.items(.loc)[p.tokenIdx],
            .expansionLocs = p.pp.expansionSlice(p.tokenIdx),
        };
        const maxCharsExpected = 4;

        var sf = std.heap.stackFallback(maxCharsExpected * @sizeOf(u32), gpa);
        const allocator = sf.get();

        var chars: std.ArrayList(u32) = .empty;
        defer chars.deinit(allocator);

        chars.ensureUnusedCapacity(allocator, maxCharsExpected) catch unreachable; // stack allocation already succeeded

        while (try CharLiteralParser.next()) |item|
            switch (item) {
                .value => |v| try chars.append(allocator, v),
                .codepoint => |c| try chars.append(allocator, c),
                .improperlyEncoded => |s| {
                    try chars.ensureUnusedCapacity(allocator, s.len);
                    for (s) |c| chars.appendAssumeCapacity(c);
                },
                .utf8Text => |view| {
                    var it = view.iterator();
                    var maxcodepointSeen: u21 = 0;
                    while (it.nextCodepoint()) |c| {
                        maxcodepointSeen = @max(maxcodepointSeen, c);
                        try chars.append(allocator, c);
                    }
                    if (maxcodepointSeen > maxCodepoint)
                        try CharLiteralParser.err(.char_too_large, .{});
                },
            };

        const isMultichar = chars.items.len > 1;
        if (isMultichar) {
            if (charKind == .char and chars.items.len == 4) {
                try CharLiteralParser.warn(.four_char_char_literal, .{});
            } else if (charKind == .char) {
                try CharLiteralParser.warn(.multichar_literal_warning, .{});
            } else {
                const kind = switch (charKind) {
                    .wide => "wide",
                    .utf8, .utf16, .utf32 => "Unicode",
                    else => unreachable,
                };
                try CharLiteralParser.err(.invalid_multichar_literal, .{kind});
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
            try CharLiteralParser.err(.char_lit_too_wide, .{});
    }

    const charliteralQt = charKind.charLiteralType(p.comp);
    // This is the type the literal will have if we're in a macro; macros always operate on intmax_t/uintmax_t values
    const macroQt = if (charliteralQt.isUnsigned(p.comp) or (charKind == .char and p.comp.getCharSignedness() == .unsigned))
        try p.comp.typeStore.intmax.makeIntUnsigned(p.comp)
    else
        p.comp.typeStore.intmax;

    const res: Result = .{
        .qt = if (p.inMacro) macroQt else charliteralQt,
        .value = try Value.int(val, p.comp),
        .node = try p.addNode(.{ .charLiteral = .{ .qt = charliteralQt, .literalToken = p.tokenIdx, .kind = switch (charKind) {
            .char, .unterminated => .ascii,
            .wide => .wide,
            .utf8 => .utf8,
            .utf16 => .utf16,
            .utf32 => .utf32,
        } } }),
    };

    if (!p.inMacro) try p.tree.valueMap.put(gpa, res.node, res.value);
    return res;
}

fn parseStringLiteral(p: *Parser) Error!Result {
    const gpa = p.comp.gpa;
    const stringStart = p.tokenIdx;
    var stringEnd = p.tokenIdx;
    var stringKind: TextLiteral.Kind = .char;
    while (TextLiteral.Kind.classify(p.tokenIds[stringEnd], .StringLiteral)) |next| : (stringEnd += 1) {
        stringKind = stringKind.concat(next) catch {
            try p.err(.unsupported_str_cat, stringEnd, .{});
            while (p.currToken().isStringLiteral()) : (p.tokenIdx += 1) {}
            return error.ParsingFailed;
        };

        if (stringKind == .unterminated) {
            p.tokenIdx = stringEnd + 1;
            return error.ParsingFailed;
        }
    }

    const count = stringEnd - p.tokenIdx;
    assert(count > 0);

    const charWidth = stringKind.charUnitSize(p.comp);

    const stringsTop = p.strings.items.len;
    defer p.strings.items.len = stringsTop;

    const literalStart = std.mem.alignForward(usize, stringsTop, @intFromEnum(charWidth));
    try p.strings.resize(gpa, literalStart);

    while (p.tokenIdx < stringEnd) : (p.tokenIdx += 1) {
        const thisKind = TextLiteral.Kind.classify(p.currToken(), .StringLiteral).?;
        const slice = thisKind.contentSlice(p.getTokenText(p.tokenIdx));
        var charLiteralParser: TextLiteral.Parser = .{
            .comp = p.comp,
            .literal = slice,
            .kind = thisKind,
            .maxCodepoint = 0x10ffff,
            .loc = p.pp.tokens.items(.loc)[p.tokenIdx],
            .expansionLocs = p.pp.expansionSlice(p.tokenIdx),
            .incorrectEncodingIsError = count > 1,
        };

        try p.strings.ensureUnusedCapacity(gpa, (slice.len + 1) * @intFromEnum(charWidth));
        while (try charLiteralParser.next()) |item| switch (item) {
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
            .improperlyEncoded => |bytes| {
                if (count > 1) return error.ParsingFailed;
                p.strings.appendSliceAssumeCapacity(bytes);
            },
            .utf8Text => |view| {
                switch (charWidth) {
                    .@"1" => p.strings.appendSliceAssumeCapacity(view.bytes),
                    .@"2" => {
                        const capacitySlice: []align(@alignOf(u16)) u8 = @alignCast(p.strings.allocatedSlice()[literalStart..]);
                        const destLen = std.mem.alignBackward(usize, capacitySlice.len, 2);
                        const dest = std.mem.bytesAsSlice(u16, capacitySlice[0..destLen]);
                        const wordsWritten = std.unicode.utf8ToUtf16Le(dest, view.bytes) catch unreachable;
                        p.strings.resize(gpa, p.strings.items.len + wordsWritten * 2) catch unreachable;
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
    }

    p.strings.appendNTimesAssumeCapacity(0, @intFromEnum(charWidth));
    const slice = p.strings.items[literalStart..];

    const internedAlign = std.mem.alignForward(
        usize,
        p.comp.interner.strings.items.len,
        stringKind.internalStorageAlignment(p.comp),
    );
    try p.comp.interner.strings.resize(gpa, internedAlign);

    const val = try Value.intern(p.comp, .{ .bytes = slice });
    const arrayQt = try p.comp.typeStore.put(gpa, .{ .array = .{
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
                .kind = switch (stringKind) {
                    .char, .unterminated => .ascii,
                    .wide => .wide,
                    .utf8 => .utf8,
                    .utf16 => .utf16,
                    .utf32 => .utf32,
                },
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
    const gpa = p.comp.gpa;
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
                .array => try p.err(.generic_array_type, start, .{}),
                .func => try p.err(.generic_func_type, start, .{}),
                else => if (qt.isQualified()) {
                    try p.err(.generic_qual_type, start, .{});
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

            try p.listBuffer.append(gpa, res.node);
            try p.paramBuffer.append(gpa, .{ .name = undefined, .qt = qt, .nameToken = start, .node = undefined });

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
                    try p.err(.generic_duplicate, start, .{qt});
                    try p.err(.generic_duplicate_here, prevItem.nameToken, .{qt});
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
                try p.err(.generic_duplicate_default, tok, .{});
                try p.err(.previous_case, prev, .{});
            }

            default = res;
            defaultToken = tok;
        } else {
            if (p.listBuffer.items.len == listBufferTop) {
                try p.err(.expected_type, p.tokenIdx, .{});
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
            try p.err(.generic_no_match, controllingToken, .{controllingQt});
            return error.ParsingFailed;
        }
    } else if (defaultToken != null) {
        try p.listBuffer.append(gpa, default.node);
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

test "Node locations" {
    const allocator = std.testing.allocator;
    const io = std.testing.io;

    var arenaState: std.heap.ArenaAllocator = .init(allocator);
    defer arenaState.deinit();
    const arena = arenaState.allocator();

    var diagnostics: Diagnostics = .{ .output = .ignore };
    var comp = Compilation.init(allocator, arena, io, &diagnostics, std.fs.cwd());
    defer comp.deinit();

    const file = try comp.addSourceFromBuffer("file.c",
        \\int foo = 5;
        \\int bar = 10;
        \\int main(void) {}
        \\
    );

    const builtinMacros = try comp.generateBuiltinMacros(.NoSystemDefines);

    var pp = Preprocessor.init(&comp, .default);
    defer pp.deinit();
    try pp.addBuiltinMacros();

    _ = try pp.preprocess(builtinMacros);

    const eof = try pp.preprocess(file);
    try pp.addToken(eof);

    var tree = try Parser.parse(&pp);
    defer tree.deinit();

    try std.testing.expectEqual(0, comp.diagnostics.total);
    for (tree.rootDecls.items[tree.rootDecls.items.len - 3 ..], 0..) |node, i| {
        const slice = tree.tokenSlice(node.tok(&tree));
        const expected = switch (i) {
            0 => "foo",
            1 => "bar",
            2 => "main",
            else => unreachable,
        };
        try std.testing.expectEqualStrings(expected, slice);
    }
}
