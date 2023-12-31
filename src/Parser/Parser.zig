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
const Scope = @import("../Sema/Scope.zig").Scope;
const Result = @import("Result.zig");
const InitList = @import("InitList.zig");
const Attribute = @import("../Lexer/Attribute.zig");
const CharInfo = @import("../Basic/CharInfo.zig");
const Token = AST.Token;
const TokenIndex = AST.TokenIndex;
const NodeIndex = AST.NodeIndex;
const Allocator = std.mem.Allocator;
const NodeList = std.ArrayList(NodeIndex);

const Parser = @This();
pub const Error = Compilation.Error || error{ParsingFailed};

// values from pp
pp: *Preprocessor,
tokenIds: []const TokenType,
index: u32 = 0,

// value of incomplete AST
arena: Allocator,
nodes: AST.Node.List = .{},
data: NodeList,
strings: std.ArrayList(u8),
valueMap: AST.ValueMap,

// buffers used during compilation
scopes: std.ArrayList(Scope),
labels: std.ArrayList(Label),
listBuffer: NodeList,
declBuffer: NodeList,
paramBuffer: std.ArrayList(Type.Function.Param),
enumBuffer: std.ArrayList(Type.Enum.Field),
recordBuffer: std.ArrayList(Type.Record.Field),
attrBuffer: std.ArrayList(Attribute),

// configuration
noEval: bool = false,
inMacro: bool = false,
extensionSuppressd: bool = false,
containsAddresssOfLabel: bool = false,
returnType: ?Type = null,
funcName: TokenIndex = 0,
labelCount: u32 = 0,
/// location of first computed goto in function currently being parsed
/// if a computed goto is used, the function must contain an
/// address-of-label expression (tracked with ContainsAddressOfLabel)
computedGotoTok: ?TokenIndex = null,

const Label = union(enum) {
    unresolvedGoto: TokenIndex,
    label: TokenIndex,
};

fn checkIdentifierCodepoint(comp: *Compilation, codepoint: u21, loc: Source.Location) Compilation.Error!bool {
    if (codepoint <= 0x7F) return false;
    var diagnosed = false;
    if (!CharInfo.isC99IdChar(codepoint)) {
        try comp.diag.add(.{
            .tag = .c99_compat,
            .loc = loc,
        }, &.{});
        diagnosed = true;
    }
    if (CharInfo.isInvisible(codepoint)) {
        try comp.diag.add(.{
            .tag = .unicode_zero_width,
            .loc = loc,
            .extra = .{ .actualCodePoint = codepoint },
        }, &.{});
        diagnosed = true;
    }
    if (CharInfo.homoglyph(codepoint)) |resembles| {
        try comp.diag.add(.{
            .tag = .unicode_homoglyph,
            .loc = loc,
            .extra = .{ .codePoints = .{ .actual = codepoint, .resembles = resembles } },
        }, &.{});
        diagnosed = true;
    }
    return diagnosed;
}

fn eatIdentifier(p: *Parser) !?TokenIndex {
    switch (p.getCurrToken()) {
        .Identifier => {},
        .ExtendedIdentifier => {
            const slice = p.getTokenSlice(p.index);
            var it = std.unicode.Utf8View.initUnchecked(slice).iterator();
            var loc = p.pp.tokens.items(.loc)[p.index];

            if (std.mem.indexOfScalar(u8, slice, '$')) |i| {
                loc.byteOffset += @as(u32, @intCast(i));
                try p.pp.compilation.diag.add(.{
                    .tag = .dollar_in_identifier_extension,
                    .loc = loc,
                }, &.{});
                loc = p.pp.tokens.items(.loc)[p.index];
            }

            while (it.nextCodepoint()) |c| {
                if (try checkIdentifierCodepoint(p.pp.compilation, c, loc)) break;
                loc.byteOffset += std.unicode.utf8CodepointSequenceLength(c) catch unreachable;
            }
        },
        else => return null,
    }
    p.index += 1;

    // Handle illegal '$' characters in identifiers
    if (!p.pp.compilation.langOpts.dollarsInIdentifiers) {
        if (p.getCurrToken() == .Invalid and p.getTokenSlice(p.index)[0] == '$') {
            try p.err(.dollars_in_identifiers);
            p.index += 1;
            return error.ParsingFailed;
        }
    }

    return p.index - 1;
}

fn expectIdentifier(p: *Parser) Error!TokenIndex {
    if (p.getCurrToken() != .Identifier and p.getCurrToken() != .ExtendedIdentifier) {
        try p.errExtra(.expected_token, p.index, .{ .tokenId = .{
            .expected = .Identifier,
            .actual = p.getCurrToken(),
        } });
        return error.ParsingFailed;
    }
    return (try p.eatIdentifier()) orelse unreachable;
}

fn eat(p: *Parser, expected: TokenType) ?TokenIndex {
    std.debug.assert(expected != .Identifier and expected != .ExtendedIdentifier); // use eatIdentifier
    if (p.getCurrToken() == expected) {
        defer p.index += 1;
        return p.index;
    } else return null;
}

pub fn getCurrToken(p: *Parser) TokenType {
    return p.lookAhead(0);
}

pub fn lookAhead(p: *Parser, n: u32) TokenType {
    std.debug.assert(p.index + n < p.tokenIds.len);
    return p.tokenIds[p.index + n];
}

fn expectToken(p: *Parser, expected: TokenType) Error!TokenIndex {
    std.debug.assert(expected != .Identifier and expected != .ExtendedIdentifier); // use eatIdentifier
    const actual = p.getCurrToken();
    if (actual != expected) {
        switch (actual) {
            .Invalid => try p.errExtra(.expected_invalid, p.index, .{ .expectedTokenId = expected }),
            else => try p.errExtra(.expected_token, p.index, .{ .tokenId = .{
                .expected = expected,
                .actual = actual,
            } }),
        }
        return error.ParsingFailed;
    }

    defer p.index += 1;
    return p.index;
}

fn expectClosing(p: *Parser, opening: TokenIndex, id: TokenType) Error!void {
    _ = p.expectToken(id) catch |e|
        {
        if (e == error.ParsingFailed) {
            const token = p.pp.tokens.get(opening);
            try p.pp.compilation.diag.add(.{
                .tag = switch (id) {
                    .RParen => .to_match_paren,
                    .RBrace => .to_match_brace,
                    .RBracket => .to_match_bracket,
                    else => unreachable,
                },
                .loc = token.loc,
            }, token.expansionSlice());
        }

        return e;
    };
}

fn getTokenSlice(p: *Parser, index: TokenIndex) []const u8 {
    if (p.tokenIds[index].lexeMe()) |some|
        return some;

    const loc = p.pp.tokens.items(.loc)[index];
    var lexer = Lexer{
        .buffer = p.pp.compilation.getSource(loc.id).buffer,
        .comp = p.pp.compilation,
        .index = loc.byteOffset,
        .source = .generated,
    };

    const res = lexer.next();
    return lexer.buffer[res.start..res.end];
}

pub fn errStr(p: *Parser, tag: Diagnostics.Tag, index: TokenIndex, str: []const u8) Compilation.Error!void {
    @setCold(true);
    return p.errExtra(tag, index, .{ .str = str });
}

pub fn errExtra(p: *Parser, tag: Diagnostics.Tag, index: TokenIndex, extra: Diagnostics.Message.Extra) Compilation.Error!void {
    @setCold(true);
    const token = p.pp.tokens.get(index);
    try p.pp.compilation.diag.add(.{
        .tag = tag,
        .loc = token.loc,
        .extra = extra,
    }, token.expansionSlice());
}

pub fn errToken(p: *Parser, tag: Diagnostics.Tag, index: TokenIndex) Compilation.Error!void {
    @setCold(true);
    const token = p.pp.tokens.get(index);
    try p.pp.compilation.diag.add(.{
        .tag = tag,
        .loc = token.loc,
    }, token.expansionSlice());
}

pub fn err(p: *Parser, tag: Diagnostics.Tag) Compilation.Error!void {
    @setCold(true);
    return p.errToken(tag, p.index);
}

pub fn todo(p: *Parser, msg: []const u8) Error {
    try p.errStr(.todo, p.index, msg);
    return error.ParsingFailed;
}

pub fn ignoredAttrString(p: *Parser, attr: Attribute.Tag, context: Attribute.ParseContext) ![]const u8 {
    const stringTop = p.strings.items.len;
    defer p.strings.items.len = stringTop;

    try p.strings.writer().print("Attribute '{s}' ignored in {s} context", .{ @tagName(attr), @tagName(context) });
    return try p.arena.dupe(u8, p.strings.items[stringTop..]);
}

pub fn typeStr(p: *Parser, ty: Type) ![]const u8 {
    if (TypeBuilder.fromType(ty).toString()) |str| return str;
    const stringsTop = p.strings.items.len;
    defer p.strings.items.len = stringsTop;

    try ty.print(p.strings.writer());
    return try p.arena.dupe(u8, p.strings.items[stringsTop..]);
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
    return try p.arena.dupe(u8, p.strings.items[stringsTop..]);
}

pub fn addNode(p: *Parser, node: AST.Node) Allocator.Error!NodeIndex {
    if (p.inMacro)
        return .none;

    const res = p.nodes.len;
    try p.nodes.append(p.pp.compilation.gpa, node);

    return @as(NodeIndex, @enumFromInt(res));
}

fn addList(p: *Parser, nodes: []const NodeIndex) Allocator.Error!AST.Range {
    if (p.inMacro)
        return AST.Range{ .start = 0, .end = 0 };
    const start: u32 = @intCast(p.data.items.len);
    try p.data.appendSlice(nodes);
    const end: u32 = @intCast(p.data.items.len);

    return AST.Range{ .start = start, .end = end };
}

fn findTypedef(p: *Parser, nameToken: TokenIndex, notTypeYet: bool) !?Scope.Symbol {
    const name = p.getTokenSlice(nameToken);
    var i = p.scopes.items.len;
    while (i > 0) {
        i -= 1;
        switch (p.scopes.items[i]) {
            .typedef => |t| {
                if (std.mem.eql(u8, t.name, name))
                    return t;
            },
            .@"struct" => |s| if (std.mem.eql(u8, s.name, name)) {
                if (notTypeYet)
                    return null;
                try p.errStr(.must_use_struct, nameToken, name);
                return s;
            },
            .@"union" => |u| if (std.mem.eql(u8, u.name, name)) {
                if (notTypeYet)
                    return null;
                try p.errStr(.must_use_union, nameToken, name);
                return u;
            },
            .@"enum" => |e| if (std.mem.eql(u8, e.name, name)) {
                if (notTypeYet)
                    return null;
                try p.errStr(.must_use_enum, nameToken, name);
                return e;
            },
            .definition,
            .declaration,
            => |d| if (std.mem.eql(u8, d.name, name)) return null,

            else => {},
        }
    }

    return null;
}

fn inLoop(p: *Parser) bool {
    var i = p.scopes.items.len;
    while (i > 0) {
        i -= 1;
        switch (p.scopes.items[i]) {
            .loop => return true,
            else => {},
        }
    }

    return false;
}

fn inLoopOrSwitch(p: *Parser) bool {
    var i = p.scopes.items.len;
    while (i > 0) {
        i -= 1;
        switch (p.scopes.items[i]) {
            .loop, .@"switch" => return true,
            else => {},
        }
    }
    return false;
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

fn findSwitch(p: *Parser) ?*Scope.Switch {
    var i = p.scopes.items.len;
    while (i > 0) {
        i -= 1;
        switch (p.scopes.items[i]) {
            .@"switch" => |s| return s,
            else => {},
        }
    }

    return null;
}

fn nodeIs(p: *Parser, node: NodeIndex, tag: AstTag) bool {
    var cur = node;
    const tags = p.nodes.items(.tag);
    const data = p.nodes.items(.data);
    while (true) {
        const curTag = tags[@intFromEnum(cur)];
        if (curTag == .ParenExpr) {
            cur = data[@intFromEnum(cur)].UnaryExpr;
        } else if (curTag == tag) {
            return true;
        } else {
            return false;
        }
    }
}

fn findSymbol(p: *Parser, nameToken: TokenIndex, refKind: enum { reference, definition }) ?Scope {
    const name = p.getTokenSlice(nameToken);
    var i = p.scopes.items.len;
    while (i > 0) {
        i -= 1;
        const sym = p.scopes.items[i];
        switch (sym) {
            .definition, .declaration, .param => |s| if (std.mem.eql(u8, s.name, name)) return sym,
            .enumeration => |e| if (std.mem.eql(u8, e.name, name)) return sym,
            .block => if (refKind == .definition) return null,
            else => {},
        }
    }

    return null;
}

fn findTag(p: *Parser, kind: TokenType, nameToken: TokenIndex, refKind: enum { reference, definition }) !?Scope.Symbol {
    const name = p.getTokenSlice(nameToken);
    var i = p.scopes.items.len;
    var sawBlock = false;
    while (i > 0) {
        i -= 1;
        const sym = p.scopes.items[i];
        switch (sym) {
            .@"enum" => |e| if (std.mem.eql(u8, e.name, name)) {
                if (kind == .KeywordEnum) return e;
                if (sawBlock) return null;
                try p.errStr(.wrong_tag, nameToken, name);
                try p.errToken(.previous_definition, e.nameToken);
                return null;
            },

            .@"struct" => |s| if (std.mem.eql(u8, s.name, name)) {
                if (kind == .KeywordStruct) return s;
                if (sawBlock) return null;
                try p.errStr(.wrong_tag, nameToken, name);
                try p.errToken(.previous_definition, s.nameToken);
                return null;
            },

            .@"union" => |u| if (std.mem.eql(u8, u.name, name)) {
                if (kind == .KeywordUnion) return u;
                if (sawBlock) return null;

                try p.errStr(.wrong_tag, nameToken, name);
                try p.errToken(.previous_definition, u.nameToken);
                return null;
            },

            .block => if (refKind == .reference) {
                sawBlock = true;
            } else return null,

            else => {},
        }
    }
    return null;
}

fn pragma(p: *Parser) !bool {
    var foundPragma = false;
    while (p.eat(.KeywordPragma)) |pragmaToken| {
        foundPragma = true;
        const nameToken = p.index;
        const name = p.getTokenSlice(nameToken);
        const endIdx = std.mem.indexOfScalarPos(TokenType, p.tokenIds, p.index, .NewLine) orelse {
            try p.errToken(.pragma_inside_macro, pragmaToken);
            return error.ParsingFailed;
        };
        const pragmaLen = @as(TokenIndex, @intCast(endIdx)) - p.index;
        defer p.index += pragmaLen + 1; // skip past .nl as well
        if (p.pp.compilation.getPragma(name)) |prag| {
            try prag.parserCB(p, p.index);
        }
    }
    return foundPragma;
}

fn defineVaList(p: *Parser) !void {
    const Kind = enum { CharPtr, VoidPtr, AArch64VaList, X86_64VaList };
    const kind: Kind = switch (p.pp.compilation.target.cpu.arch) {
        .aarch64 => switch (p.pp.compilation.target.os.tag) {
            .windows => @as(Kind, .CharPtr),
            .ios, .macos, .tvos, .watchos => .CharPtr,
            else => .AArch64VaList,
        },
        .sparc, .wasm32, .wasm64, .bpfel, .bpfeb, .riscv32, .riscv64, .avr, .spirv32, .spirv64 => .VoidPtr,
        .powerpc => switch (p.pp.compilation.target.os.tag) {
            .ios, .macos, .tvos, .watchos, .aix => @as(Kind, .CharPtr),
            else => return, // unknown
        },
        .x86 => .CharPtr,
        .x86_64 => switch (p.pp.compilation.target.os.tag) {
            .windows => @as(Kind, .CharPtr),
            else => .X86_64VaList,
        },
        else => return, // unknown
    };

    var ty: Type = undefined;
    switch (kind) {
        .CharPtr => ty = .{ .specifier = .Char },
        .VoidPtr => ty = .{ .specifier = .Void },
        .AArch64VaList => {
            const recordType = try p.arena.create(Type.Record);
            recordType.* = .{
                .name = "__va_list_tag",
                .fields = try p.arena.alloc(Type.Record.Field, 5),
                .size = 32,
                .alignment = 8,
            };
            const voidType = try p.arena.create(Type);
            voidType.* = .{ .specifier = .Void };
            const voidPtr = Type{ .specifier = .Pointer, .data = .{ .subType = voidType } };
            recordType.fields[0] = .{ .name = "__stack", .ty = voidPtr };
            recordType.fields[1] = .{ .name = "__gr_top", .ty = voidPtr };
            recordType.fields[2] = .{ .name = "__vr_top", .ty = voidPtr };
            recordType.fields[3] = .{ .name = "__gr_offs", .ty = .{ .specifier = .Int } };
            recordType.fields[4] = .{ .name = "__vr_offs", .ty = .{ .specifier = .Int } };
            ty = .{ .specifier = .Struct, .data = .{ .record = recordType } };
        },
        .X86_64VaList => {
            const recordType = try p.arena.create(Type.Record);
            recordType.* = .{
                .name = "__va_list_tag",
                .fields = try p.arena.alloc(Type.Record.Field, 4),
                .size = 24,
                .alignment = 8,
            };
            const voidType = try p.arena.create(Type);
            voidType.* = .{ .specifier = .Void };
            const voidPtr = Type{ .specifier = .Pointer, .data = .{ .subType = voidType } };
            recordType.fields[0] = .{ .name = "gp_offset", .ty = .{ .specifier = .UInt } };
            recordType.fields[1] = .{ .name = "fp_offset", .ty = .{ .specifier = .UInt } };
            recordType.fields[2] = .{ .name = "overflow_arg_area", .ty = voidPtr };
            recordType.fields[3] = .{ .name = "reg_save_area", .ty = voidPtr };
            ty = .{ .specifier = .Struct, .data = .{ .record = recordType } };
        },
    }

    if (kind == .CharPtr or kind == .VoidPtr) {
        const elemType = try p.arena.create(Type);
        elemType.* = ty;
        ty = Type{ .specifier = .Pointer, .data = .{ .subType = elemType } };
    } else {
        const arrType = try p.arena.create(Type.Array);
        arrType.* = .{ .len = 1, .elem = ty };
        ty = Type{ .specifier = .Array, .data = .{ .array = arrType } };
    }

    const sym = Scope.Symbol{ .name = "__builtin_va_list", .type = ty, .nameToken = 0 };
    try p.scopes.append(.{ .typedef = sym });
}

/// root : (decl | inline assembly ';' | staticAssert)*
pub fn parse(pp: *Preprocessor) Compilation.Error!AST {
    pp.compilation.pragmaEvent(.BeforeParse);

    var arena = std.heap.ArenaAllocator.init(pp.compilation.gpa);
    errdefer arena.deinit();

    var p = Parser{
        .pp = pp,
        .arena = arena.allocator(),
        .tokenIds = pp.tokens.items(.id),
        .scopes = std.ArrayList(Scope).init(pp.compilation.gpa),
        .data = NodeList.init(pp.compilation.gpa),
        .labels = std.ArrayList(Label).init(pp.compilation.gpa),
        .strings = std.ArrayList(u8).init(pp.compilation.gpa),
        .valueMap = AST.ValueMap.init(pp.compilation.gpa),
        .listBuffer = NodeList.init(pp.compilation.gpa),
        .declBuffer = NodeList.init(pp.compilation.gpa),
        .paramBuffer = std.ArrayList(Type.Function.Param).init(pp.compilation.gpa),
        .enumBuffer = std.ArrayList(Type.Enum.Field).init(pp.compilation.gpa),
        .recordBuffer = std.ArrayList(Type.Record.Field).init(pp.compilation.gpa),
        .attrBuffer = std.ArrayList(Attribute).init(pp.compilation.gpa),
    };

    defer {
        p.scopes.deinit();
        p.data.deinit();
        p.labels.deinit();
        p.listBuffer.deinit();
        p.declBuffer.deinit();
        p.paramBuffer.deinit();
        p.enumBuffer.deinit();
        p.recordBuffer.deinit();
        p.attrBuffer.deinit();
    }

    errdefer {
        p.nodes.deinit(pp.compilation.gpa);
        p.strings.deinit();
        p.valueMap.deinit();
    }

    _ = try p.addNode(.{ .tag = .Invalid, .type = undefined, .data = undefined });
    try p.defineVaList();

    while (p.eat(.Eof) == null) {
        const foundPragma = p.pragma() catch |er| switch (er) {
            error.ParsingFailed => break,
            else => |e| return e,
        };
        if (foundPragma)
            continue;

        if (p.parseStaticAssert() catch |er| switch (er) {
            error.ParsingFailed => {
                p.nextExternDecl();
                continue;
            },
            else => |e| return e,
        }) continue;

        if (p.parseDeclaration() catch |er| switch (er) {
            error.ParsingFailed => {
                p.nextExternDecl();
                continue;
            },
            else => |e| return e,
        }) continue;

        if (p.eat(.KeywordGccExtension)) |_| {
            const saveExtension = p.extensionSuppressd;
            defer p.extensionSuppressd = saveExtension;

            p.extensionSuppressd = true;

            if (p.parseDeclaration() catch |er| switch (er) {
                error.ParsingFailed => {
                    p.nextExternDecl();
                    continue;
                },
                else => |e| return e,
            }) continue;
        }

        if (p.assembly(.global) catch |er| switch (er) {
            error.ParsingFailed => {
                p.nextExternDecl();
                continue;
            },
            else => |e| return e,
        }) |_| continue;

        try p.err(.expected_external_decl);
        p.index += 1;
    }

    const rootDecls = try p.declBuffer.toOwnedSlice();
    if (rootDecls.len == 0)
        try p.errToken(.empty_translation_unit, p.index - 1);

    const data = try p.data.toOwnedSlice();
    errdefer pp.compilation.gpa.free(data);

    pp.compilation.pragmaEvent(.AfterParse);

    return AST{
        .comp = pp.compilation,
        .tokens = pp.tokens.slice(),
        .arena = arena,
        .generated = pp.compilation.generatedBuffer.items,
        .nodes = p.nodes.toOwnedSlice(),
        .data = data,
        .rootDecls = rootDecls,
        .strings = try p.strings.toOwnedSlice(),
        .valueMap = p.valueMap,
    };
}

fn nextExternDecl(p: *Parser) void {
    var parens: u32 = 0;
    while (true) : (p.index += 1) {
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
                p.index += 1;
                return;
            },
            else => {},
        }
    }
}

fn skipToPragmaSentinel(p: *Parser) void {
    while (true) : (p.index += 1) {
        if (p.getCurrToken() == .NewLine) return;
        if (p.getCurrToken() == .Eof) {
            p.index -= 1;
            return;
        }
    }
}

fn skipTo(p: *Parser, id: TokenType) void {
    var parens: u32 = 0;
    while (true) : (p.index += 1) {
        if (p.getCurrToken() == id and parens == 0) {
            p.index += 1;
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

// ====== declarations ======
/// decl
///  : declSpec (initDeclarator ( ',' initDeclarator)*)? ';'
///  | declSpec declarator decl* compoundStmt
///  | staticAssert
fn parseDeclaration(p: *Parser) Error!bool {
    _ = try p.pragma();
    const firstTokenIndex = p.index;
    const attrBufferTop = p.attrBuffer.items.len;
    defer p.attrBuffer.items.len = attrBufferTop;
    // TODO: at this point we don't know what we're trying to parse, so we'll need to check
    // the attributes against what kind of decl was parsed after the fact
    try p.parseAttributeSpecifier(.any);

    var declSpec = if (try p.declSpecifier(false)) |some| some else blk: {
        if (p.returnType != null) {
            p.index = firstTokenIndex;
            return false;
        }

        switch (p.tokenIds[firstTokenIndex]) {
            .Asterisk, .LParen, .Identifier => {},
            else => return false,
        }

        var spec: TypeBuilder = .{};
        break :blk DeclSpec{ .type = try spec.finish(p) };
    };

    var initD = (try p.parseInitDeclarator(&declSpec)) orelse {
        // eat ';'
        _ = try p.expectToken(.Semicolon);
        if (declSpec.type.is(.Enum) or (declSpec.type.isRecord() and !declSpec.type.isAnonymousRecord() and !declSpec.type.isTypeof()))
            return true;

        try p.errToken(.missing_declaration, firstTokenIndex);
        return true;
    };

    // check for funtion definition
    if (initD.d.funcDeclarator != null and initD.initializer == .none and initD.d.type.isFunc()) fndef: {
        switch (p.getCurrToken()) {
            .Comma, .Semicolon => break :fndef,
            .LBrace => {},
            else => {
                if (initD.d.oldTypeFunc == null) {
                    try p.err(.expected_fn_body);
                    return true;
                }
            },
        }

        if (p.returnType != null)
            try p.err(.func_not_in_root);

        if (p.findSymbol(initD.d.name, .definition)) |sym| {
            if (sym == .definition) {
                try p.errStr(.redefinition, initD.d.name, p.getTokenSlice(initD.d.name));
                try p.errToken(.previous_definition, sym.definition.nameToken);
            }
        } else {
            try p.scopes.append(.{ .definition = .{
                .name = p.getTokenSlice(initD.d.name),
                .type = initD.d.type,
                .nameToken = initD.d.name,
            } });
        }

        const returnType = p.returnType;
        const funcName = p.funcName;
        p.returnType = initD.d.type.data.func.returnType;
        p.funcName = initD.d.name;

        defer {
            p.returnType = returnType;
            p.funcName = funcName;
        }

        const scopesTop = p.scopes.items.len;
        defer p.scopes.items.len = scopesTop;

        // findSymbol stops the search at .block
        try p.scopes.append(.block);

        // collect old style parameters
        if (initD.d.oldTypeFunc != null) {
            const paramBufferTop = p.paramBuffer.items.len;
            defer p.paramBuffer.items.len = paramBufferTop;

            initD.d.type.specifier = .Func;
            paramLoop: while (true) {
                const paramDeclSpec = (try p.declSpecifier(true)) orelse break;
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
                    for (initD.d.type.data.func.params) |*param| {
                        if (std.mem.eql(u8, param.name, name)) {
                            param.ty = d.type;
                            break;
                        }
                    } else {
                        try p.errStr(.parameter_missing, d.name, name);
                    }

                    try p.scopes.append(.{ .param = .{ .name = name, .nameToken = d.name, .type = d.type } });
                    if (p.eat(.Comma) == null) break;
                }

                _ = try p.expectToken(.Semicolon);
            }
        } else {
            for (initD.d.type.data.func.params) |param| {
                if (param.ty.hasUnboundVLA())
                    try p.errToken(.unbound_vla, param.nameToken);
                if (param.ty.hasIncompleteSize() and !param.ty.is(.Void))
                    try p.errStr(.parameter_incomplete_ty, param.nameToken, try p.typeStr(param.ty));
                if (param.name.len == 0) {
                    try p.errToken(.omitting_parameter_name, param.nameToken);
                    continue;
                }

                try p.scopes.append(.{
                    .param = .{
                        .name = param.name,
                        .type = param.ty,
                        .nameToken = param.nameToken,
                    },
                });
            }
        }

        const body = (try p.parseCompoundStmt(true)) orelse {
            std.debug.assert(initD.d.oldTypeFunc != null);
            try p.err(.expected_fn_body);
            return true;
        };
        const node = try p.addNode(.{
            .type = initD.d.type,
            .tag = try declSpec.validateFnDef(p),
            .data = .{ .Declaration = .{ .name = initD.d.name, .node = body } },
        });
        try p.declBuffer.append(node);

        // check gotos
        if (returnType == null) {
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
        if (initD.d.oldTypeFunc) |tokenIdx|
            try p.errToken(.invalid_old_style_params, tokenIdx);

        const tag = try declSpec.validate(p, &initD.d.type, initD.initializer != .none);
        const attrs = p.attrBuffer.items[attrBufferTop..];
        initD.d.type = try initD.d.type.withAttributes(p.arena, attrs);

        const node = try p.addNode(.{
            .type = initD.d.type,
            .tag = tag,
            .data = .{ .Declaration = .{ .name = initD.d.name, .node = initD.initializer } },
        });
        try p.declBuffer.append(node);

        const sym = Scope.Symbol{
            .name = p.getTokenSlice(initD.d.name),
            .type = initD.d.type,
            .nameToken = initD.d.name,
        };
        if (declSpec.storageClass == .typedef) {
            try p.scopes.append(.{ .typedef = sym });
        } else if (initD.initializer != .none) {
            try p.scopes.append(.{ .definition = sym });
        } else {
            try p.scopes.append(.{ .declaration = sym });
        }

        if (p.eat(.Comma) == null)
            break;

        initD = (try p.parseInitDeclarator(&declSpec)) orelse {
            try p.err(.expected_ident_or_l_paren);
            continue;
        };
    }

    _ = try p.expectToken(.Semicolon);
    return true;
}

/// staticAssert : keyword_static_assert '(' constExpr ',' STRING_LITERAL ')' ';'
fn parseStaticAssert(p: *Parser) Error!bool {
    const curToken = p.eat(.KeywordStaticAssert) orelse return false;
    const lp = try p.expectToken(.LParen);
    const resToken = p.index;
    const res = try p.constExpr();

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

    if (res.value == .unavailable) {
        // an unavailable sizeof expression is already a compile error, so we don't emit
        // another error for an invalid _Static_assert condition. This matches the behavior
        // of gcc/clang
        if (!p.nodeIs(res.node, .SizeOfExpr))
            try p.errToken(.static_assert_not_constant, resToken);
    } else if (!res.getBool()) {
        if (str.node != .none) {
            var buffer = std.ArrayList(u8).init(p.pp.compilation.gpa);
            defer buffer.deinit();

            const data = p.nodes.items(.data)[@intFromEnum(str.node)].String;
            try buffer.ensureUnusedCapacity(data.len);
            try AST.dumpString(
                p.strings.items[data.index..][0..data.len],
                p.nodes.items(.tag)[@intFromEnum(str.node)],
                buffer.writer(),
            );

            try p.errStr(
                .static_assert_failure_message,
                curToken,
                try p.arena.dupe(u8, buffer.items),
            );
        } else try p.errToken(.static_assert_failure, curToken);
    }

    const node = try p.addNode(.{
        .tag = .StaticAssert,
        .data = .{
            .BinaryExpr = .{
                .lhs = res.node,
                .rhs = str.node,
            },
        },
    });

    try p.declBuffer.append(node);
    return true;
}

/// typeof
///   : keyword_typeof '(' typeName ')'
///   | keyword_typeof '(' expr ')'
fn typeof(p: *Parser) Error!?Type {
    switch (p.getCurrToken()) {
        .KeywordGccTypeof, .KeywordTypeof1, .KeywordTypeof2 => p.index += 1,
        else => return null,
    }

    const lp = try p.expectToken(.LParen);
    if (try p.typeName()) |ty| {
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

/// declSpec: (storageClassSpec | typeSpec | typeQual | funcSpec | alignSpec)+
/// storageClassSpec:
///  : keyword_typedef
///  | keyword_extern
///  | keyword_static
///  | keyword_threadlocal
///  | keyword_auto
///  | keyword_register
fn declSpecifier(p: *Parser, isParam: bool) Error!?DeclSpec {
    var d: DeclSpec = .{};
    var spec: TypeBuilder = .{};

    const start = p.index;
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
                    try p.errStr(.multiple_storage_class, p.index, @tagName(d.storageClass));

                    return error.ParsingFailed;
                }

                if (d.threadLocal != null) {
                    switch (token) {
                        .KeywordTypedef,
                        .KeywordAuto,
                        .KeywordRegister,
                        => try p.errStr(.cannot_combine_spec, p.index, token.lexeMe().?),

                        else => {},
                    }
                }

                switch (token) {
                    .KeywordTypedef => d.storageClass = .{ .typedef = p.index },
                    .KeywordExtern => d.storageClass = .{ .@"extern" = p.index },
                    .KeywordStatic => d.storageClass = .{ .static = p.index },
                    .KeywordAuto => d.storageClass = .{ .auto = p.index },
                    .KeywordRegister => d.storageClass = .{ .register = p.index },
                    else => unreachable,
                }
            },

            .KeywordThreadLocal => {
                if (d.threadLocal != null) {
                    try p.errStr(.duplicate_declspec, p.index, "_Thread_local");
                }

                switch (d.storageClass) {
                    .@"extern", .none, .static => {},
                    else => try p.errStr(.cannot_combine_spec, p.index, @tagName(d.storageClass)),
                }

                d.threadLocal = p.index;
            },

            .KeywordInline, .KeywordGccInline1, .KeywordGccInline2 => {
                if (d.@"inline" != null) {
                    try p.errStr(.duplicate_declspec, p.index, "inline");
                }

                d.@"inline" = p.index;
            },

            .KeywordNoreturn => {
                if (d.noreturn != null) {
                    try p.errStr(.duplicate_declspec, p.index, "_Noreturn");
                }

                d.noreturn = null;
            },
            else => break,
        }

        p.index += 1;
    }

    if (p.index == start) return null;
    if (isParam and spec.alignToken != null) {
        try p.errToken(.alignas_on_param, spec.alignToken.?);
        spec.alignToken = null;
    }

    d.type = try spec.finish(p);
    return d;
}

/// attribute
///  : attrIdentifier
///  | attrIdentifier '(' identifier ')'
///  | attrIdentifier '(' identifier (',' expr)+ ')'
///  | attrIdentifier '(' (expr (',' expr)*)? ')'
fn attribute(p: *Parser) Error!Attribute {
    const nameToken = p.index;
    switch (p.getCurrToken()) {
        .KeywordConst, .KeywordGccConst1, .KeywordGccConst2 => p.index += 1,
        else => _ = try p.expectIdentifier(),
    }

    switch (p.getCurrToken()) {
        .Comma, .RParen => { // will be consumed in attributeList
            return Attribute{ .name = nameToken };
        },

        .LParen => {
            p.index += 1;
            if (p.eat(.RParen)) |_|
                return Attribute{ .name = nameToken };

            const maybeIdent = try p.eatIdentifier();
            if (maybeIdent != null and p.eat(.RParen) != null) {
                const argNode = try p.addNode(.{
                    .tag = .AttrArgIdentifier,
                    .type = .{ .specifier = .Void },
                    .data = .{ .DeclarationRef = maybeIdent.? },
                });
                return Attribute{
                    .name = nameToken,
                    .params = argNode,
                };
            }
            const listBufferTop = p.listBuffer.items.len;
            defer p.listBuffer.items.len = listBufferTop;

            if (maybeIdent) |ident| {
                const argNode = try p.addNode(.{
                    .tag = .AttrArgIdentifier,
                    .type = .{ .specifier = .Void },
                    .data = .{ .DeclarationRef = ident },
                });
                try p.listBuffer.append(argNode);
            } else {
                var firstExpr = try p.assignExpr();
                try firstExpr.expect(p);
                try firstExpr.saveValue(p);
                try p.listBuffer.append(firstExpr.node);
            }

            while (p.getCurrToken() != .RParen) {
                _ = try p.expectToken(.Comma);

                var attrExpr = try p.assignExpr();
                try attrExpr.expect(p);
                try attrExpr.saveValue(p);

                try p.listBuffer.append(attrExpr.node);
            }
            p.index += 1; // eat closing r_paren

            const items = p.listBuffer.items[listBufferTop..];
            std.debug.assert(items.len > 0);

            var node: AST.Node = .{
                .tag = .AttrParamsTwo,
                .type = .{ .specifier = .Void },
                .data = .{ .BinaryExpr = .{ .lhs = items[0], .rhs = .none } },
            };
            switch (items.len) {
                0 => unreachable,
                1 => {},
                2 => node.data.BinaryExpr.rhs = items[1],
                else => {
                    node.tag = .AttrParams;
                    node.data = .{ .range = try p.addList(items) };
                },
            }
            return Attribute{
                .name = nameToken,
                .params = try p.addNode(node),
            };
        },
        else => return error.ParsingFailed,
    }
}

fn validateAttr(p: *Parser, attr: Attribute, context: Attribute.ParseContext) Error!bool {
    const name = p.getTokenSlice(attr.name);
    if (Attribute.Tag.fromString(name)) |tag| {
        if (tag.allowedInContext(context))
            return true;

        if (context == .statement) {
            try p.errToken(.cannot_apply_attribute_to_statement, attr.name);
            return error.ParsingFailed;
        }
        try p.errStr(.ignored_attribute, attr.name, try p.ignoredAttrString(tag, context));
    } else {
        try p.errStr(.unknown_attribute, attr.name, name);
    }

    return false;
}

/// attributeList : (attribute (',' attribute)*)?
fn parseAttributeList(p: *Parser, context: Attribute.ParseContext) Error!void {
    if (p.getCurrToken() != .RParen) {
        const attr = try p.attribute();
        if (try p.validateAttr(attr, context)) {
            try p.attrBuffer.append(attr);
        }
        while (p.getCurrToken() != .RParen) {
            _ = try p.expectToken(.Comma);
            const nextAttr = try p.attribute();
            if (try p.validateAttr(nextAttr, context)) {
                try p.attrBuffer.append(nextAttr);
            }
        }
    }
}

/// attributeSpecifier : (__attribute__ '( '(' attributeList ')' ')')*
fn parseAttributeSpecifier(p: *Parser, context: Attribute.ParseContext) Error!void {
    while (p.getCurrToken() == .KeywordAttribute1 or p.getCurrToken() == .KeywordAttribute2) {
        p.index += 1;
        const paren1 = try p.expectToken(.LParen);
        const paren2 = try p.expectToken(.LParen);

        try p.parseAttributeList(context);

        _ = try p.expectClosing(paren2, .RParen);
        _ = try p.expectClosing(paren1, .RParen);
    }
}

const InitDeclarator = struct { d: Declarator, initializer: NodeIndex = .none };

/// initDeclarator : declarator assembly? attributeSpecifier? ('=' initializer)?
fn parseInitDeclarator(p: *Parser, declSpec: *DeclSpec) Error!?InitDeclarator {
    var initD = InitDeclarator{ .d = (try p.declarator(declSpec.type, .normal)) orelse return null };
    _ = try p.assembly(.decl);
    try p.parseAttributeSpecifier(if (initD.d.type.isFunc()) .function else .variable);

    if (p.eat(.Equal)) |eq| init: {
        if (initD.d.type.hasIncompleteSize() and !initD.d.type.isArray()) {
            try p.errStr(.variable_incomplete_ty, initD.d.name, try p.typeStr(initD.d.type));
            return error.ParsingFailed;
        }

        if (declSpec.storageClass == .typedef or initD.d.funcDeclarator != null)
            try p.errToken(.illegal_initializer, eq)
        else if (initD.d.type.is(.VariableLenArray))
            try p.errToken(.vla_init, eq)
        else if (declSpec.storageClass == .@"extern") {
            try p.err(.extern_initializer);
            declSpec.storageClass = .none;
        }

        var initListExpr = try p.initializer(initD.d.type);
        initD.initializer = initListExpr.node;
        // int j [] = c; // c -> *int
        if (!initListExpr.ty.isArray()) break :init;
        if (initD.d.type.specifier == .IncompleteArray) {
            initD.d.type.data.array.len = initListExpr.ty.data.array.len;
            initD.d.type.specifier = .Array;
        } else if (initD.d.type.is(.IncompleteArray)) {
            const arrayType = try p.arena.create(Type.Array);
            arrayType.* = .{
                .elem = initD.d.type.getElemType(),
                .len = initListExpr.ty.arrayLen().?,
            };
            initD.d.type = .{
                .specifier = .Array,
                .data = .{ .array = arrayType },
                .alignment = initD.d.type.alignment,
            };
        }
    }

    const name = initD.d.name;
    if (declSpec.storageClass != .typedef and initD.d.type.hasIncompleteSize()) incomplete: {
        const specifier = initD.d.type.canonicalize(.standard).specifier;
        if (declSpec.storageClass == .@"extern") switch (specifier) {
            .Struct, .Union, .Enum => break :incomplete,
            .IncompleteArray => {
                initD.d.type.decayArray();
                break :incomplete;
            },
            else => {},
        };

        // if there was an initializer expression it must have contained an error
        if (initD.initializer != .none)
            break :incomplete;
        try p.errStr(.variable_incomplete_ty, name, try p.typeStr(initD.d.type));
        return initD;
    }

    if (p.findSymbol(name, .definition)) |scope| switch (scope) {
        .enumeration => {
            try p.errStr(.redefinition_different_sym, name, p.getTokenSlice(name));
            try p.errToken(.previous_definition, scope.enumeration.nameToken);
        },

        .declaration => |s| if (!s.type.eql(initD.d.type, true)) {
            try p.errStr(.redefinition, name, p.getTokenSlice(name));
            try p.errToken(.previous_definition, s.nameToken);
        },

        .definition => |s| if (!s.type.eql(initD.d.type, true)) {
            try p.errStr(.redefinition_incompatible, name, p.getTokenSlice(name));
            try p.errToken(.previous_definition, s.nameToken);
        } else if (initD.initializer != .none) {
            try p.errStr(.redefinition, name, p.getTokenSlice(name));
            try p.errToken(.previous_definition, s.nameToken);
        },

        .param => |s| {
            try p.errStr(.redefinition, name, p.getTokenSlice(name));
            try p.errToken(.previous_definition, s.nameToken);
        },

        else => unreachable,
    };

    return initD;
}

/// typeSpec
///  : keyword_void
///  | keyword_char
///  | keyword_short
///  | keyword_int
///  | keyword_long
///  | keyword_float
///  | keyword_double
///  | keyword_signed
///  | keyword_unsigned
///  | keyword_bool
///  | keyword_complex
///  | atomicTypeSpec
///  | recordSpec
///  | enumSpec
///  | typedef  // IDENTIFIER
///  | typeof
/// atomicTypeSpec : keyword_atomic '(' typeName ')'
/// alignSpec
///   : keyword_alignas '(' typeName ')'
///   | keyword_alignas '(' constExpr ')'
fn parseTypeSpec(p: *Parser, ty: *TypeBuilder) Error!bool {
    const start = p.index;
    while (true) {
        try p.parseAttributeSpecifier(.typedef);
        if (try p.typeof()) |innerType| {
            try ty.combineFromTypeof(p, innerType, start);
            continue;
        }

        if (try p.parseTypeQual(&ty.qual))
            continue;

        switch (p.getCurrToken()) {
            .KeywordVoid => try ty.combine(p, .Void, p.index),
            .KeywordBool => try ty.combine(p, .Bool, p.index),
            .KeywordChar => try ty.combine(p, .Char, p.index),
            .KeywordShort => try ty.combine(p, .Short, p.index),
            .KeywordInt => try ty.combine(p, .Int, p.index),
            .KeywordLong => try ty.combine(p, .Long, p.index),
            .KeywordSigned => try ty.combine(p, .Signed, p.index),
            .KeywordUnsigned => try ty.combine(p, .Unsigned, p.index),
            .KeywordFloat => try ty.combine(p, .Float, p.index),
            .KeywordDouble => try ty.combine(p, .Double, p.index),
            .KeywordComplex => try ty.combine(p, .Complex, p.index),

            .KeywordAtomic => {
                const atomicToken = p.index;
                p.index += 1;
                const lp = p.eat(.LParen) orelse {
                    // _Atomic qualifier not _Atomic(typeName)
                    p.index = atomicToken;
                    break;
                };
                const innerType = (try p.typeName()) orelse {
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
                const tagToken = p.index;
                try ty.combine(p, .{ .Enum = try p.parseEnumSpec() }, tagToken);
                continue;
            },

            .KeywordStruct => {
                const tagToken = p.index;
                try ty.combine(p, .{ .Struct = try p.parseRecordSpec() }, tagToken);
                continue;
            },

            .KeywordUnion => {
                const tagToken = p.index;
                try ty.combine(p, .{ .Union = try p.parseRecordSpec() }, tagToken);
                continue;
            },

            .KeywordAlignas => {
                if (ty.alignToken != null)
                    try p.errStr(.duplicate_declspec, p.index, "alignment");

                ty.alignToken = p.index;
                p.index += 1;

                const lp = try p.expectToken(.LParen);
                if (try p.typeName()) |innerType| {
                    ty.alignment = innerType.alignment;
                } else blk: {
                    const res = try p.constExpr();
                    if (res.value == .unavailable) {
                        try p.errToken(.alignas_unavailable, ty.alignToken.?);
                        break :blk;
                    } else if (res.value == .signed and res.value.signed < 0) {
                        try p.errExtra(.negative_alignment, ty.alignToken.?, .{ .signed = res.value.signed });
                        break :blk;
                    }
                    var requested = std.math.cast(u29, res.asU64());
                    if (requested == null) {
                        try p.errExtra(.maximum_alignment, ty.alignToken.?, .{ .unsigned = res.asU64() });
                        break :blk;
                    }

                    if (requested == 0) {
                        try p.errToken(.zero_align_ignored, ty.alignToken.?);
                    } else if (!std.mem.isValidAlign(requested.?)) {
                        requested = 0;
                        try p.errToken(.non_pow2_align, ty.alignToken.?);
                    }
                    ty.alignment = requested.?;
                }

                try p.expectClosing(lp, .RParen);
                continue;
            },

            .Identifier, .ExtendedIdentifier => {
                const typedef = (try p.findTypedef(p.index, ty.specifier != .None)) orelse break;
                if (!(try ty.combineTypedef(p, typedef.type, typedef.nameToken)))
                    break;
            },
            else => break,
        }

        if (try p.eatIdentifier()) |_| {} else p.index += 1;
    }

    return p.index != start;
}

fn getAnonymousName(p: *Parser, kindToken: TokenIndex) ![]const u8 {
    const loc = p.pp.tokens.items(.loc)[kindToken];
    const source = p.pp.compilation.getSource(loc.id);
    const col = source.getLineCol(loc.byteOffset).col;

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
        .{ kindStr, source.path, loc.line, col },
    );
}

/// recordSpec
///  : (keyword_struct | keyword_union) IDENTIFIER? { recordDecl* }
///  | (keyword_struct | keyword_union) IDENTIFIER
fn parseRecordSpec(p: *Parser) Error!*Type.Record {
    const kindToken = p.index;
    const isStruct = p.tokenIds[kindToken] == .KeywordStruct;
    p.index += 1;

    const attrBufferTop = p.attrBuffer.items.len;
    defer p.attrBuffer.items.len = attrBufferTop;

    try p.parseAttributeSpecifier(.record);

    const maybeIdent = try p.eatIdentifier();
    const lb = p.eat(.LBrace) orelse {
        const ident = maybeIdent orelse {
            try p.err(.ident_or_l_brace);
            return error.ParsingFailed;
        };

        // check if this is a reference to a previous type
        if (try p.findTag(p.tokenIds[kindToken], ident, .reference)) |prev| {
            return prev.type.data.record;
        } else {
            // this is a forward declaration, create a new record type.
            const recordType = try Type.Record.create(p.arena, p.getTokenSlice(ident));
            const ty = Type{
                .specifier = if (isStruct) .Struct else .Union,
                .data = .{ .record = recordType },
            };
            const sym = Scope.Symbol{ .name = recordType.name, .type = ty, .nameToken = ident };
            try p.scopes.append(if (isStruct) .{ .@"struct" = sym } else .{ .@"union" = sym });
            return recordType;
        }
    };

    // Get forward declared type or create a new one
    var defined = false;
    const recordType: *Type.Record = if (maybeIdent) |ident| recordTy: {
        if (try p.findTag(p.tokenIds[kindToken], ident, .definition)) |prev| {
            if (!prev.type.data.record.isIncomplete()) {
                // if the record isn't incomplete, this is a redefinition
                try p.errStr(.redefinition, ident, p.getTokenSlice(ident));
                try p.errToken(.previous_definition, prev.nameToken);
            } else {
                defined = true;
                break :recordTy prev.type.data.record;
            }
        }
        break :recordTy try Type.Record.create(p.arena, p.getTokenSlice(ident));
    } else try Type.Record.create(p.arena, try p.getAnonymousName(kindToken));

    const ty = Type{
        .specifier = if (isStruct) .Struct else .Union,
        .data = .{ .record = recordType },
    };

    // declare a symbol for the type
    if (maybeIdent != null and !defined) {
        const sym = Scope.Symbol{
            .name = recordType.name,
            .type = ty,
            .nameToken = maybeIdent.?,
        };
        try p.scopes.append(if (isStruct) .{ .@"struct" = sym } else .{ .@"union" = sym });
    }

    // reserve space for this record
    try p.declBuffer.append(.none);
    const declBufferTop = p.declBuffer.items.len;
    const recordBufferTop = p.recordBuffer.items.len;
    errdefer p.declBuffer.items.len = declBufferTop - 1;

    defer {
        p.declBuffer.items.len = declBufferTop;
        p.recordBuffer.items.len = recordBufferTop;
    }

    try p.recordDecls();
    recordType.fields = try p.arena.dupe(Type.Record.Field, p.recordBuffer.items[recordBufferTop..]);
    recordType.size = 1;
    recordType.alignment = 1;

    if (p.recordBuffer.items.len == recordBufferTop)
        try p.errStr(.empty_record, kindToken, p.getTokenSlice(kindToken));

    try p.expectClosing(lb, .RBrace);
    try p.parseAttributeSpecifier(.record);

    // finish by creating a node
    var node: AST.Node = .{
        .tag = if (isStruct) .StructDeclTwo else .UnionDeclTwo,
        .type = ty,
        .data = .{ .BinaryExpr = .{ .lhs = .none, .rhs = .none } },
    };
    const recordDLS = p.declBuffer.items[declBufferTop..];
    switch (recordDLS.len) {
        0 => {},
        1 => node.data = .{ .BinaryExpr = .{ .lhs = recordDLS[0], .rhs = .none } },
        2 => node.data = .{ .BinaryExpr = .{ .lhs = recordDLS[0], .rhs = recordDLS[1] } },
        else => {
            node.tag = if (isStruct) .StructDecl else .UnionDecl;
            node.data = .{ .range = try p.addList(recordDLS) };
        },
    }

    p.declBuffer.items[declBufferTop - 1] = try p.addNode(node);
    return recordType;
}
/// recordDecl
///  : specQual+ (recordDeclarator (',' recordDeclarator)*)? ;
///  | staticAssert
fn recordDecls(p: *Parser) Error!void {
    while (true) {
        if (try p.pragma()) continue;
        if (try p.parseStaticAssert()) continue;
        if (p.eat(.KeywordGccExtension)) |_| {
            const saveExtension = p.extensionSuppressd;
            defer p.extensionSuppressd = saveExtension;
            p.extensionSuppressd = true;

            if (p.recordDeclarator() catch |e| switch (e) {
                error.ParsingFailed => {
                    p.nextExternDecl();
                    continue;
                },
                else => |er| return er,
            }) continue;

            try p.err(.expected_type);
            p.nextExternDecl();
            continue;
        }

        if (p.recordDeclarator() catch |e| switch (e) {
            error.ParsingFailed => {
                p.nextExternDecl();
                continue;
            },
            else => |er| return er,
        }) continue;
        break;
    }
}

/// recordDeclarator : declarator (':' constExpr)?
fn recordDeclarator(p: *Parser) Error!bool {
    const attrBuffTop = p.attrBuffer.items.len;
    defer p.attrBuffer.items.len = attrBuffTop;
    const baseType = (try p.specQual()) orelse return false;

    while (true) {
        const thisDeclTop = p.attrBuffer.items.len;
        defer p.attrBuffer.items.len = thisDeclTop;

        try p.parseAttributeSpecifier(.record);
        // 0 means unnamed
        var nameToken: TokenIndex = 0;
        var ty = baseType;
        var bitsNode: NodeIndex = .none;
        var bits: u32 = 0;
        const firstToken = p.index;
        if (try p.declarator(ty, .record)) |d| {
            nameToken = d.name;
            ty = d.type;
        }

        try p.parseAttributeSpecifier(.record);
        const attrs = p.attrBuffer.items[attrBuffTop..];
        ty = try ty.withAttributes(p.arena, attrs);

        if (p.eat(.Colon)) |_| bits: {
            const res = try p.constExpr();
            if (!ty.isInt()) {
                try p.errStr(.non_int_bitfield, firstToken, try p.typeStr(ty));
                break :bits;
            }

            if (res.value == .unavailable) {
                try p.errToken(.expected_integer_constant_expr, firstToken);
                break :bits;
            } else if (res.value == .signed and res.value.signed < 0) {
                try p.errExtra(.negative_bitwidth, firstToken, .{ .signed = res.value.signed });
                break :bits;
            }

            const width = res.asU64();
            if (width == 0 and nameToken != 0) {
                try p.errToken(.zero_width_named_field, nameToken);
                break :bits;
            } else if (width > ty.bitSizeof(p.pp.compilation).?) {
                try p.errToken(.bitfield_too_big, nameToken);
                break :bits;
            }

            bits = @as(u32, @truncate(width));
            bitsNode = res.node;
        }
        if (nameToken == 0 and bitsNode == .none) unnamed: {
            if (ty.is(.Enum)) break :unnamed;
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
                break; // must be followed by a semicolon
            }
            try p.err(.missing_declaration);
        } else {
            try p.recordBuffer.append(.{
                .name = if (nameToken != 0) p.getTokenSlice(nameToken) else try p.getAnonymousName(firstToken),
                .ty = ty,
                .bitWidth = bits,
            });

            const node = try p.addNode(.{
                .tag = .RecordFieldDecl,
                .type = ty,
                .data = .{ .Declaration = .{ .name = nameToken, .node = bitsNode } },
            });
            try p.declBuffer.append(node);
        }
        if (p.eat(.Comma) == null) break;
    }
    _ = try p.expectToken(.Semicolon);
    return true;
}

// specQual : typeSpec | typeQual | alignSpec
fn specQual(p: *Parser) Error!?Type {
    var spec: TypeBuilder = .{};
    if (try p.parseTypeSpec(&spec)) {
        if (spec.alignment != 0)
            try p.errToken(.align_ignored, spec.alignToken.?);
        spec.alignToken = null;
        return try spec.finish(p);
    }

    return null;
}

/// enumSpec
///  : keyword_enum IDENTIFIER? { enumerator (',' enumerator)? ',') }
///  | keyword_enum IDENTIFIER
fn parseEnumSpec(p: *Parser) Error!*Type.Enum {
    const enumTK = p.index;
    p.index += 1;

    const attrBufferTop = p.attrBuffer.items.len;
    defer p.attrBuffer.items.len = attrBufferTop;

    try p.parseAttributeSpecifier(.record);

    const maybeID = try p.eatIdentifier();
    const lb = p.eat(.LBrace) orelse {
        const ident = maybeID orelse {
            try p.err(.ident_or_l_brace);
            return error.ParsingFailed;
        };

        // check if this is a reference to a previous type
        if (try p.findTag(.KeywordEnum, ident, .reference)) |prev| {
            return prev.type.data.@"enum";
        } else {
            const enumType = try Type.Enum.create(p.arena, p.getTokenSlice(ident));
            const ty = Type{ .specifier = .Enum, .data = .{ .@"enum" = enumType } };
            const sym = Scope.Symbol{ .name = enumType.name, .type = ty, .nameToken = ident };
            try p.scopes.append(.{ .@"enum" = sym });
            return enumType;
        }
    };

    // Get forward declared type or create a new one
    var defined = false;
    const enumType: *Type.Enum = if (maybeID) |ident| enumTy: {
        if (try p.findTag(.KeywordEnum, ident, .definition)) |prev| {
            if (!prev.type.data.@"enum".isIncomplete()) {
                // if the enum isn't incomplete, this is a redefinition
                try p.errStr(.redefinition, ident, p.getTokenSlice(ident));
                try p.errToken(.previous_definition, prev.nameToken);
            } else {
                defined = true;
                break :enumTy prev.type.data.@"enum";
            }
        }
        break :enumTy try Type.Enum.create(p.arena, p.getTokenSlice(ident));
    } else try Type.Enum.create(p.arena, try p.getAnonymousName(enumTK));

    const ty = Type{
        .specifier = .Enum,
        .data = .{ .@"enum" = enumType },
    };

    if (maybeID != null and !defined) {
        try p.scopes.append(.{ .@"enum" = .{
            .name = enumType.name,
            .type = ty,
            .nameToken = maybeID.?,
        } });
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
    enumType.tagType = .{ .specifier = .Int };

    if (p.enumBuffer.items.len == enumBufferTop)
        try p.err(.empty_enum);

    try p.expectClosing(lb, .RBrace);
    try p.parseAttributeSpecifier(.record);

    // finish by creating a node
    var node: AST.Node = .{
        .tag = .EnumDeclTwo,
        .type = ty,
        .data = .{ .BinaryExpr = .{ .lhs = .none, .rhs = .none } },
    };

    const fieldNodes = p.listBuffer.items[listBufferTop..];
    switch (fieldNodes.len) {
        0 => {},
        1 => node.data = .{ .BinaryExpr = .{ .lhs = fieldNodes[0], .rhs = .none } },
        2 => node.data = .{ .BinaryExpr = .{ .lhs = fieldNodes[0], .rhs = fieldNodes[1] } },
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
            .ty = .{ .specifier = if (p.pp.compilation.langOpts.shortEnums) .SChar else .Int },
            .value = .{ .signed = 0 },
        } };
    }

    /// Increment enumerator value adjusting type if needed.
    fn incr(e: *Enumerator, _: *Parser) !void {
        e.res.node = .none;
        switch (e.res.value) {
            .unavailable => unreachable,
            .signed => |*v| v.* += 1,
            .unsigned => |*v| v.* += 1,
        }
        // TODO adjust type if value does not fit current
    }

    /// Set enumerator value to specified value, adjusting type if needed.
    fn set(e: *Enumerator, _: *Parser, res: Result) !void {
        e.res = res;
        // TODO adjust res type to try to fit with the previous type
    }
};

const EnumFieldAndNode = struct { field: Type.Enum.Field, node: NodeIndex };
/// enumerator : IDENTIFIER ('=' constExpr)
fn enumerator(p: *Parser, e: *Enumerator) Error!?EnumFieldAndNode {
    _ = try p.pragma();
    const nameToken = try p.eatIdentifier() orelse {
        if (p.getCurrToken() == .RBrace) return null;
        try p.err(.expected_identifier);
        p.skipTo(.RBrace);
        return error.ParsingFailed;
    };

    const name = p.getTokenSlice(nameToken);
    const attrBufferTop = p.attrBuffer.items.len;
    defer p.attrBuffer.items.len = attrBufferTop;

    try p.parseAttributeSpecifier(.@"enum");

    if (p.eat(.Equal)) |_| {
        const specified = try p.constExpr();
        if (specified.value == .unavailable) {
            try p.errToken(.enum_val_unavailable, nameToken + 2);
            try e.incr(p);
        } else {
            try e.set(p, specified);
        }
    } else {
        try e.incr(p);
    }

    if (p.findSymbol(nameToken, .definition)) |scope| switch (scope) {
        .enumeration => |sym| {
            try p.errStr(.redefinition, nameToken, name);
            try p.errToken(.previous_definition, sym.nameToken);
        },

        .declaration, .definition, .param => |sym| {
            try p.errStr(.redefinition_different_sym, nameToken, name);
            try p.errToken(.previous_definition, sym.nameToken);
        },

        else => unreachable,
    };

    try p.scopes.append(.{ .enumeration = .{
        .name = name,
        .value = e.res,
        .nameToken = nameToken,
    } });

    return EnumFieldAndNode{
        .field = .{
            .name = name,
            .ty = e.res.ty,
            .value = e.res.asU64(),
        },
        .node = try p.addNode(
            .{
                .tag = .EnumFieldDecl,
                .type = e.res.ty,
                .data = .{
                    .Declaration = .{
                        .name = nameToken,
                        .node = e.res.node,
                    },
                },
            },
        ),
    };
}
/// atomicTypeSpec : keyword_atomic '(' typeName ')'
/// typeQual : keyword_const | keyword_restrict | keyword_volatile | keyword_atomic
fn parseTypeQual(p: *Parser, b: *Type.Qualifiers.Builder) Error!bool {
    var any = false;

    while (true) {
        switch (p.getCurrToken()) {
            .KeywordRestrict,
            .KeywordGccRestrict1,
            .KeywordGccRestrict2,
            => {
                if (b.restrict != null)
                    try p.errStr(.duplicate_declspec, p.index, "restrict")
                else
                    b.restrict = p.index;
            },

            .KeywordConst,
            .KeywordGccConst1,
            .KeywordGccConst2,
            => {
                if (b.@"const" != null)
                    try p.errStr(.duplicate_declspec, p.index, "const")
                else
                    b.@"const" = p.index;
            },

            .KeywordVolatile, .KeywordGccVolatile1, .KeywordGccVolatile2 => {
                if (b.@"volatile" != null)
                    try p.errStr(.duplicate_declspec, p.index, "volatile")
                else
                    b.@"volatile" = p.index;
            },

            .KeywordAtomic => {
                // _Atomic(typeName) instead of just _Atomic
                if (p.tokenIds[p.index + 1] == .LParen) break;
                if (b.atomic != null)
                    try p.errStr(.duplicate_declspec, p.index, "atomic")
                else
                    b.atomic = p.index;
            },

            else => break,
        }
        p.index += 1;
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

/// declarator: pointer? directDeclarator
/// abstractDeclarator
/// : pointer? ('(' abstractDeclarator ')')? directAbstractDeclarator*
fn declarator(p: *Parser, baseType: Type, kind: DeclaratorKind) Error!?Declarator {
    const start = p.index;
    var d = Declarator{ .name = 0, .type = try p.pointer(baseType) };

    const maybeIdent = p.index;
    if (kind != .abstract and (try p.eatIdentifier()) != null) {
        d.name = maybeIdent;
        d.type = try p.directDeclarator(d.type, &d, kind);
        return d;
    } else if (p.eat(.LParen)) |lp| blk: {
        var res = (try p.declarator(.{ .specifier = .Void }, kind)) orelse {
            p.index = lp;
            break :blk;
        };

        try p.expectClosing(lp, .RParen);
        const suffixStart = p.index;
        const outer = try p.directDeclarator(d.type, &d, kind);

        try res.type.combine(outer, p, res.funcDeclarator orelse suffixStart);
        res.oldTypeFunc = d.oldTypeFunc;
        return res;
    }

    if (kind == .normal and !baseType.isEnumOrRecord()) {
        try p.err(.expected_ident_or_l_paren);
    }

    d.type = try p.directDeclarator(d.type, &d, kind);
    if (start == p.index)
        return null;

    return d;
}

/// directDeclarator
///  : '[' typeQual* assignExpr? ']'  directDeclarator?
///  | '[' keyword_static typeQual* assignExpr ']' directDeclarator?
///  | '[' typeQual+ keyword_static assignExpr ']' directDeclarator?
///  | '[' typeQual* '*' ']' directDeclarator?
///  | '(' paramDecls ')' directDeclarator?
///  | '(' (IDENTIFIER (',' IDENTIFIER))? ')' directDeclarator?
/// directAbstractDeclarator
///  : '[' typeQual* assignExpr? ']'
///  | '[' keyword_static typeQual* assignExpr ']'
///  | '[' typeQual+ keyword_static assignExpr ']'
///  | '[' '*' ']'
///  | '(' paramDecls? ')'
fn directDeclarator(p: *Parser, baseType: Type, d: *Declarator, kind: DeclaratorKind) Error!Type {
    if (p.eat(.LBracket)) |lb| {
        var resType = Type{ .specifier = .Pointer };
        var qualsBuilder = Type.Qualifiers.Builder{};
        var gotQuals = try p.parseTypeQual(&qualsBuilder);
        var static = p.eat(.KeywordStatic);

        if (static != null and !gotQuals)
            gotQuals = try p.parseTypeQual(&qualsBuilder);

        var star = p.eat(.Asterisk);
        const size = if (star) |_| Result{} else try p.assignExpr();

        try p.expectClosing(lb, .RBracket);

        if (star != null and static != null) {
            try p.errToken(.invalid_static_star, static.?);
        }

        if (kind != .param) {
            if (static != null)
                try p.errToken(.star_non_param, lb)
            else if (gotQuals)
                try p.errToken(.array_qualifiers, lb);

            if (star) |some| try p.errToken(.star_non_param, some);

            static = null;
            qualsBuilder = .{};
            star = null;
        } else {
            try qualsBuilder.finish(p, &resType);
        }

        if (static) |_|
            try size.expect(p);

        const outer = try p.directDeclarator(baseType, d, kind);
        var maxBits = p.pp.compilation.target.ptrBitWidth();
        if (maxBits > 61) maxBits = 61;

        const maxBytes = (@as(u64, 1) << @as(u6, @truncate(maxBits))) - 1;
        const maxElems = maxBytes / @max(1, outer.sizeof(p.pp.compilation) orelse 1);

        switch (size.value) {
            .unavailable => if (size.node != .none) {
                if (p.returnType == null and kind != .param)
                    try p.errToken(.variable_len_array_file_scope, lb);

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
            },

            .unsigned => |v| {
                const arrayType = try p.arena.create(Type.Array);
                arrayType.elem = .{ .specifier = .Void };
                arrayType.len = v;
                if (arrayType.len > maxElems) {
                    try p.errToken(.array_too_large, lb);
                    arrayType.len = maxElems;
                }
                resType.data = .{ .array = arrayType };
                resType.specifier = .Array;
            },

            .signed => |v| {
                if (v < 0)
                    try p.errToken(.negative_array_size, lb);

                const arrayType = try p.arena.create(Type.Array);
                arrayType.elem = .{ .specifier = .Void };
                arrayType.len = @as(u64, @bitCast(v));
                if (arrayType.len > maxElems) {
                    try p.errToken(.array_too_large, lb);
                    arrayType.len = maxElems;
                }
                resType.data = .{ .array = arrayType };
                resType.specifier = .Array;
            },
        }

        try resType.combine(outer, p, lb);
        return resType;
    } else if (p.eat(.LParen)) |lp| {
        d.funcDeclarator = lp;
        if (p.getCurrToken() == .Ellipsis) {
            try p.err(.param_before_var_args);
            p.index += 1;
        }

        const funcType = try p.arena.create(Type.Function);
        funcType.params = &.{};
        funcType.returnType.specifier = .Void;
        var specifier: Type.Specifier = .Func;

        if (try p.paramDecls()) |params| {
            funcType.params = params;
            if (p.eat(.Ellipsis)) |_|
                specifier = .VarArgsFunc;
        } else if (p.getCurrToken() == .RParen) {
            specifier = .OldStyleFunc;
        } else if (p.getCurrToken() == .Identifier or p.getCurrToken() == .ExtendedIdentifier) {
            d.oldTypeFunc = p.index;

            const paramBufferTop = p.paramBuffer.items.len;
            const scopesTop = p.scopes.items.len;

            defer {
                p.paramBuffer.items.len = paramBufferTop;
                p.scopes.items.len = scopesTop;
            }

            // find Symbol stops search at the block
            try p.scopes.append(.block);

            specifier = .OldStyleFunc;
            while (true) {
                const nameToken = try p.expectIdentifier();
                if (p.findSymbol(nameToken, .definition)) |scope| {
                    try p.errStr(.redefinition_of_parameter, nameToken, p.getTokenSlice(nameToken));
                    try p.errToken(.previous_definition, scope.param.nameToken);
                }

                try p.scopes.append(.{ .param = .{
                    .name = p.getTokenSlice(nameToken),
                    .type = undefined,
                    .nameToken = nameToken,
                } });

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

/// pointer : '*' typeQual* pointer?
fn pointer(p: *Parser, baseType: Type) Error!Type {
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

/// paramDecls : paramDecl (',' paramDecl)* (',' '...')
/// paramDecl : declSpec (declarator | abstractDeclarator)
fn paramDecls(p: *Parser) Error!?[]Type.Function.Param {
    const paramBufferTop = p.paramBuffer.items.len;
    const scopesTop = p.scopes.items.len;

    defer {
        p.paramBuffer.items.len = paramBufferTop;
        p.scopes.items.len = scopesTop;
    }

    // findSymbol stops the search at .block
    try p.scopes.append(.block);

    while (true) {
        const paramDeclSpec = if (try p.declSpecifier(true)) |some|
            some
        else if (p.paramBuffer.items.len == paramBufferTop)
            return null
        else blk: {
            var spec: TypeBuilder = .{};
            break :blk DeclSpec{ .type = try spec.finish(p) };
        };

        var nameToken: TokenIndex = 0;
        const firstToken = p.index;
        var paramType = paramDeclSpec.type;
        if (try p.declarator(paramDeclSpec.type, .param)) |some| {
            if (some.oldTypeFunc) |tokenIdx|
                try p.errToken(.invalid_old_style_params, tokenIdx);

            nameToken = some.name;
            paramType = some.type;
            if (some.name != 0) {
                if (p.findSymbol(nameToken, .definition)) |scope| {
                    if (scope == .enumeration) {
                        try p.errStr(.redefinition_of_parameter, nameToken, p.getTokenSlice(nameToken));
                        try p.errToken(.previous_definition, scope.enumeration.nameToken);
                    } else {
                        try p.errStr(.redefinition_of_parameter, nameToken, p.getTokenSlice(nameToken));
                        try p.errToken(.previous_definition, scope.param.nameToken);
                    }
                }

                try p.scopes.append(.{ .param = .{
                    .name = p.getTokenSlice(nameToken),
                    .type = some.type,
                    .nameToken = nameToken,
                } });
            }
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

/// typeName : specQual+ abstractDeclarator
fn typeName(p: *Parser) Error!?Type {
    const ty = (try p.specQual()) orelse return null;
    if (try p.declarator(ty, .abstract)) |some| {
        if (some.oldTypeFunc) |tokenIdx|
            try p.errToken(.invalid_old_style_params, tokenIdx);

        return some.type;
    } else return ty;
}

/// initializer
///  : assignExpr
///  | '{' initializerItems '}'
pub fn initializer(p: *Parser, initType: Type) Error!Result {
    // fast path for non-braced initializers
    if (p.getCurrToken() != .LBrace) {
        const token = p.index;
        var res = try p.assignExpr();
        try res.expect(p);
        if (try p.coerceArrayInit(&res, token, initType))
            return res;

        try p.coerceInit(&res, token, initType);
        return res;
    }

    var il: InitList = .{};
    defer il.deinit(p.pp.compilation.gpa);

    _ = try p.initializerItem(&il, initType);

    const res = try p.convertInitList(il, initType);
    var resType = p.nodes.items(.type)[@intFromEnum(res)];
    resType.qual = initType.qual;
    return Result{ .ty = resType, .node = res };
}

/// initializerItems : designation? initializer (',' designation? initializer)* ','?
/// designation : designator+ '='
/// designator
///  : '[' constExpr ']'
///  | '.' identifier
pub fn initializerItem(p: *Parser, il: *InitList, initType: Type) Error!bool {
    const lb = p.eat(.LBrace) orelse {
        const token = p.index;
        var res = try p.assignExpr();
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
    while (true) : (count += 1) {
        errdefer p.skipTo(.RBrace);
        const firstToken = p.index;
        var curType = initType;
        var curIL = il;
        var designation = false;
        while (true) {
            if (p.eat(.LBracket)) |lbr| {
                if (!curType.isArray()) {
                    try p.errStr(.invalid_array_designator, lbr, try p.typeStr(curType));
                    return error.ParsingFailed;
                }

                const indexRes = try p.constExpr();
                try p.expectClosing(lbr, .RBracket);
                const indexUnchecked = switch (indexRes.value) {
                    .unsigned => |val| val,
                    .signed => |val| if (val < 0) {
                        try p.errExtra(.negative_array_designator, lbr + 1, .{ .signed = val });
                        return error.ParsingFailed;
                    } else @as(u64, @intCast(val)),

                    .unavailable => unreachable,
                };

                const maxLen = curType.arrayLen() orelse std.math.maxInt(usize);
                if (indexUnchecked >= maxLen) {
                    try p.errExtra(.oob_array_designator, lbr + 1, .{ .unsigned = indexUnchecked });
                    return error.ParsingFailed;
                }

                const checked = @as(usize, @intCast(indexUnchecked));
                curIL = try curIL.find(p.pp.compilation.gpa, checked);
                curType = curType.getElemType();
                designation = true;
            } else if (p.eat(.Period)) |period| {
                const identifier = try p.expectIdentifier();
                if (!curType.isRecord()) {
                    try p.errStr(.invalid_field_designator, period, try p.typeStr(curType));
                    return error.ParsingFailed;
                }
                const field = curType.getField(p.getTokenSlice(identifier)) orelse
                    {
                    try p.errStr(.no_such_field_designator, period, p.getTokenSlice(identifier));
                    return error.ParsingFailed;
                };
                curIL = try curIL.find(p.pp.compilation.gpa, field.i);
                curType = field.f.ty;
                designation = true;
            } else break;
        }

        if (designation)
            _ = try p.expectToken(.Equal);

        var saw = false;

        if (isStrInit and p.isStringInit(initType)) {
            var tempIL = InitList{};
            defer tempIL.deinit(p.pp.compilation.gpa);
            saw = try p.initializerItem(&tempIL, .{ .specifier = .Void });
        } else if (count == 0 and p.isStringInit(initType)) {
            saw = try p.initializerItem(il, initType);
        } else if (isScalar and count != 0) {
            // discard further scalars
            var tempIL = InitList{};
            defer tempIL.deinit(p.pp.compilation.gpa);
            saw = try p.initializerItem(&tempIL, .{ .specifier = .Void });
        } else if (p.getCurrToken() == .LBrace) {
            if (curType.isArray()) {
                curIL = try curIL.find(p.pp.compilation.gpa, count);
                curType = curType.getElemType();
                saw = try p.initializerItem(curIL, curType);
            } else {
                // TODO: warning scalar braces
                saw = try p.initializerItem(curIL, curType);
            }
        } else if (try p.findScalarInitializer(&curIL, &curType)) {
            saw = try p.initializerItem(curIL, curType);
        } else if (designation) {
            // designation overrides previous value, let existing mechanism handle it
            saw = try p.initializerItem(curIL, curType);
        } else {
            // discard further values
            var tempIL = InitList{};
            defer tempIL.deinit(p.pp.compilation.gpa);
            saw = try p.initializerItem(&tempIL, .{ .specifier = .Void });
            if (!warnedExcess)
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

/// Returns true if the value is unused.
fn findScalarInitializer(p: *Parser, il: **InitList, ty: *Type) Error!bool {
    if (ty.isArray()) {
        var index = il.*.list.items.len;
        if (index != 0) index = il.*.list.items[index - 1].index;

        const arrayType = ty.*;
        const maxElems = arrayType.arrayLen() orelse std.math.maxInt(usize);
        if (maxElems == 0) {
            if (p.getCurrToken() != .LBrace) {
                try p.err(.empty_aggregate_init_braces);
                return error.ParsingFailed;
            }
            return false;
        }
        const elemType = arrayType.getElemType();
        const arrayIL = il.*;
        while (index < maxElems) : (index += 1) {
            ty.* = elemType;
            il.* = try arrayIL.find(p.pp.compilation.gpa, index);
            if (try p.findScalarInitializer(il, ty))
                return true;
        }
        return false;
    } else if (ty.get(.Struct)) |structType| {
        var index = il.*.list.items.len;
        if (index != 0) index = il.*.list.items[index - 1].index + 1;

        const max_elems = structType.data.record.fields.len;
        if (max_elems == 0) {
            if (p.getCurrToken() == .LBrace) {
                try p.err(.empty_aggregate_init_braces);
                return error.ParsingFailed;
            }
            return false;
        }
        const structIL = il.*;
        while (index < max_elems) : (index += 1) {
            const field = structType.data.record.fields[index];
            ty.* = field.ty;
            il.* = try structIL.find(p.pp.compilation.gpa, index);
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
        il.* = try il.*.find(p.pp.compilation.gpa, 0);
        if (try p.findScalarInitializer(il, ty))
            return true;
        return false;
    }
    return il.*.node == .none;
}

fn coerceArrayInit(p: *Parser, item: *Result, token: TokenIndex, target: Type) !bool {
    if (!target.isArray())
        return false;

    if (!item.ty.isArray()) {
        const eMsg = " from incompatible type ";
        try p.errStr(.incompatible_init, token, try p.typePairStrExtra(target, eMsg, item.ty));
        return true; // do not do further coercion
    }

    if (!target.getElemType().eql(item.ty.getElemType(), false)) {
        const eMsg = " with array of type ";
        try p.errStr(.incompatible_array_init, token, try p.typePairStrExtra(target, eMsg, item.ty));
        return true; // do not do further coercion
    }

    if (target.get(.Array)) |arrayType| {
        std.debug.assert(item.ty.is(.Array));
        const len = item.ty.arrayLen().?;
        const arrayLen = arrayType.arrayLen().?;
        if (p.nodeIs(item.node, .StringLiteralExpr)) {
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
            try item.boolCast(p, unqualType);
        } else {
            try p.errStr(.incompatible_init, token, try p.typePairStrExtra(target, eMsg, item.ty));
        }
    } else if (unqualType.isInt()) {
        if (item.ty.isInt() or item.ty.isFloat()) {
            try item.intCast(p, unqualType);
        } else if (item.ty.isPointer()) {
            try p.errStr(.implicit_ptr_to_int, token, try p.typePairStrExtra(item.ty, " to ", target));
            try item.intCast(p, unqualType);
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
        if (item.isZero()) {
            try item.nullCast(p, target);
        } else if (item.ty.isInt()) {
            try p.errStr(.implicit_int_to_ptr, token, try p.typePairStrExtra(item.ty, " to ", target));
            try item.ptrCast(p, unqualType);
        } else if (item.ty.isPointer()) {
            if (!item.ty.isVoidStar() and !unqualType.isVoidStar() and !unqualType.eql(item.ty, false)) {
                try p.errStr(.incompatible_ptr_init, token, try p.typePairStrExtra(target, eMsg, item.ty));
                try item.ptrCast(p, unqualType);
            } else if (!unqualType.eql(item.ty, true)) {
                if (!unqualType.getElemType().qual.hasQuals(item.ty.getElemType().qual))
                    try p.errStr(.ptr_init_discards_quals, token, try p.typePairStrExtra(target, eMsg, item.ty));

                try item.ptrCast(p, unqualType);
            }
        } else {
            try p.errStr(.incompatible_init, token, try p.typePairStrExtra(target, eMsg, item.ty));
        }
    } else if (unqualType.isRecord()) {
        if (!unqualType.eql(item.ty, false))
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

    var i = p.index;
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
        // array element type invalid, reported earlier
        if (initType.getElemType().hasIncompleteSize())
            return error.ParsingFailed;

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
                    .data = .{ .Int = init.index - start },
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
            .data = .{ .BinaryExpr = .{ .lhs = .none, .rhs = .none } },
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
                .alignment = initType.alignment,
            };
        } else if (start < maxItems) {
            const elem = try p.addNode(.{
                .tag = .ArrayFillerExpr,
                .type = elemType,
                .data = .{ .Int = maxItems - start },
            });
            try p.listBuffer.append(elem);
        }

        const items = p.listBuffer.items[listBuffTop..];
        switch (items.len) {
            0 => {},
            1 => arrInitNode.data.BinaryExpr.lhs = items[0],
            2 => arrInitNode.data.BinaryExpr = .{ .lhs = items[0], .rhs = items[1] },
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
            .data = .{ .BinaryExpr = .{ .lhs = .none, .rhs = .none } },
        };
        const items = p.listBuffer.items[listBuffTop..];
        switch (items.len) {
            0 => {},
            1 => structInitNode.data.BinaryExpr.lhs = items[0],
            2 => structInitNode.data.BinaryExpr = .{ .lhs = items[0], .rhs = items[1] },
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
            .data = .{ .UnionInit = .{ .fieldIndex = 0, .node = .none } },
        };
        if (unionType.data.record.fields.len == 0) {
            // do nothing for empty unions
        } else if (il.list.items.len == 0) {
            unionInitNode.data.UnionInit.node = try p.addNode(.{
                .tag = .DefaultInitExpr,
                .type = initType,
                .data = undefined,
            });
        } else {
            const init = il.list.items[0];
            const fieldType = unionType.data.record.fields[init.index].ty;
            unionInitNode.data.UnionInit = .{
                .fieldIndex = @as(u32, @truncate(init.index)),
                .node = try p.convertInitList(init.list, fieldType),
            };
        }
        return try p.addNode(unionInitNode);
    } else if (initType.isFunc()) {
        return error.ParsingFailed; // invalid func initializer, reported earlier
    } else if (initType.is(.Void)) {
        try p.errStr(.variable_incomplete_ty, il.tok, try p.typeStr(initType));
        return error.ParsingFailed;
    } else {
        unreachable;
    }
}

/// assembly : keyword_asm asmQual* '(' asmStr ')'
fn assembly(p: *Parser, kind: enum { global, decl, stmt }) Error!?NodeIndex {
    switch (p.getCurrToken()) {
        .KeywordGccAsm, .KeywordGccAsm1, .KeywordGccAsm2 => p.index += 1,
        else => return null,
    }

    var @"volatile" = false;
    var @"inline" = false;
    var goto = false;
    while (true) : (p.index += 1) switch (p.getCurrToken()) {
        .KeywordVolatile, .KeywordGccVolatile1, .KeywordGccVolatile2 => {
            if (kind != .stmt) try p.errStr(.meaningless_asm_qual, p.index, "volatile");
            if (@"volatile") try p.errStr(.duplicate_asm_qual, p.index, "volatile");
            @"volatile" = true;
        },
        .KeywordInline, .KeywordGccInline1, .KeywordGccInline2 => {
            if (kind != .stmt) try p.errStr(.meaningless_asm_qual, p.index, "inline");
            if (@"inline") try p.errStr(.duplicate_asm_qual, p.index, "inline");
            @"inline" = true;
        },
        .KeywordGoto => {
            if (kind != .stmt) try p.errStr(.meaningless_asm_qual, p.index, "goto");
            if (goto) try p.errStr(.duplicate_asm_qual, p.index, "goto");
            goto = true;
        },
        else => break,
    };

    const lparen = try p.expectToken(.LParen);
    if (kind != .stmt) {
        _ = try p.asmString();
    } else {
        return p.todo("assembly statements");
    }
    try p.expectClosing(lparen, .RParen);

    if (kind != .decl)
        _ = try p.expectToken(.Semicolon);
    return .none;
}

/// Same as stringLiteral but errors on unicode and wide string literals
fn asmString(p: *Parser) Error!NodeIndex {
    var i = p.index;
    while (true) : (i += 1) switch (p.tokenIds[i]) {
        .StringLiteral => {},
        .StringLiteralUTF_8, .StringLiteralUTF_16, .StringLiteralUTF_32 => {
            try p.errStr(.invalid_asm_str, p.index, "unicode");
            return error.ParsingFailed;
        },
        .StringLiteralWide => {
            try p.errStr(.invalid_asm_str, p.index, "wide");
            return error.ParsingFailed;
        },
        else => break,
    };
    return (try p.parseStringLiteral()).node;
}

// ====== statements ======

/// stmt
///  : labeledStmt
///  | compoundStmt
///  | keyword_if '(' expr ')' stmt (keyword_else stmt)?
///  | keyword_switch '(' expr ')' stmt
///  | keyword_while '(' expr ')' stmt
///  | keyword_do stmt while '(' expr ')' ';'
///  | keyword_for '(' (decl | expr? ';') expr? ';' expr? ')' stmt
///  | keyword_goto ( IDENTIFIER | ( '*' expr)) ';'
///  | keyword_continue ';'
///  | keyword_break ';'
///  | keyword_return expr? ';'
///  | expr? ';'
fn stmt(p: *Parser) Error!NodeIndex {
    if (try p.labeledStmt()) |some|
        return some;

    if (try p.parseCompoundStmt(false)) |some|
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

    if (p.eat(.KeywordGoto)) |gotoToken| {
        if (p.eat(.Asterisk)) |_| {
            const expr = p.index;
            var e = try p.parseExpr();
            try e.expect(p);
            try e.lvalConversion(p);
            p.computedGotoTok = gotoToken;
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

                if (e.isZero()) {
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
            .data = .{ .DeclarationRef = nameToken },
        });
    }

    if (p.eat(.KeywordContinue)) |cont| {
        if (!p.inLoop())
            try p.errToken(.continue_not_in_loop, cont);
        _ = try p.expectToken(.Semicolon);

        return try p.addNode(.{ .tag = .ContinueStmt, .data = undefined });
    }

    if (p.eat(.KeywordBreak)) |br| {
        if (!p.inLoopOrSwitch())
            try p.errToken(.break_not_in_loop_or_switch, br);

        _ = try p.expectToken(.Semicolon);
        return try p.addNode(.{ .tag = .BreakStmt, .data = undefined });
    }

    if (try p.parseReturnStmt()) |some|
        return some;

    const exprStart = p.index;
    const errStart = p.pp.compilation.diag.list.items.len;

    const e = try p.parseExpr();
    if (e.node != .none) {
        _ = try p.expectToken(.Semicolon);
        try e.maybeWarnUnused(p, exprStart, errStart);
        return e.node;
    }

    const attrBufferTop = p.attrBuffer.items.len;
    defer p.attrBuffer.items.len = attrBufferTop;
    try p.parseAttributeSpecifier(.statement);
    const attrs = p.attrBuffer.items[attrBufferTop..];

    if (p.eat(.Semicolon)) |_| {
        var nullNode: AST.Node = .{ .tag = .NullStmt, .data = undefined };
        if (attrs.len > 0) {
            // TODO: this condition is not completely correct; the last statement of a compound
            // statement is also valid if it precedes a switch label (so intervening '}' are ok,
            // but only if they close a compound statement)
            if (p.getCurrToken() != .KeywordCase and p.getCurrToken() != .KeywordDefault)
                try p.errToken(.invalid_fallthrough, exprStart);

            nullNode.type = try nullNode.type.withAttributes(p.arena, attrs);
        }

        return p.addNode(nullNode);
    }

    try p.err(.expected_stmt);
    return error.ParsingFailed;
}

fn parseIfStmt(p: *Parser) Error!NodeIndex {
    const startScopeLen = p.scopes.items.len;
    defer p.scopes.items.len = startScopeLen;

    const lp = try p.expectToken(.LParen);
    var cond = try p.parseExpr();

    try cond.expect(p);
    try cond.lvalConversion(p);
    if (cond.ty.isInt())
        try cond.intCast(p, cond.ty.integerPromotion(p.pp.compilation))
    else if (!cond.ty.isFloat() and !cond.ty.isPointer())
        try p.errStr(.statement_scalar, lp + 1, try p.typeStr(cond.ty));

    try cond.saveValue(p);
    try p.expectClosing(lp, .RParen);

    const then = try p.stmt();
    const elseTK = if (p.eat(.KeywordElse)) |_| try p.stmt() else .none;

    if (then != .none and elseTK != .none) {
        return try p.addNode(.{ .tag = .IfThenElseStmt, .data = .{
            .If3 = .{
                .cond = cond.node,
                .body = (try p.addList(&.{ then, elseTK })).start,
            },
        } });
    } else if (then == .none and elseTK != .none) {
        return try p.addNode(.{
            .tag = .IfElseStmt,
            .data = .{ .BinaryExpr = .{ .lhs = cond.node, .rhs = elseTK } },
        });
    } else return try p.addNode(.{
        .tag = .IfThenStmt,
        .data = .{ .BinaryExpr = .{ .lhs = cond.node, .rhs = then } },
    });
}

fn parseForStmt(p: *Parser) Error!NodeIndex {
    const startScopeLen = p.scopes.items.len;
    defer p.scopes.items.len = startScopeLen;

    const declBufferTop = p.declBuffer.items.len;
    defer p.declBuffer.items.len = declBufferTop;

    const lp = try p.expectToken(.LParen);
    const gotDecl = try p.parseDeclaration();

    // for-init
    const initStart = p.index;
    var errStart = p.pp.compilation.diag.list.items.len;
    var init = if (!gotDecl) try p.parseExpr() else Result{};
    try init.saveValue(p);
    try init.maybeWarnUnused(p, initStart, errStart);

    if (!gotDecl)
        _ = try p.expectToken(.Semicolon);

    // cond
    var cond = try p.parseExpr();
    if (cond.node != .none) {
        try cond.lvalConversion(p);
        if (cond.ty.isInt())
            try cond.intCast(p, cond.ty.integerPromotion(p.pp.compilation))
        else if (!cond.ty.isFloat() and !cond.ty.isPointer())
            try p.errStr(.statement_scalar, lp + 1, try p.typeStr(cond.ty));
    }
    try cond.saveValue(p);
    _ = try p.expectToken(.Semicolon);

    // increment
    const incrStart = p.index;
    errStart = p.pp.compilation.diag.list.items.len;
    var incr = try p.parseExpr();
    try incr.maybeWarnUnused(p, incrStart, errStart);
    try incr.saveValue(p);
    try p.expectClosing(lp, .RParen);

    try p.scopes.append(.loop);
    const body = try p.stmt();

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
            .data = .{ .UnaryExpr = body },
        });
    } else {
        return try p.addNode(.{
            .tag = .ForStmt,
            .data = .{
                .If3 = .{
                    .cond = body,
                    .body = (try p.addList(&.{ init.node, cond.node, incr.node })).start,
                },
            },
        });
    }
}

fn parseWhileStmt(p: *Parser) Error!NodeIndex {
    const startScopeLen = p.scopes.items.len;
    defer p.scopes.items.len = startScopeLen;

    const lp = try p.expectToken(.LParen);
    var cond = try p.parseExpr();

    try cond.expect(p);
    try cond.lvalConversion(p);
    if (cond.ty.isInt())
        try cond.intCast(p, cond.ty.integerPromotion(p.pp.compilation))
    else if (!cond.ty.isFloat() and !cond.ty.isPointer())
        try p.errStr(.statement_scalar, lp + 1, try p.typeStr(cond.ty));

    try cond.saveValue(p);
    try p.expectClosing(lp, .RParen);

    try p.scopes.append(.loop);
    const body = try p.stmt();

    return try p.addNode(.{
        .tag = .WhileStmt,
        .data = .{ .BinaryExpr = .{ .rhs = cond.node, .lhs = body } },
    });
}

fn parseDoWhileStmt(p: *Parser) Error!NodeIndex {
    const startScopeLen = p.scopes.items.len;
    defer p.scopes.items.len = startScopeLen;

    try p.scopes.append(.loop);
    const body = try p.stmt();
    p.scopes.items.len = startScopeLen;

    _ = try p.expectToken(.KeywordWhile);
    const lp = try p.expectToken(.LParen);
    var cond = try p.parseExpr();

    try cond.expect(p);
    try cond.lvalConversion(p);
    if (cond.ty.isInt())
        try cond.intCast(p, cond.ty.integerPromotion(p.pp.compilation))
    else if (!cond.ty.isFloat() and !cond.ty.isPointer())
        try p.errStr(.statement_scalar, lp + 1, try p.typeStr(cond.ty));

    try cond.saveValue(p);
    try p.expectClosing(lp, .RParen);

    _ = try p.expectToken(.Semicolon);
    return try p.addNode(.{
        .tag = .WhileStmt,
        .data = .{ .BinaryExpr = .{ .rhs = cond.node, .lhs = body } },
    });
}

fn parseSwitchStmt(p: *Parser) Error!NodeIndex {
    const startScopeLen = p.scopes.items.len;
    defer p.scopes.items.len = startScopeLen;

    const lp = try p.expectToken(.LParen);
    var cond = try p.parseExpr();

    try cond.expect(p);
    try cond.lvalConversion(p);
    if (cond.ty.isInt())
        try cond.intCast(p, cond.ty.integerPromotion(p.pp.compilation))
    else
        try p.errStr(.statement_int, lp + 1, try p.typeStr(cond.ty));

    try cond.saveValue(p);
    try p.expectClosing(lp, .RParen);

    var switchScope = Scope.Switch{
        .cases = Scope.Switch.CaseMap.init(p.pp.compilation.gpa),
    };
    defer switchScope.cases.deinit();

    try p.scopes.append(.{ .@"switch" = &switchScope });
    const body = try p.stmt();

    return try p.addNode(.{
        .tag = .SwitchStmt,
        .data = .{ .BinaryExpr = .{ .lhs = cond.node, .rhs = body } },
    });
}

fn parseCaseStmt(p: *Parser, caseToken: u32) Error!?NodeIndex {
    const val = try p.constExpr();
    _ = try p.expectToken(.Colon);

    const s = try p.stmt();
    const node = try p.addNode(.{
        .tag = .CaseStmt,
        .data = .{ .BinaryExpr = .{
            .lhs = val.node,
            .rhs = s,
        } },
    });

    if (p.findSwitch()) |some| {
        if (val.value == .unavailable) {
            try p.errToken(.case_val_unavailable, caseToken + 1);
            return node;
        }

        const gop = try some.cases.getOrPut(val);
        if (gop.found_existing) {
            switch (val.value) {
                .unsigned => |v| try p.errExtra(.duplicate_switch_case_unsigned, caseToken, .{ .unsigned = v }),
                .signed => |v| try p.errExtra(.duplicate_switch_case_signed, caseToken, .{ .signed = v }),
                else => unreachable,
            }
            try p.errToken(.previous_case, gop.value_ptr.token);
        } else {
            gop.value_ptr.* = .{
                .token = caseToken,
                .node = node,
            };
        }
    } else {
        try p.errStr(.case_not_in_switch, caseToken, "case");
    }
    return node;
}

fn parseDefaultStmt(p: *Parser, defaultToken: u32) Error!?NodeIndex {
    _ = try p.expectToken(.Colon);
    const s = try p.stmt();

    const node = try p.addNode(.{
        .tag = .DefaultStmt,
        .data = .{ .UnaryExpr = s },
    });

    if (p.findSwitch()) |some| {
        if (some.default) |previous| {
            try p.errToken(.multiple_default, defaultToken);
            try p.errToken(.previous_case, previous.token);
        } else {
            some.default = .{
                .token = defaultToken,
                .node = node,
            };
        }
    } else {
        try p.errStr(.case_not_in_switch, defaultToken, "default");
    }

    return node;
}

/// labeledStmt
/// : IDENTIFIER ':' stmt
/// | keyword_case constExpr ':' stmt
/// | keyword_default ':' stmt
fn labeledStmt(p: *Parser) Error!?NodeIndex {
    if ((p.getCurrToken() == .Identifier or p.getCurrToken() == .ExtendedIdentifier) and p.lookAhead(1) == .Colon) {
        const nameToken = p.expectIdentifier() catch unreachable;
        const str = p.getTokenSlice(nameToken);
        if (p.findLabel(str)) |some| {
            try p.errStr(.duplicate_label, nameToken, str);
            try p.errStr(.previous_label, some, str);
        } else {
            p.labelCount += 1;
            try p.labels.append(.{ .label = nameToken });

            var i: usize = 0;
            while (i < p.labels.items.len) : (i += 1) {
                if (p.labels.items[i] == .unresolvedGoto and std.mem.eql(u8, p.getTokenSlice(p.labels.items[i].unresolvedGoto), str))
                    _ = p.labels.swapRemove(i);
            }
        }

        p.index += 1;

        const attrBufferTop = p.attrBuffer.items.len;
        defer p.attrBuffer.items.len = attrBufferTop;

        try p.parseAttributeSpecifier(.label);

        return try p.addNode(.{
            .tag = .LabeledStmt,
            .data = .{ .Declaration = .{ .name = nameToken, .node = try p.stmt() } },
        });
    } else if (p.eat(.KeywordCase)) |case| {
        return p.parseCaseStmt(case);
    } else if (p.eat(.KeywordDefault)) |default| {
        return p.parseDefaultStmt(default);
    } else return null;
}

/// compoundStmt : '{' ( decl | KeywordGccExtensionDecl |staticAssert |stmt)* '}'
fn parseCompoundStmt(p: *Parser, isFnBody: bool) Error!?NodeIndex {
    const lBrace = p.eat(.LBrace) orelse return null;

    const declBufferTop = p.declBuffer.items.len;
    const scopeTop = p.scopes.items.len;

    defer {
        p.declBuffer.items.len = declBufferTop;
        p.scopes.items.len = scopeTop;
    }

    // the parameters of a function are in the same scope as the body
    if (!isFnBody)
        try p.scopes.append(.block);

    var noreturnIdx: ?TokenIndex = null;
    var noreturnLabelCount: u32 = 0;

    while (p.eat(.RBrace) == null) : (_ = try p.pragma()) {
        if (p.parseStaticAssert() catch |er| switch (er) {
            error.ParsingFailed => {
                try p.nextStmt(lBrace);
                continue;
            },
            else => |e| return e,
        }) continue;

        if (p.parseDeclaration() catch |er| switch (er) {
            error.ParsingFailed => {
                try p.nextStmt(lBrace);
                continue;
            },
            else => |e| return e,
        }) continue;

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
            p.index = ext;
        }

        const s = p.stmt() catch |er| switch (er) {
            error.ParsingFailed => {
                try p.nextStmt(lBrace);
                continue;
            },
            else => |e| return e,
        };

        if (s == .none) continue;
        try p.declBuffer.append(s);

        if (noreturnIdx == null and p.nodeIsNoreturn(s)) {
            noreturnIdx = p.index;
            noreturnLabelCount = p.labelCount;
        }

        switch (p.nodes.items(.tag)[@intFromEnum(s)]) {
            .CaseStmt, .DefaultStmt, .LabeledStmt => noreturnIdx = null,
            else => {},
        }
    }

    if (noreturnIdx) |some| {
        if (noreturnLabelCount == p.labelCount and some != p.index - 1)
            try p.errToken(.unreachable_code, some);
    }

    if (isFnBody and (p.declBuffer.items.len == declBufferTop or !p.nodeIsNoreturn(p.declBuffer.items[p.declBuffer.items.len - 1]))) {
        if (!p.returnType.?.is(.Void))
            try p.errStr(.func_does_not_return, p.index - 1, p.getTokenSlice(p.funcName));

        try p.declBuffer.append(try p.addNode(.{
            .tag = .ImplicitReturn,
            .type = p.returnType.?,
            .data = undefined,
        }));
    }

    var node: AST.Node = .{
        .tag = .CompoundStmtTwo,
        .data = .{ .BinaryExpr = .{ .lhs = .none, .rhs = .none } },
    };
    const statements = p.declBuffer.items[declBufferTop..];
    switch (statements.len) {
        0 => {},
        1 => node.data = .{ .BinaryExpr = .{ .lhs = statements[0], .rhs = .none } },
        2 => node.data = .{ .BinaryExpr = .{ .lhs = statements[0], .rhs = statements[1] } },
        else => {
            node.tag = .CompoundStmt;
            node.data = .{ .range = try p.addList(statements) };
        },
    }

    return try p.addNode(node);
}

fn parseReturnStmt(p: *Parser) Error!?NodeIndex {
    const retToken = p.eat(.KeywordReturn) orelse return null;

    const eToken = p.index;
    var expr = try p.parseExpr();
    _ = try p.expectToken(.Semicolon);
    const returnType = p.returnType.?;

    if (expr.node == .none) {
        if (!returnType.is(.Void))
            try p.errStr(.func_should_return, retToken, p.getTokenSlice(p.funcName));
        return try p.addNode(.{ .tag = .ReturnStmt, .data = .{ .UnaryExpr = expr.node } });
    } else if (returnType.is((.Void))) {
        try p.errStr(.void_func_returns_value, eToken, p.getTokenSlice(p.funcName));
        return try p.addNode(.{ .tag = .ReturnStmt, .data = .{ .UnaryExpr = expr.node } });
    }

    try expr.lvalConversion(p);
    // Return type conversion is done as if it was assignment
    if (returnType.is(.Bool)) {
        // this is ridiculous but it's what clang does
        if (expr.ty.isInt() or expr.ty.isFloat() or expr.ty.isPointer()) {
            try expr.boolCast(p, returnType);
        } else {
            try p.errStr(.incompatible_return, eToken, try p.typeStr(expr.ty));
        }
    } else if (returnType.isInt()) {
        if (expr.ty.isInt() or expr.ty.isFloat()) {
            try expr.intCast(p, returnType);
        } else if (expr.ty.isPointer()) {
            try p.errStr(.implicit_ptr_to_int, eToken, try p.typePairStrExtra(expr.ty, " to ", returnType));
            try expr.intCast(p, returnType);
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
        if (expr.isZero()) {
            try expr.nullCast(p, returnType);
        } else if (expr.ty.isInt()) {
            try p.errStr(.implicit_int_to_ptr, eToken, try p.typePairStrExtra(expr.ty, " to ", returnType));
            try expr.intCast(p, returnType);
        } else if (!expr.ty.isVoidStar() and !returnType.isVoidStar() and !returnType.eql(expr.ty, false)) {
            try p.errStr(.incompatible_return, eToken, try p.typeStr(expr.ty));
        }
    } else if (returnType.isRecord()) { // enum.isInt() == true
        if (!returnType.eql(expr.ty, false)) {
            try p.errStr(.incompatible_return, eToken, try p.typeStr(expr.ty));
        }
    } else {
        unreachable;
    }

    try expr.saveValue(p);
    return try p.addNode(.{ .tag = .ReturnStmt, .data = .{ .UnaryExpr = expr.node } });
}

fn nodeIsNoreturn(p: *Parser, node: NodeIndex) bool {
    switch (p.nodes.items(.tag)[@intFromEnum(node)]) {
        .BreakStmt, .ContinueStmt, .ReturnStmt => return true,
        .IfThenElseStmt => {
            const data = p.data.items[p.nodes.items(.data)[@intFromEnum(node)].If3.body..];
            return p.nodeIsNoreturn(data[0]) and p.nodeIsNoreturn(data[1]);
        },

        .CompoundStmtTwo => {
            const data = p.nodes.items(.data)[@intFromEnum(node)];
            if (data.BinaryExpr.rhs != .none) return p.nodeIsNoreturn(data.BinaryExpr.rhs);
            if (data.BinaryExpr.lhs != .none) return p.nodeIsNoreturn(data.BinaryExpr.lhs);
            return false;
        },

        .CompoundStmt => {
            const data = p.nodes.items(.data)[@intFromEnum(node)];
            return p.nodeIsNoreturn(p.data.items[data.range.end - 1]);
        },

        .LabeledStmt => {
            const data = p.nodes.items(.data)[@intFromEnum(node)];
            return p.nodeIsNoreturn(data.Declaration.node);
        },

        else => return false,
    }
}

fn nextStmt(p: *Parser, lBrace: TokenIndex) !void {
    var parens: u32 = 0;
    while (p.index < p.tokenIds.len) : (p.index += 1) {
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
    p.index -= 1;
    try p.expectClosing(lBrace, .RBrace);
    unreachable;
}

//////////////////////
//       Expr       //
/////////////////////

pub fn macroExpr(p: *Parser) Compilation.Error!bool {
    const res = p.conditionalExpr() catch |e| switch (e) {
        error.OutOfMemory => return error.OutOfMemory,
        error.FatalError => return error.FatalError,
        error.ParsingFailed => return false,
    };
    if (res.value == .unavailable) {
        try p.errToken(.expected_expr, p.index);
        return false;
    }
    const value: bool = res.getBool();
    return value;
}

/// expr : assignExpr (',' assignExpr)*
fn parseExpr(p: *Parser) Error!Result {
    var exprStartIdx = p.index;
    var errStart = p.pp.compilation.diag.list.items.len;
    var lhs = try p.assignExpr();

    if (p.getCurrToken() == .Comma)
        try lhs.expect(p);

    while (p.eat(.Comma)) |_| {
        try lhs.maybeWarnUnused(p, exprStartIdx, errStart);
        exprStartIdx = p.index;
        errStart = p.pp.compilation.diag.list.items.len;

        const rhs = try p.assignExpr();
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

/// assignExpr
///  : conditionalExpr
///  | unaryExpr ('=' | '*=' | '/=' | '%=' | '+=' | '-=' | '<<=' | '>>=' | '&=' | '^=' | '|=') assignExpr
fn assignExpr(p: *Parser) Error!Result {
    var lhs = try p.conditionalExpr();
    if (lhs.empty(p))
        return lhs;

    const token = p.index;
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
    var rhs = try p.assignExpr();
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
            if (rhs.isZero()) {
                switch (tag) {
                    .DivAssignExpr => try p.errStr(.division_by_zero, div.?, "division"),
                    .ModAssignExpr => try p.errStr(.division_by_zero, mod.?, "remainder"),
                    else => {},
                }
            }
            _ = try lhsCopy.adjustTypes(token, &rhs, p, .arithmetic);
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
            try rhs.boolCast(p, lhs.ty);
        } else {
            try p.errStr(.incompatible_assign, token, try p.typePairStrExtra(lhs.ty, eMsg, rhs.ty));
        }
    } else if (unqualType.isInt()) {
        if (rhs.ty.isInt() or rhs.ty.isFloat()) {
            try rhs.intCast(p, unqualType);
        } else if (rhs.ty.isPointer()) {
            try p.errStr(.implicit_ptr_to_int, token, try p.typePairStrExtra(rhs.ty, " to ", lhs.ty));
            try rhs.intCast(p, unqualType);
        } else {
            try p.errStr(.incompatible_assign, token, try p.typePairStrExtra(lhs.ty, eMsg, rhs.ty));
        }
    } else if (unqualType.isFloat()) {
        if (rhs.ty.isInt() or rhs.ty.isFloat())
            try rhs.floatCast(p, unqualType)
        else
            try p.errStr(.incompatible_assign, token, try p.typePairStrExtra(lhs.ty, eMsg, rhs.ty));
    } else if (lhs.ty.isPointer()) {
        if (rhs.isZero()) {
            try rhs.nullCast(p, lhs.ty);
        } else if (rhs.ty.isInt()) {
            try p.errStr(.implicit_int_to_ptr, token, try p.typePairStrExtra(rhs.ty, " to ", lhs.ty));
            try rhs.ptrCast(p, unqualType);
        } else if (rhs.ty.isPointer()) {
            if (!unqualType.isVoidStar() and !rhs.ty.isVoidStar() and !unqualType.eql(rhs.ty, false)) {
                try p.errStr(.incompatible_ptr_assign, token, try p.typePairStrExtra(lhs.ty, eMsg, rhs.ty));
            } else if (!unqualType.eql(rhs.ty, true)) {
                if (!unqualType.getElemType().qual.hasQuals(rhs.ty.getElemType().qual))
                    try p.errStr(.ptr_assign_discards_quals, token, try p.typePairStrExtra(lhs.ty, eMsg, rhs.ty));
                try rhs.ptrCast(p, unqualType);
            }
        } else {
            try p.errStr(.incompatible_assign, token, try p.typePairStrExtra(lhs.ty, eMsg, rhs.ty));
        }
    } else if (lhs.ty.isRecord()) {
        if (!unqualType.eql(rhs.ty, false))
            try p.errStr(.incompatible_assign, token, try p.typePairStrExtra(lhs.ty, eMsg, rhs.ty));
    } else if (lhs.ty.isArray() or lhs.ty.isFunc()) {
        try p.errToken(.not_assignable, token);
    } else {
        try p.errStr(.incompatible_assign, token, try p.typePairStrExtra(lhs.ty, eMsg, rhs.ty));
    }

    try lhs.bin(p, tag, rhs);
    return lhs;
}

/// constExpr : conditionalExpr
fn constExpr(p: *Parser) Error!Result {
    const start = p.index;
    const res = try p.conditionalExpr();
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

/// conditionalExpr : logicalOrExpr ('?' expression? ':' conditionalExpr)?
fn conditionalExpr(p: *Parser) Error!Result {
    var cond = try p.logicalOrExpr();
    if (cond.empty(p) or p.eat(.QuestionMark) == null)
        return cond;
    const savedEval = p.noEval;

    // Depending on the value of the condition, avoid  evaluating unreachable
    var thenExpr = blk: {
        defer p.noEval = savedEval;
        if (cond.value != .unavailable and !cond.getBool())
            p.noEval = true;

        break :blk try p.parseExpr();
    };
    try thenExpr.expect(p);

    const colon = try p.expectToken(.Colon);

    var elseExpr = blk: {
        defer p.noEval = savedEval;
        if (cond.value != .unavailable and cond.getBool())
            p.noEval = true;

        break :blk try p.conditionalExpr();
    };
    try elseExpr.expect(p);

    _ = try thenExpr.adjustTypes(colon, &elseExpr, p, .conditional);

    if (cond.value != .unavailable) {
        cond.value = if (cond.getBool()) thenExpr.value else elseExpr.value;
    } else {
        try thenExpr.saveValue(p);
        try elseExpr.saveValue(p);
    }

    cond.ty = thenExpr.ty;
    cond.node = try p.addNode(.{
        .tag = .CondExpr,
        .type = cond.ty,
        .data = .{ .If3 = .{ .cond = cond.node, .body = (try p.addList(&.{ thenExpr.node, elseExpr.node })).start } },
    });

    return cond;
}

/// logicalOrExpr : logicalAndExpr ('||' logicalAndExpr)*
fn logicalOrExpr(p: *Parser) Error!Result {
    var lhs = try p.logicalAndExpr();
    if (lhs.empty(p))
        return lhs;
    const savedEval = p.noEval;
    defer p.noEval = savedEval;

    while (p.eat(.PipePipe)) |token| {
        if (lhs.value != .unavailable and lhs.getBool())
            p.noEval = true;

        var rhs = try p.logicalAndExpr();
        try rhs.expect(p);

        if (try lhs.adjustTypes(token, &rhs, p, .booleanLogic)) {
            const res = @intFromBool(lhs.getBool() or rhs.getBool());
            lhs.value = .{ .signed = res };
        }

        lhs.ty = .{ .specifier = .Int };
        try lhs.bin(p, .BoolOrExpr, rhs);
    }

    return lhs;
}

/// logicalAndExpr : orExpr ('&&' orExpr)*
fn logicalAndExpr(p: *Parser) Error!Result {
    var lhs = try p.orExpr();
    if (lhs.empty(p))
        return lhs;

    const savedEval = p.noEval;
    defer p.noEval = savedEval;

    while (p.eat(.AmpersandAmpersand)) |token| {
        if (lhs.value != .unavailable and !lhs.getBool())
            p.noEval = true;

        var rhs = try p.orExpr();
        try rhs.expect(p);

        if (try lhs.adjustTypes(token, &rhs, p, .booleanLogic)) {
            const res = @intFromBool(lhs.getBool() and rhs.getBool());
            lhs.value = .{ .signed = res };
        }

        lhs.ty = .{ .specifier = .Int };
        try lhs.bin(p, .BoolAndExpr, rhs);
    }
    return lhs;
}

/// orExpr : xorExpr ('|' xorExpr)*
fn orExpr(p: *Parser) Error!Result {
    var lhs = try p.xorExpr();
    if (lhs.empty(p))
        return lhs;

    while (p.eat(.Pipe)) |token| {
        var rhs = try p.xorExpr();
        try rhs.expect(p);

        if (try lhs.adjustTypes(token, &rhs, p, .integer)) {
            lhs.value = switch (lhs.value) {
                .unsigned => |v| .{ .unsigned = v | rhs.value.unsigned },
                .signed => |v| .{ .signed = v | rhs.value.signed },
                else => unreachable,
            };
        }

        try lhs.bin(p, .BitOrExpr, rhs);
    }
    return lhs;
}

/// xorExpr : andExpr ('^' andExpr)*
fn xorExpr(p: *Parser) Error!Result {
    var lhs = try p.andExpr();
    if (lhs.empty(p))
        return lhs;

    while (p.eat(.Caret)) |token| {
        var rhs = try p.andExpr();
        try rhs.expect(p);

        if (try lhs.adjustTypes(token, &rhs, p, .integer)) {
            lhs.value = switch (lhs.value) {
                .unsigned => |v| .{ .unsigned = v ^ rhs.value.unsigned },
                .signed => |v| .{ .signed = v ^ rhs.value.signed },
                else => unreachable,
            };
        }

        try lhs.bin(p, .BitXorExpr, rhs);
    }
    return lhs;
}

/// andExpr : eqExpr ('&' eqExpr)*
fn andExpr(p: *Parser) Error!Result {
    var lhs = try p.eqExpr();
    if (lhs.empty(p))
        return lhs;

    while (p.eat(.Ampersand)) |token| {
        var rhs = try p.eqExpr();
        try rhs.expect(p);

        if (try lhs.adjustTypes(token, &rhs, p, .integer)) {
            lhs.value = switch (lhs.value) {
                .unsigned => |v| .{ .unsigned = v & rhs.value.unsigned },
                .signed => |v| .{ .signed = v & rhs.value.signed },
                else => unreachable,
            };
        }

        try lhs.bin(p, .BitAndExpr, rhs);
    }
    return lhs;
}

/// eqExpr : compExpr (('==' | '!=') compExpr)*
fn eqExpr(p: *Parser) Error!Result {
    var lhs = try p.compExpr();
    if (lhs.empty(p))
        return lhs;

    while (true) {
        const eq = p.eat(.EqualEqual);
        const ne = eq orelse p.eat(.BangEqual);
        const tag = p.tokToTag(ne orelse break);
        var rhs = try p.compExpr();
        try rhs.expect(p);

        if (try lhs.adjustTypes(ne.?, &rhs, p, .equality)) {
            const res = if (tag == .EqualExpr)
                lhs.compare(.eq, rhs)
            else
                lhs.compare(.neq, rhs);

            lhs.value = .{ .signed = @intFromBool(res) };
        }

        lhs.ty = .{ .specifier = .Int };
        try lhs.bin(p, tag, rhs);
    }
    return lhs;
}

/// compExpr : shiftExpr (('<' | '<=' | '>' | '>=') shiftExpr)*
fn compExpr(p: *Parser) Error!Result {
    var lhs = try p.shiftExpr();
    if (lhs.empty(p))
        return lhs;

    while (true) {
        const lt = p.eat(.AngleBracketLeft);
        const le = lt orelse p.eat(.AngleBracketLeftEqual);
        const gt = le orelse p.eat(.AngleBracketRight);
        const ge = gt orelse p.eat(.AngleBracketRightEqual);
        const tag = p.tokToTag(ge orelse break);
        var rhs = try p.shiftExpr();
        try rhs.expect(p);

        if (try lhs.adjustTypes(ge.?, &rhs, p, .relational)) {
            const res = @intFromBool(switch (tag) {
                .LessThanExpr => lhs.compare(.lt, rhs),
                .LessThanEqualExpr => lhs.compare(.lte, rhs),
                .GreaterThanExpr => lhs.compare(.gt, rhs),
                .GreaterThanEqualExpr => lhs.compare(.gte, rhs),
                else => unreachable,
            });

            lhs.value = .{ .signed = res };
        }

        lhs.ty = .{ .specifier = .Int };
        try lhs.bin(p, tag, rhs);
    }

    return lhs;
}

/// shiftExpr : addExpr (('<<' | '>>') addExpr)*
fn shiftExpr(p: *Parser) Error!Result {
    var lhs = try p.addExpr();
    if (lhs.empty(p))
        return lhs;

    while (true) {
        const shl = p.eat(.AngleBracketAngleBracketLeft);
        const shr = shl orelse p.eat(.AngleBracketAngleBracketRight);
        const tag = p.tokToTag(shr orelse break);
        var rhs = try p.addExpr();
        try rhs.expect(p);

        if (try lhs.adjustTypes(shr.?, &rhs, p, .integer)) {
            // TODO overflow
            if (shl != null) {
                lhs.value = switch (lhs.value) {
                    .unsigned => |v| .{ .unsigned = v << @as(u6, @intCast(rhs.value.unsigned)) },
                    .signed => |v| .{ .signed = v << @as(u6, @intCast(rhs.value.signed)) },
                    else => unreachable,
                };
            } else {
                lhs.value = switch (lhs.value) {
                    .unsigned => |v| .{ .unsigned = v >> @as(u6, @intCast(rhs.value.unsigned)) },
                    .signed => |v| .{ .signed = v >> @as(u6, @intCast(rhs.value.signed)) },
                    else => unreachable,
                };
            }
        }

        try lhs.bin(p, tag, rhs);
    }
    return lhs;
}

/// addExpr : mulExpr (('+' | '-') mulExpr)*
fn addExpr(p: *Parser) Error!Result {
    var lhs = try p.mulExpr();
    if (lhs.empty(p))
        return lhs;

    while (true) {
        const plus = p.eat(.Plus);
        const minus = plus orelse p.eat(.Minus);
        const tag = p.tokToTag(minus orelse break);
        var rhs = try p.mulExpr();
        try rhs.expect(p);

        if (try lhs.adjustTypes(minus.?, &rhs, p, if (plus != null) .add else .sub)) {
            if (plus != null) {
                try lhs.add(plus.?, rhs, p);
            } else {
                try lhs.sub(minus.?, rhs, p);
            }
        }

        try lhs.bin(p, tag, rhs);
    }
    return lhs;
}

/// Implements C's % operator for signed integers, for evaluating constant expressions
/// caller guarantees rhs != 0
/// caller guarantees lhs != std.math.minInt(i64) OR rhs != -1
fn signedRemainder(lhs: i64, rhs: i64) i64 {
    if (rhs > 0) return @rem(lhs, rhs);
    return lhs - @divTrunc(lhs, rhs) * rhs;
}

/// mulExpr : castExpr (('*' | '/' | '%') castExpr)*´
fn mulExpr(p: *Parser) Error!Result {
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

        if (rhs.isZero() and mul == null and !p.noEval) {
            const errTag: Diagnostics.Tag = if (p.inMacro) .division_by_zero_macro else .division_by_zero;
            lhs.value = .unavailable;
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
                try lhs.mul(mul.?, rhs, p);
            } else if (div != null) {
                lhs.value = switch (lhs.value) {
                    .unsigned => |v| .{ .unsigned = v / rhs.value.unsigned },
                    .signed => |v| .{ .signed = @divFloor(v, rhs.value.signed) },
                    else => unreachable,
                };
            } else {
                if (lhs.value == .signed and lhs.value.signed == std.math.minInt(i64) and rhs.value.signed == -1) {
                    lhs.value = if (p.inMacro) .{ .signed = 0 } else .unavailable;
                } else {
                    lhs.value = switch (lhs.value) {
                        .unsigned => |v| .{ .unsigned = v % rhs.value.unsigned },
                        .signed => |v| .{ .signed = signedRemainder(v, rhs.value.signed) },
                        else => unreachable,
                    };
                }
            }
        }
        try lhs.bin(p, tag, rhs);
    }

    return lhs;
}

/// castExpr :  ( '(' type_name ')' )
///  :  '(' typeName ')' castExpr
///  | '(' typeName ')' '{' initializerItems '}'
///  | __builtin_choose_expr '(' constExpr ',' assignExpr ',' assignExpr ')'
///  | unExpr
fn parseCastExpr(p: *Parser) Error!Result {
    if (p.eat(.LParen)) |lp| {
        if (try p.typeName()) |ty| {
            try p.expectClosing(lp, .RParen);

            if (p.getCurrToken() == .LBrace) {
                // compound literal
                if (ty.isFunc())
                    try p.err(.func_init)
                else if (ty.is(.VariableLenArray))
                    try p.err(.vla_init);

                var initListExpr = try p.initializer(ty);
                try initListExpr.un(p, .CompoundLiteralExpr);
                return initListExpr;
            }

            var operand = try p.parseCastExpr();
            try operand.expect(p);

            if (ty.is(.Void)) {
                // everything can cast to void
            } else if (ty.isInt() or ty.isFloat() or ty.isPointer()) {
                if (ty.isFloat() and operand.ty.isPointer())
                    try p.errStr(.invalid_cast_to_float, lp, try p.typeStr(operand.ty));
                if (operand.ty.isFloat() and ty.isPointer())
                    try p.errStr(.invalid_cast_to_pointer, lp, try p.typeStr(operand.ty));

                const isUnsigned = ty.isUnsignedInt(p.pp.compilation);
                if (isUnsigned and operand.value == .signed) {
                    const copy = operand.value.signed;
                    operand.value = .{ .unsigned = @as(u64, @bitCast(copy)) };
                } else if (!isUnsigned and operand.value == .unsigned) {
                    const copy = operand.value.unsigned;
                    operand.value = .{ .signed = @as(i64, @bitCast(copy)) };
                }
            } else {
                try p.errStr(.invalid_cast_type, lp, try p.typeStr(operand.ty));
            }

            if (ty.containAnyQual())
                try p.errStr(.qual_cast, lp, try p.typeStr(ty));

            operand.ty = ty;
            operand.ty.qual = .{};

            try operand.un(p, .CastExpr);
            return operand;
        }
        p.index -= 1;
    }

    switch (p.getCurrToken()) {
        .BuiltinChooseExpr => return p.parseBuiltinChooseExpr(),
        // TODO: other special-cased builtins
        else => {},
    }

    return p.parseUnaryExpr();
}

fn parseBuiltinChooseExpr(p: *Parser) Error!Result {
    p.index += 1;
    const lp = try p.expectToken(.LParen);
    const condToken = p.index;
    var cond = try p.constExpr();
    if (cond.value == .unavailable) {
        try p.errToken(.builtin_choose_cond, condToken);
        return error.ParsingFailed;
    }

    _ = try p.expectToken(.Comma);

    var thenExpr = if (cond.getBool()) try p.assignExpr() else try p.parseNoEval(assignExpr);
    try thenExpr.expect(p);

    _ = try p.expectToken(.Comma);

    var elseExpr = if (!cond.getBool()) try p.assignExpr() else try p.parseNoEval(assignExpr);
    try elseExpr.expect(p);

    try p.expectClosing(lp, .RParen);

    if (cond.getBool()) {
        cond.value = thenExpr.value;
        cond.ty = thenExpr.ty;
    } else {
        cond.value = elseExpr.value;
        cond.ty = elseExpr.ty;
    }
    cond.node = try p.addNode(.{
        .tag = .BuiltinChooseExpr,
        .type = cond.ty,
        .data = .{ .If3 = .{ .cond = cond.node, .body = (try p.addList(&.{ thenExpr.node, elseExpr.node })).start } },
    });

    return cond;
}

/// unaryExpr
///  : primaryExpr suffixExpr*
///  | '&&' identifier
///  | ('&' | '*' | '+' | '-' | '~' | '!' | '++' | '--' | KeywordGccExtension) castExpr
///  | keyword_sizeof unaryExpr
///  | keyword_sizeof '(' type_name ')'
///  | keyword_alignof '(' type_name ')'
fn parseUnaryExpr(p: *Parser) Error!Result {
    const index = p.index;
    switch (p.tokenIds[index]) {
        .Ampersand => {
            if (p.inMacro) {
                try p.err(.invalid_preproc_operator);
                return error.ParsingFailed;
            }
            p.index += 1;
            var operand = try p.parseCastExpr();
            try operand.expect(p);

            const slice = p.nodes.slice();
            if (!AST.isLValue(slice, p.data.items, p.valueMap, operand.node)) {
                try p.errToken(.addr_of_rvalue, index);
            }

            if (operand.ty.qual.register)
                try p.errToken(.addr_of_register, index);

            const elemType = try p.arena.create(Type);
            elemType.* = operand.ty;
            operand.ty = Type{
                .specifier = .Pointer,
                .data = .{ .subType = elemType },
            };

            try operand.un(p, .AddrOfExpr);
            return operand;
        },

        .AmpersandAmpersand => {
            const addressToken = p.index;
            p.index += 1;
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
                    .data = .{ .DeclarationRef = nameToken },
                    .type = resultType,
                }),
                .ty = resultType,
            };
        },

        .Asterisk => {
            const asteriskLoc = p.index;
            p.index += 1;
            var operand = try p.parseCastExpr();
            try operand.expect(p);

            if (operand.ty.isArray() or operand.ty.isPointer()) {
                operand.ty = operand.ty.getElemType();
            } else if (!operand.ty.isFunc()) {
                try p.errToken(.indirection_ptr, index);
            }

            if (operand.ty.hasIncompleteSize() and !operand.ty.is(.Void))
                try p.errStr(.deref_incomplete_ty_ptr, asteriskLoc, try p.typeStr(operand.ty));

            operand.ty.qual = .{};
            try operand.un(p, .DerefExpr);
            return operand;
        },

        .Plus => {
            p.index += 1;
            var operand = try p.parseCastExpr();
            try operand.expect(p);
            try operand.lvalConversion(p);

            if (!operand.ty.isInt() and !operand.ty.isFloat())
                try p.errStr(.invalid_argument_un, index, try p.typeStr(operand.ty));

            if (operand.ty.isInt())
                try operand.intCast(p, operand.ty.integerPromotion(p.pp.compilation));

            return operand;
        },

        .Minus => {
            p.index += 1;
            var operand = try p.parseCastExpr();
            try operand.expect(p);
            try operand.lvalConversion(p);

            if (!operand.ty.isInt() and !operand.ty.isFloat())
                try p.errStr(.invalid_argument_un, index, try p.typeStr(operand.ty));

            if (operand.ty.isInt())
                try operand.intCast(p, operand.ty.integerPromotion(p.pp.compilation));

            const size = operand.ty.sizeof(p.pp.compilation).?;
            switch (operand.value) {
                .unsigned => |*v| switch (size) {
                    1, 2, 4 => v.* = @truncate(0 -% v.*),
                    8 => v.* = 0 -% v.*,
                    else => unreachable,
                },
                .signed => |*v| switch (size) {
                    1, 2, 4 => v.* = @truncate(0 -% v.*),
                    8 => v.* = 0 -% v.*,
                    else => unreachable,
                },
                .unavailable => {},
            }
            try operand.un(p, .NegateExpr);
            return operand;
        },

        .PlusPlus => {
            p.index += 1;
            var operand = try p.parseCastExpr();
            try operand.expect(p);

            if (!operand.ty.isInt() and !operand.ty.isFloat() and !operand.ty.isReal() and !operand.ty.isPointer())
                try p.errStr(.invalid_argument_un, index, try p.typeStr(operand.ty));

            if (!AST.isLValue(p.nodes.slice(), p.data.items, p.valueMap, operand.node) or operand.ty.isConst()) {
                try p.errToken(.not_assignable, index);
                return error.ParsingFailed;
            }

            if (operand.ty.isInt())
                try operand.intCast(p, operand.ty.integerPromotion(p.pp.compilation));

            switch (operand.value) {
                .unsigned => |*v| v.* += 1,
                .signed => |*v| v.* += 1,
                .unavailable => {},
            }

            try operand.un(p, .PreIncExpr);
            return operand;
        },

        .MinusMinus => {
            p.index += 1;
            var operand = try p.parseCastExpr();
            try operand.expect(p);

            if (!operand.ty.isInt() and !operand.ty.isFloat() and !operand.ty.isReal() and !operand.ty.isPointer())
                try p.errStr(.invalid_argument_un, index, try p.typeStr(operand.ty));

            if (!AST.isLValue(p.nodes.slice(), p.data.items, p.valueMap, operand.node) or operand.ty.isConst()) {
                try p.errToken(.not_assignable, index);
                return error.ParsingFailed;
            }

            if (operand.ty.isInt())
                try operand.intCast(p, operand.ty.integerPromotion(p.pp.compilation));

            switch (operand.value) {
                .unsigned => |*v| v.* -= 1,
                .signed => |*v| v.* -= 1,
                .unavailable => {},
            }

            try operand.un(p, .PreDecExpr);
            return operand;
        },

        .Tilde => {
            p.index += 1;
            var operand = try p.parseCastExpr();
            try operand.expect(p);
            try operand.lvalConversion(p);

            if (!operand.ty.isInt())
                try p.errStr(.invalid_argument_un, index, try p.typeStr(operand.ty));

            if (operand.ty.isInt())
                try operand.intCast(p, operand.ty.integerPromotion(p.pp.compilation));

            switch (operand.value) {
                .unsigned => |*v| v.* = ~v.*,
                .signed => |*v| v.* = ~v.*,
                .unavailable => {},
            }

            try operand.un(p, .BoolNotExpr);
            return operand;
        },

        .Bang => {
            p.index += 1;
            var operand = try p.parseCastExpr();
            try operand.expect(p);
            try operand.lvalConversion(p);

            if (!operand.ty.isInt() and !operand.ty.isFloat() and !operand.ty.isPointer())
                try p.errStr(.invalid_argument_un, index, try p.typeStr(operand.ty));

            if (operand.ty.isInt())
                try operand.intCast(p, operand.ty.integerPromotion(p.pp.compilation));

            if (operand.value != .unavailable) {
                const res = @intFromBool(!operand.getBool());
                operand.value = .{ .signed = res };
            }

            operand.ty = .{ .specifier = .Int };
            try operand.un(p, .BoolNotExpr);
            return operand;
        },

        .KeywordSizeof => {
            p.index += 1;
            const expectedParen = p.index;
            var res = Result{};
            if (try p.typeName()) |ty| {
                res.ty = ty;
                try p.errToken(.expected_parens_around_typename, expectedParen);
            } else if (p.eat(.LParen)) |lp| {
                if (try p.typeName()) |ty| {
                    res.ty = ty;
                    try p.expectClosing(lp, .RParen);
                } else {
                    p.index = expectedParen;
                    res = try p.parseNoEval(parseUnaryExpr);
                }
            } else {
                res = try p.parseNoEval(parseUnaryExpr);
            }

            if (res.ty.sizeof(p.pp.compilation)) |size| {
                res.value = .{ .unsigned = size };
            } else {
                res.value = .unavailable;
                try p.errStr(.invalid_sizeof, expectedParen - 1, try p.typeStr(res.ty));
            }

            res.ty = Type.sizeT(p.pp.compilation);
            try res.un(p, .SizeOfExpr);
            return res;
        },

        .KeywordAlignof, .KeywordGccAlignof1, .KeywordGccAlignof2 => {
            p.index += 1;
            const expectedParen = p.index;
            var res = Result{};
            if (try p.typeName()) |ty| {
                res.ty = ty;
                try p.errToken(.expected_parens_around_typename, expectedParen);
            } else if (p.eat(.LParen)) |lp| {
                if (try p.typeName()) |ty| {
                    res.ty = ty;
                    try p.expectClosing(lp, .RParen);
                } else {
                    p.index = expectedParen;
                    res = try p.parseNoEval(parseUnaryExpr);
                    try p.errToken(.alignof_expr, expectedParen);
                }
            } else {
                res = try p.parseNoEval(parseUnaryExpr);
                try p.errToken(.alignof_expr, expectedParen);
            }

            res.ty = Type.sizeT(p.pp.compilation);
            res.value = .{ .unsigned = res.ty.alignof(p.pp.compilation) };
            try res.un(p, .AlignOfExpr);
            return res;
        },

        .KeywordGccExtension => {
            p.index += 1;
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

/// suffixExpr
///  : '[' expr ']'
///  | '(' argumentExprList? ')'
///  | '.' IDENTIFIER
///  | '->' IDENTIFIER
///  | '++'
///  | '--'
fn parseSuffixExpr(p: *Parser, lhs: Result) Error!Result {
    std.debug.assert(!lhs.empty(p));
    switch (p.getCurrToken()) {
        .LBracket => {
            const lb = p.index;
            p.index += 1;
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
            p.index += 1;
            const name = try p.expectIdentifier();
            const fieldType = try p.getFieldAccessField(lhs.ty, name, false);
            return Result{
                .ty = fieldType,
                .node = try p.addNode(.{
                    .tag = .MemberAccessExpr,
                    .type = fieldType,
                    .data = .{ .Member = .{ .lhs = lhs.node, .name = name } },
                }),
            };
        },

        .Arrow => {
            p.index += 1;
            const name = try p.expectIdentifier();
            const fieldType = try p.getFieldAccessField(lhs.ty, name, true);
            return Result{
                .ty = fieldType,
                .node = try p.addNode(.{
                    .tag = .MemberAccessPtrExpr,
                    .type = fieldType,
                    .data = .{ .Member = .{ .lhs = lhs.node, .name = name } },
                }),
            };
        },

        .PlusPlus => {
            defer p.index += 1;
            var operand = lhs;

            if (!operand.ty.isInt() and !operand.ty.isFloat() and !operand.ty.isReal() and !operand.ty.isPointer())
                try p.errStr(.invalid_argument_un, p.index, try p.typeStr(operand.ty));

            if (!AST.isLValue(p.nodes.slice(), p.data.items, p.valueMap, operand.node) or operand.ty.isConst()) {
                try p.err(.not_assignable);
                return error.ParsingFailed;
            }

            if (operand.ty.isInt())
                try operand.intCast(p, operand.ty.integerPromotion(p.pp.compilation));

            try operand.un(p, .PostIncExpr);
            return operand;
        },

        .MinusMinus => {
            defer p.index += 1;
            var operand = lhs;

            if (!operand.ty.isInt() and !operand.ty.isFloat() and !operand.ty.isReal() and !operand.ty.isPointer())
                try p.errStr(.invalid_argument_un, p.index, try p.typeStr(operand.ty));

            if (!AST.isLValue(p.nodes.slice(), p.data.items, p.valueMap, operand.node) or operand.ty.isConst()) {
                try p.err(.not_assignable);
                return error.ParsingFailed;
            }

            if (operand.ty.isInt())
                try operand.intCast(p, operand.ty.integerPromotion(p.pp.compilation));

            try operand.un(p, .PostDecExpr);
            return operand;
        },

        else => return Result{},
    }
}

fn getFieldAccessField(
    p: *Parser,
    exprType: Type,
    fieldNameToken: TokenIndex,
    isArrow: bool,
) !Type {
    const isPtr = exprType.get(.Pointer) != null;
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

    // TODO deal with anonymous structs
    const fieldName = p.getTokenSlice(fieldNameToken);
    const field = recordType.getField(fieldName) orelse {
        const stringsTop = p.strings.items.len;
        defer p.strings.items.len = stringsTop;

        try p.strings.writer().print("'{s}' in '", .{fieldName});
        try exprType.print(p.strings.writer());
        try p.strings.append('\'');

        try p.errStr(.no_such_member, fieldNameToken, try p.arena.dupe(u8, p.strings.items[stringsTop..]));
        return error.ParsingFailed;
    };
    return field.f.ty;
}

fn reportParam(p: *Parser, paramToken: TokenIndex, arg: Result, argCount: u32, params: []Type.Function.Param) Error!void {
    try p.errStr(.incompatible_param, paramToken, try p.typeStr(arg.ty));
    try p.errToken(.parameter_here, params[argCount].nameToken);
}

fn parseCallExpr(p: *Parser, lhs: Result) Error!Result {
    const lParen = p.index;
    p.index += 1;
    const ty = lhs.ty.isCallable() orelse {
        try p.errStr(.not_callable, lParen, try p.typeStr(lhs.ty));
        return error.ParsingFailed;
    };

    const params = ty.data.func.params;
    var func = lhs;
    try func.lvalConversion(p);

    const listBufferTop = p.listBuffer.items.len;
    defer p.listBuffer.items.len = listBufferTop;
    try p.listBuffer.append(func.node);
    var argCount: u32 = 0;

    var firstAfter = lParen;
    if (p.eat(.RParen) == null) {
        while (true) {
            const paramToken = p.index;
            if (argCount == params.len)
                firstAfter = p.index;

            var arg = try p.assignExpr();
            try arg.expect(p);
            try arg.lvalConversion(p);
            if (arg.ty.hasIncompleteSize() and !arg.ty.is(.Void))
                return error.ParsingFailed;

            if (argCount < params.len) {
                const paramType = params[argCount].ty;
                if (paramType.is(.Bool)) {
                    // this is ridiculous but it's what clang does
                    if (arg.ty.isInt() or arg.ty.isFloat() or arg.ty.isPointer())
                        try arg.boolCast(p, paramType)
                    else
                        try p.reportParam(paramToken, arg, argCount, params);
                } else if (paramType.isInt()) {
                    if (arg.ty.isInt() or arg.ty.isFloat()) {
                        try arg.intCast(p, paramType);
                    } else if (arg.ty.isPointer()) {
                        try p.errStr(
                            .implicit_ptr_to_int,
                            paramToken,
                            try p.typePairStrExtra(arg.ty, " to ", paramType),
                        );
                        try p.errToken(.parameter_here, params[argCount].nameToken);
                        try arg.intCast(p, paramType);
                    } else {
                        try p.reportParam(paramToken, arg, argCount, params);
                    }
                } else if (paramType.isFloat()) {
                    if (arg.ty.isInt() or arg.ty.isFloat())
                        try arg.floatCast(p, paramType)
                    else
                        try p.reportParam(paramToken, arg, argCount, params);
                } else if (paramType.isPointer()) {
                    if (arg.isZero()) {
                        try arg.nullCast(p, paramType);
                    } else if (arg.ty.isInt()) {
                        try p.errStr(
                            .implicit_int_to_ptr,
                            paramToken,
                            try p.typePairStrExtra(arg.ty, " to ", paramType),
                        );
                        try p.errToken(.parameter_here, params[argCount].nameToken);
                        try arg.intCast(p, paramType);
                    } else if (!arg.ty.isVoidStar() and !paramType.isVoidStar() and !paramType.eql(arg.ty, false)) {
                        try p.reportParam(paramToken, arg, argCount, params);
                    }
                } else if (paramType.isRecord()) {
                    if (!paramType.eql(arg.ty, false)) {
                        try p.reportParam(paramToken, arg, argCount, params);
                    }
                } else {
                    // should be unreachable
                    try p.reportParam(paramToken, arg, argCount, params);
                }
            } else {
                if (arg.ty.isInt())
                    try arg.intCast(p, arg.ty.integerPromotion(p.pp.compilation));
                if (arg.ty.is(.Float))
                    try arg.floatCast(p, .{ .specifier = .Double });
            }

            try arg.saveValue(p);
            try p.listBuffer.append(arg.node);
            argCount += 1;
            _ = p.eat(.Comma) orelse break;
        }

        try p.expectClosing(lParen, .RParen);
    }

    const extra = Diagnostics.Message.Extra{
        .arguments = .{
            .expected = @as(u32, @intCast(params.len)),
            .actual = @as(u32, @intCast(argCount)),
        },
    };
    if (ty.is(.Func) and params.len != argCount)
        try p.errExtra(.expected_arguments, firstAfter, extra);

    if (ty.is(.OldStyleFunc) and params.len != argCount)
        try p.errExtra(.expected_arguments_old, firstAfter, extra);

    if (ty.is(.VarArgsFunc) and argCount < params.len)
        try p.errExtra(.expected_at_least_arguments, firstAfter, extra);

    var callNode: AST.Node = .{
        .tag = .CallExprOne,
        .type = ty.data.func.returnType,
        .data = .{ .BinaryExpr = .{ .lhs = lhs.node, .rhs = .none } },
    };

    const args = p.listBuffer.items[listBufferTop..];
    switch (argCount) {
        0 => {},
        1 => callNode.data.BinaryExpr.rhs = args[1], //args[0]  == lhs.node
        else => {
            callNode.tag = .CallExpr;
            callNode.data = .{ .range = try p.addList(args) };
        },
    }
    return Result{ .node = try p.addNode(callNode), .ty = callNode.type };
}

fn checkArrayBounds(p: *Parser, index: Result, arrayType: Type, token: TokenIndex) !void {
    const len = arrayType.arrayLen() orelse return;
    switch (index.value) {
        .unsigned => |val| if (std.math.compare(val, .gte, len))
            try p.errExtra(.array_after, token, .{ .unsigned = val }),
        .signed => |val| if (val < 0)
            try p.errExtra(.array_before, token, .{ .signed = val })
        else if (std.math.compare(val, .gte, len))
            try p.errExtra(.array_after, token, .{ .unsigned = @as(u64, @intCast(val)) }),
        .unavailable => return,
    }
}

//// primaryExpr
////  : IDENTIFIER
////  | INTEGER_LITERAL
////  | FLOAT_LITERAL
////  | CHAR_LITERAL
////  | STRING_LITERAL
////  | '(' expr ')'
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
            const sym = p.findSymbol(nameToken, .reference) orelse {
                if (p.getCurrToken() == .LParen) {
                    // implicitly declare simple functions as like `puts("foo")`;
                    const name = p.getTokenSlice(nameToken);
                    try p.errStr(.implicit_func_decl, nameToken, name);

                    const funcType = try p.arena.create(Type.Function);
                    funcType.* = .{ .returnType = .{ .specifier = .Int }, .params = &.{} };
                    const ty: Type = .{ .specifier = .OldStyleFunc, .data = .{ .func = funcType } };
                    const node = try p.addNode(.{
                        .type = ty,
                        .tag = .FnProto,
                        .data = .{ .Declaration = .{ .name = nameToken } },
                    });

                    try p.declBuffer.append(node);
                    try p.scopes.append(.{ .declaration = .{
                        .name = name,
                        .type = ty,
                        .nameToken = nameToken,
                    } });

                    return Result{
                        .ty = ty,
                        .node = try p.addNode(.{ .tag = .DeclRefExpr, .type = ty, .data = .{ .DeclarationRef = nameToken } }),
                    };
                }
                try p.errStr(.undeclared_identifier, nameToken, p.getTokenSlice(nameToken));
                return error.ParsingFailed;
            };

            switch (sym) {
                .enumeration => |e| {
                    var res = e.value;
                    res.node = try p.addNode(.{
                        .tag = .EnumerationRef,
                        .type = res.ty,
                        .data = .{ .DeclarationRef = nameToken },
                    });

                    return res;
                },

                .declaration, .definition, .param => |s| return Result{
                    .ty = s.type,
                    .node = try p.addNode(.{
                        .tag = .DeclRefExpr,
                        .type = s.type,
                        .data = .{ .DeclarationRef = nameToken },
                    }),
                },

                else => unreachable,
            }
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

        .FloatLiteral => {
            defer p.index += 1;
            const ty = Type{ .specifier = .Double };
            return Result{
                .ty = ty,
                .node = try p.addNode(
                    .{
                        .tag = .DoubleLiteral,
                        .type = ty,
                        .data = .{ .Double = try p.parseFloat(p.index, f64) },
                    },
                ),
            };
        },

        .FloatLiteral_F => {
            defer p.index += 1;
            const ty = Type{ .specifier = .Float };
            return Result{
                .ty = ty,
                .node = try p.addNode(
                    .{
                        .tag = .FloatLiteral,
                        .type = ty,
                        .data = .{ .Float = try p.parseFloat(p.index, f32) },
                    },
                ),
            };
        },

        .FloatLiteral_L => return p.todo("long double literals"),

        .Zero => {
            p.index += 1;
            var res: Result = .{ .value = .{ .signed = 0 } };
            res.node = try p.addNode(.{ .tag = .IntLiteral, .type = res.ty, .data = .{ .Int = 0 } });
            return res;
        },

        .One => {
            p.index += 1;
            var res: Result = .{ .value = .{ .signed = 1 } };
            res.node = try p.addNode(.{ .tag = .IntLiteral, .type = res.ty, .data = .{ .Int = 1 } });
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

fn parseFloat(p: *Parser, tok: TokenIndex, comptime T: type) Error!T {
    var bytes = p.getTokenSlice(tok);
    if (p.tokenIds[tok] != .FloatLiteral)
        bytes = bytes[0 .. bytes.len - 1];

    return std.fmt.parseFloat(T, bytes) catch |e| switch (e) {
        error.InvalidCharacter => unreachable, // validated by Tokenizer
    };
}

fn castInt(p: *Parser, val: u64, specs: []const Type.Specifier) Error!Result {
    var res: Result = .{};
    for (specs) |spec| {
        const ty = Type{ .specifier = spec };
        const isUnsigned = ty.isUnsignedInt(p.pp.compilation);
        const tySize = ty.sizeof(p.pp.compilation).?;
        res.ty = ty;

        if (isUnsigned) {
            res.value = .{ .unsigned = val };
            switch (tySize) {
                2 => if (val <= std.math.maxInt(u16)) break,
                4 => if (val <= std.math.maxInt(u32)) break,
                8 => if (val <= std.math.maxInt(u64)) break,
                else => unreachable,
            }
        } else {
            res.value = .{ .signed = @as(i64, @bitCast(val)) };
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

    res.node = try p.addNode(.{ .tag = .IntLiteral, .type = res.ty, .data = .{ .Int = val } });
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

//// genericSelection : keyword_generic '(' assignExpr ',' genericAssoc (',' genericAssoc)* ')'
//// genericAssoc
////  : typeName ':' assignExpr
////  | keyword_default ':' assignExpr
fn parseGenericSelection(p: *Parser) Error!Result {
    p.index += 1;
    const lp = try p.expectToken(.LParen);
    const controlling = try p.parseNoEval(assignExpr);
    _ = try p.expectToken(.Comma);

    const listBufferTop = p.listBuffer.items.len;
    defer p.listBuffer.items.len = listBufferTop;

    try p.listBuffer.append(controlling.node);

    var defaultToken: ?TokenIndex = null;
    var chosen: Result = .{};

    while (true) {
        const start = p.index;
        if (try p.typeName()) |ty| {
            if (ty.containAnyQual()) {
                try p.errToken(.generic_qual_type, start);
            }

            _ = try p.expectToken(.Colon);
            chosen = try p.assignExpr();
            try chosen.expect(p);
            try chosen.saveValue(p);

            try p.listBuffer.append(try p.addNode(.{
                .tag = .GenericAssociationExpr,
                .type = ty,
                .data = .{ .UnaryExpr = chosen.node },
            }));
        } else if (p.eat(.KeywordDefault)) |tok| {
            if (defaultToken) |prev| {
                try p.errToken(.generic_duplicate_default, tok);
                try p.errToken(.previous_case, prev);
            }

            defaultToken = tok;
            _ = try p.expectToken(.Colon);
            chosen = try p.assignExpr();
            try chosen.expect(p);
            try chosen.saveValue(p);

            try p.listBuffer.append(try p.addNode(.{
                .tag = .GenericDefaultExpr,
                .data = .{ .UnaryExpr = chosen.node },
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
        .data = .{ .BinaryExpr = .{ .lhs = controlling.node, .rhs = chosen.node } },
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
    var slice = p.getTokenSlice(p.index);

    defer p.index += 1;

    var base: u8 = 10;
    if (std.mem.startsWith(u8, slice, "0x") or std.mem.startsWith(u8, slice, "0X")) {
        slice = slice[2..];
        base = 16;
    } else if (std.mem.startsWith(u8, slice, "0b") or std.mem.startsWith(u8, slice, "0B")) {
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

        const addOV = @addWithOverflow(value, digit);
        if (addOV[1] != 0)
            overflow = true;
        value = addOV[0];
    }

    if (overflow) {
        try p.err(.int_literal_too_big);
        var res: Result = .{ .ty = .{ .specifier = .ULongLong }, .value = .{ .unsigned = value } };
        res.node = try p.addNode(.{ .tag = .IntLiteral, .type = res.ty, .data = .{ .Int = value } });
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
    const litToken = p.index;
    const ty: Type = switch (p.getCurrToken()) {
        .CharLiteral => .{ .specifier = .Int },
        else => return p.todo("unicode char literals"),
    };
    p.index += 1;

    var val: u32 = 0;
    var overflowReported = false;
    var multichar: u8 = 0;
    var slice = p.getTokenSlice(litToken);
    slice = slice[0 .. slice.len - 1];
    var i = std.mem.indexOf(u8, slice, "\'").? + 1;
    while (i < slice.len) : (i += 1) {
        var c = slice[i];
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
                    'e' => c = 0x1B,
                    'f' => c = 0x0C,
                    'v' => c = 0x0B,
                    'x' => c = try p.parseNumberEscape(litToken, 16, slice, &i),
                    '0'...'7' => c = try p.parseNumberEscape(litToken, 8, slice, &i),
                    'u', 'U' => return p.todo("unicode escapes in char literals"),
                    else => unreachable,
                }
            },
            else => {},
        }

        const mulOV = @mulWithOverflow(val, 0xff);
        if (mulOV[1] != 0 and !overflowReported) {
            try p.errExtra(.char_lit_too_wide, litToken, .{ .unsigned = i });
            overflowReported = true;
        }
        val = mulOV[0] + c;

        switch (multichar) {
            0 => multichar = 1,
            1 => {
                multichar = 2;
                try p.errToken(.multichar_literal, litToken);
            },
            else => {},
        }
    }

    return Result{
        .ty = ty,
        .value = .{ .unsigned = val },
        .node = try p.addNode(.{
            .tag = .IntLiteral,
            .type = ty,
            .data = .{ .Int = val },
        }),
    };
}

fn parseStringLiteral(p: *Parser) Error!Result {
    var start = p.index;
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

        p.index += 1;
    }

    if (width == null)
        width = 8;

    if (width.? != 8)
        return p.todo("unicode string literals");

    const index = p.strings.items.len;

    while (start < p.index) : (start += 1) {
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
                        'e' => p.strings.appendAssumeCapacity(0x1B),
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
    const len = p.strings.items.len - index;

    const arrayType = try p.arena.create(Type.Array);
    arrayType.* = .{ .elem = .{ .specifier = .Char }, .len = len };

    var res: Result = .{
        .ty = .{
            .specifier = .Array,
            .data = .{ .array = arrayType },
        },
    };

    res.node = try p.addNode(.{
        .tag = .StringLiteralExpr,
        .type = res.ty,
        .data = .{ .String = .{ .index = @as(u32, @intCast(index)), .len = @as(u32, @intCast(len)) } },
    });

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
