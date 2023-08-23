const std = @import("std");
const Compilation = @import("../Basic/Compilation.zig");
const Source = @import("../Basic/Source.zig");
const TokenType = @import("../Basic/TokenType.zig").TokenType;
const Lexer = @import("../Lexer/Lexer.zig");
const Preprocessor = @import("../Lexer/Preprocessor.zig");
const AST = @import("../AST/AST.zig");
const AstTag = @import("../AST/AstTag.zig").Tag;
const Type = @import("../AST/Type.zig");
const TypeBuilder = @import("../AST/TypeBuilder.zig").Builder;
const Diagnostics = @import("../Basic/Diagnostics.zig");
const DeclSpec = @import("../AST/DeclSpec.zig");
const Scope = @import("../Sema/Scope.zig").Scope;
const Result = @import("Result.zig");
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

// configuration
noEval: bool = false,
inMacro: bool = false,
inFunc: bool = false,
labelCount: u32 = 0,

const Label = union(enum) {
    unresolvedGoto: TokenIndex,
    label: TokenIndex,
};

fn eat(p: *Parser, expected: TokenType) ?TokenIndex {
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
    const actual = p.getCurrToken();
    if (actual != expected) {
        try p.errExtra(
            switch (actual) {
                .Invalid => .expected_invalid,
                else => .expected_token,
            },
            p.index,
            .{
                .tokenId = .{
                    .expected = expected,
                    .actual = actual,
                },
            },
        );
        return error.ParsingFailed;
    }

    defer p.index += 1;
    return p.index;
}

fn expectClosing(p: *Parser, opening: TokenIndex, id: TokenType) Error!void {
    _ = p.expectToken(id) catch |e|
        {
        if (e == error.ParsingFailed) {
            try p.pp.compilation.diag.add(.{
                .tag = switch (id) {
                    .RParen => .to_match_paren,
                    .RBrace => .to_match_brace,
                    .RBracket => .to_match_bracket,
                    else => unreachable,
                },
                .loc = p.pp.tokens.items(.loc)[opening],
            });
        }

        return e;
    };
}

fn tokSlice(p: *Parser, index: TokenIndex) []const u8 {
    if (p.tokenIds[index].lexeMe()) |some|
        return some;

    const loc = p.pp.tokens.items(.loc)[index];
    var lexer = Lexer{
        .buffer = if (loc.id == .generated)
            p.pp.generated.items
        else
            p.pp.compilation.getSource(loc.id).buffer,
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
    try p.pp.compilation.diag.add(.{
        .tag = tag,
        .loc = p.pp.tokens.items(.loc)[index],
        .extra = extra,
    });
}

pub fn errToken(p: *Parser, tag: Diagnostics.Tag, index: TokenIndex) Compilation.Error!void {
    @setCold(true);

    try p.pp.compilation.diag.add(.{
        .tag = tag,
        .loc = p.pp.tokens.items(.loc)[index],
    });
}

pub fn err(p: *Parser, tag: Diagnostics.Tag) Compilation.Error!void {
    @setCold(true);
    return p.errToken(tag, p.index);
}

pub fn todo(p: *Parser, msg: []const u8) Error {
    try p.errStr(.todo, p.index, msg);
    return error.ParsingFailed;
}

pub fn addNode(p: *Parser, node: AST.Node) Allocator.Error!NodeIndex {
    if (p.inMacro)
        return .none;

    const res = p.nodes.len;
    try p.nodes.append(p.pp.compilation.gpa, node);

    return @enumFromInt(@as(u32, @intCast(res)));
}

fn addList(p: *Parser, nodes: []const NodeIndex) Allocator.Error!AST.Range {
    const start: u32 = @intCast(p.data.items.len);
    try p.data.appendSlice(nodes);
    const end: u32 = @intCast(p.data.items.len);

    return AST.Range{ .start = start, .end = end };
}

fn findTypedef(p: *Parser, name: []const u8) ?Scope.Symbol {
    var i = p.scopes.items.len;
    while (i > 0) {
        i -= 1;
        switch (p.scopes.items[i]) {
            .typedef => |t| {
                if (std.mem.eql(u8, t.name, name))
                    return t;
            },

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
            .label => |l| if (std.mem.eql(u8, p.tokSlice(l), name)) return l,
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

fn findSymbol(p: *Parser, nameToken: TokenIndex) ?Scope {
    const name = p.tokSlice(nameToken);
    var i = p.scopes.items.len;
    while (i > 0) {
        i -= 1;
        const sym = p.scopes.items[i];
        switch (sym) {
            .symbol => |s| if (std.mem.eql(u8, s.name, name)) return sym,
            .enumeration => |e| if (std.mem.eql(u8, e.name, name)) return sym,
            else => {},
        }
    }

    return null;
}

fn findTag(p: *Parser, kind: TokenType, nameToken: TokenIndex) !?Scope.Symbol {
    const name = p.tokSlice(nameToken);
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

            .block => sawBlock = true,
            else => {},
        }
    }
    return null;
}

/// root : (decl | staticAssert)*
pub fn parse(pp: *Preprocessor) Compilation.Error!AST {
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
    }

    errdefer {
        p.nodes.deinit(pp.compilation.gpa);
        p.strings.deinit();
        p.valueMap.deinit();
    }

    _ = try p.addNode(.{ .tag = .Invalid, .type = undefined, .data = undefined });

    while (p.eat(.Eof) == null) {
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

        try p.err(.expected_external_decl);
        p.index += 1;
    }

    const data = try p.data.toOwnedSlice();
    errdefer pp.compilation.gpa.free(data);

    return AST{
        .comp = pp.compilation,
        .tokens = pp.tokens.slice(),
        .arena = arena,
        .generated = pp.generated.items,
        .nodes = p.nodes.toOwnedSlice(),
        .data = data,
        .rootDecls = try p.declBuffer.toOwnedSlice(),
        .strings = try p.strings.toOwnedSlice(),
        .valueMap = p.valueMap,
    };
}

fn nextExternDecl(p: *Parser) void {
    var parens: u32 = 0;

    while (p.index < p.tokenIds.len) : (p.index += 1) {
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
            .Identifier,
            => if (parens == 0) return,
            else => {},
        }
    }

    p.index -= 1; // so that we can consume the eof token elsewhere
}

// ====== declarations ======

/// decl
///  : declSpec (initDeclarator ( ',' initDeclarator)*)? ';'
///  | declSpec declarator decl* compoundStmt
///  | staticAssert
fn parseDeclaration(p: *Parser) Error!bool {
    const firstTokenIndex = p.index;
    var declSpec = if (try p.declSpecifier()) |some|
        some
    else blk: {
        if (p.inFunc) return false;

        switch (p.tokenIds[firstTokenIndex]) {
            .Asterisk, .LParen, .Identifier => {},
            else => return false,
        }

        var d = DeclSpec{};
        var spec: TypeBuilder = .{};
        try spec.finish(p, &d.type);

        break :blk d;
    };

    var initD = (try p.parseInitDeclarator(&declSpec)) orelse {
        // eat ';'
        _ = try p.expectToken(.Semicolon);

        if (declSpec.type.specifier == .Enum)
            return true;

        if (declSpec.type.isEnumOrRecord()) {
            //TODO: check that there was a name token
            return true;
        }

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
                    break :fndef;
                }
            },
        }

        if (p.inFunc)
            try p.err(.func_not_in_root);

        const inFunction = p.inFunc;
        p.inFunc = true;
        defer p.inFunc = inFunction;

        // collect old style parameters
        if (initD.d.oldTypeFunc != null) {
            const paramBufferTop = p.paramBuffer.items.len;
            const scopeTop = p.scopes.items.len;

            defer {
                p.paramBuffer.items.len = paramBufferTop;
                p.scopes.items.len = scopeTop;
            }

            initD.d.type.specifier = .Func;
            paramLoop: while (true) {
                const paramDeclSpec = (try p.declSpecifier()) orelse break;
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

                    if (d.type.isFunc()) {
                        const elemType = try p.arena.create(Type);
                        elemType.* = d.type;
                        d.type = Type{
                            .specifier = .Pointer,
                            .data = .{ .subType = elemType },
                        };
                    } else if (d.type.isArray()) {
                        // param declared as arrays are converted to pointers
                        const elemType = try p.arena.create(Type);
                        elemType.* = d.type.getElemType();
                        d.type = Type{
                            .specifier = .Pointer,
                            .data = .{ .subType = elemType },
                        };
                    } else if (d.type.specifier == .Void) {
                        try p.errToken(.invalid_void_param, d.name);
                    }

                    // find and correct parameter types
                    // TODO check for missing declaration and redefinition
                    const name = p.tokSlice(d.name);
                    for (initD.d.type.data.func.params) |*param| {
                        if (std.mem.eql(u8, param.name, name)) {
                            param.ty = d.type;
                            break;
                        }
                    } else {
                        try p.errStr(.parameter_missing, d.name, name);
                    }

                    try p.scopes.append(.{ .symbol = .{ .name = name, .nameToken = d.name, .type = d.type } });
                    if (p.eat(.Comma) == null) break;
                }

                _ = try p.expectToken(.Semicolon);
            }
        }

        for (initD.d.type.data.func.params) |param| {
            try p.scopes.append(.{
                .symbol = .{
                    .name = param.name,
                    .type = param.ty,
                    .nameToken = 0, // TODO split Scope.Symbol into Scope.Typedef
                },
            });
        }

        const node = try p.addNode(.{
            .type = initD.d.type,
            .tag = try declSpec.validateFnDef(p),
            .data = .{ .Declaration = .{ .name = initD.d.name } },
        });
        try p.scopes.append(.{ .symbol = .{
            .name = p.tokSlice(initD.d.name),
            .type = initD.d.type,
            .nameToken = initD.d.name,
        } });

        const body = try p.parseCompoundStmt();
        p.nodes.items(.data)[@intFromEnum(node)].Declaration.node = body.?;

        // check gotos
        if (inFunction) {
            for (p.labels.items) |item| {
                if (item == .unresolvedGoto)
                    try p.errStr(.undeclared_label, item.unresolvedGoto, p.tokSlice(item.unresolvedGoto));

                p.labels.items.len = 0;
                p.labelCount = 0;
            }
        }

        try p.declBuffer.append(node);
        return true;
    }

    // Declare all variable/typedef declarators.
    while (true) {
        if (initD.d.oldTypeFunc) |tokenIdx|
            try p.errToken(.invalid_old_style_params, tokenIdx);

        const node = try p.addNode(.{
            .type = initD.d.type,
            .tag = try declSpec.validate(p, initD.d.type, initD.initializer != .none),
            .data = .{ .Declaration = .{ .name = initD.d.name, .node = initD.initializer } },
        });
        try p.declBuffer.append(node);

        if (declSpec.storageClass == .typedef) {
            try p.scopes.append(.{ .typedef = .{
                .name = p.tokSlice(initD.d.name),
                .type = initD.d.type,
                .nameToken = initD.d.name,
            } });
        } else {
            try p.scopes.append(.{ .symbol = .{
                .name = p.tokSlice(initD.d.name),
                .type = initD.d.type,
                .nameToken = initD.d.name,
            } });
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
    const res = try p.constExpr();

    _ = try p.expectToken(.Comma);
    const str = switch (p.getCurrToken()) {
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
    };

    try p.expectClosing(lp, .RParen);
    _ = try p.expectToken(.Semicolon);

    if (res.value != .unavailable and !res.getBool()) {
        const stringsTop = p.strings.items.len;
        defer p.strings.items.len = stringsTop;

        const data = p.nodes.items(.data)[@intFromEnum(str.node)].String;
        try AST.dumpString(
            p.strings.items[data.index..][0..data.len],
            p.nodes.items(.tag)[@intFromEnum(str.node)],
            p.strings.writer(),
        );

        try p.errStr(
            .static_assert_failure,
            curToken,
            try p.pp.arena.allocator().dupe(u8, p.strings.items[stringsTop..]),
        );
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

/// declSpec: (storageClassSpec | typeSpec | typeQual | funcSpec | alignSpec)+
/// storageClassSpec:
///  : keyword_typedef
///  | keyword_extern
///  | keyword_static
///  | keyword_threadlocal
///  | keyword_auto
///  | keyword_register
fn declSpecifier(p: *Parser) Error!?DeclSpec {
    var d: DeclSpec = .{};
    var spec: TypeBuilder = .{};

    const start = p.index;
    while (true) {
        if (try p.parseTypeSpec(&spec, &d.type))
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

            .KeywordInline => {
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
    try spec.finish(p, &d.type);

    return d;
}

const InitDeclarator = struct { d: Declarator, initializer: NodeIndex = .none };

/// initDeclarator : declarator ('=' initializer)?
fn parseInitDeclarator(p: *Parser, declSpec: *DeclSpec) Error!?InitDeclarator {
    var initD = InitDeclarator{ .d = (try p.declarator(declSpec.type, .normal)) orelse return null };

    if (p.eat(.Equal)) |_| {
        if (declSpec.storageClass == .typedef or declSpec.type.isFunc())
            try p.err(.illegal_initializer);

        if (declSpec.storageClass == .@"extern") {
            try p.err(.extern_initializer);
            declSpec.storageClass = .none;
        }

        const init = try p.initializer(declSpec.type);
        try init.expect(p);
        const casted = try init.coerce(p, initD.d.type);

        initD.initializer = casted.node;
    }

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
/// atomicTypeSpec : keyword_atomic '(' typeName ')'
/// alignSpec
///   : keyword_alignas '(' typeName ')'
///   | keyword_alignas '(' constExpr ')'
fn parseTypeSpec(p: *Parser, ty: *TypeBuilder, completeType: *Type) Error!bool {
    const start = p.index;
    while (true) {
        if (try p.parseTypeQual(completeType)) {
            continue;
        }

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

                if (completeType.qual.atomic)
                    try p.errStr(.duplicate_declspec, atomicToken, "atomic")
                else
                    completeType.qual.atomic = true;

                // TODO check that the type can be atomic
                continue;
            },

            .KeywordEnum => {
                try ty.combine(p, .IncompleteEnum, p.index);
                try p.parseEnumSpec(ty);
                continue;
            },

            .KeywordStruct => {
                try ty.combine(p, .IncompleteStruct, p.index);
                try p.parseRecordSpec(ty);
                continue;
            },

            .KeywordUnion => {
                try ty.combine(p, .IncompleteUnion, p.index);
                try p.parseRecordSpec(ty);
                continue;
            },

            .KeywordAlignas => {
                if (completeType.alignment != 0)
                    try p.errStr(.duplicate_declspec, p.index, "alignment");

                p.index += 1;

                const lp = try p.expectToken(.LParen);
                if (try p.typeName()) |inner_ty| {
                    completeType.alignment = inner_ty.alignment;
                } else {
                    const res = try p.constExpr();
                    // TODO more validation here
                    completeType.alignment = @as(u32, @intCast(res.asU64()));
                }

                try p.expectClosing(lp, .RParen);
                continue;
            },

            .Identifier => {
                const typedef = p.findTypedef(p.tokSlice(p.index)) orelse break;
                const newSpec = TypeBuilder.fromType(typedef.type);

                const errStart = p.pp.compilation.diag.list.items.len;
                ty.combine(p, newSpec, p.index) catch {
                    p.pp.compilation.diag.list.items.len = errStart;
                    break;
                };

                ty.typedef = .{
                    .token = typedef.nameToken,
                    .spec = newSpec.toString(),
                };
            },
            else => break,
        }

        p.index += 1;
    }

    return p.index != start;
}

fn getAnonymousName(p: *Parser, kindToken: TokenIndex) ![]const u8 {
    const loc = p.pp.tokens.items(.loc)[kindToken];
    const source = p.pp.compilation.getSource(loc.id);
    const lcs = source.lineColString(loc.byteOffset);

    const kindStr = switch (p.tokenIds[kindToken]) {
        .KeywordStruct,
        .KeywordUnion,
        .KeywordEnum,
        => p.tokSlice(kindToken),
        else => "record field",
    };

    return std.fmt.allocPrint(
        p.arena,
        "(anonymous {s} at {s}:{d}:{d})",
        .{ kindStr, source.path, lcs.line, lcs.col },
    );
}

/// recordSpec
///  : (keyword_struct | keyword_union) IDENTIFIER? { recordDecl* }
///  | (keyword_struct | keyword_union) IDENTIFIER
fn parseRecordSpec(p: *Parser, tyBuilder: *TypeBuilder) Error!void {
    const kindToken = p.index;
    const isStruct = p.tokenIds[kindToken] == .KeywordStruct;
    p.index += 1;
    const maybeIdent = p.eat(.Identifier);
    const lb = p.eat(.LBrace) orelse {
        const ident = maybeIdent orelse {
            try p.err(.ident_or_l_brace);
            return error.ParsingFailed;
        };

        // check if this is a referense to a previous type
        if (try p.findTag(p.tokenIds[kindToken], ident)) |prev| {
            if (prev.type.specifier == .Union) {
                tyBuilder.kind = .{ .Union = prev.type.data.record };
            } else if (prev.type.specifier == .Struct) {
                tyBuilder.kind = .{ .Struct = prev.type.data.record };
            }
        } else {
            // this is a forward declaration
        }

        return;
    };

    // check if this is a redefinition
    if (maybeIdent) |ident| {
        if (try p.findTag(p.tokenIds[kindToken], ident)) |prev| {
            try p.errStr(.redefinition, ident, p.tokSlice(ident));
            try p.errToken(.previous_definition, prev.nameToken);
        }
    }

    // declare a symbol for the type
    const recordName = if (maybeIdent) |ident| p.tokSlice(ident) else try p.getAnonymousName(kindToken);
    var symIndex: ?usize = null;
    if (maybeIdent) |ident| {
        const sym = Scope.Symbol{
            .name = recordName,
            .type = .{ .specifier = if (isStruct) .IncompleteStruct else .IncompleteUnion },
            .nameToken = ident,
        };
        symIndex = p.scopes.items.len;
        try p.scopes.append(if (isStruct) .{ .@"struct" = sym } else .{ .@"union" = sym });
    }

    // reserve space for this record
    try p.declBuffer.append(.none);
    const declBufferTop = p.declBuffer.items.len;
    const recordBufferTop = p.recordBuffer.items.len;

    defer {
        p.declBuffer.items.len = declBufferTop;
        p.recordBuffer.items.len = recordBufferTop;
    }

    try p.recordDecls();

    if (p.recordBuffer.items.len == recordBufferTop)
        try p.errStr(.empty_record, kindToken, p.tokSlice(kindToken));

    try p.expectClosing(lb, .RBrace);

    // create the type
    const recordType = try p.arena.create(Type.Record);
    recordType.* = .{
        .name = recordName,
        .size = 0, // TODO calculate
        .alignment = 0, // TODO calculate
        .fields = try p.arena.dupe(Type.Record.Field, p.recordBuffer.items[recordBufferTop..]),
    };
    tyBuilder.kind = if (isStruct) .{ .Struct = recordType } else .{ .Union = recordType };
    const ty = Type{
        .specifier = if (isStruct) .Struct else .Union,
        .data = .{ .record = recordType },
    };

    if (symIndex) |index|
        if (isStruct) {
            p.scopes.items[index].@"struct".type = ty;
        } else {
            p.scopes.items[index].@"union".type = ty;
        };

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
}
/// recordDecl
///  : specQual+ (recordDeclarator (',' recordDeclarator)*)? ;
///  | staticAssert
/// recordDeclarator : declarator (':' constExpr)?
fn recordDecls(p: *Parser) Error!void {
    while (true) {
        if (try p.parseStaticAssert()) continue;
        const baseType = (try p.specQual()) orelse return;

        while (true) {
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
            if (p.eat(.Colon)) |_| {
                const res = try p.constExpr();
                // TODO check using math.cast
                switch (res.value) {
                    .unsigned => |v| bits = @as(u32, @intCast(v)),
                    .signed => |v| bits = @as(u32, @intCast(v)),
                    .unavailable => unreachable,
                }
                bitsNode = res.node;
            }
            if (nameToken == 0 and bitsNode == .none) {
                try p.err(.missing_declaration);
            } else {
                try p.recordBuffer.append(.{
                    .name = if (nameToken != 0) p.tokSlice(nameToken) else try p.getAnonymousName(firstToken),
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
    }
}

// specQual : typeSpec | typeQual | alignSpec
fn specQual(p: *Parser) Error!?Type {
    var spec: TypeBuilder = .{};
    var ty: Type = .{ .specifier = undefined };

    if (try p.parseTypeSpec(&spec, &ty)) {
        try spec.finish(p, &ty);
        return ty;
    }

    return null;
}

/// enumSpec
///  : keyword_enum IDENTIFIER? { enumerator (',' enumerator)? ',') }
///  | keyword_enum IDENTIFIER
fn parseEnumSpec(p: *Parser, tyBuilder: *TypeBuilder) Error!void {
    const enumTK = p.index;
    p.index += 1;
    const maybeID = p.eat(.Identifier);
    const lb = p.eat(.LBrace) orelse {
        const ident = maybeID orelse {
            try p.err(.ident_or_l_brace);
            return error.ParsingFailed;
        };

        // check if this is a referense to a previous type
        if (try p.findTag(.KeywordEnum, ident)) |prev| {
            if (prev.type.specifier == .Enum) {
                tyBuilder.kind = .{ .Enum = prev.type.data.@"enum" };
            }
        } else {
            //forward declaration
        }

        return;
    };

    // check if this is a redefinition.
    if (maybeID) |ident| {
        if (try p.findTag(.KeywordEnum, ident)) |prev| {
            try p.errStr(.redefinition, ident, p.tokSlice(ident));
            try p.errToken(.previous_definition, prev.nameToken);
        }
    }

    // Create Type
    const enumType = try p.arena.create(Type.Enum);
    enumType.* = .{
        .name = if (maybeID) |ident| p.tokSlice(ident) else try p.getAnonymousName(enumTK),
        .tagType = .{ .specifier = .Void }, // void means incomplete
        .fields = &.{},
    };

    tyBuilder.kind = .{ .Enum = enumType };
    const ty = Type{
        .specifier = .Enum,
        .data = .{ .@"enum" = enumType },
    };

    if (maybeID) |ident| {
        try p.scopes.append(.{ .@"enum" = .{
            .name = enumType.name,
            .type = ty,
            .nameToken = ident,
        } });
    }

    // reserve space for this enum
    try p.declBuffer.append(.none);
    const declBufferTop = p.declBuffer.items.len;
    const listBufferTop = p.listBuffer.items.len;
    const enumBufferTop = p.enumBuffer.items.len;

    defer {
        p.declBuffer.items.len = declBufferTop;
        p.listBuffer.items.len = listBufferTop;
        p.enumBuffer.items.len = enumBufferTop;
    }

    while (try p.enumerator()) |fieldAndNode| {
        try p.enumBuffer.append(fieldAndNode.field);
        try p.listBuffer.append(fieldAndNode.node);
        if (p.eat(.Comma) == null) break;
    }

    if (p.enumBuffer.items.len == enumBufferTop)
        try p.err(.empty_enum);

    try p.expectClosing(lb, .RBrace);
    enumType.fields = try p.arena.dupe(Type.Enum.Field, p.enumBuffer.items[enumBufferTop..]);

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
}

const EnumFieldAndNode = struct { field: Type.Enum.Field, node: NodeIndex };
/// enumerator : IDENTIFIER ('=' constExpr)
fn enumerator(p: *Parser) Error!?EnumFieldAndNode {
    const nameToken = p.eat(.Identifier) orelse {
        if (p.getCurrToken() == .RBrace) return null;
        try p.err(.expected_identifier);
        // TODO skip to }
        return error.ParsingFailed;
    };

    const name = p.tokSlice(nameToken);
    var res: Result = .{
        .ty = .{ .specifier = .Int },
        .value = .{
            .unsigned = 0,
        },
    };

    if (p.eat(.Equal)) |_| {
        res = try p.constExpr();
    }

    try p.scopes.append(.{ .enumeration = .{
        .name = name,
        .value = res,
    } });

    return EnumFieldAndNode{
        .field = .{
            .name = name,
            .ty = res.ty,
            .value = res.asU64(),
        },
        .node = try p.addNode(
            .{
                .tag = .EnumFieldDecl,
                .type = res.ty,
                .data = .{
                    .Declaration = .{
                        .name = nameToken,
                        .node = res.node,
                    },
                },
            },
        ),
    };
}
/// atomicTypeSpec : keyword_atomic '(' typeName ')'
/// typeQual : keyword_const | keyword_restrict | keyword_volatile | keyword_atomic
fn parseTypeQual(p: *Parser, ty: *Type) Error!bool {
    var any = false;

    while (true) {
        switch (p.getCurrToken()) {
            .KeywordRestrict,
            .KeywordGccRestrict1,
            .KeywordGccRestrict2,
            => {
                if (ty.specifier != .Pointer)
                    try p.errExtra(
                        .restrict_non_pointer,
                        p.index,
                        .{ .str = TypeBuilder.fromType(ty.*).toString() },
                    )
                else if (ty.qual.restrict)
                    try p.errStr(.duplicate_declspec, p.index, "restrict")
                else
                    ty.qual.restrict = true;
            },

            .KeywordConst,
            .KeywordGccConst1,
            .KeywordGccConst2,
            => {
                if (ty.qual.@"const")
                    try p.errStr(.duplicate_declspec, p.index, "const")
                else
                    ty.qual.@"const" = true;
            },

            .KeywordVolatile,
            .KeywordGccVolatile1,
            .KeywordGccVolatile2,
            => {
                if (ty.qual.@"volatile")
                    try p.errStr(.duplicate_declspec, p.index, "volatile")
                else
                    ty.qual.@"volatile" = true;
            },

            .KeywordAtomic => {
                // _Atomic(typeName) instead of just _Atomic
                if (p.tokenIds[p.index + 1] == .LParen) break;
                if (ty.qual.atomic)
                    try p.errStr(.duplicate_declspec, p.index, "atomic")
                else
                    ty.qual.atomic = true;
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

    if (kind != .abstract and p.getCurrToken() == .Identifier) {
        d.name = p.index;
        p.index += 1;
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
        var gotQuals = try p.parseTypeQual(&resType);
        var static = p.eat(.KeywordStatic);

        if (static != null and !gotQuals)
            gotQuals = try p.parseTypeQual(&resType);

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
            resType.qual = .{};
            star = null;
        }

        if (static) |_|
            try size.expect(p);

        switch (size.value) {
            .unavailable => if (size.node != .none) {
                if (!p.inFunc and kind != .param)
                    try p.errToken(.variable_len_array_file_scope, lb);

                const vlaType = try p.arena.create(Type.VLA);
                vlaType.expr = size.node;
                resType.data = .{ .vla = vlaType };
                resType.specifier = .VariableLenArray;

                if (static) |some|
                    try p.errToken(.useless_static, some);
            } else if (star) |_| {
                const elemType = try p.arena.create(Type);
                resType.data = .{ .subType = elemType };
                resType.specifier = .UnspecifiedVariableLenArray;
            } else {
                const arrayType = try p.arena.create(Type.Array);
                arrayType.len = 0;
                resType.data = .{ .array = arrayType };
                resType.specifier = .IncompleteArray;
            },

            .unsigned => |v| {
                const arrayType = try p.arena.create(Type.Array);
                arrayType.len = v;
                resType.data = .{ .array = arrayType };
                resType.specifier = .Array;
            },

            .signed => |v| {
                if (v < 0)
                    try p.errToken(.negative_array_size, lb);

                const arrayType = try p.arena.create(Type.Array);
                arrayType.len = @as(u64, @bitCast(v));
                resType.data = .{ .array = arrayType };
                resType.specifier = .Array;
            },
        }

        const outer = try p.directDeclarator(baseType, d, kind);
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
        var specifier: Type.Specifier = .Func;

        if (try p.paramDecls()) |params| {
            funcType.params = params;

            if (p.eat(.Ellipsis)) |_|
                specifier = .VarArgsFunc;
        } else if (p.getCurrToken() == .RParen) {
            specifier = .OldStyleFunc;
        } else if (p.getCurrToken() == .Identifier) {
            d.oldTypeFunc = p.index;

            const paramBufferTop = p.paramBuffer.items.len;
            defer p.paramBuffer.items.len = paramBufferTop;

            specifier = .OldStyleFunc;
            while (true) {
                try p.paramBuffer.append(.{
                    .name = p.tokSlice(try p.expectToken(.Identifier)),
                    .ty = .{ .specifier = .Int },
                    .register = false,
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
        _ = try p.parseTypeQual(&ty);
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

    defer p.paramBuffer.items.len = paramBufferTop;
    while (true) {
        const paramDeclSpec = if (try p.declSpecifier()) |some|
            some
        else if (p.paramBuffer.items.len == paramBufferTop)
            return null
        else blk: {
            var d: DeclSpec = .{};
            var spec: TypeBuilder = .{};

            try spec.finish(p, &d.type);

            break :blk d;
        };

        var nameToken: TokenIndex = 0;
        var paramType = paramDeclSpec.type;
        if (try p.declarator(paramDeclSpec.type, .param)) |some| {
            if (some.oldTypeFunc) |tokenIdx|
                try p.errToken(.invalid_old_style_params, tokenIdx);

            nameToken = some.name;
            paramType = some.type;
            if (some.name != 0)
                try p.scopes.append(.{ .symbol = .{
                    .name = p.tokSlice(some.name),
                    .type = some.type,
                    .nameToken = some.name,
                } });
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
            const elemType = try p.arena.create(Type);
            elemType.* = paramType.getElemType();
            paramType = Type{ .specifier = .Pointer, .data = .{ .subType = elemType } };
        } else if (paramType.specifier == .Void) {
            // validate void parameters
            if (p.paramBuffer.items.len == paramBufferTop) {
                if (p.getCurrToken() != .RParen) {
                    try p.err(.void_only_param);
                    if (paramType.qual.any())
                        try p.err(.void_param_qualified);

                    return error.ParsingFailed;
                }

                return &[0]Type.Function.Param{};
            }

            try p.err(.void_must_be_first_param);

            return error.ParsingFailed;
        }

        try paramDeclSpec.validateParam(p, paramType);

        try p.paramBuffer.append(.{
            .name = if (nameToken == 0) "" else p.tokSlice(nameToken),
            .ty = paramType,
            .register = paramDeclSpec.storageClass == .register,
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
    var ty = (try p.specQual()) orelse return null;
    if (try p.declarator(ty, .abstract)) |some| {
        if (some.oldTypeFunc) |tokenIdx|
            try p.errToken(.invalid_old_style_params, tokenIdx);

        return some.type;
    } else return ty;
}

/// initializer
///  : assignExpr
///  | '{' initializerItems '}'
fn initializer(p: *Parser, parenType: Type) Error!Result {
    if (p.eat(.LBrace)) |lb| {
        const listBufferTop = p.listBuffer.items.len;
        defer p.listBuffer.items.len = listBufferTop;

        while (try p.parseInitializerItems(parenType)) |item| {
            try p.listBuffer.append(item);
            if (p.eat(.Comma) == null) break;
        }
        try p.expectClosing(lb, .RBrace);

        var node: AST.Node = .{
            .tag = .CompoundInitializerExprTwo,
            .type = parenType,
            .data = .{ .BinaryExpr = .{ .lhs = .none, .rhs = .none } },
        };

        const initializers = p.listBuffer.items[listBufferTop..];
        switch (initializers.len) {
            0 => {},
            1 => node.data = .{ .BinaryExpr = .{ .lhs = initializers[0], .rhs = .none } },
            2 => node.data = .{ .BinaryExpr = .{ .lhs = initializers[0], .rhs = initializers[1] } },
            else => {
                node.tag = .CompoundInitializerExpr;
                node.data = .{ .range = try p.addList(initializers) };
            },
        }

        return Result{ .node = try p.addNode(node), .ty = parenType };
    }

    return p.assignExpr();
}
/// initializerItems : designation? initializer (',' designation? initializer)* ','?
/// designation : designator+ '='
/// designator
///  : '[' constExpr ']'
///  | '.' identifier
fn parseInitializerItems(p: *Parser, parenType: Type) Error!?NodeIndex {
    var currType = parenType;
    var designation: NodeIndex = .none;
    while (true) {
        if (p.eat(.LBracket)) |lb| {
            const res = try p.constExpr();
            try p.expectClosing(lb, .RBracket);
            designation = try p.addNode(.{
                .tag = .ArrayDesignatorExpr,
                // TODO do type checking
                .data = .{ .BinaryExpr = .{
                    .lhs = designation,
                    .rhs = res.node,
                } },
            });
        } else if (p.eat(.Period)) |_| {
            const identifier = try p.expectToken(.Identifier);
            designation = try p.addNode(.{
                .tag = .MemberDesignatorExpr,
                .data = .{
                    .Member = .{
                        .lhs = designation,
                        // TODO do type checking
                        .name = identifier,
                    },
                },
            });
        } else break;
    }
    if (designation != .none) _ = try p.expectToken(.Equal);

    const initRes = try p.initializer(currType);
    if (designation != .none) {
        try initRes.expect(p);
    } else if (initRes.node == .none) {
        return null;
    }
    return try p.addNode(.{
        .tag = .InitializerItemExpr,
        // TODO do type checking
        .data = .{ .BinaryExpr = .{
            .lhs = designation,
            .rhs = initRes.node,
        } },
    });
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
///  | keyword_goto IDENTIFIER ';'
///  | keyword_continue ';'
///  | keyword_break ';'
///  | keyword_return expr? ';'
///  | expr? ';'
fn stmt(p: *Parser) Error!NodeIndex {
    if (try p.labeledStmt()) |some|
        return some;

    if (try p.parseCompoundStmt()) |some|
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

    if (p.eat(.KeywordGoto)) |_| {
        const nameToken = try p.expectToken(.Identifier);
        const str = p.tokSlice(nameToken);

        if (p.findLabel(str) == null)
            try p.labels.append(.{ .unresolvedGoto = nameToken });

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

    if (p.eat(.KeywordReturn)) |_| {
        const e = try p.expr();
        _ = try p.expectToken(.Semicolon);

        return try p.addNode(.{ .tag = .ReturnStmt, .data = .{ .UnaryExpr = e.node } });
    }

    const exprStart = p.index;
    const e = try p.expr();
    if (e.node != .none) {
        _ = try p.expectToken(.Semicolon);
        try p.maybeWarnUnused(e.node, exprStart);
        return e.node;
    }

    if (p.eat(.Semicolon)) |_|
        return .none;

    try p.err(.expected_stmt);
    return error.ParsingFailed;
}

fn parseIfStmt(p: *Parser) Error!NodeIndex {
    const startScopeLen = p.scopes.items.len;
    defer p.scopes.items.len = startScopeLen;

    const lp = try p.expectToken(.LParen);
    const cond = try p.expr();

    try cond.expect(p);

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
    const init = if (!gotDecl) try p.expr() else Result{};
    try p.maybeWarnUnused(init.node, initStart);

    if (!gotDecl)
        _ = try p.expectToken(.Semicolon);

    // cond
    const cond = try p.expr();
    _ = try p.expectToken(.Semicolon);

    // increment
    const incrStart = p.index;
    const incr = try p.expr();
    try p.maybeWarnUnused(incr.node, incrStart);
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
            .data = .{ .range = try p.addList(&.{ init.node, cond.node, incr.node }) },
        });
    }
}

fn parseWhileStmt(p: *Parser) Error!NodeIndex {
    const startScopeLen = p.scopes.items.len;
    defer p.scopes.items.len = startScopeLen;

    const lp = try p.expectToken(.LParen);
    const cond = try p.expr();

    try cond.expect(p);
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
    const cond = try p.expr();

    try cond.expect(p);
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
    const cond = try p.expr();

    try cond.expect(p);
    try p.expectClosing(lp, .RParen);

    var switchScope = Scope.Switch{
        .cases = Scope.Switch.CaseMap.init(p.pp.compilation.gpa),
    };
    defer switchScope.cases.deinit();

    try p.scopes.append(.{ .@"switch" = &switchScope });
    const body = try p.stmt();

    return try p.addNode(.{
        .tag = .SwitchStmt,
        .data = .{ .BinaryExpr = .{ .rhs = cond.node, .lhs = body } },
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

fn maybeWarnUnused(p: *Parser, node: NodeIndex, exprStart: TokenIndex) Error!void {
    switch (p.nodes.items(.tag)[@intFromEnum(node)]) {
        .Invalid,
        .AssignExpr,
        .MulAssignExpr,
        .DivAssignExpr,
        .ModAssignExpr,
        .AddAssignExpr,
        .SubAssignExpr,
        .ShlAssignExpr,
        .ShrAssignExpr,
        .BitAndAssignExpr,
        .BitXorAssignExpr,
        .BitOrAssignExpr, //
        .CallExprOne,
        => return,

        .CastExpr => if (p.nodes.items(.type)[@intFromEnum(node)].specifier == .Void) return,
        else => {},
    }
    try p.errToken(.unused_value, exprStart);
}

/// labeledStmt
/// : IDENTIFIER ':' stmt
/// | keyword_case constExpr ':' stmt
/// | keyword_default ':' stmt
fn labeledStmt(p: *Parser) Error!?NodeIndex {
    if (p.getCurrToken() == .Identifier and p.lookAhead(1) == .Colon) {
        const nameToken = p.index;
        const str = p.tokSlice(nameToken);
        if (p.findLabel(str)) |some| {
            try p.errStr(.duplicate_label, nameToken, str);
            try p.errStr(.previous_label, some, str);
        } else {
            p.labelCount += 1;
            try p.labels.append(.{ .label = nameToken });

            var i: usize = 0;
            while (i < p.labels.items.len) : (i += 1) {
                if (p.labels.items[i] == .unresolvedGoto and std.mem.eql(u8, p.tokSlice(p.labels.items[i].unresolvedGoto), str))
                    _ = p.labels.swapRemove(i);
            }
        }

        p.index += 2;

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

/// compoundStmt : '{' ( decl | staticAssert |stmt)* '}'
fn parseCompoundStmt(p: *Parser) Error!?NodeIndex {
    const lBrace = p.eat(.LBrace) orelse return null;

    const declBufferTop = p.declBuffer.items.len;
    const scopeTop = p.scopes.items.len;

    defer {
        p.declBuffer.items.len = declBufferTop;
        p.scopes.items.len = scopeTop;
    }

    try p.scopes.append(.block);

    var noreturnIdx: ?TokenIndex = null;
    var noreturnLabelCount: u32 = 0;

    while (p.eat(.RBrace) == null) {
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
    }

    if (noreturnIdx) |some| {
        if (noreturnLabelCount == p.labelCount and some != p.index - 1)
            try p.errToken(.unreachable_code, some);
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

fn nodeIsNoreturn(p: *Parser, node: NodeIndex) bool {
    switch (p.nodes.items(.tag)[@intFromEnum(node)]) {
        .BreakStmt, .ContinueStmt, .ReturnStmt => return true,
        .IfThenElseStmt => {
            const data = p.data.items[p.nodes.items(.data)[@intFromEnum(node)].If3.body..];
            return p.nodeIsNoreturn(data[0]) and p.nodeIsNoreturn(data[1]);
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
            => if (parens == 0) return,
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
    return res.getBool();
}

/// expr : assignExpr (',' assignExpr)*
fn expr(p: *Parser) Error!Result {
    var exprStartIdx = p.index;
    var lhs = try p.assignExpr();
    while (p.eat(.Comma)) |_| {
        try p.maybeWarnUnused(lhs.node, exprStartIdx);
        exprStartIdx = p.index;

        const rhs = try p.assignExpr();
        lhs.value = rhs.value;
        try lhs.bin(p, .CommaExpr, rhs);
    }

    return lhs;
}

/// assignExpr
///  : conditionalExpr
///  | unaryExpr ('=' | '*=' | '/=' | '%=' | '+=' | '-=' | '<<=' | '>>=' | '&=' | '^=' | '|=') assignExpr
fn assignExpr(p: *Parser) Error!Result {
    var lhs = try p.conditionalExpr();

    const index = p.index;
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

    if (bitOr == null)
        return lhs;

    if (!AST.isLValue(p.nodes.slice(), lhs.node)) {
        try p.errToken(.not_assignable, index);
        return error.ParsingFailed;
    }

    var rhs = try p.assignExpr();
    try lhs.bin(p, if (eq != null)
        .AssignExpr
    else if (mul != null)
        AstTag.MulAssignExpr
    else if (div != null)
        AstTag.DivAssignExpr
    else if (mod != null)
        AstTag.ModAssignExpr
    else if (add != null)
        AstTag.AddAssignExpr
    else if (sub != null)
        AstTag.SubAssignExpr
    else if (shl != null)
        AstTag.ShlAssignExpr
    else if (shr != null)
        AstTag.ShrAssignExpr
    else if (bitAnd != null)
        AstTag.BitAndAssignExpr
    else if (bitXor != null)
        AstTag.BitXorAssignExpr
    else
        AstTag.BitOrAssignExpr, rhs);

    return lhs;
}

/// constExpr : conditionalExpr
fn constExpr(p: *Parser) Error!Result {
    const res = try p.conditionalExpr();
    try res.expect(p);

    return res;
}

/// conditionalExpr : logicalOrExpr ('?' expression? ':' conditionalExpr)?
fn conditionalExpr(p: *Parser) Error!Result {
    var cond = try p.logicalOrExpr();
    if (p.eat(.QuestionMark) == null)
        return cond;
    const savedEval = p.noEval;

    // Depending on the value of the condition, avoid  evaluating unreachable
    const thenExpr = blk: {
        defer p.noEval = savedEval;
        if (cond.value != .unavailable and !cond.getBool())
            p.noEval = true;

        break :blk try p.expr();
    };

    _ = try p.expectToken(.Colon);

    const elseExpr = blk: {
        defer p.noEval = savedEval;
        if (cond.value != .unavailable and cond.getBool())
            p.noEval = true;

        break :blk try p.conditionalExpr();
    };

    if (cond.value != .unavailable)
        cond.value = if (cond.getBool()) thenExpr.value else elseExpr.value;

    cond.node = try p.addNode(.{
        .tag = .CondExpr,
        .type = thenExpr.ty,
        .data = .{ .If3 = .{ .cond = cond.node, .body = (try p.addList(&.{ thenExpr.node, elseExpr.node })).start } },
    });

    return cond;
}

/// logicalOrExpr : logicalAndExpr ('||' logicalAndExpr)*
fn logicalOrExpr(p: *Parser) Error!Result {
    var lhs = try p.logicalAndExpr();
    const savedEval = p.noEval;
    defer p.noEval = savedEval;

    while (p.eat(.PipePipe)) |_| {
        if (lhs.value != .unavailable and lhs.getBool())
            p.noEval = true;

        const rhs = try p.logicalAndExpr();
        if (lhs.value != .unavailable and rhs.value != .unavailable) {
            lhs.value = .{ .signed = @intFromBool(lhs.getBool() or rhs.getBool()) };
        }

        lhs.ty = .{ .specifier = .Int };
        try lhs.bin(p, .BoolOrExpr, rhs);
    }

    return lhs;
}

/// logicalAndExpr : orExpr ('&&' orExpr)*
fn logicalAndExpr(p: *Parser) Error!Result {
    var lhs = try p.orExpr();
    const savedEval = p.noEval;
    defer p.noEval = savedEval;

    while (p.eat(.AmpersandAmpersand)) |_| {
        if (lhs.value != .unavailable and lhs.getBool())
            p.noEval = true;

        const rhs = try p.orExpr();
        if (lhs.value != .unavailable and rhs.value != .unavailable) {
            lhs.value = .{ .signed = @intFromBool(lhs.getBool() or rhs.getBool()) };
        }

        lhs.ty = .{ .specifier = .Int };
        try lhs.bin(p, .BoolAndExpr, rhs);
    }
    return lhs;
}

/// orExpr : xorExpr ('|' xorExpr)*
fn orExpr(p: *Parser) Error!Result {
    var lhs = try p.xorExpr();
    while (p.eat(.Pipe)) |_| {
        var rhs = try p.xorExpr();

        if (try lhs.adjustTypes(&rhs, p)) {
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
    while (p.eat(.Caret)) |_| {
        var rhs = try p.andExpr();

        if (try lhs.adjustTypes(&rhs, p)) {
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
    while (p.eat(.Ampersand)) |_| {
        var rhs = try p.eqExpr();

        if (try lhs.adjustTypes(&rhs, p)) {
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
    while (true) {
        const eq = p.eat(.EqualEqual);
        const ne = eq orelse p.eat(.BangEqual);

        if (ne == null) break;
        var rhs = try p.compExpr();

        if (try lhs.adjustTypes(&rhs, p)) {
            const res = if (eq != null)
                lhs.compare(.eq, rhs)
            else
                lhs.compare(.neq, rhs);

            lhs.value = .{ .signed = @intFromBool(res) };
        }

        lhs.ty = .{ .specifier = .Int };
        try lhs.bin(p, if (eq != null) .EqualExpr else .NotEqualExpr, rhs);
    }
    return lhs;
}

/// compExpr : shiftExpr (('<' | '<=' | '>' | '>=') shiftExpr)*
fn compExpr(p: *Parser) Error!Result {
    var lhs = try p.shiftExpr();
    while (true) {
        const lt = p.eat(.AngleBracketLeft);
        const le = lt orelse p.eat(.AngleBracketLeftEqual);
        const gt = le orelse p.eat(.AngleBracketRight);
        const ge = gt orelse p.eat(.AngleBracketRightEqual);
        if (ge == null) break;
        var rhs = try p.shiftExpr();

        if (try lhs.adjustTypes(&rhs, p)) {
            const res = if (lt != null)
                lhs.compare(.lt, rhs)
            else if (le != null)
                lhs.compare(.lte, rhs)
            else if (gt != null)
                lhs.compare(.gt, rhs)
            else
                lhs.compare(.gte, rhs);
            lhs.value = .{ .signed = @intFromBool(res) };
        }

        lhs.ty = .{ .specifier = .Int };
        try lhs.bin(p, if (lt != null)
            .LessThanExpr
        else if (le != null)
            AstTag.LessThanEqualExpr
        else if (gt != null)
            AstTag.GreaterThanExpr
        else
            AstTag.GreaterThanEqualExpr, rhs);
    }

    return lhs;
}

/// shiftExpr : addExpr (('<<' | '>>') addExpr)*
fn shiftExpr(p: *Parser) Error!Result {
    var lhs = try p.addExpr();
    while (true) {
        const shl = p.eat(.AngleBracketAngleBracketLeft);
        const shr = shl orelse p.eat(.AngleBracketAngleBracketRight);
        if (shr == null) break;
        var rhs = try p.addExpr();

        if (try lhs.adjustTypes(&rhs, p)) {
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

        try lhs.bin(p, if (shl != null) .ShlExpr else .ShrExpr, rhs);
    }
    return lhs;
}

/// addExpr : mulExpr (('+' | '-') mulExpr)*
fn addExpr(p: *Parser) Error!Result {
    var lhs = try p.mulExpr();
    while (true) {
        const plus = p.eat(.Plus);
        const minus = plus orelse p.eat(.Minus);
        if (minus == null) break;
        var rhs = try p.mulExpr();

        if (try lhs.adjustTypes(&rhs, p)) {
            if (plus != null) {
                try lhs.add(plus.?, rhs, p);
            } else {
                try lhs.sub(minus.?, rhs, p);
            }
        }

        try lhs.bin(p, if (plus != null) .AddExpr else .SubExpr, rhs);
    }
    return lhs;
}

/// mulExpr : castExpr (('*' | '/' | '%') castExpr)*´
fn mulExpr(p: *Parser) Error!Result {
    var lhs = try p.parseCastExpr();
    while (true) {
        const mul = p.eat(.Asterisk);
        const div = mul orelse p.eat(.Slash);
        const percent = div orelse p.eat(.Percent);
        if (percent == null) break;
        var rhs = try p.parseCastExpr();

        if (try lhs.adjustTypes(&rhs, p)) {
            // TODO divide by 0
            if (mul != null) {
                try lhs.mul(mul.?, rhs, p);
            } else if (div != null) {
                lhs.value = switch (lhs.value) {
                    .unsigned => |v| .{ .unsigned = v / rhs.value.unsigned },
                    .signed => |v| .{ .signed = @divFloor(v, rhs.value.signed) },
                    else => unreachable,
                };
            } else {
                lhs.value = switch (lhs.value) {
                    .unsigned => |v| .{ .unsigned = v % rhs.value.unsigned },
                    .signed => |v| .{ .signed = @rem(v, rhs.value.signed) },
                    else => unreachable,
                };
            }
        }

        try lhs.bin(p, if (mul != null)
            .MulExpr
        else if (div != null)
            AstTag.DivExpr
        else
            AstTag.ModExpr, rhs);
    }
    return lhs;
}

/// castExpr :  ( '(' type_name ')' )
///  :  '(' typeName ')' castExpr
///  | '(' typeName ')' '{' initializerItems '}'
///  | unExpr
fn parseCastExpr(p: *Parser) Error!Result {
    if (p.eat(.LParen)) |lp| {
        if (try p.typeName()) |ty| {
            try p.expectClosing(lp, .RParen);

            if (p.eat(.LBrace)) |lb| {
                const listBufferTop = p.listBuffer.items.len;
                defer p.listBuffer.items.len = listBufferTop;

                while (try p.parseInitializerItems(ty)) |item| {
                    try p.listBuffer.append(item);
                    if (p.eat(.Comma) == null) break;
                }
                try p.expectClosing(lb, .RBrace);

                var node: AST.Node = .{
                    .tag = .CompoundLiteralExprTwo,
                    .type = ty,
                    .data = .{ .BinaryExpr = .{ .lhs = .none, .rhs = .none } },
                };

                const initializers = p.listBuffer.items[listBufferTop..];
                switch (initializers.len) {
                    0 => {},
                    1 => node.data = .{ .BinaryExpr = .{ .lhs = initializers[0], .rhs = .none } },
                    2 => node.data = .{ .BinaryExpr = .{ .lhs = initializers[0], .rhs = initializers[1] } },
                    else => {
                        node.tag = .CompoundLiteralExpr;
                        node.data = .{ .range = try p.addList(initializers) };
                    },
                }
                return Result{ .node = try p.addNode(node), .ty = ty };
            }
            var operand = try p.parseCastExpr();
            operand.ty = ty;
            return operand.un(p, .CastExpr);
        }
        p.index -= 1;
    }

    return p.parseUnaryExpr();
}

/// unaryExpr
///  : primaryExpr suffixExpr*
///  | ('&' | '*' | '+' | '-' | '~' | '!' | '++' | '--') castExpr
///  | keyword_sizeof unaryExpr
///  | keyword_sizeof '(' type_name ')'
///  | keyword_alignof '(' type_name ')'
fn parseUnaryExpr(p: *Parser) Error!Result {
    const index = p.index;
    switch (p.tokenIds[index]) {
        .Ampersand => {
            p.index += 1;

            var operand = try p.parseCastExpr();
            if (!AST.isLValue(p.nodes.slice(), operand.node)) {
                try p.errToken(.addr_of_rvalue, index);
                return error.ParsingFailed;
            }

            const elemType = try p.arena.create(Type);
            elemType.* = operand.ty;
            operand.ty = Type{
                .specifier = .Pointer,
                .data = .{ .subType = elemType },
            };

            return operand.un(p, .AddrOfExpr);
        },

        .Asterisk => {
            p.index += 1;
            var operand = try p.parseCastExpr();

            switch (operand.ty.specifier) {
                .Pointer => {
                    operand.ty = operand.ty.data.subType.*;
                },

                .Array, .StaticArray => {
                    operand.ty = operand.ty.data.array.elem;
                },

                .Func, .VarArgsFunc, .OldStyleFunc => {},
                else => {
                    try p.errToken(.indirection_ptr, index);
                    return error.ParsingFailed;
                },
            }
            return operand.un(p, .DerefExpr);
        },

        .Plus => {
            p.index += 1;
            // TODO upcast to int / validate arithmetic type
            return p.parseCastExpr();
        },

        .Minus => {
            p.index += 1;
            var operand = try p.parseCastExpr();
            // TODO upcast to int / validate arithmetic type
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
            return operand.un(p, .NegateExpr);
        },

        .PlusPlus => {
            p.index += 1;
            var operand = try p.parseCastExpr();
            if (!AST.isLValue(p.nodes.slice(), operand.node)) {
                try p.errToken(.not_assignable, index);
                return error.ParsingFailed;
            }

            switch (operand.value) {
                .unsigned => |*v| v.* += 1,
                .signed => |*v| v.* += 1,
                .unavailable => {},
            }

            return operand.un(p, .PreIncExpr);
        },

        .MinusMinus => {
            p.index += 1;
            var operand = try p.parseCastExpr();
            if (!AST.isLValue(p.nodes.slice(), operand.node)) {
                try p.errToken(.not_assignable, index);
                return error.ParsingFailed;
            }

            switch (operand.value) {
                .unsigned => |*v| v.* -= 1,
                .signed => |*v| v.* -= 1,
                .unavailable => {},
            }

            return operand.un(p, .PreDecExpr);
        },

        .Tilde => {
            p.index += 1;
            var operand = try p.parseUnaryExpr();

            switch (operand.value) {
                .unsigned => |*v| v.* = ~v.*,
                .signed => |*v| v.* = ~v.*,
                .unavailable => {},
            }

            return operand.un(p, .BoolNotExpr);
        },

        .Bang => {
            p.index += 1;
            var operand = try p.parseUnaryExpr();
            if (operand.value != .unavailable) {
                operand.value = .{ .signed = @intFromBool(!operand.getBool()) };
            }

            operand.ty = .{ .specifier = .Int };
            return operand.un(p, .BoolNotExpr);
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
                    res = try p.parseUnaryExpr();
                }
            } else {
                res = try p.parseUnaryExpr();
            }

            if (res.ty.sizeof(p.pp.compilation)) |size| {
                res.value = .{ .unsigned = size };
            } else {
                res.value = .unavailable;
                try p.errStr(.invalid_sizeof, expectedParen, TypeBuilder.fromType(res.ty).toString());
            }
            return res.un(p, .SizeOfExpr);
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
                    res = try p.parseUnaryExpr();
                    try p.errToken(.alignof_expr, expectedParen);
                }
            } else {
                res = try p.parseUnaryExpr();
                try p.errToken(.alignof_expr, expectedParen);
            }

            res.value = .{ .unsigned = res.ty.alignment };
            return res.un(p, .AlignOfExpr);
        },

        else => {
            var lhs = try p.parsePrimaryExpr();
            while (true) {
                const suffix = try p.parseSuffixExpr(lhs);
                if (suffix.node == .none) break;
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
    switch (p.getCurrToken()) {
        .LBracket => {
            var operand = lhs;

            const lb = p.index;
            p.index += 1;
            const index = try p.expr();
            try p.expectClosing(lb, .RBracket);

            // TODO validate type
            try operand.bin(p, .ArrayAccessExpr, index);
            return operand;
        },

        .LParen => {
            const lParen = p.index;
            p.index += 1;
            const ty = lhs.ty.isCallable() orelse {
                try p.errStr(.not_callable, lParen, TypeBuilder.fromType(lhs.ty).toString());
                return error.ParsingFailed;
            };

            const params = ty.data.func.params;

            const listBufferTop = p.listBuffer.items.len;
            defer p.listBuffer.items.len = listBufferTop;
            try p.listBuffer.append(lhs.node);
            var argCount: u32 = 0;

            var firstAfter = lParen;
            if (p.eat(.RParen) == null) {
                while (true) {
                    if (argCount == params.len)
                        firstAfter = p.index;

                    const arg = try p.assignExpr();
                    try arg.expect(p);

                    if (argCount < params.len) {
                        const casted = try arg.coerce(p, params[argCount].ty);
                        try p.listBuffer.append(casted.node);
                    } else {
                        // TODO: coerce to var args passable type
                        try p.listBuffer.append(arg.node);
                    }

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
            if (ty.specifier == .Func and params.len != argCount) {
                try p.errExtra(.expected_arguments, firstAfter, extra);
            }

            if (ty.specifier == .OldStyleFunc and params.len != argCount) {
                try p.errExtra(.expected_arguments_old, firstAfter, extra);
            }

            if (ty.specifier == .VarArgsFunc and argCount < params.len) {
                try p.errExtra(.expected_at_least_arguments, firstAfter, extra);
            }

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
        },

        .Period => {
            p.index += 1;
            const name = try p.expectToken(.Identifier);
            // TODO validate type
            return Result{
                .ty = lhs.ty,
                .node = try p.addNode(.{
                    .tag = .MemberAccessExpr,
                    .type = lhs.ty,
                    .data = .{ .Member = .{ .lhs = lhs.node, .name = name } },
                }),
            };
        },

        .Arrow => {
            p.index += 1;
            const name = try p.expectToken(.Identifier);
            // TODO validate type / deref
            return Result{
                .ty = lhs.ty,
                .node = try p.addNode(.{
                    .tag = .MemberAccessPtrExpr,
                    .type = lhs.ty,
                    .data = .{ .Member = .{ .lhs = lhs.node, .name = name } },
                }),
            };
        },

        .PlusPlus => {
            defer p.index += 1;

            var operand = lhs;
            if (!AST.isLValue(p.nodes.slice(), operand.node)) {
                try p.err(.not_assignable);
                return error.ParsingFailed;
            }

            return operand.un(p, .PostIncExpr);
        },

        .MinusMinus => {
            defer p.index += 1;

            var operand = lhs;
            if (!AST.isLValue(p.nodes.slice(), operand.node)) {
                try p.err(.not_assignable);
                return error.ParsingFailed;
            }

            return operand.un(p, .PostDecExpr);
        },

        else => return Result{},
    }
}

//// primaryExpr
////  : IDENTIFIER
////  | INTEGER_LITERAL
////  | FLOAT_LITERAL
////  | CHAR_LITERAL
////  | STRING_LITERAL
////  | '(' expr ')'
////  | keyword_generic '(' assignExpr ',' genericAssoc (',' genericAssoc)* ')'
////
//// genericAssoc
////  : typeName ':' assignExpr
////  | keyword_default ':' assignExpr
fn parsePrimaryExpr(p: *Parser) Error!Result {
    if (p.eat(.LParen)) |lp| {
        var e = try p.expr();
        try p.expectClosing(lp, .RParen);
        return e.un(p, .ParenExpr);
    }

    switch (p.getCurrToken()) {
        .Identifier => {
            const nameToken = p.index;
            p.index += 1;

            const sym = p.findSymbol(nameToken) orelse {
                if (p.getCurrToken() == .LParen) {
                    // implicitly declare simple functions as like `puts("foo")`;
                    const name = p.tokSlice(nameToken);
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
                    try p.scopes.append(.{ .symbol = .{
                        .name = name,
                        .type = ty,
                        .nameToken = nameToken,
                    } });

                    return Result{
                        .ty = ty,
                        .node = try p.addNode(.{ .tag = .DeclRefExpr, .type = ty, .data = .{ .DeclarationRef = nameToken } }),
                    };
                }
                try p.errStr(.undeclared_identifier, nameToken, p.tokSlice(nameToken));
                return error.ParsingFailed;
            };

            switch (sym) {
                .enumeration => |e| {
                    //TODO  actually check type
                    var res = e.value;
                    res.node = try p.addNode(.{
                        .tag = .EnumerationRef,
                        .type = res.ty,
                        .data = .{ .DeclarationRef = nameToken },
                    });

                    return res;
                },

                .symbol => |s| {
                    // TODO actually check type
                    return Result{
                        .ty = s.type,
                        .node = try p.addNode(.{
                            .tag = .DeclRefExpr,
                            .type = s.type,
                            .data = .{ .DeclarationRef = nameToken },
                        }),
                    };
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
        => {
            return p.todo("char literals");
        },

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
            var res: Result = .{ .value = .{ .signed = 0 } };
            res.node = try p.addNode(.{ .tag = .IntLiteral, .type = res.ty, .data = .{ .Int = 0 } });
            return res;
        },

        .IntegerLiteral,
        .IntegerLiteral_U,
        .IntegerLiteral_L,
        .IntegerLiteral_LU,
        .IntegerLiteral_LL,
        .IntegerLiteral_LLU,
        => {
            const curToken = p.getCurrToken();
            var slice = p.tokSlice(p.index);

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
        },

        .KeywordGeneric => {
            p.index += 1;
            const lp = try p.expectToken(.LParen);
            const controlling = try p.assignExpr();
            _ = try p.expectToken(.Comma);

            const listBufferTop = p.listBuffer.items.len;
            defer p.listBuffer.items.len = listBufferTop;

            try p.listBuffer.append(controlling.node);

            var defaultToken: ?TokenIndex = null;
            var chosen: Result = .{};

            while (true) {
                const start = p.index;
                if (try p.typeName()) |ty| {
                    if (ty.qual.any()) {
                        try p.errToken(.generic_qual_type, start);
                    }

                    _ = try p.expectToken(.Colon);
                    chosen = try p.assignExpr();

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
        },

        else => return Result{},
    }
}

fn parseFloat(p: *Parser, tok: TokenIndex, comptime T: type) Error!T {
    var bytes = p.tokSlice(tok);
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
        return p.todo("non utf-8 strings");

    const index = p.strings.items.len;

    while (start < p.index) : (start += 1) {
        var slice = p.tokSlice(start);
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
                        'x' => try p.parseNumberEscape(start, 16, slice, &i),
                        'u' => try p.parseUnicodeEscape(start, 4, slice, &i),
                        'U' => try p.parseUnicodeEscape(start, 8, slice, &i),
                        '0'...'7' => try p.parseNumberEscape(start, 8, slice, &i),
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

fn parseNumberEscape(p: *Parser, tok: TokenIndex, base: u8, slice: []const u8, i: *usize) !void {
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
    p.strings.appendAssumeCapacity(char);
}

fn parseUnicodeEscape(p: *Parser, tok: TokenIndex, count: u8, slice: []const u8, i: *usize) !void {
    const c = std.fmt.parseInt(u21, slice[i.* + 1 ..][0..count], 16) catch unreachable; // Validated by tokenizer
    i.* += count + 1;
    if (c >= 0x110000 or (c < 0xa0 and c != '$' and c != '@' and c != '¸')) {
        try p.errExtra(.invalid_universal_character, tok, .{ .unsigned = i.* - count - 2 });
        return;
    }
    var buf: [4]u8 = undefined;
    const to_write = std.unicode.utf8Encode(c, &buf) catch unreachable; // validated above
    p.strings.appendSliceAssumeCapacity(buf[0..to_write]);
}
