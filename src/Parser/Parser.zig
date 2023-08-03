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

pp: *Preprocessor,
arena: Allocator,
tokenIds: []const TokenType,
nodes: AST.Node.List = .{},
data: NodeList,
scopes: std.ArrayList(Scope),
index: u32 = 0,
wantConst: bool = false,
inFunc: bool = false,
currDeclList: *NodeList,
labels: std.ArrayList(Label),
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
    const res = p.nodes.len;
    try p.nodes.append(p.pp.compilation.gpa, node);

    return @intCast(res);
}

const Range = struct { start: u32, end: u32 };
fn addList(p: *Parser, nodes: []const NodeIndex) Allocator.Error!Range {
    const start: u32 = @intCast(p.data.items.len);
    try p.data.appendSlice(nodes);
    const end: u32 = @intCast(p.data.items.len);

    return Range{ .start = start, .end = end };
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

fn findLabel(p: *Parser, name: []const u8) ?NodeIndex {
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

/// root : (decl | staticAssert)*
pub fn parse(pp: *Preprocessor) Compilation.Error!AST {
    var rootDecls = NodeList.init(pp.compilation.gpa);
    defer rootDecls.deinit();

    var arena = std.heap.ArenaAllocator.init(pp.compilation.gpa);
    errdefer arena.deinit();

    var p = Parser{
        .pp = pp,
        .arena = arena.allocator(),
        .tokenIds = pp.tokens.items(.id),
        .currDeclList = &rootDecls,
        .scopes = std.ArrayList(Scope).init(pp.compilation.gpa),
        .data = NodeList.init(pp.compilation.gpa),
        .labels = std.ArrayList(Label).init(pp.compilation.gpa),
    };
    defer p.scopes.deinit();
    defer p.data.deinit();
    defer p.labels.deinit();

    errdefer p.nodes.deinit(pp.compilation.gpa);

    _ = try p.addNode(.{ .tag = .Invalid, .type = undefined });

    while (p.eat(.Eof) == null) {
        if (p.staticAssert() catch |er| switch (er) {
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
        .rootDecls = try rootDecls.toOwnedSlice(),
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

    var initD = (try p.initDeclarator(&declSpec)) orelse {
        // TODO: return if enum, struct union
        try p.errToken(.missing_declaration, firstTokenIndex);

        // eat ';'
        _ = try p.expectToken(.Semicolon);

        return true;
    };

    // check for funtion definition
    if (initD.d.funcDeclarator != null and initD.initializer == 0 and initD.d.type.isFunc()) fndef: {
        switch (p.getCurrToken()) {
            .Comma, .Semicolon => break :fndef,
            .LBrace => {},
            else => {
                if (!p.inFunc) try p.err(.expected_fn_body);
                break :fndef;
            },
        }
        // TODO declare all parameters

        // collect old style parameters
        if (initD.d.oldTypeFunc != null and !p.inFunc) {
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
                    } else if (d.type.specifier == .Void) {
                        try p.errToken(.invalid_void_param, d.name);
                    }

                    if (p.eat(.Comma) == null) break;
                }

                _ = try p.expectToken(.Semicolon);
            }
        }

        if (p.inFunc) try p.err(.func_not_in_root);

        const inFunction = p.inFunc;
        p.inFunc = true;
        defer p.inFunc = inFunction;

        const node = try p.addNode(.{
            .type = initD.d.type,
            .tag = try declSpec.validateFnDef(p),
            .first = initD.d.name,
        });
        try p.scopes.append(.{ .symbol = .{
            .name = p.tokSlice(initD.d.name),
            .node = node,
            .nameToken = initD.d.name,
        } });

        const body = try p.compoundStmt();
        p.nodes.items(.second)[node] = body.?;

        // check gotos
        if (inFunction) {
            for (p.labels.items) |item| {
                if (item == .unresolvedGoto)
                    try p.errStr(.undeclared_label, item.unresolvedGoto, p.tokSlice(item.unresolvedGoto));

                p.labels.items.len = 0;
                p.labelCount = 0;
            }
        }

        try p.currDeclList.append(node);

        return true;
    }

    // Declare all variable/typedef declarators.
    while (true) {
        if (initD.d.oldTypeFunc) |tokenIdx|
            try p.errToken(.invalid_old_style_params, tokenIdx);

        const node = try p.addNode(.{
            .type = initD.d.type,
            .tag = try declSpec.validate(p, initD.d.type, initD.initializer != 0),
            .first = initD.d.name,
            .second = initD.initializer,
        });
        try p.currDeclList.append(node);

        if (declSpec.storageClass == .typedef) {
            try p.scopes.append(.{ .typedef = .{
                .name = p.tokSlice(initD.d.name),
                .node = node,
                .nameToken = initD.d.name,
            } });
        } else {
            try p.scopes.append(.{ .symbol = .{
                .name = p.tokSlice(initD.d.name),
                .node = node,
                .nameToken = initD.d.name,
            } });
        }

        if (p.eat(.Comma) == null)
            break;

        initD = (try p.initDeclarator(&declSpec)) orelse {
            try p.err(.expected_ident_or_l_paren);
            continue;
        };
    }

    _ = try p.expectToken(.Semicolon);
    return true;
}

/// staticAssert : keyword_static_assert '(' constExpr ',' STRING_LITERAL ')' ';'
fn staticAssert(p: *Parser) Error!bool {
    const curToken = p.eat(.KeywordStaticAssert) orelse return false;

    const lParen = try p.expectToken(.LParen);
    var start = p.index;
    const res = try p.constExpr();
    const end = p.index;

    _ = try p.expectToken(.Comma);
    // TODO: reslove the string literal.
    const str = try p.expectToken(.StringLiteral);
    try p.expectClosing(lParen, .RParen);

    if (!res.getBool()) {
        var msg = std.ArrayList(u8).init(p.pp.compilation.gpa);
        defer msg.deinit();

        try msg.append('\'');
        while (start < end) {
            try msg.appendSlice(p.tokSlice(start));
            start += 1;
            if (start != end) try msg.append(' ');
        }

        try msg.appendSlice("' ");
        try msg.appendSlice(p.tokSlice(str));
        try p.errStr(.static_assert_failure, curToken, try p.pp.arena.allocator().dupe(u8, msg.items));
    }

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
        if (try p.typeSpec(&spec, &d.type))
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

const InitDeclarator = struct { d: Declarator, initializer: NodeIndex = 0 };

/// initDeclarator : declarator ('=' initializer)?
fn initDeclarator(p: *Parser, declSpec: *DeclSpec) Error!?InitDeclarator {
    var initD = InitDeclarator{ .d = (try p.declarator(declSpec.type, .normal)) orelse return null };

    if (p.eat(.Equal)) |_| {
        if (declSpec.storageClass == .typedef or declSpec.type.isFunc())
            try p.err(.illegal_initializer);

        if (declSpec.storageClass == .@"extern") {
            try p.err(.extern_initializer);
            declSpec.storageClass = .none;
        }

        const init = try p.initializer();
        const casted = try init.coerce(p, initD.d.type);

        initD.initializer = try casted.toNode(p);
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
/// alignSpec : keyword_alignas '(' typeName ')'
fn typeSpec(p: *Parser, ty: *TypeBuilder, completeType: *Type) Error!bool {
    const start = p.index;
    while (true) {
        if (try p.typeQual(completeType)) {
            continue;
        }

        switch (p.getCurrToken()) {
            .KeywordVoid => try ty.combine(p, .Void),
            .KeywordBool => try ty.combine(p, .Bool),
            .KeywordChar => try ty.combine(p, .Char),
            .KeywordShort => try ty.combine(p, .Short),
            .KeywordInt => try ty.combine(p, .Int),
            .KeywordLong => try ty.combine(p, .Long),
            .KeywordSigned => try ty.combine(p, .Signed),
            .KeywordUnsigned => try ty.combine(p, .Unsigned),
            .KeywordFloat => try ty.combine(p, .Float),
            .KeywordDouble => try ty.combine(p, .Double),
            .KeywordComplex => try ty.combine(p, .Complex),

            .KeywordAtomic => return p.todo("atomic types"),
            .KeywordEnum => {
                try ty.combine(p, .{ .Enum = 0 });
                ty.kind.Enum = try p.enumSpec();
                continue;
            },

            .KeywordStruct => {
                try ty.combine(p, .{ .Struct = 0 });
                ty.kind.Struct = try p.recordSpec();
                continue;
            },

            .KeywordUnion => {
                try ty.combine(p, .{ .Union = 0 });
                ty.kind.Union = try p.recordSpec();
                continue;
            },

            .KeywordAlignas => {
                if (completeType.alignment != 0)
                    try p.errStr(.duplicate_declspec, p.index, "alignment");

                const lp = try p.expectToken(.LParen);
                const otherType = (try p.typeName()) orelse {
                    try p.err(.expected_type);
                    return error.ParsingFailed;
                };

                try p.expectClosing(lp, .RParen);
                completeType.alignment = otherType.alignment;
            },

            .Identifier => {
                const typedef = p.findTypedef(p.tokSlice(p.index)) orelse break;
                const newSpec = TypeBuilder.fromType(p.nodes.items(.type)[typedef.node]);

                const errStart = p.pp.compilation.diag.list.items.len;
                ty.combine(p, newSpec) catch {
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

/// recordSpec
///  : (keyword_struct | keyword_union) IDENTIFIER? { recordDecl* }
///  | (keyword_struct | keyword_union) IDENTIFIER
fn recordSpec(p: *Parser) Error!NodeIndex {
    _ = p.getCurrToken();
    p.index += 1;

    return p.todo("recordSpec");
}
/// recordDecl
///  : specQual+ (recordDeclarator (',' recordDeclarator)*)? ;
///  | staticAssert
fn recordDecl(p: *Parser) Error!NodeIndex {
    return p.todo("recordDecl");
}

/// recordDeclarator : declarator (':' constExpr)?
fn recordDeclarator(p: *Parser) Error!NodeIndex {
    return p.todo("recordDeclarator");
}

// specQual : typeSpec | typeQual | alignSpec
fn specQual(p: *Parser) Error!?Type {
    var spec: TypeBuilder = .{};
    var ty: Type = .{ .specifier = undefined };

    if (try p.typeSpec(&spec, &ty)) {
        try spec.finish(p, &ty);
        return ty;
    }

    return null;
}

/// enumSpec
///  : keyword_enum IDENTIFIER? { enumerator (',' enumerator)? ',') }
///  | keyword_enum IDENTIFIER
fn enumSpec(p: *Parser) Error!NodeIndex {
    _ = p.getCurrToken();
    p.index += 1;

    return p.todo("enumSpec");
}

/// enumerator : IDENTIFIER ('=' constExpr)
fn enumerator(p: *Parser) Error!NodeIndex {
    return p.todo("enumerator");
}

/// atomicTypeSpec : keyword_atomic '(' typeName ')'
/// typeQual : keyword_const | keyword_restrict | keyword_volatile | keyword_atomic
fn typeQual(p: *Parser, ty: *Type) Error!bool {
    var any = false;

    while (true) {
        switch (p.getCurrToken()) {
            .KeywordRestrict => {
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

            .KeywordConst => {
                if (ty.qual.@"const")
                    try p.errStr(.duplicate_declspec, p.index, "const")
                else
                    ty.qual.@"const" = true;
            },

            .KeywordVolatile => {
                if (ty.qual.@"volatile")
                    try p.errStr(.duplicate_declspec, p.index, "volatile")
                else
                    ty.qual.@"volatile" = true;
            },

            .KeywordAtomic => {
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

const DeclaratorKind = enum { normal, abstract, param };

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

    if (kind == .normal) {
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
        var gotQuals = try p.typeQual(&resType);
        var static = p.eat(.KeywordStatic);

        if (static != null and !gotQuals)
            gotQuals = try p.typeQual(&resType);

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

        switch (size.data) {
            .none => if (star) |_| {
                const elemType = try p.arena.create(Type);
                resType.data = .{ .subType = elemType };
                resType.specifier = .UnspecifiedVariableLenArray;
            } else {
                const arrayType = try p.arena.create(Type.Array);
                arrayType.len = 0;
                resType.data = .{ .array = arrayType };
                resType.specifier = .IncompleteArray;
            },

            .lVal, .node => |n| {
                if (!p.inFunc and kind != .param) try p.errToken(.variable_len_array_file_scope, lb);
                const vlaType = try p.arena.create(Type.VLA);
                vlaType.expr = n;
                resType.data = .{ .vla = vlaType };
                resType.specifier = .VariableLenArray;

                if (static) |some| try p.errToken(.useless_static, some);
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
        funcType.paramTypes = &.{};
        var specifier: Type.Specifier = .Func;

        if (try p.paramDecls()) |params| {
            funcType.paramTypes = params;

            if (p.eat(.Ellipsis)) |_|
                specifier = .VarArgsFunc;
        } else if (p.getCurrToken() == .RParen) {
            specifier = .OldStyleFunc;
        } else if (p.getCurrToken() == .Identifier) {
            d.oldTypeFunc = p.index;

            var params = NodeList.init(p.pp.compilation.gpa);
            defer params.deinit();

            specifier = .OldStyleFunc;
            while (true) {
                const param = try p.addNode(.{
                    .tag = .ParamDecl,
                    .type = .{ .specifier = .Int },
                    .first = try p.expectToken(.Identifier),
                });
                try params.append(param);

                if (p.eat(.Comma) == null) break;
            }

            funcType.paramTypes = try p.arena.dupe(NodeIndex, params.items);
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
        _ = try p.typeQual(&ty);
    }

    return ty;
}

/// paramDecls : paramDecl (',' paramDecl)* (',' '...')
/// paramDecl : declSpec (declarator | abstractDeclarator)
fn paramDecls(p: *Parser) Error!?[]NodeIndex {
    var params = NodeList.init(p.pp.compilation.gpa);
    defer params.deinit();

    while (true) {
        const paramDeclSpec = if (try p.declSpecifier()) |some|
            some
        else if (params.items.len == 0)
            return null
        else blk: {
            var d: DeclSpec = .{};
            var spec: TypeBuilder = .{};

            try spec.finish(p, &d.type);

            break :blk d;
        };

        var nameToken = p.index;
        var paramType = paramDeclSpec.type;
        if (try p.declarator(paramDeclSpec.type, .param)) |some| {
            if (some.oldTypeFunc) |tokenIdx|
                try p.errToken(.invalid_old_style_params, tokenIdx);

            // TODO: declare()
            nameToken = some.name;
            paramType = some.type;
        }

        if (paramType.isFunc()) {
            // params declared as functions are converted to function pointers
            const elemType = try p.arena.create(Type);
            elemType.* = paramType;
            paramType = Type{
                .specifier = .Pointer,
                .data = .{ .subType = elemType },
            };
        } else if (paramType.specifier == .Void) {
            // validate void parameters
            if (params.items.len == 0) {
                if (p.getCurrToken() != .RParen) {
                    try p.err(.void_only_param);
                    if (paramType.qual.any())
                        try p.err(.void_param_qualified);

                    return error.ParsingFailed;
                }

                return &[0]NodeIndex{};
            }

            try p.err(.void_must_be_first_param);

            return error.ParsingFailed;
        } else if (paramType.isArray()) {
            // TODO : convert to pointer
        }

        const param = try p.addNode(.{
            .tag = try paramDeclSpec.validateParam(p),
            .type = paramType,
            .first = nameToken,
        });
        try params.append(param);

        if (p.eat(.Comma) == null)
            break;

        if (p.getCurrToken() == .Ellipsis)
            break;
    }

    return try p.arena.dupe(NodeIndex, params.items);
}

/// typeName : specQual+ abstractDeclarator
fn typeName(p: *Parser) Error!?Type {
    var ty = (try p.specQual()) orelse return null;
    if (try p.declarator(ty, .abstract)) |some| {
        if (some.oldTypeFunc) |tokenIdx|
            try p.errToken(.invalid_old_style_params, tokenIdx);

        return some.type;
    } else return null;
}

/// initializer
///  : assignExpr
///  | '{' initializerItems '}'
fn initializer(p: *Parser) Error!Result {
    if (p.eat(.LBrace)) |_| {
        return p.todo("compound initializer");
    }

    const res = try p.assignExpr();
    try res.expect(p);

    return res;
}

/// initializerItems : designation? initializer  (',' designation? initializer)? ','?
fn initializerItems(p: *Parser) Error!NodeIndex {
    return p.todo("initializerItems");
}

/// designation : designator+ '='
fn designation(p: *Parser) Error!NodeIndex {
    return p.todo("designation");
}

/// designator
///  : '[' constExpr ']'
///  | '.' identifier
fn designator(p: *Parser) Error!NodeIndex {
    return p.todo("designator");
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

    if (try p.compoundStmt()) |some|
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
            .first = nameToken,
        });
    }

    if (p.eat(.KeywordContinue)) |cont| {
        if (!p.inLoop())
            try p.errToken(.continue_not_in_loop, cont);
        _ = try p.expectToken(.Semicolon);

        return try p.addNode(.{ .tag = .ContinueStmt });
    }

    if (p.eat(.KeywordBreak)) |br| {
        if (!p.inLoopOrSwitch())
            try p.errToken(.break_not_in_loop_or_switch, br);

        _ = try p.expectToken(.Semicolon);
        return try p.addNode(.{ .tag = .BreakStmt });
    }

    if (p.eat(.KeywordReturn)) |_| {
        const e = try p.expr();
        _ = try p.expectToken(.Semicolon);

        const result = try e.toNode(p);
        return try p.addNode(.{ .tag = .ReturnStmt, .first = result });
    }

    const exprStart = p.index;
    const e = try p.expr();
    if (e.data != .none) {
        _ = try p.expectToken(.Semicolon);
        const exprNode = try e.toNode(p);
        try p.maybeWarnUnused(exprNode, exprStart);
        return exprNode;
    }

    if (p.eat(.Semicolon)) |_|
        return @as(NodeIndex, 0);

    try p.err(.expected_stmt);
    return error.ParsingFailed;
}

fn parseIfStmt(p: *Parser) Error!NodeIndex {
    const startScopeLen = p.scopes.items.len;
    defer p.scopes.items.len = startScopeLen;

    const lp = try p.expectToken(.LParen);
    const cond = try p.expr();

    try cond.expect(p);

    const condNode = try cond.toNode(p);
    try p.expectClosing(lp, .RParen);

    const then = try p.stmt();
    const elseTK = if (p.eat(.KeywordElse)) |_| try p.stmt() else 0;

    if (then != 0 and elseTK != 0) {
        return try p.addNode(.{
            .tag = .IfThenElseStmt,
            .first = condNode,
            .second = (try p.addList(&.{ then, elseTK })).start,
        });
    } else if (then == 0 and elseTK != 0) {
        return try p.addNode(.{
            .tag = .IfElseStmt,
            .first = condNode,
            .second = elseTK,
        });
    } else return try p.addNode(.{
        .tag = .IfThenStmt,
        .first = condNode,
        .second = elseTK,
    });
}

fn parseForStmt(p: *Parser) Error!NodeIndex {
    const startScopeLen = p.scopes.items.len;
    defer p.scopes.items.len = startScopeLen;

    var decls = NodeList.init(p.pp.compilation.gpa);
    defer decls.deinit();

    const savedDecls = p.currDeclList;
    defer p.currDeclList = savedDecls;
    p.currDeclList = &decls;

    const lp = try p.expectToken(.LParen);
    const gotDecl = try p.parseDeclaration();

    // for-init
    const initStart = p.index;
    const init = if (!gotDecl) try p.expr() else Result{};
    const initNode = try init.toNode(p);
    try p.maybeWarnUnused(initNode, initStart);

    if (!gotDecl)
        _ = try p.expectToken(.Semicolon);

    // cond
    const cond = try p.expr();
    const condNode = try cond.toNode(p);
    _ = try p.expectToken(.Semicolon);

    // increment
    const incrStart = p.index;
    const incr = try p.expr();
    const incrNode = try incr.toNode(p);
    try p.maybeWarnUnused(incrNode, incrStart);
    try p.expectClosing(lp, .RParen);

    try p.scopes.append(.loop);
    const body = try p.stmt();

    if (gotDecl) {
        const start = (try p.addList(decls.items)).start;
        const end = (try p.addList(&.{ condNode, incrNode, body })).end;

        return try p.addNode(.{
            .tag = .ForDeclStmt,
            .first = start,
            .second = end,
        });
    } else if (init.data == .none and cond.data == .none and incr.data == .none) {
        return try p.addNode(.{
            .tag = .ForEverStmt,
            .first = body,
        });
    } else {
        const range = try p.addList(&.{ initNode, condNode, incrNode });
        return try p.addNode(.{
            .tag = .ForStmt,
            .first = range.start,
            .second = body,
        });
    }
}

fn parseWhileStmt(p: *Parser) Error!NodeIndex {
    const startScopeLen = p.scopes.items.len;
    defer p.scopes.items.len = startScopeLen;

    const lp = try p.expectToken(.LParen);
    const cond = try p.expr();

    try cond.expect(p);
    const condNode = try cond.toNode(p);
    try p.expectClosing(lp, .RParen);

    try p.scopes.append(.loop);
    const body = try p.stmt();

    return try p.addNode(.{
        .tag = .WhileStmt,
        .first = condNode,
        .second = body,
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
    const condNode = try cond.toNode(p);
    try p.expectClosing(lp, .RParen);

    _ = try p.expectToken(.Semicolon);
    return try p.addNode(.{
        .tag = .WhileStmt,
        .first = condNode,
        .second = body,
    });
}

fn parseSwitchStmt(p: *Parser) Error!NodeIndex {
    const startScopeLen = p.scopes.items.len;
    defer p.scopes.items.len = startScopeLen;

    const lp = try p.expectToken(.LParen);
    const cond = try p.expr();

    try cond.expect(p);
    const condNode = try cond.toNode(p);
    try p.expectClosing(lp, .RParen);

    var switchScope = Scope.Switch{
        .cases = Scope.Switch.CaseMap.init(p.pp.compilation.gpa),
    };
    defer switchScope.cases.deinit();

    try p.scopes.append(.{ .@"switch" = &switchScope });
    const body = try p.stmt();

    return try p.addNode(.{
        .tag = .SwitchStmt,
        .first = condNode,
        .second = body,
    });
}

fn parseCaseStmt(p: *Parser, caseToken: u32) Error!?NodeIndex {
    const val = try p.constExpr();
    _ = try p.expectToken(.Colon);

    const s = try p.stmt();
    const node = try p.addNode(.{
        .tag = .CaseStmt,
        .first = try val.toNode(p),
        .second = s,
    });

    if (p.findSwitch()) |some| {
        const gop = try some.cases.getOrPut(val);
        if (gop.found_existing) {
            switch (val.data) {
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
        .first = s,
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
    switch (p.nodes.items(.tag)[node]) {
        .Invalid,
        .AssignExpr,
        .MulAssignExpr,
        .DivAssignExpr,
        .ModAssignExpr,
        .AddAssignExpr,
        .SubAssignExpr,
        .ShlAssignExpr,
        .ShrAssignExpr,
        .AndAssignExpr,
        .XorAssignExpr,
        .OrAssignExpr, //
        .CallExpr,
        .CallExprOne,
        => {},
        else => try p.errToken(.unused_value, exprStart),
    }
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
            .first = nameToken,
            .second = try p.stmt(),
        });
    } else if (p.eat(.KeywordCase)) |case| {
        return p.parseCaseStmt(case);
    } else if (p.eat(.KeywordDefault)) |default| {
        return p.parseDefaultStmt(default);
    } else return null;
}

/// compoundStmt : '{' ( decl | staticAssert |stmt)* '}'
fn compoundStmt(p: *Parser) Error!?NodeIndex {
    const lBrace = p.eat(.LBrace) orelse return null;

    var statements = NodeList.init(p.pp.compilation.gpa);
    defer statements.deinit();

    const savedDS = p.currDeclList;
    defer p.currDeclList = savedDS;
    p.currDeclList = &statements;

    var noreturnIdx: ?TokenIndex = null;
    var noreturnLabelCount: u32 = 0;

    while (p.eat(.RBrace) == null) {
        if (p.staticAssert() catch |er| switch (er) {
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

        if (s == 0) continue;
        try statements.append(s);

        if (noreturnIdx == null and p.nodeIsNoreturn(s)) {
            noreturnIdx = p.index;
            noreturnLabelCount = p.labelCount;
        }
    }

    if (noreturnIdx) |some| {
        if (noreturnLabelCount == p.labelCount and some != p.index - 1)
            try p.errToken(.unreachable_code, some);
    }

    switch (statements.items.len) {
        0 => return try p.addNode(.{ .tag = .CompoundStmtTwo }),
        1 => return try p.addNode(.{
            .tag = .CompoundStmtTwo,
            .first = statements.items[0],
        }),
        2 => return try p.addNode(.{
            .tag = .CompoundStmtTwo,
            .first = statements.items[0],
            .second = statements.items[1],
        }),
        else => {
            const range = try p.addList(statements.items);
            return try p.addNode(.{
                .tag = .CompoundStmt,
                .first = range.start,
                .second = range.end,
            });
        },
    }
}

fn nodeIsNoreturn(p: *Parser, node: NodeIndex) bool {
    switch (p.nodes.items(.tag)[node]) {
        .BreakStmt, .ContinueStmt, .ReturnStmt => return true,
        .IfThenElseStmt => {
            const data = p.data.items[p.nodes.items(.second)[node]..];
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

/// expr : assignExpr (',' assignExpr)*
fn expr(p: *Parser) Error!Result {
    var lhs = try p.assignExpr();
    while (p.eat(.Comma)) |_| {
        return p.todo("comma operator");
    }

    return lhs;
}

/// assignExpr
///  : conditionalExpr
///  | unaryExpr ('=' | '*=' | '/=' | '%=' | '+=' | '-=' | '<<=' | '>>=' | '&=' | '^=' | '|=') assignExpr
fn assignExpr(p: *Parser) Error!Result {
    return p.conditionalExpr();
}

/// constExpr : conditionalExpr
pub fn constExpr(p: *Parser) Error!Result {
    const saved_const = p.wantConst;
    defer p.wantConst = saved_const;
    p.wantConst = true;

    const res = try p.conditionalExpr();
    try res.expect(p);

    return res;
}

/// conditionalExpr : logicalOrExpr ('?' expression? ':' conditionalExpr)?
fn conditionalExpr(p: *Parser) Error!Result {
    const cond = try p.logicalOrExpr();
    if (p.eat(.QuestionMark) == null)
        return cond;

    const thenExpr = try p.expr();
    _ = try p.expectToken(.Colon);
    const elseExpr = try p.conditionalExpr();

    if (cond.data == .signed or cond.data == .unsigned)
        return if (cond.getBool()) thenExpr else elseExpr;

    return p.todo("ast");
}

/// logicalOrExpr : logicalAndExpr ('||' logicalAndExpr)*
fn logicalOrExpr(p: *Parser) Error!Result {
    var lhs = try p.logicalAndExpr();
    while (p.eat(.PipePipe)) |_| {
        const rhs = try p.logicalAndExpr();

        if ((lhs.data == .unsigned or lhs.data == .signed) and (rhs.data == .unsigned or rhs.data == .signed)) {
            lhs = Result{ .data = .{ .signed = @intFromBool(lhs.getBool() or rhs.getBool()) } };
        } else {
            return p.todo("ast");
        }
    }

    return lhs;
}

/// logicalAndExpr : orExpr ('&&' orExpr)*
fn logicalAndExpr(p: *Parser) Error!Result {
    var lhs = try p.orExpr();
    while (p.eat(.AmpersandAmpersand)) |_| {
        const rhs = try p.orExpr();

        if ((lhs.data == .unsigned or lhs.data == .signed) and (rhs.data == .unsigned or rhs.data == .signed)) {
            lhs = Result{ .data = .{ .signed = @intFromBool(lhs.getBool() or rhs.getBool()) } };
        } else return p.todo("ast");
    }
    return lhs;
}

/// orExpr : xorExpr ('|' xorExpr)*
fn orExpr(p: *Parser) Error!Result {
    var lhs = try p.xorExpr();
    while (p.eat(.Pipe)) |_| {
        var rhs = try p.xorExpr();

        if (try lhs.adjustTypes(&rhs, p)) {
            lhs.data = switch (lhs.data) {
                .unsigned => |v| .{ .unsigned = v | rhs.data.unsigned },
                .signed => |v| .{ .signed = v | rhs.data.signed },
                else => unreachable,
            };
        } else return p.todo("ast");
    }
    return lhs;
}

/// xorExpr : andExpr ('^' andExpr)*
fn xorExpr(p: *Parser) Error!Result {
    var lhs = try p.andExpr();
    while (p.eat(.Caret)) |_| {
        var rhs = try p.andExpr();

        if (try lhs.adjustTypes(&rhs, p)) {
            lhs.data = switch (lhs.data) {
                .unsigned => |v| .{ .unsigned = v ^ rhs.data.unsigned },
                .signed => |v| .{ .signed = v ^ rhs.data.signed },
                else => unreachable,
            };
        } else return p.todo("ast");
    }
    return lhs;
}

/// andExpr : eqExpr ('&' eqExpr)*
fn andExpr(p: *Parser) Error!Result {
    var lhs = try p.eqExpr();
    while (p.eat(.Ampersand)) |_| {
        var rhs = try p.eqExpr();

        if (try lhs.adjustTypes(&rhs, p)) {
            lhs.data = switch (lhs.data) {
                .unsigned => |v| .{ .unsigned = v & rhs.data.unsigned },
                .signed => |v| .{ .signed = v & rhs.data.signed },
                else => unreachable,
            };
        } else return p.todo("ast");
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

            lhs = Result{ .data = .{ .signed = @intFromBool(res) } };
        }

        return p.todo("ast");
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
            lhs = Result{ .data = .{ .signed = @intFromBool(res) } };
        } else return p.todo("ast");
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
                lhs.data = switch (lhs.data) {
                    .unsigned => |v| .{ .unsigned = v << @as(u6, @intCast(rhs.data.unsigned)) },
                    .signed => |v| .{ .signed = v << @as(u6, @intCast(rhs.data.signed)) },
                    else => unreachable,
                };
            } else {
                lhs.data = switch (lhs.data) {
                    .unsigned => |v| .{ .unsigned = v >> @as(u6, @intCast(rhs.data.unsigned)) },
                    .signed => |v| .{ .signed = v >> @as(u6, @intCast(rhs.data.signed)) },
                    else => unreachable,
                };
            }
        } else return p.todo("ast");

        return p.todo("ast");
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
        } else return p.todo("ast");
    }
    return lhs;
}

/// mulExpr : castExpr (('*' | '/' | '%') castExpr)*´
fn mulExpr(p: *Parser) Error!Result {
    var lhs = try p.castExpr();
    while (true) {
        const mul = p.eat(.Asterisk);
        const div = mul orelse p.eat(.Slash);
        const percent = div orelse p.eat(.Percent);
        if (percent == null) break;
        var rhs = try p.castExpr();

        if (try lhs.adjustTypes(&rhs, p)) {
            // TODO divide by 0
            if (mul != null) {
                try lhs.mul(mul.?, rhs, p);
            } else if (div != null) {
                lhs.data = switch (lhs.data) {
                    .unsigned => |v| .{ .unsigned = v / rhs.data.unsigned },
                    .signed => |v| .{ .signed = @divFloor(v, rhs.data.signed) },
                    else => unreachable,
                };
            } else {
                lhs.data = switch (lhs.data) {
                    .unsigned => |v| .{ .unsigned = v % rhs.data.unsigned },
                    .signed => |v| .{ .signed = @rem(v, rhs.data.signed) },
                    else => unreachable,
                };
            }
        } else return p.todo("ast");
    }
    return lhs;
}

/// castExpr :  ( '(' type_name ')' )* unaryExpr
fn castExpr(p: *Parser) Error!Result {
    while (p.eat(.LParen)) |lp| {
        const ty = (try p.typeName()) orelse {
            p.index += 1;
            break;
        };
        _ = ty;
        try p.expectClosing(lp, .RParen);

        return p.todo("cast");
    }

    return p.unaryExpr();
}

/// unaryExpr
///  : primaryExpr suffixExpr*
///  | ('&' | '*' | '+' | '-' | '~' | '!' | '++' | '--') castExpr
///  | keyword_sizeof unaryExpr
///  | keyword_sizeof '(' type_name ')'
///  | keyword_alignof '(' type_name ')'
fn unaryExpr(p: *Parser) Error!Result {
    switch (p.getCurrToken()) {
        .Ampersand => return p.todo("unaryExpr ampersand"),
        .Asterisk => return p.todo("unaryExpr asterisk"),
        .Plus => {
            p.index += 1;
            // TODO upcast to int / validate arithmetic type
            return p.castExpr();
        },
        .Minus => {
            p.index += 1;
            var operand = try p.castExpr();
            // TODO upcast to int / validate arithmetic type
            const size = operand.ty.sizeof(p.pp.compilation);
            switch (operand.data) {
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
                else => return p.todo("ast"),
            }
            return operand;
        },
        .PlusPlus => return p.todo("unary inc"),
        .MinusMinus => return p.todo("unary dec"),
        .Tilde => return p.todo("unaryExpr tilde"),
        .Bang => {
            p.index += 1;
            const lhs = try p.unaryExpr();
            if (lhs.data == .unsigned or lhs.data == .signed) {
                return Result{ .data = .{ .signed = @intFromBool(!lhs.getBool()) } };
            }
            return p.todo("ast");
        },
        .KeywordSizeof => return p.todo("unaryExpr sizeof"),
        else => {
            var lhs = try p.primaryExpr();
            while (true) {
                const suffix = try p.suffixExpr(lhs);
                if (suffix.data == .none) break;
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
fn suffixExpr(p: *Parser, lhs: Result) Error!Result {
    switch (p.getCurrToken()) {
        .LBracket => return p.todo("array access"),
        .LParen => {
            const lParen = p.index;
            p.index += 1;
            const ty = lhs.ty.isCallable() orelse {
                try p.errStr(.not_callable, lParen, TypeBuilder.fromType(lhs.ty).toString());
                return error.ParsingFailed;
            };

            const paramTypes = ty.data.func.paramTypes;

            var args = NodeList.init(p.pp.compilation.gpa);
            defer args.deinit();

            var firstAfter = lParen;
            if (p.eat(.RParen) == null) {
                while (true) {
                    if (args.items.len == paramTypes.len)
                        firstAfter = p.index;

                    const arg = try p.assignExpr();
                    try arg.expect(p);

                    if (args.items.len < paramTypes.len) {
                        const paramType = p.nodes.items(.type)[paramTypes[args.items.len]];
                        const casted = try arg.coerce(p, paramType);

                        try args.append(try casted.toNode(p));
                    } else {
                        // TODO: coerce to var args passable type
                        try args.append(try arg.toNode(p));
                    }

                    _ = p.eat(.Comma) orelse break;
                }

                try p.expectClosing(lParen, .RParen);
            }

            const extra = Diagnostics.Message.Extra{ .arguments = .{ .expected = @as(u32, @intCast(paramTypes.len)), .actual = @as(u32, @intCast(args.items.len)) } };
            if (ty.specifier == .Func and paramTypes.len != args.items.len) {
                try p.errExtra(.expected_arguments, firstAfter, extra);
            }

            if (ty.specifier == .OldStyleFunc and paramTypes.len != args.items.len) {
                try p.errExtra(.expected_arguments_old, firstAfter, extra);
            }

            if (ty.specifier == .VarArgsFunc and args.items.len < paramTypes.len) {
                try p.errExtra(.expected_at_least_arguments, firstAfter, extra);
            }

            switch (args.items.len) {
                0 => return try Result.node(p, .{
                    .tag = .CallExprOne,
                    .type = ty.data.func.returnType,
                    .first = try lhs.toNode(p),
                }),
                1 => return try Result.node(p, .{
                    .tag = .CallExprOne,
                    .type = ty.data.func.returnType,
                    .first = try lhs.toNode(p),
                    .second = args.items[0],
                }),
                else => {
                    try p.data.append(try lhs.toNode(p));
                    const range = try p.addList(args.items);
                    return try Result.node(p, .{
                        .tag = .CallExpr,
                        .type = ty.data.func.returnType,
                        .first = range.start - 1,
                        .second = range.end,
                    });
                },
            }
        },
        .Period => return p.todo("member access"),
        .Arrow => return p.todo("member access pointer"),
        .PlusPlus => return p.todo("post inc"),
        .MinusMinus => return p.todo("post dec"),

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
////  | '(' typeName ')' '{' initializerItems '}'
////  | keyword_generic '(' assignExpr ',' genericAssoc (',' genericAssoc)* ')'
////
//// genericAssoc
////  : typeName ':' assignExpr
////  | keyword_default ':' assignExpr
fn primaryExpr(p: *Parser) Error!Result {
    if (p.eat(.LParen)) |lp| {
        const e = try p.expr();
        try p.expectClosing(lp, .RParen);
        return e;
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
                    funcType.* = .{ .returnType = .{ .specifier = .Int }, .paramTypes = &.{} };
                    const ty: Type = .{ .specifier = .OldStyleFunc, .data = .{ .func = funcType } };
                    const node = try p.addNode(.{
                        .type = ty,
                        .tag = .FnProto,
                        .first = nameToken,
                    });

                    try p.currDeclList.append(node);
                    try p.scopes.append(.{ .symbol = .{
                        .name = name,
                        .node = node,
                        .nameToken = nameToken,
                    } });

                    return try Result.leftValue(p, .{
                        .tag = .DeclRefExpr,
                        .type = ty,
                        .first = nameToken,
                        .second = node,
                    });
                }
                try p.errStr(.undeclared_identifier, nameToken, p.tokSlice(nameToken));
                return error.ParsingFailed;
            };

            switch (sym) {
                .enumeration => |e| return e.value,
                .symbol => |s| {
                    // TODO actually check type
                    if (p.wantConst) {
                        try p.err(.expected_integer_constant_expr);
                        return error.ParsingFailed;
                    }
                    return try Result.leftValue(p, .{
                        .tag = .DeclRefExpr,
                        .type = p.nodes.items(.type)[s.node],
                        .first = nameToken,
                        .second = s.node,
                    });
                },
                else => unreachable,
            }
        },

        .StringLiteral,
        .StringLiteralUTF_8,
        .StringLiteralUTF_16,
        .StringLiteralUTF_32,
        .StringLiteralWide,
        => {
            if (p.wantConst) {
                try p.err(.expected_integer_constant_expr);
                return error.ParsingFailed;
            }

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

            var builder = std.ArrayList(u8).init(p.pp.compilation.gpa);
            defer builder.deinit();

            while (start < p.index) : (start += 1) {
                var slice = p.tokSlice(start);
                slice = slice[std.mem.indexOf(u8, slice, "\"").? + 1 .. slice.len - 1];

                try builder.ensureTotalCapacity(slice.len);
                var i: u32 = 0;
                while (i < slice.len) : (i += 1) {
                    switch (slice[i]) {
                        '\\' => {
                            i += 1;
                            switch (slice[i]) {
                                '\n' => i += 1,
                                '\r' => i += 2,
                                '\'', '\"', '\\', '?' => |c| builder.appendAssumeCapacity(c),
                                'n' => builder.appendAssumeCapacity('\n'),
                                'r' => builder.appendAssumeCapacity('\r'),
                                't' => builder.appendAssumeCapacity('\t'),
                                'a' => builder.appendAssumeCapacity(0x07),
                                'b' => builder.appendAssumeCapacity(0x08),
                                'e' => builder.appendAssumeCapacity(0x1B),
                                'f' => builder.appendAssumeCapacity(0x0C),
                                'v' => builder.appendAssumeCapacity(0x0B),
                                'x' => return p.todo("hex escape"),
                                'u' => return p.todo("u escape"),
                                'U' => return p.todo("U escape"),
                                '0'...'7' => return p.todo("octal escape"),
                                else => unreachable,
                            }
                        },
                        else => |c| builder.appendAssumeCapacity(c),
                    }
                }
            }

            try builder.append(0);
            const str = try p.arena.dupe(u8, builder.items);
            const ptrLoc = @as(u32, @intCast(p.data.items.len));
            try p.data.appendSlice(&@as([2]u32, @bitCast(@intFromPtr(str.ptr))));

            const arrayType = try p.arena.create(Type.Array);
            arrayType.* = .{ .elem = .{ .specifier = .Char }, .len = str.len };

            return try Result.node(p, .{
                .tag = .StringLiteralExpr,
                .type = .{
                    .specifier = .Array,
                    .data = .{ .array = arrayType },
                },
                .first = ptrLoc,
                .second = @intCast(str.len),
            });
        },

        .CharLiteral,
        .CharLiteralUTF_16,
        .CharLiteralUTF_32,
        .CharLiteralWide,
        => {
            if (p.wantConst) {
                return p.todo("char literals");
            }
            return p.todo("ast");
        },

        .FloatLiteral,
        .FloatLiteral_F,
        .FloatLiteral_L,
        => {
            if (p.wantConst) {
                try p.err(.expected_integer_constant_expr);
                return error.ParsingFailed;
            }
            return p.todo("ast");
        },

        .Zero => {
            p.index += 1;
            return Result{ .data = .{ .signed = 0 } };
        },

        .One => {
            p.index += 1;
            return Result{ .data = .{ .signed = 1 } };
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
                return Result{ .ty = .{ .specifier = .ULongLong }, .data = .{ .unsigned = value } };
            }

            p.index += 1;

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
            return p.todo("generic");
        },

        else => return Result{},
    }
}

fn castInt(p: *Parser, val: u64, specs: []const Type.Specifier) Error!Result {
    for (specs) |spec| {
        const ty = Type{ .specifier = spec };
        const isUnsigned = ty.isUnsignedInt(p.pp.compilation);
        const tySize = ty.sizeof(p.pp.compilation);

        if (isUnsigned) {
            switch (tySize) {
                2 => if (val < std.math.maxInt(u16)) return Result{ .ty = ty, .data = .{ .unsigned = val } },
                4 => if (val < std.math.maxInt(u32)) return Result{ .ty = ty, .data = .{ .unsigned = val } },
                8 => if (val < std.math.maxInt(u64)) return Result{ .ty = ty, .data = .{ .unsigned = val } },
                else => unreachable,
            }
        } else {
            switch (tySize) {
                2 => if (val < std.math.maxInt(i16)) return Result{ .ty = ty, .data = .{ .signed = @as(i16, @intCast(val)) } },
                4 => if (val < std.math.maxInt(i32)) return Result{ .ty = ty, .data = .{ .signed = @as(i32, @intCast(val)) } },
                8 => if (val < std.math.maxInt(i64)) return Result{ .ty = ty, .data = .{ .signed = @as(i64, @intCast(val)) } },
                else => unreachable,
            }
        }
    }

    return Result{ .ty = .{ .specifier = .ULongLong }, .data = .{ .unsigned = val } };
}
