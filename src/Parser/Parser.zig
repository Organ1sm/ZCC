const std = @import("std");
const Compilation = @import("../Basic/Compilation.zig");
const Source = @import("../Basic/Source.zig");
const Token = @import("../Lexer/Token.zig").Token;
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
const TokenIndex = AST.TokenIndex;
const NodeIndex = AST.NodeIndex;
const Allocator = std.mem.Allocator;
const NodeList = std.ArrayList(NodeIndex);

const Parser = @This();
pub const Error = Compilation.Error || error{ParsingFailed};

pp: *Preprocessor,
arena: Allocator,
tokens: []const Token,
nodes: AST.Node.List = .{},
data: NodeList,
scopes: std.ArrayList(Scope),
index: u32 = 0,
wantConst: bool = false,
inFunc: bool = false,
currDeclList: *NodeList,

pub const Result = union(enum) {
    none,
    bool: bool,
    u8: u8,
    i8: i8,
    u16: u16,
    i16: i16,
    u32: u32,
    i32: i32,
    u64: u64,
    i64: i64,
    lVal: NodeIndex,
    node: NodeIndex,

    pub fn getBool(res: Result) bool {
        return switch (res) {
            .bool => |v| v,

            .u8 => |v| v != 0,
            .i8 => |v| v != 0,
            .u16 => |v| v != 0,
            .i16 => |v| v != 0,
            .u32 => |v| v != 0,
            .i32 => |v| v != 0,
            .u64 => |v| v != 0,
            .i64 => |v| v != 0,

            .node, .none, .lVal => unreachable,
        };
    }

    fn expect(res: Result, p: *Parser) Error!void {
        if (res == .none) {
            try p.errToken(.expected_expr, p.index);
            return error.ParsingFailed;
        }
    }

    fn node(p: *Parser, n: AST.Node) !Result {
        return Result{ .node = try p.addNode(n) };
    }

    fn leftValue(p: *Parser, n: AST.Node) !Result {
        return Result{ .lVal = try p.addNode(n) };
    }

    fn ty(res: Result, p: *Parser) Type {
        return switch (res) {
            .none => unreachable,
            .node, .lVal => |n| p.nodes.items(.type)[n],
            else => .{ .specifier = .Int }, // TODO get actual type
        };
    }

    fn toNode(res: Result, p: *Parser) !NodeIndex {
        return switch (res) {
            .none => 0,
            .node, .lVal => |n| n,
            else => p.todo("number to ast"),
        };
    }
};

fn eat(p: *Parser, id: TokenType) ?TokenIndex {
    const token = p.getCurrToken();

    if (token.id == id) {
        defer p.index += 1;
        return p.index;
    } else return null;
}

pub fn getCurrToken(p: *Parser) Token {
    return p.tokens[p.index];
}

fn expectToken(p: *Parser, id: TokenType) Error!TokenIndex {
    const token = p.getCurrToken();
    if (token.id != id) {
        try p.errExtra(
            switch (token.id) {
                .Invalid => .expected_invalid,
                else => .expected_token,
            },
            p.index,
            .{
                .tokenId = .{
                    .expected = id,
                    .actual = token.id,
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
            const oToken = p.tokens[opening];
            try p.pp.compilation.diag.add(.{
                .tag = switch (id) {
                    .RParen => .to_match_paren,
                    .RBrace => .to_match_brace,
                    .RBracket => .to_match_bracket,
                    else => unreachable,
                },
                .sourceId = oToken.source,
                .locStart = oToken.loc.start,
            });
        }

        return e;
    };
}

pub fn errStr(p: *Parser, tag: Diagnostics.Tag, index: TokenIndex, str: []const u8) Compilation.Error!void {
    @setCold(true);
    return p.errExtra(tag, index, .{ .str = str });
}

pub fn errExtra(p: *Parser, tag: Diagnostics.Tag, index: TokenIndex, extra: Diagnostics.Message.Extra) Compilation.Error!void {
    @setCold(true);
    const token = p.tokens[index];

    try p.pp.compilation.diag.add(.{
        .tag = tag,
        .sourceId = token.source,
        .locStart = token.loc.start,
        .extra = extra,
    });
}

pub fn errToken(p: *Parser, tag: Diagnostics.Tag, index: TokenIndex) Compilation.Error!void {
    @setCold(true);

    const token = p.tokens[index];
    try p.pp.compilation.diag.add(.{
        .tag = tag,
        .sourceId = token.source,
        .locStart = token.loc.start,
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

fn addNode(p: *Parser, node: AST.Node) Allocator.Error!NodeIndex {
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

fn findSymbol(p: *Parser, nameToken: TokenIndex) ?Scope {
    const name = p.pp.tokSlice(p.tokens[nameToken]);
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
        .tokens = pp.tokens.items,
        .currDeclList = &rootDecls,
        .scopes = std.ArrayList(Scope).init(pp.compilation.gpa),
        .data = NodeList.init(pp.compilation.gpa),
    };
    defer p.scopes.deinit();
    defer p.data.deinit();

    errdefer p.nodes.deinit(pp.compilation.gpa);

    while (p.eat(.Eof) != null) {
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
        .tokens = pp.tokens.items,
        .arena = arena,
        .generated = pp.generated.items,
        .nodes = p.nodes.toOwnedSlice(),
        .data = data,
        .rootDecls = try rootDecls.toOwnedSlice(),
    };
}

fn nextExternDecl(p: *Parser) void {
    var parens: u32 = 0;

    while (p.index < p.tokens.len) : (p.index += 1) {
        switch (p.getCurrToken().id) {
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
///  | declSpec declarator declarator* compoundStmt
///  | staticAssert
fn parseDeclaration(p: *Parser) Error!bool {
    const firstTokenIndex = p.index;
    var declSpec = if (try p.declSpecifier()) |some|
        some
    else blk: {
        if (p.inFunc) return false;

        switch (p.tokens[firstTokenIndex].id) {
            .Asterisk, .LParen, .Identifier => {},
            else => return false,
        }

        var d = DeclSpec{};
        var spec: TypeBuilder = .{};
        try spec.finish(p, &d.type);

        break :blk d;
    };

    var initD = (try p.initDeclarator(&declSpec, true)) orelse {
        // TODO: return if enum, struct union
        try p.errToken(.missing_declaration, firstTokenIndex);

        // eat ';'
        _ = try p.expectToken(.Semicolon);

        return true;
    };

    // check for funtion definition
    if (initD.d.funcDeclarator and initD.initializer == 0 and (p.getCurrToken().id == .LBrace or initD.d.oldTypeFunc)) {
        if (p.inFunc)
            try p.err(.func_not_in_root);

        const inFunc = p.inFunc;
        p.inFunc = true;
        defer p.inFunc = inFunc;

        const node = try p.addNode(.{
            .type = initD.d.type,
            .tag = try declSpec.validateFnDef(p),
            .first = initD.d.name,
        });
        try p.scopes.append(.{ .symbol = .{
            .name = p.pp.tokSlice(p.tokens[initD.d.name]),
            .node = node,
            .nameToken = initD.d.name,
        } });

        const body = try p.compoundStmt();
        p.nodes.items(.second)[node] = body.?;

        try p.currDeclList.append(node);

        return true;
    }

    while (true) {
        const node = try p.addNode(.{
            .type = initD.d.type,
            .tag = try declSpec.validate(p, initD.d.type, initD.initializer != 0),
            .first = initD.d.name,
        });
        try p.currDeclList.append(node);

        if (declSpec.storageClass == .typedef) {
            try p.scopes.append(.{ .typedef = .{
                .name = p.pp.tokSlice(p.tokens[initD.d.name]),
                .node = node,
                .nameToken = initD.d.name,
            } });
        } else {
            try p.scopes.append(.{ .symbol = .{
                .name = p.pp.tokSlice(p.tokens[initD.d.name]),
                .node = node,
                .nameToken = initD.d.name,
            } });
        }

        if (p.eat(.Comma) == null)
            break;

        initD = (try p.initDeclarator(&declSpec, false)) orelse {
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
    const start = p.index;
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
        for (p.tokens[start..end], 0..) |token, i| {
            if (i != 0)
                try msg.append(' ');
            try msg.appendSlice(p.pp.tokSlice(token));
        }

        try msg.appendSlice("' ");
        try msg.appendSlice(p.pp.tokSlice(p.tokens[str]));
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
        switch (token.id) {
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
                    switch (token.id) {
                        .KeywordTypedef,
                        .KeywordAuto,
                        .KeywordRegister,
                        => try p.errStr(.cannot_combine_spec, p.index, token.id.lexeMe().?),

                        else => {},
                    }
                }

                switch (token.id) {
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
fn initDeclarator(p: *Parser, declSpec: *DeclSpec, allowOldStyle: bool) Error!?InitDeclarator {
    var init = InitDeclarator{ .d = (try p.declarator(declSpec.type, allowOldStyle)) orelse return null };

    if (p.eat(.Equal)) |_| {
        if (declSpec.storageClass == .typedef or declSpec.type.specifier == .Func)
            try p.err(.illegal_initializer);

        if (declSpec.storageClass == .@"extern") {
            try p.err(.extern_initializer);
            declSpec.storageClass = .none;
        }

        init.initializer = try p.initializer();
    }

    return init;
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

        const token = p.getCurrToken();
        switch (token.id) {
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
                const typedef = p.findTypedef(p.pp.tokSlice(token)) orelse break;
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
        const token = p.getCurrToken();
        switch (token.id) {
            .KeywordRestrict => {
                if (ty.specifier != .Pointer)
                    try p.pp.compilation.diag.add(.{
                        .tag = .restrict_non_pointer,
                        .sourceId = token.source,
                        .locStart = token.loc.start,
                        .extra = .{ .str = TypeBuilder.fromType(ty.*).toString() },
                    })
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
    funcDeclarator: bool = false,
    oldTypeFunc: bool = false,
};

/// declarator: pointer? directDeclarator
fn declarator(p: *Parser, baseType: Type, allowOldStyle: bool) Error!?Declarator {
    var ty = baseType;
    const sawPtr = try p.pointer(&ty);

    if (p.eat(.Identifier)) |name| {
        var d = Declarator{ .name = name, .type = ty };
        try p.directDeclarator(&d, allowOldStyle);
        return d;
    } else if (p.eat(.LParen)) |lp| {
        const res = try p.declarator(ty, allowOldStyle);

        try p.expectClosing(lp, .RParen);
        const unwrapped = res orelse {
            try p.err(.expected_ident_or_l_paren);
            return null;
        };

        var d = Declarator{ .name = unwrapped.name, .type = ty };
        try p.directDeclarator(&d, allowOldStyle);

        d.type = try unwrapped.type.combine(d.type, p);
        return d;
    }

    if (!sawPtr) {
        return null;
    } else {
        try p.err(.expected_ident_or_l_paren);
        return null;
    }
}

/// directDeclarator
///  : '[' typeQual* assignExpr? ']'
///  | '[' keyword_static typeQual* assignExpr ']'
///  | '[' typeQual+ keyword_static assignExpr ']'
///  | '[' typeQual* '*' ']'
///  | '(' paramDecls ')'
///  | '(' (IDENTIFIER (',' IDENTIFIER))? ')'
/// directAbstractDeclarator
///  : '[' typeQual* assignExpr? ']'
///  | '[' keyword_static typeQual* assignExpr ']'
///  | '[' typeQual+ keyword_static assignExpr ']'
///  | '[' '*' ']'
///  | '(' paramDecls? ')'
/// if declarator.name == 0 then the declarator is assumed to be abstract
fn directDeclarator(p: *Parser, d: *Declarator, allowOldStyle: bool) Error!void {
    _ = allowOldStyle;
    while (true) {
        if (p.eat(.LBracket)) |lb| {
            try p.expectClosing(lb, .RBracket);
            return p.todo("array byte");
        } else if (p.eat(.LParen)) |lp| {
            switch (d.type.specifier) {
                .Func,
                .VarArgsFunc,
                => try p.err(.func_cannot_return_func),

                .Array,
                .StaticArray,
                => try p.err(.func_cannot_return_array),

                else => {},
            }
            d.funcDeclarator = true;
            if (p.getCurrToken().id == .Ellipsis) {
                try p.err(.param_before_var_args);
                return error.ParsingFailed;
            }

            if (try p.paramDecls()) |params| {
                var func = try p.arena.create(Type.Function);
                func.* = .{
                    .returnType = d.type,
                    .paramTypes = params,
                };

                var funcType = Type{
                    .specifier = .Func,
                    .data = .{ .func = func },
                };

                if (p.eat(.Ellipsis)) |_| {
                    funcType.specifier = .VarArgsFunc;
                }

                try p.expectClosing(lp, .RParen);
                d.type = funcType;
                continue;
            }

            if (p.eat(.RParen)) |_| {
                var func = try p.arena.create(Type.Function);
                func.* = .{
                    .returnType = d.type,
                    .paramTypes = &.{},
                };

                var funcType = Type{
                    .specifier = .VarArgsFunc,
                    .data = .{ .func = func },
                };
                d.type = funcType;
                continue;
            }

            return p.todo("old style function type");
        } else return;
    }
}

/// pointer : '*' typeQual* pointer?
fn pointer(p: *Parser, ty: *Type) Error!bool {
    const start = p.index;
    while (p.eat(.Asterisk)) |_| {
        const elemType = try p.arena.create(Type);
        elemType.* = ty.*;
        var ptrType = Type{
            .specifier = .Pointer,
            .data = .{ .subType = elemType },
        };
        _ = try p.typeQual(&ptrType);
        ty.* = ptrType;
    }

    return start != p.index;
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
        var paramType = (try p.abstractDeclarator(paramDeclSpec.type)) orelse
            if (try p.declarator(paramDeclSpec.type, false)) |some|
        blk: {
            nameToken = some.name;
            break :blk some.type;
        } else paramDeclSpec.type;

        if (paramType.specifier == .Func or paramType.specifier == .VarArgsFunc) {
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
                if (p.getCurrToken().id != .RParen) {
                    try p.err(.void_only_param);
                    if (paramType.qual.any())
                        try p.err(.void_param_qualified);

                    return error.ParsingFailed;
                }

                return &[0]NodeIndex{};
            }

            try p.err(.void_must_be_first_param);

            return error.ParsingFailed;
        }

        const param = try p.addNode(.{
            .tag = try paramDeclSpec.validateParam(p, paramType),
            .type = paramType,
            .first = nameToken,
        });
        try params.append(param);

        if (p.eat(.Comma) == null)
            break;

        if (p.getCurrToken().id == .Ellipsis)
            break;
    }

    return try p.arena.dupe(NodeIndex, params.items);
}

/// typeName : specQual+ abstractDeclarator
fn typeName(p: *Parser) Error!?Type {
    var ty = (try p.specQual()) orelse return null;
    return (try p.abstractDeclarator(ty)) orelse return ty;
}

/// abstractDeclarator
/// : pointer
/// : pointer? ('(' abstractDeclarator ')')? directAbstractDeclarator*
fn abstractDeclarator(p: *Parser, baseType: Type) Error!?Type {
    const start = p.index;
    var ty = baseType;
    _ = try p.pointer(&ty);

    if (p.eat(.LParen)) |lp| blk: {
        const res = (try p.abstractDeclarator(ty)) orelse {
            p.index -= 1;
            break :blk;
        };

        try p.expectClosing(lp, .RParen);

        var d = Declarator{ .name = 0, .type = ty };
        try p.directDeclarator(&d, false);

        return try res.combine(d.type, p);
    }

    var d = Declarator{ .name = 0, .type = ty };
    try p.directDeclarator(&d, false);

    if (p.index == start)
        return null;

    return ty;
}

/// initializer
///  : assignExpr
///  | '{' initializerItems '}'
fn initializer(p: *Parser) Error!NodeIndex {
    if (p.eat(.LBrace)) |_| {
        return p.todo("compound initializer");
    }

    const res = try p.assignExpr();
    try res.expect(p);

    return res.node;
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
    if (try p.compoundStmt()) |some|
        return some;

    const e = try p.expr();
    if (e == .node) {
        _ = try p.expectToken(.Semicolon);
        return e.node;
    }

    if (p.eat(.Semicolon)) |_| return 0;

    try p.err(.expected_stmt);
    return error.ParsingFailed;
}

/// labeledStmt
/// : IDENTIFIER ':' stmt
/// | keyword_case constExpr ':' stmt
/// | keyword_default ':' stmt
fn labeledStmt(p: *Parser) Error!NodeIndex {
    return p.todo("labeledStmt");
}

/// compoundStmt : '{' ( decl | staticAssert |stmt)* '}'
fn compoundStmt(p: *Parser) Error!?NodeIndex {
    const lBrace = p.eat(.LBrace) orelse return null;

    var statements = NodeList.init(p.pp.compilation.gpa);
    defer statements.deinit();

    const savedDS = p.currDeclList;
    defer p.currDeclList = savedDS;
    p.currDeclList = &statements;

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

        try statements.append(s);
    }

    switch (statements.items.len) {
        0 => return try p.addNode(.{
            .tag = .CompoundStmtTwo,
            .type = .{ .specifier = .Void },
        }),
        1 => return try p.addNode(.{
            .tag = .CompoundStmtTwo,
            .type = .{ .specifier = .Void },
            .first = statements.items[0],
        }),
        2 => return try p.addNode(.{
            .tag = .CompoundStmtTwo,
            .type = .{ .specifier = .Void },
            .first = statements.items[0],
            .second = statements.items[1],
        }),
        else => {
            const range = try p.addList(statements.items);
            return try p.addNode(.{
                .tag = .CompoundStmt,
                .type = .{ .specifier = .Void },
                .first = range.start,
                .second = range.end,
            });
        },
    }
}

fn nextStmt(p: *Parser, lBrace: TokenIndex) !void {
    var parens: u32 = 0;
    while (p.index < p.tokens.len) : (p.index += 1) {
        switch (p.getCurrToken().id) {
            .LParen, .LBrace, .LBracket => parens += 1,
            .RParen, .RBracket => if (parens != 0) {
                parens -= 1;
            },

            .RBrace => if (parens == 0)
                return
            else {
                parens -= 1;
            },

            // TODO: uncomment once implemented
            // .KeywordFor,
            // .KeywordWhile,
            // .KeywordDo,
            // .KeywordIf,
            // .KeywordGoto,
            // .KeywordSwitch,
            // .KeywordContinue,
            // .KeywordBreak,
            // .KeywordReturn,
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

    return p.conditionalExpr();
}

/// conditionalExpr : logicalOrExpr ('?' expression? ':' conditionalExpr)?
fn conditionalExpr(p: *Parser) Error!Result {
    const cond = try p.logicalOrExpr();
    if (p.eat(.QuestionMark) == null)
        return cond;

    const thenExpr = try p.expr();
    _ = try p.expectToken(.Colon);
    const elseExpr = try p.conditionalExpr();

    if (p.wantConst or cond != .node)
        return if (cond.getBool()) thenExpr else elseExpr;

    return p.todo("ast");
}

/// logicalOrExpr : logicalAndExpr ('||' logicalAndExpr)*
fn logicalOrExpr(p: *Parser) Error!Result {
    var lhs = try p.logicalAndExpr();
    while (p.eat(.PipePipe)) |_| {
        const rhs = try p.logicalAndExpr();

        if (p.wantConst or (lhs != .node and rhs != .node)) {
            lhs = Result{ .bool = lhs.getBool() or rhs.getBool() };
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

        if (p.wantConst or (lhs != .node and rhs != .node)) {
            lhs = Result{ .bool = lhs.getBool() and rhs.getBool() };
        } else return p.todo("ast");
    }
    return lhs;
}

/// orExpr : xorExpr ('|' xorExpr)*
fn orExpr(p: *Parser) Error!Result {
    var lhs = try p.xorExpr();
    while (p.eat(.Pipe)) |_| {
        const rhs = try p.xorExpr();

        if (p.wantConst or (lhs != .node and rhs != .node)) {
            return p.todo("or constExpr");
        } else return p.todo("ast");
    }
    return lhs;
}

/// xorExpr : andExpr ('^' andExpr)*
fn xorExpr(p: *Parser) Error!Result {
    var lhs = try p.andExpr();
    while (p.eat(.Caret)) |_| {
        const rhs = try p.andExpr();

        if (p.wantConst or (lhs != .node and rhs != .node)) {
            return p.todo("xor constExpr");
        } else return p.todo("ast");
    }
    return lhs;
}

/// andExpr : eqExpr ('&' eqExpr)*
fn andExpr(p: *Parser) Error!Result {
    var lhs = try p.eqExpr();
    while (p.eat(.Ampersand)) |_| {
        const rhs = try p.eqExpr();

        if (p.wantConst or (lhs != .node and rhs != .node)) {
            return p.todo("and constExpr");
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
        const rhs = try p.compExpr();

        if (p.wantConst or (lhs != .node and rhs != .node)) {
            return p.todo("equality constExpr");
        }

        return p.todo("ast");
    }
    return lhs;
}

/// compExpr : shiftExpr (('<' | '<=' | '>' | '>=') shiftExpr)*
fn compExpr(p: *Parser) Error!Result {
    const lhs = try p.shiftExpr();
    while (true) {
        const lt = p.eat(.AngleBracketLeft);
        const le = lt orelse p.eat(.AngleBracketLeftEqual);
        const gt = le orelse p.eat(.AngleBracketRight);
        const ge = gt orelse p.eat(.AngleBracketRightEqual);
        if (ge == null) break;
        const rhs = try p.shiftExpr();

        if (p.wantConst or (lhs != .node and rhs != .node)) {
            return p.todo("comp constExpr");
        }

        return p.todo("ast");
    }
    return lhs;
}

/// shiftExpr : addExpr (('<<' | '>>') addExpr)*
fn shiftExpr(p: *Parser) Error!Result {
    const lhs = try p.addExpr();
    while (true) {
        const shl = p.eat(.AngleBracketAngleBracketLeft);
        const shr = shl orelse p.eat(.AngleBracketAngleBracketRight);
        if (shr == null) break;
        const rhs = try p.addExpr();

        if (p.wantConst or (lhs != .node and rhs != .node)) {
            return p.todo("shift constExpr");
        }

        return p.todo("ast");
    }
    return lhs;
}

/// addExpr : mulExpr (('+' | '-') mulExpr)*
fn addExpr(p: *Parser) Error!Result {
    const lhs = try p.mulExpr();
    while (true) {
        const plus = p.eat(.Plus);
        const minus = plus orelse p.eat(.Minus);
        if (minus == null) break;
        const rhs = try p.mulExpr();

        if (p.wantConst or (lhs != .node and rhs != .node)) {
            return p.todo("shift constExpr");
        }
        return p.todo("ast");
    }
    return lhs;
}

/// mulExpr : castExpr (('*' | '/' | '%') castExpr)*´
fn mulExpr(p: *Parser) Error!Result {
    const lhs = try p.castExpr();
    while (true) {
        const mul = p.eat(.Plus);
        const div = mul orelse p.eat(.Slash);
        const percent = div orelse p.eat(.Percent);
        if (percent == null) break;
        const rhs = try p.castExpr();

        if (p.wantConst or (lhs != .node and rhs != .node)) {
            return p.todo("mul constExpr");
        }
        return p.todo("ast");
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
    switch (p.getCurrToken().id) {
        .Ampersand => return p.todo("unaryExpr ampersand"),
        .Asterisk => return p.todo("unaryExpr asterisk"),
        .Plus => return p.todo("unaryExpr plus"),
        .Minus => return p.todo("unaryExpr minus"),
        .PlusPlus => return p.todo("unary inc"),
        .MinusMinus => return p.todo("unary dec"),
        .Tilde => return p.todo("unaryExpr tilde"),
        .Bang => {
            p.index += 1;
            const lhs = try p.unaryExpr();
            if (p.wantConst or lhs != .node) {
                return Result{ .bool = !lhs.getBool() };
            }
            return p.todo("ast");
        },
        .KeywordSizeof => return p.todo("unaryExpr sizeof"),
        else => {
            var lhs = try p.primaryExpr();
            while (true) {
                const suffix = try p.suffixExpr(lhs);
                if (suffix == .none) break;
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
    switch (p.getCurrToken().id) {
        .LBracket => return p.todo("array access"),
        .LParen => {
            const lParen = p.index;
            p.index += 1;
            const lhsType = lhs.ty(p);
            const ty = lhsType.isCallable() orelse {
                try p.errStr(.not_callable, lParen, TypeBuilder.fromType(lhsType).toString());
                return error.ParsingFailed;
            };
            var args = NodeList.init(p.pp.compilation.gpa);
            defer args.deinit();

            for (ty.data.func.paramTypes, 0..) |paramDecl, i| {
                _ = paramDecl;
                if (i != 0) _ = try p.expectToken(.Comma);
                // TODO coerce type
                // const param_ty = p.nodes.items(.ty)[param_decl];
                const arg = try p.assignExpr();
                try arg.expect(p);
                try args.append(try arg.toNode(p));
            }
            if (ty.specifier == .VarArgsFunc) blk: {
                if (args.items.len != 0)
                    _ = p.eat(.Comma) orelse break :blk;

                while (true) {
                    // TODO coerce type
                    const arg = try p.assignExpr();
                    try arg.expect(p);
                    try args.append(try arg.toNode(p));
                    _ = p.eat(.Comma) orelse break;
                }
            }
            try p.expectClosing(lParen, .RParen);

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

        else => return Result.none,
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

    const token = p.getCurrToken();
    switch (token.id) {
        .Identifier => {
            const nameToken = p.index;
            p.index += 1;

            const sym = p.findSymbol(nameToken) orelse {
                if (p.getCurrToken().id == .LParen) {
                    // implicitly declare simple functions as like `puts("foo")`;
                    const name = p.pp.tokSlice(p.tokens[nameToken]);
                    try p.errStr(.implicit_func_decl, nameToken, name);

                    const funcType = try p.arena.create(Type.Function);
                    funcType.* = .{ .returnType = .{ .specifier = .Int }, .paramTypes = &.{} };
                    const ty: Type = .{ .specifier = .VarArgsFunc, .data = .{ .func = funcType } };
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
                try p.errToken(.undeclared_identifier, nameToken);
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

            const start = p.index;
            var width: ?u8 = null;

            while (true) {
                switch (p.getCurrToken().id) {
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

            for (p.tokens[start..p.index]) |tk| {
                var slice = p.pp.tokSlice(tk);
                slice = slice[std.mem.indexOf(u8, slice, "\"").? .. slice.len - 1];

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
            return Result{ .u32 = 0 };
        },

        .One => {
            p.index += 1;
            return Result{ .u32 = 1 };
        },

        .IntegerLiteral,
        .IntegerLiteral_U,
        .IntegerLiteral_L,
        .IntegerLiteral_LU,
        .IntegerLiteral_LL,
        .IntegerLiteral_LLU,
        => {
            if (p.wantConst) {
                return p.todo("integer literals");
            }
            return p.todo("ast");
        },
        .KeywordGeneric => {
            return p.todo("generic");
        },

        else => return Result{ .none = {} },
    }
}
