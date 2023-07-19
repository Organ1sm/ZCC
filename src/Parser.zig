const std = @import("std");
const Compilation = @import("Compilation.zig");
const Source = @import("Source.zig");
const Token = @import("Token.zig").Token;
const TokenType = @import("TokenType.zig").TokenType;
const Lexer = @import("Lexer.zig");
const Preprocessor = @import("Preprocessor.zig");
const AST = @import("AST.zig");
const Type = @import("Type.zig");
const TypeBuilder = @import("TypeBuilder.zig").Builder;
const Diagnostics = @import("Diagnostics.zig");

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

            .node, .none => unreachable,
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
        try p.pp.compilation.diag.add(.{
            .tag = switch (token.id) {
                .Invalid => .expected_invalid,
                else => .expected_token,
            },
            .sourceId = token.source,
            .locStart = token.loc.start,
            .extra = .{
                .tokenId = .{
                    .expected = id,
                    .actual = token.id,
                },
            },
        });
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
    const token = p.tokens[index];

    try p.pp.compilation.diag.add(.{
        .tag = tag,
        .sourceId = token.source,
        .locStart = token.loc.start,
        .extra = .{ .str = str },
    });
}

fn errToken(p: *Parser, tag: Diagnostics.Tag, index: TokenIndex) Compilation.Error!void {
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
    };

    errdefer p.nodes.deinit(pp.compilation.gpa);

    while (p.eat(.Eof) != null) {
        if (p.staticAssert() catch |er| switch (er) {
            error.ParsingFailed => {
                p.nextExternDecl();
                continue;
            },
            else => |e| return e,
        }) continue;

        if (p.decl() catch |er| switch (er) {
            error.ParsingFailed => {
                p.nextExternDecl();
                continue;
            },
            else => |e| return e,
        }) continue;

        try p.err(.expected_external_decl);
        p.index += 1;
    }

    return AST{
        .comp = pp.compilation,
        .tokens = pp.tokens.items,
        .arena = arena,
        .generated = pp.generated.items,
        .nodes = p.nodes.toOwnedSlice(),
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
fn decl(p: *Parser) Error!bool {
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
        var spec: TypeBuilder = .None;
        try spec.finish(p, &d.type);

        break :blk d;
    };

    const first = (try p.initDeclarator(&declSpec, true)) orelse {
        // TODO: return if enum, struct union
        try p.errToken(.missing_declaration, firstTokenIndex);

        // eat ';'
        _ = try p.expectToken(.Semicolon);

        return true;
    };

    // check for funtion definition
    if (first.d.funcDeclarator and first.initializer == 0 and (p.getCurrToken().id == .LBrace or first.d.oldTypeFunc)) {
        if (!p.inFunc)
            try p.err(.func_not_in_root);

        const inFunc = p.inFunc;
        p.inFunc = true;
        defer p.inFunc = inFunc;

        _ = try p.compoundStmt();
        return true;
    }

    const node = try p.addNode(.{
        .type = first.d.type,
        .tag = .FnProto,
        .first = first.d.name,
    });
    try p.currDeclList.append(node);

    while (p.eat(.Comma)) |_| {
        _ = (try p.initDeclarator(&declSpec, false)) orelse {
            try p.err(.expected_ident_or_l_paren);
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

pub const DeclSpec = struct {
    storageClass: union(enum) {
        auto: TokenIndex,
        @"extern": TokenIndex,
        register: TokenIndex,
        static: TokenIndex,
        typedef: TokenIndex,
        none,
    } = .none,

    threadLocal: ?TokenIndex = null,
    @"inline": ?TokenIndex = null,
    noreturn: ?TokenIndex = null,
    type: Type = .{ .specifier = undefined },

    fn validateParam(d: DeclSpec, p: *Parser, ty: Type) Error!AST.Tag {
        switch (d.storageClass) {
            .none, .register => {},
            .auto, .@"extern", .static, .typedef => |tokenIndex| try p.errTok(.invalid_storage_on_param, tokenIndex),
        }
        if (d.threadLocal) |tokenIndex| try p.errTok(.threadlocal_non_var, tokenIndex);
        if (ty.specifier != .Func) {
            if (d.@"inline") |tokenIndex| try p.errStr(.func_spec_non_func, tokenIndex, "inline");
            if (d.noreturn) |tokenIndex| try p.errStr(.func_spec_non_func, tokenIndex, "_Noreturn");
        }

        return if (d.storageClass == .register) .register_param_decl else .param_decl;
    }
};

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
    var spec: TypeBuilder = .None;

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
                ty.Enum = try p.enumSpec();
                continue;
            },

            .KeywordStruct => {
                try ty.combine(p, .{ .Struct = 0 });
                ty.Struct = try p.recordSpec();
                continue;
            },

            .KeywordUnion => {
                try ty.combine(p, .{ .Union = 0 });
                ty.Union = try p.recordSpec();
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
                if (true) break; // TODO check for typdef identifier
                const typedefType: Type = undefined;
                const new_spec = Type.Builder.fromType(typedefType);

                ty.combine(new_spec) catch |e| switch (e) {
                    error.OutOfMemory => return e,
                    error.ParsingFailed, error.FatalError => {
                        // TODO use typedef decl token
                        try p.errStr(.typedef_is, p.index, new_spec.str());
                        return e;
                    },
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
    var spec: TypeBuilder = .None;
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

        return p.todo("combine ty and res");
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
    return p.todo("paramDecls");
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
        _ = res;

        try p.expectClosing(lp, .RParen);

        var d = Declarator{ .name = 0, .type = ty };

        try p.directDeclarator(&d, false);

        return p.todo("combine ty and res");
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
    return p.todo("initializer");
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
    return p.todo("stmt");
}

/// labeledStmt
/// : IDENTIFIER ':' stmt
/// | keyword_case constExpr ':' stmt
/// | keyword_default ':' stmt
fn labeledStmt(p: *Parser) Error!NodeIndex {
    return p.todo("labeledStmt");
}

/// compoundStmt : '{' ( decl | staticAssert |stmt)* '}'
fn compoundStmt(p: *Parser) Error!NodeIndex {
    return p.todo("compoundStmt");
}

//////////////////////
//       Expr       //
/////////////////////

/// expr : assignExpr (',' assignExpr)*
fn expr(p: *Parser) Error!Result {
    return p.todo("expr");
}

/// assignExpr
///  : conditionalExpr
///  | unaryExpr ('=' | '*=' | '/=' | '%=' | '+=' | '-=' | '<<=' | '>>=' | '&=' | '^=' | '|=') assignExpr
fn assignExpr(p: *Parser) Error!Result {
    _ = p;
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
                const suffix = try p.suffixExpr(&lhs);
                if (suffix == .none) break;
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
fn suffixExpr(p: *Parser, lhs: *Result) Error!Result {
    _ = lhs;
    switch (p.getCurrToken().id) {
        .LBracket => return p.todo("array access"),
        .LParen => return p.todo("call"),
        .Period => return p.todo("member access"),
        .Arrow => return p.todo("member access pointer"),
        .PlusPlus => return p.todo("post inc"),
        .MinusMinus => return p.todo("post dec"),

        else => return Result{ .none = {} },
    }
}

/// argumentExprList : assignExpr (',' assignExpr)*
fn argumentExprList(p: *Parser) Error!Result {
    _ = p;
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
    switch (p.getCurrToken().id) {
        .Identifier => return p.todo("ast"),

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
            return p.todo("ast");
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

        else => {
            try p.err(.expected_expr);
            return error.ParsingFailed;
        },
    }
}
