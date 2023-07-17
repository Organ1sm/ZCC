const std = @import("std");
const Compilation = @import("Compilation.zig");
const Source = @import("Source.zig");
const Token = @import("Token.zig").Token;
const TokenType = @import("TokenType.zig").TokenType;
const Lexer = @import("Lexer.zig");
const Preprocessor = @import("Preprocessor.zig");
const AST = @import("AST.zig");
const Type = @import("Type.zig");
const Diagnostics = @import("Diagnostics.zig");

const TokenIndex = AST.TokenIndex;
const TagIndex = AST.TagIndex;
const Allocator = std.mem.Allocator;
const Qualifiers = Type.Qualifiers;

const Parser = @This();
const Error = Compilation.Error || error{ParsingFailed};

pp: *Preprocessor,
tokens: []const Token,
index: u32 = 0,
wantConst: bool = false,
inFunc: bool = false,

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
    node: void,

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

fn eat(p: *Parser, id: TokenType) ?Token {
    const token = p.getCurrToken();

    if (token.id == id) {
        p.index += 1;
        return token;
    } else return null;
}

fn getCurrToken(p: *Parser) Token {
    return p.tokens[p.index];
}

fn expectToken(p: *Parser, id: TokenType) Error!Token {
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
    p.index += 1;

    return token;
}

fn expectClosing(p: *Parser, opening: Token, id: TokenType) Error!void {
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
                .sourceId = opening.source,
                .locStart = opening.loc.start,
            });
        }

        return e;
    };
}

fn err(p: *Parser, tag: Diagnostics.Tag) Compilation.Error!void {
    const token = p.getCurrToken();

    try p.pp.compilation.diag.add(.{
        .tag = tag,
        .sourceId = token.source,
        .locStart = token.loc.start,
    });
}

fn todo(p: *Parser, msg: []const u8) Error {
    const token = p.getCurrToken();

    try p.pp.compilation.diag.add(.{
        .tag = .todo,
        .sourceId = token.source,
        .locStart = token.loc.start,
        .extra = .{ .str = msg },
    });

    return error.ParsingFailed;
}

pub fn parse(p: *Parser) Compilation.Error!void {
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
    const firstToken = p.getCurrToken();
    const declSpecRaw = try p.declSpecifier();

    if (declSpecRaw == null) {
        if (p.inFunc) return false;

        switch (firstToken.id) {
            .Asterisk, .LParen, .Identifier => {},
            else => return false,
        }
    }

    const declSpec = declSpecRaw orelse blk: {
        var d = DeclSpec{};
        try p.defaultTypeSpec(&d.type);
        break :blk d;
    };

    const first = (try p.initDeclarator(declSpec.type)) orelse {
        // TODO: return if enum, struct union
        try p.pp.compilation.diag.add(.{
            .tag = .missing_declaration,
            .sourceId = firstToken.source,
            .locStart = firstToken.loc.start,
        });

        // eat ';'
        _ = try p.expectToken(.Semicolon);

        return true;
    };

    // check for funtion definition
    if (first.d.type.specifier == .Function and first.initializer == null and (p.getCurrToken().id == .LBrace or first.k_r_function)) {
        if (!p.inFunc) {
            try p.err(.func_not_in_root);
        }

        const inFunc = p.inFunc;
        p.inFunc = true;
        defer p.inFunc = inFunc;

        _ = try p.compoundStmt();
        return true;
    }

    while (p.eat(.Comma)) |_| {
        _ = (try p.initDeclarator(declSpec.type)) orelse {
            try p.err(.expected_ident_or_l_paren);
        };
    }

    _ = try p.expectToken(.Semicolon);
    return true;
}

/// staticAssert : keyword_static_assert '(' constExpr ',' STRING_LITERAL ')' ';'
fn staticAssert(p: *Parser) Error!bool {
    const curToken = p.getCurrToken();
    if (p.eat(.KeywordStaticAssert) == null)
        return false;

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
        try msg.appendSlice(p.pp.tokSlice(str));

        try p.pp.compilation.diag.add(.{
            .tag = .static_assert_failure,
            .sourceId = curToken.source,
            .locStart = curToken.loc.start,
            .extra = .{ .str = try p.pp.arena.allocator().dupe(u8, msg.items) },
        });
    }

    return true;
}

pub const DeclSpec = struct {
    storageClass: union(enum) {
        auto,
        @"extern",
        register,
        static,
        typedef,
        none,
    } = .none,

    threadLocal: bool = false,
    @"inline": bool = false,
    noreturn: bool = false,
    type: Type = .{},
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
    var d = DeclSpec{};
    var any: bool = false;

    while (true) {
        if (try p.typeSpec(&d.type)) {
            any = true;
            continue;
        }

        const token = p.getCurrToken();
        switch (token.id) {
            .KeywordTypedef,
            .KeywordExtern,
            .KeywordStatic,
            .KeywordAuto,
            .KeywordRegister,
            => {
                if (d.storageClass != .none) {
                    try p.pp.compilation.diag.add(.{
                        .tag = .multiple_storage_class,
                        .sourceId = token.source,
                        .locStart = token.loc.start,
                        .extra = .{ .str = @tagName(d.storageClass) },
                    });

                    return error.ParsingFailed;
                }

                switch (token.id) {
                    .KeywordTypedef => d.storageClass = .typedef,
                    .KeywordExtern => d.storageClass = .@"extern",
                    .KeywordStatic => d.storageClass = .static,
                    .KeywordAuto => d.storageClass = .auto,
                    .KeywordRegister => d.storageClass = .register,
                    else => unreachable,
                }
            },

            .KeywordThreadLocal => {
                if (d.threadLocal) {
                    try p.err(.duplicate_declspec);
                }

                d.threadLocal = true;
            },

            .KeywordInline => {
                if (d.@"inline") {
                    try p.err(.duplicate_declspec);
                }

                d.@"inline" = true;
            },

            .KeywordNoreturn => {
                if (d.noreturn) {
                    try p.err(.duplicate_declspec);
                }

                d.noreturn = true;
            },
            else => break,
        }

        p.index += 1;
        any = true;
    }

    if (!any) return null;
    try p.defaultTypeSpec(&d.type);
    return d;
}

const InitDeclarator = struct { d: Declarator, initializer: ?TagIndex, k_r_function: bool = false };
/// initDeclarator : declarator ('=' initializer)?
fn initDeclarator(p: *Parser, baseType: Type) Error!?InitDeclarator {
    const d = (try p.declarator(baseType)) orelse return null;
    if (p.eat(.Equal)) |_| {
        return p.todo("initializer");
    }

    return InitDeclarator{ .d = d, .initializer = null };
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
fn typeSpec(p: *Parser, ty: *Type) Error!bool {
    var any = false;

    while (true) {
        if (try p.typeQual(ty)) {
            any = true;
            continue;
        }

        const token = p.getCurrToken();
        switch (token.id) {
            .KeywordVoid => {
                if (ty.specifier != .None) {
                    return p.cannotCombineSpec(ty.specifier);
                }

                ty.specifier = .Void;
            },

            .KeywordBool => {
                if (ty.specifier != .None) {
                    return p.cannotCombineSpec(ty.specifier);
                }
                ty.specifier = .Bool;
            },

            .KeywordChar => switch (ty.specifier) {
                .None => ty.specifier = .Char,
                .Unsigned => ty.specifier = .UChar,
                .Signed => ty.specifier = .SChar,
                else => return p.cannotCombineSpec(ty.specifier),
            },

            .KeywordShort => switch (ty.specifier) {
                .None, .Signed => ty.specifier = .Short,
                .Unsigned => ty.specifier = .UShort,
                .UShort, .Short => try p.duplicateSpecifier("short"),
                else => return p.cannotCombineSpec(ty.specifier),
            },

            .KeywordInt => switch (ty.specifier) {
                .None, .Signed => ty.specifier = .Int,
                .Unsigned => ty.specifier = .UInt,

                .UShort,
                .Long,
                .ULong,
                .LongLong,
                .ULongLong,
                => {}, // TODO warn duplicate int specifier

                .Int, .UInt => try p.duplicateSpecifier("int"),
                else => return p.cannotCombineSpec(ty.specifier),
            },
            .KeywordLong => switch (ty.specifier) {
                .None, .Signed => ty.specifier = .Long,
                .Long => ty.specifier = .LongLong,
                .Unsigned => ty.specifier = .ULong,
                .ULong => ty.specifier = .ULongLong,
                .LongLong, .ULongLong => try p.duplicateSpecifier("long"),
                else => return p.cannotCombineSpec(ty.specifier),
            },

            .KeywordSigned => switch (ty.specifier) {
                .None => ty.specifier = .Signed,
                .Char => ty.specifier = .SChar,

                .Int,
                .Short,
                .Long,
                .LongLong,
                => {}, // TODO warn duplicate signed specifier

                .SChar, .Signed => try p.duplicateSpecifier("signed"),
                else => return p.cannotCombineSpec(ty.specifier),
            },

            .KeywordUnsigned => switch (ty.specifier) {
                .None => ty.specifier = .Unsigned,
                .Char => ty.specifier = .UChar,

                .UInt,
                .UShort,
                .ULong,
                .ULongLong,
                => {}, // TODO warn duplicate unsigned specifier

                .UChar, .Unsigned => try p.duplicateSpecifier("unsigned"),
                else => return p.cannotCombineSpec(ty.specifier),
            },
            .KeywordFloat => switch (ty.specifier) {
                .LongDouble,
                .ComplexLongDouble,
                .ComplexLong,
                .ComplexDouble,
                .Double,
                => {}, // TODO warn duplicate float

                .Long => ty.specifier = .LongDouble, // TODO long float is invalid
                .None => ty.specifier = .Float,
                .Complex => ty.specifier = .ComplexFloat,

                .ComplexFloat, .Float => try p.duplicateSpecifier("float"),
                else => return p.cannotCombineSpec(ty.specifier),
            },

            .KeywordDouble => switch (ty.specifier) {
                .Long => ty.specifier = .LongDouble,
                .ComplexLong => ty.specifier = .ComplexLongDouble,
                .ComplexFloat, .Complex => ty.specifier = .ComplexDouble,
                .Float, .None => ty.specifier = .Double,

                .LongDouble,
                .ComplexLongDouble,
                .ComplexDouble,
                .Double,
                => try p.duplicateSpecifier("double"),
                else => return p.cannotCombineSpec(ty.specifier),
            },

            .KeywordComplex => switch (ty.specifier) {
                .Long => ty.specifier = .ComplexLong,
                .Float => ty.specifier = .ComplexFloat,
                .Double => ty.specifier = .ComplexDouble,
                .LongDouble => ty.specifier = .ComplexLongDouble,
                .None => ty.specifier = .Complex,

                .ComplexLong,
                .Complex,
                .ComplexFloat,
                .ComplexDouble,
                .ComplexLongDouble,
                => try p.duplicateSpecifier("_Complex"),
                else => return p.cannotCombineSpec(ty.specifier),
            },

            .KeywordAtomic => return p.todo("atomic types"),
            .KeywordEnum => return p.todo("enum types"),
            .KeywordStruct => return p.todo("struct types"),
            .KeywordUnion => return p.todo("union types"),
            .KeywordAlignas => {
                if (ty.alignment != 0)
                    try p.duplicateSpecifier("alignment");

                const lp = try p.expectToken(.LParen);
                const otherType = try p.typeName();
                try p.expectClosing(lp, .RParen);

                ty.alignment = otherType.alignment;
            },
            else => break,
        }

        p.index += 1;
        any = true;
    }

    return any;
}

fn cannotCombineSpec(p: *Parser, spec: Type.Specifier) Error {
    const token = p.getCurrToken();
    try p.pp.compilation.diag.add(.{
        .tag = .cannot_combine_spec,
        .sourceId = token.source,
        .locStart = token.loc.start,
        .extra = .{ .str = spec.toString() },
    });

    return error.ParsingFailed;
}

fn duplicateSpecifier(p: *Parser, spec: []const u8) Error!void {
    const token = p.getCurrToken();
    try p.pp.compilation.diag.add(.{
        .tag = .duplicate_declspec,
        .sourceId = token.source,
        .locStart = token.loc.start,
        .extra = .{ .str = spec },
    });
}

fn defaultTypeSpec(p: *Parser, ty: *Type) Error!void {
    switch (ty.specifier) {
        .None => {
            ty.specifier = .Int;
            try p.err(.missing_type_specifier);
        },
        .Unsigned => ty.specifier = .UInt,
        .Signed => ty.specifier = .Int,
        .Complex => ty.specifier = .ComplexDouble,
        .ComplexLong => ty.specifier = .ComplexLongDouble,
        else => {},
    }
}
/// recordSpec
///  : (keyword_struct | keyword_union) IDENTIFIER? { recordDecl* }
///  | (keyword_struct | keyword_union) IDENTIFIER
fn recordSpec(p: *Parser) Error!TagIndex {
    return p.todo("recordSpec");
}
/// recordDecl
///  : specQual+ (recordDeclarator (',' recordDeclarator)*)? ;
///  | staticAssert
fn recordDecl(p: *Parser) Error!TagIndex {
    return p.todo("recordDecl");
}

/// recordDeclarator : declarator (':' constExpr)?
fn recordDeclarator(p: *Parser) Error!TagIndex {
    return p.todo("recordDeclarator");
}

// specQual : typeSpec | typeQual | alignSpec
fn specQual(p: *Parser) Error!Type {
    var ty = Type{};
    if (try p.typeSpec(&ty)) {
        try p.defaultTypeSpec(&ty);
        return ty;
    }

    try p.err(.expected_type);

    return error.ParsingFailed;
}

/// enumSpec
///  : keyword_enum IDENTIFIER? { enumerator (',' enumerator)? ',') }
///  | keyword_enum IDENTIFIER
fn enumSpec(p: *Parser) Error!TagIndex {
    return p.todo("enumSpec");
}

/// enumerator : IDENTIFIER ('=' constExpr)
fn enumerator(p: *Parser) Error!TagIndex {
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
                        .extra = .{ .str = ty.specifier.toString() },
                    })
                else if (ty.qual.restrict)
                    try p.duplicateSpecifier("restrict")
                else
                    ty.qual.restrict = true;
            },

            .KeywordConst => {
                if (ty.qual.@"const")
                    try p.duplicateSpecifier("const")
                else
                    ty.qual.@"const" = true;
            },

            .KeywordVolatile => {
                if (ty.qual.@"volatile")
                    try p.duplicateSpecifier("volatile")
                else
                    ty.qual.@"volatile" = true;
            },

            .KeywordAtomic => {
                if (ty.qual.atomic)
                    try p.duplicateSpecifier("atomic")
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

const Declarator = struct { name: []const u8, type: Type };
/// declarator: pointer? directDeclarator
fn declarator(p: *Parser, baseType: Type) Error!?Declarator {
    var ty = baseType;
    const sawPtr = try p.pointer(&ty);

    if (p.eat(.Identifier)) |some| {
        const name = p.pp.tokSlice(some);
        try p.directDeclarator(&ty, false);

        return Declarator{ .name = name, .type = ty };
    } else if (p.eat(.LParen)) |lp| {
        const res = try p.declarator(ty);

        try p.expectClosing(lp, .RParen);
        var unwrapped = res orelse return null;

        try p.directDeclarator(&unwrapped.type, false);
        return unwrapped;
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
fn directDeclarator(p: *Parser, ty: *Type, isAbstract: bool) Error!void {
    _ = ty;
    _ = isAbstract;
    while (true) {
        if (p.eat(.LBracket)) |_| {
            return p.todo("array byte");
        } else if (p.eat(.LParen)) |_| {
            return p.todo("function type");
        } else return;
    }
}

/// pointer : '*' typeQual* pointer?
fn pointer(p: *Parser, ty: *Type) Error!bool {
    _ = ty;
    if (p.eat(.Asterisk) == null)
        return false;

    return p.todo("pointer");
}

/// paramDecls : paramDecl (',' paramDecl)* (',' '...')
/// paramDecl : declSpec (declarator | abstractDeclarator)
fn paramDecls(p: *Parser) Error!TagIndex {
    return p.todo("paramDecls");
}

/// typeName : specQual+ abstractDeclarator
fn typeName(p: *Parser) Error!Type {
    _ = try p.specQual();

    return p.todo("typeName");
}

/// abstractDeclarator
/// : pointer
/// : pointer? ('(' abstractDeclarator ')')? directAbstractDeclarator*
fn abstractDeclarator(p: *Parser) Error!TagIndex {
    return p.todo("abstractDeclarator");
}

/// initializer
///  : assignExpr
///  | '{' initializerItems '}'
fn initializer(p: *Parser) Error!TagIndex {
    return p.todo("initializer");
}

/// initializerItems : designation? initializer  (',' designation? initializer)? ','?
fn initializerItems(p: *Parser) Error!TagIndex {
    return p.todo("initializerItems");
}

/// designation : designator+ '='
fn designation(p: *Parser) Error!TagIndex {
    return p.todo("designation");
}

/// designator
///  : '[' constExpr ']'
///  | '.' identifier
fn designator(p: *Parser) Error!TagIndex {
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
fn stmt(p: *Parser) Error!TagIndex {
    return p.todo("stmt");
}

/// labeledStmt
/// : IDENTIFIER ':' stmt
/// | keyword_case constExpr ':' stmt
/// | keyword_default ':' stmt
fn labeledStmt(p: *Parser) Error!TagIndex {
    return p.todo("labeledStmt");
}

/// compoundStmt : '{' ( decl | staticAssert |stmt)* '}'
fn compoundStmt(p: *Parser) Error!TagIndex {
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
        _ = try p.typeName();
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
