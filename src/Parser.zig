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

fn eat(p: *Parser, id: TokenType) bool {
    if (p.tokens[p.index].id == id) {
        p.index += 1;
        return true;
    }

    return false;
}

fn expectToken(p: *Parser, id: TokenType) Error!void {
    const token = p.tokens[p.index];
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
    }

    return error.ParsingFailed;
}

fn err(p: *Parser, tag: Diagnostics.Tag) Error {
    const token = p.tokens[p.index];

    try p.pp.compilation.diag.add(.{
        .tag = tag,
        .sourceId = token.source,
        .locStart = token.loc.start,
    });

    return error.ParsingFailed;
}

fn todo(p: *Parser, msg: []const u8) Error {
    const token = p.tokens[p.index];

    try p.pp.compilation.diag.add(.{
        .tag = .todo,
        .sourceId = token.source,
        .locStart = token.loc.start,
        .extra = .{ .str = msg },
    });

    return error.ParsingFailed;
}

pub fn parse(p: *Parser) Error!AST {
    try p.err(.expected_expr);
    p.index += 1;
}

// ====== declarations ======

/// decl
///  : declSpec (initDeclarator ( ',' initDeclarator)*)? ';'
///  | declSpec declarator declarator* compoundStmt
///  | staticAssert
/// staticAssert : keyword_static_assert '(' constExpr ',' STRING_LITERAL ')' ';'
fn staticAssert(p: *Parser) Error!bool {
    const curToken = p.tokens[p.index];
    if (!p.eat(curToken))
        return false;

    try expectToken(.LParen);

    const start = p.index;
    const res = try p.constExpr();
    const end = p.index;

    try p.expectToken(.Comma);
    const str = p.tokens[p.index];

    try p.expectToken(.StringLiteral);
    try p.expectToken(.RParen);

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

        return true;
    }
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
fn declSpec(p: *Parser) Error!?DeclSpec {
    var d = DeclSpec{};
    var any: bool = false;

    while (true) {
        const token = p.tokens[p.index];
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

                any = true;
                continue;
            },

            .KeywordThreadLocal => {
                if (d.threadLocal) {
                    try p.err(.duplicate_declsepc);
                }

                d.threadLocal = true;
                any = true;
                continue;
            },

            .KeywordInline => {
                if (d.@"inline") {
                    try p.err(.duplicate_declsepc);
                }

                d.@"inline" = true;
                any = true;
                continue;
            },

            .KeywordNoreturn => {
                if (d.noreturn) {
                    try p.err(.duplicate_declsepc);
                }

                d.noreturn = true;
                any = true;
                continue;
            },
        }

        if (!any) return null;

        if (d.type.specifier == .none) {
            d.type.specifier = .int;
            try p.err(.missing_type_specifier);
        }

        return d;
    }
}

/// initDeclarator : declarator ('=' initializer)?
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
///  | atomic-type-specifier
///  | recordSpec
///  | enumSpec
///  | typedef  // IDENTIFIER
/// alignSpec : keyword_alignas '(' typeName ')'
fn typeSpec(p: *Parser, ty: *Type) Error!bool {
    _ = ty;
    return p.todo("typeSpec");
}
/// recordSpec
///  : (keyword_struct | keyword_union) IDENTIFIER? { recordDecl* }
///  | (keyword_struct | keyword_union) IDENTIFIER
/// recordDecl
///  : specQual+ (recordDeclarator (',' recordDeclarator)*)? ;
///  | staticAssert
/// recordDeclarator : declarator (':' constExpr)?

// specQual : typeSpec | typeQual | alignSpec
fn specQual(p: *Parser) Error!Type {
    return p.todo("specQual");
}

/// enumSpec
///  : keyword_enum IDENTIFIER? { enumerator (',' enumerator)? ',') }
///  | keyword_enum IDENTIFIER
/// enumerator : IDENTIFIER ('=' constExpr)
/// atomicTypeSpec : keyword_atomic '(' typeName ')'
/// typeQual : keyword_const | keyword_restrict | keyword_volatile | keyword_atomic
fn typeQual(p: *Parser, quals: *Qualifiers) Error!bool {
    _ = quals;
    return p.todo("typeQual");
}
/// declarator: pointer? directDeclarator

/// directDeclarator
///  : IDENTIFIER
///  | '(' declarator ')'
///  | directDeclarator '[' typeQual* assignExpr? ']'
///  | directDeclarator '[' keyword_static typeQual* assignExpr ']'
///  | directDeclarator '[' typeQual* keyword_static assignExpr ']'
///  | directDeclarator '[' typeQual* '*' ']'
///  | directDeclarator '(' paramDecls ')'
///  | directDeclarator '(' (IDENTIFIER (',' IDENTIFIER))? ')'
/// pointer : '*' typeQual* pointer?
/// paramDecls : paramDecl (',' paramDecl)* (',' '...')
/// paramDecl : declSpec (declarator | abstractDeclarator?)
/// typeName : specQual+ abstractDeclarator?
/// abstractDeclarator
/// : pointer
/// | pointer? directAbstractDeclarator
/// directAbstractDeclarator
///  : '(' abstractDeclarator ')'
///  | directAbstractDeclarator? '[' typeQual* assignExpr? ']'
///  | directAbstractDeclarator? '[' keyword_static typeQual* assignExpr ']'
///  | directAbstractDeclarator? '[' typeQual+ keyword_static assignExpr ']'
///  | directAbstractDeclarator? '[' '*' ']'
///  | directAbstractDeclarator? '(' paramDecls? ')'
/// initializer
///  : assignExpr
///  | '{' initializerItems '}'
/// initializerItems : designation? initializer  (',' designation? initializer)? ','?
/// designation : designator+ '='
/// designator
///  : '[' constExpr ']'
///  | '.' identifier

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

/// compoundStmt : '{' ( decl | stmt)* '}'
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
    if (!p.eat(.QuestionMark))
        return cond;

    const thenExpr = try p.expr();
    try p.expectToken(.Colon);
    const elseExpr = try p.conditionalExpr();

    if (p.wantConst or cond != .node)
        return if (cond.getBool()) thenExpr else elseExpr;

    return p.todo("ast");
}

/// logicalOrExpr : logicalAndExpr ('||' logicalAndExpr)*
fn logicalOrExpr(p: *Parser) Error!Result {
    var lhs = try p.logicalAndExpr();
    while (p.eat(.PipePipe)) {
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
    while (p.eat(.AmpersandAmpersand)) {
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
    while (p.eat(.Pipe)) {
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
    while (p.eat(.Caret)) {
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
    while (p.eat(.Ampersand)) {
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
        if (!eq and !p.eat(.BangEqual)) break;
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
        const le = lt or p.eat(.AngleBracketLeftEqual);
        const gt = le or p.eat(.AngleBracketRight);
        const ge = gt or p.eat(.AngleBracketRightEqual);
        if (!ge) break;
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
        const shr = shl or p.eat(.AngleBracketAngleBracketRight);
        if (!shr) break;
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
        const minus = plus or p.eat(.Minus);
        if (!minus) break;
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
        const div = mul or p.eat(.Slash);
        const percent = div or p.eat(.Percent);
        if (!percent) break;
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
    if (!p.eat(.LParen)) {
        return p.unaryExpr();
    }
    return p.todo("cast");
}

/// unaryExpr
///  : primaryExpr suffixExpr*
///  | ('&' | '*' | '+' | '-' | '~' | '!' | '++' | '--') castExpr
///  | keyword_sizeof unaryExpr
///  | keyword_sizeof '(' type_name ')'
///  | keyword_alignof '(' type_name ')'
fn unaryExpr(p: *Parser) Error!Result {
    switch (p.tokens[p.index].id) {
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
    switch (p.tokens[p.index].id) {
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
    if (p.eat(.LParen)) {
        const e = try p.expr();
        try p.expectToken(.RParen);
        return e;
    }
    switch (p.tokens[p.index].id) {
        .Identifier => return p.todo("ast"),

        .StringLiteral,
        .StringLiteralUTF_8,
        .StringLiteralUTF_16,
        .StringLiteralUTF_32,
        .StringLiteralWide,
        => {
            if (p.wantConst) {
                return p.err(.expected_integer_constant_expr);
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
                return p.err(.expected_integer_constant_expr);
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

        else => return p.err(.expected_expr),
    }
}
