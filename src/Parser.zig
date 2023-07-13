const std = @import("std");
const Compilation = @import("Compilation.zig");
const Source = @import("Source.zig");
const Token = @import("Token.zig").Token;
const TokenType = @import("TokenType.zig").TokenType;
const Lexer = @import("Lexer.zig");
const Preprocessor = @import("Preprocessor.zig");

const Allocator = std.mem.Allocator;

const Parser = @This();
const Error = Allocator.Error || error{ParsingFailed};

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
    node: void, // TODO

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
    const current = p.tokens[p.index].id;
    if (current != id) {
        switch (current) {
            .Invalid => return p.failFmt("expected '{s}', found invalid bytes", .{@tagName(id)}),
            else => return p.failFmt("expected '{s}', found '{s}'", .{ @tagName(id), @tagName(current) }),
        }
    }

    p.index += 1;
}

fn failFmt(p: *Parser, comptime fmt: []const u8, args: anytype) Error {
    const tok = p.tokens[p.index];
    const source = p.pp.compilation.getSource(tok.source);

    const lcs = source.lineColString(tok.loc);

    p.pp.compilation.printErrStart(source.path, lcs);
    std.debug.print(fmt, args);
    p.pp.compilation.printErrEnd(lcs);

    return error.ParsingFailed;
}

fn fail(p: *Parser, msg: []const u8) Error {
    return p.failFmt("{s}", .{msg});
}

/// expr : assignExpr (',' assignExpr)*
fn expr(p: *Parser) Error!Result {
    return p.fail("TODO expr");
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

/// conditionalExpr : logicalOrExpr ('?' expression ':' conditionalExpr)?
fn conditionalExpr(p: *Parser) Error!Result {
    const cond = try p.logicalOrExpr();
    if (!p.eat(.QuestionMark))
        return cond;

    const thenExpr = try p.expr();
    try p.expectToken(.Colon);
    const elseExpr = try p.conditionalExpr();

    if (p.wantConst or cond != .node)
        return if (cond.getBool()) thenExpr else elseExpr;

    return p.fail("TODO");
}

/// logicalOrExpr : logicalAndExpr ('||' logicalAndExpr)*
fn logicalOrExpr(p: *Parser) Error!Result {
    var lhs = try p.logicalAndExpr();
    while (p.eat(.PipePipe)) {
        const rhs = try p.logicalAndExpr();

        if (p.wantConst or (lhs != .node and rhs != .node)) {
            lhs = Result{ .bool = lhs.getBool() or rhs.getBool() };
        } else return p.fail("TOOD");
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
        } else return p.fail("TODO ast");
    }
    return lhs;
}

/// orExpr : xorExpr ('|' xorExpr)*
fn orExpr(p: *Parser) Error!Result {
    var lhs = try p.xorExpr();
    while (p.eat(.Pipe)) {
        const rhs = try p.xorExpr();

        if (p.wantConst or (lhs != .node and rhs != .node)) {
            return p.fail("TODO or constExpr");
        } else return p.fail("TODO ast");
    }
    return lhs;
}

/// xorExpr : andExpr ('^' andExpr)*
fn xorExpr(p: *Parser) Error!Result {
    var lhs = try p.andExpr();
    while (p.eat(.Caret)) {
        const rhs = try p.andExpr();

        if (p.wantConst or (lhs != .node and rhs != .node)) {
            return p.fail("TODO xor constExpr");
        } else return p.fail("TODO ast");
    }
    return lhs;
}

/// andExpr : eqExpr ('&' eqExpr)*
fn andExpr(p: *Parser) Error!Result {
    var lhs = try p.eqExpr();
    while (p.eat(.Ampersand)) {
        const rhs = try p.eqExpr();

        if (p.wantConst or (lhs != .node and rhs != .node)) {
            return p.fail("TODO and constExpr");
        } else return p.fail("TODO ast");
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
            return p.fail("TODO equality constExpr");
        }
        return p.fail("TODO ast");
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
            return p.fail("TODO comp constExpr");
        }
        return p.fail("TODO ast");
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
            return p.fail("TODO shift constExpr");
        }
        return p.fail("TODO ast");
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
            return p.fail("TODO shift constExpr");
        }
        return p.fail("TODO ast");
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
            return p.fail("TODO mul constExpr");
        }
        return p.fail("TODO ast");
    }
    return lhs;
}

/// castExpr :  ( '(' type_name ')' )* unaryExpr
fn castExpr(p: *Parser) Error!Result {
    if (!p.eat(.LParen)) {
        return p.unaryExpr();
    }
    return p.fail("TODO cast");
}

/// unary_operator
///  : '&'
///  | '*'
///  | '+'
///  | '-'
///  | '~'
///  | '!'
/// unaryExpr
///  : primaryExpr suffixExpr*
///  | '++' unaryExpr
///  | '--' unaryExpr
///  | unary_operator castExpr
///  | keyword_sizeof unaryExpr
///  | keyword_sizeof '(' type_name ')'
fn unaryExpr(p: *Parser) Error!Result {
    switch (p.tokens[p.index].id) {
        .Ampersand => return p.fail("TODO unaryExpr ampersand"),
        .Asterisk => return p.fail("TODO unaryExpr asterisk"),
        .Plus => return p.fail("TODO unaryExpr plus"),
        .Minus => return p.fail("TODO unaryExpr minus"),
        .PlusPlus => return p.fail("TODO unary inc"),
        .MinusMinus => return p.fail("TODO unary dec"),
        .Tilde => return p.fail("TODO unaryExpr tilde"),
        .Bang => {
            p.index += 1;
            const lhs = try p.unaryExpr();
            if (p.wantConst or lhs != .node) {
                return Result{ .bool = !lhs.getBool() };
            }
            return p.fail("TODO ast");
        },
        .KeywordSizeof => return p.fail("TODO unaryExpr sizeof"),
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
        .LBracket => return p.fail("TODO array access"),
        .LParen => return p.fail("TODO call"),
        .Period => return p.fail("TODO member access"),
        .Arrow => return p.fail("TODO member access pointer"),
        .PlusPlus => return p.fail("TODO post inc"),
        .MinusMinus => return p.fail("TODO post dec"),

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
fn primaryExpr(p: *Parser) Error!Result {
    if (p.eat(.LParen)) {
        const e = try p.expr();
        try p.expectToken(.RParen);
        return e;
    }
    switch (p.tokens[p.index].id) {
        .Identifier => return p.fail("TODO ast"),

        .StringLiteral,
        .StringLiteralUTF_8,
        .StringLiteralUTF_16,
        .StringLiteralUTF_32,
        .StringLiteralWide,
        => {
            if (p.wantConst) {
                return p.fail("expression is not an integer constant expression");
            }
            return p.fail("TODO ast");
        },

        .CharLiteral,
        .CharLiteralUTF_16,
        .CharLiteralUTF_32,
        .CharLiteralWide,
        => {
            if (p.wantConst) {
                return p.fail("TODO char literals");
            }
            return p.fail("TODO ast");
        },

        .FloatLiteral,
        .FloatLiteral_F,
        .FloatLiteral_L,
        => {
            if (p.wantConst) {
                return p.fail("expression is not an integer constant expression");
            }
            return p.fail("TODO ast");
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
                return p.fail("TODO integer literals");
            }
            return p.fail("TODO ast");
        },
        else => return p.failFmt("expected literal, identifier or grouped expression, found '{s}'", .{@tagName(p.tokens[p.index].id)}),
    }
}
