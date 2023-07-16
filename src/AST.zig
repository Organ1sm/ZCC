const std = @import("std");
const Type = @import("Type.zig");
const Token = @import("Token.zig").Token;

const AST = @This();

pub const TokenIndex = u32;
pub const TagIndex = u32;

tokens: []const Token,
tags: []const Tag,
stmts: []const Stmt,
exprs: []const Expr,
rootDecls: []const Tag,

pub const Tag = enum(u8) {
    LabeledStmt,
    CompoundStmt,
    IfStmt,
    IfElseStmt,
    SwitchStmt,
    WhileStmt,
    DoWhileStmt,
    ForStmt,
    GotoStmt,
    ContinueStmt,
    BreakStmt,
    ReturnStmt,
    ExprStmt,

    // ====== Expr ======

    /// lhs , rhs
    CommaExpr,
    /// lhs ?: rhs
    InaryCondExpr,
    /// TODO
    CondExpr,
    /// lhs = rhs
    AssignExpr,
    /// lhs *= rhs
    MulAssignExpr,
    /// lhs /= rhs
    DivAssignExpr,
    /// lhs %= rhs
    ModAssignExpr,
    /// lhs += rhs
    AddAssignExpr,
    /// lhs -= rhs
    SubAssignExpr,
    /// lhs <<= rhs
    ShlAssignExpr,
    /// lhs >>= rhs
    ShrAssignExpr,
    /// lhs &= rhs
    AndAssignExpr,
    /// lhs ^= rhs
    XorAssignExpr,
    /// lhs |= rhs
    OrAssignExpr,
    /// lhs || rhs
    BoolOrExpr,
    /// lhs && rhs
    BoolAndExpr,
    /// lhs | rhs
    BitOrExpr,
    /// lhs ^ rhs
    BitXorExpr,
    /// lhs & rhs
    BitAndExpr,
    /// lhs == rhs
    EqualExpr,
    /// lhs != rhs
    NotEqualExpr,
    /// lhs < rhs
    LessThanExpr,
    /// lhs <= rhs
    LessThanEqualExpr,
    /// lhs > rhs
    GreaterThanExpr,
    /// lhs >= rhs
    GreaterThanEqualExpr,
    /// lhs << rhs
    ShlExpr,
    /// lhs >> rhs
    ShrExpr,
    /// lhs + rhs
    AddExpr,
    /// lhs - rhs
    SubExpr,
    /// lhs * rhs
    MulExpr,
    /// lhs / rhs
    DivExpr,
    /// lhs % rhs
    ModExpr,
    /// Explicit (type)lhs
    CastExpr,
    /// &lhs
    AddrOfExpr,
    /// *lhs
    DerefExpr,
    /// +lhs
    PlusExpr,
    /// -lhs
    NegateExpr,
    /// ~lhs
    BitNotExpr,

    /// !lhs
    BoolNotExpr,
    /// ++lhs
    PreIncExpr,
    /// --lhs
    PreDecExpr,
    /// lhs[rhs]  lhs is pointer/array type, rhs is integer type
    ArrayAccessExpr,
    /// TODO
    CallExpr,
    /// lhs.rhs rhs is a TokenIndex of the identifier
    MemberAccessExpr,
    /// lhs->rhs rhs is a TokenIndex of the identifier
    MemberAccessPtrExpr,
    /// lhs++
    PostIncExpr,
    /// lhs--
    PostDecExpr,
    /// lhs is a TokenIndex of the identifier
    DeclRefExpr,
    /// TODO
    IntegerLiteralExpr,
    /// TODO
    FloatLiteralExpr,
    /// TODO
    CharLiteralExpr,
    /// TODO
    StringLiteralExpr,
    /// TODO
    CompoundLiteralExpr,

    /// Asserts that the tag is an expression.
    pub fn isLeftvalue(tag: Tag) bool {
        return switch (tag) {
            .CommaExpr,
            .BinaryCondExpr,
            .CondExpr,
            .AssignExpr,
            .MulAssignExpr,
            .DivAssignExpr,
            .ModAssignExpr,
            .AddAssignExpr,
            .SubAssignExpr,
            .ShlAssignExpr,
            .ShrAssignExpr,
            .AndAssignExpr
                .XorAssignExpr,
            .OrAssignExpr,
            .BoolOrExpr,
            .BoolAndExpr,
            .BitOrExpr,
            .BitXorExpr,
            .BitAndExpr,
            .EqualExpr,
            .NotEqualExpr,
            .LessThanExpr,
            .LessThanEqualExpr,
            .GreaterThanExpr,
            .GreaterThanEqualExpr,
            .ShlExpr,
            .ShrExpr,
            .AddExpr,
            .SubExpr,
            .MulExpr,
            .DivExpr,
            .ModExpr,
            .CastExpr,
            .AddrOfExpr,
            .DerefExpr,
            .PlusExpr,
            .NegateExpr,
            .BitNotExpr,
            .BoolNotExpr,
            .PreIncExpr,
            .PreDecExpr,
            .ArrayAccessExpr,
            .CallExpr,
            => false,

            .DeclRefExpr,
            .StringLiteralExpr,
            .CompoundLiteralExpr,
            // .member_access_expr, if lhs.isLval()
            .MemberAccessPtrExpr,
            .Deref,
            .ArrayAccessExpr,
            => true,

            else => unreachable,
        };
    }
};

pub const Decl = struct {};

pub const Stmt = struct {};

pub const Expr = struct {
    type: Type,
};
