const Declaration = @import("AST.zig").Declaration;
const Statement = @import("AST.zig").Statement;
const Expression = @import("AST.zig").Expression;

pub const Tag = enum(u8) {
    /// Only appears at index 0 and reaching it is always a result of a bug.
    invalid,

    // ====== Decl ======

    // function prototype
    FnProto,
    ExternFnProto,
    StaticFnProto,
    InlineFnProto,
    InlineExternFnProto,
    InlineStaticFnProto,
    NoreturnFnProto,
    NoreturnExternFnProto,
    NoreturnStaticFnProto,
    NoreturnInlineFnProto,
    NoreturnInlineExternFnProto,
    NoreturnInlineStaticFnProto,

    // function definition
    FnDef,
    ExternFnDef,
    StaticFnDef,
    InlineFnDef,
    InlineExternFnDef,
    InlineStaticFnDef,
    NoreturnFnDef,
    NoreturnExternFnDef,
    NoreturnStaticFnDef,
    NoreturnInlineFnDef,
    NoreturnInlineExternFnDef,
    NoreturnInlineStaticFnDef,

    // parameter
    ParamDecl,
    RegisterParamDecl,

    // variable declaration
    Var,
    AutoVar,
    ExternVar,
    StaticVar,
    RegisterVar,
    ThreadlocalVar,
    ThreadlocalAutoVar,
    ThreadlocalExternVar,
    ThreadlocalStaticVar,
    ThreadlocalRegisterVar,

    // typdef declaration
    TypeDef,

    // container type forward declarations
    StructForward,
    UnionForward,
    EnumForawrd,

    // container type definitions
    StructDef,
    UnionDef,
    EnumDef,

    // ====== Stmt ======
    LabeledStmt,
    CompoundStmt,
    IfStmt,
    SwitchStmt,
    WhileStmt,
    DoWhileStmt,
    ForStmt,
    GotoStmt,
    ContinueStmt,
    BreakStmt,
    ReturnStmt,

    // ====== Expr ======
    /// lhs , rhs
    CommaExpr,
    /// lhs ?: rhs
    BinaryCondExpr,
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
            .AndAssignExpr,
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

    pub fn Type(comptime tag: Tag) ?type {
        return switch (tag) {
            .Invalid => unreachable,
            .FnProto,
            .ExternFnProto,
            .StaticFnProto,
            .InlineFnProto,
            .InlineExternFnProto,
            .InlineStaticFnProto,
            => Declaration.FnProto,

            .FnDef,
            .ExternFnDef,
            .StaticFnDef,
            .InlineFnDef,
            .InlineExternFnDef,
            .InlineStaticFnDef,
            => Declaration.FnDef,

            .ParamDecl, .RegisterParamDecl => Declaration.Param,

            .Var,
            .AutoVar,
            .ExternVar,
            .StaticVar,
            .RegisterVar,
            .ThreadlocalVar,
            .ThreadlocalAutoVar,
            .ThreadlocalExternVar,
            .ThreadlocalStaticVar,
            .ThreadlocalRegisterVar,
            => Declaration.Var,

            .TypeDef => Declaration.Typedef,

            .StructForward,
            .UnionForward,
            => Declaration.RecordForward,

            .EnumForawrd => Declaration.EnumForward,

            .StructDef,
            .UnionDef,
            => Declaration.Record,

            .EnumDef => Declaration.Enum,
            .FieldDecl => Declaration.Field,

            .LabeledStmt => Statement.Labeled,
            .CompoundStmt => Statement.Compound,
            .IfStmt => Statement.If,
            .SwitchStmt => Statement.Switch,
            .WhileStmt, .DoWhileStmt => Statement.While,
            .ForStmt => Statement.For,
            .GotoStmt => Statement.Goto,
            .ContinueStmt, .BreakStmt => null,
            .ReturnStmt => Statement.Return,

            .CommaExpr,
            .BinaryCondExpr,
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
            => Expression.BinOp,

            else => @panic("TODO: Tag.Type()"),
        };
    }
};
