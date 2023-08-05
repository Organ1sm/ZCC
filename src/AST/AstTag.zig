const Declaration = @import("AST.zig").Declaration;
const Statement = @import("AST.zig").Statement;
const Expression = @import("AST.zig").Expression;

pub const Tag = enum(u8) {
    /// Only appears at index 0 and reaching it is always a result of a bug.
    Invalid,

    // ====== Decl ======

    // function prototype
    FnProto,
    StaticFnProto,
    InlineFnProto,
    InlineStaticFnProto,
    NoreturnFnProto,
    NoreturnStaticFnProto,
    NoreturnInlineFnProto,
    NoreturnInlineStaticFnProto,

    // function definition
    FnDef,
    StaticFnDef,
    InlineFnDef,
    InlineStaticFnDef,
    NoreturnFnDef,
    NoreturnStaticFnDef,
    NoreturnInlineFnDef,
    NoreturnInlineStaticFnDef,

    // parameter
    ParamDecl,
    RegisterParamDecl,

    // variable declaration
    Var,
    ExternVar,
    StaticVar,
    RegisterVar,
    ThreadlocalVar,
    ThreadlocalExternVar,
    ThreadlocalStaticVar,

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

    // {first; second;} first and second may be null
    CompoundStmtTwo,

    // { data }
    CompoundStmt,

    /// if (a) do_1 else do_2
    IfThenElseStmt,

    /// if (a); else do_2;
    IfElseStmt,

    /// if (a) do_1; do_1 may be null
    IfThenStmt,

    /// switch (first): second
    SwitchStmt,

    /// case first: second
    CaseStmt,

    /// default: first
    DefaultStmt,

    /// while (first) second
    WhileStmt,

    /// do second while(first);
    DoWhileStmt,

    /// for (data[..]; data[len-3]; data[len-2]) data[len-1]
    ForDeclStmt,

    /// for(;;;) first
    ForEverStmt,

    /// for(data[first]; data[first + 1]; data[first+2]) second
    ForStmt,

    /// goto first;
    GotoStmt,
    ContinueStmt,
    BreakStmt,
    ReturnStmt,

    // ====== Expr ======
    /// lhs , rhs
    CommaExpr,
    /// lhs ?: rhs
    BinaryCondExpr,
    /// lhs ? data[0] : data[1]
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
    /// first(second) second may be 0
    CallExprOne,
    /// data[0](data[1..])
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
    /// integer literal with 64bit, split in first and second
    IntLiteral,
    /// f32 literal stored in node.first
    FloatLiteral,
    /// f64 literal stored in node.first and node.second
    DoubleLiteral,
    /// data[first][0..second]
    StringLiteralExpr,
    /// TODO
    CompoundLiteralExpr,

    // implicit casts ///
    /// convert T[] to T*
    ArrayToPointer,
    ///  same as deref
    LValueToRValue,
};
