const Declaration = @import("AST.zig").Declaration;
const Statement = @import("AST.zig").Statement;
const Expression = @import("AST.zig").Expression;

pub const Tag = enum(u8) {
    /// Only appears at index 0 and reaching it is always a result of a bug.
    Invalid,

    // ====== Decl ======

    // _Static_assert
    StaticAssert,

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

    // variable declaration
    Var,
    ExternVar,
    StaticVar,
    ThreadlocalVar,
    ThreadlocalExternVar,
    ThreadlocalStaticVar,

    // typdef declaration
    TypeDef,

    // container declarations
    /// { lhs; rhs; }
    StructDeclTwo,
    /// { lhs; rhs; }
    UnionDeclTwo,
    /// { lhs, rhs, }
    EnumDeclTwo,
    /// { range }
    StructDecl,
    /// { range }
    UnionDecl,
    /// { range }
    EnumDecl,

    /// name = node
    EnumFieldDecl,
    /// ty name : node
    /// name == 0 means unnamed
    RecordFieldDecl,
    /// Used when a record has an unnamed record as a field
    IndirectRecordFieldDecl,

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
    BitAndAssignExpr,
    /// lhs ^= rhs
    BitXorAssignExpr,
    /// lhs |= rhs
    BitOrAssignExpr,
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
    /// lhs.member
    MemberAccessExpr,
    /// lhs->member
    MemberAccessPtrExpr,
    /// lhs++
    PostIncExpr,
    /// lhs--
    PostDecExpr,
    /// (lhs)
    ParenExpr,
    /// DeclarationRef
    DeclRefExpr,
    /// Enumeration Ref
    EnumerationRef,
    /// integer literal, always unsigned
    IntLiteral,
    /// f32 literal
    FloatLiteral,
    /// f64 literal
    DoubleLiteral,
    /// tree.string[index][0..len]
    StringLiteralExpr,
    /// sizeof(un?)
    SizeOfExpr,
    /// _Alignof(un?)
    AlignOfExpr,
    /// _Generic(controlling lhs, chosen rhs)
    GenericExprOne,
    /// _Generic(controlling range[0], chosen range[1], rest range[2..])
    GenericExpr,
    /// ty: un
    GenericAssociationExpr,
    // default: un
    GenericDefaultExpr,
    /// __builtin_choose_expr(lhs, data[0], data[1])
    BuiltinChooseExpr,

    /// ====== Initializer expressions ======
    /// { lhs, rhs }
    InitListExprTwo,
    /// { range }
    InitListExpr,
    /// (ty){ un }
    CompoundLiteralExpr,

    // implicit casts
    /// convert T[] to T*
    ArrayToPointer,
    /// Converts an lvalue to an rvalue
    LValueToRValue,
    /// Convert a function type to a pointer to a function
    FunctionToPointer,
    /// Convert a pointer type to a _Bool
    PointerToBool,
    /// Convert a pointer type to an integer type
    PointerToInt,
    /// Convert _Bool to an integer type
    BoolToInt,
    /// Convert _Bool to a floating type
    BoolToFloat,
    /// Convert a _Bool to a pointer; will cause a  warning
    BoolToPointer,
    /// Convert an integer type to _Bool
    IntToBool,
    /// Convert an integer to a floating
    IntToFloat,
    /// Convert an integer type to a pointer type
    IntToPointer,
    /// Convert a floating type to a _Bool
    FloatToBool,
    /// Convert a floating type to an integer
    FloatToInt,
    /// Convert one integer type to another
    IntCast,
    /// Convert one floating type to another
    FloatCast,
    /// Convert pointer to one with same child type but more CV-quals,
    /// OR to appropriately-qualified void *
    /// only appears on the branches of a conditional expr
    QualCast,
    /// Convert type to void; only appears on the branches of a conditional expr
    ToVoid,

    /// Inserted at the end of a function body if no return stmt is found.
    /// ty is the functions return type
    ImplicitReturn,

    /// Inserted in init_list_expr to represent unspecified elements.
    /// data.int contains the amount of elements.
    ArrayFillerExpr,
    /// Inserted in init_list_expr to represent unspecified fields.
    StructFillerExpr,

    /// Convert a literal 0 to a null pointer
    NullToPointer,

    pub fn isImplicit(tag: Tag) bool {
        return switch (tag) {
            .ArrayToPointer,
            .LValueToRValue,
            .FunctionToPointer,
            .PointerToBool,
            .PointerToInt,
            .BoolToInt,
            .BoolToFloat,
            .BoolToPointer,
            .IntToBool,
            .IntToFloat,
            .IntToPointer,
            .FloatToBool,
            .FloatToInt,
            .IntCast,
            .FloatCast,
            .ToVoid,
            .ImplicitReturn,
            .QualCast,
            .NullToPointer,
            .ArrayFillerExpr,
            .StructFillerExpr,
            => true,

            else => false,
        };
    }
};
