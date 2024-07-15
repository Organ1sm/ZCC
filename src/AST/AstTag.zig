pub const Tag = enum(u8) {
    /// Must appear at index 0. 
    /// Also used as the tag for __builtin_types_compatible_p arguments, since the arguments are types
    /// Reaching it is always the result of a bug.
    Invalid,

    // ====== Decl ======

    // _Static_assert
    StaticAssert,

    // function prototype
    FnProto,
    StaticFnProto,
    InlineFnProto,
    InlineStaticFnProto,

    // function definition
    FnDef,
    StaticFnDef,
    InlineFnDef,
    InlineStaticFnDef,

    // variable declaration
    Var,
    ExternVar,
    StaticVar,
    // same as static_var, used for __func__, __FUNCTION__ and __PRETTY_FUNCTION__
    ImplicitStaticVar,
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
    /// struct Name;
    StructForwardDecl,
    /// union Name;
    UnionForwardDecl,
    /// enum Name;
    EnumForwardDecl,

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

    /// if (a) do_1; do_1 may be null
    IfThenStmt,

    /// switch (first): second
    SwitchStmt,

    /// case first: second
    CaseStmt,

    /// case data[body]...data[body + 1]: condition expr
    CaseRangeStmt,

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
    /// goto *un;
    ComputedGotoStmt,
    ContinueStmt,
    /// break; first and second unused
    BreakStmt,
    /// null statement (just a semicolon); first and second unused
    NullStmt,
    /// return first; first may be null
    ReturnStmt,

    // ====== Expr ======
    /// lhs , rhs
    CommaExpr,
    /// lhs ? data[0] : data[1]
    BinaryCondExpr,
    /// Used as the base for casts of the lhs in `BinaryCondExpr`.
    CondDummyExpr,
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
    /// Explicit: (type) cast
    ExplicitCast,
    /// Implicit: cast
    ImplicitCast,
    /// &lhs
    AddrOfExpr,
    /// &&DeclRef
    AddrOfLabel,
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
    /// __imag un
    ImagExpr,
    /// __real un
    RealExpr,
    /// lhs[rhs]  lhs is pointer/array type, rhs is integer type
    ArrayAccessExpr,
    /// first(second) second may be 0
    CallExprOne,
    /// data[0](data[1..])
    CallExpr,
    ///decl
    BuiltinCallExprOne,
    BuiltinCallExpr,
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
    /// C23 bool literal `true` / `false`
    BoolLiteral,
    /// C23 nullptr literal
    NullPtrLiteral,
    /// integer literal, always unsigned
    IntLiteral,
    /// Same as IntLiteral
    CharLiteral,
    /// f32 literal
    FloatLiteral,
    /// f64 literal
    DoubleLiteral,
    /// wraps a float or double literal: un
    ImaginaryLiteral,
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
    /// __builtin_types_compatible_p(lhs, rhs)
    BuiltinTypesCompatibleP,
    /// ({ un }) *gnu extension*
    StmtExpr,

    /// ====== Initializer expressions ======
    /// { lhs, rhs }
    ArrayInitExprTwo,
    /// { range }
    ArrayInitExpr,
    /// { lhs, rhs }
    StructInitExprTwo,
    /// { range }
    StructInitExpr,
    /// { union_init }
    UnionInitExpr,
    /// (ty){ un }
    CompoundLiteralExpr,

    /// Inserted at the end of a function body if no return stmt is found.
    /// ty is the functions return type
    /// data is return zero which is true if the function is called "main"
    ImplicitReturn,

    /// Inserted in array init expr to represent unspecified elements.
    /// data.int contains the amount of elements.
    ArrayFillerExpr,
    /// Inserted in record and scalar initilizers for represent unspecified fields.
    DefaultInitExpr,

    pub fn isImplicit(tag: Tag) bool {
        return switch (tag) {
            .ImplicitCast,
            .ImplicitReturn,
            .ImplicitStaticVar,
            .ArrayFillerExpr,
            .DefaultInitExpr,
            .CondDummyExpr,
            => true,

            else => false,
        };
    }
};
