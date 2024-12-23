UnionDeclTwo: 'union U'
  RecordFieldDecl: 'int'
   name: x
  RecordFieldDecl: 'float'
   name: y

FnDef: 'fn () int'
 name: bar
 body:
  CompoundStmtTwo: 'void'
    ReturnStmt: 'void'
     expr:
      IntLiteral: 'int' (value: 42)

FnDef: 'fn () void'
 name: foo
 body:
  CompoundStmt: 'void'
    Var: 'int'
     name: x

    Var: 'float'
     name: f

    Var: 'double'
     name: d

    Var: '[2]int'
     name: arr

    Var: '*int'
     name: p

    AssignExpr: '*int'
     lhs:
      DeclRefExpr: '*int' lvalue
       name: p
     rhs:
      ExplicitCast: (Bitcast) '*int'
        AddrOfExpr: '*float'
         operand:
          DeclRefExpr: 'float' lvalue
           name: f

    AssignExpr: '*int'
     lhs:
      DeclRefExpr: '*int' lvalue
       name: p
     rhs:
      ImplicitCast: (ArrayToPointer) '*d[2]int'
        DeclRefExpr: '[2]int' lvalue
         name: arr

    AssignExpr: 'int'
     lhs:
      DeclRefExpr: 'int' lvalue
       name: x
     rhs:
      CallExprOne: 'int'
       lhs:
        ImplicitCast: (FunctionToPointer) '*fn () int'
          DeclRefExpr: 'fn () int' lvalue
           name: bar

    Var: '_Bool'
     name: b
     init:
      ImplicitCast: (PointerToBool) '_Bool'
        ImplicitCast: (LValToRVal) '*int'
          DeclRefExpr: '*int' lvalue
           name: p

    AssignExpr: 'int'
     lhs:
      DeclRefExpr: 'int' lvalue
       name: x
     rhs:
      ImplicitCast: (PointerToInt) 'int'
        ImplicitCast: (LValToRVal) '*int'
          DeclRefExpr: '*int' lvalue
           name: p

    AssignExpr: 'int'
     lhs:
      DeclRefExpr: 'int' lvalue
       name: x
     rhs:
      ImplicitCast: (BoolToInt) 'int'
        ImplicitCast: (LValToRVal) '_Bool'
          DeclRefExpr: '_Bool' lvalue
           name: b

    AssignExpr: 'float'
     lhs:
      DeclRefExpr: 'float' lvalue
       name: f
     rhs:
      ImplicitCast: (BoolToFloat) 'float'
        ImplicitCast: (LValToRVal) '_Bool'
          DeclRefExpr: '_Bool' lvalue
           name: b

    AssignExpr: '*int'
     lhs:
      DeclRefExpr: '*int' lvalue
       name: p
     rhs:
      ImplicitCast: (BoolToPointer) '*int'
        ImplicitCast: (LValToRVal) '_Bool'
          DeclRefExpr: '_Bool' lvalue
           name: b

    AssignExpr: '_Bool'
     lhs:
      DeclRefExpr: '_Bool' lvalue
       name: b
     rhs:
      ImplicitCast: (IntToBool) '_Bool'
        ImplicitCast: (LValToRVal) 'int'
          DeclRefExpr: 'int' lvalue
           name: x

    AssignExpr: 'float'
     lhs:
      DeclRefExpr: 'float' lvalue
       name: f
     rhs:
      ImplicitCast: (IntToFloat) 'float'
        ImplicitCast: (LValToRVal) 'int'
          DeclRefExpr: 'int' lvalue
           name: x

    AssignExpr: '*int'
     lhs:
      DeclRefExpr: '*int' lvalue
       name: p
     rhs:
      ImplicitCast: (IntToPointer) '*int'
        ImplicitCast: (LValToRVal) 'int'
          DeclRefExpr: 'int' lvalue
           name: x

    AssignExpr: '_Bool'
     lhs:
      DeclRefExpr: '_Bool' lvalue
       name: b
     rhs:
      ImplicitCast: (FloatToBool) '_Bool'
        ImplicitCast: (LValToRVal) 'float'
          DeclRefExpr: 'float' lvalue
           name: f

    AssignExpr: 'int'
     lhs:
      DeclRefExpr: 'int' lvalue
       name: x
     rhs:
      ImplicitCast: (FloatToInt) 'int'
        ImplicitCast: (LValToRVal) 'float'
          DeclRefExpr: 'float' lvalue
           name: f

    AssignExpr: 'int'
     lhs:
      DeclRefExpr: 'int' lvalue
       name: x
     rhs:
      ImplicitCast: (IntCast) 'int'
        IntLiteral: 'long' (value: 1)

    AssignExpr: 'float'
     lhs:
      DeclRefExpr: 'float' lvalue
       name: f
     rhs:
      ImplicitCast: (FloatCast) 'float'
        ImplicitCast: (LValToRVal) 'double'
          DeclRefExpr: 'double' lvalue
           name: d

    AssignExpr: 'double'
     lhs:
      DeclRefExpr: 'double' lvalue
       name: d
     rhs:
      ImplicitCast: (FloatCast) 'double'
        ImplicitCast: (LValToRVal) 'float'
          DeclRefExpr: 'float' lvalue
           name: f

    AssignExpr: '*int'
     lhs:
      DeclRefExpr: '*int' lvalue
       name: p
     rhs:
      ImplicitCast: (NullToPointer) '*int'
        IntLiteral: 'int' (value: 0)

    ExplicitCast: (ToVoid) 'void'
      ImplicitCast: (LValToRVal) '*int'
        DeclRefExpr: '*int' lvalue
         name: p

    Var: 'union U'
     name: u

    AssignExpr: 'union U'
     lhs:
      DeclRefExpr: 'union U' lvalue
       name: u
     rhs:
      ExplicitCast: (UnionCast) 'union U'
        ImplicitCast: (LValToRVal) 'int'
          DeclRefExpr: 'int' lvalue
           name: x

    AssignExpr: 'union U'
     lhs:
      DeclRefExpr: 'union U' lvalue
       name: u
     rhs:
      ExplicitCast: (UnionCast) 'union U'
        ImplicitCast: (LValToRVal) 'float'
          DeclRefExpr: 'float' lvalue
           name: f

    ImplicitReturn: 'void'

