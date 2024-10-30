FnDef: 'fn () void'
 name: foo
 body:
  CompoundStmt: 'void'
    Var: '_Complex double'
     name: cd
     init:
      ArrayInitExprTwo: '_Complex double'
        FloatLiteral: 'double' (value: 1)
        FloatLiteral: 'double' (value: 2)

    Var: '_Complex float'
     name: cf
     init:
      ArrayInitExprTwo: '_Complex float'
        FloatLiteral: 'float' (value: 1)
        FloatLiteral: 'float' (value: 2)

    Var: '_Complex int'
     name: ci
     init:
      ArrayInitExprTwo: '_Complex int'
        IntLiteral: 'int' (value: 1)
        IntLiteral: 'int' (value: 2)

    AssignExpr: '_Complex double'
     lhs:
      DeclRefExpr: '_Complex double' lvalue
       name: cd
     rhs:
      ImplicitCast: (LValToRVal) '_Complex double'
        CompoundLiteralExpr: '_Complex double' lvalue
         ArrayInitExprTwo: '_Complex double'
           ImplicitCast: (FloatCast) 'double'
             FloatLiteral: 'float' (value: 1)
           ImplicitCast: (FloatCast) 'double'
             FloatLiteral: 'float' (value: 2)

    AssignExpr: '_Complex float'
     lhs:
      DeclRefExpr: '_Complex float' lvalue
       name: cf
     rhs:
      ImplicitCast: (LValToRVal) '_Complex float'
        CompoundLiteralExpr: '_Complex float' lvalue
         ArrayInitExprTwo: '_Complex float'
           ImplicitCast: (FloatCast) 'float'
             FloatLiteral: 'double' (value: 1)
           ImplicitCast: (FloatCast) 'float'
             FloatLiteral: 'double' (value: 2)

    ImplicitReturn: 'void'

