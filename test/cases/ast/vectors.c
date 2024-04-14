TypeDef: '*float'
 name: invalid1

TypeDef: 'float'
 name: invalid2

TypeDef: 'vector((2, float)'
 name: f2v

FnDef: 'fn () void'
 name: foo
 body:
  CompoundStmt: 'void'
    Var: 'vector((2, float)'
     name: a

    Var: 'vector((2, float)'
     name: b

    NullStmt: 'void'

    MulAssignExpr: 'vector((2, float)'
     lhs:
      DeclRefExpr: 'vector((2, float)' lvalue
       name: a
     rhs:
      ImplicitCast: (VectorSplat) 'float'
        ImplicitCast: (IntToFloat) 'float' (value: 2)
          IntLiteral: 'int' (value: 2)

    NullStmt: 'void'

    ImplicitReturn: 'void'

