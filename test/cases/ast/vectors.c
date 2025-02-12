TypeDef: 'invalid'
 name: invalid1

TypeDef: 'float'
 name: invalid2

TypeDef: 'vector((2, float)'
 name: f2v

FnDef: 'fn () void'
 name: foo
 body:
  CompoundStmt: 'void'
    Var: 'f2v': 'vector((2, float)'
     name: a

    Var: 'f2v': 'vector((2, float)'
     name: b

    MulAssignExpr: 'f2v': 'vector((2, float)'
     lhs:
      DeclRefExpr: 'f2v': 'vector((2, float)' lvalue
       name: a
     rhs:
      ImplicitCast: (VectorSplat) 'float'
        ImplicitCast: (IntToFloat) 'float' (value: 2)
          IntLiteral: 'int' (value: 2)

    ImplicitReturn: 'void'

