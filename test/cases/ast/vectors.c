typedef: 'invalid'
 name: invalid1

typedef: 'float'
 name: invalid2

typedef: 'vector((2, float)'
 name: f2v

fnDef: 'fn () void'
 name: foo
 body:
  compoundStmt: 'void'
    variable: 'f2v': 'vector((2, float)'
     name: a

    variable: 'f2v': 'vector((2, float)'
     name: b

    mulAssignExpr: 'f2v': 'vector((2, float)'
     lhs:
      declRefExpr: 'f2v': 'vector((2, float)' lvalue
       name: a
     rhs:
      implicitCast: (VectorSplat) 'float'
        implicitCast: (IntToFloat) 'float' (value: 2)
          intLiteral: 'int' (value: 2)

    implicitReturn: 'void'

