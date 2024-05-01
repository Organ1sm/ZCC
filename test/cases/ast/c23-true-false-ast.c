Var: 'bool'
 name: a
 init:
  BoolLiteral: 'bool' (value: true)

Var: 'bool'
 name: b
 init:
  BoolLiteral: 'bool' (value: false)

Var: 'bool'
 name: c
 init:
  ImplicitCast: (IntToBool) 'bool'
    IntLiteral: 'int' (value: 0)

Var: 'bool'
 name: d
 init:
  ImplicitCast: (IntToBool) 'bool'
    IntLiteral: 'int' (value: 1)

Var: 'int'
 name: e
 init:
  ImplicitCast: (BoolToInt) 'int'
    BoolLiteral: 'bool' (value: true)

Var: 'int'
 name: f
 init:
  ImplicitCast: (BoolToInt) 'int'
    BoolLiteral: 'bool' (value: false)

Var: 'int'
 name: g
 init:
  AddExpr: 'int'
   lhs:
    ImplicitCast: (BoolToInt) 'int'
      BoolLiteral: 'bool' (value: true)
   rhs:
    IntLiteral: 'int' (value: 1)

