variable: 'bool'
 name: a
 init:
  boolLiteral: 'bool' (value: true)

variable: 'bool'
 name: b
 init:
  boolLiteral: 'bool' (value: false)

variable: 'bool'
 name: c
 init:
  implicitCast: (IntToBool) 'bool'
    intLiteral: 'int' (value: 0)

variable: 'bool'
 name: d
 init:
  implicitCast: (IntToBool) 'bool'
    intLiteral: 'int' (value: 1)

variable: 'int'
 name: e
 init:
  implicitCast: (BoolToInt) 'int'
    boolLiteral: 'bool' (value: true)

variable: 'int'
 name: f
 init:
  implicitCast: (BoolToInt) 'int'
    boolLiteral: 'bool' (value: false)

variable: 'int'
 name: g
 init:
  addExpr: 'int'
   lhs:
    implicitCast: (BoolToInt) 'int'
      boolLiteral: 'bool' (value: true)
   rhs:
    intLiteral: 'int' (value: 1)

