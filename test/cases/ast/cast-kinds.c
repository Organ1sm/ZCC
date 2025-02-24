unionDecl: 'union U'
  recordField: 'int'
   name: x

  recordField: 'float'
   name: y

fnDef: 'fn () int'
 name: bar
 body:
  compoundStmt: 'void'
    returnStmt: 'int'
     expr:
      intLiteral: 'int' (value: 42)

fnDef: 'fn () void'
 name: foo
 body:
  compoundStmt: 'void'
    variable: 'int'
     name: x

    variable: 'float'
     name: f

    variable: 'double'
     name: d

    variable: '[2]int'
     name: arr

    variable: '*int'
     name: p

    assignExpr: '*int'
     lhs:
      declRefExpr: '*int' lvalue
       name: p
     rhs:
      explicitCast: (Bitcast) '*int'
        addrOfExpr: '*float'
         operand:
          declRefExpr: 'float' lvalue
           name: f

    assignExpr: '*int'
     lhs:
      declRefExpr: '*int' lvalue
       name: p
     rhs:
      implicitCast: (ArrayToPointer) '*d[2]int'
        declRefExpr: '[2]int' lvalue
         name: arr

    assignExpr: 'int'
     lhs:
      declRefExpr: 'int' lvalue
       name: x
     rhs:
      callExpr: 'int'
       callee:
        implicitCast: (FunctionToPointer) '*fn () int'
          declRefExpr: 'fn () int' lvalue
           name: bar

    variable: '_Bool'
     name: b
     init:
      implicitCast: (PointerToBool) '_Bool'
        implicitCast: (LValToRVal) '*int'
          declRefExpr: '*int' lvalue
           name: p

    assignExpr: 'int'
     lhs:
      declRefExpr: 'int' lvalue
       name: x
     rhs:
      implicitCast: (PointerToInt) 'int'
        implicitCast: (LValToRVal) '*int'
          declRefExpr: '*int' lvalue
           name: p

    assignExpr: 'int'
     lhs:
      declRefExpr: 'int' lvalue
       name: x
     rhs:
      implicitCast: (BoolToInt) 'int'
        implicitCast: (LValToRVal) '_Bool'
          declRefExpr: '_Bool' lvalue
           name: b

    assignExpr: 'float'
     lhs:
      declRefExpr: 'float' lvalue
       name: f
     rhs:
      implicitCast: (BoolToFloat) 'float'
        implicitCast: (LValToRVal) '_Bool'
          declRefExpr: '_Bool' lvalue
           name: b

    assignExpr: '*int'
     lhs:
      declRefExpr: '*int' lvalue
       name: p
     rhs:
      implicitCast: (BoolToPointer) '*int'
        implicitCast: (LValToRVal) '_Bool'
          declRefExpr: '_Bool' lvalue
           name: b

    assignExpr: '_Bool'
     lhs:
      declRefExpr: '_Bool' lvalue
       name: b
     rhs:
      implicitCast: (IntToBool) '_Bool'
        implicitCast: (LValToRVal) 'int'
          declRefExpr: 'int' lvalue
           name: x

    assignExpr: 'float'
     lhs:
      declRefExpr: 'float' lvalue
       name: f
     rhs:
      implicitCast: (IntToFloat) 'float'
        implicitCast: (LValToRVal) 'int'
          declRefExpr: 'int' lvalue
           name: x

    assignExpr: '*int'
     lhs:
      declRefExpr: '*int' lvalue
       name: p
     rhs:
      implicitCast: (IntToPointer) '*int'
        implicitCast: (LValToRVal) 'int'
          declRefExpr: 'int' lvalue
           name: x

    assignExpr: '_Bool'
     lhs:
      declRefExpr: '_Bool' lvalue
       name: b
     rhs:
      implicitCast: (FloatToBool) '_Bool'
        implicitCast: (LValToRVal) 'float'
          declRefExpr: 'float' lvalue
           name: f

    assignExpr: 'int'
     lhs:
      declRefExpr: 'int' lvalue
       name: x
     rhs:
      implicitCast: (FloatToInt) 'int'
        implicitCast: (LValToRVal) 'float'
          declRefExpr: 'float' lvalue
           name: f

    assignExpr: 'int'
     lhs:
      declRefExpr: 'int' lvalue
       name: x
     rhs:
      implicitCast: (IntCast) 'int'
        intLiteral: 'long' (value: 1)

    assignExpr: 'float'
     lhs:
      declRefExpr: 'float' lvalue
       name: f
     rhs:
      implicitCast: (FloatCast) 'float'
        implicitCast: (LValToRVal) 'double'
          declRefExpr: 'double' lvalue
           name: d

    assignExpr: 'double'
     lhs:
      declRefExpr: 'double' lvalue
       name: d
     rhs:
      implicitCast: (FloatCast) 'double'
        implicitCast: (LValToRVal) 'float'
          declRefExpr: 'float' lvalue
           name: f

    assignExpr: '*int'
     lhs:
      declRefExpr: '*int' lvalue
       name: p
     rhs:
      implicitCast: (NullToPointer) '*int'
        intLiteral: 'int' (value: 0)

    explicitCast: (ToVoid) 'void'
      implicitCast: (LValToRVal) '*int'
        declRefExpr: '*int' lvalue
         name: p

    variable: 'union U'
     name: u

    assignExpr: 'union U'
     lhs:
      declRefExpr: 'union U' lvalue
       name: u
     rhs:
      explicitCast: (UnionCast) 'union U'
        implicitCast: (LValToRVal) 'int'
          declRefExpr: 'int' lvalue
           name: x

    assignExpr: 'union U'
     lhs:
      declRefExpr: 'union U' lvalue
       name: u
     rhs:
      explicitCast: (UnionCast) 'union U'
        implicitCast: (LValToRVal) 'float'
          declRefExpr: 'float' lvalue
           name: f

    implicitReturn: 'void'

