fnDef: 'fn () void'
 name: foo
 body:
  compoundStmt: 'void'
    variable: '_Complex double'
     name: cd
     init:
      arrayInitExpr: '_Complex double'
        floatLiteral: 'double' (value: 1)

        floatLiteral: 'double' (value: 2)

    variable: '_Complex float'
     name: cf
     init:
      arrayInitExpr: '_Complex float'
        floatLiteral: 'float' (value: 1)

        floatLiteral: 'float' (value: 2)

    variable: '_Complex int'
     name: ci
     init:
      arrayInitExpr: '_Complex int'
        intLiteral: 'int' (value: 1)

        intLiteral: 'int' (value: 2)

    assignExpr: '_Complex double'
     lhs:
      declRefExpr: '_Complex double' lvalue
       name: cd
     rhs:
      implicitCast: (LValToRVal) '_Complex double'
        compoundLiteralExpr: '_Complex double' lvalue
         arrayInitExpr: '_Complex double'
           implicitCast: (FloatCast) 'double'
             floatLiteral: 'float' (value: 1)

           implicitCast: (FloatCast) 'double'
             floatLiteral: 'float' (value: 2)

    assignExpr: '_Complex float'
     lhs:
      declRefExpr: '_Complex float' lvalue
       name: cf
     rhs:
      implicitCast: (LValToRVal) '_Complex float'
        compoundLiteralExpr: '_Complex float' lvalue
         arrayInitExpr: '_Complex float'
           implicitCast: (FloatCast) 'float'
             floatLiteral: 'double' (value: 1)

           implicitCast: (FloatCast) 'float'
             floatLiteral: 'double' (value: 2)

    implicitReturn: 'void'

