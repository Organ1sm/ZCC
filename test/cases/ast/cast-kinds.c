implicit typedef: '__int128'
 name: __int128_t

implicit typedef: 'unsigned __int128'
 name: __uint128_t

implicit typedef: '*char'
 name: __builtin_ms_va_list

implicit typedef: '[1]struct __va_list_tag'
 name: __builtin_va_list

implicit typedef: 'long double'
 name: __float80

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
      cast: (Bitcast) '*int'
        addrOfExpr: '*float'
         operand:
          declRefExpr: 'float' lvalue
           name: f

    assignExpr: '*int'
     lhs:
      declRefExpr: '*int' lvalue
       name: p
     rhs:
      implicit cast: (ArrayToPointer) '*d[2]int'
        declRefExpr: '[2]int' lvalue
         name: arr

    assignExpr: 'int'
     lhs:
      declRefExpr: 'int' lvalue
       name: x
     rhs:
      callExpr: 'int'
       callee:
        implicit cast: (FunctionToPointer) '*fn () int'
          declRefExpr: 'fn () int' lvalue
           name: bar

    variable: '_Bool'
     name: b
     init:
      implicit cast: (PointerToBool) '_Bool'
        implicit cast: (LValToRVal) '*int'
          declRefExpr: '*int' lvalue
           name: p

    assignExpr: 'int'
     lhs:
      declRefExpr: 'int' lvalue
       name: x
     rhs:
      implicit cast: (PointerToInt) 'int'
        implicit cast: (LValToRVal) '*int'
          declRefExpr: '*int' lvalue
           name: p

    assignExpr: 'int'
     lhs:
      declRefExpr: 'int' lvalue
       name: x
     rhs:
      implicit cast: (BoolToInt) 'int'
        implicit cast: (LValToRVal) '_Bool'
          declRefExpr: '_Bool' lvalue
           name: b

    assignExpr: 'float'
     lhs:
      declRefExpr: 'float' lvalue
       name: f
     rhs:
      implicit cast: (BoolToFloat) 'float'
        implicit cast: (LValToRVal) '_Bool'
          declRefExpr: '_Bool' lvalue
           name: b

    assignExpr: '*int'
     lhs:
      declRefExpr: '*int' lvalue
       name: p
     rhs:
      implicit cast: (BoolToPointer) '*int'
        implicit cast: (LValToRVal) '_Bool'
          declRefExpr: '_Bool' lvalue
           name: b

    assignExpr: '_Bool'
     lhs:
      declRefExpr: '_Bool' lvalue
       name: b
     rhs:
      implicit cast: (IntToBool) '_Bool'
        implicit cast: (LValToRVal) 'int'
          declRefExpr: 'int' lvalue
           name: x

    assignExpr: 'float'
     lhs:
      declRefExpr: 'float' lvalue
       name: f
     rhs:
      implicit cast: (IntToFloat) 'float'
        implicit cast: (LValToRVal) 'int'
          declRefExpr: 'int' lvalue
           name: x

    assignExpr: '*int'
     lhs:
      declRefExpr: '*int' lvalue
       name: p
     rhs:
      implicit cast: (IntToPointer) '*int'
        implicit cast: (LValToRVal) 'int'
          declRefExpr: 'int' lvalue
           name: x

    assignExpr: '_Bool'
     lhs:
      declRefExpr: '_Bool' lvalue
       name: b
     rhs:
      implicit cast: (FloatToBool) '_Bool'
        implicit cast: (LValToRVal) 'float'
          declRefExpr: 'float' lvalue
           name: f

    assignExpr: 'int'
     lhs:
      declRefExpr: 'int' lvalue
       name: x
     rhs:
      implicit cast: (FloatToInt) 'int'
        implicit cast: (LValToRVal) 'float'
          declRefExpr: 'float' lvalue
           name: f

    assignExpr: 'int'
     lhs:
      declRefExpr: 'int' lvalue
       name: x
     rhs:
      implicit cast: (IntCast) 'int'
        intLiteral: 'long' (value: 1)

    assignExpr: 'float'
     lhs:
      declRefExpr: 'float' lvalue
       name: f
     rhs:
      implicit cast: (FloatCast) 'float'
        implicit cast: (LValToRVal) 'double'
          declRefExpr: 'double' lvalue
           name: d

    assignExpr: 'double'
     lhs:
      declRefExpr: 'double' lvalue
       name: d
     rhs:
      implicit cast: (FloatCast) 'double'
        implicit cast: (LValToRVal) 'float'
          declRefExpr: 'float' lvalue
           name: f

    assignExpr: '*int'
     lhs:
      declRefExpr: '*int' lvalue
       name: p
     rhs:
      implicit cast: (NullToPointer) '*int'
        intLiteral: 'int' (value: 0)

    cast: (ToVoid) 'void'
      implicit cast: (LValToRVal) '*int'
        declRefExpr: '*int' lvalue
         name: p

    variable: 'union U'
     name: u

    assignExpr: 'union U'
     lhs:
      declRefExpr: 'union U' lvalue
       name: u
     rhs:
      cast: (UnionCast) 'union U'
        implicit cast: (LValToRVal) 'int'
          declRefExpr: 'int' lvalue
           name: x

    assignExpr: 'union U'
     lhs:
      declRefExpr: 'union U' lvalue
       name: u
     rhs:
      cast: (UnionCast) 'union U'
        implicit cast: (LValToRVal) 'float'
          declRefExpr: 'float' lvalue
           name: f

    implicit returnStmt: 'void'

