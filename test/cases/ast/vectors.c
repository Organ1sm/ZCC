implicit typedef: '__int128'
 name: __int128_t

implicit typedef: 'unsigned __int128'
 name: __uint128_t

implicit typedef: '*char'
 name: __builtin_ms_va_list

implicit typedef: '[1]struct __va_list_tag'
 name: __builtin_va_list

implicit typedef: 'struct __NSConstantString_tag'
 name: __NSConstantString

implicit typedef: 'long double'
 name: __float80

typedef: 'float'
 name: invalid2

typedef: 'vector(2, float)'
 name: f2v

fnDef: 'fn () void'
 name: foo
 body:
  compoundStmt
    variable: 'f2v: vector(2, float)'
     name: a

    variable: 'f2v: vector(2, float)'
     name: b

    assignExpr: 'f2v: vector(2, float)'
     lhs:
      declRefExpr: 'f2v: vector(2, float)' lvalue
       name: a
     rhs:
      implicit cast: (LValToRVal) 'f2v: vector(2, float)'
        declRefExpr: 'f2v: vector(2, float)' lvalue
         name: b

    mulAssignExpr: 'f2v: vector(2, float)'
     lhs:
      declRefExpr: 'f2v: vector(2, float)' lvalue
       name: a
     rhs:
      mulExpr: 'f2v: vector(2, float)'
       lhs:
        implicit cast: (LValToRVal) 'f2v: vector(2, float)'
          implicit compoundAssignDummyExpr: 'f2v: vector(2, float)' lvalue
       rhs:
        implicit cast: (VectorSplat) 'f2v: vector(2, float)'
          implicit cast: (IntToFloat) 'float' (value: 2)
            intLiteral: 'int' (value: 2)

    implicit returnStmt: 'void'

fnDef: 'fn (vec: f2v: vector(2, float), index: int) float'
 name: subscript
 body:
  compoundStmt
    assignExpr: 'float'
     lhs:
      arrayAccessExpr: 'float' lvalue
       lhs:
        declRefExpr: 'f2v: vector(2, float)' lvalue
         name: vec
       index:
        implicit cast: (LValToRVal) 'int'
          declRefExpr: 'int' lvalue
           name: index
     rhs:
      implicit cast: (IntToFloat) 'float'
        intLiteral: 'int' (value: 1)

    returnStmt: 'float'
     expr:
      implicit cast: (LValToRVal) 'float'
        arrayAccessExpr: 'float' lvalue
         lhs:
          declRefExpr: 'f2v: vector(2, float)' lvalue
           name: vec
         index:
          implicit cast: (LValToRVal) 'int'
            declRefExpr: 'int' lvalue
             name: index

typedef: 'vector(2, int)'
 name: i2v

typedef: 'vector(3, int)'
 name: i3v

fnDef: 'fn (a: f2v: vector(2, float), b: i2v: vector(2, int), c: i3v: vector(3, int)) void'
 name: vector_conversions
 body:
  compoundStmt
    addExpr: 'f2v: vector(2, float)'
     lhs:
      implicit cast: (LValToRVal) 'f2v: vector(2, float)'
        declRefExpr: 'f2v: vector(2, float)' lvalue
         name: a
     rhs:
      implicit cast: (Bitcast) 'f2v: vector(2, float)'
        implicit cast: (LValToRVal) 'i2v: vector(2, int)'
          declRefExpr: 'i2v: vector(2, int)' lvalue
           name: b

    addExpr: 'i2v: vector(2, int)'
     lhs:
      implicit cast: (LValToRVal) 'i2v: vector(2, int)'
        declRefExpr: 'i2v: vector(2, int)' lvalue
         name: b
     rhs:
      implicit cast: (Bitcast) 'i2v: vector(2, int)'
        implicit cast: (LValToRVal) 'f2v: vector(2, float)'
          declRefExpr: 'f2v: vector(2, float)' lvalue
           name: a

    addExpr: 'f2v: vector(2, float)'
     lhs:
      implicit cast: (LValToRVal) 'f2v: vector(2, float)'
        declRefExpr: 'f2v: vector(2, float)' lvalue
         name: a
     rhs:
      implicit cast: (VectorSplat) 'f2v: vector(2, float)'
        implicit cast: (IntToFloat) 'float' (value: 1)
          intLiteral: 'int' (value: 1)

    addExpr: 'i2v: vector(2, int)'
     lhs:
      implicit cast: (LValToRVal) 'i2v: vector(2, int)'
        declRefExpr: 'i2v: vector(2, int)' lvalue
         name: b
     rhs:
      implicit cast: (VectorSplat) 'i2v: vector(2, int)'
        intLiteral: 'int' (value: 1)

    addExpr: 'invalid'
     lhs:
      implicit cast: (LValToRVal) 'f2v: vector(2, float)'
        declRefExpr: 'f2v: vector(2, float)' lvalue
         name: a
     rhs:
      implicit cast: (LValToRVal) 'i3v: vector(3, int)'
        declRefExpr: 'i3v: vector(3, int)' lvalue
         name: c

    implicit returnStmt: 'void'

