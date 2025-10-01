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
        implicit cast: (VectorSplat) 'float'
          implicit cast: (IntToFloat) 'float' (value: 2)
            intLiteral: 'int' (value: 2)

    implicit returnStmt: 'void'

