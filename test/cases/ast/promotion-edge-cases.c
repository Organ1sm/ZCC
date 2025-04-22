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

structDecl: 'struct S'
  recordField: 'unsigned int'
   name: x
   bits:
    intLiteral: 'int' (value: 3)

  recordField: 'long'
   name: y
   bits:
    intLiteral: 'int' (value: 5)

fnDef: 'fn () void'
 name: foo
 body:
  compoundStmt: 'void'
    variable: 'char'
     name: c
     init:
      implicit cast: (IntCast) 'char' (value: 0)
        intLiteral: 'int' (value: 0)

    variable: 'double'
     name: d
     init:
      floatLiteral: 'double' (value: 2)

    assignExpr: 'double'
     lhs:
      declRefExpr: 'double' lvalue
       name: d
     rhs:
      addExpr: 'double'
       lhs:
        implicit cast: (LValToRVal) 'double'
          declRefExpr: 'double' lvalue
           name: d
       rhs:
        implicit cast: (IntToFloat) 'double'
          implicit cast: (IntCast) 'int'
            implicit cast: (LValToRVal) 'char'
              declRefExpr: 'char' lvalue
               name: c

    variable: 'struct S'
     name: s
     init:
      structInitExpr: 'struct S'
        intLiteral: 'unsigned int' (value: 1)

        intLiteral: 'long' (value: 1)

    variable: 'int'
     name: x
     init:
      addExpr: 'int'
       lhs:
        implicit cast: (IntCast) 'int'
          implicit cast: (LValToRVal) 'unsigned int'
            memberAccessExpr: 'unsigned int' lvalue bitfield
             lhs:
              declRefExpr: 'struct S' lvalue
               name: s
             name: x
       rhs:
        intLiteral: 'int' (value: 1)

    variable: 'int'
     name: y
     init:
      addExpr: 'int'
       lhs:
        implicit cast: (IntCast) 'int'
          implicit cast: (LValToRVal) 'long'
            memberAccessExpr: 'long' lvalue bitfield
             lhs:
              declRefExpr: 'struct S' lvalue
               name: s
             name: y
       rhs:
        intLiteral: 'int' (value: 1)

    variable: '__fp16'
     name: fp16
     init:
      implicit cast: (FloatCast) '__fp16' (value: 0)
        floatLiteral: 'float' (value: 0)

    assignExpr: '__fp16'
     lhs:
      declRefExpr: '__fp16' lvalue
       name: fp16
     rhs:
      implicit cast: (FloatCast) '__fp16'
        addExpr: 'float'
         lhs:
          implicit cast: (FloatCast) 'float'
            implicit cast: (LValToRVal) '__fp16'
              declRefExpr: '__fp16' lvalue
               name: fp16
         rhs:
          implicit cast: (FloatCast) 'float'
            implicit cast: (LValToRVal) '__fp16'
              declRefExpr: '__fp16' lvalue
               name: fp16

    implicit returnStmt: 'void'

