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

fnDef: 'fn () void'
 name: foo
 body:
  compoundStmt: 'void'
    variable: '__fp16'
     name: x
     init:
      implicit cast: (FloatCast) '__fp16' (value: 1)
        floatLiteral: 'float' (value: 1)

    variable: '__fp16'
     name: y
     init:
      implicit cast: (FloatCast) '__fp16' (value: 2)
        floatLiteral: 'float' (value: 2)

    assignExpr: '__fp16'
     lhs:
      declRefExpr: '__fp16' lvalue
       name: x
     rhs:
      addExpr: '__fp16'
       lhs:
        implicit cast: (LValToRVal) '__fp16'
          declRefExpr: '__fp16' lvalue
           name: x
       rhs:
        implicit cast: (LValToRVal) '__fp16'
          declRefExpr: '__fp16' lvalue
           name: y

    implicit returnStmt: 'void'

