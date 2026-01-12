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

function: 'fn () void'
 name: foo
 body:
  compoundStmt
    variable: 'float'
     name: a
     init:
      floatLiteral: 'float' (value: 1)

    variable: 'float'
     name: b
     init:
      floatLiteral: 'float' (value: 2)

    variable: 'float'
     name: c
     init:
      addExpr: 'float'
       lhs:
        implicit cast: (LValToRVal) 'float'
          declRefExpr: 'float' lvalue
           name: a
       rhs:
        implicit cast: (LValToRVal) 'float'
          declRefExpr: 'float' lvalue
           name: b

    variable: '_Complex float'
     name: ca
     init:
      implicit cast: (RealToComplexFloat) '_Complex float' (value: 0 + 0i)
        floatLiteral: 'float' (value: 0)

    assignExpr: '_Complex float'
     lhs:
      declRefExpr: '_Complex float' lvalue
       name: ca
     rhs:
      addExpr: '_Complex float'
       lhs:
        implicit cast: (LValToRVal) '_Complex float'
          declRefExpr: '_Complex float' lvalue
           name: ca
       rhs:
        implicit cast: (RealToComplexFloat) '_Complex float'
          implicit cast: (LValToRVal) 'float'
            declRefExpr: 'float' lvalue
             name: a

    implicit returnStmt: 'void'

