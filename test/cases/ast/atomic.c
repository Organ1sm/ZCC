implicit typedef: 'unsigned char'
 name: char8_t

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

variable: 'invalid'
 name: a

variable: 'invalid'
 name: b

variable: 'invalid'
 name: c

structForwardDecl: 'struct A'

variable: 'invalid'
 name: d

variable: 'invalid'
 name: e

variable: 'invalid'
 name: f

variable: 'invalid'
 name: g
 init:
  intLiteral: 'int' (value: 1)

variable: '_Atomic(int *)'
 name: i

variable: '_Atomic(int (*)[2])'
 name: j

fnDef: 'fn () void'
 name: test_func
 body:
  compoundStmt
    variable: '_Atomic(int)'
     name: a

    variable: 'long'
     name: b

    assignExpr: 'long'
     lhs:
      declRefExpr: 'long' lvalue
       name: b
     rhs:
      implicit cast: (IntCast) 'long'
        implicit cast: (LValToRVal) '_Atomic(int)'
          declRefExpr: '_Atomic(int)' lvalue
           name: a

    assignExpr: '_Atomic(int)'
     lhs:
      declRefExpr: '_Atomic(int)' lvalue
       name: a
     rhs:
      implicit cast: (IntCast) '_Atomic(int)'
        implicit cast: (LValToRVal) 'long'
          declRefExpr: 'long' lvalue
           name: b

    addAssignExpr: '_Atomic(int)'
     lhs:
      declRefExpr: '_Atomic(int)' lvalue
       name: a
     rhs:
      implicit cast: (IntCast) '_Atomic(int)' (value: 1)
        intLiteral: 'int' (value: 1)

    variable: '_Atomic(float)'
     name: f

    assignExpr: '_Atomic(int)'
     lhs:
      declRefExpr: '_Atomic(int)' lvalue
       name: a
     rhs:
      implicit cast: (FloatToInt) '_Atomic(int)'
        implicit cast: (LValToRVal) '_Atomic(float)'
          declRefExpr: '_Atomic(float)' lvalue
           name: f

    assignExpr: '_Atomic(float)'
     lhs:
      declRefExpr: '_Atomic(float)' lvalue
       name: f
     rhs:
      implicit cast: (IntToFloat) '_Atomic(float)'
        implicit cast: (LValToRVal) '_Atomic(int)'
          declRefExpr: '_Atomic(int)' lvalue
           name: a

    structDecl: 'struct B'
      recordField: 'int'
       name: a

    variable: '_Atomic(struct B)'
     name: s

    implicit returnStmt: 'void'

