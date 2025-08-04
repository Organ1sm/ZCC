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
  compoundStmt
    variable: 'char'
     name: x
     init:
      implicit cast: (IntCast) 'char' (value: 0)
        intLiteral: 'int' (value: 0)

    variable: 'unsigned int'
     name: y
     init:
      implicit cast: (IntCast) 'unsigned int' (value: 2)
        intLiteral: 'int' (value: 2)

    variable: '_Bool'
     name: overflowed

    variable: 'long'
     name: res

    assignExpr: '_Bool'
     lhs:
      declRefExpr: '_Bool' lvalue
       name: overflowed
     rhs:
      builtinCallExpr: '_Bool'
       name: __builtin_add_overflow
       args:
        implicit cast: (LValToRVal) 'char'
          declRefExpr: 'char' lvalue
           name: x
        implicit cast: (LValToRVal) 'unsigned int'
          declRefExpr: 'unsigned int' lvalue
           name: y
        addrOfExpr: '*long' (value: &res)
         operand:
          declRefExpr: 'long' lvalue
           name: res

    assignExpr: '_Bool'
     lhs:
      declRefExpr: '_Bool' lvalue
       name: overflowed
     rhs:
      builtinCallExpr: '_Bool'
       name: __builtin_sub_overflow
       args:
        implicit cast: (LValToRVal) 'char'
          declRefExpr: 'char' lvalue
           name: x
        implicit cast: (LValToRVal) 'unsigned int'
          declRefExpr: 'unsigned int' lvalue
           name: y
        addrOfExpr: '*long' (value: &res)
         operand:
          declRefExpr: 'long' lvalue
           name: res

    assignExpr: '_Bool'
     lhs:
      declRefExpr: '_Bool' lvalue
       name: overflowed
     rhs:
      builtinCallExpr: '_Bool'
       name: __builtin_mul_overflow
       args:
        implicit cast: (LValToRVal) 'char'
          declRefExpr: 'char' lvalue
           name: x
        implicit cast: (LValToRVal) 'unsigned int'
          declRefExpr: 'unsigned int' lvalue
           name: y
        addrOfExpr: '*long' (value: &res)
         operand:
          declRefExpr: 'long' lvalue
           name: res

    implicit returnStmt: 'void'

