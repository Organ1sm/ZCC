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

fnDef: 'fn () int'
 name: main
 body:
  compoundStmt
    forStmt
     decl:
      variable: 'int'
       name: x
       init:
        intLiteral: 'int' (value: 0)

      variable: 'int'
       name: y
       init:
        intLiteral: 'int' (value: 1)

     incr:
      postIncExpr: 'int'
       operand:
        declRefExpr: 'int' lvalue
         name: x
     body:
      nullStmt: 'void'

    implicit returnStmt: 'int'

