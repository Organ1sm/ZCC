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

fnProto: 'invalid'
 name: a

fnDef: 'fn () void'
 name: bad
 body:
  compoundStmt
    variable: 'invalid'
     name: a

    variable: 'int'
     name: b
     init:
      intLiteral: 'int' (value: 1)

    variable: 'int'
     name: c
     init:
      intLiteral: 'int' (value: 2)

    variable: 'int'
     name: d
     init:
      intLiteral: 'int' (value: 3)

    variable: 'invalid'
     name: e
     init:
      stringLiteralExpr: '[1]char' lvalue (value: "")

    variable: 'invalid'
     name: f
     init:
      implicit defaultInitExpr: 'invalid'

    variable: 'int'
     name: g
     init:
      intLiteral: 'int' (value: 1)

    implicit returnStmt: 'void'

fnDef: 'fn () void'
 name: good
 body:
  compoundStmt
    variable: 'int'
     name: a
     init:
      intLiteral: 'int' (value: 1)

    variable: 'decayed *[4]char'
     name: b
     init:
      implicit cast: (ArrayToPointer) 'decayed *[4]char'
        stringLiteralExpr: '[4]char' lvalue (value: "foo")

    variable: '*fn () void'
     name: c
     init:
      implicit cast: (FunctionToPointer) '*fn () void'
        declRefExpr: 'fn () void' lvalue
         name: good

    implicit returnStmt: 'void'

