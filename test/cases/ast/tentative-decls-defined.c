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

fnProto: 'fn (int) int'
 name: foo
 definition: 0x9

fnProto: 'fn (int) int'
 name: foo
 definition: 0x9

fnProto: 'fn (int) int'
 name: foo
 definition: 0x9

fnDef: 'fn (a: int) int'
 name: foo
 body:
  compoundStmt
    returnStmt: 'int'
     expr:
      implicit cast: (LValToRVal) 'int'
        declRefExpr: 'int' lvalue
         name: a

fnProto: 'fn (int) int'
 name: foo
 definition: 0x9

fnProto: 'fn (int) int'
 name: foo
 definition: 0x9

fnProto: 'fn (int) int'
 name: foo
 definition: 0x9

variable: 'int'
 extern name: a
 definition: 0x14

variable: 'int'
 name: a
 definition: 0x14

variable: 'int'
 name: a
 init:
  intLiteral: 'int' (value: 1)

variable: 'int'
 extern name: a
 definition: 0x14

variable: 'int'
 name: a
 definition: 0x14

fnDef: 'fn () int'
 name: bar
 body:
  compoundStmt
    fnProto: 'fn () int'
     name: baz

    fnProto: 'fn () int'
     name: baz
     definition: 0x19

    variable: 'int'
     extern name: b

    variable: 'int'
     extern name: b
     definition: 0x1B

    returnStmt: 'int'
     expr:
      implicit cast: (LValToRVal) 'int'
        declRefExpr: 'int' lvalue
         name: b

