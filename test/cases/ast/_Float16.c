implicit typedef: '__int128'
 name: __int128_t

implicit typedef: 'unsigned __int128'
 name: __uint128_t

implicit typedef: '*char'
 name: __builtin_ms_va_list

implicit typedef: '[1]struct __va_list_tag'
 name: __builtin_va_list

typedef: '__builtin_va_list': '[1]struct __va_list_tag'
 name: va_list

typedef: '__builtin_va_list': '[1]struct __va_list_tag'
 name: __gnuc_va_list

fnDef: 'fn (x: _Float16, y: _Float16) _Float16'
 name: foo
 body:
  compoundStmt: 'void'
    returnStmt: '_Float16'
     expr:
      addExpr: '_Float16'
       lhs:
        implicit cast: (LValToRVal) '_Float16'
          declRefExpr: '_Float16' lvalue
           name: x
       rhs:
        implicit cast: (LValToRVal) '_Float16'
          declRefExpr: '_Float16' lvalue
           name: y

fnDef: 'fn (x: int) void'
 name: bar
 body:
  compoundStmt: 'void'
    variable: 'va_list': '[1]struct __va_list_tag'
     name: va

    builtinCallExpr: 'void'
     name: __builtin_va_start
     args:
      implicit cast: (ArrayToPointer) 'va_list': '*d[1]struct __va_list_tag'
        declRefExpr: 'va_list': '[1]struct __va_list_tag' lvalue
         name: va
      declRefExpr: 'int' lvalue
       name: x

    builtinCallExpr: 'void'
     name: __builtin_va_end
     args:
      implicit cast: (ArrayToPointer) 'va_list': '*d[1]struct __va_list_tag'
        declRefExpr: 'va_list': '[1]struct __va_list_tag' lvalue
         name: va

    implicit returnStmt: 'void'

fnDef: 'fn () void'
 name: quux
 body:
  compoundStmt: 'void'
    variable: '_Float16'
     name: f
     init:
      floatLiteral: '_Float16' (value: 1)

    callExpr: 'void'
     callee:
      implicit cast: (FunctionToPointer) '*fn (x: int) void'
        declRefExpr: 'fn (x: int) void' lvalue
         name: bar
     args:
      intLiteral: 'int' (value: 1)
      implicit cast: (LValToRVal) '_Float16'
        declRefExpr: '_Float16' lvalue
         name: f

    implicit returnStmt: 'void'

fnDef: 'fn () void'
 name: conversions
 body:
  compoundStmt: 'void'
    variable: 'double'
     name: d
     init:
      floatLiteral: 'double' (value: 1)

    variable: '_Float16'
     name: f16
     init:
      floatLiteral: '_Float16' (value: 2)

    variable: '__fp16'
     name: fp16
     init:
      implicit cast: (IntToFloat) '__fp16'
        intLiteral: 'int' (value: 0)

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
        implicit cast: (FloatCast) 'double'
          implicit cast: (LValToRVal) '_Float16'
            declRefExpr: '_Float16' lvalue
             name: f16

    cast: (ToVoid) 'void'
      parenExpr: '__fp16'
       operand:
        addExpr: '__fp16'
         lhs:
          implicit cast: (FloatCast) '__fp16'
            implicit cast: (LValToRVal) '_Float16'
              declRefExpr: '_Float16' lvalue
               name: f16
         rhs:
          implicit cast: (LValToRVal) '__fp16'
            declRefExpr: '__fp16' lvalue
             name: fp16

    implicit returnStmt: 'void'

