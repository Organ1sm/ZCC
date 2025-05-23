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

fnDef: 'fn () void'
 name: foo
 body:
  compoundStmt
    variable: '_Complex double'
     name: cd
     init:
      arrayInitExpr: '_Complex double' (value: 1 + 2i)
        floatLiteral: 'double' (value: 1)

        floatLiteral: 'double' (value: 2)

    variable: '_Complex float'
     name: cf
     init:
      arrayInitExpr: '_Complex float' (value: 1 + 2i)
        floatLiteral: 'float' (value: 1)

        floatLiteral: 'float' (value: 2)

    assignExpr: '_Complex double'
     lhs:
      declRefExpr: '_Complex double' lvalue
       name: cd
     rhs:
      implicit cast: (LValToRVal) '_Complex double'
        compoundLiteralExpr: '_Complex double' lvalue
         arrayInitExpr: '_Complex double' (value: 1 + 2i)
           implicit cast: (FloatCast) 'double' (value: 1)
             floatLiteral: 'float' (value: 1)

           implicit cast: (FloatCast) 'double' (value: 2)
             floatLiteral: 'float' (value: 2)

    assignExpr: '_Complex float'
     lhs:
      declRefExpr: '_Complex float' lvalue
       name: cf
     rhs:
      implicit cast: (LValToRVal) '_Complex float'
        compoundLiteralExpr: '_Complex float' lvalue
         arrayInitExpr: '_Complex float' (value: 1 + 2i)
           implicit cast: (FloatCast) 'float' (value: 1)
             floatLiteral: 'double' (value: 1)

           implicit cast: (FloatCast) 'float' (value: 2)
             floatLiteral: 'double' (value: 2)

    implicit returnStmt: 'void'

