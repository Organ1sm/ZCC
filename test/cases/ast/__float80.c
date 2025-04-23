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
    variable: '__float80': 'long double'
     name: x
     init:
      floatLiteral: 'long double' (value: 1)

    assignExpr: '__float80': 'long double'
     lhs:
      declRefExpr: '__float80': 'long double' lvalue
       name: x
     rhs:
      floatLiteral: 'long double' (value: 1)

    variable: '_Complex long double'
     name: z

    assignExpr: '_Complex long double'
     lhs:
      declRefExpr: '_Complex long double' lvalue
       name: z
     rhs:
      implicit cast: (ComplexFloatCast) '_Complex long double'
        imaginaryLiteral: '_Complex double' (value: 0 + 1i)
         operand:
          floatLiteral: 'long double'

    assignExpr: '_Complex long double'
     lhs:
      declRefExpr: '_Complex long double' lvalue
       name: z
     rhs:
      implicit cast: (ComplexFloatCast) '_Complex long double'
        imaginaryLiteral: '_Complex double' (value: 0 + 1i)
         operand:
          floatLiteral: 'long double'

    implicit returnStmt: 'void'

