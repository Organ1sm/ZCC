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

variable: 'int'
 name: x
 init:
  genericExpr: 'int' (value: 42)
   controlling:
    intLiteral: 'int' (value: 5)
   chosen:
    genericAssociationExpr
      intLiteral: 'int' (value: 42)
   rest:
    genericAssociationExpr
      floatLiteral: 'double' (value: 32.5)

variable: 'int'
 name: y
 init:
  genericExpr: 'int' (value: 42)
   controlling:
    intLiteral: 'int' (value: 5)
   chosen:
    genericAssociationExpr
      intLiteral: 'int' (value: 42)
   rest:
    genericAssociationExpr
      floatLiteral: 'double' (value: 32.5)
    genericDefaultExpr
      stringLiteralExpr: '[7]char' lvalue (value: "string")

variable: 'double'
 name: z
 init:
  implicit cast: (IntToFloat) 'double' (value: 32)
    genericExpr: 'int'
     controlling:
      intLiteral: 'int' (value: 5)
     chosen:
      genericDefaultExpr
        intLiteral: 'int' (value: 32)

