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

enumDecl: 'attributed(enum Small: unsigned char)'
 attr: packed
  enumField: 'int' (value: 0)
   name: A

staticAssert
 condition:
  implicit cast: (IntToBool) '_Bool'
    equalExpr: 'int' (value: 1)
     lhs:
      sizeofExpr: 'unsigned long'
       operand type: attributed(enum Small: unsigned char)
     rhs:
      implicit cast: (IntCast) 'unsigned long'
        intLiteral: 'int' (value: 1)
 diagnostic:
  stringLiteralExpr: '[6]char' lvalue (value: "Small")

enumDecl: 'attributed(enum StillSmall: unsigned char)'
 attr: packed
  enumField: 'int' (value: 255)
   name: B
   init:
    intLiteral: 'int' (value: 255)

staticAssert
 condition:
  implicit cast: (IntToBool) '_Bool'
    equalExpr: 'int' (value: 1)
     lhs:
      sizeofExpr: 'unsigned long'
       operand type: attributed(enum StillSmall: unsigned char)
     rhs:
      implicit cast: (IntCast) 'unsigned long'
        intLiteral: 'int' (value: 1)
 diagnostic:
  stringLiteralExpr: '[11]char' lvalue (value: "StillSmall")

enumDecl: 'attributed(enum Medium: unsigned short)'
 attr: packed
  enumField: 'int' (value: 255)
   name: C
   init:
    intLiteral: 'int' (value: 255)

  enumField: 'int' (value: 256)
   name: D

staticAssert
 condition:
  implicit cast: (IntToBool) '_Bool'
    equalExpr: 'int' (value: 1)
     lhs:
      sizeofExpr: 'unsigned long'
       operand type: attributed(enum Medium: unsigned short)
     rhs:
      implicit cast: (IntCast) 'unsigned long'
        intLiteral: 'int' (value: 2)
 diagnostic:
  stringLiteralExpr: '[7]char' lvalue (value: "Medium")

enumDecl: 'attributed(enum StillMedium: short)'
 attr: packed
  enumField: 'int' (value: -32768)
   name: E
   init:
    negateExpr: 'int' (value: -32768)
     operand:
      intLiteral: 'int' (value: 32768)

  enumField: 'int' (value: 32767)
   name: F
   init:
    intLiteral: 'int' (value: 32767)

staticAssert
 condition:
  implicit cast: (IntToBool) '_Bool'
    equalExpr: 'int' (value: 1)
     lhs:
      sizeofExpr: 'unsigned long'
       operand type: attributed(enum StillMedium: short)
     rhs:
      implicit cast: (IntCast) 'unsigned long'
        intLiteral: 'int' (value: 2)
 diagnostic:
  stringLiteralExpr: '[12]char' lvalue (value: "StillMedium")

enumDecl: 'enum Normal: int'
  enumField: 'int' (value: -2147483648)
   name: G
   init:
    implicit cast: (IntCast) 'int'
      negateExpr: 'long' (value: -2147483648)
       operand:
        intLiteral: 'long' (value: 2147483648)

  enumField: 'int' (value: 2147483647)
   name: H
   init:
    intLiteral: 'int' (value: 2147483647)

staticAssert
 condition:
  implicit cast: (IntToBool) '_Bool'
    equalExpr: 'int' (value: 1)
     lhs:
      sizeofExpr: 'unsigned long'
       operand type: enum Normal: int
     rhs:
      implicit cast: (IntCast) 'unsigned long'
        intLiteral: 'int' (value: 4)
 diagnostic:
  stringLiteralExpr: '[7]char' lvalue (value: "Normal")

enumDecl: 'enum Unsigned: unsigned int'
  enumField: 'unsigned int' (value: 4294967295)
   name: I
   init:
    implicit cast: (IntCast) 'unsigned int'
      intLiteral: 'long' (value: 4294967295)

staticAssert
 condition:
  implicit cast: (IntToBool) '_Bool'
    equalExpr: 'int' (value: 1)
     lhs:
      sizeofExpr: 'unsigned long'
       operand type: enum Unsigned: unsigned int
     rhs:
      implicit cast: (IntCast) 'unsigned long'
        intLiteral: 'int' (value: 4)
 diagnostic:
  stringLiteralExpr: '[9]char' lvalue (value: "Unsigned")

enumDecl: 'enum Large: long'
  enumField: 'int' (value: -1)
   name: J
   init:
    negateExpr: 'int' (value: -1)
     operand:
      intLiteral: 'int' (value: 1)

  enumField: 'long' (value: 4294967295)
   name: K
   init:
    intLiteral: 'long' (value: 4294967295)

staticAssert
 condition:
  implicit cast: (IntToBool) '_Bool'
    equalExpr: 'int' (value: 1)
     lhs:
      sizeofExpr: 'unsigned long'
       operand type: enum Large: long
     rhs:
      implicit cast: (IntCast) 'unsigned long'
        intLiteral: 'int' (value: 8)
 diagnostic:
  stringLiteralExpr: '[6]char' lvalue (value: "Large")

enumDecl: 'enum Huge: unsigned long'
  enumField: 'unsigned long' (value: 18446744073709551615)
   name: L
   init:
    implicit cast: (IntCast) 'unsigned long'
      intLiteral: 'unsigned long long' (value: 18446744073709551615)

staticAssert
 condition:
  implicit cast: (IntToBool) '_Bool'
    equalExpr: 'int' (value: 1)
     lhs:
      sizeofExpr: 'unsigned long'
       operand type: enum Huge: unsigned long
     rhs:
      implicit cast: (IntCast) 'unsigned long'
        intLiteral: 'int' (value: 8)
 diagnostic:
  stringLiteralExpr: '[5]char' lvalue (value: "Huge")

enumDecl: 'enum EnumWithInits: long long'
  enumField: 'int' (value: -2)
   name: Negative
   init:
    negateExpr: 'int' (value: -2)
     operand:
      intLiteral: 'int' (value: 2)

  enumField: 'long long' (value: 18446744073709551615)
   name: Positive
   init:
    implicit cast: (IntCast) 'long long'
      intLiteral: 'unsigned long' (value: 18446744073709551615)

