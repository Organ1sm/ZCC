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

variable: 'bool'
 name: a
 init:
  boolLiteral: 'bool' (value: true)

variable: 'bool'
 name: b
 init:
  boolLiteral: 'bool' (value: false)

variable: 'bool'
 name: c
 init:
  implicit cast: (IntToBool) 'bool' (value: false)
    intLiteral: 'int' (value: 0)

variable: 'bool'
 name: d
 init:
  implicit cast: (IntToBool) 'bool' (value: true)
    intLiteral: 'int' (value: 1)

variable: 'int'
 name: e
 init:
  implicit cast: (BoolToInt) 'int' (value: 1)
    boolLiteral: 'bool' (value: true)

variable: 'int'
 name: f
 init:
  implicit cast: (BoolToInt) 'int' (value: 0)
    boolLiteral: 'bool' (value: false)

variable: 'int'
 name: g
 init:
  addExpr: 'int' (value: 2)
   lhs:
    implicit cast: (BoolToInt) 'int'
      boolLiteral: 'bool' (value: true)
   rhs:
    intLiteral: 'int' (value: 1)

