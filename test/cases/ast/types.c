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

variable: 'attributed(int)'
 attr: aligned alignment: .{ .node = .null, .requested = 4 }
 attr: aligned alignment: .{ .node = .null, .requested = 4 }
 attr: aligned alignment: .{ .node = @enumFromInt(5), .requested = 16 }
 name: a

variable: 'const volatile int'
 name: b

variable: 'const volatile int'
 name: c

variable: 'const volatile int'
 name: d

fnProto: 'fn (a: restrict *int, b: restrict *int, c: restrict *int) int'
 name: foo

fnProto: 'fn (n: int, bar: decayed *[<expr>]int) int'
 name: bar

typedef: 'void'
 name: baz

fnProto: 'attributed(fn () void)'
 attr: noreturn
 name: abort

typedef: 'int'
 name: A

typedef: 'A: int'
 name: B

typedef: 'A: int'
 name: C

typedef: 'C: A: int'
 name: B

typedef: '[2]int'
 name: I

fnDef: 'fn (a: decayed *const I: [2]int, b: decayed *const I: [2]int) void'
 name: qux
 body:
  compoundStmt
    addAssignExpr: 'decayed *const I: [2]int'
     lhs:
      declRefExpr: 'decayed *const I: [2]int' lvalue
       name: b
     rhs:
      implicit cast: (IntToPointer) 'decayed *const I: [2]int'
        intLiteral: 'int' (value: 1)

    addAssignExpr: 'decayed *const I: [2]int'
     lhs:
      declRefExpr: 'decayed *const I: [2]int' lvalue
       name: a
     rhs:
      implicit cast: (IntToPointer) 'decayed *const I: [2]int'
        intLiteral: 'int' (value: 1)

    implicit returnStmt: 'void'

enumDecl: 'enum E: unsigned int'
  enumField: 'int' (value: 2)
   name: D
   init:
    implicit cast: (IntCast) 'int'
      cast: (IntCast) 'char' (value: 2)
        intLiteral: 'int' (value: 2)

  enumField: 'int' (value: 3)
   name: E
   init:
    implicit cast: (IntCast) 'int'
      cast: (IntCast) 'long' (value: 3)
        intLiteral: 'int' (value: 3)

