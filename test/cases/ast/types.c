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
 attr: aligned alignment: zinc.Lexer.Attribute.Alignment{ .node = zinc.AST.AST.Node.OptIndex.null, .requested = 4 }
 attr: aligned alignment: zinc.Lexer.Attribute.Alignment{ .node = zinc.AST.AST.Node.OptIndex.null, .requested = 4 }
 attr: aligned alignment: zinc.Lexer.Attribute.Alignment{ .node = zinc.AST.AST.Node.OptIndex(5), .requested = 16 }
 name: a

variable: 'const volatile int'
 name: b

variable: 'const volatile int'
 name: c

variable: 'const volatile int'
 name: d

fnProto: 'fn (a: restrict *int, b: restrict *int, c: restrict *int) int'
 name: foo

fnProto: 'fn (n: int, bar: *d[<expr>]int) int'
 name: bar

typedef: 'void'
 name: baz

fnProto: 'attributed(fn () void)'
 attr: noreturn
 name: abort

typedef: 'int'
 name: A

typedef: 'A': 'int'
 name: B

typedef: 'A': 'int'
 name: C

typedef: 'C': 'int'
 name: B

typedef: '[2]int'
 name: I

fnDef: 'fn (a: *d[2]const int, b: *d[2]const int) void'
 name: qux
 body:
  compoundStmt: 'void'
    addAssignExpr: 'I': '*d[2]const int'
     lhs:
      declRefExpr: 'I': '*d[2]const int' lvalue
       name: b
     rhs:
      implicit cast: (IntToPointer) 'I': '*d[2]const int'
        intLiteral: 'int' (value: 1)

    addAssignExpr: 'I': '*d[2]const int'
     lhs:
      declRefExpr: 'I': '*d[2]const int' lvalue
       name: a
     rhs:
      implicit cast: (IntToPointer) 'I': '*d[2]const int'
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

