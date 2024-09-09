Var: 'attributed(int)'
 attr: aligned alignment: Lexer.Attribute.Alignment{ .node = AST.AST.NodeIndex.none, .requested = 4 }
 attr: aligned alignment: Lexer.Attribute.Alignment{ .node = AST.AST.NodeIndex.none, .requested = 4 }
 attr: aligned alignment: Lexer.Attribute.Alignment{ .node = AST.AST.NodeIndex(1), .requested = 16 }
 name: a

Var: 'const volatile int'
 name: b

Var: 'const volatile int'
 name: c

Var: 'const volatile int'
 name: d

FnProto: 'fn (a: restrict *int, b: restrict *int, c: restrict *int) int'
 name: foo

FnProto: 'fn (n: int, bar: *d[<expr>]int) int'
 name: bar

TypeDef: 'void'
 name: baz

FnProto: 'attributed(fn () void)'
 attr: noreturn
 name: abort

TypeDef: 'int'
 name: A

TypeDef: 'int'
 name: B

TypeDef: 'int'
 name: C

TypeDef: 'int'
 name: B

TypeDef: '[2]int'
 name: I

FnDef: 'fn (a: d[2]const int, b: d[2]const int) void'
 name: qux
 body:
  CompoundStmt: 'void'
    AddAssignExpr: 'd[2]const int'
     lhs:
      DeclRefExpr: 'd[2]const int' lvalue
       name: b
     rhs:
      ImplicitCast: (IntToPointer) 'd[2]const int'
        IntLiteral: 'int' (value: 1)

    AddAssignExpr: 'd[2]const int'
     lhs:
      DeclRefExpr: 'd[2]const int' lvalue
       name: a
     rhs:
      ImplicitCast: (IntToPointer) 'd[2]const int'
        IntLiteral: 'int' (value: 1)

    ImplicitReturn: 'void'

EnumDeclTwo: 'enum E: unsigned int'
  EnumFieldDecl: 'int' (value: 2)
   name: D
   value:
    ImplicitCast: (IntCast) 'int'
      ExplicitCast: (IntCast) 'char' (value: 2)
        IntLiteral: 'int' (value: 2)
  EnumFieldDecl: 'int' (value: 3)
   name: E
   value:
    ImplicitCast: (IntCast) 'int'
      ExplicitCast: (IntCast) 'long' (value: 3)
        IntLiteral: 'int' (value: 3)

