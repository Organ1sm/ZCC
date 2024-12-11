Var: 'attributed([1]int)'
 attr: aligned alignment: null
 name: arr
 init:
  ArrayInitExprTwo: '[1]int'
    IntLiteral: 'int' (value: 0)

Var: '*int'
 name: ptr
 init:
  ImplicitCast: (ArrayToPointer) '*d:attributed([1]int)'
   attr: aligned alignment: null
    DeclRefExpr: 'attributed([1]int)' lvalue
     attr: aligned alignment: null
     name: arr

FnDef: 'fn () void'
 name: foo
 body:
  CompoundStmt: 'void'
    Var: 'attributed([64]char)'
     attr: aligned alignment: zinc.Lexer.Attribute.Alignment{ .node = zinc.AST.AST.NodeIndex(9), .requested = 8 }
     name: x

    Var: '*char'
     name: y
     init:
      AddrOfExpr: '*char'
       operand:
        ArrayAccessExpr: 'char' lvalue
         lhs:
          ImplicitCast: (ArrayToPointer) '*d:attributed([64]char)'
           attr: aligned alignment: zinc.Lexer.Attribute.Alignment{ .node = zinc.AST.AST.NodeIndex(9), .requested = 8 }
            ParenExpr: 'attributed([64]char)' lvalue
             attr: aligned alignment: zinc.Lexer.Attribute.Alignment{ .node = zinc.AST.AST.NodeIndex(9), .requested = 8 }
             operand:
              DeclRefExpr: 'attributed([64]char)' lvalue
               attr: aligned alignment: zinc.Lexer.Attribute.Alignment{ .node = zinc.AST.AST.NodeIndex(9), .requested = 8 }
               name: x
         index:
          IntLiteral: 'int' (value: 0)

    ImplicitReturn: 'void'

