variable: 'attributed([1]int)'
 attr: aligned alignment: null
 name: arr
 init:
  arrayInitExpr: '[1]int'
    intLiteral: 'int' (value: 0)

variable: '*int'
 name: ptr
 init:
  implicitCast: (ArrayToPointer) '*d:attributed([1]int)'
   attr: aligned alignment: null
    declRefExpr: 'attributed([1]int)' lvalue
     attr: aligned alignment: null
     name: arr

fnDef: 'fn () void'
 name: foo
 body:
  compoundStmt: 'void'
    variable: 'attributed([64]char)'
     attr: aligned alignment: zinc.Lexer.Attribute.Alignment{ .node = zinc.AST.AST.Node.OptIndex(8), .requested = 8 }
     name: x

    variable: '*char'
     name: y
     init:
      addrOfExpr: '*char'
       operand:
        arrayAccessExpr: 'char' lvalue
         lhs:
          implicitCast: (ArrayToPointer) '*d:attributed([64]char)'
           attr: aligned alignment: zinc.Lexer.Attribute.Alignment{ .node = zinc.AST.AST.Node.OptIndex(8), .requested = 8 }
            parenExpr: 'attributed([64]char)' lvalue
             attr: aligned alignment: zinc.Lexer.Attribute.Alignment{ .node = zinc.AST.AST.Node.OptIndex(8), .requested = 8 }
             operand:
              declRefExpr: 'attributed([64]char)' lvalue
               attr: aligned alignment: zinc.Lexer.Attribute.Alignment{ .node = zinc.AST.AST.Node.OptIndex(8), .requested = 8 }
               name: x
         index:
          intLiteral: 'int' (value: 0)

    implicitReturn: 'void'

