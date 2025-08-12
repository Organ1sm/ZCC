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

variable: 'attributed([1]int)'
 attr: aligned alignment: null
 name: arr
 init:
  arrayInitExpr: '[1]int'
    intLiteral: 'int' (value: 0)

variable: '*int'
 name: ptr
 init:
  implicit cast: (ArrayToPointer) 'decayed *attributed([1]int)' (value: &arr)
    declRefExpr: 'attributed([1]int)' lvalue
     attr: aligned alignment: null
     name: arr

fnDef: 'fn () void'
 name: foo
 body:
  compoundStmt
    variable: 'attributed([64]char)'
     attr: aligned alignment: .{ .node = @enumFromInt(13), .requested = 8 }
     name: x

    variable: '*char'
     name: y
     init:
      addrOfExpr: '*char' (value: &x)
       operand:
        arrayAccessExpr: 'char' lvalue
         lhs:
          implicit cast: (ArrayToPointer) 'decayed *attributed([64]char)'
            parenExpr: 'attributed([64]char)' lvalue
             attr: aligned alignment: .{ .node = @enumFromInt(13), .requested = 8 }
             operand:
              declRefExpr: 'attributed([64]char)' lvalue
               attr: aligned alignment: .{ .node = @enumFromInt(13), .requested = 8 }
               name: x
         index:
          intLiteral: 'int' (value: 0)

    implicit returnStmt: 'void'

