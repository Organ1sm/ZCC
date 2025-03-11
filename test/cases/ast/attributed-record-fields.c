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

structDecl: 'struct S1'

structDecl: 'struct S2'
  recordField: 'int'
   name: x
   field attr: packed

structDecl: 'struct S3'
  recordField: 'int'
   name: x
   field attr: packed

structDecl: 'struct S4'
  recordField: 'int'
   name: x
   field attr: packed

structDecl: 'struct S5'
  recordField: 'int'
   name: x
   field attr: packed

  recordField: 'int'
   name: y
   field attr: packed

structDecl: 'struct S6'
  recordField: 'int'
   name: x
   field attr: packed

  recordField: 'int'
   name: y
   field attr: packed

structDecl: 'struct S7'
  recordField: 'int'
   name: x
   field attr: packed

  recordField: 'int'
   name: y

structDecl: 'struct S8'
  recordField: 'int'
   name: x

  recordField: 'int'
   name: y
   field attr: packed

structDecl: 'struct S9'
  recordField: 'int'
   name: x
   field attr: packed
   field attr: aligned alignment: null

  recordField: 'float'
   name: f

  recordField: 'long'
   name: l
   field attr: packed
   field attr: aligned alignment: zinc.Lexer.Attribute.Alignment{ .node = zinc.AST.AST.Node.OptIndex.null, .requested = 16 }
   field attr: warn_if_not_aligned alignment: zinc.Lexer.Attribute.Alignment{ .node = zinc.AST.AST.Node.OptIndex.null, .requested = 16 }

unionDecl: 'union U1'
  recordField: 'long'
   name: x

  recordField: 'int'
   name: y
   field attr: aligned alignment: zinc.Lexer.Attribute.Alignment{ .node = zinc.AST.AST.Node.OptIndex.null, .requested = 32 }

  recordField: 'unsigned int'
   name: z
   field attr: packed

unionDecl: 'union U2'
  recordField: 'int'
   name: x
   field attr: packed

  recordField: 'int'
   name: y

  recordField: 'int'
   name: z
   field attr: packed

  recordField: 'int'
   name: w
   field attr: aligned alignment: null

