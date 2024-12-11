StructDeclTwo: 'struct S1'

StructDeclTwo: 'struct S2'
  RecordFieldDecl: 'int'
   name: x
   field attr: packed

StructDeclTwo: 'struct S3'
  RecordFieldDecl: 'int'
   name: x
   field attr: packed

StructDeclTwo: 'struct S4'
  RecordFieldDecl: 'int'
   name: x
   field attr: packed

StructDeclTwo: 'struct S5'
  RecordFieldDecl: 'int'
   name: x
   field attr: packed
  RecordFieldDecl: 'int'
   name: y
   field attr: packed

StructDeclTwo: 'struct S6'
  RecordFieldDecl: 'int'
   name: x
   field attr: packed
  RecordFieldDecl: 'int'
   name: y
   field attr: packed

StructDeclTwo: 'struct S7'
  RecordFieldDecl: 'int'
   name: x
   field attr: packed
  RecordFieldDecl: 'int'
   name: y

StructDeclTwo: 'struct S8'
  RecordFieldDecl: 'int'
   name: x
  RecordFieldDecl: 'int'
   name: y
   field attr: packed

StructDecl: 'struct S9'
  RecordFieldDecl: 'int'
   name: x
   field attr: packed
   field attr: aligned alignment: null

  RecordFieldDecl: 'float'
   name: f

  RecordFieldDecl: 'long'
   name: l
   field attr: packed
   field attr: aligned alignment: zinc.Lexer.Attribute.Alignment{ .node = zinc.AST.AST.NodeIndex.none, .requested = 16 }
   field attr: warn_if_not_aligned alignment: zinc.Lexer.Attribute.Alignment{ .node = zinc.AST.AST.NodeIndex.none, .requested = 16 }

UnionDecl: 'union U1'
  RecordFieldDecl: 'long'
   name: x

  RecordFieldDecl: 'int'
   name: y
   field attr: aligned alignment: zinc.Lexer.Attribute.Alignment{ .node = zinc.AST.AST.NodeIndex.none, .requested = 32 }

  RecordFieldDecl: 'unsigned int'
   name: z
   field attr: packed

UnionDecl: 'union U2'
  RecordFieldDecl: 'int'
   name: x
   field attr: packed

  RecordFieldDecl: 'int'
   name: y

  RecordFieldDecl: 'int'
   name: z
   field attr: packed

  RecordFieldDecl: 'int'
   name: w
   field attr: aligned alignment: null

