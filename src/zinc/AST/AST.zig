const std = @import("std");

const Interner = @import("backend").Interner;

const Attribute = @import("../Lexer/Attribute.zig");
const CodeGen = @import("../CodeGen/CodeGen.zig");
const Compilation = @import("../Basic/Compilation.zig");
const Lexer = @import("../Lexer/Lexer.zig");
const NumberAffixes = @import("NumberAffixes.zig");
const Source = @import("../Basic/Source.zig");
const QualType = @import("TypeStore.zig").QualType;
const Value = @import("Value.zig");

const Tree = @This();

pub const TokenIndex = u32;
pub const ValueMap = std.AutoHashMapUnmanaged(Node.Index, Value);

comp: *Compilation,

/// Values from Preprocessor
tokens: Token.List.Slice,

// Values owned by this Tree
nodes: std.MultiArrayList(Node.Repr) = .empty,
extra: std.ArrayList(u32) = .empty,
rootDecls: std.ArrayList(Node.Index) = .empty,
valueMap: ValueMap = .empty,

pub const genIR = CodeGen.generateIR;

pub fn deinit(tree: *Tree) void {
    tree.nodes.deinit(tree.comp.gpa);
    tree.extra.deinit(tree.comp.gpa);
    tree.rootDecls.deinit(tree.comp.gpa);
    tree.valueMap.deinit(tree.comp.gpa);
    tree.* = undefined;
}

pub const GNUAssemblyQualifiers = struct {
    @"volatile": bool = false,
    @"inline": bool = false,
    goto: bool = false,
};

pub const Token = struct {
    id: TokenType,
    loc: Source.Location,

    pub inline fn is(self: Token, kind: TokenType) bool {
        return self.id == kind;
    }

    pub inline fn isNot(self: Token, kind: TokenType) bool {
        return self.id != kind;
    }

    pub fn isOneOf(self: Token, kinds: anytype) bool {
        inline for (kinds) |k| {
            if (self.id == k) {
                return true;
            }
        }
        return false;
    }
    pub const TokenType = @import("../Basic/TokenType.zig").TokenType;
    pub const List = std.MultiArrayList(Token);
    pub const NumberPrefix = NumberAffixes.Prefix;
    pub const NumberSuffix = NumberAffixes.Suffix;
};

pub const TokenWithExpansionLocs = struct {
    id: Token.TokenType,
    flags: packed struct {
        expansionDisabled: bool = false,
        isMacroArg: bool = false,
    } = .{},
    /// This location contains the actual token slice which might be generated.
    /// If it is generated then there is guaranteed to be at least one expansion location.
    loc: Source.Location,
    expansionLocs: ?[*]Source.Location = null,

    pub fn expansionSlice(tok: TokenWithExpansionLocs) []const Source.Location {
        const locs = tok.expansionLocs orelse return &[0]Source.Location{};
        var i: usize = 0;
        while (locs[i].id != .unused) : (i += 1) {}
        return locs[0..i];
    }

    pub fn addExpansionLocation(tok: *TokenWithExpansionLocs, gpa: std.mem.Allocator, new: []const Source.Location) !void {
        if (new.len == 0 or tok.id == .WhiteSpace or tok.id == .MacroWS or tok.id == .PlaceMarker) return;
        var list: std.ArrayList(Source.Location) = .empty;
        defer {
            @memset(list.items.ptr[list.items.len..list.capacity], .{});
            // add a sentinel since the allocator is not guaranteed
            // to return the exact desired size
            if (list.capacity > 0)
                list.items.ptr[list.capacity - 1].byteOffset = 1;
            tok.expansionLocs = list.items.ptr;
        }

        if (tok.expansionLocs) |locs| {
            var i: usize = 0;
            while (locs[i].id != .unused) : (i += 1) {}
            list.items = locs[0..i];
            while (locs[i].byteOffset != 1) : (i += 1) {}
            list.capacity = i + 1;
        }

        const minLen = @max(list.items.len + new.len + 1, 4);
        const wantedLen = std.math.ceilPowerOfTwo(usize, minLen) catch
            return error.OutOfMemory;
        try list.ensureTotalCapacity(gpa, wantedLen);

        for (new) |newLoc| {
            if (newLoc.id == .generated) continue;
            list.appendAssumeCapacity(newLoc);
        }
    }

    pub fn free(expansionLocs: ?[*]Source.Location, gpa: std.mem.Allocator) void {
        const locs = expansionLocs orelse return;
        var i: usize = 0;
        while (locs[i].id != .unused) : (i += 1) {}
        while (locs[i].byteOffset != 1) : (i += 1) {}
        gpa.free(locs[0 .. i + 1]);
    }

    pub fn dupe(tok: TokenWithExpansionLocs, gpa: std.mem.Allocator) !TokenWithExpansionLocs {
        var copy = tok;
        copy.expansionLocs = null;
        try copy.addExpansionLocation(gpa, tok.expansionSlice());
        return copy;
    }

    pub fn checkMsEof(tok: TokenWithExpansionLocs, source: Source, comp: *Compilation) !void {
        std.debug.assert(tok.id == .Eof);
        if (source.buffer.len > tok.loc.byteOffset and source.buffer[tok.loc.byteOffset] == 0x1A) {
            const diagnostic: Compilation.Diagnostic = .ctrl_z_eof;
            try comp.diagnostics.add(.{
                .text = diagnostic.fmt,
                .kind = diagnostic.kind,
                .opt = diagnostic.opt,
                .extension = diagnostic.extension,
                .location = source.getLineCol(.{
                    .id = source.id,
                    .byteOffset = tok.loc.byteOffset,
                    .line = tok.loc.line,
                }),
            });
        }
    }

    pub inline fn is(self: TokenWithExpansionLocs, kind: Token.TokenType) bool {
        return self.id == kind;
    }

    pub inline fn isNot(self: TokenWithExpansionLocs, kind: Token.TokenType) bool {
        return self.id != kind;
    }

    pub fn isOneOf(self: TokenWithExpansionLocs, kinds: anytype) bool {
        inline for (kinds) |k| {
            if (self.id == k) {
                return true;
            }
        }
        return false;
    }
};

pub const Node = union(enum) {
    emptyDecl: EmptyDecl,
    staticAssert: StaticAssert,
    fnProto: FnProto,
    fnDef: FnDef,
    param: Param,
    variable: Variable,
    typedef: Typedef,
    globalAsm: SimpleAsm,

    structDecl: ContainerDecl,
    unionDecl: ContainerDecl,
    enumDecl: ContainerDecl,
    structForwardDecl: ContainerForwardDecl,
    unionForwardDecl: ContainerForwardDecl,
    enumForwardDecl: ContainerForwardDecl,

    enumField: EnumField,
    recordField: RecordField,

    labeledStmt: LabeledStmt,
    compoundStmt: CompoundStmt,
    ifStmt: IfStmt,
    switchStmt: SwitchStmt,
    caseStmt: CaseStmt,
    defaultStmt: DefaultStmt,
    whileStmt: WhileStmt,
    doWhileStmt: DoWhileStmt,
    forStmt: ForStmt,
    gotoStmt: GotoStmt,
    computedGotoStmt: ComputedGotoStmt,
    continueStmt: ContinueStmt,
    breakStmt: BreakStmt,
    nullStmt: NullStmt,
    returnStmt: ReturnStmt,

    gnuAsmSimple: SimpleAsm,

    assignExpr: Binary,
    mulAssignExpr: Binary,
    divAssignExpr: Binary,
    modAssignExpr: Binary,
    addAssignExpr: Binary,
    subAssignExpr: Binary,
    shlAssignExpr: Binary,
    shrAssignExpr: Binary,
    bitAndAssignExpr: Binary,
    bitXorAssignExpr: Binary,
    bitOrAssignExpr: Binary,

    commaExpr: Binary,
    boolOrExpr: Binary,
    boolAndExpr: Binary,
    bitOrExpr: Binary,
    bitXorExpr: Binary,
    bitAndExpr: Binary,
    equalExpr: Binary,
    notEqualExpr: Binary,
    lessThanExpr: Binary,
    lessThanEqualExpr: Binary,
    greaterThanExpr: Binary,
    greaterThanEqualExpr: Binary,
    shlExpr: Binary,
    shrExpr: Binary,
    addExpr: Binary,
    subExpr: Binary,
    mulExpr: Binary,
    divExpr: Binary,
    modExpr: Binary,

    cast: Cast,

    addrOfExpr: Unary,
    compoundAssignDummyExpr: Unary,
    derefExpr: Unary,
    plusExpr: Unary,
    negateExpr: Unary,
    bitNotExpr: Unary,
    boolNotExpr: Unary,
    preIncExpr: Unary,
    preDecExpr: Unary,
    imagExpr: Unary,
    realExpr: Unary,
    postIncExpr: Unary,
    postDecExpr: Unary,
    parenExpr: Unary,
    stmtExpr: Unary,

    addrOfLabel: AddrOfLabel,
    arrayAccessExpr: ArrayAccess,

    memberAccessExpr: MemberAccess,
    memberAccessPtrExpr: MemberAccess,

    callExpr: Call,

    declRefExpr: DeclRef,
    enumerationRef: DeclRef,

    builtinCallExpr: BuiltinCall,
    builtinRef: BuiltinRef,
    builtinChooseExpr: Conditional,
    builtinTypesCompatibleP: TypesCompatible,

    /// C23 bool literal `true` / `false`
    boolLiteral: Literal,
    /// C23 nullptr literal
    nullptrLiteral: Literal,
    /// integer literal, always unsigned
    intLiteral: Literal,
    /// Same as int_literal, but originates from a char literal
    charLiteral: CharLiteral,
    /// a floating point literal
    floatLiteral: Literal,
    stringLiteralExpr: CharLiteral,
    /// wraps a float or double literal: un
    imaginaryLiteral: Unary,
    /// A compound literal (type){ init }
    compoundLiteralExpr: CompoundLiteral,

    sizeofExpr: TypeInfo,
    alignofExpr: TypeInfo,

    genericExpr: GenericExpr,
    genericAssociationExpr: GenericExpr.Association,
    genericDefaultExpr: GenericExpr.Default,

    binaryCondExpr: Conditional,
    /// Used as the base for casts of the lhs in `binary_cond_expr`.
    condDummyExpr: Unary,
    condExpr: Conditional,

    arrayInitExpr: ContainerInit,
    structInitExpr: ContainerInit,
    unionInitExpr: UnionInit,
    /// Inserted in array_init_expr to represent unspecified elements.
    /// data.int contains the amount of elements.
    arrayFillerExpr: ArrayFiller,
    /// Inserted in record and scalar initializers for unspecified elements.
    defaultInitExpr: DefaultInit,

    pub const EmptyDecl = struct { semicolon: TokenIndex };

    pub const StaticAssert = struct {
        assertToken: TokenIndex,
        cond: Node.Index,
        message: ?Node.Index,
    };

    pub const FnProto = struct {
        nameToken: TokenIndex,
        qt: QualType,
        static: bool,
        @"inline": bool,
        /// The definition for this prototype if one exists.
        definition: ?Node.Index,
    };

    pub const FnDef = struct {
        nameToken: TokenIndex,
        qt: QualType,
        static: bool,
        @"inline": bool,
        body: Node.Index,
    };

    pub const Param = struct {
        nameToken: TokenIndex,
        qt: QualType,
        storageClass: enum { auto, register },
    };

    pub const Variable = struct {
        nameToken: TokenIndex,
        qt: QualType,
        storageClass: enum { auto, static, @"extern", register },
        threadLocal: bool,
        /// From predefined macro  __func__, __FUNCTION__ or __PRETTY_FUNCTION__.
        /// Implies `static == true`.
        implicit: bool,
        initializer: ?Node.Index,
    };

    pub const Typedef = struct {
        nameToken: TokenIndex,
        qt: QualType,
        implicit: bool,
    };

    pub const SimpleAsm = struct {
        asmToken: TokenIndex,
        asmString: Node.Index,
    };

    pub const ContainerDecl = struct {
        nameOrKindToken: TokenIndex,
        containerQt: QualType,
        fields: []const Node.Index,
    };

    pub const ContainerForwardDecl = struct {
        nameOrKindToken: TokenIndex,
        containerQt: QualType,
        /// The definition for this forward declaration if one exists.
        definition: ?Node.Index,
    };

    pub const EnumField = struct {
        nameToken: TokenIndex,
        qt: QualType,
        init: ?Node.Index,
    };

    pub const RecordField = struct {
        nameOrFirstToken: TokenIndex,
        qt: QualType,
        bitWidth: ?Node.Index,
    };

    pub const LabeledStmt = struct {
        labelToken: TokenIndex,
        body: Node.Index,
        qt: QualType,
    };

    pub const CompoundStmt = struct {
        lbraceToken: TokenIndex,
        body: []const Node.Index,
    };

    pub const IfStmt = struct {
        ifToken: TokenIndex,
        cond: Node.Index,
        thenBody: Node.Index,
        elseBody: ?Node.Index,
    };

    pub const SwitchStmt = struct {
        switchToken: TokenIndex,
        cond: Node.Index,
        body: Node.Index,
    };

    pub const CaseStmt = struct {
        caseToken: TokenIndex,
        start: Node.Index,
        end: ?Node.Index,
        body: Node.Index,
    };

    pub const DefaultStmt = struct {
        defaultToken: TokenIndex,
        body: Node.Index,
    };

    pub const WhileStmt = struct {
        whileToken: TokenIndex,
        cond: Node.Index,
        body: Node.Index,
    };

    pub const DoWhileStmt = struct {
        doToken: TokenIndex,
        cond: Node.Index,
        body: Node.Index,
    };

    pub const ForStmt = struct {
        forToken: TokenIndex,
        init: union(enum) {
            decls: []const Node.Index,
            expr: ?Node.Index,
        },
        cond: ?Node.Index,
        incr: ?Node.Index,
        body: Node.Index,
    };

    pub const GotoStmt = struct { labelToken: TokenIndex };
    pub const ComputedGotoStmt = struct {
        gotoToken: TokenIndex,
        expr: Node.Index,
    };

    pub const ContinueStmt = struct { continueToken: TokenIndex };
    pub const BreakStmt = struct { breakToken: TokenIndex };

    pub const NullStmt = struct {
        semicolonOrRbraceToken: TokenIndex,
        qt: QualType,
    };

    pub const ReturnStmt = struct {
        returnToken: TokenIndex,
        returnQt: QualType,
        operand: union(enum) {
            expr: Node.Index,
            /// True if the function is called "main" and returnQt is compatible with int
            implicit: bool,
            none,
        },
    };

    pub const Binary = struct {
        qt: QualType,
        lhs: Node.Index,
        opToken: TokenIndex,
        rhs: Node.Index,
    };

    pub const Cast = struct {
        qt: QualType,
        lparen: TokenIndex,
        kind: Kind,
        operand: Node.Index,
        implicit: bool,

        pub const Kind = enum(u8) {
            /// Does nothing except possibly add qualifiers
            NoOP,
            /// Interpret one bit pattern as another. Used for operands which have the same
            /// size and unrelated types, e.g. casting one pointer type to another
            Bitcast,
            /// Convert T[] to T *
            ArrayToPointer,
            /// Convert an lvalue to an rvalue
            LValToRVal,
            /// Convert a function type to a pointer to a function
            FunctionToPointer,
            /// Convert a pointer type to a _Bool
            PointerToBool,
            /// Convert a pointer type to an integer type
            PointerToInt,
            /// Convert _Bool to an integer type
            BoolToInt,
            /// Convert _Bool to a floating type
            BoolToFloat,
            /// Convert a _Bool to a pointer; will cause a  warning
            BoolToPointer,
            /// Convert an integer type to _Bool
            IntToBool,
            /// Convert an integer to a floating type
            IntToFloat,
            /// Convert a complex integer to a complex floating type
            ComplexIntToComplexFloat,
            /// Convert an integer type to a pointer type
            IntToPointer,
            /// Convert a floating type to a _Bool
            FloatToBool,
            /// Convert a floating type to an integer
            FloatToInt,
            /// Convert a complex floating type to a complex integer
            ComplexFloatToComplexInt,
            /// Convert one integer type to another
            IntCast,
            /// Convert one complex integer type to another
            ComplexIntCast,
            /// Convert real part of complex integer to a integer
            ComplexIntToReal,
            /// Create a complex integer type using operand as the real part
            RealToComplexInt,
            /// Convert one floating type to another
            FloatCast,
            /// Convert one complex floating type to another
            ComplexFloatCast,
            /// Convert real part of complex float to a float
            ComplexFloatToReal,
            /// Create a complex floating type using operand as the real part
            RealToComplexFloat,
            /// Convert type to void
            ToVoid,
            /// Convert a literal 0 to a null pointer
            NullToPointer,
            /// GNU cast-to-union extension
            UnionCast,
            ///Create vector where each value is same as the input scalar
            VectorSplat,
            /// Convert an atomic type to its non atomic base type.
            AtomicToNonAtomic,
            /// Convert a non atomic type to an atomic type.
            NonAtomicToAtomic,
        };
    };

    pub const Unary = struct {
        qt: QualType,
        opToken: TokenIndex,
        operand: Node.Index,
    };

    pub const AddrOfLabel = struct {
        labelToken: TokenIndex,
        qt: QualType,
    };

    pub const ArrayAccess = struct {
        lbracketToken: TokenIndex,
        qt: QualType,
        base: Node.Index,
        index: Node.Index,
    };

    pub const MemberAccess = struct {
        qt: QualType,
        base: Node.Index,
        accessToken: TokenIndex,
        memberIndex: u32,

        pub fn isBitFieldWidth(access: MemberAccess, tree: *const Tree) ?u32 {
            var qt = access.base.qt(tree);
            if (qt.isInvalid()) return null;
            if (qt.get(tree.comp, .pointer)) |pointer| qt = pointer.child;
            const recordTy = switch (qt.base(tree.comp).type) {
                .@"struct", .@"union" => |record| record,
                else => return null,
            };
            return recordTy.fields[access.memberIndex].bitWidth.unpack();
        }
    };

    pub const Call = struct {
        lparenToken: TokenIndex,
        qt: QualType,
        callee: Node.Index,
        args: []const Node.Index,
    };

    pub const DeclRef = struct {
        nameToken: TokenIndex,
        qt: QualType,
        decl: Node.Index,
    };

    pub const BuiltinCall = struct {
        builtinToken: TokenIndex,
        qt: QualType,
        args: []const Node.Index,
    };

    pub const BuiltinRef = struct {
        nameToken: TokenIndex,
        qt: QualType,
    };

    pub const TypesCompatible = struct {
        builtinToken: TokenIndex,
        lhs: QualType,
        rhs: QualType,
    };

    pub const Literal = struct {
        literalToken: TokenIndex,
        qt: QualType,
    };

    pub const CharLiteral = struct {
        literalToken: TokenIndex,
        qt: QualType,
        kind: enum {
            ascii,
            wide,
            utf8,
            utf16,
            utf32,
        },
    };

    pub const CompoundLiteral = struct {
        lparenToken: TokenIndex,
        qt: QualType,
        threadLocal: bool,
        initializer: Node.Index,
        storageClass: enum { auto, static, register },
    };

    pub const GenericExpr = struct {
        genericToken: TokenIndex,
        qt: QualType,

        controlling: Node.Index,
        chosen: Node.Index,
        rest: []const Node.Index,

        pub const Association = struct {
            colonToken: TokenIndex,
            associationQt: QualType,
            expr: Node.Index,
        };

        pub const Default = struct {
            defaultToken: TokenIndex,
            expr: Node.Index,
        };
    };

    pub const Conditional = struct {
        condToken: TokenIndex,
        qt: QualType,
        cond: Node.Index,
        thenExpr: Node.Index,
        elseExpr: Node.Index,
    };

    pub const ContainerInit = struct {
        lbraceToken: TokenIndex,
        containerQt: QualType,
        items: []const Node.Index,
    };

    pub const UnionInit = struct {
        lbraceToken: TokenIndex,
        unionQt: QualType,
        fieldIndex: u32,
        initializer: ?Node.Index,
    };

    pub const ArrayFiller = struct {
        lastToken: TokenIndex,
        qt: QualType,
        count: u64,
    };

    pub const DefaultInit = struct {
        lastToken: TokenIndex,
        qt: QualType,
    };

    pub const TypeInfo = struct {
        qt: QualType,
        opToken: TokenIndex,
        expr: ?Node.Index,
        operandQt: QualType,
    };

    pub const Index = enum(u32) {
        _,

        pub fn get(index: Index, tree: *const Tree) Node {
            const nodeToken = tree.nodes.items(.tok)[@intFromEnum(index)];
            const nodeData = &tree.nodes.items(.data)[@intFromEnum(index)];
            const nodeTag = tree.nodes.items(.tag)[@intFromEnum(index)];
            return switch (nodeTag) {
                .EmptyDecl => .{ .emptyDecl = .{ .semicolon = nodeToken } },
                .StaticAssert => .{
                    .staticAssert = .{
                        .assertToken = nodeToken,
                        .cond = @enumFromInt(nodeData[0]),
                        .message = unpackOptIndex(nodeData[1]),
                    },
                },
                .FnProto => {
                    const attr: Node.Repr.DeclAttr = @bitCast(nodeData[1]);
                    return .{
                        .fnProto = .{
                            .nameToken = nodeToken,
                            .qt = @bitCast(nodeData[0]),
                            .static = attr.static,
                            .@"inline" = attr.@"inline",
                            // TODO decide how to handle definition
                            .definition = null,
                        },
                    };
                },
                .FnDef => {
                    const attr: Node.Repr.DeclAttr = @bitCast(nodeData[1]);
                    return .{
                        .fnDef = .{
                            .nameToken = nodeToken,
                            .qt = @bitCast(nodeData[0]),
                            .static = attr.static,
                            .@"inline" = attr.@"inline",
                            .body = @enumFromInt(nodeData[2]),
                        },
                    };
                },
                .Param => {
                    const attr: Node.Repr.DeclAttr = @bitCast(nodeData[1]);
                    return .{
                        .param = .{
                            .nameToken = nodeToken,
                            .qt = @bitCast(nodeData[0]),
                            .storageClass = if (attr.register) .register else .auto,
                        },
                    };
                },
                .Variable => {
                    const attr: Node.Repr.DeclAttr = @bitCast(nodeData[1]);
                    return .{
                        .variable = .{
                            .nameToken = nodeToken,
                            .qt = @bitCast(nodeData[0]),
                            .storageClass = if (attr.static)
                                .static
                            else if (attr.@"extern")
                                .@"extern"
                            else if (attr.register)
                                .register
                            else
                                .auto,
                            .threadLocal = attr.threadLocal,
                            .implicit = attr.implicit,
                            .initializer = unpackOptIndex(nodeData[2]),
                        },
                    };
                },
                .Typedef => .{
                    .typedef = .{
                        .nameToken = nodeToken,
                        .qt = @bitCast(nodeData[0]),
                        .implicit = nodeData[1] != 0,
                    },
                },
                .GlobalAsm => .{
                    .globalAsm = .{
                        .asmToken = nodeToken,
                        .asmString = @enumFromInt(nodeData[0]),
                    },
                },
                .StructDecl => .{
                    .structDecl = .{
                        .nameOrKindToken = nodeToken,
                        .containerQt = @bitCast(nodeData[0]),
                        .fields = @ptrCast(tree.extra.items[nodeData[1]..][0..nodeData[2]]),
                    },
                },
                .StructDeclTwo => .{
                    .structDecl = .{
                        .nameOrKindToken = nodeToken,
                        .containerQt = @bitCast(nodeData[0]),
                        .fields = unPackElems(nodeData[1..]),
                    },
                },
                .UnionDecl => .{
                    .unionDecl = .{
                        .nameOrKindToken = nodeToken,
                        .containerQt = @bitCast(nodeData[0]),
                        .fields = @ptrCast(tree.extra.items[nodeData[1]..][0..nodeData[2]]),
                    },
                },
                .UnionDeclTwo => .{
                    .unionDecl = .{
                        .nameOrKindToken = nodeToken,
                        .containerQt = @bitCast(nodeData[0]),
                        .fields = unPackElems(nodeData[1..]),
                    },
                },
                .EnumDecl => .{
                    .enumDecl = .{
                        .nameOrKindToken = nodeToken,
                        .containerQt = @bitCast(nodeData[0]),
                        .fields = @ptrCast(tree.extra.items[nodeData[1]..][0..nodeData[2]]),
                    },
                },
                .EnumDeclTwo => .{
                    .enumDecl = .{
                        .nameOrKindToken = nodeToken,
                        .containerQt = @bitCast(nodeData[0]),
                        .fields = unPackElems(nodeData[1..]),
                    },
                },
                .StructForwardDecl => .{
                    .structForwardDecl = .{
                        .nameOrKindToken = nodeToken,
                        .containerQt = @bitCast(nodeData[0]),
                        .definition = null,
                    },
                },
                .UnionForwardDecl => .{
                    .unionForwardDecl = .{
                        .nameOrKindToken = nodeToken,
                        .containerQt = @bitCast(nodeData[0]),
                        .definition = null,
                    },
                },
                .EnumForwardDecl => .{
                    .enumForwardDecl = .{
                        .nameOrKindToken = nodeToken,
                        .containerQt = @bitCast(nodeData[0]),
                        .definition = null,
                    },
                },
                .EnumField => .{
                    .enumField = .{
                        .nameToken = nodeToken,
                        .qt = @bitCast(nodeData[0]),
                        .init = unpackOptIndex(nodeData[1]),
                    },
                },
                .RecordField => .{
                    .recordField = .{
                        .nameOrFirstToken = nodeToken,
                        .qt = @bitCast(nodeData[0]),
                        .bitWidth = unpackOptIndex(nodeData[1]),
                    },
                },
                .LabeledStmt => .{
                    .labeledStmt = .{
                        .labelToken = nodeToken,
                        .qt = @bitCast(nodeData[0]),
                        .body = @enumFromInt(nodeData[1]),
                    },
                },
                .CompoundStmt => .{
                    .compoundStmt = .{
                        .lbraceToken = nodeToken,
                        .body = @ptrCast(tree.extra.items[nodeData[0]..][0..nodeData[1]]),
                    },
                },
                .CompoundStmtThree => .{
                    .compoundStmt = .{
                        .lbraceToken = nodeToken,
                        .body = unPackElems(nodeData),
                    },
                },
                .IfStmt => .{
                    .ifStmt = .{
                        .ifToken = nodeToken,
                        .cond = @enumFromInt(nodeData[0]),
                        .thenBody = @enumFromInt(nodeData[1]),
                        .elseBody = unpackOptIndex(nodeData[2]),
                    },
                },
                .SwitchStmt => .{
                    .switchStmt = .{
                        .switchToken = nodeToken,
                        .cond = @enumFromInt(nodeData[0]),
                        .body = @enumFromInt(nodeData[1]),
                    },
                },
                .CaseStmt => .{
                    .caseStmt = .{
                        .caseToken = nodeToken,
                        .start = @enumFromInt(nodeData[0]),
                        .end = unpackOptIndex(nodeData[1]),
                        .body = @enumFromInt(nodeData[2]),
                    },
                },
                .DefaultStmt => .{
                    .defaultStmt = .{
                        .defaultToken = nodeToken,
                        .body = @enumFromInt(nodeData[0]),
                    },
                },
                .WhileStmt => .{
                    .whileStmt = .{
                        .whileToken = nodeToken,
                        .cond = @enumFromInt(nodeData[0]),
                        .body = @enumFromInt(nodeData[1]),
                    },
                },
                .DoWhileStmt => .{
                    .doWhileStmt = .{
                        .doToken = nodeToken,
                        .cond = @enumFromInt(nodeData[0]),
                        .body = @enumFromInt(nodeData[1]),
                    },
                },
                .ForDecl => .{
                    .forStmt = .{
                        .forToken = nodeToken,
                        .init = .{ .decls = @ptrCast(tree.extra.items[nodeData[0]..][0 .. nodeData[1] - 2]) },
                        .cond = unpackOptIndex(tree.extra.items[nodeData[0] + nodeData[1] - 2]),
                        .incr = unpackOptIndex(tree.extra.items[nodeData[0] + nodeData[1] - 1]),
                        .body = @enumFromInt(nodeData[2]),
                    },
                },
                .ForExpr => .{
                    .forStmt = .{
                        .forToken = nodeToken,
                        .init = .{ .expr = unpackOptIndex(nodeData[0]) },
                        .cond = unpackOptIndex(tree.extra.items[nodeData[1]]),
                        .incr = unpackOptIndex(tree.extra.items[nodeData[1] + 1]),
                        .body = @enumFromInt(nodeData[2]),
                    },
                },
                .GotoStmt => .{
                    .gotoStmt = .{
                        .labelToken = nodeToken,
                    },
                },
                .ComputedGotoStmt => .{
                    .computedGotoStmt = .{
                        .gotoToken = nodeToken,
                        .expr = @enumFromInt(nodeData[0]),
                    },
                },
                .ContinueStmt => .{
                    .continueStmt = .{
                        .continueToken = nodeToken,
                    },
                },
                .BreakStmt => .{
                    .breakStmt = .{
                        .breakToken = nodeToken,
                    },
                },
                .NullStmt => .{
                    .nullStmt = .{
                        .semicolonOrRbraceToken = nodeToken,
                        .qt = @bitCast(nodeData[0]),
                    },
                },
                .ReturnStmt => .{
                    .returnStmt = .{
                        .returnToken = nodeToken,
                        .returnQt = @bitCast(nodeData[0]),
                        .operand = .{ .expr = @enumFromInt(nodeData[1]) },
                    },
                },
                .ReturnNoneStmt => .{
                    .returnStmt = .{
                        .returnToken = nodeToken,
                        .returnQt = @bitCast(nodeData[0]),
                        .operand = .none,
                    },
                },
                .ImplicitReturn => .{
                    .returnStmt = .{
                        .returnToken = nodeToken,
                        .returnQt = @bitCast(nodeData[0]),
                        .operand = .{ .implicit = nodeData[1] != 0 },
                    },
                },
                .GnuAsmSimple => .{
                    .gnuAsmSimple = .{
                        .asmToken = nodeToken,
                        .asmString = @enumFromInt(nodeData[0]),
                    },
                },
                .CommaExpr => .{
                    .commaExpr = .{
                        .opToken = nodeToken,
                        .qt = @bitCast(nodeData[0]),
                        .lhs = @enumFromInt(nodeData[1]),
                        .rhs = @enumFromInt(nodeData[2]),
                    },
                },
                .AssignExpr => .{
                    .assignExpr = .{
                        .opToken = nodeToken,
                        .qt = @bitCast(nodeData[0]),
                        .lhs = @enumFromInt(nodeData[1]),
                        .rhs = @enumFromInt(nodeData[2]),
                    },
                },
                .MulAssignExpr => .{
                    .mulAssignExpr = .{
                        .opToken = nodeToken,
                        .qt = @bitCast(nodeData[0]),
                        .lhs = @enumFromInt(nodeData[1]),
                        .rhs = @enumFromInt(nodeData[2]),
                    },
                },
                .DivAssignExpr => .{
                    .divAssignExpr = .{
                        .opToken = nodeToken,
                        .qt = @bitCast(nodeData[0]),
                        .lhs = @enumFromInt(nodeData[1]),
                        .rhs = @enumFromInt(nodeData[2]),
                    },
                },
                .ModAssignExpr => .{
                    .modAssignExpr = .{
                        .opToken = nodeToken,
                        .qt = @bitCast(nodeData[0]),
                        .lhs = @enumFromInt(nodeData[1]),
                        .rhs = @enumFromInt(nodeData[2]),
                    },
                },
                .AddAssignExpr => .{
                    .addAssignExpr = .{
                        .opToken = nodeToken,
                        .qt = @bitCast(nodeData[0]),
                        .lhs = @enumFromInt(nodeData[1]),
                        .rhs = @enumFromInt(nodeData[2]),
                    },
                },
                .SubAssignExpr => .{
                    .subAssignExpr = .{
                        .opToken = nodeToken,
                        .qt = @bitCast(nodeData[0]),
                        .lhs = @enumFromInt(nodeData[1]),
                        .rhs = @enumFromInt(nodeData[2]),
                    },
                },
                .ShlAssignExpr => .{
                    .shlAssignExpr = .{
                        .opToken = nodeToken,
                        .qt = @bitCast(nodeData[0]),
                        .lhs = @enumFromInt(nodeData[1]),
                        .rhs = @enumFromInt(nodeData[2]),
                    },
                },
                .ShrAssignExpr => .{
                    .shrAssignExpr = .{
                        .opToken = nodeToken,
                        .qt = @bitCast(nodeData[0]),
                        .lhs = @enumFromInt(nodeData[1]),
                        .rhs = @enumFromInt(nodeData[2]),
                    },
                },
                .BitAndAssignExpr => .{
                    .bitAndAssignExpr = .{
                        .opToken = nodeToken,
                        .qt = @bitCast(nodeData[0]),
                        .lhs = @enumFromInt(nodeData[1]),
                        .rhs = @enumFromInt(nodeData[2]),
                    },
                },
                .BitXorAssignExpr => .{
                    .bitXorAssignExpr = .{
                        .opToken = nodeToken,
                        .qt = @bitCast(nodeData[0]),
                        .lhs = @enumFromInt(nodeData[1]),
                        .rhs = @enumFromInt(nodeData[2]),
                    },
                },
                .BitOrAssignExpr => .{
                    .bitOrAssignExpr = .{
                        .opToken = nodeToken,
                        .qt = @bitCast(nodeData[0]),
                        .lhs = @enumFromInt(nodeData[1]),
                        .rhs = @enumFromInt(nodeData[2]),
                    },
                },
                .CompoundAssignDummyExpr => .{
                    .compoundAssignDummyExpr = .{
                        .opToken = nodeToken,
                        .qt = @bitCast(nodeData[0]),
                        .operand = @enumFromInt(nodeData[1]),
                    },
                },
                .BoolOrExpr => .{
                    .boolOrExpr = .{
                        .opToken = nodeToken,
                        .qt = @bitCast(nodeData[0]),
                        .lhs = @enumFromInt(nodeData[1]),
                        .rhs = @enumFromInt(nodeData[2]),
                    },
                },
                .BoolAndExpr => .{
                    .boolAndExpr = .{
                        .opToken = nodeToken,
                        .qt = @bitCast(nodeData[0]),
                        .lhs = @enumFromInt(nodeData[1]),
                        .rhs = @enumFromInt(nodeData[2]),
                    },
                },
                .BitOrExpr => .{
                    .bitOrExpr = .{
                        .opToken = nodeToken,
                        .qt = @bitCast(nodeData[0]),
                        .lhs = @enumFromInt(nodeData[1]),
                        .rhs = @enumFromInt(nodeData[2]),
                    },
                },
                .BitXorExpr => .{
                    .bitXorExpr = .{
                        .opToken = nodeToken,
                        .qt = @bitCast(nodeData[0]),
                        .lhs = @enumFromInt(nodeData[1]),
                        .rhs = @enumFromInt(nodeData[2]),
                    },
                },
                .BitAndExpr => .{
                    .bitAndExpr = .{
                        .opToken = nodeToken,
                        .qt = @bitCast(nodeData[0]),
                        .lhs = @enumFromInt(nodeData[1]),
                        .rhs = @enumFromInt(nodeData[2]),
                    },
                },
                .EqualExpr => .{
                    .equalExpr = .{
                        .opToken = nodeToken,
                        .qt = @bitCast(nodeData[0]),
                        .lhs = @enumFromInt(nodeData[1]),
                        .rhs = @enumFromInt(nodeData[2]),
                    },
                },
                .NotEqualExpr => .{
                    .notEqualExpr = .{
                        .opToken = nodeToken,
                        .qt = @bitCast(nodeData[0]),
                        .lhs = @enumFromInt(nodeData[1]),
                        .rhs = @enumFromInt(nodeData[2]),
                    },
                },
                .LessThanExpr => .{
                    .lessThanExpr = .{
                        .opToken = nodeToken,
                        .qt = @bitCast(nodeData[0]),
                        .lhs = @enumFromInt(nodeData[1]),
                        .rhs = @enumFromInt(nodeData[2]),
                    },
                },
                .LessThanEqualExpr => .{
                    .lessThanEqualExpr = .{
                        .opToken = nodeToken,
                        .qt = @bitCast(nodeData[0]),
                        .lhs = @enumFromInt(nodeData[1]),
                        .rhs = @enumFromInt(nodeData[2]),
                    },
                },
                .GreaterThanExpr => .{
                    .greaterThanExpr = .{
                        .opToken = nodeToken,
                        .qt = @bitCast(nodeData[0]),
                        .lhs = @enumFromInt(nodeData[1]),
                        .rhs = @enumFromInt(nodeData[2]),
                    },
                },
                .GreaterThanEqualExpr => .{
                    .greaterThanEqualExpr = .{
                        .opToken = nodeToken,
                        .qt = @bitCast(nodeData[0]),
                        .lhs = @enumFromInt(nodeData[1]),
                        .rhs = @enumFromInt(nodeData[2]),
                    },
                },
                .ShlExpr => .{
                    .shlExpr = .{
                        .opToken = nodeToken,
                        .qt = @bitCast(nodeData[0]),
                        .lhs = @enumFromInt(nodeData[1]),
                        .rhs = @enumFromInt(nodeData[2]),
                    },
                },
                .ShrExpr => .{
                    .shrExpr = .{
                        .opToken = nodeToken,
                        .qt = @bitCast(nodeData[0]),
                        .lhs = @enumFromInt(nodeData[1]),
                        .rhs = @enumFromInt(nodeData[2]),
                    },
                },
                .AddExpr => .{
                    .addExpr = .{
                        .opToken = nodeToken,
                        .qt = @bitCast(nodeData[0]),
                        .lhs = @enumFromInt(nodeData[1]),
                        .rhs = @enumFromInt(nodeData[2]),
                    },
                },
                .SubExpr => .{
                    .subExpr = .{
                        .opToken = nodeToken,
                        .qt = @bitCast(nodeData[0]),
                        .lhs = @enumFromInt(nodeData[1]),
                        .rhs = @enumFromInt(nodeData[2]),
                    },
                },
                .MulExpr => .{
                    .mulExpr = .{
                        .opToken = nodeToken,
                        .qt = @bitCast(nodeData[0]),
                        .lhs = @enumFromInt(nodeData[1]),
                        .rhs = @enumFromInt(nodeData[2]),
                    },
                },
                .DivExpr => .{
                    .divExpr = .{
                        .opToken = nodeToken,
                        .qt = @bitCast(nodeData[0]),
                        .lhs = @enumFromInt(nodeData[1]),
                        .rhs = @enumFromInt(nodeData[2]),
                    },
                },
                .ModExpr => .{
                    .modExpr = .{
                        .opToken = nodeToken,
                        .qt = @bitCast(nodeData[0]),
                        .lhs = @enumFromInt(nodeData[1]),
                        .rhs = @enumFromInt(nodeData[2]),
                    },
                },
                .ExplicitCast => .{
                    .cast = .{
                        .lparen = nodeToken,
                        .qt = @bitCast(nodeData[0]),
                        .kind = @enumFromInt(nodeData[1]),
                        .operand = @enumFromInt(nodeData[2]),
                        .implicit = false,
                    },
                },
                .ImplicitCast => .{
                    .cast = .{
                        .lparen = nodeToken,
                        .qt = @bitCast(nodeData[0]),
                        .kind = @enumFromInt(nodeData[1]),
                        .operand = @enumFromInt(nodeData[2]),
                        .implicit = true,
                    },
                },
                .AddrOfExpr => .{
                    .addrOfExpr = .{
                        .opToken = nodeToken,
                        .qt = @bitCast(nodeData[0]),
                        .operand = @enumFromInt(nodeData[1]),
                    },
                },
                .DerefExpr => .{
                    .derefExpr = .{
                        .opToken = nodeToken,
                        .qt = @bitCast(nodeData[0]),
                        .operand = @enumFromInt(nodeData[1]),
                    },
                },
                .PlusExpr => .{
                    .plusExpr = .{
                        .opToken = nodeToken,
                        .qt = @bitCast(nodeData[0]),
                        .operand = @enumFromInt(nodeData[1]),
                    },
                },
                .NegateExpr => .{
                    .negateExpr = .{
                        .opToken = nodeToken,
                        .qt = @bitCast(nodeData[0]),
                        .operand = @enumFromInt(nodeData[1]),
                    },
                },
                .BitNotExpr => .{
                    .bitNotExpr = .{
                        .opToken = nodeToken,
                        .qt = @bitCast(nodeData[0]),
                        .operand = @enumFromInt(nodeData[1]),
                    },
                },
                .BoolNotExpr => .{
                    .boolNotExpr = .{
                        .opToken = nodeToken,
                        .qt = @bitCast(nodeData[0]),
                        .operand = @enumFromInt(nodeData[1]),
                    },
                },
                .PreIncExpr => .{
                    .preIncExpr = .{
                        .opToken = nodeToken,
                        .qt = @bitCast(nodeData[0]),
                        .operand = @enumFromInt(nodeData[1]),
                    },
                },
                .PreDecExpr => .{
                    .preDecExpr = .{
                        .opToken = nodeToken,
                        .qt = @bitCast(nodeData[0]),
                        .operand = @enumFromInt(nodeData[1]),
                    },
                },
                .ImagExpr => .{
                    .imagExpr = .{
                        .opToken = nodeToken,
                        .qt = @bitCast(nodeData[0]),
                        .operand = @enumFromInt(nodeData[1]),
                    },
                },
                .RealExpr => .{
                    .realExpr = .{
                        .opToken = nodeToken,
                        .qt = @bitCast(nodeData[0]),
                        .operand = @enumFromInt(nodeData[1]),
                    },
                },
                .PostIncExpr => .{
                    .postIncExpr = .{
                        .opToken = nodeToken,
                        .qt = @bitCast(nodeData[0]),
                        .operand = @enumFromInt(nodeData[1]),
                    },
                },
                .PostDecExpr => .{
                    .postDecExpr = .{
                        .opToken = nodeToken,
                        .qt = @bitCast(nodeData[0]),
                        .operand = @enumFromInt(nodeData[1]),
                    },
                },
                .ParenExpr => .{
                    .parenExpr = .{
                        .opToken = nodeToken,
                        .qt = @bitCast(nodeData[0]),
                        .operand = @enumFromInt(nodeData[1]),
                    },
                },
                .StmtExpr => .{
                    .stmtExpr = .{
                        .opToken = nodeToken,
                        .qt = @bitCast(nodeData[0]),
                        .operand = @enumFromInt(nodeData[1]),
                    },
                },
                .CondDummyExpr => .{
                    .condDummyExpr = .{
                        .opToken = nodeToken,
                        .qt = @bitCast(nodeData[0]),
                        .operand = @enumFromInt(nodeData[1]),
                    },
                },
                .AddrOfLabel => .{
                    .addrOfLabel = .{
                        .labelToken = nodeToken,
                        .qt = @bitCast(nodeData[0]),
                    },
                },
                .ArrayAccessExpr => .{
                    .arrayAccessExpr = .{
                        .lbracketToken = nodeToken,
                        .qt = @bitCast(nodeData[0]),
                        .base = @enumFromInt(nodeData[1]),
                        .index = @enumFromInt(nodeData[2]),
                    },
                },
                .CallExpr => .{
                    .callExpr = .{
                        .lparenToken = nodeToken,
                        .qt = @bitCast(nodeData[0]),
                        .callee = @enumFromInt(tree.extra.items[nodeData[1]]),
                        .args = @ptrCast(tree.extra.items[nodeData[1] + 1 ..][0 .. nodeData[2] - 1]),
                    },
                },
                .CallExprOne => .{
                    .callExpr = .{
                        .lparenToken = nodeToken,
                        .qt = @bitCast(nodeData[0]),
                        .callee = @enumFromInt(nodeData[1]),
                        .args = unPackElems(nodeData[2..]),
                    },
                },
                .BuiltinCallExpr => .{
                    .builtinCallExpr = .{
                        .builtinToken = nodeToken,
                        .qt = @bitCast(nodeData[0]),
                        .args = @ptrCast(tree.extra.items[nodeData[1]..][0..nodeData[2]]),
                    },
                },
                .BuiltinCallExprTwo => .{
                    .builtinCallExpr = .{
                        .builtinToken = nodeToken,
                        .qt = @bitCast(nodeData[0]),
                        .args = unPackElems(nodeData[1..]),
                    },
                },
                .MemberAccessExpr => .{
                    .memberAccessExpr = .{
                        .accessToken = nodeToken,
                        .qt = @bitCast(nodeData[0]),
                        .base = @enumFromInt(nodeData[1]),
                        .memberIndex = nodeData[2],
                    },
                },
                .MemberAccessPtrExpr => .{
                    .memberAccessPtrExpr = .{
                        .accessToken = nodeToken,
                        .qt = @bitCast(nodeData[0]),
                        .base = @enumFromInt(nodeData[1]),
                        .memberIndex = nodeData[2],
                    },
                },
                .DeclRefExpr => .{
                    .declRefExpr = .{
                        .nameToken = nodeToken,
                        .qt = @bitCast(nodeData[0]),
                        .decl = @enumFromInt(nodeData[1]),
                    },
                },
                .EnumerationRef => .{
                    .enumerationRef = .{
                        .nameToken = nodeToken,
                        .qt = @bitCast(nodeData[0]),
                        .decl = @enumFromInt(nodeData[1]),
                    },
                },
                .BuiltinRef => .{
                    .builtinRef = .{
                        .nameToken = nodeToken,
                        .qt = @bitCast(nodeData[0]),
                    },
                },
                .BoolLiteral => .{
                    .boolLiteral = .{
                        .literalToken = nodeToken,
                        .qt = @bitCast(nodeData[0]),
                    },
                },
                .NullptrLiteral => .{
                    .nullptrLiteral = .{
                        .literalToken = nodeToken,
                        .qt = @bitCast(nodeData[0]),
                    },
                },
                .IntLiteral => .{
                    .intLiteral = .{
                        .literalToken = nodeToken,
                        .qt = @bitCast(nodeData[0]),
                    },
                },
                .CharLiteral => .{
                    .charLiteral = .{
                        .literalToken = nodeToken,
                        .qt = @bitCast(nodeData[0]),
                        .kind = @enumFromInt(nodeData[1]),
                    },
                },
                .FloatLiteral => .{
                    .floatLiteral = .{
                        .literalToken = nodeToken,
                        .qt = @bitCast(nodeData[0]),
                    },
                },
                .StringLiteralExpr => .{
                    .stringLiteralExpr = .{
                        .literalToken = nodeToken,
                        .qt = @bitCast(nodeData[0]),
                        .kind = @enumFromInt(nodeData[1]),
                    },
                },
                .ImaginaryLiteral => .{
                    .imaginaryLiteral = .{
                        .opToken = nodeToken,
                        .qt = @bitCast(nodeData[0]),
                        .operand = @enumFromInt(nodeData[1]),
                    },
                },
                .SizeofExpr => .{
                    .sizeofExpr = .{
                        .opToken = nodeToken,
                        .qt = @bitCast(nodeData[0]),
                        .expr = unpackOptIndex(nodeData[1]),
                        .operandQt = @bitCast(nodeData[2]),
                    },
                },
                .AlignofExpr => .{
                    .alignofExpr = .{
                        .opToken = nodeToken,
                        .qt = @bitCast(nodeData[0]),
                        .expr = unpackOptIndex(nodeData[1]),
                        .operandQt = @bitCast(nodeData[2]),
                    },
                },

                .GenericExprZero => .{
                    .genericExpr = .{
                        .genericToken = nodeToken,
                        .qt = @bitCast(nodeData[0]),
                        .controlling = @enumFromInt(nodeData[1]),
                        .chosen = @enumFromInt(nodeData[2]),
                        .rest = &.{},
                    },
                },
                .GenericExpr => .{
                    .genericExpr = .{
                        .genericToken = nodeToken,
                        .qt = @bitCast(nodeData[0]),
                        .controlling = @enumFromInt(tree.extra.items[nodeData[1]]),
                        .chosen = @enumFromInt(tree.extra.items[nodeData[1] + 1]),
                        .rest = @ptrCast(tree.extra.items[nodeData[1] + 2 ..][0 .. nodeData[2] - 2]),
                    },
                },
                .GenericAssociationExpr => .{
                    .genericAssociationExpr = .{
                        .colonToken = nodeToken,
                        .associationQt = @bitCast(nodeData[0]),
                        .expr = @enumFromInt(nodeData[1]),
                    },
                },
                .GenericDefaultExpr => .{
                    .genericDefaultExpr = .{
                        .defaultToken = nodeToken,
                        .expr = @enumFromInt(nodeData[0]),
                    },
                },
                .BinaryCondExpr => .{
                    .binaryCondExpr = .{
                        .condToken = nodeToken,
                        .qt = @bitCast(nodeData[0]),
                        .cond = @enumFromInt(nodeData[1]),
                        .thenExpr = @enumFromInt(tree.extra.items[nodeData[2]]),
                        .elseExpr = @enumFromInt(tree.extra.items[nodeData[2] + 1]),
                    },
                },
                .CondExpr => .{
                    .condExpr = .{
                        .condToken = nodeToken,
                        .qt = @bitCast(nodeData[0]),
                        .cond = @enumFromInt(nodeData[1]),
                        .thenExpr = @enumFromInt(tree.extra.items[nodeData[2]]),
                        .elseExpr = @enumFromInt(tree.extra.items[nodeData[2] + 1]),
                    },
                },
                .BuiltinChooseExpr => .{
                    .builtinChooseExpr = .{
                        .condToken = nodeToken,
                        .qt = @bitCast(nodeData[0]),
                        .cond = @enumFromInt(nodeData[1]),
                        .thenExpr = @enumFromInt(tree.extra.items[nodeData[2]]),
                        .elseExpr = @enumFromInt(tree.extra.items[nodeData[2] + 1]),
                    },
                },
                .BuiltinTypesCompatibleP => .{
                    .builtinTypesCompatibleP = .{
                        .builtinToken = nodeToken,
                        .lhs = @bitCast(nodeData[0]),
                        .rhs = @bitCast(nodeData[1]),
                    },
                },
                .ArrayInitExprTwo => .{
                    .arrayInitExpr = .{
                        .lbraceToken = nodeToken,
                        .containerQt = @bitCast(nodeData[0]),
                        .items = unPackElems(nodeData[1..]),
                    },
                },
                .ArrayInitExpr => .{
                    .arrayInitExpr = .{
                        .lbraceToken = nodeToken,
                        .containerQt = @bitCast(nodeData[0]),
                        .items = @ptrCast(tree.extra.items[nodeData[1]..][0..nodeData[2]]),
                    },
                },
                .StructInitExprTwo => .{
                    .structInitExpr = .{
                        .lbraceToken = nodeToken,
                        .containerQt = @bitCast(nodeData[0]),
                        .items = unPackElems(nodeData[1..]),
                    },
                },
                .StructInitExpr => .{
                    .structInitExpr = .{
                        .lbraceToken = nodeToken,
                        .containerQt = @bitCast(nodeData[0]),
                        .items = @ptrCast(tree.extra.items[nodeData[1]..][0..nodeData[2]]),
                    },
                },
                .UnionInitExpr => .{
                    .unionInitExpr = .{
                        .lbraceToken = nodeToken,
                        .unionQt = @bitCast(nodeData[0]),
                        .fieldIndex = nodeData[1],
                        .initializer = unpackOptIndex(nodeData[2]),
                    },
                },
                .ArrayFillerExpr => .{
                    .arrayFillerExpr = .{
                        .lastToken = nodeToken,
                        .qt = @bitCast(nodeData[0]),
                        .count = @bitCast(nodeData[1..].*),
                    },
                },
                .DefaultInitExpr => .{
                    .defaultInitExpr = .{
                        .lastToken = nodeToken,
                        .qt = @bitCast(nodeData[0]),
                    },
                },
                .CompoundLiteralExpr => {
                    const attr: Node.Repr.DeclAttr = @bitCast(nodeData[1]);
                    return .{
                        .compoundLiteralExpr = .{
                            .lparenToken = nodeToken,
                            .qt = @bitCast(nodeData[0]),
                            .storageClass = if (attr.static)
                                .static
                            else if (attr.register)
                                .register
                            else
                                .auto,
                            .threadLocal = attr.threadLocal,
                            .initializer = @enumFromInt(nodeData[2]),
                        },
                    };
                },
            };
        }

        pub fn tok(index: Index, tree: *const Tree) TokenIndex {
            return tree.nodes.items(.tok)[@intFromEnum(index)];
        }

        pub fn loc(index: Index, tree: *const Tree) ?Source.Location {
            const tokenIndex = index.tok(tree);
            return tree.tokens.items(.loc)[@intFromEnum(tokenIndex)];
        }

        pub fn qt(index: Index, tree: *const Tree) QualType {
            return index.qtOrNull(tree) orelse .void;
        }

        pub fn qtOrNull(index: Index, tree: *const Tree) ?QualType {
            const tag = tree.nodes.items(.tag)[@intFromEnum(index)];
            return switch (tag) {
                .EmptyDecl,
                .StaticAssert,
                .CompoundStmt,
                .CompoundStmtThree,
                .IfStmt,
                .SwitchStmt,
                .CaseStmt,
                .DefaultStmt,
                .WhileStmt,
                .DoWhileStmt,
                .ForDecl,
                .ForExpr,
                .GotoStmt,
                .ComputedGotoStmt,
                .ContinueStmt,
                .BreakStmt,
                .GnuAsmSimple,
                .GlobalAsm,
                .GenericAssociationExpr,
                .GenericDefaultExpr,
                => null,

                .BuiltinTypesCompatibleP => .int,

                else => {
                    // If a node is typed the type is stored in data[0].
                    return @bitCast(tree.nodes.items(.data)[@intFromEnum(index)][0]);
                },
            };
        }
    };

    pub const OptIndex = enum(u32) {
        null = std.math.maxInt(u32),
        _,

        pub fn unpack(opt: OptIndex) ?Index {
            return if (opt == .null) null else @enumFromInt(@intFromEnum(opt));
        }

        pub fn pack(index: Index) OptIndex {
            return @enumFromInt(@intFromEnum(index));
        }

        pub fn packOpt(optional: ?Index) OptIndex {
            return if (optional) |some| @enumFromInt(@intFromEnum(some)) else .null;
        }
    };

    pub const Repr = struct {
        tag: Tag,
        /// If a node is typed the type is stored in data[0]
        data: [3]u32,
        tok: TokenIndex,

        pub const DeclAttr = packed struct(u32) {
            @"extern": bool = false,
            static: bool = false,
            register: bool = false,
            @"inline": bool = false,
            threadLocal: bool = false,
            implicit: bool = false,
            _: u26 = 0,
        };

        pub const Tag = enum(u8) {
            EmptyDecl,
            StaticAssert,
            FnProto,
            FnDef,
            Param,
            Variable,
            Typedef,
            GlobalAsm,
            StructDecl,
            UnionDecl,
            EnumDecl,
            StructDeclTwo,
            UnionDeclTwo,
            EnumDeclTwo,
            StructForwardDecl,
            UnionForwardDecl,
            EnumForwardDecl,
            EnumField,
            RecordField,
            LabeledStmt,
            CompoundStmt,
            CompoundStmtThree,
            IfStmt,
            SwitchStmt,
            CaseStmt,
            DefaultStmt,
            WhileStmt,
            DoWhileStmt,
            ForExpr,
            ForDecl,
            GotoStmt,
            ComputedGotoStmt,
            ContinueStmt,
            BreakStmt,
            NullStmt,
            ReturnStmt,
            ReturnNoneStmt,
            ImplicitReturn,
            GnuAsmSimple,
            CommaExpr,
            AssignExpr,
            MulAssignExpr,
            DivAssignExpr,
            ModAssignExpr,
            AddAssignExpr,
            SubAssignExpr,
            ShlAssignExpr,
            ShrAssignExpr,
            BitAndAssignExpr,
            BitXorAssignExpr,
            BitOrAssignExpr,
            CompoundAssignDummyExpr,
            BoolOrExpr,
            BoolAndExpr,
            BitOrExpr,
            BitXorExpr,
            BitAndExpr,
            EqualExpr,
            NotEqualExpr,
            LessThanExpr,
            LessThanEqualExpr,
            GreaterThanExpr,
            GreaterThanEqualExpr,
            ShlExpr,
            ShrExpr,
            AddExpr,
            SubExpr,
            MulExpr,
            DivExpr,
            ModExpr,
            ExplicitCast,
            ImplicitCast,
            AddrOfExpr,
            DerefExpr,
            PlusExpr,
            NegateExpr,
            BitNotExpr,
            BoolNotExpr,
            PreIncExpr,
            PreDecExpr,
            ImagExpr,
            RealExpr,
            PostIncExpr,
            PostDecExpr,
            ParenExpr,
            StmtExpr,
            AddrOfLabel,
            ArrayAccessExpr,
            CallExprOne,
            CallExpr,
            BuiltinCallExpr,
            BuiltinCallExprTwo,
            MemberAccessExpr,
            MemberAccessPtrExpr,
            DeclRefExpr,
            EnumerationRef,
            BuiltinRef,
            BoolLiteral,
            NullptrLiteral,
            IntLiteral,
            CharLiteral,
            FloatLiteral,
            StringLiteralExpr,
            ImaginaryLiteral,
            SizeofExpr,
            AlignofExpr,
            GenericExpr,
            GenericExprZero,
            GenericAssociationExpr,
            GenericDefaultExpr,
            BinaryCondExpr,
            CondDummyExpr,
            CondExpr,
            BuiltinChooseExpr,
            BuiltinTypesCompatibleP,
            ArrayInitExpr,
            ArrayInitExprTwo,
            StructInitExpr,
            StructInitExprTwo,
            UnionInitExpr,
            ArrayFillerExpr,
            DefaultInitExpr,
            CompoundLiteralExpr,
        };
    };

    pub fn isImplicit(node: Node) bool {
        return switch (node) {
            .arrayFillerExpr, .defaultInitExpr, .condDummyExpr, .compoundAssignDummyExpr => true,
            .cast => |cast| cast.implicit,
            .variable => |info| info.implicit,
            .typedef => |info| info.implicit,
            .returnStmt => |ret| ret.operand == .implicit,
            else => false,
        };
    }
};

pub fn addNode(tree: *Tree, node: Node) !Node.Index {
    const index = try tree.nodes.addOne(tree.comp.gpa);
    try tree.setNode(index, node);
    return @enumFromInt(index);
}

pub fn setNode(tree: *Tree, index: usize, node: Node) !void {
    var repr: Node.Repr = undefined;
    switch (node) {
        .emptyDecl => |empty| {
            repr.tag = .EmptyDecl;
            repr.tok = empty.semicolon;
        },
        .staticAssert => |assert| {
            repr.tag = .StaticAssert;
            repr.data[0] = @intFromEnum(assert.cond);
            repr.data[1] = packOptIndex(assert.message);
            repr.tok = assert.assertToken;
        },
        .fnProto => |proto| {
            repr.tag = .FnProto;
            repr.data[0] = @bitCast(proto.qt);
            repr.data[1] = @bitCast(Node.Repr.DeclAttr{
                .static = proto.static,
                .@"inline" = proto.@"inline",
            });
            // TODO decide how to handle definition
            // repr.data[2] = proto.definition;
            repr.tok = proto.nameToken;
        },
        .fnDef => |def| {
            repr.tag = .FnDef;
            repr.data[0] = @bitCast(def.qt);
            repr.data[1] = @bitCast(Node.Repr.DeclAttr{
                .static = def.static,
                .@"inline" = def.@"inline",
            });
            repr.data[2] = @intFromEnum(def.body);
            repr.tok = def.nameToken;
        },
        .param => |param| {
            repr.tag = .Param;
            repr.data[0] = @bitCast(param.qt);
            repr.data[1] = @bitCast(Node.Repr.DeclAttr{
                .register = param.storageClass == .register,
            });
            repr.tok = param.nameToken;
        },
        .variable => |variable| {
            repr.tag = .Variable;
            repr.data[0] = @bitCast(variable.qt);
            repr.data[1] = @bitCast(Node.Repr.DeclAttr{
                .@"extern" = variable.storageClass == .@"extern",
                .static = variable.storageClass == .static,
                .register = variable.storageClass == .register,
                .threadLocal = variable.threadLocal,
                .implicit = variable.implicit,
            });
            repr.data[2] = packOptIndex(variable.initializer);
            repr.tok = variable.nameToken;
        },
        .typedef => |typedef| {
            repr.tag = .Typedef;
            repr.data[0] = @bitCast(typedef.qt);
            repr.data[1] = @intFromBool(typedef.implicit);
            repr.tok = typedef.nameToken;
        },
        .globalAsm => |globalAsm| {
            repr.tag = .GlobalAsm;
            repr.data[0] = @intFromEnum(globalAsm.asmString);
            repr.tok = globalAsm.asmToken;
        },
        .structDecl => |decl| {
            repr.data[0] = @bitCast(decl.containerQt);
            if (decl.fields.len > 2) {
                repr.tag = .StructDecl;
                repr.data[1], repr.data[2] = try tree.addExtra(decl.fields);
            } else {
                repr.tag = .StructDeclTwo;
                repr.data[1] = packElem(decl.fields, 0);
                repr.data[2] = packElem(decl.fields, 1);
            }
            repr.tok = decl.nameOrKindToken;
        },
        .unionDecl => |decl| {
            repr.data[0] = @bitCast(decl.containerQt);
            if (decl.fields.len > 2) {
                repr.tag = .UnionDecl;
                repr.data[1], repr.data[2] = try tree.addExtra(decl.fields);
            } else {
                repr.tag = .UnionDeclTwo;
                repr.data[1] = packElem(decl.fields, 0);
                repr.data[2] = packElem(decl.fields, 1);
            }
            repr.tok = decl.nameOrKindToken;
        },
        .enumDecl => |decl| {
            repr.data[0] = @bitCast(decl.containerQt);
            if (decl.fields.len > 2) {
                repr.tag = .EnumDecl;
                repr.data[1], repr.data[2] = try tree.addExtra(decl.fields);
            } else {
                repr.tag = .EnumDeclTwo;
                repr.data[1] = packElem(decl.fields, 0);
                repr.data[2] = packElem(decl.fields, 1);
            }
            repr.tok = decl.nameOrKindToken;
        },
        .structForwardDecl => |decl| {
            repr.tag = .StructForwardDecl;
            repr.data[0] = @bitCast(decl.containerQt);
            // TODO decide how to handle definition
            // repr.data[1] = decl.definition;
            repr.tok = decl.nameOrKindToken;
        },
        .unionForwardDecl => |decl| {
            repr.tag = .UnionForwardDecl;
            repr.data[0] = @bitCast(decl.containerQt);
            // TODO decide how to handle definition
            // repr.data[1] = decl.definition;
            repr.tok = decl.nameOrKindToken;
        },
        .enumForwardDecl => |decl| {
            repr.tag = .EnumForwardDecl;
            repr.data[0] = @bitCast(decl.containerQt);
            // TODO decide how to handle definition
            // repr.data[1] = decl.definition;
            repr.tok = decl.nameOrKindToken;
        },
        .enumField => |field| {
            repr.tag = .EnumField;
            repr.data[0] = @bitCast(field.qt);
            repr.data[1] = packOptIndex(field.init);
            repr.tok = field.nameToken;
        },
        .recordField => |field| {
            repr.tag = .RecordField;
            repr.data[0] = @bitCast(field.qt);
            repr.data[1] = packOptIndex(field.bitWidth);
            repr.tok = field.nameOrFirstToken;
        },
        .labeledStmt => |labeled| {
            repr.tag = .LabeledStmt;
            repr.data[0] = @bitCast(labeled.qt);
            repr.data[1] = @intFromEnum(labeled.body);
            repr.tok = labeled.labelToken;
        },
        .compoundStmt => |compound| {
            if (compound.body.len > 3) {
                repr.tag = .CompoundStmt;
                repr.data[0], repr.data[1] = try tree.addExtra(compound.body);
            } else {
                repr.tag = .CompoundStmtThree;
                for (&repr.data, 0..) |*data, idx|
                    data.* = packElem(compound.body, idx);
            }
            repr.tok = compound.lbraceToken;
        },
        .ifStmt => |@"if"| {
            repr.tag = .IfStmt;
            repr.data[0] = @intFromEnum(@"if".cond);
            repr.data[1] = @intFromEnum(@"if".thenBody);
            repr.data[2] = packOptIndex(@"if".elseBody);
            repr.tok = @"if".ifToken;
        },
        .switchStmt => |@"switch"| {
            repr.tag = .SwitchStmt;
            repr.data[0] = @intFromEnum(@"switch".cond);
            repr.data[1] = @intFromEnum(@"switch".body);
            repr.tok = @"switch".switchToken;
        },
        .caseStmt => |case| {
            repr.tag = .CaseStmt;
            repr.data[0] = @intFromEnum(case.start);
            repr.data[1] = packOptIndex(case.end);
            repr.data[2] = packOptIndex(case.body);
            repr.tok = case.caseToken;
        },
        .defaultStmt => |default| {
            repr.tag = .DefaultStmt;
            repr.data[0] = @intFromEnum(default.body);
            repr.tok = default.defaultToken;
        },
        .whileStmt => |@"while"| {
            repr.tag = .WhileStmt;
            repr.data[0] = @intFromEnum(@"while".cond);
            repr.data[1] = @intFromEnum(@"while".body);
            repr.tok = @"while".whileToken;
        },
        .doWhileStmt => |doWhile| {
            repr.tag = .DoWhileStmt;
            repr.data[0] = @intFromEnum(doWhile.cond);
            repr.data[1] = @intFromEnum(doWhile.body);
            repr.tok = doWhile.doToken;
        },
        .forStmt => |@"for"| {
            switch (@"for".init) {
                .decls => |decls| {
                    repr.tag = .ForDecl;
                    repr.data[0] = @intCast(tree.extra.items.len);
                    const len: u32 = @intCast(decls.len + 2);
                    try tree.extra.ensureUnusedCapacity(tree.comp.gpa, len);
                    repr.data[1] = len;
                    tree.extra.appendSliceAssumeCapacity(@ptrCast(decls));
                    tree.extra.appendAssumeCapacity(packOptIndex(@"for".cond));
                    tree.extra.appendAssumeCapacity(packOptIndex(@"for".incr));
                },
                .expr => |expr| {
                    repr.tag = .ForExpr;
                    repr.data[0] = packOptIndex(expr);
                    repr.data[1] = @intCast(tree.extra.items.len);
                    try tree.extra.ensureUnusedCapacity(tree.comp.gpa, 2);
                    tree.extra.appendAssumeCapacity(packOptIndex(@"for".cond));
                    tree.extra.appendAssumeCapacity(packOptIndex(@"for".incr));
                },
            }
            repr.data[2] = @intFromEnum(@"for".body);
            repr.tok = @"for".forToken;
        },
        .gotoStmt => |goto| {
            repr.tag = .GotoStmt;
            repr.tok = goto.labelToken;
        },
        .computedGotoStmt => |computedGoto| {
            repr.tag = .ComputedGotoStmt;
            repr.data[0] = @intFromEnum(computedGoto.expr);
            repr.tok = computedGoto.gotoToken;
        },
        .continueStmt => |@"continue"| {
            repr.tag = .ContinueStmt;
            repr.tok = @"continue".continueToken;
        },
        .breakStmt => |@"break"| {
            repr.tag = .BreakStmt;
            repr.tok = @"break".breakToken;
        },
        .nullStmt => |@"null"| {
            repr.tag = .NullStmt;
            repr.data[0] = @bitCast(@"null".qt);
            repr.tok = @"null".semicolonOrRbraceToken;
        },
        .returnStmt => |@"return"| {
            repr.data[0] = @bitCast(@"return".returnQt);
            switch (@"return".operand) {
                .expr => |expr| {
                    repr.tag = .ReturnStmt;
                    repr.data[1] = @intFromEnum(expr);
                },
                .none => repr.tag = .ReturnNoneStmt,
                .implicit => |zeroes| {
                    repr.tag = .ImplicitReturn;
                    repr.data[1] = @intFromBool(zeroes);
                },
            }
            repr.tok = @"return".returnToken;
        },
        .gnuAsmSimple => |gnuAsmSimple| {
            repr.tag = .GnuAsmSimple;
            repr.data[0] = @intFromEnum(gnuAsmSimple.asmString);
            repr.tok = gnuAsmSimple.asmToken;
        },
        .assignExpr => |bin| {
            repr.tag = .AssignExpr;
            repr.data[0] = @bitCast(bin.qt);
            repr.data[1] = @intFromEnum(bin.lhs);
            repr.data[2] = @intFromEnum(bin.rhs);
            repr.tok = bin.opToken;
        },
        .mulAssignExpr => |bin| {
            repr.tag = .MulAssignExpr;
            repr.data[0] = @bitCast(bin.qt);
            repr.data[1] = @intFromEnum(bin.lhs);
            repr.data[2] = @intFromEnum(bin.rhs);
            repr.tok = bin.opToken;
        },
        .divAssignExpr => |bin| {
            repr.tag = .DivAssignExpr;
            repr.data[0] = @bitCast(bin.qt);
            repr.data[1] = @intFromEnum(bin.lhs);
            repr.data[2] = @intFromEnum(bin.rhs);
            repr.tok = bin.opToken;
        },
        .modAssignExpr => |bin| {
            repr.tag = .ModAssignExpr;
            repr.data[0] = @bitCast(bin.qt);
            repr.data[1] = @intFromEnum(bin.lhs);
            repr.data[2] = @intFromEnum(bin.rhs);
            repr.tok = bin.opToken;
        },
        .addAssignExpr => |bin| {
            repr.tag = .AddAssignExpr;
            repr.data[0] = @bitCast(bin.qt);
            repr.data[1] = @intFromEnum(bin.lhs);
            repr.data[2] = @intFromEnum(bin.rhs);
            repr.tok = bin.opToken;
        },
        .subAssignExpr => |bin| {
            repr.tag = .SubAssignExpr;
            repr.data[0] = @bitCast(bin.qt);
            repr.data[1] = @intFromEnum(bin.lhs);
            repr.data[2] = @intFromEnum(bin.rhs);
            repr.tok = bin.opToken;
        },
        .shlAssignExpr => |bin| {
            repr.tag = .ShlAssignExpr;
            repr.data[0] = @bitCast(bin.qt);
            repr.data[1] = @intFromEnum(bin.lhs);
            repr.data[2] = @intFromEnum(bin.rhs);
            repr.tok = bin.opToken;
        },
        .shrAssignExpr => |bin| {
            repr.tag = .ShrAssignExpr;
            repr.data[0] = @bitCast(bin.qt);
            repr.data[1] = @intFromEnum(bin.lhs);
            repr.data[2] = @intFromEnum(bin.rhs);
            repr.tok = bin.opToken;
        },
        .bitAndAssignExpr => |bin| {
            repr.tag = .BitAndAssignExpr;
            repr.data[0] = @bitCast(bin.qt);
            repr.data[1] = @intFromEnum(bin.lhs);
            repr.data[2] = @intFromEnum(bin.rhs);
            repr.tok = bin.opToken;
        },
        .bitXorAssignExpr => |bin| {
            repr.tag = .BitXorAssignExpr;
            repr.data[0] = @bitCast(bin.qt);
            repr.data[1] = @intFromEnum(bin.lhs);
            repr.data[2] = @intFromEnum(bin.rhs);
            repr.tok = bin.opToken;
        },
        .bitOrAssignExpr => |bin| {
            repr.tag = .BitOrAssignExpr;
            repr.data[0] = @bitCast(bin.qt);
            repr.data[1] = @intFromEnum(bin.lhs);
            repr.data[2] = @intFromEnum(bin.rhs);
            repr.tok = bin.opToken;
        },
        .commaExpr => |bin| {
            repr.tag = .CommaExpr;
            repr.data[0] = @bitCast(bin.qt);
            repr.data[1] = @intFromEnum(bin.lhs);
            repr.data[2] = @intFromEnum(bin.rhs);
            repr.tok = bin.opToken;
        },
        .compoundAssignDummyExpr => |un| {
            repr.tag = .CompoundAssignDummyExpr;
            repr.data[0] = @bitCast(un.qt);
            repr.data[1] = @intFromEnum(un.operand);
            repr.tok = un.opToken;
        },
        .boolOrExpr => |bin| {
            repr.tag = .BoolOrExpr;
            repr.data[0] = @bitCast(bin.qt);
            repr.data[1] = @intFromEnum(bin.lhs);
            repr.data[2] = @intFromEnum(bin.rhs);
            repr.tok = bin.opToken;
        },
        .boolAndExpr => |bin| {
            repr.tag = .BoolAndExpr;
            repr.data[0] = @bitCast(bin.qt);
            repr.data[1] = @intFromEnum(bin.lhs);
            repr.data[2] = @intFromEnum(bin.rhs);
            repr.tok = bin.opToken;
        },
        .bitOrExpr => |bin| {
            repr.tag = .BitOrExpr;
            repr.data[0] = @bitCast(bin.qt);
            repr.data[1] = @intFromEnum(bin.lhs);
            repr.data[2] = @intFromEnum(bin.rhs);
            repr.tok = bin.opToken;
        },
        .bitXorExpr => |bin| {
            repr.tag = .BitXorExpr;
            repr.data[0] = @bitCast(bin.qt);
            repr.data[1] = @intFromEnum(bin.lhs);
            repr.data[2] = @intFromEnum(bin.rhs);
            repr.tok = bin.opToken;
        },
        .bitAndExpr => |bin| {
            repr.tag = .BitAndExpr;
            repr.data[0] = @bitCast(bin.qt);
            repr.data[1] = @intFromEnum(bin.lhs);
            repr.data[2] = @intFromEnum(bin.rhs);
            repr.tok = bin.opToken;
        },
        .equalExpr => |bin| {
            repr.tag = .EqualExpr;
            repr.data[0] = @bitCast(bin.qt);
            repr.data[1] = @intFromEnum(bin.lhs);
            repr.data[2] = @intFromEnum(bin.rhs);
            repr.tok = bin.opToken;
        },
        .notEqualExpr => |bin| {
            repr.tag = .NotEqualExpr;
            repr.data[0] = @bitCast(bin.qt);
            repr.data[1] = @intFromEnum(bin.lhs);
            repr.data[2] = @intFromEnum(bin.rhs);
            repr.tok = bin.opToken;
        },
        .lessThanExpr => |bin| {
            repr.tag = .LessThanExpr;
            repr.data[0] = @bitCast(bin.qt);
            repr.data[1] = @intFromEnum(bin.lhs);
            repr.data[2] = @intFromEnum(bin.rhs);
            repr.tok = bin.opToken;
        },
        .lessThanEqualExpr => |bin| {
            repr.tag = .LessThanEqualExpr;
            repr.data[0] = @bitCast(bin.qt);
            repr.data[1] = @intFromEnum(bin.lhs);
            repr.data[2] = @intFromEnum(bin.rhs);
            repr.tok = bin.opToken;
        },
        .greaterThanExpr => |bin| {
            repr.tag = .GreaterThanExpr;
            repr.data[0] = @bitCast(bin.qt);
            repr.data[1] = @intFromEnum(bin.lhs);
            repr.data[2] = @intFromEnum(bin.rhs);
            repr.tok = bin.opToken;
        },
        .greaterThanEqualExpr => |bin| {
            repr.tag = .GreaterThanEqualExpr;
            repr.data[0] = @bitCast(bin.qt);
            repr.data[1] = @intFromEnum(bin.lhs);
            repr.data[2] = @intFromEnum(bin.rhs);
            repr.tok = bin.opToken;
        },
        .shlExpr => |bin| {
            repr.tag = .ShlExpr;
            repr.data[0] = @bitCast(bin.qt);
            repr.data[1] = @intFromEnum(bin.lhs);
            repr.data[2] = @intFromEnum(bin.rhs);
            repr.tok = bin.opToken;
        },
        .shrExpr => |bin| {
            repr.tag = .ShrExpr;
            repr.data[0] = @bitCast(bin.qt);
            repr.data[1] = @intFromEnum(bin.lhs);
            repr.data[2] = @intFromEnum(bin.rhs);
            repr.tok = bin.opToken;
        },
        .addExpr => |bin| {
            repr.tag = .AddExpr;
            repr.data[0] = @bitCast(bin.qt);
            repr.data[1] = @intFromEnum(bin.lhs);
            repr.data[2] = @intFromEnum(bin.rhs);
            repr.tok = bin.opToken;
        },
        .subExpr => |bin| {
            repr.tag = .SubExpr;
            repr.data[0] = @bitCast(bin.qt);
            repr.data[1] = @intFromEnum(bin.lhs);
            repr.data[2] = @intFromEnum(bin.rhs);
            repr.tok = bin.opToken;
        },
        .mulExpr => |bin| {
            repr.tag = .MulExpr;
            repr.data[0] = @bitCast(bin.qt);
            repr.data[1] = @intFromEnum(bin.lhs);
            repr.data[2] = @intFromEnum(bin.rhs);
            repr.tok = bin.opToken;
        },
        .divExpr => |bin| {
            repr.tag = .DivExpr;
            repr.data[0] = @bitCast(bin.qt);
            repr.data[1] = @intFromEnum(bin.lhs);
            repr.data[2] = @intFromEnum(bin.rhs);
            repr.tok = bin.opToken;
        },
        .modExpr => |bin| {
            repr.tag = .ModExpr;
            repr.data[0] = @bitCast(bin.qt);
            repr.data[1] = @intFromEnum(bin.lhs);
            repr.data[2] = @intFromEnum(bin.rhs);
            repr.tok = bin.opToken;
        },
        .cast => |cast| {
            repr.tag = if (cast.implicit) .ImplicitCast else .ExplicitCast;
            repr.data[0] = @bitCast(cast.qt);
            repr.data[1] = @intFromEnum(cast.kind);
            repr.data[2] = @intFromEnum(cast.operand);
            repr.tok = cast.lparen;
        },
        .addrOfExpr => |un| {
            repr.tag = .AddrOfExpr;
            repr.data[0] = @bitCast(un.qt);
            repr.data[1] = @intFromEnum(un.operand);
            repr.tok = un.opToken;
        },
        .derefExpr => |un| {
            repr.tag = .DerefExpr;
            repr.data[0] = @bitCast(un.qt);
            repr.data[1] = @intFromEnum(un.operand);
            repr.tok = un.opToken;
        },
        .plusExpr => |un| {
            repr.tag = .PlusExpr;
            repr.data[0] = @bitCast(un.qt);
            repr.data[1] = @intFromEnum(un.operand);
            repr.tok = un.opToken;
        },
        .negateExpr => |un| {
            repr.tag = .NegateExpr;
            repr.data[0] = @bitCast(un.qt);
            repr.data[1] = @intFromEnum(un.operand);
            repr.tok = un.opToken;
        },
        .bitNotExpr => |un| {
            repr.tag = .BitNotExpr;
            repr.data[0] = @bitCast(un.qt);
            repr.data[1] = @intFromEnum(un.operand);
            repr.tok = un.opToken;
        },
        .boolNotExpr => |un| {
            repr.tag = .BoolNotExpr;
            repr.data[0] = @bitCast(un.qt);
            repr.data[1] = @intFromEnum(un.operand);
            repr.tok = un.opToken;
        },
        .preIncExpr => |un| {
            repr.tag = .PreIncExpr;
            repr.data[0] = @bitCast(un.qt);
            repr.data[1] = @intFromEnum(un.operand);
            repr.tok = un.opToken;
        },
        .preDecExpr => |un| {
            repr.tag = .PreDecExpr;
            repr.data[0] = @bitCast(un.qt);
            repr.data[1] = @intFromEnum(un.operand);
            repr.tok = un.opToken;
        },
        .imagExpr => |un| {
            repr.tag = .ImagExpr;
            repr.data[0] = @bitCast(un.qt);
            repr.data[1] = @intFromEnum(un.operand);
            repr.tok = un.opToken;
        },
        .realExpr => |un| {
            repr.tag = .RealExpr;
            repr.data[0] = @bitCast(un.qt);
            repr.data[1] = @intFromEnum(un.operand);
            repr.tok = un.opToken;
        },
        .postIncExpr => |un| {
            repr.tag = .PostIncExpr;
            repr.data[0] = @bitCast(un.qt);
            repr.data[1] = @intFromEnum(un.operand);
            repr.tok = un.opToken;
        },
        .postDecExpr => |un| {
            repr.tag = .PostDecExpr;
            repr.data[0] = @bitCast(un.qt);
            repr.data[1] = @intFromEnum(un.operand);
            repr.tok = un.opToken;
        },
        .parenExpr => |un| {
            repr.tag = .ParenExpr;
            repr.data[0] = @bitCast(un.qt);
            repr.data[1] = @intFromEnum(un.operand);
            repr.tok = un.opToken;
        },
        .stmtExpr => |un| {
            repr.tag = .StmtExpr;
            repr.data[0] = @bitCast(un.qt);
            repr.data[1] = @intFromEnum(un.operand);
            repr.tok = un.opToken;
        },
        .condDummyExpr => |un| {
            repr.tag = .CondDummyExpr;
            repr.data[0] = @bitCast(un.qt);
            repr.data[1] = @intFromEnum(un.operand);
            repr.tok = un.opToken;
        },
        .addrOfLabel => |addrOf| {
            repr.tag = .AddrOfLabel;
            repr.data[0] = @bitCast(addrOf.qt);
            repr.tok = addrOf.labelToken;
        },
        .arrayAccessExpr => |access| {
            repr.tag = .ArrayAccessExpr;
            repr.data[0] = @bitCast(access.qt);
            repr.data[1] = @intFromEnum(access.base);
            repr.data[2] = @intFromEnum(access.index);
            repr.tok = access.lbracketToken;
        },
        .callExpr => |call| {
            repr.data[0] = @bitCast(call.qt);
            if (call.args.len > 1) {
                repr.tag = .CallExpr;
                repr.data[1] = @intCast(tree.extra.items.len);
                const len: u32 = @intCast(call.args.len + 1);
                repr.data[2] = len;
                try tree.extra.ensureUnusedCapacity(tree.comp.gpa, len);
                tree.extra.appendAssumeCapacity(@intFromEnum(call.callee));
                tree.extra.appendSliceAssumeCapacity(@ptrCast(call.args));
            } else {
                repr.tag = .CallExprOne;
                repr.data[1] = @intFromEnum(call.callee);
                repr.data[2] = packElem(call.args, 0);
            }
            repr.tok = call.lparenToken;
        },
        .builtinCallExpr => |call| {
            repr.data[0] = @bitCast(call.qt);
            if (call.args.len > 2) {
                repr.tag = .BuiltinCallExpr;
                repr.data[1], repr.data[2] = try tree.addExtra(call.args);
            } else {
                repr.tag = .BuiltinCallExprTwo;
                repr.data[1] = packElem(call.args, 0);
                repr.data[2] = packElem(call.args, 1);
            }
            repr.tok = call.builtinToken;
        },
        .memberAccessExpr => |access| {
            repr.tag = .MemberAccessExpr;
            repr.data[0] = @bitCast(access.qt);
            repr.data[1] = @intFromEnum(access.base);
            repr.data[2] = access.memberIndex;
            repr.tok = access.accessToken;
        },
        .memberAccessPtrExpr => |access| {
            repr.tag = .MemberAccessPtrExpr;
            repr.data[0] = @bitCast(access.qt);
            repr.data[1] = @intFromEnum(access.base);
            repr.data[2] = access.memberIndex;
            repr.tok = access.accessToken;
        },
        .declRefExpr => |declRef| {
            repr.tag = .DeclRefExpr;
            repr.data[0] = @bitCast(declRef.qt);
            repr.data[1] = @intFromEnum(declRef.decl);
            repr.tok = declRef.nameToken;
        },
        .enumerationRef => |enumRef| {
            repr.tag = .EnumerationRef;
            repr.data[0] = @bitCast(enumRef.qt);
            repr.data[1] = @intFromEnum(enumRef.decl);
            repr.tok = enumRef.nameToken;
        },
        .builtinRef => |builtinRef| {
            repr.tag = .BuiltinRef;
            repr.data[0] = @bitCast(builtinRef.qt);
            repr.tok = builtinRef.nameToken;
        },
        .boolLiteral => |literal| {
            repr.tag = .BoolLiteral;
            repr.data[0] = @bitCast(literal.qt);
            repr.tok = literal.literalToken;
        },
        .nullptrLiteral => |literal| {
            repr.tag = .NullptrLiteral;
            repr.data[0] = @bitCast(literal.qt);
            repr.tok = literal.literalToken;
        },
        .intLiteral => |literal| {
            repr.tag = .IntLiteral;
            repr.data[0] = @bitCast(literal.qt);
            repr.tok = literal.literalToken;
        },
        .charLiteral => |literal| {
            repr.tag = .CharLiteral;
            repr.data[0] = @bitCast(literal.qt);
            repr.data[1] = @intFromEnum(literal.kind);
            repr.tok = literal.literalToken;
        },
        .floatLiteral => |literal| {
            repr.tag = .FloatLiteral;
            repr.data[0] = @bitCast(literal.qt);
            repr.tok = literal.literalToken;
        },
        .stringLiteralExpr => |literal| {
            repr.tag = .StringLiteralExpr;
            repr.data[0] = @bitCast(literal.qt);
            repr.data[1] = @intFromEnum(literal.kind);
            repr.tok = literal.literalToken;
        },
        .imaginaryLiteral => |un| {
            repr.tag = .ImaginaryLiteral;
            repr.data[0] = @bitCast(un.qt);
            repr.data[1] = @intFromEnum(un.operand);
            repr.tok = un.opToken;
        },
        .sizeofExpr => |typeInfo| {
            repr.tag = .SizeofExpr;
            repr.data[0] = @bitCast(typeInfo.qt);
            repr.data[1] = packOptIndex(typeInfo.expr);
            repr.data[2] = @bitCast(typeInfo.operandQt);
            repr.tok = typeInfo.opToken;
        },
        .alignofExpr => |typeInfo| {
            repr.tag = .AlignofExpr;
            repr.data[0] = @bitCast(typeInfo.qt);
            repr.data[1] = packOptIndex(typeInfo.expr);
            repr.data[2] = @bitCast(typeInfo.operandQt);
            repr.tok = typeInfo.opToken;
        },
        .genericExpr => |generic| {
            repr.data[0] = @bitCast(generic.qt);
            if (generic.rest.len > 0) {
                repr.tag = .GenericExpr;
                repr.data[1] = @intCast(tree.extra.items.len);
                const len: u32 = @intCast(generic.rest.len + 2);
                repr.data[2] = len;
                try tree.extra.ensureUnusedCapacity(tree.comp.gpa, len);
                tree.extra.appendAssumeCapacity(@intFromEnum(generic.controlling));
                tree.extra.appendAssumeCapacity(@intFromEnum(generic.chosen));
                tree.extra.appendSliceAssumeCapacity(@ptrCast(generic.rest));
            } else {
                repr.tag = .GenericExprZero;
                repr.data[1] = @intFromEnum(generic.controlling);
                repr.data[2] = @intFromEnum(generic.chosen);
            }
            repr.tok = generic.genericToken;
        },
        .genericAssociationExpr => |association| {
            repr.tag = .GenericAssociationExpr;
            repr.data[0] = @bitCast(association.associationQt);
            repr.data[1] = @intFromEnum(association.expr);
            repr.tok = association.colonToken;
        },
        .genericDefaultExpr => |default| {
            repr.tag = .GenericDefaultExpr;
            repr.data[0] = @intFromEnum(default.expr);
            repr.tok = default.defaultToken;
        },
        .binaryCondExpr => |cond| {
            repr.tag = .BinaryCondExpr;
            repr.data[0] = @bitCast(cond.qt);
            repr.data[1] = @intFromEnum(cond.cond);
            repr.data[2], _ = try tree.addExtra(&.{ cond.thenExpr, cond.elseExpr });
            repr.tok = cond.condToken;
        },
        .condExpr => |cond| {
            repr.tag = .CondExpr;
            repr.data[0] = @bitCast(cond.qt);
            repr.data[1] = @intFromEnum(cond.cond);
            repr.data[2], _ = try tree.addExtra(&.{ cond.thenExpr, cond.elseExpr });
            repr.tok = cond.condToken;
        },
        .builtinChooseExpr => |cond| {
            repr.tag = .BuiltinChooseExpr;
            repr.data[0] = @bitCast(cond.qt);
            repr.data[1] = @intFromEnum(cond.cond);
            repr.data[2], _ = try tree.addExtra(&.{ cond.thenExpr, cond.elseExpr });
            repr.tok = cond.condToken;
        },
        .builtinTypesCompatibleP => |builtin| {
            repr.tag = .BuiltinTypesCompatibleP;
            repr.data[0] = @bitCast(builtin.lhs);
            repr.data[1] = @bitCast(builtin.rhs);
            repr.tok = builtin.builtinToken;
        },
        .arrayInitExpr => |init| {
            repr.data[0] = @bitCast(init.containerQt);
            if (init.items.len > 2) {
                repr.tag = .ArrayInitExpr;
                repr.data[1], repr.data[2] = try tree.addExtra(init.items);
            } else {
                repr.tag = .ArrayInitExprTwo;
                repr.data[1] = packElem(init.items, 0);
                repr.data[2] = packElem(init.items, 1);
            }
            repr.tok = init.lbraceToken;
        },
        .structInitExpr => |init| {
            repr.data[0] = @bitCast(init.containerQt);
            if (init.items.len > 2) {
                repr.tag = .StructInitExpr;
                repr.data[1], repr.data[2] = try tree.addExtra(init.items);
            } else {
                repr.tag = .StructInitExprTwo;
                repr.data[1] = packElem(init.items, 0);
                repr.data[2] = packElem(init.items, 1);
            }
            repr.tok = init.lbraceToken;
        },
        .unionInitExpr => |init| {
            repr.tag = .UnionInitExpr;
            repr.data[0] = @bitCast(init.unionQt);
            repr.data[1] = init.fieldIndex;
            repr.data[2] = packOptIndex(init.initializer);
            repr.tok = init.lbraceToken;
        },
        .arrayFillerExpr => |filler| {
            repr.tag = .ArrayFillerExpr;
            repr.data[0] = @bitCast(filler.qt);
            repr.data[1], repr.data[2] = @as([2]u32, @bitCast(filler.count));
            repr.tok = filler.lastToken;
        },
        .defaultInitExpr => |default| {
            repr.tag = .DefaultInitExpr;
            repr.data[0] = @bitCast(default.qt);
            repr.tok = default.lastToken;
        },
        .compoundLiteralExpr => |literal| {
            repr.tag = .CompoundLiteralExpr;
            repr.data[0] = @bitCast(literal.qt);
            repr.data[1] = @bitCast(Node.Repr.DeclAttr{
                .static = literal.storageClass == .static,
                .register = literal.storageClass == .register,
                .threadLocal = literal.threadLocal,
            });
            repr.data[2] = @intFromEnum(literal.initializer);
            repr.tok = literal.lparenToken;
        },
    }

    tree.nodes.set(index, repr);
}

fn packOptIndex(opt: ?Node.Index) u32 {
    return @intFromEnum(Node.OptIndex.packOpt(opt));
}

fn unpackOptIndex(idx: u32) ?Node.Index {
    return @as(Node.OptIndex, @enumFromInt(idx)).unpack();
}

fn packElem(nodes: []const Node.Index, index: usize) u32 {
    return if (nodes.len > index) @intFromEnum(nodes[index]) else @intFromEnum(Node.OptIndex.null);
}

fn unPackElems(data: []const u32) []const Node.Index {
    const sentinel = @intFromEnum(Node.OptIndex.null);
    for (data, 0..) |item, i| {
        if (item == sentinel) return @ptrCast(data[0..i]);
    }
    return @ptrCast(data);
}

/// Returns index to `tree.extra` and length of data
fn addExtra(tree: *Tree, data: []const Node.Index) !struct { u32, u32 } {
    const index: u32 = @intCast(tree.extra.items.len);
    try tree.extra.appendSlice(tree.comp.gpa, @ptrCast(data));
    return .{ index, @intCast(data.len) };
}

pub fn isBitField(tree: *const Tree, node: Node.Index) bool {
    return tree.bitfieldWidth(node, false) != null;
}

/// Returns null if node is not a bitfield. If inspect_lval is true, this function will
/// recurse into implicit lval_to_rval casts (useful for arithmetic conversions)
pub fn bitfieldWidth(tree: *const Tree, node: Node.Index, inspectLval: bool) ?u32 {
    switch (node.get(tree)) {
        .memberAccessExpr, .memberAccessPtrExpr => |access| return access.isBitFieldWidth(tree),
        .cast => |cast| {
            if (!inspectLval) return null;

            return switch (cast.kind) {
                .LValToRVal => tree.bitfieldWidth(cast.operand, false),
                else => null,
            };
        },
        else => return null,
    }
}

pub fn isLValue(tree: *const Tree, node: Node.Index) bool {
    var isConst: bool = undefined;
    return tree.isLValueExtra(node, &isConst);
}

pub fn isLValueExtra(tree: *const Tree, node: Node.Index, isConst: *bool) bool {
    isConst.* = false;

    var curNode = node;
    switch (curNode.get(tree)) {
        .compoundLiteralExpr => |literal| {
            isConst.* = literal.qt.@"const";
            return true;
        },

        .stringLiteralExpr => return true,
        .memberAccessPtrExpr => |access| {
            const ptrQualTy = access.base.qt(tree);
            if (ptrQualTy.get(tree.comp, .pointer)) |pointer| isConst.* = pointer.child.@"const";
            return true;
        },

        .memberAccessExpr => |access| return tree.isLValueExtra(access.base, isConst),

        .arrayAccessExpr => |access| {
            const baseQualTy = access.base.qt(tree);
            // Array access operand undergoes lval conversions so the base can never
            // be a pure array type.
            if (baseQualTy.get(tree.comp, .pointer)) |pointer| isConst.* = pointer.child.@"const";
            return true;
        },

        .declRefExpr => |declRef| {
            isConst.* = declRef.qt.@"const";
            return true;
        },

        .derefExpr => |un| {
            const operandQualTy = un.operand.qt(tree);
            switch (operandQualTy.base(tree.comp).type) {
                .func => return false,
                .pointer => |pointer| isConst.* = pointer.child.@"const",
                else => {},
            }
            return true;
        },

        .parenExpr => |un| return tree.isLValueExtra(un.operand, isConst),

        .builtinChooseExpr => |conditional| {
            if (tree.valueMap.get(conditional.cond)) |val| {
                if (!val.isZero(tree.comp))
                    return tree.isLValueExtra(conditional.thenExpr, isConst)
                else
                    return tree.isLValueExtra(conditional.elseExpr, isConst);
            }
            return false;
        },

        .compoundAssignDummyExpr => return true,
        else => return false,
    }
}

const CallableResultUsage = struct {
    /// name token of the thing being called, for diagnostics
    token: TokenIndex,
    /// true if `nodiscard` attribute present
    nodiscard: bool,
    /// true if `warn_unused_result` attribute present
    warnUnusedResult: bool,
};

pub fn callableResultUsage(tree: *const Tree, node: Node.Index) ?CallableResultUsage {
    var curNode = node;
    while (true) switch (curNode.get(tree)) {
        .declRefExpr => |declRef| return .{
            .token = declRef.nameToken,
            .nodiscard = declRef.qt.hasAttribute(tree.comp, .nodiscard),
            .warnUnusedResult = declRef.qt.hasAttribute(tree.comp, .warn_unused_result),
        },
        .parenExpr, .addrOfExpr, .derefExpr => |un| curNode = un.operand,
        .commaExpr => |bin| curNode = bin.rhs,

        .cast => |cast| curNode = cast.operand,
        .callExpr => |call| curNode = call.callee,

        .memberAccessExpr, .memberAccessPtrExpr => |access| {
            var qt = access.base.qt(tree);
            if (qt.get(tree.comp, .pointer)) |pointer| qt = pointer.child;
            const recordTy = switch (qt.base(tree.comp).type) {
                .@"struct", .@"union" => |record| record,
                else => return null,
            };

            const field = recordTy.fields[access.memberIndex];
            const attributes = field.attributes(tree.comp);
            return .{
                .token = field.nameToken,
                .nodiscard = for (attributes) |attr| {
                    if (attr.tag == .nodiscard) break true;
                } else false,
                .warnUnusedResult = for (attributes) |attr| {
                    if (attr.tag == .warn_unused_result) break true;
                } else false,
            };
        },
        else => return null,
    };
}

pub fn tokenSlice(tree: Tree, tokenIdx: TokenIndex) []const u8 {
    if (tree.tokens.items(.id)[tokenIdx].lexeme()) |some| return some;
    const loc = tree.tokens.items(.loc)[tokenIdx];
    return tree.comp.locSlice(loc);
}

pub fn dump(tree: *const Tree, config: std.Io.tty.Config, w: *std.Io.Writer) std.Io.tty.Config.SetColorError!void {
    for (tree.rootDecls.items) |i| {
        try tree.dumpNode(i, 0, config, w);
        try w.writeByte('\n');
    }
    try w.flush();
}

fn dumpFieldAttributes(tree: *const Tree, attributes: []const Attribute, level: u32, w: *std.Io.Writer) !void {
    for (attributes) |attr| {
        try w.splatByteAll(' ', level);
        try w.print("field attr: {s}", .{@tagName(attr.tag)});
        try tree.dumpAttribute(attr, w);
    }
}

fn dumpAttribute(tree: *const Tree, attr: Attribute, w: *std.Io.Writer) !void {
    switch (attr.tag) {
        inline else => |tag| {
            const args = @field(attr.args, @tagName(tag));
            const fields = @typeInfo(@TypeOf(args)).@"struct".fields;
            if (fields.len == 0) {
                try w.writeByte('\n');
                return;
            }
            try w.writeByte(' ');
            inline for (fields, 0..) |f, i| {
                if (comptime std.mem.eql(u8, f.name, "__name_token")) continue;
                if (i != 0) {
                    try w.writeAll(", ");
                }
                try w.writeAll(f.name);
                try w.writeAll(": ");
                switch (f.type) {
                    Interner.Ref => try w.print("\"{s}\"", .{tree.interner.get(@field(args, f.name)).bytes}),
                    ?Interner.Ref => try w.print("\"{?s}\"", .{if (@field(args, f.name)) |str| tree.interner.get(str).bytes else null}),
                    else => switch (@typeInfo(f.type)) {
                        .@"enum" => try w.writeAll(@tagName(@field(args, f.name))),
                        else => try w.print("{any}", .{@field(args, f.name)}),
                    },
                }
            }
            try w.writeByte('\n');
            return;
        },
    }
}

fn dumpNode(
    tree: *const Tree,
    nodeIndex: Node.Index,
    level: u32,
    config: std.Io.tty.Config,
    w: *std.Io.Writer,
) !void {
    const delta = 2;
    const half = delta / 2;
    const TYPE = std.Io.tty.Color.bright_magenta;
    const TAG = std.Io.tty.Color.bright_cyan;
    const IMPLICIT = std.Io.tty.Color.bright_blue;
    const NAME = std.Io.tty.Color.bright_red;
    const LITERAL = std.Io.tty.Color.bright_green;
    const ATTRIBUTE = std.Io.tty.Color.bright_yellow;

    const node = nodeIndex.get(tree);
    try w.splatByteAll(' ', level);

    if (config == .no_color) {
        if (node.isImplicit()) try w.writeAll("implicit ");
    } else {
        try config.setColor(w, if (node.isImplicit()) IMPLICIT else TAG);
    }

    try w.print("{s}", .{@tagName(node)});

    if (nodeIndex.qtOrNull(tree)) |qt| {
        try w.writeAll(": ");
        switch (node) {
            .cast => |cast| {
                try config.setColor(w, .white);
                try w.print("({s}) ", .{@tagName(cast.kind)});
            },
            else => {},
        }

        try config.setColor(w, TYPE);
        try w.writeByte('\'');
        try qt.dump(tree.comp, w);
        try w.writeByte('\'');
    }

    if (tree.isLValue(nodeIndex)) {
        try config.setColor(w, ATTRIBUTE);
        try w.writeAll(" lvalue");
    }

    if (tree.isBitField(nodeIndex)) {
        try config.setColor(w, ATTRIBUTE);
        try w.writeAll(" bitfield");
    }

    if (tree.valueMap.get(nodeIndex)) |val| {
        try config.setColor(w, LITERAL);
        try w.writeAll(" (value: ");
        if (try val.print(nodeIndex.qt(tree), tree.comp, w)) |nested| switch (nested) {
            .pointer => |ptr| {
                switch (tree.nodes.items(.tag)[ptr.node]) {
                    .CompoundLiteralExpr => {
                        try w.writeAll("(compound literal): ");
                        _ = try ptr.offset.print(tree.comp.typeStore.ptrdiff, tree.comp, w);
                    },
                    else => {
                        const ptrNode: Node.Index = @enumFromInt(ptr.node);
                        const declName = tree.tokenSlice(ptrNode.tok(tree));
                        try ptr.offset.printPointer(declName, tree.comp, w);
                    },
                }
            },
        };
        try w.writeByte(')');
    }

    if (node == .returnStmt and node.returnStmt.operand == .implicit and node.returnStmt.operand.implicit) {
        try config.setColor(w, IMPLICIT);
        try w.writeAll(" (value: 0)");
        try config.setColor(w, .reset);
    }

    try w.writeAll("\n");
    try config.setColor(w, .reset);

    if (nodeIndex.qtOrNull(tree)) |qt| {
        try config.setColor(w, ATTRIBUTE);
        var it = Attribute.Iterator.initType(qt, tree.comp);
        while (it.next()) |item| {
            const attr, _ = item;
            try w.splatByteAll(' ', level + half);
            try w.print("attr: {s}", .{@tagName(attr.tag)});
            try tree.dumpAttribute(attr, w);
        }
        try config.setColor(w, .reset);
    }

    switch (node) {
        .emptyDecl => {},
        .globalAsm, .gnuAsmSimple => |@"asm"| {
            try w.splatByteAll(' ', level + 1);
            try tree.dumpNode(@"asm".asmString, level + delta, config, w);
        },

        .staticAssert => |assert| {
            try w.splatByteAll(' ', level + 1);
            try w.writeAll("condition:\n");
            try tree.dumpNode(assert.cond, level + delta, config, w);

            if (assert.message) |some| {
                try w.splatByteAll(' ', level + 1);
                try w.writeAll("diagnostic:\n");
                try tree.dumpNode(some, level + delta, config, w);
            }
        },

        .fnProto => |proto| {
            try w.splatByteAll(' ', level + half);

            try config.setColor(w, ATTRIBUTE);
            if (proto.static) try w.writeAll("static ");
            if (proto.@"inline") try w.writeAll("inline ");

            try config.setColor(w, .reset);
            try w.writeAll("name: ");

            try config.setColor(w, NAME);
            try w.print("{s}\n", .{tree.tokenSlice(proto.nameToken)});
            try config.setColor(w, .reset);
        },

        .fnDef => |def| {
            try w.splatByteAll(' ', level + half);
            try config.setColor(w, ATTRIBUTE);
            if (def.static) try w.writeAll("static ");
            if (def.@"inline") try w.writeAll("inline ");

            try config.setColor(w, .reset);
            try w.writeAll("name: ");

            try config.setColor(w, NAME);
            try w.print("{s}\n", .{tree.tokenSlice(def.nameToken)});

            try config.setColor(w, .reset);
            try w.splatByteAll(' ', level + half);
            try w.writeAll("body:\n");
            try tree.dumpNode(def.body, level + delta, config, w);
        },

        .typedef => |typedef| {
            try w.splatByteAll(' ', level + half);
            try w.writeAll("name: ");
            try config.setColor(w, NAME);
            try w.print("{s}\n", .{tree.tokenSlice(typedef.nameToken)});
            try config.setColor(w, .reset);
        },

        .param => |param| {
            try w.splatByteAll(' ', level + half);

            switch (param.storageClass) {
                .auto => {},
                .register => {
                    try config.setColor(w, ATTRIBUTE);
                    try w.writeAll("register ");
                    try config.setColor(w, .reset);
                },
            }

            try w.writeAll("name: ");
            try config.setColor(w, NAME);
            try w.print("{s}\n", .{tree.tokenSlice(param.nameToken)});
            try config.setColor(w, .reset);
        },

        .variable => |variable| {
            try w.splatByteAll(' ', level + half);

            try config.setColor(w, ATTRIBUTE);
            switch (variable.storageClass) {
                .auto => {},
                .static => try w.writeAll("static "),
                .@"extern" => try w.writeAll("extern "),
                .register => try w.writeAll("register "),
            }
            if (variable.threadLocal) try w.writeAll("thread_local ");

            try config.setColor(w, .reset);
            try w.writeAll("name: ");

            try config.setColor(w, NAME);
            try w.print("{s}\n", .{tree.tokenSlice(variable.nameToken)});
            try config.setColor(w, .reset);

            if (variable.initializer) |some| {
                try config.setColor(w, .reset);
                try w.splatByteAll(' ', level + half);
                try w.writeAll("init:\n");
                try tree.dumpNode(some, level + delta, config, w);
            }
        },

        .enumField => |field| {
            try w.splatByteAll(' ', level + half);
            try w.writeAll("name: ");

            try config.setColor(w, NAME);
            try w.print("{s}\n", .{tree.tokenSlice(field.nameToken)});
            try config.setColor(w, .reset);

            if (field.init) |some| {
                try w.splatByteAll(' ', level + half);
                try w.writeAll("init:\n");
                try tree.dumpNode(some, level + delta, config, w);
            }
        },

        .recordField => |field| {
            const nameTokenID = tree.tokens.items(.id)[field.nameOrFirstToken];
            if (nameTokenID == .Identifier or nameTokenID == .ExtendedIdentifier) {
                try w.splatByteAll(' ', level + half);
                try w.writeAll("name: ");

                try config.setColor(w, NAME);
                try w.print("{s}\n", .{tree.tokenSlice(field.nameOrFirstToken)});
                try config.setColor(w, .reset);
            }

            if (field.bitWidth) |some| {
                try w.splatByteAll(' ', level + half);
                try w.writeAll("bits:\n");
                try tree.dumpNode(some, level + delta, config, w);
            }
        },

        .compoundStmt => |compound| {
            for (compound.body, 0..) |stmt, i| {
                if (i != 0) try w.writeByte('\n');
                try tree.dumpNode(stmt, level + delta, config, w);
            }
        },

        .enumDecl => |decl| {
            for (decl.fields, 0..) |field, i| {
                if (i != 0) try w.writeByte('\n');
                try tree.dumpNode(field, level + delta, config, w);
            }
        },

        .unionDecl, .structDecl => |decl| {
            const fields = switch (nodeIndex.qt(tree).base(tree.comp).type) {
                .@"struct", .@"union" => |record| record.fields,
                else => unreachable,
            };

            var fieldIndex: u32 = 0;
            for (decl.fields, 0..) |fieldNode, i| {
                if (i != 0) try w.writeByte('\n');
                try tree.dumpNode(fieldNode, level + delta, config, w);

                if (fieldNode.get(tree) != .recordField) continue;
                if (fields.len == 0) continue;

                const fieldAttributes = fields[fieldIndex].attributes(tree.comp);
                fieldIndex += 1;

                if (fieldAttributes.len == 0) continue;

                try config.setColor(w, ATTRIBUTE);
                try tree.dumpFieldAttributes(fieldAttributes, level + delta + half, w);
                try config.setColor(w, .reset);
            }
        },

        .arrayInitExpr, .structInitExpr => |init| {
            for (init.items, 0..) |item, i| {
                if (i != 0) try w.writeByte('\n');
                try tree.dumpNode(item, level + delta, config, w);
            }
        },

        .unionInitExpr => |init| {
            try w.splatByteAll(' ', level + half);
            try w.writeAll("field index: ");

            try config.setColor(w, LITERAL);
            try w.print("{d}\n", .{init.fieldIndex});

            try config.setColor(w, .reset);
            if (init.initializer) |some| {
                try tree.dumpNode(some, level + delta, config, w);
            }
        },

        .compoundLiteralExpr => |literal| {
            if (literal.storageClass != .auto or literal.threadLocal) {
                try w.splatByteAll(' ', level + half - 1);

                try config.setColor(w, ATTRIBUTE);
                switch (literal.storageClass) {
                    .auto => {},
                    .static => try w.writeAll(" static"),
                    .register => try w.writeAll(" register"),
                }
                try w.writeByte('\n');
                try config.setColor(w, .reset);
            }
            try tree.dumpNode(literal.initializer, level + half, config, w);
        },

        .labeledStmt => |labeled| {
            try w.splatByteAll(' ', level + half);
            try w.writeAll("label: ");

            try config.setColor(w, LITERAL);
            try w.print("{s}\n", .{tree.tokenSlice(labeled.labelToken)});

            try config.setColor(w, .reset);
            try w.splatByteAll(' ', level + half);
            try w.writeAll("stmt:\n");
            try tree.dumpNode(labeled.body, level + delta, config, w);
        },

        .binaryCondExpr, .condExpr, .builtinChooseExpr => |conditional| {
            try w.splatByteAll(' ', level + half);
            try w.writeAll("cond:\n");
            try tree.dumpNode(conditional.cond, level + delta, config, w);

            try w.splatByteAll(' ', level + half);
            try w.writeAll("then:\n");
            try tree.dumpNode(conditional.thenExpr, level + delta, config, w);

            try w.splatByteAll(' ', level + half);
            try w.writeAll("else:\n");
            try tree.dumpNode(conditional.elseExpr, level + delta, config, w);
        },

        .builtinTypesCompatibleP => |call| {
            try w.splatByteAll(' ', level + half);
            try w.writeAll("lhs: ");

            try config.setColor(w, TYPE);
            try call.lhs.dump(tree.comp, w);
            try w.writeByte('\n');
            try config.setColor(w, .reset);

            try w.splatByteAll(' ', level + half);
            try w.writeAll("rhs: ");
            try config.setColor(w, TYPE);
            try call.rhs.dump(tree.comp, w);
            try w.writeByte('\n');
            try config.setColor(w, .reset);
        },

        .ifStmt => |@"if"| {
            try w.splatByteAll(' ', level + half);
            try w.writeAll("cond:\n");
            try tree.dumpNode(@"if".cond, level + delta, config, w);

            try w.splatByteAll(' ', level + half);
            try w.writeAll("then:\n");
            try tree.dumpNode(@"if".thenBody, level + delta, config, w);

            if (@"if".elseBody) |some| {
                try w.splatByteAll(' ', level + half);
                try w.writeAll("else:\n");
                try tree.dumpNode(some, level + delta, config, w);
            }
        },

        .addrOfLabel => |addr| {
            try w.splatByteAll(' ', level + half);
            try w.writeAll("label: ");
            try config.setColor(w, LITERAL);
            try w.print("{s}\n", .{tree.tokenSlice(addr.labelToken)});
            try config.setColor(w, .reset);
        },

        .gotoStmt => |goto| {
            try w.splatByteAll(' ', level + half);
            try w.writeAll("label: ");
            try config.setColor(w, LITERAL);
            try w.print("{s}\n", .{tree.tokenSlice(goto.labelToken)});
            try config.setColor(w, .reset);
        },

        .computedGotoStmt => |goto| {
            try w.splatByteAll(' ', level + half);
            try w.writeAll("expr:\n");
            try tree.dumpNode(goto.expr, level + delta, config, w);
        },

        .nullStmt, .continueStmt, .breakStmt => {},

        .returnStmt => |ret| {
            switch (ret.operand) {
                .expr => |expr| {
                    try w.splatByteAll(' ', level + half);
                    try w.writeAll("expr:\n");
                    try tree.dumpNode(expr, level + delta, config, w);
                },
                .implicit => {},
                .none => {},
            }
        },

        .forStmt => |@"for"| {
            switch (@"for".init) {
                .decls => |decls| {
                    try w.splatByteAll(' ', level + half);
                    try w.writeAll("decl:\n");
                    for (decls) |decl| {
                        try tree.dumpNode(decl, level + delta, config, w);
                        try w.writeByte('\n');
                    }
                },
                .expr => |expr| if (expr) |some| {
                    try w.splatByteAll(' ', level + half);
                    try w.writeAll("init:\n");
                    try tree.dumpNode(some, level + delta, config, w);
                },
            }

            if (@"for".cond) |some| {
                try w.splatByteAll(' ', level + half);
                try w.writeAll("cond:\n");
                try tree.dumpNode(some, level + delta, config, w);
            }
            if (@"for".incr) |some| {
                try w.splatByteAll(' ', level + half);
                try w.writeAll("incr:\n");
                try tree.dumpNode(some, level + delta, config, w);
            }
            try w.splatByteAll(' ', level + half);
            try w.writeAll("body:\n");
            try tree.dumpNode(@"for".body, level + delta, config, w);
        },

        .switchStmt => |@"switch"| {
            try w.splatByteAll(' ', level + half);
            try w.writeAll("cond:\n");
            try tree.dumpNode(@"switch".cond, level + delta, config, w);

            try w.splatByteAll(' ', level + half);
            try w.writeAll("body:\n");
            try tree.dumpNode(@"switch".body, level + delta, config, w);
        },

        .whileStmt => |@"while"| {
            try w.splatByteAll(' ', level + half);
            try w.writeAll("cond:\n");
            try tree.dumpNode(@"while".cond, level + delta, config, w);

            try w.splatByteAll(' ', level + half);
            try w.writeAll("body:\n");
            try tree.dumpNode(@"while".body, level + delta, config, w);
        },

        .doWhileStmt => |do| {
            try w.splatByteAll(' ', level + half);
            try w.writeAll("cond:\n");
            try tree.dumpNode(do.cond, level + delta, config, w);

            try w.splatByteAll(' ', level + half);
            try w.writeAll("body:\n");
            try tree.dumpNode(do.body, level + delta, config, w);
        },

        .caseStmt => |case| {
            try w.splatByteAll(' ', level + half);

            if (case.end) |some| {
                try w.writeAll("range start:\n");
                try tree.dumpNode(case.start, level + delta, config, w);

                try w.splatByteAll(' ', level + half);
                try w.writeAll("range end:\n");
                try tree.dumpNode(some, level + delta, config, w);
            } else {
                try w.writeAll("value:\n");
                try tree.dumpNode(case.start, level + delta, config, w);
            }

            try w.splatByteAll(' ', level + half);
            try w.writeAll("stmt:\n");
            try tree.dumpNode(case.body, level + delta, config, w);
        },

        .defaultStmt => |default| {
            try w.splatByteAll(' ', level + half);
            try w.writeAll("stmt:\n");
            try tree.dumpNode(default.body, level + delta, config, w);
        },

        .callExpr => |call| {
            try w.splatByteAll(' ', level + half);
            try w.writeAll("callee:\n");
            try tree.dumpNode(call.callee, level + delta, config, w);

            if (call.args.len > 0) {
                try w.splatByteAll(' ', level + half);
                try w.writeAll("args:\n");
                for (call.args) |arg| {
                    try tree.dumpNode(arg, level + delta, config, w);
                }
            }
        },

        .builtinCallExpr => |call| {
            try w.splatByteAll(' ', level + half);
            try w.writeAll("name: ");

            try config.setColor(w, NAME);
            try w.print("{s}\n", .{tree.tokenSlice(call.builtinToken)});
            try config.setColor(w, .reset);

            if (call.args.len > 0) {
                try w.splatByteAll(' ', level + half);
                try w.writeAll("args:\n");
                for (call.args) |arg| {
                    try tree.dumpNode(arg, level + delta, config, w);
                }
            }
        },

        .assignExpr,
        .mulAssignExpr,
        .divAssignExpr,
        .modAssignExpr,
        .addAssignExpr,
        .subAssignExpr,
        .shlAssignExpr,
        .shrAssignExpr,
        .bitAndAssignExpr,
        .bitXorAssignExpr,
        .bitOrAssignExpr,
        .boolOrExpr,
        .boolAndExpr,
        .bitOrExpr,
        .bitXorExpr,
        .bitAndExpr,
        .commaExpr,
        .equalExpr,
        .notEqualExpr,
        .lessThanExpr,
        .lessThanEqualExpr,
        .greaterThanExpr,
        .greaterThanEqualExpr,
        .shlExpr,
        .shrExpr,
        .addExpr,
        .subExpr,
        .mulExpr,
        .divExpr,
        .modExpr,
        => |bin| {
            try w.splatByteAll(' ', level + 1);
            try w.writeAll("lhs:\n");
            try tree.dumpNode(bin.lhs, level + delta, config, w);

            try w.splatByteAll(' ', level + 1);
            try w.writeAll("rhs:\n");
            try tree.dumpNode(bin.rhs, level + delta, config, w);
        },

        .cast => |cast| try tree.dumpNode(cast.operand, level + delta, config, w),

        .addrOfExpr,
        .derefExpr,
        .plusExpr,
        .negateExpr,
        .bitNotExpr,
        .boolNotExpr,
        .preIncExpr,
        .preDecExpr,
        .imagExpr,
        .realExpr,
        .postIncExpr,
        .postDecExpr,
        .parenExpr,
        .stmtExpr,
        .imaginaryLiteral,
        => |un| {
            try w.splatByteAll(' ', level + 1);
            try w.writeAll("operand:\n");
            try tree.dumpNode(un.operand, level + delta, config, w);
        },

        .declRefExpr, .enumerationRef => |dr| {
            try w.splatByteAll(' ', level + 1);
            try w.writeAll("name: ");

            try config.setColor(w, NAME);
            try w.print("{s}\n", .{tree.tokenSlice(dr.nameToken)});
            try config.setColor(w, .reset);
        },

        .builtinRef => |ndr| {
            try w.splatByteAll(' ', level + 1);
            try w.writeAll("name: ");

            try config.setColor(w, NAME);
            try w.print("{s}\n", .{tree.tokenSlice(ndr.nameToken)});
            try config.setColor(w, .reset);
        },

        .boolLiteral,
        .nullptrLiteral,
        .charLiteral,
        .intLiteral,
        .floatLiteral,
        .stringLiteralExpr,
        => {},

        .memberAccessExpr, .memberAccessPtrExpr => |access| {
            try w.splatByteAll(' ', level + 1);
            try w.writeAll("lhs:\n");
            try tree.dumpNode(access.base, level + delta, config, w);

            var baseQualTy = access.base.qt(tree);
            if (baseQualTy.get(tree.comp, .pointer)) |some| baseQualTy = some.child;
            const fields = (baseQualTy.getRecord(tree.comp) orelse return).fields;

            try w.splatByteAll(' ', level + 1);
            try w.writeAll("name: ");

            try config.setColor(w, NAME);
            try w.print("{s}\n", .{fields[access.memberIndex].name.lookup(tree.comp)});
            try config.setColor(w, .reset);
        },

        .arrayAccessExpr => |access| {
            try w.splatByteAll(' ', level + 1);
            try w.writeAll("lhs:\n");
            try tree.dumpNode(access.base, level + delta, config, w);

            try w.splatByteAll(' ', level + 1);
            try w.writeAll("index:\n");
            try tree.dumpNode(access.index, level + delta, config, w);
        },

        .sizeofExpr, .alignofExpr => |typeInfo| {
            if (typeInfo.expr) |some| {
                try w.splatByteAll(' ', level + 1);
                try w.writeAll("expr:\n");
                try tree.dumpNode(some, level + delta, config, w);
            } else {
                try w.splatByteAll(' ', level + half);
                try w.writeAll("operand type: ");
                try config.setColor(w, TYPE);
                try typeInfo.operandQt.dump(tree.comp, w);
                try w.writeByte('\n');
                try config.setColor(w, .reset);
            }
        },

        .genericExpr => |generic| {
            try w.splatByteAll(' ', level + 1);
            try w.writeAll("controlling:\n");
            try tree.dumpNode(generic.controlling, level + delta, config, w);
            try w.splatByteAll(' ', level + 1);
            try w.writeAll("chosen:\n");
            try tree.dumpNode(generic.chosen, level + delta, config, w);

            if (generic.rest.len > 0) {
                try w.splatByteAll(' ', level + 1);
                try w.writeAll("rest:\n");
                for (generic.rest) |expr| {
                    try tree.dumpNode(expr, level + delta, config, w);
                }
            }
        },

        .genericAssociationExpr => |assoc| {
            try tree.dumpNode(assoc.expr, level + delta, config, w);
        },

        .genericDefaultExpr => |default| {
            try tree.dumpNode(default.expr, level + delta, config, w);
        },

        .arrayFillerExpr => |filler| {
            try w.splatByteAll(' ', level + 1);
            try w.writeAll("count: ");
            try config.setColor(w, LITERAL);
            try w.print("{d}\n", .{filler.count});
            try config.setColor(w, .reset);
        },

        .structForwardDecl,
        .unionForwardDecl,
        .enumForwardDecl,
        .defaultInitExpr,
        .condDummyExpr,
        .compoundAssignDummyExpr,
        => {},
    }
}
