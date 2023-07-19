const std = @import("std");
const Type = @import("Type.zig");
const Token = @import("Token.zig").Token;
const Compilation = @import("Compilation.zig");

const AST = @This();

pub const TokenIndex = u32;
pub const NodeIndex = u32;

comp: *Compilation,
arena: std.heap.ArenaAllocator,
generated: []const u8,
tokens: []const Token,
nodes: Node.List.Slice,
rootDecls:[]const NodeIndex,

pub fn deinit(tree: *AST) void {
    tree.comp.gpa.free(tree.rootDecls);
    tree.nodes.deinit(tree.comp.gpa);
    tree.arena.deinit();
}

pub const Node = struct {
    tag: Tag,
    type: Type,
    first: NodeIndex = 0,
    second: NodeIndex = 0,
    third: NodeIndex = 0,

    pub const List = std.MultiArrayList(Node);
};

pub fn tokSlice(tree: AST, index: TokenIndex) []const u8 {
    const token = tree.tokens[index];
    if (token.id.lexeMe()) |some| return some;

    if (token.source.isGenerated()) {
        return tree.generated[token.loc.start..token.loc.end];
    } else {
        const source = tree.comp.getSource(token.source);
        return source.buffer[token.loc.start..token.loc.end];
    }
}

pub const Tag = enum(u8) {
    /// Only appears at index 0 and reaching it is always a result of a bug.
    invalid,

    // ====== Decl ======

    // function prototype
    FnProto,
    ExternFnProto,
    StaticFnProto,
    InlineFnProto,
    InlineExternFnProto,
    InlineStaticFnProto,
    NoreturnFnProto,
    NoreturnExternFnProto,
    NoreturnStaticFnProto,
    NoreturnInlineFnProto,
    NoreturnInlineExternFnProto,
    NoreturnInlineStaticFnProto,

    // function definition
    FnDef,
    ExternFnDef,
    StaticFnDef,
    InlineFnDef,
    InlineExternFnDef,
    InlineStaticFnDef,
    NoreturnFnDef,
    NoreturnExternFnDef,
    NoreturnStaticFnDef,
    NoreturnInlineFnDef,
    NoreturnInlineExternFnDef,
    NoreturnInlineStaticFnDef,

    // variable declaration
    Var,
    AutoVar,
    ExternVar,
    StaticVar,
    RegisterVar,
    ThreadlocalVar,
    ThreadlocalAutoVar,
    ThreadlocalExternVar,
    ThreadlocalStaticVar,
    ThreadlocalRegisterVar,

    // typdef declaration
    TypeDef,

    // container type forward declarations
    StructForward,
    UnionForward,
    EnumForawrd,

    // container type definitions
    StructDef,
    UnionDef,
    EnumDef,

    // ====== Stmt ======
    LabeledStmt,
    CompoundStmt,
    IfStmt,
    SwitchStmt,
    WhileStmt,
    DoWhileStmt,
    ForStmt,
    GotoStmt,
    ContinueStmt,
    BreakStmt,
    ReturnStmt,

    // ====== Expr ======
    /// lhs , rhs
    CommaExpr,
    /// lhs ?: rhs
    BinaryCondExpr,
    /// TODO
    CondExpr,
    /// lhs = rhs
    AssignExpr,
    /// lhs *= rhs
    MulAssignExpr,
    /// lhs /= rhs
    DivAssignExpr,
    /// lhs %= rhs
    ModAssignExpr,
    /// lhs += rhs
    AddAssignExpr,
    /// lhs -= rhs
    SubAssignExpr,
    /// lhs <<= rhs
    ShlAssignExpr,
    /// lhs >>= rhs
    ShrAssignExpr,
    /// lhs &= rhs
    AndAssignExpr,
    /// lhs ^= rhs
    XorAssignExpr,
    /// lhs |= rhs
    OrAssignExpr,
    /// lhs || rhs
    BoolOrExpr,
    /// lhs && rhs
    BoolAndExpr,
    /// lhs | rhs
    BitOrExpr,
    /// lhs ^ rhs
    BitXorExpr,
    /// lhs & rhs
    BitAndExpr,
    /// lhs == rhs
    EqualExpr,
    /// lhs != rhs
    NotEqualExpr,
    /// lhs < rhs
    LessThanExpr,
    /// lhs <= rhs
    LessThanEqualExpr,
    /// lhs > rhs
    GreaterThanExpr,
    /// lhs >= rhs
    GreaterThanEqualExpr,
    /// lhs << rhs
    ShlExpr,
    /// lhs >> rhs
    ShrExpr,
    /// lhs + rhs
    AddExpr,
    /// lhs - rhs
    SubExpr,
    /// lhs * rhs
    MulExpr,
    /// lhs / rhs
    DivExpr,
    /// lhs % rhs
    ModExpr,
    /// Explicit (type)lhs
    CastExpr,
    /// &lhs
    AddrOfExpr,
    /// *lhs
    DerefExpr,
    /// +lhs
    PlusExpr,
    /// -lhs
    NegateExpr,
    /// ~lhs
    BitNotExpr,

    /// !lhs
    BoolNotExpr,
    /// ++lhs
    PreIncExpr,
    /// --lhs
    PreDecExpr,
    /// lhs[rhs]  lhs is pointer/array type, rhs is integer type
    ArrayAccessExpr,
    /// TODO
    CallExpr,
    /// lhs.rhs rhs is a TokenIndex of the identifier
    MemberAccessExpr,
    /// lhs->rhs rhs is a TokenIndex of the identifier
    MemberAccessPtrExpr,
    /// lhs++
    PostIncExpr,
    /// lhs--
    PostDecExpr,
    /// lhs is a TokenIndex of the identifier
    DeclRefExpr,
    /// TODO
    IntegerLiteralExpr,
    /// TODO
    FloatLiteralExpr,
    /// TODO
    CharLiteralExpr,
    /// TODO
    StringLiteralExpr,
    /// TODO
    CompoundLiteralExpr,

    /// Asserts that the tag is an expression.
    pub fn isLeftvalue(tag: Tag) bool {
        return switch (tag) {
            .CommaExpr,
            .BinaryCondExpr,
            .CondExpr,
            .AssignExpr,
            .MulAssignExpr,
            .DivAssignExpr,
            .ModAssignExpr,
            .AddAssignExpr,
            .SubAssignExpr,
            .ShlAssignExpr,
            .ShrAssignExpr,
            .AndAssignExpr,
            .XorAssignExpr,
            .OrAssignExpr,
            .BoolOrExpr,
            .BoolAndExpr,
            .BitOrExpr,
            .BitXorExpr,
            .BitAndExpr,
            .EqualExpr,
            .NotEqualExpr,
            .LessThanExpr,
            .LessThanEqualExpr,
            .GreaterThanExpr,
            .GreaterThanEqualExpr,
            .ShlExpr,
            .ShrExpr,
            .AddExpr,
            .SubExpr,
            .MulExpr,
            .DivExpr,
            .ModExpr,
            .CastExpr,
            .AddrOfExpr,
            .DerefExpr,
            .PlusExpr,
            .NegateExpr,
            .BitNotExpr,
            .BoolNotExpr,
            .PreIncExpr,
            .PreDecExpr,
            .ArrayAccessExpr,
            .CallExpr,
            => false,

            .DeclRefExpr,
            .StringLiteralExpr,
            .CompoundLiteralExpr,
            // .member_access_expr, if lhs.isLval()
            .MemberAccessPtrExpr,
            .Deref,
            .ArrayAccessExpr,
            => true,

            else => unreachable,
        };
    }

    pub fn Type(comptime tag: Tag) ?type {
        return switch (tag) {
            .Invalid => unreachable,
            .FnProto,
            .ExternFnProto,
            .StaticFnProto,
            .InlineFnProto,
            .InlineExternFnProto,
            .InlineStaticFnProto,
            => Declaration.FnProto,

            .FnDef,
            .ExternFnDef,
            .StaticFnDef,
            .InlineFnDef,
            .InlineExternFnDef,
            .InlineStaticFnDef,
            => Declaration.FnDef,

            .Var,
            .AutoVar,
            .ExternVar,
            .StaticVar,
            .RegisterVar,
            .ThreadlocalVar,
            .ThreadlocalAutoVar,
            .ThreadlocalExternVar,
            .ThreadlocalStaticVar,
            .ThreadlocalRegisterVar,
            => Declaration.Var,

            .TypeDef => Declaration.Typedef,

            .StructForward,
            .UnionForward,
            => Declaration.RecordForward,

            .EnumForawrd => Declaration.EnumForward,

            .StructDef,
            .UnionDef,
            => Declaration.Record,

            .EnumDef => Declaration.Enum,
            .FieldDecl => Declaration.Field,

            .LabeledStmt => Statement.Labeled,
            .CompoundStmt => Statement.Compound,
            .IfStmt => Statement.If,
            .SwitchStmt => Statement.Switch,
            .WhileStmt, .DoWhileStmt => Statement.While,
            .ForStmt => Statement.For,
            .GotoStmt => Statement.Goto,
            .ContinueStmt, .BreakStmt => null,
            .ReturnStmt => Statement.Return,

            .CommaExpr,
            .BinaryCondExpr,
            .AssignExpr,
            .MulAssignExpr,
            .DivAssignExpr,
            .ModAssignExpr,
            .AddAssignExpr,
            .SubAssignExpr,
            .ShlAssignExpr,
            .ShrAssignExpr,
            .AndAssignExpr,
            .XorAssignExpr,
            .OrAssignExpr,
            .BoolOrExpr,
            .BoolAndExpr,
            .BitOrExpr,
            .BitXorExpr,
            .BitAndExpr,
            .EqualExpr,
            .NotEqualExpr,
            .LessThanExpr,
            .LessThanEqualExpr,
            .GreaterThanExpr,
            .GreaterThanEqualExpr,
            .ShlExpr,
            .ShrExpr,
            .AddExpr,
            .SubExpr,
            .MulExpr,
            .DivExpr,
            .ModExpr,
            => Expression.BinOp,

            else => @panic("TODO: Tag.Type()"),
        };
    }
};

pub const Declaration = struct {
    pub const FnProto = struct {
        name: []const u8,
        ty: Type,
        storageClass: enum {
            none,
            static,
            @"extern",
        },
        isInline: bool,
        definition: ?*FnDef,
    };

    pub const FnDef = struct {
        name: []const u8,
        ty: Type,
        storageClass: enum {
            none,
            static,
            @"extern",
        },
        isInline: bool,
        body: NodeIndex,
    };

    pub const Var = struct {
        name: []const u8,
        ty: Type,
        storageClass: enum {
            none,
            auto,
            static,
            @"extern",
            register,
        },
        isThreadlocal: bool,
        init: ?NodeIndex,
    };

    pub const Typedef = struct {
        name: []const u8,
        ty: Type,
    };

    pub const RecordForward = struct {
        name: []const u8,
        definition: ?*Record,
    };

    pub const Record = struct {
        // identifier or keyword_struct/union if anonymous
        nameToken: TokenIndex,
        fields: []Field,
    };

    pub const Field = struct {
        // identifier or keyword_struct/union if anonymous
        name: TokenIndex,
        ty: Type,
        // TODO bit field stuff
    };

    pub const EnumForward = struct {
        name: []const u8,
        definition: ?*Enum,
    };

    pub const Enum = struct {
        // identifier or keyword_enum if anonymous
        tagType: Type,
        nameToken: TokenIndex,
    };
};

pub const Statement = struct {
    pub const Labeled = struct {
        label: []const u8,
        stmt: NodeIndex,
    };

    pub const Compound = struct {
        stmts: NodeIndex,
    };

    pub const If = struct {
        cond: NodeIndex,
        then: NodeIndex,
        @"else": NodeIndex,
    };

    pub const Switch = struct { cond: NodeIndex, stmt: NodeIndex };

    pub const While = struct {
        cond: NodeIndex,
        stmt: NodeIndex,
    };

    pub const For = struct {
        init: NodeIndex,
        cond: NodeIndex,
        inc: NodeIndex,
        stmt: NodeIndex,
    };

    pub const Goto = struct {
        label: []const u8,
    };

    pub const Return = struct {
        stmt: NodeIndex,
    };
};

pub const Expression = struct {
    pub const Float = f32;
    pub const Double = f64;

    pub const String = struct {
        tokens: []TokenIndex,
    };

    // pub const Number = struct {
    //     ty: Type,

    // };

    pub const BinOp = struct {
        ty: Type,
        lhs: NodeIndex,
        rhs: NodeIndex,
    };

    pub const UnaryOp = struct {
        ty: Type,
        operand: NodeIndex,
    };

    pub const Conditional = struct {
        ty: Type,
        cond: NodeIndex,
        then: NodeIndex,
        @"else": NodeIndex,
    };
};
