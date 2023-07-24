const std = @import("std");
const Type = @import("Type.zig");
const Token = @import("../Lexer/Token.zig").Token;
const Compilation = @import("../Basic/Compilation.zig");
const AstTag = @import("AstTag.zig").Tag;

const AST = @This();

pub const TokenIndex = u32;
pub const NodeIndex = u32;

comp: *Compilation,
arena: std.heap.ArenaAllocator,
generated: []const u8,
tokens: []const Token,
nodes: Node.List.Slice,
data: []const NodeIndex,
rootDecls: []const NodeIndex,

pub fn deinit(tree: *AST) void {
    tree.comp.gpa.free(tree.rootDecls);
    tree.comp.gpa.free(tree.data);
    tree.nodes.deinit(tree.comp.gpa);
    tree.arena.deinit();
}

pub const Node = struct {
    tag: AstTag,
    type: Type,
    first: NodeIndex = 0,
    second: NodeIndex = 0,

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

pub const Declaration = struct {
    pub const FnProto = struct {
        name: []const u8,
        ty: Type,
        isStatic: bool,
        isInline: bool,
        definition: ?*FnDef,
    };

    pub const FnDef = struct {
        name: []const u8,
        ty: Type,
        isStatic: bool,
        isInline: bool,
        body: NodeIndex,
    };

    pub const Param = struct {
        nameToken: TokenIndex,
        ty: Type,
    };

    pub const Var = struct {
        name: []const u8,
        ty: Type,
        storageClass: enum {
            none,
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

pub fn dump(tree: AST, writer: anytype) @TypeOf(writer).Error!void {
    for (tree.rootDecls) |i| {
        try tree.dumpNode(i, 0, writer);
        try writer.writeByte('\n');
    }
}

fn dumpNode(tree: AST, node: NodeIndex, level: u32, w: anytype) @TypeOf(w).Error!void {
    const delta = 2;
    const half = delta / 2;
    const PURPLE = "\x1b[35;1m";
    const CYAN = "\x1b[36;1m";
    const GREEN = "\x1b[32;1m";
    const RESET = "\x1b[0m";

    const tag = tree.nodes.items(.tag)[node];
    try w.writeByteNTimes(' ', level);
    try w.print(CYAN ++ "{s}: " ++ PURPLE ++ "'", .{@tagName(tag)});
    try tree.nodes.items(.type)[node].dump(tree, w);
    try w.writeAll("'\n" ++ RESET);

    switch (tag) {
        .Invalid => unreachable,

        .FnProto,
        .StaticFnProto,
        .InlineFnProto,
        .InlineStaticFnProto,
        .NoreturnFnProto,
        .NoreturnStaticFnProto,
        .NoreturnInlineFnProto,
        .NoreturnInlineStaticFnProto,
        => {
            try w.writeByteNTimes(' ', level + half);
            try w.print("name: " ++ GREEN ++ "{s}\n" ++ RESET, .{tree.tokSlice(tree.nodes.items(.first)[node])});
        },

        .FnDef,
        .StaticFnDef,
        .InlineFnDef,
        .InlineStaticFnDef,
        .NoreturnFnDef,
        .NoreturnStaticFnDef,
        .NoreturnInlineFnDef,
        .NoreturnInlineStaticFnDef,
        => {
            try w.writeByteNTimes(' ', level + half);
            try w.print("name: " ++ GREEN ++ "{s}\n" ++ RESET, .{tree.tokSlice(tree.nodes.items(.first)[node])});
            try w.writeByteNTimes(' ', level + half);
            try w.writeAll("body:\n");
            try tree.dumpNode(tree.nodes.items(.second)[node], level + delta, w);
        },

        .CompoundStmt => {
            const start = tree.nodes.items(.first)[node];
            const end = tree.nodes.items(.second)[node];
            for (tree.data[start..end]) |stmt| try tree.dumpNode(stmt, level + delta, w);
        },

        .CompoundStmtTwo => {
            const first = tree.nodes.items(.first)[node];
            if (first != 0) try tree.dumpNode(first, level + delta, w);
            const second = tree.nodes.items(.second)[node];
            if (second != 0) try tree.dumpNode(second, level + delta, w);
        },

        .Var,
        .ExternVar,
        .StaticVar,
        .RegisterVar,
        .ThreadlocalVar,
        .ThreadlocalExternVar,
        .ThreadlocalStaticVar,
        .TypeDef,
        => {
            try w.writeByteNTimes(' ', level + half);
            try w.print("name: " ++ GREEN ++ "\"{s}\"\n" ++ RESET, .{tree.tokSlice(tree.nodes.items(.first)[node])});
            const init = tree.nodes.items(.second)[node];
            if (init != 0) {
                try w.writeByteNTimes(' ', level + half);
                try w.writeAll("init:\n");
                try tree.dumpNode(init, level + delta, w);
            }
        },

        .StringLiteralExpr => {
            const start = tree.nodes.items(.first)[node];
            const ptr: [*]const u8 = @ptrFromInt(@as(usize, @bitCast(tree.data[start..][0..2].*)));
            const len = tree.nodes.items(.second)[node];

            try w.writeByteNTimes(' ', level + half);
            try w.print("data: " ++ GREEN ++ "{s}\n" ++ RESET, .{ptr[0 .. len - 1]});
        },

        .CallExpr => {
            const start = tree.nodes.items(.first)[node];
            const end = tree.nodes.items(.second)[node];
            try w.writeByteNTimes(' ', level + half);
            try w.writeAll("lhs:\n");
            try tree.dumpNode(tree.data[start], level + delta, w);

            try w.writeByteNTimes(' ', level + half);
            try w.writeAll("args:\n");
            for (tree.data[start + 1 .. end]) |stmt| try tree.dumpNode(stmt, level + delta, w);
        },

        .CallExprOne => {
            try w.writeByteNTimes(' ', level + half);
            try w.writeAll("lhs:\n");
            try tree.dumpNode(tree.nodes.items(.first)[node], level + delta, w);
            const arg = tree.nodes.items(.second)[node];
            if (arg != 0) {
                try w.writeByteNTimes(' ', level + half);
                try w.writeAll("arg:\n");
                try tree.dumpNode(arg, level + delta, w);
            }
        },

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
        => {
            try w.writeByteNTimes(' ', level + 1);
            try w.writeAll("lhs:\n");
            try tree.dumpNode(tree.nodes.items(.first)[node], level + delta, w);
            try w.writeByteNTimes(' ', level + 1);
            try w.writeAll("rhs:\n");
            try tree.dumpNode(tree.nodes.items(.second)[node], level + delta, w);
        },

        .CastExpr,
        .AddrOfExpr,
        .DerefExpr,
        .PlusExpr,
        .NegateExpr,
        .BitNotExpr,
        .BoolNotExpr,
        .PreIncExpr,
        .PreDecExpr,
        .PostIncExpr,
        .PostDecExpr,
        .ArrayToPointer,
        => {
            try w.writeByteNTimes(' ', level + 1);
            try w.writeAll("operand:\n");
            try tree.dumpNode(tree.nodes.items(.first)[node], level + delta, w);
        },

        .DeclRefExpr => {
            try w.writeByteNTimes(' ', level + 1);
            try w.print("name: " ++ GREEN ++ "{s}\n" ++ RESET, .{tree.tokSlice(tree.nodes.items(.first)[node])});
        },

        else => {},
    }
}
