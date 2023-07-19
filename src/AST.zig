const std = @import("std");
const Type = @import("Type.zig");
const Token = @import("Token.zig").Token;
const Compilation = @import("Compilation.zig");
const AstTag = @import("AstTag.zig").Tag;

const AST = @This();

pub const TokenIndex = u32;
pub const NodeIndex = u32;

comp: *Compilation,
arena: std.heap.ArenaAllocator,
generated: []const u8,
tokens: []const Token,
nodes: Node.List.Slice,
rootDecls: []const NodeIndex,

pub fn deinit(tree: *AST) void {
    tree.comp.gpa.free(tree.rootDecls);
    tree.nodes.deinit(tree.comp.gpa);
    tree.arena.deinit();
}

pub const Node = struct {
    tag: AstTag,
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

pub fn dump(tree: AST) void {
    for (tree.rootDecls) |i|
        tree.dumpDeclaration(i, 0);
}

fn dumpDeclaration(tree: AST, node: NodeIndex, level: u32) void {
    std.debug.print("{s: >[1]} ", .{ @tagName(tree.nodes.items(.tag)[node]), level });
    tree.nodes.items(.type)[node].dump(tree);
    std.debug.print("\n", .{});
}
