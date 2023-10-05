const std = @import("std");
const Type = @import("Type.zig");
const Compilation = @import("../Basic/Compilation.zig");
const Source = @import("../Basic/Source.zig");
const Lexer = @import("../Lexer/Lexer.zig");
const TokenType = @import("../Basic/TokenType.zig").TokenType;
const AstTag = @import("AstTag.zig").Tag;

const AST = @This();

pub const TokenIndex = u32;
pub const NodeIndex = enum(u32) { none, _ };
pub const ValueMap = std.AutoHashMap(NodeIndex, u64);

comp: *Compilation,
arena: std.heap.ArenaAllocator,
generated: []const u8,
tokens: Token.List.Slice,
nodes: Node.List.Slice,
data: []const NodeIndex,
rootDecls: []const NodeIndex,
strings: []const u8,
valueMap: ValueMap,

pub fn deinit(tree: *AST) void {
    tree.comp.gpa.free(tree.rootDecls);
    tree.comp.gpa.free(tree.data);
    tree.comp.gpa.free(tree.strings);
    tree.nodes.deinit(tree.comp.gpa);
    tree.arena.deinit();
    tree.valueMap.deinit();
}

pub const Token = struct {
    id: TokenType,

    /// This location contains the actual token slice which might be generated.
    /// If it is generated then the next location will be the location of the concatenation.
    /// Any subsequent locations mark where the token was expanded from.
    loc: Source.Location,

    /// How many source locations do we track for each token.
    /// Must be at least 2.
    pub const List = std.MultiArrayList(Token);
};

pub const Range = struct { start: u32, end: u32 };
pub const Node = struct {
    tag: AstTag,
    type: Type = .{ .specifier = .Void },
    data: Data,

    pub const Data = union {
        Declaration: struct { name: TokenIndex, node: NodeIndex = .none },
        DeclarationRef: TokenIndex,

        range: Range,

        If3: struct { cond: NodeIndex, body: u32 },

        String: struct { index: u32, len: u32 },

        UnaryExpr: NodeIndex,

        BinaryExpr: struct { lhs: NodeIndex, rhs: NodeIndex },

        Member: struct { lhs: NodeIndex, name: TokenIndex },

        UnionInit: struct { fieldIndex: u32, node: NodeIndex },

        Int: u64,
        Float: f32,
        Double: f64,

        pub fn forDecl(data: Data, tree: AST) struct {
            decls: []const NodeIndex,
            cond: NodeIndex,
            incr: NodeIndex,
            body: NodeIndex,
        } {
            const items = tree.data[data.range.start..data.range.end];

            return .{
                .decls = items[0 .. items.len - 3],
                .cond = items[items.len - 3],
                .incr = items[items.len - 2],
                .body = items[items.len - 1],
            };
        }

        pub fn forStmt(data: Data, tree: AST) struct {
            init: NodeIndex,
            cond: NodeIndex,
            incr: NodeIndex,
            body: NodeIndex,
        } {
            const items = tree.data[data.If3.body..];

            return .{
                .init = items[0],
                .cond = items[1],
                .incr = items[2],
                .body = data.If3.cond,
            };
        }
    };

    pub const List = std.MultiArrayList(Node);
};

pub fn isLValue(nodes: Node.List.Slice, extra: []const NodeIndex, valueMap: ValueMap, node: NodeIndex) bool {
    switch (nodes.items(.tag)[@intFromEnum(node)]) {
        .CompoundLiteralExpr,
        .StringLiteralExpr,
        .MemberAccessPtrExpr,
        .ArrayAccessExpr,
        .DeclRefExpr,
        => return true,

        .DerefExpr => {
            const data = nodes.items(.data)[@intFromEnum(node)];
            return !nodes.items(.type)[@intFromEnum(data.UnaryExpr)].isFunc();
        },

        .MemberAccessExpr => {
            const data = nodes.items(.data)[@intFromEnum(node)];
            return isLValue(nodes, extra, valueMap, data.Member.lhs);
        },

        .ParenExpr => {
            const data = nodes.items(.data)[@intFromEnum(node)];
            return isLValue(nodes, extra, valueMap, data.UnaryExpr);
        },

        .BuiltinChooseExpr => {
            const data = nodes.items(.data)[@intFromEnum(node)];
            if (valueMap.get(data.If3.cond)) |val| {
                const offset = @intFromBool(val == 0);
                return isLValue(nodes, extra, valueMap, extra[data.If3.body + offset]);
            }

            return false;
        },

        else => return false,
    }
}

pub fn dumpString(bytes: []const u8, tag: AstTag, writer: anytype) !void {
    switch (tag) {
        .StringLiteralExpr => try writer.print("\"{}\"", .{std.zig.fmtEscapes(bytes)}),
        else => unreachable,
    }
}

pub fn tokSlice(tree: AST, index: TokenIndex) []const u8 {
    if (tree.tokens.items(.id)[index].lexeMe()) |some|
        return some;

    const loc = tree.tokens.items(.loc)[index];

    var lexer = Lexer{
        .buffer = if (loc.id == .generated)
            tree.generated
        else
            tree.comp.getSource(loc.id).buffer,
        .comp = tree.comp,
        .index = loc.byteOffset,
        .source = .generated,
    };

    const token = lexer.next();
    return lexer.buffer[token.start..token.end];
}

pub fn dump(tree: AST, writer: anytype) @TypeOf(writer).Error!void {
    for (tree.rootDecls) |i| {
        try tree.dumpNode(i, 0, writer);
        try writer.writeByte('\n');
    }
}

fn dumpNode(tree: AST, node: NodeIndex, level: u32, w: anytype) @TypeOf(w).Error!void {
    const delta = 2;
    const half = delta / 2;
    const win = @import("builtin").os.tag == .windows;
    const TYPE = if (win) "" else "\x1b[35;1m";
    const TAG = if (win) "" else "\x1b[36;1m";
    const IMPLICIT = if (win) "" else "\x1b[34;1m";
    const NAME = if (win) "" else "\x1b[91;1m";
    const LITERAL = if (win) "" else "\x1b[32;1m";
    const ATTRIBUTE = if (win) "" else "\x1b[93;1m";
    const RESET = if (win) "" else "\x1b[0m";

    std.debug.assert(node != .none);

    const tag = tree.nodes.items(.tag)[@intFromEnum(node)];
    const data = tree.nodes.items(.data)[@intFromEnum(node)];
    const ty = tree.nodes.items(.type)[@intFromEnum(node)];

    try w.writeByteNTimes(' ', level);
    if (tag.isImplicit()) {
        try w.print(IMPLICIT ++ "{s}: " ++ TYPE ++ "'", .{@tagName(tag)});
    } else {
        try w.print(TAG ++ "{s}: " ++ TYPE ++ "'", .{@tagName(tag)});
    }

    try ty.dump(w);
    try w.writeByte('\'');

    if (isLValue(tree.nodes, tree.data, tree.valueMap, node))
        try w.writeAll(ATTRIBUTE ++ " lvalue");

    if (tree.valueMap.get(node)) |val| {
        if (ty.isUnsignedInt(tree.comp))
            try w.print(LITERAL ++ " (value: {d})" ++ RESET, .{val})
        else
            try w.print(LITERAL ++ " (value: {d})" ++ RESET, .{@as(i64, @bitCast(val))});
    }

    try w.writeAll("\n" ++ RESET);

    switch (tag) {
        .Invalid => unreachable,

        .StaticAssert => {
            try w.writeByteNTimes(' ', level + 1);
            try w.writeAll("condition:\n");
            try tree.dumpNode(data.BinaryExpr.lhs, level + delta, w);

            if (data.BinaryExpr.rhs != .none) {
                try w.writeByteNTimes(' ', level + 1);
                try w.writeAll("diagnostic:\n");
                try tree.dumpNode(data.BinaryExpr.rhs, level + delta, w);
            }
        },

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
            try w.print("name: " ++ NAME ++ "{s}\n" ++ RESET, .{tree.tokSlice(data.Declaration.name)});
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
            try w.print("name: " ++ NAME ++ "{s}\n" ++ RESET, .{tree.tokSlice(data.Declaration.name)});
            try w.writeByteNTimes(' ', level + half);
            try w.writeAll("body:\n");
            try tree.dumpNode(data.Declaration.node, level + delta, w);
        },

        .CompoundStmt,
        .ArrayInitExpr,
        .StructInitExpr,
        .EnumDecl,
        .StructDecl,
        .UnionDecl,
        => {
            for (tree.data[data.range.start..data.range.end], 0..) |stmt, i| {
                if (i != 0)
                    try w.writeByte('\n');
                try tree.dumpNode(stmt, level + delta, w);
            }
        },

        .IndirectRecordFieldDecl => {},

        .CompoundStmtTwo,
        .ArrayInitExprTwo,
        .StructInitExprTwo,
        .EnumDeclTwo,
        .StructDeclTwo,
        .UnionDeclTwo,
        => {
            if (data.BinaryExpr.lhs != .none) try tree.dumpNode(data.BinaryExpr.lhs, level + delta, w);
            if (data.BinaryExpr.rhs != .none) try tree.dumpNode(data.BinaryExpr.rhs, level + delta, w);
        },

        .UnionInitExpr => {
            try w.writeByteNTimes(' ', level + half);
            try w.print("field index: " ++ LITERAL ++ "{d}\n" ++ RESET, .{data.UnionInit.fieldIndex});
            if (data.UnionInit.node != .none) {
                try tree.dumpNode(data.UnionInit.node, level + delta, w);
            }
        },

        .CompoundLiteralExpr => {
            try tree.dumpNode(data.UnaryExpr, level + half, w);
        },

        .LabeledStmt => {
            try w.writeByteNTimes(' ', level + half);
            try w.print("label: " ++ LITERAL ++ "{s}\n" ++ RESET, .{tree.tokSlice(data.Declaration.name)});
            if (data.Declaration.node != .none) {
                try w.writeByteNTimes(' ', level + half);
                try w.writeAll("stmt:\n");
                try tree.dumpNode(data.Declaration.node, level + delta, w);
            }
        },

        .CondExpr, .IfThenElseStmt, .BuiltinChooseExpr => {
            try w.writeByteNTimes(' ', level + half);
            try w.writeAll("cond:\n");
            try tree.dumpNode(data.If3.cond, level + delta, w);

            try w.writeByteNTimes(' ', level + half);
            try w.writeAll("then:\n");
            try tree.dumpNode(tree.data[data.If3.body], level + delta, w);

            try w.writeByteNTimes(' ', level + half);
            try w.writeAll("else:\n");
            try tree.dumpNode(tree.data[data.If3.body + 1], level + delta, w);
        },

        .IfElseStmt => {
            try w.writeByteNTimes(' ', level + half);
            try w.writeAll("cond:\n");
            try tree.dumpNode(data.BinaryExpr.lhs, level + delta, w);

            try w.writeByteNTimes(' ', level + half);
            try w.writeAll("else:\n");
            try tree.dumpNode(data.BinaryExpr.rhs, level + delta, w);
        },

        .IfThenStmt => {
            try w.writeByteNTimes(' ', level + half);
            try w.writeAll("cond:\n");
            try tree.dumpNode(data.BinaryExpr.lhs, level + delta, w);

            if (data.BinaryExpr.rhs != .none) {
                try w.writeByteNTimes(' ', level + half);
                try w.writeAll("then:\n");
                try tree.dumpNode(data.BinaryExpr.rhs, level + delta, w);
            }
        },

        .GotoStmt => {
            try w.writeByteNTimes(' ', level + half);
            try w.print("label: " ++ LITERAL ++ "{s}\n" ++ RESET, .{tree.tokSlice(data.DeclarationRef)});
        },

        .ContinueStmt,
        .BreakStmt,
        .ImplicitReturn,
        => {},

        .ReturnStmt => {
            if (data.UnaryExpr != .none) {
                try w.writeByteNTimes(' ', level + half);
                try w.writeAll("expr:\n");
                try tree.dumpNode(data.UnaryExpr, level + delta, w);
            }
        },

        .ForDeclStmt => {
            const forDecl = data.forDecl(tree);

            try w.writeByteNTimes(' ', level + half);
            try w.writeAll("decl:\n");
            for (forDecl.decls) |decl| {
                try tree.dumpNode(decl, level + delta, w);
                try w.writeByte('\n');
            }

            if (forDecl.cond != .none) {
                try w.writeByteNTimes(' ', level + half);
                try w.writeAll("cond:\n");
                try tree.dumpNode(forDecl.cond, level + delta, w);
            }

            if (forDecl.incr != .none) {
                try w.writeByteNTimes(' ', level + half);
                try w.writeAll("incr:\n");
                try tree.dumpNode(forDecl.incr, level + delta, w);
            }

            if (forDecl.body != .none) {
                try w.writeByteNTimes(' ', level + half);
                try w.writeAll("body:\n");
                try tree.dumpNode(forDecl.body, level + delta, w);
            }
        },

        .ForEverStmt => {
            if (data.UnaryExpr != .none) {
                try w.writeByteNTimes(' ', level + half);
                try w.writeAll("body:\n");
                try tree.dumpNode(data.UnaryExpr, level + delta, w);
            }
        },

        .ForStmt => {
            const forStmt = data.forStmt(tree);

            if (forStmt.init != .none) {
                try w.writeByteNTimes(' ', level + half);
                try w.writeAll("init:\n");
                try tree.dumpNode(forStmt.init, level + delta, w);
            }

            if (forStmt.cond != .none) {
                try w.writeByteNTimes(' ', level + half);
                try w.writeAll("cond:\n");
                try tree.dumpNode(forStmt.cond, level + delta, w);
            }

            if (forStmt.incr != .none) {
                try w.writeByteNTimes(' ', level + half);
                try w.writeAll("incr:\n");
                try tree.dumpNode(forStmt.incr, level + delta, w);
            }

            if (forStmt.body != .none) {
                try w.writeByteNTimes(' ', level + half);
                try w.writeAll("body:\n");
                try tree.dumpNode(forStmt.body, level + delta, w);
            }
        },

        .SwitchStmt, .WhileStmt, .DoWhileStmt => {
            try w.writeByteNTimes(' ', level + half);
            try w.writeAll("cond:\n");
            try tree.dumpNode(data.BinaryExpr.lhs, level + delta, w);

            if (data.BinaryExpr.rhs != .none) {
                try w.writeByteNTimes(' ', level + half);
                try w.writeAll("body:\n");
                try tree.dumpNode(data.BinaryExpr.rhs, level + delta, w);
            }
        },

        .CaseStmt => {
            try w.writeByteNTimes(' ', level + half);
            try w.writeAll("value:\n");
            try tree.dumpNode(data.BinaryExpr.lhs, level + delta, w);

            if (data.BinaryExpr.rhs != .none) {
                try w.writeByteNTimes(' ', level + half);
                try w.writeAll("stmt:\n");
                try tree.dumpNode(data.BinaryExpr.rhs, level + delta, w);
            }
        },

        .DefaultStmt => {
            if (data.UnaryExpr != .none) {
                try w.writeByteNTimes(' ', level + half);
                try w.writeAll("stmt:\n");
                try tree.dumpNode(data.UnaryExpr, level + delta, w);
            }
        },

        .Var,
        .ExternVar,
        .StaticVar,
        .ThreadlocalVar,
        .ThreadlocalExternVar,
        .ThreadlocalStaticVar,
        .TypeDef,
        => {
            try w.writeByteNTimes(' ', level + half);
            try w.print("name: " ++ LITERAL ++ "{s}\n" ++ RESET, .{tree.tokSlice(data.Declaration.name)});

            if (data.Declaration.node != .none) {
                try w.writeByteNTimes(' ', level + half);
                try w.writeAll("init:\n");
                try tree.dumpNode(data.Declaration.node, level + delta, w);
            }
        },

        .EnumFieldDecl => {
            try w.writeByteNTimes(' ', level + half);
            try w.print("name: " ++ NAME ++ "{s}\n" ++ RESET, .{tree.tokSlice(data.Declaration.name)});
            if (data.Declaration.node != .none) {
                try w.writeByteNTimes(' ', level + half);
                try w.writeAll("value:\n");
                try tree.dumpNode(data.Declaration.node, level + delta, w);
            }
        },

        .RecordFieldDecl => {
            if (data.Declaration.name != 0) {
                try w.writeByteNTimes(' ', level + half);
                try w.print("name: " ++ NAME ++ "{s}\n" ++ RESET, .{tree.tokSlice(data.Declaration.name)});
            }

            if (data.Declaration.node != .none) {
                try w.writeByteNTimes(' ', level + half);
                try w.writeAll("bits:\n");
                try tree.dumpNode(data.Declaration.node, level + delta, w);
            }
        },

        .StringLiteralExpr => {
            try w.writeByteNTimes(' ', level + half);
            try w.writeAll("data: " ++ LITERAL);
            try dumpString(tree.strings[data.String.index..][0..data.String.len], tag, w);
            try w.writeAll("\n" ++ RESET);
        },

        .CallExpr => {
            try w.writeByteNTimes(' ', level + half);
            try w.writeAll("lhs:\n");
            try tree.dumpNode(tree.data[data.range.start], level + delta, w);

            try w.writeByteNTimes(' ', level + half);
            try w.writeAll("args:\n");
            for (tree.data[data.range.start + 1 .. data.range.end]) |arg| try tree.dumpNode(arg, level + delta, w);
        },

        .CallExprOne => {
            try w.writeByteNTimes(' ', level + half);
            try w.writeAll("lhs:\n");
            try tree.dumpNode(data.BinaryExpr.lhs, level + delta, w);

            const arg = data.BinaryExpr.rhs;
            if (arg != .none) {
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
        .BitAndAssignExpr,
        .BitXorAssignExpr,
        .BitOrAssignExpr,
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
            try tree.dumpNode(data.BinaryExpr.lhs, level + delta, w);
            try w.writeByteNTimes(' ', level + 1);
            try w.writeAll("rhs:\n");
            try tree.dumpNode(data.BinaryExpr.rhs, level + delta, w);
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
        .ParenExpr,
        => {
            try w.writeByteNTimes(' ', level + 1);
            try w.writeAll("operand:\n");
            try tree.dumpNode(data.UnaryExpr, level + delta, w);
        },

        .DeclRefExpr => {
            try w.writeByteNTimes(' ', level + 1);
            try w.print("name: " ++ NAME ++ "{s}\n" ++ RESET, .{tree.tokSlice(data.DeclarationRef)});
        },

        .EnumerationRef => {
            try w.writeByteNTimes(' ', level + 1);
            try w.print("name: " ++ NAME ++ "{s}\n" ++ RESET, .{tree.tokSlice(data.DeclarationRef)});
        },

        .IntLiteral => {
            try w.writeByteNTimes(' ', level + 1);
            try w.print("value: " ++ LITERAL ++ "{d}\n" ++ RESET, .{data.Int});
        },

        .FloatLiteral => {
            try w.writeByteNTimes(' ', level + 1);
            try w.print("value: " ++ LITERAL ++ "{d}\n" ++ RESET, .{data.Float});
        },

        .DoubleLiteral => {
            try w.writeByteNTimes(' ', level + 1);
            try w.print("value: " ++ LITERAL ++ "{d}\n" ++ RESET, .{data.Double});
        },

        .MemberAccessExpr, .MemberAccessPtrExpr => {
            if (data.Member.lhs != .none) {
                try w.writeByteNTimes(' ', level + 1);
                try w.writeAll("lhs:\n");
                try tree.dumpNode(data.Member.lhs, level + delta, w);
            }
            try w.writeByteNTimes(' ', level + 1);
            try w.print("name: " ++ NAME ++ "{s}\n" ++ RESET, .{tree.tokSlice(data.Member.name)});
        },

        .ArrayAccessExpr => {
            if (data.BinaryExpr.lhs != .none) {
                try w.writeByteNTimes(' ', level + 1);
                try w.writeAll("lhs:\n");
                try tree.dumpNode(data.BinaryExpr.lhs, level + delta, w);
            }
            try w.writeByteNTimes(' ', level + 1);
            try w.writeAll("index:\n");
            try tree.dumpNode(data.BinaryExpr.rhs, level + delta, w);
        },

        .SizeOfExpr,
        .AlignOfExpr,
        => {
            if (data.UnaryExpr != .none) {
                try w.writeByteNTimes(' ', level + 1);
                try w.writeAll("expr:\n");
                try tree.dumpNode(data.UnaryExpr, level + delta, w);
            }
        },

        .GenericExprOne => {
            try w.writeByteNTimes(' ', level + 1);
            try w.writeAll("controlling:\n");
            try tree.dumpNode(data.BinaryExpr.lhs, level + delta, w);
            try w.writeByteNTimes(' ', level + 1);
            try w.writeAll("chosen:\n");
            try tree.dumpNode(data.BinaryExpr.rhs, level + delta, w);
        },

        .GenericExpr => {
            const nodes = tree.data[data.range.start..data.range.end];
            try w.writeByteNTimes(' ', level + 1);
            try w.writeAll("controlling:\n");
            try tree.dumpNode(nodes[0], level + delta, w);
            try w.writeByteNTimes(' ', level + 1);
            try w.writeAll("chosen:\n");
            try tree.dumpNode(nodes[1], level + delta, w);
            try w.writeByteNTimes(' ', level + 1);
            try w.writeAll("rest:\n");
            for (nodes[2..]) |expr| {
                try tree.dumpNode(expr, level + delta, w);
            }
        },

        .GenericAssociationExpr, .GenericDefaultExpr => {
            try tree.dumpNode(data.UnaryExpr, level + delta, w);
        },

        .ArrayToPointer,
        .LValueToRValue,
        .FunctionToPointer,
        .PointerToBool,
        .PointerToInt,
        .BoolToInt,
        .BoolToFloat,
        .BoolToPointer,
        .IntToBool,
        .IntToFloat,
        .IntToPointer,
        .FloatToBool,
        .FloatToInt,
        .IntCast,
        .FloatCast,
        .ToVoid,
        .QualCast,
        .NullToPointer,
        => {
            try tree.dumpNode(data.UnaryExpr, level + delta, w);
        },

        .ArrayFillerExpr => {
            try w.writeByteNTimes(' ', level + 1);
            try w.print("count: " ++ LITERAL ++ "{d}\n" ++ RESET, .{data.Int});
        },

        .DefaultInitExpr => {},
    }
}
