const std = @import("std");
const Type = @import("Type.zig");
const Compilation = @import("../Basic/Compilation.zig");
const Source = @import("../Basic/Source.zig");
const Lexer = @import("../Lexer/Lexer.zig");
const TokenType = @import("../Basic/TokenType.zig").TokenType;
const AstTag = @import("AstTag.zig").Tag;
const Attribute = @import("../Lexer/Attribute.zig");
const Value = @import("Value.zig");

const AST = @This();

pub const TokenIndex = u32;
pub const NodeIndex = enum(u32) { none, _ };
pub const ValueMap = std.AutoHashMap(NodeIndex, Value);

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
    /// If it is generated then there is guaranteed to be at least one expansion location.
    loc: Source.Location,
    expansionLocs: ?[*]Source.Location = null,

    pub fn expansionSlice(tok: Token) []const Source.Location {
        const locs = tok.expansionLocs orelse return &[0]Source.Location{};
        var i: usize = 0;
        while (locs[i].id != .unused) : (i += 1) {}
        return locs[0..i];
    }

    pub fn addExpansionLocation(tok: *Token, gpa: std.mem.Allocator, new: []const Source.Location) !void {
        if (new.len == 0 or tok.id == .WhiteSpace) return;
        var list = std.ArrayList(Source.Location).init(gpa);
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
        try list.ensureTotalCapacity(wantedLen);

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

    pub fn dupe(tok: Token, gpa: std.mem.Allocator) !Token {
        var copy = tok;
        copy.expansionLocs = null;
        try copy.addExpansionLocation(gpa, tok.expansionSlice());
        return copy;
    }

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
        decl: struct { name: TokenIndex, node: NodeIndex = .none },
        declRef: TokenIndex,
        range: Range,

        if3: struct { cond: NodeIndex, body: u32 },

        unExpr: NodeIndex,
        binExpr: struct { lhs: NodeIndex, rhs: NodeIndex },

        member: struct { lhs: NodeIndex, index: u32 },
        unionInit: struct { fieldIndex: u32, node: NodeIndex },

        cast: struct { operand: NodeIndex, kind: CastKind },

        int: u64,

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
            const items = tree.data[data.if3.body..];

            return .{
                .init = items[0],
                .cond = items[1],
                .incr = items[2],
                .body = data.if3.cond,
            };
        }
    };

    pub const List = std.MultiArrayList(Node);
};

pub const CastKind = enum(u8) {
    /// Does nothing except possibly add qualifiers
    NoOP,
    /// Interpret one bit pattern as another. Used for operands which have the same
    /// size and unrelated types, e.g. casting one pointer type to another
    Bitcast,
    /// Convert T[] to T *
    ArrayToPointer,
    /// Converts an lvalue to an rvalue
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
    /// Convert an integer to a floating
    IntToFloat,
    /// Convert an integer type to a pointer type
    IntToPointer,
    /// Convert a floating type to a _Bool
    FloatToBool,
    /// Convert a floating type to an integer
    FloatToInt,
    /// Convert one integer type to another
    IntCast,
    /// Convert one floating type to another
    FloatCast,
    /// Convert type to void
    ToVoid,
    /// Convert a literal 0 to a null pointer
    NullToPointer,
    /// GNU cast-to-union extension
    UnionCast,

    pub fn fromExplicitCast(to: Type, from: Type, comp: *Compilation) CastKind {
        if (to.eql(from, comp, false)) return .NoOP;
        if (to.is(.Bool)) {
            if (from.isPointer()) return .PointerToBool;
            if (from.isInt()) return .IntToBool;
            if (from.isFloat()) return .FloatToBool;
        } else if (to.isInt()) {
            if (from.is(.Bool)) return .BoolToInt;
            if (from.isInt()) return .IntCast;
            if (from.isPointer()) return .PointerToInt;
            if (from.isFloat()) return .FloatToInt;
        } else if (to.isPointer()) {
            if (from.isArray()) return .ArrayToPointer;
            if (from.isPointer()) return .Bitcast;
            if (from.isFunc()) return .FunctionToPointer;
            if (from.is(.Bool)) return .BoolToPointer;
            if (from.isInt()) return .IntToPointer;
        } else if (to.isFloat()) {
            if (from.is(.Bool)) return .BoolToFloat;
            if (from.isInt()) return .IntToFloat;
            if (from.isFloat()) return .FloatCast;
        }
        unreachable;
    }
};

pub fn isLValue(nodes: Node.List.Slice, extra: []const NodeIndex, valueMap: ValueMap, node: NodeIndex) bool {
    var isConst: bool = undefined;
    return isLValueExtra(nodes, extra, valueMap, node, &isConst);
}

pub fn isLValueExtra(
    nodes: Node.List.Slice,
    extra: []const NodeIndex,
    valueMap: ValueMap,
    node: NodeIndex,
    isConst: *bool,
) bool {
    isConst.* = false;
    switch (nodes.items(.tag)[@intFromEnum(node)]) {
        .CompoundLiteralExpr => {
            isConst.* = nodes.items(.type)[@intFromEnum(node)].isConst();
            return true;
        },

        .StringLiteralExpr => return true,
        .MemberAccessPtrExpr => {
            const lhsExpr = nodes.items(.data)[@intFromEnum(node)].member.lhs;
            const ptrExpr = nodes.items(.type)[@intFromEnum(lhsExpr)];
            if (ptrExpr.isPointer())
                isConst.* = ptrExpr.getElemType().isConst();

            return true;
        },

        .ArrayAccessExpr => {
            const lhsExpr = nodes.items(.data)[@intFromEnum(node)].binExpr.lhs;
            if (lhsExpr != .none) {
                const arrayType = nodes.items(.type)[@intFromEnum(lhsExpr)];
                if (arrayType.isPointer() or arrayType.isArray())
                    isConst.* = arrayType.getElemType().isConst();
            }
            return true;
        },

        .DeclRefExpr => {
            const declType = nodes.items(.type)[@intFromEnum(node)];
            isConst.* = declType.isConst();
            return true;
        },

        .DerefExpr => {
            const data = nodes.items(.data)[@intFromEnum(node)];
            const operandType = nodes.items(.type)[@intFromEnum(data.unExpr)];
            if (operandType.isFunc())
                return false;
            if (operandType.isPointer() or operandType.isArray())
                isConst.* = operandType.getElemType().isConst();
            return true;
        },

        .MemberAccessExpr => {
            const data = nodes.items(.data)[@intFromEnum(node)];
            return isLValueExtra(nodes, extra, valueMap, data.member.lhs, isConst);
        },

        .ParenExpr => {
            const data = nodes.items(.data)[@intFromEnum(node)];
            return isLValueExtra(nodes, extra, valueMap, data.unExpr, isConst);
        },

        .BuiltinChooseExpr => {
            const data = nodes.items(.data)[@intFromEnum(node)];
            if (valueMap.get(data.if3.cond)) |val| {
                const offset = @intFromBool(val.isZero());
                return isLValueExtra(nodes, extra, valueMap, extra[data.if3.body + offset], isConst);
            }

            return false;
        },

        else => return false,
    }
}

pub fn dumpString(bytes: []const u8, tag: AstTag, writer: anytype) !void {
    switch (tag) {
        .StringLiteralExpr => try writer.print("\"{}\"", .{std.zig.fmtEscapes(bytes[0 .. bytes.len - 1])}),
        else => unreachable,
    }
}

pub fn getTokenSlice(tree: AST, index: TokenIndex) []const u8 {
    if (tree.tokens.items(.id)[index].getTokenText()) |some|
        return some;

    const loc = tree.tokens.items(.loc)[index];
    var lexer = Lexer{
        .buffer = tree.comp.getSource(loc.id).buffer,
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

fn dumpAttribute(attr: Attribute, writer: anytype) !void {
    inline for (std.meta.fields(Attribute.Tag)) |e| {
        if (e.value == @intFromEnum(attr.tag)) {
            try writer.print("{}\n", .{@field(attr.args, e.name)});
            return;
        }
    }
}

fn dumpNode(tree: AST, node: NodeIndex, level: u32, w: anytype) @TypeOf(w).Error!void {
    const delta = 2;
    const half = delta / 2;
    const util = @import("../Basic/Util.zig");
    const TYPE = util.Color.purple;
    const TAG = util.Color.cyan;
    const IMPLICIT = util.Color.blue;
    const NAME = util.Color.red;
    const LITERAL = util.Color.green;
    const ATTRIBUTE = util.Color.yellow;

    std.debug.assert(node != .none);

    const tag = tree.nodes.items(.tag)[@intFromEnum(node)];
    const data = tree.nodes.items(.data)[@intFromEnum(node)];
    const ty = tree.nodes.items(.type)[@intFromEnum(node)];
    try w.writeByteNTimes(' ', level);

    util.setColor(if (tag.isImplicit()) IMPLICIT else TAG, w);

    try w.print("{s}: ", .{@tagName(tag)});
    if (tag == .ImplicitCast or tag == .ExplicitCast) {
        util.setColor(.white, w);
        try w.print("({s}) ", .{@tagName(data.cast.kind)});
    }
    util.setColor(TYPE, w);
    try w.writeByte('\'');
    try ty.dump(w);
    try w.writeByte('\'');

    if (isLValue(tree.nodes, tree.data, tree.valueMap, node)) {
        util.setColor(ATTRIBUTE, w);
        try w.writeAll(" lvalue");
    }

    if (tree.valueMap.get(node)) |val| {
        util.setColor(LITERAL, w);
        try w.writeAll(" (value: ");
        try val.dump(ty, tree.comp, w);
        try w.writeByte(')');
    }

    try w.writeAll("\n");
    util.setColor(.reset, w);

    if (ty.specifier == .Attributed) {
        util.setColor(ATTRIBUTE, w);
        for (ty.data.attributed.attributes) |attr| {
            try w.writeByteNTimes(' ', level + half);
            try w.print("attr: {s}\n", .{@tagName(attr.tag)});
            try dumpAttribute(attr, w);
        }
        util.setColor(.reset, w);
    }

    switch (tag) {
        .Invalid => unreachable,
        .StaticAssert => {
            try w.writeByteNTimes(' ', level + 1);
            try w.writeAll("condition:\n");
            try tree.dumpNode(data.binExpr.lhs, level + delta, w);

            if (data.binExpr.rhs != .none) {
                try w.writeByteNTimes(' ', level + 1);
                try w.writeAll("diagnostic:\n");
                try tree.dumpNode(data.binExpr.rhs, level + delta, w);
            }
        },

        .FnProto,
        .StaticFnProto,
        .InlineFnProto,
        .InlineStaticFnProto,
        => {
            try w.writeByteNTimes(' ', level + half);
            try w.writeAll("name: ");
            util.setColor(NAME, w);
            try w.print("{s}\n", .{tree.getTokenSlice(data.decl.name)});
            util.setColor(.reset, w);
        },

        .FnDef,
        .StaticFnDef,
        .InlineFnDef,
        .InlineStaticFnDef,
        => {
            try w.writeByteNTimes(' ', level + half);
            try w.writeAll("name: ");
            util.setColor(NAME, w);
            try w.print("{s}\n", .{tree.getTokenSlice(data.decl.name)});
            util.setColor(.reset, w);
            try w.writeByteNTimes(' ', level + half);
            try w.writeAll("body:\n");
            try tree.dumpNode(data.decl.node, level + delta, w);
        },

        .CompoundStmt,
        .ArrayInitExpr,
        .StructInitExpr,
        .EnumDecl,
        .StructDecl,
        .UnionDecl,
        .AttrParams,
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
        .AttrParamsTwo,
        => {
            if (data.binExpr.lhs != .none) try tree.dumpNode(data.binExpr.lhs, level + delta, w);
            if (data.binExpr.rhs != .none) try tree.dumpNode(data.binExpr.rhs, level + delta, w);
        },

        .UnionInitExpr => {
            try w.writeByteNTimes(' ', level + half);
            try w.writeAll("field index: ");
            util.setColor(LITERAL, w);
            try w.print("{d}\n", .{data.unionInit.fieldIndex});
            util.setColor(.reset, w);
            if (data.unionInit.node != .none) {
                try tree.dumpNode(data.unionInit.node, level + delta, w);
            }
        },

        .CompoundLiteralExpr => {
            try tree.dumpNode(data.unExpr, level + half, w);
        },

        .LabeledStmt => {
            try w.writeByteNTimes(' ', level + half);
            try w.writeAll("label: ");
            util.setColor(LITERAL, w);
            try w.print("{s}\n", .{tree.getTokenSlice(data.decl.name)});
            util.setColor(.reset, w);
            if (data.decl.node != .none) {
                try w.writeByteNTimes(' ', level + half);
                try w.writeAll("stmt:\n");
                try tree.dumpNode(data.decl.node, level + delta, w);
            }
        },

        .BinaryCondExpr,
        .CondExpr,
        .IfThenElseStmt,
        .BuiltinChooseExpr,
        => {
            try w.writeByteNTimes(' ', level + half);
            try w.writeAll("cond:\n");
            try tree.dumpNode(data.if3.cond, level + delta, w);

            try w.writeByteNTimes(' ', level + half);
            try w.writeAll("then:\n");
            try tree.dumpNode(tree.data[data.if3.body], level + delta, w);

            try w.writeByteNTimes(' ', level + half);
            try w.writeAll("else:\n");
            try tree.dumpNode(tree.data[data.if3.body + 1], level + delta, w);
        },

        .IfElseStmt => {
            try w.writeByteNTimes(' ', level + half);
            try w.writeAll("cond:\n");
            try tree.dumpNode(data.binExpr.lhs, level + delta, w);

            try w.writeByteNTimes(' ', level + half);
            try w.writeAll("else:\n");
            try tree.dumpNode(data.binExpr.rhs, level + delta, w);
        },

        .IfThenStmt => {
            try w.writeByteNTimes(' ', level + half);
            try w.writeAll("cond:\n");
            try tree.dumpNode(data.binExpr.lhs, level + delta, w);

            if (data.binExpr.rhs != .none) {
                try w.writeByteNTimes(' ', level + half);
                try w.writeAll("then:\n");
                try tree.dumpNode(data.binExpr.rhs, level + delta, w);
            }
        },

        .GotoStmt, .AddrOfLabel => {
            try w.writeByteNTimes(' ', level + half);
            try w.writeAll("label: ");
            util.setColor(LITERAL, w);
            try w.print("{s}\n", .{tree.getTokenSlice(data.declRef)});
            util.setColor(.reset, w);
        },

        .ContinueStmt,
        .BreakStmt,
        .ImplicitReturn,
        .NullStmt,
        => {},

        .ReturnStmt => {
            if (data.unExpr != .none) {
                try w.writeByteNTimes(' ', level + half);
                try w.writeAll("expr:\n");
                try tree.dumpNode(data.unExpr, level + delta, w);
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
            if (data.unExpr != .none) {
                try w.writeByteNTimes(' ', level + half);
                try w.writeAll("body:\n");
                try tree.dumpNode(data.unExpr, level + delta, w);
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
            try tree.dumpNode(data.binExpr.lhs, level + delta, w);

            if (data.binExpr.rhs != .none) {
                try w.writeByteNTimes(' ', level + half);
                try w.writeAll("body:\n");
                try tree.dumpNode(data.binExpr.rhs, level + delta, w);
            }
        },

        .CaseStmt => {
            try w.writeByteNTimes(' ', level + half);
            try w.writeAll("value:\n");
            try tree.dumpNode(data.binExpr.lhs, level + delta, w);

            if (data.binExpr.rhs != .none) {
                try w.writeByteNTimes(' ', level + half);
                try w.writeAll("stmt:\n");
                try tree.dumpNode(data.binExpr.rhs, level + delta, w);
            }
        },

        .CaseRangeStmt => {
            try w.writeByteNTimes(' ', level + half);
            try w.writeAll("range start:\n");
            try tree.dumpNode(tree.data[data.if3.body], level + delta, w);

            try w.writeByteNTimes(' ', level + half);
            try w.writeAll("range end:\n");
            try tree.dumpNode(tree.data[data.if3.body + 1], level + delta, w);

            if (data.if3.cond != .none) {
                try w.writeByteNTimes(' ', level + half);
                try w.writeAll("stmt:\n");
                try tree.dumpNode(data.if3.cond, level + delta, w);
            }
        },

        .DefaultStmt => {
            if (data.unExpr != .none) {
                try w.writeByteNTimes(' ', level + half);
                try w.writeAll("stmt:\n");
                try tree.dumpNode(data.unExpr, level + delta, w);
            }
        },

        .Var,
        .ExternVar,
        .StaticVar,
        .ImplicitStaticVar,
        .ThreadlocalVar,
        .ThreadlocalExternVar,
        .ThreadlocalStaticVar,
        .TypeDef,
        => {
            try w.writeByteNTimes(' ', level + half);
            try w.writeAll("name: ");
            util.setColor(NAME, w);
            try w.print("{s}\n", .{tree.getTokenSlice(data.decl.name)});
            util.setColor(.reset, w);

            if (data.decl.node != .none) {
                try w.writeByteNTimes(' ', level + half);
                try w.writeAll("init:\n");
                try tree.dumpNode(data.decl.node, level + delta, w);
            }
        },

        .EnumFieldDecl => {
            try w.writeByteNTimes(' ', level + half);
            try w.writeAll("name: ");
            util.setColor(NAME, w);
            try w.print("{s}\n", .{tree.getTokenSlice(data.decl.name)});
            util.setColor(.reset, w);
            if (data.decl.node != .none) {
                try w.writeByteNTimes(' ', level + half);
                try w.writeAll("value:\n");
                try tree.dumpNode(data.decl.node, level + delta, w);
            }
        },

        .RecordFieldDecl => {
            if (data.decl.name != 0) {
                try w.writeByteNTimes(' ', level + half);
                try w.writeAll("name: ");
                util.setColor(NAME, w);
                try w.print("{s}\n", .{tree.getTokenSlice(data.decl.name)});
                util.setColor(.reset, w);
            }

            if (data.decl.node != .none) {
                try w.writeByteNTimes(' ', level + half);
                try w.writeAll("bits:\n");
                try tree.dumpNode(data.decl.node, level + delta, w);
            }
        },

        .AttrArgIdentifier => {
            try w.writeByteNTimes(' ', level + half);
            util.setColor(ATTRIBUTE, w);
            try w.print("name: {s}\n", .{tree.getTokenSlice(data.declRef)});
            util.setColor(.reset, w);
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
            try tree.dumpNode(data.binExpr.lhs, level + delta, w);

            const arg = data.binExpr.rhs;
            if (arg != .none) {
                try w.writeByteNTimes(' ', level + half);
                try w.writeAll("arg:\n");
                try tree.dumpNode(arg, level + delta, w);
            }
        },

        .BuiltinCallExpr => {
            try w.writeByteNTimes(' ', level + half);
            try w.writeAll("name: ");
            util.setColor(NAME, w);
            try w.print("{s}\n", .{tree.getTokenSlice(@intFromEnum(tree.data[data.range.start]))});
            util.setColor(.reset, w);

            try w.writeByteNTimes(' ', level + half);
            try w.writeAll("args:\n");
            for (tree.data[data.range.start + 1 .. data.range.end]) |arg|
                try tree.dumpNode(arg, level + delta, w);
        },

        .BuiltinCallExprOne => {
            try w.writeByteNTimes(' ', level + half);
            try w.writeAll("name: ");
            util.setColor(NAME, w);
            try w.print("{s}\n", .{tree.getTokenSlice(data.decl.name)});
            util.setColor(.reset, w);
            if (data.decl.node != .none) {
                try w.writeByteNTimes(' ', level + half);
                try w.writeAll("arg:\n");
                try tree.dumpNode(data.decl.node, level + delta, w);
            }
        },

        .CommaExpr,
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
            try tree.dumpNode(data.binExpr.lhs, level + delta, w);
            try w.writeByteNTimes(' ', level + 1);
            try w.writeAll("rhs:\n");
            try tree.dumpNode(data.binExpr.rhs, level + delta, w);
        },

        .ExplicitCast,
        .ImplicitCast,
        => try tree.dumpNode(data.cast.operand, level + delta, w),

        .AddrOfExpr,
        .ComputedGotoStmt,
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
            try tree.dumpNode(data.unExpr, level + delta, w);
        },

        .DeclRefExpr => {
            try w.writeByteNTimes(' ', level + 1);
            try w.writeAll("name: ");
            util.setColor(NAME, w);
            try w.print("{s}\n", .{tree.getTokenSlice(data.declRef)});
            util.setColor(.reset, w);
        },

        .EnumerationRef => {
            try w.writeByteNTimes(' ', level + 1);
            try w.writeAll("name: ");
            util.setColor(NAME, w);
            try w.print("{s}\n", .{tree.getTokenSlice(data.declRef)});
            util.setColor(.reset, w);
        },

        .CharLiteral,
        .IntLiteral,
        .FloatLiteral,
        .DoubleLiteral,
        .StringLiteralExpr,
        => {},

        .MemberAccessExpr, .MemberAccessPtrExpr => {
            try w.writeByteNTimes(' ', level + 1);
            try w.writeAll("lhs:\n");
            try tree.dumpNode(data.member.lhs, level + delta, w);

            var lhsType = tree.nodes.items(.type)[@intFromEnum(data.member.lhs)];
            if (lhsType.isPointer()) lhsType = lhsType.getElemType();
            lhsType = lhsType.canonicalize(.standard);

            try w.writeByteNTimes(' ', level + 1);
            try w.writeAll("name: ");
            util.setColor(NAME, w);
            try w.print("{s}\n", .{lhsType.data.record.fields[data.member.index].name});
            util.setColor(.reset, w);
        },

        .ArrayAccessExpr => {
            if (data.binExpr.lhs != .none) {
                try w.writeByteNTimes(' ', level + 1);
                try w.writeAll("lhs:\n");
                try tree.dumpNode(data.binExpr.lhs, level + delta, w);
            }
            try w.writeByteNTimes(' ', level + 1);
            try w.writeAll("index:\n");
            try tree.dumpNode(data.binExpr.rhs, level + delta, w);
        },

        .SizeOfExpr,
        .AlignOfExpr,
        => {
            if (data.unExpr != .none) {
                try w.writeByteNTimes(' ', level + 1);
                try w.writeAll("expr:\n");
                try tree.dumpNode(data.unExpr, level + delta, w);
            }
        },

        .GenericExprOne => {
            try w.writeByteNTimes(' ', level + 1);
            try w.writeAll("controlling:\n");
            try tree.dumpNode(data.binExpr.lhs, level + delta, w);
            try w.writeByteNTimes(' ', level + 1);
            try w.writeAll("chosen:\n");
            try tree.dumpNode(data.binExpr.rhs, level + delta, w);
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

        .GenericAssociationExpr,
        .GenericDefaultExpr,
        .StmtExpr,
        .ImaginaryLiteral,
        => try tree.dumpNode(data.unExpr, level + delta, w),

        .ArrayFillerExpr => {
            try w.writeByteNTimes(' ', level + 1);
            try w.writeAll("count: ");
            util.setColor(LITERAL, w);
            try w.print("{d}\n", .{data.int});
            util.setColor(.reset, w);
        },

        .StructForwardDecl,
        .UnionForwardDecl,
        .EnumForwardDecl,
        .DefaultInitExpr,
        .CondDummyExpr,
        => {},
    }
}
