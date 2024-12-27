const std = @import("std");
const assert = std.debug.assert;
const Interner = @import("backend").Interner;
const Type = @import("Type.zig");
const Compilation = @import("../Basic/Compilation.zig");
const CodeGen = @import("../CodeGen/CodeGen.zig");
const Source = @import("../Basic/Source.zig");
const Lexer = @import("../Lexer/Lexer.zig");
const TokenType = @import("../Basic/TokenType.zig").TokenType;
const AstTag = @import("AstTag.zig").Tag;
const Attribute = @import("../Lexer/Attribute.zig");
const Value = @import("Value.zig");
const StringInterner = @import("../Basic/StringInterner.zig");

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
valueMap: ValueMap,

pub const genIR = CodeGen.generateIR;

pub fn deinit(tree: *AST) void {
    tree.comp.gpa.free(tree.rootDecls);
    tree.comp.gpa.free(tree.data);
    tree.nodes.deinit(tree.comp.gpa);
    tree.arena.deinit();
    tree.valueMap.deinit();
}

pub const GNUAssemblyQualifiers = struct {
    @"volatile": bool = false,
    @"inline": bool = false,
    goto: bool = false,
};

pub const Token = struct {
    id: TokenType,
    flags: packed struct {
        expansionDisabled: bool = false,
        isMacroArg: bool = false,
    } = .{},
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

    pub fn checkMsEof(tok: Token, source: Source, comp: *Compilation) !void {
        std.debug.assert(tok.id == .Eof);
        if (source.buffer.len > tok.loc.byteOffset and source.buffer[tok.loc.byteOffset] == 0x1A) {
            try comp.addDiagnostic(.{
                .tag = .ctrl_z_eof,
                .loc = .{
                    .id = source.id,
                    .byteOffset = tok.loc.byteOffset,
                    .line = tok.loc.line,
                },
            }, &.{});
        }
    }

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

    /// How many source locations do we track for each token.
    /// Must be at least 2.
    pub const List = std.MultiArrayList(Token);
};

pub const Range = struct { start: u32, end: u32 };
pub const Node = struct {
    tag: AstTag,
    type: Type = Type.Void,
    data: Data,

    pub const List = std.MultiArrayList(Node);

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
        returnZero: bool,

        pub fn forDecl(data: Data, tree: *const AST) struct {
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

        pub fn forStmt(data: Data, tree: *const AST) struct {
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
};

pub fn isBitField(tree: *const AST, node: NodeIndex) bool {
    switch (tree.nodes.items(.tag)[@intFromEnum(node)]) {
        .MemberAccessExpr, .MemberAccessPtrExpr => {
            const member = tree.nodes.items(.data)[@intFromEnum(node)].member;
            var ty = tree.nodes.items(.type)[@intFromEnum(member.lhs)];
            if (ty.isPointer())
                ty = ty.getElemType();

            const recordTy = ty.get(.Struct) orelse ty.get(.Union) orelse return false;
            const field = recordTy.data.record.fields[member.index];
            return field.bitWidth != null;
        },
        else => return false,
    }
}

pub fn isLValue(tree: *const AST, node: NodeIndex) bool {
    var isConst: bool = undefined;
    return tree.isLValueExtra(node, &isConst);
}

pub fn isLValueExtra(tree: *const AST, node: NodeIndex, isConst: *bool) bool {
    isConst.* = false;

    const tag = tree.nodes.items(.tag)[@intFromEnum(node)];
    const data = tree.nodes.items(.data)[@intFromEnum(node)];
    switch (tag) {
        .CompoundLiteralExpr,
        .StaticCompoundLiteralExpr,
        .ThreadLocalCompoundLiteralExpr,
        .StaticThreadLocalCompoundLiteralExpr,
        => {
            isConst.* = tree.nodes.items(.type)[@intFromEnum(node)].isConst();
            return true;
        },

        .StringLiteralExpr => return true,
        .MemberAccessPtrExpr => {
            const lhsExpr = data.member.lhs;
            const ptrExpr = tree.nodes.items(.type)[@intFromEnum(lhsExpr)];
            if (ptrExpr.isPointer())
                isConst.* = ptrExpr.getElemType().isConst();
            return true;
        },

        .ArrayAccessExpr => {
            const lhsExpr = data.binExpr.lhs;
            if (lhsExpr != .none) {
                const arrayType = tree.nodes.items(.type)[@intFromEnum(lhsExpr)];
                if (arrayType.isPointer() or arrayType.isArray())
                    isConst.* = arrayType.getElemType().isConst();
            }
            return true;
        },

        .DeclRefExpr => {
            const declType = tree.nodes.items(.type)[@intFromEnum(node)];
            isConst.* = declType.isConst();
            return true;
        },

        .DerefExpr => {
            const operandType = tree.nodes.items(.type)[@intFromEnum(data.unExpr)];
            if (operandType.isFunc())
                return false;
            if (operandType.isPointer() or operandType.isArray())
                isConst.* = operandType.getElemType().isConst();
            return true;
        },

        .MemberAccessExpr => return tree.isLValueExtra(data.member.lhs, isConst),

        .ParenExpr => return tree.isLValueExtra(data.unExpr, isConst),

        .BuiltinChooseExpr => {
            if (tree.valueMap.get(data.if3.cond)) |val| {
                const offset = @intFromBool(val.isZero(tree.comp));
                return tree.isLValueExtra(tree.data[data.if3.body + offset], isConst);
            }
            return false;
        },

        else => return false,
    }
}

pub fn getTokenSlice(tree: AST, index: TokenIndex) []const u8 {
    if (tree.tokens.items(.id)[index].lexeme()) |some|
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

pub fn dump(tree: AST, config: std.io.tty.Config, writer: anytype) !void {
    const mapper = tree.comp.stringInterner.getFastTypeMapper(tree.comp.gpa) catch tree.comp.stringInterner.getSlowTypeMapper();
    defer mapper.deinit(tree.comp.gpa);

    for (tree.rootDecls) |i| {
        try tree.dumpNode(i, 0, mapper, config, writer);
        try writer.writeByte('\n');
    }
}

fn dumpFieldAttributes(tree: *const AST, attributes: []const Attribute, level: u32, writer: anytype) !void {
    for (attributes) |attr| {
        try writer.writeByteNTimes(' ', level);
        try writer.print("field attr: {s}", .{@tagName(attr.tag)});
        try tree.dumpAttribute(attr, writer);
    }
}

fn dumpAttribute(tree: *const AST, attr: Attribute, writer: anytype) !void {
    switch (attr.tag) {
        inline else => |tag| {
            const args = @field(attr.args, @tagName(tag));
            const fields = @typeInfo(@TypeOf(args)).@"struct".fields;
            if (fields.len == 0) {
                try writer.writeByte('\n');
                return;
            }
            try writer.writeByte(' ');
            inline for (fields, 0..) |f, i| {
                if (comptime std.mem.eql(u8, f.name, "__name_token")) continue;
                if (i != 0)
                    try writer.writeAll(", ");

                try writer.writeAll(f.name);
                try writer.writeAll(": ");
                switch (f.type) {
                    Interner.Ref => try writer.print("\"{s}\"", .{tree.interner.get(@field(args, f.name)).bytes}),
                    ?Interner.Ref => try writer.print("\"{?s}\"", .{if (@field(args, f.name)) |str| tree.interner.get(str).bytes else null}),
                    else => switch (@typeInfo(f.type)) {
                        .@"enum" => try writer.writeAll(@tagName(@field(args, f.name))),
                        else => try writer.print("{any}", .{@field(args, f.name)}),
                    },
                }
            }
            try writer.writeByte('\n');
            return;
        },
    }
}

fn dumpNode(
    tree: *const AST,
    node: NodeIndex,
    level: u32,
    mapper: StringInterner.TypeMapper,
    config: std.io.tty.Config,
    w: anytype,
) !void {
    const delta = 2;
    const half = delta / 2;
    const TYPE = std.io.tty.Color.bright_magenta;
    const TAG = std.io.tty.Color.bright_cyan;
    const IMPLICIT = std.io.tty.Color.bright_blue;
    const NAME = std.io.tty.Color.bright_red;
    const LITERAL = std.io.tty.Color.bright_green;
    const ATTRIBUTE = std.io.tty.Color.bright_yellow;

    assert(node != .none);

    const tag = tree.nodes.items(.tag)[@intFromEnum(node)];
    const data = tree.nodes.items(.data)[@intFromEnum(node)];
    const ty = tree.nodes.items(.type)[@intFromEnum(node)];
    try w.writeByteNTimes(' ', level);

    try config.setColor(w, if (tag.isImplicit()) IMPLICIT else TAG);
    try w.print("{s}: ", .{@tagName(tag)});
    if (tag == .ImplicitCast or tag == .ExplicitCast) {
        try config.setColor(w, .white);
        try w.print("({s}) ", .{@tagName(data.cast.kind)});
    }

    try config.setColor(w, TYPE);
    try w.writeByte('\'');
    try ty.dump(mapper, tree.comp.langOpts, w);
    try w.writeByte('\'');

    if (tree.isLValue(node)) {
        try config.setColor(w, ATTRIBUTE);
        try w.writeAll(" lvalue");
    }

    if (tree.isBitField(node)) {
        try config.setColor(w, ATTRIBUTE);
        try w.writeAll(" bitfield");
    }

    if (tree.valueMap.get(node)) |val| {
        try config.setColor(w, LITERAL);
        try w.writeAll(" (value: ");
        try val.print(ty, tree.comp, w);
        try w.writeByte(')');
    }

    if (tag == .ImplicitReturn and data.returnZero) {
        try config.setColor(w, IMPLICIT);
        try w.writeAll(" (value: 0)");
        try config.setColor(w, .reset);
    }

    try w.writeAll("\n");
    try config.setColor(w, .reset);

    if (ty.specifier == .Attributed) {
        try config.setColor(w, ATTRIBUTE);
        for (ty.data.attributed.attributes) |attr| {
            try w.writeByteNTimes(' ', level + half);
            try w.print("attr: {s}", .{@tagName(attr.tag)});
            try tree.dumpAttribute(attr, w);
        }

        try config.setColor(w, .reset);
    }

    switch (tag) {
        .Invalid => unreachable,
        .FileScopeAsm => {
            try w.writeByteNTimes(' ', level + 1);
            try tree.dumpNode(data.decl.node, level + delta, mapper, config, w);
        },

        .GNUAsmSimple => {
            try w.writeByteNTimes(' ', level);
            try tree.dumpNode(data.unExpr, level, mapper, config, w);
        },

        .StaticAssert => {
            try w.writeByteNTimes(' ', level + 1);
            try w.writeAll("condition:\n");
            try tree.dumpNode(data.binExpr.lhs, level + delta, mapper, config, w);

            if (data.binExpr.rhs != .none) {
                try w.writeByteNTimes(' ', level + 1);
                try w.writeAll("diagnostic:\n");
                try tree.dumpNode(data.binExpr.rhs, level + delta, mapper, config, w);
            }
        },

        .FnProto,
        .StaticFnProto,
        .InlineFnProto,
        .InlineStaticFnProto,
        => {
            try w.writeByteNTimes(' ', level + half);
            try w.writeAll("name: ");

            try config.setColor(w, NAME);
            try w.print("{s}\n", .{tree.getTokenSlice(data.decl.name)});
            try config.setColor(w, .reset);
        },

        .FnDef,
        .StaticFnDef,
        .InlineFnDef,
        .InlineStaticFnDef,
        => {
            try w.writeByteNTimes(' ', level + half);
            try w.writeAll("name: ");

            try config.setColor(w, NAME);
            try w.print("{s}\n", .{tree.getTokenSlice(data.decl.name)});
            try config.setColor(w, .reset);

            try w.writeByteNTimes(' ', level + half);
            try w.writeAll("body:\n");
            try tree.dumpNode(data.decl.node, level + delta, mapper, config, w);
        },

        .CompoundStmt,
        .ArrayInitExpr,
        .StructInitExpr,
        .EnumDecl,
        .StructDecl,
        .UnionDecl,
        => {
            const maybeFieldAttrs = if (ty.getRecord()) |record| record.fieldAttributes else null;
            for (tree.data[data.range.start..data.range.end], 0..) |stmt, i| {
                if (i != 0)
                    try w.writeByte('\n');
                try tree.dumpNode(stmt, level + delta, mapper, config, w);
                if (maybeFieldAttrs) |fieldAttrs| {
                    if (fieldAttrs[i].len == 0) continue;

                    try config.setColor(w, ATTRIBUTE);
                    try tree.dumpFieldAttributes(fieldAttrs[i], level + delta + half, w);
                    try config.setColor(w, .reset);
                }
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
            var attrArray = [2][]const Attribute{ &.{}, &.{} };
            const empty: [][]const Attribute = &attrArray;
            const fieldAttrs = if (ty.getRecord()) |record|
                (record.fieldAttributes orelse empty.ptr)
            else
                empty.ptr;

            if (data.binExpr.lhs != .none) {
                try tree.dumpNode(data.binExpr.lhs, level + delta, mapper, config, w);
                if (fieldAttrs[0].len > 0) {
                    try config.setColor(w, ATTRIBUTE);
                    try tree.dumpFieldAttributes(fieldAttrs[0], level + delta + half, w);
                    try config.setColor(w, .reset);
                }
            }

            if (data.binExpr.rhs != .none) {
                try tree.dumpNode(data.binExpr.rhs, level + delta, mapper, config, w);
                if (fieldAttrs[1].len > 0) {
                    try config.setColor(w, ATTRIBUTE);
                    try tree.dumpFieldAttributes(fieldAttrs[1], level + delta + half, w);
                    try config.setColor(w, .reset);
                }
            }
        },

        .UnionInitExpr => {
            try w.writeByteNTimes(' ', level + half);
            try w.writeAll("field index: ");

            try config.setColor(w, LITERAL);
            try w.print("{d}\n", .{data.unionInit.fieldIndex});
            try config.setColor(w, .reset);

            if (data.unionInit.node != .none)
                try tree.dumpNode(data.unionInit.node, level + delta, mapper, config, w);
        },

        .CompoundLiteralExpr,
        .StaticCompoundLiteralExpr,
        .ThreadLocalCompoundLiteralExpr,
        .StaticThreadLocalCompoundLiteralExpr,
        => try tree.dumpNode(data.unExpr, level + half, mapper, config, w),

        .LabeledStmt => {
            try w.writeByteNTimes(' ', level + half);
            try w.writeAll("label: ");

            try config.setColor(w, LITERAL);
            try w.print("{s}\n", .{tree.getTokenSlice(data.decl.name)});
            try config.setColor(w, .reset);

            if (data.decl.node != .none) {
                try w.writeByteNTimes(' ', level + half);
                try w.writeAll("stmt:\n");
                try tree.dumpNode(data.decl.node, level + delta, mapper, config, w);
            }
        },

        .BinaryCondExpr,
        .CondExpr,
        .IfThenElseStmt,
        .BuiltinChooseExpr,
        => {
            try w.writeByteNTimes(' ', level + half);
            try w.writeAll("cond:\n");
            try tree.dumpNode(data.if3.cond, level + delta, mapper, config, w);

            try w.writeByteNTimes(' ', level + half);
            try w.writeAll("then:\n");
            try tree.dumpNode(tree.data[data.if3.body], level + delta, mapper, config, w);

            try w.writeByteNTimes(' ', level + half);
            try w.writeAll("else:\n");
            try tree.dumpNode(tree.data[data.if3.body + 1], level + delta, mapper, config, w);
        },

        .BuiltinTypesCompatibleP => {
            assert(tree.nodes.items(.tag)[@intFromEnum(data.binExpr.lhs)] == .Invalid);
            assert(tree.nodes.items(.tag)[@intFromEnum(data.binExpr.rhs)] == .Invalid);

            try w.writeByteNTimes(' ', level + half);
            try w.writeAll("lhs: ");

            const lhsTy = tree.nodes.items(.type)[@intFromEnum(data.binExpr.lhs)];
            try config.setColor(w, TYPE);
            try lhsTy.dump(mapper, tree.comp.langOpts, w);
            try config.setColor(w, .reset);
            try w.writeByte('\n');

            try w.writeByteNTimes(' ', level + half);
            try w.writeAll("rhs: ");

            const rhsTy = tree.nodes.items(.type)[@intFromEnum(data.binExpr.rhs)];
            try config.setColor(w, TYPE);
            try rhsTy.dump(mapper, tree.comp.langOpts, w);
            try config.setColor(w, .reset);
            try w.writeByte('\n');
        },

        .IfThenStmt => {
            try w.writeByteNTimes(' ', level + half);
            try w.writeAll("cond:\n");
            try tree.dumpNode(data.binExpr.lhs, level + delta, mapper, config, w);

            if (data.binExpr.rhs != .none) {
                try w.writeByteNTimes(' ', level + half);
                try w.writeAll("then:\n");
                try tree.dumpNode(data.binExpr.rhs, level + delta, mapper, config, w);
            }
        },

        .GotoStmt, .AddrOfLabel => {
            try w.writeByteNTimes(' ', level + half);
            try w.writeAll("label: ");

            try config.setColor(w, LITERAL);
            try w.print("{s}\n", .{tree.getTokenSlice(data.declRef)});
            try config.setColor(w, .reset);
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
                try tree.dumpNode(data.unExpr, level + delta, mapper, config, w);
            }
        },

        .ForDeclStmt => {
            const forDecl = data.forDecl(tree);

            try w.writeByteNTimes(' ', level + half);
            try w.writeAll("decl:\n");
            for (forDecl.decls) |decl| {
                try tree.dumpNode(decl, level + delta, mapper, config, w);
                try w.writeByte('\n');
            }

            if (forDecl.cond != .none) {
                try w.writeByteNTimes(' ', level + half);
                try w.writeAll("cond:\n");
                try tree.dumpNode(forDecl.cond, level + delta, mapper, config, w);
            }

            if (forDecl.incr != .none) {
                try w.writeByteNTimes(' ', level + half);
                try w.writeAll("incr:\n");
                try tree.dumpNode(forDecl.incr, level + delta, mapper, config, w);
            }

            if (forDecl.body != .none) {
                try w.writeByteNTimes(' ', level + half);
                try w.writeAll("body:\n");
                try tree.dumpNode(forDecl.body, level + delta, mapper, config, w);
            }
        },

        .ForEverStmt => {
            if (data.unExpr != .none) {
                try w.writeByteNTimes(' ', level + half);
                try w.writeAll("body:\n");
                try tree.dumpNode(data.unExpr, level + delta, mapper, config, w);
            }
        },

        .ForStmt => {
            const forStmt = data.forStmt(tree);

            if (forStmt.init != .none) {
                try w.writeByteNTimes(' ', level + half);
                try w.writeAll("init:\n");
                try tree.dumpNode(forStmt.init, level + delta, mapper, config, w);
            }

            if (forStmt.cond != .none) {
                try w.writeByteNTimes(' ', level + half);
                try w.writeAll("cond:\n");
                try tree.dumpNode(forStmt.cond, level + delta, mapper, config, w);
            }

            if (forStmt.incr != .none) {
                try w.writeByteNTimes(' ', level + half);
                try w.writeAll("incr:\n");
                try tree.dumpNode(forStmt.incr, level + delta, mapper, config, w);
            }

            if (forStmt.body != .none) {
                try w.writeByteNTimes(' ', level + half);
                try w.writeAll("body:\n");
                try tree.dumpNode(forStmt.body, level + delta, mapper, config, w);
            }
        },

        .SwitchStmt, .WhileStmt, .DoWhileStmt => {
            try w.writeByteNTimes(' ', level + half);
            try w.writeAll("cond:\n");
            try tree.dumpNode(data.binExpr.lhs, level + delta, mapper, config, w);

            if (data.binExpr.rhs != .none) {
                try w.writeByteNTimes(' ', level + half);
                try w.writeAll("body:\n");
                try tree.dumpNode(data.binExpr.rhs, level + delta, mapper, config, w);
            }
        },

        .CaseStmt => {
            try w.writeByteNTimes(' ', level + half);
            try w.writeAll("value:\n");
            try tree.dumpNode(data.binExpr.lhs, level + delta, mapper, config, w);

            if (data.binExpr.rhs != .none) {
                try w.writeByteNTimes(' ', level + half);
                try w.writeAll("stmt:\n");
                try tree.dumpNode(data.binExpr.rhs, level + delta, mapper, config, w);
            }
        },

        .CaseRangeStmt => {
            try w.writeByteNTimes(' ', level + half);
            try w.writeAll("range start:\n");
            try tree.dumpNode(tree.data[data.if3.body], level + delta, mapper, config, w);

            try w.writeByteNTimes(' ', level + half);
            try w.writeAll("range end:\n");
            try tree.dumpNode(tree.data[data.if3.body + 1], level + delta, mapper, config, w);

            if (data.if3.cond != .none) {
                try w.writeByteNTimes(' ', level + half);
                try w.writeAll("stmt:\n");
                try tree.dumpNode(data.if3.cond, level + delta, mapper, config, w);
            }
        },

        .DefaultStmt => {
            if (data.unExpr != .none) {
                try w.writeByteNTimes(' ', level + half);
                try w.writeAll("stmt:\n");
                try tree.dumpNode(data.unExpr, level + delta, mapper, config, w);
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

            try config.setColor(w, NAME);
            try w.print("{s}\n", .{tree.getTokenSlice(data.decl.name)});
            try config.setColor(w, .reset);

            if (data.decl.node != .none) {
                try w.writeByteNTimes(' ', level + half);
                try w.writeAll("init:\n");
                try tree.dumpNode(data.decl.node, level + delta, mapper, config, w);
            }
        },

        .EnumFieldDecl => {
            try w.writeByteNTimes(' ', level + half);
            try w.writeAll("name: ");

            try config.setColor(w, NAME);
            try w.print("{s}\n", .{tree.getTokenSlice(data.decl.name)});
            try config.setColor(w, .reset);

            if (data.decl.node != .none) {
                try w.writeByteNTimes(' ', level + half);
                try w.writeAll("value:\n");
                try tree.dumpNode(data.decl.node, level + delta, mapper, config, w);
            }
        },

        .RecordFieldDecl => {
            if (data.decl.name != 0) {
                try w.writeByteNTimes(' ', level + half);
                try w.writeAll("name: ");

                try config.setColor(w, NAME);
                try w.print("{s}\n", .{tree.getTokenSlice(data.decl.name)});
                try config.setColor(w, .reset);
            }

            if (data.decl.node != .none) {
                try w.writeByteNTimes(' ', level + half);
                try w.writeAll("bits:\n");
                try tree.dumpNode(data.decl.node, level + delta, mapper, config, w);
            }
        },

        .CallExpr => {
            try w.writeByteNTimes(' ', level + half);
            try w.writeAll("lhs:\n");
            try tree.dumpNode(tree.data[data.range.start], level + delta, mapper, config, w);

            try w.writeByteNTimes(' ', level + half);
            try w.writeAll("args:\n");
            for (tree.data[data.range.start + 1 .. data.range.end]) |arg| try tree.dumpNode(arg, level + delta, mapper, config, w);
        },

        .CallExprOne => {
            try w.writeByteNTimes(' ', level + half);
            try w.writeAll("lhs:\n");
            try tree.dumpNode(data.binExpr.lhs, level + delta, mapper, config, w);

            const arg = data.binExpr.rhs;
            if (arg != .none) {
                try w.writeByteNTimes(' ', level + half);
                try w.writeAll("arg:\n");
                try tree.dumpNode(arg, level + delta, mapper, config, w);
            }
        },

        .BuiltinCallExpr => {
            try w.writeByteNTimes(' ', level + half);
            try w.writeAll("name: ");

            try config.setColor(w, NAME);
            try w.print("{s}\n", .{tree.getTokenSlice(@intFromEnum(tree.data[data.range.start]))});
            try config.setColor(w, .reset);

            try w.writeByteNTimes(' ', level + half);
            try w.writeAll("args:\n");
            for (tree.data[data.range.start + 1 .. data.range.end]) |arg|
                try tree.dumpNode(arg, level + delta, mapper, config, w);
        },

        .BuiltinCallExprOne => {
            try w.writeByteNTimes(' ', level + half);
            try w.writeAll("name: ");

            try config.setColor(w, NAME);
            try w.print("{s}\n", .{tree.getTokenSlice(data.decl.name)});
            try config.setColor(w, .reset);

            if (data.decl.node != .none) {
                try w.writeByteNTimes(' ', level + half);
                try w.writeAll("arg:\n");
                try tree.dumpNode(data.decl.node, level + delta, mapper, config, w);
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
            try tree.dumpNode(data.binExpr.lhs, level + delta, mapper, config, w);
            try w.writeByteNTimes(' ', level + 1);
            try w.writeAll("rhs:\n");
            try tree.dumpNode(data.binExpr.rhs, level + delta, mapper, config, w);
        },

        .ExplicitCast,
        .ImplicitCast,
        => try tree.dumpNode(data.cast.operand, level + delta, mapper, config, w),

        .AddrOfExpr,
        .ComputedGotoStmt,
        .DerefExpr,
        .PlusExpr,
        .NegateExpr,
        .BitNotExpr,
        .BoolNotExpr,
        .PreIncExpr,
        .PreDecExpr,
        .ImagExpr,
        .RealExpr,
        .PostIncExpr,
        .PostDecExpr,
        .ParenExpr,
        => {
            try w.writeByteNTimes(' ', level + 1);
            try w.writeAll("operand:\n");
            try tree.dumpNode(data.unExpr, level + delta, mapper, config, w);
        },

        .DeclRefExpr => {
            try w.writeByteNTimes(' ', level + 1);
            try w.writeAll("name: ");

            try config.setColor(w, NAME);
            try w.print("{s}\n", .{tree.getTokenSlice(data.declRef)});
            try config.setColor(w, .reset);
        },

        .EnumerationRef => {
            try w.writeByteNTimes(' ', level + 1);
            try w.writeAll("name: ");
            try config.setColor(w, NAME);
            try w.print("{s}\n", .{tree.getTokenSlice(data.declRef)});
            try config.setColor(w, .reset);
        },

        .BoolLiteral,
        .NullPtrLiteral,
        .CharLiteral,
        .IntLiteral,
        .FloatLiteral,
        .StringLiteralExpr,
        => {},

        .MemberAccessExpr, .MemberAccessPtrExpr => {
            try w.writeByteNTimes(' ', level + 1);
            try w.writeAll("lhs:\n");
            try tree.dumpNode(data.member.lhs, level + delta, mapper, config, w);

            var lhsType = tree.nodes.items(.type)[@intFromEnum(data.member.lhs)];
            if (lhsType.isPointer()) lhsType = lhsType.getElemType();
            lhsType = lhsType.canonicalize(.standard);

            try w.writeByteNTimes(' ', level + 1);
            try w.writeAll("name: ");

            try config.setColor(w, NAME);
            try w.print("{s}\n", .{mapper.lookup(lhsType.data.record.fields[data.member.index].name)});
            try config.setColor(w, .reset);
        },

        .ArrayAccessExpr => {
            if (data.binExpr.lhs != .none) {
                try w.writeByteNTimes(' ', level + 1);
                try w.writeAll("lhs:\n");
                try tree.dumpNode(data.binExpr.lhs, level + delta, mapper, config, w);
            }
            try w.writeByteNTimes(' ', level + 1);
            try w.writeAll("index:\n");
            try tree.dumpNode(data.binExpr.rhs, level + delta, mapper, config, w);
        },

        .SizeOfExpr,
        .AlignOfExpr,
        => {
            if (data.unExpr != .none) {
                try w.writeByteNTimes(' ', level + 1);
                try w.writeAll("expr:\n");
                try tree.dumpNode(data.unExpr, level + delta, mapper, config, w);
            }
        },

        .GenericExprOne => {
            try w.writeByteNTimes(' ', level + 1);
            try w.writeAll("controlling:\n");
            try tree.dumpNode(data.binExpr.lhs, level + delta, mapper, config, w);
            try w.writeByteNTimes(' ', level + 1);

            if (data.binExpr.rhs != .none) {
                try w.writeAll("chosen:\n");
                try tree.dumpNode(data.binExpr.rhs, level + delta, mapper, config, w);
            }
        },

        .GenericExpr => {
            const nodes = tree.data[data.range.start..data.range.end];
            try w.writeByteNTimes(' ', level + 1);
            try w.writeAll("controlling:\n");
            try tree.dumpNode(nodes[0], level + delta, mapper, config, w);
            try w.writeByteNTimes(' ', level + 1);
            try w.writeAll("chosen:\n");
            try tree.dumpNode(nodes[1], level + delta, mapper, config, w);
            try w.writeByteNTimes(' ', level + 1);
            try w.writeAll("rest:\n");
            for (nodes[2..]) |expr|
                try tree.dumpNode(expr, level + delta, mapper, config, w);
        },

        .GenericAssociationExpr,
        .GenericDefaultExpr,
        .StmtExpr,
        .ImaginaryLiteral,
        => try tree.dumpNode(data.unExpr, level + delta, mapper, config, w),

        .ArrayFillerExpr => {
            try w.writeByteNTimes(' ', level + 1);
            try w.writeAll("count: ");
            try config.setColor(w, LITERAL);
            try w.print("{d}\n", .{data.int});
            try config.setColor(w, .reset);
        },

        .StructForwardDecl,
        .UnionForwardDecl,
        .EnumForwardDecl,
        .DefaultInitExpr,
        .CondDummyExpr,
        => {},
    }
}
