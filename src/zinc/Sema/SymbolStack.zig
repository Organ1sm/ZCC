const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
const assert = std.debug.assert;

const Parser = @import("../Parser/Parser.zig");
const QualType = @import("../AST/TypeStore.zig").QualType;
const Value = @import("../AST/Value.zig");
const StringId = @import("../Basic/StringInterner.zig").StringId;

const TokenType = @import("../Basic/TokenType.zig").TokenType;
const Tree = @import("../AST/AST.zig");
const Token = Tree.Token;
const TokenIndex = Tree.TokenIndex;
const Node = Tree.Node;

const SymbolStack = @This();

pub const Symbol = struct {
    /// The name of the symbol.
    name: StringId,
    /// The type of the symbol.
    qt: QualType,
    /// The token index that represents this symbol.
    token: TokenIndex,
    /// The node index in the AST (Abstract Syntax Tree) that represents this symbol.
    node: Node.OptIndex = .null,
    /// The kind of the symbol, which categorizes it into various types like typedef, enum, etc.
    kind: Kind,
    /// The value of the symbol, if it has one.
    value: Value,
};

/// Kind is an enumeration of possible kinds that a Symbol can have,
// zig fmt: off
pub const Kind = enum {
    typedef,     // A type definition.
    @"struct",   // A structure.
    @"union",    // A union.
    @"enum",     // An enum.
    declaration, // A declaration of a symbol without a definition.
    definition,  // A definition of a symbol where its complete information is provided.
    enumeration, // An enumeration value within an enum.
    constexpr,
};
// zig fmt: on

const Scope = struct {
    vars: std.AutoHashMapUnmanaged(StringId, Symbol) = .{},
    tags: std.AutoHashMapUnmanaged(StringId, Symbol) = .{},

    fn deinit(self: *Scope, allocator: Allocator) void {
        self.vars.deinit(allocator);
        self.tags.deinit(allocator);
    }

    fn clearRetainingCapacity(self: *Scope) void {
        self.vars.clearRetainingCapacity();
        self.tags.clearRetainingCapacity();
    }
};

/// allocations from nested scopes are retained after popping;
/// `activeLen` is the number of currently-active items in `scopes`.
activeLen: usize = 0,
scopes: std.ArrayListUnmanaged(Scope) = .{},
p: *Parser = undefined,

pub fn deinit(self: *SymbolStack, gpa: Allocator) void {
    assert(self.activeLen == 0); // all scopes should be popped at end
    for (self.scopes.items) |*scope|
        scope.deinit(gpa);
    self.scopes.deinit(gpa);
    self.p = undefined;
    self.* = undefined;
}

pub fn pushScope(self: *SymbolStack) !void {
    if (self.activeLen + 1 > self.scopes.items.len) {
        try self.scopes.append(self.p.gpa, .{});
        self.activeLen = self.scopes.items.len;
    } else {
        self.scopes.items[self.activeLen].clearRetainingCapacity();
        self.activeLen += 1;
    }
}

pub fn popScope(self: *SymbolStack) void {
    self.activeLen -= 1;
}

/// findTypedef searches for a typedef symbol with the given name within the symbol stack.
/// If the symbol is found and it is a struct, union, or enum, it ensures that the type has been
/// defined unless `noTypeYet` is true, in which case it returns null.
/// If the symbol is a typedef, it returns the symbol immediately.
/// If the symbol is not found, or it is a definition or declaration, it returns null.
/// @param self        The SymbolStack instance.
/// @param nameToken   The index of the token representing the symbol's name.
/// @param noTypeYet   A boolean indicating whether the type should not have been defined yet.
/// @return            A nullable Symbol if found, or null if not found or if it is a definition or declaration.
pub fn findTypedef(
    self: *SymbolStack,
    name: StringId,
    nameToken: TokenIndex,
    noTypeYet: bool,
) !?Symbol {
    const prev = self.lookup(name, .vars) orelse self.lookup(name, .tags) orelse return null;
    switch (prev.kind) {
        .typedef => return prev,
        .@"struct", .@"union", .@"enum" => {
            if (noTypeYet) return null;
            try self.p.err(switch (prev.kind) {
                .@"struct" => .must_use_struct,
                .@"union" => .must_use_union,
                .@"enum" => .must_use_enum,
                else => unreachable,
            }, nameToken, .{self.p.getTokenText(nameToken)});
            return prev;
        },
        else => return null,
    }
}

/// Find a symbol by its name within the current symbol stack.
/// @param  self       The SymbolStack instance.
/// @param  name       The Symbol's name.
/// @return            A nullable Symbol if found, or null if not found.
pub fn findSymbol(self: *SymbolStack, name: StringId) ?Symbol {
    return self.lookup(name, .vars);
}

/// Find a tag symbol within the symbol stack by its kind and name.
/// @param self       The SymbolStack instance.
/// @param kind       The TokenType representing the kind of tag (enum, struct, union).
/// @param nameToken  The token index used to identify the symbol's name.
/// @return           A nullable Symbol if a matching tag is found, or null otherwise.
pub fn findTag(
    self: *SymbolStack,
    name: StringId,
    kind: TokenType,
    nameToken: TokenIndex,
    nextTokenID: TokenType,
) !?Symbol {
    // `tag Name;` should always result in a new type if in a new scope.
    const prev = (if (nextTokenID == .Semicolon) self.get(name, .tags) else self.lookup(name, .tags)) orelse return null;
    switch (prev.kind) {
        .@"enum" => if (kind == .KeywordEnum) return prev,
        .@"struct" => if (kind == .KeywordStruct) return prev,
        .@"union" => if (kind == .KeywordUnion) return prev,
        else => unreachable,
    }

    if (self.get(name, .tags) == null) return null;
    try self.p.err(.wrong_tag, nameToken, .{self.p.getTokenText(nameToken)});
    try self.p.err(.previous_definition, prev.token, .{});

    return null;
}

const ScopeKind = enum {
    /// structs, enums, unions
    tags,
    /// variables
    vars,
};

/// Return the Symbol for `name` (or null if not found) in the innermost scope
pub fn get(s: *SymbolStack, name: StringId, kind: ScopeKind) ?Symbol {
    return switch (kind) {
        .vars => s.scopes.items[s.activeLen - 1].vars.get(name),
        .tags => s.scopes.items[s.activeLen - 1].tags.get(name),
    };
}

/// Return the Symbol for `name` (or null if not found) in the nearest active scope,
/// starting at the innermost.
fn lookup(s: *SymbolStack, name: StringId, kind: ScopeKind) ?Symbol {
    var i = s.activeLen;
    while (i > 0) {
        i -= 1;
        switch (kind) {
            .vars => if (s.scopes.items[i].vars.get(name)) |sym| return sym,
            .tags => if (s.scopes.items[i].tags.get(name)) |sym| return sym,
        }
    }
    return null;
}

/// Define a symbol in the innermost scope. Does not issue diagnostics or check correctness
/// with regard to the C standard.
pub fn define(self: *SymbolStack, symbol: Symbol) !void {
    switch (symbol.kind) {
        .constexpr, .definition, .declaration, .enumeration, .typedef => {
            try self.scopes.items[self.activeLen - 1].vars.put(self.p.gpa, symbol.name, symbol);
        },
        .@"struct", .@"union", .@"enum" => {
            try self.scopes.items[self.activeLen - 1].tags.put(self.p.gpa, symbol.name, symbol);
        },
    }
}

pub fn defineTypedef(
    self: *SymbolStack,
    name: StringId,
    qt: QualType,
    token: TokenIndex,
    node: Node.Index,
) !void {
    if (self.get(name, .vars)) |prev| {
        switch (prev.kind) {
            .typedef => {
                if (!prev.qt.isInvalid() and !qt.eqlQualified(prev.qt, self.p.comp)) {
                    if (qt.isInvalid()) return;
                    const nonTypedefQt = qt.type(self.p.comp).typedef.base;
                    const nonTypedefPrevQt = prev.qt.type(self.p.comp).typedef.base;
                    try self.p.err(.redefinition_of_typedef, token, .{ nonTypedefQt, nonTypedefPrevQt });
                    if (prev.token != 0)
                        try self.p.err(.previous_definition, prev.token, .{});
                }
            },

            .enumeration, .declaration, .definition, .constexpr => {
                try self.p.err(.redefinition_different_sym, token, .{self.p.getTokenText(token)});
                try self.p.err(.previous_definition, prev.token, .{});
            },

            else => unreachable,
        }
    }
    try self.define(.{
        .kind = .typedef,
        .name = name,
        .token = token,
        .qt = qt,
        .node = .pack(node),
        .value = .{},
    });
}

/// Define a symbol in the symbol stack.
///
/// This function checks if the symbol to be defined already exists in the current scope,
/// and if it does, it reports appropriate redefinition errors.
/// The symbol is then appended to the symbol stack with its type, token, and associated value.
///
pub fn defineSymbol(
    self: *SymbolStack,
    name: StringId,
    qt: QualType,
    token: TokenIndex,
    node: Node.Index,
    val: Value,
    constexpr: bool,
) !void {
    if (self.get(name, .vars)) |prev| {
        switch (prev.kind) {
            .enumeration => {
                if (qt.isInvalid()) return;
                try self.p.err(.redefinition_different_sym, token, .{self.p.getTokenText(token)});
                try self.p.err(.previous_definition, prev.token, .{});
            },
            .declaration => {
                if (!prev.qt.isInvalid() and !qt.eqlQualified(prev.qt, self.p.comp)) {
                    if (qt.isInvalid()) return;
                    try self.p.err(.redefinition_incompatible, token, .{self.p.getTokenText(token)});
                    try self.p.err(.previous_definition, prev.token, .{});
                }
            },
            .definition, .constexpr => if (!prev.qt.isInvalid()) {
                if (qt.isInvalid()) return;
                try self.p.err(.redefinition, token, .{self.p.getTokenText(token)});
                try self.p.err(.previous_definition, prev.token, .{});
            },
            .typedef => {
                if (qt.isInvalid()) return;
                try self.p.err(.redefinition_different_sym, token, .{self.p.getTokenText(token)});
                try self.p.err(.previous_definition, prev.token, .{});
            },
            else => unreachable,
        }
    }
    try self.define(.{
        .kind = if (constexpr) .constexpr else .definition,
        .name = name,
        .token = token,
        .qt = qt,
        .node = .pack(node),
        .value = val,
    });
}

/// Get a pointer to the named symbol in the innermost scope.
/// Asserts that a symbol with the name exists.
pub fn getPtr(s: *SymbolStack, name: StringId, kind: ScopeKind) *Symbol {
    return switch (kind) {
        .tags => s.scopes.items[s.activeLen - 1].tags.getPtr(name).?,
        .vars => s.scopes.items[s.activeLen - 1].vars.getPtr(name).?,
    };
}

pub fn declareSymbol(
    self: *SymbolStack,
    name: StringId,
    qt: QualType,
    token: TokenIndex,
    node: Node.Index,
) !void {
    if (self.get(name, .vars)) |prev| {
        switch (prev.kind) {
            .enumeration => {
                if (qt.isInvalid()) return;
                try self.p.err(.redefinition_different_sym, token, .{self.p.getTokenText(token)});
                try self.p.err(.previous_definition, prev.token, .{});
            },
            .declaration => {
                if (!prev.qt.isInvalid() and !qt.eqlQualified(prev.qt, self.p.comp)) {
                    if (qt.isInvalid()) return;
                    try self.p.err(.redefinition_incompatible, token, .{self.p.getTokenText(token)});
                    try self.p.err(.previous_definition, prev.token, .{});
                }
            },
            .definition, .constexpr => {
                if (!prev.qt.isInvalid() and !qt.eqlQualified(prev.qt, self.p.comp)) {
                    if (qt.isInvalid()) return;
                    try self.p.err(.redefinition_incompatible, token, .{self.p.getTokenText(token)});
                    try self.p.err(.previous_definition, prev.token, .{});
                } else {
                    return;
                }
            },
            .typedef => {
                if (qt.isInvalid()) return;
                try self.p.err(.redefinition_different_sym, token, .{self.p.getTokenText(token)});
                try self.p.err(.previous_definition, prev.token, .{});
            },
            else => unreachable,
        }
    }
    try self.define(.{
        .kind = .declaration,
        .name = name,
        .token = token,
        .qt = qt,
        .node = .pack(node),
        .value = .{},
    });
}

pub fn defineParam(
    self: *SymbolStack,
    name: StringId,
    qt: QualType,
    token: TokenIndex,
    node: ?Node.Index,
) !void {
    if (self.get(name, .vars)) |prev| {
        switch (prev.kind) {
            .enumeration, .declaration, .definition, .constexpr => if (!prev.qt.isInvalid()) {
                if (qt.isInvalid()) return;
                try self.p.err(.redefinition_of_parameter, token, .{self.p.getTokenText(token)});
                try self.p.err(.previous_definition, prev.token, .{});
            },
            .typedef => {
                if (qt.isInvalid()) return;
                try self.p.err(.redefinition_different_sym, token, .{self.p.getTokenText(token)});
                try self.p.err(.previous_definition, prev.token, .{});
            },
            else => unreachable,
        }
    }

    try self.define(.{
        .kind = .definition,
        .name = name,
        .token = token,
        .qt = qt,
        .node = .packOpt(node),
        .value = .{},
    });
}

pub fn defineTag(
    self: *SymbolStack,
    name: StringId,
    kind: TokenType,
    token: TokenIndex,
) !?Symbol {
    const prev = self.get(name, .tags) orelse return null;
    switch (prev.kind) {
        .@"enum" => {
            if (kind == .KeywordEnum) return prev;
            try self.p.err(.wrong_tag, token, .{self.p.getTokenText(token)});
            try self.p.err(.previous_definition, prev.token, .{});
            return null;
        },
        .@"struct" => {
            if (kind == .KeywordStruct) return prev;
            try self.p.err(.wrong_tag, token, .{self.p.getTokenText(token)});
            try self.p.err(.previous_definition, prev.token, .{});
            return null;
        },
        .@"union" => {
            if (kind == .KeywordUnion) return prev;
            try self.p.err(.wrong_tag, token, .{self.p.getTokenText(token)});
            try self.p.err(.previous_definition, prev.token, .{});
            return null;
        },
        else => unreachable,
    }
}

pub fn defineEnumeration(
    self: *SymbolStack,
    name: StringId,
    qt: QualType,
    token: TokenIndex,
    value: Value,
    node: Node.Index,
) !void {
    if (self.get(name, .vars)) |prev| {
        switch (prev.kind) {
            .enumeration => if (!prev.qt.isInvalid()) {
                if (qt.isInvalid()) return;
                try self.p.err(.redefinition, token, .{self.p.getTokenText(token)});
                try self.p.err(.previous_definition, prev.token, .{});
                return;
            },
            .declaration, .definition, .constexpr => {
                if (qt.isInvalid()) return;
                try self.p.err(.redefinition_different_sym, token, .{self.p.getTokenText(token)});
                try self.p.err(.previous_definition, prev.token, .{});
                return;
            },
            .typedef => {
                if (qt.isInvalid()) return;
                try self.p.err(.redefinition_different_sym, token, .{self.p.getTokenText(token)});
                try self.p.err(.previous_definition, prev.token, .{});
                return;
            },
            else => unreachable,
        }
    }
    try self.define(.{
        .kind = .enumeration,
        .name = name,
        .token = token,
        .qt = qt,
        .value = value,
        .node = .pack(node),
    });
}
