const std = @import("std");
const assert = std.debug.assert;
const Tree = @import("../AST/AST.zig");
const Type = @import("../AST/Type.zig");
const Parser = @import("../Parser/Parser.zig");
const Value = @import("../AST/Value.zig");
const TokenType = @import("../Basic/TokenType.zig").TokenType;

const mem = std.mem;
const Allocator = mem.Allocator;
const Token = Tree.Token;
const TokenIndex = Tree.TokenIndex;
const NodeIndex = Tree.NodeIndex;

const SymbolStack = @This();

pub const Symbol = struct {
    /// The name of the symbol.
    name: []const u8,
    /// The type of the symbol.
    type: Type,
    /// The token index that represents this symbol.
    token: TokenIndex,
    /// The node index in the AST (Abstract Syntax Tree) that represents this symbol.
    node: NodeIndex = .none,
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
};
// zig fmt: on

symbols: std.MultiArrayList(Symbol) = .{},
scopes: std.ArrayListUnmanaged(u32) = .{},
p: *Parser = undefined,

pub fn deinit(self: *SymbolStack, gpa: Allocator) void {
    self.symbols.deinit(gpa);
    self.scopes.deinit(gpa);
    self.p = undefined;
    self.* = undefined;
}

pub fn scopeEnd(self: SymbolStack) u32 {
    if (self.scopes.items.len == 0) return 0;
    return self.scopes.items[self.scopes.items.len - 1];
}

pub fn pushScope(self: *SymbolStack) !void {
    try self.scopes.append(self.p.pp.comp.gpa, @intCast(self.symbols.len));
}

pub fn popScope(self: *SymbolStack) void {
    self.symbols.len = self.scopes.pop();
}

pub fn appendSymbol(self: *SymbolStack, symbol: Symbol) !void {
    try self.symbols.append(self.p.pp.comp.gpa, symbol);
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
pub fn findTypedef(self: *SymbolStack, nameToken: TokenIndex, noTypeYet: bool) !?Symbol {
    const name = self.p.getTokenSlice(nameToken);
    const kinds = self.symbols.items(.kind);
    const names = self.symbols.items(.name);
    var i = self.symbols.len;
    while (i > 0) {
        i -= 1;
        switch (kinds[i]) {
            // If it's a typedef and names match, return the symbol.
            .typedef => if (mem.eql(u8, names[i], name)) return self.symbols.get(i),

            // For struct, union, and enum, check if the type should not be defined yet.
            // If it should, report an error. Otherwise, return null or the symbol.
            .@"struct", .@"union", .@"enum" => if (mem.eql(u8, names[i], name)) {
                if (noTypeYet) return null;
                try self.p.errStr(switch (kinds[i]) {
                    .@"struct" => .must_use_struct,
                    .@"union" => .must_use_union,
                    .@"enum" => .must_use_enum,
                    else => unreachable,
                }, nameToken, name);
                return self.symbols.get(i);
            },

            // If it's a definition or declaration, return null.
            .definition, .declaration => if (mem.eql(u8, names[i], name)) return null,
            else => {},
        }
    }
    return null;
}

/// Find a symbol by its name token within the current symbol stack.
/// @param  self       The SymbolStack instance.
/// @param  nameToken  The token index used to identify the symbol's name.
/// @return            A nullable Symbol if found, or null if not found.
pub fn findSymbol(self: *SymbolStack, nameToken: TokenIndex) ?Symbol {
    const name = self.p.getTokenSlice(nameToken);
    const kinds = self.symbols.items(.kind);
    const names = self.symbols.items(.name);

    var i = self.symbols.len;
    while (i > 0) {
        i -= 1;

        switch (kinds[i]) {
            .definition, .declaration, .enumeration => if (mem.eql(u8, names[i], name)) return self.symbols.get(i),
            else => {},
        }
    }

    return null;
}

/// Find a tag symbol within the symbol stack by its kind and name.
/// @param self       The SymbolStack instance.
/// @param kind       The TokenType representing the kind of tag (enum, struct, union).
/// @param nameToken  The token index used to identify the symbol's name.
/// @return           A nullable Symbol if a matching tag is found, or null otherwise.
pub fn findTag(self: *SymbolStack, kind: TokenType, nameToken: TokenIndex) !?Symbol {
    const name = self.p.getTokenSlice(nameToken);
    const kinds = self.symbols.items(.kind);
    const names = self.symbols.items(.name);

    var i = self.symbols.len;
    while (i > 0) {
        i -= 1;
        switch (kinds[i]) {
            .@"enum" => if (mem.eql(u8, names[i], name)) {
                if (kind == .KeywordEnum) return self.symbols.get(i);
                break;
            },
            .@"struct" => if (mem.eql(u8, names[i], name)) {
                if (kind == .KeywordStruct) return self.symbols.get(i);
                break;
            },
            .@"union" => if (mem.eql(u8, names[i], name)) {
                if (kind == .KeywordUnion) return self.symbols.get(i);
                break;
            },
            else => {},
        }
    }
    
    // If we've exited the loop because i reached 0, return null indicating no symbol was found.
    if (i <= self.scopeEnd()) return null;
    
    // If we've reached this point, the symbol was found but did not match the kind. Report an error.
    try self.p.errStr(.wrong_tag, nameToken, name);
    try self.p.errToken(.previous_definition, self.symbols.items(.token)[i]);
    
    // Return null as no matching symbol was found.
    return null;
}

pub fn defineTypedef(
    self: *SymbolStack,
    name: []const u8,
    ty: Type,
    token: TokenIndex,
    node: NodeIndex,
) !void {
    const kinds = self.symbols.items(.kind);
    const names = self.symbols.items(.name);
    const end = self.scopeEnd();
    var i = self.symbols.len;
    while (i > end) {
        i -= 1;
        switch (kinds[i]) {
            .typedef => if (mem.eql(u8, names[i], name)) {
                const prevTy = self.symbols.items(.type)[i];
                if (ty.eql(prevTy, self.p.pp.comp, true))
                    break;
                try self.p.errStr(.redefinition_of_typedef, token, try self.p.typePairStrExtra(ty, " vs ", prevTy));
                const prevToken = self.symbols.items(.token)[i];
                if (prevToken != 0)
                    try self.p.errToken(.previous_definition, prevToken);
                break;
            },
            else => {},
        }
    }
    try self.appendSymbol(.{
        .kind = .typedef,
        .name = name,
        .token = token,
        .type = ty,
        .node = node,
        .value = .{},
    });
}

pub fn defineSymbol(
    self: *SymbolStack,
    ty: Type,
    token: TokenIndex,
    node: NodeIndex,
    val: Value,
) !void {
    const name = self.p.getTokenSlice(token);
    const kinds = self.symbols.items(.kind);
    const names = self.symbols.items(.name);
    const end = self.scopeEnd();
    var i = self.symbols.len;
    while (i > end) {
        i -= 1;
        switch (kinds[i]) {
            .enumeration => if (mem.eql(u8, names[i], name)) {
                try self.p.errStr(.redefinition_different_sym, token, name);
                try self.p.errToken(.previous_definition, self.symbols.items(.token)[i]);
                break;
            },
            .declaration => if (mem.eql(u8, names[i], name)) {
                const prevTy = self.symbols.items(.type)[i];
                if (!ty.eql(prevTy, self.p.pp.comp, true)) { // TODO adjusted equality check
                    try self.p.errStr(.redefinition_incompatible, token, name);
                    try self.p.errToken(.previous_definition, self.symbols.items(.token)[i]);
                }
                break;
            },
            .definition => if (mem.eql(u8, names[i], name)) {
                try self.p.errStr(.redefinition, token, name);
                try self.p.errToken(.previous_definition, self.symbols.items(.token)[i]);
                break;
            },
            else => {},
        }
    }
    try self.appendSymbol(.{
        .kind = .definition,
        .name = name,
        .token = token,
        .type = ty,
        .node = node,
        .value = val,
    });
}

pub fn declareSymbol(
    self: *SymbolStack,
    ty: Type,
    token: TokenIndex,
    node: NodeIndex,
) !void {
    const name = self.p.getTokenSlice(token);
    const kinds = self.symbols.items(.kind);
    const names = self.symbols.items(.name);
    const end = self.scopeEnd();
    var i = self.symbols.len;
    while (i > end) {
        i -= 1;
        switch (kinds[i]) {
            .enumeration => if (mem.eql(u8, names[i], name)) {
                try self.p.errStr(.redefinition_different_sym, token, name);
                try self.p.errToken(.previous_definition, self.symbols.items(.token)[i]);
                break;
            },
            .declaration => if (mem.eql(u8, names[i], name)) {
                const prevTy = self.symbols.items(.type)[i];
                if (!ty.eql(prevTy, self.p.pp.comp, true)) { // TODO adjusted equality check
                    try self.p.errStr(.redefinition_incompatible, token, name);
                    try self.p.errToken(.previous_definition, self.symbols.items(.token)[i]);
                }
                break;
            },
            .definition => if (mem.eql(u8, names[i], name)) {
                const prevTy = self.symbols.items(.type)[i];
                if (!ty.eql(prevTy, self.p.pp.comp, true)) { // TODO adjusted equality check
                    try self.p.errStr(.redefinition_incompatible, token, name);
                    try self.p.errToken(.previous_definition, self.symbols.items(.token)[i]);
                    break;
                }
                return;
            },
            else => {},
        }
    }
    try self.appendSymbol(.{
        .kind = .declaration,
        .name = name,
        .token = token,
        .type = ty,
        .node = node,
        .value = .{},
    });
}

pub fn defineParam(self: *SymbolStack, ty: Type, token: TokenIndex) !void {
    const name = self.p.getTokenSlice(token);
    const kinds = self.symbols.items(.kind);
    const names = self.symbols.items(.name);
    const end = self.scopeEnd();
    var i = self.symbols.len;
    while (i > end) {
        i -= 1;
        switch (kinds[i]) {
            .enumeration, .declaration, .definition => if (mem.eql(u8, names[i], name)) {
                try self.p.errStr(.redefinition_of_parameter, token, name);
                try self.p.errToken(.previous_definition, self.symbols.items(.token)[i]);
                break;
            },
            else => {},
        }
    }
    try self.appendSymbol(.{
        .kind = .definition,
        .name = name,
        .token = token,
        .type = ty,
        .value = .{},
    });
}

pub fn defineTag(
    self: *SymbolStack,
    kind: TokenType,
    token: TokenIndex,
) !?Symbol {
    const name = self.p.getTokenSlice(token);
    const kinds = self.symbols.items(.kind);
    const names = self.symbols.items(.name);
    const end = self.scopeEnd();
    var i = self.symbols.len;
    while (i > end) {
        i -= 1;
        switch (kinds[i]) {
            .@"enum" => if (mem.eql(u8, names[i], name)) {
                if (kind == .KeywordEnum) return self.symbols.get(i);
                try self.p.errStr(.wrong_tag, token, name);
                try self.p.errToken(.previous_definition, self.symbols.items(.token)[i]);
                return null;
            },
            .@"struct" => if (mem.eql(u8, names[i], name)) {
                if (kind == .KeywordStruct) return self.symbols.get(i);
                try self.p.errStr(.wrong_tag, token, name);
                try self.p.errToken(.previous_definition, self.symbols.items(.token)[i]);
                return null;
            },
            .@"union" => if (mem.eql(u8, names[i], name)) {
                if (kind == .KeywordUnion) return self.symbols.get(i);
                try self.p.errStr(.wrong_tag, token, name);
                try self.p.errToken(.previous_definition, self.symbols.items(.token)[i]);
                return null;
            },
            else => {},
        }
    }
    return null;
}

pub fn defineEnumeration(
    self: *SymbolStack,
    ty: Type,
    token: TokenIndex,
) !void {
    const name = self.p.getTokenSlice(token);
    const kinds = self.symbols.items(.kind);
    const names = self.symbols.items(.name);
    const end = self.scopeEnd();
    var i = self.symbols.len;
    while (i > end) {
        i -= 1;
        switch (kinds[i]) {
            .enumeration => if (mem.eql(u8, names[i], name)) {
                try self.p.errStr(.redefinition, token, name);
                try self.p.errToken(.previous_definition, self.symbols.items(.token)[i]);
                return;
            },
            .declaration, .definition => if (mem.eql(u8, names[i], name)) {
                try self.p.errStr(.redefinition_different_sym, token, name);
                try self.p.errToken(.previous_definition, self.symbols.items(.token)[i]);
                return;
            },
            else => {},
        }
    }
    try self.appendSymbol(.{
        .kind = .enumeration,
        .name = name,
        .token = token,
        .type = ty,
        .value = .{},
    });
}
