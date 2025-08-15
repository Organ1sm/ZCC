const std = @import("std");

const AST = @import("AST.zig");
const TokenIndex = AST.TokenIndex;
const Compilation = @import("../Basic/Compilation.zig");
const Parser = @import("../Parser/Parser.zig");
const QualType = @import("TypeStore.zig").QualType;

pub const Error = Compilation.Error || error{ParsingFailed};
const DeclSpec = @This();

storageClass: union(enum) {
    auto: TokenIndex,
    @"extern": TokenIndex,
    register: TokenIndex,
    static: TokenIndex,
    typedef: TokenIndex,
    none,
} = .none,

threadLocal: ?TokenIndex = null,
constexpr: ?TokenIndex = null,
@"inline": ?TokenIndex = null,
noreturn: ?TokenIndex = null,
autoType: ?TokenIndex = null,
c23Auto: ?TokenIndex = null,
qt: QualType,

pub fn validateParam(d: DeclSpec, p: *Parser) Error!void {
    switch (d.storageClass) {
        .none, .register => {},
        .auto, .@"extern", .static, .typedef => |tokenIndex| try p.err(.invalid_storage_on_param, tokenIndex, .{}),
    }

    if (d.threadLocal) |tokenIndex| try p.err(.threadlocal_non_var, tokenIndex, .{});
    if (d.@"inline") |tokenIndex| try p.err(.func_spec_non_func, tokenIndex, .{"inline"});
    if (d.noreturn) |tokenIndex| try p.err(.func_spec_non_func, tokenIndex, .{"_Noreturn"});
    if (d.constexpr) |tokenIndex| try p.err(.invalid_storage_on_param, tokenIndex, .{});
}

pub fn validateFnDef(d: DeclSpec, p: *Parser) Error!void {
    switch (d.storageClass) {
        .none, .@"extern", .static => {},
        .auto, .register, .typedef => |index| try p.err(.illegal_storage_on_func, index, .{}),
    }

    if (d.threadLocal) |tokenIdx| try p.err(.threadlocal_non_var, tokenIdx, .{});
    if (d.constexpr) |tokenIdx| try p.err(.illegal_storage_on_func, tokenIdx, .{});
}

pub fn validateFnDecl(d: DeclSpec, p: *Parser) Error!void {
    switch (d.storageClass) {
        .none, .@"extern" => {},
        .static => |tokenIdx| if (p.func.qt != null) try p.err(.static_func_not_global, tokenIdx, .{}),
        .typedef => unreachable,
        .auto, .register => |tokenIdx| try p.err(.illegal_storage_on_func, tokenIdx, .{}),
    }

    if (d.threadLocal) |tokenIdx| try p.err(.threadlocal_non_var, tokenIdx, .{});
    if (d.constexpr) |tokenIdx| try p.err(.illegal_storage_on_func, tokenIdx, .{});
}

pub fn validateDecl(d: DeclSpec, p: *Parser) Error!void {
    if (d.@"inline") |tokenIdx| try p.err(.func_spec_non_func, tokenIdx, .{"inline"});
    if (d.noreturn) |tokenIdx| try p.err(.func_spec_non_func, tokenIdx, .{"_Noreturn"});

    switch (d.storageClass) {
        .auto => std.debug.assert(!p.comp.langOpts.standard.atLeast(.c23)),
        .register => if (p.func.qt == null) try p.err(.illegal_storage_on_global, p.tokenIdx, .{}),
        else => {},
    }
}

pub fn initContext(d: DeclSpec, p: *Parser) Parser.InitContext {
    if (d.constexpr != null) return .constexpr;
    if (p.func.qt == null or d.storageClass == .static) return .static;
    return .runtime;
}
