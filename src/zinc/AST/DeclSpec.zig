const AST = @import("AST.zig");
const TokenIndex = AST.TokenIndex;
const Type = @import("Type.zig");
const Parser = @import("../Parser/Parser.zig");
const Compilation = @import("../Basic/Compilation.zig");

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
type: Type,

pub fn validateParam(d: DeclSpec, p: *Parser, ty: *Type) Error!void {
    switch (d.storageClass) {
        .none => {},
        .register => ty.qual.register = true,
        .auto, .@"extern", .static, .typedef => |tokenIndex| try p.errToken(.invalid_storage_on_param, tokenIndex),
    }

    if (d.threadLocal) |tokenIndex| try p.errToken(.threadlocal_non_var, tokenIndex);
    if (d.@"inline") |tokenIndex| try p.errStr(.func_spec_non_func, tokenIndex, "inline");
    if (d.noreturn) |tokenIndex| try p.errStr(.func_spec_non_func, tokenIndex, "_Noreturn");
    if (d.constexpr) |tokenIndex| try p.errToken(.invalid_storage_on_param, tokenIndex);
    if (d.autoType) |tokenIndex| {
        try p.errStr(.auto_type_not_allowed, tokenIndex, "function prototype");
        ty.* = Type.Invalid;
    }
}

pub fn validateFnDef(d: DeclSpec, p: *Parser) Error!void {
    switch (d.storageClass) {
        .none, .@"extern", .static => {},
        .auto, .register, .typedef => |index| try p.errToken(.illegal_storage_on_func, index),
    }

    if (d.threadLocal) |tokenIdx| try p.errToken(.threadlocal_non_var, tokenIdx);
    if (d.constexpr) |tokenIdx| try p.errToken(.illegal_storage_on_func, tokenIdx);
}

pub fn validate(d: DeclSpec, p: *Parser, ty: *Type) Error!void {
    if (ty.isFunc() and d.storageClass != .typedef) {
        switch (d.storageClass) {
            .none, .@"extern" => {},
            .static => |tokenIdx| if (p.func.type != null) try p.errToken(.static_func_not_global, tokenIdx),
            .typedef => unreachable,
            .auto, .register => |tokenIdx| try p.errToken(.illegal_storage_on_func, tokenIdx),
        }

        if (d.threadLocal) |tokenIdx| try p.errToken(.threadlocal_non_var, tokenIdx);
        if (d.constexpr) |tokenIdx| try p.errToken(.illegal_storage_on_func, tokenIdx);
    } else {
        if (d.@"inline") |tokenIdx| try p.errStr(.func_spec_non_func, tokenIdx, "inline");
        if (d.noreturn) |tokenIdx| try p.errStr(.func_spec_non_func, tokenIdx, "_Noreturn");

        switch (d.storageClass) {
            .auto => if (p.func.type == null and !p.comp.langOpts.standard.atLeast(.c23)) {
                try p.err(.illegal_storage_on_global);
            },
            .register => if (p.func.type == null) try p.err(.illegal_storage_on_global),
            else => {},
        }
        ty.qual.register = (d.storageClass == .register);
    }
}
