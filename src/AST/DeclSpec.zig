const AST = @import("AST.zig");
const TokenIndex = AST.TokenIndex;
const AstTag = @import("AstTag.zig").Tag;
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
}

pub fn validateFnDef(d: DeclSpec, p: *Parser) Error!AstTag {
    switch (d.storageClass) {
        .none, .@"extern", .static => {},
        .auto, .register, .typedef => |index| try p.errToken(.illegal_storage_on_func, index),
    }

    if (d.threadLocal) |tokenIdx|
        try p.errToken(.threadlocal_non_var, tokenIdx);

    if (d.constexpr) |tokenIdx|
        try p.errToken(.illegal_storage_on_func, tokenIdx);

    const isStatic = d.storageClass == .static;
    const isInline = d.@"inline" != null;
    if (isStatic) {
        return if (isInline) AstTag.InlineStaticFnDef else AstTag.StaticFnDef;
    } else {
        return if (isInline) AstTag.InlineFnDef else AstTag.FnDef;
    }
}

pub fn validate(d: DeclSpec, p: *Parser, ty: *Type, hasInit: bool) Error!AstTag {
    const isStatic = d.storageClass == .static;
    if (ty.isFunc() and d.storageClass != .typedef) {
        switch (d.storageClass) {
            .none, .@"extern" => {},
            .static => |tokenIdx| if (p.func.type != null) try p.errToken(.static_func_not_global, tokenIdx),
            .typedef => unreachable,
            .auto, .register => |tokenIndex| try p.errToken(.illegal_storage_on_func, tokenIndex),
        }

        if (d.threadLocal) |tokenIndex|
            try p.errToken(.threadlocal_non_var, tokenIndex);

        if (d.constexpr) |tokenIdx|
            try p.errToken(.illegal_storage_on_func, tokenIdx);

        const isInline = d.@"inline" != null;
        if (isStatic)
            return if (isInline) AstTag.InlineStaticFnProto else AstTag.StaticFnProto
        else
            return if (isInline) AstTag.InlineFnProto else AstTag.FnProto;
    } else {
        if (d.@"inline") |tokenIndex|
            try p.errStr(.func_spec_non_func, tokenIndex, "inline");

        if (d.noreturn) |tokenIndex|
            try p.errStr(.func_spec_non_func, tokenIndex, "_Noreturn");

        switch (d.storageClass) {
            .auto, .register => if (p.func.type == null) try p.err(.illegal_storage_on_global),
            .typedef => return AstTag.TypeDef,
            else => {},
        }

        ty.qual.register = (d.storageClass == .register);

        const isExtern = d.storageClass == .@"extern" and !hasInit;
        if (d.threadLocal != null) {
            if (isStatic) return AstTag.ThreadlocalStaticVar;
            if (isExtern) return AstTag.ThreadlocalExternVar;
            return AstTag.ThreadlocalVar;
        } else {
            if (isStatic) return AstTag.StaticVar;
            if (isExtern) return AstTag.ExternVar;
            return AstTag.Var;
        }
    }
}
