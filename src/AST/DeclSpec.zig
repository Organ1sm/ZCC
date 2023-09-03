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
@"inline": ?TokenIndex = null,
noreturn: ?TokenIndex = null,
type: Type = .{ .specifier = undefined },

pub fn validateParam(d: DeclSpec, p: *Parser, ty: Type) Error!void {
    _ = ty;
    switch (d.storageClass) {
        .none, .register => {},
        .auto, .@"extern", .static, .typedef => |tokenIndex| try p.errToken(.invalid_storage_on_param, tokenIndex),
    }

    if (d.threadLocal) |tokenIndex| try p.errToken(.threadlocal_non_var, tokenIndex);
    if (d.@"inline") |tokenIndex| try p.errStr(.func_spec_non_func, tokenIndex, "inline");
    if (d.noreturn) |tokenIndex| try p.errStr(.func_spec_non_func, tokenIndex, "_Noreturn");
}

pub fn validateFnDef(d: DeclSpec, p: *Parser) Error!AstTag {
    switch (d.storageClass) {
        .none, //
        .@"extern",
        .static,
        => {},

        .auto, //
        .register,
        .typedef,
        => |index| try p.errToken(.illegal_storage_on_func, index),
    }

    if (d.threadLocal) |index|
        try p.errToken(.threadlocal_non_var, index);

    const isStatic = d.storageClass == .static;
    const isInline = d.@"inline" != null;
    const isNoreturn = d.noreturn != null;

    if (isStatic) {
        if (isInline and isNoreturn)
            return AstTag.NoreturnInlineStaticFnDef;
        if (isInline)
            return AstTag.InlineStaticFnDef;
        if (isNoreturn)
            return AstTag.NoreturnStaticFnDef;

        return AstTag.StaticFnDef;
    } else {
        if (isInline and isNoreturn)
            return AstTag.NoreturnInlineFnDef;
        if (isInline)
            return AstTag.InlineFnDef;
        if (isNoreturn)
            return AstTag.NoreturnFnDef;

        return AstTag.FnDef;
    }
}

pub fn validate(d: DeclSpec, p: *Parser, ty: Type, hasInit: bool) Error!AstTag {
    const isStatic = d.storageClass == .static;
    if (ty.isFunc() and d.storageClass != .typedef) {
        switch (d.storageClass) {
            .none, //
            .@"extern",
            => {},
            .static => |tokenIdx| if (p.returnType != null) try p.errToken(.static_func_not_global, tokenIdx),
            .typedef => unreachable,

            .auto, //
            .register,
            => |tokenIndex| try p.errToken(.illegal_storage_on_func, tokenIndex),
        }
        if (d.threadLocal) |tokenIndex|
            try p.errToken(.threadlocal_non_var, tokenIndex);

        const isInline = d.@"inline" != null;
        const isNoreturn = d.noreturn != null;

        if (isStatic) {
            if (isInline and isNoreturn)
                return AstTag.NoreturnInlineStaticFnProto;
            if (isInline)
                return AstTag.InlineStaticFnProto;
            if (isNoreturn)
                return AstTag.NoreturnStaticFnProto;

            return AstTag.StaticFnProto;
        } else {
            if (isInline and isNoreturn)
                return AstTag.NoreturnInlineFnProto;
            if (isInline)
                return AstTag.InlineFnProto;
            if (isNoreturn) return AstTag.NoreturnFnProto;

            return AstTag.FnProto;
        }
    } else {
        if (d.@"inline") |tokenIndex|
            try p.errStr(.func_spec_non_func, tokenIndex, "inline");
        if (d.noreturn) |tokenIndex|
            try p.errStr(.func_spec_non_func, tokenIndex, "_Noreturn");
        switch (d.storageClass) {
            .auto, .register => if (p.returnType == null) try p.err(.illegal_storage_on_global),
            .typedef => return AstTag.TypeDef,
            else => {},
        }

        const isExtern = d.storageClass == .@"extern" and !hasInit;
        if (d.threadLocal != null) {
            if (isStatic)
                return AstTag.ThreadlocalStaticVar;
            if (isExtern)
                return AstTag.ThreadlocalExternVar;

            return AstTag.ThreadlocalVar;
        } else {
            if (isStatic) return AstTag.StaticVar;
            if (isExtern) return AstTag.ExternVar;
            return AstTag.Var;
        }
    }
}
