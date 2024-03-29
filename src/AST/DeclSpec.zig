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

    if (isStatic) {
        if (isInline)
            return AstTag.InlineStaticFnDef;
        return AstTag.StaticFnDef;
    } else {
        if (isInline)
            return AstTag.InlineFnDef;
        return AstTag.FnDef;
    }
}

pub fn validate(d: DeclSpec, p: *Parser, ty: *Type, hasInit: bool) Error!AstTag {
    const isStatic = d.storageClass == .static;
    if (ty.isFunc() and d.storageClass != .typedef) {
        switch (d.storageClass) {
            .none, //
            .@"extern",
            => {},
            .static => |tokenIdx| if (p.func.type != null) try p.errToken(.static_func_not_global, tokenIdx),
            .typedef => unreachable,

            .auto, //
            .register,
            => |tokenIndex| try p.errToken(.illegal_storage_on_func, tokenIndex),
        }
        if (d.threadLocal) |tokenIndex|
            try p.errToken(.threadlocal_non_var, tokenIndex);

        const isInline = d.@"inline" != null;

        if (isStatic) {
            if (isInline)
                return AstTag.InlineStaticFnProto;
            return AstTag.StaticFnProto;
        } else {
            if (isInline)
                return AstTag.InlineFnProto;
            return AstTag.FnProto;
        }
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

        ty.qual.register = d.storageClass == .register;

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

/// Warns about ignored attributes for declarations that are enum or record types.
/// This function iterates through the attributes from a starting index and uses
/// the parser to report errors for ignored attributes.
///
/// @param d  The DeclSpec object which contains declaration specifications.
/// @param p  The Parser object which is used for error reporting.
/// @param attrBufferStart  The index to start checking for ignored attributes.
pub fn warnIgnoredAttrs(d: DeclSpec, p: *Parser, attrBufferStart: usize) !void {
    // If the type of the declaration is not an enum or record, there is nothing to do.
    if (!d.type.isEnumOrRecord())
        return;

    // Start from the given index and iterate over the attribute buffer
    var i = attrBufferStart;
    while (i < p.attrBuffer.len) : (i += 1) {
        // Get the attribute at the current index
        const ignoredAttr = p.attrBuffer.get(i);

        // Report an error for the ignored attribute with details about the tag and specifier
        try p.errExtra(.ignored_record_attr, ignoredAttr.tok, .{
            .ignoredRecordAttr = .{
                // The tag of the ignored attribute
                .tag = ignoredAttr.attr.tag,
                // Determine the specifier string based on the declaration type
                .specifier = switch (d.type.specifier) {
                    .Enum => .@"enum",
                    .Struct => .@"struct",
                    .Union => .@"union",
                    // Continue the loop if the type specifier is not one of the above
                    else => continue,
                },
            },
        });
    }
}
