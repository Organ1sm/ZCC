const std = @import("std");
const mem = std.mem;
const ZigType = std.builtin.Type;

const CallingConvention = @import("backend").CallingConvention;

const Compilation = @import("../Basic/Compilation.zig");
const Diagnostics = @import("../Basic/Diagnostics.zig");
const Parser = @import("../Parser/Parser.zig");
const QualType = @import("../AST/TypeStore.zig").QualType;
const Result = @import("../Parser/Result.zig");
const Tree = @import("../AST/AST.zig");
const TokenIndex = Tree.TokenIndex;
const Value = @import("../AST/Value.zig");

const Attribute = @This();

tag: Tag,
syntax: Syntax,
args: Arguments,

pub const Syntax = enum {
    c23,
    declspec,
    gnu,
    keyword,
};

pub const Kind = enum {
    c23,
    declspec,
    gnu,

    pub fn toSyntax(kind: Kind) Syntax {
        return switch (kind) {
            .c23 => .c23,
            .declspec => .declspec,
            .gnu => .gnu,
        };
    }
};

pub const Iterator = struct {
    source: ?struct {
        qt: QualType,
        comp: *const Compilation,
    },
    slice: []const Attribute,
    index: usize,

    pub fn initSlice(slice: []const Attribute) Iterator {
        return .{ .source = null, .slice = slice, .index = 0 };
    }

    pub fn initType(qt: QualType, comp: *const Compilation) Iterator {
        return .{ .source = .{ .qt = qt, .comp = comp }, .slice = &.{}, .index = 0 };
    }

    /// returns the next attribute as well as its index within the slice or current type
    /// The index can be used to determine when a nested type has been recursed into
    pub fn next(self: *Iterator) ?struct { Attribute, usize } {
        if (self.index < self.slice.len) {
            defer self.index += 1;
            return .{ self.slice[self.index], self.index };
        }
        if (self.source) |*source| {
            var cur = source.qt;
            // std.debug.print("cur: {}\n", .{cur._index});
            if (cur.isInvalid()) {
                self.source = null;
                return null;
            }
            while (true)
                switch (cur.type(source.comp)) {
                    .typeof => |typeof| cur = typeof.base,
                    .attributed => |attributed| {
                        self.slice = attributed.attributes;
                        self.index = 1;
                        source.qt = attributed.base;
                        return .{ self.slice[0], 0 };
                    },
                    .typedef => |typedef| cur = typedef.base,
                    else => {
                        self.source = null;
                        break;
                    },
                };
        }
        return null;
    }
};

/// number of required arguments
pub fn requiredArgCount(attr: Tag) u32 {
    switch (attr) {
        inline else => |tag| {
            comptime var needed = 0;
            comptime {
                const fields = std.meta.fields(@field(attributes, @tagName(tag)));
                for (fields) |argField| {
                    // filter the `__name_token` and nullable field
                    if (!mem.eql(u8, argField.name, "__name_token") and @typeInfo(argField.type) != .optional)
                        needed += 1;
                }
            }
            return needed;
        },
    }
}

/// maximum number of args that can be passed
pub fn maxArgCount(attr: Tag) u32 {
    switch (attr) {
        inline else => |tag| {
            comptime var max = 0;
            comptime {
                const fields = std.meta.fields(@field(attributes, @tagName(tag)));
                for (fields) |argField| {
                    if (!mem.eql(u8, argField.name, "__name_token"))
                        max += 1;
                }
            }
            return max;
        },
    }
}

fn UnwrapOptional(comptime T: type) type {
    return switch (@typeInfo(T)) {
        .optional => |optional| optional.child,
        else => T,
    };
}

pub const Formatting = struct {
    /// The quote char (single or double) to use when printing identifiers/strings corresponding
    /// to the enum in the first field of the `attr`. Identifier enums use single quotes, string enums
    /// use double quotes
    pub fn quoteChar(attr: Tag) []const u8 {
        switch (attr) {
            .calling_convention => unreachable,
            inline else => |tag| {
                const fields = std.meta.fields((@field(attributes, @tagName(tag))));
                if (fields.len == 0) unreachable;

                const Unwrapped = UnwrapOptional(fields[0].type);
                if (@typeInfo(Unwrapped) != .@"enum") unreachable;

                return if (Unwrapped.opts.enum_kind == .identifier) "'" else "\"";
            },
        }
    }

    /// returns a comma-separated string of quoted enum values, representing the valid
    /// choices for the string or identifier enum of the first field of the `attr`.
    pub fn choices(attr: Tag) []const u8 {
        switch (attr) {
            .calling_convention => unreachable,
            inline else => |tag| {
                const fields = std.meta.fields((@field(attributes, @tagName(tag))));

                if (fields.len == 0) unreachable;
                const Unwrapped = UnwrapOptional(fields[0].type);
                if (@typeInfo(Unwrapped) != .@"enum") unreachable;

                const enumFields = @typeInfo(Unwrapped).@"enum".fields;
                @setEvalBranchQuota(3000);
                const quote = comptime quoteChar(@enumFromInt(@intFromEnum(tag)));
                comptime var values: []const u8 = quote ++ enumFields[0].name ++ quote;
                inline for (enumFields[1..]) |enumField| {
                    values = values ++ ", ";
                    values = values ++ quote ++ enumField.name ++ quote;
                }
                return values;
            },
        }
    }
};

/// Checks if the first argument (if it exists) is an identifier enum
pub fn wantsIdentEnum(attr: Tag) bool {
    switch (attr) {
        .calling_convention => unreachable,
        inline else => |tag| {
            const fields = std.meta.fields(@field(attributes, @tagName(tag)));

            if (fields.len == 0) return false;
            const Unwrapped = UnwrapOptional(fields[0].type);
            if (@typeInfo(Unwrapped) != .@"enum") return false;

            return Unwrapped.opts.enum_kind == .identifier;
        },
    }
}

pub fn diagnoseIdent(attr: Tag, arguments: *Arguments, ident: TokenIndex, p: *Parser) !bool {
    switch (attr) {
        inline else => |tag| {
            // Get the fields of the Args struct corresponding to the given tag
            const fields = std.meta.fields(@field(attributes, @tagName(tag)));
            if (fields.len == 0)
                unreachable;

            const Unwrapped = UnwrapOptional(fields[0].type);
            if (@typeInfo(Unwrapped) != .@"enum") unreachable;
            if (std.meta.stringToEnum(Unwrapped, normalize(p.getTokenText(ident)))) |enumVal| {
                @field(@field(arguments, @tagName(tag)), fields[0].name) = enumVal;
                return false;
            }

            try p.err(.unknown_attr_enum, ident, .{ @tagName(attr), Formatting.choices(attr) });
            return true;
        },
    }
}

/// Returns whether the given attribute's argument at the given index wants an Alignment value.
pub fn wantsAlignment(attr: Tag, idx: usize) bool {
    switch (attr) {
        inline else => |tag| {
            const fields = std.meta.fields(@field(attributes, @tagName(tag)));
            if (fields.len == 0) return false;

            return switch (idx) {
                inline 0...fields.len - 1 => |i| UnwrapOptional(fields[i].type) == Alignment,
                else => false,
            };
        },
    }
}

/// Diagnoses an alignment attribute argument.
///
/// This function takes the attribute tag, the arguments for the attribute, the index of the
/// argument to diagnose, the value of the argument, the type of the argument, and the compilation
/// object. It returns a diagnostics message if the alignment argument is invalid, and null if it is
/// valid.
pub fn diagnoseAlignment(
    attr: Tag,
    arguments: *Arguments,
    argIdx: u32,
    res: Result,
    argStart: TokenIndex,
    p: *Parser,
) !bool {
    switch (attr) {
        inline else => |tag| {
            const argFields = std.meta.fields(@field(attributes, @tagName(tag)));
            if (argFields.len == 0) unreachable;

            switch (argIdx) {
                inline 0...argFields.len - 1 => |argI| {
                    if (UnwrapOptional(argFields[argI].type) != Alignment) unreachable;

                    if (!res.value.is(.int, p.comp)) {
                        try p.err(.alignas_unavailable, argStart, .{});
                        return true;
                    }

                    if (res.value.compare(.lt, Value.zero, p.comp)) {
                        try p.err(.negative_alignment, argStart, .{res});
                        return true;
                    }

                    const requested = res.value.toInt(u29, p.comp) orelse {
                        try p.err(.maximum_alignment, argStart, .{res});
                        return true;
                    };
                    if (!std.mem.isValidAlign(requested)) {
                        try p.err(.non_pow2_align, argStart, .{});
                        return true;
                    }

                    // Set the alignment of the argument to the requested value
                    @field(@field(arguments, @tagName(tag)), argFields[argI].name) = Alignment{ .requested = requested };
                    return false;
                },
                else => unreachable,
            }
        },
    }
}

fn diagnoseField(
    comptime decl: ZigType.Declaration,
    comptime field: ZigType.StructField,
    comptime Wanted: type,
    arguments: *Arguments,
    res: Result,
    argStart: TokenIndex,
    node: Tree.Node,
    p: *Parser,
) !bool {
    const string = "a string";
    const identifier = "an identifier";
    const int = "an integer constant";
    const alignment = "an integer constant";
    const nullptrTy = "nullptr";
    const float = "a floating point number";
    const complexFloat = "a complex floating point number";
    const expression = "an expression";

    const expected: []const u8 = switch (Wanted) {
        Value => string,
        Identifier => identifier,
        u32 => int,
        Alignment => alignment,
        CallingConvention => identifier,
        else => switch (@typeInfo(Wanted)) {
            .@"enum" => if (Wanted.opts.enum_kind == .string) string else identifier,
            else => unreachable,
        },
    };

    if (res.value.isNone()) {
        if (Wanted == Identifier and node == .declRefExpr) {
            @field(@field(arguments, decl.name), field.name) = .{ .tok = node.declRefExpr.nameToken };
            return false;
        }
        try p.err(.attribute_arg_invalid, argStart, .{ expected, expression });
        return true;
    }

    const key = p.comp.interner.get(res.value.ref());
    switch (key) {
        .int => {
            if (@typeInfo(Wanted) == .int) {
                @field(@field(arguments, decl.name), field.name) = res.value.toInt(Wanted, p.comp) orelse {
                    try p.err(.attribute_int_out_of_range, argStart, .{res});
                    return true;
                };
                return false;
            }
        },
        .bytes => |bytes| {
            if (Wanted == Value) {
                validate: {
                    if (node != .stringLiteralExpr) break :validate;
                    switch (node.stringLiteralExpr.qt.childType(p.comp).get(p.comp, .int).?) {
                        .Char, .UChar, .SChar => {},
                        else => break :validate,
                    }

                    @field(@field(arguments, decl.name), field.name) = try p.removeNull(res.value);
                    return false;
                }
                try p.err(.attribute_requires_string, argStart, .{decl.name});
                return true;
            } else if (@typeInfo(Wanted) == .@"enum" and @hasDecl(Wanted, "opts") and Wanted.opts.enum_kind == .string) {
                const str = bytes[0 .. bytes.len - 1];
                if (std.meta.stringToEnum(Wanted, str)) |enum_val| {
                    @field(@field(arguments, decl.name), field.name) = enum_val;
                    return false;
                } else {
                    try p.err(.unknown_attr_enum, argStart, .{ decl.name, Formatting.choices(@field(Tag, decl.name)) });
                    return true;
                }
            }
        },
        else => {},
    }

    try p.err(.attribute_arg_invalid, argStart, .{ expected, switch (key) {
        .int => int,
        .bytes => string,
        .float => float,
        .complex => complexFloat,
        .null => nullptrTy,
        else => unreachable,
    } });
    return true;
}

pub fn diagnose(
    attr: Tag,
    arguments: *Arguments,
    argIdx: u32,
    res: Result,
    argStart: TokenIndex,
    node: Tree.Node,
    p: *Parser,
) !bool {
    switch (attr) {
        inline else => |tag| {
            const decl = @typeInfo(attributes).@"struct".decls[@intFromEnum(tag)];
            const max_arg_count = comptime maxArgCount(tag);
            if (argIdx >= max_arg_count) {
                try p.err(.attribute_too_many_args, argStart, .{ @tagName(attr), max_arg_count });
                return true;
            }

            const argFields = std.meta.fields(@field(attributes, decl.name));
            switch (argIdx) {
                inline 0...argFields.len - 1 => |argI| {
                    return diagnoseField(decl, argFields[argI], UnwrapOptional(argFields[argI].type), arguments, res, argStart, node, p);
                },
                else => unreachable,
            }
        },
    }
}

const EnumTypes = enum {
    string,
    identifier,
};

pub const Alignment = struct {
    node: Tree.Node.OptIndex = .null,
    requested: u32,
};

pub const Identifier = struct {
    tok: TokenIndex = 0,
};

const attributes = struct {
    pub const access = struct {
        access_mode: enum {
            read_only,
            read_write,
            write_only,
            none,

            const opts = struct {
                const enum_kind = .identifier;
            };
        },
        ref_index: u32,
        size_index: ?u32 = null,
    };
    pub const alias = struct {
        alias: Value,
    };
    pub const aligned = struct {
        alignment: ?Alignment = null,
        __name_token: TokenIndex,
    };
    pub const alloc_align = struct {
        position: u32,
    };
    pub const alloc_size = struct {
        position_1: u32,
        position_2: ?u32 = null,
    };
    pub const allocate = struct {
        segname: Value,
    };
    pub const allocator = struct {};
    pub const always_inline = struct {};
    pub const appdomain = struct {};
    pub const artificial = struct {};
    pub const assume_aligned = struct {
        alignment: Alignment,
        offset: ?u32 = null,
    };
    pub const cleanup = struct {
        function: Identifier,
    };
    pub const code_seg = struct {
        segname: Value,
    };
    pub const cold = struct {};
    pub const common = struct {};
    pub const @"const" = struct {};
    pub const constructor = struct {
        priority: ?u32 = null,
    };
    pub const copy = struct {
        function: Identifier,
    };
    pub const deprecated = struct {
        msg: ?Value = null,
        __name_token: TokenIndex,
    };
    pub const designated_init = struct {};
    pub const destructor = struct {
        priority: ?u32 = null,
    };
    pub const dllexport = struct {};
    pub const dllimport = struct {};
    pub const @"error" = struct {
        msg: Value,
        __name_token: TokenIndex,
    };
    pub const externally_visible = struct {};
    pub const fallthrough = struct {};
    pub const flatten = struct {};
    pub const format = struct {
        archetype: enum {
            printf,
            scanf,
            strftime,
            strfmon,

            const opts = struct {
                const enum_kind = .identifier;
            };
        },
        string_index: u32,
        first_to_check: u32,
    };
    pub const format_arg = struct {
        string_index: u32,
    };
    pub const gnu_inline = struct {};
    pub const hot = struct {};
    pub const ifunc = struct {
        resolver: Value,
    };
    pub const interrupt = struct {};
    pub const interrupt_handler = struct {};
    pub const jitintrinsic = struct {};
    pub const leaf = struct {};
    pub const malloc = struct {};
    pub const may_alias = struct {};
    pub const mode = struct {
        mode: enum {
            // zig fmt: off
                byte,  word,  pointer,
                BI,    QI,    HI,
                PSI,   SI,    PDI,
                DI,    TI,    OI,
                XI,    QF,    HF,
                TQF,   SF,    DF,
                XF,    SD,    DD,
                TD,    TF,    QQ,
                HQ,    SQ,    DQ,
                TQ,    UQQ,   UHQ,
                USQ,   UDQ,   UTQ,
                HA,    SA,    DA,
                TA,    UHA,   USA,
                UDA,   UTA,   CC,
                BLK,   VOID,  QC,
                HC,    SC,    DC,
                XC,    TC,    CQI,
                CHI,   CSI,   CDI,
                CTI,   COI,   CPSI,
                BND32, BND64,
                // zig fmt: on

            const opts = struct {
                const enum_kind = .identifier;
            };
        },
    };
    pub const naked = struct {};
    pub const no_address_safety_analysis = struct {};
    pub const no_icf = struct {};
    pub const no_instrument_function = struct {};
    pub const no_profile_instrument_function = struct {};
    pub const no_reorder = struct {};
    pub const no_sanitize = struct {
        /// Todo: represent args as union?
        alignment: Value,
        object_size: ?Value = null,
    };
    pub const no_sanitize_address = struct {};
    pub const no_sanitize_coverage = struct {};
    pub const no_sanitize_thread = struct {};
    pub const no_sanitize_undefined = struct {};
    pub const no_split_stack = struct {};
    pub const no_stack_limit = struct {};
    pub const no_stack_protector = struct {};
    pub const @"noalias" = struct {};
    pub const noclone = struct {};
    pub const nocommon = struct {};
    pub const nodiscard = struct {};
    pub const noinit = struct {};
    pub const @"noinline" = struct {};
    pub const noipa = struct {};
    // TODO: arbitrary number of arguments
    //            arg_index: []const u32,
    //    };
    pub const nonstring = struct {};
    pub const noplt = struct {};
    pub const @"noreturn" = struct {};
    pub const nothrow = struct {};
    // TODO: union args ?
    //    const optimize = struct {
    //            optimize, // u32 | []const u8 -- optimize?
    //    };
    pub const @"packed" = struct {};
    pub const patchable_function_entry = struct {};
    pub const persistent = struct {};
    pub const process = struct {};
    pub const pure = struct {};
    pub const reproducible = struct {};
    pub const restrict = struct {};
    pub const retain = struct {};
    pub const returns_nonnull = struct {};
    pub const returns_twice = struct {};
    pub const safebuffers = struct {};
    pub const scalar_storage_order = struct {
        order: enum {
            @"little-endian",
            @"big-endian",

            const opts = struct {
                const enum_kind = .string;
            };
        },
    };
    pub const section = struct {
        name: Value,
    };
    pub const selectany = struct {};
    pub const sentinel = struct {
        position: ?u32 = null,
    };
    pub const simd = struct {
        mask: ?enum {
            notinbranch,
            inbranch,

            const opts = struct {
                const enum_kind = .string;
            };
        } = null,
    };
    pub const spectre = struct {
        arg: enum {
            nomitigation,

            const opts = struct {
                const enum_kind = .identifier;
            };
        },
    };
    pub const stack_protect = struct {};
    pub const symver = struct {
        version: Value, // TODO: validate format "name2@nodename"
    };
    pub const target = struct {
        options: Value, // TODO: multiple arguments
    };
    pub const target_clones = struct {
        options: Value, // TODO: multiple arguments
    };
    pub const thread = struct {};
    pub const tls_model = struct {
        model: enum {
            @"global-dynamic",
            @"local-dynamic",
            @"initial-exec",
            @"local-exec",

            const opts = struct {
                const enum_kind = .string;
            };
        },
    };
    pub const transparent_union = struct {};
    pub const unavailable = struct {
        msg: ?Value = null,
        __name_token: TokenIndex,
    };
    pub const uninitialized = struct {};
    pub const unsequenced = struct {};
    pub const unused = struct {};
    pub const used = struct {};
    pub const uuid = struct {
        uuid: Value,
    };
    pub const vector_size = struct {
        bytes: u32, // TODO: validate "The bytes argument must be a positive power-of-two multiple of the base type size"
    };
    pub const visibility = struct {
        visibility_type: enum {
            default,
            hidden,
            internal,
            protected,

            const opts = struct {
                const enum_kind = .string;
            };
        },
    };
    pub const warn_if_not_aligned = struct {
        alignment: Alignment,
    };
    pub const warn_unused_result = struct {};
    pub const warning = struct {
        msg: Value,
        __name_token: TokenIndex,
    };
    pub const weak = struct {};
    pub const weakref = struct {
        target: ?Value = null,
    };
    pub const zero_call_used_regs = struct {
        choice: enum {
            skip,
            used,
            @"used-gpr",
            @"used-arg",
            @"used-gpr-arg",
            all,
            @"all-gpr",
            @"all-arg",
            @"all-gpr-arg",

            const opts = struct {
                const enum_kind = .string;
            };
        },
    };
    pub const asm_label = struct {
        name: Value,
    };
    pub const calling_convention = struct {
        cc: CallingConvention,
    };
    pub const nullability = struct {
        kind: enum {
            nonnull,
            nullable,
            nullableResult,
            unspecified,

            const opts = struct {
                const enum_kind = .identifier;
            };
        },
    };
    pub const unaligned = struct {};
    pub const pcs = struct {
        kind: enum {
            aapcs,
            @"aapcs-vfp",

            const opts = struct {
                const enum_kind = .string;
            };
        },
    };
    pub const riscv_vector_cc = struct {};
    pub const aarch64_sve_pcs = struct {};
    pub const aarch64_vector_pcs = struct {};
    pub const fastcall = struct {};
    pub const stdcall = struct {};
    pub const vectorcall = struct {};
    pub const cdecl = struct {};
    pub const thiscall = struct {};
    pub const sysv_abi = struct {};
};

/// The Attributes enum tag
pub const Tag = std.meta.DeclEnum(attributes);

/// Generate a union type with fields based on the attributes' "Args" declarations.
/// This block introspects `attributes` to discover any "Args" fields within its
/// declarations and constructs a union type accordingly. Each union field corresponds
/// to an attribute declaration that contains "Args".
pub const Arguments = blk: {
    // Retrieve the declarations from the attributes type.
    const decls = @typeInfo(attributes).@"struct".decls;
    var union_fields: [decls.len]std.builtin.Type.UnionField = undefined;
    for (decls, &union_fields) |decl, *field| {
        field.* = .{
            .name = decl.name,
            .type = @field(attributes, decl.name),
            .alignment = @alignOf(@field(attributes, decl.name)),
        };
    }

    // Construct and return the union type with the fields we've just populated.
    break :blk @Type(.{
        .@"union" = .{
            .layout = .auto, // The layout of the union is automatically determined.
            .tag_type = null, // The union is untagged (no explicit tag type).
            .fields = &union_fields, // Specify the fields of the union.
            .decls = &.{}, // No additional declarations are provided.
        },
    });
};

pub fn ArgumentsForTag(comptime tag: Tag) type {
    const decl = @typeInfo(attributes).@"struct".decls[@intFromEnum(tag)];
    return @field(attributes, decl.name);
}

pub fn initArguments(tag: Tag, nameToken: TokenIndex) Arguments {
    switch (tag) {
        inline else => |argTag| {
            const tagName = @tagName(argTag);
            const unionElement = @field(attributes, tagName); // get nested field
            const init = std.mem.zeroInit(unionElement, .{});

            var args = @unionInit(Arguments, tagName, init);
            if (@hasField(unionElement, "__name_token")) {
                @field(args, tagName).__name_token = nameToken;
            }
            return args;
        },
    }
}

pub fn fromString(kind: Kind, namespace: ?[]const u8, name: []const u8) ?Tag {
    const Properties = struct {
        tag: Tag,
        gnu: bool = false,
        declspec: bool = false,
        c23: bool = false,
    };
    const attributesNames = @import("Attribute/names.def").with(Properties);

    const normalized = normalize(name);
    const actualKind: Kind = if (namespace) |ns| blk: {
        const normalizedNs = normalize(ns);
        if (mem.eql(u8, normalizedNs, "gnu")) {
            break :blk .gnu;
        }
        return null;
    } else kind;

    const tagAndOpts = attributesNames.fromName(normalized) orelse return null;
    switch (actualKind) {
        inline else => |tag| {
            if (@field(tagAndOpts.properties, @tagName(tag)))
                return tagAndOpts.properties.tag;
        },
    }
    return null;
}

/// Normalize an attribute name by removing the leading and trailing double underscores.
/// @param name The attribute name to normalize.
pub fn normalize(name: []const u8) []const u8 {
    if (name.len >= 4 and mem.startsWith(u8, name, "__") and mem.endsWith(u8, name, "__")) {
        return name[2 .. name.len - 2];
    }
    return name;
}

fn ignoredAttrErr(p: *Parser, token: TokenIndex, attr: Attribute.Tag, context: []const u8) !void {
    try p.err(.ignored_attribute, token, .{ @tagName(attr), context });
}

pub const applyParameterAttributes = applyVariableAttributes;

pub fn applyVariableAttributes(p: *Parser, qt: QualType, attrBufferStart: usize, diag: ?Parser.Diagnostic) !QualType {
    const gpa = p.comp.gpa;

    const attrs = p.attrBuffer.items(.attr)[attrBufferStart..];
    const toks = p.attrBuffer.items(.tok)[attrBufferStart..];
    p.attrApplicationBuffer.items.len = 0;

    var baseQt = qt;
    var common = false;
    var nocommon = false;
    for (attrs, toks) |attr, tok| switch (attr.tag) {
        // zig fmt: off
        .alias, .may_alias, .deprecated, .unavailable, .unused, .warn_if_not_aligned, .weak, .used,
        .noinit, .retain, .persistent, .section, .mode, .asm_label, .nullability, .unaligned,
         => try p.attrApplicationBuffer.append(gpa, attr),
        // zig fmt: on

        .common => if (nocommon) {
            try p.err(.ignore_common, tok, .{});
        } else {
            try p.attrApplicationBuffer.append(gpa, attr);
            common = true;
        },

        .nocommon => if (common) {
            try p.err(.ignore_nocommon, tok, .{});
        } else {
            try p.attrApplicationBuffer.append(gpa, attr);
            nocommon = true;
        },

        .vector_size => try attr.applyVectorSize(p, tok, &baseQt),
        .aligned => try attr.applyAligned(p, baseQt, diag),

        .nonstring => {
            if (baseQt.get(p.comp, .array)) |arrayTy| {
                if (arrayTy.elem.get(p.comp, .int)) |intTy| switch (intTy) {
                    .Char, .UChar, .SChar => {
                        try p.attrApplicationBuffer.append(gpa, attr);
                        continue;
                    },
                    else => {},
                };
            }
            try p.err(.non_string_ignored, tok, .{qt});
        },
        .uninitialized => if (p.func.qt == null) {
            try p.err(.local_variable_attribute, tok, .{"uninitialized"});
        } else {
            try p.attrApplicationBuffer.append(gpa, attr);
        },

        .cleanup => if (p.func.qt == null) {
            try p.err(.local_variable_attribute, tok, .{"cleanup"});
        } else {
            try p.attrApplicationBuffer.append(gpa, attr);
        },

        .calling_convention => try applyCallingConvention(attr, p, tok, baseQt),

        .alloc_size,
        .copy,
        .tls_model,
        .visibility,
        => |t| try p.err(.attribute_todo, tok, .{ @tagName(t), "variables" }),

        .noreturn => if (attr.syntax != .keyword) try ignoredAttrErr(p, tok, attr.tag, "variables"),
        else => try ignoredAttrErr(p, tok, attr.tag, "variables"),
    };

    return applySelected(baseQt, p);
}

pub fn applyFieldAttributes(p: *Parser, fieldQt: *QualType, attrBufferStart: usize) ![]const Attribute {
    const attrs = p.attrBuffer.items(.attr)[attrBufferStart..];
    const toks = p.attrBuffer.items(.tok)[attrBufferStart..];
    p.attrApplicationBuffer.items.len = 0;

    for (attrs, toks) |attr, tok| switch (attr.tag) {
        // zig fmt: off
        .@"packed", .may_alias, .deprecated, .unavailable, .unused, .warn_if_not_aligned,
        .mode,.warn_unused_result, .nodiscard, .nullability, .unaligned,
        => try p.attrApplicationBuffer.append(p.comp.gpa, attr),
        // zig fmt: on

        .vector_size => try attr.applyVectorSize(p, tok, fieldQt),
        .aligned => try attr.applyAligned(p, fieldQt.*, null),
        else => try ignoredAttrErr(p, tok, attr.tag, "fields"),
    };

    return p.attrApplicationBuffer.items;
}

pub fn applyTypeAttributes(p: *Parser, qt: QualType, attrBufferStart: usize, diag: ?Parser.Diagnostic) !QualType {
    const gpa = p.comp.gpa;
    const attrs = p.attrBuffer.items(.attr)[attrBufferStart..];
    const toks = p.attrBuffer.items(.tok)[attrBufferStart..];
    p.attrApplicationBuffer.items.len = 0;

    var baseQt = qt;
    for (attrs, toks) |attr, tok| switch (attr.tag) {
        .@"packed",
        .may_alias,
        .deprecated,
        .unavailable,
        .unused,
        .warn_if_not_aligned,
        .mode,
        .nullability,
        .unaligned,
        => try p.attrApplicationBuffer.append(gpa, attr),

        .transparent_union => try attr.applyTransparentUnion(p, tok, baseQt),
        .vector_size => try attr.applyVectorSize(p, tok, &baseQt),
        .aligned => try attr.applyAligned(p, baseQt, diag),

        .designated_init => if (baseQt.is(p.comp, .@"struct")) {
            try p.attrApplicationBuffer.append(gpa, attr);
        } else {
            try p.err(.designated_init_invalid, tok, .{});
        },

        .calling_convention => try applyCallingConvention(attr, p, tok, baseQt),
        .alloc_size,
        .copy,
        .scalar_storage_order,
        .nonstring,
        => |t| try p.err(.attribute_todo, tok, .{ .attributeTodo = .{ @tagName(t), "types" } }),

        else => try ignoredAttrErr(p, tok, attr.tag, "types"),
    };

    return applySelected(baseQt, p);
}

pub fn applyFunctionAttributes(p: *Parser, qt: QualType, attrBufferStart: usize) !QualType {
    const gpa = p.comp.gpa;
    const attrs = p.attrBuffer.items(.attr)[attrBufferStart..];
    const toks = p.attrBuffer.items(.tok)[attrBufferStart..];
    p.attrApplicationBuffer.items.len = 0;

    const baseQt = qt;
    var hot = false;
    var cold = false;
    var @"noinline" = false;
    var alwaysInline = false;
    for (attrs, toks) |attr, tok| switch (attr.tag) {
        // zig fmt: off
        .noreturn, .unused, .used, .warning, .deprecated, .unavailable, .weak, .pure, .leaf,
        .@"const", .warn_unused_result, .section, .returns_nonnull, .returns_twice, .@"error",
        .externally_visible, .retain, .flatten, .gnu_inline, .alias, .asm_label, .nodiscard,
        .reproducible, .unsequenced, .nothrow, .nullability, .unaligned, 
         => try p.attrApplicationBuffer.append(gpa, attr),
        // zig fmt: on

        .hot => if (cold) {
            try p.err(.ignore_hot, tok, .{});
        } else {
            try p.attrApplicationBuffer.append(gpa, attr);
            hot = true;
        },

        .cold => if (hot) {
            try p.err(.ignore_cold, tok, .{});
        } else {
            try p.attrApplicationBuffer.append(gpa, attr);
            cold = true;
        },

        .always_inline => if (@"noinline") {
            try p.err(.ignore_always_inline, tok, .{});
        } else {
            try p.attrApplicationBuffer.append(gpa, attr);
            alwaysInline = true;
        },

        .@"noinline" => if (alwaysInline) {
            try p.err(.ignore_noinline, tok, .{});
        } else {
            try p.attrApplicationBuffer.append(gpa, attr);
            @"noinline" = true;
        },

        .aligned => try attr.applyAligned(p, baseQt, null),
        .format => try attr.applyFormat(p, baseQt),

        .calling_convention => try applyCallingConvention(attr, p, tok, baseQt),

        .fastcall => if (p.comp.target.cpu.arch == .x86) {
            try p.attrApplicationBuffer.append(gpa, .{
                .tag = .calling_convention,
                .args = .{ .calling_convention = .{ .cc = .fastcall } },
                .syntax = attr.syntax,
            });
        } else {
            try p.err(.callconv_not_supported, tok, .{"fastcall"});
        },
        .stdcall => if (p.comp.target.cpu.arch == .x86) {
            try p.attrApplicationBuffer.append(gpa, .{
                .tag = .calling_convention,
                .args = .{ .calling_convention = .{ .cc = .stdcall } },
                .syntax = attr.syntax,
            });
        } else {
            try p.err(.callconv_not_supported, tok, .{"stdcall"});
        },
        .thiscall => if (p.comp.target.cpu.arch == .x86) {
            try p.attrApplicationBuffer.append(gpa, .{
                .tag = .calling_convention,
                .args = .{ .calling_convention = .{ .cc = .thiscall } },
                .syntax = attr.syntax,
            });
        } else {
            try p.err(.callconv_not_supported, tok, .{"thiscall"});
        },
        .vectorcall => if (p.comp.target.cpu.arch == .x86 or p.comp.target.cpu.arch.isAARCH64()) {
            try p.attrApplicationBuffer.append(gpa, .{
                .tag = .calling_convention,
                .args = .{ .calling_convention = .{ .cc = .vectorcall } },
                .syntax = attr.syntax,
            });
        } else {
            try p.err(.callconv_not_supported, tok, .{"vectorcall"});
        },
        .cdecl => {},

        .pcs => if (p.comp.target.cpu.arch.isArm()) {
            try p.attrApplicationBuffer.append(gpa, .{
                .tag = .calling_convention,
                .args = .{
                    .calling_convention = .{
                        .cc = switch (attr.args.pcs.kind) {
                            .aapcs => .arm_aapcs,
                            .@"aapcs-vfp" => .arm_aapcs_vfp,
                        },
                    },
                },
                .syntax = attr.syntax,
            });
        } else {
            try p.err(.callconv_not_supported, tok, .{"pcs"});
        },

        .riscv_vector_cc => if (p.comp.target.cpu.arch.isRISCV()) {
            try p.attrApplicationBuffer.append(gpa, .{
                .tag = .calling_convention,
                .args = .{ .calling_convention = .{ .cc = .riscv_vector } },
                .syntax = attr.syntax,
            });
        } else {
            try p.err(.callconv_not_supported, tok, .{"riscv_vector_cc"});
        },

        .aarch64_sve_pcs => if (p.comp.target.cpu.arch.isAARCH64()) {
            try p.attrApplicationBuffer.append(gpa, .{
                .tag = .calling_convention,
                .args = .{ .calling_convention = .{ .cc = .aarch64_sve_pcs } },
                .syntax = attr.syntax,
            });
        } else {
            try p.err(.callconv_not_supported, tok, .{"pcs"});
        },

        .aarch64_vector_pcs => if (p.comp.target.cpu.arch.isAARCH64()) {
            try p.attrApplicationBuffer.append(gpa, .{
                .tag = .calling_convention,
                .args = .{ .calling_convention = .{ .cc = .aarch64_vector_pcs } },
                .syntax = attr.syntax,
            });
        } else {
            try p.err(.callconv_not_supported, tok, .{"pcs"});
        },

        .alloc_align => {
            const funcTy = baseQt.get(p.comp, .func).?;
            if (funcTy.returnType.isPointer(p.comp)) {
                if (attr.args.alloc_align.position == 0 or attr.args.alloc_align.position > funcTy.params.len) {
                    try p.err(.attribute_param_out_of_bounds, tok, .{ "alloc_align", 1 });
                } else {
                    const argQt = funcTy.params[attr.args.alloc_align.position - 1].qt;
                    if (argQt.isInvalid()) continue;
                    const argSk = argQt.scalarKind(p.comp);
                    if (!argSk.isInt() or !argSk.isReal()) {
                        try p.err(.alloc_align_required_int_param, tok, .{});
                    } else {
                        try p.attrApplicationBuffer.append(gpa, attr);
                    }
                }
            } else {
                try p.err(.alloc_align_requires_ptr_return, tok, .{});
            }
        },

        .access,
        .alloc_size,
        .artificial,
        .assume_aligned,
        .constructor,
        .copy,
        .destructor,
        .format_arg,
        .ifunc,
        .interrupt,
        .interrupt_handler,
        .malloc,
        .no_address_safety_analysis,
        .no_icf,
        .no_instrument_function,
        .no_profile_instrument_function,
        .no_reorder,
        .no_sanitize,
        .no_sanitize_address,
        .no_sanitize_coverage,
        .no_sanitize_thread,
        .no_sanitize_undefined,
        .no_split_stack,
        .no_stack_limit,
        .no_stack_protector,
        .noclone,
        .noipa,
        // .nonnull,
        .noplt,
        // .optimize,
        .patchable_function_entry,
        .sentinel,
        .simd,
        .stack_protect,
        .symver,
        .target,
        .target_clones,
        .visibility,
        .weakref,
        .zero_call_used_regs,
        => |t| try p.err(.attribute_todo, tok, .{ .attributeTodo = .{ @tagName(t), "functions" } }),
        else => try ignoredAttrErr(p, tok, attr.tag, "functions"),
    };
    return applySelected(qt, p);
}

pub fn applyLabelAttributes(p: *Parser, attrBufferStart: usize) !QualType {
    const attrs = p.attrBuffer.items(.attr)[attrBufferStart..];
    const toks = p.attrBuffer.items(.tok)[attrBufferStart..];
    p.attrApplicationBuffer.items.len = 0;

    for (attrs, toks) |attr, tok| switch (attr.tag) {
        .cold, .hot, .unused => try p.attrApplicationBuffer.append(p.comp.gpa, attr),
        else => try ignoredAttrErr(p, tok, attr.tag, "labels"),
    };
    return applySelected(.void, p);
}

pub fn applyStatementAttributes(p: *Parser, exprStart: TokenIndex, attrBufferStart: usize) !QualType {
    const attrs = p.attrBuffer.items(.attr)[attrBufferStart..];
    const toks = p.attrBuffer.items(.tok)[attrBufferStart..];
    p.attrApplicationBuffer.items.len = 0;

    for (attrs, toks) |attr, tok| switch (attr.tag) {
        .fallthrough => {
            for (p.tokenIds[p.tokenIdx..]) |tokenId| {
                switch (tokenId) {
                    .KeywordCase, .KeywordDefault, .Eof => {
                        try p.attrApplicationBuffer.append(p.comp.gpa, attr);
                        break;
                    },
                    .RBrace => {},
                    else => {
                        try p.err(.invalid_fallthrough, exprStart, .{});
                        break;
                    },
                }
            }
        },
        else => try p.err(.cannot_apply_attribute_to_statement, tok, .{@tagName(attr.tag)}),
    };
    return applySelected(.void, p);
}

pub fn applyEnumeratorAttributes(p: *Parser, qt: QualType, attrBufferStart: usize) !QualType {
    const attrs = p.attrBuffer.items(.attr)[attrBufferStart..];
    const toks = p.attrBuffer.items(.tok)[attrBufferStart..];
    p.attrApplicationBuffer.items.len = 0;

    for (attrs, toks) |attr, tok| switch (attr.tag) {
        .deprecated, .unavailable => try p.attrApplicationBuffer.append(p.comp.gpa, attr),
        else => try ignoredAttrErr(p, tok, attr.tag, "enums"),
    };
    return applySelected(qt, p);
}

fn applyAligned(attr: Attribute, p: *Parser, qt: QualType, diag: ?Parser.Diagnostic) !void {
    if (attr.args.aligned.alignment) |alignment| alignas: {
        if (attr.syntax != .keyword)
            break :alignas;

        const alignToken = attr.args.aligned.__name_token;
        if (diag) |t| try p.err(t, alignToken, .{});

        if (qt.isInvalid()) return;

        const defaultAlign = qt.base(p.comp).qt.alignof(p.comp);
        if (qt.is(p.comp, .func)) {
            try p.err(.alignas_on_func, alignToken, .{});
        } else if (alignment.requested < defaultAlign) {
            try p.err(.minimum_alignment, alignToken, .{defaultAlign});
        }
    }
    try p.attrApplicationBuffer.append(p.comp.gpa, attr);
}

fn applyTransparentUnion(attr: Attribute, p: *Parser, token: TokenIndex, qt: QualType) !void {
    const unionTy = qt.get(p.comp, .@"union") orelse {
        return p.err(.transparent_union_wrong_type, token, .{});
    };

    // TODO validate union defined at end
    if (unionTy.layout == null) return;
    if (unionTy.fields.len == 0)
        return p.err(.transparent_union_one_field, token, .{});

    const firstFieldSize = unionTy.fields[0].qt.bitSizeof(p.comp);
    for (unionTy.fields[1..]) |field| {
        const fieldSize = field.qt.bitSizeof(p.comp);
        if (fieldSize == firstFieldSize)
            continue;

        try p.err(.transparent_union_size, field.nameToken, .{ field.name.lookup(p.comp), fieldSize });
        return p.err(.transparent_union_size_note, unionTy.fields[0].nameToken, .{firstFieldSize});
    }

    try p.attrApplicationBuffer.append(p.comp.gpa, attr);
}

fn applyVectorSize(attr: Attribute, p: *Parser, tok: TokenIndex, qt: *QualType) !void {
    if (qt.isInvalid()) return;
    const scalarKind = qt.scalarKind(p.comp);
    if (!scalarKind.isArithmetic() or !scalarKind.isReal() or scalarKind == .Enum) {
        if (qt.get(p.comp, .@"enum")) |enumTy| {
            if (p.comp.langOpts.emulate == .clang and enumTy.incomplete) {
                return; // Clang silently ignores vector_size on incomplete enums.
            }
        }
        try p.err(.invalid_vec_elem_ty, tok, .{qt.*});
        return error.ParsingFailed;
    }

    const vecBytes = attr.args.vector_size.bytes;
    const elemSize = qt.sizeof(p.comp);
    if (vecBytes % elemSize != 0)
        return p.err(.vec_size_not_multiple, tok, .{});

    qt.* = try p.comp.typeStore.put(p.comp.gpa, .{
        .vector = .{
            .elem = qt.*,
            .len = @intCast(vecBytes / elemSize),
        },
    });
}

fn applyFormat(attr: Attribute, p: *Parser, qt: QualType) !void {
    // TODO validate
    _ = qt;
    try p.attrApplicationBuffer.append(p.comp.gpa, attr);
}

fn applyCallingConvention(attr: Attribute, p: *Parser, token: TokenIndex, qt: QualType) !void {
    const comp = p.comp;
    const gpa = comp.gpa;
    const symbol = p.tokenIds[token].symbol();
    if (!qt.is(comp, .func)) {
        return p.err(.callconv_non_func, token, .{ symbol, qt });
    }
    switch (attr.args.calling_convention.cc) {
        .c, .stdcall, .thiscall, .fastcall, .regcall => switch (comp.target.cpu.arch) {
            .x86 => try p.attrApplicationBuffer.append(gpa, attr),
            else => try p.err(.callconv_not_supported, token, .{symbol}),
        },

        .vectorcall => switch (comp.target.cpu.arch) {
            .x86, .aarch64, .aarch64_be => try p.attrApplicationBuffer.append(gpa, attr),
            else => try p.err(.callconv_not_supported, token, .{symbol}),
        },

        .riscv_vector,
        .aarch64_sve_pcs,
        .aarch64_vector_pcs,
        .arm_aapcs,
        .arm_aapcs_vfp,
        => unreachable, // These can't come from keyword syntax
    }
}

fn applySelected(qt: QualType, p: *Parser) !QualType {
    if (p.attrApplicationBuffer.items.len == 0 or qt.isInvalid()) return qt;
    return (try p.comp.typeStore.put(p.comp.gpa, .{
        .attributed = .{
            .base = qt,
            .attributes = p.attrApplicationBuffer.items,
        },
    })).withQualifiers(qt);
}
