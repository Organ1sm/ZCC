const std = @import("std");
const mem = std.mem;
const ZigType = std.builtin.Type;
const CallingConvention = @import("backend").CallingConvention;
const Compilation = @import("../Basic/Compilation.zig");
const Diagnostics = @import("../Basic/Diagnostics.zig");
const Parser = @import("../Parser/Parser.zig");
const Result = @import("../Parser/Result.zig");
const QualType = @import("../AST/TypeStore.zig").QualType;
const Tree = @import("../AST/AST.zig");
const Value = @import("../AST/Value.zig");
const TokenIndex = Tree.TokenIndex;

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

pub const ArgumentType = enum {
    string,
    identifier,
    int,
    alignment,
    float,
    expression,
    nullptrTy,

    pub fn toString(self: ArgumentType) []const u8 {
        return switch (self) {
            .string => "a string",
            .identifier => "an identifier",
            .int, .alignment => "an integer constant",
            .nullptrTy => "nullptr",
            .float => "a floating point number",
            .expression => "an expression",
        };
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

pub fn diagnoseIdent(attr: Tag, arguments: *Arguments, ident: []const u8) ?Diagnostics.Message {
    switch (attr) {
        inline else => |tag| {
            // Get the fields of the Args struct corresponding to the given tag
            const fields = std.meta.fields(@field(attributes, @tagName(tag)));
            if (fields.len == 0)
                unreachable;

            const Unwrapped = UnwrapOptional(fields[0].type);
            if (@typeInfo(Unwrapped) != .@"enum") unreachable;
            if (std.meta.stringToEnum(Unwrapped, normalize(ident))) |enumVal| {
                @field(@field(arguments, @tagName(tag)), fields[0].name) = enumVal;
                return null;
            }

            return Diagnostics.Message{
                .tag = .unknown_attr_enum,
                .extra = .{ .attrEnum = .{ .tag = attr } },
            };
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
pub fn diagnoseAlignment(attr: Tag, arguments: *Arguments, argIdx: u32, res: Result, p: *Parser) !?Diagnostics.Message {
    switch (attr) {
        inline else => |tag| {
            const argFields = std.meta.fields(@field(attributes, @tagName(tag)));
            if (argFields.len == 0) unreachable;

            switch (argIdx) {
                inline 0...argFields.len - 1 => |argI| {
                    if (UnwrapOptional(argFields[argI].type) != Alignment) unreachable;

                    if (!res.value.is(.int, p.comp))
                        return Diagnostics.Message{ .tag = .alignas_unavailable };

                    if (res.value.compare(.lt, Value.zero, p.comp))
                        return Diagnostics.Message{ .tag = .negative_alignment, .extra = .{ .str = try res.str(p) } };

                    const requested = res.value.toInt(u29, p.comp) orelse {
                        return Diagnostics.Message{ .tag = .maximum_alignment, .extra = .{ .str = try res.str(p) } };
                    };
                    if (!std.mem.isValidAlign(requested))
                        return Diagnostics.Message{ .tag = .non_pow2_align };

                    // Set the alignment of the argument to the requested value
                    @field(@field(arguments, @tagName(tag)), argFields[argI].name) = Alignment{ .requested = requested };
                    return null;
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
    node: Tree.Node,
    p: *Parser,
) !?Diagnostics.Message {
    if (res.value.isNone()) {
        if (Wanted == Identifier and node == .declRefExpr) {
            @field(@field(arguments, decl.name), field.name) = .{ .tok = node.declRefExpr.nameToken };
            return null;
        }
        return invalidArgMsg(Wanted, .expression);
    }
    const key = p.comp.interner.get(res.value.ref());
    switch (key) {
        .int => {
            if (@typeInfo(Wanted) == .int) {
                @field(@field(arguments, decl.name), field.name) = res.value.toInt(Wanted, p.comp) orelse return .{
                    .tag = .attribute_int_out_of_range,
                    .extra = .{ .str = try res.str(p) },
                };
                return null;
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
                    return .{
                        .tag = .attribute_requires_string,
                        .extra = .{ .str = decl.name },
                    };
                }
                @field(@field(arguments, decl.name), field.name) = try p.removeNull(res.value);
                return null;
            } else if (@typeInfo(Wanted) == .@"enum" and @hasDecl(Wanted, "opts") and Wanted.opts.enum_kind == .string) {
                const str = bytes[0 .. bytes.len - 1];
                if (std.meta.stringToEnum(Wanted, str)) |enum_val| {
                    @field(@field(arguments, decl.name), field.name) = enum_val;
                    return null;
                } else {
                    @setEvalBranchQuota(3000);
                    return .{
                        .tag = .unknown_attr_enum,
                        .extra = .{ .attrEnum = .{ .tag = std.meta.stringToEnum(Tag, decl.name).? } },
                    };
                }
            }
        },
        else => {},
    }

    return invalidArgMsg(Wanted, switch (key) {
        .int => .int,
        .bytes => .string,
        .float => .float,
        .null => .nullptrTy,
        else => unreachable,
    });
}

fn invalidArgMsg(comptime Expected: type, actual: ArgumentType) Diagnostics.Message {
    return .{
        .tag = .attribute_arg_invalid,
        .extra = .{
            .attrArgType = .{
                .expected = switch (Expected) {
                    Value => .string,
                    Identifier => .identifier,
                    u32 => .int,
                    Alignment => .alignment,
                    CallingConvention => .identifier,
                    else => switch (@typeInfo(Expected)) {
                        .@"enum" => if (Expected.opts.enum_kind == .string) .string else .identifier,
                        else => unreachable,
                    },
                },
                .actual = actual,
            },
        },
    };
}

pub fn diagnose(
    attr: Tag,
    arguments: *Arguments,
    argIdx: u32,
    res: Result,
    node: Tree.Node,
    p: *Parser,
) !?Diagnostics.Message {
    switch (attr) {
        inline else => |tag| {
            const decl = @typeInfo(attributes).@"struct".decls[@intFromEnum(tag)];
            const max_arg_count = comptime maxArgCount(tag);
            if (argIdx >= max_arg_count)
                return Diagnostics.Message{
                    .tag = .attribute_too_many_args,
                    .extra = .{
                        .attrArgCount = .{
                            .attribute = attr,
                            .expected = max_arg_count,
                        },
                    },
                };

            const argFields = std.meta.fields(@field(attributes, decl.name));
            switch (argIdx) {
                inline 0...argFields.len - 1 => |argI| {
                    return diagnoseField(decl, argFields[argI], UnwrapOptional(argFields[argI].type), arguments, res, node, p);
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
            .alignment = 0,
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
    const stringsTop = p.strings.items.len;
    defer p.strings.items.len = stringsTop;

    try p.strings.writer().print("attribute '{s}' ignored on {s}", .{ @tagName(attr), context });
    const str = try p.comp.diagnostics.arena.allocator().dupe(u8, p.strings.items[stringsTop..]);
    try p.errStr(.ignored_attribute, token, str);
}

pub const applyParameterAttributes = applyVariableAttributes;
pub fn applyVariableAttributes(p: *Parser, qt: QualType, attrBufferStart: usize, tag: ?Diagnostics.Tag) !QualType {
    const attrs = p.attrBuffer.items(.attr)[attrBufferStart..];
    const toks = p.attrBuffer.items(.tok)[attrBufferStart..];
    p.attrApplicationBuffer.items.len = 0;

    var baseQt = qt;
    var common = false;
    var nocommon = false;
    for (attrs, toks) |attr, tok| switch (attr.tag) {
        // zig fmt: off
        .alias, .may_alias, .deprecated, .unavailable, .unused, .warn_if_not_aligned, .weak, .used,
        .noinit, .retain, .persistent, .section, .mode, .asm_label,
         => try p.attrApplicationBuffer.append(p.gpa, attr),
        // zig fmt: on

        .common => if (nocommon) {
            try p.errToken(.ignore_common, tok);
        } else {
            try p.attrApplicationBuffer.append(p.gpa, attr);
            common = true;
        },

        .nocommon => if (common) {
            try p.errToken(.ignore_nocommon, tok);
        } else {
            try p.attrApplicationBuffer.append(p.gpa, attr);
            nocommon = true;
        },

        .vector_size => try attr.applyVectorSize(p, tok, &baseQt),
        .aligned => try attr.applyAligned(p, baseQt, tag),

        .nonstring => {
            if (baseQt.get(p.comp, .array)) |arrayTy| {
                if (arrayTy.elem.get(p.comp, .int)) |intTy| switch (intTy) {
                    .Char, .UChar, .SChar => {
                        try p.attrApplicationBuffer.append(p.gpa, attr);
                        continue;
                    },
                    else => {},
                };
            }
            try p.errStr(.non_string_ignored, tok, try p.typeStr(qt));
        },
        .uninitialized => if (p.func.qt == null) {
            try p.errStr(.local_variable_attribute, tok, "uninitialized");
        } else {
            try p.attrApplicationBuffer.append(p.gpa, attr);
        },

        .cleanup => if (p.func.qt == null) {
            try p.errStr(.local_variable_attribute, tok, "cleanup");
        } else {
            try p.attrApplicationBuffer.append(p.gpa, attr);
        },

        .alloc_size,
        .copy,
        .tls_model,
        .visibility,
        => std.debug.panic("apply variable attribute {s}", .{@tagName(attr.tag)}),
        else => try ignoredAttrErr(p, tok, attr.tag, "variables"),
    };

    return applySelected(baseQt, p);
}

pub fn applyFieldAttributes(p: *Parser, fieldTy: *QualType, attrBufferStart: usize) ![]const Attribute {
    const attrs = p.attrBuffer.items(.attr)[attrBufferStart..];
    const toks = p.attrBuffer.items(.tok)[attrBufferStart..];
    p.attrApplicationBuffer.items.len = 0;

    for (attrs, toks) |attr, tok| switch (attr.tag) {
        // zig fmt: off
        .@"packed", .may_alias, .deprecated, .unavailable, .unused, .warn_if_not_aligned, .mode,.warn_unused_result, .nodiscard,
        => try p.attrApplicationBuffer.append(p.gpa, attr),
        // zig fmt: on

        .vector_size => try attr.applyVectorSize(p, tok, fieldTy),
        .aligned => try attr.applyAligned(p, fieldTy.*, null),
        else => try ignoredAttrErr(p, tok, attr.tag, "fields"),
    };

    if (p.attrApplicationBuffer.items.len == 0) return &.{};

    return p.attrApplicationBuffer.items;
}

pub fn applyTypeAttributes(p: *Parser, qt: QualType, attrBufferStart: usize, tag: ?Diagnostics.Tag) !QualType {
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
        => try p.attrApplicationBuffer.append(p.gpa, attr),

        .transparent_union => try attr.applyTransparentUnion(p, tok, baseQt),
        .vector_size => try attr.applyVectorSize(p, tok, &baseQt),
        .aligned => try attr.applyAligned(p, baseQt, tag),

        .designated_init => if (baseQt.is(p.comp, .@"struct")) {
            try p.attrApplicationBuffer.append(p.gpa, attr);
        } else {
            try p.errToken(.designated_init_invalid, tok);
        },

        .alloc_size,
        .copy,
        .scalar_storage_order,
        .nonstring,
        => std.debug.panic("apply type attribute {s}", .{@tagName(attr.tag)}),

        else => try ignoredAttrErr(p, tok, attr.tag, "types"),
    };

    return applySelected(baseQt, p);
}

pub fn applyFunctionAttributes(p: *Parser, qt: QualType, attrBufferStart: usize) !QualType {
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
        .reproducible, .unsequenced,
         => try p.attrApplicationBuffer.append(p.gpa, attr),
        // zig fmt: on

        .hot => if (cold) {
            try p.errToken(.ignore_hot, tok);
        } else {
            try p.attrApplicationBuffer.append(p.gpa, attr);
            hot = true;
        },

        .cold => if (hot) {
            try p.errToken(.ignore_cold, tok);
        } else {
            try p.attrApplicationBuffer.append(p.gpa, attr);
            cold = true;
        },

        .always_inline => if (@"noinline") {
            try p.errToken(.ignore_always_inline, tok);
        } else {
            try p.attrApplicationBuffer.append(p.gpa, attr);
            alwaysInline = true;
        },

        .@"noinline" => if (alwaysInline) {
            try p.errToken(.ignore_noinline, tok);
        } else {
            try p.attrApplicationBuffer.append(p.gpa, attr);
            @"noinline" = true;
        },

        .aligned => try attr.applyAligned(p, baseQt, null),
        .format => try attr.applyFormat(p, baseQt),

        .calling_convention => switch (attr.args.calling_convention.cc) {
            .C => continue,
            .stdcall, .thiscall => switch (p.comp.target.cpu.arch) {
                .x86 => try p.attrApplicationBuffer.append(p.gpa, attr),
                else => try p.errStr(.callconv_not_supported, tok, p.tokenIds[tok].lexeme().?),
            },
            .vectorcall => switch (p.comp.target.cpu.arch) {
                .x86, .aarch64, .aarch64_be => try p.attrApplicationBuffer.append(p.gpa, attr),
                else => try p.errStr(.callconv_not_supported, tok, p.tokenIds[tok].lexeme().?),
            },
        },

        .access,
        .alloc_align,
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
        => std.debug.panic("apply type attribute {s}", .{@tagName(attr.tag)}),
        else => try ignoredAttrErr(p, tok, attr.tag, "functions"),
    };
    return applySelected(qt, p);
}

pub fn applyLabelAttributes(p: *Parser, attrBufferStart: usize) !QualType {
    const attrs = p.attrBuffer.items(.attr)[attrBufferStart..];
    const toks = p.attrBuffer.items(.tok)[attrBufferStart..];
    p.attrApplicationBuffer.items.len = 0;

    for (attrs, toks) |attr, tok| switch (attr.tag) {
        .cold, .hot, .unused => try p.attrApplicationBuffer.append(p.gpa, attr),
        else => try ignoredAttrErr(p, tok, attr.tag, "labels"),
    };
    return applySelected(.void, p);
}

pub fn applyStatementAttributes(p: *Parser, exprStart: TokenIndex, attrBufferStart: usize) !QualType {
    const attrs = p.attrBuffer.items(.attr)[attrBufferStart..];
    const toks = p.attrBuffer.items(.tok)[attrBufferStart..];
    p.attrApplicationBuffer.items.len = 0;

    for (attrs, toks) |attr, tok| switch (attr.tag) {
        .fallthrough => if (p.currToken() != .KeywordCase and p.currToken() != .KeywordDefault) {
            // TODO: this condition is not completely correct; the last statement of a compound
            // statement is also valid if it precedes a switch label (so intervening '}' are ok,
            // but only if they close a compound statement)
            try p.errToken(.invalid_fallthrough, exprStart);
        } else {
            try p.attrApplicationBuffer.append(p.gpa, attr);
        },
        else => try p.errStr(.cannot_apply_attribute_to_statement, tok, @tagName(attr.tag)),
    };
    return applySelected(.void, p);
}

pub fn applyEnumeratorAttributes(p: *Parser, qt: QualType, attrBufferStart: usize) !QualType {
    const attrs = p.attrBuffer.items(.attr)[attrBufferStart..];
    const toks = p.attrBuffer.items(.tok)[attrBufferStart..];
    p.attrApplicationBuffer.items.len = 0;

    for (attrs, toks) |attr, tok| switch (attr.tag) {
        .deprecated, .unavailable => try p.attrApplicationBuffer.append(p.gpa, attr),
        else => try ignoredAttrErr(p, tok, attr.tag, "enums"),
    };
    return applySelected(qt, p);
}

fn applyAligned(attr: Attribute, p: *Parser, qt: QualType, tag: ?Diagnostics.Tag) !void {
    if (attr.args.aligned.alignment) |alignment| alignas: {
        if (attr.syntax != .keyword)
            break :alignas;

        const alignToken = attr.args.aligned.__name_token;
        if (tag) |t| try p.errToken(t, alignToken);

        const defaultAlign = qt.base(p.comp).qt.alignof(p.comp);
        if (qt.is(p.comp, .func)) {
            try p.errToken(.alignas_on_func, alignToken);
        } else if (alignment.requested < defaultAlign) {
            try p.errExtra(.minimum_alignment, alignToken, .{ .unsigned = defaultAlign });
        }
    }
    try p.attrApplicationBuffer.append(p.gpa, attr);
}

fn applyTransparentUnion(attr: Attribute, p: *Parser, token: TokenIndex, qt: QualType) !void {
    const unionTy = qt.get(p.comp, .@"union") orelse {
        return p.errToken(.transparent_union_wrong_type, token);
    };

    // TODO validate union defined at end
    if (unionTy.layout == null) return;
    if (unionTy.fields.len == 0)
        return p.errToken(.transparent_union_one_field, token);

    const firstFieldSize = unionTy.fields[0].qt.bitSizeof(p.comp);
    for (unionTy.fields[1..]) |field| {
        const fieldSize = field.qt.bitSizeof(p.comp);
        if (fieldSize == firstFieldSize)
            continue;

        const str = try std.fmt.allocPrint(
            p.comp.diagnostics.arena.allocator(),
            "'{s}' ({d}",
            .{ field.name.lookup(p.comp), fieldSize },
        );
        try p.errStr(.transparent_union_size, field.nameToken, str);
        return p.errExtra(.transparent_union_size_note, unionTy.fields[0].nameToken, .{ .unsigned = firstFieldSize });
    }

    try p.attrApplicationBuffer.append(p.gpa, attr);
}

fn applyVectorSize(attr: Attribute, p: *Parser, tok: TokenIndex, qt: *QualType) !void {
    const scalarKind = qt.scalarKind(p.comp);
    if (!scalarKind.isArithmetic() or !scalarKind.isReal()) {
        if (qt.get(p.comp, .@"enum")) |enumTy| {
            if (p.comp.langOpts.emulate == .clang and enumTy.incomplete) {
                return; // Clang silently ignores vector_size on incomplete enums.
            }
        }
        const originTy = try p.typeStr(qt.*);
        return p.errStr(.invalid_vec_elem_ty, tok, originTy);
    }

    const vecBytes = attr.args.vector_size.bytes;
    const elemSize = qt.sizeof(p.comp);
    if (vecBytes % elemSize != 0)
        return p.errToken(.vec_size_not_multiple, tok);

    qt.* = try p.comp.typeStore.put(p.gpa, .{
        .vector = .{
            .elem = qt.*,
            .len = @intCast(vecBytes / elemSize),
        },
    });
}

fn applyFormat(attr: Attribute, p: *Parser, qt: QualType) !void {
    // TODO validate
    _ = qt;
    try p.attrApplicationBuffer.append(p.gpa, attr);
}

fn applySelected(qt: QualType, p: *Parser) !QualType {
    if (p.attrApplicationBuffer.items.len == 0) return qt;
    return (try p.comp.typeStore.put(p.gpa, .{
        .attributed = .{
            .base = qt,
            .attributes = p.attrApplicationBuffer.items,
        },
    })).withQualifiers(qt);
}
