const std = @import("std");
const mem = std.mem;
const ZigType = std.builtin.Type;
const CallingConvention = @import("../zcc.zig").CallingConvention;
const Compilation = @import("../Basic/Compilation.zig");
const Diagnostics = @import("../Basic/Diagnostics.zig");
const Parser = @import("../Parser/Parser.zig");
const Type = @import("../AST/Type.zig");
const Tree = @import("../AST/AST.zig");
const Value = @import("../AST/Value.zig");
const NodeIndex = Tree.NodeIndex;
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

    fn fromType(comptime T: type) ArgumentType {
        return switch (T) {
            Value.ByteRange => .string,
            Identifier => .identifier,
            u32 => .int,
            Alignment => .alignment,
            CallingConvention => .identifier,
            else => switch (@typeInfo(T)) {
                .@"enum" => if (T.opts.enum_kind == .string) .string else .identifier,
                else => unreachable,
            },
        };
    }

    fn fromVal(value: Value) ArgumentType {
        return switch (value.tag) {
            .int => .int,
            .bytes => .string,
            .unavailable => .expression,
            .float => .float,
            .nullptrTy => .nullptrTy,
        };
    }
};

/// representing the fields of the `Args` struct declaration within the `descriptor` type.
/// otherwise, returns an empty slice.
fn getArguments(comptime descriptor: type) []const ZigType.StructField {
    return if (@hasDecl(descriptor, "Args")) std.meta.fields(descriptor.Args) else &.{};
}

/// number of required arguments
pub fn requiredArgCount(attr: Tag) u32 {
    switch (attr) {
        inline else => |tag| {
            comptime var needed = 0;
            comptime {
                const fields = getArguments(@field(attributes, @tagName(tag)));
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
                const fields = getArguments(@field(attributes, @tagName(tag)));
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
    /// to the enum in the first field of the Args of `attr`. Identifier enums use single quotes, string enums
    /// use double quotes
    pub fn quoteChar(attr: Tag) []const u8 {
        switch (attr) {
            .calling_convention => unreachable,
            inline else => |tag| {
                const fields = getArguments((@field(attributes, @tagName(tag))));
                if (fields.len == 0) unreachable;

                const Unwrapped = UnwrapOptional(fields[0].type);
                if (@typeInfo(Unwrapped) != .@"enum") unreachable;

                return if (Unwrapped.opts.enum_kind == .identifier) "'" else "\"";
            },
        }
    }

    /// returns a comma-separated string of quoted enum values, representing the valid
    /// choices for the string or identifier enum of the first field of the Args of `attr`.
    pub fn choices(attr: Tag) []const u8 {
        switch (attr) {
            .calling_convention => unreachable,
            inline else => |tag| {
                const fields = getArguments((@field(attributes, @tagName(tag))));

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
            const fields = getArguments(@field(attributes, @tagName(tag)));

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
            const fields = getArguments(@field(attributes, @tagName(tag)));
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
            const fields = getArguments(@field(attributes, @tagName(tag)));
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
pub fn diagnoseAlignment(attr: Tag, arguments: *Arguments, argIdx: u32, val: Value, ty: Type, p: *Parser) ?Diagnostics.Message {
    switch (attr) {
        inline else => |tag| {
            const argFields = getArguments(@field(attributes, @tagName(tag)));
            if (argFields.len == 0) unreachable;

            switch (argIdx) {
                inline 0...argFields.len - 1 => |argI| {
                    if (UnwrapOptional(argFields[argI].type) != Alignment) unreachable;

                    if (val.tag != .int)
                        return Diagnostics.Message{ .tag = .alignas_unavailable };

                    if (val.compare(.lt, Value.zero, p))
                        return Diagnostics.Message{
                            .tag = .negative_alignment,
                            .extra = .{ .signed = val.signExtend(ty, p.comp) },
                        };

                    const requested = std.math.cast(u29, val.data.int) orelse {
                        return Diagnostics.Message{ .tag = .maximum_alignment, .extra = .{ .unsigned = val.data.int } };
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
    comptime wanted: type,
    arguments: *Arguments,
    val: Value,
    node: Tree.Node,
    strings: []const u8,
) ?Diagnostics.Message {
    switch (val.tag) {
        .int => {
            if (@typeInfo(wanted) == .int) {
                @field(@field(arguments, decl.name), field.name) = val.getInt(wanted);
                return null;
            }
        },
        .bytes => {
            const bytes = val.data.bytes.trim(1); // remove null terminator
            if (wanted == Value.ByteRange) {
                std.debug.assert(node.tag == .StringLiteralExpr);
                const nodeElemTy = node.type.getElemType();

                if (!nodeElemTy.is(.Char) and !nodeElemTy.is(.UChar)) {
                    return Diagnostics.Message{
                        .tag = .attribute_requires_string,
                        .extra = .{ .str = decl.name },
                    };
                }
                @field(@field(arguments, decl.name), field.name) = bytes;
                return null;
            } else if (@typeInfo(wanted) == .@"enum" and @hasDecl(wanted, "opts") and wanted.opts.enum_kind == .string) {
                const str = bytes.slice(strings, .@"1");
                if (std.meta.stringToEnum(wanted, str)) |enum_val| {
                    @field(@field(arguments, decl.name), field.name) = enum_val;
                    return null;
                } else {
                    @setEvalBranchQuota(3000);
                    return Diagnostics.Message{
                        .tag = .unknown_attr_enum,
                        .extra = .{ .attrEnum = .{ .tag = std.meta.stringToEnum(Tag, decl.name).? } },
                    };
                }
            }
        },
        else => {
            if (wanted == Identifier and node.tag == .DeclRefExpr) {
                @field(@field(arguments, decl.name), field.name) = Identifier{ .tok = node.data.declRef };
                return null;
            }
        },
    }

    return Diagnostics.Message{
        .tag = .attribute_arg_invalid,
        .extra = .{
            .attrArgType = .{
                .expected = ArgumentType.fromType(wanted),
                .actual = ArgumentType.fromVal(val),
            },
        },
    };
}

pub fn diagnose(
    attr: Tag,
    arguments: *Arguments,
    argIdx: u32,
    val: Value,
    node: Tree.Node,
    strings: []const u8,
) ?Diagnostics.Message {
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

            const argFields = getArguments(@field(attributes, decl.name));
            switch (argIdx) {
                inline 0...argFields.len - 1 => |argI| {
                    return diagnoseField(decl, argFields[argI], UnwrapOptional(argFields[argI].type), arguments, val, node, strings);
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
    node: NodeIndex = .none,
    requested: u29,
};
pub const Identifier = struct {
    tok: TokenIndex = 0,
};

const attributes = struct {
    pub const access = struct {
        const gnu = "access";
        const Args = struct {
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
    };
    pub const alias = struct {
        const gnu = "alias";
        const Args = struct {
            alias: Value.ByteRange,
        };
    };
    pub const aligned = struct {
        const gnu = "aligned";
        const declspec = "align";

        const Args = struct {
            alignment: ?Alignment = null,
            __name_token: TokenIndex,
        };
    };
    pub const alloc_align = struct {
        const gnu = "alloc_align";
        const Args = struct {
            position: u32,
        };
    };
    pub const alloc_size = struct {
        const gnu = "alloc_size";
        const Args = struct {
            position_1: u32,
            position_2: ?u32 = null,
        };
    };
    pub const allocate = struct {
        const declspec = "allocate";
        const Args = struct {
            segname: Value.ByteRange,
        };
    };
    pub const allocator = struct {
        const declspec = "allocator";
    };
    pub const always_inline = struct {
        const gnu = "always_inline";
    };
    pub const appdomain = struct {
        const declspec = "appdomain";
    };
    pub const artificial = struct {
        const gnu = "artificial";
    };
    pub const assume_aligned = struct {
        const gnu = "assume_aligned";
        const Args = struct {
            alignment: Alignment,
            offset: ?u32 = null,
        };
    };
    pub const cleanup = struct {
        const gnu = "cleanup";
        const Args = struct {
            function: Identifier,
        };
    };
    pub const code_seg = struct {
        const declspec = "code_seg";
        const Args = struct {
            segname: Value.ByteRange,
        };
    };
    pub const cold = struct {
        const gnu = "cold";
    };
    pub const common = struct {
        const gnu = "common";
    };
    pub const @"const" = struct {
        const gnu = "const";
    };
    pub const constructor = struct {
        const gnu = "constructor";
        const Args = struct {
            priority: ?u32 = null,
        };
    };
    pub const copy = struct {
        const gnu = "copy";
        const Args = struct {
            function: Identifier,
        };
    };
    pub const deprecated = struct {
        const gnu = "deprecated";
        const declspec = "deprecated";
        const c23 = "deprecated";
        const Args = struct {
            msg: ?Value.ByteRange = null,
            __name_token: TokenIndex,
        };
    };
    pub const designated_init = struct {
        const gnu = "designated_init";
    };
    pub const destructor = struct {
        const gnu = "destructor";
        const Args = struct {
            priority: ?u32 = null,
        };
    };
    pub const dllexport = struct {
        const declspec = "dllexport";
    };
    pub const dllimport = struct {
        const declspec = "dllimport";
    };
    pub const @"error" = struct {
        const gnu = "error";
        const Args = struct {
            msg: Value.ByteRange,
            __name_token: TokenIndex,
        };
    };
    pub const externally_visible = struct {
        const gnu = "externally_visible";
    };
    pub const fallthrough = struct {
        const gnu = "fallthrough";
        const c23 = "fallthrough";
    };
    pub const flatten = struct {
        const gnu = "flatten";
    };
    pub const format = struct {
        const gnu = "format";
        const Args = struct {
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
    };
    pub const format_arg = struct {
        const gnu = "format_arg";
        const Args = struct {
            string_index: u32,
        };
    };
    pub const gnu_inline = struct {
        const gnu = "gnu_inline";
    };
    pub const hot = struct {
        const gnu = "hot";
    };
    pub const ifunc = struct {
        const gnu = "ifunc";
        const Args = struct {
            resolver: Value.ByteRange,
        };
    };
    pub const interrupt = struct {
        const gnu = "interrupt";
    };
    pub const interrupt_handler = struct {
        const gnu = "interrupt_handler";
    };
    pub const jitintrinsic = struct {
        const declspec = "jitintrinsic";
    };
    pub const leaf = struct {
        const gnu = "leaf";
    };
    pub const malloc = struct {
        const gnu = "malloc";
    };
    pub const may_alias = struct {
        const gnu = "may_alias";
    };
    pub const mode = struct {
        const gnu = "mode";
        const Args = struct {
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
    };
    pub const naked = struct {
        const declspec = "naked";
    };
    pub const no_address_safety_analysis = struct {
        const gnu = "no_address_safety_analysise";
    };
    pub const no_icf = struct {
        const gnu = "no_icf";
    };
    pub const no_instrument_function = struct {
        const gnu = "no_instrument_function";
    };
    pub const no_profile_instrument_function = struct {
        const gnu = "no_profile_instrument_function";
    };
    pub const no_reorder = struct {
        const gnu = "no_reorder";
    };
    pub const no_sanitize = struct {
        const gnu = "no_sanitize";
        /// Todo: represent args as union?
        const Args = struct {
            alignment: Value.ByteRange,
            object_size: ?Value.ByteRange = null,
        };
    };
    pub const no_sanitize_address = struct {
        const gnu = "no_sanitize_address";
        const declspec = "no_sanitize_address";
    };
    pub const no_sanitize_coverage = struct {
        const gnu = "no_sanitize_coverage";
    };
    pub const no_sanitize_thread = struct {
        const gnu = "no_sanitize_thread";
    };
    pub const no_sanitize_undefined = struct {
        const gnu = "no_sanitize_undefined";
    };
    pub const no_split_stack = struct {
        const gnu = "no_split_stack";
    };
    pub const no_stack_limit = struct {
        const gnu = "no_stack_limit";
    };
    pub const no_stack_protector = struct {
        const gnu = "no_stack_protector";
    };
    pub const @"noalias" = struct {
        const declspec = "noalias";
    };
    pub const noclone = struct {
        const gnu = "noclone";
    };
    pub const nocommon = struct {
        const gnu = "nocommon";
    };
    pub const nodiscard = struct {
        const c23 = "nodiscard";
    };
    pub const noinit = struct {
        const gnu = "noinit";
    };
    pub const @"noinline" = struct {
        const gnu = "noinline";
        const declspec = "noinline";
    };
    pub const noipa = struct {
        const gnu = "noipa";
    };
    // TODO: arbitrary number of arguments
    //    const nonnull = struct {
    //        const Args = struct {
    //            arg_index: []const u32,
    //        };
    //    };
    pub const nonstring = struct {
        const gnu = "nonstring";
    };
    pub const noplt = struct {
        const gnu = "noplt";
    };
    pub const @"noreturn" = struct {
        const gnu = "noreturn";
        const c23 = "noreturn";
        const declspec = "noreturn";
    };
    // TODO: union args ?
    //    const optimize = struct {
    //        const Args = struct {
    //            optimize, // u32 | []const u8 -- optimize?
    //        };
    //    };
    pub const @"packed" = struct {
        const gnu = "packed";
    };
    pub const patchable_function_entry = struct {
        const gnu = "patchable_function_entry";
    };
    pub const persistent = struct {
        const gnu = "persistent";
    };
    pub const process = struct {
        const declspec = "process";
    };
    pub const pure = struct {
        const gnu = "pure";
    };
    pub const reproducible = struct {
        const c23 = "reproducible";
    };
    pub const restrict = struct {
        const declspec = "restrict";
    };
    pub const retain = struct {
        const gnu = "retain";
    };
    pub const returns_nonnull = struct {
        const gnu = "returns_nonnull";
    };
    pub const returns_twice = struct {
        const gnu = "returns_twice";
    };
    pub const safebuffers = struct {
        const declspec = "safebuffers";
    };
    pub const scalar_storage_order = struct {
        const gnu = "scalar_storage_order";
        const Args = struct {
            order: enum {
                @"little-endian",
                @"big-endian",

                const opts = struct {
                    const enum_kind = .string;
                };
            },
        };
    };
    pub const section = struct {
        const gnu = "section";
        const Args = struct {
            name: Value.ByteRange,
        };
    };
    pub const sentinel = struct {
        const gnu = "sentinel";
        const Args = struct {
            position: ?u32 = null,
        };
    };
    pub const simd = struct {
        const gnu = "simd";
        const Args = struct {
            mask: ?enum {
                notinbranch,
                inbranch,

                const opts = struct {
                    const enum_kind = .string;
                };
            } = null,
        };
    };
    pub const spectre = struct {
        const declspec = "spectre";
        const Args = struct {
            arg: enum {
                nomitigation,

                const opts = struct {
                    const enum_kind = .identifier;
                };
            },
        };
    };
    pub const stack_protect = struct {
        const gnu = "stack_protect";
    };
    pub const symver = struct {
        const gnu = "symver";
        const Args = struct {
            version: Value.ByteRange, // TODO: validate format "name2@nodename"
        };
    };
    pub const target = struct {
        const gnu = "target";
        const Args = struct {
            options: Value.ByteRange, // TODO: multiple arguments
        };
    };
    pub const target_clones = struct {
        const gnu = "target_clones";
        const Args = struct {
            options: Value.ByteRange, // TODO: multiple arguments
        };
    };
    pub const thread = struct {
        const declspec = "thread";
    };
    pub const tls_model = struct {
        const gnu = "tls_model";
        const Args = struct {
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
    };
    pub const transparent_union = struct {
        const gnu = "transparent_union";
    };
    pub const unavailable = struct {
        const gnu = "unavailable";
        const Args = struct {
            msg: ?Value.ByteRange = null,
            __name_token: TokenIndex,
        };
    };
    pub const uninitialized = struct {
        const gnu = "uninitialized";
    };
    pub const unsequenced = struct {
        const c23 = "unsequenced";
    };
    pub const unused = struct {
        const gnu = "unused";
        const c23 = "maybe_unused";
    };
    pub const used = struct {
        const gnu = "used";
    };
    pub const uuid = struct {
        const declspec = "uuid";
        const Args = struct {
            uuid: Value.ByteRange,
        };
    };
    pub const vector_size = struct {
        const gnu = "vector_size";
        const Args = struct {
            bytes: u32, // TODO: validate "The bytes argument must be a positive power-of-two multiple of the base type size"
        };
    };
    pub const visibility = struct {
        const gnu = "visibility";
        const Args = struct {
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
    };
    pub const warn_if_not_aligned = struct {
        const gnu = "warn_if_not_aligned";
        const Args = struct {
            alignment: Alignment,
        };
    };
    pub const warn_unused_result = struct {
        const gnu = "warn_unused_result";
    };
    pub const warning = struct {
        const gnu = "warning";
        const Args = struct {
            msg: Value.ByteRange,
            __name_token: TokenIndex,
        };
    };
    pub const weak = struct {
        const gnu = "weak";
    };
    pub const weakref = struct {
        const gnu = "weakref";
        const Args = struct {
            target: ?Value.ByteRange = null,
        };
    };
    pub const zero_call_used_regs = struct {
        const gnu = "zero_call_used_regs";
        const Args = struct {
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
    };
    pub const asm_label = struct {
        const Args = struct {
            name: Value.ByteRange,
        };
    };
    pub const calling_convention = struct {
        const Args = struct {
            cc: CallingConvention,
        };
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
            .type = if (@hasDecl(@field(attributes, decl.name), "Args")) @field(attributes, decl.name).Args else void,
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
    const field = @field(attributes, decl.name);
    return if (@hasDecl(field, "Args")) field.Args else void;
}

pub fn initArguments(tag: Tag, nameToken: TokenIndex) Arguments {
    switch (tag) {
        inline else => |argTag| {
            const tagName = @tagName(argTag);
            const unionElement = @field(attributes, tagName); // get nested field
            const hasArgs = @hasDecl(unionElement, "Args");
            const init = if (hasArgs) std.mem.zeroInit(unionElement.Args, .{}) else {};
            var args = @unionInit(Arguments, tagName, init);
            if (@hasDecl(unionElement, "Args") and @hasField(unionElement.Args, "__name_token")) {
                @field(args, tagName).__name_token = nameToken;
            }
            return args;
        },
    }
}

pub fn fromString(kind: Kind, namespace: ?[]const u8, name: []const u8) ?Tag {
    return switch (kind) {
        .c23 => fromStringC23(namespace, name),
        .declspec => fromStringDeclspec(name),
        .gnu => fromStringGnu(name),
    };
}

fn fromStringGnu(name: []const u8) ?Tag {
    const normalized = normalize(name);
    const decls = @typeInfo(attributes).@"struct".decls;
    @setEvalBranchQuota(3000);
    inline for (decls, 0..) |decl, i| {
        const field = @field(attributes, decl.name);
        if (@hasDecl(field, "gnu")) {
            if (mem.eql(u8, field.gnu, normalized)) {
                return @enumFromInt(i);
            }
        }
    }
    return null;
}

fn fromStringC23(namespace: ?[]const u8, name: []const u8) ?Tag {
    const normalized = normalize(name);
    if (namespace) |ns| {
        const normalizedNS = normalize(ns);
        if (mem.eql(u8, normalizedNS, "gnu")) {
            return fromStringGnu(normalized);
        }
        return null;
    }

    const decls = @typeInfo(attributes).@"struct".decls;
    inline for (decls, 0..) |decl, i| {
        const field = @field(attributes, decl.name);
        if (@hasDecl(field, "c23")) {
            if (mem.eql(u8, field.c23, normalized)) {
                return @enumFromInt(i);
            }
        }
    }
    return null;
}

fn fromStringDeclspec(name: []const u8) ?Tag {
    const normalized = normalize(name);
    const decls = @typeInfo(attributes).@"struct".decls;
    inline for (decls, 0..) |decl, i| {
        const field = @field(attributes, decl.name);
        if (@hasDecl(field, "declspec")) {
            if (mem.eql(u8, field.declspec, normalized)) {
                return @enumFromInt(i);
            }
        }
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
pub fn applyVariableAttributes(p: *Parser, ty: Type, attrBufferStart: usize, tag: ?Diagnostics.Tag) !Type {
    const attrs = p.attrBuffer.items(.attr)[attrBufferStart..];
    const toks = p.attrBuffer.items(.tok)[attrBufferStart..];
    p.attrApplicationBuffer.items.len = 0;

    var baseTy = ty;
    if (baseTy.specifier == .Attributed)
        baseTy = baseTy.data.attributed.base;

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

        .vector_size => try attr.applyVectorSize(p, tok, &baseTy),
        .aligned => try attr.applyAligned(p, baseTy, tag),

        .nonstring => if (!baseTy.isArray() or !baseTy.isChar()) {
            try p.errStr(.non_string_ignored, tok, try p.typeStr(ty));
        } else {
            try p.attrApplicationBuffer.append(p.gpa, attr);
        },

        .uninitialized => if (p.func.type == null) {
            try p.errStr(.local_variable_attribute, tok, "uninitialized");
        } else {
            try p.attrApplicationBuffer.append(p.gpa, attr);
        },

        .cleanup => if (p.func.type == null) {
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

    const existing = ty.getAttributes();
    if (existing.len == 0 and p.attrApplicationBuffer.items.len == 0)
        return baseTy;
    if (existing.len == 0)
        return baseTy.withAttributes(p.arena, p.attrApplicationBuffer.items);

    const attributedTy = try Type.Attributed.create(p.arena, baseTy, existing, p.attrApplicationBuffer.items);
    return Type{ .specifier = .Attributed, .data = .{ .attributed = attributedTy } };
}

pub fn applyFieldAttributes(p: *Parser, fieldTy: *Type, attrBufferStart: usize) ![]const Attribute {
    const attrs = p.attrBuffer.items(.attr)[attrBufferStart..];
    const toks = p.attrBuffer.items(.tok)[attrBufferStart..];
    p.attrApplicationBuffer.items.len = 0;

    for (attrs, toks) |attr, tok| switch (attr.tag) {
        // zig fmt: off
        .@"packed", .may_alias, .deprecated, .unavailable, .unused, .warn_if_not_aligned, .mode,
        => try p.attrApplicationBuffer.append(p.gpa, attr),
        // zig fmt: on

        .vector_size => try attr.applyVectorSize(p, tok, fieldTy),
        .aligned => try attr.applyAligned(p, fieldTy.*, null),
        else => try ignoredAttrErr(p, tok, attr.tag, "fields"),
    };

    if (p.attrApplicationBuffer.items.len == 0) return &[0]Attribute{};
    return p.arena.dupe(Attribute, p.attrApplicationBuffer.items);
}

pub fn applyTypeAttributes(p: *Parser, ty: Type, attrBufferStart: usize, tag: ?Diagnostics.Tag) !Type {
    const attrs = p.attrBuffer.items(.attr)[attrBufferStart..];
    const toks = p.attrBuffer.items(.tok)[attrBufferStart..];
    p.attrApplicationBuffer.items.len = 0;

    var baseTy = ty;
    if (baseTy.specifier == .Attributed)
        baseTy = baseTy.data.attributed.base;

    for (attrs, toks) |attr, tok| switch (attr.tag) {
        .@"packed",
        .may_alias,
        .deprecated,
        .unavailable,
        .unused,
        .warn_if_not_aligned,
        .mode,
        => try p.attrApplicationBuffer.append(p.gpa, attr),

        .transparent_union => try attr.applyTransparentUnion(p, tok, baseTy),
        .vector_size => try attr.applyVectorSize(p, tok, &baseTy),
        .aligned => try attr.applyAligned(p, baseTy, tag),

        .designated_init => if (baseTy.is(.Struct)) {
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

    const existing = ty.getAttributes();
    // TODO: the alignment annotation on a type should override
    // the decl it refers to. This might not be true for others.  Maybe bug.

    // if there are annotations on this type def use those.
    if (p.attrApplicationBuffer.items.len > 0) {
        return try baseTy.withAttributes(p.arena, p.attrApplicationBuffer.items);
    } else if (existing.len > 0) {
        // else use the ones on the typedef decl we were refering to.
        return try baseTy.withAttributes(p.arena, existing);
    }
    return baseTy;
}

pub fn applyFunctionAttributes(p: *Parser, ty: Type, attrBufferStart: usize) !Type {
    const attrs = p.attrBuffer.items(.attr)[attrBufferStart..];
    const toks = p.attrBuffer.items(.tok)[attrBufferStart..];
    p.attrApplicationBuffer.items.len = 0;

    var baseTy = ty;
    if (baseTy.specifier == .Attributed)
        baseTy = baseTy.data.attributed.base;

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

        .aligned => try attr.applyAligned(p, baseTy, null),
        .format => try attr.applyFormat(p, baseTy),

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
    return ty.withAttributes(p.arena, p.attrApplicationBuffer.items);
}

pub fn applyLabelAttributes(p: *Parser, ty: Type, attrBufferStart: usize) !Type {
    const attrs = p.attrBuffer.items(.attr)[attrBufferStart..];
    const toks = p.attrBuffer.items(.tok)[attrBufferStart..];
    p.attrApplicationBuffer.items.len = 0;

    for (attrs, toks) |attr, tok| switch (attr.tag) {
        .cold, .hot, .unused => try p.attrApplicationBuffer.append(p.gpa, attr),
        else => try ignoredAttrErr(p, tok, attr.tag, "labels"),
    };
    return ty.withAttributes(p.arena, p.attrApplicationBuffer.items);
}

pub fn applyStatementAttributes(p: *Parser, ty: Type, exprStart: TokenIndex, attrBufferStart: usize) !Type {
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
    return ty.withAttributes(p.arena, p.attrApplicationBuffer.items);
}

pub fn applyEnumeratorAttributes(p: *Parser, ty: Type, attrBufferStart: usize) !Type {
    const attrs = p.attrBuffer.items(.attr)[attrBufferStart..];
    const toks = p.attrBuffer.items(.tok)[attrBufferStart..];
    p.attrApplicationBuffer.items.len = 0;

    for (attrs, toks) |attr, tok| switch (attr.tag) {
        .deprecated, .unavailable => try p.attrApplicationBuffer.append(p.gpa, attr),
        else => try ignoredAttrErr(p, tok, attr.tag, "enums"),
    };
    return ty.withAttributes(p.arena, p.attrApplicationBuffer.items);
}

fn applyAligned(attr: Attribute, p: *Parser, ty: Type, tag: ?Diagnostics.Tag) !void {
    const base = ty.canonicalize(.standard);
    const defaultAlign = base.alignof(p.comp);
    if (attr.args.aligned.alignment) |alignment| alignas: {
        if (attr.syntax != .keyword)
            break :alignas;

        const alignToken = attr.args.aligned.__name_token;
        if (tag) |t| try p.errToken(t, alignToken);
        if (ty.isFunc()) {
            try p.errToken(.alignas_on_func, alignToken);
        } else if (alignment.requested < defaultAlign) {
            try p.errExtra(.minimum_alignment, alignToken, .{ .unsigned = defaultAlign });
        }
    }
    try p.attrApplicationBuffer.append(p.gpa, attr);
}

fn applyTransparentUnion(attr: Attribute, p: *Parser, token: TokenIndex, ty: Type) !void {
    const unionTy = ty.get(.Union) orelse {
        return p.errToken(.transparent_union_wrong_type, token);
    };

    // TODO validate union defined at end
    if (unionTy.data.record.isIncomplete())
        return;

    const fields = unionTy.data.record.fields;
    if (fields.len == 0)
        return p.errToken(.transparent_union_one_field, token);

    const firstFieldSize = fields[0].ty.bitSizeof(p.comp).?;
    for (fields[1..]) |field| {
        const fieldSize = field.ty.bitSizeof(p.comp).?;
        if (fieldSize == firstFieldSize)
            continue;

        const mapper = p.comp.stringInterner.getSlowTypeMapper();
        const str = try std.fmt.allocPrint(
            p.comp.diagnostics.arena.allocator(),
            "'{s}' ({d}",
            .{ mapper.lookup(field.name), fieldSize },
        );
        try p.errStr(.transparent_union_size, field.nameToken, str);
        return p.errExtra(.transparent_union_size_note, fields[0].nameToken, .{ .unsigned = firstFieldSize });
    }

    try p.attrApplicationBuffer.append(p.gpa, attr);
}

fn applyVectorSize(attr: Attribute, p: *Parser, tok: TokenIndex, ty: *Type) !void {
    if (!(ty.isInt() or ty.isFloat()) or !ty.isReal()) {
        const originTy = try p.typeStr(ty.*);
        ty.* = Type.Invalid;
        return p.errStr(.invalid_vec_elem_ty, tok, originTy);
    }

    const vecBytes = attr.args.vector_size.bytes;
    const tySize = ty.sizeof(p.comp).?;
    if (vecBytes % tySize != 0)
        return p.errToken(.vec_size_not_multiple, tok);

    const vec_size = vecBytes / tySize;

    const arrTy = try p.arena.create(Type.Array);
    arrTy.* = .{ .elem = ty.*, .len = vec_size };
    ty.* = Type{
        .specifier = .Vector,
        .data = .{ .array = arrTy },
    };
}

fn applyFormat(attr: Attribute, p: *Parser, ty: Type) !void {
    // TODO validate
    _ = ty;
    try p.attrApplicationBuffer.append(p.gpa, attr);
}
