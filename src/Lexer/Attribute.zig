const std = @import("std");
const mem = std.mem;
const ZigType = std.builtin.Type;
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
    array,
    expression,

    pub fn toString(self: ArgumentType) []const u8 {
        return switch (self) {
            .string => "a string",
            .identifier => "an identifier",
            .int, .alignment => "an integer constant",
            .float => "a floating point number",
            .array => "an array",
            .expression => "an expression",
        };
    }

    fn fromType(comptime T: type) ArgumentType {
        return switch (T) {
            []const u8 => .string,
            Identifier => .identifier,
            u32 => .int,
            Alignment => .alignment,
            else => switch (@typeInfo(T)) {
                .Enum => if (T.opts.enum_kind == .string) .string else .identifier,
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
            .array => .array,
        };
    }
};

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
                for (fields) |arg_field| {
                    if (!mem.eql(u8, arg_field.name, "__name_token") and @typeInfo(arg_field.type) != .Optional)
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
                for (fields) |arg_field| {
                    if (!mem.eql(u8, arg_field.name, "__name_token"))
                        max += 1;
                }
            }
            return max;
        },
    }
}

fn UnwrapOptional(comptime T: type) type {
    return switch (@typeInfo(T)) {
        .Optional => |optional| optional.child,
        else => T,
    };
}

pub const Formatting = struct {
    /// The quote char (single or double) to use when printing identifiers/strings corresponding
    /// to the enum in the first field of the Args of `attr`. Identifier enums use single quotes, string enums
    /// use double quotes
    pub fn quoteChar(attr: Tag) []const u8 {
        switch (attr) {
            inline else => |tag| {
                const fields = getArguments((@field(attributes, @tagName(tag))));

                if (fields.len == 0) unreachable;
                const Unwrapped = UnwrapOptional(fields[0].type);
                if (@typeInfo(Unwrapped) != .Enum) unreachable;

                return if (Unwrapped.opts.enum_kind == .identifier) "'" else "\"";
            },
        }
    }

    /// returns a comma-separated string of quoted enum values, representing the valid
    /// choices for the string or identifier enum of the first field of the Args of `attr`.
    pub fn choices(attr: Tag) []const u8 {
        switch (attr) {
            inline else => |tag| {
                const fields = getArguments((@field(attributes, @tagName(tag))));

                if (fields.len == 0) unreachable;
                const Unwrapped = UnwrapOptional(fields[0].type);
                if (@typeInfo(Unwrapped) != .Enum) unreachable;

                const enum_fields = @typeInfo(Unwrapped).Enum.fields;
                @setEvalBranchQuota(3000);
                const quote = comptime quoteChar(@enumFromInt(@intFromEnum(tag)));
                comptime var values: []const u8 = quote ++ enum_fields[0].name ++ quote;
                inline for (enum_fields[1..]) |enum_field| {
                    values = values ++ ", ";
                    values = values ++ quote ++ enum_field.name ++ quote;
                }
                return values;
            },
        }
    }
};

/// Checks if the first argument (if it exists) is an identifier enum
pub fn wantsIdentEnum(attr: Tag) bool {
    switch (attr) {
        inline else => |tag| {
            const fields = getArguments(@field(attributes, @tagName(tag)));

            if (fields.len == 0) return false;
            const Unwrapped = UnwrapOptional(fields[0].type);
            if (@typeInfo(Unwrapped) != .Enum) return false;

            return Unwrapped.opts.enum_kind == .identifier;
        },
    }
}

pub fn diagnoseIdent(attr: Tag, arguments: *Arguments, ident: []const u8) ?Diagnostics.Message {
    switch (attr) {
        inline else => |tag| {
            const fields = getArguments(@field(attributes, @tagName(tag)));
            if (fields.len == 0)
                unreachable;

            const Unwrapped = UnwrapOptional(fields[0].type);
            if (@typeInfo(Unwrapped) != .Enum) unreachable;
            if (std.meta.stringToEnum(Unwrapped, normalize(ident))) |enum_val| {
                @field(@field(arguments, @tagName(tag)), fields[0].name) = enum_val;
                return null;
            }

            return Diagnostics.Message{
                .tag = .unknown_attr_enum,
                .extra = .{ .attrEnum = .{ .tag = attr } },
            };
        },
    }
}

pub fn wantsAlignment(attr: Tag, idx: usize) bool {
    inline for (@typeInfo(Tag).Enum.fields, 0..) |field, i| {
        if (field.value == @intFromEnum(attr)) {
            const decl = @typeInfo(attributes).Struct.decls[i];
            const fields = getArguments(@field(attributes, decl.name));

            if (idx >= fields.len) return false;
            inline for (fields, 0..) |arg_field, field_idx| {
                if (field_idx == idx) {
                    return UnwrapOptional(arg_field.type) == Alignment;
                }
            }
        }
    }
    unreachable;
}

pub fn diagnoseAlignment(attr: Tag, arguments: *Arguments, arg_idx: u32, val: Value, ty: Type, comp: *Compilation) ?Diagnostics.Message {
    switch (attr) {
        inline else => |tag| {
            const arg_fields = getArguments(@field(attributes, @tagName(tag)));
            if (arg_fields.len == 0) unreachable;

            switch (arg_idx) {
                inline 0...arg_fields.len - 1 => |arg_i| {
                    if (UnwrapOptional(arg_fields[arg_i].type) != Alignment) unreachable;

                    if (val.tag != .int) return Diagnostics.Message{ .tag = .alignas_unavailable };
                    if (val.compare(.lt, Value.int(0), ty, comp)) {
                        return Diagnostics.Message{ .tag = .negative_alignment, .extra = .{ .signed = val.signExtend(ty, comp) } };
                    }
                    const requested = std.math.cast(u29, val.data.int) orelse {
                        return Diagnostics.Message{ .tag = .maximum_alignment, .extra = .{ .unsigned = val.data.int } };
                    };
                    if (!std.mem.isValidAlign(requested)) return Diagnostics.Message{ .tag = .non_pow2_align };

                    @field(@field(arguments, @tagName(tag)), arg_fields[arg_i].name) = Alignment{ .requested = requested };
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
) ?Diagnostics.Message {
    switch (val.tag) {
        .int => {
            if (@typeInfo(wanted) == .Int) {
                @field(@field(arguments, decl.name), field.name) = val.getInt(wanted);
                return null;
            }
        },
        .bytes => {
            const bytes = @as([]const u8, val.data.bytes[0 .. val.data.bytes.len - 1]);
            if (wanted == []const u8) {
                @field(@field(arguments, decl.name), field.name) = bytes;
                return null;
            } else if (@typeInfo(wanted) == .Enum and wanted.opts.enum_kind == .string) {
                if (std.meta.stringToEnum(wanted, bytes)) |enum_val| {
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

pub fn diagnose(attr: Tag, arguments: *Arguments, arg_idx: u32, val: Value, node: Tree.Node) ?Diagnostics.Message {
    switch (attr) {
        inline else => |tag| {
            const decl = @typeInfo(attributes).Struct.decls[@intFromEnum(tag)];
            const max_arg_count = comptime maxArgCount(tag);
            if (arg_idx >= max_arg_count)
                return Diagnostics.Message{
                    .tag = .attribute_too_many_args,
                    .extra = .{
                        .attrArgCount = .{
                            .attribute = attr,
                            .expected = max_arg_count,
                        },
                    },
                };

            const arg_fields = getArguments(@field(attributes, decl.name));
            switch (arg_idx) {
                inline 0...arg_fields.len - 1 => |arg_i| {
                    return diagnoseField(decl, arg_fields[arg_i], UnwrapOptional(arg_fields[arg_i].type), arguments, val, node);
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
            alias: []const u8,
        };
    };
    pub const aligned = struct {
        const gnu = "aligned";
        const declspec = "align";

        const Args = struct {
            alignment: ?Alignment = null,
            __name_token: TokenIndex = undefined,
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
            segname: []const u8,
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
            segname: []const u8,
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
            msg: ?[]const u8 = null,
            __name_token: TokenIndex = undefined,
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
            message: []const u8,
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
            resolver: []const u8,
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
            alignment: []const u8,
            object_size: ?[]const u8 = null,
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
    pub const nothrow = struct {
        const gnu = "nothrow";
        const declspec = "nothrow";
    };
    pub const novtable = struct {
        const declspec = "novtable";
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
            name: []const u8,
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
            version: []const u8, // TODO: validate format "name2@nodename"
        };
    };
    pub const target = struct {
        const gnu = "target";
        const Args = struct {
            options: []const u8, // TODO: multiple arguments
        };
    };
    pub const target_clones = struct {
        const gnu = "target_clones";
        const Args = struct {
            options: []const u8, // TODO: multiple arguments
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
            msg: ?[]const u8 = null,
            __name_token: TokenIndex = undefined,
        };
    };
    pub const uninitialized = struct {
        const gnu = "uninitialized";
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
            uuid: []const u8,
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
            message: []const u8,
        };
    };
    pub const weak = struct {
        const gnu = "weak";
    };
    pub const weakref = struct {
        const gnu = "weakref";
        const Args = struct {
            target: ?[]const u8 = null,
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
            name: []const u8,
        };
    };
};

pub const Tag = std.meta.DeclEnum(attributes);

pub const Arguments = blk: {
    const decls = @typeInfo(attributes).Struct.decls;
    var union_fields: [decls.len]std.builtin.Type.UnionField = undefined;
    for (decls, &union_fields) |decl, *field| {
        field.* = .{
            .name = decl.name,
            .type = if (@hasDecl(@field(attributes, decl.name), "Args")) @field(attributes, decl.name).Args else void,
            .alignment = 0,
        };
    }

    break :blk @Type(.{
        .Union = .{
            .layout = .auto,
            .tag_type = null,
            .fields = &union_fields,
            .decls = &.{},
        },
    });
};

pub fn ArgumentsForTag(comptime tag: Tag) type {
    const decl = @typeInfo(attributes).Struct.decls[@intFromEnum(tag)];
    return if (@hasDecl(@field(attributes, decl.name), "Args")) @field(attributes, decl.name).Args else void;
}

pub fn initArguments(tag: Tag, nameToken: TokenIndex) Arguments {
    inline for (@typeInfo(Tag).Enum.fields) |field| {
        if (@intFromEnum(tag) == field.value) {
            var args = @unionInit(Arguments, field.name, undefined);
            const decl = @typeInfo(attributes).Struct.decls[field.value];
            if (@hasDecl(@field(attributes, decl.name), "Args") and @hasField(@field(attributes, decl.name).Args, "__name_token")) {
                @field(@field(args, field.name), "__name_token") = nameToken;
            }
            return args;
        }
    }
    unreachable;
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
    const decls = @typeInfo(attributes).Struct.decls;
    @setEvalBranchQuota(3000);
    inline for (decls, 0..) |decl, i| {
        if (@hasDecl(@field(attributes, decl.name), "gnu")) {
            if (mem.eql(u8, @field(attributes, decl.name).gnu, normalized)) {
                return @enumFromInt(i);
            }
        }
    }
    return null;
}

fn fromStringC23(namespace: ?[]const u8, name: []const u8) ?Tag {
    const normalized = normalize(name);
    if (namespace) |ns| {
        const normalized_ns = normalize(ns);
        if (mem.eql(u8, normalized_ns, "gnu")) {
            return fromStringGnu(normalized);
        }
        return null;
    }
    const decls = @typeInfo(attributes).Struct.decls;
    inline for (decls, 0..) |decl, i| {
        if (@hasDecl(@field(attributes, decl.name), "c23")) {
            if (mem.eql(u8, @field(attributes, decl.name).c23, normalized)) {
                return @enumFromInt(i);
            }
        }
    }
    return null;
}

fn fromStringDeclspec(name: []const u8) ?Tag {
    const decls = @typeInfo(attributes).Struct.decls;
    inline for (decls, 0..) |decl, i| {
        if (@hasDecl(@field(attributes, decl.name), "declspec")) {
            if (mem.eql(u8, @field(attributes, decl.name).declspec, name)) {
                return @enumFromInt(i);
            }
        }
    }
    return null;
}

fn normalize(name: []const u8) []const u8 {
    if (name.len >= 4 and mem.startsWith(u8, name, "__") and mem.endsWith(u8, name, "__")) {
        return name[2 .. name.len - 2];
    }
    return name;
}

fn ignoredAttrErr(p: *Parser, token: TokenIndex, attr: Attribute.Tag, context: []const u8) !void {
    const stringsTop = p.strings.items.len;
    defer p.strings.items.len = stringsTop;

    try p.strings.writer().print("attribute '{s}' ignored on {s}", .{ @tagName(attr), context });
    const str = try p.comp.diag.arena.allocator().dupe(u8, p.strings.items[stringsTop..]);
    try p.errStr(.ignored_attribute, token, str);
}

pub const applyParameterAttributes = applyVariableAttributes;
pub fn applyVariableAttributes(p: *Parser, ty: Type, attrBufferStart: usize) !Type {
    const attrs = p.attrBuffer.items(.attr)[attrBufferStart..];
    _ = attrs;
    return ty;
}

pub const applyFieldAttributes = applyTypeAttributes;
pub fn applyTypeAttributes(p: *Parser, ty: Type, attrBufferStart: usize, tag: ?Diagnostics.Tag) !Type {
    const attrs = p.attrBuffer.items(.attr)[attrBufferStart..];
    const toks = p.attrBuffer.items(.tok)[attrBufferStart..];
    p.attrApplicationBuffer.items.len = 0;

    var baseTy = ty;
    if (baseTy.specifier == .Attributed)
        baseTy = baseTy.data.attributed.base;

    for (attrs, 0..) |attr, i| switch (attr.tag) {
        .@"packed",
        .may_alias,
        .deprecated,
        .unavailable,
        .unused,
        .warn_if_not_aligned,
        .mode,
        => try p.attrApplicationBuffer.append(p.gpa, attr),

        .transparent_union => try attr.applyTransparentUnion(p, toks[i], baseTy),
        .vector_size => try attr.applyVectorSize(p, toks[i], &baseTy),
        .aligned => try attr.applyAligned(p, baseTy, tag),

        .designated_init => if (baseTy.is(.Struct)) {
            try p.attrApplicationBuffer.append(p.gpa, attr);
        } else {
            try p.errToken(.designated_init_invalid, toks[i]);
        },

        .alloc_size,
        .copy,
        .scalar_storage_order,
        .nonstring,
        => std.debug.panic("apply type attribute {s}", .{@tagName(attr.tag)}),

        else => try ignoredAttrErr(p, toks[i], attr.tag, "types"),
    };
    return ty.withAttributes(p.arena, p.attrApplicationBuffer.items);
}

pub fn applyFunctionAttributes(p: *Parser, ty: Type, attrBufferStart: usize) !Type {
    const attrs = p.attrBuffer.items(.attr)[attrBufferStart..];
    const toks = p.attrBuffer.items(.tok)[attrBufferStart..];
    _ = attrs;
    _ = toks;
    return ty;
}

pub fn applyLabelAttributes(p: *Parser, ty: Type, attrBufferStart: usize) !Type {
    const attrs = p.attrBuffer.items(.attr)[attrBufferStart..];
    const toks = p.attrBuffer.items(.tok)[attrBufferStart..];
    p.attrApplicationBuffer.items.len = 0;
    for (attrs, 0..) |attr, i| switch (attr.tag) {
        .cold, .hot, .unused => try p.attrApplicationBuffer.append(p.gpa, attr),
        else => try ignoredAttrErr(p, toks[i], attr.tag, "labels"),
    };
    return ty.withAttributes(p.arena, p.attrApplicationBuffer.items);
}

pub fn applyStatementAttributes(p: *Parser, ty: Type, expr_start: TokenIndex, attrBufferStart: usize) !Type {
    const attrs = p.attrBuffer.items(.attr)[attrBufferStart..];
    const toks = p.attrBuffer.items(.tok)[attrBufferStart..];
    p.attrApplicationBuffer.items.len = 0;
    for (attrs, 0..) |attr, i| switch (attr.tag) {
        .fallthrough => if (p.getCurrToken() != .KeywordCase and p.getCurrToken() != .KeywordDefault) {
            // TODO: this condition is not completely correct; the last statement of a compound
            // statement is also valid if it precedes a switch label (so intervening '}' are ok,
            // but only if they close a compound statement)
            try p.errToken(.invalid_fallthrough, expr_start);
        } else {
            try p.attrApplicationBuffer.append(p.gpa, attr);
        },
        else => try p.errStr(.cannot_apply_attribute_to_statement, toks[i], @tagName(attr.tag)),
    };
    return ty.withAttributes(p.arena, p.attrApplicationBuffer.items);
}

pub fn applyEnumeratorAttributes(p: *Parser, ty: Type, attrBufferStart: usize) !Type {
    const attrs = p.attrBuffer.items(.attr)[attrBufferStart..];
    const toks = p.attrBuffer.items(.tok)[attrBufferStart..];
    p.attrApplicationBuffer.items.len = 0;
    for (attrs, 0..) |attr, i| switch (attr.tag) {
        .deprecated, .unavailable => try p.attrApplicationBuffer.append(p.gpa, attr),
        else => try ignoredAttrErr(p, toks[i], attr.tag, "enums"),
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

        const str = try std.fmt.allocPrint(p.comp.diag.arena.allocator(), "'{s}' ({d}", .{ field.name, fieldSize });
        try p.errStr(.transparent_union_size, field.nameToken, str);
        return p.errExtra(.transparent_union_size_note, fields[0].nameToken, .{ .unsigned = firstFieldSize });
    }

    try p.attrApplicationBuffer.append(p.gpa, attr);
}

fn applyVectorSize(attr: Attribute, p: *Parser, tok: TokenIndex, ty: *Type) !void {
    _ = attr;
    _ = tok;
    _ = ty;
    return p.todo("apply vector_size attribute");
}
