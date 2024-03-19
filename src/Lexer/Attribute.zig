const std = @import("std");
const mem = std.mem;
const Tree = @import("../AST/AST.zig");
const Value = @import("../AST/Value.zig");
const Diagnostics = @import("../Basic/Diagnostics.zig");
const Parser = @import("../Parser/Parser.zig");
const NodeIndex = Tree.NodeIndex;
const TokenIndex = Tree.TokenIndex;
const ZigType = std.builtin.Type;

const Attribute = @This();

tag: Tag,
args: Arguments,

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
                    if (!mem.eql(u8, arg_field.name, "__name_tok") and @typeInfo(arg_field.type) != .Optional)
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
                    if (!mem.eql(u8, arg_field.name, "__name_tok"))
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
                .extra = .{ .attrEnum = .{ .tag = attr, .actual = ident } },
            };
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
                        .extra = .{
                            .attrEnum = .{
                                .tag = std.meta.stringToEnum(Tag, decl.name).?,
                                .actual = bytes,
                            },
                        },
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
            size_index: ?u32,
        };
    };
    pub const alias = struct {
        const Args = struct {
            alias: []const u8,
        };
    };
    pub const aligned = struct {
        const Args = struct {
            alignment: ?Alignment,
        };
    };
    pub const alloc_align = struct {
        const Args = struct {
            position: u32,
        };
    };
    pub const alloc_size = struct {
        const Args = struct {
            position_1: u32,
            position_2: ?u32 = null,
        };
    };
    pub const always_inline = struct {};
    pub const artificial = struct {};
    pub const assume_aligned = struct {
        const Args = struct {
            alignment: Alignment,
            offset: ?u32 = null,
        };
    };
    pub const cleanup = struct {
        const Args = struct {
            function: Identifier,
        };
    };
    pub const cold = struct {};
    pub const common = struct {};
    pub const @"const" = struct {};
    pub const constructor = struct {
        const Args = struct {
            priority: ?u32 = null,
        };
    };
    pub const copy = struct {
        const Args = struct {
            function: Identifier,
        };
    };
    pub const deprecated = struct {
        const Args = struct {
            msg: ?[]const u8,
        };
    };
    pub const designated_init = struct {};
    pub const destructor = struct {
        const Args = struct {
            priority: ?u32 = null,
        };
    };
    pub const @"error" = struct {
        const Args = struct {
            message: []const u8,
        };
    };
    pub const externally_visible = struct {};
    const fallthrough = struct {};
    const flatten = struct {};
    const format = struct {
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
        const Args = struct {
            string_index: u32,
        };
    };
    pub const gnu_inline = struct {};
    pub const hot = struct {};
    pub const ifunc = struct {
        const Args = struct {
            resolver: []const u8,
        };
    };
    pub const interrupt = struct {};
    pub const interrupt_handler = struct {};
    pub const leaf = struct {};
    pub const malloc = struct {};
    pub const may_alias = struct {};
    pub const mode = struct {
        const Args = struct {
            mode: enum {
                byte,
                __byte__,
                word,
                __word__,
                pointer,
                __pointer__,

                const opts = struct {
                    const enum_kind = .identifier;
                };
            },
        };
    };
    pub const no_address_safety_analysis = struct {};
    pub const no_icf = struct {};
    pub const no_instrument_function = struct {};
    pub const no_profile_instrument_function = struct {};
    pub const no_reorder = struct {};
    pub const no_sanitize = struct {
        /// Todo: represent args as union?
        const Args = struct {
            alignment: []const u8,
            object_size: ?[]const u8 = null,
        };
    };
    pub const no_sanitize_address = struct {};
    pub const no_sanitize_coverage = struct {};
    pub const no_sanitize_thread = struct {};
    pub const no_sanitize_undefined = struct {};
    pub const no_split_stack = struct {};
    pub const no_stack_limit = struct {};
    pub const no_stack_protector = struct {};
    pub const noclone = struct {};
    pub const nocommon = struct {};
    pub const noinit = struct {};
    pub const @"noinline" = struct {};
    pub const noipa = struct {};
    // TODO: arbitrary number of arguments
    //    const nonnull = struct {
    //        const Args = struct {
    //            arg_index: []const u32,
    //        };
    //    };
    pub const nonstring = struct {};
    pub const noplt = struct {};
    pub const @"noreturn" = struct {};
    pub const nothrow = struct {};
    // TODO: union args ?
    //    const optimize = struct {
    //        const Args = struct {
    //            optimize, // u32 | []const u8 -- optimize?
    //        };
    //    };
    pub const @"packed" = struct {};
    pub const patchable_function_entry = struct {};
    pub const persistent = struct {};
    pub const pure = struct {};
    pub const retain = struct {};
    pub const returns_nonnull = struct {};
    pub const returns_twice = struct {};
    pub const scalar_storage_order = struct {
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
        const Args = struct {
            name: []const u8,
        };
    };
    pub const sentinel = struct {
        const Args = struct {
            position: ?u32 = null,
        };
    };
    pub const simd = struct {
        const Args = struct {
            mask: ?enum {
                notinbranch,
                inbranch,

                const opts = struct {
                    const enum_kind = .string;
                };
            },
        };
    };
    pub const stack_protect = struct {};
    pub const symver = struct {
        const Args = struct {
            version: []const u8, // TODO: validate format "name2@nodename"
        };
    };
    pub const target = struct {
        const Args = struct {
            options: []const u8, // TODO: multiple arguments
        };
    };
    pub const target_clones = struct {
        const Args = struct {
            options: []const u8, // TODO: multiple arguments
        };
    };
    pub const tls_model = struct {
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
    pub const transparent_union = struct {};
    pub const unavailable = struct {
        const Args = struct {
            msg: ?[]const u8,
        };
    };
    pub const uninitialized = struct {};
    pub const unused = struct {};
    pub const used = struct {};
    pub const vector_size = struct {
        const Args = struct {
            bytes: u32, // TODO: validate "The bytes argument must be a positive power-of-two multiple of the base type size"
        };
    };
    pub const visibility = struct {
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
        const Args = struct {
            alignment: Alignment,
        };
    };
    pub const warn_unused_result = struct {};
    pub const warning = struct {
        const Args = struct {
            message: []const u8,
        };
    };
    pub const weak = struct {};
    pub const weakref = struct {
        const Args = struct {
            target: ?[]const u8,
        };
    };
    pub const zero_call_used_regs = struct {
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

pub fn initArguments(tag: Tag) Arguments {
    inline for (@typeInfo(Tag).Enum.fields) |field| {
        if (@intFromEnum(tag) == field.value) {
            return @unionInit(Arguments, field.name, undefined);
        }
    }
    unreachable;
}

pub fn fromString(name: []const u8) ?Tag {
    @setEvalBranchQuota(3000);
    return std.meta.stringToEnum(Tag, normalize(name));
}

fn normalize(name: []const u8) []const u8 {
    if (name.len >= 4 and mem.startsWith(u8, name, "__") and mem.endsWith(u8, name, "__")) {
        return name[2 .. name.len - 2];
    }
    return name;
}
