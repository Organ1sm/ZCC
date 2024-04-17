const std = @import("std");
const builtin = @import("builtin");
const Source = @import("Source.zig");
const TokenType = @import("TokenType.zig").TokenType;
const Tree = @import("../AST/AST.zig");
const Compilation = @import("Compilation.zig");
const Attribute = @import("../Lexer/Attribute.zig");
const DiagnosticsMessages = @import("../Basic/DiagnosticsMessages.zig");
const util = @import("../Basic/Util.zig");
const IsWindows = @import("builtin").os.tag == .windows;

const Allocator = std.mem.Allocator;
const Diagnostics = @This();

pub const Message = struct {
    tag: Tag,
    loc: Source.Location = .{},
    extra: Extra = .{ .none = {} },
    kind: Kind = undefined,

    pub const Extra = union {
        str: []const u8,
        tokenId: struct {
            expected: TokenType,
            actual: TokenType,
        },
        expectedTokenId: TokenType,

        arguments: struct {
            expected: u32,
            actual: u32,
        },

        codePoints: struct {
            actual: u21,
            resembles: u21,
        },
        attrArgCount: struct {
            attribute: Attribute.Tag,
            expected: u32,
        },
        attrArgType: struct {
            expected: Attribute.ArgumentType,
            actual: Attribute.ArgumentType,
        },
        attrEnum: struct {
            tag: Attribute.Tag,
        },
        ignoredRecordAttr: struct {
            tag: Attribute.Tag,
            specifier: enum { @"struct", @"union", @"enum" },
        },
        actualCodePoint: u21,
        pow2AsString: u8,
        unsigned: u64,
        signed: i64,
        none: void,
    };
};

pub const Tag = std.meta.DeclEnum(DiagnosticsMessages);

pub const Kind = enum(u4) { @"fatal error", @"error", note, warning, off, default };

pub const Options = packed struct {
    // do not directly use these, instead add `const NAME = true;`
    all: Kind = .default,
    extra: Kind = .default,
    pedantic: Kind = .default,

    @"unsupported-pragma": Kind = .default,
    @"c99-extensions": Kind = .default,
    @"implicit-int": Kind = .default,
    @"duplicate-decl-specifier": Kind = .default,
    @"missing-declaration": Kind = .default,
    @"extern-initializer": Kind = .default,
    @"implicit-function-declaration": Kind = .default,
    @"unused-value": Kind = .default,
    @"unreachable-code": Kind = .default,
    @"unknown-warning-option": Kind = .default,
    @"gnu-empty-struct": Kind = .default,
    @"gnu-alignof-expression": Kind = .default,
    @"macro-redefined": Kind = .default,
    @"generic-qual-type": Kind = .default,
    multichar: Kind = .default,
    @"pointer-integer-compare": Kind = .default,
    @"compare-distinct-pointer-types": Kind = .default,
    @"literal-conversion": Kind = .default,
    @"cast-qualifiers": Kind = .default,
    @"array-bounds": Kind = .default,
    @"int-conversion": Kind = .default,
    @"pointer-type-mismatch": Kind = .default,
    @"c2x-extensions": Kind = .default,
    @"incompatible-pointer-types": Kind = .default,
    @"excess-initializers": Kind = .default,
    @"division-by-zero": Kind = .default,
    @"initializer-overrides": Kind = .default,
    @"incompatible-pointer-types-discards-qualifiers": Kind = .default,
    @"unknown-attributes": Kind = .default,
    @"ignored-attributes": Kind = .default,
    @"builtin-macro-redefined": Kind = .default,
    @"gnu-label-as-value": Kind = .default,
    @"malformed-warning-check": Kind = .default,
    @"#pragma-messages": Kind = .default,
    @"newline-eof": Kind = .default,
    @"empty-translation-unit": Kind = .default,
    @"implicitly-unsigned-literal": Kind = .default,
    @"c99-compat": Kind = .default,
    @"unicode-zero-width": Kind = .default,
    @"unicode-homoglyph": Kind = .default,
    @"return-type": Kind = .default,
    @"dollar-in-identifier-extension": Kind = .default,
    @"unknown-pragmas": Kind = .default,
    @"predefined-identifier-outside-function": Kind = .default,
    @"many-braces-around-scalar-init": Kind = .default,
    uninitialized: Kind = .default,
    @"gnu-statement-expression": Kind = .default,
    @"gnu-imaginary-constant": Kind = .default,
    @"gnu-complex-integer": Kind = .default,
    @"ignored-qualifiers": Kind = .default,
    @"integer-overflow": Kind = .default,
    @"extra-semi": Kind = .default,
    @"gnu-binary-literal": Kind = .default,
    @"variadic-macros": Kind = .default,
    varargs: Kind = .default,
    @"#warnings": Kind = .default,
    @"deprecated-declarations": Kind = .default,
    @"backslash-newline-escape": Kind = .default,
    @"pointer-to-int-cast": Kind = .default,
    @"gnu-case-range": Kind = .default,
    @"c++-compat": Kind = .default,
    vla: Kind = .default,
    @"float-overflow-conversion": Kind = .default,
    @"float-zero-conversion": Kind = .default,
    @"float-conversion": Kind = .default,
    @"gnu-folding-constant": Kind = .default,
    undef: Kind = .default,
    @"gnu-include-next": Kind = .default,
    @"include-next-outside-header": Kind = .default,
    @"include-next-absolute-path": Kind = .default,
    @"ignored-pragmas": Kind = .default,
    @"enum-too-large": Kind = .default,
    @"fixed-enum-extension": Kind = .default,
    @"designated-init": Kind = .default,
    @"attribute-warning": Kind = .default,
    @"invalid-noreturn": Kind = .default,
    @"zero-length-array": Kind = .default,
    @"old-style-flexible-struct": Kind = .default,
};

list: std.ArrayListUnmanaged(Message) = .{},
arena: std.heap.ArenaAllocator,
color: bool = true,
fatalErrors: bool = false,
options: Options = .{},
errors: u32 = 0,
macroBacktraceLimit: u32 = 6,

pub fn warningExists(name: []const u8) bool {
    inline for (std.meta.fields(Options)) |f| {
        if (std.mem.eql(u8, f.name, name)) return true;
    }
    return false;
}

pub fn set(diag: *Diagnostics, name: []const u8, to: Kind) !void {
    inline for (std.meta.fields(Options)) |f| {
        if (std.mem.eql(u8, f.name, name)) {
            @field(diag.options, f.name) = to;
            return;
        }
    }
    try diag.add(.{
        .tag = .unknown_warning,
        .extra = .{ .str = name },
    }, &.{});
}

pub fn setAll(diag: *Diagnostics, to: Kind) void {
    inline for (std.meta.fields(Options)) |f| {
        @field(diag.options, f.name) = to;
    }
}

pub fn init(gpa: Allocator) Diagnostics {
    return .{
        .arena = std.heap.ArenaAllocator.init(gpa),
    };
}

pub fn deinit(diag: *Diagnostics) void {
    diag.list.deinit(diag.arena.allocator());
    diag.arena.deinit();
}

pub fn add(diag: *Diagnostics, msg: Message, expansionLocs: []const Source.Location) Compilation.Error!void {
    const kind = diag.tagKind(msg.tag);
    if (kind == .off) return;
    var copy = msg;
    copy.kind = kind;

    if (expansionLocs.len != 0)
        copy.loc = expansionLocs[expansionLocs.len - 1];

    try diag.list.append(diag.arena.allocator(), copy);

    if (expansionLocs.len != 0) {
        // Add macro backtrace notes in reverse order omitting from the middle if needed.
        var i = expansionLocs.len - 1;
        const half = diag.macroBacktraceLimit / 2;
        const limit = if (i < diag.macroBacktraceLimit) 0 else i - half;
        try diag.list.ensureUnusedCapacity(
            diag.arena.allocator(),
            if (limit == 0) expansionLocs.len else diag.macroBacktraceLimit + 1,
        );
        while (i > limit) {
            i -= 1;
            diag.list.appendAssumeCapacity(.{
                .tag = .expanded_from_here,
                .kind = .note,
                .loc = expansionLocs[i],
            });
        }
        if (limit != 0) {
            diag.list.appendAssumeCapacity(.{
                .tag = .skipping_macro_backtrace,
                .kind = .note,
                .extra = .{ .unsigned = expansionLocs.len - diag.macroBacktraceLimit },
            });
            i = half - 1;
            while (i > 0) {
                i -= 1;
                diag.list.appendAssumeCapacity(.{
                    .tag = .expanded_from_here,
                    .kind = .note,
                    .loc = expansionLocs[i],
                });
            }
        }

        diag.list.appendAssumeCapacity(.{
            .tag = .expanded_from_here,
            .kind = .note,
            .loc = msg.loc,
        });
    }

    if (kind == .@"fatal error" or (kind == .@"error" and diag.fatalErrors))
        return error.FatalError;
}

pub fn fatal(
    diag: *Diagnostics,
    path: []const u8,
    line: []const u8,
    lineNo: u32,
    col: u32,
    comptime fmt: []const u8,
    args: anytype,
) Compilation.Error {
    var m = MsgWriter.init(diag.color);
    defer m.deinit();

    m.location(path, lineNo, col);
    m.start(.@"fatal error");
    m.print(fmt, args);
    m.end(line, col, false);
    return error.FatalError;
}

pub fn fatalNoSrc(diag: *Diagnostics, comptime fmt: []const u8, args: anytype) error{FatalError} {
    if (!diag.color) {
        std.debug.print("fatal error: " ++ fmt ++ "\n", args);
    } else {
        const std_err = std.io.getStdErr().writer();
        util.setColor(.red, std_err);
        std_err.writeAll("fatal error: ") catch {};
        util.setColor(.white, std_err);
        std_err.print(fmt ++ "\n", args) catch {};
        util.setColor(.reset, std_err);
    }
    return error.FatalError;
}

pub fn render(comp: *Compilation) void {
    if (comp.diag.list.items.len == 0) return;
    var m = MsgWriter.init(comp.diag.color);
    defer m.deinit();

    renderExtra(comp, &m);
}

pub fn renderExtra(comp: *Compilation, m: anytype) void {
    var errors: u32 = 0;
    var warnings: u32 = 0;
    for (comp.diag.list.items) |msg| {
        switch (msg.kind) {
            .@"fatal error", .@"error" => errors += 1,
            .warning => warnings += 1,
            .note => {},
            .off => continue, // happens if an error is added before it is disabled
            .default => unreachable,
        }

        var line: ?[]const u8 = null;
        var endWithSplice = false;
        const width = if (msg.loc.id != .unused) blk: {
            var loc = msg.loc;
            switch (msg.tag) {
                .escape_sequence_overflow,
                .invalid_universal_character,
                .non_standard_escape_char,
                // use msg.extra.unsigned for index into string literal
                => loc.byteOffset += @as(u32, @truncate(msg.extra.unsigned)),
                else => {},
            }

            const source = comp.getSource(loc.id);
            var lineAndCol = source.getLineCol(loc);
            line = lineAndCol.line;
            endWithSplice = lineAndCol.endWithSplic;
            if (msg.tag == .backslash_newline_escape) {
                line = lineAndCol.line[0 .. lineAndCol.col - 1];
                lineAndCol.col += 1;
                lineAndCol.width += 1;
            }
            m.location(source.path, lineAndCol.lineNO, lineAndCol.col);
            break :blk lineAndCol.width;
        } else 0;

        m.start(msg.kind);
        @setEvalBranchQuota(1500);
        inline for (std.meta.fields(Tag)) |field| {
            if (field.value == @intFromEnum(msg.tag)) {
                const info = @field(DiagnosticsMessages, field.name);
                if (@hasDecl(info, "extra")) {
                    switch (info.extra) {
                        .str => m.print(info.msg, .{msg.extra.str}),
                        .tok_id => m.print(info.msg, .{
                            msg.extra.tokenId.expected.symbol(),
                            msg.extra.tokenId.actual.symbol(),
                        }),
                        .tok_id_expected => m.print(info.msg, .{msg.extra.expectedTokenId.symbol()}),
                        .arguments => m.print(info.msg, .{ msg.extra.arguments.expected, msg.extra.arguments.actual }),
                        .codepoints => m.print(info.msg, .{
                            msg.extra.codePoints.actual,
                            msg.extra.codePoints.resembles,
                        }),
                        .attr_arg_count => m.print(info.msg, .{
                            @tagName(msg.extra.attrArgCount.attribute),
                            msg.extra.attrArgCount.expected,
                        }),
                        .attr_arg_type => m.print(info.msg, .{
                            msg.extra.attrArgType.expected.toString(),
                            msg.extra.attrArgType.actual.toString(),
                        }),
                        .actual_codepoint => m.print(info.msg, .{msg.extra.actualCodePoint}),
                        .pow_2_as_string => m.print(info.msg, .{switch (msg.extra.pow2AsString) {
                            63 => "9223372036854775808",
                            64 => "18446744073709551616",
                            127 => "170141183460469231731687303715884105728",
                            128 => "340282366920938463463374607431768211456",
                            else => unreachable,
                        }}),
                        .unsigned => m.print(info.msg, .{msg.extra.unsigned}),
                        .signed => m.print(info.msg, .{msg.extra.signed}),
                        .attr_enum => m.print(info.msg, .{
                            @tagName(msg.extra.attrEnum.tag),
                            Attribute.Formatting.choices(msg.extra.attrEnum.tag),
                        }),
                        .ignored_record_attr => m.print(info.msg, .{
                            @tagName(msg.extra.ignoredRecordAttr.tag),
                            @tagName(msg.extra.ignoredRecordAttr.specifier),
                        }),
                        else => @compileError("invalid extra kind " ++ @tagName(info.extra)),
                    }
                } else {
                    m.write(info.msg);
                }

                if (@hasDecl(info, "opt")) {
                    if (msg.kind == .@"error" and info.kind != .@"error") {
                        m.print(" [-Werror, -W{s}]", .{info.opt});
                    } else if (msg.kind != .note) {
                        m.print(" [-W{s}]", .{info.opt});
                    }
                }
            }
        }

        m.end(line, width, endWithSplice);
    }

    const ws: []const u8 = if (warnings == 1) "" else "s";
    const es: []const u8 = if (errors == 1) "" else "s";
    if (errors != 0 and warnings != 0) {
        m.print("{d} warning{s} and {d} error{s} generated.\n", .{ warnings, ws, errors, es });
    } else if (warnings != 0) {
        m.print("{d} warning{s} generated.\n", .{ warnings, ws });
    } else if (errors != 0) {
        m.print("{d} error{s} generated.\n", .{ errors, es });
    }

    comp.diag.list.items.len = 0;
    comp.diag.errors += errors;
}

fn tagKind(diag: *Diagnostics, tag: Tag) Kind {
    // XXX: horrible hack, do not do this
    const comp: *Compilation = @fieldParentPtr("diag", diag);

    var kind: Kind = undefined;
    inline for (std.meta.fields(Tag)) |field| {
        if (field.value == @intFromEnum(tag)) {
            const info = @field(DiagnosticsMessages, field.name);
            kind = info.kind;

            // stage1 doesn't like when I combine these ifs
            if (@hasDecl(info, "all")) {
                if (diag.options.all != .default)
                    kind = diag.options.all;
            }

            if (@hasDecl(info, "w_extra")) {
                if (diag.options.extra != .default)
                    kind = diag.options.extra;
            }

            if (@hasDecl(info, "pedantic")) {
                if (diag.options.pedantic != .default)
                    kind = diag.options.pedantic;
            }

            if (@hasDecl(info, "opt")) {
                if (@field(diag.options, info.opt) != .default)
                    kind = @field(diag.options, info.opt);
            }

            if (@hasDecl(info, "suppress_version"))
                if (comp.langOpts.standard.atLeast(info.suppress_version))
                    return .off;

            if (@hasDecl(info, "suppress_gnu"))
                if (comp.langOpts.standard.isExplicitGNU())
                    return .off;

            if (@hasDecl(info, "suppress_language_option"))
                if (!@field(comp.langOpts, info.suppress_language_option))
                    return .off;

            if (kind == .@"error" and diag.fatalErrors)
                kind = .@"fatal error";

            return kind;
        }
    }
    unreachable;
}

const MsgWriter = struct {
    w: std.io.BufferedWriter(4096, std.fs.File.Writer),
    color: bool,

    fn init(color: bool) MsgWriter {
        std.debug.getStderrMutex().lock();
        return .{
            .w = std.io.bufferedWriter(std.io.getStdErr().writer()),
            .color = color,
        };
    }

    fn deinit(m: *MsgWriter) void {
        m.w.flush() catch {};
        std.debug.getStderrMutex().unlock();
    }

    fn print(m: *MsgWriter, comptime fmt: []const u8, args: anytype) void {
        m.w.writer().print(fmt, args) catch {};
    }

    fn write(m: *MsgWriter, msg: []const u8) void {
        m.w.writer().writeAll(msg) catch {};
    }

    fn setColor(m: *MsgWriter, color: util.Color) void {
        util.setColor(color, m.w.writer());
    }

    fn location(m: *MsgWriter, path: []const u8, line: u32, col: u32) void {
        const prefix = if (std.fs.path.dirname(path) == null and path[0] != '<') "." ++ std.fs.path.sep_str else "";
        if (m.color) {
            m.print("{s}{s}:{d}:{d}: ", .{ prefix, path, line, col });
        } else {
            m.setColor(.white);
            m.print("{s}{s}:{d}:{d}: ", .{ prefix, path, line, col });
        }
    }

    fn start(m: *MsgWriter, kind: Kind) void {
        if (!m.color) {
            m.print("{s}: ", .{@tagName(kind)});
        } else {
            switch (kind) {
                .@"fatal error", .@"error" => m.setColor(.red),
                .note => m.setColor(.cyan),
                .warning => m.setColor(.purple),
                .off, .default => unreachable,
            }
            m.write(switch (kind) {
                .@"fatal error" => "fatal error: ",
                .@"error" => "error: ",
                .note => "note: ",
                .warning => "warning: ",
                .off, .default => unreachable,
            });
            m.setColor(.white);
        }
    }

    fn end(m: *MsgWriter, maybe_line: ?[]const u8, col: u32, endWithSplice: bool) void {
        const line = maybe_line orelse {
            m.write("\n");
            return;
        };
        const trailer = if (endWithSplice) "\\ " else "";
        if (!m.color) {
            m.print("\n{s}{s}\n", .{ line, trailer });
            m.print("{s: >[1]}^\n", .{ "", col });
        } else {
            m.setColor(.reset);
            m.print("\n{s}{s}\n{s: >[3]}", .{ line, trailer, "", col });
            m.setColor(.green);
            m.write("^\n");
            m.setColor(.reset);
        }
    }
};
