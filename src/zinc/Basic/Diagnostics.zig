const std = @import("std");
const builtin = @import("builtin");
const Source = @import("Source.zig");
const TokenType = @import("TokenType.zig").TokenType;
const Tree = @import("../AST/AST.zig");
const Compilation = @import("Compilation.zig");
const Attribute = @import("../Lexer/Attribute.zig");
const Builtins = @import("../Builtins.zig");
const Builtin = Builtins.Builtin;
const Header = @import("../Builtins/Properties.zig").Header;
const LangOpts = @import("../Basic/LangOpts.zig");
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
            tagKind: enum { @"struct", @"union", @"enum" },
        },
        builtinWithHeader: struct {
            builtin: Builtin.Tag,
            header: Header,
        },
        invalidEscape: struct {
            offset: u32,
            char: u8,
        },
        actualCodePoint: u21,
        ascii: u7,
        pow2AsString: u8,
        unsigned: u64,
        offset: u64,
        signed: i64,
        none: void,
    };
};

const Properties = struct {
    msg: []const u8,
    kind: Kind,
    extra: std.meta.FieldEnum(Message.Extra) = .none,
    opt: ?u8 = null,
    all: bool = false,
    w_extra: bool = false,
    pedantic: bool = false,
    suppress_version: ?LangOpts.Standard = null,
    suppress_unless_version: ?LangOpts.Standard = null,
    suppress_gnu: bool = false,
    suppress_gcc: bool = false,
    suppress_clang: bool = false,
    suppress_msvc: bool = false,

    pub fn makeOpt(comptime str: []const u8) u16 {
        return @offsetOf(Options, str);
    }

    pub fn getKind(prop: Properties, options: *Options) Kind {
        const opt = @as([*]Kind, @ptrCast(options))[prop.opt orelse return prop.kind];
        if (opt == .default) return prop.kind;
        return opt;
    }

    pub const max_bits = Compilation.BitIntMaxBits;
};

pub const Tag = @import("Diagnostics/messages.def").with(Properties).Tag;

pub const Kind = enum { @"fatal error", @"error", note, warning, off, default };

pub const Options = struct {
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
    @"c23-extensions": Kind = .default,
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
    unicode: Kind = .default,
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
    @"gnu-zero-variadic-macro-arguments": Kind = .default,
    @"expansion-to-defined": Kind = .default,
    @"main-return-type": Kind = .default,
    @"bit-int-extension": Kind = .default,
    @"keyword-macro": Kind = .default,
    @"pointer-arith": Kind = .default,
    @"sizeof-array-argument": Kind = .default,
    @"pre-c23-compat": Kind = .default,
    @"pointer-bool-conversion": Kind = .default,
    @"string-conversion": Kind = .default,
    @"gnu-auto-type": Kind = .default,
    @"gnu-union-cast": Kind = .default,
    @"pointer-sign": Kind = .default,
    @"deprecated-non-prototype": Kind = .default,
    @"fuse-ld-path": Kind = .default,
    @"language-extension-token": Kind = .default,
    @"complex-component-init": Kind = .default,
    @"microsoft-include": Kind = .default,
    @"microsoft-end-of-file": Kind = .default,
    @"invalid-source-encoding": Kind = .default,
    @"four-char-constants": Kind = .default,
    @"unknown-escape-sequence": Kind = .default,
    @"invalid-pp-token": Kind = .default,
    @"duplicate-embed-param": Kind = .default,
    @"unsupported-embed-param": Kind = .default,
    @"unused-result": Kind = .default,
    @"atomic-access": Kind = .default,
};

list: std.ArrayListUnmanaged(Message) = .{},
arena: std.heap.ArenaAllocator,
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
    try diag.addExtra(
        .{},
        .{ .tag = .unknown_warning, .extra = .{ .str = name } },
        &.{},
        true,
    );
}

pub fn setAll(diag: *Diagnostics, to: Kind) void {
    inline for (std.meta.fields(Options)) |f| {
        @field(diag.options, f.name) = to;
    }
}

pub fn init(gpa: Allocator) Diagnostics {
    return .{ .arena = std.heap.ArenaAllocator.init(gpa) };
}

pub fn deinit(diag: *Diagnostics) void {
    diag.list.deinit(diag.arena.child_allocator);
    diag.arena.deinit();
}

pub fn add(comp: *Compilation, msg: Message, expansionLocs: []const Source.Location) Compilation.Error!void {
    return comp.diagnostics.addExtra(comp.langOpts, msg, expansionLocs, true);
}

pub fn addExtra(
    diag: *Diagnostics,
    langopts: LangOpts,
    msg: Message,
    expansionLocs: []const Source.Location,
    noteMsgLoc: bool,
) Compilation.Error!void {
    const kind = diag.tagKind(msg.tag, langopts);
    if (kind == .off) return;
    var copy = msg;
    copy.kind = kind;

    if (expansionLocs.len != 0)
        copy.loc = expansionLocs[expansionLocs.len - 1];

    try diag.list.append(diag.arena.child_allocator, copy);

    if (expansionLocs.len != 0) {
        // Add macro backtrace notes in reverse order omitting from the middle if needed.
        var i = expansionLocs.len - 1;
        const half = diag.macroBacktraceLimit / 2;
        const limit = if (i < diag.macroBacktraceLimit) 0 else i - half;
        try diag.list.ensureUnusedCapacity(
            diag.arena.child_allocator,
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

        if (noteMsgLoc) diag.list.appendAssumeCapacity(.{
            .tag = .expanded_from_here,
            .kind = .note,
            .loc = msg.loc,
        });
    }

    if (kind == .@"fatal error" or (kind == .@"error" and diag.fatalErrors))
        return error.FatalError;
}

pub fn defaultMsgWriter(config: std.io.tty.Config) MsgWriter {
    return MsgWriter.init(config);
}

pub fn render(comp: *Compilation, config: std.io.tty.Config) void {
    if (comp.diagnostics.list.items.len == 0) return;

    var m = defaultMsgWriter(config);
    defer m.deinit();

    renderMessages(comp, &m);
}

pub fn renderMessages(comp: *Compilation, m: anytype) void {
    var errors: u32 = 0;
    var warnings: u32 = 0;
    for (comp.diagnostics.list.items) |msg| {
        switch (msg.kind) {
            .@"fatal error", .@"error" => errors += 1,
            .warning => warnings += 1,
            .note => {},
            .off => continue, // happens if an error is added before it is disabled
            .default => unreachable,
        }
        renderMessage(comp, m, msg);
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

    comp.diagnostics.list.items.len = 0;
    comp.diagnostics.errors += errors;
}

pub fn renderMessage(comp: *Compilation, m: anytype, msg: Message) void {
    var line: ?[]const u8 = null;
    var endWithSplice = false;
    const width = if (msg.loc.id != .unused) blk: {
        var loc = msg.loc;
        switch (msg.tag) {
            .escape_sequence_overflow,
            .invalid_universal_character,
            => loc.byteOffset += @as(u32, @truncate(msg.extra.offset)),
            .non_standard_escape_char,
            .unknown_escape_sequence,
            => loc.byteOffset += msg.extra.invalidEscape.offset,
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
    const prop = msg.tag.property();
    switch (prop.extra) {
        .str => printRt(m, prop.msg, .{"{s}"}, .{msg.extra.str}),
        .tokenId => printRt(m, prop.msg, .{ "{s}", "{s}" }, .{
            msg.extra.tokenId.expected.symbol(),
            msg.extra.tokenId.actual.symbol(),
        }),
        .expectedTokenId => printRt(m, prop.msg, .{"{s}"}, .{msg.extra.expectedTokenId.symbol()}),
        .arguments => printRt(m, prop.msg, .{ "{d}", "{d}" }, .{
            msg.extra.arguments.expected,
            msg.extra.arguments.actual,
        }),
        .codePoints => printRt(m, prop.msg, .{ "{X:0>4}", "{u}" }, .{
            msg.extra.codePoints.actual,
            msg.extra.codePoints.resembles,
        }),
        .attrArgCount => printRt(m, prop.msg, .{ "{s}", "{d}" }, .{
            @tagName(msg.extra.attrArgCount.attribute),
            msg.extra.attrArgCount.expected,
        }),
        .attrArgType => printRt(m, prop.msg, .{ "{s}", "{s}" }, .{
            msg.extra.attrArgType.expected.toString(),
            msg.extra.attrArgType.actual.toString(),
        }),
        .actualCodePoint => printRt(m, prop.msg, .{"{X:0>4}"}, .{msg.extra.actualCodePoint}),
        .ascii => printRt(m, prop.msg, .{"{c}"}, .{msg.extra.ascii}),
        .unsigned => printRt(m, prop.msg, .{"{d}"}, .{msg.extra.unsigned}),
        .pow2AsString => printRt(m, prop.msg, .{"{s}"}, .{switch (msg.extra.pow2AsString) {
            63 => "9223372036854775808",
            64 => "18446744073709551616",
            127 => "170141183460469231731687303715884105728",
            128 => "340282366920938463463374607431768211456",
            else => unreachable,
        }}),
        .signed => printRt(m, prop.msg, .{"{d}"}, .{msg.extra.signed}),
        .attrEnum => printRt(m, prop.msg, .{ "{s}", "{s}" }, .{
            @tagName(msg.extra.attrEnum.tag),
            Attribute.Formatting.choices(msg.extra.attrEnum.tag),
        }),
        .ignoredRecordAttr => printRt(m, prop.msg, .{ "{s}", "{s}" }, .{
            @tagName(msg.extra.ignoredRecordAttr.tag),
            @tagName(msg.extra.ignoredRecordAttr.tagKind),
        }),
        .builtinWithHeader => printRt(m, prop.msg, .{ "{s}", "{s}" }, .{
            @tagName(msg.extra.builtinWithHeader.header),
            Builtin.nameFromTag(msg.extra.builtinWithHeader.builtin).span(),
        }),
        .invalidEscape => {
            if (std.ascii.isPrint(msg.extra.invalidEscape.char)) {
                const str: [1]u8 = .{msg.extra.invalidEscape.char};
                printRt(m, prop.msg, .{"{s}"}, .{&str});
            } else {
                var buf: [3]u8 = undefined;
                const str = std.fmt.bufPrint(&buf, "x{x}", .{std.fmt.fmtSliceHexLower(&.{msg.extra.invalidEscape.char})}) catch unreachable;
                printRt(m, prop.msg, .{"{s}"}, .{str});
            }
        },
        .none, .offset => m.write(prop.msg),
    }

    if (prop.opt) |some| {
        if (msg.kind == .@"error" and prop.kind != .@"error") {
            m.print(" [-Werror, -W{s}]", .{optName(some)});
        } else if (msg.kind != .note) {
            m.print(" [-W{s}]", .{optName(some)});
        }
    }

    m.end(line, width, endWithSplice);
}

fn printRt(m: anytype, str: []const u8, comptime fmts: anytype, args: anytype) void {
    var i: usize = 0;
    inline for (fmts, args) |fmt, arg| {
        const new = std.mem.indexOfPos(u8, str, i, fmt).?;
        m.write(str[i..new]);
        i = new + fmt.len;
        m.print(fmt, .{arg});
    }
    m.write(str[i..]);
}

fn optName(offset: u16) []const u8 {
    return std.meta.fieldNames(Options)[offset / @sizeOf(Kind)];
}

fn tagKind(diag: *Diagnostics, tag: Tag, langOpts: LangOpts) Kind {
    const prop = tag.property();
    var kind = prop.getKind(&diag.options);

    if (prop.all) {
        if (diag.options.all != .default)
            kind = diag.options.all;
    }

    if (prop.w_extra) {
        if (diag.options.extra != .default)
            kind = diag.options.extra;
    }

    if (prop.pedantic) {
        if (diag.options.pedantic != .default)
            kind = diag.options.pedantic;
    }

    if (prop.suppress_version) |some| if (langOpts.standard.atLeast(some)) return .off;
    if (prop.suppress_unless_version) |some| if (!langOpts.standard.atLeast(some)) return .off;
    if (prop.suppress_gnu and langOpts.standard.isExplicitGNU()) return .off;
    if (prop.suppress_gcc and langOpts.emulate == .gcc) return .off;
    if (prop.suppress_clang and langOpts.emulate == .clang) return .off;
    if (prop.suppress_msvc and langOpts.emulate == .msvc) return .off;
    if (kind == .@"error" and diag.fatalErrors) kind = .@"fatal error";

    return kind;
}

const MsgWriter = struct {
    w: std.io.BufferedWriter(4096, std.fs.File.Writer),
    config: std.io.tty.Config,

    fn init(config: std.io.tty.Config) MsgWriter {
        std.debug.lockStdErr();
        return .{
            .w = std.io.bufferedWriter(std.io.getStdErr().writer()),
            .config = config,
        };
    }

    pub fn deinit(m: *MsgWriter) void {
        m.w.flush() catch {};
        std.debug.unlockStdErr();
    }

    pub fn print(m: *MsgWriter, comptime fmt: []const u8, args: anytype) void {
        m.w.writer().print(fmt, args) catch {};
    }

    fn write(m: *MsgWriter, msg: []const u8) void {
        m.w.writer().writeAll(msg) catch {};
    }

    fn setColor(m: *MsgWriter, color: std.io.tty.Color) void {
        m.config.setColor(m.w.writer(), color) catch {};
    }

    fn location(m: *MsgWriter, path: []const u8, line: u32, col: u32) void {
        m.setColor(.bold);
        m.print("{s}:{d}:{d}: ", .{ path, line, col });
    }

    fn start(m: *MsgWriter, kind: Kind) void {
        switch (kind) {
            .@"fatal error", .@"error" => m.setColor(.bright_red),
            .note => m.setColor(.bright_cyan),
            .warning => m.setColor(.bright_magenta),
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

    fn end(m: *MsgWriter, maybe_line: ?[]const u8, col: u32, endWithSplice: bool) void {
        const line = maybe_line orelse {
            m.write("\n");
            m.setColor(.reset);
            return;
        };
        const trailer = if (endWithSplice) "\\ " else "";
        m.setColor(.reset);
        m.print("\n{s}{s}\n{s: >[3]}", .{ line, trailer, "", col });
        m.setColor(.bold);
        m.setColor(.bright_green);
        m.write("^\n");
        m.setColor(.reset);
    }
};
