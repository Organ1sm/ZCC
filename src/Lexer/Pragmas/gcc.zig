const std = @import("std");
const Compilation = @import("../../Basic/Compilation.zig");
const Pragma = @import("../Pragma.zig");
const Diagnostics = @import("../../Basic/Diagnostics.zig");
const Preprocessor = @import("../Preprocessor.zig");
const Parser = @import("../../Parser//Parser.zig");
const TokenIndex = @import("../../AST/AST.zig").TokenIndex;

const GCC = @This();

pragma: Pragma = .{
    .beforeParse = beforeParse,
    .beforePreprocess = beforePreprocess,
    .afterParse = afterParse,
    .deinit = deinit,
    .preprocessorHandler = preprocessorHandler,
    .parserHandler = parserHandler,
    .preserveTokens = preserveTokens,
},
originalOptions: Diagnostics.Options = .{},

const Directive = enum {
    warning,
    @"error",
    diagnostic,
    poison,
    const Diagnostics = enum {
        ignored,
        warning,
        @"error",
        fatal,
        push,
        pop,
    };
};

fn beforePreprocess(pragma: *Pragma, comp: *Compilation) void {
    var self = @fieldParentPtr(GCC, "pragma", pragma);
    self.originalOptions = comp.diag.options;
}

fn beforeParse(pragma: *Pragma, comp: *Compilation) void {
    var self = @fieldParentPtr(GCC, "pragma", pragma);
    comp.diag.options = self.originalOptions;
}

fn afterParse(pragma: *Pragma, comp: *Compilation) void {
    var self = @fieldParentPtr(GCC, "pragma", pragma);
    comp.diag.options = self.originalOptions;
}

pub fn init(allocator: std.mem.Allocator) !*Pragma {
    var gcc = try allocator.create(GCC);
    gcc.* = .{};
    return &gcc.pragma;
}

pub fn deinit(pragma: *Pragma, comp: *Compilation) void {
    var self = @fieldParentPtr(GCC, "pragma", pragma);
    comp.gpa.destroy(self);
}

fn diagnosticHandler(pp: *Preprocessor, startIdx: TokenIndex) !void {
    const diagnosticToken = pp.tokens.get(startIdx);
    if (diagnosticToken.id == .NewLine)
        return;

    if (std.meta.stringToEnum(Directive.Diagnostics, pp.expandedSlice(diagnosticToken))) |diagnostic| {
        switch (diagnostic) {
            .ignored, .warning, .@"error", .fatal => {
                const str = Pragma.pasteTokens(pp, startIdx + 1) catch |err| switch (err) {
                    error.ExpectedStringLiteral => {
                        return pp.compilation.addDiagnostic(.{
                            .tag = .pragma_requires_string_literal,
                            .loc = diagnosticToken.loc,
                            .extra = .{ .str = "pragma diagnostic" },
                        });
                    },
                    else => |e| return e,
                };
                if (!std.mem.startsWith(u8, str, "-W")) {
                    return pp.compilation.addDiagnostic(.{
                        .tag = .malformed_warning_check,
                        .loc = pp.tokens.get(startIdx + 1).loc,
                        .extra = .{ .str = "pragma diagnostic" },
                    });
                }
                const newKind = switch (diagnostic) {
                    .ignored => Diagnostics.Kind.off,
                    .warning => Diagnostics.Kind.warning,
                    .@"error" => Diagnostics.Kind.@"error",
                    .fatal => Diagnostics.Kind.@"fatal error",
                    else => unreachable,
                };

                return pp.compilation.diag.set(str[2..], newKind);
            },
            .push, .pop => {},
        }
    }
    return error.UnknownPragma;
}

fn preprocessorHandler(_: *Pragma, pp: *Preprocessor, startIdx: TokenIndex) Pragma.Error!void {
    const directiveToken = pp.tokens.get(startIdx + 1);
    if (directiveToken.id == .NewLine)
        return;

    if (std.meta.stringToEnum(Directive, pp.expandedSlice(directiveToken))) |gcc_pragma| {
        switch (gcc_pragma) {
            .warning, .@"error" => {
                const text = Pragma.pasteTokens(pp, startIdx + 2) catch |err| switch (err) {
                    error.ExpectedStringLiteral => {
                        return pp.compilation.addDiagnostic(.{
                            .tag = .pragma_requires_string_literal,
                            .loc = directiveToken.loc,
                            .extra = .{ .str = @tagName(gcc_pragma) },
                        });
                    },
                    else => |e| return e,
                };
                const extra = Diagnostics.Message.Extra{ .str = try pp.arena.allocator().dupe(u8, text) };
                const diagnosticTag: Diagnostics.Tag = if (gcc_pragma == .warning) .pragma_warning_message else .pragma_error_message;
                return pp.compilation.addDiagnostic(.{ .tag = diagnosticTag, .loc = directiveToken.loc, .extra = extra });
            },
            .diagnostic => return diagnosticHandler(pp, startIdx + 2),
            .poison => {
                var i: usize = 2;
                while (true) : (i += 1) {
                    const tok = pp.tokens.get(startIdx + i);
                    if (tok.id == .NewLine)
                        break;

                    if (!tok.id.isMacroIdentifier()) {
                        return pp.compilation.addDiagnostic(.{
                            .tag = .pragma_poison_identifier,
                            .loc = tok.loc,
                        });
                    }
                    const str = pp.expandedSlice(tok);
                    if (pp.defines.get(str) != null) {
                        try pp.compilation.addDiagnostic(.{
                            .tag = .pragma_poison_macro,
                            .loc = tok.loc,
                        });
                    }
                    try pp.poisonedIdentifiers.put(str, {});
                }
                return;
            },
        }
    }
    return error.UnknownPragma;
}

fn parserHandler(_: *Pragma, p: *Parser, startIdx: TokenIndex) Compilation.Error!void {
    const directiveToken = p.pp.tokens.get(startIdx + 1);
    if (directiveToken.id == .NewLine)
        return;

    const name = p.pp.expandedSlice(directiveToken);
    if (std.mem.eql(u8, name, "diagnostic")) {
        return diagnosticHandler(p.pp, startIdx + 2) catch |err| switch (err) {
            error.UnknownPragma => {}, // handled during preprocessing
            else => |e| return e,
        };
    }
}

fn preserveTokens(_: *Pragma, pp: *Preprocessor, startIdx: TokenIndex) bool {
    const next = pp.tokens.get(startIdx + 1);
    if (next.id != .NewLine) {
        const name = pp.expandedSlice(next);
        if (std.mem.eql(u8, name, "poison")) {
            return false;
        }
    }
    return true;
}
