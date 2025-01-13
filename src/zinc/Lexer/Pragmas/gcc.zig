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
optionsStack: std.ArrayListUnmanaged(Diagnostics.Options) = .{},

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
    var self: *GCC = @alignCast(@fieldParentPtr("pragma", pragma));
    self.originalOptions = comp.diagnostics.options;
}

fn beforeParse(pragma: *Pragma, comp: *Compilation) void {
    var self: *GCC = @alignCast(@fieldParentPtr("pragma", pragma));
    comp.diagnostics.options = self.originalOptions;
    self.optionsStack.items.len = 0;
}

fn afterParse(pragma: *Pragma, comp: *Compilation) void {
    var self: *GCC = @alignCast(@fieldParentPtr("pragma", pragma));
    comp.diagnostics.options = self.originalOptions;
    self.optionsStack.items.len = 0;
}

pub fn init(allocator: std.mem.Allocator) !*Pragma {
    var gcc = try allocator.create(GCC);
    gcc.* = .{};
    return &gcc.pragma;
}

pub fn deinit(pragma: *Pragma, comp: *Compilation) void {
    const self: *GCC = @alignCast(@fieldParentPtr("pragma", pragma));
    self.optionsStack.deinit(comp.gpa);
    comp.gpa.destroy(self);
}

fn diagnosticHandler(self: *GCC, pp: *Preprocessor, startIdx: TokenIndex) Pragma.Error!void {
    const diagnosticToken = pp.tokens.get(startIdx);
    if (diagnosticToken.is(.NewLine))
        return;

    const diagnostic = std.meta.stringToEnum(Directive.Diagnostics, pp.expandedSlice(diagnosticToken)) orelse
        return error.UnknownPragma;

    switch (diagnostic) {
        .ignored, .warning, .@"error", .fatal => {
            const str = Pragma.pasteTokens(pp, startIdx + 1) catch |err| switch (err) {
                error.ExpectedStringLiteral => {
                    return pp.comp.addDiagnostic(.{
                        .tag = .pragma_requires_string_literal,
                        .loc = diagnosticToken.loc,
                        .extra = .{ .str = "pragma diagnostic" },
                    }, pp.expansionSlice(startIdx));
                },
                else => |e| return e,
            };
            if (!std.mem.startsWith(u8, str, "-W")) {
                const next = pp.tokens.get(startIdx + 1);
                return pp.comp.addDiagnostic(.{
                    .tag = .malformed_warning_check,
                    .loc = next.loc,
                    .extra = .{ .str = "pragma diagnostic" },
                }, pp.expansionSlice(startIdx + 1));
            }
            const newKind = switch (diagnostic) {
                .ignored => Diagnostics.Kind.off,
                .warning => Diagnostics.Kind.warning,
                .@"error" => Diagnostics.Kind.@"error",
                .fatal => Diagnostics.Kind.@"fatal error",
                else => unreachable,
            };

            try pp.comp.diagnostics.set(str[2..], newKind);
        },
        .push => try self.optionsStack.append(pp.comp.gpa, pp.comp.diagnostics.options),
        .pop => pp.comp.diagnostics.options = self.optionsStack.popOrNull() orelse self.originalOptions,
    }
}

fn preprocessorHandler(pragma: *Pragma, pp: *Preprocessor, startIdx: TokenIndex) Pragma.Error!void {
    var self: *GCC = @alignCast(@fieldParentPtr("pragma", pragma));
    const directiveToken = pp.tokens.get(startIdx + 1);
    if (directiveToken.is(.NewLine))
        return;

    const gccPragma = std.meta.stringToEnum(Directive, pp.expandedSlice(directiveToken)) orelse {
        return pp.comp.addDiagnostic(.{
            .tag = .unknown_gcc_pragma,
            .loc = directiveToken.loc,
        }, directiveToken.expansionSlice());
    };
    switch (gccPragma) {
        .warning, .@"error" => {
            const text = Pragma.pasteTokens(pp, startIdx + 2) catch |err| switch (err) {
                error.ExpectedStringLiteral => {
                    return pp.comp.addDiagnostic(.{
                        .tag = .pragma_requires_string_literal,
                        .loc = directiveToken.loc,
                        .extra = .{ .str = @tagName(gccPragma) },
                    }, pp.expansionSlice(startIdx + 1));
                },
                else => |e| return e,
            };
            const extra = Diagnostics.Message.Extra{ .str = try pp.comp.diagnostics.arena.allocator().dupe(u8, text) };
            const diagnosticTag: Diagnostics.Tag = if (gccPragma == .warning) .pragma_warning_message else .pragma_error_message;
            return pp.comp.addDiagnostic(
                .{ .tag = diagnosticTag, .loc = directiveToken.loc, .extra = extra },
                pp.expansionSlice(startIdx + 1),
            );
        },
        .diagnostic => return self.diagnosticHandler(pp, startIdx + 2) catch |err| switch (err) {
            error.UnknownPragma => {
                const tok = pp.tokens.get(startIdx + 2);
                return pp.comp.addDiagnostic(.{
                    .tag = .unknown_gcc_pragma_directive,
                    .loc = tok.loc,
                }, pp.expansionSlice(startIdx + 2));
            },
            else => |e| return e,
        },
        .poison => {
            var i: u32 = 2;
            while (true) : (i += 1) {
                const tok = pp.tokens.get(startIdx + i);
                if (tok.is(.NewLine))
                    break;

                if (!tok.id.isMacroIdentifier()) {
                    return pp.comp.addDiagnostic(.{
                        .tag = .pragma_poison_identifier,
                        .loc = tok.loc,
                    }, pp.expansionSlice(startIdx + i));
                }
                const str = pp.expandedSlice(tok);
                if (pp.defines.get(str) != null) {
                    try pp.comp.addDiagnostic(.{
                        .tag = .pragma_poison_macro,
                        .loc = tok.loc,
                    }, pp.expansionSlice(startIdx + i));
                }
                try pp.poisonedIdentifiers.put(str, {});
            }
            return;
        },
    }
}

fn parserHandler(pragma: *Pragma, p: *Parser, startIdx: TokenIndex) Compilation.Error!void {
    var self: *GCC = @alignCast(@fieldParentPtr("pragma", pragma));
    const directiveToken = p.pp.tokens.get(startIdx + 1);
    if (directiveToken.is(.NewLine))
        return;

    const name = p.pp.expandedSlice(directiveToken);
    if (std.mem.eql(u8, name, "diagnostic")) {
        return self.diagnosticHandler(p.pp, startIdx + 2) catch |err| switch (err) {
            error.UnknownPragma => {}, // handled during preprocessing
            error.StopPreprocessing => unreachable, // Only used by #pragma once
            else => |e| return e,
        };
    }
}

fn preserveTokens(_: *Pragma, pp: *Preprocessor, startIdx: TokenIndex) bool {
    const next = pp.tokens.get(startIdx + 1);
    if (next.isNot(.NewLine)) {
        const name = pp.expandedSlice(next);
        if (std.mem.eql(u8, name, "poison")) {
            return false;
        }
    }
    return true;
}
