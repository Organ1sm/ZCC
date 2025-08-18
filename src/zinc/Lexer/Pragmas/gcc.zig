const std = @import("std");

const Compilation = @import("../../Basic/Compilation.zig");
const Diagnostics = @import("../../Basic/Diagnostics.zig");
const Parser = @import("../../Parser//Parser.zig");
const Pragma = @import("../Pragma.zig");
const Preprocessor = @import("../Preprocessor.zig");
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
originalState: Diagnostics.State = .{},
stateStack: std.ArrayList(Diagnostics.State) = .{},

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
    self.originalState = comp.diagnostics.state;
}

fn beforeParse(pragma: *Pragma, comp: *Compilation) void {
    var self: *GCC = @alignCast(@fieldParentPtr("pragma", pragma));
    comp.diagnostics.state = self.originalState;
    self.stateStack.items.len = 0;
}

fn afterParse(pragma: *Pragma, comp: *Compilation) void {
    var self: *GCC = @alignCast(@fieldParentPtr("pragma", pragma));
    comp.diagnostics.state = self.originalState;
    self.stateStack.items.len = 0;
}

pub fn init(allocator: std.mem.Allocator) !*Pragma {
    var gcc = try allocator.create(GCC);
    gcc.* = .{};
    return &gcc.pragma;
}

pub fn deinit(pragma: *Pragma, comp: *Compilation) void {
    const self: *GCC = @alignCast(@fieldParentPtr("pragma", pragma));
    self.stateStack.deinit(comp.gpa);
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
                error.ExpectedStringLiteral => return Pragma.err(pp, startIdx, .pragma_requires_string_literal, .{"GCC diagnostic"}),
                else => |e| return e,
            };
            if (!std.mem.startsWith(u8, str, "-W"))
                return Pragma.err(pp, startIdx + 1, .malformed_warning_check, .{"GCC diagnostics"});

            const newKind: Diagnostics.Message.Kind = switch (diagnostic) {
                .ignored => .off,
                .warning => .warning,
                .@"error" => .@"error",
                .fatal => .@"fatal error",
                else => unreachable,
            };

            try pp.diagnostics.set(str[2..], newKind);
        },
        .push => try self.stateStack.append(pp.comp.gpa, pp.diagnostics.state),
        .pop => pp.diagnostics.state = self.stateStack.pop() orelse self.originalState,
    }
}

fn preprocessorHandler(pragma: *Pragma, pp: *Preprocessor, startIdx: TokenIndex) Pragma.Error!void {
    var self: *GCC = @alignCast(@fieldParentPtr("pragma", pragma));
    const directiveToken = pp.tokens.get(startIdx + 1);
    if (directiveToken.is(.NewLine))
        return;

    const gccPragma = std.meta.stringToEnum(Directive, pp.expandedSlice(directiveToken)) orelse {
        return Pragma.err(pp, startIdx + 1, .unknown_gcc_pragma, .{});
    };
    switch (gccPragma) {
        .warning, .@"error" => {
            const text = Pragma.pasteTokens(pp, startIdx + 2) catch |err| switch (err) {
                error.ExpectedStringLiteral => {
                    return Pragma.err(pp, startIdx + 1, .pragma_requires_string_literal, .{@tagName(gccPragma)});
                },
                else => |e| return e,
            };
            return Pragma.err(
                pp,
                startIdx + 1,
                if (gccPragma == .warning) .pragma_warning_message else .pragma_error_message,
                .{text},
            );
        },
        .diagnostic => return self.diagnosticHandler(pp, startIdx + 2) catch |err| switch (err) {
            error.UnknownPragma => {
                return Pragma.err(pp, startIdx + 2, .unknown_gcc_pragma_directive, .{});
            },
            else => |e| return e,
        },
        .poison => {
            var i: u32 = 2;
            while (true) : (i += 1) {
                const tok = pp.tokens.get(startIdx + i);
                if (tok.is(.NewLine))
                    break;

                if (!tok.id.isMacroIdentifier())
                    return Pragma.err(pp, startIdx + i, .pragma_poison_identifier, .{});

                const str = pp.expandedSlice(tok);
                if (pp.defines.get(str) != null) {
                    try Pragma.err(pp, startIdx + i, .pragma_poison_macro, .{});
                }
                try pp.poisonedIdentifiers.put(pp.comp.gpa, str, {});
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
