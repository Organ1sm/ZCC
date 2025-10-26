const std = @import("std");
const assert = std.debug.assert;

const Compilation = @import("../Basic/Compilation.zig");
const Diagnostics = @import("../Basic/Diagnostics.zig");
const Parser = @import("../Parser/Parser.zig");
const Preprocessor = @import("Preprocessor.zig");
const TokenIndex = @import("../AST/AST.zig").TokenIndex;

const Pragma = @This();

pub const Error = Compilation.Error || error{ UnknownPragma, StopPreprocessing };

/// Called during Preprocessor.init
beforePreprocess: ?*const fn (*Pragma, *Compilation) void = null,

/// Called at the beginning of Parser.parse
beforeParse: ?*const fn (*Pragma, *Compilation) void = null,

/// Called at the end of Parser.parse if a Tree was successfully parsed
afterParse: ?*const fn (*Pragma, *Compilation) void = null,

/// Called during Compilation.deinit
deinit: *const fn (*Pragma, *Compilation) void,

/// Called whenever the preprocessor encounters this pragma. `start_idx` is the index
/// within `pp.tokens` of the pragma name token. The pragma end is indicated by a
/// newline token (which may be generated if the source ends with a pragma with no newline)
/// As an example, given the following line:
///     #pragma GCC diagnostic error "-Wnewline-eof" \n
/// Then pp.tokens.get(startIdx) will return the `GCC` token.
/// Return error.UnknownPragma to emit an `unknown_pragma` diagnostic
/// Return error.StopPreprocessing to stop preprocessing the current file (see once.zig)
preprocessorHandler: ?*const fn (*Pragma, *Preprocessor, startIdx: TokenIndex) Error!void = null,

/// Called during token pretty-printing (`-E` option). If this returns true, the pragma will
/// be printed; otherwise it will be omitted. startIdx is the index of the pragma name token
preserveTokens: ?*const fn (*Pragma, *Preprocessor, startIdx: TokenIndex) bool = null,

/// Same as preprocessorHandler except called during parsing
/// The parser's `p.index` field must not be changed
parserHandler: ?*const fn (*Pragma, *Parser, startIdx: TokenIndex) Compilation.Error!void = null,

pub fn pasteTokens(pp: *Preprocessor, startIdx: TokenIndex) ![]const u8 {
    if (pp.tokens.get(startIdx).is(.NewLine))
        return error.ExpectedStringLiteral;

    const charBufferTop = pp.charBuffer.items.len;
    defer pp.charBuffer.items.len = charBufferTop;

    var i: usize = 0;
    var lparenCount: u32 = 0;
    var rparenCount: u32 = 0;
    while (true) : (i += 1) {
        const tok = pp.tokens.get(startIdx + i);
        if (tok.is(.NewLine))
            break;
        switch (tok.id) {
            .LParen => {
                if (lparenCount != i) return error.ExpectedStringLiteral;
                lparenCount += 1;
            },
            .RParen => rparenCount += 1,
            .StringLiteral => {
                if (rparenCount != 0)
                    return error.ExpectedStringLiteral;
                const str = pp.expandedSlice(tok);
                try pp.charBuffer.appendSlice(pp.comp.gpa, str[1 .. str.len - 1]);
            },
            else => return error.ExpectedStringLiteral,
        }
    }

    if (lparenCount != rparenCount)
        return error.ExpectedStringLiteral;

    return pp.charBuffer.items[charBufferTop..];
}

pub fn shouldPreserveTokens(self: *Pragma, pp: *Preprocessor, startIdx: TokenIndex) bool {
    if (self.preserveTokens) |func| return func(self, pp, startIdx);
    return true;
}

pub fn preprocessorCB(self: *Pragma, pp: *Preprocessor, startIdx: TokenIndex) Error!void {
    if (self.preprocessorHandler) |func| return func(self, pp, startIdx);
}

pub fn parserCB(self: *Pragma, p: *Parser, startIdx: TokenIndex) Compilation.Error!void {
    const tokenIdx = p.tokenIdx;
    defer assert(tokenIdx == p.tokenIdx);
    if (self.parserHandler) |func| return func(self, p, startIdx);
}

pub const Diagnostic = struct {
    fmt: []const u8,
    kind: Diagnostics.Message.Kind,
    opt: ?Diagnostics.Option = null,
    extension: bool = false,

    pub const pragma_warning_message: Diagnostic = .{
        .fmt = "{s}",
        .kind = .warning,
        .opt = .@"#pragma-messages",
    };

    pub const pragma_error_message: Diagnostic = .{
        .fmt = "{s}",
        .kind = .@"error",
    };

    pub const pragma_message: Diagnostic = .{
        .fmt = "#pragma message: {s}",
        .kind = .note,
    };

    pub const pragma_requires_string_literal: Diagnostic = .{
        .fmt = "pragma {s} requires string literal",
        .kind = .@"error",
    };

    pub const poisoned_identifier: Diagnostic = .{
        .fmt = "attempt to use a poisoned identifier",
        .kind = .@"error",
    };

    pub const pragma_poison_identifier: Diagnostic = .{
        .fmt = "can only poison identifier tokens",
        .kind = .@"error",
    };

    pub const pragma_poison_macro: Diagnostic = .{
        .fmt = "poisoning existing macro",
        .kind = .warning,
    };

    pub const unknown_gcc_pragma: Diagnostic = .{
        .fmt = "pragma GCC expected 'error', 'warning', 'diagnostic', 'poison'",
        .kind = .off,
        .opt = .@"unknown-pragmas",
    };

    pub const unknown_gcc_pragma_directive: Diagnostic = .{
        .fmt = "pragma GCC diagnostic expected 'error', 'warning', 'ignored', 'fatal', 'push', or 'pop'",
        .kind = .warning,
        .opt = .@"unknown-pragmas",
        .extension = true,
    };

    pub const malformed_warning_check: Diagnostic = .{
        .fmt = "{s} expected option name (e.g. \"-Wundef\")",
        .opt = .@"malformed-warning-check",
        .kind = .warning,
        .extension = true,
    };

    pub const pragma_pack_lparen: Diagnostic = .{
        .fmt = "missing '(' after '#pragma pack' - ignoring",
        .kind = .warning,
        .opt = .@"ignored-pragmas",
    };

    pub const pragma_pack_rparen: Diagnostic = .{
        .fmt = "missing ')' after '#pragma pack' - ignoring",
        .kind = .warning,
        .opt = .@"ignored-pragmas",
    };

    pub const pragma_pack_unknown_action: Diagnostic = .{
        .fmt = "unknown action for '#pragma pack' - ignoring",
        .kind = .warning,
        .opt = .@"ignored-pragmas",
    };

    pub const pragma_pack_show: Diagnostic = .{
        .fmt = "value of #pragma pack(show) == {d}",
        .kind = .warning,
    };

    pub const pragma_pack_int_ident: Diagnostic = .{
        .fmt = "expected integer or identifier in '#pragma pack' - ignored",
        .kind = .warning,
        .opt = .@"ignored-pragmas",
    };

    pub const pragma_pack_int: Diagnostic = .{
        .fmt = "expected #pragma pack parameter to be '1', '2', '4', '8', or '16'",
        .opt = .@"ignored-pragmas",
        .kind = .warning,
    };

    pub const pragma_pack_undefined_pop: Diagnostic = .{
        .fmt = "specifying both a name and alignment to 'pop' is undefined",
        .kind = .warning,
    };

    pub const pragma_pack_empty_stack: Diagnostic = .{
        .fmt = "#pragma pack(pop, ...) failed: stack empty",
        .opt = .@"ignored-pragmas",
        .kind = .warning,
    };
};

pub fn err(pp: *Preprocessor, tokenIdx: TokenIndex, diagnostic: Diagnostic, args: anytype) Compilation.Error!void {
    var sf = std.heap.stackFallback(1024, pp.comp.gpa);
    var allocating: std.Io.Writer.Allocating = .init(sf.get());
    defer allocating.deinit();

    Diagnostics.formatArgs(&allocating.writer, diagnostic.fmt, args) catch return error.OutOfMemory;

    try pp.diagnostics.addWithLocation(pp.comp, .{
        .kind = diagnostic.kind,
        .opt = diagnostic.opt,
        .text = allocating.written(),
        .location = pp.tokens.items(.loc)[tokenIdx].expand(pp.comp),
        .extension = diagnostic.extension,
    }, pp.expansionSlice(tokenIdx), true);
}
