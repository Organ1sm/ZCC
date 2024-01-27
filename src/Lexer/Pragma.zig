const std = @import("std");
const Compilation = @import("../Basic/Compilation.zig");
const Preprocessor = @import("Preprocessor.zig");
const Parser = @import("../Parser/Parser.zig");
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
    if (pp.tokens.get(startIdx).id == .NewLine)
        return error.ExpectedStringLiteral;

    const charBufferTop = pp.charBuffer.items.len;
    defer pp.charBuffer.items.len = charBufferTop;

    var i: usize = 0;
    var lparenCount: u32 = 0;
    var rparenCount: u32 = 0;
    while (true) : (i += 1) {
        const tok = pp.tokens.get(startIdx + i);
        if (tok.id == .NewLine)
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
                try pp.charBuffer.appendSlice(str[1 .. str.len - 1]);
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
    return false;
}

pub fn preprocessorCB(self: *Pragma, pp: *Preprocessor, startIdx: TokenIndex) Error!void {
    if (self.preprocessorHandler) |func| return func(self, pp, startIdx);
}

pub fn parserCB(self: *Pragma, p: *Parser, startIdx: TokenIndex) Compilation.Error!void {
    const tokenIdx = p.index;
    defer std.debug.assert(tokenIdx == p.index);
    if (self.parserHandler) |func| return func(self, p, startIdx);
}
