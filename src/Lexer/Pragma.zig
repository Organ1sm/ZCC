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

/// Called whenever the preprocessor encounters this pragma. `startIdx` is the index
/// within `pp.tokens` of the pragma name token. `len` is the number of tokens in the
/// pragma, including the name token. It is always greater than zero.
/// As an example, given the following line:
///     #pragma GCC diagnostic error "-Wnewline-eof"
/// Then pp.tokens.get(startIdx) will return the `GCC` token and `len` will be 4
/// Return error.UnknownPragma to emit an `unsupported_pragma` diagnostic
/// Return error.StopPreprocessing to stop preprocessing the current file (see once.zig)
preprocessorHandler: ?*const fn (*Pragma, *Preprocessor, startIdx: TokenIndex, len: u32) Error!void = null,

/// Called during token pretty-printing (`-E` option). If this returns true, the pragma will
/// be printed; otherwise it will be omitted. startIdx and len behave the same as they do
/// in preprocessorHandler
preserveTokens: ?*const fn (*Pragma, *Preprocessor, startIdx: TokenIndex, len: u32) bool = null,

/// Same as preprocessorHandler except called during parsing
/// The parser's `p.index` field must not be changed
parserHandler: ?*const fn (*Pragma, *Parser, startIdx: TokenIndex, len: u32) Compilation.Error!void = null,

pub fn pasteTokens(pp: *Preprocessor, startIdx: TokenIndex, len: u32) ![]const u8 {
    if (len == 0)
        return error.ExpectedStringLiteral;

    const charBufferTop = pp.charBuffer.items.len;
    defer pp.charBuffer.items.len = charBufferTop;

    var i: usize = 0;
    var parenCount: usize = 0;
    while (i < len) : (i += 1) {
        const tok = pp.tokens.get(startIdx + i);
        switch (tok.id) {
            .LParen => {
                if (parenCount != i) return error.ExpectedStringLiteral;
                parenCount += 1;
            },
            .RParen => {
                if (parenCount + i != len) return error.ExpectedStringLiteral;
                parenCount -= 1;
            },
            .StringLiteral => {
                const str = pp.expandedSlice(tok);
                try pp.charBuffer.appendSlice(str[1 .. str.len - 1]);
            },
            else => return error.ExpectedStringLiteral,
        }
    }
    if (parenCount != 0) return error.ExpectedStringLiteral;
    return pp.charBuffer.items[charBufferTop..];
}

pub fn shouldPreserveTokens(self: *Pragma, pp: *Preprocessor, startIdx: TokenIndex, len: u32) bool {
    if (self.preserveTokens) |func| return func(self, pp, startIdx, len);
    return false;
}

pub fn preprocessorCB(self: *Pragma, pp: *Preprocessor, startIdx: TokenIndex, len: u32) Error!void {
    if (self.preprocessorHandler) |func| return func(self, pp, startIdx, len);
}

pub fn parserCB(self: *Pragma, p: *Parser, startIdx: TokenIndex, len: u32) Compilation.Error!void {
    const tokenIdx = p.index;
    defer std.debug.assert(tokenIdx == p.index);
    if (self.parserHandler) |func| return func(self, p, startIdx, len);
}
