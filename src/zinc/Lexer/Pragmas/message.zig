const std = @import("std");
const Compilation = @import("../../Basic/Compilation.zig");
const Pragma = @import("../Pragma.zig");
const Diagnostics = @import("../../Basic/Diagnostics.zig");
const Preprocessor = @import("../Preprocessor.zig");
const Parser = @import("../../Parser//Parser.zig");
const TokenIndex = @import("../../AST/AST.zig").TokenIndex;
const Source = @import("../../Basic/Source.zig");
const Message = @This();

pragma: Pragma = .{
    .deinit = deinit,
    .preprocessorHandler = preprocessorHandler,
},

pub fn init(allocator: std.mem.Allocator) !*Pragma {
    var once = try allocator.create(Message);
    once.* = .{};
    return &once.pragma;
}

fn deinit(pragma: *Pragma, comp: *Compilation) void {
    const self: *Message = @fieldParentPtr("pragma", pragma);
    comp.gpa.destroy(self);
}

fn preprocessorHandler(_: *Pragma, pp: *Preprocessor, startIdx: TokenIndex) Pragma.Error!void {
    const str = Pragma.pasteTokens(pp, startIdx + 1) catch |err| switch (err) {
        error.ExpectedStringLiteral => {
            return Pragma.err(pp, startIdx, .pragma_requires_string_literal, .{"message"});
        },
        else => |e| return e,
    };

    const messageToken = pp.tokens.get(startIdx);
    const messageExpansionLocs = pp.expansionSlice(startIdx);
    const loc = if (messageExpansionLocs.len != 0)
        messageExpansionLocs[messageExpansionLocs.len - 1]
    else
        messageToken.loc;

    const diagnostic: Pragma.Diagnostic = .pragma_message;
    var sf = std.heap.stackFallback(1024, pp.gpa);
    var buf = std.ArrayList(u8).init(sf.get());
    defer buf.deinit();

    try Diagnostics.formatArgs(buf.writer(), diagnostic.fmt, .{str});
    try pp.diagnostics.add(.{
        .text = buf.items,
        .kind = diagnostic.kind,
        .opt = diagnostic.opt,
        .location = loc.expand(pp.comp),
    });
}
