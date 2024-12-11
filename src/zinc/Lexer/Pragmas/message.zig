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
    const messageToken = pp.tokens.get(startIdx);
    const messageExpansionLocs = messageToken.expansionSlice();

    const str = Pragma.pasteTokens(pp, startIdx + 1) catch |err| switch (err) {
        error.ExpectedStringLiteral => {
            return pp.comp.addDiagnostic(.{
                .tag = .pragma_requires_string_literal,
                .loc = messageToken.loc,
                .extra = .{ .str = "message" },
            }, messageExpansionLocs);
        },
        else => |e| return e,
    };

    const loc = if (messageExpansionLocs.len != 0)
        messageExpansionLocs[messageExpansionLocs.len - 1]
    else
        messageToken.loc;
    const extra = Diagnostics.Message.Extra{ .str = try pp.arena.allocator().dupe(u8, str) };
    return pp.comp.addDiagnostic(.{ .tag = .pragma_message, .loc = loc, .extra = extra }, &.{});
}
