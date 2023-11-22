const std = @import("std");
const Compilation = @import("../../Basic/Compilation.zig");
const Pragma = @import("../Pragma.zig");
const Diagnostics = @import("../../Basic/Diagnostics.zig");
const Preprocessor = @import("../Preprocessor.zig");
const Parser = @import("../../Parser//Parser.zig");
const TokenIndex = @import("../../AST/AST.zig").TokenIndex;
const Source = @import("../../Basic/Source.zig");

const Once = @This();

pragma: Pragma = .{
    .afterParse = afterParse,
    .deinit = deinit,
    .preprocessorHandler = preprocessorHandler,
},
pragmaOnce: std.AutoHashMap(Source.ID, void),
preprocessCount: u32 = 0,

pub fn init(allocator: std.mem.Allocator) !*Pragma {
    var once = try allocator.create(Once);
    once.* = .{
        .pragmaOnce = std.AutoHashMap(Source.ID, void).init(allocator),
    };
    return &once.pragma;
}

fn afterParse(pragma: *Pragma, _: *Compilation) void {
    var self = @fieldParentPtr(Once, "pragma", pragma);
    self.pragmaOnce.clearRetainingCapacity();
}

pub fn deinit(pragma: *Pragma, comp: *Compilation) void {
    var self = @fieldParentPtr(Once, "pragma", pragma);
    self.pragmaOnce.deinit();
    comp.gpa.destroy(self);
}

fn preprocessorHandler(pragma: *Pragma, pp: *Preprocessor, startIdx: TokenIndex, len: u32) Pragma.Error!void {
    var self = @fieldParentPtr(Once, "pragma", pragma);
    const nameToken = pp.tokens.get(startIdx);
    if (len != 1) {
        try pp.compilation.addDiagnostic(.{
            .tag = .extra_tokens_directive_end,
            .loc = nameToken.loc,
        });
    }
    const seen = self.preprocessCount == pp.preprocessCount;
    const prev = try self.pragmaOnce.fetchPut(nameToken.loc.id, {});
    if (prev != null and !seen) {
        return error.StopPreprocessing;
    }
    self.preprocessCount = pp.preprocessCount;
}
