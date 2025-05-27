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
    var self: *Once = @fieldParentPtr("pragma", pragma);
    self.pragmaOnce.clearRetainingCapacity();
}

pub fn deinit(pragma: *Pragma, comp: *Compilation) void {
    var self: *Once = @fieldParentPtr("pragma", pragma);
    self.pragmaOnce.deinit();
    comp.gpa.destroy(self);
}

fn preprocessorHandler(pragma: *Pragma, pp: *Preprocessor, startIdx: TokenIndex) Pragma.Error!void {
    var self: *Once = @fieldParentPtr("pragma", pragma);
    const nameToken = pp.tokens.get(startIdx);
    const next = pp.tokens.get(startIdx + 1);
    if (next.isNot(.NewLine)) {
        const diagnostic: Preprocessor.Diagnostic = .extra_tokens_directive_end;
        return pp.diagnostics.addWithLocation(pp.comp, .{
            .text = diagnostic.fmt,
            .kind = diagnostic.kind,
            .opt = diagnostic.opt,
            .location = nameToken.loc.expand(pp.comp),
        }, pp.expansionSlice(startIdx + 1), true);
    }
    const seen = self.preprocessCount == pp.preprocessCount;
    const prev = try self.pragmaOnce.fetchPut(nameToken.loc.id, {});
    if (prev != null and !seen) {
        return error.StopPreprocessing;
    }
    self.preprocessCount = pp.preprocessCount;
}
