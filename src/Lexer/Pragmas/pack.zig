const std = @import("std");
const mem = std.mem;
const Compilation = @import("../../Basic//Compilation.zig");
const Pragma = @import("../Pragma.zig");
const Diagnostics = @import("../../Basic/Diagnostics.zig");
const Preprocessor = @import("../Preprocessor.zig");
const Parser = @import("../../Parser/Parser.zig");
const Tree = @import("../../AST/AST.zig");
const TokenIndex = Tree.TokenIndex;

const Pack = @This();

pragma: Pragma = .{
    .deinit = deinit,
    .parserHandler = parserHandler,
    .preserveTokens = preserveTokens,
},
stack: std.ArrayListUnmanaged(struct { label: []const u8, val: u8 }) = .{},

pub fn init(allocator: mem.Allocator) !*Pragma {
    var pack = try allocator.create(Pack);
    pack.* = .{};
    return &pack.pragma;
}

fn deinit(pragma: *Pragma, comp: *Compilation) void {
    var self: *Pack = @fieldParentPtr("pragma", pragma);
    self.stack.deinit(comp.gpa);
    comp.gpa.destroy(self);
}

fn parserHandler(pragma: *Pragma, p: *Parser, startIdx: TokenIndex) Compilation.Error!void {
    var pack: *Pack = @fieldParentPtr("pragma", pragma);
    var idx = startIdx + 1;
    const lparen = p.pp.tokens.get(idx);
    if (lparen.id != .LParen) {
        return p.comp.diag.add(.{
            .tag = .pragma_pack_lparen,
            .loc = lparen.loc,
        }, lparen.expansionSlice());
    }
    idx += 1;

    // TODO -fapple-pragma-pack -fxl-pragma-pack
    const appleOrXL = false;
    const arg = p.pp.tokens.get(idx);
    switch (arg.id) {
        .Identifier => {
            idx += 1;
            const Action = enum {
                show,
                push,
                pop,
            };
            const action = std.meta.stringToEnum(Action, p.pp.expandedSlice(arg)) orelse {
                return p.comp.diag.add(.{
                    .tag = .pragma_pack_unknown_action,
                    .loc = arg.loc,
                }, arg.expansionSlice());
            };
            switch (action) {
                .show => {
                    try p.comp.diag.add(.{
                        .tag = .pragma_pack_show,
                        .loc = arg.loc,
                        .extra = .{ .unsigned = p.pragmaPack },
                    }, arg.expansionSlice());
                },
                .push, .pop => {
                    var newVal: ?u8 = null;
                    var label: ?[]const u8 = null;
                    if (p.pp.tokens.get(idx).id == .Comma) {
                        idx += 1;
                        const next = p.pp.tokens.get(idx);
                        idx += 1;
                        switch (next.id) {
                            .IntegerLiteral => newVal = (try packInt(p, next)) orelse return,
                            .Identifier => {
                                label = p.pp.expandedSlice(next);
                                if (p.pp.tokens.get(idx).id == .Comma) {
                                    idx += 1;
                                    const int = p.pp.tokens.get(idx);
                                    idx += 1;
                                    if (int.id != .IntegerLiteral) return p.comp.diag.add(.{
                                        .tag = .pragma_pack_int_ident,
                                        .loc = int.loc,
                                    }, int.expansionSlice());

                                    newVal = (try packInt(p, int)) orelse return;
                                }
                            },
                            else => return p.comp.diag.add(.{
                                .tag = .pragma_pack_int_ident,
                                .loc = next.loc,
                            }, next.expansionSlice()),
                        }
                    }
                    if (action == .push) {
                        try pack.stack.append(p.comp.gpa, .{ .label = label orelse "", .val = p.pragmaPack });
                    } else {
                        pack.pop(p, label);
                        if (newVal != null) {
                            try p.comp.diag.add(.{
                                .tag = .pragma_pack_undefined_pop,
                                .loc = arg.loc,
                            }, arg.expansionSlice());
                        } else if (pack.stack.items.len == 0) {
                            try p.comp.diag.add(.{
                                .tag = .pragma_pack_empty_stack,
                                .loc = arg.loc,
                            }, arg.expansionSlice());
                        }
                    }
                    if (newVal) |some| {
                        p.pragmaPack = some;
                    }
                },
            }
        },

        .RParen => if (appleOrXL) {
            pack.pop(p, null);
        } else {
            p.pragmaPack = 8;
        },

        .IntegerLiteral => {
            idx += 1;
            const new_val = (try packInt(p, arg)) orelse return;
            if (appleOrXL) {
                try pack.stack.append(p.comp.gpa, .{ .label = "", .val = p.pragmaPack });
            }
            p.pragmaPack = new_val;
        },
        else => {},
    }

    const rparen = p.pp.tokens.get(idx);
    if (rparen.id != .RParen) {
        return p.comp.diag.add(.{
            .tag = .pragma_pack_rparen,
            .loc = rparen.loc,
        }, rparen.expansionSlice());
    }
}

fn packInt(p: *Parser, arg: Tree.Token) Compilation.Error!?u8 {
    const int = std.fmt.parseInt(u8, p.pp.expandedSlice(arg), 10) catch 99;
    switch (int) {
        1, 2, 4, 8, 16 => return int,
        else => {
            try p.comp.diag.add(.{
                .tag = .pragma_pack_int,
                .loc = arg.loc,
            }, arg.expansionSlice());
            return null;
        },
    }
}

fn pop(pack: *Pack, p: *Parser, maybeLabel: ?[]const u8) void {
    if (maybeLabel) |label| {
        var i = pack.stack.items.len;
        while (i > 0) {
            i -= 1;
            if (std.mem.eql(u8, pack.stack.items[i].label, label)) {
                const prev = pack.stack.orderedRemove(i);
                p.pragmaPack = prev.val;
                return;
            }
        }
    } else {
        const prev = pack.stack.popOrNull() orelse {
            p.pragmaPack = 2;
            return;
        };
        p.pragmaPack = prev.val;
    }
}

fn preserveTokens(_: *Pragma, pp: *Preprocessor, startIdx: TokenIndex) bool {
    _ = pp;
    _ = startIdx;
    return true;
}
