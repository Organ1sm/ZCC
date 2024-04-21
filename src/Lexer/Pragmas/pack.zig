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
    const tokenIds = p.pp.tokens.items(.id);
    const arg = idx;
    switch (tokenIds[arg]) {
        .Identifier => {
            idx += 1;
            const Action = enum {
                show,
                push,
                pop,
            };
            const action = std.meta.stringToEnum(Action, p.getTokenSlice(arg)) orelse {
                return p.errToken(.pragma_pack_unknown_action, arg);
            };
            switch (action) {
                .show => try p.errExtra(.pragma_pack_show, arg, .{ .unsigned = p.pragmaPack orelse 8 }),

                .push, .pop => {
                    var newVal: ?u8 = null;
                    var label: ?[]const u8 = null;
                    if (tokenIds[idx] == .Comma) {
                        idx += 1;
                        const next = idx;
                        idx += 1;
                        switch (tokenIds[next]) {
                            .PPNumber => newVal = (try packInt(p, next)) orelse return,
                            .Identifier => {
                                label = p.getTokenSlice(next);
                                if (tokenIds[idx] == .Comma) {
                                    idx += 1;
                                    const int = idx;
                                    idx += 1;
                                    if (tokenIds[int] != .PPNumber)
                                        return p.errToken(.pragma_pack_int_ident, int);

                                    newVal = (try packInt(p, int)) orelse return;
                                }
                            },
                            else => return p.errToken(.pragma_pack_int_ident, next),
                        }
                    }
                    if (action == .push) {
                        try pack.stack.append(p.comp.gpa, .{ .label = label orelse "", .val = p.pragmaPack orelse 8 });
                    } else {
                        pack.pop(p, label);
                        if (newVal != null) {
                            try p.errToken(.pragma_pack_undefined_pop, arg);
                        } else if (pack.stack.items.len == 0) {
                            try p.errToken(.pragma_pack_empty_stack, arg);
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
            p.pragmaPack = null;
        },

        .PPNumber => {
            const new_val = (try packInt(p, arg)) orelse return;
            idx += 1;
            if (appleOrXL) {
                try pack.stack.append(p.comp.gpa, .{ .label = "", .val = p.pragmaPack });
            }
            p.pragmaPack = new_val;
        },
        else => {},
    }

    if (tokenIds[idx] != .RParen)
        return p.errToken(.pragma_pack_rparen, idx);
}

fn packInt(p: *Parser, tokenIdx: TokenIndex) Compilation.Error!?u8 {
    const res = p.parseNumberToken(tokenIdx) catch |err| switch (err) {
        error.ParsingFailed => {
            try p.errToken(.pragma_pack_int, tokenIdx);
            return null;
        },
        else => |e| return e,
    };
    const int = if (res.value.tag == .int) res.value.getInt(u64) else 99;
    switch (int) {
        1, 2, 4, 8, 16 => return @intCast(int),
        else => {
            try p.errToken(.pragma_pack_int, tokenIdx);
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
