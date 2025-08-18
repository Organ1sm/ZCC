const std = @import("std");
const mem = std.mem;

const Compilation = @import("../../Basic//Compilation.zig");
const Diagnostics = @import("../../Basic/Diagnostics.zig");
const Parser = @import("../../Parser/Parser.zig");
const Pragma = @import("../Pragma.zig");
const Preprocessor = @import("../Preprocessor.zig");
const TokenIndex = @import("../../AST/AST.zig").TokenIndex;

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
    if (lparen.isNot(.LParen))
        return Pragma.err(p.pp, idx, .pragma_pack_lparen, .{});

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
            const action = std.meta.stringToEnum(Action, p.getTokenText(arg)) orelse {
                return Pragma.err(p.pp, arg, .pragma_pack_unknown_action, .{});
            };
            switch (action) {
                .show => return Pragma.err(p.pp, arg, .pragma_pack_show, .{p.pragmaPack orelse 8}),

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
                                label = p.getTokenText(next);
                                if (tokenIds[idx] == .Comma) {
                                    idx += 1;
                                    const int = idx;
                                    idx += 1;
                                    if (tokenIds[int] != .PPNumber)
                                        return Pragma.err(p.pp, int, .pragma_pack_int_ident, .{});

                                    newVal = (try packInt(p, int)) orelse return;
                                }
                            },
                            else => return Pragma.err(p.pp, next, .pragma_pack_int_ident, .{}),
                        }
                    }
                    if (action == .push) {
                        try pack.stack.append(p.comp.gpa, .{ .label = label orelse "", .val = p.pragmaPack orelse 8 });
                    } else {
                        pack.pop(p, label);
                        if (newVal != null) {
                            try Pragma.err(p.pp, arg, .pragma_pack_undefined_pop, .{});
                        } else if (pack.stack.items.len == 0) {
                            try Pragma.err(p.pp, arg, .pragma_pack_empty_stack, .{});
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
                try pack.stack.append(p.gpa, .{ .label = "", .val = p.pragmaPack });
            }
            p.pragmaPack = new_val;
        },
        else => {},
    }

    if (tokenIds[idx] != .RParen)
        return Pragma.err(p.pp, idx, .pragma_pack_rparen, .{});
}

fn packInt(p: *Parser, tokenIdx: TokenIndex) Compilation.Error!?u8 {
    const res = p.parseNumberToken(tokenIdx) catch |err| switch (err) {
        error.ParsingFailed => {
            try Pragma.err(p.pp, tokenIdx, .pragma_pack_int, .{});
            return null;
        },
        else => |e| return e,
    };

    const int = res.value.toInt(u64, p.comp) orelse 99;
    switch (int) {
        1, 2, 4, 8, 16 => return @intCast(int),
        else => {
            try Pragma.err(p.pp, tokenIdx, .pragma_pack_int, .{});
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
        const prev = pack.stack.pop() orelse {
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
