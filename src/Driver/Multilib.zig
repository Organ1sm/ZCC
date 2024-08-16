const std = @import("std");
const util = @import("../Basic/Util.zig");
const Filesystem = @import("Filesystem.zig").Filesystem;

pub const Flags = std.BoundedArray([]const u8, 6);

pub const Detected = struct {
    multilibs: std.BoundedArray(Multilib, 4) = .{},
    selected: Multilib = .{},
    biarchSibling: ?Multilib = null,

    pub fn filter(self: *Detected, multilibFilter: Filter, fs: Filesystem) void {
        var foundCount: usize = 0;
        for (self.multilibs.constSlice()) |multilib| {
            if (multilibFilter.exists(multilib, fs)) {
                self.multilibs.set(foundCount, multilib);
                foundCount += 1;
            }
        }
        self.multilibs.resize(foundCount) catch unreachable;
    }

    pub fn select(self: *Detected, flags: Flags) bool {
        var filtered: std.BoundedArray(Multilib, 4) = .{};
        for (self.multilibs.constSlice()) |multilib| {
            for (multilib.flags.constSlice()) |multilibFlag| {
                const matched = for (flags.constSlice()) |argFlag| {
                    if (std.mem.eql(u8, argFlag[1..], multilibFlag[1..])) break argFlag;
                } else multilibFlag;

                if (matched[0] != multilibFlag[0])
                    break;
            } else {
                filtered.appendAssumeCapacity(multilib);
            }
        }

        if (filtered.len == 0)
            return false;

        if (filtered.len == 1) {
            self.selected = filtered.get(0);
            return true;
        }

        @panic("Got too many multilibs");
    }
};

pub const Filter = struct {
    base: [2][]const u8,
    file: []const u8,

    pub fn exists(self: Filter, m: Multilib, fs: Filesystem) bool {
        return fs.joinedExists(&.{ self.base[0], self.base[1], m.gccSuffix, self.file });
    }
};

const Multilib = @This();

gccSuffix: []const u8 = "",
osSuffix: []const u8 = "",
includeSuffix: []const u8 = "",
flags: Flags = .{},
priority: u32 = 0,

pub fn init(gccSuffix: []const u8, osSuffix: []const u8, flags: []const []const u8) Multilib {
    var self: Multilib = .{
        .flags = Flags.init(0) catch unreachable,
        .gccSuffix = gccSuffix,
        .osSuffix = osSuffix,
    };
    self.flags.appendSliceAssumeCapacity(flags);
    return self;
}
