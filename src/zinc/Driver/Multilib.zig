const std = @import("std");
const Filesystem = @import("Filesystem.zig").Filesystem;

pub const Flags = std.BoundedArray([]const u8, 6);

/// Large enough for GCCDetector for Linux; may need to be increased to support other toolchains.
const MaxMultilibs = 4;

pub const Detected = struct {
    multilibBuffer: [MaxMultilibs]Multilib = undefined,
    multilibCount: u8 = 0,
    selected: Multilib = .{},
    biarchSibling: ?Multilib = null,

    pub fn filter(d: *Detected, multilibFilter: Filter, fs: Filesystem) void {
        var foundCount: u8 = 0;
        for (d.multilibs()) |multilib| {
            if (multilibFilter.exists(multilib, fs)) {
                d.multilibBuffer[foundCount] = multilib;
                foundCount += 1;
            }
        }
        d.multilibCount = foundCount;
    }

    pub fn select(d: *Detected, checkFlags: []const []const u8) !bool {
        var selected: ?Multilib = null;

        for (d.multilibs()) |multilib| {
            for (multilib.flags()) |multilibFlag| {
                const matched = for (checkFlags) |argFlag| {
                    if (std.mem.eql(u8, argFlag[1..], multilibFlag[1..])) break argFlag;
                } else multilibFlag;

                if (matched[0] != multilibFlag[0]) break;
            } else if (selected != null) {
                return error.TooManyMultilibs;
            } else {
                selected = multilib;
            }
        }

        if (selected) |multilib| {
            d.selected = multilib;
            return true;
        }
        return false;
    }

    pub fn multilibs(d: *const Detected) []const Multilib {
        return d.multilibBuffer[0..d.multilibCount];
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
flagBuffer: [6][]const u8 = undefined,
flagCount: u8 = 0,
priority: u32 = 0,

pub fn init(gccSuffix: []const u8, osSuffix: []const u8, initFlags: []const []const u8) Multilib {
    var self: Multilib = .{
        .gccSuffix = gccSuffix,
        .osSuffix = osSuffix,
    };

    @memcpy(self.flagBuffer[0..initFlags.len], initFlags);
    return self;
}

pub fn flags(m: *const Multilib) []const []const u8 {
    return m.flagBuffer[0..m.flagCount];
}
