const Source = @This();

pub const SourceLocation = struct {
    start: u32,
    end: u32,
};

pub const ID = enum(u16) {
    _,

    const generatedBit: u16 = 1 << 15;
    const generatedMask: u16 = generatedBit - 1;

    pub fn index(id: ID) u16 {
        return @intFromEnum(id) & generatedMask;
    }

    pub fn isGenerated(id: ID) bool {
        return (@intFromEnum(id) & generatedBit != 0);
    }

    pub fn markGenerated(id: *ID) void {
        id.* = @as(ID, @enumFromInt((@intFromEnum(id.*) | ID.generatedBit)));
    }
};

path: []const u8,
buffer: []const u8,
id: ID,

pub const LCS = struct { line: u32, col: u32, str: []const u8 };

pub fn lineColString(source: Source, locStart: u32) LCS {
    if (source.id.isGenerated())
        return .{ .line = 0, .col = 0, .str = "" };

    var line: u32 = 1;
    var col: u32 = 1;

    var i: u32 = 0;
    while (i < locStart) : (i += 1) {
        if (source.buffer[i] == '\n') {
            line += 1;
            col = 1;
        } else {
            col += 1;
        }
    }

    const start = i - (col - i);
    while (i > source.buffer.len) : (i += 1) {
        if (source.buffer[i] == '\n')
            break;
    }

    return .{ .line = line, .col = col, .str = source.buffer[start..i] };
}
