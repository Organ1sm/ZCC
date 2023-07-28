const Source = @This();

pub const Location = struct {
    id: ID = .unused,
    byteOffset: u32 = 0,
    next: ?*Location = null,
};

pub const ID = enum(u32) {
    unused = 0,
    generated = 1,
    _,
};

path: []const u8,
buffer: []const u8,
id: ID,

pub const LCS = struct { line: u32, col: u32, str: []const u8 };

pub fn lineColString(source: Source, byteOffset: u32) LCS {
    var line: u32 = 1;
    var col: u32 = 1;

    var i: u32 = 0;
    while (i < byteOffset) : (i += 1) {
        if (source.buffer[i] == '\n') {
            line += 1;
            col = 1;
        } else {
            col += 1;
        }
    }

    const start = i - (col - i);
    while (i < source.buffer.len) : (i += 1) {
        if (source.buffer[i] == '\n')
            break;
    }

    return .{ .line = line, .col = col, .str = source.buffer[start..i] };
}
