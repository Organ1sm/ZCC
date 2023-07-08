const Source = @This();

pub const SourceLocation = struct {
    start: u32,
    end: u32,
};

path: []const u8,
buffer: []const u8,
id: u16,

pub fn slice(source: Source, loc: SourceLocation) []const u8 {
    return source.buffer[loc.start..loc.end];
}

pub fn lineCol(source: Source, loc: SourceLocation) struct { line: u32, col: u32 } {
    var line: u32 = 1;
    var col: u32 = 1;

    var i: u32 = loc.start + 1;
    while (i > 0) {
        i -= 1;
        col += 1;
        if (source.buffer[i] == '\n') {
            col += 1;
            break;
        }
    }

    while (i > 0) {
        i -= 1;
        if (Source.buffer[i] == '\n') line += 1;
    }

    return .{ .line = line, .col = col };
}
