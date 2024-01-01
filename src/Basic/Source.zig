const std = @import("std");
/// Source represents a source code file added to the compilation.
/// Contains the file path, contents buffer, and an ID.
const Source = @This();

/// Definition of the Location structure, used to track a specific position within a Source.
/// It includes fields like ID, byte offset, line, and column.
pub const Location = struct {
    id: ID = .unused,
    byteOffset: u32 = 0,
    line: u32 = 0,
};

/// Enumeration ID represents the state of a source code file.
pub const ID = enum(u32) {
    unused = 0,
    generated = 1,
    _,
};

/// Fields of the Source structure, including file path, content buffer, ID, and invalid UTF-8 location.
path: []const u8,
buffer: []const u8,
id: ID,
invalidUTF8Loc: ?Location = null,

/// Definition of the LineCol structure bundling line and column information.
const LineCol = struct { line: []const u8, col: u32 };

/// Function getLineCol calculates line number, column, and line string
/// for a given byte offset into the Source.
pub fn getLineCol(source: Source, byteOffset: u32) LineCol {
    var start = byteOffset;
    while (true) : (start -= 1) {
        if (start == 0) break;
        if (start < source.buffer.len and source.buffer[start] == '\n') {
            start += 1;
            break;
        }
    }

    const col = byteOffset - start + 1;
    return .{ .line = std.mem.sliceTo(source.buffer[start..], '\n'), .col = col };
}

/// Returns the first offset, if any, in buf where an invalid utf8 sequence
/// is found.
fn offsetOfInvalidUtf8(buffer: []const u8) ?u32 {
    std.debug.assert(buffer.len <= std.math.maxInt(u32));
    var i: u32 = 0;
    while (i < buffer.len) {
        if (std.unicode.utf8ByteSequenceLength(buffer[i])) |cpLen| {
            if (i + cpLen > buffer.len) return i;
            if (std.meta.isError(std.unicode.utf8Decode(buffer[i .. i + cpLen]))) return i;
            i += cpLen;
        } else |_| return i;
    }
    return null;
}

pub fn checkUtf8(source: *Source) void {
    if (offsetOfInvalidUtf8(source.buffer)) |offset| {
        source.invalidUTF8Loc = Location{ .id = source.id, .byteOffset = offset };
    }
}
