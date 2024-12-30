const std = @import("std");
const assert = std.debug.assert;

const Source = @This();

/// Definition of the Location structure, used to track a specific position within a Source.
/// It includes fields like ID, byte offset, line, and column.
pub const Location = struct {
    id: ID = .unused,
    byteOffset: u32 = 0,
    line: u32 = 0,

    pub fn eql(a: Location, b: Location) bool {
        return (a.id == b.id) and (a.byteOffset == b.byteOffset) and (a.line == b.line);
    }
};

/// Enumeration ID represents the state of a source code file.
/// - `unused` represent the source file is unused.
/// - `generated` represent the source file is generated by the Zinc.
/// - `_` represent the index of the source file.
pub const ID = enum(u32) {
    unused = 0,
    generated = 1,
    _,
};

/// Classifies the file for line marker output in -E mode
pub const Kind = enum {
    /// regular file
    User,
    /// Included from a system include directory
    System,
    /// Included from an "implicit extern C" directory
    ExternCSystem,
};

/// source file path
path: []const u8,
/// source file content buffer
buffer: []const u8,
///represent source file state or this source file where in position
id: ID,
/// invalid UTF-8 location
invalidUTF8Loc: ?Location = null,

/// each entry represents a byte position within `buf` where a backslash+newline was deleted
/// from the original raw buffer. The same position can appear multiple times if multiple
/// consecutive splices happened. Guaranteed to be non-decreasing
spliceLocs: []const u32,
kind: Kind,

/// Todo: binary search instead of scanning entire `spliceLocs`.
pub fn numSplicesBefore(source: Source, byteOffset: u32) u32 {
    for (source.spliceLocs, 0..) |spliceOffset, i|
        if (spliceOffset > byteOffset) return @intCast(i);
    return @intCast(source.spliceLocs.len);
}

/// Returns the actual line number (before newline splicing) of a Location
/// This corresponds to what the user would actually see in their text editor
pub fn physicalLine(source: Source, loc: Location) u32 {
    return loc.line + source.numSplicesBefore(loc.byteOffset);
}

/// Definition of the LineCol structure(corresponding to the byte offset).
const LineCol = struct {
    /// The content of the source code line as a byte slice.
    line: []const u8,
    /// The number of the line within the source code, starting from 1.
    lineNO: u32,
    /// The column number within the line, starting from 1.
    col: u32,
    /// The visual width of the line, taking into account Unicode full-width and half-width characters.
    width: u32,
    /// True if the line ends with a line splice (backslash followed by newline), false otherwise.
    endWithSplic: bool,
};

/// Calculates line and column information for a given location within a Source.
/// This includes finding the start and end of the line, and computing visual width and column number.
/// Handles newline splices within the source buffer.
///
/// @param source: The Source containing the text buffer and splice locations.
/// @param loc: The Location indicating the byte offset within the source buffer.
/// @return A LineCol struct containing the extracted line information.
pub fn getLineCol(source: Source, loc: Location) LineCol {
    var start: usize = 0;
    // The start of the line is after the last newline before the byte offset, or at the beginning of the buffer.
    if (std.mem.lastIndexOfScalar(u8, source.buffer[0..loc.byteOffset], '\n')) |some|
        start = some + 1;

    // Find the closest splice location before the current line start.
    const spliceIndex: u32 = for (source.spliceLocs, 0..) |spliceOffset, i| {
        if (spliceOffset > start) {
            if (spliceOffset < loc.byteOffset) {
                start = spliceOffset; // Use the splice as the new line start if it's before the location.
                break @as(u32, @intCast(i)) + 1;
            }
            break @intCast(i); // No splice before location, return current index.
        }
    } else @intCast(source.spliceLocs.len); // No splices found, return length of splices array.

    var i: usize = start;
    var col: u32 = 1; // Column numbers start at 1.
    var width: u32 = 0; // Width is the calculated visual width of the characters.

    // Iterate from the start of the line to the given byte offset to calculate column and visual width.
    while (i < loc.byteOffset) : (col += 1) {
        const len = std.unicode.utf8ByteSequenceLength(source.buffer[i]) catch {
            i += 1; // Skip invalid UTF-8 sequence and continue.
            continue;
        };
        const cp = std.unicode.utf8Decode(source.buffer[i..][0..len]) catch {
            i += 1;
            continue;
        };
        width += codepointWidth(cp); // Add the visual width of the code point.

        i += len; // Advance by the length of the UTF-8 sequence.
    }

    // Find the end of the line by looking for the next newline or EOF.
    var nl = source.buffer.len;
    var endWithSplice = false;
    if (std.mem.indexOfScalar(u8, source.buffer[start..], '\n')) |some|
        nl = some + start;
    if (source.spliceLocs.len > spliceIndex and
        nl > source.spliceLocs[spliceIndex] and
        source.spliceLocs[spliceIndex] > start)
    {
        endWithSplice = true;
        nl = source.spliceLocs[spliceIndex];
    }

    return .{
        .line = source.buffer[start..nl],
        .lineNO = loc.line + spliceIndex,
        .col = col,
        .width = width,
        .endWithSplic = endWithSplice,
    };
}

/// Determines the display width of a given Unicode code point.
/// Returns 1 for half-width characters and 2 for full-width characters.
fn codepointWidth(cp: u32) u32 {
    return switch (cp) {
        // Half-width characters and special cases
        0x1100...0x115F,
        0x2329,
        0x232A,
        0x2E80...0x303F,
        0x3040...0x3247,
        0x3250...0x4DBF,
        0x4E00...0xA4C6,
        0xA960...0xA97C,
        0xAC00...0xD7A3,
        0xF900...0xFAFF,
        0xFE10...0xFE19,
        0xFE30...0xFE6B,
        0xFF01...0xFF60,
        0xFFE0...0xFFE6,
        0x1B000...0x1B001,
        0x1F200...0x1F251,
        0x20000...0x3FFFD,
        0x1F300...0x1F5FF,
        0x1F900...0x1F9FF,
        => 2,
        else => 1, // Full-width characters
    };
}