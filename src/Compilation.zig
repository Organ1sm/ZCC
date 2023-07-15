const std = @import("std");
const builtin = @import("builtin");
const Source = @import("Source.zig");
const Diagnostics = @import("Diagnostics.zig");
const Token = @import("Token.zig").Token;

const Allocator = std.mem.Allocator;

const Compilation = @This();
pub const Error = error{
    /// A fatal error has ocurred and compilation has stopped.
    FatalError,
} || Allocator.Error;

gpa: Allocator,
sources: std.StringArrayHashMap(Source),
diag: Diagnostics,


pub fn init(gpa: Allocator) Compilation {
    return .{
        .gpa = gpa,
        .sources = std.StringArrayHashMap(Source).init(gpa),
        .diag = Diagnostics.init(gpa),
    };
}

pub fn deinit(compilation: *Compilation) void {
    for (compilation.sources.values()) |source| {
        compilation.gpa.free(source.path);
        compilation.gpa.free(source.buffer);
    }

    compilation.sources.deinit();
    compilation.diag.deinit();
}

pub fn getSource(comp: *Compilation, id: Source.ID) Source {
    var src = comp.sources.values()[id.index()];
    if (id.isGenerated())
        src.id.markGenerated();

    return src;
}

pub fn addSource(compilation: *Compilation, path: []const u8) !Source {
    if (compilation.sources.get(path)) |some| return some;

    const file = try std.fs.cwd().openFile(path, .{});
    defer file.close();

    const dupedPath = try compilation.gpa.dupe(u8, path);
    errdefer compilation.gpa.free(dupedPath);

    const contents = try file.reader().readAllAlloc(compilation.gpa, std.math.maxInt(u32));
    errdefer compilation.gpa.free(contents);

    const source = Source{
        .id = @enumFromInt(@as(u15, @intCast(compilation.sources.count()))),
        .path = dupedPath,
        .buffer = contents,
    };

    try compilation.sources.put(dupedPath, source);

    return source;
}

pub fn findInclude(comp: *Compilation, token: Token, filename: []const u8, searchWord: bool) !Source {
    _ = searchWord;
    return comp.addSource(filename) catch return comp.fatal(token, "'{s}' not found", .{filename});
}

pub fn fatal(comp: *Compilation, token: Token, comptime fmt: []const u8, args :anytype) Error {
    const source = comp.getSource(token.source);
    const lcs = source.lineColString(token.loc.start);

    return comp.diag.fatal(source.path, lcs, fmt, args);
}

pub const fatalNoSrc = Diagnostics.fatalNoSrc;
pub const renderErrors = Diagnostics.render;
