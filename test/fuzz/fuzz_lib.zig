const std = @import("std");
const zinc = @import("zinc");
const mem = std.mem;
const Allocator = mem.Allocator;
const process = std.process;
const Codegen = zinc.CodeGen;
const Compilation = zinc.Compilation;
const Source = zinc.Source;
const Preprocessor = zinc.Preprocessor;
const Parser = zinc.Parser;

var fixed_buffer_mem: [10 * 1024 * 1024]u8 = undefined;

export fn compile_c_buf(buf: [*]const u8, len: c_int) void {
    compileSlice(buf[0..@as(usize, @intCast(len))]) catch unreachable;
}

fn compileSlice(buf: []const u8) !void {
    var fixed_allocator = std.heap.FixedBufferAllocator.init(fixed_buffer_mem[0..]);
    const allocator = fixed_allocator.allocator();

    var comp = Compilation.init(allocator);
    defer comp.deinit();

    try comp.addDefaultPragmaHandlers();
    try comp.defineSystemIncludes();

    const builtin = try comp.generateBuiltinMacros();
    var userSource = Source{
        .id = @as(Source.ID, @enumFromInt(comp.sources.count() + 2)),
        .path = "<STDIN>",
        .buffer = buf,
    };
    userSource.checkUtf8();
    try comp.sources.put(userSource.path, userSource);

    processSource(&comp, builtin, userSource) catch |e| switch (e) {
        error.FatalError => {},
        else => |err| return err,
    };
    _ = comp.sources.swapRemove(userSource.path);
}

fn processSource(comp: *Compilation, builtin: Source, userSource: Source) !void {
    var pp = Preprocessor.init(comp);
    defer pp.deinit();
    try pp.addBuiltinMacros();

    _ = try pp.preprocess(builtin);
    const eof = try pp.preprocess(userSource);
    try pp.tokens.append(pp.compilation.gpa, eof);

    var tree = try Parser.parse(&pp);
    defer tree.deinit();

    try tree.dump(std.io.null_writer);
}
