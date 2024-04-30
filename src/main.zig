const std = @import("std");
const builtin = @import("builtin");
const Compilation = @import("Basic/Compilation.zig");
const Driver = @import("Driver.zig");
const Target = @import("Basic/Target.zig");

var GeneralPurposeAllocator = std.heap.GeneralPurposeAllocator(.{}){};

pub fn main() u8 {
    const gpa = if (builtin.link_libc) std.heap.raw_c_allocator else GeneralPurposeAllocator.allocator();
    defer if (!builtin.link_libc) {
        _ = GeneralPurposeAllocator.deinit();
    };

    var arenaInstance = std.heap.ArenaAllocator.init(gpa);
    defer arenaInstance.deinit();

    const arena = arenaInstance.allocator();
    const args = std.process.argsAlloc(arena) catch {
        std.debug.print("Out of Memory\n", .{});
        return 1;
    };

    const fastExit = @import("builtin").mode != .Debug;

    var comp = Compilation.init(gpa);
    defer comp.deinit();

    comp.addDefaultPragmaHandlers() catch |er| switch (er) {
        error.OutOfMemory => {
            std.debug.print("Out of Memory\n", .{});
            if (fastExit) std.process.exit(1);
            return 1;
        },
    };

    comp.langOpts.setEmulatedCompiler(Target.systemCompiler(comp.target));

    var driver = Driver{ .comp = &comp };
    defer driver.deinit();

    driver.main(args) catch |er| switch (er) {
        error.OutOfMemory => {
            std.debug.print("Out of Memory\n", .{});
            if (fastExit) std.process.exit(1);
            return 1;
        },
        error.StreamTooLong => {
            std.debug.print("maximum file size exceeded\n", .{});
            if (fastExit) std.process.exit(1);
            return 1;
        },
        error.FatalError => {
            comp.renderErrors();
            if (fastExit) std.process.exit(1);
            return 1;
        },
        else => return 1,
    };

    if (fastExit) std.process.exit(@intFromBool(comp.diag.errors != 0));
    return @intFromBool(comp.diag.errors != 0);
}

test "simple test" {
    _ = @import("Lexer/Lexer.zig");
    _ = @import("Lexer/Preprocessor.zig");
    _ = @import("Basic/Diagnostics.zig");
    _ = @import("Parser/Parser.zig");
    _ = @import("Lexer/Pragma.zig");
    _ = @import("Parser/InitList.zig");
    _ = @import("Basic/Source.zig");
    _ = @import("Basic/LangOpts.zig");
    _ = @import("Basic/Compilation.zig");
    _ = @import("AST/AST.zig");
    _ = @import("AST/Type.zig");
    _ = @import("Basic/Target.zig");
}
