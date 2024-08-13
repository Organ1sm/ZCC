const std = @import("std");
const builtin = @import("builtin");
const Compilation = @import("Basic/Compilation.zig");
const Driver = @import("Driver.zig");
const Toolchain = @import("Toolchain.zig");
const Target = @import("Basic/Target.zig");

var GeneralPurposeAllocator = std.heap.GeneralPurposeAllocator(.{}){};

pub fn main() !u8 {
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

    const zccName = std.fs.selfExePathAlloc(gpa) catch {
        std.debug.print("unable to find Zcc executable path\n", .{});
        if (fastExit) std.process.exit(1);
        return 1;
    };
    defer gpa.free(zccName);

    var comp = Compilation.init(gpa);
    defer comp.deinit();

    comp.environment.loadAll(gpa) catch |er| switch (er) {
        error.OutOfMemory => {
            std.debug.print("Out of Memory\n", .{});
            if (fastExit) std.process.exit(1);
            return 1;
        },
    };
    defer comp.environment.deinit(gpa);

    comp.addDefaultPragmaHandlers() catch |er| switch (er) {
        error.OutOfMemory => {
            std.debug.print("Out of Memory\n", .{});
            if (fastExit) std.process.exit(1);
            return 1;
        },
    };

    comp.langOpts.setEmulatedCompiler(Target.systemCompiler(comp.target));

    var driver = Driver{ .comp = &comp, .zccName = zccName };
    defer driver.deinit();

    var toolChain: Toolchain = .{ .driver = &driver, .arena = arena };
    defer toolChain.deinit();

    driver.main(&toolChain, args) catch |er| switch (er) {
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
        else => |err| return err,
    };

    if (fastExit) std.process.exit(@intFromBool(comp.diagnostics.errors != 0));
    return @intFromBool(comp.diagnostics.errors != 0);
}

test "simple test" {
    _ = @import("Basic/Diagnostics.zig");
    _ = @import("Basic/Source.zig");
    _ = @import("Basic/LangOpts.zig");
    _ = @import("Basic/Compilation.zig");
    _ = @import("Basic/Target.zig");
    _ = @import("Driver/Distro.zig");
    _ = @import("Lexer/Lexer.zig");
    _ = @import("Lexer/Preprocessor.zig");
    _ = @import("Lexer/Pragma.zig");
    _ = @import("Parser/Parser.zig");
    _ = @import("Parser/InitList.zig");
    _ = @import("AST/AST.zig");
    _ = @import("AST/Type.zig");
}
