const std = @import("std");
const process = std.process;
const builtin = @import("builtin");
const zinc = @import("zinc");
const Compilation = zinc.Compilation;
const Diagnostics = zinc.Diagnostics;
const Driver = zinc.Driver;
const Toolchain = zinc.ToolChain;
const Target = zinc.TargetUtil;

var GeneralPurposeAllocator = std.heap.GeneralPurposeAllocator(.{}){};

pub fn main() u8 {
    const gpa = if (builtin.link_libc) std.heap.raw_c_allocator else GeneralPurposeAllocator.allocator();
    defer if (!builtin.link_libc) {
        _ = GeneralPurposeAllocator.deinit();
    };

    var arenaInstance = std.heap.ArenaAllocator.init(gpa);
    defer arenaInstance.deinit();

    const arena = arenaInstance.allocator();
    const args = process.argsAlloc(arena) catch {
        std.debug.print("Out of Memory\n", .{});
        return 1;
    };

    const fastExit = @import("builtin").mode != .Debug;

    const zincName = std.fs.selfExePathAlloc(gpa) catch {
        std.debug.print("unable to find Zinc executable path\n", .{});
        if (fastExit) process.exit(1);
        return 1;
    };
    defer gpa.free(zincName);

    const stderrFile = std.io.getStdErr();
    var diagnostics: Diagnostics = .{
        .output = .{
            .toFile = .{
                .config = std.io.tty.detectConfig(stderrFile),
                .file = stderrFile,
            },
        },
    };

    var comp = Compilation.initDefault(gpa, &diagnostics, std.fs.cwd()) catch |er| switch (er) {
        error.OutOfMemory => {
            std.debug.print("Out of Memory\n", .{});
            if (fastExit) process.exit(1);
            return 1;
        },
    };
    defer comp.deinit();

    var driver = Driver{ .comp = &comp, .zincName = zincName, .diagnostics = &diagnostics };
    defer driver.deinit();

    var toolChain: Toolchain = .{ .driver = &driver, .arena = arena, .filesystem = .{ .real = comp.cwd } };
    defer toolChain.deinit();

    driver.main(&toolChain, args, fastExit) catch |er| switch (er) {
        error.OutOfMemory => {
            std.debug.print("Out of Memory\n", .{});
            if (fastExit) process.exit(1);
            return 1;
        },
        error.StreamTooLong => {
            std.debug.print("maximum file size exceeded\n", .{});
            if (fastExit) process.exit(1);
            return 1;
        },
        error.FatalError => {
            driver.printDiagnosticsStats();
            if (fastExit) process.exit(1);
            return 1;
        },
        error.TooManyMultilibs => {
            std.debug.print("found more than one multilib with the same priority\n", .{});
            if (fastExit) process.exit(1);
            return 1;
        },
        else => |err| return err,
    };

    if (fastExit) process.exit(@intFromBool(comp.diagnostics.errors != 0));
    return @intFromBool(diagnostics.errors != 0);
}
