const std = @import("std");
const process = std.process;
const builtin = @import("builtin");

const zinc = @import("zinc");
const Compilation = zinc.Compilation;
const Diagnostics = zinc.Diagnostics;
const Driver = zinc.Driver;
const Target = zinc.TargetUtil;
const Toolchain = zinc.ToolChain;
const AssemblyBackend = @import("assembly-backend");

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

    var stderrBuffer: [1024]u8 = undefined;
    var stderr = std.fs.File.stderr().writer(&stderrBuffer);
    var diagnostics: Diagnostics = .{
        .output = .{ .toWriter = .{
            .color = .detect(stderr.file),
            .writer = &stderr.interface,
        } },
    };

    var comp = Compilation.initDefault(gpa, arena, &diagnostics, std.fs.cwd()) catch |er| switch (er) {
        error.OutOfMemory => {
            std.debug.print("Out of Memory\n", .{});
            if (fastExit) process.exit(1);
            return 1;
        },
    };
    defer comp.deinit();

    var driver = Driver{ .comp = &comp, .zincName = zincName, .diagnostics = &diagnostics };
    defer driver.deinit();

    var toolChain: Toolchain = .{ .driver = &driver, .filesystem = .{ .real = comp.cwd } };
    defer toolChain.deinit();

    driver.main(&toolChain, args, fastExit, AssemblyBackend.genAsm) catch |er| switch (er) {
        error.OutOfMemory => {
            std.debug.print("Out of Memory\n", .{});
            if (fastExit) process.exit(1);
            return 1;
        },
        error.FatalError => {
            driver.printDiagnosticsStats();
            if (fastExit) process.exit(1);
            return 1;
        },
    };

    if (fastExit) process.exit(@intFromBool(comp.diagnostics.errors != 0));
    return @intFromBool(diagnostics.errors != 0);
}
