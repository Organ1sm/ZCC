const std = @import("std");

pub fn build(b: *std.Build) !void {
    const target = b.standardTargetOptions(.{});
    const mode = b.standardOptimizeOption(.{});

    const EnableLinkerBuildId = b.option(bool, "enable-linker-build-id", "pass --build-id to linker") orelse false;
    const TestAllAllocationFailures = b.option(bool, "test-all-allocation-failures", "Test all allocation failures") orelse false;
    const LinkLibc = b.option(bool, "link-libc", "Force self-hosted compile to link libc") orelse (mode != .Debug);
    const DefaultLinker = b.option([]const u8, "default-linker", "Default linker zcc will use if none is supplied via -fuse-ld") orelse "ld";
    const DefaultSysroot = b.option([]const u8, "default-sysroot", "Default <path> to all compiler invocations for --sysroot=<path>.") orelse "";
    const DefaultRtlib = b.option([]const u8, "default-rtlib", "Default compiler runtime library if --rtlib is not specified") orelse "";
    const DefaultUnwindlib = b.option([]const u8, "default-unwindlib", "Default unwind library to use (\"none\" \"libgcc\" or \"libunwind\", empty to match runtime library.)") orelse
        if (std.mem.eql(u8, DefaultRtlib, "libgcc")) "libgcc" else "";
    const GCCInstallPrefix = b.option([]const u8, "gcc-install-prefix", "Directory where gcc is installed.") orelse "";

    const systemDefaults = b.addOptions();
    systemDefaults.addOption(bool, "enableLinkerBuildId", EnableLinkerBuildId);
    systemDefaults.addOption([]const u8, "linker", DefaultLinker);
    systemDefaults.addOption([]const u8, "sysroot", DefaultSysroot);
    systemDefaults.addOption([]const u8, "gccInstallPrefix", GCCInstallPrefix);
    systemDefaults.addOption([]const u8, "rtlib", DefaultRtlib);
    systemDefaults.addOption([]const u8, "unwindlib", DefaultUnwindlib);

    const depsModule = b.createModule(.{ .root_source_file = b.path("deps/lib.zig") });
    const zincModule = b.addModule("zinc", .{
        .root_source_file = b.path("src/zinc.zig"),
        .imports = &.{
            .{
                .name = "system-defaults",
                .module = systemDefaults.createModule(),
            },
            .{
                .name = "deps",
                .module = depsModule,
            },
        },
    });

    const exe = b.addExecutable(.{
        .name = "Zcc",
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = .Debug,
        .single_threaded = true,
    });
    exe.root_module.addOptions("system-defaults", systemDefaults);
    exe.root_module.addImport("zinc", zincModule);
    exe.root_module.addImport("deps", depsModule);

    if (LinkLibc)
        exe.linkLibC();

    b.installArtifact(exe);

    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const unit_tests_step = step: {
        var unit_tests = b.addTest(.{ .root_source_file = b.path("src/zinc.zig") });
        for (zincModule.import_table.keys(), zincModule.import_table.values()) |name, module| {
            unit_tests.root_module.addImport(name, module);
        }
        const run_test = b.addRunArtifact(unit_tests);

        const unit_test_step = b.step("test-unit", "run unit tests");
        unit_test_step.dependOn(&run_test.step);
        break :step unit_test_step;
    };

    const integration_tests_step = step: {
        var integration_tests = b.addExecutable(.{
            .name = "test-runner",
            .root_source_file = b.path("test/test_runner.zig"),
            .target = target,
            .optimize = mode,
        });
        integration_tests.root_module.addImport("zinc", zincModule);
        const test_runner_options = b.addOptions();
        integration_tests.root_module.addOptions("build_options", test_runner_options);
        test_runner_options.addOption(bool, "TestAllAllocationFailures", TestAllAllocationFailures);

        const integration_test_runner = b.addRunArtifact(integration_tests);
        integration_test_runner.addArg(b.pathFromRoot("test/cases"));
        integration_test_runner.addArg(b.graph.zig_exe);

        const integration_tests_step = b.step("test-integration", "Run integration tests");
        integration_tests_step.dependOn(&integration_test_runner.step);

        b.installArtifact(integration_tests);
        break :step integration_tests_step;
    };

    const record_tests_step = step: {
        const record_tests = b.addExecutable(.{
            .name = "record-runner",
            .root_source_file = b.path("test/record_runner.zig"),
            .optimize = mode,
            .target = target,
        });
        record_tests.root_module.addImport("zinc", zincModule);

        const record_tests_runner = b.addRunArtifact(record_tests);
        record_tests_runner.addArg(b.pathFromRoot("test/records"));

        const record_tests_step = b.step("test-record", "Run record layout tests");
        record_tests_step.dependOn(&record_tests_runner.step);

        break :step record_tests_step;
    };

    const tests_step = b.step("test", "Run all tests");
    tests_step.dependOn(unit_tests_step);
    tests_step.dependOn(integration_tests_step);
    tests_step.dependOn(record_tests_step);
}
