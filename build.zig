const std = @import("std");

pub fn build(b: *std.Build) !void {
    const target = b.standardTargetOptions(.{});
    const mode = b.standardOptimizeOption(.{});

    const TestAllAllocationFailures = b.option(bool, "test-all-allocation-failures", "Test all allocation failures") orelse false;
    const LinkLibc = b.option(bool, "link-libc", "Force self-hosted compile to link libc") orelse (mode != .Debug);

    const depsModule = b.createModule(.{ .root_source_file = b.path("deps/lib.zig") });
    const zccModule = b.addModule("zcc", .{
        .root_source_file = b.path("src/zcc.zig"),
        .imports = &.{.{
            .name = "deps",
            .module = depsModule,
        }},
    });

    const exe = b.addExecutable(.{
        .name = "Zcc",
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = .Debug,
    });
    exe.root_module.addImport("zcc", zccModule);
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
        var unit_tests = b.addTest(.{ .root_source_file = b.path("src/main.zig") });
        unit_tests.root_module.addImport("deps", depsModule);

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
        integration_tests.root_module.addImport("zcc", zccModule);
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
        record_tests.root_module.addImport("zcc", zccModule);

        const record_tests_runner = b.addRunArtifact(record_tests);
        record_tests_runner.addArg(b.pathFromRoot("test/records"));

        const record_tests_step = b.step("test-record", "Run record layout tests");
        record_tests_step.dependOn(&record_tests_runner.step);


        b.installArtifact(record_tests);
        break :step record_tests_step;
    };

    const tests_step = b.step("test", "Run all tests");
    tests_step.dependOn(unit_tests_step);
    tests_step.dependOn(integration_tests_step);
    tests_step.dependOn(record_tests_step);

}
