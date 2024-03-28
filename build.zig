const std = @import("std");
const ZigLibDirStep = @import("build/ZigLibDir.zig");

fn addFuzzStep(b: *std.Build, target: std.Build.ResolvedTarget, aflClangLTOPath: []const u8, zccModule: *std.Build.Module) !void {
    const fuzzStep = b.step("fuzz", "Build executable for fuzz testing.");
    var fuzzTarget = target;
    fuzzTarget.result.ofmt = .c;

    const libDirStep = try ZigLibDirStep.create(b);
    const compiler_rt = b.createModule(.{
        .root_source_file = libDirStep.getCompilerRTPath(),
    });

    const fuzz_lib = b.addStaticLibrary(.{
        .name = "fuzz-lib",
        .root_source_file = .{ .path = "test/fuzz/fuzz_lib.zig" },
        .optimize = .Debug,
        .target = fuzzTarget,
        .single_threaded = true,
    });

    fuzz_lib.root_module.addImport("zcc", zccModule);
    fuzz_lib.root_module.addImport("compiler_rt", compiler_rt);

    // We want `afl-clang-lto -o path/to/output test/fuzz/main.c path/to/library`
    const fuzz_compile = b.addSystemCommand(&.{
        aflClangLTOPath,
        "-Wno-incompatible-pointer-types",
        "-nostdinc",
        "-isystem",
    });
    fuzz_compile.addArgs(&.{
        "-isystem",
        "/usr/include",
        "-isystem",
        "/usr/local/include",
        "-std=c99",
    });
    fuzz_compile.addFileArg(.{ .path = "test/fuzz/main.c" });
    fuzz_compile.addArg("-I");
    fuzz_compile.addDirectoryArg(libDirStep.getLibPath());
    fuzz_compile.addArg("-o");
    const fuzz_exe = fuzz_compile.addOutputFileArg("zccfuzz");
    const fuzz_install = b.addInstallBinFile(fuzz_exe, "zccfuzz");

    // Add the path to the library file to afl-clang-lto's args
    fuzz_compile.addArtifactArg(fuzz_lib);
    fuzzStep.dependOn(&fuzz_install.step);
}

pub fn build(b: *std.Build) !void {
    const target = b.standardTargetOptions(.{});
    const mode = b.standardOptimizeOption(.{});

    const TestAllAllocationFailures = b.option(bool, "test-all-allocation-failures", "Test all allocation failures") orelse false;

    const depsModule = b.createModule(.{ .root_source_file = .{ .path = "deps/lib.zig" } });
    const zccModule = b.addModule("zcc", .{
        .root_source_file = .{ .path = "src/zcc.zig" },
        .imports = &.{.{
            .name = "deps",
            .module = depsModule,
        }},
    });

    const exe = b.addExecutable(.{
        .name = "Zcc",
        .root_source_file = .{ .path = "src/main.zig" },
        .target = target,
        .optimize = .Debug,
    });
    exe.root_module.addImport("deps", depsModule);

    // This declares intent for the executable to be installed into the
    // standard location when the user invokes the "install" step (the default
    // step when running `zig build`).
    b.installArtifact(exe);

    // This *creates* a Run step in the build graph, to be executed when another
    // step is evaluated that depends on it. The next line below will establish
    // such a dependency.
    const run_cmd = b.addRunArtifact(exe);

    // By making the run step depend on the install step, it will be run from the
    // installation directory rather than directly from within the cache directory.
    // This is not necessary, however, if the application depends on other installed
    // files, this ensures they will be present and in the expected location.
    run_cmd.step.dependOn(b.getInstallStep());

    // This allows the user to pass arguments to the application in the build
    // command itself, like this: `zig build run -- arg1 arg2 etc`
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    // This creates a build step. It will be visible in the `zig build --help` menu,
    // and can be selected like this: `zig build run`
    // This will evaluate the `run` step rather than the default, which is "install".
    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    // Similar to creating the run step earlier, this exposes a `test` step to
    // the `zig build --help` menu, providing a way for the user to request
    // running the unit tests.
    const test_step = b.step("test", "Run all tests");
    test_step.dependOn(&exe.step);

    // Creates a step for unit testing. This only builds the test executable
    // but does not run it.
    const unit_tests = b.addTest(.{
        .root_source_file = .{ .path = "src/main.zig" },
        .target = target,
        .optimize = mode,
    });
    unit_tests.root_module.addImport("deps", depsModule);
    const run_unit_tests = b.addRunArtifact(unit_tests);
    test_step.dependOn(&run_unit_tests.step);

    const integration_tests = b.addExecutable(.{
        .name = "test-runner",
        .root_source_file = .{ .path = "test/test_runner.zig" },
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

    b.installArtifact(integration_tests);
    test_step.dependOn(&integration_test_runner.step);

    const afl_clang_lto_path = b.option([]const u8, "afl-clang-lto-path", "Path to afl-clang-lto") orelse "afl-clang-lto";
    try addFuzzStep(b, target, afl_clang_lto_path, zccModule);
}
