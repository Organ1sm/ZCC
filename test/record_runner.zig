const std = @import("std");
const assert = std.debug.assert;
const print = std.debug.print;
const zinc = @import("zinc");

/// These tests don't work for any platform due to bugs.
/// Skip entirely.
/// To skip a test entirely just put the test name as a single-element tuple e.g. initComptime(.{.{"0044"}});
const globalTestExclude = std.StaticStringMap(void).initComptime(.{});

fn lessThan(_: void, lhs: []const u8, rhs: []const u8) bool {
    return std.mem.lessThan(u8, lhs, rhs);
}

const MAX_MEM_PER_TEST = 1024 * 1024 * 16;

/// Set true to debug specific targets w/ specific tests.
const testSingleTarget = false;
const single_target = .{
    // .target = "arm-cortex_r4-ios-none:Clang",
    // .c_test = "0064",
    // .target = "s390x-generic-linux-gnu:Gcc",
    // .c_test = "00", // run all the tests
    // .target = "x86-i586-linux-gnu:Gcc",
    // .c_test = "0002",
    .target = "x86_64-x86_64-windows-msvc:Msvc",
    .c_test = "0018", // run all the tests
    // .target = "arm-arm1136j_s-freebsd-gnu:Clang",
    // .c_test = "0052",
};

const Stats = struct {
    okCount: u32 = 0,
    failCount: u32 = 0,
    skipCount: u32 = 0,
    invalidTargetCount: u32 = 0,
    maxAlloc: usize = 0,
    rootNode: std.Progress.Node,

    const ResultKind = enum {
        ok,
        fail,
        skip,
        invalidTarget,
    };

    fn recordResult(self: *Stats, kind: ResultKind) void {
        const ptr = switch (kind) {
            .ok => &self.okCount,
            .fail => &self.failCount,
            .skip => &self.skipCount,
            .invalidTarget => &self.invalidTargetCount,
        };
        _ = @atomicRmw(u32, ptr, .Add, 1, .monotonic);
    }

    fn updateMaxMemUsage(self: *Stats, bytes: usize) void {
        _ = @atomicRmw(usize, &self.maxAlloc, .Max, bytes, .monotonic);
    }
};

const TestCase = struct {
    c_define: []const u8,
    target: []const u8,
    path: []const u8,
    source: []const u8,

    const List = std.ArrayList(TestCase);
};

/// Types of failures expected.
/// For any given test. `true` = expected failure
const ExpectedFailure = struct {
    parse: bool = false,
    layout: bool = false,
    extra: bool = false,
    offset: bool = false,

    fn any(self: ExpectedFailure) bool {
        return self.parse or self.layout or self.extra or self.offset;
    }

    fn eql(self: ExpectedFailure, other: ExpectedFailure) bool {
        return std.meta.eql(self, other);
    }
};

const builtin = @import("builtin");
pub fn main() !void {
    var general_purpose_allocator = std.heap.GeneralPurposeAllocator(.{}){};
    const gpa = general_purpose_allocator.allocator();
    defer if (general_purpose_allocator.deinit() == .leak) std.process.exit(1);

    // max file size < 2M. max total use < 12M
    const args = try std.process.argsAlloc(gpa);
    defer std.process.argsFree(gpa, args);

    const testDir = args[1];

    if (args.len != 2) {
        print("expected test case directory and zig executable as only arguments\n", .{});
        return error.InvalidArguments;
    }

    var cases = std.ArrayList([]const u8).init(gpa);
    defer {
        for (cases.items) |path| gpa.free(path);
        cases.deinit();
    }

    // Collect all cases. Set scope to clean when done.
    {
        var cases_dir = try std.fs.cwd().openDir(args[1], .{ .iterate = true });
        defer cases_dir.close();
        var buf: [1024]u8 = undefined;
        var buf_strm = std.io.fixedBufferStream(&buf);

        var it = cases_dir.iterate();
        while (try it.next()) |entry| {
            if (entry.kind == .directory) continue;
            if (entry.kind != .file) {
                print("skipping non file entry '{s}'\n", .{entry.name});
                continue;
            }

            if (std.ascii.indexOfIgnoreCase(entry.name, "_test.c") != null) {
                buf_strm.reset();
                try buf_strm.writer().print("{s}{c}{s}", .{ args[1], std.fs.path.sep, entry.name });
                try cases.append(try gpa.dupe(u8, buf[0..buf_strm.pos]));
            }
        }
    }

    std.mem.sort([]const u8, cases.items, {}, lessThan);

    var arenaInstance = std.heap.ArenaAllocator.init(gpa);
    defer arenaInstance.deinit();

    const arena = arenaInstance.allocator();
    const threadCount = @max(1, std.Thread.getCpuCount() catch 1);

    var threadPool: std.Thread.Pool = undefined;
    try threadPool.init(.{ .allocator = arena, .n_jobs = @intCast(threadCount) });
    defer threadPool.deinit();

    var waitGroup: std.Thread.WaitGroup = .{};

    var testCases = TestCase.List.init(gpa);
    defer testCases.deinit();

    // Iterate over all cases
    for (cases.items) |path| {
        // Read the test into memory.
        const source = try std.fs.cwd().readFileAlloc(arena, path, std.math.maxInt(u32));
        try parseTargetsFromCode(&testCases, path, source);
    }

    const rootNode = std.Progress.start(.{
        .disable_printing = false,
        .root_name = "Layout",
        .estimated_total_items = testCases.items.len,
    });

    var stats = Stats{ .rootNode = rootNode };

    for (0..threadCount) |i| {
        waitGroup.start();
        try threadPool.spawn(runTestCases, .{ gpa, testDir, &waitGroup, testCases.items[i..], threadCount, &stats });
    }
    threadPool.waitAndWork(&waitGroup);
    rootNode.end();

    std.debug.print("max mem used = {:.2}\n", .{std.fmt.fmtIntSizeBin(stats.maxAlloc)});
    if (stats.okCount == cases.items.len and stats.skipCount == 0) {
        print("All {d} tests passed. ({d} invalid targets)\n", .{ stats.okCount, stats.invalidTargetCount });
    } else if (stats.failCount == 0) {
        print("{d} passed; {d} skipped. ({d} invalid targets)\n", .{ stats.okCount, stats.skipCount, stats.invalidTargetCount });
    } else {
        print("{d} passed; {d} failed. ({d} invalid targets)\n\n", .{ stats.okCount, stats.failCount, stats.invalidTargetCount });
        std.process.exit(1);
    }
}

fn runTestCases(
    allocator: std.mem.Allocator,
    testDir: []const u8,
    wg: *std.Thread.WaitGroup,
    testCases: []const TestCase,
    stride: usize,
    stats: *Stats,
) void {
    defer wg.finish();
    const mem = allocator.alloc(u8, MAX_MEM_PER_TEST) catch |err| {
        std.log.err("{s}", .{@errorName(err)});
        if (@errorReturnTrace()) |trace| {
            std.debug.dumpStackTrace(trace.*);
        }
        stats.recordResult(.fail);
        return;
    };

    defer allocator.free(mem);

    var fib = std.heap.FixedBufferAllocator.init(mem);

    for (testCases, 0..) |case, i| {
        if (i % stride != 0) continue;
        defer fib.end_index = 0;

        singleRun(fib.allocator(), testDir, case, stats) catch |err| {
            std.log.err("{s}", .{@errorName(err)});
            if (@errorReturnTrace()) |trace| {
                std.debug.dumpStackTrace(trace.*);
            }
            stats.recordResult(.fail);
        };
        stats.updateMaxMemUsage(fib.end_index);
    }
}

fn singleRun(alloc: std.mem.Allocator, testDir: []const u8, testCase: TestCase, stats: *Stats) !void {
    const path = testCase.path;

    var comp = zinc.Compilation.init(alloc);
    defer comp.deinit();

    try comp.addDefaultPragmaHandlers();
    try comp.defineSystemIncludes(testDir);

    try setTarget(&comp, testCase.target);
    switch (comp.target.os.tag) {
        .hermit => {
            stats.recordResult(.invalidTarget);
            return; // Skip targets zinc doesn't support.
        },
        .ios, .macos => {
            switch (comp.target.cpu.arch) {
                .x86, .arm => {
                    stats.recordResult(.invalid_target);
                    return; // Skip targets Aro doesn't support.
                },
                else => {},
            }
        },
        else => {},
    }

    var caseName = std.ArrayList(u8).init(alloc);
    defer caseName.deinit();

    const testName = std.mem.sliceTo(std.fs.path.basename(path), '_');
    try caseName.writer().print("{s} | {s} | {s}", .{
        testName,
        testCase.target,
        testCase.c_define,
    });

    var caseNode = stats.rootNode.start(caseName.items, 0);
    defer caseNode.end();

    const file = comp.addSourceFromBuffer(path, testCase.source) catch |err| {
        stats.recordResult(.fail);
        std.debug.print("could not add source '{s}': {s}\n", .{ path, @errorName(err) });
        return;
    };

    var macroBuffer = std.ArrayList(u8).init(comp.gpa);
    defer macroBuffer.deinit();

    comp.langOpts.setEmulatedCompiler(zinc.TargetUtil.systemCompiler(comp.target));

    const macroWriter = macroBuffer.writer();
    try macroWriter.print("#define {s}\n", .{testCase.c_define});
    if (comp.langOpts.emulate == .msvc) {
        comp.langOpts.enableMSExtensions();
        try macroWriter.writeAll("#define MSVC\n");
    }

    const userMacros = try comp.addSourceFromBuffer("<command line>", macroBuffer.items);
    const builtinMacros = try comp.generateBuiltinMacros();

    var pp = zinc.Preprocessor.init(&comp);
    defer pp.deinit();
    try pp.addBuiltinMacros();

    _ = try pp.preprocess(builtinMacros);
    _ = try pp.preprocess(userMacros);
    const eof = pp.preprocess(file) catch |err| {
        stats.recordResult(.fail);
        std.debug.print("could not preprocess file '{s}': {s}\n", .{ path, @errorName(err) });
        return;
    };
    try pp.tokens.append(alloc, eof);

    var tree = try zinc.Parser.parse(&pp);
    defer tree.deinit();
    tree.dump(.no_color, std.io.null_writer) catch {};

    if (testSingleTarget) {
        zinc.Diagnostics.render(&comp, std.io.tty.detectConfig(std.io.getStdErr()));
        return;
    }

    if (globalTestExclude.has(testName)) {
        stats.skipCount += 1;
        return;
    }

    var buf: [128]u8 = undefined;
    var bufferStrm = std.io.fixedBufferStream(&buf);
    try bufferStrm.writer().print("{s}|{s}", .{ testCase.target, testName });

    const expected = compErr.get(buf[0..bufferStrm.pos]) orelse ExpectedFailure{};

    if (comp.diagnostics.list.items.len == 0 and expected.any()) {
        std.debug.print("\nTest Passed when failures expected:\n\texpected:{any}\n", .{expected});
    } else {
        var m = zinc.Diagnostics.defaultMsgWriter(std.io.tty.detectConfig(std.io.getStdErr()));
        defer m.deinit();

        var actual = ExpectedFailure{};
        for (comp.diagnostics.list.items) |msg| {
            switch (msg.kind) {
                .@"fatal error", .@"error" => {},
                else => continue,
            }
            const src = comp.getSource(msg.loc.id);
            const line = src.getLineCol(msg.loc).line;
            if (std.ascii.indexOfIgnoreCase(line, "_Static_assert") != null) {
                if (std.ascii.indexOfIgnoreCase(line, "_extra_") != null) {
                    actual.extra = true;
                } else if (std.ascii.indexOfIgnoreCase(line, "_bitoffsetof") != null) {
                    actual.offset = true;
                } else if (std.ascii.indexOfIgnoreCase(line, "sizeof") != null or
                    std.ascii.indexOfIgnoreCase(line, "_alignof") != null)
                {
                    actual.layout = true;
                } else unreachable;
            } else {
                actual.parse = true;
            }
        }

        if (!expected.eql(actual)) {
            m.print("\nexp:{any}\nact:{any}\n", .{ expected, actual });
            for (comp.diagnostics.list.items) |msg| {
                zinc.Diagnostics.renderMessage(&comp, &m, msg);
            }
            stats.recordResult(.fail);
        } else if (actual.any()) {
            stats.skipCount += 1;
        } else {
            stats.okCount += 1;
        }
    }
}

/// Get Zig std.Target from string in the arch-cpu-os-abi format.
fn getTarget(zig_target_string: []const u8) !std.Target {
    var buffer: [128]u8 = undefined;
    var iter = std.mem.tokenizeScalar(u8, zig_target_string, '-');

    const arch = iter.next().?;
    const model = iter.next().?;
    const os = iter.next().?;
    const abi = iter.next().?;
    var fb = std.io.fixedBufferStream(&buffer);
    try std.fmt.format(fb.writer(), "{s}-{s}-{s}", .{ arch, os, abi });

    const query = try std.Target.Query.parse(.{
        .arch_os_abi = fb.getWritten(),
        .cpu_features = model,
    });
    return std.zig.system.resolveTargetQuery(query);
}

fn setTarget(comp: *zinc.Compilation, target: []const u8) !void {
    const compiler_split_index = std.mem.indexOf(u8, target, ":").?;

    comp.target = try getTarget(target[0..compiler_split_index]);
    comp.langOpts.emulate = zinc.TargetUtil.systemCompiler(comp.target);

    const expected_compiler_name = target[compiler_split_index + 1 ..];
    const set_name = @tagName(comp.langOpts.emulate);

    // std.debug.print(" '{s}': {s}\n", .{ expected_compiler_name, set_name });

    assert(std.ascii.eqlIgnoreCase(set_name, expected_compiler_name));
}

fn parseTargetsFromCode(cases: *TestCase.List, path: []const u8, source: []const u8) !void {
    var lines = std.mem.tokenizeScalar(u8, source, '\n');
    while (lines.next()) |line| {
        if (std.mem.indexOf(u8, line, "// MAPPING|") == null) continue;

        assert(std.mem.count(u8, line, "|") > 1);
        var parts = std.mem.tokenizeScalar(u8, line, '|');
        _ = parts.next(); // Skip the MAPPING bit
        const define = parts.next().?; // The define to set for this chunk.

        while (parts.next()) |target| {
            if (std.mem.startsWith(u8, target, "END")) break;
            // These point to source, which lives
            // for the life of the test. So should be ok
            try cases.append(.{
                .path = path,
                .source = source,
                .c_define = define,
                .target = target,
            });
        }
    }
}

const compErr = blk: {
    @setEvalBranchQuota(100_000);
    break :blk std.StaticStringMap(ExpectedFailure).initComptime(.{
        .{
            "aarch64-generic-windows-msvc:Msvc|0011",
            .{ .parse = false, .layout = true, .extra = true, .offset = false },
        },
        .{
            "aarch64-generic-windows-msvc:Msvc|0014",
            .{ .parse = false, .layout = true, .extra = true, .offset = false },
        },
        .{
            "aarch64-generic-windows-msvc:Msvc|0018",
            .{ .parse = false, .layout = false, .extra = true, .offset = false },
        },
        .{
            "aarch64-generic-windows-msvc:Msvc|0020",
            .{ .parse = false, .layout = true, .extra = true, .offset = true },
        },
        .{
            "aarch64-generic-windows-msvc:Msvc|0020",
            .{ .parse = false, .layout = true, .extra = true, .offset = true },
        },
        .{
            "aarch64-generic-windows-msvc:Msvc|0021",
            .{ .parse = false, .layout = true, .extra = true, .offset = true },
        },
        .{
            "aarch64-generic-windows-msvc:Msvc|0023",
            .{ .parse = false, .layout = true, .extra = true, .offset = false },
        },
        .{
            "aarch64-generic-windows-msvc:Msvc|0025",
            .{ .parse = false, .layout = true, .extra = true, .offset = false },
        },
        .{
            "aarch64-generic-windows-msvc:Msvc|0026",
            .{ .parse = false, .layout = true, .extra = true, .offset = false },
        },
        .{
            "aarch64-generic-windows-msvc:Msvc|0029",
            .{ .parse = false, .layout = true, .extra = true, .offset = false },
        },
        .{
            "aarch64-generic-windows-msvc:Msvc|0037",
            .{ .parse = false, .layout = true, .extra = true, .offset = false },
        },
        .{
            "aarch64-generic-windows-msvc:Msvc|0039",
            .{ .parse = false, .layout = true, .extra = true, .offset = false },
        },
        .{
            "aarch64-generic-windows-msvc:Msvc|0042",
            .{ .parse = false, .layout = true, .extra = true, .offset = true },
        },
        .{
            "aarch64-generic-windows-msvc:Msvc|0044",
            .{ .parse = false, .layout = true, .extra = true, .offset = false },
        },
        .{
            "aarch64-generic-windows-msvc:Msvc|0045",
            .{ .parse = false, .layout = false, .extra = true, .offset = false },
        },
        .{
            "aarch64-generic-windows-msvc:Msvc|0046",
            .{ .parse = false, .layout = true, .extra = true, .offset = false },
        },
        .{
            "aarch64-generic-windows-msvc:Msvc|0053",
            .{ .parse = false, .layout = false, .extra = true, .offset = false },
        },
        .{
            "aarch64-generic-windows-msvc:Msvc|0063",
            .{ .parse = false, .layout = true, .extra = true, .offset = false },
        },
        .{
            "aarch64-generic-windows-msvc:Msvc|0065",
            .{ .parse = false, .layout = true, .extra = true, .offset = false },
        },
        .{
            "aarch64-generic-windows-msvc:Msvc|0066",
            .{ .parse = false, .layout = true, .extra = true, .offset = false },
        },
        .{
            "aarch64-generic-windows-msvc:Msvc|0072",
            .{ .parse = false, .layout = true, .extra = true, .offset = false },
        },
        .{
            "aarch64-generic-windows-msvc:Msvc|0080",
            .{ .parse = false, .layout = true, .extra = true, .offset = false },
        },
        .{
            "x86-i586-windows-msvc:Msvc|0011",
            .{ .parse = false, .layout = true, .extra = true, .offset = false },
        },
        .{
            "x86-i586-windows-msvc:Msvc|0014",
            .{ .parse = false, .layout = true, .extra = true, .offset = false },
        },
        .{
            "x86-i586-windows-msvc:Msvc|0018",
            .{ .parse = false, .layout = false, .extra = true, .offset = false },
        },
        .{
            "x86-i586-windows-msvc:Msvc|0023",
            .{ .parse = false, .layout = true, .extra = true, .offset = false },
        },
        .{
            "x86-i586-windows-msvc:Msvc|0025",
            .{ .parse = false, .layout = true, .extra = true, .offset = false },
        },
        .{
            "x86-i586-windows-msvc:Msvc|0026",
            .{ .parse = false, .layout = true, .extra = true, .offset = false },
        },
        .{
            "x86-i586-windows-msvc:Msvc|0029",
            .{ .parse = false, .layout = true, .extra = true, .offset = false },
        },
        .{
            "x86-i586-windows-msvc:Msvc|0042",
            .{ .parse = false, .layout = true, .extra = true, .offset = true },
        },
        .{
            "x86-i586-windows-msvc:Msvc|0044",
            .{ .parse = false, .layout = true, .extra = true, .offset = false },
        },
        .{
            "x86-i586-windows-msvc:Msvc|0045",
            .{ .parse = false, .layout = false, .extra = true, .offset = false },
        },
        .{
            "x86-i586-windows-msvc:Msvc|0046",
            .{ .parse = false, .layout = true, .extra = true, .offset = false },
        },
        .{
            "x86-i586-windows-msvc:Msvc|0053",
            .{ .parse = false, .layout = false, .extra = true, .offset = false },
        },
        .{
            "x86-i586-windows-msvc:Msvc|0063",
            .{ .parse = false, .layout = true, .extra = true, .offset = false },
        },
        .{
            "x86-i586-windows-msvc:Msvc|0065",
            .{ .parse = false, .layout = true, .extra = true, .offset = false },
        },
        .{
            "x86-i586-windows-msvc:Msvc|0066",
            .{ .parse = false, .layout = true, .extra = true, .offset = false },
        },
        .{
            "x86-i686-uefi-msvc:Msvc|0011",
            .{ .parse = false, .layout = true, .extra = true, .offset = false },
        },
        .{
            "x86-i686-uefi-msvc:Msvc|0014",
            .{ .parse = false, .layout = true, .extra = true, .offset = false },
        },
        .{
            "x86-i686-uefi-msvc:Msvc|0018",
            .{ .parse = false, .layout = false, .extra = true, .offset = false },
        },
        .{
            "x86-i686-uefi-msvc:Msvc|0023",
            .{ .parse = false, .layout = true, .extra = true, .offset = false },
        },
        .{
            "x86-i686-uefi-msvc:Msvc|0025",
            .{ .parse = false, .layout = true, .extra = true, .offset = false },
        },
        .{
            "x86-i686-uefi-msvc:Msvc|0026",
            .{ .parse = false, .layout = true, .extra = true, .offset = false },
        },
        .{
            "x86-i686-uefi-msvc:Msvc|0029",
            .{ .parse = false, .layout = true, .extra = true, .offset = false },
        },
        .{
            "x86-i686-uefi-msvc:Msvc|0042",
            .{ .parse = false, .layout = true, .extra = true, .offset = true },
        },
        .{
            "x86-i686-uefi-msvc:Msvc|0044",
            .{ .parse = false, .layout = true, .extra = true, .offset = false },
        },
        .{
            "x86-i686-uefi-msvc:Msvc|0045",
            .{ .parse = false, .layout = false, .extra = true, .offset = false },
        },
        .{
            "x86-i686-uefi-msvc:Msvc|0046",
            .{ .parse = false, .layout = true, .extra = true, .offset = false },
        },
        .{
            "x86-i686-uefi-msvc:Msvc|0053",
            .{ .parse = false, .layout = false, .extra = true, .offset = false },
        },
        .{
            "x86-i686-uefi-msvc:Msvc|0063",
            .{ .parse = false, .layout = true, .extra = true, .offset = false },
        },
        .{
            "x86-i686-uefi-msvc:Msvc|0065",
            .{ .parse = false, .layout = true, .extra = true, .offset = false },
        },
        .{
            "x86-i686-uefi-msvc:Msvc|0066",
            .{ .parse = false, .layout = true, .extra = true, .offset = false },
        },
        .{
            "x86-i686-windows-msvc:Msvc|0011",
            .{ .parse = false, .layout = true, .extra = true, .offset = false },
        },
        .{
            "x86-i686-windows-msvc:Msvc|0014",
            .{ .parse = false, .layout = true, .extra = true, .offset = false },
        },
        .{
            "x86-i686-windows-msvc:Msvc|0018",
            .{ .parse = false, .layout = false, .extra = true, .offset = false },
        },
        .{
            "x86-i686-windows-msvc:Msvc|0023",
            .{ .parse = false, .layout = true, .extra = true, .offset = false },
        },
        .{
            "x86-i686-windows-msvc:Msvc|0025",
            .{ .parse = false, .layout = true, .extra = true, .offset = false },
        },
        .{
            "x86-i686-windows-msvc:Msvc|0026",
            .{ .parse = false, .layout = true, .extra = true, .offset = false },
        },
        .{
            "x86-i686-windows-msvc:Msvc|0029",
            .{ .parse = false, .layout = true, .extra = true, .offset = false },
        },
        .{
            "x86-i686-windows-msvc:Msvc|0042",
            .{ .parse = false, .layout = true, .extra = true, .offset = true },
        },
        .{
            "x86-i686-windows-msvc:Msvc|0044",
            .{ .parse = false, .layout = true, .extra = true, .offset = false },
        },
        .{
            "x86-i686-windows-msvc:Msvc|0045",
            .{ .parse = false, .layout = false, .extra = true, .offset = false },
        },
        .{
            "x86-i686-windows-msvc:Msvc|0046",
            .{ .parse = false, .layout = true, .extra = true, .offset = false },
        },
        .{
            "x86-i686-windows-msvc:Msvc|0053",
            .{ .parse = false, .layout = false, .extra = true, .offset = false },
        },
        .{
            "x86-i686-windows-msvc:Msvc|0063",
            .{ .parse = false, .layout = true, .extra = true, .offset = false },
        },
        .{
            "x86-i686-windows-msvc:Msvc|0065",
            .{ .parse = false, .layout = true, .extra = true, .offset = false },
        },
        .{
            "x86-i686-windows-msvc:Msvc|0066",
            .{ .parse = false, .layout = true, .extra = true, .offset = false },
        },
        .{
            "thumb-baseline-windows-msvc:Msvc|0011",
            .{ .parse = false, .layout = true, .extra = true, .offset = false },
        },
        .{
            "thumb-baseline-windows-msvc:Msvc|0014",
            .{ .parse = false, .layout = true, .extra = true, .offset = false },
        },
        .{
            "thumb-baseline-windows-msvc:Msvc|0018",
            .{ .parse = false, .layout = false, .extra = true, .offset = false },
        },
        .{
            "thumb-baseline-windows-msvc:Msvc|0020",
            .{ .parse = false, .layout = true, .extra = true, .offset = true },
        },
        .{
            "thumb-baseline-windows-msvc:Msvc|0021",
            .{ .parse = false, .layout = true, .extra = true, .offset = true },
        },
        .{
            "thumb-baseline-windows-msvc:Msvc|0023",
            .{ .parse = false, .layout = true, .extra = true, .offset = false },
        },
        .{
            "thumb-baseline-windows-msvc:Msvc|0025",
            .{ .parse = false, .layout = true, .extra = true, .offset = false },
        },
        .{
            "thumb-baseline-windows-msvc:Msvc|0026",
            .{ .parse = false, .layout = true, .extra = true, .offset = false },
        },
        .{
            "thumb-baseline-windows-msvc:Msvc|0029",
            .{ .parse = false, .layout = true, .extra = true, .offset = false },
        },
        .{
            "thumb-baseline-windows-msvc:Msvc|0037",
            .{ .parse = false, .layout = true, .extra = true, .offset = false },
        },
        .{
            "thumb-baseline-windows-msvc:Msvc|0039",
            .{ .parse = false, .layout = true, .extra = true, .offset = false },
        },
        .{
            "thumb-baseline-windows-msvc:Msvc|0042",
            .{ .parse = false, .layout = true, .extra = true, .offset = true },
        },
        .{
            "thumb-baseline-windows-msvc:Msvc|0044",
            .{ .parse = false, .layout = true, .extra = true, .offset = false },
        },
        .{
            "thumb-baseline-windows-msvc:Msvc|0045",
            .{ .parse = false, .layout = false, .extra = true, .offset = false },
        },
        .{
            "thumb-baseline-windows-msvc:Msvc|0046",
            .{ .parse = false, .layout = true, .extra = true, .offset = false },
        },
        .{
            "thumb-baseline-windows-msvc:Msvc|0053",
            .{ .parse = false, .layout = false, .extra = true, .offset = false },
        },
        .{
            "thumb-baseline-windows-msvc:Msvc|0063",
            .{ .parse = false, .layout = true, .extra = true, .offset = false },
        },
        .{
            "thumb-baseline-windows-msvc:Msvc|0065",
            .{ .parse = false, .layout = true, .extra = true, .offset = false },
        },
        .{
            "thumb-baseline-windows-msvc:Msvc|0066",
            .{ .parse = false, .layout = true, .extra = true, .offset = false },
        },
        .{
            "thumb-baseline-windows-msvc:Msvc|0072",
            .{ .parse = false, .layout = true, .extra = true, .offset = false },
        },
        .{
            "thumb-baseline-windows-msvc:Msvc|0080",
            .{ .parse = false, .layout = true, .extra = true, .offset = false },
        },
        .{
            "x86_64-x86_64-uefi-msvc:Msvc|0011",
            .{ .parse = false, .layout = true, .extra = true, .offset = false },
        },
        .{
            "x86_64-x86_64-uefi-msvc:Msvc|0014",
            .{ .parse = false, .layout = true, .extra = true, .offset = false },
        },
        .{
            "x86_64-x86_64-uefi-msvc:Msvc|0018",
            .{ .parse = false, .layout = false, .extra = true, .offset = false },
        },
        .{
            "x86_64-x86_64-uefi-msvc:Msvc|0023",
            .{ .parse = false, .layout = true, .extra = true, .offset = false },
        },
        .{
            "x86_64-x86_64-uefi-msvc:Msvc|0025",
            .{ .parse = false, .layout = true, .extra = true, .offset = false },
        },
        .{
            "x86_64-x86_64-uefi-msvc:Msvc|0026",
            .{ .parse = false, .layout = true, .extra = true, .offset = false },
        },
        .{
            "x86_64-x86_64-uefi-msvc:Msvc|0029",
            .{ .parse = false, .layout = true, .extra = true, .offset = false },
        },
        .{
            "x86_64-x86_64-uefi-msvc:Msvc|0042",
            .{ .parse = false, .layout = true, .extra = true, .offset = true },
        },
        .{
            "x86_64-x86_64-uefi-msvc:Msvc|0044",
            .{ .parse = false, .layout = true, .extra = true, .offset = false },
        },
        .{
            "x86_64-x86_64-uefi-msvc:Msvc|0045",
            .{ .parse = false, .layout = false, .extra = true, .offset = false },
        },
        .{
            "x86_64-x86_64-uefi-msvc:Msvc|0046",
            .{ .parse = false, .layout = true, .extra = true, .offset = false },
        },
        .{
            "x86_64-x86_64-uefi-msvc:Msvc|0053",
            .{ .parse = false, .layout = false, .extra = true, .offset = false },
        },
        .{
            "x86_64-x86_64-uefi-msvc:Msvc|0063",
            .{ .parse = false, .layout = true, .extra = true, .offset = false },
        },
        .{
            "x86_64-x86_64-uefi-msvc:Msvc|0065",
            .{ .parse = false, .layout = true, .extra = true, .offset = false },
        },
        .{
            "x86_64-x86_64-uefi-msvc:Msvc|0066",
            .{ .parse = false, .layout = true, .extra = true, .offset = false },
        },
        .{
            "x86_64-x86_64-windows-msvc:Msvc|0011",
            .{ .parse = false, .layout = true, .extra = true, .offset = false },
        },
        .{
            "x86_64-x86_64-windows-msvc:Msvc|0014",
            .{ .parse = false, .layout = true, .extra = true, .offset = false },
        },
        .{
            "x86_64-x86_64-windows-msvc:Msvc|0018",
            .{ .parse = false, .layout = false, .extra = true, .offset = false },
        },
        .{
            "x86_64-x86_64-windows-msvc:Msvc|0023",
            .{ .parse = false, .layout = true, .extra = true, .offset = false },
        },
        .{
            "x86_64-x86_64-windows-msvc:Msvc|0025",
            .{ .parse = false, .layout = true, .extra = true, .offset = false },
        },
        .{
            "x86_64-x86_64-windows-msvc:Msvc|0026",
            .{ .parse = false, .layout = true, .extra = true, .offset = false },
        },
        .{
            "x86_64-x86_64-windows-msvc:Msvc|0029",
            .{ .parse = false, .layout = true, .extra = true, .offset = false },
        },
        .{
            "x86_64-x86_64-windows-msvc:Msvc|0042",
            .{ .parse = false, .layout = true, .extra = true, .offset = true },
        },
        .{
            "x86_64-x86_64-windows-msvc:Msvc|0044",
            .{ .parse = false, .layout = true, .extra = true, .offset = false },
        },
        .{
            "x86_64-x86_64-windows-msvc:Msvc|0045",
            .{ .parse = false, .layout = false, .extra = true, .offset = false },
        },
        .{
            "x86_64-x86_64-windows-msvc:Msvc|0046",
            .{ .parse = false, .layout = true, .extra = true, .offset = false },
        },
        .{
            "x86_64-x86_64-windows-msvc:Msvc|0053",
            .{ .parse = false, .layout = false, .extra = true, .offset = false },
        },
        .{
            "x86_64-x86_64-windows-msvc:Msvc|0063",
            .{ .parse = false, .layout = true, .extra = true, .offset = false },
        },
        .{
            "x86_64-x86_64-windows-msvc:Msvc|0065",
            .{ .parse = false, .layout = true, .extra = true, .offset = false },
        },
        .{
            "x86_64-x86_64-windows-msvc:Msvc|0066",
            .{ .parse = false, .layout = true, .extra = true, .offset = false },
        },
    });
};
