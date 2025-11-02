const std = @import("std");
const assert = std.debug.assert;
const buildOptions = @import("build_options");
const print = std.debug.print;
const zinc = @import("zinc");
const CodeGen = zinc.CodeGen;
const Tree = zinc.Tree;
const Token = Tree.Token;
const Node = Tree.Node;
const AllocatorError = std.mem.Allocator.Error;

var DebugAllocator: std.heap.DebugAllocator(.{
    .stack_trace_frames = if (buildOptions.DebugAllocations and std.debug.sys_can_stack_trace) 10 else 0,
    .resize_stack_traces = buildOptions.DebugAllocations,
    // A unique value so that when a default-constructed
    // GeneralPurposeAllocator is incorrectly passed to testing allocator, or
    // vice versa, panic occurs.
    .canary = @truncate(0xc647026dc6875134),
}) = .init();

const AddCommandLineArgsResult = struct {
    bool,
    zinc.Preprocessor.LineMarkers,
    zinc.Compilation.SystemDefinesMode,
    zinc.Preprocessor.DumpMode,
};

/// Returns onlyPreprocess and lineMarkers settings if saw -E
fn addCommandLineArgs(
    comp: *zinc.Compilation,
    file: zinc.Source,
    macroBuffer: *std.ArrayListUnmanaged(u8),
) !AddCommandLineArgsResult {
    var onlyPreprocess = false;
    var lineMarkers: zinc.Preprocessor.LineMarkers = .None;
    var systemDefines: zinc.Compilation.SystemDefinesMode = .IncludeSystemDefines;
    var dumpMode: zinc.Preprocessor.DumpMode = .ResultOnly;

    comp.langOpts.gnucVersion = 40201;

    if (std.mem.startsWith(u8, file.buffer, "//zinc-args")) {
        var testArgs: std.ArrayList([]const u8) = .empty;
        defer testArgs.deinit(comp.gpa);

        const nl = std.mem.indexOfAny(u8, file.buffer, "\n\r") orelse file.buffer.len;
        var it = std.mem.tokenizeScalar(u8, file.buffer[0..nl], ' ');
        while (it.next()) |some| try testArgs.append(comp.gpa, some);

        var driver = zinc.Driver{ .comp = comp, .diagnostics = comp.diagnostics };
        defer driver.deinit();

        var discardBuffer: [256]u8 = undefined;
        var discarding: std.Io.Writer.Discarding = .init(&discardBuffer);
        _ = try driver.parseArgs(&discarding.writer, macroBuffer, testArgs.items);

        onlyPreprocess = driver.onlyPreprocess;
        systemDefines = driver.systemDefines;
        dumpMode = driver.debugDumpLetters.getPreprocessorDumpMode();

        if (onlyPreprocess) {
            if (driver.lineCommands)
                lineMarkers = if (driver.useLineDirectives) .LineDirectives else .NumericDirectives;
        }
    }

    if (std.mem.indexOf(u8, file.buffer, "//zinc-env")) |idx| {
        const buf = file.buffer[idx..];
        const nl = std.mem.indexOfAny(u8, buf, "\n\r") orelse buf.len;

        var it = std.mem.tokenizeScalar(u8, buf[0..nl], ' ');
        while (it.next()) |some| {
            var parts = std.mem.splitScalar(u8, some, '=');
            const name = parts.next().?;
            const val = parts.next() orelse "";
            inline for (@typeInfo(zinc.Compilation.Environment).@"struct".fields) |field| {
                var envVarBuffer: [field.name.len]u8 = undefined;
                const varName = std.ascii.lowerString(&envVarBuffer, field.name);
                if (std.ascii.eqlIgnoreCase(name, varName))
                    @field(comp.environment, field.name) = val;
            }
        }
    }

    return .{ onlyPreprocess, lineMarkers, systemDefines, dumpMode };
}

fn testOne(gpa: std.mem.Allocator, path: []const u8, testDir: []const u8) !void {
    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();

    var comp = zinc.Compilation.init(gpa, arena.allocator(), std.fs.cwd());
    defer comp.deinit();

    try comp.addDefaultPragmaHandlers();
    try comp.addBuiltinIncludeDir(testDir);

    const file = try comp.addSourceFromPath(path);
    var macroBuffer = std.ArrayList(u8).init(comp.gpa);
    defer macroBuffer.deinit();

    _, _, const systemDefines = try addCommandLineArgs(&comp, file, macroBuffer.writer());
    const userMacros = try comp.addSourceFromBuffer("<command line>", macroBuffer.items);

    const bulitinMacros = try comp.generateBuiltinMacros(systemDefines);

    var pp = zinc.Preprocessor.init(&comp);
    defer pp.deinit();

    try pp.addBuiltinMacros();

    if (comp.langOpts.msExtensions)
        comp.msCwdSourceId = file.id;

    _ = try pp.preprocess(bulitinMacros);
    _ = try pp.preprocess(userMacros);

    const eof = pp.preprocess(file);
    try pp.tokens.append(gpa, eof);

    var tree = try zinc.Parser.parse(&pp);
    defer tree.deinit();
    tree.dump(false, std.Io.null_writer) catch {};
}

fn testAllAllocationFailures(cases: [][]const u8, testDir: []const u8) !void {
    var progress = std.Progress{};
    const rootNode = progress.start(.{
        .disable_printing = false,
        .root_name = "Memory Allocation Test",
        .estimated_total_items = cases.len,
    });

    for (cases) |case| {
        const caseName = std.mem.sliceTo(std.fs.path.basename(case), '.');
        var caseNode = rootNode.start(caseName, 0);
        defer caseNode.end();

        try std.testing.checkAllAllocationFailures(std.testing.allocator, testOne, .{ case, testDir }) catch |er| switch (er) {
            error.SwallowedOutOfMemoryError => {},
            else => |e| return e,
        };
    }
    rootNode.end();
}

pub fn main() !void {
    const gpa = DebugAllocator.allocator();
    defer if (DebugAllocator.deinit() == .leak) std.process.exit(1);

    var arenaState: std.heap.ArenaAllocator = .init(gpa);
    defer arenaState.deinit();
    const arena = arenaState.allocator();

    const args = try std.process.argsAlloc(arena);

    if (args.len != 3) {
        print("expected test case directory and zig executable as only argument\n", .{});
        return error.InvalidArguments;
    }

    const testDir = args[1];

    var buffer: std.ArrayListUnmanaged(u8) = .empty;
    var cases: std.ArrayList([]const u8) = .empty;

    defer {
        buffer.deinit(gpa);
        cases.deinit(gpa);
    }

    // collect all cases
    {
        var casesDir = try std.fs.cwd().openDir(args[1], .{ .iterate = true });
        defer casesDir.close();

        var it = casesDir.iterate();
        while (try it.next()) |entry| {
            if (entry.kind == .directory)
                continue;

            if (entry.kind != .file) {
                print("skipping non file entry '{s}'\n", .{entry.name});
                continue;
            }
            try cases.append(gpa, try std.fmt.allocPrint(arena, "{s}{c}{s}", .{ args[1], std.fs.path.sep, entry.name }));
        }
    }

    if (buildOptions.TestAllAllocationFailures) {
        return testAllAllocationFailures(cases.items, testDir);
    }

    const rootNode = std.Progress.start(.{
        .disable_printing = true,
        .root_name = "Test",
        .estimated_total_items = cases.items.len,
    });

    var diagBuffer: std.Io.Writer.Allocating = .init(gpa);
    defer diagBuffer.deinit();

    var diagnostics: zinc.Diagnostics = .{
        .output = .{
            .toWriter = .{ .writer = &diagBuffer.writer, .color = .no_color },
        },
    };
    defer diagnostics.deinit();

    // prepare compiler
    var initialComp = zinc.Compilation.init(gpa, arena, &diagnostics, std.fs.cwd());
    defer initialComp.deinit();

    const casesIncludeDir = try std.fs.path.join(gpa, &.{ args[1], "include" });
    const caseNextIncludeDir = try std.fs.path.join(gpa, &.{ args[1], "include", "next" });
    defer {
        gpa.free(casesIncludeDir);
        gpa.free(caseNextIncludeDir);
    }

    try initialComp.includeDirs.append(gpa, casesIncludeDir);
    try initialComp.includeDirs.append(gpa, caseNextIncludeDir);

    try initialComp.addDefaultPragmaHandlers();
    try initialComp.addBuiltinIncludeDir(testDir);

    // apparently we can't use setAstCwd without libc on windows yet
    const win = @import("builtin").os.tag == .windows;
    var tmpDir = if (!win) std.testing.tmpDir(.{});
    defer if (!win) tmpDir.cleanup();

    if (!win)
        try tmpDir.dir.setAsCwd();

    // iterate over all cases
    var passCount: u32 = 0;
    var failCount: u32 = 0;
    var skipCount: u32 = 0;
    next_test: for (cases.items) |path| {
        diagBuffer.shrinkRetainingCapacity(0);
        diagnostics = .{ .output = .{ .toWriter = .{
            .writer = &diagBuffer.writer,
            .color = .no_color,
        } } };

        var comp = initialComp;
        defer {
            comp.includeDirs = .{};
            comp.systemIncludeDirs = .{};
            comp.pragmaHandlers = .{};
            comp.environment = .{};

            // reset everything else
            comp.deinit();
        }

        const case = std.mem.sliceTo(std.fs.path.basename(path), '.');
        var caseNode = rootNode.start(case, 0);
        defer caseNode.end();

        const file = comp.addSourceFromPath(path) catch |err| {
            failCount += 1;
            std.debug.print("could not add source '{s}': {s}\n", .{ path, @errorName(err) });
            continue;
        };

        var macroBuffer: std.ArrayListUnmanaged(u8) = .empty;
        defer macroBuffer.deinit(comp.gpa);

        const onlyPreprocess, const lineMarkers, const systemDefines, const dumpNode = try addCommandLineArgs(&comp, file, &macroBuffer);

        const userMacros = try comp.addSourceFromBuffer("<command line>", macroBuffer.items);
        const builtinMacros = try comp.generateBuiltinMacros(systemDefines);

        var pp = try zinc.Preprocessor.initDefault(&comp);
        defer pp.deinit();

        if (onlyPreprocess) {
            pp.preserveWhitespace = true;
            pp.linemarkers = lineMarkers;
            if (dumpNode != .ResultOnly) pp.storeMacroTokens = true;
        }

        if (comp.langOpts.msExtensions)
            comp.msCwdSourceId = file.id;

        _ = try pp.preprocess(builtinMacros);
        _ = try pp.preprocess(userMacros);

        const eof = pp.preprocess(file) catch |err| {
            failCount += 1;
            std.debug.print("could not preprocess file '{s}': {s}\n", .{ path, @errorName(err) });
            continue;
        };
        try pp.addToken(eof);

        if (pp.defines.get("TESTS_SKIPPED")) |macro| {
            if (macro.isFunc or macro.tokens.len != 1 or macro.tokens[0].isNot(.PPNumber)) {
                failCount += 1;
                std.debug.print("invalid TESTS_SKIPPED, definition should contain exactly one integer literal {}\n", .{macro});
                continue;
            }
            const tokSlice = pp.getTokenSlice(macro.tokens[0]);
            const testsSkipped = try std.fmt.parseInt(u32, tokSlice, 0);
            std.debug.print("{s}: {d} test{s} skipped\n", .{ case, testsSkipped, if (testsSkipped == 1) "" else "s" });
            skipCount += testsSkipped;
            continue;
        }

        if (onlyPreprocess) {
            if (try checkExpectedErrors(&pp, &buffer, diagBuffer.written(), case)) |some| {
                if (!some) {
                    failCount += 1;
                    continue;
                }
            } else {
                var stderrBuffer: [4096]u8 = undefined;
                var stderr = std.fs.File.stderr().writer(&stderrBuffer);
                try stderr.interface.writeAll(diagBuffer.written());
                try stderr.interface.flush();

                if (comp.diagnostics.errors != 0) {
                    failCount += 1;
                    continue;
                }
            }

            const expectedOutput = blk: {
                const expandedPath = try std.fs.path.join(gpa, &.{ args[1], "expanded", std.fs.path.basename(path) });
                defer gpa.free(expandedPath);

                break :blk std.fs.cwd().readFileAlloc(gpa, expandedPath, std.math.maxInt(u32)) catch |err| {
                    failCount += 1;
                    std.debug.print("could not open expanded file '{s}': {s}\n", .{ path, @errorName(err) });
                    continue;
                };
            };
            defer gpa.free(expectedOutput);

            var output: std.Io.Writer.Allocating = .init(gpa);
            defer output.deinit();

            try pp.prettyPrintTokens(&output.writer, dumpNode);
            if (pp.defines.contains("CHECK_PARTIAL_MATCH")) {
                const index = std.mem.indexOf(u8, output.written(), expectedOutput);
                if (index != null) {
                    passCount += 1;
                } else {
                    failCount += 1;
                    std.debug.print("\n====== expected to find: =========\n", .{});
                    std.debug.print("{s}", .{expectedOutput});
                    std.debug.print("\n======== but did not find it in this: =========\n", .{});
                    std.debug.print("{s}", .{output.written()});
                    std.debug.print("\n======================================\n", .{});
                }
            } else {
                if (std.testing.expectEqualStrings(expectedOutput, output.written()))
                    passCount += 1
                else |_|
                    failCount += 1;
            }
            continue;
        }

        const expectedTypes = pp.defines.get("EXPECTED_TYPES");

        var tree = zinc.Parser.parse(&pp) catch |err| switch (err) {
            error.FatalError => {
                if (try checkExpectedErrors(&pp, &buffer, diagBuffer.written(), case)) |some| {
                    if (some) passCount += 1 else failCount += 1;
                }
                continue;
            },
            else => |e| return e,
        };
        defer tree.deinit();

        const astPath = try std.fs.path.join(gpa, &.{ args[1], "ast", std.fs.path.basename(path) });
        defer gpa.free(astPath);

        const maybeAST = std.fs.cwd().readFileAlloc(gpa, astPath, std.math.maxInt(u32)) catch null;
        if (maybeAST) |expectedAST| {
            defer gpa.free(expectedAST);
            var actualAST: std.Io.Writer.Allocating = .init(gpa);
            defer actualAST.deinit();

            try tree.dump(.no_color, &actualAST.writer);
            std.testing.expectEqualStrings(expectedAST, actualAST.written()) catch {
                failCount += 1;
                break;
            };
        } else {
            var discardBuffer: [256]u8 = undefined;
            var discarding: std.Io.Writer.Discarding = .init(&discardBuffer);
            tree.dump(.no_color, &discarding.writer) catch {};
        }

        if (expectedTypes) |types| {
            const testFn = for (tree.rootDecls.items) |decl| {
                const node = decl.get(&tree);
                if (node == .fnDef) break node.fnDef;
            } else {
                failCount += 1;
                std.debug.print("{s}:\n", .{case});
                std.debug.print("EXPECTED_TYPES requires a function to be defined\n", .{});
                break;
            };

            var actual: StmtTypeDumper = .{};
            defer actual.deinit(gpa);

            try actual.dump(gpa, &tree, testFn.body);

            var i: usize = 0;
            for (types.tokens) |str| {
                if (str.is(.MacroWS)) continue;
                if (str.isNot(.StringLiteral)) {
                    failCount += 1;
                    std.debug.print("{s}:\n", .{case});
                    std.debug.print("EXPECTED_TYPES tokens must be string literals (found {s})\n", .{@tagName(str.id)});
                    continue :next_test;
                }

                defer i += 1;
                if (i >= actual.types.items.len) continue;

                const expectedType = std.mem.trim(u8, pp.getTokenSlice(str), "\"");
                const actualType = actual.types.items[i];
                if (!std.mem.eql(u8, expectedType, actualType)) {
                    failCount += 1;
                    std.debug.print("{s}:\n", .{case});
                    std.debug.print("expected type '{s}' did not match actual type '{s}'\n", .{
                        expectedType,
                        actualType,
                    });
                    continue :next_test;
                }
            }
            if (i != actual.types.items.len) {
                failCount += 1;
                std.debug.print("{s}:\n", .{case});
                std.debug.print(
                    "EXPECTED_TYPES count differs: expected {d} found {d}\n",
                    .{ i, actual.types.items.len },
                );
                continue;
            }
        }

        if (try checkExpectedErrors(&pp, &buffer, diagBuffer.written(), case)) |some| {
            if (some) passCount += 1 else {
                std.debug.print("in case {s}\n", .{case});
                failCount += 1;
            }
            continue;
        }

        if (pp.defines.contains("NO_ERROR_VALIDATION")) continue;

        {
            var stderrBuffer: [4096]u8 = undefined;
            var stderr = std.fs.File.stderr().writer(&stderrBuffer);
            try stderr.interface.writeAll(diagBuffer.written());
            try stderr.interface.flush();
        }

        if (pp.defines.get("EXPECTED_OUTPUT")) |macro| blk: {
            if (comp.diagnostics.errors != 0) break :blk;

            if (macro.isFunc) {
                failCount += 1;
                std.debug.print("{s}:\n", .{case});
                std.debug.print("invalid EXPECTED_OUTPUT {}\n", .{macro});
                continue;
            }

            if (macro.tokens.len != 1 or macro.tokens[0].isNot(.StringLiteral)) {
                failCount += 1;
                std.debug.print("{s}:\n", .{case});
                std.debug.print("EXPECTED_OUTPUT takes exactly one string", .{});
                continue;
            }

            defer buffer.items.len = 0;
            {
                var allocating: std.Io.Writer.Allocating = .fromArrayList(gpa, &buffer);
                defer buffer = allocating.toArrayList();
                assert((try std.zig.string_literal.parseWrite(&allocating.writer, pp.getTokenSlice(macro.tokens[0]))) == .success);
            }
            const expectedOutput = buffer.items;

            const objName = "testObject.o";
            if (true) @panic("no backend available");
            // {
            // const obj = try CodeGen.generateTree(&comp, tree);
            // defer obj.deinit();

            // const outFile = try std.fs.cwd().createFile(objName, .{});
            // defer outFile.close();

            // try obj.finish(outFile);
            // }
            var child = std.process.Child.init(&.{ args[2], "run", "-lc", objName }, gpa);
            child.stdout_behavior = .Pipe;

            try child.spawn();

            const stdout = try child.stdout.?.reader().readAllAlloc(gpa, std.math.maxInt(u16));
            defer gpa.free(stdout);

            switch (try child.wait()) {
                .Exited => |code| if (code != 0) {
                    failCount += 1;
                    continue;
                },
                else => {
                    failCount += 1;
                    continue;
                },
            }

            if (!std.mem.eql(u8, expectedOutput, stdout)) {
                failCount += 1;
                std.debug.print("{s}:\n", .{case});
                std.debug.print(
                    \\
                    \\======= expected output =======
                    \\{s}
                    \\
                    \\=== but output does not contain it ===
                    \\{s}
                    \\
                    \\
                , .{ expectedOutput, stdout });
                break;
            }

            passCount += 1;
            continue;
        }

        if (comp.diagnostics.errors != 0) failCount += 1 else passCount += 1;
    }

    rootNode.end();
    if (passCount == cases.items.len and skipCount == 0) {
        print("All {d} tests passed.\n\n", .{passCount});
    } else if (failCount == 0) {
        print("{d} passed; {d} skipped.\n\n", .{ passCount, skipCount });
    } else {
        print("{d} passed; {d} failed.\n\n", .{ passCount, failCount });
        std.process.exit(1);
    }
}

// returns true if passed
fn checkExpectedErrors(
    pp: *zinc.Preprocessor,
    buf: *std.ArrayListUnmanaged(u8),
    errors: []const u8,
    case: []const u8,
) !?bool {
    const macro = pp.defines.get("EXPECTED_ERRORS") orelse return null;

    const expectedCount = pp.diagnostics.total;

    if (macro.isFunc) {
        std.debug.print("invalid EXPECTED_ERRORS {}\n", .{macro});
        return false;
    }
    buf.items.len = 0;

    var count: usize = 0;
    for (macro.tokens) |str| {
        if (str.is(.MacroWS)) continue;
        if (str.isNot(.StringLiteral)) {
            std.debug.print("EXPECTED_ERRORS tokens must be string literals (found {s})\n", .{@tagName(str.id)});
            return false;
        }
        defer count += 1;
        if (count >= expectedCount) continue;

        const start = buf.items.len;
        // realistically the strings will only contain \" if any escapes so we can use Zig's string parsing
        {
            var allocating: std.Io.Writer.Allocating = .fromArrayList(pp.comp.gpa, buf);
            defer buf.* = allocating.toArrayList();
            std.debug.assert((try std.zig.string_literal.parseWrite(&allocating.writer, pp.getTokenSlice(macro.tokens[0]))) == .success);
        }
        try buf.append(pp.comp.gpa, '\n');
        const expectedError = buf.items[start..];

        const index = std.mem.indexOf(u8, errors, expectedError);
        if (index == null) {
            std.debug.print(
                \\
                \\======= expected to find error =======
                \\{s}
                \\
                \\=== but output does not contain it ===
                \\{s}
                \\
                \\
            , .{ expectedError, errors });
            return false;
        }
    }

    if (count != expectedCount) {
        std.debug.print(
            \\{s}: EXPECTED_ERRORS missing errors, expected {d} found {d},
            \\
        , .{ case, count, expectedCount });
        var it = std.mem.tokenizeScalar(u8, errors, '\n');
        while (it.next()) |msg| {
            const start = std.mem.indexOf(u8, msg, ".c:") orelse continue;
            const index = std.mem.indexOf(u8, buf.items, msg[start..]);
            if (index == null) {
                std.debug.print(
                    \\
                    \\========= new error ==========
                    \\{s}
                    \\
                    \\=== not in EXPECTED_ERRORS ===
                    \\
                    \\
                , .{msg});
            }
        }
        return false;
    }
    return true;
}

const StmtTypeDumper = struct {
    types: std.ArrayList([]const u8) = .empty,

    fn deinit(self: *StmtTypeDumper, allocator: std.mem.Allocator) void {
        for (self.types.items) |t| {
            allocator.free(t);
        }
        self.types.deinit(allocator);
    }

    fn dumpNode(
        self: *StmtTypeDumper,
        gpa: std.mem.Allocator,
        tree: *const Tree,
        node: Node.Index,
    ) AllocatorError!void {
        const maybeRet = node.get(tree);
        if (maybeRet == .returnStmt and maybeRet.returnStmt.operand == .implicit) return;

        var allocating: std.Io.Writer.Allocating = .init(gpa);
        defer allocating.deinit();

        node.qt(tree)
            .dump(tree.comp, &allocating.writer) catch {};
        const owned = try allocating.toOwnedSlice();
        errdefer allocating.allocator.free(owned);

        try self.types.append(gpa, owned);
    }

    fn dump(
        self: *StmtTypeDumper,
        gpa: std.mem.Allocator,
        tree: *const Tree,
        body: Node.Index,
    ) AllocatorError!void {
        const compound = body.get(tree).compoundStmt;
        for (compound.body) |stmt| {
            try self.dumpNode(gpa, tree, stmt);
        }
    }
};
