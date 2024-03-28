const std = @import("std");
const buildOptions = @import("build_options");
const print = std.debug.print;
const zcc = @import("zcc");
const CodeGen = zcc.CodeGen;
const Tree = zcc.Tree;
const Token = Tree.Token;
const NodeIndex = Tree.NodeIndex;
const AllocatorError = std.mem.Allocator.Error;

var general_purpose_allocator = std.heap.GeneralPurposeAllocator(.{}){};

fn addCommandLineArgs(comp: *zcc.Compilation, file: zcc.Source, macroBuffer: anytype) !void {
    if (std.mem.startsWith(u8, file.buffer, "//zcc-args")) {
        var testArgs = std.ArrayList([]const u8).init(comp.gpa);
        defer testArgs.deinit();

        const nl = std.mem.indexOfAny(u8, file.buffer, "\n\r") orelse file.buffer.len;
        var it = std.mem.tokenize(u8, file.buffer[0..nl], " ");
        while (it.next()) |some| try testArgs.append(some);

        var sourceFiles = std.ArrayList(zcc.Source).init(std.testing.failing_allocator);
        _ = try zcc.parseArgs(comp, std.io.null_writer, &sourceFiles, macroBuffer, testArgs.items);
    }
}

fn testOne(allocator: std.mem.Allocator, path: []const u8) !void {
    var comp = zcc.Compilation.init(allocator);
    defer comp.deinit();

    try comp.addDefaultPragmaHandlers();
    try comp.defineSystemIncludes();

    const file = try comp.addSourceFromPath(path);
    var macroBuffer = std.ArrayList(u8).init(comp.gpa);
    defer macroBuffer.deinit();

    try addCommandLineArgs(&comp, file, macroBuffer.writer());
    const userMacros = try comp.addSourceFromBuffer("<command line>", macroBuffer.items);

    const bulitinMacros = try comp.generateBuiltinMacros();

    var pp = zcc.Preprocessor.init(&comp);
    defer pp.deinit();

    try pp.addBuiltinMacros();

    _ = try pp.preprocess(bulitinMacros);
    _ = try pp.preprocess(userMacros);

    const eof = pp.preprocess(file) catch |err| {
        if (!std.unicode.utf8ValidateSlice(file.buf)) {
            if (comp.diag.list.items.len > 0 and comp.diag.list.items[comp.diag.list.items.len - 1].tag == .invalid_utf8) {
                return;
            }
        }
        return err;
    };
    try pp.tokens.append(allocator, eof);

    var tree = try zcc.Parser.parse(&pp);
    defer tree.deinit();
    tree.dump(std.io.null_writer) catch {};
}

fn testAllAllocationFailures(cases: [][]const u8) !void {
    var progress = std.Progress{};
    const rootNode = progress.start("Memory Allocation Test", cases.len);

    for (cases) |case| {
        const caseName = std.mem.sliceTo(std.fs.path.basename(case), '.');
        var caseNode = rootNode.start(caseName, 0);
        caseNode.activate();
        defer caseNode.end();
        progress.refresh();

        try std.testing.checkAllAllocationFailures(std.testing.allocator, testOne, .{case});
    }
    rootNode.end();
}

pub fn main() !void {
    const gpa = general_purpose_allocator.allocator();
    defer if (general_purpose_allocator.deinit() == .leak) std.process.exit(1);

    const args = try std.process.argsAlloc(gpa);
    defer std.process.argsFree(gpa, args);

    if (args.len != 3) {
        print("expected test case directory  and zig executable as only argument\n", .{});
        return error.InvalidArguments;
    }

    var buffer = std.ArrayList(u8).init(gpa);
    var cases = std.ArrayList([]const u8).init(gpa);

    defer {
        for (cases.items) |path|
            gpa.free(path);
        buffer.deinit();
        cases.deinit();
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

            defer buffer.items.len = 0;
            try buffer.writer().print("{s}{c}{s}", .{ args[1], std.fs.path.sep, entry.name });

            try cases.append(try gpa.dupe(u8, buffer.items));
        }
    }

    if (buildOptions.TestAllAllocationFailures) {
        return testAllAllocationFailures(cases.items);
    }

    var progress = std.Progress{};
    const rootNode = progress.start("Test", cases.items.len);

    // prepare compiler
    var initialComp = zcc.Compilation.init(gpa);
    defer initialComp.deinit();

    try initialComp.addDefaultPragmaHandlers();
    try initialComp.defineSystemIncludes();

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
        var comp = initialComp;
        defer {
            comp.systemIncludeDirs = @TypeOf(comp.systemIncludeDirs).init(gpa);
            comp.pragmaHandlers = .{};
            comp.builtinHeaderPath = null;

            // reset everything else
            comp.deinit();
        }

        const case = std.mem.sliceTo(std.fs.path.basename(path), '.');
        var caseNode = rootNode.start(case, 0);
        caseNode.activate();
        defer caseNode.end();
        progress.refresh();

        const file = comp.addSourceFromPath(path) catch |err| {
            failCount += 1;
            progress.log("could not add source '{s}': {s}\n", .{ path, @errorName(err) });
            continue;
        };

        var macroBuffer = std.ArrayList(u8).init(comp.gpa);
        defer macroBuffer.deinit();

        try addCommandLineArgs(&comp, file);
        const userMacros = try comp.addSourceFromBuffer("<command line>", macroBuffer.items);

        const builtinMacros = try comp.generateBuiltinMacros();

        comp.diag.errors = 0;
        var pp = zcc.Preprocessor.init(&comp);
        defer pp.deinit();

        try pp.addBuiltinMacros();

        _ = try pp.preprocess(builtinMacros);
        _ = try pp.preprocess(userMacros);

        const eof = pp.preprocess(file) catch |err| {
            failCount += 1;
            progress.log("could not preprocess file '{s}': {s}\n", .{ path, @errorName(err) });
            continue;
        };
        try pp.tokens.append(gpa, eof);

        if (pp.defines.get("TESTS_SKIPPED")) |macro| {
            if (macro.isFunc or macro.tokens.len != 1 or macro.tokens[0].id != .IntegerLiteral) {
                failCount += 1;
                progress.log("invalid TESTS_SKIPPED, definition should contain exactly one integer literal {}\n", .{macro});
                continue;
            }
            const tokSlice = pp.getTokenSlice(macro.tokens[0]);
            const testsSkipped = try std.fmt.parseInt(u32, tokSlice, 0);
            progress.log("{d} test{s} skipped\n", .{ testsSkipped, if (testsSkipped == 1) "" else "s" });
            skipCount += testsSkipped;
            continue;
        }

        if (comp.onlyPreprocess) {
            if (try checkExpectedErrors(&pp, &progress, &buffer)) |some| {
                if (!some) {
                    failCount += 1;
                    continue;
                }
            } else {
                comp.renderErrors();
                if (comp.diag.errors != 0) {
                    failCount += 1;
                    continue;
                }
            }

            const expectedOutput = blk: {
                const expandedPath = try std.fs.path.join(gpa, &.{ args[1], "expanded", std.fs.path.basename(path) });
                defer gpa.free(expandedPath);

                break :blk try std.fs.cwd().readFileAlloc(gpa, expandedPath, std.math.maxInt(u32));
            };
            defer gpa.free(expectedOutput);

            var output = std.ArrayList(u8).init(gpa);
            defer output.deinit();

            try pp.prettyPrintTokens(output.writer());

            if (std.testing.expectEqualStrings(expectedOutput, output.items))
                passCount += 1
            else |_|
                failCount += 1;
            continue;
        }

        const expectedTypes = pp.defines.get("EXPECTED_TYPES");

        var tree = zcc.Parser.parse(&pp) catch |err| switch (err) {
            error.FatalError => {
                if (try checkExpectedErrors(&pp, &progress, &buffer)) |some| {
                    if (some) passCount += 1 else failCount += 1;
                }
                continue;
            },
            else => |e| return e,
        };
        defer tree.deinit();

        tree.dump(std.io.null_writer) catch {};

        if (expectedTypes) |types| {
            const testFn = for (tree.rootDecls) |decl| {
                if (tree.nodes.items(.tag)[@intFromEnum(decl)] == .FnDef) break tree.nodes.items(.data)[@intFromEnum(decl)];
            } else {
                failCount += 1;
                progress.log("EXPECTED_TYPES requires a function to be defined\n", .{});
                break;
            };

            var actual = StmtTypeDumper.init(gpa);
            defer actual.deinit(gpa);

            try actual.dump(&tree, testFn.decl.node, gpa);

            var i: usize = 0;
            for (types.tokens) |str| {
                if (str.id == .MacroWS) continue;
                if (str.id != .StringLiteral) {
                    failCount += 1;
                    progress.log("EXPECTED_TYPES tokens must be string literals (found {s})\n", .{@tagName(str.id)});
                    continue :next_test;
                }

                defer i += 1;
                if (i >= actual.types.items.len) continue;

                const expectedType = std.mem.trim(u8, pp.getTokenSlice(str), "\"");
                const actualType = actual.types.items[i];
                if (!std.mem.eql(u8, expectedType, actualType)) {
                    failCount += 1;
                    progress.log("expected type '{s}' did not match actual type '{s}'\n", .{
                        expectedType,
                        actualType,
                    });
                    continue :next_test;
                }
            }
            if (i != actual.types.items.len) {
                failCount += 1;
                progress.log(
                    "EXPECTED_TYPES count differs: expected {d} found {d}\n",
                    .{ i, actual.types.items.len },
                );
                continue;
            }
        }

        if (try checkExpectedErrors(&pp, &progress, &buffer)) |some| {
            if (some) passCount += 1 else failCount += 1;
            progress.log("passed\n", .{});
            continue;
        }

        comp.renderErrors();

        if (pp.defines.get("EXPECTED_OUTPUT")) |macro| blk: {
            if (comp.diag.errors != 0) break :blk;

            if (macro.isFunc) {
                failCount += 1;
                progress.log("invalid EXPECTED_OUTPUT {}\n", .{macro});
                continue;
            }

            if (macro.tokens.len != 1 or macro.tokens[0].id != .StringLiteral) {
                failCount += 1;
                progress.log("EXPECTED_OUTPUT takes exactly one string", .{});
                continue;
            }

            defer buffer.items.len = 0;
            // realistically the strings will only contain \" if any escapes so we can use Zig's string parsing
            std.debug.assert((try std.zig.string_literal.parseWrite(buffer.writer(), pp.getTokenSlice(macro.tokens[0]))) == .success);
            const expectedOutput = buffer.items;

            const objName = "testObject.o";
            {
                const obj = try CodeGen.generateTree(&comp, tree);
                defer obj.deinit();

                const outFile = try std.fs.cwd().createFile(objName, .{});
                defer outFile.close();

                try obj.finish(outFile);
            }
            var child = std.ChildProcess.init(&.{ args[2], "run", "-lc", objName }, gpa);
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
                progress.log(
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

        if (comp.diag.errors != 0) failCount += 1 else passCount += 1;
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
fn checkExpectedErrors(pp: *zcc.Preprocessor, progress: *std.Progress, buf: *std.ArrayList(u8)) !?bool {
    const macro = pp.defines.get("EXPECTED_ERRORS") orelse return null;

    const expectedCount = pp.comp.diag.list.items.len;
    var m = MsgWriter.init(pp.comp.gpa);
    defer m.deinit();
    zcc.Diagnostics.renderExtra(pp.comp, &m);

    if (macro.isFunc) {
        progress.log("invalid EXPECTED_ERRORS {}\n", .{macro});
        return false;
    }

    var count: usize = 0;
    for (macro.tokens) |str| {
        if (str.id == .MacroWS) continue;
        if (str.id != .StringLiteral) {
            progress.log("EXPECTED_ERRORS tokens must be string literals (found {s})\n", .{@tagName(str.id)});
            return false;
        }
        defer count += 1;
        if (count >= expectedCount) continue;

        defer buf.items.len = 0;
        // realistically the strings will only contain \" if any escapes so we can use Zig's string parsing
        std.debug.assert((try std.zig.string_literal.parseWrite(buf.writer(), pp.getTokenSlice(str))) == .success);
        try buf.append('\n');
        const expectedError = buf.items;

        const index = std.mem.indexOf(u8, m.buf.items, expectedError);
        if (index == null) {
            progress.log(
                \\
                \\======= expected to find error =======
                \\{s}
                \\
                \\=== but output does not contain it ===
                \\{s}
                \\
                \\
            , .{ expectedError, m.buf.items });
            return false;
        }
    }

    if (count != expectedCount) {
        progress.log(
            \\EXPECTED_ERRORS missing errors, expected {d} found {d},
            \\=== actual output ===
            \\{s}
            \\
            \\
        , .{ count, expectedCount, m.buf.items });
        return false;
    }
    return true;
}

const MsgWriter = struct {
    buf: std.ArrayList(u8),

    fn init(gpa: std.mem.Allocator) MsgWriter {
        return .{
            .buf = std.ArrayList(u8).init(gpa),
        };
    }

    fn deinit(m: *MsgWriter) void {
        m.buf.deinit();
    }

    pub fn print(m: *MsgWriter, comptime fmt: []const u8, args: anytype) void {
        m.buf.writer().print(fmt, args) catch {};
    }

    pub fn write(m: *MsgWriter, msg: []const u8) void {
        m.buf.writer().writeAll(msg) catch {};
    }

    pub fn location(m: *MsgWriter, path: []const u8, line: u32, col: u32) void {
        m.print("{s}:{d}:{d}: ", .{ path, line, col });
    }

    pub fn start(m: *MsgWriter, kind: zcc.Diagnostics.Kind) void {
        m.print("{s}: ", .{@tagName(kind)});
    }

    pub fn end(m: *MsgWriter, maybeLine: ?[]const u8, col: u32, endWithSplice: bool) void {
        const line = maybeLine orelse {
            m.write("\n");
            return;
        };
        const trailer = if (endWithSplice) "\\ " else "";
        m.print("\n{s}{s}\n", .{ line, trailer });
        m.print("{s: >[1]}^\n", .{ "", col });
    }
};

const StmtTypeDumper = struct {
    types: std.ArrayList([]const u8),

    fn deinit(self: *StmtTypeDumper, allocator: std.mem.Allocator) void {
        for (self.types.items) |t| {
            allocator.free(t);
        }
        self.types.deinit();
    }

    fn init(allocator: std.mem.Allocator) StmtTypeDumper {
        return .{
            .types = std.ArrayList([]const u8).init(allocator),
        };
    }

    fn dumpNode(self: *StmtTypeDumper, tree: *const Tree, node: NodeIndex, m: *MsgWriter) AllocatorError!void {
        if (node == .none)
            return;
        const tag = tree.nodes.items(.tag)[@intFromEnum(node)];
        if (tag == .ImplicitReturn)
            return;

        const ty = tree.nodes.items(.type)[@intFromEnum(node)];
        ty.dump(m.buf.writer()) catch {};

        const owned = try m.buf.toOwnedSlice();
        errdefer m.buf.allocator.free(owned);

        try self.types.append(owned);
    }

    fn dump(self: *StmtTypeDumper, tree: *const Tree, declIdx: NodeIndex, allocator: std.mem.Allocator) AllocatorError!void {
        var m = MsgWriter.init(allocator);
        defer m.deinit();

        const idx = @intFromEnum(declIdx);

        const tag = tree.nodes.items(.tag)[idx];
        const data = tree.nodes.items(.data)[idx];

        switch (tag) {
            .CompoundStmtTwo => {
                try self.dumpNode(tree, data.binExpr.lhs, &m);
                try self.dumpNode(tree, data.binExpr.rhs, &m);
            },

            .CompoundStmt => {
                for (tree.data[data.range.start..data.range.end]) |stmt| {
                    try self.dumpNode(tree, stmt, &m);
                }
            },

            else => unreachable,
        }
    }
};
