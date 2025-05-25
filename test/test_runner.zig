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

var general_purpose_allocator = std.heap.GeneralPurposeAllocator(.{}){};

/// Returns onlyPreprocess and lineMarkers settings if saw -E
fn addCommandLineArgs(
    comp: *zinc.Compilation,
    file: zinc.Source,
    macroBuffer: anytype,
) !struct { bool, zinc.Preprocessor.LineMarkers, zinc.Compilation.SystemDefinesMode } {
    var onlyPreprocess = false;
    var lineMarkers: zinc.Preprocessor.LineMarkers = .None;
    var systemDefines: zinc.Compilation.SystemDefinesMode = .IncludeSystemDefines;

    comp.langOpts.gnucVersion = 40201;

    if (std.mem.startsWith(u8, file.buffer, "//zinc-args")) {
        var testArgs = std.ArrayList([]const u8).init(comp.gpa);
        defer testArgs.deinit();

        const nl = std.mem.indexOfAny(u8, file.buffer, "\n\r") orelse file.buffer.len;
        var it = std.mem.tokenizeScalar(u8, file.buffer[0..nl], ' ');
        while (it.next()) |some| try testArgs.append(some);

        var driver = zinc.Driver{ .comp = comp };
        defer driver.deinit();

        _ = try driver.parseArgs(std.io.null_writer, macroBuffer, testArgs.items);

        onlyPreprocess = driver.onlyPreprocess;
        systemDefines = driver.systemDefines;

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

    return .{ onlyPreprocess, lineMarkers, systemDefines };
}

fn testOne(allocator: std.mem.Allocator, path: []const u8, testDir: []const u8) !void {
    var comp = zinc.Compilation.init(allocator, std.fs.cwd());
    defer comp.deinit();

    try comp.addDefaultPragmaHandlers();
    try comp.defineSystemIncludes(testDir);

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
    try pp.tokens.append(allocator, eof);

    var tree = try zinc.Parser.parse(&pp);
    defer tree.deinit();
    tree.dump(false, std.io.null_writer) catch {};
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
    const gpa = general_purpose_allocator.allocator();
    defer if (general_purpose_allocator.deinit() == .leak) std.process.exit(1);

    const args = try std.process.argsAlloc(gpa);
    defer std.process.argsFree(gpa, args);

    if (args.len != 3) {
        print("expected test case directory and zig executable as only argument\n", .{});
        return error.InvalidArguments;
    }

    const testDir = args[1];

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
        return testAllAllocationFailures(cases.items, testDir);
    }

    const rootNode = std.Progress.start(.{
        .disable_printing = true,
        .root_name = "Test",
        .estimated_total_items = cases.items.len,
    });

    // prepare compiler
    var initialComp = zinc.Compilation.init(gpa, std.fs.cwd());
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

        var macroBuffer = std.ArrayList(u8).init(comp.gpa);
        defer macroBuffer.deinit();

        const onlyPreprocess, const lineMarkers, const systemDefines = try addCommandLineArgs(&comp, file, macroBuffer.writer());
        const userMacros = try comp.addSourceFromBuffer("<command line>", macroBuffer.items);

        const builtinMacros = try comp.generateBuiltinMacros(systemDefines);

        comp.diagnostics.errors = 0;
        var pp = zinc.Preprocessor.init(&comp);
        defer pp.deinit();

        if (onlyPreprocess) {
            pp.preserveWhitespace = true;
            pp.linemarkers = lineMarkers;
        }
        try pp.addBuiltinMacros();

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
            if (try checkExpectedErrors(&pp, &buffer, case)) |some| {
                if (!some) {
                    failCount += 1;
                    continue;
                }
            } else {
                zinc.Diagnostics.render(&comp, std.io.tty.detectConfig(std.io.getStdErr()));
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

        var tree = zinc.Parser.parse(&pp) catch |err| switch (err) {
            error.FatalError => {
                if (try checkExpectedErrors(&pp, &buffer, case)) |some| {
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
            var actualAST = std.ArrayList(u8).init(gpa);
            defer actualAST.deinit();

            try tree.dump(.no_color, actualAST.writer());
            std.testing.expectEqualStrings(expectedAST, actualAST.items) catch {
                failCount += 1;
                break;
            };
        } else tree.dump(.no_color, std.io.null_writer) catch {};

        if (expectedTypes) |types| {
            const testFn = for (tree.rootDecls.items) |decl| {
                const node = decl.get(&tree);
                if (node == .fnDef) break node.fnDef;
            } else {
                failCount += 1;
                std.debug.print("EXPECTED_TYPES requires a function to be defined\n", .{});
                break;
            };

            var actual = StmtTypeDumper.init(gpa);
            defer actual.deinit(gpa);

            try actual.dump(&tree, testFn.body, gpa);

            var i: usize = 0;
            for (types.tokens) |str| {
                if (str.is(.MacroWS)) continue;
                if (str.isNot(.StringLiteral)) {
                    failCount += 1;
                    std.debug.print("EXPECTED_TYPES tokens must be string literals (found {s})\n", .{@tagName(str.id)});
                    continue :next_test;
                }

                defer i += 1;
                if (i >= actual.types.items.len) continue;

                const expectedType = std.mem.trim(u8, pp.getTokenSlice(str), "\"");
                const actualType = actual.types.items[i];
                if (!std.mem.eql(u8, expectedType, actualType)) {
                    failCount += 1;
                    std.debug.print("expected type '{s}' did not match actual type '{s}'\n", .{
                        expectedType,
                        actualType,
                    });
                    continue :next_test;
                }
            }
            if (i != actual.types.items.len) {
                failCount += 1;
                std.debug.print(
                    "EXPECTED_TYPES count differs: expected {d} found {d}\n",
                    .{ i, actual.types.items.len },
                );
                continue;
            }
        }

        if (try checkExpectedErrors(&pp, &buffer, case)) |some| {
            if (some) passCount += 1 else failCount += 1;
            continue;
        }

        if (pp.defines.contains("NO_ERROR_VALIDATION")) continue;

        zinc.Diagnostics.render(&comp, std.io.tty.detectConfig(std.io.getStdErr()));

        if (pp.defines.get("EXPECTED_OUTPUT")) |macro| blk: {
            if (comp.diagnostics.errors != 0) break :blk;

            if (macro.isFunc) {
                failCount += 1;
                std.debug.print("invalid EXPECTED_OUTPUT {}\n", .{macro});
                continue;
            }

            if (macro.tokens.len != 1 or macro.tokens[0].isNot(.StringLiteral)) {
                failCount += 1;
                std.debug.print("EXPECTED_OUTPUT takes exactly one string", .{});
                continue;
            }

            defer buffer.items.len = 0;
            // realistically the strings will only contain \" if any escapes so we can use Zig's string parsing
            assert((try std.zig.string_literal.parseWrite(buffer.writer(), pp.getTokenSlice(macro.tokens[0]))) == .success);
            const expectedOutput = buffer.items;

            const objName = "testObject.o";
            if (true) break :blk;
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
fn checkExpectedErrors(pp: *zinc.Preprocessor, buf: *std.ArrayList(u8), case: []const u8) !?bool {
    const macro = pp.defines.get("EXPECTED_ERRORS") orelse return null;
    const expectedCount = pp.comp.diagnostics.list.items.len;

    var m = MsgWriter.init(pp.comp.gpa);
    defer m.deinit();

    zinc.Diagnostics.renderMessages(pp.comp, &m);

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
        assert((try std.zig.string_literal.parseWrite(buf.writer(), pp.getTokenSlice(str))) == .success);
        try buf.append('\n');
        const expectedError = buf.items[start..];

        const index = std.mem.indexOf(u8, m.buf.items, expectedError);
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
            , .{ expectedError, m.buf.items });
            return false;
        }
    }

    if (count != expectedCount) {
        std.debug.print(
            \\{s}: EXPECTED_ERRORS missing errors, expected {d} found {d},
            \\
        , .{ case, count, expectedCount });
        var it = std.mem.tokenizeScalar(u8, m.buf.items, '\n');
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

    pub fn start(m: *MsgWriter, kind: zinc.Diagnostics.Kind) void {
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

    fn dumpNode(
        self: *StmtTypeDumper,
        tree: *const Tree,
        node: Node.Index,
        m: *MsgWriter,
    ) AllocatorError!void {
        const maybeRet = node.get(tree);
        if (maybeRet == .returnStmt and maybeRet.returnStmt.operand == .implicit) return;

        node.qt(tree)
            .dump(tree.comp, m.buf.writer()) catch {};

        const owned = try m.buf.toOwnedSlice();
        errdefer m.buf.allocator.free(owned);

        try self.types.append(owned);
    }

    fn dump(
        self: *StmtTypeDumper,
        tree: *const Tree,
        body: Node.Index,
        allocator: std.mem.Allocator,
    ) AllocatorError!void {
        var m = MsgWriter.init(allocator);
        defer m.deinit();

        const compound = body.get(tree).compoundStmt;
        for (compound.body) |stmt| {
            try self.dumpNode(tree, stmt, &m);
        }
    }
};
