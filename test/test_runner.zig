const std = @import("std");
const print = std.debug.print;
const zcc = @import("zcc");
const CodeGen = zcc.CodeGen;
const Tree = zcc.Tree;
const Token = Tree.Token;
const NodeIndex = Tree.NodeIndex;
const AllocatorError = std.mem.Allocator.Error;

const predefined_macros =
    \\#define EXPECT(x) _Static_assert(x, "unexpected result")
;

var general_purpose_allocator = std.heap.GeneralPurposeAllocator(.{}){};

pub fn main() !void {
    const gpa = general_purpose_allocator.allocator();
    defer _ = general_purpose_allocator.deinit();

    var args = try std.process.argsAlloc(gpa);
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
        var casesDir = try std.fs.cwd().openIterableDir(args[1], .{});
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

    var progress = std.Progress{};
    const rootNode = progress.start("Test", cases.items.len);

    // prepare compiler
    var comp = zcc.Compilation.init(gpa);
    defer comp.deinit();

    try comp.addDefaultPragmaHandlers();
    try comp.defineSystemIncludes();

    const testRunnerMacros = blk: {
        const dupedPath = try gpa.dupe(u8, "<test_runner>");
        errdefer comp.gpa.free(dupedPath);

        const contents = try gpa.dupe(u8, predefined_macros);
        errdefer comp.gpa.free(contents);

        const source = zcc.Source{
            .id = @as(zcc.Source.ID, @enumFromInt(comp.sources.count() + 2)),
            .path = dupedPath,
            .buffer = contents,
        };
        try comp.sources.put(dupedPath, source);
        break :blk source;
    };

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

    const initialOptions = comp.diag.options;
    for (cases.items) |path| {
        comp.langOpts.standard = .default;
        comp.diag.options = initialOptions;
        const file = comp.addSource(path) catch |err| {
            failCount += 1;
            progress.log("could not add source '{s}': {s}\n", .{ path, @errorName(err) });
            continue;
        };

        defer {
            _ = comp.sources.swapRemove(file.path);
            gpa.free(file.path);
            gpa.free(file.buffer);
        }

        if (std.mem.startsWith(u8, file.buffer, "//std=")) {
            const suffix = file.buffer["//std=".len..];
            var it = std.mem.tokenize(u8, suffix, " \r\n");
            if (it.next()) |standard| {
                try comp.langOpts.setStandard(standard);
            }
        }

        const builtinMacros = try comp.generateBuiltinMacros();
        defer {
            _ = comp.sources.swapRemove(builtinMacros.path);
            gpa.free(builtinMacros.path);
            gpa.free(builtinMacros.buffer);
        }

        const case = std.mem.sliceTo(std.fs.path.basename(path), '.');
        var caseNode = rootNode.start(case, 0);
        caseNode.activate();
        defer caseNode.end();
        progress.refresh();

        comp.diag.errors = 0;
        var pp = zcc.Preprocessor.init(&comp);
        defer pp.deinit();

        try pp.addBuiltinMacros();

        try pp.preprocess(builtinMacros);
        try pp.preprocess(testRunnerMacros);
        pp.preprocess(file) catch |err| {
            failCount += 1;
            progress.log("could not preprocess file '{s}': {s}\n", .{ path, @errorName(err) });
            continue;
        };
        try pp.tokens.append(pp.compilation.gpa, .{
            .id = .Eof,
            .loc = .{ .id = file.id, .byteOffset = @as(u32, @intCast(file.buffer.len)) },
        });

        if (pp.defines.get("TESTS_SKIPPED")) |macro| {
            if (macro.isFunc or macro.tokens.len != 1 or macro.tokens[0].id != .IntegerLiteral) {
                failCount += 1;
                progress.log("invalid TESTS_SKIPPED, definition should contain exactly one integer literal {}\n", .{macro});
                continue;
            }
            const tokSlice = pp.tokSliceSafe(macro.tokens[0]);
            const testsSkipped = try std.fmt.parseInt(u32, tokSlice, 0);
            progress.log("{d} test{s} skipped\n", .{ testsSkipped, if (testsSkipped == 1) @as([]const u8, "") else "s" });
            skipCount += testsSkipped;
            continue;
        }

        if (pp.defines.get("EXPECTED_TOKENS")) |macro| {
            comp.renderErrors();

            if (macro.isFunc) {
                failCount += 1;
                progress.log("invalid EXPECTED_TOKENS {}\n", .{macro});
                continue;
            }

            const expectedTokens = macro.tokens;
            if (pp.tokens.len - 1 != expectedTokens.len) {
                failCount += 1;
                print(
                    "EXPECTED_TOKENS count differs: expected {d} found {d}\n",
                    .{ expectedTokens.len, pp.tokens.len - 1 },
                );
                continue;
            }

            var i: usize = 0;
            while (true) : (i += 1) {
                const tok = pp.tokens.get(i);
                if (tok.id == .Eof) {
                    if (comp.diag.errors != 0)
                        failCount += 1
                    else
                        passCount += 1;

                    break;
                }

                const expected = pp.tokSliceSafe(expectedTokens[i]);
                const actual = pp.expandedSlice(tok);
                if (!std.mem.eql(u8, expected, actual)) {
                    failCount += 1;
                    progress.log(
                        "unexpected token found: expected '{s}' found '{s}'\n",
                        .{ expected, actual },
                    );
                    break;
                }
            }
            continue;
        }

        const expectedTypes = pp.defines.get("EXPECTED_TYPES");

        var tree = try zcc.Parser.parse(&pp);
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

            try actual.dump(&tree, testFn.Declaration.node, gpa);

            if (types.tokens.len != actual.types.items.len) {
                failCount += 1;
                progress.log("EXPECTED_TYPES count of {d} does not match function statement length of {d}\n", .{
                    types.tokens.len,
                    actual.types.items.len,
                });
                break;
            }
            for (types.tokens, 0..) |str, i| {
                if (str.id != .StringLiteral) {
                    failCount += 1;
                    progress.log("EXPECTED_TYPES tokens must be string literals (found {s})\n", .{@tagName(str.id)});
                    break;
                }
                const expectedType = std.mem.trim(u8, pp.tokSliceSafe(str), "\"");
                const actualType = actual.types.items[i];
                if (!std.mem.eql(u8, expectedType, actualType)) {
                    failCount += 1;
                    progress.log("expected type '{s}' did not match actual type '{s}'\n", .{
                        expectedType,
                        actualType,
                    });
                    break;
                }
            }
        }

        if (pp.defines.get("EXPECTED_ERRORS")) |macro| {
            const expectedCount = comp.diag.list.items.len;
            var m = MsgWriter.init(gpa);
            defer m.deinit();

            zcc.Diagnostics.renderExtra(&comp, &m);

            if (macro.isFunc) {
                failCount += 1;
                progress.log("invalid EXPECTED_ERRORS {}\n", .{macro});
                continue;
            }

            if (macro.tokens.len != expectedCount) {
                failCount += 1;
                progress.log(
                    \\EXPECTED_ERRORS missing errors, expected {d} found {d},
                    \\=== actual output ===
                    \\{s}
                    \\
                    \\
                ,
                    .{ macro.tokens.len, expectedCount, m.buf.items },
                );
                continue;
            }

            for (macro.tokens) |str| {
                if (str.id != .StringLiteral) {
                    failCount += 1;
                    progress.log("EXPECTED_ERRORS tokens must be string literals (found {s})\n", .{@tagName(str.id)});
                    break;
                }

                defer buffer.items.len = 0;

                std.debug.assert((try std.zig.string_literal.parseWrite(buffer.writer(), pp.tokSliceSafe(str))) == .success);

                const expectedError = buffer.items;
                const index = std.mem.indexOf(u8, m.buf.items, expectedError);
                if (index == null or m.buf.items[index.? + expectedError.len] != '\n') {
                    failCount += 1;
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
                    break;
                }
            } else {
                passCount += 1;
                progress.log("passed\n", .{});
            }
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
            std.debug.assert((try std.zig.string_literal.parseWrite(buffer.writer(), pp.tokSliceSafe(macro.tokens[0]))) == .success);
            const expectedOutput = buffer.items;

            const objName = "testObject.o";
            {
                const obj = try CodeGen.generateTree(&comp, tree);
                defer obj.deinit();

                const outFile = try std.fs.cwd().createFile(objName, .{});
                defer outFile.close();

                try obj.finish(outFile);
            }
            var child = std.ChildProcess.init(&.{ args[2], "run", "-lc", objName }, comp.gpa);
            child.stdout_behavior = .Pipe;

            try child.spawn();

            const stdout = try child.stdout.?.reader().readAllAlloc(comp.gpa, std.math.maxInt(u16));
            defer comp.gpa.free(stdout);

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

    pub fn location(m: *MsgWriter, path: []const u8, lcs: zcc.Source.LCS) void {
        m.print("{s}:{d}:{d}: ", .{ path, lcs.line, lcs.col });
    }

    pub fn start(m: *MsgWriter, kind: zcc.Diagnostics.Kind) void {
        m.print("{s}: ", .{@tagName(kind)});
    }

    pub fn end(m: *MsgWriter, lcs: ?zcc.Source.LCS) void {
        if (lcs == null) {
            m.write("\n");
            return;
        }
        m.print("\n{s}\n", .{lcs.?.str});
        m.print("{s: >[1]}^\n", .{ "", lcs.?.col - 1 });
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
        try self.types.append(try m.buf.toOwnedSlice());
    }

    fn dump(self: *StmtTypeDumper, tree: *const Tree, declIdx: NodeIndex, allocator: std.mem.Allocator) AllocatorError!void {
        var m = MsgWriter.init(allocator);
        defer m.deinit();

        const idx = @intFromEnum(declIdx);

        const tag = tree.nodes.items(.tag)[idx];
        const data = tree.nodes.items(.data)[idx];

        switch (tag) {
            .CompoundStmtTwo => {
                try self.dumpNode(tree, data.BinaryExpr.lhs, &m);
                try self.dumpNode(tree, data.BinaryExpr.rhs, &m);
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
