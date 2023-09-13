const std = @import("std");
const print = std.debug.print;
const zcc = @import("zcc");

const predefined_macros =
    \\#define EXPECT(x) _Static_assert(x, "unexpected result")
;

var general_purpose_allocator = std.heap.GeneralPurposeAllocator(.{}){};

pub fn main() !void {
    const gpa = general_purpose_allocator.allocator();
    defer _ = general_purpose_allocator.deinit();

    var args = try std.process.argsAlloc(gpa);
    defer std.process.argsFree(gpa, args);

    if (args.len != 2) {
        print("expected test case directory as only argument\n", .{});
        return error.InvalidArguments;
    }

    var pathBuffer = std.ArrayList(u8).init(gpa);
    var cases = std.ArrayList(struct { start: usize, end: usize }).init(gpa);

    defer {
        pathBuffer.deinit();
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

            const start = pathBuffer.items.len;
            try pathBuffer.writer().print("{s}{c}{s}", .{ args[1], std.fs.path.sep, entry.name });

            try cases.append(.{ .start = start, .end = pathBuffer.items.len });
        }
    }

    var progress = std.Progress{};
    const rootNode = progress.start("Test", cases.items.len);

    // prepare compiler
    var comp = zcc.Compilation.init(gpa);
    defer comp.deinit();

    const builtinMacros = try comp.generateBuiltinMacros();
    const testRunnerMacros = blk: {
        const dupedPath = try gpa.dupe(u8, "<test_runner>");
        errdefer comp.gpa.free(dupedPath);

        const contents = try gpa.dupe(u8, predefined_macros);
        errdefer comp.gpa.free(contents);

        const source = zcc.Source{
            .id = @as(zcc.Source.ID, @enumFromInt(@as(u32, @intCast(comp.sources.count() + 2)))),
            .path = dupedPath,
            .buffer = contents,
        };
        try comp.sources.put(dupedPath, source);
        break :blk source;
    };

    // iterate over all cases
    var passCount: u32 = 0;
    var failCount: u32 = 0;
    var skipCount: u32 = 0;
    for (cases.items) |range| {
        const path = pathBuffer.items[range.start..range.end];
        const file = comp.addSource(path) catch |err| {
            failCount += 1;
            progress.log("could not add source '{s}': {s}\n", .{ path, @errorName(err) });
            continue;
        };

        defer {
            _ = comp.sources.swapRemove(path);
            gpa.free(file.path);
            gpa.free(file.buffer);
        }

        const case = std.mem.sliceTo(std.fs.path.basename(path), '.');
        var caseNode = rootNode.start(case, 0);
        caseNode.activate();
        defer caseNode.end();
        progress.refresh();

        comp.diag.errors = 0;
        var pp = zcc.Preprocessor.init(&comp);
        defer pp.deinit();

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
            if (macro != .simple or macro.simple.tokens.len != 1 or macro.simple.tokens[0].id != .IntegerLiteral) {
                failCount += 1;
                progress.log("invalid TESTS_SKIPPED, definition should contain exactly one integer literal {}\n", .{macro});
                continue;
            }
            const tokSlice = pp.tokSliceSafe(macro.simple.tokens[0]);
            const testsSkipped = try std.fmt.parseInt(u32, tokSlice, 0);
            progress.log("{d} test{s} skipped\n", .{ testsSkipped, if (testsSkipped == 1) @as([]const u8, "") else "s" });
            skipCount += testsSkipped;
            continue;
        }

        if (pp.defines.get("EXPECTED_TOKENS")) |macro| {
            comp.renderErrors();

            const expectedTokens = switch (macro) {
                .simple => |simple| simple.tokens,
                .empty => &[_]zcc.Token{},
                else => {
                    failCount += 1;
                    progress.log("invalid EXPECTED_TOKENS {}\n", .{macro});
                    continue;
                },
            };

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

        var tree = try zcc.Parser.parse(&pp);
        defer tree.deinit();

        if (pp.defines.get("EXPECTED_ERRORS")) |macro| {
            const expectedCount = comp.diag.list.items.len;
            var m = MsgWriter.init(gpa);
            defer m.deinit();

            zcc.Diagnostics.renderExtra(&comp, &m);

            if (macro != .simple) {
                failCount += 1;
                progress.log("invalid EXPECTED_ERRORS {}\n", .{macro});
                continue;
            }

            if (macro.simple.tokens.len != expectedCount) {
                failCount += 1;
                progress.log(
                    \\EXPECTED_ERRORS missing errors, expected {d} found {d},
                    \\=== actual output ===
                    \\{s}
                    \\
                    \\
                ,
                    .{ macro.simple.tokens.len, expectedCount, m.buf.items },
                );
                continue;
            }

            for (macro.simple.tokens) |str| {
                if (str.id != .StringLiteral) {
                    failCount += 1;
                    progress.log("EXPECTED_ERRORS tokens must be string literals (found {s})\n", .{@tagName(str.id)});
                    break;
                }

                const expectedError = std.mem.trim(u8, pp.tokSliceSafe(str), "\"");
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
            }
            continue;
        }

        comp.renderErrors();
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
