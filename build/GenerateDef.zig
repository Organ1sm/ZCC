const std = @import("std");
const Step = std.Build.Step;
const Allocator = std.mem.Allocator;
const GeneratedFile = std.Build.GeneratedFile;

const GenerateDef = @This();

step: Step,
path: []const u8,
name: []const u8,
kind: Options.Kind,
needs_large_dafsa_node: bool,
generated_file: GeneratedFile,

pub const base_id: Step.Id = .custom;

pub const Options = struct {
    name: []const u8,
    src_prefix: []const u8 = "src/zinc",
    kind: Kind = .dafsa,
    needs_large_dafsa_node: bool = false,

    pub const Kind = enum { dafsa, named };
};

pub fn create(owner: *std.Build, options: Options) std.Build.Module.Import {
    const self = owner.allocator.create(GenerateDef) catch @panic("OOM");
    const path = owner.pathJoin(&.{ options.src_prefix, options.name });

    const name = owner.fmt("GenerateDef {s}", .{options.name});
    self.* = .{
        .step = Step.init(.{
            .id = base_id,
            .name = name,
            .owner = owner,
            .makeFn = make,
        }),
        .path = path,
        .name = options.name,
        .kind = options.kind,
        .needs_large_dafsa_node = options.needs_large_dafsa_node,
        .generated_file = .{ .step = &self.step },
    };

    const module = self.step.owner.createModule(.{
        .root_source_file = .{ .generated = .{ .file = &self.generated_file } },
    });

    return .{
        .module = module,
        .name = self.name,
    };
}

fn make(step: *Step, options: std.Build.Step.MakeOptions) !void {
    _ = options;
    const b = step.owner;
    const self: *GenerateDef = @fieldParentPtr("step", step);
    const arena = b.allocator;

    var man = b.graph.cache.obtain();
    defer man.deinit();

    // Random bytes to make GenerateDef unique. Refresh this with new
    // random bytes when GenerateDef implementation is modified in a
    // non-backwards-compatible way.
    man.hash.add(@as(u32, 0xDCC14144));

    const contents = try b.build_root.handle.readFileAlloc(arena, self.path, std.math.maxInt(u32));
    man.hash.addBytes(contents);

    const outName = b.fmt("{s}.zig", .{std.fs.path.stem(self.path)});
    if (try step.cacheHit(&man)) {
        const digest = man.final();
        self.generated_file.path = try b.cache_root.join(arena, &.{
            "o", &digest, outName,
        });
        return;
    }

    const digest = man.final();

    const subPath = try std.fs.path.join(arena, &.{ "o", &digest, outName });
    const subPathDirname = std.fs.path.dirname(subPath).?;

    b.cache_root.handle.makePath(subPathDirname) catch |err| {
        return step.fail("unable to make path '{}{s}': {s}", .{
            b.cache_root, subPathDirname, @errorName(err),
        });
    };

    const output = try self.generate(contents);
    b.cache_root.handle.writeFile(.{ .sub_path = subPath, .data = output }) catch |err| {
        return step.fail("unable to write file '{}{s}': {s}", .{
            b.cache_root, subPath, @errorName(err),
        });
    };

    self.generated_file.path = try b.cache_root.join(arena, &.{subPath});
    try man.writeManifest();
}

const Value = struct {
    name: []const u8,
    properties: []const []const u8,
};

fn generate(self: *GenerateDef, input: []const u8) ![]const u8 {
    const arena = self.step.owner.allocator;

    var values = std.StringArrayHashMap([]const []const u8).init(arena);
    defer values.deinit();
    var properties = std.ArrayList([]const u8).init(arena);
    defer properties.deinit();
    var headers = std.ArrayList([]const u8).init(arena);
    defer headers.deinit();

    var valueName: ?[]const u8 = null;
    var it = std.mem.tokenizeAny(u8, input, "\r\n");
    while (it.next()) |lineUntrimmed| {
        const line = std.mem.trim(u8, lineUntrimmed, " \t");
        if (line.len == 0 or line[0] == '#') continue;
        if (std.mem.startsWith(u8, line, "const ") or
            std.mem.startsWith(u8, line, "pub const "))
        {
            try headers.append(line);
            continue;
        }

        if (line[0] == '.') {
            if (valueName == null) {
                return self.step.fail("property not attached to a value:\n\"{s}\"", .{line});
            }
            try properties.append(line);
            continue;
        }

        if (valueName) |name| {
            const old = try values.fetchPut(name, try properties.toOwnedSlice());
            if (old != null) return self.step.fail("duplicate value \"{s}\"", .{name});
        }
        valueName = line;
    }

    if (valueName) |name| {
        const old = try values.fetchPut(name, try properties.toOwnedSlice());
        if (old != null) return self.step.fail("duplicate value \"{s}\"", .{name});
    }

    {
        const sortedList = try arena.dupe([]const u8, values.keys());
        defer arena.free(sortedList);

        std.mem.sort([]const u8, sortedList, {}, struct {
            pub fn lessThan(_: void, a: []const u8, b: []const u8) bool {
                return std.mem.lessThan(u8, a, b);
            }
        }.lessThan);

        var longestName: usize = 0;
        var shortestName: usize = std.math.maxInt(usize);

        var builder = try DafsaBuilder.init(arena);
        defer builder.deinit();

        for (sortedList) |name| {
            try builder.insert(name);
            longestName = @max(name.len, longestName);
            shortestName = @min(name.len, shortestName);
        }
        try builder.finish();
        builder.calcNumbers();

        // As a sanity check, confirm that the minimal perfect hashing doesn't
        // have any collisions
        {
            var indexSet = std.AutoHashMap(usize, void).init(arena);
            defer indexSet.deinit();

            for (values.keys()) |name| {
                const index = builder.getUniqueIndex(name).?;
                const result = try indexSet.getOrPut(index);
                if (result.found_existing) {
                    return self.step.fail("clobbered {}, name={s}\n", .{ index, name });
                }
            }
        }

        var outBuffer = std.ArrayList(u8).init(arena);
        defer outBuffer.deinit();
        const writer = outBuffer.writer();

        try writer.print(
            \\//! Autogenerated by GenerateDef from {s}, do not edit
            \\
            \\const std = @import("std");
            \\
            \\pub fn with(comptime Properties: type) type {{
            \\return struct {{
            \\
        , .{self.path});
        for (headers.items) |line| {
            try writer.print("{s}\n", .{line});
        }

        if (self.kind == .named) {
            try writer.writeAll("pub const Tag = enum {\n");
            for (values.keys()) |property| {
                try writer.print("    {p},\n", .{std.zig.fmtId(property)});
            }
            try writer.writeAll(
                \\
                \\    pub fn property(tag: Tag) Properties {
                \\        return named_data[@intFromEnum(tag)];
                \\    }
                \\
                \\    const named_data = [_]Properties{
                \\
            );
            for (values.values()) |valProps| {
                try writer.writeAll("        .{");
                for (valProps, 0..) |valProp, j| {
                    if (j != 0) try writer.writeByte(',');
                    try writer.writeByte(' ');
                    try writer.writeAll(valProp);
                }
                try writer.writeAll(" },\n");
            }
            try writer.writeAll(
                \\    };
                \\};
                \\};
                \\}
                \\
            );

            return outBuffer.toOwnedSlice();
        }

        var valuesArray = try arena.alloc(Value, values.count());
        defer arena.free(valuesArray);

        for (values.keys(), values.values()) |name, props| {
            const uniqueIdx = builder.getUniqueIndex(name).?;
            const dataIdx = uniqueIdx - 1;
            valuesArray[dataIdx] = .{ .name = name, .properties = props };
        }

        try writer.writeAll(
            \\
            \\tag: Tag,
            \\properties: Properties,
            \\
            \\/// Integer starting at 0 derived from the unique index,
            \\/// corresponds with the data array index.
            \\pub const Tag = enum(u16) {
            \\
        );
        for (valuesArray) |value| {
            try writer.print("    {},\n", .{std.zig.fmtId(value.name)});
        }
        try writer.writeAll(
            \\};
            \\
            \\const Self = @This();
            \\
            \\pub fn fromName(name: []const u8) ?@This() {
            \\    const data_index = tagFromName(name) orelse return null;
            \\    return data[@intFromEnum(data_index)];
            \\}
            \\
            \\pub fn tagFromName(name: []const u8) ?Tag {
            \\    const unique_index = uniqueIndex(name) orelse return null;
            \\    return @enumFromInt(unique_index - 1);
            \\}
            \\
            \\pub fn fromTag(tag: Tag) @This() {
            \\    return data[@intFromEnum(tag)];
            \\}
            \\
            \\pub fn nameFromTagIntoBuf(tag: Tag, name_buf: []u8) []u8 {
            \\    std.debug.assert(name_buf.len >= longest_name);
            \\    const unique_index = @intFromEnum(tag) + 1;
            \\    return nameFromUniqueIndex(unique_index, name_buf);
            \\}
            \\
            \\pub fn nameFromTag(tag: Tag) NameBuf {
            \\    var name_buf: NameBuf = undefined;
            \\    const unique_index = @intFromEnum(tag) + 1;
            \\    const name = nameFromUniqueIndex(unique_index, &name_buf.buf);
            \\    name_buf.len = @intCast(name.len);
            \\    return name_buf;
            \\}
            \\
            \\pub const NameBuf = struct {
            \\    buf: [longest_name]u8 = undefined,
            \\    len: std.math.IntFittingRange(0, longest_name),
            \\
            \\    pub fn span(self: *const NameBuf) []const u8 {
            \\        return self.buf[0..self.len];
            \\    }
            \\};
            \\
            \\pub fn exists(name: []const u8) bool {
            \\    if (name.len < shortest_name or name.len > longest_name) return false;
            \\
            \\    var index: u16 = 0;
            \\    for (name) |c| {
            \\        index = findInList(dafsa[index].child_index, c) orelse return false;
            \\    }
            \\    return dafsa[index].end_of_word;
            \\}
            \\
            \\
        );
        try writer.print("pub const shortest_name = {};\n", .{shortestName});
        try writer.print("pub const longest_name = {};\n\n", .{longestName});
        try writer.writeAll(
            \\/// Search siblings of `first_child_index` for the `char`
            \\/// If found, returns the index of the node within the `dafsa` array.
            \\/// Otherwise, returns `null`.
            \\pub fn findInList(first_child_index: u16, char: u8) ?u16 {
            \\
        );
        try writer.print("    @setEvalBranchQuota({d});\n", .{values.count() * 2});
        try writer.writeAll(
            \\    var index = first_child_index;
            \\    while (true) {
            \\        if (dafsa[index].char == char) return index;
            \\        if (dafsa[index].end_of_list) return null;
            \\        index += 1;
            \\    }
            \\    unreachable;
            \\}
            \\
            \\/// Returns a unique (minimal perfect hash) index (starting at 1) for the `name`,
            \\/// or null if the name was not found.
            \\pub fn uniqueIndex(name: []const u8) ?u16 {
            \\    if (name.len < shortest_name or name.len > longest_name) return null;
            \\
            \\    var index: u16 = 0;
            \\    var node_index: u16 = 0;
            \\
            \\    for (name) |c| {
            \\        const child_index = findInList(dafsa[node_index].child_index, c) orelse return null;
            \\        var sibling_index = dafsa[node_index].child_index;
            \\        while (true) {
            \\            const sibling_c = dafsa[sibling_index].char;
            \\            std.debug.assert(sibling_c != 0);
            \\            if (sibling_c < c) {
            \\                index += dafsa[sibling_index].number;
            \\            }
            \\            if (dafsa[sibling_index].end_of_list) break;
            \\            sibling_index += 1;
            \\        }
            \\        node_index = child_index;
            \\        if (dafsa[node_index].end_of_word) index += 1;
            \\    }
            \\
            \\    if (!dafsa[node_index].end_of_word) return null;
            \\
            \\    return index;
            \\}
            \\
            \\/// Returns a slice of `buf` with the name associated with the given `index`.
            \\/// This function should only be called with an `index` that
            \\/// is already known to exist within the `dafsa`, e.g. an index
            \\/// returned from `uniqueIndex`.
            \\pub fn nameFromUniqueIndex(index: u16, buf: []u8) []u8 {
            \\    std.debug.assert(index >= 1 and index <= data.len);
            \\
            \\    var node_index: u16 = 0;
            \\    var count: u16 = index;
            \\    var fbs = std.io.fixedBufferStream(buf);
            \\    const w = fbs.writer();
            \\
            \\    while (true) {
            \\        var sibling_index = dafsa[node_index].child_index;
            \\        while (true) {
            \\            if (dafsa[sibling_index].number > 0 and dafsa[sibling_index].number < count) {
            \\                count -= dafsa[sibling_index].number;
            \\            } else {
            \\                w.writeByte(dafsa[sibling_index].char) catch unreachable;
            \\                node_index = sibling_index;
            \\                if (dafsa[node_index].end_of_word) {
            \\                    count -= 1;
            \\                }
            \\                break;
            \\            }
            \\
            \\            if (dafsa[sibling_index].end_of_list) break;
            \\            sibling_index += 1;
            \\        }
            \\        if (count == 0) break;
            \\    }
            \\
            \\    return fbs.getWritten();
            \\}
            \\
            \\
        );
        if (self.needs_large_dafsa_node) {
            try writer.writeAll(
                \\/// We're 1 bit shy of being able to fit this in a u32:
                \\/// - char only contains 0-9, a-z, A-Z, and _, so it could use a enum(u6) with a way to convert <-> u8
                \\///   (note: this would have a performance cost that may make the u32 not worth it)
                \\/// - number has a max value of > 2047 and < 4095 (the first _ node has the largest number),
                \\///   so it could fit into a u12
                \\/// - child_index currently has a max of > 4095 and < 8191, so it could fit into a u13
                \\///
                \\/// with the end_of_word/end_of_list 2 bools, that makes 33 bits total
                \\const Node = packed struct(u64) {
                \\    char: u8,
                \\    /// Nodes are numbered with "an integer which gives the number of words that
                \\    /// would be accepted by the automaton starting from that state." This numbering
                \\    /// allows calculating "a one-to-one correspondence between the integers 1 to L
                \\    /// (L is the number of words accepted by the automaton) and the words themselves."
                \\    ///
                \\    /// Essentially, this allows us to have a minimal perfect hashing scheme such that
                \\    /// it's possible to store & lookup the properties of each builtin using a separate array.
                \\    number: u16,
                \\    /// If true, this node is the end of a valid builtin.
                \\    /// Note: This does not necessarily mean that this node does not have child nodes.
                \\    end_of_word: bool,
                \\    /// If true, this node is the end of a sibling list.
                \\    /// If false, then (index + 1) will contain the next sibling.
                \\    end_of_list: bool,
                \\    /// Padding bits to get to u64, unsure if there's some way to use these to improve something.
                \\    _extra: u22 = 0,
                \\    /// Index of the first child of this node.
                \\    child_index: u16,
                \\};
                \\
                \\
            );
        } else {
            try writer.writeAll(
                \\const Node = packed struct(u32) {
                \\    char: u8,
                \\    /// Nodes are numbered with "an integer which gives the number of words that
                \\    /// would be accepted by the automaton starting from that state." This numbering
                \\    /// allows calculating "a one-to-one correspondence between the integers 1 to L
                \\    /// (L is the number of words accepted by the automaton) and the words themselves."
                \\    ///
                \\    /// Essentially, this allows us to have a minimal perfect hashing scheme such that
                \\    /// it's possible to store & lookup the properties of each name using a separate array.
                \\    number: u8,
                \\    /// If true, this node is the end of a valid name.
                \\    /// Note: This does not necessarily mean that this node does not have child nodes.
                \\    end_of_word: bool,
                \\    /// If true, this node is the end of a sibling list.
                \\    /// If false, then (index + 1) will contain the next sibling.
                \\    end_of_list: bool,
                \\    /// Index of the first child of this node.
                \\    child_index: u14,
                \\};
                \\
                \\
            );
        }
        try builder.writeDafsa(writer);
        try writeData(writer, valuesArray);
        try writer.writeAll(
            \\};
            \\}
            \\
        );

        return outBuffer.toOwnedSlice();
    }
}

fn writeData(writer: anytype, values: []const Value) !void {
    try writer.writeAll("pub const data = blk: {\n");
    try writer.print("    @setEvalBranchQuota({d});\n", .{values.len * 7});
    try writer.writeAll("    break :blk [_]@This(){\n");

    for (values) |value| {
        try writer.print("        .{{ .tag = .{}, .properties = .{{", .{std.zig.fmtId(value.name)});
        for (value.properties, 0..) |property, j| {
            if (j != 0) try writer.writeByte(',');
            try writer.writeByte(' ');
            try writer.writeAll(property);
        }
        if (value.properties.len != 0) try writer.writeByte(' ');
        try writer.writeAll("} },\n");
    }
    try writer.writeAll("    };\n");
    try writer.writeAll("};\n");
}

const DafsaBuilder = struct {
    root: *Node,
    arena: std.heap.ArenaAllocator.State,
    allocator: Allocator,
    unchecked_nodes: std.ArrayListUnmanaged(UncheckedNode),
    minimized_nodes: std.HashMapUnmanaged(*Node, *Node, Node.DuplicateContext, std.hash_map.default_max_load_percentage),
    previous_word_buf: [128]u8 = undefined,
    previous_word: []u8 = &[_]u8{},

    const UncheckedNode = struct {
        parent: *Node,
        char: u8,
        child: *Node,
    };

    pub fn init(allocator: Allocator) !DafsaBuilder {
        var arena = std.heap.ArenaAllocator.init(allocator);
        errdefer arena.deinit();

        const root = try arena.allocator().create(Node);
        root.* = .{};
        return DafsaBuilder{
            .root = root,
            .allocator = allocator,
            .arena = arena.state,
            .unchecked_nodes = .{},
            .minimized_nodes = .{},
        };
    }

    pub fn deinit(self: *DafsaBuilder) void {
        self.arena.promote(self.allocator).deinit();
        self.unchecked_nodes.deinit(self.allocator);
        self.minimized_nodes.deinit(self.allocator);
        self.* = undefined;
    }

    const Node = struct {
        children: [256]?*Node = [_]?*Node{null} ** 256,
        is_terminal: bool = false,
        number: usize = 0,

        const DuplicateContext = struct {
            pub fn hash(ctx: @This(), key: *Node) u64 {
                _ = ctx;
                var hasher = std.hash.Wyhash.init(0);
                std.hash.autoHash(&hasher, key.children);
                std.hash.autoHash(&hasher, key.is_terminal);
                return hasher.final();
            }

            pub fn eql(ctx: @This(), a: *Node, b: *Node) bool {
                _ = ctx;
                return a.is_terminal == b.is_terminal and std.mem.eql(?*Node, &a.children, &b.children);
            }
        };

        pub fn calcNumbers(self: *Node) void {
            self.number = @intFromBool(self.is_terminal);
            for (self.children) |maybe_child| {
                const child = maybe_child orelse continue;
                // A node's number is the sum of the
                // numbers of its immediate child nodes.
                child.calcNumbers();
                self.number += child.number;
            }
        }

        pub fn numDirectChildren(self: *const Node) u8 {
            var num: u8 = 0;
            for (self.children) |child| {
                if (child != null) num += 1;
            }
            return num;
        }
    };

    pub fn insert(self: *DafsaBuilder, str: []const u8) !void {
        if (std.mem.order(u8, str, self.previous_word) == .lt) {
            @panic("insertion order must be sorted");
        }

        var common_prefix_len: usize = 0;
        for (0..@min(str.len, self.previous_word.len)) |i| {
            if (str[i] != self.previous_word[i]) break;
            common_prefix_len += 1;
        }

        try self.minimize(common_prefix_len);

        var node = if (self.unchecked_nodes.items.len == 0)
            self.root
        else
            self.unchecked_nodes.getLast().child;

        for (str[common_prefix_len..]) |c| {
            std.debug.assert(node.children[c] == null);

            var arena = self.arena.promote(self.allocator);
            const child = try arena.allocator().create(Node);
            self.arena = arena.state;

            child.* = .{};
            node.children[c] = child;
            try self.unchecked_nodes.append(self.allocator, .{
                .parent = node,
                .char = c,
                .child = child,
            });
            node = node.children[c].?;
        }
        node.is_terminal = true;

        self.previous_word = self.previous_word_buf[0..str.len];
        @memcpy(self.previous_word, str);
    }

    pub fn minimize(self: *DafsaBuilder, down_to: usize) !void {
        if (self.unchecked_nodes.items.len == 0) return;
        while (self.unchecked_nodes.items.len > down_to) {
            const unchecked_node = self.unchecked_nodes.pop();
            if (self.minimized_nodes.getPtr(unchecked_node.child)) |child| {
                unchecked_node.parent.children[unchecked_node.char] = child.*;
            } else {
                try self.minimized_nodes.put(self.allocator, unchecked_node.child, unchecked_node.child);
            }
        }
    }

    pub fn finish(self: *DafsaBuilder) !void {
        try self.minimize(0);
    }

    fn nodeCount(self: *const DafsaBuilder) usize {
        return self.minimized_nodes.count();
    }

    fn edgeCount(self: *const DafsaBuilder) usize {
        var count: usize = 0;
        var it = self.minimized_nodes.iterator();
        while (it.next()) |entry| {
            for (entry.key_ptr.*.children) |child| {
                if (child != null) count += 1;
            }
        }
        return count;
    }

    fn contains(self: *const DafsaBuilder, str: []const u8) bool {
        var node = self.root;
        for (str) |c| {
            node = node.children[c] orelse return false;
        }
        return node.is_terminal;
    }

    fn calcNumbers(self: *const DafsaBuilder) void {
        self.root.calcNumbers();
    }

    fn getUniqueIndex(self: *const DafsaBuilder, str: []const u8) ?usize {
        var index: usize = 0;
        var node = self.root;

        for (str) |c| {
            const child = node.children[c] orelse return null;
            for (node.children, 0..) |sibling, sibling_c| {
                if (sibling == null) continue;
                if (sibling_c < c) {
                    index += sibling.?.number;
                }
            }
            node = child;
            if (node.is_terminal) index += 1;
        }

        return index;
    }

    fn writeDafsa(self: *const DafsaBuilder, writer: anytype) !void {
        try writer.writeAll("const dafsa = [_]Node{\n");

        // write root
        try writer.writeAll("    .{ .char = 0, .end_of_word = false, .end_of_list = true, .number = 0, .child_index = 1 },\n");

        var queue = std.ArrayList(*Node).init(self.allocator);
        defer queue.deinit();

        var childIndexes = std.AutoHashMap(*Node, usize).init(self.allocator);
        defer childIndexes.deinit();

        try childIndexes.ensureTotalCapacity(@intCast(self.edgeCount()));

        var firstAvalilableIndex: usize = self.root.numDirectChildren() + 1;
        firstAvalilableIndex = try writeDafsaChildren(self.root, writer, &queue, &childIndexes, firstAvalilableIndex);

        while (queue.items.len > 0) {
            // TODO: something with better time complexity
            const node = queue.orderedRemove(0);

            firstAvalilableIndex = try writeDafsaChildren(node, writer, &queue, &childIndexes, firstAvalilableIndex);
        }

        try writer.writeAll("};\n");
    }

    fn writeDafsaChildren(
        node: *Node,
        writer: anytype,
        queue: *std.ArrayList(*Node),
        childIndexes: *std.AutoHashMap(*Node, usize),
        firstAvailableIndex: usize,
    ) !usize {
        var curAvailableIndex = firstAvailableIndex;
        const numChildren = node.numDirectChildren();
        var childI: usize = 0;
        for (node.children, 0..) |maybeChild, c_usize| {
            const child = maybeChild orelse continue;
            const c: u8 = @intCast(c_usize);
            const isLastChild = childI == numChildren - 1;

            if (!childIndexes.contains(child)) {
                const childNumChildren = child.numDirectChildren();
                if (childNumChildren > 0) {
                    childIndexes.putAssumeCapacityNoClobber(child, curAvailableIndex);
                    curAvailableIndex += childNumChildren;
                }
                try queue.append(child);
            }

            try writer.print(
                "    .{{ .char = '{c}', .end_of_word = {}, .end_of_list = {}, .number = {}, .child_index = {} }},\n",
                .{ c, child.is_terminal, isLastChild, child.number, childIndexes.get(child) orelse 0 },
            );

            childI += 1;
        }
        return curAvailableIndex;
    }
};
