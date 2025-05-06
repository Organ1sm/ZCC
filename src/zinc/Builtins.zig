const std = @import("std");

const Compilation = @import("Basic/Compilation.zig");
const LangOpts = @import("Basic/LangOpts.zig");
const Parser = @import("Parser/Parser.zig");
const TargetUtil = @import("Basic/Target.zig");
const TypeStore = @import("AST/TypeStore.zig");
const Type = TypeStore.Type;
const QualType = TypeStore.QualType;
const Builder = TypeStore.Builder;
const TypeDescription = @import("Builtins/TypeDescription.zig");

const Properties = @import("Builtins/Properties.zig");
pub const Builtin = @import("Builtins/Builtin.def").with(Properties);

const Expanded = struct {
    qt: QualType,
    builtin: Builtin,
};

const NameToTypeMap = std.StringHashMapUnmanaged(QualType);

const Builtins = @This();

_name_to_type_map: NameToTypeMap = .{},

pub fn deinit(b: *Builtins, gpa: std.mem.Allocator) void {
    b._name_to_type_map.deinit(gpa);
}

fn specForSize(comp: *const Compilation, sizeBits: u32) Builder.Specifier {
    var qt: QualType = .short;
    if (qt.bitSizeof(comp) * 8 == sizeBits) return .Short;

    qt = .int;
    if (qt.bitSizeof(comp) * 8 == sizeBits) return .Int;

    qt = .long;
    if (qt.bitSizeof(comp) * 8 == sizeBits) return .Long;

    qt = .longlong;
    if (qt.bitSizeof(comp) * 8 == sizeBits) return .LongLong;

    unreachable;
}

fn createType(
    desc: TypeDescription,
    it: *TypeDescription.TypeIterator,
    comp: *Compilation,
) !QualType {
    var parser: Parser = undefined;
    parser.comp = comp;
    var builder: Builder = .{ .parser = &parser, .errorOnInvalid = true };

    var requireNativeInt32 = false;
    var requireNativeInt64 = false;
    for (desc.prefix) |prefix| {
        switch (prefix) {
            .L => builder.combine(.Long, 0) catch unreachable,
            .LL => {
                builder.combine(.Long, 0) catch unreachable;
                builder.combine(.Long, 0) catch unreachable;
            },
            .LLL => {
                switch (builder.type) {
                    .None => builder.type = .Int128,
                    .Signed => builder.type = .SInt128,
                    .Unsigned => builder.type = .UInt128,
                    else => unreachable,
                }
            },
            .Z => requireNativeInt32 = true,
            .W => requireNativeInt64 = true,
            .N => {
                std.debug.assert(desc.spec == .i);
                if (!TargetUtil.isLP64(comp.target)) {
                    builder.combine(.Long, 0) catch unreachable;
                }
            },
            .O => {
                builder.combine(.Long, 0) catch unreachable;
                if (comp.target.os.tag != .opencl) {
                    builder.combine(.Long, 0) catch unreachable;
                }
            },
            .S => builder.combine(.Signed, 0) catch unreachable,
            .U => builder.combine(.Unsigned, 0) catch unreachable,
            .I => {
                // Todo: compile-time constant integer
            },
        }
    }
    switch (desc.spec) {
        .v => builder.combine(.Void, 0) catch unreachable,
        .b => builder.combine(.Bool, 0) catch unreachable,
        .c => builder.combine(.Char, 0) catch unreachable,
        .s => builder.combine(.Short, 0) catch unreachable,
        .i => {
            if (requireNativeInt32) {
                builder.type = specForSize(comp, 32);
            } else if (requireNativeInt64) {
                builder.type = specForSize(comp, 64);
            } else {
                switch (builder.type) {
                    .Int128, .SInt128, .UInt128 => {},
                    else => builder.combine(.Int, 0) catch unreachable,
                }
            }
        },
        .h => builder.combine(.FP16, 0) catch unreachable,
        .x => builder.combine(.Float16, 0) catch unreachable,
        .y => {
            // Todo: __bf16
            return .invalid;
        },
        .f => builder.combine(.Float, 0) catch unreachable,
        .d => {
            if (builder.type == .LongLong) {
                builder.type = .Float128;
            } else {
                builder.combine(.Double, 0) catch unreachable;
            }
        },
        .z => {
            std.debug.assert(builder.type == .None);
            builder.type = Builder.fromType(comp, comp.typeStore.size);
        },
        .w => {
            std.debug.assert(builder.type == .None);
            builder.type = Builder.fromType(comp, comp.typeStore.wchar);
        },
        .F => {
            return .invalid;
        },
        .G => {
            // Todo: id
            return .invalid;
        },
        .H => {
            // Todo: SEL
            return .invalid;
        },
        .M => {
            // Todo: struct objc_super
            return .invalid;
        },
        .a => {
            std.debug.assert(builder.type == .None);
            std.debug.assert(desc.suffix.len == 0);
            builder.type = Builder.fromType(comp, comp.typeStore.vaList);
        },
        .A => {
            std.debug.assert(builder.type == .None);
            std.debug.assert(desc.suffix.len == 0);
            var vaList = comp.typeStore.vaList;
            std.debug.assert(!vaList.is(comp, .array));
            builder.type = Builder.fromType(comp, vaList);
        },
        .V => |elementCount| {
            std.debug.assert(desc.suffix.len == 0);
            const childDesc = it.next().?;
            const elemQt = try createType(childDesc, undefined, comp);
            const vectorQt = try comp.typeStore.put(comp.gpa, .{
                .vector = .{ .len = elementCount, .elem = elemQt },
            });
            builder.type = .{ .other = vectorQt };
        },
        .q => {
            // Todo: scalable vector
            return .invalid;
        },
        .E => {
            // Todo: ext_vector (OpenCL vector)
            return .invalid;
        },
        .X => |child| {
            builder.combine(.Complex, 0) catch unreachable;
            switch (child) {
                .float => builder.combine(.Float, 0) catch unreachable,
                .double => builder.combine(.Double, 0) catch unreachable,
                .longdouble => {
                    builder.combine(.Long, 0) catch unreachable;
                    builder.combine(.Double, 0) catch unreachable;
                },
            }
        },
        .Y => {
            return .invalid;
        },
        .P => {
            return .invalid;
        },
        .J => {
            return .invalid;
        },
        .SJ => {
            // todo
            return .invalid;
        },
        .K => {
            // todo
            return .invalid;
        },
        .p => {
            return .invalid;
        },
        .@"!" => return .invalid,
    }
    for (desc.suffix) |suffix| {
        switch (suffix) {
            .@"*" => |addressSpace| {
                _ = addressSpace; // TODO: handle address space
                var pointerQt = try comp.typeStore.put(comp.gpa, .{ .pointer = .{
                    .child = builder.finish() catch unreachable,
                    .decayed = null,
                } });
                pointerQt.@"const" = builder.@"const" != null;
                pointerQt.@"volatile" = builder.@"volatile" != null;
                pointerQt.restrict = builder.restrict != null;

                builder.@"const" = null;
                builder.@"volatile" = null;
                builder.restrict = null;
                builder.type = .{ .other = pointerQt };
            },
            .C => builder.@"const" = 0,
            .D => builder.@"volatile" = 0,
            .R => builder.restrict = 0,
        }
    }
    return builder.finish() catch unreachable;
}

fn createBuiltin(comp: *Compilation, builtin: Builtin) !QualType {
    var it = TypeDescription.TypeIterator.init(builtin.properties.param_str);

    const retTyDesc = it.next().?;
    if (retTyDesc.spec == .@"!") {
        // Todo: handle target-dependent definition
    }
    const retTy = try createType(retTyDesc, &it, comp);
    var paramCount: usize = 0;
    var params: [Builtin.max_param_count]Type.Func.Param = undefined;
    while (it.next()) |desc| : (paramCount += 1) {
        params[paramCount] = .{ .nameToken = 0, .qt = try createType(desc, &it, comp), .name = .empty, .node = .null };
    }

    return comp.typeStore.put(comp.gpa, .{ .func = .{
        .returnType = retTy,
        .kind = if (builtin.properties.isVarArgs()) .Variadic else .Normal,
        .params = params[0..paramCount],
    } });
}

/// Asserts that the builtin has already been created
pub fn lookup(b: *const Builtins, name: []const u8) Expanded {
    const builtin = Builtin.fromName(name).?;
    const qt = b._name_to_type_map.get(name).?;
    return .{ .builtin = builtin, .qt = qt };
}

pub fn getOrCreate(b: *Builtins, comp: *Compilation, name: []const u8) !?Expanded {
    const qt = b._name_to_type_map.get(name) orelse {
        const builtin = Builtin.fromName(name) orelse return null;
        if (!comp.hasBuiltinFunction(builtin)) return null;

        try b._name_to_type_map.ensureUnusedCapacity(comp.gpa, 1);
        const qt = try createBuiltin(comp, builtin);
        b._name_to_type_map.putAssumeCapacity(name, qt);

        return .{ .builtin = builtin, .qt = qt };
    };

    const builtin = Builtin.fromName(name).?;
    return .{ .builtin = builtin, .qt = qt };
}

pub const Iterator = struct {
    index: u16 = 1,
    nameBuffer: [Builtin.longest_name]u8 = undefined,

    pub const Entry = struct {
        /// Memory of this slice is overwritten on every call to `next`
        name: []const u8,
        builtin: Builtin,
    };

    pub fn next(self: *Iterator) ?Entry {
        if (self.index > Builtin.data.len) return null;
        const index = self.index;
        const dataIndex = index - 1;
        self.index += 1;
        return .{
            .name = Builtin.nameFromUniqueIndex(index, &self.nameBuffer),
            .builtin = Builtin.data[dataIndex],
        };
    }
};

test Iterator {
    var it = Iterator{};

    var seen = std.StringHashMap(Builtin).init(std.testing.allocator);
    defer seen.deinit();

    var arenaState = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arenaState.deinit();
    const arena = arenaState.allocator();

    while (it.next()) |entry| {
        const index = Builtin.uniqueIndex(entry.name).?;
        var buf: [Builtin.longest_name]u8 = undefined;
        const name_from_index = Builtin.nameFromUniqueIndex(index, &buf);
        try std.testing.expectEqualStrings(entry.name, name_from_index);

        if (seen.contains(entry.name)) {
            std.debug.print("iterated over {s} twice\n", .{entry.name});
            std.debug.print("current data: {}\n", .{entry.builtin});
            std.debug.print("previous data: {}\n", .{seen.get(entry.name).?});
            return error.TestExpectedUniqueEntries;
        }
        try seen.put(try arena.dupe(u8, entry.name), entry.builtin);
    }
    try std.testing.expectEqual(@as(usize, Builtin.data.len), seen.count());
}

test "All builtins" {
    var comp = Compilation.init(std.testing.allocator, std.fs.cwd());
    defer comp.deinit();

    _ = try comp.generateBuiltinMacros(.IncludeSystemDefines);

    var builtinIt = Iterator{};
    while (builtinIt.next()) |entry| {
        const interned = try comp.internString(entry.name);
        const name = try interned.lookup(&comp);
        if (try comp.builtins.getOrCreate(&comp, name)) |func_ty| {
            const get_again = (try comp.builtins.getOrCreate(&comp, name)).?;
            const found_by_lookup = comp.builtins.lookup(name);
            try std.testing.expectEqual(func_ty.builtin.tag, get_again.builtin.tag);
            try std.testing.expectEqual(func_ty.builtin.tag, found_by_lookup.builtin.tag);
        }
    }
}

test "Allocation failures" {
    const Test = struct {
        fn testOne(allocator: std.mem.Allocator) !void {
            var comp = Compilation.init(allocator, std.fs.cwd());
            defer comp.deinit();

            _ = try comp.generateBuiltinMacros(.IncludeSystemDefines);

            const num_builtins = 40;
            var builtin_it = Iterator{};
            for (0..num_builtins) |_| {
                const entry = builtin_it.next().?;
                _ = try comp.builtins.getOrCreate(&comp, entry.name);
            }
        }
    };

    try std.testing.checkAllAllocationFailures(std.testing.allocator, Test.testOne, .{});
}
