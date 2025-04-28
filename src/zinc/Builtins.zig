const std = @import("std");

const Compilation = @import("Basic/Compilation.zig");
const LangOpts = @import("Basic/LangOpts.zig");
const Parser = @import("Parser/Parser.zig");
const target_util = @import("Basic/Target.zig");
const QualType = @import("AST/TypeStore.zig").QualType;
const TypeBuilder = @import("AST/TypeBuilder.zig");
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

fn specForSize(comp: *const Compilation, sizeBits: u32) TypeBuilder.Specifier {
    var qt: QualType = .short;
    if (qt.bitSizeof(comp).? * 8 == sizeBits) return .Short;

    qt = .int;
    if (qt.bitSizeof(comp).? * 8 == sizeBits) return .Int;

    qt = .long;
    if (qt.bitSizeof(comp).? * 8 == sizeBits) return .Long;

    qt = .longlong;
    if (qt.bitSizeof(comp).? * 8 == sizeBits) return .LongLong;

    unreachable;
}

fn createType(
    desc: TypeDescription,
    it: *TypeDescription.TypeIterator,
    comp: *const Compilation,
    allocator: std.mem.Allocator,
) !QualType {
    var builder: QualType.Builder = .{ .errorOnInvalid = true };
    var requireNativeInt32 = false;
    var requireNativeInt64 = false;
    for (desc.prefix) |prefix| {
        switch (prefix) {
            .L => builder.combine(undefined, .Long, 0) catch unreachable,
            .LL => {
                builder.combine(undefined, .Long, 0) catch unreachable;
                builder.combine(undefined, .Long, 0) catch unreachable;
            },
            .LLL => {
                switch (builder.specifier) {
                    .None => builder.specifier = .Int128,
                    .Signed => builder.specifier = .SInt128,
                    .Unsigned => builder.specifier = .UInt128,
                    else => unreachable,
                }
            },
            .Z => requireNativeInt32 = true,
            .W => requireNativeInt64 = true,
            .N => {
                std.debug.assert(desc.spec == .i);
                if (!target_util.isLP64(comp.target)) {
                    builder.combine(undefined, .Long, 0) catch unreachable;
                }
            },
            .O => {
                builder.combine(undefined, .Long, 0) catch unreachable;
                if (comp.target.os.tag != .opencl) {
                    builder.combine(undefined, .Long, 0) catch unreachable;
                }
            },
            .S => builder.combine(undefined, .Signed, 0) catch unreachable,
            .U => builder.combine(undefined, .Unsigned, 0) catch unreachable,
            .I => {
                // Todo: compile-time constant integer
            },
        }
    }
    switch (desc.spec) {
        .v => builder.combine(undefined, .Void, 0) catch unreachable,
        .b => builder.combine(undefined, .Bool, 0) catch unreachable,
        .c => builder.combine(undefined, .Char, 0) catch unreachable,
        .s => builder.combine(undefined, .Short, 0) catch unreachable,
        .i => {
            if (requireNativeInt32) {
                builder.specifier = specForSize(comp, 32);
            } else if (requireNativeInt64) {
                builder.specifier = specForSize(comp, 64);
            } else {
                switch (builder.specifier) {
                    .Int128, .SInt128, .UInt128 => {},
                    else => builder.combine(undefined, .Int, 0) catch unreachable,
                }
            }
        },
        .h => builder.combine(undefined, .FP16, 0) catch unreachable,
        .x => builder.combine(undefined, .Float16, 0) catch unreachable,
        .y => {
            // Todo: __bf16
            return .invalid;
        },
        .f => builder.combine(undefined, .Float, 0) catch unreachable,
        .d => {
            if (builder.specifier == .LongLong) {
                builder.specifier = .Float128;
            } else {
                builder.combine(undefined, .Double, 0) catch unreachable;
            }
        },
        .z => {
            std.debug.assert(builder.specifier == .None);
            builder.specifier = QualType.Builder.fromType(comp.types.size);
        },
        .w => {
            std.debug.assert(builder.specifier == .None);
            builder.specifier = QualType.Builder.fromType(comp.types.wchar);
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
            std.debug.assert(builder.specifier == .None);
            std.debug.assert(desc.suffix.len == 0);
            builder.specifier = QualType.Builder.fromType(comp.types.vaList);
        },
        .A => {
            std.debug.assert(builder.specifier == .None);
            std.debug.assert(desc.suffix.len == 0);
            var vaList = comp.typeStore.vaList;
            if (vaList.isArray()) vaList.decayArray();
            builder.specifier = QualType.Builder.fromType(vaList);
        },
        .V => |elementCount| {
            std.debug.assert(desc.suffix.len == 0);
            const childDesc = it.next().?;
            const childTy = try createType(childDesc, undefined, comp, allocator);
            const arrTy = try allocator.create(QualType.Array);
            arrTy.* = .{
                .len = elementCount,
                .elem = childTy,
            };
            const vectorTy: QualType = .{ .specifier = .Vector, .data = .{ .array = arrTy } };
            builder.specifier = QualType.Builder.fromType(vectorTy);
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
            builder.combine(undefined, .Complex, 0) catch unreachable;
            switch (child) {
                .float => builder.combine(undefined, .Float, 0) catch unreachable,
                .double => builder.combine(undefined, .Double, 0) catch unreachable,
                .longdouble => {
                    builder.combine(undefined, .Long, 0) catch unreachable;
                    builder.combine(undefined, .Double, 0) catch unreachable;
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
                const elemTy = try allocator.create(QualType);
                elemTy.* = builder.finish(undefined) catch unreachable;
                const ty = QualType{
                    .specifier = .Pointer,
                    .data = .{ .subType = elemTy },
                };
                builder.qual = .{};
                builder.specifier = QualType.Builder.fromType(ty);
            },
            .C => builder.qual.@"const" = 0,
            .D => builder.qual.@"volatile" = 0,
            .R => builder.qual.restrict = 0,
        }
    }
    return builder.finish(undefined) catch unreachable;
}

fn createBuiltin(comp: *const Compilation, builtin: Builtin, typeArena: std.mem.Allocator) !QualType {
    var it = TypeDescription.TypeIterator.init(builtin.properties.param_str);

    const retTyDesc = it.next().?;
    if (retTyDesc.spec == .@"!") {
        // Todo: handle target-dependent definition
    }
    const retTy = try createType(retTyDesc, &it, comp, typeArena);
    var paramCount: usize = 0;
    var params: [Builtin.max_param_count]QualType.Func.Param = undefined;
    while (it.next()) |desc| : (paramCount += 1) {
        params[paramCount] = .{ .nameToken = 0, .ty = try createType(desc, &it, comp, typeArena), .name = .empty, .node = .null };
    }

    const dupedParams = try typeArena.dupe(QualType.Func.Param, params[0..paramCount]);
    const func = try typeArena.create(QualType.Func);

    func.* = .{
        .returnType = retTy,
        .params = dupedParams,
    };
    return .{
        .specifier = if (builtin.properties.isVarArgs()) .VarArgsFunc else .Func,
        .data = .{ .func = func },
    };
}

/// Asserts that the builtin has already been created
pub fn lookup(b: *const Builtins, name: []const u8) Expanded {
    const builtin = Builtin.fromName(name).?;
    const qt = b._name_to_type_map.get(name).?;
    return .{ .builtin = builtin, .qt = qt };
}

pub fn getOrCreate(b: *Builtins, comp: *Compilation, name: []const u8, typeArena: std.mem.Allocator) !?Expanded {
    const ty = b._name_to_type_map.get(name) orelse {
        const builtin = Builtin.fromName(name) orelse return null;
        if (!comp.hasBuiltinFunction(builtin)) return null;

        try b._name_to_type_map.ensureUnusedCapacity(comp.gpa, 1);
        const ty = try createBuiltin(comp, builtin, typeArena);
        b._name_to_type_map.putAssumeCapacity(name, ty);

        return .{
            .builtin = builtin,
            .ty = ty,
        };
    };
    const builtin = Builtin.fromName(name).?;
    return .{
        .builtin = builtin,
        .ty = ty,
    };
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

    var arena = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer arena.deinit();

    const typeArena = arena.allocator();

    var builtinIt = Iterator{};
    while (builtinIt.next()) |entry| {
        const name = try typeArena.dupe(u8, entry.name);
        if (try comp.builtins.getOrCreate(&comp, name, typeArena)) |func_ty| {
            const get_again = (try comp.builtins.getOrCreate(&comp, name, std.testing.failing_allocator)).?;
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

            var arena = std.heap.ArenaAllocator.init(comp.gpa);
            defer arena.deinit();

            const type_arena = arena.allocator();

            const num_builtins = 40;
            var builtin_it = Iterator{};
            for (0..num_builtins) |_| {
                const entry = builtin_it.next().?;
                _ = try comp.builtins.getOrCreate(&comp, entry.name, type_arena);
            }
        }
    };

    try std.testing.checkAllAllocationFailures(std.testing.allocator, Test.testOne, .{});
}
