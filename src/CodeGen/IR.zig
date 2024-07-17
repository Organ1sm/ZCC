const std = @import("std");
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const Compilation = @import("../Basic/Compilation.zig");
const Interner = @import("Interner.zig");
const StringId = @import("../Basic/StringInterner.zig").StringId;
const Value = @import("../AST/Value.zig");

const IR = @This();

pool: Interner,
// decls: std.StringArrayHashMapUnmanaged(Decl),

instructions: std.MultiArrayList(Inst),
body: std.ArrayListUnmanaged(Ref),
arena: std.heap.ArenaAllocator.State,

pub const Builder = struct {
    gpa: Allocator,
    arena: std.heap.ArenaAllocator,
    instructions: std.MultiArrayList(IR.Inst) = .{},
    body: std.ArrayListUnmanaged(Ref) = .{},
    argCount: u32 = 0,
    allocCount: u32 = 0,
    pool: Interner = .{},
    currentLabel: Ref = undefined,

    pub fn deinit(b: *Builder) void {
        b.arena.deinit();
        b.instructions.deinit(b.gpa);
        b.body.deinit(b.gpa);
        b.pool.deinit(b.gpa);
        b.* = undefined;
    }

    pub fn startFn(b: *Builder) Allocator.Error!void {
        b.allocCount = 0;
        b.argCount = 0;
        b.instructions.len = 0;
        b.body.items.len = 0;
        const entry = try b.makeLabel("entry");
        try b.body.append(b.gpa, entry);
        b.currentLabel = entry;
    }

    pub fn startBlock(b: *Builder, label: Ref) !void {
        try b.body.append(b.gpa, label);
        b.currentLabel = label;
    }

    pub fn addArg(b: *Builder, ty: Interner.Ref) Allocator.Error!Ref {
        const ref: Ref = @enumFromInt(b.instructions.len);
        try b.instructions.append(b.gpa, .{ .tag = .Arg, .data = .{ .none = {} }, .type = ty });
        try b.body.insert(b.gpa, b.argCount, ref);
        b.argCount += 1;
        return ref;
    }

    pub fn addAlloc(b: *Builder, size: u32, @"align": u32) Allocator.Error!Ref {
        const ref: Ref = @enumFromInt(b.instructions.len);
        try b.instructions.append(b.gpa, .{
            .tag = .Alloc,
            .data = .{ .alloc = .{ .size = size, .@"align" = @"align" } },
            .type = .ptr,
        });
        try b.body.insert(b.gpa, b.allocCount + b.argCount, ref);
        b.allocCount += 1;
        return ref;
    }

    pub fn addInst(b: *Builder, tag: IR.Inst.Tag, data: IR.Inst.Data, ty: Interner.Ref) Allocator.Error!Ref {
        const ref: Ref = @enumFromInt(b.instructions.len);
        try b.instructions.append(b.gpa, .{ .tag = tag, .data = data, .type = ty });
        try b.body.append(b.gpa, ref);
        return ref;
    }

    pub fn makeLabel(b: *Builder, name: [*:0]const u8) Allocator.Error!Ref {
        const ref: Ref = @enumFromInt(b.instructions.len);
        try b.instructions.append(b.gpa, .{ .tag = .Label, .data = .{ .label = name }, .type = .void });
        return ref;
    }

    pub fn addJump(b: *Builder, label: Ref) Allocator.Error!void {
        _ = try b.addInst(.Jmp, .{ .un = label }, .noreturn);
    }

    pub fn addBranch(b: *Builder, cond: Ref, trueLabel: Ref, falseLabel: Ref) Allocator.Error!void {
        const branch = try b.arena.allocator().create(IR.Inst.Branch);
        branch.* = .{
            .cond = cond,
            .then = trueLabel,
            .@"else" = falseLabel,
        };
        _ = try b.addInst(.Branch, .{ .branch = branch }, .noreturn);
    }

    pub fn addSwitch(b: *Builder, target: Ref, values: []Interner.Ref, labels: []Ref, default: Ref) Allocator.Error!void {
        assert(values.len == labels.len);
        const a = b.arena.allocator();
        const @"switch" = try a.create(IR.Inst.Switch);
        @"switch".* = .{
            .target = target,
            .cases_len = @intCast(values.len),
            .case_vals = (try a.dupe(Interner.Ref, values)).ptr,
            .case_labels = (try a.dupe(Ref, labels)).ptr,
            .default = default,
        };
        _ = try b.addInst(.Switch, .{ .@"switch" = @"switch" }, .noreturn);
    }

    pub fn addStore(b: *Builder, ptr: Ref, val: Ref) Allocator.Error!void {
        _ = try b.addInst(.Store, .{ .bin = .{ .lhs = ptr, .rhs = val } }, .void);
    }

    pub fn addConstant(b: *Builder, val: Value, ty: Interner.Ref) Allocator.Error!Ref {
        const ref: Ref = @enumFromInt(b.instructions.len);
        const key: Interner.Key = .{ .value = val };
        const valRef = try b.pool.put(b.gpa, key);

        try b.instructions.append(b.gpa, .{
            .tag = .Constant,
            .data = .{ .constant = valRef },
            .type = ty,
        });
        return ref;
    }

    pub fn addPhi(b: *Builder, inputs: []const Inst.Phi.Input, ty: Interner.Ref) Allocator.Error!Ref {
        const a = b.arena.allocator();
        const inputRefs = try a.alloc(Ref, inputs.len * 2 + 1);
        inputRefs[0] = @enumFromInt(inputs.len);
        @memcpy(inputRefs[1..], std.mem.bytesAsSlice(Ref, std.mem.sliceAsBytes(inputs)));

        return b.addInst(.Phi, .{ .phi = .{ .ptr = inputRefs.ptr } }, ty);
    }

    pub fn addSelect(b: *Builder, cond: Ref, then: Ref, @"else": Ref, ty: Interner.Ref) Allocator.Error!Ref {
        const branch = try b.arena.allocator().create(IR.Inst.Branch);
        branch.* = .{
            .cond = cond,
            .then = then,
            .@"else" = @"else",
        };
        return b.addInst(.Select, .{ .branch = branch }, ty);
    }
};

pub const Ref = enum(u32) { none = std.math.maxInt(u32), _ };

pub const Inst = struct {
    tag: Tag,
    data: Data,
    type: Interner.Ref,

    pub const Tag = enum {
        // data.constant
        // not included in blocks
        Constant,

        // data.arg
        // not included in blocks
        Arg,
        Symbol,

        // data.label
        Label,

        // data.block
        LabelAddr,
        Jmp,

        // data.switch
        Switch,

        // data.branch
        Branch,
        Select,

        // data.un
        JmpVal,

        // data.call
        Call,

        // data.alloc
        Alloc,

        // data.phi
        Phi,

        // data.bin
        Store,
        BitOr,
        BitXor,
        BitAnd,
        BitShl,
        BitShr,
        CmpEQ,
        CmpNE,
        CmpLT,
        CmpLTE,
        CmpGT,
        CmpGTE,
        Add,
        Sub,
        Mul,
        Div,
        Mod,

        // data.un
        Ret,
        Load,
        BitNot,
        Negate,
        Trunc,
        Zext,
        Sext,
    };

    pub const Data = union {
        constant: Interner.Ref,
        none: void,
        bin: struct {
            lhs: Ref,
            rhs: Ref,
        },
        un: Ref,
        arg: u32,
        alloc: struct {
            size: u32,
            @"align": u32,
        },
        @"switch": *Switch,
        call: *Call,
        label: [*:0]const u8,
        branch: *Branch,
        phi: Phi,
    };

    pub const Branch = struct {
        cond: Ref,
        then: Ref,
        @"else": Ref,
    };

    pub const Switch = struct {
        target: Ref,
        casesLen: u32,
        default: Ref,
        caseVals: [*]Interner.Ref,
        caseLabels: [*]Ref,
    };

    pub const Call = struct {
        func: Ref,
        argsLen: u32,
        argsPtr: [*]Ref,

        pub fn args(c: Call) []Ref {
            return c.argsPtr[0..c.argsLen];
        }
    };

    pub const Phi = struct {
        ptr: [*]IR.Ref,

        pub const Input = struct {
            label: IR.Ref,
            value: IR.Ref,
        };

        pub fn inputs(p: Phi) []Input {
            const len = @intFromEnum(p.ptr[0]) * 2;
            const slice = (p.ptr + 1)[0..len];
            return std.mem.bytesAsSlice(Input, std.mem.sliceAsBytes(slice));
        }
    };
};

pub fn deinit(ir: *IR, gpa: std.mem.Allocator) void {
    ir.arena.promote(gpa).deinit();
    ir.instructions.deinit(gpa);
    ir.* = undefined;
}

const util = @import("../Basic/Util.zig");
const TYPE = util.Color.purple;
const INST = util.Color.cyan;
const REF = util.Color.blue;
const LITERAL = util.Color.green;
const ATTRIBUTE = util.Color.yellow;

const RefMap = std.AutoArrayHashMap(Ref, void);

pub fn dump(ir: IR, gpa: Allocator, name: []const u8, color: bool, w: anytype) !void {
    const tags = ir.instructions.items(.tag);
    const data = ir.instructions.items(.data);

    var refMap = RefMap.init(gpa);
    var labelMap = RefMap.init(gpa);

    defer {
        refMap.deinit();
        labelMap.deinit();
    }

    const retInst = ir.body.items[ir.body.items.len - 1];
    const retOperand = data[@intFromEnum(retInst)].un;
    const retTy = if (retOperand == .none) .void else ir.instructions.items(.type)[@intFromEnum(retOperand)];
    try ir.writeType(retTy, color, w);
    if (color) util.setColor(REF, w);
    try w.print(" @{s}", .{name});
    if (color) util.setColor(.reset, w);
    try w.writeAll("(");

    var argCount: u32 = 0;
    while (true) : (argCount += 1) {
        const ref = ir.body.items[argCount];
        const tag = tags[@intFromEnum(ref)];
        if (tag != .Arg) break;
        if (argCount != 0) try w.writeAll(", ");
        try refMap.put(ref, {});
        try ir.writeRef(&refMap, ref, color, w);
        if (color) util.setColor(.reset, w);
    }
    try w.writeAll(") {\n");

    for (tags, 0..) |tag, i| {
        std.debug.print("{s}({d}) ", .{ @tagName(tag), i });
    }
    std.debug.print("\n", .{});

    for (ir.body.items) |ref| {
        const val = @intFromEnum(ref);
        std.debug.print("{s}({d}) ", .{ @tagName(tags[val]), val });
    }
    std.debug.print("\n", .{});

    for (ir.body.items[argCount..]) |ref| {
        switch (tags[@intFromEnum(ref)]) {
            .Label => try labelMap.put(ref, {}),
            else => {},
        }
    }

    var it = labelMap.iterator();
    while (it.next()) |entry| {
        std.debug.print("Label: {d} ", .{@intFromEnum(entry.key_ptr.*)});
    }
    std.debug.print("\n", .{});

    for (ir.body.items[argCount..]) |ref| {
        const i = @intFromEnum(ref);
        const tag = tags[i];
        switch (tag) {
            .Arg, .Constant, .Symbol => unreachable,

            .Label => {
                const labelIdx = labelMap.getIndex(ref).?;
                if (color) util.setColor(REF, w);
                try w.print("{s}.{d}:\n", .{ data[i].label, labelIdx });
            },

            // .label_val => {
            //     const un = data[i].un;
            //     try w.print("    %{d} = label.{d}\n", .{ i, @intFromEnum(un) });
            // },

            .Jmp => {
                const un = data[i].un;
                if (color) util.setColor(INST, w);
                try w.writeAll("    Jmp ");
                try ir.writeLabel(&labelMap, un, color, w);
                try w.writeByte('\n');
            },

            .Branch => {
                const br = data[i].branch;
                if (color) util.setColor(INST, w);
                try w.writeAll("    Branch ");
                try ir.writeRef(&refMap, br.cond, color, w);
                if (color) util.setColor(.reset, w);
                try w.writeAll(", ");
                try ir.writeLabel(&labelMap, br.then, color, w);
                if (color) util.setColor(.reset, w);
                try w.writeAll(", ");
                try ir.writeLabel(&labelMap, br.@"else", color, w);
                try w.writeByte('\n');
            },

            .Select => {
                const br = data[i].branch;
                try ir.writeNewRef(&refMap, ref, color, w);
                try w.writeAll("Select ");
                try ir.writeRef(&refMap, br.cond, color, w);
                if (color) util.setColor(.reset, w);
                try w.writeAll(", ");
                try ir.writeRef(&refMap, br.then, color, w);
                if (color) util.setColor(.reset, w);
                try w.writeAll(", ");
                try ir.writeRef(&refMap, br.@"else", color, w);
                try w.writeByte('\n');
            },

            // .JmpVal => {
            //     const bin = data[i].bin;
            //     try w.print("    %{s} %{d} label.{d}\n", .{ @tagName(tag), @intFromEnum(bin.lhs), @intFromEnum(bin.rhs) });
            // },

            .Switch => {
                const @"switch" = data[i].@"switch";
                if (color) util.setColor(INST, w);
                try w.writeAll("    Switch ");
                try ir.writeRef(&refMap, @"switch".target, color, w);
                if (color) util.setColor(.reset, w);
                try w.writeAll(" {");
                for (@"switch".caseVals[0..@"switch".casesLen], @"switch".caseLabels) |valRef, labelRef| {
                    try w.writeAll("\n        ");
                    try ir.writeValue(valRef, color, w);
                    if (color) util.setColor(.reset, w);
                    try w.writeAll(" => ");
                    try ir.writeLabel(&labelMap, labelRef, color, w);
                    if (color) util.setColor(.reset, w);
                }
                if (color) util.setColor(LITERAL, w);
                try w.writeAll("\n        default ");
                if (color) util.setColor(.reset, w);
                try w.writeAll("=> ");
                try ir.writeLabel(&labelMap, @"switch".default, color, w);
                if (color) util.setColor(.reset, w);
                try w.writeAll("\n    }\n");
            },

            .Call => {
                const call = data[i].call;
                try ir.writeNewRef(&refMap, ref, color, w);
                try w.writeAll("Call ");
                try ir.writeRef(&refMap, call.func, color, w);
                if (color) util.setColor(.reset, w);
                try w.writeAll("(");

                for (call.args(), 0..) |arg, argIdx| {
                    if (argIdx != 0) try w.writeAll(", ");
                    try ir.writeRef(&refMap, arg, color, w);
                    if (color) util.setColor(.reset, w);
                }
                try w.writeAll(")\n");
            },

            .Alloc => {
                const alloc = data[i].alloc;
                try ir.writeNewRef(&refMap, ref, color, w);
                try w.writeAll("Alloc ");
                if (color) util.setColor(ATTRIBUTE, w);
                try w.writeAll("size ");
                if (color) util.setColor(LITERAL, w);
                try w.print("{d}", .{alloc.size});
                if (color) util.setColor(ATTRIBUTE, w);
                try w.writeAll(" Align ");
                if (color) util.setColor(LITERAL, w);
                try w.print("{d}", .{alloc.@"align"});
                try w.writeByte('\n');
            },

            .Phi => {
                try ir.writeNewRef(&refMap, ref, color, w);
                try w.writeAll("phi");
                if (color) util.setColor(.reset, w);
                try w.writeAll(" {");
                for (data[i].phi.inputs()) |input| {
                    try w.writeAll("\n        ");
                    try ir.writeLabel(&labelMap, input.label, color, w);
                    if (color) util.setColor(.reset, w);
                    try w.writeAll(" => ");
                    try ir.writeRef(&refMap, input.value, color, w);
                    if (color) util.setColor(.reset, w);
                }
                if (color) util.setColor(.reset, w);
                try w.writeAll("\n    }\n");
            },

            .Store => {
                const bin = data[i].bin;
                if (color) util.setColor(INST, w);
                try w.writeAll("    Store ");
                try ir.writeRef(&refMap, bin.lhs, color, w);
                if (color) util.setColor(.reset, w);
                try w.writeAll(", ");
                try ir.writeRef(&refMap, bin.rhs, color, w);
                try w.writeByte('\n');
            },

            .Ret => {
                if (color) util.setColor(INST, w);
                try w.writeAll("    Ret ");
                if (data[i].un != .none)
                    try ir.writeRef(&refMap, data[i].un, color, w);
                try w.writeByte('\n');
            },

            .Load => {
                try ir.writeNewRef(&refMap, ref, color, w);
                try w.writeAll("Load ");
                try ir.writeRef(&refMap, data[i].un, color, w);
                try w.writeByte('\n');
            },

            .BitOr,
            .BitXor,
            .BitAnd,
            .BitShl,
            .BitShr,
            .CmpEQ,
            .CmpNE,
            .CmpLT,
            .CmpLTE,
            .CmpGT,
            .CmpGTE,
            .Add,
            .Sub,
            .Mul,
            .Div,
            .Mod,
            => {
                const bin = data[i].bin;
                try ir.writeNewRef(&refMap, ref, color, w);
                try w.print("{s} ", .{@tagName(tag)});
                try ir.writeRef(&refMap, bin.lhs, color, w);
                if (color) util.setColor(.reset, w);
                try w.writeAll(", ");
                try ir.writeRef(&refMap, bin.rhs, color, w);
                try w.writeByte('\n');
            },

            .BitNot,
            .Negate,
            .Trunc,
            .Zext,
            .Sext,
            => {
                const un = data[i].un;
                try ir.writeNewRef(&refMap, ref, color, w);
                try w.print("{s} ", .{@tagName(tag)});
                try ir.writeRef(&refMap, un, color, w);
                try w.writeByte('\n');
            },

            .LabelAddr, .JmpVal => {},
        }
    }
    if (color) util.setColor(.reset, w);
    try w.writeAll("}\n\n");
}

fn writeType(ir: IR, tyRef: Interner.Ref, color: bool, w: anytype) !void {
    const ty = ir.pool.get(tyRef);
    if (color) util.setColor(TYPE, w);
    switch (ty) {
        .value => unreachable,
        .ptr, .noreturn, .void, .func => try w.writeAll(@tagName(ty)),
        .int => |bits| try w.print("i{d}", .{bits}),
        .float => |bits| try w.print("f{d}", .{bits}),
        .array => |info| {
            try w.print("[{d} * ", .{info.len});
            try ir.writeType(info.child, false, w);
            try w.writeByte(']');
        },
        .vector => |info| {
            try w.print("<{d} * ", .{info.len});
            try ir.writeType(info.child, false, w);
            try w.writeByte('>');
        },
        .record => |info| {
            // TODO collect into buffer and only print once
            try w.writeAll("{ ");
            for (info.elements, 0..) |elem, i| {
                if (i != 0) try w.writeAll(", ");
                try ir.writeType(elem, color, w);
            }
            try w.writeAll(" }");
        },
    }
}

fn writeValue(ir: IR, valRe: Interner.Ref, color: bool, w: anytype) !void {
    const v = ir.pool.get(valRe).value;
    if (color) util.setColor(LITERAL, w);
    switch (v.tag) {
        .unavailable => try w.writeAll(" unavailable"),
        .int => try w.print("{d}", .{v.data.int}),
        .bytes => try w.print("\"{s}\"", .{v.data.bytes}),
        // std.fmt does @as instead of @floatCast
        .float => try w.print("{d}", .{@as(f64, @floatCast(v.data.float))}),
        else => try w.print("({s})", .{@tagName(v.tag)}),
    }
}

fn writeRef(ir: IR, refMap: *RefMap, ref: Ref, color: bool, w: anytype) !void {
    assert(ref != .none);
    const index = @intFromEnum(ref);
    const tyRef = ir.instructions.items(.type)[index];

    if (ir.instructions.items(.tag)[index] == .Constant) {
        try ir.writeType(tyRef, color, w);
        const vRef = ir.instructions.items(.data)[index].constant;
        try w.writeByte(' ');
        try ir.writeValue(vRef, color, w);
        return;
    } else if (ir.instructions.items(.tag)[index] == .Symbol) {
        const name = ir.instructions.items(.data)[index].label;
        try ir.writeType(tyRef, color, w);
        if (color) util.setColor(REF, w);
        try w.print(" @{s}", .{name});
        return;
    }

    try ir.writeType(tyRef, color, w);
    if (color) util.setColor(REF, w);
    const refIndex = refMap.getIndex(ref).?;
    try w.print(" %{d}", .{refIndex});
}

fn writeNewRef(ir: IR, refMap: *RefMap, ref: Ref, color: bool, w: anytype) !void {
    try refMap.put(ref, {});
    try w.writeAll("    ");
    try ir.writeRef(refMap, ref, color, w);
    if (color) util.setColor(.reset, w);
    try w.writeAll(" = ");
    if (color) util.setColor(INST, w);
}

fn writeLabel(ir: IR, labelMap: *RefMap, ref: Ref, color: bool, w: anytype) !void {
    assert(ref != .none);
    const index = @intFromEnum(ref);
    const label = ir.instructions.items(.data)[index].label;

    if (color) util.setColor(REF, w);
    const labelIndex = labelMap.getIndex(ref).?;
    try w.print("{s}.{d}", .{ label, labelIndex });
}
