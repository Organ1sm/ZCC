const std = @import("std");
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const Interner = @import("Interner.zig");
const Object = @import("Object.zig");

const IR = @This();

interner: *Interner,
decls: std.StringArrayHashMapUnmanaged(Decl),

pub const Decl = struct {
    instructions: std.MultiArrayList(Inst),
    body: std.ArrayListUnmanaged(Ref),
    arena: std.heap.ArenaAllocator.State,

    pub fn deinit(decl: *Decl, gpa: Allocator) void {
        decl.instructions.deinit(gpa);
        decl.body.deinit(gpa);
        decl.arena.promote(gpa).deinit();
    }
};

pub const Builder = struct {
    gpa: Allocator,
    arena: std.heap.ArenaAllocator,
    decls: std.StringArrayHashMapUnmanaged(Decl) = .{},
    interner: *Interner,

    instructions: std.MultiArrayList(IR.Inst) = .{},
    body: std.ArrayListUnmanaged(Ref) = .{},
    argCount: u32 = 0,
    allocCount: u32 = 0,
    currentLabel: Ref = undefined,

    pub fn deinit(b: *Builder) void {
        for (b.decls.values()) |*decl| {
            decl.deinit(b.gpa);
        }
        b.decls.deinit(b.gpa);
        b.arena.deinit();
        b.instructions.deinit(b.gpa);
        b.body.deinit(b.gpa);
        b.* = undefined;
    }

    pub fn finish(b: *Builder) IR {
        return .{
            .interner = b.interner,
            .decls = b.decls.move(),
        };
    }

    pub fn startFn(b: *Builder) Allocator.Error!void {
        const entry = try b.makeLabel("entry");
        try b.body.append(b.gpa, entry);
        b.currentLabel = entry;
    }

    pub fn finishFn(b: *Builder, name: []const u8) !void {
        var dupedInstructions = try b.instructions.clone(b.gpa);
        var dupedBody = try b.body.clone(b.gpa);

        errdefer dupedInstructions.deinit(b.gpa);
        errdefer dupedBody.deinit(b.gpa);

        try b.decls.put(b.gpa, name, .{
            .instructions = dupedInstructions,
            .body = dupedBody,
            .arena = b.arena.state,
        });

        b.instructions.shrinkRetainingCapacity(0);
        b.body.shrinkRetainingCapacity(0);
        b.arena = std.heap.ArenaAllocator.init(b.gpa);
        b.allocCount = 0;
        b.argCount = 0;
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

    pub fn addConstant(b: *Builder, val: Interner.Ref, ty: Interner.Ref) Allocator.Error!Ref {
        const ref: Ref = @enumFromInt(b.instructions.len);
        try b.instructions.append(b.gpa, .{
            .tag = .Constant,
            .data = .{ .constant = val },
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

pub const Renderer = struct {
    gpa: Allocator,
    obj: *Object,
    ir: *const IR,
    errors: ErrorList = .{},

    pub const ErrorList = std.StringArrayHashMapUnmanaged([]const u8);

    pub const Error = Allocator.Error || error{LowerFail};

    pub fn deinit(r: *Renderer) void {
        for (r.errors.values()) |msg| r.gpa.free(msg);
        r.errors.deinit(r.gpa);
    }

    pub fn render(r: *Renderer) !void {
        switch (r.obj.target.cpu.arch) {
            .x86, .x86_64 => return @import("IR/x86/Renderer.zig").render(r),
            else => unreachable,
        }
    }

    pub fn fail(
        r: *Renderer,
        name: []const u8,
        comptime format: []const u8,
        args: anytype,
    ) Error {
        try r.errors.ensureUnusedCapacity(r.gpa, 1);
        r.errors.putAssumeCapacity(name, try std.fmt.allocPrint(r.gpa, format, args));
        return error.LowerFail;
    }
};

pub fn render(
    ir: *const IR,
    gpa: Allocator,
    target: std.Target,
    errors: ?*Renderer.ErrorList,
) !*Object {
    const obj = try Object.create(gpa, target);
    errdefer obj.deinit();

    var renderer: Renderer = .{
        .gpa = gpa,
        .obj = obj,
        .ir = ir,
    };

    defer {
        if (errors) |some| some.* = renderer.errors.move();
        renderer.deinit();
    }

    try renderer.render();
    return obj;
}

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
    for (ir.decls.values()) |*decl| {
        decl.deinit(gpa);
    }
    ir.decls.deinit(gpa);
    ir.* = undefined;
}

const TYPE = std.io.tty.Color.bright_magenta;
const INST = std.io.tty.Color.bright_cyan;
const REF = std.io.tty.Color.bright_blue;
const LITERAL = std.io.tty.Color.bright_green;
const ATTRIBUTE = std.io.tty.Color.bright_yellow;

const RefMap = std.AutoArrayHashMap(Ref, void);

pub fn dump(ir: *const IR, gpa: Allocator, config: std.io.tty.Config, w: anytype) !void {
    for (ir.decls.keys(), ir.decls.values()) |name, *decl| {
        try ir.dumpDecl(decl, gpa, name, config, w);
    }
}

fn dumpDecl(ir: *const IR, decl: *const Decl, gpa: Allocator, name: []const u8, config: std.io.tty.Config, w: anytype) !void {
    const tags = decl.instructions.items(.tag);
    const data = decl.instructions.items(.data);

    var refMap = RefMap.init(gpa);
    var labelMap = RefMap.init(gpa);

    defer {
        refMap.deinit();
        labelMap.deinit();
    }

    const retInst = decl.body.items[decl.body.items.len - 1];
    const retOperand = data[@intFromEnum(retInst)].un;
    const retTy = if (retOperand == .none) .void else decl.instructions.items(.type)[@intFromEnum(retOperand)];
    try ir.writeType(retTy, config, w);
    try config.setColor(w, REF);
    try w.print(" @{s}", .{name});
    try config.setColor(w, .reset);
    try w.writeAll("(");

    var argCount: u32 = 0;
    while (true) : (argCount += 1) {
        const ref = decl.body.items[argCount];
        const tag = tags[@intFromEnum(ref)];
        if (tag != .Arg) break;
        if (argCount != 0) try w.writeAll(", ");
        try refMap.put(ref, {});
        try ir.writeRef(decl, &refMap, ref, config, w);
        try config.setColor(w, .reset);
    }
    try w.writeAll(") {\n");

    for (tags, 0..) |tag, i| {
        std.debug.print("{s}({d}) ", .{ @tagName(tag), i });
    }
    std.debug.print("\n", .{});

    for (decl.body.items) |ref| {
        const val = @intFromEnum(ref);
        std.debug.print("{s}({d}) ", .{ @tagName(tags[val]), val });
    }
    std.debug.print("\n", .{});

    for (decl.body.items[argCount..]) |ref| {
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

    for (decl.body.items[argCount..]) |ref| {
        const i = @intFromEnum(ref);
        const tag = tags[i];
        switch (tag) {
            .Arg, .Constant, .Symbol => unreachable,

            .Label => {
                const labelIdx = labelMap.getIndex(ref).?;
                try config.setColor(w, REF);
                try w.print("{s}.{d}:\n", .{ data[i].label, labelIdx });
            },

            // .label_val => {
            //     const un = data[i].un;
            //     try w.print("    %{d} = label.{d}\n", .{ i, @intFromEnum(un) });
            // },

            .Jmp => {
                const un = data[i].un;
                try config.setColor(w, INST);
                try w.writeAll("    Jmp ");
                try writeLabel(decl, &labelMap, un, config, w);
                try w.writeByte('\n');
            },

            .Branch => {
                const br = data[i].branch;
                try config.setColor(w, INST);
                try w.writeAll("    Branch ");
                try ir.writeRef(decl, &refMap, br.cond, config, w);
                try config.setColor(w, .reset);
                try w.writeAll(", ");
                try writeLabel(decl, &labelMap, br.then, config, w);
                try config.setColor(w, .reset);
                try w.writeAll(", ");
                try writeLabel(decl, &labelMap, br.@"else", config, w);
                try w.writeByte('\n');
            },

            .Select => {
                const br = data[i].branch;
                try ir.writeNewRef(decl, &refMap, ref, config, w);
                try w.writeAll("Select ");
                try ir.writeRef(decl, &refMap, br.cond, config, w);
                try config.setColor(w, .reset);
                try w.writeAll(", ");
                try ir.writeRef(decl, &refMap, br.then, config, w);
                try config.setColor(w, .reset);
                try w.writeAll(", ");
                try ir.writeRef(decl, &refMap, br.@"else", config, w);
                try w.writeByte('\n');
            },

            // .JmpVal => {
            //     const bin = data[i].bin;
            //     try w.print("    %{s} %{d} label.{d}\n", .{ @tagName(tag), @intFromEnum(bin.lhs), @intFromEnum(bin.rhs) });
            // },

            .Switch => {
                const @"switch" = data[i].@"switch";
                try config.setColor(w, INST);
                try w.writeAll("    Switch ");
                try ir.writeRef(decl, &refMap, @"switch".target, config, w);
                try config.setColor(w, .reset);
                try w.writeAll(" {");
                for (@"switch".caseVals[0..@"switch".casesLen], @"switch".caseLabels) |valRef, labelRef| {
                    try w.writeAll("\n        ");
                    try ir.writeValue(valRef, config, w);
                    try config.setColor(w, .reset);
                    try w.writeAll(" => ");
                    try writeLabel(decl, &labelMap, labelRef, config, w);
                    try config.setColor(w, .reset);
                }
                try config.setColor(w, LITERAL);
                try w.writeAll("\n        default ");
                try config.setColor(w, .reset);
                try w.writeAll("=> ");
                try writeLabel(decl, &labelMap, @"switch".default, config, w);
                try config.setColor(w, .reset);
                try w.writeAll("\n    }\n");
            },

            .Call => {
                const call = data[i].call;
                try ir.writeNewRef(decl, &refMap, ref, config, w);
                try w.writeAll("Call ");
                try ir.writeRef(decl, &refMap, call.func, config, w);
                try config.setColor(w, .reset);
                try w.writeAll("(");

                for (call.args(), 0..) |arg, argIdx| {
                    if (argIdx != 0) try w.writeAll(", ");
                    try ir.writeRef(decl, &refMap, arg, config, w);
                    try config.setColor(w, .reset);
                }
                try w.writeAll(")\n");
            },

            .Alloc => {
                const alloc = data[i].alloc;
                try ir.writeNewRef(decl, &refMap, ref, config, w);
                try w.writeAll("Alloc ");
                try config.setColor(w, ATTRIBUTE);
                try w.writeAll("size ");
                try config.setColor(w, LITERAL);
                try w.print("{d}", .{alloc.size});
                try config.setColor(w, ATTRIBUTE);
                try w.writeAll(" Align ");
                try config.setColor(w, LITERAL);
                try w.print("{d}", .{alloc.@"align"});
                try w.writeByte('\n');
            },

            .Phi => {
                try ir.writeNewRef(decl, &refMap, ref, config, w);
                try w.writeAll("phi");
                try config.setColor(w, .reset);
                try w.writeAll(" {");
                for (data[i].phi.inputs()) |input| {
                    try w.writeAll("\n        ");
                    try writeLabel(decl, &labelMap, input.label, config, w);
                    try config.setColor(w, .reset);
                    try w.writeAll(" => ");
                    try ir.writeRef(decl, &refMap, input.value, config, w);
                    try config.setColor(w, .reset);
                }
                try config.setColor(w, .reset);
                try w.writeAll("\n    }\n");
            },

            .Store => {
                const bin = data[i].bin;
                try config.setColor(w, INST);
                try w.writeAll("    Store ");
                try ir.writeRef(decl, &refMap, bin.lhs, config, w);
                try config.setColor(w, .reset);
                try w.writeAll(", ");
                try ir.writeRef(decl, &refMap, bin.rhs, config, w);
                try w.writeByte('\n');
            },

            .Ret => {
                try config.setColor(w, INST);
                try w.writeAll("    Ret ");
                if (data[i].un != .none)
                    try ir.writeRef(decl, &refMap, data[i].un, config, w);
                try w.writeByte('\n');
            },

            .Load => {
                try ir.writeNewRef(decl, &refMap, ref, config, w);
                try w.writeAll("Load ");
                try ir.writeRef(decl, &refMap, data[i].un, config, w);
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
                try ir.writeNewRef(decl, &refMap, ref, config, w);
                try w.print("{s} ", .{@tagName(tag)});
                try ir.writeRef(decl, &refMap, bin.lhs, config, w);
                try config.setColor(w, .reset);
                try w.writeAll(", ");
                try ir.writeRef(decl, &refMap, bin.rhs, config, w);
                try w.writeByte('\n');
            },

            .BitNot,
            .Negate,
            .Trunc,
            .Zext,
            .Sext,
            => {
                const un = data[i].un;
                try ir.writeNewRef(decl, &refMap, ref, config, w);
                try w.print("{s} ", .{@tagName(tag)});
                try ir.writeRef(decl, &refMap, un, config, w);
                try w.writeByte('\n');
            },

            .LabelAddr, .JmpVal => {},
        }
    }
    try config.setColor(w, .reset);
    try w.writeAll("}\n\n");
}

fn writeType(ir: IR, tyRef: Interner.Ref, config: std.io.tty.Config, w: anytype) !void {
    const ty = ir.interner.get(tyRef);
    try config.setColor(w, TYPE);
    switch (ty) {
        .ptrTy, .noreturnTy, .voidTy, .funcTy => try w.writeAll(@tagName(ty)),
        .intTy => |bits| try w.print("i{d}", .{bits}),
        .floatTy => |bits| try w.print("f{d}", .{bits}),
        .arrayTy => |info| {
            try w.print("[{d} * ", .{info.len});
            try ir.writeType(info.child, .no_color, w);
            try w.writeByte(']');
        },
        .vectorTy => |info| {
            try w.print("<{d} * ", .{info.len});
            try ir.writeType(info.child, .no_color, w);
            try w.writeByte('>');
        },
        .recordTy => |elems| {
            // TODO collect into buffer and only print once
            try w.writeAll("{ ");
            for (elems, 0..) |elem, i| {
                if (i != 0) try w.writeAll(", ");
                try ir.writeType(elem, config, w);
            }
            try w.writeAll(" }");
        },
        else => unreachable,
    }
}

fn writeValue(ir: IR, val: Interner.Ref, config: std.io.tty.Config, w: anytype) !void {
    try config.setColor(w, LITERAL);
    const key = ir.interner.get(val);
    switch (key) {
        .null => return w.writeAll("nullptr_t"),
        .int => |repr| switch (repr) {
            inline else => |x| return w.print("{d}", .{x}),
        },
        .float => |repr| switch (repr) {
            inline else => |x| return w.print("{d}", .{@as(f64, @floatCast(x))}),
        },
        .bytes => |b| return std.zig.stringEscape(b, "", .{}, w),
        else => unreachable, // not a value
    }
}

fn writeRef(ir: IR, decl: *const Decl, refMap: *RefMap, ref: Ref, config: std.io.tty.Config, w: anytype) !void {
    assert(ref != .none);
    const index = @intFromEnum(ref);
    const tyRef = decl.instructions.items(.type)[index];

    if (decl.instructions.items(.tag)[index] == .Constant) {
        try ir.writeType(tyRef, config, w);
        const vRef = decl.instructions.items(.data)[index].constant;
        try w.writeByte(' ');
        try ir.writeValue(vRef, config, w);
        return;
    } else if (decl.instructions.items(.tag)[index] == .Symbol) {
        const name = decl.instructions.items(.data)[index].label;
        try ir.writeType(tyRef, config, w);
        try config.setColor(w, REF);
        try w.print(" @{s}", .{name});
        return;
    }

    try ir.writeType(tyRef, config, w);
    try config.setColor(w, REF);
    const refIndex = refMap.getIndex(ref).?;
    try w.print(" %{d}", .{refIndex});
}

fn writeNewRef(ir: IR, decl: *const Decl, refMap: *RefMap, ref: Ref, config: std.io.tty.Config, w: anytype) !void {
    try refMap.put(ref, {});
    try w.writeAll("    ");
    try ir.writeRef(decl, refMap, ref, config, w);
    try config.setColor(w, .reset);
    try w.writeAll(" = ");
    try config.setColor(w, INST);
}

fn writeLabel(decl: *const Decl, labelMap: *RefMap, ref: Ref, config: std.io.tty.Config, w: anytype) !void {
    assert(ref != .none);
    const index = @intFromEnum(ref);
    const label = decl.instructions.items(.data)[index].label;

    try config.setColor(w, REF);
    const labelIndex = labelMap.getIndex(ref).?;
    try w.print("{s}.{d}", .{ label, labelIndex });
}
