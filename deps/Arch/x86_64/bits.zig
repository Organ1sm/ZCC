const std = @import("std");
const assert = std.debug.assert;
const expect = std.testing.expect;

const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const DW = std.dwarf;

/// EFLAGS condition codes
pub const Condition = enum(u5) {
    /// above
    a,
    /// above or equal
    ae,
    /// below
    b,
    /// below or equal
    be,
    /// carry
    c,
    /// equal
    e,
    /// greater
    g,
    /// greater or equal
    ge,
    /// less
    l,
    /// less or equal
    le,
    /// not above
    na,
    /// not above or equal
    nae,
    /// not below
    nb,
    /// not below or equal
    nbe,
    /// not carry
    nc,
    /// not equal
    ne,
    /// not greater
    ng,
    /// not greater or equal
    nge,
    /// not less
    nl,
    /// not less or equal
    nle,
    /// not overflow
    no,
    /// not parity
    np,
    /// not sign
    ns,
    /// not zero
    nz,
    /// overflow
    o,
    /// parity
    p,
    /// parity even
    pe,
    /// parity odd
    po,
    /// sign
    s,
    /// zero
    z,

    // Pseudo conditions
    /// zero and not parity
    z_and_np,
    /// not zero or parity
    nz_or_p,

    /// Converts a std.math.CompareOperator into a condition flag,
    /// i.e. returns the condition that is true iff the result of the
    /// comparison is true. Assumes signed comparison
    pub fn fromCompareOperatorSigned(op: std.math.CompareOperator) Condition {
        return switch (op) {
            .gte => .ge,
            .gt => .g,
            .neq => .ne,
            .lt => .l,
            .lte => .le,
            .eq => .e,
        };
    }

    /// Converts a std.math.CompareOperator into a condition flag,
    /// i.e. returns the condition that is true iff the result of the
    /// comparison is true. Assumes unsigned comparison
    pub fn fromCompareOperatorUnsigned(op: std.math.CompareOperator) Condition {
        return switch (op) {
            .gte => .ae,
            .gt => .a,
            .neq => .ne,
            .lt => .b,
            .lte => .be,
            .eq => .e,
        };
    }

    pub fn fromCompareOperator(
        signedness: std.builtin.Signedness,
        op: std.math.CompareOperator,
    ) Condition {
        return switch (signedness) {
            .signed => fromCompareOperatorSigned(op),
            .unsigned => fromCompareOperatorUnsigned(op),
        };
    }

    /// Returns the condition which is true iff the given condition is false
    pub fn negate(cond: Condition) Condition {
        return switch (cond) {
            .a => .na,
            .ae => .nae,
            .b => .nb,
            .be => .nbe,
            .c => .nc,
            .e => .ne,
            .g => .ng,
            .ge => .nge,
            .l => .nl,
            .le => .nle,
            .na => .a,
            .nae => .ae,
            .nb => .b,
            .nbe => .be,
            .nc => .c,
            .ne => .e,
            .ng => .g,
            .nge => .ge,
            .nl => .l,
            .nle => .le,
            .no => .o,
            .np => .p,
            .ns => .s,
            .nz => .z,
            .o => .no,
            .p => .np,
            .pe => .po,
            .po => .pe,
            .s => .ns,
            .z => .nz,

            .z_and_np => .nz_or_p,
            .nz_or_p => .z_and_np,
        };
    }
};

pub const Register = enum(u7) {
    // zig fmt: off
    rax, rcx, rdx, rbx, rsp, rbp, rsi, rdi,
    r8, r9, r10, r11, r12, r13, r14, r15,

    eax, ecx, edx, ebx, esp, ebp, esi, edi,
    r8d, r9d, r10d, r11d, r12d, r13d, r14d, r15d,

    ax, cx, dx, bx, sp, bp, si, di,
    r8w, r9w, r10w, r11w, r12w, r13w, r14w, r15w,

    al, cl, dl, bl, spl, bpl, sil, dil,
    r8b, r9b, r10b, r11b, r12b, r13b, r14b, r15b,

    ah, ch, dh, bh,

    ymm0, ymm1, ymm2,  ymm3,  ymm4,  ymm5,  ymm6,  ymm7,
    ymm8, ymm9, ymm10, ymm11, ymm12, ymm13, ymm14, ymm15,

    xmm0, xmm1, xmm2,  xmm3,  xmm4,  xmm5,  xmm6,  xmm7,
    xmm8, xmm9, xmm10, xmm11, xmm12, xmm13, xmm14, xmm15,

    mm0, mm1, mm2, mm3, mm4, mm5, mm6, mm7,

    st0, st1, st2, st3, st4, st5, st6, st7,

    es, cs, ss, ds, fs, gs,

    rip, eip, ip,

    none,
    // zig fmt: on

    pub const Class = enum {
        general_purpose,
        segment,
        x87,
        mmx,
        sse,
    };

    pub fn class(reg: Register) Class {
        return switch (@intFromEnum(reg)) {
            // zig fmt: off
            @intFromEnum(Register.rax)  ... @intFromEnum(Register.r15)   => .general_purpose,
            @intFromEnum(Register.eax)  ... @intFromEnum(Register.r15d)  => .general_purpose,
            @intFromEnum(Register.ax)   ... @intFromEnum(Register.r15w)  => .general_purpose,
            @intFromEnum(Register.al)   ... @intFromEnum(Register.r15b)  => .general_purpose,
            @intFromEnum(Register.ah)   ... @intFromEnum(Register.bh)    => .general_purpose,

            @intFromEnum(Register.ymm0) ... @intFromEnum(Register.ymm15) => .sse,
            @intFromEnum(Register.xmm0) ... @intFromEnum(Register.xmm15) => .sse,
            @intFromEnum(Register.mm0)  ... @intFromEnum(Register.mm7)   => .mmx,
            @intFromEnum(Register.st0)  ... @intFromEnum(Register.st7)   => .x87,

            @intFromEnum(Register.es)   ... @intFromEnum(Register.gs)    => .segment,

            else => unreachable,
            // zig fmt: on
        };
    }

    pub fn id(reg: Register) u6 {
        const base = switch (@intFromEnum(reg)) {
            // zig fmt: off
            @intFromEnum(Register.rax)  ... @intFromEnum(Register.r15)   => @intFromEnum(Register.rax),
            @intFromEnum(Register.eax)  ... @intFromEnum(Register.r15d)  => @intFromEnum(Register.eax),
            @intFromEnum(Register.ax)   ... @intFromEnum(Register.r15w)  => @intFromEnum(Register.ax),
            @intFromEnum(Register.al)   ... @intFromEnum(Register.r15b)  => @intFromEnum(Register.al),
            @intFromEnum(Register.ah)   ... @intFromEnum(Register.bh)    => @intFromEnum(Register.ah),

            @intFromEnum(Register.ymm0) ... @intFromEnum(Register.ymm15) => @intFromEnum(Register.ymm0) - 16,
            @intFromEnum(Register.xmm0) ... @intFromEnum(Register.xmm15) => @intFromEnum(Register.xmm0) - 16,
            @intFromEnum(Register.mm0)  ... @intFromEnum(Register.mm7)   => @intFromEnum(Register.mm0) - 32,
            @intFromEnum(Register.st0)  ... @intFromEnum(Register.st7)   => @intFromEnum(Register.st0) - 40,

            @intFromEnum(Register.es)   ... @intFromEnum(Register.gs)    => @intFromEnum(Register.es) - 48,

            else => unreachable,
            // zig fmt: on
        };
        return @intCast(@intFromEnum(reg) - base);
    }

    pub fn bitSize(reg: Register) u10 {
        return switch (@intFromEnum(reg)) {
            // zig fmt: off
            @intFromEnum(Register.rax)  ... @intFromEnum(Register.r15)   => 64,
            @intFromEnum(Register.eax)  ... @intFromEnum(Register.r15d)  => 32,
            @intFromEnum(Register.ax)   ... @intFromEnum(Register.r15w)  => 16,
            @intFromEnum(Register.al)   ... @intFromEnum(Register.r15b)  => 8,
            @intFromEnum(Register.ah)   ... @intFromEnum(Register.bh)    => 8,

            @intFromEnum(Register.ymm0) ... @intFromEnum(Register.ymm15) => 256,
            @intFromEnum(Register.xmm0) ... @intFromEnum(Register.xmm15) => 128,
            @intFromEnum(Register.mm0)  ... @intFromEnum(Register.mm7)   => 64,
            @intFromEnum(Register.st0)  ... @intFromEnum(Register.st7)   => 80,

            @intFromEnum(Register.es)   ... @intFromEnum(Register.gs)    => 16,

            else => unreachable,
            // zig fmt: on
        };
    }

    pub fn isExtended(reg: Register) bool {
        return switch (@intFromEnum(reg)) {
            // zig fmt: off
            @intFromEnum(Register.r8)  ... @intFromEnum(Register.r15)    => true,
            @intFromEnum(Register.r8d) ... @intFromEnum(Register.r15d)   => true,
            @intFromEnum(Register.r8w) ... @intFromEnum(Register.r15w)   => true,
            @intFromEnum(Register.r8b) ... @intFromEnum(Register.r15b)   => true,

            @intFromEnum(Register.ymm8) ... @intFromEnum(Register.ymm15) => true,
            @intFromEnum(Register.xmm8) ... @intFromEnum(Register.xmm15) => true,

            else => false,
            // zig fmt: on
        };
    }

    pub fn enc(reg: Register) u4 {
        const base = switch (@intFromEnum(reg)) {
            // zig fmt: off
            @intFromEnum(Register.rax)  ... @intFromEnum(Register.r15)   => @intFromEnum(Register.rax),
            @intFromEnum(Register.eax)  ... @intFromEnum(Register.r15d)  => @intFromEnum(Register.eax),
            @intFromEnum(Register.ax)   ... @intFromEnum(Register.r15w)  => @intFromEnum(Register.ax),
            @intFromEnum(Register.al)   ... @intFromEnum(Register.r15b)  => @intFromEnum(Register.al),
            @intFromEnum(Register.ah)   ... @intFromEnum(Register.bh)    => @intFromEnum(Register.ah) - 4,

            @intFromEnum(Register.ymm0) ... @intFromEnum(Register.ymm15) => @intFromEnum(Register.ymm0),
            @intFromEnum(Register.xmm0) ... @intFromEnum(Register.xmm15) => @intFromEnum(Register.xmm0),
            @intFromEnum(Register.mm0)  ... @intFromEnum(Register.mm7)   => @intFromEnum(Register.mm0),
            @intFromEnum(Register.st0)  ... @intFromEnum(Register.st7)   => @intFromEnum(Register.st0),

            @intFromEnum(Register.es)   ... @intFromEnum(Register.gs)    => @intFromEnum(Register.es),

            else => unreachable,
            // zig fmt: on
        };
        return @truncate(@intFromEnum(reg) - base);
    }

    pub fn lowEnc(reg: Register) u3 {
        return @truncate(reg.enc());
    }

    pub fn toBitSize(reg: Register, bit_size: u64) Register {
        return switch (bit_size) {
            8 => reg.to8(),
            16 => reg.to16(),
            32 => reg.to32(),
            64 => reg.to64(),
            128 => reg.to128(),
            256 => reg.to256(),
            else => unreachable,
        };
    }

    fn gpBase(reg: Register) u7 {
        assert(reg.class() == .general_purpose);
        return switch (@intFromEnum(reg)) {
            // zig fmt: off
            @intFromEnum(Register.rax)  ... @intFromEnum(Register.r15)   => @intFromEnum(Register.rax),
            @intFromEnum(Register.eax)  ... @intFromEnum(Register.r15d)  => @intFromEnum(Register.eax),
            @intFromEnum(Register.ax)   ... @intFromEnum(Register.r15w)  => @intFromEnum(Register.ax),
            @intFromEnum(Register.al)   ... @intFromEnum(Register.r15b)  => @intFromEnum(Register.al),
            @intFromEnum(Register.ah)   ... @intFromEnum(Register.bh)    => @intFromEnum(Register.ah) - 4,
            else => unreachable,
            // zig fmt: on
        };
    }

    pub fn to64(reg: Register) Register {
        return @enumFromInt(@intFromEnum(reg) - reg.gpBase() + @intFromEnum(Register.rax));
    }

    pub fn to32(reg: Register) Register {
        return @enumFromInt(@intFromEnum(reg) - reg.gpBase() + @intFromEnum(Register.eax));
    }

    pub fn to16(reg: Register) Register {
        return @enumFromInt(@intFromEnum(reg) - reg.gpBase() + @intFromEnum(Register.ax));
    }

    pub fn to8(reg: Register) Register {
        return @enumFromInt(@intFromEnum(reg) - reg.gpBase() + @intFromEnum(Register.al));
    }

    fn sseBase(reg: Register) u7 {
        assert(reg.class() == .sse);
        return switch (@intFromEnum(reg)) {
            @intFromEnum(Register.ymm0)...@intFromEnum(Register.ymm15) => @intFromEnum(Register.ymm0),
            @intFromEnum(Register.xmm0)...@intFromEnum(Register.xmm15) => @intFromEnum(Register.xmm0),
            else => unreachable,
        };
    }

    pub fn to256(reg: Register) Register {
        return @enumFromInt(@intFromEnum(reg) - reg.sseBase() + @intFromEnum(Register.ymm0));
    }

    pub fn to128(reg: Register) Register {
        return @enumFromInt(@intFromEnum(reg) - reg.sseBase() + @intFromEnum(Register.xmm0));
    }

    /// DWARF register encoding
    pub fn dwarfNum(reg: Register) u6 {
        return switch (reg.class()) {
            .general_purpose => if (reg.isExtended())
                reg.enc()
            else
                @as(u3, @truncate(@as(u24, 0o54673120) >> @as(u5, reg.enc()) * 3)),
            .sse => 17 + @as(u6, reg.enc()),
            .x87 => 33 + @as(u6, reg.enc()),
            .mmx => 41 + @as(u6, reg.enc()),
            .segment => 50 + @as(u6, reg.enc()),
        };
    }
};

pub const FrameIndex = enum(u32) {
    // This index refers to the start of the arguments passed to this function
    args_frame,
    // This index refers to the return address pushed by a `call` and popped by a `ret`.
    ret_addr,
    // This index refers to the base pointer pushed in the prologue and popped in the epilogue.
    base_ptr,
    // This index refers to the entire stack frame.
    stack_frame,
    // This index refers to the start of the call frame for arguments passed to called functions
    call_frame,
    // Other indices are used for local variable stack slots
    _,

    pub const named_count = @typeInfo(FrameIndex).Enum.fields.len;

    pub fn isNamed(fi: FrameIndex) bool {
        return @intFromEnum(fi) < named_count;
    }

    pub fn format(
        fi: FrameIndex,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) @TypeOf(writer).Error!void {
        try writer.writeAll("FrameIndex");
        if (fi.isNamed()) {
            try writer.writeByte('.');
            try writer.writeAll(@tagName(fi));
        } else {
            try writer.writeByte('(');
            try std.fmt.formatType(@intFromEnum(fi), fmt, options, writer, 0);
            try writer.writeByte(')');
        }
    }
};

/// A linker symbol not yet allocated in VM.
pub const Symbol = struct {
    /// Index of the containing atom.
    atom_index: u32,
    /// Index into the linker's symbol table.
    sym_index: u32,

    pub fn format(
        sym: Symbol,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) @TypeOf(writer).Error!void {
        try writer.writeAll("Symbol(");
        try std.fmt.formatType(sym.atom_index, fmt, options, writer, 0);
        try writer.writeAll(", ");
        try std.fmt.formatType(sym.sym_index, fmt, options, writer, 0);
        try writer.writeByte(')');
    }
};

pub const Memory = struct {
    base: Base,
    mod: Mod,

    pub const Base = union(enum(u2)) {
        none,
        reg: Register,
        frame: FrameIndex,
        reloc: Symbol,

        pub const Tag = @typeInfo(Base).Union.tag_type.?;

        pub fn isExtended(self: Base) bool {
            return switch (self) {
                .none, .frame, .reloc => false, // rsp, rbp, and rip are not extended
                .reg => |reg| reg.isExtended(),
            };
        }
    };

    pub const Mod = union(enum(u1)) {
        rm: struct {
            size: Size,
            index: Register = .none,
            scale: Scale = .@"1",
            disp: i32 = 0,
        },
        off: u64,
    };

    pub const Size = enum(u4) {
        none,
        byte,
        word,
        dword,
        qword,
        tbyte,
        xword,
        yword,
        zword,

        pub fn fromSize(size: u32) Size {
            return switch (size) {
                1...1 => .byte,
                2...2 => .word,
                3...4 => .dword,
                5...8 => .qword,
                9...16 => .xword,
                17...32 => .yword,
                33...64 => .zword,
                else => unreachable,
            };
        }

        pub fn fromBitSize(bit_size: u64) Size {
            return switch (bit_size) {
                8 => .byte,
                16 => .word,
                32 => .dword,
                64 => .qword,
                80 => .tbyte,
                128 => .xword,
                256 => .yword,
                512 => .zword,
                else => unreachable,
            };
        }

        pub fn bitSize(s: Size) u64 {
            return switch (s) {
                .none => 0,
                .byte => 8,
                .word => 16,
                .dword => 32,
                .qword => 64,
                .tbyte => 80,
                .xword => 128,
                .yword => 256,
                .zword => 512,
            };
        }

        pub fn format(
            s: Size,
            comptime _: []const u8,
            _: std.fmt.FormatOptions,
            writer: anytype,
        ) @TypeOf(writer).Error!void {
            if (s == .none) return;
            try writer.writeAll(@tagName(s));
            try writer.writeAll(" ptr");
        }
    };

    pub const Scale = enum(u2) { @"1", @"2", @"4", @"8" };
};

pub const Immediate = union(enum) {
    signed: i32,
    unsigned: u64,

    pub fn u(x: u64) Immediate {
        return .{ .unsigned = x };
    }

    pub fn s(x: i32) Immediate {
        return .{ .signed = x };
    }

    pub fn asSigned(imm: Immediate, bit_size: u64) i64 {
        return switch (imm) {
            .signed => |x| switch (bit_size) {
                1, 8 => @as(i8, @intCast(x)),
                16 => @as(i16, @intCast(x)),
                32, 64 => x,
                else => unreachable,
            },
            .unsigned => |x| switch (bit_size) {
                1, 8 => @as(i8, @bitCast(@as(u8, @intCast(x)))),
                16 => @as(i16, @bitCast(@as(u16, @intCast(x)))),
                32 => @as(i32, @bitCast(@as(u32, @intCast(x)))),
                64 => @bitCast(x),
                else => unreachable,
            },
        };
    }

    pub fn asUnsigned(imm: Immediate, bit_size: u64) u64 {
        return switch (imm) {
            .signed => |x| switch (bit_size) {
                1, 8 => @as(u8, @bitCast(@as(i8, @intCast(x)))),
                16 => @as(u16, @bitCast(@as(i16, @intCast(x)))),
                32, 64 => @as(u32, @bitCast(x)),
                else => unreachable,
            },
            .unsigned => |x| switch (bit_size) {
                1, 8 => @as(u8, @intCast(x)),
                16 => @as(u16, @intCast(x)),
                32 => @as(u32, @intCast(x)),
                64 => x,
                else => unreachable,
            },
        };
    }
};
