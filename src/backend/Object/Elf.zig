const std = @import("std");
const Object = @import("../Object.zig");

const Elf = @This();

const Section = struct {
    data: std.ArrayList(u8) = .empty,
    relocations: std.ArrayList(Relocation) = .empty,
    flags: u64,
    type: u32,
    index: u16 = undefined,
};

const Symbol = struct {
    section: ?*Section,
    size: u64,
    offset: u64,
    index: u16 = undefined,
    info: u8,
};

const Relocation = struct {
    symbol: *Symbol,
    addend: i64,
    offset: u48,
    type: u8,
};

const AdditionalSections = 3; // null section, strtab, symtab
const StringTabIndex = 1;
const SymbolTabIndex = 2;
const DefaultStringTable = "\x00.strtab\x00.symtab\x00";
const StringTableName = 1;
const SymbolTableName = "\x00.strtab\x00".len;

obj: Object,
/// The keys are owned by the Codegen.tree
sections: std.StringHashMapUnmanaged(*Section) = .empty,
localSymbols: std.StringHashMapUnmanaged(*Symbol) = .empty,
globalSymbols: std.StringHashMapUnmanaged(*Symbol) = .empty,
unnamedSymbolMangle: u32 = 0,
stringTabLen: u64 = DefaultStringTable.len,
arena: std.heap.ArenaAllocator,

pub fn create(gpa: std.mem.Allocator, target: std.Target) !*Object {
    const elf = try gpa.create(Elf);
    elf.* = .{
        .obj = .{ .format = .elf, .target = target },
        .arena = std.heap.ArenaAllocator.init(gpa),
    };
    return &elf.obj;
}

pub fn deinit(elf: *Elf) void {
    const gpa = elf.arena.child_allocator;

    {
        var it = elf.sections.valueIterator();
        while (it.next()) |sect| {
            sect.*.data.deinit(gpa);
            sect.*.relocations.deinit(gpa);
        }
    }

    elf.sections.deinit(gpa);
    elf.localSymbols.deinit(gpa);
    elf.globalSymbols.deinit(gpa);
    elf.arena.deinit();
    gpa.destroy(elf);
}

fn sectionString(sec: Object.Section) []const u8 {
    return switch (sec) {
        .undefined => unreachable,
        .data => "data",
        .readOnlydata => "rodata",
        .func => "text",
        .strings => "rodata.str",
        .custom => |name| name,
    };
}

pub fn getSection(elf: *Elf, sectionKind: Object.Section) !*std.ArrayList(u8) {
    const sectionName = sectionString(sectionKind);
    const section = elf.sections.get(sectionName) orelse blk: {
        const section = try elf.arena.allocator().create(Section);
        section.* = .{
            .data = std.ArrayList(u8).init(elf.arena.child_allocator),
            .type = std.elf.SHT_PROGBITS,
            .flags = switch (sectionKind) {
                .func, .custom => std.elf.SHF_ALLOC + std.elf.SHF_EXECINSTR,
                .strings => std.elf.SHF_ALLOC + std.elf.SHF_MERGE + std.elf.SHF_STRINGS,
                .readOnlydata => std.elf.SHF_ALLOC,
                .data => std.elf.SHF_ALLOC + std.elf.SHF_WRITE,
                .undefined => unreachable,
            },
        };
        try elf.sections.putNoClobber(elf.arena.child_allocator, sectionName, section);
        elf.stringTabLen += sectionName.len + ".\x00".len;
        break :blk section;
    };

    return &section.data;
}

pub fn declareSymbol(
    elf: *Elf,
    sectionKind: Object.Section,
    maybeName: ?[]const u8,
    linkage: std.builtin.GlobalLinkage,
    @"type": Object.SymbolType,
    offset: u64,
    size: u64,
) ![]const u8 {
    const section = blk: {
        if (sectionKind == .undefined)
            break :blk null;

        const sectionName = sectionString(sectionKind);
        break :blk elf.sections.get(sectionName);
    };
    const binding: u8 = switch (linkage) {
        .internal => std.elf.STB_LOCAL,
        .strong => std.elf.STB_GLOBAL,
        .weak => std.elf.STB_WEAK,
        .link_once => unreachable,
    };
    const symType: u8 = switch (@"type") {
        .func => std.elf.STT_FUNC,
        .variable => std.elf.STT_OBJECT,
        .external => std.elf.STT_NOTYPE,
    };

    const name = if (maybeName) |some| some else blk: {
        defer elf.unnamedSymbolMangle += 1;
        break :blk try std.fmt.allocPrint(elf.arena.allocator(), ".L.{d}", .{elf.unnamedSymbolMangle});
    };

    const gop = if (linkage == .internal)
        try elf.localSymbols.getOrPut(elf.arena.child_allocator, name)
    else
        try elf.globalSymbols.getOrPut(elf.arena.child_allocator, name);

    if (!gop.found_existing) {
        gop.value_ptr.* = try elf.arena.allocator().create(Symbol);
        elf.stringTabLen += name.len + 1; // +1 for null byte
    }

    gop.value_ptr.*.* = .{
        .section = section,
        .size = size,
        .offset = offset,
        .info = (binding << 4) + symType,
    };

    return name;
}

pub fn addRelocation(elf: *Elf, name: []const u8, sectionKind: Object.Section, address: u64, addend: i64) !void {
    const sectionName = sectionString(sectionKind);
    const symbol = elf.localSymbols.get(name) orelse elf.globalSymbols.get(name).?; // reference to undeclared symbol
    const section = elf.sections.get(sectionName).?;
    if (section.relocations.items.len == 0)
        elf.stringTabLen += ".rela".len;

    try section.relocations.append(elf.arena.child_allocator, .{
        .symbol = symbol,
        .offset = @intCast(address),
        .addend = addend,
        .type = if (symbol.section == null) 4 else 2, // TODO
    });
}

/// elf header
/// sections contents
/// symbols
/// relocations
/// strtab
/// section headers
pub fn finish(elf: *Elf, w: *std.Io.Writer) !void {
    var numSections: std.elf.Half = AdditionalSections;
    var relocationsLen: std.elf.Elf64_Off = 0;
    var sectionsLen: std.elf.Elf64_Off = 0;
    {
        var it = elf.sections.valueIterator();
        while (it.next()) |sect| {
            sectionsLen += sect.*.data.items.len;
            relocationsLen += sect.*.relocations.items.len * @sizeOf(std.elf.Elf64_Rela);
            sect.*.index = numSections;
            numSections += 1;
            numSections += @intFromBool(sect.*.relocations.items.len != 0);
        }
    }

    const symTabLen = (elf.localSymbols.count() + elf.globalSymbols.count() + 1) * @sizeOf(std.elf.Elf64_Sym);
    const symTabOffset = @sizeOf(std.elf.Elf64_Ehdr) + sectionsLen;
    const symTabOffsetAligned = std.mem.alignForward(u64, symTabOffset, 8);
    const relaOffset = symTabOffsetAligned + symTabLen;
    const strTabOffset = relaOffset + relocationsLen;
    const shOffset = strTabOffset + elf.stringTabLen;
    const shOffsetAligned = std.mem.alignForward(u64, shOffset, 16);
    const endian = elf.obj.target.cpu.arch.endian();

    const elfHeader: std.elf.Elf64_Ehdr = .{
        .e_ident = .{ 0x7F, 'E', 'L', 'F', 2, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
        .e_type = std.elf.ET.REL, // we only produce relocatables
        .e_machine = elf.obj.target.toElfMachine(),
        .e_version = 1,
        .e_entry = 0, // linker will handle this
        .e_phoff = 0, // no program header
        .e_shoff = shOffsetAligned, // section headers offset
        .e_flags = 0, // no flags
        .e_ehsize = @sizeOf(std.elf.Elf64_Ehdr),
        .e_phentsize = 0, // no program header
        .e_phnum = 0, // no program header
        .e_shentsize = @sizeOf(std.elf.Elf64_Shdr),
        .e_shnum = numSections,
        .e_shstrndx = 1,
    };
    try w.writeStruct(elfHeader, endian);

    // write contents of sections
    {
        var it = elf.sections.valueIterator();
        while (it.next()) |sect| try w.writeAll(sect.*.data.items);
    }

    // pad to 8 bytes
    try w.splatByteAll(0, @intCast(symTabOffsetAligned - symTabOffset));

    var nameOffset: u32 = DefaultStringTable.len;
    // write symbols
    {
        // first symbol must be null
        try w.writeStruct(std.mem.zeroes(std.elf.Elf64_Sym), endian);

        var symIdx: u16 = 1;
        var it = elf.localSymbols.iterator();
        while (it.next()) |entry| {
            const sym = entry.value_ptr.*;
            try w.writeStruct(std.elf.Elf64_Sym{
                .st_name = nameOffset,
                .st_info = sym.info,
                .st_other = 0,
                .st_shndx = if (sym.section) |some| some.index else 0,
                .st_value = sym.offset,
                .st_size = sym.size,
            }, endian);
            sym.index = symIdx;
            symIdx += 1;
            nameOffset += @intCast(entry.key_ptr.len + 1); // +1 for null byte
        }
        it = elf.globalSymbols.iterator();
        while (it.next()) |entry| {
            const sym = entry.value_ptr.*;
            try w.writeStruct(std.elf.Elf64_Sym{
                .st_name = nameOffset,
                .st_info = sym.info,
                .st_other = 0,
                .st_shndx = if (sym.section) |some| some.index else 0,
                .st_value = sym.offset,
                .st_size = sym.size,
            }, endian);
            sym.index = symIdx;
            symIdx += 1;
            nameOffset += @intCast(entry.key_ptr.len + 1); // +1 for null byte
        }
    }

    // write relocations
    {
        var it = elf.sections.valueIterator();
        while (it.next()) |sect| {
            for (sect.*.relocations.items) |rela| {
                try w.writeStruct(std.elf.Elf64_Rela{
                    .r_offset = rela.offset,
                    .r_addend = rela.addend,
                    .r_info = (@as(u64, rela.symbol.index) << 32) | rela.type,
                }, endian);
            }
        }
    }

    // write strtab
    try w.writeAll(DefaultStringTable);
    {
        var it = elf.localSymbols.keyIterator();
        while (it.next()) |key|
            try w.print("{s}\x00", .{key.*});

        it = elf.globalSymbols.keyIterator();
        while (it.next()) |key|
            try w.print("{s}\x00", .{key.*});
    }
    {
        var it = elf.sections.iterator();
        while (it.next()) |entry| {
            if (entry.value_ptr.*.relocations.items.len != 0)
                try w.writeAll(".rela");
            try w.print(".{s}\x00", .{entry.key_ptr.*});
        }
    }

    // pad to 16 bytes
    try w.splatByteAll(0, @intCast(shOffsetAligned - shOffset));
    // mandatory null header
    try w.writeStruct(std.mem.zeroes(std.elf.Elf64_Shdr), endian);

    // write strtab section header
    {
        const sectHeader: std.elf.Elf64_Shdr = .{
            .sh_name = StringTableName,
            .sh_type = std.elf.SHT_STRTAB,
            .sh_flags = 0,
            .sh_addr = 0,
            .sh_offset = strTabOffset,
            .sh_size = elf.stringTabLen,
            .sh_link = 0,
            .sh_info = 0,
            .sh_addralign = 1,
            .sh_entsize = 0,
        };
        try w.writeStruct(sectHeader, endian);
    }

    // write symtab section header
    {
        const sectHeader: std.elf.Elf64_Shdr = .{
            .sh_name = SymbolTableName,
            .sh_type = std.elf.SHT_SYMTAB,
            .sh_flags = 0,
            .sh_addr = 0,
            .sh_offset = symTabOffsetAligned,
            .sh_size = symTabLen,
            .sh_link = StringTabIndex,
            .sh_info = elf.localSymbols.size + 1,
            .sh_addralign = 8,
            .sh_entsize = @sizeOf(std.elf.Elf64_Sym),
        };
        try w.writeStruct(sectHeader, endian);
    }

    // remaining section headers
    {
        var sectOffset: u64 = @sizeOf(std.elf.Elf64_Ehdr);
        var relaSectOffset: u64 = relaOffset;
        var it = elf.sections.iterator();
        while (it.next()) |entry| {
            const sect = entry.value_ptr.*;
            const relaCount = sect.relocations.items.len;
            const relaNameOffset: u32 = if (relaCount != 0) @truncate(".rela".len) else 0;
            try w.writeStruct(std.elf.Elf64_Shdr{
                .sh_name = relaNameOffset + nameOffset,
                .sh_type = sect.type,
                .sh_flags = sect.flags,
                .sh_addr = 0,
                .sh_offset = sectOffset,
                .sh_size = sect.data.items.len,
                .sh_link = 0,
                .sh_info = 0,
                .sh_addralign = if (sect.flags & std.elf.SHF_EXECINSTR != 0) 16 else 1,
                .sh_entsize = 0,
            }, endian);

            if (relaCount != 0) {
                const size = relaCount * @sizeOf(std.elf.Elf64_Rela);
                try w.writeStruct(std.elf.Elf64_Shdr{
                    .sh_name = nameOffset,
                    .sh_type = std.elf.SHT_RELA,
                    .sh_flags = 0,
                    .sh_addr = 0,
                    .sh_offset = relaSectOffset,
                    .sh_size = relaCount * @sizeOf(std.elf.Elf64_Rela),
                    .sh_link = SymbolTabIndex,
                    .sh_info = sect.index,
                    .sh_addralign = 8,
                    .sh_entsize = @sizeOf(std.elf.Elf64_Rela),
                }, endian);
                relaSectOffset += size;
            }

            sectOffset += sect.data.items.len;
            nameOffset += @as(u32, @intCast(entry.key_ptr.len + ".\x00".len)) + relaNameOffset;
        }
    }
    try w.flush();
}
