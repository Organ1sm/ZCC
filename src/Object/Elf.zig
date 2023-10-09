const std = @import("std");
const Compilation = @import("../Basic/Compilation.zig");
const Object = @import("Object.zig");

const Elf = @This();

const Section = struct {
    data: std.ArrayListUnmanaged(u8) = .{},
    nameOffset: std.elf.Elf64_Word,
    type: std.elf.Elf64_Word,
    flags: std.elf.Elf64_Xword,
};

const AdditionalSections = 3; // null section, strtab, symtab
const DefaultStringTable = "\x00.strtab\x00.symtab\x00";
const StringTableName = 1;
const SymbolTableName = "\x00.strtab\x00".len;

obj: Object,
/// The keys are owned by the Codegen.tree
sections: std.StringArrayHashMap(Section),
symbolTable: std.ArrayList(std.elf.Elf64_Sym),
stringTable: std.ArrayList(u8),
firstGlobal: std.elf.Elf64_Word = 0,
unnamedSymbolMangle: u32 = 0,

pub fn create(comp: *Compilation) !*Object {
    var strTable = std.ArrayList(u8).init(comp.gpa);
    var symTable = std.ArrayList(std.elf.Elf64_Sym).init(comp.gpa);
    errdefer {
        strTable.deinit();
        symTable.deinit();
    }

    try strTable.appendSlice(DefaultStringTable);
    try symTable.append(std.mem.zeroes(std.elf.Elf64_Sym));

    const elf = try comp.gpa.create(Elf);
    elf.* = .{
        .obj = .{ .format = .elf, .comp = comp },
        .sections = std.StringArrayHashMap(Section).init(comp.gpa),
        .symbolTable = symTable,
        .stringTable = strTable,
    };

    return &elf.obj;
}

pub fn deinit(elf: *Elf) void {
    const gpa = elf.sections.allocator;

    for (elf.sections.values()) |*sect|
        sect.data.deinit(gpa);

    elf.sections.deinit();
    elf.symbolTable.deinit();
    elf.stringTable.deinit();
    gpa.destroy(elf);
}

fn sectionString(sec: Object.Section) []const u8 {
    return switch (sec) {
        .data => "data",
        .readOnlydata => "rodata",
        .func => "text",
        .strings => "rodata.str",
        .custom => |name| name,
    };
}

pub fn getSection(elf: *Elf, section: Object.Section) !*std.ArrayListUnmanaged(u8) {
    const sectionName = sectionString(section);
    const sectIdx = elf.sections.getIndex(sectionName) orelse blk: {
        const idx = elf.sections.count();
        try elf.sections.putNoClobber(sectionName, .{
            .nameOffset = @as(std.elf.Elf64_Word, @truncate(elf.stringTable.items.len)),
            .type = std.elf.SHT_PROGBITS,
            .flags = switch (section) {
                .func, .custom => std.elf.SHF_ALLOC + std.elf.SHF_EXECINSTR,
                .strings => std.elf.SHF_ALLOC + std.elf.SHF_MERGE + std.elf.SHF_STRINGS,
                .readOnlydata => std.elf.SHF_ALLOC,
                .data => std.elf.SHF_ALLOC + std.elf.SHF_WRITE,
            },
        });
        try elf.stringTable.writer().print(".{s}\x00", .{sectionName});
        break :blk idx;
    };

    return &elf.sections.values()[sectIdx].data;
}

pub fn declareSymbol(
    elf: *Elf,
    section: Object.Section,
    name: ?[]const u8,
    linkage: std.builtin.GlobalLinkage,
    @"type": Object.SymbolType,
    offset: u64,
    size: u64,
) ![]const u8 {
    const sectionName = sectionString(section);
    const sectIdx = @as(std.elf.Elf64_Half, @truncate(elf.sections.getIndex(sectionName).?));
    const binding: u8 = switch (linkage) {
        .Internal => std.elf.STB_LOCAL,
        .Strong => std.elf.STB_GLOBAL,
        .Weak => std.elf.STB_WEAK,
        .LinkOnce => unreachable,
    };
    const sym_type: u8 = switch (@"type") {
        .func => std.elf.STT_FUNC,
        .variable => std.elf.STT_OBJECT,
    };
    var sym = std.elf.Elf64_Sym{
        .st_name = @as(std.elf.Elf64_Word, @truncate(elf.stringTable.items.len)),
        .st_info = (binding << 4) + sym_type,
        .st_other = 0,
        .st_shndx = sectIdx + AdditionalSections,
        .st_value = offset,
        .st_size = size,
    };
    if (elf.firstGlobal == 0 and linkage == .Strong)
        elf.firstGlobal = @as(std.elf.Elf64_Word, @truncate(elf.symbolTable.items.len));

    if (name) |some| {
        try elf.stringTable.writer().print("{s}\x00", .{some});
    } else {
        try elf.stringTable.writer().print(".L.{d}\x00", .{elf.unnamedSymbolMangle});
        elf.unnamedSymbolMangle += 1;
    }

    try elf.symbolTable.append(sym);
    return elf.stringTable.items[sym.st_name..];
}

pub fn finish(elf: *Elf, file: std.fs.File) !void {
    var buffWriter = std.io.bufferedWriter(file.writer());
    const w = buffWriter.writer();
    const stringTabIndex = 1;

    const sectionsLen = blk: {
        var len: std.elf.Elf64_Off = 0;
        for (elf.sections.values()) |sect|
            len += sect.data.items.len;
        break :blk len;
    };

    const strTabLen = elf.stringTable.items.len;
    const symTabLen = elf.symbolTable.items.len * @sizeOf(std.elf.Elf64_Sym);

    const strTabOffset = @sizeOf(std.elf.Elf64_Ehdr) + sectionsLen;
    const symTabOffset = strTabOffset + strTabLen;
    const symTabOffsetAligned = std.mem.alignForward(u64, symTabOffset, 8);
    const shOffset = symTabOffsetAligned + symTabLen;
    const shOffsetAligned = std.mem.alignForward(u64, shOffset, 16);

    var elf_header = std.elf.Elf64_Ehdr{
        .e_ident = .{ 0x7F, 'E', 'L', 'F', 2, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
        .e_type = std.elf.ET.REL, // we only produce relocatables
        .e_machine = elf.obj.comp.target.cpu.arch.toElfMachine(),
        .e_version = 1,
        .e_entry = 0, // linker will handle this
        .e_phoff = 0, // no program header
        .e_shoff = shOffsetAligned, // section headers offset
        .e_flags = 0, // no flags
        .e_ehsize = @sizeOf(std.elf.Elf64_Ehdr),
        .e_phentsize = 0, // no program header
        .e_phnum = 0, // no program header
        .e_shentsize = @sizeOf(std.elf.Elf64_Shdr),
        .e_shnum = @as(std.elf.Elf64_Half, @truncate(elf.sections.count() + AdditionalSections)),
        .e_shstrndx = 1,
    };
    try w.writeStruct(elf_header);

    // write contents of sections
    for (elf.sections.values()) |sect|
        try w.writeAll(sect.data.items);

    // write strtab
    try w.writeAll(elf.stringTable.items);
    try w.writeByteNTimes(0, symTabOffsetAligned - symTabOffset);
    try w.writeAll(std.mem.sliceAsBytes(elf.symbolTable.items));

    // pad to 16 bytes
    try w.writeByteNTimes(0, shOffsetAligned - shOffset);
    // mandatory null header
    try w.writeStruct(std.mem.zeroes(std.elf.Elf64_Shdr));

    // write strtab section header
    {
        var sect_header = std.elf.Elf64_Shdr{
            .sh_name = StringTableName,
            .sh_type = std.elf.SHT_STRTAB,
            .sh_flags = 0,
            .sh_addr = 0,
            .sh_offset = strTabOffset,
            .sh_size = strTabLen,
            .sh_link = 0,
            .sh_info = 0,
            .sh_addralign = 1,
            .sh_entsize = 0,
        };
        try w.writeStruct(sect_header);
    }

    // write symtab section header
    {
        var sect_header = std.elf.Elf64_Shdr{
            .sh_name = SymbolTableName,
            .sh_type = std.elf.SHT_SYMTAB,
            .sh_flags = 0,
            .sh_addr = 0,
            .sh_offset = symTabOffsetAligned,
            .sh_size = symTabLen,
            .sh_link = stringTabIndex,
            .sh_info = elf.firstGlobal,
            .sh_addralign = 8,
            .sh_entsize = @sizeOf(std.elf.Elf64_Sym),
        };
        try w.writeStruct(sect_header);
    }

    // remaining section headers
    {
        var sect_offset: std.elf.Elf64_Addr = @sizeOf(std.elf.Elf64_Ehdr);
        for (elf.sections.values()) |sect| {
            var sect_header = std.elf.Elf64_Shdr{
                .sh_name = sect.nameOffset,
                .sh_type = sect.type,
                .sh_flags = sect.flags,
                .sh_addr = 0,
                .sh_offset = sect_offset,
                .sh_size = sect.data.items.len,
                .sh_link = 0,
                .sh_info = 0,
                .sh_addralign = 16,
                .sh_entsize = 0,
            };
            try w.writeStruct(sect_header);

            sect_offset += sect.data.items.len;
        }
    }
    try buffWriter.flush();
}
