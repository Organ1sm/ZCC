const std = @import("std");
const Compilation = @import("../Basic/Compilation.zig");
const Object = @import("Object.zig");

const Elf = @This();

const Section = struct {
    data: std.ArrayListUnmanaged(u8) = .{},
};

obj: Object,
/// The keys are owned by the Codegen.tree
sections: std.StringHashMap(Section),

pub fn create(comp: *Compilation) !*Object {
    const elf = try comp.gpa.create(Elf);
    elf.* = .{
        .obj = .{ .format = .elf, .comp = comp },
        .sections = std.StringHashMap(Section).init(comp.gpa),
    };
    return &elf.obj;
}

pub fn deinit(elf: *Elf) void {
    const gpa = elf.sections.allocator;

    var it = elf.sections.valueIterator();
    while (it.next()) |sect|
        sect.data.deinit(gpa);

    elf.sections.deinit();
    gpa.destroy(elf);
}

pub fn declareSymbol(elf: *Elf, sectionName: []const u8, name: []const u8) !*std.ArrayListUnmanaged(u8) {
    const section = (try elf.sections.getOrPutValue(sectionName, .{})).value_ptr;
    _ = name; // TODO declare symbol
    return &section.data;
}

pub fn finish(elf: *Elf, file: std.fs.File) !void {
    var buffWriter = std.io.bufferedWriter(file.writer());
    const w = buffWriter.writer();
    const shstrtabBeginning = "\x00.shstrtab\x00";
    const addtionalSections = 2; // null section, shstrtab

    const shstrtabLen = blk: {
        var len = shstrtabBeginning.len;
        var it = elf.sections.keyIterator();
        while (it.next()) |name|
            len += name.len + 2;
        break :blk len;
    };

    const shoff = blk: {
        var offset: std.elf.Elf64_Off = shstrtabLen;
        var it = elf.sections.valueIterator();
        while (it.next()) |sect|
            offset += sect.data.items.len;
        break :blk offset;
    };

    var elf_header = std.elf.Elf64_Ehdr{
        .e_ident = .{ 0x7F, 'E', 'L', 'F', 2, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
        .e_type = std.elf.ET.REL, // we only produce relocatables
        .e_machine = elf.obj.comp.target.cpu.arch.toElfMachine(),
        .e_version = 1,
        .e_entry = 0, // linker will handle this
        .e_phoff = 0, // no program header
        .e_shoff = std.mem.alignForward(u64, shoff, 16) + @sizeOf(std.elf.Elf64_Ehdr), // section headers offset
        .e_flags = 0, // no flags
        .e_ehsize = @sizeOf(std.elf.Elf64_Ehdr),
        .e_phentsize = 0, // no program header
        .e_phnum = 0, // no program header
        .e_shentsize = @sizeOf(std.elf.Elf64_Shdr),
        .e_shnum = @as(std.elf.Elf64_Half, @truncate(elf.sections.count() + addtionalSections)),
        .e_shstrndx = 1,
    };
    try w.writeStruct(elf_header);

    // write contents of sections
    {
        var it = elf.sections.valueIterator();
        while (it.next()) |sect| {
            try w.writeAll(sect.data.items);
        }
    }
    // write shstrtab
    {
        try w.writeAll(shstrtabBeginning);
        var it = elf.sections.keyIterator();
        while (it.next()) |name| try w.print(".{s}\x00", .{name.*});
    }

    // pad to 16 bytes
    try w.writeByteNTimes(0, std.mem.alignForward(u64, shoff, 16) - shoff);
    // mandatory null header
    try w.writeStruct(std.mem.zeroes(std.elf.Elf64_Shdr));

    // write shstrtab section header
    {
        var sect_header = std.elf.Elf64_Shdr{
            .sh_name = 1,
            .sh_type = std.elf.SHT_STRTAB,
            .sh_flags = 0,
            .sh_addr = 0,
            .sh_offset = @sizeOf(std.elf.Elf64_Ehdr) + (shoff - shstrtabLen),
            .sh_size = shstrtabLen,
            .sh_link = 0,
            .sh_info = 0,
            .sh_addralign = 1,
            .sh_entsize = 0,
        };
        try w.writeStruct(sect_header);
    }

    // remaining section headers
    {
        var name_offset: std.elf.Elf64_Word = shstrtabBeginning.len;
        var sect_offset: std.elf.Elf64_Addr = @sizeOf(std.elf.Elf64_Ehdr);
        var it = elf.sections.iterator();
        while (it.next()) |entry| {
            var sect_header = std.elf.Elf64_Shdr{
                .sh_name = name_offset,
                .sh_type = std.elf.SHT_PROGBITS,
                .sh_flags = std.elf.SHF_ALLOC + std.elf.SHF_EXECINSTR,
                .sh_addr = 0,
                .sh_offset = sect_offset,
                .sh_size = entry.value_ptr.data.items.len,
                .sh_link = 0,
                .sh_info = 0,
                .sh_addralign = 16,
                .sh_entsize = 0,
            };
            try w.writeStruct(sect_header);

            name_offset += @as(u32, @truncate(entry.key_ptr.len + 2));
            sect_offset += entry.value_ptr.data.items.len;
        }
    }
    try buffWriter.flush();
}
