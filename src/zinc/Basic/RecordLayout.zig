//! Record layout code adapted from https://github.com/mahkoh/repr-c
//! Licensed under MIT license: https://github.com/mahkoh/repr-c/tree/master/repc/facade

const std = @import("std");
const assert = std.debug.assert;

const Attribute = @import("../Lexer/Attribute.zig");
const Compilation = @import("../Basic/Compilation.zig");
const Parser = @import("../Parser/Parser.zig");
const Target = @import("Target.zig");
const TypeStore = @import("../AST/TypeStore.zig");
const QualType = TypeStore.QualType;
const Type = TypeStore.Type;
const Record = Type.Record;
const Field = Record.Field;
const RecordLayout = Type.Record.Layout;
const FieldLayout = Type.Record.Field.Layout;

const BITS_PER_BYTE = 8;

const OngoingBitfield = struct {
    sizeBits: u64,
    unusedSizeBits: u64,
};

pub const Error = error{Overflow};

fn alignForward(addr: u64, alignment: u64) !u64 {
    const forward_addr = try std.math.add(u64, addr, alignment - 1);
    return std.mem.alignBackward(u64, forward_addr, alignment);
}

const SysVContext = struct {
    /// Does the record has an __attribute__((packed)) annotation.
    attrPacked: bool,
    /// The value of #pragma pack(N) at the type level if any.
    maxFieldAlignBits: ?u64,
    /// The alignment of this record.
    alignedBits: u32,
    isUnion: bool,
    /// The size of the record. This might not be a multiple of 8 if the record contains bit-fields.
    /// For structs, this is also the offset of the first bit after the last field.
    sizeBits: u64,
    /// non-null if the previous field was a non-zero-sized bit-field. Only used by MinGW.
    ongoingBitfield: ?OngoingBitfield,

    comp: *const Compilation,

    fn init(qt: QualType, comp: *const Compilation, pragmaPack: ?u8) SysVContext {
        const packValue: ?u64 = if (pragmaPack) |pak| @as(u64, pak) * BITS_PER_BYTE else null;
        const reqAlign = @as(u32, (qt.requestedAlignment(comp) orelse 1)) * BITS_PER_BYTE;

        return SysVContext{
            .attrPacked = qt.hasAttribute(comp, .@"packed"),
            .maxFieldAlignBits = packValue,
            .alignedBits = reqAlign,
            .isUnion = qt.is(comp, .@"union"),
            .sizeBits = 0,
            .comp = comp,
            .ongoingBitfield = null,
        };
    }

    fn layoutFields(self: *SysVContext, fields: []Type.Record.Field) !void {
        for (fields) |*field| {
            if (field.qt.isInvalid()) continue;

            const typeLayout = computeLayout(field.qt, self.comp);
            const attributes = field.attributes(self.comp);

            if (self.comp.target.isMinGW()) {
                field.layout = try self.layoutMinGWField(field, attributes, typeLayout);
            } else {
                if (field.bitWidth.unpack()) |bitWidth|
                    field.layout = try self.layoutBitField(attributes, typeLayout, field.nameToken != 0, bitWidth)
                else
                    field.layout = try self.layoutRegularField(attributes, typeLayout);
            }
        }
    }

    /// On MinGW the alignment of the field is calculated in the usual way except that the alignment of
    /// the underlying type is ignored in three cases
    /// - the field is packed
    /// - the field is a bit-field and the previous field was a non-zero-sized bit-field with the same type size
    /// - the field is a zero-sized bit-field and the previous field was not a non-zero-sized bit-field
    /// See test case 0068.
    fn ignoreTypeAlignment(
        isAttrPacked: bool,
        bitWidth: ?u32,
        ongoingBitField: ?OngoingBitfield,
        fieldLayout: RecordLayout,
    ) bool {
        if (isAttrPacked) return true;
        if (bitWidth) |width| {
            if (ongoingBitField) |ongoing| {
                if (ongoing.sizeBits == fieldLayout.sizeBits) return true;
            } else {
                if (width == 0) return true;
            }
        }
        return false;
    }

    fn layoutMinGWField(
        self: *SysVContext,
        field: *const Field,
        fieldAttrs: []const Attribute,
        fieldLayout: RecordLayout,
    ) !FieldLayout {
        const annotationAlignmentBits = BITS_PER_BYTE * @as(u32, (QualType.annotationAlignment(self.comp, Attribute.Iterator.initSlice(fieldAttrs)) orelse 1));
        const isAttrPacked = self.attrPacked or isPacked(fieldAttrs);
        const ignoreTypeAlign = ignoreTypeAlignment(isAttrPacked, field.bitWidth.unpack(), self.ongoingBitfield, fieldLayout);

        var fieldAlignmentBits = fieldLayout.fieldAlignmentBits;
        if (ignoreTypeAlign)
            fieldAlignmentBits = BITS_PER_BYTE;

        fieldAlignmentBits = @max(fieldAlignmentBits, annotationAlignmentBits);
        if (self.maxFieldAlignBits) |bits|
            fieldAlignmentBits = @min(fieldAlignmentBits, bits);

        // The field affects the record alignment in one of three cases
        // - the field is a regular field
        // - the field is a zero-width bit-field following a non-zero-width bit-field
        // - the field is a non-zero-width bit-field and not packed.
        // See test case 0069.
        const updateRecordAlignment = field.bitWidth == .null or
            (field.bitWidth.unpack().? == 0 and self.ongoingBitfield != null) or
            (field.bitWidth.unpack().? != 0 and !isAttrPacked);

        // If a field affects the alignment of a record, the alignment is calculated in the
        // usual way except that __attribute__((packed)) is ignored on a zero-width bit-field.
        // See test case 0068.
        if (updateRecordAlignment) {
            var tyAlignmentBits = fieldLayout.fieldAlignmentBits;
            if (isAttrPacked and (field.bitWidth == .null or field.bitWidth.unpack().? != 0))
                tyAlignmentBits = BITS_PER_BYTE;

            tyAlignmentBits = @max(tyAlignmentBits, annotationAlignmentBits);
            if (self.maxFieldAlignBits) |bits|
                tyAlignmentBits = @min(tyAlignmentBits, bits);

            self.alignedBits = @max(self.alignedBits, tyAlignmentBits);
        }

        // NOTE: ty_alignment_bits and field_alignment_bits are different in the following case:
        // Y = { size: 64, alignment: 64 }struct {
        //     { offset: 0, size: 1 }c { size: 8, alignment: 8 }char:1,
        //     @attr_packed _ { size: 64, alignment: 64 }long long:0,
        //     { offset: 8, size: 8 }d { size: 8, alignment: 8 }char,
        // }
        if (field.bitWidth.unpack()) |bitWidth| {
            return self.layoutBitFieldMinGW(fieldLayout.sizeBits, fieldAlignmentBits, field.nameToken != 0, bitWidth);
        } else {
            return self.layoutRegularFieldMinGW(fieldLayout.sizeBits, fieldAlignmentBits);
        }
    }

    fn layoutBitFieldMinGW(
        self: *SysVContext,
        tySizeBits: u64,
        fieldAlignmentBits: u64,
        isNamed: bool,
        width: u64,
    ) !FieldLayout {
        assert(width <= tySizeBits); // validated in parser

        // In a union, the size of the underlying type does not affect the size of the union.
        // See test case 0070.
        if (self.isUnion) {
            self.sizeBits = @max(self.sizeBits, width);
            if (!isNamed) return .{};
            return .{
                .offsetBits = 0,
                .sizeBits = width,
            };
        }
        if (width == 0) {
            self.ongoingBitfield = null;
        } else {
            // If there is an ongoing bit-field in a struct whose underlying type has the same size and
            // if there is enough space left to place this bit-field, then this bit-field is placed in
            // the ongoing bit-field and the size of the struct is not affected by this
            // bit-field. See test case 0037.
            if (self.ongoingBitfield) |*ongoing| {
                if (ongoing.sizeBits == tySizeBits and ongoing.unusedSizeBits >= width) {
                    const offsetBits = self.sizeBits - ongoing.unusedSizeBits;
                    ongoing.unusedSizeBits -= width;
                    if (!isNamed) return .{};
                    return .{
                        .offsetBits = offsetBits,
                        .sizeBits = width,
                    };
                }
            }
            // Otherwise this field is part of a new ongoing bit-field.
            self.ongoingBitfield = .{
                .sizeBits = tySizeBits,
                .unusedSizeBits = tySizeBits - width,
            };
        }

        const offsetBits = try alignForward(self.sizeBits, fieldAlignmentBits);
        self.sizeBits = if (width == 0) offsetBits else offsetBits + tySizeBits;
        if (!isNamed) return .{};
        return .{
            .offsetBits = offsetBits,
            .sizeBits = width,
        };
    }

    fn layoutRegularFieldMinGW(
        self: *SysVContext,
        tySizeBits: u64,
        fieldAlignmentBits: u64,
    ) !FieldLayout {
        self.ongoingBitfield = null;
        // A struct field starts at the next offset in the struct that is properly
        // aligned with respect to the start of the struct. See test case 0033.
        // A union field always starts at offset 0.
        const offsetBits = if (self.isUnion) 0 else try alignForward(self.sizeBits, fieldAlignmentBits);

        // Set the size of the record to the maximum of the current size and the end of
        // the field. See test case 0034.
        self.sizeBits = @max(self.sizeBits, try std.math.add(u64, offsetBits, tySizeBits));

        return .{
            .offsetBits = offsetBits,
            .sizeBits = tySizeBits,
        };
    }

    fn layoutRegularField(
        self: *SysVContext,
        fieldAttrs: []const Attribute,
        fieldLayout: RecordLayout,
    ) !FieldLayout {
        var fieldAlignBits = fieldLayout.fieldAlignmentBits;

        // If the struct or the field is packed, then the alignment of the underlying type is
        // ignored. See test case 0084.
        if (self.attrPacked or isPacked(fieldAttrs))
            fieldAlignBits = BITS_PER_BYTE;

        // The field alignment can be increased by __attribute__((aligned)) annotations on the
        // field. See test case 0085.
        if (QualType.annotationAlignment(self.comp, Attribute.Iterator.initSlice(fieldAttrs))) |anno|
            fieldAlignBits = @max(fieldAlignBits, @as(u32, anno) * BITS_PER_BYTE);

        // #pragma pack takes precedence over all other attributes. See test cases 0084 and
        // 0085.
        if (self.maxFieldAlignBits) |req_bits|
            fieldAlignBits = @min(fieldAlignBits, req_bits);

        // A struct field starts at the next offset in the struct that is properly
        // aligned with respect to the start of the struct.
        const offsetBits = if (self.isUnion) 0 else try alignForward(self.sizeBits, fieldAlignBits);
        const sizeBits = fieldLayout.sizeBits;

        // The alignment of a record is the maximum of its field alignments. See test cases
        // 0084, 0085, 0086.
        self.sizeBits = @max(self.sizeBits, try std.math.add(u64, offsetBits, sizeBits));
        self.alignedBits = @max(self.alignedBits, fieldAlignBits);

        return FieldLayout{ .offsetBits = offsetBits, .sizeBits = sizeBits };
    }

    fn layoutBitField(
        self: *SysVContext,
        fieldAttrs: []const Attribute,
        fieldLayout: RecordLayout,
        isNamed: bool,
        bitWidth: u64,
    ) !FieldLayout {
        const tySizeBits = fieldLayout.sizeBits;
        var tyFieldAlignBits: u32 = fieldLayout.fieldAlignmentBits;

        if (bitWidth > 0) {
            assert(bitWidth <= tySizeBits); // Checked in Parser
            //// Some targets ignore the alignment of the underlying type when laying out
            //// non-zero-sized bit-fields. See test case 0072. On such targets, bit-fields never
            //// cross a storage boundary. See test case 0081.
            if (Target.ignoreNonZeroSizedBitfieldTypeAlignment(self.comp.target))
                tyFieldAlignBits = 1;
        } else {
            // Some targets ignore the alignment of the underlying type when laying out
            // zero-sized bit-fields. See test case 0073.
            if (Target.ignoreZeroSizedBitfieldTypeAlignment(self.comp.target))
                tyFieldAlignBits = 1;

            // Some targets have a minimum alignment of zero-sized bit-fields. See test case
            // 0074.
            if (Target.minZeroWidthBitfieldAlignment(self.comp.target)) |targetAlign|
                tyFieldAlignBits = @max(tyFieldAlignBits, targetAlign);
        }

        // __attribute__((packed)) on the record is identical to __attribute__((packed)) on each
        // field. See test case 0067.
        const attrPacked = self.attrPacked or isPacked(fieldAttrs);
        const hasPackingAnnotation = attrPacked or self.maxFieldAlignBits != null;
        const annotationAlignment = if (QualType.annotationAlignment(self.comp, Attribute.Iterator.initSlice(fieldAttrs))) |anno| @as(u32, anno) * BITS_PER_BYTE else 1;
        const firstUnusedBit: u64 = if (self.isUnion) 0 else self.sizeBits;

        var fieldAlignBits: u64 = 1;
        if (bitWidth == 0) {
            fieldAlignBits = @max(tyFieldAlignBits, annotationAlignment);
        } else if (self.comp.langOpts.emulate == .gcc) {
            // On GCC, the field alignment is at least the alignment requested by annotations
            // except as restricted by #pragma pack. See test case 0083.
            fieldAlignBits = annotationAlignment;
            if (self.maxFieldAlignBits) |maxBits|
                fieldAlignBits = @min(annotationAlignment, maxBits);

            // On GCC, if there are no packing annotations and
            // - the field would otherwise start at an offset such that it would cross a
            //   storage boundary or
            // - the alignment of the type is larger than its size,
            // then it is aligned to the type's field alignment. See test case 0083.
            if (!hasPackingAnnotation) {
                const startBit = try alignForward(firstUnusedBit, fieldAlignBits);
                const doesFieldCrossBoundary = startBit % tyFieldAlignBits + bitWidth > tySizeBits;

                if (tyFieldAlignBits > tySizeBits or doesFieldCrossBoundary)
                    fieldAlignBits = @max(fieldAlignBits, tyFieldAlignBits);
            }
        } else {
            assert(self.comp.langOpts.emulate == .clang);

            // On Clang, the alignment requested by annotations is not respected if it is
            // larger than the value of #pragma pack. See test case 0083.
            if (annotationAlignment <= self.maxFieldAlignBits orelse std.math.maxInt(u29))
                fieldAlignBits = @max(fieldAlignBits, annotationAlignment);

            // On Clang, if there are no packing annotations and the field would cross a
            // storage boundary if it were positioned at the first unused bit in the record,
            // it is aligned to the type's field alignment. See test case 0083.
            if (!hasPackingAnnotation) {
                const doesFieldCrossBoundary = firstUnusedBit % tyFieldAlignBits + bitWidth > tySizeBits;
                if (doesFieldCrossBoundary)
                    fieldAlignBits = @max(fieldAlignBits, tyFieldAlignBits);
            }
        }

        const offsetBits = try alignForward(firstUnusedBit, fieldAlignBits);
        self.sizeBits = @max(self.sizeBits, try std.math.add(u64, offsetBits, bitWidth));

        // Unnamed fields do not contribute to the record alignment except on a few targets.
        // See test case 0079.
        if (isNamed or Target.unnamedFieldAffectsAlignment(self.comp.target)) {
            var inheritedAlignBits: u32 = undefined;

            if (bitWidth == 0) {
                // If the width is 0, #pragma pack and __attribute__((packed)) are ignored.
                // See test case 0075.
                inheritedAlignBits = @max(tyFieldAlignBits, annotationAlignment);
            } else if (self.maxFieldAlignBits) |maxAlignBits| {
                // Otherwise, if a #pragma pack is in effect, __attribute__((packed)) on the field or
                // record is ignored. See test case 0076.
                inheritedAlignBits = @max(tyFieldAlignBits, annotationAlignment);
                inheritedAlignBits = @min(inheritedAlignBits, maxAlignBits);
            } else if (attrPacked) {
                // Otherwise, if the field or the record is packed, the field alignment is 1 bit unless
                // it is explicitly increased with __attribute__((aligned)). See test case 0077.
                inheritedAlignBits = annotationAlignment;
            } else {
                // Otherwise, the field alignment is the field alignment of the underlying type unless
                // it is explicitly increased with __attribute__((aligned)). See test case 0078.
                inheritedAlignBits = @max(tyFieldAlignBits, annotationAlignment);
            }
            self.alignedBits = @max(self.alignedBits, inheritedAlignBits);
        }

        if (!isNamed) return .{};
        return FieldLayout{ .offsetBits = offsetBits, .sizeBits = bitWidth };
    }
};

const MsvcContext = struct {
    reqAlignBits: u32,
    maxFieldAlignBits: ?u32,
    /// The alignment of pointers that point to an object of this type. This is greater to or equal
    /// to the required alignment. Once all fields have been laid out, the size of the record will be
    /// rounded up to this value.
    pointerAlignBits: u32,
    /// The alignment of this type when it is used as a record field. This is greater to or equal to
    /// the pointer alignment.
    fieldAlignBits: u32,
    sizeBits: u64,
    ongoingBitField: ?OngoingBitfield,
    containsNonBitField: bool,
    isUnion: bool,
    comp: *const Compilation,

    fn init(qt: QualType, comp: *const Compilation, pragmaPack: ?u8) MsvcContext {
        var packValue: ?u32 = null;
        if (qt.hasAttribute(comp, .@"packed")) {
            // __attribute__((packed)) behaves like #pragma pack(1) in clang. See test case 0056.
            packValue = BITS_PER_BYTE;
        }
        if (packValue == null) {
            if (pragmaPack) |pack| {
                packValue = pack * BITS_PER_BYTE;
                // std.debug.print("pragma : {}\n", .{pack});
            }
        }
        if (packValue) |pack| {
            packValue = msvcPragmaPack(comp, pack);
            // std.debug.print("pre:{} post:{?}\n", .{ pack, pack_value });
        }

        // The required alignment can be increased by adding a __declspec(align)
        // annotation. See test case 0023.
        const mustAlign = @as(u32, (qt.requestedAlignment(comp) orelse 1)) * BITS_PER_BYTE;
        return MsvcContext{
            .reqAlignBits = mustAlign,
            .pointerAlignBits = mustAlign,
            .fieldAlignBits = mustAlign,
            .sizeBits = 0,
            .maxFieldAlignBits = packValue,
            .ongoingBitField = null,
            .containsNonBitField = false,
            .isUnion = qt.is(comp, .@"union"),
            .comp = comp,
        };
    }

    fn layoutField(self: *MsvcContext, field: *const Field, fieldAttrs: []const Attribute) !FieldLayout {
        const typeLayout = computeLayout(field.qt, self.comp);

        // The required alignment of the field is the maximum of the required alignment of the
        // underlying type and the __declspec(align) annotation on the field itself.
        // See test case 0028.
        var reqAlign = typeLayout.requiredAlignmentBits;
        if (QualType.annotationAlignment(self.comp, Attribute.Iterator.initSlice(fieldAttrs))) |anno|
            reqAlign = @max(@as(u32, anno) * BITS_PER_BYTE, reqAlign);

        // The required alignment of a record is the maximum of the required alignments of its
        // fields except that the required alignment of bitfields is ignored.
        // See test case 0029.
        if (field.bitWidth == .null)
            self.reqAlignBits = @max(self.reqAlignBits, reqAlign);

        // The offset of the field is based on the field alignment of the underlying type.
        // See test case 0027.
        var fieldAlignBits = typeLayout.fieldAlignmentBits;
        if (self.maxFieldAlignBits) |maxAlign|
            fieldAlignBits = @min(fieldAlignBits, maxAlign);

        // check the requested alignment of the field type.
        if (field.qt.requestedAlignment(self.comp)) |typeReqAlign|
            fieldAlignBits = @max(fieldAlignBits, typeReqAlign * 8);

        if (isPacked(fieldAttrs)) {
            // __attribute__((packed)) on a field is a clang extension. It behaves as if #pragma
            // pack(1) had been applied only to this field. See test case 0057.
            fieldAlignBits = BITS_PER_BYTE;
        }

        // __attribute__((packed)) on a field is a clang extension. It behaves as if #pragma
        // pack(1) had been applied only to this field. See test case 0057.
        fieldAlignBits = @max(fieldAlignBits, reqAlign);
        if (field.bitWidth.unpack()) |bitWidth| {
            return self.layoutBitField(typeLayout.sizeBits, fieldAlignBits, bitWidth);
        } else {
            return self.layoutRegularField(typeLayout.sizeBits, fieldAlignBits);
        }
    }

    fn layoutBitField(
        self: *MsvcContext,
        tySizeBits: u64,
        fieldAlign: u32,
        bitWidth: u32,
    ) !FieldLayout {
        if (bitWidth == 0) {
            // A zero-sized bit-field that does not follow a non-zero-sized bit-field does not affect
            // the overall layout of the record. Even in a union where the order would otherwise
            // not matter. See test case 0035.
            if (self.ongoingBitField) |_| {
                self.ongoingBitField = null;
            } else {
                // this field takes 0 space.
                return .{ .offsetBits = self.sizeBits, .sizeBits = bitWidth };
            }
        } else {
            assert(bitWidth <= tySizeBits);
            // If there is an ongoing bit-field in a struct whose underlying type has the same size and
            // if there is enough space left to place this bit-field, then this bit-field is placed in
            // the ongoing bit-field and the overall layout of the struct is not affected by this
            // bit-field. See test case 0037.
            if (!self.isUnion) {
                if (self.ongoingBitField) |*p| {
                    if (p.sizeBits == tySizeBits and p.unusedSizeBits >= bitWidth) {
                        const offsetBits = self.sizeBits - p.unusedSizeBits;
                        p.unusedSizeBits -= bitWidth;
                        return FieldLayout{ .offsetBits = offsetBits, .sizeBits = bitWidth };
                    }
                }
            }
            // Otherwise this field is part of a new ongoing bit-field.
            self.ongoingBitField = .{ .sizeBits = tySizeBits, .unusedSizeBits = tySizeBits - bitWidth };
        }
        const offsetBits = if (!self.isUnion) bits: {
            // This is the one place in the layout of a record where the pointer alignment might
            // get assigned a smaller value than the field alignment. This can only happen if
            // the field or the type of the field has a required alignment. Otherwise the value
            // of field_alignment_bits is already bound by max_field_alignment_bits.
            // See test case 0038.
            const pAlign = if (self.maxFieldAlignBits) |maxFieldAlign|
                @min(maxFieldAlign, fieldAlign)
            else
                fieldAlign;

            self.pointerAlignBits = @max(self.pointerAlignBits, pAlign);
            self.fieldAlignBits = @max(self.fieldAlignBits, fieldAlign);

            const offsetBits = try alignForward(self.sizeBits, fieldAlign);
            self.sizeBits = if (bitWidth == 0) offsetBits else offsetBits + tySizeBits;

            break :bits offsetBits;
        } else bits: {
            // Bit-fields do not affect the alignment of a union. See test case 0041.
            self.sizeBits = @max(self.sizeBits, tySizeBits);
            break :bits 0;
        };

        return FieldLayout{ .offsetBits = offsetBits, .sizeBits = bitWidth };
    }

    fn layoutRegularField(
        self: *MsvcContext,
        sizeBits: u64,
        fieldAlign: u32,
    ) !FieldLayout {
        self.containsNonBitField = true;
        self.ongoingBitField = null;
        // The alignment of the field affects both the pointer alignment and the field
        // alignment of the record. See test case 0032.
        self.pointerAlignBits = @max(self.pointerAlignBits, fieldAlign);
        self.fieldAlignBits = @max(self.fieldAlignBits, fieldAlign);
        const offsetBits = switch (self.isUnion) {
            true => 0,
            false => try alignForward(self.sizeBits, fieldAlign),
        };
        self.sizeBits = @max(self.sizeBits, offsetBits + sizeBits);
        return FieldLayout{ .offsetBits = offsetBits, .sizeBits = sizeBits };
    }
    fn handleZeroSizedRecord(self: *MsvcContext) void {
        if (self.isUnion) {
            // MSVC does not allow unions without fields.
            // If all fields in a union have size 0, the size of the union is set to
            // - its field alignment if it contains at least one non-bitfield
            // - 4 bytes if it contains only bitfields
            // See test case 0025.
            if (self.containsNonBitField)
                self.sizeBits = self.fieldAlignBits
            else
                self.sizeBits = 4 * BITS_PER_BYTE;
        } else {
            // If all fields in a struct have size 0, its size is set to its required alignment
            // but at least to 4 bytes. See test case 0026.
            self.sizeBits = @max(self.reqAlignBits, 4 * BITS_PER_BYTE);
            self.pointerAlignBits = @min(self.pointerAlignBits, self.sizeBits);
        }
    }
};

pub fn compute(fields: []Type.Record.Field, qt: QualType, comp: *const Compilation, pragmaPack: ?u8) Error!Type.Record.Layout {
    switch (comp.langOpts.emulate) {
        .gcc, .clang => {
            var context = SysVContext.init(qt, comp, pragmaPack);
            try context.layoutFields(fields);

            context.sizeBits = try alignForward(context.sizeBits, context.alignedBits);

            return .{
                .sizeBits = context.sizeBits,
                .fieldAlignmentBits = context.alignedBits,
                .pointerAlignmentBits = context.alignedBits,
                .requiredAlignmentBits = BITS_PER_BYTE,
            };
        },

        .msvc => {
            var context = MsvcContext.init(qt, comp, pragmaPack);
            for (fields) |*field| {
                if (field.qt.isInvalid()) continue;
                field.layout = try context.layoutField(field, field.attributes(comp));
            }
            if (context.sizeBits == 0) {
                // As an extension, MSVC allows records that only contain zero-sized bitfields and empty
                // arrays. Such records would be zero-sized but this case is handled here separately to
                // ensure that there are no zero-sized records.
                context.handleZeroSizedRecord();
            }

            context.sizeBits = try alignForward(context.sizeBits, context.pointerAlignBits);
            return .{
                .sizeBits = context.sizeBits,
                .fieldAlignmentBits = context.fieldAlignBits,
                .pointerAlignmentBits = context.pointerAlignBits,
                .requiredAlignmentBits = context.reqAlignBits,
            };
        },
    }
}

pub fn computeLayout(qt: QualType, comp: *const Compilation) RecordLayout {
    switch (qt.base(comp).type) {
        .@"struct", .@"union" => |record| {
            const requested = BITS_PER_BYTE * (qt.requestedAlignment(comp) orelse 0);
            return .{
                .sizeBits = record.layout.?.sizeBits,
                .pointerAlignmentBits = @max(requested, record.layout.?.pointerAlignmentBits),
                .fieldAlignmentBits = @max(requested, record.layout.?.fieldAlignmentBits),
                .requiredAlignmentBits = record.layout.?.requiredAlignmentBits,
            };
        },
        else => {
            const typeAlign = qt.alignof(comp) * BITS_PER_BYTE;
            return .{
                .sizeBits = qt.bitSizeofOrNull(comp) orelse 0,
                .pointerAlignmentBits = typeAlign,
                .fieldAlignmentBits = typeAlign,
                .requiredAlignmentBits = BITS_PER_BYTE,
            };
        },
    }
}

fn isPacked(attrs: ?[]const Attribute) bool {
    const a = attrs orelse return false;

    for (a) |attribute| {
        if (attribute.tag != .@"packed") continue;
        return true;
    }
    return false;
}

// The effect of #pragma pack(N) depends on the target.
//
// x86: By default, there is no maximum field alignment. N={1,2,4} set the maximum field
//      alignment to that value. All other N activate the default.
// x64: By default, there is no maximum field alignment. N={1,2,4,8} set the maximum field
//      alignment to that value. All other N activate the default.
// arm: By default, the maximum field alignment is 8. N={1,2,4,8,16} set the maximum field
//      alignment to that value. All other N activate the default.
// arm64: By default, the maximum field alignment is 8. N={1,2,4,8} set the maximum field
//        alignment to that value. N=16 disables the maximum field alignment. All other N
//        activate the default.
//
// See test case 0020.
pub fn msvcPragmaPack(comp: *const Compilation, pack: u32) ?u32 {
    return switch (pack) {
        8, 16, 32 => pack,
        64 => if (comp.target.cpu.arch == .x86) null else pack,
        128 => if (comp.target.cpu.arch == .thumb) pack else null,
        else => {
            return switch (comp.target.cpu.arch) {
                .thumb, .aarch64 => 64,
                else => null,
            };
        },
    };
}
