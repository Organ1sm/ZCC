const std = @import("std");
pub const CodeGen = @import("CodeGen/CodeGen.zig");
pub const Compilation = @import("Basic/Compilation.zig");
pub const Diagnostics = @import("Basic/Diagnostics.zig");
pub const Driver = @import("Driver.zig");
pub const Interner = @import("CodeGen/Interner.zig");
pub const IR = @import("CodeGen/IR.zig");
pub const Object = @import("Object/Object.zig");
pub const Parser = @import("Parser/Parser.zig");
pub const Preprocessor = @import("Lexer/Preprocessor.zig");
pub const Source = @import("Basic/Source.zig");
pub const Token = @import("Lexer/Token.zig").Token;
pub const Lexer = @import("Lexer/Lexer.zig");
pub const Tree = @import("AST/AST.zig");
pub const Type = @import("AST/Type.zig");
pub const VersionStr = @import("Basic/Info.zig").VersionStr;
pub const TypeMapper = @import("Basic/StringInterner.zig").TypeMapper;
pub const TargetUtil = @import("Basic/Target.zig");

pub const version = std.SemanticVersion.parse(VersionStr) catch unreachable;
pub const CallingConvention = enum {
    C,
    stdcall,
    thiscall,
    vectorcall,
};

test "simple test" {
    _ = @import("Basic/CharInfo.zig");
    _ = @import("Basic/Compilation.zig");
    _ = @import("Basic/Target.zig");
    _ = @import("Driver/Distro.zig");
    _ = @import("Driver/GCCVersion.zig");
    _ = @import("Driver/Filesystem.zig");
    _ = @import("Lexer/Lexer.zig");
    _ = @import("Lexer/Preprocessor.zig");
    _ = @import("Parser/InitList.zig");
    _ = @import("AST/AST.zig");
    _ = @import("AST/Value.zig");
}
