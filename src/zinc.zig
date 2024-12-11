pub const CodeGen = @import("zinc/CodeGen/CodeGen.zig");
pub const Compilation = @import("zinc/Basic/Compilation.zig");
pub const Diagnostics = @import("zinc/Basic/Diagnostics.zig");
pub const Driver = @import("zinc/Driver.zig");
pub const Parser = @import("zinc/Parser/Parser.zig");
pub const Preprocessor = @import("zinc/Lexer/Preprocessor.zig");
pub const Source = @import("zinc/Basic/Source.zig");
pub const Token = @import("zinc/Lexer/Token.zig").Token;
pub const Lexer = @import("zinc/Lexer/Lexer.zig");
pub const Tree = @import("zinc/AST/AST.zig");
pub const Type = @import("zinc/AST/Type.zig");
pub const TypeMapper = @import("zinc/Basic/StringInterner.zig").TypeMapper;
pub const TargetUtil = @import("zinc/Basic/Target.zig");
pub const ToolChain = @import("zinc/Toolchain.zig");
pub const Value = @import("zinc/AST/Value.zig");

const backend = @import("backend");
pub const Interner = backend.Interner;
pub const IR = backend.Ir;
pub const Object = backend.Object;

pub const CallingConvention = backend.CallingConvention;

pub const VersionStr = backend.version_str;
pub const version = backend.version;

test "simple test" {
    _ = @import("zinc/Basic/CharInfo.zig");
    _ = @import("zinc/Basic/Compilation.zig");
    _ = @import("zinc/Basic/Target.zig");
    _ = @import("zinc/Driver/Distro.zig");
    _ = @import("zinc/Driver/GCCVersion.zig");
    _ = @import("zinc/Driver/Filesystem.zig");
    _ = @import("zinc/Lexer/Lexer.zig");
    _ = @import("zinc/Lexer/Preprocessor.zig");
    _ = @import("zinc/Parser/InitList.zig");
    _ = @import("zinc/AST/AST.zig");
    _ = @import("zinc/AST/Value.zig");
}
