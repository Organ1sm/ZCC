pub const CodeGen = @import("zinc/CodeGen/CodeGen.zig");
pub const Compilation = @import("zinc/Basic/Compilation.zig");
pub const Diagnostics = @import("zinc/Basic/Diagnostics.zig");
pub const Driver = @import("zinc/Driver.zig");
pub const Lexer = @import("zinc/Lexer/Lexer.zig");
pub const Parser = @import("zinc/Parser/Parser.zig");
pub const Preprocessor = @import("zinc/Lexer/Preprocessor.zig");
pub const Source = @import("zinc/Basic/Source.zig");
pub const StringInterner = @import("zinc/Basic/StringInterner.zig");
pub const Token = @import("zinc/Lexer/Token.zig").Token;
pub const Tree = @import("zinc/AST/AST.zig");
pub const TypeStore = @import("zinc/AST/TypeStore.zig");
pub const QualType = TypeStore.QualType;
pub const Type = TypeStore.Type;
pub const TargetUtil = @import("zinc/Basic/Target.zig");
pub const ToolChain = @import("zinc/Toolchain.zig");
pub const Value = @import("zinc/AST/Value.zig");

const backend = @import("backend");
pub const CallingConvention = backend.CallingConvention;
pub const IR = backend.Ir;
pub const Interner = backend.Interner;
pub const Object = backend.Object;
pub const Assembly = backend.Assembly;

pub const VersionStr = backend.VersionStr;
pub const version = backend.Version;

test "simple test" {
    _ = @import("zinc/AnnexG.zig");
    _ = @import("zinc/Basic/CharInfo.zig");
    _ = @import("zinc/Basic/Compilation.zig");
    _ = @import("zinc/Basic/Target.zig");
    _ = @import("zinc/Driver/Distro.zig");
    _ = @import("zinc/Driver/GCCVersion.zig");
    _ = @import("zinc/Lexer/Lexer.zig");
    _ = @import("zinc/Lexer/Preprocessor.zig");
    _ = @import("zinc/Parser/InitList.zig");
    _ = @import("zinc/AST/AST.zig");
    _ = @import("zinc/AST/Value.zig");
    _ = @import("zinc/Toolchains/Linux.zig");
}
