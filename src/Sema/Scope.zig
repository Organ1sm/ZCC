const AST = @import("../AST/AST.zig");

const TokenIndex = AST.TokenIndex;
const NodeIndex = AST.NodeIndex;

pub const Scope = union(enum) {
    typedef: Symbol,
    @"struct": Symbol,
    @"union": Symbol,
    @"enum": Symbol,
    symbol: Symbol,
    loop: Symbol,
    @"switch",

    pub const Symbol = struct {
        name: []const u8,
        node: NodeIndex,
        nameToken: TokenIndex,
    };
};
