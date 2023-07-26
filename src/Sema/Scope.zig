const AST = @import("../AST/AST.zig");
const Parser = @import("../Parser/Parser.zig");

const TokenIndex = AST.TokenIndex;
const NodeIndex = AST.NodeIndex;
const Result = Parser.Result;

pub const Scope = union(enum) {
    typedef: Symbol,
    @"struct": Symbol,
    @"union": Symbol,
    @"enum": Symbol,
    symbol: Symbol,
    enumeration: Enumeration,
    loop,
    @"switch",

    pub const Symbol = struct {
        name: []const u8,
        node: NodeIndex,
        nameToken: TokenIndex,
    };

    pub const Enumeration = struct {
        name: []const u8,
        value: Result,
    };
};
