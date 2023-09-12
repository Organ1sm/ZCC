const std = @import("std");
const AST = @import("../AST/AST.zig");
const Type = @import("../AST/Type.zig");
const Parser = @import("../Parser/Parser.zig");
const Result = @import("../Parser/Result.zig");


const TokenIndex = AST.TokenIndex;
const NodeIndex = AST.NodeIndex;

pub const Scope = union(enum) {
    typedef: Symbol,
    @"struct": Symbol,
    @"union": Symbol,
    @"enum": Symbol,
    symbol: Symbol,
    declaration: Symbol,
    definition: Symbol,
    param: Symbol,
    enumeration: Enumeration,
    loop,
    @"switch": *Switch,
    block,

    pub const Symbol = struct {
        name: []const u8,
        type: Type,
        nameToken: TokenIndex,
    };

    pub const Enumeration = struct {
        name: []const u8,
        value: Result,
        nameToken: TokenIndex,
    };

    pub const Switch = struct {
        cases: CaseMap,
        default: ?Case = null,

        const ResultContext = struct {
            pub fn eql(_: ResultContext, a: Result, b: Result) bool {
                return a.eql(b);
            }
            pub fn hash(_: ResultContext, a: Result) u64 {
                return a.hash();
            }
        };

        pub const CaseMap = std.HashMap(Result, Case, ResultContext, std.hash_map.default_max_load_percentage);
        const Case = struct {
            node: NodeIndex,
            token: TokenIndex,
        };
    };
};
