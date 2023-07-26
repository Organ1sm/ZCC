const std = @import("std");
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
    @"switch": *Switch,

    pub const Symbol = struct {
        name: []const u8,
        node: NodeIndex,
        nameToken: TokenIndex,
    };

    pub const Enumeration = struct {
        name: []const u8,
        value: Result,
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