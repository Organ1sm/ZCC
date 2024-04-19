const std = @import("std");
const Compilation = @import("Compilation.zig");
const Type = @import("../AST/Type.zig");

const Builtins = @This();

const Builtin = struct {
    /// The type specifier of the builtin.
    spec: Type.Specifier,
    /// The function type information for the builtin.
    funcType: Type.Function,
    /// Attributes that provide additional metadata about the behavior of the builtin.
    attrs: Attributes,

    /// Attributes is a packed struct that holds flags for various properties
    /// that a builtin function might have.
    ///
    /// Fields:
    /// - printf_like: Set to 1 if the function behaves like printf.
    /// - vprintf_like: Set to 1 if the function behaves like vprintf.
    /// - noreturn: Indicates that the function does not return.
    /// - libm: Indicates if the function is part of the math library.
    /// - libc: Indicates if the function is part of the C standard library.
    /// - returns_twice: Indicates if the function can return more than once.
    /// - eval_args: Indicates if the arguments to the function should be evaluated.
    const Attributes = packed struct {
        printf_like: u8 = 0,
        vprintf_like: u8 = 0,
        noreturn: bool = false,
        libm: bool = false,
        libc: bool = false,
        returns_twice: bool = false,
        eval_args: bool = true,
    };
};
const BuiltinMap = std.StringHashMapUnmanaged(Builtin);

_builtins: BuiltinMap = .{},
_params: []Type.Function.Param = &.{},

pub fn deinit(b: *Builtins, gpa: std.mem.Allocator) void {
    b._builtins.deinit(gpa);
    gpa.free(b._params);
}

/// Adds a new builtin function to the BuiltinMap with the specified properties.
/// This function allocates memory for the function parameters and initializes them.
/// It then inserts the builtin function into the BuiltinMap.
///
/// Params:
///   a: Allocator used to allocate memory for the function parameters.
///   b: Pointer to the BuiltinMap to which the new builtin function will be added.
///   name: The name of the builtin function as a slice of bytes.
///   returnType: The return type of the builtin function.
///   paramTypes: A slice of Types representing the parameter types of the function.
///   spec: The type specifier for the builtin function.
///   attrs: Attributes that provide additional metadata about the behavior of the builtin function.
fn add(
    a: std.mem.Allocator,
    b: *BuiltinMap,
    name: []const u8,
    returnType: Type,
    paramTypes: []const Type,
    spec: Type.Specifier,
    attrs: Builtin.Attributes,
) void {
    var params = a.alloc(Type.Function.Param, paramTypes.len) catch unreachable;
    for (paramTypes, 0..) |paramTy, i| {
        params[i] = .{ .nameToken = 0, .ty = paramTy, .name = .empty };
    }
    b.putAssumeCapacity(name, .{
        .spec = spec,
        .funcType = .{
            .returnType = returnType,
            .params = params,
        },
        .attrs = attrs,
    });
}

pub fn create(comp: *Compilation) !Builtins {
    const builtinCount = 3;
    const paramCount = 5;

    var b = BuiltinMap{};
    try b.ensureTotalCapacity(comp.gpa, builtinCount);
    errdefer b.deinit(comp.gpa);

    const _params = try comp.gpa.alloc(Type.Function.Param, paramCount);
    errdefer comp.gpa.free(_params);

    var fib_state = std.heap.FixedBufferAllocator.init(std.mem.sliceAsBytes(_params));
    const a = fib_state.allocator();

    const voidType = Type.Void;
    var vaList = comp.types.vaList;
    if (vaList.isArray()) vaList.decayArray();

    add(a, &b, "__builtin_va_start", voidType, &.{ vaList, .{ .specifier = .SpecialVaStart } }, .Func, .{});
    add(a, &b, "__builtin_va_end", voidType, &.{vaList}, .Func, .{});
    add(a, &b, "__builtin_va_copy", voidType, &.{ vaList, vaList }, .Func, .{});

    return Builtins{ ._builtins = b, ._params = _params };
}

pub fn hasBuiltin(b: Builtins, name: []const u8) bool {
    if (std.mem.eql(u8, name, "__builtin_va_arg") or
        std.mem.eql(u8, name, "__builtin_choose_expr") or
        std.mem.eql(u8, name, "__builtin_bitoffsetof") or
        std.mem.eql(u8, name, "__builtin_offsetof"))
        return true;
    return b._builtins.getPtr(name) != null;
}

pub fn get(b: Builtins, name: []const u8) ?Type {
    const builtin = b._builtins.getPtr(name) orelse return null;
    return Type{
        .specifier = builtin.spec,
        .data = .{ .func = &builtin.funcType },
    };
}
