# Features/syntax support

## Preprocessor

### Preprocessor directive

- [x] `#if`
- [x] `#endif`
- [x] `#elif`
- [x] `#else`
- [x] `#ifndef`
- [x] `#ifdef`
- [x] `#error`
- [x] `#warning`
- [x] `#define`
- [x] `#undef`
- [x] `#defined`
- [x] `#include`
- [x] `#include_next`
- [x] `#pragma`
- [x] `#embed`
- [ ] `#elifdef`
- [ ] `#elifndef`

### Builtin Macros

- [x] `__func__`
- [x] `__FUNCTION__`
- [x] `__PRETTY_FUNCTION__`

### Predefined Macros

- [x] `__STD_VERSION__`
- [x] `__DATE__`
- [x] `__TIME__`
- [x] `__TIMESTAMP__`
- [x] [`__LINE__`](../test/cases/__line__.c)
- [x] `__FILE__`
- [x] [`__COUNTER__`](../test/cases/line-counter.c)
- [x] `__VA_ARGS__`
- [x] `__has_attribute`
- [x] `__has_builtin`
- [x] `__has_extension`
- [x] `__has_feature`
- [x] `__has_include`

[There defined so many macros](https://github.com/Organ1sm/ZCC/blob/fbaff1263fc379bed20cb85c7099ea43f936dec8/src/Basic/Compilation.zig#L140)

### Pragmas

- **#pragma diagnostic**
  - [x] `#pragma GCC diagnostic warning`
  - [x] `#pragma GCC diagnostic error`
  - [x] `#pragma GCC diagnostic ignored`
  - [x] `#pragma GCC diagnostic push`
  - [x] `#pragma GCC diagnostic pop`

- [x] [`#pragma pack`](../test/cases/pragma-pack.c)
- [x] [`#pragma once`](../test/cases/include/global-var-once.h)
- [x] [`#pragma poison`](../test/cases/pragma-poison.c)

## Basic

- [x] `auto`
- [x] `signed, unsigned`
- [x] `void, char, int, short, long, long long, unsigned long long, float, double,`
- [x] `const, volatile`
- [x] `break, continue, goto`
- [x] `do, while, for`
- [x] `if, else`
- [x] `switch, case, default`
- [x] `extern`
- [x] `register`
- [x] `return`
- [x] [`sizeof`](../test/cases/sizeof-alignof.c)
- [x] `static`
- [x] `enum`
- [x] `struct, union`
- [x] `typedef`
- [x] comment `//` `/* asdas*/`

---

- **Fixed size int**

  - [x] `int64_t, uint64_t`
  - [x] `int32_t, uint32_t`
  - [x] `int16_t, uint16_t`
  - [x] `int8_t, uint8_t`

- **Number Suffix**

  - [x] [`i, fi, f`](../test/cases/arithmetic-conversion-floats.c)
  - [x] `u`, `ul`, `ull`, `iu`, `iul`, `iull`, `l`, `il`, `ll`, `ill`
  - [x] `f`, `if`
  - [x] [`wb, uwb, iwb, iuwb`](../test/cases/_BitInt.c)

- **Labels**
  - [x] `label:`

- [**BitFields**](../test/cases/bitfields.c)

```c
struct Foo
{
    int a : 1;
    int b : 2;
};
```

- [**Comma operator**](../test/cases/comma-operator.c)

```c
const int x = 0;
__typeof__((void) 0, x) y = 5;
```

- **Flexible Array**

- **Tentative Array**

- **Boolean Type**

- [**Type Casting**](../test/cases/cast-kinds.c)
  - `(type) value`
  - Implicit Conversion

- **String Literals**
  - [x] `L"nop"`

- [**Statements**](../test/cases/statements.c)

- **Allow tentative top-level records and enums**

- [**Main function implicit return zero**](../test/cases/implicit-main-return-zero.c)

- [**Newline splicing**](../test/cases/newline-splicing.c)

- [**Variadic functions**](../test/cases/stdarg.c)

```c
#include <stdarg.h>

void foo(int a, ...) 
{
    va_list va, new;
    va_start(va, a);
    int arg = va_arg(va, int);
    va_copy(va, new);
    va_end(va);
}
```

---

### c99

- [x] `_Bool`
- [x] `_Complex`
- [x] `_Imaginary`
- [x] [`_Pragma`](../test/cases/pragma-operator.c)
- [x] `Inline`
- [x] `restrict`
- [x] Flexible array members
- [x] Implicit `return 0;` in the `main()` function
- [ ] Variable-length array (VLA) types and variably-modified (VM) types
- [ ] Improvements of braced-init-list for array, struct and union types
  - [ ] Non-constant initializers
  - [ ] Designated initializers
- [x] Variadic macros

---

### c11

- [x] `_Alignas`
- [x] `_Alignof`
- [x] [`_Generic`](../test/cases/generic.c)
- [ ] `_Atomic`(support ast/sematic)
- [x] `_Thread_local`
- [x] `_Static_assert`
- [x] `_Noreturn`
- [x] `va_list, va_start, va_arg, va_copy, va_end`
- [x] Anonymous struct and union members
- [ ] Unicode support
  - [ ] u/U character constants
  - [ ] u8/u/U string literals

---

### c23

- [x] `alignof`
- [x] `alignas`
- [x] [`static_assert` and with no message](../test/cases/static-assert-c2x.c)
- [x] `thread_local`
- [x] `bool, true, false`
- [x] [`constexpr`](../test/cases/constexpr.c)
- [x] [`typeof`](../test/cases/parse-using-typeof-types.c)
- [x] `unreachable()`
- [x] [Bit-precise integers `_BitInt(N)` and suffixes](../test/cases/_BitInt.c)
- [x] Allow duplicate attributes

- [ ] Decimal floating-point types (`_Decimal32, _Decimal64, and _Decimal128`)
- [x] [Binary integer constants / Digit separator](../test/cases/c2x-digit-separators.c)

  `_Static_assert(0b1001'0110 == 150);`

- [x] [u8 character constants](../test/cases/u8-character-constant.c)

   `const unsigned char c2 = u8'™';`

- [x] `char8_t` [as the type of UTF-8 string literal](../test/cases/c2x-char8_t.c)

- [ ] Empty initializer `= {}`

- [x] **Attributes**

 1. [**standard attribute**](../test/cases/c23-attributes.c)
    - [x] [[deprecated]]
    - [x] [[fallthrough]]
    - [x] [[nodiscard]]
    - [x] [[maybe_unused]]
    - [x] [[noreturn]]
    - [x] [[reproducible]]
    - [x] [[unsequenced]]

 2. **attribute with a namespace**
    - [x] such as `[[gnu::unused]]`

 3. **standard attribute with arguments**
    - [x] such as `[[deprecated("reason")]] int x`

 4. **attribute with both a namespace and an argument list**
    - [x] such as `[[__gnu__::__aligned__(16)]] int x;`

- [x] [Unnamed parameters in function definitions](../test/cases/nameless-param.c)

```c
void bar(float, char) {
    return;
}
```

- [x] Single-argument _Static_assert
   `void foo(void) { static_assert(1); }`

- [x]  [`nullptr` constant and the associated `nullptr_t` type](../test/cases/nullptr.c)

---

## Extensions

### GCC

- [x] `__const, __const__`
- [x] `__inline, __inline__`
- [x] `__volatile, __volatile__`
- [x] `__restrict, __restrict__`
- [x] `__alignof, __alignof__`
- [x] `__extension__`
- [x] `asm, __asm, __asm__`
- [x] `__float80`
- [x] `__float128`
- [x] `__int128`
- [x] `__imag, __imag__`
- [x] `__real, __real__`
- [x] `__typeof__, __typeof`
- [x] `__attribute, __attribute__`
- [x] [`__auto_type` as type specifier](../test/cases/__auto_type.c)

- [x] [**Label as values / Computed goto statement**](../test/cases/address-of-label.c)

```c
void foo()
{
    int x = 5;
    void *y = &&baz;

    bar:
      y = &&bar;

    baz:
      x = 0;

    goto *y;
    goto *&&baz;
}
```

- [x] [**Binary Literal**](../test/cases/binary-literal.c)

  `0b0011110`

- [x] [**Cast to Union**](../test/cases/cast-to-union.c)

```c
union U {
    int x;
    float y;
};

void foo(void) {
    union U u;
    u = (union U)1;
}
```

- [x] [**Empty struct/union**](../test/cases/empty-records.c)

```c
#pragma GCC diagnostic warning "-Wgnu-empty-struct"

struct {} s;
union {} u;
```

- **Attribute Specifiers**
  - [x] `aligned, __aligned__`
  - [x] `access`
  - [x] `alias`
  - [x] `assume_aligned`
  - [x] `cold`
  - [x] `hot, __hot__`,
  - [x] `pure`
  - [x] `cleanup`
  - [x] `cleanup`
  - [x] `__const__`
  - [x] `simd`
  - [x] `deprecated`
  - [x] `packed`
  - [x] `unavailable`
  - [x] `noreturn, __noreturn__`
  - [x] `mode`
  - [x] `section`
  - [x] `unused, __unused__`
  - [x] `uninitialized`
  - [x] `fallthrough`
  - [x] `format`
  - [x] `vector_size`
  - [x] `designated_init`
  - [x] `transparent_union`

- [**Folding constant**](../test/cases/const-decl-folding.c)

```c
#pragma GCC diagnostic warning "-Wgnu-folding-constant"

const int x = 1;

struct S { int bits: x; };

void foo(void) 
{
    int array[] = {[x+10] = 1};
    _Static_assert(sizeof(array) == sizeof(array[0]) * (1 + 10 + 1), "wrong size");
}

void switch_fn(int param) 
{
    switch (param) {
        case x: return;
    }
}
```

- [**Statement expression**](../test/cases/statement-expr.c)

```c
void foo(void) 
{
    int y = ({
        int z = 5;
        z += 10;
        z;
    });
}
```

- [**Inline Assembly**](../test/cases/gnu-inline-assembly-statements.c)

    `__asm__ volatile`

```c
int add(void) 
{
    int src = 1;
    int dst;

    __asm__("mov %1, %0\n\t"
        "add $1, %0"
        : "=r" (dst)
        : "r" (src));

    return dst;
}
```

- **Built-in Functions**
  - [x] [`__has_builtin()`](../test/cases/__has_builtin.c)
  - [x] [`__has_attribute()`](../test/cases/__has_attribute.c)
  - [x] [`__has_extension()`](../test/cases/__has_extension.c)
  - [x] [`__has_feature()`](../test/cases/__has_feature.c)
  - [x] [`__has_include()`](../test/cases/__has_include.c)
  - [x] [`__has_warning()`](../test/cases/__has_warning.c)
  - [x] [`__is_identifier()`](../test/cases/__is_identifier.c)
  - [x] [`__builtin_types_compatible_p`](../test/cases/__builtin_types_compatible_p.c)
  - [x] [`__builtin_choose_expr`](../test/cases/builtin-choose-expr.c)
  - [x] [`__builtin_offsetof`](../test/cases/offsetof.c)
  - [x] [`__builtin_bitoffsetof`](../test/cases/offsetof.c)

- **Variadic Macros**

### clang

- [x] `__fp16`

- [**fixed enum**](../test/cases/enum-fixed.c)

```c
#pragma GCC diagnostic warning "-Wfixed-enum-extension"

enum E3: unsigned char {
    A = 254,
    B,
};

enum E6: char {
    a = 0u,
};
```

- [Allow initializer list initialization of complex numbers](../test/cases/complex-init.c)

### MSVC

- [x] `_int64, __int64`
- [x] `_int32, __int32`
- [x] `_int16, __int16`
- [x] `_int8, __int8`
- [ ] `__stdcall, _stdcall`
- [ ] `__thiscall, _thiscall`
- [ ] `__vectorcall, _vectorcall`
- [x] [`__declspec`](../test/cases/declspec.c)

```c
__declspec(align(16)) int bar;
```

- [**dollars in identifiers**](../test/cases/dollars-in-identifiers.c)

```c
void fib() {
  int $test;
}
```

- **zero size arrray**

---
