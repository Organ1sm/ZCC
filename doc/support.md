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
- [x] `__LINE__`
- [x] `__FILE__`
- [x] `__COUNTER__`
- [x] `__VA_ARGS__`
- [x] `__has_attribute`
- [x] `__has_builtin`
- [x] `__has_extension`
- [x] `__has_feature`
- [x] `__has_include`

[There defined so many macros](https://github.com/Organ1sm/ZCC/blob/fbaff1263fc379bed20cb85c7099ea43f936dec8/src/Basic/Compilation.zig#L140)

### Pragmas

- `#pragma diagnostic`
  - [x] `#pragma GCC diagnostic warning`
  - [x] `#pragma GCC diagnostic error`
  - [x] `#pragma GCC diagnostic ignored`
  - [x] `#pragma GCC diagnostic push`
  - [x] `#pragma GCC diagnostic pop`

- [x] [`#pragma pack`](https://github.com/Organ1sm/ZCC/blob/main/test/cases/pragma-pack.c)
- [x] [`#pragma once`](https://github.com/Organ1sm/ZCC/blob/main/test/cases/include/global-var-once.h)
- [x] [`#pragma poison`](https://github.com/Organ1sm/ZCC/blob/main/test/cases/pragma-poison.c)

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
- [x] `sizeof`
- [x] `static`
- [x] `enum`
- [x] `struct, union`
- [x] `typedef`
- [x] comment `//` `/* asdas*/`

- **Number Suffix**

- **Labels**
  - [x] `label:`

- **Flexible Array**

- **tentative array**

- **Boolean Type**

- **Type Casting**
  - `(type) value`
  - Implicit Conversion

- **String Literals**
  - [x] `L"nop"`

- **statement expression**

---

### c99

- [x] `_Bool`
- [x] `_Complex`
- [x] `_Imaginary`
- [x] `_Pragma`
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
- [x] `_Generic`
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
- [x] `static_assert`
- [x] `thread_local`
- [x] `bool, true, false`
- [x] `constexpr`
- [x] `typeof`
- [x] `unreachable()`
- [x] [Bit-precise integers `_BitInt(N)` and suffixes](../test/cases/_BitInt.c)
- [x] Allow duplicate attributes

- [ ] Decimal floating-point types (`_Decimal32, _Decimal64, and _Decimal128`)
- [x] Binary integer constants / Digit separator

  `_Static_assert(0b1001'0110 == 150);`

- [x] u8 character constants

   `u8"nop"`

- [x] `char8_t` as the type of UTF-8 string literal

- [ ] Empty initializer `= {}`

- [x] **Attributes**

 1. **standard attribute**
    - [x] [[deprecated]]
    - [x] [[fallthrough]]
    - [x] [[nodiscard]]
    - [x] [[maybe_unused]]
    - [x] [[noreturn]]
    - [x] [[reproducible]]

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

- [x]  `nullptr` constant and the associated `nullptr_t` type

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

- **Binary Literal**
  - [x] `0b11`

- **Empty struct/union**

```c
#pragma GCC diagnostic warning "-Wgnu-empty-struct"

struct {}s;
union {}u;
```

- **Attribute Specifiers**
  - [x] `aligned, __aligned__`
  - [x] `__alignof__`
  - [x] `access`
  - [x] `alias`
  - [x] `assume_aligned`
  - [x] `hot, pure`
  - [x] `cleanup`
  - [x] `cleanup`
  - [x] `__const__`
  - [x] `simd`
  - [x] `invalid_attribute`
  - [x] `deprecated`
  - [x] `cold`
  - [x] `packed`
  - [x] `unavailable`
  - [x] `noreturn, __noreturn__`
  - [x] `does_not_exit`
  - [x] `mode`
  - [x] `section`
  - [x] `__unused__`
  - [x] `fallthrough`
  - [x] `format`
  - [x] `vector_size`
  - [x] `designated_init`
  - [x] `transparent_union`

- **Folding constant**

```c
#pragma GCC diagnostic warning "-Wgnu-folding-constant"

const int x = 1;

struct S { int bits: x; };

void foo(void) {
    int array[] = {[x+10] = 1};
    _Static_assert(sizeof(array) == sizeof(array[0]) * (1 + 10 + 1), "wrong size");
}

void switch_fn(int param) {
    switch (param) {
        case x: return;
    }
}
```

- **Inline Assembly**

    `__asm__ volatile`

- **Built-in Functions**
  - [x] `__has_builtin()`
  - [x] `__has_attribute()`
  - [x] `__has_extension()`
  - [x] `__has_feature()`
  - [x] `__has_include()`
  - [x] `__has_warning()`
  - [x] `__is_identifier()`
  - [x] `__builtin_types_compatible_p`
  - [x] `__builtin_choose_expr`
  - [x] `__builtin_offsetof`
  - [x] `__builtin_bitoffsetof`

- **Variadic Macros**

### clang

- [x] `__fp16`

- **fixed enum**

```c
#pragma GCC diagnostic warning "-Wfixed-enum-extension"

enum E3: unsigned char {
    A = 255,
    B,
};
```

### MSVC

- [x] `_int64, __int64`
- [x] `_int32, __int32`
- [x] `_int16, __int16`
- [x] `_int8, __int8`
- [ ] `__stdcall, _stdcall`
- [ ] `__thiscall, _thiscall`
- [ ] `__vectorcall, _vectorcall`
- [x] `__declspec`

- **dollars in identifiers**

```c
void fib() {
  int $test;
}
```

- **zero size arrray**

---
