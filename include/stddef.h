/* <stddef.h> for the ZCC C compiler */

#pragma once

typedef __PTRDIFF_TYPE__ ptrdiff_t;
typedef __SIZE_TYPE__ size_t;
typedef __WCHAR_TYPE__ wchar_t;

/* define max_align_t to match GCC and Clang */
typedef struct {
  long long __zcc_max_align_ll;
  long double __zcc_max_align_ld;
} max_align_t;

#define NULL ((void*)0)
#define offsetof(T, member) _builtin_offsetof(T, member)