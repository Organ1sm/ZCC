#define FOO 1

#include_next <my-include.h> // test/cases/include/next/my-include.h
#include_next <other.h>  // test/cases/include/next/other.h

#if __has_include_next(<foobar.h>)
#error "Should not exist"
#endif

#if __has_include_next(<my-include.h>)
#define HAS_INCLUDE_NEXT_WORKED 1
#endif

#if __has_include_next("global-var.h")
#error "Should not find this with include_next"
#endif