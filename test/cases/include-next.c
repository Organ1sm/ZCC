//zinc-args -Wgnu-include-next -Wno-gnu-alignof-expression
#include <my-include.h> // test/cases/include/my_include.h
#include_next <stdalign.h>
#include_next "global-var.h"
#include_next <other.h> // test/cases/include/other.h
_Static_assert(FOO == 1, "Did not include correct file");
_Static_assert(BAR == 2, "Did not include_next correct file");
_Static_assert(alignof(multiple) == _Alignof(int), "#include_next quotes");
_Static_assert(OTHER_INCLUDED == 1, "OTHER_INCLUDED");
_Static_assert(NEXT_OTHER_INCLUDED == 1, "NEXT_OTHER_INCLUDED");
_Static_assert(HAS_INCLUDE_NEXT_WORKED == 1, "HAS_INCLUDE_NEXT_WORKED");

#if __has_include_next("stdalign.h")
#define TOP_LEVEL_INCLUDE_NEXT 1
#endif
_Static_assert(TOP_LEVEL_INCLUDE_NEXT == 1, "TOP_LEVEL_INCLUDE_NEXT");


#define EXPECTED_ERRORS \
	"my-include.h:3:2: warning: #include_next is a language extension [-Wgnu-include-next]" \
	"my-include.h:4:2: warning: #include_next is a language extension [-Wgnu-include-next]" \
	"include-next.c:3:2: warning: #include_next is a language extension [-Wgnu-include-next]" \
	"include-next.c:3:2: warning: #include_next in primary source file; will search from start of include path [-Winclude-next-outside-header]" \
	"include-next.c:4:2: warning: #include_next is a language extension [-Wgnu-include-next]" \
	"include-next.c:4:2: warning: #include_next in primary source file; will search from start of include path [-Winclude-next-outside-header]" \
	"include-next.c:5:2: warning: #include_next is a language extension [-Wgnu-include-next]" \
	"include-next.c:5:2: warning: #include_next in primary source file; will search from start of include path [-Winclude-next-outside-header]" \
	"include-next.c:13:5: warning: #include_next in primary source file; will search from start of include path [-Winclude-next-outside-header]" \

