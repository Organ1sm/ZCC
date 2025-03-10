//zinc-args --target=x86_64-macos
__float128 q = 0.0;

#define EXPECTED_ERRORS "__float128-darwin.c:2:1: error: __float128 is not supported on this target" \


