//zcc-args -fdeclspec

__declspec(align) int foo;
__declspec(align(4)) int foo;
__declspec(aligned(4)) int bar;

#define EXPECTED_ERRORS "declspec.c:5:12: warning: __declspec attribute 'aligned' is not supported [-Wignored-attributes]" \
