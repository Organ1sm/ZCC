//zcc-args -std=c23
#define EXPECTED_ERRORS \
    "missing-type-specifier-c2x.c:4:8: error: a type specifier is required for all declarations"
static x = 5;

