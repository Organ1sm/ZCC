long float a;
_Atomic(int) _Atomic(float) b;
enum Foo {};
_Atomic(int(void)) c;
_Atomic(int [3]) d;
_Atomic void e;
void f[4];
struct Bar f;
int x[2305843009213693951u];
_Complex long cl; /* TODO: complex integers extension */
_Complex z;
_Complex long int zi; /* TODO: complex integers extension */

#define TESTS_SKIPPED 6
#define EXPECTED_ERRORS \
    "invalid-types.c:1:6: error: cannot combine with previous 'long' specifier" \
    "invalid-types.c:2:14: warning: duplicate 'atomic' declaration specifier" \
    "invalid-types.c:2:14: error: cannot combine with previous 'int' specifier" \
    "invalid-types.c:3:11: error: empty enum is invalid" \
    "invalid-types.c:4:1: error: atomic cannot be applied to function type 'int (void)'" \
    "invalid-types.c:5:1: error: atomic cannot be applied to array type 'int [3]'" \
    "invalid-types.c:6:1: error: atomic cannot be applied to incomplete type 'void'" \
    "invalid-types.c:6:14: error: variable has incomplete type 'void'" \
    "invalid-types.c:7:7: error: array has incomplete element type 'void'" \
    "invalid-types.c:8:12: error: variable has incomplete type 'struct Bar'" \
    "invalid-types.c:9:6: error: array is too large" \
    "invalid-types.c:10:15: error: '_Complex long' is invalid" \
    "invalid-types.c:10:15: warning: type specifier missing, defaults to 'int'" \
    "invalid-types.c:11:1: warning: plain '_Complex' requires a type specifier; assuming '_Complex double'" \
    "invalid-types.c:12:15: error: cannot combine with previous '_Complex long' specifier" \
    "invalid-types.c:12:19: error: '_Complex long' is invalid" \
    "invalid-types.c:12:19: warning: type specifier missing, defaults to 'int'" \
