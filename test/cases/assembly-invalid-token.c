void foo(void) {
    __asm__("" )
}

#define EXPECTED_ERRORS "assembly-invalid-token.c:2:15: error: expected ')', found 'invalid bytes'" \

