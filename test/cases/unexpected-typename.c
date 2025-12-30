typedef int foo;
void bar(void) {
    int a = foo;
}

#define EXPECTED_ERRORS "unexpected-typename.c:3:13: error: unexpected type name 'foo': expected expression" \


