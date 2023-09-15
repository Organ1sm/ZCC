int foo(int bar) {
    (void)sizeof(struct Foo { int a; });
    (void)__alignof(struct Foo);
    return bar;
}

int fooo(bar, baz)
    float bar;
    int *baz;
    double quux;
{
    return *baz * bar;
}

int foooo(n, bar)
    int n;
    int bar[n];
{}

int bar(int) = foo;

int baz(int a[*]) {
    return a[0];
}

#define TESTS_SKIPPED 1
#define EXPECTED_ERRORS "functions.c:10:12: error: parameter named 'quux' is missing" \
    "functions.c:20:14: error: illegal initializer (only variables can be initialized)" \
    "functions.c:18:2: warning: non-void function 'foooo' does not return a value" \
    "functions.c:22:13: error: variable length array must be bound in function definition"
