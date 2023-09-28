void foo(void) {
    int a, b[a] = {};
    (int(int)){};
    int *c = 1.f;
    int d = {};
    int e = {1, 2};
    const char f[] = {"foo", "bar"};
    const char g[2] = "foo";
    // int h[2] = (int []){1,2,3};
    int i[] = "foo";
    int j = { [0] = 1 } // TODO incorrect expected external declaration ;
    int k[2] = { [-1] = 1 };
    int l[2] = { [2] = 1 };
    #define N 1000000000
    int m[] = { [N] = 1, [N] = 2 };
    int n = { .foo = 1 };
    struct Foo {
        int a;
        int b;
        int c;
    } a = { .b = 1, 2 };
    struct Foo b = { .d = 0 };
}

#define TESTS_SKIPPED 2
#define EXPECTED_ERRORS "initializers.c:2:17: error: variable-sized object may not be initialized" \
    "initializers.c:3:15: error: illegal initializer type" \
    "initializers.c:4:14: error: initializing 'int *' from incompatible type 'float'" \
    "initializers.c:5:13: error: scalar initializer cannot be empty" \
    "initializers.c:6:17: warning: excess elements in scalar initializer" \
    "initializers.c:7:30: warning: excess elements in string initializer" \
    "initializers.c:8:23: warning: initializer-string for char array is too long" \
    // "initializers.c:9:16: error: cannot initialize type ('int [2]' with array of type 'int [3]')"
    "initializers.c:10:15: error: cannot initialize array of type 'int []' with array of type 'char [4]'" \
    "initializers.c:11:15: error: array designator used for non-array type 'int'" \
    "initializers.c:12:19: error: array designator value -1 is negative" \
    "initializers.c:13:19: error: array designator index 2 exceeds array bounds" \
    "initializers.c:15:32: warning: initializer overrides previous initialization" \
    "initializers.c:15:23: note: previous initialization" \
    "initializers.c:16:15: error: field designator used for non-record type 'int'" \
    "initializers.c:22:22: error: record type has no field named 'd'"
