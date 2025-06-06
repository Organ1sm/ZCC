void test_typeof_quals(void) {
    const int a;
    typeof(a) b;
    volatile int c;
    typeof(c) d;
    register int e;
    typeof(e) f;
    (void)&f;
    _Atomic int g;
    typeof(g) h;
    int *restrict i = 0;
    typeof(i) j;
}

#define EXPECTED_TYPES "const int" "const typeof(<expr>: const int)" \
    "volatile int" "volatile typeof(<expr>: volatile int)" \
    "int" "typeof(<expr>: int)" "void" \
    "_Atomic(int)" "typeof(<expr>: _Atomic(int))" \
    "restrict *int" "restrict typeof(<expr>: restrict *int)"

