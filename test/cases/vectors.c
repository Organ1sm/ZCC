//zinc-args --target=x86_64-linux-gnu
typedef float *invalid1 __attribute__((vector_size(8)));
typedef float invalid2 __attribute__((vector_size(9)));
typedef float f2v __attribute__((vector_size(8)));

void foo(void) {
    f2v a, b;
    a = b;
    a *= 2;
    (f2v)1;
}

#define EXPECTED_ERRORS "vectors.c:2:40: error: invalid vector element type 'float *'" \
    "vectors.c:3:39: error: vector size not an integral multiple of component size" \
    "vectors.c:10:5: error: cannot cast to non arithmetic or pointer type 'f2v'" \
