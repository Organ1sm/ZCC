//zinc-args --target=x86_64-linux-gnu
void foo(__fp16 param) {

}

#define EXPECTED_ERRORS "fp16-parameter.c:2:17: error: parameters cannot have __fp16 type; did you forget * ?" \


