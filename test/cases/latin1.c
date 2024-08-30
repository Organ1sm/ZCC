int main(void) {
    char *ÿ = "ABC";
    return 0;
}

#define EXPECTED_ERRORS "latin1.c:2:11: error: source file is not valid UTF-8"