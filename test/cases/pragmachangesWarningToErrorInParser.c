#pragma GCC diagnostic error "-Wint-conversion"
void foo(void) {
	int *x = 5;
}
#define EXPECTED_ERRORS "pragmachangesWarningToErrorInParser.c:3:11: error: implicit integer to pointer conversion from 'int' to 'int *' [-Werror, -Wint-conversion]" \
