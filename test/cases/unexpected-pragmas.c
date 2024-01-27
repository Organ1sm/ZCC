int foo,
#pragma bar bar
int baz;
#pragma qux

#define EXPECTED_ERRORS "unexpected-pragmas.c:2:2: error: expected identifier or '('" \
	"unexpected-pragmas.c:2:2: error: expected identifier or '('" \
	"unexpected-pragmas.c:2:2: error: expected ';', found 'pragma'"

