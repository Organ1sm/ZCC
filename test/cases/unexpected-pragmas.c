int foo,
#pragma bar bar
int baz;
#pragma qux

#define EXPECTED_ERRORS "unexpected-pragmas.c:2:9: warning: unsupported #pragma directive 'bar' [-Wunsupported-pragma]" \
	"unexpected-pragmas.c:4:9: warning: unsupported #pragma directive 'qux' [-Wunsupported-pragma]" \
	"unexpected-pragmas.c:2:2: error: expected identifier or '('" \
	"unexpected-pragmas.c:2:2: error: expected identifier or '('" \
	"unexpected-pragmas.c:2:2: error: expected ';', found 'pragma'"

