#define FOO 1
#define FOO 2

#pragma GCC diagnostic error "-Wmacro-redefined"
#define BAR 1
#define BAR 2

#pragma GCC diagnostic ignored "-Wmacro-redefined"
#define BAZ 1
#define BAZ 2

// #pragma GCC diagnostic push
// #pragma GCC diagnostic pop

#define EXPECTED_ERRORS "adjust-diagnostic-level.c:2:9: warning: 'FOO' macro redefined" \
	"adjust-diagnostic-level.c:6:9: error: 'BAR' macro redefined"
