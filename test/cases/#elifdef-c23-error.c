//zinc-args -E -std=c23 -P
#define EXPECTED_ERRORS \
    "#elifdef-c23-error.c:8:9: error: macro name missing" \
    "#elifdef-c23-error.c:17:10: error: macro name missing"
#ifdef FOO
long long
#elifdef
long
#else
int
#endif

#define BAR
#ifdef FOO
long long
#elifndef
long
#else
int
#endif