//zinc-args -E -dD

#define CHECK_PARTIAL_MATCH

#define FOO 42
#define BAR FOO
int x = BAR;
#undef FOO
#define FOO 43
