//zinc-args -E -std=c99 -P

#define ZERO_ARGS(...) foo(a, ##__VA_ARGS__)

ZERO_ARGS()
ZERO_ARGS(b)

