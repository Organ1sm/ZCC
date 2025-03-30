//zinc-args -E -P
#define foo(X) 1 bar
#define bar(X) 2 foo
 
foo(X)(Y)(Z)
// 1 Bar(Y)(Z) -> 1 2 foo(Z)

