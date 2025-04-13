# Zcc - Zinc C Compiler

A C compiler with the goal of providing fast compilation and low memory usage with good diagnostics.

Currently it can preprocess, parse and semantically analyze ~85% of standard C17 with
work still being needed to support all of the usual extensions.

Basic code generation is supported for x86-64 linux and can produce a valid hello world:

```sh-session
$ cat hello.c
extern int printf(const char *restrict fmt, ...);

int main() {
    printf("Hello, world!\n");
    return 0;
}
$ zig build run -- hello.c -o hello
$ ./hello
Hello, world!
$
```

```c
---
## Ignoring errors in tests
A test can ignore errors by defining a `NO_ERROR_VALIDATION` macro. This means the test will only fail if the
compilation panics / crashes. This is useful for testing bugs found by fuzzing, where there may be a large number of
uninteresting diagnostics.

**example:**
```c
#define NO_ERROR_VALIDATION
/* code goes here */
```
