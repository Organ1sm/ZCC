# ZCC
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


