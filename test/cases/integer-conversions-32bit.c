//zinc-args --target=x86-linux-gnu -Wno-unused-value -Wno-c23-extensions
#include "include/test-helpers.h"

void foo(void) {
    EXPECT_TYPE(1U + 1L, unsigned long);
}
