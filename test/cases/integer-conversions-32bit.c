//zcc-args --target=x86-linux-gnu -Wno-unused-value -Wno-c2x-extensions
#include "include/test-helpers.h"

void foo(void) {
    EXPECT_TYPE(1U + 1L, unsigned long);
}
