/* <stdbool.h> for the ZCC C compiler */

#pragma once

/* Todo: Set to 202311L once header is compliant with C23 */
#define __STDC_VERSION_STDDEF_H__ 0

#if __STDC_VERSION__ < 202311L
#define bool _Bool
#endif

#define true 1
#define false 0

#define __bool_true_false_are_defined 1
