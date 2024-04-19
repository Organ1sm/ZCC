#include "correct-guard.h"
#undef CORRECT_GUARD_H
#include "correct-guard.h"

#define EXPECTED_ERRORS \
	"correct-guard.h:3:5: error: redefinition of 'x'" \
	"correct-guard.h:3:5: note: previous definition is here" \


