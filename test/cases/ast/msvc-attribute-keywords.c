implicit typedef: '__int128'
 name: __int128_t

implicit typedef: 'unsigned __int128'
 name: __uint128_t

implicit typedef: '*char'
 name: __builtin_ms_va_list

implicit typedef: '*char'
 name: __builtin_va_list

implicit typedef: 'struct __NSConstantString_tag'
 name: __NSConstantString

variable: '*attributed(int)'
 name: a

variable: 'attributed(int)'
 attr: unaligned
 name: b

fnProto: 'kr (...) int'
 name: foo

fnProto: 'attributed(kr (...) *int)'
 attr: calling_convention cc: stdcall
 name: bar

fnProto: 'fn (decayed *[]attributed(int), decayed *attributed([]int)) int'
 name: baz

fnProto: 'fn (fn_ptr: *fn () void) void'
 name: quux

