//std=c99
void foo(void) {
  _Static_assert(__STDC_VERSION__ == 199901, "__STDC_VERSION__ is incorrect");
  (void)__DATE__;
  (void)__TIME__;
  (void)__TIMESTAMP__;
  (void)__CHAR_BIT__;
  (void)__SCHAR_MAX__;
  (void)__SHRT_MAX__;
  (void)__INT_MAX__;
  (void)__LONG_MAX__;
  (void)__LONG_LONG_MAX__;
  (void)__WCHAR_MAX__;
  (void)__SIZE_MAX__;
  (void)__PTRDIFF_MAX__;
  (void)__SIZEOF_FLOAT__;
  (void)__SIZEOF_DOUBLE__;
  (void)__SIZEOF_LONG_DOUBLE__;
  (void)__SIZEOF_SHORT__;
  (void)__SIZEOF_INT__;
  (void)__SIZEOF_LONG__;
  (void)__SIZEOF_LONG_LONG__;
  (void)__SIZEOF_POINTER__;
  (void)__SIZEOF_PTRDIFF_T__;
  (void)__SIZEOF_SIZE_T__;
  (void)__SIZEOF_WCHAR_T__;
}