void ns_constant_string(void) {
    const __NSConstantString *ns_str = __builtin___NSStringMakeConstantString("Hello, world!");
    (void)ns_str;
}
