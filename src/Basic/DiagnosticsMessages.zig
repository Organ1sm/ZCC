const DiagnosticsMessages = @This();

pub const todo = struct { // Maybe someday this will no longer be needed.
    pub const msg = "TODO: {s}";
    pub const extra = .str;
    pub const kind = .@"error";
};
pub const error_directive = struct {
    pub const msg = "{s}";
    pub const extra = .str;
    pub const kind = .@"error";
};
pub const warning_directive = struct {
    pub const msg = "{s}";
    pub const opt = "#warnings";
    pub const extra = .str;
    pub const kind = .warning;
};
pub const elif_without_if = struct {
    pub const msg = "#elif without #if";
    pub const kind = .@"error";
};
pub const elif_after_else = struct {
    pub const msg = "#elif after #else";
    pub const kind = .@"error";
};
pub const else_without_if = struct {
    pub const msg = "#else without #if";
    pub const kind = .@"error";
};
pub const else_after_else = struct {
    pub const msg = "#else after #else";
    pub const kind = .@"error";
};
pub const endif_without_if = struct {
    pub const msg = "#endif without #if";
    pub const kind = .@"error";
};
pub const unknown_pragma = struct {
    pub const msg = "unknown pragma ignored";
    pub const opt = "unknown-pragmas";
    pub const kind = .off;
    pub const all = true;
};
pub const line_simple_digit = struct {
    pub const msg = "#line directive requires a simple digit sequence";
    pub const kind = .@"error";
};
pub const line_invalid_filename = struct {
    pub const msg = "invalid filename for #line directive";
    pub const kind = .@"error";
};
pub const unterminated_conditional_directive = struct {
    pub const msg = "unterminated conditional directive";
    pub const kind = .@"error";
};
pub const invalid_preprocessing_directive = struct {
    pub const msg = "invalid preprocessing directive";
    pub const kind = .@"error";
};
pub const macro_name_missing = struct {
    pub const msg = "macro name missing";
    pub const kind = .@"error";
};
pub const extra_tokens_directive_end = struct {
    pub const msg = "extra tokens at end of macro directive";
    pub const kind = .@"error";
};
pub const expected_value_in_expr = struct {
    pub const msg = "expected value in expression";
    pub const kind = .@"error";
};
pub const closing_paren = struct {
    pub const msg = "expected closing ')'";
    pub const kind = .@"error";
};
pub const to_match_paren = struct {
    pub const msg = "to match this '('";
    pub const kind = .note;
};
pub const to_match_brace = struct {
    pub const msg = "to match this '{'";
    pub const kind = .note;
};
pub const to_match_bracket = struct {
    pub const msg = "to match this '['";
    pub const kind = .note;
};
pub const header_str_closing = struct {
    pub const msg = "expected closing '>'";
    pub const kind = .@"error";
};
pub const header_str_match = struct {
    pub const msg = "to match this '<'";
    pub const kind = .note;
};
pub const string_literal_in_pp_expr = struct {
    pub const msg = "string literal in preprocessor expression";
    pub const kind = .@"error";
};
pub const float_literal_in_pp_expr = struct {
    pub const msg = "floating point literal in preprocessor expression";
    pub const kind = .@"error";
};
pub const defined_as_macro_name = struct {
    pub const msg = "'defined' cannot be used as a macro name";
    pub const kind = .@"error";
};
pub const macro_name_must_be_identifier = struct {
    pub const msg = "macro name must be an identifier";
    pub const kind = .@"error";
};
pub const whitespace_after_macro_name = struct {
    pub const msg = "ISO C99 requires whitespace after the macro name";
    pub const opt = "c99-extensions";
    pub const kind = .warning;
};
pub const hash_hash_at_start = struct {
    pub const msg = "'##' cannot appear at the start of a macro expansion";
    pub const kind = .@"error";
};
pub const hash_hash_at_end = struct {
    pub const msg = "'##' cannot appear at the end of a macro expansion";
    pub const kind = .@"error";
};
pub const pasting_formed_invalid = struct {
    pub const msg = "pasting formed '{s}', an invalid preprocessing token";
    pub const extra = .str;
    pub const kind = .@"error";
};
pub const missing_paren_param_list = struct {
    pub const msg = "missing ')' in macro parameter list";
    pub const kind = .@"error";
};
pub const unterminated_macro_param_list = struct {
    pub const msg = "unterminated macro param list";
    pub const kind = .@"error";
};
pub const invalid_token_param_list = struct {
    pub const msg = "invalid token in macro parameter list";
    pub const kind = .@"error";
};
pub const expected_comma_param_list = struct {
    pub const msg = "expected comma in macro parameter list";
    pub const kind = .@"error";
};
pub const hash_not_followed_param = struct {
    pub const msg = "'#' is not followed by a macro parameter";
    pub const kind = .@"error";
};
pub const expected_filename = struct {
    pub const msg = "expected \"FILENAME\" or <FILENAME>";
    pub const kind = .@"error";
};
pub const empty_filename = struct {
    pub const msg = "empty filename";
    pub const kind = .@"error";
};
pub const expected_invalid = struct {
    pub const msg = "expected '{s}', found invalid bytes";
    pub const extra = .tok_id_expected;
    pub const kind = .@"error";
};
pub const expected_eof = struct {
    pub const msg = "expected '{s}' before end of file";
    pub const extra = .tok_id_expected;
    pub const kind = .@"error";
};
pub const expected_token = struct {
    pub const msg = "expected '{s}', found '{s}'";
    pub const extra = .tok_id;
    pub const kind = .@"error";
};
pub const expected_expr = struct {
    pub const msg = "expected expression";
    pub const kind = .@"error";
};
pub const expected_integer_constant_expr = struct {
    pub const msg = "expression is not an integer constant expression";
    pub const kind = .@"error";
};
pub const missing_type_specifier = struct {
    pub const msg = "type specifier missing, defaults to 'int'";
    pub const opt = "implicit-int";
    pub const kind = .warning;
    pub const all = true;
};
pub const multiple_storage_class = struct {
    pub const msg = "cannot combine with previous '{s}' declaration specifier";
    pub const extra = .str;
    pub const kind = .@"error";
};
pub const static_assert_failure = struct {
    pub const msg = "static assertion failed";
    pub const kind = .@"error";
};
pub const static_assert_failure_message = struct {
    pub const msg = "static assertion failed {s}";
    pub const extra = .str;
    pub const kind = .@"error";
};
pub const expected_type = struct {
    pub const msg = "expected a type";
    pub const kind = .@"error";
};
pub const cannot_combine_spec = struct {
    pub const msg = "cannot combine with previous '{s}' specifier";
    pub const extra = .str;
    pub const kind = .@"error";
};
pub const duplicate_declspec = struct {
    pub const msg = "duplicate '{s}' declaration specifier";
    pub const extra = .str;
    pub const opt = "duplicate-decl-specifier";
    pub const kind = .warning;
    pub const all = true;
};
pub const restrict_non_pointer = struct {
    pub const msg = "restrict requires a pointer or reference ('{s}' is invalid)";
    pub const extra = .str;
    pub const kind = .@"error";
};
pub const expected_external_decl = struct {
    pub const msg = "expected external declaration";
    pub const kind = .@"error";
};
pub const expected_ident_or_l_paren = struct {
    pub const msg = "expected identifier or '('";
    pub const kind = .@"error";
};
pub const missing_declaration = struct {
    pub const msg = "declaration does not declare anything";
    pub const opt = "missing-declaration";
    pub const kind = .warning;
};
pub const func_not_in_root = struct {
    pub const msg = "function definition is not allowed here";
    pub const kind = .@"error";
};
pub const illegal_initializer = struct {
    pub const msg = "illegal initializer (only variables can be initialized)";
    pub const kind = .@"error";
};
pub const extern_initializer = struct {
    pub const msg = "extern variable has initializer";
    pub const opt = "extern-initializer";
    pub const kind = .warning;
};
pub const spec_from_typedef = struct {
    pub const msg = "'{s}' came from typedef";
    pub const extra = .str;
    pub const kind = .note;
};
pub const type_is_invalid = struct {
    pub const msg = "'{s}' is invalid";
    pub const extra = .str;
    pub const kind = .@"error";
};
pub const param_before_var_args = struct {
    pub const msg = "ISO C requires a named parameter before '...'";
    pub const kind = .@"error";
};
pub const void_only_param = struct {
    pub const msg = "'void' must be the only parameter if specified";
    pub const kind = .@"error";
};
pub const void_param_qualified = struct {
    pub const msg = "'void' parameter cannot be qualified";
    pub const kind = .@"error";
};
pub const void_must_be_first_param = struct {
    pub const msg = "'void' must be the first parameter if specified";
    pub const kind = .@"error";
};
pub const invalid_storage_on_param = struct {
    pub const msg = "invalid storage class on function parameter";
    pub const kind = .@"error";
};
pub const threadlocal_non_var = struct {
    pub const msg = "_Thread_local only allowed on variables";
    pub const kind = .@"error";
};
pub const func_spec_non_func = struct {
    pub const msg = "'{s}' can only appear on functions";
    pub const extra = .str;
    pub const kind = .@"error";
};
pub const illegal_storage_on_func = struct {
    pub const msg = "illegal storage class on function";
    pub const kind = .@"error";
};
pub const illegal_storage_on_global = struct {
    pub const msg = "illegal storage class on global variable";
    pub const kind = .@"error";
};
pub const expected_stmt = struct {
    pub const msg = "expected statement";
    pub const kind = .@"error";
};
pub const func_cannot_return_func = struct {
    pub const msg = "function cannot return a function";
    pub const kind = .@"error";
};
pub const func_cannot_return_array = struct {
    pub const msg = "function cannot return an array";
    pub const kind = .@"error";
};
pub const undeclared_identifier = struct {
    pub const msg = "use of undeclared identifier '{s}'";
    pub const extra = .str;
    pub const kind = .@"error";
};
pub const not_callable = struct {
    pub const msg = "cannot call non function type '{s}'";
    pub const extra = .str;
    pub const kind = .@"error";
};
pub const unsupported_str_cat = struct {
    pub const msg = "unsupported string literal concatenation";
    pub const kind = .@"error";
};
pub const static_func_not_global = struct {
    pub const msg = "static functions must be global";
    pub const kind = .@"error";
};
pub const implicit_func_decl = struct {
    pub const msg = "implicit declaration of function '{s}' is invalid in C99";
    pub const extra = .str;
    pub const opt = "implicit-function-declaration";
    pub const kind = .warning;
    pub const all = true;
};
pub const unknown_builtin = struct {
    pub const msg = "use of unknown builtin '{s}'";
    pub const extra = .str;
    pub const opt = "implicit-function-declaration";
    pub const kind = .@"error";
    pub const all = true;
};

pub const expected_param_decl = struct {
    pub const msg = "expected parameter declaration";
    pub const kind = .@"error";
};
pub const invalid_old_style_params = struct {
    pub const msg = "identifier parameter lists are only allowed in function definitions";
    pub const kind = .@"error";
};
pub const expected_fn_body = struct {
    pub const msg = "expected function body after function declaration";
    pub const kind = .@"error";
};
pub const invalid_void_param = struct {
    pub const msg = "parameter cannot have void type";
    pub const kind = .@"error";
};
pub const unused_value = struct {
    pub const msg = "expression result unused";
    pub const opt = "unused-value";
    pub const kind = .warning;
    pub const all = true;
};
pub const continue_not_in_loop = struct {
    pub const msg = "'continue' statement not in a loop";
    pub const kind = .@"error";
};
pub const break_not_in_loop_or_switch = struct {
    pub const msg = "'break' statement not in a loop or a switch";
    pub const kind = .@"error";
};
pub const unreachable_code = struct {
    pub const msg = "unreachable code";
    pub const opt = "unreachable-code";
    pub const kind = .warning;
    pub const all = true;
};
pub const duplicate_label = struct {
    pub const msg = "duplicate label '{s}'";
    pub const extra = .str;
    pub const kind = .@"error";
};
pub const previous_label = struct {
    pub const msg = "previous definition of label '{s}' was here";
    pub const extra = .str;
    pub const kind = .note;
};
pub const undeclared_label = struct {
    pub const msg = "use of undeclared label '{s}'";
    pub const extra = .str;
    pub const kind = .@"error";
};
pub const case_not_in_switch = struct {
    pub const msg = "'{s}' statement not in a switch statement";
    pub const extra = .str;
    pub const kind = .@"error";
};
pub const duplicate_switch_case_signed = struct {
    pub const msg = "duplicate case value '{d}'";
    pub const extra = .signed;
    pub const kind = .@"error";
};
pub const duplicate_switch_case_unsigned = struct {
    pub const msg = "duplicate case value '{d}'";
    pub const extra = .unsigned;
    pub const kind = .@"error";
};
pub const multiple_default = struct {
    pub const msg = "multiple default cases in the same switch";
    pub const kind = .@"error";
};
pub const previous_case = struct {
    pub const msg = "previous case defined here";
    pub const kind = .note;
};
pub const expected_arguments = struct {
    pub const msg = "expected {d} argument(s) got {d}";
    pub const extra = .arguments;
    pub const kind = .@"error";
};
pub const expected_arguments_old = struct {
    pub const msg = expected_arguments.msg;
    pub const extra = .arguments;
    pub const kind = .warning;
};
pub const expected_at_least_arguments = struct {
    pub const msg = "expected at least {d} argument(s) got {d}";
    pub const extra = .arguments;
    pub const kind = .warning;
};
pub const invalid_static_star = struct {
    pub const msg = "'static' may not be used with an unspecified variable length array size";
    pub const kind = .@"error";
};
pub const static_non_param = struct {
    pub const msg = "'static' used outside of function parameters";
    pub const kind = .@"error";
};
pub const array_qualifiers = struct {
    pub const msg = "type qualifier in non parameter array type";
    pub const kind = .@"error";
};
pub const star_non_param = struct {
    pub const msg = "star modifier used outside of function parameters";
    pub const kind = .@"error";
};
pub const variable_len_array_file_scope = struct {
    pub const msg = "variable length arrays not allowed at file scope";
    pub const kind = .@"error";
};
pub const useless_static = struct {
    pub const msg = "'static' useless without a constant size";
    pub const kind = .warning;
    pub const w_extra = true;
};
pub const negative_array_size = struct {
    pub const msg = "array size must be 0 or greater";
    pub const kind = .@"error";
};
pub const array_incomplete_elem = struct {
    pub const msg = "array has incomplete element type '{s}'";
    pub const extra = .str;
    pub const kind = .@"error";
};
pub const array_func_elem = struct {
    pub const msg = "arrays cannot have functions as their element type";
    pub const kind = .@"error";
};
pub const static_non_outermost_array = struct {
    pub const msg = "'static' used in non-outermost array type";
    pub const kind = .@"error";
};
pub const qualifier_non_outermost_array = struct {
    pub const msg = "type qualifier used in non-outermost array type";
    pub const kind = .@"error";
};
pub const unterminated_macro_arg_list = struct {
    pub const msg = "unterminated function macro argument list";
    pub const kind = .@"error";
};
pub const unknown_warning = struct {
    pub const msg = "unknown warning '{s}'";
    pub const extra = .str;
    pub const opt = "unknown-warning-option";
    pub const kind = .warning;
};
pub const overflow_signed = struct {
    pub const msg = "overflow in expression; result is '{d}'";
    pub const extra = .signed;
    pub const opt = "integer-overflow";
    pub const kind = .warning;
};
pub const overflow_unsigned = struct {
    pub const msg = overflow_signed.msg;
    pub const extra = .unsigned;
    pub const opt = "integer-overflow";
    pub const kind = .warning;
};
pub const int_literal_too_big = struct {
    pub const msg = "integer literal is too large to be represented in any integer type";
    pub const kind = .@"error";
};
pub const indirection_ptr = struct {
    pub const msg = "indirection requires pointer operand";
    pub const kind = .@"error";
};
pub const addr_of_rvalue = struct {
    pub const msg = "cannot take the address of an rvalue";
    pub const kind = .@"error";
};
pub const not_assignable = struct {
    pub const msg = "expression is not assignable";
    pub const kind = .@"error";
};
pub const ident_or_l_brace = struct {
    pub const msg = "expected identifier or '{'";
    pub const kind = .@"error";
};
pub const empty_enum = struct {
    pub const msg = "empty enum is invalid";
    pub const kind = .@"error";
};
pub const redefinition = struct {
    pub const msg = "redefinition of '{s}'";
    pub const extra = .str;
    pub const kind = .@"error";
};
pub const previous_definition = struct {
    pub const msg = "previous definition is here";
    pub const kind = .note;
};
pub const expected_identifier = struct {
    pub const msg = "expected identifier";
    pub const kind = .@"error";
};
pub const expected_str_literal = struct {
    pub const msg = "expected string literal for diagnostic message in static_assert";
    pub const kind = .@"error";
};
pub const expected_str_literal_in = struct {
    pub const msg = "expected string literal in '{s}'";
    pub const extra = .str;
    pub const kind = .@"error";
};
pub const parameter_missing = struct {
    pub const msg = "parameter named '{s}' is missing";
    pub const extra = .str;
    pub const kind = .@"error";
};
pub const empty_record = struct {
    pub const msg = "empty {s} is a GNU extension";
    pub const extra = .str;
    pub const opt = "gnu-empty-struct";
    pub const kind = .off;
    pub const pedantic = true;
};
pub const empty_record_size = struct {
    pub const msg = "empty {s} has size 0 in C, size 1 in C++";
    pub const extra = .str;
    pub const opt = "c++-compat";
    pub const kind = .off;
};
pub const wrong_tag = struct {
    pub const msg = "use of '{s}' with tag type that does not match previous definition";
    pub const extra = .str;
    pub const kind = .@"error";
};
pub const expected_parens_around_typename = struct {
    pub const msg = "expected parentheses around type name";
    pub const kind = .@"error";
};
pub const alignof_expr = struct {
    pub const msg = "'_Alignof' applied to an expression is a GNU extension";
    pub const opt = "gnu-alignof-expression";
    pub const kind = .warning;
    pub const suppress_gnu = true;
};
pub const invalid_sizeof = struct {
    pub const msg = "invalid application of 'sizeof' to an incomplete type '{s}'";
    pub const extra = .str;
    pub const kind = .@"error";
};
pub const macro_redefined = struct {
    pub const msg = "'{s}' macro redefined";
    pub const extra = .str;
    pub const opt = "macro-redefined";
    pub const kind = .warning;
};
pub const generic_qual_type = struct {
    pub const msg = "generic association with qualifiers cannot be matched with";
    pub const opt = "generic-qual-type";
    pub const kind = .warning;
};
pub const generic_duplicate = struct {
    pub const msg = "type '{s}' in generic association compatible with previously specified type";
    pub const extra = .str;
    pub const kind = .@"error";
};
pub const generic_duplicate_default = struct {
    pub const msg = "duplicate default generic association";
    pub const kind = .@"error";
};
pub const generic_no_match = struct {
    pub const msg = "controlling expression type '{s}' not compatible with any generic association type";
    pub const extra = .str;
    pub const kind = .@"error";
};
pub const escape_sequence_overflow = struct {
    pub const msg = "escape sequence out of range";
    pub const kind = .@"error";
};
pub const invalid_universal_character = struct {
    pub const msg = "invalid universal character";
    pub const kind = .@"error";
};
pub const multichar_literal = struct {
    pub const msg = "multi-character character constant";
    pub const opt = "multichar";
    pub const kind = .warning;
    pub const all = true;
};
pub const unicode_multichar_literal = struct {
    pub const msg = "Unicode character literals may not contain multiple characters";
    pub const kind = .@"error";
};
pub const wide_multichar_literal = struct {
    pub const msg = "extraneous characters in character constant ignored";
    pub const kind = .warning;
};
pub const char_lit_too_wide = struct {
    pub const msg = "character constant too long for its type";
    pub const kind = .warning;
    pub const all = true;
};
pub const char_too_large = struct {
    pub const msg = "character too large for enclosing character literal type";
    pub const kind = .@"error";
};
pub const must_use_struct = struct {
    pub const msg = "must use 'struct' tag to refer to type '{s}'";
    pub const extra = .str;
    pub const kind = .@"error";
};
pub const must_use_union = struct {
    pub const msg = "must use 'union' tag to refer to type '{s}'";
    pub const extra = .str;
    pub const kind = .@"error";
};
pub const must_use_enum = struct {
    pub const msg = "must use 'enum' tag to refer to type '{s}'";
    pub const extra = .str;
    pub const kind = .@"error";
};
pub const redefinition_different_sym = struct {
    pub const msg = "redefinition of '{s}' as different kind of symbol";
    pub const extra = .str;
    pub const kind = .@"error";
};
pub const redefinition_incompatible = struct {
    pub const msg = "redefinition of '{s}' with a different type";
    pub const extra = .str;
    pub const kind = .@"error";
};
pub const redefinition_of_parameter = struct {
    pub const msg = "redefinition of parameter '{s}'";
    pub const extra = .str;
    pub const kind = .@"error";
};
pub const invalid_bin_types = struct {
    pub const msg = "invalid operands to binary expression ({s})";
    pub const extra = .str;
    pub const kind = .@"error";
};
pub const comparison_ptr_int = struct {
    pub const msg = "comparison between pointer and integer ({s})";
    pub const extra = .str;
    pub const opt = "pointer-integer-compare";
    pub const kind = .warning;
};
pub const comparison_distinct_ptr = struct {
    pub const msg = "comparison of distinct pointer types ({s})";
    pub const extra = .str;
    pub const opt = "compare-distinct-pointer-types";
    pub const kind = .warning;
};
pub const incompatible_pointers = struct {
    pub const msg = "incompatible pointer types ({s})";
    pub const extra = .str;
    pub const kind = .@"error";
};
pub const invalid_argument_un = struct {
    pub const msg = "invalid argument type '{s}' to unary expression";
    pub const extra = .str;
    pub const kind = .@"error";
};
pub const incompatible_assign = struct {
    pub const msg = "assignment to {s}";
    pub const extra = .str;
    pub const kind = .@"error";
};
pub const implicit_ptr_to_int = struct {
    pub const msg = "implicit pointer to integer conversion from {s}";
    pub const extra = .str;
    pub const opt = "int-conversion";
    pub const kind = .warning;
};
pub const invalid_cast_to_float = struct {
    pub const msg = "pointer cannot be cast to type '{s}'";
    pub const extra = .str;
    pub const kind = .@"error";
};
pub const invalid_cast_to_pointer = struct {
    pub const msg = "operand of type '{s}' cannot be cast to a pointer type";
    pub const extra = .str;
    pub const kind = .@"error";
};
pub const invalid_cast_type = struct {
    pub const msg = "cannot cast to non arithmetic or pointer type '{s}'";
    pub const extra = .str;
    pub const kind = .@"error";
};
pub const qual_cast = struct {
    pub const msg = "cast to type '{s}' will not preserve qualifiers";
    pub const extra = .str;
    pub const opt = "cast-qualifiers";
    pub const kind = .warning;
};
pub const invalid_index = struct {
    pub const msg = "array subscript is not an integer";
    pub const kind = .@"error";
};
pub const invalid_subscript = struct {
    pub const msg = "subscripted value is not an array or pointer";
    pub const kind = .@"error";
};
pub const array_after = struct {
    pub const msg = "array index {d} is past the end of the array";
    pub const extra = .unsigned;
    pub const opt = "array-bounds";
    pub const kind = .warning;
};
pub const array_before = struct {
    pub const msg = "array index {d} is before the beginning of the array";
    pub const extra = .signed;
    pub const opt = "array-bounds";
    pub const kind = .warning;
};
pub const statement_int = struct {
    pub const msg = "statement requires expression with integer type ('{s}' invalid)";
    pub const extra = .str;
    pub const kind = .@"error";
};
pub const statement_scalar = struct {
    pub const msg = "statement requires expression with scalar type ('{s}' invalid)";
    pub const extra = .str;
    pub const kind = .@"error";
};
pub const func_should_return = struct {
    pub const msg = "non-void function '{s}' should return a value";
    pub const extra = .str;
    pub const opt = "return-type";
    pub const kind = .@"error";
};
pub const incompatible_return = struct {
    pub const msg = "returning {s}";
    pub const extra = .str;
    pub const kind = .@"error";
};
pub const implicit_int_to_ptr = struct {
    pub const msg = "implicit integer to pointer conversion from {s}";
    pub const extra = .str;
    pub const opt = "int-conversion";
    pub const kind = .warning;
};
pub const func_does_not_return = struct {
    pub const msg = "non-void function '{s}' does not return a value";
    pub const extra = .str;
    pub const opt = "return-type";
    pub const kind = .warning;
    pub const all = true;
};
pub const void_func_returns_value = struct {
    pub const msg = "void function '{s}' should not return a value";
    pub const extra = .str;
    pub const opt = "return-type";
    pub const kind = .@"error";
    pub const all = true;
};
pub const incompatible_arg = struct {
    pub const msg = "passing {s}";
    pub const extra = .str;
    pub const kind = .@"error";
};
pub const parameter_here = struct {
    pub const msg = "passing argument to parameter here";
    pub const kind = .note;
};
pub const atomic_array = struct {
    pub const msg = "atomic cannot be applied to array type '{s}'";
    pub const extra = .str;
    pub const kind = .@"error";
};
pub const atomic_func = struct {
    pub const msg = "atomic cannot be applied to function type '{s}'";
    pub const extra = .str;
    pub const kind = .@"error";
};
pub const atomic_incomplete = struct {
    pub const msg = "atomic cannot be applied to incomplete type '{s}'";
    pub const extra = .str;
    pub const kind = .@"error";
};
pub const addr_of_register = struct {
    pub const msg = "address of register variable requested";
    pub const kind = .@"error";
};
pub const variable_incomplete_ty = struct {
    pub const msg = "variable has incomplete type '{s}'";
    pub const extra = .str;
    pub const kind = .@"error";
};
pub const parameter_incomplete_ty = struct {
    pub const msg = "parameter has incomplete type '{s}'";
    pub const extra = .str;
    pub const kind = .@"error";
};
pub const deref_incomplete_ty_ptr = struct {
    pub const msg = "dereferencing pointer to incomplete type '{s}'";
    pub const extra = .str;
    pub const kind = .@"error";
};
pub const alignas_on_func = struct {
    pub const msg = "'_Alignas' attribute only applies to variables and fields";
    pub const kind = .@"error";
};
pub const alignas_on_param = struct {
    pub const msg = "'_Alignas' attribute cannot be applied to a function parameter";
    pub const kind = .@"error";
};
pub const minimum_alignment = struct {
    pub const msg = "requested alignment is less than minimum alignment of {d}";
    pub const extra = .unsigned;
    pub const kind = .@"error";
};
pub const maximum_alignment = struct {
    pub const msg = "requested alignment of {d} is too large";
    pub const extra = .unsigned;
    pub const kind = .@"error";
};
pub const negative_alignment = struct {
    pub const msg = "requested negative alignment of {d} is invalid";
    pub const extra = .signed;
    pub const kind = .@"error";
};
pub const align_ignored = struct {
    pub const msg = "'_Alignas' attribute is ignored here";
    pub const kind = .warning;
};
pub const zero_align_ignored = struct {
    pub const msg = "requested alignment of zero is ignored";
    pub const kind = .warning;
};
pub const non_pow2_align = struct {
    pub const msg = "requested alignment is not a power of 2";
    pub const kind = .@"error";
};
pub const pointer_mismatch = struct {
    pub const msg = "pointer type mismatch ({s})";
    pub const extra = .str;
    pub const opt = "pointer-type-mismatch";
    pub const kind = .warning;
};
pub const static_assert_not_constant = struct {
    pub const msg = "static_assert expression is not an integral constant expression";
    pub const kind = .@"error";
};
pub const static_assert_missing_message = struct {
    pub const msg = "static_assert with no message is a C2X extension";
    pub const opt = "c2x-extensions";
    pub const kind = .warning;
    pub const suppress_version = .c2x;
};
pub const unbound_vla = struct {
    pub const msg = "variable length array must be bound in function definition";
    pub const kind = .@"error";
};
pub const array_too_large = struct {
    pub const msg = "array is too large";
    pub const kind = .@"error";
};
pub const incompatible_ptr_init = struct {
    pub const msg = "incompatible pointer types initializing {s}";
    pub const extra = .str;
    pub const opt = "incompatible-pointer-types";
    pub const kind = .warning;
};
pub const incompatible_ptr_assign = struct {
    pub const msg = "incompatible pointer types assigning to {s}";
    pub const extra = .str;
    pub const opt = "incompatible-pointer-types";
    pub const kind = .warning;
};
pub const vla_init = struct {
    pub const msg = "variable-sized object may not be initialized";
    pub const kind = .@"error";
};
pub const func_init = struct {
    pub const msg = "illegal initializer type";
    pub const kind = .@"error";
};
pub const incompatible_init = struct {
    pub const msg = "initializing {s}";
    pub const extra = .str;
    pub const kind = .@"error";
};
pub const empty_scalar_init = struct {
    pub const msg = "scalar initializer cannot be empty";
    pub const kind = .@"error";
};
pub const excess_scalar_init = struct {
    pub const msg = "excess elements in scalar initializer";
    pub const opt = "excess-initializers";
    pub const kind = .warning;
};
pub const excess_str_init = struct {
    pub const msg = "excess elements in string initializer";
    pub const opt = "excess-initializers";
    pub const kind = .warning;
};
pub const excess_struct_init = struct {
    pub const msg = "excess elements in struct initializer";
    pub const opt = "excess-initializers";
    pub const kind = .warning;
};
pub const excess_array_init = struct {
    pub const msg = "excess elements in array initializer";
    pub const opt = "excess-initializers";
    pub const kind = .warning;
};
pub const str_init_too_long = struct {
    pub const msg = "initializer-string for char array is too long";
    pub const opt = "excess-initializers";
    pub const kind = .warning;
};
pub const arr_init_too_long = struct {
    pub const msg = "cannot initialize type ({s})";
    pub const extra = .str;
    pub const kind = .@"error";
};
pub const invalid_typeof = struct {
    pub const msg = "'{s} typeof' is invalid";
    pub const extra = .str;
    pub const kind = .@"error";
};
pub const division_by_zero = struct {
    pub const msg = "{s} by zero is undefined";
    pub const extra = .str;
    pub const opt = "division-by-zero";
    pub const kind = .warning;
};
pub const division_by_zero_macro = struct {
    pub const msg = "{s} by zero in preprocessor expression";
    pub const extra = .str;
    pub const kind = .@"error";
};
pub const builtin_choose_cond = struct {
    pub const msg = "'__builtin_choose_expr' requires a constant expression";
    pub const kind = .@"error";
};
pub const alignas_unavailable = struct {
    pub const msg = "'_Alignas' attribute requires integer constant expression";
    pub const kind = .@"error";
};
pub const case_val_unavailable = struct {
    pub const msg = "case value must be an integer constant expression";
    pub const kind = .@"error";
};
pub const enum_val_unavailable = struct {
    pub const msg = "enum value must be an integer constant expression";
    pub const kind = .@"error";
};
pub const incompatible_array_init = struct {
    pub const msg = "cannot initialize array of type {s}";
    pub const extra = .str;
    pub const kind = .@"error";
};
pub const array_init_str = struct {
    pub const msg = "array initializer must be an initializer list or wide string literal";
    pub const kind = .@"error";
};
pub const initializer_overrides = struct {
    pub const msg = "initializer overrides previous initialization";
    pub const opt = "initializer-overrides";
    pub const kind = .warning;
    pub const w_extra = true;
};
pub const previous_initializer = struct {
    pub const msg = "previous initialization";
    pub const kind = .note;
};
pub const invalid_array_designator = struct {
    pub const msg = "array designator used for non-array type '{s}'";
    pub const extra = .str;
    pub const kind = .@"error";
};
pub const negative_array_designator = struct {
    pub const msg = "array designator value {d} is negative";
    pub const extra = .signed;
    pub const kind = .@"error";
};
pub const oob_array_designator = struct {
    pub const msg = "array designator index {d} exceeds array bounds";
    pub const extra = .unsigned;
    pub const kind = .@"error";
};
pub const invalid_field_designator = struct {
    pub const msg = "field designator used for non-record type '{s}'";
    pub const extra = .str;
    pub const kind = .@"error";
};
pub const no_such_field_designator = struct {
    pub const msg = "record type has no field named '{s}'";
    pub const extra = .str;
    pub const kind = .@"error";
};
pub const empty_aggregate_init_braces = struct {
    pub const msg = "initializer for aggregate with no elements requires explicit braces";
    pub const kind = .@"error";
};
pub const ptr_init_discards_quals = struct {
    pub const msg = "initializing {s} discards qualifiers";
    pub const extra = .str;
    pub const opt = "incompatible-pointer-types-discards-qualifiers";
    pub const kind = .warning;
};
pub const ptr_assign_discards_quals = struct {
    pub const msg = "assigning to {s} discards qualifiers";
    pub const extra = .str;
    pub const opt = "incompatible-pointer-types-discards-qualifiers";
    pub const kind = .warning;
};
pub const ptr_ret_discards_quals = struct {
    pub const msg = "returning {s} discards qualifiers";
    pub const extra = .str;
    pub const opt = "incompatible-pointer-types-discards-qualifiers";
    pub const kind = .warning;
};
pub const ptr_arg_discards_quals = struct {
    pub const msg = "passing {s} discards qualifiers";
    pub const extra = .str;
    pub const opt = "incompatible-pointer-types-discards-qualifiers";
    pub const kind = .warning;
};
pub const unknown_attribute = struct {
    pub const msg = "unknown attribute '{s}' ignored";
    pub const extra = .str;
    pub const opt = "unknown-attributes";
    pub const kind = .warning;
};
pub const ignored_attribute = struct {
    pub const msg = "{s}";
    pub const extra = .str;
    pub const opt = "ignored-attributes";
    pub const kind = .warning;
};
pub const invalid_fallthrough = struct {
    pub const msg = "fallthrough annotation does not directly precede switch label";
    pub const kind = .@"error";
};
pub const cannot_apply_attribute_to_statement = struct {
    pub const msg = "'{s}' attribute cannot be applied to a statement";
    pub const extra = .str;
    pub const kind = .@"error";
};
pub const builtin_macro_redefined = struct {
    pub const msg = "redefining builtin macro";
    pub const opt = "builtin-macro-redefined";
    pub const kind = .warning;
};
pub const feature_check_requires_identifier = struct {
    pub const msg = "builtin feature check macro requires a parenthesized identifier";
    pub const kind = .@"error";
};
pub const missing_tok_builtin = struct {
    pub const msg = "missing '{s}', after builtin feature-check macro";
    pub const extra = .tok_id_expected;
    pub const kind = .@"error";
};
pub const gnu_label_as_value = struct {
    pub const msg = "use of GNU address-of-label extension";
    pub const opt = "gnu-label-as-value";
    pub const kind = .off;
    pub const pedantic = true;
};
pub const expected_record_ty = struct {
    pub const msg = "member reference base type '{s}' is not a structure or union";
    pub const extra = .str;
    pub const kind = .@"error";
};
pub const member_expr_not_ptr = struct {
    pub const msg = "member reference type '{s}' is not a pointer; did you mean to use '.'?";
    pub const extra = .str;
    pub const kind = .@"error";
};
pub const member_expr_ptr = struct {
    pub const msg = "member reference type '{s}' is a pointer; did you mean to use '->'?";
    pub const extra = .str;
    pub const kind = .@"error";
};
pub const no_such_member = struct {
    pub const msg = "no member named {s}";
    pub const extra = .str;
    pub const kind = .@"error";
};
pub const malformed_warning_check = struct {
    pub const msg = "{s} expected option name (e.g. \"-Wundef\")";
    pub const extra = .str;
    pub const opt = "malformed-warning-check";
    pub const kind = .warning;
    pub const all = true;
};
pub const invalid_computed_goto = struct {
    pub const msg = "computed goto in function with no address-of-label expressions";
    pub const kind = .@"error";
};
pub const pragma_warning_message = struct {
    pub const msg = "{s}";
    pub const extra = .str;
    pub const opt = "#pragma-messages";
    pub const kind = .warning;
};
pub const pragma_error_message = struct {
    pub const msg = "{s}";
    pub const extra = .str;
    pub const kind = .@"error";
};
pub const pragma_message = struct {
    pub const msg = "#pragma message: {s}";
    pub const extra = .str;
    pub const kind = .note;
};
pub const pragma_requires_string_literal = struct {
    pub const msg = "pragma {s} requires string literal";
    pub const extra = .str;
    pub const kind = .@"error";
};
pub const poisoned_identifier = struct {
    pub const msg = "attempt to use a poisoned identifier";
    pub const kind = .@"error";
};
pub const pragma_poison_identifier = struct {
    pub const msg = "can only poison identifier tokens";
    pub const kind = .@"error";
};
pub const pragma_poison_macro = struct {
    pub const msg = "poisoning existing macro";
    pub const kind = .warning;
};
pub const newline_eof = struct {
    pub const msg = "no newline at end of file";
    pub const opt = "newline-eof";
    pub const kind = .off;
    pub const pedantic = true;
};
pub const empty_translation_unit = struct {
    pub const msg = "ISO C requires a translation unit to contain at least one declaration";
    pub const opt = "empty-translation-unit";
    pub const kind = .off;
    pub const pedantic = true;
};
pub const omitting_parameter_name = struct {
    pub const msg = "omitting the parameter name in a function definition is a C2x extension";
    pub const opt = "c2x-extensions";
    pub const kind = .warning;
    pub const suppress_version = .c2x;
};
pub const non_int_bitfield = struct {
    pub const msg = "bit-field has non-integer type '{s}'";
    pub const extra = .str;
    pub const kind = .@"error";
};
pub const negative_bitwidth = struct {
    pub const msg = "bit-field has negative width ({d})";
    pub const extra = .signed;
    pub const kind = .@"error";
};
pub const zero_width_named_field = struct {
    pub const msg = "named bit-field has zero width";
    pub const kind = .@"error";
};
pub const bitfield_too_big = struct {
    pub const msg = "width of bit-field exceeds width of its type";
    pub const kind = .@"error";
};
pub const invalid_utf8 = struct {
    pub const msg = "source file is not valid UTF-8";
    pub const kind = .@"error";
};
pub const implicitly_unsigned_literal = struct {
    pub const msg = "integer literal is too large to be represented in a signed integer type, interpreting as unsigned";
    pub const opt = "implicitly-unsigned-literal";
    pub const kind = .warning;
};
pub const invalid_preproc_operator = struct {
    pub const msg = "token is not a valid binary operator in a preprocessor subexpression";
    pub const kind = .@"error";
};
pub const invalid_preproc_expr_start = struct {
    pub const msg = "invalid token at start of a preprocessor expression";
    pub const kind = .@"error";
};
pub const c99_compat = struct {
    pub const msg = "using this character in an identifier is incompatible with C99";
    pub const opt = "c99-compat";
    pub const kind = .off;
};
pub const unexpected_character = struct {
    pub const msg = "unexpected character <U+{X:0>4}>";
    pub const extra = .actual_codepoint;
    pub const kind = .@"error";
};
pub const invalid_identifier_start_char = struct {
    pub const msg = "character <U+{X:0>4}> not allowed at the start of an identifier";
    pub const extra = .actual_codepoint;
    pub const kind = .@"error";
};
pub const unicode_zero_width = struct {
    pub const msg = "identifier contains Unicode character <U+{X:0>4}> that is invisible in some environments";
    pub const opt = "unicode-homoglyph";
    pub const extra = .actual_codepoint;
    pub const kind = .warning;
};
pub const unicode_homoglyph = struct {
    pub const msg = "treating Unicode character <U+{X:0>4}> as identifier character rather than as '{u}' symbol";
    pub const extra = .codepoints;
    pub const opt = "unicode-homoglyph";
    pub const kind = .warning;
};
pub const meaningless_asm_qual = struct {
    pub const msg = "meaningless '{s}' on assembly outside function";
    pub const extra = .str;
    pub const kind = .@"error";
};
pub const duplicate_asm_qual = struct {
    pub const msg = "duplicate asm qualifier '{s}'";
    pub const extra = .str;
    pub const kind = .@"error";
};
pub const invalid_asm_str = struct {
    pub const msg = "cannot use {s} string literal in assembly";
    pub const extra = .str;
    pub const kind = .@"error";
};
pub const expanded_from_here = struct {
    pub const msg = "expanded from here";
    pub const kind = .note;
};
pub const skipping_macro_backtrace = struct {
    pub const msg = "(skipping {d} expansions in backtrace; use -fmacro-backtrace-limit=0 to see all)";
    pub const extra = .unsigned;
    pub const kind = .note;
};
pub const dollar_in_identifier_extension = struct {
    pub const msg = "'$' in identifier";
    pub const opt = "dollar-in-identifier-extension";
    pub const kind = .off;
    pub const suppress_language_option = "dollarsInIdentifiers";
    pub const pedantic = true;
};
pub const dollars_in_identifiers = struct {
    pub const msg = "illegal character '$' in identifier";
    pub const kind = .@"error";
};
pub const pragma_operator_string_literal = struct {
    pub const msg = "_Pragma requires exactly one string literal token";
    pub const kind = .@"error";
};
pub const unknown_gcc_pragma = struct {
    pub const msg = "pragma GCC expected 'error', 'warning', 'diagnostic', 'poison'";
    pub const opt = "unknown-pragmas";
    pub const kind = .off;
    pub const all = true;
};
pub const unknown_gcc_pragma_directive = struct {
    pub const msg = "pragma GCC diagnostic expected 'error', 'warning', 'ignored', 'fatal', 'push', or 'pop'";
    pub const opt = "unknown-pragmas";
    pub const kind = .off;
    pub const all = true;
};
pub const predefined_top_level = struct {
    pub const msg = "predefined identifier is only valid inside function";
    pub const opt = "predefined-identifier-outside-function";
    pub const kind = .warning;
};
pub const incompatible_va_arg = struct {
    pub const msg = "first argument to va_arg, is of type '{s}' and not 'va_list'";
    pub const extra = .str;
    pub const kind = .@"error";
};
pub const too_many_scalar_init_braces = struct {
    pub const msg = "too many braces around scalar initializer";
    pub const opt = "many-braces-around-scalar-init";
    pub const kind = .warning;
};
pub const uninitialized_in_own_init = struct {
    pub const msg = "variable '{s}' is uninitialized when used within its own initialization";
    pub const extra = .str;
    pub const opt = "uninitialized";
    pub const kind = .off;
    pub const all = true;
};
pub const gnu_statement_expression = struct {
    pub const msg = "use of GNU statement expression extension";
    pub const opt = "gnu-statement-expression";
    pub const kind = .off;
    pub const suppress_gnu = true;
    pub const pedantic = true;
};
pub const stmt_expr_not_allowed_file_scope = struct {
    pub const msg = "statement expression not allowed at file scope";
    pub const kind = .@"error";
};
pub const gnu_imaginary_constant = struct {
    pub const msg = "imaginary constants are a GNU extension";
    pub const opt = "gnu-imaginary-constant";
    pub const kind = .off;
    pub const suppress_gnu = true;
    pub const pedantic = true;
};
pub const plain_complex = struct {
    pub const msg = "plain '_Complex' requires a type specifier; assuming '_Complex double'";
    pub const kind = .warning;
};
pub const qual_on_ret_type = struct {
    pub const msg = "'{s}' type qualifier on return type has no effect";
    pub const opt = "ignored-qualifiers";
    pub const extra = .str;
    pub const kind = .off;
    pub const all = true;
};

pub const cli_invalid_standard = struct {
    pub const msg = "invalid standard '{s}'";
    pub const extra = .str;
    pub const kind = .@"error";
};
pub const cli_invalid_target = struct {
    pub const msg = "invalid target '{s}'";
    pub const extra = .str;
    pub const kind = .@"error";
};
pub const cli_invalid_emulate = struct {
    pub const msg = "invalid compiler '{s}'";
    pub const extra = .str;
    pub const kind = .@"error";
};
pub const cli_unknown_arg = struct {
    pub const msg = "unknown argument '{s}'";
    pub const extra = .str;
    pub const kind = .@"error";
};
pub const cli_error = struct {
    pub const msg = "{s}";
    pub const extra = .str;
    pub const kind = .@"error";
};
pub const extra_semi = struct {
    pub const msg = "extra ';' outside of a function";
    pub const opt = "extra-semi";
    pub const kind = .off;
    pub const pedantic = true;
};
pub const func_field = struct {
    pub const msg = "field declared as a function";
    pub const kind = .@"error";
};
pub const vla_field = struct {
    pub const msg = "variable length array fields extension is not supported";
    pub const kind = .@"error";
};
pub const field_incomplete_ty = struct {
    pub const msg = "field has incomplete type '{s}'";
    pub const extra = .str;
    pub const kind = .@"error";
};
pub const flexible_in_union = struct {
    pub const msg = "flexible array member in union is not allowed";
    pub const kind = .@"error";
};
pub const flexible_non_final = struct {
    pub const msg = "flexible array member is not at the end of struct";
    pub const kind = .@"error";
};
pub const flexible_in_empty = struct {
    pub const msg = "flexible array member in otherwise empty struct";
    pub const kind = .@"error";
};
pub const duplicate_member = struct {
    pub const msg = "duplicate member '{s}'";
    pub const extra = .str;
    pub const kind = .@"error";
};
pub const binary_integer_literal = struct {
    pub const msg = "binary integer literals are a GNU extension";
    pub const kind = .off;
    pub const opt = "gnu-binary-literal";
    pub const pedantic = true;
};
pub const gnu_va_macro = struct {
    pub const msg = "named variadic macros are a GNU extension";
    pub const opt = "variadic-macros";
    pub const kind = .off;
    pub const pedantic = true;
};
pub const builtin_must_be_called = struct {
    pub const msg = "builtin function must be directly called";
    pub const kind = .@"error";
};
pub const va_start_not_in_func = struct {
    pub const msg = "'va_start' cannot be used outside a function";
    pub const kind = .@"error";
};
pub const va_start_fixed_args = struct {
    pub const msg = "'va_start' used in a function with fixed args";
    pub const kind = .@"error";
};
pub const va_start_not_last_param = struct {
    pub const msg = "second argument to 'va_start' is not the last named parameter";
    pub const opt = "varargs";
    pub const kind = .warning;
};
pub const attribute_not_enough_args = struct {
    pub const msg = "'{s}' attribute takes at least {d} argument(s)";
    pub const kind = .@"error";
    pub const extra = .attr_arg_count;
};
pub const attribute_too_many_args = struct {
    pub const msg = "'{s}' attribute takes at most {d} argument(s)";
    pub const kind = .@"error";
    pub const extra = .attr_arg_count;
};
pub const attribute_arg_invalid = struct {
    pub const msg = "Attribute argument is invalid, expected {s} but got {s}";
    pub const kind = .@"error";
    pub const extra = .attr_arg_type;
};
pub const unknown_attr_enum = struct {
    pub const msg = "Unknown `{s}` argument. Possible values are: {s}";
    pub const kind = .@"error";
    pub const extra = .attr_enum;
};
pub const attribute_requires_identifier = struct {
    pub const msg = "'{s}' attribute requires an identifier";
    pub const kind = .@"error";
    pub const extra = .str;
};
pub const declspec_not_enabled = struct {
    pub const msg = "'__declspec' attributes are not enabled; use '-fdeclspec' or '-fms-extensions' to enable support for __declspec attributes";
    pub const kind = .@"error";
};
pub const declspec_attr_not_supported = struct {
    pub const msg = "__declspec attribute '{s}' is not supported";
    pub const extra = .str;
    pub const opt = "ignored-attributes";
    pub const kind = .warning;
};
pub const deprecated_declarations = struct {
    pub const msg = "{s}";
    pub const extra = .str;
    pub const opt = "deprecated-declarations";
    pub const kind = .warning;
};
pub const deprecated_note = struct {
    pub const msg = "'{s}' has been explicitly marked deprecated here";
    pub const extra = .str;
    pub const opt = "deprecated-declarations";
    pub const kind = .note;
};
pub const unavailable = struct {
    pub const msg = "{s}";
    pub const extra = .str;
    pub const kind = .@"error";
};
pub const unavailable_note = struct {
    pub const msg = "'{s}' has been explicitly marked unavailable here";
    pub const extra = .str;
    pub const kind = .note;
};
pub const ignored_record_attr = struct {
    pub const msg = "attribute '{s}' is ignored, place it after \"{s}\" to apply attribute to type declaration";
    pub const extra = .ignored_record_attr;
    pub const kind = .warning;
    pub const opt = "ignored-attributes";
};
pub const backslash_newline_escape = struct {
    pub const msg = "backslash and newline separated by space";
    pub const kind = .warning;
    pub const opt = "backslash-newline-escape";
};
pub const array_size_non_int = struct {
    pub const msg = "size of array has non-integer type '{s}'";
    pub const extra = .str;
    pub const kind = .@"error";
};
pub const cast_to_smaller_int = struct {
    pub const msg = "cast to smaller integer type {s}";
    pub const extra = .str;
    pub const kind = .warning;
    pub const opt = "pointer-to-int-cast";
};
pub const gnu_switch_range = struct {
    pub const msg = "use of GNU case range extension";
    pub const opt = "gnu-case-range";
    pub const kind = .off;
    pub const pedantic = true;
};
pub const empty_case_range = struct {
    pub const msg = "empty case range specified";
    pub const kind = .warning;
};
pub const non_standard_escape_char = struct {
    pub const msg = "use of non-standard escape character '\\e'";
    pub const kind = .off;
    pub const opt = "pedantic";
};
pub const invalid_pp_stringify_escape = struct {
    pub const msg = "invalid string literal, ignoring final '\\'";
    pub const kind = .warning;
};
pub const vla = struct {
    pub const msg = "variable length array used";
    pub const kind = .off;
    pub const opt = "vla";
};
pub const float_overflow_conversion = struct {
    pub const msg = "implicit conversion of non-finite value from {s} is undefined";
    pub const extra = .str;
    pub const kind = .off;
    pub const opt = "float-overflow-conversion";
};
pub const float_out_of_range = struct {
    pub const msg = "implicit conversion of out of range value from {s} is undefined";
    pub const extra = .str;
    pub const kind = .warning;
    pub const opt = "literal-conversion";
};
pub const float_zero_conversion = struct {
    pub const msg = "implicit conversion from {s}";
    pub const extra = .str;
    pub const kind = .off;
    pub const opt = "float-zero-conversion";
};
pub const float_value_changed = struct {
    pub const msg = "implicit conversion from {s}";
    pub const extra = .str;
    pub const kind = .warning;
    pub const opt = "float-conversion";
};
pub const float_to_int = struct {
    pub const msg = "implicit conversion turns floating-point number into integer: {s}";
    pub const extra = .str;
    pub const kind = .off;
    pub const opt = "literal-conversion";
};
pub const const_decl_folded = struct {
    pub const msg = "expression is not an integer constant expression; folding it to a constant is a GNU extension";
    pub const kind = .off;
    pub const opt = "gnu-folding-constant";
    pub const pedantic = true;
};
pub const const_decl_folded_vla = struct {
    pub const msg = "variable length array folded to constant array as an extension";
    pub const kind = .off;
    pub const opt = "gnu-folding-constant";
    pub const pedantic = true;
};
pub const redefinition_of_typedef = struct {
    pub const msg = "typedef redefinition with different types ({s})";
    pub const extra = .str;
    pub const kind = .@"error";
};
pub const undefined_macro = struct {
    pub const msg = "'{s}' is not defined, evaluates to 0";
    pub const extra = .str;
    pub const kind = .off;
    pub const opt = "undef";
};
pub const fn_macro_undefined = struct {
    pub const msg = "function-like macro '{s}' is not defined";
    pub const extra = .str;
    pub const kind = .@"error";
};
pub const preprocessing_directive_only = struct {
    pub const msg = "'{s}' must be used within a preprocessing directive";
    pub const extra = .str;
    pub const kind = .@"error";
};
pub const missing_lparen_after_builtin = struct {
    pub const msg = "Missing '(' after built-in macro '{s}'";
    pub const extra = .str;
    pub const kind = .@"error";
};
pub const too_many_includes = struct {
    pub const msg = "#include nested too deeply";
    pub const kind = .@"error";
};
pub const include_next = struct {
    pub const msg = "#include_next is a language extension";
    pub const kind = .off;
    pub const pedantic = true;
    pub const opt = "gnu-include-next";
};
pub const include_next_outside_header = struct {
    pub const msg = "#include_next in primary source file; will search from start of include path";
    pub const kind = .warning;
    pub const opt = "include-next-outside-header";
};
pub const offsetof_ty = struct {
    pub const msg = "offsetof requires struct or union type, '{s}' invalid";
    pub const extra = .str;
    pub const kind = .@"error";
};
pub const offsetof_incomplete = struct {
    pub const msg = "offsetof of incomplete type '{s}'";
    pub const extra = .str;
    pub const kind = .@"error";
};
pub const offsetof_array = struct {
    pub const msg = "offsetof requires array type, '{s}' invalid";
    pub const extra = .str;
    pub const kind = .@"error";
};
pub const pragma_pack_lparen = struct {
    pub const msg = "missing '(' after '#pragma pack' - ignoring";
    pub const kind = .warning;
    pub const opt = "ignored-pragmas";
};
pub const pragma_pack_rparen = struct {
    pub const msg = "missing ')' after '#pragma pack' - ignoring";
    pub const kind = .warning;
    pub const opt = "ignored-pragmas";
};
pub const pragma_pack_unknown_action = struct {
    pub const msg = "unknown action for '#pragma pack' - ignoring";
    pub const opt = "ignored-pragmas";
    pub const kind = .warning;
};
pub const pragma_pack_show = struct {
    pub const msg = "value of #pragma pack(show) == {d}";
    pub const extra = .unsigned;
    pub const kind = .warning;
};
pub const pragma_pack_int = struct {
    pub const msg = "expected #pragma pack parameter to be '1', '2', '4', '8', or '16'";
    pub const opt = "ignored-pragmas";
    pub const kind = .warning;
};
pub const pragma_pack_int_ident = struct {
    pub const msg = "expected integer or identifier in '#pragma pack' - ignored";
    pub const opt = "ignored-pragmas";
    pub const kind = .warning;
};
pub const pragma_pack_undefined_pop = struct {
    pub const msg = "specifying both a name and alignment to 'pop' is undefined";
    pub const kind = .warning;
};
pub const pragma_pack_empty_stack = struct {
    pub const msg = "#pragma pack(pop, ...) failed: stack empty";
    pub const opt = "ignored-pragmas";
    pub const kind = .warning;
};
pub const cond_expr_type = struct {
    pub const msg = "used type '{s}' where arithmetic or pointer type is required";
    pub const extra = .str;
    pub const kind = .@"error";
};
pub const enumerator_too_small = struct {
    pub const msg = "ISO C restricts enumerator values to range of 'int' ({d} is too small)";
    pub const extra = .signed;
    pub const kind = .off;
    pub const opt = "pedantic";
};
pub const enumerator_too_large = struct {
    pub const msg = "ISO C restricts enumerator values to range of 'int' ({d} is too large)";
    pub const extra = .unsigned;
    pub const kind = .off;
    pub const opt = "pedantic";
};
pub const enumerator_overflow = struct {
    pub const msg = "overflow in enumeration value";
    pub const kind = .warning;
};
pub const enum_not_representable = struct {
    pub const msg = "incremented enumerator value {s} is not representable in the largest integer type";
    pub const kind = .warning;
    pub const opt = "enum-too-large";
    pub const extra = .pow_2_as_string;
};
pub const enum_too_large = struct {
    pub const msg = "enumeration values exceed range of largest integer";
    pub const kind = .warning;
    pub const opt = "enum-too-large";
};
pub const enum_fixed = struct {
    pub const msg = "enumeration types with a fixed underlying type are a Clang extension";
    pub const kind = .off;
    pub const pedantic = true;
    pub const opt = "fixed-enum-extension";
};
pub const enum_prev_nonfixed = struct {
    pub const msg = "enumeration previously declared with nonfixed underlying type";
    pub const kind = .@"error";
};
pub const enum_prev_fixed = struct {
    pub const msg = "enumeration previously declared with fixed underlying type";
    pub const kind = .@"error";
};
pub const enum_different_explicit_ty = struct {
    // str will be like 'new' (was 'old'
    pub const msg = "enumeration redeclared with different underlying type {s})";
    pub const extra = .str;
    pub const kind = .@"error";
};
pub const enum_not_representable_fixed = struct {
    pub const msg = "enumerator value is not representable in the underlying type '{s}'";
    pub const extra = .str;
    pub const kind = .@"error";
};
pub const transparent_union_wrong_type = struct {
    pub const msg = "'transparent_union' attribute only applies to unions";
    pub const opt = "ignored-attributes";
    pub const kind = .warning;
};
pub const transparent_union_one_field = struct {
    pub const msg = "transparent union definition must contain at least one field; transparent_union attribute ignored";
    pub const opt = "ignored-attributes";
    pub const kind = .warning;
};
pub const transparent_union_size = struct {
    pub const msg = "size of field {s} bits) does not match the size of the first field in transparent union; transparent_union attribute ignored";
    pub const extra = .str;
    pub const opt = "ignored-attributes";
    pub const kind = .warning;
};
pub const transparent_union_size_note = struct {
    pub const msg = "size of first field is {d}";
    pub const extra = .unsigned;
    pub const kind = .note;
};
pub const designated_init_invalid = struct {
    pub const msg = "'designated_init' attribute is only valid on 'struct' type'";
    pub const kind = .@"error";
};
pub const designated_init_needed = struct {
    pub const msg = "positional initialization of field in 'struct' declared with 'designated_init' attribute";
    pub const opt = "designated-init";
    pub const kind = .warning;
};
