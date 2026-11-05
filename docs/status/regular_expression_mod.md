# regular_expression_mod

- Module: regular_expression_mod
- Upstream: regex-emacs.c, regex-emacs.h
- State: partial

## Ported

the compiler (`regular_expression_compile`: groups, shy and numbered groups, alternatives, `*` `+` `?` and their lazy forms, intervals, bracket expressions with ranges and character classes, back references, `\w` `\W` `\sC` `\SC` `\cC` `\CC` (the current buffer's category table) `\b` `\B` `\<` `\>` `\_<` `\_>` `\=` `` \` `` `\'`, whitespace regexps), the first-character analysis (`forall_firstchar`, `analyze_first`, the fastmap, `is_mutually_exclusive` for on_failure_jump_smart), `re_search_2` and the backtracking matcher `regular_expression_match_2_internal` over strings and the buffer (across the gap), case folding through the case canon table, `regular_expression_wctype_parse`, `regular_expression_iswctype`, `syntax-table` text properties for `\b` `\B` `\<` `\>` `\_<` `\_>` `\sC` `\SC` in buffers (character classes use the buffer's table, as upstream's BUFFER_SYNTAX), a matcher reentrant from syntax-propertize

## Deferred

unibyte patterns and text,
the regex debugging output

## Tests

test_regular_expression_mod (ports regex-word-cc-fallback-test, the character class tests, regex-repeat-limit, regexp-multibyte-multibyte, regexp-case-fold, regexp-eszett, regexp-atomic-failure, regexp-tests-backtrack-optimization, regexp-tests-zero-width-assertion-repetition, regex-tests-mutual-exclusive-inf-rec, regex-tests-PTESTS, -TESTS, -BOOST and -PCRE with their data files; own tests)
