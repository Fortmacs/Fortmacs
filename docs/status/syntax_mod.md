# syntax_mod

- Module: syntax_mod, syntax_skip_mod, syntax_scan_mod (submodules)
- Upstream: syntax.c, syntax.h (+ category.c `is_word_boundary`, subr.el `make-syntax-table`)
- State: done

## Ported

syntax classes and raw descriptors (code with flags, matching character),
syntax tables as registry objects with parents (`standard-syntax-table` from `initialize_syntax_once` plus characters_mod, `make-syntax-table`, `copy-syntax-table`, `set-syntax-table` with `bset_syntax_table`, `syntax-table`, `syntax-table-p`, `check_syntax_table`, `SET_RAW_SYNTAX_ENTRY(_RANGE)`, the buffer's table),
`char-syntax` (raw bytes in unibyte buffers),
`internal-describe-syntax-value` (the text it inserts; `SYNTAX_FLAGS_COMMENT_STYLEB`, `SYNTAX_FLAGS_COMMENT_STYLEC`),
`SYNTAX_MATCH`,
`SYNTAX_COMEND_FIRST`,
`in_classes`,
`syntax-class-to-char`,
`matching-paren`,
`string-to-syntax`,
`modify-syntax-entry` (characters and ranges),
`scan_words`,
`forward-word` (M-f; fields),
`words-include-escapes`,
word boundaries between scripts (`char-script-table`) with characters.el's `word-combining-categories` and `word-separating-categories` ;
`skip-chars-forward`,
`skip-chars-backward` (ranges, `^`, quoting, character classes; in the submodule syntax_skip_mod),
`skip-syntax-forward`,
`skip-syntax-backward`,
`backward-prefix-chars` (with `character_quoted`);
comments and expressions in the submodule syntax_scan_mod: `forward-comment` (`forw_comment`, `back_comment` with its lossage path through `find_subprogram_start`, `previous_character_comend_first`),
`scan-lists`,
`scan-sexps` (`scan_groups`, `syntax_multibyte`; the `scan-error` error with its data),
`parse-partial-sexp` (`scan_expressions_forward`, `in_2char_comment_start`, `internalize_parse_state`; the state as `parse_state_type`, TARGETDEPTH, STOPBEFORE, OLDSTATE, COMMENTSTOP nil/t/`syntax-table`),
comment styles a,
b and c,
nested comments,
two-character delimiters (flags 1234),
generic comment and string fences,
`parse-sexp-ignore-comments`,
`comment-end-can-be-escaped` (buffer-local when set),
`multibyte-syntax-as-symbol`,
`open-paren-in-column-0-is-defun-start`,
`comment-use-syntax-ppss` (through `syntax_parser_state`, which is syntax_propertize_mod's cached `syntax-ppss`),
`syntax-table` text properties (`parse-sexp-lookup-properties`, `syntax-propertize--done`, both buffer-local when set; raw descriptors and syntax tables as values) in the comment and expression scanners,
`scan_words`,
`skip-syntax-*` and `backward-prefix-chars`,
with `parse_expression_propertize` calling `internal--syntax-propertize` lazily (lookups by position replace `gl_state`'s interval cache)

## Deferred

`parse_expression_propertize`'s errors when `internal--syntax-propertize` modified the buffer or did not move `syntax-propertize--done`,
`forward-comment-function`,
raw bytes in `skip-chars-*`,
`find-word-boundary-function-table`,
`describe-syntax`,
`forward-word-strictly`,
unibyte raw bytes in the scanners,
quitting during scans;
in part: `update_syntax_table`,
`update_syntax_table_forward`,
`SETUP_SYNTAX_TABLE` and `RE_SETUP_SYNTAX_TABLE_FOR_OBJECT` (the `syntax-table` property is looked up at each position instead of gl_state's interval cache; strings have none);
Not applicable: the extractor's `c` and `point` lines (comment text);
waits for the GUI: nothing

## Tests

test_syntax_mod (ports syntax-class-character-test, syntax-char-syntax (interpreted only: no byte compiler); own tests, including `internal-describe-syntax-value`, including skip-chars and skip-syntax),
test_syntax_scan_mod (ports parse-partial-sexp-continue-over-comment-marker, parse-partial-sexp-paren-comments, test-from-to-parse-partial-sexp and all the syntax-comments, syntax-br-comments and syntax-pps-comments tests over syntax-resources/syntax-comments.txt; own tests for nested comments, C-like two-character delimiters, scan errors and the parse state)
