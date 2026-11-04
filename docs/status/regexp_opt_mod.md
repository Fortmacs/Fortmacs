# regexp_opt_mod

- Module: regexp_opt_mod
- Upstream: emacs-lisp/regexp-opt.el (+ subr.el `regexp-unmatchable`)
- State: partial

## Ported

`regexp-opt` (PAREN nil, t, `words`, `symbols` or an opening string; common prefixes and suffixes merged, one-character alternatives as character sets, longest match first),
`regexp-opt-charset`,
`regexp-opt-depth`

## Deferred

strings ending in spaces (the strings are a character array)

## Tests

test_f90_mod (ports regexp-opt-tests: regexp-opt-longest-match, regexp-opt-charset; own: F90 mode's regexps),
test_syntax_propertize_mod (own: `regexp-opt-depth`)
