# syntax_propertize_mod

- Module: syntax_propertize_mod (named so as not to clash with syntax_mod)
- Upstream: emacs-lisp/syntax.el (+ subr.el `subregexp-context-p` in subr_mod, regexp-opt.el `regexp-opt-depth` in regexp_opt_mod)
- State: partial

## Ported

`syntax-propertize-function` (buffer-local when set),
`syntax-propertize` (chunks, `syntax-propertize-extend-region-functions` run until stable with `syntax-propertize--done` bound against recursion, the narrowing message, silent modifications, `syntax-ppss-table`),
`internal--syntax-propertize` (hooked into syntax_mod, `syntax-propertize-chunks`),
`syntax-propertize-wholelines` (`syntax-wholeline-max`, `syntax--lbp`),
`syntax-propertize-multiline`,
`syntax-propertize--shift-groups-and-backrefs`,
`syntax-propertize-rules` as `syntax_propertize_rules_type` (one regexp of all rules, extra groups for match 0, shifted groups and back-references, match data shifted for code; string, code and (prog1 SYNTAX . EXPS) / (ignore . EXPS) highlights; precompiled rules as rule arrays),
`syntax-ppss` with both caches (wide and narrowed, `syntax-ppss-max-span`, intermediate entries, the last-call shortcut and its statistics, `syntax-begin-function`, args-out-of-range fallback),
`syntax-ppss-flush-cache` (in `before-change-functions` at depth 99),
`syntax-ppss-depth`,
`syntax-ppss-toplevel-pos`,
`syntax-ppss-context`,
`syntax-propertize--in-process-p`

## Deferred

`syntax-propertize-via-font-lock` (font-lock-syntactic-keywords),
`syntax-ppss-stats`,
`syntax-ppss-debug`,
`ignore` as a `syntax-propertize-function` (null is nil)

## Tests

test_syntax_propertize_mod (ports syntax-propertize--shift-groups-and-backrefs; own tests of `regexp-opt-depth`, `subregexp-context-p`, rules that the scanners and `\s<` obey, the cache against `parse-partial-sexp` before and after a change, `syntax-propertize-wholelines`)
