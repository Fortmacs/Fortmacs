# programmable_completion_mod

- Module: programmable_completion_mod
- Upstream: pcomplete.el
- State: partial

## Ported

`pcomplete-completions-at-point` (the stub and the buffer text matched through unquoting, `completion-table-subvert` when they differ, `completion-table-with-quoting` with `pcomplete-unquote-argument-function` and `pcomplete-requote-argument-function`, paring with `pcomplete-use-paring`, `pcomplete-exit-function` and `pcomplete-default-exit-function` with `pcomplete-termination-string`),
`pcomplete-completions`,
`pcomplete-here` and `pcomplete-here*` (completion functions return when it gives the table, the throw to `pcomplete-completions`),
`pcomplete-arg`,
`pcomplete-actual-arg`,
`pcomplete-begin`,
`pcomplete-next-arg`,
`pcomplete-command-name`,
`pcomplete-match`,
`pcomplete-match-string`,
`pcomplete-test`,
`pcomplete-parse-buffer-arguments`,
`pcomplete-parse-comint-arguments`,
`pcomplete-parse-arguments`,
`pcomplete-entries` (environment variables expanded, `pcomplete-file-ignore`, `pcomplete-dir-ignore`),
`pcomplete-all-entries`,
`pcomplete-dirs`,
`pcomplete-executables`,
`pcomplete-dirs-or-entries`,
`pcomplete-find-completion-function` (pcomplete/NAME and pcomplete/MODE/NAME as `pcomplete_command_functions%append`),
`pcomplete-parse-arguments-function`,
`pcomplete-command-completion-function`,
`pcomplete-command-name-function`,
`pcomplete-default-completion-function` (buffer-local with `set_local`),
`pcomplete-try-first-hook`,
`pcomplete-expand-before-complete`,
`pcomplete-allow-modifications`

## Deferred

pcomplete's own UI (`pcomplete`, `pcomplete-reverse`, `pcomplete-expand-and-complete`, `pcomplete-continue`, `pcomplete-expand`, `pcomplete-help`, `pcomplete-list`, cycling, `pcomplete-show-completions`, `pcomplete-insert-entry`, `pcomplete-stub`) and so `pcomplete-comint-setup` and `pcomplete-shell-setup`;
`pcomplete-opt`,
`pcomplete-from-help`,
`pcomplete-here-using-help`,
host names and the pcmpl-*.el command completions;
argument lists computed by the parse function;
the annotation and help properties of candidates;
`pcomplete-compare-entry-function`;
completion-table-case-fold

## Tests

test_programmable_completion_mod (own: pcomplete-tests.el tests `pcomplete-from-help`)
