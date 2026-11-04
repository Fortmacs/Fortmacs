# error_mod

- Module: error_mod
- Upstream: data.c `syms_of_data` error table (subset), fileio.c `file-error`, print.c `print_error_message` layout
- State: done

## Ported

status type,
codes (incl. `void-function`, `file-missing`, `file-notify-error`, `file-already-exists`, `mark-inactive`, `search-failed`, `invalid-regexp`, syntax.c's `scan-error`),
file-error messages,
`condition_matches`,
`format_error`;
`file-date-error` (fileio.c),
lread.c's `end-of-file` and `invalid-read-syntax`,
`wrong-number-of-arguments`;
imenu.el's `imenu-unavailable` (step 49c)

## Deferred

remaining error symbols (added as modules need them)

## Tests

test_error_mod
