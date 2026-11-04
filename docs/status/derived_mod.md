# derived_mod

- Module: derived_mod
- Upstream: emacs-lisp/derived.el
- State: partial

## Ported

`define-derived-mode` as a registry plus `derived_mode_begin`/`derived_mode_end` around a Fortran body: parent mode with delayed hooks (or `kill-all-local-variables`),
`major-mode`,
`mode-name`,
`MODE-map` inheriting the parent's map,
the syntax table,
`MODE-hook`,
`MODE-abbrev-table` (the table passed to `define`, made known as NAME_abbrev_table; its parent the parent mode's table; set as `local-abbrev-table`)

## Deferred

:abbrev-table (a table of another name),
:after-hook,
:interactive,
`derived-mode-init-mode-variables` docstrings,
`derived-mode-merge-*`

## Tests

test_derived_mod
