# prog_mode_mod

- Module: prog_mode_mod
- Upstream: progmodes/prog-mode.el
- State: partial

## Ported

`prog-mode` (`require-final-newline`, `parse-sexp-ignore-comments`),
`prog-first-column`,
`prog-indentation-context` (FIRST-COLUMN; REST is unused upstream)

## Deferred

its keys (C-M-q, M-q),
the context menu,
prettify-symbols

## Tests

test_derived_mod
