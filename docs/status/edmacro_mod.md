# edmacro_mod

- Module: edmacro_mod
- Upstream: edmacro.el
- State: partial

## Ported

`format-kbd-macro` (edmacro-format-keys: prefix arguments, the longest bound key, local-function-key-map translations, runs of self-inserting characters, `<<COMMAND>>` for M-x, repetitions as N*KEY, the 72-column, one-line and verbose layouts),
`edmacro-eight-bits`

## Deferred

the macro editor (`edit-kbd-macro`, `edmacro-mode`, `insert-kbd-macro`),
`read-kbd-macro`/`edmacro-parse-keys`,
menu and mouse events in macros

## Tests

test_kmacro_mod
