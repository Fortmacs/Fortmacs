# simple_set_variable_mod

- Module: simple_set_variable_mod (submodule of simple_mod)
- Upstream: simple.el `set-variable` (+ minibuf.c `read-variable`, the reading of the value)
- State: partial

## Ported

M-x set-variable: the user options with completion (custom-variable-p over `tools/user-options.d`, history `custom-variable-history`),
the prompt "Set NAME globally/buffer-locally/(buffer-local) to value: " with the current value for M-n,
the value as one Fortran literal of the option's type (`.true.`/`.false.`, integers, reals, character literals in either quote, named constants of enumerated settings, or `.true.`, `.false.` and the named constants of a choice setting, read with completion over them, each annotated with its value),
another value of an enumerated setting (such as upstream's symbol) a `wrong-type-argument` error naming its constants,
"Value ‘…’ does not match type … of …",
the reader's "End of file during parsing" and "Trailing garbage following expression",
C-u and automatically buffer-local options set buffer-locally (make-local-variable)

## Deferred

options holding hooks,
procedures,
arrays and other derived types,
unallocated string values,
the default from point,
`variable-interactive`,
obsolescence messages,
`minibuffer-help-form`,
buffer-local values of options that are not setting objects ("Symbol NAME may not be buffer-local")

## Tests

test_simple_set_variable_mod (own; upstream has none)
