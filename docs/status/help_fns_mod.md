# help_fns_mod

- Module: help_fns_mod
- Upstream: help-fns.el (+ help-mode.el describe-symbol-backends)
- State: partial

## Ported

`describe-function` (C-h f: kind, Fortran file, keys, how Fortran calls it, the docstring),
`describe-function-1`,
`describe-variable` (C-h v: file, value as Fortran — `.true.`/`.false.`, integers, string literals, named constants of enumerated settings, procedure names or `null()`, `unallocated`, arrays as constructors — the value at startup when it differs, Fortran name, docstring, then "Automatically becomes buffer-local when set." and "You can customize this variable." as help-fns--var-bufferlocal and help-fns--customize-variable print them),
`describe-symbol` (C-h o: function and variable, a separator between),
`describe-command` (C-h x),
`describe-mode` (C-h m: the major mode, then the local and global minor modes with their indicators and docstrings, in the layout of `describe-mode-outline` t)

## Deferred

defaults from point,
buttons,
"Probably introduced",
obsolescence,
aliases,
buffer-local values and the other describe-variable functions,
the values of hooks,
keymaps,
syntax tables and other derived-type variables (no value line yet),
faces and types in describe-symbol,
describe-mode's BUFFER and Outline minor mode,
the Customize interface

## Tests

test_help_mod (ports help-fns-test-describe-symbol on a function and variable; own)
