# print_mod

- Module: print_mod
- Upstream: print.c
- State: partial

## Ported

`prin1-to-string` (nil, integers, symbols, strings with escapes, lists, buffers, markers as `#<marker at N in BUFFER>`, sparse keymaps as their list);
`fortran_text` and `fortran_string` (ours: values as Fortran for help and error messages);
`prin1-to-string`'s NOESCAPE (`princ` on strings: `readable_text(object, noescape=.true.)`),
`float_to_string` (real64 values: dtoastr's shortest digits, `1.0e+INF`, `0.0e+NaN`),
`sprintf_float` (C's %e %f %g with flags, exact digits rounded to even, for `format`)

## Deferred

print streams,
`print-length`/`print-level`,
symbol escaping,
float values (`float_to_string` takes a real64),
`float-output-format`,
the text properties of a printed string,
full keymaps' char-tables

## Tests

test_help_mod,
test_marker_mod,
test_keymap_mod,
test_editing_functions_mod (floats through `format`)
