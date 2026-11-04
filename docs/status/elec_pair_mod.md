# elec_pair_mod

- Module: elec_pair_mod
- Upstream: elec-pair.el
- State: partial

## Ported

Electric Pair mode and Electric Pair Local mode,
pairs of characters and of strings (`electric-pair-pairs`, `-text-pairs`),
`electric-pair-syntax-info`,
wrapping the region,
escapes,
skipping (`electric-pair-skip-self`, `-skip-whitespace` t/chomp, `-skip-whitespace-chars`, `-skip-whitespace-function`),
pairing (`electric-pair-inhibit-predicate`, default and conservative),
balance (`--balance-info`, `-inhibit-if-helps-balance`, `-skip-if-helps-balance`, `--unbalanced-strings-p`, `--inside-string-p`, `--syntax-ppss`, `-text-syntax-table`, `-string-bound-function`),
`electric-pair-open-newline-between-pairs`,
`electric-pair-delete-pair`

## Deferred

the DEL binding of `electric-pair-mode-map` and `electric-pair-delete-adjacent-pairs` (keymap menu items with :filter),
`self-insert-uses-region-functions`,
`electric-pair-analyze-conversion`,
function forms of `-open-newline-between-pairs` and `-skip-whitespace`,
overwrite mode

## Tests

test_elec_pair_mod (ports electric-tests.el's pair, skip, quote, whitespace, angle-bracket, autowrapping, string-pair, backspace, undo and extra-newline cases in Fundamental mode, generated as `define-electric-pair-test` does)
