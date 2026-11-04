# apropos_mod

- Module: apropos_mod, apropos_print_mod (submodule)
- Upstream: apropos.el
- State: partial

## Ported

`apropos-command` (C-h a; C-u for all functions),
`apropos-function`,
`apropos-user-option` (C-h u; C-u for all variables),
`apropos-variable`,
`apropos-documentation` (C-h d; sorted by score, full docstrings filled, "----------------" between items),
`apropos-read-pattern`,
`apropos-parse-pattern` (word lists with synonyms, regexps, multiline),
`apropos-words-to-regexp`,
`apropos-calc-scores`,
`apropos-score-doc`,
`apropos-score-symbol`,
`apropos-true-hit`,
`apropos-print` (keys from column 30, type labels, "Value:" excerpts as Fortran, the return message),
Apropos mode with `apropos-follow` (RET),
`apropos-next-symbol` (n),
`apropos-previous-symbol` (p);
labels and names marked by text properties standing for the buttons

## Deferred

`apropos` itself,
`apropos-value`,
`apropos-library`,
`apropos-local-variable`,
`apropos-local-value` (symbol plists, faces, libraries, values of all symbols),
`apropos-sort-by-scores` and its verbose scores,
faces,
mouse buttons,
`apropos-compact-layout`,
`apropos-do-all`,
revert

## Tests

test_apropos_mod (ports apropos-tests-words-to-regexp-1, -2, -parse-pattern-1, -2, -3, -single-regexp, -synonyms, -calc-scores, -true-hit; own tests through the command loop)
