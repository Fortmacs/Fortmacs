# electric_mod

- Module: electric_mod
- Upstream: electric.el
- State: partial

## Ported

Electric Indent mode (global, on by default; reindents on `electric-indent-chars` from `post-self-insert-hook`, deleting the previous line's trailing whitespace),
Electric Indent Local mode,
`electric--after-char-pos`,
`electric-indent-can-reindent-p` with `electric-indent-functions-without-reindent`,
`electric-indent-inhibit` (buffer-local),
`electric-indent-just-newline`,
`electric-newline-and-maybe-indent` (C-j)

## Deferred

`electric-indent-functions`,
`electric-indent-actions`,
buffer-local `electric-indent-chars`,
Electric Layout mode,
Electric Quote mode,
`Electric-command-loop`,
`Electric-pop-up-window`

## Tests

test_electric_mod (own: electric-tests.el's indent cases need c-mode/js-mode/python-mode)
