# simple_electric_mod

- Module: simple_electric_mod (submodule of simple_mod)
- Upstream: simple.el
- State: done

## Ported

`blink-matching-open` (`blink-matching-paren` t/jump/jump-offscreen/nil, `-on-screen`, `-distance`, `-delay`, `-dont-ignore-comments`, the overlay, "Mismatched parentheses", "No matching parenthesis found", "Matches ..."),
`blink-matching-check-mismatch` (`blink-matching-check-function`),
`blink-paren-open-paren-line-string`,
`blink-paren-function`,
`blink-paren-post-self-insert-function` on `post-self-insert-hook`,
`newline-and-indent`,
`reindent-then-newline-and-indent`,
`backward-delete-char-untabify` (`backward-delete-char-untabify-method`),
`blink-matching-check-function` buffer-local (`%set_local`),
`syntax-propertize` in `blink-matching-open`

## Deferred

`blink-matching-paren-highlight-offscreen` and its face,
`noninteractive` (no batch mode)

## Tests

test_simple_electric_mod (own; simple-tests.el has no blink cases)
