# parenthesis_mod

- Module: parenthesis_mod
- Upstream: paren.el
- State: done

## Ported

Show Paren mode (global, on by default) and Show Paren Local mode,
the idle timer,
the two overlays,
`show-paren-function`,
`show-paren--default` (`show_parenthesis_data_function`),
`--locate-near-paren`,
`--categorize-paren`,
`--unescaped-p`,
`--enabled-p`,
the default `show-paren-predicate` (not in special-mode buffers) as a procedure,
all the options (style, delay, priority, ring bell, inside paren, periphery, highlight openparen, `not-in-comments-or-strings`, context when offscreen in the echo area),
`syntax-propertize` before narrowing,
`show-paren-data-function` as a buffer-local procedure setting (`%procedure`, `%set_local`, `%get_current`, `%is_local`, `%clear_local`)

## Deferred

context in an overlay or child frame,
the `shadow` face of "Matches ",
buffer-match-p conditions

## Tests

test_parenthesis_mod (ports all of paren-tests.el; own: overlays and faces, modes, the idle timer on a pty)
