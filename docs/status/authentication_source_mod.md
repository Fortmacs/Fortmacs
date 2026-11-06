# authentication_source_mod

- Module: authentication_source_mod
- Upstream: auth-source.el
- State: partial

## Ported

`read-passwd` (CONFIRM, DEFAULT, no history),
`read-passwd-mode` with `read-passwd-map` (C-u, TAB),
`read-passwd-toggle-visibility`,
`read-passwd--hide-password` (`display` properties of `read-hide-char`)

## Deferred

the authentication sources,
the mode line icon (icons, `global-mode-string`),
the minibuffer's own `show-paren-mode`,
`select-active-regions` and `inhibit--record-char`,
text conversion

## Tests

through test_command_interpreter_mod (the password tests)
