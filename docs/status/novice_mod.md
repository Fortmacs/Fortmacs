# novice_mod

- Module: novice_mod
- Upstream: novice.el (+ subr.el and bindings.el `disabled` properties)
- State: partial

## Ported

`disabled-command-function` (n, y, SPC, !; the *Disabled Command* help with the first paragraph of the docstring),
the `disabled` property test of `command-execute` for keys and M-x,
`enable-command`,
`disable-command` (for the session; a configuration calls them to make it last);
`upcase-region`,
`downcase-region` and `narrow-to-region` disabled

## Deferred

writing the choice to an init file (the Fortran configuration is not edited)

## Tests

test_novice_mod (own; upstream has none)
