# ediff_help_mod

- Module: ediff_help_mod
- Upstream: vc/ediff-help.el
- State: partial

## Ported

`ediff-long-help-message-head`,
`-tail`,
`-compare2`,
`-compare3`,
`ediff-brief-message-string`,
`ediff-use-long-help-message`,
`ediff-set-help-message` (2-way and 3-way comparisons),
`ediff-indent-help-message`,
`ediff-help-message-line-length`,
`ediff-set-help-overlays`

## Deferred

the word-mode,
narrow and merge messages (with those jobs),
`ediff-brief-help-message-function` and `ediff-long-help-message-function`,
`ediff-help-region-map` (mouse-2),
`ediff-help-for-quick-help` (the Ediff manual),
`ediff-customize` (Customize)

## Tests

test_ediff_util_mod (own: ? toggles the message)
