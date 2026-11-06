# fortmacs_diff_help_mod

- Module: fortmacs_diff_help_mod
- Upstream: vc/ediff-help.el
- State: partial

## Ported

the procedures that act on one session, as type-bound procedures of `fortmacs_diff_session_type` (panel 14) in fortmacs_diff_settings_help_mod, a submodule of fortmacs_diff_settings_mod,
`ediff-long-help-message-head`,
`-tail`,
`-compare2`,
`-compare3`,
`-merge`,
`ediff-brief-message-string`,
`ediff-use-long-help-message`,
`ediff-set-help-message` (2-way and 3-way comparisons, merging),
`ediff-indent-help-message`,
`ediff-help-message-line-length`,
`ediff-set-help-overlays`

## Deferred

the word-mode and narrow messages (with those jobs),
`ediff-brief-help-message-function` and `ediff-long-help-message-function`,
`ediff-help-region-map` (mouse-2),
`ediff-help-for-quick-help` (the Ediff manual),
`ediff-customize` (Customize)

## Tests

test_fortmacs_diff_utilities_mod (own: ? toggles the message)
