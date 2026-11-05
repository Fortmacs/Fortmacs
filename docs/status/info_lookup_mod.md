# info_lookup_mod

- Module: info_lookup_mod
- Upstream: info-look.el
- State: partial

## Ported

`info-lookup-symbol` (C-h S; the help for Fortmacs commands and variables reads (fortmacs)Command Index and (fortmacs)Variable Index, R6-32),
`info-lookup-file`,
`info-lookup-reset`,
`info-lookup`,
`info-lookup-add-help` and `info-lookup-maybe-add-help` (typed arguments; doc specs built with `%append`, translations as procedures),
`info-lookup-interactive-arguments`,
`info-lookup-select-mode` (the file name table, then the major mode's parents),
`info-lookup-change-mode`,
`info-lookup--item-to-mode`,
`info-lookup-setup-mode`,
`info-lookup-make-completions`,
`info-lookup-guess-default` and `-default*`,
`info-lookup-guess-c-symbol`,
`info-lookup-guess-gdb-script-symbol`,
the cache,
`info-lookup-mode`,
`info-lookup-other-window-flag`,
`info-lookup-highlight-face` (an overlay),
`info-lookup-file-name-alist`;
the help specifications of all upstream's modes (as data, whether the mode is ported or not)

## Deferred

`info-complete-symbol`,
`info-complete-file` and `info-lookup-completions-at-point` (completion in the buffer),
the obsolete `info-lookup-guess-custom-symbol`, *info* shown in another frame

## Tests

test_info_lookup_mod (own tests: looking up in the index of test.info, the guess at point, `info-display-manual`)
