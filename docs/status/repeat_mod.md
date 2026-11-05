# repeat_mod

- Module: repeat_mod
- Upstream: repeat.el
- State: partial

## Ported

`repeat` (C-x z: the last repeatable command again, with its prefix argument or a new one, then its final key repeats it; `repeat-too-dangerous`, `repeat-on-final-keystroke`, `repeat-is-really-this-command`, `repeat-previous-repeated-command`),
`repeat-mode` (`repeat-pre-hook` with `repeat-keep-prefix`, `repeat-post-hook`, `repeat-get-map-sym`, `repeat-check-key`, `repeat-check-map` with the minibuffer state, `repeat-exit-key`, `repeat-exit-timeout`, `repeat-in-progress`, `repeat-map`, `repeat-exit`, `repeat-echo-function` with `repeat-echo-message` and `repeat-echo-message-string` (hints through `rmc--add-key-description`));
the repeat properties as `command_type` components (`repeat_map`, `repeat_continue`, `repeat_continue_all`, `repeat_check_key` with `PROPERTY_*`, `repeat_exit_timeout`, `repeat_hint`),
keymap_mod's `set_repeat_map` (defvar-keymap's `:repeat` and `:exit`);
the repeat maps of ported commands: `undo-repeat-map`,
`buffer-navigation-repeat-map`,
`next-error-repeat-map`,
`page-navigation-repeat-map` (bindings.el),
`other-window-repeat-map` (window.el, with `other-window-backward`),
`comint-repeat-map`,
`shell-repeat-map`,
`winner-repeat-map`

## Deferred

repeating with the arguments read in the minibuffer (`command-history`),
keyboard macros as commands,
`repeat-echo-mode-line`,
the faces of the hint,
`repeat-message-function`,
a list as `repeat-on-final-keystroke`,
`describe-repeat-maps`,
the repeat maps of commands not ported (window resizing and rotating, tab bar, outline, image; `smerge-repeat-map` came with simple_merge_mode_mod)

## Tests

test_repeat_mod (ports repeat-tests-check-key, -exit-command, -exit-key, -keep-prefix, -continue, -continue-another, -hints without faces; own test of C-x z)
