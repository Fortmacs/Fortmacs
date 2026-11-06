# warnings_mod

- Module: warnings_mod
- Upstream: emacs-lisp/warnings.el (+ subr.el `delay-warning`, `display-delayed-warnings`, `collapse-delayed-warnings`, `delayed-warnings-hook`; keyboard.c `delayed-warnings-list`; emacs.c `after-init-time`)
- State: partial

## Ported

`display-warning` (a type of one name or a list of names given as an array of `string_type`; the *Warnings* buffer, created in Special mode, read-only and without undo; another buffer by name),
`lwarn` (`display_formatted_warning`, with optional format arguments),
`warn` (the type "fortmacs"),
`warning-levels` (an array of `warning_level_type` records; the WARNING_EMERGENCY entry calls `ding`),
the levels as named constants (WARNING_EMERGENCY, WARNING_ERROR, WARNING_WARNING, WARNING_DEBUG),
`warning-numeric-level`,
`warning-minimum-level` and `warning-minimum-log-level` (choice settings),
`warning-suppress-types`, `warning-suppress-log-types` and `warning-inhibit-types` (`warning_types_type` lists, `%append` and `%clear`),
`warning-suppress-p` (`is_warning_suppressed`),
`warning-display-at-bottom` (`display-buffer--maybe-at-bottom` with a window of at most 10 lines, the last warning at the bottom),
`warning-prefix-function` (a procedure pointer),
`warning-series` (a choice setting with `%procedure` and `%marker`),
`warning-fill-prefix`,
`warning-fill-column`,
`warning-type-format`,
`warning--display-buffer` (`display_warning_buffer`);
`delay-warning`,
`delayed-warnings-list` (`delayed_warnings`, keyboard_mod; run after `post-command-hook` and at startup after the init file),
`delayed-warnings-hook`,
`display-delayed-warnings`,
`collapse-delayed-warnings`,
`after-init-time` (`after_initialization_time`, startup_mod)

## Deferred

the suppress button before each warning and `warnings-suppress` (icons.el's `warnings-suppress` icon and `buttonize` of a string are not ported, and saving the choice needs `customize-save-variable`),
the obsolete `warning-level-aliases` (`display-warning-minimum-level` and `log-warning-minimum-level` with it),
a buffer (not its name) as BUFFER-NAME,
popping up a frame for `pop-up-frames` in `display-buffer--maybe-at-bottom`,
the `category` entry of the display action (`display-buffer-alist` is not ported);
Not applicable: the batch-mode output to the echo area and *Messages* and the daemon's first client frame (Fortmacs has no batch mode and no daemon)

## Tests

test_warnings_mod (ports test-warning-suppress-p, test-display-warning (the buffer; *Messages* only in batch mode), test-display-warning/warning-minimum-level (interactively) and test-display-warning/warning-minimum-log-level; own tests of the *Warnings* buffer, `warn` and `display_formatted_warning`, the suppression lists, `warning_series` and the delayed warnings)
