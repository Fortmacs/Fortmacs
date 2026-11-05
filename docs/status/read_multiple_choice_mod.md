# read_multiple_choice_mod

- Module: read_multiple_choice_mod
- Upstream: emacs-lisp/rmc.el
- State: partial

## Ported

`read-multiple-choice` (the minibuffer path; short answers with `read-char-choice-use-read-key`, with recenter, scroll and edit answers; long answers with completion; C-g quits),
`rmc--add-key-description` (terminal form),
`rmc--show-help` (the help string or the generated multi-column help, in a window split off the bottom, removed afterwards)

## Deferred

dialog boxes,
touch-screen events,
faces

## Tests

test_read_multiple_choice_mod (ports test-rmc--add-key-description/non-graphical-display, test-read-multiple-choice, the layout of test-read-multiple-choice-help)
