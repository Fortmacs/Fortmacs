# read_y_or_n_each_mod

- Module: read_y_or_n_each_mod
- Upstream: emacs-lisp/map-ynp.el
- State: partial

## Ported

`map-y-or-n-p` (minibuffer and read-key paths, help, commands among the answers, "Type C-h for help", the echo area restored; body in the submodule read_y_or_n_each_ask_mod) with Fortran prompter and actor procedures;
`read-answer` (short answers as keys with `read-answer-short`, long ones in the minibuffer, help)

## Deferred

ACTION-ALIST,
dialog boxes,
switch-frame events

## Tests

test_files_mod,
test_directory_editor_mode_mod
