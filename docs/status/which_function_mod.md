# which_function_mod

- Module: which_function_mod
- Upstream: progmodes/which-func.el
- State: partial

## Ported

`which-function-mode` (global; its idle timer with `which-func-update-delay`, `which-func-ff-hook` in every buffer and on `after-change-major-mode-hook`),
`which-func-mode` (buffer-local),
`which-func-try-to-enable` (`which-func-modes` as `which_func_modes_type`),
`which-func--disable`,
`which-func-ff-hook` (`which-func-non-auto-modes`, `which-func-maxout`, `buffer-saved-size`, `imenu-unavailable`),
`which-func-update`,
`which-func-update-1` (which-func-table per window),
`which-function` (`which-func-functions` as a `string_result_hook_type`, `add-log-current-defun`, the Imenu index with `which-function-imenu-failed`, `which-func-imenu-joiner-function` given the entry and its submenus, `which-func-cleanup-function`),
`which-func-unknown`,
the face `which-func`,
the mode line element of `mode-line-misc-info` with `which-func-format`'s default "[NAME]" (`format_which_func`)

## Deferred

the header line (`which-func-display`, `which-func--use-header-line`, `which-func--use-mode-line`, `which-func--header-line-remove`: header lines have no `:eval` segments),
`which-func-format` as a variable (mode-line constructs),
`which-func-keymap` and the mouse-face and help-echo of the name (no mouse),
`which-func-update-ediff-windows` (Ediff);
the modes' functions waiting on unported code: Diff mode's `diff-current-defun` (so `which-function` there takes the add-log heuristic, then the index) and js-mode's `js--which-func-joiner`

## Tests

test_which_function_mod (ports which-func-tests-toggle, with a Shell-script buffer for the Emacs Lisp file and without the header line; own: `which-function` through add-log, Imenu and `which-func-functions`, the cleanup, the modes, the mode line element)
