# subr_mod

- Module: subr_mod
- Upstream: subr.el
- State: partial

## Ported

`define-mail-user-agent` (the type-bound `define` of `mail_user_agent_type`),
`prepare-change-group`,
`activate-change-group`,
`accept-change-group`,
`cancel-change-group` (in the submodule subr_change_group_mod; upstream's atomic-change-group is written with them),
`add-to-history` (by name through `find_history`, then `history_setting_type%append`: MAXELT, KEEP-ALL, the history's `%length` (upstream's `history-length` property; HISTORY_LENGTH_DEFAULT, HISTORY_LENGTH_UNLIMITED), `history-delete-duplicates`, a name that is no history is `void-variable`),
`read-key` (single events),
`y-or-n-p` (minibuffer path with `y-or-n-p-map`, and the `y-or-n-p-use-read-key` path; submodule subr_read_mod),
`query-replace-map` (replace.el) as a keymap with all its events,
`forward-symbol`,
`find-tag-default` (thingatpt.el bounds of a symbol),
`find-tag-default-as-regexp`,
`find-tag-default-as-symbol-regexp`,
`abort-recursive-edit` (keyboard.c command),
`global-set-key`,
`match-string`,
`match-string-no-properties`,
`string-match-p`,
`looking-at-p`,
`looking-back`,
`match-substitute-replacement`,
`read-number` (the reader's integer syntax, a list of defaults; in the submodule subr_read_mod),
`add-hook` (with depths),
`remove-hook`,
`run-mode-hooks`,
`delay-mode-hooks`,
`provided-mode-derived-p`,
`derived-mode-p`,
`delayed-after-hook-functions` (submodule subr_hooks_mod),
`with-silent-modifications` (`silent_modifications_begin`/`_end`);
`shell-quote-argument` (POSIX),
`read-char-from-minibuffer` (CHARS, HISTORY, `read-char-from-minibuffer-map` with its insert commands, `help-form`),
`read-char-choice` (`read-char-choice-with-read-key`, INHIBIT-KEYBOARD-QUIT, `help-form`),
pulsing progress reporters (MIN-VALUE and MAX-VALUE omitted),
`subregexp-context-p`;
`add-to-invisibility-spec`,
`remove-from-invisibility-spec`,
`replace-string-in-region`,
`replace-regexp-in-region`;
`version-to-list` (the function `version`, whose `version_type` result holds the numbers and the parse status: invalid syntax an `error` error condition in its `status` component; with `version-separator` and `version-regexp-alist` as constants),
`version-list-<`,
`version-list-<=`,
`version-list-=`,
`version<`,
`version<=`,
`version=` (the operators `<`, `<=`, `==`, with `>`, `>=`, `/=`, between two `version_type` values: `version(a) <= version(b)`; owner 2026-11-04; a comparison with an invalid version is false, except `/=`; the procedures behind them are private, in C-h f as `version_less`, `version_less_or_equal`, `version_equal` and the others with the operator call line)

## Deferred

help-form (outside `read-char-from-minibuffer` and `read-char-choice`),
change groups of several buffers,
`history-length` t,
floats in `read-number`,
minibuffer-scroll-other-window(-down),
`read-key` key sequences,
hook depths of local values,
`derived-mode-add-parents`,
mode aliases,
`hack-local-variables` in `run-mode-hooks`,
`version-separator` and `version-regexp-alist` as settings

## Tests

test_subr_mod (ports subr-test-version-parsing without its cases for another version-separator, subr-test-version-list-<, subr-test-version-list-<=),
test_files_mod,
test_keymap_mod,
test_regex_mod,
test_derived_mod (ports subr-tests-add-hook-depth, provided-mode-derived-p)
