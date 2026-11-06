# log_edit_mod

- Module: log_edit_mod, log_edit_version_control_mod (submodule)
- Upstream: vc/log-edit.el
- State: partial

## Ported

`log-edit` (SETUP, the PARAMS as arguments: list function, diff function, VC backend), `log-edit-callback` and `log-edit-diff-function` as procedure settings (`%set_local`, `%get_current`) and `log-edit-vc-backend` as a forwarded integer setting (`%set_local`, `%value`), cleared by kill-all-local-variables,
`log-edit-mode` (font lock of headers and GNU-style entries, `log-edit-do-auto-fill`, `log-edit-fill-entry` as `fill-paragraph-function`),
`log-edit-done` (empty headers, blank lines, final newline, `log-edit-confirm`, `log-edit-done-hook`, the callback),
`log-edit-kill-buffer`,
`log-edit-files`,
`log-edit-show-files`,
`log-edit-show-diff`,
`log-edit-diff-fileset`,
the comment ring (M-n, M-p, M-r, M-s),
`log-edit-insert-message-template`,
`log-edit-insert-cvs-template`,
`log-edit-insert-cvs-rcstemplate`,
`log-edit-insert-filenames(-without-changelog)`,
`log-edit-add-field`,
`log-edit-insert-changelog` and its ChangeLog entry search,
`log-edit-toggle-header`,
`log-edit-set-header`,
`log-edit-extract-headers`,
`log-edit--toggle-amend`,
`log-edit-set-common-indentation`,
`log-edit-rewrite-fixes` (two strings),
`log-edit-done-strip-cvs-lines`,
`log-edit-mode-help`,
`log-edit-generate-changelog-from-diff` (C-c C-w, `generate_log_edit_changelog_from_diff` in log_edit_version_control_mod: the diff function's buffer is caught through `display_buffer_overriding_action` with an action function and `display-buffer-no-window`, ported to window_mod for it)

## Deferred

`log-edit-add-to-changelog` and `log-edit-comment-to-change-log` (their verb-first Fortran names wait for the owner),
the BUFFER argument (PCL-CVS),
`log-edit-maybe-show-diff`,
`log-edit-diff-patch`,
the menu and tool bar,
help-echo of the GNU-style keywords,
directory-local variables

## Tests

test_log_edit_mod (ports log-edit-tests.el: the eight log-edit-fill-entry tests and the four log-edit-done-strip-cvs-lines tests),
test_add_log_mod (own: `log-edit-generate-changelog-from-diff`),
test_buffer_local_setting_mod (own: `log-edit-callback`, `log-edit-diff-function` and `log-edit-vc-backend` per buffer)
