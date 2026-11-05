# recent_files_mod

- Module: recent_files_mod
- Upstream: recentf.el
- State: partial

## Ported

`recentf-mode` (global; loads `recentf-save-file`, tracks files on `find-file-hook` and `kill-buffer-hook`, saves on `kill-emacs-hook`, `recentf-auto-cleanup` on mode or after idle seconds, `recentf-autosave-interval`),
`recentf-list` as an array of `recentf_file_type`,
`recentf-add-file`,
`recentf-push`,
`recentf-include-p` and `recentf-keep-p` (`recentf-exclude`, `recentf-keep` as `recentf_check_type` records: a regexp or a test; `recentf-exclude-file-by-extension-p` with `recentf-exclude-ignored-extensions`, `recentf-keep-default-predicate`),
`recentf-filename-handlers` (`abbreviate-file-name`, `file-truename`),
`recentf-max-saved-items`,
`recentf-save-list` and `recentf-load-list` (namelist syntax, `recentf-save-file-modes`, `recentf-initialize-file-name-history`),
`recentf-cleanup`,
`recentf-open` (`recentf`, completing with category `recentf`, `recentf-menu-action`),
`recentf-open-most-recent-file`,
`recentf-show-messages`

## Deferred

tracking on `write-file-functions`,
the menu bar's Open Recent menu and its filters,
the `recentf-open-files` and `recentf-edit-list` dialogs (widget.el),
remote files,
a time string in `recentf-auto-cleanup`,
the warning when the file can't be written,
obsolete functions

## Tests

test_recent_files_mod (own tests: no upstream tests)
