# saveplace_mod

- Module: saveplace_mod
- Upstream: saveplace.el
- State: partial

## Ported

`save-place-mode` (global, its variable buffer-local),
`save-place-local-mode`,
`save-place-alist` as an array of `save_place_type` (a position, or a Dired file),
`save-place-to-alist` (on `kill-buffer-hook`; files and Dired directories, `save-place-ignore-files-regexp`, `save-place-abbreviate-file-names`, position 1 left out),
`save-places-to-alist`,
`save-place-load-alist-from-file` (`save-place-limit`, `save-place--normalize-alist`),
`save-place-alist-to-file` (`save-place-forget-unreadable-files`, the option as `save_place_forget_unreadable_files_option`),
`save-place-forget-unreadable-files` (`save-place-skip-check-regexp`, `save-place-save-skipped`),
`save-place-find-file-hook` with `save-place-after-find-file-hook`,
`save-place-dired-hook` on `dired-initial-position-hook`,
`save-place-kill-emacs-hook`,
`save-place-autosave-interval`,
`save-place-version-control`

## Deferred

Hexl mode's addresses (hexl.el),
the :set function of `save-place-abbreviate-file-names` (the table is rewritten when next loaded)

## Tests

test_saveplace_mod (ports saveplace-tests.el, its resource file in namelist syntax)
