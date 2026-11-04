# autorevert_mod

- Module: autorevert_mod
- Upstream: autorevert.el
- State: partial

## Ported

`auto-revert-mode`,
`auto-revert-tail-mode`,
`global-auto-revert-mode` (with `turn-on-auto-revert-mode`, `turn-on-auto-revert-tail-mode`, their hooks and lighters),
`auto-revert-set-timer`,
`auto-revert-buffers` (polled buffers, `auto-revert-remaining-buffers`, `auto-revert-stop-on-user-input`),
`auto-revert-buffer`,
`auto-revert-handler` (`buffer-stale-function`, keeping point and window points at the end, `vc-refresh-state`, `auto-revert-check-vc-info`),
`auto-revert-tail-handler` (inserting the new tail of the file),
the notification watches (`auto-revert-notify-add-watch`, `-rm-watch`, `-handler`, `auto-revert-notify-exclude-dir-regexp`, `auto-revert-use-notify` made local on failure, the lockout with `auto-revert--lockout-interval`),
`auto-revert-avoid-polling`,
`global-auto-revert-non-file-buffers`,
`global-auto-revert-ignore-modes`,
`global-auto-revert-ignore-buffer`,
`inhibit-auto-revert` (a procedure taking the body),
`auto-revert-active-p`,
the `find-file-hook` and `after-set-visited-file-name-hook` functions;
the modes and the watch descriptor are permanent-local

## Deferred

remote files,
a test as `global-auto-revert-ignore-buffer`,
the :set functions of the options (Customize),
`auto-revert-debug`,
`auto-revert-load-hook` (obsolete)

## Tests

test_autorevert_mod (ports auto-revert-test00-auto-revert-mode, -test02-auto-revert-deleted-file, -test03-auto-revert-tail-mode, -test05-global-notify, -test06-write-file, -test08-auto-revert-inhibit-auto-revert, local files)
