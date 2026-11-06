# log_view_mod

- Module: log_view_mod
- Upstream: vc/log-view.el
- State: partial

## Ported

`log-view-mode` (read-only, the font lock of file and message lines, `log-view-file-re`, `log-view-message-re`, `log-view-per-file-logs`, `log-view-expanded-log-entry-function`, `log-view-vc-fileset`, `log-view-vc-backend` as buffer-local values),
`log-view-msg-next`/`-prev` (n, p, TAB, <backtab>),
`log-view-file-next`/`-prev`,
`log-view-goto-rev`,
`log-view-msg-and-diff-next`/`-prev` (M-n, M-p),
`log-view-current-file`,
`log-view-current-entry`,
`log-view-current-tag`,
marking (`log-view-mark-entry` m, `log-view-unmark-entry` u, `log-view-unmark-all-entries` U, `log-view-toggle-mark-entry`, `log-view-get-marked`),
`log-view-toggle-entry-display` (RET),
`log-view-display-entry-and-diff` (M-RET),
`log-view-beginning-of-defun`,
`log-view-end-of-defun`,
`log-view-find-revision` (f),
`log-view-diff` (d, =),
`log-view-diff-changeset` (D),
`log-view-copy-revision-as-kill` (w),
`log-view-annotate-version` (a, calling `version_control_annotate` through the pointer `log_view_version_control_annotate`),
the faces,
`log-view-expanded-log-entry-function` as a buffer-local procedure setting (`%procedure`, `%set_local`, `%get_current`, `%clear_local`)

## Deferred

`log-view-modify-change-comment` (e),
cherry-picks,
reverts and deleting revisions (C, R, x, X),
PCL-CVS linkage,
`log-view-extract-comment`,
restoring marks on revert,
`beginning-of-defun-function` (lisp.el's subprogram motion),
the menu,
project buffers,
directory-local variables

## Tests

test_version_control_mod (own: Git logs, n and p, d, m, U, RET)
