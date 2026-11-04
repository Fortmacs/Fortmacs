# vc_mod

- Module: vc_mod
- Upstream: vc/vc.el (+ dired-aux.el `dired-vc-deduce-fileset`, diff-mode.el `diff-vc-deduce-fileset`)
- State: partial

## Ported

`vc-deduce-fileset` (file buffers, Dired, Diff mode, directories),
`vc-deduce-backend`,
`vc-root-dir`,
`vc-switches` (vc-BACKEND-OP-switches for diff and register) and the `vc-*-switches` options,
`vc-next-action` (C-x v v: register, mark resolved, check out, commit; patches deferred),
`vc-checkin` (the log buffer or COMMENT, `vc-checkin-hook`, version backups deleted),
`vc-register` (C-x v i),
`vc-ignore` (C-x v G),
`vc-checkout`,
`vc-mark-resolved`,
`vc-diff-internal`,
`vc-diff` (C-x v =, C-u for revisions),
`vc-version-diff`,
`vc-root-diff` (C-x v D),
`vc-root-version-diff`,
`vc-read-revision`,
`vc-revision-other-window` (C-x v ~) and `vc-find-revision`,
`vc-revert` (C-x v u, `vc-revert-show-diff`),
`vc-revert-file`,
`vc-revert-files`,
`vc-delete-file` (C-x v x),
`vc-rename-file` (C-x v R),
`vc-file-tree-walk`,
`vc-checkout-hook`,
`vc-checkin-hook`,
`vc-before-checkin-hook`,
the logs: `vc-print-log` (C-x v l),
`vc-print-root-log` (C-x v L),
`vc-print-log-internal`,
`vc-log-internal-common` (with `vc-default-show-log-entry`),
`vc-log-short-style`,
`vc-log-show-limit`,
`vc-log-view-type`,
`vc-log-finish-functions`,
`vc-shrink-buffer-window`,
`vc-message-unresolved-conflicts`;
tags and branches (vc_branches_mod): `vc-create-tag` (C-x v s; C-u for a branch),
`vc-create-branch` (C-x v b c),
`vc-retrieve-tag` (C-x v r; C-u for a branch),
`vc-switch-branch` (C-x v b s),
`vc-retrieve-tag-hook`;
`vc-pull` (C-x v +, and its alias `vc-update`),
`vc-push` (C-x v P),
`vc--incoming-revision` (remembered per repository, forgotten after a pull or push);
the logs (vc_log_mod): `vc-print-fileset-branch-log` (C-x v b l),
`vc-print-root-branch-log` (C-x v b L, and its obsolete alias `vc-print-branch-log`),
`vc--read-branch-to-log`,
`vc-root-log-incoming` (C-x v I) and `vc-root-log-outgoing` (C-x v O, C-x v E L) with their obsolete aliases `vc-log-incoming` and `vc-log-outgoing`,
`vc--maybe-read-upstream-location`,
`vc-incoming-outgoing-internal`,
`vc-default-log-incoming`,
`vc-default-log-outgoing`

## Deferred

committing patches and `vc-async-checkin`,
the buttons at the end of a log (button.el) and logs across renames,
`vc-print-change-log`,
`vc-print-root-change-log`,
search logs (`vc-log-search`),
`vc-log-mergebase` and `vc-diff-mergebase`,
the incoming and outgoing diffs (`vc-diff-incoming`, `vc-diff-outgoing`, `vc-root-diff-incoming`, `vc-root-diff-outgoing`, `vc-diff-outgoing-and-edited`),
unintegrated changes (C-x v T),
`vc--count-outgoing`,
the backends' own `log-incoming` and `log-outgoing` (deprecated upstream; no ported backend has them),
`vc-pull` on backends with `merge-news` or the locking model,
`vc-pull-and-push`,
`vc-default-retrieve-tag` and `vc-tag-precondition` (file-granular backends),
merge (`vc-merge`),
working trees,
cherry-picks and history rewriting,
`vc-find-revision-no-save`,
`vc-coding-system-for-diff`,
`vc-diff-restore-buffer`,
locking backends,
changing a file's backend

## Tests

test_vc_mod (ports vc-test-git05-rename-file, -git06-version-diff, -git10-rename-directory; own: C-x v = in Diff mode, reverting)
