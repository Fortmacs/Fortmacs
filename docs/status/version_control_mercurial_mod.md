# version_control_mercurial_mod

- Module: version_control_mercurial_mod
- Upstream: vc/vc-hg.el
- State: partial

## Ported

`vc-hg-registered`,
`vc-hg-state`,
`vc-hg-state-slow`,
`vc-hg-working-revision`,
`vc-hg-working-revision-symbol`,
`vc-hg--active-bookmark-internal`,
`vc-hg--run-log`,
`vc-hg--symbolic-revision`,
`vc-hg-symbolic-revision-styles` (the active bookmark and templates),
`vc-hg-use-file-version-for-mode-line-version`,
`vc-hg-mode-line-string`,
`vc-hg-root`,
`vc-hg-responsible-p`,
`vc-hg-checkout-model`,
`vc-hg-create-repo`,
`vc-hg-register`,
`vc-hg-unregister`,
`vc-hg-find-revision`,
`vc-hg-find-ignore-file`,
`vc-hg-checkout`,
`vc-hg-revert`,
`vc-hg-revert-files`,
`vc-hg-delete-file(s)`,
`vc-hg-rename-file`,
`vc-hg-diff`,
`vc-hg-revision-table`/completion,
`vc-hg-previous-revision`,
`vc-hg-next-revision` (the `next_revision` slot),
`vc-hg-checkin` and `vc-hg--checkin`,
`vc-hg--extract-headers`,
`vc-hg-log-edit-mode` (C-c C-e `vc-hg-log-edit-toggle-amend`),
`vc-hg-print-log`,
`vc-hg-log-view-mode`,
`vc-hg-expanded-log-entry`,
`vc-hg-log-format`,
`vc-hg-log-graph`,
`vc-hg-root-log-format`,
`vc-hg-log-switches`,
`vc-hg-dir-status-files`,
`vc-hg-after-dir-status`,
`vc-hg-dir-printer`,
`vc-hg-dir-extra-headers`,
`vc-hg-command` (`vc-hg--command-1`),
`vc-hg--program-version`,
`vc-hg-program`,
`vc-hg-global-switches`,
`vc-hg-diff-switches`,
`vc-hg-revert-switches`;
vc.el `vc-default-show-log-entry`

## Deferred

`vc-hg-state-fast` and the native data structure reading (`vc-hg-parse-hg-data-structures`, the dirstate search and its cache, the requirements check, the .hgignore parsing: since Mercurial 5.7 new repositories require `share-safe`, which upstream does not understand, so it runs `hg status` anyway),
the functions of `vc-hg-symbolic-revision-styles`,
annotate (`vc-hg-annotate-command`, `-time`, `-extract-revision-at-line`, `vc-hg-annotate-switches`, `vc-hg-annotate-show-revision-numbers`),
tags and branches (`vc-hg-create-tag`, `vc-hg-retrieve-tag`, `vc-hg-create-bookmark`, `vc-hg-working-branch`, `vc-hg--bookmarks`, `vc-hg-trunk-or-topic-p`, `vc-hg-topic-outgoing-base`),
pull,
push and merge (`vc-hg--pushpull`, `vc-hg-merge-branch`, `vc-hg-incoming-revision`, `vc-hg-mergebase`, `vc-hg-error-regexp-alist`),
`vc-hg-clone`,
`vc-hg-repository-url`,
working trees,
patches (`vc-hg-checkin-patch`, `vc-hg-prepare-patch`),
asynchronous checkins (`vc-hg-async-checkins`, `vc-hg--async-command`),
cherry-picks and history rewriting,
`vc-hg-region-history` and its mode,
`vc-hg-file-name-changes`,
`vc-hg-find-file-hook` with `vc-hg-resolve-when-done` and `vc-hg-resolve-conflicts` (Smerge mode),
the menus (`vc-hg-extra-menu`),
`vc-hg-revision-granularity` and `vc-hg-update-on-retrieve-tag` (no slot yet),
the faces and help-echo of the mode-line string,
`truncate-lines` in short logs

## Tests

test_version_control_mercurial_mod (ports vc-hg-tests.el vc-hg-after-dir-status and vc-hg-diff-revision-arguments, vc-tests.el vc-test-hg00-create-repo to -hg06-version-diff and -hg10-rename-directory; own: the version, the mode line, previous and next revisions, C-x v l, C-x v L, C-x v d; the tests running hg skip without it)
