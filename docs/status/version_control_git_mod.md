# version_control_git_mod

- Module: version_control_git_mod
- Upstream: vc/vc-git.el
- State: partial

## Ported

`vc-git-registered`,
`vc-git-state`,
`vc-git-working-revision`,
`vc-git--symbolic-ref`,
`vc-git-mode-line-string`,
`vc-git-root`,
`vc-git-checkout-model`,
`vc-git-create-repo`,
`vc-git-register`,
`vc-git-unregister`,
`vc-git-find-revision`,
`vc-git-find-ignore-file`,
`vc-git-checkout`,
`vc-git-revert`,
`vc-git-revert-files`,
`vc-git-delete-file(s)`,
`vc-git-rename-file`,
`vc-git-mark-resolved`,
`vc-git-diff` (diff-index and diff-tree),
`vc-git-revision-table`/completion,
`vc-git-previous-revision`,
`vc-git--rev-parse`,
`vc-git-symbolic-commit`,
`vc-git--program-version`,
`vc-git-command`,
`vc-git--call`,
`vc-git-program`,
`vc-git-diff-switches`,
`vc-git-revision-complete-only-branches`,
`vc-git-use-literal-pathspecs`,
`vc-git-checkin` (`--only`, the extracted headers),
`vc-git-log-edit-mode` (C-c C-s sign-off, C-c C-n no-verify, the summary length check and faces),
`vc-git-print-log`,
`vc-git-print-log-follow`,
`vc-git-log-switches`,
`vc-git-shortlog-switches`,
`vc-git-root-log-format`,
`vc-git-log-view-mode`,
`vc-git-show-log-entry`,
`vc-git-expanded-log-entry`,
`vc-git-dir-status-files` (its stages),
`vc-git-dir-extra-headers` (branch, tracking, remote, commands in progress),
`vc-git-dir-printer`,
`vc-git-working-branch`,
`vc-git-repository-url`,
`vc-git--git-path`,
`vc-git--out-match`,
`vc-git--out-str`,
`vc-git-revision-granularity`,
`vc-git-update-on-retrieve-tag`,
`vc-git-branches`,
`vc-git--branch-remotes`,
`vc-git--read-start-point`,
`vc-git-create-tag`,
`vc-git-retrieve-tag`,
`vc-git--pushpull` (carriage returns of progress output as newlines, VC-Compilation mode, `compile-command` and `compilation-directory`),
`vc-git-pull`,
`vc-git-push`,
`vc-git-error-regexp-alist`,
`vc-git-incoming-revision`,
`vc-git-mergebase`,
`vc-git-find-file-hook` (Smerge mode on a conflicted file),
`vc-git-resolve-conflicts`,
`vc-git-resolve-when-done`,
`vc-git-conflicted-files`,
`vc-git--cmds-in-progress` (`vc_git_commands_in_progress_type`),
the annotate functions: `vc-git-annotate-command`,
`vc-git-annotate-time`,
`vc-git-annotate-extract-revision-at-line`,
`vc-git-annotate-switches`,
`vc-git-next-revision`,
`vc-git-file-name-changes`;
`vc-git-show-log-entry` tells whether it found the entry

## Deferred

the backend functions of the deferred VC commands (`vc-git-merge-branch`, `vc-git-trunk-or-topic-p`, `vc-git-topic-outgoing-base`, `vc-git-log-search`),
the `compilation-arguments` of a *vc-git* buffer (g there recompiles in *compilation*),
the difftool path of `vc-git-diff`,
the stash headers and commands (button.el),
the coding systems of Git's output,
remote directories

## Tests

test_version_control_mod (ports vc-git-tests.el vc-git-test-program-version-general, -windows, -apple, -other, -invalid-leading-string, -invalid-leading-dot, vc-git-test-branch-remotes; own: C-x v l and C-x v L, tags and branches, branch logs, push, pull, incoming and outgoing logs with a bare remote and a second clone; test_version_control_annotate_mod: `vc-git-annotate-time`)
