# vc_svn_mod

- Module: vc_svn_mod
- Upstream: vc/vc-svn.el
- State: partial

## Ported

`vc-svn-registered`,
`vc-svn-state`,
`vc-svn-parse-status`,
`vc-svn-working-revision`,
`vc-svn-root` (and `vc-svn-responsible-p`),
`vc-svn-checkout-model`,
`vc-svn-create-repo` (svnadmin create and a checkout),
`vc-svn-register`,
`vc-svn-checkin`,
`vc-svn-find-revision`,
`vc-svn-checkout`,
`vc-svn-update`,
`vc-svn-delete-file`,
`vc-svn-rename-file`,
`vc-svn-revert`,
`vc-svn-previous-revision`,
`vc-svn-revision-table` (revision completion),
`vc-svn-diff`,
`vc-svn-print-log`,
`vc-svn-log-view-mode`,
`vc-svn-dir-status-files`,
`vc-svn-after-dir-status`,
`vc-svn-dir-extra-headers`,
`vc-svn-command`,
`vc-svn-program`,
`vc-svn-global-switches`,
`vc-svn-register-switches`,
`vc-svn-diff-switches`,
`vc-svn-admin-directory` (the mode line is vc-default-mode-line-string, the log entry vc-default-show-log-entry, as upstream)

## Deferred

`vc-svn-annotate-command` and the annotate functions,
`vc-svn-annotate-switches` and `vc-svn-next-revision` (vc-annotate),
`vc-svn-create-tag` and `vc-svn-retrieve-tag` (tags and branches),
`vc-svn-merge-file`,
`vc-svn-merge` and `vc-svn-merge-news` (merging),
`vc-svn-ignore` and `vc-svn-ignore-completion-table` (no `ignore` backend function yet: `vc_ignore` uses vc-default-ignore, so C-x v G reports `vc_not_supported` under SVN),
`vc-svn-find-file-hook`,
`vc-svn-resolve-when-done`,
`vc-svn-resolve-conflicts` and the `vc-svn-status` file property only they read (Smerge mode, step 48),
`vc-svn-modify-change-comment`,
`vc-svn-header` and `vc-svn-check-headers` (vc-insert-headers),
`vc-svn-revision-granularity`,
`vc-svn-update-on-retrieve-tag`,
`vc-svn-valid-symbolic-tag-name-p`,
`vc-svn-valid-revision-number-p`,
`vc-svn-find-admin-dir`,
`vc-svn-repository-url`,
`vc-svn-clone`;
Not applicable: SVN_ASP_DOT_NET_HACK's "_svn" (Windows),
the MSYS file names of `vc-svn-create-repo`

## Tests

test_vc_svn_mod (ports vc-tests.el vc-test-svn00-create-repo to -svn06-version-diff (-svn05: `svn remove` of an added file fails without --force, as upstream, so OK-IF-ALREADY-EXISTS gives an error condition); own: parsing `svn status -v`, the previous revision, C-x v l in SVN-Log-View mode, C-x v =, C-x v d; skipped without svn and svnadmin)
