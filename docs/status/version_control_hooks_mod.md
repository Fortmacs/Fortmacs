# version_control_hooks_mod

- Module: version_control_hooks_mod
- Upstream: vc/vc-hooks.el
- State: partial

## Ported

the backend registry (`vc-handled-backends`, a `version_control_backend_type` of procedure pointers per backend),
the file properties (`vc-file-setprop`/`-getprop`/`-clearprops`),
`vc-registered`,
`vc-backend`,
`vc-responsible-backend`'s probing,
`vc-state` and `vc-state-refresh`,
`vc-up-to-date-p`,
`vc-working-revision`,
`vc-symbolic-working-revision`,
`vc-checkout-model`,
`vc-ignore-dir-regexp`,
`vc-directory-exclusion-list`,
`vc-follow-symlinks` and `vc-follow-link`,
`vc-display-status`,
`vc-consult-headers`,
`vc-make-backup-files`,
version backups (`vc-version-backup-file-name`, `vc-make-version-backup`, `vc-delete-automatic-version-backups`),
`vc-before-save`/`vc-after-save`,
`vc-mode-line` with `vc-default-mode-line-string` (`Git-main`, `Git:main`, `Git@main` ...),
`vc-refresh-state` on `find-file-hook`,
`vc-kill-buffer-hook`,
the vc-*-state faces,
`vc-prefix-map` on C-x v (with C-x v s, r, b c, b l, b L, b s, +, P, I, O, E L),
the per-repository property `vc-incoming-revision` (`version_control--repo-setprop`, `version_control--repo-getprop`),
the backend functions `revision-granularity`,
`update-on-retrieve-tag`,
`create-tag`,
`retrieve-tag`,
`pull`,
`push`,
`incoming-revision` and `mergebase`,
the backend functions of VC-Annotate in `version_control_backend_type` (`annotate_command`, `annotate_time`, `annotate_extract_revision_at_line`, `next_revision`, `file_name_changes`, `revision_granularity`, `annotate_switches`)

## Deferred

the backends other than Git and than Git,
SVN and Hg,
file name handlers,
the faces,
help-echo and mouse menu on the mode-line string,
the menus,
`vc-auto-revert-mode`,
VC-Dir resynchronization,
`vc-use-incoming-outgoing-prefixes` (`vc-incoming-prefix-map`, `vc-outgoing-prefix-map`),
`version_control--repo-clearprops`,
the C-x v keys of commands not ported yet

## Tests

test_version_control_mod (ports vc-test-git00-create-repo, -git01-register, -git02-state, -git03-working-revision, -git04-checkout-model; own: the Git mode line)
