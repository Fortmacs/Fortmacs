# ediff_mod

- Module: ediff_mod
- Upstream: vc/ediff.el
- State: partial

## Ported

`ediff-files` (M-x ediff_files and `ediff`: the file readers with `ediff-use-last-dir`, a directory as file B),
`ediff-files3` (`ediff3`),
`ediff-buffers` (`ebuffers`, JOB-NAME),
`ediff-buffers3` (`ebuffers3`),
`ediff-current-file`,
`ediff-backup`,
`ediff-find-file` (readable, not a directory, `ediff-last-dir-A`/`-B`/`-C`/`-ancestor`, `ediff-verify-file-buffer`),
`ediff-files-internal`,
`ediff--buffer-file-name`,
`ediff-buffers-internal` (the buffers in temporary files, deleted after the setup),
`ediff-get-default-file-name` (outside Dired);
STARTUP-HOOKS as a `hook_type`

## Deferred

the directory, window, region, merge, patch and revision jobs (`ediff-directories`, `-directories3`, `-directory-revisions`, `ediff-windows-wordwise`/`-linewise`, `ediff-regions-wordwise`/`-linewise`, `ediff-merge-files` and the other `ediff-merge-*`, `ediff-patch-file`/`-buffer`, `ediff-revision`; the next PR does merging and ediff-mult.el),
`ediff-version`,
`ediff-documentation` (the Ediff manual),
the `*-command` functions of the command line,
`ediff-load-version-control`,
`ediff-set-read-only-in-buf-A`,
compressed and remote files in `ediff-find-file` (`ediff--magic-file-name`, `ediff--startup-hook`),
Dired's files in `ediff-get-default-file-name` (FILENO) and `dired-dwim-target-defaults`,
the buffer reordering of `(save-window-excursion (other-window 1))` in the readers of `ediff-buffers` and `ediff-buffers3`,
auto-save files in `ediff-current-file`

## Tests

test_ediff_util_mod (own: ediff-files)
