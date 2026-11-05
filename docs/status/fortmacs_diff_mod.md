# fortmacs_diff_mod

- Module: fortmacs_diff_mod
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
`ediff-files-internal` (MERGE-BUFFER-FILE, the ancestor's last directory),
`ediff--buffer-file-name`,
`ediff-buffers-internal` (the buffers in temporary files, deleted after the setup; MERGE-BUFFER-FILE),
`ediff-merge-files` (`ediff-merge`),
`ediff-merge-files-with-ancestor` (`ediff-merge-with-ancestor`),
`ediff-merge-buffers`,
`ediff-merge-buffers-with-ancestor`,
`ediff-merge-on-startup`,
`ediff-get-default-file-name` (outside Dired);
STARTUP-HOOKS as a `hook_type`

## Deferred

the directory, window, region, patch and revision jobs (`ediff-directories`, `-directories3`, `-directory-revisions`, `ediff-merge-directories` and `-with-ancestor`, `ediff-merge-directory-revisions` and `-with-ancestor`, `ediff-windows-wordwise`/`-linewise`, `ediff-regions-wordwise`/`-linewise`, `ediff-patch-file`/`-buffer`, `ediff-revision`, `ediff-merge-revisions` and `-with-ancestor`: ediff-mult.el, ediff-vers.el and ediff-ptch.el come next),
`ediff-last-merge-autostore-dir`,
`ediff-version`,
`ediff-documentation` (the Ediff manual),
the `*-command` functions of the command line,
`ediff-load-version-control`,
`ediff-set-read-only-in-buf-A`,
compressed and remote files in `ediff-find-file` (`ediff--magic-file-name`, `ediff--startup-hook`),
Dired's files in `ediff-get-default-file-name` (FILENO) and `dired-dwim-target-defaults`,
the buffer reordering of `(save-window-excursion (other-window 1))` in the readers of `ediff-buffers`, `ediff-buffers3`, `ediff-merge-buffers` and `ediff-merge-buffers-with-ancestor`,
auto-save files in `ediff-current-file`

## Tests

test_fortmacs_diff_utilities_mod (own: ediff-files); test_fortmacs_diff_merging_mod (own: ediff-merge-buffers, ediff-merge-buffers-with-ancestor, MERGE-BUFFER-FILE)
