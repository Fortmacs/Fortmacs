# fortmacs_diff_settings_mod

- Module: fortmacs_diff_settings_mod, fortmacs_diff_settings_skip_mod (submodule)
- Upstream: vc/ediff-init.el
- State: partial

## Ported

the session variables of the control buffer as `ediff_session_type` (`ediff-buffer-A`/`-B`/`-C`, `ediff-ancestor-buffer`, `ediff-control-buffer`, the difference vectors, `ediff-current-difference`, `ediff-number-of-differences`, the bounds, the diff and error buffers, `ediff-killed-diffs-alist` as `replaced_regions` of `ediff_replaced_region_type` records, `ediff-highlighting-style`, `ediff-job-name` as a pointer to the command that started the session, and the job classification of `ediff-3way-comparison-job` and the others; its type-bound `check_region_skip`, the merge-region skip check of n and p, is in the submodule fortmacs_diff_settings_skip_mod, see fortmacs_diff_merging_mod),
the buffer types A, B, C and Ancestor as named constants,
`ediff-force-faces`,
`ediff-has-face-support-p`,
`ediff-char-to-buftype`,
`ediff-before-flag-bol`/`-mol`,
`ediff-after-flag-eol`/`-mol`,
`ediff-use-faces`,
`ediff-keep-variants`,
`ediff-highlight-all-diffs`,
`ediff-coding-system-for-read`/`-write`,
`ediff-make-buffers-readonly-at-startup`,
`ediff-show-ancestor`,
`ediff-autostore-merges` (a choice setting: .true., .false., EDIFF_AUTOSTORE_MERGES_GROUP_JOBS_ONLY; each session takes its own value),
`ediff-state-of-merge` (`merge_states` of `ediff_merge_state_type` records, with the internal EDIFF_STATE_OF_MERGE_* states),
`ediff--show-ancestor-orig`,
`ediff-merge-store-file` (a session component),
`ediff-merge-filename-prefix`,
`ediff-meta-buffer` and `ediff-meta-session-number` (session components),
the metajob tests (`ediff-ancestor-metajob`, `ediff-collect-diffs-metajob`, `ediff-merge-metajob`, `ediff-metajob3`, `ediff-comparison-metajob3`, of the ported directory jobs),
`ediff-abbrev-jobname` (the ported jobs),
`ediff-strip-last-dir`,
`ediff-no-emacs-help-in-control-buffer` (`ediff_fortmacs_help_in_control_buffer`, named positively: `.true.`, the default, keeps C-h for help),
`ediff-temp-file-mode`,
the hooks (`ediff-before-setup-hook`, `-before-setup-windows-hook`, `-after-setup-windows-hook`, `-startup-hook`, `-select-hook`, `-unselect-hook`, `-prepare-buffer-hook`, `-mode-hook`, `-keymap-setup-hook`, `-display-help-hook`, `-quit-hook`, `-cleanup-hook`),
the 16 faces (current, fine, even and odd differences of A, B, C and Ancestor),
`ediff-KILLED-VITAL-BUFFER`,
`ediff-NO-DIFFERENCES`,
`ediff-BAD-DIFF-NUMBER`,
`ediff-barf-if-not-control-buffer`,
`ediff-in-control-buffer-p`,
`ediff-with-current-buffer`,
`ediff-valid-difference-p`,
`ediff-kill-buffer-carefully`,
`ediff-background-face`,
`ediff-paint-background-regions(-in-one-buffer)`,
`ediff-set-overlay-face`,
`ediff-move-overlay`

## Deferred

control frames,
the mouse and the toolbar (`ediff-grab-mouse`, `ediff-reset-mouse`, `ediff-before-setup-control-frame-hook` and `-after-`),
`ediff-suspend-hook`,
`ediff-version-control-package`,
`ediff-metachars`,
the metajob tests of the revision, patch and file group jobs (`ediff-revision-metajob`, `ediff-patch-metajob`, `ediff-one-filegroup-metajob`),
the regexp filters' variables,
`ediff-start-narrowed`,
`ediff-quit-widened`,
`ediff-temp-indirect-buffer`,
`ediff-strip-mode-line-format`,
the faces' stipples,
`ediff-region-help-echo`;
not applicable: `ediff-load-hook` (obsolete)

## Tests

through test_fortmacs_diff_diff_mod, test_fortmacs_diff_utilities_mod and test_fortmacs_diff_multiple_mod
