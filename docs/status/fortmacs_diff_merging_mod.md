# fortmacs_diff_merging_mod

- Module: fortmacs_diff_merging_mod
- Upstream: vc/ediff-merg.el
- State: partial

## Ported

the procedures that act on one session, as type-bound procedures of `fortmacs_diff_session_type` (panel 14) in fortmacs_diff_settings_merging_mod, a submodule of fortmacs_diff_settings_mod,
`ediff-quit-merge-hook` (its default function `maybe_save_and_delete_fortmacs_diff_merge` is added by fortmacs_diff_utilities_mod),
`ediff-default-variant` (a choice setting: FORTMACS_DIFF_DEFAULT_VARIANT_DEFAULT_A, _DEFAULT_B, _COMBINED),
`ediff-combination-pattern` (an array of `fortmacs_diff_combination_part_type` records: a delimiter and a buffer type, 0 after the last delimiter),
`ediff-show-clashes-only`,
`ediff-skip-merge-regions-that-differ-from-default`,
`ediff-merge-region-is-non-clash`,
`ediff-merge-region-is-non-clash-to-skip` and
`ediff-skip-merge-region-if-changed-from-default-p` (both in one type-bound check of the session, `call fortmacs_diff_sessions(session_index)%check_region_skip(difference, skip, status)`, in fortmacs_diff_settings_merging_mod; panel 13),
`ediff-get-combined-region`,
`ediff-set-state-of-all-diffs-in-all-buffers`,
`ediff-set-state-of-diff-in-all-buffers`,
`ediff-set-merge-mode`,
`ediff-do-merge`,
`ediff-re-merge` (&, the new default read as a named constant),
`ediff-shrink-window-C` (s),
`ediff-combine-diffs` (+),
`ediff-merge-changed-from-default-p`;
from ediff-init.el: `ediff-get-state-of-merge`

## Deferred

`ediff-looks-like-combined-merge` (with fine differences, its only user),
a delimiter of `ediff-combination-pattern` with trailing blanks (the fixed-length delimiters drop them)

## Tests

test_fortmacs_diff_merging_mod (own: the merge buffer with the variants combined and default-B, the keys a, b, r and +, merging again with a new default, the help, merging with an ancestor (prefer-A, prefer-B, the ancestor window), $$ with n, a merge stored in a file)
