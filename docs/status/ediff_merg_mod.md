# ediff_merg_mod

- Module: ediff_merg_mod
- Upstream: vc/ediff-merg.el
- State: partial

## Ported

`ediff-quit-merge-hook` (its default function `maybe_save_and_delete_ediff_merge` is added by ediff_util_mod),
`ediff-default-variant` (a choice setting: EDIFF_DEFAULT_VARIANT_DEFAULT_A, _DEFAULT_B, _COMBINED),
`ediff-combination-pattern` (an array of `ediff_combination_part_type` records: a delimiter and a buffer type, 0 after the last delimiter),
`ediff-show-clashes-only`,
`ediff-skip-merge-regions-that-differ-from-default`,
`ediff-merge-region-is-non-clash`,
`ediff-merge-region-is-non-clash-to-skip`,
`ediff-skip-merge-region-if-changed-from-default-p` (`is_ediff_merge_region_changed_from_default_to_skip`),
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

test_ediff_merg_mod (own: the merge buffer with the variants combined and default-B, the keys a, b, r and +, merging again with a new default, the help, merging with an ancestor (prefer-A, prefer-B, the ancestor window), $$ with n, a merge stored in a file)
