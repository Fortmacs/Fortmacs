# undo_mod

- Module: undo_mod
- Upstream: undo.c (+ simple.el undo-auto)
- State: partial

## Ported

`record_point`,
`record_insert` (merging consecutive insertions),
`record_delete`,
`record_change` (step 42f, for transpose-regions, translate-region-internal and subst-char-in-region; the text without properties),
`record_first_change`,
`undo-boundary`;
simple.el's `undo-auto--undoable-change`,
`undo-auto--boundaries`,
`undo-auto--ensure-boundary`,
`undo-auto--add-boundary`,
`undo-auto-amalgamate` (`amalgamating-undo-limit`),
here because the C side calls them (`undo-auto--undoable-change` from insdel.c `execute_undoable_change`)

## Deferred

marker adjustments,
property changes,
the boundary timer,
`truncate_undo_entries` (undo-limit),
`buffer-undo-list` as a Lisp value,
`buffer-disable-undo` (`buffer-enable-undo` is buffer_mod's)

## Tests

test_undo_mod (ports undo-tests: undo-test0, undo-test2, undo-test-buffer-modified, undo-test-file-modified; simple-tests: simple-tests--undo, undo-point-in-wrong-place (first case); own: amalgamation)
