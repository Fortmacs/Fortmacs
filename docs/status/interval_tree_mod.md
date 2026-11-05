# interval_tree_mod

- Module: interval_tree_mod
- Upstream: itree.c, buffer.c (overlays)
- State: done

## Ported

overlays as registry objects with itree.c's node semantics in a flat array: `make-overlay`,
`move-overlay`,
`delete-overlay`,
`delete-all-overlays` (kill-buffer; `delete_all_overlays`, `itree_clear`),
`is_overlay`,
CHECK_OVERLAY,
`overlay-start`/`-end` (`itree_node_begin`/`_end`)/`-buffer`,
`overlay-get`,
`overlay-put`,
`overlay-properties`,
`overlays-at` (SORTED; `overlays_at`),
`overlays-in`,
`next-overlay-change`,
`previous-overlay-change` (with their C helpers),
`overlay-lists`,
`overlay-recenter` (a no-op, as upstream),
`sort_overlays`/`compare_overlays`,
`modify_overlay`,
`add_buffer_overlay`,
`remove_buffer_overlay` (`itree_insert(_node)`, `itree_remove`, `itree_node_init`, `itree_node_set_region`),
the ascending traversal (`itree_iterator_start`, `_next`, `_first_node`, `itree_iter_next_in_subtree`),
`adjust_overlays_for_insert`/`_delete` (`adjust_overlays_for_delete_in_buffer`) from insdel (itree_insert_gap/delete_gap rules: front/rear advance, empty overlays),
`evaporate`,
the overlay modification hooks (`report_overlay_modification`, `add_overlay_mod_hooklist`, `execute_overlay_mod_hooks`: `modification-hooks`, `insert-in-front-hooks`, `insert-behind-hooks`, run by insert_delete_mod before and after a change),
`is_overlay_touches`,
`mouse_face_overlay_overlaps`,
`disable_line_numbers_overlay_at_eob`;
`destroy_overlay` (ours);
`overlay_advance` (front/rear-advance),
`overlay_get_prop`,
`swap_buffer_overlays` (for buffer-swap-text)

## Deferred

indirect buffers,
the `window`,
`mouse-face` (mouse highlighting, with xdisp) and `intangible` properties (the display ones are display_engine_mod's),
(PRIMARY . SECONDARY) priorities (subr.el's `remove-overlays` and `copy-overlay` are in basic_subprograms_mod);
Not applicable: the red-black tree machinery the flat array replaces (`itree_rotate_left/right`, `itree_insert_fix`, `itree_remove_fix`, `itree_transplant`, `itree_replace_child`, `itree_subtree_min`, the limit and offset bookkeeping `itree_update_limit`, `itree_propagate_limit`, `itree_newlimit`, `itree_limit_is_stable`, `itree_inherit_offset`, `itree_total_offset`, the iterator stack `itree_stack_*`, `itree_max_height`, `itree_iterator_narrow`, `itree_size`, `null_safe_is_red/black`),
tree allocation (`itree_create`, `itree_init`, `itree_destroy`),
the consistency checks (`itree_validate`, `itree_contains`, `check_tree`, `check_subtree`),
the scanner's doc-line artifact `Copyright`;
waits for the GUI: nothing

## Tests

test_interval_tree_mod (ports buffer-tests.el overlay-modification-hooks, overlay-modification-hooks-deleted-overlay, test-overlay-lists-1, test-overlay-recenter, test-buffer-swap-text-1, test-buffer-swap-text-2; own: bounds, advance rules, evaporation, queries, kill-buffer, faces in redisplay by priority with the region)
