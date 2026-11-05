# intervals_mod

- Module: intervals_mod
- Upstream: intervals.c, intervals.h
- State: done

## Ported

intervals as a sorted run array: `create_root_interval`,
`find_interval`,
`split_interval_right/left`,
`intervals_equal` (`intervals_equal_1`),
`offset_intervals` (insertion/deletion: `adjust_intervals_for_insertion`, `adjust_intervals_for_deletion`; the inserted text inheriting by stickiness through `insertion_plist`),
`merge_properties`,
`merge_properties_sticky`,
`graft_intervals_into_buffer` (`copy_properties`),
`copy_intervals`,
`copy_intervals_to_string`,
`compare_string_intervals`,
`textget` (with `category` and `char-property-alias-alist`),
`lookup_char_property`,
TMEM,
textprop.c `property_value`;
text_properties_mod has `temporary_set_point`,
`set_point`,
`set_point_both`,
`adjust_for_invis_intang`,
`move_if_not_intangible` (they need the char-property functions),
keymap_mod `get_local_map`

## Deferred

`default-text-properties`,
`set_intervals_multibyte(_1)` (with set-buffer-multibyte);
Not applicable: the balanced tree and its parent links,
which the run array replaces (`balance_*`, `rotate_left/right`, `set_interval_left/right`, `copy_interval_parent`, `delete_node`, `delete_interval`, `merge_interval_left/right`, `interval_deletion_adjustment`, `reproduce_tree(_obj)`, `reproduce_interval`, `update_interval`, `next_interval`/`previous_interval` (k ± 1), `interval_start_pos`, `traverse_intervals(_noorder)`);
waits for the GUI: nothing

## Tests

test_text_properties_mod,
test_insert_delete_mod
