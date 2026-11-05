# insert_delete_mod

- Module: insert_delete_mod
- Upstream: insdel.c (+ buffer.c `before-change-functions`, `after-change-functions`, `first-change-hook`)
- State: done

## Ported

undo recording (`record_insert`, `record_delete`, `record_first_change` in `modify_text`),
`execute_undoable_change` (undo-auto--undoable-change in `prepare_to_modify_buffer_1`, as upstream),
gap motion,
`make_gap` (larger/smaller; `buffer_overflow`; buffer.c `enlarge_buffer_text`),
`make_gap_1`,
`copy_text` (unibyte/multibyte conversion),
`insert` (`insert_string`),
`insert_char`,
`insert_before_markers`,
`insert_and_inherit`,
`insert_before_markers_and_inherit` (the four are generic interfaces, which editing_functions_mod extends to several strings),
`insert_1_both` (INHERIT by stickiness),
`insert_from_string(_1)`,
`insert_from_string_before_markers` (with the string's properties),
`insert_from_buffer(_1)` (with the other buffer's properties, converting unibyte/multibyte),
`adjust_point`,
`adjust_markers_for_insert/delete`,
`del_range`,
`del_range_1/_byte/_both/_2` (the start kept in a marker across the before-change hooks),
`safe_del_range(_1/_2)`,
`modify_text`,
`prepare_to_modify_buffer(_1)` (read-only check, text property modification hooks with PRESERVE kept in a marker, `deactivate-mark`, PRESERVE_PTR),
`replace_range` (with `adjust_markers_for_replace`; ADJUST_MATCH_DATA),
`replace_range_2` (with `adjust_markers_bytepos`, `count_bytes`),
the change hooks: `signal_before_change` (`first-change-hook` on an unmodified buffer, `before-change-functions`, the overlays' modification hooks, positions in markers meanwhile),
`signal_after_change` (`after-change-functions`, the overlays' modification hooks, the text properties' insert hooks),
`inhibit-modification-hooks` (declared in text_properties_mod, which needs it; bound to t while they run),
a change hook set to nil in the current buffer when one of its functions fails (`reset_var_on_error`),
`combine-after-change-calls` and `combine-after-change-execute` (`Fcombine_after_change_execute_1`)

## Deferred

`combine-change-calls` and the subr.el `combine-after-change-calls` macro,
file locking and `select-active-regions` in prepare_to_modify_buffer,
compositions (`update_compositions`),
quit during gap motion,
`replace-region-contents`,
the undo marker adjustments;
Not applicable: the interfaces for text decoded in place in the gap (`insert_from_gap(_1)`, `adjust_after_insert`, `adjust_after_replace`: Fortmacs decodes into strings that `insert` takes),
`invalidate_buffer_caches` (no newline, width-run or bidi paragraph caches are kept),
the debugging checks `check_markers`,
`count_combining_before`/`_after` (BYTE_COMBINING_DEBUG),
`syms_of_insdel` (symbol and variable definitions);
deferred to 42d: `adjust_suspend_auto_hscroll` (with window.c's auto-hscroll);
waits for the GUI: nothing

## Tests

test_insert_delete_mod (own; no upstream insdel-tests.el; ports editfns-tests: delete-region-undo-markers-2 (the text; the undo marker adjustments are deferred), editfns-tests--before/after-change-functions (on the ported operations instead of dabbrev and coding); own: order and arguments of the hooks for each primitive, local and global functions, inhibiting, first-change-hook, the error reset, combine-after-change-calls, insert-and-inherit, inserting with properties, copy_text, safe_del_range)
