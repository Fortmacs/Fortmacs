# buffer_mod

- Module: buffer_mod
- Upstream: buffer.c, buffer.h (+ intervals.c `temporary_set_point_both`, lisp.h `modiff_increment`)
- State: done

## Ported

BVAR `undo_entries` (an array of entries, newest last) and the undo-equiv-table per buffer,
BVAR `mark`,
`mark-active` and the mark ring,
`transient-mark-mode`,
`deactivate-mark` (the variable, as `deactivate_mark_variable`),
struct buffer_text / struct buffer (subset; `reset_buffer` and `alloc_buffer_text` in `get-buffer-create`),
registry,
`get-buffer` (`assoc_ignore_text_properties`),
`get-buffer-create`,
`generate-new-buffer-name`,
`buffer-name`,
`buffer-last-name`,
`rename-buffer`,
`buffer-list`,
`set-buffer` (`restore_buffer`, `set_buffer_internal_1`/`_2`, `set_buffer_if_live`),
`kill-buffer` (core),
`buffer-modified-p`,
`restore-buffer-modified-p`,
`set-buffer-modified-p`,
`force-mode-line-update` (redisplay recomputes every mode line, so there is no flag to set),
`buffer-enable-undo`,
`buffer-file-name`,
`nsberror`,
modified ticks,
`barf-if-buffer-read-only`,
BUFFER_BYTE_ADDRESS / FETCH_BYTE / FETCH_CHAR / next/prev char len,
BUFFER_COMPUTE_UNCHANGED,
`get-file-buffer` (given an expanded name),
BVAR filename/directory,
`buffer-backed-up`,
visited modtime,
new buffers inherit `default-directory`,
`record-buffer` (buffer order),
`buffer-list-update-hook` (hooks_mod; run by get-buffer-create, rename-buffer, kill-buffer, bury-buffer-internal and record_buffer unless the buffer inhibits buffer hooks),
the window part of `kill-buffer` (through a hook to window_mod),
`fill-column`,
`left-margin` and `auto-fill-function` (one global value each);
BVAR `invisibility_specification` (`buffer-invisibility-spec`: t, or ATOM and (ATOM . ELLIPSIS) entries; reset by kill-all-local-variables);
BVAR `header_line_format` (a `header_line_format_type`: text segments with a face and a column each, `add` and `clear`);
the overlay functions of buffer.c are interval_tree_mod's,
`buffer-local-value` and `buffer-local-variables` (with `buffer_local_value`, `buffer_lisp_local_variables`, `buffer_local_variables_1`) data_mod's;
step 44a: the frames' buffer lists exchanged on a frame switch (`exchange_frame_buffers`; closed buffers leave a frame's lists when it is selected again);
BVAR `save_length` (`buffer-saved-size`, set where fileio.c sets it; not a variable of its own yet) (step 49c),
`frame_buried_buffers` (the selected frame's buried_buffer_list, for tab-bar.el)

## Deferred

indirect buffers (`make-indirect-buffer`, `buffer-base-buffer`, `copy_overlays`, `clone_per_buffer_values`, `record_buffer_markers`, `fetch_buffer_markers`: they need buffer text shared between buffer records, held outside `buffer_type`),
`buffer-swap-text` (`swap_buffer_overlays`; swapping texts, markers and overlays between records),
`set-buffer-multibyte` (`advance_to_char_boundary`, `set_overlays_multibyte`, intervals.c `set_intervals_multibyte`),
`get-truename-buffer` (BVAR `file_truename` is not kept yet: files.el sets it, with file locking in 42e),
`overlay_strings`,
`record_overlay_string` and `cmp_for_strings` (for indent.c's compute_motion and xdisp, 42d),
buffer-local vars beyond data_mod's,
files/locking,
the `autosaved` state,
`inhibit-read-only` as a buffer-local;
Not applicable: the BVAR setters (`bset_*`),
the per-buffer variable machinery (`defvar_per_buffer`, `valid_per_buffer_index`, `init_buffer_once`: typed per-buffer components and data_mod replace it),
memory management (`free_buffer_text`, `compact_buffer`, `mmap_*`, `free_buffer_overlays`),
`set_buffer_temp` (the switch that leaves buffer-local bindings alone: Fortmacs swaps them on every switch),
`fix_position` (positions are integers),
the itree debugging dump (`overlay-tree`, `overlay_tree`, `make_lispy_itree_node`),
`syms_of_buffer`'s symbol and variable definitions (typed declarations),
and the scanner's doc-line artifacts `Copyright`,
`Functions`,
`from`,
`last`;
waits for the GUI: nothing

## Tests

test_buffer_mod (ports bug27966, buffer-tests-inhibit-buffer-hooks (its `buffer-list-update-hook` part) and the file-independent parts of test-buffer-modifications, test-restore-buffer-modified-p, test-buffer-chars-modified-ticks; own: buffer-list-update-hook runs, the DEFUNs)
