# display_update_mod

- Module: display_update_mod
- Upstream: dispnew.c (+ frame.c `adjust_frame_size`, xdisp.c `redisplaying_p`)
- State: partial

## Ported

frame matrices (`adjust_frame_glyphs`, clear current/desired),
`frame-or-buffer-changed-p` (a `frame_buffer_state_type` object for VARIABLE),
`redraw_frame`,
`write_row` (prefix/suffix matching, character insertion and deletion by cost),
`write_matrix` (last row first),
`make_matrix_current`,
`tty_set_cursor`,
`update_frame` (terminal path; a window-system frame is flushed),
`ding`;
term.c `calculate_ins_del_char_costs`;
the window-system path: `adjust_frame_glyphs` without frame matrices,
`redraw_frame` through clear_frame,
`update_window_line` (update_text_area for a row drawn whole);
step 42d: `redraw-frame`,
`redraw-display` (a command),
`redisplay` (with xdisp.c's redisplay_preserve_echo_area),
`send-string-to-terminal`,
`internal-show-cursor`,
`internal-show-cursor-p` (a window's `cursor_off_p`, read by tty_update_end);
cited: `update_frame`,
`combine_updates`,
`flush_terminal`,
`adjust_frame_glyphs_for_frame_redisplay`,
`allocate_matrices_for_frame_redisplay`,
`new_glyph_matrix`,
`adjust_glyph_matrix`,
`clear_glyph_matrix`,
`clear_glyph_matrix_rows`,
`clear_glyph_row`,
`blank_row`,
`build_frame_matrix`,
`build_frame_matrix_from_window_tree`,
`update_tty_frame` (redisplay writes windows straight into the frame matrix),
`init_display`,
`init_display_interactive`;
step 42e: frame size changes: `handle_window_change_signal` (run by `do_pending_window_change` after a SIGWINCH, outside the signal handler),
`do_pending_window_change` (before redisplay, fortmacs_mod's redisplay hook),
`change_frame_size`,
`change_frame_size_1`,
`frame_size_change_delayed` (`is_frame_size_change_delayed`),
frame.c's `adjust_frame_size` with INHIBIT 5 (windows resized, the terminal's size set, the selected window's cursor kept inside it, matrices reallocated, costs recomputed, the frame marked for redraw),
xdisp.c's `redisplaying_p` (`redisplaying`; both here, not in frame_mod and display_engine_mod, which would then use each other: adjust_frame_size needs the matrices and the terminal's costs, whose modules use frame_mod, and display_engine_mod uses display_update_mod, which reads redisplaying_p)

## Deferred

line scrolling (scroll.c),
window matrices (on a window-system frame every row is drawn whole, without comparing it with the current one),
pausing for input,
child frames,
menus;
for frame size changes: `frame-size-history` (`frame_size_history_extra`, `frame_size_history_adjust`),
the window change hooks (FRAME_WINDOW_CHANGE),
`dont_resize_frames` (tool bar, format-mode-line's buffer),
PRETEND (frame.c's set-frame-size family),
adjust_frame_size's other INHIBIT values,
fullscreen and child frames (`keep_ratio`),
and the second do_pending_window_change at the end of redisplay_internal with its retry;
after step 42d: window matrices and the window-system update (wait for the GUI) (`adjust_frame_glyphs_for_window_redisplay`, `allocate_matrices_for_window_redisplay`, `clear_window_matrices`, `destroy_window_matrices`, `update_window_frame`, `update_window_tree`, `update_single_window`, `update_window`, `update_text_area`, `update_marginal_area`, `gui_update_window_begin`, `gui_update_window_end`, `set_window_cursor_after_update`, `set_window_update_flags`, `sync_window_with_frame_matrix_rows`, `mirror_make_current`, `make_current`, `assign_row`, `copy_row_except_pointers`, `swap_glyph_pointers`, `swap_glyphs_in_rows`, `row_equal_p`, `increment_matrix_positions`, `increment_row_positions`, `shift_glyph_matrix`, `rotate_matrix`, `prepare_desired_row`, `required_matrix_height`, `required_matrix_width`, `margin_glyphs_to_reserve`, `showing_window_margins_p`, `fill_up_glyph_row_with_spaces`, `fill_up_glyph_row_area_with_spaces`, `fill_up_frame_row_with_spaces`, `first_enabled_row`, `matrix_row`, `frame_row_to_window`, `window_to_frame_hpos`, `window_to_frame_vpos`, `find_glyph_row_slice`, `glyph_row_slice_p`, `neutralize_wide_char`, `redraw_overlapped_rows`, `redraw_overlapping_rows`, `update_bar_window`, `is_cursor_obscured`, `fake_current_matrices`, `save_current_matrix`, `restore_current_matrix`);
line insertion and deletion (scroll.c) (`scrolling`, `scrolling_window`, `line_hash_code`, `line_draw_cost`, `verify_row_hash`, `add_row_entry`, `reverse_rows`, `mirror_line_dance`, `mirrored_line_dance`);
child frames and their borders (step 44) (`copy_child_glyphs`, `frames_in_reverse_z_order`, `frames_with_parent`, `frames_with_root`, `frame_z_order_cmp`, `max_child_z_order`, `Fframe__z_order_lessp`, `frame_ancestors_visible_p`, `frame_rect_abs`, `rect_intersect`, `child_xy`, `root_xy`, `abs_cursor_pos`, `is_tty_child_frame`, `is_tty_frame`, `is_tty_root_frame`, `is_tty_root_frame_with_visible_child`, `prepare_desired_root_row`, `terminal_cursor_magic`, `tty_raise_lower_frame`, `frame_selected_window_frame`, `box_default`, `box_from_display_table`, `box_glyph`, `produce_box_glyphs`, `produce_box_line`, `produce_box_sides`, `spec_glyph_lookup_face`);
menus,
the tool bar and the tab bar (`update_frame_with_menu`, `update_menu_bar`, `update_tool_bar`, `update_tab_bar`);
the mouse (`buffer_posn_from_coords`, `mode_line_string`, `marginal_area_string`, `update_mouse_position`, `Fdisplay__update_for_mouse_movement`);
the termscript,
`sleep-for` (waiting for process output without reading input, with process.c, 42e),
`format-mode-line`'s buffer (`Fopen_termscript`, `Fsleep_for`, `adjust_decode_mode_spec_buffer`);
Not applicable: batch mode (`update_initial_frame`, `flush_stdout`, `init_faces_initial`),
debugging (`dump-redisplay-history`, the display histories, the matrix checks),
memory management (glyph pools: allocatable arrays),
the dumper (`syms_of_display_for_pdumper`),
the scanner's artifacts `Copyright`,
`__executable_start` — `update_initial_frame`,
`flush_stdout`,
`init_faces_initial`,
`Fdump_redisplay_history`,
`add_frame_display_history`,
`add_window_display_history`,
`check_current_matrix_flags`,
`check_glyph_memory`,
`check_matrix_invariants`,
`check_matrix_pointer_lossage`,
`check_matrix_pointers`,
`check_rows`,
`check_window_matrix_pointers`,
`is_in_matrix`,
`free_glyph_matrix`,
`free_glyph_pool`,
`new_glyph_pool`,
`realloc_glyph_pool`,
`free_glyphs`,
`make_glyph_space`,
`syms_of_display_for_pdumper`,
`Copyright`,
`__executable_start`

## Tests

test_display_update_mod (own; byte-exact xterm output),
test_fake_graphical_user_interface_mod;
own: internal-show-cursor,
send-string-to-terminal,
redraw-frame,
change-frame-size (one window, the minimum size),
change-frame-size/windows (a split frame's windows grown, shrunk to and below their minimum sizes and widened and narrowed, with the sizes GNU Emacs gives them),
window-change-signal (a pty's size, delayed then made),
suspend-emacs (`redraw_after_suspend` true and false; the test process in a process group of its own, SIGTSTP ignored; skipped when the process already leads its group)
