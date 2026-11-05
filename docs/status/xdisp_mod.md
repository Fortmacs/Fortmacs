# xdisp_mod

- Module: xdisp_mod
- Upstream: xdisp.c (+ term.c `produce_glyphs` terminal part)
- State: partial

## Ported

`fontification-functions` (`handle_fontified_prop`: called at text not `fontified`, errors ignored),
faces of buffer text (`face` property, the active region as the region overlay in the selected window, `escape-glyph`, `nobreak-space`/`nobreak-hyphen`, the end-of-line space in the newline's face, `extend_face_to_end_of_line`),
`mode-line-active`/`mode-line-inactive` (CURRENT_MODE_LINE_ACTIVE_FACE_ID_3) with `mode-line-buffer-id` on `%12b`,
`vertical-border`,
the minibuffer prompt's face,
`redisplay` (every window of the tree, each recentered when its point leaves it; vertical borders between side-by-side windows, one mode line per window),
`redisplay_window` core,
glyph production for terminals (TAB, `^X` and `\ooo` control/raw bytes, wide characters, `\` continuation),
`display_mode_line` (standard-a buffer's own `mode-line-format` as a string (BVAR `mode_line_format`, the calendar's), else the default mode-line-format expanded in code: `%z` as UUU:, `%*%+%@`, ` %F  `, `%12b`, `%p` with percent99, `%l` with `??`, `%n`, the border glyph),
recentering by screen lines (`move_it_vertically_backward`),
continuation as `display_line` (wide characters whole, TAB/^X/\ooo split),
echo area glyphs like buffer text,
`message` in an active minibuffer (set-minibuffer-message),
`truncate-partial-width-windows` (the variable),
echo area (`message`, `cursor-in-echo-area`),
the active minibuffer on one line (with a minibuffer-message after its text) with auto-hscroll and `$` marks,
line counting for `L`,
`glyph_columns` (display width at a column),
the mode line's `mode-name` and minor-mode lighters (`minor-mode-alist`, via `add_minor_mode_lighter`; a lighter may have a face and a Fortran test of its variable, as bindings.el's `defining-kbd-macro` entry " Def" in `font-lock-warning-face`);
window-system frames (so far the tests' fake device): `gui_produce_glyphs` (character glyphs as wide as their font's glyphs, TAB stretch glyphs to tab stops of tab-width spaces),
`display_line` in pixels (continuation glyph after the last glyph that fits, a TAB filling the row, a lone glyph kept, display vectors one element a glyph, row ascent and height from the fonts with the newline's),
`append_space_for_newline`,
`compute_line_metrics`,
`produce_special_glyphs` (continuation),
`init_iterator`'s continuation_pixel_width,
`try_window` down to the text area's height in pixels,
`estimate_mode_line_height`,
the graphic mode line (mode-line-front-space " ", mode-line-frame-control "  ", no mode-line-end-spaces; display_string truncation and the space fill),
`get_phys_cursor_geometry`,
`gui_draw_vertical_border`,
the selected window's filled box cursor,
the echo area;
`clear_garbaged_frames`; *Messages*: `message_dolog` (duplicates combined into "[N times]", progress messages ending in "..." replaced, `message-log-max` lines kept),
`message_log_maybe_newline`,
`message3_nolog` (the prompts of isearch, read-key, map-y-or-n-p and help's key reading are not logged, as upstream);
display properties (the items shown at a position: overlay before- and after-strings in load_overlay_strings's order, invisible text skipped per `buffer-invisibility-spec` with the ellipsis, `display` strings and `(space :width N)`/`(space :align-to N)` specs replacing their text, `(left_fringe ...)`/`(right_fringe ...)` specs hiding it (a terminal has no fringes; also in overlay strings); header lines (`display_mode_line` for the header line: the segments of `header_line_format` in header-line-active or -inactive, on a terminal and a window system, the text area starting below it); strings and newlines in them continue rows; the cursor of a point in hidden or replaced text as GNU Emacs places it),
lines starting after visible newlines (back_to_previous_visible_line_start),
`invisible-p`,
overlay strings' `face`,
`cursor` (an integer) and `(space ...)` display specs,
overlays' `window` property,
`redisplay-highlight-region-function` and `redisplay-unhighlight-region-function` (simple.el; `redisplay--update-region-highlight`),
the buffer identification in the mode line (a buffer's `mode_line_buffer_identification` segments, or the name padded to 12),
display strings in their `face` (as overlay strings);
step 42d: `bidi-find-overridden-directionality` (the buffer or a string),
`current-bidi-paragraph-direction` (BVAR `bidi_display_reordering` and `bidi_paragraph_direction` in buffer_mod),
`bidi-inhibit-bpa` (bidi_mod);
cited: `handle_invisible_prop`,
`setup_for_ellipsis`,
`next_element_from_ellipsis`,
`in_ellipses_for_invisible_text_p`,
`handle_display_prop`,
`handle_overlay_change`,
`next_element_from_buffer`,
`next_element_from_string`,
`next_element_from_stretch`,
`merge_escape_glyph_face`,
`append_space_for_newline`,
`extend_face_to_end_of_line`,
`back_to_previous_line_start`,
`display_count_lines`,
`display_count_lines_logically`,
`percent99`,
`display_mode_lines`,
`redisplay_windows`,
`clear_garbaged_frames`,
`display_echo_area`,
`display_echo_area_1`,
`message1`,
`message3_frame`,
`message1_nolog`,
`message3_frame_nolog`,
`set_message`,
`clear_message`,
the C printf forms (`message`, `vmessage`, `message_with_string`, `add_to_log`, `vadd_to_log`, `format_nargs`: callers format with format_message);
step 45c: `pre-redisplay-function` (a procedure pointer, called with all windows as prepare_menu_bars does; null for upstream's `ignore`),
no redisplay while one runs (redisplaying_p),
a forced window start kept and point moved to the middle of the window when it does not show (redisplay_window's force_start, on a terminal frame),
`window-scroll-functions` run for a forced start and for a recentered one (`run_window_scroll_functions`, as `execute_scroll_functions_for_start`),
`window_end_pos` counted from the end of the text (Z less the end);
bindings.el's `mode-line-misc-info` after the modes (`prepend_mode_line_misc_info`: procedures giving segments, called with the window's buffer current, the displayed window or else the selected one) (step 49c),
step 44b: xdisp.c:display_tab_bar for a terminal frame (with update_tab_bar: the items made again at each redisplay through `get_tab_bar_items`, set by tab_bar_mod; `tab_bar_run_type` runs of text in their faces merged into `tab_bar`)

## Deferred

the re-run of `pre-redisplay-function` for one window after point moved (redisplay_window, with the region's highlighting),
point in invisible text at a forced start,
mode-line-format interpretation,
faces in echo-area messages and mode-line constructs other than the buffer name,
truncation (incl. truncate-partial-width-windows),
horizontal scrolling of other windows,
multi-line minibuffers (resize-mini-windows),
bidi,
`scroll-margin`/`scroll-conservatively`,
the other display specs (images, margins, `raise`, `height`, `space-width`, `min-width`, `when`, lists and vectors of specs),
text properties in overlay and display strings (no string properties),
the `window`,
`mouse-face`,
`cursor` and `line-prefix`/`wrap-prefix` properties,
fringes,
header/tab lines;
on window-system frames: recentering and vertical motion in pixels (`vertical-motion`, `recenter_start`),
the active minibuffer and the echo area cursor,
`extend_face_to_end_of_line` (fill_line_p),
realize_gui_face beyond the font,
glyphless characters,
FONT_TOO_HIGH fonts,
line-height/line-spacing,
partially visible cursor rows,
mode lines taller than their face's font,
non-selected windows' hollow cursors;
after step 42d: the struct it iterator itself (xdisp_mod lays rows out with its own iterator, peek_element and consume_element, and indent_mod moves by screen lines; these come with the features still Deferred: strings with properties, `scroll-margin` and `scroll-conservatively`, partial lines, pixel motion) (`init_from_display_pos`, `init_to_row_start`, `init_to_row_end`, `reseat`, `reseat_1`, `reseat_at_next_visible_line_start`, `reseat_at_previous_visible_line_start`, `reseat_to_string`, `push_it`, `pop_it`, `compute_stop_pos`, `compute_stop_pos_backwards`, `handle_stop_backwards`, `forward_to_next_line_start`, `next_overlay_string`, `get_overlay_strings`, `get_overlay_strings_1`, `next_element_from_c_string`, `c_string_pos`, `compute_string_pos`, `string_pos`, `string_pos_nchars_ahead`, `number_of_chars`, `string_buffer_position`, `string_buffer_position_lim`, `strings_with_newlines`, `face_before_or_after_it_pos`, `face_at_pos`, `underlying_face_id`, `move_it_by_lines`, `move_it_in_display_line`, `move_it_in_display_line_to`, `move_it_past_eol`, `move_it_to`, `move_it_vertically`, `pos_visible_p`, `line_bottom_y`, `cursor_row_p`, `row_containing_pos`, `row_for_charpos_p`, `row_text_area_empty`, `find_row_edges`, `find_last_row_displaying_text`, `adjust_window_ends`, `compute_window_start_on_continuation_line`, `try_scrolling`, `window_start_acceptable_p`, `window_text_bottom_y`, `display_count_lines_visually`, `count_lines`, `Fdisplay__line_is_continued_p`, `clear_position`, `restore_face_box_flags`, `reset_box_start_end_flags`);
the display specs other than strings and spaces,
and display properties during bidi iteration (lists and vectors of specs, `min-width`, `get-display-property`; ports test-get-display-property with them) (`handle_display_spec`, `single_display_spec_string_p`, `string_from_display_spec`, `display_prop_end`, `display_prop_string_p`, `find_display_property`, `get_display_property`, `Fget_display_property`, `compute_display_string_pos`, `compute_display_string_end`, `iterate_out_of_display_property`, `in_display_vector_p`, `display_min_width`, `calc_pixel_width_or_height`, `produce_stretch_glyph`, `append_stretch_glyph`);
compositions (with composite.c) (`handle_composition_prop`, `next_element_from_composition`, `append_composite_glyph`, `check_point_in_composition`);
glyphless characters (`glyphless-char-display`) (`produce_glyphless_glyph`, `append_glyphless_glyph`, `lookup_glyphless_char_display`, `merge_glyphless_glyph_face`, `forget_escape_and_glyphless_faces`);
display tables (`disp_char_vector`, `next_element_from_display_vector`);
`line-prefix` and `wrap-prefix` (`handle_line_prefix`, `get_line_prefix_it_property`, `push_prefix_prop`, `get_it_property`);
`display-line-numbers` (`maybe_produce_line_number`, `should_produce_line_number`);
`show-trailing-whitespace` (`highlight_trailing_whitespace`, `trailing_whitespace_p`);
`display-fill-column-indicator` (`fill_column_indicator_column`);
overlay arrows (`overlay-arrow-position`) (`get_overlay_arrow_glyph_row`, `overlay_arrow_at_row`, `overlay_arrow_in_current_buffer_p`, `overlay_arrow_string_or_property`, `update_overlay_arrows`, `deep_copy_glyph_row`);
truncation (`truncate-lines`) and horizontal scrolling of other windows than the minibuffer's (`insert_left_trunc_glyphs`, `hscroll_window_tree`, `hscroll_windows`, `hscrolling_current_line_p`, `window_hscroll_limited`);
`word-wrap` (`char_can_wrap_after`, `char_can_wrap_before`);
bidi reordering in redisplay (bidi_mod is ported; display_line does not use it yet) (`get_visually_first_element`, `unproduce_glyphs`, `Fbidi_resolved_levels`, `Fmove_point_visually`);
mode-line-format interpretation (`format-mode-line`; xdisp-test-format-mode-line with it) (`Fformat_mode_line`, `store_mode_line_noprop`, `store_mode_line_noprop_char`, `store_mode_line_string`, `pint2str`, `pint2hrstr`, `format_mode_line_unwind_data`, `unwind_format_mode_line`, `move_elt_to_front`, `safe_set_text_properties`);
the echo-area buffers and multi-line echo areas (the echo area is a string; resize-mini-windows) (`ensure_echo_area_buffers`, `with_echo_area_buffer`, `with_echo_area_buffer_unwind_data`, `unwind_with_echo_area_buffer`, `setup_echo_area_for_printing`, `set_message_1`, `truncate_message_1`, `push_message`, `pop_message_unwind`, `restore_message`, `clear_message_stack`, `current_message`, `current_message_1`, `update_echo_area`, `resize_echo_area_exactly`, `resize_mini_window`, `resize_mini_window_1`, `reset_message_log_need_newline`, `unwind_redisplay_preserve_echo_area`, `window_frozen_p`, `init_xdisp`, `log_message`);
the redisplay optimizations (speed only: redisplay recomputes every window) (`try_cursor_movement`, `try_window_id`, `try_window_reusing_current_matrix`, `find_first_unchanged_at_end_row`, `find_last_unchanged_at_beginning_row`, `text_outside_line_unchanged_p`, `row_hash`, `needs_no_redisplay`, `reconsider_clip_changes`, `mark_window_display_accurate`, `mark_window_display_accurate_1`, `mode_line_update_needed`, `propagate_buffer_redisplay`, `bset_redisplay`, `bset_update_mode_line`, `fset_redisplay`, `wset_redisplay`, `wset_update_mode_line`, `Fset_buffer_redisplay`, `window_buffer_changed`, `block_buffer_flips`, `unblock_buffer_flips`, `buffer_flipping_blocked_p`, `overlay_arrows_changed_p`, `redisplay_other_windows`, `sync_frame_with_window_matrix_rows`);
long-line optimizations (`long-line-threshold`, `max-redisplay-ticks`) (`get_large_narrowing_begv`, `get_large_narrowing_zv`, `get_medium_narrowing_begv`, `get_medium_narrowing_zv`, `get_small_narrowing_begv`, `get_narrowed_len`, `get_narrowed_width`, `get_nearby_bol_pos`, `unwind_narrowed_begv`, `Flong_line_optimizations_p`, `fast_move_it_horizontally`, `update_redisplay_ticks`);
mouse highlighting and positions (no mouse support yet) (`cancel_mouse_face`, `clear_mouse_face`, `coords_in_mouse_face_p`, `cursor_in_mouse_face_p`, `draw_row_with_mouse_face`, `adjust_glyph_width_for_mouse_face`, `get_cursor_offset_for_mouse_face`, `mouse_face_from_buffer_pos`, `mouse_face_from_string_pos`, `note_mouse_highlight`, `note_line_or_margin_highlight`, `show_mouse_face`, `remember_mouse_glyph`, `Fremember_mouse_glyph`, `rows_from_pos_range`, `fast_find_string_pos`, `x_y_to_hpos_vpos`, `x_y_to_column_row`);
the tab bar (step 44) (`build_desired_tab_bar_string`, `display_tab_bar`, `display_tab_bar_line`, `redisplay_tab_bar`, `tab_bar_height`, `Ftab_bar_height`, `get_tab_bar_item`, `get_tab_bar_item_kbd`, `tab_bar_item_info`, `handle_tab_bar_click`, `tty_get_tab_bar_item`, `tty_handle_tab_bar_click`, `note_tab_bar_highlight`, `update_tab_bar`, `Fdump_tab_bar_row`);
the menu bar and tty menus (`display_menu_bar`, `update_menu_bar`, `prepare_menu_bars`, `display_tty_menu_item`, `display_tty_menu_separator`, `display_tty_menu_separator_char`);
display margins and the window box geometry with margins and fringes (`window_box`, `window_box_edges`, `window_box_height`, `window_box_left`, `window_box_left_offset`, `window_box_right`, `window_box_right_offset`, `window_box_width`);
Not applicable: debugging (GLYPH_DEBUG dumps and traces, checks) (`CHECK_IT`, `CHECK_WINDOW_END`, `Fdump_frame_glyph_matrix`, `Fdump_glyph_matrix`, `Fdump_glyph_row`, `Ftrace_redisplay`, `Ftrace_to_stderr`, `debug_method_add`, `redisplay_trace`, `move_trace`, `dump_glyph`, `dump_glyph_matrix`, `dump_glyph_row`, `dump_glyph_string`, `check_message_stack`, `check_char_and_length`);
safe Lisp evaluation and unwinding during redisplay (no Lisp runs in redisplay, only procedures, whose errors are statuses, and nothing unwinds through it) (`dsafe__call`, `dsafe_call1`, `dsafe_eval`, `dsafe_eval_handler`, `funcall_with_backtraces`, `redisplay_window_0`, `redisplay_window_1`, `redisplay_window_error`, `unwind_redisplay`, `unwind_display_working_on_window`, `restore_selected_window`, `restore_frame_selected_window`);
batch mode — `message_to_stderr`;
`syms_of_xdisp`,
`Copyright`;
waits for the GUI: glyph strings,
fonts and drawing (`append_glyph_string`, `append_glyph_string_lists`, `prepend_glyph_string_lists`, `compute_overhangs_and_x`, `draw_glyphs`, `fill_composite_glyph_string`, `fill_glyph_string`, `fill_glyphless_glyph_string`, `fill_gstring_glyph_string`, `fill_image_glyph_string`, `fill_stretch_glyph_string`, `fill_xwidget_glyph_string`, `init_glyph_string`, `get_glyph_string_clip_rect`, `get_glyph_string_clip_rects`, `glyph_string_containing_background_width`, `set_glyph_string_background_width`, `left_overwriting`, `left_overwritten`, `right_overwriting`, `right_overwritten`, `get_char_face_and_encoding`, `get_glyph_face_and_encoding`, `get_char_glyph_code`, `get_per_char_metric`, `get_font_ascent_descent`, `font_for_underline_metrics`, `append_glyph`);
the gui_* output functions and exposure (`gui_clear_cursor`, `gui_clear_end_of_line`, `gui_clear_window_mouse_face`, `gui_consider_frame_title`, `gui_draw_bottom_divider`, `gui_draw_right_divider`, `gui_fix_overlapping_area`, `gui_get_glyph_overhangs`, `gui_insert_glyphs`, `gui_intersect_rectangles`, `gui_union_rectangles`, `gui_update_cursor`, `gui_write_glyphs`, `expose_area`, `expose_frame`, `expose_line`, `expose_overlaps`, `expose_window`, `expose_window_tree`);
cursors of window-system frames (`draw_phys_cursor_glyph`, `erase_phys_cursor`, `display_and_set_cursor`, `update_window_cursor`, `update_cursor_in_window_tree`, `phys_cursor_in_rect_p`, `notice_overwritten_cursor`, `define_frame_cursor1`, `set_frame_cursor_types`, `get_specified_cursor_type`, `get_window_cursor_type`, `cursor_row_fully_visible_p`);
images and xwidgets (`produce_image_glyph`, `produce_xwidget_glyph`, `next_element_from_image`, `next_element_from_xwidget`, `Flookup_image_map`, `find_hot_spot`, `on_hot_spot_p`);
the tool bar (`build_desired_tool_bar_string`, `display_tool_bar_line`, `redisplay_tool_bar`, `tool_bar_height`, `Ftool_bar_height`, `get_tool_bar_item`, `tool_bar_item_info`, `handle_tool_bar_click`, `handle_tool_bar_click_with_device`, `note_tool_bar_highlight`, `update_tool_bar`, `Fdump_tool_bar_row`);
the hourglass,
scroll bars and fringes (`show_hourglass`, `start_hourglass`, `cancel_hourglass`, `set_vertical_scroll_bar`, `set_horizontal_scroll_bar`, `note_fringe_highlight`);
pixel sizes and line heights (`normal_char_height`, `pixel_to_glyph_coords`, `frame_to_window_pixel_xy`, `Fbuffer_text_pixel_size`, `Fwindow_text_pixel_size`, `window_text_pixel_size`, `Fline_pixel_height`, `default_line_pixel_height`, `calc_line_height_property`, `partial_line_height`, `take_vertical_position_into_account`),
after step 44b: the tab bar of a window-system frame (build_desired_tab_bar_string, display_tab_bar_line, tab_bar_height, redisplay_tab_bar), tab bar lines wrapped to more than one line, tab_bar_item_info and the mouse on the tab bar

## Tests

test_xdisp_mod (own; xdisp-tests.el needs windows and Lisp),
test_fake_gui_mod;
ports xdisp-tests--find-directional-overrides-case-1 to -3 (in test_bidi_mod)
