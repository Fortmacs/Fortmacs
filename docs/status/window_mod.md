# window_mod

- Module: window_mod, window_scroll_mod, window_commands_mod, window_display_mod, window_configuration_mod, window_sides_mod, window_change_mod (submodules)
- Upstream: window.c, window.h, window.el, buffer.c (`other-buffer`, `record-buffer`, `bury-buffer-internal`, frame buffer lists)
- State: partial

## Ported

`split-window-below` and `split-window-right` as functions (`ediff_split_window_function`'s values),
`struct window` (subset: buffer, start and point markers, geometry, cursor, hscroll, prev/next buffers, `quit-restore`/`quit-restore-prev`, dedication),
`set-window-buffer` (unshow_buffer, last window start),
`window-start`,
`set-window-start`,
`window-point`,
`set-window-point`,
`window-body-height` (less the header line),
`window_wants_header_line`;
scrolling: `scroll-up`,
`scroll-down`,
`recenter`,
`move-to-window-line`,
`scroll-up-command`,
`scroll-down-command`,
`recenter-top-bottom`,
`move-to-window-line-top-bottom`,
`scroll-other-window`,
`scroll-other-window-down`,
`other-window-for-scrolling`,
`pos-visible-in-window-p` (partially);
`record-window-buffer`,
`switch-to-prev-buffer`,
`switch-to-next-buffer`,
`previous-buffer`,
`next-buffer`,
`bury-buffer`,
`quit-restore-window`,
`quit-window`,
`replace-buffer-in-windows`,
`display-buffer-record-window`,
`display-buffer` with its fallback action (same/reuse/pop-up/previous/use-some window) and `display-buffer-at-bottom`,
the `inhibit-same-window` and `window-height` table entries,
`display-buffer-overriding-action` (functions and `inhibit-same-window`),
`display-buffer-mark-dedicated`,
`pop-to-buffer`,
`pop-to-buffer-same-window`,
`temp-buffer-window-show`,
`minibuffer-scroll-window`,
`get-buffer-window`,
`get-lru-window`,
`get-mru-window`,
`get-largest-window`,
`split-window-sensibly`,
`window--even-window-sizes`,
`window-resize` (with the window next to it),
`fit-window-to-buffer` (vertically),
`window-min-size`,
`current-window-configuration`,
`set-window-configuration` (deleted windows kept for revival);
step 45c: `window-end` (with UPDATE always computed: no record of what changed since redisplay, window_outdated; without it, as redisplay recorded it, counted from the end of the text),
`pos-visible-in-window-p` through it (the end of the text shows when the window reaches it),
`run-window-scroll-functions` (`execute_window_scroll_functions`),
`window-scroll-functions` run by `set-window-buffer` and twice by `split-window` (set_window_buffer's run_hooks_p and window.el's call);
C-v,
M-v, <next>, <prior>,
C-l,
M-r,
C-M-v,
M-<next>,
M-<prior>,
C-M-S-v,
C-x <left>/<right>;
`display-buffer-below-selected`,
the `body-function` table entry,
`switch-to-buffer-other-window` (C-x 4 b is still unbound);
step 45a: `window-in-direction` in all four directions with SIGN,
WRAP and MINIBUF (`window--in-direction-2`, `window-at-side-p`, `walk-window-tree`'s MINIBUF; IGNORE makes no difference while no window has a `no-other-window` parameter, and is left out),
`split-window`'s SIDE `above` and `left` (`split_window`'s BEFORE),
`window-configuration-equal-p` (`compare_window_configurations`: the frame's size and current buffer, and each window's buffer, edges, place in the tree and whether it is the current one; configurations record the frame's size),
copies of window configurations (`copy_window_configuration`, for Winner mode's rings),
`window-configuration-change-hook` run by redisplay (`run_window_change_functions`, its configuration change part: a window changed its buffer or total or body size, or one was deleted; `window_change_record`, `window_change_record_windows`, `run-window-configuration-change-hook` with `run_window_configuration_change_hook` and `run_funs`; the frame's change stamp and window count kept in window_change_mod, one frame; a change is found by comparing, so FRAME_WINDOW_CHANGE is not needed);
step 42d (window_info_mod): the accessors of struct window (`window-buffer` with `decode_any_window`, `is_window` (a deleted window stays a window: handle_mod's `issued`), `window-parent`, `window-top-child`, `window-left-child`, `window-next-sibling`, `window-prev-sibling`, `frame-root-window`, `frame-selected-window`, `minibuffer-window`, `window-minibuffer-p`, `minibuffer-selected-window`, `window-total-height` and `window-total-width` (ROUND makes no difference), `window-left-column`, `window-top-line`, `window-body-height` and `window-body-width` in canonical units, `window-mode-line-height`, `window-header-line-height`, `window-tab-line-height`, `window-hscroll`, `window-dedicated-p`, `set-window-dedicated-p`, `window-use-time`, `window-bump-use-time`, `window-prev-buffers`, `window-next-buffers`; `decode_live_window`, `decode_valid_window`),
`next-window`,
`previous-window`,
`window-list`,
`window-list-1` (`next_window`, `window_list_1`, `window_list`, `add_window_to_list`, `foreach_window`, `is_candidate_window`, `decode_next_window_arguments`; MINIBUF as MINIBUF_ACTIVE, MINIBUF_YES or MINIBUF_NO; one frame, so ALL-FRAMES makes no difference and is left out),
`coordinates-in-window-p` and `window-at` on a terminal frame (`coordinates_in_window`, `window_from_coordinates`, `check_window_containing`),
`force-window-update` (nothing to mark),
`window_wants_mode_line`,
`sanitize_next_screen_context_lines`;
step 42e: normal sizes (`normal_lines`, `normal_columns`, `window-normal-size`; set by split-window, delete-window, delete-other-windows, window-resize and window configurations, and copied by `replace_window`) and resizing a frame's root window by them for a frame size change (frame_mod's `resize_frame_windows`): window.el's `window--resize-root-window` (`window_resize_root_window`, which window.c's `resize_root_window` calls),
`window--resize-reset`,
`window--resize-this-window`,
`window--resize-child-windows` (the proportional part),
`window-sizable`,
`window-sizable-p`,
`window-min-delta` and `window--min-delta-1` (no TRAIL),
`window-size`,
`window-min-size` with IGNORE t,
window.c's `window_resize_check` and `window_resize_apply` (new sizes in `new_total`, whole lines and columns);
cited: `select_window`,
`select_window_1`,
`select_window_norecord`,
`window_point`,
`allocate_window`,
`window_internal_height`,
`window_sub_list`,
`get_leaf_windows`,
`window_loop`,
`replace_buffer_in_windows`,
`display_buffer`,
`restore_window_configuration`,
`window_scroll`,
`displayed_window_lines`,
`init_window_once`;
step 45b (window_sides_mod): side windows: the `window-side`,
`window-slot`,
`no-other-window` and `no-delete-other-windows` parameters (typed components),
`ignore-window-parameters`,
`window-sides-vertical`,
`window-sides-reversed`,
`window-sides-slots`,
`window--sides-shown`,
`walk-window-tree` and `walk-window-subtree` (a list of windows),
`window-with-parameter` (for `window-side`: `window_with_side`),
`window-no-other-p`,
`window-main-window`,
`window-child-count`,
`window-combinations`,
`window--combination-resizable`,
`window--sides-check` and `window--sides-check-failed`,
`window--sides-reverse-on-frame-p` (no `window-sides-main-selected-window` frame parameter),
`window--make-major-side-window-next-to`,
`window--make-major-side-window`,
`display-buffer-in-side-window` with the `side`,
`slot` and `dedicated` table entries and dedication `side` (DEDICATED_SIDE, kept by `set-window-buffer-start-and-point`, `quit-restore-window`, `replace-buffer-in-windows`),
the numeric `window-height` and `window-width` entries;
split-window with SIDE `above` and `left`,
the side window checks,
`window-combination-resize` `side` (the new side window takes its space from the side windows next to it) and `window-combination-limit` t for a main window next to a side window;
delete-window (the main window),
delete-other-windows (side windows, `no-delete-other-windows`, the main window as ROOT of `delete-other-windows-internal`, "No other windows to delete"),
other-window (`no-other-window`, "No other window to select"),
the MRU window delete-window selects (no dedicated or `no-other-window` windows),
`window-deletable-p` (the main window),
`window-splittable-p` (no side windows),
switch-to-prev-buffer and switch-to-next-buffer in side windows (parent, siblings, combinations; `split-window` of live and internal windows with upstream's size checks, `delete-window`, `delete-other-windows`),
`selected-window`,
`select-window`,
`other-window`,
`window-live-p`,
`switch-to-buffer` (window.el: dedicated and minibuffer windows, window point preserved),
`kill-buffer` command;
C-x 0,
C-x 1,
C-x 2,
C-x 3,
C-x o,
C-x b,
C-x k;
after step 42d: other frames and frames as objects (step 44) (`Fwindow_frame`, `Fset_frame_selected_window`, `select_frame_norecord`, `mru_rooted_frame`);
pixelwise sizes,
cursor types,
scroll bars,
fringes and dividers (wait for the GUI) (`Fwindow_pixel_width`, `Fwindow_pixel_height`, `Fwindow_pixel_left`, `Fwindow_pixel_top`, `Fwindow_old_pixel_width`, `Fwindow_old_pixel_height`, `Fwindow_text_width`, `Fwindow_text_height`, `Fwindow_line_height`, `Fwindow_lines_pixel_dimensions`, `Fwindow_cursor_info`, `Fwindow_vscroll`, `Fset_window_vscroll`, `Fwindow_scroll_bar_width`, `Fwindow_scroll_bar_height`, `Fwindow_scroll_bars`, `Fset_window_scroll_bars`, `set_window_scroll_bars`, `Fwindow_fringes`, `Fset_window_fringes`, `set_window_fringes`, `window_pixel_to_total`, `window_body_unit_from_symbol`, `window_scroll_pixel_based`, `get_phys_cursor_glyph`, `mark_window_cursors_off`, `Fwindow_cursor_type`, `Fset_window_cursor_type`, `extract_dimension`, `apply_window_adjustment`);
display margins (`Fwindow_margins`, `Fset_window_margins`, `set_window_margins`);
horizontal scrolling of other windows than the minibuffer's (`Fset_window_hscroll`, `set_window_hscroll`, `Fscroll_left`, `Fscroll_right`);
window parameters other than `quit-restore` (`Fwindow_parameter`, `Fwindow_parameters`, `Fset_window_parameter`, `window_parameter`);
resizing whole combinations other than a frame's root window and combination limits (`Fwindow_new_normal`, `Fwindow_new_total`, `Fwindow_new_pixel`, `Fset_window_new_normal`, `Fset_window_new_total`, `Fset_window_new_pixel`, `Fwindow_resize_apply`, `Fwindow_resize_apply_total`, `window_resize_apply_total`, `window_set_parent_and_normal_sizes`, `make_parent_window`, `recombine_windows`, `Fcombine_windows`, `Funcombine_window`, `Fwindow_combination_limit`, `Fset_window_combination_limit`, `Fdelete_other_windows_internal`);
resizing the minibuffer window (`resize-mini-windows`) (`Fresize_mini_window_internal`, `grow_mini_window`, `shrink_mini_window`, `resize_mini_window_apply`);
the window change hooks other than `window-configuration-change-hook` (`window-buffer-change-functions`, `window-size-change-functions`, `window-selection-change-functions`, `window-state-change-functions`, `window-state-change-hook`) (step 45) (`run_window_change_functions_globally`, `run_window_change_functions_locally`, `Fwindow_old_buffer`, `Fwindow_old_point`, `Fframe_old_selected_window`, `Fold_selected_window`);
an error in `window-configuration-change-hook` during redisplay is not reported (redisplay has no way to pass it to the command loop yet);
the tab line (step 44) (`window_wants_tab_line`);
display tables (`Fwindow_display_table`, `Fset_window_display_table`, `window_display_table`);
`scroll-margin` (`window_scroll_margin`);
long-line optimizations (`window_scroll_for_long_lines`);
the frame of a window configuration (step 44) (`Fwindow_configuration_frame`);
setting the buffer lists (for window states, step 45) (`Fset_window_prev_buffers`, `Fset_window_next_buffers`);
discarding a killed buffer from the windows' lists and saved configurations,
and kill-buffer's fallback when a hook shows the buffer again (`Fwindow_discard_buffer`, `window_discard_buffer_from_table`, `window_discard_buffer_from_list`, `window_discard_buffer_from_dead_windows`, `replace_buffer_in_windows_safely`, `delete_deletable_window`);
buffer window counts and `window_outdated` (redisplay keeps no record of what a window showed) (`adjust_window_count`, `window_outdated`);
mouse positions (`window_relative_x_coord`);
Not applicable: the setters (`wset_*`),
typed window configurations (`window-configuration-p`, CHECK_WINDOW_CONFIGURATION),
the cached window list (`init_window`),
debugging (`check_all_windows`),
`syms_of_window`,
the scanner's artifacts `buffer`,
`width`,
`redirection` — `wset_buffer`,
`wset_combination`,
`wset_combination_limit`,
`wset_dedicated`,
`wset_display_table`,
`wset_new_normal`,
`wset_new_total`,
`wset_normal_cols`,
`wset_normal_lines`,
`wset_old_buffer`,
`wset_old_pointm`,
`wset_parent`,
`wset_pointm`,
`wset_start`,
`wset_temslot`,
`wset_vertical_scroll_bar_type`,
`wset_window_parameters`,
`Fwindow_configuration_p`,
`CHECK_WINDOW_CONFIGURATION`,
`init_window`,
`check_all_windows`,
`syms_of_window`,
`buffer`,
`width`,
`redirection`,
step 44b: window.el's `window-state-get` and `window-state-put` in part (`get_window_state`, `set_window_state`, `destroy_window_state`: the selected frame's root window, markers for starts and points, put into the whole frame with new windows when the sizes are equal, as tab-bar.el uses them),
the `tab` quit-restore type (`QR_TAB`, window.el's display-buffer-record-window) and the `tab` cases of `window-deletable-p`, `window--delete` (through `close_tab_hook`) and `quit-restore-window`,
an ACTION function argument of `display-buffer` and `pop-to-buffer` (`action_function`),
`set-window-configuration` resizing the windows to the frame's top margin (adjust_frame_size, when the configuration was saved with other tab bar lines)

## Deferred

other frames and tabs,
atomic windows,
window parameters other than `quit-restore` and the side window ones above (the `delete-window`, `other-window`, `split-window` and `delete-other-windows` function parameters, `window-preserved-size`, `window-parameters` table entries),
`display-buffer-alist` and the other action table entries (`preserve-size`, `body-lines` and `body-columns`, a `window-width` function),
`window-toggle-side-windows` (C-x w s) and window states (`window-state-get`, `window-state-put`, `window-swap-states`),
changing `window-sides-vertical` and `window-sides-reversed` with side windows shown (`window--sides-verticalize`, `window--sides-reverse` and their helpers),
persistent window parameters in window configurations (`window-persistent-parameters`),
recombining windows (so no main window needs a combination limit),
`window-combination-resize`,
resizing that takes from more than the next window (`window--resize-siblings`),
the children of a combination that split-window,
delete-window,
delete-other-windows or window-resize resizes (they share the change in proportion to their sizes, not by window--resize-this-window as for a frame size change),
`balance-windows`,
resizing commands,
`scroll-margin`,
`scroll-preserve-screen-position`,
pixel scrolling,
`recenter-positions`,
`switch-to-buffer-obey-display-actions`,
`switch-to-prev-buffer-skip`,
window commands while the minibuffer is selected,
`buffer-list-update-hook`;
own: window-accessors,
next-window,
coordinates-in-window-p,
after step 44b: `window-state-get` with WRITABLE true, window parameters, pixel and minimum sizes, putting a state into a window of another size or into a given WINDOW (`window--state-put-1` resizing), `window-state-buffers`

## Tests

test_window_mod (own; upstream has no ERT tests for windows),
test_window_sides_mod (own),
test_display_engine_mod
