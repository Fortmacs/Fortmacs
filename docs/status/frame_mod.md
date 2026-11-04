# frame_mod

- Module: frame_mod, frame_cmds_mod (submodule)
- Upstream: frame.h (+ window.h pixel edges, window.c `window_body_width` and `resize_frame_windows`, window.el `frame-windows-min-size`, frame.c's divider widths, frame.el's window dividers); step 44a: frame.c (terminal frames), frame.el (C-x 5), minibuf.c `move_minibuffers_onto_frame`
- State: partial

## Ported

`frame_type` (terminal, current and desired matrices, face table, cursor, root, minibuffer and selected window handles, FRAME_RIF `rif`, the font driver, FRAME_COLUMN_WIDTH/FRAME_LINE_HEIGHT, FRAME_SMALLEST_CHAR_WIDTH/FONT_HEIGHT, text_cols/text_lines),
`selected_frame` (frame.c),
`frame-selected-window`,
`FRAME_WINDOW_P`,
`FRAME_LINES`,
`FRAME_COLS`,
WINDOW_LEFT_EDGE_X,
WINDOW_TOP_EDGE_Y,
WINDOW_PIXEL_WIDTH/HEIGHT,
`window_body_width` in pixels;
step 42e: a pending size change (`new_width`, `new_height`, `new_size_pending` for new_size_p),
`needs_redraw` for frame.h:garbaged (frame.h:SET_FRAME_GARBAGED),
window.c's `resize_frame_windows` (here, not in window_mod: it needs the frame, and window_mod is below frame_mod; a root window with children shares the change by the windows' normal and minimum sizes through window_mod's port of window.el's `window--resize-root-window`, first with `window_min_height` and `window_min_width`, then ignoring them),
window.el's `frame-windows-min-size` with IGNORE `safe` (`window-min-size`, `window--min-size-1`, `window-safe-min-size`);
step 45c: frame parameters as typed components (`frame_parameters_type`: `right-divider-width`, `bottom-divider-width`),
a frame's own (`parameters`) and `default-frame-alist` as `default_frame`,
the divider widths a frame draws (frame.h `right_divider_width`, `bottom_divider_width`: 0 on a terminal frame, as upstream, whose tty frames only store the parameters),
frame.c's `frame-right-divider-width` and `frame-bottom-divider-width`,
window.c's `window-right-divider-width` and `window-bottom-divider-width` (window.h WINDOW_RIGHT_DIVIDER_WIDTH and WINDOW_BOTTOM_DIVIDER_WIDTH, here because window_mod is below frame_mod);
frame.el's `window-divider-mode` (global, with `window-divider-mode-hook`),
`window-divider-mode-apply` (modify-all-frames-parameters on the frame's parameters and `default_frame`, a width of 0 or less left out of the latter),
`window-divider-default-places` (`WINDOW_DIVIDER_DEFAULT_PLACES_BOTTOM_ONLY`, `_RIGHT_ONLY`, `_BOTTOM_AND_RIGHT`),
`window-divider-default-bottom-width`,
`window-divider-default-right-width`,
`window-divider-width-valid-p` (`is_window_divider_width_valid`, an integer);
step 44a: several frames on one terminal: a frame registry (`frame_pointer_type` slots with handles, `selected_frame` a pointer to the selected one, `selected_frame%handle` for frame.c:Fselected_frame),
frame.c's `Vframe_list` order (newest first),
`frame-list`,
`frame-live-p` (`is_frame_live`),
`next-frame` and `previous-frame` (MINIFRAME absent),
`frame_get` (`find_frame`),
`window-frame` (from the window's root),
frame names F<num> (`frame_next_F_name`, `frame_name_fnn_p`, `set_term_frame_name`) shown by the mode line's %F,
frame.el:`make-frame` with frame.c's `make_terminal_frame` and `make_frame` (the current buffer, another if hidden),
`select-frame` (`do_switch_frame`: the terminal's top frame, termchar.h `top_frame`, gets the terminal state and size and is redrawn; each frame keeps its own buffer lists, exchanged with buffer_mod's),
minibuf.c `move_minibuffers_onto_frame` (`minibuffer-follows-selected-frame` t),
subr.el `with-selected-frame` (`select_frame_begin`/`select_frame_end`),
`delete-frame` (`delete_frame`, `other_frames`; `delete-frame-choose-selected` mru: frame.el `get-mru-frame`, `frame-use-time`),
`delete-other-frames` (ICONIFY does nothing on a terminal),
`other-frame`,
`select-frame-by-name` (`make-frame-names-alist`, `frame-list-1`),
`set-frame-name`,
`make-frame-command`;
C-x 5 2,
1,
0,
o,
b,
C-o,
f,
C-f,
r,
d

## Deferred

child frames,
the other frame parameters (`frame-parameter`, `frame-parameters`, `modify-frame-parameters` and `modify-all-frames-parameters` as such, `initial-frame-alist`, `window-system-default-frame-alist`),
pixelwise frame and window sizes,
internal borders;
`window-size-fixed`;
window dividers drawn on a window-system frame (wait for the GUI: frame.c `gui_set_right_divider_width`, `gui_set_bottom_divider_width`, the frame's text area shrunk by them, the faces `window-divider`, `window-divider-first-pixel`, `window-divider-last-pixel` used);
the :set functions of the window divider options (Customize: setting one while the mode is on applies it again);
the menu bar's divider commands (menu-bar.el, with the menu bar);
after step 44a: frame parameters (`frame-parameter(s)`, `modify-frame-parameters` beyond the name, `no-other-frame`, `delete-before`, `auto-hide-function`),
`before-make-frame-hook`,
`after-make-frame-functions`,
`delete-frame-functions`,
`after-delete-frame-functions`,
`frame-auto-hide-function` (its default, iconify-frame, does nothing on a terminal, which window_delete does),
`delete-frame-choose-selected` other than mru,
`make-frame-on-display`,
`make-frame-on-monitor`,
`make-frame-on-current-monitor`,
`clone-frame` (C-x 5 c: window-state-get),
`undelete-frame` (C-x 5 u) and `undelete-frame-mode`,
`other-frame-prefix` (C-x 5 5: display-buffer-override-next-command),
`select-frame-by-id`,
`frame-id`,
`iconify-frame`,
`make-frame-visible`/`invisible`,
`raise-frame`/`lower-frame` (nothing on a terminal root frame),
`select-frame-set-input-focus` (select-frame here),
`last-nonminibuffer-frame`,
`frame-terminal`,
several terminals (`make-frame-on-tty`),
minibuffer-only and minibuffer-less frames,
`minibuffer-follows-selected-frame` other than t,
`pop-up-frames`,
`pop-up-frame-function`,
`pop-up-frame-alist`,
`display-buffer-reuse-frames`,
the `pop-up-frame-parameters`,
`inhibit-switch-frame` and `reusable-frames` frame-object entries,
`compose-mail-other-frame` (C-x 5 m),
`xref-find-definitions-other-frame` (C-x 5 .),
`project-other-frame-command` (C-x 5 p),
`find-file-other-frame` WILDCARDS

## Tests

test_dispnew_mod,
test_xdisp_mod,
test_fake_gui_mod,
test_frame_mod (own; upstream has no window divider tests)
