# terminal_output_mod

- Module: terminal_output_mod
- Upstream: term.c (+ sysdep.c `reset_all_sys_modes`, `init_all_sys_modes`, dispnew.c `no-redraw-on-reenter`)
- State: partial

## Ported

`init_tty` (capabilities, `is_tty_capable`, colors incl. 24-bit, size, raw mode),
`delete_tty`,
`term_get_fkeys` (terminfo keys into input-decode-map),
`calculate_costs` (motion part),
bell,
terminal modes,
scroll region,
insert/standout modes,
cursor show/hide,
`cursor_to`,
clearing,
`write_glyphs` (incl. the bottom-right cell),
`insert_glyphs`,
`delete_glyphs`,
`ins_del_lines`,
`turn_on_face`/`turn_off_face`,
`encode_terminal_code` (UTF-8);
step 42d: `tty-display-color-p`,
`tty-display-color-cells`,
`tty-type` (`tty_type_name`),
`controlling-tty-p` (the tty is named /dev/tty, as upstream's initial terminal),
`tty-no-underline`;
step 42e: sysdep.c's `reset_all_sys_modes` and `init_all_sys_modes` (here, not in system_dependent_mod: they need the terminal and the frame, and system_dependent_mod is below both) with the terminal part of `reset_sys_modes` (the last line cleared, full-screen mode left) and of `init_sys_modes` (full-screen mode entered, the frame marked for redraw, `needs_redraw`, unless `redraw_after_suspend` is false; one terminal, the selected frame's),
dispnew.c's `no-redraw-on-reenter` as `redraw_after_suspend` (named positively, its value inverted; here because init_all_sys_modes reads it and display_update_mod is above terminal_output_mod);
cited: `term_get_fkeys_1`,
`append_glyph` and `tty_append_glyph` (display_engine_mod's glyph production),
`maybe_fatal`,
`fatal`,
`vfatal` (init_tty's error goes back to main, which prints it and exits)

## Deferred

glyph production (with redisplay),
menus,
mouse/GPM,
color-mode switching,
char/line ins-del cost tables,
tty-mode strings,
DOS/Windows/Android;
`tty-erase-char` (set by init_sys_modes);
after step 42d: suspending the terminal and its output buffer (with sysdep.c, 42e) (`Fsuspend_tty`, `Fresume_tty`, `Ftty__output_buffer_size`, `Ftty__set_output_buffer_size`, `dissociate_if_controlling_tty`);
the screen's size in pixels (TIOCGWINSZ, with sysdep.c, 42e) (`Ftty_display_pixel_width`, `Ftty_display_pixel_height`, `tty_display_dimension`);
several terminals and terminal frames,
child frames (step 44) (`Ftty_top_frame`, `Ftty_frame_at`, `Ftty_frame_edges`, `Ftty_frame_geometry`, `Ftty_frame_list_z_order`, `Ftty_frame_restack`, `tty_frame_at`, `tty_frame_geometry`, `create_tty_output`, `tty_free_frame_resources`, `set_tty_hooks`, `clear_tty_hooks`);
the mouse (GPM, xterm-mouse) (`Fgpm_mouse_start`, `Fgpm_mouse_stop`, `close_gpm`, `handle_one_term_event`, `term_mouse_click`, `term_mouse_moveto`, `term_mouse_position`, `mouse_get_xy`, `current_Time`, `tty_draw_row_with_mouse_face`, `tty_write_glyphs_with_face`);
tty menus (`tty_menu_activate`, `tty_menu_add_pane`, `tty_menu_add_selection`, `tty_menu_calc_size`, `tty_menu_create`, `tty_menu_destroy`, `tty_menu_display`, `tty_menu_help_callback`, `tty_menu_last_menubar_item`, `tty_menu_locate`, `tty_menu_make_room`, `tty_menu_new_item_coords`, `tty_menu_search_pane`, `tty_menu_show`, `tty_pop_down_menu`, `tty_highlight_if_desired`, `read_menu_input`, `save_and_enable_current_matrix`, `restore_desired_matrix`, `screen_update`, `free_saved_screen`);
compositions and glyphless characters (`produce_composite_glyph`, `append_composite_glyph`, `produce_glyphless_glyph`, `append_glyphless_glyph`);
line insertion and deletion costs (with scroll.c) (`per_line_cost`, `string_cost_one_line`);
color-mode switching (`tty-color-mode`) and `tty-mode-set-strings` (`set_tty_color_mode`, `tty_setup_colors`, `tty_default_color_capabilities`, `tty_send_additional_strings`);
Not applicable: `syms_of_term`

## Tests

test_terminal_output_mod (own; byte-exact xterm output, a pty);
own: tty-display-functions
