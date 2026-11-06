# keyboard_mod

- Module: keyboard_mod, macros_mod and keyboard_suspend_mod (submodules)
- Upstream: keyboard.c (+ simple.el `command-execute`/`undefined`, macros.c, dispnew.c `ding`/`bitch_at_user`, frame.el `suspend-frame`, subr.el `suspend-hook`/`suspend-resume-hook`)
- State: done

## Ported

timers: Emacs is idle while waiting for terminal input,
and the wait runs the ripe timers and lasts until the next one (timer_mod),
`sit-for` runs ordinary timers;
`pre-command-hook` as a hook,
`last-repeatable-command`,
`num-input-keys`,
the exitfun of `set-transient-map` (`exit_transient_map`);
other descriptors watched while waiting (process.c `add_read_fd`, `delete_read_fd`: inotify's) and special events (the events `special-event-map` binds: queued with `insert_special_event`, also subr.el's `insert-special-event`, and handled whenever events are read, in `sit-for` and by `read-event`),
`read-event`'s SECONDS (also without a terminal);
keyboard macro recording (the macro part of `record_character`: every event read while no macro executes, unread events too; `finalize_keyboard_macro_characters` after each command that leaves no prefix argument; `discard-input` cancels the current command's events and ends the definition, so command errors and C-g do),
`last-input-event`,
the echo area kept while a macro's commands run;
macros.c in macros_mod: `start-kbd-macro` (APPEND re-executing the last macro unless NO-EXEC),
`end-kbd-macro` (REPEAT, LOOPFUNC),
`call-last-kbd-macro`,
`execute-kbd-macro` (strings, lists and arrays of events; COUNT; LOOPFUNC),
`store-kbd-macro-event`,
`cancel-kbd-macro-events`,
`defining-kbd-macro` (nil, t, `append`),
`last-kbd-macro`,
`executing-kbd-macro-index`,
`kbd-macro-termination-hook`,
the commands start-kbd-macro,
end-kbd-macro,
call-last-kbd-macro;
the disabled-command check of `command-execute` (a hook novice_mod installs),
undo boundaries before each command (`undo-auto--add-boundary`, `point_before_last_command_or_undo`),
deactivating the mark after a command that set `deactivate-mark`,
event queue,
`terminal_read_avail_input` (meta modes 0/1/2, UTF-8 keyboard decoding),
`unread-command-events`,
`discard-input`,
`input-pending-p`,
`read_character` core (with `input-method-function`: the first event of a key sequence goes to the current buffer's input method and the events it returns are read in turn, the rest through `unread-post-input-method-events`, with `this-command-keys` and the echo saved around it; `input-method-previous-message`),
`unread-post-input-method-events`,
`unread-input-method-events`,
`read_key_sequence` (input-decode-map, local-function-key-map, key-translation-map, upper-case and shift fallbacks),
`keyremap_step`,
`is_undefined_binding`,
`command_loop_1` (prefix arg, keyboard macros run by a command loop over their events, `execute_error` with discard-input and the bell),
subr.el `set-transient-map` (one key sequence, pushed over the previous overriding map as internal-push-keymap does; its MESSAGE as MESSAGE_BEGINNING and MESSAGE_END around the keys, no %k format, 2026-11-06),
redisplay before waiting for input (a hook, to keep keyboard_mod below display_engine_mod),
`recursive_edit_1` and the `exit` catch (`exit-recursive-edit`, `abort-recursive-edit`),
a pre-command hook (one function; it may change `this-command`),
`post-command-hook` (a hook; an error ends its run),
`has_unread_command_events`,
C-g from SIGINT read as the quit character,
subr.el `sit-for`,
subprocess output taken while waiting for input and in `sit-for` (`process_wake_fd`, `process_output_hook`);
`recent-keys` (the ring of the last 300 events and commands as (nil . COMMAND), `lossage-size`),
command errors logged in *Messages*;
`adjust_point_for_property` after each command (out of invisible text and text a display string or space replaces, as keyboard.c),
`disable-point-adjustment`,
`global-disable-point-adjustment`;
step 42c: echoing (`echo-keystrokes`, `echo-keystrokes-help` with help.el's help--append-keystrokes-help, `echo_add_key`, `echo_dash`, `echo_update` with simple.el's internal-echo-keystrokes-prefix and universal-argument--description, `echo_now`, `cancel_echoing`, `echo_length`, `echo_truncate`, `is_echo_keystrokes`),
the echo area wiped as typed events are read,
`help-char` and `help-event-list` (`is_help_character`),
`this_command_keys` as read_character adds events (`add_command_key`; `this-command-keys`, `this-command-keys-vector`, `this-single-command-keys`, `this-single-command-raw-keys`, `clear-this-command-keys`, `set--this-command-keys`),
`read-char`,
`read-char-exclusive` and `read-event` with PROMPT and SECONDS (`read_filtered_event`, the `ascii-character` of keypad keys),
`read-key-sequence` and `read-key-sequence-vector` (`read_key_sequence_vs`: PROMPT, CONTINUE-ECHO, DONT-DOWNCASE-LAST; `follow_key`, `active_maps`, `access_keymap_keyremap`; the case fallback through the case tables and `translate-upper-case-key-bindings`; fabricated events returned),
`recursion-depth`,
`top-level` (the status `TOP_LEVEL_CODE`),
`recursive-edit` (`recursive_edit_unwind`; `command_loop_level` counts recursive edits, not minibuffers),
the start of command_loop_1 in a recursive edit (post-command-hook, `last-command`),
`exit-recursive-edit` with `user_error`,
`lossage-size` with ARG (`update_recent_keys`),
`num-nonmacro-input-events`,
`event-convert-list` (`parse_solitary_modifier`, `make_control_character`),
`internal-event-symbol-parse-modifiers` (`lispy_modifier_list`),
`safe_execute_hooks` for pre- and post-command-hook (`safe_run_hook_funcall`, `safe_execute_hooks_1`, `safe_execute_hooks_error`, `safe_execute_hooks_maybe_narrowed`: the failing function is shown and removed),
`open-dribble-file` (file_input_output_mod) and the dribble part of `record_character`,
idle timers started before the echo wait,
`insert-special-event`,
the input queue (`kbd_buffer_get_event`, `read_event_from_main_queue`, `next_kbd_event`, `clear_event`, `get_input_pending`, `readable_events`, `detect_input_pending`, `detect_input_pending_execute_timers`, `requeued_events_pending_p`, `requeued_command_events_pending_p`, `kbd_buffer_events_waiting`, `gobble_input`),
`quit_throw_to_read_char`,
`cmd_error_internal`,
`command_loop` and `command_loop_2`;
subr.el's C-M-c and C-] bindings;
the command state (`last-command`, `real-this-command`, `real-last-command`, `last-repeatable-command`) is `type(command_type), pointer` and `last-prefix-arg` a `prefix_argument_type` (A7);
where upstream holds a keyboard macro there (a key bound to a macro, `call-last-kbd-macro`, `kmacro-call-macro`) it points at `last_macro_command`,
whose `macro` handle names the macro's events in the registry `command_macros` (A7a; a handle, not an allocatable component, keeps the generated commands fast to compile),
so C-x z repeats it;
step 42e: `suspend-emacs` with `stuff_buffered_input` (submodule keyboard_suspend_mod, because it needs terminal_output_mod and display_update_mod, which use keyboard_mod: `suspend_hook` and `suspend_resume_hook`, subr.el's; the terminal reset and set again, terminal_output_mod's `reset_all_system_modes` and `initialize_all_system_modes`; the frame resized if the terminal's size changed meanwhile) and frame.el's `suspend-frame` (in keyboard_suspend_mod too),
bound to C-z and C-x C-z (subr.el binds suspend-emacs, which bindings.el substitutes);
a SIGWINCH while waiting for input or in `sit-for` redisplays,
which makes the pending size change (process.c:wait_reading_process_output).

## Deferred

idle hooks;
mouse,
focus and frame events and posn values (`make_lispy_event`, `make_lispy_position`, `make_lispy_tty_position`, `make_lispy_movement`, `make_lispy_switch_frame`, `make_lispy_focus_in`/`_out`, `make_scroll_bar_position`, `posn-at-point`, `posn-at-x-y`, `internal-track-mouse`, `internal-handle-focus-in`, `discard_mouse_events`, `some_mouse_moved`, `tracking_off`, `xevent_start`, `lucid_event_type_list_p`, `modify_event_symbol`, `line_number_mode_hscroll`, `save_line_number_display_width`: no mouse support yet, xterm-mouse-mode on the terminal included);
menus,
the tab bar and help echo (`menu_bar_items`, `menu_bar_item`, `menu_item_eval_property(_1)`, `menu_separator_name_p`, `parse_menu_item`, `read_char_minibuf_menu_prompt`, `read_char_x_menu_prompt`, `read_menu_command`, `record_menu_key`, `tab_bar_items`, `init_tab_bar_items`, `parse_tab_bar_item`, `process_tab_bar_item`, `append_tab_bar_item`, `set_prop_tab_bar`, `coords_in_menu_bar_window`, `coords_in_tab_bar_window`, `show_help_echo`, `gen_help_event`, `kbd_buffer_store_help_event`, `help_echo_substitute_command_keys`);
several keyboards for several terminals (`allocate_kboard`, `delete_kboard`, `init_kboard`, `wipe_kboard`, `push_kboard`, `pop_kboard`, `temporarily_switch_to_single_kboard`, `not_single_kboard_state`, `restore_kboard_configuration`, `event_to_kboard`);
to 42e with sysdep.c: terminal modes (`set-input-mode`, `set-input-interrupt-mode`, `set-output-flow-control`, `set-quit-char`),
the subshell instead of suspending (`cannot-suspend`: GNU/Linux has job control, so upstream runs a subshell only when it is set),
frame.el's `suspend-tty` and `iconify-or-deiconify-frame` branches of `suspend-frame` (a secondary terminal, a graphical frame),
signals and polling (`handle_interrupt`, `deliver_interrupt_signal`, `handle_async_input`, `handle_input_available_signal`, `deliver_input_available_signal`, `process_pending_signals`, `poll_for_input(_1)`, `start_polling`, `stop_polling`, `bind_polling_period`, `set_poll_suppress_count`, `input_polling_used`, the user signals `add_user_signal`, `deliver_user_signal`, `handle_user_signal`, `find_user_signal_name`, `store_user_signal_events`),
auto-saving after so many keystrokes (`record_auto_save`, `force_auto_save_soon`, with fileio.c);
quit while computing (C-g is an ordinary key; `kbd_buffer_store_buffered_event`'s quit check);
`while-no-input` and its ignored events (`init_while_no_input_ignore_events`, `is_ignored_event`);
`help-form` and `prefix-help-command` (`read_char_help_procedure_unwind`: help.el's describe-prefix-bindings needs describe-bindings with PREFIX);
`safe_execute_hooks_2` (for window-scroll-functions, 42d);
the long-line narrowing of the command hooks;
macros that are symbols,
the (t . EVENT) and (no-record . EVENT) forms of `unread-command-events`,
translation functions in keyremaps,
the `minibuffer-quit` condition,
`minibuffer-error-function`,
`ring-bell-function`,
prefix-command-echo-keystrokes-functions as a hook (its one upstream function is built in);
Not applicable: the setjmp/longjmp of reading (`save_getcjmp`, `restore_getcjmp`),
blocked input (`unblock_input`, `unblock_input_to`, `totally_unblock_input`) and the waiting flags signal handlers read (`set_waiting_for_input`, `clear_waiting_for_input`, `clear_input_pending`: the SIGINT handler only sets a flag the wait polls),
the growable event queue's slot count (`kbd_buffer_nr_stored`),
the Lisp startup form (`top_level_1`, `top_level_2`, `eval_dyn`),
batch mode (`read_stdin`, `write_stdout`),
text conversion (`restore_reading_key_sequence`, `kbd_buffer_get_event_1`, `kbd_buffer_get_event_2`: input-method preedit text),
the KVAR setters (`kset_*`),
GC (`mark_kboards`),
`syms_of_keyboard` and `syms_of_keyboard_for_pdumper`,
`keys_of_keyboard` (special-event-map's frame events),
the scanner's artifacts `Copyright`,
`Second`,
`instance`;
waits for the GUI: the tool bar (`tool_bar_items`, `init_tool_bar_items`, `parse_tool_bar_item`, `process_tool_bar_item`, `append_tool_bar_item`, `set_prop`, `toolkit_menubar_in_use`),
X timestamps (`Time_to_position`, `position_to_Time`),
X selection requests (`process_special_events`, `swallow_events`)

## Tests

test_keyboard_mod (own; keyboard-tests.el and callint-tests.el need Lisp);
ports keyboard-tests.el: the read-key-sequence tests (prefix, unbound, decode maps, function-key map, key-translation map, upcase and shifted fallbacks, dont-downcase-last),
keyboard-lossage-size,
keyboard-inhibit-interaction;
own: read-char,
this-command-keys,
echo-keystrokes,
event-symbols,
recursive-edit/top-level;
suspend-emacs is tested in test_display_update_mod (it sets up the terminal, which must not happen before test_terminal_xterm_mod)
