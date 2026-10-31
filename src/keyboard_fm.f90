! keyboard_fm.f90 -- keyboard input and the editor command loop.
! Ported from GNU Emacs 31.1 src/keyboard.c.
!
! Copyright (C) 1985-1989, 1993-1997, 1999-2026 Free Software Foundation,
! Inc.
! Copyright (C) 2026 The Fortmacs authors
!
! This file is part of Fortmacs.
!
! Fortmacs is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! Fortmacs is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with Fortmacs.  If not, see <https://www.gnu.org/licenses/>.

!> Input flows: terminal bytes -> `kbd_store_bytes' (meta handling and
!> keyboard decoding) -> the event queue -> `read_event' (after
!> `unread_command_events') -> `read_key_sequence' (active keymaps plus
!> input-decode-map, local-function-key-map and key-translation-map)
!> -> `command_loop_step', which runs the bound command.
!>
!> Events are fixnums (characters with modifier bits) and symbols
!> (function keys).  Without a terminal (`set_input_fd' not called) the
!> event queue is the only source, which is how tests drive the loop;
!> `read_event' then reports end of input instead of blocking.
!>
!> Keyboard macros run as upstream (macros.c:Fexecute_kbd_macro): a
!> command loop reads the macro's events until they run out, and an
!> error ends the macro and reaches the command loop's error handler.
!> They are recorded as upstream: every event `read_event' returns while
!> no macro is executing goes into the macro being defined
!> (keyboard.c:record_char), the events of a command become part of it
!> once the command has finished without leaving a prefix argument
!> (`finalize_kbd_macro_chars' in command_loop_1), and `discard_input'
!> (so any command error) drops the events of the current command and
!> ends the definition.  macros.c is the submodule macros_fm.
!>
!> Keystrokes are echoed as keyboard.c does (`echo_keystrokes'), in the
!> echo area, whose text is kept in `last_message'.
!>
!> Not ported yet: idle hooks, mouse and
!> switch-frame events, the (t . EVENT) and (no-record . EVENT) forms of
!> `unread_command_events', `help_procedure' and `prefix_help_command', quit
!> while computing (C-g is an ordinary key), `ring_bell_function', the
!> `minibuffer_quit' condition (a quit in the minibuffer is handled as any
!> quit) and `minibuffer_error_function'.
module keyboard_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use error_fm, only: status_type, set_error, format_error, error_name, FM_GENERIC_ERROR, FM_QUIT, FM_USER_ERROR, &
                        FM_INHIBITED_INTERACTION, FM_TOP_LEVEL
    use handle_fm, only: handle_type, null_handle
    use data_fm, only: value_eq
    use lisp_fm, only: value_type, V_LIST, nil, make_fixnum, &
                       make_keymap_ref, vtag, nilp, fixnump, symbolp, &
                       stringp, xfixnum, xsymbol, xstring, list_elements, list_elt, make_list, symbols, &
                       command_proc
    use lread_fm, only: intern, sym
    use character_fm, only: CHAR_SHIFT, CHAR_META, CHAR_MODIFIER_MASK, CHAR_CTL, CHAR_ALT, CHAR_SUPER, CHAR_HYPER, &
                            bytes_by_char_head, byte8_to_char, &
                            multibyte_length, string_char_and_length, is_char_byte8, char_to_byte8, &
                            char_resolve_modifier_mask, MAX_CHAR
    use keymap_fm, only: is_keymap, is_keymap_live, access_keymap, &
                         current_active_maps, &
                         key_description, parse_modifiers, apply_modifiers, &
                         overriding_terminal_local_map, pushed_terminal_local_map, keymap_events, lookup_key, command_remapping, &
                         push_key_description, key_parse, M_UP, M_DOWN, M_DRAG, M_CLICK, M_DOUBLE, M_TRIPLE
    use callint_fm, only: call_interactively, funcall, current_prefix_arg, prefix_arg, &
                          prefix_numeric_value, this_command, command_name_of
    use buffer_fm, only: buffers, cur, deactivate_mark_variable, transient_mark_mode, current_buffer, set_buffer
    use casetab_fm, only: downcase, is_uppercase
    use undo_fm, only: undo_auto_add_boundary, point_before_last_command_or_undo, &
                       buffer_before_last_command_or_undo
    use sysdep_fm, only: emacs_read, input_available, interrupt_pending, inputs_available, emacs_write
    use eval_fm, only: hook_type
    use textprop_fm, only: get_char_property_and_overlay, get_property_and_range, text_prop_means_invisible, &
                           next_single_char_property_change, previous_single_char_property_change, get_pos_property
    use itree_fm, only: overlay_start, overlay_end, overlay_get
    use editfns_fm, only: goto_char
    use timer_fm, only: timer_check, timer_start_idle, timer_stop_idle, timers_run, timer_error_message
    use coding_fm, only: keyboard_coding_system, coding_system_type, decode_coding_string
    implicit none(type, external)
    private

    public :: current_input_meta_mode, set_input_meta_mode, set_input_fd, kbd_store_bytes, &
              tty_read_avail_input, unread_command_events, &
              discard_input, is_input_pending, read_event, has_unread_command_events, read_input_event, store_event
    public :: read_key_sequence, command_loop_step, command_loop, &
              request_exit, is_exit_requested, set_transient_map, recursive_edit_1, &
              throw_exit, throw_exit_string, exit_recursive_edit, abort_recursive_edit, sit_for
    public :: exit_transient_map
    public :: add_read_fd, delete_read_fd, insert_special_event, run_special_events, has_pending_special_events, &
              read_fd_proc, special_event_proc
    public :: execute_kbd_macro, ring_bell, bitch_at_user, is_executing_kbd_macro
    public :: start_kbd_macro, end_kbd_macro, call_last_kbd_macro, store_kbd_macro_char, store_kbd_macro_event, &
              finalize_kbd_macro_chars, cancel_kbd_macro_events, finish_kbd_macro, is_defining_kbd_macro, &
              syms_of_macros
    public :: recent_keys, lossage_size, message_log_proc
    public :: read_char, read_char_exclusive, read_filtered_event, read_key_sequence_vs, barf_if_interaction_inhibited
    public :: this_command_keys_vector, this_single_command_keys, this_single_command_raw_keys, &
              set__this_command_keys, clear_this_command_keys, recursion_depth, top_level, recursive_edit
    public :: cancel_echoing, echo_now, prefix_command_update, internal_echo_keystrokes_prefix, is_help_char
    public :: event_convert_list, event_symbol_parse_modifiers, parse_solitary_modifier, lispy_modifier_list, &
              make_ctrl_char, ascii_character, safe_run_hooks
    public :: READ_KEY_ELTS, redisplay_proc, transient_exit_proc, kbd_macro_loop_proc

    !> keyboard.c:READ_KEY_ELTS, the longest key sequence.
    integer, parameter :: READ_KEY_ELTS = 30

    integer(int64), parameter :: M_META = int(CHAR_META, int64), &
                                 M_SHIFT = int(CHAR_SHIFT, int64), &
                                 M_MASK = int(CHAR_MODIFIER_MASK, int64)

    !> Keymaps translating key sequences before lookup (null: none).
    !> KVAR input_decode_map: terminal escape sequences -> function keys.
    type(handle_type), public :: input_decode_map
    !> KVAR Vlocal_function_key_map: applied only to undefined sequences.
    type(handle_type), public :: local_function_key_map
    !> keyboard.c:Vkey_translation_map.
    type(handle_type), public :: key_translation_map

    !> keyboard.c:last_command_event, KVAR last_command and friends
    !> (Vthis_command is callint_fm's `this_command').
    type(value_type), public :: last_command_event, &
                                real_this_command, last_command, &
                                real_last_command, last_prefix_arg
    public :: this_command
    !> Last command that may be repeated.
    !> The last command executed that was not bound to an input event.
    !> This is the command `repeat' will try to repeat.
    !> Taken from a previous value of `real_this_command'.  Port of
    !> keyboard.c last-repeatable-command.
    type(value_type), public :: last_repeatable_command
    !> Number of complete key sequences read as input so far.
    !> This includes key sequences read from keyboard macros.
    !> The number is effectively the number of interactive command
    !> invocations.  Port of keyboard.c num-input-keys.
    integer(int64), public :: num_input_keys = 0
    !> keyboard.c:Vthis_command_keys_shift_translated.
    logical, public :: this_command_keys_shift_translated = .false.
    !> The keys of the command being run (this-command-keys).
    type(value_type), allocatable, public :: this_command_keys(:)
    !> Stand-in for the echo area: the last message shown.
    character(len=:), allocatable, public :: last_message
    !> Stand-in for minibuffer.el's minibuffer--message-overlay: text shown
    !> after the active minibuffer's contents until the next command
    !> (`minibuffer_message'); unallocated for none.
    character(len=:), allocatable, public :: minibuffer_message_text
    !> Stand-in for minibuffer.el's minibuffer-message-overlay: the text
    !> `message' shows after the active minibuffer's contents
    !> (set-minibuffer-message) until the next input
    !> (clear-minibuffer-message); unallocated for none.
    character(len=:), allocatable, public :: set_minibuffer_message_text

    !> tty->meta_key: 0 strips the 8th bit, 1 makes it Meta, 2 passes
    !> bytes on to keyboard decoding (UTF-8).
    integer :: meta_key = 2
    !> Terminal input file descriptor, or -1 for none.
    integer(int32) :: input_fd = -1
    !> Bytes of an incomplete UTF-8 sequence.
    character(len=:), allocatable :: pending_bytes
    ! Bytes of an incomplete sequence in the keyboard coding system.
    character(len=:), allocatable :: pending_encoded

    !> The event queue (keyboard.c:kbd_buffer): events(head:tail).
    type(value_type), allocatable :: events(:)
    integer(int64) :: head = 1, tail = 0
    !> Vunread_command_events, read before the queue.
    type(value_type), allocatable :: unread(:)
    integer(int64) :: nunread = 0

    abstract interface
        !> Redisplay all frames (xdisp.c:redisplay); also the form of a
        !> post-command hook.  Not a port.
        subroutine redisplay_proc()
            implicit none(type, external)
        end subroutine redisplay_proc

        !> The ON-EXIT function of a transient map.  Not a port.
        subroutine transient_exit_proc()
            implicit none(type, external)
        end subroutine transient_exit_proc

        !> A keyboard macro's LOOPFUNC: called before each iteration of the
        !> macro, which stops when it returns false.  Not a port.
        logical function kbd_macro_loop_proc()
            implicit none(type, external)
        end function kbd_macro_loop_proc

        !> Log `text' in the *Messages* buffer as echo-area output
        !> (xdisp.c via print.c:printchar).  Not a port.
        subroutine message_log_proc(text)
            implicit none(type, external)
            character(len=*), intent(in) :: text
        end subroutine message_log_proc

        !> Run a disabled command's handler instead of the command, if
        !> CMD is disabled: `handled' says it was.  Not a port.
        subroutine disabled_command_proc(cmd, handled, status)
            import :: value_type, status_type
            implicit none(type, external)
            type(value_type), intent(in) :: cmd
            logical, intent(out) :: handled
            type(status_type), intent(out) :: status
        end subroutine disabled_command_proc

        !> Run when descriptor FD, given to `add_read_fd', is readable.
        !> Not a port.
        recursive subroutine read_fd_proc(fd)
            import :: int32
            implicit none(type, external)
            integer(int32), intent(in) :: fd
        end subroutine read_fd_proc

        !> Handle the special event ARG (as `insert_special_event' was
        !> given it).  Not a port.
        recursive subroutine special_event_proc(arg, status)
            import :: int64, status_type
            implicit none(type, external)
            integer(int64), intent(in) :: arg
            type(status_type), intent(out) :: status
        end subroutine special_event_proc
    end interface

    !> Other descriptors watched while waiting for input, with the
    !> procedure run when one is readable (process.c:add_read_fd's
    !> fd_callback_info, used by inotify).
    type :: read_fd_type
        integer(int32) :: fd = -1
        procedure(read_fd_proc), pointer, nopass :: callback => null()
    end type read_fd_type
    type(read_fd_type), allocatable :: read_fds(:)

    !> Special events not handled yet (the kbd_buffer events that
    !> `special_event_map' binds, such as file-notify events): a handler
    !> and an argument identifying the event's data for the module that
    !> queued it.
    type :: special_event_type
        procedure(special_event_proc), pointer, nopass :: handler => null()
        integer(int64) :: arg = 0
    end type special_event_type
    type(special_event_type), allocatable :: special_events(:)
    integer :: n_special_events = 0

    !> Checks for disabled commands (simple.el:command-execute's `disabled'
    !> property test and `disabled_command_function'), set by novice_fm;
    !> null for none.
    procedure(disabled_command_proc), pointer, public :: disabled_command_hook => null()

    !> Logs the error messages of the command loop in *Messages*
    !> (xdisp_fm sits above this module; set by simple_fm); null for
    !> none.
    procedure(message_log_proc), pointer, public :: message_log_hook => null()

    !> keyboard.c:MIN_NUM_RECENT_KEYS and MAX_NUM_RECENT_KEYS, the limits
    !> of `lossage_size' (INT_MAX / EMACS_INT_WIDTH for the maximum).
    integer, parameter :: MIN_NUM_RECENT_KEYS = 100, MAX_NUM_RECENT_KEYS = 33554431
    !> keyboard.c:lossage_limit, the number of events and commands
    !> `recent_keys' keeps (3 * MIN_NUM_RECENT_KEYS at first).
    integer :: lossage_limit = 3*MIN_NUM_RECENT_KEYS

    !> keyboard.c:recent_keys, a ring of the last `lossage_limit' input
    !> events not from keyboard macros, and of the commands run, as
    !> upstream's pseudo-events (nil . COMMAND): `recent_is_command' marks
    !> those, whose command (nil for an undefined key) is in
    !> `recent_events'.  The next entry goes at `recent_keys_index'
    !> (1-based); `total_keys' entries are filled.
    type(value_type), allocatable :: recent_events(:)
    logical, allocatable :: recent_is_command(:)
    integer :: recent_keys_index = 1, total_keys = 0

    !> Called by read_event before it waits for terminal input, so the
    !> screen is current.  Set by the program (xdisp_fm sits above this
    !> module); null in tests.
    procedure(redisplay_proc), pointer, public :: redisplay_hook => null()

    !> simple.el's `deactivate_mark', which the command loop calls when a
    !> command set `deactivate_mark' (call0 (Qdeactivate_mark)).  Set by
    !> simple_fm (above this module); null in tests that don't load it.
    procedure(redisplay_proc), pointer, public :: deactivate_mark_function => null()

    !> If true, suppress point adjustment after executing a command.
    !>
    !> After a command is executed, if point moved into a region that has
    !> special properties (e.g. composition, display), Emacs adjusts point
    !> to the boundary of the region.  But when a command leaves this
    !> variable at a true value, this point adjustment is suppressed.
    !>
    !> This variable is set to false before reading a command, and is
    !> checked just after executing the command.  Port of keyboard.c
    !> disable-point-adjustment.
    logical, public :: disable_point_adjustment = .false.

    !> If true, always suppress point adjustments.
    !>
    !> The default value is false, in which case point adjustments are
    !> suppressed only after special commands that leave
    !> `disable_point_adjustment' (which see) at a true value.  Port of
    !> keyboard.c global-disable-point-adjustment.
    logical, public :: global_disable_point_adjustment = .false.

    !> Normal hook run after each command is executed.  (An error in one
    !> of its functions ends the run; safe_run_hooks's message and removal
    !> of the function are deferred.)  Port of keyboard.c
    !> post-command-hook.
    type(hook_type), target, public :: post_command_hook = hook_type(name="post_command_hook")

    !> Normal hook run before each command is executed.  (An error in one
    !> of its functions ends the run; safe_run_hooks's message and removal
    !> of the function are deferred.)  Port of keyboard.c
    !> pre-command-hook.
    type(hook_type), target, public :: pre_command_hook = hook_type(name="pre_command_hook")

    abstract interface
        !> A command run in place of the command CMD about to be executed
        !> (`this_command_wrapper').
        recursive subroutine command_wrapper_proc(cmd, status)
            import :: value_type, status_type
            implicit none(type, external)
            type(value_type), intent(in) :: cmd
            type(status_type), intent(out) :: status
        end subroutine command_wrapper_proc
    end interface

    !> A command to execute in place of the command about to be executed,
    !> which it is given: a function of `pre_command_hook' sets it (upstream,
    !> such a function sets `this_command' to a lambda calling the command),
    !> and the command loop clears it before executing it.
    procedure(command_wrapper_proc), pointer, public :: this_command_wrapper => null()

    !> Hook run when `kill_emacs' is called.
    !> Since `kill_emacs' may be invoked when the terminal is disconnected
    !> (or in other similar situations), functions placed on this hook
    !> should not expect to be able to interact with the user.  To ask for
    !> confirmation, see `kill_emacs_query_functions' instead.  Port of
    !> emacs.c kill-emacs-hook (here, below the modules that add to it;
    !> emacs_fm runs it).
    type(hook_type), target, public :: kill_emacs_hook = hook_type(name="kill_emacs_hook")

    !> Ring the bell of the selected frame's terminal (terminal.c:
    !> ring_bell's FRAME_TERMINAL (f)->ring_bell_hook); set by the
    !> program, null without a terminal.
    procedure(redisplay_proc), pointer, public :: ring_bell_hook => null()

    !> Subprocess output: while waiting for input, `process_wake_fd' (the
    !> descriptor the process workers wake the main thread through, -1 for
    !> none) is watched too, and when it is ready `process_output_hook'
    !> takes the output (runs filters and sentinels), as
    !> process.c:wait_reading_process_output does.  Set by process_fm.
    integer(int32), public :: process_wake_fd = -1
    !> What the command loop calls when `process_wake_fd' is ready: it takes
    !> the subprocess output.  Set by process_fm.
    procedure(redisplay_proc), pointer, public :: process_output_hook => null()

    !> macros.c:Vexecuting_kbd_macro: `macro_events' are the events of
    !> the keyboard macro being executed (when `macro_active'); a string
    !> macro (`macro_string') gives characters 128..255 the meta bit.
    !> `macro_forced_end' stands for the value t, which ends the macro.
    type(value_type), allocatable :: macro_events(:)
    logical :: macro_active = .false., macro_string = .false., macro_forced_end = .false.
    !> macros.c:executing_kbd_macro_index, the index of the next event.
    integer(int64), public, protected :: executing_kbd_macro_index = 0
    !> macros.c:executing_kbd_macro (non-nil): the most recently started
    !> macro, for the "After N kbd macro iterations" error context.
    logical :: macro_started = .false.
    !> macros.c:executing_kbd_macro_iterations.
    integer(int64), public, protected :: executing_kbd_macro_iterations = 0
    !> Values of `defining_kbd_macro': nil, t and `append'.
    integer, parameter, public :: DEFINING_KBD_MACRO_NO = 0, DEFINING_KBD_MACRO_YES = 1, DEFINING_KBD_MACRO_APPEND = 2

    !> Not `DEFINING_KBD_MACRO_NO' while a keyboard macro is being defined.
    !> Don't set this!
    !> The value is `DEFINING_KBD_MACRO_APPEND' while appending to the
    !> definition of an existing macro.  Port of macros.c defining-kbd-macro
    !> (DEFINING_KBD_MACRO_NO for nil, DEFINING_KBD_MACRO_YES for t).
    integer, public :: defining_kbd_macro = DEFINING_KBD_MACRO_NO

    !> Last kbd macro defined, as an array of events; not allocated if none
    !> defined.  Port of macros.c last-kbd-macro (a vector: no string
    !> macros).
    type(value_type), allocatable, public :: last_kbd_macro(:)

    !> Normal hook run whenever a keyboard macro terminates.
    !> This is run whether the macro ends normally or prematurely due to
    !> an error.  Port of macros.c kbd-macro-termination-hook.
    type(hook_type), target, public :: kbd_macro_termination_hook = hook_type(name="kbd_macro_termination_hook")

    !> The last input event received.
    !> This is the last event read by any means, also from a keyboard
    !> macro.  Port of keyboard.c last-input-event.
    type(value_type), public :: last_input_event

    !> The keyboard macro being defined (struct kboard's kbd_macro_buffer):
    !> `kbd_macro_ptr' events stored, of which the first `kbd_macro_end'
    !> belong to it (the rest are the current command's).
    type(value_type), allocatable :: kbd_macro_buffer(:)
    integer(int64) :: kbd_macro_ptr = 0, kbd_macro_end = 0

    !> The depth of active minibuffers.  Port of minibuf.c:minibuf_level
    !> (here because keyboard.c reads it; minibuf_fm sets it and
    !> re-exports it).
    integer, public :: minibuf_level = 0

    !> True means any user interaction will signal an error.
    !> This variable can be bound when user interaction can't be performed,
    !> for instance when running a headless Emacs server.  Functions like
    !> `read_from_minibuffer' (and the like) will signal `inhibited_interaction'
    !> instead.  Port of minibuf.c inhibit-interaction (here because
    !> keyboard.c's `read_event' reads it too).
    logical, public :: inhibit_interaction = .false.

    !> Nonzero means echo unfinished commands after this many seconds of pause.
    !> The value may be integer or floating point.
    !> If the value is zero, don't echo at all.  Port of keyboard.c
    !> echo-keystrokes.
    real, public :: echo_keystrokes = 1.0

    !> If true, interpret upper case keys as lower case (when applicable).
    !> Emacs allows binding both upper and lower case key sequences to
    !> commands.  However, if there is a lower case key sequence bound to a
    !> command, and the user enters an upper case key sequence that is not
    !> bound to a command, Emacs will use the lower case binding.  Setting
    !> this variable to false inhibits this behavior.  Port of keyboard.c
    !> translate-upper-case-key-bindings.
    logical, public :: translate_upper_case_key_bindings = .true.

    !> keyboard.c:num_input_events, the number of input events read.
    integer(int64), public, protected :: num_input_events = 0
    !> Number of input events read from the keyboard so far.
    !> This does not include events generated by keyboard macros.  Port of
    !> keyboard.c num-nonmacro-input-events.
    integer(int64), public, protected :: num_nonmacro_input_events = 0

    !> The keyboard's echo state (struct kboard's echo_string, echo_prompt
    !> and immediate_echo): the text being echoed (unallocated for nil),
    !> the prompt it begins with (unallocated for nil), and whether
    !> keystrokes are echoed as they come.
    character(len=:), allocatable :: echo_string, echo_prompt
    logical :: immediate_echo = .false.
    !> keyboard.c:echo_kboard is the current keyboard: the echo area shows
    !> our echo.
    logical :: echo_kboard_current = .false.
    !> keyboard.c:ok_to_echo_at_next_pause is the current keyboard.
    logical :: ok_to_echo_at_next_pause = .false.

    !> Where the keys of the current key sequence start in
    !> `this_command_keys' (keyboard.c:this_single_command_key_start,
    !> 0-based).
    integer :: this_single_command_key_start = 0
    !> The events read for the current key sequence before any translation
    !> (keyboard.c:raw_keybuf and raw_keybuf_count).
    type(value_type), allocatable :: raw_keybuf(:)
    integer :: raw_keybuf_count = 0

    !> The descriptor of the dribble file (keyboard.c:dribble), -1 for
    !> none; `open_dribble_file' (fileio_fm) sets it.
    integer(int32), public :: dribble_fd = -1

    !> Character to recognize as meaning Help.
    !> When it is read, do `(eval help-form)', and display result if it's a string.
    !> If the value of `help_procedure' is nil, this char can be read normally.
    !> Port of keyboard.c help-char (a character).
    integer(int64), public :: help_char = 8
    !> List of input events to recognize as meaning Help.
    !> These work just like the value of `help_char' (see that).  Port of
    !> keyboard.c help-event-list (the events as `is_key_valid' strings;
    !> bindings.el's value is set by bindings_fm).
    character(len=16), allocatable, public :: help_event_list(:)
    !> Whether to append help text to echoed commands.
    !> When true, a reference to `C-h' is printed after echoed
    !> keystrokes.  Port of keyboard.c echo-keystrokes-help.
    logical, public :: echo_keystrokes_help = .true.
    !> simple.el's prefix-command--needs-update and prefix-command--last-echo:
    !> whether a prefix command changed the prefix state, and the last
    !> description of it (unallocated for nil).
    logical :: prefix_command_needs_update = .false.
    character(len=:), allocatable :: prefix_command_last_echo
    !> A throw to `top_level' is unwinding the recursive edits.
    logical :: top_level_pending = .false.

    !> Whether errors of the innermost command loop are handled there
    !> (command_loop_2 with handlers `error'): false in a keyboard macro's
    !> loop, where they end the macro.
    logical :: loop_handles_errors = .true.

    !> How long to display an echo-area message when the minibuffer is active.
    !> If the value is a number, it should be specified in seconds.
    !> Port of keyboard.c minibuffer-message-timeout (a non-number, no
    !> timeout, is not ported: Deferred).
    integer, public :: minibuffer_message_timeout = 2

    !> Set by `request_exit' to end `command_loop'.
    logical :: exit_requested = .false.
    !> keyboard.c:command_loop_level, the depth of recursive edits.
    integer, public, protected :: command_loop_level = 0
    !> A (throw 'exit VALUE) is pending; `exit_abort' for VALUE t.
    logical :: exit_pending = .false., exit_abort = .false.
    !> How many recursive edits the pending abort leaves
    !> (minibuffer-quit-recursive-edit's LEVELS).
    integer :: exit_levels = 1
    !> VALUE is this string: the recursive edit signals it as an error.
    character(len=:), allocatable :: exit_string
    !> overriding-terminal-local-map holds a transient map, to be dropped
    !> before the next command runs.
    logical :: transient_active = .false.
    ! The active transient map's KEEP-PRED (t), ON-EXIT and MESSAGE
    ! (unallocated for nil).
    logical :: transient_keep = .false.
    procedure(transient_exit_proc), pointer :: transient_on_exit => null()
    character(len=:), allocatable :: transient_message

    !> keyboard.c:struct keyremap.  `start' and `end' are 0-based offsets
    !> into the key buffer, as upstream.
    type :: keyremap_type
        type(value_type) :: parent, map
        integer :: start = 0, end = 0
    end type keyremap_type

    !> Execute MACRO as a sequence of events.
    !> If MACRO is a string or vector, then the events in it are executed
    !> exactly as if they had been input by the user.
    !>
    !> COUNT is a repeat count, or nil for once, or 0 for infinite loop.
    !>
    !> Optional third arg LOOPFUNC may be a function that is called prior to
    !> each iteration of the macro.  Iteration stops if LOOPFUNC returns
    !> false.
    !>
    !> The buffer shown in the currently selected window will be made the current
    !> buffer before the macro is executed.  Port of
    !> macros.c:Fexecute_kbd_macro (MACRO is a string, a list of events or
    !> an array of events; no symbols' function definitions).
    interface execute_kbd_macro
        ! Port of macros.c:Fexecute_kbd_macro.
        recursive module subroutine execute_kbd_macro_value(macro, count, status, loopfunc)
            implicit none(type, external)
            type(value_type), intent(in) :: macro, count
            type(status_type), intent(out) :: status
            procedure(kbd_macro_loop_proc), optional :: loopfunc
        end subroutine execute_kbd_macro_value

        ! Port of macros.c:Fexecute_kbd_macro.
        recursive module subroutine execute_kbd_macro_vector(macro, count, status, loopfunc)
            implicit none(type, external)
            type(value_type), intent(in) :: macro(:), count
            type(status_type), intent(out) :: status
            procedure(kbd_macro_loop_proc), optional :: loopfunc
        end subroutine execute_kbd_macro_vector
    end interface execute_kbd_macro

    interface
        !> Record subsequent keyboard input, defining a keyboard macro.
        !> The commands are recorded even as they are executed.
        !> Use \[end_kbd_macro] to finish recording and make the macro available.
        !> True APPEND (prefix arg) means append to last macro defined;
        !> this begins by re-executing that macro as if you typed it again.
        !> If optional second arg, NO_EXEC, is true, do not re-execute last
        !> macro before appending to it.  Port of macros.c:Fstart_kbd_macro.
        recursive module subroutine start_kbd_macro(append, no_exec, status)
            implicit none(type, external)
            logical, intent(in) :: append, no_exec
            type(status_type), intent(out) :: status
        end subroutine start_kbd_macro

        !> Finish defining a keyboard macro.
        !> The definition was started by \[start_kbd_macro].
        !> The macro is now available for use via \[call_last_kbd_macro].
        !>
        !> With numeric arg, repeat macro now that many times,
        !> counting the definition just completed as the first repetition.
        !> An argument of zero means repeat until error.
        !>
        !> Optional second arg LOOPFUNC may be a function that is called prior to
        !> each iteration of the macro.  Iteration stops if LOOPFUNC returns
        !> false.  Port of macros.c:Fend_kbd_macro (REPEAT is nil or a
        !> fixnum).
        recursive module subroutine end_kbd_macro(repeat, status, loopfunc)
            implicit none(type, external)
            type(value_type), intent(in) :: repeat
            type(status_type), intent(out) :: status
            procedure(kbd_macro_loop_proc), optional :: loopfunc
        end subroutine end_kbd_macro

        !> Call the last keyboard macro that you defined with \[start_kbd_macro].
        !>
        !> A prefix argument serves as a repeat count.  Zero means repeat until error.
        !>
        !> Optional second arg LOOPFUNC may be a function that is called prior to
        !> each iteration of the macro.  Iteration stops if LOOPFUNC returns
        !> false.  Port of macros.c:Fcall_last_kbd_macro.
        recursive module subroutine call_last_kbd_macro(prefix, status, loopfunc)
            implicit none(type, external)
            type(value_type), intent(in) :: prefix
            type(status_type), intent(out) :: status
            procedure(kbd_macro_loop_proc), optional :: loopfunc
        end subroutine call_last_kbd_macro

        !> Store character C into kbd macro being defined.  Port of
        !> macros.c:store_kbd_macro_char.
        module subroutine store_kbd_macro_char(c)
            implicit none(type, external)
            type(value_type), intent(in) :: c
        end subroutine store_kbd_macro_char

        !> Store EVENT into the keyboard macro being defined.  Port of
        !> macros.c:Fstore_kbd_macro_event.
        module subroutine store_kbd_macro_event(event)
            implicit none(type, external)
            type(value_type), intent(in) :: event
        end subroutine store_kbd_macro_event

        !> Declare that all chars stored so far in the kbd macro being defined
        !> really belong to it.  This is done in between editor commands.
        !> Port of macros.c:finalize_kbd_macro_chars.
        module subroutine finalize_kbd_macro_chars()
            implicit none(type, external)
        end subroutine finalize_kbd_macro_chars

        !> Cancel the events added to a keyboard macro for this command.
        !> Port of macros.c:Fcancel_kbd_macro_events.
        module subroutine cancel_kbd_macro_events()
            implicit none(type, external)
        end subroutine cancel_kbd_macro_events

        !> Finish defining the current keyboard macro.  Port of
        !> macros.c:end_kbd_macro.
        module subroutine finish_kbd_macro()
            implicit none(type, external)
        end subroutine finish_kbd_macro

        !> Register the commands of macros.c (syms_of_macros).
        !> Port of macros.c:syms_of_macros.
        module subroutine syms_of_macros()
            implicit none(type, external)
        end subroutine syms_of_macros
    end interface

contains

    !> Whether a keyboard macro is being defined (`defining_kbd_macro' is
    !> not nil).  Not a port.
    logical function is_defining_kbd_macro()
        is_defining_kbd_macro = defining_kbd_macro /= DEFINING_KBD_MACRO_NO
    end function is_defining_kbd_macro

    ! ------------------------------------------------------------------
    ! Input
    ! ------------------------------------------------------------------

    !> Port of keyboard.c:Fset_input_meta_mode (META: 0 nil, 1 t, 2
    !> encoded).
    subroutine set_input_meta_mode(mode)
        integer, intent(in) :: mode

        meta_key = mode
    end subroutine set_input_meta_mode

    !> The META element of `current_input_mode': 0 (nil: the 8th bit is
    !> stripped), 1 (t: it is Meta) or 2 (encoded: bytes go on to keyboard
    !> decoding).  Port of the META part of keyboard.c:Fcurrent_input_mode.
    integer function current_input_meta_mode()
        current_input_meta_mode = meta_key
    end function current_input_meta_mode

    !> Read keyboard input from `fd' (-1 for none).  Not a port.
    subroutine set_input_fd(fd)
        integer(int32), intent(in) :: fd

        input_fd = fd
    end subroutine set_input_fd

    !> Append `ev' to the event queue.  Port of keyboard.c:kbd_buffer_store_event.
    subroutine store_event(ev)
        type(value_type), intent(in) :: ev
        type(value_type), allocatable :: grown(:)
        integer(int64) :: n

        if (.not. allocated(events)) allocate (events(64))

        if (tail == size(events, kind=int64)) then
            n = tail - head + 1

            if (head > 1 .and. n < size(events, kind=int64)/2) then
                ! Compact in place.
                events(1:n) = events(head:tail)
            else
                allocate (grown(2*size(events, kind=int64)))
                grown(1:n) = events(head:tail)
                call move_alloc(grown, events)
            end if

            head = 1
            tail = n
        end if

        tail = tail + 1
        events(tail) = ev
    end subroutine store_event

    !> Store character event `c'.  Not a port.
    subroutine store_char(c)
        integer(int64), intent(in) :: c

        call store_event(make_fixnum(c))
    end subroutine store_char

    !> Flush incomplete UTF-8 bytes as raw-byte characters.  Not a port.
    subroutine flush_pending()
        integer(int64) :: k

        if (.not. allocated(pending_bytes)) return

        do k = 1, len(pending_bytes, kind=int64)
            call store_char(int(byte8_to_char(ichar(pending_bytes(k:k), int32)), int64))
        end do

        deallocate (pending_bytes)
    end subroutine flush_pending

    !> Decode byte `b' with the keyboard coding system (UTF-8): complete
    !> characters become events, invalid bytes raw-byte characters.  Port of
    !> keyboard.c:read_decoded_event_from_main_queue (the decoding part).
    subroutine decode_byte(b)
        integer(int32), intent(in) :: b
        integer(int32) :: need, ch, length

        if (allocated(pending_bytes)) then
            if (iand(b, int(z'C0', int32)) /= int(z'80', int32)) then
                ! Not a trailing byte: the pending sequence is invalid.
                call flush_pending()
            end if
        end if

        if (.not. allocated(pending_bytes)) then
            if (b < 128) then
                call store_char(int(b, int64))
                return
            end if

            if (iand(b, int(z'C0', int32)) == int(z'80', int32)) then
                call store_char(int(byte8_to_char(b), int64))
                return
            end if

            pending_bytes = char(b)
            return
        end if

        pending_bytes = pending_bytes//char(b)
        need = bytes_by_char_head(ichar(pending_bytes(1:1), int32))
        if (len(pending_bytes) < need) return

        if (multibyte_length(pending_bytes, 1_int64, .false.) == need) then
            call string_char_and_length(pending_bytes, 1_int64, ch, length)
            call store_char(int(ch, int64))
            deallocate (pending_bytes)
        else
            call flush_pending()
        end if
    end subroutine decode_byte

    ! Decode the byte B of keyboard input with the keyboard coding system
    ! (`set_keyboard_coding_system').  Port of
    ! keyboard.c:read_decoded_event_from_main_queue, in part.
    subroutine decode_keyboard_byte(b)
        integer(int32), intent(in) :: b
        character(len=:), allocatable :: coding, text
        type(status_type) :: st
        integer(int64) :: k
        integer(int32) :: ch, length

        coding = keyboard_coding_system()

        select case (coding_system_type(coding))
        case ("utf-8", "undecided")
            call decode_byte(b)
        case ("raw-text")
            ! No decoding needed.
            call store_char(int(b, int64))
        case default
            ! An encoded byte sequence, let's try to decode it.
            if (allocated(pending_encoded)) then
                pending_encoded = pending_encoded//char(b)
            else
                pending_encoded = char(b)
            end if

            call decode_coding_string(pending_encoded, coding, text, st)

            if (.not. st%ok() .or. len(text) == 0) then
                ! The encoded sequence is incomplete: read on, unless it is
                ! too long (MAX_ENCODED_BYTES).
                if (len(pending_encoded) < 16) return
                text = ""

                do k = 1, len(pending_encoded, kind=int64)
                    call store_char(int(ichar(pending_encoded(k:k), int32), int64))
                end do
            end if

            k = 1

            do while (k <= len(text, kind=int64))
                call string_char_and_length(text, k, ch, length)
                call store_char(int(ch, int64))
                k = k + length
            end do

            deallocate (pending_encoded)
        end select
    end subroutine decode_keyboard_byte

    !> Turn terminal input `bytes' into events.  Port of
    !> keyboard.c:tty_read_avail_input (the per-byte part).
    subroutine kbd_store_bytes(bytes)
        character(len=*), intent(in) :: bytes
        integer(int64) :: k
        integer(int32) :: b

        do k = 1, len(bytes, kind=int64)
            b = ichar(bytes(k:k), int32)

            select case (meta_key)
            case (1)
                if (b >= 128) then
                    call store_char(M_META + b - 128)
                else
                    call store_char(int(b, int64))
                end if
            case (2)
                call decode_keyboard_byte(b)
            case default
                call store_char(int(iand(b, 127_int32), int64))
            end select
        end do
    end subroutine kbd_store_bytes

    !> Read whatever input the terminal has, without waiting; `nread' is
    !> the number of bytes (0 at end of file).  Port of
    !> keyboard.c:tty_read_avail_input (with keyboard.c:gobble_input, which
    !> reads what one terminal has).
    subroutine tty_read_avail_input(nread, status)
        integer(int64), intent(out) :: nread
        type(status_type), intent(out) :: status
        character(len=4096) :: buf

        nread = 0
        if (input_fd < 0) return
        if (.not. input_available(input_fd, 0_int32)) return
        call emacs_read(input_fd, buf, nread, status)
        if (.not. status%ok()) return
        if (nread > 0) call kbd_store_bytes(buf(1:nread))
    end subroutine tty_read_avail_input

    !> Push `evs' to be read before any other input, in order.
    !> Like setting keyboard.c:Vunread_command_events.  Not a port.
    subroutine unread_command_events(evs)
        type(value_type), intent(in) :: evs(:)
        type(value_type), allocatable :: merged(:)
        integer(int64) :: n

        n = size(evs, kind=int64)
        allocate (merged(n + nunread))
        merged(1:n) = evs
        if (nunread > 0) merged(n + 1:) = unread(1:nunread)
        call move_alloc(merged, unread)
        nunread = nunread + n
    end subroutine unread_command_events

    !> Discard the contents of the terminal input buffer.
    !> Also end any kbd macro being defined.  Port of
    !> keyboard.c:Fdiscard_input.
    subroutine discard_input()
        if (defining_kbd_macro /= DEFINING_KBD_MACRO_NO) then
            ! Discard the last command from the macro.
            call cancel_kbd_macro_events()
            call finish_kbd_macro()
        end if

        nunread = 0
        if (allocated(unread)) deallocate (unread)
        head = 1
        tail = 0
        if (allocated(events)) deallocate (events)
        if (allocated(pending_bytes)) deallocate (pending_bytes)
    end subroutine discard_input

    !> Whether `unread_command_events' holds events (the variable is
    !> non-nil).  Not a port.
    logical function has_unread_command_events()
        has_unread_command_events = nunread > 0
    end function has_unread_command_events

    !> Whether input is waiting: unread events, queued events or bytes
    !> from the terminal.  Port of keyboard.c:Finput_pending_p with
    !> keyboard.c:get_input_pending, keyboard.c:readable_events,
    !> keyboard.c:detect_input_pending, keyboard.c:requeued_events_pending_p,
    !> keyboard.c:requeued_command_events_pending_p and
    !> keyboard.c:kbd_buffer_events_waiting (no input methods).
    logical function is_input_pending()
        is_input_pending = nunread > 0 .or. tail >= head
        if (is_input_pending .or. input_fd < 0) return
        is_input_pending = input_available(input_fd, 0_int32)
    end function is_input_pending

    !> The next input event.  Waits for terminal input if there is a
    !> terminal; otherwise `have' is false when the queue is empty (end of
    !> input).  The event becomes `last_input_event' and is recorded
    !> (`record_char'); an event from the terminal wipes the echo area,
    !> and one not from `unread_command_events' (or the first of a
    !> command) goes into `this_command_keys' and the echo.  With SECONDS,
    !> wait at most that long (also without a terminal), handling special
    !> events, timers and subprocess output meanwhile; `have' is then
    !> false on a timeout (read-event's SECONDS), and the event is not
    !> added to `this_command_keys'.  Before waiting, keystrokes are
    !> echoed if `echo_keystrokes' seconds pass in the middle of a key
    !> sequence.  Port of keyboard.c:read_char (its core).
    recursive subroutine read_input_event(ev, have, status, seconds)
        type(value_type), intent(out) :: ev
        logical, intent(out) :: have
        type(status_type), intent(out) :: status
        real, intent(in), optional :: seconds
        logical :: reread, from_macro

        call read_event_1(ev, have, status, seconds, reread, from_macro)

        if (.not. have) then
            ! Input ended (no terminal): no longer waiting.
            if (.not. present(seconds)) call timer_stop_idle()
            return
        end if

        ! Now that we have read an event, Emacs is not idle.
        call timer_stop_idle()
        ! Store these characters into recent_keys, the dribble file if
        ! any, and the keyboard macro being defined, if any.
        call record_char(ev)

        ! Now wipe the echo area (clear_message, which also runs
        ! clear-message-function).
        if (.not. reread .and. .not. from_macro) then
            if (echo_area_busy()) last_message = ""
            if (allocated(set_minibuffer_message_text)) deallocate (set_minibuffer_message_text)
        end if

        if ((.not. reread .or. size_of_command_keys() == 0) .and. .not. present(seconds)) then
            ! Once we reread a character, echoing can happen the next time
            ! we pause to read a new one.
            ok_to_echo_at_next_pause = .true.
            ! Record this character as part of the current key.
            call add_command_key(ev)
            call echo_update()
        end if

        last_input_event = ev
        num_input_events = num_input_events + 1
    end subroutine read_input_event

    !> The number of events in `this_command_keys'
    !> (keyboard.c:this_command_key_count).  Not a port.
    integer function size_of_command_keys() result(n)
        n = 0
        if (allocated(this_command_keys)) n = size(this_command_keys)
    end function size_of_command_keys

    !> Record the input event C in various ways: the ring of recent keys,
    !> the dribble file and the keyboard macro being defined, unless it
    !> comes from a keyboard macro being executed.  Port of
    !> keyboard.c:record_char (no help-echo or mouse-movement events).
    subroutine record_char(c)
        type(value_type), intent(in) :: c

        if (macro_active) return
        ! recent_keys should not include events from keyboard macros.
        call record_recent(c, .false.)
        num_nonmacro_input_events = num_nonmacro_input_events + 1
        ! Write c to the dribble file.
        call record_dribble(c)
        call store_kbd_macro_char(c)
    end subroutine record_char

    !> Write event C to the dribble file, if one is open: a character
    !> below 256 as its byte, a larger one as " 0xHEX", a symbol (or the
    !> head of a composite event) as <NAME>.  Port of keyboard.c:record_char
    !> (the dribble part).
    subroutine record_dribble(c)
        type(value_type), intent(in) :: c
        type(value_type) :: head
        type(status_type) :: st
        character(len=24) :: buf

        if (dribble_fd < 0) return

        if (fixnump(c)) then
            if (xfixnum(c) >= 0 .and. xfixnum(c) < 256) then
                call emacs_write(dribble_fd, achar(xfixnum(c)), st)
            else
                write (buf, "(z0)") xfixnum(c)
                call emacs_write(dribble_fd, " 0x"//to_lower(trim(buf)), st)
            end if
        else
            ! If it's a structured event, take the event header.
            head = c
            if (vtag(c) == V_LIST) head = list_elt(c, 1_int64)
            if (symbolp(head)) call emacs_write(dribble_fd, "<"//symbols(xsymbol(head))%name//">", st)
        end if

    contains

        ! The hexadecimal digits in lower case.
        function to_lower(t) result(r)
            character(len=*), intent(in) :: t
            character(len=len(t)) :: r
            integer :: i

            r = t

            do i = 1, len(t)
                if (t(i:i) >= "A" .and. t(i:i) <= "F") r(i:i) = achar(iachar(t(i:i)) + 32)
            end do
        end function to_lower
    end subroutine record_dribble

    !> Store `ev' (an event, or the command run if `command') into the
    !> ring of recent keys.  Port of keyboard.c:record_char (the
    !> recent_keys part; no help-echo or mouse-movement events) and
    !> keyboard.c:command_loop_1 in part.
    subroutine record_recent(ev, command)
        type(value_type), intent(in) :: ev
        logical, intent(in) :: command

        if (.not. allocated(recent_events)) then
            allocate (recent_events(lossage_limit), recent_is_command(lossage_limit))
            recent_is_command = .false.
        end if

        if (total_keys < lossage_limit) total_keys = total_keys + 1
        recent_events(recent_keys_index) = ev
        recent_is_command(recent_keys_index) = command
        recent_keys_index = recent_keys_index + 1
        if (recent_keys_index > lossage_limit) recent_keys_index = 1
    end subroutine record_recent

    !> Return the last few events, not counting those from keyboard macros.
    !> If INCLUDE_CMDS is true, include the commands that were run, as
    !> pseudo-events: `is_command' marks them, their command (.false.
    !> for none) being the event.  Port of keyboard.c:Frecent_keys.
    subroutine recent_keys(include_cmds, keys, is_command)
        logical, intent(in) :: include_cmds
        type(value_type), allocatable, intent(out) :: keys(:)
        logical, allocatable, intent(out) :: is_command(:)
        integer :: i, k, n, first

        if (total_keys < lossage_limit) then
            first = 1
        else
            first = recent_keys_index
        end if

        n = total_keys
        if (.not. include_cmds) n = total_keys - count(recent_is_command(1:total_keys))
        allocate (keys(n), is_command(n))
        n = 0
        i = first

        do k = 1, total_keys
            if (include_cmds .or. .not. recent_is_command(i)) then
                n = n + 1
                keys(n) = recent_events(i)
                is_command(n) = recent_is_command(i)
            end if

            i = i + 1
            if (i > lossage_limit) i = 1
        end do
    end subroutine recent_keys

    !> The next input event (read_input_event without recording it):
    !> `reread' tells it came from `unread_command_events', `from_macro'
    !> from the keyboard macro being executed.  Port of
    !> keyboard.c:kbd_buffer_get_event and
    !> keyboard.c:read_event_from_main_queue (with keyboard.c:next_kbd_event
    !> and keyboard.c:clear_event: the queue's head advances, its slot
    !> cleared) and of keyboard.c:read_char's reading part.
    recursive subroutine read_event_1(ev, have, status, seconds, reread, from_macro)
        type(value_type), intent(out) :: ev
        logical, intent(out) :: have
        type(status_type), intent(out) :: status
        real, intent(in), optional :: seconds
        logical, intent(out) :: reread, from_macro
        integer(int64) :: nread, start, now, rate
        integer(int32) :: timeout
        logical :: echoed

        have = .false.
        reread = .false.
        from_macro = .false.
        echoed = .false.
        call system_clock(start, rate)

        reading: do
            call run_special_events()

            if (nunread > 0) then
                ev = unread(1)
                unread(1:nunread - 1) = unread(2:nunread)
                unread(nunread) = nil()
                nunread = nunread - 1
                have = .true.
                reread = .true.
                return
            end if

            ! If we're executing a macro, process it unless we are at its
            ! end.
            if (macro_active) then
                if (.not. is_at_end_of_macro()) then
                    executing_kbd_macro_index = executing_kbd_macro_index + 1
                    ev = macro_events(executing_kbd_macro_index)

                    if (macro_string .and. fixnump(ev)) then
                        if (iand(xfixnum(ev), 128_int64) /= 0 .and. xfixnum(ev) <= 255) then
                            ev = make_fixnum(ior(M_META, iand(xfixnum(ev), not(128_int64))))
                        end if
                    end if

                    have = .true.
                    from_macro = .true.
                    return
                end if
            end if

            if (.not. echoed) then
                echoed = .true.
                ! Start idle timers if no time limit is supplied.  We don't
                ! do it if a time limit is supplied to avoid an infinite
                ! recursion in the situation where an idle timer calls
                ! `sit_for'.
                if (.not. present(seconds)) call timer_start_idle()
                call echo_before_reading(present(seconds))
                if (nunread > 0) cycle reading
            end if

            if (tail >= head) then
                ev = events(head)
                events(head) = nil()
                head = head + 1

                if (head > tail) then
                    head = 1
                    tail = 0
                end if

                have = .true.
                return
            end if

            if (input_fd < 0) then
                if (.not. present(seconds)) return
                ! Wait for the other descriptors and the timers only.
                if (time_left(timeout) < 0) return
                if (wait_for_input(timeout)) cycle reading
                cycle reading
            end if

            ! C-g on the terminal arrives as SIGINT; while waiting for
            ! input it reads as the quit character
            ! (keyboard.c:quit_throw_to_read_char, as handle_interrupt
            ! calls it).
            if (interrupt_pending()) then
                ev = make_fixnum(7_int64)
                have = .true.
                return
            end if

            ! Nothing typed yet: show the screen before waiting (read_char's
            ! redisplay).
            if (associated(redisplay_hook)) then
                if (.not. input_available(input_fd, 0_int32)) call redisplay_hook()
            end if

            ! Emacs is idle while it waits for input; the timers run while
            ! waiting, and the wait ends when the next one is due
            ! (read_char's timer_start_idle, wait_reading_process_output's
            ! timer_check).
            call timer_start_idle()

            waiting: do
                if (present(seconds)) then
                    if (time_left(timeout) < 0) return
                    if (wait_for_input(timeout)) exit waiting
                else
                    if (wait_for_input(run_timers())) exit waiting
                end if

                ! A timer or subprocess may have queued events, or C-g came.
                if (nunread > 0 .or. tail >= head) cycle reading

                if (interrupt_pending()) then
                    ev = make_fixnum(7_int64)
                    have = .true.
                    return
                end if
            end do waiting

            call tty_read_avail_input(nread, status)
            if (.not. status%ok()) return
            ! End of file: the terminal went away.
            if (nread == 0) return
        end do reading

    contains

        !> The milliseconds left until SECONDS have passed (negative when
        !> they have), and in MS how long to wait: until then or until
        !> the next timer, after running the ripe timers.
        recursive integer(int64) function time_left(ms) result(left)
            integer(int32), intent(out) :: ms
            integer(int32) :: next_timer

            call system_clock(now)
            left = int(seconds*1000, int64) - (now - start)*1000/max(rate, 1_int64)
            ms = int(max(left, 0_int64), int32)
            next_timer = run_timers()
            if (next_timer >= 0) ms = min(ms, next_timer)
        end function time_left
    end subroutine read_event_1

    ! ------------------------------------------------------------------
    ! Key sequences
    ! ------------------------------------------------------------------

    ! Not a port.
    function map_value(km) result(v)
        type(handle_type), intent(in) :: km
        type(value_type) :: v

        if (is_keymap_live(km)) v = make_keymap_ref(km)
    end function map_value

    !> Whether BINDING is undefined: nil, `undefined', or a command
    !> remapped to `undefined'.  Port of keyboard.c:test_undefined.
    logical function test_undefined(binding)
        type(value_type), intent(in) :: binding
        type(value_type) :: remapped

        test_undefined = nilp(binding)

        if (.not. test_undefined .and. symbolp(binding)) then
            test_undefined = xsymbol(binding) == intern("undefined")

            if (.not. test_undefined) then
                call command_remapping(binding, remapped)
                if (symbolp(remapped)) test_undefined = xsymbol(remapped) == intern("undefined")
            end if
        end if
    end function test_undefined

    !> Events of translation `next' (a list of events or a string).
    !> Not a port.
    subroutine sequence_events(next, seq)
        type(value_type), intent(in) :: next
        type(value_type), allocatable, intent(out) :: seq(:)
        character(len=:), allocatable :: s
        integer(int64) :: pos, n
        integer(int32) :: ch, length

        if (vtag(next) == V_LIST) then
            call list_elements(next, seq)
            return
        end if

        s = xstring(next)
        allocate (seq(len(s)))
        n = 0
        pos = 1

        do while (pos <= len(s, kind=int64))
            call string_char_and_length(s, pos, ch, length)
            n = n + 1
            seq(n) = make_fixnum(int(ch, int64))
            pos = pos + length
        end do

        seq = seq(1:n)
    end subroutine sequence_events

    !> One step of key remapping: extend `fk''s scanned key by one event
    !> and, if the map binds it to a key sequence and `doit', replace the
    !> scanned events with it (`done'; `diff' is the change in length).
    !> `input' is the number of events in `keybuf'.  Port of
    !> keyboard.c:keyremap_step with keyboard.c:access_keymap_keyremap
    !> (translation functions are not supported).
    subroutine keyremap_step(keybuf, fk, input, doit, diff, done, status)
        type(value_type), intent(inout) :: keybuf(:)
        type(keyremap_type), intent(inout) :: fk
        integer, intent(in) :: input
        logical, intent(in) :: doit
        integer, intent(out) :: diff
        logical, intent(out) :: done
        type(status_type), intent(out) :: status
        type(value_type) :: key, next
        type(value_type), allocatable :: seq(:)
        integer :: i, n

        done = .false.
        diff = 0
        key = keybuf(fk%end + 1)
        fk%end = fk%end + 1
        if (is_keymap(fk%parent)) next = access_keymap(fk%map, key, .true., .false.)

        if (doit .and. (stringp(next) .or. (vtag(next) == V_LIST .and. &
                                            .not. is_keymap(next)))) then
            call sequence_events(next, seq)
            n = size(seq)
            diff = n - (fk%end - fk%start)

            if (READ_KEY_ELTS - input <= diff) then
                call set_error(status, FM_GENERIC_ERROR, "Key sequence too long")
                return
            end if

            if (diff < 0) then
                do i = fk%end, input - 1
                    keybuf(i + diff + 1) = keybuf(i + 1)
                end do
            else if (diff > 0) then
                do i = input - 1, fk%end, -1
                    keybuf(i + diff + 1) = keybuf(i + 1)
                end do
            end if

            keybuf(fk%start + 1:fk%start + n) = seq
            fk%end = fk%end + diff
            fk%start = fk%end
            fk%map = fk%parent
            done = .true.
            return
        end if

        if (is_keymap(next)) then
            fk%map = next
        else
            fk%start = fk%start + 1
            fk%end = fk%start
            fk%map = fk%parent
        end if
    end subroutine keyremap_step

    ! Not a port.
    subroutine reset_remap(fk, pos)
        type(keyremap_type), intent(inout) :: fk
        integer, intent(in) :: pos

        fk%start = pos
        fk%end = pos
        fk%map = fk%parent
    end subroutine reset_remap

    !> Read a key sequence: events until the active keymaps give a
    !> non-prefix binding (or none), with terminal escape sequences and
    !> other translations applied.  `keys' is the sequence and `binding'
    !> its binding (nil if undefined).  `eof' is set when input ended;
    !> `keys' then holds what was read.  PROMPT begins the echo of the
    !> keys; with DONT_DOWNCASE_LAST, an upper-case last event that was
    !> looked up in lower case is returned as it was typed.  The events
    !> read go into `this_command_keys' (the first of them at
    !> `this_single_command_key_start') and, untranslated, into the raw
    !> key buffer.  Port of keyboard.c:read_key_sequence (no mouse or
    !> frame events, input methods or translation functions) with
    !> keyboard.c:follow_key.
    recursive subroutine read_key_sequence(keys, binding, eof, status, prompt, dont_downcase_last)
        type(value_type), allocatable, intent(out) :: keys(:)
        type(value_type), intent(out) :: binding
        logical, intent(out) :: eof
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: prompt
        logical, intent(in), optional :: dont_downcase_last
        type(value_type) :: keybuf(READ_KEY_ELTS)
        type(value_type) :: current_binding, new_binding, key, &
                            original_uppercase
        type(keyremap_type) :: indec, fkey, keytran
        character(len=:), allocatable :: base
        integer :: t, mock_input, first_unbound, diff, i, &
                   original_uppercase_position, echo_start, keys_start
        integer(int64) :: k, c, bits, dc
        logical :: have, done, shift_translated, macro_end, keep_case

        eof = .false.
        t = 0
        mock_input = 0
        original_uppercase_position = -1
        shift_translated = .false.
        keep_case = .false.
        if (present(dont_downcase_last)) keep_case = dont_downcase_last

        if (present(prompt)) then
            ! Install the string PROMPT as the beginning of the string of
            ! echoing, so that it serves as a prompt for the next
            ! character.
            echo_prompt = prompt
            immediate_echo = .false.
            call echo_now()
            if (.not. is_echo_keystrokes()) immediate_echo = .false.
        end if

        ! Record the initial state of the echo area and this_command_keys;
        ! we will need to restore them if we replay a key sequence.
        echo_start = echo_length()
        keys_start = size_of_command_keys()
        this_single_command_key_start = keys_start
        indec%parent = map_value(input_decode_map)
        fkey%parent = map_value(local_function_key_map)
        keytran%parent = map_value(key_translation_map)
        call reset_remap(indec, 0)
        call reset_remap(fkey, 0)
        call reset_remap(keytran, 0)

        replay: do
            first_unbound = READ_KEY_ELTS + 1
            ! keyboard.c:active_maps (no mouse positions)
            current_binding = current_active_maps()
            t = 0
            ! These are no-ops the first time through, but if we restart,
            ! they revert the echo area and this_command_keys to their
            ! original state.
            call set_command_key_count(keys_start)
            if (t < mock_input) call echo_truncate(echo_start)

            keyloop: do
                if (nilp(current_binding)) then
                    if (keytran%start >= t) exit replay
                else if (.not. is_keymap(current_binding)) then
                    exit replay
                end if

                if (first_unbound < keytran%start) then
                    ! The prefix up to first_unbound is unbound and has no
                    ! translation left: drop it and start over.
                    do i = first_unbound + 1, t - 1
                        keybuf(i - first_unbound) = keybuf(i + 1)
                    end do

                    mock_input = t - first_unbound - 1
                    call reset_remap(indec, indec%start - (first_unbound + 1))
                    call reset_remap(fkey, fkey%start - (first_unbound + 1))
                    call reset_remap(keytran, keytran%start - (first_unbound + 1))
                    cycle replay
                end if

                if (t >= READ_KEY_ELTS) then
                    call set_error(status, FM_GENERIC_ERROR, "Key sequence too long")
                    return
                end if

                macro_end = .false.
                if (macro_active .and. nunread == 0) macro_end = is_at_end_of_macro()

                if (t < mock_input) then
                    key = keybuf(t + 1)
                    call add_command_key(key)

                    if (immediate_echo) then
                        ! Set immediate_echo to false so as to force echo_now
                        ! to redisplay (it will set immediate_echo right back
                        ! to true).
                        immediate_echo = .false.
                        call echo_now()
                    end if
                else if (macro_end) then
                    ! If we're at the end of a macro, exit it by returning
                    ! 0, unless there are unread events pending.
                    t = 0
                    eof = .true.
                    exit replay
                else
                    call read_input_event(key, have, status)
                    if (.not. status%ok()) return

                    if (.not. have) then
                        eof = .true.
                        exit replay
                    end if

                    call add_raw_key(key)
                end if

                ! keyboard.c:follow_key
                new_binding = access_keymap(current_binding, key, .true., .false.)

                if (.not. nilp(new_binding)) then
                    first_unbound = max(t + 1, first_unbound)
                else
                    first_unbound = min(t, first_unbound)
                end if

                current_binding = new_binding
                keybuf(t + 1) = key
                t = t + 1
                ! Record what part of this_command_keys is the current key
                ! sequence.
                this_single_command_key_start = max(size_of_command_keys() - t, 0)

                ! input-decode-map: applies wherever it matches.
                do while (indec%end < t)
                    call keyremap_step(keybuf, indec, max(t, mock_input), .true., &
                                       diff, done, status)
                    if (.not. status%ok()) return

                    if (done) then
                        mock_input = diff + max(t, mock_input)
                        cycle replay
                    end if
                end do

                ! local-function-key-map: only for undefined sequences.
                if (.not. is_keymap(current_binding) .and. &
                    .not. test_undefined(current_binding) .and. indec%start >= t) then
                    if (fkey%start < t) call reset_remap(fkey, t)
                else
                    do while (fkey%end < indec%start)
                        call keyremap_step(keybuf, fkey, max(t, mock_input), &
                                           fkey%end + 1 == t .and. &
                                           test_undefined(current_binding), &
                                           diff, done, status)
                        if (.not. status%ok()) return

                        if (done) then
                            mock_input = diff + max(t, mock_input)
                            indec%end = indec%end + diff
                            indec%start = indec%start + diff
                            cycle replay
                        end if
                    end do
                end if

                ! key-translation-map.
                do while (keytran%end < fkey%start)
                    call keyremap_step(keybuf, keytran, max(t, mock_input), .true., &
                                       diff, done, status)
                    if (.not. status%ok()) return

                    if (done) then
                        mock_input = diff + max(t, mock_input)
                        indec%end = indec%end + diff
                        indec%start = indec%start + diff
                        fkey%end = fkey%end + diff
                        fkey%start = fkey%start + diff
                        cycle replay
                    end if
                end do

                ! If KEY is not defined in any of the keymaps, and cannot be
                ! part of a function key or translation, and is an upper case
                ! letter, use the corresponding lower-case letter instead.
                if (nilp(current_binding) .and. keytran%start >= t .and. fixnump(key) .and. &
                    translate_upper_case_key_bindings) then
                    k = xfixnum(key)
                    c = iand(k, not(M_MASK))
                    dc = -1

                    if (iand(k, M_SHIFT) /= 0) then
                        dc = k - M_SHIFT
                    else if (c >= 0 .and. c <= MAX_CHAR) then
                        if (downcase(int(c, int32)) /= c) dc = ior(int(downcase(int(c, int32)), int64), iand(k, M_MASK))
                    end if

                    if (dc >= 0) then
                        original_uppercase = key
                        original_uppercase_position = t - 1
                        ! We have to do this unconditionally, regardless of
                        ! whether the lower-case char is defined in the
                        ! keymaps, because they might get translated through
                        ! function-key-map.
                        keybuf(t) = make_fixnum(dc)
                        mock_input = max(t, mock_input)
                        shift_translated = .true.
                        cycle replay
                    end if
                end if

                ! If KEY is not defined in any of the keymaps, and cannot be
                ! part of a function key or translation, and is a shifted
                ! function key, use the corresponding unshifted function key
                ! instead.
                if (nilp(current_binding) .and. keytran%start >= t .and. translate_upper_case_key_bindings) then
                    bits = 0
                    if (symbolp(key)) call parse_modifiers(key, bits, base)
                    if (fixnump(key)) bits = iand(xfixnum(key), M_MASK)
                    c = -1
                    if (fixnump(key)) c = iand(xfixnum(key), not(M_MASK))

                    ! Treat uppercase keys as shifted.
                    if (iand(bits, M_SHIFT) /= 0 .or. uppercase_key(c)) then
                        original_uppercase = key
                        original_uppercase_position = t - 1

                        if (iand(bits, M_SHIFT) /= 0 .and. symbolp(key)) then
                            keybuf(t) = apply_modifiers(bits - M_SHIFT, base)
                        else if (iand(bits, M_SHIFT) /= 0) then
                            keybuf(t) = make_fixnum(xfixnum(key) - M_SHIFT)
                        else
                            keybuf(t) = make_fixnum(ior(int(downcase(int(c, int32)), int64), bits))
                        end if

                        mock_input = max(t, mock_input)
                        ! Reset fkey (and consequently keytran) to apply
                        ! function-key-map on the result, so that
                        ! S-backspace is correctly mapped to DEL (via
                        ! backspace).  OTOH, input-decode-map doesn't need
                        ! to go through it again.
                        call reset_remap(fkey, 0)
                        call reset_remap(keytran, 0)
                        shift_translated = .true.
                        cycle replay
                    end if
                end if
            end do keyloop
        end do replay

        ! Don't downcase the last character if the caller says don't.
        ! Don't downcase it if the result is undefined, either.
        if ((keep_case .or. nilp(current_binding)) .and. t > 0 .and. &
            t - 1 == original_uppercase_position) then
            keybuf(t) = original_uppercase
            shift_translated = .false.
        end if

        if (shift_translated) this_command_keys_shift_translated = .true.
        binding = current_binding
        if (is_keymap(binding)) binding = nil()

        ! Occasionally we fabricate events, perhaps by expanding something
        ! according to function-key-map.  In this case, return the entire
        ! generated key sequence, even if we hit an unbound prefix or a
        ! definition before the end.
        do while (t < mock_input .and. .not. eof)
            call add_command_key(keybuf(t + 1))
            t = t + 1
        end do

        call echo_update()
        allocate (keys(t))
        keys = keybuf(1:t)
        if (.not. eof) num_input_keys = num_input_keys + 1

    contains

        !> Whether character C (-1 for none) is an upper-case letter.
        logical function uppercase_key(c)
            integer(int64), intent(in) :: c

            uppercase_key = .false.
            if (c < 0 .or. c > MAX_CHAR) return
            uppercase_key = is_uppercase(int(c, int32))
        end function uppercase_key
    end subroutine read_key_sequence

    !> Add KEY to the raw key buffer (keyboard.c's GROW_RAW_KEYBUF and
    !> raw_keybuf).  Not a port.
    subroutine add_raw_key(key)
        type(value_type), intent(in) :: key
        type(value_type), allocatable :: grown(:)

        if (.not. allocated(raw_keybuf)) allocate (raw_keybuf(READ_KEY_ELTS))

        if (raw_keybuf_count == size(raw_keybuf)) then
            allocate (grown(2*size(raw_keybuf)))
            grown(1:raw_keybuf_count) = raw_keybuf
            call move_alloc(grown, raw_keybuf)
        end if

        raw_keybuf_count = raw_keybuf_count + 1
        raw_keybuf(raw_keybuf_count) = key
    end subroutine add_raw_key

    ! ------------------------------------------------------------------
    ! The command loop
    ! ------------------------------------------------------------------

    !> Set MAP as a temporary keymap taking precedence over other keymaps.
    !> Normally, MAP is used only once, to look up the very next key.
    !> However, if the optional argument KEEP_PRED is true, MAP stays
    !> active if a key from MAP is used.
    !>
    !> Optional arg ON_EXIT, if present, specifies a procedure that is
    !> called, with no arguments, after MAP is deactivated.
    !>
    !> Optional arg MESSAGE, if present, requests display of an informative
    !> message after activating the transient map: the %k specifier in the
    !> string is replaced with the list of keys from the transient map.
    !> Upon deactivating the map, the displayed message will be cleared
    !> out.  Port of subr.el:set-transient-map (no KEEP-PRED function,
    !> TIMEOUT or nested transient maps); like upstream, the map is
    !> dropped from the pre-command hook.
    subroutine set_transient_map(km, keep_pred, on_exit, message)
        type(handle_type), intent(in) :: km
        logical, intent(in), optional :: keep_pred
        procedure(transient_exit_proc), optional :: on_exit
        character(len=*), intent(in), optional :: message
        type(value_type), allocatable :: events(:), key(:)
        character(len=:), allocatable :: keys
        integer :: k, at

        ! internal-push-keymap: the old map stays active below.
        if (.not. transient_active) pushed_terminal_local_map = overriding_terminal_local_map
        overriding_terminal_local_map = km
        transient_active = .true.
        transient_keep = .false.
        if (present(keep_pred)) transient_keep = keep_pred
        transient_on_exit => null()
        if (present(on_exit)) transient_on_exit => on_exit
        if (allocated(transient_message)) deallocate (transient_message)
        if (.not. present(message)) return
        call keymap_events(km, events)
        keys = ""

        do k = 1, size(events)
            key = [events(k)]
            if (k > 1) keys = keys//", "
            keys = keys//key_description(key)
        end do

        ! format-spec with %k.
        transient_message = message
        at = index(transient_message, "%k")
        if (at > 0) transient_message = transient_message(1:at - 1)//keys//transient_message(at + 2:)
        last_message = transient_message
    end subroutine set_transient_map

    !> Deactivate transient map KM if it is still the active one: the
    !> exitfun that subr.el:set-transient-map returns.
    !> Port of subr.el:set-transient-map in part.
    subroutine exit_transient_map(km)
        type(handle_type), intent(in) :: km
        type(value_type) :: none(0)

        if (.not. transient_active) return
        if (overriding_terminal_local_map%index /= km%index .or. &
            overriding_terminal_local_map%generation /= km%generation) return
        transient_keep = .false.
        call clear_transient_map(none)
    end subroutine exit_transient_map

    !> The pre-command-hook function of the active transient map: keep it
    !> (repeating its message) when KEEP_PRED is t and the command is the
    !> map's binding of the keys, else deactivate it.  The clearfun of
    !> subr.el:set-transient-map, with its exitfun.
    !> Port of subr.el:set-transient-map in part.
    subroutine clear_transient_map(keys)
        type(value_type), intent(in) :: keys(:)
        type(value_type) :: mc
        type(status_type) :: st
        procedure(transient_exit_proc), pointer :: on_exit

        if (transient_keep) then
            call lookup_key(map_value(overriding_terminal_local_map), keys, mc, st)

            if (st%ok() .and. .not. nilp(mc) .and. value_eq(this_command, mc)) then
                ! Repeat the message for the next command.
                if (allocated(transient_message)) last_message = transient_message
                return
            end if
        end if

        overriding_terminal_local_map = pushed_terminal_local_map
        pushed_terminal_local_map = null_handle()
        transient_active = .false.

        ! Clear the prompt after exiting.
        if (allocated(transient_message)) then
            last_message = ""
            deallocate (transient_message)
        end if

        on_exit => transient_on_exit
        transient_on_exit => null()
        if (associated(on_exit)) call on_exit()
    end subroutine clear_transient_map

    !> Run the ripe timers (keyboard.c:timer_check), show the error a
    !> timer function signaled, and redisplay if any timer ran, as
    !> process.c:wait_reading_process_output does.  The result is the
    !> number of milliseconds to wait for the next timer, or -1 for none.
    !> Port of keyboard.c:detect_input_pending_run_timers (the timers and
    !> the redisplay; `is_input_pending' checks for input).
    recursive integer(int32) function run_timers() result(ms)
        integer(int64) :: old_timers_run

        old_timers_run = timers_run
        ms = timer_check()

        if (allocated(timer_error_message)) then
            last_message = timer_error_message
            deallocate (timer_error_message)
        end if

        if (timers_run /= old_timers_run .and. associated(redisplay_hook)) call redisplay_hook()
    end function run_timers

    !> Redisplay, then wait `ms' milliseconds or until input arrives,
    !> taking subprocess output, handling special events and running the
    !> ordinary timers meanwhile.  Port of subr.el:sit-for (for a
    !> terminal; without one, only subprocess output and the other
    !> descriptors are waited for).
    recursive subroutine sit_for(ms)
        integer(int32), intent(in) :: ms
        logical :: timed_out

        timed_out = sit_for_input(ms)
    end subroutine sit_for

    !> Redisplay, then wait `ms' milliseconds or until input arrives, as
    !> `sit_for' does; true if the time passed without input (t from
    !> dispnew.c:sit_for).  Without a terminal no input can arrive: true
    !> unless events are queued.  Not a port.
    recursive logical function sit_for_input(ms) result(timed_out)
        integer(int32), intent(in) :: ms
        integer(int64) :: start, now, rate, left
        integer(int32) :: timeout, next_timer

        timed_out = .false.
        if (associated(redisplay_hook)) call redisplay_hook()
        if (is_input_pending()) return
        call system_clock(start, rate)

        do
            call run_special_events()
            call system_clock(now)
            left = int(ms, int64) - (now - start)*1000/max(rate, 1_int64)
            timed_out = left < 0
            if (timed_out) return

            if (input_fd < 0 .and. .not. associated(process_output_hook) .and. n_read_fds() == 0) then
                timed_out = .not. is_input_pending()
                return
            end if

            timeout = int(left, int32)
            next_timer = run_timers()
            if (next_timer >= 0) timeout = min(timeout, next_timer)
            if (wait_for_input(timeout)) return
            if (is_input_pending()) return
        end do
    end function sit_for_input

    !> Wait up to `timeout' milliseconds (negative: no limit) for terminal
    !> input, taking subprocess output and running the callbacks of the
    !> other descriptors if they are ready first (then redisplaying and
    !> returning false, as for a timeout).  Not a port.
    recursive logical function wait_for_input(timeout) result(ready)
        integer(int32), intent(in) :: timeout
        integer(int32), allocatable :: fds(:)
        logical, allocatable :: got(:)
        integer :: k, n

        n = n_read_fds()

        if (n == 0 .and. (process_wake_fd < 0 .or. .not. associated(process_output_hook))) then
            ready = input_available(input_fd, timeout)
            return
        end if

        allocate (fds(2 + n), got(2 + n))
        fds(1) = input_fd
        fds(2) = -1
        if (associated(process_output_hook)) fds(2) = process_wake_fd

        do k = 1, n
            fds(2 + k) = read_fds(k)%fd
        end do

        call inputs_available(fds, timeout, got)
        ready = got(1)
        if (ready) return
        if (got(2)) call process_output_hook()

        do k = 1, n
            if (.not. got(2 + k)) cycle
            ! A callback may have removed descriptors.
            if (k > n_read_fds()) exit
            if (read_fds(k)%fd /= fds(2 + k)) cycle
            call read_fds(k)%callback(fds(2 + k))
        end do

        call run_special_events()
        if (any(got) .and. associated(redisplay_hook)) call redisplay_hook()
    end function wait_for_input

    !> The number of descriptors given to `add_read_fd'.  Not a port.
    integer function n_read_fds()
        n_read_fds = 0
        if (allocated(read_fds)) n_read_fds = size(read_fds)
    end function n_read_fds

    !> Add file descriptor FD to be watched while waiting for input, with
    !> CALLBACK run when it is readable (replacing any callback FD had).
    !> Port of process.c:add_read_fd.
    subroutine add_read_fd(fd, callback)
        integer(int32), intent(in) :: fd
        procedure(read_fd_proc) :: callback
        type(read_fd_type), allocatable :: grown(:)
        integer :: k, n

        if (.not. allocated(read_fds)) allocate (read_fds(0))
        n = size(read_fds)

        do k = 1, n
            if (read_fds(k)%fd == fd) then
                read_fds(k)%callback => callback
                return
            end if
        end do

        allocate (grown(n + 1))
        grown(1:n) = read_fds
        grown(n + 1)%fd = fd
        grown(n + 1)%callback => callback
        call move_alloc(grown, read_fds)
    end subroutine add_read_fd

    !> Stop watching file descriptor FD.  Port of process.c:delete_read_fd.
    subroutine delete_read_fd(fd)
        integer(int32), intent(in) :: fd
        type(read_fd_type), allocatable :: kept(:)
        integer :: k, j

        if (.not. allocated(read_fds)) return
        allocate (kept(count(read_fds%fd /= fd)))
        j = 0

        do k = 1, size(read_fds)
            if (read_fds(k)%fd == fd) cycle
            j = j + 1
            kept(j) = read_fds(k)
        end do

        call move_alloc(kept, read_fds)
    end subroutine delete_read_fd

    !> Queue a special event: HANDLER is called with ARG the next time
    !> events are read, as `special_event_map' bindings are run by
    !> read_char.  Port of keyboard.c:kbd_buffer_store_event for special
    !> events, and of keyboard.c:Finsert_special_event (subr.el's
    !> insert-special-event).
    subroutine insert_special_event(handler, arg)
        procedure(special_event_proc) :: handler
        integer(int64), intent(in) :: arg
        type(special_event_type), allocatable :: grown(:)

        if (.not. allocated(special_events)) allocate (special_events(8))

        if (n_special_events == size(special_events)) then
            allocate (grown(2*n_special_events))
            grown(1:n_special_events) = special_events
            call move_alloc(grown, special_events)
        end if

        n_special_events = n_special_events + 1
        special_events(n_special_events)%handler => handler
        special_events(n_special_events)%arg = arg
    end subroutine insert_special_event

    !> Whether special events are waiting to be handled.  Not a port.
    logical function has_pending_special_events()
        has_pending_special_events = n_special_events > 0
    end function has_pending_special_events

    !> Handle the queued special events, oldest first, including those
    !> queued meanwhile; an error is reported as the command loop does.
    !> Port of keyboard.c:read_char in part.
    recursive subroutine run_special_events()
        type(special_event_type) :: ev
        type(status_type) :: st

        do while (n_special_events > 0)
            ev = special_events(1)
            special_events(1:n_special_events - 1) = special_events(2:n_special_events)
            n_special_events = n_special_events - 1
            call ev%handler(ev%arg, st)
            if (.not. st%ok()) call cmd_error(st)
        end do
    end subroutine run_special_events

    !> Make `command_loop' return after the current command.  Not a port.
    subroutine request_exit()
        exit_requested = .true.
    end subroutine request_exit

    !> Whether a command asked the command loop to stop.  Not a port.
    logical function is_exit_requested()
        is_exit_requested = exit_requested
    end function is_exit_requested

    !> Whether the execution of a macro has reached its end.
    !> This should be called only while executing a macro.  Port of
    !> macros.c:at_end_of_macro_p.
    logical function is_at_end_of_macro()
        is_at_end_of_macro = macro_forced_end .or. .not. allocated(macro_events)

        if (.not. is_at_end_of_macro) then
            is_at_end_of_macro = executing_kbd_macro_index >= size(macro_events, kind=int64)
        end if
    end function is_at_end_of_macro

    !> Whether a keyboard macro is executing (`executing_kbd_macro'
    !> true).  Not a port.
    logical function is_executing_kbd_macro()
        is_executing_kbd_macro = macro_active
    end function is_executing_kbd_macro

    !> Make the selected window's buffer current (set_buffer_internal
    !> (XBUFFER (XWINDOW (selected_window)->contents)) in
    !> keyboard.c:command_loop_1).  Not a port.
    subroutine select_window_buffer()
        use frame_fm, only: selected_window
        use window_fm, only: windows, is_window_live
        use buffer_fm, only: set_buffer, is_buffer_live
        type(handle_type) :: sw
        type(status_type) :: st

        sw = selected_window()
        if (.not. is_window_live(sw)) return
        if (is_buffer_live(windows(sw%index)%buffer)) call set_buffer(windows(sw%index)%buffer, st)
    end subroutine select_window_buffer

    !> Ring the bell (flash the screen with `visible_bell').  Port of
    !> terminal.c:ring_bell (no `ring_bell_function').
    subroutine ring_bell()
        if (associated(ring_bell_hook)) call ring_bell_hook()
    end subroutine ring_bell

    !> Ring the bell, or, while a keyboard macro is executing, stop it with
    !> an error.  Port of dispnew.c:bitch_at_user.
    subroutine bitch_at_user(status)
        type(status_type), intent(inout) :: status

        if (macro_active) then
            ! Stop executing a keyboard macro.
            call set_error(status, FM_USER_ERROR, "Keyboard macro terminated by a command ringing the bell")
        else
            call ring_bell()
        end if
    end subroutine bitch_at_user

    !> Report the error `status' of a command: end any keyboard macro,
    !> discard input, ring the bell and show the message, after "After N
    !> kbd macro iterations: " if a macro was running.  Port of
    !> keyboard.c:cmd_error with keyboard.c:cmd_error_internal and
    !> Fcommand_error_default_function.
    subroutine cmd_error(status)
        type(status_type), intent(in) :: status
        type(status_type) :: st
        character(len=:), allocatable :: macroerror
        character(len=24) :: buf

        if (status%code == FM_TOP_LEVEL) then
            ! A throw to `top_level' (Ftop_level) reaches command_loop's
            ! catch: no message; the keyboard macro being executed ends.
            macro_active = .false.
            macro_started = .false.
            if (allocated(macro_events)) deallocate (macro_events)
            prefix_arg = nil()
            last_prefix_arg = nil()
            return
        end if

        macroerror = ""

        if (macro_started) then
            if (executing_kbd_macro_iterations == 1) then
                macroerror = "After 1 kbd macro iteration: "
            else
                write (buf, "(i0)") executing_kbd_macro_iterations
                macroerror = "After "//trim(buf)//" kbd macro iterations: "
            end if
        end if

        macro_active = .false.
        macro_started = .false.
        if (allocated(macro_events)) deallocate (macro_events)
        prefix_arg = nil()
        last_prefix_arg = nil()
        ! command-error-default-function
        call discard_input()
        call bitch_at_user(st)
        last_message = macroerror//format_error(status)
        if (associated(message_log_hook)) call message_log_hook(last_message)
    end subroutine cmd_error

    !> Read one key sequence and run its command.  `done' at end of input
    !> (or of the keyboard macro being executed).  Errors from the command
    !> are returned in `status'; the command loop's error handler reports
    !> them in `last_message', with the bell (`cmd_error'), except in a
    !> keyboard macro, which they end.  Port of
    !> keyboard.c:command_loop_1 (one iteration) and simple.el:command-execute.
    recursive subroutine command_loop_step(done, status)
        logical, intent(out) :: done
        type(status_type), intent(out) :: status
        type(value_type), allocatable :: keys(:)
        type(value_type) :: cmd, remapped
        logical :: eof, handled, have_prefix
        procedure(command_wrapper_proc), pointer :: wrapper
        character(len=:), allocatable :: prefix
        integer(int64) :: last_point_position, prev_serial, prev_modiff

        done = .false.
        ! Make sure the current window's buffer is selected.
        call select_window_buffer()
        this_command = nil()
        real_this_command = nil()
        this_command_keys_shift_translated = .false.
        ! Read next key sequence.
        raw_keybuf_count = 0
        call read_key_sequence(keys, cmd, eof, status)

        if (.not. status%ok()) then
            if (loop_handles_errors) call cmd_error(status)
            return
        end if

        if (eof .and. size(keys) == 0) then
            done = .true.
            return
        end if

        ! minibuffer--delete-message-overlay runs from pre-command-hook.
        if (allocated(minibuffer_message_text)) deallocate (minibuffer_message_text)
        ! Input ended in the middle of a key sequence: it is unbound.
        if (eof) cmd = nil()

        ! Remap command through active keymaps (read_key_sequence_remapped).
        if (symbolp(cmd) .and. .not. nilp(cmd)) then
            call command_remapping(cmd, remapped)
            if (.not. nilp(remapped)) cmd = remapped
        end if

        last_command_event = keys(size(keys))
        ! Execute the command: recorded in recent_keys as (nil . cmd).
        call record_recent(cmd, .true.)
        this_command = cmd
        real_this_command = cmd
        ! pre-command-hook: a transient map lasts for one key sequence,
        ! unless KEEP-PRED keeps it.
        if (transient_active) call clear_transient_map(keys)
        call safe_run_hooks_maybe_narrowed(pre_command_hook)
        ! The hook may have changed the command (command-execute runs
        ! this-command).
        cmd = this_command
        wrapper => this_command_wrapper
        this_command_wrapper => null()
        ! By default, we adjust point to a boundary of a region that has
        ! such a property that should be treated intangible (e.g.
        ! composition, display).  But, some commands will set this
        ! variable differently.
        disable_point_adjustment = .false.
        last_point_position = 0
        prev_serial = -1
        prev_modiff = 0

        if (cur /= 0) then
            last_point_position = buffers(cur)%pt
            prev_serial = buffers(cur)%serial
            prev_modiff = buffers(cur)%text%modiff
        end if

        ! keyboard.c:command_loop_1: undo boundaries for the changes of the
        ! last command, and where point is for record_point.
        if (cur /= 0) then
            call undo_auto_add_boundary()
            point_before_last_command_or_undo = buffers(cur)%pt
            buffer_before_last_command_or_undo = buffers(cur)%serial
        end if

        deactivate_mark_variable = .false.

        if (nilp(cmd)) then
            ! nil means key is undefined.
            call funcall(sym("undefined"), status)
        else
            ! simple.el:command-execute
            current_prefix_arg = prefix_arg
            prefix_arg = nil()

            if (associated(wrapper)) then
                call wrapper(cmd, status)
            else if (stringp(cmd) .or. vtag(cmd) == V_LIST) then
                call execute_kbd_macro(cmd, current_prefix_arg, status)
            else
                handled = .false.
                if (associated(disabled_command_hook)) call disabled_command_hook(cmd, handled, status)
                if (.not. handled) call call_interactively(cmd, status)
            end if
        end if

        if (.not. status%ok()) then
            ! The error leaves command_loop_1: in a keyboard macro's loop it
            ! ends the macro; else the handler reports it and the next
            ! command_loop_1 runs post-command-hook and sets last-command.
            if (.not. loop_handles_errors) return
            call cmd_error(status)
            ! command_loop_1 starts again.
            call cancel_echoing()
            call set_command_key_count(0)
            this_single_command_key_start = 0
            call safe_run_hooks_maybe_narrowed(post_command_hook)
            last_command = this_command
            real_last_command = real_this_command
            if (vtag(last_command_event) /= V_LIST) last_repeatable_command = real_this_command
            done = eof
            return
        end if

        last_prefix_arg = current_prefix_arg
        call safe_run_hooks_maybe_narrowed(post_command_hook)
        last_command = this_command
        real_last_command = real_this_command
        if (vtag(last_command_event) /= V_LIST) last_repeatable_command = real_this_command
        call set_command_key_count(0)
        this_single_command_key_start = 0
        call internal_echo_keystrokes_prefix(prefix, have_prefix)

        if (immediate_echo .and. have_prefix) then
            immediate_echo = .false.
            ! Refresh the echo message.
            call echo_now()
        else
            call cancel_echoing()
        end if

        ! A command that changed the buffer (or asked to) ends an active
        ! region in Transient Mark mode (simple.el:deactivate-mark).
        if (cur /= 0) then
            if (buffers(cur)%mark_active .and. deactivate_mark_variable .and. transient_mark_mode%value) then
                if (associated(deactivate_mark_function)) then
                    call deactivate_mark_function()
                else
                    buffers(cur)%mark_active = .false.
                end if
            end if
        end if

        ! finalize: point moved into text that should be intangible.
        if (cur /= 0) then
            if (buffers(cur)%serial == prev_serial .and. selected_window_shows_current() .and. &
                last_point_position /= buffers(cur)%pt) then
                if (.not. disable_point_adjustment .and. .not. global_disable_point_adjustment) then
                    call adjust_point_for_property(last_point_position, buffers(cur)%text%modiff /= prev_modiff)
                end if
            end if
        end if

        ! Install chars successfully executed in kbd macro.
        if (defining_kbd_macro /= DEFINING_KBD_MACRO_NO .and. nilp(prefix_arg)) call finalize_kbd_macro_chars()
        done = eof
    end subroutine command_loop_step

    !> Whether the selected window shows the current buffer.  Not a port.
    logical function selected_window_shows_current() result(shows)
        use frame_fm, only: selected_window
        use window_fm, only: windows, is_window_live
        type(handle_type) :: sw

        shows = .false.
        sw = selected_window()
        if (.not. is_window_live(sw)) return
        shows = windows(sw%index)%buffer%index == buffer_handle_index()
    end function selected_window_shows_current

    !> The handle index of the current buffer.  Not a port.
    integer function buffer_handle_index() result(k)
        use buffer_fm, only: current_buffer
        type(handle_type) :: b

        b = current_buffer()
        k = b%index
    end function buffer_handle_index

    !> Whether display property value PROP replaces the text it is on (a
    !> string or a space spec; the other replacing specs, such as
    !> images, are not shown yet).  Port of
    !> xdisp.c:display_prop_intangible_p.
    logical function is_display_prop_intangible(prop) result(replaces)
        type(value_type), intent(in) :: prop
        type(value_type), allocatable :: elts(:)

        replaces = stringp(prop)
        if (replaces .or. vtag(prop) /= V_LIST) return
        call list_elements(prop, elts)
        if (size(elts) > 0) replaces = value_eq(elts(1), sym("space"))
    end function is_display_prop_intangible

    !> Adjust point to a boundary of a region that has such a property
    !> that should be treated intangible (`display' replacing text, and
    !> `invisible'; no compositions yet).  For the moment, we check
    !> `composition', `display' and `invisible' properties.  LAST_PT is
    !> the last position of point.  MODIFIED says the command changed
    !> the buffer.  Port of keyboard.c:adjust_point_for_property.
    subroutine adjust_point_for_property(last_pt, modified)
        integer(int64), intent(in) :: last_pt
        logical, intent(in) :: modified
        integer(int64) :: beg, end, orig_pt, pt_before_invis, begv, zv
        type(value_type) :: val
        type(handle_type) :: overlay
        logical :: check_display, check_invisible, ellipsis, shown, ok
        integer :: inv

        check_display = .true.
        check_invisible = .true.
        orig_pt = pt()
        pt_before_invis = pt()
        begv = buffers(cur)%begv
        zv = buffers(cur)%zv

        do while (check_display .or. check_invisible)
            if (check_display .and. pt() > begv .and. pt() < zv) then
                call get_char_property_and_overlay(pt(), sym("display"), val, overlay)
                ok = .false.
                if (.not. nilp(val)) ok = is_display_prop_intangible(val)

                if (ok) then
                    if (overlay%index == 0) then
                        call get_property_and_range(pt(), sym("display"), val, beg, end)
                    else
                        beg = overlay_start(overlay)
                        end = overlay_end(overlay)
                    end if

                    if (beg < pt() .or. (beg <= pt() .and. empty_string(val))) then
                        if (pt() < last_pt) then
                            if (empty_string(val)) then
                                call goto_char(max(beg - 1, begv))
                            else
                                call goto_char(beg)
                            end if
                        else
                            call goto_char(end)
                        end if

                        check_invisible = .true.
                    end if
                end if
            end if

            check_display = .false.
            pt_before_invis = pt()

            if (check_invisible .and. pt() > begv .and. pt() < zv) then
                ellipsis = .false.
                beg = pt()
                end = pt()

                ! Find boundaries `beg' and `end' of the invisible area, if
                ! any.
                do while (end < zv)
                    call get_char_property_and_overlay(end, sym("invisible"), val, overlay)
                    if (nilp(val)) exit
                    inv = text_prop_means_invisible(val)
                    if (inv == 0) exit
                    ellipsis = ellipsis .or. inv > 1 .or. has_overlay_strings(overlay)
                    end = next_single_char_property_change(end, sym("invisible"))
                end do

                do while (beg > begv)
                    call get_char_property_and_overlay(beg - 1, sym("invisible"), val, overlay)
                    if (nilp(val)) exit
                    inv = text_prop_means_invisible(val)
                    if (inv == 0) exit
                    ellipsis = ellipsis .or. inv > 1 .or. has_overlay_strings(overlay)
                    beg = previous_single_char_property_change(beg, sym("invisible"))
                end do

                ! Move away from the inside area.
                if (beg < pt() .and. end > pt()) then
                    if (orig_pt == pt() .and. (last_pt < beg .or. last_pt > end)) then
                        ! We haven't moved yet (so we don't need to fear
                        ! infinite-looping) and we were outside the range
                        ! before (so either end of the range still
                        ! corresponds to a move in the right direction):
                        ! pretend we moved less than we actually did, so that
                        ! we still have more freedom below in choosing which
                        ! end of the range to go to.
                        orig_pt = -1

                        if (pt() < last_pt) then
                            call goto_char(end)
                        else
                            call goto_char(beg)
                        end if
                    else
                        ! We either have moved already or the last point was
                        ! already in the range: we don't get to choose which
                        ! end of the range we have to go to.
                        if (pt() < last_pt) then
                            call goto_char(beg)
                        else
                            call goto_char(end)
                        end if
                    end if

                    check_display = .true.
                end if

                ! Pretend the area doesn't exist if the buffer is not
                ! modified, but only if the invisible text is not shown on
                ! display.  Invisible text might be "shown on display" if
                ! there's a replacing display property on the same text.
                call get_char_property_and_overlay(beg, sym("display"), val, overlay)
                shown = .false.
                if (.not. nilp(val)) shown = is_display_prop_intangible(val)
                ! If the "invisible" text is shown, undo any point
                ! adjustments due to invisible property, as cursor
                ! positioning by the display engine will do a better job.
                if (shown) call goto_char(pt_before_invis)

                if (.not. modified .and. .not. shown .and. .not. ellipsis .and. beg < end) then
                    if (last_pt == beg .and. pt() == end .and. end < zv) then
                        check_display = .true.
                        call goto_char(end + 1)
                    else if (last_pt == end .and. pt() == beg .and. beg > begv) then
                        check_display = .true.
                        call goto_char(beg - 1)
                    else if (pt() == merge(beg, end, pt() < last_pt)) then
                        ! We've already moved as far as we can.  Trying to go
                        ! to the other end would mean moving backwards and
                        ! thus could lead to an infinite loop.
                        continue
                    else if (text_prop_means_invisible(get_pos_property(pt(), sym("invisible"))) /= 0) then
                        if (text_prop_means_invisible(get_pos_property(merge(end, beg, pt() == beg), &
                                                                       sym("invisible"))) == 0) then
                            check_display = .true.
                            call goto_char(merge(end, beg, pt() == beg))
                        end if
                    end if
                end if
            end if

            check_invisible = .false.
        end do

    contains

        integer(int64) function pt()
            pt = buffers(cur)%pt
        end function pt

        logical function empty_string(v)
            type(value_type), intent(in) :: v

            empty_string = .false.
            if (stringp(v)) empty_string = len(xstring(v)) == 0
        end function empty_string

        !> Whether OVERLAY has an after-string or a before-string.
        logical function has_overlay_strings(ov)
            type(handle_type), intent(in) :: ov

            has_overlay_strings = .false.
            if (ov%index == 0) return
            has_overlay_strings = .not. nilp(overlay_get(ov, "after_string")) .or. &
                                  .not. nilp(overlay_get(ov, "before_string"))
        end function has_overlay_strings
    end subroutine adjust_point_for_property

    !> Run commands until input ends or `request_exit' is called.  Command
    !> errors don't stop the loop.  Port of keyboard.c:command_loop_1 with
    !> keyboard.c:command_loop and keyboard.c:command_loop_2 (the
    !> top-level loop; errors are handled by `command_loop_step').
    recursive subroutine command_loop(status)
        type(status_type), intent(out) :: status
        type(status_type) :: step_status
        logical :: done, saved_handles

        exit_requested = .false.
        saved_handles = loop_handles_errors
        loop_handles_errors = .true.
        ! (command_loop_1's other initializations are left out: callers
        ! run this loop again for more input, as one session.)
        call cancel_echoing()
        call set_command_key_count(0)
        this_single_command_key_start = 0

        do
            call command_loop_step(done, step_status)
            if (done .or. exit_requested) exit
        end do

        macro_started = .false.
        loop_handles_errors = saved_handles
        exit_requested = .false.
    end subroutine command_loop

    !> Run a command loop until a command throws to `exit': return when
    !> it throws nil, signal quit when it throws t.  End of input, or a
    !> request to leave Emacs, also ends it with a quit; the end of the
    !> keyboard macro being executed ends it as an exit.  Its command loop
    !> handles errors (internal_catch (Qexit, command_loop_2, Qerror)).
    !> Minibuffers call it directly; `recursive_edit' counts the level.
    !> Port of keyboard.c:recursive_edit_1 with keyboard.c:command_loop.
    recursive subroutine recursive_edit_1(status)
        type(status_type), intent(out) :: status
        type(status_type) :: step_status
        logical :: done, saved_handles, macro_end

        exit_pending = .false.
        saved_handles = loop_handles_errors
        loop_handles_errors = .true.
        macro_end = .false.
        exit_levels = 1
        if (allocated(exit_string)) deallocate (exit_string)
        call command_loop_1_start()

        do
            call command_loop_step(done, step_status)
            if (done .and. macro_active) macro_end = is_at_end_of_macro()
            if (exit_pending .or. done .or. exit_requested) exit
        end do

        loop_handles_errors = saved_handles
        ! command_loop: executing_kbd_macro = Qnil.
        macro_started = .false.

        if (macro_end .and. .not. exit_pending) then
            ! command_loop_1 returned at the end of the macro: as (throw
            ! 'exit nil).
            exit_pending = .true.
            exit_abort = .false.
        end if

        if (allocated(exit_string)) then
            call set_error(status, FM_GENERIC_ERROR, exit_string)
            deallocate (exit_string)
        else if (top_level_pending) then
            ! A throw to `top_level' passes through.
            call set_error(status, FM_TOP_LEVEL)
            if (exit_levels <= 1) top_level_pending = .false.
        else if (.not. exit_pending .or. exit_abort) then
            call set_error(status, FM_QUIT)
        end if

        if (exit_pending .and. exit_abort .and. exit_levels > 1) then
            ! Quit the enclosing recursive edits too.
            exit_levels = exit_levels - 1
        else
            exit_pending = .false.
            exit_abort = .false.
            exit_levels = 1
        end if
    end subroutine recursive_edit_1

    !> (throw 'exit nil), or with `abort' (throw 'exit t): end the
    !> innermost recursive edit once the current command returns.  With
    !> `abort', `levels' (default 1) is the number of nested recursive
    !> edits to quit (as minibuffer.el:minibuffer-quit-recursive-edit).
    !> Not a port.
    subroutine throw_exit(abort, levels)
        logical, intent(in) :: abort
        integer, intent(in), optional :: levels

        exit_pending = .true.
        exit_abort = abort
        exit_levels = 1
        if (present(levels)) exit_levels = max(1, levels)
    end subroutine throw_exit

    !> (throw 'exit STRING): end the innermost recursive edit once the
    !> current command returns; it signals an error with message `msg'.
    !> Port of keyboard.c:Frecursive_edit's handling of a string thrown
    !> to `exit'.
    subroutine throw_exit_string(msg)
        character(len=*), intent(in) :: msg

        exit_pending = .true.
        exit_abort = .false.
        exit_levels = 1
        exit_string = msg
    end subroutine throw_exit_string

    !> Exit from the innermost recursive edit or minibuffer.  Port of
    !> keyboard.c:Fexit_recursive_edit (with keyboard.c:user_error).
    subroutine exit_recursive_edit(status)
        type(status_type), intent(out) :: status

        if (command_loop_level > 0 .or. minibuf_level > 0) then
            call throw_exit(.false.)
        else
            call set_error(status, FM_USER_ERROR, "No recursive edit is in progress")
        end if
    end subroutine exit_recursive_edit

    !> Abort the command that started the innermost recursive edit or
    !> minibuffer.  Port of keyboard.c:Fabort_recursive_edit.
    subroutine abort_recursive_edit(status)
        type(status_type), intent(out) :: status

        if (command_loop_level > 0 .or. minibuf_level > 0) then
            call throw_exit(.true.)
        else
            call set_error(status, FM_USER_ERROR, "No recursive edit is in progress")
        end if
    end subroutine abort_recursive_edit

    ! ------------------------------------------------------------------
    ! Echoing
    ! ------------------------------------------------------------------

    !> Whether echoing is on: `echo_keystrokes' is positive.  Port of
    !> keyboard.c:echo_keystrokes_p.
    logical function is_echo_keystrokes()
        is_echo_keystrokes = echo_keystrokes > 0.0
    end function is_echo_keystrokes

    !> Add C to the echo string, without echoing it immediately.  C can
    !> be a character, which is pretty-printed, or a symbol, whose name is
    !> printed.  Port of keyboard.c:echo_add_key.
    subroutine echo_add_key(c)
        type(value_type), intent(in) :: c
        type(value_type) :: head
        character(len=:), allocatable :: new_string
        logical :: empty

        new_string = ""
        empty = .true.
        if (allocated(echo_string)) empty = len(echo_string) == 0
        ! Add a space at the end as a separator between keys.
        if (.not. empty) new_string = " "
        ! If someone has passed us a composite event, use its head symbol.
        head = c
        if (vtag(c) == V_LIST) head = list_elt(c, 1_int64)

        if (fixnump(head)) then
            new_string = new_string//push_key_description(xfixnum(head))
        else if (symbolp(head)) then
            new_string = new_string//symbols(xsymbol(head))%name
        end if

        if (empty .and. is_help_char(head)) then
            new_string = new_string//" (Type ? for further options, C-q for quick help)"
        end if

        if (allocated(echo_string)) then
            echo_string = echo_string//new_string
        else
            echo_string = new_string
        end if
    end subroutine echo_add_key

    !> Temporarily add a dash to the end of the echo string if it's not
    !> empty, so that it serves as a mini-prompt for the very next
    !> character.  Port of keyboard.c:echo_dash.
    recursive subroutine echo_dash()
        integer :: n

        ! Do nothing if not echoing at all.
        if (.not. allocated(echo_string)) return
        if (.not. immediate_echo .and. len(echo_string) == 0) return

        ! Do nothing if we just printed a prompt.
        if (allocated(echo_prompt)) then
            if (len(echo_prompt) == len(echo_string)) return
        end if

        ! Do nothing if we have already put a dash at the end.
        n = len(echo_string)

        if (n > 1) then
            if (echo_string(n:n) == "-" .and. echo_string(n - 1:n - 1) /= " ") return
            ! Or a keystroke help message.
            if (echo_keystrokes_help .and. echo_string(n:n) == ")" .and. echo_string(n - 1:n - 1) == "p") return
        end if

        ! Put a dash at the end of the buffer temporarily, but make it go
        ! away when the next character is added.
        echo_string = echo_string//"-"
        if (echo_keystrokes_help) echo_string = append_keystrokes_help(echo_string)
        call echo_now()
    end subroutine echo_dash

    !> Make the echo string the prompt, the prefix state and the keys of
    !> the current command, and show it, when echoing immediately.
    !> Port of keyboard.c:echo_update.
    recursive subroutine echo_update()
        character(len=:), allocatable :: prefix
        logical :: have_prefix
        integer :: i

        if (.not. immediate_echo) return
        call internal_echo_keystrokes_prefix(prefix, have_prefix)
        if (allocated(echo_string)) deallocate (echo_string)

        if (allocated(echo_prompt)) then
            echo_string = echo_prompt
            if (have_prefix) echo_string = echo_string//prefix
        else if (have_prefix) then
            echo_string = prefix
        end if

        if (allocated(this_command_keys)) then
            do i = 1, size(this_command_keys)
                call echo_add_key(this_command_keys(i))
            end do
        end if

        call echo_now()
    end subroutine echo_update

    !> Display the current echo string, and begin echoing if not already
    !> doing so.  Port of keyboard.c:echo_now (the echo area is
    !> `last_message').
    recursive subroutine echo_now()
        if (.not. immediate_echo) then
            immediate_echo = .true.
            call echo_update()
            ! Put a dash at the end to invite the user to type more.
            call echo_dash()
        end if

        ! message3_nolog
        if (allocated(echo_string)) then
            last_message = echo_string
        else
            last_message = ""
        end if

        ! Record in what buffer we echoed, and from which kboard.
        echo_kboard_current = .true.
    end subroutine echo_now

    !> Turn off echoing, for the start of a new command.  Port of
    !> keyboard.c:cancel_echoing.
    subroutine cancel_echoing()
        immediate_echo = .false.
        if (allocated(echo_prompt)) deallocate (echo_prompt)
        if (allocated(echo_string)) deallocate (echo_string)
        ok_to_echo_at_next_pause = .false.
        echo_kboard_current = .false.
    end subroutine cancel_echoing

    !> The length of the current echo string (in bytes: only compared with
    !> other such lengths).  Port of keyboard.c:echo_length.
    integer function echo_length()
        echo_length = 0
        if (allocated(echo_string)) echo_length = len(echo_string)
    end function echo_length

    !> Truncate the current echo message to its first NCHARS (bytes, as
    !> `echo_length' gives them).  Port of keyboard.c:echo_truncate
    !> (with xdisp.c:truncate_echo_area).
    subroutine echo_truncate(nchars)
        integer, intent(in) :: nchars

        if (allocated(echo_string)) then
            if (len(echo_string) > nchars) echo_string = echo_string(1:nchars)
        end if

        if (allocated(last_message)) then
            if (len(last_message) > nchars) last_message = last_message(1:nchars)
        end if
    end subroutine echo_truncate

    !> Whether the echo area shows a message.  Not a port.
    logical function echo_area_busy()
        echo_area_busy = allocated(last_message)
        if (echo_area_busy) echo_area_busy = len(last_message) > 0
    end function echo_area_busy

    !> Before waiting for an event: let a message turn off echoing, or
    !> invite more keys with a dash; then, in the middle of a key sequence
    !> and with no minibuffer active, start echoing if `echo_keystrokes'
    !> seconds pass without input.  Port of keyboard.c:read_char in part.
    recursive subroutine echo_before_reading(end_time)
        !> The read has a time limit (read_char's END_TIME).
        logical, intent(in) :: end_time
        character(len=:), allocatable :: prefix
        logical :: have_prefix, ok

        ! Message turns off echoing unless more keystrokes turn it on
        ! again.
        if (echo_area_busy() .and. (.not. echo_kboard_current .or. .not. ok_to_echo_at_next_pause)) then
            call cancel_echoing()
        else
            call echo_dash()
        end if

        ! If in middle of key sequence and minibuffer not active, start
        ! echoing if enough time elapses.
        if (minibuf_level /= 0 .or. end_time .or. immediate_echo .or. .not. is_echo_keystrokes()) return
        ok = .false.
        if (allocated(this_command_keys)) ok = size(this_command_keys) > 0

        if (.not. ok) then
            call internal_echo_keystrokes_prefix(prefix, have_prefix)
            ok = have_prefix
        end if

        if (.not. ok) return
        ! No message, or already echoing, or not echoing before and echoing
        ! allowed.
        if (echo_area_busy() .and. .not. ok_to_echo_at_next_pause) return

        if (sit_for_input(int(echo_keystrokes*1000))) then
            if (nunread == 0) call echo_now()
        end if
    end subroutine echo_before_reading

    !> Whether event C means help: `help_char' or one of
    !> `help_event_list'.  Port of keyboard.c:help_char_p.
    logical function is_help_char(c)
        type(value_type), intent(in) :: c
        type(value_type), allocatable :: evs(:)
        type(status_type) :: st
        integer :: k

        is_help_char = .false.

        if (fixnump(c)) then
            is_help_char = xfixnum(c) == help_char
            if (is_help_char) return
        end if

        if (.not. allocated(help_event_list)) return

        do k = 1, size(help_event_list)
            call key_parse(trim(help_event_list(k)), evs, st)
            if (.not. st%ok() .or. size(evs) /= 1) cycle

            if (value_eq(evs(1), c)) then
                is_help_char = .true.
                return
            end if
        end do
    end function is_help_char

    !> STR followed by a hint naming the first help key (`help_char' for
    !> `help', or a `help_event_list' event) that the maps the keys of the
    !> current command lead to don't bind.  Port of
    !> help.el:help--append-keystrokes-help.
    function append_keystrokes_help(str) result(res)
        character(len=*), intent(in) :: str
        character(len=:), allocatable :: res
        type(value_type), allocatable :: keys(:), maps(:), bindings(:), key(:), evs(:)
        type(value_type) :: b, def
        type(status_type) :: st
        integer :: k, j, n
        logical :: bound

        res = str
        keys = this_single_command_keys()
        allocate (bindings(0))

        if (size(keys) > 0) then
            call list_elements(current_active_maps(), maps)
            deallocate (bindings)
            allocate (bindings(size(maps)))
            n = 0

            do k = 1, size(maps)
                call lookup_key(maps(k), keys, b, st, accept_default=.true.)
                if (.not. st%ok() .or. nilp(b)) cycle
                n = n + 1
                bindings(n) = b
            end do

            bindings = bindings(1:n)
        end if

        allocate (key(1))
        n = 0
        if (allocated(help_event_list)) n = size(help_event_list)

        do k = 0, n
            if (k == 0) then
                ! (The list starts with `help', which stands for help-char.)
                key(1) = make_fixnum(help_char)
            else
                call key_parse(trim(help_event_list(k)), evs, st)
                if (.not. st%ok() .or. size(evs) /= 1) cycle
                if (value_eq(evs(1), sym("help"))) cycle
                key(1) = evs(1)
            end if

            bound = .false.

            do j = 1, size(bindings)
                if (.not. is_keymap(bindings(j))) cycle
                call lookup_key(bindings(j), key, def, st)
                if (st%ok() .and. .not. nilp(def)) bound = .true.
            end do

            if (.not. bound) then
                res = str//" ("//key_description(key)//" for help)"
                return
            end if
        end do
    end function append_keystrokes_help

    !> The description of the current prefix state, if a prefix command
    !> has updated it (`have' is false for nil): what the functions of
    !> prefix-command-echo-keystrokes-functions return, here
    !> `universal_argument_description'.  Port of
    !> simple.el:internal-echo-keystrokes-prefix.
    subroutine internal_echo_keystrokes_prefix(prefix, have)
        character(len=:), allocatable, intent(out) :: prefix
        logical, intent(out) :: have

        if (prefix_command_needs_update) then
            ! (Unallocated when there is no prefix argument.)
            call universal_argument_description(prefix_command_last_echo, have)
        end if

        have = allocated(prefix_command_last_echo)
        if (have) prefix = prefix_command_last_echo
    end subroutine internal_echo_keystrokes_prefix

    !> Update state of prefix commands.
    !> Call it whenever you change the "prefix command state".  Port of
    !> simple.el:prefix-command-update.
    subroutine prefix_command_update()
        prefix_command_needs_update = .true.
    end subroutine prefix_command_update

    !> The description of `prefix_arg' for echoing ("C-u", "C-u C-u",
    !> "C-u 5", "C-u -"; `have' is false when there is no prefix
    !> argument).  Port of simple.el:universal-argument--description.
    subroutine universal_argument_description(s, have)
        character(len=:), allocatable, intent(out) :: s
        logical, intent(out) :: have
        type(value_type) :: head
        character(len=:), allocatable :: str
        character(len=24) :: buf
        integer(int64) :: n

        have = .not. nilp(prefix_arg)
        if (.not. have) return
        s = "C-u"

        if (value_eq(prefix_arg, sym("-"))) then
            s = s//" -"
        else if (vtag(prefix_arg) == V_LIST) then
            head = list_elt(prefix_arg, 1_int64)

            if (fixnump(head)) then
                n = xfixnum(head)
                str = ""

                do while (n > 4 .and. modulo(n, 4_int64) == 0)
                    str = str//" C-u"
                    n = n/4
                end do

                if (n == 4) then
                    s = s//str
                else
                    write (buf, "(i0)") xfixnum(head)
                    s = s//" ("//trim(buf)//")"
                end if
            end if
        else if (fixnump(prefix_arg)) then
            write (buf, "(i0)") xfixnum(prefix_arg)
            s = s//" "//trim(buf)
        end if
    end subroutine universal_argument_description

    ! ------------------------------------------------------------------
    ! this-command-keys
    ! ------------------------------------------------------------------

    !> Add KEY to `this_command_keys'.  Port of keyboard.c:add_command_key.
    subroutine add_command_key(key)
        type(value_type), intent(in) :: key
        type(value_type), allocatable :: grown(:)
        integer :: n

        if (.not. allocated(this_command_keys)) allocate (this_command_keys(0))
        n = size(this_command_keys)
        allocate (grown(n + 1))
        if (n > 0) grown(1:n) = this_command_keys
        grown(n + 1) = key
        call move_alloc(grown, this_command_keys)
    end subroutine add_command_key

    !> Keep the first N of `this_command_keys' (keyboard.c's
    !> `this_command_key_count = N').  Not a port.
    subroutine set_command_key_count(n)
        integer, intent(in) :: n
        type(value_type), allocatable :: kept(:)

        if (.not. allocated(this_command_keys)) allocate (this_command_keys(0))
        if (size(this_command_keys) <= n) return
        allocate (kept(n))
        if (n > 0) kept = this_command_keys(1:n)
        call move_alloc(kept, this_command_keys)
    end subroutine set_command_key_count

    !> Return the key sequence that invoked this command, as a vector.
    !> However, if the command has called `read_key_sequence', it returns
    !> the last key sequence that has been read.
    !>
    !> See also `this_command_keys'.  Port of
    !> keyboard.c:Fthis_command_keys_vector and of
    !> keyboard.c:Fthis_command_keys (events are kept as arrays, not
    !> strings).
    function this_command_keys_vector() result(keys)
        type(value_type), allocatable :: keys(:)

        if (allocated(this_command_keys)) then
            keys = this_command_keys
        else
            allocate (keys(0))
        end if
    end function this_command_keys_vector

    !> Return the key sequence that invoked this command.
    !> More generally, it returns the last key sequence read, either by
    !> the command loop or by `read_key_sequence'.
    !> The value is always a vector.  Port of
    !> keyboard.c:Fthis_single_command_keys.
    function this_single_command_keys() result(keys)
        type(value_type), allocatable :: keys(:)
        integer :: n

        n = 0
        if (allocated(this_command_keys)) n = size(this_command_keys) - this_single_command_key_start

        if (n <= 0) then
            allocate (keys(0))
        else
            keys = this_command_keys(this_single_command_key_start + 1:)
        end if
    end function this_single_command_keys

    !> Return the raw events that were read for this command.
    !> More generally, it returns the last key sequence read, either by
    !> the command loop or by `read_key_sequence'.
    !> Unlike `this_single_command_keys', this function's value
    !> shows the events before all translations (except for input methods).
    !> The value is always a vector.  Port of
    !> keyboard.c:Fthis_single_command_raw_keys.
    function this_single_command_raw_keys() result(keys)
        type(value_type), allocatable :: keys(:)

        allocate (keys(raw_keybuf_count))
        if (raw_keybuf_count > 0) keys = raw_keybuf(1:raw_keybuf_count)
    end function this_single_command_raw_keys

    !> Set the vector to be returned by `this_command_keys'.
    !> The argument KEYS must be a string.
    !> Internal use only.  Port of keyboard.c:Fset__this_command_keys.
    subroutine set__this_command_keys(keys)
        character(len=*), intent(in) :: keys
        integer(int64) :: pos
        integer(int32) :: c, length
        logical :: first

        call set_command_key_count(0)
        this_single_command_key_start = 0
        pos = 1
        first = .true.

        do while (pos <= len(keys, kind=int64))
            call string_char_and_length(keys, pos, c, length)
            pos = pos + length
            if (is_char_byte8(c)) c = char_to_byte8(c)

            ! Kludge alert: this makes M-x be in the form expected by
            ! novice.el.  (248 is \370, a.k.a. "Meta-x".)
            if (first .and. c == 248) then
                call add_command_key(make_fixnum(ior(int(iachar("x"), int64), M_META)))
            else
                call add_command_key(make_fixnum(int(c, int64)))
            end if

            first = .false.
        end do
    end subroutine set__this_command_keys

    !> Clear out the vector that `this_command_keys' returns.
    !> Also clear the record of the last 300 input events, unless optional arg
    !> KEEP_RECORD is true.  Port of keyboard.c:Fclear_this_command_keys.
    subroutine clear_this_command_keys(keep_record)
        logical, intent(in), optional :: keep_record
        logical :: keep

        call set_command_key_count(0)
        keep = .false.
        if (present(keep_record)) keep = keep_record
        if (keep) return
        if (allocated(recent_events)) deallocate (recent_events, recent_is_command)
        total_keys = 0
        recent_keys_index = 1
    end subroutine clear_this_command_keys

    !> Return the current depth in recursive edits.  Port of
    !> keyboard.c:Frecursion_depth.
    integer function recursion_depth()
        recursion_depth = command_loop_level + minibuf_level
    end function recursion_depth

    ! ------------------------------------------------------------------
    ! Reading events
    ! ------------------------------------------------------------------

    !> Signal `inhibited_interaction' if `inhibit_interaction' is true.
    !> Port of minibuf.c:barf_if_interaction_inhibited.
    subroutine barf_if_interaction_inhibited(status)
        type(status_type), intent(inout) :: status

        if (inhibit_interaction) call set_error(status, FM_INHIBITED_INTERACTION)
    end subroutine barf_if_interaction_inhibited

    !> Show PROMPT for reading an event (read-char and friends): echoing
    !> is canceled and PROMPT shown in the echo area.  Not a port.
    subroutine show_read_prompt(prompt)
        character(len=*), intent(in), optional :: prompt

        if (.not. present(prompt)) return
        call cancel_echoing()
        ! message_with_string, without logging
        last_message = prompt
    end subroutine show_read_prompt

    !> Read an event, ignoring switch-frame events.  With ASCII_REQUIRED,
    !> function keys with a character equivalent (`ascii_character')
    !> become that character, with their modifiers; other non-character
    !> events are an error ("Non-character input-event", the event going
    !> back to `unread_command_events') with ERROR_NONASCII, else skipped.
    !> With SECONDS, wait at most that long: `have' is then false on a
    !> timeout.  Port of keyboard.c:read_filtered_event (no input methods
    !> or text conversion).
    recursive subroutine read_filtered_event(ascii_required, error_nonascii, ev, have, status, seconds)
        logical, intent(in) :: ascii_required, error_nonascii
        type(value_type), intent(out) :: ev
        logical, intent(out) :: have
        type(status_type), intent(out) :: status
        real, intent(in), optional :: seconds
        character(len=:), allocatable :: base
        integer(int64) :: bits, c

        do
            call read_input_event(ev, have, status, seconds)
            if (.not. status%ok() .or. .not. have) return
            if (.not. ascii_required) return

            ! Convert certain symbols to their ASCII equivalents.
            if (symbolp(ev)) then
                call parse_modifiers(ev, bits, base)
                c = ascii_character(sym(base))
                ! Merge this symbol's modifier bits with the ASCII
                ! equivalent of its basic code.
                if (c >= 0) ev = make_fixnum(ior(c, bits))
            end if

            ! If we don't have a character now, deal with it appropriately.
            if (fixnump(ev)) return

            if (error_nonascii) then
                call unread_command_events([ev])
                call set_error(status, FM_GENERIC_ERROR, "Non-character input-event")
                return
            end if
        end do
    end subroutine read_filtered_event

    !> Read a character event from the command input (keyboard or macro).
    !> Return the character as a number.
    !> If the event has modifiers, they are resolved and reflected in the
    !> returned character code if possible (e.g. C-SPC yields 0 and C-a yields 97).
    !> If some of the modifiers cannot be reflected in the character code, the
    !> returned value will include those modifiers, and will not be a valid
    !> character code: it will fail the `characterp' test.  Use `event_basic_type'
    !> to recover the character code with the modifiers removed.
    !>
    !> If the user generates an event which is not a character (i.e. a mouse
    !> click or function key event), `read_char' signals an error.  As an
    !> exception, switch-frame events are put off until non-character events
    !> can be read.
    !> If you want to read non-character events, or ignore them, call
    !> `read_event' or `read_char_exclusive' instead.
    !>
    !> If the optional argument PROMPT is present, display that as a prompt.
    !>
    !> If the optional argument SECONDS is present, it should be a number
    !> specifying the maximum number of seconds to wait for input.  If no
    !> input arrives in that time, return false in HAVE.  SECONDS may be a
    !> floating-point value.
    !>
    !> If `inhibit_interaction' is true, this function will signal an
    !> `inhibited_interaction' error.  Port of keyboard.c:Fread_char (no
    !> input methods).
    recursive subroutine read_char(c, have, status, prompt, seconds)
        integer(int64), intent(out) :: c
        logical, intent(out) :: have
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: prompt
        real, intent(in), optional :: seconds
        type(value_type) :: ev

        c = -1
        have = .false.
        call barf_if_interaction_inhibited(status)
        if (.not. status%ok()) return
        call show_read_prompt(prompt)
        call read_filtered_event(.true., .true., ev, have, status, seconds)
        if (.not. status%ok() .or. .not. have) return
        have = fixnump(ev)
        if (have) c = int(char_resolve_modifier_mask(int(xfixnum(ev), int32)), int64)
    end subroutine read_char

    !> Read and return an event object from the input stream.
    !>
    !> If you want to read non-character events, consider calling `read_key'
    !> instead.  `read_key' will decode events via `input_decode_map' that
    !> `read_event' will not.  On a terminal this includes function keys such
    !> as <F7> and <RIGHT>, or mouse events generated by `xterm_mouse_mode'.
    !>
    !> If the optional argument PROMPT is present, display that as a prompt.
    !>
    !> If the optional argument SECONDS is present, it should be a number
    !> specifying the maximum number of seconds to wait for input.  If no
    !> input arrives in that time, return false in HAVE.  SECONDS may be a
    !> floating-point value.
    !>
    !> If `inhibit_interaction' is true, this function will signal an
    !> `inhibited_interaction' error.  Port of keyboard.c:Fread_event (no
    !> input methods).
    recursive subroutine read_event(ev, have, status, seconds, prompt)
        type(value_type), intent(out) :: ev
        logical, intent(out) :: have
        type(status_type), intent(out) :: status
        real, intent(in), optional :: seconds
        character(len=*), intent(in), optional :: prompt

        have = .false.
        call barf_if_interaction_inhibited(status)
        if (.not. status%ok()) return
        call show_read_prompt(prompt)
        call read_filtered_event(.false., .false., ev, have, status, seconds)
    end subroutine read_event

    !> Read a character event from the command input (keyboard or macro).
    !> Return the character as a number.  Non-character events are ignored.
    !> If the event has modifiers, they are resolved and reflected in the
    !> returned character code if possible (e.g. C-SPC yields 0 and C-a yields 97).
    !> If some of the modifiers cannot be reflected in the character code, the
    !> returned value will include those modifiers, and will not be a valid
    !> character code: it will fail the `characterp' test.  Use `event_basic_type'
    !> to recover the character code with the modifiers removed.
    !>
    !> If the optional argument PROMPT is present, display that as a prompt.
    !>
    !> If the optional argument SECONDS is present, it should be a number
    !> specifying the maximum number of seconds to wait for input.  If no
    !> input arrives in that time, return false in HAVE.  SECONDS may be a
    !> floating-point value.
    !>
    !> If `inhibit_interaction' is true, this function will signal an
    !> `inhibited_interaction' error.  Port of
    !> keyboard.c:Fread_char_exclusive (no input methods).
    recursive subroutine read_char_exclusive(c, have, status, prompt, seconds)
        integer(int64), intent(out) :: c
        logical, intent(out) :: have
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: prompt
        real, intent(in), optional :: seconds
        type(value_type) :: ev

        c = -1
        have = .false.
        call barf_if_interaction_inhibited(status)
        if (.not. status%ok()) return
        call show_read_prompt(prompt)
        call read_filtered_event(.true., .false., ev, have, status, seconds)
        if (.not. status%ok() .or. .not. have) return
        have = fixnump(ev)
        if (have) c = int(char_resolve_modifier_mask(int(xfixnum(ev), int32)), int64)
    end subroutine read_char_exclusive

    !> Read a key sequence as `read_key_sequence' does, as the commands
    !> and the functions read-key-sequence and read-key-sequence-vector
    !> do: unless CONTINUE_ECHO, `this_command_keys' starts afresh; PROMPT
    !> begins the echo; DONT_DOWNCASE_LAST keeps an upper-case last event
    !> that was looked up in lower case.  Port of
    !> keyboard.c:read_key_sequence_vs with keyboard.c:Fread_key_sequence
    !> and keyboard.c:Fread_key_sequence_vector (keys are arrays of
    !> events; no switch-frame events or input methods).
    recursive subroutine read_key_sequence_vs(keys, binding, eof, status, prompt, continue_echo, dont_downcase_last)
        type(value_type), allocatable, intent(out) :: keys(:)
        type(value_type), intent(out) :: binding
        logical, intent(out) :: eof
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: prompt
        logical, intent(in), optional :: continue_echo, dont_downcase_last
        logical :: cont

        cont = .false.
        if (present(continue_echo)) cont = continue_echo

        if (.not. cont) then
            call set_command_key_count(0)
            this_single_command_key_start = 0
        end if

        raw_keybuf_count = 0
        call read_key_sequence(keys, binding, eof, status, prompt, dont_downcase_last)
    end subroutine read_key_sequence_vs

    ! ------------------------------------------------------------------
    ! Event symbols
    ! ------------------------------------------------------------------

    !> The modifier bit that symbol SYMBOL names by itself (A, alt, C,
    !> ctrl, control, click, H, hyper, M, meta, S, shift, s, super, drag,
    !> down, double, triple, up), or 0.  Port of
    !> keyboard.c:parse_solitary_modifier.
    integer(int64) function parse_solitary_modifier(symbol) result(bit)
        type(value_type), intent(in) :: symbol

        bit = 0
        if (.not. symbolp(symbol)) return

        select case (symbols(xsymbol(symbol))%name)
        case ("A", "alt")
            bit = int(CHAR_ALT, int64)
        case ("C", "ctrl", "control")
            bit = int(CHAR_CTL, int64)
        case ("click")
            bit = M_CLICK
        case ("H", "hyper")
            bit = int(CHAR_HYPER, int64)
        case ("M", "meta")
            bit = M_META
        case ("S", "shift")
            bit = M_SHIFT
        case ("s", "super")
            bit = int(CHAR_SUPER, int64)
        case ("drag")
            bit = M_DRAG
        case ("down")
            bit = M_DOWN
        case ("double")
            bit = M_DOUBLE
        case ("triple")
            bit = M_TRIPLE
        case ("up")
            bit = M_UP
        case default
            bit = 0
        end select
    end function parse_solitary_modifier

    !> The list of modifier symbols for the bits MODIFIERS, the highest
    !> bit first: (meta control) for C-M-.  Port of
    !> keyboard.c:lispy_modifier_list.
    function lispy_modifier_list(modifiers) result(v)
        integer(int64), intent(in) :: modifiers
        type(value_type) :: v
        character(len=7), parameter :: names(0:27) = [character(len=7) :: "up", "down", "drag", "click", "double", &
                                                                         "triple", "", "", "", "", "", "", "", "", "", "", "", "", &
                                                                      "", "", "", "", "alt", "super", "hyper", "shift", "control", &
                                                                                                                        "meta"]
        type(value_type) :: syms(28)
        integer :: i, n

        n = 0

        do i = 27, 0, -1
            if (len_trim(names(i)) == 0) cycle
            if (.not. btest(modifiers, i)) cycle
            n = n + 1
            syms(n) = sym(trim(names(i)))
        end do

        v = make_list(syms(1:n))
    end function lispy_modifier_list

    !> Parse the event symbol.  For internal use.  The value is the list
    !> (BASE MODIFIERS...), as the `event_symbol_elements' property of
    !> SYMBOL upstream.  Port of keyboard.c:Fevent_symbol_parse_modifiers.
    function event_symbol_parse_modifiers(symbol) result(v)
        type(value_type), intent(in) :: symbol
        type(value_type) :: v
        type(value_type), allocatable :: mods(:), elts(:)
        character(len=:), allocatable :: base
        integer(int64) :: bits

        call parse_modifiers(symbol, bits, base)
        call list_elements(lispy_modifier_list(bits), mods)
        allocate (elts(size(mods) + 1))
        elts(1) = sym(base)
        if (size(mods) > 0) elts(2:) = mods
        v = make_list(elts)
    end function event_symbol_parse_modifiers

    !> Apply the control modifier to character code C.  Port of
    !> keyboard.c:make_ctrl_char.
    integer(int64) function make_ctrl_char(c_in) result(c)
        integer(int64), intent(in) :: c_in
        integer(int64) :: upper, oc

        c = c_in
        ! Save the upper bits here.
        upper = iand(c, not(127_int64))

        if (c < 0 .or. c > 127) then
            c = ior(c, int(CHAR_CTL, int64))
            return
        end if

        c = iand(c, 127_int64)

        if (c >= 64 .and. c < 96) then
            ! Everything in the columns containing the upper-case letters
            ! denotes a control character.
            oc = c
            c = iand(c, not(96_int64))
            ! Set the shift modifier for a control char made from a shifted
            ! letter.  But only for letters!
            if (oc >= iachar("A") .and. oc <= iachar("Z")) c = ior(c, M_SHIFT)
        else if (c >= iachar("a") .and. c <= iachar("z")) then
            ! The lower-case letters denote control characters too.
            c = iand(c, not(96_int64))
        else if (c >= iachar(" ")) then
            ! Include the bits for control and shift only if the basic
            ! ASCII code can't indicate them.
            c = ior(c, int(CHAR_CTL, int64))
        end if

        ! Replace the high bits.
        c = ior(c, iand(upper, not(int(CHAR_CTL, int64))))
    end function make_ctrl_char

    !> Convert the event description list EVENT_DESC to an event type.
    !> EVENT_DESC should contain one base event type (a character or symbol)
    !> and zero or more modifier names (control, meta, hyper, super, shift, alt,
    !> drag, down, double or triple).  The base must be last.
    !>
    !> The return value is an event type (a character or symbol) which has
    !> essentially the same base event type and all the specified modifiers.
    !> (Some compatibility base types, like symbols that represent a
    !> character, are not returned verbatim.)  Port of
    !> keyboard.c:Fevent_convert_list (EVENT_DESC as an array).
    subroutine event_convert_list(event_desc, ev, status)
        type(value_type), intent(in) :: event_desc(:)
        type(value_type), intent(out) :: ev
        type(status_type), intent(out) :: status
        type(value_type) :: base
        character(len=:), allocatable :: name
        integer(int64) :: modifiers, this, c
        integer :: k

        modifiers = 0
        base = nil()

        do k = 1, size(event_desc)
            this = 0
            ! Given a symbol, see if it is a modifier name.
            if (symbolp(event_desc(k)) .and. k < size(event_desc)) this = parse_solitary_modifier(event_desc(k))

            if (this /= 0) then
                modifiers = ior(modifiers, this)
            else if (.not. nilp(base)) then
                call set_error(status, FM_GENERIC_ERROR, "Two bases given in one event")
                return
            else
                base = event_desc(k)
            end if
        end do

        ! Let the symbol A refer to the character A.
        if (symbolp(base) .and. .not. nilp(base)) then
            name = symbols(xsymbol(base))%name
            if (len(name) == 1) base = make_fixnum(int(iachar(name), int64))
        end if

        if (fixnump(base)) then
            c = xfixnum(base)

            ! Turn (shift a) into A.
            if (iand(modifiers, M_SHIFT) /= 0 .and. c >= iachar("a") .and. c <= iachar("z")) then
                c = c - (iachar("a") - iachar("A"))
                modifiers = iand(modifiers, not(M_SHIFT))
            end if

            ! Turn (control a) into C-a.
            if (iand(modifiers, int(CHAR_CTL, int64)) /= 0) then
                ev = make_fixnum(ior(iand(modifiers, not(int(CHAR_CTL, int64))), make_ctrl_char(c)))
            else
                ev = make_fixnum(ior(modifiers, c))
            end if
        else if (symbolp(base) .and. .not. nilp(base)) then
            name = symbols(xsymbol(base))%name
            ev = apply_modifiers(modifiers, name)
        else
            call set_error(status, FM_GENERIC_ERROR, "Invalid base event")
        end if
    end subroutine event_convert_list

    !> The `ascii_character' property of function-key symbol EVENT: the
    !> character a keypad or other key with a character equivalent stands
    !> for, or -1 (nil).  The properties bindings.el and simple.el's
    !> keypad support give.  Not a port.
    integer(int64) function ascii_character(event) result(c)
        type(value_type), intent(in) :: event
        character(len=:), allocatable :: name

        c = -1
        if (.not. symbolp(event)) return
        name = symbols(xsymbol(event))%name

        if (len(name) == 4) then
            if (name(1:3) == "kp-" .and. verify(name(4:4), "0123456789") == 0) then
                c = iachar(name(4:4))
                return
            end if
        end if

        select case (name)
        case ("kp-decimal")
            c = iachar(".")
        case ("kp-add")
            c = iachar("+")
        case ("kp-subtract")
            c = iachar("-")
        case ("kp-multiply")
            c = iachar("*")
        case ("kp-divide")
            c = iachar("/")
        case ("kp-space")
            c = iachar(" ")
        case ("kp-tab", "tab")
            c = 9
        case ("kp-enter", "return")
            c = 13
        case ("kp-separator")
            c = iachar(",")
        case ("kp-equal")
            c = iachar("=")
        case ("backspace", "delete")
            c = 127
        case ("linefeed")
            c = 10
        case ("clear")
            c = 12
        case ("escape")
            c = 27
        case default
            c = -1
        end select
    end function ascii_character

    ! ------------------------------------------------------------------
    ! Hooks and the top level
    ! ------------------------------------------------------------------

    !> Run HOOK's functions, catching their errors: an error is shown as
    !> "Error in HOOK (FUNCTION): ERROR" and the function that signaled it
    !> is removed from the hook (from the current buffer's own functions
    !> if it is one of them, else from the default ones), the other
    !> functions still running.  Port of keyboard.c:safe_run_hooks, with
    !> keyboard.c:safe_run_hook_funcall, keyboard.c:safe_run_hooks_1 and
    !> keyboard.c:safe_run_hooks_error (the error as its symbol and
    !> message, a status having no other data).
    recursive subroutine safe_run_hooks(hook)
        type(hook_type), intent(inout) :: hook

        call hook%run_safe(report_hook_error)
    end subroutine safe_run_hooks

    !> Run HOOK as `safe_run_hooks' does, for pre-command-hook and
    !> post-command-hook.  Port of keyboard.c:safe_run_hooks_maybe_narrowed
    !> (no long-line optimizations: their narrowing is Deferred with
    !> xdisp.c).
    recursive subroutine safe_run_hooks_maybe_narrowed(hook)
        type(hook_type), intent(inout) :: hook

        call safe_run_hooks(hook)
    end subroutine safe_run_hooks_maybe_narrowed

    !> Show the error STATUS of FN, a function of the hook named HOOK_NAME
    !> (safe_run_hooks_error's message).  Not a port.
    subroutine report_hook_error(hook_name, fn, status)
        character(len=*), intent(in) :: hook_name
        procedure(command_proc) :: fn
        type(status_type), intent(in) :: status
        character(len=:), allocatable :: name, err

        name = command_name_of(fn)
        err = "("//error_name(status%code)
        if (allocated(status%detail)) err = err//" """//status%detail//""""
        err = err//")"
        last_message = "Error in "//trim(hook_name)//" ("//name//"): "//err
        if (associated(message_log_hook)) call message_log_hook(last_message)
    end subroutine report_hook_error

    !> Exit all recursive editing levels.
    !> This also exits all active minibuffers.  Port of
    !> keyboard.c:Ftop_level: the throw to `top_level' is the status
    !> `FM_TOP_LEVEL', which the recursive edits pass on and the top-level
    !> command loop takes without a message.
    subroutine top_level(status)
        type(status_type), intent(out) :: status

        call set_error(status, FM_TOP_LEVEL)

        if (recursion_depth() > 0) then
            ! Each recursive edit ends once the current command returns.
            call throw_exit(.true., recursion_depth())
            top_level_pending = .true.
        end if
    end subroutine top_level

    !> Invoke the editor command loop recursively.
    !> To get out of the recursive edit, a command can throw to `exit' --
    !> for instance (throw \='exit nil).
    !>
    !> The following values (last argument to `throw') can be used when
    !> throwing to \='exit:
    !>
    !> - t causes `recursive_edit' to quit, so that control returns to the
    !> command loop one level up.
    !>
    !> - A string causes `recursive_edit' to signal an error, printing that
    !> string as the error message.
    !>
    !> - False causes `recursive_edit' to return normally to the
    !> function that called it.
    !>
    !> This function is called by the editor initialization to begin editing.
    !> Port of keyboard.c:Frecursive_edit (with
    !> keyboard.c:recursive_edit_unwind).
    recursive subroutine recursive_edit(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: buffer
        type(status_type) :: st
        logical :: restore

        restore = .not. selected_window_shows_current()
        if (restore) buffer = current_buffer()
        command_loop_level = command_loop_level + 1
        call recursive_edit_1(status)
        ! recursive_edit_unwind
        if (restore) call set_buffer(buffer, st)
        command_loop_level = command_loop_level - 1
    end subroutine recursive_edit

    !> Return or set the maximum number of keystrokes to save.
    !> If called with ARG, set the limit to ARG and return it.
    !> Otherwise, return the current limit.
    !>
    !> The saved keystrokes are shown by `view_lossage'.  Port of
    !> keyboard.c:Flossage_size.
    integer function lossage_size(arg, status) result(size_now)
        integer, intent(in), optional :: arg
        type(status_type), intent(out), optional :: status
        type(status_type) :: st
        character(len=24) :: buf
        integer :: kept

        size_now = lossage_limit
        if (present(status)) status = st
        if (.not. present(arg)) return

        if (arg < 0) then
            call set_error(st, FM_USER_ERROR, "Value must be a positive integer")
        else if (arg /= lossage_limit .and. arg < MIN_NUM_RECENT_KEYS) then
            write (buf, "(i0)") MIN_NUM_RECENT_KEYS
            call set_error(st, FM_USER_ERROR, "Value must be >= "//trim(buf))
        else if (arg > MAX_NUM_RECENT_KEYS) then
            write (buf, "(i0)") MAX_NUM_RECENT_KEYS
            call set_error(st, FM_USER_ERROR, "Value must be <= "//trim(buf))
        else if (arg /= lossage_limit) then
            if (arg > lossage_limit) then
                kept = total_keys
            else
                kept = min(arg, total_keys)
            end if

            call update_recent_keys(arg, kept)
        end if

        if (present(status)) status = st
        size_now = lossage_limit
    end function lossage_size

    !> Make the ring of recent keys NEW_SIZE long, keeping its last
    !> KEPT_KEYS entries.  Port of keyboard.c:update_recent_keys.
    subroutine update_recent_keys(new_size, kept_keys)
        integer, intent(in) :: new_size, kept_keys
        type(value_type), allocatable :: v(:)
        logical, allocatable :: is_cmd(:)
        integer :: i, idx

        allocate (v(new_size), is_cmd(new_size))
        is_cmd = .false.

        if (allocated(recent_events)) then
            do i = 1, kept_keys
                idx = recent_keys_index - 1 - kept_keys + (i - 1)

                do while (idx < 0)
                    idx = idx + size(recent_events)
                end do

                v(i) = recent_events(idx + 1)
                is_cmd(i) = recent_is_command(idx + 1)
            end do
        end if

        call move_alloc(v, recent_events)
        call move_alloc(is_cmd, recent_is_command)
        total_keys = kept_keys
        recent_keys_index = modulo(total_keys, new_size) + 1
        lossage_limit = new_size
    end subroutine update_recent_keys

    !> What keyboard.c:command_loop_1 does before its loop: no prefix
    !> argument, echoing off, no keys of a command, post-command-hook run
    !> and `last_command' set.  Port of keyboard.c:command_loop_1 (its
    !> start).
    recursive subroutine command_loop_1_start()
        prefix_arg = nil()
        last_prefix_arg = nil()
        deactivate_mark_variable = .false.
        call cancel_echoing()
        call set_command_key_count(0)
        this_single_command_key_start = 0
        ! Make sure this hook runs after commands that get errors and throw
        ! to top level.
        call safe_run_hooks_maybe_narrowed(post_command_hook)
        ! Do this after running Vpost_command_hook, for consistency.
        last_command = this_command
        real_last_command = real_this_command
        if (vtag(last_command_event) /= V_LIST) last_repeatable_command = real_this_command
    end subroutine command_loop_1_start
end module keyboard_fm
