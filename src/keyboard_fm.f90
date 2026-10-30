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
!> Not ported yet: echoing, the echo area and `message' (the last message
!> is kept in `last_message'), idle hooks, mouse and
!> switch-frame events, the (t . EVENT) and (no-record . EVENT) forms of
!> `unread_command_events', `help_char', case
!> tables (upper-case fallback is ASCII only), quit while computing (C-g
!> is an ordinary key), `ring_bell_function', the `minibuffer_quit'
!> condition (a quit in the minibuffer is handled as any quit) and
!> `minibuffer_error_function'.
module keyboard_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use error_fm, only: status_type, set_error, format_error, FM_GENERIC_ERROR, FM_QUIT, FM_USER_ERROR
    use handle_fm, only: handle_type, null_handle
    use data_fm, only: value_eq
    use lisp_fm, only: value_type, V_LIST, nil, make_fixnum, &
                       make_keymap_ref, vtag, nilp, fixnump, symbolp, &
                       stringp, xfixnum, xsymbol, xstring, list_elements
    use lread_fm, only: intern, sym
    use character_fm, only: CHAR_SHIFT, CHAR_META, CHAR_MODIFIER_MASK, &
                            bytes_by_char_head, byte8_to_char, &
                            multibyte_length, string_char_and_length
    use keymap_fm, only: keymapp, keymap_live_p, access_keymap, &
                         current_active_maps, &
                         key_description, parse_modifiers, apply_modifiers, &
                         overriding_terminal_local_map, pushed_terminal_local_map, keymap_events, lookup_key, command_remapping
    use callint_fm, only: call_interactively, funcall, current_prefix_arg, prefix_arg, &
                          prefix_numeric_value, this_command
    use buffer_fm, only: buffers, cur, deactivate_mark_variable, transient_mark_mode
    use undo_fm, only: undo_auto_add_boundary, point_before_last_command_or_undo, &
                       buffer_before_last_command_or_undo
    use sysdep_fm, only: emacs_read, input_available, interrupt_pending, inputs_available
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
              discard_input, input_pending_p, read_event, unread_command_events_p
    public :: read_key_sequence, command_loop_step, command_loop, &
              request_exit, exit_requested_p, set_transient_map, recursive_edit_1, &
              throw_exit, throw_exit_string, exit_recursive_edit, abort_recursive_edit, sit_for
    public :: exit_transient_map
    public :: add_read_fd, delete_read_fd, insert_special_event, run_special_events, special_events_pending_p, &
              read_fd_proc, special_event_proc
    public :: execute_kbd_macro, ring_bell, bitch_at_user, executing_kbd_macro_p
    public :: start_kbd_macro, end_kbd_macro, call_last_kbd_macro, store_kbd_macro_char, store_kbd_macro_event, &
              finalize_kbd_macro_chars, cancel_kbd_macro_events, finish_kbd_macro, defining_kbd_macro_p, &
              syms_of_macros
    public :: recent_keys, lossage_size, message_log_proc
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

    !> keyboard.c:lossage_limit, the number of events and commands
    !> `recent_keys' keeps (3 * MIN_NUM_RECENT_KEYS).
    integer, parameter :: LOSSAGE_LIMIT = 300

    !> keyboard.c:recent_keys, a ring of the last LOSSAGE_LIMIT input
    !> events not from keyboard macros, and of the commands run, as
    !> upstream's pseudo-events (nil . COMMAND): `recent_is_command' marks
    !> those, whose command (nil for an undefined key) is in
    !> `recent_events'.  The next entry goes at `recent_keys_index'
    !> (1-based); `total_keys' entries are filled.
    type(value_type) :: recent_events(LOSSAGE_LIMIT)
    logical :: recent_is_command(LOSSAGE_LIMIT) = .false.
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
    integer, parameter, public :: DEFINING_KBD_MACRO_NIL = 0, DEFINING_KBD_MACRO_T = 1, DEFINING_KBD_MACRO_APPEND = 2

    !> Not `DEFINING_KBD_MACRO_NIL' while a keyboard macro is being defined.
    !> Don't set this!
    !> The value is `DEFINING_KBD_MACRO_APPEND' while appending to the
    !> definition of an existing macro.  Port of macros.c defining-kbd-macro
    !> (DEFINING_KBD_MACRO_NIL for nil, DEFINING_KBD_MACRO_T for t).
    integer, public :: defining_kbd_macro = DEFINING_KBD_MACRO_NIL

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
    logical function defining_kbd_macro_p()
        defining_kbd_macro_p = defining_kbd_macro /= DEFINING_KBD_MACRO_NIL
    end function defining_kbd_macro_p

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
    !> keyboard.c:tty_read_avail_input.
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
        if (defining_kbd_macro /= DEFINING_KBD_MACRO_NIL) then
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
    logical function unread_command_events_p()
        unread_command_events_p = nunread > 0
    end function unread_command_events_p

    !> Whether input is waiting.  Port of keyboard.c:Finput_pending_p.
    logical function input_pending_p()
        input_pending_p = nunread > 0 .or. tail >= head
        if (input_pending_p .or. input_fd < 0) return
        input_pending_p = input_available(input_fd, 0_int32)
    end function input_pending_p

    !> The next input event.  Waits for terminal input if there is a
    !> terminal; otherwise `have' is false when the queue is empty (end of
    !> input).  The event becomes `last_input_event' and, unless it comes
    !> from a keyboard macro being executed, goes into the keyboard macro
    !> being defined.  With SECONDS, wait at most that long (also without
    !> a terminal), handling special events, timers and subprocess output
    !> meanwhile; `have' is then false on a timeout (read-event's
    !> SECONDS).  Port of keyboard.c:read_char (its core) and
    !> keyboard.c:record_char (its macro part).
    recursive subroutine read_event(ev, have, status, seconds)
        type(value_type), intent(out) :: ev
        logical, intent(out) :: have
        type(status_type), intent(out) :: status
        real, intent(in), optional :: seconds

        call read_event_1(ev, have, status, seconds)
        if (.not. have) return
        ! Now that we have read an event, Emacs is not idle.
        call timer_stop_idle()
        last_input_event = ev
        if (.not. macro_active) call store_kbd_macro_char(ev)
        ! recent_keys should not include events from keyboard macros.
        if (.not. macro_active) call record_recent(ev, .false.)
    end subroutine read_event

    !> Store `ev' (an event, or the command run if `command') into the
    !> ring of recent keys.  Port of keyboard.c:record_char (the
    !> recent_keys part; no help-echo or mouse-movement events) and
    !> keyboard.c:command_loop_1 in part.
    subroutine record_recent(ev, command)
        type(value_type), intent(in) :: ev
        logical, intent(in) :: command

        if (total_keys < LOSSAGE_LIMIT) total_keys = total_keys + 1
        recent_events(recent_keys_index) = ev
        recent_is_command(recent_keys_index) = command
        recent_keys_index = recent_keys_index + 1
        if (recent_keys_index > LOSSAGE_LIMIT) recent_keys_index = 1
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

        if (total_keys < LOSSAGE_LIMIT) then
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
            if (i > LOSSAGE_LIMIT) i = 1
        end do
    end subroutine recent_keys

    !> Return the maximum number of keystrokes and commands recorded by
    !> `recent_keys'.  Port of keyboard.c:Flossage_size (no ARG: the size
    !> can't be changed yet).
    integer function lossage_size()
        lossage_size = LOSSAGE_LIMIT
    end function lossage_size

    !> The next input event (read_event without recording it).  Not a port.
    recursive subroutine read_event_1(ev, have, status, seconds)
        type(value_type), intent(out) :: ev
        logical, intent(out) :: have
        type(status_type), intent(out) :: status
        real, intent(in), optional :: seconds
        integer(int64) :: nread, start, now, rate
        integer(int32) :: timeout

        have = .false.
        call system_clock(start, rate)

        reading: do
            call run_special_events()

            if (nunread > 0) then
                ev = unread(1)
                unread(1:nunread - 1) = unread(2:nunread)
                unread(nunread) = nil()
                nunread = nunread - 1
                have = .true.
                return
            end if

            ! If we're executing a macro, process it unless we are at its
            ! end.
            if (macro_active) then
                if (.not. at_end_of_macro_p()) then
                    executing_kbd_macro_index = executing_kbd_macro_index + 1
                    ev = macro_events(executing_kbd_macro_index)

                    if (macro_string .and. fixnump(ev)) then
                        if (iand(xfixnum(ev), 128_int64) /= 0 .and. xfixnum(ev) <= 255) then
                            ev = make_fixnum(ior(M_META, iand(xfixnum(ev), not(128_int64))))
                        end if
                    end if

                    have = .true.
                    return
                end if
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
            ! input it reads as the quit character (keyboard.c:
            ! handle_interrupt's quit_throw_to_read_char).
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

        if (keymap_live_p(km)) v = make_keymap_ref(km)
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
    !> keyboard.c:keyremap_step (translation functions are not supported).
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
        if (keymapp(fk%parent)) next = access_keymap(fk%map, key, .true., .false.)

        if (doit .and. (stringp(next) .or. (vtag(next) == V_LIST .and. &
                                            .not. keymapp(next)))) then
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

        if (keymapp(next)) then
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
    !> `keys' then holds what was read.  Port of
    !> keyboard.c:read_key_sequence (no echoing, mouse or frame events).
    subroutine read_key_sequence(keys, binding, eof, status)
        type(value_type), allocatable, intent(out) :: keys(:)
        type(value_type), intent(out) :: binding
        logical, intent(out) :: eof
        type(status_type), intent(out) :: status
        type(value_type) :: keybuf(READ_KEY_ELTS)
        type(value_type) :: current_binding, new_binding, key, &
                            original_uppercase
        type(keyremap_type) :: indec, fkey, keytran
        character(len=:), allocatable :: base
        integer :: t, mock_input, first_unbound, diff, i, &
                   original_uppercase_position
        integer(int64) :: k, c, bits
        logical :: have, done, shift_translated, macro_end

        eof = .false.
        t = 0
        mock_input = 0
        original_uppercase_position = -1
        shift_translated = .false.
        indec%parent = map_value(input_decode_map)
        fkey%parent = map_value(local_function_key_map)
        keytran%parent = map_value(key_translation_map)
        call reset_remap(indec, 0)
        call reset_remap(fkey, 0)
        call reset_remap(keytran, 0)

        replay: do
            first_unbound = READ_KEY_ELTS + 1
            current_binding = current_active_maps()
            t = 0

            keyloop: do
                if (nilp(current_binding)) then
                    if (keytran%start >= t) exit replay
                else if (.not. keymapp(current_binding)) then
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
                if (macro_active .and. nunread == 0) macro_end = at_end_of_macro_p()

                if (t < mock_input) then
                    key = keybuf(t + 1)
                else if (macro_end) then
                    ! If we're at the end of a macro, exit it by returning
                    ! 0, unless there are unread events pending.
                    t = 0
                    eof = .true.
                    exit replay
                else
                    call read_event(key, have, status)
                    if (.not. status%ok()) return

                    if (.not. have) then
                        eof = .true.
                        exit replay
                    end if
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
                if (.not. keymapp(current_binding) .and. &
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

                ! An undefined upper-case or shifted character: try it without
                ! the shift.
                if (nilp(current_binding) .and. keytran%start >= t .and. &
                    fixnump(key)) then
                    k = xfixnum(key)
                    c = iand(k, not(M_MASK))

                    if (iand(k, M_SHIFT) /= 0) then
                        k = k - M_SHIFT
                    else if (c >= iachar("A") .and. c <= iachar("Z")) then
                        k = k + 32
                    else
                        cycle keyloop
                    end if

                    original_uppercase = key
                    original_uppercase_position = t - 1
                    keybuf(t) = make_fixnum(k)
                    mock_input = max(t, mock_input)
                    shift_translated = .true.
                    cycle replay
                end if

                ! An undefined shifted function key: try it unshifted.
                if (nilp(current_binding) .and. keytran%start >= t .and. &
                    symbolp(key)) then
                    call parse_modifiers(key, bits, base)

                    if (iand(bits, M_SHIFT) /= 0) then
                        original_uppercase = key
                        original_uppercase_position = t - 1
                        keybuf(t) = apply_modifiers(bits - M_SHIFT, base)
                        mock_input = max(t, mock_input)
                        call reset_remap(fkey, 0)
                        call reset_remap(keytran, 0)
                        shift_translated = .true.
                        cycle replay
                    end if
                end if
            end do keyloop
        end do replay

        ! An undefined key keeps its original case.
        if (nilp(current_binding) .and. t > 0 .and. &
            t - 1 == original_uppercase_position) then
            keybuf(t) = original_uppercase
            shift_translated = .false.
        end if

        this_command_keys_shift_translated = shift_translated
        binding = current_binding
        if (keymapp(binding)) binding = nil()
        allocate (keys(t))
        keys = keybuf(1:t)
        if (.not. eof) num_input_keys = num_input_keys + 1
    end subroutine read_key_sequence

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
    !> Not a port.
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
        integer(int64) :: start, now, rate, left
        integer(int32) :: timeout, next_timer

        if (associated(redisplay_hook)) call redisplay_hook()

        if (input_fd >= 0) then
            if (input_pending_p()) return
        end if

        call system_clock(start, rate)

        do
            call run_special_events()
            call system_clock(now)
            left = int(ms, int64) - (now - start)*1000/max(rate, 1_int64)
            if (left < 0) return
            if (input_fd < 0 .and. .not. associated(process_output_hook) .and. n_read_fds() == 0) return
            timeout = int(left, int32)
            next_timer = run_timers()
            if (next_timer >= 0) timeout = min(timeout, next_timer)
            if (wait_for_input(timeout)) return
        end do
    end subroutine sit_for

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
    !> events, and of subr.el:insert-special-event.
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
    logical function special_events_pending_p()
        special_events_pending_p = n_special_events > 0
    end function special_events_pending_p

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
    logical function exit_requested_p()
        exit_requested_p = exit_requested
    end function exit_requested_p

    !> Whether the execution of a macro has reached its end.
    !> This should be called only while executing a macro.  Port of
    !> macros.c:at_end_of_macro_p.
    logical function at_end_of_macro_p()
        at_end_of_macro_p = macro_forced_end .or. .not. allocated(macro_events)

        if (.not. at_end_of_macro_p) then
            at_end_of_macro_p = executing_kbd_macro_index >= size(macro_events, kind=int64)
        end if
    end function at_end_of_macro_p

    !> Whether a keyboard macro is executing (`executing_kbd_macro'
    !> true).  Not a port.
    logical function executing_kbd_macro_p()
        executing_kbd_macro_p = macro_active
    end function executing_kbd_macro_p

    !> Make the selected window's buffer current (set_buffer_internal
    !> (XBUFFER (XWINDOW (selected_window)->contents)) in
    !> keyboard.c:command_loop_1).  Not a port.
    subroutine select_window_buffer()
        use frame_fm, only: selected_window
        use window_fm, only: windows, window_live_p
        use buffer_fm, only: set_buffer, buffer_live_p
        type(handle_type) :: sw
        type(status_type) :: st

        sw = selected_window()
        if (.not. window_live_p(sw)) return
        if (buffer_live_p(windows(sw%index)%buffer)) call set_buffer(windows(sw%index)%buffer, st)
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
    !> keyboard.c:cmd_error with Fcommand_error_default_function.
    subroutine cmd_error(status)
        type(status_type), intent(in) :: status
        type(status_type) :: st
        character(len=:), allocatable :: macroerror
        character(len=24) :: buf

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
        logical :: eof, handled
        procedure(command_wrapper_proc), pointer :: wrapper
        type(status_type) :: hook_status
        integer(int64) :: last_point_position, prev_serial, prev_modiff

        done = .false.
        ! Make sure the current window's buffer is selected.
        call select_window_buffer()
        this_command = nil()
        real_this_command = nil()
        call read_key_sequence(keys, cmd, eof, status)

        if (.not. status%ok()) then
            if (loop_handles_errors) call cmd_error(status)
            return
        end if

        if (eof .and. size(keys) == 0) then
            done = .true.
            return
        end if

        ! A new command clears the echo area (read_char wipes it, except
        ! for events from a keyboard macro).
        if (.not. macro_active) then
            if (allocated(last_message)) last_message = ""
        end if

        ! minibuffer--delete-message-overlay runs from pre-command-hook.
        if (allocated(minibuffer_message_text)) deallocate (minibuffer_message_text)

        ! read_char's clear_message runs clear-message-function.
        if (.not. macro_active) then
            if (allocated(set_minibuffer_message_text)) deallocate (set_minibuffer_message_text)
        end if

        ! Input ended in the middle of a key sequence: it is unbound.
        if (eof) cmd = nil()

        ! Remap command through active keymaps (read_key_sequence_remapped).
        if (symbolp(cmd) .and. .not. nilp(cmd)) then
            call command_remapping(cmd, remapped)
            if (.not. nilp(remapped)) cmd = remapped
        end if

        last_command_event = keys(size(keys))
        this_command_keys = keys
        ! Execute the command: recorded in recent_keys as (nil . cmd).
        call record_recent(cmd, .true.)
        this_command = cmd
        real_this_command = cmd
        ! pre-command-hook: a transient map lasts for one key sequence,
        ! unless KEEP-PRED keeps it.
        if (transient_active) call clear_transient_map(keys)
        call pre_command_hook%run(hook_status)
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
            last_command = this_command
            real_last_command = real_this_command
            if (vtag(last_command_event) /= V_LIST) last_repeatable_command = real_this_command
            call post_command_hook%run(hook_status)
            done = eof
            return
        end if

        last_prefix_arg = current_prefix_arg
        last_command = this_command
        real_last_command = real_this_command
        if (vtag(last_command_event) /= V_LIST) last_repeatable_command = real_this_command
        ! post-command-hook
        call post_command_hook%run(hook_status)

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
        if (defining_kbd_macro /= DEFINING_KBD_MACRO_NIL .and. nilp(prefix_arg)) call finalize_kbd_macro_chars()
        done = eof
    end subroutine command_loop_step

    !> Whether the selected window shows the current buffer.  Not a port.
    logical function selected_window_shows_current() result(shows)
        use frame_fm, only: selected_window
        use window_fm, only: windows, window_live_p
        type(handle_type) :: sw

        shows = .false.
        sw = selected_window()
        if (.not. window_live_p(sw)) return
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
    logical function display_prop_intangible_p(prop) result(replaces)
        type(value_type), intent(in) :: prop
        type(value_type), allocatable :: elts(:)

        replaces = stringp(prop)
        if (replaces .or. vtag(prop) /= V_LIST) return
        call list_elements(prop, elts)
        if (size(elts) > 0) replaces = value_eq(elts(1), sym("space"))
    end function display_prop_intangible_p

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
                if (.not. nilp(val)) ok = display_prop_intangible_p(val)

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
                    ellipsis = ellipsis .or. inv > 1 .or. overlay_strings_p(overlay)
                    end = next_single_char_property_change(end, sym("invisible"))
                end do

                do while (beg > begv)
                    call get_char_property_and_overlay(beg - 1, sym("invisible"), val, overlay)
                    if (nilp(val)) exit
                    inv = text_prop_means_invisible(val)
                    if (inv == 0) exit
                    ellipsis = ellipsis .or. inv > 1 .or. overlay_strings_p(overlay)
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
                if (.not. nilp(val)) shown = display_prop_intangible_p(val)
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
        logical function overlay_strings_p(ov)
            type(handle_type), intent(in) :: ov

            overlay_strings_p = .false.
            if (ov%index == 0) return
            overlay_strings_p = .not. nilp(overlay_get(ov, "after_string")) .or. &
                                .not. nilp(overlay_get(ov, "before_string"))
        end function overlay_strings_p
    end subroutine adjust_point_for_property

    !> Run commands until input ends or `request_exit' is called.  Command
    !> errors don't stop the loop.  Port of keyboard.c:command_loop_1.
    recursive subroutine command_loop(status)
        type(status_type), intent(out) :: status
        type(status_type) :: step_status
        logical :: done, saved_handles

        exit_requested = .false.
        saved_handles = loop_handles_errors
        loop_handles_errors = .true.

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
    !> Port of keyboard.c:recursive_edit_1 with Frecursive_edit's catch.
    recursive subroutine recursive_edit_1(status)
        type(status_type), intent(out) :: status
        type(status_type) :: step_status
        logical :: done, saved_handles, macro_end

        command_loop_level = command_loop_level + 1
        exit_pending = .false.
        saved_handles = loop_handles_errors
        loop_handles_errors = .true.
        macro_end = .false.
        exit_levels = 1
        if (allocated(exit_string)) deallocate (exit_string)

        do
            call command_loop_step(done, step_status)
            if (done .and. macro_active) macro_end = at_end_of_macro_p()
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
        else if (.not. exit_pending .or. exit_abort) then
            call set_error(status, FM_QUIT)
        end if

        command_loop_level = command_loop_level - 1

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

    !> Leave the innermost recursive edit or minibuffer.
    !> Port of keyboard.c:Fexit_recursive_edit.
    subroutine exit_recursive_edit(status)
        type(status_type), intent(out) :: status

        if (command_loop_level > 0) then
            call throw_exit(.false.)
        else
            call set_error(status, FM_USER_ERROR, "No recursive edit is in progress")
        end if
    end subroutine exit_recursive_edit

    !> Abort the command that started the innermost recursive edit or
    !> minibuffer.  Port of keyboard.c:Fabort_recursive_edit.
    subroutine abort_recursive_edit(status)
        type(status_type), intent(out) :: status

        if (command_loop_level > 0) then
            call throw_exit(.true.)
        else
            call set_error(status, FM_USER_ERROR, "No recursive edit is in progress")
        end if
    end subroutine abort_recursive_edit
end module keyboard_fm
