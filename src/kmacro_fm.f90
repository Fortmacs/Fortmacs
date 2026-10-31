! kmacro_fm.f90 -- enhanced keyboard macros.
! Ported from GNU Emacs 31.1 lisp/kmacro.el.
!
! Copyright (C) 2002-2026 Free Software Foundation, Inc.
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

!> The user interface to keyboard macros (macros.c is the submodule
!> macros_fm of keyboard_fm): C-x ( starts defining a macro, C-x ) ends
!> it and C-x e calls it (then e repeats it); <f3> starts a macro or
!> inserts the macro counter, <f4> ends or calls it; C-x C-k is
!> `kmacro_keymap' (the counter, the macro ring, viewing and the commands
!> that repeat on their last key).
!>
!> A macro of the ring is a `kmacro_type' (upstream's `kmacro' oclosure:
!> its keys, counter and format); calling one runs it with its own
!> counter and format, as calling the oclosure does.  The head of the
!> ring is `last_kbd_macro' with `kmacro_counter' and
!> `kmacro_counter_format_start'.
!>
!> Fortran has no anonymous commands: the key that repeats a macro after
!> C-x e (upstream an anonymous procedure in a transient keymap) runs the command
!> `kmacro_call_macro_repeat'.
!>
!> Deferred: editing macros (`kmacro_edit_macro', `kmacro_edit_macro_repeat',
!> `kmacro_edit_lossage', `edit_kbd_macro': C-x C-k C-e, RET, e and l),
!> step editing (`kmacro_step_edit_macro', C-x C-k SPC,
!> `kmacro_step_edit_mini_window_height'), naming and binding macros
!> (`kmacro_name_last_macro', `kmacro_bind_to_key': C-x C-k n and b, the
!> keyboard macros as commands they make), `kmacro_to_register' (C-x C-k
!> x: register values that are macros), `apply_macro_to_region_lines'
!> (C-x C-k r) and `kbd_macro_query' (C-x C-k q, C-x q) from macros.el,
!> the Keyboard Macro Menu (`list_keyboard_macros'), mouse events
!> (`kmacro_call_mouse_event', S-mouse-3, the EVENT of
!> `kmacro_end_and_call_macro', `kmacro_end_call_mouse'), a key name as
!> `kmacro_call_repeat_key', repeat-mode, the obsolete
!> `kmacro_lambda_form', `kmacro_extract_lambda' and
!> `kmacro_exec_ring_item', and the obsolete list form of ring elements.
module kmacro_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use callint_fm, only: prefix_arg_type, PREFIX_NONE, PREFIX_NUMBER, PREFIX_RAW, PREFIX_MINUS
    use commands_fm, only: commands
    use error_fm, only: status_type, set_error, FM_GENERIC_ERROR, FM_USER_ERROR, FM_WRONG_TYPE_ARGUMENT
    use handle_fm, only: handle_type
    use lisp_fm, only: value_type, command_type
    use data_fm, only: value_eq
    use keymap_fm, only: make_sparse_keymap, define_key, key_binding, is_keymap_live, key_parse, free_keymap
    use callint_fm, only: current_prefix_arg, prefix_numeric_value, find_function
    use buffer_fm, only: is_buffer_live, buffers
    use marker_fm, only: marker_position
    use insdel_fm, only: insert
    use keyboard_fm, only: defining_kbd_macro, DEFINING_KBD_MACRO_NO, DEFINING_KBD_MACRO_APPEND, last_kbd_macro, &
                           is_executing_kbd_macro, start_kbd_macro, end_kbd_macro, execute_kbd_macro, this_command, &
                           real_this_command, last_command, this_command_keys, last_input_event, last_message, &
                           read_event, unread_command_events, set_transient_map, redisplay_hook, is_defining_kbd_macro, &
                           clear_this_command_keys
    use xdisp_fm, only: message, add_minor_mode_lighter
    use print_fm, only: fortran_string, marker_repr
    use subr_fm, only: read_number
    use minibuf_fm, only: read_string
    use register_fm, only: register_read_with_preview, get_register, set_register, register_contents_type, REGISTER_EMPTY, &
                           REGISTER_MARKER, REGISTER_TEXT, REGISTER_RECTANGLE, REGISTER_NUMBER, &
                           REGISTER_WINDOW_CONFIGURATION, REGISTER_FILE, REGISTER_FILE_QUERY, REGISTER_BUFFER
    use rect_fm, only: format_integer
    use edmacro_fm, only: format_kbd_macro
    use simple_fm, only: kmacro_keyboard_quit_hook
    implicit none(type, external)
    private

    public :: kmacro_insert_counter, kmacro_set_format, kmacro_display_counter, kmacro_set_counter, kmacro_add_counter, &
              kmacro_reg_load_counter, kmacro_reg_save_counter, kmacro_reg_add_counter, kmacro_quit_counter, &
              kmacro_loop_setup_function, kmacro_ring_head, kmacro_push_ring, kmacro_split_ring_element, &
              kmacro_pop_ring1, kmacro_pop_ring, is_kmacro_ring_empty, kmacro_display, kmacro_repeat_on_last_key, &
              kmacro_call_ring_2nd, kmacro_call_ring_2nd_repeat, kmacro_view_ring_2nd, kmacro_cycle_ring_next, &
              kmacro_cycle_ring_previous, kmacro_swap_ring, kmacro_delete_ring_head, kmacro_start_macro, &
              kmacro_end_macro, kmacro_call_macro, kmacro_start_macro_or_insert_counter, kmacro_end_or_call_macro, &
              kmacro_end_or_call_macro_repeat, kmacro_end_and_call_macro, kmacro_view_macro, &
              kmacro_view_macro_repeat, kmacro_redisplay, kmacro_keyboard_quit, kmacro_call, kmacro, syms_of_kmacro

    !> Values of `kmacro_call_repeat_key' other than a character:
    !> KMACRO_CALL_REPEAT_DISABLED and KMACRO_CALL_REPEAT_LAST_KEY (the last key).
    integer, parameter, public :: KMACRO_CALL_REPEAT_DISABLED = -1, KMACRO_CALL_REPEAT_LAST_KEY = -2

    !> Values of `kmacro_reg_add_counter''s and `kmacro_quit_counter''s
    !> PRED: =, < and >.
    integer, parameter, public :: KMACRO_PRED_EQUAL = 0, KMACRO_PRED_LESS = 1, KMACRO_PRED_GREATER = 2

    !> Values of `kmacro_call_macro''s NO_REPEAT: KMACRO_REPEAT_YES, KMACRO_REPEAT_NO and KMACRO_REPEAT_REPEATING.
    integer, parameter, public :: KMACRO_REPEAT_YES = 0, KMACRO_REPEAT_NO = 1, KMACRO_REPEAT_REPEATING = 2

    !> Keyboard macro: its KEYS, COUNTER and FORMAT (upstream's `kmacro'
    !> oclosure).  ID tells macros apart, as the identity of the
    !> oclosure does: running a macro updates its counter in the ring.
    type, public :: kmacro_type
        type(value_type), allocatable :: keys(:)
        integer(int64) :: counter = 0
        character(len=:), allocatable :: format
        integer(int64) :: id = 0
    end type kmacro_type

    !> Maximum number of keyboard macros to save in macro ring.  Port of
    !> kmacro.el kmacro-ring-max.
    integer(int64), public :: kmacro_ring_max = 8

    !> Controls whether appending to a macro starts by executing the macro.
    !> If true, using a single \[universal_argument] prefix executes the macro
    !> before appending, while more than one \[universal_argument] prefix does not
    !> execute the macro.
    !> Otherwise, a single \[universal_argument] prefix does not execute the
    !> macro, while more than one \[universal_argument] prefix causes the
    !> macro to be executed before appending to it.  Port of kmacro.el
    !> kmacro-execute-before-append.
    logical, public :: kmacro_execute_before_append = .true.

    !> Allow repeating certain macro commands without entering the \[kmacro_keymap] prefix.
    !> Port of kmacro.el kmacro-repeat-no-prefix.
    logical, public :: kmacro_repeat_no_prefix = .true.

    !> Allow repeating macro call using last key or a specific key.
    !> In Fortran: KMACRO_CALL_REPEAT_DISABLED (none),
    !> KMACRO_CALL_REPEAT_LAST_KEY (the last key) or a character.  Port of
    !> kmacro.el kmacro-call-repeat-key (a key name is Deferred).
    integer, public :: kmacro_call_repeat_key = KMACRO_CALL_REPEAT_LAST_KEY

    !> Repeat macro call with original arg when true; repeat once if false.
    !> Port of kmacro.el kmacro-call-repeat-with-arg.
    logical, public :: kmacro_call_repeat_with_arg = .false.

    !> Current keyboard macro counter.
    !>
    !> This is normally initialized to zero when the macro is defined,
    !> and incremented each time the value of the counter is inserted
    !> into a buffer.  See `kmacro_start_macro_or_insert_counter' for
    !> more details.  Port of kmacro.el kmacro-counter.
    integer(int64), public :: kmacro_counter = 0

    !> The counter format of the macros defined from now on.  Port of
    !> kmacro.el kmacro-default-counter-format.
    character(len=:), allocatable, public :: kmacro_default_counter_format

    !> Current keyboard macro counter format.
    !>
    !> Can be set directly via `kmacro_set_format', which see.  Port of
    !> kmacro.el kmacro-counter-format.
    character(len=:), allocatable, public :: kmacro_counter_format

    !> Macro format at start of macro execution.  Port of kmacro.el
    !> kmacro-counter-format-start.
    character(len=:), allocatable, public :: kmacro_counter_format_start

    !> Macro counter at start of macro execution.  Port of kmacro.el
    !> kmacro-counter-value-start.
    integer(int64), public :: kmacro_counter_value_start = 0

    !> Last counter inserted by key macro.  Port of kmacro.el
    !> kmacro-last-counter.
    integer(int64), public :: kmacro_last_counter = 0

    !> Initial counter value for the next keyboard macro to be defined.
    !> Not allocated for none.  Port of kmacro.el
    !> kmacro-initial-counter-value.
    integer(int64), allocatable, public :: kmacro_initial_counter_value

    !> The keyboard macro ring.
    !> Each element is a `kmacro_type'.  Actually, the head of
    !> the macro ring (when defining or executing) is not stored in the ring;
    !> instead it is available in the variables `last_kbd_macro', `kmacro_counter',
    !> and `kmacro_counter_format'.  Port of kmacro.el kmacro-ring.
    type(kmacro_type), allocatable, public :: kmacro_ring(:)

    !> Keymap for keyboard macro commands.  Port of kmacro.el kmacro-keymap.
    type(handle_type), public :: kmacro_keymap

    !> What `kmacro_view_macro' is looking at: element KMACRO_VIEW_LAST_ITEM
    !> of the ring onwards (0 for none, upstream's tail of the ring), the
    !> KMACRO_VIEW_ITEM_NO'th previous macro.
    integer :: kmacro_view_last_item = 0
    integer(int64) :: kmacro_view_item_no = 0

    !> The last `kmacro_type' id given out.
    integer(int64) :: last_kmacro_id = 0

    !> The transient keymap of the repeat key, and the arguments of the
    !> call it repeats (the procedure's ARG and MACRO, if `repeat_has_macro').
    type(handle_type) :: repeat_map
    type(value_type) :: repeat_key_bound
    type(prefix_arg_type) :: repeat_arg
    type(value_type), allocatable :: repeat_macro(:)
    logical :: repeat_has_macro = .false.

    !> The `kmacro_repeat' property of the commands that repeat on their
    !> last key: `head', `ring' and `stop'.
    integer, parameter :: REPEAT_NONE = 0, REPEAT_HEAD = 1, REPEAT_RING = 2, REPEAT_STOP = 3

contains

    ! ------------------------------------------------------------------
    ! The macro counter
    ! ------------------------------------------------------------------

    !> Give the format variables their standard values ("%d") the first
    !> time.  Not a port.
    subroutine ensure_formats()
        if (.not. allocated(kmacro_default_counter_format)) kmacro_default_counter_format = "%d"
        if (.not. allocated(kmacro_counter_format)) kmacro_counter_format = "%d"
        if (.not. allocated(kmacro_counter_format_start)) kmacro_counter_format_start = kmacro_counter_format
    end subroutine ensure_formats

    !> Take up `kmacro_initial_counter_value', if set, as the counter.
    !> Not a port.
    subroutine take_initial_counter_value()
        if (allocated(kmacro_initial_counter_value)) then
            kmacro_counter = kmacro_initial_counter_value
            deallocate (kmacro_initial_counter_value)
        end if
    end subroutine take_initial_counter_value

    !> Insert current value of `kmacro_counter', then increment it by ARG.
    !> Interactively, ARG defaults to 1.  With \[universal_argument], insert
    !> the previous value of `kmacro_counter', and do not increment the
    !> current value.
    !>
    !> The previous value of the counter is the one it had before
    !> the last increment.  Port of kmacro.el:kmacro-insert-counter (ARG
    !> is the raw prefix argument).
    recursive subroutine kmacro_insert_counter(arg, status)
        type(prefix_arg_type), intent(in) :: arg
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: text

        call ensure_formats()
        call take_initial_counter_value()

        if (arg%kind == PREFIX_RAW) then
            call format_integer(kmacro_counter_format, kmacro_last_counter, text, status)
            if (status%ok()) call insert(text, status)
        else
            call format_integer(kmacro_counter_format, kmacro_counter, text, status)
            if (status%ok()) call insert(text, status)
            if (status%ok()) call kmacro_add_counter(prefix_numeric_value(arg), status)
        end if
    end subroutine kmacro_insert_counter

    !> Set the format of `kmacro_counter' to FORMAT.
    !>
    !> The default format is "%d", which means to insert the number in
    !> decimal without any padding.  You can specify any format string
    !> that the `format' function accepts and that makes sense with a
    !> single integer extra argument.
    !>
    !> If you run this command while no keyboard macro is being defined,
    !> the new format affects all subsequent macro definitions.
    !>
    !> If you run this command while defining a keyboard macro, it
    !> affects only that macro, from that point on.
    !>
    !> Do not put the format string inside double quotes when you insert
    !> it in the minibuffer.  Port of kmacro.el:kmacro-set-format.
    subroutine kmacro_set_format(format)
        character(len=*), intent(in) :: format

        call ensure_formats()

        if (len(format) == 0) then
            kmacro_counter_format = "%d"
        else
            kmacro_counter_format = format
        end if

        ! Redefine initial macro counter if we are not executing a macro.
        if (.not. (is_defining_kbd_macro() .or. is_executing_kbd_macro())) then
            kmacro_default_counter_format = kmacro_counter_format
        end if
    end subroutine kmacro_set_format

    !> Display current counter value (or VALUE).  Port of
    !> kmacro.el:kmacro-display-counter.
    subroutine kmacro_display_counter(status, value)
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: value
        character(len=:), allocatable :: text
        integer(int64) :: v

        call ensure_formats()
        v = kmacro_counter
        if (present(value)) v = value
        call format_integer(kmacro_counter_format, v, text, status)
        if (status%ok()) call message("New macro counter value: "//text//" ("//int_text(v)//")")
    end subroutine kmacro_display_counter

    !> Set the value of `kmacro_counter' to ARG, or prompt for value if no argument.
    !> With \[universal_argument] prefix, reset counter to its value prior to this iteration of the
    !> macro.  Port of kmacro.el:kmacro-set-counter.
    subroutine kmacro_set_counter(arg, status)
        integer(int64), intent(in) :: arg
        type(status_type), intent(out) :: status

        status = status_type()

        if (.not. (is_defining_kbd_macro() .or. is_executing_kbd_macro())) then
            kmacro_initial_counter_value = arg
            call kmacro_display_counter(status, kmacro_initial_counter_value)
        else
            kmacro_last_counter = kmacro_counter

            if (current_prefix_arg%kind == PREFIX_RAW) then
                kmacro_counter = kmacro_counter_value_start
            else
                kmacro_counter = arg
            end if

            if (.not. is_executing_kbd_macro()) call kmacro_display_counter(status)
        end if
    end subroutine kmacro_set_counter

    !> Add the value of numeric prefix arg (prompt if missing) to `kmacro_counter'.
    !> With \[universal_argument], restore previous counter value.  Port of
    !> kmacro.el:kmacro-add-counter.
    recursive subroutine kmacro_add_counter(arg, status)
        integer(int64), intent(in) :: arg
        type(status_type), intent(out) :: status
        integer(int64) :: last

        status = status_type()
        call take_initial_counter_value()
        last = kmacro_last_counter
        kmacro_last_counter = kmacro_counter

        if (current_prefix_arg%kind == PREFIX_RAW) then
            kmacro_counter = last
        else
            kmacro_counter = kmacro_counter + arg
        end if

        if (.not. is_executing_kbd_macro()) call kmacro_display_counter(status)
    end subroutine kmacro_add_counter

    !> Load the value of a REGISTER into `kmacro_counter'.  Port of
    !> kmacro.el:kmacro-reg-load-counter.
    subroutine kmacro_reg_load_counter(register)
        integer(int64), intent(in) :: register
        type(register_contents_type) :: v

        call get_register(register, v)
        if (v%kind == REGISTER_NUMBER) kmacro_counter = v%number
    end subroutine kmacro_reg_load_counter

    !> Save the value of `kmacro_counter' to a REGISTER.  Port of
    !> kmacro.el:kmacro-reg-save-counter.
    subroutine kmacro_reg_save_counter(register)
        integer(int64), intent(in) :: register

        call set_register(register, kmacro_counter)
    end subroutine kmacro_reg_save_counter

    !> Increment `kmacro_counter' by ARG if PRED returns true.
    !> PRED (KMACRO_PRED_EQUAL, KMACRO_PRED_LESS or KMACRO_PRED_GREATER) is
    !> called with two arguments: `kmacro_counter' and REGISTER.  Port of
    !> kmacro.el:kmacro-reg-add-counter.
    recursive subroutine kmacro_reg_add_counter(pred, register, arg, status)
        integer, intent(in) :: pred
        integer(int64), intent(in) :: register, arg
        type(status_type), intent(out) :: status
        integer(int64) :: n

        call register_as_number(register, n, status)
        if (.not. status%ok()) return

        if (compare(pred, kmacro_counter, n)) then
            current_prefix_arg = prefix_arg_type()
            call kmacro_add_counter(arg, status)
        end if
    end subroutine kmacro_reg_add_counter

    !> The contents of REGISTER as a number or marker position, as the
    !> arithmetic comparisons take them (else a `wrong_type_argument' error condition occurs).
    !> Not a port.
    subroutine register_as_number(register, n, status)
        integer(int64), intent(in) :: register
        integer(int64), intent(out) :: n
        type(status_type), intent(out) :: status
        type(register_contents_type) :: v

        status = status_type()
        n = 0
        call get_register(register, v)

        select case (v%kind)
        case (REGISTER_NUMBER)
            n = v%number
        case (REGISTER_MARKER)
            call marker_position(v%marker, n, status)
        case default
            call set_error(status, FM_WRONG_TYPE_ARGUMENT, &
                           "expected integer, real or type(marker_type), got "//register_value_repr(v))
        end select
    end subroutine register_as_number

    !> The printed representation of register contents V.  Not a port.
    function register_value_repr(v) result(s)
        type(register_contents_type), intent(in) :: v
        character(len=:), allocatable :: s
        character(len=:), allocatable :: name
        type(status_type) :: st
        integer :: k

        select case (v%kind)
        case (REGISTER_EMPTY)
            s = ".false."
        case (REGISTER_TEXT)
            s = fortran_string(v%text)
        case (REGISTER_RECTANGLE)
            s = "["

            do k = 1, size(v%rectangle)
                if (k > 1) s = s//", "
                s = s//fortran_string(v%rectangle(k)%text)
            end do

            s = s//"]"
        case (REGISTER_WINDOW_CONFIGURATION)
            s = "(#<window-configuration> "//marker_repr(v%marker)//")"
        case (REGISTER_FILE)
            s = "(file . "//fortran_string(v%text)//")"
        case (REGISTER_FILE_QUERY)
            s = "(file-query "//fortran_string(v%text)//" "//int_text(v%number)//")"
        case (REGISTER_BUFFER)
            if (is_buffer_live(v%buffer)) then
                name = buffers(v%buffer%index)%name
                s = "(buffer . #<buffer "//name//">)"
            else
                s = "(buffer . #<killed buffer>)"
            end if
        case default
            s = ".false."
        end select

        st = status_type()
    end function register_value_repr

    !> PRED applied to A and B.  Not a port.
    logical function compare(pred, a, b)
        integer, intent(in) :: pred
        integer(int64), intent(in) :: a, b

        select case (pred)
        case (KMACRO_PRED_LESS)
            compare = a < b
        case (KMACRO_PRED_GREATER)
            compare = a > b
        case default
            compare = a == b
        end select
    end function compare

    !> Quit the keyboard macro if PRED returns true.
    !> PRED (KMACRO_PRED_EQUAL, KMACRO_PRED_LESS or KMACRO_PRED_GREATER) is
    !> called with two arguments: `kmacro_counter' and ARG.
    !> ARG defaults to zero if there is no prefix argument.  Port of
    !> kmacro.el:kmacro-quit-counter.
    recursive subroutine kmacro_quit_counter(pred, arg, status)
        integer, intent(in) :: pred
        integer(int64), intent(in) :: arg
        type(status_type), intent(out) :: status
        integer(int64) :: a

        status = status_type()
        call take_initial_counter_value()
        a = arg
        if (current_prefix_arg%kind == PREFIX_NONE) a = 0

        if (compare(pred, kmacro_counter, a)) then
            if (associated(commands%keyboard_quit%run)) call commands%keyboard_quit%run(status)
        end if
    end subroutine kmacro_quit_counter

    !> Function called prior to each iteration of macro.  Port of
    !> kmacro.el:kmacro-loop-setup-function.
    logical function kmacro_loop_setup_function()
        call ensure_formats()
        ! Restore macro counter format to initial format, so it is ok to change
        ! counter format in the macro without restoring it.
        kmacro_counter_format = kmacro_counter_format_start
        ! Save initial counter value so we can restore it with C-u kmacro-set-counter.
        kmacro_counter_value_start = kmacro_counter
        ! Return true to continue execution.
        kmacro_loop_setup_function = .true.
    end function kmacro_loop_setup_function

    ! ------------------------------------------------------------------
    ! The macro ring
    ! ------------------------------------------------------------------

    !> Create a `kmacro_type' for macro bound to name or key.
    !> KEYS is an array of events; COUNTER defaults to 0 and FORMAT to
    !> "%d".  Port of kmacro.el:kmacro (KEYS a vector).
    function kmacro(keys, counter, format) result(m)
        type(value_type), intent(in) :: keys(:)
        integer(int64), intent(in), optional :: counter
        character(len=*), intent(in), optional :: format
        type(kmacro_type) :: m

        m%keys = keys
        m%counter = 0
        if (present(counter)) m%counter = counter
        m%format = "%d"
        if (present(format)) m%format = format
        last_kmacro_id = last_kmacro_id + 1
        m%id = last_kmacro_id
    end function kmacro

    !> Run the keyboard macro M with ARG (a repeat count), with its own
    !> counter and format, and keep the counter it ends with in M (and
    !> in the ring element that is M).  Port of kmacro.el:kmacro (the
    !> function of its oclosure).
    recursive subroutine kmacro_call(m, arg, status)
        type(kmacro_type), intent(inout) :: m
        type(prefix_arg_type), intent(in) :: arg
        type(status_type), intent(out) :: status
        type(value_type), allocatable :: keys(:)
        integer(int64) :: saved_counter
        character(len=:), allocatable :: saved_format
        integer :: k

        call ensure_formats()
        ! Use counter and format specific to the macro on the ring!
        saved_counter = kmacro_counter
        saved_format = kmacro_counter_format_start
        kmacro_counter = m%counter
        kmacro_counter_format_start = m%format
        keys = m%keys
        call execute_kbd_macro(keys, arg, status, kmacro_loop_setup_function)

        if (status%ok()) then
            m%counter = kmacro_counter

            if (allocated(kmacro_ring)) then
                do k = 1, size(kmacro_ring)
                    if (kmacro_ring(k)%id == m%id) kmacro_ring(k)%counter = m%counter
                end do
            end if
        end if

        kmacro_counter = saved_counter
        kmacro_counter_format_start = saved_format
    end subroutine kmacro_call

    !> Run element K of the ring (call it).  Not a port.
    recursive subroutine call_ring_element(k, arg, status)
        integer, intent(in) :: k
        type(prefix_arg_type), intent(in) :: arg
        type(status_type), intent(out) :: status
        type(kmacro_type) :: m

        m = kmacro_ring(k)
        call kmacro_call(m, arg, status)
    end subroutine call_ring_element

    !> Return pseudo head element in macro ring (`found' false if
    !> `last_kbd_macro' is unallocated).  Port of kmacro.el:kmacro-ring-head.
    subroutine kmacro_ring_head(head, found)
        type(kmacro_type), intent(out) :: head
        logical, intent(out) :: found

        call ensure_formats()
        found = allocated(last_kbd_macro)
        if (found) head = kmacro(last_kbd_macro, kmacro_counter, kmacro_counter_format_start)
    end subroutine kmacro_ring_head

    !> The number of macros in the ring.  Not a port.
    integer function ring_size()
        ring_size = 0
        if (allocated(kmacro_ring)) ring_size = size(kmacro_ring)
    end function ring_size

    !> Whether macros A and B are `equal'.  Not a port.
    logical function kmacro_equal(a, b)
        type(kmacro_type), intent(in) :: a, b
        integer :: k

        kmacro_equal = size(a%keys) == size(b%keys) .and. a%counter == b%counter .and. a%format == b%format
        if (.not. kmacro_equal) return

        do k = 1, size(a%keys)
            if (.not. value_eq(a%keys(k), b%keys(k))) then
                kmacro_equal = .false.
                return
            end if
        end do
    end function kmacro_equal

    !> Push ELT or current macro onto `kmacro_ring'.  Port of
    !> kmacro.el:kmacro-push-ring (with subr.el add-to-history,
    !> `history_delete_duplicates' false).
    subroutine kmacro_push_ring(elt)
        type(kmacro_type), intent(in), optional :: elt
        type(kmacro_type) :: e
        type(kmacro_type), allocatable :: ring(:)
        logical :: found
        integer :: n, k

        if (present(elt)) then
            e = elt
        else
            call kmacro_ring_head(e, found)
            if (.not. found) return
        end if

        n = ring_size()

        if (n > 0) then
            if (kmacro_equal(kmacro_ring(1), e)) return
        end if

        if (kmacro_ring_max <= 0) then
            allocate (ring(0))
        else
            allocate (ring(min(int(kmacro_ring_max), n + 1)))
            ring(1) = e

            do k = 2, size(ring)
                ring(k) = kmacro_ring(k - 1)
            end do
        end if

        call move_alloc(ring, kmacro_ring)
    end subroutine kmacro_push_ring

    !> Make ELT the head of the ring: `last_kbd_macro', `kmacro_counter'
    !> and `kmacro_counter_format_start'.  Port of
    !> kmacro.el:kmacro-split-ring-element.
    subroutine kmacro_split_ring_element(elt)
        type(kmacro_type), intent(in) :: elt

        last_kbd_macro = elt%keys
        kmacro_counter = elt%counter
        kmacro_counter_format_start = elt%format
    end subroutine kmacro_split_ring_element

    !> Remove the first element of the ring.  Not a port.
    subroutine drop_first()
        type(kmacro_type), allocatable :: ring(:)
        integer :: k, n

        n = ring_size()
        allocate (ring(max(0, n - 1)))

        do k = 2, n
            ring(k - 1) = kmacro_ring(k)
        end do

        call move_alloc(ring, kmacro_ring)
    end subroutine drop_first

    !> Pop head element off macro ring (no check).
    !> True arg RAW means just return raw first element.  Port of
    !> kmacro.el:kmacro-pop-ring1.
    subroutine kmacro_pop_ring1(raw)
        logical, intent(in) :: raw
        type(kmacro_type) :: first

        if (ring_size() == 0) then
            if (.not. raw) then
                ! (kmacro-split-ring-element nil) sets the head to nil.
                if (allocated(last_kbd_macro)) deallocate (last_kbd_macro)
                kmacro_counter = 0
                kmacro_counter_format_start = "%d"
            end if

            return
        end if

        first = kmacro_ring(1)
        if (.not. raw) call kmacro_split_ring_element(first)
        call drop_first()
    end subroutine kmacro_pop_ring1

    !> Pop head element off macro ring.
    !> True arg RAW means just return raw first element.  Port of
    !> kmacro.el:kmacro-pop-ring.
    subroutine kmacro_pop_ring(raw)
        logical, intent(in) :: raw

        if (.not. is_kmacro_ring_empty()) call kmacro_pop_ring1(raw)
    end subroutine kmacro_pop_ring

    !> Tell user and return true if `last_kbd_macro' is unallocated or `kmacro_ring' is empty.
    !> Check only `last_kbd_macro' if optional arg NONE is true.  Port of
    !> kmacro.el:kmacro-ring-empty-p.
    logical function is_kmacro_ring_empty(none)
        logical, intent(in), optional :: none
        logical :: only_last

        only_last = .false.
        if (present(none)) only_last = none

        do while (.not. allocated(last_kbd_macro) .and. ring_size() > 0)
            call kmacro_pop_ring1(.false.)
        end do

        is_kmacro_ring_empty = .true.

        if (.not. allocated(last_kbd_macro)) then
            call message("No keyboard macro defined.")
        else if (.not. only_last .and. ring_size() == 0) then
            call message("Only one keyboard macro defined.")
        else
            is_kmacro_ring_empty = .false.
        end if
    end function is_kmacro_ring_empty

    !> Display a keyboard MACRO.
    !> Optional arg TRUNC true specifies to limit width of macro to 60 chars.
    !> Optional arg DESCR is descriptive text for macro; default is "Macro".
    !> Optional arg EMPTY is message to print if no macros are defined
    !> (MACRO not present).  Port of kmacro.el:kmacro-display.
    recursive subroutine kmacro_display(status, macro, trunc, descr, empty)
        type(status_type), intent(out) :: status
        type(value_type), intent(in), optional :: macro(:)
        logical, intent(in), optional :: trunc
        character(len=*), intent(in), optional :: descr, empty
        character(len=:), allocatable :: m, d, counter_part
        integer, parameter :: x = 60
        logical :: z

        status = status_type()
        call ensure_formats()

        if (present(macro)) then
            m = format_kbd_macro(macro)
            z = .false.
            if (present(trunc)) z = trunc .and. nchars(m) > x
            d = "Macro"
            if (present(descr)) d = descr
            counter_part = ""

            if (kmacro_counter /= 0) then
                call format_integer(kmacro_counter_format_start, kmacro_counter, counter_part, status)
                if (.not. status%ok()) return
                counter_part = " ["//counter_part//"]"
            end if

            if (z) then
                call message(d//counter_part//": "//first_chars(m, x - 1)//"...")
            else
                call message(d//counter_part//": "//m)
            end if
        else if (present(empty)) then
            call message(empty)
        else
            call message("No keyboard macros defined")
        end if
    end subroutine kmacro_display

    !> `kmacro_display' of `last_kbd_macro' (nothing if not allocated).
    !> Not a port.
    recursive subroutine display_last(status, trunc, descr, empty)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: trunc
        character(len=*), intent(in), optional :: descr, empty
        type(value_type), allocatable :: macro(:)

        if (allocated(last_kbd_macro)) then
            macro = last_kbd_macro
            call kmacro_display(status, macro, trunc, descr, empty)
        else
            call kmacro_display(status, trunc=trunc, descr=descr, empty=empty)
        end if
    end subroutine display_last

    !> The `kmacro_repeat' property of command CMD.  Not a port.
    integer function repeat_property(cmd)
        type(value_type), intent(in) :: cmd

        repeat_property = REPEAT_NONE
        if (value_eq(cmd, value_type(name="kmacro_end_or_call_macro_repeat"))) repeat_property = REPEAT_HEAD
        if (value_eq(cmd, value_type(name="kmacro_call_ring_2nd_repeat"))) repeat_property = REPEAT_HEAD
        if (value_eq(cmd, value_type(name="kmacro_delete_ring_head"))) repeat_property = REPEAT_HEAD
        if (value_eq(cmd, value_type(name="kmacro_cycle_ring_next"))) repeat_property = REPEAT_RING
        if (value_eq(cmd, value_type(name="kmacro_cycle_ring_previous"))) repeat_property = REPEAT_RING
        if (value_eq(cmd, value_type(name="kmacro_view_macro_repeat"))) repeat_property = REPEAT_RING
    end function repeat_property

    !> Call command CMD with no prefix argument and `kmacro_repeat_no_prefix'
    !> .false., both saved and restored around the call.  Not a port.
    recursive subroutine call_without_prefix(cmd, status)
        type(value_type), intent(in) :: cmd
        type(status_type), intent(out) :: status
        type(prefix_arg_type) :: saved_arg
        logical :: saved_no_prefix
        type(command_type), pointer :: fn

        saved_arg = current_prefix_arg
        saved_no_prefix = kmacro_repeat_no_prefix
        current_prefix_arg = prefix_arg_type()
        kmacro_repeat_no_prefix = .false.
        call find_function(cmd, fn, status)
        if (status%ok()) call fn%run(status)
        kmacro_repeat_no_prefix = saved_no_prefix
        current_prefix_arg = saved_arg
    end subroutine call_without_prefix

    !> Process kmacro commands keys immediately after cycling the ring.
    !> Port of kmacro.el:kmacro-repeat-on-last-key (at the end of input it
    !> returns).
    recursive subroutine kmacro_repeat_on_last_key(keys, status)
        type(value_type), intent(in) :: keys(:)
        type(status_type), intent(out) :: status
        type(value_type), allocatable :: k(:)
        type(value_type) :: ev, cmd
        type(status_type) :: st
        logical :: done, have
        integer :: n

        status = status_type()
        k = keys
        n = size(k)
        done = .false.

        do
            if (.not. allocated(last_kbd_macro) .or. done) exit
            call read_event(ev, have, st)
            if (.not. have) return
            k(n) = ev
            call key_binding(k, cmd, st, accept_default=.true.)

            select case (repeat_property(cmd))
            case (REPEAT_RING)
                call clear_this_command_keys(keep_record=.true.)

                if (ring_size() > 0) then
                    call call_without_prefix(cmd, status)
                else
                    call display_last(status, trunc=.true.)
                end if
            case (REPEAT_HEAD)
                call clear_this_command_keys(keep_record=.true.)
                call call_without_prefix(cmd, status)
            case (REPEAT_STOP)
                call clear_this_command_keys(keep_record=.true.)
                call call_without_prefix(cmd, status)
                done = .true.
            case default
                exit
            end select

            if (.not. status%ok()) return
            last_input_event = value_type()
        end do

        if (allocated(last_input_event%a)) then
            call clear_this_command_keys(keep_record=.true.)
            call unread_command_events([last_input_event])
        end if
    end subroutine kmacro_repeat_on_last_key

    !> The keys of the command, when they can be repeated by their last
    !> key (`found').  Port of kmacro.el:kmacro-get-repeat-prefix.
    subroutine kmacro_get_repeat_prefix(keys, found)
        type(value_type), allocatable, intent(out) :: keys(:)
        logical, intent(out) :: found

        found = .false.
        if (.not. kmacro_repeat_no_prefix) return
        if (.not. allocated(this_command_keys)) return
        if (size(this_command_keys) <= 1) return
        keys = this_command_keys
        found = .true.
    end subroutine kmacro_get_repeat_prefix

    !> Execute second keyboard macro in macro ring.
    !> With numeric argument ARG, execute the macro that many times.
    !> Port of kmacro.el:kmacro-call-ring-2nd.
    recursive subroutine kmacro_call_ring_2nd(arg, status)
        type(prefix_arg_type), intent(in) :: arg
        type(status_type), intent(out) :: status

        status = status_type()
        if (.not. is_kmacro_ring_empty()) call call_ring_element(1, arg, status)
    end subroutine kmacro_call_ring_2nd

    !> Execute second keyboard macro in macro ring.
    !> This is like `kmacro_call_ring_2nd', but allows repeating macro commands
    !> without repeating the prefix.  Port of
    !> kmacro.el:kmacro-call-ring-2nd-repeat.
    recursive subroutine kmacro_call_ring_2nd_repeat(arg, status)
        type(prefix_arg_type), intent(in) :: arg
        type(status_type), intent(out) :: status
        type(value_type), allocatable :: keys(:)
        logical :: found

        call kmacro_get_repeat_prefix(keys, found)
        call kmacro_call_ring_2nd(arg, status)
        if (.not. status%ok()) return
        if (ring_size() > 0 .and. found) call kmacro_repeat_on_last_key(keys, status)
    end subroutine kmacro_call_ring_2nd_repeat

    !> Display the second macro in the keyboard macro ring.  Port of
    !> kmacro.el:kmacro-view-ring-2nd.
    subroutine kmacro_view_ring_2nd(status)
        type(status_type), intent(out) :: status
        type(value_type), allocatable :: keys(:)

        status = status_type()

        if (.not. is_kmacro_ring_empty()) then
            keys = kmacro_ring(1)%keys
            call kmacro_display(status, keys, descr="2nd macro")
        end if
    end subroutine kmacro_view_ring_2nd

    !> Move to next keyboard macro in keyboard macro ring.
    !> Displays the selected macro in the echo area.  Port of
    !> kmacro.el:kmacro-cycle-ring-next.
    recursive subroutine kmacro_cycle_ring_next(status)
        type(status_type), intent(out) :: status
        type(value_type), allocatable :: keys(:)
        type(kmacro_type), allocatable :: ring(:)
        type(kmacro_type) :: elt
        logical :: found
        integer :: n, k

        status = status_type()
        if (is_kmacro_ring_empty()) return
        call kmacro_push_ring()
        call kmacro_get_repeat_prefix(keys, found)
        n = ring_size()

        if (n < 2) then
            ! (kmacro-split-ring-element nil)
            call set_error(status, FM_WRONG_TYPE_ARGUMENT, "expected type(kmacro_type), got .false.")
            return
        end if

        elt = kmacro_ring(n)
        allocate (ring(n - 1))

        do k = 1, n - 1
            ring(k) = kmacro_ring(k)
        end do

        call move_alloc(ring, kmacro_ring)
        call kmacro_split_ring_element(elt)
        call display_last(status, trunc=.true.)
        if (.not. status%ok()) return
        if (found) call kmacro_repeat_on_last_key(keys, status)
    end subroutine kmacro_cycle_ring_next

    !> Move to previous keyboard macro in keyboard macro ring.
    !> Displays the selected macro in the echo area.  Port of
    !> kmacro.el:kmacro-cycle-ring-previous.
    recursive subroutine kmacro_cycle_ring_previous(status)
        type(status_type), intent(out) :: status
        type(value_type), allocatable :: keys(:)
        type(kmacro_type), allocatable :: ring(:)
        type(kmacro_type) :: cur
        logical :: found, have
        integer :: n, k

        status = status_type()
        if (is_kmacro_ring_empty()) return
        call kmacro_get_repeat_prefix(keys, found)
        call kmacro_ring_head(cur, have)
        call kmacro_pop_ring1(.false.)
        n = ring_size()
        allocate (ring(n + 1))

        do k = 1, n
            ring(k) = kmacro_ring(k)
        end do

        ring(n + 1) = cur
        call move_alloc(ring, kmacro_ring)
        call display_last(status, trunc=.true.)
        if (.not. status%ok()) return
        if (found) call kmacro_repeat_on_last_key(keys, status)
    end subroutine kmacro_cycle_ring_previous

    !> Swap first two elements on keyboard macro ring.  Port of
    !> kmacro.el:kmacro-swap-ring.
    subroutine kmacro_swap_ring(status)
        type(status_type), intent(out) :: status
        type(kmacro_type) :: cur
        logical :: have

        status = status_type()
        if (is_kmacro_ring_empty()) return
        call kmacro_ring_head(cur, have)
        call kmacro_pop_ring1(.false.)
        call kmacro_push_ring(cur)
        call display_last(status, trunc=.true.)
    end subroutine kmacro_swap_ring

    !> Delete current macro from keyboard macro ring.  Port of
    !> kmacro.el:kmacro-delete-ring-head.
    subroutine kmacro_delete_ring_head(status)
        type(status_type), intent(out) :: status

        status = status_type()
        if (is_kmacro_ring_empty(.true.)) return

        if (ring_size() == 0) then
            if (allocated(last_kbd_macro)) deallocate (last_kbd_macro)
        else
            call kmacro_pop_ring(.false.)
        end if

        call display_last(status, trunc=.true., empty="Keyboard macro ring is now empty.")
    end subroutine kmacro_delete_ring_head

    ! ------------------------------------------------------------------
    ! Traditional bindings
    ! ------------------------------------------------------------------

    !> Record subsequent keyboard input, defining a keyboard macro.
    !> The commands are recorded even as they are executed.
    !> Use \[kmacro_end_macro] to finish recording and make the macro available.
    !> Use \[kmacro_end_and_call_macro] to execute the macro.
    !>
    !> True arg (prefix arg) means append to last macro defined.
    !>
    !> With \[universal_argument] prefix, append to last keyboard macro
    !> defined.  Depending on `kmacro_execute_before_append', this may begin
    !> by re-executing the last macro as if you typed it again.
    !>
    !> Otherwise, it sets `kmacro_counter' to ARG or 0 if missing before
    !> defining the macro.
    !>
    !> Use \[kmacro_insert_counter] to insert (and increment) the macro counter.
    !> The counter value can be set or modified via \[kmacro_set_counter] and \[kmacro_add_counter].
    !> The format of the counter can be modified via \[kmacro_set_format].
    !> Port of kmacro.el:kmacro-start-macro (ARG is the raw prefix
    !> argument; naming and binding the macro are not ported).
    recursive subroutine kmacro_start_macro(arg, status)
        type(prefix_arg_type), intent(in) :: arg
        type(status_type), intent(out) :: status
        logical :: append, no_exec
        integer(int64) :: n

        status = status_type()
        call ensure_formats()

        if (is_defining_kbd_macro() .or. is_executing_kbd_macro()) then
            call message("Already defining keyboard macro.")
            return
        end if

        append = arg%kind == PREFIX_RAW

        if (.not. append) then
            if (allocated(last_kbd_macro)) call kmacro_push_ring()

            if (arg%kind /= PREFIX_NONE) then
                kmacro_counter = prefix_numeric_value(arg)
            else if (allocated(kmacro_initial_counter_value)) then
                kmacro_counter = kmacro_initial_counter_value
            else
                kmacro_counter = 0
            end if

            if (allocated(kmacro_initial_counter_value)) deallocate (kmacro_initial_counter_value)
            kmacro_counter_value_start = kmacro_counter
            kmacro_last_counter = kmacro_counter
            kmacro_counter_format = kmacro_default_counter_format
            kmacro_counter_format_start = kmacro_default_counter_format
        end if

        no_exec = .false.

        if (append) then
            n = prefix_numeric_value(arg)

            if (kmacro_execute_before_append) then
                no_exec = n > 4
            else
                no_exec = n == 4
            end if
        end if

        call start_kbd_macro(append, no_exec, status)
        if (.not. status%ok()) return
        if (is_defining_kbd_macro() .and. append) defining_kbd_macro = DEFINING_KBD_MACRO_APPEND
    end subroutine kmacro_start_macro

    !> Finish defining a keyboard macro.
    !> The definition was started by \[kmacro_start_macro].
    !> The macro is now available for use via \[kmacro_call_macro].
    !>
    !> With numeric ARG, repeat the macro that many times,
    !> counting the definition just completed as the first repetition.
    !> An argument of zero means repeat until error.  Port of
    !> kmacro.el:kmacro-end-macro (naming the macro is not ported).
    recursive subroutine kmacro_end_macro(arg, status)
        type(prefix_arg_type), intent(in) :: arg
        type(status_type), intent(out) :: status

        status = status_type()
        ! Isearch may push the kmacro-end-macro key sequence onto the macro.
        ! Just ignore it when executing the macro.
        if (is_executing_kbd_macro()) return
        call end_kbd_macro(arg, status, kmacro_loop_setup_function)
        if (.not. status%ok()) return

        if (allocated(last_kbd_macro)) then
            if (size(last_kbd_macro) == 0) then
                deallocate (last_kbd_macro)
                call message("Ignore empty macro")

                ! Don't call `kmacro-ring-empty-p' to avoid its messages.
                do while (.not. allocated(last_kbd_macro) .and. ring_size() > 0)
                    call kmacro_pop_ring1(.false.)
                end do
            end if
        end if
    end subroutine kmacro_end_macro

    !> Call the keyboard MACRO that you defined with \[kmacro_start_macro].
    !> A prefix argument serves as a repeat count.  Zero means repeat until error.
    !> MACRO defaults to `last_kbd_macro'.
    !>
    !> When you call the macro, you can call the macro again by repeating
    !> just the last key in the key sequence that you used to call this
    !> command.  See `kmacro_call_repeat_key' and `kmacro_call_repeat_with_arg'
    !> for details on how to adjust or disable this behavior.  Port of
    !> kmacro.el:kmacro-call-macro (ARG is the prefix argument, raw or
    !> numeric; REPEAT one of the KMACRO_REPEAT_ values).
    recursive subroutine kmacro_call_macro(arg, status, repeat, end_macro, macro)
        type(prefix_arg_type), intent(in) :: arg
        type(status_type), intent(out) :: status
        integer, intent(in), optional :: repeat
        logical, intent(in), optional :: end_macro
        type(value_type), intent(in), optional :: macro(:)
        type(value_type), allocatable :: saved(:), events(:), m(:)
        type(value_type) :: repeat_key, key(1)
        type(prefix_arg_type) :: a
        type(status_type) :: st
        character(len=:), allocatable :: times
        logical :: ending, had_saved
        integer :: nr

        status = status_type()
        nr = KMACRO_REPEAT_YES
        if (present(repeat)) nr = repeat
        ending = .false.
        if (present(end_macro)) ending = end_macro
        repeat_key = value_type()

        if (nr == KMACRO_REPEAT_REPEATING) then
            repeat_key = last_input_event
        else if (nr == KMACRO_REPEAT_YES .and. allocated(this_command_keys)) then
            if (size(this_command_keys) > 1) repeat_key = last_input_event
        end if

        if (ending) then
            ! Modifies last-kbd-macro.
            call kmacro_end_macro(arg, status)
            if (.not. status%ok()) return
        else
            ! (let ((last-kbd-macro (or macro last-kbd-macro))) ...)
            had_saved = allocated(last_kbd_macro)
            if (had_saved) saved = last_kbd_macro
            if (present(macro)) last_kbd_macro = macro
            this_command => last_command
            ! Upstream keeps the macro in `real_this_command', so that C-x z
            ! repeats it; a command pointer cannot hold a keyboard macro
            ! (Deferred).
            real_this_command => null()

            if (is_defining_kbd_macro()) then
                call set_error(status, FM_GENERIC_ERROR, "Can't execute anonymous macro while defining one")
            else if (.not. allocated(last_kbd_macro)) then
                call set_error(status, FM_GENERIC_ERROR, "No kbd macro has been defined")
            else
                events = last_kbd_macro
                call execute_kbd_macro(events, arg, status, kmacro_loop_setup_function)
                if (status%ok()) this_command => last_command
            end if

            if (allocated(last_kbd_macro)) deallocate (last_kbd_macro)
            if (had_saved) call move_alloc(saved, last_kbd_macro)
            if (.not. status%ok()) return
        end if

        ! C-u's count as their number.
        a = arg
        if (a%kind == PREFIX_RAW) a%kind = PREFIX_NUMBER

        if (a%kind == PREFIX_MINUS) then
            call set_error(status, FM_WRONG_TYPE_ARGUMENT, "expected integer, real or type(marker_type), got -")
            return
        end if

        if (a%kind == PREFIX_NUMBER) then
            if (a%value <= 0) return
        end if

        select case (kmacro_call_repeat_key)
        case (KMACRO_CALL_REPEAT_LAST_KEY)
            continue
        case (KMACRO_CALL_REPEAT_DISABLED)
            repeat_key = value_type()
        case default
            repeat_key = value_type(int(kmacro_call_repeat_key, int64))
        end select

        if (.not. allocated(repeat_key%a)) return

        ! Issue a hint to the user, if the echo area isn't in use.
        if (.not. is_current_message()) then
            times = ""

            if (kmacro_call_repeat_with_arg .and. a%kind == PREFIX_NUMBER) then
                if (a%value > 1) times = " "//int_text(a%value)//" times"
            end if

            key(1) = repeat_key
            call message("(Type "//format_kbd_macro(key)//" to repeat macro"//times//")")
        end if

        ! The repeat key's map: (kmacro-call-macro RA 'repeating nil M).
        if (kmacro_call_repeat_with_arg) then
            repeat_arg = a
        else
            repeat_arg = prefix_arg_type()
        end if

        repeat_has_macro = .false.

        if (ending) then
            if (allocated(last_kbd_macro)) then
                m = last_kbd_macro
                repeat_has_macro = .true.
            end if
        else if (present(macro)) then
            m = macro
            repeat_has_macro = .true.
        else if (allocated(last_kbd_macro)) then
            m = last_kbd_macro
            repeat_has_macro = .true.
        end if

        if (repeat_has_macro) call move_alloc(m, repeat_macro)
        if (.not. is_keymap_live(repeat_map)) call make_sparse_keymap(repeat_map)

        if (allocated(repeat_key_bound%a)) then
            key(1) = repeat_key_bound
            call define_key(repeat_map, key, value_type(), st, remove=.true.)
        end if

        key(1) = repeat_key
        call define_key(repeat_map, key, value_type(name="kmacro_call_macro_repeat"), st)
        repeat_key_bound = repeat_key
        call set_transient_map(repeat_map)
    end subroutine kmacro_call_macro

    !> Whether the echo area shows a message (current-message).  Not a port.
    logical function is_current_message()
        is_current_message = .false.
        if (allocated(last_message)) is_current_message = len(last_message) > 0
    end function is_current_message

    ! ------------------------------------------------------------------
    ! Combined function key bindings
    ! ------------------------------------------------------------------

    !> Record subsequent keyboard input, defining a keyboard macro.
    !> The commands are recorded even as they are executed.
    !>
    !> Initializes the macro's `kmacro_counter' to ARG (or 0 if no prefix arg)
    !> before defining the macro.
    !>
    !> With \[universal_argument], appends to current keyboard macro (keeping
    !> the current value of `kmacro_counter').
    !>
    !> When used during defining/executing a macro, inserts the current value
    !> of `kmacro_counter' and increments the counter value by ARG (or by 1 if no
    !> prefix argument).  With just \[universal_argument], inserts the previous
    !> value of `kmacro_counter', and does not modify the counter; this is
    !> different from incrementing the counter by zero.  (The previous value
    !> of the counter is the one it had before the last increment.)
    !>
    !> The macro counter can be set directly via \[kmacro_set_counter] and \[kmacro_add_counter].
    !> The format of the inserted value of the counter can be controlled
    !> via \[kmacro_set_format].  Port of
    !> kmacro.el:kmacro-start-macro-or-insert-counter.
    recursive subroutine kmacro_start_macro_or_insert_counter(arg, status)
        type(prefix_arg_type), intent(in) :: arg
        type(status_type), intent(out) :: status

        if (is_defining_kbd_macro() .or. is_executing_kbd_macro()) then
            call kmacro_insert_counter(arg, status)
        else
            call kmacro_start_macro(arg, status)
        end if
    end subroutine kmacro_start_macro_or_insert_counter

    !> End kbd macro if currently being defined; else call last kbd macro.
    !> With numeric prefix ARG, repeat macro that many times.
    !> With \[universal_argument], call second macro in macro ring.  Port of
    !> kmacro.el:kmacro-end-or-call-macro.
    recursive subroutine kmacro_end_or_call_macro(arg, status, repeat)
        type(prefix_arg_type), intent(in) :: arg
        type(status_type), intent(out) :: status
        integer, intent(in), optional :: repeat
        integer :: nr

        nr = KMACRO_REPEAT_YES
        if (present(repeat)) nr = repeat
        status = status_type()

        if (is_defining_kbd_macro()) then
            if (kmacro_call_repeat_key /= KMACRO_CALL_REPEAT_DISABLED) then
                call kmacro_call_macro(arg, status, nr, .true.)
            else
                call kmacro_end_macro(arg, status)
            end if
        else if (associated(this_command, commands%kmacro_view_macro) .and. kmacro_view_last_item > 0) then
            ! We are in repeat mode!
            if (kmacro_view_last_item <= ring_size()) call call_ring_element(kmacro_view_last_item, arg, status)
        else if (arg%kind == PREFIX_RAW) then
            call kmacro_call_ring_2nd(prefix_arg_type(PREFIX_NUMBER, 1_int64), status)
        else
            call kmacro_call_macro(arg, status, nr)
        end if
    end subroutine kmacro_end_or_call_macro

    !> As `kmacro_end_or_call_macro' but allow repeat without repeating prefix.
    !> Port of kmacro.el:kmacro-end-or-call-macro-repeat.
    recursive subroutine kmacro_end_or_call_macro_repeat(arg, status)
        type(prefix_arg_type), intent(in) :: arg
        type(status_type), intent(out) :: status
        type(value_type), allocatable :: keys(:)
        logical :: found

        call kmacro_get_repeat_prefix(keys, found)
        call kmacro_end_or_call_macro(arg, status, KMACRO_REPEAT_NO)
        if (.not. status%ok()) return
        if (found) call kmacro_repeat_on_last_key(keys, status)
    end subroutine kmacro_end_or_call_macro_repeat

    !> Call last keyboard macro, ending it first if currently being defined.
    !> With numeric prefix ARG, repeat macro that many times.
    !> Zero argument means repeat until there is an error.  Port of
    !> kmacro.el:kmacro-end-and-call-macro (no mouse EVENT).
    recursive subroutine kmacro_end_and_call_macro(arg, status, repeat)
        type(prefix_arg_type), intent(in) :: arg
        type(status_type), intent(out) :: status
        integer, intent(in), optional :: repeat

        status = status_type()

        if (is_defining_kbd_macro()) then
            call kmacro_end_macro(prefix_arg_type(), status)
            if (.not. status%ok()) return
        end if

        call kmacro_call_macro(arg, status, repeat)
    end subroutine kmacro_end_and_call_macro

    ! ------------------------------------------------------------------
    ! Viewing the ring
    ! ------------------------------------------------------------------

    !> Display the last keyboard macro.
    !> If repeated, it shows previous elements in the macro ring.  Port of
    !> kmacro.el:kmacro-view-macro.
    subroutine kmacro_view_macro(status)
        type(status_type), intent(out) :: status
        type(value_type), allocatable :: keys(:)
        character(len=:), allocatable :: descr

        status = status_type()

        if (is_kmacro_ring_empty()) then
            kmacro_view_last_item = 0
        else if (.not. associated(last_command, commands%kmacro_view_macro)) then
            kmacro_view_last_item = 0
        else if (kmacro_view_last_item == 0) then
            kmacro_view_last_item = 1
            kmacro_view_item_no = 2
        else
            kmacro_view_last_item = kmacro_view_last_item + 1
            if (kmacro_view_last_item > ring_size()) kmacro_view_last_item = 0
            kmacro_view_item_no = kmacro_view_item_no + 1
        end if

        this_command => commands%kmacro_view_macro
        ! In case we repeat.
        last_command => this_command

        if (kmacro_view_last_item > 0) then
            select case (kmacro_view_item_no)
            case (2)
                descr = "2nd"
            case (3)
                descr = "3rd"
            case default
                descr = int_text(kmacro_view_item_no)//"th"
            end select

            keys = kmacro_ring(kmacro_view_last_item)%keys
            call kmacro_display(status, keys, descr=descr//" previous macro")
        else
            call display_last(status, descr="Last macro")
        end if
    end subroutine kmacro_view_macro

    !> Display the last keyboard macro.
    !> If repeated, it shows previous elements in the macro ring.
    !> To execute the displayed macro ring item without changing the macro ring,
    !> just enter \`C-k'.
    !> This is like `kmacro_view_macro', but allows repeating macro commands
    !> without repeating the prefix.  Port of
    !> kmacro.el:kmacro-view-macro-repeat.
    recursive subroutine kmacro_view_macro_repeat(status)
        type(status_type), intent(out) :: status
        type(value_type), allocatable :: keys(:)
        logical :: found

        call kmacro_get_repeat_prefix(keys, found)
        call kmacro_view_macro(status)
        if (.not. status%ok()) return
        if (allocated(last_kbd_macro) .and. found) call kmacro_repeat_on_last_key(keys, status)
    end subroutine kmacro_view_macro_repeat

    !> Force redisplay during keyboard macro execution.  Port of
    !> kmacro.el:kmacro-redisplay.
    subroutine kmacro_redisplay(status)
        type(status_type), intent(out) :: status

        status = status_type()

        if (.not. (is_executing_kbd_macro() .or. is_defining_kbd_macro())) then
            call set_error(status, FM_USER_ERROR, "Not defining or executing keyboard macro")
            return
        end if

        if (is_executing_kbd_macro()) then
            if (associated(redisplay_hook)) call redisplay_hook()
        end if
    end subroutine kmacro_redisplay

    !> Called from `keyboard_quit': a macro being defined (not appended
    !> to) is dropped, and the previous one becomes the head of the ring
    !> again.  Port of kmacro.el:kmacro-keyboard-quit.
    subroutine kmacro_keyboard_quit()
        if (.not. is_defining_kbd_macro()) return
        if (defining_kbd_macro == DEFINING_KBD_MACRO_APPEND) return
        if (is_kmacro_ring_empty()) return
        call kmacro_pop_ring(.false.)
    end subroutine kmacro_keyboard_quit

    ! ------------------------------------------------------------------
    ! Commands
    ! ------------------------------------------------------------------

    !> The prefix argument as interactive "N" reads it: its numeric value,
    !> or a number read with PROMPT if there is none.  Not a port.
    subroutine read_n(prompt, n, status)
        character(len=*), intent(in) :: prompt
        integer(int64), intent(out) :: n
        type(status_type), intent(out) :: status

        status = status_type()

        if (current_prefix_arg%kind /= PREFIX_NONE) then
            n = prefix_numeric_value(current_prefix_arg)
        else
            call read_number(prompt, n, status)
        end if
    end subroutine read_n

    !> Record subsequent keyboard input, defining a keyboard macro.
    !> The commands are recorded even as they are executed.
    !> Use \[kmacro_end_macro] to finish recording and make the macro available.
    !> Use \[kmacro_end_and_call_macro] to execute the macro.
    !>
    !> A true arg (prefix arg) means append to last macro defined.
    !>
    !> With \[universal_argument] prefix, append to last keyboard macro
    !> defined.  Depending on `kmacro_execute_before_append', this may begin
    !> by re-executing the last macro as if you typed it again.
    !>
    !> Otherwise, it sets `kmacro_counter' to ARG or 0 if missing before
    !> defining the macro.
    !>
    !> Use \[kmacro_insert_counter] to insert (and increment) the macro counter.
    !> The counter value can be set or modified via \[kmacro_set_counter] and \[kmacro_add_counter].
    !> The format of the counter can be modified via \[kmacro_set_format].
    !> Port of kmacro.el:kmacro-start-macro.
    recursive subroutine cmd_kmacro_start_macro(status)
        type(status_type), intent(out) :: status

        call kmacro_start_macro(current_prefix_arg, status)
    end subroutine cmd_kmacro_start_macro

    !> Finish defining a keyboard macro.
    !> The definition was started by \[kmacro_start_macro].
    !> The macro is now available for use via \[kmacro_call_macro].
    !>
    !> With numeric ARG, repeat the macro that many times,
    !> counting the definition just completed as the first repetition.
    !> An argument of zero means repeat until error.  Port of
    !> kmacro.el:kmacro-end-macro.
    recursive subroutine cmd_kmacro_end_macro(status)
        type(status_type), intent(out) :: status

        call kmacro_end_macro(prefix_arg_type(PREFIX_NUMBER, prefix_numeric_value(current_prefix_arg)), status)
    end subroutine cmd_kmacro_end_macro

    !> Call the keyboard MACRO that you defined with \[kmacro_start_macro].
    !> A prefix argument serves as a repeat count.  Zero means repeat until error.
    !> MACRO defaults to `last_kbd_macro'.
    !>
    !> When you call the macro, you can call the macro again by repeating
    !> just the last key in the key sequence that you used to call this
    !> command.  See `kmacro_call_repeat_key' and `kmacro_call_repeat_with_arg'
    !> for details on how to adjust or disable this behavior.  Port of
    !> kmacro.el:kmacro-call-macro.
    recursive subroutine cmd_kmacro_call_macro(status)
        type(status_type), intent(out) :: status

        call kmacro_call_macro(prefix_arg_type(PREFIX_NUMBER, prefix_numeric_value(current_prefix_arg)), status)
    end subroutine cmd_kmacro_call_macro

    !> Call the keyboard macro again, with the argument and macro of the
    !> call it repeats (the command of the repeat key after
    !> \[kmacro_end_and_call_macro]).  Upstream an anonymous command made
    !> by kmacro.el:kmacro-call-macro.
    recursive subroutine cmd_kmacro_call_macro_repeat(status)
        type(status_type), intent(out) :: status
        type(value_type), allocatable :: m(:)
        type(prefix_arg_type) :: ra

        ra = repeat_arg

        if (repeat_has_macro) then
            m = repeat_macro
            call kmacro_call_macro(ra, status, KMACRO_REPEAT_REPEATING, .false., m)
        else
            call kmacro_call_macro(ra, status, KMACRO_REPEAT_REPEATING, .false.)
        end if
    end subroutine cmd_kmacro_call_macro_repeat

    !> Record subsequent keyboard input, defining a keyboard macro.
    !> The commands are recorded even as they are executed.
    !>
    !> Initializes the macro's `kmacro_counter' to ARG (or 0 if no prefix arg)
    !> before defining the macro.
    !>
    !> With \[universal_argument], appends to current keyboard macro (keeping
    !> the current value of `kmacro_counter').
    !>
    !> When used during defining/executing a macro, inserts the current value
    !> of `kmacro_counter' and increments the counter value by ARG (or by 1 if no
    !> prefix argument).  With just \[universal_argument], inserts the previous
    !> value of `kmacro_counter', and does not modify the counter; this is
    !> different from incrementing the counter by zero.  (The previous value
    !> of the counter is the one it had before the last increment.)
    !>
    !> The macro counter can be set directly via \[kmacro_set_counter] and \[kmacro_add_counter].
    !> The format of the inserted value of the counter can be controlled
    !> via \[kmacro_set_format].  Port of
    !> kmacro.el:kmacro-start-macro-or-insert-counter.
    recursive subroutine cmd_kmacro_start_macro_or_insert_counter(status)
        type(status_type), intent(out) :: status

        call kmacro_start_macro_or_insert_counter(current_prefix_arg, status)
    end subroutine cmd_kmacro_start_macro_or_insert_counter

    !> End kbd macro if currently being defined; else call last kbd macro.
    !> With numeric prefix ARG, repeat macro that many times.
    !> With \[universal_argument], call second macro in macro ring.  Port of
    !> kmacro.el:kmacro-end-or-call-macro.
    recursive subroutine cmd_kmacro_end_or_call_macro(status)
        type(status_type), intent(out) :: status

        call kmacro_end_or_call_macro(current_prefix_arg, status)
    end subroutine cmd_kmacro_end_or_call_macro

    !> As `kmacro_end_or_call_macro' but allow repeat without repeating prefix.
    !> Port of kmacro.el:kmacro-end-or-call-macro-repeat.
    recursive subroutine cmd_kmacro_end_or_call_macro_repeat(status)
        type(status_type), intent(out) :: status

        call kmacro_end_or_call_macro_repeat(current_prefix_arg, status)
    end subroutine cmd_kmacro_end_or_call_macro_repeat

    !> Call last keyboard macro, ending it first if currently being defined.
    !> With numeric prefix ARG, repeat macro that many times.
    !> Zero argument means repeat until there is an error.
    !>
    !> To give a macro a name, so you can call it even after defining other
    !> macros, use \[kmacro_name_last_macro].  Port of
    !> kmacro.el:kmacro-end-and-call-macro.
    recursive subroutine cmd_kmacro_end_and_call_macro(status)
        type(status_type), intent(out) :: status

        call kmacro_end_and_call_macro(current_prefix_arg, status)
    end subroutine cmd_kmacro_end_and_call_macro

    !> Insert current value of `kmacro_counter', then increment it by ARG.
    !> Interactively, ARG defaults to 1.  With \[universal_argument], insert
    !> the previous value of `kmacro_counter', and do not increment the
    !> current value.
    !>
    !> The previous value of the counter is the one it had before
    !> the last increment.
    !>
    !> See Info node `(emacs) Keyboard Macro Counter' for more
    !> information.  Port of kmacro.el:kmacro-insert-counter.
    recursive subroutine cmd_kmacro_insert_counter(status)
        type(status_type), intent(out) :: status

        call kmacro_insert_counter(current_prefix_arg, status)
    end subroutine cmd_kmacro_insert_counter

    !> Set the format of `kmacro_counter' to FORMAT.
    !>
    !> The default format is "%d", which means to insert the number in
    !> decimal without any padding.  You can specify any format string
    !> that the `format' function accepts and that makes sense with a
    !> single integer extra argument.
    !>
    !> If you run this command while no keyboard macro is being defined,
    !> the new format affects all subsequent macro definitions.
    !>
    !> If you run this command while defining a keyboard macro, it
    !> affects only that macro, from that point on.
    !>
    !> Do not put the format string inside double quotes when you insert
    !> it in the minibuffer.
    !>
    !> See Info node `(emacs) Keyboard Macro Counter' for more
    !> information.  Port of kmacro.el:kmacro-set-format.
    recursive subroutine cmd_kmacro_set_format(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: format

        call read_string("Macro Counter Format: ", format, status)
        if (status%ok()) call kmacro_set_format(format)
    end subroutine cmd_kmacro_set_format

    !> Set the value of `kmacro_counter' to ARG, or prompt for value if no argument.
    !> With \[universal_argument] prefix, reset counter to its value prior to this iteration of the
    !> macro.
    !>
    !> See Info node `(emacs) Keyboard Macro Counter' for more
    !> information.  Port of kmacro.el:kmacro-set-counter.
    recursive subroutine cmd_kmacro_set_counter(status)
        type(status_type), intent(out) :: status
        integer(int64) :: n

        call read_n("Macro counter value: ", n, status)
        if (status%ok()) call kmacro_set_counter(n, status)
    end subroutine cmd_kmacro_set_counter

    !> Add the value of numeric prefix arg (prompt if missing) to `kmacro_counter'.
    !> With \[universal_argument], restore previous counter value.
    !>
    !> See Info node `(emacs) Keyboard Macro Counter' for more
    !> information.  Port of kmacro.el:kmacro-add-counter.
    recursive subroutine cmd_kmacro_add_counter(status)
        type(status_type), intent(out) :: status
        integer(int64) :: n

        call read_n("Add to macro counter: ", n, status)
        if (status%ok()) call kmacro_add_counter(n, status)
    end subroutine cmd_kmacro_add_counter

    !> Load the value of a REGISTER into `kmacro_counter'.  Port of
    !> kmacro.el:kmacro-reg-load-counter.
    recursive subroutine cmd_kmacro_reg_load_counter(status)
        type(status_type), intent(out) :: status
        integer(int64) :: register

        call register_read_with_preview("Load register to counter: ", register, status)
        if (status%ok()) call kmacro_reg_load_counter(register)
    end subroutine cmd_kmacro_reg_load_counter

    !> Save the value of `kmacro_counter' to a REGISTER.  Port of
    !> kmacro.el:kmacro-reg-save-counter.
    recursive subroutine cmd_kmacro_reg_save_counter(status)
        type(status_type), intent(out) :: status
        integer(int64) :: register

        call register_read_with_preview("Save counter to register: ", register, status)
        if (status%ok()) call kmacro_reg_save_counter(register)
    end subroutine cmd_kmacro_reg_save_counter

    !> Read the register to compare and increment the counter by the
    !> numeric prefix if PRED holds.  Not a port.
    recursive subroutine reg_add_counter_command(pred, status)
        integer, intent(in) :: pred
        type(status_type), intent(out) :: status
        integer(int64) :: register, arg

        arg = prefix_numeric_value(current_prefix_arg)
        call register_read_with_preview("Compare counter to register: ", register, status)
        if (status%ok()) call kmacro_reg_add_counter(pred, register, arg, status)
    end subroutine reg_add_counter_command

    !> Increment counter by one if it is equal to register value.
    !> Prompt for the register to compare.
    !> Optional ARG (the prefix argument) specifies the increment.  Port of
    !> kmacro.el:kmacro-reg-add-counter-equal.
    recursive subroutine cmd_kmacro_reg_add_counter_equal(status)
        type(status_type), intent(out) :: status

        call reg_add_counter_command(KMACRO_PRED_EQUAL, status)
    end subroutine cmd_kmacro_reg_add_counter_equal

    !> Increment counter by one if it is less than register value.
    !> Prompt for the register to compare.
    !> Optional ARG (the prefix argument) specifies increment.  Port of
    !> kmacro.el:kmacro-reg-add-counter-less.
    recursive subroutine cmd_kmacro_reg_add_counter_less(status)
        type(status_type), intent(out) :: status

        call reg_add_counter_command(KMACRO_PRED_LESS, status)
    end subroutine cmd_kmacro_reg_add_counter_less

    !> Increment counter by one if it is greater than register value.
    !> Prompt for the register to compare.
    !> Optional ARG (the prefix argument) specifies increment.  Port of
    !> kmacro.el:kmacro-reg-add-counter-greater.
    recursive subroutine cmd_kmacro_reg_add_counter_greater(status)
        type(status_type), intent(out) :: status

        call reg_add_counter_command(KMACRO_PRED_GREATER, status)
    end subroutine cmd_kmacro_reg_add_counter_greater

    !> Quit the keyboard macro if the counter is equal to ARG.
    !> ARG defaults to zero if omitted (no prefix argument).  Port of
    !> kmacro.el:kmacro-quit-counter-equal.
    recursive subroutine cmd_kmacro_quit_counter_equal(status)
        type(status_type), intent(out) :: status

        call kmacro_quit_counter(KMACRO_PRED_EQUAL, prefix_numeric_value(current_prefix_arg), status)
    end subroutine cmd_kmacro_quit_counter_equal

    !> Quit the keyboard macro if the counter is less than ARGS.
    !> ARG defaults to zero if omitted (no prefix argument).  Port of
    !> kmacro.el:kmacro-quit-counter-less.
    recursive subroutine cmd_kmacro_quit_counter_less(status)
        type(status_type), intent(out) :: status

        call kmacro_quit_counter(KMACRO_PRED_LESS, prefix_numeric_value(current_prefix_arg), status)
    end subroutine cmd_kmacro_quit_counter_less

    !> Quit the keyboard macro if the counter is greater than ARG.
    !> ARG defaults to zero if omitted (no prefix argument).  Port of
    !> kmacro.el:kmacro-quit-counter-greater.
    recursive subroutine cmd_kmacro_quit_counter_greater(status)
        type(status_type), intent(out) :: status

        call kmacro_quit_counter(KMACRO_PRED_GREATER, prefix_numeric_value(current_prefix_arg), status)
    end subroutine cmd_kmacro_quit_counter_greater

    !> Execute second keyboard macro in macro ring.
    !> With numeric argument ARG, execute the macro that many times.
    !> Port of kmacro.el:kmacro-call-ring-2nd.
    recursive subroutine cmd_kmacro_call_ring_2nd(status)
        type(status_type), intent(out) :: status

        call kmacro_call_ring_2nd(prefix_arg_type(PREFIX_NUMBER, prefix_numeric_value(current_prefix_arg)), status)
    end subroutine cmd_kmacro_call_ring_2nd

    !> Execute second keyboard macro in macro ring.
    !> This is like `kmacro_call_ring_2nd', but allows repeating macro commands
    !> without repeating the prefix.  Port of
    !> kmacro.el:kmacro-call-ring-2nd-repeat.
    recursive subroutine cmd_kmacro_call_ring_2nd_repeat(status)
        type(status_type), intent(out) :: status

        call kmacro_call_ring_2nd_repeat(prefix_arg_type(PREFIX_NUMBER, prefix_numeric_value(current_prefix_arg)), status)
    end subroutine cmd_kmacro_call_ring_2nd_repeat

    !> Display the second macro in the keyboard macro ring.  Port of
    !> kmacro.el:kmacro-view-ring-2nd.
    recursive subroutine cmd_kmacro_view_ring_2nd(status)
        type(status_type), intent(out) :: status

        call kmacro_view_ring_2nd(status)
    end subroutine cmd_kmacro_view_ring_2nd

    !> Move to next keyboard macro in keyboard macro ring.
    !> Displays the selected macro in the echo area.
    !> The ARG parameter is unused.  Port of
    !> kmacro.el:kmacro-cycle-ring-next.
    recursive subroutine cmd_kmacro_cycle_ring_next(status)
        type(status_type), intent(out) :: status

        call kmacro_cycle_ring_next(status)
    end subroutine cmd_kmacro_cycle_ring_next

    !> Move to previous keyboard macro in keyboard macro ring.
    !> Displays the selected macro in the echo area.
    !> The ARG parameter is unused.  Port of
    !> kmacro.el:kmacro-cycle-ring-previous.
    recursive subroutine cmd_kmacro_cycle_ring_previous(status)
        type(status_type), intent(out) :: status

        call kmacro_cycle_ring_previous(status)
    end subroutine cmd_kmacro_cycle_ring_previous

    !> Swap first two elements on keyboard macro ring.  Port of
    !> kmacro.el:kmacro-swap-ring.
    recursive subroutine cmd_kmacro_swap_ring(status)
        type(status_type), intent(out) :: status

        call kmacro_swap_ring(status)
    end subroutine cmd_kmacro_swap_ring

    !> Delete current macro from keyboard macro ring.
    !> The ARG parameter is unused.  Port of
    !> kmacro.el:kmacro-delete-ring-head.
    recursive subroutine cmd_kmacro_delete_ring_head(status)
        type(status_type), intent(out) :: status

        call kmacro_delete_ring_head(status)
    end subroutine cmd_kmacro_delete_ring_head

    !> Display the last keyboard macro.
    !> If repeated, it shows previous elements in the macro ring.
    !> The ARG parameter is unused.  Port of kmacro.el:kmacro-view-macro.
    recursive subroutine cmd_kmacro_view_macro(status)
        type(status_type), intent(out) :: status

        call kmacro_view_macro(status)
    end subroutine cmd_kmacro_view_macro

    !> Display the last keyboard macro.
    !> If repeated, it shows previous elements in the macro ring.
    !> To execute the displayed macro ring item without changing the macro ring,
    !> just enter \`C-k'.
    !> This is like `kmacro_view_macro', but allows repeating macro commands
    !> without repeating the prefix.  Port of
    !> kmacro.el:kmacro-view-macro-repeat.
    recursive subroutine cmd_kmacro_view_macro_repeat(status)
        type(status_type), intent(out) :: status

        call kmacro_view_macro_repeat(status)
    end subroutine cmd_kmacro_view_macro_repeat

    !> Force redisplay during keyboard macro execution.  Port of
    !> kmacro.el:kmacro-redisplay.
    recursive subroutine cmd_kmacro_redisplay(status)
        type(status_type), intent(out) :: status

        call kmacro_redisplay(status)
    end subroutine cmd_kmacro_redisplay

    !> Bind KEYS (kbd syntax) to command CMD in `kmacro_keymap'.  Not a port.
    subroutine bind(keys, cmd)
        character(len=*), intent(in) :: keys, cmd
        type(value_type), allocatable :: key(:)
        type(status_type) :: st

        call key_parse(keys, key, st)
        call define_key(kmacro_keymap, key, value_type(name=cmd), st)
    end subroutine bind

    !> Register the commands, build `kmacro_keymap' (bindings_fm makes
    !> kmacro.el's autoloaded global keys: C-x (, C-x ), C-x e, <f3>, <f4>
    !> and C-x C-k), and show " Def" in the mode line while a macro is being
    !> defined (bindings.el's minor-mode-alist entry
    !> (defining-kbd-macro mode-line-defining-kbd-macro)).  Not a port.
    subroutine syms_of_kmacro()
        type(status_type) :: st

        call ensure_formats()
        commands%kmacro_start_macro%run => cmd_kmacro_start_macro
        commands%kmacro_end_macro%run => cmd_kmacro_end_macro
        commands%kmacro_call_macro%run => cmd_kmacro_call_macro
        commands%kmacro_call_macro_repeat%run => cmd_kmacro_call_macro_repeat
        commands%kmacro_start_macro_or_insert_counter%run => cmd_kmacro_start_macro_or_insert_counter
        commands%kmacro_end_or_call_macro%run => cmd_kmacro_end_or_call_macro
        commands%kmacro_end_or_call_macro_repeat%run => cmd_kmacro_end_or_call_macro_repeat
        commands%kmacro_end_and_call_macro%run => cmd_kmacro_end_and_call_macro
        commands%kmacro_insert_counter%run => cmd_kmacro_insert_counter
        commands%kmacro_set_format%run => cmd_kmacro_set_format
        commands%kmacro_set_counter%run => cmd_kmacro_set_counter
        commands%kmacro_add_counter%run => cmd_kmacro_add_counter
        commands%kmacro_reg_load_counter%run => cmd_kmacro_reg_load_counter
        commands%kmacro_reg_save_counter%run => cmd_kmacro_reg_save_counter
        commands%kmacro_reg_add_counter_equal%run => cmd_kmacro_reg_add_counter_equal
        commands%kmacro_reg_add_counter_less%run => cmd_kmacro_reg_add_counter_less
        commands%kmacro_reg_add_counter_greater%run => cmd_kmacro_reg_add_counter_greater
        commands%kmacro_quit_counter_equal%run => cmd_kmacro_quit_counter_equal
        commands%kmacro_quit_counter_less%run => cmd_kmacro_quit_counter_less
        commands%kmacro_quit_counter_greater%run => cmd_kmacro_quit_counter_greater
        commands%kmacro_call_ring_2nd%run => cmd_kmacro_call_ring_2nd
        commands%kmacro_call_ring_2nd_repeat%run => cmd_kmacro_call_ring_2nd_repeat
        commands%kmacro_view_ring_2nd%run => cmd_kmacro_view_ring_2nd
        commands%kmacro_cycle_ring_next%run => cmd_kmacro_cycle_ring_next
        commands%kmacro_cycle_ring_previous%run => cmd_kmacro_cycle_ring_previous
        commands%kmacro_swap_ring%run => cmd_kmacro_swap_ring
        commands%kmacro_delete_ring_head%run => cmd_kmacro_delete_ring_head
        commands%kmacro_view_macro%run => cmd_kmacro_view_macro
        commands%kmacro_view_macro_repeat%run => cmd_kmacro_view_macro_repeat
        commands%kmacro_redisplay%run => cmd_kmacro_redisplay
        kmacro_keyboard_quit_hook => kmacro_keyboard_quit
        ! The keymap is rebuilt.
        if (is_keymap_live(kmacro_keymap)) call free_keymap(kmacro_keymap, st)
        call make_sparse_keymap(kmacro_keymap)
        ! Start, end, execute macros
        call bind("s", "kmacro_start_macro")
        call bind("C-s", "kmacro_start_macro")
        call bind("C-k", "kmacro_end_or_call_macro_repeat")
        call bind("d", "kmacro_redisplay")
        ! macro ring
        call bind("C-n", "kmacro_cycle_ring_next")
        call bind("C-p", "kmacro_cycle_ring_previous")
        call bind("C-v", "kmacro_view_macro_repeat")
        call bind("C-d", "kmacro_delete_ring_head")
        call bind("C-t", "kmacro_swap_ring")
        call bind("C-l", "kmacro_call_ring_2nd_repeat")
        ! macro counter
        call bind("C-f", "kmacro_set_format")
        call bind("C-c", "kmacro_set_counter")
        call bind("C-i", "kmacro_insert_counter")
        call bind("C-a", "kmacro_add_counter")
        call bind("C-r l", "kmacro_reg_load_counter")
        call bind("C-r s", "kmacro_reg_save_counter")
        call bind("C-r a =", "kmacro_reg_add_counter_equal")
        call bind("C-r a <", "kmacro_reg_add_counter_less")
        call bind("C-r a >", "kmacro_reg_add_counter_greater")
        call bind("C-q =", "kmacro_quit_counter_equal")
        call bind("C-q <", "kmacro_quit_counter_less")
        call bind("C-q >", "kmacro_quit_counter_greater")
        call add_minor_mode_lighter("defining_kbd_macro", " Def", face="font_lock_warning_face", &
                                    active=is_defining_kbd_macro)
    end subroutine syms_of_kmacro

    !> N in decimal.  Not a port.
    function int_text(n) result(s)
        integer(int64), intent(in) :: n
        character(len=:), allocatable :: s
        character(len=24) :: buf

        write (buf, "(i0)") n
        s = trim(buf)
    end function int_text

    !> The number of characters of the UTF-8 text S.  Not a port.
    integer function nchars(s)
        character(len=*), intent(in) :: s
        integer :: k

        nchars = 0

        do k = 1, len(s)
            if (iand(ichar(s(k:k)), 192) /= 128) nchars = nchars + 1
        end do
    end function nchars

    !> The first N characters of the UTF-8 text S.  Not a port.
    function first_chars(s, n) result(t)
        character(len=*), intent(in) :: s
        integer, intent(in) :: n
        character(len=:), allocatable :: t
        integer :: k, count

        count = 0

        do k = 1, len(s)
            if (iand(ichar(s(k:k)), 192) /= 128) then
                count = count + 1

                if (count > n) then
                    t = s(1:k - 1)
                    return
                end if
            end if
        end do

        t = s
    end function first_chars
end module kmacro_fm
