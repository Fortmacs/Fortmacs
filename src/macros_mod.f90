! macros_mod.f90 -- keyboard macros.
! Ported from GNU Emacs 31.1 src/macros.c.
!
! Copyright (C) 1985-1986, 1993, 2000-2026 Free Software Foundation, Inc.
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

!> Defining and executing keyboard macros: the submodule of keyboard_mod
!> holding macros.c (keyboard_mod keeps the state, as struct kboard does,
!> and records the events read).  A macro is an array of events
!> (`last_keyboard_macro'); the macro being defined is collected in
!> `keyboard_macro_buffer'.
!>
!> Deferred: macros that are names (their function definitions), and
!> string macros other than as `execute_keyboard_macro' arguments (a string
!> `last_keyboard_macro').
submodule(keyboard_mod) macros_mod
    use commands_mod, only: commands
    use error_mod, only: WRONG_TYPE_ARGUMENT_CODE
    use value_mod, only: value_type, vtag, V_STRING, V_ARRAY
    use execute_interactively_mod, only: prefix_argument_text
    use print_mod, only: fortran_text
    use display_engine_mod, only: message
    implicit none(type, external)

contains

    module procedure start_keyboard_macro
    type(value_type), allocatable :: grown(:), macro(:)
    integer(int64) :: n

    status = status_type()

    if (defining_keyboard_macro /= DEFINING_KEYBOARD_MACRO_NO) then
        call set_error(status, GENERIC_ERROR_CODE, "Already defining kbd macro")
        return
    end if

    if (.not. allocated(keyboard_macro_buffer)) allocate (keyboard_macro_buffer(30))

    if (.not. append) then
        keyboard_macro_pointer = 0
        keyboard_macro_end = 0
        call message("Defining kbd macro...")
    else
        ! Check the type of last-kbd-macro.
        if (.not. allocated(last_keyboard_macro)) then
            call set_error(status, WRONG_TYPE_ARGUMENT_CODE, "expected array, got .false.")
            return
        end if

        n = size(last_keyboard_macro, kind=int64)

        ! Copy last-kbd-macro into the buffer.
        if (size(keyboard_macro_buffer, kind=int64) - 30 < n) then
            allocate (grown(n + 30))
            call move_alloc(grown, keyboard_macro_buffer)
        end if

        if (n > 0) keyboard_macro_buffer(1:n) = last_keyboard_macro
        keyboard_macro_pointer = n
        keyboard_macro_end = n

        ! Re-execute the macro we are appending to, for consistency of
        ! behavior.
        if (.not. no_exec) then
            macro = last_keyboard_macro
            call execute_keyboard_macro(macro, prefix_argument_type(PREFIX_NUMBER, 1_int64), status)
            if (.not. status%ok()) return
        end if

        call message("Appending to kbd macro...")
    end if

    defining_keyboard_macro = DEFINING_KEYBOARD_MACRO_YES
    end procedure start_keyboard_macro

    module procedure finish_keyboard_macro
    defining_keyboard_macro = DEFINING_KEYBOARD_MACRO_NO
    if (allocated(last_keyboard_macro)) deallocate (last_keyboard_macro)
    allocate (last_keyboard_macro(keyboard_macro_end))
    if (keyboard_macro_end > 0) last_keyboard_macro = keyboard_macro_buffer(1:keyboard_macro_end)
    end procedure finish_keyboard_macro

    module procedure end_keyboard_macro
    type(value_type), allocatable :: macro(:)
    integer(int64) :: n

    status = status_type()

    if (defining_keyboard_macro == DEFINING_KEYBOARD_MACRO_NO) then
        call set_error(status, GENERIC_ERROR_CODE, "Not defining kbd macro")
        return
    end if

    select case (repeat%kind)
    case (PREFIX_NONE)
        n = 1
    case (PREFIX_NUMBER)
        n = repeat%value
    case default
        call set_error(status, WRONG_TYPE_ARGUMENT_CODE, "expected integer, got "//prefix_argument_text(repeat))
        return
    end select

    call finish_keyboard_macro()
    call message("Keyboard macro defined")
    macro = last_keyboard_macro

    if (n == 0) then
        call execute_keyboard_macro(macro, prefix_argument_type(PREFIX_NUMBER, 0_int64), status, loopfunc)
    else if (n > 1) then
        call execute_keyboard_macro(macro, prefix_argument_type(PREFIX_NUMBER, n - 1), status, loopfunc)
    end if
    end procedure end_keyboard_macro

    module procedure store_keyboard_macro_character
    type(value_type), allocatable :: grown(:)

    if (defining_keyboard_macro == DEFINING_KEYBOARD_MACRO_NO) return
    if (.not. allocated(keyboard_macro_buffer)) allocate (keyboard_macro_buffer(30))

    if (keyboard_macro_pointer == size(keyboard_macro_buffer, kind=int64)) then
        allocate (grown(2*keyboard_macro_pointer + 1))
        grown(1:keyboard_macro_pointer) = keyboard_macro_buffer(1:keyboard_macro_pointer)
        call move_alloc(grown, keyboard_macro_buffer)
    end if

    keyboard_macro_pointer = keyboard_macro_pointer + 1
    keyboard_macro_buffer(keyboard_macro_pointer) = c
    end procedure store_keyboard_macro_character

    module procedure finalize_keyboard_macro_characters
    keyboard_macro_end = keyboard_macro_pointer
    end procedure finalize_keyboard_macro_characters

    module procedure cancel_keyboard_macro_events
    keyboard_macro_pointer = keyboard_macro_end
    end procedure cancel_keyboard_macro_events

    module procedure set_last_macro_command
    type(command_macro_type) :: macro

    if (last_macro_command%macro == 0) then
        if (.not. allocated(command_macros)) allocate (command_macros(0))
        command_macros = [command_macros, macro]
        last_macro_command%macro = size(command_macros)
    end if

    command_macros(last_macro_command%macro)%events = events
    last_macro_command%name = fortran_text(make_list(events))
    last_macro_command%is_command = .true.
    last_macro_command%execute => execute_last_macro_command
    end procedure set_last_macro_command

    !> The procedure of `last_macro_command': execute its events once
    !> (repeat.el:repeat runs a macro with no repeat count).  Not a port.
    recursive subroutine execute_last_macro_command(status)
        type(status_type), intent(out) :: status
        type(value_type), allocatable :: events(:)

        ! A copy: the macro's commands may run another macro.
        events = command_macros(last_macro_command%macro)%events
        call execute_keyboard_macro(events, prefix_argument_type(), status)
    end subroutine execute_last_macro_command

    module procedure store_keyboard_macro_event
    call store_keyboard_macro_character(event)
    end procedure store_keyboard_macro_event

    module procedure execute_last_keyboard_macro
    type(value_type), allocatable :: macro(:)

    status = status_type()
    ! Don't interfere with recognition of the previous command
    ! from before this macro started.
    this_command => last_command
    ! C-x z after the macro should repeat the macro.
    real_this_command => null()

    if (allocated(last_keyboard_macro)) then
        call set_last_macro_command(last_keyboard_macro)
        real_this_command => last_macro_command
    end if

    if (defining_keyboard_macro /= DEFINING_KEYBOARD_MACRO_NO) then
        call set_error(status, GENERIC_ERROR_CODE, "Can't execute anonymous macro while defining one")
        return
    else if (.not. allocated(last_keyboard_macro)) then
        call set_error(status, GENERIC_ERROR_CODE, "No kbd macro has been defined")
        return
    end if

    macro = last_keyboard_macro
    call execute_keyboard_macro(macro, prefix, status, loopfunc)
    if (.not. status%ok()) return
    ! command_loop_1 sets this to nil before it returns; get back the
    ! last command within the macro so that it can be last, again, after
    ! we return.
    this_command => last_command
    end procedure execute_last_keyboard_macro

    module procedure execute_keyboard_macro_value
    type(value_type), allocatable :: final(:)

    status = status_type()

    if (.not. (vtag(macro) == V_STRING .or. vtag(macro) == V_ARRAY)) then
        call set_error(status, GENERIC_ERROR_CODE, "Keyboard macros must be strings or vectors")
        return
    end if

    call macro_events_of(macro, final)
    call execute_events(final, vtag(macro) == V_STRING, count, status, loopfunc)
    end procedure execute_keyboard_macro_value

    !> The events of MACRO, a list of events or a string (its
    !> characters).  (Here, not keyboard_mod's sequence_events: gfortran
    !> drops private parent procedures that submodules call.)  Not a port.
    subroutine macro_events_of(macro, evs)
        type(value_type), intent(in) :: macro
        type(value_type), allocatable, intent(out) :: evs(:)
        character(len=:), allocatable :: s
        integer(int64) :: position, n
        integer(int32) :: ch, length

        if (vtag(macro) == V_ARRAY) then
            call list_elements(macro, evs)
            return
        end if

        s = xstring(macro)
        allocate (evs(len(s)))
        n = 0
        position = 1

        do while (position <= len(s, kind=int64))
            call string_character_and_length(s, position, ch, length)
            n = n + 1
            evs(n) = value_type(int(ch, int64))
            position = position + length
        end do

        evs = evs(1:n)
    end subroutine macro_events_of

    module procedure execute_keyboard_macro_events
    call execute_events(macro, .false., count, status, loopfunc)
    end procedure execute_keyboard_macro_events

    !> Execute the events FINAL (of a string macro if IS_STRING) COUNT
    !> times.  Port of macros.c:Fexecute_kbd_macro, in part (its body), and
    !> macros.c:pop_kbd_macro.
    recursive subroutine execute_events(final, is_string, count, status, loopfunc)
        type(value_type), intent(in) :: final(:)
        type(prefix_argument_type), intent(in) :: count
        logical, intent(in) :: is_string
        type(status_type), intent(out) :: status
        procedure(keyboard_macro_loop_procedure), optional :: loopfunc
        type(value_type), allocatable :: saved_events(:)
        type(command_type), pointer :: saved_real_this_command
        type(status_type) :: hook_status
        integer(int64) :: repeat, success_count, saved_index
        logical :: saved_active, saved_string, saved_forced, saved_handles, done

        status = status_type()
        repeat = 1
        success_count = 0
        executing_keyboard_macro_iterations = 0
        if (count%kind /= PREFIX_NONE) repeat = prefix_numeric_value(count)
        ! record_unwind_protect (pop_keyboard_macro, ...)
        saved_active = macro_active
        saved_string = macro_string
        saved_forced = macro_forced_end
        saved_index = executing_keyboard_macro_index
        if (allocated(macro_events)) call move_alloc(macro_events, saved_events)
        saved_real_this_command => real_this_command
        saved_handles = loop_handles_errors

        do
            macro_events = final
            macro_active = .true.
            macro_string = is_string
            macro_forced_end = .false.
            macro_started = .true.
            executing_keyboard_macro_index = 0
            prefix_argument = prefix_argument_type()

            if (present(loopfunc)) then
                if (.not. loopfunc()) exit
            end if

            ! command_loop_2 (list1 (Qminibuffer_quit)): run the macro's
            ! commands until read_key_sequence finds its end; an error
            ! ends it.
            loop_handles_errors = .false.

            do
                call command_loop_step(done, status)
                if (.not. status%ok()) exit
                if (done .or. exit_requested) exit
            end do

            if (.not. status%ok()) exit
            success_count = success_count + 1
            executing_keyboard_macro_iterations = success_count
            repeat = repeat - 1
            if (repeat == 0 .or. .not. macro_active .or. macro_forced_end) exit
        end do

        if (status%ok()) macro_started = .false.
        ! pop_keyboard_macro
        loop_handles_errors = saved_handles
        macro_active = saved_active
        macro_string = saved_string
        macro_forced_end = saved_forced
        executing_keyboard_macro_index = saved_index
        if (allocated(macro_events)) deallocate (macro_events)
        if (allocated(saved_events)) call move_alloc(saved_events, macro_events)
        real_this_command => saved_real_this_command
        call keyboard_macro_termination_hook%execute(hook_status)
        if (status%ok() .and. .not. hook_status%ok()) status = hook_status
    end subroutine execute_events

    ! ------------------------------------------------------------------
    ! Commands
    ! ------------------------------------------------------------------

    !> Record subsequent keyboard input, defining a keyboard macro.
    !> The commands are recorded even as they are executed.
    !> Use \[end_keyboard_macro] to finish recording and make the macro available.
    !> A prefix argument means append to last macro defined;
    !> this begins by re-executing that macro as if you typed it again.
    !> Port of macros.c:Fstart_kbd_macro (the command, "P").
    recursive subroutine execute_start_keyboard_macro(status)
        type(status_type), intent(out) :: status

        call start_keyboard_macro(current_prefix_argument%kind /= PREFIX_NONE, .false., status)
    end subroutine execute_start_keyboard_macro

    !> Finish defining a keyboard macro.
    !> The definition was started by \[start_keyboard_macro].
    !> The macro is now available for use via \[execute_last_keyboard_macro].
    !>
    !> With numeric arg, repeat macro now that many times,
    !> counting the definition just completed as the first repetition.
    !> An argument of zero means repeat until error.  Port of
    !> macros.c:Fend_kbd_macro (the command, "p").
    recursive subroutine execute_end_keyboard_macro(status)
        type(status_type), intent(out) :: status

        call end_keyboard_macro(prefix_argument_type(PREFIX_NUMBER, prefix_numeric_value(current_prefix_argument)), status)
    end subroutine execute_end_keyboard_macro

    !> Call the last keyboard macro that you defined with \[start_keyboard_macro].
    !>
    !> A prefix argument serves as a repeat count.  Zero means repeat until
    !> error.  Port of macros.c:Fcall_last_kbd_macro (the command, "p").
    recursive subroutine execute_execute_last_keyboard_macro(status)
        type(status_type), intent(out) :: status

        call execute_last_keyboard_macro(prefix_argument_type(PREFIX_NUMBER, prefix_numeric_value(current_prefix_argument)), status)
    end subroutine execute_execute_last_keyboard_macro

    module procedure initialize_macros_commands
    commands%start_keyboard_macro%execute => execute_start_keyboard_macro
    commands%end_keyboard_macro%execute => execute_end_keyboard_macro
    commands%execute_last_keyboard_macro%execute => execute_execute_last_keyboard_macro
    end procedure initialize_macros_commands
end submodule macros_mod
