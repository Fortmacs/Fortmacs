! test_simple_set_variable_mod.f90 -- tests for set_variable
! (simple_set_variable_mod).
! Own tests; upstream has no tests for set-variable.
!
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

!> Own tests (upstream has none for set-variable), through the command
!> loop: each kind of value, buffer-local values, and the errors.
module test_simple_set_variable_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_mod, only: status_type
    use handle_mod, only: handle_type
    use value_mod, only: value_type
    use data_mod, only: is_local_variable
    use buffer_mod, only: get_or_make_buffer, set_buffer, fill_column
    use frame_mod, only: selected_frame, ensure_frame_windows
    use window_mod, only: delete_other_windows, switch_to_buffer, resize_window_tree
    use keymap_mod, only: key_parse
    use keyboard_mod, only: unread_command_events, discard_input, last_message, command_loop
    use bindings_mod, only: initialize_bindings
    use replace_mod, only: case_replace
    use compile_mod, only: compile_command, compilation_scroll_output, COMPILATION_SCROLL_OUTPUT_FIRST_ERROR
    use display_engine_mod, only: message_log_maximum, message_log_maximum_choice_setting_type
    use files_mod, only: require_final_newline, REQUIRE_FINAL_NEWLINE_VISIT
    use simple_mod, only: set_variable, copy_region_blink_delay
    use parenthesis_mod, only: show_parenthesis_style, SHOW_PARENTHESIS_STYLE_PARENTHESIS
    implicit none(type, external)
    private

    public :: collect_simple_set_variable_mod

    character(len=*), parameter :: LQ = char(226)//char(128)//char(152), RQ = char(226)//char(128)//char(153)

contains

    !> Collect all exported unit tests.
    subroutine collect_simple_set_variable_mod(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("set-variable-integer-buffer-local", test_integer), &
                    new_unittest("set-variable-logical", test_logical), &
                    new_unittest("set-variable-string", test_string), &
                    new_unittest("set-variable-choice", test_choice), &
                    new_unittest("set-variable-on-off-choice", test_on_off_choice), &
                    new_unittest("choice-setting-number-kind", test_number_kind), &
                    new_unittest("set-variable-rejected-choice", test_rejected_choice), &
                    new_unittest("set-variable-real", test_real), &
                    new_unittest("set-variable-type-mismatch", test_mismatch), &
                    new_unittest("set-variable-read-errors", test_read_errors), &
                    new_unittest("set-variable-default", test_default), &
                    new_unittest("set-variable-prefix-argument", test_prefix) &
                    ]
    end subroutine collect_simple_set_variable_mod

    subroutine fresh()
        type(handle_type) :: buffer
        type(status_type) :: st

        call initialize_bindings()
        call discard_input()
        call get_or_make_buffer("*scratch*", buffer, st)
        call set_buffer(buffer, st)
        call ensure_frame_windows()
        call delete_other_windows(selected_frame%selected_window, st)
        call resize_window_tree(selected_frame%root_window, 0, 0, 23, 80, 80)
        call switch_to_buffer("*scratch*", st)
        last_message = ""
    end subroutine fresh

    subroutine type_keys(keys)
        character(len=*), intent(in) :: keys
        type(value_type), allocatable :: evs(:)
        type(status_type) :: st

        call key_parse(keys, evs, st)
        call unread_command_events(evs)
    end subroutine type_keys

    !> Keys typing `text', spaces as SPC.
    function spelled(text) result(keys)
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: keys
        integer :: k

        keys = ""

        do k = 1, len(text)
            if (k > 1) keys = keys//" "

            if (text(k:k) == " ") then
                keys = keys//"SPC"
            else
                keys = keys//text(k:k)
            end if
        end do
    end function spelled

    !> M-x set_variable RET NAME RET VALUE RET, after `prefix' keys.
    subroutine set_test_variable(name, value, prefix)
        character(len=*), intent(in) :: name, value
        character(len=*), intent(in), optional :: prefix
        type(status_type) :: st
        character(len=:), allocatable :: keys

        keys = ""
        if (present(prefix)) keys = prefix//" "
        keys = keys//"M-x "//spelled("set_variable")//" RET "//spelled(name)//" RET"
        if (len(value) > 0) keys = keys//" "//spelled(value)
        call type_keys(keys//" RET")
        call command_loop(st)
    end subroutine set_test_variable

    !> fill_column is automatically buffer-local: setting it gives the
    !> buffer its own value.
    subroutine test_integer(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st

        call fresh()
        call switch_to_buffer("*setvar*", st)
        call set_test_variable("fill_column", "72")
        call check(error, fill_column%value == 72_int64 .and. is_local_variable(value_type(name="fill_column")), last_message)
        if (allocated(error)) return
        call switch_to_buffer("*scratch*", st)
        call check(error, fill_column%value == 70_int64, "the default is unchanged")
    end subroutine test_integer

    subroutine test_logical(error)
        type(error_type), allocatable, intent(out) :: error

        call fresh()
        call set_test_variable("case_replace", ".false.")
        call check(error,.not. case_replace, last_message)
        if (allocated(error)) return
        ! Fortran is case-insensitive.
        call set_test_variable("case_replace", ".TRUE.")
        call check(error, case_replace, last_message)
    end subroutine test_logical

    subroutine test_string(error)
        type(error_type), allocatable, intent(out) :: error

        call fresh()
        call set_test_variable("compile_command", '"make ""all"""')
        call check(error, compile_command%value, 'make "all"')
        if (allocated(error)) return
        call set_test_variable("compile_command", "'make -k'")
        call check(error, compile_command%value, "make -k")
    end subroutine test_string

    !> An enumerated setting takes its named constants only: anything else,
    !> such as upstream's symbol, is a type error naming them.
    subroutine test_choice(error)
        type(error_type), allocatable, intent(out) :: error

        call fresh()
        call set_test_variable("require_final_newline", "require_final_newline_visit")
        call check(error, require_final_newline%choice == REQUIRE_FINAL_NEWLINE_VISIT, last_message)
        if (allocated(error)) return
        call set_test_variable("require_final_newline", "7")
        call check(error, last_message, "Wrong type argument: expected .true., .false., "// &
                   "REQUIRE_FINAL_NEWLINE_VISIT, REQUIRE_FINAL_NEWLINE_VISIT_SAVE or REQUIRE_FINAL_NEWLINE_ASK, got 7")
        if (allocated(error)) return
        call set_test_variable("require_final_newline", "visit")
        call check(error, index(last_message, "Wrong type argument: expected .true., .false.") == 1 .and. &
                   index(last_message, ", got visit") > 0, last_message)
        if (allocated(error)) return
        call set_test_variable("require_final_newline", ".false.")
        call check(error, require_final_newline%is_off(), last_message)
    end subroutine test_choice

    !> A choice setting (R6-38) takes .true. and .false. for its on and off
    !> states and its named constants for the others.
    subroutine test_on_off_choice(error)
        type(error_type), allocatable, intent(out) :: error

        call fresh()
        call set_test_variable("compilation_scroll_output", ".TRUE.")
        call check(error, compilation_scroll_output%value, last_message)
        if (allocated(error)) return
        call set_test_variable("compilation_scroll_output", "compilation_scroll_output_first_error")
        call check(error, compilation_scroll_output%choice == COMPILATION_SCROLL_OUTPUT_FIRST_ERROR, last_message)
        if (allocated(error)) return
        call set_test_variable("compilation_scroll_output", "7")
        call check(error, last_message, "Wrong type argument: expected .true., .false. or "// &
                   "COMPILATION_SCROLL_OUTPUT_FIRST_ERROR, got 7")
        if (allocated(error)) return
        call set_test_variable("compilation_scroll_output", ".false.")
        call check(error, compilation_scroll_output%is_off(), last_message)
    end subroutine test_on_off_choice

    !> A number in a named component of a choice setting (R6-39) is a
    !> value of its own: the setting is not off, help shows the number,
    !> and any `=' clears it.
    subroutine test_number_kind(error)
        type(error_type), allocatable, intent(out) :: error
        type(message_log_maximum_choice_setting_type) :: saved

        saved = message_log_maximum
        message_log_maximum%lines = 5
        call check(error,.not. message_log_maximum%is_off() .and. message_log_maximum%other_value() == "%lines = 5", "a number")
        if (allocated(error)) return
        message_log_maximum = .true.
        call check(error, &
                  message_log_maximum%lines < 0 .and. message_log_maximum%value .and. len(message_log_maximum%other_value()) == 0, &
                   "cleared by =")
        if (allocated(error)) return
        message_log_maximum = .false.
        call check(error, message_log_maximum%is_off(), "off")
        message_log_maximum = saved
    end subroutine test_number_kind

    !> A value the choice setting does not accept is an error, as in the
    !> init file (panel 2026-11-04), and the value stays.
    subroutine test_rejected_choice(error)
        type(error_type), allocatable, intent(out) :: error

        call fresh()
        call set_test_variable("show_parenthesis_style", ".true.")
        call check(error, last_message, "Wrong type argument: show_parenthesis_style: .true. is not a value of this option")
        if (allocated(error)) return
        call check(error, show_parenthesis_style%choice == SHOW_PARENTHESIS_STYLE_PARENTHESIS, "value kept")
    end subroutine test_rejected_choice

    subroutine test_real(error)
        type(error_type), allocatable, intent(out) :: error
        real :: saved

        call fresh()
        saved = copy_region_blink_delay
        call set_test_variable("copy_region_blink_delay", "0.5e0")
        call check(error, abs(copy_region_blink_delay - 0.5) < 1e-6, last_message)
        copy_region_blink_delay = saved
    end subroutine test_real

    subroutine test_mismatch(error)
        type(error_type), allocatable, intent(out) :: error

        call fresh()
        call set_test_variable("fill_column", "abc")
        call check(error, last_message, "Value "//LQ//"abc"//RQ//" does not match type integer of fill_column")
        if (allocated(error)) return
        call set_test_variable("case_replace", "1")
        call check(error, last_message, "Value "//LQ//"1"//RQ//" does not match type logical of case_replace")
        if (allocated(error)) return
        call set_test_variable("compile_command", "make")
        call check(error, last_message, "Value "//LQ//"make"//RQ//" does not match type character of compile_command")
        if (allocated(error)) return
        ! Not an expression evaluator.
        call set_test_variable("fill_column", "70+2")
        call check(error, last_message, "Value "//LQ//"70+2"//RQ//" does not match type integer of fill_column")
        if (allocated(error)) return
        call check(error, fill_column%value == 70_int64, "unchanged")
    end subroutine test_mismatch

    !> The Lisp reader's errors: something after the value, an
    !> unterminated string.
    subroutine test_read_errors(error)
        type(error_type), allocatable, intent(out) :: error

        call fresh()
        call set_test_variable("case_replace", ".true. x")
        call check(error, last_message, 'Invalid read syntax: "Trailing garbage following expression"')
        if (allocated(error)) return
        call set_test_variable("compile_command", '"make')
        call check(error, last_message, "End of file during parsing")
    end subroutine test_read_errors

    !> Empty input sets the current value again (the default, for M-n).
    subroutine test_default(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st

        call fresh()
        compile_command = "make -k"
        call set_test_variable("compile_command", "")
        call check(error, compile_command%value, "make -k")
        if (allocated(error)) return
        ! M-n leaves point at the start of the value (goto-history-element).
        call type_keys("M-x "//spelled("set_variable")//" RET "//spelled("compile_command")//" RET M-n C-e DEL DEL "// &
                       spelled('j"')//" RET")
        call command_loop(st)
        call check(error, compile_command%value, "make -j", last_message)
    end subroutine test_default

    !> With a prefix argument, a setting object is set buffer-locally; a
    !> plain variable can't be.
    subroutine test_prefix(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st

        call fresh()
        call switch_to_buffer("*setvar-prefix*", st)
        call set_test_variable("case_fold_search", ".false.", "C-u")
        call check(error, is_local_variable(value_type(name="case_fold_search")), last_message)
        if (allocated(error)) return
        call set_test_variable("case_replace", ".false.", "C-u")
        call check(error, last_message, "Symbol case_replace may not be buffer-local")
        if (allocated(error)) return
        call check(error, case_replace, "unchanged")
        call switch_to_buffer("*scratch*", st)
        call set_variable("case_replace", ".true.", .false., st)
        call check(error, st%ok() .and. case_replace, "set_variable from Fortran")
    end subroutine test_prefix
end module test_simple_set_variable_mod
