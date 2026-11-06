! test_error_mod.f90 -- tests for error_mod.
! Own tests; upstream has no tests for the condition table.
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

module test_error_mod
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_mod, only: substitute_quotes
    use error_mod, only: condition_matches, error_message, error_name, arguments_out_of_range_code, &
                         arithmetic_error_code, buffer_read_only_code, end_of_buffer_code, generic_error_code, &
                         ok_code, overflow_error_code, quit_code, text_read_only_code, user_error_code, &
                         void_function_code, format_error, set_error, status_type
    implicit none(type, external)
    private

    public :: collect_error_mod

contains

    !> Collect all exported unit tests.
    subroutine collect_error_mod(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("default-is-ok", test_default_ok), &
                    new_unittest("intent-out-resets", test_intent_out_resets), &
                    new_unittest("names-and-messages", test_names), &
                    new_unittest("condition-hierarchy", test_hierarchy), &
                    new_unittest("format-error", test_format) &
                    ]
    end subroutine collect_error_mod

    subroutine test_default_ok(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st

        call check(error, st%ok(), "fresh status not ok")
        if (allocated(error)) return
        call set_error(st, ARGUMENTS_OUT_OF_RANGE_CODE, "5, 10")
        call check(error,.not. st%ok(), "error status reported ok")
        if (allocated(error)) return
        call check(error, st%detail, "5, 10")
    end subroutine test_default_ok

    subroutine fail_then_return(status)
        type(status_type), intent(out) :: status
    end subroutine fail_then_return

    subroutine test_intent_out_resets(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st

        call set_error(st, GENERIC_ERROR_CODE, "old")
        call fail_then_return(st)
        call check(error, st%ok(), "intent(out) did not reset status")
        if (allocated(error)) return
        call check(error,.not. allocated(st%detail), "detail survived reset")
    end subroutine test_intent_out_resets

    subroutine test_names(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, error_name(ARGUMENTS_OUT_OF_RANGE_CODE), "arguments_out_of_range")
        if (allocated(error)) return
        call check(error, error_message(BUFFER_READ_ONLY_CODE), "Buffer is read-only")
        if (allocated(error)) return
        call check(error, error_name(TEXT_READ_ONLY_CODE), "text_read_only")
        if (allocated(error)) return
        call check(error, error_message(USER_ERROR_CODE), "")
        if (allocated(error)) return
        call check(error, error_name(999), "error")
    end subroutine test_names

    ! Mirrors the error-conditions lists built in data.c:syms_of_data.
    subroutine test_hierarchy(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, condition_matches(TEXT_READ_ONLY_CODE, BUFFER_READ_ONLY_CODE), &
                   "text-read-only is not a buffer-read-only")
        if (allocated(error)) return
        call check(error, condition_matches(TEXT_READ_ONLY_CODE, GENERIC_ERROR_CODE), &
                   "text-read-only is not an error")
        if (allocated(error)) return
        call check(error, condition_matches(OVERFLOW_ERROR_CODE, ARITHMETIC_ERROR_CODE), &
                   "overflow-error is not an arith-error")
        if (allocated(error)) return
        call check(error,.not. condition_matches(QUIT_CODE, GENERIC_ERROR_CODE), &
                   "quit must not be an error")
        if (allocated(error)) return
        call check(error,.not. condition_matches(BUFFER_READ_ONLY_CODE, TEXT_READ_ONLY_CODE), &
                   "parent matched child condition")
        if (allocated(error)) return
        call check(error,.not. condition_matches(OK_CODE, GENERIC_ERROR_CODE), &
                   "success matched a condition")
    end subroutine test_hierarchy

    subroutine test_format(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st

        call check(error, format_error(st), "")
        if (allocated(error)) return
        call set_error(st, END_OF_BUFFER_CODE)
        call check(error, format_error(st), "End of buffer")
        if (allocated(error)) return
        call set_error(st, ARGUMENTS_OUT_OF_RANGE_CODE, "5, 10")
        call check(error, format_error(st), "Args out of range: 5, 10")
        if (allocated(error)) return
        call set_error(st, USER_ERROR_CODE, "No more undo")
        call check(error, format_error(st), "No more undo")
        if (allocated(error)) return
        ! A quit's data is never shown (skeleton's quit with `recursive').
        call set_error(st, QUIT_CODE, "recursive")
        call check(error, format_error(st), "Quit")
        if (allocated(error)) return
        ! (error "Cyclic keymap inheritance")
        call set_error(st, GENERIC_ERROR_CODE, "Cyclic keymap inheritance")
        call check(error, format_error(st), "Cyclic keymap inheritance")
        if (allocated(error)) return
        call set_error(st, VOID_FUNCTION_CODE, "foo")
        call check(error, format_error(st), substitute_quotes("Function is not defined: foo"))
    end subroutine test_format
end module test_error_mod
