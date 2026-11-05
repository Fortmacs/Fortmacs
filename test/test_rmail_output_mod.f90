! test_rmail_output_mod.f90 -- tests of rmail_output_mod.
! Own tests; upstream has no rmailout-tests.el.
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

!> Writing messages to files: o appends the message with its whole
!> header to an mbox file and marks it filed, w writes the body, and C-o
!> waits for mail-mbox-from.
module test_rmail_output_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_mod, only: status_type, format_error, FM_VOID_FUNCTION
    use files_mod, only: delete_file
    use file_input_output_mod, only: file_exists
    use rmail_mod, only: get_rmail_header
    use rmail_output_mod, only: output_rmail_message, output_rmail_message_as_seen, output_rmail_message_body_to_file
    use test_rmail_support_mod, only: set_up_rmail, leave_rmail, visit_rmail, write_file, read_file_text, SAMPLE_MBOX, LF
    implicit none(type, external)
    private

    public :: collect_rmail_output_mod

contains

    !> Collect all exported unit tests.
    subroutine collect_rmail_output_mod(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("rmail-output", test_output), &
                    new_unittest("rmail-output-body-to-file", test_output_body), &
                    new_unittest("rmail-output-as-seen-needs-mbox-from", test_output_as_seen) &
                    ]
    end subroutine collect_rmail_output_mod

    !> o appends the whole message and sets the filed attribute.
    subroutine test_output(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: file, output, text, value
        type(status_type) :: st

        call set_up_rmail(file)
        output = file//"-out"
        call write_file(output, "")
        call visit_rmail(file, SAMPLE_MBOX, st)
        call output_rmail_message(output, st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        text = read_file_text(output)
        call check(error, text == "From alice@example.com  Mon Nov  2 10:00:00 2026"//LF// &
                   "Received: from mail.example.com"//LF// &
                   "From: Alice <alice@example.com>"//LF// &
                   "To: me@example.org"//LF// &
                   "Subject: First message"//LF// &
                   "Date: Mon, 2 Nov 2026 10:00:00 +0000"//LF// &
                   "X-RMAIL-ATTRIBUTES: --------"//LF// &
                   LF// &
                   "Hello, this is the first message."//LF// &
                   ">From the archives."//LF// &
                   LF, text)
        if (allocated(error)) return
        call get_rmail_header("X-RMAIL-ATTRIBUTES", value, st, 1_int64)
        call check(error, value == "---F----", value)
        call leave_rmail(file, output)
    end subroutine test_output

    !> w writes the body of the message as shown.
    subroutine test_output_body(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: file, output, text
        type(status_type) :: st

        call set_up_rmail(file)
        output = file//"-body"
        call visit_rmail(file, SAMPLE_MBOX, st)
        call output_rmail_message_body_to_file(output, st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        text = read_file_text(output)
        call check(error, text == "Hello, this is the first message."//LF//"From the archives."//LF//LF, text)
        call leave_rmail(file, output)
    end subroutine test_output_body

    !> C-o of a shown message, which has no "From " line, needs
    !> mail-mbox-from (Deferred): a `void_function' error condition.
    subroutine test_output_as_seen(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: file, output
        type(status_type) :: st

        call set_up_rmail(file)
        output = file//"-seen"
        call write_file(output, "")
        call visit_rmail(file, SAMPLE_MBOX, st)
        call output_rmail_message_as_seen(output, st)
        call check(error, st%code == FM_VOID_FUNCTION, format_error(st))
        call leave_rmail(file, output)
    end subroutine test_output_as_seen
end module test_rmail_output_mod
