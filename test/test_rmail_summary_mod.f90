! test_rmail_summary_mod.f90 -- tests of rmail_summary_mod.
! Ports GNU Emacs 31.1 test/lisp/mail/rmailsum-tests.el in part; the tests
! of rmail-summary-name-or-address and rmail-summary-recipient-names wait
! for mail-parse.el.
!
! Copyright (C) 2026 Free Software Foundation, Inc.
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

!> The summary: its lines, moving in it, deleting from it and the
!> summaries by senders and labels.
module test_rmail_summary_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_mod, only: status_type, format_error
    use buffer_mod, only: buffers, current, set_buffer
    use editing_functions_mod, only: buffer_string
    use startup_mod, only: user_mail_address
    use rmail_mod, only: rmail_states, is_rmail_message_deleted
    use rmail_summary_mod, only: rmail_summary, rmail_summary_by_senders, rmail_summary_recipient_strip_quoted_names, &
                                 rmail_summary_goto_message
    use test_rmail_support_mod, only: set_up_rmail, leave_rmail, visit_rmail, rmail_state, SAMPLE_MBOX, LF
    implicit none(type, external)
    private

    public :: collect_rmail_summary_mod

    !> The summary of the sample file, its first message shown.
    character(len=*), parameter :: SUMMARY_TEXT = &
                                   "    1   2-Nov          alice@example.com  [11] First message"//LF// &
                                   "    2-  3-Nov            bob@example.com   [9] Second message"//LF

contains

    !> Collect all exported unit tests.
    subroutine collect_rmail_summary_mod(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("rmailsum-tests-recipient-strip-quoted-names-first-line", test_strip_first_line), &
                    new_unittest("rmail-summary-lines", test_summary_lines), &
                    new_unittest("rmail-summary-by-senders", test_by_senders), &
                    new_unittest("rmail-summary-goto-message", test_goto) &
                    ]
    end subroutine collect_rmail_summary_mod

    !> Ports rmailsum-tests.el:rmailsum-tests-recipient-strip-quoted-names-first-line.
    subroutine test_strip_first_line(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: shown

        call rmail_summary_recipient_strip_quoted_names("Foo Bar <foo@example.test>,"//LF// &
                                                        " Baz Quux <baz@example.test>", shown)
        call check(error, shown == "foo@example.test,", shown)
    end subroutine test_strip_first_line

    !> h makes a line per message: number, deleted or unseen, date,
    !> sender, lines, labels and subject.
    subroutine test_summary_lines(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: file
        type(status_type) :: st

        call set_up_rmail(file)
        user_mail_address = "me@example.org"
        call visit_rmail(file, SAMPLE_MBOX, st)
        call rmail_summary(st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call check(error, buffers(current)%major_mode == "rmail_summary_mode", buffers(current)%major_mode)
        if (allocated(error)) return
        call check(error, buffer_string() == SUMMARY_TEXT, buffer_string())
        call leave_rmail(file)
    end subroutine test_summary_lines

    !> C-M-f shows the messages of a sender only.
    subroutine test_by_senders(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: file
        type(status_type) :: st

        call set_up_rmail(file)
        user_mail_address = "me@example.org"
        call visit_rmail(file, SAMPLE_MBOX, st)
        call rmail_summary_by_senders("bob", st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call check(error, index(buffer_string(), "Second message") > 0 .and. index(buffer_string(), "First message") == 0, &
                   buffer_string())
        call leave_rmail(file)
    end subroutine test_by_senders

    !> j in the summary shows the message of the line.
    subroutine test_goto(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: file
        type(status_type) :: st
        logical :: found
        integer :: k

        call set_up_rmail(file)
        user_mail_address = "me@example.org"
        call visit_rmail(file, SAMPLE_MBOX, st)
        k = rmail_state(file)
        call rmail_summary(st)
        call rmail_summary_goto_message(st, 2_int64, found=found)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call check(error, found .and. rmail_states(k)%current_message == 2_int64)
        call leave_rmail(file)
    end subroutine test_goto
end module test_rmail_summary_mod
