! test_rmail_mod.f90 -- tests of rmail_mod.
! Ports GNU Emacs 31.1 test/lisp/mail/rmail-tests.el.
!
! Copyright (C) 2015-2026 Free Software Foundation, Inc.
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

!> Rmail: visiting an mbox file, showing a message with its header
!> pruned and the quoted From lines unquoted, the attributes, moving,
!> deleting, undeleting, expunging and saving, getting new mail from an
!> inbox through movemail_mod, searching and toggling the header.
module test_rmail_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_mod, only: status_type, format_error
    use buffer_mod, only: buffers, current, get_file_buffer, set_buffer
    use editing_functions_mod, only: buffer_string, point, goto_char
    use commands_mod, only: commands
    use file_input_output_mod, only: file_exists
    use rmail_mod, only: rmail, rmail_states, rmail_next_undeleted_message, rmail_previous_undeleted_message, &
                         rmail_delete_forward, rmail_undelete_previous_message, rmail_expunge, rmail_expunge_and_save, &
                         rmail_get_header, rmail_get_attribute_names, has_rmail_message_attributes, rmail_toggle_header, &
                         rmail_search, rmail_show_message, rmail_first_message, rmail_last_message, rmail_get_new_mail, &
                         rmail_primary_inboxes, rmail_file_name, is_rmail_message_deleted, rmail_what_message, &
                         rmail_widen, is_rmail_buffers_swapped
    use test_rmail_support_mod, only: set_up_rmail, leave_rmail, write_file, read_file_text, visit_rmail, rmail_state, &
                                      SAMPLE_MBOX, SAMPLE_INBOX, LF
    implicit none(type, external)
    private

    public :: collect_rmail_mod

    !> The first sample message as Rmail shows it.
    character(len=*), parameter :: FIRST_SHOWN = &
                                   "From: Alice <alice@example.com>"//LF// &
                                   "To: me@example.org"//LF// &
                                   "Subject: First message"//LF// &
                                   "Date: Mon, 2 Nov 2026 10:00:00 +0000"//LF// &
                                   LF// &
                                   "Hello, this is the first message."//LF// &
                                   "From the archives."//LF// &
                                   LF

contains

    !> Collect all exported unit tests.
    subroutine collect_rmail_mod(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("rmail-autoload", test_rmail_autoload), &
                    new_unittest("rmail-visit-shows-first-unseen", test_visit), &
                    new_unittest("rmail-attributes", test_attributes), &
                    new_unittest("rmail-move", test_move), &
                    new_unittest("rmail-delete-undelete", test_delete_undelete), &
                    new_unittest("rmail-expunge-and-save", test_expunge_and_save), &
                    new_unittest("rmail-toggle-header", test_toggle_header), &
                    new_unittest("rmail-search", test_search), &
                    new_unittest("rmail-widen", test_widen), &
                    new_unittest("rmail-get-new-mail", test_get_new_mail) &
                    ]
    end subroutine collect_rmail_mod

    !> Test that `rmail_edit_current_message' is a command (upstream: that
    !> it has been autoloaded; Fortmacs builds Rmail in).  Ports
    !> rmail-tests.el:rmail-autoload.
    subroutine test_rmail_autoload(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: file

        call set_up_rmail(file)
        call check(error, associated(commands%rmail_edit_current_message%execute))
    end subroutine test_rmail_autoload

    !> Visiting a file shows its first unseen message, its header pruned
    !> and the quoted "From " line unquoted.
    subroutine test_visit(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: file
        type(status_type) :: st
        integer :: k

        call set_up_rmail(file)
        call visit_rmail(file, SAMPLE_MBOX, st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        k = rmail_state(file)
        call check(error, k > 0)
        if (allocated(error)) return
        call check(error, rmail_states(k)%total_messages == 2_int64)
        if (allocated(error)) return
        call check(error, rmail_states(k)%current_message == 1_int64)
        if (allocated(error)) return
        call check(error, buffers(current)%major_mode == "rmail_mode")
        if (allocated(error)) return
        call check(error, buffer_string() == FIRST_SHOWN, buffer_string())
        if (allocated(error)) return
        call check(error, is_rmail_buffers_swapped())
        call leave_rmail(file)
    end subroutine test_visit

    !> New messages get an attribute header; showing one removes unseen.
    subroutine test_attributes(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: file, value, names
        type(status_type) :: st

        call set_up_rmail(file)
        call visit_rmail(file, SAMPLE_MBOX, st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call rmail_get_header("X-RMAIL-ATTRIBUTES", value, st, 1_int64)
        call check(error, allocated(value))
        if (allocated(error)) return
        call check(error, value == "--------", value)
        if (allocated(error)) return
        call rmail_get_header("X-RMAIL-ATTRIBUTES", value, st, 2_int64)
        call check(error, value == "------U-", value)
        if (allocated(error)) return
        call rmail_get_attribute_names(names, st, 2_int64)
        call check(error, names == "unseen", names)
        if (allocated(error)) return
        call check(error, has_rmail_message_attributes(2_int64, "......U"))
        call leave_rmail(file)
    end subroutine test_attributes

    !> n and p move between messages; < and > to the first and last.
    subroutine test_move(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: file
        type(status_type) :: st
        integer :: k

        call set_up_rmail(file)
        call visit_rmail(file, SAMPLE_MBOX, st)
        k = rmail_state(file)
        call rmail_next_undeleted_message(1_int64, st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call check(error, rmail_states(k)%current_message == 2_int64)
        if (allocated(error)) return
        call check(error, index(buffer_string(), "Subject: Second message") > 0, buffer_string())
        if (allocated(error)) return
        call rmail_previous_undeleted_message(1_int64, st)
        call check(error, rmail_states(k)%current_message == 1_int64)
        if (allocated(error)) return
        call rmail_last_message(st)
        call check(error, rmail_states(k)%current_message == 2_int64)
        if (allocated(error)) return
        call rmail_first_message(st)
        call check(error, rmail_states(k)%current_message == 1_int64)
        if (allocated(error)) return
        call rmail_show_message(st, 5_int64)
        call check(error, rmail_states(k)%current_message == 2_int64)
        call leave_rmail(file)
    end subroutine test_move

    !> d marks the message deleted and moves on; u undeletes it.
    subroutine test_delete_undelete(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: file, value
        type(status_type) :: st
        integer :: k

        call set_up_rmail(file)
        call visit_rmail(file, SAMPLE_MBOX, st)
        k = rmail_state(file)
        call rmail_delete_forward(st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call check(error, is_rmail_message_deleted(1_int64))
        if (allocated(error)) return
        call check(error, rmail_states(k)%current_message == 2_int64)
        if (allocated(error)) return
        call rmail_get_header("X-RMAIL-ATTRIBUTES", value, st, 1_int64)
        call check(error, value == "-D------", value)
        if (allocated(error)) return
        call rmail_undelete_previous_message(1_int64, st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call check(error,.not. is_rmail_message_deleted(1_int64))
        if (allocated(error)) return
        call check(error, rmail_states(k)%current_message == 1_int64)
        call leave_rmail(file)
    end subroutine test_delete_undelete

    !> x erases the deleted messages; s saves the file in mbox format.
    subroutine test_expunge_and_save(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: file, text
        type(status_type) :: st
        integer :: k

        call set_up_rmail(file)
        call visit_rmail(file, SAMPLE_MBOX, st)
        k = rmail_state(file)
        call rmail_delete_forward(st)
        call rmail_expunge_and_save(st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call check(error, rmail_states(k)%total_messages == 1_int64)
        if (allocated(error)) return
        call check(error, index(buffer_string(), "Subject: Second message") > 0, buffer_string())
        if (allocated(error)) return
        text = read_file_text(file)
        call check(error, text == "From bob@example.com  Tue Nov  3 11:00:00 2026"//LF// &
                   "From: Bob <bob@example.com>"//LF// &
                   "To: me@example.org"//LF// &
                   "Subject: Second message"//LF// &
                   "Date: Tue, 3 Nov 2026 11:00:00 +0000"//LF// &
                   "X-RMAIL-ATTRIBUTES: --------"//LF// &
                   LF// &
                   "Second body."//LF// &
                   LF, text)
        call leave_rmail(file)
    end subroutine test_expunge_and_save

    !> t shows the full header, and again the pruned one.
    subroutine test_toggle_header(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: file
        type(status_type) :: st

        call set_up_rmail(file)
        call visit_rmail(file, SAMPLE_MBOX, st)
        call rmail_toggle_header(st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call check(error, index(buffer_string(), "Received: from mail.example.com") > 0, buffer_string())
        if (allocated(error)) return
        call rmail_toggle_header(st)
        call check(error, buffer_string() == FIRST_SHOWN, buffer_string())
        call leave_rmail(file)
    end subroutine test_toggle_header

    !> M-s shows the next message matching a regexp.
    subroutine test_search(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: file
        type(status_type) :: st
        integer :: k

        call set_up_rmail(file)
        call visit_rmail(file, SAMPLE_MBOX, st)
        k = rmail_state(file)
        call rmail_search("Second b[a-z]+", st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call check(error, rmail_states(k)%current_message == 2_int64)
        if (allocated(error)) return
        call rmail_search("no such text", st)
        call check(error, rmail_states(k)%current_message == 2_int64)
        call leave_rmail(file)
    end subroutine test_search

    !> C-c C-w shows the whole mailbox; the message numbers are found
    !> from positions in it.
    subroutine test_widen(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: file
        type(status_type) :: st

        call set_up_rmail(file)
        call visit_rmail(file, SAMPLE_MBOX, st)
        call rmail_widen(st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call check(error,.not. is_rmail_buffers_swapped())
        if (allocated(error)) return
        call check(error, index(buffer_string(), "From bob@example.com  Tue Nov  3") > 0, buffer_string())
        if (allocated(error)) return
        call check(error, rmail_what_message(1_int64) == 1_int64)
        if (allocated(error)) return
        call check(error, rmail_what_message(int(index(buffer_string(), "From bob"), int64)) == 2_int64)
        call leave_rmail(file)
    end subroutine test_widen

    !> \[rmail] on the primary file moves the mail of the inbox into it,
    !> saves it, empties the inbox and shows the new message.
    subroutine test_get_new_mail(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: file, inbox, text
        type(status_type) :: st
        integer :: k

        call set_up_rmail(file)
        inbox = file//"-inbox"
        call write_file(inbox, SAMPLE_INBOX)
        call write_file(file, SAMPLE_MBOX)
        rmail_file_name = file
        rmail_primary_inboxes = [character(len=len(inbox)) :: inbox]
        call rmail(st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        k = rmail_state(file)
        call check(error, rmail_states(k)%total_messages == 3_int64)
        if (allocated(error)) return
        ! The first unseen message is the first of the file.
        call check(error, rmail_states(k)%current_message == 1_int64)
        if (allocated(error)) return
        call check(error, read_file_text(inbox) == "", read_file_text(inbox))
        if (allocated(error)) return
        text = read_file_text(file)
        call check(error, index(text, "Subject: New mail"//LF//"Date: Wed, 4 Nov 2026 12:00:00 +0000"//LF// &
                                "X-RMAIL-ATTRIBUTES: ------U-"//LF//LF//"Fresh from the spool.") > 0, text)
        if (allocated(error)) return
        call check(error,.not. file_exists(inbox//".lock"))
        call leave_rmail(file, inbox)
        deallocate (rmail_primary_inboxes)
        rmail_file_name = "~/RMAIL"
    end subroutine test_get_new_mail
end module test_rmail_mod
