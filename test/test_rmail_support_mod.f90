! test_rmail_support_mod.f90 -- helpers of the Rmail tests.
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

!> Helpers of the Rmail tests: an mbox file of known messages, reading
!> and writing files, and visiting a file with \[rmail] in a known
!> editor state.
module test_rmail_support_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use error_mod, only: status_type
    use handle_mod, only: handle_type
    use buffer_mod, only: buffers, current, current_buffer, get_file_buffer, is_buffer_live, close_buffer, &
                          set_buffer_modified
    use editing_functions_mod, only: buffer_string
    use bindings_mod, only: initialize_bindings
    use test_support_mod, only: show_alone
    use files_mod, only: create_temporary_file, delete_file
    use file_input_output_mod, only: file_exists
    use rmail_mod, only: rmail, rmail_confirm_expunge, rmail_movemail_variant_in_use, rmail_file_name, &
                         rmail_state_index, rmail_states, rmail_primary_inboxes, rmail_display_summary
    implicit none(type, external)
    private

    public :: set_up_rmail, leave_rmail, write_file, read_file_text, visit_rmail, rmail_state, SAMPLE_MBOX, &
              SAMPLE_INBOX, LF

    !> A newline.
    character(len=*), parameter :: LF = achar(10)

    !> Two unseen messages in mbox format, the first with a quoted "From "
    !> line and an ignored Received field.
    character(len=*), parameter :: SAMPLE_MBOX = &
                                   "From alice@example.com  Mon Nov  2 10:00:00 2026"//LF// &
                                   "Received: from mail.example.com"//LF// &
                                   "From: Alice <alice@example.com>"//LF// &
                                   "To: me@example.org"//LF// &
                                   "Subject: First message"//LF// &
                                   "Date: Mon, 2 Nov 2026 10:00:00 +0000"//LF// &
                                   "X-RMAIL-ATTRIBUTES: ------U-"//LF// &
                                   LF// &
                                   "Hello, this is the first message."//LF// &
                                   ">From the archives."//LF// &
                                   LF// &
                                   "From bob@example.com  Tue Nov  3 11:00:00 2026"//LF// &
                                   "From: Bob <bob@example.com>"//LF// &
                                   "To: me@example.org"//LF// &
                                   "Subject: Second message"//LF// &
                                   "Date: Tue, 3 Nov 2026 11:00:00 +0000"//LF// &
                                   "X-RMAIL-ATTRIBUTES: ------U-"//LF// &
                                   LF// &
                                   "Second body."//LF// &
                                   LF

    !> One new message, as in a mail spool file.
    character(len=*), parameter :: SAMPLE_INBOX = &
                                   "From carol@example.com  Wed Nov  4 12:00:00 2026"//LF// &
                                   "From: Carol <carol@example.com>"//LF// &
                                   "To: me@example.org"//LF// &
                                   "Subject: New mail"//LF// &
                                   "Date: Wed, 4 Nov 2026 12:00:00 +0000"//LF// &
                                   LF// &
                                   "Fresh from the spool."//LF

contains

    !> The editor with known Rmail settings (no questions before
    !> expunging, the movemail of movemail_mod); FILE is a new temporary
    !> file name to use.
    subroutine set_up_rmail(file)
        character(len=:), allocatable, intent(out) :: file
        type(status_type) :: st

        call initialize_bindings()
        call show_alone()
        rmail_confirm_expunge = .false.
        rmail_movemail_variant_in_use = "emacs"
        rmail_display_summary = .false.
        if (allocated(rmail_primary_inboxes)) deallocate (rmail_primary_inboxes)
        call create_temporary_file("rmail-test", file, st)
        call delete_file(file, st)
    end subroutine set_up_rmail

    !> Close the buffers visiting FILE and delete FILE and EXTRA.
    subroutine leave_rmail(file, extra)
        character(len=*), intent(in) :: file
        character(len=*), intent(in), optional :: extra
        type(handle_type) :: h
        type(status_type) :: st

        h = get_file_buffer(file)

        if (is_buffer_live(h)) then
            buffers(h%index)%read_only = .false.
            call close_buffer(h, st)
        end if

        if (file_exists(file)) call delete_file(file, st)
        if (file_exists(file//"~")) call delete_file(file//"~", st)

        if (present(extra)) then
            if (file_exists(extra)) call delete_file(extra, st)
        end if
    end subroutine leave_rmail

    !> Write TEXT to FILE, replacing it.
    subroutine write_file(file, text)
        character(len=*), intent(in) :: file, text
        integer :: unit

        open (newunit=unit, file=file, access="stream", form="unformatted", status="replace", action="write")
        write (unit) text
        close (unit)
    end subroutine write_file

    !> The contents of FILE ("" if it does not exist).
    function read_file_text(file) result(text)
        character(len=*), intent(in) :: file
        character(len=:), allocatable :: text
        integer :: unit, size_bytes
        logical :: exists

        inquire (file=file, exist=exists, size=size_bytes)

        if (.not. exists .or. size_bytes <= 0) then
            text = ""
            return
        end if

        allocate (character(len=size_bytes) :: text)
        open (newunit=unit, file=file, access="stream", form="unformatted", status="old", action="read")
        read (unit) text
        close (unit)
    end function read_file_text

    !> Write CONTENTS to FILE and run \[rmail] on it (no new mail).
    subroutine visit_rmail(file, contents, status)
        character(len=*), intent(in) :: file, contents
        type(status_type), intent(out) :: status

        call write_file(file, contents)
        call rmail(status, file)
    end subroutine visit_rmail

    !> The index of the state of the Rmail buffer visiting FILE.
    integer function rmail_state(file)
        character(len=*), intent(in) :: file

        rmail_state = rmail_state_index(get_file_buffer(file))
    end function rmail_state
end module test_rmail_support_mod
