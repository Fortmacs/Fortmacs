! test_movemail_mod.f90 -- tests of movemail_mod.
! Own tests; upstream has no tests of movemail.c.
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

!> Moving mail: the mail is copied, the inbox emptied (or kept with
!> PRESERVE) and the lock file removed; a missing inbox, an existing
!> destination and an empty destination name are reported as movemail
!> reports them.
module test_movemail_mod
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_mod, only: status_type
    use files_mod, only: create_temp_file, delete_file
    use file_input_output_mod, only: file_exists
    use movemail_mod, only: move_mail
    use test_rmail_support_mod, only: write_file, read_file_text, SAMPLE_INBOX
    implicit none(type, external)
    private

    public :: collect_movemail_mod

contains

    !> Collect all exported unit tests.
    subroutine collect_movemail_mod(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("movemail-moves-and-empties", test_move), &
                    new_unittest("movemail-preserve", test_preserve), &
                    new_unittest("movemail-missing-inbox", test_missing), &
                    new_unittest("movemail-existing-destination", test_existing), &
                    new_unittest("movemail-empty-destination", test_empty_destination) &
                    ]
    end subroutine collect_movemail_mod

    ! A new temporary file name INBOX and the destination TOFILE beside it.
    subroutine names(inbox, tofile)
        character(len=:), allocatable, intent(out) :: inbox, tofile
        type(status_type) :: st

        call create_temp_file("movemail-test", inbox, st)
        call delete_file(inbox, st)
        tofile = inbox//"-new"
    end subroutine names

    ! Delete INBOX, TOFILE and the lock file.
    subroutine clean(inbox, tofile)
        character(len=*), intent(in) :: inbox, tofile
        type(status_type) :: st

        if (file_exists(inbox)) call delete_file(inbox, st)
        if (file_exists(tofile)) call delete_file(tofile, st)
        if (file_exists(inbox//".lock")) call delete_file(inbox//".lock", st)
    end subroutine clean

    !> The mail is moved; the inbox is left empty and unlocked.
    subroutine test_move(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: inbox, tofile, errors

        call names(inbox, tofile)
        call write_file(inbox, SAMPLE_INBOX)
        call move_mail(inbox, tofile, .false., errors)
        call check(error, errors == "", errors)
        if (allocated(error)) return
        call check(error, read_file_text(tofile) == SAMPLE_INBOX, "the mail is in the new file")
        if (allocated(error)) return
        call check(error, file_exists(inbox) .and. read_file_text(inbox) == "", "the inbox is empty")
        if (allocated(error)) return
        call check(error,.not. file_exists(inbox//".lock"), "no lock file is left")
        call clean(inbox, tofile)
    end subroutine test_move

    !> With PRESERVE (-p) the inbox keeps its mail.
    subroutine test_preserve(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: inbox, tofile, errors

        call names(inbox, tofile)
        call write_file(inbox, SAMPLE_INBOX)
        call move_mail(inbox, tofile, .true., errors)
        call check(error, errors == "", errors)
        if (allocated(error)) return
        call check(error, read_file_text(inbox) == SAMPLE_INBOX, "the inbox keeps its mail")
        if (allocated(error)) return
        call check(error, read_file_text(tofile) == SAMPLE_INBOX, "the mail is copied")
        call clean(inbox, tofile)
    end subroutine test_preserve

    !> A missing inbox is reported with the system's message.
    subroutine test_missing(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: inbox, tofile, errors

        call names(inbox, tofile)
        call move_mail(inbox, tofile, .false., errors)
        call check(error, errors == "movemail: No such file or directory for "//inbox//achar(10), errors)
        if (allocated(error)) return
        call check(error,.not. file_exists(inbox//".lock"), "the lock file is removed")
        call clean(inbox, tofile)
    end subroutine test_missing

    !> An existing destination is not overwritten.
    subroutine test_existing(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: inbox, tofile, errors

        call names(inbox, tofile)
        call write_file(inbox, SAMPLE_INBOX)
        call write_file(tofile, "old")
        call move_mail(inbox, tofile, .false., errors)
        call check(error, errors == "movemail: File exists for "//tofile//achar(10), errors)
        if (allocated(error)) return
        call check(error, read_file_text(inbox) == SAMPLE_INBOX, "the inbox keeps its mail")
        if (allocated(error)) return
        call check(error, read_file_text(tofile) == "old", "the destination is unchanged")
        call clean(inbox, tofile)
    end subroutine test_existing

    !> An empty destination name is refused.
    subroutine test_empty_destination(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: errors

        call move_mail("/nonexistent/inbox", "", .false., errors)
        call check(error, errors == "movemail: Destination file name is empty"//achar(10), errors)
    end subroutine test_empty_destination
end module test_movemail_mod
