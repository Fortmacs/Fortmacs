! movemail_mod.f90 -- move mail from a local spool file, with locking.
! Ported from GNU Emacs 31.1 lib-src/movemail.c (in part: the local
! inbox with a lock file).
!
! Copyright (C) 1986, 1992-1994, 1996, 1999, 2001-2026 Free Software
! Foundation, Inc.
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

!> Moving mail out of a local inbox (a mail spool file) into a new file,
!> as the program movemail does for Rmail: the inbox is locked with a
!> lock file named after it with ".lock" appended (made by linking a
!> temporary file to that name, so that two movers never both get the
!> lock), copied to the new file and then emptied unless it is to be
!> preserved.  Upstream this is a separate program that Rmail runs; here
!> it runs inside Fortmacs and gives back what the program would have
!> written on its standard error.  See docs/status/movemail_mod.md for
!> what is deferred (POP and the system locks).
module movemail_mod
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use system_dependent_mod, only: fortmacs_mkostemp, fortmacs_open_read, fortmacs_open_mode, fortmacs_creat, &
                                    fortmacs_read, fortmacs_full_write, fortmacs_fsync, fortmacs_strerror, fortmacs_sleep, &
                                    fortmacs_umask, fortmacs_set_umask, close_fd, sys_link, sys_unlink, sys_stat, &
                                    file_stat_type, fortmacs_clock_realtime, O_WRONLY, O_CREAT, O_EXCL, EEXIST, ENOENT, &
                                    EINVAL
    use error_mod, only: status_type
    implicit none(type, external)
    private

    public :: move_mail

    character(len=*), parameter :: LF = achar(10)

contains

    !> Move the mail in the local file INBOX to the new file DESTINATION,
    !> holding the lock file INBOX.lock meanwhile, and empty INBOX unless
    !> PRESERVE is true (movemail's -p).  ERRORS is what movemail writes on
    !> its standard error: "" when the mail was moved, else lines starting
    !> with "movemail: ".  Port of movemail.c:main (the local inbox,
    !> without MAIL_USE_SYSTEM_LOCK or maillock: a lock file is used; no
    !> privileges to drop, so no child process).
    subroutine move_mail(inbox, destination, preserve, errors)
        character(len=*), intent(in) :: inbox, destination
        logical, intent(in) :: preserve
        character(len=:), allocatable, intent(out) :: errors
        character(len=:), allocatable :: lockname, tempname
        character(len=1024) :: buffer
        type(file_stat_type) :: st
        type(status_type) :: rst, wst
        integer(int64) :: nread, written, now
        integer(int32) :: desc, tem, indesc, outdesc, err, mask, nanoseconds
        integer :: dirlen
        logical :: lockname_unlinked

        errors = ""

        if (len(destination) == 0) then
            call report_fatal_error("Destination file name is empty")
            return
        end if

        ! Use a lock file named after our first argument with .lock
        ! appended: if it exists, the mail file is locked.  This locking
        ! mechanism is required by the mailer (on systems which use it)
        ! to prevent loss of mail.
        lockname_unlinked = .false.
        lockname = inbox//".lock"
        dirlen = scan(inbox, "/", back=.true.)

        do
            ! Create the lock file, but not under the lock file name.
            ! Give up if cannot do that.
            tempname = inbox(1:dirlen)//"EXXXXXX"
            err = fortmacs_mkostemp(tempname, desc)

            if (err /= 0) then
                call report_error("error while creating what would become the lock file")
                call report_fatal_error(fortmacs_strerror(err)//" for "//tempname)
                return
            end if

            call close_fd(desc)
            err = sys_link(tempname, lockname)

            if (err /= 0 .and. err /= EEXIST) then
                call report_fatal_error(fortmacs_strerror(err)//" for "//lockname)
                return
            end if

            tem = sys_unlink(tempname)
            if (err == 0) exit
            call fortmacs_sleep(1)

            ! If lock file is five minutes old, unlock it.  Five minutes
            ! should be good enough to cope with crashes and wedgitude, and
            ! long enough to avoid being fooled by time differences between
            ! machines.
            if (.not. lockname_unlinked) then
                if (sys_stat(lockname, st) == 0) then
                    call fortmacs_clock_realtime(now, nanoseconds)

                    if (st%ctime_sec < now - 300) then
                        tem = sys_unlink(lockname)
                        lockname_unlinked = tem == 0 .or. tem == ENOENT
                    end if
                end if
            end if
        end do

        err = fortmacs_open_read(inbox, indesc)

        if (err /= 0) then
            call report_fatal_error(fortmacs_strerror(err)//" for "//inbox, lockname)
            return
        end if

        ! Make sure the user can read the output file.
        mask = fortmacs_umask()
        call fortmacs_set_umask(iand(mask, int(o'377', int32)))
        err = fortmacs_open_mode(destination, ior(O_WRONLY, ior(O_CREAT, O_EXCL)), int(o'666', int32), outdesc)
        call fortmacs_set_umask(mask)

        if (err /= 0) then
            call close_fd(indesc)
            call report_fatal_error(fortmacs_strerror(err)//" for "//destination, lockname)
            return
        end if

        do
            call fortmacs_read(indesc, buffer, nread, rst, err)

            if (.not. rst%ok()) then
                call close_fd(indesc)
                call close_fd(outdesc)
                call report_fatal_error(fortmacs_strerror(err)//" for "//inbox, lockname)
                return
            end if

            call fortmacs_full_write(outdesc, buffer(1:nread), written, wst, err)

            if (.not. wst%ok() .or. written /= nread) then
                tem = sys_unlink(destination)
                call close_fd(indesc)
                call close_fd(outdesc)
                call report_fatal_error(fortmacs_strerror(err)//" for "//destination, lockname)
                return
            end if

            if (nread < len(buffer)) exit
        end do

        err = fortmacs_fsync(outdesc)

        if (err /= 0 .and. err /= EINVAL) then
            call close_fd(indesc)
            call close_fd(outdesc)
            tem = sys_unlink(destination)
            call report_fatal_error(fortmacs_strerror(err)//" for "//destination, lockname)
            return
        end if

        ! Check to make sure no errors before we zap the inbox.
        call close_fd(outdesc)
        call close_fd(indesc)

        if (.not. preserve) then
            ! Get rid of the contents of the input file.
            err = fortmacs_creat(inbox, int(o'600', int32), desc)
            if (err == 0) call close_fd(desc)
        end if

        tem = sys_unlink(lockname)

    contains

        ! Report an error.  Port of movemail.c:error.
        subroutine report_error(text)
            character(len=*), intent(in) :: text

            errors = errors//"movemail: "//text//LF
        end subroutine report_error

        ! Report an error and give up, deleting the lock file LOCK if
        ! present.  Port of movemail.c:fatal.
        subroutine report_fatal_error(text, lock)
            character(len=*), intent(in) :: text
            character(len=*), intent(in), optional :: lock
            integer(int32) :: ignored

            if (present(lock)) ignored = sys_unlink(lock)
            call report_error(text)
        end subroutine report_fatal_error
    end subroutine move_mail
end module movemail_mod
