! test_inotify_fm.f90 -- tests for inotify_fm.
! Ported from GNU Emacs 31.1 test/src/inotify-tests.el.
!
! Copyright (C) 2012-2026 Free Software Foundation, Inc.
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

!> Ports inotify-valid-p-simple, inotify-file-watch-simple and
!> inotify-file-watch-stop-delivery (waiting with `read_event' as
!> upstream does, until the callback has run or the time is up).
module test_inotify_fm
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_fm, only: status_type, format_error
    use handle_fm, only: handle_type
    use lisp_fm, only: value_type
    use buffer_fm, only: get_buffer_create, set_buffer, kill_buffer, current_buffer, generate_new_buffer_name
    use insdel_fm, only: insert
    use fileio_fm, only: write_region
    use files_fm, only: make_temp_file, delete_file
    use keyboard_fm, only: read_event
    use inotify_fm, only: watch_descriptor_type, inotify_event_type, inotify_add_watch, inotify_rm_watch, &
                          inotify_valid_p, INOTIFY_ALL_EVENTS, INOTIFY_IGNORED
    implicit none(type, external)
    private

    public :: collect_inotify_fm

    !> What the callbacks saw.
    integer :: events = 0
    logical :: success = .false.

    abstract interface
        logical function pred_proc()
            implicit none(type, external)
        end function pred_proc
    end interface

contains

    subroutine collect_inotify_fm(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)
        testsuite = [ &
                    new_unittest("inotify-valid-p-simple", test_valid_p_simple), &
                    new_unittest("inotify-file-watch-simple", test_file_watch_simple), &
                    new_unittest("inotify-file-watch-stop-delivery", test_stop_delivery) &
                    ]
    end subroutine collect_inotify_fm

    !> Write TEXT to FILE (with-temp-file).
    subroutine write_file(file, text)
        character(len=*), intent(in) :: file, text
        type(handle_type) :: old, h
        type(status_type) :: st
        character(len=:), allocatable :: msg
        old = current_buffer()
        call get_buffer_create(generate_new_buffer_name(" *inotify-test*"), h, st)
        call set_buffer(h, st)
        call insert(text, st)
        call write_region(file, .false., st, message=msg)
        call kill_buffer(h, st)
        call set_buffer(old, st)
    end subroutine write_file

    !> Read events for up to SECONDS, until DONE returns true.
    subroutine wait_until(done, seconds)
        procedure(pred_proc) :: done
        real, intent(in) :: seconds
        type(value_type) :: ev
        type(status_type) :: st
        logical :: have
        integer :: k
        do k = 1, int(seconds*10)
            if (done()) return
            call read_event(ev, have, st, seconds=0.1)
        end do
    end subroutine wait_until

    logical function have_events()
        have_events = events > 0
    end function have_events

    logical function have_success()
        have_success = success
    end function have_success

    recursive subroutine count_event(event, status)
        type(inotify_event_type), intent(in) :: event
        type(status_type), intent(out) :: status
        status = status_type()
        if (event%aspects /= 0) events = events + 1
    end subroutine count_event

    recursive subroutine note_ignored(event, status)
        type(inotify_event_type), intent(in) :: event
        type(status_type), intent(out) :: status
        status = status_type()
        if (iand(event%aspects, INOTIFY_IGNORED) /= 0) success = .true.
    end subroutine note_ignored

    !> Simple tests for `inotify_valid_p'.
    subroutine test_valid_p_simple(error)
        type(error_type), allocatable, intent(out) :: error
        type(watch_descriptor_type) :: wd
        call check(error,.not. inotify_valid_p(wd), "invalid descriptor")
        if (allocated(error)) return
        wd%wd = 0
        wd%id = 0
        call check(error,.not. inotify_valid_p(wd), "(0 . 0)")
    end subroutine test_valid_p_simple

    !> Test if watching a normal file works.
    subroutine test_file_watch_simple(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: temp_file
        type(watch_descriptor_type) :: wd
        type(status_type) :: st
        call make_temp_file("inotify-test", temp_file, st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        events = 0
        call inotify_add_watch(temp_file, INOTIFY_ALL_EVENTS, count_event, wd, st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call write_file(temp_file, "Foo"//achar(10))
        call wait_until(have_events, 5.0)
        call check(error, events > 0, "no events")
        if (.not. allocated(error)) call check(error, inotify_valid_p(wd), "valid")
        call inotify_rm_watch(wd, st)
        if (.not. allocated(error)) call check(error,.not. inotify_valid_p(wd), "invalid after rm")
        call delete_file(temp_file, st)
    end subroutine test_file_watch_simple

    !> Test whether IN_IGNORE events are delivered.
    subroutine test_stop_delivery(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: temp_file
        type(watch_descriptor_type) :: wd
        type(status_type) :: st
        call make_temp_file("inotify-test", temp_file, st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        success = .false.
        call inotify_add_watch(temp_file, INOTIFY_ALL_EVENTS, note_ignored, wd, st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call delete_file(temp_file, st)
        call wait_until(have_success, 5.0)
        call check(error, success, "no ignored event")
    end subroutine test_stop_delivery
end module test_inotify_fm
