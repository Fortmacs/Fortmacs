! test_filenotify_fm.f90 -- tests for filenotify_fm.
! Ports GNU Emacs 31.1 test/lisp/filenotify-tests.el in part; only the
! inotify tests for local files.
!
! Copyright (C) 2013-2026 Free Software Foundation, Inc.
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

!> Ports file-notify-test00-availability, -test01-add-watch (the
!> errors Fortran's types leave: bad flags and a missing directory),
!> -test02-rm-watch, -test03-events (the inotify expectations), and
!> -test05-file-validity and -test06-dir-validity, for local files.
module test_filenotify_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_fm, only: status_type, format_error, condition_matches, FM_WRONG_TYPE_ARGUMENT, FM_FILE_NOTIFY_ERROR
    use handle_fm, only: handle_type
    use lisp_fm, only: value_type
    use buffer_fm, only: get_buffer_create, set_buffer, kill_buffer, current_buffer, generate_new_buffer_name
    use insdel_fm, only: insert
    use fileio_fm, only: write_region, is_file_directory, file_exists, directory_file_name, make_directory_internal, &
                         rename_file, copy_file, set_file_modes, set_file_times
    use files_fm, only: make_temp_file, delete_file, delete_directory
    use keyboard_fm, only: read_event
    use inotify_fm, only: watch_descriptor_type, operator(==)
    use filenotify_fm, only: file_notify_event_type, file_notify_add_watch, file_notify_rm_watch, &
                             file_notify_rm_all_watches, is_file_notify_valid, file_notify_watch_count, &
                             FILE_NOTIFY_CHANGE, FILE_NOTIFY_ATTRIBUTE_CHANGE, FILE_NOTIFY_CREATED, FILE_NOTIFY_DELETED, &
                             FILE_NOTIFY_CHANGED, FILE_NOTIFY_RENAMED, FILE_NOTIFY_ATTRIBUTE_CHANGED, FILE_NOTIFY_STOPPED
    implicit none(type, external)
    private

    public :: collect_filenotify_fm

    integer, parameter :: CREATED = FILE_NOTIFY_CREATED, DELETED = FILE_NOTIFY_DELETED, CHANGED = FILE_NOTIFY_CHANGED, &
                          RENAMED = FILE_NOTIFY_RENAMED, ATTRIBUTE_CHANGED = FILE_NOTIFY_ATTRIBUTE_CHANGED, &
                          STOPPED = FILE_NOTIFY_STOPPED

    !> The test directory and files (file-notify--test-tmpdir,
    !> -tmpfile and -tmpfile1).
    character(len=:), allocatable :: tmpdir, tmpfile, tmpfile1
    !> The watch descriptors (file-notify--test-desc, -desc1).
    type(watch_descriptor_type) :: desc, desc1
    !> The file the handler's watch is for (file-notify--test-file).
    character(len=:), allocatable :: watched
    !> The actions received (file-notify--test-events), and whether an
    !> event failed file-notify--test-event-test (file-notify--test-results).
    integer :: actions(100)
    integer :: nactions = 0
    character(len=:), allocatable :: bad_event
    !> What first-callback and second-callback saw (results).
    integer :: results(10)
    integer :: nresults = 0
    !> The most actions expected.
    integer :: max_length = 0

    abstract interface
        logical function pred_proc()
            implicit none(type, external)
        end function pred_proc
    end interface

contains

    !> Collect all exported unit tests.
    subroutine collect_filenotify_fm(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("file-notify-test00-availability", test00_availability), &
                    new_unittest("file-notify-test01-add-watch", test01_add_watch), &
                    new_unittest("file-notify-test02-rm-watch", test02_rm_watch), &
                    new_unittest("file-notify-test03-events", test03_events), &
                    new_unittest("file-notify-test05-file-validity", test05_file_validity), &
                    new_unittest("file-notify-test06-dir-validity", test06_dir_validity) &
                    ]
    end subroutine collect_filenotify_fm

    ! ------------------------------------------------------------------
    ! The test environment
    ! ------------------------------------------------------------------

    !> Write TEXT to FILE (write-region ... 'no-message).
    subroutine write_file(file, text)
        character(len=*), intent(in) :: file, text
        type(handle_type) :: old, h
        type(status_type) :: st
        character(len=:), allocatable :: msg

        old = current_buffer()
        call get_buffer_create(generate_new_buffer_name(" *filenotify-test*"), h, st)
        call set_buffer(h, st)
        call insert(text, st)
        call write_region(file, .false., st, message=msg)
        call kill_buffer(h, st)
        call set_buffer(old, st)
    end subroutine write_file

    !> A temporary file name with PREFIX (made, then deleted).
    function temp_name(prefix) result(name)
        character(len=*), intent(in) :: prefix
        character(len=:), allocatable :: name
        type(status_type) :: st

        call make_temp_file(prefix, name, st)
        call delete_file(name, st)
    end function temp_name

    !> with-file-notify-test: a fresh directory and a file in it.
    subroutine setup()
        type(status_type) :: st

        tmpdir = temp_name("file-notify-test-parent")
        call make_directory_internal(tmpdir, st)
        call make_temp_file(tmpdir//"/file-notify-test", tmpfile, st)
        tmpfile1 = ""
        nactions = 0
        nresults = 0
        if (allocated(bad_event)) deallocate (bad_event)
    end subroutine setup

    !> Cleanup after a test (file-notify--test-cleanup).
    subroutine cleanup()
        type(status_type) :: st

        call file_notify_rm_all_watches()
        if (file_exists(tmpfile)) call delete_file(tmpfile, st)

        if (len(tmpfile1) > 0) then
            if (file_exists(tmpfile1)) call delete_file(tmpfile1, st)
        end if

        if (is_file_directory(tmpdir)) call delete_directory(tmpdir, st, .true.)
        desc = watch_descriptor_type()
        desc1 = watch_descriptor_type()
    end subroutine cleanup

    !> Wait for one event (file-notify--test-wait-event).
    subroutine wait_event()
        type(value_type) :: ev
        type(status_type) :: st
        logical :: have

        call read_event(ev, have, st, seconds=0.01)
    end subroutine wait_event

    !> Wait up to SECONDS until UNTIL is true
    !> (file-notify--test-wait-for-events).
    subroutine wait_for_events(seconds, until)
        real, intent(in) :: seconds
        procedure(pred_proc) :: until
        integer :: k

        do k = 1, int(seconds*100)
            if (until()) return
            call wait_event()
        end do
    end subroutine wait_for_events

    logical function no_descriptors()
        no_descriptors = file_notify_watch_count() == 0
    end function no_descriptors

    !> The environment has been cleaned up (file-notify--test-cleanup-p).
    subroutine check_cleanup(error)
        type(error_type), allocatable, intent(out) :: error

        call wait_for_events(3.0, no_descriptors)
        call check(error, no_descriptors(), "watch descriptors left")
    end subroutine check_cleanup

    !> The event handler: check the event and record its action
    !> (file-notify--test-event-handler with file-notify--test-event-test).
    recursive subroutine event_handler(event, status)
        type(file_notify_event_type), intent(in) :: event
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: file

        status = status_type()
        file = directory_file_name(watched)
        if (event%action /= STOPPED .and. .not. (event%descriptor == desc)) bad_event = "descriptor"
        if (index(event%file, file) /= 1) bad_event = "file "//event%file

        if (event%action == RENAMED) then
            if (.not. allocated(event%file1)) then
                bad_event = "no file1"
            else if (index(event%file1, file) /= 1) then
                bad_event = "file1 "//event%file1
            end if
        end if

        ! Do not add lock files, this would confuse the checks.
        if (index(event%file, ".#") > 0) return

        if (nactions < size(actions)) then
            nactions = nactions + 1
            actions(nactions) = event%action
        end if
    end subroutine event_handler

    !> file-notify--test-add-watch with the event handler.
    subroutine add_watch(file, flags, d)
        character(len=*), intent(in) :: file
        integer, intent(in) :: flags
        type(watch_descriptor_type), intent(out) :: d
        type(status_type) :: st

        watched = file
        call file_notify_add_watch(file, flags, event_handler, d, st)
    end subroutine add_watch

    recursive subroutine ignore(event, status)
        type(file_notify_event_type), intent(in) :: event
        type(status_type), intent(out) :: status

        status = status_type()
        if (event%action == 0) continue
    end subroutine ignore

    !> Start collecting actions (the first part of
    !> file-notify--test-with-actions).
    subroutine start_actions()
        call wait_event()
        nactions = 0
        if (allocated(bad_event)) deallocate (bad_event)
    end subroutine start_actions

    logical function actions_done()
        actions_done = nactions >= max_length .or. any(actions(1:nactions) == STOPPED)
    end function actions_done

    !> Wait for the actions and check them against EXPECTED (and
    !> ALTERNATIVE, if given): the end of file-notify--test-with-actions.
    subroutine check_actions(error, expected, alternative)
        type(error_type), allocatable, intent(out) :: error
        integer, intent(in) :: expected(:)
        integer, intent(in), optional :: alternative(:)
        logical :: ok
        character(len=200) :: got

        max_length = size(expected)
        if (present(alternative)) max_length = max(max_length, size(alternative))
        call wait_for_events(3.0, actions_done)

        if (allocated(bad_event)) then
            call check(error, .false., "bad event: "//bad_event)
            return
        end if

        ok = same(expected)
        if (present(alternative)) ok = ok .or. same(alternative)
        write (got, "(*(i0, :, ' '))") actions(1:nactions)
        call check(error, ok, "received actions "//trim(got))

    contains

        logical function same(list)
            integer, intent(in) :: list(:)

            same = size(list) == nactions
            if (same) same = all(list == actions(1:nactions))
        end function same
    end subroutine check_actions

    ! ------------------------------------------------------------------
    ! The tests
    ! ------------------------------------------------------------------

    !> Test availability of `file_notify'.
    subroutine test00_availability(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st

        call setup()
        call file_notify_add_watch(tmpdir, FILE_NOTIFY_CHANGE, ignore, desc, st)
        call check(error, st%ok(), format_error(st))
        call file_notify_rm_watch(desc)
        if (.not. allocated(error)) call check_cleanup(error)
        call cleanup()
    end subroutine test00_availability

    !> Check `file_notify_add_watch'.
    subroutine test01_add_watch(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st

        call setup()
        tmpfile1 = tmpfile//"/0123456789abcdef"
        call delete_file(tmpfile, st)

        body: block
            ! Check, that different valid parameters are accepted.
            call file_notify_add_watch(tmpdir, FILE_NOTIFY_CHANGE, ignore, desc, st)
            call check(error, st%ok(), format_error(st))
            if (allocated(error)) exit body
            call file_notify_rm_watch(desc)
            call file_notify_add_watch(tmpdir, FILE_NOTIFY_ATTRIBUTE_CHANGE, ignore, desc, st)
            call check(error, st%ok(), format_error(st))
            if (allocated(error)) exit body
            call file_notify_rm_watch(desc)
            call file_notify_add_watch(tmpdir, ior(FILE_NOTIFY_CHANGE, FILE_NOTIFY_ATTRIBUTE_CHANGE), ignore, desc, st)
            call check(error, st%ok(), format_error(st))
            if (allocated(error)) exit body
            call file_notify_rm_watch(desc)
            call file_notify_add_watch(tmpfile, ior(FILE_NOTIFY_CHANGE, FILE_NOTIFY_ATTRIBUTE_CHANGE), ignore, desc, st)
            call check(error, st%ok(), format_error(st))
            if (allocated(error)) exit body
            call file_notify_rm_watch(desc)
            ! Check error handling.
            call file_notify_add_watch(tmpdir, 0, ignore, desc, st)
            call check(error, condition_matches(st%code, FM_WRONG_TYPE_ARGUMENT), "flags 0")
            if (allocated(error)) exit body
            call file_notify_add_watch(tmpdir, 8, ignore, desc, st)
            call check(error, condition_matches(st%code, FM_WRONG_TYPE_ARGUMENT), "flags 8")
            if (allocated(error)) exit body
            ! The upper directory of a file must exist.
            call file_notify_add_watch(tmpfile1, ior(FILE_NOTIFY_CHANGE, FILE_NOTIFY_ATTRIBUTE_CHANGE), ignore, desc, st)
            call check(error, st%code == FM_FILE_NOTIFY_ERROR, format_error(st))
            if (allocated(error)) exit body
            call check(error, format_error(st), "Directory does not exist: "//tmpfile)
            if (allocated(error)) exit body
            call check_cleanup(error)
        end block body

        tmpfile1 = ""
        call cleanup()
    end subroutine test01_add_watch

    recursive subroutine first_callback(event, status)
        type(file_notify_event_type), intent(in) :: event
        type(status_type), intent(out) :: status

        status = status_type()

        if (event%action == DELETED) then
            nresults = nresults + 1
            results(nresults) = 1
        end if
    end subroutine first_callback

    recursive subroutine second_callback(event, status)
        type(file_notify_event_type), intent(in) :: event
        type(status_type), intent(out) :: status

        status = status_type()

        if (event%action == DELETED) then
            nresults = nresults + 1
            results(nresults) = 2
        end if
    end subroutine second_callback

    logical function have_results()
        have_results = nresults > 0
    end function have_results

    !> Check `file_notify_rm_watch' and `file_notify_rm_all_watches'.
    subroutine test02_rm_watch(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st

        ! Check, that `file_notify_rm_watch' works.
        call setup()
        call file_notify_add_watch(tmpdir, FILE_NOTIFY_CHANGE, ignore, desc, st)
        call check(error, st%ok(), format_error(st))
        call file_notify_rm_watch(desc)
        ! Check, that any parameter is accepted.
        call file_notify_rm_watch(watch_descriptor_type())
        if (.not. allocated(error)) call check_cleanup(error)
        call cleanup()
        if (allocated(error)) return
        ! Check, that no error is returned removing a watch descriptor twice.
        call setup()
        tmpfile1 = temp_name(tmpdir//"/file-notify-test")
        call write_file(tmpfile, "any text")
        call write_file(tmpfile1, "any text")
        call file_notify_add_watch(tmpfile, FILE_NOTIFY_CHANGE, ignore, desc, st)
        call check(error, st%ok(), format_error(st))
        call file_notify_add_watch(tmpfile1, FILE_NOTIFY_CHANGE, ignore, desc1, st)
        if (.not. allocated(error)) call check(error, st%ok(), format_error(st))
        call file_notify_rm_watch(desc)
        call file_notify_rm_watch(desc)
        call file_notify_rm_watch(desc1)
        if (.not. allocated(error)) call check_cleanup(error)
        call cleanup()
        if (allocated(error)) return
        ! Check, that removing watch descriptors out of order do not harm.
        call setup()
        call write_file(tmpfile, "any text")
        call file_notify_add_watch(tmpfile, FILE_NOTIFY_CHANGE, first_callback, desc, st)
        call check(error, st%ok(), format_error(st))
        call file_notify_add_watch(tmpfile, FILE_NOTIFY_CHANGE, second_callback, desc1, st)
        if (.not. allocated(error)) call check(error, st%ok(), format_error(st))
        ! Remove first watch.
        call file_notify_rm_watch(desc)
        ! Only the second callback shall run.
        call wait_event()
        call delete_file(tmpfile, st)
        call wait_for_events(3.0, have_results)
        if (.not. allocated(error)) call check(error, nresults == 1, "one result")
        if (.not. allocated(error)) call check(error, results(1) == 2, "second callback")
        if (.not. allocated(error)) call check_cleanup(error)
        call cleanup()
        if (allocated(error)) return
        ! Check `file_notify_rm_all_watches'.
        call setup()
        tmpfile1 = temp_name(tmpdir//"/file-notify-test")
        call write_file(tmpfile, "any text")
        call write_file(tmpfile1, "any text")
        call file_notify_add_watch(tmpfile, FILE_NOTIFY_CHANGE, ignore, desc, st)
        call file_notify_add_watch(tmpfile1, FILE_NOTIFY_CHANGE, ignore, desc1, st)
        call file_notify_rm_all_watches()
        call check_cleanup(error)
        call cleanup()
    end subroutine test02_rm_watch

    !> Check file creation/change/removal notifications.
    subroutine test03_events(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st

        ! Check file creation, change and deletion.
        call setup()
        call delete_file(tmpfile, st)
        call add_watch(tmpfile, FILE_NOTIFY_CHANGE, desc)
        call start_actions()
        call write_file(tmpfile, "another text")
        call wait_event()
        call delete_file(tmpfile, st)
        call check_actions(error, [CREATED, CHANGED, DELETED, STOPPED])
        call file_notify_rm_watch(desc)
        if (.not. allocated(error)) call check_cleanup(error)
        call cleanup()
        if (allocated(error)) return
        ! Check file change and deletion.
        call setup()
        call write_file(tmpfile, "any text")
        call add_watch(tmpfile, FILE_NOTIFY_CHANGE, desc)
        call start_actions()
        call write_file(tmpfile, "another text")
        call wait_event()
        call delete_file(tmpfile, st)
        ! There could be one or two `changed' events.
        call check_actions(error, [CHANGED, DELETED, STOPPED], [CHANGED, CHANGED, DELETED, STOPPED])
        call file_notify_rm_watch(desc)
        if (.not. allocated(error)) call check_cleanup(error)
        call cleanup()
        if (allocated(error)) return
        ! Check file creation, change and deletion when watching a
        ! directory.  There must be a `stopped' event when deleting the
        ! directory.
        call setup()
        call delete_file(tmpfile, st)
        call add_watch(tmpdir, FILE_NOTIFY_CHANGE, desc)
        call start_actions()
        call write_file(tmpfile, "any text")
        call wait_event()
        call delete_directory(tmpdir, st, .true.)
        call check_actions(error, [CREATED, CHANGED, DELETED, DELETED, STOPPED])
        call file_notify_rm_watch(desc)
        if (.not. allocated(error)) call check_cleanup(error)
        call cleanup()
        if (allocated(error)) return
        ! Check copy of files inside a directory.
        call setup()
        tmpfile1 = temp_name(tmpdir//"/file-notify-test")
        call delete_file(tmpfile, st)
        call add_watch(tmpdir, FILE_NOTIFY_CHANGE, desc)
        call start_actions()
        call write_file(tmpfile, "any text")
        call wait_event()
        call copy_file(tmpfile, tmpfile1, st)
        ! The next two events shall not be visible.
        call wait_event()
        call set_file_modes(tmpfile, 0, st, nofollow=.true.)
        call wait_event()
        call set_file_times(tmpfile, st, mtime_sec=0_int64, mtime_nsec=0, nofollow=.true.)
        call wait_event()
        call delete_directory(tmpdir, st, .true.)
        call check_actions(error, [CREATED, CHANGED, CREATED, CHANGED, DELETED, DELETED, DELETED, STOPPED])
        call file_notify_rm_watch(desc)
        if (.not. allocated(error)) call check_cleanup(error)
        call cleanup()
        if (allocated(error)) return
        ! Check rename of files inside a directory.
        call setup()
        call delete_file(tmpfile, st)
        tmpfile = temp_name(tmpdir//"/file-notify-test")
        tmpfile1 = temp_name(tmpdir//"/file-notify-test")
        call add_watch(tmpdir, FILE_NOTIFY_CHANGE, desc)
        call start_actions()
        call write_file(tmpfile, "any text")
        call wait_event()
        call rename_file(tmpfile, tmpfile1, st)
        ! After the rename, we won't get events anymore.
        call wait_event()
        call delete_directory(tmpdir, st, .true.)
        call check_actions(error, [CREATED, CHANGED, RENAMED, DELETED, DELETED, STOPPED])
        call file_notify_rm_watch(desc)
        if (.not. allocated(error)) call check_cleanup(error)
        call cleanup()
        if (allocated(error)) return
        ! Check attribute change.
        call setup()
        call write_file(tmpfile, "any text")
        call add_watch(tmpfile, FILE_NOTIFY_ATTRIBUTE_CHANGE, desc)
        call start_actions()
        call write_file(tmpfile, "any text")
        call wait_event()
        call set_file_modes(tmpfile, 0, st, nofollow=.true.)
        call wait_event()
        call set_file_times(tmpfile, st, mtime_sec=0_int64, mtime_nsec=0, nofollow=.true.)
        call wait_event()
        call delete_file(tmpfile, st)
        call check_actions(error, [ATTRIBUTE_CHANGED, ATTRIBUTE_CHANGED])
        call file_notify_rm_watch(desc)
        if (.not. allocated(error)) call check_cleanup(error)
        call cleanup()
    end subroutine test03_events

    logical function desc_invalid()
        desc_invalid = .not. is_file_notify_valid(desc)
    end function desc_invalid

    !> Check `is_file_notify_valid' for files.
    subroutine test05_file_validity(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st

        call setup()
        call write_file(tmpfile, "any text")
        call file_notify_add_watch(tmpfile, FILE_NOTIFY_CHANGE, ignore, desc, st)
        call check(error, is_file_notify_valid(desc), "valid")
        ! After calling `file_notify_rm_watch', the descriptor is not valid
        ! anymore.
        call file_notify_rm_watch(desc)
        if (.not. allocated(error)) call check(error,.not. is_file_notify_valid(desc), "invalid after rm")
        if (.not. allocated(error)) call check_cleanup(error)
        call cleanup()
        if (allocated(error)) return
        call setup()
        call write_file(tmpfile, "any text")
        call add_watch(tmpfile, FILE_NOTIFY_CHANGE, desc)
        call check(error, is_file_notify_valid(desc), "valid")

        if (allocated(error)) then
            call cleanup()
            return
        end if

        call start_actions()
        call write_file(tmpfile, "another text")
        call wait_event()
        call delete_file(tmpfile, st)
        call check_actions(error, [CHANGED, DELETED, STOPPED], [CHANGED, CHANGED, DELETED, STOPPED])
        call wait_event()
        ! After deleting the file, the descriptor is not valid anymore.
        if (.not. allocated(error)) call check(error,.not. is_file_notify_valid(desc), "invalid after delete")
        call file_notify_rm_watch(desc)
        if (.not. allocated(error)) call check_cleanup(error)
        call cleanup()
        if (allocated(error)) return
        call setup()
        call delete_file(tmpfile, st)
        call add_watch(tmpdir, FILE_NOTIFY_CHANGE, desc)
        call check(error, is_file_notify_valid(desc), "valid")

        if (allocated(error)) then
            call cleanup()
            return
        end if

        call start_actions()
        call write_file(tmpfile, "any text")
        call wait_event()
        call delete_directory(tmpdir, st, .true.)
        call check_actions(error, [CREATED, CHANGED, DELETED, DELETED, STOPPED])
        ! After deleting the parent directory, the descriptor must not be
        ! valid anymore.
        if (.not. allocated(error)) call check(error,.not. is_file_notify_valid(desc), "invalid after delete")
        if (.not. allocated(error)) call check_cleanup(error)
        call cleanup()
    end subroutine test05_file_validity

    !> Check `is_file_notify_valid' for directories.
    subroutine test06_dir_validity(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st

        call setup()
        call file_notify_add_watch(tmpdir, FILE_NOTIFY_CHANGE, ignore, desc, st)
        call check(error, is_file_notify_valid(desc), "valid")
        ! After removing the watch, the descriptor must not be valid
        ! anymore.
        call file_notify_rm_watch(desc)
        call wait_for_events(3.0, desc_invalid)
        if (.not. allocated(error)) call check(error,.not. is_file_notify_valid(desc), "invalid after rm")
        if (.not. allocated(error)) call check_cleanup(error)
        call cleanup()
        if (allocated(error)) return
        call setup()
        call file_notify_add_watch(tmpdir, FILE_NOTIFY_CHANGE, ignore, desc, st)
        call check(error, is_file_notify_valid(desc), "valid")
        ! After deleting the directory, the descriptor must not be valid
        ! anymore.
        call delete_directory(tmpdir, st, .true.)
        call wait_for_events(3.0, desc_invalid)
        if (.not. allocated(error)) call check(error,.not. is_file_notify_valid(desc), "invalid after delete")
        if (.not. allocated(error)) call check_cleanup(error)
        call cleanup()
    end subroutine test06_dir_validity
end module test_filenotify_fm
