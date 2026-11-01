! test_autorevert_mod.f90 -- tests for autorevert_mod.
! Ports GNU Emacs 31.1 test/lisp/autorevert-tests.el in part; the others
! are not ported yet.
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

!> Ports auto-revert-test00-auto-revert-mode, -test02-auto-revert-deleted-file,
!> -test03-auto-revert-tail-mode, -test05-global-notify, -test06-write-file
!> and -test08-auto-revert-inhibit-auto-revert, for local files.  The
!> messages are captured from *Messages*.
module test_autorevert_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_mod, only: status_type, format_error
    use handle_mod, only: handle_type
    use value_mod, only: value_type
    use buffer_mod, only: buffers, current, get_buffer, get_or_make_buffer, set_buffer, close_buffer, current_buffer, &
                          generate_new_buffer_name, is_buffer_live, restore_buffer_modified, erase_buffer
    use insdel_mod, only: insert
    use editfns_mod, only: buffer_string
    use fileio_mod, only: write_region, set_file_times
    use files_mod, only: create_temp_file, delete_file, find_file_noselect, save_buffer, write_file, before_revert_hook
    use sysdep_mod, only: emacs_time
    use keyboard_mod, only: read_event
    use modes_mod, only: minor_modes
    use bindings_mod, only: init_bindings
    use filenotify_mod, only: file_notify_rm_all_watches
    use autorevert_mod, only: auto_revert_interval, auto_revert_lockout_interval, auto_revert_stop_on_user_input, &
                              auto_revert_notify_exclude_dir_regexp, inhibit_auto_revert, auto_revert_use_notify
    implicit none(type, external)
    private

    public :: collect_autorevert_mod

    !> The length of *Messages* when the capture started.
    integer :: messages_start = 0
    !> The buffer whose text `buffer_text_is' waits for, and the text.
    type(handle_type) :: wait_buffer
    character(len=:), allocatable :: wait_text, wait_message
    !> Settings saved by `begin' (with-auto-revert-test).
    real :: interval_orig, lockout_orig

    abstract interface
        logical function test_procedure()
            implicit none(type, external)
        end function test_procedure
    end interface

contains

    !> Collect all exported unit tests.
    subroutine collect_autorevert_mod(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("auto-revert-test00-auto-revert-mode", test00_auto_revert_mode), &
                    new_unittest("auto-revert-test02-auto-revert-deleted-file", test02_deleted_file), &
                    new_unittest("auto-revert-test03-auto-revert-tail-mode", test03_tail_mode), &
                    new_unittest("auto-revert-test05-global-notify", test05_global_notify), &
                    new_unittest("auto-revert-test06-write-file", test06_write_file), &
                    new_unittest("auto-revert-test08-auto-revert-inhibit-auto-revert", test08_inhibit) &
                    ]
    end subroutine collect_autorevert_mod

    ! ------------------------------------------------------------------
    ! The test environment
    ! ------------------------------------------------------------------

    !> with-auto-revert-test: short intervals.
    subroutine begin()
        call init_bindings()
        auto_revert_stop_on_user_input = .false.
        auto_revert_notify_exclude_dir_regexp = "nothing-to-be-excluded"
        interval_orig = auto_revert_interval
        lockout_orig = auto_revert_lockout_interval
        auto_revert_interval = 0.1
        auto_revert_lockout_interval = 0.05
    end subroutine begin

    !> The end of with-auto-revert-test.
    subroutine finish()
        auto_revert_interval = interval_orig
        auto_revert_lockout_interval = lockout_orig
        call file_notify_rm_all_watches()
    end subroutine finish

    !> Close BUFFER, unmodified, and delete FILE.
    subroutine close_and_delete(buffer, file)
        type(handle_type), intent(in) :: buffer
        character(len=*), intent(in) :: file
        type(status_type) :: st

        if (is_buffer_live(buffer)) then
            call set_buffer(buffer, st)
            call restore_buffer_modified(.false.)
            call close_buffer(buffer, st)
        end if

        call delete_file(file, st)
    end subroutine close_and_delete

    !> Write STRING to FILE (auto-revert-test--write-region).
    subroutine write_text(text, file)
        character(len=*), intent(in) :: text, file
        type(handle_type) :: old, h
        type(status_type) :: st
        character(len=:), allocatable :: msg

        old = current_buffer()
        call get_or_make_buffer(generate_new_buffer_name(" *autorevert-test*"), h, st)
        call set_buffer(h, st)
        call insert(text, st)
        call write_region(file, .false., st, message=msg)
        call close_buffer(h, st)
        call set_buffer(old, st)
    end subroutine write_text

    !> Write STRING to FILE, setting its modification time a minute
    !> back in order to trigger auto-revert (auto-revert-test--write-file).
    subroutine write_text_old(text, file)
        character(len=*), intent(in) :: text, file
        type(status_type) :: st

        call write_text(text, file)
        call set_file_times(file, st, mtime_sec=emacs_time() - 60, mtime_nsec=0)
    end subroutine write_text_old

    !> The text of *Messages*.
    function messages() result(text)
        character(len=:), allocatable :: text
        type(handle_type) :: h, old
        type(status_type) :: st

        text = ""
        h = get_buffer("*Messages*")
        if (.not. is_buffer_live(h)) return
        old = current_buffer()
        call set_buffer(h, st)
        text = buffer_string()
        call set_buffer(old, st)
    end function messages

    !> Start capturing messages (ert-with-message-capture).
    subroutine capture_messages()
        messages_start = len(messages())
    end subroutine capture_messages

    !> Whether the captured messages include `wait_message'.
    logical function message_seen()
        character(len=:), allocatable :: text

        text = messages()
        message_seen = .false.
        if (len(text) > messages_start) message_seen = index(text(messages_start + 1:), wait_message) > 0
    end function message_seen

    !> The text of BUFFER.
    function text_of(buffer) result(text)
        type(handle_type), intent(in) :: buffer
        character(len=:), allocatable :: text
        type(handle_type) :: old
        type(status_type) :: st

        old = current_buffer()
        call set_buffer(buffer, st)
        text = buffer_string()
        call set_buffer(old, st)
    end function text_of

    logical function buffer_text_is()
        buffer_text_is = text_of(wait_buffer) == wait_text .and. len(text_of(wait_buffer)) == len(wait_text)
    end function buffer_text_is

    !> Wait until TEST is true, or MAX_WAIT seconds elapsed
    !> (auto-revert-test--wait-for).
    subroutine wait_for(test, max_wait)
        procedure(test_procedure) :: test
        real, intent(in) :: max_wait
        type(value_type) :: ev
        type(status_type) :: st
        logical :: have
        integer :: k

        do k = 1, int(max_wait*10)
            if (test()) return
            call read_event(ev, have, st, seconds=0.1)
        end do
    end subroutine wait_for

    !> Wait until a message reports reversion of BUFFER
    !> (auto-revert--wait-for-revert).
    subroutine wait_for_revert(buffer)
        type(handle_type), intent(in) :: buffer

        wait_message = "Reverting buffer `"//buffers(buffer%index)%name//"'"
        call wait_for(message_seen, auto_revert_interval + 1)
    end subroutine wait_for_revert

    !> Wait until BUFFER has the contents TEXT
    !> (auto-revert-test--wait-for-buffer-text).
    subroutine wait_for_buffer_text(buffer, text, max_wait)
        type(handle_type), intent(in) :: buffer
        character(len=*), intent(in) :: text
        real, intent(in) :: max_wait

        wait_buffer = buffer
        wait_text = text
        call wait_for(buffer_text_is, max_wait)
    end subroutine wait_for_buffer_text

    ! ------------------------------------------------------------------
    ! The tests
    ! ------------------------------------------------------------------

    !> Check autorevert for a file.
    subroutine test00_auto_revert_mode(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: tmpfile
        type(handle_type) :: buffer
        type(status_type) :: st

        call begin()
        call create_temp_file("auto-revert-test", tmpfile, st)

        body: block
            call write_text("any text", tmpfile)
            call find_file_noselect(tmpfile, buffer, st)
            call set_buffer(buffer, st)
            call capture_messages()
            call check(error, buffer_string(), "any text")
            if (allocated(error)) exit body
            call minor_modes%auto_revert_mode%enable(st)
            call check(error, minor_modes%auto_revert_mode%enabled(), "auto_revert_mode")
            if (allocated(error)) exit body
            ! The initial call of `auto_revert_handler' locks file
            ! notification; wait until the lockout has ended.
            call wait_for(always_false, 0.3)
            call write_text_old("another text", tmpfile)
            ! Check, that the buffer has been reverted.
            call wait_for_revert(buffer)
            call check(error, index(buffer_string(), "another text") > 0, "reverted: "//buffer_string())
            if (allocated(error)) exit body
            ! When the buffer is modified, it shall not be reverted.
            call capture_messages()
            call restore_buffer_modified(.true.)
            call write_text_old("any text", tmpfile)
            ! Check, that the buffer hasn't been reverted.
            call wait_for_revert(buffer)
            call check(error, index(buffer_string(), "any text") == 0, "not reverted: "//buffer_string())
        end block body

        call close_and_delete(buffer, tmpfile)
        call finish()
    end subroutine test00_auto_revert_mode

    logical function always_false()
        always_false = .false.
    end function always_false

    !> before-revert-hook: delete the visited file.
    recursive subroutine delete_visited_file(status)
        type(status_type), intent(out) :: status

        call delete_file(buffers(current)%filename, status)
    end subroutine delete_visited_file

    !> Check autorevert for a deleted file.
    subroutine test02_deleted_file(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: tmpfile
        type(handle_type) :: buffer
        type(status_type) :: st

        call begin()
        call create_temp_file("auto-revert-test", tmpfile, st)

        body: block
            call write_text("any text", tmpfile)
            call find_file_noselect(tmpfile, buffer, st)
            call set_buffer(buffer, st)
            call check(error, buffer_string(), "any text")
            if (allocated(error)) exit body
            call minor_modes%auto_revert_mode%enable(st)
            call check(error, minor_modes%auto_revert_mode%enabled(), "auto_revert_mode")
            if (allocated(error)) exit body
            ! Remove file while reverting.  We simulate this by
            ! modifying `before_revert_hook'.
            call before_revert_hook%append(delete_visited_file, local=.true.)
            call capture_messages()
            call write_text_old("another text", tmpfile)
            call wait_for_revert(buffer)
            ! Check, that the buffer hasn't been reverted.  File
            ! notification should be disabled, falling back to polling.
            call check(error, index(buffer_string(), "any text") > 0, "not reverted: "//buffer_string())
            if (allocated(error)) exit body
            ! Once the file has been recreated, the buffer shall be
            ! reverted.
            call before_revert_hook%clear_local()
            call capture_messages()
            call write_text_old("another text", tmpfile)
            call wait_for_revert(buffer)
            call check(error, index(buffer_string(), "another text") > 0, "reverted: "//buffer_string())
            if (allocated(error)) exit body
            ! An empty file shall still be reverted.
            call capture_messages()
            call write_text_old("", tmpfile)
            call wait_for_revert(buffer)
            call check(error, buffer_string(), "")
        end block body

        call close_and_delete(buffer, tmpfile)
        call finish()
    end subroutine test02_deleted_file

    !> Check autorevert tail mode.
    subroutine test03_tail_mode(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: tmpfile
        type(handle_type) :: buffer
        type(status_type) :: st

        call begin()
        call create_temp_file("auto-revert-test", tmpfile, st)

        body: block
            call capture_messages()
            call write_text("any text", tmpfile)
            call find_file_noselect(tmpfile, buffer, st)
            call set_buffer(buffer, st)
            call minor_modes%auto_revert_tail_mode%enable(st)
            call check(error, minor_modes%auto_revert_tail_mode%enabled(), "auto_revert_tail_mode")
            if (allocated(error)) exit body
            call erase_buffer(st)
            call insert("modified text"//achar(10), st)
            call restore_buffer_modified(.false.)
            ! Modify file (appending "another text").
            call write_text_old("any textanother text", tmpfile)
            ! Check, that the buffer has been reverted.
            call wait_for_revert(buffer)
            call check(error, index(buffer_string(), "modified text"//achar(10)//"another text") > 0, &
                       "tail: "//buffer_string())
        end block body

        call close_and_delete(buffer, tmpfile)
        call finish()
    end subroutine test03_tail_mode

    !> Test `global_auto_revert_mode'.
    subroutine test05_global_notify(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: file_1, file_2, file_3, file_2b
        type(handle_type) :: buffer_1, buffer_2, buffer_3
        type(status_type) :: st

        call begin()
        auto_revert_use_notify%value = .true.
        call create_temp_file("auto-revert-test", file_1, st)
        call create_temp_file("auto-revert-test", file_2, st)
        call create_temp_file("auto-revert-test", file_3, st)
        file_2b = file_2//"-b"

        body: block
            call find_file_noselect(file_1, buffer_1, st)
            call check(error,.not. minor_modes%auto_revert_mode%enabled_in(int(buffer_1%index, int64)), "mode 1")
            if (allocated(error)) exit body
            call find_file_noselect(file_2, buffer_2, st)
            call write_text_old("1-a", file_1)
            call check(error, text_of(buffer_1), "")
            if (allocated(error)) exit body
            call minor_modes%global_auto_revert_mode%enable(st)
            call check(error,.not. minor_modes%auto_revert_mode%enabled_in(int(buffer_1%index, int64)), "mode 1 global")
            if (allocated(error)) exit body
            ! buffer_1 should have been reverted immediately when the mode
            ! was enabled.
            call check(error, text_of(buffer_1), "1-a")
            if (allocated(error)) exit body
            ! Alter a file.
            call write_text("2-a", file_2)
            call wait_for_buffer_text(buffer_2, "2-a", auto_revert_interval + 1)
            call check(error, text_of(buffer_2), "2-a")
            if (allocated(error)) exit body
            ! Visit a file, and modify it on disk.
            call find_file_noselect(file_3, buffer_3, st)
            call write_text("3-a", file_3)
            call wait_for_buffer_text(buffer_3, "3-a", auto_revert_interval + 1)
            call check(error, text_of(buffer_3), "3-a")
            if (allocated(error)) exit body
            ! Delete a visited file, and re-create it with new contents.
            call delete_file(file_1, st)
            call check(error, text_of(buffer_1), "1-a")
            if (allocated(error)) exit body
            call write_text("1-b", file_1)
            call wait_for_buffer_text(buffer_1, "1-b", 2*(auto_revert_interval + 1))
            call check(error, text_of(buffer_1), "1-b")
            if (allocated(error)) exit body
            ! Write a buffer to a new file, then modify the new file on
            ! disk.
            call set_buffer(buffer_2, st)
            call write_file(file_2b, .false., st)
            call check(error, text_of(buffer_2), "2-a")
            if (allocated(error)) exit body
            call write_text("2-b", file_2b)
            call wait_for_buffer_text(buffer_2, "2-b", auto_revert_interval + 1)
            call check(error, text_of(buffer_2), "2-b")
        end block body

        call minor_modes%global_auto_revert_mode%disable(st)
        call close_and_delete(buffer_1, file_1)
        call close_and_delete(buffer_2, file_2)
        call close_and_delete(buffer_3, file_3)
        call delete_file(file_2b, st)
        call finish()
    end subroutine test05_global_notify

    !> Verify that notification follows `write_file' correctly.
    subroutine test06_write_file(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: file_1, file_2
        type(handle_type) :: buffer
        type(status_type) :: st

        call begin()
        auto_revert_use_notify%value = .true.
        call create_temp_file("auto-revert-test", file_1, st)
        file_2 = file_1//"-2"

        body: block
            call find_file_noselect(file_1, buffer, st)
            call set_buffer(buffer, st)
            call insert("A", st)
            call save_buffer(1_int64, .false., st)
            call minor_modes%auto_revert_mode%enable(st)
            call insert("B", st)
            call write_file(file_2, .false., st)
            call check(error, st%ok(), format_error(st))
            if (allocated(error)) exit body
            call write_text("C", file_2)
            call wait_for_buffer_text(buffer, "C", auto_revert_interval + 1)
            call check(error, text_of(buffer), "C")
        end block body

        call close_and_delete(buffer, file_1)
        call delete_file(file_2, st)
        call finish()
    end subroutine test06_write_file

    !> The body of `inhibit_auto_revert' in test08.
    recursive subroutine inhibited(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: buffer

        call minor_modes%auto_revert_mode%enable(status)
        buffer = current_buffer()
        call write_text_old("another text", buffers(current)%filename)
        ! The buffer must not be reverted.
        call wait_for_revert(buffer)
    end subroutine inhibited

    !> Check the power of `inhibit_auto_revert'.
    subroutine test08_inhibit(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: tmpfile
        type(handle_type) :: buffer
        type(status_type) :: st

        call begin()
        call create_temp_file("auto-revert-test", tmpfile, st)

        body: block
            call write_text("any text", tmpfile)
            call find_file_noselect(tmpfile, buffer, st)
            call set_buffer(buffer, st)
            call capture_messages()
            call inhibit_auto_revert(inhibited, st)
            call check(error, minor_modes%auto_revert_mode%enabled(), "auto_revert_mode")
            if (allocated(error)) exit body
            ! Check, that the buffer hasn't been reverted.
            call check(error, index(buffer_string(), "another text") == 0, "inhibited: "//buffer_string())
            if (allocated(error)) exit body
            ! Check, that the buffer has been reverted.
            call wait_for_revert(buffer)
            call check(error, index(buffer_string(), "another text") > 0, "reverted: "//buffer_string())
        end block body

        call close_and_delete(buffer, tmpfile)
        call finish()
    end subroutine test08_inhibit
end module test_autorevert_mod
