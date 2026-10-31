! test_autorevert_fm.f90 -- tests for autorevert_fm.
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
module test_autorevert_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_fm, only: status_type, format_error
    use handle_fm, only: handle_type
    use lisp_fm, only: value_type
    use buffer_fm, only: buffers, cur, get_buffer, get_buffer_create, set_buffer, kill_buffer, current_buffer, &
                         generate_new_buffer_name, is_buffer_live, restore_buffer_modified, erase_buffer
    use insdel_fm, only: insert
    use editfns_fm, only: buffer_string
    use fileio_fm, only: write_region, set_file_times
    use files_fm, only: make_temp_file, delete_file, find_file_noselect, save_buffer, write_file, before_revert_hook
    use sysdep_fm, only: emacs_time
    use keyboard_fm, only: read_event
    use modes_fm, only: minor_modes
    use bindings_fm, only: init_bindings
    use filenotify_fm, only: file_notify_rm_all_watches
    use autorevert_fm, only: auto_revert_interval, auto_revert_lockout_interval, auto_revert_stop_on_user_input, &
                             auto_revert_notify_exclude_dir_regexp, inhibit_auto_revert, auto_revert_use_notify
    implicit none(type, external)
    private

    public :: collect_autorevert_fm

    !> The length of *Messages* when the capture started.
    integer :: messages_start = 0
    !> The buffer whose text `buffer_text_is' waits for, and the text.
    type(handle_type) :: wait_buffer
    character(len=:), allocatable :: wait_text, wait_message
    !> Settings saved by `begin' (with-auto-revert-test).
    real :: interval_orig, lockout_orig

    abstract interface
        logical function pred_proc()
            implicit none(type, external)
        end function pred_proc
    end interface

contains

    !> Collect all exported unit tests.
    subroutine collect_autorevert_fm(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("auto-revert-test00-auto-revert-mode", test00_auto_revert_mode), &
                    new_unittest("auto-revert-test02-auto-revert-deleted-file", test02_deleted_file), &
                    new_unittest("auto-revert-test03-auto-revert-tail-mode", test03_tail_mode), &
                    new_unittest("auto-revert-test05-global-notify", test05_global_notify), &
                    new_unittest("auto-revert-test06-write-file", test06_write_file), &
                    new_unittest("auto-revert-test08-auto-revert-inhibit-auto-revert", test08_inhibit) &
                    ]
    end subroutine collect_autorevert_fm

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

    !> Kill BUF, unmodified, and delete FILE.
    subroutine kill_and_delete(buf, file)
        type(handle_type), intent(in) :: buf
        character(len=*), intent(in) :: file
        type(status_type) :: st

        if (is_buffer_live(buf)) then
            call set_buffer(buf, st)
            call restore_buffer_modified(.false.)
            call kill_buffer(buf, st)
        end if

        call delete_file(file, st)
    end subroutine kill_and_delete

    !> Write STRING to FILE (auto-revert-test--write-region).
    subroutine write_text(text, file)
        character(len=*), intent(in) :: text, file
        type(handle_type) :: old, h
        type(status_type) :: st
        character(len=:), allocatable :: msg

        old = current_buffer()
        call get_buffer_create(generate_new_buffer_name(" *autorevert-test*"), h, st)
        call set_buffer(h, st)
        call insert(text, st)
        call write_region(file, .false., st, message=msg)
        call kill_buffer(h, st)
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

    !> The text of BUF.
    function text_of(buf) result(text)
        type(handle_type), intent(in) :: buf
        character(len=:), allocatable :: text
        type(handle_type) :: old
        type(status_type) :: st

        old = current_buffer()
        call set_buffer(buf, st)
        text = buffer_string()
        call set_buffer(old, st)
    end function text_of

    logical function buffer_text_is()
        buffer_text_is = text_of(wait_buffer) == wait_text .and. len(text_of(wait_buffer)) == len(wait_text)
    end function buffer_text_is

    !> Wait until PRED is true, or MAX_WAIT seconds elapsed
    !> (auto-revert-test--wait-for).
    subroutine wait_for(pred, max_wait)
        procedure(pred_proc) :: pred
        real, intent(in) :: max_wait
        type(value_type) :: ev
        type(status_type) :: st
        logical :: have
        integer :: k

        do k = 1, int(max_wait*10)
            if (pred()) return
            call read_event(ev, have, st, seconds=0.1)
        end do
    end subroutine wait_for

    !> Wait until a message reports reversion of BUF
    !> (auto-revert--wait-for-revert).
    subroutine wait_for_revert(buf)
        type(handle_type), intent(in) :: buf

        wait_message = "Reverting buffer `"//buffers(buf%index)%name//"'"
        call wait_for(message_seen, auto_revert_interval + 1)
    end subroutine wait_for_revert

    !> Wait until BUF has the contents TEXT
    !> (auto-revert-test--wait-for-buffer-text).
    subroutine wait_for_buffer_text(buf, text, max_wait)
        type(handle_type), intent(in) :: buf
        character(len=*), intent(in) :: text
        real, intent(in) :: max_wait

        wait_buffer = buf
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
        type(handle_type) :: buf
        type(status_type) :: st

        call begin()
        call make_temp_file("auto-revert-test", tmpfile, st)

        body: block
            call write_text("any text", tmpfile)
            call find_file_noselect(tmpfile, buf, st)
            call set_buffer(buf, st)
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
            call wait_for_revert(buf)
            call check(error, index(buffer_string(), "another text") > 0, "reverted: "//buffer_string())
            if (allocated(error)) exit body
            ! When the buffer is modified, it shall not be reverted.
            call capture_messages()
            call restore_buffer_modified(.true.)
            call write_text_old("any text", tmpfile)
            ! Check, that the buffer hasn't been reverted.
            call wait_for_revert(buf)
            call check(error, index(buffer_string(), "any text") == 0, "not reverted: "//buffer_string())
        end block body

        call kill_and_delete(buf, tmpfile)
        call finish()
    end subroutine test00_auto_revert_mode

    logical function always_false()
        always_false = .false.
    end function always_false

    !> before-revert-hook: delete the visited file.
    recursive subroutine delete_visited_file(status)
        type(status_type), intent(out) :: status

        call delete_file(buffers(cur)%filename, status)
    end subroutine delete_visited_file

    !> Check autorevert for a deleted file.
    subroutine test02_deleted_file(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: tmpfile
        type(handle_type) :: buf
        type(status_type) :: st

        call begin()
        call make_temp_file("auto-revert-test", tmpfile, st)

        body: block
            call write_text("any text", tmpfile)
            call find_file_noselect(tmpfile, buf, st)
            call set_buffer(buf, st)
            call check(error, buffer_string(), "any text")
            if (allocated(error)) exit body
            call minor_modes%auto_revert_mode%enable(st)
            call check(error, minor_modes%auto_revert_mode%enabled(), "auto_revert_mode")
            if (allocated(error)) exit body
            ! Remove file while reverting.  We simulate this by
            ! modifying `before_revert_hook'.
            call before_revert_hook%add(delete_visited_file, local=.true.)
            call capture_messages()
            call write_text_old("another text", tmpfile)
            call wait_for_revert(buf)
            ! Check, that the buffer hasn't been reverted.  File
            ! notification should be disabled, falling back to polling.
            call check(error, index(buffer_string(), "any text") > 0, "not reverted: "//buffer_string())
            if (allocated(error)) exit body
            ! Once the file has been recreated, the buffer shall be
            ! reverted.
            call before_revert_hook%kill_local()
            call capture_messages()
            call write_text_old("another text", tmpfile)
            call wait_for_revert(buf)
            call check(error, index(buffer_string(), "another text") > 0, "reverted: "//buffer_string())
            if (allocated(error)) exit body
            ! An empty file shall still be reverted.
            call capture_messages()
            call write_text_old("", tmpfile)
            call wait_for_revert(buf)
            call check(error, buffer_string(), "")
        end block body

        call kill_and_delete(buf, tmpfile)
        call finish()
    end subroutine test02_deleted_file

    !> Check autorevert tail mode.
    subroutine test03_tail_mode(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: tmpfile
        type(handle_type) :: buf
        type(status_type) :: st

        call begin()
        call make_temp_file("auto-revert-test", tmpfile, st)

        body: block
            call capture_messages()
            call write_text("any text", tmpfile)
            call find_file_noselect(tmpfile, buf, st)
            call set_buffer(buf, st)
            call minor_modes%auto_revert_tail_mode%enable(st)
            call check(error, minor_modes%auto_revert_tail_mode%enabled(), "auto_revert_tail_mode")
            if (allocated(error)) exit body
            call erase_buffer(st)
            call insert("modified text"//achar(10), st)
            call restore_buffer_modified(.false.)
            ! Modify file (appending "another text").
            call write_text_old("any textanother text", tmpfile)
            ! Check, that the buffer has been reverted.
            call wait_for_revert(buf)
            call check(error, index(buffer_string(), "modified text"//achar(10)//"another text") > 0, &
                       "tail: "//buffer_string())
        end block body

        call kill_and_delete(buf, tmpfile)
        call finish()
    end subroutine test03_tail_mode

    !> Test `global_auto_revert_mode'.
    subroutine test05_global_notify(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: file_1, file_2, file_3, file_2b
        type(handle_type) :: buf_1, buf_2, buf_3
        type(status_type) :: st

        call begin()
        auto_revert_use_notify%value = .true.
        call make_temp_file("auto-revert-test", file_1, st)
        call make_temp_file("auto-revert-test", file_2, st)
        call make_temp_file("auto-revert-test", file_3, st)
        file_2b = file_2//"-b"

        body: block
            call find_file_noselect(file_1, buf_1, st)
            call check(error,.not. minor_modes%auto_revert_mode%enabled_in(int(buf_1%index, int64)), "mode 1")
            if (allocated(error)) exit body
            call find_file_noselect(file_2, buf_2, st)
            call write_text_old("1-a", file_1)
            call check(error, text_of(buf_1), "")
            if (allocated(error)) exit body
            call minor_modes%global_auto_revert_mode%enable(st)
            call check(error,.not. minor_modes%auto_revert_mode%enabled_in(int(buf_1%index, int64)), "mode 1 global")
            if (allocated(error)) exit body
            ! buf_1 should have been reverted immediately when the mode
            ! was enabled.
            call check(error, text_of(buf_1), "1-a")
            if (allocated(error)) exit body
            ! Alter a file.
            call write_text("2-a", file_2)
            call wait_for_buffer_text(buf_2, "2-a", auto_revert_interval + 1)
            call check(error, text_of(buf_2), "2-a")
            if (allocated(error)) exit body
            ! Visit a file, and modify it on disk.
            call find_file_noselect(file_3, buf_3, st)
            call write_text("3-a", file_3)
            call wait_for_buffer_text(buf_3, "3-a", auto_revert_interval + 1)
            call check(error, text_of(buf_3), "3-a")
            if (allocated(error)) exit body
            ! Delete a visited file, and re-create it with new contents.
            call delete_file(file_1, st)
            call check(error, text_of(buf_1), "1-a")
            if (allocated(error)) exit body
            call write_text("1-b", file_1)
            call wait_for_buffer_text(buf_1, "1-b", 2*(auto_revert_interval + 1))
            call check(error, text_of(buf_1), "1-b")
            if (allocated(error)) exit body
            ! Write a buffer to a new file, then modify the new file on
            ! disk.
            call set_buffer(buf_2, st)
            call write_file(file_2b, .false., st)
            call check(error, text_of(buf_2), "2-a")
            if (allocated(error)) exit body
            call write_text("2-b", file_2b)
            call wait_for_buffer_text(buf_2, "2-b", auto_revert_interval + 1)
            call check(error, text_of(buf_2), "2-b")
        end block body

        call minor_modes%global_auto_revert_mode%disable(st)
        call kill_and_delete(buf_1, file_1)
        call kill_and_delete(buf_2, file_2)
        call kill_and_delete(buf_3, file_3)
        call delete_file(file_2b, st)
        call finish()
    end subroutine test05_global_notify

    !> Verify that notification follows `write_file' correctly.
    subroutine test06_write_file(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: file_1, file_2
        type(handle_type) :: buf
        type(status_type) :: st

        call begin()
        auto_revert_use_notify%value = .true.
        call make_temp_file("auto-revert-test", file_1, st)
        file_2 = file_1//"-2"

        body: block
            call find_file_noselect(file_1, buf, st)
            call set_buffer(buf, st)
            call insert("A", st)
            call save_buffer(1_int64, .false., st)
            call minor_modes%auto_revert_mode%enable(st)
            call insert("B", st)
            call write_file(file_2, .false., st)
            call check(error, st%ok(), format_error(st))
            if (allocated(error)) exit body
            call write_text("C", file_2)
            call wait_for_buffer_text(buf, "C", auto_revert_interval + 1)
            call check(error, text_of(buf), "C")
        end block body

        call kill_and_delete(buf, file_1)
        call delete_file(file_2, st)
        call finish()
    end subroutine test06_write_file

    !> The body of `inhibit_auto_revert' in test08.
    recursive subroutine inhibited(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: buf

        call minor_modes%auto_revert_mode%enable(status)
        buf = current_buffer()
        call write_text_old("another text", buffers(cur)%filename)
        ! The buffer must not be reverted.
        call wait_for_revert(buf)
    end subroutine inhibited

    !> Check the power of `inhibit_auto_revert'.
    subroutine test08_inhibit(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: tmpfile
        type(handle_type) :: buf
        type(status_type) :: st

        call begin()
        call make_temp_file("auto-revert-test", tmpfile, st)

        body: block
            call write_text("any text", tmpfile)
            call find_file_noselect(tmpfile, buf, st)
            call set_buffer(buf, st)
            call capture_messages()
            call inhibit_auto_revert(inhibited, st)
            call check(error, minor_modes%auto_revert_mode%enabled(), "auto_revert_mode")
            if (allocated(error)) exit body
            ! Check, that the buffer hasn't been reverted.
            call check(error, index(buffer_string(), "another text") == 0, "inhibited: "//buffer_string())
            if (allocated(error)) exit body
            ! Check, that the buffer has been reverted.
            call wait_for_revert(buf)
            call check(error, index(buffer_string(), "another text") > 0, "reverted: "//buffer_string())
        end block body

        call kill_and_delete(buf, tmpfile)
        call finish()
    end subroutine test08_inhibit
end module test_autorevert_fm
