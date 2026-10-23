! test_undo_fm.f90 -- tests for undo: recording (undo_fm) and the undo
! commands (simple_fm).
! Ported from GNU Emacs 31.1 test/src/undo-tests.el and
! test/lisp/simple-tests.el (the undo tests that need no text properties,
! words or regions), plus our own.
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

module test_undo_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use test_support, only: show_alone
    use error_fm, only: status_type, format_error
    use handle_fm, only: handle_type
    use lisp_fm, only: nil
    use lread_fm, only: sym
    use buffer_fm, only: buffers, cur, get_buffer_create, set_buffer, kill_buffer, erase_buffer, &
                         buffer_modified_p, restore_buffer_modified_p, current_buffer
    use insdel_fm, only: insert
    use editfns_fm, only: point, point_min, point_max, goto_char, buffer_string, delete_region
    use keyboard_fm, only: kbd_store_bytes, command_loop, discard_input, last_command, this_command
    use callint_fm, only: prefix_arg, call_interactively
    use bindings_fm, only: init_bindings
    use undo_fm, only: undo_boundary
    use simple_fm, only: undo, undo_more, undo_only, undo_redo
    use fileio_fm, only: write_region, delete_file_internal
    use files_fm, only: find_file_noselect, temporary_file_directory
    implicit none(type, external)
    private

    public :: collect_undo_fm

contains

    subroutine collect_undo_fm(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)
        testsuite = [ &
                    new_unittest("undo-test0", test_undo0), &
                    new_unittest("undo-test2", test_undo2), &
                    new_unittest("undo-test-buffer-modified", test_buffer_modified), &
                    new_unittest("undo-test-file-modified", test_file_modified), &
                    new_unittest("simple-tests--undo", test_simple_undo), &
                    new_unittest("undo-point-in-wrong-place", test_point_in_wrong_place), &
                    new_unittest("undo-amalgamates-typing", test_amalgamate) &
                    ]
    end subroutine collect_undo_fm

    !> A fresh current buffer with undo enabled and an empty undo list.
    subroutine fresh(h)
        type(handle_type), intent(out) :: h
        type(status_type) :: st
        call get_buffer_create("*undo-test*", h, st)
        call kill_buffer(h, st)
        call get_buffer_create("*undo-test*", h, st)
        call set_buffer(h, st)
        last_command = nil()
        this_command = nil()
        call show_alone()
    end subroutine fresh

    !> Run the commands in `cmds` as the command loop would, each followed
    !> by an undo boundary.  Port of simple-tests.el:simple-tests--exec.
    subroutine exec(cmds)
        character(len=*), intent(in) :: cmds(:)
        type(status_type) :: st
        integer :: k
        do k = 1, size(cmds)
            last_command = this_command
            this_command = sym(trim(cmds(k)))
            prefix_arg = nil()
            call call_interactively(sym(trim(cmds(k))), st)
            call undo_boundary()
        end do
    end subroutine exec

    subroutine test_undo0(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        call fresh(h)
        call undo(nil(), st)
        if (st%ok()) call undo(nil(), st)
        call check(error, format_error(st), "No further undo information")
        if (allocated(error)) return
        call undo_boundary()
        call insert("This", st)
        call undo_boundary()
        call erase_buffer(st)
        call undo_boundary()
        call insert("That", st)
        call undo_boundary()
        call goto_char(1_int64)
        call undo_boundary()
        call insert("With ", st)
        call undo_boundary()
        call goto_char(6_int64)
        call undo_boundary()
        call delete_region(6_int64, 10_int64, st)
        call undo_boundary()
        call check(error, buffer_string(), "With ")
        if (allocated(error)) return
        last_command = nil()
        call undo(nil(), st)
        call check(error, buffer_string(), "With That")
        if (allocated(error)) return
        call undo_more(4_int64, st)
        call check(error, st%ok() .and. buffer_string() == "", buffer_string())
        call kill_buffer(h, st)
    end subroutine test_undo0

    !> Test basic redoing with `undo`.
    subroutine test_undo2(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        character(len=:), allocatable :: before
        call fresh(h)
        call undo_boundary()
        call insert("One", st)
        call undo_boundary()
        call insert(" Zero", st)
        call undo_boundary()
        call delete_region(5_int64, 9_int64, st)
        call undo_boundary()
        call goto_char(1_int64)
        call insert("Zero", st)
        call undo_boundary()
        last_command = nil()
        call undo(nil(), st)
        before = buffer_string()
        call check(error, before, "One ")
        if (allocated(error)) return
        call undo_more(2_int64, st)
        call check(error, buffer_string(), "One")
        if (allocated(error)) return
        ! A new undo series first undoes the undos (they have no boundary
        ! of their own here), then the change before them.
        last_command = nil()
        call undo(nil(), st)
        call check(error, buffer_string(), before)
        call kill_buffer(h, st)
    end subroutine test_undo2

    !> Test undoing marks buffer unmodified.
    subroutine test_buffer_modified(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        logical :: modified
        call fresh(h)
        call insert("1", st)
        call undo_boundary()
        call restore_buffer_modified_p(.false.)
        call insert("2", st)
        last_command = nil()
        call undo(nil(), st)
        call buffer_modified_p(h, modified, st)
        call check(error,.not. modified, "unmodified")
        call kill_buffer(h, st)
    end subroutine test_buffer_modified

    !> Test undoing marks buffer visiting file unmodified.
    subroutine test_file_modified(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, buf
        type(status_type) :: st
        character(len=:), allocatable :: file
        logical :: modified
        file = temporary_file_directory()//"fortmacs-undo-test"
        call get_buffer_create(" *undo-file*", h, st)
        call set_buffer(h, st)
        call write_region(file, .false., st)
        call find_file_noselect(file, buf, st)
        call set_buffer(buf, st)
        call insert("1", st)
        call undo_boundary()
        call restore_buffer_modified_p(.false.)
        call insert("2", st)
        last_command = nil()
        call undo(nil(), st)
        call buffer_modified_p(buf, modified, st)
        call check(error,.not. modified, "unmodified")
        call kill_buffer(buf, st)
        call kill_buffer(h, st)
        call delete_file_internal(file, st)
    end subroutine test_file_modified

    subroutine test_simple_undo(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        character(len=1), parameter :: letters(5) = ["a", "b", "c", "d", "e"]
        integer :: k
        call fresh(h)
        call init_bindings()
        do k = 1, size(letters)
            call insert(letters(k), st)
            call undo_boundary()
        end do
        call check(error, buffer_string(), "abcde")
        if (allocated(error)) return
        call exec([character(len=16) :: "undo", "undo"])
        call check(error, buffer_string(), "abc")
        if (allocated(error)) return
        call exec([character(len=16) :: "backward_char", "undo"])
        call check(error, buffer_string(), "abcd")
        if (allocated(error)) return
        call exec([character(len=16) :: "undo"])
        call check(error, buffer_string(), "abcde")
        if (allocated(error)) return
        call exec([character(len=16) :: "backward_char", "undo", "undo"])
        call check(error, buffer_string(), "abc")
        if (allocated(error)) return
        call exec([character(len=16) :: "backward_char", "undo_redo"])
        call check(error, buffer_string(), "abcd")
        if (allocated(error)) return
        call exec([character(len=16) :: "undo"])
        call check(error, buffer_string(), "abc")
        if (allocated(error)) return
        call exec([character(len=16) :: "backward_char", "undo_redo", "undo_redo"])
        call check(error, buffer_string(), "abcde")
        if (allocated(error)) return
        call exec([character(len=16) :: "undo", "undo"])
        call check(error, buffer_string(), "abc")
        if (allocated(error)) return
        call exec([character(len=16) :: "backward_char", "undo_only", "undo_only"])
        call check(error, buffer_string(), "a")
        if (allocated(error)) return
        call exec([character(len=16) :: "backward_char", "undo_redo", "undo_redo"])
        call check(error, buffer_string(), "abc")
        if (allocated(error)) return
        call exec([character(len=16) :: "backward_char", "undo_redo", "undo_redo"])
        call check(error, buffer_string(), "abcde")
        call kill_buffer(h, st)
    end subroutine test_simple_undo

    !> Typing through the command loop, then undo: point comes back to where
    !> the undone change was.  Port of the first case of
    !> simple-tests.el:undo-point-in-wrong-place (C-b for <left>).
    subroutine test_point_in_wrong_place(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        call fresh(h)
        call discard_input()
        prefix_arg = nil()
        call init_bindings()
        call insert("a"//achar(10)//"b"//achar(10)//"c"//achar(10), st)
        call goto_char(point_max())
        ! left, DEL (deletes "c"), left x3, DEL (deletes "a"), C-_
        call kbd_store_bytes(achar(2)//achar(127)//achar(2)//achar(2)//achar(2)//achar(127)//achar(31))
        call command_loop(st)
        call check(error, point(), 2_int64)
        if (.not. allocated(error)) call check(error, buffer_string(), "a"//achar(10)//"b"//achar(10)//achar(10))
        call kill_buffer(h, st)
    end subroutine test_point_in_wrong_place

    !> Consecutive self-inserts are undone together, at most
    !> amalgamating-undo-limit plus one at a time.
    subroutine test_amalgamate(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        call fresh(h)
        call discard_input()
        prefix_arg = nil()
        call init_bindings()
        call kbd_store_bytes("abc"//achar(1)//"xyz"//achar(31))
        call command_loop(st)
        call check(error, buffer_string(), "abc")
        if (allocated(error)) return
        call kbd_store_bytes(achar(31))
        call command_loop(st)
        call check(error, buffer_string(), "")
        if (allocated(error)) return
        ! A different command breaks the chain: C-f, then undo twice redoes.
        ! (C-f fails at the end of the buffer, which discards the input
        ! typed ahead.)
        call kbd_store_bytes(achar(6))
        call command_loop(st)
        call kbd_store_bytes(achar(31)//achar(31))
        call command_loop(st)
        call check(error, buffer_string(), "xyzabc")
        if (allocated(error)) return
        call erase_buffer(st)
        call kbd_store_bytes("abcdefghijklmnopqrstuvwxy"//achar(31))
        call command_loop(st)
        call check(error, buffer_string(), "abcdefghijklmnopqrstu")
        call kill_buffer(h, st)
    end subroutine test_amalgamate
end module test_undo_fm
