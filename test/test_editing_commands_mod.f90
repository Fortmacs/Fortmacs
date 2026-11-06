! test_editing_commands_mod.f90 -- tests for search_mod (newlines), line positions in
! editing_functions_mod, and editing_commands_mod.
! Ports GNU Emacs 31.1 test/src/cmds-tests.el.
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

module test_editing_commands_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_mod, only: status_type, format_error, BEGINNING_OF_BUFFER_CODE, END_OF_BUFFER_CODE
    use handle_mod, only: handle_type
    use buffer_mod, only: get_or_make_buffer, close_buffer, set_buffer, &
                          current_buffer, is_buffer_live
    use insert_delete_mod, only: insert, move_gap_both
    use search_mod, only: find_newline, find_before_next_newline
    use editing_functions_mod, only: point, goto_character, buffer_string, position_beginning_of_line, position_end_of_line, &
                                     narrow_to_region
    use editing_commands_mod, only: backward_character, beginning_of_line, delete_character, end_of_line, forward_character, &
                                    forward_line, self_insert_command
    implicit none(type, external)
    private

    public :: collect_editing_commands_mod

    character(len=*), parameter :: newline = achar(10)
    character(len=*), parameter :: euro = char(226)//char(130)//char(172)
    !> most-positive-fixnum on a 64-bit host.
    integer(int64), parameter :: most_positive_fixnum = 2_int64**61 - 1
    integer(int64), parameter :: most_negative_fixnum = -2_int64**61

contains

    !> Collect all exported unit tests.
    subroutine collect_editing_commands_mod(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("find-newline-forward", test_find_forward), &
                    new_unittest("find-newline-backward", test_find_backward), &
                    new_unittest("find-before-next-newline", test_before_next), &
                    new_unittest("pos-bol-eol", test_beginning_of_line_end_of_line), &
                    new_unittest("forward-backward-char", test_characters), &
                    new_unittest("forward-line", test_forward_line), &
                    new_unittest("self-insert-command-with-negative-argument", test_self_insert_negative), &
                    new_unittest("forward-line-with-bignum", test_bignum), &
                    new_unittest("beginning-end-of-line", test_beginning_of_line_commands), &
                    new_unittest("delete-char", test_delete_character) &
                    ]
    end subroutine collect_editing_commands_mod

    subroutine enter(name, h, old)
        character(len=*), intent(in) :: name
        type(handle_type), intent(out) :: h, old
        type(status_type) :: st

        old = current_buffer()
        call get_or_make_buffer(name, h, st)
        call set_buffer(h, st)
    end subroutine enter

    subroutine leave(h, old)
        type(handle_type), intent(in) :: h, old
        type(status_type) :: st

        if (is_buffer_live(old)) call set_buffer(old, st)
        call close_buffer(h, st)
    end subroutine leave

    !> Three lines with a multibyte char, gap placed mid-text.
    subroutine fill_lines()
        type(status_type) :: st

        ! chars:  a  €  \n  b  b  \n  c
        ! pos:    1  2  3   4  5  6   7   (Z = 8)
        call insert("a"//euro//newline//"bb"//newline//"c", st)
        call move_gap_both(5_int64, 7_int64)
    end subroutine fill_lines

    subroutine test_find_forward(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, old
        integer(int64) :: position, counted, byte_position

        call enter("tc-ff", h, old)
        call fill_lines()
        position = find_newline(1_int64, count=1_int64, counted=counted, byte_position=byte_position)
        call check(error, position == 4 .and. counted == 1 .and. byte_position == 6, "first newline")
        if (allocated(error)) return
        position = find_newline(1_int64, count=2_int64, counted=counted)
        call check(error, position == 7 .and. counted == 2, "second newline (across the gap)")
        if (allocated(error)) return
        position = find_newline(1_int64, count=5_int64, counted=counted)
        call check(error, position == 8 .and. counted == 2, "too few: stop at ZV")
        call leave(h, old)
    end subroutine test_find_forward

    subroutine test_find_backward(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, old
        integer(int64) :: position, counted

        call enter("tc-fb", h, old)
        call fill_lines()
        ! Backward results are positions *after* the newline found.
        position = find_newline(8_int64, count=-1_int64, counted=counted)
        call check(error, position == 7 .and. counted == -1, "last newline")
        if (allocated(error)) return
        position = find_newline(8_int64, count=-2_int64, counted=counted)
        call check(error, position == 4 .and. counted == -2, "first newline")
        if (allocated(error)) return
        position = find_newline(8_int64, count=-3_int64, counted=counted)
        call check(error, position == 1 .and. counted == -2, "too few: stop at BEGV")
        call leave(h, old)
    end subroutine test_find_backward

    subroutine test_before_next(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, old

        call enter("tc-bnn", h, old)
        call fill_lines()
        call check(error, find_before_next_newline(1_int64, 1_int64), 3_int64)
        if (allocated(error)) return
        call check(error, find_before_next_newline(7_int64, 1_int64), 8_int64)
        call leave(h, old)
    end subroutine test_before_next

    subroutine test_beginning_of_line_end_of_line(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, old
        type(status_type) :: st

        call enter("tc-bol", h, old)
        call fill_lines()
        call goto_character(5_int64)
        call check(error, position_beginning_of_line() == 4 .and. position_end_of_line() == 6, "line 2")
        if (allocated(error)) return
        call check(error, position_beginning_of_line(2_int64) == 7 .and. position_end_of_line(2_int64) == 8, "next line")
        if (allocated(error)) return
        call check(error, position_beginning_of_line(0_int64) == 1 .and. position_end_of_line(0_int64) == 3, "previous line")
        if (allocated(error)) return
        call check(error, point(), 5_int64)   ! point untouched
        if (allocated(error)) return
        ! Narrowing bounds the scan.
        call narrow_to_region(5_int64, 8_int64, st)
        call goto_character(5_int64)
        call check(error, position_beginning_of_line(), 5_int64)
        call leave(h, old)
    end subroutine test_beginning_of_line_end_of_line

    subroutine test_characters(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, old
        type(status_type) :: st

        call enter("tc-chars", h, old)
        call insert("abc", st)
        call backward_character(st, 2_int64)
        call check(error, st%ok() .and. point() == 2, "backward 2")
        if (allocated(error)) return
        call forward_character(st)
        call check(error, point(), 3_int64)
        if (allocated(error)) return
        call forward_character(st, 5_int64)
        call check(error, st%code == END_OF_BUFFER_CODE .and. point() == 4, "end_of_buffer")
        if (allocated(error)) return
        call backward_character(st, 10_int64)
        call check(error, st%code == BEGINNING_OF_BUFFER_CODE .and. point() == 1, "beginning_of_buffer")
        call leave(h, old)
    end subroutine test_characters

    subroutine test_forward_line(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, old

        call enter("tc-fl", h, old)
        call fill_lines()
        call goto_character(2_int64)
        call check(error, forward_line(), 0_int64)
        if (allocated(error)) return
        call check(error, point(), 4_int64)
        if (allocated(error)) return
        ! Last line is non-empty: moving to its end counts as a line.
        call check(error, forward_line(5_int64), 3_int64)
        if (allocated(error)) return
        call check(error, point(), 8_int64)
        if (allocated(error)) return
        call check(error, forward_line(0_int64), 0_int64)   ! to bol
        if (allocated(error)) return
        call check(error, point(), 7_int64)
        if (allocated(error)) return
        call check(error, forward_line(-5_int64), -3_int64)
        if (allocated(error)) return
        call check(error, point(), 1_int64)
        call leave(h, old)
    end subroutine test_forward_line

    !> cmds-tests.el: self-insert-command-with-negative-argument.
    subroutine test_self_insert_negative(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, old
        type(status_type) :: st

        call enter("tc-selfneg", h, old)
        call self_insert_command(-1_int64, int(iachar("a"), int64), st)
        call check(error, format_error(st), "Negative repetition argument -1")
        call leave(h, old)
    end subroutine test_self_insert_negative

    !> cmds-tests.el: forward-line-with-bignum.
    subroutine test_bignum(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, old
        type(status_type) :: st

        call enter("tc-bignum", h, old)
        call insert("x"//newline, st)
        call check(error, forward_line(most_negative_fixnum - 1), most_negative_fixnum)
        if (allocated(error)) return
        call check(error, forward_line(most_positive_fixnum + 2), most_positive_fixnum + 1)
        call leave(h, old)
    end subroutine test_bignum

    subroutine test_beginning_of_line_commands(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, old

        call enter("tc-bolcmd", h, old)
        call fill_lines()
        call goto_character(5_int64)
        call end_of_line()
        call check(error, point(), 6_int64)
        if (allocated(error)) return
        call beginning_of_line()
        call check(error, point(), 4_int64)
        if (allocated(error)) return
        call end_of_line(2_int64)
        call check(error, point(), 8_int64)
        call leave(h, old)
    end subroutine test_beginning_of_line_commands

    subroutine test_delete_character(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, old
        type(status_type) :: st

        call enter("tc-del", h, old)
        call insert("abcdef", st)
        call goto_character(3_int64)
        call delete_character(2_int64, st)
        call check(error, buffer_string(), "abef")
        if (allocated(error)) return
        call delete_character(-1_int64, st)
        call check(error, buffer_string(), "aef")
        if (allocated(error)) return
        call delete_character(10_int64, st)
        call check(error, st%code == END_OF_BUFFER_CODE .and. buffer_string() == "aef", "end_of_buffer")
        if (allocated(error)) return
        call delete_character(-10_int64, st)
        call check(error, st%code, BEGINNING_OF_BUFFER_CODE)
        call leave(h, old)
    end subroutine test_delete_character
end module test_editing_commands_mod
