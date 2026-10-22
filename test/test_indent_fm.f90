! test_indent_fm.f90 -- tests for indent_fm and vertical motion in
! simple_fm (next-line, previous-line).
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

!> Own tests: indent-tests.el needs invisible text, and line-move has no
!> ERT tests that run without a display.
module test_indent_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_fm, only: status_type, FM_END_OF_BUFFER, FM_BEGINNING_OF_BUFFER
    use handle_fm, only: handle_type
    use lisp_fm, only: value_type, make_fixnum
    use buffer_fm, only: get_buffer_create, set_buffer, kill_buffer, current_buffer
    use insdel_fm, only: insert
    use editfns_fm, only: goto_char, point
    use frame_fm, only: selected_frame
    use indent_fm, only: current_column, move_to_column, vertical_motion, screen_column
    use simple_fm, only: line_move, goal_column, line_move_visual
    use keyboard_fm, only: unread_command_events, discard_input, command_loop, last_command
    use lisp_fm, only: nil
    use bindings_fm, only: init_bindings
    implicit none(type, external)
    private

    public :: collect_indent_fm

    character(len=*), parameter :: nl = achar(10)

contains

    subroutine collect_indent_fm(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("current-column", test_current_column), &
                    new_unittest("move-to-column", test_move_to_column), &
                    new_unittest("vertical-motion-lines", test_vertical_motion), &
                    new_unittest("vertical-motion-wrapped", test_wrapped), &
                    new_unittest("next-line-goal-column", test_goal_column), &
                    new_unittest("line-move-errors", test_errors), &
                    new_unittest("logical-line-move", test_logical) &
                    ]
    end subroutine collect_indent_fm


    !> A fresh buffer holding `text`, point at 1, in a window `cols` wide.
    subroutine enter(text, cols, h)
        character(len=*), intent(in) :: text
        integer, intent(in) :: cols
        type(handle_type), intent(out) :: h
        type(status_type) :: st

        call get_buffer_create("indent-test", h, st)
        call set_buffer(h, st)
        call insert(text, st)
        call goto_char(1_int64)
        ! A fresh goal column: the last command wasn't a line move.
        last_command = nil()
        selected_frame%root_window%total_cols = cols
        selected_frame%minibuffer_selected = .false.
    end subroutine enter


    subroutine leave(h)
        type(handle_type), intent(in) :: h
        type(status_type) :: st

        call kill_buffer(h, st)
        selected_frame%root_window%total_cols = 0
    end subroutine leave


    subroutine test_current_column(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h

        ! "a<TAB>b", C-a (^A), a raw byte, then a wide character.
        call enter("a"//achar(9)//"b"//achar(1)//"c"//achar(195)//achar(169)//"x"// &
                   achar(228)//achar(184)//achar(173)//"y", 80, h)
        call check(error, current_column() == 0)
        if (allocated(error)) return
        call goto_char(3_int64)
        call check(error, current_column() == 8, "tab to column 8")
        if (allocated(error)) return
        call goto_char(5_int64)
        call check(error, current_column() == 11, "^A is two columns")
        if (allocated(error)) return
        call goto_char(8_int64)
        call check(error, current_column() == 14, "é and x")
        if (allocated(error)) return
        call goto_char(9_int64)
        call check(error, current_column() == 16, "a wide character is two columns")
        call leave(h)
    end subroutine test_current_column


    subroutine test_move_to_column(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        integer(int64) :: reached

        call enter("ab"//achar(9)//"cd"//nl//"next", 80, h)
        call move_to_column(1_int64, reached)
        call check(error, point() == 2 .and. reached == 1)
        if (allocated(error)) return
        ! Inside the tab: point goes after it.
        call move_to_column(5_int64, reached)
        call check(error, point() == 4 .and. reached == 8, "after the tab")
        if (allocated(error)) return
        ! Past the end of the line: the end of the line.
        call move_to_column(50_int64, reached)
        call check(error, point() == 6 .and. reached == 10, "end of line")
        call leave(h)
    end subroutine test_move_to_column


    subroutine test_vertical_motion(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        integer(int64) :: moved

        call enter("one"//nl//"two"//nl//"three", 80, h)
        call vertical_motion(1_int64, moved)
        call check(error, moved == 1 .and. point() == 5)
        if (allocated(error)) return
        call vertical_motion(5_int64, moved, 3)
        call check(error, moved == 1 .and. point() == 12, "stops on the last line, at column 3")
        if (allocated(error)) return
        call vertical_motion(-9_int64, moved)
        call check(error, moved == -2 .and. point() == 1)
        call leave(h)
    end subroutine test_vertical_motion


    !> Lines wider than the window continue on the next screen line.
    subroutine test_wrapped(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        integer(int64) :: moved

        ! An 11-column window shows 10 characters per row.
        call enter("abcdefghijklmnopqrstuvwxy"//nl//"z", 11, h)
        call goto_char(4_int64)
        call vertical_motion(1_int64, moved, 3)
        call check(error, moved == 1 .and. point() == 14, "row 2, column 3")
        if (allocated(error)) return
        call check(error, screen_column() == 3)
        if (allocated(error)) return
        call vertical_motion(1_int64, moved, 7)
        call check(error, point() == 26, "the last row is short: its end")
        if (allocated(error)) return
        call vertical_motion(1_int64, moved, 7)
        call check(error, moved == 1 .and. point() == 28, "next line")
        if (allocated(error)) return
        ! Back up into the continued row's end: the last character on it.
        call goto_char(1_int64)
        call vertical_motion(1_int64, moved, 20)
        call check(error, point() == 20, "end of a continued row")
        call leave(h)
    end subroutine test_wrapped


    !> C-n keeps the column across a short line (temporary-goal-column).
    subroutine test_goal_column(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        type(value_type) :: keys(2)

        call init_bindings()
        call discard_input()
        call enter("first line"//nl//"ab"//nl//"third line", 80, h)
        call goto_char(7_int64)
        keys(1) = make_fixnum(14_int64)
        keys(2) = make_fixnum(14_int64)
        call unread_command_events(keys)
        call command_loop(st)
        call check(error, point() == 21, "column 6 on the third line")
        if (allocated(error)) return
        ! C-p twice: back to column 6.
        keys(1) = make_fixnum(16_int64)
        keys(2) = make_fixnum(16_int64)
        call unread_command_events(keys)
        call command_loop(st)
        call check(error, point() == 7)
        call leave(h)
    end subroutine test_goal_column


    subroutine test_errors(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call enter("one"//nl//"two", 80, h)
        call line_move(-1_int64, st)
        call check(error, st%code == FM_BEGINNING_OF_BUFFER)
        if (allocated(error)) return
        call line_move(5_int64, st)
        call check(error, st%code == FM_END_OF_BUFFER)
        if (allocated(error)) return
        call line_move(5_int64, st, noerror=.true.)
        call check(error, st%ok())
        call leave(h)
    end subroutine test_errors


    !> With line-move-visual off (or a goal column), lines are buffer
    !> lines even when wrapped.
    subroutine test_logical(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call enter("abcdefghijklmnopqrstuvwxy"//nl//"z1234"//nl//"q", 11, h)
        call goto_char(3_int64)
        line_move_visual = .false.

        body: block
            call line_move(1_int64, st)
            call check(error, st%ok() .and. point() == 29, "column 2 of the next buffer line")
            if (allocated(error)) exit body
            goal_column = 4
            call line_move(-1_int64, st)
            call check(error, point() == 5, "goal-column 4")
        end block body

        line_move_visual = .true.
        goal_column = -1
        call leave(h)
    end subroutine test_logical

end module test_indent_fm
