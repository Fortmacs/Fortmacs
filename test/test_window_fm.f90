! test_window_fm.f90 -- tests for scrolling and recentering (window_fm).
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

!> Own tests: upstream has no ERT tests for scrolling (it needs a
!> display).  The buffer holds 30 lines "line01" to "line30"; line k
!> starts at 7k - 6.  The window has 10 lines of text.
module test_window_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_fm, only: status_type, FM_END_OF_BUFFER, FM_BEGINNING_OF_BUFFER
    use handle_fm, only: handle_type
    use lisp_fm, only: value_type, nil, make_fixnum, list
    use lread_fm, only: sym
    use buffer_fm, only: get_buffer_create, set_buffer, kill_buffer
    use insdel_fm, only: insert
    use editfns_fm, only: goto_char, point
    use frame_fm, only: selected_frame
    use window_fm, only: set_window_buffer, set_window_start, window_start, free_window, &
                         scroll_up, scroll_down, recenter, move_to_window_line, &
                         scroll_up_command, scroll_down_command, recenter_top_bottom, &
                         move_to_window_line_top_bottom, scroll_error_top_bottom
    use simple_fm, only: goal_column
    use keyboard_fm, only: this_command, last_command, kbd_store_bytes, command_loop, &
                           discard_input
    use callint_fm, only: prefix_arg
    use bindings_fm, only: init_bindings
    implicit none(type, external)
    private

    public :: collect_window_fm

contains

    subroutine collect_window_fm(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)
        testsuite = [ &
                    new_unittest("scroll-up-and-down", test_scroll), &
                    new_unittest("scroll-point-not-visible", test_not_visible), &
                    new_unittest("scroll-error-top-bottom", test_error_top_bottom), &
                    new_unittest("scroll-goal-column", test_goal_column), &
                    new_unittest("recenter", test_recenter), &
                    new_unittest("recenter-top-bottom", test_recenter_top_bottom), &
                    new_unittest("move-to-window-line", test_move_to_window_line), &
                    new_unittest("scroll-keys", test_keys) &
                    ]
    end subroutine collect_window_fm

    !> Line `k` of the test buffer starts here.
    pure integer(int64) function line(k)
        integer, intent(in) :: k
        line = 7*k - 6
    end function line

    !> A fresh 30-line buffer shown in the root window from its start,
    !> point at 1.
    subroutine enter(h)
        type(handle_type), intent(out) :: h
        type(status_type) :: st
        character(len=7) :: text
        integer :: k
        call get_buffer_create("window-test", h, st)
        call set_buffer(h, st)
        do k = 1, 30
            write (text, '("line", i2.2, a)') k, achar(10)
            call insert(text, st)
        end do
        call goto_char(1_int64)
        selected_frame%minibuffer_selected = .false.
        selected_frame%root_window%total_lines = 11
        selected_frame%root_window%total_cols = 80
        call set_window_buffer(selected_frame%root_window, h, st)
        this_command = nil()
        last_command = nil()
    end subroutine enter

    subroutine leave(h)
        type(handle_type), intent(in) :: h
        type(status_type) :: st
        call free_window(selected_frame%root_window)
        call kill_buffer(h, st)
        selected_frame%root_window%total_lines = 0
        selected_frame%root_window%total_cols = 0
        selected_frame%garbaged = .true.
    end subroutine leave

    subroutine test_scroll(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        call enter(h)
        ! A screenful less two lines of context; point follows the top.
        call scroll_up(nil(), st)
        call check(error, st%ok() .and. window_start(selected_frame%root_window) == line(9) &
                   .and. point() == line(9), "scroll-up")
        if (allocated(error)) return
        call scroll_up(nil(), st)
        call check(error, window_start(selected_frame%root_window) == line(17) .and. point() == line(17))
        if (allocated(error)) return
        ! Point is still in the window: it stays.
        call scroll_down(nil(), st)
        call check(error, st%ok() .and. window_start(selected_frame%root_window) == line(9) &
                   .and. point() == line(17), "scroll-down")
        if (allocated(error)) return
        ! Point would fall off the bottom: the last line of the window.
        call scroll_down(nil(), st)
        call check(error, window_start(selected_frame%root_window) == line(1) .and. point() == line(10))
        if (allocated(error)) return
        call scroll_down(nil(), st)
        call check(error, st%code == FM_BEGINNING_OF_BUFFER, "beginning-of-buffer")
        if (allocated(error)) return
        ! By lines; `-` reverses a screenful.
        call scroll_up(make_fixnum(3_int64), st)
        call check(error, window_start(selected_frame%root_window) == line(4) .and. point() == line(10))
        if (allocated(error)) return
        call scroll_down(sym("-"), st)
        call check(error, window_start(selected_frame%root_window) == line(12) .and. point() == line(12))
        if (allocated(error)) return
        call scroll_up(make_fixnum(-2_int64), st)
        call check(error, window_start(selected_frame%root_window) == line(10) .and. point() == line(12))
        if (allocated(error)) return
        ! Near the end: nothing more to show.
        call set_window_start(selected_frame%root_window, line(25), st)
        call goto_char(line(25))
        call scroll_up(nil(), st)
        call check(error, st%code == FM_END_OF_BUFFER .and. point() == line(25), "end-of-buffer")
        call leave(h)
    end subroutine test_scroll

    subroutine test_not_visible(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        call enter(h)
        ! Point below the window: scroll from point recentered.
        call goto_char(line(20))
        call scroll_up(make_fixnum(1_int64), st)
        call check(error, st%ok() .and. window_start(selected_frame%root_window) == line(16) &
                   .and. point() == line(20))
        call leave(h)
    end subroutine test_not_visible

    subroutine test_error_top_bottom(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        call enter(h)
        scroll_error_top_bottom = .true.
        call check_error_top_bottom(error)
        scroll_error_top_bottom = .false.
        call leave(h)
    end subroutine test_error_top_bottom

    subroutine check_error_top_bottom(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        call set_window_start(selected_frame%root_window, line(25), st)
        call goto_char(line(26))
        call scroll_up_command(nil(), st)
        call check(error, st%ok() .and. point() == line(31), "to the end")
        if (allocated(error)) return
        call scroll_up_command(nil(), st)
        call check(error, st%code == FM_END_OF_BUFFER, "then signal")
        if (allocated(error)) return
        ! By lines when a scroll can't be done.
        call set_window_start(selected_frame%root_window, line(1), st)
        call goto_char(line(3))
        call scroll_down_command(make_fixnum(2_int64), st)
        call check(error, st%ok() .and. point() == line(1), "forward-line back")
        if (allocated(error)) return
        ! A negative argument scrolls the other way.
        call scroll_down_command(make_fixnum(-2_int64), st)
        call check(error, st%ok() .and. window_start(selected_frame%root_window) == line(3) &
                   .and. point() == line(3))
    end subroutine check_error_top_bottom

    subroutine test_goal_column(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        call enter(h)
        goal_column = 3
        call scroll_up_command(nil(), st)
        goal_column = -1
        call check(error, st%ok() .and. point() == line(9) + 3)
        call leave(h)
    end subroutine test_goal_column

    subroutine test_recenter(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        call enter(h)
        call goto_char(line(20) + 2)
        selected_frame%garbaged = .false.
        ! Point on the middle line, the frame redrawn.
        call recenter(nil(), .true., st)
        call check(error, st%ok() .and. window_start(selected_frame%root_window) == line(15) &
                   .and. point() == line(20) + 2 .and. selected_frame%garbaged, "middle")
        if (allocated(error)) return
        selected_frame%garbaged = .false.
        call recenter(make_fixnum(0_int64), .true., st)
        call check(error, window_start(selected_frame%root_window) == line(20), "top")
        if (allocated(error)) return
        call recenter(make_fixnum(-1_int64), .true., st)
        call check(error, window_start(selected_frame%root_window) == line(11), "bottom")
        if (allocated(error)) return
        call recenter(make_fixnum(3_int64), .true., st)
        call check(error, window_start(selected_frame%root_window) == line(17), "line 3")
        if (allocated(error)) return
        ! Too large: the last line.
        call recenter(make_fixnum(50_int64), .true., st)
        call check(error, window_start(selected_frame%root_window) == line(11), "clipped")
        if (allocated(error)) return
        ! C-u: center without redrawing.
        call recenter(list(make_fixnum(4_int64)), .true., st)
        call check(error, window_start(selected_frame%root_window) == line(15) &
                   .and. .not. selected_frame%garbaged, "C-u")
        if (allocated(error)) return
        ! Near the top: the start can't go before the buffer.
        call goto_char(line(2))
        call recenter(nil(), .false., st)
        call check(error, window_start(selected_frame%root_window) == line(1) &
                   .and. .not. selected_frame%garbaged, "near the top")
        call leave(h)
    end subroutine test_recenter

    subroutine test_recenter_top_bottom(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        integer(int64), parameter :: expected(4) = [15, 20, 11, 15]
        integer :: k
        call enter(h)
        call goto_char(line(20))
        this_command = sym("recenter-top-bottom")
        ! Middle, top, bottom, then middle again.
        do k = 1, 4
            call recenter_top_bottom(nil(), st)
            call check(error, st%ok() .and. window_start(selected_frame%root_window) == line(int(expected(k))))
            if (allocated(error)) return
            last_command = this_command
        end do
        ! Another command in between starts over.
        call recenter_top_bottom(nil(), st)
        last_command = nil()
        call recenter_top_bottom(nil(), st)
        call check(error, window_start(selected_frame%root_window) == line(15), "starts over")
        if (allocated(error)) return
        ! With an argument: as recenter.
        call recenter_top_bottom(make_fixnum(2_int64), st)
        call check(error, window_start(selected_frame%root_window) == line(18), "argument")
        call leave(h)
    end subroutine test_recenter_top_bottom

    subroutine test_move_to_window_line(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        integer(int64), parameter :: expected(4) = [6, 1, 10, 6]
        integer(int64) :: vpos
        integer :: k
        call enter(h)
        call goto_char(line(4))
        call move_to_window_line(nil(), vpos, st)
        call check(error, st%ok() .and. point() == line(6) .and. vpos == 5, "middle")
        if (allocated(error)) return
        call move_to_window_line(make_fixnum(-1_int64), vpos, st)
        call check(error, point() == line(10) .and. vpos == 9, "bottom")
        if (allocated(error)) return
        call move_to_window_line(make_fixnum(2_int64), vpos, st)
        call check(error, point() == line(3) .and. vpos == 2, "line 2")
        if (allocated(error)) return
        this_command = sym("move-to-window-line-top-bottom")
        do k = 1, 4
            call move_to_window_line_top_bottom(nil(), st)
            call check(error, st%ok() .and. point() == line(int(expected(k))))
            if (allocated(error)) return
            last_command = this_command
        end do
        call leave(h)
    end subroutine test_move_to_window_line

    !> C-v, M-v and C-l C-l through the command loop.
    subroutine test_keys(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        call discard_input()
        prefix_arg = nil()
        call init_bindings()
        call enter(h)
        call kbd_store_bytes(achar(22)//achar(22)//achar(27)//"v")
        call command_loop(st)
        call check(error, window_start(selected_frame%root_window) == line(9) .and. point() == line(17))
        if (allocated(error)) return
        call goto_char(line(20))
        call kbd_store_bytes(achar(12)//achar(12))
        call command_loop(st)
        call check(error, window_start(selected_frame%root_window) == line(20), "C-l C-l")
        call leave(h)
    end subroutine test_keys
end module test_window_fm
