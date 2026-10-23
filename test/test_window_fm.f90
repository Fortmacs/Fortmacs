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
    use buffer_fm, only: get_buffer_create, set_buffer, kill_buffer, buffers, cur, buffer_list, current_buffer, &
                         get_buffer, buffer_live_p
    use keymap_fm, only: key_parse
    use keyboard_fm, only: unread_command_events, last_message
    use insdel_fm, only: insert
    use editfns_fm, only: goto_char, point
    use handle_fm, only: null_handle
    use frame_fm, only: selected_frame, frame_selected_window_ref, ensure_frame_windows
    use window_fm, only: window_type, set_window_buffer, set_window_start, window_start, delete_window_tree, &
                         resize_window_tree, &
                         windows, window_leaves, same_window, window_text_cols, other_buffer, &
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
                    new_unittest("scroll-keys", test_keys), &
                    new_unittest("split-window", test_split), &
                    new_unittest("other-window", test_other_window), &
                    new_unittest("delete-window", test_delete), &
                    new_unittest("switch-to-buffer", test_switch_to_buffer), &
                    new_unittest("kill-buffer", test_kill_buffer) &
                    ]
    end subroutine collect_window_fm

    !> The selected window.
    function win() result(w)
        type(window_type), pointer :: w
        w => frame_selected_window_ref()
    end function win

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
        type(window_type), pointer :: w
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
        ! One window, 11 lines by 80 columns.
        call delete_window_tree(selected_frame%root_window)
        selected_frame%root_window = null_handle()
        call ensure_frame_windows()
        call resize_window_tree(selected_frame%root_window, 0, 0, 11, 80, 80)
        w => win()
        call set_window_buffer(w, h, st)
        this_command = nil()
        last_command = nil()
    end subroutine enter

    subroutine leave(h)
        type(handle_type), intent(in) :: h
        type(status_type) :: st
        call kill_buffer(h, st)
        call delete_window_tree(selected_frame%root_window)
        selected_frame%root_window = null_handle()
        selected_frame%garbaged = .true.
    end subroutine leave

    subroutine test_scroll(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        call enter(h)
        ! A screenful less two lines of context; point follows the top.
        call scroll_up(nil(), st)
        call check(error, st%ok() .and. window_start(win()) == line(9) &
                   .and. point() == line(9), "scroll_up")
        if (allocated(error)) return
        call scroll_up(nil(), st)
        call check(error, window_start(win()) == line(17) .and. point() == line(17))
        if (allocated(error)) return
        ! Point is still in the window: it stays.
        call scroll_down(nil(), st)
        call check(error, st%ok() .and. window_start(win()) == line(9) &
                   .and. point() == line(17), "scroll_down")
        if (allocated(error)) return
        ! Point would fall off the bottom: the last line of the window.
        call scroll_down(nil(), st)
        call check(error, window_start(win()) == line(1) .and. point() == line(10))
        if (allocated(error)) return
        call scroll_down(nil(), st)
        call check(error, st%code == FM_BEGINNING_OF_BUFFER, "beginning_of_buffer")
        if (allocated(error)) return
        ! By lines; `-` reverses a screenful.
        call scroll_up(make_fixnum(3_int64), st)
        call check(error, window_start(win()) == line(4) .and. point() == line(10))
        if (allocated(error)) return
        call scroll_down(sym("-"), st)
        call check(error, window_start(win()) == line(12) .and. point() == line(12))
        if (allocated(error)) return
        call scroll_up(make_fixnum(-2_int64), st)
        call check(error, window_start(win()) == line(10) .and. point() == line(12))
        if (allocated(error)) return
        ! Near the end: nothing more to show.
        call set_window_start(win(), line(25), st)
        call goto_char(line(25))
        call scroll_up(nil(), st)
        call check(error, st%code == FM_END_OF_BUFFER .and. point() == line(25), "end_of_buffer")
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
        call check(error, st%ok() .and. window_start(win()) == line(16) &
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
        call set_window_start(win(), line(25), st)
        call goto_char(line(26))
        call scroll_up_command(nil(), st)
        call check(error, st%ok() .and. point() == line(31), "to the end")
        if (allocated(error)) return
        call scroll_up_command(nil(), st)
        call check(error, st%code == FM_END_OF_BUFFER, "then signal")
        if (allocated(error)) return
        ! By lines when a scroll can't be done.
        call set_window_start(win(), line(1), st)
        call goto_char(line(3))
        call scroll_down_command(make_fixnum(2_int64), st)
        call check(error, st%ok() .and. point() == line(1), "forward-line back")
        if (allocated(error)) return
        ! A negative argument scrolls the other way.
        call scroll_down_command(make_fixnum(-2_int64), st)
        call check(error, st%ok() .and. window_start(win()) == line(3) &
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
        if (allocated(error)) return
        ! truncate-partial-width-windows is non-nil: the goal is a column
        ! as move-to-column counts it (column 3 is in the middle of the
        ! wide character, so point goes after it), not a screen x.
        call set_window_start(win(), line(1), st)
        call goto_char(line(9) + 2)
        call insert(char(230)//char(188)//char(162), st)
        call goto_char(line(1))
        goal_column = 3
        call scroll_up_command(nil(), st)
        goal_column = -1
        call check(error, st%ok() .and. point() == line(9) + 3, "move-to-column")
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
        call check(error, st%ok() .and. window_start(win()) == line(15) &
                   .and. point() == line(20) + 2 .and. selected_frame%garbaged, "middle")
        if (allocated(error)) return
        selected_frame%garbaged = .false.
        call recenter(make_fixnum(0_int64), .true., st)
        call check(error, window_start(win()) == line(20), "top")
        if (allocated(error)) return
        call recenter(make_fixnum(-1_int64), .true., st)
        call check(error, window_start(win()) == line(11), "bottom")
        if (allocated(error)) return
        call recenter(make_fixnum(3_int64), .true., st)
        call check(error, window_start(win()) == line(17), "line 3")
        if (allocated(error)) return
        ! Too large: the last line.
        call recenter(make_fixnum(50_int64), .true., st)
        call check(error, window_start(win()) == line(11), "clipped")
        if (allocated(error)) return
        ! C-u: center without redrawing.
        call recenter(list(make_fixnum(4_int64)), .true., st)
        call check(error, window_start(win()) == line(15) &
                   .and. .not. selected_frame%garbaged, "C-u")
        if (allocated(error)) return
        ! Near the top: the start can't go before the buffer.
        call goto_char(line(2))
        call recenter(nil(), .false., st)
        call check(error, window_start(win()) == line(1) &
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
        this_command = sym("recenter_top_bottom")
        ! Middle, top, bottom, then middle again.
        do k = 1, 4
            call recenter_top_bottom(nil(), st)
            call check(error, st%ok() .and. window_start(win()) == line(int(expected(k))))
            if (allocated(error)) return
            last_command = this_command
        end do
        ! Another command in between starts over.
        call recenter_top_bottom(nil(), st)
        last_command = nil()
        call recenter_top_bottom(nil(), st)
        call check(error, window_start(win()) == line(15), "starts over")
        if (allocated(error)) return
        ! With an argument: as recenter.
        call recenter_top_bottom(make_fixnum(2_int64), st)
        call check(error, window_start(win()) == line(18), "argument")
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
        this_command = sym("move_to_window_line_top_bottom")
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
        call check(error, window_start(win()) == line(9) .and. point() == line(17))
        if (allocated(error)) return
        call goto_char(line(20))
        call kbd_store_bytes(achar(12)//achar(12))
        call command_loop(st)
        call check(error, window_start(win()) == line(20), "C-l C-l")
        call leave(h)
    end subroutine test_keys

    !> One window on a 24-line, 80-column frame (23 lines above the
    !> minibuffer line), showing a fresh buffer "split-test" holding
    !> `text`, selected and current.
    subroutine enter_frame(text, h)
        character(len=*), intent(in) :: text
        type(handle_type), intent(out) :: h
        type(status_type) :: st
        type(window_type), pointer :: w
        call discard_input()
        prefix_arg = nil()
        call init_bindings()
        selected_frame%minibuffer_selected = .false.
        call delete_window_tree(selected_frame%root_window)
        selected_frame%root_window = null_handle()
        call ensure_frame_windows()
        call resize_window_tree(selected_frame%root_window, 0, 0, 23, 80, 80)
        call get_buffer_create("split-test", h, st)
        call set_buffer(h, st)
        call insert(text, st)
        call goto_char(1_int64)
        w => win()
        call set_window_buffer(w, h, st)
    end subroutine enter_frame

    subroutine leave_frame()
        type(handle_type), allocatable :: all(:)
        type(status_type) :: st
        integer :: k
        all = buffer_list()
        do k = 1, size(all)
            if (buffers(all(k)%index)%name == "split-test" .or. buffers(all(k)%index)%name == "other-test") then
                call kill_buffer(all(k), st)
            end if
        end do
        call delete_window_tree(selected_frame%root_window)
        selected_frame%root_window = null_handle()
    end subroutine leave_frame

    !> Run `keys` (a key_valid_p string) through the command loop.
    subroutine run_keys(keys)
        character(len=*), intent(in) :: keys
        type(value_type), allocatable :: evs(:)
        type(status_type) :: st
        call key_parse(keys, evs, st)
        call unread_command_events(evs)
        call command_loop(st)
    end subroutine run_keys

    !> The live windows, top to bottom and left to right.
    function leaves() result(list)
        type(handle_type), allocatable :: list(:)
        call window_leaves(selected_frame%root_window, list)
    end function leaves

    !> C-x 2 and C-x 3 split the selected window in half; the new window
    !> shows the same buffer and the old one stays selected.
    subroutine test_split(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(handle_type), allocatable :: ws(:)
        call enter_frame("abc", h)
        call run_keys("C-x 2")
        ws = leaves()
        call check(error, size(ws) == 2, "two windows")
        if (allocated(error)) return
        call check(error, windows(ws(1)%index)%top_line == 0 .and. windows(ws(1)%index)%total_lines == 12 .and. &
                   windows(ws(2)%index)%top_line == 12 .and. windows(ws(2)%index)%total_lines == 11, &
                   "12 and 11 lines")
        if (allocated(error)) return
        call check(error, same_window(selected_frame%selected_window, ws(1)) .and. &
                   windows(ws(2)%index)%buffer%index == h%index, "same buffer, top selected")
        if (allocated(error)) return
        call run_keys("C-x 3")
        ws = leaves()
        call check(error, size(ws) == 3, "three windows")
        if (allocated(error)) return
        call check(error, windows(ws(1)%index)%total_cols == 40 .and. windows(ws(1)%index)%divider .and. &
                   window_text_cols(windows(ws(1)%index)) == 39 .and. windows(ws(2)%index)%left_col == 40 .and. &
                   windows(ws(2)%index)%total_cols == 40 .and. .not. windows(ws(2)%index)%divider .and. &
                   windows(ws(2)%index)%total_lines == 12, "side by side")
        if (allocated(error)) return
        ! C-u 5 C-x 2: the upper window gets 5 lines.
        call run_keys("C-u 5 C-x 2")
        ws = leaves()
        call check(error, size(ws) == 4 .and. windows(ws(1)%index)%total_lines == 5 .and. &
                   windows(ws(2)%index)%total_lines == 7, "C-u 5 C-x 2")
        if (allocated(error)) return
        ! A window too small to split.
        call run_keys("C-x 2 C-x 2")
        call check(error, index(last_message, "too small for splitting") > 0, last_message)
        call leave_frame()
    end subroutine test_split

    !> C-x o cycles through the windows; each keeps its own point.
    subroutine test_other_window(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(handle_type), allocatable :: ws(:)
        call enter_frame("abcdef", h)
        call run_keys("C-x 2 C-x 3")
        ws = leaves()
        call goto_char(3_int64)
        call run_keys("C-x o")
        call check(error, same_window(selected_frame%selected_window, ws(2)) .and. point() == 1, "second window")
        if (allocated(error)) return
        call goto_char(5_int64)
        call run_keys("C-x o C-x o")
        call check(error, same_window(selected_frame%selected_window, ws(1)) .and. point() == 3, &
                   "back to the first, at its point")
        if (allocated(error)) return
        call run_keys("C-u -1 C-x o")
        call check(error, same_window(selected_frame%selected_window, ws(3)), "backwards")
        if (allocated(error)) return
        call run_keys("C-x o C-x o")
        call check(error, same_window(selected_frame%selected_window, ws(2)) .and. point() == 5, "its point")
        call leave_frame()
    end subroutine test_other_window

    !> C-x 0 gives the space to a neighbour and selects the most recently
    !> used window; C-x 1 keeps only the selected one.
    subroutine test_delete(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(handle_type), allocatable :: ws(:)
        call enter_frame("abc", h)
        call run_keys("C-x 0")
        call check(error, last_message == "Attempt to delete minibuffer or sole ordinary window", last_message)
        if (allocated(error)) return
        call run_keys("C-x 2 C-x 3 C-x o C-x o")
        ws = leaves()
        ! The third (bottom) window is selected; delete it.
        call run_keys("C-x 0")
        ws = leaves()
        call check(error, size(ws) == 2 .and. windows(ws(1)%index)%total_lines == 23 .and. &
                   windows(ws(2)%index)%total_lines == 23, "the top windows take the space")
        if (allocated(error)) return
        call check(error, same_window(selected_frame%selected_window, ws(2)), "the most recently used")
        if (allocated(error)) return
        call run_keys("C-x 0")
        ws = leaves()
        call check(error, size(ws) == 1 .and. windows(ws(1)%index)%total_cols == 80 .and. &
                   same_window(ws(1), selected_frame%root_window), "one window again")
        if (allocated(error)) return
        call run_keys("C-x 2 C-x 3 C-x o C-x 1")
        ws = leaves()
        call check(error, size(ws) == 1 .and. same_window(ws(1), selected_frame%selected_window) .and. &
                   windows(ws(1)%index)%total_lines == 23 .and. windows(ws(1)%index)%total_cols == 80, "C-x 1")
        call leave_frame()
    end subroutine test_delete

    !> C-x b switches the selected window to a buffer, making a new one
    !> for a new name; the default is the other buffer.
    subroutine test_switch_to_buffer(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, o, d
        type(handle_type), allocatable :: ws(:)
        call enter_frame("abc", h)
        call run_keys("C-x 2 C-x b o t h e r - t e s t RET")
        ws = leaves()
        o = current_buffer()
        call check(error, buffers(cur)%name == "other-test" .and. windows(ws(1)%index)%buffer%index == o%index &
                   .and. windows(ws(2)%index)%buffer%index == h%index, "the selected window shows the new buffer")
        if (allocated(error)) return
        ! RET takes the default, the other buffer (one not shown in a
        ! window if there is one).
        d = other_buffer(o)
        call run_keys("C-x b RET")
        call check(error, same_buffer(current_buffer(), d) .and. same_buffer(windows(ws(1)%index)%buffer, d))
        if (allocated(error)) return
        call run_keys("C-x b s p l i t - t e s t RET")
        call check(error, same_buffer(current_buffer(), h))
        call leave_frame()
    contains
        logical function same_buffer(a, b)
            type(handle_type), intent(in) :: a, b
            same_buffer = a%index == b%index .and. a%generation == b%generation
        end function same_buffer
    end subroutine test_switch_to_buffer

    !> C-x k kills a buffer; the windows showing it go back to the buffer
    !> they showed before.  A modified file buffer asks first.
    subroutine test_kill_buffer(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(handle_type), allocatable :: ws(:)
        call enter_frame("abc", h)
        call run_keys("C-x 2 C-x b o t h e r - t e s t RET C-x o C-x b o t h e r - t e s t RET")
        ws = leaves()
        ! Both windows show other-test now; the bottom one is selected.
        call run_keys("C-x k RET")
        call check(error,.not. buffer_live_p(get_buffer("other-test")), "killed")
        if (allocated(error)) return
        call check(error, windows(ws(1)%index)%buffer%index == h%index .and. &
                   windows(ws(2)%index)%buffer%index == h%index .and. buffers(cur)%name == "split-test", &
                   "both windows show split-test")
        if (allocated(error)) return
        ! A modified buffer visiting a file asks; "no" keeps it.
        buffers(cur)%filename = "/nonexistent/split-test"
        call run_keys("C-x k RET n o RET")
        call check(error, buffer_live_p(h), "no: kept")
        if (allocated(error)) return
        call run_keys("C-x k RET y e s RET")
        call check(error,.not. buffer_live_p(h), "yes: killed")
        call leave_frame()
    end subroutine test_kill_buffer
end module test_window_fm
