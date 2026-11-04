! test_follow_mod.f90 -- tests for follow_mod.
! Ports GNU Emacs 31.1 test/lisp/follow-tests.el.
!
! Copyright (C) 2026 Free Software Foundation, Inc.
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

!> Ports follow-tests-do-nothing-test (upstream's only test: loading
!> works); own tests of the window chain, the alignment Follow mode makes
!> before redisplay, its commands and keys.  Redisplay does not run in
!> the tests: `record_window_ends' records where each window's display
!> ends, as redisplay does.
module test_follow_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use callint_mod, only: prefix_arg_type, PREFIX_NUMBER
    use error_mod, only: status_type, FM_END_OF_BUFFER
    use handle_mod, only: handle_type, null_handle
    use value_mod, only: value_type
    use buffer_mod, only: get_or_make_buffer, set_buffer, close_buffer, buffers, current
    use insdel_mod, only: insert
    use editfns_mod, only: goto_char, point, point_max
    use keymap_mod, only: key_parse
    use keyboard_mod, only: unread_command_events, command_loop, discard_input, last_message
    use callint_mod, only: prefix_arg
    use modes_mod, only: minor_modes
    use bindings_mod, only: init_bindings
    use frame_mod, only: selected_frame, ensure_frame_windows
    use xdisp_mod, only: pre_redisplay_function
    use window_mod, only: windows, window_start, window_end, window_leaves, same_window, delete_window_tree, &
                          resize_window_tree, set_window_buffer, select_window, set_window_start, window_point, &
                          set_window_point, is_pos_visible_in_window, window_body_lines
    use follow_mod, only: follow_all_followers, follow_post_command_hook, scroll_follow_up, scroll_follow_down, &
                          follow_delete_other_windows_and_split, follow_mode
    implicit none(type, external)
    private

    public :: collect_follow_mod

contains

    !> Collect all exported unit tests.
    subroutine collect_follow_mod(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("follow-tests-do-nothing-test", test_do_nothing), &
                    new_unittest("follow-delete-other-windows-and-split", test_split), &
                    new_unittest("follow-point-moves-to-next-window", test_point_moves), &
                    new_unittest("follow-scroll", test_scroll), &
                    new_unittest("follow-window-commands", test_window_commands), &
                    new_unittest("follow-mode-off", test_mode_off) &
                    ]
    end subroutine collect_follow_mod

    !> Line `k` of the test buffer starts here ("lineNN" and a newline).
    pure integer(int64) function line(k)
        integer, intent(in) :: k

        line = 7*k - 6
    end function line

    !> A 60-line buffer in one window of 11 lines (10 text lines) by 80
    !> columns, point at 1.
    subroutine enter(h)
        type(handle_type), intent(out) :: h
        type(status_type) :: st
        character(len=7) :: text
        integer :: k

        call discard_input()
        prefix_arg = prefix_arg_type()
        call init_bindings()
        call get_or_make_buffer("follow-test", h, st)
        call set_buffer(h, st)

        do k = 1, 60
            write (text, '("line", i2.2, a)') k, achar(10)
            call insert(text, st)
        end do

        call goto_char(1_int64)
        selected_frame%minibuffer_selected = .false.
        call delete_window_tree(selected_frame%root_window)
        selected_frame%root_window = null_handle()
        call ensure_frame_windows()
        call resize_window_tree(selected_frame%root_window, 0, 0, 11, 80, 80)
        call set_window_buffer(selected_frame%selected_window, h, st)
    end subroutine enter

    subroutine leave(h)
        type(handle_type), intent(in) :: h
        type(status_type) :: st

        call set_buffer(h, st)
        if (minor_modes%follow_mode%enabled()) call minor_modes%follow_mode%disable(st)
        call close_buffer(h, st)
        call delete_window_tree(selected_frame%root_window)
        selected_frame%root_window = null_handle()
    end subroutine leave

    !> Execute `keys` (an is_key_valid string) through the command loop,
    !> then what Follow mode does before redisplay.
    subroutine execute_keys(keys)
        character(len=*), intent(in) :: keys
        type(value_type), allocatable :: evs(:)
        type(status_type) :: st

        call key_parse(keys, evs, st)
        call unread_command_events(evs)
        call command_loop(st)
        call follow_post_command_hook()
        call record_window_ends()
    end subroutine execute_keys

    !> C-c . 1 (follow_delete_other_windows_and_split), then what Follow
    !> mode does before redisplay.
    subroutine split_and_follow()
        type(status_type) :: st

        call follow_delete_other_windows_and_split(prefix_arg_type(), st)
        call follow_post_command_hook()
        call record_window_ends()
    end subroutine split_and_follow

    !> Record where each window's display ends, as redisplay does
    !> (window.h window_end_pos), and move the point of another window
    !> that does not show it to the middle of the window, as redisplay does
    !> for a forced start (the test's lines are all 7 characters).  Not a
    !> port.
    subroutine record_window_ends()
        type(handle_type), allocatable :: ws(:)
        type(status_type) :: st
        integer :: k

        call window_leaves(selected_frame%root_window, ws)

        do k = 1, size(ws)
            associate (w => windows(ws(k)%index))
                w%window_end_pos = buffers(current)%text%z - window_end(ws(k), st, update=.true.)

                if (.not. same_window(ws(k), selected_frame%selected_window) .and. &
                    .not. is_pos_visible_in_window(window_point(w), ws(k))) then
                    call set_window_point(w, min(point_max(), window_start(w) + 7*(window_body_lines(w)/2 - 1)), st)
                end if
            end associate
        end do
    end subroutine record_window_ends

    !> The live windows, top to bottom and left to right.
    function leaves() result(list)
        type(handle_type), allocatable :: list(:)

        call window_leaves(selected_frame%root_window, list)
    end function leaves

    !> Loading Follow mode works.
    subroutine test_do_nothing(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, .true.)
    end subroutine test_do_nothing

    !> C-c . 1: two side-by-side windows in Follow mode, the second
    !> starting where the first ends.
    subroutine test_split(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(handle_type), allocatable :: ws(:), chain(:)

        call enter(h)
        call split_and_follow()
        ws = leaves()
        call check(error, size(ws) == 2, "two windows")
        if (allocated(error)) return
        call check(error, windows(ws(2)%index)%left_col == 40 .and. minor_modes%follow_mode%enabled(), &
                   "side by side, Follow mode")
        if (allocated(error)) return
        call check(error, associated(pre_redisplay_function), "pre_redisplay_function")
        if (allocated(error)) return
        call check(error, window_start(windows(ws(1)%index)) == 1 .and. &
                   window_start(windows(ws(2)%index)) == line(11), "aligned")
        if (allocated(error)) return
        call check(error, same_window(selected_frame%selected_window, ws(1)), "left window selected")
        if (allocated(error)) return
        ! The chain, top and left first.
        call check(error, size(follow_all_followers()) == 2, "followers")
        if (allocated(error)) return
        chain = follow_all_followers(ws(2))
        call check(error, same_window(chain(1), ws(1)) .and. same_window(chain(2), ws(2)), "the chain")
        call leave(h)
    end subroutine test_split

    !> Moving point below the first window selects the second, which shows
    !> it; moving back up selects the first again.
    subroutine test_point_moves(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(handle_type), allocatable :: ws(:)

        call enter(h)
        call split_and_follow()
        ws = leaves()
        call execute_keys("C-u 14 C-n")
        call check(error, same_window(selected_frame%selected_window, ws(2)) .and. point() == line(15), &
                   "second window")
        if (allocated(error)) return
        call check(error, window_start(windows(ws(1)%index)) == 1 .and. &
                   window_start(windows(ws(2)%index)) == line(11), "still aligned")
        if (allocated(error)) return
        call execute_keys("C-u 10 C-p")
        call check(error, same_window(selected_frame%selected_window, ws(1)) .and. point() == line(5), &
                   "first window")
        if (allocated(error)) return
        ! M->: the end of the buffer shows in the last window, the first
        ! one before it.
        call execute_keys("M->")
        call check(error, point() == point_max() .and. same_window(selected_frame%selected_window, ws(2)), "M->")
        if (allocated(error)) return
        call check(error, window_start(windows(ws(1)%index)) == line(44) .and. &
                   window_start(windows(ws(2)%index)) == line(54), "M-> windows")
        call leave(h)
    end subroutine test_point_moves

    !> C-c . C-v and C-c . M-v scroll the chain as one window.
    subroutine test_scroll(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(handle_type), allocatable :: ws(:)
        type(status_type) :: st

        call enter(h)
        call split_and_follow()
        ws = leaves()
        ! The last window ends at line 21: the first starts two lines
        ! before (next_screen_context_lines).
        call execute_keys("C-c . C-v")
        call check(error, window_start(windows(ws(1)%index)) == line(19), "first window scrolled")
        if (allocated(error)) return
        call check(error, window_start(windows(ws(2)%index)) == line(29) .and. &
                   same_window(selected_frame%selected_window, ws(1)), "second follows")
        if (allocated(error)) return
        call execute_keys("C-c . M-v")
        call check(error, window_start(windows(ws(1)%index)) == 1 .and. &
                   window_start(windows(ws(2)%index)) == line(11), "back")
        if (allocated(error)) return
        ! At the top: an error condition occurs.
        call execute_keys("C-c . M-v")
        call check(error, window_start(windows(ws(1)%index)) == 1, "at the top")
        if (allocated(error)) return
        ! The end shows in the last window: an error condition occurs.
        call select_window(ws(1), st)
        call set_window_start(windows(ws(1)%index), line(51), st)
        call goto_char(line(55))
        call follow_post_command_hook()
        call record_window_ends()
        call check(error, window_start(windows(ws(2)%index)) == point_max(), "the end in the last window")
        if (allocated(error)) return
        call scroll_follow_up(prefix_arg_type(), st)
        call check(error, st%code == FM_END_OF_BUFFER, "end_of_buffer")
        if (allocated(error)) return
        ! With an argument: by lines.
        call scroll_follow_down(prefix_arg_type(PREFIX_NUMBER, 3_int64), st)
        call check(error, st%ok() .and. window_start(windows(selected_frame%selected_window%index)) == line(48), &
                   "scroll down by 3")
        call leave(h)
    end subroutine test_scroll

    !> The window commands of C-c .: n, p, <, > and C-l.
    subroutine test_window_commands(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(handle_type), allocatable :: ws(:)

        call enter(h)
        call split_and_follow()
        ws = leaves()
        call execute_keys("C-c . n")
        call check(error, same_window(selected_frame%selected_window, ws(2)), "next window: "//last_message)
        if (allocated(error)) return
        call execute_keys("C-c . p")
        call check(error, same_window(selected_frame%selected_window, ws(1)), "previous window")
        if (allocated(error)) return
        call execute_keys("C-c . >")
        call check(error, same_window(selected_frame%selected_window, ws(2)), "last window")
        if (allocated(error)) return
        call execute_keys("C-c . <")
        call check(error, same_window(selected_frame%selected_window, ws(1)), "first window")
        if (allocated(error)) return
        ! C-l: the middle window (of two, the first) recentered on point,
        ! the other following.
        call goto_char(line(30))
        call execute_keys("C-c . C-l")
        call check(error, same_window(selected_frame%selected_window, ws(1)) .and. &
                   window_start(windows(ws(1)%index)) == line(25), "recenter")
        if (allocated(error)) return
        call check(error, window_start(windows(ws(2)%index)) == line(35), "recenter: second follows")
        call leave(h)
    end subroutine test_window_commands

    !> Turning Follow mode off in its last buffer removes its function
    !> before redisplay.
    subroutine test_mode_off(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call enter(h)
        call follow_mode(st)
        call check(error, minor_modes%follow_mode%enabled(), "on")
        if (allocated(error)) return
        call check(error, associated(pre_redisplay_function), "on: pre_redisplay_function")
        if (allocated(error)) return
        call follow_mode(st)
        call check(error,.not. minor_modes%follow_mode%enabled() .and. .not. associated(pre_redisplay_function), "off")
        call leave(h)
    end subroutine test_mode_off
end module test_follow_mod
