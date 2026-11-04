! test_window_sides_mod.f90 -- tests for side windows (window_mod).
! Own tests; upstream has no ERT tests for side windows.
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

!> Own tests: upstream has no ERT tests for side windows.  The frame's
!> root window is 23 lines by 80 columns.
module test_window_sides_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use callint_mod, only: prefix_arg_type, prefix_arg
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_mod, only: status_type
    use handle_mod, only: handle_type, null_handle
    use value_mod, only: value_type
    use buffer_mod, only: get_or_make_buffer, set_buffer, close_buffer, buffers, buffer_list
    use keymap_mod, only: key_parse
    use keyboard_mod, only: unread_command_events, last_message, command_loop, discard_input
    use insdel_mod, only: insert
    use editfns_mod, only: goto_char
    use frame_mod, only: selected_frame, ensure_frame_windows
    use bindings_mod, only: init_bindings
    use window_mod, only: windows, window_leaves, same_window, set_window_buffer, delete_window_tree, &
                          resize_window_tree, display_buffer, display_action_parameters_type, split_window, &
                          select_window, quit_window, window_main_window, window_with_side, is_window_live, &
                          is_window_valid, window_sides_slots, window_sides_slots_type, window_sides_vertical, &
                          ignore_window_parameters, &
                          DISPLAY_ACTION_IN_SIDE_WINDOW, WINDOW_SIDE_NONE, WINDOW_SIDE_LEFT, WINDOW_SIDE_TOP, &
                          WINDOW_SIDE_RIGHT, WINDOW_SIDE_BOTTOM, WINDOW_SIDES_SLOTS_UNLIMITED, DEDICATED_SIDE, &
                          DEDICATED_NONE
    implicit none(type, external)
    private

    public :: collect_window_sides_mod

contains

    !> Collect all exported unit tests.
    subroutine collect_window_sides_mod(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("display-buffer-in-side-window", test_bottom), &
                    new_unittest("side-window-slots", test_slots), &
                    new_unittest("window-sides-slots", test_max_slots), &
                    new_unittest("side-windows-on-two-sides", test_two_sides), &
                    new_unittest("window-sides-vertical", test_vertical), &
                    new_unittest("split-and-delete-with-side-windows", test_split_delete), &
                    new_unittest("delete-other-windows-side-windows", test_delete_other), &
                    new_unittest("no-other-window", test_no_other), &
                    new_unittest("quit-side-window", test_quit) &
                    ]
    end subroutine collect_window_sides_mod

    !> One window showing a fresh buffer "side-main".
    subroutine enter_frame(h)
        type(handle_type), intent(out) :: h
        type(status_type) :: st

        call discard_input()
        prefix_arg = prefix_arg_type()
        call init_bindings()
        selected_frame%minibuffer_selected = .false.
        call delete_window_tree(selected_frame%root_window)
        selected_frame%root_window = null_handle()
        call ensure_frame_windows()
        call resize_window_tree(selected_frame%root_window, 0, 0, 23, 80, 80)
        call get_or_make_buffer("side-main", h, st)
        call set_buffer(h, st)
        call insert("main", st)
        call goto_char(1_int64)
        call set_window_buffer(selected_frame%selected_window, h, st)
    end subroutine enter_frame

    subroutine leave_frame()
        type(handle_type), allocatable :: all(:)
        type(status_type) :: st
        integer :: k

        all = buffer_list()

        do k = 1, size(all)
            if (index(buffers(all(k)%index)%name, "side-") == 1) call close_buffer(all(k), st)
        end do

        call delete_window_tree(selected_frame%root_window)
        selected_frame%root_window = null_handle()
        window_sides_slots = window_sides_slots_type()
        window_sides_vertical = .false.
        ignore_window_parameters = .false.
    end subroutine leave_frame

    !> Execute `keys` (an is_key_valid string) through the command loop.
    subroutine execute_keys(keys)
        character(len=*), intent(in) :: keys
        type(value_type), allocatable :: evs(:)
        type(status_type) :: st

        call key_parse(keys, evs, st)
        call unread_command_events(evs)
        call command_loop(st)
    end subroutine execute_keys

    !> The live windows, top to bottom and left to right.
    function leaves() result(list)
        type(handle_type), allocatable :: list(:)

        call window_leaves(selected_frame%root_window, list)
    end function leaves

    !> Show a new buffer NAME in a side window on SIDE in SLOT.
    subroutine show_side(name, side, slot, window, st)
        character(len=*), intent(in) :: name
        integer, intent(in) :: side, slot
        type(handle_type), intent(out) :: window
        type(status_type), intent(out) :: st
        type(handle_type) :: b

        call get_or_make_buffer(name, b, st)
        if (.not. st%ok()) return
        call display_buffer(b, window, st, [DISPLAY_ACTION_IN_SIDE_WINDOW], &
                            display_action_parameters_type(side=side, slot=slot))
    end subroutine show_side

    !> A bottom side window takes a fourth of the frame's height, is
    !> dedicated with DEDICATED_SIDE, and the other window is the main
    !> window; showing another buffer in slot 0 reuses it.
    subroutine test_bottom(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, side, again, main
        type(status_type) :: st

        call enter_frame(h)
        main = selected_frame%selected_window
        call show_side("side-a", WINDOW_SIDE_BOTTOM, 0, side, st)
        call check(error, st%ok() .and. is_window_live(side), "a side window")
        if (allocated(error)) return

        associate (w => windows(side%index))
            call check(error, w%window_side == WINDOW_SIDE_BOTTOM .and. w%window_slot == 0 .and. &
                       w%dedicated == DEDICATED_SIDE, "its parameters")
            if (allocated(error)) return
            call check(error, w%total_lines == 5 .and. w%top_line == 18 .and. w%total_cols == 80, "a fourth of 23 lines")
            if (allocated(error)) return
        end associate

        call check(error, same_window(window_main_window(), main) .and. &
                   same_window(selected_frame%selected_window, main), "the main window stays selected")
        if (allocated(error)) return
        call check(error, buffers(windows(side%index)%buffer%index)%window_sides_shown, "window_sides_shown")
        if (allocated(error)) return
        call show_side("side-b", WINDOW_SIDE_BOTTOM, 0, again, st)
        call check(error, same_window(again, side) .and. size(leaves()) == 2 .and. &
                   buffers(windows(side%index)%buffer%index)%name == "side-b", "slot 0 reused")
        if (allocated(error)) return
        call check(error, windows(side%index)%dedicated == DEDICATED_SIDE, "still DEDICATED_SIDE")
        call leave_frame()
    end subroutine test_bottom

    !> Slots order the windows on a side: a larger slot goes to the
    !> right of a bottom side window, a smaller one to its left; all are
    !> side windows of one internal bottom side window.
    subroutine test_slots(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, a, b, c, parent
        type(handle_type), allocatable :: ws(:)
        type(status_type) :: st

        call enter_frame(h)
        call show_side("side-a", WINDOW_SIDE_BOTTOM, 0, a, st)
        call show_side("side-b", WINDOW_SIDE_BOTTOM, 1, b, st)
        call check(error, st%ok() .and. is_window_live(b) .and. .not. same_window(a, b), "a second side window")
        if (allocated(error)) return
        parent = windows(a%index)%parent
        call check(error, same_window(windows(b%index)%parent, parent) .and. &
                   windows(parent%index)%window_side == WINDOW_SIDE_BOTTOM .and. windows(parent%index)%horizontal, &
                   "one bottom side combination")
        if (allocated(error)) return
        call check(error, windows(b%index)%left_col > windows(a%index)%left_col .and. &
                   windows(b%index)%window_slot == 1 .and. windows(b%index)%window_side == WINDOW_SIDE_BOTTOM, &
                   "slot 1 to the right")
        if (allocated(error)) return
        call show_side("side-c", WINDOW_SIDE_BOTTOM, -1, c, st)
        call check(error, st%ok() .and. is_window_live(c), "a third side window")
        if (allocated(error)) return
        ws = leaves()
        call check(error, size(ws) == 4, "four windows")
        if (allocated(error)) return
        call check(error, same_window(ws(2), c) .and. same_window(ws(3), a) .and. same_window(ws(4), b), &
                   "slots -1, 0 and 1 from the left")
        if (allocated(error)) return
        call check(error, windows(c%index)%left_col == 0 .and. &
                   windows(c%index)%total_cols + windows(a%index)%total_cols + windows(b%index)%total_cols == 80, &
                   "they share the frame's width")
        if (allocated(error)) return
        call check(error, windows(c%index)%total_cols >= 26 .and. windows(b%index)%total_cols >= 26, &
                   "about a third each")
        call leave_frame()
    end subroutine test_slots

    !> With `window_sides_slots' 1 on the bottom, a new slot reuses the
    !> window there and gives it the new slot.
    subroutine test_max_slots(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, a, b
        type(status_type) :: st

        call enter_frame(h)
        window_sides_slots%bottom = 1
        call show_side("side-a", WINDOW_SIDE_BOTTOM, 0, a, st)
        call show_side("side-b", WINDOW_SIDE_BOTTOM, 2, b, st)
        call check(error, same_window(a, b) .and. size(leaves()) == 2 .and. windows(a%index)%window_slot == 2, &
                   "reused with the new slot")
        if (allocated(error)) return
        window_sides_slots%top = 0
        call show_side("side-c", WINDOW_SIDE_TOP, 0, b, st)
        call check(error, st%ok() .and. size(leaves()) == 3 .and. windows(b%index)%window_side == WINDOW_SIDE_NONE, &
                   "no slot on top: another action shows it")
        call leave_frame()
    end subroutine test_max_slots

    !> A left side window spans the main window's height, above the
    !> bottom side window (`window_sides_vertical' false); a right side
    !> one goes to the main window's right.
    subroutine test_two_sides(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, main, bottom, left, right
        type(status_type) :: st

        call enter_frame(h)
        main = selected_frame%selected_window
        call show_side("side-a", WINDOW_SIDE_BOTTOM, 0, bottom, st)
        call show_side("side-b", WINDOW_SIDE_LEFT, 0, left, st)
        call check(error, st%ok() .and. is_window_live(left), "a left side window")
        if (allocated(error)) return

        associate (l => windows(left%index), m => windows(main%index), b => windows(bottom%index))
            call check(error, l%window_side == WINDOW_SIDE_LEFT .and. l%left_col == 0 .and. l%total_cols == 20 .and. &
                       l%top_line == 0 .and. l%total_lines == m%total_lines .and. m%left_col == 20, &
                       "left of the main window, a fourth of 80 columns")
            if (allocated(error)) return
            call check(error, b%total_cols == 80, "the bottom one spans the frame")
            if (allocated(error)) return
        end associate

        call show_side("side-c", WINDOW_SIDE_RIGHT, 0, right, st)
        call check(error, st%ok() .and. is_window_live(right), "a right side window")
        if (allocated(error)) return
        call check(error, windows(right%index)%left_col + windows(right%index)%total_cols == 80 .and. &
                   same_window(windows(right%index)%prev, main), "right of the main window")
        if (allocated(error)) return
        call check(error, same_window(window_main_window(), main), "still the main window")
        if (allocated(error)) return
        call check(error, same_window(window_with_side(WINDOW_SIDE_RIGHT, .false.), right) .and. &
                   same_window(window_with_side(WINDOW_SIDE_NONE, .false.), left), "window_with_side")
        call leave_frame()
    end subroutine test_two_sides

    !> With `window_sides_vertical', a left side window spans the
    !> frame's height and a bottom one only the main window's width.
    subroutine test_vertical(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, bottom, left
        type(status_type) :: st

        call enter_frame(h)
        window_sides_vertical = .true.
        call show_side("side-a", WINDOW_SIDE_LEFT, 0, left, st)
        call show_side("side-b", WINDOW_SIDE_BOTTOM, 0, bottom, st)
        call check(error, st%ok() .and. is_window_live(bottom), "two side windows")
        if (allocated(error)) return
        call check(error, windows(left%index)%total_lines == 23 .and. windows(bottom%index)%left_col == 20 .and. &
                   windows(bottom%index)%total_cols == 60, "the left side window is full height")
        call leave_frame()
    end subroutine test_vertical

    !> A side window can't be split; splitting the main window nests it;
    !> splitting the root window splits the main window; the main window
    !> can't be deleted, a side window can.
    subroutine test_split_delete(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, main, side, new, parent
        type(status_type) :: st

        call enter_frame(h)
        main = selected_frame%selected_window
        call show_side("side-a", WINDOW_SIDE_BOTTOM, 0, side, st)
        call split_window(side, 0, .false., .false., new, st)
        call check(error,.not. st%ok(), "a side window can't be split")
        if (allocated(error)) return
        call check(error, st%detail == "Cannot split side window or parent of side window", st%detail)
        if (allocated(error)) return
        call execute_keys("C-x 0")
        call check(error, last_message == "Attempt to delete main window of frame F1", last_message)
        if (allocated(error)) return
        call execute_keys("C-x 2")
        parent = windows(main%index)%parent
        call check(error, size(leaves()) == 3 .and. same_window(windows(side%index)%prev, parent) .and. &
                   .not. windows(parent%index)%horizontal, "the main window nested in a new parent")
        if (allocated(error)) return
        call check(error, same_window(window_main_window(), parent), "the main window is now internal")
        if (allocated(error)) return
        call split_window(selected_frame%root_window, 0, .false., .true., new, st)
        call check(error, st%ok() .and. size(leaves()) == 4 .and. windows(side%index)%total_cols == 80 .and. &
                   windows(side%index)%total_lines == 5, "the root window splits the main window")
        if (allocated(error)) return
        call select_window(side, st)
        call execute_keys("C-x 0")
        call check(error, size(leaves()) == 3 .and. .not. is_window_valid(window_with_side(WINDOW_SIDE_NONE, .true.)), &
                   "the side window deleted")
        call leave_frame()
    end subroutine test_split_delete

    !> C-x 1 in a side window is an error; in the main window it deletes
    !> the side windows too, unless they have `no_delete_other_windows'.
    subroutine test_delete_other(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, main, side, new
        type(status_type) :: st

        call enter_frame(h)
        main = selected_frame%selected_window
        call show_side("side-a", WINDOW_SIDE_BOTTOM, 0, side, st)
        call select_window(side, st)
        call execute_keys("C-x 1")
        call check(error, last_message == "Cannot make side window the only window", last_message)
        if (allocated(error)) return
        call select_window(main, st)
        windows(side%index)%no_delete_other_windows = .true.
        call execute_keys("C-x 2")
        call execute_keys("C-x 1")
        call check(error, size(leaves()) == 2 .and. is_window_live(side) .and. is_window_live(main), &
                   "the side window stays")
        if (allocated(error)) return
        call check(error, windows(main%index)%total_lines == 18 .and. windows(main%index)%top_line == 0, &
                   "the main window takes the main area")
        if (allocated(error)) return
        call execute_keys("C-x 1")
        call check(error, last_message == "No other windows to delete", last_message)
        if (allocated(error)) return
        windows(side%index)%no_delete_other_windows = .false.
        call split_window(main, 0, .false., .false., new, st)
        call execute_keys("C-x 1")
        call check(error, size(leaves()) == 1 .and. windows(main%index)%total_lines == 23, "all others deleted")
        call leave_frame()
    end subroutine test_delete_other

    !> C-x o skips a window whose `no_other_window' is true, unless
    !> `ignore_window_parameters'.
    subroutine test_no_other(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, main, side
        type(status_type) :: st

        call enter_frame(h)
        main = selected_frame%selected_window
        call show_side("side-a", WINDOW_SIDE_BOTTOM, 0, side, st)
        windows(side%index)%no_other_window = .true.
        call execute_keys("C-x o")
        call check(error, same_window(selected_frame%selected_window, main), "the side window skipped")
        if (allocated(error)) return
        call check(error, last_message == "No other window to select", last_message)
        if (allocated(error)) return
        ignore_window_parameters = .true.
        call execute_keys("C-x o")
        call check(error, same_window(selected_frame%selected_window, side), "not with ignore_window_parameters")
        call leave_frame()
    end subroutine test_no_other

    !> Quitting a side window deletes it (it was made for its buffer).
    subroutine test_quit(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, side
        type(status_type) :: st

        call enter_frame(h)
        call show_side("side-a", WINDOW_SIDE_TOP, 0, side, st)
        call check(error, windows(side%index)%top_line == 0 .and. windows(side%index)%total_lines == 5, &
                   "a top side window")
        if (allocated(error)) return
        call quit_window(.false., st, side)
        call check(error, st%ok() .and. size(leaves()) == 1 .and. &
                   windows(selected_frame%root_window%index)%dedicated == DEDICATED_NONE, "quit deletes it")
        call leave_frame()
    end subroutine test_quit
end module test_window_sides_mod
