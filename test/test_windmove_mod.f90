! test_windmove_mod.f90 -- tests for windmove_mod and window_in_direction.
! Own tests; upstream has no ERT tests for windmove.el or
! window-in-direction.
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

!> Own tests: upstream has no ERT tests for windmove.el.  The frame is
!> 23 lines by 80 columns; C-x 2 C-x 3 makes three windows: two side by
!> side on top (12 lines, 40 columns each) and one below (11 lines).
module test_windmove_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_mod, only: status_type
    use handle_mod, only: handle_type, null_handle
    use value_mod, only: value_type, make_keymap_ref
    use fns_mod, only: value_equal
    use buffer_mod, only: get_or_make_buffer, set_buffer, close_buffer, buffers, buffer_list, get_buffer, is_buffer_live
    use keymap_mod, only: key_parse, lookup_key
    use keyboard_mod, only: unread_command_events, last_message, command_loop, discard_input, this_command, &
                            last_command
    use callint_mod, only: prefix_arg, prefix_arg_type, current_prefix_arg
    use insdel_mod, only: insert
    use editfns_mod, only: goto_char
    use frame_mod, only: selected_frame, ensure_frame_windows
    use window_mod, only: windows, window_leaves, is_window_live, delete_other_windows, same_window, delete_window_tree, &
                          resize_window_tree, set_window_buffer, &
                          window_in_direction, DIRECTION_ABOVE, DIRECTION_BELOW, DIRECTION_LEFT, DIRECTION_RIGHT, &
                          MINIBUF_YES
    use bindings_mod, only: init_bindings
    use windmove_mod, only: windmove_default_keybindings, windmove_delete_default_keybindings, windmove_mode_map, &
                            windmove_wrap_around, windmove_create_window
    implicit none(type, external)
    private

    public :: collect_windmove_mod

    !> The minibuffer window's top line, height, left column and width
    !> before a test.
    integer :: saved_mini(4) = 0

contains

    !> Collect all exported unit tests.
    subroutine collect_windmove_mod(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("window-in-direction", test_window_in_direction), &
                    new_unittest("windmove-default-keybindings", test_keybindings), &
                    new_unittest("windmove-select", test_select), &
                    new_unittest("windmove-wrap-around", test_wrap_around), &
                    new_unittest("windmove-create-window", test_create_window), &
                    new_unittest("windmove-delete", test_delete) &
                    ]
    end subroutine collect_windmove_mod

    !> Three windows on a 23 by 80 frame showing a buffer of 3 lines,
    !> the top left one selected.
    subroutine enter_frame(h)
        type(handle_type), intent(out) :: h
        type(status_type) :: st

        call discard_input()
        prefix_arg = prefix_arg_type()
        call init_bindings()
        selected_frame%minibuffer_selected = .false.

        if (.not. is_window_live(selected_frame%selected_window)) then
            call delete_window_tree(selected_frame%root_window)
            selected_frame%root_window = null_handle()
            call ensure_frame_windows()
        else
            call delete_other_windows(selected_frame%selected_window, st)
        end if

        call resize_window_tree(selected_frame%root_window, 0, 0, 23, 80, 80)

        ! The minibuffer window on the last line (its geometry is put back
        ! on leaving).
        associate (m => windows(selected_frame%minibuffer_window%index))
            saved_mini = [m%top_line, m%total_lines, m%left_col, m%total_cols]
        end associate

        windows(selected_frame%minibuffer_window%index)%top_line = 23
        windows(selected_frame%minibuffer_window%index)%total_lines = 1
        windows(selected_frame%minibuffer_window%index)%left_col = 0
        windows(selected_frame%minibuffer_window%index)%total_cols = 80
        call get_or_make_buffer("windmove-test", h, st)
        call set_buffer(h, st)
        call insert("abc"//achar(10)//"def"//achar(10)//"ghi", st)
        call goto_char(1_int64)
        call set_window_buffer(selected_frame%selected_window, h, st)
        call execute_keys("C-x 2 C-x 3")
        this_command => null()
        last_command => null()
    end subroutine enter_frame

    subroutine leave_frame()
        type(handle_type), allocatable :: all(:)
        type(status_type) :: st
        integer :: k

        ! No prefix argument is left for the next suites.
        current_prefix_arg = prefix_arg_type()
        prefix_arg = prefix_arg_type()
        all = buffer_list()

        do k = 1, size(all)
            if (buffers(all(k)%index)%name == "windmove-test" .or. buffers(all(k)%index)%name == "windmove-other") then
                call close_buffer(all(k), st)
            end if
        end do

        windmove_wrap_around = .false.
        windmove_create_window = .false.

        associate (m => windows(selected_frame%minibuffer_window%index))
            m%top_line = saved_mini(1)
            m%total_lines = saved_mini(2)
            m%left_col = saved_mini(3)
            m%total_cols = saved_mini(4)
        end associate

        ! The frame keeps one window, as later suites expect.
        call delete_other_windows(selected_frame%selected_window, st)
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

    !> The live windows, in cyclic order.
    function leaves() result(list)
        type(handle_type), allocatable :: list(:)

        call window_leaves(selected_frame%root_window, list)
    end function leaves

    !> Whether H is the null handle.
    pure logical function is_none(h)
        type(handle_type), intent(in) :: h

        is_none = h%index == 0
    end function is_none

    !> Whether KEY is bound to COMMAND in `windmove_mode_map'.
    logical function is_bound(key, command)
        character(len=*), intent(in) :: key, command
        type(value_type), allocatable :: evs(:)
        type(value_type) :: val
        type(status_type) :: st

        call key_parse(key, evs, st)
        call lookup_key(make_keymap_ref(windmove_mode_map), evs, val, st)
        is_bound = .false.
        if (allocated(val%a)) is_bound = value_equal(val, value_type(name=command))
    end function is_bound

    !> The nearest window in each direction, by point or by an edge, and
    !> wrapping around.
    subroutine test_window_in_direction(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(handle_type), allocatable :: ws(:)

        call enter_frame(h)
        ws = leaves()
        call check(error, size(ws) == 3, "three windows")
        if (allocated(error)) return
        call check(error, same_window(window_in_direction(DIRECTION_RIGHT, ws(1)), ws(2)), "right of the first")
        if (allocated(error)) return
        call check(error, same_window(window_in_direction(DIRECTION_BELOW, ws(1)), ws(3)), "below the first")
        if (allocated(error)) return
        call check(error, is_none(window_in_direction(DIRECTION_ABOVE, ws(1))) .and. &
                   is_none(window_in_direction(DIRECTION_LEFT, ws(1))), "nothing above or to the left")
        if (allocated(error)) return
        call check(error, same_window(window_in_direction(DIRECTION_LEFT, ws(2)), ws(1)), "left of the second")
        if (allocated(error)) return
        ! From the bottom window, point (column 0) is below the first.
        call check(error, same_window(window_in_direction(DIRECTION_ABOVE, ws(3)), ws(1)), "above the third, by point")
        if (allocated(error)) return
        call check(error, same_window(window_in_direction(DIRECTION_ABOVE, ws(3), sign=-1_int64), ws(2)), &
                   "above the third, by its right edge")
        if (allocated(error)) return
        call check(error, same_window(window_in_direction(DIRECTION_ABOVE, ws(3), sign=1_int64), ws(1)), &
                   "above the third, by its left edge")
        if (allocated(error)) return
        ! The minibuffer window counts while it is active, or always.
        call check(error, is_none(window_in_direction(DIRECTION_BELOW, ws(3))), "nothing below the third")
        if (allocated(error)) return
        call check(error, same_window(window_in_direction(DIRECTION_BELOW, ws(3), minibuf=MINIBUF_YES), &
                                      selected_frame%minibuffer_window), "the minibuffer window below")
        if (allocated(error)) return
        ! Wrapping around the frame's edges.
        call check(error, same_window(window_in_direction(DIRECTION_LEFT, ws(1), wrap=.true.), ws(2)), "wrap left")
        if (allocated(error)) return
        call check(error, same_window(window_in_direction(DIRECTION_ABOVE, ws(1), wrap=.true.), ws(3)), "wrap above")
        if (allocated(error)) return
        call check(error, same_window(window_in_direction(DIRECTION_BELOW, ws(3), wrap=.true.), ws(1)), "wrap below")
        call leave_frame()
    end subroutine test_window_in_direction

    !> `windmove_default_keybindings' binds the arrows with modifiers in
    !> `windmove_mode_map', replacing earlier bindings.
    subroutine test_keybindings(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st

        call windmove_default_keybindings(st)
        call check(error, st%ok() .and. is_bound("S-<left>", "windmove_left") .and. is_bound("S-<right>", "windmove_right") &
                   .and. is_bound("S-<up>", "windmove_up") .and. is_bound("S-<down>", "windmove_down"), "shift")
        if (allocated(error)) return
        call windmove_default_keybindings(st, "shift", "meta")
        call check(error, st%ok() .and. is_bound("M-S-<left>", "windmove_left") .and. &
                   .not. is_bound("S-<left>", "windmove_left"), "meta and shift")
        if (allocated(error)) return
        call windmove_default_keybindings(st, "none")
        call check(error, st%ok() .and. is_bound("<up>", "windmove_up") .and. .not. is_bound("M-S-<up>", "windmove_up"), &
                   "none")
        if (allocated(error)) return
        call windmove_delete_default_keybindings(st)
        call check(error, st%ok() .and. is_bound("C-x S-<down>", "windmove_delete_down"), "delete")
        if (allocated(error)) return
        call windmove_default_keybindings(st)
        call check(error, st%ok() .and. .not. is_bound("<up>", "windmove_up"), "shift again")
    end subroutine test_keybindings

    !> S-<arrow> select the window in that direction; at an edge, an
    !> error says there is none, and below the bottom window is the inactive
    !> minibuffer.
    subroutine test_select(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(handle_type), allocatable :: ws(:)
        type(status_type) :: st

        call enter_frame(h)
        call windmove_default_keybindings(st)
        ws = leaves()
        call execute_keys("S-<right>")
        call check(error, same_window(selected_frame%selected_window, ws(2)), "right")
        if (allocated(error)) return
        call execute_keys("S-<down>")
        call check(error, same_window(selected_frame%selected_window, ws(3)), "down")
        if (allocated(error)) return
        call execute_keys("S-<down>")
        call check(error, last_message == "Minibuffer is inactive", last_message)
        if (allocated(error)) return
        ! C-u: by the window's top edge... the left one above.
        call execute_keys("C-u S-<up>")
        call check(error, same_window(selected_frame%selected_window, ws(1)), "up by the left edge")
        if (allocated(error)) return
        call execute_keys("S-<left>")
        call check(error, last_message == "No window left from selected window", last_message)
        call leave_frame()
    end subroutine test_select

    !> With `windmove_wrap_around', moving off an edge finds the window
    !> on the other side.
    subroutine test_wrap_around(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(handle_type), allocatable :: ws(:)
        type(status_type) :: st

        call enter_frame(h)
        call windmove_default_keybindings(st)
        windmove_wrap_around = .true.
        ws = leaves()
        call execute_keys("S-<left>")
        call check(error, same_window(selected_frame%selected_window, ws(2)), "left wraps to the right")
        if (allocated(error)) return
        call execute_keys("S-<up>")
        call check(error, same_window(selected_frame%selected_window, ws(3)), "up wraps to the bottom")
        call leave_frame()
    end subroutine test_wrap_around

    !> With `windmove_create_window', moving off an edge splits the window
    !> on that side and selects the new window.
    subroutine test_create_window(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, old
        type(handle_type), allocatable :: ws(:)
        type(status_type) :: st

        call enter_frame(h)
        call windmove_default_keybindings(st)
        windmove_create_window = .true.
        old = selected_frame%selected_window
        call execute_keys("S-<left>")
        ws = leaves()
        call check(error, size(ws) == 4, "a new window")
        if (allocated(error)) return
        call check(error, same_window(selected_frame%selected_window, ws(1)) .and. windows(ws(1)%index)%left_col == 0 .and. &
                   windows(ws(1)%index)%total_cols == 20 .and. windows(ws(1)%index)%divider .and. &
                   windows(old%index)%left_col == 20 .and. windows(old%index)%total_cols == 20, "on the left, selected")
        if (allocated(error)) return
        call execute_keys("S-<right> S-<up>")
        ws = leaves()
        call check(error, size(ws) == 5 .and. windows(selected_frame%selected_window%index)%top_line == 0 .and. &
                   windows(selected_frame%selected_window%index)%total_lines == 6 .and. &
                   windows(old%index)%top_line == 6, "above")
        call leave_frame()
    end subroutine test_create_window

    !> C-x S-<arrow> deletes the window in that direction; with C-u it also
    !> kills its buffer; with M-0 it deletes the selected window instead.
    subroutine test_delete(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, other
        type(handle_type), allocatable :: ws(:)
        type(status_type) :: st

        call enter_frame(h)
        call windmove_delete_default_keybindings(st)
        ws = leaves()
        call execute_keys("C-x S-<right>")
        call check(error, size(leaves()) == 2 .and. same_window(selected_frame%selected_window, ws(1)), "right deleted")
        if (allocated(error)) return
        call execute_keys("C-x S-<up>")
        call check(error, last_message == "No window up from selected window", last_message)
        if (allocated(error)) return
        ! C-u: the buffer of the window below is killed too.
        call get_or_make_buffer("windmove-other", other, st)
        call set_window_buffer(ws(3), other, st)
        call execute_keys("C-u C-x S-<down>")
        call check(error, size(leaves()) == 1 .and. .not. is_buffer_live(get_buffer("windmove-other")), &
                   "down deleted, its buffer killed")
        if (allocated(error)) return
        call execute_keys("C-x 2 M-0 C-x S-<down>")
        ws = leaves()
        call check(error, size(ws) == 1, "M-0: one window left")
        if (allocated(error)) return
        call check(error, windows(ws(1)%index)%top_line == 0 .and. windows(ws(1)%index)%total_lines == 23, &
                   "M-0: the selected one deleted")
        call leave_frame()
    end subroutine test_delete
end module test_windmove_mod
