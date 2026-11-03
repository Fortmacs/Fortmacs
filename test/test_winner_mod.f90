! test_winner_mod.f90 -- tests for winner_mod and the window
! configuration infrastructure it uses.
! Ports GNU Emacs 31.1 test/lisp/winner-tests.el in part; the parts that
! set `winner_dont_bind_my_keys' with setopt wait for its :set function
! (Customize).
!
! Copyright (C) 2025-2026 Free Software Foundation, Inc.
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

!> Ports winner-dont-bind-my-keys (its first check); own tests of
!> undoing and redoing window configurations, comparing window
!> configurations and `window_configuration_change_hook'.  Redisplay, which
!> runs the window change hooks, does not run between the keys of a test,
!> so the tests call `execute_window_change_functions' where it would.
module test_winner_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_mod, only: status_type
    use handle_mod, only: handle_type, null_handle
    use value_mod, only: value_type, make_keymap_ref
    use fns_mod, only: value_equal
    use buffer_mod, only: get_or_make_buffer, set_buffer, close_buffer, buffers, buffer_list
    use keymap_mod, only: key_parse, lookup_key
    use keyboard_mod, only: unread_command_events, last_message, command_loop, discard_input, this_command, &
                            last_command
    use callint_mod, only: prefix_arg, prefix_arg_type, current_prefix_arg
    use insdel_mod, only: insert
    use editfns_mod, only: goto_char
    use frame_mod, only: selected_frame, ensure_frame_windows
    use modes_mod, only: minor_modes
    use window_mod, only: window_leaves, is_window_live, delete_other_windows, delete_window_tree, resize_window_tree, &
                          set_window_buffer, &
                          window_configuration_type, current_window_configuration, destroy_window_configuration, &
                          is_window_configuration_equal, copy_window_configuration, set_window_configuration, &
                          execute_window_change_functions, window_configuration_change_hook
    use bindings_mod, only: init_bindings
    use winner_mod, only: winner_mode_map, winner_boring_buffers
    implicit none(type, external)
    private

    public :: collect_winner_mod

    !> How many times `count_changes' ran.
    integer :: changes = 0

contains

    !> Collect all exported unit tests.
    subroutine collect_winner_mod(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("window-configuration-equal-p", test_configuration_equal), &
                    new_unittest("window-configuration-change-hook", test_change_hook), &
                    new_unittest("winner-undo-redo", test_undo_redo), &
                    new_unittest("winner-boring-buffers", test_boring_buffers), &
                    new_unittest("winner-dont-bind-my-keys", test_dont_bind_my_keys) &
                    ]
    end subroutine collect_winner_mod

    !> One window on a 23 by 80 frame showing a buffer of three lines.
    !> The windows of the frame are kept from one test to the next
    !> (deleted with `delete_other_windows'), as the configurations Winner
    !> mode saved refer to them.
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
        call get_or_make_buffer("winner-test", h, st)
        call set_buffer(h, st)
        call insert("abc"//achar(10)//"def"//achar(10)//"ghi", st)
        call goto_char(1_int64)
        call set_window_buffer(selected_frame%selected_window, h, st)
        this_command => null()
        last_command => null()
        call execute_window_change_functions(st)
    end subroutine enter_frame

    subroutine leave_frame()
        type(handle_type), allocatable :: all(:)
        type(status_type) :: st
        integer :: k

        ! No prefix argument is left for the next suites.
        current_prefix_arg = prefix_arg_type()
        prefix_arg = prefix_arg_type()
        if (minor_modes%winner_mode%enabled()) call minor_modes%winner_mode%disable(st)
        all = buffer_list()

        do k = 1, size(all)
            if (buffers(all(k)%index)%name == "winner-test" .or. buffers(all(k)%index)%name == "*Completions*") then
                call close_buffer(all(k), st)
            end if
        end do
    end subroutine leave_frame

    !> Execute `keys` (an is_key_valid string) through the command loop,
    !> then run the window change hooks as redisplay would.
    subroutine execute_keys(keys)
        character(len=*), intent(in) :: keys
        type(value_type), allocatable :: evs(:)
        type(status_type) :: st

        call key_parse(keys, evs, st)
        call unread_command_events(evs)
        call command_loop(st)
        call execute_window_change_functions(st)
    end subroutine execute_keys

    !> The number of live windows.
    integer function window_count()
        type(handle_type), allocatable :: list(:)

        call window_leaves(selected_frame%root_window, list)
        window_count = size(list)
    end function window_count

    !> Count the window configuration changes.
    recursive subroutine count_changes(status)
        type(status_type), intent(out) :: status

        status = status_type()
        changes = changes + 1
    end subroutine count_changes

    !> Window configurations of the same layout are equal, whatever the
    !> points; another layout is not.
    subroutine test_configuration_equal(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(window_configuration_type) :: a, b, c

        call enter_frame(h)
        call current_window_configuration(a)
        call goto_char(5_int64)
        call current_window_configuration(b)
        call check(error, is_window_configuration_equal(a, b), "same layout")
        if (allocated(error)) return
        call execute_keys("C-x 2")
        call destroy_window_configuration(b)
        call current_window_configuration(b)
        call check(error,.not. is_window_configuration_equal(a, b), "split")
        if (allocated(error)) return
        call copy_window_configuration(a, c)
        call check(error, is_window_configuration_equal(a, c), "a copy")
        if (allocated(error)) return
        call destroy_window_configuration(a)
        call set_window_configuration(c)
        call check(error, window_count() == 1, "the copy restored")
        call destroy_window_configuration(b)
        call destroy_window_configuration(c)
        call leave_frame()
    end subroutine test_configuration_equal

    !> The hook runs when a window was added, deleted or resized or changed
    !> its buffer since the last time, once.
    subroutine test_change_hook(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call enter_frame(h)
        call window_configuration_change_hook%prepend(count_changes)
        changes = 0
        call execute_window_change_functions(st)
        call check(error, changes == 0, "no change")
        if (allocated(error)) return
        call execute_keys("C-x 2")
        call check(error, changes == 1, "split")
        if (allocated(error)) return
        call execute_window_change_functions(st)
        call check(error, changes == 1, "only once")
        if (allocated(error)) return
        call execute_keys("C-x 0")
        call check(error, changes == 2, "deleted")
        if (allocated(error)) return
        call execute_keys("C-x o")
        call check(error, changes == 2, "selecting is no configuration change")
        call window_configuration_change_hook%remove(count_changes)
        call leave_frame()
    end subroutine test_change_hook

    !> Winner mode binds C-c <left> in its map.  Ports winner-tests.el
    !> winner-dont-bind-my-keys (its first check).
    subroutine test_dont_bind_my_keys(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(value_type), allocatable :: evs(:)
        type(value_type) :: val
        type(status_type) :: st

        call enter_frame(h)
        call execute_keys("M-x winner_mode RET")
        call key_parse("C-c <left>", evs, st)
        call lookup_key(make_keymap_ref(winner_mode_map), evs, val, st)
        call check(error, allocated(val%a), "C-c <left> bound")
        if (allocated(error)) return
        call check(error, value_equal(val, value_type(name="winner_undo")), "to winner_undo")
        call leave_frame()
    end subroutine test_dont_bind_my_keys

    !> C-c <left> goes back to the configuration before the last change,
    !> C-c <right> returns to it; outside Winner mode, or after another
    !> command, an error says why not.
    subroutine test_undo_redo(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h

        call enter_frame(h)
        call execute_keys("M-x winner_undo RET")
        call check(error, last_message == "Winner mode is turned off", last_message)
        if (allocated(error)) return
        call execute_keys("M-x winner_mode RET")
        call execute_keys("C-x 2")
        call check(error, window_count() == 2, "split")
        if (allocated(error)) return
        call execute_keys("C-x 3")
        call check(error, window_count() == 3, "split again")
        if (allocated(error)) return
        call execute_keys("C-c <left>")
        call check(error, window_count() == 2 .and. last_message == "Winner undo (1 / 2)", last_message)
        if (allocated(error)) return
        call execute_keys("C-c <left>")
        call check(error, window_count() == 1 .and. last_message == "Winner undo (2 / 2)", last_message)
        if (allocated(error)) return
        call execute_keys("C-c <left>")
        call check(error, window_count() == 1 .and. last_message == "No further window configuration undo information", &
                   last_message)
        if (allocated(error)) return
        call execute_keys("C-c <right>")
        call check(error, window_count() == 3 .and. last_message == "Winner undid undo", last_message)
        if (allocated(error)) return
        call execute_keys("C-c <right>")
        call check(error, last_message == "Previous command was not a ‘winner_undo’", last_message)
        call leave_frame()
    end subroutine test_undo_redo

    !> Windows showing a boring buffer are not restored.
    subroutine test_boring_buffers(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, completions
        type(handle_type), allocatable :: list(:)
        type(status_type) :: st

        call enter_frame(h)
        call execute_keys("M-x winner_mode RET")
        call execute_keys("C-x 2")
        call window_leaves(selected_frame%root_window, list)
        call get_or_make_buffer(trim(winner_boring_buffers(1)), completions, st)
        call set_window_buffer(list(2), completions, st)
        call execute_keys("C-f")
        call execute_keys("C-x 3")
        call check(error, window_count() == 3, "three windows")
        if (allocated(error)) return
        ! The configuration with *Completions* below becomes one window.
        call execute_keys("C-c <left>")
        call check(error, window_count() == 1, "the boring window deleted")
        call leave_frame()
    end subroutine test_boring_buffers
end module test_winner_mod
