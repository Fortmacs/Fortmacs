! test_frame_mod.f90 -- tests for frame_mod (frame.el's Window Divider
! mode, the divider widths).
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

!> Own tests (upstream has no ERT tests for window dividers): Window
!> Divider mode sets the frame's divider parameters and those of new
!> frames, by `window_divider_default_places'; a terminal frame draws no
!> dividers, so the divider widths stay 0.
module test_frame_mod
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_mod, only: status_type
    use handle_mod, only: handle_type, null_handle
    use modes_mod, only: minor_modes
    use bindings_mod, only: init_bindings
    use window_mod, only: windows, window_leaves, delete_window_tree, resize_window_tree, split_window
    use frame_mod, only: selected_frame, ensure_frame_windows, default_frame, window_divider_mode, &
                         window_divider_default_places, window_divider_default_bottom_width, &
                         window_divider_default_right_width, WINDOW_DIVIDER_DEFAULT_PLACES_BOTTOM_ONLY, &
                         WINDOW_DIVIDER_DEFAULT_PLACES_RIGHT_ONLY, WINDOW_DIVIDER_DEFAULT_PLACES_BOTTOM_AND_RIGHT, &
                         frame_right_divider_width, frame_bottom_divider_width, window_right_divider_width, &
                         window_bottom_divider_width, is_window_divider_width_valid
    implicit none(type, external)
    private

    public :: collect_frame_mod

contains

    !> Collect all exported unit tests.
    subroutine collect_frame_mod(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("window-divider-mode", test_window_divider_mode), &
                    new_unittest("window-divider-widths", test_divider_widths) &
                    ]
    end subroutine collect_frame_mod

    !> The mode applies the default places and widths; off, all are 0.
    subroutine test_window_divider_mode(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st

        call init_bindings()
        call window_divider_mode(st)
        call check(error, st%ok() .and. minor_modes%window_divider_mode%enabled(), "on")
        if (allocated(error)) return
        ! Right only, by default.
        call check(error, selected_frame%parameters%right_divider_width == 6 .and. &
                   selected_frame%parameters%bottom_divider_width == 0 .and. &
                   default_frame%right_divider_width == 6 .and. default_frame%bottom_divider_width == 0, "right only")
        if (allocated(error)) return
        call window_divider_mode(st)
        call check(error,.not. minor_modes%window_divider_mode%enabled() .and. &
                   selected_frame%parameters%right_divider_width == 0 .and. default_frame%right_divider_width == 0, &
                   "off")
        if (allocated(error)) return
        window_divider_default_places = WINDOW_DIVIDER_DEFAULT_PLACES_BOTTOM_AND_RIGHT
        window_divider_default_bottom_width = 3
        call minor_modes%window_divider_mode%enable(st)
        call check(error, selected_frame%parameters%right_divider_width == 6 .and. &
                   selected_frame%parameters%bottom_divider_width == 3 .and. default_frame%bottom_divider_width == 3, &
                   "both")
        if (allocated(error)) return
        call minor_modes%window_divider_mode%disable(st)
        window_divider_default_places = WINDOW_DIVIDER_DEFAULT_PLACES_BOTTOM_ONLY
        window_divider_default_right_width = -2
        call minor_modes%window_divider_mode%enable(st)
        call check(error, selected_frame%parameters%right_divider_width == 0 .and. &
                   selected_frame%parameters%bottom_divider_width == 3, "bottom only")
        if (allocated(error)) return
        call minor_modes%window_divider_mode%disable(st)
        window_divider_default_places = WINDOW_DIVIDER_DEFAULT_PLACES_RIGHT_ONLY
        call minor_modes%window_divider_mode%enable(st)
        ! A width that is not valid is stored in the frame, not for new
        ! frames.
        call check(error, selected_frame%parameters%right_divider_width == -2 .and. &
                   default_frame%right_divider_width == 0, "not valid")
        if (allocated(error)) return
        call check(error, is_window_divider_width_valid(6) .and. .not. is_window_divider_width_valid(0) .and. &
                   .not. is_window_divider_width_valid(-2), "is_window_divider_width_valid")
        call minor_modes%window_divider_mode%disable(st)
        window_divider_default_right_width = 6
        window_divider_default_bottom_width = 6
    end subroutine test_window_divider_mode

    !> A terminal frame draws no dividers: the widths are 0, the mode on
    !> or off.
    subroutine test_divider_widths(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type), allocatable :: ws(:)
        type(handle_type) :: new
        type(status_type) :: st
        integer :: k

        call init_bindings()
        call delete_window_tree(selected_frame%root_window)
        selected_frame%root_window = null_handle()
        call ensure_frame_windows()
        call resize_window_tree(selected_frame%root_window, 0, 0, 23, 80, 80)
        call split_window(selected_frame%selected_window, 0, .false., .true., new, st)
        call minor_modes%window_divider_mode%enable(st)
        call check(error, frame_right_divider_width(selected_frame) == 0 .and. &
                   frame_bottom_divider_width(selected_frame) == 0, "frame")
        if (allocated(error)) return
        call window_leaves(selected_frame%root_window, ws)

        do k = 1, size(ws)
            call check(error, window_right_divider_width(windows(ws(k)%index)) == 0 .and. &
                       window_bottom_divider_width(windows(ws(k)%index)) == 0, "window")
            if (allocated(error)) return
        end do

        ! With widths (as a window-system frame would draw them): none on
        ! the right edge, below a window above the minibuffer window.
        selected_frame%right_divider_width = 4
        selected_frame%bottom_divider_width = 2
        call check(error, window_right_divider_width(windows(ws(1)%index)) == 4 .and. &
                   window_right_divider_width(windows(ws(2)%index)) == 0 .and. &
                   window_bottom_divider_width(windows(ws(1)%index)) == 2 .and. &
                   window_bottom_divider_width(windows(selected_frame%minibuffer_window%index)) == 0, "macros")
        selected_frame%right_divider_width = 0
        selected_frame%bottom_divider_width = 0
        call minor_modes%window_divider_mode%disable(st)
        call delete_window_tree(selected_frame%root_window)
        selected_frame%root_window = null_handle()
    end subroutine test_divider_widths
end module test_frame_mod
