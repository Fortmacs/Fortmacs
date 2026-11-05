! test_frame_mod.f90 -- tests for several frames on one terminal
! (frame_mod, frame_commands_mod) and frame.el's Window Divider mode.
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

!> Own tests: upstream's test/src/frame-tests.el tests frame parameters
!> and child frames on window systems, which are not ported.  Each test
!> starts and ends with the initial frame alone, 24 lines by 80 columns.
!> Upstream has no ERT tests for window dividers either: Window Divider
!> mode sets the frame's divider parameters and those of new frames, by
!> `window_divider_default_places'; a terminal frame draws no dividers,
!> so the divider widths stay 0.
module test_frame_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_mod, only: status_type
    use handle_mod, only: handle_type, null_handle
    use modes_mod, only: minor_modes
    use bindings_mod, only: init_bindings
    use buffer_mod, only: get_or_make_buffer, set_buffer, close_buffer, current_buffer, is_buffer_live, record_buffer, &
                          frame_seen_buffers
    use frame_mod, only: frame_type, selected_frame, ensure_frame_windows, make_frame, select_frame, delete_frame, &
                         delete_other_frames, frame_list, next_frame, previous_frame, is_frame_live, get_frame, &
                         other_frame, set_frame_name, select_frame_by_name, find_frame, window_frame, default_frame, &
                         window_divider_mode, window_divider_default_places, window_divider_default_bottom_width, &
                         window_divider_default_right_width, WINDOW_DIVIDER_DEFAULT_PLACES_BOTTOM_ONLY, &
                         WINDOW_DIVIDER_DEFAULT_PLACES_RIGHT_ONLY, WINDOW_DIVIDER_DEFAULT_PLACES_BOTTOM_AND_RIGHT, &
                         frame_right_divider_width, frame_bottom_divider_width, window_right_divider_width, &
                         window_bottom_divider_width, is_window_divider_width_valid
    use window_mod, only: windows, resize_window_tree, set_window_buffer, same_handle, is_window_live, &
                          switch_to_buffer_other_frame, display_buffer_other_frame, select_window, init_window_display, &
                          window_leaves, delete_window_tree, split_window
    implicit none(type, external)
    private

    public :: collect_frame_mod

contains

    !> Collect all exported unit tests.
    subroutine collect_frame_mod(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("make-frame", test_make_frame), &
                    new_unittest("select-frame", test_select_frame), &
                    new_unittest("next-frame", test_next_frame), &
                    new_unittest("delete-frame", test_delete_frame), &
                    new_unittest("frame-name", test_frame_name), &
                    new_unittest("switch-to-buffer-other-frame", test_other_frame_buffer), &
                    new_unittest("close-buffer-other-frame", test_close_buffer), &
                    new_unittest("select-window-other-frame", test_select_window), &
                    new_unittest("window-divider-mode", test_window_divider_mode), &
                    new_unittest("window-divider-widths", test_divider_widths) &
                    ]
    end subroutine collect_frame_mod

    !> The initial frame alone, 24 by 80, showing buffer "frame-test".
    subroutine enter(h)
        type(handle_type), intent(out) :: h
        type(status_type) :: st

        call ensure_frame_windows()
        call delete_other_frames(st)
        ! close_buffer replaces the buffer in windows through its hook.
        call init_window_display()
        selected_frame%tty%wcm%cm_rows = 24
        selected_frame%tty%wcm%cm_cols = 80
        call resize_window_tree(selected_frame%root_window, 0, 0, 23, 80, 80)
        call resize_window_tree(selected_frame%minibuffer_window, 23, 0, 1, 80, 80)
        call get_or_make_buffer("frame-test", h, st)
        call set_buffer(h, st)
        call set_window_buffer(selected_frame%selected_window, h, st)
    end subroutine enter

    subroutine leave(h)
        type(handle_type), intent(in) :: h
        type(status_type) :: st

        call delete_other_frames(st)
        call close_buffer(h, st)
    end subroutine leave

    subroutine test_make_frame(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, first, second
        type(frame_type), pointer :: f
        type(handle_type), allocatable :: frames(:)
        type(status_type) :: st

        call enter(h)
        first = selected_frame%handle
        call make_frame(second, st)
        call check(error, st%ok() .and. is_frame_live(second), "made")
        if (allocated(error)) return
        ! The new frame shows the current buffer and is not selected.
        f => get_frame(second)
        call check(error, same_handle(windows(f%root_window%index)%buffer, h) .and. &
                   same_handle(selected_frame%handle, first), "shows the buffer, not selected")
        if (allocated(error)) return
        call check(error, f%name(1:1) == "F" .and. f%name /= selected_frame%name, "named F<num>")
        if (allocated(error)) return
        ! Newest first.
        frames = frame_list()
        call check(error, size(frames) == 2, "two frames")
        if (allocated(error)) return
        call check(error, same_handle(frames(1), second) .and. same_handle(frames(2), first), "newest first")
        if (allocated(error)) return
        ! Its windows are its own and as large as the terminal.
        call check(error, same_handle(window_frame(f%root_window), second) .and. &
                   windows(f%root_window%index)%total_lines == 23 .and. &
                   windows(f%minibuffer_window%index)%top_line == 23, "its windows")
        call leave(h)
    end subroutine test_make_frame

    subroutine test_select_frame(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, other, first, second
        type(handle_type), allocatable :: seen(:)
        type(status_type) :: st

        call enter(h)
        first = selected_frame%handle
        call make_frame(second, st)
        call select_frame(second, st)
        call check(error, st%ok() .and. same_handle(selected_frame%handle, second), "selected")
        if (allocated(error)) return
        call check(error, selected_frame%needs_redraw .and. selected_frame%tty%wcm%cm_rows == 24, "shown")
        if (allocated(error)) return
        ! Each frame has its own buffer list.
        call get_or_make_buffer("frame-test-other", other, st)
        call set_window_buffer(selected_frame%selected_window, other, st)
        call record_buffer(other)
        seen = frame_seen_buffers()
        call check(error, any(seen%index == other%index), "recorded here")
        if (allocated(error)) return
        call select_frame(first, st)
        seen = frame_seen_buffers()
        call check(error, same_handle(selected_frame%handle, first) .and. .not. any(seen%index == other%index), &
                   "not recorded there")
        if (allocated(error)) return
        call check(error, same_handle(current_buffer(), h), "the frame's window's buffer")
        call close_buffer(other, st)
        call leave(h)
    end subroutine test_select_frame

    subroutine test_next_frame(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, f1, f2, f3
        type(status_type) :: st

        call enter(h)
        f1 = selected_frame%handle
        call make_frame(f2, st)
        call make_frame(f3, st)
        ! The frame list is (F3 F2 F1): after F1 comes F3.
        call check(error, same_handle(next_frame(), f3) .and. same_handle(next_frame(f3), f2) .and. &
                   same_handle(next_frame(f2), f1), "next")
        if (allocated(error)) return
        call check(error, same_handle(previous_frame(), f2) .and. same_handle(previous_frame(f3), f1), "previous")
        if (allocated(error)) return
        call other_frame(1_int64, st)
        call check(error, same_handle(selected_frame%handle, f3), "other-frame 1")
        if (allocated(error)) return
        call other_frame(-2_int64, st)
        call check(error, same_handle(selected_frame%handle, f2), "other-frame -2")
        if (allocated(error)) return
        call select_frame(f1, st)
        call leave(h)
    end subroutine test_next_frame

    subroutine test_delete_frame(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, f1, f2, f3
        type(status_type) :: st

        call enter(h)
        f1 = selected_frame%handle
        call delete_frame(st)
        call check(error, st%detail == "Attempt to delete the sole visible or iconified frame", "sole frame")
        if (allocated(error)) return
        call make_frame(f2, st)
        call make_frame(f3, st)
        ! Deleting the selected frame selects the most recently used one.
        call select_frame(f2, st)
        call select_frame(f3, st)
        call delete_frame(st)
        call check(error, st%ok() .and. .not. is_frame_live(f3) .and. same_handle(selected_frame%handle, f2), &
                   "mru selected")
        if (allocated(error)) return
        ! Another frame.
        call delete_frame(st, f1)
        call check(error, st%ok() .and. .not. is_frame_live(f1) .and. size(frame_list()) == 1, "other deleted")
        if (allocated(error)) return
        call check(error, is_window_live(selected_frame%selected_window), "windows kept")
        call leave(h)
    end subroutine test_delete_frame

    subroutine test_frame_name(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, f1, f2
        type(frame_type), pointer :: f
        type(status_type) :: st

        call enter(h)
        f1 = selected_frame%handle
        call make_frame(f2, st)
        f => get_frame(f2)
        call set_frame_name(f%name, st)
        call check(error, st%detail == "Frame of the form F<num> named `"//f%name//"' already exists", "F<num> taken")
        if (allocated(error)) return
        call set_frame_name("editing", st)
        call check(error, st%ok() .and. selected_frame%name == "editing" .and. selected_frame%explicit_name)
        if (allocated(error)) return
        call check(error, same_handle(find_frame("editing"), f1), "found")
        if (allocated(error)) return
        call select_frame_by_name(f%name, st)
        call check(error, st%ok() .and. same_handle(selected_frame%handle, f2), "by name")
        if (allocated(error)) return
        call select_frame_by_name("nowhere", st)
        call check(error, st%detail == "There is no frame named `nowhere'", "no frame")
        if (allocated(error)) return
        call select_frame(f1, st)
        call leave(h)
    end subroutine test_frame_name

    subroutine test_other_frame_buffer(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, other, f1, window
        type(status_type) :: st

        call enter(h)
        f1 = selected_frame%handle
        call get_or_make_buffer("frame-test-other", other, st)
        ! A new frame, selected, shows the buffer.
        call switch_to_buffer_other_frame(other, st)
        call check(error, st%ok() .and. size(frame_list()) == 2 .and. .not. same_handle(selected_frame%handle, f1) &
                   .and. same_handle(windows(selected_frame%selected_window%index)%buffer, other), "popped up")
        if (allocated(error)) return
        ! Back on F1, the window on the other frame is reused.
        call select_frame(f1, st)
        call switch_to_buffer_other_frame(other, st)
        call check(error, st%ok() .and. size(frame_list()) == 2 .and. .not. same_handle(selected_frame%handle, f1), &
                   "reused")
        if (allocated(error)) return
        ! Displaying in another frame does not select it.
        call select_frame(f1, st)
        call display_buffer_other_frame(other, window, st)
        call check(error, st%ok() .and. size(frame_list()) == 2 .and. same_handle(selected_frame%handle, f1) .and. &
                   .not. same_handle(window_frame(window), f1), "displayed")
        if (allocated(error)) return
        call close_buffer(other, st)
        call leave(h)
    end subroutine test_other_frame_buffer

    subroutine test_close_buffer(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, other, f2
        type(frame_type), pointer :: f
        type(status_type) :: st

        call enter(h)
        call get_or_make_buffer("frame-test-other", other, st)
        call make_frame(f2, st)
        f => get_frame(f2)
        call set_window_buffer(f%root_window, other, st)
        ! Closing the buffer replaces it in the other frame's window too.
        call close_buffer(other, st)
        call check(error, st%ok() .and. .not. is_buffer_live(other) .and. is_window_live(f%root_window) .and. &
                   is_buffer_live(windows(f%root_window%index)%buffer), "replaced")
        call leave(h)
    end subroutine test_close_buffer

    subroutine test_select_window(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, f2
        type(frame_type), pointer :: f
        type(status_type) :: st

        call enter(h)
        call make_frame(f2, st)
        f => get_frame(f2)
        ! Selecting a window on another frame selects that frame.
        call select_window(f%root_window, st)
        call check(error, st%ok() .and. same_handle(selected_frame%handle, f2) .and. &
                   same_handle(selected_frame%selected_window, f%root_window))
        call leave(h)
    end subroutine test_select_window

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
