! window_change_mod.f90 -- the window change hooks.
! Ported from GNU Emacs 31.1 src/window.c (in part:
! run_window_change_functions, window_change_record,
! run_window_configuration_change_hook).
!
! Copyright (C) 1985-1987, 1993-1998, 2000-2026 Free Software Foundation,
! Inc.
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

!> The window change hooks: redisplay calls
!> `execute_window_change_functions', which runs
!> `window_configuration_change_hook' when a window of the frame changed
!> its buffer or size, or one was deleted, since the last time, and then
!> records the windows' state.  One frame: its change stamp and number of
!> windows (struct frame change_stamp and number_of_windows) are kept
!> here.  A change is found by comparing the windows with what was
!> recorded, so the frame's window change flag (FRAME_WINDOW_CHANGE) is
!> not needed.  Deferred: see docs/status/window_mod.md.
submodule(window_mod) window_change_mod
    use buffer_mod, only: current_buffer, set_buffer, is_buffer_live
    use frame_mod, only: selected_frame
    implicit none(type, external)

    !> The frame's change stamp and its number of live windows when the
    !> state was last recorded (frame.h change_stamp, number_of_windows).
    integer :: frame_change_stamp = 0, frame_number_of_windows = 0

contains

    recursive module subroutine execute_window_change_functions(status)
        type(status_type), intent(out) :: status
        type(handle_type), allocatable :: leaves(:)
        logical :: window_buffer_change, frame_size_change, window_deleted
        integer :: k

        status = status_type()
        if (.not. is_window_valid(selected_frame%root_window)) return
        frame_size_change = .false.
        ! Analyze windows in pre-order.
        call window_leaves(selected_frame%root_window, leaves)

        do k = 1, size(leaves)
            if (.not. is_window_live(leaves(k))) cycle

            associate (w => windows(leaves(k)%index))
                ! A "buffer change" means either the window's buffer
                ! changed or the window was not part of this frame the last
                ! time window change functions were run for it.
                window_buffer_change = .not. same_handle(w%buffer, w%old_buffer) .or. &
                                       w%change_stamp /= frame_change_stamp
                ! A "size change" means either a buffer change or that the
                ! total or body size of the window has changed.
                frame_size_change = frame_size_change .or. window_buffer_change .or. &
                                    w%total_lines /= w%old_total_lines .or. w%total_columns /= w%old_total_columns .or. &
                                    window_body_lines(w) /= w%old_body_lines .or. window_text_columns(w) /= w%old_body_columns
            end associate
        end do

        ! When the number of windows on a frame has decreased, at least
        ! one window of that frame was deleted.
        window_deleted = size(leaves) < frame_number_of_windows
        ! A frame's configuration changed when one of its windows has
        ! changed buffer or size or at least one window was deleted.
        ! (This runs any buffer local window configuration change hook as
        ! well.)
        if (frame_size_change .or. window_deleted) call execute_window_configuration_change_hook(status)
        ! Record changes for the frame (upstream on unwinding, so also
        ! after an error).
        if (frame_size_change .or. window_deleted) call window_change_record()
    end subroutine execute_window_change_functions

    !> Record the frame's selected window, bump its change stamp and record
    !> the states of all its live windows: their buffers and sizes.
    !> Port of window.c:window_change_record (with
    !> window.c:window_change_record_windows; one frame).
    subroutine window_change_record()
        type(handle_type), allocatable :: leaves(:)
        integer :: k

        ! Bump up the frame's change stamp.  If this wraps, make it 1 to
        ! avoid that a new window (whose change stamp is always set to 0)
        ! gets reported as "existing before".
        if (frame_change_stamp == huge(frame_change_stamp)) then
            frame_change_stamp = 1
        else
            frame_change_stamp = frame_change_stamp + 1
        end if

        call window_leaves(selected_frame%root_window, leaves)

        do k = 1, size(leaves)
            associate (w => windows(leaves(k)%index))
                w%change_stamp = frame_change_stamp
                w%old_buffer = w%buffer
                w%old_total_lines = w%total_lines
                w%old_total_columns = w%total_columns
                w%old_body_lines = window_body_lines(w)
                w%old_body_columns = window_text_columns(w)
            end associate
        end do

        frame_number_of_windows = size(leaves)
    end subroutine window_change_record

    recursive module subroutine execute_window_configuration_change_hook(status)
        type(status_type), intent(out) :: status
        type(handle_type), allocatable :: leaves(:)
        type(handle_type) :: old_buffer, old_selected
        type(status_type) :: st
        integer :: k

        status = status_type()
        if (.not. is_window_live(selected_frame%selected_window)) return
        old_buffer = current_buffer()
        ! Use the right buffer.  Matters when running the local hooks.
        call set_buffer(windows(selected_frame%selected_window%index)%buffer, st)
        ! Look for buffer-local values.
        leaves = window_list(MINIBUF_NO, status=st)

        do k = 1, size(leaves)
            if (.not. is_window_live(leaves(k))) cycle
            call set_buffer(windows(leaves(k)%index)%buffer, st)

            if (window_configuration_change_hook%is_local()) then
                old_selected = selected_frame%selected_window
                call select_window(leaves(k), st, norecord=.true.)
                call window_configuration_change_hook%execute_local(status)
                if (is_window_live(old_selected)) call select_window(old_selected, st, norecord=.true.)
                if (.not. status%ok()) exit
            end if
        end do

        if (status%ok()) then
            if (is_window_live(selected_frame%selected_window)) then
                call set_buffer(windows(selected_frame%selected_window%index)%buffer, st)
            end if

            call window_configuration_change_hook%execute_default(status)
        end if

        if (is_buffer_live(old_buffer)) call set_buffer(old_buffer, st)
    end subroutine execute_window_configuration_change_hook
end submodule window_change_mod
