! test_support_mod.f90 -- helpers shared by the test suites.
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

!> Helpers shared by the test suites.
module test_support_mod
    use error_mod, only: status_type
    use buffer_mod, only: current_buffer, set_buffer
    use handle_mod, only: handle_type
    use frame_mod, only: selected_frame, ensure_frame_windows
    use window_mod, only: windows, delete_other_windows, resize_window_tree, set_window_buffer
    implicit none(type, external)
    private

    public :: show_alone

contains

    !> Show the current buffer in the only window of the selected frame (a
    !> 24-line terminal), as the command loop expects: it makes the selected
    !> window's buffer current.
    subroutine show_alone()
        type(status_type) :: st
        type(handle_type) :: buffer

        ! Deleting the other windows makes the selected window's buffer
        ! current: keep this one.
        buffer = current_buffer()
        call ensure_frame_windows()
        selected_frame%minibuffer_selected = .false.
        call delete_other_windows(selected_frame%selected_window, st)

        if (windows(selected_frame%root_window%index)%total_lines == 0) then
            call resize_window_tree(selected_frame%root_window, 0, 0, 23, 80, 80)
        end if

        call set_window_buffer(selected_frame%selected_window, buffer, st)
        call set_buffer(buffer, st)
    end subroutine show_alone
end module test_support_mod
