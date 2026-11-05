! fortmacs_diff_settings_window_mod.f90 -- the window procedures of an Ediff session,
! a submodule of fortmacs_diff_settings_mod.
! Ported from GNU Emacs 31.1 lisp/vc/ediff-wind.el (in part: see
! docs/status/fortmacs_diff_window_mod.md).
!
! Copyright (C) 1994-2026 Free Software Foundation, Inc.
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

!> The type-bound procedures of `fortmacs_diff_session_type' that port
!> ediff-wind.el: a submodule of the type's module, fortmacs_diff_settings_mod,
!> because they use fortmacs_diff_window_mod, which uses it.
submodule(fortmacs_diff_settings_mod) fortmacs_diff_settings_window_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use, intrinsic :: ieee_arithmetic, only: ieee_rint
    use handle_mod, only: handle_type
    use buffer_mod, only: buffers, is_buffer_live, header_line_format_type
    use window_mod, only: windows, split_window_below, split_window_right
    implicit none(type, external)

contains

    recursive module subroutine refresh_mode_lines(self)
        class(fortmacs_diff_session_type), intent(inout), target :: self
        type(handle_type) :: control

        control = self%control_buffer
        if (.not. is_buffer_live(control)) return
        ! Control buffer id.
        buffers(control%index)%mode_line_buffer_identification = self%make_wide_control_buffer_id()
    end subroutine refresh_mode_lines

    module function make_wide_control_buffer_id(self) result(id)
        class(fortmacs_diff_session_type), intent(in) :: self
        type(header_line_format_type) :: id
        character(len=64) :: position

        associate (s => self)
            if (s%current_difference < 0) then
                write (position, '("At start of ", i0, " diffs")') s%number_of_differences
            else if (s%current_difference >= s%number_of_differences) then
                write (position, '("At end of ", i0, " diffs")') s%number_of_differences
            else
                write (position, '("diff ", i0, " of ", i0)') s%current_difference + 1, s%number_of_differences
            end if
        end associate

        call id%append("%b   ")
        call id%append(trim(position), "mode_line_buffer_id")
    end function make_wide_control_buffer_id

    module function get_window_configuration(self) result(configuration)
        class(fortmacs_diff_session_type), intent(in) :: self
        character(len=:), allocatable :: configuration
        integer :: t
        character(len=240) :: windows_text

        associate (s => self)
            ! The control window, then the windows by buffer type.
            write (windows_text, '(5("#<window ", i0, ".", i0, ">"))') s%control_window%index, &
                s%control_window%generation, (s%windows(t)%index, s%windows(t)%generation, &
                                              t=FORTMACS_DIFF_BUFFER_TYPE_A, FORTMACS_DIFF_BUFFER_TYPE_ANCESTOR)
            configuration = trim(windows_text)

            if (associated(s%split_window_function, split_window_right)) then
                configuration = configuration//"split_window_right"
            else if (associated(s%split_window_function, split_window_below)) then
                configuration = configuration//"split_window_below"
            else
                configuration = configuration//"a procedure"
            end if
        end associate
    end function get_window_configuration
end submodule fortmacs_diff_settings_window_mod
