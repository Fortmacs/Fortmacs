! frame_cmds_mod.f90 -- Window Divider mode.
! Ported from GNU Emacs 31.1 lisp/frame.el (window-divider-mode,
! window-divider-mode-apply).
!
! Copyright (C) 1993-2026 Free Software Foundation, Inc.
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

!> Part of frame_mod: frame.el's commands.  Window Divider mode is a
!> minor mode (easy_mmode_mod, which depends on frame_mod, hence the
!> submodule).  It sets the divider widths of every frame and of new ones
!> (`default_frame'); a terminal frame stores them as parameters but draws
!> no dividers, as upstream on a text terminal.  What is not ported yet is
!> in PORTING.md's frame_mod row.
submodule(frame_mod) frame_cmds_mod
    use commands_mod, only: commands
    use modes_mod, only: minor_modes
    implicit none(type, external)

contains

    recursive module subroutine window_divider_mode(status)
        type(status_type), intent(out) :: status

        call minor_modes%window_divider_mode%command(status)
    end subroutine window_divider_mode

    !> The body of Window Divider mode.  Port of
    !> frame.el:window-divider-mode in part.
    subroutine window_divider_mode_body(enable, status)
        logical, intent(in) :: enable
        type(status_type), intent(out) :: status

        status = status_type()
        call window_divider_mode_apply(enable)
    end subroutine window_divider_mode_body

    module subroutine window_divider_mode_apply(enable)
        logical, intent(in) :: enable
        integer :: bottom, right

        bottom = 0
        right = 0

        if (enable) then
            if (window_divider_default_places == WINDOW_DIVIDER_DEFAULT_PLACES_BOTTOM_ONLY .or. &
                window_divider_default_places == WINDOW_DIVIDER_DEFAULT_PLACES_BOTTOM_AND_RIGHT) then
                bottom = window_divider_default_bottom_width
            end if

            if (window_divider_default_places == WINDOW_DIVIDER_DEFAULT_PLACES_RIGHT_ONLY .or. &
                window_divider_default_places == WINDOW_DIVIDER_DEFAULT_PLACES_BOTTOM_AND_RIGHT) then
                right = window_divider_default_right_width
            end if
        end if

        ! modify-all-frames-parameters: the frame (a terminal frame only
        ! stores them: frame.c:Fmodify_frame_parameters) and new frames,
        ! where a width of 0 is left out (the default).
        selected_frame%parameters%bottom_divider_width = bottom
        selected_frame%parameters%right_divider_width = right
        default_frame%bottom_divider_width = max(0, bottom)
        default_frame%right_divider_width = max(0, right)
    end subroutine window_divider_mode_apply

    module subroutine init_frame_commands()
        call minor_modes%window_divider_mode%define("window_divider_mode", global=.true., &
                                                    hook=window_divider_mode_hook, body=window_divider_mode_body)
        commands%window_divider_mode%execute => window_divider_mode
    end subroutine init_frame_commands
end submodule frame_cmds_mod
