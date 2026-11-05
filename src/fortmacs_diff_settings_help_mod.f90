! fortmacs_diff_settings_help_mod.f90 -- the help procedures of an Ediff session,
! a submodule of fortmacs_diff_settings_mod.
! Ported from GNU Emacs 31.1 lisp/vc/ediff-help.el (in part: see
! docs/status/fortmacs_diff_help_mod.md).
!
! Copyright (C) 1996-2026 Free Software Foundation, Inc.
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
!> ediff-help.el: a submodule of the type's module, fortmacs_diff_settings_mod,
!> because they use fortmacs_diff_help_mod, which uses it.
submodule(fortmacs_diff_settings_mod) fortmacs_diff_settings_help_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use error_mod, only: status_type
    use editing_functions_mod, only: point, goto_character, point_minimum, point_maximum
    use insert_delete_mod, only: insert
    use editing_commands_mod, only: forward_line, end_of_line, beginning_of_line
    use indent_mod, only: current_column
    use window_mod, only: window_body_width
    use frame_mod, only: selected_frame
    use fortmacs_diff_help_mod, only: FORTMACS_DIFF_BRIEF_MESSAGE_STRING, FORTMACS_DIFF_LONG_HELP_MESSAGE_COMPARE2, &
                                      FORTMACS_DIFF_LONG_HELP_MESSAGE_COMPARE3, FORTMACS_DIFF_LONG_HELP_MESSAGE_HEAD, &
                                      FORTMACS_DIFF_LONG_HELP_MESSAGE_MERGE, FORTMACS_DIFF_LONG_HELP_MESSAGE_TAIL
    implicit none(type, external)

contains

    module integer function get_help_message_line_length(self) result(n)
        class(fortmacs_diff_session_type), intent(in) :: self
        integer(int64) :: original_point, shortage

        original_point = point()
        call goto_character(point_minimum())
        if (self%use_long_help_message) shortage = forward_line(1_int64)
        call end_of_line(1_int64)
        n = int(current_column())
        call goto_character(original_point)
    end function get_help_message_line_length

    module subroutine indent_help_message(self, status)
        class(fortmacs_diff_session_type), intent(inout), target :: self
        type(status_type), intent(out) :: status
        integer(int64) :: original_point, shortage
        integer :: shift, width
        integer :: session_index

        call self%get_index(session_index, status)
        if (.not. status%ok()) return
        width = window_body_width(selected_frame%selected_window, status)
        if (.not. status%ok()) return
        shift = max(0, width - fortmacs_diff_sessions(session_index)%get_help_message_line_length())/2
        original_point = point()
        call goto_character(point_minimum())

        do while (point() < point_maximum())
            call insert(repeat(" ", shift), status)

            if (.not. status%ok()) then
                call goto_character(original_point)
                return
            end if

            call beginning_of_line(1_int64)
            shortage = forward_line(1_int64)
        end do

        call goto_character(original_point)
    end subroutine indent_help_message

    recursive module subroutine set_help_message(self, status)
        class(fortmacs_diff_session_type), intent(inout), target :: self
        type(status_type), intent(out) :: status
        integer :: session_index

        call self%get_index(session_index, status)
        if (.not. status%ok()) return

        if (fortmacs_diff_sessions(session_index)%merge_job) then
            fortmacs_diff_sessions(session_index)%long_help_message = FORTMACS_DIFF_LONG_HELP_MESSAGE_HEAD &
                                                                      //FORTMACS_DIFF_LONG_HELP_MESSAGE_MERGE// &
                                                                      FORTMACS_DIFF_LONG_HELP_MESSAGE_TAIL
        else if (fortmacs_diff_sessions(session_index)%diff3_job) then
            fortmacs_diff_sessions(session_index)%long_help_message = FORTMACS_DIFF_LONG_HELP_MESSAGE_HEAD &
                                                                      //FORTMACS_DIFF_LONG_HELP_MESSAGE_COMPARE3// &
                                                                      FORTMACS_DIFF_LONG_HELP_MESSAGE_TAIL
        else
            fortmacs_diff_sessions(session_index)%long_help_message = FORTMACS_DIFF_LONG_HELP_MESSAGE_HEAD &
                                                                      //FORTMACS_DIFF_LONG_HELP_MESSAGE_COMPARE2// &
                                                                      FORTMACS_DIFF_LONG_HELP_MESSAGE_TAIL
        end if

        ! Long brief msg, not multiframe --- put in the middle.
        fortmacs_diff_sessions(session_index)%brief_help_message = FORTMACS_DIFF_BRIEF_MESSAGE_STRING

        if (fortmacs_diff_sessions(session_index)%use_long_help_message) then
            fortmacs_diff_sessions(session_index)%help_message = fortmacs_diff_sessions(session_index)%long_help_message
        else
            fortmacs_diff_sessions(session_index)%help_message = fortmacs_diff_sessions(session_index)%brief_help_message
        end if

        call fortmacs_diff_display_help_hook%execute(status)
    end subroutine set_help_message
end submodule fortmacs_diff_settings_help_mod
