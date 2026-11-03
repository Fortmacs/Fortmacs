! ediff_help_mod.f90 -- the help messages of the Ediff control panel.
! Ported from GNU Emacs 31.1 lisp/vc/ediff-help.el (in part: no mouse
! clicks on the help text, no `ediff-customize').
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

!> The brief and the long help message of the Ediff control buffer
!> (? toggles between them), composed for the kind of job, centered in
!> the control window, with each command of the long message marked by
!> an overlay.
module ediff_help_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use error_mod, only: status_type
    use handle_mod, only: handle_type
    use value_mod, only: value_type
    use buffer_mod, only: buffers, current
    use editfns_mod, only: point, goto_char, point_min, point_max, buffer_substring
    use insdel_mod, only: insert
    use cmds_mod, only: forward_line, end_of_line, beginning_of_line
    use indent_mod, only: current_column
    use search_mod, only: re_search_forward, match_beginning, match_end, SEARCH_NOERROR
    use itree_mod, only: make_overlay, overlay_put
    use window_mod, only: window_body_width
    use frame_mod, only: selected_frame
    use ediff_init_mod, only: ediff_sessions, ediff_display_help_hook
    implicit none(type, external)
    private

    public :: set_ediff_help_message, ediff_help_message_line_length, indent_ediff_help_message, set_ediff_help_overlays

    character, parameter :: LF = achar(10)

    ! The face of the help text under the mouse.
    character(len=*), parameter :: HIGHLIGHT_FACE = "highlight"

    !> The head of the full help message.  Port of ediff-help.el
    !> ediff-long-help-message-head.
    character(len=*), parameter, public :: EDIFF_LONG_HELP_MESSAGE_HEAD = &
                                           "    Move around      |      Toggle features      |        Manipulate"//LF// &
                                           "=====================|===========================|============================="

    !> The tail of the full-help message.  Port of ediff-help.el
    !> ediff-long-help-message-tail.
    character(len=*), parameter, public :: EDIFF_LONG_HELP_MESSAGE_TAIL = &
                                           "=====================|===========================|============================="//LF// &
                                           "    R -show registry |     = -compare regions    |  M   -show session group"//LF// &
                                           "    D -diff output   |     E -browse Ediff manual|  G   -send bug report"//LF// &
                                           "    i -status info   |     ? -help off           |  z/q -suspend/quit"//LF// &
                                           "-------------------------------------------------------------------------------"//LF// &
                                           "For help on a specific command:  Click Button 2 over it; or"//LF// &
                                           "              "//achar(9)//achar(9)//achar(9)//" Put the cursor over it and type RET."

    ! The help tables keep upstream's words and columns (buf).
    ! fortran-names: off
    !> Help message usually used for 3-way comparison.
    !> Normally, not a user option.  See `ediff_help_message' for details.
    !> Port of ediff-help.el ediff-long-help-message-compare3.
    character(len=*), parameter, public :: EDIFF_LONG_HELP_MESSAGE_COMPARE3 = LF// &
                                           "p,DEL -previous diff |     | -vert/horiz split   | xy -copy buf X's region to Y"//LF// &
                                           "n,SPC -next diff     |     h -highlighting       | rx -restore buf X's old diff"//LF// &
                                           "    j -jump to diff  |     @ -auto-refinement    |  * -refine current region"//LF// &
                                           "   gx -goto X's point|    ## -ignore whitespace  |  ! -update diff regions"//LF// &
                                           "  C-l -recenter      |    #c -ignore case        |"//LF// &
                                           "  v/V -scroll up/dn  | #f/#h -focus/hide regions | wx -save buf X"//LF// &
                                           "  </> -scroll lt/rt  |     X -read-only in buf X | wd -save diff output"//LF// &
                                           "    ~ -rotate buffers|     m -wide display       |"//LF

    !> Help message usually used for 2-way comparison.
    !> Normally, not a user option.  See `ediff_help_message' for details.
    !> Port of ediff-help.el ediff-long-help-message-compare2.
    character(len=*), parameter, public :: EDIFF_LONG_HELP_MESSAGE_COMPARE2 = LF// &
                                           "p,DEL -previous diff |     | -vert/horiz split   |a/b -copy A/B's region to B/A"//LF// &
                                           "n,SPC -next diff     |     h -highlighting       | rx -restore buf X's old diff"//LF// &
                                           "    j -jump to diff  |     @ -auto-refinement    |  * -refine current region"//LF// &
                                           "   gx -goto X's point|    ## -ignore whitespace  |  ! -update diff regions"//LF// &
                                           "  C-l -recenter      |    #c -ignore case        |"//LF// &
                                           "  v/V -scroll up/dn  | #f/#h -focus/hide regions | wx -save buf X"//LF// &
                                           "  </> -scroll lt/rt  |     X -read-only in buf X | wd -save diff output"//LF// &
                                           "    ~ -swap variants |     m -wide display       |"//LF
    ! fortran-names: on

    !> Contents of the brief help message.  Port of ediff-help.el
    !> ediff-brief-message-string.
    character(len=*), parameter, public :: EDIFF_BRIEF_MESSAGE_STRING = " Type ? for help"

    !> If true, Ediff displays a long help message.  Short help message
    !> otherwise.  Port of ediff-help.el ediff-use-long-help-message (each
    !> session takes its own value when it starts).
    logical, public :: ediff_use_long_help_message = .false.

contains

    !> Mark each command of the help message in the current buffer (the
    !> control buffer) with an overlay whose `ediff_help_info' property
    !> is the command's key, highlighted under the mouse.  Port of
    !> ediff-help.el:ediff-set-help-overlays.
    subroutine set_ediff_help_overlays()
        integer(int64) :: found, beginning, end
        type(handle_type) :: overlay
        type(status_type) :: st
        character(len=:), allocatable :: cmd

        call goto_char(point_min())

        do
            call re_search_forward(" *\([^ "//achar(9)//LF//"|]+\||\) +-[^|"//LF//"]+", found, st, &
                                   noerror=SEARCH_NOERROR)
            if (.not. st%ok() .or. found == 0) exit
            beginning = match_beginning(1, st)
            end = match_end(0, st)
            call buffer_substring(beginning, match_end(1, st), cmd, st)
            call make_overlay(overlay, beginning, end, st)
            if (.not. st%ok()) exit
            call overlay_put(overlay, "mouse_face", value_type(name=HIGHLIGHT_FACE), st)
            call overlay_put(overlay, "ediff_help_info", value_type(text=cmd), st)
        end do
    end subroutine set_ediff_help_overlays

    !> The length of the first line of the help message in the current
    !> buffer (the second one with the long message).  Port of
    !> ediff-help.el:ediff-help-message-line-length.
    integer function ediff_help_message_line_length(k) result(n)
        integer, intent(in) :: k
        integer(int64) :: opoint, shortage

        opoint = point()
        call goto_char(point_min())
        if (ediff_sessions(k)%use_long_help_message) shortage = forward_line(1_int64)
        call end_of_line(1_int64)
        n = int(current_column())
        call goto_char(opoint)
    end function ediff_help_message_line_length

    !> Center the help message in the control window: indent each line of
    !> the current buffer by half the room the window leaves.  Port of
    !> ediff-help.el:ediff-indent-help-message.
    subroutine indent_ediff_help_message(k)
        integer, intent(in) :: k
        integer(int64) :: opoint, shortage
        integer :: shift
        type(status_type) :: st

        shift = max(0, window_body_width(selected_frame%selected_window, st) - ediff_help_message_line_length(k))/2
        opoint = point()
        call goto_char(point_min())

        do while (point() < point_max())
            call insert(repeat(" ", shift), st)
            call beginning_of_line(1_int64)
            shortage = forward_line(1_int64)
        end do

        call goto_char(opoint)
    end subroutine indent_ediff_help_message

    !> Compose the help message of session K: the long one for its kind
    !> of job, the brief one, and the one to show
    !> (`ediff_use_long_help_message'); then run `ediff_display_help_hook'.
    !> Port of ediff-help.el:ediff-set-help-message (the user's message
    !> functions and the word-mode, narrowing and merge messages are
    !> deferred with those jobs).
    recursive subroutine set_ediff_help_message(k)
        integer, intent(in) :: k
        type(status_type) :: st

        if (ediff_sessions(k)%diff3_job) then
            ediff_sessions(k)%long_help_message = EDIFF_LONG_HELP_MESSAGE_HEAD//EDIFF_LONG_HELP_MESSAGE_COMPARE3// &
                                                  EDIFF_LONG_HELP_MESSAGE_TAIL
        else
            ediff_sessions(k)%long_help_message = EDIFF_LONG_HELP_MESSAGE_HEAD//EDIFF_LONG_HELP_MESSAGE_COMPARE2// &
                                                  EDIFF_LONG_HELP_MESSAGE_TAIL
        end if

        ! Long brief msg, not multiframe --- put in the middle.
        ediff_sessions(k)%brief_help_message = EDIFF_BRIEF_MESSAGE_STRING

        if (ediff_sessions(k)%use_long_help_message) then
            ediff_sessions(k)%help_message = ediff_sessions(k)%long_help_message
        else
            ediff_sessions(k)%help_message = ediff_sessions(k)%brief_help_message
        end if

        call ediff_display_help_hook%execute(st)
    end subroutine set_ediff_help_message
end module ediff_help_mod
