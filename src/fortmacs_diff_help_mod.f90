! fortmacs_diff_help_mod.f90 -- the help messages of the Ediff control panel.
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
!> an overlay.  The procedures that act on one session are type-bound procedures of
!> `fortmacs_diff_session_type', in fortmacs_diff_settings_help_mod.
module fortmacs_diff_help_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use error_mod, only: status_type
    use handle_mod, only: handle_type
    use value_mod, only: value_type
    use buffer_mod, only: buffers, current
    use editing_functions_mod, only: point, goto_character, point_minimum
    use basic_subprograms_mod, only: match_string
    use search_mod, only: regular_expression_search_forward, match_beginning, match_end, SEARCH_NOERROR
    use interval_tree_mod, only: make_overlay, overlay_put
    implicit none(type, external)
    private

    public :: set_fortmacs_diff_help_overlays

    character, parameter :: LF = achar(10)

    ! The face of the help text under the mouse.
    character(len=*), parameter :: HIGHLIGHT_FACE = "highlight"

    !> The head of the full help message.  Port of ediff-help.el
    !> ediff-long-help-message-head.
    character(len=*), parameter, public :: FORTMACS_DIFF_LONG_HELP_MESSAGE_HEAD = &
                                           "    Move around      |      Toggle features      |        Manipulate"//LF// &
                                           "=====================|===========================|============================="

    !> The tail of the full-help message.  Port of ediff-help.el
    !> ediff-long-help-message-tail.
    character(len=*), parameter, public :: FORTMACS_DIFF_LONG_HELP_MESSAGE_TAIL = &
                                           "=====================|===========================|============================="//LF// &
                                           "    R -show registry |     = -compare regions    |  M   -show session group"//LF// &
                                           "    D -diff output   |     E -browse Ediff manual|  G   -send bug report"//LF// &
                                           "    i -status info   |     ? -help off           |  z/q -suspend/quit"//LF// &
                                           "-------------------------------------------------------------------------------"//LF// &
                                           "For help on a specific command:  Click Button 2 over it; or"//LF// &
                                           "              "//achar(9)//achar(9)//achar(9)//" Put the cursor over it and type RET."

    ! The help tables keep upstream's words and columns.
    ! fortran-names: off
    !> Help message usually used for 3-way comparison.
    !> Normally, not a user option.  See `ediff_help_message' for details.
    !> Port of ediff-help.el ediff-long-help-message-compare3.
    character(len=*), parameter, public :: FORTMACS_DIFF_LONG_HELP_MESSAGE_COMPARE3 = LF// &
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
    character(len=*), parameter, public :: FORTMACS_DIFF_LONG_HELP_MESSAGE_COMPARE2 = LF// &
                                           "p,DEL -previous diff |     | -vert/horiz split   |a/b -copy A/B's region to B/A"//LF// &
                                           "n,SPC -next diff     |     h -highlighting       | rx -restore buf X's old diff"//LF// &
                                           "    j -jump to diff  |     @ -auto-refinement    |  * -refine current region"//LF// &
                                           "   gx -goto X's point|    ## -ignore whitespace  |  ! -update diff regions"//LF// &
                                           "  C-l -recenter      |    #c -ignore case        |"//LF// &
                                           "  v/V -scroll up/dn  | #f/#h -focus/hide regions | wx -save buf X"//LF// &
                                           "  </> -scroll lt/rt  |     X -read-only in buf X | wd -save diff output"//LF// &
                                           "    ~ -swap variants |     m -wide display       |"//LF

    !> Help message for merge sessions.
    !> Normally, not a user option.  See `ediff_help_message' for details.
    !> Port of ediff-help.el ediff-long-help-message-merge.
    character(len=*), parameter, public :: FORTMACS_DIFF_LONG_HELP_MESSAGE_MERGE = LF// &
                                           "p,DEL -previous diff |     | -vert/horiz split   |  x -copy buf X's region to C"//LF// &
                                           "n,SPC -next diff     |     h -highlighting       |  r -restore buf C's old diff"//LF// &
                                           "    j -jump to diff  |     @ -auto-refinement    |  * -refine current region"//LF// &
                                           "   gx -goto X's point|    ## -ignore whitespace  |  ! -update diff regions"//LF// &
                                           "  C-l -recenter      | #f/#h -focus/hide regions |  + -combine diff regions"//LF// &
                                           "  v/V -scroll up/dn  |     X -read-only in buf X | wx -save buf X"//LF// &
                                           "  </> -scroll lt/rt  |     m -wide display       | wd -save diff output"//LF// &
                                           "    ~ -swap variants |     s -shrink window C    |  / -show/hide ancestor buff"//LF// &
                                           "                     |  $$ -show clashes only    |  & -merge w/new default"//LF// &
                                           "                     |  $* -skip changed regions |"//LF
    ! fortran-names: on

    !> Contents of the brief help message.  Port of ediff-help.el
    !> ediff-brief-message-string.
    character(len=*), parameter, public :: FORTMACS_DIFF_BRIEF_MESSAGE_STRING = " Type ? for help"

    !> If true, Ediff displays a long help message.  Short help message
    !> otherwise.  Port of ediff-help.el ediff-use-long-help-message (each
    !> session takes its own value when it starts).
    logical, public :: fortmacs_diff_use_long_help_message = .false.

contains

    !> Mark each command of the help message in the current buffer (the
    !> control buffer) with an overlay whose `fortmacs_diff_help_info' property
    !> is the command's key, highlighted under the mouse.  Port of
    !> ediff-help.el:ediff-set-help-overlays.
    subroutine set_fortmacs_diff_help_overlays(status)
        type(status_type), intent(out) :: status
        integer(int64) :: found, beginning, end
        type(handle_type) :: overlay
        character(len=:), allocatable :: command

        call goto_character(point_minimum())

        do
            call regular_expression_search_forward(" *\([^ "//achar(9)//LF//"|]+\||\) +-[^|"//LF//"]+", found, status, &
                                                   noerror=SEARCH_NOERROR)
            if (.not. status%ok()) return
            if (found == 0) exit
            beginning = match_beginning(1, status)
            if (.not. status%ok()) return
            end = match_end(0, status)
            if (.not. status%ok()) return
            command = match_string(1)
            call make_overlay(overlay, beginning, end, status)
            if (.not. status%ok()) return
            call overlay_put(overlay, "mouse_face", value_type(name=HIGHLIGHT_FACE), status)
            if (.not. status%ok()) return
            call overlay_put(overlay, "fortmacs_diff_help_info", value_type(text=command), status)
            if (.not. status%ok()) return
        end do
    end subroutine set_fortmacs_diff_help_overlays
end module fortmacs_diff_help_mod
