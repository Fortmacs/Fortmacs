! cc_align_fm.f90 -- custom indentation functions for CC Mode.
! Ported from GNU Emacs 31.1 lisp/progmodes/cc-align.el.
!
! Copyright (C) 1985, 1987, 1992-2026 Free Software Foundation, Inc.
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

!> `c_lineup_c_comments', the line-up function of C block comments that
!> JavaScript mode uses for its comments.
!>
!> Deferred: CC Mode itself (the syntactic analysis that gives a line its
!> syntactic elements, `c_offsets') and the other line-up
!> functions of cc-align.el.
module cc_align_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use error_fm, only: status_type
    use editfns_fm, only: point, goto_char
    use cmds_fm, only: forward_line
    use indent_fm, only: current_column
    use search_fm, only: looking_at, match_beginning, match_end, regexp_quote
    use syntax_fm, only: skip_chars_backward
    use simple_fm, only: back_to_indentation
    use subr_fm, only: match_string
    use newcomment_fm, only: comment_start_skip
    implicit none(type, external)
    private

    public :: c_lineup_c_comments, c_lineup_procedure

    abstract interface
        !> A line-up function of the syntactic symbol `c' (a C comment
        !> continuation line): the column of the current line, whose comment
        !> starts at LANGELEM_POS (its anchor), when its comment line prefix
        !> is matched by COMMENT_PREFIX (c-current-comment-prefix).
        integer function c_lineup_procedure(langelem_pos, comment_prefix)
            import :: int64
            implicit none(type, external)
            integer(int64), intent(in) :: langelem_pos
            character(len=*), intent(in) :: comment_prefix
        end function c_lineup_procedure
    end interface

contains

    !> Line up C block comment continuation lines.
    !> Various heuristics are used to handle many of the common comment
    !> styles.  Some examples:
    !>
    !>     /*          /**         /*         /* text      /*          /**
    !>      * text      * text       text        text      ** text      ** text
    !>      */          */         */         */           */           */
    !>
    !>     /*********************************************************************
    !>      * text
    !>      ********************************************************************/
    !>
    !>     /*********************************************************************
    !>         Free form text comments:
    !>      In comments with a long delimiter line at the start, the indentation
    !>      is kept unchanged for lines that start with an empty comment line
    !>      prefix.  The delimiter line is whatever matches the
    !>      `comment_start_skip' regexp.
    !>     *********************************************************************/
    !>
    !> COMMENT_PREFIX (`c_comment_prefix_regexp' as the mode sets it) is
    !> used to recognize the comment line prefix, e.g. the `*' that usually
    !> starts every line inside a comment.  The result is the column of the
    !> current line (an absolute column, CC Mode's vector).
    !>
    !> Works with: The `c' syntactic symbol.  Port of
    !> cc-align.el:c-lineup-C-comments.
    integer function c_lineup_c_comments(langelem_pos, comment_prefix) result(column)
        integer(int64), intent(in) :: langelem_pos
        character(len=*), intent(in) :: comment_prefix
        type(status_type) :: st
        character(len=:), allocatable :: prefix_match, skip
        integer(int64) :: here, opoint, prefixlen, starterlen, a, b, shortage, end1, moved
        logical :: prefix_matched, matched

        opoint = point()
        here = point()
        skip = ""
        if (allocated(comment_start_skip%value)) skip = comment_start_skip%value
        call back_to_indentation()
        prefixlen = 0
        prefix_matched = looking_at(comment_prefix, st)
        prefix_match = ""

        if (prefix_matched) then
            prefixlen = match_end(0, st) - point()
            prefix_match = match_string(0)
        end if

        ! Get the length of the comment starter, not including the first
        ! '/'.  We check if the comment prefix matched on the current line
        ! matches the starter or if it matches comment_start_skip, and
        ! choose whichever is longest.
        a = 0
        call goto_char(langelem_pos + 1)

        if (prefix_matched) then
            if (looking_at(regexp_quote(prefix_match), st)) a = match_end(0, st) - match_beginning(0, st)
        end if

        call goto_char(langelem_pos)
        matched = looking_at(skip, st)
        end1 = match_end(1, st)

        if (end1 > 0) then
            b = end1 - point() - 1
        else
            call goto_char(match_end(0, st))
            moved = skip_chars_backward(" "//achar(9), st)
            b = point() - langelem_pos - 1
            call goto_char(langelem_pos)
        end if

        starterlen = max(a, b)
        call goto_char(here)

        if (starterlen > 10 .and. prefixlen == 0) then
            ! The comment has a long starter and the line doesn't have a
            ! nonempty comment prefix.  Treat it as free form text and
            ! don't change the indentation.
            column = int(current_column())
            call goto_char(opoint)
            return
        end if

        ! Go back to the previous non-blank line, if any.
        do
            shortage = forward_line(-1_int64)
            call back_to_indentation()
            if (.not. point() > langelem_pos) exit
            if (.not. looking_at("[ "//achar(9)//"]*$", st)) exit
        end do

        ! Is the starting line the first continuation line with content?
        if (langelem_pos >= point()) then
            if (prefixlen == 0) then
                ! No nonempty comment prefix.  Align after comment starter.
                matched = looking_at(skip, st)
                call goto_char(match_end(0, st))
                column = int(current_column())
            else if (starterlen > prefixlen) then
                ! How long is the comment starter?  if greater than the
                ! length of the comment prefix, align left.  if less than
                ! or equal, align right.  this should also pick up Javadoc
                ! style comments.
                call goto_char(langelem_pos)
                column = int(current_column()) + 1
            else
                call goto_char(langelem_pos + starterlen + 1)
                column = int(current_column() - prefixlen)
            end if
        else
            ! We didn't start on the first non-blank continuation line.  If
            ! the previous line has a nonempty comment prefix, align with
            ! it.  Otherwise, align with the previous nonempty line, but
            ! align the comment ender with the starter.
            matched = looking_at(comment_prefix, st)
            if (matched) matched = match_beginning(0, st) /= match_end(0, st)

            if (.not. matched) then
                call goto_char(here)
                call back_to_indentation()

                if (looking_at("\("//comment_prefix//"\)\*/", st)) then
                    call goto_char(langelem_pos)
                else
                    do
                        shortage = forward_line(-1_int64)
                        if (shortage /= 0) exit
                        if (.not. looking_at("^[ "//achar(9)//"]*$", st)) exit
                    end do

                    call back_to_indentation()
                    ! Align with the comment starter rather than with the
                    ! code before it.
                    if (point() < langelem_pos) call goto_char(langelem_pos)
                end if
            end if

            column = int(current_column())
        end if

        call goto_char(opoint)
    end function c_lineup_c_comments
end module cc_align_fm
