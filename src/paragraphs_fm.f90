! paragraphs_fm.f90 -- moving over, marking and killing paragraphs.
! Ported from GNU Emacs 31.1 lisp/textmodes/paragraphs.el.
!
! Copyright (C) 1985-1987, 1991, 1994-1997, 1999-2026 Free Software
! Foundation, Inc.
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

!> Paragraph motion.  Upstream finds paragraphs with the regexps
!> `paragraph-start` ("\f\\|[ \t]*$": a line starting with a form feed, or
!> a blank line) and `paragraph-separate` ("[ \t\f]*$": a line of spaces,
!> tabs and form feeds); there is no regexp engine yet, so those default
!> values are built in here as `looking_at_parstart` and
!> `looking_at_parsep`, and the searches for them as line scans.  With no
!> `fill-prefix`, `left-margin` 0 and no hard newlines, the rest follows
!> upstream step by step.
!>
!> Deferred: setting `paragraph-start` and `paragraph-separate` (they need
!> regexps), `fill-prefix`, `use-hard-newlines`, sentences (`forward-sentence`,
!> M-a, M-e, M-k: `sentence-end` is a regexp), `transpose-paragraphs`
!> (transpose-subr).
module paragraphs_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use error_fm, only: status_type, set_error, FM_GENERIC_ERROR
    use buffer_fm, only: buffers, cur, buf_fetch_char, transient_mark_mode
    use marker_fm, only: char_to_byte
    use editfns_fm, only: point, goto_char, bobp, eobp, bolp, point_min, point_max, constrain_to_field
    use cmds_fm, only: forward_line, beginning_of_line, end_of_line
    use indent_fm, only: current_column
    use simple_fm, only: kill_region, set_mark, push_mark
    use editfns_fm, only: mark_position
    use data_fm, only: value_eq
    use callint_fm, only: defcommand, current_prefix_arg, prefix_numeric_value
    use keyboard_fm, only: this_command, last_command
    implicit none(type, external)
    private

    public :: forward_paragraph, backward_paragraph, mark_paragraph, kill_paragraph, backward_kill_paragraph, &
              start_of_paragraph_text, end_of_paragraph_text, syms_of_paragraphs

    integer, parameter :: TAB = 9, NEWLINE = 10, FORMFEED = 12, SPACE = 32

contains

    !> The character at `pos`, or -1 outside the accessible region.
    integer function char_at(pos)
        integer(int64), intent(in) :: pos
        char_at = -1
        if (pos < buffers(cur)%begv .or. pos >= buffers(cur)%zv) return
        char_at = buf_fetch_char(buffers(cur), char_to_byte(pos))
    end function char_at

    !> Whether `pos` is at the end of a line (or of the accessible region).
    logical function eol_at(pos)
        integer(int64), intent(in) :: pos
        eol_at = char_at(pos) == NEWLINE .or. pos >= buffers(cur)%zv
    end function eol_at

    !> Whether `pos` starts a line (^).
    logical function bol_at(pos)
        integer(int64), intent(in) :: pos
        bol_at = pos == buffers(cur)%begv .or. char_at(pos - 1) == NEWLINE
    end function bol_at

    !> The first position from `pos` on that is not one of `chars`.
    integer(int64) function skip(pos, chars) result(p)
        integer(int64), intent(in) :: pos
        integer, intent(in) :: chars(:)
        p = pos
        do while (any(char_at(p) == chars))
            p = p + 1
        end do
    end function skip

    !> (looking-at paragraph-separate) for "[ \t\f]*$".
    logical function looking_at_parsep()
        looking_at_parsep = eol_at(skip(point(), [SPACE, TAB, FORMFEED]))
    end function looking_at_parsep

    !> (looking-at paragraph-start) for "\f\\|[ \t]*$".
    logical function looking_at_parstart()
        looking_at_parstart = char_at(point()) == FORMFEED
        if (.not. looking_at_parstart) looking_at_parstart = eol_at(skip(point(), [SPACE, TAB]))
    end function looking_at_parstart

    !> Whether the line starting at `bol` matches sp-parstart,
    !> "^[ \t]*\\(?:PARSTART\\|PARSEP\\)", and where the match ends.
    logical function sp_parstart_at(bol, mend)
        integer(int64), intent(in) :: bol
        integer(int64), intent(out) :: mend
        integer(int64) :: p
        p = skip(bol, [SPACE, TAB])
        mend = p
        if (char_at(p) == FORMFEED) then
            mend = p + 1
            sp_parstart_at = .true.
        else
            sp_parstart_at = eol_at(p)
        end if
    end function sp_parstart_at

    !> (re-search-forward sp-parstart nil 1): go to the end of the next
    !> match and return true, with `mbeg` its start; else go to the end of
    !> the accessible region and return false.
    logical function search_forward_parstart(mbeg) result(found)
        integer(int64), intent(out) :: mbeg
        integer(int64) :: b, mend
        b = point()
        if (.not. bol_at(b)) b = next_bol(b)
        found = .false.
        do while (b <= buffers(cur)%zv)
            if (.not. bol_at(b)) exit
            if (sp_parstart_at(b, mend)) then
                mbeg = b
                found = .true.
                call goto_char(mend)
                return
            end if
            if (b == buffers(cur)%zv) exit
            b = next_bol(b)
        end do
        call goto_char(buffers(cur)%zv)
    end function search_forward_parstart

    !> The start of the line after the one containing `pos` (the end of
    !> the accessible region if there is none).
    integer(int64) function next_bol(pos) result(p)
        integer(int64), intent(in) :: pos
        p = pos
        do while (p < buffers(cur)%zv)
            p = p + 1
            if (char_at(p - 1) == NEWLINE) return
        end do
    end function next_bol

    !> The start of the line containing `pos`.
    integer(int64) function this_bol(pos) result(p)
        integer(int64), intent(in) :: pos
        p = pos
        do while (p > buffers(cur)%begv)
            if (char_at(p - 1) == NEWLINE) return
            p = p - 1
        end do
    end function this_bol

    !> (re-search-backward sp-parstart nil 1): go to the start of the last
    !> match that ends at or before point and return true; else go to the
    !> start of the accessible region and return false.
    logical function search_backward_parstart() result(found)
        integer(int64) :: b, mend, limit
        limit = point()
        b = this_bol(limit)
        found = .false.
        do
            if (sp_parstart_at(b, mend)) then
                if (mend <= limit) then
                    found = .true.
                    call goto_char(b)
                    return
                end if
            end if
            if (b == buffers(cur)%begv) exit
            b = this_bol(b - 1)
        end do
        call goto_char(buffers(cur)%begv)
    end function search_backward_parstart

    !> (re-search-backward "^\n" (max (1- (point)) (point-min)) t): an
    !> empty line just before point.
    logical function search_backward_empty_line() result(found)
        integer(int64) :: p
        p = point() - 1
        found = p >= buffers(cur)%begv
        if (found) found = bol_at(p) .and. char_at(p) == NEWLINE
        if (found) call goto_char(p)
    end function search_backward_empty_line

    !> move-to-left-margin with `left-margin' 0: the start of the line.
    subroutine move_to_left_margin()
        call beginning_of_line()
    end subroutine move_to_left_margin

    !> skip-chars-backward " \t".
    subroutine skip_blanks_backward()
        do while (point() > buffers(cur)%begv)
            if (char_at(point() - 1) /= SPACE .and. char_at(point() - 1) /= TAB) exit
            call goto_char(point() - 1)
        end do
    end subroutine skip_blanks_backward

    !> Move forward to end of paragraph.
    !> With argument ARG, do it ARG times;
    !> a negative argument ARG = -N means move backward N paragraphs.
    !>
    !> A line which `paragraph-start' matches either separates paragraphs
    !> (if `paragraph-separate' matches it also) or is the first line of a paragraph.
    !> A paragraph end is the beginning of a line which is not part of the paragraph
    !> to which the end of the previous line belongs, or the end of the buffer.
    !> Returns the count of paragraphs left to move.  Port of
    !> paragraphs.el:forward-paragraph.
    integer(int64) function forward_paragraph(arg) result(left)
        integer(int64), intent(in) :: arg
        integer(int64) :: opoint, start, shortage
        logical :: found_start
        left = arg
        opoint = point()
        start = point()
        do while (left < 0 .and. .not. bobp())
            if (.not. looking_at_parsep() .and. search_backward_empty_line() .and. looking_at_parsep()) then
                left = left + 1
            else
                start = point()
                ! Move back over paragraph-separating lines.
                call goto_char(point() - 1)
                call beginning_of_line()
                do while (.not. bobp())
                    call move_to_left_margin()
                    if (.not. looking_at_parsep()) exit
                    shortage = forward_line(-1_int64)
                end do
                if (.not. bobp()) then
                    left = left + 1
                    ! Go to end of the previous (non-separating) line.
                    call end_of_line()
                    ! Search back for line that starts or separates paragraphs.
                    found_start = .false.
                    do
                        if (.not. search_backward_parstart()) exit
                        found_start = .true.
                        ! Found a candidate, but need to check if it is a
                        ! REAL parstart.
                        start = point()
                        call move_to_left_margin()
                        if (looking_at_parsep()) exit
                        if (looking_at_parstart()) exit
                        found_start = .false.
                        call goto_char(start)
                    end do
                    if (found_start) then
                        ! Move forward over paragraph separators.  We know
                        ! this cannot reach the place we started because we
                        ! know we moved back over a non-separator.
                        do while (.not. eobp())
                            call move_to_left_margin()
                            if (.not. looking_at_parsep()) exit
                            shortage = forward_line(1_int64)
                        end do
                        ! If line before paragraph is just margin, back up
                        ! to there.
                        call end_of_line(0_int64)
                        if (current_column() > 0) then
                            call goto_char(point() + 1)
                        else
                            call skip_blanks_backward()
                            if (.not. bolp()) shortage = forward_line(1_int64)
                        end if
                    else
                        ! No starter or separator line => use buffer beg.
                        call goto_char(point_min())
                    end if
                end if
            end if
        end do
        do while (left > 0 .and. .not. eobp())
            ! Move forward over separator lines...
            do while (.not. eobp())
                call move_to_left_margin()
                if (eobp()) exit
                if (.not. looking_at_parsep()) exit
                shortage = forward_line(1_int64)
            end do
            if (.not. eobp()) left = left - 1
            ! ... and one more line.
            shortage = forward_line(1_int64)
            do
                if (.not. search_forward_parstart(start)) exit
                call goto_char(start)
                if (eobp()) exit
                call move_to_left_margin()
                if (looking_at_parsep()) exit
                if (looking_at_parstart()) exit
                call goto_char(point() + 1)
            end do
            if (point() < point_max()) call goto_char(start)
        end do
        call goto_char(constrain_to_field(point(), opoint, .true., .false.))
    end function forward_paragraph

    !> Move backward to start of paragraph.
    !> With argument ARG, do it ARG times;
    !> a negative argument ARG = -N means move forward N paragraphs.
    !>
    !> A paragraph start is the beginning of a line which is a
    !> `paragraph-start' or which is ordinary text and follows a
    !> `paragraph-separate'ing line; except: if the first real line of a
    !> paragraph is preceded by a blank line, the paragraph starts at that
    !> blank line.
    !>
    !> See `forward-paragraph' for more information.  Port of
    !> paragraphs.el:backward-paragraph.
    subroutine backward_paragraph(arg)
        integer(int64), intent(in) :: arg
        integer(int64) :: left
        left = forward_paragraph(-arg)
    end subroutine backward_paragraph

    !> Put point at beginning of this paragraph, mark at end.
    !> The paragraph marked is the one that contains point or follows point.
    !>
    !> With argument ARG, puts mark at end of a following paragraph, so that
    !> the number of paragraphs marked equals ARG.
    !>
    !> If ARG is negative, point is put at end of this paragraph, mark is put
    !> at beginning of this or a previous paragraph.
    !>
    !> Interactively (or if ALLOW-EXTEND is non-nil), if this command is
    !> repeated or (in Transient Mark mode) if the mark is active,
    !> it marks the next ARG paragraphs after the ones already marked.
    !> Port of paragraphs.el:mark-paragraph.
    subroutine mark_paragraph(arg, allow_extend, status)
        integer(int64), intent(in) :: arg
        logical, intent(in) :: allow_extend
        type(status_type), intent(out) :: status
        integer(int64) :: opoint, target, left
        status = status_type()
        if (arg == 0) then
            call set_error(status, FM_GENERIC_ERROR, "Cannot mark zero paragraphs")
            return
        end if
        if (allow_extend .and. ((value_eq(last_command, this_command) .and. mark_position() /= 0) .or. &
                                (transient_mark_mode .and. buffers(cur)%mark_active))) then
            opoint = point()
            call goto_char(mark_position())
            left = forward_paragraph(arg)
            target = point()
            call goto_char(opoint)
            call set_mark(target)
        else
            left = forward_paragraph(arg)
            call push_mark(nomsg=.true., activate=.true.)
            call backward_paragraph(arg)
        end if
    end subroutine mark_paragraph

    !> Kill forward to end of paragraph.
    !> With ARG N, kill forward to Nth end of paragraph;
    !> negative ARG -N means kill backward to Nth start of paragraph.
    !> Port of paragraphs.el:kill-paragraph.
    subroutine kill_paragraph(arg, status)
        integer(int64), intent(in) :: arg
        type(status_type), intent(out) :: status
        integer(int64) :: start, left
        start = point()
        left = forward_paragraph(arg)
        call kill_region(start, point(), status)
    end subroutine kill_paragraph

    !> Kill back to start of paragraph.
    !> With ARG N, kill back to Nth start of paragraph;
    !> negative ARG -N means kill forward to Nth end of paragraph.
    !> Port of paragraphs.el:backward-kill-paragraph.
    subroutine backward_kill_paragraph(arg, status)
        integer(int64), intent(in) :: arg
        type(status_type), intent(out) :: status
        integer(int64) :: start
        start = point()
        call backward_paragraph(arg)
        call kill_region(start, point(), status)
    end subroutine backward_kill_paragraph

    !> Move to the start of the current paragraph.
    !> Port of paragraphs.el:start-of-paragraph-text.
    recursive subroutine start_of_paragraph_text()
        integer(int64) :: opoint, npoint, left
        opoint = point()
        left = forward_paragraph(-1_int64)
        npoint = point()
        call goto_char(skip(point(), [SPACE, TAB, NEWLINE]))
        ! If the range of blank lines found spans the original start point,
        ! try again from the beginning of it.  Must be careful to avoid
        ! infinite loop when following a single return at start of buffer.
        if (point() >= opoint .and. npoint < opoint) then
            call goto_char(npoint)
            if (npoint > point_min()) call start_of_paragraph_text()
        end if
    end subroutine start_of_paragraph_text

    !> Move to the end of the current paragraph.
    !> Port of paragraphs.el:end-of-paragraph-text.
    recursive subroutine end_of_paragraph_text()
        integer(int64) :: opoint, left
        opoint = point()
        left = forward_paragraph(1_int64)
        if (char_at(point() - 1) == NEWLINE) call goto_char(point() - 1)
        if (point() <= opoint) then
            call goto_char(point() + 1)
            if (point() < point_max()) call end_of_paragraph_text()
        end if
    end subroutine end_of_paragraph_text

    !> Move forward to end of paragraph.
    !> With argument ARG, do it ARG times;
    !> a negative argument ARG = -N means move backward N paragraphs.
    !>
    !> A line which `paragraph-start' matches either separates paragraphs
    !> (if `paragraph-separate' matches it also) or is the first line of a paragraph.
    !> A paragraph end is the beginning of a line which is not part of the paragraph
    !> to which the end of the previous line belongs, or the end of the buffer.
    !> Port of paragraphs.el:forward-paragraph (the command).
    subroutine cmd_forward_paragraph(status)
        type(status_type), intent(out) :: status
        integer(int64) :: left
        status = status_type()
        left = forward_paragraph(prefix_numeric_value(current_prefix_arg))
    end subroutine cmd_forward_paragraph

    subroutine cmd_backward_paragraph(status)
        type(status_type), intent(out) :: status
        status = status_type()
        call backward_paragraph(prefix_numeric_value(current_prefix_arg))
    end subroutine cmd_backward_paragraph

    subroutine cmd_mark_paragraph(status)
        type(status_type), intent(out) :: status
        call mark_paragraph(prefix_numeric_value(current_prefix_arg), .true., status)
    end subroutine cmd_mark_paragraph

    subroutine cmd_kill_paragraph(status)
        type(status_type), intent(out) :: status
        call kill_paragraph(prefix_numeric_value(current_prefix_arg), status)
    end subroutine cmd_kill_paragraph

    subroutine cmd_backward_kill_paragraph(status)
        type(status_type), intent(out) :: status
        call backward_kill_paragraph(prefix_numeric_value(current_prefix_arg), status)
    end subroutine cmd_backward_kill_paragraph

    !> Register the commands.
    subroutine syms_of_paragraphs()
        call defcommand("forward-paragraph", cmd_forward_paragraph)
        call defcommand("backward-paragraph", cmd_backward_paragraph)
        call defcommand("mark-paragraph", cmd_mark_paragraph)
        call defcommand("kill-paragraph", cmd_kill_paragraph)
        call defcommand("backward-kill-paragraph", cmd_backward_kill_paragraph)
    end subroutine syms_of_paragraphs
end module paragraphs_fm
