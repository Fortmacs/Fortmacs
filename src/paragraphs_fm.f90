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

!> Paragraph and sentence motion, by the regexps `paragraph-start`,
!> `paragraph-separate` and `sentence-end`.  With `left-margin` 0, so
!> that the left margin is the start of the line.
!>
!> Deferred: `fill-prefix` and `paragraph-ignore-fill-prefix` (with
!> filling), `use-hard-newlines`, `transpose-paragraphs` and
!> `transpose-sentences` (transpose-subr), `repunctuate-sentences`
!> (query-replace), `count-sentences`, `forward-sentence-function`.
module paragraphs_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use commands_fm, only: commands
    use error_fm, only: status_type, set_error, FM_GENERIC_ERROR
    use buffer_fm, only: buffers, cur, buf_fetch_char, transient_mark_mode, barf_if_buffer_read_only
    use marker_fm, only: char_to_byte
    use editfns_fm, only: point, goto_char, bobp, eobp, bolp, point_min, point_max, constrain_to_field
    use cmds_fm, only: forward_line, beginning_of_line, end_of_line
    use indent_fm, only: current_column
    use simple_fm, only: kill_region, set_mark, push_mark, transpose_subr
    use editfns_fm, only: mark_position
    use data_fm, only: value_eq
    use callint_fm, only: current_prefix_arg, prefix_numeric_value
    use keyboard_fm, only: this_command, last_command
    use search_fm, only: looking_at, re_search_forward, re_search_backward, match_beginning, match_end, &
                         SEARCH_NOERROR, SEARCH_MOVE
    implicit none(type, external)
    private

    public :: transpose_paragraphs, transpose_sentences, init_paragraph_variables
    public :: forward_paragraph, backward_paragraph, mark_paragraph, kill_paragraph, backward_kill_paragraph, &
              start_of_paragraph_text, end_of_paragraph_text, syms_of_paragraphs
    public :: sentence_end_regexp, forward_sentence, backward_sentence, kill_sentence, backward_kill_sentence, &
              mark_end_of_sentence

    integer, parameter :: TAB = 9, NEWLINE = 10, FORMFEED = 12, SPACE = 32

    !> Regexp for beginning of a line that starts OR separates paragraphs.
    !> This regexp should match lines that separate paragraphs and should
    !> also match lines that start a paragraph (and are part of that
    !> paragraph).
    !>
    !> This is matched against the text at the left margin, which is not
    !> necessarily the beginning of the line, so it should never use "^"
    !> as an anchor.  This ensures that the paragraph functions will work
    !> equally well within a region of text indented by a margin setting.
    !>
    !> The variable `paragraph-separate' specifies how to distinguish lines
    !> that start paragraphs from lines that separate them.
    !>
    !> If the variable `use-hard-newlines' is non-nil, then only lines
    !> following a hard newline are considered to match.  Port of
    !> paragraphs.el paragraph-start (one global value; set by
    !> syms_of_paragraphs).
    character(len=:), allocatable, public :: paragraph_start

    !> Regexp for beginning of a line that separates paragraphs.
    !> If you change this, you may have to change `paragraph-start' also.
    !>
    !> This is matched against the text at the left margin, which is not
    !> necessarily the beginning of the line, so it should not use "^" as
    !> an anchor.  This ensures that the paragraph functions will work
    !> equally within a region of text indented by a margin setting.  Port
    !> of paragraphs.el paragraph-separate.
    character(len=:), allocatable, public :: paragraph_separate

    !> Non-nil means a single space does not end a sentence.
    !> This user option affects fill commands and sentence commands.
    !>
    !> If this variable is non-nil, Emacs considers a period followed by
    !> two spaces or by a newline as the end of a sentence.  Port of
    !> paragraphs.el sentence-end-double-space.
    logical, public :: sentence_end_double_space = .true.

    !> Non-nil means a sentence will end without a period.
    !> For example, a sentence in Thai text ends with double space but
    !> without a period.
    !>
    !> This value is used by the function `sentence-end' to construct the
    !> regexp describing the end of a sentence, when the value of the
    !> variable `sentence-end' is nil.  Port of paragraphs.el
    !> sentence-end-without-period.
    logical, public :: sentence_end_without_period = .false.

    !> String of characters that end sentence without following spaces.
    !>
    !> This value is used by the function `sentence-end' to construct the
    !> regexp describing the end of a sentence, when the value of the
    !> variable `sentence-end' is nil.  Port of paragraphs.el
    !> sentence-end-without-space.
    character(len=:), allocatable, public :: sentence_end_without_space

    !> Regexp describing the end of a sentence.
    !> The value includes the whitespace following the sentence.
    !> All paragraph boundaries also end sentences, regardless.
    !>
    !> The value nil (unallocated) means to use the default value defined
    !> by the function `sentence-end'.  You should always use this function
    !> to obtain the value of this variable.  Port of paragraphs.el
    !> sentence-end.
    character(len=:), allocatable, public :: sentence_end

    !> Regexp matching the basic end of a sentence, not including
    !> following space.  Port of paragraphs.el sentence-end-base.
    character(len=:), allocatable, public :: sentence_end_base

    !> No-break space, which counts as a space in sentence ends.
    character(len=*), parameter :: NBSP = char(194)//char(160)

contains

    !> Give the variables their default values, unless already set.
    subroutine init_paragraph_variables()
        if (.not. allocated(paragraph_start)) paragraph_start = achar(FORMFEED)//"\|[ "//achar(TAB)//"]*$"
        if (.not. allocated(paragraph_separate)) paragraph_separate = "[ "//achar(TAB)//achar(FORMFEED)//"]*$"
        if (.not. allocated(sentence_end_without_space)) sentence_end_without_space = "。．？！"
        if (.not. allocated(sentence_end_base)) sentence_end_base = "[.?!…‽][]""'”’)}»›]*"
    end subroutine init_paragraph_variables

    !> The character at `pos`, or -1 outside the accessible region.
    integer function char_at(pos)
        integer(int64), intent(in) :: pos
        char_at = -1
        if (pos < buffers(cur)%begv .or. pos >= buffers(cur)%zv) return
        char_at = buf_fetch_char(buffers(cur), char_to_byte(pos))
    end function char_at

    !> The first position from `pos` on that is not one of `chars`
    !> (skip-chars-forward).
    integer(int64) function skip(pos, chars) result(p)
        integer(int64), intent(in) :: pos
        integer, intent(in) :: chars(:)
        p = pos
        do while (any(char_at(p) == chars))
            p = p + 1
        end do
    end function skip

    !> (looking-at REGEXP), false for an invalid one.
    logical function looking_at_p(regexp)
        character(len=*), intent(in) :: regexp
        type(status_type) :: status
        looking_at_p = looking_at(regexp, status)
        if (.not. status%ok()) looking_at_p = .false.
    end function looking_at_p

    !> (re-search-backward REGEXP BOUND NOERROR) for NOERROR t or other
    !> than nil and t (`mode`).
    logical function re_search_back(regexp, mode, bound) result(found)
        character(len=*), intent(in) :: regexp
        integer, intent(in) :: mode
        integer(int64), intent(in), optional :: bound
        type(status_type) :: status
        integer(int64) :: pos
        call re_search_backward(regexp, pos, status, bound=bound, noerror=mode)
        found = status%ok() .and. pos > 0
    end function re_search_back

    !> (re-search-forward REGEXP BOUND NOERROR) for NOERROR t or other than
    !> nil and t (`mode`).
    logical function re_search_fwd(regexp, mode, bound) result(found)
        character(len=*), intent(in) :: regexp
        integer, intent(in) :: mode
        integer(int64), intent(in), optional :: bound
        type(status_type) :: status
        integer(int64) :: pos
        call re_search_forward(regexp, pos, status, bound=bound, noerror=mode)
        found = status%ok() .and. pos > 0
    end function re_search_fwd

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

    !> REGEXP without a leading ^.
    function unanchored(regexp) result(r)
        character(len=*), intent(in) :: regexp
        character(len=:), allocatable :: r
        r = regexp
        if (len(regexp) > 0) then
            if (regexp(1:1) == "^") r = regexp(2:)
        end if
    end function unanchored

    !> Move forward to end of paragraph.
    !> With argument ARG, do it ARG times;
    !> a negative argument ARG = -N means move backward N paragraphs.
    !>
    !> A line which `paragraph-start' matches either separates paragraphs
    !> (if `paragraph-separate' matches it also) or is the first line of a paragraph.
    !> A paragraph end is the beginning of a line which is not part of the paragraph
    !> to which the end of the previous line belongs, or the end of the buffer.
    !> Returns the count of paragraphs left to move.  Port of
    !> paragraphs.el:forward-paragraph (no fill prefix or hard newlines).
    integer(int64) function forward_paragraph(arg) result(left)
        integer(int64), intent(in) :: arg
        integer(int64) :: opoint, start, shortage
        character(len=:), allocatable :: parstart, parsep, sp_parstart
        logical :: found_start
        type(status_type) :: status
        call init_paragraph_variables()
        left = arg
        opoint = point()
        ! Remove ^ from paragraph-start and paragraph-sep if they are there.
        ! These regexps shouldn't be anchored, because we look for them
        ! starting at the left-margin.  This allows paragraph commands to
        ! work normally with indented text.
        parstart = unanchored(paragraph_start)
        parsep = unanchored(paragraph_separate)
        ! This is used for searching.
        sp_parstart = "^[ "//achar(TAB)//"]*\(?:"//parstart//"\|"//parsep//"\)"
        start = point()
        do while (left < 0 .and. .not. bobp())
            if (.not. looking_at_p(parsep) .and. re_search_back("^"//achar(NEWLINE), SEARCH_NOERROR, &
                                                                max(point() - 1, point_min())) .and. &
                looking_at_p(parsep)) then
                left = left + 1
            else
                start = point()
                ! Move back over paragraph-separating lines.
                call goto_char(point() - 1)
                call beginning_of_line()
                do while (.not. bobp())
                    call move_to_left_margin()
                    if (.not. looking_at_p(parsep)) exit
                    shortage = forward_line(-1_int64)
                end do
                if (.not. bobp()) then
                    left = left + 1
                    ! Go to end of the previous (non-separating) line.
                    call end_of_line()
                    ! Search back for line that starts or separates paragraphs.
                    found_start = .false.
                    do
                        if (.not. re_search_back(sp_parstart, SEARCH_MOVE)) exit
                        found_start = .true.
                        ! Found a candidate, but need to check if it is a
                        ! REAL parstart.
                        start = point()
                        call move_to_left_margin()
                        if (looking_at_p(parsep)) exit
                        if (looking_at_p(parstart)) exit
                        found_start = .false.
                        call goto_char(start)
                    end do
                    if (found_start) then
                        ! Move forward over paragraph separators.  We know
                        ! this cannot reach the place we started because we
                        ! know we moved back over a non-separator.
                        do while (.not. eobp())
                            call move_to_left_margin()
                            if (.not. looking_at_p(parsep)) exit
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
                if (.not. looking_at_p(parsep)) exit
                shortage = forward_line(1_int64)
            end do
            if (.not. eobp()) left = left - 1
            ! ... and one more line.
            shortage = forward_line(1_int64)
            do
                if (.not. re_search_fwd(sp_parstart, SEARCH_MOVE)) exit
                start = match_beginning(0, status)
                call goto_char(start)
                if (eobp()) exit
                call move_to_left_margin()
                if (looking_at_p(parsep)) exit
                if (looking_at_p(parstart)) exit
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

    !> Return the regexp describing the end of a sentence.
    !>
    !> This function returns either the value of the variable
    !> `sentence-end' if it is non-nil, or the default value constructed
    !> from the variables `sentence-end-base', `sentence-end-double-space',
    !> `sentence-end-without-period' and `sentence-end-without-space'.
    !>
    !> The default value specifies that in order to be recognized as the
    !> end of a sentence, the ending period, question mark, or exclamation
    !> point must be followed by two spaces, with perhaps some closing
    !> delimiters in between.  Port of paragraphs.el:sentence-end.
    function sentence_end_regexp() result(r)
        character(len=:), allocatable :: r
        call init_paragraph_variables()
        if (allocated(sentence_end)) then
            r = sentence_end
            return
        end if
        ! We accept non-break space along with space.
        r = ""
        if (sentence_end_without_period) r = "\w[ "//NBSP//"][ "//NBSP//"]\|"
        r = r//"\("//sentence_end_base
        if (sentence_end_double_space) then
            r = r//"\($\|[ "//NBSP//"]$\|"//achar(TAB)//"\|[ "//NBSP//"][ "//NBSP//"]\)"
        else
            r = r//"\($\|["//achar(TAB)//" "//NBSP//"]\)"
        end if
        r = r//"\|["//sentence_end_without_space//"]+\)[ "//NBSP//achar(TAB)//achar(NEWLINE)//"]*"
    end function sentence_end_regexp

    !> Move forward to next end of sentence.  With argument ARG, repeat.
    !> If ARG is negative, move backward repeatedly to start of sentence.
    !>
    !> The variable `sentence-end' is a regular expression that matches
    !> ends of sentences.  Also, every paragraph boundary terminates
    !> sentences as well.  Port of paragraphs.el:forward-sentence with
    !> forward-sentence-default-function.
    subroutine forward_sentence(arg)
        integer(int64), intent(in) :: arg
        integer(int64) :: n, opoint, pos, par_beg, par_text_beg, par_end
        character(len=:), allocatable :: send
        type(status_type) :: status
        n = arg
        opoint = point()
        send = sentence_end_regexp()
        do while (n < 0)
            pos = point()
            ! Start of real text in the paragraph.  We move back to here if
            ! we don't see a sentence-end.
            call start_of_paragraph_text()
            par_text_beg = point()
            ! Start of the first line of the paragraph.  We use this as the
            ! search limit to allow sentence-end to match if it is anchored
            ! at BOL and the paragraph starts indented.
            call beginning_of_line()
            par_beg = point()
            call goto_char(pos)
            if (back_twice()) then
                call goto_char(match_end(0, status))
            else
                call goto_char(par_text_beg)
            end if
            n = n + 1
        end do
        do while (n > 0)
            pos = point()
            call end_of_paragraph_text()
            par_end = point()
            call goto_char(pos)
            if (re_search_fwd(send, SEARCH_NOERROR, par_end)) then
                call skip_whitespace_backward()
            else
                call goto_char(par_end)
            end if
            n = n - 1
        end do
        call goto_char(constrain_to_field(point(), opoint, .true., .false.))
    contains

        !> (and (re-search-backward sentence-end par-beg t)
        !>      (or (< (match-end 0) pos)
        !>          (re-search-backward sentence-end par-beg t)))
        logical function back_twice()
            back_twice = re_search_back(send, SEARCH_NOERROR, par_beg)
            if (.not. back_twice) return
            if (match_end(0, status) < pos) return
            back_twice = re_search_back(send, SEARCH_NOERROR, par_beg)
        end function back_twice
    end subroutine forward_sentence

    !> skip-chars-backward " \t\n".
    subroutine skip_whitespace_backward()
        do while (point() > buffers(cur)%begv)
            if (all(char_at(point() - 1) /= [SPACE, TAB, NEWLINE])) exit
            call goto_char(point() - 1)
        end do
    end subroutine skip_whitespace_backward

    !> Move backward to start of sentence.
    !> With ARG, do it ARG times.  See `forward-sentence' for more
    !> information.  Port of paragraphs.el:backward-sentence.
    subroutine backward_sentence(arg)
        integer(int64), intent(in) :: arg
        call forward_sentence(-arg)
    end subroutine backward_sentence

    !> Kill from point to end of sentence.
    !> With ARG, repeat; negative ARG -N means kill back to Nth start of
    !> sentence.  Port of paragraphs.el:kill-sentence.
    subroutine kill_sentence(arg, status)
        integer(int64), intent(in) :: arg
        type(status_type), intent(out) :: status
        integer(int64) :: start
        start = point()
        call forward_sentence(arg)
        call kill_region(start, point(), status)
    end subroutine kill_sentence

    !> Kill back from point to start of sentence.
    !> With ARG, repeat, or kill forward to Nth end of sentence if
    !> negative ARG -N.  Port of paragraphs.el:backward-kill-sentence.
    subroutine backward_kill_sentence(arg, status)
        integer(int64), intent(in) :: arg
        type(status_type), intent(out) :: status
        integer(int64) :: start
        start = point()
        call backward_sentence(arg)
        call kill_region(start, point(), status)
    end subroutine backward_kill_sentence

    !> Put mark at end of sentence.
    !> ARG works as in `forward-sentence'.  If this command is repeated,
    !> it marks the next ARG sentences after the ones already marked.  Port
    !> of paragraphs.el:mark-end-of-sentence.
    subroutine mark_end_of_sentence(arg)
        integer(int64), intent(in) :: arg
        integer(int64) :: opoint, target
        opoint = point()
        if (value_eq(last_command, this_command) .and. mark_position() /= 0) call goto_char(mark_position())
        call forward_sentence(arg)
        target = point()
        call goto_char(opoint)
        call push_mark(target, nomsg=.true., activate=.true.)
    end subroutine mark_end_of_sentence

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

    subroutine cmd_forward_sentence(status)
        type(status_type), intent(out) :: status
        status = status_type()
        call forward_sentence(prefix_numeric_value(current_prefix_arg))
    end subroutine cmd_forward_sentence

    subroutine cmd_backward_sentence(status)
        type(status_type), intent(out) :: status
        status = status_type()
        call backward_sentence(prefix_numeric_value(current_prefix_arg))
    end subroutine cmd_backward_sentence

    subroutine cmd_kill_sentence(status)
        type(status_type), intent(out) :: status
        call kill_sentence(prefix_numeric_value(current_prefix_arg), status)
    end subroutine cmd_kill_sentence

    subroutine cmd_backward_kill_sentence(status)
        type(status_type), intent(out) :: status
        call backward_kill_sentence(prefix_numeric_value(current_prefix_arg), status)
    end subroutine cmd_backward_kill_sentence

    subroutine cmd_mark_end_of_sentence(status)
        type(status_type), intent(out) :: status
        status = status_type()
        call mark_end_of_sentence(prefix_numeric_value(current_prefix_arg))
    end subroutine cmd_mark_end_of_sentence

    !> Interchange the current paragraph with the next one.
    !> With prefix argument ARG a non-zero integer, moves the current
    !> paragraph past ARG paragraphs, leaving point after the current paragraph.
    !> If ARG is positive, moves the current paragraph forwards, if
    !> ARG is negative moves it backwards.  If ARG is zero, exchanges
    !> the current paragraph with the one containing the mark.  Port of
    !> paragraphs.el:transpose-paragraphs.
    subroutine transpose_paragraphs(arg, status)
        integer(int64), intent(in) :: arg
        type(status_type), intent(out) :: status
        call transpose_subr(move_paragraphs, arg, status)
    end subroutine transpose_paragraphs

    !> Interchange the current sentence with the next one.
    !> With prefix argument ARG a non-zero integer, moves the current
    !> sentence past ARG sentences, leaving point after the current sentence.
    !> If ARG is positive, moves the current sentence forwards, if
    !> ARG is negative moves it backwards.  If ARG is zero, exchanges
    !> the current sentence with the one containing the mark.  Port of
    !> paragraphs.el:transpose-sentences.
    subroutine transpose_sentences(arg, status)
        integer(int64), intent(in) :: arg
        type(status_type), intent(out) :: status
        call transpose_subr(move_sentences, arg, status)
    end subroutine transpose_sentences

    subroutine move_paragraphs(n, status)
        integer(int64), intent(in) :: n
        type(status_type), intent(out) :: status
        integer(int64) :: left
        status = status_type()
        left = forward_paragraph(n)
    end subroutine move_paragraphs

    subroutine move_sentences(n, status)
        integer(int64), intent(in) :: n
        type(status_type), intent(out) :: status
        status = status_type()
        call forward_sentence(n)
    end subroutine move_sentences

    subroutine cmd_transpose_paragraphs(status)
        type(status_type), intent(out) :: status
        call barf_if_buffer_read_only(status)
        if (status%ok()) call transpose_paragraphs(prefix_numeric_value(current_prefix_arg), status)
    end subroutine cmd_transpose_paragraphs

    subroutine cmd_transpose_sentences(status)
        type(status_type), intent(out) :: status
        call barf_if_buffer_read_only(status)
        if (status%ok()) call transpose_sentences(prefix_numeric_value(current_prefix_arg), status)
    end subroutine cmd_transpose_sentences

    !> Give the variables their defaults and register the commands.
    subroutine syms_of_paragraphs()
        call init_paragraph_variables()
        commands%forward_sentence%run => cmd_forward_sentence
        commands%backward_sentence%run => cmd_backward_sentence
        commands%kill_sentence%run => cmd_kill_sentence
        commands%backward_kill_sentence%run => cmd_backward_kill_sentence
        commands%mark_end_of_sentence%run => cmd_mark_end_of_sentence
        commands%forward_paragraph%run => cmd_forward_paragraph
        commands%backward_paragraph%run => cmd_backward_paragraph
        commands%mark_paragraph%run => cmd_mark_paragraph
        commands%kill_paragraph%run => cmd_kill_paragraph
        commands%backward_kill_paragraph%run => cmd_backward_kill_paragraph
        commands%transpose_paragraphs%run => cmd_transpose_paragraphs
        commands%transpose_sentences%run => cmd_transpose_sentences
    end subroutine syms_of_paragraphs
end module paragraphs_fm
