! fill_fm.f90 -- fill commands for Fortmacs.
! Ported from GNU Emacs 31.1 lisp/textmodes/fill.el.
!
! Copyright (C) 1985-2026 Free Software Foundation, Inc.
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

!> Filling: `fill_paragraph' (M-q), `fill_region',
!> `fill_region_as_paragraph', `set_fill_prefix' (C-x .), adaptive fill
!> prefixes, the no-break predicates, canonical spacing and
!> justification (left, right, full, center), `fill_paragraph_function' and
!> comments (`fill_comment_paragraph', in the submodule fill_comment_fm).
!>
!> Not ported yet: comments in `fill_region' and auto-fill, hard newlines,
!> `fill_region_as_paragraph_function', the semantic-linefeed variants,
!> kinsoku (`fill_find_break_point'), invisible text,
!> `fill_individual_paragraphs' and `set_justification'.
module fill_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use commands_fm, only: commands
    use error_fm, only: format_message, status_type, set_error, FM_GENERIC_ERROR, FM_USER_ERROR
    use handle_fm, only: handle_type
    use lisp_fm, only: value_type, nil, nilp, fixnump, xfixnum, symbolp, make_string, stringp, xstring
    use lread_fm, only: sym
    use data_fm, only: value_eq, symbol_name, logical_setting, add_local_variables_killer
    use buffer_fm, only: buffers, cur, fill_column, transient_mark_mode, barf_if_buffer_read_only, &
                         buffer_modified_p, restore_buffer_modified_p, current_buffer, buffer_serial_live
    use character_fm, only: string_char_and_length
    use characters_fm, only: char_has_category
    use charscript_fm, only: char_script, script_names
    use marker_fm, only: copy_marker_at, marker_position, free_marker
    use editfns_fm, only: point, goto_char, point_min, point_max, bobp, eobp, bolp, eolp, buffer_substring, &
                          delete_region, char_after, char_before, line_beginning_position, line_end_position, &
                          subst_char_in_region, region_beginning, region_end, narrow_to_region, widen, &
                          point_marker
    use insdel_fm, only: insert, insert_before_markers
    use cmds_fm, only: forward_line, beginning_of_line, end_of_line, forward_char, delete_char
    use textprop_fm, only: get_text_property, put_text_property, text_property_not_all
    use syntax_fm, only: skip_chars_forward, skip_chars_backward, skip_syntax_backward, char_syntax
    use search_fm, only: looking_at, re_search_forward, re_search_backward, search_forward, search_backward, &
                         match_beginning, match_end, replace_match, regexp_quote, string_match, SEARCH_NOERROR, &
                         SEARCH_MOVE
    use subr_fm, only: match_string
    use xdisp_fm, only: message, glyph_columns
    use indent_fm, only: current_column, move_to_column, indent_to, current_indentation, indent_line_to, &
                         current_left_margin, move_to_left_margin, indent_according_to_mode, beginning_of_line_text
    use paragraphs_fm, only: paragraph_start, sentence_end_double_space, sentence_end_without_period, &
                             sentence_end_without_space, sentence_end_regexp, forward_paragraph, &
                             init_paragraph_variables
    use simple_fm, only: fill_prefix, use_region_p, mark
    use minibuf_fm, only: minibufferp, minibuffer_prompt_end
    use callint_fm, only: current_prefix_arg, prefix_numeric_value
    implicit none(type, external)
    private

    public :: prefix_at_point, nchars
    public :: set_fill_prefix, current_fill_column, canonically_space_region, fill_common_string_prefix, &
              fill_context_prefix, fill_single_word_nobreak_p, fill_french_nobreak_p, fill_polish_nobreak_p, &
              fill_single_char_nobreak_p, fill_nobreak_p, fill_delete_prefix, fill_delete_newlines, &
              fill_move_to_break_point, fill_newline, fill_indent_to_left_margin, fill_region_as_paragraph, &
              fill_paragraph, fill_region, unfill_paragraph, current_justification, justify_current_line, &
              unjustify_current_line, unjustify_region, string_width, syms_of_fill, fill_comment_paragraph
    public :: fill_nobreak_predicate_proc, fill_paragraph_proc

    !> Justification styles: the values of `default_justification' (and
    !> `none'), and t for "as `current_justification' says".
    integer, parameter, public :: JUSTIFY_NONE = 0, JUSTIFY_LEFT = 1, JUSTIFY_RIGHT = 2, JUSTIFY_FULL = 3, &
                                  JUSTIFY_CENTER = 4, JUSTIFY_T = 5

    !> True means put two spaces after a colon when filling.  Port of
    !> fill.el colon-double-space.
    logical, public :: colon_double_space = .false.

    !> True means to use a space to separate words of a different kind.
    !> For example, when an English word at the end of a line and a CJK word
    !> at the beginning of the next line are joined into a single line, they
    !> will be separated by a space if this variable is true.
    !> Whether to use a space to separate such words also depends on the entry
    !> in `fill_nospace_between_words_table' for the characters before and
    !> after the newline.  Port of fill.el
    !> fill-separate-heterogeneous-words-with-space.
    logical, public :: fill_separate_heterogeneous_words_with_space = .false.

    !> True means determine a paragraph's fill prefix from its text.
    !> Port of fill.el adaptive-fill-mode.
    type(logical_setting), public :: adaptive_fill_mode = logical_setting(name="adaptive_fill_mode", value=.true.)

    !> Regexp to match text at start of line that constitutes indentation.
    !> If Adaptive Fill mode is enabled, a prefix matching this pattern
    !> on the first and second lines of a paragraph is used as the
    !> standard indentation for the whole paragraph.
    !>
    !> If the paragraph has just one line, the indentation is taken from that
    !> line, but in that case `adaptive_fill_first_line_regexp' also plays
    !> a role.  Port of fill.el adaptive-fill-regexp (unallocated is nil).
    character(len=:), allocatable, public :: adaptive_fill_regexp

    !> Regexp specifying whether to set fill prefix from a one-line paragraph.
    !> When a paragraph has just one line, then after `adaptive_fill_regexp'
    !> finds the prefix at the beginning of the line, if it doesn't
    !> match this regexp, it is replaced with whitespace.
    !>
    !> By default, this regexp matches sequences of just spaces and tabs.
    !>
    !> However, we never use a prefix from a one-line paragraph
    !> if it would act as a paragraph-starter on the second line.  Port of
    !> fill.el adaptive-fill-first-line-regexp.
    character(len=:), allocatable, public :: adaptive_fill_first_line_regexp

    !> Whether or not filling should try to use the major mode's indentation.
    !> Port of fill.el fill-indent-according-to-mode.
    logical, public :: fill_indent_according_to_mode = .false.

    !> Method of justifying text not otherwise specified.
    !> Possible values are JUSTIFY_LEFT, JUSTIFY_RIGHT, JUSTIFY_FULL,
    !> JUSTIFY_CENTER, or JUSTIFY_NONE.
    !> The values JUSTIFY_LEFT and JUSTIFY_RIGHT mean lines are lined up at,
    !> respectively, left or right margin, and ragged at the other margin.
    !> JUSTIFY_FULL means lines are lined up at both margins.  JUSTIFY_CENTER means each
    !> line is centered.  JUSTIFY_NONE means no justification or centering.
    !> The requested kind of justification is done whenever lines are filled.
    !> The `justification' text-property can locally override this variable.
    !> Port of fill.el default-justification (a JUSTIFY_ constant).
    integer, public :: default_justification = JUSTIFY_LEFT

    abstract interface
        !> A predicate of `fill_nobreak_predicate': true forbids breaking
        !> the line at point.  Not a port.
        logical function fill_nobreak_predicate_proc()
            implicit none(type, external)
        end function fill_nobreak_predicate_proc
    end interface

    !> An element of `fill_nobreak_predicate'.
    type, public :: fill_nobreak_predicate_type
        procedure(fill_nobreak_predicate_proc), pointer, nopass :: fn => null()
    end type fill_nobreak_predicate_type

    !> Array of predicates for recognizing places not to break a line.
    !> The predicates are called with no arguments, with point at the place to
    !> be tested.  If it returns true, fill commands do not break
    !> the line there.  Port of fill.el fill-nobreak-predicate.
    type(fill_nobreak_predicate_type), allocatable, public :: fill_nobreak_predicate(:)

    character(len=*), parameter :: TAB = achar(9), LF = achar(10)

    ! fill-paragraph-function is t: a fill-paragraph-function (or
    ! fill_minibuffer_function) is running.
    logical :: in_fill_paragraph_function = .false.

    abstract interface
        !> A `fill_paragraph_function': fill the paragraph at point (JUSTIFY
        !> as in `fill_paragraph'); DONE false (upstream's nil) means
        !> `fill_paragraph' should do its normal work.  Not a port.
        subroutine fill_paragraph_proc(status, done, justify)
            import :: status_type
            implicit none(type, external)
            type(status_type), intent(out) :: status
            logical, intent(out) :: done
            integer, intent(in), optional :: justify
        end subroutine fill_paragraph_proc
    end interface

    !> One buffer's `fill_paragraph_function'.
    type :: fill_paragraph_entry_type
        integer(int64) :: serial = 0
        procedure(fill_paragraph_proc), pointer, nopass :: fn => null()
    end type fill_paragraph_entry_type

    !> The type of `fill_paragraph_function': a default `value' (assign
    !> with `=>'; null is nil) and buffers' own (`set_local').
    type, public :: fill_paragraph_function_type
        procedure(fill_paragraph_proc), pointer, nopass :: value => null()
        type(fill_paragraph_entry_type), allocatable :: entries(:)
    contains
        !> Make FN the current buffer's function (setq-local).
        procedure :: set_local => fill_paragraph_set_local
        !> The current buffer's function (null for nil).
        procedure :: get => fill_paragraph_get
        !> Make the current buffer use the default again.
        procedure :: kill_local => fill_paragraph_kill_local
    end type fill_paragraph_function_type

    !> Mode-specific function to fill a paragraph, or null if there is none.
    !> If the function returns false (DONE), then `fill_paragraph' does its
    !> normal work.
    !> Note: This only affects `fill_paragraph' and not `fill_region'
    !> nor `auto_fill_mode', so it is often better to use some other hook,
    !> such as `fill_forward_paragraph_function'.  Port of fill.el
    !> fill-paragraph-function (its value t is internal).
    type(fill_paragraph_function_type), public :: fill_paragraph_function

    !> True means paragraph filling will try to pay attention to comments.
    !> Port of fill.el fill-paragraph-handle-comment.
    logical, public :: fill_paragraph_handle_comment = .true.

    interface
        !> Fill current comment.
        !> If we're not in a comment, just return false (DONE) so that the
        !> caller can take care of filling.  JUSTIFY is used as in
        !> `fill_paragraph'.  Port of fill.el:fill-comment-paragraph.  (In
        !> fill_comment_fm.)
        recursive module subroutine fill_comment_paragraph(status, done, justify)
            implicit none(type, external)
            type(status_type), intent(out) :: status
            logical, intent(out) :: done
            integer, intent(in), optional :: justify
        end subroutine fill_comment_paragraph

        !> Whether `fill_paragraph' can fill comments (step 3): a comment
        !> syntax ending at end of line.  (In fill_comment_fm.)  Not a port.
        module logical function fill_comment_syntax_p()
            implicit none(type, external)
        end function fill_comment_syntax_p

        !> `comment_start_skip', "" for nil.  (In fill_comment_fm.)
        !> Not a port.
        module function fill_comment_start_skip() result(skip)
            implicit none(type, external)
            character(len=:), allocatable :: skip
        end function fill_comment_start_skip
    end interface

contains

    ! Not a port.
    subroutine init_fill_variables()
        if (.not. allocated(adaptive_fill_regexp)) then
            adaptive_fill_regexp = "[-"//char(226)//char(128)//char(147)//"!|#%;>*"//char(194)//char(183)// &
                                   char(226)//char(128)//char(162)//char(226)//char(128)//char(163)// &
                                   char(226)//char(129)//char(131)//char(226)//char(151)//char(166)//" "//TAB//"]*"
        end if

        if (.not. allocated(adaptive_fill_first_line_regexp)) adaptive_fill_first_line_regexp = "\`[ "//TAB//"]*\'"
        call init_paragraph_variables()
    end subroutine init_fill_variables

    !> Number of columns STRING occupies on display.  Port of
    !> character.c:Fstring_width (no display properties).
    integer(int64) function string_width(string) result(width)
        character(len=*), intent(in) :: string
        integer(int64) :: i
        integer(int32) :: c, clen

        width = 0
        i = 1

        do while (i <= len(string, kind=int64))
            call string_char_and_length(string, i, c, clen)
            i = i + clen
            width = width + glyph_columns(c, int(width))
        end do
    end function string_width

    !> Set the fill prefix to the current line up to point.
    !> Filling expects lines to start with the fill prefix and
    !> reinserts the fill prefix in each resulting line.
    !> With a prefix argument, cancel the fill prefix.  Port of
    !> fill.el:set-fill-prefix.
    subroutine set_fill_prefix(arg)
        logical, intent(in) :: arg
        integer(int64) :: left_margin_pos, opoint
        type(status_type) :: st

        if (allocated(fill_prefix%value)) deallocate (fill_prefix%value)

        if (.not. arg) then
            opoint = point()
            call move_to_left_margin(st)
            left_margin_pos = point()
            call goto_char(opoint)

            if (opoint > left_margin_pos) then
                call buffer_substring(left_margin_pos, opoint, fill_prefix%value, st)

                if (allocated(fill_prefix%value)) then
                    if (len(fill_prefix%value) == 0) deallocate (fill_prefix%value)
                end if
            end if
        end if

        if (allocated(fill_prefix%value)) then
            call message("fill_prefix: """//fill_prefix%value//"""")
        else
            call message("fill_prefix cancelled")
        end if
    end subroutine set_fill_prefix

    !> Return the fill-column to use for this line.
    !> The fill-column to use for a buffer is stored in the variable `fill_column',
    !> but can be locally modified by the `right_margin' text property, which is
    !> subtracted from `fill_column'.
    !>
    !> The fill column to use for a line is the first column at which the column
    !> number equals or exceeds the local fill-column - right-margin difference.
    !> Port of fill.el:current-fill-column.
    integer(int64) function current_fill_column() result(fc)
        integer(int64) :: here, here_col, eol, fill_col, change, col, opoint
        type(value_type) :: margin
        type(status_type) :: st
        logical :: found

        opoint = point()
        here = line_beginning_position()
        here_col = 0
        call end_of_line()
        eol = point()

        ! Look separately at each region of line with a different
        ! right-margin.
        do
            call get_text_property(here, sym("right_margin"), margin, st)
            fill_col = fill_column%value
            if (fixnump(margin)) fill_col = fill_column%value - xfixnum(margin)
            call text_property_not_all(here, eol, sym("right_margin"), margin, change, found, st)
            if (.not. found) exit
            call goto_char(change - 1)
            col = current_column()
            if (col >= fill_col) exit
            here = change
            here_col = col
        end do

        fc = max(here_col, fill_col)
        call goto_char(opoint)
    end function current_fill_column

    !> Remove extra spaces between words in region.
    !> Leave one space between words, two at end of sentences or after colons
    !> (depending on values of `sentence_end_double_space', `colon_double_space',
    !> and `sentence_end_without_period').
    !> Remove indentation from each line.  Port of
    !> fill.el:canonically-space-region.
    subroutine canonically_space_region(beg, end, status)
        integer(int64), intent(in) :: beg, end
        type(status_type), intent(out) :: status
        type(handle_type) :: end_marker
        type(status_type) :: st
        character(len=:), allocatable :: end_spc_re
        integer(int64) :: opoint, endpos, pos, from, me0, me1, mb0, keep, moved
        integer(int32) :: c
        logical :: found

        status = status_type()
        call copy_marker_at(end, end_marker, .true., st)
        end_spc_re = "\("//sentence_end_regexp()//"\) *\|  +"
        opoint = point()
        call goto_char(beg)
        ! Nuke tabs; they get screwed up in a fill.
        ! This is quick, but loses when a tab follows the end of a sentence.
        ! Actually, it is difficult to tell that from "Mr.\tSmith".
        ! Blame the typist.
        call subst_char_in_region(beg, end, 9, 32, status)

        do while (status%ok())
            call marker_position(end_marker, endpos, st)
            if (point() >= endpos) exit
            call re_search_forward(end_spc_re, pos, status, bound=endpos, noerror=SEARCH_NOERROR)
            if (.not. status%ok() .or. pos == 0) exit
            me0 = match_end(0, st)
            me1 = match_end(1, st)
            mb0 = match_beginning(0, st)

            if (me1 > 0 .and. me0 > me1) then
                ! `sentence_end' matched and did not match all spaces.
                ! I.e. it only matched the number of spaces it needs: drop the
                ! rest.
                from = me1
            else if (me1 > 0) then
                ! `sentence_end' matched but with nothing left.  Either that
                ! means nothing should be removed, or it means it's the
                ! "old-style" sentence-end which matches all it can.  Keep only
                ! 2 spaces.
                call goto_char(me0)
                moved = skip_chars_backward(" ", st)
                from = min(me0, merge(2_int64, 1_int64, sentence_end_double_space%value) + point())
                call goto_char(me0)
            else
                ! It's not an end of sentence.  Determine number of spaces
                ! to leave.
                call goto_char(me0)
                call goto_char(mb0)
                moved = skip_chars_backward(" ]})""'", st)
                call char_before(c, found)
                keep = 1
                if (.not. found) c = 0

                if (sentence_end_double_space%value .and. (c == iachar(".") .or. c == iachar("?") .or. &
                                                           c == iachar("!"))) then
                    keep = 2
                else if (sentence_end_double_space%value .and. sentence_end_without_period .and. found) then
                    if (char_syntax(int(c)) == "w") keep = 2
                end if

                if (keep /= 2) then
                    if (colon_double_space .and. c == iachar(":")) then
                        keep = 2
                    else if (c == 10) then
                        keep = 0
                    end if
                end if

                from = mb0 + keep
                call goto_char(me0)
            end if

            call delete_region(from, me0, status)
        end do

        call goto_char(opoint)
        call free_marker(end_marker, st)
    end subroutine canonically_space_region

    !> Return the longest common prefix of strings S1 and S2, or nil if none.
    !> `found' is false for nil.  Port of fill.el:fill-common-string-prefix.
    subroutine fill_common_string_prefix(s1, s2, prefix, found)
        character(len=*), intent(in) :: s1, s2
        character(len=:), allocatable, intent(out) :: prefix
        logical, intent(out) :: found
        integer :: n, i1, i2
        integer(int64) :: p1, p2
        integer(int32) :: c1, c2, l1, l2

        ! Compare by characters.
        p1 = 1
        p2 = 1
        n = 0

        do while (p1 <= len(s1, kind=int64) .and. p2 <= len(s2, kind=int64))
            call string_char_and_length(s1, p1, c1, l1)
            call string_char_and_length(s2, p2, c2, l2)
            if (c1 /= c2) exit
            p1 = p1 + l1
            p2 = p2 + l2
            n = n + 1
        end do

        i1 = int(p1)
        i2 = int(p2)
        found = n > 0
        prefix = s1(1:i1 - 1)
        if (i2 < 0) found = .false.
    end subroutine fill_common_string_prefix

    ! fill.el:fill-match-adaptive-prefix (adaptive-fill-function is
    ! `ignore').  Port of fill.el:fill-match-adaptive-prefix.
    subroutine fill_match_adaptive_prefix(str, found)
        character(len=:), allocatable, intent(out) :: str
        logical, intent(out) :: found
        type(status_type) :: st

        str = ""
        found = .false.
        if (.not. allocated(adaptive_fill_regexp)) return
        if (.not. looking_at(adaptive_fill_regexp, st)) return
        str = match_string(0)
        found = .true.

        ! Death to insanely long prefixes.
        if (current_left_margin() + nchars(str) >= current_fill_column()) then
            found = .false.
            str = ""
        end if
    end subroutine fill_match_adaptive_prefix

    !> Compute a fill prefix from the text between FROM and TO.
    !> This uses the variables `adaptive_fill_regexp' and `adaptive_fill_function'
    !> and `adaptive_fill_first_line_regexp'.  `paragraph_start' also plays a role;
    !> we reject a prefix based on a one-line paragraph if that prefix would
    !> act as a paragraph-separator.  `found' is false for nil.  Port of
    !> fill.el:fill-context-prefix.
    subroutine fill_context_prefix(from, to, prefix, found, first_line_regexp)
        integer(int64), intent(in) :: from, to
        character(len=:), allocatable, intent(out) :: prefix
        logical, intent(out) :: found
        character(len=*), intent(in), optional :: first_line_regexp
        character(len=:), allocatable :: first_line_prefix, second_line_prefix, re, tmp, result, flr
        logical :: have_first, have_second, matched
        integer(int64) :: opoint, shortage, idx
        type(status_type) :: st

        call init_fill_variables()
        prefix = ""
        found = .false.
        flr = adaptive_fill_first_line_regexp
        if (present(first_line_regexp)) flr = first_line_regexp
        opoint = point()
        call goto_char(from)
        if (eolp()) shortage = forward_line(1_int64)
        ! Move to the second line unless there is just one.
        call move_to_left_margin(st)
        call fill_match_adaptive_prefix(first_line_prefix, have_first)
        shortage = forward_line(1_int64)

        if (point() < to) then
            call move_to_left_margin(st)
            have_second = .false.
            if (.not. looking_at(paragraph_start%value, st)) call fill_match_adaptive_prefix(second_line_prefix, have_second)

            ! If we get a fill prefix from the second line, make sure it or
            ! something compatible is on the first line too.
            if (have_second) then
                if (.not. have_first) first_line_prefix = ""
                ! If the non-whitespace chars match the first line, just use
                ! it (this subsumes the 2 checks used previously).
                tmp = second_line_prefix
                re = "\`"

                do
                    idx = string_match("\`[ "//TAB//"]*\([^ "//TAB//"]+\)", tmp, st)
                    if (idx == 0 .or. .not. st%ok()) exit
                    re = re//".*"//regexp_quote(match_string(1, tmp))
                    tmp = tmp(match_end(0, st):)
                end do

                if (string_match(re, first_line_prefix, st) > 0) then
                    prefix = second_line_prefix
                    found = .true.
                else
                    ! Use the longest common substring of both prefixes, if
                    ! there is one.
                    call fill_common_string_prefix(first_line_prefix, second_line_prefix, prefix, found)
                end if
            end if
        else if (have_first) then
            ! If we get a fill prefix from a one-line paragraph, maybe change
            ! it to whitespace, and check that it isn't a paragraph starter.
            matched = string_match(flr, first_line_prefix, st) > 0

            if (matched) then
                result = first_line_prefix
            else
                result = repeat(" ", int(string_width(first_line_prefix)))
            end if

            ! But either way, reject it if it indicates the start of a
            ! paragraph when text follows it.
            if (string_match(paragraph_start%value, result//"a", st) /= 1) then
                prefix = result
                found = .true.
            end if
        end if

        call goto_char(opoint)
    end subroutine fill_context_prefix

    !> Don't break a line after the first or before the last word of a sentence.
    !> Port of fill.el:fill-single-word-nobreak-p.
    logical function fill_single_word_nobreak_p() result(nobreak)
        type(status_type) :: st
        integer(int64) :: opoint, moved

        nobreak = looking_at("[ "//TAB//"]*\sw+\(?:"//sentence_end_regexp()//"\)[ "//TAB//"]*$", st)
        if (nobreak) return
        opoint = point()
        moved = skip_chars_backward(" "//TAB, st)

        if (skip_syntax_backward("w") /= 0) then
            if (skip_chars_backward(" "//TAB, st) /= 0) then
                if (skip_chars_backward(".?!:", st) /= 0) nobreak = looking_at(sentence_end_regexp(), st)
            end if
        end if

        call goto_char(opoint)
    end function fill_single_word_nobreak_p

    !> Return false if French style allows breaking the line at point.
    !> This is used in `fill_nobreak_predicate' to prevent breaking lines just
    !> after an opening paren or just before a closing paren or a punctuation
    !> mark such as `?' or `:'.  It is common in French writing to put a space
    !> at such places, which would normally allow breaking the line at those
    !> places.  Port of fill.el:fill-french-nobreak-p.
    logical function fill_french_nobreak_p() result(nobreak)
        type(status_type) :: st
        integer(int64) :: opoint, moved
        integer(int32) :: c, d
        logical :: found, found2

        nobreak = looking_at("[ "//TAB//"]*[])}"//char(194)//char(187)//"?!;:-]", st)
        if (nobreak) return
        opoint = point()
        moved = skip_chars_backward(" "//TAB, st)

        if (.not. bolp()) then
            call goto_char(point() - 1)
            nobreak = looking_at("[([{"//char(194)//char(171)//"]", st)

            if (.not. nobreak) then
                ! Don't cut right after a single-letter word.
                call char_before(c, found)
                call char_after(d, found2)
                if (found .and. found2) nobreak = (c == 9 .or. c == 32) .and. char_syntax(int(d)) == "w"
            end if
        end if

        call goto_char(opoint)
    end function fill_french_nobreak_p

    !> Return false if Polish style allows breaking the line at point.
    !> This function may be used in the `fill_nobreak_predicate' hook.
    !> It is almost the same as `fill_single_char_nobreak_p', with the
    !> exception that it does not require the one-letter word to be
    !> preceded by a space.  This blocks line-breaking in cases like
    !> "(a jednak)".  Port of fill.el:fill-polish-nobreak-p.
    logical function fill_polish_nobreak_p() result(nobreak)
        nobreak = two_back_looking_at("[^[:alpha:]]\cl")
    end function fill_polish_nobreak_p

    !> Return true if a one-letter word is before point.
    !> This function is suitable for adding to the hook `fill_nobreak_predicate',
    !> to prevent the breaking of a line just after a one-letter word,
    !> which is an error according to some typographical conventions.
    !> Port of fill.el:fill-single-char-nobreak-p.
    logical function fill_single_char_nobreak_p() result(nobreak)
        nobreak = two_back_looking_at("[[:space:]][[:alpha:]]")
    end function fill_single_char_nobreak_p

    ! (save-excursion (skip-chars-backward " \t") (backward-char 2)
    !                 (looking-at REGEXP)); at the start of the buffer
    ! backward-char signals an error, which counts as nil.  Not a port.
    logical function two_back_looking_at(regexp) result(matched)
        character(len=*), intent(in) :: regexp
        type(status_type) :: st
        integer(int64) :: opoint, moved

        matched = .false.
        opoint = point()
        moved = skip_chars_backward(" "//TAB, st)

        if (point() - 2 >= point_min()) then
            call goto_char(point() - 2)
            matched = looking_at(regexp, st)
        end if

        call goto_char(opoint)
    end function two_back_looking_at

    !> Return false if breaking the line at point is allowed.
    !> Can be customized with the variables `fill_nobreak_predicate'
    !> and `fill_nobreak_invisible'.  Port of fill.el:fill-nobreak-p.
    logical function fill_nobreak_p() result(nobreak)
        type(status_type) :: st
        integer(int64) :: opoint, moved
        integer(int32) :: c
        logical :: found
        integer :: k

        nobreak = .false.
        if (bolp()) return
        opoint = point()

        ! Don't break after a period followed by just one space.  Move back
        ! to the previous place to break.  The reason is that if a period
        ! ends up at the end of a line, further fills will assume it ends a
        ! sentence.  If we now know it does not end a sentence, avoid
        ! putting it at the end of the line.
        if (sentence_end_double_space%value) then
            moved = skip_chars_backward(" ", st)
            call char_before(c, found)
            ! There's something more after the space.
            if (found .and. c == iachar(".")) nobreak = looking_at(" [^ "//LF//"]", st)
            call goto_char(opoint)
            if (nobreak) return
        end if

        ! Don't split a line if the rest would look like a new paragraph.
        moved = skip_chars_forward(" "//TAB, st)
        ! If this break point is at the end of the line, which can occur
        ! for auto-fill, don't consider the newline which follows as a
        ! reason to return t.
        if (.not. eolp()) nobreak = looking_at(paragraph_start%value, st)
        call goto_char(opoint)
        if (nobreak) return

        if (allocated(fill_nobreak_predicate)) then
            do k = 1, size(fill_nobreak_predicate)
                if (.not. associated(fill_nobreak_predicate(k)%fn)) cycle
                nobreak = fill_nobreak_predicate(k)%fn()
                call goto_char(opoint)
                if (nobreak) return
            end do
        end if
    end function fill_nobreak_p

    !> Delete the fill prefix from every line except the first.
    !> The first line may not even have a fill prefix.
    !> Point is moved to just past the fill prefix on the first line.
    !> Port of fill.el:fill-delete-prefix.
    integer(int64) function fill_delete_prefix(from, to, prefix, have_prefix) result(pos)
        integer(int64), intent(in) :: from, to
        character(len=*), intent(in) :: prefix
        logical, intent(in) :: have_prefix
        character(len=:), allocatable :: fpre
        type(status_type) :: st
        type(handle_type) :: to_marker
        integer(int64) :: shortage, me, endpos
        logical :: blank

        blank = .true.
        if (have_prefix) blank = string_match("\`[ "//TAB//"]*\'", prefix, st) > 0

        if (have_prefix .and. .not. blank) then
            fpre = "[ "//TAB//"]*\("//spaces_to_lax(regexp_quote(prefix))//"\)?[ "//TAB//"]*"
        else
            fpre = "[ "//TAB//"]*"
        end if

        ! TO is a marker upstream.
        call copy_marker_at(to, to_marker, .true., st)
        call goto_char(from)
        shortage = forward_line(1_int64)

        do
            call marker_position(to_marker, endpos, st)
            if (point() >= endpos) exit

            if (looking_at(fpre, st)) then
                me = match_end(0, st)
                call delete_region(point(), me, st)
            end if

            shortage = forward_line(1_int64)
        end do

        call free_marker(to_marker, st)
        call goto_char(from)
        if (looking_at(fpre, st)) call goto_char(match_end(0, st))
        pos = point()
    end function fill_delete_prefix

    ! (replace-regexp-in-string "[ \t]+" "[ \t]*" STRING) Not a port.
    function spaces_to_lax(string) result(lax)
        character(len=*), intent(in) :: string
        character(len=:), allocatable :: lax
        integer :: i
        logical :: in_run

        lax = ""
        in_run = .false.

        do i = 1, len(string)
            if (string(i:i) == " " .or. string(i:i) == TAB) then
                if (.not. in_run) lax = lax//"[ "//TAB//"]*"
                in_run = .true.
            else
                lax = lax//string(i:i)
                in_run = .false.
            end if
        end do
    end function spaces_to_lax

    ! fill-nospace-between-words-table: the Han, Kana, Bopomofo and
    ! CJK-misc scripts and the full-width forms.  Not a port.
    logical function nospace_between_words(c)
        integer(int32), intent(in) :: c
        integer :: script

        nospace_between_words = c >= int(z'FF01') .and. c <= int(z'FFE6')
        if (nospace_between_words) return
        script = char_script(int(c))
        if (script <= 0) return

        select case (trim(script_names(script)))
        case ("han", "kana", "bopomofo", "cjk_misc")
            nospace_between_words = .true.
        case default
            nospace_between_words = .false.
        end select
    end function nospace_between_words

    !> Join the lines from FROM to TO into one: make sure sentences ending
    !> at the end of a line get an extra space, delete newlines between
    !> words that are not separated by spaces (CJK), change the other
    !> newlines to spaces and, unless NOSQUEEZE, make the spacing
    !> canonical (from SQUEEZE_AFTER if positive).  Port of
    !> fill.el:fill-delete-newlines.
    subroutine fill_delete_newlines(from, to, justify, nosqueeze, squeeze_after, status)
        integer(int64), intent(in) :: from, to
        integer, intent(in) :: justify
        logical, intent(in) :: nosqueeze
        integer(int64), intent(in) :: squeeze_after
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: eol_double_space_re, se
        type(value_type) :: fill_space
        type(status_type) :: st
        type(handle_type) :: to_marker
        integer(int64) :: pos, mb, endpos, moved
        integer(int32) :: prev, next, c
        logical :: found, found2, join
        integer :: at

        status = status_type()
        ! TO moves with the text we insert and delete.
        call copy_marker_at(to, to_marker, .false., st)
        call goto_char(from)
        ! Make sure sentences ending at end of line get an extra space.
        ! loses on split abbrevs ("Mr.\nSmith")
        se = sentence_end_regexp()

        if (.not. colon_double_space) then
            eol_double_space_re = se//"$"
        else if (string_match("\[[^][]*\(\.\)[^][]*\]", se, st) > 0) then
            ! Try to add the : inside the `sentence_end' regexp.
            at = int(match_beginning(1, st))
            eol_double_space_re = se(1:at - 1)//".:"//se(at + 1:)//"$"
        else
            ! Can't find the right spot to insert the colon.
            eol_double_space_re = "[.?!:][])}""']*$"
        end if

        do
            call marker_position(to_marker, endpos, st)
            call re_search_forward(eol_double_space_re, pos, status, bound=endpos, noerror=SEARCH_NOERROR)
            if (.not. status%ok() .or. pos == 0) exit
            if (point() >= endpos) cycle
            call char_before(c, found)
            if (c == 9 .or. c == 32) cycle
            call char_after(c, found, match_beginning(0, st))

            if (allocated(sentence_end_without_space) .and. found) then
                if (index(sentence_end_without_space, char_of(c)) > 0) cycle
            end if

            call insert(" ", status)
            if (.not. status%ok()) exit
        end do

        if (.not. status%ok()) return
        call goto_char(from)

        ! Delete unnecessary newlines surrounded by words.  The character
        ! category `|' means that we can break a line at the character.
        ! And, char-table `fill_nospace_between_words_table' tells how to
        ! concatenate words.  If a character has non-nil value in the
        ! table, never put spaces between words, thus delete a newline
        ! between them.  Otherwise, delete a newline only when a character
        ! preceding a newline has non-nil value in that table.
        do
            call marker_position(to_marker, endpos, st)
            call search_forward(LF, pos, status, bound=endpos, noerror=SEARCH_NOERROR)
            if (.not. status%ok() .or. pos == 0) exit
            mb = match_beginning(0, st)
            call get_text_property(mb, sym("fill_space"), fill_space, st)

            if (stringp(fill_space)) then
                call replace_match(xstring(fill_space), status, literal=.true.)
                if (.not. status%ok()) exit
            else
                call char_before(prev, found, mb)
                call char_after(next, found2)

                if (found .and. found2) then
                    if (fill_separate_heterogeneous_words_with_space) then
                        join = char_has_category(int(next), "|") .and. char_has_category(int(prev), "|")
                    else
                        join = char_has_category(int(next), "|") .or. char_has_category(int(prev), "|")
                    end if

                    if (join) join = nospace_between_words(next) .or. nospace_between_words(prev)
                    if (join) call delete_char(-1_int64, status)
                end if
            end if
        end do

        if (.not. status%ok()) return
        call marker_position(to_marker, endpos, st)
        call goto_char(from)
        moved = skip_chars_forward(" "//TAB, st)
        ! Then change all newlines to spaces.
        call subst_char_in_region(from, endpos, 10, 32, status)
        if (.not. status%ok()) return

        if (.not. nosqueeze .or. justify == JUSTIFY_FULL) then
            if (squeeze_after > 0) then
                call canonically_space_region(squeeze_after, endpos, status)
            else
                call canonically_space_region(point(), endpos, status)
            end if

            if (.not. status%ok()) return
            ! Remove trailing whitespace.  Maybe canonically-space-region
            ! should do that.
            call marker_position(to_marker, endpos, st)
            call goto_char(endpos)
            moved = skip_chars_backward(" "//TAB, st)
            call delete_region(point(), endpos, status)
        end if

        call goto_char(from)
        call free_marker(to_marker, st)
    end subroutine fill_delete_newlines

    ! The UTF-8 of character C.  Not a port.
    function char_of(c) result(s)
        integer(int32), intent(in) :: c
        character(len=:), allocatable :: s

        if (c < 128) then
            s = achar(c)
        else if (c < 2048) then
            s = char(192 + c/64)//char(128 + mod(c, 64))
        else if (c < 65536) then
            s = char(224 + c/4096)//char(128 + mod(c/64, 64))//char(128 + mod(c, 64))
        else
            s = char(240 + c/262144)//char(128 + mod(c/4096, 64))//char(128 + mod(c/64, 64))//char(128 + mod(c, 64))
        end if
    end function char_of

    !> Move to the position where the line should be broken.
    !> The break position will be always after LINEBEG and generally before point.
    !> Port of fill.el:fill-move-to-break-point (no kinsoku).
    subroutine fill_move_to_break_point(linebeg)
        integer(int64), intent(in) :: linebeg
        type(status_type) :: st
        integer(int64) :: pos, moved, to, brk
        logical :: first, found

        ! If the fill column is before linebeg, move to linebeg.
        if (linebeg > point()) call goto_char(linebeg)

        ! Move back to the point where we can break the line at.  We break
        ! the line between word or after/before the character which has
        ! character category `|'.  We search space, \c| followed by a
        ! character, or \c| following a character.  If not found, place
        ! the point at linebeg.
        do
            call re_search_backward("[ "//TAB//"]\|\c|.\|.\c|", pos, st, bound=linebeg, noerror=SEARCH_MOVE)
            if (pos == 0) exit
            ! In case of space, we place the point at next to the point
            ! where the break occurs actually, because we don't want to
            ! change the following logic of original Emacs.  In case of
            ! \c|, the point is at the place where the break occurs.
            call goto_char(point() + 1)
            if (.not. fill_nobreak_p()) exit
            moved = skip_chars_backward(" "//TAB, st, linebeg)
        end do

        ! Move back over the single space between the words.
        moved = skip_chars_backward(" "//TAB, st)

        ! If the left margin and fill prefix by themselves pass the
        ! fill-column. or if they are zero but we have no room for even one
        ! word, keep at least one word or a character which has category
        ! `|' anyway.
        if (linebeg >= point()) then
            ! Ok, skip at least one word or one \c| character.  Meanwhile,
            ! don't stop at a period followed by one space.
            to = line_end_position()
            first = .true.
            call goto_char(linebeg)

            do while (point() < to)
                if (.not. first) then
                    if (.not. fill_nobreak_p()) exit
                end if

                ! Find a breakable point while ignoring the following
                ! spaces.
                moved = skip_chars_forward(" "//TAB, st)

                if (looking_at("\c|", st)) then
                    call goto_char(point() + 1)
                else
                    pos = point()
                    moved = skip_chars_forward("^ "//LF//TAB, st)
                    brk = point()
                    call goto_char(pos)
                    call re_search_forward("\c|", pos, st, bound=brk, noerror=SEARCH_NOERROR)
                    found = pos > 0

                    if (found) then
                        call goto_char(point() - 1)
                    else
                        call goto_char(brk)
                    end if
                end if

                first = .false.
            end do
        end if
    end subroutine fill_move_to_break_point

    !> Replace whitespace here with one newline, then indent to left
    !> margin, and insert the fill prefix.  Port of fill.el:fill-newline
    !> (no text properties other than `fill_space').
    subroutine fill_newline(prefix, have_prefix, status)
        character(len=*), intent(in) :: prefix
        logical, intent(in) :: have_prefix
        type(status_type), intent(out) :: status
        type(status_type) :: st
        integer(int64) :: moved, me2
        integer(int32) :: c
        logical :: found, mark_space

        moved = skip_chars_backward(" "//TAB, status)
        call insert(LF, status)
        if (.not. status%ok()) return

        if (looking_at("\( ["//TAB//" ]*\)\(\c|\)?", st)) then
            me2 = match_end(2, st)
            call char_before(c, found, point() - 1)
            mark_space = me2 > 0
            if (found) mark_space = mark_space .or. char_has_category(int(c), "|")
            ! When refilling later on, this newline would normally not be
            ! replaced by a space, so we need to mark it specially to
            ! re-install the space when we unfill.
            if (mark_space) call put_text_property(point() - 1, point(), sym("fill_space"), &
                                                   make_string(match_string(1)), st)
        end if

        if (have_prefix .or. .not. fill_indent_according_to_mode) then
            call fill_indent_to_left_margin(status)
        else
            call indent_according_to_mode(status)
        end if

        if (.not. status%ok()) return
        ! Insert the fill prefix after indentation.  Markers that were after
        ! the whitespace are now at point: insert before them so they don't
        ! get stuck before the prefix.
        if (have_prefix .and. len(prefix) > 0) call insert_before_markers(prefix, status)
    end subroutine fill_newline

    !> Indent current line to the column given by `current_left_margin'.
    !> Port of fill.el:fill-indent-to-left-margin.
    subroutine fill_indent_to_left_margin(status)
        type(status_type), intent(out) :: status

        call indent_line_to(current_left_margin(), status)
    end subroutine fill_indent_to_left_margin

    !> Fill the region as if it were a single paragraph.
    !> This command removes any paragraph breaks in the region and
    !> extra newlines at the end, and indents and fills lines between the
    !> margins given by the `current_left_margin' and `current_fill_column'
    !> functions.  (In most cases, the variable `fill_column' controls the
    !> width.)  It leaves point at the beginning of the line following the
    !> region.
    !>
    !> Normally, the command performs justification according to
    !> the `current_justification' function, but with a prefix arg, it
    !> does full justification instead.
    !>
    !> JUSTIFY (a JUSTIFY_ constant; JUSTIFY_T for `current_justification')
    !> can specify any type of justification; see `default_justification'
    !> for the possible values.
    !> Optional fourth arg NOSQUEEZE true means not to make spaces
    !> between words canonical before filling.
    !> Fifth arg SQUEEZE_AFTER, if present, should be a buffer position; it
    !> means canonicalize spaces only starting from that position.
    !> See `canonically_space_region' for the meaning of canonicalization
    !> of spaces.
    !>
    !> Return the `fill_prefix' used for filling (`used', with
    !> `have_used' false for none).
    !>
    !> If `sentence_end_double_space' is true, then period followed by one
    !> space does not end a sentence, so don't break a line there.  Port of
    !> fill.el:fill-region-as-paragraph-default (with
    !> fill-region-as-paragraph; JUSTIFY nil is JUSTIFY_T, as the
    !> `current_justification' default).
    recursive subroutine fill_region_as_paragraph(from_in, to_in, status, justify, nosqueeze, squeeze_after, &
                                                  used, have_used)
        integer(int64), intent(in) :: from_in, to_in
        type(status_type), intent(out) :: status
        integer, intent(in), optional :: justify
        logical, intent(in), optional :: nosqueeze
        integer(int64), intent(in), optional :: squeeze_after
        character(len=:), allocatable, intent(out), optional :: used
        logical, intent(out), optional :: have_used
        type(handle_type) :: to_marker, opos_marker
        type(status_type) :: st
        character(len=:), allocatable :: prefix
        logical :: have_prefix, oneleft, squeeze_none
        integer(int64) :: from, to, from_plus_indent, moved, linebeg, shortage, sq, col, opos
        integer(int32) :: c
        logical :: found, cut
        integer :: how

        call init_fill_variables()
        status = status_type()
        if (present(have_used)) have_used = .false.
        if (present(used)) used = ""
        how = JUSTIFY_T
        if (present(justify)) how = justify
        squeeze_none = .false.
        if (present(nosqueeze)) squeeze_none = nosqueeze
        sq = 0
        if (present(squeeze_after)) sq = squeeze_after
        ! Make sure "to" is the endpoint.
        call goto_char(min(from_in, to_in))
        to = max(from_in, to_in)
        from = min(from_in, to_in)
        ! Ignore blank lines at beginning of region.
        moved = skip_chars_forward(" "//TAB//LF, st)
        from_plus_indent = point()
        call beginning_of_line()
        ! We used to round up to whole line, but that prevents us from
        ! correctly handling filling of mixed code-and-comment where we do
        ! want to fill the comment but not the code.  So only use (point)
        ! if it's further than `from', which means that `from' is followed
        ! by some number of empty lines.
        from = max(point(), from)
        ! Delete all but one soft newline at end of region.  And leave TO
        ! before that one.
        call goto_char(to)
        oneleft = .false.

        do while (point() > from)
            call char_before(c, found)
            if (.not. found .or. c /= 10) exit

            if (oneleft) then
                call delete_char(-1_int64, status)
                if (.not. status%ok()) return
            else
                call goto_char(point() - 1)
                oneleft = .true.
            end if
        end do

        call copy_marker_at(point(), to_marker, .true., st)
        call goto_char(from_plus_indent)
        call marker_position(to_marker, to, st)

        if (.not. (to > point())) then
            ! There is no paragraph, only whitespace: exit now.
            call free_marker(to_marker, st)
            return
        end if

        if (how == JUSTIFY_T) how = current_justification()
        ! Don't let Adaptive Fill mode alter the fill prefix permanently.
        have_prefix = allocated(fill_prefix%value)
        prefix = ""
        if (have_prefix) prefix = fill_prefix%value

        ! Figure out how this paragraph is indented, if desired.
        if (adaptive_fill_mode%value .and. (.not. have_prefix .or. len(prefix) == 0)) then
            call fill_context_prefix(from, to, prefix, have_prefix)

            ! Ignore a white-space only fill-prefix if we
            ! indent-according-to-mode.
            if (have_prefix .and. fill_indent_according_to_mode) then
                if (string_match("\`[ "//TAB//"]*\'", prefix, st) > 0) have_prefix = .false.
            end if
        end if

        if (.not. have_prefix) prefix = ""
        call goto_char(from)
        call beginning_of_line()

        if (how == JUSTIFY_NONE) then
            ! filling disabled: just check indentation
            call goto_char(from)

            do
                call marker_position(to_marker, to, st)
                if (point() >= to) exit

                if (.not. eolp()) then
                    if (current_indentation() < current_left_margin()) call fill_indent_to_left_margin(status)
                end if

                shortage = forward_line(1_int64)
            end do
        else
            ! Make sure first line is indented (at least) to left margin...
            if (how == JUSTIFY_RIGHT .or. how == JUSTIFY_CENTER) then
                call fill_indent_to_left_margin(status)
            else if (current_indentation() < current_left_margin()) then
                call fill_indent_to_left_margin(status)
            end if

            if (.not. status%ok()) return
            ! Delete the fill-prefix from every line.
            call marker_position(to_marker, to, st)
            from = fill_delete_prefix(from, to, prefix, have_prefix)
            ! FROM, and point, are now before the text to fill, but after
            ! any fill prefix on the first line.
            call marker_position(to_marker, to, st)
            call fill_delete_newlines(from, to, how, squeeze_none, sq, status)
            if (.not. status%ok()) return
            ! This is the actual filling loop.
            call goto_char(from)

            do
                call marker_position(to_marker, to, st)
                if (point() >= to) exit
                linebeg = point()
                call move_to_column(current_fill_column(), col)
                cut = .false.

                if (point() < to .and. linebeg < to) then
                    ! Find the position where we'll break the line.  Use an
                    ! immediately following space, if any.  However, note
                    ! that `move_to_column' may overshoot if there are wide
                    ! characters (Bug#3234).
                    if (.not. (current_column() > current_fill_column())) call goto_char(point() + 1)
                    call fill_move_to_break_point(linebeg)
                    ! Check again to see if we got to the end of the
                    ! paragraph.
                    moved = skip_chars_forward(" "//TAB, st)
                    call marker_position(to_marker, to, st)
                    cut = point() < to
                end if

                if (cut) then
                    ! Found a place to cut.
                    call fill_newline(prefix, have_prefix, status)
                    if (.not. status%ok()) return

                    if (how /= JUSTIFY_NONE) then
                        ! Justify the line just ended, if desired.
                        ! (save-excursion)
                        call copy_marker_at(point(), opos_marker, .false., st)
                        shortage = forward_line(-1_int64)
                        call justify_current_line(status, how, .false., .true.)
                        call marker_position(opos_marker, opos, st)
                        call goto_char(opos)
                        call free_marker(opos_marker, st)
                        if (.not. status%ok()) return
                    end if
                else
                    call marker_position(to_marker, to, st)
                    call goto_char(to)
                    ! Justify this last line, if desired.
                    call justify_current_line(status, how, .true., .true.)
                    if (.not. status%ok()) return
                    exit
                end if
            end do
        end if

        ! Leave point after final newline.
        call marker_position(to_marker, to, st)
        call goto_char(to)
        if (.not. eobp()) call goto_char(point() + 1)
        call free_marker(to_marker, st)
        ! Return the fill-prefix we used
        if (present(used)) used = prefix
        if (present(have_used)) have_used = have_prefix
    end subroutine fill_region_as_paragraph

    !> Fill paragraph at or after point.
    !>
    !> If JUSTIFY is present (interactively, with prefix argument), justify as well.
    !> If `sentence_end_double_space' is true, then period followed by one
    !> space does not end a sentence, so don't break a line there.
    !> The variable `fill_column' controls the width for filling.
    !>
    !> Return the `fill_prefix' used for filling (`used').
    !>
    !> The REGION argument is true if called interactively; in that
    !> case, if Transient Mark mode is enabled and the mark is active,
    !> call `fill_region' to fill each of the paragraphs in the active
    !> region, instead of just filling the current paragraph.  Port of
    !> fill.el:fill-paragraph (no fill-paragraph-function or comments;
    !> JUSTIFY is a JUSTIFY_ constant, JUSTIFY_T for nil).
    recursive subroutine fill_paragraph(status, justify, region, used)
        type(status_type), intent(out) :: status
        integer, intent(in), optional :: justify
        logical, intent(in), optional :: region
        character(len=:), allocatable, intent(out), optional :: used
        character(len=:), allocatable :: before_text, after_text
        logical :: modified
        type(status_type) :: st

        ! (with-buffer-unmodified-if-unchanged ...)
        call buffer_modified_p(current_buffer(), modified, st)
        if (.not. modified) before_text = whole_buffer_text()
        call fill_paragraph_1(status, justify, region, used)

        if (allocated(before_text) .and. cur > 0) then
            call buffer_modified_p(current_buffer(), modified, st)

            if (modified) then
                after_text = whole_buffer_text()

                if (after_text == before_text .and. len(after_text) == len(before_text)) then
                    call restore_buffer_modified_p(.false.)
                end if
            end if
        end if
    end subroutine fill_paragraph

    !> Fill a paragraph in the minibuffer, ignoring the prompt.  Port of
    !> fill.el:fill-minibuffer-function.
    recursive subroutine fill_minibuffer_function(status, justify, used)
        type(status_type), intent(out) :: status
        integer, intent(in), optional :: justify
        character(len=:), allocatable, intent(out) :: used
        integer(int64) :: savedb, savedz
        type(status_type) :: st

        savedb = buffers(cur)%begv
        savedz = buffers(cur)%text%z - buffers(cur)%zv
        call narrow_to_region(minibuffer_prompt_end(), point_max(), st)
        call fill_paragraph(status, justify, used=used)
        call widen()
        call narrow_to_region(savedb, buffers(cur)%text%z - savedz, st)
    end subroutine fill_minibuffer_function

    !> The bytes of the whole current buffer, ignoring narrowing (what
    !> subr-x.el's with-buffer-unmodified-if-unchanged compares with
    !> `buffer_hash').  Not a port.
    function whole_buffer_text() result(text)
        character(len=:), allocatable :: text
        integer(int64) :: savedb, savedz
        type(status_type) :: st

        savedb = buffers(cur)%begv
        savedz = buffers(cur)%text%z - buffers(cur)%zv
        call widen()
        call buffer_substring(point_min(), point_max(), text, st)
        call narrow_to_region(savedb, buffers(cur)%text%z - savedz, st)
    end function whole_buffer_text

    !> The body of fill_paragraph.  Port of fill.el:fill-paragraph, in part.
    recursive subroutine fill_paragraph_1(status, justify, region, used)
        type(status_type), intent(out) :: status
        integer, intent(in), optional :: justify
        logical, intent(in), optional :: region
        character(len=:), allocatable, intent(out), optional :: used
        type(handle_type) :: saved
        integer(int64) :: before, end, beg, b, e
        type(status_type) :: st
        character(len=:), allocatable :: pfx
        integer :: how
        procedure(fill_paragraph_proc), pointer :: fn
        logical :: done, in_minibuffer, saved_handle
        character(len=:), allocatable :: paragraph_start_orig, skip
        integer(int64) :: shortage

        call init_fill_variables()
        status = status_type()
        how = JUSTIFY_T
        if (present(justify)) how = justify
        pfx = ""

        ! 1. Fill the region if it is active when called interactively.
        if (present(region)) then
            if (region .and. transient_mark_mode%value .and. buffers(cur)%mark_active) then
                call region_beginning(b, st)
                call region_end(e, st)

                if (b /= e) then
                    call fill_region(b, e, status, how, used=pfx)
                    if (present(used)) used = pfx
                    return
                end if
            end if
        end if

        ! 2. Try fill-paragraph-function.
        if (.not. in_fill_paragraph_function) then
            fn => fill_paragraph_function%get()
            in_minibuffer = .false.
            if (.not. associated(fn) .and. minibufferp()) in_minibuffer = point_min() == 1

            if (associated(fn) .or. in_minibuffer) then
                ! If fill-paragraph-function is set, it probably takes care
                ! of comments and stuff.  If not, it will have to set
                ! fill-paragraph-handle-comment back to t explicitly or
                ! return nil.
                saved_handle = fill_paragraph_handle_comment
                fill_paragraph_handle_comment = .false.
                in_fill_paragraph_function = .true.

                if (associated(fn)) then
                    call fn(status, done, justify)
                else
                    ! In the minibuffer, don't count the width of the
                    ! prompt.
                    call fill_minibuffer_function(status, justify, pfx)
                    done = .true.
                end if

                in_fill_paragraph_function = .false.
                fill_paragraph_handle_comment = saved_handle

                if (done .or. .not. status%ok()) then
                    if (present(used)) used = pfx
                    return
                end if
            end if
        end if

        ! 3. Try our syntax-aware filling code.
        if (fill_paragraph_handle_comment) then
            ! Our code only handles \n-terminated comments right now.
            if (fill_comment_syntax_p()) then
                saved_handle = fill_paragraph_handle_comment
                fill_paragraph_handle_comment = .false.
                call fill_comment_paragraph(status, done, justify)
                fill_paragraph_handle_comment = saved_handle

                if (done .or. .not. status%ok()) then
                    if (present(used)) used = pfx
                    return
                end if
            end if
        end if

        ! 4. If it all fails, default to the good ol' text paragraph
        ! filling.
        before = point()
        call init_paragraph_variables()
        paragraph_start_orig = paragraph_start%value
        skip = ""
        if (fill_paragraph_handle_comment) skip = fill_comment_start_skip()
        ! Try to prevent code sections and comment sections from being
        ! filled together.
        if (len(skip) > 0) paragraph_start%value = paragraph_start_orig//"\|[ "//TAB//"]*\(?:"//skip//"\)"
        ! To make sure the return value of forward-paragraph is
        ! meaningful, we have to start from the beginning of line,
        ! otherwise skipping past the last few chars of a
        ! paragraph-separator would count as a paragraph (and not skipping
        ! any chars at EOB would not count as a paragraph even if it is).
        call save_point(saved)
        call move_to_left_margin(st)

        if (forward_paragraph(1_int64) /= 0) then
            ! There's no paragraph at or after point: give up.
            pfx = ""
        else
            end = point()
            if (forward_paragraph(-1_int64) /= 0) continue
            beg = point()

            ! If the paragraph starts with a comment line preceding point
            ! on a non-comment line, skip such comment lines, so they
            ! are not filled together (bug#80449).
            if (len(skip) > 0 .and. beg < before) then
                call goto_char(beg)
                if (looking_at(paragraph_start_orig, st)) call goto_char(match_end(0, st) + 1)

                if (looking_at(skip, st)) then
                    shortage = forward_line(1_int64)
                    beg = point()
                end if
            end if

            call goto_char(before)
            call fill_region_as_paragraph(beg, end, status, how, used=pfx)
        end if

        paragraph_start%value = paragraph_start_orig
        call restore_point(saved)
        if (present(used)) used = pfx
    end subroutine fill_paragraph_1

    !> Join lines of this paragraph and fix up whitespace at joins.
    !> Interactively, if the region is active, join lines of each paragraph in
    !> the region.  A numeric prefix argument means join the lines of the
    !> following ARG paragraphs.  In this case an active region is ignored.
    !>
    !> With an active region and no prefix argument this is roughly the same as
    !> `delete_indentation' with that active region, except that this command
    !> only joins lines within paragraphs, preserving the paragraphs
    !> themselves.
    !>
    !> When called from Fortran, ARG is the number of following paragraphs to join
    !> lines within, or if ARG is 0, optional arguments BEG and END present
    !> means to join the lines of each paragraph in the region delimited by BEG
    !> and END.  Port of fill.el:unfill-paragraph (ARG 0 for nil).
    recursive subroutine unfill_paragraph(arg, status, beg, end)
        integer(int64), intent(in) :: arg
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: beg, end
        integer(int64) :: b, e, opoint, left, saved

        status = status_type()

        if (present(beg) .and. present(end) .and. arg == 0) then
            b = beg
            e = end
        else
            if (arg == 0) then
                call set_error(status, FM_USER_ERROR, format_message("Invalid numeric argument to `unfill_paragraph'"))
                return
            end if

            opoint = point()
            left = forward_paragraph(1_int64)
            left = forward_paragraph(-1_int64)
            b = point()
            left = forward_paragraph(arg)
            e = point()
            call goto_char(opoint)
        end if

        saved = fill_column%value
        fill_column = huge(fill_column%value)/4
        call fill_region(b, e, status)
        fill_column = saved
    end subroutine unfill_paragraph

    !> Fill each of the paragraphs in the region.
    !> A prefix arg means justify as well.
    !> The `fill_column' variable controls the width.
    !>
    !> Noninteractively, the third argument JUSTIFY specifies which
    !> kind of justification to do: JUSTIFY_FULL, JUSTIFY_LEFT, JUSTIFY_RIGHT,
    !> JUSTIFY_CENTER, or JUSTIFY_NONE.  A value of JUSTIFY_T (or absent) means handle each
    !> paragraph as specified by its text properties.
    !>
    !> The fourth arg NOSQUEEZE true means to leave whitespace other
    !> than line breaks untouched, and fifth arg TO_EOP true means
    !> to keep filling to the end of the paragraph (or next hard newline,
    !> if variable `use_hard_newlines' is on).
    !>
    !> Return the `fill_prefix' used for filling the last paragraph.
    !>
    !> If `sentence_end_double_space' is true, then period followed by one
    !> space does not end a sentence, so don't break a line there.  Port of
    !> fill.el:fill-region (JUSTIFY_T for nil).
    recursive subroutine fill_region(from, to, status, justify, nosqueeze, to_eop, used)
        integer(int64), intent(in) :: from, to
        type(status_type), intent(out) :: status
        integer, intent(in), optional :: justify
        logical, intent(in), optional :: nosqueeze, to_eop
        character(len=:), allocatable, intent(out), optional :: used
        type(handle_type) :: start_point, max_marker
        type(status_type) :: st
        character(len=:), allocatable :: pfx
        integer(int64) :: maxpos, beg, initial, end, left, moved, pos
        integer :: how
        logical :: sq

        status = status_type()
        how = JUSTIFY_T
        if (present(justify)) how = justify
        sq = .false.
        if (present(nosqueeze)) sq = nosqueeze
        pfx = ""
        call point_marker(start_point)
        call goto_char(max(from, to))

        if (present(to_eop)) then
            if (to_eop) then
                moved = skip_chars_backward(LF, st)
                left = forward_paragraph(1_int64)
            end if
        end if

        call copy_marker_at(point(), max_marker, .true., st)
        beg = min(from, to)
        call goto_char(beg)
        call beginning_of_line()

        do
            call marker_position(max_marker, maxpos, st)
            if (point() >= maxpos) exit
            initial = point()
            left = forward_paragraph(1_int64)
            end = min(maxpos, point())
            left = forward_paragraph(-1_int64)
            if (point() < beg) call goto_char(beg)

            if (point() >= initial .and. point() < end) then
                call fill_region_as_paragraph(point(), end, status, how, sq, used=pfx)
                if (.not. status%ok()) exit
            else
                call goto_char(end)
            end if
        end do

        call marker_position(start_point, pos, st)
        call goto_char(pos)
        call free_marker(start_point, st)
        call free_marker(max_marker, st)
        if (present(used)) used = pfx
    end subroutine fill_region

    !> How should we justify this line?
    !> This returns the value of the text-property `justification',
    !> or the variable `default_justification' if there is no text-property.
    !> However, it returns JUSTIFY_NONE to mean "don't justify".
    !> Port of fill.el:current-justification (JUSTIFY_NONE for nil).
    integer function current_justification() result(j)
        type(value_type) :: v
        type(status_type) :: st
        character(len=:), allocatable :: name
        integer(int64) :: opoint, moved, pos

        opoint = point()
        ! Make sure we're looking at paragraph body.
        moved = skip_chars_forward(" "//TAB, st)
        pos = point()
        if (eobp() .and. .not. bobp()) pos = pos - 1
        call goto_char(opoint)
        call get_text_property(pos, sym("justification"), v, st)
        j = default_justification

        if (symbolp(v) .and. .not. nilp(v)) then
            call symbol_name(v, name, st)

            select case (name)
            case ("left")
                j = JUSTIFY_LEFT
            case ("right")
                j = JUSTIFY_RIGHT
            case ("full")
                j = JUSTIFY_FULL
            case ("center")
                j = JUSTIFY_CENTER
            case default
                j = JUSTIFY_NONE
            end select
        end if
    end function current_justification

    !> Do some kind of justification on this line.
    !> Normally does full justification: adds spaces to the line to make it end at
    !> the column given by `current_fill_column'.
    !> Optional first argument HOW specifies alternate type of justification:
    !> it can be JUSTIFY_LEFT, JUSTIFY_RIGHT, JUSTIFY_FULL, JUSTIFY_CENTER, or JUSTIFY_NONE; for their
    !> meaning, see `default_justification'.
    !> If HOW is JUSTIFY_T, will justify however the `current_justification' function says to.
    !> If HOW is absent, full justification is done by default.
    !> Second arg EOP true means that this is the last line of the paragraph, so
    !> it will not be stretched by full justification.
    !> Third arg NOSQUEEZE true means to leave interior whitespace unchanged,
    !> otherwise it is made canonical.  Port of fill.el:justify-current-line.
    subroutine justify_current_line(status, how_in, eop, nosqueeze)
        type(status_type), intent(out) :: status
        integer, intent(in), optional :: how_in
        logical, intent(in), optional :: eop, nosqueeze
        type(handle_type) :: pos_marker
        type(status_type) :: st
        integer(int64) :: fc, fp_end, beg, end, indent, endcol, ncols, nspaces, curr_fracspace, count, moved, &
                          pos, col, savedb, savedz, n
        integer :: how
        logical :: is_eop, no_squeeze

        status = status_type()
        how = JUSTIFY_FULL
        if (present(how_in)) how = how_in
        if (how == JUSTIFY_T) how = current_justification()
        is_eop = .false.
        if (present(eop)) is_eop = eop
        no_squeeze = .false.
        if (present(nosqueeze)) no_squeeze = nosqueeze
        ! No action required for these.
        if (how == JUSTIFY_NONE .or. how == JUSTIFY_LEFT) return
        call init_fill_variables()
        fc = current_fill_column()
        call point_marker(pos_marker)
        call end_of_line()
        moved = skip_chars_backward(" "//TAB, st)

        ! Quick exit if it appears to be properly justified already or
        ! there is no text.
        if (bolp() .or. ((how == JUSTIFY_FULL .or. how == JUSTIFY_RIGHT) .and. current_column() == fc)) then
            call finish()
            return
        end if

        end = point()
        call beginning_of_line()
        moved = skip_chars_forward(" "//TAB, st)

        ! Skip over fill-prefix.
        if (prefix_at_point()) then
            call goto_char(point() + nchars(fill_prefix%value))
        else if (adaptive_fill_mode%value .and. allocated(adaptive_fill_regexp)) then
            if (looking_at(adaptive_fill_regexp, st)) call goto_char(match_end(0, st))
        end if

        fp_end = point()
        moved = skip_chars_forward(" "//TAB, st)
        ! This is beginning of the line's text.
        indent = current_column()
        beg = point()
        call goto_char(end)
        endcol = current_column()

        select case (how)
        case (JUSTIFY_RIGHT)
            ncols = fc - endcol

            if (ncols < 0) then
                ! Need to remove some indentation
                call goto_char(fp_end)
                if (current_column() < indent + ncols) call move_to_column(indent + ncols, col, .true., st)
                pos = point()
                call move_to_column(indent, col)
                call delete_region(pos, point(), status)
            else
                ! Need to add some
                call goto_char(beg)
                col = indent_to(indent + ncols, status)
                ! If point was at beginning of text, keep it there.
                call marker_position(pos_marker, pos, st)
                if (beg == pos) call set_pos(point())
            end if
        case (JUSTIFY_CENTER)
            ! Figure out how much indentation is needed
            ncols = current_left_margin() + (fc - current_left_margin() - (endcol - indent))/2

            if (ncols < indent) then
                ! Have too much indentation - remove some
                call goto_char(fp_end)
                if (current_column() < ncols) call move_to_column(ncols, col, .true., st)
                pos = point()
                call move_to_column(indent, col)
                call delete_region(pos, point(), status)
            else
                ! Have too little - add some
                call goto_char(beg)
                col = indent_to(ncols, status)
                ! If point was at beginning of text, keep it there.
                call marker_position(pos_marker, pos, st)
                if (beg == pos) call set_pos(point())
            end if
        case default
            ! Insert extra spaces between words to justify line
            ! (save-restriction: the end of the restriction keeps its
            ! distance from the end of the buffer).
            savedb = buffers(cur)%begv
            savedz = buffers(cur)%text%z - buffers(cur)%zv
            call narrow_to_region(beg, end, st)
            if (.not. no_squeeze) call canonically_space_region(beg, end, status)
            call goto_char(point_max())
            ! count word spaces in line
            nspaces = 0

            do
                call search_backward(" ", pos, st, noerror=SEARCH_NOERROR)
                if (pos == 0) exit
                nspaces = nspaces + 1
                moved = skip_chars_backward(" ", st)
            end do

            ncols = fc - endcol

            ! Ncols is number of additional space chars needed
            if (ncols > 0 .and. nspaces > 0 .and. .not. is_eop) then
                curr_fracspace = ncols + nspaces/2
                count = nspaces

                do while (count > 0)
                    moved = skip_chars_forward(" ", st)
                    n = curr_fracspace/nspaces
                    if (n > 0) call insert(repeat(" ", int(n)), status)
                    call search_forward(" ", pos, st, noerror=SEARCH_NOERROR)
                    count = count - 1
                    curr_fracspace = mod(curr_fracspace, nspaces) + ncols
                end do
            end if

            call widen()
            call narrow_to_region(savedb, buffers(cur)%text%z - savedz, st)
        end select

        call finish()

    contains

        subroutine finish()
            integer(int64) :: p

            call marker_position(pos_marker, p, st)
            call goto_char(p)
            call free_marker(pos_marker, st)
        end subroutine finish

        subroutine set_pos(p)
            integer(int64), intent(in) :: p

            call free_marker(pos_marker, st)
            call copy_marker_at(p, pos_marker, .false., st)
        end subroutine set_pos
    end subroutine justify_current_line

    !> Whether point is before a non-empty `fill-prefix`.  Not a port.
    logical function prefix_at_point() result(at)
        character(len=:), allocatable :: text
        type(status_type) :: st

        at = .false.
        if (.not. allocated(fill_prefix%value)) return
        if (len(fill_prefix%value) == 0) return
        call buffer_substring(point(), min(point_max(), point() + nchars(fill_prefix%value)), text, st)
        at = text == fill_prefix%value .and. len(text) == len(fill_prefix%value)
    end function prefix_at_point

    !> The number of characters of the UTF-8 string S.  Not a port.
    pure integer(int64) function nchars(s)
        character(len=*), intent(in) :: s
        integer :: i

        nchars = 0

        do i = 1, len(s)
            if (iand(iachar(s(i:i)), 192) /= 128) nchars = nchars + 1
        end do
    end function nchars

    !> Remove justification whitespace from current line.
    !> If the line is centered or right-justified, this function removes any
    !> indentation past the left margin.  If the line is full-justified, it removes
    !> extra spaces between words.  It does nothing in other justification modes.
    !> Port of fill.el:unjustify-current-line.
    subroutine unjustify_current_line(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: saved
        integer(int64) :: moved, from
        type(status_type) :: st

        status = status_type()

        select case (current_justification())
        case (JUSTIFY_FULL)
            ! full justify: remove extra spaces
            call beginning_of_line_text()
            call canonically_space_region(point(), line_end_position(), status)
        case (JUSTIFY_CENTER, JUSTIFY_RIGHT)
            call save_point(saved)
            call move_to_left_margin(st, force=.true.)
            ! Position ourselves after any fill-prefix.
            if (prefix_at_point()) call goto_char(point() + nchars(fill_prefix%value))
            from = point()
            moved = skip_chars_forward(" "//TAB, st)
            call delete_region(from, point(), status)
            call restore_point(saved)
        case default
            continue
        end select
    end subroutine unjustify_current_line

    !> Remove justification whitespace from region.
    !> For centered or right-justified regions, this function removes any indentation
    !> past the left margin from each line.  For full-justified lines, it removes
    !> extra spaces between words.  It does nothing in other justification modes.
    !> Arguments BEGIN and END are optional; default is the whole buffer.
    !> Port of fill.el:unjustify-region.
    subroutine unjustify_region(status, begin, end)
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: begin, end
        integer(int64) :: savedb, savedz, shortage
        type(handle_type) :: saved
        type(status_type) :: st

        status = status_type()
        call save_point(saved)
        savedb = buffers(cur)%begv
        savedz = buffers(cur)%text%z - buffers(cur)%zv
        if (present(end)) call narrow_to_region(point_min(), end, st)

        if (present(begin)) then
            call goto_char(begin)
        else
            call goto_char(point_min())
        end if

        do while (.not. eobp())
            call unjustify_current_line(status)
            if (.not. status%ok()) exit
            shortage = forward_line(1_int64)
        end do

        call widen()
        call narrow_to_region(savedb, buffers(cur)%text%z - savedz, st)
        call restore_point(saved)
    end subroutine unjustify_region

    !> Start a `save_excursion': remember point in marker SAVED.
    !> Not a port.
    subroutine save_point(saved)
        type(handle_type), intent(out) :: saved
        type(status_type) :: st

        call copy_marker_at(point(), saved, .false., st)
    end subroutine save_point

    !> End a `save_excursion': go back to SAVED and free it.  Not a port.
    subroutine restore_point(saved)
        type(handle_type), intent(inout) :: saved
        type(status_type) :: st
        integer(int64) :: pos

        call marker_position(saved, pos, st)
        call goto_char(pos)
        call free_marker(saved, st)
    end subroutine restore_point

    ! ------------------------------------------------------------------
    ! Commands
    ! ------------------------------------------------------------------

    ! Not a port.
    integer function prefix_justify() result(how)
        how = JUSTIFY_T
        if (.not. nilp(current_prefix_arg)) how = JUSTIFY_FULL
    end function prefix_justify

    recursive subroutine cmd_fill_paragraph(status)
        type(status_type), intent(out) :: status

        call barf_if_buffer_read_only(status)
        if (status%ok()) call fill_paragraph(status, prefix_justify(), .true.)
    end subroutine cmd_fill_paragraph

    recursive subroutine cmd_fill_region(status)
        type(status_type), intent(out) :: status
        integer(int64) :: b, e

        call barf_if_buffer_read_only(status)
        if (.not. status%ok()) return
        call region_beginning(b, status)
        if (.not. status%ok()) return
        call region_end(e, status)
        call fill_region(b, e, status, prefix_justify())
    end subroutine cmd_fill_region

    !> Fill the region as if it were a single paragraph.
    !> The implementation is `fill_region_as_paragraph_default'.
    !>
    !> The arguments FROM and TO define the boundaries of the region.
    !>
    !> The optional third argument JUSTIFY, when called interactively with a
    !> prefix arg, is assigned the value JUSTIFY_FULL.
    !> When called from Fortran, JUSTIFY can specify any type of justification;
    !> see `default_justification' for the possible values.
    !> Optional fourth arg NOSQUEEZE true means not to make spaces between
    !> words canonical before filling.
    !> Fifth arg SQUEEZE_AFTER, if present, should be a buffer position; it
    !> means canonicalize spaces only starting from that position.
    !> See `canonically_space_region' for the meaning of canonicalization of
    !> spaces.
    !>
    !> It returns the `fill_prefix' used for filling.  Port of
    !> fill.el:fill-region-as-paragraph (the command;
    !> `fill_region_as_paragraph_function' is not ported, so it is always
    !> `fill_region_as_paragraph_default').
    recursive subroutine cmd_fill_region_as_paragraph(status)
        type(status_type), intent(out) :: status
        integer(int64) :: b, e

        call barf_if_buffer_read_only(status)
        if (.not. status%ok()) return
        call region_beginning(b, status)
        if (.not. status%ok()) return
        call region_end(e, status)
        call fill_region_as_paragraph(b, e, status, prefix_justify())
    end subroutine cmd_fill_region_as_paragraph

    recursive subroutine cmd_unfill_paragraph(status)
        type(status_type), intent(out) :: status
        integer(int64) :: b, e

        if (nilp(current_prefix_arg) .and. use_region_p()) then
            call region_beginning(b, status)
            call region_end(e, status)
            call unfill_paragraph(0_int64, status, b, e)
        else
            call unfill_paragraph(prefix_numeric_value(current_prefix_arg), status)
        end if
    end subroutine cmd_unfill_paragraph

    subroutine cmd_set_fill_prefix(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call set_fill_prefix(.not. nilp(current_prefix_arg))
    end subroutine cmd_set_fill_prefix

    subroutine cmd_justify_current_line(status)
        type(status_type), intent(out) :: status

        call barf_if_buffer_read_only(status)
        if (status%ok()) call justify_current_line(status)
    end subroutine cmd_justify_current_line

    !> Give the variables their defaults and register the commands.
    !> Not a port.
    subroutine fill_paragraph_set_local(self, fn)
        class(fill_paragraph_function_type), intent(inout) :: self
        procedure(fill_paragraph_proc) :: fn
        type(fill_paragraph_entry_type), allocatable :: grown(:)
        integer :: i, n

        if (cur == 0) return
        if (.not. allocated(self%entries)) allocate (self%entries(0))

        do i = 1, size(self%entries)
            if (self%entries(i)%serial == buffers(cur)%serial) then
                self%entries(i)%fn => fn
                return
            end if
        end do

        ! Reuse the entry of a dead buffer.
        do i = 1, size(self%entries)
            if (.not. buffer_serial_live(self%entries(i)%serial)) then
                self%entries(i)%serial = buffers(cur)%serial
                self%entries(i)%fn => fn
                return
            end if
        end do

        n = size(self%entries)
        allocate (grown(n + 1))

        do i = 1, n
            grown(i)%serial = self%entries(i)%serial
            grown(i)%fn => self%entries(i)%fn
        end do

        grown(n + 1)%serial = buffers(cur)%serial
        grown(n + 1)%fn => fn
        call move_alloc(grown, self%entries)
    end subroutine fill_paragraph_set_local

    ! Not a port.
    function fill_paragraph_get(self) result(fn)
        class(fill_paragraph_function_type), intent(in) :: self
        procedure(fill_paragraph_proc), pointer :: fn
        integer :: i

        fn => self%value
        if (cur == 0) return
        if (.not. allocated(self%entries)) return

        do i = 1, size(self%entries)
            if (self%entries(i)%serial == buffers(cur)%serial) then
                if (associated(self%entries(i)%fn)) fn => self%entries(i)%fn
            end if
        end do
    end function fill_paragraph_get

    ! Not a port.
    subroutine fill_paragraph_kill_local(self)
        class(fill_paragraph_function_type), intent(inout) :: self
        integer :: i

        if (cur == 0) return
        if (.not. allocated(self%entries)) return

        do i = 1, size(self%entries)
            if (self%entries(i)%serial == buffers(cur)%serial) then
                self%entries(i)%serial = 0
                self%entries(i)%fn => null()
            end if
        end do
    end subroutine fill_paragraph_kill_local

    !> kill-all-local-variables: forget the current buffer's
    !> `fill_paragraph_function'.  Not a port.
    subroutine kill_fill_locals()
        call fill_paragraph_function%kill_local()
    end subroutine kill_fill_locals

    !> Set up fill.el's variables and commands.  Not a port: upstream
    !> defines them where fill.el is loaded.
    subroutine syms_of_fill()
        call init_fill_variables()
        call add_local_variables_killer(kill_fill_locals)
        commands%fill_paragraph%run => cmd_fill_paragraph
        commands%fill_region%run => cmd_fill_region
        commands%fill_region_as_paragraph%run => cmd_fill_region_as_paragraph
        commands%unfill_paragraph%run => cmd_unfill_paragraph
        commands%set_fill_prefix%run => cmd_set_fill_prefix
        commands%justify_current_line%run => cmd_justify_current_line
    end subroutine syms_of_fill
end module fill_fm
