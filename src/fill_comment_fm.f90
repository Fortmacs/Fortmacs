! fill_comment_fm.f90 -- filling comments, a submodule of fill_fm.
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

!> `fill_comment_paragraph' and what `fill_paragraph' asks of the
!> comment variables: a submodule because it uses newcomment_fm, which
!> uses fill_fm.
submodule(fill_fm) fill_comment_fm
    use lisp_fm, only: vtag, V_STRING, value_type
    use newcomment_fm, only: comment_start, comment_end, comment_start_skip, comment_normalize_vars, &
                             comment_search_forward, comment_string_strip
    use paragraphs_fm, only: paragraph_separate
    use data_fm, only: default_value
    use xdisp_fm, only: tab_width
    use indent_fm, only: indent_tabs_mode
    implicit none(type, external)

contains

    module procedure is_fill_comment_syntax
    is_fill_comment_syntax = .false.
    if (.not. allocated(comment_start%value)) return
    if (.not. allocated(comment_end%value)) return
    is_fill_comment_syntax = len(comment_end%value) == 0
    end procedure is_fill_comment_syntax

    module procedure fill_comment_start_skip
    skip = ""
    if (allocated(comment_start_skip%value)) skip = comment_start_skip%value
    end procedure fill_comment_start_skip

    !> The text from FROM to TO.  Not a port.
    function text_of(from, to) result(s)
        integer(int64), intent(in) :: from, to
        character(len=:), allocatable :: s
        type(status_type) :: st

        call buffer_substring(min(from, to), max(from, to), s, st)
        if (.not. allocated(s)) s = ""
    end function text_of

    !> The default (global) value of the string variable NAME, "" if it is
    !> not a string.  Not a port.
    function default_string(name) result(s)
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: s
        type(value_type) :: v
        type(status_type) :: st

        s = ""
        call default_value(value_type(name=name), v, st)
        if (st%ok() .and. vtag(v) == V_STRING) s = xstring(v)
    end function default_string

    module procedure fill_comment_paragraph
    integer(int64) :: opoint, comstart, comin, moved, nstart, nend, after_line, beg, end, cs, shortage, squeeze, found, col
    logical :: has_code_and_comment, have_prefix, saved_have_prefix, ok
    character(len=:), allocatable :: commark, comment_re, comment_fill_prefix, skip, separate_orig, start_orig, &
                                     saved_prefix, prefix, last
    integer(int64) :: savedb, savedz
    type(status_type) :: st

    done = .false.
    call comment_normalize_vars(status)
    if (.not. status%ok()) return
    skip = fill_comment_start_skip()
    opoint = point()
    ! Figure out what kind of comment we are looking at.
    call beginning_of_line(1_int64)
    has_code_and_comment = .false.
    comin = 0
    comstart = comment_search_forward(line_end_position(), .true., st)

    if (comstart > 0) then
        comin = point()
        call goto_char(comstart)
        moved = skip_chars_backward(" "//TAB, st)
        has_code_and_comment = .not. is_bol()
    end if

    call goto_char(opoint)
    ! Make sure the comment-start mark we found is accepted by
    ! comment-start-skip.  If not, all bets are off, and we'd better not
    ! mess with it.
    if (comstart == 0) return
    if (string_match(skip, text_of(comstart, comin), st) == 0) return
    ! Narrow to include only the comment, and then fill the region.
    commark = comment_string_strip(text_of(comstart, comin), .false., .true.)

    ! A regexp more specialized than comment-start-skip, that only
    ! matches the current commark rather than any valid commark.
    !
    ! The specialized regexp only works for "normal" comment syntax, not
    ! for Texinfo's "@c" (which can't be immediately followed by
    ! word-chars) or Fortran's "C" (which needs to be at bol), so check
    ! that comment-start-skip indeed allows the commark to appear in the
    ! middle of the line and followed by word chars.  The choice of "\0"
    ! and "a" is mostly arbitrary.
    if (string_match(skip, achar(0)//commark//"a", st) > 0) then
        last = last_char(commark)
        comment_re = "[ "//TAB//"]*"//regexp_quote(commark)//"[^"//last//"]"
    else
        comment_re = "[ "//TAB//"]*\(?:"//skip//"\)"
    end if

    ! Compute a fill prefix.
    call goto_char(comstart)

    if (has_code_and_comment) then
        col = current_column()

        if (.not. indent_tabs_mode%value .or. tab_width%value <= 0) then
            comment_fill_prefix = repeat(" ", int(col))
        else
            comment_fill_prefix = repeat(TAB, int(col/tab_width%value))//repeat(" ", int(mod(col, tab_width%value)))
        end if

        comment_fill_prefix = comment_fill_prefix//text_of(point(), comin)
    else
        comment_fill_prefix = text_of(line_beginning_position(), comin)
    end if

    call goto_char(opoint)
    ! (save-excursion (save-restriction ...))
    savedb = buffers(cur)%begv
    savedz = buffers(cur)%text%z - buffers(cur)%zv
    call beginning_of_line(1_int64)

    ! Find the first line we should include in the region to fill.
    if (has_code_and_comment) then
        nstart = line_beginning_position()
    else
        do
            if (forward_line(-1_int64) /= 0) exit
            if (.not. looking_at(comment_re, st)) exit
        end do

        ! We may have gone too far.  Go forward again.
        cs = comment_search_forward(line_end_position(), .true., st)
        if (cs > 0) call goto_char(cs)

        if (looking_at(comment_re, st)) then
            comstart = point()
            nstart = line_beginning_position()
        else
            comstart = point()
            nstart = line_beginning_position(2_int64)
        end if

        call goto_char(opoint)
        call beginning_of_line(1_int64)
    end if

    ! Find the beginning of the first line past the region to fill.
    do
        found = point()
        shortage = forward_line(1_int64)
        if (point() == found) exit
        if (.not. looking_at(comment_re, st)) exit
    end do

    nend = point()
    call goto_char(opoint)
    call beginning_of_line(1_int64)
    call narrow_to_region(nstart, nend, st)
    ! Obey paragraph starters and boundaries within comments.
    call init_paragraph_variables()
    separate_orig = paragraph_separate%value
    start_orig = paragraph_start%value
    ! Use the default values since they correspond to the values to use
    ! for plain text.
    paragraph_separate%value = separate_orig//"\|[ "//TAB//"]*\(?:"//skip//"\)\(?:"// &
                               default_string("paragraph_separate")//"\)"
    paragraph_start%value = start_orig//"\|[ "//TAB//"]*\(?:"//skip//"\)\(?:"//default_string("paragraph_start")//"\)"
    ! We used to rely on fill-prefix to break paragraph at comment-starter
    ! changes, but it did not work for the first line (mixed
    ! comment&code).  We now use comment-re instead to "manually" make
    ! sure we treat comment-marker changes as paragraph boundaries.
    after_line = -1
    if (has_code_and_comment) after_line = line_beginning_position(2_int64)
    shortage = forward_paragraph(1_int64)
    end = point()
    ! If this comment starts on a line with code, include that line in
    ! the filling.
    shortage = forward_paragraph(-1_int64)
    if (point() == after_line) shortage = forward_line(-1_int64)
    beg = point()
    paragraph_separate%value = separate_orig
    paragraph_start%value = start_orig
    call widen()
    call narrow_to_region(savedb, buffers(cur)%text%z - savedz, st)
    call goto_char(opoint)
    ! Find the fill-prefix to use.
    saved_have_prefix = allocated(fill_prefix%value)
    if (saved_have_prefix) saved_prefix = fill_prefix%value

    if (.not. saved_have_prefix) then
        ! Try adaptive fill mode.
        ok = .false.

        if (adaptive_fill_mode%value) then
            call fill_context_prefix(beg, end, prefix, have_prefix)

            if (have_prefix) then
                fill_prefix%value = prefix
                ok = string_match(skip, prefix, st) > 0
            end if
        end if

        if (.not. ok) fill_prefix%value = comment_fill_prefix
    end if

    ! Don't canonicalize spaces within the code just before the comment.
    opoint = point()
    call goto_char(beg)
    squeeze = 0

    if (.not. looking_at(fill_prefix%value, st)) then
        call re_search_forward(skip, found, status)
        if (status%ok()) squeeze = point()
    end if

    call goto_char(opoint)

    if (status%ok()) then
        ! Don't fill with narrowing.
        if (squeeze > 0) then
            call fill_region_as_paragraph(max(comstart, beg), end, status, justify, nosqueeze=.false., &
                                          squeeze_after=squeeze)
        else
            call fill_region_as_paragraph(max(comstart, beg), end, status, justify)
        end if
    end if

    if (saved_have_prefix) then
        fill_prefix%value = saved_prefix
    else if (allocated(fill_prefix%value)) then
        deallocate (fill_prefix%value)
    end if

    ! Make sure we don't return nil.
    done = .true.
    end procedure fill_comment_paragraph

    !> The last character of S (UTF-8), as a string.  Not a port.
    function last_char(s) result(c)
        character(len=*), intent(in) :: s
        character(len=:), allocatable :: c
        integer :: k

        c = ""
        if (len(s) == 0) return
        k = len(s)

        do while (k > 1)
            if (iand(iachar(s(k:k)), 192) /= 128) exit
            k = k - 1
        end do

        c = s(k:)
    end function last_char
end submodule fill_comment_fm
