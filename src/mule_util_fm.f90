! mule_util_fm.f90 -- utility functions for multilingual environment (mule).
! Ported from GNU Emacs 31.1 lisp/international/mule-util.el.
!
! Copyright (C) 1997-1998, 2000-2026 Free Software Foundation, Inc.
! Copyright (C) 1995, 2000, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009,
!   2010, 2011
!   National Institute of Advanced Industrial Science and Technology (AIST)
!   Registration Number H14PRO021
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

!> String truncation by display width: `truncate_string_to_width' and
!> `truncate_string_ellipsis' (its variable is
!> `truncate_string_ellipsis_variable': a variable named like a function
!> takes the suffix _variable).
!>
!> Deferred: `store_substring', the nested tables, the coding-system
!> helpers, `with_coding_priority', `detect_coding_with_language_environment',
!> `filepos_to_bufferpos' and `bufferpos_to_filepos'.
module mule_util_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use lisp_fm, only: value_type, make_string, make_list
    use lread_fm, only: sym
    use character_fm, only: string_char_and_length
    use xdisp_fm, only: glyph_columns
    use textprop_fm, only: propertize, propertized_concat
    implicit none(type, external)
    private

    public :: truncate_string_ellipsis, truncate_string_to_width, truncate_string_to_width_propertized

    !> String to use to indicate truncation (unallocated for the default).
    !> Serves as default value of ELLIPSIS argument to
    !> `truncate_string_to_width' returned by the function
    !> `truncate_string_ellipsis'.  Port of mule-util.el
    !> truncate-string-ellipsis (renamed: the function
    !> `truncate_string_ellipsis' has its name).
    character(len=:), allocatable, public :: truncate_string_ellipsis_variable

contains

    !> Return the string used to indicate truncation.
    !> Use the value of the variable `truncate_string_ellipsis_variable' when
    !> it's allocated.  Otherwise, return the Unicode character U+2026
    !> "HORIZONTAL ELLIPSIS" when it's displayable on the selected frame,
    !> or `...'.  Port of mule-util.el:truncate-string-ellipsis (the variable is
    !> `truncate_string_ellipsis_variable'; the
    !> terminal's coding system is UTF-8, so U+2026 is displayable).
    function truncate_string_ellipsis() result(ellipsis)
        character(len=:), allocatable :: ellipsis

        if (allocated(truncate_string_ellipsis_variable)) then
            ellipsis = truncate_string_ellipsis_variable
        else
            ellipsis = char(226)//char(128)//char(166)
        end if
    end function truncate_string_ellipsis

    ! The characters of STR with their widths and byte offsets.  Not a port.
    subroutine decode(str, widths, starts)
        character(len=*), intent(in) :: str
        integer, allocatable, intent(out) :: widths(:)
        integer(int64), allocatable, intent(out) :: starts(:)
        integer(int64) :: i
        integer(int32) :: c, clen
        integer :: n

        allocate (widths(len(str)), starts(len(str) + 1))
        n = 0
        i = 1

        do while (i <= len(str, kind=int64))
            call string_char_and_length(str, i, c, clen)
            n = n + 1
            widths(n) = glyph_columns(c, 0)
            starts(n) = i
            i = i + clen
        end do

        starts(n + 1) = len(str, kind=int64) + 1
        widths = widths(1:n)
        starts = starts(1:n + 1)
    end subroutine decode

    ! Not a port.
    pure integer function total(widths) result(t)
        integer, intent(in) :: widths(:)
        integer :: k

        t = 0

        do k = 1, size(widths)
            t = t + widths(k)
        end do
    end function total

    ! The parts of the result: HEAD padding, the characters FROM_IDX to
    ! IDX (0-based, exclusive) of STR, TAIL padding and the ELLIPSIS
    ! (DONE when the whole result is RESULT, START_COLUMN being beyond STR).
    ! Port of mule-util.el:truncate-string-to-width in part.
    subroutine truncate_parts(str, end_column_in, start_column, padding, ellipsis_in, head, from_idx, idx, tail, &
                              ellipsis, done, result)
        character(len=*), intent(in) :: str, ellipsis_in, padding
        integer, intent(in) :: end_column_in, start_column
        character(len=:), allocatable, intent(out) :: head, tail, ellipsis, result
        integer, intent(out) :: from_idx, idx
        logical, intent(out) :: done
        integer, allocatable :: widths(:)
        integer(int64), allocatable :: starts(:)
        integer :: str_len, str_width, ellipsis_width, column, last_column, last_idx, end_column
        integer, allocatable :: ew(:)
        integer(int64), allocatable :: es(:)

        call decode(str, widths, starts)
        str_len = size(widths)
        str_width = total(widths)
        ellipsis = ellipsis_in
        call decode(ellipsis, ew, es)
        ellipsis_width = total(ew)
        end_column = end_column_in
        idx = 0
        column = 0
        head = ""
        tail = ""
        done = .false.
        from_idx = 0

        do while (column < start_column)
            if (idx >= str_len) exit
            idx = idx + 1
            column = column + widths(idx)
        end do

        if (column < start_column) then
            done = .true.

            if (len(padding) > 0) then
                result = repeat(padding, max(0, end_column))
            else
                result = ""
            end if

            return
        end if

        if (len(padding) > 0 .and. column > start_column) head = repeat(padding, column - start_column)
        from_idx = idx

        if (end_column >= column) then
            if (end_column < str_width .and. str_width > ellipsis_width) then
                end_column = end_column - ellipsis_width
            else
                ellipsis = ""
            end if

            last_column = column
            last_idx = idx

            do while (column < end_column)
                if (idx >= str_len) exit
                last_column = column
                last_idx = idx
                idx = idx + 1
                column = column + widths(idx)
            end do

            if (column > end_column) then
                column = last_column
                idx = last_idx
            end if

            if (len(padding) > 0 .and. column < end_column) tail = repeat(padding, end_column - column)
        end if
    end subroutine truncate_parts

    ! The bytes of characters FROM to TO (0-based, TO exclusive) of STR.
    ! Not a port.
    function chars(str, from, to) result(s)
        character(len=*), intent(in) :: str
        integer, intent(in) :: from, to
        character(len=:), allocatable :: s
        integer, allocatable :: widths(:)
        integer(int64), allocatable :: starts(:)

        call decode(str, widths, starts)

        if (to <= from) then
            s = ""
        else
            s = str(starts(from + 1):starts(to + 1) - 1)
        end if
    end function chars

    !> Truncate string STR to end at column END_COLUMN.
    !> The optional 3rd arg START_COLUMN, if present, specifies the
    !> starting column (default: zero); that means to return the characters
    !> occupying columns START_COLUMN ... END_COLUMN of STR.  Both
    !> END_COLUMN and START_COLUMN are specified in terms of character
    !> display width in the current buffer; see `char_width'.
    !>
    !> Since character composition on display can produce glyphs whose
    !> width is smaller than the sum of `char_width' values of the composed
    !> characters, this function can produce inaccurate results when used
    !> in such cases.
    !>
    !> The optional 4th arg PADDING, if present, specifies a padding
    !> character (which should have a display width of 1) to add at the end
    !> of the result if STR doesn't reach column END_COLUMN, or if
    !> END_COLUMN comes in the middle of a character in STR.  PADDING is
    !> also added at the beginning of the result if column START_COLUMN
    !> appears in the middle of a character in STR.
    !>
    !> If PADDING is absent, no padding is added in these cases, so the
    !> resulting string may be narrower than END_COLUMN.
    !>
    !> If ELLIPSIS is present, it should be a string which will replace the
    !> end of STR (including any padding) if it extends beyond END_COLUMN,
    !> unless the display width of STR is equal to or less than the display
    !> width of ELLIPSIS.  If DEFAULT_ELLIPSIS is true (upstream: ELLIPSIS
    !> true and not a string), ELLIPSIS defaults to
    !> `truncate_string_ellipsis'.  Port of
    !> mule-util.el:truncate-string-to-width (for ELLIPSIS-TEXT-PROPERTY,
    !> see `truncate_string_to_width_propertized').
    function truncate_string_to_width(str, end_column, start_column, padding, ellipsis, default_ellipsis) result(s)
        character(len=*), intent(in) :: str
        integer, intent(in) :: end_column
        integer, intent(in), optional :: start_column
        character(len=*), intent(in), optional :: padding, ellipsis
        logical, intent(in), optional :: default_ellipsis
        character(len=:), allocatable :: s
        character(len=:), allocatable :: head, tail, ell, result
        integer :: from_idx, idx
        logical :: done

        call truncate_parts(str, end_column, opt_int(start_column), opt_str(padding), &
                            ellipsis_of(ellipsis, default_ellipsis), head, from_idx, idx, tail, ell, done, result)

        if (done) then
            s = result
        else
            ! (Possibly) chop off bits of the string.
            s = head//chars(str, from_idx, idx)//tail//ell
        end if
    end function truncate_string_to_width

    !> `truncate_string_to_width' with ELLIPSIS-TEXT-PROPERTY true: a
    !> too-long string will not be truncated, but instead the elided parts
    !> will be covered by a `display' text property showing the ellipsis.
    !> The result is a string with text properties.  Port of
    !> mule-util.el:truncate-string-to-width (ELLIPSIS-TEXT-PROPERTY).
    function truncate_string_to_width_propertized(str, end_column, start_column, padding, ellipsis, &
                                                  default_ellipsis) result(v)
        character(len=*), intent(in) :: str
        integer, intent(in) :: end_column
        integer, intent(in), optional :: start_column
        character(len=*), intent(in), optional :: padding, ellipsis
        logical, intent(in), optional :: default_ellipsis
        type(value_type) :: v
        character(len=:), allocatable :: head, tail, ell, result
        type(value_type) :: props(2), rest
        integer :: from_idx, idx, n
        logical :: done
        integer, allocatable :: widths(:)
        integer(int64), allocatable :: starts(:)

        call truncate_parts(str, end_column, opt_int(start_column), opt_str(padding), &
                            ellipsis_of(ellipsis, default_ellipsis), head, from_idx, idx, tail, ell, done, result)

        if (done) then
            v = make_string(result)
        else if (len(ell) > 0) then
            ! Use text properties for the ellipsis.
            call decode(str, widths, starts)
            n = size(widths)
            props(1) = sym("display")
            props(2) = make_string(ell)
            rest = propertize(make_string(chars(str, idx, n)), make_list(props))
            v = propertized_concat(make_string(head//chars(str, from_idx, idx)), rest)
        else
            v = make_string(head//chars(str, from_idx, idx)//tail//ell)
        end if
    end function truncate_string_to_width_propertized

    ! Not a port.
    integer function opt_int(x)
        integer, intent(in), optional :: x

        opt_int = 0
        if (present(x)) opt_int = x
    end function opt_int

    ! Not a port.
    function opt_str(x) result(s)
        character(len=*), intent(in), optional :: x
        character(len=:), allocatable :: s

        s = ""
        if (present(x)) s = x
    end function opt_str

    ! Not a port.
    function ellipsis_of(ellipsis, default_ellipsis) result(s)
        character(len=*), intent(in), optional :: ellipsis
        logical, intent(in), optional :: default_ellipsis
        character(len=:), allocatable :: s

        s = ""
        if (present(ellipsis)) s = ellipsis

        if (present(default_ellipsis)) then
            if (default_ellipsis) s = truncate_string_ellipsis()
        end if
    end function ellipsis_of
end module mule_util_fm
