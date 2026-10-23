! syntax_skip_fm.f90 -- skip_chars, a submodule of syntax_fm.
! Ported from GNU Emacs 31.1 src/syntax.c.
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

!> `skip-chars-forward' and `skip-chars-backward': a submodule because
!> the character classes come from regex_emacs_fm, which uses syntax_fm.
!> The set is kept as character ranges and classes rather than
!> upstream's fastmap of bytes and leading codes; the result is the same
!> for multibyte buffers (unibyte buffers and raw bytes are not
!> distinguished).
submodule(syntax_fm) syntax_skip_fm
    use regex_emacs_fm, only: re_wctype_parse, re_iswctype
    implicit none(type, external)

contains

    module procedure skip_chars
    integer(int32), allocatable :: lows(:), highs(:)
    integer :: classes(32), nclasses, nranges, cc
    integer(int32) :: c, c2, clen
    integer(int64) :: i, n, p, limit, start, pos, pos_byte
    logical :: negate
    status = status_type()
    moved = 0
    n = len(string, kind=int64)
    allocate (lows(max(1_int64, n)), highs(max(1_int64, n)))
    nranges = 0
    nclasses = 0
    negate = .false.
    i = 1
    if (n > 0) then
        if (string(1:1) == "^") then
            negate = .true.
            i = 2
        end if
    end if
    ! Find the characters specified.  Handle backslashes and ranges
    ! specially.
    do while (i <= n)
        p = i
        cc = re_wctype_parse(string, p, n + 1)
        if (cc == 0) then
            call set_error(status, FM_GENERIC_ERROR, "Invalid ISO C character class")
            return
        end if
        if (cc /= -1) then
            if (.not. any(classes(1:nclasses) == cc)) then
                nclasses = nclasses + 1
                classes(nclasses) = cc
            end if
            i = p
            cycle
        end if
        if (string(i:i) == "\") then
            if (i == n) exit
            i = i + 1
        end if
        call string_char_and_length(string, i, c, clen)
        i = i + clen
        ! Treat `-' as range character only if another character follows.
        if (i + 1 <= n) then
            if (string(i:i) == "-") then
                ! Skip over the dash.
                i = i + 1
                ! Get the end of the range.
                call string_char_and_length(string, i, c2, clen)
                i = i + clen
                if (c2 == iachar("\") .and. i <= n) then
                    call string_char_and_length(string, i, c2, clen)
                    i = i + clen
                end if
                if (c <= c2) then
                    nranges = nranges + 1
                    lows(nranges) = c
                    highs(nranges) = c2
                end if
                cycle
            end if
        end if
        nranges = nranges + 1
        lows(nranges) = c
        highs(nranges) = c
    end do
    limit = skip_limit(forwardp, lim)
    start = point()
    pos = start
    associate (b => buffers(cur))
        if (forwardp) then
            do while (pos < limit)
                pos_byte = char_to_byte(pos)
                c = buf_fetch_char(b, pos_byte)
                if (in_set(c) .eqv. negate) exit
                pos = pos + 1
            end do
        else
            do while (pos > limit)
                pos_byte = char_to_byte(pos)
                if (b%multibyte) then
                    pos_byte = pos_byte - buf_prev_char_len(b, pos_byte)
                else
                    pos_byte = pos_byte - 1
                end if
                c = buf_fetch_char(b, pos_byte)
                if (in_set(c) .eqv. negate) exit
                pos = pos - 1
            end do
        end if
    end associate
    call goto_char(pos)
    moved = pos - start
contains
    logical function in_set(ch)
        integer(int32), intent(in) :: ch
        integer :: k
        in_set = .true.
        do k = 1, nranges
            if (lows(k) <= ch .and. ch <= highs(k)) return
        end do
        do k = 1, nclasses
            if (re_iswctype(ch, classes(k))) return
        end do
        in_set = .false.
    end function in_set
    end procedure skip_chars
end submodule syntax_skip_fm
