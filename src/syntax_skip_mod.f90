! syntax_skip_mod.f90 -- skip_characters, a submodule of syntax_mod.
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

!> `skip_characters_forward' and `skip_characters_backward': a submodule because
!> the character classes come from regular_expression_mod, which uses syntax_mod.
!> The set is kept as character ranges and classes rather than
!> upstream's fastmap of bytes and leading codes; the result is the same
!> for multibyte buffers (unibyte buffers and raw bytes are not
!> distinguished).
submodule(syntax_mod) syntax_skip_mod
    use regular_expression_mod, only: regular_expression_wctype_parse, regular_expression_iswctype
    implicit none(type, external)

contains

    module procedure skip_characters
    integer(int32), allocatable :: lows(:), highs(:)
    integer :: classes(32), nclasses, nranges, cc
    integer(int32) :: c, c2, clen
    integer(int64) :: i, n, p, limit_position, start, position, position_byte
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
        cc = regular_expression_wctype_parse(string, p, n + 1)

        if (cc == 0) then
            call set_error(status, GENERIC_ERROR_CODE, "Invalid ISO C character class")
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

        call string_character_and_length(string, i, c, clen)
        i = i + clen

        ! Treat `-' as range character only if another character follows.
        if (i + 1 <= n) then
            if (string(i:i) == "-") then
                ! Skip over the dash.
                i = i + 1
                ! Get the end of the range.
                call string_character_and_length(string, i, c2, clen)
                i = i + clen

                if (c2 == iachar("\") .and. i <= n) then
                    call string_character_and_length(string, i, c2, clen)
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

    limit_position = skip_limit(forwardp, limit)
    start = point()
    position = start

    associate (b => buffers(current))
        if (forwardp) then
            do while (position < limit_position)
                position_byte = character_to_byte(position)
                c = buffer_fetch_character(b, position_byte)
                if (in_set(c) .eqv. negate) exit
                position = position + 1
            end do
        else
            do while (position > limit_position)
                position_byte = character_to_byte(position)

                if (b%multibyte) then
                    position_byte = position_byte - buffer_previous_character_length(b, position_byte)
                else
                    position_byte = position_byte - 1
                end if

                c = buffer_fetch_character(b, position_byte)
                if (in_set(c) .eqv. negate) exit
                position = position - 1
            end do
        end if
    end associate

    call goto_character(position)
    moved = position - start

contains

    ! Whether CH is in the ranges or the character classes.  Port of
    ! syntax.c:in_classes (the classes).
    logical function in_set(ch)
        integer(int32), intent(in) :: ch
        integer :: k

        in_set = .true.

        do k = 1, nranges
            if (lows(k) <= ch .and. ch <= highs(k)) return
        end do

        do k = 1, nclasses
            if (regular_expression_iswctype(ch, classes(k))) return
        end do

        in_set = .false.
    end function in_set
    end procedure skip_characters
end submodule syntax_skip_mod
