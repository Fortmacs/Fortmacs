! casetab_fm.f90 -- case tables: downcase, upcase and canonicalize.
! Ported from GNU Emacs 31.1 src/casetab.c (the standard case table),
! the case parts of src/buffer.h and lisp/international/characters.el.
!
! Copyright (C) 1993-1994, 2001-2026 Free Software Foundation, Inc.
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

!> The standard case table, which every buffer uses (no buffer-local
!> case tables yet).  ASCII is as in casetab.c:init_casetab_once; the
!> rest comes from the Unicode simple case mappings as characters.el sets
!> them (generated into uni_case_fm).  A character missing from a table
!> maps to itself.
module casetab_fm
    use, intrinsic :: iso_fortran_env, only: int32
    use uni_case_fm, only: down_count, down_keys, down_values, up_count, up_keys, up_values, &
                           canon_count, canon_keys, canon_values
    implicit none(type, external)
    private

    public :: downcase, upcase, case_canon, uppercasep, lowercasep

contains

    !> The value for `c` in the sorted table `keys`/`values`; `c` if absent.
    pure integer(int32) function lookup(c, keys, values, n) result(v)
        integer(int32), intent(in) :: c
        integer, intent(in) :: n
        integer(int32), intent(in) :: keys(n), values(n)
        integer :: lo, hi, mid

        v = c
        lo = 1
        hi = n

        do while (lo <= hi)
            mid = (lo + hi)/2

            if (keys(mid) == c) then
                v = values(mid)
                return
            else if (keys(mid) < c) then
                lo = mid + 1
            else
                hi = mid - 1
            end if
        end do
    end function lookup

    !> Downcase a character C, or make no change if that cannot be done.
    !> Port of buffer.h:downcase.
    elemental integer(int32) function downcase(c)
        integer(int32), intent(in) :: c

        if (c < 128) then
            downcase = c
            if (c >= iachar("A") .and. c <= iachar("Z")) downcase = c + 32
        else
            downcase = lookup(c, down_keys, down_values, down_count)
        end if
    end function downcase

    !> Upcase a character C, or make no change if that cannot be done.
    !> Port of buffer.h:upcase.
    elemental integer(int32) function upcase(c)
        integer(int32), intent(in) :: c

        if (c < 128) then
            upcase = c
            if (c >= iachar("a") .and. c <= iachar("z")) upcase = c - 32
        else
            upcase = lookup(c, up_keys, up_values, up_count)
        end if
    end function upcase

    !> The canonical form of `c` for case-insensitive comparison: the
    !> case_canon_table entry (casetab.c:set_canon), so that σ, ς and Σ
    !> all compare equal.
    elemental integer(int32) function case_canon(c)
        integer(int32), intent(in) :: c

        if (c < 128) then
            case_canon = downcase(c)
        else
            case_canon = lookup(c, canon_keys, canon_values, canon_count)
        end if
    end function case_canon

    !> True if C is upper case.  Port of buffer.h:uppercasep.
    elemental logical function uppercasep(c)
        integer(int32), intent(in) :: c

        uppercasep = downcase(c) /= c
    end function uppercasep

    !> True if C is lower case.  Port of buffer.h:lowercasep.
    elemental logical function lowercasep(c)
        integer(int32), intent(in) :: c

        lowercasep = .not. uppercasep(c) .and. upcase(c) /= c
    end function lowercasep
end module casetab_fm
