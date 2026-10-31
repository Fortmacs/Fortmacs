! thingatpt_fm.f90 -- get the "thing" at point.
! Ported from GNU Emacs 31.1 lisp/thingatpt.el.
!
! Copyright (C) 1991-1998, 2000-2026 Free Software Foundation, Inc.
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

!> The number at point (`number_at_point', as the goto commands read their
!> default) and `thing_at_point_looking_at'.
!>
!> Deferred: the rest of thingatpt.el (`thing_at_point' and
!> `bounds_of_thing_at_point' for the other things, `forward_thing',
!> `beginning_of_thing', `end_of_thing', the URL, email, UUID and file
!> name things, `thing_at_point_provider_alist', `sexp_at_point' and the
!> others), and floating-point numbers (a decimal number with a fraction
!> is found, but not given as a value).
module thingatpt_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use error_fm, only: status_type
    use editfns_fm, only: point, point_min, point_max, goto_char, buffer_substring
    use cmds_fm, only: forward_char
    use search_fm, only: re_search_forward, match_beginning, match_end, SEARCH_NOERROR
    implicit none(type, external)
    private

    public :: thing_at_point_looking_at, number_at_point

    !> A regexp matching a decimal number.  Port of thingatpt.el
    !> thing-at-point-decimal-regexp.
    character(len=*), parameter, public :: thing_at_point_decimal_regexp = "-?[0-9]+\.?[0-9]*"

    !> A regexp matching a hexadecimal number.  Port of thingatpt.el
    !> thing-at-point-hexadecimal-regexp.
    character(len=*), parameter, public :: thing_at_point_hexadecimal_regexp = "\(0x\|#x\)\([a-fA-F0-9]+\)"

contains

    !> Return true if point is in or just after a match for REGEXP.
    !> Set the match data from the earliest such match ending at or after
    !> point.
    !>
    !> Optional argument DISTANCE limits search for REGEXP forward and
    !> back from point.  Port of thingatpt.el:thing-at-point-looking-at.
    logical function thing_at_point_looking_at(regexp, distance) result(found)
        character(len=*), intent(in) :: regexp
        integer(int64), intent(in), optional :: distance
        integer(int64) :: old, beg, end, prev, match, mb, me
        type(status_type) :: st

        old = point()
        beg = point_min()
        if (present(distance)) beg = max(point_min(), old - distance)
        end = 0
        if (present(distance)) end = min(point_max(), old + distance)
        call goto_char(beg)

        do
            prev = point()

            if (end > 0) then
                call re_search_forward(regexp, match, st, bound=end, noerror=SEARCH_NOERROR)
            else
                call re_search_forward(regexp, match, st, noerror=SEARCH_NOERROR)
            end if

            if (.not. st%ok()) match = 0
            if (match <= 0) exit
            if (.not. match_end(0, st) < old) exit
            call goto_char(match_beginning(0, st))
            ! Avoid inflooping when `regexp' matches the empty string.
            if (.not. prev < point()) call forward_char(st)
        end do

        call goto_char(old)
        found = .false.

        if (match > 0) then
            mb = match_beginning(0, st)
            me = match_end(0, st)
            found = mb <= old .and. old <= me
        end if
    end function thing_at_point_looking_at

    !> Return the number at point (FOUND false if none is found).
    !> Decimal numbers like "14" or "-14.5", as well as hex numbers
    !> like "0xBEEF09" or "#xBEEF09", are recognized.  A decimal number
    !> with digits after its point is found but is not an integer
    !> (IS_INTEGER false, N unset; floating point is Deferred).  Port of
    !> thingatpt.el:number-at-point.
    subroutine number_at_point(n, found, is_integer)
        integer(int64), intent(out) :: n
        logical, intent(out) :: found, is_integer
        character(len=:), allocatable :: text
        type(status_type) :: st
        integer :: k, d, ios

        n = 0
        is_integer = .false.
        found = thing_at_point_looking_at(thing_at_point_hexadecimal_regexp, 500_int64)

        if (found) then
            call buffer_substring(match_beginning(2, st), match_end(2, st), text, st)

            do k = 1, len(text)
                d = index("0123456789abcdef", text(k:k))
                if (d == 0) d = index("0123456789ABCDEF", text(k:k))
                n = 16*n + d - 1
            end do

            is_integer = .true.
            return
        end if

        found = thing_at_point_looking_at(thing_at_point_decimal_regexp, 500_int64)
        if (.not. found) return
        call buffer_substring(match_beginning(0, st), match_end(0, st), text, st)
        k = index(text, ".")
        ! "14." reads as the integer 14.
        if (k > 0 .and. k < len(text)) return
        if (k == len(text)) text = text(1:k - 1)
        read (text, *, iostat=ios) n
        is_integer = ios == 0
    end subroutine number_at_point
end module thingatpt_fm
