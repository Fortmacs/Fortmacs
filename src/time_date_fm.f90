! time_date_fm.f90 -- date and time handling functions.
! Ported from GNU Emacs 31.1 lisp/calendar/time-date.el.
!
! Copyright (C) 1998-2026 Free Software Foundation, Inc.
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

!> Time intervals as text: `seconds_to_string'.
!>
!> Deferred: the rest of time-date.el (date arithmetic, `date-to-time',
!> `format-seconds', `time-subtract' and the other time functions).
module time_date_fm
    use, intrinsic :: iso_fortran_env, only: int64, real64
    implicit none(type, external)
    private

    public :: seconds_to_string, SECONDS_TO_STRING_PLAIN, SECONDS_TO_STRING_READABLE, &
              SECONDS_TO_STRING_EXPANDED

    !> Values of READABLE for `seconds_to_string': a float in one unit
    !> (nil), readable units (t), or two readable units if appropriate
    !> (`expanded').
    integer, parameter :: SECONDS_TO_STRING_PLAIN = 0, SECONDS_TO_STRING_READABLE = 1, &
                          SECONDS_TO_STRING_EXPANDED = 2

    ! seconds-to-string: the units of the plain format, (LIMIT UNIT
    ! SECS); the last has no limit.
    real(real64), parameter :: PLAIN_LIMITS(5) = [1.0_real64, 100.0_real64, 6000.0_real64, &
                                                  108000.0_real64, 34560000.0_real64]
    character(len=2), parameter :: PLAIN_UNITS(6) = ["ms", "s ", "m ", "h ", "d ", "y "]
    integer, parameter :: PLAIN_UNIT_LEN(6) = [2, 1, 1, 1, 1, 1]
    real(real64), parameter :: PLAIN_SECS(6) = [0.001_real64, 1.0_real64, 60.0_real64, 3600.0_real64, &
                                                86400.0_real64, 365.25_real64*24*3600]

    ! seconds-to-string-readable: (ABBREV-UNIT UNIT UNIT-PLURAL SECS).
    character(len=1), parameter :: READABLE_ABBREV(7) = ["Y", "M", "w", "d", "h", "m", "s"]
    character(len=6), parameter :: READABLE_UNIT(7) = ["year  ", "month ", "week  ", "day   ", "hour  ", "minute", &
                                                       "second"]
    character(len=7), parameter :: READABLE_PLURAL(7) = ["years  ", "months ", "weeks  ", "days   ", "hours  ", &
                                                         "minutes", "seconds"]
    integer(int64), parameter :: READABLE_SECS(7) = [31556952_int64, 2629746_int64, 604800_int64, 86400_int64, &
                                                     3600_int64, 60_int64, 1_int64]

contains

    !> Convert time interval DELAY (in seconds) to a string.
    !> By default, the returned string is formatted as a float in the smallest
    !> unit from the variable `seconds_to_string' that is longer than DELAY,
    !> and a precision of two.  If READABLE is `SECONDS_TO_STRING_READABLE',
    !> convert DELAY into a readable string, using the information provided
    !> in the variable `seconds_to_string_readable'.  If it is
    !> `SECONDS_TO_STRING_EXPANDED', use two units to describe DELAY, if
    !> appropriate.  E.g. "1 hour 32 minutes".  If ABBREV is true,
    !> abbreviate the readable units.  If PRECISION is a whole number, round
    !> the value associated with the smallest displayed unit to that many
    !> digits after the decimal.  If PRECISION_FRACTION is a non-negative
    !> float less than 1.0, round to that value.  Port of
    !> time-date.el:seconds-to-string.
    recursive function seconds_to_string(delay, readable, abbrev, precision, precision_fraction) result(s)
        real(real64), intent(in) :: delay
        integer, intent(in), optional :: readable
        logical, intent(in), optional :: abbrev
        integer, intent(in), optional :: precision
        real(real64), intent(in), optional :: precision_fraction
        character(len=:), allocatable :: s
        integer :: how, digits, here, here_pre, k
        logical :: abbreviate, expanded, isfloatp, have_pre
        real(real64) :: round_to, cnt, cnt_pre, cnt_val
        character(len=:), allocatable :: padding
        character(len=64) :: buf

        how = SECONDS_TO_STRING_PLAIN
        if (present(readable)) how = readable
        abbreviate = .false.
        if (present(abbrev)) abbreviate = abbrev

        if (delay < 0) then
            ! (Upstream passes PRECISION as ABBREV here.)
            if (present(precision)) then
                s = "-"//seconds_to_string(-delay, how, .true.)
            else if (present(precision_fraction)) then
                s = "-"//seconds_to_string(-delay, how, .true.)
            else
                s = "-"//seconds_to_string(-delay, how)
            end if

            return
        end if

        if (how /= SECONDS_TO_STRING_PLAIN) then
            expanded = how == SECONDS_TO_STRING_EXPANDED

            if (present(precision)) then
                digits = precision
                round_to = 10.0_real64**(-precision)
            else if (present(precision_fraction)) then
                digits = -floor(log10(precision_fraction))
                round_to = precision_fraction
            else
                digits = 0
                round_to = 1
            end if

            padding = " "
            if (abbreviate) padding = ""

            if (abs(lisp_round(delay, round_to)) < 0.5_real64) then
                if (abbreviate) then
                    s = "0s"
                else
                    s = "0 seconds"
                end if

                return
            end if

            here = 1

            do while (here < size(READABLE_SECS))
                if (delay/real(READABLE_SECS(here), real64) >= 1) exit
                here = here + 1
            end do

            have_pre = .false.
            here_pre = 0
            cnt_pre = 0

            if (expanded .and. here < size(READABLE_SECS)) then
                here_pre = here
                here = here + 1
                cnt_pre = floor(delay/real(READABLE_SECS(here_pre), real64))
                cnt = lisp_round((delay - cnt_pre*real(READABLE_SECS(here_pre), real64)) &
                                 /real(READABLE_SECS(here), real64), round_to)

                if (cnt > 0) then
                    have_pre = .true.
                else
                    cnt = cnt_pre
                    here = here_pre
                end if
            end if

            if (.not. have_pre) cnt = lisp_round(delay/real(READABLE_SECS(here), real64), round_to)
            cnt_val = cnt*round_to
            isfloatp = digits > 0 .and. cnt_val - floor(cnt_val) > 0
            s = ""

            if (have_pre) then
                write (buf, "(i0)") int(cnt_pre, int64)
                s = trim(buf)//padding//unit_name(cnt_pre, here_pre, abbreviate, .false.)//" "
            end if

            if (isfloatp) then
                write (buf, "(f0."//itoa(digits)//")") cnt_val
                buf = adjustl(buf)
                if (buf(1:1) == ".") buf = "0"//buf(1:len(buf) - 1)
            else
                write (buf, "(i0)") floor(cnt_val, int64)
            end if

            ! Float formats are always plural.
            s = s//trim(buf)//padding//unit_name(cnt_val, here, abbreviate, isfloatp)
        else if (.not. (delay > 0)) then
            s = "0s"
        else
            k = 1

            do while (k <= size(PLAIN_LIMITS))
                if (.not. (PLAIN_LIMITS(k) <= delay)) exit
                k = k + 1
            end do

            write (buf, "(f0.2)") delay/PLAIN_SECS(k)
            buf = adjustl(buf)
            if (buf(1:1) == ".") buf = "0"//buf(1:len(buf) - 1)
            s = trim(buf)//PLAIN_UNITS(k) (1:PLAIN_UNIT_LEN(k))
        end if
    end function seconds_to_string

    ! The unit HERE for VAL: abbreviated, singular or PLURAL.  Not a port.
    function unit_name(val, here, abbrev, plural) result(name)
        real(real64), intent(in) :: val
        integer, intent(in) :: here
        logical, intent(in) :: abbrev, plural
        character(len=:), allocatable :: name

        if (abbrev) then
            name = trim(READABLE_ABBREV(here))
        else if (.not. plural .and. floor(val) <= 1) then
            name = trim(READABLE_UNIT(here))
        else
            name = trim(READABLE_PLURAL(here))
        end if
    end function unit_name

    ! (round X DIVISOR): the integer nearest to X/DIVISOR, ties to even.
    ! Not a port.
    real(real64) function lisp_round(x, divisor) result(r)
        real(real64), intent(in) :: x, divisor
        real(real64) :: q, f

        q = x/divisor
        f = floor(q)

        if (q - f > 0.5_real64) then
            r = f + 1
        else if (q - f < 0.5_real64) then
            r = f
        else if (modulo(int(f, int64), 2_int64) == 0) then
            r = f
        else
            r = f + 1
        end if
    end function lisp_round

    ! Not a port.
    function itoa(n) result(s)
        integer, intent(in) :: n
        character(len=:), allocatable :: s
        character(len=12) :: buf

        write (buf, "(i0)") n
        s = trim(buf)
    end function itoa
end module time_date_fm
