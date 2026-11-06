! time_date_parse_mod.f90 -- dates as text.
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

!> Part of time_date_mod: the functions that parse a date as text, which
!> need parse_time_mod (a module above time_date_mod, since parse_time_mod
!> uses iso8601_mod, which uses time_date_mod).
submodule(time_date_mod) time_date_parse_mod
    use parse_time_mod, only: parse_time_string
    implicit none(type, external)

contains

    module function date_to_time(date, status) result(time)
        character(len=*), intent(in) :: date
        type(status_type), intent(out) :: status
        type(time_type) :: time
        type(partial_decoded_time_type) :: parsed

        ! Parse DATE.  If it contains a year, use defaults for other
        ! components.  Then encode the result; this is an error if the year
        ! is missing, because encoding is an error if crucial time components
        ! are unknown.  This heuristic uses local time if the string lacks
        ! time zone info, because an unknown time zone is local time.
        parsed = parse_time_string(date)
        if (allocated(parsed%year)) call set_decoded_time_defaults(parsed)
        time = encode_partial_decoded_time(parsed, status)
        if (status%ok()) return

        if (status%code == GENERIC_ERROR_CODE .and. allocated(status%detail)) then
            if (status%detail == "Specified time is not representable") return
        end if

        call set_error(status, GENERIC_ERROR_CODE, "Invalid date: "//date)
    end function date_to_time

    module function safe_date_to_time(date) result(time)
        character(len=*), intent(in) :: date
        type(time_type) :: time
        type(status_type) :: status

        time = date_to_time(date, status)
        if (.not. status%ok()) time = time_type()
    end function safe_date_to_time

    module function time_since_date(date, status) result(elapsed)
        character(len=*), intent(in) :: date
        type(status_type), intent(out) :: status
        type(time_type) :: elapsed
        type(time_type) :: time

        elapsed = time_type()
        ! Convert date strings to internal time.
        time = date_to_time(date, status)
        if (status%ok()) elapsed = time_since_time(time)
    end function time_since_date

    module function date_to_day(date, status) result(day)
        character(len=*), intent(in) :: date
        type(status_type), intent(out) :: status
        integer(int64) :: day
        type(time_type) :: time

        day = 0
        time = date_to_time(date, status)
        if (status%ok()) day = time_to_days(time, status)
    end function date_to_day

    module function days_between(date1, date2, status) result(days)
        character(len=*), intent(in) :: date1, date2
        type(status_type), intent(out) :: status
        integer(int64) :: days
        integer(int64) :: day1, day2

        days = 0
        day1 = date_to_day(date1, status)
        if (.not. status%ok()) return
        day2 = date_to_day(date2, status)
        if (status%ok()) days = day1 - day2
    end function days_between
end submodule time_date_parse_mod
