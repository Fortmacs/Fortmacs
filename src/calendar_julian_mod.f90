! calendar_julian_mod.f90 -- the Julian calendar's conversions and holidays.
! Ported from GNU Emacs 31.1 lisp/calendar/cal-julian.el (in part).
!
! Copyright (C) 1995, 1997, 2001-2026 Free Software Foundation, Inc.
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

!> Part of holidays_mod: the conversions between Julian and absolute
!> dates, which the Julian and Greek Orthodox holidays use, and
!> `holiday_julian'.  The rest of cal-julian.el is listed as Deferred in
!> docs/status/holidays_mod.md.
submodule(holidays_mod) calendar_julian_mod
    implicit none(type, external)

contains

    module procedure calendar_julian_to_absolute
    ! correct for Julian but not Gregorian leap year
    if (mod(date%year, 100) == 0 .and. mod(date%year, 400) /= 0 .and. date%month > 2) then
        absolute = 1
    else
        absolute = 0
    end if

    absolute = absolute + calendar_day_number(date) + 365*(date%year - 1) + (date%year - 1)/4 - 2
    end procedure calendar_julian_to_absolute

    module procedure calendar_julian_from_absolute
    integer, parameter :: LAST_DAYS(12) = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    integer :: approx, year, month, last

    ! approximation from below
    approx = (date + 2)/366
    ! search forward from the approximation
    year = approx

    do while (date >= calendar_julian_to_absolute(calendar_date_type(1, 1, year + 1)))
        year = year + 1
    end do

    ! search forward from January
    month = 1

    do
        last = LAST_DAYS(month)
        if (month == 2 .and. mod(year, 4) == 0) last = 29
        if (date <= calendar_julian_to_absolute(calendar_date_type(month, last, year))) exit
        month = month + 1
    end do

    ! calculate the day by subtraction
    julian = calendar_date_type(month, date - (calendar_julian_to_absolute(calendar_date_type(month, 1, year)) - 1), year)
    end procedure calendar_julian_from_absolute

    module procedure holiday_julian
    type(calendar_date_type), allocatable :: dates(:)
    integer :: k

    call get_calendar_nongregorian_visible_dates(month, day, calendar_julian_to_absolute, &
                                                 calendar_julian_from_absolute, dates)
    allocate (holidays(0))

    do k = 1, size(dates)
        call append_holiday(holidays, dates(k), string)
    end do
    end procedure holiday_julian
end submodule calendar_julian_mod
