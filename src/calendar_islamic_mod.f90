! calendar_islamic_mod.f90 -- the Islamic calendar's conversions and holidays.
! Ported from GNU Emacs 31.1 lisp/calendar/cal-islam.el (in part).
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

!> Part of holidays_mod: the conversions between Islamic and absolute
!> dates and the Islamic holidays.  The rest of cal-islam.el is listed as
!> Deferred in docs/status/holidays_mod.md.
submodule(holidays_mod) calendar_islamic_mod
    implicit none(type, external)

contains

    module procedure is_calendar_islamic_leap_year
    leap = any(mod(year, 30) == [2, 5, 7, 10, 13, 16, 18, 21, 24, 26, 29])
    end procedure is_calendar_islamic_leap_year

    module procedure calendar_islamic_last_day_of_month
    if (any(month == [1, 3, 5, 7, 9, 11])) then
        last = 30
    else if (any(month == [2, 4, 6, 8, 10])) then
        last = 29
    else if (is_calendar_islamic_leap_year(year)) then
        last = 30
    else
        last = 29
    end if
    end procedure calendar_islamic_last_day_of_month

    module procedure calendar_islamic_day_number
    day_of_year = 30*(date%month/2) + 29*((date%month - 1)/2) + date%day
    end procedure calendar_islamic_day_number

    module procedure calendar_islamic_to_absolute
    integer :: y, leap_years_in_cycle

    y = mod(date%year, 30)

    if (y < 3) then
        leap_years_in_cycle = 0
    else if (y < 6) then
        leap_years_in_cycle = 1
    else if (y < 8) then
        leap_years_in_cycle = 2
    else if (y < 11) then
        leap_years_in_cycle = 3
    else if (y < 14) then
        leap_years_in_cycle = 4
    else if (y < 17) then
        leap_years_in_cycle = 5
    else if (y < 19) then
        leap_years_in_cycle = 6
    else if (y < 22) then
        leap_years_in_cycle = 7
    else if (y < 25) then
        leap_years_in_cycle = 8
    else if (y < 27) then
        leap_years_in_cycle = 9
    else
        leap_years_in_cycle = 10
    end if

    ! days so far this year + days in all non-leap years + leap days in
    ! complete cycles + leap days this cycle + days before start of
    ! calendar
    absolute = calendar_islamic_day_number(date) + (date%year - 1)*354 + 11*(date%year/30) + leap_years_in_cycle &
               + calendar_islamic_epoch() - 1
    end procedure calendar_islamic_to_absolute

    module procedure calendar_islamic_from_absolute
    integer :: epoch, approx, year, month

    epoch = calendar_islamic_epoch()

    if (date < epoch) then
        ! pre-Islamic date
        islamic = calendar_date_type(0, 0, 0)
        return
    end if

    ! approximation from below
    approx = (date - epoch)/355
    ! search forward from the approximation
    year = approx

    do while (date >= calendar_islamic_to_absolute(calendar_date_type(1, 1, year + 1)))
        year = year + 1
    end do

    ! search forward from Muharram
    month = 1

    do while (date > calendar_islamic_to_absolute(calendar_date_type(month, &
                                                                     calendar_islamic_last_day_of_month(month, year), year)))
        month = month + 1
    end do

    ! calculate the day by subtraction
    islamic = calendar_date_type(month, date - (calendar_islamic_to_absolute(calendar_date_type(month, 1, year)) - 1), year)
    end procedure calendar_islamic_from_absolute

    !> Absolute date of start of Islamic calendar = July 16, 622 AD (Julian).
    !> Port of cal-islam.el calendar-islamic-epoch (a function: the constant
    !> computed).
    pure integer function calendar_islamic_epoch() result(epoch)
        epoch = calendar_julian_to_absolute(calendar_date_type(7, 16, 622))
    end function calendar_islamic_epoch

    module procedure holiday_islamic
    type(calendar_date_type), allocatable :: dates(:)
    type(calendar_date_type) :: islamic_date, date
    integer :: k, m, y

    allocate (holidays(0))

    if (calendar_total_months_local() /= 3) then
        call get_calendar_nongregorian_visible_dates(month, day, calendar_islamic_to_absolute, &
                                                     calendar_islamic_from_absolute, dates)

        do k = 1, size(dates)
            call append_holiday(holidays, dates(k), string)
        end do

        return
    end if

    ! When the calendar displays 3 months, we can calculate only one
    ! local date, which corresponds to the center of the calendar
    ! window, instead of two local dates.  Specifically, there are
    ! approx 45 visible days either side of this date.  Given the
    ! length of the Islamic months, this means up to two different
    ! months are visible either side of the central date.
    islamic_date = calendar_islamic_from_absolute(calendar_absolute_from_gregorian( &
                                                  calendar_date_type(calendar_displayed_month(), 15, &
                                                                     calendar_displayed_year())))
    m = islamic_date%month
    y = islamic_date%year
    ! Islamic calendar doesn't apply
    if (m < 1) return
    ! Since converting to absolute dates can be a complex
    ! operation, we try to speed things up by excluding those date
    ! ranges that can't possibly be visible.
    ! We can view the situation (see above) as if we had a calendar
    ! window displaying 5 months at a time.  When month m is
    ! central, months m-2:m+2 (modulo 12) might be visible.
    ! Recall from holiday-fixed that with a 3 month calendar
    ! window, November is special, because we can do a one-sided
    ! inclusion test.  When November is central is when the end of
    ! year first appears on the calendar.  Similarly, with a 5
    ! month window, October is special.  When October is central is
    ! when the end of year first appears, and when January is
    ! central, October is no longer visible.  October is visible
    ! when the central month is >= 8.
    ! Hence to test if any given month might be visible, we can
    ! shift things and ask about October.
    ! At the same time, we work out the appropriate year y to use.
    call calendar_increment_month(m, y, 10 - month)
    ! Islamic date might be visible
    if (m <= 7) return
    date = calendar_gregorian_from_absolute(calendar_islamic_to_absolute(calendar_date_type(month, day, y)))
    if (is_calendar_date_visible(date)) call append_holiday(holidays, date, string)
    end procedure holiday_islamic

    module procedure holiday_islamic_new_year
    type(holiday_type), allocatable :: entries(:)
    type(calendar_date_type) :: islamic
    character(len=16) :: digits
    integer :: k, m, y

    status = status_type()
    call holiday_islamic(1, 1, "", entries)
    allocate (holidays(0))

    do k = 1, size(entries)
        m = entries(k)%date%month
        y = entries(k)%date%year
        islamic = calendar_islamic_from_absolute(calendar_absolute_from_gregorian( &
                                                 calendar_date_type(m, calendar_last_day_of_month(m, y), y)))
        write (digits, "(i0)") islamic%year
        call append_holiday(holidays, entries(k)%date, "Islamic New Year "//trim(digits))
    end do
    end procedure holiday_islamic_new_year
end submodule calendar_islamic_mod
