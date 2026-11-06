! time_date_mod.f90 -- date and time handling functions.
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

!> Date and time handling: dates as text (`date_to_time', `days_between';
!> parsed by parse_time_mod, in the submodule time_date_parse_mod),
!> decoded times whose components may be unknown
!> (`partial_decoded_time_type', `set_decoded_time_defaults',
!> `add_decoded_time', `decoded_time_period'), calendar arithmetic
!> (`is_date_leap_year', `date_days_in_month', `date_ordinal_to_time',
!> `time_to_days', `days_to_time') and time intervals as text
!> (`format_seconds', `seconds_to_string').
!>
!> What is not ported yet is listed in docs/status/time_date_mod.md.
module time_date_mod
    use, intrinsic :: iso_fortran_env, only: int64, real64
    use error_mod, only: status_type, set_error, GENERIC_ERROR_CODE, WRONG_TYPE_ARGUMENT_CODE
    use time_functions_mod, only: time_type, decoded_time_type, decode_time, current_time, time_add, time_subtract, &
                                  time_convert, is_time_less, real, DECODED_TIME_DAYLIGHT_SAVING_TIME_UNKNOWN, &
                                  DECODED_TIME_DAYLIGHT_SAVING_TIME_NO
    use search_mod, only: string_match, match_beginning, match_end, case_fold_search
    use basic_subprograms_mod, only: match_string
    implicit none(type, external)
    private

    public :: seconds_to_string, SECONDS_TO_STRING_PLAIN, SECONDS_TO_STRING_READABLE, &
              SECONDS_TO_STRING_EXPANDED
    public :: date_to_time, safe_date_to_time, time_since, date_to_day, days_between, is_date_leap_year, &
              time_to_day_in_year, time_to_days, days_to_time, format_seconds, &
              date_days_in_month, date_ordinal_to_time, add_decoded_time, set_decoded_time_defaults, &
              decoded_time_period, encode_partial_decoded_time

    !> A decoded time, as `decode_time' gives it, whose components may be
    !> unknown: an unallocated component is one the text did not give
    !> as `parse_time_string' and `parse_iso8601' return
    !> it, and the components that `add_decoded_time' should change.  The
    !> structure constructor is upstream's make-decoded-time:
    !> partial_decoded_time_type(month=2) has only its month and DAYLIGHT_SAVING_TIME
    !> unknown.  `daylight_saving_time' is one of time_functions_mod's DECODED_TIME_DAYLIGHT_SAVING_TIME_
    !> constants, and `zone' the UTC offset in seconds east of Greenwich
    !> (unallocated: local time).
    type, public :: partial_decoded_time_type
        type(time_type), allocatable :: second
        integer, allocatable :: minute
        integer, allocatable :: hour
        integer, allocatable :: day
        integer, allocatable :: month
        integer(int64), allocatable :: year
        integer, allocatable :: weekday
        integer :: daylight_saving_time = DECODED_TIME_DAYLIGHT_SAVING_TIME_UNKNOWN
        integer, allocatable :: zone
    end type partial_decoded_time_type

    !> Return the time elapsed since TIME.  TIME should be either a time
    !> value or a date-time string.
    interface time_since
        module procedure time_since_time
        module procedure time_since_date
    end interface time_since

    interface
        !> Parse a string DATE that represents a date-time and return a time
        !> value.  DATE should be in one of the forms recognized by
        !> `parse_time_string'.  If DATE lacks time zone information, local
        !> time is assumed.  A date without a year, or one `encode_time'
        !> cannot encode, is the error "Invalid date: DATE" in STATUS (but
        !> "Specified time is not representable" stays itself).  Port of
        !> time-date.el:date-to-time.  (In submodule time_date_parse_mod.)
        module function date_to_time(date, status) result(time)
            implicit none(type, external)
            character(len=*), intent(in) :: date
            type(status_type), intent(out) :: status
            type(time_type) :: time
        end function date_to_time

        !> Parse a string DATE that represents a date-time and return a time
        !> value.  If DATE is malformed, return a time value of zero.
        !> Port of time-date.el:safe-date-to-time.  (In submodule
        !> time_date_parse_mod.)
        module function safe_date_to_time(date) result(time)
            implicit none(type, external)
            character(len=*), intent(in) :: date
            type(time_type) :: time
        end function safe_date_to_time

        !> Return the time elapsed since DATE, a date-time string (an error
        !> of `date_to_time' in STATUS).  Port of time-date.el:time-since (a
        !> string TIME).  (In submodule time_date_parse_mod.)
        module function time_since_date(date, status) result(elapsed)
            implicit none(type, external)
            character(len=*), intent(in) :: date
            type(status_type), intent(out) :: status
            type(time_type) :: elapsed
        end function time_since_date

        !> Return the absolute date of DATE, a date-time string.  The
        !> absolute date is the number of days elapsed since the imaginary
        !> Gregorian date Sunday, December 31, 1 BC.  Port of
        !> time-date.el:date-to-day.  (In submodule time_date_parse_mod.)
        module function date_to_day(date, status) result(day)
            implicit none(type, external)
            character(len=*), intent(in) :: date
            type(status_type), intent(out) :: status
            integer(int64) :: day
        end function date_to_day

        !> Return the number of days between DATE1 and DATE2.  DATE1 and
        !> DATE2 should be date-time strings.  Port of
        !> time-date.el:days-between.  (In submodule time_date_parse_mod.)
        module function days_between(date1, date2, status) result(days)
            implicit none(type, external)
            character(len=*), intent(in) :: date1, date2
            type(status_type), intent(out) :: status
            integer(int64) :: days
        end function days_between
    end interface

    !> Values of READABLE for `seconds_to_string': a float in one unit
    !> (SECONDS_TO_STRING_PLAIN), readable units (SECONDS_TO_STRING_READABLE), or two readable units if appropriate
    !> (SECONDS_TO_STRING_EXPANDED).
    integer, parameter :: SECONDS_TO_STRING_PLAIN = 0, SECONDS_TO_STRING_READABLE = 1, &
                          SECONDS_TO_STRING_EXPANDED = 2

    ! seconds-to-string: the units of the plain format, (LIMIT UNIT
    ! SECONDS); the last has no limit.
    real(real64), parameter :: PLAIN_LIMITS(5) = [1.0_real64, 100.0_real64, 6000.0_real64, &
                                                  108000.0_real64, 34560000.0_real64]
    character(len=2), parameter :: PLAIN_UNITS(6) = ["ms", "s ", "m ", "h ", "d ", "y "]
    integer, parameter :: PLAIN_UNIT_LENGTH(6) = [2, 1, 1, 1, 1, 1]
    real(real64), parameter :: PLAIN_SECONDS(6) = [0.001_real64, 1.0_real64, 60.0_real64, 3600.0_real64, &
                                                   86400.0_real64, 365.25_real64*24*3600]

    ! seconds-to-string-readable: (ABBREV-UNIT UNIT UNIT-PLURAL SECONDS).
    character(len=1), parameter :: READABLE_ABBREVIATION(7) = ["Y", "M", "w", "d", "h", "m", "s"]
    character(len=6), parameter :: READABLE_UNIT(7) = ["year  ", "month ", "week  ", "day   ", "hour  ", "minute", &
                                                       "second"]
    character(len=7), parameter :: READABLE_PLURAL(7) = ["years  ", "months ", "weeks  ", "days   ", "hours  ", &
                                                         "minutes", "seconds"]
    integer(int64), parameter :: READABLE_SECONDS(7) = [31556952_int64, 2629746_int64, 604800_int64, 86400_int64, &
                                                        3600_int64, 60_int64, 1_int64]

    ! format-seconds: the units, (SPECIFICATION NAME SECONDS); "z" and "x" are flags.
    character(len=1), parameter :: FORMAT_UNIT_SPECIFICATIONS(7) = ["y", "d", "h", "m", "s", "z", "x"]
    character(len=6), parameter :: FORMAT_UNIT_NAMES(5) = ["year  ", "day   ", "hour  ", "minute", "second"]
    integer(int64), parameter :: FORMAT_UNIT_SECONDS(5) = [31536000_int64, 86400_int64, 3600_int64, 60_int64, 1_int64]

contains

    ! ------------------------------------------------------------------
    ! Time values and days
    ! ------------------------------------------------------------------

    !> Return the time elapsed since TIME, a time value.  Port of
    !> time-date.el:time-since.
    type(time_type) function time_since_time(time) result(elapsed)
        type(time_type), intent(in) :: time

        elapsed = time_subtract(current_time(), time)
    end function time_since_time

    !> Convert Fortmacs-epoch DAYS, a real number, into a time value (an
    !> error of `time_convert' in STATUS).  Note that this does not use the
    !> same epoch as `time_to_days'; you must subtract
    !> time_to_days(time_type()) first to convert.  An integer number of
    !> days is time_type(seconds=86400*DAYS).
    !> Port of time-date.el:days-to-time.
    type(time_type) function days_to_time(days, status) result(time)
        real(real64), intent(in) :: days
        type(status_type), intent(out), optional :: status

        time = time_convert(86400*days, status=status)
    end function days_to_time

    !> Return true if YEAR is a leap year.  Port of
    !> time-date.el:date-leap-year-p.
    elemental logical function is_date_leap_year(year)
        integer(int64), intent(in) :: year

        is_date_leap_year = (modulo(year, 4_int64) == 0 .and. modulo(year, 100_int64) /= 0) &
                            .or. modulo(year, 400_int64) == 0
    end function is_date_leap_year

    ! The day number within the year of the decoded time TIME.  Port of
    ! time-date.el:time-date--day-in-year.
    integer function day_in_year(time) result(day)
        type(decoded_time_type), intent(in) :: time

        day = time%day + 31*(time%month - 1)

        if (time%month > 2) then
            day = day - (23 + 4*time%month)/10
            if (is_date_leap_year(time%year)) day = day + 1
        end if
    end function day_in_year

    !> Return the day number within the year corresponding to TIME (absent:
    !> the current time), in local time.  Port of
    !> time-date.el:time-to-day-in-year.
    integer function time_to_day_in_year(time, status) result(day)
        type(time_type), intent(in), optional :: time
        type(status_type), intent(out), optional :: status
        type(status_type) :: st

        day = day_in_year(decode_time(time, status=st))
        if (present(status)) status = st
    end function time_to_day_in_year

    !> The absolute pseudo-Gregorian date for TIME, a time value (absent:
    !> the current time), in local time.  The absolute date is the number
    !> of days elapsed since the imaginary Gregorian date Sunday, December
    !> 31, 1 BC.  Port of time-date.el:time-to-days.
    integer(int64) function time_to_days(time, status) result(days)
        type(time_type), intent(in), optional :: time
        type(status_type), intent(out), optional :: status
        type(decoded_time_type) :: decoded
        type(status_type) :: st
        integer(int64) :: year

        decoded = decode_time(time, status=st)
        if (present(status)) status = st
        year = decoded%year
        ! Days this year, plus days in prior years, plus Julian leap years,
        ! minus century years, plus Gregorian leap years (Lisp's `/'
        ! truncates).
        days = day_in_year(decoded) + 365*(year - 1) + (year - 1)/4 - (year - 1)/100 + (year - 1)/400
    end function time_to_days

    ! ------------------------------------------------------------------
    ! Months and decoded times
    ! ------------------------------------------------------------------

    !> The number of days in MONTH in YEAR.  A MONTH outside 1 to 12 is the
    !> error "Month MONTH is invalid" in STATUS.  Port of
    !> time-date.el:date-days-in-month.
    integer function date_days_in_month(year, month, status) result(days)
        integer(int64), intent(in) :: year
        integer, intent(in) :: month
        type(status_type), intent(out), optional :: status
        character(len=24) :: text

        if (present(status)) status = status_type()
        days = 0

        if (month < 1 .or. month > 12) then
            write (text, "(i0)") month
            if (present(status)) call set_error(status, GENERIC_ERROR_CODE, "Month "//trim(text)//" is invalid")
            return
        end if

        select case (month)
        case (2)
            days = merge(29, 28, is_date_leap_year(year))
        case (1, 3, 5, 7, 8, 10, 12)
            days = 31
        case default
            days = 30
        end select
    end function date_days_in_month

    !> Convert a YEAR/ORDINAL to the equivalent decoded time.  ORDINAL is
    !> the number of days since the start of the year, with January 1st
    !> being 1.  Only the day, month and year are known; DAYLIGHT_SAVING_TIME is
    !> DECODED_TIME_DAYLIGHT_SAVING_TIME_NO.  Port of time-date.el:date-ordinal-to-time.
    function date_ordinal_to_time(year, ordinal) result(time)
        integer(int64), intent(in) :: year
        integer, intent(in) :: ordinal
        type(partial_decoded_time_type) :: time
        integer :: day, month

        day = ordinal
        month = 1

        do while (day > date_days_in_month(year, month))
            day = day - date_days_in_month(year, month)
            month = month + 1
        end do

        time%day = day
        time%month = month
        time%year = year
        time%daylight_saving_time = DECODED_TIME_DAYLIGHT_SAVING_TIME_NO
    end function date_ordinal_to_time

    ! Set STATUS to the wrong-type error of a component that is unknown
    ! where a number is needed.  Not a port.
    subroutine unknown_component(status)
        type(status_type), intent(inout) :: status

        call set_error(status, WRONG_TYPE_ARGUMENT_CODE, "expected integer, got unallocated")
    end subroutine unknown_component

    ! The number of days in the month of TIME: date_days_in_month of its
    ! year and month, whose year is needed only for February (an unknown
    ! month is the error "Month unallocated is invalid").  Not a port.
    integer function month_days(time, status) result(days)
        type(partial_decoded_time_type), intent(in) :: time
        type(status_type), intent(inout) :: status

        days = 0

        if (.not. allocated(time%month)) then
            call set_error(status, GENERIC_ERROR_CODE, "Month unallocated is invalid")
        else if (time%month == 2 .and. .not. allocated(time%year)) then
            call unknown_component(status)
        else if (allocated(time%year)) then
            days = date_days_in_month(time%year, time%month, status)
        else
            ! Any year but a leap year's February, which is excluded above.
            days = date_days_in_month(1_int64, time%month, status)
        end if
    end function month_days

    !> Add DELTA to TIME, both of which are decoded times.  TIME should
    !> represent a time, while DELTA should have known components only for
    !> the values that should be altered.
    !>
    !> For instance, if you want to "add two months" to TIME, then leave
    !> all other components but the month component in DELTA unknown, and
    !> make the month component 2.  For instance:
    !>
    !>   add_decoded_time(time, partial_decoded_time_type(month=2))
    !>
    !> The values in DELTA can be negative.
    !>
    !> If applying a month/year delta leaves the time spec invalid, it is
    !> decreased to be valid ("add one month" to January 31st 2019 will
    !> yield a result of February 28th 2019 and "add one year" to February
    !> 29th 2020 will result in February 28th 2021).
    !>
    !> Components are added in a most to least significant order, so if the
    !> adjustment described above happens, it happens before adding days,
    !> hours, minutes or seconds.
    !>
    !> When changing the time bits in TIME (i.e., second/minute/hour),
    !> changes in daylight saving time are not taken into account.
    !>
    !> A component of TIME that is unknown where it is needed is an error
    !> in STATUS, as upstream's arithmetic on an unknown component.  Port of
    !> time-date.el:decoded-time-add.
    function add_decoded_time(time, delta, status) result(sum)
        type(partial_decoded_time_type), intent(in) :: time, delta
        type(status_type), intent(out), optional :: status
        type(partial_decoded_time_type) :: sum
        type(status_type) :: st
        type(time_type) :: seconds
        integer :: new, days, k
        logical :: increase

        sum = time

        body: block
            ! Years are simple.
            if (allocated(delta%year)) then
                if (.not. allocated(sum%year)) then
                    call unknown_component(st)
                    exit body
                end if

                sum%year = sum%year + delta%year
            end if

            ! Months are pretty simple, but start at 1 (for January).
            if (allocated(delta%month)) then
                if (.not. allocated(sum%month) .or. .not. allocated(sum%year)) then
                    call unknown_component(st)
                    exit body
                end if

                new = sum%month - 1 + delta%month
                sum%month = 1 + modulo(new, 12)
                sum%year = sum%year + (new - modulo(new, 12))/12
            end if

            ! Adjust for month length (as described in the doc string).
            days = month_days(sum, st)
            if (.not. st%ok()) exit body

            if (.not. allocated(sum%day)) then
                call unknown_component(st)
                exit body
            end if

            sum%day = min(days, sum%day)

            ! Days are iterative.
            if (allocated(delta%day)) then
                increase = delta%day > 0

                do k = 1, abs(delta%day)
                    call alter_day(sum, increase, st)
                    if (.not. st%ok()) exit body
                end do

                if (.not. st%ok()) exit body
            end if

            ! Do the time part, which is pretty simple (except for leap
            ! seconds, I guess).  Time zone adjustments are basically the
            ! same as time adjustments.
            if (allocated(delta%second)) then
                seconds = delta%second
            else
                seconds = time_type()
            end if

            seconds = time_add(seconds, time_type(seconds=3600_int64*known(delta%hour) + 60_int64*known(delta%minute) &
                                                  + known(delta%zone)))
            call alter_second(sum, seconds, st)
        end block body

        if (present(status)) status = st
    end function add_decoded_time

    ! The value of the optional component VALUE, 0 if unknown.  Not a port.
    integer function known(value)
        integer, allocatable, intent(in) :: value

        known = 0
        if (allocated(value)) known = value
    end function known

    ! Increase or decrease the month in TIME by 1 (an unknown year where it
    ! changes is an error in STATUS).  Port of
    ! time-date.el:decoded-time--alter-month.
    subroutine alter_month(time, increase, status)
        type(partial_decoded_time_type), intent(inout) :: time
        logical, intent(in) :: increase
        type(status_type), intent(inout) :: status

        if (increase) then
            time%month = time%month + 1
            if (time%month <= 12) return
            time%month = 1
        else
            time%month = time%month - 1
            if (time%month /= 0) return
            time%month = 12
        end if

        if (.not. allocated(time%year)) then
            call unknown_component(status)
        else if (increase) then
            time%year = time%year + 1
        else
            time%year = time%year - 1
        end if
    end subroutine alter_month

    ! Increase or decrease the day in TIME by 1 (an error in STATUS as
    ! `month_days' and `alter_month' give it).  Port of
    ! time-date.el:decoded-time--alter-day.
    subroutine alter_day(time, increase, status)
        type(partial_decoded_time_type), intent(inout) :: time
        logical, intent(in) :: increase
        type(status_type), intent(inout) :: status
        integer :: days

        if (increase) then
            time%day = time%day + 1
            days = month_days(time, status)
            if (.not. status%ok()) return

            if (time%day > days) then
                time%day = 1
                call alter_month(time, .true., status)
            end if
        else
            time%day = time%day - 1

            if (time%day == 0) then
                call alter_month(time, .false., status)
                if (.not. status%ok()) return
                time%day = month_days(time, status)
            end if
        end if
    end subroutine alter_day

    ! Increase the time in TIME by SECONDS (an error in STATUS as
    ! `alter_day' gives it).  Port of
    ! time-date.el:decoded-time--alter-second.
    subroutine alter_second(time, seconds, status)
        type(partial_decoded_time_type), intent(inout) :: time
        type(time_type), intent(in) :: seconds
        type(status_type), intent(inout) :: status
        type(time_type) :: time_second, old, new, seconds_per_day
        integer(int64) :: second

        if (allocated(time%second)) then
            time_second = time%second
        else
            time_second = time_type()
        end if

        old = time_add(time_second, time_type(seconds=3600_int64*known(time%hour) + 60_int64*known(time%minute)))
        new = time_add(old, seconds)
        seconds_per_day = time_type(seconds=86400_int64, count_rate=new%count_rate)

        ! Hm...  DAYLIGHT_SAVING_TIME...
        do while (is_time_less(new, time_type()))
            call alter_day(time, .false., status)
            if (.not. status%ok()) return
            new = time_add(new, seconds_per_day)
        end do

        do while (.not. is_time_less(new, seconds_per_day))
            call alter_day(time, .true., status)
            if (.not. status%ok()) return
            new = time_subtract(new, seconds_per_day)
        end do

        second = new%seconds
        time%second = time_type(mod(second, 60_int64), new%count, new%count_rate)
        time%minute = int(mod(second/60, 60_int64))
        time%hour = int(second/3600)
    end subroutine alter_second

    !> Set most unknown components of the decoded time TIME to default
    !> values.  This can set TIME's year, month, day, hour, minute and
    !> second.  The default value is based on January 1st, 1970 at
    !> midnight.  This year is used to guarantee portability; see Info node
    !> `(elisp) Time of Day'.
    !>
    !> Optional argument DEFAULT_ZONE specifies what time zone to default to
    !> when TIME's time zone is unknown (meaning local time).
    !>
    !> TIME is modified.  Port of time-date.el:decoded-time-set-defaults.
    subroutine set_decoded_time_defaults(time, default_zone)
        type(partial_decoded_time_type), intent(inout) :: time
        integer, intent(in), optional :: default_zone

        if (.not. allocated(time%second)) time%second = time_type()
        if (.not. allocated(time%minute)) time%minute = 0
        if (.not. allocated(time%hour)) time%hour = 0
        if (.not. allocated(time%day)) time%day = 1
        if (.not. allocated(time%month)) time%month = 1
        if (.not. allocated(time%year)) time%year = 1970
        if (.not. allocated(time%zone) .and. present(default_zone)) time%zone = default_zone
        ! Do not set the weekday or DAYLIGHT_SAVING_TIME, as `encode_time' can infer them
        ! well enough when unknown.
    end subroutine set_decoded_time_defaults

    !> Interpret TIME as a period and return its length in seconds.  For
    !> computational purposes, years are 365 days long and months are 30
    !> days long.  Unknown components count as 0.  Port of
    !> time-date.el:decoded-time-period.
    real(real64) function decoded_time_period(time) result(period)
        type(partial_decoded_time_type), intent(in) :: time

        period = 0

        if (allocated(time%second)) then
            ! A fractional second.
            period = real(time%second)
        end if

        period = period + real(known(time%minute), real64)*60 + real(known(time%hour), real64)*60*60 &
                 + real(known(time%day), real64)*60*60*24 + real(known(time%month), real64)*60*60*24*30
        if (allocated(time%year)) period = period + real(time%year, real64)*60*60*24*365
    end function decoded_time_period

    !> Convert the decoded time TIME to a time value, as `encode_time' does
    !> (its weekday ignored).  An unknown second, minute, hour, day, month
    !> or year is a `wrong_type_argument' error condition in STATUS; an
    !> unknown zone is local time.  Port of timefns.c:Fencode_time (a
    !> decoded time with unknown components: check_tm_member).
    type(time_type) function encode_partial_decoded_time(time, status) result(encoded)
        use time_functions_mod, only: encode_time
        type(partial_decoded_time_type), intent(in) :: time
        type(status_type), intent(out) :: status
        type(decoded_time_type) :: decoded

        status = status_type()
        encoded = time_type()

        if (.not. allocated(time%second) .or. .not. allocated(time%minute) .or. .not. allocated(time%hour) .or. &
            .not. allocated(time%day) .or. .not. allocated(time%month) .or. .not. allocated(time%year)) then
            call unknown_component(status)
            return
        end if

        decoded%second = time%second
        decoded%minute = time%minute
        decoded%hour = time%hour
        decoded%day = time%day
        decoded%month = time%month
        decoded%year = time%year
        decoded%daylight_saving_time = time%daylight_saving_time
        if (allocated(time%zone)) decoded%zone = time%zone
        encoded = encode_time(decoded, status=status)
    end function encode_partial_decoded_time

    ! ------------------------------------------------------------------
    ! Intervals as text
    ! ------------------------------------------------------------------

    !> Use format control STRING to format the number SECONDS.  The valid
    !> format specifiers are:
    !> %y is the number of (365-day) years.
    !> %d is the number of days.
    !> %h is the number of hours.
    !> %m is the number of minutes.
    !> %s is the number of seconds.
    !> %z is a non-printing control flag (see below).
    !> %% is a literal "%".
    !>
    !> Upper-case specifiers are followed by the unit-name (e.g. "years").
    !> Lower-case specifiers return only the unit.
    !>
    !> "%" may be followed by a number specifying a width, with an optional
    !> leading "." for zero-padding.  For example, "%.3Y" will return
    !> something of the form "001 year".
    !>
    !> The "%s" spec takes an additional optional parameter, introduced by
    !> the "," character, to say how many decimals to use.  "%,1s" means
    !> "use one decimal".
    !>
    !> The "%z" specifier does not print anything.  When it is used,
    !> specifiers must be given in order of decreasing size.  To the left of
    !> "%z", nothing is output until the first non-zero unit is encountered.
    !>
    !> The "%x" specifier does not print anything.  When it is used,
    !> specifiers must be given in order of decreasing size.  To the right
    !> of "%x", trailing zero units are not output.
    !>
    !> A bad specifier, one given twice and units out of order with "%z"
    !> or "%x" are errors in STATUS.  A time value is given as real(TIME).
    !> Port of time-date.el:format-seconds.
    function format_seconds(string, seconds, status) result(text)
        character(len=*), intent(in) :: string
        real(real64), intent(in) :: seconds
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: text
        logical :: saved_fold

        saved_fold = case_fold_search%value
        case_fold_search%value = .true.
        text = format_seconds_1(string, seconds, status)
        case_fold_search%value = saved_fold
    end function format_seconds

    ! The body of `format_seconds', with case_fold_search true.  Port of
    ! time-date.el:format-seconds.
    function format_seconds_1(string, seconds, status) result(text)
        character(len=*), intent(in) :: string
        real(real64), intent(in) :: seconds
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: text
        character(len=:), allocatable :: specification, lower, minus, pre, width, precision, suffix
        character(len=1) :: used(size(FORMAT_UNIT_SPECIFICATIONS))
        type(status_type) :: st
        integer(int64) :: start, unit, previous, whole, number, beginning, leading_zero_position, trailing_zero_position, &
                          chop_leading
        integer :: u, n_used
        logical :: zero_flag, larger, has_previous, has_leading, has_trailing, has_chop_leading, chop_trailing, is_zero
        real(real64) :: fraction, value

        text = ""
        start = 1
        n_used = 0
        zero_flag = .false.
        larger = .false.
        has_previous = .false.
        previous = 0

        do while (string_match("%\.?[0-9]*\(,[0-9]\)?\(.\)", string, st, start=start) > 0)
            start = match_end(0, st)
            specification = match_string(2, string)
            if (specification == "%") cycle
            lower = ascii_downcase(specification)
            u = findloc(FORMAT_UNIT_SPECIFICATIONS == lower, .true., dim=1)

            if (u == 0) then
                call set_error(status, GENERIC_ERROR_CODE, "Bad format specifier: `"//specification//"'")
                return
            end if

            if (any(used(1:n_used) == lower)) then
                call set_error(status, GENERIC_ERROR_CODE, "Multiple instances of specifier: `"//specification//"'")
                return
            end if

            if (u > size(FORMAT_UNIT_SECONDS)) then
                zero_flag = .true.
            else if (.not. larger) then
                unit = FORMAT_UNIT_SECONDS(u)
                larger = has_previous .and. unit > previous
                previous = unit
                has_previous = .true.
            end if

            n_used = n_used + 1
            used(n_used) = lower
        end do

        if (zero_flag .and. larger) then
            call set_error(status, GENERIC_ERROR_CODE, "Units are not in decreasing order of size")
            return
        end if

        ! Treat -0.0 like 0.0.
        minus = ""
        if (seconds < 0) minus = "-"
        whole = floor(abs(seconds), int64)
        fraction = abs(seconds) - real(whole, real64)
        text = string
        has_leading = .false.
        has_trailing = .false.
        has_chop_leading = .false.
        chop_trailing = .false.
        leading_zero_position = 0
        trailing_zero_position = 0
        chop_leading = 0

        do u = 1, size(FORMAT_UNIT_SPECIFICATIONS)
            if (string_match("%\(\.?[0-9]+\)?\(,[0-9]+\)?\("//FORMAT_UNIT_SPECIFICATIONS(u)//"\)", text, st) == 0) cycle
            ! Positions from here on count from 0, as upstream's.
            beginning = match_beginning(0, st) - 1

            select case (FORMAT_UNIT_SPECIFICATIONS(u))
            case ("z")
                if (has_leading) then
                    chop_leading = min(leading_zero_position, beginning)
                else
                    ! The entire spec is zero, get past "%z" to last 0.
                    chop_leading = beginning + 2
                end if

                has_chop_leading = .true.
            case ("x")
                chop_trailing = .true.
            case default
                ! Cf article-make-date-line in gnus-art.
                unit = FORMAT_UNIT_SECONDS(u)
                number = whole/unit
                whole = whole - number*unit

                if (unit == 1) then
                    is_zero = real(number, real64) + fraction == 0
                    value = real(number, real64) + fraction
                else
                    is_zero = number == 0
                    value = real(number, real64)
                end if

                ! Start position of the first non-zero unit.
                if (.not. has_leading .and. .not. is_zero) then
                    leading_zero_position = beginning
                    has_leading = .true.
                end if

                if (.not. is_zero) has_trailing = .false.

                if (.not. has_trailing .and. is_zero) then
                    trailing_zero_position = beginning
                    has_trailing = .true.
                end if

                width = match_string(1, text)

                if (len(width) > 0) then
                    if (width(1:1) == ".") width = "0"//width(2:)
                end if

                if (match_string(3, text) == FORMAT_UNIT_SPECIFICATIONS(u)) then
                    ! Lower-case, no unit-name.
                    suffix = ""
                else if (number == 1) then
                    suffix = " "//trim(FORMAT_UNIT_NAMES(u))
                else
                    suffix = " "//trim(FORMAT_UNIT_NAMES(u))//"s"
                end if

                if (match_beginning(2, st) > 0) then
                    precision = match_string(2, text)
                    text = text(1:offset(text, beginning))//format_real(value, width, precision(2:))//suffix// &
                           text(offset(text, match_end(0, st) - 1) + 1:)
                else
                    text = text(1:offset(text, beginning))//format_integer(int(value, int64), width)//suffix// &
                           text(offset(text, match_end(0, st) - 1) + 1:)
                end if
            end select
        end do

        pre = text
        if (chop_trailing .and. has_trailing) text = text(1:offset(text, trailing_zero_position))
        if (has_chop_leading) text = text(offset(text, chop_leading) + 1:)
        ! If we ended up removing everything, return the formatted string
        ! in full.
        if (len(text) == 0) text = pre
        text = minus//trim_whitespace(replace_percents(remove_flags(text)))
    end function format_seconds_1

    ! The byte offset of the character POSITION (counted from 0) of TEXT,
    ! at most its length.  Not a port.
    integer(int64) function offset(text, position)
        use search_mod, only: string_index_to_offset
        character(len=*), intent(in) :: text
        integer(int64), intent(in) :: position

        offset = min(string_index_to_offset(text, position + 1), len(text, kind=int64))
    end function offset

    ! TEXT with ASCII upper-case letters in lower case.  Not a port.
    pure function ascii_downcase(text) result(lower)
        character(len=*), intent(in) :: text
        character(len=len(text)) :: lower
        integer :: k

        lower = text

        do k = 1, len(text)
            if (lge(text(k:k), "A") .and. lle(text(k:k), "Z")) lower(k:k) = achar(iachar(text(k:k)) + 32)
        end do
    end function ascii_downcase

    ! The integer VALUE as format's "%WIDTHd" writes it (WIDTH with a
    ! leading 0 pads with zeros).  Not a port.
    function format_integer(value, width) result(text)
        integer(int64), intent(in) :: value
        character(len=*), intent(in) :: width
        character(len=:), allocatable :: text
        character(len=24) :: buffer

        write (buffer, "(i0)") value
        text = pad(trim(buffer), width)
    end function format_integer

    ! The real VALUE as format's "%WIDTH.PRECISIONf" writes it.  Not a port.
    function format_real(value, width, precision) result(text)
        real(real64), intent(in) :: value
        character(len=*), intent(in) :: width, precision
        character(len=:), allocatable :: text
        character(len=64) :: buffer
        character(len=16) :: format
        integer :: digits, ios

        read (precision, *, iostat=ios) digits
        if (ios /= 0) digits = 0
        write (format, '("(f0.", i0, ")")') digits
        write (buffer, format) value
        buffer = adjustl(buffer)
        if (buffer(1:1) == ".") buffer = "0"//buffer(1:len(buffer) - 1)
        text = trim(buffer)
        if (digits == 0 .and. text(len(text):len(text)) == ".") text = text(1:len(text) - 1)
        text = pad(text, width)
    end function format_real

    ! TEXT right-aligned in the field WIDTH ("" for none), with zeros if
    ! WIDTH starts with 0, else spaces.  Not a port.
    function pad(text, width) result(padded)
        character(len=*), intent(in) :: text, width
        character(len=:), allocatable :: padded
        integer :: field, ios

        padded = text
        if (len(width) == 0) return
        read (width, *, iostat=ios) field
        if (ios /= 0) return
        if (field <= len(text)) return

        if (width(1:1) == "0") then
            padded = repeat("0", field - len(text))//text
        else
            padded = repeat(" ", field - len(text))//text
        end if
    end function pad

    ! TEXT without its "%z" and "%x" flags (either case).  Not a port.
    function remove_flags(text) result(removed)
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: removed
        integer :: k

        removed = ""
        k = 1

        do while (k <= len(text))
            if (text(k:k) == "%" .and. k < len(text)) then
                if (scan(text(k + 1:k + 1), "zxZX") > 0) then
                    k = k + 2
                    cycle
                end if
            end if

            removed = removed//text(k:k)
            k = k + 1
        end do
    end function remove_flags

    ! TEXT with each "%%" replaced by "%".  Not a port.
    function replace_percents(text) result(replaced)
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: replaced
        integer :: k

        replaced = ""
        k = 1

        do while (k <= len(text))
            replaced = replaced//text(k:k)

            if (text(k:k) == "%" .and. k < len(text)) then
                if (text(k + 1:k + 1) == "%") k = k + 1
            end if

            k = k + 1
        end do
    end function replace_percents

    ! TEXT without leading and trailing spaces, tabs, newlines and
    ! returns (string-trim).  Not a port.
    function trim_whitespace(text) result(trimmed)
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: trimmed
        character(len=*), parameter :: WHITESPACE = " "//achar(9)//achar(10)//achar(13)
        integer :: first, last

        first = verify(text, WHITESPACE)

        if (first == 0) then
            trimmed = ""
            return
        end if

        last = verify(text, WHITESPACE, back=.true.)
        trimmed = text(first:last)
    end function trim_whitespace

    !> Convert time interval DELAY (in seconds) to a string.
    !> By default, the returned string is formatted as a float in the smallest
    !> unit from the variable `seconds_to_string' that is longer than DELAY,
    !> and a precision of two.  If READABLE is `SECONDS_TO_STRING_READABLE',
    !> convert DELAY into a readable string, using the information provided
    !> in the variable `seconds_to_string_readable'.  If it is
    !> `SECONDS_TO_STRING_EXPANDED', use two units to describe DELAY, if
    !> appropriate.  E.g. "1 hour 32 minutes".  If ABBREVIATION is true,
    !> abbreviate the readable units.  If PRECISION is a whole number, round
    !> the value associated with the smallest displayed unit to that many
    !> digits after the decimal.  If PRECISION_FRACTION is a non-negative
    !> float less than 1.0, round to that value.  Port of
    !> time-date.el:seconds-to-string.
    recursive function seconds_to_string(delay, readable, abbreviation, precision, precision_fraction) result(s)
        real(real64), intent(in) :: delay
        integer, intent(in), optional :: readable
        logical, intent(in), optional :: abbreviation
        integer, intent(in), optional :: precision
        real(real64), intent(in), optional :: precision_fraction
        character(len=:), allocatable :: s
        integer :: how, digits, here, here_pre, k
        logical :: abbreviate, expanded, isfloatp, has_pre
        real(real64) :: round_to, count, count_pre, count_value
        character(len=:), allocatable :: padding
        character(len=64) :: buffer
        character(len=16) :: format

        how = SECONDS_TO_STRING_PLAIN
        if (present(readable)) how = readable
        abbreviate = .false.
        if (present(abbreviation)) abbreviate = abbreviation

        if (delay < 0) then
            ! (Upstream passes PRECISION as ABBREVIATION here.)
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

            do while (here < size(READABLE_SECONDS))
                if (delay/real(READABLE_SECONDS(here), real64) >= 1) exit
                here = here + 1
            end do

            has_pre = .false.
            here_pre = 0
            count_pre = 0

            if (expanded .and. here < size(READABLE_SECONDS)) then
                here_pre = here
                here = here + 1
                count_pre = floor(delay/real(READABLE_SECONDS(here_pre), real64))
                count = lisp_round((delay - count_pre*real(READABLE_SECONDS(here_pre), real64)) &
                                   /real(READABLE_SECONDS(here), real64), round_to)

                if (count > 0) then
                    has_pre = .true.
                else
                    count = count_pre
                    here = here_pre
                end if
            end if

            if (.not. has_pre) count = lisp_round(delay/real(READABLE_SECONDS(here), real64), round_to)
            count_value = count*round_to
            isfloatp = digits > 0 .and. count_value - floor(count_value) > 0
            s = ""

            if (has_pre) then
                write (buffer, "(i0)") int(count_pre, int64)
                s = trim(buffer)//padding//unit_name(count_pre, here_pre, abbreviate, .false.)//" "
            end if

            if (isfloatp) then
                write (format, '("(f0.", i0, ")")') digits
                write (buffer, format) count_value
                buffer = adjustl(buffer)
                if (buffer(1:1) == ".") buffer = "0"//buffer(1:len(buffer) - 1)
            else
                write (buffer, "(i0)") floor(count_value, int64)
            end if

            ! Float formats are always plural.
            s = s//trim(buffer)//padding//unit_name(count_value, here, abbreviate, isfloatp)
        else if (.not. (delay > 0)) then
            s = "0s"
        else
            k = 1

            do while (k <= size(PLAIN_LIMITS))
                if (.not. (PLAIN_LIMITS(k) <= delay)) exit
                k = k + 1
            end do

            write (buffer, "(f0.2)") delay/PLAIN_SECONDS(k)
            buffer = adjustl(buffer)
            if (buffer(1:1) == ".") buffer = "0"//buffer(1:len(buffer) - 1)
            s = trim(buffer)//PLAIN_UNITS(k) (1:PLAIN_UNIT_LENGTH(k))
        end if
    end function seconds_to_string

    ! The unit HERE for VALUE: abbreviated, singular or PLURAL.  Not a port.
    function unit_name(value, here, abbreviation, plural) result(name)
        real(real64), intent(in) :: value
        integer, intent(in) :: here
        logical, intent(in) :: abbreviation, plural
        character(len=:), allocatable :: name

        if (abbreviation) then
            name = trim(READABLE_ABBREVIATION(here))
        else if (.not. plural .and. floor(value) <= 1) then
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
end module time_date_mod
