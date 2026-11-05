! iso8601_mod.f90 -- parse ISO 8601 date/time strings.
! Ported from GNU Emacs 31.1 lisp/calendar/iso8601.el.
!
! Copyright (C) 2019-2026 Free Software Foundation, Inc.
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

!> ISO 8601 times basically look like 1985-04-01T15:23:49...  Or so you'd
!> think.  This is what everybody means when they say "ISO8601", but it's
!> in reality a quite large collection of syntaxes, including week
!> numbers, ordinal dates, durations and intervals.  This module has
!> functions for parsing them all: `parse_iso8601', `parse_iso8601_date',
!> `parse_iso8601_time', `parse_iso8601_zone', `parse_iso8601_duration'
!> and `parse_iso8601_interval'.  They all return decoded times
!> (time_date_mod's `partial_decoded_time_type'), except the last one,
!> which returns three of them:
!>
!>   parse_iso8601_interval("P1Y2M10DT2H30M/2008W32T153000-01", ...)
!>
!> gives 2007-05-24 13:00:00, 2008-08-03 15:30:00 (both with zone -3600)
!> and the duration of 1 year, 2 months, 10 days, 2 hours and 30 minutes.
!>
!> Text that is not ISO 8601 is a `wrong_type_argument' error condition
!> in STATUS.  The standard can be found at
!> https://www.loc.gov/standards/datetime/iso-tc154-wg5_n0038_iso_wd_8601-1_2016-02-16.pdf
!> RFC 3339 defines the subset that everybody thinks of as "ISO8601".
!> What is not ported yet is listed in docs/status/iso8601_mod.md.
module iso8601_mod
    use, intrinsic :: iso_fortran_env, only: int64, real64
    use, intrinsic :: ieee_arithmetic, only: ieee_rint
    use error_mod, only: status_type, set_error, FM_WRONG_TYPE_ARGUMENT
    use time_functions_mod, only: time_type, decoded_time_type, decode_time, time_subtract, time_zone_rule, &
                                  DECODED_TIME_DST_UNKNOWN, DECODED_TIME_DST_NO
    use time_date_mod, only: partial_decoded_time_type, is_date_leap_year, date_ordinal_to_time, add_decoded_time, &
                             set_decoded_time_defaults, encode_partial_decoded_time
    use search_mod, only: string_match, match_beginning
    use basic_subprograms_mod, only: match_string
    use data_mod, only: string_to_number
    implicit none(type, external)
    private

    public :: parse_iso8601, parse_iso8601_date, parse_iso8601_time, parse_iso8601_zone, is_iso8601_valid, &
              parse_iso8601_duration, parse_iso8601_interval

    character(len=*), parameter :: YEAR_MATCH = "\([+-]?[0-9][0-9][0-9][0-9]\)"
    character(len=*), parameter :: FULL_DATE_MATCH = "\([+-]?[0-9][0-9][0-9][0-9]\)-?\([0-9][0-9]\)-?\([0-9][0-9]\)"
    character(len=*), parameter :: WITHOUT_DAY_MATCH = "\([+-]?[0-9][0-9][0-9][0-9]\)-\([0-9][0-9]\)"
    character(len=*), parameter :: OUTDATED_DATE_MATCH = "--\([0-9][0-9]\)-?\([0-9][0-9]\)"
    character(len=*), parameter :: OUTDATED_REDUCED_PRECISION_DATE_MATCH = "---?\([0-9][0-9]\)"
    character(len=*), parameter :: WEEK_DATE_MATCH = "\([+-]?[0-9][0-9][0-9][0-9]\)-?W\([0-9][0-9]\)-?\([0-9]\)?"
    character(len=*), parameter :: ORDINAL_DATE_MATCH = "\([+-]?[0-9][0-9][0-9][0-9]\)-?\([0-9][0-9][0-9]\)"
    character(len=*), parameter :: TIME_MATCH = "\([0-9][0-9]\):?\([0-9][0-9]\)?:?\([0-9][0-9]\)?[.,]?\([0-9]*\)"
    character(len=*), parameter :: ZONE_MATCH = "\(Z\|\([+-]\)\([0-9][0-9]\):?\([0-9][0-9]\)?\)"
    character(len=*), parameter :: DURATION_FULL_MATCH = &
                                   "P\([0-9]+Y\)?\([0-9]+M\)?\([0-9]+D\)?\(T\([0-9]+H\)?\([0-9]+M\)?\([0-9]+S\)?\)?"
    character(len=*), parameter :: DURATION_WEEK_MATCH = "P\([0-9]+\)W"

contains

    ! ------------------------------------------------------------------
    ! The regular expressions
    ! ------------------------------------------------------------------

    ! REGULAR_EXPRESSION with its groups made shy: each "(" replaced by
    ! "(?:" (string-replace).  Not a port.
    pure function shy(regular_expression) result(shy_expression)
        character(len=*), intent(in) :: regular_expression
        character(len=:), allocatable :: shy_expression
        integer :: k

        shy_expression = ""

        do k = 1, len(regular_expression)
            if (regular_expression(k:k) == "(") then
                shy_expression = shy_expression//"(?:"
            else
                shy_expression = shy_expression//regular_expression(k:k)
            end if
        end do
    end function shy

    ! The date forms, one alternative each, their groups shy.  Port of
    ! iso8601.el:iso8601--date-match (with iso8601--concat-regexps).
    pure function date_match() result(regular_expression)
        character(len=:), allocatable :: regular_expression

        regular_expression = "\(?:"//shy(YEAR_MATCH)//"\)\|\(?:"//shy(FULL_DATE_MATCH)//"\)\|\(?:"// &
                             shy(WITHOUT_DAY_MATCH)//"\)\|\(?:"//shy(OUTDATED_DATE_MATCH)//"\)\|\(?:"// &
                             shy(OUTDATED_REDUCED_PRECISION_DATE_MATCH)//"\)\|\(?:"//shy(WEEK_DATE_MATCH)// &
                             "\)\|\(?:"//shy(ORDINAL_DATE_MATCH)//"\)"
    end function date_match

    ! A time and an optional zone: group 1 the time, 2 the zone.  Port of
    ! iso8601.el:iso8601--full-time-match.
    pure function full_time_match() result(regular_expression)
        character(len=:), allocatable :: regular_expression

        regular_expression = "\("//shy(TIME_MATCH)//"\)\("//ZONE_MATCH//"\)?"
    end function full_time_match

    ! A date with an optional time and zone: group 1 the date, 2 the time,
    ! 3 the zone.  Port of iso8601.el:iso8601--combined-match.
    pure function combined_match() result(regular_expression)
        character(len=:), allocatable :: regular_expression

        regular_expression = "\("//date_match()//"\)\(?:T\("//shy(TIME_MATCH)//"\)\("//ZONE_MATCH//"\)?\)?"
    end function combined_match

    ! Whether REGULAR_EXPRESSION matches all of STRING (the match data set).
    ! Port of iso8601.el:iso8601--match.
    logical function is_match(regular_expression, string)
        character(len=*), intent(in) :: regular_expression, string
        type(status_type) :: st

        is_match = string_match("\`"//regular_expression//"\'", string, st) > 0
    end function is_match

    ! Whether group N of the last match matched.  Not a port.
    logical function has_group(n)
        integer, intent(in) :: n
        type(status_type) :: st

        has_group = match_beginning(n, st) > 0
    end function has_group

    ! The number at the start of TEXT, 0 if none (string-to-number).
    ! Not a port.
    integer(int64) function number_of(text) result(number)
        character(len=*), intent(in) :: text

        call string_to_number(text, number)
    end function number_of

    ! Set STATUS to the error for STRING, which is not ISO 8601.
    ! Not a port.
    subroutine not_iso8601(string, status)
        character(len=*), intent(in) :: string
        type(status_type), intent(inout) :: status

        call set_error(status, FM_WRONG_TYPE_ARGUMENT, "expected ISO 8601 text, got """//string//"""")
    end subroutine not_iso8601

    ! The DST of a decoded time with ZONE (absent: no zone): "Z" and "z"
    ! are standard time; no zone and [-+][0-9][0-9]... are local time with
    ! unknown DST.  Port of iso8601.el:iso8601--zone-dst.
    integer function zone_dst(zone)
        character(len=*), intent(in), optional :: zone

        zone_dst = DECODED_TIME_DST_UNKNOWN

        if (present(zone)) then
            if (len(zone) == 1) zone_dst = DECODED_TIME_DST_NO
        end if
    end function zone_dst

    ! ------------------------------------------------------------------
    ! Parsing
    ! ------------------------------------------------------------------

    !> Parse an ISO 8601 date/time string and return a decoded time.
    !>
    !> The ISO 8601 date/time strings look like "2008-03-02T13:47:30" or
    !> "2024-04-05T14:30Z" or "2024-04-05T12:30-02:00", but shorter,
    !> incomplete strings like "2008-03-02" are valid, as well as variants
    !> like "2008W32" (week number) and "2008-234" (ordinal day number).
    !> Values returned are identical to those of `decode_time', except that
    !> an unknown DST value is DECODED_TIME_DST_UNKNOWN and other unknown
    !> values are unallocated.
    !>
    !> Note that, unlike `decode_time', this function does not interpret
    !> the time string, and in particular the time-zone designator or UTC
    !> offset that is part of STRING does not affect the returned value of
    !> date and time, it only affects the last two components of the
    !> returned value.  This function simply parses the textual
    !> representation of date and time into separate numerical values, and
    !> doesn't care whether the time is local or UTC.
    !>
    !> If KEEP_FRACTION is true, a decimal fraction is kept, as
    !> `decode_time' keeps a time value's precision: of a second in the
    !> second's count, of a minute as seconds and of an hour as minutes;
    !> otherwise it is dropped.  Port of iso8601.el:iso8601-parse (FORM t
    !> is KEEP_FRACTION true).
    function parse_iso8601(string, status, keep_fraction) result(date)
        character(len=*), intent(in) :: string
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: keep_fraction
        type(partial_decoded_time_type) :: date
        type(partial_decoded_time_type) :: time
        character(len=:), allocatable :: date_string, time_string, zone_string
        logical :: has_time, has_zone

        if (.not. is_iso8601_valid(string)) then
            call not_iso8601(string, status)
            return
        end if

        date_string = match_string(1, string)
        has_time = has_group(2)
        time_string = match_string(2, string)
        has_zone = has_group(3)
        zone_string = match_string(3, string)
        date = parse_iso8601_date(date_string, status)
        if (.not. status%ok()) return

        ! The time portion is optional.
        if (has_time) then
            time = parse_iso8601_time(time_string, status, keep_fraction)
            if (.not. status%ok()) return
            date%hour = time%hour
            date%minute = time%minute
            date%second = time%second
        end if

        ! The time zone is optional.
        if (has_zone) then
            ! The time zone in decoded times are in seconds.
            date%zone = parse_iso8601_zone(zone_string, status)*60
            date%dst = zone_dst(zone_string)
        end if
    end function parse_iso8601

    !> Parse STRING (in ISO 8601 format) and return a decoded time.
    !> Port of iso8601.el:iso8601-parse-date.
    function parse_iso8601_date(string, status) result(date)
        character(len=*), intent(in) :: string
        type(status_type), intent(out) :: status
        type(partial_decoded_time_type) :: date
        type(partial_decoded_time_type) :: january_4
        type(decoded_time_type) :: decoded
        type(time_type) :: encoded
        integer(int64) :: year
        integer :: week, day_of_week, january_start, correction, ordinal

        status = status_type()

        if (is_match(YEAR_MATCH, string)) then
            ! Just a year: [+-]YYYY.
            date%year = number_of(string)
        else if (is_match(FULL_DATE_MATCH, string)) then
            ! Calendar dates: YYYY-MM-DD and variants.
            date%year = number_of(match_string(1, string))
            date%month = int(number_of(match_string(2, string)))
            date%day = int(number_of(match_string(3, string)))
        else if (is_match(WITHOUT_DAY_MATCH, string)) then
            ! Calendar date without day: YYYY-MM.
            date%year = number_of(string)
            date%month = int(number_of(match_string(2, string)))
        else if (is_match(OUTDATED_DATE_MATCH, string)) then
            ! Outdated date without year: --MM-DD
            date%month = int(number_of(match_string(1, string)))
            date%day = int(number_of(match_string(2, string)))
        else if (is_match(WEEK_DATE_MATCH, string)) then
            ! Week dates: YYYY-Www-D
            year = number_of(string)
            week = int(number_of(match_string(2, string)))
            day_of_week = 0
            if (has_group(3)) day_of_week = int(number_of(match_string(3, string)))
            january_4 = partial_decoded_time_type(year=year, month=1, day=4)
            call set_decoded_time_defaults(january_4)
            encoded = encode_partial_decoded_time(january_4, status)
            if (.not. status%ok()) return
            decoded = decode_time(encoded, status=status)
            if (.not. status%ok()) return
            january_start = decoded%weekday
            correction = merge(7, january_start, january_start == 0) + 3
            ordinal = week*7 + day_of_week - correction

            if (ordinal < 1) then
                ! Monday 29 December 2008 is written "2009-W01-1".
                year = year - 1
                ordinal = ordinal + merge(366, 365, is_date_leap_year(year))
            else if (ordinal > merge(366, 365, is_date_leap_year(year))) then
                ! Sunday 3 January 2010 is written "2009-W53-7".
                ordinal = ordinal - merge(366, 365, is_date_leap_year(year))
                year = year + 1
            end if

            call month_day_date(year, ordinal, date)
        else if (is_match(ORDINAL_DATE_MATCH, string)) then
            ! Ordinal dates: YYYY-DDD
            year = number_of(match_string(1, string))
            ordinal = int(number_of(match_string(2, string)))
            call month_day_date(year, ordinal, date)
        else if (is_match("--\([0-9][0-9]\)", string)) then
            ! Obsolete format with implied year: --MM
            date%month = int(number_of(match_string(1, string)))
        else if (is_match("---\([0-9][0-9]\)", string)) then
            ! Obsolete format with implied year and month: ---DD
            date%day = int(number_of(match_string(1, string)))
        else
            call not_iso8601(string, status)
        end if
    end function parse_iso8601_date

    ! The decoded time of the YEAR, month and day of the day ORDINAL of
    ! YEAR.  Not a port.
    subroutine month_day_date(year, ordinal, date)
        integer(int64), intent(in) :: year
        integer, intent(in) :: ordinal
        type(partial_decoded_time_type), intent(inout) :: date
        type(partial_decoded_time_type) :: month_day

        month_day = date_ordinal_to_time(year, ordinal)
        date%year = year
        date%month = month_day%month
        date%day = month_day%day
    end subroutine month_day_date

    !> Parse STRING, which should be an ISO 8601 time string.  The return
    !> value will be a decoded time with just the hour/minutes/seconds/zone
    !> components known.  KEEP_FRACTION is as for `parse_iso8601'.  Port of
    !> iso8601.el:iso8601-parse-time (FORM t is KEEP_FRACTION true).
    function parse_iso8601_time(string, status, keep_fraction) result(decoded)
        character(len=*), intent(in) :: string
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: keep_fraction
        type(partial_decoded_time_type) :: decoded
        character(len=:), allocatable :: time, zone, fraction_string
        integer, allocatable :: minute
        type(time_type), allocatable :: second
        integer(int64) :: fraction
        integer :: hour, fraction_precision, last
        logical :: has_zone, has_fraction, keep

        status = status_type()
        keep = .false.
        if (present(keep_fraction)) keep = keep_fraction

        if (.not. is_match(full_time_match(), string)) then
            call not_iso8601(string, status)
            return
        end if

        time = match_string(1, string)
        has_zone = has_group(2)
        zone = match_string(2, string)

        if (.not. is_match(TIME_MATCH, time)) then
            call not_iso8601(string, status)
            return
        end if

        hour = int(number_of(match_string(1, time)))
        if (has_group(2)) minute = int(number_of(match_string(2, time)))
        if (has_group(3)) second = time_type(seconds=number_of(match_string(3, time)))
        has_fraction = .false.

        if (has_group(4)) then
            ! Remove trailing zeroes.
            fraction_string = match_string(4, time)
            last = verify(fraction_string, "0", back=.true.)

            if (last > 0) then
                fraction = number_of(fraction_string(1:last))
                fraction_precision = last
                has_fraction = .true.
            end if
        end if

        if (has_fraction .and. keep) then
            if (allocated(second)) then
                ! Sub-second time.
                second = time_type(second%seconds, fraction, 10_int64**fraction_precision)
            else if (allocated(minute)) then
                ! Fractional minute.
                second = time_type(seconds=decimalize(fraction, 60))
            else
                ! Fractional hour.
                minute = int(decimalize(fraction, 60))
            end if
        end if

        decoded%hour = hour

        if (allocated(minute)) then
            decoded%minute = minute
        else
            decoded%minute = 0
        end if

        if (allocated(second)) then
            decoded%second = second
        else
            decoded%second = time_type()
        end if

        if (has_zone) then
            decoded%dst = zone_dst(zone)
            decoded%zone = 60*parse_iso8601_zone(zone, status)
        else
            decoded%dst = zone_dst()
        end if
    end function parse_iso8601_time

    ! FRACTION, the digits after a decimal point, as a share of BASE,
    ! rounded (ties to even).  Port of iso8601.el:iso8601--decimalize.
    integer(int64) function decimalize(fraction, base)
        integer(int64), intent(in) :: fraction
        integer, intent(in) :: base

        decimalize = int(ieee_rint(base*(real(fraction, real64)/10.0_real64**(1 + int(log10(real(fraction, real64)))))), &
                         int64)
    end function decimalize

    !> Parse STRING, which should be an ISO 8601 time zone.  Return the
    !> number of minutes.  Port of iso8601.el:iso8601-parse-zone.
    integer function parse_iso8601_zone(string, status) result(minutes)
        character(len=*), intent(in) :: string
        type(status_type), intent(out) :: status
        integer :: hour, minute

        status = status_type()
        minutes = 0

        if (.not. is_match(ZONE_MATCH, string)) then
            call not_iso8601(string, status)
            return
        end if

        ! "Z" is 0.
        if (.not. has_group(2)) return
        ! HH:MM-ish.
        hour = int(number_of(match_string(3, string)))
        minute = 0
        if (has_group(4)) minute = int(number_of(match_string(4, string)))
        minutes = hour*60 + minute
        if (match_string(2, string) == "-") minutes = -minutes
    end function parse_iso8601_zone

    !> Say whether STRING is a valid ISO 8601 representation.  Port of
    !> iso8601.el:iso8601-valid-p.
    logical function is_iso8601_valid(string)
        character(len=*), intent(in) :: string

        is_iso8601_valid = is_match(combined_match(), string)
    end function is_iso8601_valid

    !> Parse ISO 8601 durations on the form P3Y6M4DT12H30M5S.  Port of
    !> iso8601.el:iso8601-parse-duration.
    function parse_iso8601_duration(string, status) result(duration)
        character(len=*), intent(in) :: string
        type(status_type), intent(out) :: status
        type(partial_decoded_time_type) :: duration
        logical :: full

        status = status_type()
        full = is_match(DURATION_FULL_MATCH, string)
        ! Just a "P" isn't valid; there has to be at least one element,
        ! like P1M.
        if (full) full = len(match_string(0, string)) > 2

        if (full) then
            duration%year = number_of(match_string(1, string))
            duration%month = int(number_of(match_string(2, string)))
            duration%day = int(number_of(match_string(3, string)))
            duration%hour = int(number_of(match_string(5, string)))
            duration%minute = int(number_of(match_string(6, string)))
            duration%second = time_type(seconds=number_of(match_string(7, string)))
        else if (is_match(DURATION_WEEK_MATCH, string)) then
            ! PnW: Weeks.  Does this make sense?  Hm...
            duration%day = int(number_of(match_string(1, string)))*7
        else if (is_match("P"//combined_match(), string)) then
            ! P<date>T<time>
            duration = parse_iso8601(string(2:), status)
        else
            call not_iso8601(string, status)
        end if
    end function parse_iso8601_duration

    !> Parse ISO 8601 intervals: START, END and DURATION.  Port of
    !> iso8601.el:iso8601-parse-interval.
    subroutine parse_iso8601_interval(string, start, end, duration, status)
        character(len=*), intent(in) :: string
        type(partial_decoded_time_type), intent(out) :: start, end, duration
        type(status_type), intent(out) :: status
        type(partial_decoded_time_type) :: negated
        type(decoded_time_type) :: difference
        type(time_type) :: start_time, end_time
        character(len=:), allocatable :: first, second
        integer :: slash
        logical :: has_start, has_end, has_duration

        status = status_type()
        slash = index(string, "/")

        if (slash == 0 .or. index(string(slash + 1:), "/") > 0) then
            call not_iso8601(string, status)
            return
        end if

        first = string(1:slash - 1)
        second = string(slash + 1:)
        has_start = .false.
        has_end = .false.
        has_duration = .false.

        ! The intervals may be an explicit start/end times, or either a
        ! start or an end, and an accompanying duration.
        if (is_duration(first) .and. is_iso8601_valid(second)) then
            duration = parse_iso8601_duration(first, status)
            if (.not. status%ok()) return
            end = parse_iso8601(second, status)
            has_duration = .true.
            has_end = .true.
        else if (is_duration(second) .and. is_iso8601_valid(first)) then
            duration = parse_iso8601_duration(second, status)
            if (.not. status%ok()) return
            start = parse_iso8601(first, status)
            has_duration = .true.
            has_start = .true.
        else if (is_iso8601_valid(first) .and. is_iso8601_valid(second)) then
            start = parse_iso8601(first, status)
            if (.not. status%ok()) return
            end = parse_iso8601(second, status)
            has_start = .true.
            has_end = .true.
        else
            call not_iso8601(string, status)
        end if

        if (.not. status%ok()) return

        if (.not. has_end) then
            end = add_decoded_time(start, duration, status)
            if (.not. status%ok()) return
        end if

        if (.not. has_start) then
            ! We negate the duration so that we get a subtraction.
            negated = negate(duration)
            start = add_decoded_time(end, negated, status)
            if (.not. status%ok()) return
        end if

        if (has_duration) return
        ! FIXME: Support subseconds.  FIXME: It makes no sense to decode a
        ! time difference according to the zone of END, or according to any
        ! other time zone for that matter.
        end_time = encode_with_defaults(end, status)
        if (.not. status%ok()) return
        start_time = encode_with_defaults(start, status)
        if (.not. status%ok()) return

        if (allocated(end%zone)) then
            difference = decode_time(time_subtract(end_time, start_time), time_zone_rule(end%zone), status)
        else
            difference = decode_time(time_subtract(end_time, start_time), time_zone_rule(0), status)
        end if

        duration%second = time_type(seconds=difference%second%seconds)
        duration%minute = difference%minute
        duration%hour = difference%hour
        duration%day = difference%day
        duration%month = difference%month
        duration%year = difference%year
        duration%weekday = difference%weekday
        duration%dst = difference%dst
        if (allocated(difference%zone)) duration%zone = difference%zone
    end subroutine parse_iso8601_interval

    ! Whether TEXT starts with "P" (a duration).  Not a port.
    logical function is_duration(text)
        character(len=*), intent(in) :: text
        type(status_type) :: st

        is_duration = string_match("\`P", text, st) > 0
    end function is_duration

    ! DURATION with each known number negated, as upstream negates each
    ! number of the decoded time.  Not a port.
    function negate(duration) result(negated)
        type(partial_decoded_time_type), intent(in) :: duration
        type(partial_decoded_time_type) :: negated

        negated = duration
        if (allocated(negated%second)) negated%second = time_subtract(time_type(), negated%second)
        if (allocated(negated%minute)) negated%minute = -negated%minute
        if (allocated(negated%hour)) negated%hour = -negated%hour
        if (allocated(negated%day)) negated%day = -negated%day
        if (allocated(negated%month)) negated%month = -negated%month
        if (allocated(negated%year)) negated%year = -negated%year
        if (allocated(negated%weekday)) negated%weekday = -negated%weekday
        negated%dst = -negated%dst
        if (allocated(negated%zone)) negated%zone = -negated%zone
    end function negate

    ! Like `encode_partial_decoded_time', but fill in unknown values in
    ! TIME.  Port of iso8601.el:iso8601--encode-time.
    type(time_type) function encode_with_defaults(time, status) result(encoded)
        type(partial_decoded_time_type), intent(in) :: time
        type(status_type), intent(out) :: status
        type(partial_decoded_time_type) :: copy

        copy = time
        call set_decoded_time_defaults(copy)
        encoded = encode_partial_decoded_time(copy, status)
    end function encode_with_defaults
end module iso8601_mod
