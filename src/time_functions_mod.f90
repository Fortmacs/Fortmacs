! time_functions_mod.f90 -- time values, time zones and the formatting of times.
! Ported from GNU Emacs 31.1 src/timefns.c and lib/strftime.c (in part:
! the C locale's names and the values of its conversions).
!
! Copyright (C) 1985-1987, 1989, 1993-2026 Free Software Foundation, Inc.
! Copyright (C) 1991-2026 Free Software Foundation, Inc.
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

!> Time values and what Fortmacs does with them: the clock (`current_time'),
!> arithmetic and comparison (`time_add', `is_time_less'), conversion
!> (`time_convert', `real'), the calendar in a time zone
!> (`decode_time', `encode_time') and text (`format_date' with a
!> `date_format_type', `current_time_string').
!>
!> Upstream's several shapes of a time value (an integer, a float,
!> (TICKS . HZ), (HIGH LOW USEC PSEC)) are one `time_type' (R6-3): whole
!> `seconds' since the epoch plus `count'/`count_rate' of a second, the
!> terms of the intrinsic `system_clock'.  It is exact for any rate up to
!> huge(0_int64), so upstream's bignum arithmetic becomes int64 and, for
!> the products, a 128-bit integer kind.  Where upstream stands for the
!> current time with an omitted time value, the argument is optional
!> (float-time is the intrinsic `real' extended, R6-14, so its time is
!> given: real(current_time())).
!>
!> A time zone rule (upstream's ZONE) is a TZ string as POSIX defines it
!> ("UTC0" for upstream's t); absent, it is Fortmacs's local time (R6-12,
!> R6-13): `time_zone_rule' turns a UTC offset, with or without an
!> abbreviation, into one, and TIME_ZONE_SYSTEM stands for the system's
!> wall clock time (upstream's `wall').  The calendar comes from the C
!> library (localtime_r, mktime) under that rule, as upstream's
!> localtime_rz and mktime_z do on GNU/Linux.  A date format is a Fortran
!> format and the parts of the date it writes (`date_format_type', owner,
!> 2026-11-06), not upstream's strftime string; `format_date' takes the
!> parts' values and names from Gnulib's nstrftime in the C locale.
module time_functions_mod
    use, intrinsic :: iso_fortran_env, only: int32, int64, real64
    use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
    use error_mod, only: status_type, set_error, GENERIC_ERROR_CODE
    use system_dependent_mod, only: fortmacs_clock_realtime, fortmacs_clock_process_cputime, system_localtime, system_mktime, &
                                    system_set_time_zone
    implicit none(type, external)
    private

    public :: current_time, current_cpu_time, time_add, time_subtract, is_time_less, is_time_equal, real, &
              time_convert, decode_time, encode_time, format_date, current_time_string, current_time_zone, &
              set_time_zone_rule, time_zone_rule

    !> The count rate of file and clock times: nanoseconds (timespec.h
    !> TIMESPEC_HZ).
    integer(int64), parameter, public :: TIMESPEC_COUNT_RATE = 1000000000_int64

    !> A time value: `seconds' whole seconds since the epoch, 1970-01-01
    !> 00:00:00 UTC, plus `count'/`count_rate' of a second, where
    !> 0 <= count < count_rate.  A time difference is a time value too.
    !> time_type(seconds=N) is N seconds; time_type(1, 1, 2) is 1.5 seconds.
    type, public :: time_type
        integer(int64) :: seconds = 0
        integer(int64) :: count = 0
        integer(int64) :: count_rate = 1
    end type time_type

    !> `daylight_saving_time' of a decoded time: daylight saving time is in effect.
    integer, parameter, public :: DECODED_TIME_DAYLIGHT_SAVING_TIME_YES = 1
    !> `daylight_saving_time' of a decoded time: daylight saving time is not in effect.
    integer, parameter, public :: DECODED_TIME_DAYLIGHT_SAVING_TIME_NO = 0
    !> `daylight_saving_time' of a decoded time: daylight saving information is not
    !> available (for `encode_time': guess it).
    integer, parameter, public :: DECODED_TIME_DAYLIGHT_SAVING_TIME_UNKNOWN = -1

    !> A decoded time, as `decode_time' gives it and `encode_time' takes it
    !> (simple.el's decoded-time).  `second' is a nonnegative time value
    !> less than 60 (or 61 where the system has leap seconds), `minute'
    !> 0-59, `hour' 0-23, `day' 1-31, `month' 1-12, `year' the year number
    !> (0 is 1 BC), `weekday' 0-6 with 0 for Sunday, `daylight_saving_time' one of the
    !> DECODED_TIME_DAYLIGHT_SAVING_TIME_ constants, and `zone' the UTC offset in seconds
    !> east of Greenwich (unallocated: Fortmacs's local time).
    type, public :: decoded_time_type
        type(time_type) :: second
        integer :: minute = 0
        integer :: hour = 0
        integer :: day = 1
        integer :: month = 1
        integer(int64) :: year = 1970
        integer :: weekday = 4
        integer :: daylight_saving_time = DECODED_TIME_DAYLIGHT_SAVING_TIME_UNKNOWN
        integer, allocatable :: zone
    end type decoded_time_type

    !> The offset of a time zone in seconds ahead of UTC (east of
    !> Greenwich; negative west of it) and its name, as
    !> `current_time_zone' gives them.
    type, public :: time_zone_type
        integer :: offset = 0
        character(len=:), allocatable :: name
    end type time_zone_type

    !> The most parts a `date_format_type' writes.
    integer, parameter, public :: DATE_FORMAT_MAXIMUM_PARTS = 16
    !> The longest Fortran format a `date_format_type' holds.
    integer, parameter, public :: DATE_FORMAT_LENGTH = 256

    ! The parts of a date take the stems of the intrinsic date_and_time's
    ! arguments (owner, 2026-11-06, #272): DATE_ for the calendar, TIME_ for
    ! the time of day and ZONE_ for the time zone.

    !> No part: the unused entries of a date format's `parts'.
    integer, parameter, public :: DATE_NONE = 0
    !> The year, a number (strftime's %Y; i4.4 for four digits).
    integer, parameter, public :: DATE_YEAR = 1
    !> The century, a number: the year divided by 100, rounded down (%C).
    integer, parameter, public :: DATE_CENTURY = 2
    !> The year without its century, a number 0-99 (%y; i2.2).
    integer, parameter, public :: DATE_YEAR_OF_CENTURY = 3
    !> The year of the ISO 8601 week, a number (%G).
    integer, parameter, public :: DATE_ISO_WEEK_YEAR = 4
    !> The year of the ISO 8601 week without its century, a number 0-99
    !> (%g).
    integer, parameter, public :: DATE_ISO_WEEK_YEAR_OF_CENTURY = 5
    !> The ISO 8601 week, a number 1-53: weeks start on Monday, and week 1
    !> has the year's first Thursday (%V).
    integer, parameter, public :: DATE_ISO_WEEK = 6
    !> The quarter of the year, a number 1-4 (%q).
    integer, parameter, public :: DATE_QUARTER = 7
    !> The month, a number 1-12 (%m; i2.2, or i0 without the zero).
    integer, parameter, public :: DATE_MONTH = 8
    !> The week of the year, a number 0-53, with weeks starting on Sunday
    !> and week 1 at the year's first Sunday (%U).
    integer, parameter, public :: DATE_WEEK_STARTING_SUNDAY = 9
    !> The week of the year, a number 0-53, with weeks starting on Monday
    !> and week 1 at the year's first Monday (%W).
    integer, parameter, public :: DATE_WEEK_STARTING_MONDAY = 10
    !> The day of the month, a number 1-31 (%d with i2.2, %e with i2).
    integer, parameter, public :: DATE_DAY = 11
    !> The day of the year, a number 1-366 (%j; i3.3).
    integer, parameter, public :: DATE_DAY_OF_YEAR = 12
    !> The day of the week, a number 0-6, 0 for Sunday (%w).
    integer, parameter, public :: DATE_WEEKDAY = 13
    !> The day of the week, a number 1-7, 1 for Monday (%u).
    integer, parameter, public :: DATE_ISO_WEEKDAY = 14
    !> The hour on a 24-hour clock, a number 0-23 (%H with i2.2, %k with
    !> i2).
    integer, parameter, public :: TIME_HOUR = 15
    !> The hour on a 12-hour clock, a number 1-12 (%I with i2.2, %l with
    !> i2).
    integer, parameter, public :: TIME_HOUR_OF_TWELVE_HOUR_CLOCK = 16
    !> The minute, a number 0-59 (%M; i2.2).
    integer, parameter, public :: TIME_MINUTE = 17
    !> The second, a number 0-60, 60 for a leap second (%S; i2.2).
    integer, parameter, public :: TIME_SECOND = 18
    !> The millisecond of the second, a number 0-999 (%3N; i3.3).
    integer, parameter, public :: TIME_MILLISECOND = 19
    !> The microsecond of the second, a number 0-999999 (%6N; i6.6).
    integer, parameter, public :: TIME_MICROSECOND = 20
    !> The nanosecond of the second, a number 0-999999999 (%N; i9.9).
    integer, parameter, public :: TIME_NANOSECOND = 21
    !> The seconds since 1970-01-01 00:00:00 UTC, a number (%s).
    integer, parameter, public :: TIME_SECONDS_SINCE_EPOCH = 22
    !> The offset of the time zone from UTC as hours and minutes, a number
    !> (+0530 is 530; %z with sp, i5.4).  A zero offset that the zone
    !> writes as -0000 needs ZONE_OFFSET_SIGN.
    integer, parameter, public :: ZONE_OFFSET = 23
    !> The whole hours of the time zone's offset from UTC, a number
    !> without its sign (%:z is a, i2.2, ":", i2.2 of ZONE_OFFSET_SIGN,
    !> ZONE_OFFSET_HOURS and ZONE_OFFSET_MINUTES).
    integer, parameter, public :: ZONE_OFFSET_HOURS = 24
    !> The minutes of the time zone's offset beyond its hours, a number
    !> 0-59 without its sign.
    integer, parameter, public :: ZONE_OFFSET_MINUTES = 25
    !> The seconds of the time zone's offset beyond its minutes, a number
    !> 0-59 without its sign (the last field of %::z).
    integer, parameter, public :: ZONE_OFFSET_SECONDS = 26
    !> The name of the day of the week, text: "Sunday" (%A).
    integer, parameter, public :: DATE_WEEKDAY_NAME = 41
    !> The abbreviated name of the day of the week, text: "Sun" (%a).
    integer, parameter, public :: DATE_WEEKDAY_ABBREVIATION = 42
    !> The name of the day of the week in upper case, text: "SUNDAY" (%^A).
    integer, parameter, public :: DATE_WEEKDAY_NAME_UPPER_CASE = 43
    !> The abbreviated name of the day of the week in upper case, text:
    !> "SUN" (%^a).
    integer, parameter, public :: DATE_WEEKDAY_ABBREVIATION_UPPER_CASE = 44
    !> The name of the month, text: "December" (%B).
    integer, parameter, public :: DATE_MONTH_NAME = 45
    !> The abbreviated name of the month, text: "Dec" (%b, %h).
    integer, parameter, public :: DATE_MONTH_ABBREVIATION = 46
    !> The name of the month in upper case, text: "DECEMBER" (%^B).
    integer, parameter, public :: DATE_MONTH_NAME_UPPER_CASE = 47
    !> The abbreviated name of the month in upper case, text: "DEC" (%^b).
    integer, parameter, public :: DATE_MONTH_ABBREVIATION_UPPER_CASE = 48
    !> Before or after noon, text: "AM" or "PM" (%p).
    integer, parameter, public :: TIME_MERIDIEM = 49
    !> Before or after noon in lower case, text: "am" or "pm" (%P).
    integer, parameter, public :: TIME_MERIDIEM_LOWER_CASE = 50
    !> The abbreviation of the time zone, text: "CET" (%Z).
    integer, parameter, public :: ZONE_ABBREVIATION = 51
    !> The abbreviation of the time zone in lower case, text: "cet" (%#Z).
    integer, parameter, public :: ZONE_ABBREVIATION_LOWER_CASE = 52
    !> The sign of the time zone's offset from UTC, text: "+" east of
    !> Greenwich and for UTC, "-" west of it and for a zone that writes
    !> itself as "-00".
    integer, parameter, public :: ZONE_OFFSET_SIGN = 53

    ! The ranges of the number parts and of the text parts.
    integer, parameter :: FIRST_NUMBER_PART = DATE_YEAR, LAST_NUMBER_PART = ZONE_OFFSET_SECONDS, &
                          FIRST_TEXT_PART = DATE_WEEKDAY_NAME, LAST_TEXT_PART = ZONE_OFFSET_SIGN

    !> A date and time format: `format', a Fortran format with edit
    !> descriptors, and `parts', the DATE_, TIME_ and ZONE_ constants of the
    !> parts of the date it writes, in order, one for each data edit descriptor: I, B,
    !> O, Z or G for a number, A or G for text.  `format_date' writes a
    !> time with it:
    !>
    !>     date_format_type('(i4.4, "-", i2.2, "-", i2.2)', [DATE_YEAR, DATE_MONTH, DATE_DAY])
    !>
    !> gives "2026-11-06".  `format' has at most DATE_FORMAT_LENGTH
    !> characters and `parts' at most DATE_FORMAT_MAXIMUM_PARTS entries;
    !> the entries after the last part are DATE_NONE.  A named constant
    !> (a `parameter') gives `parts' in full, with
    !> reshape([...], [DATE_FORMAT_MAXIMUM_PARTS], pad=[DATE_NONE]), as
    !> MAIL_DATE does.  Not a port: upstream writes a date format as a
    !> strftime string (owner, 2026-11-06).
    type, public :: date_format_type
        character(len=DATE_FORMAT_LENGTH) :: format = "()"
        integer :: parts(DATE_FORMAT_MAXIMUM_PARTS) = DATE_NONE
        ! The format or the parts given to the constructor did not fit.
        logical, private :: is_truncated = .false.
    end type date_format_type

    !> date_format_type(format, parts): the date format of FORMAT, a
    !> Fortran format, and PARTS, the parts it writes in order (any number
    !> of them up to DATE_FORMAT_MAXIMUM_PARTS).
    interface date_format_type
        module procedure date_format_of_parts
    end interface date_format_type

    !> The date of a mail message's Date field, as RFC 822 writes it:
    !> "Fri, 06 Nov 2026 14:03:09 +0100" (upstream's "%a, %d %b %Y %T %z"
    !> of message-make-date).
    type(date_format_type), parameter, public :: MAIL_DATE = &
                         date_format_type(format='(a, ", ", i2.2, 1x, a, 1x, i4.4, 1x, i2.2, ":", i2.2, ":", i2.2, 1x, sp, i5.4)', &
                                parts=reshape([DATE_WEEKDAY_ABBREVIATION, DATE_DAY, DATE_MONTH_ABBREVIATION, DATE_YEAR, TIME_HOUR, &
                                                              TIME_MINUTE, TIME_SECOND, ZONE_OFFSET], [DATE_FORMAT_MAXIMUM_PARTS], &
                                                                                pad=[DATE_NONE]), &
                                                                  is_truncated=.false.)

    !> Convert a time value to another count rate, or a real number of
    !> seconds to a time value.
    interface time_convert
        module procedure time_convert_time, time_convert_real
    end interface time_convert

    !> Return the time value TIME as a real number of seconds since the
    !> epoch; real(current_time()) is the current time.  The intrinsic
    !> `real' extended to a time value (R6-14): the result is real(real64),
    !> correctly rounded.
    !> WARNING: Since the result is floating point, it may not be exact.
    !> If precise time stamps are required, use either `time_convert', or
    !> (if you need time as a string) `format_date'.
    interface real
        module procedure time_to_real
    end interface real

    !> The zone that stands for the system's wall clock time, as if TZ
    !> were unset (upstream's ZONE `wall'): a value no TZ rule can have.
    character(len=*), parameter, public :: TIME_ZONE_SYSTEM = achar(0)

    ! A 128-bit integer kind for the products of counts and rates (gfortran
    ! has it on x86-64 and aarch64).
    integer, parameter :: int128 = selected_int_kind(38)

    ! A broken-down time (struct tm) with what strftime needs besides:
    ! `year' counts from 1900 and `month' from 0, as in struct tm.
    type :: broken_down_time_type
        integer(int64) :: seconds = 0
        integer :: nanoseconds = 0
        integer :: second = 0, minute = 0, hour = 0, day = 1, month = 0, year = 70, weekday = 4, &
                   year_day = 0, daylight_saving_time = -1
        integer(int64) :: offset = 0
        character(len=:), allocatable :: abbreviation
    end type broken_down_time_type

    ! The TZ rule in effect before a switch to another one.
    type :: saved_time_zone_type
        logical :: switched = .false.
        logical :: had_rule = .false.
        character(len=:), allocatable :: rule
    end type saved_time_zone_type

    ! The kinds of the tokens of a Fortran format that `split_date_format'
    ! keeps: a data edit descriptor, an opening and a closing parenthesis,
    ! and a sign edit descriptor (S, SP, SS).
    integer, parameter :: TOKEN_DATA = 1, TOKEN_OPEN = 2, TOKEN_CLOSE = 3, TOKEN_SIGN = 4

    ! The longest data edit descriptor of a date format, without its
    ! repeat count.
    integer, parameter :: DESCRIPTOR_LENGTH = 32

    ! One token of a Fortran format: its kind, its repeat count, for an
    ! opening parenthesis the index of its closing one (`partner'), and
    ! for a data edit descriptor its `letters' (I, A, EN, ...), its field
    ! `width' (-1 for none) and the whole descriptor without the repeat
    ! count (for a sign edit descriptor, the descriptor).
    type :: format_token_type
        integer :: kind = 0
        integer :: repeat = 1
        integer :: partner = 0
        integer :: width = -1
        character(len=2) :: letters = ""
        character(len=DESCRIPTOR_LENGTH) :: descriptor = ""
    end type format_token_type

    ! The longest text one part, and a whole date, can be written as.
    integer, parameter :: PART_TEXT_LENGTH = 256, DATE_TEXT_LENGTH = 4096

    ! The character `format_date' writes after the date to find where the
    ! text ends (a blank at its end is part of it).
    character(len=*), parameter :: END_OF_DATE = achar(1)

    character(len=*), parameter :: WEEKDAY_NAMES(0:6) = [character(len=9) :: "Sunday", "Monday", "Tuesday", &
                                                                                      "Wednesday", "Thursday", "Friday", "Saturday"]
    character(len=*), parameter :: MONTH_NAMES(0:11) = [character(len=9) :: "January", "February", "March", &
                                                                            "April", "May", "June", "July", "August", "September", &
                                                                                                  "October", "November", "December"]

contains

    ! ------------------------------------------------------------------
    ! The clock
    ! ------------------------------------------------------------------

    !> Return the current time, as the number of seconds since 1970-01-01
    !> 00:00:00, counted in nanoseconds (`count_rate' is
    !> TIMESPEC_COUNT_RATE).  Port of timefns.c:Fcurrent_time.
    type(time_type) function current_time() result(now)
        integer(int64) :: second
        integer(int32) :: nsec

        call fortmacs_clock_realtime(second, nsec)
        now = time_type(second, int(nsec, int64), TIMESPEC_COUNT_RATE)
    end function current_time

    !> Return the current CPU time along with its resolution: the
    !> processor time this process has used, with `count_rate' the C
    !> library's CLOCKS_PER_SECOND (1000000).  Port of
    !> timefns.c:Fcurrent_cpu_time (clock(3) reads the same clock).
    type(time_type) function current_cpu_time() result(used)
        integer(int64), parameter :: CLOCKS_PER_SECOND = 1000000_int64
        integer(int64) :: second
        integer(int32) :: nsec

        call fortmacs_clock_process_cputime(second, nsec)
        used = time_type(second, int(nsec, int64)/(TIMESPEC_COUNT_RATE/CLOCKS_PER_SECOND), CLOCKS_PER_SECOND)
    end function current_cpu_time

    ! ------------------------------------------------------------------
    ! Arithmetic and comparison
    ! ------------------------------------------------------------------

    !> Return the sum of two time values A and B, as a time value.
    !> Port of timefns.c:Ftime_add.
    pure type(time_type) function time_add(a, b)
        type(time_type), intent(in) :: a, b

        time_add = time_arith(a, b, .false.)
    end function time_add

    !> Return the difference between two time values A and B, as a time
    !> value.  You can use `real' to convert the difference into
    !> elapsed seconds.  Port of timefns.c:Ftime_subtract.
    pure type(time_type) function time_subtract(a, b)
        type(time_type), intent(in) :: a, b

        time_subtract = time_arith(a, b, .true.)
    end function time_subtract

    ! A + B, or A - B if SUBTRACT, exactly: with the count rate both
    ! share, else with the least common multiple of their rates reduced
    ! by the common factors of the result, but not below the smaller rate.
    ! Where that rate exceeds huge(0_int64), the operand with the smaller
    ! rate is first truncated to the larger one.  Port of
    ! timefns.c:time_arith (and lispint_arith).
    pure type(time_type) function time_arith(a, b, subtract) result(r)
        type(time_type), intent(in) :: a, b
        logical, intent(in) :: subtract
        integer(int128) :: g, factor_a, factor_b, rate, counts, shared, smallest_rate, rescale, sign, b_count
        integer(int64) :: seconds

        sign = merge(-1_int128, 1_int128, subtract)
        seconds = a%seconds + int(sign, int64)*b%seconds
        g = gcd(int(a%count_rate, int128), int(b%count_rate, int128))
        factor_a = a%count_rate/g
        factor_b = b%count_rate/g

        if (a%count_rate == b%count_rate) then
            rate = a%count_rate
            counts = a%count + sign*b%count
        else if (factor_a*b%count_rate > huge(0_int64)) then
            rate = max(a%count_rate, b%count_rate)

            if (a%count_rate >= b%count_rate) then
                b_count = (int(b%count, int128)*rate)/b%count_rate
                counts = a%count + sign*b_count
            else
                counts = (int(a%count, int128)*rate)/a%count_rate + sign*b%count
            end if
        else
            rate = factor_a*b%count_rate
            counts = factor_b*a%count + sign*factor_a*b%count
            shared = gcd(counts, rate)

            if (shared > 1) then
                counts = counts/shared
                rate = rate/shared
                smallest_rate = min(a%count_rate, b%count_rate)

                if (rate < smallest_rate) then
                    rescale = (smallest_rate + rate - 1)/rate
                    counts = counts*rescale
                    rate = rate*rescale
                end if
            end if
        end if

        r%count_rate = int(rate, int64)
        r%count = int(modulo(counts, rate), int64)
        r%seconds = seconds + int((counts - modulo(counts, rate))/rate, int64)
    end function time_arith

    ! The greatest common divisor of A and B (B when A is 0).  Port of
    ! timefns.c:emacs_gcd.
    pure recursive integer(int128) function gcd(a, b) result(d)
        integer(int128), intent(in) :: a, b

        if (a == 0) then
            d = abs(b)
        else
            d = gcd(mod(b, a), a)
        end if
    end function gcd

    ! Negative, 0 or positive as A is less than, equal to or greater than
    ! B.  Port of timefns.c:time_cmp.
    pure integer function time_cmp(a, b)
        type(time_type), intent(in) :: a, b
        integer(int128) :: x, y

        if (a%seconds /= b%seconds) then
            time_cmp = merge(-1, 1, a%seconds < b%seconds)
            return
        end if

        x = int(a%count, int128)*b%count_rate
        y = int(b%count, int128)*a%count_rate
        time_cmp = 0
        if (x < y) time_cmp = -1
        if (x > y) time_cmp = 1
    end function time_cmp

    !> Return true if time value A is less than time value B.
    !> Port of timefns.c:Ftime_less_p.
    pure logical function is_time_less(a, b)
        type(time_type), intent(in) :: a, b

        is_time_less = time_cmp(a, b) < 0
    end function is_time_less

    !> Return true if A and B are equal time values.
    !> Port of timefns.c:Ftime_equal_p.
    pure logical function is_time_equal(a, b)
        type(time_type), intent(in) :: a, b

        is_time_equal = time_cmp(a, b) == 0
    end function is_time_equal

    ! ------------------------------------------------------------------
    ! Conversion
    ! ------------------------------------------------------------------

    !> Return the time value TIME as a real number of seconds since the
    !> epoch; real(current_time()) is the current time.
    !> WARNING: Since the result is floating point, it may not be exact.
    !> If precise time stamps are required, use either `time_convert', or
    !> (if you need time as a string) `format_date'.
    !> Port of timefns.c:Ffloat_time (and float_time).
    real(real64) function time_to_real(time) result(seconds)
        type(time_type), intent(in) :: time

        seconds = frac_to_double(int(time%seconds, int128)*time%count_rate + time%count, time%count_rate)
    end function time_to_real

    ! NUMERATOR / DENOMINATOR (positive), rounded to the nearest real,
    ! ties to even.  Port of timefns.c:frac_to_double.
    pure real(real64) function frac_to_double(numerator, denominator) result(x)
        integer(int128), intent(in) :: numerator
        integer(int64), intent(in) :: denominator
        integer(int128) :: n, d, q, r
        integer :: numerator_digits, denominator_digits, scaling, increment

        if (mod(numerator, int(denominator, int128)) == 0) then
            x = real(numerator/denominator, real64)
            return
        end if

        ! The number of binary digits in numerator and denominator.
        n = abs(numerator)
        d = denominator
        numerator_digits = int(bit_size(n) - leadz(n))
        denominator_digits = int(bit_size(d) - leadz(d))
        ! Scale with SCALING when doing integer division: compute
        ! (N * 2**SCALING) / D [or, if SCALING is negative,
        ! N / (D * 2**-SCALING)], convert it to a real, then divide by
        ! 2**SCALING.
        scaling = denominator_digits - numerator_digits + digits(x)

        if (scaling < 0) then
            d = d*2_int128**(-scaling)
        else
            n = n*2_int128**scaling
        end if

        q = n/d
        r = n - q*d

        ! Round the quotient before converting it to a real: to the
        ! nearest integer if it has at most digits(x) binary digits,
        ! otherwise to the nearest even integer; ties to even.
        if (int(bit_size(q) - leadz(q)) <= digits(x)) then
            increment = 0
            if (2*r > d .or. (2*r == d .and. mod(q, 2_int128) == 1)) increment = 1
        else
            increment = 0
            if (mod(q, 2_int128) == 1 .and. (mod(q/2, 2_int128) == 1 .or. r /= 0)) increment = 2
        end if

        ! (Converting to a real discards the low digit, as mpz_get_d
        ! truncates.)
        q = q + increment
        if (int(bit_size(q) - leadz(q)) > digits(x)) q = q - mod(q, 2_int128)
        x = scale(real(q, real64), -scaling)
        if (numerator < 0) x = -x
    end function frac_to_double

    !> Convert TIME to a time value with COUNT_RATE counts per second (a
    !> positive integer), truncating toward minus infinity.  If COUNT_RATE
    !> is absent, return TIME.  TIME%seconds is the integer count of
    !> seconds, and `real' gives the real one.  A `count_rate' that
    !> is not positive is an error.  Port of timefns.c:Ftime_convert
    !> (FORM a positive integer or t; ticks_hz_hz_ticks).
    type(time_type) function time_convert_time(time, count_rate, status) result(converted)
        type(time_type), intent(in) :: time
        integer(int64), intent(in), optional :: count_rate
        type(status_type), intent(out), optional :: status

        if (present(status)) status = status_type()
        converted = time
        if (.not. present(count_rate)) return

        if (count_rate <= 0) then
            if (present(status)) call set_error(status, GENERIC_ERROR_CODE, "Invalid time frequency")
            return
        end if

        converted%count_rate = count_rate
        converted%count = int((int(time%count, int128)*count_rate)/time%count_rate, int64)
    end function time_convert_time

    !> Convert SECONDS, a real number of seconds since the epoch, to a time
    !> value with COUNT_RATE counts per second, truncating toward minus
    !> infinity.  If COUNT_RATE is absent, the rate is the least power of 2
    !> that represents SECONDS exactly (upstream keeps the trailing zero
    !> digits of the real's mantissa in it, which a sum with a decimal rate
    !> could not fit), but at most 2**62, so a value with more significant
    !> binary digits after the point is truncated.  A value that
    !> is not finite, or whose seconds do not fit, is an error.  Port of
    !> timefns.c:Ftime_convert (TIME a float; decode_float_time).
    type(time_type) function time_convert_real(seconds, count_rate, status) result(converted)
        real(real64), intent(in) :: seconds
        integer(int64), intent(in), optional :: count_rate
        type(status_type), intent(out), optional :: status
        integer(int128) :: mantissa, counts, rate
        integer :: scaling, shift

        if (present(status)) status = status_type()
        converted = time_type()

        if (.not. ieee_is_finite(seconds)) then
            if (present(status)) call set_error(status, GENERIC_ERROR_CODE, "Invalid time specification")
            return
        end if

        if (abs(seconds) >= 2.0_real64**63) then
            if (present(status)) call set_error(status, GENERIC_ERROR_CODE, "Specified time is not representable")
            return
        end if

        ! SECONDS is MANTISSA * 2**(exponent(seconds) - digits(seconds)).
        mantissa = int(scale(fraction(seconds), digits(seconds)), int128)

        if (mantissa /= 0) then
            ! The number of significant binary digits after the point
            ! (double_integer_scale, without trailing zeros).
            scaling = digits(seconds) - exponent(seconds) - trailz(int(mantissa, int64))
            scaling = min(max(scaling, 0), 62)
            ! COUNTS = floor(SECONDS * 2**SCALING).
            shift = exponent(seconds) - digits(seconds) + scaling

            if (shift >= 0) then
                counts = mantissa*2_int128**shift
            else
                counts = (mantissa - modulo(mantissa, 2_int128**(-shift)))/2_int128**(-shift)
            end if

            rate = 2_int128**scaling
            converted%count_rate = int(rate, int64)
            converted%count = int(modulo(counts, rate), int64)
            converted%seconds = int((counts - modulo(counts, rate))/rate, int64)
        end if

        if (present(count_rate)) converted = time_convert_time(converted, count_rate, status)
    end function time_convert_real

    ! ------------------------------------------------------------------
    ! Time zones
    ! ------------------------------------------------------------------

    !> The TZ rule for a UTC offset of OFFSET seconds east of Greenwich
    !> (negative west of it), applied without consideration for daylight
    !> saving time, for the ZONE argument of `decode_time',
    !> `encode_time', `format_date', `current_time_string' and
    !> `current_time_zone'.  ABBREVIATION is the zone's name (as from
    !> `current_time_zone'); absent, the name is the numeric offset (as
    !> `decode_time' gives it).  time_zone_rule(-28800, "PST") is
    !> "<PST>8:00:00", time_zone_rule(-28800) is "<-08>8:00:00" and
    !> time_zone_rule(0) is "UTC0".  Port of timefns.c:tzlookup (an integer
    !> ZONE, or a list (OFFSET ABBR)).
    pure function time_zone_rule(offset, abbreviation) result(rule)
        integer, intent(in) :: offset
        character(len=*), intent(in), optional :: abbreviation
        character(len=:), allocatable :: rule
        character(len=32) :: buffer
        integer :: absolute, hour, remainder, minute, second, digit_count, number

        if (offset == 0 .and. .not. present(abbreviation)) then
            rule = "UTC0"
            return
        end if

        absolute = abs(offset)
        hour = absolute/3600
        remainder = mod(absolute, 3600)
        minute = remainder/60
        second = mod(remainder, 60)

        if (present(abbreviation)) then
            rule = "<"//abbreviation//">"
        else
            digit_count = 2
            number = hour

            if (remainder /= 0) then
                digit_count = 4
                number = 100*number + minute

                if (second /= 0) then
                    digit_count = 6
                    number = 100*number + second
                end if
            end if

            write (buffer, "(i0)") number
            rule = "<"//merge("-", "+", offset < 0)//repeat("0", max(digit_count - len_trim(buffer), 0))// &
                   trim(buffer)//">"
        end if

        write (buffer, "(i0, ':', i2.2, ':', i2.2)") hour, minute, second
        if (offset >= 0) rule = rule//"-"
        rule = rule//trim(buffer)
    end function time_zone_rule

    ! Make RULE the C library's time zone rule until time_zone_end, saving
    ! the one in effect in SAVED; TZ unset for TIME_ZONE_SYSTEM (the
    ! system's wall clock time); nothing when RULE is absent (Fortmacs's
    ! local time).  Port of timefns.c:tzlookup (SETTZ false).
    subroutine time_zone_begin(rule, saved)
        character(len=*), intent(in), optional :: rule
        type(saved_time_zone_type), intent(out) :: saved
        integer :: length, stat

        if (.not. present(rule)) return
        call get_environment_variable("TZ", length=length, status=stat)
        saved%had_rule = stat == 0
        allocate (character(len=max(length, 0)) :: saved%rule)
        if (saved%had_rule .and. length > 0) call get_environment_variable("TZ", saved%rule)
        saved%switched = .true.

        if (rule == TIME_ZONE_SYSTEM) then
            call system_set_time_zone()
        else
            call system_set_time_zone(rule)
        end if
    end subroutine time_zone_begin

    ! Restore the time zone rule time_zone_begin saved in SAVED.  Port of
    ! timefns.c:xtzfree.
    subroutine time_zone_end(saved)
        type(saved_time_zone_type), intent(in) :: saved

        if (.not. saved%switched) return

        if (saved%had_rule) then
            call system_set_time_zone(saved%rule)
        else
            call system_set_time_zone()
        end if
    end subroutine time_zone_end

    ! The broken-down time TM of TIME under the time zone rule ZONE
    ! (absent: Fortmacs's local time); `ok' is false when its year does not
    ! fit.  Port of timefns.c:emacs_localtime_rz.
    subroutine localtime_in_zone(time, zone, tm, ok)
        type(time_type), intent(in) :: time
        character(len=*), intent(in), optional :: zone
        type(broken_down_time_type), intent(out) :: tm
        logical, intent(out) :: ok
        type(saved_time_zone_type) :: saved
        integer :: fields(9)

        call time_zone_begin(zone, saved)
        call system_localtime(time%seconds, fields, tm%offset, tm%abbreviation, ok)
        call time_zone_end(saved)
        tm%seconds = time%seconds
        tm%nanoseconds = int((int(time%count, int128)*TIMESPEC_COUNT_RATE)/time%count_rate)
        tm%second = fields(1)
        tm%minute = fields(2)
        tm%hour = fields(3)
        tm%day = fields(4)
        tm%month = fields(5)
        tm%year = fields(6)
        tm%weekday = fields(7)
        tm%year_day = fields(8)
        tm%daylight_saving_time = fields(9)
    end subroutine localtime_in_zone

    ! An error condition: the time does not fit.  Port of
    ! timefns.c:time_overflow.
    subroutine time_overflow(status)
        type(status_type), intent(out), optional :: status

        if (present(status)) call set_error(status, GENERIC_ERROR_CODE, "Specified time is not representable")
    end subroutine time_overflow

    !> Set the Fortmacs local time zone using TZ, a string specifying a time
    !> zone rule.  If TZ is absent or TIME_ZONE_SYSTEM, use system wall
    !> clock time; this differs from the usual Fortmacs convention where an
    !> absent zone means current local time.  If TZ is "UTC0", use
    !> Universal Time.  If TZ is `time_zone_rule(offset, abbreviation)' of
    !> an offset (as from `current_time_zone' or `decode_time'), use the
    !> specified time zone without consideration for daylight saving time.
    !>
    !> Instead of calling this subroutine, you typically want something
    !> else.  To temporarily use a different time zone rule for just one
    !> invocation of `decode_time', `encode_time', or `format_date',
    !> pass the function a ZONE argument.  Port of
    !> timefns.c:Fset_time_zone_rule (and emacs_setenv_TZ).
    subroutine set_time_zone_rule(tz)
        character(len=*), intent(in), optional :: tz

        if (.not. present(tz)) then
            call system_set_time_zone()
        else if (tz == TIME_ZONE_SYSTEM) then
            call system_set_time_zone()
        else
            call system_set_time_zone(tz)
        end if
    end subroutine set_time_zone_rule

    ! ------------------------------------------------------------------
    ! The calendar
    ! ------------------------------------------------------------------

    !> Decode a timestamp into its second, minute, hour, day, month, year,
    !> day of week, daylight saving flag and UTC offset.  The optional TIME
    !> is the time value to convert (absent: the current time).
    !>
    !> The optional ZONE is absent for Fortmacs local time, "UTC0" for
    !> Universal Time, TIME_ZONE_SYSTEM for system wall clock time, or a
    !> string as in the TZ environment variable.  It can also be
    !> `time_zone_rule(offset, abbreviation)' of an offset (as from
    !> `current_time_zone' or `decode_time') applied without consideration
    !> for daylight saving time.
    !>
    !> The `second' of the result is a time value with the same precision
    !> as TIME; its `seconds' is the integer second.
    !>
    !> The result has the following nine components: SECOND is a time value
    !> representing a nonnegative value less than 60 (or less than 61 if the
    !> operating system supports leap seconds).  MINUTE is an integer
    !> between 0 and 59.  HOUR is an integer between 0 and 23.  DAY is an
    !> integer between 1 and 31.  MONTH is an integer between 1 and 12.
    !> YEAR is the year number, an integer; 0 represents 1 BC.  WEEKDAY is
    !> the day of week, an integer between 0 and 6, where 0 is Sunday.  DAYLIGHT_SAVING_TIME
    !> is DECODED_TIME_DAYLIGHT_SAVING_TIME_YES if daylight saving time is in effect,
    !> DECODED_TIME_DAYLIGHT_SAVING_TIME_NO if it is not in effect, and
    !> DECODED_TIME_DAYLIGHT_SAVING_TIME_UNKNOWN if daylight saving information is not
    !> available.  ZONE is an integer indicating the UTC offset in seconds,
    !> i.e., the number of seconds east of Greenwich.
    !> Port of timefns.c:Fdecode_time (FORM t).
    function decode_time(time, zone, status) result(decoded)
        type(time_type), intent(in), optional :: time
        character(len=*), intent(in), optional :: zone
        type(status_type), intent(out), optional :: status
        type(decoded_time_type) :: decoded
        type(time_type) :: t
        type(broken_down_time_type) :: tm
        logical :: ok

        if (present(status)) status = status_type()

        if (present(time)) then
            t = time
        else
            t = current_time()
        end if

        call localtime_in_zone(t, zone, tm, ok)

        if (.not. ok) then
            call time_overflow(status)
            return
        end if

        decoded%second = time_type(tm%second, t%count, t%count_rate)
        decoded%minute = tm%minute
        decoded%hour = tm%hour
        decoded%day = tm%day
        decoded%month = tm%month + 1
        decoded%year = int(tm%year, int64) + 1900
        decoded%weekday = tm%weekday

        if (tm%daylight_saving_time < 0) then
            decoded%daylight_saving_time = DECODED_TIME_DAYLIGHT_SAVING_TIME_UNKNOWN
        else if (tm%daylight_saving_time == 0) then
            decoded%daylight_saving_time = DECODED_TIME_DAYLIGHT_SAVING_TIME_NO
        else
            decoded%daylight_saving_time = DECODED_TIME_DAYLIGHT_SAVING_TIME_YES
        end if

        allocate (decoded%zone, source=int(tm%offset))
    end function decode_time

    !> Convert TIME to a timestamp.
    !>
    !> TIME is a decoded time in the style of `decode_time', so that
    !> encode_time(decode_time(...)) works.  Its ZONE is the UTC offset in
    !> seconds, applied without consideration for daylight saving time, or
    !> unallocated for Fortmacs local time; the optional ZONE argument, a
    !> zone as in `decode_time' (a TZ string, "UTC0" for Universal Time,
    !> TIME_ZONE_SYSTEM or a `time_zone_rule'), is used instead when
    !> present.  If the zone has daylight
    !> saving transitions, DAYLIGHT_SAVING_TIME is DECODED_TIME_DAYLIGHT_SAVING_TIME_YES for daylight saving
    !> time, DECODED_TIME_DAYLIGHT_SAVING_TIME_NO for standard time, and
    !> DECODED_TIME_DAYLIGHT_SAVING_TIME_UNKNOWN to cause the daylight saving flag to be
    !> guessed.  A decoded_time_type made with only SECOND through YEAR
    !> leaves DAYLIGHT_SAVING_TIME unknown and the zone local.
    !>
    !> The range of supported years is at least 1970 to the near future.
    !> Out-of-range values for SECOND through MONTH are brought into range
    !> via date arithmetic.  This can be tricky especially when combined
    !> with DAYLIGHT_SAVING_TIME; see Info node `(elisp)Time Conversion' for details and
    !> caveats.  Port of timefns.c:Fencode_time (and check_tm_member).
    type(time_type) function encode_time(time, zone, status) result(encoded)
        type(decoded_time_type), intent(in) :: time
        character(len=*), intent(in), optional :: zone
        type(status_type), intent(out), optional :: status
        type(saved_time_zone_type) :: saved
        integer :: fields(9)
        integer(int64) :: value
        logical :: ok

        if (present(status)) status = status_type()
        encoded = time_type()

        if (abs(time%second%seconds) > huge(0) .or. abs(time%year - 1900) > huge(0)) then
            call time_overflow(status)
            return
        end if

        fields = [int(time%second%seconds), time%minute, time%hour, time%day, time%month - 1, &
                  int(time%year - 1900), -1, 0, -1]

        if (present(zone) .or. .not. allocated(time%zone)) then
            if (time%daylight_saving_time /= DECODED_TIME_DAYLIGHT_SAVING_TIME_UNKNOWN) fields(9) = merge(1, 0, &
                                                                 time%daylight_saving_time == DECODED_TIME_DAYLIGHT_SAVING_TIME_YES)
        end if

        if (present(zone)) then
            call time_zone_begin(zone, saved)
        else if (allocated(time%zone)) then
            call time_zone_begin(time_zone_rule(time%zone), saved)
        else
            call time_zone_begin(saved=saved)
        end if

        call system_mktime(fields, value, ok)
        call time_zone_end(saved)

        if (.not. ok) then
            call time_overflow(status)
            return
        end if

        encoded = time_type(value, time%second%count, time%second%count_rate)
    end function encode_time

    ! ------------------------------------------------------------------
    ! Text
    ! ------------------------------------------------------------------

    !> The date format of FORMAT, a Fortran format with edit descriptors,
    !> and PARTS, the DATE_, TIME_ and ZONE_ constants of the parts it writes, in order: the
    !> constructor date_format_type(format, parts).  A FORMAT longer than
    !> DATE_FORMAT_LENGTH or more than DATE_FORMAT_MAXIMUM_PARTS parts make
    !> a date format that `format_date' reports as an error.  Not a port.
    pure function date_format_of_parts(format, parts) result(date_format)
        character(len=*), intent(in) :: format
        integer, intent(in) :: parts(:)
        type(date_format_type) :: date_format
        integer :: n

        n = min(size(parts), DATE_FORMAT_MAXIMUM_PARTS)
        date_format%format = format
        date_format%parts(:n) = parts(:n)
        date_format%is_truncated = len(format) > DATE_FORMAT_LENGTH .or. size(parts) > DATE_FORMAT_MAXIMUM_PARTS
    end function date_format_of_parts

    !> Return the time value TIME as text written with DATE_FORMAT, a
    !> Fortran format and the parts of the date it writes (see
    !> `date_format_type').  A time value that is absent stands for the
    !> current time.
    !>
    !> The optional ZONE is absent for Fortmacs local time, "UTC0" for
    !> Universal Time, TIME_ZONE_SYSTEM for system wall clock time, or a
    !> string as in the TZ environment variable.  It can also be
    !> `time_zone_rule(offset, abbreviation)' of an offset (as from
    !> `current_time_zone' or `decode_time') applied without consideration
    !> for daylight saving time.
    !>
    !> Names are the English ones (the C locale).  Blanks at the end of the
    !> text are kept, except in a format that moves left (T or TL).  An
    !> error condition occurs, and a check of STATUS finds it, when the
    !> format is not a Fortran format, when its data edit descriptors and
    !> the parts differ in number or kind (an I descriptor for a name),
    !> when a format reversion or an unlimited repeat (*) would be needed,
    !> and when the time does not fit ("Specified time is not
    !> representable").
    !>
    !> The format is applied in two steps, since one `write' cannot take a
    !> list of items whose types are chosen at run time: each part is
    !> written alone with its own data edit descriptor (and the sign mode in
    !> effect there), then one `write' of the whole format, with every data
    !> edit descriptor turned into A, writes those pieces with the
    !> character string and control edit descriptors between them.
    !> Not a port: upstream's format-time-string takes a strftime string
    !> (owner, 2026-11-06).
    function format_date(date_format, time, zone, status) result(text)
        type(date_format_type), intent(in) :: date_format
        type(time_type), intent(in), optional :: time
        character(len=*), intent(in), optional :: zone
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: text
        character(len=DESCRIPTOR_LENGTH + 3) :: descriptors(DATE_FORMAT_MAXIMUM_PARTS)
        character(len=2) :: letters(DATE_FORMAT_MAXIMUM_PARTS)
        character(len=PART_TEXT_LENGTH) :: pieces(DATE_FORMAT_MAXIMUM_PARTS)
        character(len=DATE_TEXT_LENGTH) :: buffer
        character(len=256) :: message
        character(len=:), allocatable :: items_format, name
        integer :: widths(DATE_FORMAT_MAXIMUM_PARTS), lengths(DATE_FORMAT_MAXIMUM_PARTS)
        integer :: part_count, item_count, k, part, iostat, last
        logical :: moves_left, ok
        type(time_type) :: t
        type(broken_down_time_type) :: tm

        status = status_type()
        text = ""

        if (date_format%is_truncated) then
            write (message, '("Invalid date format: more than ", i0, " characters or ", i0, " parts")') DATE_FORMAT_LENGTH, &
                DATE_FORMAT_MAXIMUM_PARTS
            call set_error(status, GENERIC_ERROR_CODE, trim(message))
            return
        end if

        part_count = findloc(date_format%parts /= DATE_NONE, .true., dim=1, back=.true.)

        do k = 1, part_count
            part = date_format%parts(k)

            if (.not. (is_date_number_part(part) .or. is_date_text_part(part))) then
                write (message, '("Invalid date format: part ", i0, " is not a date part")') k
                call set_error(status, GENERIC_ERROR_CODE, trim(message))
                return
            end if
        end do

        call split_date_format(trim(date_format%format), items_format, descriptors, letters, widths, item_count, moves_left, &
                               status)
        if (.not. status%ok()) return

        if (item_count /= part_count) then
            write (message, '("Invalid date format: ", i0, " data edit descriptors for ", i0, " parts")') item_count, part_count
            call set_error(status, GENERIC_ERROR_CODE, trim(message))
            return
        end if

        if (present(time)) then
            t = time
        else
            t = current_time()
        end if

        call localtime_in_zone(t, zone, tm, ok)

        if (.not. ok) then
            call time_overflow(status)
            return
        end if

        do k = 1, part_count
            part = date_format%parts(k)

            if (is_date_text_part(part)) then
                if (letters(k) /= "A" .and. letters(k) /= "G") then
                    write (message, '("Invalid date format: ", a, " cannot write part ", i0, ", a text")') &
                        trim(descriptors(k)), k
                    call set_error(status, GENERIC_ERROR_CODE, trim(message))
                    return
                end if

                name = date_part_text(part, tm)
                write (pieces(k), "("//trim(descriptors(k))//")", iostat=iostat, iomsg=message) name
            else
                if (verify(trim(letters(k)), "IBOZG") /= 0) then
                    write (message, '("Invalid date format: ", a, " cannot write part ", i0, ", a number")') &
                        trim(descriptors(k)), k
                    call set_error(status, GENERIC_ERROR_CODE, trim(message))
                    return
                end if

                write (pieces(k), "("//trim(descriptors(k))//")", iostat=iostat, iomsg=message) date_part_number(part, tm)
            end if

            if (iostat /= 0) then
                call set_error(status, GENERIC_ERROR_CODE, "Invalid date format: "//trim(message))
                return
            end if

            if (widths(k) > 0) then
                lengths(k) = widths(k)
            else if (is_date_text_part(part)) then
                lengths(k) = len(name)
            else
                lengths(k) = len_trim(pieces(k))
            end if
        end do

        if (moves_left) then
            write (buffer, "("//items_format//")", iostat=iostat, iomsg=message) (pieces(k) (1:lengths(k)), k=1, part_count)
        else if (len_trim(items_format) == 0) then
            write (buffer, '("'//END_OF_DATE//'")', iostat=iostat, iomsg=message)
        else
            write (buffer, "("//items_format//', "'//END_OF_DATE//'")', iostat=iostat, iomsg=message) &
                (pieces(k) (1:lengths(k)), k=1, part_count)
        end if

        if (iostat /= 0) then
            call set_error(status, GENERIC_ERROR_CODE, "Invalid date format: "//trim(message))
            return
        end if

        last = index(buffer, END_OF_DATE, back=.true.)

        if (last > 0) then
            text = buffer(:last - 1)
        else
            text = trim(buffer)
        end if
    end function format_date

    ! Split FORMAT, a Fortran format, for `format_date': ITEMS_FORMAT is
    ! what is between its outer parentheses with each data edit descriptor
    ! turned into A (its repeat count kept), and DESCRIPTORS, LETTERS and
    ! WIDTHS give, for each of the ITEM_COUNT items the format writes in
    ! order, its data edit descriptor (after the sign edit descriptor in
    ! effect, as "SP,I5.4"), the descriptor's letters and its field width
    ! (-1 for none); MOVES_LEFT tells whether the format has T or TL.  An
    ! error condition occurs when FORMAT is not a Fortran format, or when
    ! it uses a Hollerith constant, an unlimited repeat or a derived-type
    ! descriptor.  Not a port.
    subroutine split_date_format(format, items_format, descriptors, letters, widths, item_count, moves_left, status)
        character(len=*), intent(in) :: format
        character(len=:), allocatable, intent(out) :: items_format
        character(len=DESCRIPTOR_LENGTH + 3), intent(out) :: descriptors(:)
        character(len=2), intent(out) :: letters(:)
        integer, intent(out) :: widths(:), item_count
        logical, intent(out) :: moves_left
        type(status_type), intent(out) :: status
        character(len=*), parameter :: LOWER = "abcdefghijklmnopqrstuvwxyz", UPPER = "ABCDEFGHIJKLMNOPQRSTUVWXYZ", &
                                       DIGITS = "0123456789"
        type(format_token_type), allocatable :: tokens(:)
        character(len=:), allocatable :: inner, upper_inner
        character(len=2) :: sign
        integer, allocatable :: open_tokens(:)
        integer :: first, last, n, i, j, k, start, token_count, depth, repeat, width
        logical :: has_number

        status = status_type()
        items_format = ""
        item_count = 0
        moves_left = .false.
        descriptors = ""
        letters = ""
        widths = -1
        first = verify(format, " ")
        last = verify(format, " ", back=.true.)

        if (first == 0) then
            call fail("not a Fortran format")
            return
        end if

        if (format(first:first) /= "(" .or. format(last:last) /= ")" .or. first == last) then
            call fail("not a Fortran format")
            return
        end if

        inner = format(first + 1:last - 1)
        upper_inner = inner

        do i = 1, len(inner)
            k = index(LOWER, inner(i:i))
            if (k > 0) upper_inner(i:i) = UPPER(k:k)
        end do

        n = len(inner)
        allocate (tokens(max(n, 1)), open_tokens(max(n, 1)))
        token_count = 0
        depth = 0
        i = 1

        do while (i <= n)
            ! Separators.
            if (inner(i:i) == " " .or. inner(i:i) == ",") then
                items_format = items_format//inner(i:i)
                i = i + 1
                cycle
            end if

            ! A character string edit descriptor, its delimiter doubled inside.
            if (inner(i:i) == "'" .or. inner(i:i) == '"') then
                j = i + 1

                do
                    if (j > n) then
                        call fail("unterminated character string")
                        return
                    end if

                    if (inner(j:j) == inner(i:i)) then
                        if (j == n) exit
                        if (inner(j + 1:j + 1) /= inner(i:i)) exit
                        j = j + 1
                    end if

                    j = j + 1
                end do

                items_format = items_format//inner(i:j)
                i = j + 1
                cycle
            end if

            ! A repeat count, a position or a scale factor.
            start = i
            if (inner(i:i) == "+" .or. inner(i:i) == "-") i = i + 1
            repeat = 0
            has_number = .false.

            do while (i <= n)
                if (index(DIGITS, inner(i:i)) == 0) exit
                if (repeat < 100000) repeat = 10*repeat + index(DIGITS, inner(i:i)) - 1
                has_number = .true.
                i = i + 1
            end do

            if (i > n) then
                call fail("a number at the end")
                return
            end if

            if (inner(start:start) == "+" .or. inner(start:start) == "-") then
                if (upper_inner(i:i) /= "P" .or. .not. has_number) then
                    call fail("a sign before "//inner(i:i))
                    return
                end if
            end if

            if (has_number .and. repeat == 0 .and. upper_inner(i:i) /= "P") then
                call fail("a repeat count of 0")
                return
            end if

            if (.not. has_number) repeat = 1

            select case (upper_inner(i:i))
            case ("(")
                token_count = token_count + 1
                tokens(token_count) = format_token_type(kind=TOKEN_OPEN, repeat=repeat)
                depth = depth + 1
                open_tokens(depth) = token_count
                items_format = items_format//inner(start:i)
                i = i + 1
            case (")")
                if (has_number .or. depth == 0) then
                    call fail("unbalanced parentheses")
                    return
                end if

                token_count = token_count + 1
                tokens(token_count) = format_token_type(kind=TOKEN_CLOSE)
                tokens(open_tokens(depth))%partner = token_count
                depth = depth - 1
                items_format = items_format//inner(i:i)
                i = i + 1
            case ("/", "X", "P")
                if (upper_inner(i:i) == "P" .and. .not. has_number) then
                    call fail("P without a scale factor")
                    return
                end if

                items_format = items_format//inner(start:i)
                i = i + 1
            case (":")
                if (has_number) then
                    call fail("a number before :")
                    return
                end if

                items_format = items_format//inner(i:i)
                i = i + 1
            case ("T")
                j = i + 1

                if (j <= n) then
                    if (upper_inner(j:j) == "L" .or. upper_inner(j:j) == "R") j = j + 1
                end if

                if (upper_inner(j - 1:j - 1) /= "R") moves_left = .true.
                k = j

                do while (j <= n)
                    if (index(DIGITS, inner(j:j)) == 0) exit
                    j = j + 1
                end do

                if (has_number .or. j == k) then
                    call fail("T, TL or TR without a position")
                    return
                end if

                items_format = items_format//inner(i:j - 1)
                i = j
            case ("S")
                j = i + 1

                if (j <= n) then
                    if (upper_inner(j:j) == "P" .or. upper_inner(j:j) == "S") j = j + 1
                end if

                if (has_number) then
                    call fail("a number before "//inner(i:j - 1))
                    return
                end if

                token_count = token_count + 1
                tokens(token_count) = format_token_type(kind=TOKEN_SIGN, descriptor=upper_inner(i:j - 1))
                items_format = items_format//inner(i:j - 1)
                i = j
            case ("B", "R", "D")
                j = i + 1
                k = 0

                if (j <= n) then
                    select case (upper_inner(i:j))
                    case ("BN", "BZ", "RU", "RD", "RZ", "RN", "RC", "RP", "DC", "DP")
                        k = 1
                    case ("DT")
                        call fail("a derived-type edit descriptor")
                        return
                    case default
                        k = 0
                    end select
                end if

                if (k == 1) then
                    if (has_number) then
                        call fail("a number before "//inner(i:j))
                        return
                    end if

                    items_format = items_format//inner(i:j)
                    i = j + 1
                else if (upper_inner(i:i) == "R") then
                    call fail("unknown edit descriptor "//inner(i:i))
                    return
                else
                    call add_data_descriptor()
                    if (.not. status%ok()) return
                end if
            case ("I", "O", "Z", "F", "E", "G", "L", "A")
                call add_data_descriptor()
                if (.not. status%ok()) return
            case ("*")
                call fail("an unlimited repeat")
                return
            case ("H")
                call fail("a Hollerith constant")
                return
            case default
                call fail("unknown edit descriptor "//inner(i:i))
                return
            end select
        end do

        if (depth /= 0) then
            call fail("unbalanced parentheses")
            return
        end if

        sign = ""
        call expand_format_tokens(tokens, 1, token_count, sign, descriptors, letters, widths, item_count)

    contains

        ! The data edit descriptor at I, with the repeat count REPEAT
        ! written from START: a token, and A in ITEMS_FORMAT.
        subroutine add_data_descriptor()
            character(len=2) :: name

            j = i + 1

            if (upper_inner(i:i) == "E" .and. j <= n) then
                if (index("NSX", upper_inner(j:j)) > 0) j = j + 1
            end if

            name = upper_inner(i:j - 1)
            width = -1

            do while (j <= n)
                if (index(DIGITS, inner(j:j)) == 0) exit
                if (width < 0) width = 0
                if (width < 100000) width = 10*width + index(DIGITS, inner(j:j)) - 1
                j = j + 1
            end do

            do while (j <= n)
                if (index(DIGITS//".", inner(j:j)) == 0) exit
                j = j + 1
            end do

            ! The exponent width of E, EN, ES, EX, D and G.
            if (j < n .and. index("EDG", name(1:1)) > 0) then
                if (upper_inner(j:j) == "E" .and. index(DIGITS, inner(j + 1:j + 1)) > 0) then
                    j = j + 1

                    do while (j <= n)
                        if (index(DIGITS, inner(j:j)) == 0) exit
                        j = j + 1
                    end do
                end if
            end if

            if (j - i > DESCRIPTOR_LENGTH) then
                call fail("a data edit descriptor too long")
                return
            end if

            token_count = token_count + 1
            tokens(token_count) = format_token_type(kind=TOKEN_DATA, repeat=repeat, width=width, letters=name, &
                                                    descriptor=upper_inner(i:j - 1))
            items_format = items_format//inner(start:i - 1)//"a"
            i = j
        end subroutine add_data_descriptor

        ! The error condition "Invalid date format `FORMAT': WHAT".
        subroutine fail(what)
            character(len=*), intent(in) :: what

            call set_error(status, GENERIC_ERROR_CODE, "Invalid date format `"//format//"': "//what)
        end subroutine fail
    end subroutine split_date_format

    ! Add to DESCRIPTORS, LETTERS and WIDTHS, after their ITEM_COUNT
    ! entries, the data edit descriptors that TOKENS(FIRST:LAST) use for
    ! one item after another, each after the sign edit descriptor SIGN in
    ! effect (which they change); stop counting past the size of
    ! DESCRIPTORS.  Not a port.
    recursive subroutine expand_format_tokens(tokens, first, last, sign, descriptors, letters, widths, item_count)
        type(format_token_type), intent(in) :: tokens(:)
        integer, intent(in) :: first, last
        character(len=2), intent(inout) :: sign
        character(len=DESCRIPTOR_LENGTH + 3), intent(inout) :: descriptors(:)
        character(len=2), intent(inout) :: letters(:)
        integer, intent(inout) :: widths(:), item_count
        integer :: i, r

        i = first

        do while (i <= last)
            select case (tokens(i)%kind)
            case (TOKEN_DATA)
                do r = 1, tokens(i)%repeat
                    item_count = item_count + 1
                    if (item_count > size(descriptors)) return

                    if (len_trim(sign) > 0) then
                        descriptors(item_count) = trim(sign)//","//tokens(i)%descriptor
                    else
                        descriptors(item_count) = tokens(i)%descriptor
                    end if

                    letters(item_count) = tokens(i)%letters
                    widths(item_count) = tokens(i)%width
                end do
            case (TOKEN_SIGN)
                sign = tokens(i)%descriptor(1:2)
            case (TOKEN_OPEN)
                do r = 1, tokens(i)%repeat
                    call expand_format_tokens(tokens, i + 1, tokens(i)%partner - 1, sign, descriptors, letters, widths, &
                                              item_count)
                    if (item_count > size(descriptors)) return
                end do

                i = tokens(i)%partner
            case default
                ! A closing parenthesis: its group ends at LAST.
                continue
            end select

            i = i + 1
        end do
    end subroutine expand_format_tokens

    ! Whether PART is a number part of a date (DATE_YEAR to
    ! ZONE_OFFSET_SECONDS).  Not a port.
    elemental logical function is_date_number_part(part)
        integer, intent(in) :: part

        is_date_number_part = part >= FIRST_NUMBER_PART .and. part <= LAST_NUMBER_PART
    end function is_date_number_part

    ! Whether PART is a text part of a date (DATE_WEEKDAY_NAME to
    ! ZONE_OFFSET_SIGN).  Not a port.
    elemental logical function is_date_text_part(part)
        integer, intent(in) :: part

        is_date_text_part = part >= FIRST_TEXT_PART .and. part <= LAST_TEXT_PART
    end function is_date_text_part

    ! The number part PART of the broken-down time TM, as the strftime
    ! conversion it stands for gives it.  Port of
    ! strftime.c:__strftime_internal (the numbers of %Y, %C, %y, %G, %g,
    ! %V, %q, %m, %U, %W, %d, %j, %w, %u, %H, %I, %M, %S, %N, %s, %z).
    pure integer(int64) function date_part_number(part, tm) result(value)
        integer, intent(in) :: part
        type(broken_down_time_type), intent(in) :: tm
        integer :: n, year, year_adjust, days, d, offset

        offset = int(tm%offset)

        select case (part)
        case (DATE_YEAR)
            value = int(tm%year, int64) + 1900
        case (DATE_CENTURY)
            ! (tm_year - 99 * zero_thru_1899) / 100 + 19.
            n = tm%year
            if (tm%year >= -1900 .and. tm%year < 0) n = n - 99
            value = n/100 + 19
        case (DATE_YEAR_OF_CENTURY)
            value = year_of_century(tm%year, 0)
        case (DATE_ISO_WEEK_YEAR, DATE_ISO_WEEK_YEAR_OF_CENTURY, DATE_ISO_WEEK)
            ! YEAR is a leap year if and only if tm_year + 1900 is.
            year = tm%year + merge(300, -100, tm%year < 0)
            year_adjust = 0
            days = iso_week_days(tm%year_day, tm%weekday)

            if (days < 0) then
                ! This ISO week belongs to the previous year.
                year_adjust = -1
                days = iso_week_days(tm%year_day + (365 + merge(1, 0, is_leap_year(year - 1))), tm%weekday)
            else
                d = iso_week_days(tm%year_day - (365 + merge(1, 0, is_leap_year(year))), tm%weekday)

                if (0 <= d) then
                    ! This ISO week belongs to the next year.
                    year_adjust = 1
                    days = d
                end if
            end if

            select case (part)
            case (DATE_ISO_WEEK_YEAR)
                value = int(tm%year, int64) + 1900 + year_adjust
            case (DATE_ISO_WEEK_YEAR_OF_CENTURY)
                value = year_of_century(tm%year, year_adjust)
            case default
                value = days/7 + 1
            end select
        case (DATE_QUARTER)
            value = tm%month/3 + 1
        case (DATE_MONTH)
            value = tm%month + 1
        case (DATE_WEEK_STARTING_SUNDAY)
            value = (tm%year_day - tm%weekday + 7)/7
        case (DATE_WEEK_STARTING_MONDAY)
            value = (tm%year_day - mod(tm%weekday - 1 + 7, 7) + 7)/7
        case (DATE_DAY)
            value = tm%day
        case (DATE_DAY_OF_YEAR)
            value = tm%year_day + 1
        case (DATE_WEEKDAY)
            value = tm%weekday
        case (DATE_ISO_WEEKDAY)
            value = mod(tm%weekday - 1 + 7, 7) + 1
        case (TIME_HOUR)
            value = tm%hour
        case (TIME_HOUR_OF_TWELVE_HOUR_CLOCK)
            value = mod(tm%hour + 11, 12) + 1
        case (TIME_MINUTE)
            value = tm%minute
        case (TIME_SECOND)
            value = tm%second
        case (TIME_MILLISECOND)
            value = tm%nanoseconds/1000000
        case (TIME_MICROSECOND)
            value = tm%nanoseconds/1000
        case (TIME_NANOSECOND)
            value = tm%nanoseconds
        case (TIME_SECONDS_SINCE_EPOCH)
            ! (strftime.c regenerates the count with mktime_z; it is the
            ! time that was broken down.)
            value = tm%seconds
        case (ZONE_OFFSET)
            value = sign(abs(offset)/3600*100 + mod(abs(offset)/60, 60), offset)
        case (ZONE_OFFSET_HOURS)
            value = abs(offset)/3600
        case (ZONE_OFFSET_MINUTES)
            value = mod(abs(offset)/60, 60)
        case (ZONE_OFFSET_SECONDS)
            value = mod(abs(offset), 60)
        case default
            value = 0
        end select
    end function date_part_number

    ! The last two digits of the year TM_YEAR + 1900 + YEAR_ADJUST, as %y
    ! and %g write them.  Port of strftime.c:__strftime_internal (%y, %g).
    pure integer function year_of_century(tm_year, year_adjust)
        integer, intent(in) :: tm_year, year_adjust

        year_of_century = mod(mod(tm_year, 100) + year_adjust, 100)

        if (year_of_century < 0) then
            if (tm_year < -1900 - year_adjust) then
                year_of_century = -year_of_century
            else
                year_of_century = year_of_century + 100
            end if
        end if
    end function year_of_century

    ! The text part PART of the broken-down time TM, as the strftime
    ! conversion it stands for gives it in the C locale.  Port of
    ! strftime.c:__strftime_internal (%A, %a, %B, %b, %p, %P, %Z, the
    ! flags ^ and #, and the sign of %z).
    pure function date_part_text(part, tm) result(text)
        integer, intent(in) :: part
        type(broken_down_time_type), intent(in) :: tm
        character(len=:), allocatable :: text

        select case (part)
        case (DATE_WEEKDAY_NAME)
            text = trim(WEEKDAY_NAMES(tm%weekday))
        case (DATE_WEEKDAY_ABBREVIATION)
            text = WEEKDAY_NAMES(tm%weekday) (1:3)
        case (DATE_WEEKDAY_NAME_UPPER_CASE)
            text = changed_case(trim(WEEKDAY_NAMES(tm%weekday)), .true.)
        case (DATE_WEEKDAY_ABBREVIATION_UPPER_CASE)
            text = changed_case(WEEKDAY_NAMES(tm%weekday) (1:3), .true.)
        case (DATE_MONTH_NAME)
            text = trim(MONTH_NAMES(tm%month))
        case (DATE_MONTH_ABBREVIATION)
            text = MONTH_NAMES(tm%month) (1:3)
        case (DATE_MONTH_NAME_UPPER_CASE)
            text = changed_case(trim(MONTH_NAMES(tm%month)), .true.)
        case (DATE_MONTH_ABBREVIATION_UPPER_CASE)
            text = changed_case(MONTH_NAMES(tm%month) (1:3), .true.)
        case (TIME_MERIDIEM)
            text = merge("PM", "AM", 12 <= tm%hour)
        case (TIME_MERIDIEM_LOWER_CASE)
            text = merge("pm", "am", 12 <= tm%hour)
        case (ZONE_ABBREVIATION)
            text = tm%abbreviation
        case (ZONE_ABBREVIATION_LOWER_CASE)
            text = changed_case(tm%abbreviation, .false.)
        case (ZONE_OFFSET_SIGN)
            text = "+"
            if (tm%offset < 0) text = "-"

            if (tm%offset == 0 .and. len(tm%abbreviation) > 0) then
                if (tm%abbreviation(1:1) == "-") text = "-"
            end if
        case default
            text = ""
        end select
    end function date_part_text

    ! TEXT with its ASCII letters in upper case (UPPER_CASE true) or in
    ! lower case.  Port of strftime.c:__strftime_internal (cpy, the ^ and
    ! # flags in the C locale).
    pure function changed_case(text, upper_case) result(changed)
        character(len=*), intent(in) :: text
        logical, intent(in) :: upper_case
        character(len=len(text)) :: changed
        character(len=*), parameter :: LOWER = "abcdefghijklmnopqrstuvwxyz", UPPER = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        integer :: i, k

        changed = text

        do i = 1, len(text)
            if (upper_case) then
                k = index(LOWER, text(i:i))
                if (k > 0) changed(i:i) = UPPER(k:k)
            else
                k = index(UPPER, text(i:i))
                if (k > 0) changed(i:i) = LOWER(k:k)
            end if
        end do
    end function changed_case

    ! The number of days from the first day of the first ISO week of this
    ! year to the year day YEAR_DAY with week day WEEKDAY.  ISO weeks start on
    ! Monday; the first ISO week has the year's first Thursday.  Port of
    ! strftime.c:iso_week_days.
    pure integer function iso_week_days(year_day, weekday)
        integer, intent(in) :: year_day, weekday
        integer, parameter :: ISO_WEEK_START_WDAY = 1, ISO_WEEK1_WDAY = 4
        ! (-YDAY_MINIMUM / 7 + 2) * 7: enough to make the first operand of mod
        ! nonnegative.
        integer, parameter :: BIG_ENOUGH_MULTIPLE_OF_7 = 378

        iso_week_days = year_day - mod(year_day - weekday + ISO_WEEK1_WDAY + BIG_ENOUGH_MULTIPLE_OF_7, 7) + &
                        ISO_WEEK1_WDAY - ISO_WEEK_START_WDAY
    end function iso_week_days

    ! Whether YEAR is a leap year.  Port of strftime.c:__isleap.
    pure logical function is_leap_year(year)
        integer, intent(in) :: year

        is_leap_year = .false.

        if (mod(year, 4) == 0) then
            is_leap_year = mod(year, 100) /= 0 .or. mod(year, 400) == 0
        end if
    end function is_leap_year

    !> Return the current local time, as a human-readable string.
    !> Programs can use this function to decode a time, since the number
    !> of columns in each field is fixed if the year is in the range
    !> 1000-9999.  The format is `Sun Sep 16 01:03:52 1973'.
    !> However, see also the functions `decode_time' and
    !> `format_date' which provide a much more powerful and general
    !> facility.
    !>
    !> If TIME is given, it is the time value to format instead of the
    !> current time.
    !>
    !> The optional ZONE is absent for Fortmacs local time, "UTC0" for
    !> Universal Time, TIME_ZONE_SYSTEM for system wall clock time, or a
    !> string as in the TZ environment variable.  It can also be
    !> `time_zone_rule(offset, abbreviation)' of an offset (as from
    !> `current_time_zone' or `decode_time') applied without consideration
    !> for daylight saving time.
    !> Port of timefns.c:Fcurrent_time_string.
    function current_time_string(time, zone, status) result(text)
        type(time_type), intent(in), optional :: time
        character(len=*), intent(in), optional :: zone
        type(status_type), intent(out), optional :: status
        character(len=:), allocatable :: text
        type(time_type) :: t
        type(broken_down_time_type) :: tm
        character(len=48) :: buffer
        logical :: ok

        if (present(status)) status = status_type()
        text = ""

        if (present(time)) then
            t = time_type(time%seconds)
        else
            t = current_time()
            t = time_type(t%seconds)
        end if

        call localtime_in_zone(t, zone, tm, ok)

        if (.not. ok) then
            call time_overflow(status)
            return
        end if

        write (buffer, "(a3, 1x, a3, i3, 1x, i2.2, ':', i2.2, ':', i2.2, 1x, i0)") WEEKDAY_NAMES(tm%weekday), &
            MONTH_NAMES(tm%month), tm%day, tm%hour, tm%minute, tm%second, int(tm%year, int64) + 1900
        text = trim(buffer)
    end function current_time_string

    !> Return the offset and name for the local time zone.
    !> OFFSET is an integer number of seconds ahead of UTC (east of
    !> Greenwich).  A negative value means west of Greenwich.
    !> NAME is a string giving the name of the time zone.
    !> If TIME is given, the time zone offset is determined from it
    !> instead of using the current time.
    !>
    !> The optional ZONE is absent for Fortmacs local time, "UTC0" for
    !> Universal Time, TIME_ZONE_SYSTEM for system wall clock time, or a
    !> string as in the TZ environment variable.  It can also be
    !> `time_zone_rule(offset, abbreviation)' of an offset (as from
    !> `current_time_zone' or `decode_time') applied without consideration
    !> for daylight saving time.
    !> Port of timefns.c:Fcurrent_time_zone.
    function current_time_zone(time, zone, status) result(found)
        type(time_type), intent(in), optional :: time
        character(len=*), intent(in), optional :: zone
        type(status_type), intent(out), optional :: status
        type(time_zone_type) :: found
        type(time_type) :: t
        type(broken_down_time_type) :: tm
        character(len=16) :: buffer
        integer :: offset, hour, minimum_second, minute, second
        logical :: ok

        if (present(status)) status = status_type()
        found%name = ""

        if (present(time)) then
            t = time_type(time%seconds)
        else
            t = current_time()
            t = time_type(t%seconds)
        end if

        call localtime_in_zone(t, zone, tm, ok)

        if (.not. ok) then
            call time_overflow(status)
            return
        end if

        offset = int(tm%offset)
        found%offset = offset
        found%name = tm%abbreviation

        if (len(found%name) == 0) then
            ! No local time zone name is available; use numeric zone instead.
            hour = abs(offset)/3600
            minimum_second = mod(abs(offset), 3600)
            minute = minimum_second/60
            second = mod(minimum_second, 60)
            write (buffer, "(i2.2)") hour
            found%name = merge("-", "+", offset < 0)//trim(buffer)

            if (minimum_second /= 0) then
                write (buffer, "(i2.2)") minute
                found%name = found%name//trim(buffer)

                if (second /= 0) then
                    write (buffer, "(i2.2)") second
                    found%name = found%name//trim(buffer)
                end if
            end if
        end if
    end function current_time_zone
end module time_functions_mod
