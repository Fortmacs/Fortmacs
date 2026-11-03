! timefns_mod.f90 -- time values, time zones and the formatting of times.
! Ported from GNU Emacs 31.1 src/timefns.c and lib/strftime.c (in part:
! the C locale's names and formats).
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

!> Time values and what Emacs does with them: the clock (`current_time'),
!> arithmetic and comparison (`time_add', `is_time_less'), conversion
!> (`time_convert', `real'), the calendar in a time zone
!> (`decode_time', `encode_time') and text (`format_time_string',
!> `current_time_string').
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
!> ("UTC0" for upstream's t); absent, it is Emacs's local time (R6-12,
!> R6-13): `time_zone_rule' turns a UTC offset, with or without an
!> abbreviation, into one, and TIME_ZONE_SYSTEM stands for the system's
!> wall clock time (upstream's `wall').  The calendar comes from the C
!> library (localtime_r, mktime) under that rule, as upstream's
!> localtime_rz and mktime_z do on GNU/Linux; `format_time_string' is a
!> port of Gnulib's nstrftime in the C locale.
module timefns_mod
    use, intrinsic :: iso_fortran_env, only: int32, int64, real64
    use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
    use error_mod, only: status_type, set_error, FM_GENERIC_ERROR
    use sysdep_mod, only: emacs_clock_realtime, emacs_clock_process_cputime, sys_localtime, sys_mktime, &
                          sys_set_time_zone
    implicit none(type, external)
    private

    public :: current_time, current_cpu_time, time_add, time_subtract, is_time_less, is_time_equal, real, &
              time_convert, decode_time, encode_time, format_time_string, current_time_string, current_time_zone, &
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

    !> `dst' of a decoded time: daylight saving time is in effect.
    integer, parameter, public :: DECODED_TIME_DST_YES = 1
    !> `dst' of a decoded time: daylight saving time is not in effect.
    integer, parameter, public :: DECODED_TIME_DST_NO = 0
    !> `dst' of a decoded time: daylight saving information is not
    !> available (for `encode_time': guess it).
    integer, parameter, public :: DECODED_TIME_DST_UNKNOWN = -1

    !> A decoded time, as `decode_time' gives it and `encode_time' takes it
    !> (simple.el's decoded-time).  `second' is a nonnegative time value
    !> less than 60 (or 61 where the system has leap seconds), `minute'
    !> 0-59, `hour' 0-23, `day' 1-31, `month' 1-12, `year' the year number
    !> (0 is 1 BC), `weekday' 0-6 with 0 for Sunday, `dst' one of the
    !> DECODED_TIME_DST_ constants, and `zone' the UTC offset in seconds
    !> east of Greenwich (unallocated: Emacs's local time).
    type, public :: decoded_time_type
        type(time_type) :: second
        integer :: minute = 0
        integer :: hour = 0
        integer :: day = 1
        integer :: month = 1
        integer(int64) :: year = 1970
        integer :: weekday = 4
        integer :: dst = DECODED_TIME_DST_UNKNOWN
        integer, allocatable :: zone
    end type decoded_time_type

    !> The offset of a time zone in seconds ahead of UTC (east of
    !> Greenwich; negative west of it) and its name, as
    !> `current_time_zone' gives them.
    type, public :: time_zone_type
        integer :: offset = 0
        character(len=:), allocatable :: name
    end type time_zone_type

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
    !> (if you need time as a string) `format_time_string'.
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
                   year_day = 0, dst = -1
        integer(int64) :: offset = 0
        character(len=:), allocatable :: abbreviation
    end type broken_down_time_type

    ! The TZ rule in effect before a switch to another one.
    type :: saved_time_zone_type
        logical :: switched = .false.
        logical :: had_rule = .false.
        character(len=:), allocatable :: rule
    end type saved_time_zone_type

    ! strftime.c enum pad_style.
    integer, parameter :: ZERO_PAD = 0, ALWAYS_ZERO_PAD = 1, SIGN_PAD = 2, SPACE_PAD = 3, NO_PAD = 4

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
        integer(int64) :: sec
        integer(int32) :: nsec

        call emacs_clock_realtime(sec, nsec)
        now = time_type(sec, int(nsec, int64), TIMESPEC_COUNT_RATE)
    end function current_time

    !> Return the current CPU time along with its resolution: the
    !> processor time this process has used, with `count_rate' the C
    !> library's CLOCKS_PER_SEC (1000000).  Port of
    !> timefns.c:Fcurrent_cpu_time (clock(3) reads the same clock).
    type(time_type) function current_cpu_time() result(used)
        integer(int64), parameter :: CLOCKS_PER_SEC = 1000000_int64
        integer(int64) :: sec
        integer(int32) :: nsec

        call emacs_clock_process_cputime(sec, nsec)
        used = time_type(sec, int(nsec, int64)/(TIMESPEC_COUNT_RATE/CLOCKS_PER_SEC), CLOCKS_PER_SEC)
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
    !> (if you need time as a string) `format_time_string'.
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
            if (present(status)) call set_error(status, FM_GENERIC_ERROR, "Invalid time frequency")
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
            if (present(status)) call set_error(status, FM_GENERIC_ERROR, "Invalid time specification")
            return
        end if

        if (abs(seconds) >= 2.0_real64**63) then
            if (present(status)) call set_error(status, FM_GENERIC_ERROR, "Specified time is not representable")
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
    !> `encode_time', `format_time_string', `current_time_string' and
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
    ! system's wall clock time); nothing when RULE is absent (Emacs's
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
            call sys_set_time_zone()
        else
            call sys_set_time_zone(rule)
        end if
    end subroutine time_zone_begin

    ! Restore the time zone rule time_zone_begin saved in SAVED.  Port of
    ! timefns.c:xtzfree.
    subroutine time_zone_end(saved)
        type(saved_time_zone_type), intent(in) :: saved

        if (.not. saved%switched) return

        if (saved%had_rule) then
            call sys_set_time_zone(saved%rule)
        else
            call sys_set_time_zone()
        end if
    end subroutine time_zone_end

    ! The broken-down time TM of TIME under the time zone rule ZONE
    ! (absent: Emacs's local time); `ok' is false when its year does not
    ! fit.  Port of timefns.c:emacs_localtime_rz.
    subroutine localtime_in_zone(time, zone, tm, ok)
        type(time_type), intent(in) :: time
        character(len=*), intent(in), optional :: zone
        type(broken_down_time_type), intent(out) :: tm
        logical, intent(out) :: ok
        type(saved_time_zone_type) :: saved
        integer :: fields(9)

        call time_zone_begin(zone, saved)
        call sys_localtime(time%seconds, fields, tm%offset, tm%abbreviation, ok)
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
        tm%dst = fields(9)
    end subroutine localtime_in_zone

    ! An error condition: the time does not fit.  Port of
    ! timefns.c:time_overflow.
    subroutine time_overflow(status)
        type(status_type), intent(out), optional :: status

        if (present(status)) call set_error(status, FM_GENERIC_ERROR, "Specified time is not representable")
    end subroutine time_overflow

    !> Set the Emacs local time zone using TZ, a string specifying a time
    !> zone rule.  If TZ is absent or TIME_ZONE_SYSTEM, use system wall
    !> clock time; this differs from the usual Emacs convention where an
    !> absent zone means current local time.  If TZ is "UTC0", use
    !> Universal Time.  If TZ is `time_zone_rule(offset, abbreviation)' of
    !> an offset (as from `current_time_zone' or `decode_time'), use the
    !> specified time zone without consideration for daylight saving time.
    !>
    !> Instead of calling this subroutine, you typically want something
    !> else.  To temporarily use a different time zone rule for just one
    !> invocation of `decode_time', `encode_time', or `format_time_string',
    !> pass the function a ZONE argument.  Port of
    !> timefns.c:Fset_time_zone_rule (and emacs_setenv_TZ).
    subroutine set_time_zone_rule(tz)
        character(len=*), intent(in), optional :: tz

        if (.not. present(tz)) then
            call sys_set_time_zone()
        else if (tz == TIME_ZONE_SYSTEM) then
            call sys_set_time_zone()
        else
            call sys_set_time_zone(tz)
        end if
    end subroutine set_time_zone_rule

    ! ------------------------------------------------------------------
    ! The calendar
    ! ------------------------------------------------------------------

    !> Decode a timestamp into its second, minute, hour, day, month, year,
    !> day of week, daylight saving flag and UTC offset.  The optional TIME
    !> is the time value to convert (absent: the current time).
    !>
    !> The optional ZONE is absent for Emacs local time, "UTC0" for
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
    !> the day of week, an integer between 0 and 6, where 0 is Sunday.  DST
    !> is DECODED_TIME_DST_YES if daylight saving time is in effect,
    !> DECODED_TIME_DST_NO if it is not in effect, and
    !> DECODED_TIME_DST_UNKNOWN if daylight saving information is not
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

        if (tm%dst < 0) then
            decoded%dst = DECODED_TIME_DST_UNKNOWN
        else if (tm%dst == 0) then
            decoded%dst = DECODED_TIME_DST_NO
        else
            decoded%dst = DECODED_TIME_DST_YES
        end if

        allocate (decoded%zone, source=int(tm%offset))
    end function decode_time

    !> Convert TIME to a timestamp.
    !>
    !> TIME is a decoded time in the style of `decode_time', so that
    !> encode_time(decode_time(...)) works.  Its ZONE is the UTC offset in
    !> seconds, applied without consideration for daylight saving time, or
    !> unallocated for Emacs local time; the optional ZONE argument, a
    !> zone as in `decode_time' (a TZ string, "UTC0" for Universal Time,
    !> TIME_ZONE_SYSTEM or a `time_zone_rule'), is used instead when
    !> present.  If the zone has daylight
    !> saving transitions, DST is DECODED_TIME_DST_YES for daylight saving
    !> time, DECODED_TIME_DST_NO for standard time, and
    !> DECODED_TIME_DST_UNKNOWN to cause the daylight saving flag to be
    !> guessed.  A decoded_time_type made with only SECOND through YEAR
    !> leaves DST unknown and the zone local.
    !>
    !> The range of supported years is at least 1970 to the near future.
    !> Out-of-range values for SECOND through MONTH are brought into range
    !> via date arithmetic.  This can be tricky especially when combined
    !> with DST; see Info node `(elisp)Time Conversion' for details and
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
            if (time%dst /= DECODED_TIME_DST_UNKNOWN) fields(9) = merge(1, 0, time%dst == DECODED_TIME_DST_YES)
        end if

        if (present(zone)) then
            call time_zone_begin(zone, saved)
        else if (allocated(time%zone)) then
            call time_zone_begin(time_zone_rule(time%zone), saved)
        else
            call time_zone_begin(saved=saved)
        end if

        call sys_mktime(fields, value, ok)
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

    !> Use FORMAT_STRING to format the time value TIME.  A time value that
    !> is absent stands for the current time.  This function treats
    !> seconds as time since the epoch of 1970-01-01 00:00:00 UTC.
    !>
    !> The optional ZONE is absent for Emacs local time, "UTC0" for
    !> Universal Time, TIME_ZONE_SYSTEM for system wall clock time, or a
    !> string as in the TZ environment variable.  It can also be
    !> `time_zone_rule(offset, abbreviation)' of an offset (as from
    !> `current_time_zone' or `decode_time') applied without consideration
    !> for daylight saving time.
    !>
    !> The value is a copy of FORMAT_STRING, but with certain constructs
    !> replaced by text that describes the specified date and time in TIME:
    !>
    !> %Y is the year, %y year without century, %C the century.
    !> %G is the year corresponding to the ISO week, %g year corresponding
    !>  to the ISO week, without century.
    !> %m is the numeric month.
    !> %b and %h are the locale's abbreviated month name, %B the full name.
    !> %d is the day of the month, zero-padded, %e is blank-padded.
    !> %u is the numeric day of week from 1 (Monday) to 7, %w from 0 (Sunday) to 6.
    !> %a is the locale's abbreviated name of the day of week, %A the full name.
    !> %U is the week number starting on Sunday, %W starting on Monday,
    !>  %V the week number according to ISO 8601.
    !> %j is the day of the year.
    !>
    !> %H is the hour on a 24-hour clock, %I is on a 12-hour clock, %k is like %H
    !>  only blank-padded, %l is like %I blank-padded.
    !> %p is the locale's equivalent of either AM or PM.
    !> %q is the calendar quarter (1-4).
    !> %M is the minute (00-59).
    !> %S is the second (00-59; 00-60 on platforms with leap seconds)
    !> %s is the number of seconds since 1970-01-01 00:00:00 +0000.
    !> %N is the nanosecond, %6N the microsecond, %3N the millisecond, etc.
    !> %Z is the time zone abbreviation, %z is the numeric form.
    !>
    !> %c is the locale's date and time format.
    !> %x is the locale's "preferred" date format.
    !> %D is like "%m/%d/%y".
    !> %F is the ISO 8601 date format (like "%+4Y-%m-%d").
    !>
    !> %R is like "%H:%M", %T is like "%H:%M:%S", %r is like "%I:%M:%S %p".
    !> %X is the locale's "preferred" time format.
    !>
    !> Finally, %n is a newline, %t is a tab, %% is a literal %, and
    !> unrecognized %-sequences stand for themselves.
    !>
    !> A %-sequence can contain optional flags, field width, and a modifier
    !> (in that order) after the `%'.  The flags are:
    !>
    !> `-' Do not pad the field.
    !> `_' Pad with spaces.
    !> `0' Pad with zeros.
    !> `+' Pad with zeros and put `+' before nonnegative year numbers with >4 digits.
    !> `^' Use upper case characters if possible.
    !> `#' Use opposite case characters if possible.
    !>
    !> A field width N is an unsigned decimal integer with a leading digit
    !> nonzero.  %NX is like %X, but takes up at least N positions.  The
    !> field width is measured in bytes, not characters.  It depends on the
    !> locale what the width (in characters) %NX will end up being,
    !> especially when there are non-ASCII characters in %X.
    !>
    !> The modifiers are:
    !>
    !> `E' Use the locale's alternative version.
    !> `O' Use the locale's number symbols.
    !>
    !> For example, to produce full ISO 8601 format, use "%FT%T%z".
    !> Port of timefns.c:Fformat_time_string (and format_time_string,
    !> emacs_nmemftime).
    function format_time_string(format_string, time, zone, status) result(text)
        character(len=*), intent(in) :: format_string
        type(time_type), intent(in), optional :: time
        character(len=*), intent(in), optional :: zone
        type(status_type), intent(out), optional :: status
        character(len=:), allocatable :: text
        type(time_type) :: t
        type(broken_down_time_type) :: tm
        logical :: ok

        if (present(status)) status = status_type()
        text = ""

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

        text = nstrftime(format_string, tm, .false., ZERO_PAD, -1)
    end function format_time_string

    ! FORMAT with its %-sequences replaced from TM, upper case if UPCASE,
    ! the first sequence FIRST_WIDTH wide (-1 for its own width) and
    ! years padded by YEAR_PAD where the sequence says nothing (the
    ! subformat of %F).  Port of strftime.c:__strftime_internal (the C
    ! locale; a NUL byte is copied like any other).
    recursive function nstrftime(format, tm, upcase, year_pad, first_width) result(out)
        character(len=*), intent(in) :: format
        type(broken_down_time_type), intent(in) :: tm
        logical, intent(in) :: upcase
        integer, intent(in) :: year_pad, first_width
        character(len=:), allocatable :: out
        character(len=:), allocatable :: piece
        integer :: f, percent, pad, width, modifier, colons, hour12, subwidth, diff, hour_diff, minute_diff, &
                   second_diff, year, year_adjust, days, d, two_digit_year, digit_count, nanosecond_digits, n, j
        logical :: to_lower_case, to_upper_case, change_case, negative, done
        character :: format_char

        out = ""
        hour12 = tm%hour

        if (hour12 > 12) then
            hour12 = hour12 - 12
        else if (hour12 == 0) then
            hour12 = 12
        end if

        width = first_width
        f = 1

        do while (f <= len(format))
            if (format(f:f) /= "%") then
                out = out//format(f:f)
                width = -1
                f = f + 1
                cycle
            end if

            percent = f
            pad = ZERO_PAD
            to_lower_case = .false.
            to_upper_case = upcase
            change_case = .false.

            ! Check for flags that can modify a format.
            do
                f = f + 1
                if (f > len(format)) exit

                select case (format(f:f))
                case ("_")
                    pad = SPACE_PAD
                case ("-")
                    pad = NO_PAD
                case ("+")
                    pad = SIGN_PAD
                case ("0")
                    pad = ALWAYS_ZERO_PAD
                case ("^")
                    to_upper_case = .true.
                case ("#")
                    change_case = .true.
                case default
                    exit
                end select
            end do

            if (f <= len(format)) then
                if (verify(format(f:f), "0123456789") == 0) then
                    width = 0

                    do while (f <= len(format))
                        if (verify(format(f:f), "0123456789") /= 0) exit
                        if (width < 100000000) width = 10*width + (iachar(format(f:f)) - iachar("0"))
                        f = f + 1
                    end do
                end if
            end if

            ! Check for modifiers.
            modifier = 0

            if (f <= len(format)) then
                if (format(f:f) == "E" .or. format(f:f) == "O") then
                    modifier = iachar(format(f:f))
                    f = f + 1
                end if
            end if

            ! % at the end of the format (or before a NUL) stands for itself.
            format_char = achar(0)
            if (f <= len(format)) format_char = format(f:f)
            done = .true.

            select case (format_char)
            case ("%")
                if (f - 1 /= percent) then
                    f = f - 1
                    call bad_format()
                else
                    out = out//"%"
                end if
            case ("a", "A")
                if (modifier /= 0) then
                    call bad_format()
                else
                    if (change_case) call upper_case()

                    if (format_char == "a") then
                        call add_cased(WEEKDAY_NAMES(tm%weekday) (1:3))
                    else
                        call add_cased(trim(WEEKDAY_NAMES(tm%weekday)))
                    end if
                end if
            case ("b", "h", "B")
                if (change_case) call upper_case()

                if (modifier == iachar("E")) then
                    call bad_format()
                else if (format_char == "B") then
                    call add_cased(trim(MONTH_NAMES(tm%month)))
                else
                    call add_cased(MONTH_NAMES(tm%month) (1:3))
                end if
            case ("c")
                if (modifier == iachar("O")) then
                    call bad_format()
                else
                    call do_subformat("%a %b %e %H:%M:%S %Y", -1)
                end if
            case ("C")
                ! The century: (tm_year - 99 * zero_thru_1899) / 100 + 19.
                negative = tm%year < -1900
                n = tm%year
                if (.not. negative .and. tm%year < 0) n = n - 99
                call do_yearish(2, negative, n/100 + 19)
            case ("x")
                if (modifier == iachar("O")) then
                    call bad_format()
                else
                    call do_subformat("%m/%d/%y", -1)
                end if
            case ("D")
                if (modifier /= 0) then
                    call bad_format()
                else
                    call do_subformat("%m/%d/%y", -1)
                end if
            case ("d")
                call do_number_unless_e(2, tm%day)
            case ("e")
                if (modifier == iachar("E")) then
                    call bad_format()
                else
                    call do_number_spacepad(2, tm%day)
                end if
            case ("F")
                if (modifier /= 0) then
                    call bad_format()
                else
                    if (pad == ZERO_PAD .and. width < 0) then
                        pad = SIGN_PAD
                        subwidth = 4
                    else
                        subwidth = max(width - 6, 0)
                    end if

                    call do_subformat("%Y-%m-%d", subwidth)
                end if
            case ("H")
                call do_number_unless_e(2, tm%hour)
            case ("I")
                call do_number_unless_e(2, hour12)
            case ("k")
                if (modifier == iachar("E")) then
                    call bad_format()
                else
                    call do_number_spacepad(2, tm%hour)
                end if
            case ("l")
                if (modifier == iachar("E")) then
                    call bad_format()
                else
                    call do_number_spacepad(2, hour12)
                end if
            case ("j")
                if (modifier == iachar("E")) then
                    call bad_format()
                else
                    call do_signed_number(3, tm%year_day < -1, int(tm%year_day + 1, int64))
                end if
            case ("M")
                call do_number_unless_e(2, tm%minute)
            case ("m")
                if (modifier == iachar("E")) then
                    call bad_format()
                else
                    call do_signed_number(2, tm%month < -1, int(tm%month + 1, int64))
                end if
            case ("N")
                if (modifier == iachar("E")) then
                    call bad_format()
                else
                    n = tm%nanoseconds
                    if (width <= 0) width = 9
                    nanosecond_digits = 9

                    do while (width < nanosecond_digits .or. (1 < nanosecond_digits .and. mod(n, 10) == 0))
                        nanosecond_digits = nanosecond_digits - 1
                        n = n/10
                    end do

                    piece = repeat("0", nanosecond_digits)

                    do j = nanosecond_digits, 1, -1
                        piece(j:j) = achar(mod(n, 10) + iachar("0"))
                        n = n/10
                    end do

                    if (pad == ZERO_PAD) pad = ALWAYS_ZERO_PAD
                    out = out//piece
                    width = width - nanosecond_digits
                    call add("")
                end if
            case ("n")
                out = out//new_line("a")
            case ("P", "p")
                if (format_char == "P") to_lower_case = .true.

                if (change_case) then
                    to_upper_case = .false.
                    to_lower_case = .true.
                end if

                call add_cased(merge("PM", "AM", 12 <= tm%hour))
            case ("q")
                call do_signed_number(1, .false., int(shiftr(tm%month*11, 5) + 1, int64))
            case ("R")
                call do_subformat("%H:%M", -1)
            case ("r")
                call do_subformat("%I:%M:%S %p", -1)
            case ("S")
                call do_number_unless_e(2, tm%second)
            case ("s")
                ! (strftime.c regenerates the count with mktime_z; it is
                ! the time that was broken down.)
                call number_sign_and_padding(1, tm%seconds < 0, abs(tm%seconds), .false., 0, .true.)
            case ("X")
                if (modifier == iachar("O")) then
                    call bad_format()
                else
                    call do_subformat("%H:%M:%S", -1)
                end if
            case ("T")
                call do_subformat("%H:%M:%S", -1)
            case ("t")
                out = out//achar(9)
            case ("u")
                call do_number(1, mod(tm%weekday - 1 + 7, 7) + 1)
            case ("U")
                call do_number_unless_e(2, (tm%year_day - tm%weekday + 7)/7)
            case ("V", "g", "G")
                if (modifier == iachar("E")) then
                    call bad_format()
                else
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

                    select case (format_char)
                    case ("g")
                        two_digit_year = mod(mod(tm%year, 100) + year_adjust, 100)

                        if (two_digit_year < 0) then
                            if (tm%year < -1900 - year_adjust) then
                                two_digit_year = -two_digit_year
                            else
                                two_digit_year = two_digit_year + 100
                            end if
                        end if

                        call do_yearish(2, .false., two_digit_year)
                    case ("G")
                        call do_yearish(4, tm%year < -1900 - year_adjust, tm%year + 1900 + year_adjust)
                    case default
                        call do_number(2, days/7 + 1)
                    end select
                end if
            case ("W")
                call do_number_unless_e(2, (tm%year_day - mod(tm%weekday - 1 + 7, 7) + 7)/7)
            case ("w")
                call do_number_unless_e(1, tm%weekday)
            case ("Y")
                ! (%EY: the C locale has no era.)
                call do_yearish(4, tm%year < -1900, tm%year + 1900)
            case ("y")
                two_digit_year = mod(tm%year, 100)

                if (two_digit_year < 0) then
                    if (tm%year < -1900) then
                        two_digit_year = -two_digit_year
                    else
                        two_digit_year = two_digit_year + 100
                    end if
                end if

                call do_yearish(2, .false., two_digit_year)
            case ("Z")
                if (change_case) then
                    to_upper_case = .false.
                    to_lower_case = .true.
                end if

                call add_cased(tm%abbreviation)
            case (":", "z")
                ! :, ::, and ::: are valid only just before 'z'.
                colons = 0

                if (format_char == ":") then
                    do while (f + colons <= len(format))
                        if (format(f + colons:f + colons) /= ":") exit
                        colons = colons + 1
                    end do

                    if (f + colons > len(format)) then
                        call bad_format()
                        done = .false.
                    else if (format(f + colons:f + colons) /= "z") then
                        call bad_format()
                        done = .false.
                    else
                        f = f + colons
                    end if
                end if

                if (done .and. tm%dst >= 0) then
                    diff = int(tm%offset)
                    negative = diff < 0

                    if (diff == 0 .and. len(tm%abbreviation) > 0) then
                        negative = tm%abbreviation(1:1) == "-"
                    end if

                    hour_diff = abs(diff)/60/60
                    minute_diff = mod(abs(diff)/60, 60)
                    second_diff = mod(abs(diff), 60)

                    select case (colons)
                    case (0)
                        ! +hhmm
                        call do_zone_offset(5, 0, hour_diff*100 + minute_diff)
                    case (1)
                        ! +hh:mm
                        call do_zone_offset(6, int(o'04'), hour_diff*100 + minute_diff)
                    case (2)
                        ! +hh:mm:ss
                        call do_zone_offset(9, int(o'024'), hour_diff*10000 + minute_diff*100 + second_diff)
                    case (3)
                        ! +hh if possible, else +hh:mm, else +hh:mm:ss
                        if (second_diff /= 0) then
                            call do_zone_offset(9, int(o'024'), hour_diff*10000 + minute_diff*100 + second_diff)
                        else if (minute_diff /= 0) then
                            call do_zone_offset(6, int(o'04'), hour_diff*100 + minute_diff)
                        else
                            call do_zone_offset(3, 0, hour_diff)
                        end if
                    case default
                        call bad_format()
                    end select
                end if
            case (achar(0))
                ! % at the end of the format: it stands for itself.
                f = f - 1
                call bad_format()
            case default
                call bad_format()
            end select

            width = -1
            f = f + 1
        end do

    contains

        ! Upper case for the names (the `#' flag).
        subroutine upper_case()
            to_upper_case = .true.
            to_lower_case = .false.
        end subroutine upper_case

        ! An unknown format: output it, including the `%'.
        subroutine bad_format()
            call add_cased(format(percent:min(f, len(format))))
        end subroutine bad_format

        ! TEXT, padded to WIDTH (strftime.c add).
        subroutine add(text)
            character(len=*), intent(in) :: text
            integer :: w

            w = width
            if (pad == NO_PAD .or. width < 0) w = 0

            if (len(text) < w) then
                if (pad == ALWAYS_ZERO_PAD .or. pad == SIGN_PAD) then
                    out = out//repeat("0", w - len(text))
                else
                    out = out//repeat(" ", w - len(text))
                end if
            end if

            out = out//text
        end subroutine add

        ! TEXT in the case asked for, padded to WIDTH (strftime.c cpy).
        subroutine add_cased(text)
            character(len=*), intent(in) :: text
            character(len=len(text)) :: cased
            integer :: i, c

            cased = text

            do i = 1, len(text)
                c = iachar(text(i:i))

                if (to_lower_case .and. c >= iachar("A") .and. c <= iachar("Z")) then
                    cased(i:i) = achar(c + 32)
                else if (to_upper_case .and. c >= iachar("a") .and. c <= iachar("z")) then
                    cased(i:i) = achar(c - 32)
                end if
            end do

            call add(cased)
        end subroutine add_cased

        ! SUBFMT formatted with this sequence's case and padding, its
        ! first sequence SUB_WIDTH wide (strftime.c subformat).
        recursive subroutine do_subformat(sub_format, sub_width)
            character(len=*), intent(in) :: sub_format
            integer, intent(in) :: sub_width

            call add(nstrftime(sub_format, tm, to_upper_case, pad, sub_width))
        end subroutine do_subformat

        ! DO_NUMBER, except that the E modifier makes a bad format.
        subroutine do_number_unless_e(digits_wanted, value)
            integer, intent(in) :: digits_wanted, value

            if (modifier == iachar("E")) then
                call bad_format()
            else
                call do_number(digits_wanted, value)
            end if
        end subroutine do_number_unless_e

        ! strftime.c DO_NUMBER.
        subroutine do_number(digits_wanted, value)
            integer, intent(in) :: digits_wanted, value

            call number_sign_and_padding(digits_wanted, value < 0, abs(int(value, int64)), .false., 0, .false.)
        end subroutine do_number

        ! strftime.c DO_NUMBER_SPACEPAD.
        subroutine do_number_spacepad(digits_wanted, value)
            integer, intent(in) :: digits_wanted, value

            if (pad == ZERO_PAD) pad = SPACE_PAD
            call do_number(digits_wanted, value)
        end subroutine do_number_spacepad

        ! strftime.c DO_SIGNED_NUMBER.
        subroutine do_signed_number(digits_wanted, is_negative, value)
            integer, intent(in) :: digits_wanted
            logical, intent(in) :: is_negative
            integer(int64), intent(in) :: value

            call number_sign_and_padding(digits_wanted, is_negative, abs(value), .false., 0, .false.)
        end subroutine do_signed_number

        ! strftime.c DO_YEARISH.
        subroutine do_yearish(digits_wanted, is_negative, value)
            integer, intent(in) :: digits_wanted, value
            logical, intent(in) :: is_negative
            logical :: always_sign

            if (pad == ZERO_PAD) pad = year_pad
            always_sign = pad == SIGN_PAD .and. (merge(99, 9999, digits_wanted == 2) < abs(value) .or. &
                                                 digits_wanted < width)
            call number_sign_and_padding(digits_wanted, is_negative, abs(int(value, int64)), always_sign, 0, .false.)
        end subroutine do_yearish

        ! strftime.c DO_TZ_OFFSET.
        subroutine do_zone_offset(digits_wanted, mask, value)
            integer, intent(in) :: digits_wanted, mask, value

            call number_sign_and_padding(digits_wanted, negative, int(value, int64), .true., mask, .false.)
        end subroutine do_zone_offset

        ! MAGNITUDE's digits, with a colon before the i'th digit where
        ! the i'th bit of COLON_MASK is set, then the sign and padding
        ! (strftime.c do_number_body and do_number_sign_and_padding; with
        ! RAW, only the latter).
        subroutine number_sign_and_padding(digits_wanted, is_negative, magnitude, always_sign, colon_mask, raw)
            integer, intent(in) :: digits_wanted, colon_mask
            logical, intent(in) :: is_negative, always_sign, raw
            integer(int64), intent(in) :: magnitude
            character(len=:), allocatable :: number
            integer(int64) :: u
            integer :: mask, shortage, padding
            character(len=1) :: sign_char

            number = ""
            u = magnitude
            mask = colon_mask
            if (raw) mask = 0

            do
                if (iand(mask, 1) /= 0) number = ":"//number
                mask = shiftr(mask, 1)
                number = achar(int(mod(u, 10_int64)) + iachar("0"))//number
                u = u/10
                if (u == 0 .and. mask == 0) exit
            end do

            if (pad == ZERO_PAD) pad = ALWAYS_ZERO_PAD
            if (width < 0) width = digits_wanted
            sign_char = " "
            if (always_sign) sign_char = "+"
            if (is_negative) sign_char = "-"
            digit_count = len(number)
            shortage = width - merge(1, 0, sign_char /= " ") - digit_count
            padding = 0
            if (pad /= NO_PAD .and. shortage > 0) padding = shortage

            if (sign_char /= " ") then
                if (pad == SPACE_PAD) then
                    out = out//repeat(" ", padding)
                    width = width - padding
                end if

                out = out//sign_char
                width = width - 1
            end if

            call add_cased(number)
        end subroutine number_sign_and_padding
    end function nstrftime

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
    !> `format_time_string' which provide a much more powerful and general
    !> facility.
    !>
    !> If TIME is given, it is the time value to format instead of the
    !> current time.
    !>
    !> The optional ZONE is absent for Emacs local time, "UTC0" for
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
    !> The optional ZONE is absent for Emacs local time, "UTC0" for
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
        integer :: offset, hour, min_sec, minute, second
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
            min_sec = mod(abs(offset), 3600)
            minute = min_sec/60
            second = mod(min_sec, 60)
            write (buffer, "(i2.2)") hour
            found%name = merge("-", "+", offset < 0)//trim(buffer)

            if (min_sec /= 0) then
                write (buffer, "(i2.2)") minute
                found%name = found%name//trim(buffer)

                if (second /= 0) then
                    write (buffer, "(i2.2)") second
                    found%name = found%name//trim(buffer)
                end if
            end if
        end if
    end function current_time_zone
end module timefns_mod
