! test_time_functions_mod.f90 -- tests for time_functions_mod.
! Ports GNU Emacs 31.1 test/src/timefns-tests.el in part: the strftime
! strings of format-time-string are date formats giving the same text
! (%-N, %-6N and %-:::z, whose digits depend on the value, have none), and
! values beyond int64 (bignums, floats such as 1e37, and rates whose least
! common multiple exceeds huge(0_int64)) have none.
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

module test_time_functions_mod
    use, intrinsic :: iso_fortran_env, only: int64, real64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_mod, only: status_type, format_error
    use time_functions_mod, only: time_type, decoded_time_type, time_zone_type, TIMESPEC_COUNT_RATE, &
                                  DECODED_TIME_DAYLIGHT_SAVING_TIME_NO, DECODED_TIME_DAYLIGHT_SAVING_TIME_YES, current_time, &
                                  current_cpu_time, time_add, time_subtract, is_time_less, is_time_equal, real, time_convert, &
                                  decode_time, encode_time, format_date, date_format_type, MAIL_DATE, current_time_string, &
                                  current_time_zone, set_time_zone_rule, time_zone_rule, TIME_ZONE_SYSTEM, DATE_NONE, DATE_YEAR, &
                                  DATE_CENTURY, DATE_YEAR_OF_CENTURY, DATE_ISO_WEEK_YEAR, DATE_ISO_WEEK_YEAR_OF_CENTURY, &
                                  DATE_ISO_WEEK, DATE_QUARTER, DATE_MONTH, DATE_WEEK_STARTING_SUNDAY, DATE_WEEK_STARTING_MONDAY, &
                                  DATE_DAY, DATE_DAY_OF_YEAR, DATE_WEEKDAY, DATE_ISO_WEEKDAY, TIME_HOUR, &
                                  TIME_HOUR_OF_TWELVE_HOUR_CLOCK, &
                                  TIME_MINUTE, TIME_SECOND, TIME_MILLISECOND, TIME_MICROSECOND, TIME_NANOSECOND, &
                                  TIME_SECONDS_SINCE_EPOCH, ZONE_OFFSET, ZONE_OFFSET_HOURS, ZONE_OFFSET_MINUTES, &
                                  ZONE_OFFSET_SECONDS, DATE_WEEKDAY_NAME, DATE_WEEKDAY_ABBREVIATION, &
                                  DATE_WEEKDAY_NAME_UPPER_CASE, DATE_WEEKDAY_ABBREVIATION_UPPER_CASE, DATE_MONTH_NAME, &
                                  DATE_MONTH_ABBREVIATION, DATE_MONTH_NAME_UPPER_CASE, DATE_MONTH_ABBREVIATION_UPPER_CASE, &
                                  TIME_MERIDIEM, TIME_MERIDIEM_LOWER_CASE, ZONE_ABBREVIATION, &
                                  ZONE_ABBREVIATION_LOWER_CASE, ZONE_OFFSET_SIGN
    implicit none(type, external)
    private

    public :: collect_time_functions_mod

    !> The largest whole seconds upstream's tests use (most-positive-fixnum
    !> on 64-bit Emacs).
    integer(int64), parameter :: LARGEST_SECONDS = 2_int64**61 - 1

contains

    !> Collect all exported unit tests.
    subroutine collect_time_functions_mod(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("format-time-string-with-zone", test_format_with_zone), &
                    new_unittest("decode-then-encode-time", test_decode_then_encode), &
                    new_unittest("format-time-string-with-outlandish-zone", test_outlandish_zone), &
                    new_unittest("format-time-string-with-bignum-on-32-bit", test_year_2038), &
                    new_unittest("format-time-string-padding-minimal-deletes-unneeded-zeros", test_padding_minimal), &
                    new_unittest("format-time-string-padding-minimal-retains-needed-zeros", test_padding_retains), &
                    new_unittest("format-time-string-padding-spaces", test_padding_spaces), &
                    new_unittest("format-time-string-padding-zeros-adds-on-insignificant-side", test_padding_zeros), &
                    new_unittest("time-equal-p-nil-nil", test_equal_same), &
                    new_unittest("time-arith-tests", test_arith), &
                    new_unittest("time-rounding-tests", test_rounding), &
                    new_unittest("encode-time-dst-numeric-zone", test_daylight_saving_time_numeric_zone), &
                    new_unittest("encode-time-alternate-apis", test_alternate_apis), &
                    new_unittest("float-time-precision", test_real_time_precision), &
                    new_unittest("time-convert-forms", test_convert_forms), &
                    new_unittest("current-time-zone", test_current_time_zone), &
                    new_unittest("current-time-string", test_current_time_string), &
                    new_unittest("format-date-parts", test_parts), &
                    new_unittest("format-date-formats", test_formats), &
                    new_unittest("format-date-errors", test_format_errors), &
                    new_unittest("format-date-callers", test_callers), &
                    new_unittest("set-time-zone-rule", test_set_time_zone_rule), &
                    new_unittest("time-zone-rule", test_time_zone_rule), &
                    new_unittest("time-zone-system", test_time_zone_system), &
                    new_unittest("current-time-and-cpu-time", test_clocks) &
                    ]
    end subroutine collect_time_functions_mod

    ! timefns-tests--decode-time: decoding LOOK in ZONE gives EXPECTED, and
    ! its integer second is EXPECTED's.
    subroutine check_decode(error, look, zone, expected)
        type(error_type), allocatable, intent(out) :: error
        type(time_type), intent(in) :: look
        character(len=*), intent(in) :: zone
        type(decoded_time_type), intent(in) :: expected
        type(decoded_time_type) :: d
        type(status_type) :: st

        d = decode_time(look, zone, st)
        call check(error, st%ok(), "decode-time "//zone)
        if (allocated(error)) return
        call check(error, is_time_equal(d%second, expected%second) .and. &
                   d%second%count_rate == expected%second%count_rate .and. d%second%seconds == expected%second%seconds &
                   .and. d%minute == expected%minute .and. d%hour == expected%hour .and. d%day == expected%day .and. &
                   d%month == expected%month .and. d%year == expected%year .and. d%weekday == expected%weekday .and. &
                   d%daylight_saving_time == expected%daylight_saving_time .and. d%zone == expected%zone, "decoded "//zone)
    end subroutine check_decode

    ! TIME written with DATE_FORMAT in ZONE by format_date, or "error: "
    ! and the error message when it fails.
    function date_text(date_format, time, zone) result(text)
        type(date_format_type), intent(in) :: date_format
        type(time_type), intent(in), optional :: time
        character(len=*), intent(in), optional :: zone
        character(len=:), allocatable :: text
        type(status_type) :: st

        text = format_date(date_format, time, zone, st)
        if (.not. st%ok()) text = "error: "//format_error(st)
    end function date_text

    ! Check format-time-string and decode-time with various TZ settings.
    ! "look" is the timestamp just before the first leap second on
    ! 1972-06-30 23:59:60 UTC.  (The strftime strings of upstream's test
    ! are date formats here, giving the same text.)
    subroutine test_format_with_zone(error)
        type(error_type), allocatable, intent(out) :: error
        type(date_format_type) :: with_zone, with_abbreviation
        type(time_type) :: looks(2), look, second
        type(decoded_time_type) :: expected
        integer :: k

        ! "%Y-%m-%d %H:%M:%S.%3N %z" and "%Y-%m-%d %H:%M:%S.%3N %z (%Z)".
        with_zone = date_format_type('(i4.4, "-", i2.2, "-", i2.2, 1x, i2.2, ":", i2.2, ":", i2.2, ".", i3.3, 1x, sp, i5.4)', &
                                     [DATE_YEAR, DATE_MONTH, DATE_DAY, TIME_HOUR, TIME_MINUTE, TIME_SECOND, TIME_MILLISECOND, &
                                      ZONE_OFFSET])
        with_abbreviation = date_format_type('(i4.4, "-", i2.2, "-", i2.2, 1x, i2.2, ":", i2.2, ":", i2.2, ".", i3.3, 1x, sp, ' &
                                             //'i5.4, " (", a, ")")', [DATE_YEAR, DATE_MONTH, DATE_DAY, TIME_HOUR, TIME_MINUTE, &
                                                                       TIME_SECOND, TIME_MILLISECOND, ZONE_OFFSET, &
                                                                       ZONE_ABBREVIATION])
        ! (1202 22527 999999 999999), (7879679999900 . 100000) and
        ! (78796799999999999999 . 1000000000000).
        looks(1) = time_type(1202_int64*65536 + 22527, 999999999999_int64, 1000000000000_int64)
        looks(2) = time_type(78796799_int64, 99900_int64, 100000_int64)

        do k = 1, size(looks)
            look = looks(k)
            second = time_add(time_convert(time_type(59), look%count_rate), &
                              time_subtract(look, time_convert(time_type(look%seconds), look%count_rate)))
            ! UTC.
            call check(error, date_text(with_zone, look, "UTC0"), "1972-06-30 23:59:59.999 +0000")
            if (allocated(error)) return
            expected = decoded_time_type(second, 59, 23, 30, 6, 1972_int64, 5, DECODED_TIME_DAYLIGHT_SAVING_TIME_NO, 0)
            call check_decode(error, look, "UTC0", expected)
            if (allocated(error)) return
            call check(error, date_text(with_abbreviation, look, "UTC0"), "1972-06-30 23:59:59.999 +0000 (UTC)")
            if (allocated(error)) return
            ! Positive UTC offset that is not an hour multiple, as a string.
            call check(error, date_text(with_abbreviation, look, "IST-5:30"), "1972-07-01 05:29:59.999 +0530 (IST)")
            if (allocated(error)) return
            expected = decoded_time_type(second, 29, 5, 1, 7, 1972_int64, 6, DECODED_TIME_DAYLIGHT_SAVING_TIME_NO, 19800)
            call check_decode(error, look, "IST-5:30", expected)
            if (allocated(error)) return
            ! Negative UTC offset, as an offset and an abbreviation.
            call check(error, date_text(with_abbreviation, look, time_zone_rule(-28800, "PST")), &
                       "1972-06-30 15:59:59.999 -0800 (PST)")
            if (allocated(error)) return
            expected = decoded_time_type(second, 59, 15, 30, 6, 1972_int64, 5, DECODED_TIME_DAYLIGHT_SAVING_TIME_NO, -28800)
            call check_decode(error, look, time_zone_rule(-28800, "PST"), expected)
            if (allocated(error)) return
            ! Negative UTC offset, as an integer.
            call check(error, date_text(with_abbreviation, look, time_zone_rule(-28800)), &
                       "1972-06-30 15:59:59.999 -0800 (-08)")
            if (allocated(error)) return
            call check_decode(error, look, time_zone_rule(-28800), expected)
            if (allocated(error)) return
        end do
    end subroutine test_format_with_zone

    subroutine test_decode_then_encode(error)
        type(error_type), allocatable, intent(out) :: error
        type(time_type) :: values(12), a, e
        type(decoded_time_type) :: d
        type(status_type) :: st
        integer :: k

        values = [time_type(0), time_type(-2), time_type(1), time_convert(0.0_real64), time_convert(-0.0_real64), &
                  time_convert(-2.0_real64), time_convert(1.0_real64), time_type(-LARGEST_SECONDS - 1), &
                  time_type(LARGEST_SECONDS), time_type(1), time_type(65536), time_type(-65536)]
        values(10) = time_type(123456789, 0, 1000000)

        do k = 1, size(values)
            a = values(k)
            d = decode_time(a, "UTC0", st)
            ! (An error condition, upstream's ignore-errors, skips the value.)
            if (.not. st%ok()) cycle
            e = encode_time(d, status=st)
            call check(error, st%ok() .and. is_time_equal(a, e), "decode-then-encode")
            if (allocated(error)) return
            ! The integer form.
            d%second = time_type(d%second%seconds)
            e = encode_time(d, status=st)
            call check(error, st%ok() .and. is_time_equal(time_type(a%seconds), e), "decode-then-encode integer")
            if (allocated(error)) return
        end do
    end subroutine test_decode_then_encode

    ! This should not dump core.
    subroutine test_outlandish_zone(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: text

        text = date_text(date_format_type('(i4.4, "-", i2.2, "-", i2.2, 1x, i2.2, ":", i2.2, ":", i2.2, ".", i3.3, 1x, sp, i5.4)', &
                                          [DATE_YEAR, DATE_MONTH, DATE_DAY, TIME_HOUR, TIME_MINUTE, TIME_SECOND, &
                                           TIME_MILLISECOND, ZONE_OFFSET]), zone=repeat("X", 2048)//"0")
        call check(error, len(text) > 0)
    end subroutine test_outlandish_zone

    subroutine test_year_2038(error)
        type(error_type), allocatable, intent(out) :: error
        type(date_format_type) :: date_and_time
        logical :: leap_seconds

        ! "%Y-%m-%d %H:%M:%S".
        date_and_time = date_format_type('(i4.4, "-", i2.2, "-", i2.2, 1x, i2.2, ":", i2.2, ":", i2.2)', &
                                         [DATE_YEAR, DATE_MONTH, DATE_DAY, TIME_HOUR, TIME_MINUTE, TIME_SECOND])
        leap_seconds = date_text(date_and_time, time_type(78796800), "UTC0") == "1972-06-30 23:59:60"
        call check(error, date_text(date_and_time, time_type(2_int64**31 - 3600), "UTC0") == "2038-01-19 02:14:08" &
                   .or. leap_seconds)
    end subroutine test_year_2038

    ! A time made by encode-time from the parts given, in UTC.
    type(time_type) function utc_time(second, day, month)
        type(time_type), intent(in) :: second
        integer, intent(in) :: day, month

        utc_time = encode_time(decoded_time_type(second=second, day=day, month=month, year=2000), "UTC0")
    end function utc_time

    ! The padding of upstream's padding tests is the width of the edit
    ! descriptor; the strftime sequences whose digits depend on the value
    ! (%-N, %-6N and %-:::z) have no date format.
    subroutine test_padding_minimal(error)
        type(error_type), allocatable, intent(out) :: error
        type(time_type) :: reference_time

        reference_time = utc_time(time_type(0, 123450, 1000000), 15, 2)
        ! %-m.
        call check(error, date_text(date_format_type("(i0)", [DATE_MONTH]), reference_time, "UTC0"), "2")
    end subroutine test_padding_minimal

    subroutine test_padding_retains(error)
        type(error_type), allocatable, intent(out) :: error
        type(time_type) :: reference_time

        reference_time = utc_time(time_type(0, 3450, 1000000), 20, 10)
        ! %-z, %-4z and %4z.
        call check(error, date_text(date_format_type("(sp, i0)", [ZONE_OFFSET]), reference_time, "IST-5:30"), "+530")
        if (allocated(error)) return
        call check(error, date_text(date_format_type("(sp, i4)", [ZONE_OFFSET]), reference_time, "IST-5:30"), "+530")
        if (allocated(error)) return
        ! %-3N and %3N.
        call check(error, date_text(date_format_type("(i3.3)", [TIME_MILLISECOND]), reference_time, "UTC0"), "003")
        if (allocated(error)) return
        ! %-m, %-1m and %1m.
        call check(error, date_text(date_format_type("(i0)", [DATE_MONTH]), reference_time, "UTC0"), "10")
    end subroutine test_padding_retains

    subroutine test_padding_spaces(error)
        type(error_type), allocatable, intent(out) :: error
        type(time_type) :: reference_time

        reference_time = utc_time(time_type(0, 123000, 1000000), 10, 12)
        ! %_7z.
        call check(error, date_text(date_format_type("(sp, i7)", [ZONE_OFFSET]), reference_time, "CHA-12:45"), "  +1245")
        if (allocated(error)) return
        ! %_6N, %_9N and %_12N: the blanks at the end are kept.
        call check(error, date_text(date_format_type("(i3.3, 3x)", [TIME_MILLISECOND]), reference_time, "UTC0"), "123   ")
        if (allocated(error)) return
        call check(error, date_text(date_format_type("(i3.3, 6x)", [TIME_MILLISECOND]), reference_time, "UTC0"), &
                   "123      ")
        if (allocated(error)) return
        call check(error, date_text(date_format_type('(i3.3, "         ")', [TIME_MILLISECOND]), reference_time, "UTC0"), &
                   "123         ")
        if (allocated(error)) return
        ! %_m, %_2m and %_3m.
        call check(error, date_text(date_format_type("(i2)", [DATE_MONTH]), reference_time, "UTC0"), "12")
        if (allocated(error)) return
        call check(error, date_text(date_format_type("(i3)", [DATE_MONTH]), reference_time, "UTC0"), " 12")
    end subroutine test_padding_spaces

    ! Fractional seconds have a fixed place on the left, and any padding
    ! must happen on the right.  All other numbers have a fixed place on
    ! the right and are padded on the left.
    subroutine test_padding_zeros(error)
        type(error_type), allocatable, intent(out) :: error
        type(time_type) :: reference_time

        reference_time = utc_time(time_type(0, 123000, 1000000), 10, 12)
        ! %3m.
        call check(error, date_text(date_format_type("(i3.3)", [DATE_MONTH]), reference_time, "UTC0"), "012")
        if (allocated(error)) return
        ! %7z.
        call check(error, date_text(date_format_type("(sp, i7.6)", [ZONE_OFFSET]), reference_time, "CHA-12:45"), &
                   "+001245")
        if (allocated(error)) return
        ! %12N, %9N and %6N.
        call check(error, date_text(date_format_type('(i9.9, "000")', [TIME_NANOSECOND]), reference_time, "UTC0"), &
                   "123000000000")
        if (allocated(error)) return
        call check(error, date_text(date_format_type("(i9.9)", [TIME_NANOSECOND]), reference_time, "UTC0"), "123000000")
        if (allocated(error)) return
        call check(error, date_text(date_format_type("(i6.6)", [TIME_MICROSECOND]), reference_time, "UTC0"), "123000")
    end subroutine test_padding_zeros

    ! (time-equal-p nil nil): the current time equals itself.
    subroutine test_equal_same(error)
        type(error_type), allocatable, intent(out) :: error
        type(time_type) :: now

        now = current_time()
        call check(error, is_time_equal(now, now))
    end subroutine test_equal_same

    subroutine test_arith(error)
        type(error_type), allocatable, intent(out) :: error
        type(time_type), allocatable :: time_values(:)
        type(time_type) :: a, b, aa, sum
        real(real64) :: x, y
        integer :: i, j, n

        ! The values of upstream's list that int64 holds exactly: 0 -1 1
        ! 0.0 -0.0 -1.0 1.0, most-negative-fixnum and most-positive-fixnum, 1e1 -1e1 1e8 -1e8 1e9 -1e9
        ! 1e10 -1e10 1e16 -1e16, (0 0 0 1) (0 0 1 0) (0 1 0 0) (1 0 0 0)
        ! (-1 0 0 0) (1 2 3 4) (-1 2 3 4), (-123456789 . 100000) and
        ! (123456789 . 1000000).
        allocate (time_values(30))
        time_values(1) = time_type(0)
        time_values(2) = time_type(-1)
        time_values(3) = time_type(1)
        time_values(4) = time_convert(0.0_real64)
        time_values(5) = time_convert(-0.0_real64)
        time_values(6) = time_convert(-1.0_real64)
        time_values(7) = time_convert(1.0_real64)
        time_values(8) = time_type(-LARGEST_SECONDS - 1)
        time_values(9) = time_type(LARGEST_SECONDS)
        time_values(10) = time_convert(1e1_real64)
        time_values(11) = time_convert(-1e1_real64)
        time_values(12) = time_convert(1e8_real64)
        time_values(13) = time_convert(-1e8_real64)
        time_values(14) = time_convert(1e9_real64)
        time_values(15) = time_convert(-1e9_real64)
        time_values(16) = time_convert(1e10_real64)
        time_values(17) = time_convert(-1e10_real64)
        time_values(18) = time_convert(1e16_real64)
        time_values(19) = time_convert(-1e16_real64)
        time_values(20) = time_type(0, 1, 1000000000000_int64)
        time_values(21) = time_type(0, 1, 1000000)
        time_values(22) = time_type(1)
        time_values(23) = time_type(65536)
        time_values(24) = time_type(-65536)
        time_values(25) = time_type(65536 + 2, 3000004, 1000000000000_int64)
        time_values(26) = time_type(-65536 + 2, 3000004, 1000000000000_int64)
        time_values(27) = time_type(-1235, 43211, 100000)
        time_values(28) = time_type(123, 456789, 1000000)
        time_values(29) = time_convert(0.5_real64)
        time_values(30) = time_convert(-2.0_real64)
        n = size(time_values)

        do i = 1, n
            a = time_values(i)

            do j = 1, n
                b = time_values(j)
                ! (LARGEST_SECONDS + LARGEST_SECONDS fits.)
                aa = time_subtract(time_add(a, b), b)
                call check(error, is_time_equal(a, aa), "a + b - b")
                if (allocated(error)) return
                call check(error, count([is_time_less(a, b), is_time_equal(a, b), is_time_less(b, a)]) == 1, &
                           "trichotomy")
                if (allocated(error)) return
                sum = time_add(a, b)
                call check(error,.not. is_time_less(time_type(0), b) .or. is_time_less(a, sum) .or. &
                           is_time_equal(a, sum), "monotonic")
                if (allocated(error)) return
                x = real(sum)
                y = real(a) + real(b)
                call check(error, x == y .or. (0.99_real64 < x/y .and. x/y < 1.01_real64) .or. &
                           (0.99_real64 < -real(a)/real(b) .and. -real(a)/real(b) < 1.01_real64), &
                           "float-time of a sum")
                if (allocated(error)) return
            end do
        end do
    end subroutine test_arith

    subroutine test_rounding(error)
        type(error_type), allocatable, intent(out) :: error
        type(time_type) :: tiny_time

        tiny_time = time_convert(1e-13_real64)
        call check(error, is_time_equal(tiny_time, time_add(time_type(0), tiny_time)))
    end subroutine test_rounding

    ! Check for Bug#35502.
    subroutine test_daylight_saving_time_numeric_zone(error)
        type(error_type), allocatable, intent(out) :: error
        type(time_type) :: t
        type(status_type) :: st

        t = encode_time(decoded_time_type(time_type(29), 31, 17, 30, 4, 2019_int64, 2, DECODED_TIME_DAYLIGHT_SAVING_TIME_YES, &
                                          7200), &
                        status=st)
        call check(error, st%ok() .and. is_time_equal(t, time_type(23752_int64*65536 + 27217)))
    end subroutine test_daylight_saving_time_numeric_zone

    ! A time with only SECOND through YEAR, and one with an unknown DAYLIGHT_SAVING_TIME and
    ! Emacs local time spelled out.
    subroutine test_alternate_apis(error)
        type(error_type), allocatable, intent(out) :: error
        type(decoded_time_type) :: time, time_1

        time = decoded_time_type(second=time_type(30), minute=30, hour=12, day=15, month=6, year=1970)
        time_1 = time
        time_1%daylight_saving_time = -1
        call check(error, is_time_equal(encode_time(time), encode_time(time_1)))
    end subroutine test_alternate_apis

    subroutine test_real_time_precision(error)
        type(error_type), allocatable, intent(out) :: error
        real(real64), parameter :: multipliers(8) = [-1.9_real64, -1.5_real64, -1.1_real64, -1.0_real64, 1.0_real64, &
                                                     1.1_real64, 1.5_real64, 1.9_real64]
        real(real64) :: x, xmult
        integer :: k

        call check(error, real(time_type(1, 4025, 1000000000000_int64)) == 1.000000004025_real64, "(0 1 0 4025)")
        if (allocated(error)) return
        call check(error, real(time_type(0, 1, 10000000000_int64)) > 0, "(1 . 10000000000)")
        if (allocated(error)) return
        call check(error, real(time_type(-1, 10000000000_int64 - 1, 10000000000_int64)) < 0, "(-1 . 10000000000)")
        if (allocated(error)) return
        ! Down to the reals whose binary digits after the point fit in a
        ! count rate of 2**62.
        x = 1.0_real64

        do while (x >= 2.0_real64**(-9))
            do k = 1, size(multipliers)
                xmult = x*multipliers(k)
                call check(error, xmult == real(time_convert(xmult)), "halving")
                if (allocated(error)) return
            end do

            x = x/2
        end do

        ! Up to the reals whose seconds fit in int64.
        x = 1.0_real64

        do while (x < 2.0_real64**62)
            do k = 1, size(multipliers)
                xmult = x/multipliers(k)
                call check(error, xmult == real(time_convert(xmult)), "doubling")
                if (allocated(error)) return
            end do

            x = x*2
        end do
    end subroutine test_real_time_precision

    ! These computations involve numbers that should have exact
    ! representations on any Emacs platform.
    subroutine test_convert_forms(error)
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: times(5) = [-86400, -1, 0, 1, 86400]
        real(real64), parameter :: deltas(6) = [0.0_real64, 0.0_real64, 0.25_real64, 3.25_real64, 1000.0_real64, &
                                                1000.25_real64]
        ! FORM t (the rate kept), `list' (picoseconds), 4, 1000, 1000000
        ! and 1000000000.
        integer(int64), parameter :: rates(5) = [1000000000000_int64, 4_int64, 1000_int64, 1000000_int64, &
                                                 1000000000_int64]
        type(time_type) :: time, time_plus, time_minus
        integer :: i, j, k

        do i = 1, size(times)
            do j = 1, size(deltas)
                if (j == 1) then
                    time = time_type(times(i))
                    time_plus = time
                    time_minus = time
                else
                    time = time_convert(real(times(i), real64))
                    time_plus = time_convert(times(i) + deltas(j))
                    time_minus = time_convert(times(i) - deltas(j))
                end if

                call check(error, is_time_equal(time, time_convert(time)) .and. &
                           is_time_equal(time_plus, time_convert(time_plus)), "FORM t")
                if (allocated(error)) return

                do k = 1, size(rates)
                    call check(error, is_time_equal(time, time_convert(time, rates(k))) .and. &
                               is_time_equal(time_minus, time_convert(time_minus, rates(k))) .and. &
                               is_time_equal(time_plus, time_convert(time_plus, rates(k))), "FORM a rate")
                    if (allocated(error)) return
                end do
            end do
        end do
    end subroutine test_convert_forms

    subroutine test_current_time_zone(error)
        type(error_type), allocatable, intent(out) :: error
        type(time_zone_type) :: zone

        zone = current_time_zone()
        call check(error, allocated(zone%name), "name")
        if (allocated(error)) return
        zone = current_time_zone(time_type(0), "UTC0")
        call check(error, zone%offset == 0 .and. zone%name == "UTC", "UTC0")
        if (allocated(error)) return
        zone = current_time_zone(time_type(0), "IST-5:30")
        call check(error, zone%offset == 19800 .and. zone%name == "IST", "IST-5:30")
    end subroutine test_current_time_zone

    ! Own test: the ctime format.
    subroutine test_current_time_string(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, current_time_string(time_type(0), "UTC0") == "Thu Jan  1 00:00:00 1970", "epoch")
        if (allocated(error)) return
        call check(error, current_time_string(time_type(116989432), "UTC0") == "Sun Sep 16 01:03:52 1973", "1973")
        if (allocated(error)) return
        call check(error, len(current_time_string()) >= 24, "now")
    end subroutine test_current_time_string

    ! Own test: every part, with the text format-time-string gave for the
    ! strftime sequences named in the comments.
    subroutine test_parts(error)
        type(error_type), allocatable, intent(out) :: error
        type(time_type) :: t

        ! 2000-12-10 (a Sunday) 13:04:05 UTC.
        t = encode_time(decoded_time_type(second=time_type(5), minute=4, hour=13, day=10, month=12, year=2000), "UTC0")
        ! "%c|%x|%X|%D".
        call check(error, date_text(date_format_type('(a, 1x, a, 1x, i2, 1x, i2.2, ":", i2.2, ":", i2.2, 1x, i4.4, "|", ' &
                                                     //'i2.2, "/", i2.2, "/", i2.2, "|", i2.2, ":", i2.2, ":", i2.2, "|", ' &
                                                     //'i2.2, "/", i2.2, "/", i2.2)', &
                                                     [DATE_WEEKDAY_ABBREVIATION, DATE_MONTH_ABBREVIATION, DATE_DAY, TIME_HOUR, &
                                                      TIME_MINUTE, TIME_SECOND, DATE_YEAR, DATE_MONTH, DATE_DAY, &
                                                      DATE_YEAR_OF_CENTURY, TIME_HOUR, TIME_MINUTE, TIME_SECOND, DATE_MONTH, &
                                                      DATE_DAY, DATE_YEAR_OF_CENTURY]), t, "UTC0"), &
                   "Sun Dec 10 13:04:05 2000|12/10/00|13:04:05|12/10/00", "formats")
        if (allocated(error)) return
        ! "%F|%R|%T|%r".
        call check(error, date_text(date_format_type('(i4.4, "-", i2.2, "-", i2.2, "|", i2.2, ":", i2.2, "|", 2(i2.2, ":"), ' &
                                                     //'i2.2, "|", 2(i2.2, ":"), i2.2, 1x, a)', &
                                                     [DATE_YEAR, DATE_MONTH, DATE_DAY, TIME_HOUR, TIME_MINUTE, TIME_HOUR, &
                                                      TIME_MINUTE, TIME_SECOND, TIME_HOUR_OF_TWELVE_HOUR_CLOCK, TIME_MINUTE, &
                                                      TIME_SECOND, &
                                                      TIME_MERIDIEM]), t, "UTC0"), &
                   "2000-12-10|13:04|13:04:05|01:04:05 PM", "more formats")
        if (allocated(error)) return
        ! "%a %A %b %h %B %^a %^A %p %P %#p %Z %#Z".
        call check(error, date_text(date_format_type("(a, 11(1x, a))", &
                                                     [DATE_WEEKDAY_ABBREVIATION, DATE_WEEKDAY_NAME, DATE_MONTH_ABBREVIATION, &
                                                      DATE_MONTH_ABBREVIATION, DATE_MONTH_NAME, &
                                                      DATE_WEEKDAY_ABBREVIATION_UPPER_CASE, DATE_WEEKDAY_NAME_UPPER_CASE, &
                                                      TIME_MERIDIEM, TIME_MERIDIEM_LOWER_CASE, TIME_MERIDIEM_LOWER_CASE, &
                                                      ZONE_ABBREVIATION, ZONE_ABBREVIATION_LOWER_CASE]), t, "UTC0"), &
                   "Sun Sunday Dec Dec December SUN SUNDAY PM pm pm UTC utc", "names")
        if (allocated(error)) return
        ! "%^b %^B".
        call check(error, date_text(date_format_type("(a, 1x, a)", [DATE_MONTH_ABBREVIATION_UPPER_CASE, &
                                                                    DATE_MONTH_NAME_UPPER_CASE]), t, "UTC0"), &
                   "DEC DECEMBER", "upper case month")
        if (allocated(error)) return
        ! "%C %y %G %g %V %U %W %j %u %w %q %e %k %l %I %s".
        call check(error, date_text(date_format_type("(i2.2, 1x, i2.2, 1x, i4.4, 4(1x, i2.2), 1x, i3.3, 3(1x, i1), " &
                                                     //"3(1x, i2), 1x, i2.2, 1x, i0)", &
                                                     [DATE_CENTURY, DATE_YEAR_OF_CENTURY, DATE_ISO_WEEK_YEAR, &
                                                      DATE_ISO_WEEK_YEAR_OF_CENTURY, DATE_ISO_WEEK, DATE_WEEK_STARTING_SUNDAY, &
                                                      DATE_WEEK_STARTING_MONDAY, DATE_DAY_OF_YEAR, DATE_ISO_WEEKDAY, DATE_WEEKDAY, &
                                                      DATE_QUARTER, DATE_DAY, TIME_HOUR, TIME_HOUR_OF_TWELVE_HOUR_CLOCK, &
                                                      TIME_HOUR_OF_TWELVE_HOUR_CLOCK, &
                                                      TIME_SECONDS_SINCE_EPOCH]), t, "UTC0"), &
                   "20 00 2000 00 49 50 49 345 7 0 4 10 13  1 01 976453445", "numbers")
        if (allocated(error)) return
        ! "%t|%:z|%::z".
        call check(error, date_text(date_format_type('("'//achar(9)//'|", a, i2.2, ":", i2.2, "|", a, 2(i2.2, ":"), i2.2)', &
                                                     [ZONE_OFFSET_SIGN, ZONE_OFFSET_HOURS, ZONE_OFFSET_MINUTES, &
                                                      ZONE_OFFSET_SIGN, ZONE_OFFSET_HOURS, ZONE_OFFSET_MINUTES, &
                                                      ZONE_OFFSET_SECONDS]), t, "UTC0"), &
                   achar(9)//"|+00:00|+00:00:00", "others")
        if (allocated(error)) return
        ! "%10F|%+6Y|%_5d|%05e".
        call check(error, date_text(date_format_type('(i4.4, "-", i2.2, "-", i2.2, "|", sp, i6.5, "|", ss, i5, "|", i5.5)', &
                                                     [DATE_YEAR, DATE_MONTH, DATE_DAY, DATE_YEAR, DATE_DAY, DATE_DAY]), t, &
                                    "UTC0"), &
                   "2000-12-10|+02000|   10|00010", "widths")
        if (allocated(error)) return
        ! "%:z" west of Greenwich, by half an hour.
        call check(error, date_text(date_format_type('(a, i2.2, ":", i2.2)', [ZONE_OFFSET_SIGN, ZONE_OFFSET_HOURS, &
                                                                              ZONE_OFFSET_MINUTES]), t, &
                                    time_zone_rule(-1800)), "-00:30", "west")
    end subroutine test_parts

    ! Own test: the Fortran format of a date format: groups, upper case,
    ! character strings, positions, a format without parts.
    subroutine test_formats(error)
        type(error_type), allocatable, intent(out) :: error
        type(time_type) :: t

        t = encode_time(decoded_time_type(second=time_type(5), minute=4, hour=13, day=10, month=12, year=2000), "UTC0")
        call check(error, date_text(date_format_type('  (2(I2.2, ":"), I2.2)  ', [TIME_HOUR, TIME_MINUTE, TIME_SECOND]), t, &
                                    "UTC0"), "13:04:05", "group")
        if (allocated(error)) return
        call check(error, date_text(date_format_type("(t5, i2.2, t1, a)", [TIME_HOUR, DATE_WEEKDAY_ABBREVIATION]), t, "UTC0"), &
                   "Sun 13", "positions")
        if (allocated(error)) return
        call check(error, date_text(date_format_type("('it''s ', a5, 1x, g0, tr2, i0, 1x)", &
                                                     [DATE_MONTH_ABBREVIATION, DATE_MONTH_NAME, DATE_DAY]), t, "UTC0"), &
                   "it's   Dec December  10 ", "strings")
        if (allocated(error)) return
        call check(error, date_text(date_format_type("(b0, 1x, o0, 1x, z0)", [DATE_DAY, DATE_DAY, DATE_DAY]), t, "UTC0"), &
                   "1010 12 A", "bases")
        if (allocated(error)) return
        call check(error, date_text(date_format_type('("now")', [integer ::]), t), "now", "no parts")
        if (allocated(error)) return
        call check(error, date_text(date_format_type("()", [integer ::]), t), "", "empty")
        if (allocated(error)) return
        call check(error, date_text(date_format_type(), t), "", "default")
    end subroutine test_formats

    ! Own test: the errors of a date format.
    subroutine test_format_errors(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: FORMATS(12) = [character(len=24) :: "i2", "(i2", "(i2))", "('x)", "(*(i2))", &
                                                              "(2hab, i2)", "(f5.2)", "(a)", "(i2, i2)", "(dt)", "(i2, q)", "(0i2)"]
        integer :: k

        do k = 1, size(FORMATS)
            call check(error, index(date_text(date_format_type(trim(FORMATS(k)), [TIME_HOUR])), "error: ") == 1, &
                       trim(FORMATS(k)))
            if (allocated(error)) return
        end do

        call check(error, index(date_text(date_format_type("(i2)", [DATE_WEEKDAY_NAME])), "error: ") == 1, "name")
        if (allocated(error)) return
        call check(error, index(date_text(date_format_type("(i2)", [99])), "error: ") == 1, "not a part")
        if (allocated(error)) return
        call check(error, index(date_text(date_format_type("(17i2)", [(TIME_HOUR, k=1, 17)])), "error: ") == 1, "17 parts")
        if (allocated(error)) return
        call check(error, index(date_text(date_format_type("(i2)", [DATE_NONE, TIME_HOUR])), "error: ") == 1, "a gap")
        if (allocated(error)) return
        call check(error, index(date_text(date_format_type("(i2, /, i2)", [TIME_HOUR, TIME_MINUTE])), "error: ") == 1, &
                   "a new record")
    end subroutine test_format_errors

    ! Own test: the date formats of the callers of format_date give the
    ! text format-time-string gave for their strftime strings.
    subroutine test_callers(error)
        type(error_type), allocatable, intent(out) :: error
        type(time_type) :: t

        ! 2000-02-05 03:04:05 UTC, a Saturday; 08:34:05 in IST-5:30.
        t = time_type(949719845)
        ! add_log_iso8601_time_string, "%Y-%m-%d".
        call check(error, date_text(date_format_type('(i4.4, "-", i2.2, "-", i2.2)', [DATE_YEAR, DATE_MONTH, DATE_DAY]), t, &
                                    "IST-5:30"), "2000-02-05", "add-log")
        if (allocated(error)) return
        ! list_generations, "%b %d %Y %H:%M:%S".
        call check(error, date_text(date_format_type('(a, 1x, i2.2, 1x, i4.4, 1x, i2.2, ":", i2.2, ":", i2.2)', &
                                                     [DATE_MONTH_ABBREVIATION, DATE_DAY, DATE_YEAR, TIME_HOUR, TIME_MINUTE, &
                                                      TIME_SECOND]), t, "IST-5:30"), "Feb 05 2000 08:34:05", "generations")
        if (allocated(error)) return
        ! Dired's touch default, "%Y%m%d%H%M.%S".
        call check(error, date_text(date_format_type('(i4.4, 4i2.2, ".", i2.2)', [DATE_YEAR, DATE_MONTH, DATE_DAY, TIME_HOUR, &
                                                                                  TIME_MINUTE, TIME_SECOND]), t, "IST-5:30"), &
                   "200002050834.05", "touch")
        if (allocated(error)) return
        ! mail_rfc822_date, "%-d %b %Y %T %z".
        call check(error, date_text(date_format_type('(i0, 1x, a, 1x, i4.4, 1x, i2.2, ":", i2.2, ":", i2.2, 1x, sp, i5.4)', &
                                                     [DATE_DAY, DATE_MONTH_ABBREVIATION, DATE_YEAR, TIME_HOUR, TIME_MINUTE, &
                                                      TIME_SECOND, ZONE_OFFSET]), t, "IST-5:30"), &
                   "5 Feb 2000 08:34:05 +0530", "rfc822")
        if (allocated(error)) return
        ! MAIL_DATE, "%a, %d %b %Y %T %z".
        call check(error, date_text(MAIL_DATE, t, "IST-5:30"), "Sat, 05 Feb 2000 08:34:05 +0530", "mail date")
        if (allocated(error)) return
        call check(error, date_text(MAIL_DATE, t, time_zone_rule(-28800, "PST")), "Fri, 04 Feb 2000 19:04:05 -0800", &
                   "mail date west")
        if (allocated(error)) return
        ! The From line of an Fcc, "%a %b %e %T %z %Y".
        call check(error, date_text(date_format_type( &
                                    '(a, 1x, a, 1x, i2, 1x, i2.2, ":", i2.2, ":", i2.2, 1x, sp, i5.4, 1x, ss, i4.4)', &
                                    [DATE_WEEKDAY_ABBREVIATION, DATE_MONTH_ABBREVIATION, DATE_DAY, TIME_HOUR, TIME_MINUTE, &
                                     TIME_SECOND, ZONE_OFFSET, DATE_YEAR]), t, "IST-5:30"), &
                   "Sat Feb  5 08:34:05 +0530 2000", "from line")
        if (allocated(error)) return
        ! The draft file of Message, "-%Y%m%d-%H%M%S".
        call check(error, date_text(date_format_type('("-", i4.4, 2i2.2, "-", 3i2.2)', [DATE_YEAR, DATE_MONTH, DATE_DAY, &
                                                                                        TIME_HOUR, TIME_MINUTE, TIME_SECOND]), &
                                    t, "IST-5:30"), "-20000205-083405", "draft")
        if (allocated(error)) return
        ! The queued mail of smtpmail, "%Y-%m-%d_%H:%M:%S".
        call check(error, date_text(date_format_type('(i4.4, "-", i2.2, "-", i2.2, "_", i2.2, ":", i2.2, ":", i2.2)', &
                                                     [DATE_YEAR, DATE_MONTH, DATE_DAY, TIME_HOUR, TIME_MINUTE, TIME_SECOND]), &
                                    t, "IST-5:30"), "2000-02-05_08:34:05", "queue")
    end subroutine test_callers

    ! Own test: the local zone follows set_time_zone_rule.
    subroutine test_set_time_zone_rule(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: saved
        integer :: length, stat
        logical :: had_rule

        call get_environment_variable("TZ", length=length, status=stat)
        had_rule = stat == 0
        allocate (character(len=max(length, 0)) :: saved)
        if (had_rule .and. length > 0) call get_environment_variable("TZ", saved)
        call set_time_zone_rule("IST-5:30")
        call check(error, date_text(date_format_type('(i2.2, ":", i2.2, 1x, a)', [TIME_HOUR, TIME_MINUTE, &
                                                                                  ZONE_ABBREVIATION]), time_type(0)), &
                   "05:30 IST", "IST")

        if (had_rule) then
            call set_time_zone_rule(saved)
        else
            call set_time_zone_rule()
        end if
    end subroutine test_set_time_zone_rule

    ! Own test: the TZ rules tzlookup builds for an integer zone and for
    ! (OFFSET ABBR).
    subroutine test_time_zone_rule(error)
        type(error_type), allocatable, intent(out) :: error
        type(time_zone_type) :: zone

        call check(error, time_zone_rule(0) == "UTC0", "0")
        if (allocated(error)) return
        call check(error, time_zone_rule(-28800) == "<-08>8:00:00", "-28800")
        if (allocated(error)) return
        call check(error, time_zone_rule(-28800, "PST") == "<PST>8:00:00", "(-28800 PST)")
        if (allocated(error)) return
        call check(error, time_zone_rule(19800) == "<+0530>-5:30:00", "19800")
        if (allocated(error)) return
        call check(error, time_zone_rule(3723) == "<+010203>-1:02:03", "3723")
        if (allocated(error)) return
        call check(error, time_zone_rule(0, "GMT") == "<GMT>-0:00:00", "(0 GMT)")
        if (allocated(error)) return
        call check(error, current_time_string(time_type(0), time_zone_rule(19800)) == "Thu Jan  1 05:30:00 1970", &
                   "current-time-string")
        if (allocated(error)) return
        zone = current_time_zone(time_type(0), time_zone_rule(-28800, "PST"))
        call check(error, zone%name == "PST", "current-time-zone")
    end subroutine test_time_zone_rule

    ! Own test: TIME_ZONE_SYSTEM is the zone with TZ unset, for one call
    ! only.
    subroutine test_time_zone_system(error)
        type(error_type), allocatable, intent(out) :: error
        type(date_format_type) :: format
        character(len=:), allocatable :: saved, system, local
        integer :: length, stat
        logical :: had_rule

        call get_environment_variable("TZ", length=length, status=stat)
        had_rule = stat == 0
        allocate (character(len=max(length, 0)) :: saved)
        ! "%Y-%m-%d %H:%M:%S %z".
        format = date_format_type('(i4.4, "-", i2.2, "-", i2.2, 1x, i2.2, ":", i2.2, ":", i2.2, 1x, sp, i5.4)', &
                                  [DATE_YEAR, DATE_MONTH, DATE_DAY, TIME_HOUR, TIME_MINUTE, TIME_SECOND, ZONE_OFFSET])
        if (had_rule .and. length > 0) call get_environment_variable("TZ", saved)
        call set_time_zone_rule()
        system = date_text(format, time_type(0))
        call set_time_zone_rule("IST-5:30")
        local = date_text(format, time_type(0), TIME_ZONE_SYSTEM)
        call check(error, local == system, "wall")

        if (.not. allocated(error)) then
            call check(error, date_text(format, time_type(0)) == "1970-01-01 05:30:00 +0530", "restored")
        end if

        if (.not. allocated(error)) then
            call set_time_zone_rule(TIME_ZONE_SYSTEM)
            call check(error, date_text(format, time_type(0)) == system, "set-time-zone-rule wall")
        end if

        if (had_rule) then
            call set_time_zone_rule(saved)
        else
            call set_time_zone_rule()
        end if
    end subroutine test_time_zone_system

    ! Own test: the clocks count forward.
    subroutine test_clocks(error)
        type(error_type), allocatable, intent(out) :: error
        type(time_type) :: a, b

        a = current_time()
        b = current_time()
        call check(error, a%count_rate == TIMESPEC_COUNT_RATE .and. .not. is_time_less(b, a), "current-time")
        if (allocated(error)) return
        a = current_cpu_time()
        call check(error, a%count_rate == 1000000 .and. a%seconds >= 0, "current-cpu-time")
    end subroutine test_clocks
end module test_time_functions_mod
