! test_timefns_mod.f90 -- tests for timefns_mod.
! Ports GNU Emacs 31.1 test/src/timefns-tests.el in part; the integer and
! list ZONE forms of format-time-string and decode-time wait for their
! Fortran form, and values beyond int64 (bignums, floats such as 1e37, and
! rates whose least common multiple exceeds huge(0_int64)) have none.
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

module test_timefns_mod
    use, intrinsic :: iso_fortran_env, only: int64, real64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_mod, only: status_type
    use timefns_mod, only: time_type, decoded_time_type, time_zone_type, TIMESPEC_COUNT_RATE, DECODED_TIME_DST_NO, &
                           DECODED_TIME_DST_YES, current_time, current_cpu_time, time_add, time_subtract, is_time_less, &
                           is_time_equal, real, time_convert, decode_time, encode_time, format_time_string, &
                           current_time_string, current_time_zone, set_time_zone_rule, time_zone_rule, TIME_ZONE_SYSTEM
    implicit none(type, external)
    private

    public :: collect_timefns_mod

    !> The largest whole seconds upstream's tests use (most-positive-fixnum
    !> on 64-bit Emacs).
    integer(int64), parameter :: LARGEST_SECONDS = 2_int64**61 - 1

contains

    !> Collect all exported unit tests.
    subroutine collect_timefns_mod(testsuite)
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
                    new_unittest("encode-time-dst-numeric-zone", test_dst_numeric_zone), &
                    new_unittest("encode-time-alternate-apis", test_alternate_apis), &
                    new_unittest("float-time-precision", test_real_time_precision), &
                    new_unittest("time-convert-forms", test_convert_forms), &
                    new_unittest("current-time-zone", test_current_time_zone), &
                    new_unittest("current-time-string", test_current_time_string), &
                    new_unittest("format-time-string-directives", test_directives), &
                    new_unittest("set-time-zone-rule", test_set_time_zone_rule), &
                    new_unittest("time-zone-rule", test_time_zone_rule), &
                    new_unittest("time-zone-system", test_time_zone_system), &
                    new_unittest("current-time-and-cpu-time", test_clocks) &
                    ]
    end subroutine collect_timefns_mod

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
                   d%dst == expected%dst .and. d%zone == expected%zone, "decoded "//zone)
    end subroutine check_decode

    ! Check format-time-string and decode-time with various TZ settings.
    ! "look" is the timestamp just before the first leap second on
    ! 1972-06-30 23:59:60 UTC.
    subroutine test_format_with_zone(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: format = "%Y-%m-%d %H:%M:%S.%3N %z (%Z)"
        type(time_type) :: looks(2), look, sec
        type(decoded_time_type) :: expected
        integer :: k

        ! (1202 22527 999999 999999), (7879679999900 . 100000) and
        ! (78796799999999999999 . 1000000000000).
        looks(1) = time_type(1202_int64*65536 + 22527, 999999999999_int64, 1000000000000_int64)
        looks(2) = time_type(78796799_int64, 99900_int64, 100000_int64)

        do k = 1, size(looks)
            look = looks(k)
            sec = time_add(time_convert(time_type(59), look%count_rate), &
                           time_subtract(look, time_convert(time_type(look%seconds), look%count_rate)))
            ! UTC.
            call check(error, format_time_string("%Y-%m-%d %H:%M:%S.%3N %z", look, "UTC0") == &
                       "1972-06-30 23:59:59.999 +0000", "UTC")
            if (allocated(error)) return
            expected = decoded_time_type(sec, 59, 23, 30, 6, 1972_int64, 5, DECODED_TIME_DST_NO, 0)
            call check_decode(error, look, "UTC0", expected)
            if (allocated(error)) return
            call check(error, format_time_string(format, look, "UTC0") == "1972-06-30 23:59:59.999 +0000 (UTC)", "UTC0")
            if (allocated(error)) return
            ! Positive UTC offset that is not an hour multiple, as a string.
            call check(error, format_time_string(format, look, "IST-5:30") == "1972-07-01 05:29:59.999 +0530 (IST)", &
                       "IST-5:30")
            if (allocated(error)) return
            expected = decoded_time_type(sec, 29, 5, 1, 7, 1972_int64, 6, DECODED_TIME_DST_NO, 19800)
            call check_decode(error, look, "IST-5:30", expected)
            if (allocated(error)) return
            ! Negative UTC offset, as an offset and an abbreviation.
            call check(error, format_time_string(format, look, time_zone_rule(-28800, "PST")) == &
                       "1972-06-30 15:59:59.999 -0800 (PST)", "(-28800 PST)")
            if (allocated(error)) return
            expected = decoded_time_type(sec, 59, 15, 30, 6, 1972_int64, 5, DECODED_TIME_DST_NO, -28800)
            call check_decode(error, look, time_zone_rule(-28800, "PST"), expected)
            if (allocated(error)) return
            ! Negative UTC offset, as an integer.
            call check(error, format_time_string(format, look, time_zone_rule(-28800)) == &
                       "1972-06-30 15:59:59.999 -0800 (-08)", "-28800")
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

        text = format_time_string("%Y-%m-%d %H:%M:%S.%3N %z", zone=repeat("X", 2048)//"0")
        call check(error, len(text) > 0)
    end subroutine test_outlandish_zone

    subroutine test_year_2038(error)
        type(error_type), allocatable, intent(out) :: error
        logical :: leap_seconds

        leap_seconds = format_time_string("%Y-%m-%d %H:%M:%S", time_type(78796800), "UTC0") == "1972-06-30 23:59:60"
        call check(error, format_time_string("%Y-%m-%d %H:%M:%S", time_type(2_int64**31 - 3600), "UTC0") == &
                   "2038-01-19 02:14:08" .or. leap_seconds)
    end subroutine test_year_2038

    ! A time made by encode-time from the parts given, in UTC.
    type(time_type) function utc_time(second, day, month)
        type(time_type), intent(in) :: second
        integer, intent(in) :: day, month

        utc_time = encode_time(decoded_time_type(second=second, day=day, month=month, year=2000), "UTC0")
    end function utc_time

    subroutine test_padding_minimal(error)
        type(error_type), allocatable, intent(out) :: error
        type(time_type) :: ref_time

        ref_time = utc_time(time_type(0, 123450, 1000000), 15, 2)
        call check(error, format_time_string("%-:::z", ref_time, "FJT-12") == "+12", "%-:::z")
        if (allocated(error)) return
        call check(error, format_time_string("%-N", ref_time, "UTC0") == "12345", "%-N")
        if (allocated(error)) return
        call check(error, format_time_string("%-6N", ref_time, "UTC0") == "12345", "%-6N")
        if (allocated(error)) return
        call check(error, format_time_string("%-m", ref_time, "UTC0") == "2", "%-m")
    end subroutine test_padding_minimal

    subroutine test_padding_retains(error)
        type(error_type), allocatable, intent(out) :: error
        type(time_type) :: ref_time

        ref_time = utc_time(time_type(0, 3450, 1000000), 20, 10)
        call check(error, format_time_string("%-z", ref_time, "IST-5:30") == "+530", "%-z")
        if (allocated(error)) return
        call check(error, format_time_string("%-4z", ref_time, "IST-5:30") == "+530", "%-4z")
        if (allocated(error)) return
        call check(error, format_time_string("%4z", ref_time, "IST-5:30") == "+530", "%4z")
        if (allocated(error)) return
        call check(error, format_time_string("%-N", ref_time, "UTC0") == "00345", "%-N")
        if (allocated(error)) return
        call check(error, format_time_string("%-3N", ref_time, "UTC0") == "003", "%-3N")
        if (allocated(error)) return
        call check(error, format_time_string("%3N", ref_time, "UTC0") == "003", "%3N")
        if (allocated(error)) return
        call check(error, format_time_string("%-m", ref_time, "UTC0") == "10", "%-m")
        if (allocated(error)) return
        call check(error, format_time_string("%-1m", ref_time, "UTC0") == "10", "%-1m")
        if (allocated(error)) return
        call check(error, format_time_string("%1m", ref_time, "UTC0") == "10", "%1m")
    end subroutine test_padding_retains

    subroutine test_padding_spaces(error)
        type(error_type), allocatable, intent(out) :: error
        type(time_type) :: ref_time

        ref_time = utc_time(time_type(0, 123000, 1000000), 10, 12)
        call check(error, format_time_string("%_7z", ref_time, "CHA-12:45") == "  +1245", "%_7z")
        if (allocated(error)) return
        call check(error, format_time_string("%_6N", ref_time, "UTC0") == "123   ", "%_6N")
        if (allocated(error)) return
        call check(error, format_time_string("%_9N", ref_time, "UTC0") == "123      ", "%_9N")
        if (allocated(error)) return
        call check(error, format_time_string("%_12N", ref_time, "UTC0") == "123         ", "%_12N")
        if (allocated(error)) return
        call check(error, format_time_string("%_m", ref_time, "UTC0") == "12", "%_m")
        if (allocated(error)) return
        call check(error, format_time_string("%_2m", ref_time, "UTC0") == "12", "%_2m")
        if (allocated(error)) return
        call check(error, format_time_string("%_3m", ref_time, "UTC0") == " 12", "%_3m")
    end subroutine test_padding_spaces

    ! Fractional seconds have a fixed place on the left, and any padding
    ! must happen on the right.  All other numbers have a fixed place on
    ! the right and are padded on the left.
    subroutine test_padding_zeros(error)
        type(error_type), allocatable, intent(out) :: error
        type(time_type) :: ref_time

        ref_time = utc_time(time_type(0, 123000, 1000000), 10, 12)
        call check(error, format_time_string("%3m", ref_time, "UTC0") == "012", "%3m")
        if (allocated(error)) return
        call check(error, format_time_string("%7z", ref_time, "CHA-12:45") == "+001245", "%7z")
        if (allocated(error)) return
        call check(error, format_time_string("%12N", ref_time, "UTC0") == "123000000000", "%12N")
        if (allocated(error)) return
        call check(error, format_time_string("%9N", ref_time, "UTC0") == "123000000", "%9N")
        if (allocated(error)) return
        call check(error, format_time_string("%6N", ref_time, "UTC0") == "123000", "%6N")
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
    subroutine test_dst_numeric_zone(error)
        type(error_type), allocatable, intent(out) :: error
        type(time_type) :: t
        type(status_type) :: st

        t = encode_time(decoded_time_type(time_type(29), 31, 17, 30, 4, 2019_int64, 2, DECODED_TIME_DST_YES, 7200), &
                        status=st)
        call check(error, st%ok() .and. is_time_equal(t, time_type(23752_int64*65536 + 27217)))
    end subroutine test_dst_numeric_zone

    ! A time with only SECOND through YEAR, and one with an unknown DST and
    ! Emacs local time spelled out.
    subroutine test_alternate_apis(error)
        type(error_type), allocatable, intent(out) :: error
        type(decoded_time_type) :: time, time_1

        time = decoded_time_type(second=time_type(30), minute=30, hour=12, day=15, month=6, year=1970)
        time_1 = time
        time_1%dst = -1
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

    ! Own test: the other directives, the flags and bad sequences.
    subroutine test_directives(error)
        type(error_type), allocatable, intent(out) :: error
        type(time_type) :: t

        ! 2000-12-10 (a Sunday) 13:04:05 UTC.
        t = encode_time(decoded_time_type(second=time_type(5), minute=4, hour=13, day=10, month=12, year=2000), "UTC0")
        call check(error, format_time_string("%c|%x|%X|%D|%F|%R|%T|%r", t, "UTC0") == &
                   "Sun Dec 10 13:04:05 2000|12/10/00|13:04:05|12/10/00|2000-12-10|13:04|13:04:05|01:04:05 PM", "formats")
        if (allocated(error)) return
        call check(error, format_time_string("%a %A %b %h %B %^a %#A %p %P %#p %Z %#Z", t, "UTC0") == &
                   "Sun Sunday Dec Dec December SUN SUNDAY PM pm pm UTC utc", "names")
        if (allocated(error)) return
        call check(error, format_time_string("%C %y %G %g %V %U %W %j %u %w %q %e %k %l %I %s", t, "UTC0") == &
                   "20 00 2000 00 49 50 49 345 7 0 4 10 13  1 01 976453445", "numbers")
        if (allocated(error)) return
        call check(error, format_time_string("%%|%n|%t|%-%|%Q|%Ey|%:z|%::z|%:::z|%::::z|%", t, "UTC0") == &
                   "%|"//new_line("a")//"|"//achar(9)//"|%-%|%Q|00|+00:00|+00:00:00|+00|%::::z|%", "others")
        if (allocated(error)) return
        call check(error, format_time_string("%10F|%+6Y|%_5d|%05e", t, "UTC0") == &
                   "2000-12-10|+02000|   10|00010", "widths")
    end subroutine test_directives

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
        call check(error, format_time_string("%H:%M %Z", time_type(0)) == "05:30 IST", "IST")

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
        call check(error, current_time_zone(time_type(0), time_zone_rule(-28800, "PST"))%name == "PST", &
                   "current-time-zone")
    end subroutine test_time_zone_rule

    ! Own test: TIME_ZONE_SYSTEM is the zone with TZ unset, for one call
    ! only.
    subroutine test_time_zone_system(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: format = "%Y-%m-%d %H:%M:%S %z"
        character(len=:), allocatable :: saved, system, local
        integer :: length, stat
        logical :: had_rule

        call get_environment_variable("TZ", length=length, status=stat)
        had_rule = stat == 0
        allocate (character(len=max(length, 0)) :: saved)
        if (had_rule .and. length > 0) call get_environment_variable("TZ", saved)
        call set_time_zone_rule()
        system = format_time_string(format, time_type(0))
        call set_time_zone_rule("IST-5:30")
        local = format_time_string(format, time_type(0), TIME_ZONE_SYSTEM)
        call check(error, local == system, "wall")

        if (.not. allocated(error)) then
            call check(error, format_time_string(format, time_type(0)) == "1970-01-01 05:30:00 +0530", "restored")
        end if

        if (.not. allocated(error)) then
            call set_time_zone_rule(TIME_ZONE_SYSTEM)
            call check(error, format_time_string(format, time_type(0)) == system, "set-time-zone-rule wall")
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
end module test_timefns_mod
