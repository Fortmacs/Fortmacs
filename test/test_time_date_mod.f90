! test_time_date_mod.f90 -- tests for time_date_mod.
! Ports GNU Emacs 31.1 test/lisp/calendar/time-date-tests.el in part;
! test-obsolete-with-decoded-time-value and test-obsolete-encode-time-value
! test upstream's list forms of a time value, which are not applicable.
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

!> The time-date-tests.el tests but the two of obsolete list forms; own
!> tests of the readable forms of `seconds_to_string', of unknown
!> components in `add_decoded_time' and of the errors of `format_seconds'.
module test_time_date_mod
    use, intrinsic :: iso_fortran_env, only: int64, real64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_mod, only: status_type, format_error
    use time_functions_mod, only: time_type, current_time, time_subtract, is_time_equal, DECODED_TIME_DST_YES, &
                                  DECODED_TIME_DST_NO, DECODED_TIME_DST_UNKNOWN
    use time_date_mod, only: seconds_to_string, SECONDS_TO_STRING_READABLE, SECONDS_TO_STRING_EXPANDED, &
                             partial_decoded_time_type, date_to_time, days_between, is_date_leap_year, days_to_time, &
                             date_days_in_month, format_seconds, date_ordinal_to_time, add_decoded_time, &
                             decoded_time_period, time_since, encode_partial_decoded_time
    use test_time_support_mod, only: render_partial_decoded_time
    implicit none(type, external)
    private

    public :: collect_time_date_mod

contains

    !> Collect all exported unit tests.
    subroutine collect_time_date_mod(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("test-date-to-time", test_date_to_time), &
                    new_unittest("test-days-between", test_days_between), &
                    new_unittest("test-leap-year", test_leap_year), &
                    new_unittest("test-days-to-time", test_days_to_time), &
                    new_unittest("test-seconds-to-string", test_seconds_to_string), &
                    new_unittest("test-days-in-month", test_days_in_month), &
                    new_unittest("test-format-seconds", test_format_seconds), &
                    new_unittest("test-ordinal", test_ordinal), &
                    new_unittest("test-decoded-add", test_decoded_add), &
                    new_unittest("test-decoded-add-zone", test_decoded_add_zone), &
                    new_unittest("test-time-since", test_time_since), &
                    new_unittest("test-time-decoded-period", test_time_decoded_period), &
                    new_unittest("test-time-wrap-addition", test_time_wrap_addition), &
                    new_unittest("seconds-to-string-readable", test_readable), &
                    new_unittest("decoded-time-add-unknown", test_add_unknown), &
                    new_unittest("format-seconds-errors", test_format_seconds_errors) &
                    ]
    end subroutine collect_time_date_mod

    subroutine expect(error, actual, expected)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), intent(in) :: actual, expected

        call check(error, actual == expected, '"'//actual//'" expected "'//expected//'"')
    end subroutine expect

    ! Check that date_to_time of DATE is the time EXPECTED encodes to.
    subroutine expect_date(error, date, expected)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), intent(in) :: date
        type(partial_decoded_time_type), intent(in) :: expected
        type(status_type) :: st, encoded_status
        type(time_type) :: time, encoded

        time = date_to_time(date, st)
        encoded = encode_partial_decoded_time(expected, encoded_status)
        call check(error, st%ok() .and. encoded_status%ok() .and. is_time_equal(time, encoded), date)
    end subroutine expect_date

    ! Check that DECODED is EXPECTED, as test_time_support_mod writes it.
    subroutine expect_decoded(error, decoded, expected)
        type(error_type), allocatable, intent(out) :: error
        type(partial_decoded_time_type), intent(in) :: decoded
        character(len=*), intent(in) :: expected

        call expect(error, render_partial_decoded_time(decoded), expected)
    end subroutine expect_decoded

    ! Check that add_decoded_time of TIME and DELTA is EXPECTED.
    subroutine expect_add(error, time, delta, expected)
        type(error_type), allocatable, intent(out) :: error
        type(partial_decoded_time_type), intent(in) :: time, delta
        character(len=*), intent(in) :: expected
        type(status_type) :: st
        type(partial_decoded_time_type) :: sum

        sum = add_decoded_time(time, delta, st)

        if (.not. st%ok()) then
            call check(error, .false., format_error(st))
            return
        end if

        call expect_decoded(error, sum, expected)
    end subroutine expect_add

    ! Check that format_seconds of FORMAT and SECONDS is EXPECTED, and of
    ! -SECONDS "-" and EXPECTED when SECONDS is positive.
    subroutine expect_format(error, format, seconds, expected)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), intent(in) :: format, expected
        real(real64), intent(in) :: seconds
        type(status_type) :: st

        call expect(error, format_seconds(format, seconds, st), expected)
        if (allocated(error)) return
        if (seconds > 0) call expect(error, format_seconds(format, -seconds, st), "-"//expected)
    end subroutine expect_format

    subroutine test_date_to_time(error)
        type(error_type), allocatable, intent(out) :: error

        call expect_date(error, "2021-12-04", &
                         partial_decoded_time_type(second=time_type(0), minute=0, hour=0, day=4, month=12, &
                                                   year=2021_int64, dst=DECODED_TIME_DST_UNKNOWN))
        if (allocated(error)) return
        call expect_date(error, "2006-05-04T03:02:01Z", &
                         partial_decoded_time_type(second=time_type(1), minute=2, hour=3, day=4, month=5, &
                                                   year=2006_int64, dst=DECODED_TIME_DST_NO, zone=0))
        if (allocated(error)) return
        call expect_date(error, "14 Apr 89 03:20", &
                         partial_decoded_time_type(second=time_type(0), minute=20, hour=3, day=14, month=4, &
                                                   year=1989_int64, dst=DECODED_TIME_DST_UNKNOWN))
        if (allocated(error)) return
        call expect_date(error, "14 Apr 89 03:20:12 GMT", &
                         partial_decoded_time_type(second=time_type(12), minute=20, hour=3, day=14, month=4, &
                                                   year=1989_int64, dst=DECODED_TIME_DST_NO, zone=0))
        if (allocated(error)) return
        call expect_date(error, "Fri, 17 Mar 89 4:01", &
                         partial_decoded_time_type(second=time_type(0), minute=1, hour=4, day=17, month=3, &
                                                   year=1989_int64, dst=DECODED_TIME_DST_UNKNOWN))
        if (allocated(error)) return
        call expect_date(error, "Fri, 17 Mar 89 4:01:33 GMT", &
                         partial_decoded_time_type(second=time_type(33), minute=1, hour=4, day=17, month=3, &
                                                   year=1989_int64, dst=DECODED_TIME_DST_NO, zone=0))
        if (allocated(error)) return
        call expect_date(error, "Mon Jan 16 16:12 1989", &
                         partial_decoded_time_type(second=time_type(0), minute=12, hour=16, day=16, month=1, &
                                                   year=1989_int64, dst=DECODED_TIME_DST_UNKNOWN))
        if (allocated(error)) return
        call expect_date(error, "Mon Jan 16 16:12:37 GMT 1989", &
                         partial_decoded_time_type(second=time_type(37), minute=12, hour=16, day=16, month=1, &
                                                   year=1989_int64, dst=DECODED_TIME_DST_NO, zone=0))
        if (allocated(error)) return
        call expect_date(error, "Thu, 11 Apr 16:17:12 91", &
                         partial_decoded_time_type(second=time_type(12), minute=17, hour=16, day=11, month=4, &
                                                   year=1991_int64, dst=DECODED_TIME_DST_UNKNOWN))
        if (allocated(error)) return
        call expect_date(error, "Mon, 6  Jul 16:47:20 T 1992", &
                         partial_decoded_time_type(second=time_type(20), minute=47, hour=16, day=6, month=7, &
                                                   year=1992_int64, dst=DECODED_TIME_DST_UNKNOWN))
        if (allocated(error)) return
        call expect_date(error, "1996-06-24 21:13:12", &
                         partial_decoded_time_type(second=time_type(12), minute=13, hour=21, day=24, month=6, &
                                                   year=1996_int64, dst=DECODED_TIME_DST_UNKNOWN))
        if (allocated(error)) return
        call expect_date(error, "19960624t211312", &
                         partial_decoded_time_type(second=time_type(12), minute=13, hour=21, day=24, month=6, &
                                                   year=1996_int64, dst=DECODED_TIME_DST_UNKNOWN))
    end subroutine test_date_to_time

    subroutine test_days_between(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st

        call check(error, days_between("2021-10-22", "2020-09-29", st) == 388 .and. st%ok(), "days between")
    end subroutine test_days_between

    subroutine test_leap_year(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error,.not. is_date_leap_year(1999_int64) .and. .not. is_date_leap_year(1900_int64) .and. &
                   is_date_leap_year(2000_int64) .and. is_date_leap_year(2004_int64), "leap years")
    end subroutine test_leap_year

    subroutine test_days_to_time(error)
        type(error_type), allocatable, intent(out) :: error

        ! (An integer number of days is time_type(seconds=86400*DAYS).)
        call check(error, is_time_equal(days_to_time(0.0_real64), time_type()), "0.0")
        if (allocated(error)) return
        call check(error, is_time_equal(days_to_time(0.5_real64), time_type(seconds=43200)), "0.5")
        if (allocated(error)) return
        call check(error, is_time_equal(days_to_time(1.0_real64), time_type(seconds=86400)), "1.0")
        if (allocated(error)) return
        call check(error, is_time_equal(days_to_time(999.0_real64), time_type(seconds=86313600)), "999.0")
    end subroutine test_days_to_time

    subroutine test_days_in_month(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer :: days

        call check(error, date_days_in_month(2004_int64, 2) == 29, "2004-02")
        if (allocated(error)) return
        call check(error, date_days_in_month(2004_int64, 3) == 31, "2004-03")
        if (allocated(error)) return
        call check(error, date_days_in_month(2019_int64, 2) == 28, "2019-02")
        if (allocated(error)) return
        call check(error, date_days_in_month(2020_int64, 12) == 31, "2020-12")
        if (allocated(error)) return
        call check(error, date_days_in_month(1900_int64, 3) /= 28, "1900-03")
        if (allocated(error)) return
        days = date_days_in_month(2020_int64, 0, st)
        call check(error, format_error(st) == "Month 0 is invalid", format_error(st))
        if (allocated(error)) return
        days = date_days_in_month(2020_int64, 15, st)
        call check(error, format_error(st) == "Month 15 is invalid", format_error(st))
    end subroutine test_days_in_month

    subroutine test_format_seconds(error)
        type(error_type), allocatable, intent(out) :: error

        call expect_format(error, "%y %d %h %m %s %%", 0.0_real64, "0 0 0 0 0 %")
        if (allocated(error)) return
        call expect_format(error, "%y %d %h %m %s %%", 0.0_real64, "0 0 0 0 0 %")
        if (allocated(error)) return
        call expect_format(error, "%y %d %h %m %s %%", 9999999.0_real64, "0 115 17 46 39 %")
        if (allocated(error)) return
        call expect_format(error, "%y %d %h %m %z %s %%", 1.0_real64, "1 %")
        if (allocated(error)) return
        call expect_format(error, "%mm %ss", 66.0_real64, "1m 6s")
        if (allocated(error)) return
        call expect_format(error, "%mm %5ss", 66.0_real64, "1m     6s")
        if (allocated(error)) return
        call expect_format(error, "%mm %.5ss", 66.4_real64, "1m 00006s")
        if (allocated(error)) return
        call expect_format(error, "%mm %,1ss", 66.4_real64, "1m 6.4s")
        if (allocated(error)) return
        call expect_format(error, "%mm %5,1ss", 66.4_real64, "1m   6.4s")
        if (allocated(error)) return
        call expect_format(error, "%mm %.5,1ss", 66.4_real64, "1m 006.4s")
        if (allocated(error)) return
        call expect_format(error, "%hh %z%x%mm %ss", 120.0_real64, "2m")
        if (allocated(error)) return
        call expect_format(error, "%hh %z%mm %ss", 120.0_real64, "2m 0s")
        if (allocated(error)) return
        call expect_format(error, "%hh %x%mm %ss", 120.0_real64, "0h 2m")
        if (allocated(error)) return
        call expect_format(error, "%hh %x%mm %ss", 0.0_real64, "0h 0m 0s")
        if (allocated(error)) return
        call expect_format(error, "%y %z%d %h %m %s %%", 9999999.0_real64, "115 17 46 39 %")
        if (allocated(error)) return
        call expect_format(error, "%Y, %D, %H, %M, %z%S", 0.0_real64, "0 seconds")
    end subroutine test_format_seconds

    subroutine test_ordinal(error)
        type(error_type), allocatable, intent(out) :: error

        call expect_decoded(error, date_ordinal_to_time(2008_int64, 271), "day=27 month=9 year=2008 dst=0")
        if (allocated(error)) return
        call expect_decoded(error, date_ordinal_to_time(2008_int64, 1), "day=1 month=1 year=2008 dst=0")
        if (allocated(error)) return
        call expect_decoded(error, date_ordinal_to_time(2008_int64, 32), "day=1 month=2 year=2008 dst=0")
        if (allocated(error)) return
        call expect_decoded(error, date_ordinal_to_time(1981_int64, 95), "day=5 month=4 year=1981 dst=0")
    end subroutine test_ordinal

    subroutine test_decoded_add(error)
        type(error_type), allocatable, intent(out) :: error
        type(partial_decoded_time_type) :: time

        time = partial_decoded_time_type(second=time_type(12), minute=15, hour=16, day=8, month=7, year=2019_int64, &
                                         weekday=1, dst=DECODED_TIME_DST_YES, zone=7200)
        call expect_add(error, time, &
                        partial_decoded_time_type(year=1_int64), &
                        "second=12 minute=15 hour=16 day=8 month=7 year=2020 weekday=1 dst=1 zone=7200")
        if (allocated(error)) return
        call expect_add(error, time, &
                        partial_decoded_time_type(year=-2_int64), &
                        "second=12 minute=15 hour=16 day=8 month=7 year=2017 weekday=1 dst=1 zone=7200")
        if (allocated(error)) return
        call expect_add(error, time, &
                        partial_decoded_time_type(month=1), &
                        "second=12 minute=15 hour=16 day=8 month=8 year=2019 weekday=1 dst=1 zone=7200")
        if (allocated(error)) return
        call expect_add(error, time, &
                        partial_decoded_time_type(month=10), &
                        "second=12 minute=15 hour=16 day=8 month=5 year=2020 weekday=1 dst=1 zone=7200")
        if (allocated(error)) return
        call expect_add(error, time, &
                        partial_decoded_time_type(month=-1), &
                        "second=12 minute=15 hour=16 day=8 month=6 year=2019 weekday=1 dst=1 zone=7200")
        if (allocated(error)) return
        call expect_add(error, time, &
                        partial_decoded_time_type(month=-10), &
                        "second=12 minute=15 hour=16 day=8 month=9 year=2018 weekday=1 dst=1 zone=7200")
        if (allocated(error)) return
        call expect_add(error, time, &
                        partial_decoded_time_type(month=-14), &
                        "second=12 minute=15 hour=16 day=8 month=5 year=2018 weekday=1 dst=1 zone=7200")
        if (allocated(error)) return
        call expect_add(error, time, &
                        partial_decoded_time_type(month=-24), &
                        "second=12 minute=15 hour=16 day=8 month=7 year=2017 weekday=1 dst=1 zone=7200")
        if (allocated(error)) return
        call expect_add(error, time, &
                        partial_decoded_time_type(day=1), &
                        "second=12 minute=15 hour=16 day=9 month=7 year=2019 weekday=1 dst=1 zone=7200")
        if (allocated(error)) return
        call expect_add(error, time, &
                        partial_decoded_time_type(day=-1), &
                        "second=12 minute=15 hour=16 day=7 month=7 year=2019 weekday=1 dst=1 zone=7200")
        if (allocated(error)) return
        call expect_add(error, time, &
                        partial_decoded_time_type(day=30), &
                        "second=12 minute=15 hour=16 day=7 month=8 year=2019 weekday=1 dst=1 zone=7200")
        if (allocated(error)) return
        call expect_add(error, time, &
                        partial_decoded_time_type(day=-365), &
                        "second=12 minute=15 hour=16 day=8 month=7 year=2018 weekday=1 dst=1 zone=7200")
        if (allocated(error)) return
        call expect_add(error, time, &
                        partial_decoded_time_type(day=365), &
                        "second=12 minute=15 hour=16 day=7 month=7 year=2020 weekday=1 dst=1 zone=7200")
        if (allocated(error)) return
        call expect_add(error, time, &
                        partial_decoded_time_type(day=366), &
                        "second=12 minute=15 hour=16 day=8 month=7 year=2020 weekday=1 dst=1 zone=7200")
        if (allocated(error)) return
        call expect_add(error, time, &
                        partial_decoded_time_type(second=time_type(1)), &
                        "second=13 minute=15 hour=16 day=8 month=7 year=2019 weekday=1 dst=1 zone=7200")
        if (allocated(error)) return
        call expect_add(error, time, &
                        partial_decoded_time_type(second=time_subtract(time_type(), time_type(1))), &
                        "second=11 minute=15 hour=16 day=8 month=7 year=2019 weekday=1 dst=1 zone=7200")
        if (allocated(error)) return
        call expect_add(error, time, &
                        partial_decoded_time_type(second=time_type(61)), &
                        "second=13 minute=16 hour=16 day=8 month=7 year=2019 weekday=1 dst=1 zone=7200")
        if (allocated(error)) return
        call expect_add(error, time, &
                        partial_decoded_time_type(hour=1, minute=2, second=time_type(3)), &
                        "second=15 minute=17 hour=17 day=8 month=7 year=2019 weekday=1 dst=1 zone=7200")
        if (allocated(error)) return
        call expect_add(error, time, &
                        partial_decoded_time_type(hour=24), &
                        "second=12 minute=15 hour=16 day=9 month=7 year=2019 weekday=1 dst=1 zone=7200")
    end subroutine test_decoded_add

    subroutine test_decoded_add_zone(error)
        type(error_type), allocatable, intent(out) :: error
        type(partial_decoded_time_type) :: time

        time = partial_decoded_time_type(second=time_type(12), minute=15, hour=16, day=8, month=7, year=2019_int64, &
                                         weekday=1, dst=DECODED_TIME_DST_YES, zone=7200)
        call expect_add(error, time, &
                        partial_decoded_time_type(zone=-3600), &
                        "second=12 minute=15 hour=15 day=8 month=7 year=2019 weekday=1 dst=1 zone=7200")
        if (allocated(error)) return
        call expect_add(error, time, &
                        partial_decoded_time_type(zone=-7200), &
                        "second=12 minute=15 hour=14 day=8 month=7 year=2019 weekday=1 dst=1 zone=7200")
    end subroutine test_decoded_add_zone

    subroutine test_time_since(error)
        type(error_type), allocatable, intent(out) :: error
        type(time_type) :: elapsed

        ! (Upstream's TIME nil stands for the current time.)
        elapsed = time_since(current_time())
        call check(error, elapsed%seconds == 0, "since now")
        if (allocated(error)) return
        elapsed = time_since(time_subtract(current_time(), time_type(1)))
        call check(error, elapsed%seconds == 1, "since a second ago")
    end subroutine test_time_since

    subroutine test_time_decoded_period(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, decoded_time_period(partial_decoded_time_type(hour=1, dst=DECODED_TIME_DST_NO)) == 3600.0_real64, &
                   "period 3600")
        if (allocated(error)) return
        call check(error, decoded_time_period(partial_decoded_time_type(second=time_type(1), minute=0, hour=0, day=0, &
                                                                   month=0, year=0_int64, dst=DECODED_TIME_DST_NO)) == 1.0_real64, &
                   "period 1")
        if (allocated(error)) return
        call check(error, decoded_time_period(partial_decoded_time_type(second=time_type(0), minute=1, hour=0, day=0, &
                                                                  month=0, year=0_int64, dst=DECODED_TIME_DST_NO)) == 60.0_real64, &
                   "period 60")
        if (allocated(error)) return
        call check(error, decoded_time_period(partial_decoded_time_type(second=time_type(0), minute=0, hour=1, day=0, &
                                                                month=0, year=0_int64, dst=DECODED_TIME_DST_NO)) == 3600.0_real64, &
                   "period 3600")
        if (allocated(error)) return
        call check(error, decoded_time_period(partial_decoded_time_type(second=time_type(0), minute=0, hour=0, day=1, &
                                                               month=0, year=0_int64, dst=DECODED_TIME_DST_NO)) == 86400.0_real64, &
                   "period 86400")
        if (allocated(error)) return
        call check(error, decoded_time_period(partial_decoded_time_type(second=time_type(0), minute=0, hour=0, day=0, &
                                                             month=1, year=0_int64, dst=DECODED_TIME_DST_NO)) == 2592000.0_real64, &
                   "period 2592000")
        if (allocated(error)) return
        call check(error, decoded_time_period(partial_decoded_time_type(second=time_type(0), minute=0, hour=0, day=0, &
                                                            month=0, year=1_int64, dst=DECODED_TIME_DST_NO)) == 31536000.0_real64, &
                   "period 31536000")
        if (allocated(error)) return
        call check(error, decoded_time_period(partial_decoded_time_type(second=time_type(1), minute=2, hour=3, day=4, &
                                                           month=5, year=6_int64, dst=DECODED_TIME_DST_NO)) == 202532521.0_real64, &
                   "period 202532521")
        if (allocated(error)) return
        call check(error, decoded_time_period(partial_decoded_time_type(second=time_type(13, 5, 10), minute=0, hour=0, &
                                                           day=0, month=0, year=0_int64, dst=DECODED_TIME_DST_NO)) == 13.5_real64, &
                   "period 13.5")
    end subroutine test_time_decoded_period

    subroutine test_time_wrap_addition(error)
        type(error_type), allocatable, intent(out) :: error

        call expect_add(error, partial_decoded_time_type(second=time_type(0), minute=0, hour=0, day=1, month=11, &
                                                         year=2008_int64, dst=DECODED_TIME_DST_NO), &
                        partial_decoded_time_type(month=1), &
                        "second=0 minute=0 hour=0 day=1 month=12 year=2008 dst=0")
        if (allocated(error)) return
        call expect_add(error, partial_decoded_time_type(second=time_type(0), minute=0, hour=0, day=1, month=12, &
                                                         year=2008_int64, dst=DECODED_TIME_DST_NO), &
                        partial_decoded_time_type(month=1), &
                        "second=0 minute=0 hour=0 day=1 month=1 year=2009 dst=0")
        if (allocated(error)) return
        call expect_add(error, partial_decoded_time_type(second=time_type(0), minute=0, hour=0, day=1, month=11, &
                                                         year=2008_int64, dst=DECODED_TIME_DST_NO), &
                        partial_decoded_time_type(month=12), &
                        "second=0 minute=0 hour=0 day=1 month=11 year=2009 dst=0")
        if (allocated(error)) return
        call expect_add(error, partial_decoded_time_type(second=time_type(0), minute=0, hour=0, day=1, month=11, &
                                                         year=2008_int64, dst=DECODED_TIME_DST_NO), &
                        partial_decoded_time_type(month=13), &
                        "second=0 minute=0 hour=0 day=1 month=12 year=2009 dst=0")
        if (allocated(error)) return
        call expect_add(error, partial_decoded_time_type(second=time_type(0), minute=0, hour=0, day=30, month=12, &
                                                         year=2008_int64, dst=DECODED_TIME_DST_NO), &
                        partial_decoded_time_type(day=1), &
                        "second=0 minute=0 hour=0 day=31 month=12 year=2008 dst=0")
        if (allocated(error)) return
        call expect_add(error, partial_decoded_time_type(second=time_type(0), minute=0, hour=0, day=30, month=12, &
                                                         year=2008_int64, dst=DECODED_TIME_DST_NO), &
                        partial_decoded_time_type(day=2), &
                        "second=0 minute=0 hour=0 day=1 month=1 year=2009 dst=0")
    end subroutine test_time_wrap_addition

    !> A TIME whose month is unknown is an error; one whose year is unknown
    !> adds days within a month that is not February.
    subroutine test_add_unknown(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        type(partial_decoded_time_type) :: sum

        sum = add_decoded_time(partial_decoded_time_type(day=1), partial_decoded_time_type(day=1), st)
        call check(error, format_error(st) == "Month unallocated is invalid", format_error(st))
        if (allocated(error)) return
        call expect_add(error, partial_decoded_time_type(day=15, month=3), partial_decoded_time_type(day=1), &
                        "second=0 minute=0 hour=0 day=16 month=3 dst=-1")
    end subroutine test_add_unknown

    !> The errors of `format_seconds'.
    subroutine test_format_seconds_errors(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        character(len=:), allocatable :: text

        text = format_seconds("%q", 1.0_real64, st)
        call expect(error, format_error(st), "Bad format specifier: `q'")
        if (allocated(error)) return
        text = format_seconds("%s %S", 1.0_real64, st)
        call expect(error, format_error(st), "Multiple instances of specifier: `S'")
        if (allocated(error)) return
        text = format_seconds("%z%s %m", 1.0_real64, st)
        call expect(error, format_error(st), "Units are not in decreasing order of size")
    end subroutine test_format_seconds_errors

    subroutine test_seconds_to_string(error)
        type(error_type), allocatable, intent(out) :: error

        call expect(error, seconds_to_string(0.0_real64), "0s")
        if (allocated(error)) return
        call expect(error, seconds_to_string(9.0_real64), "9.00s")
        if (allocated(error)) return
        call expect(error, seconds_to_string(99.0_real64), "99.00s")
        if (allocated(error)) return
        call expect(error, seconds_to_string(999.0_real64), "16.65m")
        if (allocated(error)) return
        call expect(error, seconds_to_string(9999.0_real64), "2.78h")
        if (allocated(error)) return
        call expect(error, seconds_to_string(99999.0_real64), "27.78h")
        if (allocated(error)) return
        call expect(error, seconds_to_string(999999.0_real64), "11.57d")
        if (allocated(error)) return
        call expect(error, seconds_to_string(9999999.0_real64), "115.74d")
        if (allocated(error)) return
        call expect(error, seconds_to_string(99999999.0_real64), "3.17y")
        if (allocated(error)) return
        call expect(error, seconds_to_string(999999999.0_real64), "31.69y")
    end subroutine test_seconds_to_string

    !> READABLE, ABBREV and `expanded' (Ibuffer's recency column uses the
    !> abbreviated readable form).
    subroutine test_readable(error)
        type(error_type), allocatable, intent(out) :: error

        call expect(error, seconds_to_string(200.0_real64, SECONDS_TO_STRING_READABLE, .true.), "3m")
        if (allocated(error)) return
        call expect(error, seconds_to_string(0.2_real64, SECONDS_TO_STRING_READABLE, .true.), "0s")
        if (allocated(error)) return
        call expect(error, seconds_to_string(1.0_real64, SECONDS_TO_STRING_READABLE), "1 second")
        if (allocated(error)) return
        call expect(error, seconds_to_string(7200.0_real64, SECONDS_TO_STRING_READABLE), "2 hours")
        if (allocated(error)) return
        call expect(error, seconds_to_string(5520.0_real64, SECONDS_TO_STRING_EXPANDED), "1 hour 32 minutes")
        if (allocated(error)) return
        call expect(error, seconds_to_string(5520.0_real64, SECONDS_TO_STRING_READABLE, precision=1), &
                    "1.5 hours")
    end subroutine test_readable
end module test_time_date_mod
