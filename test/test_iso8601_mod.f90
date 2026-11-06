! test_iso8601_mod.f90 -- tests for iso8601_mod.
! Ports GNU Emacs 31.1 test/lisp/calendar/iso8601-tests.el.
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

!> The iso8601-tests.el tests.  A decoded time is compared as its known
!> components, written "second=30 minute=47 ... daylight_saving_time=-1 zone=3600"
!> (a second with a fraction as TICKS/COUNT_RATE; dst always).
module test_iso8601_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_mod, only: status_type, format_error
    use time_functions_mod, only: time_type, format_date, date_format_type, time_zone_rule, DATE_ZONE_OFFSET, &
                                  DATE_ZONE_OFFSET_SIGN, DATE_ZONE_OFFSET_HOURS, DATE_ZONE_OFFSET_MINUTES
    use time_date_mod, only: partial_decoded_time_type
    use iso8601_mod, only: parse_iso8601, parse_iso8601_date, parse_iso8601_time, parse_iso8601_zone, &
                           parse_iso8601_duration, parse_iso8601_interval, is_iso8601_valid
    use test_time_support_mod, only: render_partial_decoded_time
    implicit none(type, external)
    private

    public :: collect_iso8601_mod

    ! The functions `expect' calls.
    integer, parameter :: COMBINED = 1, DATE = 2, TIME = 3, DURATION = 4
    character(len=*), parameter :: NAMES(4) = [character(len=22) :: "parse_iso8601", "parse_iso8601_date", &
                                                                                     "parse_iso8601_time", "parse_iso8601_duration"]

contains

    !> Collect all exported unit tests.
    subroutine collect_iso8601_mod(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("test-iso8601-date-years", test_iso8601_date_years), &
                    new_unittest("test-iso8601-date-dates", test_iso8601_date_dates), &
                    new_unittest("test-iso8601-date-obsolete", test_iso8601_date_obsolete), &
                    new_unittest("test-iso8601-date-obsolete-2000", test_iso8601_date_obsolete_2000), &
                    new_unittest("test-iso8601-date-weeks", test_iso8601_date_weeks), &
                    new_unittest("test-iso8601-date-ordinals", test_iso8601_date_ordinals), &
                    new_unittest("test-iso8601-time", test_iso8601_time), &
                    new_unittest("test-iso8601-combined", test_iso8601_combined), &
                    new_unittest("test-iso8601-duration", test_iso8601_duration), &
                    new_unittest("test-iso8601-invalid", test_iso8601_invalid), &
                    new_unittest("test-iso8601-intervals", test_iso8601_intervals), &
                    new_unittest("standard-test-dates", test_standard_test_dates), &
                    new_unittest("standard-test-time-of-day-local-time", test_standard_test_time_of_day_local_time), &
                    new_unittest("standard-test-time-of-day-fractions", test_standard_test_time_of_day_fractions), &
                    new_unittest("nonstandard-test-time-of-day-decimals", test_nonstandard_test_time_of_day_decimals), &
                    new_unittest("standard-test-time-of-day-beginning-of-day", test_standard_test_time_of_day_beginning_of_day), &
                    new_unittest("standard-test-time-of-day-utc", test_standard_test_time_of_day_utc), &
                    new_unittest("standard-test-time-of-day-zone", test_standard_test_time_of_day_zone), &
                    new_unittest("standard-test-date-and-time-of-day", test_standard_test_date_and_time_of_day), &
                    new_unittest("standard-test-interval", test_standard_test_interval), &
                    new_unittest("iso8601-format-time-string-zone-round-trip", test_zone_round_trip) &
                    ]
    end subroutine collect_iso8601_mod

    ! Check that FUNCTION (COMBINED: parse_iso8601, DATE, TIME, DURATION)
    ! of TEXT gives the decoded time EXPECTED.
    subroutine expect(error, function, text, expected, keep_fraction)
        type(error_type), allocatable, intent(out) :: error
        integer, intent(in) :: function
        character(len=*), intent(in) :: text, expected
        logical, intent(in), optional :: keep_fraction
        type(partial_decoded_time_type) :: got
        type(status_type) :: st
        character(len=:), allocatable :: rendered

        select case (function)
        case (COMBINED)
            got = parse_iso8601(text, st, keep_fraction)
        case (DATE)
            got = parse_iso8601_date(text, st)
        case (TIME)
            got = parse_iso8601_time(text, st, keep_fraction)
        case default
            got = parse_iso8601_duration(text, st)
        end select

        if (.not. st%ok()) then
            call check(error, .false., trim(NAMES(function))//"("""//text//"""): "//format_error(st))
            return
        end if

        rendered = render_partial_decoded_time(got)
        call check(error, rendered == expected, trim(NAMES(function))//"("""//text//""") gave "//rendered// &
                   ", expected "//expected)
    end subroutine expect

    ! Check that parse_iso8601_interval of TEXT gives START, END and
    ! DURATION.
    subroutine expect_interval(error, text, start, end, duration)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), intent(in) :: text, start, end, duration
        type(partial_decoded_time_type) :: got_start, got_end, got_duration
        type(status_type) :: st
        character(len=:), allocatable :: rendered, expected

        call parse_iso8601_interval(text, got_start, got_end, got_duration, st)

        if (.not. st%ok()) then
            call check(error, .false., "parse_iso8601_interval("""//text//"""): "//format_error(st))
            return
        end if

        rendered = render_partial_decoded_time(got_start)//"; "//render_partial_decoded_time(got_end)//"; "// &
                   render_partial_decoded_time(got_duration)
        expected = start//"; "//end//"; "//duration
        call check(error, rendered == expected, "parse_iso8601_interval("""//text//""") gave "//rendered// &
                   ", expected "//expected)
    end subroutine expect_interval

    subroutine test_iso8601_date_years(error)
        type(error_type), allocatable, intent(out) :: error

        call expect(error, DATE, "1985", "year=1985 daylight_saving_time=-1")
        if (allocated(error)) return
        call expect(error, DATE, "-0003", "year=-3 daylight_saving_time=-1")
        if (allocated(error)) return
        call expect(error, DATE, "+1985", "year=1985 daylight_saving_time=-1")
    end subroutine test_iso8601_date_years

    subroutine test_iso8601_date_dates(error)
        type(error_type), allocatable, intent(out) :: error

        call expect(error, DATE, "1985-03-14", "day=14 month=3 year=1985 daylight_saving_time=-1")
        if (allocated(error)) return
        call expect(error, DATE, "19850314", "day=14 month=3 year=1985 daylight_saving_time=-1")
        if (allocated(error)) return
        call expect(error, DATE, "1985-02", "month=2 year=1985 daylight_saving_time=-1")
    end subroutine test_iso8601_date_dates

    subroutine test_iso8601_date_obsolete(error)
        type(error_type), allocatable, intent(out) :: error

        call expect(error, DATE, "--02-01", "day=1 month=2 daylight_saving_time=-1")
        if (allocated(error)) return
        call expect(error, DATE, "--0201", "day=1 month=2 daylight_saving_time=-1")
    end subroutine test_iso8601_date_obsolete

    subroutine test_iso8601_date_obsolete_2000(error)
        type(error_type), allocatable, intent(out) :: error

        call expect(error, DATE, "--12", "month=12 daylight_saving_time=-1")
        if (allocated(error)) return
        call expect(error, COMBINED, "--12T14", "second=0 minute=0 hour=14 month=12 daylight_saving_time=-1")
        if (allocated(error)) return
        call expect(error, DATE, "---12", "day=12 daylight_saving_time=-1")
        if (allocated(error)) return
        call expect(error, COMBINED, "---12T14:10:12", "second=12 minute=10 hour=14 day=12 daylight_saving_time=-1")
    end subroutine test_iso8601_date_obsolete_2000

    subroutine test_iso8601_date_weeks(error)
        type(error_type), allocatable, intent(out) :: error

        call expect(error, DATE, "2008W39-6", "day=27 month=9 year=2008 daylight_saving_time=-1")
        if (allocated(error)) return
        call expect(error, DATE, "2009W01-1", "day=29 month=12 year=2008 daylight_saving_time=-1")
        if (allocated(error)) return
        call expect(error, DATE, "2009W53-7", "day=3 month=1 year=2010 daylight_saving_time=-1")
    end subroutine test_iso8601_date_weeks

    subroutine test_iso8601_date_ordinals(error)
        type(error_type), allocatable, intent(out) :: error

        call expect(error, DATE, "1981-095", "day=5 month=4 year=1981 daylight_saving_time=-1")
    end subroutine test_iso8601_date_ordinals

    subroutine test_iso8601_time(error)
        type(error_type), allocatable, intent(out) :: error

        call expect(error, TIME, "13:47:30", "second=30 minute=47 hour=13 daylight_saving_time=-1")
        if (allocated(error)) return
        call expect(error, TIME, "134730", "second=30 minute=47 hour=13 daylight_saving_time=-1")
        if (allocated(error)) return
        call expect(error, TIME, "1347", "second=0 minute=47 hour=13 daylight_saving_time=-1")
    end subroutine test_iso8601_time

    subroutine test_iso8601_combined(error)
        type(error_type), allocatable, intent(out) :: error

        call expect(error, COMBINED, "2008-03-02T13:47:30", &
                    "second=30 minute=47 hour=13 day=2 month=3 year=2008 daylight_saving_time=-1")
        if (allocated(error)) return
        call expect(error, COMBINED, "2008-03-02T13:47:30Z", &
                    "second=30 minute=47 hour=13 day=2 month=3 year=2008 daylight_saving_time=0 zone=0")
        if (allocated(error)) return
        call expect(error, COMBINED, "2008-03-02T13:47:30+01:00", &
                    "second=30 minute=47 hour=13 day=2 month=3 year=2008 daylight_saving_time=-1 zone=3600")
        if (allocated(error)) return
        call expect(error, COMBINED, "2008-03-02T13:47:30-01", &
                    "second=30 minute=47 hour=13 day=2 month=3 year=2008 daylight_saving_time=-1 zone=-3600")
    end subroutine test_iso8601_combined

    subroutine test_iso8601_duration(error)
        type(error_type), allocatable, intent(out) :: error

        call expect(error, DURATION, "P3Y6M4DT12H30M5S", "second=5 minute=30 hour=12 day=4 month=6 year=3 daylight_saving_time=-1")
        if (allocated(error)) return
        call expect(error, DURATION, "P1M", "second=0 minute=0 hour=0 day=0 month=1 year=0 daylight_saving_time=-1")
        if (allocated(error)) return
        call expect(error, DURATION, "PT1M", "second=0 minute=1 hour=0 day=0 month=0 year=0 daylight_saving_time=-1")
        if (allocated(error)) return
        call expect(error, DURATION, "P0003-06-04T12:30:05", &
                    "second=5 minute=30 hour=12 day=4 month=6 year=3 daylight_saving_time=-1")
    end subroutine test_iso8601_duration

    subroutine test_iso8601_invalid(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error,.not. is_iso8601_valid(" 2008-03-02T13:47:30-01"), " 2008-03-02T13:47:30-01 should be invalid")
        if (allocated(error)) return
        call check(error,.not. is_iso8601_valid("2008-03-02T13:47:30-01:200"), "2008-03-02T13:47:30-01:200 should be invalid")
        if (allocated(error)) return
        call check(error,.not. is_iso8601_valid("2008-03-02T13:47:30-01 "), "2008-03-02T13:47:30-01  should be invalid")
        if (allocated(error)) return
        call check(error,.not. is_iso8601_valid("2008-03-02 T 13:47:30-01 "), "2008-03-02 T 13:47:30-01  should be invalid")
        if (allocated(error)) return
        call check(error,.not. is_iso8601_valid("20008-03-02T13:47:30-01"), "20008-03-02T13:47:30-01 should be invalid")
    end subroutine test_iso8601_invalid

    subroutine test_iso8601_intervals(error)
        type(error_type), allocatable, intent(out) :: error

        call expect_interval(error, "2007-03-01T13:00:00Z/2008-05-11T15:30:00Z", &
                             "second=0 minute=0 hour=13 day=1 month=3 year=2007 daylight_saving_time=0 zone=0", &
                             "second=0 minute=30 hour=15 day=11 month=5 year=2008 daylight_saving_time=0 zone=0", &
                             "second=0 minute=30 hour=2 day=14 month=3 year=1971 weekday=0 daylight_saving_time=0 zone=0")
        if (allocated(error)) return
        call expect_interval(error, "2007-03-01T13:00:00Z/P1Y2M10DT2H30M", &
                             "second=0 minute=0 hour=13 day=1 month=3 year=2007 daylight_saving_time=0 zone=0", &
                             "second=0 minute=30 hour=15 day=11 month=5 year=2008 daylight_saving_time=0 zone=0", &
                             "second=0 minute=30 hour=2 day=10 month=2 year=1 daylight_saving_time=-1")
        if (allocated(error)) return
        call expect_interval(error, "P1Y2M10DT2H30M/2008-05-11T15:30:00Z", &
                             "second=0 minute=0 hour=13 day=1 month=3 year=2007 daylight_saving_time=0 zone=0", &
                             "second=0 minute=30 hour=15 day=11 month=5 year=2008 daylight_saving_time=0 zone=0", &
                             "second=0 minute=30 hour=2 day=10 month=2 year=1 daylight_saving_time=-1")
    end subroutine test_iso8601_intervals

    subroutine test_standard_test_dates(error)
        type(error_type), allocatable, intent(out) :: error

        call expect(error, DATE, "19850412", "day=12 month=4 year=1985 daylight_saving_time=-1")
        if (allocated(error)) return
        call expect(error, DATE, "1985-04-12", "day=12 month=4 year=1985 daylight_saving_time=-1")
        if (allocated(error)) return
        call expect(error, DATE, "1985102", "day=12 month=4 year=1985 daylight_saving_time=-1")
        if (allocated(error)) return
        call expect(error, DATE, "1985-102", "day=12 month=4 year=1985 daylight_saving_time=-1")
        if (allocated(error)) return
        call expect(error, DATE, "1985W155", "day=12 month=4 year=1985 daylight_saving_time=-1")
        if (allocated(error)) return
        call expect(error, DATE, "1985-W15-5", "day=12 month=4 year=1985 daylight_saving_time=-1")
        if (allocated(error)) return
        call expect(error, DATE, "1985W15", "day=7 month=4 year=1985 daylight_saving_time=-1")
        if (allocated(error)) return
        call expect(error, DATE, "1985-W15", "day=7 month=4 year=1985 daylight_saving_time=-1")
        if (allocated(error)) return
        call expect(error, DATE, "1985-04", "month=4 year=1985 daylight_saving_time=-1")
        if (allocated(error)) return
        call expect(error, DATE, "1985", "year=1985 daylight_saving_time=-1")
        if (allocated(error)) return
        call expect(error, DATE, "+1985-04-12", "day=12 month=4 year=1985 daylight_saving_time=-1")
        if (allocated(error)) return
        call expect(error, DATE, "+19850412", "day=12 month=4 year=1985 daylight_saving_time=-1")
    end subroutine test_standard_test_dates

    subroutine test_standard_test_time_of_day_local_time(error)
        type(error_type), allocatable, intent(out) :: error

        call expect(error, TIME, "152746", "second=46 minute=27 hour=15 daylight_saving_time=-1")
        if (allocated(error)) return
        call expect(error, TIME, "15:27:46", "second=46 minute=27 hour=15 daylight_saving_time=-1")
        if (allocated(error)) return
        call expect(error, TIME, "1528", "second=0 minute=28 hour=15 daylight_saving_time=-1")
        if (allocated(error)) return
        call expect(error, TIME, "15:28", "second=0 minute=28 hour=15 daylight_saving_time=-1")
        if (allocated(error)) return
        call expect(error, TIME, "15", "second=0 minute=0 hour=15 daylight_saving_time=-1")
    end subroutine test_standard_test_time_of_day_local_time

    subroutine test_standard_test_time_of_day_fractions(error)
        type(error_type), allocatable, intent(out) :: error

        call expect(error, TIME, "152735,5", "second=355/10 minute=27 hour=15 daylight_saving_time=-1", .true.)
        if (allocated(error)) return
        call expect(error, TIME, "15:27:35,5", "second=355/10 minute=27 hour=15 daylight_saving_time=-1", .true.)
        if (allocated(error)) return
        call expect(error, TIME, "2320,5", "second=30 minute=20 hour=23 daylight_saving_time=-1", .true.)
        if (allocated(error)) return
        call expect(error, TIME, "23:20,8", "second=48 minute=20 hour=23 daylight_saving_time=-1", .true.)
        if (allocated(error)) return
        call expect(error, TIME, "23,3", "second=0 minute=18 hour=23 daylight_saving_time=-1", .true.)
    end subroutine test_standard_test_time_of_day_fractions

    subroutine test_nonstandard_test_time_of_day_decimals(error)
        type(error_type), allocatable, intent(out) :: error

        call expect(error, TIME, "15:27:35.123", "second=35123/1000 minute=27 hour=15 daylight_saving_time=-1", .true.)
        if (allocated(error)) return
        call expect(error, TIME, "15:27:35.123456789", &
                    "second=35123456789/1000000000 minute=27 hour=15 daylight_saving_time=-1", .true.)
        if (allocated(error)) return
        call expect(error, TIME, "15:27:35.012345678", &
                    "second=35012345678/1000000000 minute=27 hour=15 daylight_saving_time=-1", .true.)
        if (allocated(error)) return
        call expect(error, TIME, "15:27:35.00001", "second=3500001/100000 minute=27 hour=15 daylight_saving_time=-1", .true.)
        if (allocated(error)) return
        call expect(error, TIME, "15:27:35.0000100", "second=3500001/100000 minute=27 hour=15 daylight_saving_time=-1", .true.)
        if (allocated(error)) return
        call expect(error, TIME, "15:27:35.0", "second=35 minute=27 hour=15 daylight_saving_time=-1", .true.)
    end subroutine test_nonstandard_test_time_of_day_decimals

    subroutine test_standard_test_time_of_day_beginning_of_day(error)
        type(error_type), allocatable, intent(out) :: error

        call expect(error, TIME, "000000", "second=0 minute=0 hour=0 daylight_saving_time=-1")
        if (allocated(error)) return
        call expect(error, TIME, "00:00:00", "second=0 minute=0 hour=0 daylight_saving_time=-1")
        if (allocated(error)) return
        call expect(error, TIME, "0000", "second=0 minute=0 hour=0 daylight_saving_time=-1")
        if (allocated(error)) return
        call expect(error, TIME, "00:00", "second=0 minute=0 hour=0 daylight_saving_time=-1")
    end subroutine test_standard_test_time_of_day_beginning_of_day

    subroutine test_standard_test_time_of_day_utc(error)
        type(error_type), allocatable, intent(out) :: error

        call expect(error, TIME, "232030Z", "second=30 minute=20 hour=23 daylight_saving_time=0 zone=0")
        if (allocated(error)) return
        call expect(error, TIME, "23:20:30Z", "second=30 minute=20 hour=23 daylight_saving_time=0 zone=0")
        if (allocated(error)) return
        call expect(error, TIME, "2320Z", "second=0 minute=20 hour=23 daylight_saving_time=0 zone=0")
        if (allocated(error)) return
        call expect(error, TIME, "23:20Z", "second=0 minute=20 hour=23 daylight_saving_time=0 zone=0")
        if (allocated(error)) return
        call expect(error, TIME, "23Z", "second=0 minute=0 hour=23 daylight_saving_time=0 zone=0")
    end subroutine test_standard_test_time_of_day_utc

    subroutine test_standard_test_time_of_day_zone(error)
        type(error_type), allocatable, intent(out) :: error

        call expect(error, TIME, "152746+0100", "second=46 minute=27 hour=15 daylight_saving_time=-1 zone=3600")
        if (allocated(error)) return
        call expect(error, TIME, "15:27:46+0100", "second=46 minute=27 hour=15 daylight_saving_time=-1 zone=3600")
        if (allocated(error)) return
        call expect(error, TIME, "152746+01", "second=46 minute=27 hour=15 daylight_saving_time=-1 zone=3600")
        if (allocated(error)) return
        call expect(error, TIME, "15:27:46+01", "second=46 minute=27 hour=15 daylight_saving_time=-1 zone=3600")
        if (allocated(error)) return
        call expect(error, TIME, "152746-0500", "second=46 minute=27 hour=15 daylight_saving_time=-1 zone=-18000")
        if (allocated(error)) return
        call expect(error, TIME, "15:27:46-0500", "second=46 minute=27 hour=15 daylight_saving_time=-1 zone=-18000")
        if (allocated(error)) return
        call expect(error, TIME, "152746-05", "second=46 minute=27 hour=15 daylight_saving_time=-1 zone=-18000")
        if (allocated(error)) return
        call expect(error, TIME, "15:27:46-05", "second=46 minute=27 hour=15 daylight_saving_time=-1 zone=-18000")
    end subroutine test_standard_test_time_of_day_zone

    subroutine test_standard_test_date_and_time_of_day(error)
        type(error_type), allocatable, intent(out) :: error

        call expect(error, COMBINED, "19850412T101530", &
                    "second=30 minute=15 hour=10 day=12 month=4 year=1985 daylight_saving_time=-1")
        if (allocated(error)) return
        call expect(error, COMBINED, "1985-04-12T10:15:30", &
                    "second=30 minute=15 hour=10 day=12 month=4 year=1985 daylight_saving_time=-1")
        if (allocated(error)) return
        call expect(error, COMBINED, "1985102T235030Z", &
                    "second=30 minute=50 hour=23 day=12 month=4 year=1985 daylight_saving_time=0 zone=0")
        if (allocated(error)) return
        call expect(error, COMBINED, "1985-102T23:50:30Z", &
                    "second=30 minute=50 hour=23 day=12 month=4 year=1985 daylight_saving_time=0 zone=0")
        if (allocated(error)) return
        call expect(error, COMBINED, "1985W155T235030", &
                    "second=30 minute=50 hour=23 day=12 month=4 year=1985 daylight_saving_time=-1")
        if (allocated(error)) return
        call expect(error, COMBINED, "1985-W155T23:50:30", &
                    "second=30 minute=50 hour=23 day=12 month=4 year=1985 daylight_saving_time=-1")
    end subroutine test_standard_test_date_and_time_of_day

    subroutine test_standard_test_interval(error)
        type(error_type), allocatable, intent(out) :: error

        call expect(error, DURATION, "P2Y10M15DT10H20M30S", &
                    "second=30 minute=20 hour=10 day=15 month=10 year=2 daylight_saving_time=-1")
        if (allocated(error)) return
        call expect(error, DURATION, "P00021015T102030", &
                    "second=30 minute=20 hour=10 day=15 month=10 year=2 daylight_saving_time=-1")
        if (allocated(error)) return
        call expect(error, DURATION, "P0002-10-15T10:20:30", &
                    "second=30 minute=20 hour=10 day=15 month=10 year=2 daylight_saving_time=-1")
        if (allocated(error)) return
        call expect(error, DURATION, "P1Y6M", "second=0 minute=0 hour=0 day=0 month=6 year=1 daylight_saving_time=-1")
        if (allocated(error)) return
        call expect(error, DURATION, "P0001-06", "month=6 year=1 daylight_saving_time=-1")
        if (allocated(error)) return
        call expect(error, DURATION, "PT72H", "second=0 minute=0 hour=72 day=0 month=0 year=0 daylight_saving_time=-1")
        if (allocated(error)) return
        call expect_interval(error, "19850412T232050Z/19850625T103000Z", &
                             "second=50 minute=20 hour=23 day=12 month=4 year=1985 daylight_saving_time=0 zone=0", &
                             "second=0 minute=30 hour=10 day=25 month=6 year=1985 daylight_saving_time=0 zone=0", &
                             "second=10 minute=9 hour=11 day=15 month=3 year=1970 weekday=0 daylight_saving_time=0 zone=0")
        if (allocated(error)) return
        call expect_interval(error, "1985-04-12T23:20:50Z/1985-06-25T10:30:00Z", &
                             "second=50 minute=20 hour=23 day=12 month=4 year=1985 daylight_saving_time=0 zone=0", &
                             "second=0 minute=30 hour=10 day=25 month=6 year=1985 daylight_saving_time=0 zone=0", &
                             "second=10 minute=9 hour=11 day=15 month=3 year=1970 weekday=0 daylight_saving_time=0 zone=0")
        if (allocated(error)) return
        call expect_interval(error, "19850412T232000/P1Y2M15DT12H", &
                             "second=0 minute=20 hour=23 day=12 month=4 year=1985 daylight_saving_time=-1", &
                             "second=0 minute=20 hour=11 day=28 month=6 year=1986 daylight_saving_time=-1", &
                             "second=0 minute=0 hour=12 day=15 month=2 year=1 daylight_saving_time=-1")
        if (allocated(error)) return
        call expect_interval(error, "1985-04-12T23:20:00/P1Y2M15DT12H", &
                             "second=0 minute=20 hour=23 day=12 month=4 year=1985 daylight_saving_time=-1", &
                             "second=0 minute=20 hour=11 day=28 month=6 year=1986 daylight_saving_time=-1", &
                             "second=0 minute=0 hour=12 day=15 month=2 year=1 daylight_saving_time=-1")
        if (allocated(error)) return
        call expect_interval(error, "P1Y2M15DT12H/19850412T232000", &
                             "second=0 minute=20 hour=11 day=28 month=1 year=1984 daylight_saving_time=-1", &
                             "second=0 minute=20 hour=23 day=12 month=4 year=1985 daylight_saving_time=-1", &
                             "second=0 minute=0 hour=12 day=15 month=2 year=1 daylight_saving_time=-1")
    end subroutine test_standard_test_interval

    !> Round trip zone offsets through format_date and
    !> parse_iso8601_zone.  Passing a time zone created by format_date
    !> as +hhmm or +hh:mm (upstream's %z and %:z; its %:::z, +hh where the
    !> minutes are 0, has no date format) to parse_iso8601_zone should yield
    !> the original offset.
    subroutine test_zone_round_trip(error)
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: OFFSETS(13) = [5*60, 11*60, 5, 11, 5*60 + 30, 11*60 + 30, 0, &
                                             -5*60, -11*60, -5, -11, -5*60 - 30, -11*60 - 30]
        type(date_format_type) :: formats(2)
        character(len=:), allocatable :: zone_string
        type(status_type) :: st
        integer :: k, f, offset

        formats(1) = date_format_type("(sp, i5.4)", [DATE_ZONE_OFFSET])
        formats(2) = date_format_type('(a, i2.2, ":", i2.2)', [DATE_ZONE_OFFSET_SIGN, DATE_ZONE_OFFSET_HOURS, &
                                                               DATE_ZONE_OFFSET_MINUTES])

        do k = 1, size(OFFSETS)
            do f = 1, size(formats)
                offset = 0
                zone_string = format_date(formats(f), time_type(), time_zone_rule(60*OFFSETS(k)), st)
                if (st%ok()) offset = parse_iso8601_zone(zone_string, st)
                call check(error, st%ok() .and. offset == OFFSETS(k), trim(formats(f)%format)//" -> "//zone_string)
                if (allocated(error)) return
            end do
        end do
    end subroutine test_zone_round_trip
end module test_iso8601_mod
