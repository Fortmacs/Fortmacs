! test_parse_time_mod.f90 -- tests for parse_time_mod.
! Ports GNU Emacs 31.1 test/lisp/calendar/parse-time-tests.el.
!
! Copyright (C) 2016-2026 Free Software Foundation, Inc.
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

!> The parse-time-tests.el test, whose decoded times are compared as
!> test_time_support_mod writes them; and own tests of the tokens.
module test_parse_time_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_mod, only: status_type, format_error
    use time_functions_mod, only: time_type, decoded_time_type, encode_time, format_time_string, is_time_equal
    use time_date_mod, only: partial_decoded_time_type
    use parse_time_mod, only: parse_time_string, parse_iso8601_time_string
    use test_time_support_mod, only: render_partial_decoded_time
    implicit none(type, external)
    private

    public :: collect_parse_time_mod

contains

    !> Collect all exported unit tests.
    subroutine collect_parse_time_mod(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("parse-time-tests", test_parse_time), &
                    new_unittest("parse-time-string-tokens", test_tokens) &
                    ]
    end subroutine collect_parse_time_mod

    ! Check that parse_time_string of TEXT gives the decoded time EXPECTED.
    subroutine expect(error, text, expected)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), intent(in) :: text, expected
        type(status_type) :: st
        character(len=:), allocatable :: rendered

        rendered = render_partial_decoded_time(parse_time_string(text, st))
        call check(error, st%ok() .and. rendered == expected, "parse_time_string("""//text//""") gave "//rendered// &
                   ", expected "//expected)
    end subroutine expect

    ! Check that the time of parse_iso8601_time_string of TEXT is EXPECTED
    ! in UTC.
    subroutine expect_utc(error, text, expected)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), intent(in) :: text, expected
        type(status_type) :: st
        type(time_type) :: time
        character(len=:), allocatable :: got

        time = parse_iso8601_time_string(text, st)

        if (.not. st%ok()) then
            call check(error, .false., text//": "//format_error(st))
            return
        end if

        got = format_time_string("%Y-%m-%d %H:%M:%S", time, "UTC0")
        call check(error, got == expected, text//" gave "//got//", expected "//expected)
    end subroutine expect_utc

    subroutine test_parse_time(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        type(time_type) :: time

        call expect(error, "Mon, 22 Feb 2016 19:35:42 +0100", &
                    "second=42 minute=35 hour=19 day=22 month=2 year=2016 weekday=1 daylight_saving_time=-1 zone=3600")
        if (allocated(error)) return
        call expect(error, "22 Feb 2016 19:35:42 +0100", &
                    "second=42 minute=35 hour=19 day=22 month=2 year=2016 daylight_saving_time=-1 zone=3600")
        if (allocated(error)) return
        call expect(error, "22 Feb 2016 +0100", "day=22 month=2 year=2016 daylight_saving_time=-1 zone=3600")
        if (allocated(error)) return
        call expect(error, "Mon, 22 Feb 16 19:35:42 +0100", &
                    "second=42 minute=35 hour=19 day=22 month=2 year=2016 weekday=1 daylight_saving_time=-1 zone=3600")
        if (allocated(error)) return
        call expect(error, "Mon, 22 February 2016 19:35:42 +0100", &
                    "second=42 minute=35 hour=19 day=22 month=2 year=2016 weekday=1 daylight_saving_time=-1 zone=3600")
        if (allocated(error)) return
        call expect(error, "Mon, 22 feb 2016 19:35:42 +0100", &
                    "second=42 minute=35 hour=19 day=22 month=2 year=2016 weekday=1 daylight_saving_time=-1 zone=3600")
        if (allocated(error)) return
        call expect(error, "Monday, 22 february 2016 19:35:42 +0100", &
                    "second=42 minute=35 hour=19 day=22 month=2 year=2016 weekday=1 daylight_saving_time=-1 zone=3600")
        if (allocated(error)) return
        call expect(error, "Monday, 22 february 2016 19:35:42 PST", &
                    "second=42 minute=35 hour=19 day=22 month=2 year=2016 weekday=1 daylight_saving_time=0 zone=-28800")
        if (allocated(error)) return
        call expect(error, "Friday, 21 Sep 2018 13:47:58 PDT", &
                    "second=58 minute=47 hour=13 day=21 month=9 year=2018 weekday=5 daylight_saving_time=1 zone=-25200")
        if (allocated(error)) return
        call expect_utc(error, "1998-09-12T12:21:54-0200", "1998-09-12 14:21:54")
        if (allocated(error)) return
        call expect_utc(error, "1998-09-12T12:21:54-0230", "1998-09-12 14:51:54")
        if (allocated(error)) return
        call expect_utc(error, "1998-09-12T12:21:54-02:00", "1998-09-12 14:21:54")
        if (allocated(error)) return
        call expect_utc(error, "1998-09-12T12:21:54-02", "1998-09-12 14:21:54")
        if (allocated(error)) return
        call expect_utc(error, "1998-09-12T12:21:54+0230", "1998-09-12 09:51:54")
        if (allocated(error)) return
        call expect_utc(error, "1998-09-12T12:21:54+02", "1998-09-12 10:21:54")
        if (allocated(error)) return
        call expect_utc(error, "1998-09-12T12:21:54Z", "1998-09-12 12:21:54")
        if (allocated(error)) return
        time = parse_iso8601_time_string("1998-09-12T12:21:54", st)
        call check(error, st%ok() .and. is_time_equal(time, encode_time(decoded_time_type(time_type(54), 21, 12, 12, 9, &
                                                                                          1998_int64))), &
                   "1998-09-12T12:21:54 is local time")
    end subroutine test_parse_time

    !> Tokens that are not a time, a number beyond int64, and a time whose
    !> parts are not numbers.
    subroutine test_tokens(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        type(partial_decoded_time_type) :: time

        call expect(error, "Tuesday 7 Mar 89 EST", "day=7 month=3 year=1989 weekday=2 daylight_saving_time=0 zone=-18000")
        if (allocated(error)) return
        call expect(error, "12 Jan 2005 9:05", "second=0 minute=5 hour=9 day=12 month=1 year=2005 daylight_saving_time=-1")
        if (allocated(error)) return
        call expect(error, "", "daylight_saving_time=-1")
        if (allocated(error)) return
        time = parse_time_string("1a:2b:3c", st)
        call check(error, format_error(st) == "Not an integer string: `1a:2b:3c'", format_error(st))
    end subroutine test_tokens
end module test_parse_time_mod
