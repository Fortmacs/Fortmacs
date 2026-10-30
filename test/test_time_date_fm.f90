! test_time_date_fm.f90 -- tests for time_date_fm.
! Ported from GNU Emacs 31.1 test/lisp/calendar/time-date-tests.el.
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

!> Ports test-seconds-to-string of time-date-tests.el (the other tests
!> are deferred with the rest of time-date.el); own tests of the readable
!> forms.
module test_time_date_fm
    use, intrinsic :: iso_fortran_env, only: real64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use time_date_fm, only: seconds_to_string, SECONDS_TO_STRING_READABLE, SECONDS_TO_STRING_EXPANDED
    implicit none(type, external)
    private

    public :: collect_time_date_fm

contains

    subroutine collect_time_date_fm(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("test-seconds-to-string", test_seconds_to_string), &
                    new_unittest("seconds-to-string-readable", test_readable) &
                    ]
    end subroutine collect_time_date_fm

    subroutine expect(error, actual, expected)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), intent(in) :: actual, expected

        call check(error, actual == expected, '"'//actual//'" expected "'//expected//'"')
    end subroutine expect

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
end module test_time_date_fm
