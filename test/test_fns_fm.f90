! test_fns_fm.f90 -- tests for fns_fm.
!
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

!> Ports test/src/fns-tests.el fns-tests-compare-strings (the cases with
!> a missing or non-integer argument cannot be written in Fortran); own
!> tests for `substring'.
module test_fns_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_fm, only: status_type, format_error, FM_ARGS_OUT_OF_RANGE
    use fns_fm, only: compare_strings, substring, COMPARE_EQUAL
    implicit none(type, external)
    private

    public :: collect_fns_fm

    character(len=*), parameter :: hello = "こんにちは"//"ｺﾝﾆﾁﾊ"
    character(len=*), parameter :: rotated = "んにちは"//"ｺﾝﾆﾁﾊ"//"こ"

contains

    subroutine collect_fns_fm(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)
        testsuite = [ &
                    new_unittest("fns-tests-compare-strings", test_compare_strings), &
                    new_unittest("substring", test_substring) &
                    ]
    end subroutine collect_fns_fm

    subroutine test_compare_strings(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: r
        r = compare_strings("xyzzy", "zyxxy", start1=0_int64, end1=10_int64, start2=0_int64, end2=5_int64, status=st)
        call check(error, st%ok() .and. r == -1, "end1 brought into range")
        if (allocated(error)) return
        r = compare_strings("xyzzy", "zyxxy", start1=0_int64, end1=5_int64, start2=-1_int64, end2=2_int64, status=st)
        call check(error, st%code == FM_ARGS_OUT_OF_RANGE .and. format_error(st) == 'Args out of range: "zyxxy", -1, 2', &
                   format_error(st))
        if (allocated(error)) return
        call check(error, compare_strings("", "") == COMPARE_EQUAL, "empty")
        if (allocated(error)) return
        call check(error, compare_strings("", "", start1=0_int64, end1=0_int64, start2=0_int64, end2=0_int64) &
                   == COMPARE_EQUAL, "empty 0 0")
        if (allocated(error)) return
        call check(error, compare_strings("test", "test") == COMPARE_EQUAL, "test")
        if (allocated(error)) return
        call check(error, compare_strings("test", "test", .true.) == COMPARE_EQUAL, "test t")
        if (allocated(error)) return
        call check(error, compare_strings("test", "test", .false.) == COMPARE_EQUAL, "test nil")
        if (allocated(error)) return
        call check(error, compare_strings("Test", "test", .true.) == COMPARE_EQUAL, "Test t")
        if (allocated(error)) return
        call check(error, compare_strings("Test", "test") == -1, "Test")
        if (allocated(error)) return
        call check(error, compare_strings("test", "Test") == 1, "test Test")
        if (allocated(error)) return
        call check(error, compare_strings("foobaz", "barbaz") == 1, "foobaz")
        if (allocated(error)) return
        call check(error, compare_strings("barbaz", "foobar") == -1, "barbaz")
        if (allocated(error)) return
        call check(error, compare_strings("foobaz", "farbaz") == 2, "farbaz")
        if (allocated(error)) return
        call check(error, compare_strings("farbaz", "foobar") == -2, "foobar")
        if (allocated(error)) return
        call check(error, compare_strings("abcxyz", "abcprq", start1=0_int64, end1=2_int64, start2=0_int64, &
                                          end2=2_int64) == COMPARE_EQUAL, "0 2")
        if (allocated(error)) return
        call check(error, compare_strings("abcxyz", "abcprq", start1=0_int64, end1=-3_int64, start2=0_int64, &
                                          end2=-3_int64) == COMPARE_EQUAL, "0 -3")
        if (allocated(error)) return
        call check(error, compare_strings("abcxyz", "abcprq", start1=0_int64, end1=6_int64, start2=0_int64, &
                                          end2=6_int64) == 4, "0 6")
        if (allocated(error)) return
        call check(error, compare_strings("abcprq", "abcxyz", start1=0_int64, end1=6_int64, start2=0_int64, &
                                          end2=6_int64) == -4, "0 6 reversed")
        if (allocated(error)) return
        call check(error, compare_strings("xyzzy", "azza", start1=-3_int64, end1=4_int64, start2=-3_int64, &
                                          end2=3_int64) == COMPARE_EQUAL, "negative starts")
        if (allocated(error)) return
        call check(error, compare_strings(hello, hello) == COMPARE_EQUAL, "multibyte")
        if (allocated(error)) return
        call check(error, compare_strings(rotated, hello) == 1, "multibyte greater")
        if (allocated(error)) return
        call check(error, compare_strings(hello, rotated) == -1, "multibyte less")
    end subroutine test_compare_strings

    !> Negative indices count from the end; a range outside the string
    !> signals args-out-of-range (fns.c:validate_subarray).
    subroutine test_substring(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        character(len=:), allocatable :: r
        r = substring("héllo", 1_int64, 3_int64, st)
        call check(error, st%ok() .and. r == "él", r)
        if (allocated(error)) return
        r = substring("héllo", -3_int64, status=st)
        call check(error, st%ok() .and. r == "llo", r)
        if (allocated(error)) return
        r = substring("héllo", 1_int64, -1_int64, st)
        call check(error, st%ok() .and. r == "éll", r)
        if (allocated(error)) return
        r = substring("abc", 0_int64, 4_int64, st)
        call check(error, st%code == FM_ARGS_OUT_OF_RANGE .and. format_error(st) == 'Args out of range: "abc", 0, 4', &
                   format_error(st))
        if (allocated(error)) return
        r = substring("abc", 2_int64, 1_int64, st)
        call check(error, st%code == FM_ARGS_OUT_OF_RANGE, "from after to")
        if (allocated(error)) return
        r = substring("abc", -4_int64, status=st)
        call check(error, st%code == FM_ARGS_OUT_OF_RANGE .and. format_error(st) == 'Args out of range: "abc", -4, nil', &
                   format_error(st))
    end subroutine test_substring
end module test_fns_fm
