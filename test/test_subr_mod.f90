! test_subr_mod.f90 -- tests for subr_mod.
! Ports GNU Emacs 31.1 test/lisp/subr-tests.el in part (the version
! tests); the others are not ported yet.
!
! Copyright (C) 2015-2026 Free Software Foundation, Inc.
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

!> Tests for subr_mod: the version comparisons.
module test_subr_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_mod, only: status_type, format_error
    use subr_mod, only: version_to_numbers, is_version_numbers_less, is_version_numbers_less_or_equal, is_version_less, &
                        is_version_less_or_equal
    implicit none(type, external)
    private

    public :: collect_subr_mod

contains

    !> Collect all exported unit tests.
    subroutine collect_subr_mod(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("version-parsing", test_version_parsing), &
                    new_unittest("version-parsing-errors", test_version_parsing_errors), &
                    new_unittest("version-numbers-less", test_version_numbers_less), &
                    new_unittest("version-numbers-less-or-equal", test_version_numbers_less_or_equal), &
                    new_unittest("version-compare", test_version_compare) &
                    ]
    end subroutine collect_subr_mod

    !> Check that VER converts to EXPECTED.
    subroutine check_list(error, ver, expected)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), intent(in) :: ver
        integer(int64), intent(in) :: expected(:)
        integer(int64), allocatable :: numbers(:)
        type(status_type) :: st

        call version_to_numbers(ver, numbers, st)
        call check(error, st%ok(), ver//": "//format_error(st))
        if (allocated(error)) return
        call check(error, size(numbers) == size(expected), ver//": length")
        if (allocated(error)) return
        call check(error, all(numbers == expected), ver//": numbers")
    end subroutine check_list

    !> Check that VER is invalid with MESSAGE.
    subroutine check_invalid(error, ver, message)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), intent(in) :: ver, message
        integer(int64), allocatable :: numbers(:)
        type(status_type) :: st

        call version_to_numbers(ver, numbers, st)
        call check(error,.not. st%ok(), ver//": no error")
        if (allocated(error)) return
        call check(error, format_error(st), message)
    end subroutine check_invalid

    !> Port of subr-tests.el:subr-test-version-parsing (without its cases
    !> for another version-separator, a constant here).
    subroutine test_version_parsing(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=16), parameter :: versions(38) = [character(len=16) :: &
                                                        ".5", "0.9 alpha1", "0.9 snapshot", "0.9-alpha1", "0.9-snapshot", &
                                                        "0.9.snapshot", "0.9_snapshot", "0.9alpha1", "0.9snapshot", "1.0 git", &
                                                    "1.0 pre2", "1.0-git", "1.0-pre2", "1.0.1-a", "1.0.1-f", "1.0.1.a", "1.0.1.f", &
                                                       "1.0.1_a", "1.0.1_f", "1.0.1a", "1.0.1f", "1.0.7.5", "1.0.git", "1.0.pre2", &
                                                        "1.0_git", "1.0_pre2", "1.0git", "1.0pre2", "22.8 beta3", "22.8-beta3", &
                                                        "22.8.beta3", "22.8_beta3", "22.8beta3", "6.9.30 Beta", "6.9.30-Beta", &
                                                        "6.9.30.Beta", "6.9.30Beta", "6.9.30_Beta"]
        ! Each version's numbers, padded with 99.
        integer(int64), parameter :: expected(4, 38) = reshape(int([ &
                                                                   0, 5, 99, 99, 0, 9, -3, 1, 0, 9, -4, 99, 0, 9, -3, 1, &
                                                                   0, 9, -4, 99, 0, 9, -4, 99, 0, 9, -4, 99, 0, 9, -3, 1, &
                                                                   0, 9, -4, 99, 1, 0, -4, 99, 1, 0, -1, 2, 1, 0, -4, 99, &
                                                                   1, 0, -1, 2, 1, 0, 1, 1, 1, 0, 1, 6, 1, 0, 1, 1, &
                                                                   1, 0, 1, 6, 1, 0, 1, 1, 1, 0, 1, 6, 1, 0, 1, 1, &
                                                                   1, 0, 1, 6, 1, 0, 7, 5, 1, 0, -4, 99, 1, 0, -1, 2, &
                                                                   1, 0, -4, 99, 1, 0, -1, 2, 1, 0, -4, 99, 1, 0, -1, 2, &
                                                                   22, 8, -2, 3, 22, 8, -2, 3, 22, 8, -2, 3, 22, 8, -2, 3, &
                                                                   22, 8, -2, 3, 6, 9, 30, -2, 6, 9, 30, -2, 6, 9, 30, -2, &
                                                                   6, 9, 30, -2, 6, 9, 30, -2], int64), [4, 38])
        integer :: k

        do k = 1, size(versions)
            call check_list(error, trim(versions(k)), pack(expected(:, k), expected(:, k) /= 99))
            if (allocated(error)) return
        end do
    end subroutine test_version_parsing

    !> Port of subr-tests.el:subr-test-version-parsing, its errors (a
    !> version is always a string here).
    subroutine test_version_parsing_errors(error)
        type(error_type), allocatable, intent(out) :: error

        call check_invalid(error, "OTP-18.1.5", "Invalid version syntax: `OTP-18.1.5' (must start with a number)")
        if (allocated(error)) return
        call check_invalid(error, "", "Invalid version syntax: `' (must start with a number)")
        if (allocated(error)) return
        call check_invalid(error, "1.0..7.5", "Invalid version syntax: `1.0..7.5'")
        if (allocated(error)) return
        call check_invalid(error, "1.0prepre2", "Invalid version syntax: `1.0prepre2'")
        if (allocated(error)) return
        call check_invalid(error, "22.8X3", "Invalid version syntax: `22.8X3'")
        if (allocated(error)) return
        call check_invalid(error, "beta22.8alpha3", "Invalid version syntax: `beta22.8alpha3' (must start with a number)")
        if (allocated(error)) return
        call check_invalid(error, "honk", "Invalid version syntax: `honk' (must start with a number)")
    end subroutine test_version_parsing_errors

    !> Port of subr-tests.el:subr-test-version-list-<.
    subroutine test_version_numbers_less(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, is_version_numbers_less([0_int64], [1_int64]), "(0) < (1)")
        if (allocated(error)) return
        call check(error, is_version_numbers_less([0_int64, 9_int64], [1_int64, 0_int64]), "(0 9) < (1 0)")
        if (allocated(error)) return
        call check(error, is_version_numbers_less([1_int64, -1_int64], [1_int64, 0_int64]), "(1 -1) < (1 0)")
        if (allocated(error)) return
        call check(error, is_version_numbers_less([1_int64, -2_int64], [1_int64, -1_int64]), "(1 -2) < (1 -1)")
        if (allocated(error)) return
        call check(error,.not. is_version_numbers_less([1_int64], [0_int64]), "not (1) < (0)")
        if (allocated(error)) return
        call check(error,.not. is_version_numbers_less([1_int64, 1_int64], [1_int64, 0_int64]), "not (1 1) < (1 0)")
        if (allocated(error)) return
        call check(error,.not. is_version_numbers_less([1_int64], [1_int64, 0_int64]), "not (1) < (1 0)")
        if (allocated(error)) return
        call check(error,.not. is_version_numbers_less([1_int64, 0_int64], [1_int64, 0_int64, 0_int64]), &
                   "not (1 0) < (1 0 0)")
    end subroutine test_version_numbers_less

    !> Port of subr-tests.el:subr-test-version-list-<=.
    subroutine test_version_numbers_less_or_equal(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, is_version_numbers_less_or_equal([0_int64], [1_int64]), "(0) <= (1)")
        if (allocated(error)) return
        call check(error, is_version_numbers_less_or_equal([1_int64], [1_int64]), "(1) <= (1)")
        if (allocated(error)) return
        call check(error, is_version_numbers_less_or_equal([1_int64, 0_int64], [1_int64]), "(1 0) <= (1)")
        if (allocated(error)) return
        call check(error,.not. is_version_numbers_less_or_equal([1_int64], [0_int64]), "not (1) <= (0)")
    end subroutine test_version_numbers_less_or_equal

    !> version< and version<= on strings, with the examples of their
    !> docstrings (own test).
    subroutine test_version_compare(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=9), parameter :: lower(9) = [character(len=9) :: "1", "1pre", "1beta", "1alpha", "1snapshot", &
                                                                                                 "1.7-GIT", "4.6", "4.7", "1.7.6.3"]
        character(len=9), parameter :: higher(9) = [character(len=9) :: "1.0.0", "1", "1pre", "1beta", "1alpha", &
                                                                                                     "1.7", "4.7", "4.13", "2.39.2"]
        type(status_type) :: st
        logical :: less
        integer :: k

        do k = 1, size(lower)
            less = is_version_less_or_equal(trim(lower(k)), trim(higher(k)), st)
            call check(error, less .and. st%ok(), trim(lower(k))//" <= "//trim(higher(k)))
            if (allocated(error)) return
            less = is_version_less(trim(higher(k)), trim(lower(k)), st)
            call check(error,.not. less .and. st%ok(), "not "//trim(higher(k))//" < "//trim(lower(k)))
            if (allocated(error)) return
            ! "1" and "1.0.0" are equal.
            less = is_version_less(trim(lower(k)), trim(higher(k)), st)
            call check(error, less .neqv. k == 1, trim(lower(k))//" < "//trim(higher(k)))
            if (allocated(error)) return
        end do

        less = is_version_less("1..2", "3", st)
        call check(error,.not. st%ok(), "invalid syntax")
    end subroutine test_version_compare
end module test_subr_mod
