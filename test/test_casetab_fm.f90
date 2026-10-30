! test_casetab_fm.f90 -- tests for the case tables (casetab_fm).
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

!> Ports test/src/casefiddle-tests.el casefiddle-tests-case-table and
!> the upcase/downcase part of casefiddle-tests-char-casing; the
!> canonicalize table is checked on its own.
module test_casetab_fm
    use, intrinsic :: iso_fortran_env, only: int32
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use casetab_fm, only: downcase, upcase, case_canon, uppercasep, lowercasep
    implicit none(type, external)
    private

    public :: collect_casetab_fm

contains

    subroutine collect_casetab_fm(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("casefiddle-tests-case-table", test_case_table), &
                    new_unittest("casefiddle-tests-char-casing", test_char_casing), &
                    new_unittest("case-canon", test_canon) &
                    ]
    end subroutine collect_casetab_fm

    subroutine test_case_table(error)
        type(error_type), allocatable, intent(out) :: error
        ! casefiddle-tests--characters: character, uppercase, lowercase.
        integer(int32), parameter :: tests(3, 12) = reshape([ &
                                                            65, 65, 97, 97, 65, 97, &
                                                            321, 321, 322, 322, 321, 322, &
                                                            452, 452, 454, 453, 452, 454, 454, 452, 454, &
                                                            931, 931, 963, 963, 931, 963, 962, 931, 962, &
                                                            8551, 8551, 8567, 8567, 8551, 8567], [3, 12])
        integer :: k

        do k = 1, size(tests, 2)
            call check(error, upcase(tests(1, k)) == tests(2, k), "upcase")
            if (allocated(error)) return
            call check(error, downcase(tests(1, k)) == tests(3, k), "downcase")
            if (allocated(error)) return
        end do
    end subroutine test_case_table

    subroutine test_char_casing(error)
        type(error_type), allocatable, intent(out) :: error
        ! input, upcase, downcase.  We char-upcase ß to ẞ; see bug #11309.
        integer(int32), parameter :: tests(3, 11) = reshape([ &
                                                            97, 65, 97, 65, 65, 97, &
                                                            322, 321, 322, 321, 321, 322, &
                                                            223, 7838, 223, 7838, 7838, 223, &
                                                            8567, 8551, 8567, 8551, 8551, 8567, &
                                                            452, 452, 454, 453, 452, 454, 454, 452, 454], [3, 11])
        integer :: k

        do k = 1, size(tests, 2)
            call check(error, upcase(tests(1, k)) == tests(2, k) .and. downcase(tests(1, k)) == tests(3, k))
            if (allocated(error)) return
        end do
    end subroutine test_char_casing

    subroutine test_canon(error)
        type(error_type), allocatable, intent(out) :: error

        ! σ, ς and Σ are one class; so are a and A; K (Kelvin) is not k.
        call check(error, case_canon(962_int32) == 963 .and. case_canon(931_int32) == 963 &
                   .and. case_canon(963_int32) == 963, "sigma")
        if (allocated(error)) return
        call check(error, case_canon(65_int32) == 97 .and. case_canon(97_int32) == 97, "ASCII")
        if (allocated(error)) return
        call check(error, case_canon(8490_int32) == 8490 .and. downcase(8490_int32) == 8490, "Kelvin sign")
        if (allocated(error)) return
        call check(error, uppercasep(931_int32) .and. lowercasep(962_int32) .and. &
                   .not. uppercasep(49_int32) .and. .not. lowercasep(49_int32), "predicates")
    end subroutine test_canon
end module test_casetab_fm
