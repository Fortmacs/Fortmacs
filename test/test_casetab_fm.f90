! test_casetab_fm.f90 -- tests for the case tables (casetab_fm).
! Ports GNU Emacs 31.1 test/src/casefiddle-tests.el in part; only
! casefiddle-tests-case-table and the upcase and downcase part of
! casefiddle-tests-char-casing concern case tables.
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
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_fm, only: status_type
    use handle_fm, only: handle_type
    use buffer_fm, only: get_buffer_create, set_buffer, kill_buffer
    use chartab_fm, only: make_char_table, char_table_ref, char_table_set, char_table_extra_table, &
                          set_char_table_extra_table, copy_char_table, free_char_table
    use casetab_fm, only: downcase, upcase, case_canon, is_uppercase, is_lowercase, is_case_table, current_case_table, &
                          standard_case_table, set_case_table
    use uni_case_fm, only: canon_count, canon_keys, canon_values
    implicit none(type, external)
    private

    public :: collect_casetab_fm

contains

    !> Collect all exported unit tests.
    subroutine collect_casetab_fm(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("casefiddle-tests-case-table", test_case_table), &
                    new_unittest("casefiddle-tests-char-casing", test_char_casing), &
                    new_unittest("case-canon", test_canon), &
                    new_unittest("case-canon-standard-table", test_canon_table), &
                    new_unittest("case-table-p", test_is_case_table), &
                    new_unittest("set-case-table", test_set_case_table) &
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
        type(handle_type) :: down, up
        type(status_type) :: st

        down = current_case_table()
        up = char_table_extra_table(down, 0, st)

        do k = 1, size(tests, 2)
            call check(error, char_table_ref(up, tests(1, k)) == tests(2, k), "uppercase")
            if (allocated(error)) return
            call check(error, char_table_ref(down, tests(1, k)) == tests(3, k), "lowercase")
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
        call check(error, is_uppercase(931_int32) .and. is_lowercase(962_int32) .and. &
                   .not. is_uppercase(49_int32) .and. .not. is_lowercase(49_int32), "predicates")
    end subroutine test_canon

    !> The canonicalize table `set_case_table' computes for the standard
    !> table agrees with the generated one.
    subroutine test_canon_table(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: down, canon
        type(status_type) :: st
        integer(int64) :: v
        integer :: c, k, expected

        down = standard_case_table()
        canon = char_table_extra_table(down, 1, st)
        k = 1

        do c = 128, int(z'1FFFF')
            expected = c

            if (k <= canon_count) then
                if (canon_keys(k) == c) then
                    expected = canon_values(k)
                    k = k + 1
                end if
            end if

            v = char_table_ref(canon, c)
            if (v < 0) v = c
            call check(error, v == expected .and. case_canon(c) == expected, "canon")
            if (allocated(error)) return
        end do
    end subroutine test_canon_table

    subroutine test_is_case_table(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: tbl, other
        type(status_type) :: st

        call check(error, is_case_table(standard_case_table()))
        if (allocated(error)) return
        call make_char_table(tbl, "case_table", st)
        call check(error, is_case_table(tbl))
        if (allocated(error)) return
        call make_char_table(other, "foo", st)
        call check(error,.not. is_case_table(other))
        if (allocated(error)) return
        ! An EQUIVALENCES table without a CANONICALIZE table.
        call set_char_table_extra_table(tbl, 2, other, st)
        call check(error,.not. is_case_table(tbl))
        call free_char_table(other, st)
        call free_char_table(tbl, st)
    end subroutine test_is_case_table

    subroutine test_set_case_table(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, tbl, cur_table
        type(status_type) :: st

        call get_buffer_create(" *casetab-test*", h, st)
        call set_buffer(h, st)
        ! A table where a and b are one case pair (b is a's upper case).
        call make_char_table(tbl, "case_table", st)
        call char_table_set(tbl, iachar("a"), int(iachar("a"), int64), st)
        call char_table_set(tbl, iachar("b"), int(iachar("a"), int64), st)
        call set_case_table(tbl, st)
        call check(error, st%ok())
        if (allocated(error)) return
        cur_table = current_case_table()
        call check(error, cur_table%index == tbl%index)
        if (allocated(error)) return
        call check(error, downcase(iachar("b")) == iachar("a") .and. upcase(iachar("a")) == iachar("b"))
        if (allocated(error)) return
        call check(error, case_canon(iachar("b")) == iachar("a") .and. downcase(iachar("B")) == iachar("B"))
        if (allocated(error)) return
        call check(error, is_uppercase(iachar("b")) .and. is_lowercase(iachar("a")))
        if (allocated(error)) return
        call kill_buffer(h, st)
        ! Other buffers keep the standard table.
        call check(error, downcase(iachar("B")) == iachar("b"))
    end subroutine test_set_case_table
end module test_casetab_fm
