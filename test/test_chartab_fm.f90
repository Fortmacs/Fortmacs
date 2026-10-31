! test_chartab_fm.f90 -- tests for char-tables (chartab_fm).
! Ports GNU Emacs 31.1 test/src/chartab-tests.el.
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

!> Ports test/src/chartab-tests.el (a value 1 stands for t, a handle
!> that was never a char-table for the hash table); own tests for
!> parents, `char_table_range', `map_char_table' and
!> `optimize_char_table'.
module test_chartab_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_fm, only: status_type
    use handle_fm, only: handle_type, null_handle
    use character_fm, only: MAX_CHAR
    use chartab_fm, only: make_char_table, free_char_table, char_table_p, char_table_subtype, char_table_parent, &
                          set_char_table_parent, char_table_extra_slot, set_char_table_extra_slot, char_table_range, &
                          set_char_table_range, char_table_ref, char_table_set, map_char_table, optimize_char_table, &
                          copy_char_table, NO_VALUE
    implicit none(type, external)
    private

    public :: collect_chartab_fm

    !> The runs `record_run' has seen.
    integer :: nruns = 0
    integer :: run_from(16), run_to(16)
    integer(int64) :: run_val(16)

contains

    !> Collect all exported unit tests.
    subroutine collect_chartab_fm(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("chartab-test-range-setting", test_range_setting), &
                    new_unittest("chartab-test-char-table-p", test_char_table_p), &
                    new_unittest("chartab-test-char-table-subtype", test_subtype), &
                    new_unittest("chartab-test-char-table-parent", test_parent), &
                    new_unittest("chartab-test-char-table-extra-slot", test_extra_slot), &
                    new_unittest("chartab-inherit", test_inherit), &
                    new_unittest("chartab-range", test_range), &
                    new_unittest("chartab-map", test_map), &
                    new_unittest("chartab-optimize-copy", test_optimize_copy) &
                    ]
    end subroutine collect_chartab_fm

    !> chartab-set-and-test.
    subroutine set_and_test(error, from, to)
        type(error_type), allocatable, intent(out) :: error
        integer, intent(in) :: from, to
        type(handle_type) :: tbl
        type(status_type) :: status

        call make_char_table(tbl, "nil", status)
        call set_char_table_range(tbl, 1_int64, status, from, to)
        call check(error, char_table_ref(tbl, from) == 1)
        if (allocated(error)) return
        call check(error, char_table_ref(tbl, to) == 1)
        if (allocated(error)) return
        call check(error, char_table_ref(tbl, (from + to)/2) == 1)
        if (allocated(error)) return

        if (to < MAX_CHAR) then
            call check(error, char_table_ref(tbl, to + 1) /= 1)
            if (allocated(error)) return
        end if

        if (from > 0) then
            call check(error, char_table_ref(tbl, from - 1) /= 1)
            if (allocated(error)) return
        end if

        call free_char_table(tbl, status)
    end subroutine set_and_test

    subroutine test_range_setting(error)
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: ranges(2, 8) = reshape([0, 127, 128, 256, int(z'1000'), int(z'1fff'), &
                                                      int(z'1001'), int(z'2000'), int(z'10000'), int(z'20000'), &
                                                      int(z'10001'), int(z'1ffff'), int(z'20000'), int(z'30000'), &
                                                      int(z'e0e00'), int(z'e0ef6')], [2, 8])
        integer :: k

        do k = 1, size(ranges, 2)
            call set_and_test(error, ranges(1, k), ranges(2, k))
            if (allocated(error)) return
        end do
    end subroutine test_range_setting

    subroutine test_char_table_p(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: tbl
        type(status_type) :: status

        call make_char_table(tbl, "foo", status)
        call check(error, char_table_p(tbl))
        if (allocated(error)) return
        call check(error,.not. char_table_p(null_handle()))
        call free_char_table(tbl, status)
    end subroutine test_char_table_p

    subroutine test_subtype(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: tbl
        type(status_type) :: status

        call make_char_table(tbl, "foo", status)
        call check(error, char_table_subtype(tbl, status) == "foo")
        call free_char_table(tbl, status)
    end subroutine test_subtype

    subroutine test_parent(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: parent, child, h
        type(status_type) :: status

        call make_char_table(parent, "foo", status)
        call make_char_table(child, "bar", status)
        h = char_table_parent(parent, status)
        call check(error, h%index == 0)
        if (allocated(error)) return
        call set_char_table_parent(child, parent, status)
        h = char_table_parent(child, status)
        call check(error, h%index == parent%index .and. h%generation == parent%generation)
        if (allocated(error)) return
        ! No cycles.
        call set_char_table_parent(parent, child, status)
        call check(error,.not. status%ok())
        call free_char_table(child, status)
        call free_char_table(parent, status)
    end subroutine test_parent

    subroutine test_extra_slot(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: tbl
        type(status_type) :: status

        ! Use any type with extra slots, e.g. case_table.
        call make_char_table(tbl, "case_table", status)
        call set_char_table_extra_slot(tbl, 1, 42_int64, status)
        call check(error, char_table_extra_slot(tbl, 1, status) == 42)
        if (allocated(error)) return
        call check(error, char_table_extra_slot(tbl, 3, status) == NO_VALUE .and. .not. status%ok())
        call free_char_table(tbl, status)
    end subroutine test_extra_slot

    subroutine test_inherit(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: parent, child
        type(status_type) :: status

        call make_char_table(parent, "foo", status)
        call make_char_table(child, "foo", status)
        call set_char_table_parent(child, parent, status)
        call char_table_set(parent, iachar("a"), 5_int64, status)
        call set_char_table_range(parent, 7_int64, status)
        call char_table_set(child, iachar("b"), 6_int64, status)
        call check(error, char_table_ref(child, iachar("a")) == 5)
        if (allocated(error)) return
        call check(error, char_table_ref(child, iachar("b")) == 6)
        if (allocated(error)) return
        ! The parent's default value.
        call check(error, char_table_ref(child, int(z'4e00')) == 7)
        if (allocated(error)) return
        ! The child's default comes before the parent.
        call set_char_table_range(child, 8_int64, status)
        call check(error, char_table_ref(child, iachar("a")) == 8)
        if (allocated(error)) return
        call check(error, char_table_range(child, status) == 8)
        call free_char_table(child, status)
        call free_char_table(parent, status)
    end subroutine test_inherit

    subroutine test_range(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: tbl
        type(status_type) :: status

        call make_char_table(tbl, "foo", status, init=3_int64)
        call check(error, char_table_ref(tbl, int(z'10000')) == 3)
        if (allocated(error)) return
        call set_char_table_range(tbl, 9_int64, status, all=.true.)
        call check(error, char_table_ref(tbl, 0) == 9 .and. char_table_ref(tbl, MAX_CHAR) == 9)
        if (allocated(error)) return
        call set_char_table_range(tbl, 4_int64, status, 100, 200)
        call check(error, char_table_range(tbl, status, 150, 300) == 4)
        if (allocated(error)) return
        call check(error, char_table_range(tbl, status, 99) == 9)
        if (allocated(error)) return
        call check(error, char_table_range(tbl, status, -1) == NO_VALUE .and. .not. status%ok())
        call free_char_table(tbl, status)
    end subroutine test_range

    !> Record a run of `map_char_table'.
    recursive subroutine record_run(arg, from, to, value)
        type(handle_type), intent(in) :: arg
        integer, intent(in) :: from, to
        integer(int64), intent(in) :: value

        if (arg%index < 0) return
        nruns = nruns + 1
        if (nruns > size(run_from)) return
        run_from(nruns) = from
        run_to(nruns) = to
        run_val(nruns) = value
    end subroutine record_run

    subroutine test_map(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: parent, tbl
        type(status_type) :: status

        call make_char_table(tbl, "foo", status)
        call set_char_table_range(tbl, 1_int64, status, iachar("a"), iachar("z"))
        call char_table_set(tbl, iachar("m"), 2_int64, status)
        call set_char_table_range(tbl, 1_int64, status, int(z'3000'), int(z'30ff'))
        nruns = 0
        call map_char_table(record_run, tbl, status)
        call check(error, nruns == 4)
        if (allocated(error)) return
        call check(error, all(run_from(1:4) == [97, 109, 110, int(z'3000')]) .and. &
                   all(run_to(1:4) == [108, 109, 122, int(z'30ff')]) .and. all(run_val(1:4) == [1, 2, 1, 1]))
        if (allocated(error)) return
        ! A parent fills what the table leaves unset.
        call make_char_table(parent, "foo", status)
        call set_char_table_range(parent, 5_int64, status, 0, 127)
        call set_char_table_parent(tbl, parent, status)
        nruns = 0
        call map_char_table(record_run, tbl, status)
        call check(error, nruns == 6)
        if (allocated(error)) return
        call check(error, all(run_from(1:6) == [0, 97, 109, 110, 123, int(z'3000')]) .and. &
                   all(run_to(1:6) == [96, 108, 109, 122, 127, int(z'30ff')]) .and. &
                   all(run_val(1:6) == [5, 1, 2, 1, 5, 1]))
        call free_char_table(tbl, status)
        call free_char_table(parent, status)
    end subroutine test_map

    subroutine test_optimize_copy(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: tbl, copy
        type(status_type) :: status
        integer :: c

        call make_char_table(tbl, "foo", status)

        do c = 0, 255
            call char_table_set(tbl, c, 7_int64, status)
        end do

        call optimize_char_table(tbl, status)
        call check(error, status%ok() .and. char_table_ref(tbl, 65) == 7 .and. char_table_ref(tbl, 256) == NO_VALUE)
        if (allocated(error)) return
        call copy_char_table(tbl, copy, status)
        call char_table_set(copy, 65, 8_int64, status)
        call check(error, char_table_ref(tbl, 65) == 7 .and. char_table_ref(copy, 65) == 8 .and. &
                   char_table_subtype(copy, status) == "foo")
        call free_char_table(copy, status)
        call free_char_table(tbl, status)
    end subroutine test_optimize_copy
end module test_chartab_fm
