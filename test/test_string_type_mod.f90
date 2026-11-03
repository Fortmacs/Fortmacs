! test_string_type_mod.f90 -- tests for string_type_mod.
! Own tests; string_type_mod ports nothing.
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

module test_string_type_mod
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use string_type_mod, only: string_type, append_string, prepend_string
    implicit none(type, external)
    private

    public :: collect_string_type_mod

contains

    !> Collect all exported unit tests.
    subroutine collect_string_type_mod(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("append-to-unallocated", test_append_unallocated), &
                    new_unittest("append-several", test_append_several), &
                    new_unittest("prepend", test_prepend) &
                    ]
    end subroutine collect_string_type_mod

    subroutine test_append_unallocated(error)
        type(error_type), allocatable, intent(out) :: error
        type(string_type), allocatable :: list(:)

        call append_string(list, "")
        call check(error, size(list), 1)
        if (allocated(error)) return
        call check(error, len(list(1)%text), 0)
    end subroutine test_append_unallocated

    subroutine test_append_several(error)
        type(error_type), allocatable, intent(out) :: error
        type(string_type), allocatable :: list(:)

        call append_string(list, "sh")
        call append_string(list, "-c", "echo a b", "x")
        call check(error, size(list), 4)
        if (allocated(error)) return
        call check(error, list(1)%text, "sh")
        if (allocated(error)) return
        call check(error, list(3)%text, "echo a b")
        if (allocated(error)) return
        call check(error, list(4)%text, "x")
    end subroutine test_append_several

    subroutine test_prepend(error)
        type(error_type), allocatable, intent(out) :: error
        type(string_type), allocatable :: list(:)

        call prepend_string(list, "b")
        call prepend_string(list, "a")
        call check(error, size(list), 2)
        if (allocated(error)) return
        call check(error, list(1)%text//list(2)%text, "ab")
    end subroutine test_prepend
end module test_string_type_mod
