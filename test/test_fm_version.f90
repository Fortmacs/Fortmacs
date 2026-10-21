! test_fm_version.f90 -- tests for fm_version.
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

module test_fm_version
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use fm_version, only: fortmacs_version, upstream_version, version_string
    implicit none(type, external)
    private

    public :: collect_fm_version

contains

    subroutine collect_fm_version(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("upstream-is-31.1", test_upstream), &
                    new_unittest("banner-contents", test_banner) &
                    ]
    end subroutine collect_fm_version

    subroutine test_upstream(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, upstream_version, "31.1")
    end subroutine test_upstream

    subroutine test_banner(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: s

        s = version_string()
        call check(error, index(s, fortmacs_version) > 0, &
                   "banner lacks Fortmacs version")
        if (allocated(error)) return
        call check(error, index(s, "GNU Emacs "//upstream_version) > 0, &
                   "banner lacks upstream version")
    end subroutine test_banner

end module test_fm_version
