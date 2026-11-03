! test_env_mod.f90 -- tests for env_mod.
! Ports GNU Emacs 31.1 test/lisp/env-tests.el in part;
! test-substitute-env-in-file-name and test-with-environment-variables
! need substitute-env-in-file-name and with-environment-variables, which
! are not ported.
!
! Copyright (C) 2022-2026 Free Software Foundation, Inc.
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

module test_env_mod
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_mod, only: status_type, format_error
    use env_mod, only: getenv, setenv, substitute_env_vars
    implicit none(type, external)
    private

    public :: collect_env_mod

contains

    !> Collect all exported unit tests.
    subroutine collect_env_mod(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("test-getenv-setenv", test_getenv_setenv), &
                    new_unittest("setenv-remove", test_setenv_remove), &
                    new_unittest("setenv-equals-sign", test_setenv_equals_sign), &
                    new_unittest("setenv-substitute-env-vars", test_setenv_substitute) &
                    ]
    end subroutine collect_env_mod

    !> Port of env-tests.el:test-getenv-setenv.
    subroutine test_getenv_setenv(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: value
        logical :: found

        call setenv("EMACS_ENV_EL_TEST_VAR", "foobar")
        call getenv("EMACS_ENV_EL_TEST_VAR", value, found)
        call check(error, found)
        if (.not. allocated(error)) call check(error, value, "foobar")

        if (.not. allocated(error)) then
            call getenv("LIKELY_TO_BE_NON_EXISTENT_FOO_BAR_BAZ", value, found)
            call check(error,.not. found)
        end if

        call setenv("EMACS_ENV_EL_TEST_VAR")
    end subroutine test_getenv_setenv

    !> setenv without a value removes the variable, and a new value
    !> replaces the old one.
    subroutine test_setenv_remove(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: value
        logical :: found
        integer :: stat

        call setenv("FORTMACS_ENV_TEST", "one")
        call setenv("FORTMACS_ENV_TEST", "two")
        call getenv("FORTMACS_ENV_TEST", value, found)
        call check(error, value, "two")

        if (.not. allocated(error)) then
            call setenv("FORTMACS_ENV_TEST")
            call getenv("FORTMACS_ENV_TEST", value, found)
            call check(error,.not. found)
        end if

        if (.not. allocated(error)) call check(error, value, "")

        if (.not. allocated(error)) then
            ! The editor's own environment follows (status 1: not set).
            call get_environment_variable("FORTMACS_ENV_TEST", status=stat)
            call check(error, stat, 1)
        end if
    end subroutine test_setenv_remove

    !> A variable name with `=' is an error.
    subroutine test_setenv_equals_sign(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st

        call setenv("A=B", "c", st)
        call check(error, format_error(st), "Environment variable name ‘A=B’ contains ‘=’")
    end subroutine test_setenv_equals_sign

    !> getenv is what substitute_env_vars sees.
    subroutine test_setenv_substitute(error)
        type(error_type), allocatable, intent(out) :: error

        call setenv("FORTMACS_ENV_TEST", "x y")
        call check(error, substitute_env_vars("<$FORTMACS_ENV_TEST>", .true.), "<x y>")
        call setenv("FORTMACS_ENV_TEST")

        if (.not. allocated(error)) then
            call check(error, substitute_env_vars("<$FORTMACS_ENV_TEST>", .true.), "<$FORTMACS_ENV_TEST>")
        end if
    end subroutine test_setenv_substitute
end module test_env_mod
