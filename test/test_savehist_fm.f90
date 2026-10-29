! test_savehist_fm.f90 -- tests for savehist_fm.
! Ported from GNU Emacs 31.1 test/lisp/savehist-tests.el.
!
! Copyright (C) 2025-2026 Free Software Foundation, Inc.
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

!> Ports savehist-test-saved-variables and
!> savehist-test-duplicated-saved-symbols (counting the objects of the
!> namelist file instead of its setq forms).
module test_savehist_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_fm, only: status_type, format_error
    use handle_fm, only: handle_type
    use lisp_fm, only: value_type, list, make_string, nil, nilp
    use lread_fm, only: sym
    use data_fm, only: set, symbol_value
    use fns_fm, only: value_equal
    use subr_fm, only: defvar_nil
    use files_fm, only: make_temp_file, delete_file
    use bindings_fm, only: init_bindings
    use modes_fm, only: minor_modes
    use minibuf_fm, only: minibuffer_history_variable
    use buffer_fm, only: get_buffer_create, set_buffer, kill_buffer, current_buffer, generate_new_buffer_name
    use editfns_fm, only: buffer_string
    use fileio_fm, only: insert_file_contents
    use savehist_fm, only: savehist_file, savehist_save_minibuffer_history, savehist_loaded, &
                           savehist_minibuffer_history_variables, savehist_additional_variables, &
                           savehist_ignored_variables, savehist_variable_type, savehist_save, &
                           savehist_minibuffer_hook
    implicit none(type, external)
    private

    public :: collect_savehist_fm

contains

    subroutine collect_savehist_fm(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)
        testsuite = [ &
                    new_unittest("savehist-test-saved-variables", test_saved_variables), &
                    new_unittest("savehist-test-duplicated-saved-symbols", test_duplicated) &
                    ]
    end subroutine collect_savehist_fm

    !> The common setup: a fresh history file and test variables.
    subroutine setup(tmpfile, ignore_t3)
        character(len=:), allocatable, intent(out) :: tmpfile
        logical, intent(in) :: ignore_t3
        type(status_type) :: st
        call init_bindings()
        call make_temp_file("savehist-test", tmpfile, st)
        savehist_file = tmpfile
        savehist_save_minibuffer_history = .true.
        savehist_loaded = .false.
        if (allocated(savehist_minibuffer_history_variables)) deallocate (savehist_minibuffer_history_variables)
        allocate (savehist_minibuffer_history_variables(0))
        savehist_additional_variables = [savehist_variable_type(name="savehist_tests__t2")]
        if (ignore_t3) then
            savehist_ignored_variables = [character(len=64) :: "t3"]
        else
            if (allocated(savehist_ignored_variables)) deallocate (savehist_ignored_variables)
            allocate (savehist_ignored_variables(0))
        end if
        call defvar_nil("savehist_tests__t1")
        call defvar_nil("savehist_tests__t2")
        call defvar_nil("t3")
        call set(sym("savehist_tests__t1"), list(make_string("t1-value")), st)
        call set(sym("savehist_tests__t2"), list(make_string("t2-value")), st)
        call set(sym("t3"), list(make_string("t3-value")), st)
    end subroutine setup

    subroutine save_var(name)
        character(len=*), intent(in) :: name
        minibuffer_history_variable = name
        call savehist_minibuffer_hook()
    end subroutine save_var

    !> Force reloading the file.
    subroutine reload()
        type(status_type) :: st
        call minor_modes%savehist_mode%disable(st)
        savehist_loaded = .false.
        call minor_modes%savehist_mode%enable(st)
    end subroutine reload

    subroutine teardown(tmpfile)
        character(len=*), intent(in) :: tmpfile
        type(status_type) :: st
        call minor_modes%savehist_mode%disable(st)
        call delete_file(tmpfile, st)
        if (allocated(savehist_additional_variables)) deallocate (savehist_additional_variables)
        allocate (savehist_additional_variables(0))
        if (allocated(savehist_ignored_variables)) deallocate (savehist_ignored_variables)
        allocate (savehist_ignored_variables(0))
        if (allocated(savehist_minibuffer_history_variables)) deallocate (savehist_minibuffer_history_variables)
        allocate (savehist_minibuffer_history_variables(0))
        savehist_loaded = .false.
    end subroutine teardown

    function value_of(name) result(v)
        character(len=*), intent(in) :: name
        type(value_type) :: v
        type(status_type) :: st
        call symbol_value(sym(name), v, st)
    end function value_of

    subroutine test_saved_variables(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: tmpfile
        type(status_type) :: st
        type(value_type) :: t1_copy, t2_copy
        call setup(tmpfile, .true.)
        t1_copy = value_of("savehist_tests__t1")
        t2_copy = value_of("savehist_tests__t2")
        call minor_modes%savehist_mode%enable(st)
        call save_var("savehist_tests__t1")
        call save_var("savehist_tests__t2")
        ! Should be ignored.
        call save_var("t3")
        call savehist_save(st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call set(sym("savehist_tests__t1"), nil(), st)
        call set(sym("savehist_tests__t2"), nil(), st)
        call set(sym("t3"), nil(), st)
        call reload()
        call check(error, value_equal(value_of("savehist_tests__t1"), t1_copy), "t1")
        if (allocated(error)) return
        call check(error, value_equal(value_of("savehist_tests__t2"), t2_copy), "t2")
        if (allocated(error)) return
        call check(error, nilp(value_of("t3")), "t3")
        call teardown(tmpfile)
    end subroutine test_saved_variables

    !> `savehist_tests__t2' should not be saved twice.
    subroutine test_duplicated(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: tmpfile, text
        type(status_type) :: st
        type(handle_type) :: old, h
        integer(int64) :: inserted
        integer :: n1, n2
        call setup(tmpfile, .false.)
        call minor_modes%savehist_mode%enable(st)
        call save_var("savehist_tests__t1")
        call save_var("savehist_tests__t2")
        call savehist_save(st)
        call reload()
        old = current_buffer()
        call get_buffer_create(generate_new_buffer_name(" *savehist-test*"), h, st)
        call set_buffer(h, st)
        call insert_file_contents(tmpfile, .false., inserted, st)
        text = buffer_string()
        call kill_buffer(h, st)
        call set_buffer(old, st)
        n1 = count_lines_starting(text, " savehist_tests__t1(")
        n2 = count_lines_starting(text, " savehist_tests__t2(")
        call check(error, n1 == 1 .and. n2 == 1, text)
        call teardown(tmpfile)
    end subroutine test_duplicated

    !> The number of lines of TEXT beginning with PREFIX.
    integer function count_lines_starting(text, prefix) result(n)
        character(len=*), intent(in) :: text, prefix
        integer :: start, k
        n = 0
        start = 1
        do k = 1, len(text) + 1
            if (k > len(text)) then
                call try(start, k - 1)
            else if (text(k:k) == achar(10)) then
                call try(start, k - 1)
                start = k + 1
            end if
        end do
    contains
        subroutine try(a, b)
            integer, intent(in) :: a, b
            if (b - a + 1 >= len(prefix)) then
                if (text(a:a + len(prefix) - 1) == prefix) n = n + 1
            end if
        end subroutine try
    end function count_lines_starting
end module test_savehist_fm
