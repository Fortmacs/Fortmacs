! test_category_fm.f90 -- tests for categories (category_fm).
! Own tests; GNU Emacs 31.1 has no test/src/category-tests.el.
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

!> Category sets, the standard category table, defining categories,
!> `modify_category_entry', copies and buffer-local category tables.
module test_category_fm
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_fm, only: status_type
    use handle_fm, only: handle_type
    use buffer_fm, only: get_or_make_buffer, set_buffer, close_buffer
    use category_fm, only: category_set_type, make_category_set, define_category, category_docstring, &
                           get_unused_category, is_category_table, category_table, standard_category_table, &
                           copy_category_table, make_category_table, set_category_table, char_category_set, &
                           category_set_mnemonics, modify_category_entry, char_has_category
    implicit none(type, external)
    private

    public :: collect_category_fm

contains

    !> Collect all exported unit tests.
    subroutine collect_category_fm(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("category-set", test_category_set), &
                    new_unittest("category-standard-table", test_standard), &
                    new_unittest("category-define-modify", test_define_modify), &
                    new_unittest("category-buffer-table", test_buffer_table) &
                    ]
    end subroutine collect_category_fm

    subroutine test_category_set(error)
        type(error_type), allocatable, intent(out) :: error
        type(category_set_type) :: s
        type(status_type) :: st

        call make_category_set("|aC", s, st)
        call check(error, st%ok() .and. category_set_mnemonics(s) == "Ca|")
        if (allocated(error)) return
        call make_category_set(achar(127), s, st)
        call check(error,.not. st%ok())
    end subroutine test_category_set

    subroutine test_standard(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        logical :: defined
        character(len=:), allocatable :: doc

        call check(error, is_category_table(standard_category_table()))
        if (allocated(error)) return
        ! 漢 is han and line breakable; あ hiragana; a Latin; U+0301 combining.
        call check(error, category_set_mnemonics(char_category_set(int(z'6F22'))) == "C|")
        if (allocated(error)) return
        call check(error, char_has_category(int(z'3042'), "H") .and. .not. char_has_category(int(z'3042'), "K"))
        if (allocated(error)) return
        call check(error, category_set_mnemonics(char_category_set(iachar("a"))) == "l")
        if (allocated(error)) return
        call check(error, char_has_category(int(z'301'), "^") .and. .not. char_has_category(int(z'41'), "^"))
        if (allocated(error)) return
        doc = category_docstring("|", st, defined=defined)
        call check(error, defined .and. doc(1:14) == "line breakable")
        if (allocated(error)) return
        doc = category_docstring("Z", st, defined=defined)
        call check(error,.not. defined)
    end subroutine test_standard

    subroutine test_define_modify(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: tbl, copy
        type(status_type) :: st

        tbl = make_category_table()
        call check(error, is_category_table(tbl) .and. get_unused_category(st, tbl) == " ")
        if (allocated(error)) return
        call define_category("x", "Test", st, tbl)
        call check(error, st%ok())
        if (allocated(error)) return
        call define_category("x", "Again", st, tbl)
        call check(error,.not. st%ok())
        if (allocated(error)) return
        ! Undefined categories can't be given.
        call modify_category_entry(65, "y", st, table=tbl)
        call check(error,.not. st%ok())
        if (allocated(error)) return
        call modify_category_entry(65, "x", st, to=90, table=tbl)
        call copy_category_table(copy, st, tbl)
        call modify_category_entry(70, "x", st, to=72, table=tbl, reset=.true.)
        call check(error, category_docstring("x", st, copy) == "Test")
    end subroutine test_define_modify

    subroutine test_buffer_table(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, tbl, current_table
        type(status_type) :: st

        call get_or_make_buffer(" *category-test*", h, st)
        call set_buffer(h, st)
        tbl = make_category_table()
        call define_category("x", "Test", st, tbl)
        call modify_category_entry(65, "x", st, to=90, table=tbl)
        call modify_category_entry(70, "x", st, to=72, table=tbl, reset=.true.)
        call set_category_table(tbl, st)
        current_table = category_table()
        call check(error, current_table%index == tbl%index)
        if (allocated(error)) return
        call check(error, char_has_category(65, "x") .and. .not. char_has_category(70, "x") .and. &
                   char_has_category(73, "x") .and. .not. char_has_category(91, "x"))
        if (allocated(error)) return
        ! The standard table's categories are not in it.
        call check(error,.not. char_has_category(int(z'6F22'), "C"))
        if (allocated(error)) return
        call close_buffer(h, st)
        call check(error, char_has_category(int(z'6F22'), "C"))
    end subroutine test_buffer_table
end module test_category_fm
