! test_ewoc_mod.f90 -- tests for ewoc_mod.
! Own tests; upstream has no ewoc tests.
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

!> Own tests of the ewoc (upstream has none): entering, locating,
!> walking, refreshing, filtering and collecting elements, and the
!> header and footer.
module test_ewoc_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_mod, only: status_type
    use handle_mod, only: handle_type
    use buffer_mod, only: get_or_make_buffer, set_buffer, close_buffer, erase_buffer
    use insdel_mod, only: insert
    use editfns_mod, only: buffer_string, goto_char, point, point_min
    use ewoc_mod, only: ewoc_type, make_ewoc, destroy_ewoc, ewoc_enter_last, ewoc_enter_first, ewoc_enter_after, &
                        ewoc_data, ewoc_set_data, ewoc_next, ewoc_prev, ewoc_nth, ewoc_locate, ewoc_location, &
                        ewoc_invalidate, ewoc_delete, ewoc_filter, ewoc_collect, ewoc_map, ewoc_goto_next, &
                        ewoc_goto_prev, ewoc_set_hf, ewoc_get_hf, ewoc_refresh
    implicit none(type, external)
    private

    public :: collect_ewoc_mod

    character(len=*), parameter :: LF = achar(10)

    ! The "text" of each element: element N prints as "item N".
    integer(int64) :: doubled = 0

contains

    !> Collect all exported unit tests.
    subroutine collect_ewoc_mod(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("ewoc-enter-and-walk", test_enter), &
                    new_unittest("ewoc-locate-and-goto", test_locate), &
                    new_unittest("ewoc-invalidate-delete-filter", test_change), &
                    new_unittest("ewoc-header-footer-nosep", test_hf) &
                    ]
    end subroutine collect_ewoc_mod

    recursive subroutine pp(data)
        integer(int64), intent(in) :: data
        type(status_type) :: st
        character(len=24) :: buffer

        write (buffer, "(i0)") data + doubled
        call insert("item "//trim(buffer), st)
    end subroutine pp

    recursive logical function odd(data)
        integer(int64), intent(in) :: data

        odd = mod(data, 2_int64) == 1
    end function odd

    recursive logical function above_two(data)
        integer(int64), intent(in) :: data

        above_two = data > 2
    end function above_two

    subroutine temp_buffer(h)
        type(handle_type), intent(out) :: h
        type(status_type) :: st

        call get_or_make_buffer(" *ewoc-test*", h, st)
        call set_buffer(h, st)
        call erase_buffer(st)
        doubled = 0
    end subroutine temp_buffer

    subroutine test_enter(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(ewoc_type) :: e
        type(status_type) :: st
        integer :: n1, n2, n3

        call temp_buffer(h)
        call insert("before"//LF, st)
        e = make_ewoc(pp, "head", "foot")
        call check(error, buffer_string(), "before"//LF//"head"//LF//"foot"//LF)
        if (allocated(error)) return
        n2 = ewoc_enter_last(e, 2_int64)
        n3 = ewoc_enter_last(e, 3_int64)
        n1 = ewoc_enter_first(e, 1_int64)
        call check(error, buffer_string(), "before"//LF//"head"//LF//"item 1"//LF//"item 2"//LF//"item 3"//LF//"foot"//LF)
        if (allocated(error)) return
        call check(error, ewoc_nth(e, 0) == n1 .and. ewoc_nth(e, -1) == n3 .and. ewoc_nth(e, 3) == 0, "nth")
        if (allocated(error)) return
        call check(error, ewoc_next(e, n1) == n2 .and. ewoc_next(e, n3) == 0 .and. ewoc_prev(e, n1) == 0, "next/prev")
        if (allocated(error)) return
        call check(error, ewoc_data(e, n2) == 2 .and. ewoc_location(e, n2) == 20, "data and location")
        if (allocated(error)) return
        n2 = ewoc_enter_after(e, n2, 4_int64)
        call check(error, buffer_string(), "before"//LF//"head"//LF//"item 1"//LF//"item 2"//LF//"item 4"//LF// &
                   "item 3"//LF//"foot"//LF)
        call destroy_ewoc(e)
        call close_buffer(h, st)
    end subroutine test_enter

    subroutine test_locate(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(ewoc_type) :: e
        type(status_type) :: st
        integer :: n1, n2, n3, node

        call temp_buffer(h)
        e = make_ewoc(pp, "head", "foot")
        call check(error, ewoc_locate(e, 1_int64) == 0, "empty")
        if (allocated(error)) return
        n1 = ewoc_enter_last(e, 1_int64)
        n2 = ewoc_enter_last(e, 2_int64)
        n3 = ewoc_enter_last(e, 3_int64)
        ! "head\nitem 1\nitem 2\nitem 3\nfoot\n": item 2 starts at 13.
        call check(error, ewoc_locate(e, 1_int64) == n1, "before the first")
        if (allocated(error)) return
        call check(error, ewoc_locate(e, 15_int64) == n2, "within the second")
        if (allocated(error)) return
        call check(error, ewoc_locate(e, 30_int64) == n3, "after the last")
        if (allocated(error)) return
        call goto_char(point_min())
        node = ewoc_goto_next(e, 2, st)
        call check(error, st%ok() .and. node == n3 .and. point() == 20, "goto-next")
        if (allocated(error)) return
        node = ewoc_goto_prev(e, 1)
        call check(error, node == n2 .and. point() == 13, "goto-prev")
        if (allocated(error)) return
        node = ewoc_goto_prev(e, 5)
        call check(error, node == n1 .and. point() == 6, "never above the first")
        if (allocated(error)) return
        call goto_char(20_int64)
        node = ewoc_goto_next(e, 2, st)
        call check(error,.not. st%ok(), "no next")
        call destroy_ewoc(e)
        call close_buffer(h, st)
    end subroutine test_locate

    subroutine test_change(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(ewoc_type) :: e
        type(status_type) :: st
        integer(int64), allocatable :: got(:)
        integer :: n1, n2, n3, n4

        call temp_buffer(h)
        e = make_ewoc(pp)
        n1 = ewoc_enter_last(e, 1_int64)
        n2 = ewoc_enter_last(e, 2_int64)
        n3 = ewoc_enter_last(e, 3_int64)
        n4 = ewoc_enter_last(e, 4_int64)
        call check(error, buffer_string(), LF//"item 1"//LF//"item 2"//LF//"item 3"//LF//"item 4"//LF//LF)
        if (allocated(error)) return
        call ewoc_set_data(e, n2, 20_int64)
        call ewoc_invalidate(e, [n2])
        call check(error, buffer_string(), LF//"item 1"//LF//"item 20"//LF//"item 3"//LF//"item 4"//LF//LF)
        if (allocated(error)) return
        call ewoc_collect(e, above_two, got)
        call check(error, size(got) == 3, "collect size")
        if (allocated(error)) return
        call check(error, got(1) == 20 .and. got(2) == 3 .and. got(3) == 4, "collect order")
        if (allocated(error)) return
        call ewoc_delete(e, [n3])
        call check(error, buffer_string(), LF//"item 1"//LF//"item 20"//LF//"item 4"//LF//LF)
        if (allocated(error)) return
        ! ewoc-map refreshes the elements for which it returns true.
        doubled = 100
        call ewoc_map(odd, e)
        call check(error, buffer_string(), LF//"item 101"//LF//"item 20"//LF//"item 4"//LF//LF)
        if (allocated(error)) return
        call ewoc_refresh(e)
        call check(error, buffer_string(), LF//"item 101"//LF//"item 120"//LF//"item 104"//LF//LF)
        if (allocated(error)) return
        doubled = 0
        call ewoc_filter(e, odd)
        call check(error, buffer_string(), LF//"item 101"//LF//LF)
        if (allocated(error)) return
        call check(error, ewoc_nth(e, 0) == n1 .and. ewoc_nth(e, 1) == 0, "one left")
        call destroy_ewoc(e)
        call close_buffer(h, st)
    end subroutine test_change

    subroutine test_hf(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(ewoc_type) :: e
        type(status_type) :: st
        character(len=:), allocatable :: header, footer
        integer :: n

        call temp_buffer(h)
        e = make_ewoc(pp, "[", "]", nosep=.true.)
        n = ewoc_enter_last(e, 1_int64)
        n = ewoc_enter_last(e, 2_int64)
        call check(error, buffer_string(), "[item 1item 2]")
        if (allocated(error)) return
        call ewoc_set_hf(e, "<<", ">>")
        call check(error, buffer_string(), "<<item 1item 2>>")
        if (allocated(error)) return
        call ewoc_get_hf(e, header, footer)
        call check(error, header == "<<" .and. footer == ">>", "get-hf")
        call destroy_ewoc(e)
        call close_buffer(h, st)
    end subroutine test_hf
end module test_ewoc_mod
