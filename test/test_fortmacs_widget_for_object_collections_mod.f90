! test_fortmacs_widget_for_object_collections_mod.f90 -- tests for fortmacs_widget_for_object_collections_mod.
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
module test_fortmacs_widget_for_object_collections_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_mod, only: status_type
    use handle_mod, only: handle_type
    use buffer_mod, only: get_or_make_buffer, set_buffer, close_buffer, erase_buffer
    use insert_delete_mod, only: insert
    use editing_functions_mod, only: buffer_string, goto_character, point, point_minimum
    use fortmacs_widget_for_object_collections_mod, only: fortmacs_widget_for_object_collections_type, &
                                                          make_fortmacs_widget_for_object_collections, &
                                                          destroy_fortmacs_widget_for_object_collections, &
                                                          fortmacs_widget_for_object_collections_enter_last, &
                                                          fortmacs_widget_for_object_collections_enter_first, &
                                                          fortmacs_widget_for_object_collections_enter_after, &
                                                          fortmacs_widget_for_object_collections_data, &
                                                          fortmacs_widget_for_object_collections_set_data, &
                                                          fortmacs_widget_for_object_collections_next, &
                                                          fortmacs_widget_for_object_collections_previous, &
                                                          fortmacs_widget_for_object_collections_nth, &
                                                          fortmacs_widget_for_object_collections_locate, &
                                                          fortmacs_widget_for_object_collections_location, &
                                                          fortmacs_widget_for_object_collections_invalidate, &
                                                          fortmacs_widget_for_object_collections_delete, &
                                                          fortmacs_widget_for_object_collections_filter, &
                                                          fortmacs_widget_for_object_collections_collect, &
                                                          fortmacs_widget_for_object_collections_map, &
                                                          fortmacs_widget_for_object_collections_goto_next, &
                                                          fortmacs_widget_for_object_collections_goto_previous, &
                                                          fortmacs_widget_for_object_collections_set_hf, &
                                                          fortmacs_widget_for_object_collections_get_hf, &
                                                          fortmacs_widget_for_object_collections_refresh
    implicit none(type, external)
    private

    public :: collect_fortmacs_widget_for_object_collections_mod

    character(len=*), parameter :: LF = achar(10)

    ! The "text" of each element: element N prints as "item N".
    integer(int64) :: doubled = 0

contains

    !> Collect all exported unit tests.
    subroutine collect_fortmacs_widget_for_object_collections_mod(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("ewoc-enter-and-walk", test_enter), &
                    new_unittest("ewoc-locate-and-goto", test_locate), &
                    new_unittest("ewoc-invalidate-delete-filter", test_change), &
                    new_unittest("ewoc-header-footer-nosep", test_hf) &
                    ]
    end subroutine collect_fortmacs_widget_for_object_collections_mod

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

    subroutine temporary_buffer(h)
        type(handle_type), intent(out) :: h
        type(status_type) :: st

        call get_or_make_buffer(" *ewoc-test*", h, st)
        call set_buffer(h, st)
        call erase_buffer(st)
        doubled = 0
    end subroutine temporary_buffer

    subroutine test_enter(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(fortmacs_widget_for_object_collections_type) :: e
        type(status_type) :: st
        integer :: n1, n2, n3

        call temporary_buffer(h)
        call insert("before"//LF, st)
        e = make_fortmacs_widget_for_object_collections(pp, "head", "foot")
        call check(error, buffer_string(), "before"//LF//"head"//LF//"foot"//LF)
        if (allocated(error)) return
        n2 = fortmacs_widget_for_object_collections_enter_last(e, 2_int64)
        n3 = fortmacs_widget_for_object_collections_enter_last(e, 3_int64)
        n1 = fortmacs_widget_for_object_collections_enter_first(e, 1_int64)
        call check(error, buffer_string(), "before"//LF//"head"//LF//"item 1"//LF//"item 2"//LF//"item 3"//LF//"foot"//LF)
        if (allocated(error)) return
        call check(error, fortmacs_widget_for_object_collections_nth(e, &
                                                                     0) == n1 .and. fortmacs_widget_for_object_collections_nth(e, &
                                                       -1) == n3 .and. fortmacs_widget_for_object_collections_nth(e, 3) == 0, "nth")
        if (allocated(error)) return
        call check(error, fortmacs_widget_for_object_collections_next(e, &
                                                                    n1) == n2 .and. fortmacs_widget_for_object_collections_next(e, &
                                            n3) == 0 .and. fortmacs_widget_for_object_collections_previous(e, n1) == 0, "next/prev")
        if (allocated(error)) return
        call check(error, fortmacs_widget_for_object_collections_data(e, &
                                   n2) == 2 .and. fortmacs_widget_for_object_collections_location(e, n2) == 20, "data and location")
        if (allocated(error)) return
        n2 = fortmacs_widget_for_object_collections_enter_after(e, n2, 4_int64)
        call check(error, buffer_string(), "before"//LF//"head"//LF//"item 1"//LF//"item 2"//LF//"item 4"//LF// &
                   "item 3"//LF//"foot"//LF)
        call destroy_fortmacs_widget_for_object_collections(e)
        call close_buffer(h, st)
    end subroutine test_enter

    subroutine test_locate(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(fortmacs_widget_for_object_collections_type) :: e
        type(status_type) :: st
        integer :: n1, n2, n3, node

        call temporary_buffer(h)
        e = make_fortmacs_widget_for_object_collections(pp, "head", "foot")
        call check(error, fortmacs_widget_for_object_collections_locate(e, 1_int64) == 0, "empty")
        if (allocated(error)) return
        n1 = fortmacs_widget_for_object_collections_enter_last(e, 1_int64)
        n2 = fortmacs_widget_for_object_collections_enter_last(e, 2_int64)
        n3 = fortmacs_widget_for_object_collections_enter_last(e, 3_int64)
        ! "head\nitem 1\nitem 2\nitem 3\nfoot\n": item 2 starts at 13.
        call check(error, fortmacs_widget_for_object_collections_locate(e, 1_int64) == n1, "before the first")
        if (allocated(error)) return
        call check(error, fortmacs_widget_for_object_collections_locate(e, 15_int64) == n2, "within the second")
        if (allocated(error)) return
        call check(error, fortmacs_widget_for_object_collections_locate(e, 30_int64) == n3, "after the last")
        if (allocated(error)) return
        call goto_character(point_minimum())
        node = fortmacs_widget_for_object_collections_goto_next(e, 2, st)
        call check(error, st%ok() .and. node == n3 .and. point() == 20, "goto-next")
        if (allocated(error)) return
        node = fortmacs_widget_for_object_collections_goto_previous(e, 1)
        call check(error, node == n2 .and. point() == 13, "goto-prev")
        if (allocated(error)) return
        node = fortmacs_widget_for_object_collections_goto_previous(e, 5)
        call check(error, node == n1 .and. point() == 6, "never above the first")
        if (allocated(error)) return
        call goto_character(20_int64)
        node = fortmacs_widget_for_object_collections_goto_next(e, 2, st)
        call check(error,.not. st%ok(), "no next")
        call destroy_fortmacs_widget_for_object_collections(e)
        call close_buffer(h, st)
    end subroutine test_locate

    subroutine test_change(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(fortmacs_widget_for_object_collections_type) :: e
        type(status_type) :: st
        integer(int64), allocatable :: got(:)
        integer :: n1, n2, n3, n4

        call temporary_buffer(h)
        e = make_fortmacs_widget_for_object_collections(pp)
        n1 = fortmacs_widget_for_object_collections_enter_last(e, 1_int64)
        n2 = fortmacs_widget_for_object_collections_enter_last(e, 2_int64)
        n3 = fortmacs_widget_for_object_collections_enter_last(e, 3_int64)
        n4 = fortmacs_widget_for_object_collections_enter_last(e, 4_int64)
        call check(error, buffer_string(), LF//"item 1"//LF//"item 2"//LF//"item 3"//LF//"item 4"//LF//LF)
        if (allocated(error)) return
        call fortmacs_widget_for_object_collections_set_data(e, n2, 20_int64)
        call fortmacs_widget_for_object_collections_invalidate(e, [n2])
        call check(error, buffer_string(), LF//"item 1"//LF//"item 20"//LF//"item 3"//LF//"item 4"//LF//LF)
        if (allocated(error)) return
        call fortmacs_widget_for_object_collections_collect(e, above_two, got)
        call check(error, size(got) == 3, "collect size")
        if (allocated(error)) return
        call check(error, got(1) == 20 .and. got(2) == 3 .and. got(3) == 4, "collect order")
        if (allocated(error)) return
        call fortmacs_widget_for_object_collections_delete(e, [n3])
        call check(error, buffer_string(), LF//"item 1"//LF//"item 20"//LF//"item 4"//LF//LF)
        if (allocated(error)) return
        ! ewoc-map refreshes the elements for which it returns true.
        doubled = 100
        call fortmacs_widget_for_object_collections_map(odd, e)
        call check(error, buffer_string(), LF//"item 101"//LF//"item 20"//LF//"item 4"//LF//LF)
        if (allocated(error)) return
        call fortmacs_widget_for_object_collections_refresh(e)
        call check(error, buffer_string(), LF//"item 101"//LF//"item 120"//LF//"item 104"//LF//LF)
        if (allocated(error)) return
        doubled = 0
        call fortmacs_widget_for_object_collections_filter(e, odd)
        call check(error, buffer_string(), LF//"item 101"//LF//LF)
        if (allocated(error)) return
        call check(error, fortmacs_widget_for_object_collections_nth(e, &
                                                   0) == n1 .and. fortmacs_widget_for_object_collections_nth(e, 1) == 0, "one left")
        call destroy_fortmacs_widget_for_object_collections(e)
        call close_buffer(h, st)
    end subroutine test_change

    subroutine test_hf(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(fortmacs_widget_for_object_collections_type) :: e
        type(status_type) :: st
        character(len=:), allocatable :: header, footer
        integer :: n

        call temporary_buffer(h)
        e = make_fortmacs_widget_for_object_collections(pp, "[", "]", nosep=.true.)
        n = fortmacs_widget_for_object_collections_enter_last(e, 1_int64)
        n = fortmacs_widget_for_object_collections_enter_last(e, 2_int64)
        call check(error, buffer_string(), "[item 1item 2]")
        if (allocated(error)) return
        call fortmacs_widget_for_object_collections_set_hf(e, "<<", ">>")
        call check(error, buffer_string(), "<<item 1item 2>>")
        if (allocated(error)) return
        call fortmacs_widget_for_object_collections_get_hf(e, header, footer)
        call check(error, header == "<<" .and. footer == ">>", "get-hf")
        call destroy_fortmacs_widget_for_object_collections(e)
        call close_buffer(h, st)
    end subroutine test_hf
end module test_fortmacs_widget_for_object_collections_mod
