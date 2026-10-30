! test_ansi_color_fm.f90 -- tests for ansi_color_fm.
! Ports GNU Emacs 31.1 test/lisp/ansi-color-tests.el.
!
! Copyright (C) 2020-2026 Free Software Foundation, Inc.
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

!> Tests for ansi_color_fm, ported from test/lisp/ansi-color-tests.el.
module test_ansi_color_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_fm, only: status_type
    use handle_fm, only: handle_type
    use lisp_fm, only: value_type, nil, nilp, make_string, list, xstring
    use lread_fm, only: sym
    use fns_fm, only: value_equal
    use buffer_fm, only: get_buffer_create, set_buffer, kill_buffer, current_buffer
    use insdel_fm, only: insert
    use editfns_fm, only: point, point_min, point_max, buffer_string
    use textprop_fm, only: get_char_property
    use itree_fm, only: overlays_at
    use bindings_fm, only: init_bindings
    use faces_fm, only: face_spec_recalc_all
    use ansi_color_fm, only: ansi_color_apply_on_region, ansi_color_filter_region, ansi_color_filter_apply, &
                             ansi_color_apply, ansi_color_bold_is_bright, reset_ansi_color_context, &
                             ansi_color_apply_face_function, ansi_color_apply_text_property_face
    implicit none(type, external)
    private

    public :: collect_ansi_color_fm

    character(len=*), parameter :: ESC = achar(27)

    ! One of ansi-color-tests--strings: INPUT, TEXT and the faces
    ! expected (normal, and with `ansi_color_bold_is_bright').
    type :: case_type
        character(len=:), allocatable :: input, text
        type(value_type) :: face, bright_face
        logical :: has_bright = .false.
    end type case_type

contains

    !> Collect all exported unit tests.
    subroutine collect_ansi_color_fm(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("ansi-color-apply-on-region-test", test_apply_on_region), &
                    new_unittest("ansi-color-apply-on-region-bold-is-bright-test", test_bold_is_bright), &
                    new_unittest("ansi-color-apply-on-region-preserving-test", test_preserving), &
                    new_unittest("ansi-color-incomplete-sequences-test", test_incomplete) &
                    ]
    end subroutine collect_ansi_color_fm

    !> ansi-color-tests--strings.
    subroutine strings(cases)
        type(case_type), allocatable, intent(out) :: cases(:)
        type(value_type) :: yellow, bright_yellow, custom_color

        call init_bindings()
        ! The faces' attributes on the frame, as when the frame is made.
        call face_spec_recalc_all()
        yellow = make_string("yellow3")
        bright_yellow = make_string("yellow2")
        custom_color = make_string("#87FFFF")
        allocate (cases(14))
        call set(1, "Hello World", "Hello World", nil())
        call set(2, ESC//"[33mHello World"//ESC//"[0m", "Hello World", list(sym(":foreground"), yellow))
        call set(3, ESC//"[43mHello World"//ESC//"[0m", "Hello World", list(sym(":background"), yellow))
        call set(4, ESC//"[93mHello World"//ESC//"[0m", "Hello World", list(sym(":foreground"), bright_yellow))
        call set(5, ESC//"[103mHello World"//ESC//"[0m", "Hello World", list(sym(":background"), bright_yellow))
        call set(6, ESC//"[1;33mHello World"//ESC//"[0m", "Hello World", bold_fg(yellow), bold_fg(bright_yellow))
        call set(7, ESC//"[33;1mHello World"//ESC//"[0m", "Hello World", bold_fg(yellow), bold_fg(bright_yellow))
        call set(8, ESC//"[1m"//ESC//"[33mHello World"//ESC//"[0m", "Hello World", bold_fg(yellow), &
                 bold_fg(bright_yellow))
        call set(9, ESC//"[33m"//ESC//"[1mHello World"//ESC//"[0m", "Hello World", bold_fg(yellow), &
                 bold_fg(bright_yellow))
        call set(10, ESC//"[1m"//ESC//"[3m"//ESC//"[5mbold italics blink"//ESC//"[0m", "bold italics blink", &
                 list(sym("ansi_color_bold"), sym("ansi_color_italic"), sym("ansi_color_slow_blink")))
        call set(11, ESC//"[10munrecognized"//ESC//"[0m", "unrecognized", nil())
        call set(12, ESC//"[38;5;3;1mHello World"//ESC//"[0m", "Hello World", bold_fg(yellow), bold_fg(bright_yellow))
        call set(13, ESC//"[48;5;123;1mHello World"//ESC//"[0m", "Hello World", &
                 list(sym("ansi_color_bold"), list(sym(":background"), custom_color)))
        call set(14, ESC//"[48;2;135;255;255;1mHello World"//ESC//"[0m", "Hello World", &
                 list(sym("ansi_color_bold"), list(sym(":background"), custom_color)))

    contains

        subroutine set(k, input, text, face, bright_face)
            integer, intent(in) :: k
            character(len=*), intent(in) :: input, text
            type(value_type), intent(in) :: face
            type(value_type), intent(in), optional :: bright_face

            cases(k)%input = input
            cases(k)%text = text
            cases(k)%face = face

            if (present(bright_face)) then
                cases(k)%bright_face = bright_face
                cases(k)%has_bright = .true.
            end if
        end subroutine set

        function bold_fg(color) result(v)
            type(value_type), intent(in) :: color
            type(value_type) :: v

            v = list(sym("ansi_color_bold"), list(sym(":foreground"), color))
        end function bold_fg
    end subroutine strings

    ! A fresh temporary buffer, the current one before in OLD.
    subroutine temp_buffer(old, b)
        type(handle_type), intent(out) :: old, b
        type(status_type) :: st

        old = current_buffer()
        call get_buffer_create(" ansi-color test", b, st)
        call set_buffer(b, st)
    end subroutine temp_buffer

    subroutine end_temp_buffer(old, b)
        type(handle_type), intent(in) :: old, b
        type(status_type) :: st

        call set_buffer(old, st)
        call kill_buffer(b, st)
    end subroutine end_temp_buffer

    ! Whether there are overlays at the start of the buffer.
    logical function overlays_at_start()
        type(handle_type), allocatable :: vec(:)

        call overlays_at(point_min(), vec)
        overlays_at_start = size(vec) > 0
    end function overlays_at_start

    !> Port of ansi-color-tests.el ansi-color-apply-on-region-test.
    subroutine test_apply_on_region(error)
        type(error_type), allocatable, intent(out) :: error
        type(case_type), allocatable :: cases(:)
        type(handle_type) :: old, b
        type(status_type) :: st
        integer :: k

        call strings(cases)

        do k = 1, size(cases)
            call temp_buffer(old, b)
            call insert(cases(k)%input, st)
            call ansi_color_apply_on_region(point_min(), point_max())
            call check(error, buffer_string(), cases(k)%text)

            if (.not. allocated(error)) then
                call check(error, value_equal(get_char_property(point_min(), sym("face")), cases(k)%face), &
                           "face of: "//cases(k)%text)
            end if

            if (.not. allocated(error) .and. .not. nilp(cases(k)%face)) then
                call check(error, overlays_at_start(), "an overlay")
            end if

            call end_temp_buffer(old, b)
            if (allocated(error)) return
        end do
    end subroutine test_apply_on_region

    !> Port of ansi-color-tests.el
    !> ansi-color-apply-on-region-bold-is-bright-test.
    subroutine test_bold_is_bright(error)
        type(error_type), allocatable, intent(out) :: error
        type(case_type), allocatable :: cases(:)
        type(handle_type) :: old, b
        type(value_type) :: face
        type(status_type) :: st
        integer :: k

        call strings(cases)
        ansi_color_bold_is_bright = .true.

        do k = 1, size(cases)
            face = cases(k)%face
            if (cases(k)%has_bright) face = cases(k)%bright_face
            call temp_buffer(old, b)
            call insert(cases(k)%input, st)
            call ansi_color_apply_on_region(point_min(), point_max())
            call check(error, buffer_string(), cases(k)%text)

            if (.not. allocated(error)) then
                call check(error, value_equal(get_char_property(point_min(), sym("face")), face), &
                           "face of: "//cases(k)%text)
            end if

            if (.not. allocated(error) .and. .not. nilp(face)) call check(error, overlays_at_start(), "an overlay")
            call end_temp_buffer(old, b)
            if (allocated(error)) exit
        end do

        ansi_color_bold_is_bright = .false.
    end subroutine test_bold_is_bright

    !> Port of ansi-color-tests.el ansi-color-apply-on-region-preserving-test.
    subroutine test_preserving(error)
        type(error_type), allocatable, intent(out) :: error
        type(case_type), allocatable :: cases(:)
        type(handle_type) :: old, b
        type(status_type) :: st
        integer :: k

        call strings(cases)

        do k = 1, size(cases)
            call temp_buffer(old, b)
            call insert(cases(k)%input, st)
            call ansi_color_apply_on_region(point_min(), point_max(), .true.)
            call check(error, buffer_string(), cases(k)%input)
            call end_temp_buffer(old, b)
            if (allocated(error)) return
        end do
    end subroutine test_preserving

    !> Port of ansi-color-tests.el ansi-color-incomplete-sequences-test
    !> (the texts; the properties are compared through the buffer's).
    subroutine test_incomplete(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: strs(9) = [character(len=19) :: ESC//"[", "2;31m Hello World ", ESC, &
                                                             "[108;5;12", "3m", "Greetings", ESC//"[0m"//ESC//"[35;6m", "Hello", ""]
        integer, parameter :: lens(9) = [2, 18, 1, 9, 2, 9, 11, 5, 0]
        character(len=:), allocatable :: complete_str, filtered_str, joined
        type(value_type) :: propertized
        type(handle_type) :: old, b
        type(status_type) :: st
        integer(int64) :: opoint
        integer :: k

        call init_bindings()
        complete_str = ""

        do k = 1, 8
            complete_str = complete_str//strs(k) (1:lens(k))
        end do

        filtered_str = " Hello World Greetings"//"Hello"
        ! Tests for `ansi_color_filter_apply'
        call temp_buffer(old, b)
        call check(error, ansi_color_filter_apply(complete_str), filtered_str)
        call end_temp_buffer(old, b)
        if (allocated(error)) return
        call temp_buffer(old, b)
        joined = ""

        do k = 1, 8
            joined = joined//ansi_color_filter_apply(strs(k) (1:lens(k)))
        end do

        call check(error, joined, filtered_str)
        call end_temp_buffer(old, b)
        if (allocated(error)) return
        ! Tests for `ansi_color_filter_region'
        call temp_buffer(old, b)
        call insert(complete_str, st)
        call ansi_color_filter_region(point_min(), point_max())
        call check(error, buffer_string(), filtered_str)
        call end_temp_buffer(old, b)
        if (allocated(error)) return
        call temp_buffer(old, b)

        do k = 1, 8
            opoint = point()
            call insert(strs(k) (1:lens(k)), st)
            call ansi_color_filter_region(opoint, point())
        end do

        call check(error, buffer_string(), filtered_str)
        call end_temp_buffer(old, b)
        if (allocated(error)) return
        ! Test for `ansi_color_apply'
        call temp_buffer(old, b)
        propertized = ansi_color_apply(make_string(complete_str))
        call check(error, xstring(propertized), filtered_str)
        call end_temp_buffer(old, b)
        if (allocated(error)) return
        ! Tests for `ansi_color_apply_on_region' (with text properties)
        call temp_buffer(old, b)
        call ansi_color_apply_face_function%set_local(ansi_color_apply_text_property_face)

        do k = 1, 8
            opoint = point()
            call insert(strs(k) (1:lens(k)), st)
            call ansi_color_apply_on_region(opoint, point())
        end do

        call check(error, buffer_string(), filtered_str)
        call end_temp_buffer(old, b)
        if (allocated(error)) return
        ! \e not followed by '[' and invalid ANSI escape sequences
        call temp_buffer(old, b)
        call check(error, ansi_color_filter_apply(ESC), "")
        if (.not. allocated(error)) call check(error, ansi_color_filter_apply(ESC//"[33m test "//ESC//"[0m"), ESC//" test ")
        call end_temp_buffer(old, b)
        if (allocated(error)) return
        call temp_buffer(old, b)
        call check(error, ansi_color_filter_apply(ESC//" a "//ESC//ESC//"["//ESC//"["), ESC//" a "//ESC//ESC//"[")
        call end_temp_buffer(old, b)
        call reset_ansi_color_context()
    end subroutine test_incomplete
end module test_ansi_color_fm
