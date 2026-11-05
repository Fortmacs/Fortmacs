! test_parenthesis_mod.f90 -- tests for Show Paren mode (parenthesis_mod).
! Ports GNU Emacs 31.1 test/lisp/paren-tests.el.
!
! Copyright (C) 2019-2026 Free Software Foundation, Inc.
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

!> Every test creates its buffer and kills it before returning.
module test_parenthesis_mod
    use, intrinsic :: iso_fortran_env, only: int32, int64, real64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_mod, only: status_type
    use handle_mod, only: handle_type
    use value_mod, only: value_type
    use data_mod, only: value_eq
    use buffer_mod, only: get_or_make_buffer, set_buffer, close_buffer
    use insert_delete_mod, only: insert
    use editing_functions_mod, only: goto_character, point, point_minimum, point_maximum
    use interval_tree_mod, only: overlays_at, overlay_get
    use parenthesis_mod, only: is_show_parenthesis_unescaped, show_parenthesis_categorize_parenthesis, &
                               show_parenthesis_locate_near_parenthesis, show_parenthesis_default, show_parenthesis_data_type, &
                               show_parenthesis_function, show_parenthesis_when_point_inside_parenthesis, &
                               show_parenthesis_when_point_in_periphery, show_parenthesis_style, &
                               SHOW_PARENTHESIS_STYLE_EXPRESSION, SHOW_PARENTHESIS_STYLE_PARENTHESIS
    use simple_mod, only: blink_parenthesis_open_parenthesis_line_string, fundamental_mode, special_mode
    use modes_mod, only: minor_modes
    use bindings_mod, only: initialize_bindings
    use timer_mod, only: execute_with_timer, destroy_timer
    use keyboard_mod, only: read_event, set_input_fd, discard_input
    use system_dependent_mod, only: fortmacs_terminal_type, open_pty, close_fd, initialize_system_modes, reset_system_modes, &
                                    fortmacs_write
    use test_support_mod, only: show_alone
    implicit none(type, external)
    private

    public :: collect_parenthesis_mod

    integer(int32) :: master = -1

contains

    !> Collect all exported unit tests.
    subroutine collect_parenthesis_mod(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("paren-tests-unescaped-p", test_unescaped), &
                    new_unittest("paren-tests-categorize-paren", test_categorize_parenthesis), &
                    new_unittest("paren-tests-locate-near-paren", test_locate_near_parenthesis), &
                    new_unittest("paren-tests-locate-near-paren-inside", test_locate_near_parenthesis_inside), &
                    new_unittest("paren-tests-locate-near-paren-in-periphery", test_locate_near_parenthesis_in_periphery), &
                    new_unittest("paren-tests-default", test_default), &
                    new_unittest("paren-tests-open-paren-line", test_open_parenthesis_line), &
                    new_unittest("show-paren-function-overlays", test_function), &
                    new_unittest("show-paren-mode-and-local-mode", test_modes), &
                    new_unittest("show-paren-idle-timer", test_idle_timer) &
                    ]
    end subroutine collect_parenthesis_mod

    !> (with-temp-buffer (insert TEXT) ...)
    subroutine temporary_buffer(h, text)
        type(handle_type), intent(out) :: h
        character(len=*), intent(in) :: text
        type(status_type) :: st

        call get_or_make_buffer("*paren-test*", h, st)
        call set_buffer(h, st)
        call fundamental_mode(st)
        call insert(text, st)
    end subroutine temporary_buffer

    subroutine test_unescaped(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call temporary_buffer(h, "(insert)")
        call goto_character(point() - 1)
        call check(error, is_show_parenthesis_unescaped(point()), "unescaped")
        if (allocated(error)) return
        call insert("\", st)
        call check(error,.not. is_show_parenthesis_unescaped(point()), "escaped")
        call close_buffer(h, st)
    end subroutine test_unescaped

    subroutine test_categorize_parenthesis(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        integer :: directory
        integer(int64) :: outside

        call temporary_buffer(h, "(insert)")
        call goto_character(point() - 1)
        call show_parenthesis_categorize_parenthesis(point(), directory, outside)
        call check(error, directory == -1 .and. outside == point() + 1, "close paren")
        if (allocated(error)) return
        call goto_character(point_minimum())
        call show_parenthesis_categorize_parenthesis(point(), directory, outside)
        call check(error, directory == 1 .and. outside == point(), "open paren")
        call close_buffer(h, st)
    end subroutine test_categorize_parenthesis

    !> Whether locate-near-paren gives (DIRECTORY . OUTSIDE), DIRECTORY 0 for nil.
    logical function near(directory, outside)
        integer, intent(in) :: directory
        integer(int64), intent(in) :: outside
        integer :: d
        integer(int64) :: o

        call show_parenthesis_locate_near_parenthesis(d, o)
        near = d == directory
        if (near .and. directory /= 0) near = o == outside
    end function near

    subroutine test_locate_near_parenthesis(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        show_parenthesis_when_point_inside_parenthesis = .false.
        show_parenthesis_when_point_in_periphery = .false.
        call temporary_buffer(h, "(insert)")
        call check(error, near(-1, point()), "after the close paren")
        if (allocated(error)) return
        call goto_character(point() - 1)
        call check(error, near(0, 0_int64), "before the close paren")
        if (allocated(error)) return
        call goto_character(point_minimum())
        call check(error, near(1, point_minimum()), "before the open paren")
        if (allocated(error)) return
        call goto_character(point() + 1)
        call check(error, near(0, 0_int64), "after the open paren")
        call close_buffer(h, st)
    end subroutine test_locate_near_parenthesis

    subroutine test_locate_near_parenthesis_inside(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        show_parenthesis_when_point_inside_parenthesis = .true.
        show_parenthesis_when_point_in_periphery = .false.
        call temporary_buffer(h, "(insert)")
        call check(error, near(-1, point()), "after the close paren")
        if (allocated(error)) return
        call goto_character(point() - 1)
        call check(error, near(-1, point() + 1), "before the close paren")
        if (allocated(error)) return
        call goto_character(point_minimum())
        call check(error, near(1, point_minimum()), "before the open paren")
        if (allocated(error)) return
        call goto_character(point() + 1)
        call check(error, near(1, point_minimum()), "after the open paren")
        show_parenthesis_when_point_inside_parenthesis = .false.
        call close_buffer(h, st)
    end subroutine test_locate_near_parenthesis_inside

    subroutine test_locate_near_parenthesis_in_periphery(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        show_parenthesis_when_point_inside_parenthesis = .false.
        show_parenthesis_when_point_in_periphery = .true.
        call temporary_buffer(h, " (insert) ")
        call check(error, near(-1, point() - 1), "end of line")
        if (allocated(error)) return
        call goto_character(point() - 2)
        call check(error, near(0, 0_int64), "before the close paren")
        if (allocated(error)) return
        call goto_character(point_minimum())
        call check(error, near(1, point() + 1), "beginning of line")
        if (allocated(error)) return
        call goto_character(point() + 1)
        call check(error, near(1, point()), "before the open paren")
        if (allocated(error)) return
        call goto_character(point() + 1)
        call check(error, near(0, 0_int64), "after the open paren")
        show_parenthesis_when_point_in_periphery = .false.
        call close_buffer(h, st)
    end subroutine test_locate_near_parenthesis_in_periphery

    !> Whether show-paren--default gives (HERE-BEG HERE-END THERE-BEG
    !> THERE-END MISMATCH), THERE-BEG 0 for nil.
    logical function default_is(here_beginning, here_end, there_beginning, there_end, mismatch)
        integer(int64), intent(in) :: here_beginning, here_end, there_beginning, there_end
        logical, intent(in) :: mismatch
        type(show_parenthesis_data_type) :: data
        logical :: found

        call show_parenthesis_default(found, data)
        default_is = found .and. data%here_beginning == here_beginning .and. data%here_end == here_end .and. &
                     data%there_beginning == there_beginning .and. data%there_end == there_end .and. (data%mismatch .eqv. mismatch)
    end function default_is

    subroutine test_default(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call temporary_buffer(h, "(insert")
        call goto_character(point_minimum())
        call check(error, default_is(point(), point() + 1, 0_int64, 0_int64, .true.), "unmatched")
        if (allocated(error)) return
        call goto_character(point_maximum())
        call insert(")", st)
        call check(error, default_is(point() - 1, point(), point_minimum(), point_minimum() + 1, .false.), "backward")
        if (allocated(error)) return
        call goto_character(point_minimum())
        call check(error, default_is(point(), point() + 1, point_maximum() - 1, point_maximum(), .false.), "forward")
        call close_buffer(h, st)
    end subroutine test_default

    !> The context line of the open paren matching the close paren at the
    !> end of the buffer.
    function open_parenthesis_line() result(s)
        character(len=:), allocatable :: s
        type(show_parenthesis_data_type) :: data
        logical :: found

        call show_parenthesis_default(found, data)
        s = blink_parenthesis_open_parenthesis_line_string(min(data%here_beginning, data%there_beginning))
    end function open_parenthesis_line

    subroutine test_open_parenthesis_line(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        ! Lisp-like
        call temporary_buffer(h, "(defun foo ()"//achar(10)//"                  (dummy))")
        call goto_character(point_maximum())
        call check(error, open_parenthesis_line(), "(defun foo ()")
        call close_buffer(h, st)
        if (allocated(error)) return
        ! C-like
        call temporary_buffer(h, "int foo() {"//achar(10)//"                 int blah;"//achar(10)//"             }")
        call goto_character(point_maximum())
        call check(error, open_parenthesis_line(), "int foo() {")
        call close_buffer(h, st)
        if (allocated(error)) return
        ! C-like with hanging {
        call temporary_buffer(h, "int foo()"//achar(10)//"               {"//achar(10)//"                 int blah;"// &
                              achar(10)//"               }")
        call goto_character(point_maximum())
        call check(error, open_parenthesis_line(), "int foo()...{")
        call close_buffer(h, st)
    end subroutine test_open_parenthesis_line

    !> The face of the overlay at POSITION, or "" for none.
    function face_at(position) result(name)
        integer(int64), intent(in) :: position
        character(len=:), allocatable :: name
        type(handle_type), allocatable :: vector(:)
        type(value_type) :: face
        integer :: k

        name = ""
        call overlays_at(position, vector, sorted=.true.)

        do k = 1, size(vector)
            face = overlay_get(vector(k), "face")
            if (value_eq(face, value_type(name="show_parenthesis_match"))) name = "show_parenthesis_match"
            if (value_eq(face, value_type(name="show_parenthesis_mismatch"))) name = "show_parenthesis_mismatch"
            if (value_eq(face, value_type(name="show_parenthesis_match_expression"))) name = "show_parenthesis_match_expression"
            if (len(name) > 0) return
        end do
    end function face_at

    !> show-paren-function puts the faces on the paren and its match, the
    !> mismatch face on mismatched parens, the expression face with the
    !> `expression' style, and removes the highlighting away from parens.
    subroutine test_function(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call initialize_bindings()
        call temporary_buffer(h, "(a (b) c)")
        call show_alone()
        call goto_character(point_maximum())
        call show_parenthesis_function(st)
        call check(error, face_at(1_int64) == "show_parenthesis_match" .and. face_at(9_int64) == "show_parenthesis_match" .and. &
                   face_at(2_int64) == "", "both parens after a close paren")
        if (allocated(error)) return
        call goto_character(4_int64)
        call show_parenthesis_function(st)
        call check(error, face_at(4_int64) == "show_parenthesis_match" .and. face_at(6_int64) == "show_parenthesis_match" .and. &
                   face_at(1_int64) == "", "before an open paren")
        if (allocated(error)) return
        show_parenthesis_style = SHOW_PARENTHESIS_STYLE_EXPRESSION
        call show_parenthesis_function(st)
        call check(error, face_at(5_int64) == "show_parenthesis_match_expression", "the expression")
        show_parenthesis_style = SHOW_PARENTHESIS_STYLE_PARENTHESIS
        if (allocated(error)) return
        call goto_character(2_int64)
        call show_parenthesis_function(st)
        call check(error, face_at(1_int64) == "" .and. face_at(4_int64) == "", "no paren near point")
        if (allocated(error)) return
        call goto_character(point_maximum())
        call insert(" (]", st)
        call show_parenthesis_function(st)
        call check(error, face_at(point_maximum() - 1) == "show_parenthesis_mismatch" .and. &
                   face_at(point_maximum() - 2) == "show_parenthesis_mismatch", "mismatched")
        call close_buffer(h, st)
    end subroutine test_function

    !> Show Paren mode is on by default; not in special-mode buffers; and
    !> Show Paren Local mode turns it off in one buffer.
    subroutine test_modes(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call initialize_bindings()
        call check(error, minor_modes%show_parenthesis_mode%enabled(), "on by default")
        if (allocated(error)) return
        call temporary_buffer(h, "(a)")
        call show_alone()
        call special_mode(st)
        call show_parenthesis_function(st)
        call check(error, face_at(1_int64) == "", "not in special mode")
        if (allocated(error)) return
        call fundamental_mode(st)
        call show_parenthesis_function(st)
        call check(error, face_at(1_int64) == "show_parenthesis_match", "in fundamental mode")
        if (allocated(error)) return
        call minor_modes%show_parenthesis_local_mode%disable(st)
        call check(error,.not. minor_modes%show_parenthesis_local_mode%enabled() .and. face_at(1_int64) == "", &
                   "locally off")
        if (allocated(error)) return
        call show_parenthesis_function(st)
        call check(error, face_at(1_int64) == "" .and. minor_modes%show_parenthesis_mode%enabled(), &
                   "still globally on")
        if (allocated(error)) return
        call minor_modes%show_parenthesis_local_mode%enable(st)
        call show_parenthesis_function(st)
        call check(error, face_at(1_int64) == "show_parenthesis_match", "locally on again")
        call close_buffer(h, st)
    end subroutine test_modes

    subroutine write_x(status)
        type(status_type), intent(out) :: status

        call fortmacs_write(master, "x", status)
    end subroutine write_x

    !> The idle timer highlights the parens while the command loop waits
    !> for input (bounded: a timer types a key after half a second).
    subroutine test_idle_timer(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, key_timer
        type(status_type) :: st
        type(fortmacs_terminal_type) :: old
        type(value_type) :: ev
        integer(int32) :: slave
        logical :: have

        call initialize_bindings()
        call temporary_buffer(h, "(abc)")
        call show_alone()
        call discard_input()
        call open_pty(master, slave, st)
        call check(error, st%ok(), "pty")
        if (allocated(error)) return
        call initialize_system_modes(slave, old, st)
        call set_input_fd(slave)
        call execute_with_timer(key_timer, 0.5_real64, write_x, st)
        call read_event(ev, have, st)
        call check(error, have .and. face_at(1_int64) == "show_parenthesis_match" .and. &
                   face_at(5_int64) == "show_parenthesis_match", "highlighted after the delay")
        call destroy_timer(key_timer)
        call reset_system_modes(slave, old, st)
        call set_input_fd(-1_int32)
        call close_fd(slave)
        call close_fd(master)
        call discard_input()
        call close_buffer(h, st)
    end subroutine test_idle_timer
end module test_parenthesis_mod
