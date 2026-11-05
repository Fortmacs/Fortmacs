! test_skeleton_mod.f90 -- tests for statement skeletons (skeleton_mod).
! Own tests; upstream has no tests of skeleton.el.
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

!> Own tests of skeleton_mod: the elements, the interactor, sub-skeletons,
!> quitting to resume:, the transformation, regions, change groups and
!> paired insertion.
module test_skeleton_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_mod, only: status_type, format_error, FM_QUIT
    use handle_mod, only: handle_type
    use buffer_mod, only: get_or_make_buffer, set_buffer, erase_buffer
    use insert_delete_mod, only: insert
    use editing_functions_mod, only: point, goto_char, point_min, point_max, buffer_string, widen
    use keyboard_mod, only: discard_input, kbd_store_bytes, last_command_event
    use execute_interactively_mod, only: prefix_arg_type, PREFIX_NONE
    use value_mod, only: value_type
    use bindings_mod, only: init_bindings
    use simple_mod, only: fundamental_mode
    use test_support_mod, only: show_alone
    use skeleton_mod, only: skeleton_element_type, skeleton_insert, skeleton_command, skeleton_text, skeleton_literal, &
                            skeleton_delete, skeleton_prompt, skeleton_for_each, skeleton_action, skeleton_value, &
                            skeleton_pair_insert_maybe, skeleton_transformation_function_set_local, skeleton_positions, &
                            skeleton_end_newline, skeleton_pair, skeleton_state, SKELETON_NEWLINE, SKELETON_POINT, &
                            SKELETON_POINT_OVERRIDE, SKELETON_INDENT, SKELETON_POSITION, SKELETON_IF_MOVED, &
                            SKELETON_IF_NOT_MOVED, SKELETON_RESUME, SKELETON_STRING, SKELETON_V1, SKELETON_END, &
                            SKELETON_NO_INTERACTOR
    implicit none(type, external)
    private

    public :: collect_skeleton_mod

    character(len=*), parameter :: LF = achar(10), CR = achar(13)

contains

    !> Collect all exported unit tests.
    subroutine collect_skeleton_mod(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("skeleton-strings-and-point", test_strings_and_point), &
                    new_unittest("skeleton-read-string", test_read_string), &
                    new_unittest("skeleton-subskeleton-loop", test_subskeleton_loop), &
                    new_unittest("skeleton-quit-resume", test_quit_resume), &
                    new_unittest("skeleton-moved", test_moved), &
                    new_unittest("skeleton-positions", test_positions), &
                    new_unittest("skeleton-transformation", test_transformation), &
                    new_unittest("skeleton-for-each", test_for_each), &
                    new_unittest("skeleton-regions", test_regions), &
                    new_unittest("skeleton-change-group", test_change_group), &
                    new_unittest("skeleton-values", test_values), &
                    new_unittest("skeleton-pair", test_pair) &
                    ]
    end subroutine collect_skeleton_mod

    !> A fresh buffer in Fundamental mode holding TEXT, point at its end.
    subroutine temp_buffer(text)
        character(len=*), intent(in) :: text
        type(handle_type) :: buffer
        type(status_type) :: st

        call init_bindings()
        call discard_input()
        call get_or_make_buffer("*skeleton-test*", buffer, st)
        call set_buffer(buffer, st)
        call widen()
        call erase_buffer(st)
        call fundamental_mode(st)
        call show_alone()
        call insert(text, st)
    end subroutine temp_buffer

    ! An upcasing transformation.
    subroutine upcase(string, result)
        character(len=*), intent(in) :: string
        character(len=:), allocatable, intent(out) :: result
        integer :: i

        result = string

        do i = 1, len(result)
            if (result(i:i) >= "a" .and. result(i:i) <= "z") result(i:i) = achar(iachar(result(i:i)) - 32)
        end do
    end subroutine upcase

    ! An action that fails.
    recursive subroutine fail(status)
        type(status_type), intent(out) :: status

        status = status_type()
        status%code = 1
    end subroutine fail

    ! An action setting v1.
    recursive subroutine set_v1(status)
        type(status_type), intent(out) :: status

        status = status_type()
        skeleton_state%v1 = [skeleton_literal("<v1>")]
    end subroutine set_v1

    ! A value: a sub-skeleton run once with string "".
    recursive subroutine sub_value(status)
        type(status_type), intent(out) :: status

        status = status_type()
        skeleton_state%value = [skeleton_for_each([character(len=1) :: ""]), skeleton_text("[sub]"), SKELETON_END]
    end subroutine sub_value

    !> Strings, \n, > and _: the text, its end newline and point.
    subroutine test_strings_and_point(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st

        call temp_buffer("")
        call skeleton_insert([SKELETON_NO_INTERACTOR, skeleton_text("if x"), SKELETON_NEWLINE, SKELETON_INDENT, &
                              SKELETON_POINT, SKELETON_NEWLINE, skeleton_text("fi")], st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call check(error, buffer_string() == "if x"//LF//LF//"fi", "text: "//buffer_string())
        if (allocated(error)) return
        call check(error, point() == 6_int64, "point after _")
        if (allocated(error)) return
        ! skeleton_end_newline false: no newline at the end; - overrides _.
        call temp_buffer("")
        skeleton_end_newline = .false.
        call skeleton_insert([SKELETON_NO_INTERACTOR, skeleton_text("a"), SKELETON_POINT, skeleton_text("b"), &
                              SKELETON_POINT_OVERRIDE, skeleton_text("c"), skeleton_delete(1)], st)
        skeleton_end_newline = .true.
        call check(error, buffer_string() == "ab", "no end newline: "//buffer_string())
        if (allocated(error)) return
        call check(error, point() == 3_int64, "point after -")
    end subroutine test_strings_and_point

    !> string read from the minibuffer once, then inserted again; STRING given.
    subroutine test_read_string(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st

        call temp_buffer("")
        call kbd_store_bytes("foo"//CR)
        call skeleton_insert([skeleton_prompt("Name: "), skeleton_text("<"), SKELETON_STRING, skeleton_text(">"), &
                              SKELETON_STRING], st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call check(error, buffer_string() == "<foo>foo", "read: "//buffer_string())
        if (allocated(error)) return
        call temp_buffer("")
        call skeleton_insert([skeleton_prompt("Name: "), SKELETON_STRING], st, string="bar")
        call check(error, buffer_string() == "bar", "given: "//buffer_string())
    end subroutine test_read_string

    !> A sub-skeleton is repeated as long as the input is not empty.
    subroutine test_subskeleton_loop(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st

        call temp_buffer("")
        call kbd_store_bytes("a"//CR//"b"//CR//CR)
        call skeleton_insert([SKELETON_NO_INTERACTOR, skeleton_text("("), skeleton_prompt("item, %s: "), &
                              skeleton_text("["), SKELETON_STRING, skeleton_text("]"), SKELETON_END, skeleton_text(")")], st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call check(error, buffer_string() == "([a][b])", "loop: "//buffer_string())
    end subroutine test_subskeleton_loop

    !> A quit in a sub-skeleton removes it and goes on after resume:.
    subroutine test_quit_resume(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st

        call temp_buffer("")
        call kbd_store_bytes("a"//CR//achar(7))
        call skeleton_insert([SKELETON_NO_INTERACTOR, skeleton_text("("), skeleton_prompt("item, %s: "), &
                              skeleton_text("["), SKELETON_STRING, skeleton_text("]"), SKELETON_END, skeleton_text("x"), &
                              SKELETON_RESUME, skeleton_text(")")], st)
        call check(error, st%code == FM_QUIT, "the quit is passed on")
        if (allocated(error)) return
        call check(error, buffer_string() == "([a])", "resume: "//buffer_string())
    end subroutine test_quit_resume

    !> & and | test whether the previous element moved point.
    subroutine test_moved(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st

        call temp_buffer("")
        call skeleton_insert([SKELETON_NO_INTERACTOR, skeleton_text("a"), SKELETON_IF_MOVED, skeleton_text("b"), &
                              skeleton_text(""), SKELETON_IF_MOVED, skeleton_text("c"), skeleton_text(""), SKELETON_IF_NOT_MOVED, &
                              skeleton_text("d"), skeleton_text("e"), SKELETON_IF_NOT_MOVED, skeleton_text("f")], st)
        call check(error, buffer_string() == "abde", "moved: "//buffer_string())
    end subroutine test_moved

    !> @ records positions in reverse order.
    subroutine test_positions(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st

        call temp_buffer("")
        call skeleton_insert([SKELETON_NO_INTERACTOR, SKELETON_POSITION, skeleton_text("ab"), SKELETON_POSITION], st)
        call check(error, size(skeleton_positions) == 2, "two positions")
        if (allocated(error)) return
        call check(error, skeleton_positions(1) == 3_int64 .and. skeleton_positions(2) == 1_int64, "reverse order")
    end subroutine test_positions

    !> Strings are transformed, string and values are not.
    subroutine test_transformation(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st

        call temp_buffer("")
        call skeleton_transformation_function_set_local(upcase)
        call skeleton_insert([skeleton_prompt("x"), skeleton_text("ab"), SKELETON_STRING, skeleton_literal("cd")], st, &
                             string="ef")
        call skeleton_transformation_function_set_local()
        call check(error, buffer_string() == "ABefcd", "transformed: "//buffer_string())
    end subroutine test_transformation

    !> A list of strings as interactor: once for each.
    subroutine test_for_each(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st

        call temp_buffer("")
        call skeleton_insert([SKELETON_NO_INTERACTOR, skeleton_for_each([character(len=2) :: "x", "yz"]), SKELETON_STRING, &
                              skeleton_text(";"), SKELETON_END], st)
        call check(error, buffer_string() == "x;yz;", "for each: "//buffer_string())
    end subroutine test_for_each

    !> A positive REGIONS wraps _ around the next words.
    subroutine test_regions(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st

        call temp_buffer("one two three")
        call goto_char(1_int64)
        call skeleton_insert([SKELETON_NO_INTERACTOR, skeleton_text("<b>"), SKELETON_POINT, skeleton_text("</b>")], st, &
                             2_int64)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call check(error, buffer_string() == "<b>one two</b>"//LF//"three", "wrapped: "//buffer_string())
    end subroutine test_regions

    !> A skeleton command undoes its changes when an error occurs.
    subroutine test_change_group(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st

        call temp_buffer("keep")
        call skeleton_command([SKELETON_NO_INTERACTOR, skeleton_text(" more"), skeleton_action(fail)], st)
        call check(error,.not. st%ok(), "the error is passed on")
        if (allocated(error)) return
        call check(error, buffer_string() == "keep", "undone: "//buffer_string())
    end subroutine test_change_group

    !> Values and v1.
    subroutine test_values(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st

        call temp_buffer("")
        call skeleton_insert([SKELETON_NO_INTERACTOR, skeleton_action(set_v1), SKELETON_V1, skeleton_value(sub_value)], st)
        call check(error, buffer_string() == "<v1>[sub]", "values: "//buffer_string())
    end subroutine test_values

    !> skeleton_pair_insert_maybe: plain or paired.
    subroutine test_pair(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st

        call temp_buffer("")
        last_command_event = value_type(40_int64)
        call skeleton_pair_insert_maybe(prefix_arg_type(PREFIX_NONE, 0_int64), st)
        call check(error, buffer_string() == "(", "unpaired: "//buffer_string())
        if (allocated(error)) return
        call temp_buffer("")
        skeleton_pair = .true.
        call skeleton_pair_insert_maybe(prefix_arg_type(PREFIX_NONE, 0_int64), st)
        skeleton_pair = .false.
        call check(error, buffer_string() == "()", "paired: "//buffer_string())
        if (allocated(error)) return
        call check(error, point() == 2_int64, "point inside")
    end subroutine test_pair
end module test_skeleton_mod
