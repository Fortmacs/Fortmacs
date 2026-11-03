! test_button_mod.f90 -- tests for buttons (button_mod).
! Ports GNU Emacs 31.1 test/lisp/button-tests.el in part; button--help-echo-form
! is Not applicable (no run-time Lisp), and the widget and `buttonize' (string)
! parts are Deferred with widgets and strings as buttons.
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

!> Ports button-tests.el's button-at, button--help-echo-string,
!> button--help-echo-function and button--preserve-help-echo (its
!> buttonize-region parts); own tests for motion, pushing, types,
!> `describe_button', `button_mode' and the keys of Help mode.
module test_button_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_mod, only: status_type, FM_USER_ERROR
    use handle_mod, only: handle_type
    use value_mod, only: value_type, vtag, xstring, xfixnum, V_STRING
    use buffer_mod, only: get_or_make_buffer, get_buffer, set_buffer, current_buffer, erase_buffer
    use insdel_mod, only: insert
    use editfns_mod, only: point, goto_char, point_min, point_max, buffer_string, widen
    use textprop_mod, only: get_text_property, put_text_property
    use itree_mod, only: is_overlay, overlay_buffer, overlay_start
    use keymap_mod, only: make_sparse_keymap, keymap_set, key_parse
    use keyboard_mod, only: unread_command_events, discard_input, last_message, command_loop
    use frame_mod, only: selected_window, selected_frame, ensure_frame_windows
    use window_mod, only: delete_other_windows, resize_window_tree, switch_to_buffer
    use help_mod, only: register_keymap_name
    use modes_mod, only: minor_modes
    use bindings_mod, only: init_bindings
    use help_mode_mod, only: help_mode
    use button_mod, only: button_type, button_kind_type, default_button, button_face, button_at, &
                          button_start, button_end, button_label, &
                          button_help_echo, insert_button, insert_text_button, make_text_button, next_button, &
                          previous_button, forward_button, backward_button, push_button, has_button_type, &
                          is_button_subtype, describe_button, buttonize_region, unbuttonize_region
    implicit none(type, external)
    private

    public :: collect_button_mod

    !> What the test actions and help procedures saw.
    integer :: pushes = 0
    integer(int64) :: pushed_start = 0
    type(value_type) :: pushed_data
    type(handle_type) :: help_window, help_object
    integer(int64) :: help_position = 0

    !> The curved quotes of format_message.
    character(len=*), parameter :: LQ = char(226)//char(128)//char(152), RQ = char(226)//char(128)//char(153)

    !> Button types of the tests.
    type(button_kind_type), target :: test_button, test_sub_button

contains

    !> Collect all exported unit tests.
    subroutine collect_button_mod(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("button-at", test_button_at), &
                    new_unittest("button--help-echo-string", test_help_echo_string), &
                    new_unittest("button--help-echo-function", test_help_echo_function), &
                    new_unittest("button--preserve-help-echo", test_preserve_help_echo), &
                    new_unittest("button-forward-backward", test_forward_backward), &
                    new_unittest("button-skip", test_skip), &
                    new_unittest("button-push", test_push), &
                    new_unittest("button-types", test_types), &
                    new_unittest("button-describe", test_describe), &
                    new_unittest("button-mode", test_button_mode), &
                    new_unittest("button-help-mode-keys", test_help_mode_keys) &
                    ]
    end subroutine collect_button_mod

    !> A fresh empty buffer (with-temp-buffer).
    subroutine temp_buffer()
        type(handle_type) :: buffer
        type(status_type) :: st

        call init_bindings()
        call get_or_make_buffer(" *button-test*", buffer, st)
        call set_buffer(buffer, st)
        call widen()
        call erase_buffer(st)
        pushes = 0
        pushed_data = value_type()
    end subroutine temp_buffer

    !> A test action: count the pushes and keep the data.
    recursive subroutine count_push(button, data, status)
        type(button_type), intent(in) :: button
        type(value_type), intent(in) :: data
        type(status_type), intent(out) :: status

        status = status_type()
        pushes = pushes + 1
        pushed_start = button_start(button)
        pushed_data = data
    end subroutine count_push

    !> Another test action: keep the data without counting.
    recursive subroutine other_push(button, data, status)
        type(button_type), intent(in) :: button
        type(value_type), intent(in) :: data
        type(status_type), intent(out) :: status

        status = status_type()
        pushed_start = button_start(button)
        pushed_data = data
    end subroutine other_push

    !> A test help-echo procedure: record its arguments.
    subroutine record_help(window, object, position, help)
        type(handle_type), intent(in) :: window, object
        integer(int64), intent(in) :: position
        character(len=:), allocatable, intent(out) :: help

        help_window = window
        help_object = object
        help_position = position
        help = "text: \<button_tests_map>\[ignore]"
    end subroutine record_help

    !> Define the test map (button-tests--map) and the test types.
    subroutine define_test_things()
        type(handle_type), save :: map
        type(status_type) :: st

        if (map%index == 0) then
            call make_sparse_keymap(map)
            call keymap_set(map, "x", "ignore", st)
            call register_keymap_name("button_tests_map", map)
        end if

        call test_button%define("test")
        test_button%action => count_push
        test_button%help_echo = "test help"
        call test_sub_button%define("test_sub", supertype=test_button)
    end subroutine define_test_things

    subroutine test_button_at(error)
        type(error_type), allocatable, intent(out) :: error
        type(button_type) :: button, marker, overlay
        type(status_type) :: st
        logical :: found

        call temp_buffer()
        call button_at(point(), button, found)
        call check(error,.not. found, "no button in an empty buffer")
        if (allocated(error)) return
        call insert_text_button("text button", button, st)
        call button_at(point() - 1, marker, found)
        call check(error, found .and. .not. is_overlay(marker%overlay), "a text button")
        if (allocated(error)) return
        call check(error, button_end(button) == button_end(marker) .and. button_end(marker) == point(), "its end")
        if (allocated(error)) return
        call insert_button("overlay button", button, st)
        call button_at(point() - 1, overlay, found)
        call check(error, found .and. is_overlay(overlay%overlay), "an overlay button")
        if (allocated(error)) return
        call check(error, button%overlay%index == overlay%overlay%index, "the same overlay")
    end subroutine test_button_at

    subroutine test_help_echo_string(error)
        type(error_type), allocatable, intent(out) :: error
        type(button_type) :: button
        type(status_type) :: st

        call temp_buffer()
        call define_test_things()
        ! Text property buttons.
        call insert_text_button("text", button, st, help_echo="text: \<button_tests_map>\[ignore]")
        call check(error, button_help_echo(button), "text: x")
        if (allocated(error)) return
        ! Overlay buttons.
        call insert_button("overlay", button, st, help_echo="overlay: \<button_tests_map>\[ignore]")
        call check(error, button_help_echo(button), "overlay: x")
    end subroutine test_help_echo_string

    subroutine test_help_echo_function(error)
        type(error_type), allocatable, intent(out) :: error
        type(button_type) :: button
        type(status_type) :: st
        type(handle_type) :: window, buffer
        integer(int64) :: opos

        call temp_buffer()
        call define_test_things()
        ! Text property buttons.
        opos = point()
        call insert_text_button("text", button, st, help_echo_function=record_help)
        call check(error, button_help_echo(button), "text: x")
        if (allocated(error)) return
        window = selected_window()
        buffer = current_buffer()
        call check(error, help_window%index == window%index .and. help_object%index == buffer%index .and. &
                   help_position == opos, "text button arguments")
        if (allocated(error)) return
        ! Overlay buttons.
        opos = point()
        call insert_button("overlay", button, st, help_echo_function=record_help)
        call check(error, button_help_echo(button), "text: x")
        if (allocated(error)) return
        call check(error, is_overlay(help_object) .and. help_object%index == button%overlay%index, "the overlay")
        if (allocated(error)) return
        window = overlay_buffer(help_object)
        call check(error, window%index == buffer%index .and. overlay_start(help_object) == opos .and. &
                   help_position == opos, "overlay button arguments")
    end subroutine test_help_echo_function

    !> The string value of text property `help_echo' at POS, or "nil".
    function help_echo_at(pos) result(text)
        integer(int64), intent(in) :: pos
        character(len=:), allocatable :: text
        type(value_type) :: val
        type(status_type) :: st

        call get_text_property(pos, value_type(name="help_echo"), val, st)
        text = "nil"
        if (vtag(val) == V_STRING) text = xstring(val)
    end function help_echo_at

    subroutine test_preserve_help_echo(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st

        ! buttonize-region.
        call temp_buffer()
        call insert("button text", st)
        call put_text_property(point_min(), point(), value_type(name="help_echo"), value_type(text="help text"), st)
        call buttonize_region(point_min(), point(), count_push, st)
        call check(error, help_echo_at(point_min()), "help text")
        if (allocated(error)) return
        ! unbuttonize-region should not clear the property either.
        call unbuttonize_region(point_min(), point(), st)
        call check(error, help_echo_at(point_min()), "help text")
        if (allocated(error)) return
        ! unbuttonize-region should still clear properties applied with
        ! buttonize.
        call temp_buffer()
        call insert("button text", st)
        call buttonize_region(point_min(), point(), count_push, st, help_echo="help text")
        call check(error, help_echo_at(point_min()), "help text")
        if (allocated(error)) return
        call unbuttonize_region(point_min(), point(), st)
        call check(error, help_echo_at(point_min()), "nil")
    end subroutine test_preserve_help_echo

    !> A buffer "a b c dd e f" with buttons on b, dd (an overlay) and f.
    subroutine three_buttons()
        type(button_type) :: button
        type(status_type) :: st

        call temp_buffer()
        call insert("a ", st)
        call insert_text_button("b", button, st)
        call insert(" c ", st)
        call insert_button("dd", button, st)
        call insert(" e ", st)
        call insert_text_button("f", button, st)
        call goto_char(point_min())
    end subroutine three_buttons

    subroutine test_forward_backward(error)
        type(error_type), allocatable, intent(out) :: error
        type(button_type) :: button
        type(status_type) :: st
        logical :: found

        call three_buttons()
        call forward_button(1_int64, st)
        call check(error, point() == 3, "to b")
        if (allocated(error)) return
        call forward_button(1_int64, st, button=button)
        call check(error, point() == 7 .and. button_label(button) == "dd", "to dd")
        if (allocated(error)) return
        call forward_button(1_int64, st)
        call check(error, point() == 12, "to f")
        if (allocated(error)) return
        call forward_button(1_int64, st)
        call check(error, st%code == FM_USER_ERROR .and. st%detail == "No more buttons", "No more buttons")
        if (allocated(error)) return
        call forward_button(1_int64, st, wrap=.true.)
        call check(error, point() == 3, "wrapped to b")
        if (allocated(error)) return
        call backward_button(1_int64, st, wrap=.true.)
        call check(error, point() == 12, "wrapped back to f")
        if (allocated(error)) return
        call backward_button(2_int64, st)
        call check(error, point() == 3, "back two to b")
        if (allocated(error)) return
        call forward_button(-1_int64, st, no_error=.true., found=found)
        call check(error,.not. found .and. st%ok() .and. point() == 3, "no error")
        if (allocated(error)) return
        call goto_char(8_int64)
        call forward_button(0_int64, st)
        call check(error, point() == 7, "to the start of dd")
        if (allocated(error)) return
        call previous_button(8_int64, button, found, count_current=.true.)
        call check(error, found .and. button_start(button) == 7, "previous_button counting dd")
        if (allocated(error)) return
        call next_button(1_int64, button, found)
        call check(error, found .and. button_start(button) == 3, "next_button")
    end subroutine test_forward_backward

    subroutine test_skip(error)
        type(error_type), allocatable, intent(out) :: error
        type(button_type) :: button
        type(status_type) :: st
        logical :: found

        call three_buttons()
        call button_at(7_int64, button, found)
        call button%set_skip(.true., st)
        call goto_char(3_int64)
        call forward_button(1_int64, st)
        call check(error, point() == 12, "dd skipped")
    end subroutine test_skip

    subroutine test_push(error)
        type(error_type), allocatable, intent(out) :: error
        type(button_type) :: button
        type(status_type) :: st
        logical :: pushed

        call temp_buffer()
        call define_test_things()
        call insert_text_button("go", button, st, action=count_push, button_data=value_type(42_int64))
        call push_button(st, position=1_int64, pushed=pushed)
        call check(error, pushed .and. pushes == 1 .and. xfixnum(pushed_data) == 42 .and. pushed_start == 1, &
                   "the button's own action")
        if (allocated(error)) return
        call push_button(st, position=point_max(), pushed=pushed)
        call check(error,.not. pushed .and. pushes == 1, "no button at the end")
        if (allocated(error)) return
        call insert_button("typed", button, st, kind=test_button)
        call goto_char(4_int64)
        call push_button(st, pushed=pushed)
        call check(error, pushed .and. pushes == 2 .and. .not. allocated(pushed_data%a), "the type's action")
        if (allocated(error)) return
        ! RET on a button runs push_button through the button's keymap.
        call fresh_window()
        call temp_buffer()
        call switch_to_buffer(" *button-test*", st)
        call insert_text_button("go", button, st, kind=test_button)
        call goto_char(1_int64)
        call type_keys("RET")
        call command_loop(st)
        call check(error, pushes == 1, "RET")
    end subroutine test_push

    subroutine test_types(error)
        type(error_type), allocatable, intent(out) :: error
        type(button_type) :: button
        type(button_kind_type), pointer :: kind
        type(status_type) :: st

        call temp_buffer()
        call define_test_things()
        call check(error, is_button_subtype(test_sub_button, test_button) .and. &
                   is_button_subtype(test_sub_button, default_button) .and. &
                   .not. is_button_subtype(test_button, test_sub_button), "subtypes")
        if (allocated(error)) return
        call check(error, associated(test_sub_button%action, count_push) .and. test_sub_button%help_echo == "test help", &
                   "inherited components")
        if (allocated(error)) return
        call insert_text_button("plain", button, st)
        kind => button%get_kind()
        call check(error, trim(kind%name) == "button" .and. button%get_help_echo() == "mouse-2, RET: Push this button", &
                   "the default type")
        if (allocated(error)) return
        call button%set_kind(test_sub_button, st)
        call check(error, has_button_type(button, test_button) .and. button_help_echo(button) == "test help", &
                   "the type changed")
        if (allocated(error)) return
        call button%set_help_echo("own help", st)
        call check(error, button_help_echo(button), "own help")
        if (allocated(error)) return
        call check(error, associated(button%get_face(), button_face) .and. associated(button%get_action(), count_push) .and. &
                   .not. button%get_follow_link(), "the accessors")
        if (allocated(error)) return
        call button%set_follow_link(.true., st)
        call button%set_action(other_push, st)
        call check(error, button%get_follow_link() .and. associated(button%get_action(), other_push), "the setters")
    end subroutine test_types

    subroutine test_describe(error)
        type(error_type), allocatable, intent(out) :: error
        type(button_type) :: button
        type(status_type) :: st
        type(handle_type) :: old
        logical :: described
        character(len=:), allocatable :: text

        call fresh_window()
        call temp_buffer()
        call define_test_things()
        call insert_text_button("go", button, st, kind=test_button)
        call describe_button(st, position=1_int64, described=described)
        old = current_buffer()
        call set_buffer(get_buffer("*Help*"), st)
        text = buffer_string()
        call set_buffer(old, st)
        call check(error, described .and. text == "This button"//RQ//"s type is "//LQ//"test"//RQ//"."//achar(10), text)
        if (allocated(error)) return
        call describe_button(st, position=point_max(), described=described)
        call check(error,.not. described, "no button")
    end subroutine test_describe

    subroutine test_button_mode(error)
        type(error_type), allocatable, intent(out) :: error
        type(button_type) :: button
        type(status_type) :: st
        logical :: found

        call three_buttons()
        call minor_modes%button_mode%enable(st)
        call check(error, st%ok(), "enabled")
        if (allocated(error)) return
        call minor_modes%button_mode%disable(st)
        call next_button(1_int64, button, found, count_current=.true.)
        call check(error,.not. found .and. buffer_string() == "a b c dd e f", "the buttons removed")
    end subroutine test_button_mode

    subroutine test_help_mode_keys(error)
        type(error_type), allocatable, intent(out) :: error
        type(button_type) :: button
        type(status_type) :: st

        call fresh_window()
        call temp_buffer()
        call define_test_things()
        call switch_to_buffer(" *button-test*", st)
        call insert("x ", st)
        call insert_text_button("one", button, st, kind=test_button)
        call insert(" ", st)
        call insert_text_button("two", button, st, kind=test_button)
        call goto_char(1_int64)
        call help_mode(st)
        call type_keys("TAB")
        call command_loop(st)
        call check(error, point() == 3 .and. last_message == "test help", "TAB")
        if (allocated(error)) return
        call type_keys("TAB TAB")
        call command_loop(st)
        call check(error, point() == 3, "TAB wraps")
        if (allocated(error)) return
        call type_keys("<backtab>")
        call command_loop(st)
        call check(error, point() == 7, "<backtab> wraps")
    end subroutine test_help_mode_keys

    !> One window of 80 columns showing *scratch*, no pending input.
    subroutine fresh_window()
        type(handle_type) :: buffer
        type(status_type) :: st

        call init_bindings()
        call discard_input()
        call get_or_make_buffer("*scratch*", buffer, st)
        call set_buffer(buffer, st)
        call ensure_frame_windows()
        call delete_other_windows(selected_frame%selected_window, st)
        call resize_window_tree(selected_frame%root_window, 0, 0, 23, 80, 80)
        call switch_to_buffer("*scratch*", st)
        last_message = ""
    end subroutine fresh_window

    subroutine type_keys(keys)
        character(len=*), intent(in) :: keys
        type(value_type), allocatable :: evs(:)
        type(status_type) :: st

        call key_parse(keys, evs, st)
        call unread_command_events(evs)
    end subroutine type_keys
end module test_button_mod
