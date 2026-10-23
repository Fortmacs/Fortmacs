! test_easy_mmode_fm.f90 -- tests for minor modes (easy_mmode_fm),
! Auto Fill mode and the mode line's lighters.  Includes cases ported
! from GNU Emacs 31.1 test/lisp/emacs-lisp/easy-mmode-tests.el.
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

!> Buffers are global state; every test creates its buffers and kills
!> them before returning.
module test_easy_mmode_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_fm, only: status_type
    use handle_fm, only: handle_type
    use lisp_fm, only: value_type, nil, nilp, make_fixnum, t_value, list
    use lread_fm, only: sym
    use buffer_fm, only: get_buffer_create, kill_buffer, set_buffer, cur, auto_fill_function, &
                         transient_mark_mode
    use data_fm, only: symbol_value, value_eq, local_variable_p
    use callint_fm, only: current_prefix_arg
    use keyboard_fm, only: last_message
    use keymap_fm, only: keymap_set, key_binding, key_parse
    use xdisp_fm, only: mode_line_modes, mode_line_text
    use easy_mmode_fm, only: define_minor_mode, set_minor_mode, minor_mode_command, minor_mode_enabled_p, &
                             minor_mode_map, easy_mmode_pretty_mode_name
    use simple_fm, only: auto_fill_mode, syms_of_simple_modes
    implicit none(type, external)
    private

    public :: collect_easy_mmode_fm

    !> The last state the test mode's body saw.
    logical :: body_state

contains

    subroutine collect_easy_mmode_fm(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)
        testsuite = [ &
                    new_unittest("easy-mmode--minor-mode", test_minor_mode), &
                    new_unittest("easy-mmode-pretty-mode-name", test_pretty_name), &
                    new_unittest("minor-mode-local-and-map", test_local_and_map), &
                    new_unittest("auto-fill-mode-local", test_auto_fill), &
                    new_unittest("global-minor-mode", test_global), &
                    new_unittest("mode-line-text-modes", test_mode_line_text) &
                    ]
    end subroutine collect_easy_mmode_fm

    subroutine test_body(enable, status)
        logical, intent(in) :: enable
        type(status_type), intent(out) :: status
        status = status_type()
        body_state = enable
    end subroutine test_body

    !> Whether the test mode's variable is t.
    logical function test_mode_on()
        type(value_type) :: v
        type(status_type) :: st
        call symbol_value(sym("easy-mmode-test-mode"), v, st)
        test_mode_on = value_eq(v, t_value())
    end function test_mode_on

    !> easy-mmode-tests.el: easy-mmode--minor-mode ('toggle is TOGGLE).
    subroutine test_minor_mode(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: a
        type(status_type) :: st
        call define_minor_mode("easy-mmode-test-mode", "", test_body)
        call get_buffer_create("easy-mmode-test", a, st)
        call set_buffer(a, st)
        call check(error,.not. test_mode_on(), "off at first")
        if (allocated(error)) return
        call set_minor_mode("easy-mmode-test-mode", st, nil())
        call check(error, test_mode_on() .and. body_state, "nil turns it on")
        if (allocated(error)) return
        call set_minor_mode("easy-mmode-test-mode", st, make_fixnum(-33_int64))
        call check(error,.not. test_mode_on() .and. .not. body_state, "-33 turns it off")
        if (allocated(error)) return
        call set_minor_mode("easy-mmode-test-mode", st, make_fixnum(33_int64))
        call check(error, test_mode_on(), "33 turns it on")
        if (allocated(error)) return
        call set_minor_mode("easy-mmode-test-mode", st, toggle=.true.)
        call check(error,.not. test_mode_on(), "toggle")
        if (allocated(error)) return
        call set_minor_mode("easy-mmode-test-mode", st, toggle=.true.)
        call check(error, test_mode_on(), "toggle again")
        call kill_buffer(a, st)
    end subroutine test_minor_mode

    subroutine test_pretty_name(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, easy_mmode_pretty_mode_name("auto-fill-mode"), "Auto-Fill mode")
        if (allocated(error)) return
        call check(error, easy_mmode_pretty_mode_name("foo-bar-minor-mode"), "Foo-Bar minor mode")
        if (allocated(error)) return
        call check(error, easy_mmode_pretty_mode_name("global-foo-mode"), "Global Foo mode")
        if (allocated(error)) return
        call check(error, easy_mmode_pretty_mode_name("toggle-truncate-lines"), "Truncate-Lines mode")
    end subroutine test_pretty_name

    !> A buffer-local mode: on in one buffer only, with its lighter and
    !> keymap active there.
    subroutine test_local_and_map(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: a, b
        type(status_type) :: st
        type(value_type) :: binding
        type(value_type), allocatable :: key(:)
        call define_minor_mode("easy-mmode-test-lit-mode", " Lit")
        call keymap_set(minor_mode_map("easy-mmode-test-lit-mode"), "C-c l", "forward-char", st)
        call get_buffer_create("easy-mmode-test-A", a, st)
        call get_buffer_create("easy-mmode-test-B", b, st)
        call set_buffer(a, st)
        current_prefix_arg = nil()
        call minor_mode_command("easy-mmode-test-lit-mode", st)
        call check(error, minor_mode_enabled_p("easy-mmode-test-lit-mode"), "on in A")
        if (allocated(error)) return
        call check(error, last_message, "Easy-Mmode-Test-Lit mode enabled in current buffer")
        if (allocated(error)) return
        call check(error, mode_line_modes(a%index), "Fundamental Lit")
        if (allocated(error)) return
        call key_parse("C-c l", key, st)
        call key_binding(key, binding, st)
        call check(error, value_eq(binding, sym("forward-char")), "minor mode map active")
        if (allocated(error)) return
        call set_buffer(b, st)
        call check(error,.not. minor_mode_enabled_p("easy-mmode-test-lit-mode"), "off in B")
        if (allocated(error)) return
        call check(error, mode_line_modes(b%index), "Fundamental")
        if (allocated(error)) return
        call key_binding(key, binding, st)
        call check(error, nilp(binding), "map inactive in B")
        call kill_buffer(a, st)
        call kill_buffer(b, st)
    end subroutine test_local_and_map

    !> auto-fill-mode sets auto-fill-function in the current buffer only.
    subroutine test_auto_fill(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: a, b
        type(status_type) :: st
        call get_buffer_create("easy-mmode-fill-A", a, st)
        call get_buffer_create("easy-mmode-fill-B", b, st)
        call set_buffer(a, st)
        call auto_fill_mode(make_fixnum(1_int64))
        call check(error, associated(auto_fill_function%value), "on in A")
        if (allocated(error)) return
        call check(error, local_variable_p(sym("auto-fill-function")), "auto-fill-function local")
        if (allocated(error)) return
        call check(error, mode_line_modes(a%index), "Fundamental Fill")
        if (allocated(error)) return
        call set_buffer(b, st)
        call check(error,.not. associated(auto_fill_function%value), "off in B")
        if (allocated(error)) return
        call set_buffer(a, st)
        call auto_fill_mode(nil(), .true.)
        call check(error,.not. associated(auto_fill_function%value), "toggled off in A")
        call kill_buffer(a, st)
        call kill_buffer(b, st)
    end subroutine test_auto_fill

    !> transient-mark-mode is global.
    subroutine test_global(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        logical :: saved
        call syms_of_simple_modes()
        saved = transient_mark_mode%value
        current_prefix_arg = make_fixnum(-1_int64)
        call minor_mode_command("transient-mark-mode", st)
        current_prefix_arg = nil()
        call check(error,.not. transient_mark_mode%value, "turned off")
        if (allocated(error)) return
        call check(error, last_message, "Transient-Mark mode disabled")
        if (allocated(error)) return
        call set_minor_mode("transient-mark-mode", st, make_fixnum(1_int64))
        call check(error, transient_mark_mode%value, "turned on")
        transient_mark_mode = saved
    end subroutine test_global

    !> mode_line_text shows the modes it is given.
    subroutine test_mode_line_text(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: s
        s = mode_line_text("notes", .false., .false., "All", 1_int64, 80, "Text Fill")
        call check(error, index(s, "(Text Fill)") > 0, s)
    end subroutine test_mode_line_text
end module test_easy_mmode_fm
