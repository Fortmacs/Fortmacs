! test_repeat_fm.f90 -- tests for repeat_fm.
! Ported from GNU Emacs 31.1 test/lisp/repeat-tests.el.
!
! Copyright (C) 2021-2026 Free Software Foundation, Inc.
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

!> Ports repeat-tests-check-key, -exit-command, -exit-key, -keep-prefix,
!> -continue, -continue-another and -hints (the hint text without its
!> faces).  The test maps' :repeat properties are set on the commands'
!> components, as defvar-keymap :repeat does; own test of C-x z.
module test_repeat_fm
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_fm, only: status_type
    use handle_fm, only: handle_type, null_handle
    use lisp_fm, only: value_type, command_type, nil, PROPERTY_UNSET, PROPERTY_TRUE, PROPERTY_NO
    use buffer_fm, only: get_buffer_create, set_buffer, erase_buffer, generate_new_buffer_name
    use editfns_fm, only: buffer_string
    use keymap_fm, only: make_sparse_keymap, key_parse, keymap_set, use_local_map, set_repeat_map
    use keyboard_fm, only: execute_kbd_macro
    use callint_fm, only: current_prefix_arg, prefix_numeric_value
    use modes_fm, only: minor_modes
    use bindings_fm, only: init_bindings
    use test_support, only: show_alone
    use repeat_fm, only: repeat_echo_function, repeat_check_key, repeat_exit_key, repeat_keep_prefix, &
                         repeat_echo_message_string, repeat_in_progress
    implicit none(type, external)
    private

    public :: collect_repeat_fm

    !> The commands called, as "ARG NAME" (repeat-tests-calls).
    character(len=:), allocatable :: calls
    type(command_type), target, save :: cmd_a, cmd_b, cmd_c, cmd_d, cmd_e, cmd_g, cmd_o, cmd_q, cmd_s, cmd_t, cmd_u
    type(handle_type) :: global_map, another_repeat_map, repeat_map

contains

    subroutine collect_repeat_fm(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("repeat-tests-check-key", test_check_key), &
                    new_unittest("repeat-tests-exit-command", test_exit_command), &
                    new_unittest("repeat-tests-exit-key", test_exit_key), &
                    new_unittest("repeat-tests-keep-prefix", test_keep_prefix), &
                    new_unittest("repeat-tests-continue", test_continue), &
                    new_unittest("repeat-tests-continue-another", test_continue_another), &
                    new_unittest("repeat-tests-hints", test_hints), &
                    new_unittest("repeat-command", test_repeat_command) &
                    ]
    end subroutine collect_repeat_fm

    !> Record a call of command NAME with the numeric prefix argument.
    subroutine record(name)
        character(len=*), intent(in) :: name
        character(len=24) :: buf

        write (buf, "(i0)") prefix_numeric_value(current_prefix_arg)
        calls = calls//"("//trim(buf)//" "//name//")"
    end subroutine record

    recursive subroutine call_a(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call record("a")
    end subroutine call_a

    recursive subroutine call_b(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call record("b")
    end subroutine call_b

    recursive subroutine call_c(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call record("c")
    end subroutine call_c

    recursive subroutine call_d(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call record("d")
    end subroutine call_d

    recursive subroutine call_e(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call record("e")
    end subroutine call_e

    recursive subroutine call_g(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call record("g")
    end subroutine call_g

    recursive subroutine call_o(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call record("o")
    end subroutine call_o

    recursive subroutine call_q(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call record("q")
    end subroutine call_q

    recursive subroutine call_s(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call record("s")
    end subroutine call_s

    recursive subroutine call_t(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call record("t")
    end subroutine call_t

    recursive subroutine call_u(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call record("u")
    end subroutine call_u

    !> The test commands and maps (repeat-tests-global-map,
    !> repeat-tests-another-repeat-map, repeat-tests-repeat-map).
    subroutine setup()
        type(status_type) :: st

        call init_bindings()
        cmd_a = command_type("repeat_tests_call_a", call_a)
        cmd_b = command_type("repeat_tests_call_b", call_b)
        cmd_c = command_type("repeat_tests_call_c", call_c)
        cmd_d = command_type("repeat_tests_call_d", call_d)
        cmd_e = command_type("repeat_tests_call_e", call_e)
        cmd_g = command_type("repeat_tests_call_g", call_g)
        cmd_o = command_type("repeat_tests_call_o", call_o)
        cmd_q = command_type("repeat_tests_call_q", call_q)
        cmd_s = command_type("repeat_tests_call_s", call_s)
        cmd_t = command_type("repeat_tests_call_t", call_t)
        cmd_u = command_type("repeat_tests_call_u", call_u)
        call cmd_a%define()
        call cmd_b%define()
        call cmd_c%define()
        call cmd_d%define()
        call cmd_e%define()
        call cmd_g%define()
        call cmd_o%define()
        call cmd_q%define()
        call cmd_s%define()
        call cmd_t%define()
        call cmd_u%define()
        call make_sparse_keymap(global_map)
        call keymap_set(global_map, "C-x w a", "repeat_tests_call_a", st)
        call keymap_set(global_map, "C-M-a", "repeat_tests_call_a", st)
        call keymap_set(global_map, "C-M-b", "repeat_tests_call_b", st)
        call keymap_set(global_map, "C-M-e", "repeat_tests_call_e", st)
        call keymap_set(global_map, "C-M-g", "repeat_tests_call_g", st)
        call keymap_set(global_map, "C-M-o", "repeat_tests_call_o", st)
        call keymap_set(global_map, "C-M-s", "repeat_tests_call_s", st)
        call keymap_set(global_map, "C-M-u", "repeat_tests_call_u", st)
        ! repeat-tests-another-repeat-map, :repeat (:enter (s) :continue
        ! (e o u) :hints ((t . "test") (o . "another test"))).
        call make_sparse_keymap(another_repeat_map)
        call keymap_set(another_repeat_map, "s", "ignore", st)
        call keymap_set(another_repeat_map, "t", "repeat_tests_call_t", st)
        call keymap_set(another_repeat_map, "C-M-o", "repeat_tests_call_o", st)
        call keymap_set(another_repeat_map, "C-M-u", "repeat_tests_call_u", st)
        ! repeat-tests-repeat-map, :repeat (:enter (a) :continue (e o)
        ! :exit (q)).
        call make_sparse_keymap(repeat_map)
        call keymap_set(repeat_map, "a", "ignore", st)
        call keymap_set(repeat_map, "c", "repeat_tests_call_c", st)
        call keymap_set(repeat_map, "d", "repeat_tests_call_d", st)
        call keymap_set(repeat_map, "C-M-o", "repeat_tests_call_o", st)
        call keymap_set(repeat_map, "q", "repeat_tests_call_q", st)
        ! The expansion of the two defvar-keymaps, in order.
        cmd_s%repeat_map = another_repeat_map
        cmd_e%repeat_continue = [another_repeat_map]
        cmd_o%repeat_continue = [another_repeat_map]
        cmd_u%repeat_continue = [another_repeat_map]
        call set_repeat_map(another_repeat_map)
        cmd_t%repeat_hint = "test"
        cmd_o%repeat_hint = "another test"
        cmd_a%repeat_map = repeat_map
        cmd_e%repeat_continue = [repeat_map, cmd_e%repeat_continue]
        cmd_o%repeat_continue = [repeat_map, cmd_o%repeat_continue]
        call set_repeat_map(repeat_map, exit=["repeat_tests_call_q"])
        ! Test using a variable instead of the symbol:
        cmd_b%repeat_map = repeat_map
        cmd_g%repeat_continue_all = .true.
        repeat_echo_function => null()
        repeat_check_key = .true.
    end subroutine setup

    !> with-repeat-mode: Repeat mode on, a temporary buffer with MAP as
    !> its local map.
    subroutine begin(map)
        type(handle_type), intent(in) :: map
        type(handle_type) :: h
        type(status_type) :: st

        call minor_modes%repeat_mode%enable(st)
        call get_buffer_create(generate_new_buffer_name(" *repeat-test*"), h, st)
        call set_buffer(h, st)
        call show_alone()
        call use_local_map(map, st)
    end subroutine begin

    subroutine finish()
        type(status_type) :: st

        call minor_modes%repeat_mode%disable(st)
        call use_local_map(null_handle(), st)
    end subroutine finish

    !> Run KEYS and check the calls and the inserted text
    !> (repeat-tests--check).
    subroutine check_keys(error, keys, expected_calls, inserted)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), intent(in) :: keys, expected_calls, inserted
        type(value_type), allocatable :: events(:)
        type(status_type) :: st

        calls = ""
        call erase_buffer(st)
        call key_parse(keys, events, st)
        call execute_kbd_macro(events, nil(), st)
        call check(error, calls, expected_calls, keys//": "//calls)
        if (allocated(error)) return
        ! Check for self-inserting keys
        call check(error, buffer_string(), inserted, keys//": "//buffer_string())
    end subroutine check_keys

    subroutine test_check_key(error)
        type(error_type), allocatable, intent(out) :: error

        call setup()
        call begin(global_map)

        body: block
            repeat_check_key = .true.
            call check_keys(error, "C-x w a c d z", "(1 a)(1 c)(1 d)", "z")
            if (allocated(error)) exit body
            call check_keys(error, "C-M-a c d z", "(1 a)(1 c)(1 d)", "z")
            if (allocated(error)) exit body
            call check_keys(error, "C-M-b c d z", "(1 b)", "cdz")
            if (allocated(error)) exit body
            cmd_b%repeat_check_key = PROPERTY_NO
            call check_keys(error, "C-M-b c d z", "(1 b)(1 c)(1 d)", "z")
            cmd_b%repeat_check_key = PROPERTY_UNSET
            if (allocated(error)) exit body
            repeat_check_key = .false.
            call check_keys(error, "C-M-b c d z", "(1 b)(1 c)(1 d)", "z")
            if (allocated(error)) exit body
            cmd_b%repeat_check_key = PROPERTY_TRUE
            call check_keys(error, "C-M-b c d z", "(1 b)", "cdz")
            cmd_b%repeat_check_key = PROPERTY_UNSET
        end block body

        call finish()
    end subroutine test_check_key

    subroutine test_exit_command(error)
        type(error_type), allocatable, intent(out) :: error

        call setup()
        call begin(global_map)
        ! 'c' doesn't continue since 'q' exited
        call check_keys(error, "C-x w a c d q c", "(1 a)(1 c)(1 d)(1 q)", "c")
        call finish()
    end subroutine test_exit_command

    subroutine test_exit_key(error)
        type(error_type), allocatable, intent(out) :: error

        call setup()
        call begin(global_map)

        body: block
            if (allocated(repeat_exit_key)) deallocate (repeat_exit_key)
            call check_keys(error, "C-x w a c d c RET z", "(1 a)(1 c)(1 d)(1 c)", achar(10)//"z")
            if (allocated(error)) exit body
            repeat_exit_key = "<return>"
            call check_keys(error, "C-x w a c d c <return> z", "(1 a)(1 c)(1 d)(1 c)", "z")
        end block body

        if (allocated(repeat_exit_key)) deallocate (repeat_exit_key)
        call finish()
    end subroutine test_exit_key

    subroutine test_keep_prefix(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st

        call setup()
        call begin(global_map)

        body: block
            call check_keys(error, "C-x w a c d c z", "(1 a)(1 c)(1 d)(1 c)", "z")
            if (allocated(error)) exit body
            repeat_keep_prefix = .false.
            call check_keys(error, "C-2 C-x w a c d c z", "(2 a)(1 c)(1 d)(1 c)", "z")
            if (allocated(error)) exit body
            call check_keys(error, "C-2 C-x w a C-3 z", "(2 a)", "zzz")
            if (allocated(error)) exit body
            ! Fixed in bug#51281 and bug#55986
            repeat_keep_prefix = .true.
            ! Re-enable to take effect.
            call minor_modes%repeat_mode%disable(st)
            call minor_modes%repeat_mode%enable(st)
            call check_keys(error, "C-2 C-x w a c d c z", "(2 a)(2 c)(2 d)(2 c)", "z")
        end block body

        repeat_keep_prefix = .false.
        call finish()
    end subroutine test_keep_prefix

    subroutine test_continue(error)
        type(error_type), allocatable, intent(out) :: error

        call setup()
        call begin(global_map)
        repeat_check_key = .false.

        body: block
            ! Global 'C-M-g' used as continue
            call check_keys(error, "C-M-a c C-M-g c z", "(1 a)(1 c)(1 g)(1 c)", "z")
            if (allocated(error)) exit body
            ! 'C-M-e' and 'C-M-o' used as continue
            call check_keys(error, "C-M-a c C-M-e C-M-o c z", "(1 a)(1 c)(1 e)(1 o)(1 c)", "z")
            if (allocated(error)) exit body
            ! 'C-M-e' should not activate
            call check_keys(error, "C-M-e c z", "(1 e)", "cz")
            if (allocated(error)) exit body
            ! 'C-M-o' should also activate
            call check_keys(error, "C-M-o c z", "(1 o)(1 c)", "z")
        end block body

        call finish()
    end subroutine test_continue

    subroutine test_continue_another(error)
        type(error_type), allocatable, intent(out) :: error

        call setup()
        call begin(global_map)
        repeat_check_key = .false.

        body: block
            ! First test without 'C-M-O'
            call check_keys(error, "C-M-s t t z", "(1 s)(1 t)(1 t)", "z")
            if (allocated(error)) exit body
            ! 'C-M-e' and 'C-M-u' used as continue
            call check_keys(error, "C-M-s t C-M-e C-M-u t z", "(1 s)(1 t)(1 e)(1 u)(1 t)", "z")
            if (allocated(error)) exit body
            ! 'C-M-e' should not activate
            call check_keys(error, "C-M-e t z", "(1 e)", "tz")
            if (allocated(error)) exit body
            ! 'C-M-u' should also activate
            call check_keys(error, "C-M-u t z", "(1 u)(1 t)", "z")
            if (allocated(error)) exit body
            ! 'C-M-o' shared with another map should continue current map
            call check_keys(error, "C-M-s t C-M-o C-M-o t z", "(1 s)(1 t)(1 o)(1 o)(1 t)", "z")
            if (allocated(error)) exit body
            call check_keys(error, "C-M-a c C-M-o C-M-o c z", "(1 a)(1 c)(1 o)(1 o)(1 c)", "z")
        end block body

        call finish()
    end subroutine test_continue_another

    subroutine test_hints(error)
        type(error_type), allocatable, intent(out) :: error
        type(value_type), allocatable :: events(:)
        type(status_type) :: st

        call setup()
        call begin(global_map)
        repeat_check_key = .false.
        call key_parse("C-M-s", events, st)
        call execute_kbd_macro(events, nil(), st)
        call check(error, repeat_echo_message_string(repeat_in_progress), &
                   "Repeat with s, [T]est, C-M-o (an[O]ther test), C-M-u")
        call finish()
    end subroutine test_hints

    !> C-x z repeats the last command, and z repeats it again.
    subroutine test_repeat_command(error)
        type(error_type), allocatable, intent(out) :: error

        call setup()
        call begin(global_map)
        call check_keys(error, "C-M-a C-x z z z", "(1 a)(1 a)(1 a)(1 a)", "")
        if (.not. allocated(error)) call check_keys(error, "C-M-a C-u C-x z", "(1 a)(4 a)", "")
        call finish()
    end subroutine test_repeat_command
end module test_repeat_fm
