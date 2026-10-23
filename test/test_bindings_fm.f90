! test_bindings_fm.f90 -- tests for bindings_fm, simple_fm and the
! command side of cmds_fm: typing into a buffer through the command loop.
! Own tests: simple-tests.el's newline/open-line cases need margins,
! fill prefixes and undo.
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

module test_bindings_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_fm, only: status_type, FM_QUIT
    use handle_fm, only: handle_type
    use buffer_fm, only: get_buffer_create, set_buffer, kill_buffer
    use editfns_fm, only: point, buffer_string
    use lisp_fm, only: value_type, nil, xstring, list_elt
    use simple_fm, only: kill_ring
    use lread_fm, only: sym
    use data_fm, only: value_eq
    use keymap_fm, only: key_binding, key_parse
    use keyboard_fm, only: kbd_store_bytes, command_loop, command_loop_step, &
                           discard_input, last_message, last_command
    use callint_fm, only: prefix_arg
    use bindings_fm, only: init_bindings
    implicit none(type, external)
    private

    public :: collect_bindings_fm

contains

    subroutine collect_bindings_fm(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)
        testsuite = [ &
                    new_unittest("global-map-bindings", test_global_map), &
                    new_unittest("self-insert-and-motion", test_typing), &
                    new_unittest("newline-and-open-line", test_newline), &
                    new_unittest("universal-argument", test_universal_argument), &
                    new_unittest("digit-and-negative-argument", test_digit_argument), &
                    new_unittest("quoted-insert-and-quit", test_quoted_insert) &
                    ]
    end subroutine collect_bindings_fm

    !> A fresh buffer with the default bindings, holding `keys` typed.
    subroutine type_into(name, bytes, buf)
        character(len=*), intent(in) :: name, bytes
        type(handle_type), intent(out) :: buf
        type(status_type) :: st
        call discard_input()
        prefix_arg = nil()
        call init_bindings()
        call get_buffer_create(name, buf, st)
        call set_buffer(buf, st)
        call kbd_store_bytes(bytes)
        call command_loop(st)
    end subroutine type_into

    subroutine done_with(buf)
        type(handle_type), intent(in) :: buf
        type(status_type) :: st
        call kill_buffer(buf, st)
    end subroutine done_with

    function binding_of(keys) result(v)
        character(len=*), intent(in) :: keys
        type(value_type) :: v
        type(value_type), allocatable :: key(:)
        type(status_type) :: st
        call key_parse(keys, key, st)
        call key_binding(key, v, st)
    end function binding_of

    subroutine test_global_map(error)
        type(error_type), allocatable, intent(out) :: error
        call init_bindings()
        call check(error, value_eq(binding_of("a"), sym("self_insert_command")), "a")
        if (allocated(error)) return
        call check(error, value_eq(binding_of("TAB"), sym("indent_for_tab_command")), "TAB")
        if (allocated(error)) return
        ! mule-conf.el: every non-ASCII character self-inserts.
        call check(error, value_eq(binding_of("é"), sym("self_insert_command")), "é")
        if (allocated(error)) return
        call check(error, value_eq(binding_of("C-f"), sym("forward_char")), "C-f")
        if (allocated(error)) return
        call check(error, value_eq(binding_of("M-5"), sym("digit_argument")), "M-5")
        if (allocated(error)) return
        call check(error, value_eq(binding_of("C-M-5"), sym("digit_argument")), "C-M-5")
        if (allocated(error)) return
        call check(error, value_eq(binding_of("<right>"), sym("right_char")), "<right>")
        if (allocated(error)) return
        call check(error, value_eq(binding_of("DEL"), sym("delete_backward_char")), "DEL")
        if (allocated(error)) return
        ! A modified character is not covered by the self-insert range.
        call check(error, value_eq(binding_of("C-é"), nil()), "C-é")
    end subroutine test_global_map

    subroutine test_typing(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: buf
        ! h é l l o, C-b C-b, DEL, C-a, C-d, C-e, x
        call type_into("*bindings-typing*", "h"//char(195)//char(169)//"llo"// &
                       char(2)//char(2)//char(127)//char(1)//char(4)//char(5)//"x", buf)
        call check(error, buffer_string(), char(195)//char(169)//"lox")
        if (allocated(error)) return
        call check(error, point(), 5_int64)
        if (allocated(error)) return
        call check(error, value_eq(last_command, sym("self_insert_command")), "last-command")
        call done_with(buf)
    end subroutine test_typing

    subroutine test_newline(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: buf
        ! ab RET c C-a C-o
        call type_into("*bindings-newline*", "ab"//char(13)//"c"//char(1)//char(15), buf)
        call check(error, buffer_string(), "ab"//achar(10)//achar(10)//"c")
        if (allocated(error)) return
        call check(error, point(), 4_int64)
        call done_with(buf)
    end subroutine test_newline

    subroutine test_universal_argument(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: buf
        ! C-u x, C-u C-u y, C-u 3 z, C-u 1 2 C-u 0 (C-u ends the digits)
        call type_into("*bindings-uarg*", char(21)//"x"//char(21)//char(21)//"y"// &
                       char(21)//"3z"//char(21)//"12"//char(21)//"0", buf)
        call check(error, buffer_string(), repeat("x", 4)//repeat("y", 16)//"zzz"// &
                   repeat("0", 12))
        if (allocated(error)) return
        ! Prefix commands keep last-command.
        call check(error, value_eq(last_command, sym("self_insert_command")), "last-command")
        call done_with(buf)
    end subroutine test_universal_argument

    subroutine test_digit_argument(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: buf
        type(status_type) :: st
        ! M-3 a, M-1 M-0 c (ESC as Meta).
        call type_into("*bindings-digits*", char(27)//"3a"//char(27)//"1"//char(27)//"0c", buf)
        call check(error, buffer_string(), "aaa"//repeat("c", 10))
        if (allocated(error)) return
        ! C-e, then M-- 2 C-d is delete-char -2 with KILLFLAG: it kills the
        ! two characters before point.
        call kbd_store_bytes(char(5)//char(27)//"-2"//char(4))
        call command_loop(st)
        call check(error, buffer_string(), "aaa"//repeat("c", 8))
        if (allocated(error)) return
        call check(error, xstring(list_elt(kill_ring(), 1_int64)), "cc")
        if (allocated(error)) return
        ! C-a, C-u - 2 DEL: delete-backward-char -2 kills forward.
        call kbd_store_bytes(char(1)//char(21)//"-2"//char(127))
        call command_loop(st)
        call check(error, buffer_string(), "a"//repeat("c", 8))
        if (allocated(error)) return
        call check(error, xstring(list_elt(kill_ring(), 1_int64)), "aa")
        if (allocated(error)) return
        ! Without a prefix argument DEL deletes: at the beginning it
        ! signals.
        call kbd_store_bytes(char(1)//char(127))
        call command_loop(st)
        call check(error, last_message, "Beginning of buffer")
        call done_with(buf)
    end subroutine test_digit_argument

    subroutine test_quoted_insert(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: buf
        type(status_type) :: st
        logical :: done
        ! C-q C-a, C-u 2 C-q DEL
        call type_into("*bindings-quoted*", char(17)//char(1)//char(21)//"2"//char(17)//char(127), buf)
        call check(error, buffer_string(), char(1)//char(127)//char(127))
        if (allocated(error)) return
        call kbd_store_bytes(char(7))
        call command_loop_step(done, st)
        call check(error, st%code == FM_QUIT .and. last_message == "Quit", "C-g")
        call done_with(buf)
    end subroutine test_quoted_insert
end module test_bindings_fm
