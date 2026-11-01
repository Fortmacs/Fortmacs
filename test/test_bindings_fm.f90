! test_bindings_fm.f90 -- tests for bindings_fm, simple_fm and the
! command side of cmds_fm: typing into a buffer through the command
! loop.
! Own tests; simple-tests.el's newline and open-line cases need margins,
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
    use callint_fm, only: prefix_arg_type
    use commands_fm, only: commands
    use test_support, only: show_alone
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_fm, only: status_type, FM_QUIT
    use handle_fm, only: handle_type
    use buffer_fm, only: get_buffer_create, set_buffer, kill_buffer
    use editfns_fm, only: point, buffer_string, goto_char
    use lisp_fm, only: value_type, xstring, list_elt
    use simple_fm, only: kill_ring
    use data_fm, only: value_eq
    use keymap_fm, only: key_binding, key_parse
    use keyboard_fm, only: kbd_store_bytes, command_loop, command_loop_step, &
                           discard_input, last_message, last_command, ring_bell_hook
    use callint_fm, only: prefix_arg, is_command_defined
    use bindings_fm, only: init_bindings
    implicit none(type, external)
    private

    public :: collect_bindings_fm

    !> Times the bell rang.
    integer :: bells = 0

contains

    !> Collect all exported unit tests.
    subroutine collect_bindings_fm(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("global-map-bindings", test_global_map), &
                    new_unittest("self-insert-and-motion", test_typing), &
                    new_unittest("newline-and-open-line", test_newline), &
                    new_unittest("universal-argument", test_universal_argument), &
                    new_unittest("digit-and-negative-argument", test_digit_argument), &
                    new_unittest("quoted-insert-and-quit", test_quoted_insert), &
                    new_unittest("function-key-bindings", test_function_keys), &
                    new_unittest("move-beginning-and-end-of-line", test_move_line), &
                    new_unittest("undefined-rings-the-bell", test_undefined), &
                    new_unittest("no-lisp-evaluation-keys", test_no_eval_keys) &
                    ]
    end subroutine collect_bindings_fm

    !> A fresh buffer with the default bindings, holding `keys` typed.
    subroutine type_into(name, bytes, buf)
        character(len=*), intent(in) :: name, bytes
        type(handle_type), intent(out) :: buf
        type(status_type) :: st

        call discard_input()
        prefix_arg = prefix_arg_type()
        call init_bindings()
        call get_buffer_create(name, buf, st)
        call set_buffer(buf, st)
        ! The command loop works in the selected window's buffer.
        call show_alone()
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
        call check(error, value_eq(binding_of("a"), value_type(name="self_insert_command")), "a")
        if (allocated(error)) return
        call check(error, value_eq(binding_of("TAB"), value_type(name="indent_for_tab_command")), "TAB")
        if (allocated(error)) return
        ! mule-conf.el: every non-ASCII character self-inserts.
        call check(error, value_eq(binding_of("é"), value_type(name="self_insert_command")), "é")
        if (allocated(error)) return
        call check(error, value_eq(binding_of("C-f"), value_type(name="forward_char")), "C-f")
        if (allocated(error)) return
        call check(error, value_eq(binding_of("M-5"), value_type(name="digit_argument")), "M-5")
        if (allocated(error)) return
        call check(error, value_eq(binding_of("C-M-5"), value_type(name="digit_argument")), "C-M-5")
        if (allocated(error)) return
        call check(error, value_eq(binding_of("<right>"), value_type(name="right_char")), "<right>")
        if (allocated(error)) return
        call check(error, value_eq(binding_of("DEL"), value_type(name="delete_backward_char")), "DEL")
        if (allocated(error)) return
        ! A modified character is not covered by the self-insert range.
        call check(error, value_eq(binding_of("C-é"), value_type()), "C-é")
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
        call check(error, associated(last_command, commands%self_insert_command), "last-command")
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
        call check(error, associated(last_command, commands%self_insert_command), "last-command")
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

    !> Count a ring of the bell.
    subroutine count_bell()
        bells = bells + 1
    end subroutine count_bell

    !> bindings.el's function keys and help.el's <help>.
    subroutine test_function_keys(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=16), parameter :: keys(19) = [character(len=16) :: "C-a", "C-e", "<home>", "<end>", &
                                                                          "S-<insert>", "S-<insertchar>", "<paste>", "C-<insert>", &
                                                                                "C-<insertchar>", "<copy>", "S-<delete>", "<cut>", &
                                                                              "<insertline>", "<deleteline>", "<undo>", "<begin>", &
                                                                                                    "<find>", "<execute>", "<menu>"]
        character(len=24), parameter :: cmds(19) = [character(len=24) :: "move_beginning_of_line", &
                                                                                     "move_end_of_line", "move_beginning_of_line", &
                                                                     "move_end_of_line", "yank", "yank", "yank", "kill_ring_save", &
                                                                                "kill_ring_save", "kill_ring_save", "kill_region", &
                                                                                  "kill_region", "open_line", "kill_line", "undo", &
                                                                                          "beginning_of_buffer", "search_forward", &
                                                                             "execute_extended_command", "execute_extended_command"]
        integer :: k

        call init_bindings()

        do k = 1, size(keys)
            call check(error, value_eq(binding_of(trim(keys(k))), value_type(name=trim(cmds(k)))), trim(keys(k)))
            if (allocated(error)) return
        end do

        call check(error, value_eq(binding_of("<help> k"), value_type(name="describe_key")), "<help> k")
    end subroutine test_function_keys

    !> C-a and C-e with a count; simple.el move-beginning-of-line and
    !> move-end-of-line.
    subroutine test_move_line(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: buf
        type(status_type) :: st

        call type_into("*bindings-lines*", "abc"//char(13)//"def"//char(13)//"ghi", buf)
        call goto_char(2_int64)
        call kbd_store_bytes(char(5))
        call command_loop(st)
        call check(error, point(), 4_int64)
        if (allocated(error)) return
        ! C-u 2 C-e: the end of the next line.
        call kbd_store_bytes(char(21)//"2"//char(5))
        call command_loop(st)
        call check(error, point(), 8_int64)
        if (allocated(error)) return
        ! Past the last line: its end.
        call kbd_store_bytes(char(21)//"5"//char(5))
        call command_loop(st)
        call check(error, point(), 12_int64)
        if (allocated(error)) return
        call kbd_store_bytes(char(1))
        call command_loop(st)
        call check(error, point(), 9_int64)
        if (allocated(error)) return
        ! C-u -1 C-a: the previous line's beginning.
        call kbd_store_bytes(char(21)//"-1"//char(1))
        call command_loop(st)
        call check(error, point(), 1_int64)
        call done_with(buf)
    end subroutine test_move_line

    !> An undefined key rings the bell and says so (subr.el:undefined); so
    !> does a command error, which also discards the input typed ahead
    !> (keyboard.c:command-error-default-function).
    subroutine test_undefined(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: buf
        type(status_type) :: st
        logical :: done

        call type_into("*bindings-undefined*", "", buf)
        bells = 0
        ring_bell_hook => count_bell
        call kbd_store_bytes(char(24)//char(8)//"a")
        call command_loop_step(done, st)
        call check(error, last_message == "C-x C-h is undefined" .and. bells == 1, last_message)
        if (allocated(error)) return
        call command_loop(st)
        call check(error, buffer_string(), "a")
        if (allocated(error)) return
        ! C-f at the end of the buffer fails: "b" is discarded.
        call kbd_store_bytes(char(6)//"b")
        call command_loop(st)
        ring_bell_hook => null()
        call check(error, last_message == "End of buffer" .and. bells == 2 .and. buffer_string() == "a", last_message)
        call done_with(buf)
    end subroutine test_undefined

    !> The keys of upstream's run-time Lisp evaluation commands are
    !> unbound (not applicable: the configuration is Fortran compiled in),
    !> and no such command exists.
    subroutine test_no_eval_keys(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=10), parameter :: keys(4) = [character(len=10) :: "M-:", "C-x C-e", "C-M-x", "ESC :"]
        character(len=20), parameter :: names(6) = [character(len=20) :: "eval_expression", "eval_last_sexp", &
                                                                                 "eval_buffer", "eval_region", "eval_defun", "ielm"]
        integer :: k

        call init_bindings()

        do k = 1, size(keys)
            call check(error, value_eq(binding_of(trim(keys(k))), value_type()), trim(keys(k))//" is bound")
            if (allocated(error)) return
        end do

        do k = 1, size(names)
            call check(error,.not. is_command_defined(value_type(name=trim(names(k)))), trim(names(k))//" is a command")
            if (allocated(error)) return
        end do
    end subroutine test_no_eval_keys
end module test_bindings_fm
