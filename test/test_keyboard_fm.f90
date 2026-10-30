! test_keyboard_fm.f90 -- tests for keyboard_fm and callint_fm.
! Own tests: upstream keyboard-tests.el and callint-tests.el need Lisp
! (read-event with timers, call-interactively on lambdas).
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

module test_keyboard_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_fm, only: status_type, set_error, FM_USER_ERROR
    use handle_fm, only: handle_type, null_handle
    use lisp_fm, only: fixnump, list, make_fixnum, make_string, nil, nilp, value_type, xfixnum
    use lread_fm, only: sym
    use data_fm, only: value_eq
    use keymap_fm, only: define_key, free_keymap, key_description, key_parse, make_keymap, &
                         make_sparse_keymap, use_global_map
    use callint_fm, only: commandp, current_prefix_arg, command_type, prefix_arg, prefix_numeric_value
    use keyboard_fm, only: command_loop, command_loop_step, discard_input, input_decode_map, &
                           input_pending_p, kbd_store_bytes, key_translation_map, last_command, &
                           last_command_event, last_message, local_function_key_map, read_event, &
                           read_key_sequence, request_exit, set_input_fd, set_input_meta_mode, &
                           this_command_keys, this_command_keys_shift_translated, &
                           unread_command_events, ring_bell_hook
    use subr_fm, only: syms_of_subr
    use sysdep_fm, only: emacs_tty_type, open_pty, close_fd, init_sys_modes, &
                         reset_sys_modes, emacs_write
    implicit none(type, external)
    private

    public :: collect_keyboard_fm

    type(command_type), target :: cmd_cmd_a
    type(command_type), target :: cmd_cmd_b
    type(command_type), target :: cmd_cmd_fail
    type(command_type), target :: cmd_cmd_exit
    type(command_type), target :: cmd_universal_argument
    type(command_type), target :: cmd_cmd_check

    integer(int64), parameter :: META = int(z'8000000', int64)

    !> Calls of the test commands, and the last prefix value seen.
    integer :: count_a = 0, count_b = 0
    integer(int64) :: seen_prefix = 0

contains

    subroutine collect_keyboard_fm(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("input-decoding", test_input_decoding), &
                    new_unittest("unread-command-events", test_unread), &
                    new_unittest("prefix-numeric-value", test_prefix_numeric_value), &
                    new_unittest("read-key-sequence", test_read_key_sequence), &
                    new_unittest("read-key-sequence/undefined-prefix", test_undefined_prefix), &
                    new_unittest("read-key-sequence/input-decode-map", test_input_decode_map), &
                    new_unittest("read-key-sequence/function-key-map", test_function_key_map), &
                    new_unittest("read-key-sequence/shift-translation", test_shift_translation), &
                    new_unittest("command-loop", test_command_loop), &
                    new_unittest("command-loop/prefix-arg-and-macro", test_prefix_and_macro), &
                    new_unittest("execute-kbd-macro/until-error", test_macro_until_error), &
                    new_unittest("adjust-point-invisible", test_adjust_invisible), &
                    new_unittest("adjust-point-display", test_adjust_display), &
                    new_unittest("command-loop/tty", test_tty) &
                    ]
    end subroutine collect_keyboard_fm

    ! Test commands.
    subroutine cmd_a(status)
        type(status_type), intent(out) :: status

        status = status_type()
        count_a = count_a + 1
        seen_prefix = prefix_numeric_value(current_prefix_arg)
    end subroutine cmd_a

    subroutine cmd_b(status)
        type(status_type), intent(out) :: status

        status = status_type()
        count_b = count_b + 1
    end subroutine cmd_b

    subroutine cmd_fail(status)
        type(status_type), intent(out) :: status

        call set_error(status, FM_USER_ERROR, "Boom")
    end subroutine cmd_fail

    !> Fail once cmd-a has run three times.
    subroutine cmd_check(status)
        type(status_type), intent(out) :: status

        status = status_type()
        if (count_a >= 3) call set_error(status, FM_USER_ERROR, "Boom")
    end subroutine cmd_check

    subroutine cmd_exit(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call request_exit()
    end subroutine cmd_exit

    !> Like simple.el:universal-argument (one C-u).
    subroutine cmd_universal(status)
        type(status_type), intent(out) :: status

        status = status_type()
        prefix_arg = list(make_fixnum(4_int64))
    end subroutine cmd_universal

    ! Helpers.
    !> Type "aqa" when the bell rings.
    subroutine type_aqa()
        ring_bell_hook => null()
        call kbd_store_bytes("aqa")
    end subroutine type_aqa

    function kbd(text) result(key)
        character(len=*), intent(in) :: text
        type(value_type), allocatable :: key(:)
        type(status_type) :: st

        call key_parse(text, key, st)
    end function kbd

    !> A fresh global map, made current.
    !> Buffer "*adjust*" holding "abcdef", shown in the selected window,
    !> point at 1, the global bindings in place.
    subroutine adjust_buffer(h)
        use bindings_fm, only: init_bindings
        use buffer_fm, only: get_buffer_create, set_buffer, erase_buffer
        use insdel_fm, only: insert
        use editfns_fm, only: goto_char
        use test_support, only: show_alone
        use window_fm, only: switch_to_buffer
        type(handle_type), intent(out) :: h
        type(status_type) :: st

        call init_bindings()
        call discard_input()
        call get_buffer_create("*adjust*", h, st)
        call switch_to_buffer("*adjust*", st)
        call set_buffer(h, st)
        call erase_buffer(st)
        call insert("abcdef", st)
        call goto_char(1_int64)
    end subroutine adjust_buffer

    !> Point after KEYS from position FROM.
    integer(int64) function point_after(from, keys) result(pos)
        use editfns_fm, only: goto_char, point
        integer(int64), intent(in) :: from
        character(len=*), intent(in) :: keys
        type(value_type), allocatable :: evs(:)
        type(status_type) :: st

        call goto_char(from)
        call key_parse(keys, evs, st)
        call unread_command_events(evs)
        call command_loop(st)
        pos = point()
    end function point_after

    !> Point leaves invisible text, as in GNU Emacs: C-f from 1 over
    !> "bcd" (2 to 5) stays at 2, then goes past it to 6; C-b from 6 to 2.
    !> With an ellipsis, it stops at the end.
    subroutine test_adjust_invisible(error)
        use textprop_fm, only: put_text_property
        use buffer_fm, only: kill_buffer
        use lisp_fm, only: t_value
        use subr_fm, only: add_to_invisibility_spec
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call adjust_buffer(h)
        call put_text_property(2_int64, 5_int64, sym("invisible"), t_value(), st)
        call check(error, point_after(1_int64, "C-f") == 2, "C-f")
        if (allocated(error)) return
        call check(error, point_after(1_int64, "C-f C-f") == 6, "C-f C-f")
        if (allocated(error)) return
        call check(error, point_after(7_int64, "C-b") == 6, "C-b")
        if (allocated(error)) return
        call check(error, point_after(7_int64, "C-b C-b") == 2, "C-b C-b")
        if (allocated(error)) return
        call add_to_invisibility_spec("foo", ellipsis=.true.)
        call put_text_property(2_int64, 5_int64, sym("invisible"), sym("foo"), st)
        call check(error, point_after(1_int64, "C-f C-f") == 5, "ellipsis C-f C-f")
        if (allocated(error)) return
        call check(error, point_after(7_int64, "C-b C-b") == 5, "ellipsis C-b C-b")
        call kill_buffer(h, st)
        call show_scratch()
    end subroutine test_adjust_invisible

    !> Point leaves text a display string replaces (3 to 5).
    subroutine test_adjust_display(error)
        use textprop_fm, only: put_text_property
        use buffer_fm, only: kill_buffer
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call adjust_buffer(h)
        call put_text_property(3_int64, 5_int64, sym("display"), make_string("XY"), st)
        call check(error, point_after(1_int64, "C-f C-f") == 3, "to its start")
        if (allocated(error)) return
        call check(error, point_after(1_int64, "C-f C-f C-f") == 5, "past it")
        if (allocated(error)) return
        call check(error, point_after(7_int64, "C-b C-b") == 5, "back to its end")
        if (allocated(error)) return
        call check(error, point_after(7_int64, "C-b C-b C-b") == 3, "back over it")
        call kill_buffer(h, st)
        call show_scratch()
    end subroutine test_adjust_display

    !> Leave *scratch* current and shown alone, for the suites that follow.
    subroutine show_scratch()
        use buffer_fm, only: get_buffer_create, set_buffer
        use test_support, only: show_alone
        type(handle_type) :: h
        type(status_type) :: st

        call get_buffer_create("*scratch*", h, st)
        call set_buffer(h, st)
        call show_alone()
    end subroutine show_scratch

    subroutine fresh_global(km)
        type(handle_type), intent(out) :: km
        type(status_type) :: st

        call discard_input()
        call make_keymap(km)
        call use_global_map(km, st)
        input_decode_map = null_handle()
        local_function_key_map = null_handle()
        key_translation_map = null_handle()
    end subroutine fresh_global

    subroutine free(km)
        type(handle_type), intent(in) :: km
        type(status_type) :: st

        call free_keymap(km, st)
    end subroutine free

    !> The next event as a fixnum (-1 if none or not a fixnum).
    integer(int64) function next_char()
        type(value_type) :: ev
        type(status_type) :: st
        logical :: have

        next_char = -1
        call read_event(ev, have, st)
        if (have .and. fixnump(ev)) next_char = xfixnum(ev)
    end function next_char

    subroutine test_input_decoding(error)
        type(error_type), allocatable, intent(out) :: error

        call discard_input()
        call set_input_meta_mode(2)
        call kbd_store_bytes("a"//char(195)//char(161)//char(255)//char(195)//"b")
        call check(error, next_char(), 97_int64)
        if (allocated(error)) return
        call check(error, next_char(), 225_int64)
        if (allocated(error)) return
        ! Invalid bytes become raw-byte characters.
        call check(error, next_char(), int(z'3FFFFF', int64))
        if (allocated(error)) return
        call check(error, next_char(), int(z'3FFFC3', int64))
        if (allocated(error)) return
        call check(error, next_char(), 98_int64)
        if (allocated(error)) return
        call check(error, next_char(), -1_int64)
        if (allocated(error)) return
        call set_input_meta_mode(1)
        call kbd_store_bytes(char(225))
        call check(error, next_char(), META + 97)
        if (allocated(error)) return
        call set_input_meta_mode(0)
        call kbd_store_bytes(char(225))
        call check(error, next_char(), 97_int64)
        call set_input_meta_mode(2)
    end subroutine test_input_decoding

    subroutine test_unread(error)
        type(error_type), allocatable, intent(out) :: error
        type(value_type) :: evs(2)

        call discard_input()
        call kbd_store_bytes("z")
        evs(1) = make_fixnum(120_int64)
        evs(2) = make_fixnum(121_int64)
        call unread_command_events(evs)
        call check(error, input_pending_p(), "pending")
        if (allocated(error)) return
        call check(error, next_char(), 120_int64)
        if (allocated(error)) return
        call check(error, next_char(), 121_int64)
        if (allocated(error)) return
        call check(error, next_char(), 122_int64)
        if (allocated(error)) return
        call check(error,.not. input_pending_p(), "drained")
    end subroutine test_unread

    subroutine test_prefix_numeric_value(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, prefix_numeric_value(nil()), 1_int64)
        if (allocated(error)) return
        call check(error, prefix_numeric_value(sym("-")), -1_int64)
        if (allocated(error)) return
        call check(error, prefix_numeric_value(list(make_fixnum(16_int64))), 16_int64)
        if (allocated(error)) return
        call check(error, prefix_numeric_value(make_fixnum(-3_int64)), -3_int64)
        if (allocated(error)) return
        call check(error, prefix_numeric_value(sym("foo")), 1_int64)
    end subroutine test_prefix_numeric_value

    subroutine test_read_key_sequence(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: global
        type(status_type) :: st
        type(value_type), allocatable :: keys(:)
        type(value_type) :: binding
        logical :: eof

        call fresh_global(global)
        call define_key(global, kbd("C-x C-f"), sym("find_file"), st)
        call define_key(global, kbd("M-x"), sym("execute_extended_command"), st)
        call kbd_store_bytes(char(24)//char(6)//char(27)//"x")
        call read_key_sequence(keys, binding, eof, st)
        call check(error, st%ok() .and. .not. eof .and. size(keys) == 2, "C-x C-f keys")
        if (allocated(error)) return
        call check(error, value_eq(binding, sym("find_file")), "C-x C-f")
        if (allocated(error)) return
        call read_key_sequence(keys, binding, eof, st)
        call check(error, key_description(keys), "M-x")
        if (allocated(error)) return
        call check(error, value_eq(binding, sym("execute_extended_command")), "ESC x")
        if (allocated(error)) return
        ! Input ending in the middle of a prefix.
        call kbd_store_bytes(char(24))
        call read_key_sequence(keys, binding, eof, st)
        call check(error, eof .and. size(keys) == 1 .and. nilp(binding), "eof")
        if (allocated(error)) return
        call read_key_sequence(keys, binding, eof, st)
        call check(error, eof .and. size(keys) == 0, "empty eof")
        call free(global)
    end subroutine test_read_key_sequence

    !> An unbound prefix ends the sequence; the rest is a new sequence.
    subroutine test_undefined_prefix(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: global
        type(status_type) :: st
        type(value_type), allocatable :: keys(:)
        type(value_type) :: binding
        logical :: eof

        call fresh_global(global)
        call kbd_store_bytes(char(3)//"z")
        call read_key_sequence(keys, binding, eof, st)
        call check(error, size(keys) == 1 .and. nilp(binding), "C-c")
        if (allocated(error)) return
        call check(error, next_char(), 122_int64)
        call free(global)
    end subroutine test_undefined_prefix

    !> Terminal escape sequences become function keys, while other
    !> sequences sharing the ESC prefix still work.
    subroutine test_input_decode_map(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: global, decode
        type(status_type) :: st
        type(value_type), allocatable :: keys(:)
        type(value_type) :: binding, up(1)
        logical :: eof

        call fresh_global(global)
        up(1) = sym("up")
        call define_key(global, up, sym("previous_line"), st)
        call define_key(global, kbd("M-x"), sym("execute_extended_command"), st)
        call make_sparse_keymap(decode)
        call define_key(decode, kbd("ESC [ A"), list(sym("up")), st)
        input_decode_map = decode
        call kbd_store_bytes(char(27)//"[A"//char(27)//"x")
        call read_key_sequence(keys, binding, eof, st)
        call check(error, st%ok() .and. size(keys) == 1, "one key")
        if (allocated(error)) return
        call check(error, value_eq(keys(1), sym("up")) .and. &
                   value_eq(binding, sym("previous_line")), "up")
        if (allocated(error)) return
        call read_key_sequence(keys, binding, eof, st)
        call check(error, value_eq(binding, sym("execute_extended_command")), "M-x")
        call free(decode)
        call free(global)
    end subroutine test_input_decode_map

    !> local-function-key-map applies only when the sequence is undefined.
    subroutine test_function_key_map(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: global, fkeys
        type(status_type) :: st
        type(value_type), allocatable :: keys(:)
        type(value_type) :: binding
        logical :: eof

        call fresh_global(global)
        call define_key(global, kbd("<f1>"), sym("help-command"), st)
        call make_sparse_keymap(fkeys)
        call define_key(fkeys, kbd("ESC O P"), list(sym("f1")), st)
        local_function_key_map = fkeys
        call kbd_store_bytes(char(27)//"OP")
        call read_key_sequence(keys, binding, eof, st)
        call check(error, size(keys) == 1 .and. value_eq(binding, sym("help-command")), "translated")
        if (allocated(error)) return
        call define_key(global, kbd("M-O P"), sym("foo"), st)
        call kbd_store_bytes(char(27)//"OP")
        call read_key_sequence(keys, binding, eof, st)
        call check(error, size(keys) == 3 .and. value_eq(binding, sym("foo")), "defined wins")
        call free(fkeys)
        call free(global)
    end subroutine test_function_key_map

    subroutine test_shift_translation(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: global
        type(status_type) :: st
        type(value_type), allocatable :: keys(:)
        type(value_type) :: binding, key(1)
        logical :: eof

        call fresh_global(global)
        call define_key(global, kbd("a"), sym("cmd-a"), st)
        call define_key(global, kbd("<f2>"), sym("cmd-f2"), st)
        call kbd_store_bytes("AB")
        call read_key_sequence(keys, binding, eof, st)
        call check(error, value_eq(binding, sym("cmd-a")) .and. xfixnum(keys(1)) == 97 &
                   .and. this_command_keys_shift_translated, "A -> a")
        if (allocated(error)) return
        call read_key_sequence(keys, binding, eof, st)
        call check(error, nilp(binding) .and. xfixnum(keys(1)) == 66 .and. &
                   .not. this_command_keys_shift_translated, "B stays")
        if (allocated(error)) return
        key(1) = sym("S-f2")
        call unread_command_events(key)
        call read_key_sequence(keys, binding, eof, st)
        call check(error, value_eq(binding, sym("cmd-f2")) .and. value_eq(keys(1), sym("f2")), "S-<f2>")
        call free(global)
    end subroutine test_shift_translation

    subroutine test_command_loop(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: global
        type(status_type) :: st
        logical :: done

        call fresh_global(global)
        cmd_cmd_a = command_type("cmd-a", cmd_a)
        call cmd_cmd_a%define()
        cmd_cmd_b = command_type("cmd-b", cmd_b)
        call cmd_cmd_b%define()
        cmd_cmd_fail = command_type("cmd-fail", cmd_fail)
        call cmd_cmd_fail%define()
        cmd_cmd_exit = command_type("cmd-exit", cmd_exit)
        call cmd_cmd_exit%define()
        call define_key(global, kbd("a"), sym("cmd-a"), st)
        call define_key(global, kbd("C-c b"), sym("cmd-b"), st)
        call define_key(global, kbd("f"), sym("cmd-fail"), st)
        call define_key(global, kbd("q"), sym("cmd-exit"), st)
        call define_key(global, kbd("v"), sym("void-command"), st)
        call check(error, commandp(sym("cmd-a")) .and. .not. commandp(sym("void-command")), "commandp")
        if (allocated(error)) return
        count_a = 0
        count_b = 0
        call kbd_store_bytes("aa"//char(3)//"b")
        call command_loop(st)
        call check(error, count_a == 2 .and. count_b == 1, "counts")
        if (allocated(error)) return
        call check(error, value_eq(last_command, sym("cmd-b")), "last-command")
        if (allocated(error)) return
        call check(error, key_description(this_command_keys), "C-c b")
        if (allocated(error)) return
        call check(error, xfixnum(last_command_event), 98_int64)
        if (allocated(error)) return
        call syms_of_subr()
        call kbd_store_bytes("z")
        call command_loop_step(done, st)
        call check(error, last_message, "z is undefined")
        if (allocated(error)) return
        call kbd_store_bytes("f")
        call command_loop_step(done, st)
        call check(error, st%code == FM_USER_ERROR .and. last_message == "Boom", "error")
        if (allocated(error)) return
        call kbd_store_bytes("v")
        call command_loop_step(done, st)
        call check(error, last_message, "Wrong type argument: commandp, void-command")
        if (allocated(error)) return
        ! Errors don't stop the loop; cmd-exit does.  An error discards
        ! the input typed ahead and rings the bell.
        call kbd_store_bytes("fa")
        ring_bell_hook => type_aqa
        call command_loop(st)
        call check(error, count_a == 3 .and. .not. associated(ring_bell_hook), "exit")
        if (allocated(error)) return
        call check(error, input_pending_p(), "input left")
        call discard_input()
        call free(global)
    end subroutine test_command_loop

    subroutine test_prefix_and_macro(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: global
        type(status_type) :: st

        call fresh_global(global)
        cmd_cmd_a = command_type("cmd-a", cmd_a)
        call cmd_cmd_a%define()
        cmd_cmd_b = command_type("cmd-b", cmd_b)
        call cmd_cmd_b%define()
        cmd_universal_argument = command_type("test-universal-argument", cmd_universal)
        call cmd_universal_argument%define()
        call define_key(global, kbd("a"), sym("cmd-a"), st)
        call define_key(global, kbd("b"), sym("cmd-b"), st)
        call define_key(global, kbd("C-u"), sym("test-universal-argument"), st)
        call define_key(global, kbd("C-c m"), make_string("ab"), st)
        count_a = 0
        count_b = 0
        call kbd_store_bytes(char(21)//"a")
        call command_loop(st)
        call check(error, count_a == 1 .and. seen_prefix == 4, "C-u a")
        if (allocated(error)) return
        call kbd_store_bytes("a")
        call command_loop(st)
        call check(error, seen_prefix, 1_int64)
        if (allocated(error)) return
        ! A keyboard macro, run 4 times by C-u.
        count_a = 0
        call kbd_store_bytes(char(21)//char(3)//"m")
        call command_loop(st)
        call check(error, count_a == 4 .and. count_b == 4, "macro")
        call free(global)
    end subroutine test_prefix_and_macro

    !> macros.c:Fexecute_kbd_macro: a COUNT of 0 repeats the macro until
    !> an error, which the command loop reports after the iterations
    !> done; ringing the bell ends a macro.
    subroutine test_macro_until_error(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: global
        type(status_type) :: st

        call fresh_global(global)
        cmd_cmd_a = command_type("cmd-a", cmd_a)
        call cmd_cmd_a%define()
        cmd_cmd_check = command_type("cmd-check", cmd_check)
        call cmd_cmd_check%define()
        cmd_universal_argument = command_type("test-universal-argument", cmd_universal)
        call cmd_universal_argument%define()
        call syms_of_subr()
        call define_key(global, kbd("a"), sym("cmd-a"), st)
        call define_key(global, kbd("g"), sym("cmd-check"), st)
        call define_key(global, kbd("C-c m"), make_string("ag"), st)
        call define_key(global, kbd("C-c u"), make_string("za"), st)
        call define_key(global, kbd("C-c 0"), list(make_fixnum(3_int64), make_fixnum(109_int64)), st)
        count_a = 0
        ! C-c 0 is the macro C-c m with a count of 0 from the prefix.
        prefix_arg = make_fixnum(0_int64)
        call kbd_store_bytes(char(3)//"m")
        call command_loop(st)
        call check(error, count_a, 3)
        if (allocated(error)) return
        call check(error, last_message, "After 2 kbd macro iterations: Boom")
        if (allocated(error)) return
        ! An undefined key in a macro: the bell ends it.
        count_a = 0
        call kbd_store_bytes(char(3)//"u")
        call command_loop(st)
        call check(error, count_a == 0 .and. last_message == "After 0 kbd macro iterations: "// &
                   "Keyboard macro terminated by a command ringing the bell", last_message)
        if (allocated(error)) return
        ! A list of events as a macro.
        count_a = 0
        call kbd_store_bytes(char(3)//"0")
        call command_loop(st)
        call check(error, count_a == 1 .and. last_message == "", last_message)
        call free(global)
    end subroutine test_macro_until_error

    !> Input read from a terminal (a pty in raw mode).
    subroutine test_tty(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: global
        type(emacs_tty_type) :: old
        type(status_type) :: st
        type(value_type), allocatable :: keys(:)
        type(value_type) :: binding
        integer(int32) :: master, slave
        logical :: eof

        call fresh_global(global)
        call define_key(global, kbd("C-x C-s"), sym("save_buffer"), st)
        call open_pty(master, slave, st)
        call check(error, st%ok(), "open_pty")
        if (allocated(error)) return

        run: block
            call init_sys_modes(slave, old, st)
            call check(error, st%ok(), "raw mode")
            if (allocated(error)) exit run
            call set_input_fd(slave)
            call emacs_write(master, char(24)//char(19), st)
            call read_key_sequence(keys, binding, eof, st)
            call check(error, st%ok() .and. value_eq(binding, sym("save_buffer")), "C-x C-s")
            call reset_sys_modes(slave, old, st)
        end block run

        call set_input_fd(-1_int32)
        call close_fd(slave)
        call close_fd(master)
        call free(global)
    end subroutine test_tty
end module test_keyboard_fm
