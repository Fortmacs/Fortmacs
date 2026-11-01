! test_keyboard_fm.f90 -- tests for keyboard_fm and callint_fm.
! Own tests; upstream's keyboard-tests.el and callint-tests.el need
! Lisp.
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
    use callint_fm, only: prefix_arg_type, PREFIX_NUMBER, PREFIX_RAW, PREFIX_MINUS
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_fm, only: status_type, set_error, format_error, FM_USER_ERROR, FM_INHIBITED_INTERACTION, FM_TOP_LEVEL
    use handle_fm, only: handle_type, null_handle
    use lisp_fm, only: list, value_type, xfixnum, vtag, V_INTEGER
    use data_fm, only: value_eq
    use keymap_fm, only: define_key, destroy_keymap, key_description, key_parse, make_keymap, &
                         make_sparse_keymap, use_global_map, overriding_terminal_local_map, key_binding
    use callint_fm, only: is_command, current_prefix_arg, command_type, prefix_arg, prefix_numeric_value
    use keyboard_fm, only: command_loop, command_loop_step, discard_input, input_decode_map, &
                           is_input_pending, kbd_store_bytes, key_translation_map, last_command, &
                           last_command_event, last_message, local_function_key_map, read_event, &
                           read_key_sequence, request_exit, set_input_fd, set_input_meta_mode, &
                           this_command_keys, this_command_keys_shift_translated, &
                           unread_command_events, ring_bell_hook, read_key_sequence_vs, clear_this_command_keys, &
                           read_char, read_char_exclusive, lossage_size, inhibit_interaction, &
                           translate_upper_case_key_bindings, this_command_key_events, this_single_command_keys, &
                           this_single_command_raw_keys, set_this_command_keys, echo_keystrokes, echo_keystrokes_help, &
                           cancel_echoing, event_convert_list, event_name_parse_modifiers, make_ctrl_char, &
                           recursion_depth, top_level, recursive_edit, post_command_hook, safe_execute_hooks, echo_now
    use fns_fm, only: value_equal
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
    type(command_type), target :: cmd_cmd_keys
    type(command_type), target :: cmd_zero_argument
    type(command_type), target :: cmd_cmd_depth
    type(command_type), target :: cmd_cmd_top

    integer(int64), parameter :: META = int(z'8000000', int64), CTL = int(z'4000000', int64), &
                                 SHIFT = int(z'2000000', int64)

    !> Calls of the test commands, and the last prefix value seen.
    integer :: count_a = 0, count_b = 0
    integer(int64) :: seen_prefix = 0
    !> What cmd-keys saw of this-command-keys, and cmd-depth of
    !> recursion-depth.
    character(len=:), allocatable :: seen_keys, seen_single_keys, seen_raw_keys
    integer :: seen_depth = -1

contains

    !> Collect all exported unit tests.
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
                    new_unittest("command-loop/tty", test_tty), &
                    new_unittest("read-key-sequence/keyboard-tests", test_rks), &
                    new_unittest("keyboard-lossage-size", test_lossage_size), &
                    new_unittest("keyboard-inhibit-interaction", test_inhibit_interaction), &
                    new_unittest("read-char", test_read_char), &
                    new_unittest("this-command-keys", test_this_command_keys), &
                    new_unittest("echo-keystrokes", test_echo), &
                    new_unittest("event-symbols", test_event_symbols), &
                    new_unittest("recursive-edit/top-level", test_recursive_edit) &
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

    !> Record this-command-keys, this-single-command-keys and
    !> this-single-command-raw-keys.
    subroutine cmd_keys(status)
        type(status_type), intent(out) :: status

        status = status_type()
        seen_keys = key_description(this_command_key_events())
        seen_single_keys = key_description(this_single_command_keys())
        seen_raw_keys = key_description(this_single_command_raw_keys())
    end subroutine cmd_keys

    !> A prefix argument of 0 for the next command.
    subroutine cmd_zero(status)
        type(status_type), intent(out) :: status

        status = status_type()
        prefix_arg = prefix_arg_type(PREFIX_NUMBER, 0_int64)
    end subroutine cmd_zero

    !> Record recursion-depth.
    subroutine cmd_depth(status)
        type(status_type), intent(out) :: status

        status = status_type()
        seen_depth = recursion_depth()
    end subroutine cmd_depth

    !> top-level.
    subroutine cmd_top(status)
        type(status_type), intent(out) :: status

        call top_level(status)
    end subroutine cmd_top

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
        prefix_arg = prefix_arg_type(PREFIX_RAW, 4_int64)
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
        use buffer_fm, only: get_or_make_buffer, set_buffer, erase_buffer
        use insdel_fm, only: insert
        use editfns_fm, only: goto_char
        use test_support, only: show_alone
        use window_fm, only: switch_to_buffer
        type(handle_type), intent(out) :: h
        type(status_type) :: st

        call init_bindings()
        call discard_input()
        call get_or_make_buffer("*adjust*", h, st)
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
        use buffer_fm, only: close_buffer
        use lisp_fm, only: value_type
        use subr_fm, only: add_to_invisibility_spec
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call adjust_buffer(h)
        call put_text_property(2_int64, 5_int64, value_type(name="invisible"), value_type(.true.), st)
        call check(error, point_after(1_int64, "C-f") == 2, "C-f")
        if (allocated(error)) return
        call check(error, point_after(1_int64, "C-f C-f") == 6, "C-f C-f")
        if (allocated(error)) return
        call check(error, point_after(7_int64, "C-b") == 6, "C-b")
        if (allocated(error)) return
        call check(error, point_after(7_int64, "C-b C-b") == 2, "C-b C-b")
        if (allocated(error)) return
        call add_to_invisibility_spec("foo", ellipsis=.true.)
        call put_text_property(2_int64, 5_int64, value_type(name="invisible"), value_type(name="foo"), st)
        call check(error, point_after(1_int64, "C-f C-f") == 5, "ellipsis C-f C-f")
        if (allocated(error)) return
        call check(error, point_after(7_int64, "C-b C-b") == 5, "ellipsis C-b C-b")
        call close_buffer(h, st)
        call show_scratch()
    end subroutine test_adjust_invisible

    !> Point leaves text a display string replaces (3 to 5).
    subroutine test_adjust_display(error)
        use textprop_fm, only: put_text_property
        use buffer_fm, only: close_buffer
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call adjust_buffer(h)
        call put_text_property(3_int64, 5_int64, value_type(name="display"), value_type(text="XY"), st)
        call check(error, point_after(1_int64, "C-f C-f") == 3, "to its start")
        if (allocated(error)) return
        call check(error, point_after(1_int64, "C-f C-f C-f") == 5, "past it")
        if (allocated(error)) return
        call check(error, point_after(7_int64, "C-b C-b") == 5, "back to its end")
        if (allocated(error)) return
        call check(error, point_after(7_int64, "C-b C-b C-b") == 3, "back over it")
        call close_buffer(h, st)
        call show_scratch()
    end subroutine test_adjust_display

    !> Leave *scratch* current and shown alone, for the suites that follow.
    subroutine show_scratch()
        use buffer_fm, only: get_or_make_buffer, set_buffer
        use test_support, only: show_alone
        type(handle_type) :: h
        type(status_type) :: st

        call get_or_make_buffer("*scratch*", h, st)
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

        call destroy_keymap(km, st)
    end subroutine free

    !> The next event as a fixnum (-1 if none or not a fixnum).
    integer(int64) function next_char()
        type(value_type) :: ev
        type(status_type) :: st
        logical :: have

        next_char = -1
        call read_event(ev, have, st)
        if (have .and. vtag(ev) == V_INTEGER) next_char = xfixnum(ev)
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
        evs(1) = value_type(120_int64)
        evs(2) = value_type(121_int64)
        call unread_command_events(evs)
        call check(error, is_input_pending(), "pending")
        if (allocated(error)) return
        call check(error, next_char(), 120_int64)
        if (allocated(error)) return
        call check(error, next_char(), 121_int64)
        if (allocated(error)) return
        call check(error, next_char(), 122_int64)
        if (allocated(error)) return
        call check(error,.not. is_input_pending(), "drained")
    end subroutine test_unread

    subroutine test_prefix_numeric_value(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, prefix_numeric_value(prefix_arg_type()), 1_int64)
        if (allocated(error)) return
        call check(error, prefix_numeric_value(prefix_arg_type(PREFIX_MINUS)), -1_int64)
        if (allocated(error)) return
        call check(error, prefix_numeric_value(prefix_arg_type(PREFIX_RAW, 16_int64)), 16_int64)
        if (allocated(error)) return
        call check(error, prefix_numeric_value(prefix_arg_type(PREFIX_NUMBER, -3_int64)), -3_int64)
    end subroutine test_prefix_numeric_value

    subroutine test_read_key_sequence(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: global
        type(status_type) :: st
        type(value_type), allocatable :: keys(:)
        type(value_type) :: binding
        logical :: eof

        call fresh_global(global)
        call define_key(global, kbd("C-x C-f"), value_type(name="find_file"), st)
        call define_key(global, kbd("M-x"), value_type(name="execute_extended_command"), st)
        call kbd_store_bytes(char(24)//char(6)//char(27)//"x")
        call read_key_sequence(keys, binding, eof, st)
        call check(error, st%ok() .and. .not. eof .and. size(keys) == 2, "C-x C-f keys")
        if (allocated(error)) return
        call check(error, value_eq(binding, value_type(name="find_file")), "C-x C-f")
        if (allocated(error)) return
        call read_key_sequence(keys, binding, eof, st)
        call check(error, key_description(keys), "M-x")
        if (allocated(error)) return
        call check(error, value_eq(binding, value_type(name="execute_extended_command")), "ESC x")
        if (allocated(error)) return
        ! Input ending in the middle of a prefix.
        call kbd_store_bytes(char(24))
        call read_key_sequence(keys, binding, eof, st)
        call check(error, eof .and. size(keys) == 1 .and. .not. allocated(binding%a), "eof")
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
        call check(error, size(keys) == 1 .and. .not. allocated(binding%a), "C-c")
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
        up(1) = value_type(name="up")
        call define_key(global, up, value_type(name="previous_line"), st)
        call define_key(global, kbd("M-x"), value_type(name="execute_extended_command"), st)
        call make_sparse_keymap(decode)
        call define_key(decode, kbd("ESC [ A"), list(value_type(name="up")), st)
        input_decode_map = decode
        call kbd_store_bytes(char(27)//"[A"//char(27)//"x")
        call read_key_sequence(keys, binding, eof, st)
        call check(error, st%ok() .and. size(keys) == 1, "one key")
        if (allocated(error)) return
        call check(error, value_eq(keys(1), value_type(name="up")) .and. &
                   value_eq(binding, value_type(name="previous_line")), "up")
        if (allocated(error)) return
        call read_key_sequence(keys, binding, eof, st)
        call check(error, value_eq(binding, value_type(name="execute_extended_command")), "M-x")
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
        call define_key(global, kbd("<f1>"), value_type(name="help-command"), st)
        call make_sparse_keymap(fkeys)
        call define_key(fkeys, kbd("ESC O P"), list(value_type(name="f1")), st)
        local_function_key_map = fkeys
        call kbd_store_bytes(char(27)//"OP")
        call read_key_sequence(keys, binding, eof, st)
        call check(error, size(keys) == 1 .and. value_eq(binding, value_type(name="help-command")), "translated")
        if (allocated(error)) return
        call define_key(global, kbd("M-O P"), value_type(name="foo"), st)
        call kbd_store_bytes(char(27)//"OP")
        call read_key_sequence(keys, binding, eof, st)
        call check(error, size(keys) == 3 .and. value_eq(binding, value_type(name="foo")), "defined wins")
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
        call define_key(global, kbd("a"), value_type(name="cmd-a"), st)
        call define_key(global, kbd("<f2>"), value_type(name="cmd-f2"), st)
        call kbd_store_bytes("AB")
        call read_key_sequence(keys, binding, eof, st)
        call check(error, value_eq(binding, value_type(name="cmd-a")) .and. xfixnum(keys(1)) == 97 &
                   .and. this_command_keys_shift_translated, "A -> a")
        if (allocated(error)) return
        call read_key_sequence(keys, binding, eof, st)
        call check(error,.not. allocated(binding%a) .and. xfixnum(keys(1)) == 66, "B stays")
        if (allocated(error)) return
        key(1) = value_type(name="S-f2")
        call unread_command_events(key)
        call read_key_sequence(keys, binding, eof, st)
        call check(error, value_eq(binding, value_type(name="cmd-f2")) .and. value_eq(keys(1), value_type(name="f2")), "S-<f2>")
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
        call define_key(global, kbd("a"), value_type(name="cmd-a"), st)
        call define_key(global, kbd("C-c b"), value_type(name="cmd-b"), st)
        call define_key(global, kbd("f"), value_type(name="cmd-fail"), st)
        call define_key(global, kbd("q"), value_type(name="cmd-exit"), st)
        call define_key(global, kbd("v"), value_type(name="void-command"), st)
        call check(error, is_command(value_type(name="cmd-a")) .and. .not. is_command(value_type(name="void-command")), &
                   "is_command")
        if (allocated(error)) return
        count_a = 0
        count_b = 0
        call kbd_store_bytes("aa"//char(3)//"b")
        call command_loop(st)
        call check(error, count_a == 2 .and. count_b == 1, "counts")
        if (allocated(error)) return
        call check(error, associated(last_command, cmd_cmd_b), "last-command")
        if (allocated(error)) return
        call check(error, size(this_command_keys) == 0, "keys cleared after the command")
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
        call check(error, last_message, "Wrong type argument: expected type(command_type), got void-command")
        if (allocated(error)) return
        ! Errors don't stop the loop; cmd-exit does.  An error discards
        ! the input typed ahead and rings the bell.
        call kbd_store_bytes("fa")
        ring_bell_hook => type_aqa
        call command_loop(st)
        call check(error, count_a == 3 .and. .not. associated(ring_bell_hook), "exit")
        if (allocated(error)) return
        call check(error, is_input_pending(), "input left")
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
        call define_key(global, kbd("a"), value_type(name="cmd-a"), st)
        call define_key(global, kbd("b"), value_type(name="cmd-b"), st)
        call define_key(global, kbd("C-u"), value_type(name="test-universal-argument"), st)
        call define_key(global, kbd("C-c m"), value_type(text="ab"), st)
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
        call define_key(global, kbd("a"), value_type(name="cmd-a"), st)
        call define_key(global, kbd("g"), value_type(name="cmd-check"), st)
        call define_key(global, kbd("C-c m"), value_type(text="ag"), st)
        call define_key(global, kbd("C-c u"), value_type(text="za"), st)
        call define_key(global, kbd("C-c 0"), list(value_type(3_int64), value_type(109_int64)), st)
        call define_key(global, kbd("C-c z"), value_type(name="test-zero-argument"), st)
        cmd_zero_argument = command_type("test-zero-argument", cmd_zero)
        call cmd_zero_argument%define()
        count_a = 0
        ! C-c m with a count of 0 from the prefix.
        call kbd_store_bytes(char(3)//"z"//char(3)//"m")
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
        call define_key(global, kbd("C-x C-s"), value_type(name="save_buffer"), st)
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
            call check(error, st%ok() .and. value_eq(binding, value_type(name="save_buffer")), "C-x C-s")
            call reset_sys_modes(slave, old, st)
        end block run

        call set_input_fd(-1_int32)
        call close_fd(slave)
        call close_fd(master)
        call free(global)
    end subroutine test_tty

    ! ------------------------------------------------------------------
    ! Step 42c: keyboard.c
    ! ------------------------------------------------------------------

    !> As keyboard-tests.el's keyboard-tests--rks-observe: read a key
    !> sequence from EVENTS with LOCAL_MAP as overriding-terminal-local-map,
    !> giving the sequence, its binding and whether it was shift-translated.
    subroutine rks_observe(events, local_map, sequence, binding, shift_translated, dont_downcase_last)
        type(value_type), intent(in) :: events(:)
        type(handle_type), intent(in) :: local_map
        type(value_type), allocatable, intent(out) :: sequence(:)
        type(value_type), intent(out) :: binding
        logical, intent(out) :: shift_translated
        logical, intent(in), optional :: dont_downcase_last
        type(handle_type) :: saved
        type(value_type) :: b
        type(status_type) :: st
        logical :: eof

        call clear_this_command_keys()
        this_command_keys_shift_translated = .false.
        saved = overriding_terminal_local_map
        overriding_terminal_local_map = local_map
        call unread_command_events(events)
        call read_key_sequence_vs(sequence, b, eof, st, dont_downcase_last=dont_downcase_last)
        call key_binding(sequence, binding, st)
        shift_translated = this_command_keys_shift_translated
        overriding_terminal_local_map = saved
    end subroutine rks_observe

    !> The events of key description TEXT followed by symbol events NAMES.
    function evs(text) result(e)
        character(len=*), intent(in) :: text
        type(value_type), allocatable :: e(:)

        e = kbd(text)
    end function evs

    !> Ports keyboard-tests.el's read-key-sequence tests: keymap
    !> interaction, the translation pipeline and the fallbacks (those on
    !> frames, mouse events and help-char excepted).
    subroutine test_rks(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: global, map, decode, fkeys, trans
        type(status_type) :: st
        type(value_type), allocatable :: seq(:)
        type(value_type) :: binding, ev(1)
        logical :: shifted

        call fresh_global(global)
        ! keyboard-tests-rks-prefix-then-key
        call make_sparse_keymap(map)
        call define_key(map, kbd("C-c a"), value_type(name="ignore"), st)
        call rks_observe(evs("C-c a"), map, seq, binding, shifted)
        call check(error, key_description(seq) == "C-c a" .and. value_eq(binding, value_type(name="ignore")), "prefix-then-key")
        if (allocated(error)) return
        call free(map)
        ! keyboard-tests-rks-unbound-returns-sequence
        call make_sparse_keymap(map)
        ev(1) = value_type(name="symbol-not-bound")
        call rks_observe(ev, map, seq, binding, shifted)
        call check(error, key_description(seq), "<symbol-not-bound>")
        if (allocated(error)) return
        call free(map)
        ! keyboard-tests-rks-bound-sequence-ends-read
        call make_sparse_keymap(map)
        call make_sparse_keymap(decode)
        input_decode_map = decode
        call define_key(decode, kbd("C-c a b"), list(value_type(name="f99")), st)
        call define_key(map, kbd("C-c C-c C-c"), value_type(name="ignore"), st)
        call define_key(map, kbd("<f99>"), value_type(name="undo"), st)
        call rks_observe(evs("C-c C-c C-c a b"), map, seq, binding, shifted)
        call check(error, key_description(seq) == "C-c C-c C-c" .and. value_eq(binding, value_type(name="ignore")), &
                   "bound-sequence-ends-read")
        if (allocated(error)) return
        call discard_input()
        call free(map)
        call free(decode)
        ! keyboard-tests-rks-decode-fires-without-binding
        call make_sparse_keymap(map)
        call make_sparse_keymap(decode)
        input_decode_map = decode
        call define_key(decode, kbd("C-c a b"), list(value_type(name="f99")), st)
        call define_key(map, kbd("C-c C-c <f99>"), value_type(name="ignore"), st)
        call rks_observe(evs("C-c C-c C-c a b"), map, seq, binding, shifted)
        call check(error, key_description(seq) == "C-c C-c <f99>" .and. value_eq(binding, value_type(name="ignore")), &
                   "decode-fires-without-binding")
        if (allocated(error)) return
        call free(map)
        call free(decode)
        ! keyboard-tests-rks-input-decode-map
        call make_sparse_keymap(map)
        call make_sparse_keymap(decode)
        input_decode_map = decode
        call define_key(map, kbd("C-c b"), value_type(name="ignore"), st)
        call define_key(decode, kbd("C-c a"), list(value_type(3_int64), value_type(98_int64)), st)
        call rks_observe(evs("C-c a"), map, seq, binding, shifted)
        call check(error, key_description(seq) == "C-c b" .and. value_eq(binding, value_type(name="ignore")), "input-decode-map")
        if (allocated(error)) return
        call free(map)
        ! keyboard-tests-rks-input-decode-before-function-key
        call make_sparse_keymap(map)
        call make_sparse_keymap(fkeys)
        local_function_key_map = fkeys
        call define_key(map, kbd("C-c c"), value_type(name="ignore"), st)
        call define_key(fkeys, kbd("C-c b"), list(value_type(3_int64), value_type(99_int64)), st)
        call rks_observe(evs("C-c a"), map, seq, binding, shifted)
        call check(error, key_description(seq), "C-c b")
        if (allocated(error)) return
        input_decode_map = null_handle()
        call free(decode)
        call free(map)
        call free(fkeys)
        ! keyboard-tests-rks-function-key-map
        call make_sparse_keymap(map)
        call make_sparse_keymap(fkeys)
        local_function_key_map = fkeys
        call define_key(map, kbd("C-c b"), value_type(name="ignore"), st)
        call define_key(fkeys, kbd("C-c a"), list(value_type(3_int64), value_type(98_int64)), st)
        call rks_observe(evs("C-c a"), map, seq, binding, shifted)
        call check(error, key_description(seq) == "C-c b" .and. value_eq(binding, value_type(name="ignore")), "function-key-map")
        if (allocated(error)) return
        ! keyboard-tests-rks-function-key-map-on-undefined
        call define_key(map, kbd("C-c a"), value_type(name="undefined"), st)
        call rks_observe(evs("C-c a"), map, seq, binding, shifted)
        call check(error, key_description(seq) == "C-c b" .and. value_eq(binding, value_type(name="ignore")), &
                   "function-key-map-on-undefined")
        if (allocated(error)) return
        ! keyboard-tests-rks-function-key-skipped-when-bound
        call define_key(map, kbd("C-c a"), value_type(name="ignore"), st)
        call define_key(map, kbd("C-c b"), value_type(name="undo"), st)
        call rks_observe(evs("C-c a"), map, seq, binding, shifted)
        call check(error, key_description(seq) == "C-c a" .and. value_eq(binding, value_type(name="ignore")), &
                   "function-key-skipped-when-bound")
        if (allocated(error)) return
        call free(map)
        ! keyboard-tests-rks-key-translation-after-function-key
        call make_sparse_keymap(map)
        call make_sparse_keymap(trans)
        key_translation_map = trans
        call define_key(map, kbd("C-c c"), value_type(name="ignore"), st)
        call define_key(trans, kbd("C-c b"), list(value_type(3_int64), value_type(99_int64)), st)
        call rks_observe(evs("C-c a"), map, seq, binding, shifted)
        call check(error, key_description(seq) == "C-c c" .and. value_eq(binding, value_type(name="ignore")), &
                   "key-translation-after-function-key")
        if (allocated(error)) return
        local_function_key_map = null_handle()
        call free(fkeys)
        call free(map)
        call free(trans)
        ! keyboard-tests-rks-key-translation-overrides-bound-sequence
        call make_sparse_keymap(map)
        call make_sparse_keymap(trans)
        key_translation_map = trans
        call define_key(map, kbd("C-c a"), value_type(name="ignore"), st)
        call define_key(map, kbd("C-c b"), value_type(name="undo"), st)
        call define_key(trans, kbd("C-c a"), list(value_type(3_int64), value_type(98_int64)), st)
        call rks_observe(evs("C-c a"), map, seq, binding, shifted)
        call check(error, key_description(seq) == "C-c b" .and. value_eq(binding, value_type(name="undo")), &
                   "key-translation-overrides-bound-sequence")
        if (allocated(error)) return
        key_translation_map = null_handle()
        call free(map)
        call free(trans)
        ! keyboard-tests-rks-upcase-fallback
        call make_sparse_keymap(map)
        call define_key(map, kbd("C-c a"), value_type(name="ignore"), st)
        call rks_observe(evs("C-c A"), map, seq, binding, shifted)
        call check(error, key_description(seq) == "C-c a" .and. value_eq(binding, value_type(name="ignore")) .and. shifted, &
                   "upcase-fallback")
        if (allocated(error)) return
        ! keyboard-tests-rks-no-upcase-fallback
        translate_upper_case_key_bindings = .false.
        call rks_observe(evs("C-c A"), map, seq, binding, shifted)
        translate_upper_case_key_bindings = .true.
        call check(error, key_description(seq) == "C-c A" .and. .not. shifted, "no-upcase-fallback")
        if (allocated(error)) return
        ! keyboard-tests-rks-dont-downcase-last
        call rks_observe(evs("C-c A"), map, seq, binding, shifted, dont_downcase_last=.true.)
        call check(error, key_description(seq) == "C-c A" .and. .not. shifted, "dont-downcase-last")
        if (allocated(error)) return
        call free(map)
        ! keyboard-tests-rks-shifted-function-key-fallback
        call make_sparse_keymap(map)
        call define_key(map, kbd("<f2>"), value_type(name="ignore"), st)
        ev(1) = value_type(name="S-f2")
        call rks_observe(ev, map, seq, binding, shifted)
        call check(error, key_description(seq) == "<f2>" .and. value_eq(binding, value_type(name="ignore")) .and. shifted, &
                   "shifted-function-key-fallback")
        call free(map)
        call free(global)
    end subroutine test_rks

    !> Ports keyboard-tests.el's keyboard-lossage-size.
    subroutine test_lossage_size(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer :: orig, factor, n
        integer, parameter :: factors(7) = [1, 3, 4, 5, 10, 7, 3]

        orig = lossage_size()

        do factor = 1, size(factors)
            n = factors(factor)*100
            call check(error, lossage_size(n, st) == n .and. st%ok(), "set")
            if (allocated(error)) return
        end do

        ! Wrong type.
        n = lossage_size(-5, st)
        call check(error, st%code == FM_USER_ERROR, "negative")
        if (allocated(error)) return
        ! Less that minimum value.
        n = lossage_size(99, st)
        call check(error, format_error(st), "Value must be >= 100")
        if (allocated(error)) return
        call check(error, lossage_size(orig, st) == orig, "restore")
    end subroutine test_lossage_size

    !> Ports keyboard-tests.el's keyboard-inhibit-interaction.
    subroutine test_inhibit_interaction(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        type(value_type) :: ev
        integer(int64) :: c
        logical :: have

        inhibit_interaction = .true.
        call read_char(c, have, st, prompt="foo: ")
        call check(error, st%code == FM_INHIBITED_INTERACTION, "read-char")

        if (allocated(error)) then
            inhibit_interaction = .false.
            return
        end if

        call read_event(ev, have, st, prompt="foo: ")
        call check(error, st%code == FM_INHIBITED_INTERACTION, "read-event")

        if (allocated(error)) then
            inhibit_interaction = .false.
            return
        end if

        call read_char_exclusive(c, have, st, prompt="foo: ")
        inhibit_interaction = .false.
        call check(error, format_error(st), "User interaction while inhibited")
    end subroutine test_inhibit_interaction

    !> read-char resolves modifiers and takes keypad keys as characters;
    !> read-char-exclusive skips function keys, read-char signals an error.
    subroutine test_read_char(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        type(value_type) :: e(3)
        integer(int64) :: c
        logical :: have

        call discard_input()
        e(1) = value_type(ior(CTL, 97_int64))
        e(2) = value_type(name="kp-5")
        e(3) = value_type(name="f1")
        call unread_command_events(e)
        call read_char(c, have, st, prompt="Char: ")
        call check(error, have .and. c == 1 .and. last_message == "Char: ", "C-a")
        if (allocated(error)) return
        call read_char(c, have, st)
        call check(error, have .and. c == 53, "kp-5")
        if (allocated(error)) return
        call read_char(c, have, st)
        call check(error, format_error(st) == "Non-character input-event" .and. is_input_pending(), "f1")
        if (allocated(error)) return
        call kbd_store_bytes("x")
        call read_char_exclusive(c, have, st)
        call check(error, have .and. c == 120, "exclusive")
        if (allocated(error)) return
        call read_char(c, have, st, seconds=0.01)
        call check(error, st%ok() .and. .not. have, "timeout")
    end subroutine test_read_char

    !> this-command-keys and friends, inside a command and after a
    !> translation; clear-this-command-keys and set--this-command-keys.
    subroutine test_this_command_keys(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: global, decode
        type(status_type) :: st
        type(value_type), allocatable :: keys(:)
        type(value_type) :: binding
        logical :: eof

        call fresh_global(global)
        cmd_cmd_keys = command_type("cmd-keys", cmd_keys)
        call cmd_cmd_keys%define()
        call define_key(global, kbd("C-c <up>"), value_type(name="cmd-keys"), st)
        call make_sparse_keymap(decode)
        call define_key(decode, kbd("ESC [ A"), list(value_type(name="up")), st)
        input_decode_map = decode
        call kbd_store_bytes(char(3)//char(27)//"[A")
        call command_loop(st)
        call check(error, seen_keys, "C-c <up>")
        if (allocated(error)) return
        call check(error, seen_single_keys, "C-c <up>")
        if (allocated(error)) return
        call check(error, seen_raw_keys, "C-c M-[ A")
        if (allocated(error)) return
        ! After the command, the keys are cleared.
        call check(error, size(this_command_key_events()) == 0, "cleared")
        if (allocated(error)) return
        ! read-key-sequence: the last key sequence read.
        call kbd_store_bytes(char(3)//char(27)//"[A")
        call read_key_sequence_vs(keys, binding, eof, st)
        call check(error, key_description(this_single_command_keys()) == "C-c <up>" .and. &
                   value_eq(binding, value_type(name="cmd-keys")), "read-key-sequence")
        if (allocated(error)) return
        call clear_this_command_keys(keep_record=.true.)
        call check(error, size(this_command_key_events()) == 0, "clear")
        if (allocated(error)) return
        call set_this_command_keys(char(195)//char(184)//"y")
        call check(error, key_description(this_command_key_events()), "M-x y")
        input_decode_map = null_handle()
        call free(decode)
        call free(global)
    end subroutine test_this_command_keys

    !> Keystrokes are echoed when input pauses in the middle of a key
    !> sequence (here, as input ends there, the keys are then shown as
    !> read-key-sequence finishes); echo_now shows "C-x-" with the help
    !> hint, or without it.
    subroutine test_echo(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: global
        type(status_type) :: st
        type(value_type), allocatable :: keys(:)
        type(value_type) :: binding
        logical :: eof
        real :: saved

        call fresh_global(global)
        call define_key(global, kbd("C-x a"), value_type(name="ignore"), st)
        saved = echo_keystrokes
        echo_keystrokes = 0.01
        call cancel_echoing()
        last_message = ""
        call kbd_store_bytes(char(24))
        call read_key_sequence_vs(keys, binding, eof, st)
        call check(error, last_message, "C-x")

        if (allocated(error)) then
            echo_keystrokes = saved
            return
        end if

        ! No echo when echo-keystrokes is 0.
        echo_keystrokes = 0.0
        call cancel_echoing()
        last_message = ""
        call kbd_store_bytes(char(24))
        call read_key_sequence_vs(keys, binding, eof, st)
        echo_keystrokes = saved
        call check(error, last_message, "")
        if (allocated(error)) return
        call cancel_echoing()
        call set_this_command_keys(char(24))
        call echo_now()
        call check(error, last_message, "C-x- (C-h for help)")
        if (allocated(error)) return
        echo_keystrokes_help = .false.
        call cancel_echoing()
        call echo_now()
        echo_keystrokes_help = .true.
        call cancel_echoing()
        call check(error, last_message, "C-x-")
        call clear_this_command_keys(keep_record=.true.)
        call free(global)
    end subroutine test_echo

    !> event-convert-list, internal-event-symbol-parse-modifiers and
    !> make_ctrl_char.
    subroutine test_event_symbols(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        type(value_type) :: ev, parsed, d2(2), d3(3)

        d2(1) = value_type(name="control")
        d2(2) = value_type(97_int64)
        call event_convert_list(d2, ev, st)
        call check(error, xfixnum(ev), 1_int64)
        if (allocated(error)) return
        d3(1) = value_type(name="control")
        d3(2) = value_type(name="meta")
        d3(3) = value_type(name="a")
        call event_convert_list(d3, ev, st)
        call check(error, xfixnum(ev), META + 1)
        if (allocated(error)) return
        d2(1) = value_type(name="shift")
        d2(2) = value_type(97_int64)
        call event_convert_list(d2, ev, st)
        call check(error, xfixnum(ev), 65_int64)
        if (allocated(error)) return
        d2(1) = value_type(name="control")
        d2(2) = value_type(65_int64)
        call event_convert_list(d2, ev, st)
        call check(error, xfixnum(ev), SHIFT + 1)
        if (allocated(error)) return
        d2(1) = value_type(name="control")
        d2(2) = value_type(37_int64)
        call event_convert_list(d2, ev, st)
        call check(error, xfixnum(ev), CTL + 37)
        if (allocated(error)) return
        d3(1) = value_type(name="meta")
        d3(2) = value_type(name="control")
        d3(3) = value_type(name="f1")
        call event_convert_list(d3, ev, st)
        call check(error, value_eq(ev, value_type(name="C-M-f1")), "C-M-<f1>")
        if (allocated(error)) return
        d2(1) = value_type(97_int64)
        d2(2) = value_type(98_int64)
        call event_convert_list(d2, ev, st)
        call check(error, format_error(st), "Two bases given in one event")
        if (allocated(error)) return
        parsed = event_name_parse_modifiers(value_type(name="C-M-f1"))
        call check(error, value_equal(parsed, list(value_type(name="f1"), value_type(name="meta"), value_type(name="control"))), &
                   "C-M-f1")
        if (allocated(error)) return
        parsed = event_name_parse_modifiers(value_type(name="down-mouse-1"))
        call check(error, value_equal(parsed, list(value_type(name="mouse-1"), value_type(name="down"))), "down-mouse-1")
        if (allocated(error)) return
        parsed = event_name_parse_modifiers(value_type(name="mouse-2"))
        call check(error, value_equal(parsed, list(value_type(name="mouse-2"), value_type(name="click"))), "mouse-2")
        if (allocated(error)) return
        call check(error, make_ctrl_char(iachar("@", int64)), 0_int64)
    end subroutine test_event_symbols

    !> recursion-depth in a recursive edit, top-level leaving it without
    !> a message, and an error in post-command-hook shown and its function
    !> removed (safe_execute_hooks).
    subroutine test_recursive_edit(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: global
        type(status_type) :: st

        call fresh_global(global)
        cmd_cmd_depth = command_type("cmd-depth", cmd_depth)
        call cmd_cmd_depth%define()
        cmd_cmd_top = command_type("cmd-top", cmd_top)
        call cmd_cmd_top%define()
        cmd_cmd_fail = command_type("cmd-fail", cmd_fail)
        call cmd_cmd_fail%define()
        call define_key(global, kbd("d"), value_type(name="cmd-depth"), st)
        call define_key(global, kbd("t"), value_type(name="cmd-top"), st)
        seen_depth = -1
        last_message = ""
        call kbd_store_bytes("dt")
        call recursive_edit(st)
        call check(error, seen_depth == 1 .and. st%code == FM_TOP_LEVEL .and. recursion_depth() == 0, "top-level")
        if (allocated(error)) return
        call check(error, last_message, "")
        if (allocated(error)) return
        call post_command_hook%append(cmd_fail)
        call safe_execute_hooks(post_command_hook)
        call check(error, last_message, "Error in post_command_hook (cmd-fail): (user_error ""Boom"")")
        if (allocated(error)) return
        call check(error,.not. post_command_hook%member(cmd_fail), "removed")
        call free(global)
    end subroutine test_recursive_edit
end module test_keyboard_fm
