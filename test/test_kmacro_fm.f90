! test_kmacro_fm.f90 -- tests for kmacro_fm, macros_fm and edmacro_fm.
! Ports GNU Emacs 31.1 test/lisp/kmacro-tests.el in part; the others are
! not ported yet.
!
! Copyright (C) 2017-2026 Free Software Foundation, Inc.
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

module test_kmacro_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use test_support, only: show_alone
    use error_fm, only: status_type, format_error, FM_QUIT
    use handle_fm, only: handle_type
    use lisp_fm, only: value_type, list, xfixnum
    use data_fm, only: value_eq
    use buffer_fm, only: get_buffer_create, set_buffer, kill_buffer, erase_buffer, set_buffer_read_only, current_buffer
    use editfns_fm, only: point, goto_char, buffer_string
    use keymap_fm, only: key_parse, key_binding
    use callint_fm, only: current_prefix_arg, prefix_arg
    use keyboard_fm, only: kbd_store_bytes, command_loop, discard_input, last_message, unread_command_events, &
                           defining_kbd_macro, DEFINING_KBD_MACRO_NO, DEFINING_KBD_MACRO_APPEND, last_kbd_macro, &
                           this_command_keys, last_input_event, store_kbd_macro_event, finalize_kbd_macro_chars, &
                           kbd_macro_termination_hook, last_command, store_event
    use bindings_fm, only: init_bindings
    use register_fm, only: set_register, get_register, register_contents_type, REGISTER_NUMBER
    use rect_fm, only: rectangle_line_type
    use xdisp_fm, only: mode_line_modes_faces, face_span_type
    use edmacro_fm, only: format_kbd_macro
    use kmacro_fm, only: kmacro_insert_counter, kmacro_set_counter, kmacro_set_format, kmacro_start_macro, &
                         kmacro_end_macro, kmacro_end_or_execute_macro, kmacro_end_and_execute_macro, kmacro_execute_macro, &
                         kmacro_execute_ring_2nd, kmacro_view_ring_2nd, kmacro_cycle_ring_next, kmacro_cycle_ring_previous, &
                         kmacro_swap_ring, kmacro_delete_ring_head, kmacro_view_macro, kmacro_start_macro_or_insert_counter, &
                         kmacro_ring, kmacro_counter, kmacro_ring_max, kmacro_execute_repeat_key, kmacro_execute_repeat_with_arg, &
                         kmacro_initial_counter_value, kmacro_counter_format, kmacro_default_counter_format, &
                         kmacro_counter_format_start, kmacro_counter_value_start, kmacro_last_counter, &
                         kmacro_execute_before_append, kmacro_repeat_no_prefix, KMACRO_EXECUTE_REPEAT_DISABLED, &
                         KMACRO_EXECUTE_REPEAT_LAST_KEY, KMACRO_REPEAT_NO
    implicit none(type, external)
    private

    public :: collect_kmacro_fm

    !> Calls of the termination hook function.
    integer :: terminations = 0

contains

    !> Collect all exported unit tests.
    subroutine collect_kmacro_fm(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("kmacro-tests-test-insert-counter-01-nil", test_insert_counter_01), &
                    new_unittest("kmacro-tests-test-insert-counter-02-int", test_insert_counter_02), &
                    new_unittest("kmacro-tests-test-insert-counter-03-list", test_insert_counter_03), &
                    new_unittest("kmacro-tests-test-insert-counter-04-neg", test_insert_counter_04), &
                    new_unittest("kmacro-tests-test-start-format-counter", test_start_format_counter), &
                    new_unittest("kmacro-tests-test-start-macro-when-defining-macro", test_start_when_defining), &
                    new_unittest("kmacro-tests-set-macro-counter-while-defining", test_counter_while_defining), &
                    new_unittest("kmacro-tests-start-insert-counter-appends-to-macro", test_appends_to_macro), &
                    new_unittest("kmacro-tests-test-reg-and-quit-counters", test_reg_and_quit_counters), &
                    new_unittest("kmacro-tests-end-call-macro-prefix-args", test_end_execute_prefix_args), &
                    new_unittest("kmacro-tests-end-and-call-macro", test_end_and_execute_macro), &
                    new_unittest("kmacro-tests-call-macro-hint-and-repeat", test_hint_and_repeat), &
                    new_unittest("kmacro-tests-test-ring-2nd-commands", test_ring_2nd_commands), &
                    new_unittest("kmacro-tests-fill-ring-and-rotate", test_fill_ring_and_rotate), &
                    new_unittest("kmacro-tests-test-ring-commands-when-no-macros", test_ring_no_macros), &
                    new_unittest("kmacro-tests-repeat-view-and-run", test_repeat_view_and_execute), &
                    new_unittest("recording", test_recording), &
                    new_unittest("recording/prefix-args", test_recording_prefix), &
                    new_unittest("recording/end-with-repeat-count", test_end_with_count), &
                    new_unittest("recording/c-g-cancels", test_quit_cancels), &
                    new_unittest("recording/error-cancels", test_error_cancels), &
                    new_unittest("f3-f4-counter", test_f3_f4), &
                    new_unittest("termination-hook", test_termination_hook), &
                    new_unittest("mode-line-def", test_mode_line), &
                    new_unittest("format-kbd-macro", test_format_kbd_macro), &
                    new_unittest("keys", test_keys) &
                    ]
    end subroutine collect_kmacro_fm

    ! ------------------------------------------------------------------
    ! Fixtures
    ! ------------------------------------------------------------------

    !> A clean environment (kmacro-tests-with-kmacro-clean-slate) in a
    !> fresh buffer shown in the selected window.
    subroutine fresh(h)
        type(handle_type), intent(out) :: h
        type(status_type) :: st

        call get_buffer_create("*kmacro-test*", h, st)
        call set_buffer(h, st)
        call set_buffer_read_only(h, .false., st)
        call erase_buffer(st)
        call show_alone()
        defining_kbd_macro = DEFINING_KBD_MACRO_NO
        call discard_input()
        prefix_arg = value_type()
        current_prefix_arg = value_type()
        call init_bindings()
        kmacro_execute_before_append = .true.
        kmacro_ring_max = 8
        kmacro_repeat_no_prefix = .true.
        kmacro_execute_repeat_key = KMACRO_EXECUTE_REPEAT_DISABLED
        kmacro_execute_repeat_with_arg = .false.
        if (allocated(last_kbd_macro)) deallocate (last_kbd_macro)
        if (allocated(kmacro_ring)) deallocate (kmacro_ring)
        kmacro_counter = 0
        kmacro_default_counter_format = "%d"
        kmacro_counter_format = "%d"
        kmacro_counter_format_start = "%d"
        kmacro_counter_value_start = 0
        kmacro_last_counter = 0
        if (allocated(kmacro_initial_counter_value)) deallocate (kmacro_initial_counter_value)
        if (allocated(this_command_keys)) deallocate (this_command_keys)
        allocate (this_command_keys(0))
        last_message = ""
    end subroutine fresh

    !> The events of KEYS (kbd syntax).
    function kbd(keys) result(evs)
        character(len=*), intent(in) :: keys
        type(value_type), allocatable :: evs(:)
        type(status_type) :: st

        call key_parse(keys, evs, st)
    end function kbd

    !> The characters of TEXT as events.
    function chars(text) result(evs)
        character(len=*), intent(in) :: text
        type(value_type), allocatable :: evs(:)
        integer :: k

        allocate (evs(len(text)))

        do k = 1, len(text)
            evs(k) = value_type(int(iachar(text(k:k)), int64))
        end do
    end function chars

    !> Whether EVS are the events of `last_kbd_macro'.
    logical function last_macro_is(evs)
        type(value_type), intent(in) :: evs(:)
        integer :: k

        last_macro_is = .false.
        if (.not. allocated(last_kbd_macro)) return
        if (size(last_kbd_macro) /= size(evs)) return

        do k = 1, size(evs)
            if (.not. value_eq(last_kbd_macro(k), evs(k))) return
        end do

        last_macro_is = .true.
    end function last_macro_is

    !> Run the command loop over the events EVS.
    subroutine type_events(evs)
        type(value_type), intent(in) :: evs(:)
        type(status_type) :: st
        integer :: k

        ! As typed: into the event queue (unread events are not typed).
        do k = 1, size(evs)
            call store_event(evs(k))
        end do

        call command_loop(st)
    end subroutine type_events

    !> Run the command loop over the terminal input BYTES.
    subroutine type_bytes(bytes)
        character(len=*), intent(in) :: bytes
        type(status_type) :: st

        call kbd_store_bytes(bytes)
        call command_loop(st)
    end subroutine type_bytes

    !> Define EVS as a keyboard macro with the kmacro commands
    !> (kmacro-tests-define-macro): the events are recorded between
    !> `kmacro_start_macro' and `kmacro_end_macro'.
    subroutine define_macro(evs)
        type(value_type), intent(in) :: evs(:)
        type(status_type) :: st
        integer :: k

        current_prefix_arg = value_type()
        call kmacro_start_macro(value_type(), st)

        do k = 1, size(evs)
            call store_kbd_macro_event(evs(k))
        end do

        call finalize_kbd_macro_chars()
        call kmacro_end_macro(value_type(), st)
    end subroutine define_macro

    !> Insert the counter as kmacro-tests-simulate-command does, with
    !> current-prefix-arg ARG.
    subroutine insert_counter(arg)
        type(value_type), intent(in) :: arg
        type(status_type) :: st

        current_prefix_arg = arg
        call kmacro_insert_counter(arg, st)
        current_prefix_arg = value_type()
    end subroutine insert_counter

    !> Set the counter as kmacro-tests-simulate-command does.
    subroutine set_counter(n, arg)
        integer(int64), intent(in) :: n
        type(value_type), intent(in), optional :: arg
        type(status_type) :: st

        current_prefix_arg = value_type(n)
        if (present(arg)) current_prefix_arg = arg
        call kmacro_set_counter(n, st)
        current_prefix_arg = value_type()
    end subroutine set_counter

    !> The text of the current buffer.
    function text() result(s)
        character(len=:), allocatable :: s

        s = buffer_string()
    end function text

    ! ------------------------------------------------------------------
    ! Ported from kmacro-tests.el
    ! ------------------------------------------------------------------

    !> `kmacro-insert-counter' adds one to macro counter with nil arg.
    subroutine test_insert_counter_01(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call fresh(h)
        call insert_counter(value_type())
        call insert_counter(value_type())
        call check(error, text(), "01")
        call kill_buffer(h, st)
    end subroutine test_insert_counter_01

    !> `kmacro-insert-counter' increments by value of list argument.
    subroutine test_insert_counter_02(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call fresh(h)
        call insert_counter(value_type(2_int64))
        call insert_counter(value_type(3_int64))
        call insert_counter(value_type())
        call check(error, text(), "025")
        call kill_buffer(h, st)
    end subroutine test_insert_counter_02

    !> `kmacro-insert-counter' doesn't increment when given universal argument.
    subroutine test_insert_counter_03(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call fresh(h)
        call insert_counter(list(value_type(16_int64)))
        call insert_counter(list(value_type(4_int64)))
        call check(error, text(), "00")
        call kill_buffer(h, st)
    end subroutine test_insert_counter_03

    !> `kmacro-insert-counter' decrements with '- prefix argument
    subroutine test_insert_counter_04(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call fresh(h)
        call insert_counter(value_type(name="-"))
        call insert_counter(value_type())
        call check(error, text(), "0-1")
        call kill_buffer(h, st)
    end subroutine test_insert_counter_04

    !> `kmacro-insert-counter' uses start value and format.
    subroutine test_start_format_counter(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call fresh(h)
        call set_counter(10_int64)
        call insert_counter(value_type())
        call insert_counter(value_type())
        call kmacro_set_format("c=%s")
        call set_counter(50_int64)
        call insert_counter(value_type())
        call check(error, text(), "1011c=50")
        call kill_buffer(h, st)
    end subroutine test_start_format_counter

    !> Starting a macro while defining a macro does not start a second macro.
    subroutine test_start_when_defining(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call fresh(h)
        call kmacro_start_macro(value_type(), st)
        call check(error, defining_kbd_macro /= DEFINING_KBD_MACRO_NO .and. .not. allocated(last_kbd_macro), &
                   "recording")
        if (allocated(error)) return
        call kmacro_start_macro(value_type(), st)
        call check(error, defining_kbd_macro /= DEFINING_KBD_MACRO_NO .and. .not. allocated(last_kbd_macro), &
                   "still recording")
        if (allocated(error)) return
        call check(error, last_message, "Already defining keyboard macro.")
        call discard_input()
        call kill_buffer(h, st)
    end subroutine test_start_when_defining

    !> Use of the prefix arg with kmacro-start sets kmacro-counter.
    subroutine test_counter_while_defining(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call fresh(h)
        current_prefix_arg = value_type(5_int64)
        call kmacro_start_macro(value_type(5_int64), st)
        current_prefix_arg = value_type()
        call insert_counter(value_type())
        call set_counter(1_int64)
        call insert_counter(value_type())
        ! Using universal arg to set counter should reset to starting value.
        call set_counter(4_int64, list(value_type(4_int64)))
        call insert_counter(value_type())
        call check(error, text(), "515")
        call discard_input()
        call kill_buffer(h, st)
    end subroutine test_counter_while_defining

    !> Use of the universal arg appends to the previous macro.
    subroutine test_appends_to_macro(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call fresh(h)
        call kmacro_start_macro_or_insert_counter(value_type(), st)
        call check(error, defining_kbd_macro /= DEFINING_KBD_MACRO_NO, "recording")
        if (allocated(error)) return
        ! Call it again and it should insert the counter.
        call kmacro_start_macro_or_insert_counter(value_type(), st)
        call check(error, text(), "0")
        if (allocated(error)) return
        ! The macro recorded is "hello" (kmacro-tests-end-macro-advice).
        call record(chars("hello"))
        current_prefix_arg = value_type(3_int64)
        call kmacro_end_or_execute_macro(value_type(3_int64), st)
        current_prefix_arg = value_type()
        call check(error, defining_kbd_macro == DEFINING_KBD_MACRO_NO, "finished")
        if (allocated(error)) return
        call check(error, text(), "0hellohello", "the definition repeated")
        if (allocated(error)) return
        call erase_buffer(st)
        ! Now use prefix arg to append to the previous macro.
        ! This should run the previous macro first.
        current_prefix_arg = list(value_type(4_int64))
        call kmacro_start_macro_or_insert_counter(current_prefix_arg, st)
        current_prefix_arg = value_type()
        call check(error, text(), "hello")
        if (allocated(error)) return
        call check(error, defining_kbd_macro == DEFINING_KBD_MACRO_APPEND, "append")
        call discard_input()
        call kill_buffer(h, st)
    end subroutine test_appends_to_macro

    !> Record EVS in the macro being defined, as the finished events of a
    !> command.
    subroutine record(evs)
        type(value_type), intent(in) :: evs(:)
        integer :: k

        do k = 1, size(evs)
            call store_kbd_macro_event(evs(k))
        end do

        call finalize_kbd_macro_chars()
    end subroutine record

    !> The register and quit counter tests of kmacro-tests.el:
    !> test-reg-load, test-reg-save, test-reg-add-counter-equal-01 to -04,
    !> test-reg-add-counter-less, -greater, test-quit-counter-equal-01 to
    !> -04, test-quit-counter-less and -greater.
    subroutine test_reg_and_quit_counters(error)
        type(error_type), allocatable, intent(out) :: error

        call reg_case(error, "reg-load", 4_int64, 1_int64, .true., "C-x C-k C-i C-x C-k C-i C-x C-k C-r l C-r", 2_int64, &
                      "1245")
        if (allocated(error)) return
        call reg_case(error, "reg-save", -1_int64, 1_int64, .true., &
                      "C-x C-k C-i C-x C-k C-r s C-r C-u 2 C-x C-k C-a C-x C-k C-i C-x r i C-r", 2_int64, "142586")
        if (allocated(error)) return
        call reg_case(error, "equal-01", 2_int64, 0_int64, .false., "C-x C-k C-i C-x C-k C-r a = C-r", 4_int64, "0134")
        if (allocated(error)) return
        call reg_case(error, "equal-02", 2_int64, 0_int64, .false., "C-x C-k C-i C-u 2 C-x C-k C-r a = C-r", 4_int64, &
                      "0145")
        if (allocated(error)) return
        call reg_case(error, "equal-03", 2_int64, 0_int64, .false., "C-x C-k C-i C-u C-x C-k C-r a = C-r", 4_int64, &
                      "0167")
        if (allocated(error)) return
        call reg_case(error, "equal-04", 2_int64, 0_int64, .false., "C-x C-k C-i C-u - C-x C-k C-r a = C-r", 4_int64, &
                      "0111")
        if (allocated(error)) return
        call reg_case(error, "less", 6_int64, 7_int64, .true., "C-u - C-x C-k C-r a < C-r C-u - C-x C-k C-i", 4_int64, &
                      "7642")
        if (allocated(error)) return
        call reg_case(error, "greater", 1_int64, 0_int64, .false., "C-x C-k C-i C-x C-k C-r a > C-r", 4_int64, "0135")
        if (allocated(error)) return
        call quit_case(error, "quit-equal-01", 0_int64, .false., "C-x C-k C-i C-u 5 C-x C-k C-q =", "0123", 4_int64, .true.)
        if (allocated(error)) return
        call quit_case(error, "quit-equal-02", 5_int64, .true., "C-u - C-x C-k C-i C-x C-k C-q =", "5432", 1_int64, .true.)
        if (allocated(error)) return
        call quit_case(error, "quit-equal-03", 4_int64, .true., "C-u - C-x C-k C-i C-u - C-x C-k C-q =", "4321", 0_int64, &
                       .true.)
        if (allocated(error)) return
        call quit_case(error, "quit-equal-04", 0_int64, .false., "C-u 2 C-x C-k C-i C-u 7 C-x C-k C-q =", "0246", &
                       10_int64, .false.)
        if (allocated(error)) return
        call quit_case(error, "quit-less", 8_int64, .true., "C-u 5 C-x C-k C-q < C-u - C-x C-k C-i", "8765", 4_int64, &
                       .true.)
        if (allocated(error)) return
        call quit_case(error, "quit-greater", 0_int64, .false., "C-x C-k C-i C-u 4 C-x C-k C-q >", "0123", 4_int64, .true.)
    end subroutine test_reg_and_quit_counters

    !> Register C-r holds REG (none if negative); the counter is set to
    !> START first if SET; the macro MAC run COUNT times inserts EXPECTED.
    subroutine reg_case(error, name, reg, start, set, mac, count, expected)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), intent(in) :: name, mac, expected
        integer(int64), intent(in) :: reg, start, count
        logical, intent(in) :: set
        type(handle_type) :: h
        type(status_type) :: st
        type(rectangle_line_type) :: none(0)

        call fresh(h)

        if (reg >= 0) then
            call set_register(18_int64, reg)
        else
            call set_register(18_int64, none)
        end if

        if (set) call set_counter(start)
        call define_macro(kbd(mac))
        current_prefix_arg = value_type(count)
        call kmacro_end_or_execute_macro(value_type(count), st)
        current_prefix_arg = value_type()
        call check(error, st%ok(), name//": "//format_error(st))
        if (allocated(error)) return
        call check(error, text(), expected, name)
        call set_register(18_int64, none)
        call kill_buffer(h, st)
    end subroutine reg_case

    !> The counter is set to START first if SET; the macro MAC run 4 times
    !> inserts EXPECTED and leaves the counter at AFTER; run once more it
    !> quits if QUITS.
    subroutine quit_case(error, name, start, set, mac, expected, after, quits)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), intent(in) :: name, mac, expected
        integer(int64), intent(in) :: start, after
        logical, intent(in) :: set, quits
        type(handle_type) :: h
        type(status_type) :: st

        call fresh(h)
        if (set) call set_counter(start)
        call define_macro(kbd(mac))
        current_prefix_arg = value_type(4_int64)
        call kmacro_end_or_execute_macro(value_type(4_int64), st)
        call check(error, text(), expected, name)
        if (allocated(error)) return

        if (quits) then
            call check(error, kmacro_counter == after, name//": counter")
            if (allocated(error)) return
        end if

        current_prefix_arg = value_type(1_int64)
        call kmacro_end_or_execute_macro(value_type(1_int64), st)
        current_prefix_arg = value_type()
        call check(error, (st%code == FM_QUIT) .eqv. quits, name//": quit")
        if (allocated(error)) return
        if (.not. quits) call check(error, kmacro_counter == after, name//": counter")
        call kill_buffer(h, st)
    end subroutine quit_case

    !> kmacro-end-call-macro changes behavior based on prefix arg.
    subroutine test_end_execute_prefix_args(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call fresh(h)
        call define_macro(chars("macro #1"))
        call define_macro(chars("macro #2"))
        ! With no prefix arg, it should call the second macro.
        call kmacro_end_or_execute_macro(value_type(), st)
        call check(error, text(), "macro #2")
        if (allocated(error)) return
        ! With universal arg, it should call the first one.
        call kmacro_end_or_execute_macro(list(value_type(4_int64)), st)
        call check(error, text(), "macro #2macro #1")
        call kill_buffer(h, st)
    end subroutine test_end_execute_prefix_args

    !> Keyboard command to end and call macro works under various conditions.
    subroutine test_end_and_execute_macro(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call fresh(h)
        ! First, try it with no macro to record.
        call kmacro_start_macro(value_type(), st)
        call kmacro_end_and_execute_macro(value_type(2_int64), st)
        call check(error, format_error(st), "No kbd macro has been defined")
        if (allocated(error)) return
        ! Check that it stopped defining and that no macro was recorded.
        call check(error, defining_kbd_macro == DEFINING_KBD_MACRO_NO .and. .not. allocated(last_kbd_macro), &
                   "no macro")
        if (allocated(error)) return
        ! Now try it while not recording, but first record a macro.
        call define_macro(chars("macro"))
        call kmacro_end_and_execute_macro(value_type(), st)
        call check(error, text(), "macro")
        call kill_buffer(h, st)
    end subroutine test_end_and_execute_macro

    !> `kmacro-call-macro' gives hint in Messages and sets up repeat keymap.
    !> This is a regression test for: Bug#3412, Bug#11817.
    subroutine test_hint_and_repeat(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call fresh(h)
        call define_macro(chars("m"))
        kmacro_execute_repeat_key = KMACRO_EXECUTE_REPEAT_LAST_KEY
        kmacro_execute_repeat_with_arg = .true.
        last_input_event = value_type(int(iachar("e"), int64))
        this_command_keys = kbd("C-x e")
        last_message = ""
        call kmacro_execute_macro(value_type(3_int64), st)
        call check(error, last_message, "(Type e to repeat macro 3 times)")
        if (allocated(error)) return
        ! Check that it set up for repeat, and run the repeat.
        call type_events(chars("e"))
        call check(error, text(), "mmmmmm")
        call kill_buffer(h, st)
    end subroutine test_hint_and_repeat

    !> 2nd macro in ring is displayed and executed normally and on repeat.
    subroutine test_ring_2nd_commands(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call fresh(h)
        ! Record one macro, with count.
        current_prefix_arg = value_type(1_int64)
        call kmacro_start_macro(value_type(1_int64), st)
        current_prefix_arg = value_type()
        call record(kbd("C-x C-k C-i C-u C-x C-k C-i"))
        call kmacro_end_macro(value_type(), st)
        ! Check that execute and display do nothing with no 2nd macro.
        call kmacro_execute_ring_2nd(value_type(), st)
        call check(error, text(), "")
        if (allocated(error)) return
        call kmacro_view_ring_2nd(st)
        call check(error, last_message, "Only one keyboard macro defined.")
        if (allocated(error)) return
        ! Record another one, with format.
        call kmacro_set_format("=%d=")
        call define_macro(chars("bar"))
        ! Execute the first one; should get default format.
        call kmacro_execute_ring_2nd(value_type(), st)
        call check(error, text(), "11")
        if (allocated(error)) return
        ! Now display the 2nd ring macro and check result.
        call kmacro_view_ring_2nd(st)
        call check(error, last_message, "2nd macro: C-x C-k TAB C-u C-x C-k TAB")
        call kill_buffer(h, st)
    end subroutine test_ring_2nd_commands

    !> Macro ring can shift one way, shift the other way, swap and pop.
    subroutine test_fill_ring_and_rotate(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call fresh(h)
        kmacro_ring_max = 4
        ! Record enough macros that the first one drops off the history.
        call define_macro(chars("a"))
        call define_macro(chars("bb"))
        call define_macro(chars("ccc"))
        call define_macro(chars("dddd"))
        call define_macro(chars("eeeee"))
        ! Cycle the ring and check that #2 comes up.
        call kmacro_cycle_ring_next(st)
        call check(error, last_message, "Macro: bb")
        if (allocated(error)) return
        ! Execute the current macro and check arguments.
        call kmacro_execute_macro(value_type(2_int64), st, KMACRO_REPEAT_NO)
        call check(error, text(), "bbbb")
        if (allocated(error)) return
        ! Cycle the ring the other way; #5 expected.
        call kmacro_cycle_ring_previous(st)
        call check(error, last_message, "Macro: 5*e")
        if (allocated(error)) return
        ! Swapping the top two should give #4.
        call kmacro_swap_ring(st)
        call check(error, last_message, "Macro: 4*d")
        if (allocated(error)) return
        ! Delete the top and expect #5.
        call kmacro_delete_ring_head(st)
        call check(error, last_message, "Macro: 5*e")
        call kill_buffer(h, st)
    end subroutine test_fill_ring_and_rotate

    !> Ring commands give appropriate message when no macros exist.
    subroutine test_ring_no_macros(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        integer :: k

        call fresh(h)

        do k = 1, 7
            last_message = ""

            select case (k)
            case (1)
                call kmacro_cycle_ring_next(st)
            case (2)
                call kmacro_cycle_ring_previous(st)
            case (3)
                call kmacro_swap_ring(st)
            case (4)
                call kmacro_delete_ring_head(st)
            case (5)
                call kmacro_view_ring_2nd(st)
            case (6)
                call kmacro_execute_ring_2nd(value_type(), st)
            case default
                ! kmacro-view-macro says so, then that there are none.
                call kmacro_view_macro(st)
                call check(error, last_message, "No keyboard macros defined")
                exit
            end select

            call check(error, last_message, "No keyboard macro defined.")
            if (allocated(error)) return
        end do

        call kill_buffer(h, st)
    end subroutine test_ring_no_macros

    !> Kmacro view cycles through ring and executes macro just viewed.
    subroutine test_repeat_view_and_execute(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call fresh(h)
        kmacro_execute_repeat_key = KMACRO_EXECUTE_REPEAT_LAST_KEY
        call define_macro(chars("a"))
        call define_macro(chars("b"))
        call define_macro(chars("c"))
        call define_macro(chars("d"))
        last_command = value_type()
        ! 6 views (the first plus 5 repeats) should cycle through the ring
        ! and get to the second-to-last macro defined; C-k runs it.
        call type_events(kbd("C-x C-k C-v C-v C-v C-v C-v C-v C-k"))
        call check(error, text(), "c")
        if (allocated(error)) return
        ! (Reading C-k wiped the echo area; the views were logged.)
        call check(error, index(messages_text(), "2nd previous macro: c"//achar(10)) > 0, "view messages")
        call kill_buffer(h, st)
    end subroutine test_repeat_view_and_execute

    !> The text of *Messages*.
    function messages_text() result(t)
        use buffer_fm, only: get_buffer, is_buffer_live, set_buffer, current_buffer
        use editfns_fm, only: buffer_string
        character(len=:), allocatable :: t
        type(handle_type) :: old
        type(status_type) :: st

        old = current_buffer()
        t = ""
        if (.not. is_buffer_live(get_buffer("*Messages*"))) return
        call set_buffer(get_buffer("*Messages*"), st)
        t = buffer_string()
        call set_buffer(old, st)
    end function messages_text

    ! ------------------------------------------------------------------
    ! Own tests
    ! ------------------------------------------------------------------

    !> C-x ( records the keys typed until C-x ), C-x e calls the macro
    !> and e repeats it.
    subroutine test_recording(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call fresh(h)
        kmacro_execute_repeat_key = KMACRO_EXECUTE_REPEAT_LAST_KEY
        call type_events(kbd("C-x ( a b C-x )"))
        call check(error, last_macro_is(chars("ab")), "macro recorded")
        if (allocated(error)) return
        call check(error, last_message, "Keyboard macro defined")
        if (allocated(error)) return
        call check(error, defining_kbd_macro == DEFINING_KBD_MACRO_NO, "no longer defining")
        if (allocated(error)) return
        call type_events(kbd("C-x e"))
        call check(error, text(), "abab")
        if (allocated(error)) return
        call check(error, last_message, "(Type e to repeat macro)")
        if (allocated(error)) return
        ! Typed keys come after the macro's events (unread events would
        ! come first).
        call type_bytes("ee")
        call check(error, text(), "abababab")
        if (allocated(error)) return
        ! Any other key ends the repetition.
        call type_bytes("exe")
        call check(error, text(), "abababababxe")
        call kill_buffer(h, st)
    end subroutine test_recording

    !> The keys of a prefix argument are recorded with the command they
    !> are for.
    subroutine test_recording_prefix(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call fresh(h)
        call type_events(kbd("C-x ( C-u 3 a C-x )"))
        call check(error, last_macro_is(kbd("C-u 3 a")), "C-u 3 a")
        if (allocated(error)) return
        call type_events(kbd("C-x e"))
        call check(error, text(), "aaaaaa")
        call kill_buffer(h, st)
    end subroutine test_recording_prefix

    !> C-u 3 C-x ) ends the macro and runs it twice more; the keys
    !> ending it are not part of it.
    subroutine test_end_with_count(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call fresh(h)
        call type_events(kbd("C-x ( x C-u 3 C-x )"))
        call check(error, last_macro_is(chars("x")), "macro x")
        if (allocated(error)) return
        call check(error, text(), "xxx")
        call kill_buffer(h, st)
    end subroutine test_end_with_count

    !> C-g ends the definition and brings the previous macro back.
    subroutine test_quit_cancels(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call fresh(h)
        call type_events(kbd("C-x ( a C-x )"))
        call type_events(kbd("C-x ( b C-g"))
        call check(error, defining_kbd_macro == DEFINING_KBD_MACRO_NO, "not defining")
        if (allocated(error)) return
        call check(error, last_macro_is(chars("a")), "previous macro")
        if (allocated(error)) return
        call check(error,.not. allocated(kmacro_ring) .or. size(kmacro_ring) == 0, "ring popped")
        if (allocated(error)) return
        call check(error, last_message, "Quit")
        call kill_buffer(h, st)
    end subroutine test_quit_cancels

    !> An error ends the definition, without the keys of the failing
    !> command.
    subroutine test_error_cancels(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call fresh(h)
        call type_events(kbd("C-x ( b C-a C-b"))
        call check(error, defining_kbd_macro == DEFINING_KBD_MACRO_NO, "not defining")
        if (allocated(error)) return
        call check(error, last_macro_is(kbd("b C-a")), "macro b C-a")
        if (allocated(error)) return
        call check(error, last_message, "Beginning of buffer")
        call kill_buffer(h, st)
    end subroutine test_error_cancels

    !> The example of kmacro.el's commentary: M-5 F3 x M-2 F3 y F4 F4 F4
    !> F4 inserts x5yx7yx9yx11y.
    subroutine test_f3_f4(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call fresh(h)
        kmacro_execute_repeat_key = KMACRO_EXECUTE_REPEAT_LAST_KEY
        call type_events(kbd("M-5 <f3> x M-2 <f3> y <f4> <f4> <f4> <f4>"))
        call check(error, text(), "x5yx7yx9yx11y")
        if (allocated(error)) return
        call check(error, last_macro_is(kbd("x M-2 <f3> y")), "macro")
        call kill_buffer(h, st)
    end subroutine test_f3_f4

    subroutine count_termination(status)
        type(status_type), intent(out) :: status

        status = status_type()
        terminations = terminations + 1
    end subroutine count_termination

    !> kbd_macro_termination_hook runs after each execution, also one
    !> ended by an error.
    subroutine test_termination_hook(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call fresh(h)
        terminations = 0
        call kbd_macro_termination_hook%add(count_termination)
        call define_macro(chars("a"))
        call kmacro_execute_macro(value_type(2_int64), st)
        call define_macro(kbd("C-a C-b"))
        call kmacro_execute_macro(value_type(), st)
        call kbd_macro_termination_hook%remove(count_termination)
        call check(error, terminations, 2)
        if (allocated(error)) return
        call check(error, format_error(st), "Beginning of buffer")
        call kill_buffer(h, st)
    end subroutine test_termination_hook

    !> " Def" in the font_lock_warning_face shows in the mode line while
    !> a macro is being defined.
    subroutine test_mode_line(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        character(len=:), allocatable :: modes
        type(face_span_type), allocatable :: spans(:)
        character(len=64), allocatable :: faces(:)

        call fresh(h)
        call mode_line_modes_faces(h%index, modes, spans, faces)
        call check(error, modes, "Fundamental")
        if (allocated(error)) return
        call kmacro_start_macro(value_type(), st)
        call mode_line_modes_faces(h%index, modes, spans, faces)
        call check(error, modes, "Fundamental Def")
        if (allocated(error)) return
        call check(error, size(spans), 1)
        if (allocated(error)) return
        call check(error, spans(1)%first == 12 .and. spans(1)%last == 15, "span")
        if (allocated(error)) return
        call check(error, trim(faces(1)), "font_lock_warning_face")
        call discard_input()
        call kill_buffer(h, st)
    end subroutine test_mode_line

    !> The printed form of macros (edmacro.el's format-kbd-macro).
    subroutine test_format_kbd_macro(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call fresh(h)
        call check(error, format_kbd_macro(kbd("C-x C-f")), "C-x C-f")
        if (allocated(error)) return
        call check(error, format_kbd_macro(chars("abc")), "abc")
        if (allocated(error)) return
        call check(error, format_kbd_macro(kbd("C-f C-f C-f")), "3*C-f")
        if (allocated(error)) return
        call check(error, format_kbd_macro(kbd("C-u 3 C-f")), "C-u 3 C-f")
        if (allocated(error)) return
        call check(error, format_kbd_macro(kbd("M-1 M-2 a")), "M-12 a")
        if (allocated(error)) return
        call check(error, format_kbd_macro(kbd("M-x f o r w a r d _ c h a r RET")), "<<forward_char>>")
        if (allocated(error)) return
        call check(error, format_kbd_macro(kbd("a a a b <f3> SPC")), "aaab <f3> SPC")
        if (allocated(error)) return
        call check(error, format_kbd_macro(kbd("C-M-a ESC DEL")), "M-C-a ESC DEL")
        call kill_buffer(h, st)
    end subroutine test_format_kbd_macro

    !> The keys of kmacro.el's autoloads and kmacro-keymap.
    subroutine test_keys(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        character(len=*), parameter :: keys(14) = [character(len=14) :: "C-x (", "C-x )", "C-x e", "<f3>", "<f4>", &
                                                                       "C-x C-k C-k", "C-x C-k C-n", "C-x C-k C-p", "C-x C-k C-v", &
                                                                       "C-x C-k C-i", "C-x C-k C-c", "C-x C-k C-a", "C-x C-k C-f", &
                                                                                                                   "C-x C-k C-d"]
        character(len=*), parameter :: cmds(14) = [character(len=36) :: "kmacro_start_macro", "kmacro_end_macro", &
                                                           "kmacro_end_and_execute_macro", "kmacro_start_macro_or_insert_counter", &
                                                              "kmacro_end_or_execute_macro", "kmacro_end_or_execute_macro_repeat", &
                                                                           "kmacro_cycle_ring_next", "kmacro_cycle_ring_previous", &
                                                                              "kmacro_view_macro_repeat", "kmacro_insert_counter", &
                                                                  "kmacro_set_counter", "kmacro_add_counter", "kmacro_set_format", &
                                                                                                          "kmacro_delete_ring_head"]
        type(value_type) :: b
        integer :: k

        call fresh(h)

        do k = 1, size(keys)
            call key_binding(kbd(trim(keys(k))), b, st)
            call check(error, value_eq(b, value_type(name=trim(cmds(k)))), trim(keys(k)))
            if (allocated(error)) return
        end do

        call kill_buffer(h, st)
    end subroutine test_keys
end module test_kmacro_fm
