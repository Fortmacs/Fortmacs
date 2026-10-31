! test_minibuf_fm.f90 -- tests for the minibuffer (minibuf_fm,
! minibuffer_fm, minibuffer history in simple_fm, yes-or-no-p, fields).
! Ports GNU Emacs 31.1 test/src/minibuf-tests.el in part; only the
! completion tests on lists and alists of strings and symbols.
!
! Copyright (C) 2016-2026 Free Software Foundation, Inc.
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

!> Ports the minibuf-tests.el completion tests for lists and alists of
!> strings and symbols (not obarrays of their own, hash tables or
!> completion-regexp-list); the rest are own tests.
module test_minibuf_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use test_support, only: show_alone
    use error_fm, only: status_type, format_error, FM_QUIT, FM_USER_ERROR, FM_VOID_VARIABLE, FM_INHIBITED_INTERACTION
    use minibuf_fm, only: completion_regexp_list, minibuffer_depth, minibuffer_prompt, minibuffer_exit_hook, &
                          minibuffer_setup_function, minibufferp, assoc_string
    use keyboard_fm, only: inhibit_interaction
    use lisp_fm, only: list, vtag, V_LIST
    use handle_fm, only: handle_type
    use lisp_fm, only: value_type, make_fixnum, nil, list_length, list_elt, xstring, make_string, make_list, &
                       list, list_elements, stringp, nilp, t_value
    use data_fm, only: value_eq
    use fns_fm, only: value_equal
    use lread_fm, only: sym
    use data_fm, only: set, symbol_value
    use buffer_fm, only: buffers, cur, get_buffer_create, set_buffer, current_buffer, kill_buffer, get_buffer, &
                         erase_buffer, fill_column
    use insdel_fm, only: insert
    use editfns_fm, only: buffer_string, goto_char, point
    use keymap_fm, only: key_parse, define_key
    use callint_fm, only: command_type
    use keyboard_fm, only: unread_command_events, discard_input, last_message, command_loop, minibuffer_message_text, &
                           ring_bell_hook
    use keymap_fm, only: current_global_map
    use minibuffer_fm, only: completing_read, completion_auto_help, read_file_name_internal, REQUIRE_MATCH_T, &
                             last_minibuffer_message
    use dired_fm, only: file_name_completion, file_name_all_completions
    use frame_fm, only: selected_frame, frame_selected_window_ref
    use window_fm, only: window_type, get_buffer_window, window_leaves, same_window
    use bindings_fm, only: init_bindings
    use minibuf_fm, only: minibuf_level, read_from_minibuffer, read_string, minibuffer_local_map, &
                          completion_table_type, completion_list_table, completion_obarray_table, &
                          try_completion, all_completions, test_completion, completion_ignore_case, &
                          completion_function_table, minibuffer_contents
    use minibuffer_fm, only: read_file_name
    use fns_fm, only: yes_or_no_p, use_short_answers
    use subr_fm, only: add_to_history, history_length, history_delete_duplicates, y_or_n_p, y_or_n_p_use_read_key, &
                       read_number
    use fns_fm, only: put
    use minibuf_fm, only: COMPLETE_TRY, COMPLETE_ALL, COMPLETE_TEST
    use minibuffer_fm, only: read_buffer, internal_complete_buffer, insert_default_directory, &
                             read_buffer_completion_ignore_case
    use fileio_fm, only: init_fileio, emacs_wd, file_name_as_directory, write_region, &
                         make_directory_internal, delete_file_internal, file_exists_p
    use files_fm, only: set_visited_file_name, write_file
    implicit none(type, external)
    private

    public :: collect_minibuf_fm

    type(command_type), target :: cmd_test_nested_read, cmd_test_leave_and_read
    type(command_type), target :: cmd_test_record_completion
    !> What the minibuffer-setup and minibuffer-exit functions saw.
    integer :: seen_depth = -1
    character(len=:), allocatable :: seen_prompt
    logical :: exit_hook_ran = .false.

    !> What the nested-minibuffer command saw.
    type(status_type) :: nested_status

    !> The collection minibuf-tests--memq-of-collection tests membership in.
    type(completion_table_type) :: member_of

    !> What <f9> recorded: minibuffer messages, contents and whether
    !> *Completions* was visible.
    type :: record_type
        character(len=:), allocatable :: s, contents
    end type record_type

    type(record_type) :: recorded(10)
    logical :: recorded_visible(10) = .false.
    integer(int64) :: recorded_point(10) = 0
    integer :: nrecorded = 0

    !> Keys the user types once the bell rings (after an error, which
    !> discards the input typed ahead).
    character(len=:), allocatable :: keys_after_bell

contains

    !> Collect all exported unit tests.
    subroutine collect_minibuf_fm(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("abort-with-C-g", test_abort), &
                    new_unittest("read-from-minibuffer", test_read), &
                    new_unittest("prompt-is-protected", test_prompt), &
                    new_unittest("history", test_history), &
                    new_unittest("future-history-default", test_default), &
                    new_unittest("line-or-history-element", test_line_or_history), &
                    new_unittest("add-to-history", test_add_to_history), &
                    new_unittest("no-recursive-minibuffer", test_no_recursion), &
                    new_unittest("yes-or-no-p", test_yes_or_no), &
                    new_unittest("read-number", test_read_number), &
                    new_unittest("y-or-n-p-minibuffer", test_y_or_n_p_minibuffer), &
                    new_unittest("y-or-n-p-read-key", test_y_or_n_p_read_key), &
                    new_unittest("read-file-name", test_read_file_name), &
                    new_unittest("read-file-name-history", test_read_file_name_history), &
                    new_unittest("read-file-name-directory", test_read_file_name_directory), &
                    new_unittest("file-name-table-substitutes", test_file_name_table_sifn), &
                    new_unittest("read-buffer-ignore-case", test_read_buffer_ignore_case), &
                    new_unittest("internal-complete-buffer", test_internal_complete_buffer), &
                    new_unittest("minibuffer-from-other-window", test_minibuffer_other_window), &
                    new_unittest("find-file-command", test_find_file_command), &
                    new_unittest("write-file", test_write_file), &
                    new_unittest("try-completion-string-list", test_try_string_list), &
                    new_unittest("try-completion-string-list-predicate", test_try_string_list_pred), &
                    new_unittest("try-completion-symbol-list", test_try_symbol_list), &
                    new_unittest("try-completion-symbol-list-predicate", test_try_symbol_list_pred), &
                    new_unittest("try-completion-symbol-alist", test_try_symbol_alist), &
                    new_unittest("try-completion-symbol-alist-predicate", test_try_symbol_alist_pred), &
                    new_unittest("try-completion-string-alist", test_try_string_alist), &
                    new_unittest("try-completion-string-alist-predicate", test_try_string_alist_pred), &
                    new_unittest("all-completions-string-list", test_all_string_list), &
                    new_unittest("all-completions-string-list-predicate", test_all_string_list_pred), &
                    new_unittest("all-completions-symbol-list", test_all_symbol_list), &
                    new_unittest("all-completions-symbol-list-predicate", test_all_symbol_list_pred), &
                    new_unittest("all-completions-symbol-alist", test_all_symbol_alist), &
                    new_unittest("all-completions-symbol-alist-predicate", test_all_symbol_alist_pred), &
                    new_unittest("all-completions-string-alist", test_all_string_alist), &
                    new_unittest("all-completions-string-alist-predicate", test_all_string_alist_pred), &
                    new_unittest("test-completion-string-list", test_test_string_list), &
                    new_unittest("test-completion-string-list-predicate", test_test_string_list_pred), &
                    new_unittest("test-completion-symbol-list", test_test_symbol_list), &
                    new_unittest("test-completion-symbol-list-predicate", test_test_symbol_list_pred), &
                    new_unittest("test-completion-symbol-alist", test_test_symbol_alist), &
                    new_unittest("test-completion-symbol-alist-predicate", test_test_symbol_alist_pred), &
                    new_unittest("test-completion-string-alist", test_test_string_alist), &
                    new_unittest("test-completion-string-alist-predicate", test_test_string_alist_pred), &
                    new_unittest("try-completion-string-list-completion-regexp", test_try_regexp_1), &
                    new_unittest("all-completions-string-list-completion-regexp", test_all_regexp_1), &
                    new_unittest("test-completion-string-list-completion-regexp", test_test_regexp_1), &
                    new_unittest("try-completion-symbol-list-completion-regexp", test_try_regexp_2), &
                    new_unittest("all-completions-symbol-list-completion-regexp", test_all_regexp_2), &
                    new_unittest("test-completion-symbol-list-completion-regexp", test_test_regexp_2), &
                    new_unittest("try-completion-symbol-alist-completion-regexp", test_try_regexp_3), &
                    new_unittest("all-completions-symbol-alist-completion-regexp", test_all_regexp_3), &
                    new_unittest("test-completion-symbol-alist-completion-regexp", test_test_regexp_3), &
                    new_unittest("try-completion-string-alist-completion-regexp", test_try_regexp_4), &
                    new_unittest("all-completions-string-alist-completion-regexp", test_all_regexp_4), &
                    new_unittest("test-completion-string-alist-completion-regexp", test_test_regexp_4), &
                    new_unittest("completion-obarray", test_obarray), &
                    new_unittest("minibuf-depth-and-prompt", test_depth_prompt), &
                    new_unittest("test-inhibit-interaction", test_inhibit_interaction), &
                    new_unittest("assoc-string", test_assoc_string), &
                    new_unittest("read-variable", test_read_variable), &
                    new_unittest("test-try-completion-ignore-case", test_try_completion_ignore_case), &
                    new_unittest("completion-auto-help-test", test_completion_auto_help), &
                    new_unittest("completions-list-buffer", test_completion_list_buffer), &
                    new_unittest("minibuffer-complete-and-exit", test_complete_and_exit), &
                    new_unittest("minibuffer-complete-word", test_complete_word), &
                    new_unittest("execute-extended-command", test_execute_extended_command), &
                    new_unittest("file-name-completion", test_file_name_completion), &
                    new_unittest("fill-paragraph-in-minibuffer", test_fill_minibuffer) &
                    ]
    end subroutine collect_minibuf_fm

    !> Queue the keys `keys` (kbd syntax) when the bell next rings.
    subroutine type_after_bell(keys)
        character(len=*), intent(in) :: keys

        keys_after_bell = keys
        ring_bell_hook => bell_rung
    end subroutine type_after_bell

    subroutine bell_rung()
        ring_bell_hook => null()
        call type_keys(keys_after_bell)
    end subroutine bell_rung

    !> Queue the keys described by `keys` (kbd syntax).
    subroutine type_keys(keys)
        character(len=*), intent(in) :: keys
        type(value_type), allocatable :: evs(:)
        type(status_type) :: st

        call key_parse(keys, evs, st)
        call unread_command_events(evs)
    end subroutine type_keys

    !> Queue the characters of `text` as input.
    subroutine type_text(text)
        character(len=*), intent(in) :: text
        type(value_type), allocatable :: evs(:)
        integer :: i

        allocate (evs(len(text)))

        do i = 1, len(text)
            evs(i) = make_fixnum(int(iachar(text(i:i)), int64))
        end do

        call unread_command_events(evs)
    end subroutine type_text

    subroutine fresh_scratch()
        type(handle_type) :: buf
        type(status_type) :: st

        call init_bindings()
        call discard_input()
        call get_buffer_create("*scratch*", buf, st)
        call set_buffer(buf, st)
        last_message = ""
        call show_alone()
    end subroutine fresh_scratch

    subroutine test_read(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: answer
        type(status_type) :: st
        type(handle_type) :: cb

        call fresh_scratch()
        call type_text("hello"//achar(13))
        call read_from_minibuffer("Say: ", answer, st)
        cb = current_buffer()
        call check(error, st%ok() .and. answer == "hello", answer)
        if (allocated(error)) return
        call check(error, buffers(cur)%name == "*scratch*" .and. minibuf_level == 0 .and. &
                   .not. selected_frame%minibuffer_selected, "back where it started")
        if (allocated(error)) return
        ! Initial input, edited.
        call type_text(achar(127)//"p"//achar(10))
        call read_string("Say: ", answer, st, initial="hello")
        call check(error, answer == "hellp", answer)
        if (allocated(error)) return
        ! read-string's default for empty input.
        call type_text(achar(13))
        call read_string("Say: ", answer, st, default="dflt")
        call check(error, answer == "dflt", answer)
    end subroutine test_read

    subroutine test_abort(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        character(len=:), allocatable :: answer

        call fresh_scratch()
        call type_keys("C-x C-f a b C-g")
        call command_loop(st)
        call check(error, minibuf_level == 0 .and. .not. selected_frame%minibuffer_selected, "left the minibuffer")
        if (allocated(error)) return
        call check(error, last_message == "Quit", last_message)
        if (allocated(error)) return
        call check(error, buffers(cur)%name == "*scratch*", buffers(cur)%name)
        if (allocated(error)) return
        call type_keys("x C-g")
        call read_from_minibuffer("Say: ", answer, st)
        call check(error, st%code == FM_QUIT .and. len(answer) == 0)
    end subroutine test_abort

    !> C-a stops after the prompt; DEL there doesn't change the prompt.
    !> The prompt is not cursor-intangible (minibuffer-prompt-properties
    !> is (read-only t)): C-b moves into it, and typing there is a
    !> read-only error.
    subroutine test_prompt(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: answer
        type(status_type) :: st

        call fresh_scratch()
        ! The error (the prompt is read-only) discards the keys typed
        ! ahead.
        call type_keys("b C-a DEL C-b C-b a RET")
        call type_after_bell("RET")
        call read_from_minibuffer("Say: ", answer, st)
        call check(error, st%ok() .and. answer == "b", answer)
        if (allocated(error)) return
        call completion_setup()
        call type_keys("b C-a C-b C-b <f9> M-> a RET")
        call read_from_minibuffer("Say: ", answer, st)
        call check(error, st%ok() .and. answer == "ba", answer)
        if (allocated(error)) return
        call check(error, nrecorded == 1 .and. recorded_point(1) == 4, "point in the prompt")
    end subroutine test_prompt

    subroutine test_history(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: answer
        type(status_type) :: st

        call fresh_scratch()
        call set(sym("test-history"), nil(), st)
        call type_text("one"//achar(13))
        call read_from_minibuffer("? ", answer, st, history="test-history")
        call type_text("two"//achar(13))
        call read_from_minibuffer("? ", answer, st, history="test-history")
        call type_keys("M-p M-p RET")
        call read_from_minibuffer("? ", answer, st, history="test-history")
        call check(error, answer == "one", answer)
        if (allocated(error)) return
        ! Back to the typed input.
        call type_keys("x M-p M-n RET")
        call read_from_minibuffer("? ", answer, st, history="test-history")
        call check(error, answer == "x", answer)
        if (allocated(error)) return
        ! Past the oldest element ("Beginning of history" is reported and
        ! the input stays).
        call type_keys("<up> <up> <up> <up> RET")
        call read_from_minibuffer("? ", answer, st, history="test-history")
        call check(error, answer == "one", answer)
    end subroutine test_history

    !> <up> and <down> move between the lines of a multi-line input, and
    !> to the history past the first or last line; TAB inserts itself.
    subroutine test_line_or_history(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: answer
        type(status_type) :: st

        call fresh_scratch()
        call set(sym("test-history"), list(make_string("old")), st)
        ! Two lines: <up> goes to the first line, a second <up> to the
        ! history.
        call type_keys("a b c d e f C-q C-j x y z w <up> X RET")
        call read_from_minibuffer("? ", answer, st, history="test-history")
        call check(error, answer == "abXcdef"//achar(10)//"xyzw", answer)
        if (allocated(error)) return
        call set(sym("test-history"), list(make_string("old")), st)
        call type_keys("a C-q C-j b <up> <up> RET")
        call read_from_minibuffer("? ", answer, st, history="test-history")
        call check(error, answer == "old", answer)
        if (allocated(error)) return
        ! Back down to the input.
        call type_keys("x <up> <down> RET")
        call read_from_minibuffer("? ", answer, st, history="test-history")
        call check(error, answer == "x", answer)
        if (allocated(error)) return
        call type_keys("a TAB b RET")
        call read_from_minibuffer("? ", answer, st)
        call check(error, answer == "a"//achar(9)//"b", answer)
    end subroutine test_line_or_history

    subroutine test_default(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: answer
        type(status_type) :: st

        call fresh_scratch()
        call type_keys("M-n RET")
        call read_from_minibuffer("? ", answer, st, default="future")
        call check(error, answer == "future", answer)
        if (allocated(error)) return
        call type_keys("a M-n x RET")
        call type_after_bell("RET")
        call read_from_minibuffer("? ", answer, st, history="empty-history")
        call check(error, answer == "a", answer)
    end subroutine test_default

    subroutine test_add_to_history(error)
        type(error_type), allocatable, intent(out) :: error
        type(value_type) :: h
        type(status_type) :: st
        integer :: i, saved
        character(len=8) :: num

        call set(sym("h-test"), nil(), st)
        call add_to_history("h-test", "a")
        call add_to_history("h-test", "a")
        call add_to_history("h-test", "b")
        call symbol_value(sym("h-test"), h, st)
        call check(error, list_length(h) == 2 .and. xstring(list_elt(h, 1_int64)) == "b")
        if (allocated(error)) return
        saved = history_length
        history_length = 3

        do i = 1, 5
            write (num, "(i0)") i
            call add_to_history("h-test", trim(num))
        end do

        history_length = saved
        call symbol_value(sym("h-test"), h, st)
        call check(error, list_length(h) == 3 .and. xstring(list_elt(h, 1_int64)) == "5")
        if (allocated(error)) return
        ! history-delete-duplicates
        call set(sym("h-test"), nil(), st)
        history_delete_duplicates = .true.
        call add_to_history("h-test", "a")
        call add_to_history("h-test", "b")
        call add_to_history("h-test", "a")
        history_delete_duplicates = .false.
        call symbol_value(sym("h-test"), h, st)
        call check(error, list_length(h) == 2 .and. xstring(list_elt(h, 1_int64)) == "a" .and. &
                   xstring(list_elt(h, 2_int64)) == "b", "duplicates deleted")
        if (allocated(error)) return
        ! The history_length property.
        call put(sym("h-test"), sym("history_length"), make_fixnum(1_int64), st)
        call add_to_history("h-test", "c")
        call symbol_value(sym("h-test"), h, st)
        call check(error, list_length(h) == 1 .and. xstring(list_elt(h, 1_int64)) == "c", "history_length property")
        if (allocated(error)) return
        call put(sym("h-test"), sym("history_length"), t_value(), st)
        call add_to_history("h-test", "d")
        call add_to_history("h-test", "e")
        call symbol_value(sym("h-test"), h, st)
        call check(error, list_length(h) == 3, "no truncation")
        if (allocated(error)) return
        ! A void history variable.
        call add_to_history("h-test-void-variable", "x", status=st)
        call check(error, st%code == FM_VOID_VARIABLE, "void-variable")
    end subroutine test_add_to_history

    subroutine nested_read(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: answer

        call read_from_minibuffer("Inner: ", answer, nested_status)
        status = status_type()
    end subroutine nested_read

    subroutine test_no_recursion(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: answer
        type(value_type), allocatable :: key(:)
        type(status_type) :: st

        call fresh_scratch()
        cmd_test_nested_read = command_type("test-nested-read", nested_read)
        call cmd_test_nested_read%define()
        call key_parse("C-t", key, st)
        call define_key(minibuffer_local_map, key, sym("test-nested-read"), st)
        call type_keys("C-t RET")
        call read_from_minibuffer("Outer: ", answer, st)
        call check(error, st%ok() .and. nested_status%code == FM_USER_ERROR .and. &
                   format_error(nested_status) == "Command attempted to use minibuffer while in minibuffer", &
                   format_error(nested_status))
    end subroutine test_no_recursion

    subroutine test_yes_or_no(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        logical :: answer

        call fresh_scratch()
        call type_text("yes"//achar(13))
        call yes_or_no_p("Really?", answer, st)
        call check(error, st%ok() .and. answer)
        if (allocated(error)) return
        call type_text("NO"//achar(13))
        call yes_or_no_p("Really? ", answer, st)
        call check(error, st%ok() .and. .not. answer)
        if (allocated(error)) return
        call type_keys("C-g")
        call yes_or_no_p("Really? ", answer, st)
        call check(error, st%code == FM_QUIT)
        if (allocated(error)) return
        ! use-short-answers
        use_short_answers = .true.
        call type_keys("y")
        call yes_or_no_p("Really?", answer, st)
        use_short_answers = .false.
        call check(error, st%ok() .and. answer .and. last_message == "Really? (y or n) y", last_message)
    end subroutine test_yes_or_no

    !> y-or-n-p reads its answer in the minibuffer: other keys ask again
    !> with "Please answer y or n", RET exits with "no", C-g quits.
    subroutine test_y_or_n_p_minibuffer(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        logical :: answer

        call fresh_scratch()
        last_minibuffer_message = ""
        ! Another key: the answer is asked for again (and the input typed
        ! after it is discarded, so here input ends).
        call type_keys("x y")
        call y_or_n_p("Go?", answer, st)
        call check(error, st%code == FM_QUIT, "input discarded")
        if (allocated(error)) return
        call check(error, last_minibuffer_message == " [Please answer y or n]", last_minibuffer_message)
        if (allocated(error)) return
        call type_keys("y")
        call y_or_n_p("Go?", answer, st)
        call check(error, st%ok() .and. answer .and. last_message == "Go? (y or n) y", last_message)
        if (allocated(error)) return
        call type_keys("RET")
        call y_or_n_p("Go?", answer, st)
        call check(error, st%ok() .and. .not. answer, "RET is no")
        if (allocated(error)) return
        call type_keys("SPC")
        call y_or_n_p("Go?", answer, st)
        call check(error, st%ok() .and. answer, "SPC is yes")
        if (allocated(error)) return
        call type_keys("C-g")
        call y_or_n_p("Go?", answer, st)
        call check(error, st%code == FM_QUIT, "C-g quits")
        if (allocated(error)) return
        call check(error, minibuf_level == 0 .and. buffers(cur)%name == "*scratch*", "back")
    end subroutine test_y_or_n_p_minibuffer

    !> y-or-n-p-use-read-key: one key in the echo area; ESC quits.
    subroutine test_y_or_n_p_read_key(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        logical :: answer

        call fresh_scratch()
        y_or_n_p_use_read_key = .true.
        call type_keys("x n")
        call y_or_n_p("Go?", answer, st)
        call check(error, st%code == FM_QUIT .and. last_message == "Please answer y or n.  Go? (y or n) ", &
                   last_message)

        if (allocated(error)) then
            y_or_n_p_use_read_key = .false.
            return
        end if

        call type_keys("n")
        call y_or_n_p("Go?", answer, st)
        call check(error, st%ok() .and. .not. answer .and. last_message == "Go? (y or n) n", last_message)

        if (allocated(error)) then
            y_or_n_p_use_read_key = .false.
            return
        end if

        call type_keys("ESC")
        call y_or_n_p("Go?", answer, st)
        y_or_n_p_use_read_key = .false.
        call check(error, st%code == FM_QUIT, "ESC quits")
    end subroutine test_y_or_n_p_read_key

    !> read-number reads the answer as the Lisp reader does.
    subroutine test_read_number(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: n

        call fresh_scratch()
        call type_text("#x10"//achar(13))
        call read_number("N: ", n, st)
        call check(error, st%ok() .and. n == 16, "#x10")
        if (allocated(error)) return
        call type_text("?a"//achar(13))
        call read_number("N: ", n, st)
        call check(error, st%ok() .and. n == 97, "?a")
        if (allocated(error)) return
        call type_text("-12."//achar(13))
        call read_number("N: ", n, st)
        call check(error, st%ok() .and. n == -12, "-12.")
        if (allocated(error)) return
        call type_text("  5 foo"//achar(13))
        call read_number("N: ", n, st)
        call check(error, st%ok() .and. n == 5, "5 foo")
        if (allocated(error)) return
        call type_text("#3r12"//achar(13))
        call read_number("N: ", n, st)
        call check(error, st%ok() .and. n == 5, "#3r12")
        if (allocated(error)) return
        call type_text("abc"//achar(13)//"7"//achar(13))
        call read_number("N: ", n, st)
        call check(error, st%ok() .and. n == 7, "asked again")
        if (allocated(error)) return
        ! A list of defaults: the first for empty input, M-n offers all.
        call type_keys("RET")
        call read_number("N: ", n, st, defaults=[3_int64, 4_int64])
        call check(error, st%ok() .and. n == 3, "first default")
        if (allocated(error)) return
        call type_keys("M-n M-n RET")
        call read_number("N: ", n, st, defaults=[3_int64, 4_int64])
        call check(error, st%ok() .and. n == 4, "second default")
    end subroutine test_read_number

    subroutine test_read_file_name(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: name
        type(status_type) :: st

        call fresh_scratch()
        buffers(cur)%directory = "/x/y/"
        ! Unchanged input gives the default.
        call type_keys("RET")
        call read_file_name("File: ", name, st, default_filename="/x/y/z")
        call check(error, name == "/x/y/z", name)
        if (allocated(error)) return
        ! Typing an absolute name after the directory means that name.
        call type_text("/etc/hosts"//achar(13))
        call read_file_name("File: ", name, st)
        call check(error, name == "/etc/hosts", name)
        if (allocated(error)) return
        call type_text("a.txt"//achar(13))
        call read_file_name("File: ", name, st)
        call check(error, name == "/x/y/a.txt", name)
    end subroutine test_read_file_name

    !> The history gets what read_file_name returns; unchanged input is
    !> the default, empty input gives "".
    subroutine test_read_file_name_history(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: name
        type(status_type) :: st
        type(value_type) :: h

        call fresh_scratch()
        buffers(cur)%directory = "/x/y/"
        call set(sym("file_name_history"), nil(), st)
        call type_keys("RET")
        call read_file_name("File: ", name, st, default_filename="/x/y/z")
        call symbol_value(sym("file_name_history"), h, st)
        call check(error, name == "/x/y/z" .and. xstring(list_elt(h, 1_int64)) == "/x/y/z", name)
        if (allocated(error)) return
        ! Typed after the directory: the substituted name.
        call type_text("~/f"//achar(13))
        call read_file_name("File: ", name, st, default_filename="/x/y/z")
        call symbol_value(sym("file_name_history"), h, st)
        call check(error, name == "~/f" .and. list_length(h) == 2 .and. xstring(list_elt(h, 1_int64)) == "~/f", &
                   xstring(list_elt(h, 1_int64)))
        if (allocated(error)) return
        ! The same name again is a repeat: no new element.
        call type_text("~/f"//achar(13))
        call read_file_name("File: ", name, st, default_filename="/x/y/z")
        call symbol_value(sym("file_name_history"), h, st)
        call check(error, name == "~/f" .and. list_length(h) == 2, "repeat")
        if (allocated(error)) return
        ! Erasing the input gives "".
        call type_keys("C-a C-k RET")
        call read_file_name("File: ", name, st, default_filename="/x/y/z")
        call check(error, st%ok() .and. len(name) == 0, name)
    end subroutine test_read_file_name_history

    !> The minibuffer's default directory is DIR: relative names complete
    !> there.
    subroutine test_read_file_name_directory(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: name, dir
        type(status_type) :: st
        type(handle_type) :: buf

        call fresh_scratch()
        call init_fileio()
        dir = file_name_as_directory(emacs_wd)//"build/test-rfn-dir/"
        call make_directory_internal(dir, st)
        call get_buffer_create(" rfn-source", buf, st)
        call set_buffer(buf, st)
        call insert("data", st)
        call write_region(dir//"only-file.txt", .false., st)
        call kill_buffer(buf, st)
        call fresh_scratch()
        buffers(cur)%directory = "/"
        insert_default_directory = .false.
        call type_keys("o n TAB RET")
        call read_file_name("File: ", name, st, dir=dir, mustmatch=REQUIRE_MATCH_T)
        insert_default_directory = .true.
        call check(error, st%ok() .and. name == "only-file.txt", name)
        if (allocated(error)) return
        call check(error, buffers(cur)%directory == "/", "the caller's directory is unchanged")
    end subroutine test_read_file_name_directory

    !> read-file-name-internal completes the name substitute-in-file-name
    !> makes of the input, keeping the input's text.
    subroutine test_file_name_table_sifn(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: dir
        type(value_type) :: result
        type(status_type) :: st

        call fresh_scratch()
        call init_fileio()
        dir = file_name_as_directory(emacs_wd)//"build/test-rfn-dir/"
        call read_file_name_internal("/nonexistent/"//dir//"only-f", COMPLETE_TRY, result, st)
        call check(error, stringp(result), "a completion")
        if (allocated(error)) return
        call check(error, xstring(result) == "/nonexistent/"//dir//"only-file.txt", xstring(result))
        if (allocated(error)) return
        call read_file_name_internal("/nonexistent/"//dir//"only-f", COMPLETE_ALL, result, st)
        call check(error, list_length(result) == 1, "all completions")
        if (allocated(error)) return
        call check(error, xstring(list_elt(result, 1_int64)) == "only-file.txt", xstring(list_elt(result, 1_int64)))
    end subroutine test_file_name_table_sifn

    !> read-buffer-completion-ignore-case.
    subroutine test_read_buffer_ignore_case(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: name
        type(status_type) :: st
        type(handle_type) :: buf

        call fresh_scratch()
        call get_buffer_create("Zebra-Buffer", buf, st)
        read_buffer_completion_ignore_case = .true.
        call type_keys("z e b TAB RET")
        call read_buffer("Buffer: ", name, st)
        read_buffer_completion_ignore_case = .false.
        call kill_buffer(buf, st)
        call check(error, st%ok() .and. name == "Zebra-Buffer", name)
        if (allocated(error)) return
        call check(error,.not. completion_ignore_case%value, "completion-ignore-case restored")
    end subroutine test_read_buffer_ignore_case

    !> all-completions of "" leaves out internal buffers (names starting
    !> with a space), unless asked for them.
    subroutine test_internal_complete_buffer(error)
        type(error_type), allocatable, intent(out) :: error
        type(value_type) :: result
        type(value_type), allocatable :: elts(:)
        type(status_type) :: st
        type(handle_type) :: buf
        integer :: k
        logical :: internal

        call fresh_scratch()
        call get_buffer_create(" internal-test", buf, st)
        call internal_complete_buffer("", COMPLETE_ALL, result, st)
        call list_elements(result, elts)
        internal = .false.

        do k = 1, size(elts)
            if (xstring(elts(k)) == " internal-test") internal = .true.
        end do

        call check(error, size(elts) > 0 .and. .not. internal, "internal buffers left out")
        if (allocated(error)) return
        call internal_complete_buffer(" internal-t", COMPLETE_ALL, result, st)
        call check(error, list_length(result) == 1, "asked for")
        if (allocated(error)) return
        call internal_complete_buffer(" internal-test", COMPLETE_TEST, result, st)
        call check(error,.not. nilp(result), "test-completion")
        call kill_buffer(buf, st)
    end subroutine test_internal_complete_buffer

    !> Reading from the minibuffer with another window selected cancels
    !> the active minibuffer.
    subroutine leave_and_read(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: answer

        selected_frame%minibuffer_selected = .false.
        call read_from_minibuffer("Inner: ", answer, nested_status)
        status = status_type()
    end subroutine leave_and_read

    subroutine test_minibuffer_other_window(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: answer
        type(value_type), allocatable :: key(:)
        type(status_type) :: st

        call fresh_scratch()
        cmd_test_leave_and_read = command_type("test-leave-and-read", leave_and_read)
        call cmd_test_leave_and_read%define()
        call key_parse("C-t", key, st)
        call define_key(minibuffer_local_map, key, sym("test-leave-and-read"), st)
        call type_keys("C-t x RET")
        call read_from_minibuffer("Outer: ", answer, st)
        call check(error,.not. st%ok() .and. &
                   format_error(st) == "Command attempted to use minibuffer while in minibuffer", format_error(st))
        if (allocated(error)) return
        call check(error, minibuf_level == 0, "left the minibuffer")
        call discard_input()
        call define_key(minibuffer_local_map, key, nil(), st)
    end subroutine test_minibuffer_other_window

    subroutine test_find_file_command(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        type(handle_type) :: buf
        character(len=:), allocatable :: dir

        call fresh_scratch()
        call init_fileio()
        dir = file_name_as_directory(emacs_wd)//"build/test-minibuf"
        call make_directory_internal(dir, st)
        call get_buffer_create(" f-source", buf, st)
        call set_buffer(buf, st)
        call insert("data", st)
        call write_region(dir//"/f.txt", .false., st)
        call kill_buffer(buf, st)
        call fresh_scratch()
        buffers(cur)%directory = file_name_as_directory(dir)
        call type_text(achar(24)//achar(6)//"f.txt"//achar(13))
        call command_loop(st)
        call check(error, buffers(cur)%name == "f.txt" .and. buffer_string() == "data", buffers(cur)%name)
        if (allocated(error)) return
        call kill_buffer(current_buffer(), st)
    end subroutine test_find_file_command

    subroutine test_write_file(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        type(handle_type) :: buf
        character(len=:), allocatable :: dir
        logical :: ok

        call fresh_scratch()
        call init_fileio()
        dir = file_name_as_directory(emacs_wd)//"build/test-minibuf/"
        call make_directory_internal(dir, st)
        call delete_file_internal(dir//"w1.txt", st)
        call delete_file_internal(dir//"w2.txt", st)
        call get_buffer_create("unsaved", buf, st)
        call set_buffer(buf, st)
        call insert("text", st)
        call write_file(dir//"w1.txt", .true., st)
        call check(error, st%ok() .and. file_exists_p(dir//"w1.txt"), format_error(st))
        if (allocated(error)) return
        call check(error, buffers(cur)%name == "w1.txt" .and. buffers(cur)%filename == dir//"w1.txt", &
                   buffers(cur)%name)
        if (allocated(error)) return
        ! A directory means the same file name there; overwriting asks.
        call type_text("y")
        call write_file(dir, .true., st)
        ok = st%ok()
        call check(error, ok .and. buffers(cur)%filename == dir//"w1.txt", format_error(st))
        if (allocated(error)) return
        call set_visited_file_name(dir//"w2.txt", st)
        call check(error, buffers(cur)%name == "w2.txt" .and. .not. buffers(cur)%backed_up .and. &
                   buffers(cur)%text%save_modiff < buffers(cur)%text%modiff, "renamed and modified")
        call kill_buffer(current_buffer(), st)
    end subroutine test_write_file

    ! ------------------------------------------------------------------
    ! Completion primitives (ports of minibuf-tests.el)
    ! ------------------------------------------------------------------

    !> The test strings as a collection of kind `kind`: 1 a string list, 2
    !> a symbol list, 3 a symbol alist, 4 a string alist.
    function collection(kind, strings) result(table)
        integer, intent(in) :: kind
        character(len=*), intent(in) :: strings(:)
        type(completion_table_type) :: table
        type(value_type), allocatable :: elts(:)
        integer :: k

        allocate (elts(size(strings)))

        do k = 1, size(strings)
            select case (kind)
            case (1)
                elts(k) = make_string(trim(strings(k)))
            case (2)
                elts(k) = sym(trim(strings(k)))
            case (3)
                elts(k) = list(sym(trim(strings(k))), make_fixnum(int(k, int64)))
            case default
                elts(k) = list(make_string(trim(strings(k))), make_fixnum(int(k, int64)))
            end select
        end do

        table = completion_list_table(make_list(elts))
        member_of = table
    end function collection

    !> minibuf-tests--memq-of-collection: a member of the last collection.
    logical function memq_of_collection(elt)
        type(value_type), intent(in) :: elt
        type(value_type), allocatable :: elts(:)
        integer :: k

        call list_elements(member_of%list, elts)
        memq_of_collection = .false.

        do k = 1, size(elts)
            if (value_equal(elts(k), elt)) memq_of_collection = .true.
        end do
    end function memq_of_collection

    !> #'ignore.
    logical function ignore(elt)
        type(value_type), intent(in) :: elt

        ignore = .false.
        if (nilp(elt)) ignore = .false.
    end function ignore

    logical function is_string(v, s)
        type(value_type), intent(in) :: v
        character(len=*), intent(in) :: s

        is_string = .false.
        if (stringp(v)) is_string = xstring(v) == s .and. len(xstring(v)) == len(s)
    end function is_string

    !> minibuf-tests--try-completion and -pred for collection kind `kind`.
    subroutine check_try(error, kind, pred)
        type(error_type), allocatable, intent(out) :: error
        integer, intent(in) :: kind
        logical, intent(in) :: pred
        type(completion_table_type) :: abcdef, abba
        type(value_type) :: r
        type(status_type) :: st

        abcdef = collection(kind, [character(len=4) :: "abc", "def"])

        if (pred) then
            call try_completion("a", abcdef, r, st, memq_of_collection)
        else
            call try_completion("a", abcdef, r, st)
        end if

        call check(error, is_string(r, "abc"), "a in abcdef")
        if (allocated(error)) return
        abba = collection(kind, [character(len=4) :: "abc", "abba", "def"])

        if (pred) then
            call try_completion("a", abba, r, st, memq_of_collection)
            call check(error, is_string(r, "ab"), "a in +abba")
            if (allocated(error)) return
            call try_completion("abc", abba, r, st, memq_of_collection)
            call check(error, value_eq(r, t_value()), "abc is exact")
            if (allocated(error)) return
            call try_completion("abcd", abba, r, st, memq_of_collection)
            call check(error, nilp(r), "abcd")
            if (allocated(error)) return
            call try_completion("a", abcdef, r, st, ignore)
            call check(error, nilp(r), "ignore")
            if (allocated(error)) return
            call try_completion("abc", abba, r, st, ignore)
            call check(error, nilp(r), "ignore abc")
        else
            call try_completion("a", abba, r, st)
            call check(error, is_string(r, "ab"), "a in +abba")
            if (allocated(error)) return
            call try_completion("abc", abba, r, st)
            call check(error, value_eq(r, t_value()), "abc is exact")
            if (allocated(error)) return
            call try_completion("abcd", abba, r, st)
            call check(error, nilp(r), "abcd")
        end if
    end subroutine check_try

    !> minibuf-tests--all-completions and -pred for collection kind `kind`.
    subroutine check_all(error, kind, pred)
        type(error_type), allocatable, intent(out) :: error
        integer, intent(in) :: kind
        logical, intent(in) :: pred
        type(completion_table_type) :: abcdef, abba
        type(value_type) :: r
        type(status_type) :: st

        abcdef = collection(kind, [character(len=4) :: "abc", "def"])

        if (pred) then
            call all_completions("a", abcdef, r, st, memq_of_collection)
        else
            call all_completions("a", abcdef, r, st)
        end if

        call check(error, value_equal(r, list(make_string("abc"))), "a in abcdef")
        if (allocated(error)) return
        abba = collection(kind, [character(len=4) :: "abc", "abba", "def"])

        if (pred) then
            call all_completions("a", abba, r, st, memq_of_collection)
        else
            call all_completions("a", abba, r, st)
        end if

        call check(error, value_equal(r, list(make_string("abc"), make_string("abba"))), "a in +abba")
        if (allocated(error)) return

        if (pred) then
            call all_completions("abc", abba, r, st, memq_of_collection)
        else
            call all_completions("abc", abba, r, st)
        end if

        call check(error, value_equal(r, list(make_string("abc"))), "abc")
        if (allocated(error)) return
        call all_completions("abcd", abba, r, st)
        call check(error, nilp(r), "abcd")
        if (allocated(error)) return

        if (pred) then
            call all_completions("a", abba, r, st, ignore)
            call check(error, nilp(r), "ignore")
        end if
    end subroutine check_all

    !> minibuf-tests--test-completion and -pred for collection kind `kind`.
    subroutine check_test(error, kind, pred)
        type(error_type), allocatable, intent(out) :: error
        integer, intent(in) :: kind
        logical, intent(in) :: pred
        type(completion_table_type) :: abcdef, abba
        logical :: abc, def, ab, abcd

        abcdef = collection(kind, [character(len=4) :: "abc", "def"])

        if (pred) then
            abc = test_completion("abc", abcdef, memq_of_collection)
        else
            abc = test_completion("abc", abcdef)
        end if

        abba = collection(kind, [character(len=4) :: "abc", "abba", "def"])

        if (pred) then
            def = test_completion("def", abba, memq_of_collection)
            ab = test_completion("abba", abba, memq_of_collection)
            abcd = test_completion("abcd", abba, memq_of_collection)
        else
            def = test_completion("def", abba)
            ab = test_completion("abba", abba)
            abcd = test_completion("abcd", abba)
        end if

        call check(error, abc .and. def .and. ab .and. .not. abcd)
        if (allocated(error)) return

        if (pred) then
            call check(error,.not. test_completion("abc", abcdef, ignore) .and. &
                       .not. test_completion("def", abba, ignore), "ignore")
        end if
    end subroutine check_test

    subroutine test_try_string_list(error)
        type(error_type), allocatable, intent(out) :: error

        call check_try(error, 1, .false.)
    end subroutine test_try_string_list

    subroutine test_try_string_list_pred(error)
        type(error_type), allocatable, intent(out) :: error

        call check_try(error, 1, .true.)
    end subroutine test_try_string_list_pred

    subroutine test_try_symbol_list(error)
        type(error_type), allocatable, intent(out) :: error

        call check_try(error, 2, .false.)
    end subroutine test_try_symbol_list

    subroutine test_try_symbol_list_pred(error)
        type(error_type), allocatable, intent(out) :: error

        call check_try(error, 2, .true.)
    end subroutine test_try_symbol_list_pred

    subroutine test_try_symbol_alist(error)
        type(error_type), allocatable, intent(out) :: error

        call check_try(error, 3, .false.)
    end subroutine test_try_symbol_alist

    subroutine test_try_symbol_alist_pred(error)
        type(error_type), allocatable, intent(out) :: error

        call check_try(error, 3, .true.)
    end subroutine test_try_symbol_alist_pred

    subroutine test_try_string_alist(error)
        type(error_type), allocatable, intent(out) :: error

        call check_try(error, 4, .false.)
    end subroutine test_try_string_alist

    subroutine test_try_string_alist_pred(error)
        type(error_type), allocatable, intent(out) :: error

        call check_try(error, 4, .true.)
    end subroutine test_try_string_alist_pred

    subroutine test_all_string_list(error)
        type(error_type), allocatable, intent(out) :: error

        call check_all(error, 1, .false.)
    end subroutine test_all_string_list

    subroutine test_all_string_list_pred(error)
        type(error_type), allocatable, intent(out) :: error

        call check_all(error, 1, .true.)
    end subroutine test_all_string_list_pred

    subroutine test_all_symbol_list(error)
        type(error_type), allocatable, intent(out) :: error

        call check_all(error, 2, .false.)
    end subroutine test_all_symbol_list

    subroutine test_all_symbol_list_pred(error)
        type(error_type), allocatable, intent(out) :: error

        call check_all(error, 2, .true.)
    end subroutine test_all_symbol_list_pred

    subroutine test_all_symbol_alist(error)
        type(error_type), allocatable, intent(out) :: error

        call check_all(error, 3, .false.)
    end subroutine test_all_symbol_alist

    subroutine test_all_symbol_alist_pred(error)
        type(error_type), allocatable, intent(out) :: error

        call check_all(error, 3, .true.)
    end subroutine test_all_symbol_alist_pred

    subroutine test_all_string_alist(error)
        type(error_type), allocatable, intent(out) :: error

        call check_all(error, 4, .false.)
    end subroutine test_all_string_alist

    subroutine test_all_string_alist_pred(error)
        type(error_type), allocatable, intent(out) :: error

        call check_all(error, 4, .true.)
    end subroutine test_all_string_alist_pred

    subroutine test_test_string_list(error)
        type(error_type), allocatable, intent(out) :: error

        call check_test(error, 1, .false.)
    end subroutine test_test_string_list

    subroutine test_test_string_list_pred(error)
        type(error_type), allocatable, intent(out) :: error

        call check_test(error, 1, .true.)
    end subroutine test_test_string_list_pred

    subroutine test_test_symbol_list(error)
        type(error_type), allocatable, intent(out) :: error

        call check_test(error, 2, .false.)
    end subroutine test_test_symbol_list

    subroutine test_test_symbol_list_pred(error)
        type(error_type), allocatable, intent(out) :: error

        call check_test(error, 2, .true.)
    end subroutine test_test_symbol_list_pred

    subroutine test_test_symbol_alist(error)
        type(error_type), allocatable, intent(out) :: error

        call check_test(error, 3, .false.)
    end subroutine test_test_symbol_alist

    subroutine test_test_symbol_alist_pred(error)
        type(error_type), allocatable, intent(out) :: error

        call check_test(error, 3, .true.)
    end subroutine test_test_symbol_alist_pred

    subroutine test_test_string_alist(error)
        type(error_type), allocatable, intent(out) :: error

        call check_test(error, 4, .false.)
    end subroutine test_test_string_alist

    subroutine test_test_string_alist_pred(error)
        type(error_type), allocatable, intent(out) :: error

        call check_test(error, 4, .true.)
    end subroutine test_test_string_alist_pred

    !> The global obarray as a collection (upstream's tests make their own
    !> obarrays): names with a prefix no other symbol has.
    subroutine test_obarray(error)
        type(error_type), allocatable, intent(out) :: error
        type(value_type) :: r, v
        type(status_type) :: st

        v = sym("zqx-abc")
        v = sym("zqx-abba")
        call try_completion("zqx-a", completion_obarray_table(), r, st)
        call check(error, is_string(r, "zqx-ab"), "try")
        if (allocated(error)) return
        call all_completions("zqx-ab", completion_obarray_table(), r, st)
        call check(error, list_length(r) == 2, "all")
        if (allocated(error)) return
        call check(error, test_completion("zqx-abba", completion_obarray_table()) .and. &
                   .not. test_completion("zqx-ab", completion_obarray_table()), "test")
    end subroutine test_obarray

    subroutine test_try_completion_ignore_case(error)
        type(error_type), allocatable, intent(out) :: error
        type(value_type) :: r1, r2
        type(status_type) :: st

        completion_ignore_case = .true.
        call try_completion("bar", collection(1, [character(len=6) :: "bAr", "barfoo"]), r1, st)
        call check(error, is_string(r1, "bAr"), "bAr")

        if (.not. allocated(error)) then
            call try_completion("bar", collection(1, [character(len=6) :: "bArfoo", "barbaz"]), r1, st)
            call try_completion("bar", collection(1, [character(len=6) :: "barbaz", "bArfoo"]), r2, st)
            call check(error, is_string(r1, "bar") .and. value_equal(r1, r2), "bar either order")
        end if

        if (.not. allocated(error)) then
            ! bug#11339
            call try_completion("baz", collection(1, [character(len=6) :: "baz", "bAz"]), r1, st)
            call try_completion("baz", collection(1, [character(len=6) :: "bAz", "baz"]), r2, st)
            call check(error, is_string(r1, "baz") .and. value_equal(r1, r2), "baz, and not t")
        end if

        completion_ignore_case = .false.
    end subroutine test_try_completion_ignore_case

    ! ------------------------------------------------------------------
    ! Completion in the minibuffer (minibuffer-tests.el and own)
    ! ------------------------------------------------------------------

    !> Record the minibuffer message and whether *Completions* shows.
    subroutine record_completion_state(status)
        type(status_type), intent(out) :: status

        status = status_type()
        nrecorded = nrecorded + 1
        recorded(nrecorded)%s = ""
        if (allocated(last_minibuffer_message)) recorded(nrecorded)%s = last_minibuffer_message
        last_minibuffer_message = ""
        recorded_visible(nrecorded) = showing_completions()
        recorded(nrecorded)%contents = minibuffer_contents()
        recorded_point(nrecorded) = point()
    end subroutine record_completion_state

    logical function showing_completions()
        type(handle_type) :: c, w
        type(handle_type), allocatable :: leaves(:)

        c = get_buffer("*Completions*")
        showing_completions = .false.
        if (c%index == 0) return
        w = get_buffer_window(c)
        if (w%index == 0) return
        ! A window of its own at the bottom of the frame.
        call window_leaves(selected_frame%root_window, leaves)
        showing_completions = .false.
        if (size(leaves) == 2) showing_completions = same_window(w, leaves(2)) .and. &
                                                     .not. same_window(w, selected_frame%selected_window)
    end function showing_completions

    !> Fresh scratch with <f9> recording the completion state.
    subroutine completion_setup()
        type(value_type), allocatable :: key(:)
        type(status_type) :: st

        call fresh_scratch()
        nrecorded = 0
        cmd_test_record_completion = command_type("test-record-completion", record_completion_state)
        call cmd_test_record_completion%define()
        call key_parse("<f9>", key, st)
        call define_key(current_global_map(), key, sym("test-record-completion"), st)
    end subroutine completion_setup

    function strings(items) result(v)
        character(len=*), intent(in) :: items(:)
        type(value_type) :: v
        type(value_type), allocatable :: elts(:)
        integer :: k

        allocate (elts(size(items)))

        do k = 1, size(items)
            elts(k) = make_string(trim(items(k)))
        end do

        v = make_list(elts)
    end function strings

    subroutine test_completion_auto_help(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: answer
        type(status_type) :: st
        type(value_type) :: abc

        call completion_setup()
        abc = strings([character(len=2) :: "a", "ab", "ac"])
        completion_auto_help = .false.
        call type_keys("a TAB TAB <f9> b TAB <f9> C-g")
        call completing_read("Prompt: ", completion_list_table(abc), answer, st)
        completion_auto_help = .true.
        call check(error, nrecorded == 2, "recorded")
        if (allocated(error)) return
        call check(error, recorded(1)%s == " [Complete, but not unique]" .and. .not. recorded_visible(1), &
                   recorded(1)%s)
        if (allocated(error)) return
        call check(error, recorded(2)%s == " [Sole completion]", recorded(2)%s)
        if (allocated(error)) return
        nrecorded = 0
        call type_keys("a TAB TAB <f9> b TAB <f9> C-g")
        call completing_read("Prompt: ", completion_list_table(abc), answer, st)
        call check(error, recorded_visible(1), "visible")
        if (allocated(error)) return
        call check(error, recorded(2)%s == " [Sole completion]" .and. .not. recorded_visible(2), recorded(2)%s)
        if (allocated(error)) return
        ! The window shows the buffer again after the minibuffer.
        call check(error,.not. (showing_completions()), "restored")
    end subroutine test_completion_auto_help

    subroutine test_completion_list_buffer(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: answer
        type(status_type) :: st
        type(handle_type) :: buf

        call completion_setup()
        call type_keys("b ? C-g")
        call completing_read("Prompt: ", completion_list_table(strings([character(len=6) :: "beta", "bar", "baz", &
                                                                        "foo"])), answer, st)
        buf = get_buffer("*Completions*")
        call set_buffer(buf, st)
        call check(error, buffer_string() == "3 possible completions:"//achar(10)//"bar"//achar(10)//"baz"//achar(10)//"beta", &
                   buffer_string())
    end subroutine test_completion_list_buffer

    subroutine test_complete_and_exit(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: answer
        type(status_type) :: st
        type(value_type) :: foo

        call completion_setup()
        foo = strings([character(len=6) :: "foobar", "foobaz"])
        ! TAB completes as far as possible.
        call type_keys("f TAB <f9> r RET")
        call completing_read("Prompt: ", completion_list_table(foo), answer, st, require_match=REQUIRE_MATCH_T)
        call check(error, recorded(1)%contents == "fooba" .and. answer == "foobar", answer)
        if (allocated(error)) return
        ! RET completes first when that makes the input a match.
        nrecorded = 0
        call type_keys("foobaz RET")
        call completing_read("Prompt: ", completion_list_table(foo), answer, st, require_match=REQUIRE_MATCH_T)
        call check(error, answer == "foobaz", answer)
        if (allocated(error)) return
        ! No match: RET doesn't exit.
        call type_keys("x RET <f9> C-g")
        call completing_read("Prompt: ", completion_list_table(foo), answer, st, require_match=REQUIRE_MATCH_T)
        call check(error, st%code == FM_QUIT .and. recorded(1)%s == " [No match]", recorded(1)%s)
        if (allocated(error)) return
        ! Without REQUIRE-MATCH, RET takes any input; empty gives the default.
        call type_keys("x RET")
        call completing_read("Prompt: ", completion_list_table(foo), answer, st)
        call check(error, answer == "x", answer)
        if (allocated(error)) return
        call type_keys("RET")
        call completing_read("Prompt: ", completion_list_table(foo), answer, st, default="dflt")
        call check(error, answer == "dflt", answer)
    end subroutine test_complete_and_exit

    subroutine test_complete_word(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: answer
        type(status_type) :: st
        type(value_type) :: ff

        call completion_setup()
        ff = strings([character(len=24) :: "find-file", "find-file-other-window", "find-alternate-file"])
        call type_keys("f i SPC <f9> f SPC <f9> SPC <f9> C-g")
        call completing_read("Prompt: ", completion_list_table(ff), answer, st)
        call check(error, recorded(1)%contents == "find-", recorded(1)%contents)
        if (allocated(error)) return
        call check(error, recorded(2)%contents == "find-file", recorded(2)%contents)
        if (allocated(error)) return
        ! The hyphen, and only up to the next word.
        call check(error, recorded(3)%contents == "find-file-", recorded(3)%contents)
    end subroutine test_complete_word

    subroutine test_execute_extended_command(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st

        call fresh_scratch()
        call insert("abc", st)
        call goto_char(1_int64)
        call type_keys("M-x f o r w a r d _ c TAB RET")
        call command_loop(st)
        call check(error, point() == 2, "ran forward-char")
        if (allocated(error)) return
        call check(error, last_message == "You can run the command "//char(226)//char(128)//char(152)// &
                   "forward_char"//char(226)//char(128)//char(153)//" with C-f", last_message)
        if (allocated(error)) return
        ! A prefix argument goes to the command.
        call type_keys("C-u 2 M-x f o r w a r d _ c h a r RET")
        call command_loop(st)
        call check(error, point() == 4, "C-u 2")
        if (allocated(error)) return
        ! Not a command.
        call type_keys("M-x n o - s u c h RET C-g")
        call command_loop(st)
        call check(error, last_message == "Quit", last_message)
        call erase_buffer(st)
    end subroutine test_execute_extended_command

    subroutine test_file_name_completion(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        type(handle_type) :: buf
        type(value_type) :: r
        character(len=:), allocatable :: dir

        call fresh_scratch()
        call init_fileio()
        dir = file_name_as_directory(file_name_as_directory(emacs_wd)//"build/test-complete")
        call make_directory_internal(dir, st)
        call make_directory_internal(dir//"alpine", st)
        call get_buffer_create(" c-source", buf, st)
        call set_buffer(buf, st)
        call insert("data", st)
        call write_region(dir//"alpha.txt", .false., st)
        call write_region(dir//"beta.c", .false., st)
        call write_region(dir//"beta.o", .false., st)
        call kill_buffer(buf, st)
        call fresh_scratch()
        call file_name_completion("al", dir, r, st)
        call check(error, stringp(r) .and. xstring(r) == "alp", "file_name_completion")
        if (allocated(error)) return
        ! completion-ignored-extensions.
        call file_name_completion("be", dir, r, st)
        call check(error, stringp(r) .and. xstring(r) == "beta.c", "ignored .o")
        if (allocated(error)) return
        call file_name_all_completions("alp", dir, r, st)
        call check(error, list_length(r) == 2, "all")
        if (allocated(error)) return
        ! The minibuffer table: the directory part stays.
        call try_completion(dir//"alpi", completion_function_table(read_file_name_internal), r, st)
        call check(error, stringp(r) .and. xstring(r) == dir//"alpine/", "read_file_name_internal")
        if (allocated(error)) return
        call check(error, test_completion(dir//"alpha.txt", completion_function_table(read_file_name_internal)), &
                   "exists")
        if (allocated(error)) return
        ! C-x C-f with TAB.
        buffers(cur)%directory = dir
        call type_keys("C-x C-f a l p h TAB RET")
        call command_loop(st)
        call check(error, buffers(cur)%name == "alpha.txt" .and. buffer_string() == "data", buffers(cur)%name)
        if (allocated(error)) return
        call kill_buffer(current_buffer(), st)
    end subroutine test_file_name_completion

    !> M-q in the minibuffer fills its contents without the prompt
    !> (fill.el:fill-minibuffer-function).
    subroutine test_fill_minibuffer(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: answer
        type(status_type) :: st

        call fresh_scratch()
        fill_column = 12_int64
        call type_text("aaa bbb ccc ddd"//achar(27)//"q"//achar(13))
        call read_from_minibuffer("A long prompt here: ", answer, st)
        fill_column = 70_int64
        call check(error, answer, "aaa bbb ccc"//achar(10)//"ddd")
    end subroutine test_fill_minibuffer

    !> minibuf-tests--try-completion-regexp, --all-completions-regexp and
    !> --test-completion-regexp for collection kind KIND: with
    !> completion_regexp_list (".") nothing changes, with ("X") nothing
    !> matches.
    subroutine check_regexp(error, kind, fn)
        type(error_type), allocatable, intent(out) :: error
        integer, intent(in) :: kind
        character(len=*), intent(in) :: fn
        type(completion_table_type) :: abcdef, abba
        type(value_type) :: r
        type(status_type) :: st

        abcdef = collection(kind, [character(len=4) :: "abc", "def"])
        abba = collection(kind, [character(len=4) :: "abc", "abba", "def"])
        completion_regexp_list = [character(len=4) :: "."]

        select case (fn)
        case ("try")
            call try_completion("a", abcdef, r, st)
            call check(error, is_string(r, "abc"), "a in abcdef")
            if (allocated(error)) return
            call try_completion("a", abba, r, st)
            call check(error, is_string(r, "ab"), "a in +abba")
            if (allocated(error)) return
            call try_completion("abc", abba, r, st)
            call check(error, value_eq(r, t_value()), "abc is exact")
            if (allocated(error)) return
            call try_completion("abcd", abba, r, st)
            call check(error, nilp(r), "abcd")
        case ("all")
            call all_completions("a", abcdef, r, st)
            call check(error, list_length(r) == 1, "a in abcdef")
            if (allocated(error)) return
            call all_completions("a", abba, r, st)
            call check(error, list_length(r) == 2, "a in +abba")
            if (allocated(error)) return
            call all_completions("abcd", abba, r, st)
            call check(error, nilp(r), "abcd")
        case default
            call check(error, test_completion("abc", abcdef) .and. test_completion("abba", abba), "valid")
            if (allocated(error)) return
            call check(error,.not. test_completion("abcd", abba), "abcd")
        end select

        if (allocated(error)) then
            deallocate (completion_regexp_list)
            return
        end if

        completion_regexp_list = [character(len=4) :: "X"]

        select case (fn)
        case ("try")
            call try_completion("a", abcdef, r, st)
            call check(error, nilp(r), "a in abcdef")

            if (allocated(error)) then
                deallocate (completion_regexp_list)
                return
            end if

            call try_completion("abc", abba, r, st)
            call check(error, nilp(r), "abc")
        case ("all")
            call all_completions("a", abba, r, st)
            call check(error, nilp(r), "a in +abba")
        case default
            call check(error,.not. test_completion("abc", abcdef) .and. .not. test_completion("def", abba), &
                       "nothing matches X")
        end select

        deallocate (completion_regexp_list)
    end subroutine check_regexp

    subroutine test_try_regexp_1(error)
        type(error_type), allocatable, intent(out) :: error

        call check_regexp(error, 1, "try")
    end subroutine test_try_regexp_1

    subroutine test_all_regexp_1(error)
        type(error_type), allocatable, intent(out) :: error

        call check_regexp(error, 1, "all")
    end subroutine test_all_regexp_1

    subroutine test_test_regexp_1(error)
        type(error_type), allocatable, intent(out) :: error

        call check_regexp(error, 1, "test")
    end subroutine test_test_regexp_1

    subroutine test_try_regexp_2(error)
        type(error_type), allocatable, intent(out) :: error

        call check_regexp(error, 2, "try")
    end subroutine test_try_regexp_2

    subroutine test_all_regexp_2(error)
        type(error_type), allocatable, intent(out) :: error

        call check_regexp(error, 2, "all")
    end subroutine test_all_regexp_2

    subroutine test_test_regexp_2(error)
        type(error_type), allocatable, intent(out) :: error

        call check_regexp(error, 2, "test")
    end subroutine test_test_regexp_2

    subroutine test_try_regexp_3(error)
        type(error_type), allocatable, intent(out) :: error

        call check_regexp(error, 3, "try")
    end subroutine test_try_regexp_3

    subroutine test_all_regexp_3(error)
        type(error_type), allocatable, intent(out) :: error

        call check_regexp(error, 3, "all")
    end subroutine test_all_regexp_3

    subroutine test_test_regexp_3(error)
        type(error_type), allocatable, intent(out) :: error

        call check_regexp(error, 3, "test")
    end subroutine test_test_regexp_3

    subroutine test_try_regexp_4(error)
        type(error_type), allocatable, intent(out) :: error

        call check_regexp(error, 4, "try")
    end subroutine test_try_regexp_4

    subroutine test_all_regexp_4(error)
        type(error_type), allocatable, intent(out) :: error

        call check_regexp(error, 4, "all")
    end subroutine test_all_regexp_4

    subroutine test_test_regexp_4(error)
        type(error_type), allocatable, intent(out) :: error

        call check_regexp(error, 4, "test")
    end subroutine test_test_regexp_4

    !> minibuffer-depth and minibuffer-prompt inside the minibuffer, and
    !> minibuffer-exit-hook run as it exits.
    subroutine test_depth_prompt(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: answer
        type(status_type) :: st
        logical :: have

        call minibuffer_prompt(seen_prompt, have)
        call check(error,.not. have .and. minibuffer_depth() == 0, "none active")
        if (allocated(error)) return
        exit_hook_ran = .false.
        call minibuffer_exit_hook%add(note_exit)
        minibuffer_setup_function => note_depth
        call type_text("x"//achar(13))
        call read_from_minibuffer("Depth: ", answer, st)
        call minibuffer_exit_hook%remove(note_exit)
        call check(error, seen_depth == 1 .and. seen_prompt == "Depth: " .and. exit_hook_ran, "in the minibuffer")
        if (allocated(error)) return
        call check(error, minibuffer_depth() == 0 .and. answer == "x", "after")
    end subroutine test_depth_prompt

    !> Ports minibuf-tests.el's test-inhibit-interaction.
    subroutine test_inhibit_interaction(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: answer
        type(status_type) :: st

        inhibit_interaction = .true.
        call read_from_minibuffer("foo: ", answer, st)
        inhibit_interaction = .false.
        call check(error, st%code == FM_INHIBITED_INTERACTION, "read-from-minibuffer")
    end subroutine test_inhibit_interaction

    !> assoc-string: strings and symbols, conses and single strings, case
    !> folding.
    subroutine test_assoc_string(error)
        type(error_type), allocatable, intent(out) :: error
        type(value_type) :: l, r

        l = list(make_string("one"), list(sym("Two"), make_fixnum(2_int64)), make_fixnum(3_int64))
        r = assoc_string("one", l)
        call check(error, is_string(r, "one"), "single string")
        if (allocated(error)) return
        r = assoc_string("two", l)
        call check(error, nilp(r), "case matters")
        if (allocated(error)) return
        r = assoc_string("two", l, case_fold=.true.)
        call check(error, vtag(r) == V_LIST, "case folded")
    end subroutine test_assoc_string

    !> read-variable completes over the user options; RET with empty
    !> input gives the default.
    subroutine test_read_variable(error)
        use simple_fm, only: read_variable
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: name
        type(status_type) :: st

        call fresh_scratch()
        call type_text("fill_col"//achar(9)//achar(13))
        call read_variable("Variable: ", name, st)
        call check(error, st%ok() .and. name == "fill_column", name)
        if (allocated(error)) return
        call type_text(achar(13))
        call read_variable("Variable: ", name, st, default_value="tab_width")
        call check(error, name, "tab_width")
    end subroutine test_read_variable

    ! Minibuffer-setup function: note the depth and the prompt.
    subroutine note_depth()
        logical :: have

        seen_depth = minibuffer_depth()
        call minibuffer_prompt(seen_prompt, have)
    end subroutine note_depth

    ! minibuffer-exit-hook function.
    subroutine note_exit(status)
        type(status_type), intent(out) :: status

        status = status_type()
        exit_hook_ran = minibufferp()
    end subroutine note_exit
end module test_minibuf_fm
