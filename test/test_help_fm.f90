! test_help_fm.f90 -- tests for help_fm, help_fns_fm and help_mode_fm.
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

!> Ports the help-tests.el substitute-command-keys tests that need no
!> faces, keymap summaries or other quoting styles; the rest are own
!> tests, through the command loop.
module test_help_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_fm, only: status_type
    use handle_fm, only: handle_type
    use lisp_fm, only: value_type, nil
    use buffer_fm, only: buffers, cur, get_buffer_create, get_buffer, set_buffer, current_buffer, buffer_modified_p
    use frame_fm, only: selected_frame, ensure_frame_windows
    use window_fm, only: windows, window_leaves, delete_other_windows, switch_to_buffer, resize_window_tree
    use editfns_fm, only: buffer_string
    use keymap_fm, only: key_parse, keymap_global_set, keymap_set
    use text_mode_fm, only: text_mode, text_mode_map
    use keyboard_fm, only: unread_command_events, discard_input, last_message, command_loop
    use bindings_fm, only: init_bindings
    use xdisp_fm, only: tab_width
    use isearch_fm, only: search_upper_case, UPPER_CASE_T, UPPER_CASE_NOT_YANKS
    use simple_fm, only: fill_prefix
    use help_fm, only: substitute_command_keys, with_help_window
    use help_fns_fm, only: describe_function_1, describe_variable_text
    implicit none(type, external)
    private

    public :: collect_help_fm

    character(len=*), parameter :: LQ = char(226)//char(128)//char(152), RQ = char(226)//char(128)//char(153), &
                                   LF = achar(10)

contains

    subroutine collect_help_fm(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)
        testsuite = [ &
                    new_unittest("help-tests-substitute-command-keys/no-change", test_no_change), &
                    new_unittest("help-tests-substitute-command-keys/commands", test_commands), &
                    new_unittest("help-tests-substitute-command-keys/literal-key-sequence", test_literal_keys), &
                    new_unittest("help-tests-substitute-command-keys/literal-key-sequence-ignore-invalid", &
                                 test_literal_keys_invalid), &
                    new_unittest("help-tests-substitute-command-keys/quotes", test_quotes), &
                    new_unittest("help-tests-substitute-command-keys/literals", test_literals), &
                    new_unittest("help-tests-substitute-command-keys/no-change-2", test_no_change_2), &
                    new_unittest("substitute-command-keys-keymap", test_keymap), &
                    new_unittest("describe-function-command", test_describe_command), &
                    new_unittest("describe-function-api", test_describe_api), &
                    new_unittest("describe-variable", test_describe_variable), &
                    new_unittest("help-empty-input", test_empty_input), &
                    new_unittest("help-window-and-quit", test_help_window), &
                    new_unittest("describe-key", test_describe_key), &
                    new_unittest("describe-key-undefined", test_describe_key_undefined), &
                    new_unittest("describe-key-binding-locus", test_binding_locus), &
                    new_unittest("describe-key-briefly", test_describe_key_briefly), &
                    new_unittest("where-is", test_where_is), &
                    new_unittest("advertised-binding", test_advertised_binding), &
                    new_unittest("describe-function-keys", test_describe_function_keys), &
                    new_unittest("describe-variable-keys", test_describe_variable_keys) &
                    ]
    end subroutine collect_help_fm

    !> A fresh *scratch* with the default bindings.
    subroutine fresh_scratch()
        type(handle_type) :: buf
        type(status_type) :: st
        call init_bindings()
        call discard_input()
        call get_buffer_create("*scratch*", buf, st)
        call set_buffer(buf, st)
        ! One window, showing *scratch*.
        call ensure_frame_windows()
        call delete_other_windows(selected_frame%selected_window, st)
        call resize_window_tree(selected_frame%root_window, 0, 0, 23, 80, 80)
        call switch_to_buffer("*scratch*", st)
        last_message = ""
    end subroutine fresh_scratch

    !> The text of *Help*.
    function help_text() result(text)
        character(len=:), allocatable :: text
        type(handle_type) :: old
        type(status_type) :: st
        old = current_buffer()
        call set_buffer(get_buffer("*Help*"), st)
        text = buffer_string()
        call set_buffer(old, st)
    end function help_text

    !> The number of windows on the frame.
    integer function window_count()
        type(handle_type), allocatable :: leaves(:)
        call window_leaves(selected_frame%root_window, leaves)
        window_count = size(leaves)
    end function window_count

    !> The name of the buffer shown in the selected window.
    function selected_buffer_name() result(name)
        character(len=:), allocatable :: name
        name = buffers(windows(selected_frame%selected_window%index)%buffer%index)%name
    end function selected_buffer_name

    !> Queue `keys` (a key_valid_p string) as input.
    subroutine type_keys(keys)
        character(len=*), intent(in) :: keys
        type(value_type), allocatable :: evs(:)
        type(status_type) :: st
        call key_parse(keys, evs, st)
        call unread_command_events(evs)
    end subroutine type_keys

    !> Check that substitute-command-keys turns `input` into `want`.
    subroutine test_sck(error, input, want)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), intent(in) :: input, want
        character(len=:), allocatable :: got
        got = substitute_command_keys(input)
        call check(error, got == want .and. len(got) == len(want), input//" gave "//got//", want "//want)
    end subroutine test_sck

    subroutine test_no_change(error)
        type(error_type), allocatable, intent(out) :: error
        call fresh_scratch()
        call test_sck(error, "foo", "foo")
        if (allocated(error)) return
        call test_sck(error, "\invalid-escape", "\invalid-escape")
    end subroutine test_no_change

    !> As upstream, but with forward-char for goto-char (on M-g c there).
    subroutine test_commands(error)
        type(error_type), allocatable, intent(out) :: error
        call fresh_scratch()
        call test_sck(error, "foo \[forward_char]", "foo C-f")
        if (allocated(error)) return
        call test_sck(error, "\[next_line]", "C-n")
        if (allocated(error)) return
        call test_sck(error, "\[next_line]"//LF//"\[next_line]", "C-n"//LF//"C-n")
        if (allocated(error)) return
        call test_sck(error, "\[next_line]\[previous_line]", "C-nC-p")
        if (allocated(error)) return
        call test_sck(error, "\[next_line]\=\[previous_line]", "C-n\[previous_line]")
        if (allocated(error)) return
        call test_sck(error, "\[next_line]`foo'", "C-n"//LQ//"foo"//RQ)
        if (allocated(error)) return
        call test_sck(error, "\[emacs-version]", "M-x emacs-version")
        if (allocated(error)) return
        call test_sck(error, "\[emacs-version]\[next_line]", "M-x emacs-versionC-n")
        if (allocated(error)) return
        call test_sck(error, "\[emacs-version]`foo'", "M-x emacs-version"//LQ//"foo"//RQ)
    end subroutine test_commands

    subroutine test_literal_keys(error)
        type(error_type), allocatable, intent(out) :: error
        call fresh_scratch()
        call test_sck(error, "\`C-m'", "C-m")
        if (allocated(error)) return
        call test_sck(error, "\`C-m'\`C-j'", "C-mC-j")
        if (allocated(error)) return
        call test_sck(error, "foo\`C-m'bar\`C-j'baz", "fooC-mbarC-jbaz")
        if (allocated(error)) return
        call test_sck(error, "\`M-x next-line'", "M-x next-line")
        if (allocated(error)) return
        call test_sck(error, "\`mouse-1'", "mouse-1")
    end subroutine test_literal_keys

    subroutine test_literal_keys_invalid(error)
        type(error_type), allocatable, intent(out) :: error
        call fresh_scratch()
        call test_sck(error, "ab\`'cd", "ab\"//LQ//RQ//"cd")
        if (allocated(error)) return
        call test_sck(error, "\`c-c'", "\"//LQ//"c-c"//RQ)
        if (allocated(error)) return
        call test_sck(error, "\`<foo bar baz>'", "\"//LQ//"<foo bar baz>"//RQ)
    end subroutine test_literal_keys_invalid

    !> The text-quoting-style `curve` cases.
    subroutine test_quotes(error)
        type(error_type), allocatable, intent(out) :: error
        call fresh_scratch()
        call test_sck(error, "quotes "//LQ//"like this"//RQ, "quotes "//LQ//"like this"//RQ)
        if (allocated(error)) return
        call test_sck(error, "`x'", LQ//"x"//RQ)
        if (allocated(error)) return
        call test_sck(error, "`", LQ)
        if (allocated(error)) return
        call test_sck(error, "'", RQ)
        if (allocated(error)) return
        call test_sck(error, "\`", "\"//LQ)
    end subroutine test_quotes

    subroutine test_literals(error)
        type(error_type), allocatable, intent(out) :: error
        call fresh_scratch()
        call test_sck(error, "foo \=\[goto_char]", "foo \[goto_char]")
        if (allocated(error)) return
        call test_sck(error, "foo \=\=", "foo \=")
        if (allocated(error)) return
        call test_sck(error, "\=\=", "\=")
        if (allocated(error)) return
        call test_sck(error, "\=\[", "\[")
        if (allocated(error)) return
        call test_sck(error, "\=`x\='", "`x'")
    end subroutine test_literals

    subroutine test_no_change_2(error)
        type(error_type), allocatable, intent(out) :: error
        call fresh_scratch()
        call test_sck(error, "\[foobar", "\[foobar")
        if (allocated(error)) return
        call test_sck(error, "\=", "\=")
    end subroutine test_no_change_2

    !> \<MAP> makes \[COMMAND] look in MAP (and the global map).
    subroutine test_keymap(error)
        type(error_type), allocatable, intent(out) :: error
        call fresh_scratch()
        call test_sck(error, "\[next_history_element]", "M-x next_history_element")
        if (allocated(error)) return
        call test_sck(error, "\<minibuffer_local_map>\[next_history_element]", "M-n")
        if (allocated(error)) return
        call test_sck(error, "\<help_mode_map>\[quit_window]", "q")
        if (allocated(error)) return
        call test_sck(error, "\<no-such-map>\[forward_char]", "C-f")
    end subroutine test_keymap

    subroutine test_describe_command(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: text
        call fresh_scratch()
        call describe_function_1("forward_char", text)
        call check(error, index(text, "an interactive Fortran subroutine in "//LQ//"cmds_fm.f90"//RQ//"."//LF//LF) == 1, &
                   text)
        if (allocated(error)) return
        call check(error, index(text, "It is bound to C-f."//LF) > 0, text)
        if (allocated(error)) return
        call check(error, index(text, "Move point N characters forward (backward if N is negative).") > 0, text)
        if (allocated(error)) return
        ! \[right_char] became a key, and the citation is gone.
        call check(error, index(text, "This is in contrast with"//LF//"<right>, which see.") > 0 .and. &
                   index(text, "Port of") == 0, text)
        if (allocated(error)) return
        ! An apostrophe becomes a right quote.
        ! Ordinary text characters are not listed (help-fns--key-bindings).
        call describe_function_1("self_insert_command", text)
        call check(error, index(text, LF//LF//"It is bound to many ordinary text characters."//LF//LF) > 0, text)
        if (allocated(error)) return
        ! The last key follows " and ".
        call describe_function_1("kill_ring_save", text)
        call check(error, index(text, LF//LF//"It is bound to <copy>, C-<insertchar>, C-<insert> and M-w."//LF//LF) > 0, &
                   text)
        if (allocated(error)) return
        call describe_function_1("help_quit", text)
        call check(error, index(text, "Just exit from the Help command's") == 0 .and. &
                   index(text, "Just exit from the Help command"//RQ//"s command loop.") > 0, text)
    end subroutine test_describe_command

    subroutine test_describe_api(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: text
        call fresh_scratch()
        call describe_function_1("insert", text)
        call check(error, index(text, "a Fortran subroutine in "//LQ//"insdel_fm.f90"//RQ//"."//LF//LF// &
                                "call insert(") == 1, text)
        if (allocated(error)) return
        call describe_function_1("point", text)
        call check(error, index(text, "a Fortran function in "//LQ//"editfns_fm.f90"//RQ) == 1 .and. &
                   index(text, LF//"point()"//LF) > 0, text)
        if (allocated(error)) return
        call describe_function_1("no-such-function", text)
        call check(error, index(text, "Not documented.") > 0, text)
    end subroutine test_describe_api

    subroutine test_describe_variable(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: text
        logical :: found
        integer :: saved
        call fresh_scratch()
        if (allocated(fill_prefix%value)) deallocate (fill_prefix%value)
        saved = tab_width%value
        tab_width = 4
        call describe_variable_text("tab_width", text, found)
        tab_width = saved
        call check(error, found .and. index(text, "tab_width is a variable defined in "//LQ//"xdisp_fm.f90"//RQ// &
                                            "."//LF//LF//"Its value is 4"//LF//"Original value was 8"//LF// &
                                            "Its Fortran name is "//LQ//"tab_width"//RQ//"."//LF//LF// &
                                            "Distance between tab stops") == 1, text)
        if (allocated(error)) return
        call describe_variable_text("tab_width", text, found)
        call check(error, index(text, "Original value") == 0, text)
        if (allocated(error)) return
        call describe_variable_text("search_upper_case", text, found)
        call check(error, index(text, "Its value is UPPER_CASE_NOT_YANKS"//LF) > 0, text)
        if (allocated(error)) return
        call describe_variable_text("completion_ignored_extensions", text, found)
        call check(error, index(text, "Its value is"//LF//"["".o"", ""~"", ") > 0, text)
        if (allocated(error)) return
        call describe_variable_text("current_prefix_arg", text, found)
        call check(error, index(text, "Its value is nil"//LF) > 0, text)
        if (allocated(error)) return
        call describe_variable_text("no-such-variable", text, found)
        call check(error,.not. found)
        if (allocated(error)) return
        ! An original value of 20 characters or more starts on the next
        ! line.
        search_upper_case = UPPER_CASE_T
        call describe_variable_text("search_upper_case", text, found)
        search_upper_case = UPPER_CASE_NOT_YANKS
        call check(error, index(text, "Its value is UPPER_CASE_T"//LF// &
                                "Original value was "//LF//"UPPER_CASE_NOT_YANKS"//LF) > 0, text)
        if (allocated(error)) return
        ! A value fits on the "Its value is" line when it is shorter than
        ! 56 characters.
        ! (fill_prefix is nil: unallocated.)
        fill_prefix = repeat("x", 53)
        call describe_variable_text("fill_prefix", text, found)
        call check(error, index(text, "Its value is """//repeat("x", 53)//""""//LF) > 0, text)
        if (allocated(error)) return
        fill_prefix = repeat("x", 54)
        call describe_variable_text("fill_prefix", text, found)
        deallocate (fill_prefix%value)
        call check(error, index(text, "Its value is"//LF//""""//repeat("x", 54)//""""//LF) > 0, text)
    end subroutine test_describe_variable

    !> Empty input to C-h f, C-h v and C-h w is an error.
    subroutine test_empty_input(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        call fresh_scratch()
        call type_keys("C-h f RET")
        call command_loop(st)
        call check(error, last_message, "You didn"//RQ//"t specify a function symbol")
        if (allocated(error)) return
        call type_keys("C-h v RET")
        call command_loop(st)
        call check(error, last_message, "You didn"//RQ//"t specify a variable")
        if (allocated(error)) return
        call type_keys("C-h w RET")
        call command_loop(st)
        call check(error, last_message, "No command")
    end subroutine test_empty_input

    !> *Help* is read-only, in Help mode; q goes back.
    subroutine test_help_window(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        type(handle_type) :: help
        logical :: modified
        call fresh_scratch()
        call with_help_window("Some help."//LF, st)
        ! The help buffer pops up in a new window below; the selected
        ! window and the current buffer stay.
        call check(error, st%ok() .and. buffers(cur)%name == "*scratch*" .and. window_count() == 2 .and. &
                   selected_buffer_name() == "*scratch*", buffers(cur)%name)
        if (allocated(error)) return
        help = get_buffer("*Help*")
        call check(error, buffers(help%index)%read_only .and. help_text() == "Some help."//LF, help_text())
        if (allocated(error)) return
        call check(error, last_message == "Type C-x 1 to delete the help window.", last_message)
        if (allocated(error)) return
        call buffer_modified_p(help, modified, st)
        call check(error,.not. modified, "*Help* is unmodified")
        if (allocated(error)) return
        ! q in the help window deletes it.
        call type_keys("C-x o")
        call command_loop(st)
        call check(error, selected_buffer_name() == "*Help*", selected_buffer_name())
        if (allocated(error)) return
        call type_keys("q")
        call command_loop(st)
        call check(error, buffers(cur)%name == "*scratch*" .and. window_count() == 1, buffers(cur)%name)
    end subroutine test_help_window

    subroutine test_describe_key(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        call fresh_scratch()
        call type_keys("C-h k C-f")
        call command_loop(st)
        call check(error, buffers(cur)%name == "*scratch*" .and. window_count() == 2, buffers(cur)%name)
        if (allocated(error)) return
        ! The line is filled (describe-function-1).
        call check(error, index(help_text(), "C-f runs the command forward_char (found in global_map), which is "// &
                                "an"//LF//"interactive Fortran subroutine in "//LQ//"cmds_fm.f90"//RQ//"."//LF//LF) == 1, &
                   help_text())
        if (allocated(error)) return
        call type_keys("<f1> k C-x C-f")
        call command_loop(st)
        call check(error, index(help_text(), "C-x C-f runs the command find_file (found in global_map)") == 1, &
                   help_text())
        if (allocated(error)) return
        ! The help window shows *Help* again: no message.
        call check(error, window_count() == 2, "two windows")
        if (allocated(error)) return
        call type_keys("C-x o q")
        call command_loop(st)
        call check(error, buffers(cur)%name == "*scratch*", buffers(cur)%name)
    end subroutine test_describe_key

    subroutine test_describe_key_undefined(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        call fresh_scratch()
        call type_keys("C-h k C-x z")
        call command_loop(st)
        call check(error, last_message == "C-x z is undefined" .and. buffers(cur)%name == "*scratch*", last_message)
        if (allocated(error)) return
        ! A key bound to `undefined' is undefined too
        ! (help--binding-undefined-p).
        call keymap_global_set("C-c z", "undefined", st)
        call type_keys("C-h k C-c z")
        call command_loop(st)
        call check(error, last_message == "C-c z is undefined" .and. buffers(cur)%name == "*scratch*", last_message)
        if (allocated(error)) return
        call type_keys("C-h c C-c z")
        call command_loop(st)
        call check(error, last_message == "C-c z is undefined", last_message)
    end subroutine test_describe_key_undefined

    !> help--binding-locus names the major mode's map.
    subroutine test_binding_locus(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        type(handle_type) :: buf
        call fresh_scratch()
        call get_buffer_create("*locus*", buf, st)
        call set_buffer(buf, st)
        call text_mode(st)
        call keymap_set(text_mode_map, "C-c t", "forward_char", st)
        call type_keys("C-h k C-c t")
        call command_loop(st)
        call check(error, index(buffer_string(), "C-c t runs the command forward_char (found in text_mode_map)") == 1, &
                   buffer_string())
        call keymap_set(text_mode_map, "C-c t", nil(), st)
        call type_keys("q")
        call command_loop(st)
    end subroutine test_binding_locus

    !> bindings.el's :advertised-binding of undo and set-mark-command.
    subroutine test_advertised_binding(error)
        type(error_type), allocatable, intent(out) :: error
        call fresh_scratch()
        call test_sck(error, "\[undo]", "C-x u")
        if (allocated(error)) return
        call test_sck(error, "\[set_mark_command]", "C-SPC")
        if (allocated(error)) return
        call test_sck(error, "\[undo_redo]", "C-M-_")
    end subroutine test_advertised_binding

    subroutine test_describe_key_briefly(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        call fresh_scratch()
        call type_keys("C-h c C-n")
        call command_loop(st)
        call check(error, last_message == "C-n runs the command next_line", last_message)
    end subroutine test_describe_key_briefly

    subroutine test_where_is(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        call fresh_scratch()
        call type_keys("C-h w f o r w a r d _ c h a r RET")
        call command_loop(st)
        call check(error, last_message == "forward_char is on C-f", last_message)
        if (allocated(error)) return
        call type_keys("C-h w h e l p _ q u i t RET")
        call command_loop(st)
        call check(error, last_message == "help_quit is on C-h q, <f1> q, <help> q", last_message)
    end subroutine test_where_is

    subroutine test_describe_function_keys(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        call fresh_scratch()
        call type_keys("C-h f i n s e r t RET")
        call command_loop(st)
        call check(error, window_count() == 2 .and. &
                   index(help_text(), "insert is a Fortran subroutine in "//LQ//"insdel_fm.f90"//RQ) == 1, &
                   help_text())
        ! Back to one window.
        call fresh_scratch()
    end subroutine test_describe_function_keys

    subroutine test_describe_variable_keys(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        call fresh_scratch()
        call type_keys("C-h v t a b _ w i d t h RET")
        call command_loop(st)
        call check(error, window_count() == 2 .and. &
                   index(help_text(), "tab_width is a variable defined in "//LQ//"xdisp_fm.f90"//RQ) == 1, &
                   help_text())
        ! Back to one window.
        call fresh_scratch()
    end subroutine test_describe_variable_keys
end module test_help_fm
