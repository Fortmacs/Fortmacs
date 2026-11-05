! test_shell_script_mod.f90 -- tests for Shell-script mode (shell_script_mod).
! Ports GNU Emacs 31.1 test/lisp/progmodes/sh-script-tests.el.
!
! Copyright (C) 2020-2026 Free Software Foundation, Inc.
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

!> Ports sh-script-tests.el (all 6 tests; `test_indentation' runs the
!> cases of sh-indents.erts) and own tests of the feature tables, the
!> here-document syntax and `sh_set_shell'.
module test_shell_script_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use execute_interactively_mod, only: prefix_argument_type
    use testdrive, only: new_unittest, unittest_type, error_type, check, skip_test
    use error_mod, only: status_type
    use handle_mod, only: handle_type
    use value_mod, only: value_type, symbols, xsymbol, list_elements, vtag, V_NULL, V_NAME, V_ARRAY, choice_type
    use buffer_mod, only: get_or_make_buffer, set_buffer, close_buffer, erase_buffer, is_buffer_live
    use insert_delete_mod, only: insert
    use editing_functions_mod, only: point, point_minimum, point_maximum, goto_character, buffer_string
    use editing_commands_mod, only: forward_line, forward_character
    use search_mod, only: search_forward
    use text_properties_mod, only: get_text_property
    use bindings_mod, only: initialize_bindings
    use modes_mod, only: minor_modes
    use indent_mod, only: indent_region, indent_for_tab_command
    use font_lock_mod, only: font_lock_ensure
    use syntax_mod, only: parse_state_type, syntax_parser_state, ST_STRING_STYLE
    use data_mod, only: choice_state, setting_errors
    use shell_script_mod, only: sh_mode, sh_indent_supported_here, sh_indent_for_continuation, &
                                sh_indent_after_continuation, sh_smie_default_backward_token, sh_set_shell, &
                                sh_builtins, sh_canonicalize_shell, sh_shell, &
                                SH_PLUS_PLUS, &
                                SH_INDENT_AFTER_CONTINUATION_ALWAYS, &
                                sh_completion_at_point_function, sh_name_style, sh_load_style, sh_save_styles_to_buffer, &
                                sh_styles, sh_basic_offset, sh_indent_for_do, SH_PLUS, sh_if, sh_for, sh_case, &
                                sh_while_getopts, sh_while, sh_function, sh_indexed_loop, sh_modify, sh_variable_value, &
                                sh_indent_for_fi, sh_indent_choice_setting_type, SH_MINUS, SH_STAR
    use skeleton_mod, only: skeleton_element_type, skeleton_prompt, skeleton_text, SKELETON_END
    use simple_minded_indentation_engine_mod, only: smie_configuration_show_indent, smie_configuration_guess
    use keyboard_mod, only: keyboard_store_bytes, discard_input, last_message
    use minibuffer_reading_tables_mod, only: completion_at_point_function_data_type, TABLE_FUNCTION, COMPLETE_ALL
    use value_mod, only: list_length, list_element, xstring, value_type
    use expression_motion_mod, only: beginning_of_subprogram, end_of_subprogram
    use window_mod, only: display_buffer_overriding_action, display_action_type, pop_to_buffer_same_window, &
                          get_buffer_window, delete_other_windows
    use frame_mod, only: selected_window
    use buffer_mod, only: get_buffer
    use string_type_mod, only: string_type
    implicit none(type, external)
    private

    public :: collect_shell_script_mod

    character(len=*), parameter :: LF = achar(10), TAB = achar(9), CR = achar(13)

contains

    !> Collect all exported unit tests.
    subroutine collect_shell_script_mod(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("test-sh-script-indentation", test_script_indentation), &
                    new_unittest("test-basic-sh-indentation", test_basic_indentation), &
                    new_unittest("test-indentation", test_indentation), &
                    new_unittest("test-indent-after-continuation", test_after_continuation), &
                    new_unittest("test-backward-token", test_backward_token), &
                    new_unittest("sh-script-test-do-fontification", test_do_fontification), &
                    new_unittest("sh-feature", test_feature), &
                    new_unittest("sh-here-document-syntax", test_here_doc), &
                    new_unittest("sh-canonicalize-shell", test_canonicalize), &
                    new_unittest("sh-completion-at-point-function", test_completion), &
                    new_unittest("sh-defun-prompt-regexp", test_subprogram), &
                    new_unittest("sh-styles", test_styles), &
                    new_unittest("sh-var-value", test_variable_value), &
                    new_unittest("sh-skeletons", test_skeletons), &
                    new_unittest("sh-modify", test_sh_modify), &
                    new_unittest("smie-config", test_smie_configuration), &
                    new_unittest("display-buffer-overriding-action", test_overriding_action) &
                    ]
    end subroutine collect_shell_script_mod

    !> A fresh buffer holding TEXT (with-temp-buffer, insert).
    subroutine temporary_buffer(text, h)
        character(len=*), intent(in) :: text
        type(handle_type), intent(out) :: h
        type(status_type) :: st

        call initialize_bindings()
        call get_or_make_buffer(" *sh-test*", h, st)
        call set_buffer(h, st)
        call erase_buffer(st)
        call insert(text, st)
    end subroutine temporary_buffer

    !> TEXT in a fresh buffer in `shell_script_mode'.
    subroutine script(text, h)
        character(len=*), intent(in) :: text
        type(handle_type), intent(out) :: h
        type(status_type) :: st

        call temporary_buffer(text, h)
        call sh_mode(st)
    end subroutine script

    subroutine leave(h)
        type(handle_type), intent(in) :: h
        type(status_type) :: st

        call close_buffer(h, st)
    end subroutine leave

    subroutine test_script_indentation(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        integer(int64) :: shortage

        call script("relative-path/to/configure --prefix=$prefix\"//LF// &
                    "             --with-x", h)

        checks: block
            if (len(sh_indent_supported_here()) == 0) then
                call skip_test(error, "sh-indent-supported-here")
                exit checks
            end if

            call goto_character(point_minimum())
            shortage = forward_line(1_int64)
            call indent_for_tab_command(prefix_argument_type(), st)
            call check(error, buffer_string(), "relative-path/to/configure --prefix=$prefix\"//LF// &
                       TAB//TAB//TAB//"   --with-x")
        end block checks

        call leave(h)
    end subroutine test_script_indentation

    subroutine test_basic_indentation(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call script("myecho () {"//LF//"echo foo"//LF//"}"//LF, h)

        checks: block
            if (len(sh_indent_supported_here()) == 0) then
                call skip_test(error, "sh-indent-supported-here")
                exit checks
            end if

            call indent_region(point_minimum(), point_maximum(), st)
            call check(error, buffer_string(), "myecho () {"//LF//"    echo foo"//LF//"}"//LF)
        end block checks

        call leave(h)
    end subroutine test_basic_indentation

    ! One case of sh-indents.erts: TEXT is unchanged by indenting it.
    subroutine erts_case(error, name, text)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), intent(in) :: name, text
        type(handle_type) :: h
        type(status_type) :: st

        call script(text, h)

        if (len(sh_indent_supported_here()) == 0) then
            call skip_test(error, "sh-indent-supported-here")
        else
            call indent_region(point_minimum(), point_maximum(), st)
            call check(error, buffer_string(), text, name)
        end if

        call leave(h)
    end subroutine erts_case

    subroutine test_indentation(error)
        type(error_type), allocatable, intent(out) :: error

        call erts_case(error, "sh-indents1", "if test;then"//LF//"    something"//LF//"fi"//LF//"other"//LF)
        if (allocated(error)) return
        call erts_case(error, "sh-indents2", "if test; then"//LF//"    something"//LF//"fi"//LF//"other"//LF)
        if (allocated(error)) return
        call erts_case(error, "sh-indents3", "if test ; then"//LF//"    something"//LF//"fi"//LF//"other"//LF)
        if (allocated(error)) return
        call erts_case(error, "sh-indents4", "if test ;then"//LF//"    something"//LF//"fi"//LF//"other"//LF)
        if (allocated(error)) return
        call erts_case(error, "sh-indents5", "for i do echo 1; done"//LF//"for i; do echo 1; done"//LF)
        if (allocated(error)) return
        call erts_case(error, "sh-indents-case-pattern-bug#81192", &
                       "case ""$1"" in"//LF// &
                       "    a:b)"//LF// &
                       TAB//"echo ""a b"""//LF// &
                       TAB//";;"//LF// &
                       "    *,*)"//LF// &
                       TAB//"echo ""other"""//LF// &
                       TAB//";;"//LF// &
                       "esac"//LF)
    end subroutine test_indentation

    subroutine test_after_continuation(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        type(choice_type) :: saved

        saved = choice_state(sh_indent_after_continuation)
        call script("for f \"//LF//"in a; do \"//LF//"toto; \"//LF//"done"//LF, h)

        checks: block
            if (len(sh_indent_supported_here()) == 0) then
                call skip_test(error, "sh-indent-supported-here")
                exit checks
            end if

            sh_indent_for_continuation = SH_PLUS_PLUS
            sh_indent_after_continuation = .true.
            call indent_region(point_minimum(), point_maximum(), st)
            call check(error, buffer_string(), "for f \"//LF//TAB//"in a; do \"//LF//"    toto; \"//LF//"    done"//LF)
            if (allocated(error)) exit checks
            sh_indent_after_continuation = SH_INDENT_AFTER_CONTINUATION_ALWAYS
            call indent_region(point_minimum(), point_maximum(), st)
            call check(error, buffer_string(), "for f \"//LF//TAB//"in a; do \"//LF//TAB//"toto; \"//LF//TAB//"done"//LF)
            if (allocated(error)) exit checks
            sh_indent_after_continuation = .false.
            call indent_region(point_minimum(), point_maximum(), st)
            call check(error, buffer_string(), "for f \"//LF//"in a; do \"//LF//"    toto; \"//LF//"done"//LF)
        end block checks

        sh_indent_after_continuation = saved
        call leave(h)
    end subroutine test_after_continuation

    ! (test-sh-back STRING): the default backward token from the end of
    ! STRING ends at its start.
    logical function sh_back(string) result(ok)
        character(len=*), intent(in) :: string
        type(handle_type) :: h
        type(status_type) :: st
        character(len=:), allocatable :: token

        call script("", h)
        call insert(string, st)
        call sh_smie_default_backward_token(token, st)
        ok = point() == 1
        call leave(h)
    end function sh_back

    subroutine test_backward_token(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, sh_back("foo"), "foo")
        if (allocated(error)) return
        call check(error, sh_back("foo.bar"), "foo.bar")
        if (allocated(error)) return
        call check(error, sh_back("foo\1bar"), "foo\1bar")
        if (allocated(error)) return
        call check(error, sh_back("foo\"//LF//"bar"), "foo\ newline bar")
        if (allocated(error)) return
        call check(error, sh_back("foo\"//LF//"\"//LF//"\"//LF//"bar"), "three continuations")
        if (allocated(error)) return
        call check(error, sh_back("foo"), "foo again")
        if (allocated(error)) return
        call check(error,.not. sh_back("foo;bar"), "foo;bar")
        if (allocated(error)) return
        call check(error, sh_back("foo#zot"), "foo#zot")
    end subroutine test_backward_token

    !> The face at point (the first of a list), "" if none.
    function face() result(name)
        character(len=:), allocatable :: name
        type(value_type) :: v
        type(value_type), allocatable :: elements(:)
        type(status_type) :: st

        name = ""
        call get_text_property(point(), value_type(name="face"), v, st)

        if (vtag(v) == V_ARRAY) then
            call list_elements(v, elements)
            v = elements(1)
        end if

        if (.not. allocated(v%a)) return
        if ((vtag(v) == V_NAME .or. vtag(v) == V_NULL)) name = symbols(xsymbol(v))%name
    end function face

    !> Test that "do" gets fontified correctly, even with no ";".
    subroutine test_do_fontification(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        integer(int64) :: found

        call script("", h)
        call insert("for i do echo 1; done", st)
        call minor_modes%font_lock_mode%enable(st)
        call font_lock_ensure(st)
        call goto_character(point_minimum())
        call search_forward("do", found, st)
        call forward_character(st, -1_int64)
        call check(error, face(), "font_lock_keyword_face")
        call leave(h)
    end subroutine test_do_fontification

    ! sh-append and the ancestors: bash's builtins are its own, then posix's
    ! (sh's, bourne's and shell's after them).
    subroutine test_feature(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        type(string_type), allocatable :: b(:)

        call script("", h)

        checks: block
            call sh_set_shell("bash", st)
            call sh_builtins%current(b)
            call check(error, b(1)%text, ".")
            if (allocated(error)) exit checks
            call check(error, b(size(b))%text, "wait")
            if (allocated(error)) exit checks
            call check(error, any_is(b, "command"), "posix's command")
            if (allocated(error)) exit checks
            ! dash has no entry: ash, then sh.
            call sh_set_shell("dash", st)
            call sh_builtins%current(b)
            call check(error, b(1)%text, "hash")
            if (allocated(error)) exit checks
            call check(error,.not. any_is(b, "command"), "no command in dash")
        end block checks

        call leave(h)

    contains

        logical function any_is(strings, s)
            type(string_type), intent(in) :: strings(:)
            character(len=*), intent(in) :: s
            integer :: i

            any_is = .false.

            do i = 1, size(strings)
                if (strings(i)%text == s) any_is = .true.
            end do
        end function any_is
    end subroutine test_feature

    ! A here document is a generic string up to its delimiter.
    subroutine test_here_doc(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(parse_state_type) :: state

        call script("cat <<EOF"//LF//"it's $x"//LF//"EOF"//LF//"echo done"//LF, h)

        checks: block
            call syntax_parser_state(state, 14_int64)
            call check(error, state%instring == ST_STRING_STYLE, "inside the here document")
            if (allocated(error)) exit checks
            call syntax_parser_state(state, 25_int64)
            call check(error, state%instring, -1, "after it")
        end block checks

        call leave(h)
    end subroutine test_here_doc

    subroutine test_canonicalize(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, sh_canonicalize_shell("/bin/bash"), "bash")
        if (allocated(error)) return
        call check(error, sh_canonicalize_shell("/usr/bin/csh"), "tcsh")
        if (allocated(error)) return
        call check(error, sh_canonicalize_shell("/system/bin/sh"), "mksh")
        if (allocated(error)) return
        call check(error, sh_canonicalize_shell("/bin/ksh"), "pdksh")
        if (allocated(error)) return
        call check(error, len(sh_shell%name) > 0, "sh_shell")
    end subroutine test_canonicalize

    ! Whether NAME is among the strings of the list V.
    logical function in_list(v, name)
        type(value_type), intent(in) :: v
        character(len=*), intent(in) :: name
        integer(int64) :: k

        in_list = .false.

        do k = 1, list_length(v)
            if (xstring(list_element(v, k)) == name) in_list = .true.
        end do
    end function in_list

    subroutine test_completion(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(completion_at_point_function_data_type) :: data
        type(value_type) :: all
        type(status_type) :: st

        call script("FOO=1"//LF//"BAR=2"//LF//"echo $F", h)
        call goto_character(point_maximum())

        checks: block
            call sh_completion_at_point_function(data, st)
            call check(error, data%found .and. data%start == point_maximum() - 1 .and. data%end == point_maximum(), "variable")
            if (allocated(error)) exit checks
            call check(error, in_list(data%table%list, "FOO") .and. in_list(data%table%list, "BAR"), "variables")
            if (allocated(error)) exit checks
            ! At a keyword position: variables with "=", commands, keywords.
            call insert(LF//"wh", st)
            call sh_completion_at_point_function(data, st)
            call check(error, data%found .and. data%table%kind == TABLE_FUNCTION, "command")
            if (allocated(error)) exit checks
            call data%table%fn("wh", COMPLETE_ALL, all, st)
            call check(error, in_list(all, "while"), "keyword while")
            if (allocated(error)) exit checks
            call data%table%fn("F", COMPLETE_ALL, all, st)
            call check(error, in_list(all, "FOO="), "assignment FOO=")
        end block checks

        call leave(h)
    end subroutine test_completion

    subroutine test_subprogram(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        logical :: found
        type(status_type) :: st

        call script("x=1"//LF//"foo () {"//LF//"  a"//LF//"}"//LF//"function bar {"//LF//"  b"//LF//"}"//LF, h)
        call goto_character(point_maximum())

        checks: block
            call beginning_of_subprogram(1_int64, found, st)
            call check(error, found .and. point() == 20, "beginning of bar")
            if (allocated(error)) exit checks
            call beginning_of_subprogram(1_int64, found, st)
            call check(error, found .and. point() == 5, "beginning of foo")
            if (allocated(error)) exit checks
            call end_of_subprogram(1_int64, st)
            call check(error, point() == 20, "end of foo")
        end block checks

        call leave(h)
    end subroutine test_subprogram

    !> A fresh buffer in `sh_mode' for SHELL.
    subroutine shell_buffer(shell, h)
        character(len=*), intent(in) :: shell
        type(handle_type), intent(out) :: h
        type(status_type) :: st

        call script("", h)
        call discard_input()
        call sh_set_shell(shell, st, .true., .false.)
    end subroutine shell_buffer

    !> The statement skeletons for bash and csh (the texts GNU Emacs
    !> inserts for the same input).
    subroutine test_skeletons(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call shell_buffer("bash", h)
        call keyboard_store_bytes(CR)
        call sh_if(st, string="true")
        call check(error, buffer_string() == "if true; then"//LF//"    "//LF//"else"//LF//"    "//LF//"fi", &
                   "if: "//buffer_string())
        if (allocated(error)) return
        call check(error, point() == 19_int64, "if: point")
        if (allocated(error)) return
        call shell_buffer("bash", h)
        call sh_for(st, string="i")
        call check(error, buffer_string() == "for i in ; do"//LF//"    $i"//LF//"done", "for: "//buffer_string())
        if (allocated(error)) return
        call check(error, point() == 10_int64, "for: point")
        if (allocated(error)) return
        call shell_buffer("bash", h)
        call keyboard_store_bytes("a"//CR//"b"//CR//CR)
        call sh_case(st, string="$x")
        call check(error, buffer_string() == "case $x in"//LF//"    a)"//LF//TAB//LF//TAB//";;"//LF//"    b)"//LF//TAB//LF// &
                   TAB//";;"//LF//"    *)"//LF//TAB//LF//"esac", "case: "//buffer_string())
        if (allocated(error)) return
        call shell_buffer("bash", h)
        call sh_while_getopts(st, string="ab:")
        call check(error, buffer_string() == "while getopts :ab: OPT; do"//LF//"    case $OPT in"//LF//TAB//"a|+a)"//LF// &
                   TAB//"    "//LF//TAB//"    ;;"//LF//TAB//"b|+b)"//LF//TAB//'    "$OPTARG"'//LF//TAB//"    ;;"//LF// &
                   TAB//"*)"//LF//TAB//'    echo "usage: ${0##*/} [+-ab ARG} [--] ARGS..."'//LF//TAB//"    exit 2"//LF// &
                   "    esac"//LF//"done"//LF//"shift $(( OPTIND - 1 ))"//LF//"OPTIND=1", "getopts: "//buffer_string())
        if (allocated(error)) return
        call check(error, point() == 57_int64, "getopts: point")
        if (allocated(error)) return
        call shell_buffer("csh", h)
        call sh_while(st, string="x")
        call check(error, buffer_string() == "while( x )"//LF//"    "//LF//"    end", "csh while: "//buffer_string())
        if (allocated(error)) return
        call shell_buffer("bash", h)
        call sh_function(st, string="f")
        call check(error, buffer_string() == "function f() {"//LF//"    "//LF//"}", "function: "//buffer_string())
        if (allocated(error)) return
        call shell_buffer("bash", h)
        call keyboard_store_bytes("10"//CR)
        call sh_indexed_loop(st, string="i")
        call check(error, buffer_string() == "i=1"//LF//"while [ $i -le 10 ]; do"//LF//"    $i"//LF//"    i=$(( i + 1 ))"// &
                   LF//"done", "indexed loop: "//buffer_string())
        call leave(h)
    end subroutine test_skeletons

    !> sh_modify replaces top-level elements (a sub-skeleton is one).
    subroutine test_sh_modify(error)
        type(error_type), allocatable, intent(out) :: error
        type(skeleton_element_type), allocatable :: out(:)

        out = sh_modify([skeleton_prompt("p"), skeleton_text("a"), skeleton_prompt("q"), skeleton_text("b"), SKELETON_END, &
                         skeleton_text("c")], [2, 3], [skeleton_text("x"), skeleton_text("y")])
        call check(error, size(out) == 4, "four elements")
    end subroutine test_sh_modify

    !> smie-config: show, guess, local rules (the rules GNU Emacs guesses).
    subroutine test_smie_configuration(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call shell_buffer("bash", h)
        call insert("if true; then"//LF//"  echo x"//LF//"fi"//LF//"while x; do"//LF//"  y"//LF//"done"//LF, st)
        call goto_character(1_int64)
        call goto_character(15_int64)
        call smie_configuration_show_indent(.false., st)
        call check(error, last_message == 'Rules used: SMIE_ELEMENT "basic" -> 4, SMIE_BEFORE "then" -> 0, '// &
                   'SMIE_AFTER "then" -> SMIE_RULE_NONE', "show: "//last_message)
        if (allocated(error)) return
        call smie_configuration_guess(st)
        call check(error, last_message == "Local rules set", "guess: "//last_message)
        if (allocated(error)) return
        call smie_configuration_show_indent(.false., st)
        call check(error, last_message == 'Rules used: SMIE_ELEMENT "basic" -> 4, SMIE_BEFORE "then" -> -2(via 0), '// &
                   'SMIE_AFTER "then" -> SMIE_RULE_NONE', "show after: "//last_message)
        if (allocated(error)) return
        call indent_region(point_minimum(), point_maximum(), st)
        call check(error, buffer_string() == "if true; then"//LF//"  echo x"//LF//"fi"//LF//"while x; do"//LF//"  y"//LF// &
                   "done"//LF, "indented: "//buffer_string())
        call leave(h)
    end subroutine test_smie_configuration

    subroutine test_styles(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, out
        integer(int64) :: saved_offset
        type(sh_indent_choice_setting_type) :: saved_do
        character(len=:), allocatable :: text
        type(status_type) :: st

        call script("", h)
        saved_offset = sh_basic_offset%value
        saved_do = sh_indent_for_do

        checks: block
            sh_basic_offset = 2
            call sh_name_style("two", st)
            call check(error, size(sh_styles) >= 1, "style created")
            if (allocated(error)) exit checks
            call check(error, trim(sh_styles(1)%name), "two")
            if (allocated(error)) exit checks
            sh_basic_offset = 8
            call sh_load_style("two", st)
            call check(error, sh_basic_offset%value == 2, "style loaded")
            if (allocated(error)) exit checks
            call check(error, sh_indent_for_do%columns == 0, "columns loaded")
            if (allocated(error)) exit checks
            call sh_load_style("nonesuch", st)
            call check(error,.not. st%ok(), "unknown style")
            if (allocated(error)) exit checks
            sh_indent_for_do = SH_PLUS
            call sh_name_style("two", st)
            call sh_save_styles_to_buffer(" *sh-styles*", st)
            out = get_buffer(" *sh-styles*")
            call set_buffer(out, st)
            text = buffer_string()
            call check(error, index(text, LF//"sh_styles = [sh_style_type(name=""two"", values=[sh_indent_type(columns=2), ") &
                       == 1, "saved styles")
            if (allocated(error)) exit checks
            call check(error, index(text, "sh_indent_type(choice=SH_PLUS)") > 0, "named constant")
            if (allocated(error)) exit checks
            call sh_load_style("two", st)
            call check(error, sh_indent_for_do%choice == SH_PLUS .and. sh_indent_for_do%columns < 0, "named constant loaded")
            call close_buffer(out, st)
        end block checks

        sh_basic_offset = saved_offset
        sh_indent_for_do = saved_do
        deallocate (sh_styles)
        allocate (sh_styles(0))
        call set_buffer(h, st)
        call leave(h)
    end subroutine test_styles

    ! Own: the indentation variables' numbers and named constants, and their
    ! buffer-local values.
    subroutine test_variable_value(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, other
        type(sh_indent_choice_setting_type) :: saved
        integer(int64) :: saved_offset
        type(status_type) :: st

        call temporary_buffer("", h)
        saved = sh_indent_for_fi
        saved_offset = sh_basic_offset%value

        checks: block
            sh_basic_offset = 4
            sh_indent_for_fi%columns = 3
            call check(error, sh_variable_value(sh_indent_for_fi), 3)
            if (allocated(error)) exit checks
            sh_indent_for_fi%columns = -2
            call check(error, sh_variable_value(sh_indent_for_fi), -2)
            if (allocated(error)) exit checks
            sh_indent_for_fi = SH_MINUS
            call check(error, sh_variable_value(sh_indent_for_fi), -4)
            if (allocated(error)) exit checks
            call check(error, sh_indent_for_fi%other_value(), "")
            if (allocated(error)) exit checks
            sh_indent_for_fi = SH_STAR
            call check(error, sh_variable_value(sh_indent_for_fi), 2)
            if (allocated(error)) exit checks
            ! A buffer-local number survives switching buffers.
            call sh_indent_for_fi%make_local()
            sh_indent_for_fi%columns = 5
            call get_or_make_buffer(" *sh-test-other*", other, st)
            call set_buffer(other, st)
            call check(error, sh_indent_for_fi%choice == SH_STAR .and. sh_indent_for_fi%columns < 0, "default value")
            if (allocated(error)) exit checks
            call set_buffer(h, st)
            call check(error, sh_indent_for_fi%other_value(), "%columns = 5")
            if (allocated(error)) exit checks
            call sh_indent_for_fi%clear_local()
            call check(error, sh_indent_for_fi%choice == SH_STAR, "local value cleared")
            if (allocated(error)) exit checks
            ! A plain number is not a value (panel 2026-11-04).
            if (allocated(setting_errors)) deallocate (setting_errors)
            sh_indent_for_fi%choices_declared = .true.
            sh_indent_for_fi = 0
            call check(error, sh_indent_for_fi%choice == SH_STAR .and. size(setting_errors) == 1, "rejected")
            if (allocated(error)) exit checks
            call check(error, setting_errors(1)%text, "sh_indent_for_fi: 0 is not a value of this option; "// &
                       "a number of columns is `sh_indent_for_fi%columns = 0'")
            deallocate (setting_errors)
        end block checks

        if (sh_indent_for_fi%local_p()) call sh_indent_for_fi%clear_local()
        sh_indent_for_fi = saved
        sh_basic_offset = saved_offset
        if (is_buffer_live(other)) call close_buffer(other, st)
        call leave(h)
    end subroutine test_variable_value

    subroutine test_overriding_action(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, other, window
        type(status_type) :: st

        call script("", h)
        call delete_other_windows(selected_window(), st)
        call pop_to_buffer_same_window(h, st)
        call get_or_make_buffer(" *override-test*", other, st)
        display_buffer_overriding_action = display_action_type(has_inhibit_same_window=.true., inhibit_same_window=.true.)
        call pop_to_buffer_same_window(other, st)
        display_buffer_overriding_action = display_action_type()
        window = get_buffer_window(h)
        call check(error, window%index /= 0, "the same window was not used")
        call delete_other_windows(selected_window(), st)
        call close_buffer(other, st)
        call leave(h)
    end subroutine test_overriding_action
end module test_shell_script_mod
