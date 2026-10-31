! test_sh_script_fm.f90 -- tests for Shell-script mode (sh_script_fm).
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
module test_sh_script_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check, skip_test
    use error_fm, only: status_type
    use handle_fm, only: handle_type
    use lisp_fm, only: value_type, nil, nilp, consp, symbolp, symbols, xsymbol, list_elements
    use lread_fm, only: sym
    use buffer_fm, only: get_buffer_create, set_buffer, kill_buffer, erase_buffer
    use insdel_fm, only: insert
    use editfns_fm, only: point, point_min, point_max, goto_char, buffer_string
    use cmds_fm, only: forward_line, forward_char
    use search_fm, only: search_forward
    use textprop_fm, only: get_text_property
    use bindings_fm, only: init_bindings
    use modes_fm, only: minor_modes
    use indent_fm, only: indent_region, indent_for_tab_command
    use font_lock_fm, only: font_lock_ensure
    use syntax_fm, only: parse_state_type, syntax_ppss, ST_STRING_STYLE
    use sh_script_fm, only: sh_mode, sh_indent_supported_here, sh_indent_for_continuation, &
                            sh_indent_after_continuation, sh_smie_default_backward_token, sh_set_shell, &
                            sh_builtins, sh_string_type, sh_canonicalize_shell, sh_shell, &
                            SH_PLUS_PLUS, SH_CONTINUATION_NO, SH_CONTINUATION_DEEPER, SH_CONTINUATION_ALWAYS, &
                            sh_completion_at_point_function, sh_name_style, sh_load_style, sh_save_styles_to_buffer, &
                            sh_styles_table, sh_basic_offset, sh_indent_for_do, SH_PLUS
    use minibuf_table_fm, only: capf_data_type, TABLE_FUNCTION, COMPLETE_ALL
    use lisp_fm, only: list_length, list_elt, xstring
    use expression_motion_fm, only: beginning_of_subprogram, end_of_subprogram
    use window_fm, only: display_buffer_overriding_action, display_action_type, pop_to_buffer_same_window, &
                         get_buffer_window, delete_other_windows
    use frame_fm, only: selected_window
    use buffer_fm, only: get_buffer
    implicit none(type, external)
    private

    public :: collect_sh_script_fm

    character(len=*), parameter :: LF = achar(10), TAB = achar(9)

contains

    !> Collect all exported unit tests.
    subroutine collect_sh_script_fm(testsuite)
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
                    new_unittest("display-buffer-overriding-action", test_overriding_action) &
                    ]
    end subroutine collect_sh_script_fm

    !> A fresh buffer holding TEXT (with-temp-buffer, insert).
    subroutine temp_buffer(text, h)
        character(len=*), intent(in) :: text
        type(handle_type), intent(out) :: h
        type(status_type) :: st

        call init_bindings()
        call get_buffer_create(" *sh-test*", h, st)
        call set_buffer(h, st)
        call erase_buffer(st)
        call insert(text, st)
    end subroutine temp_buffer

    !> TEXT in a fresh buffer in `shell_script_mode'.
    subroutine script(text, h)
        character(len=*), intent(in) :: text
        type(handle_type), intent(out) :: h
        type(status_type) :: st

        call temp_buffer(text, h)
        call sh_mode(st)
    end subroutine script

    subroutine leave(h)
        type(handle_type), intent(in) :: h
        type(status_type) :: st

        call kill_buffer(h, st)
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

            call goto_char(point_min())
            shortage = forward_line(1_int64)
            call indent_for_tab_command(nil(), st)
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

            call indent_region(point_min(), point_max(), st)
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
            call indent_region(point_min(), point_max(), st)
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
        integer :: saved

        saved = sh_indent_after_continuation
        call script("for f \"//LF//"in a; do \"//LF//"toto; \"//LF//"done"//LF, h)

        checks: block
            if (len(sh_indent_supported_here()) == 0) then
                call skip_test(error, "sh-indent-supported-here")
                exit checks
            end if

            sh_indent_for_continuation = SH_PLUS_PLUS
            sh_indent_after_continuation = SH_CONTINUATION_DEEPER
            call indent_region(point_min(), point_max(), st)
            call check(error, buffer_string(), "for f \"//LF//TAB//"in a; do \"//LF//"    toto; \"//LF//"    done"//LF)
            if (allocated(error)) exit checks
            sh_indent_after_continuation = SH_CONTINUATION_ALWAYS
            call indent_region(point_min(), point_max(), st)
            call check(error, buffer_string(), "for f \"//LF//TAB//"in a; do \"//LF//TAB//"toto; \"//LF//TAB//"done"//LF)
            if (allocated(error)) exit checks
            sh_indent_after_continuation = SH_CONTINUATION_NO
            call indent_region(point_min(), point_max(), st)
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
        type(value_type), allocatable :: elts(:)
        type(status_type) :: st

        name = ""
        call get_text_property(point(), sym("face"), v, st)

        if (consp(v)) then
            call list_elements(v, elts)
            v = elts(1)
        end if

        if (nilp(v)) return
        if (symbolp(v)) name = symbols(xsymbol(v))%name
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
        call goto_char(point_min())
        call search_forward("do", found, st)
        call forward_char(st, -1_int64)
        call check(error, face(), "font_lock_keyword_face")
        call leave(h)
    end subroutine test_do_fontification

    ! sh-append and the ancestors: bash's builtins are its own, then posix's
    ! (sh's, bourne's and shell's after them).
    subroutine test_feature(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        type(sh_string_type), allocatable :: b(:)

        call script("", h)

        checks: block
            call sh_set_shell("bash", st)
            call sh_builtins%get(b)
            call check(error, b(1)%s, ".")
            if (allocated(error)) exit checks
            call check(error, b(size(b))%s, "wait")
            if (allocated(error)) exit checks
            call check(error, any_is(b, "command"), "posix's command")
            if (allocated(error)) exit checks
            ! dash has no entry: ash, then sh.
            call sh_set_shell("dash", st)
            call sh_builtins%get(b)
            call check(error, b(1)%s, "hash")
            if (allocated(error)) exit checks
            call check(error,.not. any_is(b, "command"), "no command in dash")
        end block checks

        call leave(h)

    contains

        logical function any_is(strings, s)
            type(sh_string_type), intent(in) :: strings(:)
            character(len=*), intent(in) :: s
            integer :: i

            any_is = .false.

            do i = 1, size(strings)
                if (strings(i)%s == s) any_is = .true.
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
            call syntax_ppss(state, 14_int64)
            call check(error, state%instring == ST_STRING_STYLE, "inside the here document")
            if (allocated(error)) exit checks
            call syntax_ppss(state, 25_int64)
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
            if (xstring(list_elt(v, k)) == name) in_list = .true.
        end do
    end function in_list

    subroutine test_completion(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(capf_data_type) :: data
        type(value_type) :: all
        type(status_type) :: st

        call script("FOO=1"//LF//"BAR=2"//LF//"echo $F", h)
        call goto_char(point_max())

        checks: block
            call sh_completion_at_point_function(data, st)
            call check(error, data%found .and. data%start == point_max() - 1 .and. data%end == point_max(), "variable")
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
        call goto_char(point_max())

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

    subroutine test_styles(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, out
        integer(int64) :: saved_offset, saved_do
        character(len=:), allocatable :: text
        type(status_type) :: st

        call script("", h)
        saved_offset = sh_basic_offset%value
        saved_do = sh_indent_for_do%value

        checks: block
            sh_basic_offset = 2
            call sh_name_style("two", st)
            call check(error, size(sh_styles_table) >= 1, "style created")
            if (allocated(error)) exit checks
            call check(error, trim(sh_styles_table(1)%name), "two")
            if (allocated(error)) exit checks
            sh_basic_offset = 8
            call sh_load_style("two", st)
            call check(error, sh_basic_offset%value == 2, "style loaded")
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
            call check(error, index(text, LF//"sh_styles_table = [sh_style_type(name=""two"", values=[2, ") == 1, &
                       "saved styles")
            if (allocated(error)) exit checks
            call check(error, index(text, "SH_PLUS") > 0, "named constant")
            call kill_buffer(out, st)
        end block checks

        sh_basic_offset = saved_offset
        sh_indent_for_do = saved_do
        deallocate (sh_styles_table)
        allocate (sh_styles_table(0))
        call set_buffer(h, st)
        call leave(h)
    end subroutine test_styles

    subroutine test_overriding_action(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, other, window
        type(status_type) :: st

        call script("", h)
        call delete_other_windows(selected_window(), st)
        call pop_to_buffer_same_window(h, st)
        call get_buffer_create(" *override-test*", other, st)
        display_buffer_overriding_action = display_action_type(has_inhibit_same_window=.true., inhibit_same_window=.true.)
        call pop_to_buffer_same_window(other, st)
        display_buffer_overriding_action = display_action_type()
        window = get_buffer_window(h)
        call check(error, window%index /= 0, "the same window was not used")
        call delete_other_windows(selected_window(), st)
        call kill_buffer(other, st)
        call leave(h)
    end subroutine test_overriding_action
end module test_sh_script_fm
