! test_python_fm.f90 -- tests for Python mode (python_fm).
! Ports GNU Emacs 31.1 test/lisp/progmodes/python-tests.el in part; the
! indentation, navigation, syntax context and info tests whose forms use
! only what is ported.
!
! Copyright (C) 2013-2026 Free Software Foundation, Inc.
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

!> Ports the tests of test/lisp/progmodes/python-tests.el about
!> indentation, navigation, the syntax context and the `python_info_*'
!> helpers whose forms use only what is ported (translated mechanically
!> from their forms).
module test_python_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use lisp_fm, only: value_type
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_fm, only: status_type
    use handle_fm, only: handle_type
    use buffer_fm, only: get_buffer_create, set_buffer, erase_buffer
    use insdel_fm, only: insert
    use editfns_fm, only: point, point_min, point_max, goto_char, widen, is_bob, is_eob, is_bol, is_eol, line_beginning_position, &
                          line_end_position
    use simple_fm, only: back_to_indentation, line_number_at_pos
    use indent_fm, only: current_column, indent_for_tab_command
    use cmds_fm, only: forward_line, end_of_line, beginning_of_line, forward_char
    use search_fm, only: looking_at, re_search_forward, re_search_backward, regexp_quote, match_end, match_beginning, &
                         SEARCH_NOERROR
    use indent_fm, only: current_indentation
    use bindings_fm, only: init_bindings
    use python_fm, only: python_mode, python_indent_context, python_indent_calculate_indentation, python_indent_line, &
                         python_indent_guess_indent_offset_variable, python_syntax_context, python_nav_beginning_of_definition, &
                         python_nav_end_of_definition, python_nav_backward_definition, python_nav_forward_definition, &
                         python_nav_beginning_of_statement, python_nav_end_of_statement, python_nav_forward_statement, &
                         python_nav_backward_statement, python_nav_beginning_of_block, python_nav_end_of_block, &
               python_nav_forward_block, python_nav_backward_block, python_nav_forward_expression, python_nav_backward_expression, &
                         python_nav_forward_expression_safe, python_nav_backward_expression_safe, python_nav_up_group, &
                         python_nav_backward_up_group, python_info_current_definition, is_python_info_statement_starts_block, &
                         is_python_info_statement_ends_block, is_python_info_beginning_of_statement, &
                         is_python_info_end_of_statement, is_python_info_beginning_of_block, is_python_info_end_of_block, &
                         python_info_dedenter_opening_block_position, is_python_info_dedenter_statement, &
                         is_python_info_line_ends_backslash, python_info_beginning_of_backslash, is_python_info_continuation_line, &
                         is_python_info_block_continuation_line, is_python_info_assignment_continuation_line, &
                         python_info_looking_at_beginning_of_definition, python_info_looking_at_beginning_of_block, &
                         is_python_info_current_line_comment, is_python_info_current_line_empty, is_python_info_docstring, &
                         is_python_info_triple_quoted_string, python_info_encoding_from_cookie, python_util_forward_comment, &
                         python_nav_if_name_main, PYTHON_CONTEXT_COMMENT, PYTHON_CONTEXT_STRING, &
                         PYTHON_CONTEXT_SINGLE_QUOTED_STRING, PYTHON_CONTEXT_TRIPLE_QUOTED_STRING, PYTHON_CONTEXT_PAREN, &
                         PYTHON_INDENT_NO_INDENT, PYTHON_INDENT_AFTER_COMMENT, PYTHON_INDENT_INSIDE_STRING, &
                         PYTHON_INDENT_INSIDE_DOCSTRING, PYTHON_INDENT_INSIDE_PAREN, &
                         PYTHON_INDENT_INSIDE_PAREN_AT_CLOSING_NESTED_PAREN, PYTHON_INDENT_INSIDE_PAREN_AT_CLOSING_PAREN, &
                         PYTHON_INDENT_INSIDE_PAREN_NEWLINE_START, PYTHON_INDENT_INSIDE_PAREN_NEWLINE_START_FROM_BLOCK, &
                         PYTHON_INDENT_INSIDE_PAREN_FROM_BLOCK, PYTHON_INDENT_INSIDE_PAREN_CONTINUATION_LINE, &
                         PYTHON_INDENT_AFTER_BACKSLASH, PYTHON_INDENT_AFTER_BACKSLASH_ASSIGNMENT_CONTINUATION, &
                         PYTHON_INDENT_AFTER_BACKSLASH_BLOCK_CONTINUATION, &
                         PYTHON_INDENT_AFTER_BACKSLASH_DOTTED_CONTINUATION, PYTHON_INDENT_AFTER_BACKSLASH_FIRST_LINE, &
                         PYTHON_INDENT_AFTER_BLOCK_END, PYTHON_INDENT_AFTER_BLOCK_START, PYTHON_INDENT_AFTER_LINE, &
                         PYTHON_INDENT_AT_DEDENTER_BLOCK_START
    implicit none(type, external)
    private

    public :: collect_python_fm

    character(len=*), parameter :: LF = achar(10), TAB = achar(9)

contains

    !> Collect all exported unit tests.
    subroutine collect_python_fm(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("python-indent-pep8-1", test_python_indent_pep8_1), &
                    new_unittest("python-indent-pep8-2", test_python_indent_pep8_2), &
                    new_unittest("python-indent-pep8-3", test_python_indent_pep8_3), &
                    new_unittest("python-indent-hanging-close-paren", test_python_indent_hanging_close_paren), &
                    new_unittest("python-indent-base-case", test_python_indent_base_case), &
                    new_unittest("python-indent-after-comment-1", test_python_indent_after_comment_1), &
                    new_unittest("python-indent-after-comment-2", test_python_indent_after_comment_2), &
                    new_unittest("python-indent-after-comment-3", test_python_indent_after_comment_3), &
                    new_unittest("python-indent-inside-paren-1", test_python_indent_inside_paren_1), &
                    new_unittest("python-indent-inside-paren-2", test_python_indent_inside_paren_2), &
                    new_unittest("python-indent-inside-paren-3", test_python_indent_inside_paren_3), &
                    new_unittest("python-indent-inside-paren-4", test_python_indent_inside_paren_4), &
                    new_unittest("python-indent-inside-paren-5", test_python_indent_inside_paren_5), &
                    new_unittest("python-indent-inside-paren-6", test_python_indent_inside_paren_6), &
                    new_unittest("python-indent-inside-paren-7", test_python_indent_inside_paren_7), &
                    new_unittest("python-indent-inside-paren-8", test_python_indent_inside_paren_8), &
                    new_unittest("python-indent-inside-paren-9", test_python_indent_inside_paren_9), &
                    new_unittest("python-indent-inside-paren-block-1", test_python_indent_inside_paren_block_1), &
                    new_unittest("python-indent-inside-paren-block-3", test_python_indent_inside_paren_block_3), &
                    new_unittest("python-indent-after-block-1", test_python_indent_after_block_1), &
                    new_unittest("python-indent-after-block-2", test_python_indent_after_block_2), &
                    new_unittest("python-indent-after-block-3", test_python_indent_after_block_3), &
                    new_unittest("python-indent-after-async-block-1", test_python_indent_after_async_block_1), &
                    new_unittest("python-indent-after-async-block-2", test_python_indent_after_async_block_2), &
                    new_unittest("python-indent-after-async-block-3", test_python_indent_after_async_block_3), &
                    new_unittest("python-indent-after-backslash-1", test_python_indent_after_backslash_1), &
                    new_unittest("python-indent-after-backslash-2", test_python_indent_after_backslash_2), &
                    new_unittest("python-indent-after-backslash-3", test_python_indent_after_backslash_3), &
                    new_unittest("python-indent-after-backslash-5", test_python_indent_after_backslash_5), &
                    new_unittest("python-indent-after-backslash-6", test_python_indent_after_backslash_6), &
                    new_unittest("python-indent-block-enders-1", test_python_indent_block_enders_1), &
                    new_unittest("python-indent-block-enders-2", test_python_indent_block_enders_2), &
                    new_unittest("python-indent-block-enders-3", test_python_indent_block_enders_3), &
                    new_unittest("python-indent-block-enders-4", test_python_indent_block_enders_4), &
                    new_unittest("python-indent-block-enders-5", test_python_indent_block_enders_5), &
                    new_unittest("python-indent-dedenters-1", test_python_indent_dedenters_1), &
                    new_unittest("python-indent-dedenters-2", test_python_indent_dedenters_2), &
                    new_unittest("python-indent-dedenters-comment-else", test_python_indent_dedenters_comment_else), &
                    new_unittest("python-indent-dedenters-3", test_python_indent_dedenters_3), &
                    new_unittest("python-indent-dedenters-4", test_python_indent_dedenters_4), &
                    new_unittest("python-indent-dedenters-5", test_python_indent_dedenters_5), &
                    new_unittest("python-indent-dedenters-6", test_python_indent_dedenters_6), &
                    new_unittest("python-indent-dedenters-7", test_python_indent_dedenters_7), &
                    new_unittest("python-indent-dedenters-8", test_python_indent_dedenters_8), &
                    new_unittest("python-indent-dedenters-9", test_python_indent_dedenters_9), &
                    new_unittest("python-indent-inside-string-1", test_python_indent_inside_string_1), &
                    new_unittest("python-indent-inside-string-2", test_python_indent_inside_string_2), &
                    new_unittest("python-indent-inside-string-3", test_python_indent_inside_string_3), &
                    new_unittest("python-indent-after-match-block", test_python_indent_after_match_block), &
                    new_unittest("python-indent-after-case-block", test_python_indent_after_case_block), &
                    new_unittest("python-indent-after-re-match", test_python_indent_after_re_match), &
                    new_unittest("python-indent-after-bare-match", test_python_indent_after_bare_match), &
                    new_unittest("python-indent-badly-indented-block-end", test_python_indent_badly_indented_block_end), &
                    new_unittest("python-syntax-context-1", test_python_syntax_context_1), &
                    new_unittest("python-nav-beginning-of-defun-1", test_python_nav_beginning_of_definition_1), &
                    new_unittest("python-nav-beginning-of-defun-2", test_python_nav_beginning_of_definition_2), &
                    new_unittest("python-nav-beginning-of-defun-3", test_python_nav_beginning_of_definition_3), &
                    new_unittest("python-nav-beginning-of-defun-4", test_python_nav_beginning_of_definition_4), &
                    new_unittest("python-nav-beginning-of-defun-5", test_python_nav_beginning_of_definition_5), &
                    new_unittest("python-nav-beginning-of-defun-6", test_python_nav_beginning_of_definition_6), &
                    new_unittest("python-nav-end-of-defun-1", test_python_nav_end_of_definition_1), &
                    new_unittest("python-nav-end-of-defun-2", test_python_nav_end_of_definition_2), &
                    new_unittest("python-nav-end-of-defun-3", test_python_nav_end_of_definition_3), &
                    new_unittest("python-nav-backward-defun-1", test_python_nav_backward_definition_1), &
                    new_unittest("python-nav-backward-defun-2", test_python_nav_backward_definition_2), &
                    new_unittest("python-nav-backward-defun-4", test_python_nav_backward_definition_4), &
                    new_unittest("python-nav-forward-defun-1", test_python_nav_forward_definition_1), &
                    new_unittest("python-nav-forward-defun-2", test_python_nav_forward_definition_2), &
                    new_unittest("python-nav-forward-defun-4", test_python_nav_forward_definition_4), &
                    new_unittest("python-nav-beginning-of-statement-1", test_python_nav_beginning_of_statement_1), &
                    new_unittest("python-nav-end-of-statement-1", test_python_nav_end_of_statement_1), &
                    new_unittest("python-nav-end-of-statement-2", test_python_nav_end_of_statement_2), &
                    new_unittest("python-nav-end-of-statement-3", test_python_nav_end_of_statement_3), &
                    new_unittest("python-nav-end-of-statement-4", test_python_nav_end_of_statement_4), &
                    new_unittest("python-nav-forward-statement-1", test_python_nav_forward_statement_1), &
                    new_unittest("python-nav-backward-statement-1", test_python_nav_backward_statement_1), &
                    new_unittest("python-nav-beginning-of-block-1", test_python_nav_beginning_of_block_1), &
                    new_unittest("python-nav-beginning-of-block-2", test_python_nav_beginning_of_block_2), &
                    new_unittest("python-nav-end-of-block-1", test_python_nav_end_of_block_1), &
                    new_unittest("python-nav-end-of-block-2", test_python_nav_end_of_block_2), &
                    new_unittest("python-nav-forward-block-1", test_python_nav_forward_block_1), &
                    new_unittest("python-nav-forward-sexp-2", test_python_nav_forward_expression_2), &
                    new_unittest("python-nav-forward-sexp-3", test_python_nav_forward_expression_3), &
                    new_unittest("python-nav-up-list-1", test_python_nav_up_group_1), &
                    new_unittest("python-info-current-defun-1", test_python_info_current_definition_1), &
                    new_unittest("python-info-current-defun-4", test_python_info_current_definition_4), &
                    new_unittest("python-info-statement-starts-block-p-1", test_is_python_info_statement_starts_block_1), &
                    new_unittest("python-info-statement-starts-block-p-2", test_is_python_info_statement_starts_block_2), &
                    new_unittest("python-info-statement-ends-block-p-1", test_is_python_info_statement_ends_block_1), &
                    new_unittest("python-info-statement-ends-block-p-2", test_is_python_info_statement_ends_block_2), &
                    new_unittest("python-info-statement-ends-block-p-3", test_is_python_info_statement_ends_block_3), &
                    new_unittest("python-info-beginning-of-statement-p-1", test_is_python_info_beginning_of_statement_1), &
                    new_unittest("python-info-beginning-of-statement-p-2", test_is_python_info_beginning_of_statement_2), &
                    new_unittest("python-info-end-of-statement-p-1", test_is_python_info_end_of_statement_1), &
                    new_unittest("python-info-end-of-statement-p-2", test_is_python_info_end_of_statement_2), &
                    new_unittest("python-info-beginning-of-block-p-1", test_is_python_info_beginning_of_block_1), &
                    new_unittest("python-info-beginning-of-block-p-2", test_is_python_info_beginning_of_block_2), &
                    new_unittest("python-info-end-of-block-p-3", test_is_python_info_end_of_block_3), &
                    new_unittest("python-info-dedenter-opening-block-position-1", &
                                 test_python_info_dedenter_opening_block_position_1), &
                    new_unittest("python-info-dedenter-opening-block-position-2", &
                                 test_python_info_dedenter_opening_block_position_2), &
                    new_unittest("python-info-dedenter-statement-p-1", test_is_python_info_dedenter_statement_1), &
                    new_unittest("python-info-dedenter-statement-p-2", test_is_python_info_dedenter_statement_2), &
                    new_unittest("python-info-dedenter-statement-p-3", test_is_python_info_dedenter_statement_3), &
                    new_unittest("python-info-dedenter-statement-p-4", test_is_python_info_dedenter_statement_4), &
                    new_unittest("python-info-dedenter-statement-p-5", test_is_python_info_dedenter_statement_5), &
                    new_unittest("python-info-dedenter-statement-p-6", test_is_python_info_dedenter_statement_6), &
                    new_unittest("python-info-line-ends-backslash-p-1", test_is_python_info_line_ends_backslash_1), &
                    new_unittest("python-info-continuation-line-p-1", test_is_python_info_continuation_line_1), &
                    new_unittest("python-info-block-continuation-line-p-1", test_is_python_info_block_continuation_line_1), &
                    new_unittest("python-info-block-continuation-line-p-2", test_is_python_info_block_continuation_line_2), &
                    new_unittest("python-info-assignment-continuation-line-p-1", &
                                 test_is_python_info_assignment_continuation_line_1), &
                    new_unittest("python-info-assignment-continuation-line-p-2", &
                                 test_is_python_info_assignment_continuation_line_2), &
               new_unittest("python-info-looking-at-beginning-of-defun-1", test_python_info_looking_at_beginning_of_definition_1), &
               new_unittest("python-info-looking-at-beginning-of-defun-2", test_python_info_looking_at_beginning_of_definition_2), &
               new_unittest("python-info-looking-at-beginning-of-defun-3", test_python_info_looking_at_beginning_of_definition_3), &
                    new_unittest("python-info-current-line-comment-p-1", test_is_python_info_current_line_comment_1), &
                    new_unittest("python-info-current-line-empty-p", test_is_python_info_current_line_empty), &
                    new_unittest("python-info-docstring-p-1", test_is_python_info_docstring_1), &
                    new_unittest("python-info-docstring-p-2", test_is_python_info_docstring_2), &
                    new_unittest("python-info-docstring-p-3", test_is_python_info_docstring_3), &
                    new_unittest("python-info-docstring-p-4", test_is_python_info_docstring_4), &
                    new_unittest("python-info-docstring-p-5", test_is_python_info_docstring_5), &
                    new_unittest("python-info-docstring-p-6", test_is_python_info_docstring_6), &
                    new_unittest("python-info-docstring-p-7", test_is_python_info_docstring_7), &
                    new_unittest("python-info-docstring-p-8", test_is_python_info_docstring_8), &
                    new_unittest("python-info-encoding-from-cookie-3", test_python_info_encoding_from_cookie_3) &
                    ]
    end subroutine collect_python_fm

    !> python-tests-with-temp-buffer: CONTENTS in a fresh `python_mode'
    !> buffer (no indentation guessing), point at its start.
    subroutine py_buffer(contents)
        character(len=*), intent(in) :: contents
        type(handle_type) :: buf
        type(status_type) :: st
        logical :: saved

        call init_bindings()
        call get_buffer_create(" *python-test*", buf, st)
        call set_buffer(buf, st)
        call widen()
        call erase_buffer(st)
        saved = python_indent_guess_indent_offset_variable%value
        python_indent_guess_indent_offset_variable = .false.
        call python_mode(st)
        python_indent_guess_indent_offset_variable = saved
        call insert(contents, st)
        call goto_char(point_min())
    end subroutine py_buffer

    !> Move point at beginning of STRING in the current buffer.
    !> Optional argument NUM defaults to 1 and is an integer indicating
    !> how many occurrences must be found, when positive the search is
    !> done forwards, otherwise backwards.  When searching forward and
    !> point is already looking at STRING, it is skipped so the next
    !> STRING occurrence is selected (python-tests-look-at).
    subroutine look(string, num)
        character(len=*), intent(in) :: string
        integer(int64), intent(in), optional :: num
        integer(int64) :: n, f
        character(len=:), allocatable :: re
        type(status_type) :: st

        n = 1
        if (present(num)) n = num
        re = regexp_quote(string)

        do while (n /= 0)
            if (n > 0) then
                if (looking_at(re, st)) call goto_char(match_end(0, st))
                call re_search_forward(re, f, st, noerror=SEARCH_NOERROR)
                if (.not. st%ok() .or. f <= 0) return
                call goto_char(match_beginning(0, st))
                n = n - 1
            else
                call re_search_backward(re, f, st, noerror=SEARCH_NOERROR)
                if (.not. st%ok() .or. f <= 0) return
                n = n + 1
            end if
        end do
    end subroutine look

    !> python-tests-look-at returning the position found (0 if none):
    !> with RESTORE, point is not moved.
    integer(int64) function look_pos(string, num, restore) result(found)
        character(len=*), intent(in) :: string
        integer(int64), intent(in) :: num
        logical, intent(in) :: restore
        integer(int64) :: n, f, starting_point
        character(len=:), allocatable :: re
        type(status_type) :: st

        n = num
        starting_point = point()
        re = regexp_quote(string)
        found = 0

        do while (n /= 0)
            if (n > 0) then
                if (looking_at(re, st)) call goto_char(match_end(0, st))
                call re_search_forward(re, f, st, noerror=SEARCH_NOERROR)
                if (.not. st%ok() .or. f <= 0) exit
                call goto_char(match_beginning(0, st))
                n = n - 1
            else
                call re_search_backward(re, f, st, noerror=SEARCH_NOERROR)
                if (.not. st%ok() .or. f <= 0) exit
                n = n + 1
            end if

            found = point()
        end do

        if (restore) call goto_char(starting_point)
    end function look_pos

    !> True when NAME is allocated and equal to EXPECTED.
    logical function name_is(name, expected)
        character(len=:), allocatable, intent(in) :: name
        character(len=*), intent(in) :: expected

        name_is = .false.
        if (allocated(name)) name_is = name == expected
    end function name_is

    !> (should (eq (car (python-indent-context)) KIND))
    subroutine expect_context(error, kind)
        type(error_type), allocatable, intent(out) :: error
        integer, intent(in) :: kind
        integer :: actual
        integer(int64) :: start
        character(len=64) :: msg

        call python_indent_context(actual, start)
        write (msg, "(a, i0, a, i0)") "context ", actual, " instead of ", kind
        call check(error, actual == kind, trim(msg))
    end subroutine expect_context

    !> (should (= (python-indent-calculate-indentation [PREVIOUS]) N))
    subroutine expect_indent(error, n, previous)
        type(error_type), allocatable, intent(out) :: error
        integer(int64), intent(in) :: n
        logical, intent(in), optional :: previous
        integer(int64) :: actual
        character(len=64) :: msg

        actual = python_indent_calculate_indentation(previous)
        write (msg, "(a, i0, a, i0)") "indentation ", actual, " instead of ", n
        call check(error, actual == n, trim(msg))
    end subroutine expect_indent

    subroutine test_python_indent_pep8_1(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage

        call py_buffer("# Aligned with opening delimiter"//LF &
                       //"foo = long_function_name(var_one, var_two,"//LF &
                       //"                         var_three, var_four)"//LF)
        call expect_context(error, PYTHON_INDENT_NO_INDENT)
        if (allocated(error)) return
        call expect_indent(error, 0_int64)
        if (allocated(error)) return
        call look("foo = long_function_name(var_one, var_two,")
        call expect_context(error, PYTHON_INDENT_AFTER_COMMENT)
        if (allocated(error)) return
        call expect_indent(error, 0_int64)
        if (allocated(error)) return
        call look("var_three, var_four)")
        call expect_context(error, PYTHON_INDENT_INSIDE_PAREN)
        if (allocated(error)) return
        call expect_indent(error, 25_int64)
        if (allocated(error)) return
        if (.false.) shortage = 0
        if (.false.) call python_indent_line(st)
    end subroutine test_python_indent_pep8_1

    subroutine test_python_indent_pep8_2(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage

        call py_buffer("# More indentation included to distinguish this from the rest."//LF &
                       //"def long_function_name("//LF &
                       //"        var_one, var_two, var_three,"//LF &
                       //"        var_four):"//LF &
                       //"    print (var_one)"//LF)
        call expect_context(error, PYTHON_INDENT_NO_INDENT)
        if (allocated(error)) return
        call expect_indent(error, 0_int64)
        if (allocated(error)) return
        call look("def long_function_name(")
        call expect_context(error, PYTHON_INDENT_AFTER_COMMENT)
        if (allocated(error)) return
        call expect_indent(error, 0_int64)
        if (allocated(error)) return
        call look("var_one, var_two, var_three,")
        call expect_context(error, PYTHON_INDENT_INSIDE_PAREN_NEWLINE_START_FROM_BLOCK)
        if (allocated(error)) return
        call expect_indent(error, 8_int64)
        if (allocated(error)) return
        call look("var_four):")
        call expect_context(error, PYTHON_INDENT_INSIDE_PAREN_CONTINUATION_LINE)
        if (allocated(error)) return
        call expect_indent(error, 8_int64)
        if (allocated(error)) return
        call look("print (var_one)")
        call expect_context(error, PYTHON_INDENT_AFTER_BLOCK_START)
        if (allocated(error)) return
        call expect_indent(error, 4_int64)
        if (allocated(error)) return
        if (.false.) shortage = 0
        if (.false.) call python_indent_line(st)
    end subroutine test_python_indent_pep8_2

    subroutine test_python_indent_pep8_3(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage

        call py_buffer("# Extra indentation is not necessary."//LF &
                       //"foo = long_function_name("//LF &
                       //"  var_one, var_two,"//LF &
                       //"  var_three, var_four)"//LF)
        call expect_context(error, PYTHON_INDENT_NO_INDENT)
        if (allocated(error)) return
        call expect_indent(error, 0_int64)
        if (allocated(error)) return
        call look("foo = long_function_name(")
        call expect_context(error, PYTHON_INDENT_AFTER_COMMENT)
        if (allocated(error)) return
        call expect_indent(error, 0_int64)
        if (allocated(error)) return
        call look("var_one, var_two,")
        call expect_context(error, PYTHON_INDENT_INSIDE_PAREN_NEWLINE_START)
        if (allocated(error)) return
        call expect_indent(error, 4_int64)
        if (allocated(error)) return
        call look("var_three, var_four)")
        call expect_context(error, PYTHON_INDENT_INSIDE_PAREN_CONTINUATION_LINE)
        if (allocated(error)) return
        call expect_indent(error, 2_int64)
        if (allocated(error)) return
        if (.false.) shortage = 0
        if (.false.) call python_indent_line(st)
    end subroutine test_python_indent_pep8_3

    subroutine test_python_indent_hanging_close_paren(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage

        call py_buffer("foo = long_function_name(var_one, var_two,"//LF &
                       //"                         var_three, var_four"//LF &
                       //"                         )"//LF)
        call expect_indent(error, 0_int64)
        if (allocated(error)) return
        call look(")")
        call expect_context(error, PYTHON_INDENT_INSIDE_PAREN_AT_CLOSING_PAREN)
        if (allocated(error)) return
        call expect_indent(error, 25_int64)
        if (allocated(error)) return
        if (.false.) shortage = 0
        if (.false.) call python_indent_line(st)
    end subroutine test_python_indent_hanging_close_paren

    subroutine test_python_indent_base_case(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage

        call py_buffer(LF &
                       //LF)
        call goto_char(point_min())
        call expect_context(error, PYTHON_INDENT_NO_INDENT)
        if (allocated(error)) return
        call expect_indent(error, 0_int64)
        if (allocated(error)) return
        shortage = forward_line(1_int64)
        call expect_context(error, PYTHON_INDENT_NO_INDENT)
        if (allocated(error)) return
        call expect_indent(error, 0_int64)
        if (allocated(error)) return
        shortage = forward_line(1_int64)
        call expect_context(error, PYTHON_INDENT_NO_INDENT)
        if (allocated(error)) return
        call expect_indent(error, 0_int64)
        if (allocated(error)) return
        if (.false.) shortage = 0
        if (.false.) call python_indent_line(st)
    end subroutine test_python_indent_base_case

    subroutine test_python_indent_after_comment_1(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage

        call py_buffer("# Contents will be modified to correct indentation"//LF &
                       //"class Blag(object):"//LF &
                       //"    def _on_child_complete(self, child_future):"//LF &
                       //"        if self.in_terminal_state():"//LF &
                       //"            pass"//LF &
                       //"        # We only complete when all our async children have entered a"//LF &
                       //"    # terminal state. At that point, if any child failed, we fail"//LF &
                       //"# with the exception with which the first child failed."//LF)
        call look("# We only complete")
        call expect_context(error, PYTHON_INDENT_AFTER_BLOCK_END)
        if (allocated(error)) return
        call expect_indent(error, 8_int64)
        if (allocated(error)) return
        call look("# terminal state")
        call expect_context(error, PYTHON_INDENT_AFTER_COMMENT)
        if (allocated(error)) return
        call expect_indent(error, 8_int64)
        if (allocated(error)) return
        call look("# with the exception")
        call expect_context(error, PYTHON_INDENT_AFTER_COMMENT)
        if (allocated(error)) return
        call expect_indent(error, 4_int64)
        if (allocated(error)) return
        call look("# terminal state", -1_int64)
        call expect_context(error, PYTHON_INDENT_AFTER_COMMENT)
        if (allocated(error)) return
        call python_indent_line(st)
        call check(error, current_indentation() == 8, "current indentation")
        if (allocated(error)) return
        call look("# with the exception")
        call expect_context(error, PYTHON_INDENT_AFTER_COMMENT)
        if (allocated(error)) return
        call expect_indent(error, 8_int64)
        if (allocated(error)) return
        if (.false.) shortage = 0
        if (.false.) call python_indent_line(st)
    end subroutine test_python_indent_after_comment_1

    subroutine test_python_indent_after_comment_2(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage

        call py_buffer("# Contents will be modified to correct indentation"//LF &
                       //"def func(arg):"//LF &
                       //"    # I don't do much"//LF &
                       //"    return arg"//LF &
                       //"    # This comment is badly indented because the user forced so."//LF &
                       //"    # At this line python.el won't dedent, user is always right."//LF &
                       //LF &
                       //"comment_wins_over_ender = True"//LF &
                       //LF &
                       //"# yeah, that."//LF)
        call look("# I don't do much")
        call expect_context(error, PYTHON_INDENT_AFTER_BLOCK_START)
        if (allocated(error)) return
        call expect_indent(error, 4_int64)
        if (allocated(error)) return
        call look("return arg")
        call expect_context(error, PYTHON_INDENT_AFTER_BLOCK_START)
        if (allocated(error)) return
        call expect_indent(error, 4_int64)
        if (allocated(error)) return
        call look("# This comment is badly indented")
        call expect_context(error, PYTHON_INDENT_AFTER_BLOCK_END)
        if (allocated(error)) return
        call expect_indent(error, 0_int64)
        if (allocated(error)) return
        call look("# At this line python.el won't dedent")
        call expect_context(error, PYTHON_INDENT_AFTER_COMMENT)
        if (allocated(error)) return
        call expect_indent(error, 4_int64)
        if (allocated(error)) return
        shortage = forward_line(1_int64)
        call expect_context(error, PYTHON_INDENT_AFTER_COMMENT)
        if (allocated(error)) return
        call expect_indent(error, 4_int64)
        if (allocated(error)) return
        call look("comment_wins_over_ender")
        call expect_context(error, PYTHON_INDENT_AFTER_COMMENT)
        if (allocated(error)) return
        call expect_indent(error, 4_int64)
        if (allocated(error)) return
        call look("# yeah, that.")
        call expect_context(error, PYTHON_INDENT_AFTER_LINE)
        if (allocated(error)) return
        call expect_indent(error, 0_int64)
        if (allocated(error)) return
        if (.false.) shortage = 0
        if (.false.) call python_indent_line(st)
    end subroutine test_python_indent_after_comment_2

    subroutine test_python_indent_after_comment_3(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage

        call py_buffer(LF &
                       //"class A(object):"//LF &
                       //LF &
                       //"    def something(self, arg):"//LF &
                       //"        if True:"//LF &
                       //"            return arg"//LF &
                       //LF &
                       //"    # A comment"//LF &
                       //LF &
                       //"    @adecorator"//LF &
                       //"    def method(self, a, b):"//LF &
                       //"        pass"//LF)
        call look("@adecorator")
        call expect_context(error, PYTHON_INDENT_AFTER_COMMENT)
        if (allocated(error)) return
        call expect_indent(error, 4_int64)
        if (allocated(error)) return
        if (.false.) shortage = 0
        if (.false.) call python_indent_line(st)
    end subroutine test_python_indent_after_comment_3

    subroutine test_python_indent_inside_paren_1(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage

        call py_buffer(LF &
                       //"data = {"//LF &
                       //"    'key':"//LF &
                       //"    {"//LF &
                       //"        'objlist': ["//LF &
                       //"            {"//LF &
                       //"                'pk': 1,"//LF &
                       //"                'name': 'first',"//LF &
                       //"            },"//LF &
                       //"            {"//LF &
                       //"                'pk': 2,"//LF &
                       //"                'name': 'second',"//LF &
                       //"            }"//LF &
                       //"        ]"//LF &
                       //"    }"//LF &
                       //"}"//LF)
        call look("data = {")
        call expect_context(error, PYTHON_INDENT_NO_INDENT)
        if (allocated(error)) return
        call expect_indent(error, 0_int64)
        if (allocated(error)) return
        call look("'key':")
        call expect_context(error, PYTHON_INDENT_INSIDE_PAREN_NEWLINE_START)
        if (allocated(error)) return
        call expect_indent(error, 4_int64)
        if (allocated(error)) return
        call look("{")
        call expect_context(error, PYTHON_INDENT_INSIDE_PAREN_CONTINUATION_LINE)
        if (allocated(error)) return
        call expect_indent(error, 4_int64)
        if (allocated(error)) return
        call look("'objlist': [")
        call expect_context(error, PYTHON_INDENT_INSIDE_PAREN_NEWLINE_START)
        if (allocated(error)) return
        call expect_indent(error, 8_int64)
        if (allocated(error)) return
        call look("{")
        call expect_context(error, PYTHON_INDENT_INSIDE_PAREN_NEWLINE_START)
        if (allocated(error)) return
        call expect_indent(error, 12_int64)
        if (allocated(error)) return
        call look("'pk': 1,")
        call expect_context(error, PYTHON_INDENT_INSIDE_PAREN_NEWLINE_START)
        if (allocated(error)) return
        call expect_indent(error, 16_int64)
        if (allocated(error)) return
        call look("'name': 'first',")
        call expect_context(error, PYTHON_INDENT_INSIDE_PAREN_CONTINUATION_LINE)
        if (allocated(error)) return
        call expect_indent(error, 16_int64)
        if (allocated(error)) return
        call look("},")
        call expect_context(error, PYTHON_INDENT_INSIDE_PAREN_AT_CLOSING_NESTED_PAREN)
        if (allocated(error)) return
        call expect_indent(error, 12_int64)
        if (allocated(error)) return
        call look("{")
        call expect_context(error, PYTHON_INDENT_INSIDE_PAREN_CONTINUATION_LINE)
        if (allocated(error)) return
        call expect_indent(error, 12_int64)
        if (allocated(error)) return
        call look("'pk': 2,")
        call expect_context(error, PYTHON_INDENT_INSIDE_PAREN_NEWLINE_START)
        if (allocated(error)) return
        call expect_indent(error, 16_int64)
        if (allocated(error)) return
        call look("'name': 'second',")
        call expect_context(error, PYTHON_INDENT_INSIDE_PAREN_CONTINUATION_LINE)
        if (allocated(error)) return
        call expect_indent(error, 16_int64)
        if (allocated(error)) return
        call look("}")
        call expect_context(error, PYTHON_INDENT_INSIDE_PAREN_AT_CLOSING_NESTED_PAREN)
        if (allocated(error)) return
        call expect_indent(error, 12_int64)
        if (allocated(error)) return
        call look("]")
        call expect_context(error, PYTHON_INDENT_INSIDE_PAREN_AT_CLOSING_NESTED_PAREN)
        if (allocated(error)) return
        call expect_indent(error, 8_int64)
        if (allocated(error)) return
        call look("}")
        call expect_context(error, PYTHON_INDENT_INSIDE_PAREN_AT_CLOSING_NESTED_PAREN)
        if (allocated(error)) return
        call expect_indent(error, 4_int64)
        if (allocated(error)) return
        call look("}")
        call expect_context(error, PYTHON_INDENT_INSIDE_PAREN_AT_CLOSING_PAREN)
        if (allocated(error)) return
        call expect_indent(error, 0_int64)
        if (allocated(error)) return
        if (.false.) shortage = 0
        if (.false.) call python_indent_line(st)
    end subroutine test_python_indent_inside_paren_1

    subroutine test_python_indent_inside_paren_2(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage

        call py_buffer(LF &
                       //"data = {'key': {"//LF &
                       //"    'objlist': ["//LF &
                       //"        {'pk': 1,"//LF &
                       //"         'name': 'first'},"//LF &
                       //"        {'pk': 2,"//LF &
                       //"         'name': 'second'}"//LF &
                       //"    ]"//LF &
                       //"}}"//LF)
        call look("data = {")
        call expect_context(error, PYTHON_INDENT_NO_INDENT)
        if (allocated(error)) return
        call expect_indent(error, 0_int64)
        if (allocated(error)) return
        call look("'objlist': [")
        call expect_context(error, PYTHON_INDENT_INSIDE_PAREN_NEWLINE_START)
        if (allocated(error)) return
        call expect_indent(error, 4_int64)
        if (allocated(error)) return
        call look("{'pk': 1,")
        call expect_context(error, PYTHON_INDENT_INSIDE_PAREN_NEWLINE_START)
        if (allocated(error)) return
        call expect_indent(error, 8_int64)
        if (allocated(error)) return
        call look("'name': 'first'},")
        call expect_context(error, PYTHON_INDENT_INSIDE_PAREN)
        if (allocated(error)) return
        call expect_indent(error, 9_int64)
        if (allocated(error)) return
        call look("{'pk': 2,")
        call expect_context(error, PYTHON_INDENT_INSIDE_PAREN_CONTINUATION_LINE)
        if (allocated(error)) return
        call expect_indent(error, 8_int64)
        if (allocated(error)) return
        call look("'name': 'second'}")
        call expect_context(error, PYTHON_INDENT_INSIDE_PAREN)
        if (allocated(error)) return
        call expect_indent(error, 9_int64)
        if (allocated(error)) return
        call look("]")
        call expect_context(error, PYTHON_INDENT_INSIDE_PAREN_AT_CLOSING_NESTED_PAREN)
        if (allocated(error)) return
        call expect_indent(error, 4_int64)
        if (allocated(error)) return
        call look("}}")
        call expect_context(error, PYTHON_INDENT_INSIDE_PAREN_AT_CLOSING_NESTED_PAREN)
        if (allocated(error)) return
        call expect_indent(error, 0_int64)
        if (allocated(error)) return
        call look("}")
        call expect_context(error, PYTHON_INDENT_INSIDE_PAREN_AT_CLOSING_PAREN)
        if (allocated(error)) return
        call expect_indent(error, 0_int64)
        if (allocated(error)) return
        if (.false.) shortage = 0
        if (.false.) call python_indent_line(st)
    end subroutine test_python_indent_inside_paren_2

    subroutine test_python_indent_inside_paren_3(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage

        call py_buffer(LF &
                       //"data = ('these',"//LF &
                       //"        'are',"//LF &
                       //"        'the',"//LF &
                       //"        'tokens')"//LF)
        call look("data = ('these',")
        call expect_context(error, PYTHON_INDENT_NO_INDENT)
        if (allocated(error)) return
        call expect_indent(error, 0_int64)
        if (allocated(error)) return
        shortage = forward_line(1_int64)
        call expect_context(error, PYTHON_INDENT_INSIDE_PAREN)
        if (allocated(error)) return
        call expect_indent(error, 8_int64)
        if (allocated(error)) return
        shortage = forward_line(1_int64)
        call expect_context(error, PYTHON_INDENT_INSIDE_PAREN_CONTINUATION_LINE)
        if (allocated(error)) return
        call expect_indent(error, 8_int64)
        if (allocated(error)) return
        shortage = forward_line(1_int64)
        call expect_context(error, PYTHON_INDENT_INSIDE_PAREN_CONTINUATION_LINE)
        if (allocated(error)) return
        call expect_indent(error, 8_int64)
        if (allocated(error)) return
        if (.false.) shortage = 0
        if (.false.) call python_indent_line(st)
    end subroutine test_python_indent_inside_paren_3

    subroutine test_python_indent_inside_paren_4(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage

        call py_buffer(LF &
                       //"data = [ [ 'these', 'are'],"//LF &
                       //"         ['the', 'tokens' ] ]"//LF)
        call look("data = [ [ 'these', 'are'],")
        call expect_context(error, PYTHON_INDENT_NO_INDENT)
        if (allocated(error)) return
        call expect_indent(error, 0_int64)
        if (allocated(error)) return
        shortage = forward_line(1_int64)
        call expect_context(error, PYTHON_INDENT_INSIDE_PAREN)
        if (allocated(error)) return
        call expect_indent(error, 9_int64)
        if (allocated(error)) return
        if (.false.) shortage = 0
        if (.false.) call python_indent_line(st)
    end subroutine test_python_indent_inside_paren_4

    subroutine test_python_indent_inside_paren_5(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage

        call py_buffer(LF &
                       //"while ((not some_condition) and"//LF &
                       //"       another_condition):"//LF &
                       //"    do_something_interesting("//LF &
                       //"        with_some_arg)"//LF)
        call look("while ((not some_condition) and")
        call expect_context(error, PYTHON_INDENT_NO_INDENT)
        if (allocated(error)) return
        call expect_indent(error, 0_int64)
        if (allocated(error)) return
        shortage = forward_line(1_int64)
        call expect_context(error, PYTHON_INDENT_INSIDE_PAREN_FROM_BLOCK)
        if (allocated(error)) return
        call expect_indent(error, 7_int64)
        if (allocated(error)) return
        shortage = forward_line(1_int64)
        call expect_context(error, PYTHON_INDENT_AFTER_BLOCK_START)
        if (allocated(error)) return
        call expect_indent(error, 4_int64)
        if (allocated(error)) return
        shortage = forward_line(1_int64)
        call expect_context(error, PYTHON_INDENT_INSIDE_PAREN_NEWLINE_START)
        if (allocated(error)) return
        call expect_indent(error, 8_int64)
        if (allocated(error)) return
        if (.false.) shortage = 0
        if (.false.) call python_indent_line(st)
    end subroutine test_python_indent_inside_paren_5

    subroutine test_python_indent_inside_paren_6(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage

        call py_buffer(LF &
                       //"CHOICES = (('some', 'choice'),"//LF &
                       //"           ('another', 'choice'),"//LF &
                       //"           ('more', 'choices'))"//LF)
        call look("CHOICES = (('some', 'choice'),")
        call expect_context(error, PYTHON_INDENT_NO_INDENT)
        if (allocated(error)) return
        call expect_indent(error, 0_int64)
        if (allocated(error)) return
        shortage = forward_line(1_int64)
        call expect_context(error, PYTHON_INDENT_INSIDE_PAREN)
        if (allocated(error)) return
        call expect_indent(error, 11_int64)
        if (allocated(error)) return
        shortage = forward_line(1_int64)
        call expect_context(error, PYTHON_INDENT_INSIDE_PAREN_CONTINUATION_LINE)
        if (allocated(error)) return
        call expect_indent(error, 11_int64)
        if (allocated(error)) return
        if (.false.) shortage = 0
        if (.false.) call python_indent_line(st)
    end subroutine test_python_indent_inside_paren_6

    subroutine test_python_indent_inside_paren_7(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage

        call py_buffer("import re as myre"//LF &
                       //"var = ["//LF)
        call goto_char(point_max())
        call expect_context(error, PYTHON_INDENT_INSIDE_PAREN_NEWLINE_START)
        if (allocated(error)) return
        if (.false.) shortage = 0
        if (.false.) call python_indent_line(st)
    end subroutine test_python_indent_inside_paren_7

    subroutine test_python_indent_inside_paren_8(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage

        call py_buffer(LF &
                       //"for a in [  # comment"//LF &
                       //"          'some',  # Manually indented."//LF &
                       //"          'thing']:  # Respect indentation of the previous line."//LF)
        call look("for a in [  # comment")
        call expect_context(error, PYTHON_INDENT_NO_INDENT)
        if (allocated(error)) return
        call expect_indent(error, 0_int64)
        if (allocated(error)) return
        shortage = forward_line(1_int64)
        call expect_context(error, PYTHON_INDENT_INSIDE_PAREN_NEWLINE_START_FROM_BLOCK)
        if (allocated(error)) return
        call expect_indent(error, 8_int64)
        if (allocated(error)) return
        shortage = forward_line(1_int64)
        call expect_context(error, PYTHON_INDENT_INSIDE_PAREN_CONTINUATION_LINE)
        if (allocated(error)) return
        call expect_indent(error, 10_int64)
        if (allocated(error)) return
        if (.false.) shortage = 0
        if (.false.) call python_indent_line(st)
    end subroutine test_python_indent_inside_paren_8

    subroutine test_python_indent_inside_paren_9(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage

        call py_buffer(LF &
                       //"a = ((("//LF &
                       //"    1, 2),"//LF &
                       //"      3),  # Do not respect the indentation of the previous line"//LF &
                       //"     4)  # Do not respect the indentation of the previous line"//LF &
                       //"b = (("//LF &
                       //"        1, 2),  # Manually indented"//LF &
                       //"     3,  # Do not respect the indentation of the previous line"//LF &
                       //"     4,  # Respect the indentation of the previous line"//LF &
                       //"        5,  # Manually indented"//LF &
                       //"        6)  # Respect the indentation of the previous line"//LF)
        call look("a = (((")
        call expect_context(error, PYTHON_INDENT_NO_INDENT)
        if (allocated(error)) return
        call expect_indent(error, 0_int64)
        if (allocated(error)) return
        shortage = forward_line(1_int64)
        call expect_context(error, PYTHON_INDENT_INSIDE_PAREN_NEWLINE_START)
        if (allocated(error)) return
        call expect_indent(error, 4_int64)
        if (allocated(error)) return
        shortage = forward_line(1_int64)
        call expect_context(error, PYTHON_INDENT_INSIDE_PAREN)
        if (allocated(error)) return
        call expect_indent(error, 6_int64)
        if (allocated(error)) return
        shortage = forward_line(1_int64)
        call expect_context(error, PYTHON_INDENT_INSIDE_PAREN)
        if (allocated(error)) return
        call expect_indent(error, 5_int64)
        if (allocated(error)) return
        shortage = forward_line(1_int64)
        call expect_context(error, PYTHON_INDENT_AFTER_LINE)
        if (allocated(error)) return
        call expect_indent(error, 0_int64)
        if (allocated(error)) return
        shortage = forward_line(1_int64)
        call expect_context(error, PYTHON_INDENT_INSIDE_PAREN_NEWLINE_START)
        if (allocated(error)) return
        call expect_indent(error, 4_int64)
        if (allocated(error)) return
        shortage = forward_line(1_int64)
        call expect_context(error, PYTHON_INDENT_INSIDE_PAREN)
        if (allocated(error)) return
        call expect_indent(error, 5_int64)
        if (allocated(error)) return
        shortage = forward_line(1_int64)
        call expect_context(error, PYTHON_INDENT_INSIDE_PAREN_CONTINUATION_LINE)
        if (allocated(error)) return
        call expect_indent(error, 5_int64)
        if (allocated(error)) return
        shortage = forward_line(1_int64)
        call expect_context(error, PYTHON_INDENT_INSIDE_PAREN_CONTINUATION_LINE)
        if (allocated(error)) return
        call expect_indent(error, 5_int64)
        if (allocated(error)) return
        shortage = forward_line(1_int64)
        call expect_context(error, PYTHON_INDENT_INSIDE_PAREN_CONTINUATION_LINE)
        if (allocated(error)) return
        call expect_indent(error, 8_int64)
        if (allocated(error)) return
        if (.false.) shortage = 0
        if (.false.) call python_indent_line(st)
    end subroutine test_python_indent_inside_paren_9

    subroutine test_python_indent_inside_paren_block_1(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage

        call py_buffer(LF &
                       //"if ('VALUE' in my_unnecessarily_long_dictionary and"//LF &
                       //"    some_other_long_condition_case):"//LF &
                       //"    do_something()"//LF &
                       //"elif (some_case or"//LF &
                       //"      another_case):"//LF &
                       //"    do_another()"//LF)
        call look("if")
        call expect_context(error, PYTHON_INDENT_NO_INDENT)
        if (allocated(error)) return
        call expect_indent(error, 0_int64)
        if (allocated(error)) return
        shortage = forward_line(1_int64)
        call expect_context(error, PYTHON_INDENT_INSIDE_PAREN_FROM_BLOCK)
        if (allocated(error)) return
        call expect_indent(error, 4_int64)
        if (allocated(error)) return
        shortage = forward_line(1_int64)
        call expect_context(error, PYTHON_INDENT_AFTER_BLOCK_START)
        if (allocated(error)) return
        call expect_indent(error, 4_int64)
        if (allocated(error)) return
        shortage = forward_line(1_int64)
        call expect_context(error, PYTHON_INDENT_AT_DEDENTER_BLOCK_START)
        if (allocated(error)) return
        call expect_indent(error, 0_int64)
        if (allocated(error)) return
        shortage = forward_line(1_int64)
        call expect_context(error, PYTHON_INDENT_INSIDE_PAREN_FROM_BLOCK)
        if (allocated(error)) return
        call expect_indent(error, 6_int64)
        if (allocated(error)) return
        shortage = forward_line(1_int64)
        call expect_context(error, PYTHON_INDENT_AFTER_BLOCK_START)
        if (allocated(error)) return
        call expect_indent(error, 4_int64)
        if (allocated(error)) return
        if (.false.) shortage = 0
        if (.false.) call python_indent_line(st)
    end subroutine test_python_indent_inside_paren_block_1

    subroutine test_python_indent_inside_paren_block_3(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage

        call py_buffer(LF &
                       //"if 'VALUE' in my_uncessarily_long_dictionary and\"//LF &
                       //"   (some_other_long_condition_case or"//LF &
                       //"    another_case):"//LF &
                       //"    do_something()"//LF)
        call look("if")
        call expect_context(error, PYTHON_INDENT_NO_INDENT)
        if (allocated(error)) return
        call expect_indent(error, 0_int64)
        if (allocated(error)) return
        shortage = forward_line(1_int64)
        call expect_context(error, PYTHON_INDENT_AFTER_BACKSLASH_BLOCK_CONTINUATION)
        if (allocated(error)) return
        call expect_indent(error, 3_int64)
        if (allocated(error)) return
        shortage = forward_line(1_int64)
        call expect_context(error, PYTHON_INDENT_INSIDE_PAREN_FROM_BLOCK)
        if (allocated(error)) return
        call expect_indent(error, 4_int64)
        if (allocated(error)) return
        shortage = forward_line(1_int64)
        call expect_context(error, PYTHON_INDENT_AFTER_BLOCK_START)
        if (allocated(error)) return
        call expect_indent(error, 4_int64)
        if (allocated(error)) return
        if (.false.) shortage = 0
        if (.false.) call python_indent_line(st)
    end subroutine test_python_indent_inside_paren_block_3

    subroutine test_python_indent_after_block_1(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage

        call py_buffer(LF &
                       //"def foo(a, b, c=True):"//LF)
        call expect_context(error, PYTHON_INDENT_NO_INDENT)
        if (allocated(error)) return
        call expect_indent(error, 0_int64)
        if (allocated(error)) return
        call goto_char(point_max())
        call expect_context(error, PYTHON_INDENT_AFTER_BLOCK_START)
        if (allocated(error)) return
        call expect_indent(error, 4_int64)
        if (allocated(error)) return
        if (.false.) shortage = 0
        if (.false.) call python_indent_line(st)
    end subroutine test_python_indent_after_block_1

    subroutine test_python_indent_after_block_2(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage

        call py_buffer(LF &
                       //"def foo(a, b, c={"//LF &
                       //"    'a':"//LF &
                       //"}):"//LF)
        call goto_char(point_max())
        call expect_context(error, PYTHON_INDENT_AFTER_BLOCK_START)
        if (allocated(error)) return
        call expect_indent(error, 4_int64)
        if (allocated(error)) return
        if (.false.) shortage = 0
        if (.false.) call python_indent_line(st)
    end subroutine test_python_indent_after_block_2

    subroutine test_python_indent_after_block_3(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage

        call py_buffer(LF &
                       //"In [1]:"//LF &
                       //"def func():"//LF &
                       //"pass"//LF &
                       //LF &
                       //"In [2]:"//LF &
                       //"something"//LF)
        call look("pass")
        call expect_context(error, PYTHON_INDENT_AFTER_BLOCK_START)
        if (allocated(error)) return
        call expect_indent(error, 4_int64)
        if (allocated(error)) return
        call look("something")
        call end_of_line()
        call expect_context(error, PYTHON_INDENT_AFTER_LINE)
        if (allocated(error)) return
        call expect_indent(error, 0_int64)
        if (allocated(error)) return
        if (.false.) shortage = 0
        if (.false.) call python_indent_line(st)
    end subroutine test_python_indent_after_block_3

    subroutine test_python_indent_after_async_block_1(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage

        call py_buffer(LF &
                       //"async def foo(a, b, c=True):"//LF)
        call expect_context(error, PYTHON_INDENT_NO_INDENT)
        if (allocated(error)) return
        call expect_indent(error, 0_int64)
        if (allocated(error)) return
        call goto_char(point_max())
        call expect_context(error, PYTHON_INDENT_AFTER_BLOCK_START)
        if (allocated(error)) return
        call expect_indent(error, 4_int64)
        if (allocated(error)) return
        if (.false.) shortage = 0
        if (.false.) call python_indent_line(st)
    end subroutine test_python_indent_after_async_block_1

    subroutine test_python_indent_after_async_block_2(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage

        call py_buffer(LF &
                       //"async with foo(a) as mgr:"//LF)
        call expect_context(error, PYTHON_INDENT_NO_INDENT)
        if (allocated(error)) return
        call expect_indent(error, 0_int64)
        if (allocated(error)) return
        call goto_char(point_max())
        call expect_context(error, PYTHON_INDENT_AFTER_BLOCK_START)
        if (allocated(error)) return
        call expect_indent(error, 4_int64)
        if (allocated(error)) return
        if (.false.) shortage = 0
        if (.false.) call python_indent_line(st)
    end subroutine test_python_indent_after_async_block_2

    subroutine test_python_indent_after_async_block_3(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage

        call py_buffer(LF &
                       //"async for a in sequencer():"//LF)
        call expect_context(error, PYTHON_INDENT_NO_INDENT)
        if (allocated(error)) return
        call expect_indent(error, 0_int64)
        if (allocated(error)) return
        call goto_char(point_max())
        call expect_context(error, PYTHON_INDENT_AFTER_BLOCK_START)
        if (allocated(error)) return
        call expect_indent(error, 4_int64)
        if (allocated(error)) return
        if (.false.) shortage = 0
        if (.false.) call python_indent_line(st)
    end subroutine test_python_indent_after_async_block_3

    subroutine test_python_indent_after_backslash_1(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage

        call py_buffer(LF &
                       //"from foo.bar.baz import something, something_1 \"//LF &
                       //"    something_2 something_3, \"//LF &
                       //"    something_4, something_5"//LF)
        call look("from foo.bar.baz import something, something_1")
        call expect_context(error, PYTHON_INDENT_NO_INDENT)
        if (allocated(error)) return
        call expect_indent(error, 0_int64)
        if (allocated(error)) return
        call look("something_2 something_3,")
        call expect_context(error, PYTHON_INDENT_AFTER_BACKSLASH_FIRST_LINE)
        if (allocated(error)) return
        call expect_indent(error, 4_int64)
        if (allocated(error)) return
        call look("something_4, something_5")
        call expect_context(error, PYTHON_INDENT_AFTER_BACKSLASH)
        if (allocated(error)) return
        call expect_indent(error, 4_int64)
        if (allocated(error)) return
        call goto_char(point_max())
        call expect_context(error, PYTHON_INDENT_AFTER_LINE)
        if (allocated(error)) return
        call expect_indent(error, 0_int64)
        if (allocated(error)) return
        if (.false.) shortage = 0
        if (.false.) call python_indent_line(st)
    end subroutine test_python_indent_after_backslash_1

    subroutine test_python_indent_after_backslash_2(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage

        call py_buffer(LF &
                       //"objects = Thing.objects.all() \"//LF &
                       //"                       .filter("//LF &
                       //"                           type='toy',"//LF &
                       //"                           status='bought'"//LF &
                       //"                       ) \"//LF &
                       //"                       .aggregate("//LF &
                       //"                           Sum('amount')"//LF &
                       //"                       ) \"//LF &
                       //"                       .values_list()"//LF)
        call look("objects = Thing.objects.all()")
        call expect_context(error, PYTHON_INDENT_NO_INDENT)
        if (allocated(error)) return
        call expect_indent(error, 0_int64)
        if (allocated(error)) return
        call look(".filter(")
        call expect_context(error, PYTHON_INDENT_AFTER_BACKSLASH_DOTTED_CONTINUATION)
        if (allocated(error)) return
        call expect_indent(error, 23_int64)
        if (allocated(error)) return
        call look("type='toy',")
        call expect_context(error, PYTHON_INDENT_INSIDE_PAREN_NEWLINE_START)
        if (allocated(error)) return
        call expect_indent(error, 27_int64)
        if (allocated(error)) return
        call look("status='bought'")
        call expect_context(error, PYTHON_INDENT_INSIDE_PAREN_CONTINUATION_LINE)
        if (allocated(error)) return
        call expect_indent(error, 27_int64)
        if (allocated(error)) return
        call look(") \")
        call expect_context(error, PYTHON_INDENT_INSIDE_PAREN_AT_CLOSING_PAREN)
        if (allocated(error)) return
        call expect_indent(error, 23_int64)
        if (allocated(error)) return
        call look(".aggregate(")
        call expect_context(error, PYTHON_INDENT_AFTER_BACKSLASH_DOTTED_CONTINUATION)
        if (allocated(error)) return
        call expect_indent(error, 23_int64)
        if (allocated(error)) return
        call look("Sum('amount')")
        call expect_context(error, PYTHON_INDENT_INSIDE_PAREN_NEWLINE_START)
        if (allocated(error)) return
        call expect_indent(error, 27_int64)
        if (allocated(error)) return
        call look(") \")
        call expect_context(error, PYTHON_INDENT_INSIDE_PAREN_AT_CLOSING_PAREN)
        if (allocated(error)) return
        call expect_indent(error, 23_int64)
        if (allocated(error)) return
        call look(".values_list()")
        call expect_context(error, PYTHON_INDENT_AFTER_BACKSLASH_DOTTED_CONTINUATION)
        if (allocated(error)) return
        call expect_indent(error, 23_int64)
        if (allocated(error)) return
        shortage = forward_line(1_int64)
        call expect_context(error, PYTHON_INDENT_AFTER_LINE)
        if (allocated(error)) return
        call expect_indent(error, 0_int64)
        if (allocated(error)) return
        if (.false.) shortage = 0
        if (.false.) call python_indent_line(st)
    end subroutine test_python_indent_after_backslash_2

    subroutine test_python_indent_after_backslash_3(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage

        call py_buffer(LF &
                       //"with open('/path/to/some/file/you/want/to/read') as file_1, \"//LF &
                       //"     open('/path/to/some/file/being/written', 'w') as file_2:"//LF &
                       //"    file_2.write(file_1.read())"//LF)
        call look("with open('/path/to/some/file/you/want/to/read') as file_1, \")
        call expect_context(error, PYTHON_INDENT_NO_INDENT)
        if (allocated(error)) return
        call expect_indent(error, 0_int64)
        if (allocated(error)) return
        call look("open('/path/to/some/file/being/written', 'w') as file_2")
        call expect_context(error, PYTHON_INDENT_AFTER_BACKSLASH_BLOCK_CONTINUATION)
        if (allocated(error)) return
        call expect_indent(error, 5_int64)
        if (allocated(error)) return
        call look("file_2.write(file_1.read())")
        call expect_context(error, PYTHON_INDENT_AFTER_BLOCK_START)
        if (allocated(error)) return
        call expect_indent(error, 4_int64)
        if (allocated(error)) return
        if (.false.) shortage = 0
        if (.false.) call python_indent_line(st)
    end subroutine test_python_indent_after_backslash_3

    subroutine test_python_indent_after_backslash_5(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage

        call py_buffer(LF &
                       //"def delete_all_things():"//LF &
                       //"    Thing \"//LF &
                       //"        .objects.all() \"//LF &
                       //"                .delete()"//LF)
        call look("Thing \")
        call expect_context(error, PYTHON_INDENT_AFTER_BLOCK_START)
        if (allocated(error)) return
        call expect_indent(error, 4_int64)
        if (allocated(error)) return
        call look(".objects.all() \")
        call expect_context(error, PYTHON_INDENT_AFTER_BACKSLASH_FIRST_LINE)
        if (allocated(error)) return
        call expect_indent(error, 8_int64)
        if (allocated(error)) return
        call look(".delete()")
        call expect_context(error, PYTHON_INDENT_AFTER_BACKSLASH_DOTTED_CONTINUATION)
        if (allocated(error)) return
        call expect_indent(error, 16_int64)
        if (allocated(error)) return
        if (.false.) shortage = 0
        if (.false.) call python_indent_line(st)
    end subroutine test_python_indent_after_backslash_5

    subroutine test_python_indent_after_backslash_6(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage

        call py_buffer(LF &
                       //"for long_variable_name \"//LF &
                       //"        in (1, 2):"//LF &
                       //"    print(long_variable_name)"//LF)
        call look("for long_variable_name \")
        call expect_context(error, PYTHON_INDENT_NO_INDENT)
        if (allocated(error)) return
        call expect_indent(error, 0_int64)
        if (allocated(error)) return
        call look("in (1, 2):")
        call expect_context(error, PYTHON_INDENT_AFTER_BACKSLASH_BLOCK_CONTINUATION)
        if (allocated(error)) return
        call expect_indent(error, 8_int64)
        if (allocated(error)) return
        call look("print(long_variable_name)")
        call expect_context(error, PYTHON_INDENT_AFTER_BLOCK_START)
        if (allocated(error)) return
        call expect_indent(error, 4_int64)
        if (allocated(error)) return
        if (.false.) shortage = 0
        if (.false.) call python_indent_line(st)
    end subroutine test_python_indent_after_backslash_6

    subroutine test_python_indent_block_enders_1(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage

        call py_buffer(LF &
                       //"Class foo(object):"//LF &
                       //LF &
                       //"    def bar(self):"//LF &
                       //"        if self.baz:"//LF &
                       //"            return (1,"//LF &
                       //"                    2,"//LF &
                       //"                    3)"//LF &
                       //LF &
                       //"        else:"//LF &
                       //"            pass"//LF)
        call look("3)")
        shortage = forward_line(1_int64)
        call expect_indent(error, 8_int64)
        if (allocated(error)) return
        call look("pass")
        shortage = forward_line(1_int64)
        call expect_context(error, PYTHON_INDENT_AFTER_BLOCK_END)
        if (allocated(error)) return
        call expect_indent(error, 8_int64)
        if (allocated(error)) return
        if (.false.) shortage = 0
        if (.false.) call python_indent_line(st)
    end subroutine test_python_indent_block_enders_1

    subroutine test_python_indent_block_enders_2(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage

        call py_buffer(LF &
                       //"Class foo(object):"//LF &
                       //"    '''raise lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do"//LF &
                       //LF &
                       //"    eiusmod tempor incididunt ut labore et dolore magna aliqua."//LF &
                       //"    '''"//LF &
                       //"    def bar(self):"//LF &
                       //"        ""return (1, 2, 3)."""//LF &
                       //"        if self.baz:"//LF &
                       //"            return (1,"//LF &
                       //"                    2,"//LF &
                       //"                    3)"//LF)
        call look("def")
        call expect_indent(error, 4_int64)
        if (allocated(error)) return
        call look("if")
        call expect_indent(error, 8_int64)
        if (allocated(error)) return
        call look("return")
        call expect_indent(error, 12_int64)
        if (allocated(error)) return
        call goto_char(point_max())
        call expect_context(error, PYTHON_INDENT_AFTER_BLOCK_END)
        if (allocated(error)) return
        call expect_indent(error, 8_int64)
        if (allocated(error)) return
        if (.false.) shortage = 0
        if (.false.) call python_indent_line(st)
    end subroutine test_python_indent_block_enders_2

    subroutine test_python_indent_block_enders_3(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage

        call py_buffer(LF &
                       //"for element in lst:"//LF &
                       //"    if element is None:"//LF &
                       //"        continue"//LF)
        call look("if")
        call expect_indent(error, 4_int64)
        if (allocated(error)) return
        call look("continue")
        call expect_indent(error, 8_int64)
        if (allocated(error)) return
        shortage = forward_line(1_int64)
        call expect_context(error, PYTHON_INDENT_AFTER_BLOCK_END)
        if (allocated(error)) return
        call expect_indent(error, 4_int64)
        if (allocated(error)) return
        if (.false.) shortage = 0
        if (.false.) call python_indent_line(st)
    end subroutine test_python_indent_block_enders_3

    subroutine test_python_indent_block_enders_4(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage

        call py_buffer(LF &
                       //"for element in lst:"//LF &
                       //"    if element is None:"//LF &
                       //"        break"//LF)
        call look("if")
        call expect_indent(error, 4_int64)
        if (allocated(error)) return
        call look("break")
        call expect_indent(error, 8_int64)
        if (allocated(error)) return
        shortage = forward_line(1_int64)
        call expect_context(error, PYTHON_INDENT_AFTER_BLOCK_END)
        if (allocated(error)) return
        call expect_indent(error, 4_int64)
        if (allocated(error)) return
        if (.false.) shortage = 0
        if (.false.) call python_indent_line(st)
    end subroutine test_python_indent_block_enders_4

    subroutine test_python_indent_block_enders_5(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage

        call py_buffer(LF &
                       //"for element in lst:"//LF &
                       //"    if element is None:"//LF &
                       //"        raise ValueError('Element cannot be None')"//LF)
        call look("if")
        call expect_indent(error, 4_int64)
        if (allocated(error)) return
        call look("raise")
        call expect_indent(error, 8_int64)
        if (allocated(error)) return
        shortage = forward_line(1_int64)
        call expect_context(error, PYTHON_INDENT_AFTER_BLOCK_END)
        if (allocated(error)) return
        call expect_indent(error, 4_int64)
        if (allocated(error)) return
        if (.false.) shortage = 0
        if (.false.) call python_indent_line(st)
    end subroutine test_python_indent_block_enders_5

    subroutine test_python_indent_dedenters_1(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage

        call py_buffer(LF &
                       //"if save:"//LF &
                       //"    try:"//LF &
                       //"        write_to_disk(data)"//LF &
                       //"    finally:"//LF &
                       //"        cleanup()"//LF &
                       //"        elif"//LF)
        call look("elif"//LF)
        call expect_context(error, PYTHON_INDENT_AT_DEDENTER_BLOCK_START)
        if (allocated(error)) return
        call expect_indent(error, 0_int64)
        if (allocated(error)) return
        call expect_indent(error, 0_int64, .true.)
        if (allocated(error)) return
        if (.false.) shortage = 0
        if (.false.) call python_indent_line(st)
    end subroutine test_python_indent_dedenters_1

    subroutine test_python_indent_dedenters_2(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage

        call py_buffer(LF &
                       //"if save:"//LF &
                       //"    try:"//LF &
                       //"        write_to_disk(data)"//LF &
                       //"    except IOError:"//LF &
                       //"        msg = 'Error saving to disk'"//LF &
                       //"        message(msg)"//LF &
                       //"        logger.exception(msg)"//LF &
                       //"    except Exception:"//LF &
                       //"        if hide_details:"//LF &
                       //"            logger.exception('Unhandled exception')"//LF &
                       //"            else"//LF &
                       //"    finally:"//LF &
                       //"        data.free()"//LF)
        call look("else"//LF)
        call expect_context(error, PYTHON_INDENT_AT_DEDENTER_BLOCK_START)
        if (allocated(error)) return
        call expect_indent(error, 8_int64)
        if (allocated(error)) return
        call python_indent_line(st, .true.)
        call expect_indent(error, 4_int64, .true.)
        if (allocated(error)) return
        call python_indent_line(st, .true.)
        call expect_indent(error, 0_int64, .true.)
        if (allocated(error)) return
        call python_indent_line(st, .true.)
        call expect_indent(error, 8_int64, .true.)
        if (allocated(error)) return
        if (.false.) shortage = 0
        if (.false.) call python_indent_line(st)
    end subroutine test_python_indent_dedenters_2

    subroutine test_python_indent_dedenters_comment_else(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage

        call py_buffer(LF &
                       //"if save:"//LF &
                       //"    try:"//LF &
                       //"        write_to_disk(data)"//LF &
                       //"    except IOError:"//LF &
                       //"        msg = 'Error saving to disk'"//LF &
                       //"        message(msg)"//LF &
                       //"        logger.exception(msg)"//LF &
                       //"    except Exception:"//LF &
                       //"        if hide_details:"//LF &
                       //"            logger.exception('Unhandled exception')"//LF &
                       //"        # comment"//LF &
                       //"            else"//LF &
                       //"    finally:"//LF &
                       //"        data.free()"//LF)
        call look("else"//LF)
        call expect_context(error, PYTHON_INDENT_AT_DEDENTER_BLOCK_START)
        if (allocated(error)) return
        call expect_indent(error, 8_int64)
        if (allocated(error)) return
        call python_indent_line(st, .true.)
        call expect_indent(error, 4_int64, .true.)
        if (allocated(error)) return
        call python_indent_line(st, .true.)
        call expect_indent(error, 0_int64, .true.)
        if (allocated(error)) return
        call python_indent_line(st, .true.)
        call expect_indent(error, 8_int64, .true.)
        if (allocated(error)) return
        if (.false.) shortage = 0
        if (.false.) call python_indent_line(st)
    end subroutine test_python_indent_dedenters_comment_else

    subroutine test_python_indent_dedenters_3(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage

        call py_buffer(LF &
                       //"if save:"//LF &
                       //"    try:"//LF &
                       //"        write_to_disk(data)"//LF &
                       //"        except"//LF)
        call look("except"//LF)
        call expect_context(error, PYTHON_INDENT_AT_DEDENTER_BLOCK_START)
        if (allocated(error)) return
        call expect_indent(error, 4_int64)
        if (allocated(error)) return
        call python_indent_line(st, .true.)
        call expect_indent(error, 4_int64, .true.)
        if (allocated(error)) return
        if (.false.) shortage = 0
        if (.false.) call python_indent_line(st)
    end subroutine test_python_indent_dedenters_3

    subroutine test_python_indent_dedenters_4(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage

        call py_buffer(LF &
                       //"if save:"//LF &
                       //"    try:"//LF &
                       //"        write_to_disk(data)"//LF &
                       //"        finally"//LF)
        call look("finally"//LF)
        call expect_context(error, PYTHON_INDENT_AT_DEDENTER_BLOCK_START)
        if (allocated(error)) return
        call expect_indent(error, 4_int64)
        if (allocated(error)) return
        call python_indent_line(st, .true.)
        call expect_indent(error, 4_int64)
        if (allocated(error)) return
        if (.false.) shortage = 0
        if (.false.) call python_indent_line(st)
    end subroutine test_python_indent_dedenters_4

    subroutine test_python_indent_dedenters_5(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage

        call py_buffer(LF &
                       //"if save:"//LF &
                       //"    try:"//LF &
                       //"        write_to_disk(data)"//LF &
                       //"    except IOError:"//LF &
                       //"        msg = 'Error saving to disk'"//LF &
                       //"        message(msg)"//LF &
                       //"        logger.exception(msg)"//LF &
                       //"    finally:"//LF &
                       //"        if cleanup:"//LF &
                       //"            do_cleanup()"//LF &
                       //"        else"//LF)
        call look("else"//LF)
        call expect_context(error, PYTHON_INDENT_AT_DEDENTER_BLOCK_START)
        if (allocated(error)) return
        call expect_indent(error, 8_int64)
        if (allocated(error)) return
        call expect_indent(error, 0_int64, .true.)
        if (allocated(error)) return
        call python_indent_line(st, .true.)
        call expect_indent(error, 8_int64, .true.)
        if (allocated(error)) return
        if (.false.) shortage = 0
        if (.false.) call python_indent_line(st)
    end subroutine test_python_indent_dedenters_5

    subroutine test_python_indent_dedenters_6(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage

        call py_buffer(LF &
                       //"try:"//LF &
                       //"    # if save:"//LF &
                       //"        write_to_disk(data)"//LF &
                       //"        else"//LF)
        call look("else"//LF)
        call expect_context(error, PYTHON_INDENT_AT_DEDENTER_BLOCK_START)
        if (allocated(error)) return
        call expect_indent(error, 0_int64)
        if (allocated(error)) return
        call expect_indent(error, 0_int64, .true.)
        if (allocated(error)) return
        if (.false.) shortage = 0
        if (.false.) call python_indent_line(st)
    end subroutine test_python_indent_dedenters_6

    subroutine test_python_indent_dedenters_7(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage

        call py_buffer(LF &
                       //"if a:"//LF &
                       //"    if b:"//LF &
                       //"        pass"//LF &
                       //"    else:"//LF &
                       //"        pass"//LF &
                       //"        else:"//LF)
        call look("else:", 2_int64)
        call expect_context(error, PYTHON_INDENT_AT_DEDENTER_BLOCK_START)
        if (allocated(error)) return
        call expect_indent(error, 0_int64)
        if (allocated(error)) return
        call expect_indent(error, 0_int64, .true.)
        if (allocated(error)) return
        if (.false.) shortage = 0
        if (.false.) call python_indent_line(st)
    end subroutine test_python_indent_dedenters_7

    subroutine test_python_indent_dedenters_8(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage

        call py_buffer(LF &
                       //"if (a == 1 or"//LF &
                       //"    a == 2):"//LF &
                       //"    pass"//LF &
                       //"elif (a == 3 or"//LF &
                       //"a == 4):"//LF)
        call look("elif (a == 3 or")
        call expect_context(error, PYTHON_INDENT_AT_DEDENTER_BLOCK_START)
        if (allocated(error)) return
        call expect_indent(error, 0_int64)
        if (allocated(error)) return
        call expect_indent(error, 0_int64, .true.)
        if (allocated(error)) return
        call look("a == 4):"//LF)
        call expect_context(error, PYTHON_INDENT_INSIDE_PAREN_FROM_BLOCK)
        if (allocated(error)) return
        call expect_indent(error, 6_int64)
        if (allocated(error)) return
        call python_indent_line(st)
        call expect_indent(error, 4_int64, .true.)
        if (allocated(error)) return
        call python_indent_line(st, .true.)
        call expect_indent(error, 0_int64, .true.)
        if (allocated(error)) return
        call python_indent_line(st, .true.)
        call expect_indent(error, 6_int64, .true.)
        if (allocated(error)) return
        if (.false.) shortage = 0
        if (.false.) call python_indent_line(st)
    end subroutine test_python_indent_dedenters_8

    subroutine test_python_indent_dedenters_9(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage

        call py_buffer(LF &
                       //"match a:"//LF &
                       //"    case 1:"//LF &
                       //"        print(1)"//LF &
                       //"        case 2"//LF)
        call look("case 2")
        call expect_context(error, PYTHON_INDENT_AT_DEDENTER_BLOCK_START)
        if (allocated(error)) return
        call expect_indent(error, 4_int64)
        if (allocated(error)) return
        call python_indent_line(st, .true.)
        call expect_indent(error, 4_int64, .true.)
        if (allocated(error)) return
        if (.false.) shortage = 0
        if (.false.) call python_indent_line(st)
    end subroutine test_python_indent_dedenters_9

    subroutine test_python_indent_inside_string_1(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage

        call py_buffer(LF &
                       //"multiline = '''"//LF &
                       //"bunch"//LF &
                       //"of"//LF &
                       //"lines"//LF &
                       //"'''"//LF)
        call look("multiline = '''")
        call expect_context(error, PYTHON_INDENT_NO_INDENT)
        if (allocated(error)) return
        call expect_indent(error, 0_int64)
        if (allocated(error)) return
        call look("bunch")
        call expect_context(error, PYTHON_INDENT_INSIDE_STRING)
        if (allocated(error)) return
        call expect_indent(error, 0_int64)
        if (allocated(error)) return
        call look("of")
        call expect_context(error, PYTHON_INDENT_INSIDE_STRING)
        if (allocated(error)) return
        call expect_indent(error, 0_int64)
        if (allocated(error)) return
        call look("lines")
        call expect_context(error, PYTHON_INDENT_INSIDE_STRING)
        if (allocated(error)) return
        call expect_indent(error, 0_int64)
        if (allocated(error)) return
        call look("'''")
        call expect_context(error, PYTHON_INDENT_INSIDE_STRING)
        if (allocated(error)) return
        call expect_indent(error, 0_int64)
        if (allocated(error)) return
        if (.false.) shortage = 0
        if (.false.) call python_indent_line(st)
    end subroutine test_python_indent_inside_string_1

    subroutine test_python_indent_inside_string_2(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage

        call py_buffer(LF &
                       //"def fn(a, b, c=True):"//LF &
                       //"    '''docstring"//LF &
                       //"    bunch"//LF &
                       //"        of"//LF &
                       //"    lines"//LF &
                       //"    '''"//LF)
        call look("'''docstring")
        call expect_context(error, PYTHON_INDENT_AFTER_BLOCK_START)
        if (allocated(error)) return
        call expect_indent(error, 4_int64)
        if (allocated(error)) return
        call look("bunch")
        call expect_context(error, PYTHON_INDENT_INSIDE_DOCSTRING)
        if (allocated(error)) return
        call expect_indent(error, 4_int64)
        if (allocated(error)) return
        call look("of")
        call expect_context(error, PYTHON_INDENT_INSIDE_DOCSTRING)
        if (allocated(error)) return
        call expect_indent(error, 8_int64)
        if (allocated(error)) return
        call look("lines")
        call expect_context(error, PYTHON_INDENT_INSIDE_DOCSTRING)
        if (allocated(error)) return
        call expect_indent(error, 4_int64)
        if (allocated(error)) return
        call look("'''")
        call expect_context(error, PYTHON_INDENT_INSIDE_DOCSTRING)
        if (allocated(error)) return
        call expect_indent(error, 4_int64)
        if (allocated(error)) return
        if (.false.) shortage = 0
        if (.false.) call python_indent_line(st)
    end subroutine test_python_indent_inside_string_2

    subroutine test_python_indent_inside_string_3(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage

        call py_buffer(LF &
                       //"def fn(a, b, c=True):"//LF &
                       //"    some_var = '''"//LF &
                       //"    bunch"//LF &
                       //"    of"//LF &
                       //"    lines"//LF &
                       //"    '''"//LF)
        call look("some_var = '''")
        call expect_context(error, PYTHON_INDENT_AFTER_BLOCK_START)
        if (allocated(error)) return
        call expect_indent(error, 4_int64)
        if (allocated(error)) return
        call look("bunch")
        call expect_context(error, PYTHON_INDENT_INSIDE_STRING)
        if (allocated(error)) return
        call expect_indent(error, 4_int64)
        if (allocated(error)) return
        call look("of")
        call expect_context(error, PYTHON_INDENT_INSIDE_STRING)
        if (allocated(error)) return
        call expect_indent(error, 4_int64)
        if (allocated(error)) return
        call look("lines")
        call expect_context(error, PYTHON_INDENT_INSIDE_STRING)
        if (allocated(error)) return
        call expect_indent(error, 4_int64)
        if (allocated(error)) return
        call look("'''")
        call expect_context(error, PYTHON_INDENT_INSIDE_STRING)
        if (allocated(error)) return
        call expect_indent(error, 4_int64)
        if (allocated(error)) return
        if (.false.) shortage = 0
        if (.false.) call python_indent_line(st)
    end subroutine test_python_indent_inside_string_3

    subroutine test_python_indent_after_match_block(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage

        call py_buffer(LF &
                       //"match foo:"//LF)
        call expect_context(error, PYTHON_INDENT_NO_INDENT)
        if (allocated(error)) return
        call expect_indent(error, 0_int64)
        if (allocated(error)) return
        call goto_char(point_max())
        call expect_context(error, PYTHON_INDENT_AFTER_BLOCK_START)
        if (allocated(error)) return
        call expect_indent(error, 4_int64)
        if (allocated(error)) return
        if (.false.) shortage = 0
        if (.false.) call python_indent_line(st)
    end subroutine test_python_indent_after_match_block

    subroutine test_python_indent_after_case_block(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage

        call py_buffer(LF &
                       //"match foo:"//LF &
                       //"    case 1:"//LF)
        call expect_context(error, PYTHON_INDENT_NO_INDENT)
        if (allocated(error)) return
        call expect_indent(error, 0_int64)
        if (allocated(error)) return
        call goto_char(point_max())
        call expect_context(error, PYTHON_INDENT_AFTER_BLOCK_START)
        if (allocated(error)) return
        call expect_indent(error, 8_int64)
        if (allocated(error)) return
        if (.false.) shortage = 0
        if (.false.) call python_indent_line(st)
    end subroutine test_python_indent_after_case_block

    subroutine test_python_indent_after_re_match(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage

        call py_buffer(LF &
                       //"def test_re(string):"//LF &
                       //"    if re.match('^[a-c]+$', string):"//LF &
                       //"        print('yes')"//LF &
                       //"    else:"//LF &
                       //"    ")
        call look("else:")
        call expect_indent(error, 4_int64)
        if (allocated(error)) return
        if (.false.) shortage = 0
        if (.false.) call python_indent_line(st)
    end subroutine test_python_indent_after_re_match

    subroutine test_python_indent_after_bare_match(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage

        call py_buffer(LF &
                       //"from re import match"//LF &
                       //LF &
                       //"def test_re(string):"//LF &
                       //"    if match('^[a-c]+$', string):"//LF &
                       //"        print('yes')"//LF &
                       //"    else:"//LF &
                       //"    ")
        call look("else:")
        call expect_indent(error, 4_int64)
        if (allocated(error)) return
        if (.false.) shortage = 0
        if (.false.) call python_indent_line(st)
    end subroutine test_python_indent_after_bare_match

    subroutine test_python_indent_badly_indented_block_end(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage

        call py_buffer(LF &
                       //"return"//LF)
        call goto_char(point_max())
        call expect_indent(error, 0_int64)
        if (allocated(error)) return
        if (.false.) shortage = 0
        if (.false.) call python_indent_line(st)
    end subroutine test_python_indent_badly_indented_block_end

    subroutine test_python_syntax_context_1(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage, v(300)
        logical :: b(300)
        character(len=:), allocatable :: name

        call py_buffer(LF &
                       //"# Comment"//LF &
                       //"s = 'Single Quoted String'"//LF &
                       //"t = '''Triple Quoted String'''"//LF &
                       //"p = (1 + 2)"//LF)
        v(1) = look_pos("Comment", 1_int64, .false.)
        v(2) = python_syntax_context(PYTHON_CONTEXT_COMMENT)
        v(3) = line_beginning_position()
        call check(error, v(2) == v(3), "(= (python-syntax-context 'comment) (pos-bol))")
        if (allocated(error)) return
        v(4) = look_pos("Single", 1_int64, .false.)
        v(5) = python_syntax_context(PYTHON_CONTEXT_STRING)
        v(6) = point()
        v(7) = v(6) - 1
        call check(error, v(5) == v(7), "(= (python-syntax-context 'string) (1- (point)))")
        if (allocated(error)) return
        v(8) = python_syntax_context(PYTHON_CONTEXT_SINGLE_QUOTED_STRING)
        v(9) = point()
        v(10) = v(9) - 1
        call check(error, v(8) == v(10), "(= (python-syntax-context 'single-quoted-string) (1- (point)")
        if (allocated(error)) return
        v(11) = python_syntax_context(PYTHON_CONTEXT_TRIPLE_QUOTED_STRING)
        call check(error,.not. (v(11) /= 0), "(not (python-syntax-context 'triple-quoted-string))")
        if (allocated(error)) return
        v(12) = look_pos("Triple", 1_int64, .false.)
        v(13) = python_syntax_context(PYTHON_CONTEXT_STRING)
        v(14) = point()
        v(15) = v(14) - 1
        call check(error, v(13) == v(15), "(= (python-syntax-context 'string) (1- (point)))")
        if (allocated(error)) return
        v(16) = python_syntax_context(PYTHON_CONTEXT_SINGLE_QUOTED_STRING)
        call check(error,.not. (v(16) /= 0), "(not (python-syntax-context 'single-quoted-string))")
        if (allocated(error)) return
        v(17) = python_syntax_context(PYTHON_CONTEXT_TRIPLE_QUOTED_STRING)
        v(18) = point()
        v(19) = v(18) - 1
        call check(error, v(17) == v(19), "(= (python-syntax-context 'triple-quoted-string) (1- (point)")
        if (allocated(error)) return
        v(20) = look_pos("1 + 2", 1_int64, .false.)
        v(21) = python_syntax_context(PYTHON_CONTEXT_PAREN)
        v(22) = point()
        v(23) = v(22) - 1
        call check(error, v(21) == v(23), "(= (python-syntax-context 'paren) (1- (point)))")
        if (allocated(error)) return
        if (.false.) shortage = v(1) + merge(1, 0, b(1))
        if (.false.) name = ""
    end subroutine test_python_syntax_context_1

    subroutine test_python_nav_beginning_of_definition_1(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage, v(300)
        logical :: b(300)
        character(len=:), allocatable :: name

        call py_buffer(LF &
                       //"def decoratorFunctionWithArguments(arg1, arg2, arg3):"//LF &
                       //"    '''print decorated function call data to stdout."//LF &
                       //LF &
                       //"    Usage:"//LF &
                       //LF &
                       //"    @decoratorFunctionWithArguments('arg1', 'arg2')"//LF &
                       //"    def func(a, b, c=True):"//LF &
                       //"        pass"//LF &
                       //"    '''"//LF &
                       //LF &
                       //"    def wwrap(f):"//LF &
                       //"        print 'Inside wwrap()'"//LF &
                       //"        def wrapped_f(*args):"//LF &
                       //"            print 'Inside wrapped_f()'"//LF &
                       //"            print 'Decorator arguments:', arg1, arg2, arg3"//LF &
                       //"            f(*args)"//LF &
                       //"            print 'After f(*args)'"//LF &
                       //"        return wrapped_f"//LF &
                       //"    return wwrap"//LF)
        v(1) = look_pos("return wwrap", 1_int64, .false.)
        v(2) = point()
        call python_nav_beginning_of_definition(1_int64, b(1), st)
        v(3) = point()
        v(4) = v(3)
        call goto_char(v(2))
        v(5) = point()
        v(6) = look_pos("def decoratorFunctionWithArguments", -1_int64, .false.)
        call beginning_of_line()
        v(7) = point()
        v(8) = v(7)
        call goto_char(v(5))
        call check(error, v(4) == v(8), "(= (save-excursion (python-nav-beginning-of-defun) (point)) ")
        if (allocated(error)) return
        v(9) = look_pos("return wrap", -1_int64, .false.)
        v(10) = point()
        call python_nav_beginning_of_definition(1_int64, b(2), st)
        v(11) = point()
        v(12) = v(11)
        call goto_char(v(10))
        v(13) = point()
        v(14) = look_pos("def wwrap(f):", -1_int64, .false.)
        call beginning_of_line()
        v(15) = point()
        v(16) = v(15)
        call goto_char(v(13))
        call check(error, v(12) == v(16), "(= (save-excursion (python-nav-beginning-of-defun) (point)) ")
        if (allocated(error)) return
        v(17) = look_pos("def wrapped_f(*args):", -1_int64, .false.)
        v(18) = point()
        call python_nav_beginning_of_definition(1_int64, b(3), st)
        v(19) = point()
        v(20) = v(19)
        call goto_char(v(18))
        v(21) = point()
        v(22) = look_pos("def wwrap(f):", -1_int64, .false.)
        call beginning_of_line()
        v(23) = point()
        v(24) = v(23)
        call goto_char(v(21))
        call check(error, v(20) == v(24), "(= (save-excursion (python-nav-beginning-of-defun) (point)) ")
        if (allocated(error)) return
        v(25) = look_pos("def wwrap(f):", -1_int64, .false.)
        v(26) = point()
        call python_nav_beginning_of_definition(1_int64, b(4), st)
        v(27) = point()
        v(28) = v(27)
        call goto_char(v(26))
        v(29) = point()
        v(30) = look_pos("def decoratorFunctionWithArguments", -1_int64, .false.)
        call beginning_of_line()
        v(31) = point()
        v(32) = v(31)
        call goto_char(v(29))
        call check(error, v(28) == v(32), "(= (save-excursion (python-nav-beginning-of-defun) (point)) ")
        if (allocated(error)) return
        if (.false.) shortage = v(1) + merge(1, 0, b(1))
        if (.false.) name = ""
    end subroutine test_python_nav_beginning_of_definition_1

    subroutine test_python_nav_beginning_of_definition_2(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage, v(300)
        logical :: b(300)
        character(len=:), allocatable :: name

        call py_buffer(LF &
                       //"class C(object):"//LF &
                       //LF &
                       //"    def m(self):"//LF &
                       //"        self.c()"//LF &
                       //LF &
                       //"        def b():"//LF &
                       //"            pass"//LF &
                       //LF &
                       //"        def a():"//LF &
                       //"            pass"//LF &
                       //LF &
                       //"        if True:"//LF &
                       //"            return a"//LF &
                       //LF &
                       //"    def c(self):"//LF &
                       //"        pass"//LF)
        v(1) = look_pos("def c(self):", 1_int64, .false.)
        v(2) = point()
        call python_nav_beginning_of_definition(1_int64, b(1), st)
        v(3) = point()
        v(4) = v(3)
        call goto_char(v(2))
        v(5) = point()
        v(6) = look_pos("def m(self):", -1_int64, .false.)
        call beginning_of_line()
        v(7) = point()
        v(8) = v(7)
        call goto_char(v(5))
        call check(error, v(4) == v(8), "(= (save-excursion (python-nav-beginning-of-defun) (point)) ")
        if (allocated(error)) return
        shortage = forward_line(-1_int64)
        v(9) = point()
        call python_nav_beginning_of_definition(1_int64, b(2), st)
        v(10) = point()
        v(11) = v(10)
        call goto_char(v(9))
        v(12) = point()
        v(13) = look_pos("def m(self):", -1_int64, .false.)
        call beginning_of_line()
        v(14) = point()
        v(15) = v(14)
        call goto_char(v(12))
        call check(error, v(11) == v(15), "(= (save-excursion (python-nav-beginning-of-defun) (point)) ")
        if (allocated(error)) return
        v(16) = look_pos("if True:", -1_int64, .false.)
        shortage = forward_line(-1_int64)
        v(17) = point()
        call python_nav_beginning_of_definition(1_int64, b(3), st)
        v(18) = point()
        v(19) = v(18)
        call goto_char(v(17))
        v(20) = point()
        v(21) = look_pos("def a():", -1_int64, .false.)
        call beginning_of_line()
        v(22) = point()
        v(23) = v(22)
        call goto_char(v(20))
        call check(error, v(19) == v(23), "(= (save-excursion (python-nav-beginning-of-defun) (point)) ")
        if (allocated(error)) return
        v(24) = look_pos("def a():", -1_int64, .false.)
        v(25) = point()
        call python_nav_beginning_of_definition(1_int64, b(4), st)
        v(26) = point()
        v(27) = v(26)
        call goto_char(v(25))
        v(28) = point()
        v(29) = look_pos("def b():", -1_int64, .false.)
        call beginning_of_line()
        v(30) = point()
        v(31) = v(30)
        call goto_char(v(28))
        call check(error, v(27) == v(31), "(= (save-excursion (python-nav-beginning-of-defun) (point)) ")
        if (allocated(error)) return
        v(32) = look_pos("def b():", -1_int64, .false.)
        v(33) = point()
        call python_nav_beginning_of_definition(1_int64, b(5), st)
        v(34) = point()
        v(35) = v(34)
        call goto_char(v(33))
        v(36) = point()
        v(37) = look_pos("def m(self):", -1_int64, .false.)
        call beginning_of_line()
        v(38) = point()
        v(39) = v(38)
        call goto_char(v(36))
        call check(error, v(35) == v(39), "(= (save-excursion (python-nav-beginning-of-defun) (point)) ")
        if (allocated(error)) return
        shortage = forward_line(-1_int64)
        v(40) = point()
        call python_nav_beginning_of_definition(1_int64, b(6), st)
        v(41) = point()
        v(42) = v(41)
        call goto_char(v(40))
        v(43) = point()
        v(44) = look_pos("def m(self):", -1_int64, .false.)
        call beginning_of_line()
        v(45) = point()
        v(46) = v(45)
        call goto_char(v(43))
        call check(error, v(42) == v(46), "(= (save-excursion (python-nav-beginning-of-defun) (point)) ")
        if (allocated(error)) return
        v(47) = look_pos("def m(self):", -1_int64, .false.)
        v(48) = point()
        call python_nav_beginning_of_definition(1_int64, b(7), st)
        v(49) = point()
        v(50) = v(49)
        call goto_char(v(48))
        v(51) = point()
        v(52) = look_pos("class C(object):", -1_int64, .false.)
        call beginning_of_line()
        v(53) = point()
        v(54) = v(53)
        call goto_char(v(51))
        call check(error, v(50) == v(54), "(= (save-excursion (python-nav-beginning-of-defun) (point)) ")
        if (allocated(error)) return
        if (.false.) shortage = v(1) + merge(1, 0, b(1))
        if (.false.) name = ""
    end subroutine test_python_nav_beginning_of_definition_2

    subroutine test_python_nav_beginning_of_definition_3(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage, v(300)
        logical :: b(300)
        character(len=:), allocatable :: name

        call py_buffer(LF &
                       //"class C(object):"//LF &
                       //LF &
                       //"    async def m(self):"//LF &
                       //"        return await self.c()"//LF &
                       //LF &
                       //"    async def c(self):"//LF &
                       //"        pass"//LF)
        v(1) = look_pos("self.c()", 1_int64, .false.)
        v(2) = point()
        call python_nav_beginning_of_definition(1_int64, b(1), st)
        v(3) = point()
        v(4) = v(3)
        call goto_char(v(2))
        v(5) = point()
        v(6) = look_pos("async def m", -1_int64, .false.)
        call beginning_of_line()
        v(7) = point()
        v(8) = v(7)
        call goto_char(v(5))
        call check(error, v(4) == v(8), "(= (save-excursion (python-nav-beginning-of-defun) (point)) ")
        if (allocated(error)) return
        if (.false.) shortage = v(1) + merge(1, 0, b(1))
        if (.false.) name = ""
    end subroutine test_python_nav_beginning_of_definition_3

    subroutine test_python_nav_beginning_of_definition_4(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage, v(300)
        logical :: b(300)
        character(len=:), allocatable :: name

        call py_buffer(LF &
                       //"def a():"//LF &
                       //"    pass"//LF &
                       //LF &
                       //"def \"//LF &
                       //"        b():"//LF &
                       //"    return 0"//LF &
                       //LF &
                       //"def c():"//LF &
                       //"    pass"//LF)
        v(1) = look_pos("def c():", 1_int64, .false.)
        v(2) = point()
        call python_nav_beginning_of_definition(1_int64, b(1), st)
        v(3) = point()
        v(4) = v(3)
        call goto_char(v(2))
        v(5) = point()
        v(6) = look_pos("def \", -1_int64, .false.)
        call beginning_of_line()
        v(7) = point()
        v(8) = v(7)
        call goto_char(v(5))
        call check(error, v(4) == v(8), "(= (save-excursion (python-nav-beginning-of-defun) (point)) ")
        if (allocated(error)) return
        v(9) = look_pos("return 0", -1_int64, .false.)
        v(10) = point()
        call python_nav_beginning_of_definition(1_int64, b(2), st)
        v(11) = point()
        v(12) = v(11)
        call goto_char(v(10))
        v(13) = point()
        v(14) = look_pos("def \", -1_int64, .false.)
        call beginning_of_line()
        v(15) = point()
        v(16) = v(15)
        call goto_char(v(13))
        call check(error, v(12) == v(16), "(= (save-excursion (python-nav-beginning-of-defun) (point)) ")
        if (allocated(error)) return
        v(17) = look_pos("b():", -1_int64, .false.)
        v(18) = point()
        call python_nav_beginning_of_definition(1_int64, b(3), st)
        v(19) = point()
        v(20) = v(19)
        call goto_char(v(18))
        v(21) = point()
        v(22) = look_pos("def \", -1_int64, .false.)
        call beginning_of_line()
        v(23) = point()
        v(24) = v(23)
        call goto_char(v(21))
        call check(error, v(20) == v(24), "(= (save-excursion (python-nav-beginning-of-defun) (point)) ")
        if (allocated(error)) return
        v(25) = look_pos("def a():", -1_int64, .false.)
        v(26) = point()
        call python_nav_beginning_of_definition(-1_int64, b(4), st)
        v(27) = point()
        v(28) = v(27)
        call goto_char(v(26))
        v(29) = point()
        v(30) = look_pos("def \", 1_int64, .false.)
        call beginning_of_line()
        v(31) = point()
        v(32) = v(31)
        call goto_char(v(29))
        call check(error, v(28) == v(32), "(= (save-excursion (python-nav-beginning-of-defun -1) (point")
        if (allocated(error)) return
        if (.false.) shortage = v(1) + merge(1, 0, b(1))
        if (.false.) name = ""
    end subroutine test_python_nav_beginning_of_definition_4

    subroutine test_python_nav_beginning_of_definition_5(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage, v(300)
        logical :: b(300)
        character(len=:), allocatable :: name

        call py_buffer(LF &
                       //"class C:"//LF &
                       //LF &
                       //"    def \"//LF &
                       //"            m(self):"//LF &
                       //"        pass"//LF)
        v(1) = look_pos("m(self):", 1_int64, .false.)
        v(2) = point()
        call python_nav_beginning_of_definition(1_int64, b(1), st)
        v(3) = point()
        v(4) = v(3)
        call goto_char(v(2))
        v(5) = point()
        v(6) = look_pos("def \", -1_int64, .false.)
        call beginning_of_line()
        v(7) = point()
        v(8) = v(7)
        call goto_char(v(5))
        call check(error, v(4) == v(8), "(= (save-excursion (python-nav-beginning-of-defun) (point)) ")
        if (allocated(error)) return
        v(9) = look_pos("class C:", -1_int64, .false.)
        v(10) = point()
        call python_nav_beginning_of_definition(-1_int64, b(2), st)
        v(11) = point()
        v(12) = v(11)
        call goto_char(v(10))
        v(13) = point()
        v(14) = look_pos("def \", 1_int64, .false.)
        call beginning_of_line()
        v(15) = point()
        v(16) = v(15)
        call goto_char(v(13))
        call check(error, v(12) == v(16), "(= (save-excursion (python-nav-beginning-of-defun -1) (point")
        if (allocated(error)) return
        if (.false.) shortage = v(1) + merge(1, 0, b(1))
        if (.false.) name = ""
    end subroutine test_python_nav_beginning_of_definition_5

    subroutine test_python_nav_beginning_of_definition_6(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage, v(300)
        logical :: b(300)
        character(len=:), allocatable :: name

        call py_buffer(LF &
                       //"class C:"//LF &
                       //"    def foo(self):"//LF &
                       //"        pass"//LF)
        v(1) = look_pos("self", 1_int64, .false.)
        v(2) = point()
        call python_nav_beginning_of_definition(1_int64, b(1), st)
        v(3) = point()
        v(4) = v(3)
        call goto_char(v(2))
        v(5) = point()
        call beginning_of_line()
        v(6) = point()
        v(7) = v(6)
        call goto_char(v(5))
        call check(error, v(4) == v(7), "(= (save-excursion (python-nav-beginning-of-defun) (point)) ")
        if (allocated(error)) return
        if (.false.) shortage = v(1) + merge(1, 0, b(1))
        if (.false.) name = ""
    end subroutine test_python_nav_beginning_of_definition_6

    subroutine test_python_nav_end_of_definition_1(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage, v(300)
        logical :: b(300)
        character(len=:), allocatable :: name

        call py_buffer(LF &
                       //"class C(object):"//LF &
                       //LF &
                       //"    def m(self):"//LF &
                       //"        self.c()"//LF &
                       //LF &
                       //"        def b():"//LF &
                       //"            pass"//LF &
                       //LF &
                       //"        def a():"//LF &
                       //"            pass"//LF &
                       //LF &
                       //"    def c(self):"//LF &
                       //"        pass"//LF)
        v(1) = point()
        v(2) = look_pos("class C(object):", 1_int64, .false.)
        b(1) = python_nav_end_of_definition()
        v(3) = point()
        v(4) = v(3)
        call goto_char(v(1))
        v(5) = point()
        v(6) = point_max()
        v(7) = v(6)
        call goto_char(v(5))
        call check(error, v(4) == v(7), "(= (save-excursion (python-tests-look-at 'class C(object):')")
        if (allocated(error)) return
        v(8) = point()
        v(9) = look_pos("def m(self):", 1_int64, .false.)
        b(2) = python_nav_end_of_definition()
        v(10) = point()
        v(11) = v(10)
        call goto_char(v(8))
        v(12) = point()
        v(13) = look_pos("def c(self):", 1_int64, .false.)
        shortage = forward_line(-1_int64)
        v(14) = point()
        v(15) = v(14)
        call goto_char(v(12))
        call check(error, v(11) == v(15), "(= (save-excursion (python-tests-look-at 'def m(self):') (py")
        if (allocated(error)) return
        v(16) = point()
        v(17) = look_pos("def b():", 1_int64, .false.)
        shortage = forward_line(-1_int64)
        b(3) = python_nav_end_of_definition()
        v(18) = point()
        v(19) = v(18)
        call goto_char(v(16))
        v(20) = point()
        v(21) = look_pos("def c(self):", 1_int64, .false.)
        shortage = forward_line(-1_int64)
        v(22) = point()
        v(23) = v(22)
        call goto_char(v(20))
        call check(error, v(19) == v(23), "(= (save-excursion (python-tests-look-at 'def b():') (forwar")
        if (allocated(error)) return
        v(24) = point()
        v(25) = look_pos("def b():", 1_int64, .false.)
        b(4) = python_nav_end_of_definition()
        v(26) = point()
        v(27) = v(26)
        call goto_char(v(24))
        v(28) = point()
        v(29) = look_pos("def b():", 1_int64, .false.)
        shortage = forward_line(2_int64)
        v(30) = point()
        v(31) = v(30)
        call goto_char(v(28))
        call check(error, v(27) == v(31), "(= (save-excursion (python-tests-look-at 'def b():') (python")
        if (allocated(error)) return
        v(32) = point()
        v(33) = look_pos("def a():", 1_int64, .false.)
        shortage = forward_line(-1_int64)
        b(5) = python_nav_end_of_definition()
        b(6) = b(5)
        call goto_char(v(32))
        call check(error,.not. (b(6)), "(not (save-excursion (python-tests-look-at 'def a():') (forw")
        if (allocated(error)) return
        v(34) = point()
        v(35) = look_pos("def c(self):", 1_int64, .false.)
        b(7) = python_nav_end_of_definition()
        v(36) = point()
        v(37) = v(36)
        call goto_char(v(34))
        v(38) = point()
        v(39) = point_max()
        v(40) = v(39)
        call goto_char(v(38))
        call check(error, v(37) == v(40), "(= (save-excursion (python-tests-look-at 'def c(self):') (py")
        if (allocated(error)) return
        if (.false.) shortage = v(1) + merge(1, 0, b(1))
        if (.false.) name = ""
    end subroutine test_python_nav_end_of_definition_1

    subroutine test_python_nav_end_of_definition_2(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage, v(300)
        logical :: b(300)
        character(len=:), allocatable :: name

        call py_buffer(LF &
                       //"def decoratorFunctionWithArguments(arg1, arg2, arg3):"//LF &
                       //"    '''print decorated function call data to stdout."//LF &
                       //LF &
                       //"    Usage:"//LF &
                       //LF &
                       //"    @decoratorFunctionWithArguments('arg1', 'arg2')"//LF &
                       //"    def func(a, b, c=True):"//LF &
                       //"        pass"//LF &
                       //"    '''"//LF &
                       //LF &
                       //"    def wwrap(f):"//LF &
                       //"        print 'Inside wwrap()'"//LF &
                       //"        def wrapped_f(*args):"//LF &
                       //"            print 'Inside wrapped_f()'"//LF &
                       //"            print 'Decorator arguments:', arg1, arg2, arg3"//LF &
                       //"            f(*args)"//LF &
                       //"            print 'After f(*args)'"//LF &
                       //"        return wrapped_f"//LF &
                       //"    return wwrap"//LF)
        v(1) = point()
        v(2) = look_pos("def decoratorFunctionWithArguments", 1_int64, .false.)
        b(1) = python_nav_end_of_definition()
        v(3) = point()
        v(4) = v(3)
        call goto_char(v(1))
        v(5) = point()
        v(6) = point_max()
        v(7) = v(6)
        call goto_char(v(5))
        call check(error, v(4) == v(7), "(= (save-excursion (python-tests-look-at 'def decoratorFunct")
        if (allocated(error)) return
        v(8) = point()
        v(9) = look_pos("@decoratorFunctionWithArguments", 1_int64, .false.)
        b(2) = python_nav_end_of_definition()
        v(10) = point()
        v(11) = v(10)
        call goto_char(v(8))
        v(12) = point()
        v(13) = point_max()
        v(14) = v(13)
        call goto_char(v(12))
        call check(error, v(11) == v(14), "(= (save-excursion (python-tests-look-at '@decoratorFunction")
        if (allocated(error)) return
        v(15) = point()
        v(16) = look_pos("def wwrap(f):", 1_int64, .false.)
        b(3) = python_nav_end_of_definition()
        v(17) = point()
        v(18) = v(17)
        call goto_char(v(15))
        v(19) = point()
        v(20) = look_pos("return wwrap", 1_int64, .false.)
        v(21) = line_beginning_position()
        v(22) = v(21)
        call goto_char(v(19))
        call check(error, v(18) == v(22), "(= (save-excursion (python-tests-look-at 'def wwrap(f):') (p")
        if (allocated(error)) return
        v(23) = point()
        v(24) = look_pos("def wrapped_f(*args):", 1_int64, .false.)
        b(4) = python_nav_end_of_definition()
        v(25) = point()
        v(26) = v(25)
        call goto_char(v(23))
        v(27) = point()
        v(28) = look_pos("return wrapped_f", 1_int64, .false.)
        v(29) = line_beginning_position()
        v(30) = v(29)
        call goto_char(v(27))
        call check(error, v(26) == v(30), "(= (save-excursion (python-tests-look-at 'def wrapped_f(*arg")
        if (allocated(error)) return
        v(31) = point()
        v(32) = look_pos("f(*args)", 1_int64, .false.)
        b(5) = python_nav_end_of_definition()
        v(33) = point()
        v(34) = v(33)
        call goto_char(v(31))
        v(35) = point()
        v(36) = look_pos("return wrapped_f", 1_int64, .false.)
        v(37) = line_beginning_position()
        v(38) = v(37)
        call goto_char(v(35))
        call check(error, v(34) == v(38), "(= (save-excursion (python-tests-look-at 'f(*args)') (python")
        if (allocated(error)) return
        if (.false.) shortage = v(1) + merge(1, 0, b(1))
        if (.false.) name = ""
    end subroutine test_python_nav_end_of_definition_2

    subroutine test_python_nav_end_of_definition_3(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage, v(300)
        logical :: b(300)
        character(len=:), allocatable :: name

        call py_buffer(LF &
                       //"def \"//LF &
                       //"        a():"//LF &
                       //"    return 0"//LF)
        v(1) = point()
        v(2) = look_pos("def \", 1_int64, .false.)
        b(1) = python_nav_end_of_definition()
        v(3) = point()
        v(4) = v(3)
        call goto_char(v(1))
        v(5) = point()
        v(6) = point_max()
        v(7) = v(6)
        call goto_char(v(5))
        call check(error, v(4) == v(7), "(= (save-excursion (python-tests-look-at 'def \\\\') (python-n")
        if (allocated(error)) return
        if (.false.) shortage = v(1) + merge(1, 0, b(1))
        if (.false.) name = ""
    end subroutine test_python_nav_end_of_definition_3

    subroutine test_python_nav_backward_definition_1(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage, v(300)
        logical :: b(300)
        character(len=:), allocatable :: name

        call py_buffer(LF &
                       //"class A(object): # A"//LF &
                       //LF &
                       //"    def a(self): # a"//LF &
                       //"        pass"//LF &
                       //LF &
                       //"    def b(self): # b"//LF &
                       //"        pass"//LF &
                       //LF &
                       //"    class B(object): # B"//LF &
                       //LF &
                       //"        class C(object): # C"//LF &
                       //LF &
                       //"            def d(self): # d"//LF &
                       //"                pass"//LF &
                       //LF &
                       //"            # def e(self): # e"//LF &
                       //"            #     pass"//LF &
                       //LF &
                       //"    def c(self): # c"//LF &
                       //"        pass"//LF &
                       //LF &
                       //"    # def d(self): # d"//LF &
                       //"    #     pass"//LF)
        v(1) = point_max()
        call goto_char(v(1))
        v(2) = point()
        v(3) = python_nav_backward_definition(1_int64)
        v(4) = v(3)
        call goto_char(v(2))
        v(5) = look_pos("    def c(self): # c", -1_int64, .false.)
        call check(error, v(4) == v(5), "(= (save-excursion (python-nav-backward-defun)) (python-test")
        if (allocated(error)) return
        v(6) = point()
        v(7) = python_nav_backward_definition(1_int64)
        v(8) = v(7)
        call goto_char(v(6))
        v(9) = look_pos("            def d(self): # d", -1_int64, .false.)
        call check(error, v(8) == v(9), "(= (save-excursion (python-nav-backward-defun)) (python-test")
        if (allocated(error)) return
        v(10) = point()
        v(11) = python_nav_backward_definition(1_int64)
        v(12) = v(11)
        call goto_char(v(10))
        v(13) = look_pos("        class C(object): # C", -1_int64, .false.)
        call check(error, v(12) == v(13), "(= (save-excursion (python-nav-backward-defun)) (python-test")
        if (allocated(error)) return
        v(14) = point()
        v(15) = python_nav_backward_definition(1_int64)
        v(16) = v(15)
        call goto_char(v(14))
        v(17) = look_pos("    class B(object): # B", -1_int64, .false.)
        call check(error, v(16) == v(17), "(= (save-excursion (python-nav-backward-defun)) (python-test")
        if (allocated(error)) return
        v(18) = point()
        v(19) = python_nav_backward_definition(1_int64)
        v(20) = v(19)
        call goto_char(v(18))
        v(21) = look_pos("    def b(self): # b", -1_int64, .false.)
        call check(error, v(20) == v(21), "(= (save-excursion (python-nav-backward-defun)) (python-test")
        if (allocated(error)) return
        v(22) = point()
        v(23) = python_nav_backward_definition(1_int64)
        v(24) = v(23)
        call goto_char(v(22))
        v(25) = look_pos("    def a(self): # a", -1_int64, .false.)
        call check(error, v(24) == v(25), "(= (save-excursion (python-nav-backward-defun)) (python-test")
        if (allocated(error)) return
        v(26) = point()
        v(27) = python_nav_backward_definition(1_int64)
        v(28) = v(27)
        call goto_char(v(26))
        v(29) = look_pos("class A(object): # A", -1_int64, .false.)
        call check(error, v(28) == v(29), "(= (save-excursion (python-nav-backward-defun)) (python-test")
        if (allocated(error)) return
        v(30) = python_nav_backward_definition(1_int64)
        call check(error,.not. (v(30) /= 0), "(not (python-nav-backward-defun))")
        if (allocated(error)) return
        if (.false.) shortage = v(1) + merge(1, 0, b(1))
        if (.false.) name = ""
    end subroutine test_python_nav_backward_definition_1

    subroutine test_python_nav_backward_definition_2(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage, v(300)
        logical :: b(300)
        character(len=:), allocatable :: name

        call py_buffer(LF &
                       //"def decoratorFunctionWithArguments(arg1, arg2, arg3):"//LF &
                       //"    '''print decorated function call data to stdout."//LF &
                       //LF &
                       //"    Usage:"//LF &
                       //LF &
                       //"    @decoratorFunctionWithArguments('arg1', 'arg2')"//LF &
                       //"    def func(a, b, c=True):"//LF &
                       //"        pass"//LF &
                       //"    '''"//LF &
                       //LF &
                       //"    def wwrap(f):"//LF &
                       //"        print 'Inside wwrap()'"//LF &
                       //"        def wrapped_f(*args):"//LF &
                       //"            print 'Inside wrapped_f()'"//LF &
                       //"            print 'Decorator arguments:', arg1, arg2, arg3"//LF &
                       //"            f(*args)"//LF &
                       //"            print 'After f(*args)'"//LF &
                       //"        return wrapped_f"//LF &
                       //"    return wwrap"//LF)
        v(1) = point_max()
        call goto_char(v(1))
        v(2) = point()
        v(3) = python_nav_backward_definition(1_int64)
        v(4) = v(3)
        call goto_char(v(2))
        v(5) = look_pos("        def wrapped_f(*args):", -1_int64, .false.)
        call check(error, v(4) == v(5), "(= (save-excursion (python-nav-backward-defun)) (python-test")
        if (allocated(error)) return
        v(6) = point()
        v(7) = python_nav_backward_definition(1_int64)
        v(8) = v(7)
        call goto_char(v(6))
        v(9) = look_pos("    def wwrap(f):", -1_int64, .false.)
        call check(error, v(8) == v(9), "(= (save-excursion (python-nav-backward-defun)) (python-test")
        if (allocated(error)) return
        v(10) = point()
        v(11) = python_nav_backward_definition(1_int64)
        v(12) = v(11)
        call goto_char(v(10))
        v(13) = look_pos("def decoratorFunctionWithArguments(arg1, arg2, arg3):", -1_int64, .false.)
        call check(error, v(12) == v(13), "(= (save-excursion (python-nav-backward-defun)) (python-test")
        if (allocated(error)) return
        v(14) = python_nav_backward_definition(1_int64)
        call check(error,.not. (v(14) /= 0), "(not (python-nav-backward-defun))")
        if (allocated(error)) return
        if (.false.) shortage = v(1) + merge(1, 0, b(1))
        if (.false.) name = ""
    end subroutine test_python_nav_backward_definition_2

    subroutine test_python_nav_backward_definition_4(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage, v(300)
        logical :: b(300)
        character(len=:), allocatable :: name

        call py_buffer(LF &
                       //"def \"//LF &
                       //"        a():"//LF &
                       //"    return 0"//LF)
        v(1) = point_max()
        call goto_char(v(1))
        v(2) = point()
        v(3) = python_nav_backward_definition(1_int64)
        v(4) = v(3)
        call goto_char(v(2))
        v(5) = look_pos("def \", -1_int64, .false.)
        call check(error, v(4) == v(5), "(= (save-excursion (python-nav-backward-defun)) (python-test")
        if (allocated(error)) return
        v(6) = python_nav_backward_definition(1_int64)
        call check(error,.not. (v(6) /= 0), "(not (python-nav-backward-defun))")
        if (allocated(error)) return
        if (.false.) shortage = v(1) + merge(1, 0, b(1))
        if (.false.) name = ""
    end subroutine test_python_nav_backward_definition_4

    subroutine test_python_nav_forward_definition_1(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage, v(300)
        logical :: b(300)
        character(len=:), allocatable :: name

        call py_buffer(LF &
                       //"class A(object): # A"//LF &
                       //LF &
                       //"    def a(self): # a"//LF &
                       //"        pass"//LF &
                       //LF &
                       //"    def b(self): # b"//LF &
                       //"        pass"//LF &
                       //LF &
                       //"    class B(object): # B"//LF &
                       //LF &
                       //"        class C(object): # C"//LF &
                       //LF &
                       //"            def d(self): # d"//LF &
                       //"                pass"//LF &
                       //LF &
                       //"            # def e(self): # e"//LF &
                       //"            #     pass"//LF &
                       //LF &
                       //"    def c(self): # c"//LF &
                       //"        pass"//LF &
                       //LF &
                       //"    # def d(self): # d"//LF &
                       //"    #     pass"//LF)
        v(1) = point_min()
        call goto_char(v(1))
        v(2) = point()
        v(3) = python_nav_forward_definition(1_int64)
        v(4) = v(3)
        call goto_char(v(2))
        v(5) = look_pos("(object): # A", 1_int64, .false.)
        call check(error, v(4) == v(5), "(= (save-excursion (python-nav-forward-defun)) (python-tests")
        if (allocated(error)) return
        v(6) = point()
        v(7) = python_nav_forward_definition(1_int64)
        v(8) = v(7)
        call goto_char(v(6))
        v(9) = look_pos("(self): # a", 1_int64, .false.)
        call check(error, v(8) == v(9), "(= (save-excursion (python-nav-forward-defun)) (python-tests")
        if (allocated(error)) return
        v(10) = point()
        v(11) = python_nav_forward_definition(1_int64)
        v(12) = v(11)
        call goto_char(v(10))
        v(13) = look_pos("(self): # b", 1_int64, .false.)
        call check(error, v(12) == v(13), "(= (save-excursion (python-nav-forward-defun)) (python-tests")
        if (allocated(error)) return
        v(14) = point()
        v(15) = python_nav_forward_definition(1_int64)
        v(16) = v(15)
        call goto_char(v(14))
        v(17) = look_pos("(object): # B", 1_int64, .false.)
        call check(error, v(16) == v(17), "(= (save-excursion (python-nav-forward-defun)) (python-tests")
        if (allocated(error)) return
        v(18) = point()
        v(19) = python_nav_forward_definition(1_int64)
        v(20) = v(19)
        call goto_char(v(18))
        v(21) = look_pos("(object): # C", 1_int64, .false.)
        call check(error, v(20) == v(21), "(= (save-excursion (python-nav-forward-defun)) (python-tests")
        if (allocated(error)) return
        v(22) = point()
        v(23) = python_nav_forward_definition(1_int64)
        v(24) = v(23)
        call goto_char(v(22))
        v(25) = look_pos("(self): # d", 1_int64, .false.)
        call check(error, v(24) == v(25), "(= (save-excursion (python-nav-forward-defun)) (python-tests")
        if (allocated(error)) return
        v(26) = point()
        v(27) = python_nav_forward_definition(1_int64)
        v(28) = v(27)
        call goto_char(v(26))
        v(29) = look_pos("(self): # c", 1_int64, .false.)
        call check(error, v(28) == v(29), "(= (save-excursion (python-nav-forward-defun)) (python-tests")
        if (allocated(error)) return
        v(30) = python_nav_forward_definition(1_int64)
        call check(error,.not. (v(30) /= 0), "(not (python-nav-forward-defun))")
        if (allocated(error)) return
        if (.false.) shortage = v(1) + merge(1, 0, b(1))
        if (.false.) name = ""
    end subroutine test_python_nav_forward_definition_1

    subroutine test_python_nav_forward_definition_2(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage, v(300)
        logical :: b(300)
        character(len=:), allocatable :: name

        call py_buffer(LF &
                       //"def decoratorFunctionWithArguments(arg1, arg2, arg3):"//LF &
                       //"    '''print decorated function call data to stdout."//LF &
                       //LF &
                       //"    Usage:"//LF &
                       //LF &
                       //"    @decoratorFunctionWithArguments('arg1', 'arg2')"//LF &
                       //"    def func(a, b, c=True):"//LF &
                       //"        pass"//LF &
                       //"    '''"//LF &
                       //LF &
                       //"    def wwrap(f):"//LF &
                       //"        print 'Inside wwrap()'"//LF &
                       //"        def wrapped_f(*args):"//LF &
                       //"            print 'Inside wrapped_f()'"//LF &
                       //"            print 'Decorator arguments:', arg1, arg2, arg3"//LF &
                       //"            f(*args)"//LF &
                       //"            print 'After f(*args)'"//LF &
                       //"        return wrapped_f"//LF &
                       //"    return wwrap"//LF)
        v(1) = point_min()
        call goto_char(v(1))
        v(2) = point()
        v(3) = python_nav_forward_definition(1_int64)
        v(4) = v(3)
        call goto_char(v(2))
        v(5) = look_pos("(arg1, arg2, arg3):", 1_int64, .false.)
        call check(error, v(4) == v(5), "(= (save-excursion (python-nav-forward-defun)) (python-tests")
        if (allocated(error)) return
        v(6) = point()
        v(7) = python_nav_forward_definition(1_int64)
        v(8) = v(7)
        call goto_char(v(6))
        v(9) = look_pos("(f):", 1_int64, .false.)
        call check(error, v(8) == v(9), "(= (save-excursion (python-nav-forward-defun)) (python-tests")
        if (allocated(error)) return
        v(10) = point()
        v(11) = python_nav_forward_definition(1_int64)
        v(12) = v(11)
        call goto_char(v(10))
        v(13) = look_pos("(*args):", 1_int64, .false.)
        call check(error, v(12) == v(13), "(= (save-excursion (python-nav-forward-defun)) (python-tests")
        if (allocated(error)) return
        v(14) = python_nav_forward_definition(1_int64)
        call check(error,.not. (v(14) /= 0), "(not (python-nav-forward-defun))")
        if (allocated(error)) return
        if (.false.) shortage = v(1) + merge(1, 0, b(1))
        if (.false.) name = ""
    end subroutine test_python_nav_forward_definition_2

    subroutine test_python_nav_forward_definition_4(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage, v(300)
        logical :: b(300)
        character(len=:), allocatable :: name

        call py_buffer(LF &
                       //"def \"//LF &
                       //"        a():"//LF &
                       //"    return 0"//LF)
        v(1) = point_min()
        call goto_char(v(1))
        v(2) = point()
        v(3) = python_nav_forward_definition(1_int64)
        v(4) = v(3)
        call goto_char(v(2))
        v(5) = look_pos("():", 1_int64, .false.)
        call check(error, v(4) == v(5), "(= (save-excursion (python-nav-forward-defun)) (python-tests")
        if (allocated(error)) return
        v(6) = python_nav_forward_definition(1_int64)
        call check(error,.not. (v(6) /= 0), "(not (python-nav-forward-defun))")
        if (allocated(error)) return
        if (.false.) shortage = v(1) + merge(1, 0, b(1))
        if (.false.) name = ""
    end subroutine test_python_nav_forward_definition_4

    subroutine test_python_nav_beginning_of_statement_1(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage, v(300)
        logical :: b(300)
        character(len=:), allocatable :: name

        call py_buffer(LF &
                       //"v1 = 123 + \"//LF &
                       //"     456 + \"//LF &
                       //"     789"//LF &
                       //"v2 = (value1,"//LF &
                       //"      value2,"//LF &
                       //LF &
                       //"      value3,"//LF &
                       //"      value4)"//LF &
                       //"v3 = ('this is a string'"//LF &
                       //LF &
                       //"      'that is continued'"//LF &
                       //"      'between lines'"//LF &
                       //"      'within a paren',"//LF &
                       //"      # this is a comment, yo"//LF &
                       //"      'continue previous line')"//LF &
                       //"v4 = '''"//LF &
                       //"a very long"//LF &
                       //"string"//LF &
                       //"'''"//LF)
        v(1) = look_pos("v2 =", 1_int64, .false.)
        b(300) = python_util_forward_comment(-1)
        v(2) = point()
        v(3) = python_nav_beginning_of_statement()
        v(4) = point()
        v(5) = v(4)
        call goto_char(v(2))
        v(6) = look_pos("v1 =", -1_int64, .true.)
        call check(error, v(5) == v(6), "(= (save-excursion (python-nav-beginning-of-statement) (poin")
        if (allocated(error)) return
        v(7) = look_pos("v3 =", 1_int64, .false.)
        b(300) = python_util_forward_comment(-1)
        v(8) = point()
        v(9) = python_nav_beginning_of_statement()
        v(10) = point()
        v(11) = v(10)
        call goto_char(v(8))
        v(12) = look_pos("v2 =", -1_int64, .true.)
        call check(error, v(11) == v(12), "(= (save-excursion (python-nav-beginning-of-statement) (poin")
        if (allocated(error)) return
        v(13) = look_pos("v4 =", 1_int64, .false.)
        b(300) = python_util_forward_comment(-1)
        v(14) = point()
        v(15) = python_nav_beginning_of_statement()
        v(16) = point()
        v(17) = v(16)
        call goto_char(v(14))
        v(18) = look_pos("v3 =", -1_int64, .true.)
        call check(error, v(17) == v(18), "(= (save-excursion (python-nav-beginning-of-statement) (poin")
        if (allocated(error)) return
        v(19) = point_max()
        call goto_char(v(19))
        b(300) = python_util_forward_comment(-1)
        v(20) = point()
        v(21) = python_nav_beginning_of_statement()
        v(22) = point()
        v(23) = v(22)
        call goto_char(v(20))
        v(24) = look_pos("v4 =", -1_int64, .true.)
        call check(error, v(23) == v(24), "(= (save-excursion (python-nav-beginning-of-statement) (poin")
        if (allocated(error)) return
        if (.false.) shortage = v(1) + merge(1, 0, b(1))
        if (.false.) name = ""
    end subroutine test_python_nav_beginning_of_statement_1

    subroutine test_python_nav_end_of_statement_1(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage, v(300)
        logical :: b(300)
        character(len=:), allocatable :: name

        call py_buffer(LF &
                       //"v1 = 123 + \"//LF &
                       //"     456 + \"//LF &
                       //"     789"//LF &
                       //"v2 = (value1,"//LF &
                       //"      value2,"//LF &
                       //LF &
                       //"      value3,"//LF &
                       //"      value4)"//LF &
                       //"v3 = ('this is a string'"//LF &
                       //LF &
                       //"      'that is continued'"//LF &
                       //"      'between lines'"//LF &
                       //"      'within a paren',"//LF &
                       //"      # this is a comment, yo"//LF &
                       //"      'continue previous line')"//LF &
                       //"v4 = '''"//LF &
                       //"a very long"//LF &
                       //"string"//LF &
                       //"'''"//LF)
        v(1) = look_pos("v1 =", 1_int64, .false.)
        v(2) = point()
        v(3) = python_nav_end_of_statement()
        v(4) = point()
        v(5) = v(4)
        call goto_char(v(2))
        v(6) = point()
        v(7) = look_pos("789", 1_int64, .false.)
        v(8) = line_end_position()
        v(9) = v(8)
        call goto_char(v(6))
        call check(error, v(5) == v(9), "(= (save-excursion (python-nav-end-of-statement) (point)) (s")
        if (allocated(error)) return
        v(10) = look_pos("v2 =", 1_int64, .false.)
        v(11) = point()
        v(12) = python_nav_end_of_statement()
        v(13) = point()
        v(14) = v(13)
        call goto_char(v(11))
        v(15) = point()
        v(16) = look_pos("value4)", 1_int64, .false.)
        v(17) = line_end_position()
        v(18) = v(17)
        call goto_char(v(15))
        call check(error, v(14) == v(18), "(= (save-excursion (python-nav-end-of-statement) (point)) (s")
        if (allocated(error)) return
        v(19) = look_pos("v3 =", 1_int64, .false.)
        v(20) = point()
        v(21) = python_nav_end_of_statement()
        v(22) = point()
        v(23) = v(22)
        call goto_char(v(20))
        v(24) = point()
        v(25) = look_pos("'continue previous line')", 1_int64, .false.)
        v(26) = line_end_position()
        v(27) = v(26)
        call goto_char(v(24))
        call check(error, v(23) == v(27), "(= (save-excursion (python-nav-end-of-statement) (point)) (s")
        if (allocated(error)) return
        v(28) = look_pos("v4 =", 1_int64, .false.)
        v(29) = point()
        v(30) = python_nav_end_of_statement()
        v(31) = point()
        v(32) = v(31)
        call goto_char(v(29))
        v(33) = point()
        v(34) = point_max()
        call goto_char(v(34))
        b(300) = python_util_forward_comment(-1)
        v(35) = point()
        v(36) = v(35)
        call goto_char(v(33))
        call check(error, v(32) == v(36), "(= (save-excursion (python-nav-end-of-statement) (point)) (s")
        if (allocated(error)) return
        if (.false.) shortage = v(1) + merge(1, 0, b(1))
        if (.false.) name = ""
    end subroutine test_python_nav_end_of_statement_1

    subroutine test_python_nav_end_of_statement_2(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage, v(300)
        logical :: b(300)
        character(len=:), allocatable :: name

        call py_buffer("'"//LF &
                       //"''"//LF)
        v(1) = python_nav_end_of_statement()
        if (.false.) shortage = v(1) + merge(1, 0, b(1))
        if (.false.) name = ""
    end subroutine test_python_nav_end_of_statement_2

    subroutine test_python_nav_end_of_statement_3(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage, v(300)
        logical :: b(300)
        character(len=:), allocatable :: name

        call py_buffer(LF &
                       //"' """""""//LF &
                       //"v = 1"//LF)
        v(1) = look_pos("v =", 1_int64, .false.)
        v(2) = point()
        v(3) = python_nav_end_of_statement()
        v(4) = point()
        v(5) = v(4)
        call goto_char(v(2))
        v(6) = point()
        v(7) = point_max()
        v(8) = v(7)
        call goto_char(v(6))
        call check(error, v(5) == v(8), "(= (save-excursion (python-nav-end-of-statement) (point)) (s")
        if (allocated(error)) return
        if (.false.) shortage = v(1) + merge(1, 0, b(1))
        if (.false.) name = ""
    end subroutine test_python_nav_end_of_statement_3

    subroutine test_python_nav_end_of_statement_4(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage, v(300)
        logical :: b(300)
        character(len=:), allocatable :: name

        call py_buffer(LF &
                       //"abc = 'a\"//LF &
                       //"b\"//LF &
                       //"c'"//LF &
                       //"d = '''d'''"//LF)
        v(1) = look_pos("b\", 1_int64, .false.)
        v(2) = point()
        v(3) = python_nav_end_of_statement()
        v(4) = point()
        v(5) = v(4)
        call goto_char(v(2))
        v(6) = point()
        v(7) = look_pos("c'", 1_int64, .false.)
        v(8) = line_end_position()
        v(9) = v(8)
        call goto_char(v(6))
        call check(error, v(5) == v(9), "(= (save-excursion (python-nav-end-of-statement) (point)) (s")
        if (allocated(error)) return
        if (.false.) shortage = v(1) + merge(1, 0, b(1))
        if (.false.) name = ""
    end subroutine test_python_nav_end_of_statement_4

    subroutine test_python_nav_forward_statement_1(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage, v(300)
        logical :: b(300)
        character(len=:), allocatable :: name

        call py_buffer(LF &
                       //"v1 = 123 + \"//LF &
                       //"     456 + \"//LF &
                       //"     789"//LF &
                       //"v2 = (value1,"//LF &
                       //"      value2,"//LF &
                       //LF &
                       //"      value3,"//LF &
                       //"      value4)"//LF &
                       //"v3 = ('this is a string'"//LF &
                       //LF &
                       //"      'that is continued'"//LF &
                       //"      'between lines'"//LF &
                       //"      'within a paren',"//LF &
                       //"      # this is a comment, yo"//LF &
                       //"      'continue previous line')"//LF &
                       //"v4 = '''"//LF &
                       //"a very long"//LF &
                       //"string"//LF &
                       //"'''"//LF)
        v(1) = look_pos("v1 =", 1_int64, .false.)
        v(2) = point()
        call python_nav_forward_statement(1_int64)
        v(3) = point()
        v(4) = v(3)
        call goto_char(v(2))
        v(5) = look_pos("v2 =", 1_int64, .false.)
        call check(error, v(4) == v(5), "(= (save-excursion (python-nav-forward-statement) (point)) (")
        if (allocated(error)) return
        v(6) = point()
        call python_nav_forward_statement(1_int64)
        v(7) = point()
        v(8) = v(7)
        call goto_char(v(6))
        v(9) = look_pos("v3 =", 1_int64, .false.)
        call check(error, v(8) == v(9), "(= (save-excursion (python-nav-forward-statement) (point)) (")
        if (allocated(error)) return
        v(10) = point()
        call python_nav_forward_statement(1_int64)
        v(11) = point()
        v(12) = v(11)
        call goto_char(v(10))
        v(13) = look_pos("v4 =", 1_int64, .false.)
        call check(error, v(12) == v(13), "(= (save-excursion (python-nav-forward-statement) (point)) (")
        if (allocated(error)) return
        v(14) = point()
        call python_nav_forward_statement(1_int64)
        v(15) = point()
        v(16) = v(15)
        call goto_char(v(14))
        v(17) = point_max()
        call check(error, v(16) == v(17), "(= (save-excursion (python-nav-forward-statement) (point)) (")
        if (allocated(error)) return
        if (.false.) shortage = v(1) + merge(1, 0, b(1))
        if (.false.) name = ""
    end subroutine test_python_nav_forward_statement_1

    subroutine test_python_nav_backward_statement_1(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage, v(300)
        logical :: b(300)
        character(len=:), allocatable :: name

        call py_buffer(LF &
                       //"v1 = 123 + \"//LF &
                       //"     456 + \"//LF &
                       //"     789"//LF &
                       //"v2 = (value1,"//LF &
                       //"      value2,"//LF &
                       //LF &
                       //"      value3,"//LF &
                       //"      value4)"//LF &
                       //"v3 = ('this is a string'"//LF &
                       //LF &
                       //"      'that is continued'"//LF &
                       //"      'between lines'"//LF &
                       //"      'within a paren',"//LF &
                       //"      # this is a comment, yo"//LF &
                       //"      'continue previous line')"//LF &
                       //"v4 = '''"//LF &
                       //"a very long"//LF &
                       //"string"//LF &
                       //"'''"//LF)
        v(1) = point_max()
        call goto_char(v(1))
        v(2) = point()
        call python_nav_backward_statement(1_int64)
        v(3) = point()
        v(4) = v(3)
        call goto_char(v(2))
        v(5) = look_pos("v4 =", -1_int64, .false.)
        call check(error, v(4) == v(5), "(= (save-excursion (python-nav-backward-statement) (point)) ")
        if (allocated(error)) return
        v(6) = point()
        call python_nav_backward_statement(1_int64)
        v(7) = point()
        v(8) = v(7)
        call goto_char(v(6))
        v(9) = look_pos("v3 =", -1_int64, .false.)
        call check(error, v(8) == v(9), "(= (save-excursion (python-nav-backward-statement) (point)) ")
        if (allocated(error)) return
        v(10) = point()
        call python_nav_backward_statement(1_int64)
        v(11) = point()
        v(12) = v(11)
        call goto_char(v(10))
        v(13) = look_pos("v2 =", -1_int64, .false.)
        call check(error, v(12) == v(13), "(= (save-excursion (python-nav-backward-statement) (point)) ")
        if (allocated(error)) return
        v(14) = point()
        call python_nav_backward_statement(1_int64)
        v(15) = point()
        v(16) = v(15)
        call goto_char(v(14))
        v(17) = look_pos("v1 =", -1_int64, .false.)
        call check(error, v(16) == v(17), "(= (save-excursion (python-nav-backward-statement) (point)) ")
        if (allocated(error)) return
        if (.false.) shortage = v(1) + merge(1, 0, b(1))
        if (.false.) name = ""
    end subroutine test_python_nav_backward_statement_1

    subroutine test_python_nav_beginning_of_block_1(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage, v(300)
        logical :: b(300)
        character(len=:), allocatable :: name

        call py_buffer(LF &
                       //"def decoratorFunctionWithArguments(arg1, arg2, arg3):"//LF &
                       //"    '''print decorated function call data to stdout."//LF &
                       //LF &
                       //"    Usage:"//LF &
                       //LF &
                       //"    @decoratorFunctionWithArguments('arg1', 'arg2')"//LF &
                       //"    def func(a, b, c=True):"//LF &
                       //"        pass"//LF &
                       //"    '''"//LF &
                       //LF &
                       //"    def wwrap(f):"//LF &
                       //"        print 'Inside wwrap()'"//LF &
                       //"        def wrapped_f(*args):"//LF &
                       //"            print 'Inside wrapped_f()'"//LF &
                       //"            print 'Decorator arguments:', arg1, arg2, arg3"//LF &
                       //"            f(*args)"//LF &
                       //"            print 'After f(*args)'"//LF &
                       //"        return wrapped_f"//LF &
                       //"    return wwrap"//LF)
        v(1) = look_pos("return wwrap", 1_int64, .false.)
        v(2) = point()
        v(3) = python_nav_beginning_of_block()
        v(4) = point()
        v(5) = v(4)
        call goto_char(v(2))
        v(6) = look_pos("def decoratorFunctionWithArguments", -1_int64, .false.)
        call check(error, v(5) == v(6), "(= (save-excursion (python-nav-beginning-of-block) (point)) ")
        if (allocated(error)) return
        v(7) = look_pos("print 'Inside wwrap()'", 1_int64, .false.)
        v(8) = point()
        v(9) = python_nav_beginning_of_block()
        v(10) = point()
        v(11) = v(10)
        call goto_char(v(8))
        v(12) = look_pos("def wwrap(f):", -1_int64, .false.)
        call check(error, v(11) == v(12), "(= (save-excursion (python-nav-beginning-of-block) (point)) ")
        if (allocated(error)) return
        v(13) = look_pos("print 'After f(*args)'", 1_int64, .false.)
        call end_of_line()
        v(14) = point()
        v(15) = python_nav_beginning_of_block()
        v(16) = point()
        v(17) = v(16)
        call goto_char(v(14))
        v(18) = look_pos("def wrapped_f(*args):", -1_int64, .false.)
        call check(error, v(17) == v(18), "(= (save-excursion (python-nav-beginning-of-block) (point)) ")
        if (allocated(error)) return
        v(19) = look_pos("return wrapped_f", 1_int64, .false.)
        v(20) = point()
        v(21) = python_nav_beginning_of_block()
        v(22) = point()
        v(23) = v(22)
        call goto_char(v(20))
        v(24) = look_pos("def wwrap(f):", -1_int64, .false.)
        call check(error, v(23) == v(24), "(= (save-excursion (python-nav-beginning-of-block) (point)) ")
        if (allocated(error)) return
        if (.false.) shortage = v(1) + merge(1, 0, b(1))
        if (.false.) name = ""
    end subroutine test_python_nav_beginning_of_block_1

    subroutine test_python_nav_beginning_of_block_2(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage, v(300)
        logical :: b(300)
        character(len=:), allocatable :: name

        call py_buffer(LF &
                       //"if True:"//LF &
                       //LF &
                       //"    pass"//LF &
                       //"if False:"//LF &
                       //"    # comment"//LF &
                       //"    pass"//LF)
        v(1) = look_pos("if True:", 1_int64, .false.)
        shortage = forward_line(1_int64)
        v(2) = point()
        v(3) = python_nav_beginning_of_block()
        v(4) = point()
        v(5) = v(4)
        call goto_char(v(2))
        v(6) = look_pos("if True:", -1_int64, .false.)
        call check(error, v(5) == v(6), "(= (save-excursion (python-nav-beginning-of-block) (point)) ")
        if (allocated(error)) return
        v(7) = look_pos("# comment", 1_int64, .false.)
        v(8) = point()
        v(9) = python_nav_beginning_of_block()
        v(10) = point()
        v(11) = v(10)
        call goto_char(v(8))
        v(12) = look_pos("if False:", -1_int64, .false.)
        call check(error, v(11) == v(12), "(= (save-excursion (python-nav-beginning-of-block) (point)) ")
        if (allocated(error)) return
        if (.false.) shortage = v(1) + merge(1, 0, b(1))
        if (.false.) name = ""
    end subroutine test_python_nav_beginning_of_block_2

    subroutine test_python_nav_end_of_block_1(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage, v(300)
        logical :: b(300)
        character(len=:), allocatable :: name

        call py_buffer(LF &
                       //"def decoratorFunctionWithArguments(arg1, arg2, arg3):"//LF &
                       //"    '''print decorated function call data to stdout."//LF &
                       //LF &
                       //"    Usage:"//LF &
                       //LF &
                       //"    @decoratorFunctionWithArguments('arg1', 'arg2')"//LF &
                       //"    def func(a, b, c=True):"//LF &
                       //"        pass"//LF &
                       //"    '''"//LF &
                       //LF &
                       //"    def wwrap(f):"//LF &
                       //"        print 'Inside wwrap()'"//LF &
                       //"        def wrapped_f(*args):"//LF &
                       //"            print 'Inside wrapped_f()'"//LF &
                       //"            print 'Decorator arguments:', arg1, arg2, arg3"//LF &
                       //"            f(*args)"//LF &
                       //"            print 'After f(*args)'"//LF &
                       //"        return wrapped_f"//LF &
                       //"    return wwrap"//LF)
        v(1) = look_pos("def decoratorFunctionWithArguments", 1_int64, .false.)
        v(2) = point()
        v(3) = python_nav_end_of_block()
        v(4) = point()
        v(5) = v(4)
        call goto_char(v(2))
        v(6) = point()
        v(7) = point_max()
        call goto_char(v(7))
        b(300) = python_util_forward_comment(-1)
        v(8) = point()
        v(9) = v(8)
        call goto_char(v(6))
        call check(error, v(5) == v(9), "(= (save-excursion (python-nav-end-of-block) (point)) (save-")
        if (allocated(error)) return
        v(10) = look_pos("def wwrap(f):", 1_int64, .false.)
        v(11) = point()
        v(12) = python_nav_end_of_block()
        v(13) = point()
        v(14) = v(13)
        call goto_char(v(11))
        v(15) = point()
        v(16) = look_pos("return wrapped_f", 1_int64, .false.)
        v(17) = line_end_position()
        v(18) = v(17)
        call goto_char(v(15))
        call check(error, v(14) == v(18), "(= (save-excursion (python-nav-end-of-block) (point)) (save-")
        if (allocated(error)) return
        call end_of_line()
        v(19) = point()
        v(20) = python_nav_end_of_block()
        v(21) = point()
        v(22) = v(21)
        call goto_char(v(19))
        v(23) = point()
        v(24) = look_pos("return wrapped_f", 1_int64, .false.)
        v(25) = line_end_position()
        v(26) = v(25)
        call goto_char(v(23))
        call check(error, v(22) == v(26), "(= (save-excursion (python-nav-end-of-block) (point)) (save-")
        if (allocated(error)) return
        v(27) = look_pos("f(*args)", 1_int64, .false.)
        v(28) = point()
        v(29) = python_nav_end_of_block()
        v(30) = point()
        v(31) = v(30)
        call goto_char(v(28))
        v(32) = point()
        v(33) = look_pos("print 'After f(*args)'", 1_int64, .false.)
        v(34) = line_end_position()
        v(35) = v(34)
        call goto_char(v(32))
        call check(error, v(31) == v(35), "(= (save-excursion (python-nav-end-of-block) (point)) (save-")
        if (allocated(error)) return
        if (.false.) shortage = v(1) + merge(1, 0, b(1))
        if (.false.) name = ""
    end subroutine test_python_nav_end_of_block_1

    subroutine test_python_nav_end_of_block_2(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage, v(300)
        logical :: b(300)
        character(len=:), allocatable :: name

        call py_buffer("def"//LF &
                       //"    =''"//LF &
                       //" '"//LF &
                       //""""""""""""""//LF &
                       //" #"//LF &
                       //"''"//LF)
        v(1) = python_nav_end_of_block()
        if (.false.) shortage = v(1) + merge(1, 0, b(1))
        if (.false.) name = ""
    end subroutine test_python_nav_end_of_block_2

    subroutine test_python_nav_forward_block_1(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage, v(300)
        logical :: b(300)
        character(len=:), allocatable :: name

        call py_buffer(LF &
                       //"if request.user.is_authenticated():"//LF &
                       //"    # def block():"//LF &
                       //"    #     pass"//LF &
                       //"    try:"//LF &
                       //"        profile = request.user.get_profile()"//LF &
                       //"    except Profile.DoesNotExist:"//LF &
                       //"        profile = Profile.objects.create(user=request.user)"//LF &
                       //"    else:"//LF &
                       //"        if profile.stats:"//LF &
                       //"            profile.recalculate_stats()"//LF &
                       //"        else:"//LF &
                       //"            profile.clear_stats()"//LF &
                       //"    finally:"//LF &
                       //"        profile.views += 1"//LF &
                       //"        profile.save()"//LF)
        v(1) = point()
        v(2) = python_nav_forward_block(1_int64)
        v(3) = v(2)
        call goto_char(v(1))
        v(4) = look_pos("if request.user.is_authenticated():", 1_int64, .false.)
        call check(error, v(3) == v(4), "(= (save-excursion (python-nav-forward-block)) (python-tests")
        if (allocated(error)) return
        v(5) = point()
        v(6) = python_nav_forward_block(1_int64)
        v(7) = v(6)
        call goto_char(v(5))
        v(8) = look_pos("try:", 1_int64, .false.)
        call check(error, v(7) == v(8), "(= (save-excursion (python-nav-forward-block)) (python-tests")
        if (allocated(error)) return
        v(9) = point()
        v(10) = python_nav_forward_block(1_int64)
        v(11) = v(10)
        call goto_char(v(9))
        v(12) = look_pos("except Profile.DoesNotExist:", 1_int64, .false.)
        call check(error, v(11) == v(12), "(= (save-excursion (python-nav-forward-block)) (python-tests")
        if (allocated(error)) return
        v(13) = point()
        v(14) = python_nav_forward_block(1_int64)
        v(15) = v(14)
        call goto_char(v(13))
        v(16) = look_pos("else:", 1_int64, .false.)
        call check(error, v(15) == v(16), "(= (save-excursion (python-nav-forward-block)) (python-tests")
        if (allocated(error)) return
        v(17) = point()
        v(18) = python_nav_forward_block(1_int64)
        v(19) = v(18)
        call goto_char(v(17))
        v(20) = look_pos("if profile.stats:", 1_int64, .false.)
        call check(error, v(19) == v(20), "(= (save-excursion (python-nav-forward-block)) (python-tests")
        if (allocated(error)) return
        v(21) = point()
        v(22) = python_nav_forward_block(1_int64)
        v(23) = v(22)
        call goto_char(v(21))
        v(24) = look_pos("else:", 1_int64, .false.)
        call check(error, v(23) == v(24), "(= (save-excursion (python-nav-forward-block)) (python-tests")
        if (allocated(error)) return
        v(25) = point()
        v(26) = python_nav_forward_block(1_int64)
        v(27) = v(26)
        call goto_char(v(25))
        v(28) = look_pos("finally:", 1_int64, .false.)
        call check(error, v(27) == v(28), "(= (save-excursion (python-nav-forward-block)) (python-tests")
        if (allocated(error)) return
        v(29) = point()
        v(30) = python_nav_forward_block(1_int64)
        v(31) = v(30)
        call goto_char(v(29))
        call check(error,.not. (v(31) /= 0), "(not (save-excursion (python-nav-forward-block)))")
        if (allocated(error)) return
        v(32) = point()
        v(33) = python_nav_forward_block(-10_int64)
        v(34) = v(33)
        call goto_char(v(32))
        v(35) = look_pos("if request.user.is_authenticated():", -1_int64, .false.)
        call check(error, v(34) == v(35), "(= (save-excursion (python-nav-forward-block -10)) (python-t")
        if (allocated(error)) return
        if (.false.) shortage = v(1) + merge(1, 0, b(1))
        if (.false.) name = ""
    end subroutine test_python_nav_forward_block_1

    subroutine test_python_nav_forward_expression_2(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage, v(300)
        logical :: b(300)
        character(len=:), allocatable :: name

        call py_buffer(LF &
                       //"def func():"//LF &
                       //"    if True:"//LF &
                       //"        aaa = bbb"//LF &
                       //"        ccc = ddd"//LF &
                       //"        eee = fff"//LF &
                       //"    return ggg"//LF)
        v(1) = look_pos("aa =", 1_int64, .false.)
        call python_nav_forward_expression(1_int64, st)
        b(1) = looking_at(" = bbb", st)
        call check(error, b(1), "(looking-at ' = bbb')")
        if (allocated(error)) return
        call python_nav_forward_expression(1_int64, st)
        b(2) = looking_at("$", st)
        call check(error, b(2), "(looking-at '$')")
        if (allocated(error)) return
        v(2) = point()
        call back_to_indentation()
        b(3) = looking_at("aaa = bbb", st)
        b(4) = b(3)
        call goto_char(v(2))
        call check(error, b(4), "(save-excursion (back-to-indentation) (looking-at 'aaa = bbb")
        if (allocated(error)) return
        call python_nav_forward_expression(1_int64, st)
        b(5) = looking_at("$", st)
        call check(error, b(5), "(looking-at '$')")
        if (allocated(error)) return
        v(3) = point()
        call back_to_indentation()
        b(6) = looking_at("ccc = ddd", st)
        b(7) = b(6)
        call goto_char(v(3))
        call check(error, b(7), "(save-excursion (back-to-indentation) (looking-at 'ccc = ddd")
        if (allocated(error)) return
        call python_nav_forward_expression(1_int64, st)
        b(8) = looking_at("$", st)
        call check(error, b(8), "(looking-at '$')")
        if (allocated(error)) return
        v(4) = point()
        call back_to_indentation()
        b(9) = looking_at("eee = fff", st)
        b(10) = b(9)
        call goto_char(v(4))
        call check(error, b(10), "(save-excursion (back-to-indentation) (looking-at 'eee = fff")
        if (allocated(error)) return
        call python_nav_forward_expression(1_int64, st)
        b(11) = looking_at("$", st)
        call check(error, b(11), "(looking-at '$')")
        if (allocated(error)) return
        v(5) = point()
        call back_to_indentation()
        b(12) = looking_at("return ggg", st)
        b(13) = b(12)
        call goto_char(v(5))
        call check(error, b(13), "(save-excursion (back-to-indentation) (looking-at 'return gg")
        if (allocated(error)) return
        call python_nav_forward_expression(-1_int64, st)
        b(14) = looking_at("def func():", st)
        call check(error, b(14), "(looking-at 'def func():')")
        if (allocated(error)) return
        if (.false.) shortage = v(1) + merge(1, 0, b(1))
        if (.false.) name = ""
    end subroutine test_python_nav_forward_expression_2

    subroutine test_python_nav_forward_expression_3(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage, v(300)
        logical :: b(300)
        character(len=:), allocatable :: name

        call py_buffer(LF &
                       //"from some_module import some_sub_module"//LF &
                       //"from another_module import another_sub_module"//LF &
                       //LF &
                       //"def another_statement():"//LF &
                       //"    pass"//LF)
        v(1) = look_pos("some_module", 1_int64, .false.)
        call python_nav_forward_expression(1_int64, st)
        b(1) = looking_at(" import", st)
        call check(error, b(1), "(looking-at ' import')")
        if (allocated(error)) return
        call python_nav_forward_expression(1_int64, st)
        b(2) = looking_at(" some_sub_module", st)
        call check(error, b(2), "(looking-at ' some_sub_module')")
        if (allocated(error)) return
        call python_nav_forward_expression(1_int64, st)
        b(3) = looking_at("$", st)
        call check(error, b(3), "(looking-at '$')")
        if (allocated(error)) return
        v(2) = point()
        call back_to_indentation()
        b(4) = looking_at("from some_module import some_sub_module", st)
        b(5) = b(4)
        call goto_char(v(2))
        call check(error, b(5), "(save-excursion (back-to-indentation) (looking-at 'from some")
        if (allocated(error)) return
        call python_nav_forward_expression(1_int64, st)
        b(6) = looking_at("$", st)
        call check(error, b(6), "(looking-at '$')")
        if (allocated(error)) return
        v(3) = point()
        call back_to_indentation()
        b(7) = looking_at("from another_module import another_sub_module", st)
        b(8) = b(7)
        call goto_char(v(3))
        call check(error, b(8), "(save-excursion (back-to-indentation) (looking-at 'from anot")
        if (allocated(error)) return
        call python_nav_forward_expression(1_int64, st)
        b(9) = looking_at("$", st)
        call check(error, b(9), "(looking-at '$')")
        if (allocated(error)) return
        v(4) = point()
        call back_to_indentation()
        b(10) = looking_at("pass", st)
        b(11) = b(10)
        call goto_char(v(4))
        call check(error, b(11), "(save-excursion (back-to-indentation) (looking-at 'pass'))")
        if (allocated(error)) return
        call python_nav_forward_expression(-1_int64, st)
        b(12) = looking_at("def another_statement():", st)
        call check(error, b(12), "(looking-at 'def another_statement():')")
        if (allocated(error)) return
        call python_nav_forward_expression(-1_int64, st)
        b(13) = looking_at("from another_module import another_sub_module", st)
        call check(error, b(13), "(looking-at 'from another_module import another_sub_module')")
        if (allocated(error)) return
        call python_nav_forward_expression(-1_int64, st)
        b(14) = looking_at("from some_module import some_sub_module", st)
        call check(error, b(14), "(looking-at 'from some_module import some_sub_module')")
        if (allocated(error)) return
        if (.false.) shortage = v(1) + merge(1, 0, b(1))
        if (.false.) name = ""
    end subroutine test_python_nav_forward_expression_3

    subroutine test_python_nav_up_group_1(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage, v(300)
        logical :: b(300)
        character(len=:), allocatable :: name

        call py_buffer(LF &
                       //"def f():"//LF &
                       //"    if True:"//LF &
                       //"        return [i for i in range(3)]"//LF)
        v(1) = look_pos("3)]", 1_int64, .false.)
        call python_nav_up_group(1_int64, st)
        b(1) = looking_at("]", st)
        call check(error, b(1), "(looking-at ']')")
        if (allocated(error)) return
        call python_nav_up_group(1_int64, st)
        b(2) = looking_at("$", st)
        call check(error, b(2), "(looking-at '$')")
        if (allocated(error)) return
        if (.false.) shortage = v(1) + merge(1, 0, b(1))
        if (.false.) name = ""
    end subroutine test_python_nav_up_group_1

    subroutine test_python_info_current_definition_1(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage, v(300)
        logical :: b(300)
        character(len=:), allocatable :: name

        call py_buffer(LF &
                       //"def foo(a, b):"//LF)
        shortage = forward_line(1_int64)
        call python_info_current_definition(name)
        call check(error, name_is(name, "foo"), "(string= 'foo' (python-info-current-defun))")
        if (allocated(error)) return
        call python_info_current_definition(name, .true.)
        call check(error, name_is(name, "def foo"), "(string= 'def foo' (python-info-current-defun t))")
        if (allocated(error)) return
        shortage = forward_line(1_int64)
        call python_info_current_definition(name)
        call check(error,.not. (allocated(name)), "(not (python-info-current-defun))")
        if (allocated(error)) return
        call indent_for_tab_command(value_type(), st)
        call python_info_current_definition(name)
        call check(error, name_is(name, "foo"), "(string= 'foo' (python-info-current-defun))")
        if (allocated(error)) return
        call python_info_current_definition(name, .true.)
        call check(error, name_is(name, "def foo"), "(string= 'def foo' (python-info-current-defun t))")
        if (allocated(error)) return
        if (.false.) shortage = v(1) + merge(1, 0, b(1))
        if (.false.) name = ""
    end subroutine test_python_info_current_definition_1

    subroutine test_python_info_current_definition_4(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage, v(300)
        logical :: b(300)
        character(len=:), allocatable :: name

        call py_buffer(LF &
                       //"def func():"//LF &
                       //"    ' """""""//LF &
                       //"    v = 1"//LF)
        v(1) = look_pos("v = 1", 1_int64, .false.)
        call python_info_current_definition(name)
        call check(error, name_is(name, "func"), "(string= (python-info-current-defun) 'func')")
        if (allocated(error)) return
        call python_info_current_definition(name, .true.)
        call check(error, name_is(name, "def func"), "(string= (python-info-current-defun t) 'def func')")
        if (allocated(error)) return
        if (.false.) shortage = v(1) + merge(1, 0, b(1))
        if (.false.) name = ""
    end subroutine test_python_info_current_definition_4

    subroutine test_is_python_info_statement_starts_block_1(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage, v(300)
        logical :: b(300)
        character(len=:), allocatable :: name

        call py_buffer(LF &
                       //"def long_function_name("//LF &
                       //"        var_one, var_two, var_three,"//LF &
                       //"        var_four):"//LF &
                       //"    print (var_one)"//LF)
        v(1) = look_pos("def long_function_name", 1_int64, .false.)
        b(1) = is_python_info_statement_starts_block()
        call check(error, b(1), "(python-info-statement-starts-block-p)")
        if (allocated(error)) return
        v(2) = look_pos("print (var_one)", 1_int64, .false.)
        b(300) = python_util_forward_comment(-1)
        b(2) = is_python_info_statement_starts_block()
        call check(error, b(2), "(python-info-statement-starts-block-p)")
        if (allocated(error)) return
        if (.false.) shortage = v(1) + merge(1, 0, b(1))
        if (.false.) name = ""
    end subroutine test_is_python_info_statement_starts_block_1

    subroutine test_is_python_info_statement_starts_block_2(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage, v(300)
        logical :: b(300)
        character(len=:), allocatable :: name

        call py_buffer(LF &
                       //"if width == 0 and height == 0 and \"//LF &
                       //"   color == 'red' and emphasis == 'strong' or \"//LF &
                       //"   highlight > 100:"//LF &
                       //"    raise ValueError('sorry, you lose')"//LF)
        v(1) = look_pos("if width == 0 and", 1_int64, .false.)
        b(1) = is_python_info_statement_starts_block()
        call check(error, b(1), "(python-info-statement-starts-block-p)")
        if (allocated(error)) return
        v(2) = look_pos("raise ValueError(", 1_int64, .false.)
        b(300) = python_util_forward_comment(-1)
        b(2) = is_python_info_statement_starts_block()
        call check(error, b(2), "(python-info-statement-starts-block-p)")
        if (allocated(error)) return
        if (.false.) shortage = v(1) + merge(1, 0, b(1))
        if (.false.) name = ""
    end subroutine test_is_python_info_statement_starts_block_2

    subroutine test_is_python_info_statement_ends_block_1(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage, v(300)
        logical :: b(300)
        character(len=:), allocatable :: name

        call py_buffer(LF &
                       //"def long_function_name("//LF &
                       //"        var_one, var_two, var_three,"//LF &
                       //"        var_four):"//LF &
                       //"    print (var_one)"//LF)
        v(1) = look_pos("print (var_one)", 1_int64, .false.)
        b(1) = is_python_info_statement_ends_block()
        call check(error, b(1), "(python-info-statement-ends-block-p)")
        if (allocated(error)) return
        if (.false.) shortage = v(1) + merge(1, 0, b(1))
        if (.false.) name = ""
    end subroutine test_is_python_info_statement_ends_block_1

    subroutine test_is_python_info_statement_ends_block_2(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage, v(300)
        logical :: b(300)
        character(len=:), allocatable :: name

        call py_buffer(LF &
                       //"if width == 0 and height == 0 and \"//LF &
                       //"   color == 'red' and emphasis == 'strong' or \"//LF &
                       //"   highlight > 100:"//LF &
                       //"    raise ValueError("//LF &
                       //"'sorry, you lose'"//LF &
                       //LF &
                       //")"//LF)
        v(1) = look_pos("raise ValueError(", 1_int64, .false.)
        b(1) = is_python_info_statement_ends_block()
        call check(error, b(1), "(python-info-statement-ends-block-p)")
        if (allocated(error)) return
        if (.false.) shortage = v(1) + merge(1, 0, b(1))
        if (.false.) name = ""
    end subroutine test_is_python_info_statement_ends_block_2

    subroutine test_is_python_info_statement_ends_block_3(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage, v(300)
        logical :: b(300)
        character(len=:), allocatable :: name

        call py_buffer(LF &
                       //"def function():"//LF &
                       //"    print()  # Comment"//LF)
        v(1) = look_pos("print()", 1_int64, .false.)
        b(1) = is_python_info_statement_ends_block()
        call check(error, b(1), "(python-info-statement-ends-block-p)")
        if (allocated(error)) return
        if (.false.) shortage = v(1) + merge(1, 0, b(1))
        if (.false.) name = ""
    end subroutine test_is_python_info_statement_ends_block_3

    subroutine test_is_python_info_beginning_of_statement_1(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage, v(300)
        logical :: b(300)
        character(len=:), allocatable :: name

        call py_buffer(LF &
                       //"def long_function_name("//LF &
                       //"        var_one, var_two, var_three,"//LF &
                       //"        var_four):"//LF &
                       //"    print (var_one)"//LF)
        v(1) = look_pos("def long_function_name", 1_int64, .false.)
        b(1) = is_python_info_beginning_of_statement()
        call check(error, b(1), "(python-info-beginning-of-statement-p)")
        if (allocated(error)) return
        call forward_char(st, 10_int64)
        b(2) = is_python_info_beginning_of_statement()
        call check(error,.not. (b(2)), "(not (python-info-beginning-of-statement-p))")
        if (allocated(error)) return
        v(2) = look_pos("print (var_one)", 1_int64, .false.)
        b(3) = is_python_info_beginning_of_statement()
        call check(error, b(3), "(python-info-beginning-of-statement-p)")
        if (allocated(error)) return
        v(3) = line_beginning_position()
        call goto_char(v(3))
        b(4) = is_python_info_beginning_of_statement()
        call check(error,.not. (b(4)), "(not (python-info-beginning-of-statement-p))")
        if (allocated(error)) return
        if (.false.) shortage = v(1) + merge(1, 0, b(1))
        if (.false.) name = ""
    end subroutine test_is_python_info_beginning_of_statement_1

    subroutine test_is_python_info_beginning_of_statement_2(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage, v(300)
        logical :: b(300)
        character(len=:), allocatable :: name

        call py_buffer(LF &
                       //"if width == 0 and height == 0 and \"//LF &
                       //"   color == 'red' and emphasis == 'strong' or \"//LF &
                       //"   highlight > 100:"//LF &
                       //"    raise ValueError("//LF &
                       //"'sorry, you lose'"//LF &
                       //LF &
                       //")"//LF)
        v(1) = look_pos("if width == 0 and", 1_int64, .false.)
        b(1) = is_python_info_beginning_of_statement()
        call check(error, b(1), "(python-info-beginning-of-statement-p)")
        if (allocated(error)) return
        call forward_char(st, 10_int64)
        b(2) = is_python_info_beginning_of_statement()
        call check(error,.not. (b(2)), "(not (python-info-beginning-of-statement-p))")
        if (allocated(error)) return
        v(2) = look_pos("raise ValueError(", 1_int64, .false.)
        b(3) = is_python_info_beginning_of_statement()
        call check(error, b(3), "(python-info-beginning-of-statement-p)")
        if (allocated(error)) return
        v(3) = line_beginning_position()
        call goto_char(v(3))
        b(4) = is_python_info_beginning_of_statement()
        call check(error,.not. (b(4)), "(not (python-info-beginning-of-statement-p))")
        if (allocated(error)) return
        if (.false.) shortage = v(1) + merge(1, 0, b(1))
        if (.false.) name = ""
    end subroutine test_is_python_info_beginning_of_statement_2

    subroutine test_is_python_info_end_of_statement_1(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage, v(300)
        logical :: b(300)
        character(len=:), allocatable :: name

        call py_buffer(LF &
                       //"def long_function_name("//LF &
                       //"        var_one, var_two, var_three,"//LF &
                       //"        var_four):"//LF &
                       //"    print (var_one)"//LF)
        v(1) = look_pos("def long_function_name", 1_int64, .false.)
        b(1) = is_python_info_end_of_statement()
        call check(error,.not. (b(1)), "(not (python-info-end-of-statement-p))")
        if (allocated(error)) return
        call end_of_line()
        b(2) = is_python_info_end_of_statement()
        call check(error,.not. (b(2)), "(not (python-info-end-of-statement-p))")
        if (allocated(error)) return
        v(2) = look_pos("print (var_one)", 1_int64, .false.)
        b(300) = python_util_forward_comment(-1)
        b(3) = is_python_info_end_of_statement()
        call check(error, b(3), "(python-info-end-of-statement-p)")
        if (allocated(error)) return
        v(3) = look_pos("print (var_one)", 1_int64, .false.)
        b(4) = is_python_info_end_of_statement()
        call check(error,.not. (b(4)), "(not (python-info-end-of-statement-p))")
        if (allocated(error)) return
        call end_of_line()
        b(5) = is_python_info_end_of_statement()
        call check(error, b(5), "(python-info-end-of-statement-p)")
        if (allocated(error)) return
        if (.false.) shortage = v(1) + merge(1, 0, b(1))
        if (.false.) name = ""
    end subroutine test_is_python_info_end_of_statement_1

    subroutine test_is_python_info_end_of_statement_2(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage, v(300)
        logical :: b(300)
        character(len=:), allocatable :: name

        call py_buffer(LF &
                       //"if width == 0 and height == 0 and \"//LF &
                       //"   color == 'red' and emphasis == 'strong' or \"//LF &
                       //"   highlight > 100:"//LF &
                       //"    raise ValueError("//LF &
                       //"'sorry, you lose'"//LF &
                       //LF &
                       //")"//LF)
        v(1) = look_pos("if width == 0 and", 1_int64, .false.)
        b(1) = is_python_info_end_of_statement()
        call check(error,.not. (b(1)), "(not (python-info-end-of-statement-p))")
        if (allocated(error)) return
        call end_of_line()
        b(2) = is_python_info_end_of_statement()
        call check(error,.not. (b(2)), "(not (python-info-end-of-statement-p))")
        if (allocated(error)) return
        v(2) = look_pos("raise ValueError(", 1_int64, .false.)
        b(300) = python_util_forward_comment(-1)
        b(3) = is_python_info_end_of_statement()
        call check(error, b(3), "(python-info-end-of-statement-p)")
        if (allocated(error)) return
        v(3) = look_pos("raise ValueError(", 1_int64, .false.)
        b(4) = is_python_info_end_of_statement()
        call check(error,.not. (b(4)), "(not (python-info-end-of-statement-p))")
        if (allocated(error)) return
        call end_of_line()
        b(5) = is_python_info_end_of_statement()
        call check(error,.not. (b(5)), "(not (python-info-end-of-statement-p))")
        if (allocated(error)) return
        v(4) = point_max()
        call goto_char(v(4))
        b(300) = python_util_forward_comment(-1)
        b(6) = is_python_info_end_of_statement()
        call check(error, b(6), "(python-info-end-of-statement-p)")
        if (allocated(error)) return
        if (.false.) shortage = v(1) + merge(1, 0, b(1))
        if (.false.) name = ""
    end subroutine test_is_python_info_end_of_statement_2

    subroutine test_is_python_info_beginning_of_block_1(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage, v(300)
        logical :: b(300)
        character(len=:), allocatable :: name

        call py_buffer(LF &
                       //"def long_function_name("//LF &
                       //"        var_one, var_two, var_three,"//LF &
                       //"        var_four):"//LF &
                       //"    print (var_one)"//LF)
        v(1) = look_pos("def long_function_name", 1_int64, .false.)
        b(1) = is_python_info_beginning_of_block()
        call check(error, b(1), "(python-info-beginning-of-block-p)")
        if (allocated(error)) return
        v(2) = look_pos("var_one, var_two, var_three,", 1_int64, .false.)
        b(2) = is_python_info_beginning_of_block()
        call check(error,.not. (b(2)), "(not (python-info-beginning-of-block-p))")
        if (allocated(error)) return
        v(3) = look_pos("print (var_one)", 1_int64, .false.)
        b(3) = is_python_info_beginning_of_block()
        call check(error,.not. (b(3)), "(not (python-info-beginning-of-block-p))")
        if (allocated(error)) return
        if (.false.) shortage = v(1) + merge(1, 0, b(1))
        if (.false.) name = ""
    end subroutine test_is_python_info_beginning_of_block_1

    subroutine test_is_python_info_beginning_of_block_2(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage, v(300)
        logical :: b(300)
        character(len=:), allocatable :: name

        call py_buffer(LF &
                       //"if width == 0 and height == 0 and \"//LF &
                       //"   color == 'red' and emphasis == 'strong' or \"//LF &
                       //"   highlight > 100:"//LF &
                       //"    raise ValueError("//LF &
                       //"'sorry, you lose'"//LF &
                       //LF &
                       //")"//LF)
        v(1) = look_pos("if width == 0 and", 1_int64, .false.)
        b(1) = is_python_info_beginning_of_block()
        call check(error, b(1), "(python-info-beginning-of-block-p)")
        if (allocated(error)) return
        v(2) = look_pos("color == 'red' and emphasis", 1_int64, .false.)
        b(2) = is_python_info_beginning_of_block()
        call check(error,.not. (b(2)), "(not (python-info-beginning-of-block-p))")
        if (allocated(error)) return
        v(3) = look_pos("raise ValueError(", 1_int64, .false.)
        b(3) = is_python_info_beginning_of_block()
        call check(error,.not. (b(3)), "(not (python-info-beginning-of-block-p))")
        if (allocated(error)) return
        if (.false.) shortage = v(1) + merge(1, 0, b(1))
        if (.false.) name = ""
    end subroutine test_is_python_info_beginning_of_block_2

    subroutine test_is_python_info_end_of_block_3(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage, v(300)
        logical :: b(300)
        character(len=:), allocatable :: name

        call py_buffer(LF &
                       //"def function():"//LF &
                       //"    print()  # Comment"//LF)
        v(1) = look_pos("  # Comment", 1_int64, .false.)
        b(1) = is_python_info_end_of_block()
        call check(error, b(1), "(python-info-end-of-block-p)")
        if (allocated(error)) return
        if (.false.) shortage = v(1) + merge(1, 0, b(1))
        if (.false.) name = ""
    end subroutine test_is_python_info_end_of_block_3

    subroutine test_python_info_dedenter_opening_block_position_1(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage, v(300)
        logical :: b(300)
        character(len=:), allocatable :: name

        call py_buffer(LF &
                       //"if request.user.is_authenticated():"//LF &
                       //"    try:"//LF &
                       //"        profile = request.user.get_profile()"//LF &
                       //"    except Profile.DoesNotExist:"//LF &
                       //"        profile = Profile.objects.create(user=request.user)"//LF &
                       //"    else:"//LF &
                       //"        if profile.stats:"//LF &
                       //"            profile.recalculate_stats()"//LF &
                       //"        else:"//LF &
                       //"            profile.clear_stats()"//LF &
                       //"    finally:"//LF &
                       //"        profile.views += 1"//LF &
                       //"        profile.save()"//LF)
        v(1) = look_pos("try:", 1_int64, .false.)
        v(2) = python_info_dedenter_opening_block_position()
        call check(error,.not. (v(2) /= 0), "(not (python-info-dedenter-opening-block-position))")
        if (allocated(error)) return
        v(3) = look_pos("except Profile.DoesNotExist:", 1_int64, .false.)
        v(4) = look_pos("try:", -1_int64, .true.)
        v(5) = python_info_dedenter_opening_block_position()
        call check(error, v(4) == v(5), "(= (python-tests-look-at 'try:' -1 t) (python-info-dedenter-")
        if (allocated(error)) return
        v(6) = look_pos("else:", 1_int64, .false.)
        v(7) = look_pos("except Profile.DoesNotExist:", -1_int64, .true.)
        v(8) = python_info_dedenter_opening_block_position()
        call check(error, v(7) == v(8), "(= (python-tests-look-at 'except Profile.DoesNotExist:' -1 t")
        if (allocated(error)) return
        v(9) = look_pos("if profile.stats:", 1_int64, .false.)
        v(10) = python_info_dedenter_opening_block_position()
        call check(error,.not. (v(10) /= 0), "(not (python-info-dedenter-opening-block-position))")
        if (allocated(error)) return
        v(11) = look_pos("else:", 1_int64, .false.)
        v(12) = look_pos("if profile.stats:", -1_int64, .true.)
        v(13) = python_info_dedenter_opening_block_position()
        call check(error, v(12) == v(13), "(= (python-tests-look-at 'if profile.stats:' -1 t) (python-i")
        if (allocated(error)) return
        v(14) = look_pos("finally:", 1_int64, .false.)
        v(15) = look_pos("else:", -2_int64, .true.)
        v(16) = python_info_dedenter_opening_block_position()
        call check(error, v(15) == v(16), "(= (python-tests-look-at 'else:' -2 t) (python-info-dedenter")
        if (allocated(error)) return
        if (.false.) shortage = v(1) + merge(1, 0, b(1))
        if (.false.) name = ""
    end subroutine test_python_info_dedenter_opening_block_position_1

    subroutine test_python_info_dedenter_opening_block_position_2(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage, v(300)
        logical :: b(300)
        character(len=:), allocatable :: name

        call py_buffer(LF &
                       //"if request.user.is_authenticated():"//LF &
                       //"    profile = Profile.objects.get_or_create(user=request.user)"//LF &
                       //"    if profile.stats:"//LF &
                       //"        profile.recalculate_stats()"//LF &
                       //LF &
                       //"data = {"//LF &
                       //"    'else': 'do it'"//LF &
                       //"}"//LF &
                       //"    'else'"//LF)
        v(1) = look_pos("'else': 'do it'", 1_int64, .false.)
        v(2) = python_info_dedenter_opening_block_position()
        call check(error,.not. (v(2) /= 0), "(not (python-info-dedenter-opening-block-position))")
        if (allocated(error)) return
        v(3) = look_pos("'else'", 1_int64, .false.)
        v(4) = python_info_dedenter_opening_block_position()
        call check(error,.not. (v(4) /= 0), "(not (python-info-dedenter-opening-block-position))")
        if (allocated(error)) return
        if (.false.) shortage = v(1) + merge(1, 0, b(1))
        if (.false.) name = ""
    end subroutine test_python_info_dedenter_opening_block_position_2

    subroutine test_is_python_info_dedenter_statement_1(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage, v(300)
        logical :: b(300)
        character(len=:), allocatable :: name

        call py_buffer("'''"//LF &
                       //"try:"//LF &
                       //"    something()"//LF &
                       //"except:"//LF &
                       //"    logger.exception('something went wrong')"//LF &
                       //"'''"//LF)
        v(1) = look_pos("except"//LF, 1_int64, .false.)
        v(2) = is_python_info_dedenter_statement()
        call check(error,.not. (v(2) /= 0), "(not (python-info-dedenter-statement-p))")
        if (allocated(error)) return
        if (.false.) shortage = v(1) + merge(1, 0, b(1))
        if (.false.) name = ""
    end subroutine test_is_python_info_dedenter_statement_1

    subroutine test_is_python_info_dedenter_statement_2(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage, v(300)
        logical :: b(300)
        character(len=:), allocatable :: name

        call py_buffer(LF &
                       //"try:"//LF &
                       //"    something()"//LF &
                       //"except:"//LF &
                       //"    logger.exception('something went wrong')"//LF)
        v(1) = look_pos("except:", 1_int64, .false.)
        v(2) = point()
        v(3) = is_python_info_dedenter_statement()
        call check(error, v(2) == v(3), "(= (point) (python-info-dedenter-statement-p))")
        if (allocated(error)) return
        call end_of_line()
        v(4) = point()
        call back_to_indentation()
        v(5) = point()
        v(6) = v(5)
        call goto_char(v(4))
        v(7) = is_python_info_dedenter_statement()
        call check(error, v(6) == v(7), "(= (save-excursion (back-to-indentation) (point)) (python-in")
        if (allocated(error)) return
        if (.false.) shortage = v(1) + merge(1, 0, b(1))
        if (.false.) name = ""
    end subroutine test_is_python_info_dedenter_statement_2

    subroutine test_is_python_info_dedenter_statement_3(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage, v(300)
        logical :: b(300)
        character(len=:), allocatable :: name

        call py_buffer(LF &
                       //"try:"//LF &
                       //"    something()"//LF &
                       //"except:"//LF &
                       //"    logger.exception('something went wrong')"//LF &
                       //"else:"//LF &
                       //"    logger.debug('all good')"//LF)
        v(1) = look_pos("else:", 1_int64, .false.)
        v(2) = point()
        v(3) = is_python_info_dedenter_statement()
        call check(error, v(2) == v(3), "(= (point) (python-info-dedenter-statement-p))")
        if (allocated(error)) return
        call end_of_line()
        v(4) = point()
        call back_to_indentation()
        v(5) = point()
        v(6) = v(5)
        call goto_char(v(4))
        v(7) = is_python_info_dedenter_statement()
        call check(error, v(6) == v(7), "(= (save-excursion (back-to-indentation) (point)) (python-in")
        if (allocated(error)) return
        if (.false.) shortage = v(1) + merge(1, 0, b(1))
        if (.false.) name = ""
    end subroutine test_is_python_info_dedenter_statement_3

    subroutine test_is_python_info_dedenter_statement_4(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage, v(300)
        logical :: b(300)
        character(len=:), allocatable :: name

        call py_buffer(LF &
                       //"try:"//LF &
                       //"    something()"//LF &
                       //"except:"//LF &
                       //"    logger.exception('something went wrong')"//LF &
                       //"else:"//LF &
                       //"    logger.debug('all good')"//LF &
                       //"finally:"//LF &
                       //"    clean()"//LF)
        v(1) = look_pos("finally:", 1_int64, .false.)
        v(2) = point()
        v(3) = is_python_info_dedenter_statement()
        call check(error, v(2) == v(3), "(= (point) (python-info-dedenter-statement-p))")
        if (allocated(error)) return
        call end_of_line()
        v(4) = point()
        call back_to_indentation()
        v(5) = point()
        v(6) = v(5)
        call goto_char(v(4))
        v(7) = is_python_info_dedenter_statement()
        call check(error, v(6) == v(7), "(= (save-excursion (back-to-indentation) (point)) (python-in")
        if (allocated(error)) return
        if (.false.) shortage = v(1) + merge(1, 0, b(1))
        if (.false.) name = ""
    end subroutine test_is_python_info_dedenter_statement_4

    subroutine test_is_python_info_dedenter_statement_5(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage, v(300)
        logical :: b(300)
        character(len=:), allocatable :: name

        call py_buffer(LF &
                       //"if a:"//LF &
                       //"    something()"//LF &
                       //"elif b:"//LF)
        v(1) = look_pos("elif b:", 1_int64, .false.)
        v(2) = point()
        v(3) = is_python_info_dedenter_statement()
        call check(error, v(2) == v(3), "(= (point) (python-info-dedenter-statement-p))")
        if (allocated(error)) return
        call end_of_line()
        v(4) = point()
        call back_to_indentation()
        v(5) = point()
        v(6) = v(5)
        call goto_char(v(4))
        v(7) = is_python_info_dedenter_statement()
        call check(error, v(6) == v(7), "(= (save-excursion (back-to-indentation) (point)) (python-in")
        if (allocated(error)) return
        if (.false.) shortage = v(1) + merge(1, 0, b(1))
        if (.false.) name = ""
    end subroutine test_is_python_info_dedenter_statement_5

    subroutine test_is_python_info_dedenter_statement_6(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage, v(300)
        logical :: b(300)
        character(len=:), allocatable :: name

        call py_buffer(LF &
                       //"match a:  # Comment"//LF &
                       //"    case 1:"//LF &
                       //"        match b:"//LF &
                       //"            case 2:"//LF &
                       //"                something()"//LF &
                       //"            case 3:"//LF)
        v(1) = look_pos("case 1:", 1_int64, .false.)
        v(2) = is_python_info_dedenter_statement()
        call check(error,.not. (v(2) /= 0), "(not (python-info-dedenter-statement-p))")
        if (allocated(error)) return
        v(3) = look_pos("case 2:", 1_int64, .false.)
        v(4) = is_python_info_dedenter_statement()
        call check(error,.not. (v(4) /= 0), "(not (python-info-dedenter-statement-p))")
        if (allocated(error)) return
        v(5) = look_pos("case 3:", 1_int64, .false.)
        v(6) = point()
        v(7) = is_python_info_dedenter_statement()
        call check(error, v(6) == v(7), "(= (point) (python-info-dedenter-statement-p))")
        if (allocated(error)) return
        if (.false.) shortage = v(1) + merge(1, 0, b(1))
        if (.false.) name = ""
    end subroutine test_is_python_info_dedenter_statement_6

    subroutine test_is_python_info_line_ends_backslash_1(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage, v(300)
        logical :: b(300)
        character(len=:), allocatable :: name

        call py_buffer(LF &
                       //"objects = Thing.objects.all() \"//LF &
                       //"                       .filter("//LF &
                       //"                           type='toy',"//LF &
                       //"                           status='bought'"//LF &
                       //"                       ) \"//LF &
                       //"                       .aggregate("//LF &
                       //"                           Sum('amount')"//LF &
                       //"                       ) \"//LF &
                       //"                       .values_list()"//LF)
        v(1) = is_python_info_line_ends_backslash(2_int64)
        call check(error, v(1) /= 0, "(python-info-line-ends-backslash-p 2)")
        if (allocated(error)) return
        v(2) = is_python_info_line_ends_backslash(3_int64)
        call check(error, v(2) /= 0, "(python-info-line-ends-backslash-p 3)")
        if (allocated(error)) return
        v(3) = is_python_info_line_ends_backslash(4_int64)
        call check(error, v(3) /= 0, "(python-info-line-ends-backslash-p 4)")
        if (allocated(error)) return
        v(4) = is_python_info_line_ends_backslash(5_int64)
        call check(error, v(4) /= 0, "(python-info-line-ends-backslash-p 5)")
        if (allocated(error)) return
        v(5) = is_python_info_line_ends_backslash(6_int64)
        call check(error, v(5) /= 0, "(python-info-line-ends-backslash-p 6)")
        if (allocated(error)) return
        v(6) = is_python_info_line_ends_backslash(7_int64)
        call check(error, v(6) /= 0, "(python-info-line-ends-backslash-p 7)")
        if (allocated(error)) return
        v(7) = is_python_info_line_ends_backslash(8_int64)
        call check(error, v(7) /= 0, "(python-info-line-ends-backslash-p 8)")
        if (allocated(error)) return
        v(8) = is_python_info_line_ends_backslash(9_int64)
        call check(error, v(8) /= 0, "(python-info-line-ends-backslash-p 9)")
        if (allocated(error)) return
        v(9) = is_python_info_line_ends_backslash(10_int64)
        call check(error,.not. (v(9) /= 0), "(not (python-info-line-ends-backslash-p 10))")
        if (allocated(error)) return
        if (.false.) shortage = v(1) + merge(1, 0, b(1))
        if (.false.) name = ""
    end subroutine test_is_python_info_line_ends_backslash_1

    subroutine test_is_python_info_continuation_line_1(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage, v(300)
        logical :: b(300)
        character(len=:), allocatable :: name

        call py_buffer(LF &
                       //"if width == 0 and height == 0 and \"//LF &
                       //"   color == 'red' and emphasis == 'strong' or \"//LF &
                       //"   highlight > 100:"//LF &
                       //"    raise ValueError("//LF &
                       //"'sorry, you lose'"//LF &
                       //LF &
                       //")"//LF)
        v(1) = look_pos("if width == 0 and height == 0 and", 1_int64, .false.)
        v(2) = is_python_info_continuation_line()
        call check(error,.not. (v(2) /= 0), "(not (python-info-continuation-line-p))")
        if (allocated(error)) return
        v(3) = look_pos("color == 'red' and emphasis == 'strong' or", 1_int64, .false.)
        v(4) = is_python_info_continuation_line()
        call check(error, v(4) /= 0, "(python-info-continuation-line-p)")
        if (allocated(error)) return
        v(5) = look_pos("highlight > 100:", 1_int64, .false.)
        v(6) = is_python_info_continuation_line()
        call check(error, v(6) /= 0, "(python-info-continuation-line-p)")
        if (allocated(error)) return
        v(7) = look_pos("raise ValueError(", 1_int64, .false.)
        v(8) = is_python_info_continuation_line()
        call check(error,.not. (v(8) /= 0), "(not (python-info-continuation-line-p))")
        if (allocated(error)) return
        v(9) = look_pos("'sorry, you lose'", 1_int64, .false.)
        v(10) = is_python_info_continuation_line()
        call check(error, v(10) /= 0, "(python-info-continuation-line-p)")
        if (allocated(error)) return
        shortage = forward_line(1_int64)
        v(11) = is_python_info_continuation_line()
        call check(error, v(11) /= 0, "(python-info-continuation-line-p)")
        if (allocated(error)) return
        v(12) = look_pos(")", 1_int64, .false.)
        v(13) = is_python_info_continuation_line()
        call check(error, v(13) /= 0, "(python-info-continuation-line-p)")
        if (allocated(error)) return
        shortage = forward_line(1_int64)
        v(14) = is_python_info_continuation_line()
        call check(error,.not. (v(14) /= 0), "(not (python-info-continuation-line-p))")
        if (allocated(error)) return
        if (.false.) shortage = v(1) + merge(1, 0, b(1))
        if (.false.) name = ""
    end subroutine test_is_python_info_continuation_line_1

    subroutine test_is_python_info_block_continuation_line_1(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage, v(300)
        logical :: b(300)
        character(len=:), allocatable :: name

        call py_buffer(LF &
                       //"if width == 0 and height == 0 and \"//LF &
                       //"   color == 'red' and emphasis == 'strong' or \"//LF &
                       //"   highlight > 100:"//LF &
                       //"    raise ValueError("//LF &
                       //"'sorry, you lose'"//LF &
                       //LF &
                       //")"//LF)
        v(1) = look_pos("if width == 0 and", 1_int64, .false.)
        v(2) = is_python_info_block_continuation_line()
        call check(error,.not. (v(2) /= 0), "(not (python-info-block-continuation-line-p))")
        if (allocated(error)) return
        v(3) = look_pos("color == 'red' and emphasis == 'strong' or", 1_int64, .false.)
        v(4) = is_python_info_block_continuation_line()
        v(5) = look_pos("if width == 0 and", -1_int64, .true.)
        call check(error, v(4) == v(5), "(= (python-info-block-continuation-line-p) (python-tests-loo")
        if (allocated(error)) return
        v(6) = look_pos("highlight > 100:", 1_int64, .false.)
        v(7) = is_python_info_block_continuation_line()
        call check(error,.not. (v(7) /= 0), "(not (python-info-block-continuation-line-p))")
        if (allocated(error)) return
        if (.false.) shortage = v(1) + merge(1, 0, b(1))
        if (.false.) name = ""
    end subroutine test_is_python_info_block_continuation_line_1

    subroutine test_is_python_info_block_continuation_line_2(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage, v(300)
        logical :: b(300)
        character(len=:), allocatable :: name

        call py_buffer(LF &
                       //"def foo(a,"//LF &
                       //"        b,"//LF &
                       //"        c):"//LF &
                       //"    pass"//LF)
        v(1) = look_pos("def foo(a,", 1_int64, .false.)
        v(2) = is_python_info_block_continuation_line()
        call check(error,.not. (v(2) /= 0), "(not (python-info-block-continuation-line-p))")
        if (allocated(error)) return
        v(3) = look_pos("b,", 1_int64, .false.)
        v(4) = is_python_info_block_continuation_line()
        v(5) = look_pos("def foo(a,", -1_int64, .true.)
        call check(error, v(4) == v(5), "(= (python-info-block-continuation-line-p) (python-tests-loo")
        if (allocated(error)) return
        v(6) = look_pos("c):", 1_int64, .false.)
        v(7) = is_python_info_block_continuation_line()
        call check(error,.not. (v(7) /= 0), "(not (python-info-block-continuation-line-p))")
        if (allocated(error)) return
        if (.false.) shortage = v(1) + merge(1, 0, b(1))
        if (.false.) name = ""
    end subroutine test_is_python_info_block_continuation_line_2

    subroutine test_is_python_info_assignment_continuation_line_1(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage, v(300)
        logical :: b(300)
        character(len=:), allocatable :: name

        call py_buffer(LF &
                       //"data = foo(), bar() \"//LF &
                       //"       baz(), 4 \"//LF &
                       //"       5, 6"//LF)
        v(1) = look_pos("data = foo(), bar()", 1_int64, .false.)
        v(2) = is_python_info_assignment_continuation_line()
        call check(error,.not. (v(2) /= 0), "(not (python-info-assignment-continuation-line-p))")
        if (allocated(error)) return
        v(3) = look_pos("baz(), 4", 1_int64, .false.)
        v(4) = is_python_info_assignment_continuation_line()
        v(5) = look_pos("foo(),", -1_int64, .true.)
        call check(error, v(4) == v(5), "(= (python-info-assignment-continuation-line-p) (python-test")
        if (allocated(error)) return
        v(6) = look_pos("5, 6", 1_int64, .false.)
        v(7) = is_python_info_assignment_continuation_line()
        call check(error,.not. (v(7) /= 0), "(not (python-info-assignment-continuation-line-p))")
        if (allocated(error)) return
        if (.false.) shortage = v(1) + merge(1, 0, b(1))
        if (.false.) name = ""
    end subroutine test_is_python_info_assignment_continuation_line_1

    subroutine test_is_python_info_assignment_continuation_line_2(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage, v(300)
        logical :: b(300)
        character(len=:), allocatable :: name

        call py_buffer(LF &
                       //"data = (foo(), bar()"//LF &
                       //"        baz(), 4"//LF &
                       //"        5, 6)"//LF)
        v(1) = look_pos("data = (foo(), bar()", 1_int64, .false.)
        v(2) = is_python_info_assignment_continuation_line()
        call check(error,.not. (v(2) /= 0), "(not (python-info-assignment-continuation-line-p))")
        if (allocated(error)) return
        v(3) = look_pos("baz(), 4", 1_int64, .false.)
        v(4) = is_python_info_assignment_continuation_line()
        v(5) = look_pos("(foo(),", -1_int64, .true.)
        call check(error, v(4) == v(5), "(= (python-info-assignment-continuation-line-p) (python-test")
        if (allocated(error)) return
        v(6) = look_pos("5, 6)", 1_int64, .false.)
        v(7) = is_python_info_assignment_continuation_line()
        call check(error,.not. (v(7) /= 0), "(not (python-info-assignment-continuation-line-p))")
        if (allocated(error)) return
        if (.false.) shortage = v(1) + merge(1, 0, b(1))
        if (.false.) name = ""
    end subroutine test_is_python_info_assignment_continuation_line_2

    subroutine test_python_info_looking_at_beginning_of_definition_1(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage, v(300)
        logical :: b(300)
        character(len=:), allocatable :: name

        call py_buffer(LF &
                       //"def decorat0r(deff):"//LF &
                       //"    '''decorates stuff."//LF &
                       //LF &
                       //"    @decorat0r"//LF &
                       //"    def foo(arg):"//LF &
                       //"        ..."//LF &
                       //"    '''"//LF &
                       //"    def wrap():"//LF &
                       //"        deff()"//LF &
                       //"    return wwrap"//LF)
        v(1) = look_pos("def decorat0r(deff):", 1_int64, .false.)
        b(1) = python_info_looking_at_beginning_of_definition()
        call check(error, b(1), "(python-info-looking-at-beginning-of-defun)")
        if (allocated(error)) return
        v(2) = look_pos("def foo(arg):", 1_int64, .false.)
        b(2) = python_info_looking_at_beginning_of_definition()
        call check(error,.not. (b(2)), "(not (python-info-looking-at-beginning-of-defun))")
        if (allocated(error)) return
        v(3) = look_pos("def wrap():", 1_int64, .false.)
        b(3) = python_info_looking_at_beginning_of_definition()
        call check(error, b(3), "(python-info-looking-at-beginning-of-defun)")
        if (allocated(error)) return
        v(4) = look_pos("deff()", 1_int64, .false.)
        b(4) = python_info_looking_at_beginning_of_definition()
        call check(error,.not. (b(4)), "(not (python-info-looking-at-beginning-of-defun))")
        if (allocated(error)) return
        if (.false.) shortage = v(1) + merge(1, 0, b(1))
        if (.false.) name = ""
    end subroutine test_python_info_looking_at_beginning_of_definition_1

    subroutine test_python_info_looking_at_beginning_of_definition_2(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage, v(300)
        logical :: b(300)
        character(len=:), allocatable :: name

        call py_buffer(LF &
                       //"def \"//LF &
                       //"        foo(arg):"//LF &
                       //"    pass"//LF)
        v(1) = look_pos("def \", 1_int64, .false.)
        b(1) = python_info_looking_at_beginning_of_definition()
        call check(error, b(1), "(python-info-looking-at-beginning-of-defun)")
        if (allocated(error)) return
        b(2) = python_info_looking_at_beginning_of_definition(check_statement=.true.)
        call check(error, b(2), "(python-info-looking-at-beginning-of-defun nil t)")
        if (allocated(error)) return
        v(2) = look_pos("foo(arg):", 1_int64, .false.)
        b(3) = python_info_looking_at_beginning_of_definition()
        call check(error,.not. (b(3)), "(not (python-info-looking-at-beginning-of-defun))")
        if (allocated(error)) return
        b(4) = python_info_looking_at_beginning_of_definition(check_statement=.true.)
        call check(error, b(4), "(python-info-looking-at-beginning-of-defun nil t)")
        if (allocated(error)) return
        v(3) = look_pos("pass", 1_int64, .false.)
        b(5) = python_info_looking_at_beginning_of_definition()
        call check(error,.not. (b(5)), "(not (python-info-looking-at-beginning-of-defun))")
        if (allocated(error)) return
        b(6) = python_info_looking_at_beginning_of_definition(check_statement=.true.)
        call check(error,.not. (b(6)), "(not (python-info-looking-at-beginning-of-defun nil t))")
        if (allocated(error)) return
        if (.false.) shortage = v(1) + merge(1, 0, b(1))
        if (.false.) name = ""
    end subroutine test_python_info_looking_at_beginning_of_definition_2

    subroutine test_python_info_looking_at_beginning_of_definition_3(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage, v(300)
        logical :: b(300)
        character(len=:), allocatable :: name

        call py_buffer(LF &
                       //"def foo(arg=""default""):  # Comment"//LF &
                       //"    pass"//LF)
        v(1) = look_pos("arg", 1_int64, .false.)
        b(1) = python_info_looking_at_beginning_of_definition()
        call check(error, b(1), "(python-info-looking-at-beginning-of-defun)")
        if (allocated(error)) return
        v(2) = look_pos("default", 1_int64, .false.)
        b(2) = python_info_looking_at_beginning_of_definition()
        call check(error, b(2), "(python-info-looking-at-beginning-of-defun)")
        if (allocated(error)) return
        v(3) = look_pos("Comment", 1_int64, .false.)
        b(3) = python_info_looking_at_beginning_of_definition()
        call check(error, b(3), "(python-info-looking-at-beginning-of-defun)")
        if (allocated(error)) return
        if (.false.) shortage = v(1) + merge(1, 0, b(1))
        if (.false.) name = ""
    end subroutine test_python_info_looking_at_beginning_of_definition_3

    subroutine test_is_python_info_current_line_comment_1(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage, v(300)
        logical :: b(300)
        character(len=:), allocatable :: name

        call py_buffer(LF &
                       //"# this is a comment"//LF &
                       //"foo = True  # another comment"//LF &
                       //"'#this is a string'"//LF &
                       //"if foo:"//LF &
                       //"    # more comments"//LF &
                       //"    print ('bar') # print bar"//LF)
        v(1) = look_pos("# this is a comment", 1_int64, .false.)
        b(1) = is_python_info_current_line_comment()
        call check(error, b(1), "(python-info-current-line-comment-p)")
        if (allocated(error)) return
        v(2) = look_pos("foo = True  # another comment", 1_int64, .false.)
        b(2) = is_python_info_current_line_comment()
        call check(error,.not. (b(2)), "(not (python-info-current-line-comment-p))")
        if (allocated(error)) return
        v(3) = look_pos("'#this is a string'", 1_int64, .false.)
        b(3) = is_python_info_current_line_comment()
        call check(error,.not. (b(3)), "(not (python-info-current-line-comment-p))")
        if (allocated(error)) return
        v(4) = look_pos("# more comments", 1_int64, .false.)
        b(4) = is_python_info_current_line_comment()
        call check(error, b(4), "(python-info-current-line-comment-p)")
        if (allocated(error)) return
        v(5) = look_pos("print ('bar') # print bar", 1_int64, .false.)
        b(5) = is_python_info_current_line_comment()
        call check(error,.not. (b(5)), "(not (python-info-current-line-comment-p))")
        if (allocated(error)) return
        if (.false.) shortage = v(1) + merge(1, 0, b(1))
        if (.false.) name = ""
    end subroutine test_is_python_info_current_line_comment_1

    subroutine test_is_python_info_current_line_empty(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage, v(300)
        logical :: b(300)
        character(len=:), allocatable :: name

        call py_buffer(LF &
                       //"# this is a comment"//LF &
                       //LF &
                       //"foo = True  # another comment"//LF)
        b(1) = is_python_info_current_line_empty()
        call check(error, b(1), "(python-info-current-line-empty-p)")
        if (allocated(error)) return
        v(1) = look_pos("# this is a comment", 1_int64, .false.)
        b(2) = is_python_info_current_line_empty()
        call check(error,.not. (b(2)), "(not (python-info-current-line-empty-p))")
        if (allocated(error)) return
        shortage = forward_line(1_int64)
        b(3) = is_python_info_current_line_empty()
        call check(error, b(3), "(python-info-current-line-empty-p)")
        if (allocated(error)) return
        if (.false.) shortage = v(1) + merge(1, 0, b(1))
        if (.false.) name = ""
    end subroutine test_is_python_info_current_line_empty

    subroutine test_is_python_info_docstring_1(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage, v(300)
        logical :: b(300)
        character(len=:), allocatable :: name

        call py_buffer("# -*- coding: utf-8 -*-"//LF &
                       //"#!/usr/bin/python"//LF &
                       //LF &
                       //"'''"//LF &
                       //"Module Docstring Django style."//LF &
                       //"'''"//LF &
                       //"u'''Additional module docstring.'''"//LF &
                       //"'''Not a module docstring.'''"//LF)
        v(1) = look_pos("Module Docstring Django style.", 1_int64, .false.)
        b(1) = is_python_info_docstring()
        call check(error, b(1), "(python-info-docstring-p)")
        if (allocated(error)) return
        v(2) = look_pos("u'''Additional module docstring.'''", 1_int64, .false.)
        b(2) = is_python_info_docstring()
        call check(error, b(2), "(python-info-docstring-p)")
        if (allocated(error)) return
        v(3) = look_pos("'''Not a module docstring.'''", 1_int64, .false.)
        b(3) = is_python_info_docstring()
        call check(error,.not. (b(3)), "(not (python-info-docstring-p))")
        if (allocated(error)) return
        if (.false.) shortage = v(1) + merge(1, 0, b(1))
        if (.false.) name = ""
    end subroutine test_is_python_info_docstring_1

    subroutine test_is_python_info_docstring_2(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage, v(300)
        logical :: b(300)
        character(len=:), allocatable :: name

        call py_buffer(LF &
                       //"variable = 42"//LF &
                       //"U'''Variable docstring.'''"//LF &
                       //"'''Additional variable docstring.'''"//LF &
                       //"'''Not a variable docstring.'''"//LF)
        v(1) = look_pos("Variable docstring.", 1_int64, .false.)
        b(1) = is_python_info_docstring()
        call check(error, b(1), "(python-info-docstring-p)")
        if (allocated(error)) return
        v(2) = look_pos("u'''Additional variable docstring.'''", 1_int64, .false.)
        b(2) = is_python_info_docstring()
        call check(error, b(2), "(python-info-docstring-p)")
        if (allocated(error)) return
        v(3) = look_pos("'''Not a variable docstring.'''", 1_int64, .false.)
        b(3) = is_python_info_docstring()
        call check(error,.not. (b(3)), "(not (python-info-docstring-p))")
        if (allocated(error)) return
        if (.false.) shortage = v(1) + merge(1, 0, b(1))
        if (.false.) name = ""
    end subroutine test_is_python_info_docstring_2

    subroutine test_is_python_info_docstring_3(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage, v(300)
        logical :: b(300)
        character(len=:), allocatable :: name

        call py_buffer(LF &
                       //"def func(a, b):"//LF &
                       //"    r'''"//LF &
                       //"    Function docstring."//LF &
                       //LF &
                       //"    onetwo style."//LF &
                       //"    '''"//LF &
                       //"    R'''Additional function docstring.'''"//LF &
                       //"    '''Not a function docstring.'''"//LF &
                       //"    return a + b"//LF)
        v(1) = look_pos("Function docstring.", 1_int64, .false.)
        b(1) = is_python_info_docstring()
        call check(error, b(1), "(python-info-docstring-p)")
        if (allocated(error)) return
        v(2) = look_pos("R'''Additional function docstring.'''", 1_int64, .false.)
        b(2) = is_python_info_docstring()
        call check(error, b(2), "(python-info-docstring-p)")
        if (allocated(error)) return
        v(3) = look_pos("'''Not a function docstring.'''", 1_int64, .false.)
        b(3) = is_python_info_docstring()
        call check(error,.not. (b(3)), "(not (python-info-docstring-p))")
        if (allocated(error)) return
        if (.false.) shortage = v(1) + merge(1, 0, b(1))
        if (.false.) name = ""
    end subroutine test_is_python_info_docstring_3

    subroutine test_is_python_info_docstring_4(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage, v(300)
        logical :: b(300)
        character(len=:), allocatable :: name

        call py_buffer(LF &
                       //"class Class:"//LF &
                       //"    ur'''"//LF &
                       //"    Class docstring."//LF &
                       //LF &
                       //"    symmetric style."//LF &
                       //"    '''"//LF &
                       //"    uR'''"//LF &
                       //"    Additional class docstring."//LF &
                       //"    '''"//LF &
                       //"    '''Not a class docstring.'''"//LF &
                       //"    pass"//LF)
        v(1) = look_pos("Class docstring.", 1_int64, .false.)
        b(1) = is_python_info_docstring()
        call check(error, b(1), "(python-info-docstring-p)")
        if (allocated(error)) return
        v(2) = look_pos("uR'''", 1_int64, .false.)
        b(2) = is_python_info_docstring()
        call check(error, b(2), "(python-info-docstring-p)")
        if (allocated(error)) return
        v(3) = look_pos("'''Not a class docstring.'''", 1_int64, .false.)
        b(3) = is_python_info_docstring()
        call check(error,.not. (b(3)), "(not (python-info-docstring-p))")
        if (allocated(error)) return
        if (.false.) shortage = v(1) + merge(1, 0, b(1))
        if (.false.) name = ""
    end subroutine test_is_python_info_docstring_4

    subroutine test_is_python_info_docstring_5(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage, v(300)
        logical :: b(300)
        character(len=:), allocatable :: name

        call py_buffer(LF &
                       //"class Class:"//LF &
                       //"    attribute = 42"//LF &
                       //"    Ur'''"//LF &
                       //"    Class attribute docstring."//LF &
                       //LF &
                       //"    pep-257 style."//LF &
                       //LF &
                       //"    '''"//LF &
                       //"    UR'''"//LF &
                       //"    Additional class attribute docstring."//LF &
                       //"    '''"//LF &
                       //"    '''Not a class attribute docstring.'''"//LF &
                       //"    pass"//LF)
        v(1) = look_pos("Class attribute docstring.", 1_int64, .false.)
        b(1) = is_python_info_docstring()
        call check(error, b(1), "(python-info-docstring-p)")
        if (allocated(error)) return
        v(2) = look_pos("UR'''", 1_int64, .false.)
        b(2) = is_python_info_docstring()
        call check(error, b(2), "(python-info-docstring-p)")
        if (allocated(error)) return
        v(3) = look_pos("'''Not a class attribute docstring.'''", 1_int64, .false.)
        b(3) = is_python_info_docstring()
        call check(error,.not. (b(3)), "(not (python-info-docstring-p))")
        if (allocated(error)) return
        if (.false.) shortage = v(1) + merge(1, 0, b(1))
        if (.false.) name = ""
    end subroutine test_is_python_info_docstring_5

    subroutine test_is_python_info_docstring_6(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage, v(300)
        logical :: b(300)
        character(len=:), allocatable :: name

        call py_buffer(LF &
                       //"class Class:"//LF &
                       //LF &
                       //"    def __init__(self, a, b):"//LF &
                       //"        self.a = a"//LF &
                       //"        self.b = b"//LF &
                       //LF &
                       //"    def __call__(self):"//LF &
                       //"        '''Method docstring."//LF &
                       //LF &
                       //"        pep-257-nn style."//LF &
                       //"        '''"//LF &
                       //"        '''Additional method docstring.'''"//LF &
                       //"        '''Not a method docstring.'''"//LF &
                       //"        return self.a + self.b"//LF)
        v(1) = look_pos("Method docstring.", 1_int64, .false.)
        b(1) = is_python_info_docstring()
        call check(error, b(1), "(python-info-docstring-p)")
        if (allocated(error)) return
        v(2) = look_pos("'''Additional method docstring.'''", 1_int64, .false.)
        b(2) = is_python_info_docstring()
        call check(error, b(2), "(python-info-docstring-p)")
        if (allocated(error)) return
        v(3) = look_pos("'''Not a method docstring.'''", 1_int64, .false.)
        b(3) = is_python_info_docstring()
        call check(error,.not. (b(3)), "(not (python-info-docstring-p))")
        if (allocated(error)) return
        if (.false.) shortage = v(1) + merge(1, 0, b(1))
        if (.false.) name = ""
    end subroutine test_is_python_info_docstring_6

    subroutine test_is_python_info_docstring_7(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage, v(300)
        logical :: b(300)
        character(len=:), allocatable :: name

        call py_buffer(LF &
                       //"{'Not a docstring': 1}"//LF &
                       //"'Also not a docstring'"//LF)
        v(1) = look_pos("Not a docstring", 1_int64, .false.)
        b(1) = is_python_info_docstring()
        call check(error,.not. (b(1)), "(not (python-info-docstring-p))")
        if (allocated(error)) return
        v(2) = look_pos("Also not a docstring", 1_int64, .false.)
        b(2) = is_python_info_docstring()
        call check(error,.not. (b(2)), "(not (python-info-docstring-p))")
        if (allocated(error)) return
        if (.false.) shortage = v(1) + merge(1, 0, b(1))
        if (.false.) name = ""
    end subroutine test_is_python_info_docstring_7

    subroutine test_is_python_info_docstring_8(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage, v(300)
        logical :: b(300)
        character(len=:), allocatable :: name

        call py_buffer("import sys"//LF &
                       //"'''Not a docstring.'''"//LF)
        v(1) = look_pos("Not a docstring", 1_int64, .false.)
        b(1) = is_python_info_docstring()
        call check(error,.not. (b(1)), "(not (python-info-docstring-p))")
        if (allocated(error)) return
        if (.false.) shortage = v(1) + merge(1, 0, b(1))
        if (.false.) name = ""
    end subroutine test_is_python_info_docstring_8

    subroutine test_python_info_encoding_from_cookie_3(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: shortage, v(300)
        logical :: b(300)
        character(len=:), allocatable :: name

        call py_buffer(LF &
                       //LF &
                       //"# coding=latin-1"//LF &
                       //"foo = True  # another comment"//LF)
        call python_info_encoding_from_cookie(name)
        call check(error,.not. (allocated(name)), "(not (python-info-encoding-from-cookie))")
        if (allocated(error)) return
        if (.false.) shortage = v(1) + merge(1, 0, b(1))
        if (.false.) name = ""
    end subroutine test_python_info_encoding_from_cookie_3
end module test_python_fm
