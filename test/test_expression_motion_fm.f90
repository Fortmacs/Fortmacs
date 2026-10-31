! test_expression_motion_fm.f90 -- tests for the subprogram and pair commands of
! expression_motion_fm.
! Ports GNU Emacs 31.1 test/lisp/emacs-lisp/lisp-tests.el in part; the
! subprogram and pair tests of the ported commands.
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

!> Ports test/lisp/emacs-lisp/lisp-tests.el's end-of-defun-twice,
!> mark-defun-no-arg-region-inactive and the lisp-delete-pair-* tests
!> (with a syntax table giving `;' comments in place of Emacs Lisp mode),
!> plus own tests of `beginning_of_subprogram' with `subprogram_prompt_regexp',
!> `narrow_to_subprogram', `insert_parentheses', `raise_expression' and
!> `check_parens'.
module test_expression_motion_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use lisp_fm, only: value_type
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_fm, only: status_type, FM_USER_ERROR
    use handle_fm, only: handle_type
    use buffer_fm, only: buffers, cur, get_buffer_create, set_buffer, erase_buffer, transient_mark_mode
    use insdel_fm, only: insert
    use editfns_fm, only: point, goto_char, point_min, point_max, buffer_string, mark_position, widen
    use keyboard_fm, only: last_command
    use bindings_fm, only: init_bindings
    use syntax_fm, only: modify_syntax_entry, make_syntax_table, copy_syntax_table, set_syntax_table, free_syntax_table, &
                         standard_syntax_table, syntax_table
    use text_mode_fm, only: text_mode
    use simple_fm, only: deactivate_mark
    use expression_motion_fm, only: beginning_of_subprogram, end_of_subprogram, mark_subprogram, narrow_to_subprogram, &
                                    insert_parentheses, &
                                    delete_pair, raise_expression, check_parens, subprogram_prompt_regexp
    implicit none(type, external)
    private

    public :: collect_expression_motion_fm

    character(len=*), parameter :: LF = achar(10)

    !> Positions named by =!NAME= in a test buffer.
    type :: position_type
        character(len=16) :: name = ""
        integer(int64) :: pos = 0
    end type position_type

    type(position_type) :: positions(32)
    integer :: npositions = 0

contains

    !> Collect all exported unit tests.
    subroutine collect_expression_motion_fm(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("end-of-defun-twice", test_end_of_subprogram_twice), &
                    new_unittest("mark-defun-no-arg-region-inactive", test_mark_subprogram), &
                    new_unittest("lisp-delete-pair-parens", test_delete_pair_parens), &
                    new_unittest("lisp-delete-pair-quotation-marks", test_delete_pair_quotes), &
                    new_unittest("lisp-delete-pair-quotes-in-text-mode", test_delete_pair_text_mode), &
                    new_unittest("lisp-delete-pair-quotes-text-mode-syntax-table", test_delete_pair_text_syntax), &
                    new_unittest("beginning-of-defun-prompt-regexp", test_subprogram_prompt), &
                    new_unittest("narrow-to-defun", test_narrow_to_subprogram), &
                    new_unittest("insert-parentheses", test_insert_parentheses), &
                    new_unittest("raise-sexp", test_raise_expression), &
                    new_unittest("check-parens", test_check_parens) &
                    ]
    end subroutine collect_expression_motion_fm

    !> A fresh buffer holding TEXT, point at its start.
    subroutine temp_buffer(text)
        character(len=*), intent(in) :: text
        type(handle_type) :: buf
        type(status_type) :: st

        call init_bindings()
        call get_buffer_create(" *lisp-cmds-test*", buf, st)
        call set_buffer(buf, st)
        call widen()
        call set_syntax_table(standard_syntax_table(), st)
        buffers(cur)%mark_active = .false.
        call erase_buffer(st)
        call insert(text, st)
        call goto_char(point_min())
        last_command = value_type()
    end subroutine temp_buffer

    !> elisp-tests-with-temp-buffer: TEXT without its =!NAME= words, whose
    !> positions `at' gives, in a buffer where `;' starts comments (TABLE,
    !> to free after).
    subroutine elisp_buffer(text, table)
        character(len=*), intent(in) :: text
        type(handle_type), intent(out) :: table
        character(len=:), allocatable :: rest, plain
        integer :: k, e
        type(status_type) :: st

        npositions = 0
        rest = text
        plain = ""

        do
            k = index(rest, "=!")

            if (k == 0) then
                plain = plain//rest
                exit
            end if

            e = index(rest(k + 2:), "=")
            plain = plain//rest(1:k - 1)
            npositions = npositions + 1
            positions(npositions)%name = rest(k + 2:k + e)
            positions(npositions)%pos = len(plain) + 1
            rest = rest(k + e + 2:)
        end do

        call temp_buffer(plain)
        call make_syntax_table(table, st)
        call modify_syntax_entry(iachar(";"), iachar(";"), "<", st, table)
        call modify_syntax_entry(10, 10, ">", st, table)
        call modify_syntax_entry(iachar('"'), iachar('"'), """", st, table)
        call set_syntax_table(table, st)
    end subroutine elisp_buffer

    integer(int64) function at(name)
        character(len=*), intent(in) :: name
        integer :: k

        at = 0

        do k = 1, npositions
            if (positions(k)%name == name) at = positions(k)%pos
        end do
    end function at

    subroutine done(table)
        type(handle_type), intent(in) :: table
        type(status_type) :: st

        call set_syntax_table(standard_syntax_table(), st)
        call free_syntax_table(table, st)
    end subroutine done

    !> Test behavior of prefix arg for `end_of_subprogram' (Bug#24427).
    !> Calling `end_of_subprogram' twice should be the same as a prefix arg
    !> of two.
    subroutine test_end_of_subprogram_twice(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: table

        call eod2("(defun a ())"//LF//LF//"(defun b ())"//LF//LF//"(defun c ())")
        if (allocated(error)) return
        call eod2("(defun a ())"//LF//"(defun b ())"//LF//"(defun c ())")
        if (allocated(error)) return
        call elisp_buffer(";; Comment header"//LF//LF//"(defun func-1 (arg)"//LF//"  ""docstring"""//LF//"  body)"//LF// &
                          "=!p1="//LF//";; Comment before a defun"//LF//"(defun func-2 (arg)"//LF//"  ""docstring"""//LF// &
                          "  body)"//LF//LF//"(defun func-3 (arg)"//LF//"  ""docstring"""//LF//"  body)"//LF// &
                          "=!p2=(defun func-4 (arg)"//LF//"  ""docstring"""//LF//"  body)"//LF//LF//";; end"//LF, table)

        checks: block
            type(status_type) :: st
            call goto_char(at("p1"))
            call end_of_subprogram(2_int64, st)
            call check(error, point() == at("p2"), "end-of-defun 2 from p1")
        end block checks

        call done(table)

    contains

        subroutine eod2(text)
            character(len=*), intent(in) :: text
            integer(int64) :: pt_eod2
            type(status_type) :: st

            call temp_buffer(text)
            call end_of_subprogram(1_int64, st)
            call end_of_subprogram(1_int64, st)
            pt_eod2 = point()
            call goto_char(point_min())
            call end_of_subprogram(2_int64, st)
            call check(error, point() == pt_eod2, "end-of-defun twice /= end-of-defun 2")
        end subroutine eod2
    end subroutine test_end_of_subprogram_twice

    !> Test `mark_subprogram' with no prefix argument and inactive region.
    subroutine test_mark_subprogram(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: table
        character(len=*), parameter :: TEXT = ";; Comment header"//LF//"=!before-1="//LF//"(defun func-1 (arg)"//LF// &
                                       "  =!inside-1=""docstring"""//LF//"  body)"//LF//"=!after-1==!before-2="//LF// &
                                       ";; Comment before a defun"//LF//"(d=!inside-2=efun func-2 (arg)"//LF// &
                                       "  ""docstring"""//LF//"  body)"//LF//"=!after-2==!before-3="//LF// &
                                       "(defun func-3 (arg)"//LF//"  ""docstring""=!inside-3="//LF//"  body)"//LF// &
                                       "=!after-3==!before-4=(defun func-4 (arg)"//LF//"  ""docstring""=!inside-4="//LF// &
                                       "  body)"//LF//"=!after-4="//LF//";; end"//LF

        call elisp_buffer(TEXT, table)

        checks: block
            ! mark-defun inside a subprogram, with comments and an empty line
            ! before
            call try("inside-1", "before-1", "after-1")
            if (allocated(error)) exit checks
            ! mark-defun inside a subprogram with comments before
            call try("inside-2", "before-2", "after-2")
            if (allocated(error)) exit checks
            ! mark-defun inside a subprogram with empty line before
            call try("inside-3", "before-3", "after-3")
            if (allocated(error)) exit checks
            ! mark-defun inside a subprogram with another one right before
            call try("inside-4", "before-4", "after-4")
            if (allocated(error)) exit checks
            ! mark-defun between a comment and a subprogram
            call try("before-1", "before-1", "after-1")
            if (allocated(error)) exit checks
            ! mark-defun between subprograms
            call try("before-3", "before-3", "after-3")
            if (allocated(error)) exit checks
            ! mark-defun in comment right before the subprogram
            call try("before-2", "before-2", "after-2")
        end block checks

        call done(table)

    contains

        subroutine try(from, before, after)
            character(len=*), intent(in) :: from, before, after
            type(status_type) :: st

            call deactivate_mark()
            last_command = value_type()
            call goto_char(at(from))
            call mark_subprogram(1_int64, st)
            call check(error, point() == at(before) .and. mark_position() == at(after), "mark-defun from "//from)
        end subroutine try
    end subroutine test_mark_subprogram

    !> Test \[delete_pair] with parens.
    subroutine test_delete_pair_parens(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st

        call temp_buffer("(foo)")
        call delete_pair(1_int64, st)
        call check(error, buffer_string(), "foo")
    end subroutine test_delete_pair_parens

    !> Test \[delete_pair] with quotation marks.
    subroutine test_delete_pair_quotes(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st

        call temp_buffer("""foo""")
        call delete_pair(1_int64, st)
        call check(error, buffer_string(), "foo")
    end subroutine test_delete_pair_quotes

    !> Test \[delete_pair] against string in Text Mode for #15014.
    subroutine test_delete_pair_text_mode(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st

        call temp_buffer("")
        call text_mode(st)
        call insert("""foo""", st)
        call goto_char(point_min())
        call delete_pair(1_int64, st)
        call check(error,.not. st%ok(), "delete-pair should fail")
    end subroutine test_delete_pair_text_mode

    !> Test \[delete_pair] with modified Text Mode syntax for #15014.
    subroutine test_delete_pair_text_syntax(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: table, old
        type(status_type) :: st

        call temp_buffer("")
        call text_mode(st)
        old = syntax_table()
        call copy_syntax_table(table, st, old)
        call modify_syntax_entry(iachar('"'), iachar('"'), "$", st, table)
        call set_syntax_table(table, st)
        call insert("""foo""", st)
        call goto_char(point_min())
        call delete_pair(1_int64, st)
        call check(error, buffer_string(), "foo")
        call set_syntax_table(old, st)
        call free_syntax_table(table, st)
    end subroutine test_delete_pair_text_syntax

    subroutine test_subprogram_prompt(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        logical :: found

        call temp_buffer("f () {"//LF//"  a"//LF//"}"//LF//"function g {"//LF//"  b"//LF//"}"//LF)
        call subprogram_prompt_regexp%set_local("^\(function[ "//achar(9)//"]+\)?[a-z]+[ ]*\(()\)?[ ]*")
        call goto_char(point_max())

        checks: block
            call beginning_of_subprogram(1_int64, found, st)
            call check(error, found .and. point() == 14, "beginning of g")
            if (allocated(error)) exit checks
            call beginning_of_subprogram(1_int64, found, st)
            call check(error, found .and. point() == 1, "beginning of f")
            if (allocated(error)) exit checks
            call goto_char(point_min())
            call end_of_subprogram(1_int64, st)
            call check(error, point() == 14, "end of f")
        end block checks

        call subprogram_prompt_regexp%kill_local()
    end subroutine test_subprogram_prompt

    subroutine test_narrow_to_subprogram(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st

        call temp_buffer("(a 1)"//LF//LF//"(b 2"//LF//" 3)"//LF//LF//"(c 4)"//LF)
        call goto_char(10_int64)
        call narrow_to_subprogram(.false., st)
        call check(error, buffer_string(), "(b 2"//LF//" 3)"//LF)
        call widen()
    end subroutine test_narrow_to_subprogram

    subroutine test_insert_parentheses(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st

        call temp_buffer("foo bar")

        checks: block
            call insert_parentheses(st)
            call check(error, buffer_string(), "() foo bar")
            if (allocated(error)) exit checks
            call check(error, point() == 2, "point between the parens")
            if (allocated(error)) exit checks
            call temp_buffer("a foo bar")
            call goto_char(3_int64)
            call insert_parentheses(st, 2_int64)
            call check(error, buffer_string(), "a (foo bar)")
            if (allocated(error)) exit checks
            call check(error, point() == 4, "point after the open paren")
        end block checks
    end subroutine test_insert_parentheses

    subroutine test_raise_expression(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st

        call temp_buffer("(let ((foo 2))"//LF//"  (progn"//LF//"    (setq foo 3)"//LF//"    (zot)"//LF//"    (+ foo 2)))")
        call goto_char(index(buffer_string(), "(zot)", kind=int64))
        call raise_expression(1_int64, st)
        call check(error, buffer_string(), "(let ((foo 2))"//LF//"  (zot))")
    end subroutine test_raise_expression

    subroutine test_check_parens(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st

        call temp_buffer("(a (b) c")

        checks: block
            call check_parens(st)
            call check(error, st%code == FM_USER_ERROR .and. st%detail == "Unmatched bracket or quote", "error")
            if (allocated(error)) exit checks
            call check(error, point() == 1, "point at the unbalanced paren")
            if (allocated(error)) exit checks
            call temp_buffer("(a (b) c)")
            call check_parens(st)
            call check(error, st%ok(), "balanced")
        end block checks
    end subroutine test_check_parens
end module test_expression_motion_fm
