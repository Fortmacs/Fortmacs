! test_replace_fm.f90 -- tests for the replace commands (replace_fm).
!
! Copyright (C) 2010-2026 Free Software Foundation, Inc.
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

!> Ports test/lisp/replace-tests.el: query-replace-tests (through the
!> command loop), perform-replace-tests, the query-replace undo tests
!> (keys fed to `read-key`, Lisp replacement expressions as Fortran
!> functions) and replace-regexp-bug45973.  Not ported: the occur tests
!> (no occur yet), query-replace--split-string-tests (the separator is
!> not a text property here), test-count-matches (no how-many yet), and
!> the `\,` case of query-replace-tests (no Lisp).
module test_replace_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_fm, only: status_type
    use handle_fm, only: handle_type
    use lisp_fm, only: value_type, nil
    use lread_fm, only: sym
    use data_fm, only: set
    use buffer_fm, only: get_buffer_create, set_buffer, kill_buffer
    use insdel_fm, only: insert
    use editfns_fm, only: goto_char, buffer_string
    use keymap_fm, only: key_parse
    use keyboard_fm, only: unread_command_events, discard_input, last_message, command_loop
    use callint_fm, only: prefix_arg
    use bindings_fm, only: init_bindings
    use subr_fm, only: match_string
    use replace_fm, only: perform_replace, replace_regexp, replace_quote, query_replace_defaults, &
                          replace_expand_count, query_replace_compile_replacement
    implicit none(type, external)
    private

    public :: collect_replace_fm

contains

    subroutine collect_replace_fm(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)
        testsuite = [ &
                    new_unittest("query-replace-tests", test_query_replace), &
                    new_unittest("perform-replace-tests", test_perform_replace), &
                    new_unittest("query-replace--undo", test_undo), &
                    new_unittest("query-replace-undo-bug31073", test_undo_31073), &
                    new_unittest("query-replace-undo-bug31492", test_undo_31492), &
                    new_unittest("query-replace-undo-bug31538", test_undo_31538), &
                    new_unittest("query-replace-undo-bug37073", test_undo_37073), &
                    new_unittest("query-replace-undo-bug37287", test_undo_37287), &
                    new_unittest("replace-regexp-bug45973", test_bug45973), &
                    new_unittest("replace-compile-replacement", test_compile) &
                    ]
    end subroutine collect_replace_fm

    subroutine enter(text, h)
        character(len=*), intent(in) :: text
        type(handle_type), intent(out) :: h
        type(status_type) :: st
        call init_bindings()
        call discard_input()
        prefix_arg = nil()
        call get_buffer_create("replace-test", h, st)
        call set_buffer(h, st)
        call insert(text, st)
        call goto_char(1_int64)
        last_message = ""
        if (allocated(query_replace_defaults)) deallocate (query_replace_defaults)
        call set(sym("query_replace_history"), nil(), st)
        call set(sym("search_ring"), nil(), st)
        call set(sym("regexp_search_ring"), nil(), st)
    end subroutine enter

    subroutine leave(h)
        type(handle_type), intent(in) :: h
        type(status_type) :: st
        call discard_input()
        call kill_buffer(h, st)
    end subroutine leave

    !> Type `keys` (kbd syntax) into the command loop.
    subroutine type_keys(keys)
        character(len=*), intent(in) :: keys
        type(value_type), allocatable :: evs(:)
        type(status_type) :: st
        call key_parse(keys, evs, st)
        call unread_command_events(evs)
        call command_loop(st)
    end subroutine type_keys

    !> Queue `keys` (kbd syntax) for `read-key`.
    subroutine queue_keys(keys)
        character(len=*), intent(in) :: keys
        type(value_type), allocatable :: evs(:)
        type(status_type) :: st
        call key_parse(keys, evs, st)
        call unread_command_events(evs)
    end subroutine queue_keys

    !> query-replace--run-tests: INPUT, KEYS, EXPECTED.
    subroutine run_case(error, input, keys, expected)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), intent(in) :: input, keys, expected
        type(handle_type) :: h
        call enter(input, h)
        call type_keys(keys)
        call check(error, buffer_string() == expected, input//" / "//keys//": got "//buffer_string())
        call leave(h)
    end subroutine run_case

    subroutine test_query_replace(error)
        type(error_type), allocatable, intent(out) :: error
        ! query-replace
        call run_case(error, "aaa", "M-% a RET 1 RET !", "111")
        if (allocated(error)) return
        call run_case(error, "aaa", "M-% a RET 1 RET y n y", "1a1")
        if (allocated(error)) return
        ! Empty inputs
        call run_case(error, "aaa", "M-% a RET RET !", "")
        if (allocated(error)) return
        call run_case(error, "aaa", "M-% RET 1 RET !", "1a1a1a")
        if (allocated(error)) return
        call run_case(error, "aaa", "M-% RET RET !", "aaa")
        if (allocated(error)) return
        ! Reuse the previous default
        call run_case(error, "aaa", "M-% a RET 1 RET . M-% RET !", "111")
        if (allocated(error)) return
        ! query-replace-regexp
        call run_case(error, "aaa", "C-M-% a* RET 1 RET !", "1")
        if (allocated(error)) return
        ! Empty inputs
        call run_case(error, "aaa", "C-M-% a* RET RET !", "")
        if (allocated(error)) return
        call run_case(error, "aaa", "C-M-% RET 1 RET !", "1a1a1a")
        if (allocated(error)) return
        call run_case(error, "aaa", "C-M-% RET RET !", "aaa")
        if (allocated(error)) return
        ! Empty matches
        call run_case(error, "aaa", "C-M-% b* RET 1 RET !", "1a1a1a")
        if (allocated(error)) return
        ! Complete matches
        call run_case(error, "aaa", "C-M-% .* RET 1 RET !", "1")
        if (allocated(error)) return
        ! Adjacent non-empty matches
        call run_case(error, "abaab", "C-M-% ab* RET 12 RET !", "121212")
        if (allocated(error)) return
        ! Adjacent non-empty and empty matches
        call run_case(error, "abab", "C-M-% a* RET 1 RET !", "1b1b")
        if (allocated(error)) return
        call run_case(error, "abab", "C-M-% b* RET 1 RET !", "1a1a1")
        if (allocated(error)) return
        ! Test case from commit 5632eb272c7
        call run_case(error, "a a a ", "C-M-% \ba SPC RET c RET !", "ccc")
        if (allocated(error)) return
        ! Backup ('^') with calculated replacement (bug#79811)
        call run_case(error, "abc", "C-M-% \([abc]\) RET [\1,\#] RET y n ^ y y", "[a,0][b,1][c,2]")
        if (allocated(error)) return
        ! Fortmacs: the word (delimited) and backward prefixes
        call run_case(error, "a aa a", "C-u M-% a RET b RET !", "b aa b")
        if (allocated(error)) return
        call run_case(error, "aaa", "C-e M-- M-% a RET b RET y y", "abb")
        if (allocated(error)) return
        ! Fortmacs: act-and-show (,) then act, and the region
        call run_case(error, "aaa", "M-% a RET b RET , y y q", "bba")
        if (allocated(error)) return
        call run_case(error, "aaaa", "C-f C-SPC C-f C-f M-% a RET b RET !", "abba")
    end subroutine test_query_replace

    subroutine test_perform_replace(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        ! Test case from commit 5632eb272c7
        call enter("a a a ", h)
        call perform_replace("\ba ", "c", .false., .true., .false., st)
        call check(error, buffer_string() == "ccc", "not ""ca c"": "//buffer_string())
        call leave(h)
    end subroutine test_perform_replace

    !> replace-tests-with-undo: replace FROM with TO in INPUT, answering
    !> KEYS, and compare the result with EXPECTED.
    subroutine with_undo(error, input, from, to, keys, expected, regexp_flag, fn)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), intent(in) :: input, from, to, keys, expected
        logical, intent(in), optional :: regexp_flag
        procedure(theorem_replacement), optional :: fn
        type(handle_type) :: h
        type(status_type) :: st
        logical :: re
        re = .true.
        if (present(regexp_flag)) re = regexp_flag
        call enter(input, h)
        call queue_keys(keys)
        call perform_replace(from, to, .true., re, .false., st, replacement_fn=fn)
        call check(error, st%ok() .and. buffer_string() == expected, keys//": got "//buffer_string())
        call leave(h)
    end subroutine with_undo

    subroutine test_undo(error)
        type(error_type), allocatable, intent(out) :: error
        call with_undo(error, "111", "1", "2", "SPC SPC u q", "211")
        if (allocated(error)) return
        call with_undo(error, "111", "1", "2", "SPC , u q", "211")
    end subroutine test_undo

    subroutine test_undo_31073(error)
        type(error_type), allocatable, intent(out) :: error
        call with_undo(error, "aaa aaa", "a", "B", "SPC SPC SPC U q", "aaa aaa")
    end subroutine test_undo_31073

    subroutine test_undo_31492(error)
        type(error_type), allocatable, intent(out) :: error
        call with_undo(error, "a"//achar(10)//"b"//achar(10)//"c"//achar(10), "^\|\b\|$", "foo", &
                       "SPC SPC U q", "a"//achar(10)//"b"//achar(10)//"c"//achar(10))
    end subroutine test_undo_31492

    subroutine test_undo_31538(error)
        type(error_type), allocatable, intent(out) :: error
        ! read-string answers "Bfoo": the minibuffer starts with "B".
        call with_undo(error, "aaa aaa", "a", "B", "SPC SPC SPC E f o o RET U q", "aaa aaa")
    end subroutine test_undo_31538

    !> (replace-quote (format "theorem \\\\ref{theo_%d}"
    !>                        (1+ (string-to-number (match-string 1)))))
    function theorem_replacement(count) result(text)
        integer(int64), intent(in) :: count
        character(len=:), allocatable :: text
        integer :: n
        character(len=:), allocatable :: digits
        digits = match_string(1)
        read (digits, *) n
        text = replace_quote("theorem \\ref{theo_"//itoa(n + 1)//"}")
        if (count < 0) text = ""
    end function theorem_replacement

    !> (replace-quote (format "%d" (1+ (string-to-number (match-string 1)))))
    function increment_replacement(count) result(text)
        integer(int64), intent(in) :: count
        character(len=:), allocatable :: text
        integer :: n
        character(len=:), allocatable :: digits
        digits = match_string(1)
        read (digits, *) n
        text = replace_quote(itoa(n + 1))
        if (count < 0) text = ""
    end function increment_replacement

    !> (replace-quote (if (match-string 2) "R" "L"))
    function swap_replacement(count) result(text)
        integer(int64), intent(in) :: count
        character(len=:), allocatable :: text
        logical :: matched
        character(len=:), allocatable :: ignored
        ignored = match_string(2, matched=matched)
        text = merge("R", "L", matched)
        if (count < 0) text = ""
    end function swap_replacement

    function itoa(n) result(s)
        integer, intent(in) :: n
        character(len=:), allocatable :: s
        character(len=12) :: buf
        write (buf, "(i0)") n
        s = trim(buf)
    end function itoa

    subroutine test_undo_37073(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: input = "theorem 1"//achar(10)//"theorem 2"//achar(10)//"theorem 3"
        call with_undo(error, input, "theorem \([0-9]+\)", "", "SPC SPC U q", input, fn=theorem_replacement)
        if (allocated(error)) return
        ! Now run a test with regexp-flag arg in `perform_replace' set to nil
        call with_undo(error, " ^theorem$ 1"//achar(10)//" ^theorem$ 2"//achar(10)//" ^theorem$ 3", &
                       "^theorem$", "theo", "SPC SPC U SPC q", &
                       " theo 1"//achar(10)//" ^theorem$ 2"//achar(10)//" ^theorem$ 3", regexp_flag=.false.)
    end subroutine test_undo_37073

    subroutine test_undo_37287(error)
        type(error_type), allocatable, intent(out) :: error
        call with_undo(error, "foo-1"//achar(10)//"foo-2"//achar(10)//"foo-3", "\([0-9]\)", "", &
                       "SPC SPC U SPC q", "foo-2"//achar(10)//"foo-2"//achar(10)//"foo-3", fn=increment_replacement)
    end subroutine test_undo_37287

    subroutine test_bug45973(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        call enter("1RB 1LC 1RC 1RB 1RD 0LE 1LA 1LD 1RH 0LA", h)
        call replace_regexp("\(\(L\)\|\(R\)\)", "", st, replacement_fn=swap_replacement)
        call check(error, buffer_string() == "1LB 1RC 1LC 1LB 1LD 0RE 1RA 1RD 1LH 0RA", buffer_string())
        call leave(h)
    end subroutine test_bug45973

    !> Fortmacs: `\#` expands to the count; `\,` is an error.
    subroutine test_compile(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        logical :: counted
        call query_replace_compile_replacement("x\#y\\#", .true., counted, st)
        call check(error, st%ok() .and. counted)
        if (allocated(error)) return
        call check(error, replace_expand_count("x\#y\\#", 7_int64) == "x7y\\#")
        if (allocated(error)) return
        call query_replace_compile_replacement("\,(foo)", .true., counted, st)
        call check(error,.not. st%ok())
        if (allocated(error)) return
        call query_replace_compile_replacement("\#", .false., counted, st)
        call check(error, st%ok() .and. .not. counted)
    end subroutine test_compile
end module test_replace_fm
