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
!> functions), replace-regexp-bug45973, the occur tests (occur-test-00 to
!> occur-test-13, replace-occur-revert-bug32543 and -bug32987) and
!> test-count-matches; own tests of the keep-lines family and Occur
!> mode.  Not ported: query-replace--split-string-tests (the separator is
!> not a text property here) and the `\,` case of query-replace-tests
!> (no Lisp).
module test_replace_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use test_support, only: show_alone
    use error_fm, only: status_type
    use handle_fm, only: handle_type
    use lisp_fm, only: value_type, nil
    use lread_fm, only: sym
    use data_fm, only: set
    use buffer_fm, only: get_buffer_create, set_buffer, kill_buffer, erase_buffer, get_buffer, buffer_live_p, buffers, cur
    use error_fm, only: format_error
    use editfns_fm, only: point, point_min, line_end_position, buffer_substring
    use files_fm, only: revert_buffer
    use insdel_fm, only: insert
    use editfns_fm, only: goto_char, buffer_string
    use keymap_fm, only: key_parse
    use keyboard_fm, only: unread_command_events, discard_input, last_message, command_loop
    use callint_fm, only: prefix_arg
    use bindings_fm, only: init_bindings
    use subr_fm, only: match_string
    use replace_fm, only: occur_edit_mode, occur_cease_edit
    use replace_fm, only: occur, occur_next, occur_mode_goto_occurrence, how_many, keep_lines, flush_lines, &
                          kill_matching_lines, copy_matching_lines, list_matching_lines_jump_to_current_line
    use replace_fm, only: perform_replace, replace_regexp, replace_quote, query_replace_defaults, &
                          replace_expand_count, query_replace_compile_replacement, read_regexp_suggestions, &
                          query_replace_read_from_suggestions
    use minibuf_fm, only: minibuffer_string_type
    use subr_fm, only: query_replace_answer, ANSWER_SCROLL_DOWN, ANSWER_SCROLL_UP, ANSWER_SKIP, ANSWER_EXIT, &
                       ANSWER_HELP, ANSWER_SCROLL_OTHER_WINDOW, ANSWER_SCROLL_OTHER_WINDOW_DOWN, ANSWER_ACT, &
                       ANSWER_NONE
    use data_fm, only: symbol_value
    use lisp_fm, only: list_length, list_elt, xstring
    implicit none(type, external)
    private

    public :: collect_replace_fm

    character(len=*), parameter :: LF = achar(10)

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
                    new_unittest("replace-compile-replacement", test_compile), &
                    new_unittest("query-replace-undo-control-chars", test_undo_control), &
                    new_unittest("replace-edit-question-mark", test_edit_question_mark), &
                    new_unittest("query-replace-map-events", test_map_events), &
                    new_unittest("query-replace-suggestions", test_suggestions), &
                    new_unittest("map-query-replace-regexp-history", test_map_history), &
                    new_unittest("occur-test-00", test_occur_00), &
                    new_unittest("occur-test-01", test_occur_01), &
                    new_unittest("occur-test-02", test_occur_02), &
                    new_unittest("occur-test-03", test_occur_03), &
                    new_unittest("occur-test-04", test_occur_04), &
                    new_unittest("occur-test-05", test_occur_05), &
                    new_unittest("occur-test-06", test_occur_06), &
                    new_unittest("occur-test-07", test_occur_07), &
                    new_unittest("occur-test-08", test_occur_08), &
                    new_unittest("occur-test-09", test_occur_09), &
                    new_unittest("occur-test-10", test_occur_10), &
                    new_unittest("occur-test-11", test_occur_11), &
                    new_unittest("occur-test-12", test_occur_12), &
                    new_unittest("occur-test-13", test_occur_13), &
                    new_unittest("replace-occur-revert-bug32543", test_revert_32543), &
                    new_unittest("replace-occur-revert-bug32987", test_revert_32987), &
                    new_unittest("test-count-matches", test_count_matches), &
                    new_unittest("keep-lines", test_keep_lines), &
                    new_unittest("occur-mode", test_occur_mode) &
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
        call show_alone()
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

    !> The stack keeps the replacement itself (query-replace-descr only
    !> changes how it is displayed), so undo finds it again.
    subroutine test_undo_control(error)
        type(error_type), allocatable, intent(out) :: error

        call with_undo(error, "aaa", "a", "x"//achar(10), "SPC u q", "aaa")
    end subroutine test_undo_control

    !> `\?' in a replacement: edit it with point where it was.
    subroutine test_edit_question_mark(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call enter("a", h)
        call queue_keys("Z RET")
        call perform_replace("a", "x\?y", .false., .true., .false., st)
        call check(error, st%ok() .and. buffer_string() == "xZy", buffer_string())
        call leave(h)
    end subroutine test_edit_question_mark

    !> All the events of query-replace-map.
    subroutine test_map_events(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, answer_of("M-v") == ANSWER_SCROLL_DOWN .and. answer_of("<prior>") == ANSWER_SCROLL_DOWN &
                   .and. answer_of("<next>") == ANSWER_SCROLL_UP .and. answer_of("<delete>") == ANSWER_SKIP &
                   .and. answer_of("<backspace>") == ANSWER_SKIP .and. answer_of("<return>") == ANSWER_EXIT &
                   .and. answer_of("<f1>") == ANSWER_HELP .and. answer_of("<help>") == ANSWER_HELP &
                   .and. answer_of("C-M-v") == ANSWER_SCROLL_OTHER_WINDOW &
                   .and. answer_of("M-<next>") == ANSWER_SCROLL_OTHER_WINDOW &
                   .and. answer_of("C-M-S-v") == ANSWER_SCROLL_OTHER_WINDOW_DOWN &
                   .and. answer_of("M-<prior>") == ANSWER_SCROLL_OTHER_WINDOW_DOWN &
                   .and. answer_of("SPC") == ANSWER_ACT .and. answer_of("ESC") == ANSWER_NONE, "answers")

    contains

        integer function answer_of(keys)
            character(len=*), intent(in) :: keys
            type(value_type), allocatable :: evs(:)
            type(status_type) :: st

            call key_parse(keys, evs, st)
            answer_of = query_replace_answer(evs(1))
        end function answer_of
    end subroutine test_map_events

    !> M-n offers the symbol at point (find-tag-default) for FROM; the
    !> regexp suggestions have it quoted and as a symbol regexp.
    subroutine test_suggestions(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(minibuffer_string_type), allocatable :: list(:)
        integer :: n

        call enter("foo_bar foo", h)
        call goto_char(2_int64)
        call query_replace_read_from_suggestions(list, n)
        call check(error, n >= 1, "a suggestion")
        if (allocated(error)) return
        call check(error, list(1)%s == "foo_bar", list(1)%s)
        if (allocated(error)) return
        call read_regexp_suggestions(list, n)
        call check(error, n >= 2, "regexp suggestions")
        if (allocated(error)) return
        call check(error, list(1)%s == "foo_bar" .and. list(2)%s == "\_<foo_bar\_>", list(1)%s//" "//list(2)%s)
        if (allocated(error)) return
        call goto_char(1_int64)
        call type_keys("M-% M-n RET x RET !")
        call check(error, buffer_string() == "x foo", buffer_string())
        call leave(h)
    end subroutine test_suggestions

    !> map-query-replace-regexp records FROM and TO in their histories.
    subroutine test_map_history(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        type(value_type) :: v

        call enter("aa", h)
        call set(sym("query_replace_history"), nil(), st)
        call type_keys("M-x map_query_replace_regexp RET a RET x SPC y RET !")
        call check(error, buffer_string() == "xy", buffer_string())
        if (allocated(error)) return
        call symbol_value(sym("query_replace_history"), v, st)
        call check(error, list_length(v) == 2, "from and to in query_replace_history")
        if (allocated(error)) return
        call check(error, xstring(list_elt(v, 1_int64)) == "x y" .and. xstring(list_elt(v, 2_int64)) == "a", &
                   xstring(list_elt(v, 1_int64)))
        call leave(h)
    end subroutine test_map_history

    ! ------------------------------------------------------------------
    ! Occur
    ! ------------------------------------------------------------------
    !> Port of replace-tests.el replace-occur-test-case: occur REGEXP with
    !> NLINES in " *test-occur*" holding INPUT; the text of *Occur* must be
    !> OUTPUT.
    subroutine occur_case(error, regexp, nlines, input, output)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), intent(in) :: regexp, input, output
        integer(int64), intent(in) :: nlines
        type(handle_type) :: h, occ
        type(status_type) :: st
        character(len=:), allocatable :: text

        call init_bindings()
        call get_buffer_create(" *test-occur*", h, st)
        call set_buffer(h, st)
        call erase_buffer(st)
        call insert(input, st)
        call occur(regexp, st, nlines)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        occ = get_buffer("*Occur*")
        call check(error, buffer_live_p(occ), "no *Occur*")
        if (allocated(error)) return
        call set_buffer(occ, st)
        text = buffer_string()
        call check(error, text, output)
        call kill_buffer(occ, st)
        call kill_buffer(h, st)
    end subroutine occur_case

    !> The ;; text of replace-occur-revert-bug32543 and bug32987.
    function scratch_text() result(text)
        character(len=:), allocatable :: text

        text = ";; This buffer is for text that is not saved, and for Lisp evaluation."//LF// &
               ";; To create a file, visit it with C-x C-f and enter text in its buffer."//LF//LF
    end function scratch_text

    !> Occur "and" in the scratch text, revert *Occur*: the header line
    !> must still count 2 matches.
    subroutine revert_case(error, jump)
        type(error_type), allocatable, intent(out) :: error
        logical, intent(in) :: jump
        type(handle_type) :: h, occ
        type(status_type) :: st
        character(len=:), allocatable :: text
        logical :: saved

        call init_bindings()
        saved = list_matching_lines_jump_to_current_line
        list_matching_lines_jump_to_current_line = jump
        call get_buffer_create(" *test-occur*", h, st)
        call set_buffer(h, st)
        call erase_buffer(st)
        call insert(scratch_text(), st)
        call occur("and", st)
        call check(error, st%ok(), format_error(st))

        if (.not. allocated(error)) then
            occ = get_buffer("*Occur*")
            call set_buffer(occ, st)
            call revert_buffer(st, noconfirm=.true.)
            call check(error, st%ok(), format_error(st))
        end if

        if (.not. allocated(error)) then
            occ = get_buffer("*Occur*")
            call set_buffer(occ, st)
            call goto_char(point_min())
            call buffer_substring(point_min(), line_end_position(), text, st)
            call check(error, index(text, "2 matches for ""and"" in buffer: ") == 1, text)
            call kill_buffer(occ, st)
        end if

        list_matching_lines_jump_to_current_line = saved
        call kill_buffer(h, st)
    end subroutine revert_case

    !> Port of replace-tests.el replace-occur-revert-bug32543: `occur'
    !> revert with `list_matching_lines_jump_to_current_line' true.
    subroutine test_revert_32543(error)
        type(error_type), allocatable, intent(out) :: error

        call revert_case(error, .true.)
    end subroutine test_revert_32543

    !> Port of replace-tests.el replace-occur-revert-bug32987: `occur'
    !> revert with `list_matching_lines_jump_to_current_line' false.
    subroutine test_revert_32987(error)
        type(error_type), allocatable, intent(out) :: error

        call revert_case(error, .false.)
    end subroutine test_revert_32987

    !> Port of replace-tests.el test-count-matches.
    subroutine test_count_matches(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        integer(int64) :: n

        call enter("oooooooooo", h)
        call how_many("oo", n, st)
        call check(error, n, 5_int64)

        if (.not. allocated(error)) then
            call how_many("o+", n, st)
            call check(error, n, 1_int64)
        end if

        if (.not. allocated(error)) then
            call erase_buffer(st)
            call insert("o"//LF//LF//LF//LF//"o"//LF//LF, st)
            call goto_char(point_min())
            call how_many("^$", n, st)
            call check(error, n, 4_int64)
        end if

        call leave(h)
    end subroutine test_count_matches

    !> `keep_lines', `flush_lines', `kill_matching_lines' and
    !> `copy_matching_lines' from point.
    subroutine test_keep_lines(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        integer(int64) :: n

        call enter("a1"//LF//"b2"//LF//"a3"//LF//"c4"//LF, h)
        call keep_lines("a", st)
        call check(error, buffer_string(), "a1"//LF//"a3"//LF)

        if (.not. allocated(error)) then
            call erase_buffer(st)
            call insert("a1"//LF//"b2"//LF//"a3"//LF//"c4"//LF, st)
            call goto_char(point_min())
            call flush_lines("a", n, st)
            call check(error, buffer_string(), "b2"//LF//"c4"//LF)
            if (.not. allocated(error)) call check(error, n, 2_int64)
        end if

        if (.not. allocated(error)) then
            call erase_buffer(st)
            call insert("x"//LF//"yes"//LF//"z"//LF//"yes too"//LF, st)
            call goto_char(point_min())
            call kill_matching_lines("yes", n, st)
            call check(error, buffer_string(), "x"//LF//"z"//LF)
            if (.not. allocated(error)) call check(error, n, 2_int64)
        end if

        if (.not. allocated(error)) then
            call erase_buffer(st)
            call insert("x"//LF//"yes"//LF//"z"//LF, st)
            call goto_char(point_min())
            call copy_matching_lines("yes", n, st)
            call check(error, buffer_string(), "x"//LF//"yes"//LF//"z"//LF)
            if (.not. allocated(error)) call check(error, n, 1_int64)
        end if

        call leave(h)
    end subroutine test_keep_lines

    !> Occur mode: RET visits the occurrence, M-n and M-p move between
    !> matches, and the collect operation.
    subroutine test_occur_mode(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h, occ
        type(status_type) :: st

        call init_bindings()
        call get_buffer_create(" *test-occur*", h, st)
        call set_buffer(h, st)
        call erase_buffer(st)
        call insert("one"//LF//"two x"//LF//"three"//LF//"x four"//LF, st)
        call show_alone()
        call set_buffer(h, st)
        call occur("x", st)
        occ = get_buffer("*Occur*")
        call check(error, buffer_live_p(occ), "*Occur*")
        if (allocated(error)) return
        call set_buffer(occ, st)
        call check(error, buffers(cur)%major_mode, "occur_mode")
        if (allocated(error)) return
        ! The first line of matches.
        call goto_char(point_min())
        call occur_find_next(st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call occur_mode_goto_occurrence(st)
        call check(error, st%ok(), format_error(st))
        if (.not. allocated(error)) call check(error, buffers(cur)%name, " *test-occur*")
        if (.not. allocated(error)) call check(error, point(), 9_int64, "the x of line 2")
        if (allocated(error)) return
        ! Occur-Edit mode: an edit of the line in *Occur* changes the
        ! line of the buffer searched.
        call set_buffer(occ, st)
        call occur_edit_mode(st)
        call check(error, st%ok(), format_error(st))
        if (allocated(error)) return
        call goto_char(point_min())
        call occur_next(st)
        call goto_char(line_end_position())
        call insert("!", st)
        call set_buffer(h, st)
        call check(error, buffer_string(), "one"//LF//"two x!"//LF//"three"//LF//"x four"//LF)
        if (allocated(error)) return
        call set_buffer(occ, st)
        call occur_cease_edit(st)
        call check(error, buffers(cur)%major_mode, "occur_mode")
        if (allocated(error)) return
        call kill_buffer(occ, st)
        ! The collect operation.
        call set_buffer(h, st)
        call occur("\([a-z]+\) x", st, collect="\1")
        occ = get_buffer("*Occur*")
        call check(error, buffer_live_p(occ), "*Occur* (collect)")
        if (allocated(error)) return
        call set_buffer(occ, st)
        call check(error, buffer_string(), "two"//LF)
        call kill_buffer(occ, st)
        call kill_buffer(h, st)
    end subroutine test_occur_mode

    !> M-n from point in an Occur buffer.
    subroutine occur_find_next(status)
        type(status_type), intent(out) :: status

        call occur_next(status)
    end subroutine occur_find_next

    !> Port of replace-tests.el occur-test-00: element 0 of replace-occur-tests.
    subroutine test_occur_00(error)
        type(error_type), allocatable, intent(out) :: error

        call occur_case(error, "x", 0_int64, &
                        "xa"//LF//"b"//LF//"cx"//LF//"xd"//LF//"xex"//LF//"fx"//LF//"", &
                        "6 matches in 5 lines for ""x"" in buffer:  *test-occur*"//LF//"      1:xa"//LF &
                        //"      3:cx"//LF//"      4:xd"//LF//"      5:xex"//LF//"      6:fx"//LF//"")
    end subroutine test_occur_00

    !> Port of replace-tests.el occur-test-01: element 1 of replace-occur-tests.
    subroutine test_occur_01(error)
        type(error_type), allocatable, intent(out) :: error

        call occur_case(error, "a"//LF//"a", 0_int64, &
                        "a"//LF//"a"//LF//"a"//LF//"a"//LF//"a"//LF//"", &
                        "2 matches for ""a"//LF//"a"" in buffer:  *test-occur*"//LF//"      1:a"//LF &
                        //"       :a"//LF//"      3:a"//LF//"       :a"//LF//"")
    end subroutine test_occur_01

    !> Port of replace-tests.el occur-test-02: element 2 of replace-occur-tests.
    subroutine test_occur_02(error)
        type(error_type), allocatable, intent(out) :: error

        call occur_case(error, "a"//LF//"b", 0_int64, &
                        "a"//LF//"b"//LF//"c"//LF//"a"//LF//"b"//LF//"", &
                        "2 matches for ""a"//LF//"b"" in buffer:  *test-occur*"//LF//"      1:a"//LF &
                        //"       :b"//LF//"      4:a"//LF//"       :b"//LF//"")
    end subroutine test_occur_02

    !> Port of replace-tests.el occur-test-03: element 3 of replace-occur-tests.
    subroutine test_occur_03(error)
        type(error_type), allocatable, intent(out) :: error

        call occur_case(error, "a"//LF//"", 0_int64, &
                        "a"//LF//""//LF//"c"//LF//"a"//LF//""//LF//"", &
                        "2 matches for ""a"//LF//""" in buffer:  *test-occur*"//LF//"      1:a"//LF &
                        //"       :"//LF//"      4:a"//LF//"       :"//LF//"")
    end subroutine test_occur_03

    !> Port of replace-tests.el occur-test-04: element 4 of replace-occur-tests.
    subroutine test_occur_04(error)
        type(error_type), allocatable, intent(out) :: error

        call occur_case(error, "x"//LF//".x"//LF//"", 0_int64, &
                        "ax"//LF//"bx"//LF//"c"//LF//"d"//LF//"ex"//LF//"fx"//LF//"", &
                        "2 matches for ""x"//LF//".x"//LF//""" in buffer:  *test-occur*"//LF//"      1:ax"//LF &
                        //"       :bx"//LF//"       :c"//LF//"      5:ex"//LF//"       :fx"//LF//"       :" &
                        //LF//"")
    end subroutine test_occur_04

    !> Port of replace-tests.el occur-test-05: element 5 of replace-occur-tests.
    subroutine test_occur_05(error)
        type(error_type), allocatable, intent(out) :: error

        call occur_case(error, "x", 1_int64, &
                        "ax"//LF//"b"//LF//"c"//LF//"d"//LF//"ex"//LF//"f"//LF//"g"//LF//"hx"//LF//"", &
                        "3 matches for ""x"" in buffer:  *test-occur*"//LF//"      1:ax"//LF//"       :b"//LF &
                        //"-------"//LF//"       :d"//LF//"      5:ex"//LF//"       :f"//LF//"-------"//LF &
                        //"       :g"//LF//"      8:hx"//LF//"")
    end subroutine test_occur_05

    !> Port of replace-tests.el occur-test-06: element 6 of replace-occur-tests.
    subroutine test_occur_06(error)
        type(error_type), allocatable, intent(out) :: error

        call occur_case(error, "x", 1_int64, &
                        "a"//LF//"bx"//LF//"c"//LF//"d"//LF//"ex"//LF//"f"//LF//"", &
                        "2 matches for ""x"" in buffer:  *test-occur*"//LF//"       :a"//LF//"      2:bx"//LF &
                        //"       :c"//LF//"-------"//LF//"       :d"//LF//"      5:ex"//LF//"       :f"//LF &
                        //"")
    end subroutine test_occur_06

    !> Port of replace-tests.el occur-test-07: element 7 of replace-occur-tests.
    subroutine test_occur_07(error)
        type(error_type), allocatable, intent(out) :: error

        call occur_case(error, "x", 2_int64, &
                        "ax"//LF//"bx"//LF//"c"//LF//"dx"//LF//"e"//LF//"f"//LF//"gx"//LF//"h"//LF//"i"//LF &
                        //"j"//LF//"kx"//LF//"", &
                        "5 matches for ""x"" in buffer:  *test-occur*"//LF//"      1:ax"//LF//"      2:bx"//LF &
                        //"       :c"//LF//"      4:dx"//LF//"       :e"//LF//"       :f"//LF//"      7:gx" &
                        //LF//"       :h"//LF//"       :i"//LF//"       :j"//LF//"     11:kx"//LF//"")
    end subroutine test_occur_07

    !> Port of replace-tests.el occur-test-08: element 8 of replace-occur-tests.
    subroutine test_occur_08(error)
        type(error_type), allocatable, intent(out) :: error

        call occur_case(error, "x", 2_int64, &
                        "a"//LF//"b"//LF//"cx"//LF//"d"//LF//"e"//LF//"f"//LF//"gx"//LF//"h"//LF//"i"//LF//"", &
                        "2 matches for ""x"" in buffer:  *test-occur*"//LF//"       :a"//LF//"       :b"//LF &
                        //"      3:cx"//LF//"       :d"//LF//"       :e"//LF//"       :f"//LF//"      7:gx" &
                        //LF//"       :h"//LF//"       :i"//LF//"")
    end subroutine test_occur_08

    !> Port of replace-tests.el occur-test-09: element 9 of replace-occur-tests.
    subroutine test_occur_09(error)
        type(error_type), allocatable, intent(out) :: error

        call occur_case(error, "x", 2_int64, &
                        ""//LF//"b"//LF//"cx"//LF//"d"//LF//"e"//LF//"f"//LF//"gx"//LF//"h"//LF//""//LF//"", &
                        "2 matches for ""x"" in buffer:  *test-occur*"//LF//"       :"//LF//"       :b"//LF &
                        //"      3:cx"//LF//"       :d"//LF//"       :e"//LF//"       :f"//LF//"      7:gx" &
                        //LF//"       :h"//LF//"       :"//LF//"")
    end subroutine test_occur_09

    !> Port of replace-tests.el occur-test-10: element 10 of replace-occur-tests.
    subroutine test_occur_10(error)
        type(error_type), allocatable, intent(out) :: error

        call occur_case(error, "x"//LF//".x", 2_int64, &
                        "ax"//LF//"bx"//LF//"c"//LF//"d"//LF//"ex"//LF//"fx"//LF//"g"//LF//"h"//LF//"i"//LF &
                        //"jx"//LF//"kx"//LF//"", &
                        "3 matches for ""x"//LF//".x"" in buffer:  *test-occur*"//LF//"      1:ax"//LF &
                        //"       :bx"//LF//"       :c"//LF//"       :d"//LF//"      5:ex"//LF//"       :fx" &
                        //LF//"       :g"//LF//"       :h"//LF//"       :i"//LF//"     10:jx"//LF &
                        //"       :kx"//LF//"")
    end subroutine test_occur_10

    !> Port of replace-tests.el occur-test-11: element 11 of replace-occur-tests.
    subroutine test_occur_11(error)
        type(error_type), allocatable, intent(out) :: error

        call occur_case(error, "x"//LF//".x", 2_int64, &
                        "ax"//LF//"bx"//LF//"c"//LF//"d"//LF//"e"//LF//"f"//LF//"gx"//LF//"hx"//LF//"", &
                        "2 matches for ""x"//LF//".x"" in buffer:  *test-occur*"//LF//"      1:ax"//LF &
                        //"       :bx"//LF//"       :c"//LF//"       :d"//LF//"-------"//LF//"       :e"//LF &
                        //"       :f"//LF//"      7:gx"//LF//"       :hx"//LF//"")
    end subroutine test_occur_11

    !> Port of replace-tests.el occur-test-12: element 12 of replace-occur-tests.
    subroutine test_occur_12(error)
        type(error_type), allocatable, intent(out) :: error

        call occur_case(error, "x", -2_int64, &
                        "a"//LF//"bx"//LF//"c"//LF//"d"//LF//"e"//LF//"fx"//LF//"g"//LF//"h"//LF//"ix"//LF//"", &
                        "3 matches for ""x"" in buffer:  *test-occur*"//LF//"       :a"//LF//"      2:bx"//LF &
                        //"-------"//LF//"       :d"//LF//"       :e"//LF//"      6:fx"//LF//"-------"//LF &
                        //"       :g"//LF//"       :h"//LF//"      9:ix"//LF//"")
    end subroutine test_occur_12

    !> Port of replace-tests.el occur-test-13: element 13 of replace-occur-tests.
    subroutine test_occur_13(error)
        type(error_type), allocatable, intent(out) :: error

        call occur_case(error, "x", -3_int64, &
                        "a"//LF//"bx"//LF//"c"//LF//"dx"//LF//"e"//LF//"f"//LF//"gx"//LF//"h"//LF//"", &
                        "3 matches for ""x"" in buffer:  *test-occur*"//LF//"       :a"//LF//"      2:bx"//LF &
                        //"       :c"//LF//"      4:dx"//LF//"       :e"//LF//"       :f"//LF//"      7:gx" &
                        //LF//"")
    end subroutine test_occur_13
end module test_replace_fm
