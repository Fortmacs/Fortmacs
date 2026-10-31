! test_isearch_fm.f90 -- tests for incremental search (isearch_fm).
! Ports GNU Emacs 31.1 test/lisp/isearch-tests.el in part; only
! isearch--test-done; the others need invisible text, outlines or text
! properties.
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

!> Ports test/lisp/isearch-tests.el isearch--test-done (the others need
!> invisible text, outlines or text properties); the rest drive isearch
!> through the command loop.  The buffer is "foo bar foo baz Foo"
!> unless a test says otherwise.
module test_isearch_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use test_support, only: show_alone
    use error_fm, only: status_type
    use handle_fm, only: handle_type
    use lisp_fm, only: value_type, list_length, list_elt, xstring
    use lread_fm, only: sym
    use data_fm, only: set, symbol_value
    use buffer_fm, only: get_buffer_create, set_buffer, kill_buffer
    use insdel_fm, only: insert
    use editfns_fm, only: goto_char, point, mark_position
    use simple_fm, only: kill_new
    use keymap_fm, only: key_parse, overriding_terminal_local_map
    use keyboard_fm, only: unread_command_events, discard_input, last_message, command_loop
    use callint_fm, only: prefix_arg
    use bindings_fm, only: init_bindings
    use isearch_fm, only: isearch_done, isearch_update, isearch_active, isearch_string, isearch_regexp, &
                          isearch_lax_whitespace
    implicit none(type, external)
    private

    public :: collect_isearch_fm

contains

    !> Collect all exported unit tests.
    subroutine collect_isearch_fm(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("isearch--test-done", test_done), &
                    new_unittest("isearch-typing", test_typing), &
                    new_unittest("isearch-repeat-and-wrap", test_repeat), &
                    new_unittest("isearch-delete-and-abort", test_abort), &
                    new_unittest("isearch-case", test_case), &
                    new_unittest("isearch-backward", test_backward), &
                    new_unittest("isearch-other-command-exits", test_exit), &
                    new_unittest("isearch-prefix-argument", test_prefix), &
                    new_unittest("isearch-search-ring", test_ring), &
                    new_unittest("isearch-nonincremental", test_nonincremental), &
                    new_unittest("isearch-yank", test_yank), &
                    new_unittest("isearch-yank-word-or-char", test_yank_word), &
                    new_unittest("isearch-regexp", test_regexp), &
                    new_unittest("isearch-regexp-incomplete-and-fallback", test_regexp_fallback), &
                    new_unittest("isearch-toggle-regexp", test_toggle_regexp), &
                    new_unittest("isearch-lax-whitespace", test_lax_whitespace), &
                    new_unittest("isearch-control-char-message", test_control_message), &
                    new_unittest("isearch-edit-string-at-failure", test_edit_at_failure) &
                    ]
    end subroutine collect_isearch_fm

    subroutine enter(text, h)
        character(len=*), intent(in) :: text
        type(handle_type), intent(out) :: h
        type(status_type) :: st

        call init_bindings()
        call discard_input()
        prefix_arg = value_type()
        call get_buffer_create("isearch-test", h, st)
        call set_buffer(h, st)
        call insert(text, st)
        call goto_char(1_int64)
        last_message = ""
        call set(sym("search_ring"), value_type(), st)
        call show_alone()
    end subroutine enter

    subroutine leave(h)
        type(handle_type), intent(in) :: h
        type(status_type) :: st

        if (isearch_active) call isearch_done()
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

    subroutine test_done(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h

        ! Emacs always has a current buffer; give the test its own.
        call enter("", h)
        ! Normal operation.
        call isearch_update()
        call isearch_done()
        call check(error,.not. isearch_active)

        if (allocated(error)) then
            call leave(h)
            return
        end if

        ! Bug #21091: let `isearch_done' work without `isearch-update'.
        call isearch_done()
        call check(error,.not. isearch_active .and. overriding_terminal_local_map%index == 0)
        call leave(h)
    end subroutine test_done

    subroutine test_typing(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h

        call enter("foo bar foo baz Foo", h)
        call type_keys("C-s f o")
        call check(error, isearch_active .and. point() == 3, "found as typed")
        if (allocated(error)) return
        call check(error, last_message == "I-search: fo", last_message)
        if (allocated(error)) return
        call type_keys("o RET")
        call check(error,.not. isearch_active .and. point() == 4, "RET exits")
        call leave(h)
    end subroutine test_typing

    subroutine test_repeat(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h

        call enter("foo bar foo baz Foo", h)
        call type_keys("C-s f o o C-s")
        call check(error, point() == 12, "second")
        if (allocated(error)) return
        call type_keys("C-s")
        call check(error, point() == 20, "folds case: Foo")
        if (allocated(error)) return
        call type_keys("C-s")
        call check(error, point() == 20 .and. last_message == "Failing I-search: foo", last_message)
        if (allocated(error)) return
        call type_keys("C-s")
        call check(error, point() == 4 .and. last_message == "Overwrapped I-search: foo", last_message)
        if (allocated(error)) return
        ! From the middle: plain wrapped.
        call type_keys("RET")
        call goto_char(9_int64)
        call type_keys("C-s b a C-s")
        call check(error, point() == 15 .and. last_message == "Failing I-search: ba", last_message)
        if (allocated(error)) return
        call type_keys("C-s")
        call check(error, point() == 7 .and. last_message == "Wrapped I-search: ba", last_message)
        call leave(h)
    end subroutine test_repeat

    subroutine test_abort(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h

        call enter("foo bar foo baz Foo", h)
        call goto_char(2_int64)
        call type_keys("C-s b a DEL")
        call check(error, isearch_active .and. point() == 6 .and. isearch_string == "b", "DEL")
        if (allocated(error)) return
        call type_keys("x y")
        call check(error, point() == 6 .and. last_message == "Failing I-search: bxy", last_message)
        if (allocated(error)) return
        ! C-g when failing rubs out the failing part.
        call type_keys("C-g")
        call check(error, isearch_active .and. isearch_string == "b" .and. point() == 6, "C-g failing")
        if (allocated(error)) return
        ! C-g when successful goes back and quits.
        call type_keys("C-g")
        call check(error,.not. isearch_active .and. point() == 2 .and. last_message == "Quit", last_message)
        call leave(h)
    end subroutine test_abort

    subroutine test_case(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h

        call enter("foo bar foo baz Foo", h)
        ! An upper-case letter makes the search case-sensitive.
        call type_keys("C-s F")
        call check(error, point() == 18, "F")
        if (allocated(error)) return
        call type_keys("RET")
        call goto_char(1_int64)
        ! M-c toggles: case-sensitive...
        call type_keys("C-s M-c f o o C-s C-s")
        call check(error, point() == 12 .and. last_message == "Failing case-sensitive I-search: foo", &
                   last_message)
        if (allocated(error)) return
        call type_keys("RET")
        call goto_char(1_int64)
        ! ... and insensitive even with an upper-case letter.
        call type_keys("C-s M-c M-c F o o")
        call check(error, point() == 4, "case insensitive")
        call leave(h)
    end subroutine test_case

    subroutine test_backward(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h

        call enter("foo bar foo baz Foo", h)
        call goto_char(20_int64)
        call type_keys("C-r f o")
        call check(error, point() == 17 .and. last_message == "I-search backward: fo", last_message)
        if (allocated(error)) return
        call type_keys("C-r")
        call check(error, point() == 9, "again")
        if (allocated(error)) return
        ! C-s turns around: the match after the current one's start.
        call type_keys("C-s")
        call check(error, point() == 11 .and. last_message == "I-search: fo", last_message)
        call leave(h)
    end subroutine test_backward

    subroutine test_exit(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h

        call enter("foo bar foo baz Foo", h)
        call type_keys("C-s b a r C-f")
        call check(error,.not. isearch_active .and. point() == 9, "C-f ends the search and runs")
        if (allocated(error)) return
        ! The search string went into the ring.
        call check(error, ring_first() == "bar", ring_first())
        call leave(h)
    end subroutine test_exit

    subroutine test_prefix(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h

        call enter("foo bar foo baz Foo", h)
        ! C-u 2 C-s skips a match; the prefix argument doesn't end the search.
        call type_keys("C-s f o o C-u 2 C-s")
        call check(error, isearch_active .and. point() == 20, "C-u 2 C-s")
        if (allocated(error)) return
        call type_keys("RET")
        call leave(h)
        ! A count for a printing character.
        call enter("aaab", h)
        call type_keys("C-s C-u 3 a b")
        call check(error, isearch_string == "aaab" .and. point() == 5, isearch_string)
        call leave(h)
    end subroutine test_prefix

    subroutine test_ring(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h

        call enter("foo bar foo baz Foo", h)
        call type_keys("C-s b a RET")
        call goto_char(1_int64)
        ! C-s C-s searches for the last string.
        call type_keys("C-s C-s")
        call check(error, isearch_active .and. isearch_string == "ba" .and. point() == 7, isearch_string)
        if (allocated(error)) return
        call type_keys("RET")
        call check(error, list_length(ring()) == 1, "no duplicate")
        call leave(h)
    end subroutine test_ring

    subroutine test_nonincremental(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h

        call enter("foo bar foo baz Foo", h)
        ! RET at once reads a string and searches for it.
        call type_keys("C-s RET b a z RET")
        call check(error,.not. isearch_active .and. point() == 16, "nonincremental search")
        if (allocated(error)) return
        call check(error, ring_first() == "baz", ring_first())
        if (allocated(error)) return
        ! M-e edits the string.
        call goto_char(1_int64)
        call type_keys("C-s b a M-e r RET")
        call check(error, isearch_active .and. isearch_string == "bar" .and. point() == 8, isearch_string)
        call leave(h)
    end subroutine test_nonincremental

    subroutine test_yank(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h

        call enter("Foo bar"//achar(10)//"foo bar", h)
        ! C-M-y yanks characters, downcased (search-upper-case not-yanks).
        call type_keys("C-s C-M-y C-M-y")
        call check(error, isearch_string == "fo" .and. point() == 3, isearch_string)
        if (allocated(error)) return
        ! M-s C-e yanks the rest of the line.
        call type_keys("M-s C-e")
        call check(error, isearch_string == "foo bar" .and. point() == 8, isearch_string)
        if (allocated(error)) return
        call type_keys("RET")
        call goto_char(1_int64)
        ! C-y yanks the last kill; M-y replaces it with the one before.
        call kill_new("BAR")
        call kill_new("oo b")
        call type_keys("C-s C-y")
        call check(error, isearch_string == "oo b" .and. point() == 6, isearch_string)
        if (allocated(error)) return
        call type_keys("M-y")
        call check(error, isearch_string == "bar" .and. point() == 8, isearch_string)
        call leave(h)
    end subroutine test_yank

    !> C-w yanks a word, or a character when no word follows.
    subroutine test_yank_word(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h

        call enter("Foo bar, baz", h)
        call type_keys("C-s C-w")
        call check(error, isearch_string == "foo" .and. point() == 4, isearch_string)
        if (allocated(error)) return
        ! A space, then a word: the word comes too.
        call type_keys("C-w")
        call check(error, isearch_string == "foo bar" .and. point() == 8, isearch_string)
        if (allocated(error)) return
        ! A comma and a space: one character.
        call type_keys("C-w")
        call check(error, isearch_string == "foo bar," .and. point() == 9, isearch_string)
        call leave(h)
    end subroutine test_yank_word

    function ring() result(v)
        type(value_type) :: v
        type(status_type) :: st

        call symbol_value(sym("search_ring"), v, st)
        if (.not. st%ok()) v = value_type()
    end function ring

    function ring_first() result(s)
        character(len=:), allocatable :: s
        type(value_type) :: v

        s = ""
        v = ring()
        if (list_length(v) > 0) s = xstring(list_elt(v, 1_int64))
    end function ring_first

    subroutine test_regexp(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(value_type) :: ring
        type(status_type) :: st

        call enter("abc 123 x45", h)
        call type_keys("C-M-s [ 0 - 9 ] +")
        call check(error, isearch_active .and. isearch_regexp .and. point() == 8, "found")
        if (allocated(error)) return
        call check(error, last_message == "Regexp I-search: [0-9]+", last_message)
        if (allocated(error)) return
        call type_keys("C-s")
        call check(error, point() == 12, "repeat")
        if (allocated(error)) return
        call type_keys("RET")
        call symbol_value(sym("regexp_search_ring"), ring, st)
        call check(error, st%ok() .and. list_length(ring) == 1, "regexp ring")
        if (allocated(error)) return
        call check(error, xstring(list_elt(ring, 1_int64)) == "[0-9]+", "ring element")
        if (allocated(error)) return
        ! C-u C-s is a regexp search too; backward with C-M-r.
        call type_keys("C-M-r [ a - c ] + RET")
        call check(error, point() == 3, "backward regexp: "//isearch_string)
        if (allocated(error)) return
        call type_keys("C-u C-s x . RET")
        call check(error, point() == 11, "C-u C-s")
        call leave(h)
    end subroutine test_regexp

    subroutine test_regexp_fallback(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h

        call enter("ac abbc", h)
        call type_keys("C-M-s \ (")
        call check(error, last_message == "Regexp I-search: \( [incomplete input]", last_message)
        if (allocated(error)) return
        call type_keys("DEL DEL a b")
        call check(error, point() == 6, "ab")
        if (allocated(error)) return
        ! * makes the regexp more liberal: go back to where the previous
        ! successful search started.
        call type_keys("*")
        call check(error, point() == 2 .and. isearch_string == "ab*", "ab*")
        call type_keys("RET")
        call leave(h)
    end subroutine test_regexp_fallback

    subroutine test_toggle_regexp(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h

        call enter("f.o foo", h)
        call type_keys("C-s f . o")
        call check(error, point() == 4, "literal f.o")
        if (allocated(error)) return
        call type_keys("M-r C-s")
        call check(error, isearch_regexp .and. point() == 8, "regexp f.o")
        if (allocated(error)) return
        call check(error, last_message == "Regexp I-search: f.o", last_message)
        call type_keys("RET")
        call leave(h)
    end subroutine test_toggle_regexp

    subroutine test_lax_whitespace(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h

        call enter("foo   bar foo bar", h)
        call type_keys("C-s f o o SPC b")
        call check(error, point() == 8, "lax: spaces match a run of whitespace")
        if (allocated(error)) return
        call type_keys("M-s SPC")
        call check(error,.not. isearch_lax_whitespace, "toggled")
        if (allocated(error)) return
        call type_keys("C-s")
        call check(error, point() == 16, "literal space")
        call type_keys("RET M-s SPC")
        call type_keys("C-s M-s SPC RET")
        call check(error, isearch_lax_whitespace, "toggled back")
        call leave(h)
    end subroutine test_lax_whitespace

    !> Control characters in the search string show as ^C.
    subroutine test_control_message(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h

        call enter("a"//achar(10)//"b", h)
        call type_keys("C-s a C-q C-j")
        call check(error, last_message == "I-search: a^J", last_message)
        if (allocated(error)) return
        call check(error, isearch_string == "a"//achar(10), "the string has the newline")
        call leave(h)
    end subroutine test_control_message

    !> M-e on a failing search puts point where the search string stops
    !> matching.
    subroutine test_edit_at_failure(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h

        call enter("abc", h)
        call type_keys("C-s a b x M-e Z RET")
        call check(error, isearch_string == "abZx", isearch_string)
        call leave(h)
    end subroutine test_edit_at_failure
end module test_isearch_fm
