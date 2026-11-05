! test_spellcheck_mod.f90 -- tests for spellcheck_mod.
! Ports GNU Emacs 31.1 test/lisp/textmodes/ispell-tests/ispell-tests.el in
! part; the tests that need a real speller's dictionaries, advice, the
! debug buffer, ispell-lookup-words, completion, the minor mode, nXML and
! Emacs Lisp modes, or the messages are not ported.
!
! Copyright (C) 2025-2026 Free Software Foundation, Inc.
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

!> ispell-tests.el's cases over upstream's fake speller,
!> test/ispell-resources/fake-aspell-new.bash (it says it is Aspell 0.60,
!> accepts every word but hellooooooo, badworddd and tarampampamtararam,
!> and learns the words sent with `@').  The tests that override
!> functions with advice check the error the misspelling gives instead.
!> Own tests: the output parser, the filter, the command loop's keys,
!> `spellcheck' on an active region.
module test_spellcheck_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use test_support_mod, only: show_alone
    use error_mod, only: status_type
    use handle_mod, only: handle_type
    use value_mod, only: value_type
    use buffer_mod, only: get_or_make_buffer, set_buffer, close_buffer, erase_buffer, restore_buffer_modified, &
                          transient_mark_mode, buffers, current
    use insert_delete_mod, only: insert
    use editing_functions_mod, only: goto_character, buffer_string, point, point_minimum, point_maximum, delete_region
    use file_input_output_mod, only: expand_file_name, fortmacs_wd, initialize_fileio
    use keymap_mod, only: key_parse
    use keyboard_mod, only: unread_command_events, discard_input, last_message, command_loop
    use execute_interactively_mod, only: prefix_argument_type, prefix_argument, current_prefix_argument
    use bindings_mod, only: initialize_bindings
    use simple_mod, only: fundamental_mode, set_mark
    use shell_script_mod, only: sh_mode
    use sgml_mode_mod, only: html_mode
    use spellcheck_mod, only: spellcheck_word, spellcheck_region, spellcheck_buffer, spellcheck, spellcheck_comments_and_strings, &
                              spellcheck_comment_or_string_at_point, spellcheck_change_dictionary, spellcheck_kill_spellcheck, &
                              spellcheck_pdict_save, spellcheck_check_version, spellcheck_init_process, spellcheck_process_status, &
                              spellcheck_add_per_file_word_list, spellcheck_buffer_local_words, spellcheck_parse_output, &
                              spellcheck_output_type, spellcheck_filter, spellcheck_program_name, spellcheck_check_only, &
                              spellcheck_dictionary, spellcheck_local_dictionary, spellcheck_pdict_modified, &
                              spellcheck_words_keyword, prepend_spellcheck_buffer_session_localword, spellcheck_get_word, &
                              spellcheck_highlight, SPELLCHECK_OUTPUT_CORRECT, SPELLCHECK_OUTPUT_ROOT, &
                              SPELLCHECK_OUTPUT_MISSPELLED, SPELLCHECK_OUTPUT_ERROR
    implicit none(type, external)
    private

    public :: collect_spellcheck_mod

    character(len=*), parameter :: LF = achar(10)
    !> ispell-tests--constants/english/correct-list.
    character(len=*), parameter :: CORRECT_TEXT = "hello test test more obvious word"
    !> ispell-tests--constants/english/wrong.
    character(len=*), parameter :: WRONG = "hellooooooo"

contains

    !> Collect all exported unit tests.
    subroutine collect_spellcheck_mod(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("ispell/ispell-program-name/noncommand/interactive", test_noncommand), &
                    new_unittest("ispell/ispell-program-name/non-executable/interactive", test_non_executable), &
                    new_unittest("ispell/ispell-add-per-file-word-list/simple", test_add_word_simple), &
                    new_unittest("ispell/ispell-add-per-file-word-list/comments", test_add_word_comments), &
                    new_unittest("ispell/ispell-add-per-file-word-list/nxml", test_add_word_html), &
                    new_unittest("ispell/ispell-add-per-file-word-list/keyword-there-space", test_add_word_keyword), &
                    new_unittest("ispell/ispell-buffer-local-words/ispell-words-keyword", test_local_words_keyword), &
                    new_unittest("ispell/ispell-buffer-local-words/ispell-buffer-session-localwords", &
                                 test_local_words_session), &
                    new_unittest("ispell/ispell-init-process/works-with-home", test_init_process), &
                    new_unittest("ispell/ispell--run-on-word/default", test_run_on_word), &
                    new_unittest("ispell/ispell-word/default/check-only/correct", test_word_correct), &
                    new_unittest("ispell/ispell-word/default/check-only/correct/add-init", test_word_correct_init), &
                    new_unittest("ispell/ispell-word/default/check-only/incorrect", test_word_incorrect), &
                    new_unittest("ispell/ispell-region/correct", test_region_correct), &
                    new_unittest("ispell/ispell-region/incorrect", test_region_incorrect), &
                    new_unittest("ispell/ispell-buffer/correct", test_buffer_correct), &
                    new_unittest("ispell/ispell-buffer/incorrect", test_buffer_incorrect), &
                    new_unittest("ispell/ispell-kill-ispell", test_kill_spellcheck), &
                    new_unittest("ispell/ispell/region", test_spellcheck_region_active), &
                    new_unittest("ispell/ispell-change-dictionary", test_change_dictionary), &
                    new_unittest("ispell/ispell-comments-and-strings/correct", test_comments_correct), &
                    new_unittest("ispell/ispell-comments-and-strings/incorrect", test_comments_incorrect), &
                    new_unittest("ispell/ispell-comment-or-string-at-point", test_comment_at_point), &
                    new_unittest("ispell/ispell-pdict-save", test_pdict_save), &
                    new_unittest("ispell/ispell-pdict-save/unmodified", test_pdict_save_unmodified), &
                    new_unittest("parse-output", test_parse_output), &
                    new_unittest("filter", test_filter), &
                    new_unittest("get-word", test_get_word), &
                    new_unittest("command-loop-replace", test_command_loop_replace), &
                    new_unittest("command-loop-accept", test_command_loop_accept) &
                    ]
    end subroutine collect_spellcheck_mod

    ! ------------------------------------------------------------------
    ! Fixtures
    ! ------------------------------------------------------------------

    !> The fake speller's file name.
    function fake_speller() result(path)
        character(len=:), allocatable :: path
        type(status_type) :: st

        call initialize_fileio()
        call expand_file_name("test/ispell-resources/fake-aspell-new.bash", path, st, fortmacs_wd)
    end function fake_speller

    !> A fresh buffer NAME shown alone, the fake speller as the program,
    !> no Ispell process, the default dictionary.
    subroutine fresh(name, h)
        character(len=*), intent(in) :: name
        type(handle_type), intent(out) :: h
        type(status_type) :: st

        call initialize_bindings()
        call discard_input()
        prefix_argument = prefix_argument_type()
        current_prefix_argument = prefix_argument_type()
        spellcheck_program_name = fake_speller()
        spellcheck_dictionary = ""
        spellcheck_check_only = .false.
        spellcheck_pdict_modified = .false.
        spellcheck_highlight = .false.
        call get_or_make_buffer(name, h, st)
        call set_buffer(h, st)
        call restore_buffer_modified(.false.)
        call erase_buffer(st)
        call fundamental_mode(st)
        call spellcheck_kill_spellcheck(st, no_error=.true., clear=.true.)
        call show_alone()
        last_message = ""
    end subroutine fresh

    !> Close buffer H and stop the speller.
    subroutine finish(h)
        type(handle_type), intent(in) :: h
        type(status_type) :: st

        call spellcheck_kill_spellcheck(st, no_error=.true., clear=.true.)
        call set_buffer(h, st)
        call restore_buffer_modified(.false.)
        call close_buffer(h, st)
        spellcheck_check_only = .false.
    end subroutine finish

    !> The detail of status ST, "" if none.
    function detail(st) result(text)
        type(status_type), intent(in) :: st
        character(len=:), allocatable :: text

        text = ""
        if (allocated(st%detail)) text = st%detail
    end function detail

    ! ------------------------------------------------------------------
    ! ispell-tests.el
    ! ------------------------------------------------------------------

    !> Should error or at least warn when `spellcheck_program_name' is a
    !> meaningless string.
    subroutine test_noncommand(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        character(len=:), allocatable :: result
        type(status_type) :: st

        call fresh("ispell-test", h)
        spellcheck_program_name = "baf8cc1a-a811-11f0-abc7-ef02b7a6dda1"
        call spellcheck_check_version(result, st)
        call finish(h)
        call check(error,.not. st%ok())
    end subroutine test_noncommand

    !> Should error or at least warn if `spellcheck_program_name' is a path to a
    !> non-executable.
    subroutine test_non_executable(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        character(len=:), allocatable :: result
        type(status_type) :: st

        call fresh("ispell-test", h)
        spellcheck_program_name = "/dev/null"
        call spellcheck_check_version(result, st, .true.)
        call finish(h)
        call check(error,.not. st%ok())
    end subroutine test_non_executable

    !> Adding a per-file word to an empty buffer.  No comment syntax
    !> expected.
    subroutine test_add_word_simple(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call fresh("ispell-test", h)
        call spellcheck_add_per_file_word_list("12345", st)
        call check(error, buffer_string(), LF//spellcheck_words_keyword//" 12345"//LF)
        call finish(h)
    end subroutine test_add_word_simple

    !> Adding a per-file word to an empty buffer.  Uses a comment syntax
    !> with no end marker (sh_mode's "# ", for upstream's emacs-lisp-mode
    !> and its ";"; the keyword follows `comment_start' and a space).
    subroutine test_add_word_comments(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call fresh("ispell-test", h)
        call sh_mode(st)
        call spellcheck_add_per_file_word_list("12345", st)
        call check(error, buffer_string(), LF//"#  "//spellcheck_words_keyword//" 12345"//LF)
        call finish(h)
    end subroutine test_add_word_comments

    !> Adding a per-file word to an empty buffer.  Uses html_mode's comment
    !> syntax (for upstream's nXML mode), which has an opening and a
    !> closing marker.
    subroutine test_add_word_html(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call fresh("ispell-test", h)
        call html_mode(st)
        call spellcheck_add_per_file_word_list("12345", st)
        call check(error, buffer_string(), LF//"<!-- "//LF//spellcheck_words_keyword//" 12345"//LF//" -->"//LF)
        call finish(h)
    end subroutine test_add_word_html

    !> Adding a per-file word to buffer with keyword.
    subroutine test_add_word_keyword(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call fresh("ispell-test", h)
        call html_mode(st)
        call insert(LF//"<!-- "//spellcheck_words_keyword//LF//"-->"//LF, st)
        call spellcheck_add_per_file_word_list("12345", st)
        call check(error, buffer_string(), LF//"<!-- "//spellcheck_words_keyword//" 12345"//LF//"-->"//LF)
        call finish(h)
    end subroutine test_add_word_keyword

    !> Send some words prefixed by @ from the file's footer to backend.
    subroutine test_local_words_keyword(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call fresh("ispell-test", h)
        call html_mode(st)
        call spellcheck_init_process(st)
        call check(error, st%ok(), "init: "//detail(st))
        if (allocated(error)) return
        call check(error, word_result(WRONG) == SPELLCHECK_OUTPUT_MISSPELLED)
        if (allocated(error)) return
        call spellcheck_add_per_file_word_list(WRONG, st)
        call spellcheck_buffer_local_words(st)
        call check(error, word_result(WRONG) == SPELLCHECK_OUTPUT_CORRECT)
        call finish(h)
    end subroutine test_local_words_keyword

    !> Send the buffer's session local words to backend.
    subroutine test_local_words_session(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call fresh("ispell-test", h)
        call html_mode(st)
        call spellcheck_init_process(st)
        call check(error, word_result(WRONG) == SPELLCHECK_OUTPUT_MISSPELLED)
        if (allocated(error)) return
        call prepend_spellcheck_buffer_session_localword(WRONG)
        call spellcheck_buffer_local_words(st)
        call check(error, word_result(WRONG) == SPELLCHECK_OUTPUT_CORRECT)
        call finish(h)
    end subroutine test_local_words_session

    !> Simple test to check that `spellcheck_init_process' works.
    subroutine test_init_process(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call fresh("ispell-test", h)
        call spellcheck_init_process(st)
        call check(error, st%ok(), detail(st))
        if (allocated(error)) return
        call check(error, spellcheck_process_status(), "run")
        call finish(h)
    end subroutine test_init_process

    !> The kind of the speller's answer for WORD, as ispell--run-on-word
    !> gives it: `spellcheck_word' in check-only mode on a copy of WORD added
    !> at the end of the buffer (and removed), the result read from the
    !> message.
    integer function word_result(word) result(kind)
        character(len=*), intent(in) :: word
        type(status_type) :: st
        integer(int64) :: start
        logical :: saved

        start = point_maximum()
        call goto_character(start)
        call insert(LF//word, st)
        saved = spellcheck_check_only
        spellcheck_check_only = .true.
        last_message = ""
        call spellcheck_word(st)
        spellcheck_check_only = saved
        kind = SPELLCHECK_OUTPUT_ERROR
        if (index(last_message, "is correct") > 0) kind = SPELLCHECK_OUTPUT_CORRECT
        if (index(last_message, "is incorrect") > 0) kind = SPELLCHECK_OUTPUT_MISSPELLED
        call delete_region(start, point_maximum(), st)
    end function word_result

    !> `ispell--run-on-word' should be the simplest interface for checking a
    !> word (through `spellcheck_word' in check-only mode).
    subroutine test_run_on_word(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call fresh("ispell-test", h)
        call html_mode(st)
        call check(error, word_result(WRONG) == SPELLCHECK_OUTPUT_MISSPELLED)
        if (allocated(error)) return
        call check(error, word_result("hello") == SPELLCHECK_OUTPUT_CORRECT)
        if (allocated(error)) return
        call check(error, word_result("obvious") == SPELLCHECK_OUTPUT_CORRECT)
        call finish(h)
    end subroutine test_run_on_word

    !> Check that `spellcheck_word' works with a default dictionary.
    subroutine test_word_correct(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call fresh("ispell-test", h)
        call insert("hello"//LF, st)
        call goto_character(1_int64)
        call spellcheck_word(st)
        call check(error, st%ok(), detail(st))
        if (allocated(error)) return
        call check(error, index(last_message, "is correct") > 0, last_message)
        call finish(h)
    end subroutine test_word_correct

    !> Check that `spellcheck_word' works with a default dictionary, after an
    !> explicit `spellcheck_init_process'.
    subroutine test_word_correct_init(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call fresh("ispell-test", h)
        spellcheck_check_only = .true.
        call spellcheck_init_process(st)
        call insert("hello"//LF, st)
        call goto_character(1_int64)
        call spellcheck_word(st)
        call check(error, index(last_message, "is correct") > 0, last_message)
        call finish(h)
    end subroutine test_word_correct_init

    !> Check that `spellcheck_word' works with a default dictionary, given a
    !> word which does not exist.
    subroutine test_word_incorrect(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call fresh("ispell-test", h)
        spellcheck_check_only = .true.
        call insert(WRONG//LF, st)
        call goto_character(1_int64)
        call spellcheck_word(st)
        call check(error, index(last_message, "is incorrect") > 0, last_message)
        call finish(h)
    end subroutine test_word_incorrect

    !> The simplest test for `spellcheck_region'.
    subroutine test_region_correct(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call fresh("ispell-test", h)
        call insert(CORRECT_TEXT, st)
        call goto_character(5_int64)
        call spellcheck_region(point(), point_maximum(), st)
        call check(error, st%ok(), detail(st))
        if (allocated(error)) return
        call check(error, index(last_message, "Spell-checking region using fake-aspell-new.bash with default "// &
                                "dictionary...done") == 1, last_message)
        call finish(h)
    end subroutine test_region_correct

    !> `spellcheck_region' on a misspelling gives an error condition (the fake
    !> speller's offset 0 is a misalignment).
    subroutine test_region_incorrect(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call fresh("ispell-test", h)
        spellcheck_check_only = .true.
        call insert("hello "//WRONG//" test", st)
        call goto_character(5_int64)
        call spellcheck_region(point(), point_maximum(), st)
        call check(error,.not. st%ok())
        call finish(h)
    end subroutine test_region_incorrect

    !> The simplest test for `spellcheck_buffer'.
    subroutine test_buffer_correct(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call fresh("ispell-test", h)
        call insert(CORRECT_TEXT, st)
        call spellcheck_buffer(st)
        call check(error, st%ok(), detail(st))
        if (allocated(error)) return
        call check(error, index(last_message, "Spell-checking ispell-test using fake-aspell-new.bash with default "// &
                                "dictionary...done") == 1, last_message)
        call finish(h)
    end subroutine test_buffer_correct

    !> `spellcheck_buffer' checks from the beginning of the buffer, whatever
    !> point: the misspelling there is offered (with no key to read, the
    !> session is suspended, as by X).
    subroutine test_buffer_incorrect(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call fresh("ispell-test", h)
        call insert(WRONG//" "//CORRECT_TEXT, st)
        call goto_character(10_int64)
        call spellcheck_buffer(st)
        call check(error, index(last_message, "Spell-checking suspended") == 1, last_message)
        call finish(h)
    end subroutine test_buffer_incorrect

    !> Test that killing ispell works.
    subroutine test_kill_spellcheck(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call fresh("ispell-test", h)
        call insert(CORRECT_TEXT, st)
        call spellcheck_init_process(st)
        call check(error, spellcheck_process_status(), "run")
        if (allocated(error)) return
        call spellcheck_kill_spellcheck(st, clear=.true.)
        call check(error, st%ok(), detail(st))
        if (allocated(error)) return
        call spellcheck_kill_spellcheck(st)
        call check(error, detail(st), "There is no Ispell process running!")
        call finish(h)
    end subroutine test_kill_spellcheck

    !> `spellcheck' checks the active region (ispell/ispell/region, with the
    !> message instead of advice).
    subroutine test_spellcheck_region_active(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        logical :: saved

        call fresh("ispell-test", h)
        saved = transient_mark_mode%value
        transient_mark_mode = .true.
        call insert(CORRECT_TEXT, st)
        call goto_character(2_int64)
        call set_mark(point())
        call goto_character(point_maximum())
        buffers(current)%mark_active = .true.
        call spellcheck(st)
        call check(error, index(last_message, "Spell-checking region using") == 1, last_message)
        buffers(current)%mark_active = .false.
        transient_mark_mode = saved
        call finish(h)
    end subroutine test_spellcheck_region_active

    !> Simple test for changing a dictionary.
    subroutine test_change_dictionary(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call fresh("ispell-test", h)
        call spellcheck_change_dictionary("english", st)
        call check(error, st%ok(), detail(st))
        if (allocated(error)) return
        call check(error, spellcheck_local_dictionary%value, "english")
        if (allocated(error)) return
        call spellcheck_change_dictionary("default", st)
        call check(error,.not. allocated(spellcheck_local_dictionary%value))
        if (allocated(error)) return
        call spellcheck_change_dictionary("english", st)
        call check(error, spellcheck_local_dictionary%value, "english")
        if (allocated(error)) return
        call spellcheck_change_dictionary("2110001888290146229", st)
        call check(error, detail(st), "Undefined dictionary: 2110001888290146229")
        call spellcheck_local_dictionary%clear_local()
        call finish(h)
    end subroutine test_change_dictionary

    !> Test that `spellcheck_comments_and_strings' does not err on a correct
    !> buffer.
    subroutine test_comments_correct(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call fresh("ispell-test", h)
        call insert("#!/bin/bash"//LF//"echo """//CORRECT_TEXT//""""//LF//"# commented line"//LF, st)
        call sh_mode(st)
        call spellcheck_comments_and_strings(st)
        call check(error, st%ok(), detail(st))
        if (allocated(error)) return
        call check(error, index(last_message, "Spell-checking region using") == 1, last_message)
        call finish(h)
    end subroutine test_comments_correct

    !> Test that `spellcheck_comments_and_strings' errs on a misspelled comment.
    subroutine test_comments_incorrect(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call fresh("ispell-test", h)
        call insert("#!/bin/bash"//LF//"echo """//CORRECT_TEXT//""""//LF//"# "//WRONG//LF, st)
        call sh_mode(st)
        call spellcheck_comments_and_strings(st)
        call check(error,.not. st%ok())
        call finish(h)
    end subroutine test_comments_incorrect

    !> Test that `spellcheck_comment_or_string_at_point' runs two tests, one
    !> correct and one incorrect in the same buffer.
    subroutine test_comment_at_point(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        character(len=*), parameter :: string1 = "#!/bin/bash"//LF//"echo""", string3 = """"//LF//"# "

        call fresh("ispell-test", h)
        call insert(string1//CORRECT_TEXT//string3//WRONG//LF, st)
        call sh_mode(st)
        call goto_character(int(len(string1) + len("hello"), int64))
        call spellcheck_comment_or_string_at_point(st)
        call check(error, st%ok(), detail(st))
        if (allocated(error)) return
        call check(error, index(last_message, "Spell-checking region using") == 1, last_message)
        if (allocated(error)) return
        call goto_character(int(len(string1) + len(CORRECT_TEXT) + len(string3) + 1, int64))
        call spellcheck_comment_or_string_at_point(st)
        call check(error,.not. st%ok())
        call finish(h)
    end subroutine test_comment_at_point

    !> Simple `spellcheck_pdict_save' test.
    subroutine test_pdict_save(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call fresh("ispell-test", h)
        call insert("12345", st)
        call spellcheck_pdict_save(st, .true., .true.)
        call check(error, st%ok(), detail(st))
        if (allocated(error)) return
        call check(error, last_message, "Personal dictionary saved.")
        call finish(h)
    end subroutine test_pdict_save

    !> An unmodified personal dictionary is not saved.
    subroutine test_pdict_save_unmodified(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call fresh("ispell-test", h)
        spellcheck_pdict_modified = .false.
        call spellcheck_pdict_save(st, .true., .false.)
        call check(error, last_message, "")
        if (allocated(error)) return
        spellcheck_pdict_modified = .true.
        call spellcheck_pdict_save(st, .true., .false.)
        call check(error, last_message, "Personal dictionary saved.")
        if (allocated(error)) return
        call check(error,.not. spellcheck_pdict_modified)
        call finish(h)
    end subroutine test_pdict_save_unmodified

    ! ------------------------------------------------------------------
    ! Own tests
    ! ------------------------------------------------------------------

    !> The forms of the speller's answers.
    subroutine test_parse_output(error)
        type(error_type), allocatable, intent(out) :: error
        type(spellcheck_output_type) :: r

        call spellcheck_parse_output("*", r)
        call check(error, r%kind == SPELLCHECK_OUTPUT_CORRECT)
        if (allocated(error)) return
        call spellcheck_parse_output("+ walk", r)
        call check(error, r%kind == SPELLCHECK_OUTPUT_ROOT .and. r%word == "walk")
        if (allocated(error)) return
        call spellcheck_parse_output("& teh 3 5: the, tea, ten, Th", r)
        call check(error, r%kind == SPELLCHECK_OUTPUT_MISSPELLED)
        if (allocated(error)) return
        call check(error, r%word, "teh")
        if (allocated(error)) return
        call check(error, r%offset == 5_int64)
        if (allocated(error)) return
        call check(error, size(r%misses) == 3 .and. size(r%guesses) == 1)
        if (allocated(error)) return
        call check(error, r%misses(2)%text, "tea")
        if (allocated(error)) return
        call check(error, r%guesses(1)%text, "Th")
        if (allocated(error)) return
        call spellcheck_parse_output("# xyzzy 7", r, shift=2_int64)
        call check(error, r%kind == SPELLCHECK_OUTPUT_MISSPELLED .and. r%offset == 9_int64 .and. size(r%misses) == 0)
    end subroutine test_parse_output

    !> The filter collects lines, a partial line continuing.
    subroutine test_filter(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call fresh("ispell-test", h)
        call spellcheck_init_process(st)
        call spellcheck_filter(h, "& wor", st)
        call spellcheck_filter(h, "d 0 1: word"//LF//LF, st)
        call check(error, st%ok())
        call finish(h)
    end subroutine test_filter

    !> `spellcheck_get_word' takes the word before point, with `otherchars'.
    subroutine test_get_word(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        character(len=:), allocatable :: word
        integer(int64) :: start, end

        call fresh("ispell-test", h)
        call insert("Steven's dog  ", st)
        call spellcheck_get_word(.false., word, start, end, st)
        call check(error, word, "dog")
        if (allocated(error)) return
        call goto_character(4_int64)
        call spellcheck_get_word(.false., word, start, end, st)
        call check(error, word, "Steven's")
        if (allocated(error)) return
        call check(error, start == 1_int64 .and. end == 9_int64)
        call finish(h)
    end subroutine test_get_word

    !> In the command loop, r replaces the word with typed-in text, which
    !> is rechecked.
    subroutine test_command_loop_replace(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        type(value_type), allocatable :: evs(:)

        call fresh("ispell-test", h)
        call insert("one "//WRONG//" two", st)
        call goto_character(8_int64)
        call key_parse("M-$ r C-a C-k h e l l o RET", evs, st)
        call unread_command_events(evs)
        call command_loop(st)
        call check(error, buffer_string(), "one hello two", last_message)
        call finish(h)
    end subroutine test_command_loop_replace

    !> In the command loop, a accepts the word for the session, so it is
    !> correct afterwards.
    subroutine test_command_loop_accept(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        type(value_type), allocatable :: evs(:)

        call fresh("ispell-test", h)
        call insert(WRONG, st)
        call key_parse("M-$ a", evs, st)
        call unread_command_events(evs)
        call command_loop(st)
        call check(error, buffer_string(), WRONG)
        if (allocated(error)) return
        last_message = ""
        call spellcheck_word(st)
        call check(error, index(last_message, "is correct") > 0, last_message)
        call finish(h)
    end subroutine test_command_loop_accept
end module test_spellcheck_mod
