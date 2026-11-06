! test_spellcheck_mode_mod.f90 -- tests for spellcheck_mode_mod.
! Own tests; upstream has no flyspell tests.
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

!> Flyspell over a fake speller, test/flyspell-resources/fake-speller.sh
!> (it says it is Aspell 0.60, answers the Ispell pipe protocol and
!> Aspell's `list', and finds only helo (corrections hello, help), wrld
!> (world) and badworddd misspelled): checking a word, repeated words and
!> their exceptions, small and large regions, the buffer's local words,
!> moving to the errors, cycling through corrections, the mode and its
!> hooks, the messages without highlighting, the mode line and the
!> programming-mode test.
module test_spellcheck_mode_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use test_support_mod, only: show_alone
    use error_mod, only: status_type
    use handle_mod, only: handle_type
    use value_mod, only: value_type, vtag, V_NAME, symbols, xsymbol
    use buffer_mod, only: get_or_make_buffer, set_buffer, close_buffer, erase_buffer, restore_buffer_modified, buffers, &
                          current, current_buffer
    use insert_delete_mod, only: insert
    use editing_functions_mod, only: goto_character, buffer_string, point, point_minimum, point_maximum
    use file_input_output_mod, only: expand_file_name, fortmacs_wd, initialize_fileio
    use keyboard_mod, only: discard_input, last_message, last_command, post_command_hook, pre_command_hook
    use execute_interactively_mod, only: prefix_argument_type, prefix_argument, current_prefix_argument
    use commands_mod, only: commands
    use modes_mod, only: minor_modes
    use bindings_mod, only: initialize_bindings
    use simple_mod, only: fundamental_mode
    use interval_tree_mod, only: overlays_in, overlay_get, overlay_start, overlay_end
    use text_properties_mod, only: put_text_property
    use spellcheck_mod, only: spellcheck_kill_spellcheck, spellcheck_program_name, spellcheck_dictionary, &
                              spellcheck_pdict_modified, spellcheck_words_keyword
    use spellcheck_mode_mod, only: spellcheck_word_highlight, spellcheck_buffer_highlight, spellcheck_region_highlight, &
                                   goto_next_spellcheck_error, auto_correct_spellcheck_word, spellcheck_post_command_hook, &
                                   spellcheck_pre_command_hook, is_spellcheck_overlay, is_spellcheck_programming_text, &
                                   spellcheck_large_region_variable, spellcheck_highlight_flag, spellcheck_issue_message_flag, &
                                   spellcheck_mark_duplications_flag, spellcheck_sort_corrections, spellcheck_incorrect, &
                                   spellcheck_duplicate, spellcheck_mode_line_string
    implicit none(type, external)
    private

    public :: collect_spellcheck_mode_mod

    character(len=*), parameter :: LF = achar(10)
    character(len=*), parameter :: P_FACE = "face", STRING_FACE = "font_lock_string_face"

contains

    !> Collect all exported unit tests.
    subroutine collect_spellcheck_mode_mod(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("flyspell-word/correct", test_word_correct), &
                    new_unittest("flyspell-word/incorrect", test_word_incorrect), &
                    new_unittest("flyspell-word/duplicate", test_word_duplicate), &
                    new_unittest("flyspell-word/duplication-exception", test_word_exception), &
                    new_unittest("flyspell-word/duplicate-misspelling", test_word_duplicate_misspelling), &
                    new_unittest("flyspell-word/messages", test_word_messages), &
                    new_unittest("flyspell-buffer/small", test_buffer_small), &
                    new_unittest("flyspell-buffer/large", test_buffer_large), &
                    new_unittest("flyspell-buffer/large-localwords", test_buffer_large_localwords), &
                    new_unittest("flyspell-goto-next-error", test_goto_next_error), &
                    new_unittest("flyspell-auto-correct-word", test_auto_correct), &
                    new_unittest("flyspell-mode", test_mode), &
                    new_unittest("flyspell-mode/post-command", test_post_command), &
                    new_unittest("flyspell-mode/lighter", test_lighter), &
                    new_unittest("flyspell-generic-progmode-verify", test_programming_text) &
                    ]
    end subroutine collect_spellcheck_mode_mod

    ! ------------------------------------------------------------------
    ! Fixtures
    ! ------------------------------------------------------------------

    !> The fake speller's file name.
    function fake_speller() result(path)
        character(len=:), allocatable :: path
        type(status_type) :: st

        call initialize_fileio()
        call expand_file_name("test/flyspell-resources/fake-speller.sh", path, st, fortmacs_wd)
    end function fake_speller

    !> A fresh buffer NAME shown alone holding TEXT, point at its end, the
    !> fake speller as the program, no speller process, the defaults of the
    !> options.
    subroutine fresh(name, text, h)
        character(len=*), intent(in) :: name, text
        type(handle_type), intent(out) :: h
        type(status_type) :: st

        call initialize_bindings()
        call discard_input()
        prefix_argument = prefix_argument_type()
        current_prefix_argument = prefix_argument_type()
        spellcheck_program_name = fake_speller()
        spellcheck_dictionary = ""
        spellcheck_pdict_modified = .false.
        spellcheck_highlight_flag = .true.
        spellcheck_issue_message_flag = .true.
        spellcheck_mark_duplications_flag = .true.
        spellcheck_sort_corrections = .false.
        spellcheck_large_region_variable%characters = 1000
        call get_or_make_buffer(name, h, st)
        call set_buffer(h, st)
        call restore_buffer_modified(.false.)
        call erase_buffer(st)
        call fundamental_mode(st)
        call spellcheck_kill_spellcheck(st, no_error=.true., clear=.true.)
        call show_alone()
        call insert(text, st)
        last_message = ""
    end subroutine fresh

    !> Close buffer H and stop the speller.
    subroutine finish(h)
        type(handle_type), intent(in) :: h
        type(status_type) :: st

        call spellcheck_kill_spellcheck(st, no_error=.true., clear=.true.)
        call set_buffer(h, st)
        if (minor_modes%spellcheck_mode%enabled()) call minor_modes%spellcheck_mode%disable(st)
        call restore_buffer_modified(.false.)
        call close_buffer(h, st)
    end subroutine finish

    !> The detail of status ST, "" if none.
    function detail(st) result(text)
        type(status_type), intent(in) :: st
        character(len=:), allocatable :: text

        text = ""
        if (allocated(st%detail)) text = st%detail
    end function detail

    !> The flyspell overlays of the buffer as "START-END:FACE" items, in
    !> order of their starts.
    function highlights() result(text)
        type(handle_type), allocatable :: found(:)
        character(len=:), allocatable :: text
        character(len=48) :: item
        type(value_type) :: face
        integer(int64) :: last
        integer :: k, best

        text = ""
        call overlays_in(point_minimum(), point_maximum(), found)
        last = 0

        do
            best = 0

            do k = 1, size(found)
                if (.not. is_spellcheck_overlay(found(k))) cycle
                if (overlay_start(found(k)) <= last) cycle

                if (best == 0) then
                    best = k
                else if (overlay_start(found(k)) < overlay_start(found(best))) then
                    best = k
                end if
            end do

            if (best == 0) exit
            face = overlay_get(found(best), "face")
            write (item, '(i0, "-", i0, ":")') overlay_start(found(best)), overlay_end(found(best))
            text = text//trim(item)
            if (vtag(face) == V_NAME) text = text//symbols(xsymbol(face))%name
            text = text//" "
            last = overlay_start(found(best))
        end do
    end function highlights

    ! ------------------------------------------------------------------
    ! Checking words
    ! ------------------------------------------------------------------

    !> A correct word is not highlighted.
    subroutine test_word_correct(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        logical :: correct

        call fresh("flyspell-test", "hello world", h)
        call spellcheck_word_highlight(st, correct=correct)
        call check(error, st%ok(), "spellcheck_word_highlight: "//detail(st))
        if (.not. allocated(error)) call check(error, correct)
        if (.not. allocated(error)) call check(error, highlights(), "")
        call finish(h)
    end subroutine test_word_correct

    !> A misspelled word is highlighted with `spellcheck_incorrect'; point
    !> does not move.
    subroutine test_word_incorrect(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        logical :: correct

        call fresh("flyspell-test", "this helo there", h)
        call goto_character(10_int64)
        call spellcheck_word_highlight(st, correct=correct)
        call check(error, st%ok(), "spellcheck_word_highlight: "//detail(st))
        if (.not. allocated(error)) call check(error,.not. correct)
        if (.not. allocated(error)) call check(error, highlights(), "6-10:"//trim(spellcheck_incorrect%name)//" ")
        if (.not. allocated(error)) call check(error, point() == 10_int64)
        call finish(h)
    end subroutine test_word_incorrect

    !> A word repeated right after itself is highlighted with
    !> `spellcheck_duplicate'.
    subroutine test_word_duplicate(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call fresh("flyspell-test", "see the the cat", h)
        call goto_character(12_int64)
        call spellcheck_word_highlight(st)
        call check(error, st%ok(), "spellcheck_word_highlight: "//detail(st))
        if (.not. allocated(error)) call check(error, highlights(), "9-12:"//trim(spellcheck_duplicate%name)//" ")
        call finish(h)
    end subroutine test_word_duplicate

    !> The words of `spellcheck_mark_duplications_exceptions' may repeat.
    subroutine test_word_exception(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call fresh("flyspell-test", "so that that is", h)
        call goto_character(13_int64)
        call spellcheck_word_highlight(st)
        call check(error, st%ok(), "spellcheck_word_highlight: "//detail(st))
        if (.not. allocated(error)) call check(error, highlights(), "")
        call finish(h)
    end subroutine test_word_exception

    !> A misspelled word found elsewhere in the buffer is highlighted with
    !> `spellcheck_duplicate'.
    subroutine test_word_duplicate_misspelling(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call fresh("flyspell-test", "helo you, helo", h)
        call spellcheck_word_highlight(st)
        call check(error, st%ok(), "spellcheck_word_highlight: "//detail(st))
        if (.not. allocated(error)) call check(error, highlights(), "11-15:"//trim(spellcheck_duplicate%name)//" ")
        call finish(h)
    end subroutine test_word_duplicate_misspelling

    !> Without highlighting, a misspelling is reported in the echo area.
    subroutine test_word_messages(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        character(len=*), parameter :: LQ = char(226)//char(128)//char(152), RQ = char(226)//char(128)//char(153)

        call fresh("flyspell-test", "a helo", h)
        spellcheck_highlight_flag = .false.
        call spellcheck_word_highlight(st)
        call check(error, st%ok(), "spellcheck_word_highlight: "//detail(st))
        if (.not. allocated(error)) call check(error, highlights(), "")
        if (.not. allocated(error)) call check(error, last_message, "misspelling "//LQ//"helo"//RQ//"  [""hello"", ""help""]")
        call finish(h)
    end subroutine test_word_messages

    ! ------------------------------------------------------------------
    ! Regions
    ! ------------------------------------------------------------------

    !> A small region is checked word by word.
    subroutine test_buffer_small(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call fresh("flyspell-test", "helo big wrld ok", h)
        call spellcheck_buffer_highlight(st)
        call check(error, st%ok(), "spellcheck_buffer_highlight: "//detail(st))
        if (.not. allocated(error)) call check(error, highlights(), "1-5:"//trim(spellcheck_incorrect%name)//" 10-14:"// &
                                               trim(spellcheck_incorrect%name)//" ")
        if (.not. allocated(error)) call check(error, last_message, "Spell Checking completed.")
        call finish(h)
    end subroutine test_buffer_small

    !> A large region goes through the speller's `list' command.
    subroutine test_buffer_large(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call fresh("flyspell-test", "helo big wrld ok badworddd", h)
        spellcheck_large_region_variable%characters = 5
        call spellcheck_buffer_highlight(st)
        call check(error, st%ok(), "spellcheck_buffer_highlight: "//detail(st))
        if (.not. allocated(error)) call check(error, highlights(), "1-5:"//trim(spellcheck_incorrect%name)//" 10-14:"// &
                                               trim(spellcheck_incorrect%name)//" 18-27:"//trim(spellcheck_incorrect%name)//" ")
        call finish(h)
    end subroutine test_buffer_large

    !> In a large region, the buffer's local words are not marked.
    subroutine test_buffer_large_localwords(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call fresh("flyspell-test", "helo big wrld"//LF//spellcheck_words_keyword//"helo"//LF, h)
        spellcheck_large_region_variable%characters = 5
        call spellcheck_buffer_highlight(st)
        call check(error, st%ok(), "spellcheck_buffer_highlight: "//detail(st))
        if (.not. allocated(error)) call check(error, highlights(), "10-14:"//trim(spellcheck_incorrect%name)//" ")
        call finish(h)
    end subroutine test_buffer_large_localwords

    !> C-, moves to each highlighted word in turn, then says there is no
    !> more.
    subroutine test_goto_next_error(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call fresh("flyspell-test", "ok helo big wrld ok", h)
        call spellcheck_buffer_highlight(st)
        call goto_character(1_int64)
        call goto_next_spellcheck_error(st)
        call check(error, point() == 4_int64, "first error")
        if (allocated(error)) return
        call goto_next_spellcheck_error(st)
        call check(error, point() == 13_int64, "second error")
        if (allocated(error)) return
        call goto_next_spellcheck_error(st)
        call check(error, point() == point_maximum(), "end")
        if (.not. allocated(error)) call check(error, last_message, "No more miss-spelled words")

        if (.not. allocated(error)) then
            call goto_next_spellcheck_error(st, previous=.true.)
            call check(error, point() == 13_int64, "previous error")
        end if

        call finish(h)
    end subroutine test_goto_next_error

    !> M-TAB replaces the word with its first correction, then cycles
    !> through the others and back to the word itself.
    subroutine test_auto_correct(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call fresh("flyspell-test", "say helo", h)
        last_command => null()
        call auto_correct_spellcheck_word(st)
        call check(error, st%ok(), "auto-correct: "//detail(st))
        if (.not. allocated(error)) call check(error, buffer_string(), "say hello")
        if (.not. allocated(error)) call check(error, last_message, "Corrections: hello hello help helo hello help helo "// &
                                               "hello help helo hello help helo")

        if (allocated(error)) then
            call finish(h)
            return
        end if

        last_command => commands%auto_correct_spellcheck_word
        call auto_correct_spellcheck_word(st)
        call check(error, buffer_string(), "say help")

        if (.not. allocated(error)) then
            call auto_correct_spellcheck_word(st)
            call check(error, buffer_string(), "say helo")
        end if

        if (.not. allocated(error)) then
            call auto_correct_spellcheck_word(st)
            call check(error, buffer_string(), "say hello")
        end if

        last_command => null()
        call finish(h)
    end subroutine test_auto_correct

    ! ------------------------------------------------------------------
    ! The mode
    ! ------------------------------------------------------------------

    !> Turning the mode on installs its hooks; turning it off removes them
    !> and the highlighting.
    subroutine test_mode(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call fresh("flyspell-test", "a helo", h)
        call minor_modes%spellcheck_mode%enable(st)
        call check(error, st%ok(), "enable: "//detail(st))
        if (.not. allocated(error)) call check(error, minor_modes%spellcheck_mode%enabled(), "enabled")
        if (.not. allocated(error)) call check(error, post_command_hook%member(spellcheck_post_command_hook), "post hook")
        if (.not. allocated(error)) call check(error, pre_command_hook%member(spellcheck_pre_command_hook), "pre hook")

        if (allocated(error)) then
            call finish(h)
            return
        end if

        call spellcheck_word_highlight(st)
        call check(error, highlights() /= "", "highlighted")
        call minor_modes%spellcheck_mode%disable(st)
        if (.not. allocated(error)) call check(error, highlights(), "")
        if (.not. allocated(error)) call check(error,.not. post_command_hook%member(spellcheck_post_command_hook), &
                                               "post hook removed")
        call finish(h)
    end subroutine test_mode

    !> After a command that moved point off a word, the post-command hook
    !> checks the word it left.
    subroutine test_post_command(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call fresh("flyspell-test", "a helo is here", h)
        call minor_modes%spellcheck_mode%enable(st)
        call check(error, st%ok(), "enable: "//detail(st))

        if (allocated(error)) then
            call finish(h)
            return
        end if

        call goto_character(7_int64)
        call spellcheck_pre_command_hook(st)
        call goto_character(point_maximum())
        call spellcheck_post_command_hook(st)
        call check(error, st%ok(), "post: "//detail(st))
        if (.not. allocated(error)) call check(error, highlights(), "3-7:"//trim(spellcheck_incorrect%name)//" ")
        call finish(h)
    end subroutine test_post_command

    !> The mode line shows `spellcheck_mode_line_string' and the
    !> dictionary's first two characters ("--" for the default one).
    subroutine test_lighter(error)
        use display_engine_mod, only: mode_line_modes
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call fresh("flyspell-test", "", h)
        call minor_modes%spellcheck_mode%enable(st)
        call check(error, index(mode_line_modes(current), spellcheck_mode_line_string//"/--") > 0, &
                   "lighter: "//mode_line_modes(current))
        call finish(h)
    end subroutine test_lighter

    !> In programming modes only text in the string, comment and doc faces
    !> is checked.
    subroutine test_programming_text(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call fresh("flyspell-test", "code ""text""", h)
        call put_text_property(6_int64, 12_int64, value_type(name=P_FACE), value_type(name=STRING_FACE), st)
        call goto_character(3_int64)
        call check(error,.not. is_spellcheck_programming_text(), "code")

        if (.not. allocated(error)) then
            call goto_character(8_int64)
            call check(error, is_spellcheck_programming_text(), "string")
        end if

        call finish(h)
    end subroutine test_programming_text
end module test_spellcheck_mode_mod
