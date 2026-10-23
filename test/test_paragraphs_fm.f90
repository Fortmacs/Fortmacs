! test_paragraphs_fm.f90 -- tests for paragraphs_fm.
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

!> Ports the paragraphs-tests.el tests of the ported commands (the
!> mark-paragraph.bin resource inlined); the rest are own tests.
module test_paragraphs_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use test_support, only: show_alone
    use error_fm, only: status_type
    use handle_fm, only: handle_type
    use lisp_fm, only: value_type
    use buffer_fm, only: buffers, cur, get_buffer_create, set_buffer, erase_buffer
    use insdel_fm, only: insert
    use editfns_fm, only: point, goto_char, point_min, point_max, buffer_string, mark_position
    use keymap_fm, only: key_parse
    use keyboard_fm, only: unread_command_events, discard_input, command_loop
    use bindings_fm, only: init_bindings
    use paragraphs_fm, only: transpose_paragraphs, transpose_sentences
    use paragraphs_fm, only: forward_paragraph, backward_paragraph, mark_paragraph, kill_paragraph, &
                             backward_kill_paragraph, start_of_paragraph_text, end_of_paragraph_text, &
                             sentence_end_regexp, sentence_end, forward_sentence, backward_sentence, kill_sentence, &
                             backward_kill_sentence, mark_end_of_sentence, paragraph_start, paragraph_separate
    use keyboard_fm, only: last_command, this_command
    use lread_fm, only: sym
    implicit none(type, external)
    private

    public :: collect_paragraphs_fm

    character(len=*), parameter :: LF = achar(10)
    character(len=*), parameter :: AB = "AA"//LF//"AA"//LF//LF//"BB"//LF//"BB"//LF

contains

    subroutine collect_paragraphs_fm(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)
        testsuite = [ &
                    new_unittest("paragraphs-tests-forward-backward-paragraph", test_forward_backward), &
                    new_unittest("paragraphs-tests-mark-paragraph", test_mark), &
                    new_unittest("paragraphs-tests-kill-paragraph", test_kill), &
                    new_unittest("paragraphs-tests-backward-kill-paragraph", test_backward_kill), &
                    new_unittest("paragraphs-tests-transpose-paragraphs", test_transpose_paragraphs), &
                    new_unittest("paragraphs-tests-transpose-sentences", test_transpose_sentences), &
                    new_unittest("paragraphs-tests-start-of-paragraph-text", test_start_text), &
                    new_unittest("paragraphs-tests-end-of-paragraph-text", test_end_text), &
                    new_unittest("test-mark-paragraphs", test_mark_paragraphs), &
                    new_unittest("form-feed-and-indented-blank-lines", test_separators), &
                    new_unittest("paragraph-keys", test_keys), &
                    new_unittest("paragraphs-tests-sentence-end", test_sentence_end), &
                    new_unittest("paragraphs-tests-forward-sentence", test_forward_sentence), &
                    new_unittest("paragraphs-tests-backward-sentence", test_backward_sentence), &
                    new_unittest("paragraphs-tests-kill-sentence", test_kill_sentence), &
                    new_unittest("paragraphs-tests-backward-kill-sentence", test_backward_kill_sentence), &
                    new_unittest("paragraphs-tests-mark-end-of-sentence", test_mark_end_of_sentence), &
                    new_unittest("paragraph-start-variable", test_paragraph_start) &
                    ]
    end subroutine collect_paragraphs_fm

    !> A fresh buffer holding `text`, point at its start.
    subroutine temp_buffer(text)
        character(len=*), intent(in) :: text
        type(handle_type) :: buf
        type(status_type) :: st
        call init_bindings()
        call discard_input()
        call get_buffer_create(" *paragraphs-test*", buf, st)
        call set_buffer(buf, st)
        buffers(cur)%mark_active = .false.
        call erase_buffer(st)
        call insert(text, st)
        call goto_char(point_min())
        call show_alone()
    end subroutine temp_buffer

    subroutine test_forward_backward(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int64) :: left
        call temp_buffer(AB)
        left = forward_paragraph(1_int64)
        call check(error, point() == 7)
        if (allocated(error)) return
        left = forward_paragraph(1_int64)
        call check(error, point() == 14)
        if (allocated(error)) return
        call backward_paragraph(1_int64)
        call check(error, point() == 7)
        if (allocated(error)) return
        call backward_paragraph(1_int64)
        call check(error, point() == point_min())
    end subroutine test_forward_backward

    subroutine test_mark(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        call temp_buffer(AB)
        call mark_paragraph(1_int64, .false., st)
        call check(error, st%ok() .and. buffers(cur)%mark_active)
        if (allocated(error)) return
        call check(error, mark_position() == 7)
        if (allocated(error)) return
        call mark_paragraph(0_int64, .false., st)
        call check(error,.not. st%ok(), "can't mark zero paragraphs")
    end subroutine test_mark

    subroutine test_kill(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        call temp_buffer(AB)
        call kill_paragraph(1_int64, st)
        call check(error, buffer_string() == LF//"BB"//LF//"BB"//LF)
    end subroutine test_kill

    subroutine test_transpose_paragraphs(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        call temp_buffer(AB)
        call goto_char(1_int64)
        call transpose_paragraphs(1_int64, st)
        call check(error, buffer_string() == LF//"BB"//LF//"BB"//LF//"AA"//LF//"AA"//LF, buffer_string())
    end subroutine test_transpose_paragraphs

    subroutine test_transpose_sentences(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        call temp_buffer("First sentence.  Second sentence.  Third sentence.")
        call goto_char(1_int64)
        call transpose_sentences(1_int64, st)
        call check(error, buffer_string() == "Second sentence.  First sentence.  Third sentence.", buffer_string())
        if (allocated(error)) return
        call goto_char(1_int64)
        call transpose_sentences(2_int64, st)
        call check(error, buffer_string() == "First sentence.  Third sentence.  Second sentence.", buffer_string())
    end subroutine test_transpose_sentences

    subroutine test_backward_kill(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        call temp_buffer(AB)
        call goto_char(7_int64)
        call backward_kill_paragraph(1_int64, st)
        call check(error, buffer_string() == LF//"BB"//LF//"BB"//LF)
    end subroutine test_backward_kill

    subroutine test_start_text(error)
        type(error_type), allocatable, intent(out) :: error
        call temp_buffer(AB)
        call goto_char(point_max())
        call start_of_paragraph_text()
        call check(error, point() == 8)
    end subroutine test_start_text

    subroutine test_end_text(error)
        type(error_type), allocatable, intent(out) :: error
        call temp_buffer(AB)
        call end_of_paragraph_text()
        call check(error, point() == 6)
    end subroutine test_end_text

    !> paragraphs-resources/mark-paragraph.bin, inlined.
    subroutine test_mark_paragraphs(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        call temp_buffer("First"//LF//"paragraph"//LF//LF//"Second"//LF//LF//"Third"//LF//"paragraph"//LF//LF// &
                         "No line end")
        call goto_char(point_max())
        ! Just a sanity check that the text hasn't changed.
        call check(error, point() == 54)
        if (allocated(error)) return
        call mark_paragraph(1_int64, .false., st)
        call check(error, point() == 42 .and. mark_position() == 54)
        if (allocated(error)) return
        ! Doesn't move.
        call mark_paragraph(1_int64, .false., st)
        call check(error, point() == 42 .and. mark_position() == 54)
        if (allocated(error)) return
        ! (forward-line -1)
        call goto_char(32_int64)
        call mark_paragraph(1_int64, .false., st)
        call check(error, point() == 25 .and. mark_position() == 42)
        if (allocated(error)) return
        call goto_char(point_min())
        call mark_paragraph(1_int64, .false., st)
        call check(error, point() == 1 .and. mark_position() == 17)
    end subroutine test_mark_paragraphs

    !> A form feed starts a paragraph; a line of blanks separates them.
    subroutine test_separators(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int64) :: left
        call temp_buffer("a"//LF//"b"//LF//achar(12)//"c"//LF//"d"//LF//" "//achar(9)//LF//"e"//LF)
        left = forward_paragraph(1_int64)
        call check(error, point() == 5 .and. left == 0, "form feed line starts a paragraph")
        if (allocated(error)) return
        left = forward_paragraph(1_int64)
        call check(error, point() == 10, "blank line separates")
        if (allocated(error)) return
        left = forward_paragraph(2_int64)
        call check(error, point() == point_max() .and. left == 1, "one paragraph left over")
        if (allocated(error)) return
        ! The blank line before is not just margin (it has a tab), so the
        ! paragraph starts after it.
        call backward_paragraph(1_int64)
        call check(error, point() == 13, "back over the blank line")
    end subroutine test_separators

    !> M-}, M-{, M-h, C-<down>, C-<up> through the command loop.
    subroutine test_keys(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        type(value_type), allocatable :: evs(:)
        call temp_buffer(AB)
        call key_parse("M-} M-}", evs, st)
        call unread_command_events(evs)
        call command_loop(st)
        call check(error, point() == 14, "M-} twice")
        if (allocated(error)) return
        call key_parse("M-{ C-<up>", evs, st)
        call unread_command_events(evs)
        call command_loop(st)
        call check(error, point() == 1, "M-{ then C-<up>")
        if (allocated(error)) return
        call key_parse("C-<down> M-h", evs, st)
        call unread_command_events(evs)
        call command_loop(st)
        call check(error, point() == 7 .and. mark_position() == 14, "M-h marks the next paragraph")
    end subroutine test_keys
    subroutine test_sentence_end(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, len(sentence_end_regexp()) > 0)
        if (allocated(error)) return
        sentence_end = "override works"
        call check(error, sentence_end_regexp() == "override works")
        deallocate (sentence_end%value)
    end subroutine test_sentence_end

    subroutine test_forward_sentence(error)
        type(error_type), allocatable, intent(out) :: error
        call temp_buffer("First sentence.  Second sentence.")
        call forward_sentence(1_int64)
        call check(error, point() == 16, "one")
        if (allocated(error)) return
        call goto_char(point_min())
        call forward_sentence(2_int64)
        call check(error, point() == 34, "two")
    end subroutine test_forward_sentence

    subroutine test_backward_sentence(error)
        type(error_type), allocatable, intent(out) :: error
        call temp_buffer("First sentence.  Second sentence.")
        call goto_char(point_max())
        call backward_sentence(1_int64)
        call check(error, point() == 18)
    end subroutine test_backward_sentence

    subroutine test_kill_sentence(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        call temp_buffer("First sentence.  Second sentence.")
        call kill_sentence(1_int64, st)
        call check(error, buffer_string() == "  Second sentence.")
    end subroutine test_kill_sentence

    subroutine test_backward_kill_sentence(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        call temp_buffer("Should not be killed.  Should be killed.")
        call goto_char(point_max())
        call backward_kill_sentence(1_int64, st)
        call check(error, buffer_string() == "Should not be killed.  ")
    end subroutine test_backward_kill_sentence

    subroutine test_mark_end_of_sentence(error)
        type(error_type), allocatable, intent(out) :: error
        call temp_buffer("Example sentence.  Followed by another one.")
        call mark_end_of_sentence(1_int64)
        call check(error, buffers(cur)%mark_active .and. mark_position() == 18, "one")
        if (allocated(error)) return
        call temp_buffer("Example sentence.  Followed by another one.")
        call mark_end_of_sentence(2_int64)
        call check(error, buffers(cur)%mark_active .and. mark_position() == 44, "two")
        if (allocated(error)) return
        ! Repeating the command marks the next sentence.
        call temp_buffer("Example sentence.  Followed by another one.")
        call mark_end_of_sentence(1_int64)
        last_command = sym("mark_end_of_sentence")
        this_command = sym("mark_end_of_sentence")
        call mark_end_of_sentence(1_int64)
        call check(error, buffers(cur)%mark_active .and. mark_position() == 44, "repeated")
    end subroutine test_mark_end_of_sentence

    !> paragraph-start and paragraph-separate are regexps.
    subroutine test_paragraph_start(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: saved_start, saved_separate
        integer(int64) :: left
        saved_start = paragraph_start%value
        saved_separate = paragraph_separate%value
        ! Lines starting with "* " start paragraphs; "---" lines separate.
        paragraph_start = "\* \|---$\|[ "//achar(9)//"]*$"
        paragraph_separate = "---$\|[ "//achar(9)//"]*$"
        call temp_buffer("* one"//LF//"text"//LF//"* two"//LF//"---"//LF//"three"//LF)
        body: block
            left = forward_paragraph(1_int64)
            call check(error, point() == 12, "item")
            if (allocated(error)) exit body
            left = forward_paragraph(1_int64)
            call check(error, point() == 18, "before separator")
            if (allocated(error)) exit body
            left = forward_paragraph(1_int64)
            call check(error, point() == point_max(), "last")
            if (allocated(error)) exit body
            call backward_paragraph(1_int64)
            call check(error, point() == 22, "back")
        end block body
        paragraph_start = saved_start
        paragraph_separate = saved_separate
    end subroutine test_paragraph_start
end module test_paragraphs_fm
