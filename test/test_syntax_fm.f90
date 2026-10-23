! test_syntax_fm.f90 -- tests for syntax_fm, characters_fm and the word
! commands.
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

!> Ports syntax-tests.el syntax-class-character-test (syntax-char-syntax
!> needs unibyte buffers; the rest need comments and sexps); own tests
!> for the standard table, characters.el's entries, word boundaries and
!> the word commands.
module test_syntax_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_fm, only: status_type
    use handle_fm, only: handle_type
    use lisp_fm, only: value_type
    use buffer_fm, only: buffers, cur, get_buffer_create, set_buffer, erase_buffer
    use insdel_fm, only: insert
    use editfns_fm, only: point, goto_char, point_min, point_max, buffer_string, mark_position
    use keymap_fm, only: key_parse
    use keyboard_fm, only: unread_command_events, discard_input, command_loop
    use bindings_fm, only: init_bindings
    use simple_fm, only: current_kill
    use syntax_fm, only: skip_chars_forward, skip_chars_backward, skip_syntax_forward, skip_syntax_backward
    use syntax_fm, only: syntax_descriptor_type, string_to_syntax, syntax_class_to_char, char_syntax, &
                         modify_syntax_entry, make_syntax_table, copy_syntax_table, free_syntax_table, &
                         set_syntax_table, standard_syntax_table, matching_paren, syntax_entry, scan_words, &
                         forward_word, word_boundary_p, SWORD, SOPEN
    use charscript_fm, only: char_script, script_names
    implicit none(type, external)
    private

    public :: collect_syntax_fm

contains

    subroutine collect_syntax_fm(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)
        testsuite = [ &
                    new_unittest("syntax-class-character-test", test_class_character), &
                    new_unittest("standard-syntax-table-ascii", test_standard_ascii), &
                    new_unittest("standard-syntax-table-characters-el", test_characters_el), &
                    new_unittest("string-to-syntax", test_string_to_syntax), &
                    new_unittest("syntax-table-inheritance", test_inheritance), &
                    new_unittest("char-script", test_char_script), &
                    new_unittest("word-boundaries", test_word_boundaries), &
                    new_unittest("forward-word", test_forward_word), &
                    new_unittest("word-keys", test_word_keys), &
                    new_unittest("skip-chars", test_skip_chars), &
                    new_unittest("skip-syntax", test_skip_syntax) &
                    ]
    end subroutine collect_syntax_fm

    !> A fresh buffer holding `text`, point at its start.
    subroutine temp_buffer(text)
        character(len=*), intent(in) :: text
        type(handle_type) :: buf
        type(status_type) :: st
        call init_bindings()
        call discard_input()
        call get_buffer_create(" *syntax-test*", buf, st)
        call set_buffer(buf, st)
        buffers(cur)%mark_active = .false.
        buffers(cur)%syntax_table = handle_type()
        call erase_buffer(st)
        call insert(text, st)
        call goto_char(point_min())
    end subroutine temp_buffer

    !> The syntax class of character `c` in table `t`.
    integer function class_in(c, t)
        integer, intent(in) :: c
        type(handle_type), intent(in) :: t
        type(syntax_descriptor_type) :: d
        d = syntax_entry(c, t)
        class_in = iand(d%code, 255)
    end function class_in

    !> The UTF-8 encoding of character `c`.
    function u(c) result(s)
        integer, intent(in) :: c
        character(len=:), allocatable :: s
        if (c < 128) then
            s = achar(c)
        else if (c < 2048) then
            s = achar(192 + c/64)//achar(128 + mod(c, 64))
        else if (c < 65536) then
            s = achar(224 + c/4096)//achar(128 + mod(c/64, 64))//achar(128 + mod(c, 64))
        else
            s = achar(240 + c/262144)//achar(128 + mod(c/4096, 64))//achar(128 + mod(c/64, 64))// &
                achar(128 + mod(c, 64))
        end if
    end function u

    subroutine test_class_character(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: chars = " .w_()'""$\/<>@!|"
        type(syntax_descriptor_type) :: d
        type(status_type) :: st
        character :: ch
        integer :: i
        do i = 0, len(chars) - 1
            call syntax_class_to_char(i, ch, st)
            call check(error, st%ok() .and. ch == chars(i + 1:i + 1), chars(i + 1:i + 1))
            if (allocated(error)) return
            call string_to_syntax(chars(i + 1:i + 1), d, st)
            ! "@" (inherit) gives nil.
            if (d%code >= 0) then
                call syntax_class_to_char(iand(d%code, 255), ch, st)
                call check(error, ch == chars(i + 1:i + 1), chars(i + 1:i + 1))
                if (allocated(error)) return
            end if
        end do
        call syntax_class_to_char(-1, ch, st)
        call check(error,.not. st%ok())
        if (allocated(error)) return
        call syntax_class_to_char(200, ch, st)
        call check(error,.not. st%ok())
    end subroutine test_class_character

    !> init_syntax_once's entries.
    subroutine test_standard_ascii(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: chars = "a Z09$%()[]{}""\_-+*/&|<>=.,;:?!#@~^'`"//achar(9)//achar(10)// &
                                       achar(0)//achar(127)
        character(len=*), parameter :: want = "w wwwww()()()""\__________............  .."
        integer :: i
        call temp_buffer("")
        do i = 1, len(chars)
            call check(error, char_syntax(iachar(chars(i:i))) == want(i:i), "syntax of "//chars(i:i)// &
                       " is "//char_syntax(iachar(chars(i:i))))
            if (allocated(error)) return
        end do
        call check(error, matching_paren(iachar("(")) == iachar(")") .and. matching_paren(iachar("a")) == -1)
    end subroutine test_standard_ascii

    !> A sample of the entries characters.el makes.
    subroutine test_characters_el(error)
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: cs(*) = [int(z'A1'), int(z'A9'), int(z'AB'), int(z'A0'), int(z'B5'), int(z'DF'), &
                                       int(z'E9'), int(z'F7'), int(z'2014'), int(z'2190'), int(z'3001'), &
                                       int(z'300C'), int(z'300D'), int(z'FF08'), int(z'FF62'), int(z'FF63'), &
                                       int(z'2160'), int(z'24D0'), int(z'2116'), int(z'4E00'), int(z'0F0B'), &
                                       int(z'03B1'), int(z'2039'), int(z'1F600')]
        character(len=*), parameter :: want = "._. www_._.()(()ww.w.w_w"
        integer :: i
        call temp_buffer("")
        do i = 1, size(cs)
            call check(error, char_syntax(cs(i)) == want(i:i), "syntax of "//u(cs(i))//" is "//char_syntax(cs(i)))
            if (allocated(error)) return
        end do
        ! 「 is an open paren matching 」.
        call check(error, matching_paren(int(z'300C')) == int(z'300D'))
    end subroutine test_characters_el

    subroutine test_string_to_syntax(error)
        type(error_type), allocatable, intent(out) :: error
        type(syntax_descriptor_type) :: d
        type(status_type) :: st
        call string_to_syntax("()", d, st)
        call check(error, st%ok() .and. d%code == SOPEN .and. d%match == iachar(")"))
        if (allocated(error)) return
        call string_to_syntax(". 124b", d, st)
        call check(error, d%code == 1 + 65536 + 131072 + 524288 + 2097152 .and. d%match == -1)
        if (allocated(error)) return
        call string_to_syntax("-", d, st)
        call check(error, d%code == 0)
        if (allocated(error)) return
        call string_to_syntax("("//u(int(z'300D')), d, st)
        call check(error, d%match == int(z'300D'))
        if (allocated(error)) return
        call string_to_syntax("x", d, st)
        call check(error,.not. st%ok())
    end subroutine test_string_to_syntax

    !> A table from make-syntax-table inherits; one from copy-syntax-table
    !> copies; set-syntax-table makes the buffer use it.
    subroutine test_inheritance(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: t1, t2
        type(status_type) :: st
        type(syntax_descriptor_type) :: d
        call temp_buffer("")
        call make_syntax_table(t1, st)
        call modify_syntax_entry(iachar("_"), iachar("_"), "w", st, t1)
        call check(error, st%ok() .and. class_in(iachar("_"), t1) == SWORD)
        if (allocated(error)) return
        ! Inherited from the standard table.
        d = syntax_entry(iachar("("), t1)
        call check(error, d%code == SOPEN .and. d%match == iachar(")"))
        if (allocated(error)) return
        call check(error, char_syntax(iachar("_")) == "_", "the buffer still uses the standard table")
        if (allocated(error)) return
        call set_syntax_table(t1, st)
        call check(error, char_syntax(iachar("_")) == "w")
        if (allocated(error)) return
        call copy_syntax_table(t2, st, t1)
        call modify_syntax_entry(int(z'4E00'), int(z'9FFF'), ".", st, t2)
        call check(error, class_in(int(z'4E01'), t2) == 1 .and. &
                   class_in(int(z'4E01'), t1) == SWORD .and. &
                   class_in(iachar("_"), t2) == SWORD, "the copy is separate")
        if (allocated(error)) return
        ! "@" makes an entry inherit again.
        call modify_syntax_entry(iachar("_"), iachar("_"), "@", st, t1)
        call check(error, char_syntax(iachar("_")) == "_")
        if (allocated(error)) return
        call free_syntax_table(t2, st)
        call check(error, st%ok())
        if (allocated(error)) return
        call free_syntax_table(standard_syntax_table(), st)
        call check(error,.not. st%ok(), "the standard table can't be freed")
        buffers(cur)%syntax_table = handle_type()
        call free_syntax_table(t1, st)
    end subroutine test_inheritance

    subroutine test_char_script(error)
        type(error_type), allocatable, intent(out) :: error
        call check(error, trim(script_names(char_script(iachar("a")))) == "latin" .and. &
                   trim(script_names(char_script(int(z'03B1')))) == "greek" .and. &
                   trim(script_names(char_script(int(z'4E00')))) == "han" .and. &
                   trim(script_names(char_script(int(z'3042')))) == "kana" .and. &
                   trim(script_names(char_script(int(z'AB65')))) == "greek" .and. &
                   trim(script_names(char_script(int(z'16FE1')))) == "nushu")
    end subroutine test_char_script

    subroutine test_word_boundaries(error)
        type(error_type), allocatable, intent(out) :: error
        ! Same script: no boundary.
        call check(error,.not. word_boundary_p(iachar("a"), iachar("b")))
        if (allocated(error)) return
        ! Latin then Greek: a boundary.
        call check(error, word_boundary_p(iachar("a"), int(z'03B1')))
        if (allocated(error)) return
        ! A combining mark joins whatever it follows.
        call check(error,.not. word_boundary_p(iachar("e"), int(z'0301')) .and. &
                   .not. word_boundary_p(int(z'0301'), iachar("x")))
        if (allocated(error)) return
        ! Kanji followed by hiragana or katakana: one word; not the reverse.
        call check(error,.not. word_boundary_p(int(z'6F22'), int(z'3042')) .and. &
                   .not. word_boundary_p(int(z'6F22'), int(z'30A2')) .and. &
                   word_boundary_p(int(z'3042'), int(z'6F22')))
        if (allocated(error)) return
        ! Hiragana then katakana (same script, kana): separated.
        call check(error, word_boundary_p(int(z'3042'), int(z'30A2')) .and. &
                   .not. word_boundary_p(int(z'30A2'), int(z'3042')))
    end subroutine test_word_boundaries

    subroutine test_forward_word(error)
        type(error_type), allocatable, intent(out) :: error
        logical :: ok
        call temp_buffer("Hello, world!  foo_bar "//u(int(z'03B1'))//u(int(z'03B2'))//"abc")
        call forward_word(1_int64, ok)
        call check(error, ok .and. point() == 6)
        if (allocated(error)) return
        call forward_word(1_int64)
        call check(error, point() == 13)
        if (allocated(error)) return
        ! _ is a symbol constituent: foo and bar are two words.
        call forward_word(1_int64)
        call check(error, point() == 19)
        if (allocated(error)) return
        call forward_word(2_int64)
        call check(error, point() == 26, "bar, then the Greek letters")
        if (allocated(error)) return
        call forward_word(1_int64)
        call check(error, point() == point_max())
        if (allocated(error)) return
        call forward_word(1_int64, ok)
        call check(error,.not. ok .and. point() == point_max(), "no more words")
        if (allocated(error)) return
        call forward_word(-2_int64)
        call check(error, point() == 24)
        if (allocated(error)) return
        call check(error, scan_words(1_int64, -1_int64) == 0 .and. scan_words(1_int64, 2_int64) == 13)
    end subroutine test_forward_word

    !> M-f, M-b, M-d, M-DEL, M-@, C-<right>, C-<left> through the command
    !> loop.
    subroutine test_word_keys(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        type(value_type), allocatable :: evs(:)
        character(len=:), allocatable :: killed
        call temp_buffer("one two three four")
        call key_parse("M-f M-f M-b", evs, st)
        call unread_command_events(evs)
        call command_loop(st)
        call check(error, point() == 5)
        if (allocated(error)) return
        call key_parse("C-<right> C-<left> C-<left>", evs, st)
        call unread_command_events(evs)
        call command_loop(st)
        call check(error, point() == 1)
        if (allocated(error)) return
        call key_parse("M-d", evs, st)
        call unread_command_events(evs)
        call command_loop(st)
        call check(error, buffer_string() == " two three four")
        if (allocated(error)) return
        call current_kill(0_int64, killed, st)
        call check(error, killed == "one", killed)
        if (allocated(error)) return
        call goto_char(point_max())
        call key_parse("M-DEL", evs, st)
        call unread_command_events(evs)
        call command_loop(st)
        call check(error, buffer_string() == " two three ", buffer_string())
        if (allocated(error)) return
        call goto_char(point_min())
        call key_parse("M-@ M-@", evs, st)
        call unread_command_events(evs)
        call command_loop(st)
        call check(error, point() == 1 .and. mark_position() == 11 .and. buffers(cur)%mark_active, &
                   "M-@ twice marks two words")
    end subroutine test_word_keys

    !> skip-chars-forward and -backward: ranges, negation, quoting,
    !> character classes, limits and the return value.
    subroutine test_skip_chars(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: moved
        call temp_buffer("abcXYZ-123 éü!")
        call goto_char(1_int64)
        moved = skip_chars_forward("a-z", st)
        call check(error, moved == 3 .and. point() == 4)
        if (allocated(error)) return
        moved = skip_chars_forward("^0-9", st)
        call check(error, moved == 4 .and. point() == 8)
        if (allocated(error)) return
        moved = skip_chars_backward("A-Z\-", st)
        call check(error, moved == -4 .and. point() == 4)
        if (allocated(error)) return
        call goto_char(8_int64)
        moved = skip_chars_forward("[:digit:][:space:]", st)
        call check(error, point() == 12)
        if (allocated(error)) return
        moved = skip_chars_forward("[:alpha:]", st)
        call check(error, point() == 14, "multibyte letters")
        if (allocated(error)) return
        call goto_char(1_int64)
        moved = skip_chars_forward("a-z", st, 3_int64)
        call check(error, point() == 3, "limit")
        if (allocated(error)) return
        ! `-' at the end is a literal; a reversed range is empty.
        call goto_char(7_int64)
        moved = skip_chars_forward("z-a-", st)
        call check(error, point() == 8)
        if (allocated(error)) return
        moved = skip_chars_forward("[:bogus:]", st)
        call check(error,.not. st%ok())
    end subroutine test_skip_chars

    subroutine test_skip_syntax(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int64) :: moved
        call temp_buffer("foo_bar  (baz)")
        call goto_char(1_int64)
        moved = skip_syntax_forward("w")
        call check(error, moved == 3 .and. point() == 4)
        if (allocated(error)) return
        moved = skip_syntax_forward("w_")
        call check(error, point() == 8)
        if (allocated(error)) return
        moved = skip_syntax_forward(" ")
        call check(error, point() == 10)
        if (allocated(error)) return
        moved = skip_syntax_forward("^)")
        call check(error, point() == 14)
        if (allocated(error)) return
        moved = skip_syntax_backward("^-")
        call check(error, moved == -4 .and. point() == 10)
    end subroutine test_skip_syntax
end module test_syntax_fm
