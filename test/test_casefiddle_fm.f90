! test_casefiddle_fm.f90 -- tests for case conversion (casefiddle_fm).
!
! Copyright (C) 2016-2026 Free Software Foundation, Inc.
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

!> Ports test/src/casefiddle-tests.el: casefiddle-tests-casing-character,
!> -casing-word, -casing, -casing-byte8, -char-casing and
!> casefiddle-case-symbols-as-words.  (The char-property and case-table
!> tests need `get-char-code-property` and case-table objects; the
!> byte8-with-changes test needs `set-case-syntax-pair`; the
!> region-extract-function test needs noncontiguous regions; the Turkish
!> test needs locales.)
module test_casefiddle_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_fm, only: status_type
    use handle_fm, only: handle_type
    use buffer_fm, only: get_buffer_create, set_buffer, kill_buffer, erase_buffer
    use insdel_fm, only: insert
    use editfns_fm, only: goto_char, point, point_min, point_max, buffer_string
    use character_fm, only: string_char
    use casefiddle_fm, only: upcase_string, downcase_string, capitalize_string, upcase_initials_string, &
                             upcase_char, downcase_char, capitalize_char, upcase_initials_char, upcase_region, &
                             downcase_region, capitalize_region, upcase_initials_region, upcase_word, &
                             downcase_word, capitalize_word, case_symbols_as_words
    implicit none(type, external)
    private

    public :: collect_casefiddle_fm

contains

    subroutine collect_casefiddle_fm(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)
        testsuite = [ &
                    new_unittest("casefiddle-tests-casing-character", test_casing_character), &
                    new_unittest("casefiddle-tests-casing-word", test_casing_word), &
                    new_unittest("casefiddle-tests-casing", test_casing), &
                    new_unittest("casefiddle-tests-casing-byte8", test_casing_byte8), &
                    new_unittest("casefiddle-tests-char-casing", test_char_casing), &
                    new_unittest("casefiddle-case-symbols-as-words", test_symbols_as_words) &
                    ]
    end subroutine collect_casefiddle_fm

    !> The character of the one-character UTF-8 string S.
    integer(int32) function ch(s)
        character(len=*), intent(in) :: s
        ch = string_char(s, 1_int64)
    end function ch

    !> The multibyte form of raw byte B.
    function raw(b) result(s)
        integer, intent(in) :: b
        character(len=2) :: s
        s = achar(192 + iand(ishft(b, -6), 1))//achar(128 + iand(b, 63))
    end function raw

    subroutine temp_buffer(h)
        type(handle_type), intent(out) :: h
        type(status_type) :: st
        call get_buffer_create(" *casefiddle-test*", h, st)
        call set_buffer(h, st)
        call erase_buffer(st)
    end subroutine temp_buffer

    subroutine test_casing_character(error)
        type(error_type), allocatable, intent(out) :: error
        ! character  uppercase  lowercase  titlecase
        character(len=8), parameter :: tests(4, 12) = reshape([character(len=8) :: &
                                                               "A", "A", "a", "A", "a", "A", "a", "A", &
                                                               "Ł", "Ł", "ł", "Ł", "ł", "Ł", "ł", "Ł", &
                                                               "Ǆ", "Ǆ", "ǆ", "ǅ", "ǅ", "Ǆ", "ǆ", "ǅ", &
                                                               "ǆ", "Ǆ", "ǆ", "ǅ", "Σ", "Σ", "σ", "Σ", &
                                                               "σ", "Σ", "σ", "Σ", "ς", "Σ", "ς", "Σ", &
                                                               "Ⅷ", "Ⅷ", "ⅷ", "Ⅷ", "ⅷ", "Ⅷ", "ⅷ", "Ⅷ"], [4, 12])
        integer :: k
        integer(int32) :: c
        do k = 1, 12
            c = ch(trim(tests(1, k)))
            call check(error, upcase_char(c) == ch(trim(tests(2, k))), trim(tests(1, k))//" upcase")
            if (allocated(error)) return
            call check(error, downcase_char(c) == ch(trim(tests(3, k))), trim(tests(1, k))//" downcase")
            if (allocated(error)) return
            call check(error, capitalize_char(c) == ch(trim(tests(4, k))), trim(tests(1, k))//" capitalize")
            if (allocated(error)) return
        end do
    end subroutine test_casing_character

    subroutine test_casing_word(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        character(len=7), parameter :: want(3) = ["FOO Bar", "foo Bar", "Foo Bar"]
        integer :: k, back
        call temp_buffer(h)
        do k = 1, 3
            do back = 0, 1
                call erase_buffer(st)
                call insert("foO Bar", st)
                call goto_char(point_min() + 4*back)
                select case (k)
                case (1)
                    call upcase_word(merge(-1_int64, 1_int64, back == 1), st)
                case (2)
                    call downcase_word(merge(-1_int64, 1_int64, back == 1), st)
                case default
                    call capitalize_word(merge(-1_int64, 1_int64, back == 1), st)
                end select
                call check(error, buffer_string() == want(k), want(k)//" got "//buffer_string())
                if (allocated(error)) exit
                call check(error, point() == point_min() + merge(4, 3, back == 1), "point")
                if (allocated(error)) exit
            end do
            if (allocated(error)) exit
        end do
        call kill_buffer(h, st)
    end subroutine test_casing_word

    !> Check INPUT cased by the string and region functions against
    !> EXPECTED (upper, lower, capitalize, up-initials).  Port of
    !> casefiddle-tests--test-casing.
    subroutine check_casing(error, input, expected)
        type(error_type), allocatable, intent(inout) :: error
        character(len=*), intent(in) :: input
        character(len=*), intent(in) :: expected(4)
        character(len=:), allocatable :: got
        type(status_type) :: st
        type(handle_type) :: h
        integer :: k
        do k = 1, 4
            select case (k)
            case (1)
                got = upcase_string(input)
            case (2)
                got = downcase_string(input)
            case (3)
                got = capitalize_string(input)
            case default
                got = upcase_initials_string(input)
            end select
            call check(error, got == trim(expected(k)), input//" string "//char(48 + k)//": "//got)
            if (allocated(error)) return
            call temp_buffer(h)
            call insert(input, st)
            select case (k)
            case (1)
                call upcase_region(point_min(), point_max(), st)
            case (2)
                call downcase_region(point_min(), point_max(), st)
            case (3)
                call capitalize_region(point_min(), point_max(), st)
            case default
                call upcase_initials_region(point_min(), point_max(), st)
            end select
            got = buffer_string()
            call kill_buffer(h, st)
            call check(error, got == trim(expected(k)), input//" region "//char(48 + k)//": "//got)
            if (allocated(error)) return
        end do
    end subroutine check_casing

    subroutine test_casing(error)
        type(error_type), allocatable, intent(out) :: error
        ! input     upper     lower    capitalize up-initials
        call check_casing(error, "Foo baR", [character(len=20) :: "FOO BAR", "foo bar", "Foo Bar", "Foo BaR"])
        if (allocated(error)) return
        call check_casing(error, "Ⅷ ⅷ", [character(len=20) :: "Ⅷ Ⅷ", "ⅷ ⅷ", "Ⅷ Ⅷ", "Ⅷ Ⅷ"])
        if (allocated(error)) return
        ! "ǅUNGLA" is an unfortunate result but it's really best we can do
        ! while still being consistent.
        call check_casing(error, "ǄUNGLA", [character(len=20) :: "ǄUNGLA", "ǆungla", "ǅungla", "ǅUNGLA"])
        if (allocated(error)) return
        call check_casing(error, "ǅungla", [character(len=20) :: "ǄUNGLA", "ǆungla", "ǅungla", "ǅungla"])
        if (allocated(error)) return
        call check_casing(error, "ǆungla", [character(len=20) :: "ǄUNGLA", "ǆungla", "ǅungla", "ǅungla"])
        if (allocated(error)) return
        call check_casing(error, "deﬁne", [character(len=20) :: "DEFINE", "deﬁne", "Deﬁne", "Deﬁne"])
        if (allocated(error)) return
        call check_casing(error, "ﬁsh", [character(len=20) :: "FISH", "ﬁsh", "Fish", "Fish"])
        if (allocated(error)) return
        call check_casing(error, "Straße", [character(len=20) :: "STRASSE", "straße", "Straße", "Straße"])
        if (allocated(error)) return
        ! The word repeated twice to test behavior at the end of a word
        ! inside of an input string as well as at the end of the string.
        call check_casing(error, "ΌΣΟΣ ΌΣΟΣ", &
                          [character(len=20) :: "ΌΣΟΣ ΌΣΟΣ", "όσος όσος", &
                           "Όσος Όσος", "ΌΣΟΣ ΌΣΟΣ"])
        if (allocated(error)) return
        ! What should be done with sole sigma?  It is 'final' but on the
        ! other hand it does not form a word.  We're using regular sigma.
        call check_casing(error, "Σ Σ", [character(len=20) :: "Σ Σ", "σ σ", "Σ Σ", "Σ Σ"])
        if (allocated(error)) return
        call check_casing(error, "όσος", [character(len=20) :: "ΌΣΟΣ", "όσος", "Όσος", "Όσος"])
        if (allocated(error)) return
        ! If sigma is already lower case, we don't want to change it.
        call check_casing(error, "όσοσ", [character(len=20) :: "ΌΣΟΣ", "όσοσ", "Όσοσ", "Όσοσ"])
    end subroutine test_casing

    subroutine test_casing_byte8(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=2) :: ff
        character(len=40) :: e(4)
        ff = raw(255)
        e(1) = ff//" FOO BAR "//ff
        e(2) = ff//" foo bar "//ff
        e(3) = ff//" Foo Bar "//ff
        e(4) = ff//" Foo BaR "//ff
        call check_casing(error, ff//" Foo baR "//ff, e)
        if (allocated(error)) return
        e(1) = ff//" ZAŻÓŁĆ GĘŚLĄ "//ff
        e(2) = ff//" zażółć gęślą "//ff
        e(3) = ff//" Zażółć Gęślą "//ff
        e(4) = ff//" Zażółć GĘŚlą "//ff
        call check_casing(error, ff//" Zażółć gĘŚlą "//ff, e)
    end subroutine test_casing_byte8

    subroutine test_char_casing(error)
        type(error_type), allocatable, intent(out) :: error
        ! input upcase downcase titlecase; we char-upcase ß to ẞ (bug
        ! #11309).
        character(len=4), parameter :: tests(4, 12) = reshape([character(len=4) :: &
                                                               "a", "A", "a", "A", "A", "A", "a", "A", &
                                                               "ł", "Ł", "ł", "Ł", "Ł", "Ł", "ł", "Ł", &
                                                               "ß", "ẞ", "ß", "ẞ", "ẞ", "ẞ", "ß", "ẞ", &
                                                               "ⅷ", "Ⅷ", "ⅷ", "Ⅷ", "Ⅷ", "Ⅷ", "ⅷ", "Ⅷ", &
                                                               "Ǆ", "Ǆ", "ǆ", "ǅ", "ǅ", "Ǆ", "ǆ", "ǅ", &
                                                               "ǆ", "Ǆ", "ǆ", "ǅ", "A", "A", "a", "A"], [4, 12])
        integer :: k
        integer(int32) :: c, tc
        do k = 1, 12
            c = ch(trim(tests(1, k)))
            tc = ch(trim(tests(4, k)))
            call check(error, upcase_char(c) == ch(trim(tests(2, k))), trim(tests(1, k))//" upcase")
            if (allocated(error)) return
            call check(error, downcase_char(c) == ch(trim(tests(3, k))), trim(tests(1, k))//" downcase")
            if (allocated(error)) return
            call check(error, capitalize_char(c) == tc, trim(tests(1, k))//" capitalize")
            if (allocated(error)) return
            call check(error, upcase_initials_char(c) == tc, trim(tests(1, k))//" upcase-initials")
            if (allocated(error)) return
        end do
    end subroutine test_char_casing

    subroutine test_symbols_as_words(error)
        type(error_type), allocatable, intent(out) :: error
        call syms("Aa_bb Cc_dd", "Aa_Bb Cc_Dd", "Aa_bb Cc_dd")
        call syms("Aa_bb cc_DD", "Aa_Bb Cc_DD", "Aa_bb Cc_DD")
        call syms("aa_bb cc_dd", "Aa_Bb Cc_Dd", "Aa_bb Cc_dd")
        call syms("Aa_Bb Cc_Dd", "Aa_Bb Cc_Dd", "Aa_Bb Cc_Dd")
        case_symbols_as_words = .false.
    contains

        subroutine syms(init, with_words, with_symbols)
            character(len=*), intent(in) :: init, with_words, with_symbols
            if (allocated(error)) return
            case_symbols_as_words = .false.
            call check(error, upcase_initials_string(init) == with_words, init//" words")
            if (allocated(error)) return
            case_symbols_as_words = .true.
            call check(error, upcase_initials_string(init) == with_symbols, init//" symbols")
        end subroutine syms
    end subroutine test_symbols_as_words
end module test_casefiddle_fm
