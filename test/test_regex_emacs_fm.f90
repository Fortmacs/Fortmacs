! test_regex_emacs_fm.f90 -- tests for regular expressions
! (regex_emacs_fm, and the regexp half of search_fm).
! Ports GNU Emacs 31.1 test/src/regex-emacs-tests.el in part; the others
! are not ported yet.
!
! Copyright (C) 2015-2026 Free Software Foundation, Inc.
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

!> Ports of test/src/regex-emacs-tests.el: regex-word-cc-fallback-test,
!> the character class tests (string-match part; skip-chars-forward is
!> not ported), regex-repeat-limit, regexp-multibyte-multibyte (strings
!> are always multibyte here), regexp-case-fold, regexp-eszett (the
!> matching part), regexp-atomic-failure,
!> regexp-tests-backtrack-optimization,
!> regexp-tests-zero-width-assertion-repetition and
!> regex-tests-mutual-exclusive-inf-rec, and regex-tests-PTESTS,
!> regex-tests-TESTS, regex-tests-BOOST and regex-tests-PCRE with copies
!> of their data files (in test/regex-resources).  String indices are 1-based here (upstream's
!> plus one).  Own tests for the search functions.
module test_regex_emacs_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use error_fm, only: status_type, format_error, FM_INVALID_REGEXP, FM_SEARCH_FAILED
    use handle_fm, only: handle_type
    use buffer_fm, only: get_or_make_buffer, set_buffer, close_buffer, erase_buffer
    use insdel_fm, only: insert
    use editfns_fm, only: goto_char, point, point_max, point_min, buffer_substring, buffer_string, delete_region
    use search_fm, only: string_index_to_offset
    use subr_fm, only: match_string, string_matches, is_looking_at, looking_back
    use syntax_fm, only: modify_syntax_entry, make_syntax_table, set_syntax_table, destroy_syntax_table
    use search_fm, only: string_match, posix_string_match, looking_at, re_search_forward, re_search_backward, &
                         match_beginning, match_end, match_data, set_match_data, regexp_quote, case_fold_search, &
                         search_spaces_regexp, posix_looking_at, SEARCH_NOERROR
    implicit none(type, external)
    private

    public :: collect_regex_emacs_fm

    !> Regex that matches an even number of \\ characters
    !> (regex-tests-re-even-escapes).
    character(len=*), parameter :: re_even_escapes = "\(?:^\|[^\]\)\(?:\\\\\)*"

contains

    !> Collect all exported unit tests.
    subroutine collect_regex_emacs_fm(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("regex-word-cc-fallback-test", test_word_cc_fallback), &
                    new_unittest("regex-tests-character-classes", test_character_classes), &
                    new_unittest("regex-repeat-limit", test_repeat_limit), &
                    new_unittest("regexp-multibyte-multibyte", test_multibyte), &
                    new_unittest("regexp-case-fold", test_case_fold), &
                    new_unittest("regexp-eszett", test_eszett), &
                    new_unittest("regexp-atomic-failure", test_atomic_failure), &
                    new_unittest("regexp-tests-backtrack-optimization", test_backtrack_optimization), &
                    new_unittest("regexp-tests-zero-width-assertion-repetition", test_zero_width), &
                    new_unittest("regex-tests-mutual-exclusive-inf-rec", test_mutual_exclusive), &
                    new_unittest("regex-tests-PTESTS", test_ptests), &
                    new_unittest("regex-tests-TESTS", test_tests), &
                    new_unittest("regex-tests-BOOST", test_boost), &
                    new_unittest("regex-tests-PCRE", test_pcre), &
                    new_unittest("regexp-syntax", test_syntax), &
                    new_unittest("regexp-groups-and-backrefs", test_groups), &
                    new_unittest("regexp-intervals", test_intervals), &
                    new_unittest("regexp-invalid", test_invalid), &
                    new_unittest("regexp-categories-latin-linebreak", test_categories), &
                    new_unittest("re-search-forward-backward", test_regexp_search), &
                    new_unittest("looking-at", test_looking_at), &
                    new_unittest("match-data", test_match_data), &
                    new_unittest("regexp-quote", test_regexp_quote), &
                    new_unittest("search-spaces-regexp", test_spaces_regexp), &
                    new_unittest("posix-string-match", test_posix), &
                    new_unittest("match-string-and-friends", test_match_string), &
                    new_unittest("looking-back", test_looking_back) &
                    ]
    end subroutine collect_regex_emacs_fm

    !> `string-match`, failing the test on an error; 0 for nil.
    integer(int64) function sm(regexp, string)
        character(len=*), intent(in) :: regexp, string
        type(status_type) :: st

        sm = string_match(regexp, string, st)
        if (.not. st%ok()) sm = -99
    end function sm

    !> (match-end N).
    integer(int64) function me(n)
        integer, intent(in) :: n
        type(status_type) :: st

        me = match_end(n, st)
    end function me

    !> (match-beginning N).
    integer(int64) function mb(n)
        integer, intent(in) :: n
        type(status_type) :: st

        mb = match_beginning(n, st)
    end function mb

    !> The multibyte form of raw byte B (string-to-multibyte "\xB").
    function raw(b) result(s)
        integer, intent(in) :: b
        character(len=2) :: s

        s = achar(192 + iand(ishft(b, -6), 1))//achar(128 + iand(b, 63))
    end function raw

    subroutine enter(text, h)
        character(len=*), intent(in) :: text
        type(handle_type), intent(out) :: h
        type(status_type) :: st

        call get_or_make_buffer("regex-test", h, st)
        call set_buffer(h, st)
        call insert(text, st)
        call goto_char(1_int64)
    end subroutine enter

    subroutine leave(h)
        type(handle_type), intent(in) :: h
        type(status_type) :: st

        case_fold_search = .true.
        if (allocated(search_spaces_regexp)) deallocate (search_spaces_regexp)
        call close_buffer(h, st)
    end subroutine leave

    !> Test that "[[:cc:]]*x" matches "x" (bug#24020).
    subroutine test_word_cc_fallback(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=10), parameter :: classes(2) = [character(len=10) :: "[[:word:]]", "\sw"]
        character(len=1), parameter :: repeats(2) = ["*", "+"]
        character(len=3), parameter :: suffixes(4) = [character(len=3) :: "", "b", "bar", "☠"]
        character(len=3), parameter :: strings(2) = [character(len=3) :: "", "foo"]
        integer :: i, j, k, l

        do i = 1, 2
            do j = 1, 2
                do k = 1, 4
                    do l = 1, 2
                        if (repeats(j) == "+" .and. strings(l) == "") cycle
                        call check(error, sm("^"//trim(classes(i))//repeats(j)//trim(suffixes(k))//"$", &
                                             trim(strings(l))//trim(suffixes(k))) > 0, &
                                   trim(classes(i))//repeats(j)//trim(suffixes(k)))
                        if (allocated(error)) return
                    end do
                end do
            end do
        end do
    end subroutine test_word_cc_fallback

    !> Port of the regex-tests-*-character-class tests (regex--test-cc,
    !> string-match-p part).
    subroutine test_character_classes(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: ws = " "//achar(9)//achar(10)

        case_fold_search = .false.
        call cc("alnum", "abcABC012łąka", "-, "//ws)
        if (allocated(error)) return
        call cc("alpha", "abcABCłąka", "-,012 "//ws)
        if (allocated(error)) return
        call cc("digit", "012", "abcABCłąka-, "//ws)
        if (allocated(error)) return
        call cc("xdigit", "0123aBc", "łąk-, "//ws)
        if (allocated(error)) return
        call cc("upper", "ABCŁĄKA", "abc012-, "//ws)
        if (allocated(error)) return
        call cc("lower", "abcłąka", "ABC012-, "//ws)
        if (allocated(error)) return
        call cc("word", "abcABC012☠", "-, "//ws)
        if (allocated(error)) return
        call cc("punct", ".,-", "abcABC012☠ "//ws)
        if (allocated(error)) return
        call cc("cntrl", achar(1)//achar(2)//achar(9)//achar(10), ".,-abcABC012☠ ")
        if (allocated(error)) return
        call cc("graph", "abcłąka☠-,", " "//achar(9)//achar(10)//achar(1))
        if (allocated(error)) return
        call cc("print", "abcłąka☠-, ", achar(9)//achar(10)//achar(1))
        if (allocated(error)) return
        call cc("space", ws//" ", "abcABCł0123")
        if (allocated(error)) return
        call cc("blank", " "//achar(9)//" ", achar(10))
        if (allocated(error)) return
        call cc("ascii", "abcABC012 "//achar(9)//achar(10)//achar(1), "łą☠")
        if (allocated(error)) return
        call cc("nonascii", "łą☢", "abcABC012 "//achar(9)//achar(10)//achar(1))
        if (allocated(error)) return
        call cc("unibyte", "abcABC012 "//achar(9)//achar(10)//achar(1), "łą☢")
        if (allocated(error)) return
        call cc("multibyte", "łą☢", "abcABC012 "//achar(9)//achar(10)//achar(1))
        case_fold_search = .true.

    contains

        subroutine cc(name, matching, not_matching)
            character(len=*), intent(in) :: name, matching, not_matching

            call check(error, sm("^[[:"//name//":]]*$", matching) > 0, name//" matching")
            if (allocated(error)) return
            call check(error, sm("^[[:"//name//":]]*?☢$", matching//"☢") > 0, name//" lazy")
            if (allocated(error)) return
            call check(error, sm("^[^[:"//name//":]]*$", not_matching) > 0, name//" not matching")
            if (allocated(error)) return
            call check(error, sm("^[^[:"//name//":]]*☢$", not_matching//"☢") > 0, name//" not, then ☢")
        end subroutine cc
    end subroutine test_character_classes

    !> Test the #xFFFF repeat limit.
    subroutine test_repeat_limit(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st
        integer(int64) :: i

        call check(error, sm("\`x\{65535\}", repeat("x", 65535)) == 1)
        if (allocated(error)) return
        call check(error, sm("\`x\{65535\}", repeat("x", 65534)) == 0)
        if (allocated(error)) return
        i = string_match("\`x\{65536\}", "X", st)
        call check(error, st%code == FM_INVALID_REGEXP, format_error(st))
    end subroutine test_repeat_limit

    !> Test matching a multibyte regexp against a multibyte string.
    subroutine test_multibyte(error)
        type(error_type), allocatable, intent(out) :: error

        ! ASCII
        call check(error, sm("a[b]", "ab") > 0, "ascii")
        if (allocated(error)) return
        ! Unicode
        call check(error, sm("å[ü]z", "åüz") > 0, "unicode")
        if (allocated(error)) return
        call check(error, sm("ü", raw(195)//raw(188)) == 0, "raw ü")
        if (allocated(error)) return
        ! Raw
        call check(error, sm(raw(241), raw(241)) > 0, "raw")
        if (allocated(error)) return
        call check(error, sm(raw(241), raw(193)//raw(177)) == 0, "raw 2")
        if (allocated(error)) return
        call check(error, sm(raw(193)//raw(177), raw(241)) == 0, "raw 3")
        if (allocated(error)) return
        ! Raw, char alt
        call check(error, sm("["//raw(241)//"]", raw(241)) > 0, "raw alt")
        if (allocated(error)) return
        ! Raw range
        call check(error, sm("["//raw(130)//"-"//raw(211)//"]", raw(187)) > 0, "raw range")
        if (allocated(error)) return
        call check(error, sm("["//raw(130)//"-"//raw(211)//"]", "a") == 0, "raw range a")
        if (allocated(error)) return
        call check(error, sm("["//raw(130)//"-"//raw(211)//"]", "Å") == 0, "raw range Å")
        if (allocated(error)) return
        call check(error, sm("["//raw(130)//"-"//raw(211)//"]", "ü") == 0, "raw range ü")
        if (allocated(error)) return
        call check(error, sm("["//raw(130)//"-"//raw(211)//"]", raw(129)) == 0, "raw range 81")
        if (allocated(error)) return
        call check(error, sm("["//raw(130)//"-"//raw(211)//"]", raw(212)) == 0, "raw range d4")
        if (allocated(error)) return
        ! ASCII-raw range: should exclude U+0100..U+10FFFF
        call check(error, sm("[f-"//raw(211)//"]", "q") > 0, "ascii-raw q")
        if (allocated(error)) return
        call check(error, sm("[f-"//raw(211)//"]", raw(187)) > 0, "ascii-raw bb")
        if (allocated(error)) return
        call check(error, sm("[f-"//raw(211)//"]", "e") == 0, "ascii-raw e")
        if (allocated(error)) return
        call check(error, sm("[f-"//raw(211)//"]", "Å") == 0, "ascii-raw Å")
        if (allocated(error)) return
        call check(error, sm("[f-"//raw(211)//"]", "ü") == 0, "ascii-raw ü")
        if (allocated(error)) return
        call check(error, sm("[f-"//raw(211)//"]", raw(212)) == 0, "ascii-raw d4")
        if (allocated(error)) return
        ! Unicode-raw range: should be empty
        call check(error, sm("[å-"//raw(211)//"]", "å") == 0, "unicode-raw å")
        if (allocated(error)) return
        call check(error, sm("[å-"//raw(211)//"]", raw(211)) == 0, "unicode-raw d3")
        if (allocated(error)) return
        call check(error, sm("[å-"//raw(211)//"]", raw(187)) == 0, "unicode-raw bb")
        if (allocated(error)) return
        call check(error, sm("[å-"//raw(211)//"]", "ü") == 0, "unicode-raw ü")
        if (allocated(error)) return
        ! No equivalence between raw bytes and latin-1
        call check(error, sm("å", raw(229)) == 0, "latin-1 1")
        if (allocated(error)) return
        call check(error, sm("[å]", raw(229)) == 0, "latin-1 2")
        if (allocated(error)) return
        call check(error, sm(raw(229), "å") == 0, "latin-1 3")
        if (allocated(error)) return
        call check(error, sm("["//raw(229)//"]", "å") == 0, "latin-1 4")
    end subroutine test_multibyte

    !> Test case-sensitive and case-insensitive matching.
    subroutine test_case_fold(error)
        type(error_type), allocatable, intent(out) :: error

        case_fold_search = .false.
        call check(error, sm("aB", "ABaB") == 3, "nil aB")
        if (allocated(error)) return
        call check(error, sm("åÄ", "ÅäåäÅÄåÄ") == 7, "nil åÄ")
        if (allocated(error)) return
        call check(error, sm("λΛ", "lΛλλΛ") == 4, "nil λΛ")
        if (allocated(error)) return
        call check(error, sm("шШ", "zШшшШ") == 4, "nil шШ")
        if (allocated(error)) return
        call check(error, sm("[[:alpha:]]+", ".3aBåÄßλΛшШ中﷽") == 3 .and. me(0) == 13, "nil alpha")
        if (allocated(error)) return
        call check(error, sm("[[:alnum:]]+", ".3aBåÄßλΛшШ中﷽") == 2 .and. me(0) == 13, "nil alnum")
        if (allocated(error)) return
        call check(error, sm("[[:upper:]]+", ".3aåλшBÄΛШ中﷽") == 7 .and. me(0) == 11, "nil upper")
        if (allocated(error)) return
        call check(error, sm("[[:lower:]]+", ".3BÄΛШaåλш中﷽") == 7 .and. me(0) == 11, "nil lower")
        if (allocated(error)) return
        case_fold_search = .true.
        call check(error, sm("aB", "ABaB") == 1, "t aB")
        if (allocated(error)) return
        call check(error, sm("åÄ", "ÅäåäÅÄåÄ") == 1, "t åÄ")
        if (allocated(error)) return
        call check(error, sm("λΛ", "lΛλλΛ") == 2, "t λΛ")
        if (allocated(error)) return
        call check(error, sm("шШ", "zШшшШ") == 2, "t шШ")
        if (allocated(error)) return
        call check(error, sm("[[:alpha:]]+", ".3aBåÄßλΛшШ中﷽") == 3 .and. me(0) == 13, "t alpha")
        if (allocated(error)) return
        call check(error, sm("[[:alnum:]]+", ".3aBåÄßλΛшШ中﷽") == 2 .and. me(0) == 13, "t alnum")
        if (allocated(error)) return
        call check(error, sm("[[:upper:]]+", ".3aåλшBÄΛШ中﷽") == 3 .and. me(0) == 11, "t upper")
        if (allocated(error)) return
        call check(error, sm("[[:lower:]]+", ".3BÄΛШaåλш中﷽") == 3 .and. me(0) == 11, "t lower")
    end subroutine test_case_fold

    !> Test matching of ß and ẞ.
    subroutine test_eszett(error)
        type(error_type), allocatable, intent(out) :: error

        case_fold_search = .false.
        call check(error, sm("ß", "ß") == 1 .and. sm("ß", "ẞ") == 0 .and. sm("ẞ", "ß") == 0 .and. &
                   sm("ẞ", "ẞ") == 1, "nil literal")
        if (allocated(error)) return
        call check(error, sm("[[:alpha:]]", "ß") == 1 .and. sm("[[:lower:]]", "ß") == 1 .and. &
                   sm("[[:upper:]]", "ß") == 0, "nil ß classes")
        if (allocated(error)) return
        call check(error, sm("[[:alpha:]]", "ẞ") == 1 .and. sm("[[:lower:]]", "ẞ") == 0 .and. &
                   sm("[[:upper:]]", "ẞ") == 1, "nil ẞ classes")
        if (allocated(error)) return
        case_fold_search = .true.
        call check(error, sm("ß", "ß") == 1 .and. sm("ß", "ẞ") == 1 .and. sm("ẞ", "ß") == 1 .and. &
                   sm("ẞ", "ẞ") == 1, "t literal")
        if (allocated(error)) return
        call check(error, sm("[[:alpha:]]", "ß") == 1 .and. sm("[[:lower:]]", "ß") == 1 .and. &
                   sm("[[:upper:]]", "ß") == 1, "t ß classes")
        if (allocated(error)) return
        call check(error, sm("[[:alpha:]]", "ẞ") == 1 .and. sm("[[:lower:]]", "ẞ") == 1 .and. &
                   sm("[[:upper:]]", "ẞ") == 1, "t ẞ classes")
        case_fold_search = .true.
    end subroutine test_eszett

    !> Bug#58726.
    subroutine test_atomic_failure(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, sm("\`\(?:ab\)*\'", "a") == 0)
        if (allocated(error)) return
        call check(error, sm("\`a\{2\}*\'", "a") == 0)
    end subroutine test_atomic_failure

    !> Make sure we don't use up the regexp stack needlessly.
    subroutine test_backtrack_optimization(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        character(len=24), parameter :: res(16) = [character(len=24) :: &
                                                   "x*=*", "x*\(=\|:\)", "x*\(=\|:\)*", "x*=*?", &
                                                   "x*\(=*\|h\)*?", "x*\(=*\|h\)*", "x*\(=*?\|h\)*", &
                                                   "x*\(=*?\|h\)*?", "x*\(=*\|h\)+?", "x*\(=*\|h\)+", &
                                                   "x*\(=*?\|h\)+", "x*\(=*?\|h\)+?", "x*\(=+\|h\)+?", &
                                                   "x*\(=+\|h\)+", "x*\(=+?\|h\)+", "x*\(=+?\|h\)+?"]
        type(status_type) :: st
        integer :: i

        call enter(repeat("x", 1000000)//"=", h)

        do i = 1, size(res)
            call check(error, looking_at(trim(res(i)), st), trim(res(i))//" "//format_error(st))
            if (allocated(error)) exit
        end do

        call leave(h)
        if (allocated(error)) return
        ! Regression check for overly optimistic optimization.
        call check(error, sm("\(ca*\|ab\)+d", "cabd") == 1, "cabd")
        if (allocated(error)) return
        call check(error, sm("\(aa*\|b\)*c", "ababc") > 0, "ababc")
        if (allocated(error)) return
        call check(error, sm(" \sw*\bfoo", " foo") > 0, "\bfoo")
        if (allocated(error)) return
        call check(error, sm(".*\>", "hello ") > 0, "\>")
    end subroutine test_backtrack_optimization

    !> Check compatibility behavior with repetition operators after
    !> certain zero-width assertions (bug#64128).
    subroutine test_zero_width(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: nl = achar(10)

        ! Postfix operators after ^ and \` become literals, for historical
        ! compatibility.  Only the first character of a lazy operator (like
        ! *?) becomes a literal.
        call t("^*a", "x"//nl//"*a", 3)
        call t("^*?a", "x"//nl//"*a", 3)
        call t("^*?a", "x"//nl//"a", 3)
        call t("^*?a", "x"//nl//"**a", 0)
        call t("\`*a", "*a", 1)
        call t("\`*?a", "*a", 1)
        call t("\`*?a", "a", 1)
        call t("\`*?a", "**a", 0)
        ! Other zero-width assertions are treated as normal elements, so
        ! postfix operators apply to them alone (which is pointless but
        ! valid).
        call t("\b*!", "*!", 2)
        call t("!\b+;", "!;", 0)
        call t("!\b+a", "!a", 1)
        call t("\B*!", "*!", 2)
        call t("!\B+;", "!;", 1)
        call t("!\B+a", "!a", 0)
        call t("\<*b", "*b", 2)
        call t("a\<*b", "ab", 1)
        call t(";\<*b", ";b", 1)
        call t("a\<+b", "ab", 0)
        call t(";\<+b", ";b", 1)
        call t("\>*;", "*;", 2)
        call t("a\>*b", "ab", 1)
        call t("a\>*;", "a;", 1)
        call t("a\>+b", "ab", 0)
        call t("a\>+;", "a;", 1)
        call t("a\'", "ab", 0)
        call t("b\'", "ab", 2)
        call t("a\'*b", "ab", 1)
        call t("a\'+", "ab", 0)
        call t("b\'+", "ab", 2)
        call t("\'+", "+", 2)
        call t("\_<*b", "*b", 2)
        call t("a\_<*b", "ab", 1)
        call t(" \_<*b", " b", 1)
        call t("a\_<+b", "ab", 0)
        call t(" \_<+b", " b", 1)
        call t("\_>*;", "*;", 2)
        call t("a\_>*b", "ab", 1)
        call t("a\_>* ", "a ", 1)
        call t("a\_>+b", "ab", 0)
        call t("a\_>+ ", "a ", 1)
        call t("\=*b", "*b", 2)
        call t("a\=*b", "a*b", 0)
        call t("a\=*b", "ab", 1)

    contains

        subroutine t(regexp, string, want)
            character(len=*), intent(in) :: regexp, string
            integer, intent(in) :: want

            if (allocated(error)) return
            call check(error, sm(regexp, string) == want, regexp//" on "//string)
        end subroutine t
    end subroutine test_zero_width

    !> Regression test for bug#65726, where this crashed Emacs.
    subroutine test_mutual_exclusive(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, sm("a*\(?:c\|b*\)*", "a") == 1)
    end subroutine test_mutual_exclusive

    !> The lines of FILE (without newlines), or an error.
    subroutine read_lines(file, lines, lengths, error)
        character(len=*), intent(in) :: file
        character(len=512), allocatable, intent(out) :: lines(:)
        integer, allocatable, intent(out) :: lengths(:)
        type(error_type), allocatable, intent(inout) :: error
        character(len=:), allocatable :: data
        integer :: u, ios, n, i, start
        integer(int64) :: fsize

        open (newunit=u, file=file, access="stream", form="unformatted", status="old", action="read", iostat=ios)
        call check(error, ios == 0, "can't open "//file)
        if (allocated(error)) return
        inquire (unit=u, size=fsize)
        allocate (character(len=fsize) :: data)
        read (u, iostat=ios) data
        close (u)
        call check(error, ios == 0, "can't read "//file)
        if (allocated(error)) return
        n = count([(data(i:i) == achar(10), i=1, len(data))])
        allocate (lines(n), lengths(n))
        start = 1
        n = 0

        do i = 1, len(data)
            if (data(i:i) == achar(10)) then
                n = n + 1
                lines(n) = data(start:i - 1)
                lengths(n) = i - start
                start = i + 1
            end if
        end do
    end subroutine read_lines

    !> Match PATTERN against STRING and compare with the expected outcome:
    !> `want_start` -2 for a pattern that should not compile, -1 for no
    !> match, 0 for any match, else the start; then `want_end` (exclusive)
    !> unless negative.  Return a message, or "" on success.  Port of
    !> regex-tests-match and regex-tests-compare (group 0).
    function try_match(pattern, string, want_start, want_end) result(msg)
        character(len=*), intent(in) :: pattern, string
        integer, intent(in) :: want_start, want_end
        character(len=:), allocatable :: msg
        type(status_type) :: st
        integer(int64) :: i
        character(len=40) :: buffer

        msg = ""
        ! [.x.] and [=x=] forms are not supported by Emacs.
        if (index(pattern, "[.") > 0 .or. index(pattern, "[=") > 0) return
        i = string_match(pattern, string, st)

        if (st%code == FM_INVALID_REGEXP) then
            if (want_start /= -2) msg = "Expected match or no match; but pattern failure"
        else if (i == 0) then
            if (want_start /= -1) msg = "Expected match or pattern failure; but no match"
        else if (want_start == -1) then
            msg = "Expected no match; but match"
        else if (want_start == -2) then
            msg = "Expected pattern failure; but match"
        else if (want_start > 0) then
            if (mb(0) /= want_start .or. (want_end >= 0 .and. me(0) /= want_end)) then
                write (buffer, "(4(i0,1x))") mb(0), want_start, me(0), want_end
                msg = "mismatch in group 0: "//trim(buffer)
            end if
        end if
    end function try_match

    !> Run the tests of PTESTS: start¦end¦pattern¦string, with 1-based
    !> start and inclusive end, -1 for no match and -2 for an invalid
    !> pattern.  Port of regex-tests-PTESTS.
    subroutine test_ptests(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=512), allocatable :: lines(:)
        integer, allocatable :: lengths(:)
        character(len=*), parameter :: sep = "¦"
        ! Emacs doesn't see DEL (0x7f) as a [:cntrl:] character, and
        ! doesn't barf on weird ranges such as [b-a], but simply fails to
        ! match.
        integer, parameter :: whitelist(2) = [138, 168]
        type(handle_type) :: h, table
        type(status_type) :: st
        character(len=:), allocatable :: line, msg, failures, fields(:)
        integer :: n, raw_start, raw_end, want_start, want_end, nf, tested

        call read_lines("test/regex-resources/PTESTS", lines, lengths, error)
        if (allocated(error)) return
        call enter("", h)
        table = underscore_word()
        case_fold_search = .false.
        failures = ""
        tested = 0

        do n = 1, size(lines)
            line = lines(n) (1:lengths(n))
            if (len(line) == 0) cycle
            if (line(1:1) == "#" .or. any(whitelist == n)) cycle
            call split(line, sep, fields, nf)
            tested = tested + 1
            read (fields(1), *) raw_start
            read (fields(2), *) raw_end

            ! 0 is a special case, treated as start 0 (our 1).
            select case (raw_start)
            case (-2, -1)
                want_start = raw_start
            case (0)
                want_start = 1
            case default
                want_start = raw_start
            end select

            want_end = raw_end + 1
            msg = try_match(trim(fields(3)), trim(fields(4)), want_start, want_end)

            if (len(msg) > 0) then
                write (msg, "(a,i0,a)") msg//" (line ", n, ")"
                failures = failures//" "//trim(fields(3))//": "//msg//";"
            end if
        end do

        call leave(h)
        call destroy_syntax_table(table, st)
        call check(error, tested > 100, "too few tests read")
        if (allocated(error)) return
        call check(error, len(failures) == 0, failures)

    contains

        subroutine split(string, sp, parts, np)
            character(len=*), intent(in) :: string, sp
            character(len=:), allocatable, intent(out) :: parts(:)
            integer, intent(out) :: np
            integer :: pos, nxt

            allocate (character(len=len(string)) :: parts(8))
            np = 0
            pos = 1

            do
                nxt = index(string(pos:), sp)
                np = np + 1

                if (nxt == 0) then
                    parts(np) = string(pos:)
                    exit
                end if

                parts(np) = string(pos:pos + nxt - 2)
                pos = pos + nxt - 1 + len(sp)
            end do
        end subroutine split
    end subroutine test_ptests

    !> Run the tests of TESTS: expected:pattern:string, with expected 0 for
    !> a match, 1 for no match and 2 for an invalid pattern; the patterns
    !> are extended regexps.  Port of regex-tests-TESTS.
    subroutine test_tests(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=512), allocatable :: lines(:)
        integer, allocatable :: lengths(:)
        ! Emacs doesn't barf on weird ranges such as [b-a]; it is more
        ! forgiving with * and ? that don't apply to characters, and more
        ! stringent with regexes involving unbalanced ).
        integer, parameter :: whitelist(6) = [42, 57, 58, 59, 60, 67]
        type(handle_type) :: h, table
        type(status_type) :: st
        character(len=:), allocatable :: line, msg, failures, pattern
        integer :: n, first, last, want, tested

        call read_lines("test/regex-resources/TESTS", lines, lengths, error)
        if (allocated(error)) return
        call enter("", h)
        table = underscore_word()
        case_fold_search = .false.
        failures = ""
        tested = 0

        do n = 1, size(lines)
            line = lines(n) (1:lengths(n))
            if (len(line) == 0) cycle
            if (line(1:1) == "#" .or. any(whitelist == n)) cycle
            first = index(line, ":")
            tested = tested + 1
            last = index(line, ":", back=.true.)
            read (line(1:first - 1), *) want
            pattern = unextend(line(first + 1:last - 1))

            select case (want)
            case (2)
                msg = try_match(pattern, line(last + 1:), -2, -1)
            case (1)
                msg = try_match(pattern, line(last + 1:), -1, -1)
            case default
                msg = try_match(pattern, line(last + 1:), 0, -1)
            end select

            if (len(msg) > 0) then
                write (msg, "(a,i0,a)") msg//" (line ", n, ")"
                failures = failures//" "//pattern//": "//msg//";"
            end if
        end do

        call leave(h)
        call destroy_syntax_table(table, st)
        call check(error, tested > 100, "too few tests read")
        if (allocated(error)) return
        call check(error, len(failures) == 0, failures)
    end subroutine test_tests

    !> Basic conversion from extended regexes to emacs ones: add \ to ()
    !> | and {}, or remove it if it already exists, and change \S (and
    !> \s) to \S- (and \s-).  Port of regex-tests-unextend.
    function unextend(pattern) result(out)
        character(len=*), intent(in) :: pattern
        character(len=:), allocatable :: out
        integer :: i, nback

        out = ""
        nback = 0

        do i = 1, len(pattern)
            if (index("()|{}", pattern(i:i)) > 0) then
                if (mod(nback, 2) == 1) then
                    ! Preceded by an odd number of \: unescape it.
                    out = out(1:len(out) - 1)//pattern(i:i)
                else
                    out = out//"\"//pattern(i:i)
                end if
            else if ((pattern(i:i) == "s" .or. pattern(i:i) == "S") .and. mod(nback, 2) == 1) then
                out = out//pattern(i:i)//"-"
            else
                out = out//pattern(i:i)
            end if

            if (pattern(i:i) == "\") then
                nback = nback + 1
            else
                nback = 0
            end if
        end do
    end function unextend

    !> The tests expect _ to be a word constituent: give the current
    !> buffer a syntax table that says so.  Return the table, for freeing.
    function underscore_word() result(table)
        type(handle_type) :: table
        type(status_type) :: st

        call make_syntax_table(table, st)
        call set_syntax_table(table, st)
        call modify_syntax_entry(iachar("_"), iachar("_"), "w", st)
    end function underscore_word

    !> Mangle \\ the way it is done in frob_escapes() in
    !> regex-tests-BOOST.c in glibc: \t, \n, \r are interpreted; \\\\,
    !> \^, \{, \|, \} are unescaped for the string (not pattern).  Uses
    !> the current buffer.  Port of regex-tests-BOOST-frob-escapes.
    function frob_escapes(string, ispattern) result(out)
        character(len=*), intent(in) :: string
        logical, intent(in) :: ispattern
        character(len=:), allocatable :: out
        type(status_type) :: st

        call erase_buffer(st)
        call insert(string, st)
        call replace_escaped("t", achar(9))
        call replace_escaped("n", achar(10))
        call replace_escaped("r", achar(13))

        if (.not. ispattern) then
            call replace_escaped("\\", "\")
            call replace_escaped("^", "^")
            call replace_escaped("{", "{")
            call replace_escaped("|", "|")
            call replace_escaped("}", "}")
        end if

        out = buffer_string()

    contains

        !> Replace each odd-escaped X (a regexp) by its group 1 followed by
        !> TO.
        subroutine replace_escaped(x, to)
            character(len=*), intent(in) :: x, to
            character(len=:), allocatable :: group
            integer(int64) :: pos

            call goto_char(point_min())

            do
                call re_search_forward("\("//re_even_escapes//"\)\\"//x, pos, st, noerror=SEARCH_NOERROR)
                if (pos == 0) exit
                call buffer_substring(mb(1), me(1), group, st)
                call delete_region(mb(0), me(0), st)
                call goto_char(mb(0))
                call insert(group//to, st)
            end do
        end subroutine replace_escaped
    end function frob_escapes

    !> Characters B to E (exclusive, 1-based) of STRING.
    function substring(string, b, e) result(sub)
        character(len=*), intent(in) :: string
        integer(int64), intent(in) :: b, e
        character(len=:), allocatable :: sub

        sub = string(string_index_to_offset(string, b) + 1:string_index_to_offset(string, e))
    end function substring

    !> Run the tests of BOOST.tests.  Port of regex-tests-BOOST.
    subroutine test_boost(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=512), allocatable :: lines(:)
        integer, allocatable :: lengths(:)
        integer, parameter :: whitelist(*) = [63, 65, 69, 91, 107, 108, 109, 122, 123, 124, 140, 141, 142, 161, 222, &
                                              223, 284, 294, 443, 444, 445, 446, 448, 449, 450, 539, 580, 581, 639, &
                                              677, 712]
        type(handle_type) :: h, parse_buffer, table
        type(status_type) :: st
        character(len=:), allocatable :: line, failures, pattern_raw, string_raw, positions_raw, pattern, string, &
                                         msg
        integer(int64) :: pos, positions(40)
        integer :: n, npos, ios, k, tested
        logical :: basic, icase, notbol, noteol, compile_fail

        call read_lines("test/regex-resources/BOOST.tests", lines, lengths, error)
        if (allocated(error)) return
        call enter("", h)
        table = underscore_word()
        call get_or_make_buffer(" boost-parse", parse_buffer, st)
        failures = ""
        basic = .false.
        icase = .false.
        notbol = .false.
        noteol = .false.
        tested = 0

        do n = 1, size(lines)
            line = lines(n) (1:lengths(n))
            if (len(line) == 0) cycle
            if (line(1:1) == ";" .or. any(whitelist == n)) cycle

            if (line(1:1) == "-") then
                basic = index(line, "REG_BASIC") > 0
                icase = index(line, "REG_ICASE") > 0
                notbol = index(line, "REG_NOTBOL") > 0
                noteol = index(line, "REG_NOTEOL") > 0
                cycle
            end if

            ! Parse the line with regexps, in a buffer of its own.
            call set_buffer(parse_buffer, st)
            call erase_buffer(st)
            call insert(line, st)
            call goto_char(1_int64)
            call re_search_forward('\(\S-+\)\s-+"\(.*\)"\s-+?\(.+\)', pos, st, noerror=SEARCH_NOERROR)
            if (pos == 0) call re_search_forward("\(\S-+\)\s-+\(\S-+\)\s-+?\(.+\)", pos, st, &
                                                 noerror=SEARCH_NOERROR)
            if (pos == 0) call re_search_forward("\(\S-+\)\s-+\(!\)", pos, st, noerror=SEARCH_NOERROR)
            call check(error, pos > 0, "can't parse line")
            if (allocated(error)) exit
            call buffer_substring(mb(1), me(1), pattern_raw, st)
            call buffer_substring(mb(2), me(2), string_raw, st)
            positions_raw = ""
            if (mb(3) > 0) call buffer_substring(mb(3), me(3), positions_raw, st)
            pattern = frob_escapes(pattern_raw, .true.)
            string = frob_escapes(string_raw, .false.)
            call set_buffer(h, st)
            compile_fail = string == "!"
            npos = 0

            if (.not. compile_fail) then
                do k = 1, size(positions)
                    read (positions_raw, *, iostat=ios) positions(1:k)
                    if (ios /= 0) exit
                    npos = k
                end do
            end if

            if (.not. basic) pattern = unextend(pattern)
            ! Skipping test: notbol/noteol not supported.
            if (notbol .or. noteol) cycle
            tested = tested + 1
            case_fold_search = icase
            msg = compare(pattern, string, compile_fail, positions(1:npos))

            if (len(msg) > 0) then
                write (msg, "(a,i0,a)") msg//" (line ", n, ")"
                failures = failures//" "//pattern//": "//msg//";"
            end if
        end do

        call close_buffer(parse_buffer, st)
        call leave(h)
        call destroy_syntax_table(table, st)
        if (allocated(error)) return
        call check(error, tested > 100, "too few tests read")
        if (allocated(error)) return
        call check(error, len(failures) == 0, failures)

    contains

        !> Match and compare the groups with the 0-based POS (start, end
        !> pairs; a negative start for no match).  Port of
        !> regex-tests-match and regex-tests-compare.
        function compare(regexp, text, want_fail, pos_ref) result(m)
            character(len=*), intent(in) :: regexp, text
            logical, intent(in) :: want_fail
            integer(int64), intent(in) :: pos_ref(:)
            character(len=:), allocatable :: m
            type(status_type) :: s
            integer(int64) :: i, b, e
            integer :: g
            logical :: want_no_match
            character(len=80) :: buffer

            m = ""
            ! [.x.] and [=x=] forms are not supported by Emacs.
            if (index(regexp, "[.") > 0 .or. index(regexp, "[=") > 0) return
            want_no_match = .false.
            if (size(pos_ref) > 0) want_no_match = pos_ref(1) < 0
            i = string_match(regexp, text, s)

            if (s%code == FM_INVALID_REGEXP) then
                if (.not. want_fail) m = "Expected match; but pattern failure"
                return
            end if

            if (want_fail) then
                m = "Expected pattern failure; but match or no match"
                return
            end if

            if (i == 0) then
                if (.not. want_no_match) m = "Expected match; but no match"
                return
            end if

            if (want_no_match) then
                m = "Expected no match; but match"
                return
            end if

            do g = 0, size(pos_ref)/2 - 1
                b = mb(g)
                e = me(g)

                if (pos_ref(2*g + 1) < 0) then
                    if (b /= 0) then
                        write (buffer, "(a,i0)") "group unexpectedly matched ", g
                        m = trim(buffer)
                        return
                    end if
                else if (b /= pos_ref(2*g + 1) + 1 .or. e /= pos_ref(2*g + 2) + 1) then
                    write (buffer, "(a,i0,a,4(1x,i0))") "mismatch in group ", g, ":", b - 1, pos_ref(2*g + 1), e - 1, &
                        pos_ref(2*g + 2)
                    m = trim(buffer)
                    return
                end if
            end do
        end function compare
    end subroutine test_boost

    !> Run the tests of PCRE.tests.  Port of regex-tests-PCRE.
    subroutine test_pcre(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=512), allocatable :: lines(:)
        integer, allocatable :: lengths(:)
        integer, parameter :: whitelist(*) = [610, 611, 1154, 1157, 1160, 1168, 1171, 1176, 1179, 1182, 1185, 1188, &
                                              1193, 1196, 1203]
        type(handle_type) :: h, table
        type(status_type) :: st
        character(len=:), allocatable :: line, failures, pattern, string, match_ref
        character(len=200) :: observed(0:20)
        logical :: icase, has_pattern, search_failed, compile_failed
        integer :: n, position, colon, last, tested, g
        integer(int64) :: i
        character(len=20) :: buffer

        call read_lines("test/regex-resources/PCRE.tests", lines, lengths, error)
        if (allocated(error)) return
        call enter("", h)
        table = underscore_word()
        failures = ""
        has_pattern = .false.
        icase = .false.
        search_failed = .false.
        compile_failed = .false.
        tested = 0

        do n = 1, size(lines)
            line = lines(n) (1:lengths(n))
            if (len(line) == 0) cycle
            if (line(1:1) == "#" .or. any(whitelist == n)) cycle
            last = index(line, "/", back=.true.)

            if (line(1:1) == "/" .and. last > 1) then
                ! A pattern.
                icase = line(last + 1:) == "i"
                pattern = unextend(line(2:last - 1))
                has_pattern = .true.
            else if (index(line, "    ") == 1 .and. len(line) > 4 .and. has_pattern) then
                ! A string: match it against the pattern and save the
                ! results.
                string = line(5:)
                case_fold_search = icase
                i = string_match(pattern, string, st)
                compile_failed = st%code == FM_INVALID_REGEXP
                search_failed = .not. compile_failed .and. i == 0
                observed = "<unset>"

                if (.not. (compile_failed .or. search_failed)) then
                    do g = 0, 20
                        if (mb(g) > 0) observed(g) = substring(string, mb(g), me(g))
                    end do
                end if

                tested = tested + 1
            else if (index(line, "No match") == 1) then
                if (.not. (compile_failed .or. search_failed)) then
                    write (buffer, "(i0)") n
                    failures = failures//" line "//trim(buffer)//": "//pattern//": Expected no match; but match;"
                end if
            else if (verification_line(line, position, colon)) then
                match_ref = line(colon + 2:)

                if (.not. (compile_failed .or. search_failed) .and. position <= 20) then
                    if (match_ref /= trim(observed(position))) then
                        write (buffer, "(i0)") n
                        failures = failures//" line "//trim(buffer)//": "//pattern//": group wrong: '"//match_ref// &
                                   "'/'"//trim(observed(position))//"';"
                    end if
                end if
            else
                ! Reset.
                has_pattern = .false.
            end if
        end do

        case_fold_search = .true.
        call leave(h)
        call destroy_syntax_table(table, st)
        call check(error, tested > 100, "too few tests read")
        if (allocated(error)) return
        call check(error, len(failures) == 0, failures)

    contains

        !> Whether LINE is " *N: text"; INDEX is N and COLON where the colon is.
        logical function verification_line(ln, num, col)
            character(len=*), intent(in) :: ln
            integer, intent(out) :: num, col
            integer :: k, ios

            verification_line = .false.
            num = -1
            col = index(ln, ": ")
            if (col < 2) return

            do k = 1, col - 1
                if (ln(k:k) /= " " .and. (ln(k:k) < "0" .or. ln(k:k) > "9")) return
            end do

            if (len_trim(ln(1:col - 1)) == 0) return
            if (index(adjustl(ln(1:col - 1)), " ") <= len_trim(adjustl(ln(1:col - 1)))) return
            read (ln(1:col - 1), *, iostat=ios) num
            verification_line = ios == 0
        end function verification_line
    end subroutine test_pcre

    !> Syntax classes, word and symbol boundaries.
    subroutine test_syntax(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, sm("\w+", "  foo-bar") == 3 .and. me(0) == 6, "\w")
        if (allocated(error)) return
        call check(error, sm("\W+", "foo-+bar") == 4 .and. me(0) == 6, "\W")
        if (allocated(error)) return
        call check(error, sm("\s-+", "a  b") == 2 .and. me(0) == 4, "\s-")
        if (allocated(error)) return
        call check(error, sm("\S-+", "  ab ") == 3 .and. me(0) == 5, "\S-")
        if (allocated(error)) return
        call check(error, sm("\<bar", "foobar bar") == 8, "\<")
        if (allocated(error)) return
        call check(error, sm("foo\>", "foobar foo") == 8, "\>")
        if (allocated(error)) return
        call check(error, sm("\bb", "ab b") == 4, "\b")
        if (allocated(error)) return
        call check(error, sm("\Bb", "b ab") == 4, "\B")
        if (allocated(error)) return
        call check(error, sm("\_<x", "a-x x") == 5, "\_<")
        if (allocated(error)) return
        call check(error, sm("\s(.*\s)", "a (b) c") == 3 .and. me(0) == 6, "\s( \s)")
        if (allocated(error)) return
        ! Script boundaries are word boundaries.
        call check(error, sm("\<日本", "abc日本") == 4, "script boundary")
    end subroutine test_syntax

    !> Groups, shy and numbered groups, alternatives and back references.
    subroutine test_groups(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, sm("\(a\)\(b\)?\(c\)", "xac") == 2, "optional group")
        if (allocated(error)) return
        call check(error, mb(1) == 2 .and. me(1) == 3 .and. mb(2) == 0 .and. me(2) == 0 .and. mb(3) == 3, &
                   "group positions")
        if (allocated(error)) return
        call check(error, sm("\(?:ab\)+\(c\)", "ababc") == 1 .and. mb(1) == 5, "shy group")
        if (allocated(error)) return
        call check(error, sm("\(?2:x\)\(?1:y\)", "xy") == 1 .and. mb(1) == 2 .and. mb(2) == 1, "numbered")
        if (allocated(error)) return
        call check(error, sm("foo\|bar\|baz", "xxbaz") == 3, "alternatives")
        if (allocated(error)) return
        call check(error, sm("\(a+\)b\1", "aaabaa") == 2 .and. me(0) == 7, "backref")
        if (allocated(error)) return
        call check(error, sm("\(.\)\1", "abccd") == 3, "backref doubled")
        if (allocated(error)) return
        call check(error, sm("^\(a\|ab\)\(c\|bcd\)\(d*\)$", "abcd") == 1 .and. mb(3) == 5, "backtracking")
        if (allocated(error)) return
        call check(error, sm("a.*?b", "axbxb") == 1 .and. me(0) == 4, "lazy")
        if (allocated(error)) return
        call check(error, sm("a.*b", "axbxb") == 1 .and. me(0) == 6, "greedy")
        if (allocated(error)) return
        call check(error, sm("x??y", "xy") == 1, "lazy ?")
        if (allocated(error)) return
        call check(error, sm("[]a]+", "b]a]") == 2 .and. me(0) == 5, "] first in set")
        if (allocated(error)) return
        call check(error, sm("[^]a]", "]ab") == 3, "] first in negated set")
        if (allocated(error)) return
        call check(error, sm("[a-]+", "x-a-") == 2 .and. me(0) == 5, "- last in set")
        if (allocated(error)) return
        call check(error, sm("\(?:\)", "abc") == 1 .and. me(0) == 1, "empty group")
    end subroutine test_groups

    !> Intervals.
    subroutine test_intervals(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, sm("a\{2,3\}", "aaaa") == 1 .and. me(0) == 4, "{2,3}")
        if (allocated(error)) return
        call check(error, sm("a\{2\}", "a aa") == 3 .and. me(0) == 5, "{2}")
        if (allocated(error)) return
        call check(error, sm("a\{,2\}b", "aaab") == 2, "{,2}")
        if (allocated(error)) return
        call check(error, sm("ba\{2,\}", "ba baaa") == 4 .and. me(0) == 8, "{2,}")
        if (allocated(error)) return
        call check(error, sm("\(ab\)\{2\}", "abab") == 1 .and. mb(1) == 3, "group {2}")
        if (allocated(error)) return
        call check(error, sm("x\{0\}y", "xy") == 2, "{0}")
        if (allocated(error)) return
        call check(error, sm("\{1\}", "{1}") == 1, "interval without operand is literal")
    end subroutine test_intervals

    !> Categories `l' and `|' as characters.el gives them.
    subroutine test_categories(error)
        type(error_type), allocatable, intent(out) :: error

        ! U+0250 (Latin Extended-B tail), U+24B6 (circled A), U+FF21
        ! (fullwidth A), U+2C60, U+A722, U+1DF00 are Latin.
        call check(error, sm("\cl", "ɐ") == 1 .and. sm("\cl", "Ⓐ") == 1 .and. sm("\cl", "Ａ") == 1 .and. &
                   sm("\cl", "Ⱡ") == 1 .and. sm("\cl", "Ꜣ") == 1 .and. sm("\cl", "𝼀") == 1, "latin")
        if (allocated(error)) return
        ! U+02B9 and U+FF10 (fullwidth 0) are not.
        call check(error, sm("\cl", "ʹ") == 0 .and. sm("\cl", "０") == 0, "not latin")
        if (allocated(error)) return
        ! Halfwidth katakana U+FF71 and Tibetan U+0F0B, U+0F7F are line
        ! breakable; U+FFE6 and Tibetan U+0F13 are not.
        call check(error, sm("\c|", "ｱ") == 1 .and. sm("\c|", "་") == 1 .and. sm("\c|", "ཿ") == 1, "breakable")
        if (allocated(error)) return
        call check(error, sm("\c|", "￦") == 0 .and. sm("\c|", "༓") == 0, "not breakable")
    end subroutine test_categories

    !> Errors.
    subroutine test_invalid(error)
        type(error_type), allocatable, intent(out) :: error

        call bad("\(", "Unmatched ( or \(")
        call bad("\)", "Unmatched ) or \)")
        call bad("[a", "Unmatched [ or [^")
        call bad("a\", "Trailing backslash")
        call bad("[[:foo:]]", "Invalid character class name")
        call bad("\(a\)\2", "Invalid back reference")
        call bad("a\{2,1\}", "Invalid content of \{\}")
        call bad("a\{1", "Unmatched \{")
        ! Class names must match exactly (re_wctype_parse compares lengths).
        call bad("[[:word :]]", "Invalid character class name")
        call bad("[[:alpha  :]]", "Invalid character class name")

    contains

        subroutine bad(regexp, msg)
            character(len=*), intent(in) :: regexp, msg
            type(status_type) :: st
            integer(int64) :: i

            if (allocated(error)) return
            i = string_match(regexp, "", st)
            call check(error, st%code == FM_INVALID_REGEXP .and. format_error(st) == "Invalid regexp: """//msg//"""", &
                       regexp//": "//format_error(st))
        end subroutine bad
    end subroutine test_invalid

    subroutine test_regexp_search(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        integer(int64) :: pos

        call enter("foo1 bar22 baz333", h)

        body: block
            call re_search_forward("[a-z]+\([0-9]+\)", pos, st)
            call check(error, st%ok() .and. pos == 5 .and. point() == 5 .and. mb(1) == 4, "first")
            if (allocated(error)) exit body
            call re_search_forward("[a-z]+\([0-9]+\)", pos, st, count=2_int64)
            call check(error, st%ok() .and. pos == 18 .and. mb(1) == 15 .and. me(1) == 18, "count 2")
            if (allocated(error)) exit body
            call re_search_backward("b[a-z]+", pos, st)
            call check(error, st%ok() .and. pos == 12 .and. me(0) == 15, "backward")
            if (allocated(error)) exit body
            call re_search_backward("^f", pos, st)
            call check(error, st%ok() .and. pos == 1, "backward anchored")
            if (allocated(error)) exit body
            call re_search_forward("qux", pos, st)
            call check(error, st%code == FM_SEARCH_FAILED .and. point() == 1, "fails")
            if (allocated(error)) exit body
            call re_search_forward("qux", pos, st, noerror=SEARCH_NOERROR)
            call check(error, st%ok() .and. pos == 0, "noerror")
            if (allocated(error)) exit body
            call re_search_forward("b..", pos, st, bound=9_int64)
            call check(error, st%ok() .and. pos == 9, "bound")
            if (allocated(error)) exit body
            call re_search_forward("\(", pos, st)
            call check(error, st%code == FM_INVALID_REGEXP, "invalid")
            if (allocated(error)) exit body
            ! A trivial regexp is searched for as a string, \ quoting.
            call goto_char(1_int64)
            call re_search_forward("bar2", pos, st)
            call check(error, st%ok() .and. pos == 10, "trivial")
        end block body

        call leave(h)
    end subroutine test_regexp_search

    subroutine test_looking_at(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call enter("hello world"//achar(10)//"next", h)

        body: block
            call check(error, looking_at("hel+o", st) .and. me(0) == 6, "at start")
            if (allocated(error)) exit body
            call check(error,.not. looking_at("world", st), "not")
            if (allocated(error)) exit body
            call goto_char(7_int64)
            call check(error, looking_at("world$", st), "$ before newline")
            if (allocated(error)) exit body
            call check(error,.not. looking_at("world\'", st), "\' not at end")
            if (allocated(error)) exit body
            call goto_char(13_int64)
            call check(error, looking_at("^next\'", st), "^ after newline")
            if (allocated(error)) exit body
            call check(error, looking_at("\=n", st), "\=")
            if (allocated(error)) exit body
            call goto_char(1_int64)
            call check(error, looking_at("hello", st, inhibit_modify=.true.) .and. mb(0) == 13, "inhibit-modify")
            if (allocated(error)) exit body
            call check(error, posix_looking_at("h\|hello", st) .and. me(0) == 6, "posix longest")
            if (allocated(error)) exit body
            call check(error, looking_at("h\|hello", st) .and. me(0) == 2, "first alternative")
        end block body

        call leave(h)
    end subroutine test_looking_at

    subroutine test_match_data(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int64), allocatable :: data(:)

        call check(error, sm("\(a\)\(x\)?\(b\)", "ab") == 1)
        if (allocated(error)) return
        data = match_data()
        call check(error, size(data) == 8, "length")
        if (allocated(error)) return
        call check(error, all(data == [1_int64, 3_int64, 1_int64, 2_int64, 0_int64, 0_int64, 2_int64, 3_int64]), &
                   "values")
        if (allocated(error)) return
        call set_match_data([5_int64, 9_int64])
        call check(error, mb(0) == 5 .and. me(0) == 9 .and. mb(1) == 0, "set_match_data")
    end subroutine test_match_data

    subroutine test_regexp_quote(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, regexp_quote("a.b*c[d]^$\?+") == "a\.b\*c\[d]\^\$\\\?\+")
        if (allocated(error)) return
        call check(error, sm(regexp_quote("1+1=2?"), "x 1+1=2?") == 3)
    end subroutine test_regexp_quote

    subroutine test_spaces_regexp(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        integer(int64) :: pos

        call enter("foo"//achar(9)//"  bar", h)

        body: block
            search_spaces_regexp = "[ "//achar(9)//achar(10)//"]+"
            call re_search_forward("foo bar", pos, st)
            call check(error, st%ok() .and. pos == point_max(), "lax whitespace")
            if (allocated(error)) exit body
            call goto_char(1_int64)
            call check(error, looking_at("foo *", st) .and. me(0) == 4, "space before * is literal")
            if (allocated(error)) exit body
            deallocate (search_spaces_regexp)
            call goto_char(1_int64)
            call re_search_forward("foo bar", pos, st, noerror=SEARCH_NOERROR)
            call check(error, pos == 0, "strict whitespace")
        end block body

        call leave(h)
    end subroutine test_spaces_regexp

    subroutine test_posix(error)
        type(error_type), allocatable, intent(out) :: error
        type(status_type) :: st

        call check(error, posix_string_match("a\|ab", "ab", st) == 1 .and. me(0) == 3, "longest")
        if (allocated(error)) return
        call check(error, sm("a\|ab", "ab") == 1 .and. me(0) == 2, "first")
        if (allocated(error)) return
        call check(error, string_match("b", "abcb", st, start=3_int64) == 4, "start")
        if (allocated(error)) return
        call check(error, string_match("b", "abcb", st, start=-1_int64) == 4, "negative start")
    end subroutine test_posix

    subroutine test_match_string(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st
        integer(int64) :: pos
        logical :: matched
        character(len=:), allocatable :: s

        call check(error, sm("\(é+\)\(x\)?\(z\)", "aéézb") == 2)
        if (allocated(error)) return
        call check(error, match_string(1, "aéézb") == "éé" .and. match_string(0, "aéézb") == "ééz", "string")
        if (allocated(error)) return
        s = match_string(2, "aéézb", matched)
        call check(error,.not. matched .and. s == "", "unmatched group")
        if (allocated(error)) return
        call check(error, string_matches("z", "aéézb", st) .and. mb(1) == 2, "string-match-p keeps data")
        if (allocated(error)) return
        call enter("key = value", h)

        body: block
            call re_search_forward("\(\w+\) = \(\w+\)", pos, st)
            call check(error, match_string(1) == "key" .and. match_string(2) == "value", "buffer")
            if (allocated(error)) exit body
            call goto_char(1_int64)
            call check(error, is_looking_at("key", st) .and. mb(1) == 1, "looking-at-p keeps data")
        end block body

        call leave(h)
    end subroutine test_match_string

    subroutine test_looking_back(error)
        type(error_type), allocatable, intent(out) :: error
        type(handle_type) :: h
        type(status_type) :: st

        call enter("aaa bbb", h)

        body: block
            call goto_char(4_int64)
            call check(error, looking_back("a+", st) .and. point() == 4, "plain")
            if (allocated(error)) exit body
            call check(error, mb(0) == 3, "non-greedy match is short")
            if (allocated(error)) exit body
            call check(error, looking_back("a+", st, greedy=.true.) .and. mb(0) == 1, "greedy")
            if (allocated(error)) exit body
            call check(error,.not. looking_back("b", st), "no")
            if (allocated(error)) exit body
            call check(error,.not. looking_back("a+", st, limit=4_int64), "limit")
        end block body

        call leave(h)
    end subroutine test_looking_back
end module test_regex_emacs_fm
