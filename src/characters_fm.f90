! characters_fm.f90 -- syntax and categories of non-ASCII characters.
! Ported from GNU Emacs 31.1 lisp/international/characters.el.
!
! Copyright (C) 1997-2026 Free Software Foundation, Inc.
! Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
!   2005, 2006, 2007, 2008, 2009, 2010, 2011
!   National Institute of Advanced Industrial Science and Technology (AIST)
!   Registration Number H14PRO021
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

!> The parts of characters.el that word motion needs: the entries it
!> makes in the standard syntax table for Unicode characters, in its
!> order (later entries override earlier ones), and the categories that
!> `word_combining_categories' and `word_separating_categories' refer to
!> (^ combining, C han, H hiragana, K katakana).
!>
!> Deferred: entries made through legacy charsets (`map_charset_chars'
!> over GB2312, Big5, JIS X 0208/0212, KS C 5601 and so on, which need
!> their mapping tables), the other categories, the case-table side of
!> `set_case_syntax', and everything else characters.el sets up.
module characters_fm
    use uni_category_fm, only: cased_letter_runs, cased_letter_starts, cased_letter_ends, &
                               nonspacing_mark_runs, nonspacing_mark_starts, nonspacing_mark_ends
    implicit none(type, external)
    private

    public :: syntax_entry_proc, characters_syntax, char_has_category

    abstract interface
        !> Give the characters `from' to `to' the syntax `descriptor'
        !> (as for `modify_syntax_entry').
        subroutine syntax_entry_proc(from, to, descriptor)
            implicit none(type, external)
            integer, intent(in) :: from, to
            character(len=*), intent(in) :: descriptor
        end subroutine syntax_entry_proc
    end interface

contains

    !> Make characters.el's standard-syntax-table entries through
    !> `entry', in characters.el's order.
    subroutine characters_syntax(entry)
        procedure(syntax_entry_proc) :: entry
        ! CJK parentheses, in more than one charset.
        integer, parameter :: cjk_parens(*) = [int(z'3008'), int(z'3009'), int(z'300A'), int(z'300B'), &
                                               int(z'300C'), int(z'300D'), int(z'300E'), int(z'300F'), &
                                               int(z'3010'), int(z'3011'), int(z'3014'), int(z'3015'), &
                                               int(z'3016'), int(z'3017'), int(z'3018'), int(z'3019'), &
                                               int(z'301A'), int(z'301B'), int(z'FE35'), int(z'FE36'), &
                                               int(z'FE37'), int(z'FE38'), int(z'FE39'), int(z'FE3A'), &
                                               int(z'FE3B'), int(z'FE3C'), int(z'FE3D'), int(z'FE3E'), &
                                               int(z'FE3F'), int(z'FE40'), int(z'FE41'), int(z'FE42'), &
                                               int(z'FE43'), int(z'FE44'), int(z'FF08'), int(z'FF09'), &
                                               int(z'FF3B'), int(z'FF3D'), int(z'FF5B'), int(z'FF5D')]
        ! Bracket pairs (the list after "Symbols and digits").
        integer, parameter :: pairs(*) = [int(z'2045'), int(z'2046'), int(z'207D'), int(z'207E'), &
                                          int(z'208D'), int(z'208E'), int(z'2329'), int(z'232A'), &
                                          int(z'23B4'), int(z'23B5'), int(z'2768'), int(z'2769'), &
                                          int(z'276A'), int(z'276B'), int(z'276C'), int(z'276D'), &
                                          int(z'2770'), int(z'2771'), int(z'2772'), int(z'2773'), &
                                          int(z'2774'), int(z'2775'), int(z'27E6'), int(z'27E7'), &
                                          int(z'27E8'), int(z'27E9'), int(z'27EA'), int(z'27EB'), &
                                          int(z'2983'), int(z'2984'), int(z'2985'), int(z'2986'), &
                                          int(z'2987'), int(z'2988'), int(z'2989'), int(z'298A'), &
                                          int(z'298B'), int(z'298C'), int(z'298D'), int(z'298E'), &
                                          int(z'298F'), int(z'2990'), int(z'2991'), int(z'2992'), &
                                          int(z'2993'), int(z'2994'), int(z'2995'), int(z'2996'), &
                                          int(z'2997'), int(z'2998'), int(z'29FC'), int(z'29FD'), &
                                          int(z'3008'), int(z'3009'), int(z'300A'), int(z'300B'), &
                                          int(z'300C'), int(z'300D'), int(z'300E'), int(z'300F'), &
                                          int(z'3010'), int(z'3011'), int(z'3014'), int(z'3015'), &
                                          int(z'3016'), int(z'3017'), int(z'3018'), int(z'3019'), &
                                          int(z'301A'), int(z'301B'), int(z'FD3E'), int(z'FD3F'), &
                                          int(z'FE35'), int(z'FE36'), int(z'FE37'), int(z'FE38'), &
                                          int(z'FE39'), int(z'FE3A'), int(z'FE3B'), int(z'FE3C'), &
                                          int(z'FE3D'), int(z'FE3E'), int(z'FE3F'), int(z'FE40'), &
                                          int(z'FE41'), int(z'FE42'), int(z'FE43'), int(z'FE44'), &
                                          int(z'FE59'), int(z'FE5A'), int(z'FE5B'), int(z'FE5C'), &
                                          int(z'FE5D'), int(z'FE5E'), int(z'FF08'), int(z'FF09'), &
                                          int(z'FF3B'), int(z'FF3D'), int(z'FF5B'), int(z'FF5D'), &
                                          int(z'FF5F'), int(z'FF60'), int(z'FF62'), int(z'FF63')]
        ! Tibetan punctuation (the deflist entries with "." syntax).
        integer, parameter :: tibetan(*, *) = reshape([ &
                                                      int(z'0F00'), int(z'0F0B'), int(z'0F0D'), int(z'0F12'), &
                                                      int(z'0F13'), int(z'0F18'), int(z'0F1A'), int(z'0F1F'), &
                                                      int(z'0F34'), int(z'0F34'), int(z'0F36'), int(z'0F36'), &
                                                      int(z'0F38'), int(z'0F3F'), int(z'0F7F'), int(z'0F7F'), &
                                                      int(z'0F85'), int(z'0F85'), int(z'0FBE'), int(z'0FCF')], &
                                                      [2, 10])
        integer :: k

        ! JISX0201-Kana.
        call entry(int(z'FF61'), int(z'FF61'), ".")
        call entry(int(z'FF64'), int(z'FF65'), ".")
        ! (Upstream makes both of these open parentheses.)
        call entry(int(z'FF62'), int(z'FF62'), "("//utf8(int(z'FF63')))
        call entry(int(z'FF63'), int(z'FF63'), "("//utf8(int(z'FF62')))

        do k = 1, size(cjk_parens), 2
            call pair(cjk_parens(k), cjk_parens(k + 1))
        end do

        ! Cyrillic: the numero sign.
        call entry(int(z'2116'), int(z'2116'), ".")
        ! Ethiopic punctuation.
        call entry(int(z'1361'), int(z'1368'), ".")
        ! Hebrew: maqaf, paseq, sof pasuq, nun hafukha, geresh, gershayim.
        call entry(int(z'05BE'), int(z'05BE'), ".")
        call entry(int(z'05C0'), int(z'05C0'), ".")
        call entry(int(z'05C3'), int(z'05C3'), ".")
        call entry(int(z'05C6'), int(z'05C6'), ".")
        call entry(int(z'05F3'), int(z'05F4'), ".")
        ! Lao and Thai symbols.
        call entry(int(z'0EAF'), int(z'0EAF'), "_")
        call entry(int(z'0EC6'), int(z'0EC6'), "_")
        call entry(int(z'0E2F'), int(z'0E2F'), "_")
        call entry(int(z'0E46'), int(z'0E46'), "_")
        call entry(int(z'0E3F'), int(z'0E3F'), "_")
        call entry(int(z'0E4F'), int(z'0E4F'), "_")
        call entry(int(z'0E5A'), int(z'0E5B'), "_")

        do k = 1, size(tibetan, 2)
            call entry(tibetan(1, k), tibetan(2, k), ".")
        end do

        ! Tai Viet symbols.
        call entry(int(z'AADB'), int(z'AADF'), "_")
        ! Latin-1.
        call entry(int(z'A1'), int(z'A1'), ".")
        call entry(int(z'A6'), int(z'A6'), "_")
        call entry(int(z'A7'), int(z'A7'), ".")
        call entry(int(z'A9'), int(z'A9'), "_")
        call entry(int(z'AB'), int(z'AB'), ".")
        call entry(int(z'BB'), int(z'BB'), ".")
        call entry(int(z'AC'), int(z'AE'), "_")
        call entry(int(z'B0'), int(z'B1'), "_")
        call entry(int(z'B5'), int(z'B5'), "_")
        call entry(int(z'B7'), int(z'B7'), "_")
        call entry(int(z'BC'), int(z'BE'), "_")
        call entry(int(z'BF'), int(z'BF'), ".")
        call entry(int(z'D7'), int(z'D7'), "_")
        call entry(int(z'DF'), int(z'DF'), "w")
        call entry(int(z'F7'), int(z'F7'), "_")
        ! Space characters (section 6.2 of the Unicode Standard).
        call entry(int(z'A0'), int(z'A0'), " ")
        call entry(int(z'2000'), int(z'200B'), " ")
        call entry(int(z'202F'), int(z'202F'), " ")
        call entry(int(z'205F'), int(z'205F'), " ")
        call entry(int(z'3000'), int(z'3000'), " ")
        ! General punctuation.
        call entry(int(z'200C'), int(z'2027'), ".")
        call entry(int(z'2030'), int(z'205E'), ".")
        call entry(int(z'2039'), int(z'203A'), "_")
        call entry(int(z'2044'), int(z'2044'), "_")
        call entry(int(z'2052'), int(z'2052'), "_")
        ! Arrows, mathematical operators, miscellaneous technical, control
        ! pictures.
        call entry(int(z'2190'), int(z'244F'), "_")
        ! Supplemental mathematical operators; miscellaneous symbols and
        ! arrows.
        call entry(int(z'2A00'), int(z'2AFF'), "_")
        call entry(int(z'2B00'), int(z'2BFF'), "_")
        ! Traditional Mongolian.
        call entry(int(z'1800'), int(z'180A'), ".")
        call entry(int(z'11660'), int(z'1166C'), ".")
        ! Supplemental punctuation; ideographic punctuation.
        call entry(int(z'2E00'), int(z'2E7F'), ".")
        call entry(int(z'3001'), int(z'3003'), ".")
        call entry(int(z'30FB'), int(z'30FB'), ".")
        ! Symbols for legacy computing.
        call entry(int(z'1FB00'), int(z'1FBCA'), "_")
        call entry(int(z'1FBCB'), int(z'1FBFF'), ".")
        ! Fullwidth and halfwidth Latin.
        call entry(int(z'FF01'), int(z'FF0F'), ".")
        call entry(int(z'FF04'), int(z'FF04'), "_")
        call entry(int(z'FF0B'), int(z'FF0B'), "_")
        call entry(int(z'FF1A'), int(z'FF1B'), ".")
        call entry(int(z'FF1F'), int(z'FF20'), ".")
        call entry(int(z'FF64'), int(z'FF65'), ".")
        call entry(int(z'FF61'), int(z'FF61'), ".")

        ! Letters (Lu, Ll, Lt) are words; so are Roman numerals I to XII
        ! and circled a to z, as they used to be.
        do k = 1, cased_letter_runs
            call entry(cased_letter_starts(k), cased_letter_ends(k), "w")
        end do

        call entry(int(z'2160'), int(z'216B'), "w")
        call entry(int(z'24D0'), int(z'24E9'), "w")

        do k = 1, size(pairs), 2
            call pair(pairs(k), pairs(k + 1))
        end do

    contains

        !> `open' and `close' as matching parentheses.
        subroutine pair(open, close)
            integer, intent(in) :: open, close

            call entry(open, open, "("//utf8(close))
            call entry(close, close, ")"//utf8(open))
        end subroutine pair
    end subroutine characters_syntax

    !> The UTF-8 encoding of character `c'.
    pure function utf8(c) result(s)
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
    end function utf8

    !> Whether character `c' has category `category' (one of ^, C, H, K,
    !> l and | so far; false for others).
    pure logical function char_has_category(c, category)
        integer, intent(in) :: c
        character, intent(in) :: category

        select case (category)
        case ("^")
            ! Combining diacritics and marks, and every nonspacing mark.
            char_has_category = in(c, int(z'300'), int(z'362')) .or. in(c, int(z'20D0'), int(z'20FF')) .or. &
                                in_runs(c, nonspacing_mark_starts, nonspacing_mark_ends, nonspacing_mark_runs)
        case ("C")
            ! Han, and the kanji-like 々 〆 〇.
            char_has_category = in(c, int(z'3400'), int(z'4DBF')) .or. in(c, int(z'4E00'), int(z'9FFF')) .or. &
                                in(c, int(z'F900'), int(z'FAFF')) .or. in(c, int(z'20000'), int(z'2FFFF')) .or. &
                                in(c, int(z'30000'), int(z'323AF')) .or. in(c, int(z'3005'), int(z'3007'))
        case ("K")
            char_has_category = in(c, int(z'3099'), int(z'309C')) .or. in(c, int(z'30A0'), int(z'30FF')) .or. &
                                in(c, int(z'31F0'), int(z'31FF')) .or. in(c, int(z'1AFF0'), int(z'1B000')) .or. &
                                in(c, int(z'1B120'), int(z'1B122')) .or. in(c, int(z'1B164'), int(z'1B167'))
        case ("H")
            char_has_category = in(c, int(z'3040'), int(z'309F')) .or. c == int(z'30A0') .or. c == int(z'30FC') .or. &
                                in(c, int(z'1B001'), int(z'1B11F')) .or. in(c, int(z'1B150'), int(z'1B152'))
        case ("l")
            ! Latin: ASCII graphic characters, U+0080..U+02B8, Latin
            ! Extended Additional, circled Latin, Latin Extended-C, D, E
            ! and G, and the fullwidth Latin letters (characters.el; the
            ! VISCII charsets add nothing outside these).
            char_has_category = in(c, 32, 127) .or. in(c, int(z'80'), int(z'2B8')) .or. &
                                in(c, int(z'1E00'), int(z'1EF9')) .or. in(c, int(z'24B6'), int(z'24E9')) .or. &
                                in(c, int(z'2C60'), int(z'2C7F')) .or. in(c, int(z'A720'), int(z'A7FF')) .or. &
                                in(c, int(z'AB30'), int(z'AB64')) .or. in(c, int(z'FF21'), int(z'FF3A')) .or. &
                                in(c, int(z'FF41'), int(z'FF5A')) .or. in(c, int(z'1DF00'), int(z'1DFFF'))
        case ("|")
            ! Line breakable: the CJK ranges, the Tibetan line-break
            ! characters, fullwidth characters and the Unicode equivalents of
            ! JISX0201-kana (characters.el; the kana blocks given `|'
            ! there are inside U+2E80..U+312F).
            char_has_category = in(c, int(z'2E80'), int(z'312F')) .or. in(c, int(z'3190'), int(z'33FF')) .or. &
                                in(c, int(z'3400'), int(z'9FFF')) .or. in(c, int(z'F900'), int(z'FAFF')) .or. &
                                in(c, int(z'20000'), int(z'2FFFF')) .or. in(c, int(z'30000'), int(z'323AF')) .or. &
                                c == int(z'F0B') .or. in(c, int(z'F0D'), int(z'F12')) .or. c == int(z'F14') .or. &
                                c == int(z'F7F') .or. in(c, int(z'FF01'), int(z'FF9F'))
        case (" ")
            ! Space for indent: characters.el gives it to no character.
            char_has_category = .false.
        case default
            char_has_category = .false.
        end select

    contains

        pure logical function in(x, lo, hi)
            integer, intent(in) :: x, lo, hi

            in = lo <= x .and. x <= hi
        end function in
    end function char_has_category

    !> Whether `c' is in one of the sorted runs `starts(k)'..`ends(k)'.
    pure logical function in_runs(c, starts, ends, n)
        integer, intent(in) :: c, n
        integer, intent(in) :: starts(:), ends(:)
        integer :: lo, hi, mid

        in_runs = .false.
        lo = 1
        hi = n

        do while (lo <= hi)
            mid = (lo + hi)/2

            if (c < starts(mid)) then
                hi = mid - 1
            else if (c > ends(mid)) then
                lo = mid + 1
            else
                in_runs = .true.
                return
            end if
        end do
    end function in_runs
end module characters_fm
