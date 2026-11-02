! characters_mod.f90 -- syntax and categories of non-ASCII characters.
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

!> The parts of characters.el that word motion, filling, indentation
!> and regexps need: the entries it makes in the standard syntax table
!> for Unicode characters, in its order (later entries override earlier
!> ones), and the categories that `word_combining_categories',
!> `word_separating_categories', filling (|) and indentation (space)
!> refer to, with their characters (^ combining, C han, H hiragana, K
!> katakana, l Latin), for the standard category table (category_mod).
!> The case entries are in casetab_mod (from uni_case_mod).
!>
!> Deferred: entries made through legacy charsets (`map_charset_chars'
!> over GB2312, Big5, JIS X 0208/0212, KS C 5601 and so on, which need
!> their mapping tables), the other categories, and everything else
!> characters.el sets up.
module characters_mod
    use uni_category_mod, only: cased_letter_runs, cased_letter_starts, cased_letter_ends, &
                                nonspacing_mark_runs, nonspacing_mark_starts, nonspacing_mark_ends
    use charset_mod, only: set_charset_attributes
    implicit none(type, external)
    private

    public :: syntax_entry_procedure, characters_syntax, category_define_procedure, category_entry_procedure, characters_categories
    public :: characters_preferred_coding_systems

    abstract interface
        !> Give the characters `from' to `to' the syntax `descriptor'
        !> (as for `modify_syntax_entry').  Not a port.
        subroutine syntax_entry_procedure(from, to, descriptor)
            implicit none(type, external)
            integer, intent(in) :: from, to
            character(len=*), intent(in) :: descriptor
        end subroutine syntax_entry_procedure

        !> Define category `category' with `docstring' (as for
        !> `define_category').  Not a port.
        recursive subroutine category_define_procedure(category, docstring)
            implicit none(type, external)
            character, intent(in) :: category
            character(len=*), intent(in) :: docstring
        end subroutine category_define_procedure

        !> Add category `category' to the characters `from' to `to' (as
        !> for `modify_category_entry').  Not a port.
        recursive subroutine category_entry_procedure(from, to, category)
            implicit none(type, external)
            integer, intent(in) :: from, to
            character, intent(in) :: category
        end subroutine category_entry_procedure
    end interface

contains

    !> Make characters.el's standard-syntax-table entries through
    !> `entry', in characters.el's order.  Not a port.
    subroutine characters_syntax(entry)
        procedure(syntax_entry_procedure) :: entry
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

    !> The UTF-8 encoding of character `c'.  Not a port.
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

    !> Define, through `define', the categories characters.el defines
    !> that are ported (in its order), and give them, through `entry',
    !> its characters: ^ combining, C han, H hiragana, K katakana, l
    !> Latin, | line breakable, and space for indent (which it gives to no
    !> character).  Not a port.
    subroutine characters_categories(define, entry)
        procedure(category_define_procedure) :: define
        procedure(category_entry_procedure) :: entry
        character(len=*), parameter :: nl = new_line("a")
        integer :: k

        call define("l", "Latin")
        call define("C", "2-byte han"//nl//"Chinese (Han) characters of 2-byte character sets")
        call define("H", "2-byte Hiragana"//nl//"Japanese Hiragana characters of 2-byte character sets")
        call define("K", "2-byte Katakana"//nl//"Japanese Katakana characters of 2-byte character sets")
        call define("|", "line breakable"//nl//"While filling, we can break a line at this character.")
        call define(" ", "space for indent"//nl//"This character counts as a space for indentation purposes.")
        call define("^", "Combining"//nl//"Combining diacritic or mark (Unicode General Category M)")
        ! Combining diacritics and marks, and every nonspacing mark.
        call entry(int(z'300'), int(z'362'), "^")
        call entry(int(z'20D0'), int(z'20FF'), "^")

        do k = 1, nonspacing_mark_runs
            call entry(nonspacing_mark_starts(k), nonspacing_mark_ends(k), "^")
        end do

        ! Han, and the kanji-like 々 〆 〇.
        call entry(int(z'3400'), int(z'4DBF'), "C")
        call entry(int(z'4E00'), int(z'9FFF'), "C")
        call entry(int(z'F900'), int(z'FAFF'), "C")
        call entry(int(z'20000'), int(z'2FFFF'), "C")
        call entry(int(z'30000'), int(z'323AF'), "C")
        call entry(int(z'3005'), int(z'3007'), "C")
        call entry(int(z'3099'), int(z'309C'), "K")
        call entry(int(z'30A0'), int(z'30FF'), "K")
        call entry(int(z'31F0'), int(z'31FF'), "K")
        call entry(int(z'1AFF0'), int(z'1B000'), "K")
        call entry(int(z'1B120'), int(z'1B122'), "K")
        call entry(int(z'1B164'), int(z'1B167'), "K")
        call entry(int(z'3040'), int(z'309F'), "H")
        call entry(int(z'30A0'), int(z'30A0'), "H")
        call entry(int(z'30FC'), int(z'30FC'), "H")
        call entry(int(z'1B001'), int(z'1B11F'), "H")
        call entry(int(z'1B150'), int(z'1B152'), "H")
        ! Latin: ASCII graphic characters, U+0080..U+02B8, Latin Extended
        ! Additional, circled Latin, Latin Extended-C, D, E and G, and the
        ! fullwidth Latin letters (the VISCII charsets add nothing outside
        ! these).
        call entry(32, 127, "l")
        call entry(int(z'80'), int(z'2B8'), "l")
        call entry(int(z'1E00'), int(z'1EF9'), "l")
        call entry(int(z'24B6'), int(z'24E9'), "l")
        call entry(int(z'2C60'), int(z'2C7F'), "l")
        call entry(int(z'A720'), int(z'A7FF'), "l")
        call entry(int(z'AB30'), int(z'AB64'), "l")
        call entry(int(z'FF21'), int(z'FF3A'), "l")
        call entry(int(z'FF41'), int(z'FF5A'), "l")
        call entry(int(z'1DF00'), int(z'1DFFF'), "l")
        ! Line breakable: the CJK ranges, the Tibetan line-break characters,
        ! fullwidth characters and the Unicode equivalents of JISX0201-kana
        ! (the kana blocks given `|' there are inside U+2E80..U+312F).
        call entry(int(z'2E80'), int(z'312F'), "|")
        call entry(int(z'3190'), int(z'33FF'), "|")
        call entry(int(z'3400'), int(z'9FFF'), "|")
        call entry(int(z'F900'), int(z'FAFF'), "|")
        call entry(int(z'20000'), int(z'2FFFF'), "|")
        call entry(int(z'30000'), int(z'323AF'), "|")
        call entry(int(z'F0B'), int(z'F0B'), "|")
        call entry(int(z'F0D'), int(z'F12'), "|")
        call entry(int(z'F14'), int(z'F14'), "|")
        call entry(int(z'F7F'), int(z'F7F'), "|")
        call entry(int(z'FF01'), int(z'FF9F'), "|")
    end subroutine characters_categories

    !> For each character set, set its `preferred-coding-system' attribute:
    !> the most proper coding system to encode it.
    !> Port of characters.el (put-charset-property).
    subroutine characters_preferred_coding_systems()
        call set_charset_attributes("latin-iso8859-1", preferred_coding_system="iso-latin-1")
        call set_charset_attributes("latin-iso8859-2", preferred_coding_system="iso-latin-2")
        call set_charset_attributes("latin-iso8859-3", preferred_coding_system="iso-latin-3")
        call set_charset_attributes("latin-iso8859-4", preferred_coding_system="iso-latin-4")
        call set_charset_attributes("thai-tis620", preferred_coding_system="thai-tis620")
        call set_charset_attributes("greek-iso8859-7", preferred_coding_system="greek-iso-8bit")
        call set_charset_attributes("arabic-iso8859-6", preferred_coding_system="iso-2022-7bit")
        call set_charset_attributes("hebrew-iso8859-8", preferred_coding_system="hebrew-iso-8bit")
        call set_charset_attributes("katakana-jisx0201", preferred_coding_system="japanese-shift-jis")
        call set_charset_attributes("latin-jisx0201", preferred_coding_system="japanese-shift-jis")
        call set_charset_attributes("cyrillic-iso8859-5", preferred_coding_system="cyrillic-iso-8bit")
        call set_charset_attributes("latin-iso8859-9", preferred_coding_system="iso-latin-5")
        call set_charset_attributes("japanese-jisx0208-1978", preferred_coding_system="iso-2022-jp")
        call set_charset_attributes("chinese-gb2312", preferred_coding_system="chinese-iso-8bit")
        call set_charset_attributes("chinese-gbk", preferred_coding_system="chinese-gbk")
        call set_charset_attributes("gb18030-2-byte", preferred_coding_system="chinese-gb18030")
        call set_charset_attributes("gb18030-4-byte-bmp", preferred_coding_system="chinese-gb18030")
        call set_charset_attributes("gb18030-4-byte-smp", preferred_coding_system="chinese-gb18030")
        call set_charset_attributes("gb18030-4-byte-ext-1", preferred_coding_system="chinese-gb18030")
        call set_charset_attributes("gb18030-4-byte-ext-2", preferred_coding_system="chinese-gb18030")
        call set_charset_attributes("japanese-jisx0208", preferred_coding_system="iso-2022-jp")
        call set_charset_attributes("korean-ksc5601", preferred_coding_system="iso-2022-kr")
        call set_charset_attributes("japanese-jisx0212", preferred_coding_system="iso-2022-jp")
        call set_charset_attributes("chinese-big5-1", preferred_coding_system="chinese-big5")
        call set_charset_attributes("chinese-big5-2", preferred_coding_system="chinese-big5")
        call set_charset_attributes("chinese-sisheng", preferred_coding_system="iso-2022-7bit")
        call set_charset_attributes("ipa", preferred_coding_system="iso-2022-7bit")
        call set_charset_attributes("vietnamese-viscii-lower", preferred_coding_system="vietnamese-viscii")
        call set_charset_attributes("vietnamese-viscii-upper", preferred_coding_system="vietnamese-viscii")
        call set_charset_attributes("arabic-digit", preferred_coding_system="iso-2022-7bit")
        call set_charset_attributes("arabic-1-column", preferred_coding_system="iso-2022-7bit")
        call set_charset_attributes("lao", preferred_coding_system="lao")
        call set_charset_attributes("arabic-2-column", preferred_coding_system="iso-2022-7bit")
        call set_charset_attributes("indian-is13194", preferred_coding_system="devanagari")
        call set_charset_attributes("indian-glyph", preferred_coding_system="devanagari")
        call set_charset_attributes("tibetan-1-column", preferred_coding_system="tibetan")
        call set_charset_attributes("ethiopic", preferred_coding_system="iso-2022-7bit")
        call set_charset_attributes("chinese-cns11643-1", preferred_coding_system="iso-2022-cn")
        call set_charset_attributes("chinese-cns11643-2", preferred_coding_system="iso-2022-cn")
        call set_charset_attributes("chinese-cns11643-3", preferred_coding_system="iso-2022-cn")
        call set_charset_attributes("chinese-cns11643-4", preferred_coding_system="iso-2022-cn")
        call set_charset_attributes("chinese-cns11643-5", preferred_coding_system="iso-2022-cn")
        call set_charset_attributes("chinese-cns11643-6", preferred_coding_system="iso-2022-cn")
        call set_charset_attributes("chinese-cns11643-7", preferred_coding_system="iso-2022-cn")
        call set_charset_attributes("indian-2-column", preferred_coding_system="devanagari")
        call set_charset_attributes("tibetan", preferred_coding_system="tibetan")
        call set_charset_attributes("latin-iso8859-14", preferred_coding_system="iso-latin-8")
        call set_charset_attributes("latin-iso8859-15", preferred_coding_system="iso-latin-9")
    end subroutine characters_preferred_coding_systems
end module characters_mod
