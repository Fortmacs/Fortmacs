! japanese_fm.f90 -- the japanese coding systems of type `charset', `shift-jis', `big5' and `iso-2022'.
! Ported from GNU Emacs 31.1 lisp/language/japanese.el.
!
! Copyright (C) 1997, 2001-2026 Free Software Foundation, Inc.
! Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
!   2005, 2006, 2007, 2008, 2009, 2010, 2011
!   National Institute of Advanced Industrial Science and Technology (AIST)
!   Registration Number H14PRO021
! Copyright (C) 2003
!   National Institute of Advanced Industrial Science and Technology (AIST)
!   Registration Number H13PRO009
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

!> The coding systems of type `charset', `shift-jis', `big5' and `iso-2022' that japanese.el defines, with their
!> aliases (its other coding systems and its language environments are
!> Deferred).  A submodule of coding_fm, which defines them when it
!> starts.
submodule(coding_fm) japanese_fm
    use mule_fm, only: set_language_info_alist
    implicit none(type, external)

contains

    !> Port of the `charset' coding systems of japanese.el.
    module subroutine syms_of_japanese()
        call define_iso_2022_coding_system("iso-2022-jp", iachar("J"), &
                                           doc="ISO 2022 based 7bit encoding for Japanese (MIME:ISO-2022-JP)" &
                                           //".", &
                                           initial=[character(len=CHARSET_NAME_LEN) :: "ascii", "", "", ""], &
                                       request_charsets=[character(len=CHARSET_NAME_LEN) :: "latin-jisx0201", "japanese-jisx0208", &
                                                             "japanese-jisx0208-1978", "ascii"], &
                                           request_registers=[integer :: 0, 0, 0, 0], &
                                           reg_usage=[4, 4], &
                                    flags=[character(len=16) :: "short", "ascii-at-eol", "ascii-at-cntl", "7-bit", "designation"], &
                                           charsets=[character(len=CHARSET_NAME_LEN) :: "ascii", "japanese-jisx0208", &
                                                     "japanese-jisx0208-1978", "latin-jisx0201"], &
                                           mime_charset="iso-2022-jp")
        call define_coding_system_alias("junet", "iso-2022-jp")
        call define_iso_2022_coding_system("iso-2022-jp-2", iachar("J"), &
                                           doc="ISO 2022 based 7bit encoding for CJK, Latin-1, Greek (MIME:I" &
                                           //"SO-2022-JP-2).", &
                                           initial=[character(len=CHARSET_NAME_LEN) :: "ascii", "", "", ""], &
                                        request_charsets=[character(len=CHARSET_NAME_LEN) :: "greek-iso8859-7", "latin-iso8859-1", &
                                                        "korean-ksc5601", "chinese-gb2312", "japanese-jisx0212", "latin-jisx0201", &
                                                             "japanese-jisx0208", "japanese-jisx0208-1978", "ascii"], &
                                           request_registers=[integer :: 2, 2, 0, 0, 0, 0, 0, 0, 0], &
                                           reg_usage=[4, 4], &
                                     flags=[character(len=16) :: "short", "ascii-at-eol", "ascii-at-cntl", "7-bit", "designation", &
                                                  "single-shift", "init-at-bol"], &
                                   charsets=[character(len=CHARSET_NAME_LEN) :: "ascii", "japanese-jisx0208", "japanese-jisx0212", &
                                                   "latin-jisx0201", "japanese-jisx0208-1978", "chinese-gb2312", "korean-ksc5601", &
                                                     "latin-iso8859-1", "greek-iso8859-7"], &
                                           mime_charset="iso-2022-jp-2")
        call define_charset_coding_system("japanese-shift-jis", iachar("S"), &
                                  charsets=[character(len=CHARSET_NAME_LEN) :: "ascii", "katakana-jisx0201", "japanese-jisx0208"], &
                                          mime_charset="shift_jis", &
                                          doc="Shift-JIS 8-bit encoding for Japanese (MIME:SHIFT_JIS)", &
                                          coding_type="shift-jis")
        call define_coding_system_alias("shift_jis", "japanese-shift-jis")
        call define_coding_system_alias("sjis", "japanese-shift-jis")
        call define_charset_coding_system("japanese-cp932", iachar("S"), &
                                          charsets=[character(len=CHARSET_NAME_LEN) :: "ascii", "katakana-sjis", "cp932-2-byte"], &
                                          doc="CP932 (Microsoft shift-jis)")
        call define_coding_system_alias("cp932", "japanese-cp932")
        call define_iso_2022_coding_system("japanese-iso-7bit-1978-irv", iachar("j"), &
                                           doc="ISO 2022 based 7-bit encoding for Japanese JISX0208-1978 and" &
                                           //" JISX0201-Roman.", &
                                           initial=[character(len=CHARSET_NAME_LEN) :: "latin-jisx0201", "", "", ""], &
                                    request_charsets=[character(len=CHARSET_NAME_LEN) :: "katakana-jisx0201", "japanese-jisx0212", &
                                                             "japanese-jisx0208", "japanese-jisx0208-1978", "latin-jisx0201"], &
                                           request_registers=[integer :: 0, 0, 0, 0, 0], &
                                           reg_usage=[4, 4], &
                                     flags=[character(len=16) :: "short", "ascii-at-eol", "ascii-at-cntl", "7-bit", "designation", &
                                                  "use-roman", "use-oldjis"], &
                                 charsets=[character(len=CHARSET_NAME_LEN) :: "ascii", "latin-jisx0201", "japanese-jisx0208-1978", &
                                                     "japanese-jisx0208", "japanese-jisx0212"])
        call define_coding_system_alias("iso-2022-jp-1978-irv", "japanese-iso-7bit-1978-irv")
        call define_coding_system_alias("old-jis", "japanese-iso-7bit-1978-irv")
        call define_iso_2022_coding_system("japanese-iso-8bit", iachar("E"), &
                                           doc="ISO 2022 based EUC encoding for Japanese (MIME:EUC-JP).", &
                                    initial=[character(len=CHARSET_NAME_LEN) :: "ascii", "japanese-jisx0208", "katakana-jisx0201", &
                                                    "japanese-jisx0212"], &
                                    request_charsets=[character(len=CHARSET_NAME_LEN) :: "japanese-jisx0212", "katakana-jisx0201", &
                                                             "japanese-jisx0208", "ascii"], &
                                           request_registers=[integer :: 3, 2, 1, 0], &
                                           reg_usage=[4, 4], &
                                           flags=[character(len=16) :: "short", "ascii-at-eol", "ascii-at-cntl", "single-shift"], &
                                      charsets=[character(len=CHARSET_NAME_LEN) :: "ascii", "latin-jisx0201", "japanese-jisx0208", &
                                                     "katakana-jisx0201", "japanese-jisx0212", "japanese-jisx0208-1978"], &
                                           mime_charset="euc-jp")
        call define_coding_system_alias("euc-japan-1990", "japanese-iso-8bit")
        call define_coding_system_alias("euc-japan", "japanese-iso-8bit")
        call define_coding_system_alias("euc-jp", "japanese-iso-8bit")
        call define_iso_2022_coding_system("iso-2022-jp-2004", iachar("J"), &
                                           doc="ISO 2022 based 7bit encoding for JIS X 0213:2004 (MIME:ISO-2" &
                                           //"022-JP-2004).", &
                                           initial=[character(len=CHARSET_NAME_LEN) :: "ascii", "", "", ""], &
                                request_charsets=[character(len=CHARSET_NAME_LEN) :: "japanese-jisx0213-2", "japanese-jisx0213-1", &
                                                             "japanese-jisx0213.2004-1", "japanese-jisx0208", "ascii"], &
                                           request_registers=[integer :: 0, 0, 0, 0, 0], &
                                           reg_usage=[4, 4], &
                                    flags=[character(len=16) :: "short", "ascii-at-eol", "ascii-at-cntl", "7-bit", "designation"], &
                                           charsets=[character(len=CHARSET_NAME_LEN) :: "ascii", "japanese-jisx0208", &
                                                     "japanese-jisx0213.2004-1", "japanese-jisx0213-1", "japanese-jisx0213-2"], &
                                           mime_charset="iso-2022-jp-2004")
        call define_coding_system_alias("iso-2022-jp-3", "iso-2022-jp-2004")
        call define_iso_2022_coding_system("euc-jis-2004", iachar("E"), &
                                           doc="ISO 2022 based EUC encoding for JIS X 0213 (MIME:EUC-JIS-200" &
                                           //"4).", &
                                           initial=[character(len=CHARSET_NAME_LEN) :: "ascii", "japanese-jisx0213.2004-1", &
                                                    "katakana-jisx0201", "japanese-jisx0213-2"], &
                                  request_charsets=[character(len=CHARSET_NAME_LEN) :: "japanese-jisx0213-2", "katakana-jisx0201", &
                                                             "japanese-jisx0213.2004-1", "ascii"], &
                                           request_registers=[integer :: 3, 2, 1, 0], &
                                           reg_usage=[4, 4], &
                                           flags=[character(len=16) :: "short", "ascii-at-eol", "ascii-at-cntl", "single-shift"], &
                               charsets=[character(len=CHARSET_NAME_LEN) :: "ascii", "latin-jisx0201", "japanese-jisx0213.2004-1", &
                                                     "japanese-jisx0213-1", "katakana-jisx0201", "japanese-jisx0213-2"], &
                                           mime_charset="euc-jis-2004")
        call define_coding_system_alias("euc-jisx0213", "euc-jis-2004")
        call define_charset_coding_system("japanese-shift-jis-2004", iachar("S"), &
                                          charsets=[character(len=CHARSET_NAME_LEN) :: "ascii", "katakana-jisx0201", &
                                                    "japanese-jisx0213.2004-1", "japanese-jisx0213-2"], &
                                          doc="Shift_JIS 8-bit encoding for Japanese (MIME:SHIFT_JIS-2004)", &
                                          coding_type="shift-jis")
        call define_coding_system_alias("shift_jis-2004", "japanese-shift-jis-2004")
        call define_charset_coding_system("ibm281", iachar("*"), &
                                          charsets=[character(len=CHARSET_NAME_LEN) :: "ibm281"], &
                                          doc="Japanese-E version of EBCDIC")
        call define_coding_system_alias("ebcdic-jp-e", "ibm281")
        call define_coding_system_alias("cp281", "ibm281")
        call define_charset_coding_system("ibm290", iachar("*"), &
                                          charsets=[character(len=CHARSET_NAME_LEN) :: "ibm290"], &
                                          doc="Japanese katakana version of EBCDIC")
        call define_coding_system_alias("ebcdic-jp-kana", "ibm290")
        call define_coding_system_alias("cp290", "ibm290")
    end subroutine syms_of_japanese

    !> Port of the language environments of japanese.el (set-language-info-alist).
    module subroutine language_info_japanese()
        call set_language_info_alist("Japanese", &
                                     documentation_t=.true., &
                                     charsets=[character(len=40) :: "japanese-jisx0208", "japanese-jisx0212", "latin-jisx0201", &
                                               "katakana-jisx0201", "japanese-jisx0213.2004-1", "japanese-jisx0213-1", &
                                               "japanese-jisx0213-2", "japanese-jisx0208-1978"], &
                                     sample_text="Japanese (日本語)"//achar(9)//"こんにちは, ｺﾝﾆﾁﾊ", &
                                    coding_systems=[character(len=40) :: "iso-2022-jp", "japanese-iso-8bit", "japanese-shift-jis", &
                                     "japanese-iso-7bit-1978-irv", "iso-2022-jp-2004", "japanese-shift-jis-2004", "euc-jis-2004"], &
                                   coding_priority=[character(len=40) :: "iso-2022-jp", "japanese-iso-8bit", "japanese-shift-jis", &
                                                  "iso-2022-jp-2004", "euc-jis-2004", "japanese-shift-jis-2004", "iso-2022-jp-2"], &
                                     input_method="japanese", &
                                     tutorial="TUTORIAL.ja", &
                                     iso639_language="ja")
    end subroutine language_info_japanese
end submodule japanese_fm
