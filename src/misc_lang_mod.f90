! misc_lang_mod.f90 -- the misc-lang coding systems of type `charset'.
! Ported from GNU Emacs 31.1 lisp/language/misc-lang.el.
!
! Copyright (C) 2012-2026 Free Software Foundation, Inc.
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

!> The coding systems of type `charset' that misc-lang.el defines, with their
!> aliases, and its language environments (its other coding systems are
!> Deferred).  A submodule of coding_mod, which defines the coding systems
!> when it starts and the language environments when mule_mod asks for them.
submodule(coding_mod) misc_lang_mod
    use mule_mod, only: define_language_environment
    implicit none(type, external)

contains

    !> Port of the `charset' coding systems of misc-lang.el.
    module subroutine init_misc_lang()
        call define_charset_coding_system("iso-8859-6", iachar("6"), &
                                          charset_names=[character(len=CHARSET_NAME_LEN) :: "iso-8859-6"], &
                                          mime_charset="iso-8859-6", &
                                          doc="ISO-8859-6 based encoding (MIME:ISO-8859-6).")
        call define_charset_coding_system("windows-1256", iachar("A"), &
                                          charset_names=[character(len=CHARSET_NAME_LEN) :: "windows-1256"], &
                                          mime_charset="windows-1256", &
                                          doc="windows-1256 (Arabic) encoding (MIME: WINDOWS-1256)")
        call define_coding_system_alias("cp1256", "windows-1256")
    end subroutine init_misc_lang

    !> Port of the language environments of misc-lang.el.
    module subroutine define_misc_lang_language_environments()
        call define_language_environment("IPA", &
                                         documentation="IPA is International Phonetic Alphabet for English, French, " &
                                         //"German"//achar(10)//"and Italian.", &
                                         charsets=[character(len=40) :: "ipa"], &
                                         coding_systems=[character(len=40) :: "utf-8"], &
                                         coding_system_order=[character(len=40) :: "utf-8"], &
                                         nonascii_translation="ipa", &
                                         input_method="ipa")
        call define_language_environment("Arabic", &
                                         documentation="Bidirectional editing is supported.", &
                                         charsets=[character(len=40) :: "unicode"], &
                                         sample_text="Arabic"//achar(9)//"السّلام عليكم", &
                                         coding_systems=[character(len=40) :: "utf-8", "iso-8859-6", "windows-1256"], &
                                         coding_system_order=[character(len=40) :: "utf-8", "iso-8859-6", "windows-1256"], &
                                         input_method="arabic")
        call define_language_environment("Persian", &
                                         documentation="Bidirectional editing is supported.", &
                                         charsets=[character(len=40) :: "unicode"], &
                                         sample_text="Persian"//achar(9)//"فارسی", &
                                         coding_systems=[character(len=40) :: "utf-8", "iso-8859-6", "windows-1256"], &
                                         coding_system_order=[character(len=40) :: "utf-8", "iso-8859-6", "windows-1256"], &
                                         input_method="farsi-transliterate-banan", &
                                         tutorial="TUTORIAL.fa")
        call define_language_environment("Syriac", &
                                         documentation="Language environment for Classical Syriac, Aramaic, and othe" &
                                         //"r languages"//achar(10)//"using the Syriac script.", &
                                         charsets=[character(len=40) :: "unicode"], &
                                         sample_text="Syriac"//achar(9)//"ܫܠܡܐ", &
                                         coding_systems=[character(len=40) :: "utf-8"], &
                                         coding_system_order=[character(len=40) :: "utf-8"], &
                                         input_method="syriac")
        call define_language_environment("Hanifi Rohingya", &
                                         documentation="Rohingya language and its script Hanifi Rohingya are support" &
                                         //"ed"//achar(10)//"in this language environment.", &
                                         charsets=[character(len=40) :: "unicode"], &
                                         sample_text="Hanifi Rohingya (𐴌𐴟𐴇" &
                                         //"𐴥𐴝𐴚𐴒𐴙𐴝 𐴇𐴝𐴕𐴞𐴉𐴞 𐴓"//"𐴠𐴑𐴤𐴝)"//achar(9) &
                                         //"𐴀𐴝𐴏𐴓𐴝𐴀𐴡𐴤𐴛𐴝𐴓𐴝𐴙𐴑𐴟"//"𐴔", &
                                         coding_systems=[character(len=40) :: "utf-8"], &
                                         coding_system_order=[character(len=40) :: "utf-8"], &
                                         input_method="hanifi-rohingya")
        call define_language_environment("Kharoshthi", &
                                         documentation="Language environment for Gāndhārī, Sanskrit, and other langu" &
                                         //"ages"//achar(10)//"using the Kharoṣṭhī script.", &
                                         charsets=[character(len=40) :: "unicode"], &
                                         sample_text="Kharoṣṭhī (𐨑𐨪𐨆𐨛𐨁)"//achar(9) &
                                         //"𐨣𐨨𐨲𐨪𐨆 𐨐𐨪𐨅𐨨𐨁", &
                                         coding_systems=[character(len=40) :: "utf-8"], &
                                         coding_system_order=[character(len=40) :: "utf-8"], &
                                         input_method="kharoshthi")
        call define_language_environment("Adlam", &
                                        documentation="Fulani language and its script Adlam are supported"//achar(10)//"in this l" &
                                         //"anguage environment.", &
                                         charsets=[character(len=40) :: "unicode"], &
                                         sample_text="Adlam (𞤀𞤣𞤤𞤢𞤥)"//achar(9)//"𞤅𞤢𞤤𞤢𞥄𞤥", &
                                         coding_systems=[character(len=40) :: "utf-8"], &
                                         coding_system_order=[character(len=40) :: "utf-8"], &
                                         input_method="adlam")
        call define_language_environment("Mende Kikakui", &
                                        documentation="Mende language and its script Kikakui are supported"//achar(10)//"in this " &
                                         //"language environment.", &
                                         charsets=[character(len=40) :: "unicode"], &
                                         sample_text="Mende Kikakui (𞠀𞠁𞠂)"//achar(9)//"𞠛𞠉", &
                                         coding_systems=[character(len=40) :: "utf-8"], &
                                         coding_system_order=[character(len=40) :: "utf-8"], &
                                         input_method="mende-kikakui")
        call define_language_environment("Gothic", &
                                         documentation="Ancient Gothic language using the Gothic script is supported" &
                                         //" in this"//achar(10)//"language environment.", &
                                         charsets=[character(len=40) :: "unicode"], &
                                         sample_text="Gothic (𐌲𐌿𐍄𐌹𐍃𐌺𐌰)"//achar(9) &
                                         //"𐌷𐌰𐌹𐌻𐍃 / 𐌷𐌰𐌹𐌻𐌰", &
                                         coding_systems=[character(len=40) :: "utf-8"], &
                                         coding_system_order=[character(len=40) :: "utf-8"], &
                                         input_method="gothic")
        call define_language_environment("Coptic", &
                                         documentation="Coptic language using the Coptic script is supported in this" &
                                         //achar(10)//"language environment.", &
                                         charsets=[character(len=40) :: "unicode"], &
                                         sample_text="Coptic (ⲘⲉⲧⲢⲉⲙ̀ⲛⲭⲏⲙⲓ)"//achar(9)// &
                                         "Ⲛⲟⲩϥⲣⲓ", &
                                         coding_systems=[character(len=40) :: "utf-8"], &
                                         coding_system_order=[character(len=40) :: "utf-8"], &
                                         input_method="coptic")
        call define_language_environment("Mongolian-traditional", &
                                         documentation="Support for Mongolian language with traditional script.", &
                                         sample_text="Mongolian Traditiona" &
                                         //"l (ᠮᠣᠩᠭᠣᠯ ᠪᠢᠴᠢᠭ᠋)"//achar(9) &
                                         //"ᠰᠠᠶᠢᠨ ᠪᠠᠶᠢᠨ᠎ᠠ ᠤᠤ?", &
                                         coding_systems=[character(len=40) :: "utf-8"], &
                                         coding_system_order=[character(len=40) :: "utf-8"])
        call define_language_environment("Tifinagh", &
                                         documentation="Tifinagh a script used to write the Berber languages.", &
                                         charsets=[character(len=40) :: "unicode"], &
                                         sample_text="Tifinagh"//achar(9)//"ⴰⵣⵓⵍ", &
                                         coding_systems=[character(len=40) :: "utf-8"], &
                                         coding_system_order=[character(len=40) :: "utf-8"], &
                                         input_method="tifinagh")
    end subroutine define_misc_lang_language_environments
end submodule misc_lang_mod
