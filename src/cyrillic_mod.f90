! cyrillic_mod.f90 -- the cyrillic coding systems of type `charset'.
! Ported from GNU Emacs 31.1 lisp/language/cyrillic.el.
!
! Copyright (C) 1997-1998, 2001-2026 Free Software Foundation, Inc.
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

!> The coding systems of type `charset' that cyrillic.el defines, with their
!> aliases, and its language environments (its other coding systems are
!> Deferred).  A submodule of coding_mod, which defines the coding systems
!> when it starts and the language environments when mule_mod asks for them.
submodule(coding_mod) cyrillic_mod
    use mule_mod, only: define_language_environment
    implicit none(type, external)

contains

    !> Port of the `charset' coding systems of cyrillic.el.
    module subroutine init_cyrillic()
        call define_charset_coding_system("cyrillic-iso-8bit", iachar("5"), &
                                          charset_names=[character(len=CHARSET_NAME_LEN) :: "iso-8859-5"], &
                                          mime_charset="iso-8859-5", &
                                          doc="ISO 2022 based 8-bit encoding for Cyrillic script (MIME:ISO-" &
                                          //"8859-5).")
        call define_coding_system_alias("iso-8859-5", "cyrillic-iso-8bit")
        call define_charset_coding_system("cyrillic-koi8", iachar("R"), &
                                          charset_names=[character(len=CHARSET_NAME_LEN) :: "koi8"], &
                                          mime_charset="koi8-r", &
                                          doc="KOI8 8-bit encoding for Cyrillic (MIME: KOI8-R).")
        call define_coding_system_alias("koi8-r", "cyrillic-koi8")
        call define_coding_system_alias("koi8", "cyrillic-koi8")
        call define_coding_system_alias("cp878", "cyrillic-koi8")
        call define_charset_coding_system("koi8-u", 1059, &
                                          charset_names=[character(len=CHARSET_NAME_LEN) :: "koi8-u"], &
                                          mime_charset="koi8-u", &
                                          doc="KOI8-U 8-bit encoding for Cyrillic (MIME: KOI8-U)")
        call define_charset_coding_system("cyrillic-alternativnyj", iachar("A"), &
                                          charset_names=[character(len=CHARSET_NAME_LEN) :: "alternativnyj"], &
                                          doc="ALTERNATIVNYJ 8-bit encoding for Cyrillic.")
        call define_coding_system_alias("alternativnyj", "cyrillic-alternativnyj")
        call define_charset_coding_system("cp866", iachar("*"), &
                                          charset_names=[character(len=CHARSET_NAME_LEN) :: "ibm866"], &
                                          mime_charset="cp866", &
                                          doc="CP866 encoding for Cyrillic.")
        call define_charset_coding_system("koi8-t", iachar("*"), &
                                          charset_names=[character(len=CHARSET_NAME_LEN) :: "koi8-t"], &
                                          mime_charset="koi8-t", &
                                          doc="KOI8-T 8-bit encoding for Cyrillic")
        call define_charset_coding_system("windows-1251", iachar("b"), &
                                          charset_names=[character(len=CHARSET_NAME_LEN) :: "windows-1251"], &
                                          mime_charset="windows-1251", &
                                          doc="windows-1251 8-bit encoding for Cyrillic (MIME: WINDOWS-1251" &
                                          //")")
        call define_coding_system_alias("cp1251", "windows-1251")
        call define_charset_coding_system("cp1125", iachar("*"), &
                                          charset_names=[character(len=CHARSET_NAME_LEN) :: "cp1125"], &
                                          doc="cp1125 8-bit encoding for Cyrillic")
        call define_coding_system_alias("ruscii", "cp1125")
        call define_coding_system_alias("cp866u", "cp1125")
        call define_charset_coding_system("cp855", iachar("D"), &
                                          charset_names=[character(len=CHARSET_NAME_LEN) :: "cp855"], &
                                          mime_charset="cp855", &
                                          doc="DOS codepage 855 (Russian)")
        call define_coding_system_alias("ibm855", "cp855")
        call define_charset_coding_system("mik", iachar("D"), &
                                          charset_names=[character(len=CHARSET_NAME_LEN) :: "mik"], &
                                          doc="Bulgarian DOS codepage")
        call define_charset_coding_system("pt154", iachar("D"), &
                                          charset_names=[character(len=CHARSET_NAME_LEN) :: "pt154"], &
                                          doc="ParaType Asian Cyrillic codepage")
    end subroutine init_cyrillic

    !> Port of the language environments of cyrillic.el.
    module subroutine define_cyrillic_language_environments()
        call define_language_environment("Cyrillic-ISO", &
                                         documentation="Support for Cyrillic ISO-8859-5.", &
                                         charsets=[character(len=40) :: "iso-8859-5"], &
                                         sample_text="Russian (Русский)"//achar(9)//"Здравствуйте!", &
                                         coding_systems=[character(len=40) :: "cyrillic-iso-8bit"], &
                                         coding_system_order=[character(len=40) :: "cyrillic-iso-8bit"], &
                                         nonascii_translation="iso-8859-5", &
                                         input_method="cyrillic-yawerty", &
                                         unibyte_display="cyrillic-iso-8bit")
        call define_language_environment("Cyrillic-KOI8", &
                                         documentation="Support for Cyrillic KOI8-R.", &
                                         charsets=[character(len=40) :: "koi8"], &
                                         sample_text="Russian (Русский)"//achar(9)//"Здравствуйте!", &
                                         coding_systems=[character(len=40) :: "cyrillic-koi8"], &
                                         coding_system_order=[character(len=40) :: "cyrillic-koi8", "cyrillic-iso-8bit"], &
                                         nonascii_translation="koi8", &
                                         input_method="russian-typewriter", &
                                         unibyte_display="cyrillic-koi8")
        call define_language_environment("Russian", &
                                         documentation="Support for Russian using koi8-r and the russian-computer in" &
                                         //"put method.", &
                                         charsets=[character(len=40) :: "cyrillic-iso8859-5"], &
                                         sample_text="Russian (Русский)"//achar(9)//"Здравствуйте!", &
                                         coding_systems=[character(len=40) :: "cyrillic-koi8"], &
                                         coding_system_order=[character(len=40) :: "cyrillic-koi8", "cyrillic-iso-8bit"], &
                                         input_method="russian-computer", &
                                         unibyte_display="cyrillic-koi8", &
                                         tutorial="TUTORIAL.ru")
        call define_language_environment("Ukrainian", &
                                         documentation="Support for Ukrainian with koi8-u character set.", &
                                         charsets=[character(len=40) :: "koi8-u"], &
                                         sample_text="Ukrainian (Українськ" &
                                         //"а)"//achar(9)//"Вітаю / Добрий день!" &
                                         //" / Привіт", &
                                         coding_systems=[character(len=40) :: "koi8-u"], &
                                         coding_system_order=[character(len=40) :: "koi8-u"], &
                                         nonascii_translation="koi8-u", &
                                         input_method="ukrainian-computer", &
                                         tutorial="TUTORIAL.uk")
        call define_language_environment("Cyrillic-ALT", &
                                         documentation="Support for Cyrillic ALTERNATIVNYJ.", &
                                         charsets=[character(len=40) :: "alternativnyj"], &
                                         sample_text="Russian (Русский)"//achar(9)//"Здравствуйте!", &
                                         coding_systems=[character(len=40) :: "cyrillic-alternativnyj"], &
                                         coding_system_order=[character(len=40) :: "cyrillic-alternativnyj"], &
                                         nonascii_translation="alternativnyj", &
                                         input_method="russian-typewriter", &
                                         unibyte_display="cyrillic-alternativnyj")
        call define_language_environment("Tajik", &
                                         documentation="Support for Tajik using KOI8-T.", &
                                         charsets=[character(len=40) :: "koi8-t"], &
                                         coding_systems=[character(len=40) :: "koi8-t"], &
                                         coding_system_order=[character(len=40) :: "koi8-t"], &
                                         nonascii_translation="cyrillic-koi8-t", &
                                         input_method="russian-typewriter")
        call define_language_environment("Bulgarian", &
                                         documentation="Support for Bulgarian with windows-1251 character set.", &
                                         charsets=[character(len=40) :: "windows-1251"], &
                                         coding_systems=[character(len=40) :: "windows-1251"], &
                                         coding_system_order=[character(len=40) :: "windows-1251"], &
                                         nonascii_translation="windows-1251", &
                                         input_method="bulgarian-bds")
        call define_language_environment("Belarusian", &
                                        documentation="Support for Belarusian with windows-1251 character set."//achar(10)//"(The" &
                                         //" name Belarusian replaced Byelorussian in the early 1990s.)", &
                                         charsets=[character(len=40) :: "windows-1251"], &
                                         coding_systems=[character(len=40) :: "windows-1251"], &
                                         coding_system_order=[character(len=40) :: "windows-1251"], &
                                         nonascii_translation="windows-1251", &
                                         input_method="belarusian")
        call define_language_environment("Mongolian-cyrillic", &
                                         documentation="Support for Mongolian language with Cyrillic alphabet.", &
                                         sample_text="Mongolian (монгол хэл)"//achar(9)//"Сайн байна уу?", &
                                         coding_systems=[character(len=40) :: "utf-8"], &
                                         coding_system_order=[character(len=40) :: "utf-8"], &
                                         input_method="cyrillic-mongolian")
    end subroutine define_cyrillic_language_environments
end submodule cyrillic_mod
