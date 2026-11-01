! cyrillic_fm.f90 -- the cyrillic coding systems of type `charset'.
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
!> aliases (its other coding systems and its language environments are
!> Deferred).  A submodule of coding_fm, which defines them when it
!> starts.
submodule(coding_fm) cyrillic_fm
    implicit none(type, external)

contains

    !> Port of the `charset' coding systems of cyrillic.el.
    module subroutine syms_of_cyrillic()
        call define_charset_coding_system("cyrillic-iso-8bit", iachar("5"), &
                                          charsets=[character(len=CHARSET_NAME_LEN) :: "iso-8859-5"], &
                                          mime_charset="iso-8859-5", &
                                          doc="ISO 2022 based 8-bit encoding for Cyrillic script (MIME:ISO-" &
                                          //"8859-5).")
        call define_coding_system_alias("iso-8859-5", "cyrillic-iso-8bit")
        call define_charset_coding_system("cyrillic-koi8", iachar("R"), &
                                          charsets=[character(len=CHARSET_NAME_LEN) :: "koi8"], &
                                          mime_charset="koi8-r", &
                                          doc="KOI8 8-bit encoding for Cyrillic (MIME: KOI8-R).")
        call define_coding_system_alias("koi8-r", "cyrillic-koi8")
        call define_coding_system_alias("koi8", "cyrillic-koi8")
        call define_coding_system_alias("cp878", "cyrillic-koi8")
        call define_charset_coding_system("koi8-u", 1059, &
                                          charsets=[character(len=CHARSET_NAME_LEN) :: "koi8-u"], &
                                          mime_charset="koi8-u", &
                                          doc="KOI8-U 8-bit encoding for Cyrillic (MIME: KOI8-U)")
        call define_charset_coding_system("cyrillic-alternativnyj", iachar("A"), &
                                          charsets=[character(len=CHARSET_NAME_LEN) :: "alternativnyj"], &
                                          doc="ALTERNATIVNYJ 8-bit encoding for Cyrillic.")
        call define_coding_system_alias("alternativnyj", "cyrillic-alternativnyj")
        call define_charset_coding_system("cp866", iachar("*"), &
                                          charsets=[character(len=CHARSET_NAME_LEN) :: "ibm866"], &
                                          mime_charset="cp866", &
                                          doc="CP866 encoding for Cyrillic.")
        call define_charset_coding_system("koi8-t", iachar("*"), &
                                          charsets=[character(len=CHARSET_NAME_LEN) :: "koi8-t"], &
                                          mime_charset="koi8-t", &
                                          doc="KOI8-T 8-bit encoding for Cyrillic")
        call define_charset_coding_system("windows-1251", iachar("b"), &
                                          charsets=[character(len=CHARSET_NAME_LEN) :: "windows-1251"], &
                                          mime_charset="windows-1251", &
                                          doc="windows-1251 8-bit encoding for Cyrillic (MIME: WINDOWS-1251" &
                                          //")")
        call define_coding_system_alias("cp1251", "windows-1251")
        call define_charset_coding_system("cp1125", iachar("*"), &
                                          charsets=[character(len=CHARSET_NAME_LEN) :: "cp1125"], &
                                          doc="cp1125 8-bit encoding for Cyrillic")
        call define_coding_system_alias("ruscii", "cp1125")
        call define_coding_system_alias("cp866u", "cp1125")
        call define_charset_coding_system("cp855", iachar("D"), &
                                          charsets=[character(len=CHARSET_NAME_LEN) :: "cp855"], &
                                          mime_charset="cp855", &
                                          doc="DOS codepage 855 (Russian)")
        call define_coding_system_alias("ibm855", "cp855")
        call define_charset_coding_system("mik", iachar("D"), &
                                          charsets=[character(len=CHARSET_NAME_LEN) :: "mik"], &
                                          doc="Bulgarian DOS codepage")
        call define_charset_coding_system("pt154", iachar("D"), &
                                          charsets=[character(len=CHARSET_NAME_LEN) :: "pt154"], &
                                          doc="ParaType Asian Cyrillic codepage")
    end subroutine syms_of_cyrillic
end submodule cyrillic_fm
