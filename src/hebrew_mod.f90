! hebrew_mod.f90 -- the hebrew coding systems of type `charset'.
! Ported from GNU Emacs 31.1 lisp/language/hebrew.el.
!
! Copyright (C) 2001-2026 Free Software Foundation, Inc.
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

!> The coding systems of type `charset' that hebrew.el defines, with their
!> aliases, and its language environments (its other coding systems are
!> Deferred).  A submodule of coding_mod, which defines the coding systems
!> when it starts and the language environments when mule_mod asks for them.
submodule(coding_mod) hebrew_mod
    use mule_mod, only: define_language_environment
    implicit none(type, external)

contains

    !> Port of the `charset' coding systems of hebrew.el.
    module subroutine init_hebrew()
        call define_charset_coding_system("hebrew-iso-8bit", iachar("8"), &
                                          charset_names=[character(len=CHARSET_NAME_LEN) :: "iso-8859-8"], &
                                          mime_charset="iso-8859-8", &
                                          doc="ISO 2022 based 8-bit encoding for Hebrew (MIME:ISO-8859-8).")
        call define_coding_system_alias("iso-8859-8", "hebrew-iso-8bit")
        call define_coding_system_alias("iso-8859-8-e", "hebrew-iso-8bit")
        call define_coding_system_alias("iso-8859-8-i", "hebrew-iso-8bit")
        call define_charset_coding_system("windows-1255", iachar("h"), &
                                          charset_names=[character(len=CHARSET_NAME_LEN) :: "windows-1255"], &
                                          mime_charset="windows-1255", &
                                          doc="windows-1255 (Hebrew) encoding (MIME: WINDOWS-1255)")
        call define_coding_system_alias("cp1255", "windows-1255")
        call define_charset_coding_system("cp862", iachar("D"), &
                                          charset_names=[character(len=CHARSET_NAME_LEN) :: "cp862"], &
                                          mime_charset="cp862", &
                                          doc="DOS codepage 862 (Hebrew)")
        call define_coding_system_alias("ibm862", "cp862")
    end subroutine init_hebrew

    !> Port of the language environments of hebrew.el.
    module subroutine define_hebrew_language_environments()
        call define_language_environment("Hebrew", &
                                         documentation="Bidirectional editing is supported.", &
                                         charsets=[character(len=40) :: "iso-8859-8"], &
                                         sample_text="Hebrew"//achar(9)//"שלום", &
                                         coding_systems=[character(len=40) :: "hebrew-iso-8bit", "windows-1255", "cp862"], &
                                         coding_system_order=[character(len=40) :: "hebrew-iso-8bit"], &
                                         nonascii_translation="iso-8859-8", &
                                         input_method="hebrew", &
                                         unibyte_display="hebrew-iso-8bit", &
                                         tutorial="TUTORIAL.he")
        call define_language_environment("Windows-1255", &
                                        documentation="Support for Windows-1255 encoding, e.g. for Yiddish."//achar(10)//"Bidirec" &
                                         //"tional editing is supported.", &
                                         coding_systems=[character(len=40) :: "windows-1255"], &
                                         coding_system_order=[character(len=40) :: "windows-1255"])
    end subroutine define_hebrew_language_environments
end submodule hebrew_mod
