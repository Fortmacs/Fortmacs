! english_fm.f90 -- the english coding systems of type `charset'.
! Ported from GNU Emacs 31.1 lisp/language/english.el.
!
! Copyright (C) 1997, 2001-2026 Free Software Foundation, Inc.
! Copyright (C) 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
!   2006, 2007, 2008, 2009, 2010, 2011
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

!> The coding systems of type `charset' that english.el defines, with their
!> aliases (its other coding systems and its language environments are
!> Deferred).  A submodule of coding_fm, which defines them when it
!> starts.
submodule(coding_fm) english_fm
    use mule_fm, only: set_language_info_alist
    implicit none(type, external)

contains

    !> Port of the `charset' coding systems of english.el.
    module subroutine syms_of_english()
        call define_charset_coding_system("ebcdic-us", iachar("*"), &
                                          charsets=[character(len=CHARSET_NAME_LEN) :: "ebcdic-us"], &
                                          doc="US version of EBCDIC")
        call define_charset_coding_system("ebcdic-uk", iachar("*"), &
                                          charsets=[character(len=CHARSET_NAME_LEN) :: "ebcdic-uk"], &
                                          doc="UK version of EBCDIC")
        call define_charset_coding_system("ibm1047", iachar("*"), &
                                          charsets=[character(len=CHARSET_NAME_LEN) :: "ibm1047"], &
                                          doc="A version of EBCDIC used in OS/390 Unix")
        call define_coding_system_alias("cp1047", "ibm1047")
        call define_charset_coding_system("ibm038", iachar("*"), &
                                          charsets=[character(len=CHARSET_NAME_LEN) :: "ibm038"], &
                                          doc="International version of EBCDIC")
        call define_coding_system_alias("ebcdic-int", "ibm038")
        call define_coding_system_alias("cp038", "ibm038")
    end subroutine syms_of_english

    !> Port of the language environments of english.el (set-language-info-alist).
    module subroutine language_info_english()
        call set_language_info_alist("English", &
                                     documentation="Nothing special is needed to handle English.", &
                                     charsets=[character(len=40) :: "ascii"], &
                                     sample_text="Hello!, Hi!, How are you?", &
                                     tutorial="TUTORIAL")
        call set_language_info_alist("ASCII", &
                                     documentation="Nothing special is needed to handle English.", &
                                     charsets=[character(len=40) :: "ascii"], &
                                     sample_text="Hello!, Hi!, How are you?", &
                                     tutorial="TUTORIAL")
    end subroutine language_info_english
end submodule english_fm
