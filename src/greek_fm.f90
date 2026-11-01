! greek_fm.f90 -- the greek coding systems of type `charset'.
! Ported from GNU Emacs 31.1 lisp/language/greek.el.
!
! Copyright (C) 2002, 2013-2026 Free Software Foundation, Inc.
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

!> The coding systems of type `charset' that greek.el defines, with their
!> aliases (its other coding systems and its language environments are
!> Deferred).  A submodule of coding_fm, which defines them when it
!> starts.
submodule(coding_fm) greek_fm
    implicit none(type, external)

contains

    !> Port of the `charset' coding systems of greek.el.
    module subroutine syms_of_greek()
        call define_charset_coding_system("greek-iso-8bit", iachar("7"), &
                                          charsets=[character(len=CHARSET_NAME_LEN) :: "iso-8859-7"], &
                                          mime_charset="iso-8859-7", &
                                          doc="ISO 2022 based 8-bit encoding for Greek (MIME:ISO-8859-7).")
        call define_coding_system_alias("iso-8859-7", "greek-iso-8bit")
        call define_charset_coding_system("windows-1253", iachar("g"), &
                                          charsets=[character(len=CHARSET_NAME_LEN) :: "windows-1253"], &
                                          mime_charset="windows-1253", &
                                          doc="windows-1253 encoding for Greek")
        call define_coding_system_alias("cp1253", "windows-1253")
        call define_charset_coding_system("cp737", iachar("D"), &
                                          charsets=[character(len=CHARSET_NAME_LEN) :: "cp737"], &
                                          mime_charset="cp737", &
                                          doc="Codepage 737 (PC Greek)")
        call define_charset_coding_system("cp851", iachar("D"), &
                                          charsets=[character(len=CHARSET_NAME_LEN) :: "cp851"], &
                                          mime_charset="cp851", &
                                          doc="DOS codepage 851 (Greek)")
        call define_coding_system_alias("ibm851", "cp851")
        call define_charset_coding_system("cp869", iachar("D"), &
                                          charsets=[character(len=CHARSET_NAME_LEN) :: "cp869"], &
                                          mime_charset="cp869", &
                                          doc="DOS codepage 869 (Greek)")
        call define_coding_system_alias("ibm869", "cp869")
    end subroutine syms_of_greek
end submodule greek_fm
