! vietnamese_mod.f90 -- the vietnamese coding systems of type `charset'.
! Ported from GNU Emacs 31.1 lisp/language/vietnamese.el.
!
! Copyright (C) 1998, 2001-2026 Free Software Foundation, Inc.
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

!> The coding systems of type `charset' that vietnamese.el defines, with their
!> aliases (its other coding systems and its language environments are
!> Deferred).  A submodule of coding_mod, which defines them when it
!> starts.
submodule(coding_mod) vietnamese_mod
    implicit none(type, external)

contains

    !> Port of the `charset' coding systems of vietnamese.el.
    module subroutine init_vietnamese()
        call define_charset_coding_system("vietnamese-viscii", iachar("V"), &
                                          charset_names=[character(len=CHARSET_NAME_LEN) :: "viscii"], &
                                          mime_charset="viscii", &
                                          doc="8-bit encoding for Vietnamese VISCII 1.1 (MIME:VISCII).")
        call define_coding_system_alias("viscii", "vietnamese-viscii")
        call define_charset_coding_system("vietnamese-vscii", iachar("v"), &
                                          charset_names=[character(len=CHARSET_NAME_LEN) :: "vscii"], &
                                          doc="8-bit encoding for Vietnamese VSCII-1 (TCVN-5712).")
        call define_coding_system_alias("vscii", "vietnamese-vscii")
        call define_coding_system_alias("vietnamese-tcvn", "vietnamese-vscii")
        call define_coding_system_alias("tcvn", "vietnamese-vscii")
        call define_coding_system_alias("tcvn-5712", "vietnamese-vscii")
        call define_charset_coding_system("windows-1258", iachar("*"), &
                                          charset_names=[character(len=CHARSET_NAME_LEN) :: "windows-1258"], &
                                          mime_charset="windows-1258", &
                                          doc="windows-1258 encoding for Vietnamese (MIME: WINDOWS-1258)")
        call define_coding_system_alias("cp1258", "windows-1258")
    end subroutine init_vietnamese
end submodule vietnamese_mod
