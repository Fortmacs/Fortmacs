! japanese_fm.f90 -- the japanese coding systems of type `charset'.
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

!> The coding systems of type `charset' that japanese.el defines, with their
!> aliases (its other coding systems and its language environments are
!> Deferred).  A submodule of coding_fm, which defines them when it
!> starts.
submodule(coding_fm) japanese_fm
    implicit none(type, external)

contains

    !> Port of the `charset' coding systems of japanese.el.
    module subroutine syms_of_japanese()
        call define_charset_coding_system("japanese-cp932", iachar("S"), &
                                          charsets=[character(len=CHARSET_NAME_LEN) :: "ascii", "katakana-sjis", "cp932-2-byte"], &
                                          doc="CP932 (Microsoft shift-jis)")
        call define_coding_system_alias("cp932", "japanese-cp932")
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
end submodule japanese_fm
