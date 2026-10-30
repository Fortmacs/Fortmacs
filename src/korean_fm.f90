! korean_fm.f90 -- the korean coding systems of type `charset'.
! Ported from GNU Emacs 31.1 lisp/language/korean.el.
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

!> The coding systems of type `charset' that korean.el defines, with their
!> aliases (its other coding systems and its language environments are
!> Deferred).  A submodule of coding_fm, which defines them when it
!> starts.
submodule(coding_fm) korean_fm
    implicit none(type, external)

contains

    !> Port of the `charset' coding systems of korean.el.
    module subroutine syms_of_korean()
        call define_charset_coding_system("korean-cp949", iachar("K"), &
                                          charsets=[character(len=CHARSET_NAME_LEN) :: "ascii", "cp949"], &
                                          doc="CP949 (Microsoft Unified Hangul Code)")
        call define_coding_system_alias("cp949", "korean-cp949")
    end subroutine syms_of_korean
end submodule korean_fm
