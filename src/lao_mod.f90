! lao_mod.f90 -- the lao coding systems of type `charset'.
! Ported from GNU Emacs 31.1 lisp/language/lao.el.
!
! Copyright (C) 2001-2026 Free Software Foundation, Inc.
! Copyright (C) 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006,
!   2007, 2008, 2009, 2010, 2011
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

!> The coding systems of type `charset' that lao.el defines, with their
!> aliases (its other coding systems and its language environments are
!> Deferred).  A submodule of coding_mod, which defines them when it
!> starts.
submodule(coding_mod) lao_mod
    implicit none(type, external)

contains

    !> Port of the `charset' coding systems of lao.el.
    module subroutine init_lao()
        call define_charset_coding_system("lao", iachar("L"), &
                                          charset_names=[character(len=CHARSET_NAME_LEN) :: "lao"], &
                                          doc="8-bit encoding for ASCII (MSB=0) and LAO (MSB=1).")
    end subroutine init_lao
end submodule lao_mod
