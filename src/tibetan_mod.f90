! tibetan_mod.f90 -- the tibetan coding systems of type `iso-2022'.
! Ported from GNU Emacs 31.1 lisp/language/tibetan.el.
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

!> The coding systems of type `iso-2022' that tibetan.el defines, with their
!> aliases (its other coding systems and its language environments are
!> Deferred).  A submodule of coding_mod, which defines them when it
!> starts.
submodule(coding_mod) tibetan_mod
    implicit none(type, external)

contains

    !> Port of the `iso-2022' coding systems of tibetan.el.
    module subroutine init_tibetan()
        call define_iso_2022_coding_system("tibetan-iso-8bit", iachar("Q"), &
                                           doc="8-bit encoding for ASCII (MSB=0) and TIBETAN (MSB=1).", &
                                           initial=[character(len=CHARSET_NAME_LEN) :: "ascii", "tibetan", "", ""], &
                                           request_charset_names=[character(len=CHARSET_NAME_LEN) :: "tibetan", "ascii"], &
                                           request_registers=[integer :: 1, 0], &
                                           register_usage=[4, 4], &
                                           flags=[character(len=16) ::], &
                                           charset_names=[character(len=CHARSET_NAME_LEN) :: "ascii", "tibetan"])
        call define_coding_system_alias("tibetan", "tibetan-iso-8bit")
    end subroutine init_tibetan
end submodule tibetan_mod
