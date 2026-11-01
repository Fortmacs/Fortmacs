! thai_mod.f90 -- the thai coding systems of type `charset'.
! Ported from GNU Emacs 31.1 lisp/language/thai.el.
!
! Copyright (C) 1997-1998, 2000-2026 Free Software Foundation, Inc.
! Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
!   2005, 2006, 2007, 2008, 2009, 2010, 2011
!   National Institute of Advanced Industrial Science and Technology (AIST)
!   Registration Number H13PRO009
! Copyright (C) 2005
!   National Institute of Advanced Industrial Science and Technology (AIST)
!   Registration Number H14PRO021
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

!> The coding systems of type `charset' that thai.el defines, with their
!> aliases (its other coding systems and its language environments are
!> Deferred).  A submodule of coding_mod, which defines them when it
!> starts.
submodule(coding_mod) thai_mod
    implicit none(type, external)

contains

    !> Port of the `charset' coding systems of thai.el.
    module subroutine init_thai()
        call define_charset_coding_system("thai-tis620", iachar("T"), &
                                          charset_names=[character(len=CHARSET_NAME_LEN) :: "tis620-2533"], &
                                          doc="8-bit encoding for ASCII (MSB=0) and Thai TIS620 (MSB=1).")
        call define_coding_system_alias("th-tis620", "thai-tis620")
        call define_coding_system_alias("tis620", "thai-tis620")
        call define_coding_system_alias("tis-620", "thai-tis620")
        call define_charset_coding_system("cp874", iachar("D"), &
                                          charset_names=[character(len=CHARSET_NAME_LEN) :: "cp874"], &
                                          mime_charset="cp874", &
                                          doc="DOS codepage 874 (Thai)")
        call define_coding_system_alias("ibm874", "cp874")
        call define_charset_coding_system("iso-8859-11", iachar("*"), &
                                          charset_names=[character(len=CHARSET_NAME_LEN) :: "iso-8859-11"], &
                                          mime_charset="iso-8859-11", &
                                          doc="ISO/IEC 8859/11 (Latin/Thai)"//achar(10)//"This is the same as `thai-tis62" &
                                          //"0' with the addition of no-break-space.")
    end subroutine init_thai
end submodule thai_mod
