! korean_mod.f90 -- the korean coding systems of type `charset' and `iso-2022'.
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

!> The coding systems of type `charset' and `iso-2022' that korean.el defines, with their
!> aliases (its other coding systems and its language environments are
!> Deferred).  A submodule of coding_mod, which defines them when it
!> starts.
submodule(coding_mod) korean_mod
    implicit none(type, external)

contains

    !> Port of the `charset' and `iso-2022' coding systems of korean.el.
    module subroutine init_korean()
        call define_iso_2022_coding_system("korean-iso-8bit", iachar("K"), &
                                           doc="ISO 2022 based EUC encoding for Korean KSC5601 (MIME:EUC-KR)" &
                                           //".", &
                                           initial=[character(len=CHARSET_NAME_LEN) :: "ascii", "korean-ksc5601", "", ""], &
                                           request_charset_names=[character(len=CHARSET_NAME_LEN) :: "korean-ksc5601", "ascii"], &
                                           request_registers=[integer :: 1, 0], &
                                           register_usage=[4, 4], &
                                           flags=[character(len=16) ::], &
                                           charset_names=[character(len=CHARSET_NAME_LEN) :: "ascii", "korean-ksc5601"], &
                                           mime_charset="euc-kr")
        call define_coding_system_alias("euc-kr", "korean-iso-8bit")
        call define_coding_system_alias("euc-korea", "korean-iso-8bit")
        call define_coding_system_alias("ks_c_5601-1987", "korean-iso-8bit")
        call define_iso_2022_coding_system("iso-2022-kr", iachar("k"), &
                                           doc="ISO 2022 based 7-bit encoding for Korean KSC5601 (MIME:ISO-2" &
                                           //"022-KR).", &
                                           initial=[character(len=CHARSET_NAME_LEN) :: "ascii", "", "", ""], &
                                           request_charset_names=[character(len=CHARSET_NAME_LEN) :: "korean-ksc5601", "ascii"], &
                                           request_registers=[integer :: 1, 0], &
                                           register_usage=[4, 4], &
                                           flags=[character(len=16) :: &
                                                  "ascii-at-eol", "ascii-at-cntl", "7-bit", "designation", "locking-shift", &
                                                  "designation-bol"], &
                                           charset_names=[character(len=CHARSET_NAME_LEN) :: "ascii", "korean-ksc5601"], &
                                           mime_charset="iso-2022-kr")
        call define_coding_system_alias("korean-iso-7bit-lock", "iso-2022-kr")
        call define_charset_coding_system("korean-cp949", iachar("K"), &
                                          charset_names=[character(len=CHARSET_NAME_LEN) :: "ascii", "cp949"], &
                                          doc="CP949 (Microsoft Unified Hangul Code)")
        call define_coding_system_alias("cp949", "korean-cp949")
    end subroutine init_korean
end submodule korean_mod
