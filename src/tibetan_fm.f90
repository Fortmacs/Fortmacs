! tibetan_fm.f90 -- the tibetan coding systems of type `charset' and `iso-2022'.
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

!> The coding systems of type `charset' and `iso-2022' that tibetan.el defines, with their
!> aliases (its other coding systems and its language environments are
!> Deferred).  A submodule of coding_fm, which defines them when it
!> starts.
submodule(coding_fm) tibetan_fm
    use mule_fm, only: set_language_info_alist
    implicit none(type, external)

contains

    !> Port of the `charset' coding systems of tibetan.el.
    module subroutine syms_of_tibetan()
        call define_iso_2022_coding_system("tibetan-iso-8bit", iachar("Q"), &
                                           doc="8-bit encoding for ASCII (MSB=0) and TIBETAN (MSB=1).", &
                                           initial=[character(len=CHARSET_NAME_LEN) :: "ascii", "tibetan", "", ""], &
                                           request_charsets=[character(len=CHARSET_NAME_LEN) :: "tibetan", "ascii"], &
                                           request_registers=[integer :: 1, 0], &
                                           reg_usage=[4, 4], &
                                           flags=[character(len=16) ::], &
                                           charsets=[character(len=CHARSET_NAME_LEN) :: "ascii", "tibetan"])
        call define_coding_system_alias("tibetan", "tibetan-iso-8bit")
    end subroutine syms_of_tibetan

    !> Port of the language environments of tibetan.el (set-language-info-alist).
    module subroutine language_info_tibetan()
        call set_language_info_alist("Tibetan", &
                                     documentation_t=.true., &
                                     charsets=[character(len=40) :: "tibetan", "tibetan-1-column"], &
                                     sample_text="Tibetan (བོད་སྐ"//char(246)//""//char(144)//""//char(130)//"" &
                                     //char(142) &
                                     //"ད་) ༄༅༅༎བཀྲ"//char(246)//""//char(144)//""//char(130)//""//char(142) &
                                     //"་ཤིས་བདེ་ལེགས༎ཨོཾ་མ" &
                                     //char(246)//""//char(144)//""//char(130)//""//char(142)//"་ཎི་པ"//char(246)//"" &
                                     //char(144)//"" &
                                     //char(130)//""//char(142)//"དྨེ་ཧའ" &
                                     //"ུྂ༎", &
                                     coding_systems=[character(len=40) :: "tibetan-iso-8bit"], &
                                     coding_priority=[character(len=40) :: "iso-2022-7bit", "tibetan-iso-8bit"], &
                                     input_method="tibetan-wylie")
    end subroutine language_info_tibetan
end submodule tibetan_fm
