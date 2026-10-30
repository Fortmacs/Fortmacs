! cham_fm.f90 -- the cham language environments.
! Ported from GNU Emacs 31.1 lisp/language/cham.el.
!
! Copyright (C) 2008, 2009, 2010, 2011, 2012
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

!> The language environments cham.el defines.  A submodule of coding_fm,
!> which defines them when mule_fm asks for them.
submodule(coding_fm) cham_fm
    use mule_fm, only: set_language_info_alist
    implicit none(type, external)

contains

    !> Port of the language environments of cham.el (set-language-info-alist).
    module subroutine language_info_cham()
        call set_language_info_alist("Cham", &
                                     documentation="The Cham script is a Brahmic script used to write Cham,"//achar(10)//"an A" &
                                     //"ustronesian language spoken by some 245,000 Chams"//achar(10)//"in Vietnam" &
                                     //" and Cambodia.", &
                                     charsets=[character(len=40) :: "unicode"], &
                                     sample_text="Cham (ꨌꩌ)"//achar(9)//"ꨦꨤꩌ ꨦꨁꨰ", &
                                     coding_systems=[character(len=40) :: "utf-8"], &
                                     coding_priority=[character(len=40) :: "utf-8"], &
                                     input_method="cham")
    end subroutine language_info_cham
end submodule cham_fm
