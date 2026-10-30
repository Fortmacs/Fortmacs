! tai_viet_fm.f90 -- the tai-viet language environments.
! Ported from GNU Emacs 31.1 lisp/language/tai-viet.el.
!
! Copyright (C) 2007-2026 Free Software Foundation, Inc.
! Copyright (C) 2007, 2008, 2009, 2010, 2011
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

!> The language environments tai-viet.el defines.  A submodule of coding_fm,
!> which defines them when mule_fm asks for them.
submodule(coding_fm) tai_viet_fm
    use mule_fm, only: set_language_info_alist
    implicit none(type, external)

contains

    !> Port of the language environments of tai-viet.el (set-language-info-alist).
    module subroutine language_info_tai_viet()
        call set_language_info_alist("TaiViet", &
                                     documentation="TaiViet refers to the Tai script, which is used to write sev" &
                                     //"eral"//achar(10)//"Tai languages of northwestern Vietnam and surrounding a" &
                                     //"reas.  These"//achar(10)//"languages are Tai Dam (also known as Black Tai " &
                                     //"or Tai Noir),"//achar(10)//"Tai Dón (also known as White Tai or Tai Blanc" &
                                     //"), Tày Tac,"//achar(10)//"Tai Daeng (also known as Red Tai or Tai Rouge)," &
                                     //achar(10)//"and Thai Song (also known as Lao Song).  However, some peop" &
                                     //"le"//achar(10)//"consider Tai Dam, Tai Dón and Tai Daeng to be dialects o" &
                                     //"f the"//achar(10)//"same language, and call them collectively ""Tai Viet""."//achar(10) &
                                     //achar(10)//"Both the script and languages have the same origin as that " &
                                     //"of Thai"//achar(10)//"language/script used in Thailand, but now they diffe" &
                                     //"r from each"//achar(10)//"other in a significant way (especially the scrip" &
                                     //"ts are)."//achar(10)//achar(10)//"The language name is spelled as ""ꪁꪫꪱꪣ ꪼꪕ"", and the" &
                                     //" script name is"//achar(10)//"spelled as ""ꪎꪳ ꪼꪕ"".", &
                                     charsets=[character(len=40) :: "unicode"], &
                                     sample_text="TaiViet (ꪁꪫꪱꪣ ꪼꪕ)"//achar(9)//achar(9) &
                                     //"ꪅꪰꪙꫂ ꪨꪮꫂ ꪁꪫꪱ / ꪅꪽ ꪨꪷ" &
                                     //" ꪁꪫꪱ", &
                                     coding_systems=[character(len=40) :: "utf-8"], &
                                     coding_priority=[character(len=40) :: "utf-8"], &
                                     input_method="tai-sonla")
    end subroutine language_info_tai_viet
end submodule tai_viet_fm
