! burmese_mod.f90 -- the burmese language environments.
! Ported from GNU Emacs 31.1 lisp/language/burmese.el.
!
! Copyright (C) 2008, 2009, 2010, 2011
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

!> The language environments burmese.el defines.  A submodule of coding_mod,
!> which defines them when mule_mod asks for them.
submodule(coding_mod) burmese_mod
    use mule_mod, only: define_language_environment
    implicit none(type, external)

contains

    !> Port of the language environments of burmese.el.
    module subroutine define_burmese_language_environments()
        call define_language_environment("Burmese", &
                                         has_documentation=.true., &
                                         charsets=[character(len=40) :: "unicode"], &
                                         sample_text="Burmese (မ္ရန္‌မာ)"//achar(9)// &
                                         "မင္‍ဂလာပာ", &
                                         coding_systems=[character(len=40) :: "utf-8"], &
                                         coding_system_order=[character(len=40) :: "utf-8"])
    end subroutine define_burmese_language_environments
end submodule burmese_mod
