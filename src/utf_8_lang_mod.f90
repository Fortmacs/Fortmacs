! utf_8_lang_mod.f90 -- the utf-8-lang language environments.
! Ported from GNU Emacs 31.1 lisp/language/utf-8-lang.el.
!
! Copyright (C) 2001-2026 Free Software Foundation, Inc.
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

!> The language environments utf-8-lang.el defines.  A submodule of coding_mod,
!> which defines them when mule_mod asks for them.
submodule(coding_mod) utf_8_lang_mod
    use mule_mod, only: define_language_environment
    implicit none(type, external)

contains

    !> Port of the language environments of utf-8-lang.el.
    module subroutine define_utf_8_lang_language_environments()
        call define_language_environment("UTF-8", &
                                         documentation="This language environment is a generic one for the Unicode c" &
                                         //"haracter set"//achar(10)//"encoded in UTF-8.", &
                                         charsets=[character(len=40) :: "unicode-bmp", "unicode"], &
                                         coding_systems=[character(len=40) :: "utf-8"], &
                                         coding_system_order=[character(len=40) :: "utf-8"], &
                                         input_method="rfc1345")
    end subroutine define_utf_8_lang_language_environments
end submodule utf_8_lang_mod
