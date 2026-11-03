! czech_mod.f90 -- the czech language environments.
! Ported from GNU Emacs 31.1 lisp/language/czech.el.
!
! Copyright (C) 1998, 2001-2026 Free Software Foundation, Inc.
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

!> The language environments czech.el defines.  A submodule of coding_mod,
!> which defines them when mule_mod asks for them.
submodule(coding_mod) czech_mod
    use mule_mod, only: define_language_environment
    implicit none(type, external)

contains

    !> Port of the language environments of czech.el.
    module subroutine define_czech_language_environments()
        call define_language_environment("Czech", &
                                        documentation="This language environment is almost the same as Latin-2,"//achar(10)//"but" &
                                         //" sets the default input method to ""czech"","//achar(10)//"and selects the C" &
                                         //"zech tutorial.", &
                                         charsets=[character(len=40) :: "ascii", "latin-iso8859-2"], &
                                         sample_text="Přejeme vám hezký den!", &
                                         coding_systems=[character(len=40) :: "iso-8859-2"], &
                                         coding_system_order=[character(len=40) :: "iso-8859-2"], &
                                         nonascii_translation="iso-8859-2", &
                                         input_method="czech", &
                                         unibyte_display="iso-8859-2", &
                                         tutorial="TUTORIAL.cs")
    end subroutine define_czech_language_environments
end submodule czech_mod
