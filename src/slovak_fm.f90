! slovak_fm.f90 -- the slovak language environments.
! Ported from GNU Emacs 31.1 lisp/language/slovak.el.
!
! Copyright (C) 1998, 2001-2026 Free Software Foundation, Inc.
!             Milan Zamazal <pdm@zamazal.org>
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

!> The language environments slovak.el defines.  A submodule of coding_fm,
!> which defines them when mule_fm asks for them.
submodule(coding_fm) slovak_fm
    use mule_fm, only: set_language_info_alist
    implicit none(type, external)

contains

    !> Port of the language environments of slovak.el (set-language-info-alist).
    module subroutine language_info_slovak()
        call set_language_info_alist("Slovak", &
                    documentation="This language environment is almost the same as Latin-2,"//achar(10)//"but" &
                    //" sets the default input method to ""slovak"","//achar(10)//"and selects the " &
                    //"Slovak tutorial.", &
                    charsets=[character(len=40) :: "ascii", "latin-iso8859-2"], &
                    sample_text="Prajeme Vám príjemný deň!", &
                    coding_systems=[character(len=40) :: "iso-8859-2"], &
                    coding_priority=[character(len=40) :: "iso-8859-2"], &
                    nonascii_translation="iso-8859-2", &
                    input_method="slovak", &
                    unibyte_display="iso-8859-2", &
                    tutorial="TUTORIAL.sk")
    end subroutine language_info_slovak
end submodule slovak_fm
