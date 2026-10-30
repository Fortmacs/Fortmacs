! romanian_fm.f90 -- the romanian coding systems of type `charset'.
! Ported from GNU Emacs 31.1 lisp/language/romanian.el.
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

!> The coding systems of type `charset' that romanian.el defines, with their
!> aliases (its other coding systems and its language environments are
!> Deferred).  A submodule of coding_fm, which defines them when it
!> starts.
submodule(coding_fm) romanian_fm
    use mule_fm, only: set_language_info_alist
    implicit none(type, external)

contains

    !> Port of the `charset' coding systems of romanian.el.
    module subroutine syms_of_romanian()
        call define_charset_coding_system("iso-latin-10", iachar("*"), &
                                          charsets=[character(len=CHARSET_NAME_LEN) :: "iso-8859-16"], &
                                          mime_charset="iso-8859-16", &
                                          doc="ISO 2022 based 8-bit encoding for Latin-10.")
        call define_coding_system_alias("iso-8859-16", "iso-latin-10")
        call define_coding_system_alias("latin-10", "iso-latin-10")
    end subroutine syms_of_romanian

    !> Port of the language environments of romanian.el (set-language-info-alist).
    module subroutine language_info_romanian()
        call set_language_info_alist("Romanian", &
                                     documentation="Romanian environment using Latin-2 encoding."//achar(10)//"An environment " &
                                     //"for generic Latin-10 encoding is also available.", &
                                     charsets=[character(len=40) :: "iso-8859-2"], &
                                     sample_text="Bună ziua, bine aţi venit!", &
                                     coding_systems=[character(len=40) :: "iso-8859-2", "iso-latin-10"], &
                                     coding_priority=[character(len=40) :: "iso-8859-2"], &
                                     nonascii_translation="iso-8859-2", &
                                     input_method="latin-2-postfix", &
                                     unibyte_display="iso-8859-2", &
                                     tutorial="TUTORIAL.ro")
    end subroutine language_info_romanian
end submodule romanian_fm
