! georgian_mod.f90 -- the georgian coding systems of type `charset'.
! Ported from GNU Emacs 31.1 lisp/language/georgian.el.
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

!> The coding systems of type `charset' that georgian.el defines, with their
!> aliases, and its language environments (its other coding systems are
!> Deferred).  A submodule of coding_mod, which defines the coding systems
!> when it starts and the language environments when mule_mod asks for them.
submodule(coding_mod) georgian_mod
    use mule_mod, only: define_language_environment
    implicit none(type, external)

contains

    !> Port of the `charset' coding systems of georgian.el.
    module subroutine init_georgian()
        call define_charset_coding_system("georgian-ps", iachar("G"), &
                                          charset_names=[character(len=CHARSET_NAME_LEN) :: "georgian-ps"], &
                                          doc="Georgian PS encoding")
        call define_charset_coding_system("georgian-academy", iachar("G"), &
                                          charset_names=[character(len=CHARSET_NAME_LEN) :: "georgian-academy"], &
                                          doc="Georgian Academy encoding")
    end subroutine init_georgian

    !> Port of the language environments of georgian.el.
    module subroutine define_georgian_language_environments()
        call define_language_environment("Georgian", &
                                         documentation="Support for georgian-ps character set.", &
                                         coding_systems=[character(len=40) :: "georgian-ps"], &
                                         coding_system_order=[character(len=40) :: "georgian-ps"], &
                                         nonascii_translation="georgian-ps", &
                                         input_method="georgian")
    end subroutine define_georgian_language_environments
end submodule georgian_mod
