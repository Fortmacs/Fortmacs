! greek_mod.f90 -- the greek coding systems of type `character_set'.
! Ported from GNU Emacs 31.1 lisp/language/greek.el.
!
! Copyright (C) 2002, 2013-2026 Free Software Foundation, Inc.
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

!> The coding systems of type `character_set' that greek.el defines, with their
!> aliases, and its language environments (its other coding systems are
!> Deferred).  A submodule of coding_mod, which defines the coding systems
!> when it starts and the language environments when multilingual_environment_mod asks for them.
submodule(coding_mod) greek_mod
    use multilingual_environment_mod, only: define_language_environment
    implicit none(type, external)

contains

    !> Port of the `character_set' coding systems of greek.el.
    module subroutine initialize_greek()
        call define_character_set_coding_system("greek-iso-8bit", iachar("7"), &
                                                character_set_names=[character(len=CHARACTER_SET_NAME_LENGTH) :: "iso-8859-7"], &
                                                mime_character_set="iso-8859-7", &
                                                doc="ISO 2022 based 8-bit encoding for Greek (MIME:ISO-8859-7).")
        call define_coding_system_alias("iso-8859-7", "greek-iso-8bit")
        call define_character_set_coding_system("windows-1253", iachar("g"), &
                                                character_set_names=[character(len=CHARACTER_SET_NAME_LENGTH) :: "windows-1253"], &
                                                mime_character_set="windows-1253", &
                                                doc="windows-1253 encoding for Greek")
        call define_coding_system_alias("cp1253", "windows-1253")
        call define_character_set_coding_system("cp737", iachar("D"), &
                                                character_set_names=[character(len=CHARACTER_SET_NAME_LENGTH) :: "cp737"], &
                                                mime_character_set="cp737", &
                                                doc="Codepage 737 (PC Greek)")
        call define_character_set_coding_system("cp851", iachar("D"), &
                                                character_set_names=[character(len=CHARACTER_SET_NAME_LENGTH) :: "cp851"], &
                                                mime_character_set="cp851", &
                                                doc="DOS codepage 851 (Greek)")
        call define_coding_system_alias("ibm851", "cp851")
        call define_character_set_coding_system("cp869", iachar("D"), &
                                                character_set_names=[character(len=CHARACTER_SET_NAME_LENGTH) :: "cp869"], &
                                                mime_character_set="cp869", &
                                                doc="DOS codepage 869 (Greek)")
        call define_coding_system_alias("ibm869", "cp869")
    end subroutine initialize_greek

    !> Port of the language environments of greek.el.
    module subroutine define_greek_language_environments()
        call define_language_environment("Greek", &
                                         documentation="Support for Greek ISO-8859-7 using the greek input method.", &
                                         character_sets=[character(len=40) :: "iso-8859-7"], &
                                         sample_text="Greek (ελληνικά)"//achar(9)//"Γειά σας", &
                                         coding_systems=[character(len=40) :: "greek-iso-8bit", "windows-1253", "cp851", "cp869"], &
                                         coding_system_order=[character(len=40) :: "greek-iso-8bit"], &
                                         nonascii_translation="iso-8859-7", &
                                         input_method="greek", &
                                         tutorial="TUTORIAL.element_GR")
    end subroutine define_greek_language_environments
end submodule greek_mod
