! philippine_fm.f90 -- the philippine language environments.
! Ported from GNU Emacs 31.1 lisp/language/philippine.el.
!
! Copyright (C) 2022-2026 Free Software Foundation, Inc.
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

!> The language environments philippine.el defines.  A submodule of coding_fm,
!> which defines them when mule_fm asks for them.
submodule(coding_fm) philippine_fm
    use mule_fm, only: set_language_info_alist
    implicit none(type, external)

contains

    !> Port of the language environments of philippine.el (set-language-info-alist).
    module subroutine language_info_philippine()
        call set_language_info_alist("Tagalog", &
                    documentation="Tagalog language using the Baybayin script is supported in"//achar(10)//"t" &
                    //"his language environment.", &
                    charsets=[character(len=40) :: "unicode"], &
                    sample_text="Tagalog (ᜊᜌ᜔ᜊᜌᜒᜈ᜔)"//achar(9)//"ᜃᜓᜋᜓᜐ᜔ᜆ", &
                    coding_systems=[character(len=40) :: "utf-8"], &
                    coding_priority=[character(len=40) :: "utf-8"], &
                    input_method="tagalog")
        call set_language_info_alist("Hanunoo", &
                    documentation="Philippine Language Hanunoo is supported in this language en" &
                    //"vironment.", &
                    charsets=[character(len=40) :: "unicode"], &
                    sample_text="Hanunoo (ᜱᜨᜳᜨᜳᜢ)"//achar(9)//"ᜫᜬᜧ᜴ ᜣᜭᜯᜥ᜴ ᜰᜲᜭᜥ᜴", &
                    coding_systems=[character(len=40) :: "utf-8"], &
                    coding_priority=[character(len=40) :: "utf-8"], &
                    input_method="hanunoo")
        call set_language_info_alist("Buhid", &
                    documentation="Philippine Language Buhid is supported in this language envi" &
                    //"ronment.", &
                    charsets=[character(len=40) :: "unicode"], &
                    coding_systems=[character(len=40) :: "utf-8"], &
                    coding_priority=[character(len=40) :: "utf-8"], &
                    input_method="buhid")
        call set_language_info_alist("Tagbanwa", &
                    documentation="Philippine Languages Tagbanwa are supported in this language" &
                    //" environment.", &
                    charsets=[character(len=40) :: "unicode"], &
                    sample_text="Tagbanwa (ᝦᝪᝯ)"//achar(9)//"ᝫᝩᝬᝥ ᝣᝮᝧᝯ", &
                    coding_systems=[character(len=40) :: "utf-8"], &
                    coding_priority=[character(len=40) :: "utf-8"], &
                    input_method="tagbanwa")
    end subroutine language_info_philippine
end submodule philippine_fm
