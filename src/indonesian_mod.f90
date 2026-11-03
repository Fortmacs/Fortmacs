! indonesian_mod.f90 -- the indonesian language environments.
! Ported from GNU Emacs 31.1 lisp/language/indonesian.el.
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

!> The language environments indonesian.el defines.  A submodule of coding_mod,
!> which defines them when mule_mod asks for them.
submodule(coding_mod) indonesian_mod
    use mule_mod, only: define_language_environment
    implicit none(type, external)

contains

    !> Port of the language environments of indonesian.el.
    module subroutine define_indonesian_language_environments()
        call define_language_environment("Balinese", &
                                         documentation="Balinese language and its script are supported in this langu" &
                                         //"age environment.", &
                                         charsets=[character(len=40) :: "unicode"], &
                                         sample_text="Balinese (ᬅᬓ᭄ᬱᬭᬩᬮᬶ)"//achar(9) &
                                         //"ᬒᬁᬲ᭄ᬯᬲ᭄ᬢ᭄ᬬᬲ᭄ᬢᬸ", &
                                         coding_systems=[character(len=40) :: "utf-8"], &
                                         coding_system_order=[character(len=40) :: "utf-8"], &
                                         input_method="balinese")
        call define_language_environment("Javanese", &
                                         documentation="Javanese language and its script are supported in this langu" &
                                         //"age environment.", &
                                         charsets=[character(len=40) :: "unicode"], &
                                         sample_text="Javanese (ꦲꦏ꧀ꦱꦫꦗꦮ)"//achar(9)//"ꦲꦭꦺꦴ", &
                                         coding_systems=[character(len=40) :: "utf-8"], &
                                         coding_system_order=[character(len=40) :: "utf-8"], &
                                         input_method="javanese")
        call define_language_environment("Sundanese", &
                                         documentation="Sundanese language and its script are supported in this lang" &
                                         //"uage environment.", &
                                         charsets=[character(len=40) :: "unicode"], &
                                         sample_text="Sundanese (ᮃᮊ᮪ᮞᮛᮞᮥᮔ᮪ᮓ)"// &
                                         "    ᮞᮙ᮪ᮕᮥᮛᮞᮥᮔ᮪", &
                                         coding_systems=[character(len=40) :: "utf-8"], &
                                         coding_system_order=[character(len=40) :: "utf-8"], &
                                         input_method="sundanese")
        call define_language_environment("Batak", &
                                         documentation="Languages that use the Batak script, such as Karo, Toba, Pak" &
                                         //"pak, Mandailing"//achar(10)//"and Simalungun, are supported in this langua" &
                                         //"ge environment.", &
                                         charsets=[character(len=40) :: "unicode"], &
                                         sample_text="Batak (ᯘᯮᯒᯗ᯲ᯅᯗᯂ"//"᯲)    ᯂᯬᯒᯘ᯲ / ᯔ" &
                                         //"ᯧᯐᯬᯀᯱᯐᯬᯀᯱ", &
                                         coding_systems=[character(len=40) :: "utf-8"], &
                                         coding_system_order=[character(len=40) :: "utf-8"], &
                                         input_method="batak")
        call define_language_environment("Rejang", &
                                         documentation="Rejang language and its script are supported in this languag" &
                                         //"e environment.", &
                                         charsets=[character(len=40) :: "unicode"], &
                                         sample_text="Rejang (ꥆꤰ꥓ꤼꤽ ꤽꥍꤺꥏ)    ꤸꥉꥐꤺꥉꥂꥎ", &
                                         coding_systems=[character(len=40) :: "utf-8"], &
                                         coding_system_order=[character(len=40) :: "utf-8"], &
                                         input_method="rejang")
        call define_language_environment("Makasar", &
                                         documentation="Makassarese language and its script Makasar are supported in" &
                                         //" this language environment.", &
                                         charsets=[character(len=40) :: "unicode"], &
                                         sample_text="Makasar (𑻪𑻢𑻪𑻢)    𑻦𑻤𑻵𑻱", &
                                         coding_systems=[character(len=40) :: "utf-8"], &
                                         coding_system_order=[character(len=40) :: "utf-8"], &
                                         input_method="makasar")
        call define_language_environment("Buginese", &
                                         documentation="Buginese language and its script Lontara are supported in th" &
                                         //"is language environment.", &
                                         charsets=[character(len=40) :: "unicode"], &
                                         sample_text="Buginese (ᨒᨚᨈᨑ)    ᨖᨒᨚ", &
                                         coding_systems=[character(len=40) :: "utf-8"], &
                                         coding_system_order=[character(len=40) :: "utf-8"], &
                                         input_method="lontara")
    end subroutine define_indonesian_language_environments
end submodule indonesian_mod
