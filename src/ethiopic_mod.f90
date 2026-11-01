! ethiopic_mod.f90 -- the ethiopic language environments.
! Ported from GNU Emacs 31.1 lisp/language/ethiopic.el.
!
! Copyright (C) 1997, 2001-2026 Free Software Foundation, Inc.
! Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
!   2005, 2006, 2007, 2008, 2009, 2010, 2011
!   National Institute of Advanced Industrial Science and Technology (AIST)
!   Registration Number H14PRO021
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

!> The language environments ethiopic.el defines.  A submodule of coding_mod,
!> which defines them when mule_mod asks for them.
submodule(coding_mod) ethiopic_mod
    use mule_mod, only: define_language_environment
    implicit none(type, external)

contains

    !> Port of the language environments of ethiopic.el.
    module subroutine define_ethiopic_language_environments()
        call define_language_environment("Ethiopic", &
                                         documentation="This language environment provides these function key bindin" &
                                      //"gs:"//achar(10)//"    [f3]   ethio-fidel-to-sera-buffer"//achar(10)//"    [S-f3] ethio-f" &
                            //"idel-to-sera-region"//achar(10)//"    [C-f3] ethio-fidel-to-sera-marker"//achar(10)//achar(10)//" " &
                                         //"   [f4]   ethio-sera-to-fidel-buffer"//achar(10)//"    [S-f4] ethio-sera-t" &
                            //"o-fidel-region"//achar(10)//"    [C-f4] ethio-sera-to-fidel-marker"//achar(10)//achar(10)//"    [S" &
                                         //"-f5] ethio-toggle-punctuation"//achar(10)//"    [S-f6] ethio-modify-vowel"//achar(10) &
                                         //"    [S-f7] ethio-replace-space"//achar(10)//achar(10)//"    [S-f9] ethio-replace-spa" &
                                         //"ce"//achar(10)//"    [C-f9] ethio-toggle-space", &
                                         charsets=[character(len=40) :: "ethiopic"], &
                                         sample_text="ፊደል", &
                                         coding_systems=[character(len=40) :: "utf-8-emacs"], &
                                         coding_system_order=[character(len=40) :: "utf-8-emacs"], &
                                         input_method="ethiopic")
    end subroutine define_ethiopic_language_environments
end submodule ethiopic_mod
