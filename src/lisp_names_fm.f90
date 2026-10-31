! lisp_names_fm.f90 -- the name and string constructors of a value.
! Ported from GNU Emacs 31.1 src/lread.c (intern) and src/alloc.c
! (build_string).
!
! Copyright (C) 1985-2026 Free Software Foundation, Inc.
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

!> Part of lisp_fm: `value_type(name=...)' and `value_type(text=...)'.
!> A name is added to the name table (lread_fm), which uses lisp_fm, so
!> this constructor lives in a submodule.
submodule(lisp_fm) lisp_names_fm
    use lread_fm, only: intern
    implicit none(type, external)

contains

    !> A name value for `name' (added to the name table; "nil" gives the
    !> null value), or a string value holding `text' (already in the
    !> internal encoding); with neither, the null value.  Port of
    !> lread.c:intern and alloc.c:build_string.
    module procedure value_of_string
    if (present(name)) then
        v = make_symbol_ref(intern(name))
    else if (present(text)) then
        allocate (v%a(1))
        v%a(1)%tag = V_STRING
        v%a(1)%s = text
    end if
    end procedure value_of_string
end submodule lisp_names_fm
