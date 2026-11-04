! add_log_diff_mod.f90 -- the Diff buffer part of finding a change log, a
! submodule of add_log_mod.
! Ported from GNU Emacs 31.1 lisp/vc/add-log.el.
!
! Copyright (C) 1985-1986, 1988, 1993-1994, 1997-1998, 2000-2026 Free
! Software Foundation, Inc.
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

!> `get_diff_source_buffer': a submodule because it uses diff_mode_mod,
!> which uses add_log_mod.
submodule(add_log_mod) add_log_diff_mod
    use diff_mode_mod, only: diff_find_source_location, diff_source_location_type
    implicit none(type, external)

contains

    module procedure get_diff_source_buffer
    type(diff_source_location_type) :: loc
    type(status_type) :: st

    ! (car (ignore-errors (diff-find-source-location)))
    call diff_find_source_location(loc, st)

    if (st%ok()) then
        buffer = loc%buffer
    else
        buffer = handle_type()
    end if
    end procedure get_diff_source_buffer
end submodule add_log_diff_mod
