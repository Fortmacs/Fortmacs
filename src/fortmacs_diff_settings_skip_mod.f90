! fortmacs_diff_settings_skip_mod.f90 -- check_region_skip of an Ediff
! session, a submodule of fortmacs_diff_settings_mod.
! Ported from GNU Emacs 31.1 lisp/vc/ediff-merg.el (in part: see
! docs/status/fortmacs_diff_merging_mod.md).
!
! Copyright (C) 1994-2026 Free Software Foundation, Inc.
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

!> `check_region_skip' of `ediff_session_type': a submodule because the
!> merge states it tests come from fortmacs_diff_merging_mod, which uses
!> fortmacs_diff_settings_mod.
submodule(fortmacs_diff_settings_mod) fortmacs_diff_settings_skip_mod
    use fortmacs_diff_merging_mod, only: is_ediff_merge_region_non_clash, is_ediff_merge_changed_from_default
    implicit none(type, external)

contains

    module procedure check_region_skip
    integer :: session_index
    logical :: changed

    status = status_type()
    skip = .false.
    if (.not. self%merge_job) return
    session_index = findloc(ediff_sessions%serial, self%serial, dim=1)
    if (self%show_clashes_only) skip = is_ediff_merge_region_non_clash(session_index, difference)

    if (self%skip_merge_regions_that_differ_from_default) then
        call is_ediff_merge_changed_from_default(session_index, difference, changed, status, prefers_too=.true.)
        if (.not. status%ok()) return
        skip = skip .or. changed
    end if
    end procedure check_region_skip
end submodule fortmacs_diff_settings_skip_mod
