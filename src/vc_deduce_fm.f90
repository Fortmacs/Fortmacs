! vc_deduce_fm.f90 -- fileset deduction in VC-Dir and Log View buffers.
! Ported from GNU Emacs 31.1 lisp/vc/vc.el.
!
! Copyright (C) 1992-2026 Free Software Foundation, Inc.
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

!> The cases of `vc_deduce_backend', `vc_deduce_fileset' and
!> `vc_ensure_vc_buffer' for VC-Dir and Log View buffers: a submodule
!> because vc_dir_fm and log_view_fm use vc_fm.
submodule(vc_fm) vc_deduce_fm
    use vc_dir_fm, only: vc_dir_backend, vc_dir_deduce_fileset, vc_dir_current_file
    use log_view_fm, only: log_view_vc_backend, log_view_vc_fileset
    use derived_fm, only: mode_p => is_derived_mode
    use buffer_fm, only: set_buffer
    use files_fm, only: find_file_noselect
    use log_edit_fm, only: log_edit_vc_backend
    implicit none(type, external)

contains

    module procedure mode_backend
    b = 0

    if (mode_p("vc_dir_mode")) then
        b = vc_dir_backend()
    else if (mode_p("log_view_mode")) then
        b = log_view_vc_backend()
    else if (mode_p("log_edit_mode")) then
        b = log_edit_vc_backend()
    end if

    handled = mode_p("vc_dir_mode") .or. mode_p("log_view_mode") .or. mode_p("log_edit_mode")
    end procedure mode_backend

    module procedure vc_dir_fileset
    status = status_type()
    handled = mode_p("vc_dir_mode")
    if (handled) call vc_dir_deduce_fileset(fs, status, state_model_only_files)
    end procedure vc_dir_fileset

    module procedure log_view_fileset
    logical :: has

    handled = mode_p("log_view_mode")
    if (.not. handled) return
    ! 'log-view-mode' stashes the backend and the fileset in the
    ! two special variables, so we use them to avoid any possible
    ! mistakes from a decision made here ad-hoc.
    fs%backend = log_view_vc_backend()
    call log_view_vc_fileset(fs%files, has)
    end procedure log_view_fileset

    module procedure vc_dir_ensure_buffer
    character(len=:), allocatable :: file
    type(handle_type) :: buffer
    type(status_type) :: st

    status = status_type()
    handled = mode_p("vc_dir_mode")
    if (.not. handled) return
    call vc_dir_current_file(file, status)
    if (.not. status%ok()) return
    call find_file_noselect(file, buffer, status)
    if (.not. status%ok()) return
    call set_buffer(buffer, st)
    end procedure vc_dir_ensure_buffer
end submodule vc_deduce_fm
