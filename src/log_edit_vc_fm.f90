! log_edit_vc_fm.f90 -- the VC parts of Log Edit mode.
! Ported from GNU Emacs 31.1 lisp/vc/log-edit.el.
!
! Copyright (C) 1999-2026 Free Software Foundation, Inc.
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

!> `log_edit_diff_fileset' and `log_edit_mode_help': a submodule because
!> they use vc_fm (`vc_diff'), vc_dispatcher_fm (`vc_log_fileset') and
!> help_fns_fm (`describe_function'), which use log_edit_fm.
submodule(log_edit_fm) log_edit_vc_fm
    use vc_fm, only: vc_diff, vc_fileset_type
    use vc_dispatcher_fm, only: vc_log_fileset
    use help_fns_fm, only: describe_function
    implicit none(type, external)

contains

    module procedure log_edit_mode_help
    status = status_type()
    if (value_eq(last_command, sym("log_edit_mode_help"))) then
        call describe_function(buffers(cur)%major_mode, status)
    else
        call message(substitute_command_keys("Type `\[log_edit_done]' to finish commit.  "// &
                                             "Try `\[describe_function] log_edit_done' for more help."))
    end if
    end procedure log_edit_mode_help

    module procedure log_edit_diff_fileset
    type(vc_fileset_type) :: fs
    logical :: found
    ! Re NOT-ESSENTIAL non-nil: this function can get called from
    ! `log-edit-hook' and we don't want to abort the whole Log Edit setup
    ! because the user says no to saving a buffer.  The buffers will
    ! still actually get saved before committing, by the
    ! `vc-log-operation' anonymous function.  Possibly
    ! `log-edit-maybe-show-diff' should catch the error instead.
    fs%backend = log_edit_vc_backend()
    call vc_log_fileset(fs%files, found)
    call vc_diff(status, not_essential=.true., fileset=fs)
    end procedure log_edit_diff_fileset
end submodule log_edit_vc_fm
