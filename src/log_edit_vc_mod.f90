! log_edit_vc_mod.f90 -- the VC parts of Log Edit mode.
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

!> `log_edit_diff_fileset', `log_edit_mode_help' and
!> `generate_log_edit_changelog_from_diff': a submodule because they use
!> vc_mod (`vc_diff'), vc_dispatcher_mod (`vc_log_fileset'), help_fns_mod
!> (`describe_function') and diff_mode_mod
!> (`diff_add_log_current_subprograms'), which use log_edit_mod.
submodule(log_edit_mod) log_edit_vc_mod
    use vc_mod, only: vc_diff, vc_fileset_type
    use vc_dispatcher_mod, only: vc_log_fileset
    use help_fns_mod, only: describe_function
    use diff_mode_mod, only: diff_add_log_current_subprograms
    use add_log_mod, only: change_log_entry_type, insert_change_log_entries
    use window_mod, only: display_action_type, display_buffer_overriding_action, display_buffer_no_window
    implicit none(type, external)

    !> The buffer `log_edit_show_diff' displayed, while
    !> `generate_log_edit_changelog_from_diff' runs it.
    type(handle_type) :: shown_diff_buffer

contains

    module procedure log_edit_mode_help
    status = status_type()

    if (associated(last_command, commands%log_edit_mode_help)) then
        call describe_function(buffers(current)%major_mode, status)
    else
        call message(substitute_command_keys("Type `\[log_edit_done]' to finish commit.  "// &
                                             "Try `\[describe_function] log_edit_done' for more help."))
    end if
    end procedure log_edit_mode_help

    module procedure log_edit_diff_fileset
    type(vc_fileset_type) :: fs
    logical :: found

    ! Re NOT-ESSENTIAL .true.: this function can get called from
    ! `log-edit-hook' and we don't want to abort the whole Log Edit setup
    ! because the user says no to saving a buffer.  The buffers will
    ! still actually get saved before committing, by the
    ! `vc-log-operation' anonymous function.  Possibly
    ! `log-edit-maybe-show-diff' should catch the error instead.
    fs%backend = log_edit_vc_backend()
    call vc_log_fileset(fs%files, found)
    call vc_diff(status, not_essential=.true., fileset=fs)
    end procedure log_edit_diff_fileset

    module procedure generate_log_edit_changelog_from_diff
    type(change_log_entry_type), allocatable :: entries(:)
    type(display_action_type) :: saved_action
    type(handle_type) :: log_buffer
    type(status_type) :: st
    integer(int64) :: opoint, eoh
    logical :: single_line_summary

    log_buffer = current_buffer()
    ! Unfortunately, `log-edit-show-diff' doesn't have a
    ! NO-SHOW option, so we try to work around it via
    ! display-buffer machinery.
    shown_diff_buffer = null_handle()
    saved_action = display_buffer_overriding_action
    display_buffer_overriding_action = display_action_type(has_allow_no_window=.true., allow_no_window=.true.)
    display_buffer_overriding_action%action_function => capture_diff_buffer
    call log_edit_show_diff(status)
    display_buffer_overriding_action = saved_action

    if (status%ok()) then
        call set_buffer(shown_diff_buffer, status)
        if (status%ok()) call diff_add_log_current_subprograms(entries, status)
    end if

    call set_buffer(log_buffer, st)
    if (.not. status%ok()) return
    single_line_summary = .false.

    if (size(entries) == 1) then
        if (size(entries(1)%subprograms) < 2) then
            opoint = point()
            call rfc822_goto_eoh(st)
            eoh = point()
            call goto_char(opoint)

            if (opoint < eoh) then
                call beginning_of_line()
                single_line_summary = looking_at("Summary:", st)
                call goto_char(opoint)
            end if
        end if
    end if

    call insert_change_log_entries(entries, status)
    if (.not. status%ok()) return

    if (single_line_summary) then
        call delete_region(point() - 1, point(), status)
        if (status%ok()) call insert(" ", status)
    end if
    end procedure generate_log_edit_changelog_from_diff

    !> The action function `generate_log_edit_changelog_from_diff' gives
    !> `display_buffer_overriding_action': remember the BUFFER shown, then
    !> `display_buffer_no_window'.  Not a port: the procedure of
    !> log-edit.el:log-edit-generate-changelog-from-diff that does this.
    recursive subroutine capture_diff_buffer(buffer, table, window, failed, status)
        type(handle_type), intent(in) :: buffer
        type(display_action_parameters_type), intent(in) :: table
        type(handle_type), intent(out) :: window
        logical, intent(out) :: failed
        type(status_type), intent(out) :: status

        shown_diff_buffer = buffer
        call display_buffer_no_window(buffer, table, window, failed, status)
    end subroutine capture_diff_buffer
end submodule log_edit_vc_mod
