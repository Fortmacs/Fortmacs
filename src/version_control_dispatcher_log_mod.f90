! version_control_dispatcher_log_mod.f90 -- the log entry buffer of VERSION_CONTROL operations.
! Ported from GNU Emacs 31.1 lisp/vc/vc-dispatcher.el.
!
! Copyright (C) 2007-2026 Free Software Foundation, Inc.
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

!> `version_control_start_logentry', `version_control_log_edit' and `version_control_finish_logentry': a
!> submodule because they use log_edit_mod and version_control_mod (`version_control_root_directory'),
!> which use version_control_dispatcher_mod.
submodule(version_control_dispatcher_mod) version_control_dispatcher_log_mod
    use log_edit_mod, only: log_edit, is_log_edit_empty_buffer, log_edit_diff_fileset, &
                            log_edit_hook
    use version_control_mod, only: version_control_root_directory
    use window_mod, only: quit_windows_on
    use buffer_mod, only: restore_buffer_modified
    use editing_functions_mod, only: buffer_string
    use string_type_mod, only: string_type
    use help_mod, only: substitute_command_keys
    implicit none(type, external)

    ! The FILESET and BACKEND that `version_control_log_edit' gives the Log Edit buffer
    ! (by `set_parameters').
    type(string_type), allocatable :: pending_fileset(:)

contains

    module procedure version_control_start_logentry
    type(handle_type) :: parent, logb
    type(hook_type) :: saved_hook
    logical :: immediate, has_comment, ic, cmt
    integer :: k

    status = status_type()
    cmt = .false.
    if (present(comment_t)) cmt = comment_t
    has_comment = present(comment) .or. cmt
    ic = .false.
    if (present(initial_contents)) ic = initial_contents
    parent = null_handle()
    if (size(files) == 1 .and. .not. version_control_dispatcher_browsing()) parent = get_file_buffer(files(1)%text)
    if (parent%index == 0) parent = current_buffer()
    immediate = has_comment .and. .not. ic
    call get_or_make_buffer(logbuf, logb, status)
    if (.not. status%ok()) return

    if (immediate) then
        call set_buffer(logb, status)
    else
        call pop_to_buffer(logb, status)
    end if

    if (.not. status%ok()) return
    call set_version_control_parent_buffer(parent, " from "//buffers(parent%index)%name)
    if (present(patch_string)) call set_version_control_patch_string(patch_string)

    ! `log-edit-hook' is usually for things like
    ! `log-edit-show-files' and `log-edit-maybe-show-diff' which
    ! don't make sense if the user is not going to do any
    ! editing, and can cause unexpected window layout changes.
    if (immediate) then
        saved_hook = log_edit_hook
        call log_edit_hook%clear()
    end if

    call version_control_log_edit(files, status, mode, backend, diff_function)
    if (immediate) log_edit_hook = saved_hook
    if (.not. status%ok()) return
    call make_log_local_entry(k)
    log_locals(k)%after_hook => null()
    if (present(after_hook)) log_locals(k)%after_hook => after_hook
    call set_version_control_log_operation(action)

    if (has_comment) then
        call erase_buffer(status)
        if (.not. status%ok()) return
        if (present(comment)) call insert(comment, status)
        if (.not. status%ok()) return
    end if

    if (immediate) then
        call version_control_finish_logentry(status, cmt)
    else
        call message(log_message//substitute_command_keys("  Type \<log_edit_mode_map>\[log_edit_done] when done"))
    end if
    end procedure version_control_start_logentry

    module procedure version_control_log_edit
    type(string_type), allocatable :: current_fileset(:)
    type(handle_type) :: parent
    logical :: setup, has

    status = status_type()
    parent = version_control_parent_buffer()
    buffers(current)%directory = buffers(parent%index)%directory
    ! Setup a new log message if the log buffer is "empty",
    ! or was previously used for a different set of files.
    setup = is_log_edit_empty_buffer()

    if (.not. setup) then
        call version_control_log_fileset(current_fileset, has)
        if (has) setup = .not. same_files(current_fileset, fileset)
    end if

    pending_fileset = fileset

    ! (cond (diff-function) (vc-patch-string #'log-edit-diff-patch)
    !       (t #'log-edit-diff-fileset)): patches are deferred.
    if (present(diff_function)) then
        call execute_log_edit(diff_function)
    else
        call execute_log_edit(log_edit_diff_fileset)
    end if

    if (.not. status%ok()) return
    ! (set-buffer-modified-p nil)
    call restore_buffer_modified(.false.)
    if (allocated(buffers(current)%filename)) deallocate (buffers(current)%filename)

contains

    recursive subroutine execute_log_edit(diff)
        procedure(command_procedure) :: diff
        integer :: b

        b = 0
        if (present(backend)) b = backend

        if (present(mode)) then
            call log_edit(version_control_finish_logentry_command, status, setup=setup, listfun=log_listfun, &
                          diff_function=diff, version_control_backend=b, set_parameters=set_parameters, mode=mode)
        else
            call log_edit(version_control_finish_logentry_command, status, setup=setup, listfun=log_listfun, &
                          diff_function=diff, version_control_backend=b, set_parameters=set_parameters)
        end if
    end subroutine execute_log_edit
    end procedure version_control_log_edit

    ! Whether the file lists A and B are equal.  Not a port.
    logical function same_files(a, b)
        type(string_type), intent(in) :: a(:), b(:)
        integer :: i

        same_files = .false.
        if (size(a) /= size(b)) return

        do i = 1, size(a)
            if (a(i)%text /= b(i)%text) return
        end do

        same_files = .true.
    end function same_files

    ! The PARAMETERS of `version_control_log_edit': vc-log-fileset (and vc-patch-string,
    ! already set).  Port of the PARAMETERS of vc-dispatcher.el:vc-log-edit.
    recursive subroutine set_parameters(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call set_version_control_log_fileset(pending_fileset)
    end subroutine set_parameters

    ! log-edit-listfun of `version_control_log_edit': the files of `version_control_log_fileset'
    ! relative to the root.  Port of the log-edit-listfun lambda of
    ! vc-dispatcher.el:vc-log-edit.
    recursive subroutine log_listfun(files, status)
        use files_mod, only: file_relative_name
        type(string_type), allocatable, intent(out) :: files(:)
        type(status_type), intent(out) :: status
        type(string_type), allocatable :: fileset(:)
        character(len=:), allocatable :: root
        logical :: found, has
        integer :: i

        ! FIXME: When fileset includes directories, and
        ! there are relevant ChangeLog files inside their
        ! children, we don't find them.  Either handle it
        ! in `log-edit-insert-changelog-entries' by
        ! walking down the file trees, or somehow pass
        ! `fileset-only-files' from `vc-next-action'
        ! through to this function.
        call version_control_root_directory(root, found, status)
        if (.not. status%ok()) return
        call version_control_log_fileset(fileset, has)
        ! Returns paths relative to the root, so that
        ! `log-edit-changelog-insert-entries'
        ! substitutes them in correctly later, even when
        ! `vc-checkin' was called from a file buffer, or
        ! a non-root VC-Dir buffer.
        allocate (files(size(fileset)))

        do i = 1, size(fileset)
            if (found) then
                files(i)%text = file_relative_name(fileset(i)%text, root)
            else
                files(i)%text = file_relative_name(fileset(i)%text, buffers(current)%directory)
            end if
        end do
    end subroutine log_listfun

    ! log-edit-callback: (call-interactively 'vc-finish-logentry).  Port of
    ! the log-edit-callback lambda of vc-dispatcher.el:vc-log-edit.
    recursive subroutine version_control_finish_logentry_command(status)
        type(status_type), intent(out) :: status

        call version_control_finish_logentry(status)
    end subroutine version_control_finish_logentry_command

    module procedure version_control_finish_logentry
    type(handle_type) :: logbuf, parent, old
    type(version_control_log_operation_type) :: operation
    type(string_type), allocatable :: fileset(:)
    type(hook_type), pointer :: after_hook
    character(len=:), allocatable :: log_entry
    type(status_type) :: st
    logical :: nc, has
    integer :: k

    status = status_type()
    nc = .false.
    if (present(nocomment)) nc = nocomment

    ! Check and record the comment, if any.
    if (.not. nc) then
        call version_control_logentry_check_hook%execute(status)
        if (.not. status%ok()) return
    end if

    k = log_local_index()
    has = k > 0
    if (has) has = log_locals(k)%has_operation
    if (has) has = associated(log_locals(k)%operation%execute)

    if (.not. has) then
        call set_error(status, GENERIC_ERROR_CODE, "No log operation is pending")
        return
    end if

    ! save the parameters held in buffer-local variables
    logbuf = current_buffer()
    operation = log_locals(k)%operation
    call version_control_log_fileset(fileset, has)
    log_entry = buffer_string()
    after_hook => log_locals(k)%after_hook
    parent = version_control_parent_buffer()
    ! OK, do it to it
    call set_buffer(parent, status)
    if (.not. status%ok()) return
    call operation%execute(operation, fileset, log_entry, status)
    if (.not. status%ok()) return
    call pop_to_buffer(parent, status)
    if (.not. status%ok()) return
    ! (setq vc-log-operation nil), in the parent buffer.
    k = log_local_index()
    if (k > 0) log_locals(k)%has_operation = .false.

    ! Quit windows on logbuf.
    if (is_buffer_live(logbuf)) then
        if (version_control_delete_logbuf_window) then
            call quit_windows_on(logbuf, .true., st)
        else
            call quit_windows_on(logbuf, .false., st)
        end if
    end if

    ! Now make sure we see the expanded headers.
    if (is_buffer_live(parent)) then
        old = current_buffer()
        call set_buffer(parent, status)
        if (.not. status%ok()) return

        do k = 1, size(fileset)
            call version_control_resynch_buffer(fileset(k)%text, buffers(current)%directory, status, keep=.true., &
                                                noquery=VERSION_CONTROL_NOQUERY_YES)
            if (.not. status%ok()) return
        end do

        if (associated(after_hook)) then
            call after_hook%execute(status)
            if (.not. status%ok()) return
        end if

        call version_control_finish_logentry_hook%execute(status)
        if (.not. status%ok()) return
        if (is_buffer_live(old)) call set_buffer(old, st)
    end if
    end procedure version_control_finish_logentry
end submodule version_control_dispatcher_log_mod
