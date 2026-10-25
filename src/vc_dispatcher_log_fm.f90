! vc_dispatcher_log_fm.f90 -- the log entry buffer of VC operations.
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

!> `vc_start_logentry', `vc_log_edit' and `vc_finish_logentry': a
!> submodule because they use log_edit_fm and vc_fm (`vc_root_dir'),
!> which use vc_dispatcher_fm.
submodule(vc_dispatcher_fm) vc_dispatcher_log_fm
    use log_edit_fm, only: log_edit, log_edit_empty_buffer_p, log_edit_string_type, log_edit_diff_fileset, &
                           log_edit_hook
    use vc_fm, only: vc_root_dir
    use window_fm, only: quit_windows_on
    use buffer_fm, only: restore_buffer_modified_p
    use editfns_fm, only: buffer_string
    implicit none(type, external)

    ! The FILESET and BACKEND that `vc_log_edit' gives the Log Edit buffer
    ! (by `set_params').
    type(vc_string_type), allocatable :: pending_fileset(:)

contains

    module procedure vc_start_logentry
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
    if (size(files) == 1 .and. .not. vc_dispatcher_browsing()) parent = get_file_buffer(files(1)%s)
    if (parent%index == 0) parent = current_buffer()
    immediate = has_comment .and. .not. ic
    call get_buffer_create(logbuf, logb, status)
    if (.not. status%ok()) return
    if (immediate) then
        call set_buffer(logb, status)
    else
        call pop_to_buffer(logb, status)
    end if
    if (.not. status%ok()) return
    call set_vc_parent_buffer(parent, " from "//buffers(parent%index)%name)
    if (present(patch_string)) call set_vc_patch_string(patch_string)
    ! `log-edit-hook' is usually for things like
    ! `log-edit-show-files' and `log-edit-maybe-show-diff' which
    ! don't make sense if the user is not going to do any
    ! editing, and can cause unexpected window layout changes.
    if (immediate) then
        saved_hook = log_edit_hook
        call log_edit_hook%clear()
    end if
    call vc_log_edit(files, status, mode, backend, diff_function)
    if (immediate) log_edit_hook = saved_hook
    if (.not. status%ok()) return
    k = log_local_index(.true.)
    log_locals(k)%after_hook => null()
    if (present(after_hook)) log_locals(k)%after_hook => after_hook
    call set_vc_log_operation(action)
    if (has_comment) then
        call erase_buffer(status)
        if (.not. status%ok()) return
        if (present(comment)) call insert(comment, status)
        if (.not. status%ok()) return
    end if
    if (immediate) then
        call vc_finish_logentry(status, cmt)
    else
        call message(substitute_keys("%s  Type \<log_edit_mode_map>\[log_edit_done] when done", msg))
    end if
    end procedure vc_start_logentry

    ! (substitute-command-keys FORMAT) with %s replaced by MSG.
    function substitute_keys(format, msg) result(text)
        use help_fm, only: substitute_command_keys
        character(len=*), intent(in) :: format, msg
        character(len=:), allocatable :: text
        integer :: i
        text = substitute_command_keys(format)
        i = index(text, "%s")
        if (i > 0) text = text(1:i - 1)//msg//text(i + 2:)
    end function substitute_keys

    module procedure vc_log_edit
    type(vc_string_type), allocatable :: current(:)
    type(handle_type) :: parent
    logical :: setup, has
    status = status_type()
    parent = vc_parent_buffer()
    buffers(cur)%directory = buffers(parent%index)%directory
    ! Setup a new log message if the log buffer is "empty",
    ! or was previously used for a different set of files.
    setup = log_edit_empty_buffer_p()
    if (.not. setup) then
        call vc_log_fileset(current, has)
        if (has) setup = .not. same_files(current, fileset)
    end if
    pending_fileset = fileset
    ! (cond (diff-function) (vc-patch-string #'log-edit-diff-patch)
    !       (t #'log-edit-diff-fileset)): patches are deferred.
    if (present(diff_function)) then
        call call_log_edit(diff_function)
    else
        call call_log_edit(log_edit_diff_fileset)
    end if
    if (.not. status%ok()) return
    ! (set-buffer-modified-p nil)
    call restore_buffer_modified_p(.false.)
    if (allocated(buffers(cur)%filename)) deallocate (buffers(cur)%filename)
contains
    recursive subroutine call_log_edit(diff)
        procedure(command_proc) :: diff
        integer :: b
        b = 0
        if (present(backend)) b = backend
        if (present(mode)) then
            call log_edit(vc_finish_logentry_command, status, setup=setup, listfun=log_listfun, &
                          diff_function=diff, vc_backend=b, set_params=set_params, mode=mode)
        else
            call log_edit(vc_finish_logentry_command, status, setup=setup, listfun=log_listfun, &
                          diff_function=diff, vc_backend=b, set_params=set_params)
        end if
    end subroutine call_log_edit
    end procedure vc_log_edit

    ! Whether the file lists A and B are equal.
    logical function same_files(a, b)
        type(vc_string_type), intent(in) :: a(:), b(:)
        integer :: i
        same_files = .false.
        if (size(a) /= size(b)) return
        do i = 1, size(a)
            if (a(i)%s /= b(i)%s) return
        end do
        same_files = .true.
    end function same_files

    ! The PARAMS of `vc_log_edit': vc-log-fileset (and vc-patch-string,
    ! already set).
    recursive subroutine set_params(status)
        type(status_type), intent(out) :: status
        status = status_type()
        call set_vc_log_fileset(pending_fileset)
    end subroutine set_params

    ! log-edit-listfun of `vc_log_edit': the files of `vc_log_fileset'
    ! relative to the root.
    recursive subroutine log_listfun(files, status)
        use files_fm, only: file_relative_name
        type(log_edit_string_type), allocatable, intent(out) :: files(:)
        type(status_type), intent(out) :: status
        type(vc_string_type), allocatable :: fileset(:)
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
        call vc_root_dir(root, found, status)
        if (.not. status%ok()) return
        call vc_log_fileset(fileset, has)
        ! Returns paths relative to the root, so that
        ! `log-edit-changelog-insert-entries'
        ! substitutes them in correctly later, even when
        ! `vc-checkin' was called from a file buffer, or
        ! a non-root VC-Dir buffer.
        allocate (files(size(fileset)))
        do i = 1, size(fileset)
            if (found) then
                files(i)%s = file_relative_name(fileset(i)%s, root)
            else
                files(i)%s = file_relative_name(fileset(i)%s, buffers(cur)%directory)
            end if
        end do
    end subroutine log_listfun

    ! log-edit-callback: (call-interactively 'vc-finish-logentry).
    recursive subroutine vc_finish_logentry_command(status)
        type(status_type), intent(out) :: status
        call vc_finish_logentry(status)
    end subroutine vc_finish_logentry_command

    module procedure vc_finish_logentry
    type(handle_type) :: logbuf, parent, old
    type(vc_log_operation_type) :: operation
    type(vc_string_type), allocatable :: fileset(:)
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
        call vc_logentry_check_hook%run(status)
        if (.not. status%ok()) return
    end if
    k = log_local_index(.false.)
    has = k > 0
    if (has) has = log_locals(k)%has_operation
    if (has) has = associated(log_locals(k)%operation%run)
    if (.not. has) then
        call set_error(status, FM_GENERIC_ERROR, "No log operation is pending")
        return
    end if
    ! save the parameters held in buffer-local variables
    logbuf = current_buffer()
    operation = log_locals(k)%operation
    call vc_log_fileset(fileset, has)
    log_entry = buffer_string()
    after_hook => log_locals(k)%after_hook
    parent = vc_parent_buffer()
    ! OK, do it to it
    call set_buffer(parent, status)
    if (.not. status%ok()) return
    call operation%run(operation, fileset, log_entry, status)
    if (.not. status%ok()) return
    call pop_to_buffer(parent, status)
    if (.not. status%ok()) return
    ! (setq vc-log-operation nil), in the parent buffer.
    k = log_local_index(.false.)
    if (k > 0) log_locals(k)%has_operation = .false.
    ! Quit windows on logbuf.
    if (buffer_live_p(logbuf)) then
        if (vc_delete_logbuf_window) then
            call quit_windows_on(logbuf, .true., st)
        else
            call quit_windows_on(logbuf, .false., st)
        end if
    end if
    ! Now make sure we see the expanded headers.
    if (buffer_live_p(parent)) then
        old = current_buffer()
        call set_buffer(parent, status)
        if (.not. status%ok()) return
        do k = 1, size(fileset)
            call vc_resynch_buffer(fileset(k)%s, buffers(cur)%directory, status, keep=.true., noquery=VC_NOQUERY_T)
            if (.not. status%ok()) return
        end do
        if (associated(after_hook)) then
            call after_hook%run(status)
            if (.not. status%ok()) return
        end if
        call vc_finish_logentry_hook%run(status)
        if (.not. status%ok()) return
        if (buffer_live_p(old)) call set_buffer(old, st)
    end if
    end procedure vc_finish_logentry
end submodule vc_dispatcher_log_fm
