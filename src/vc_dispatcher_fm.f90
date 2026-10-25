! vc_dispatcher_fm.f90 -- generic command-dispatcher facility.
!
! Copyright (C) 2008-2026 Free Software Foundation, Inc.
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

!> Running VC's back-end commands: `vc_do_command' (synchronously, or
!> asynchronously with the output inserted by `vc_process_filter'),
!> `vc_setup_buffer', running code after a command's process is done
!> (`vc_exec_after', with a `vc_action_type' standing for the closure),
!> `vc_parent_buffer', and bringing buffers back in sync with their files
!> after a VC operation (`vc_resynch_buffer', `vc_buffer_sync',
!> `vc_revert_buffer_internal'), and the log entry buffer of an operation
!> such as a commit (`vc_start_logentry', `vc_log_edit',
!> `vc_finish_logentry', in the submodule vc_dispatcher_log_fm).
!>
!> Deferred: `vc_filter_command_function' and `vc_edit_next_command'
!> (C-x v !), `vc_post_command_functions', `vc_tor', the separate
!> standard error of asynchronous commands (it is mixed with their
!> output), `vc_do_async_command' and VC-Compilation mode (pulling and
!> pushing), `vc_set_async_update', `vc_wait_for_process_before_save',
!> asynchronous log operations (`vc_async_checkin'), and View mode's state
!> after `vc_resynch_window'.
module vc_dispatcher_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use error_fm, only: status_type, set_error, FM_GENERIC_ERROR
    use handle_fm, only: handle_type, null_handle
    use lisp_fm, only: value_type, nil, t_value, make_string, make_list, make_buffer_ref, fixnump, xfixnum, &
                       stringp, xstring
    use lread_fm, only: sym
    use data_fm, only: symbol_value, set, kill_all_local_variables
    use buffer_fm, only: buffers, cur, current_buffer, set_buffer, get_buffer_create, get_buffer, buffer_live_p, &
                         erase_buffer, kill_buffer, get_file_buffer, buffer_list, resolve_buffer
    use editfns_fm, only: point, goto_char, point_min, point_max, widen, buffer_substring, mark_position
    use insdel_fm, only: insert
    use marker_fm, only: marker_position, set_marker
    use search_fm, only: search_forward, SEARCH_NOERROR
    use fileio_fm, only: expand_file_name, file_name_absolute_p, file_exists_p, file_directory_p
    use files_fm, only: file_relative_name, revert_buffer, save_buffer
    use callproc_fm, only: call_process
    use process_fm, only: make_process, process_arg_type, get_buffer_process, delete_process, process_status, &
                          process_exit_status, process_buffer, process_mark, set_process_filter, &
                          set_process_sentinel, accept_process_output, PROCESS_CONNECTION_PIPE
    use window_fm, only: pop_to_buffer, shrink_window_if_larger_than_buffer
    use xdisp_fm, only: message, set_mode_line_process, clear_mode_line_process, mode_line_part_type, &
                        add_minor_mode_lighter
    use subr_fm, only: y_or_n_p
    use vc_hooks_fm, only: vc_string_type, vc_file_clearprops
    use commands_fm, only: command_proc
    use eval_fm, only: hook_type
    use buffer_fm, only: buffer_serial_live
    implicit none(type, external)
    private

    public :: vc_action_type, vc_action_proc, vc_do_command, vc_setup_buffer, vc_exec_after, vc_process_filter, &
              vc_delistify, vc_resynch_window, vc_resynch_buffer, vc_resynch_buffers_in_directory, vc_buffer_sync, &
              vc_revert_buffer_internal, vc_parent_buffer, set_vc_parent_buffer, vc_parent_buffer_name, &
              vc_mode_line_hook_run, syms_of_vc_dispatcher, vc_start_logentry, vc_finish_logentry, vc_log_edit, &
              vc_log_fileset, set_vc_log_fileset, vc_patch_string, set_vc_patch_string, set_vc_log_operation, &
              set_vc_log_after_operation_hook, vc_dispatcher_browsing
    public :: VC_OKSTATUS_NIL, VC_OKSTATUS_T, VC_OKSTATUS_ASYNC, VC_DEST_DISCARD, VC_DEST_CURRENT, &
              VC_NOQUERY_NIL, VC_NOQUERY_T, VC_NOQUERY_UNLESS_MODIFIED

    !> Values of `vc_command_messages' (nil, t, `log').
    integer, parameter, public :: VC_COMMAND_MESSAGES_NIL = 0, VC_COMMAND_MESSAGES_T = 1, VC_COMMAND_MESSAGES_LOG = 2

    !> If true, display and log messages about running back-end commands.
    !> If the value is VC_COMMAND_MESSAGES_LOG, messages about running VC back-end commands are
    !> logged in the *Messages* buffer, but not displayed.  Port of
    !> vc-dispatcher.el vc-command-messages.
    integer, public :: vc_command_messages = VC_COMMAND_MESSAGES_NIL

    !> If true, treat user as expert; suppress yes-no prompts on some things.
    !> Port of vc-dispatcher.el vc-suppress-confirm.
    logical, public :: vc_suppress_confirm = .false.

    !> If true, display VC async command buffers when the command fails.
    !> Considers VC async commands to have failed whenever they die due to a
    !> fatal signal or exit with a non-zero status.  Port of
    !> vc-dispatcher.el vc-display-failed-async-commands.
    logical, public :: vc_display_failed_async_commands = .false.

    !> Where a sentinel wants point to go afterwards (0 for nil).  Port
    !> of vc-dispatcher.el vc-sentinel-movepoint.
    integer(int64), public :: vc_sentinel_movepoint = 0

    !> `vc_do_command' OKSTATUS values besides a maximum exit status:
    !> nil (an error only if the command is killed by a signal), t
    !> (never an error) and `async' (don't wait for the process).
    integer, parameter :: VC_OKSTATUS_NIL = -1, VC_OKSTATUS_T = -2, VC_OKSTATUS_ASYNC = -3

    !> `vc_do_command' destinations besides a buffer: nil (discard the
    !> output) and t (the current buffer).
    integer, parameter :: VC_DEST_DISCARD = 0, VC_DEST_CURRENT = 1

    !> `vc_resynch_buffer' NOQUERY values: nil, t and `unless-modified'.
    integer, parameter :: VC_NOQUERY_NIL = 0, VC_NOQUERY_T = 1, VC_NOQUERY_UNLESS_MODIFIED = 2

    !> Code run after a command's process is done (the closure CODE of
    !> `vc_exec_after'): its procedure, given the action itself and so the
    !> buffers and strings it captured, and OKSTATUS (a maximum exit
    !> status, or VC_OKSTATUS_NIL for none).
    type :: vc_action_type
        procedure(vc_action_proc), pointer, nopass :: run => null()
        type(handle_type) :: buffer, buffer2
        character(len=:), allocatable :: s1, s2
        type(vc_string_type), allocatable :: files(:)
        integer :: n = 0
        logical :: flag = .false.
        integer :: okstatus = VC_OKSTATUS_NIL
    end type vc_action_type

    abstract interface
        recursive subroutine vc_action_proc(action, status)
            import :: vc_action_type, status_type
            implicit none(type, external)
            type(vc_action_type), intent(in) :: action
            type(status_type), intent(out) :: status
        end subroutine vc_action_proc
    end interface

    !> The actions waiting for a process.
    type :: vc_pending_type
        type(handle_type) :: process
        type(vc_action_type), allocatable :: actions(:)
    end type vc_pending_type

    type(vc_pending_type), allocatable :: pending(:)

    !> A buffer's `vc_parent_buffer' and `vc_parent_buffer_name'
    !> (permanent-local), by serial number.
    type :: vc_parent_type
        integer(int64) :: serial = 0
        type(handle_type) :: parent
        character(len=:), allocatable :: name
    end type vc_parent_type

    type(vc_parent_type), allocatable :: parents(:)

    !> A bit of the text around a position: the position, the buffer size
    !> and the text (vc-position-context's list).
    type :: vc_context_type
        integer(int64) :: posn = 0, size = 0
        character(len=:), allocatable :: text
        logical :: valid = .false.
    end type vc_context_type

    !> A procedure run with the visited file of a buffer resynched
    !> (vc-mode-line-hook; set by vc_fm to `vc_mode_line').
    abstract interface
        subroutine vc_mode_line_hook_proc(file)
            implicit none(type, external)
            character(len=*), intent(in) :: file
        end subroutine vc_mode_line_hook_proc
    end interface

    !> `vc_mode_line_hook': run by `vc_resynch_window' (vc_fm sets it).
    procedure(vc_mode_line_hook_proc), pointer, public :: vc_mode_line_hook => null()

    !> The operation of a log entry buffer (the ACTION of
    !> `vc_start_logentry', upstream's closure): its procedure, given the
    !> operation itself and so what it captured (a BACKEND, a revision
    !> REV, a PATCH_STRING, files to REGISTER), and FILES and COMMENT.
    type, public :: vc_log_operation_type
        procedure(vc_log_operation_proc), pointer, nopass :: run => null()
        integer :: backend = 0
        character(len=:), allocatable :: rev, patch_string
        type(vc_string_type), allocatable :: register(:)
    end type vc_log_operation_type

    abstract interface
        recursive subroutine vc_log_operation_proc(operation, files, comment, status)
            import :: vc_log_operation_type, vc_string_type, status_type
            implicit none(type, external)
            type(vc_log_operation_type), intent(in) :: operation
            type(vc_string_type), intent(in) :: files(:)
            character(len=*), intent(in) :: comment
            type(status_type), intent(out) :: status
        end subroutine vc_log_operation_proc
    end interface
    public :: vc_log_operation_proc

    !> A log entry buffer's `vc_log_operation', `vc_log_fileset',
    !> `vc_patch_string' and `vc_log_after_operation_hook', by serial
    !> number.
    type :: vc_log_local_type
        integer(int64) :: serial = 0
        logical :: has_operation = .false.
        type(vc_log_operation_type) :: operation
        logical :: has_fileset = .false.
        type(vc_string_type), allocatable :: fileset(:)
        character(len=:), allocatable :: patch_string
        type(hook_type), pointer :: after_hook => null()
    end type vc_log_local_type

    type(vc_log_local_type), allocatable :: log_locals(:)

    !> Normal hook run by `vc_finish_logentry'.
    !> Use this to impose your own rules on the entry in addition to any the
    !> dispatcher client wants to enforce.  Port of vc-dispatcher.el
    !> vc-logentry-check-hook.
    type(hook_type), target, public :: vc_logentry_check_hook = hook_type(name="vc_logentry_check_hook")

    !> Hook run at the end of `vc_finish_logentry' (obsolete; use
    !> `vc_log_after_operation_hook').  Port of vc-dispatcher.el
    !> vc-finish-logentry-hook.
    type(hook_type), target, public :: vc_finish_logentry_hook = hook_type(name="vc_finish_logentry_hook")

    !> If true, delete the *vc-log* buffer and window after each logical action.
    !> If false, bury that buffer instead.
    !> This is most useful if you have multiple windows on a frame and would like to
    !> preserve the setting.  Port of vc-dispatcher.el vc-delete-logbuf-window.
    logical, public :: vc_delete_logbuf_window = .true.

    interface
        !> Accept a comment for an operation on FILES.
        !> If COMMENT is absent, pop up a LOGBUF buffer, emit MSG, and set the
        !> action on close to ACTION.  If COMMENT is a string and
        !> INITIAL_CONTENTS is true, then COMMENT is used as the initial
        !> contents of the log entry buffer.  If COMMENT is a string and
        !> INITIAL_CONTENTS is false, do action immediately as if the user had
        !> entered COMMENT.  If COMMENT_T (COMMENT t), also do action
        !> immediately with an empty comment.  Remember the file's buffer in
        !> `vc_parent_buffer' (current one if no file).  Puts the log-entry
        !> buffer in major mode MODE, defaulting to `log_edit_mode' if MODE is absent.
        !> AFTER_HOOK specifies the local value for `vc_log_after_operation_hook'.
        !> BACKEND, if not 0, specifies a VC backend for the Log Edit buffer.
        !> PATCH_STRING is a patch to check in.
        !> DIFF_FUNCTION is `log_edit_diff_function' for the Log Edit buffer.
        !> Port of vc-dispatcher.el:vc-start-logentry.  (In
        !> vc_dispatcher_log_fm.)
        recursive module subroutine vc_start_logentry(files, msg, logbuf, action, status, comment, comment_t, &
                                                      initial_contents, mode, after_hook, backend, patch_string, &
                                                      diff_function)
            implicit none(type, external)
            type(vc_string_type), intent(in) :: files(:)
            character(len=*), intent(in) :: msg, logbuf
            type(vc_log_operation_type), intent(in) :: action
            type(status_type), intent(out) :: status
            character(len=*), intent(in), optional :: comment, patch_string
            logical, intent(in), optional :: comment_t, initial_contents
            procedure(command_proc), optional :: mode, diff_function
            type(hook_type), intent(in), target, optional :: after_hook
            integer, intent(in), optional :: backend
        end subroutine vc_start_logentry

        !> Complete the operation implied by the current log entry.
        !> Use the contents of the current buffer as a check-in or registration
        !> comment.  If the optional arg NOCOMMENT is true, then don't check
        !> the buffer contents as a comment.  Port of
        !> vc-dispatcher.el:vc-finish-logentry (synchronous operations; the
        !> asynchronous ones of `vc_async_checkin' are deferred).  (In
        !> vc_dispatcher_log_fm.)
        recursive module subroutine vc_finish_logentry(status, nocomment)
            implicit none(type, external)
            type(status_type), intent(out) :: status
            logical, intent(in), optional :: nocomment
        end subroutine vc_finish_logentry

        !> Set up `log_edit' for use on FILESET.  Port of
        !> vc-dispatcher.el:vc-log-edit.  (In vc_dispatcher_log_fm.)
        recursive module subroutine vc_log_edit(fileset, status, mode, backend, diff_function)
            implicit none(type, external)
            type(vc_string_type), intent(in) :: fileset(:)
            type(status_type), intent(out) :: status
            procedure(command_proc), optional :: mode, diff_function
            integer, intent(in), optional :: backend
        end subroutine vc_log_edit
    end interface

contains

    ! ------------------------------------------------------------------
    ! The parent buffer
    ! ------------------------------------------------------------------

    integer function parent_index(create) result(k)
        logical, intent(in) :: create
        type(vc_parent_type), allocatable :: grown(:)
        integer :: n
        k = 0
        if (cur == 0) return
        if (.not. allocated(parents)) allocate (parents(0))
        do k = 1, size(parents)
            if (parents(k)%serial == buffers(cur)%serial) return
        end do
        k = 0
        if (.not. create) return
        n = size(parents)
        allocate (grown(n + 1))
        grown(1:n) = parents
        grown(n + 1)%serial = buffers(cur)%serial
        call move_alloc(grown, parents)
        k = n + 1
    end function parent_index

    !> The current buffer's `vc_parent_buffer': in a log entry or output
    !> buffer, the buffer for which it was made (a null handle for nil).
    function vc_parent_buffer() result(h)
        type(handle_type) :: h
        integer :: k
        h = null_handle()
        k = parent_index(.false.)
        if (k > 0) h = parents(k)%parent
    end function vc_parent_buffer

    !> The current buffer's `vc_parent_buffer_name' (" from NAME").
    function vc_parent_buffer_name() result(name)
        character(len=:), allocatable :: name
        integer :: k
        name = ""
        k = parent_index(.false.)
        if (k == 0) return
        if (allocated(parents(k)%name)) name = parents(k)%name
    end function vc_parent_buffer_name

    !> The `vc_parent_buffer_name' of buffer slot BI, shown in the mode
    !> line while it has a `vc_parent_buffer' (vc.el's minor-mode-alist
    !> entry (vc-parent-buffer vc-parent-buffer-name)).
    function parent_name_of(bi) result(text)
        integer(int64), intent(in) :: bi
        character(len=:), allocatable :: text
        integer :: k
        text = ""
        if (.not. allocated(parents)) return
        do k = 1, size(parents)
            if (parents(k)%serial /= buffers(bi)%serial) cycle
            if (parents(k)%parent%index == 0) return
            if (allocated(parents(k)%name)) text = parents(k)%name
            return
        end do
    end function parent_name_of

    !> Show `vc_parent_buffer_name' in the mode line of buffers with a
    !> `vc_parent_buffer'.  Part of vc.el (its minor-mode-alist entry).
    subroutine syms_of_vc_dispatcher()
        use data_fm, only: add_local_variables_killer
        call add_minor_mode_lighter("vc_parent_buffer", "", text=parent_name_of, first=.true.)
        call add_local_variables_killer(kill_log_locals)
    end subroutine syms_of_vc_dispatcher

    ! ------------------------------------------------------------------
    ! The log entry buffer's variables
    ! ------------------------------------------------------------------

    ! The index of the current buffer's entry in `log_locals', made if
    ! MAKE (0 if none).
    integer function log_local_index(make) result(k)
        logical, intent(in) :: make
        type(vc_log_local_type), allocatable :: grown(:)
        integer :: i, n
        k = 0
        if (cur == 0) return
        if (.not. allocated(log_locals)) allocate (log_locals(0))
        do k = 1, size(log_locals)
            if (log_locals(k)%serial == buffers(cur)%serial) return
        end do
        k = 0
        if (.not. make) return
        do k = 1, size(log_locals)
            if (.not. buffer_serial_live(log_locals(k)%serial)) then
                log_locals(k) = vc_log_local_type(serial=buffers(cur)%serial)
                return
            end if
        end do
        n = size(log_locals)
        allocate (grown(n + 1))
        do i = 1, n
            grown(i) = log_locals(i)
        end do
        grown(n + 1)%serial = buffers(cur)%serial
        call move_alloc(grown, log_locals)
        k = n + 1
    end function log_local_index

    ! kill-all-local-variables: forget the current buffer's values.
    subroutine kill_log_locals()
        integer :: k
        k = log_local_index(.false.)
        if (k > 0) log_locals(k) = vc_log_local_type()
    end subroutine kill_log_locals

    !> The current buffer's `vc_log_fileset' (FOUND false if it has none):
    !> the files of the log entry.
    subroutine vc_log_fileset(files, found)
        type(vc_string_type), allocatable, intent(out) :: files(:)
        logical, intent(out) :: found
        integer :: k
        found = .false.
        k = log_local_index(.false.)
        if (k > 0) found = log_locals(k)%has_fileset
        if (found) then
            files = log_locals(k)%fileset
        else
            allocate (files(0))
        end if
    end subroutine vc_log_fileset

    !> Set the current buffer's `vc_log_fileset' to FILES.
    subroutine set_vc_log_fileset(files)
        type(vc_string_type), intent(in) :: files(:)
        integer :: k
        k = log_local_index(.true.)
        log_locals(k)%has_fileset = .true.
        log_locals(k)%fileset = files
    end subroutine set_vc_log_fileset

    !> The current buffer's `vc_patch_string' (unallocated for nil).
    subroutine vc_patch_string(patch)
        character(len=:), allocatable, intent(out) :: patch
        integer :: k
        k = log_local_index(.false.)
        if (k == 0) return
        if (allocated(log_locals(k)%patch_string)) patch = log_locals(k)%patch_string
    end subroutine vc_patch_string

    !> Set the current buffer's `vc_patch_string' to PATCH (nil if absent).
    subroutine set_vc_patch_string(patch)
        character(len=*), intent(in), optional :: patch
        integer :: k
        k = log_local_index(.true.)
        if (allocated(log_locals(k)%patch_string)) deallocate (log_locals(k)%patch_string)
        if (present(patch)) log_locals(k)%patch_string = patch
    end subroutine set_vc_patch_string

    !> Set the current buffer's `vc_log_operation' to OPERATION.
    subroutine set_vc_log_operation(operation)
        type(vc_log_operation_type), intent(in) :: operation
        integer :: k
        k = log_local_index(.true.)
        log_locals(k)%has_operation = .true.
        log_locals(k)%operation = operation
    end subroutine set_vc_log_operation

    !> Set the current buffer's `vc_log_after_operation_hook' to HOOK (nil
    !> if absent).
    subroutine set_vc_log_after_operation_hook(hook)
        type(hook_type), intent(in), target, optional :: hook
        integer :: k
        k = log_local_index(.true.)
        log_locals(k)%after_hook => null()
        if (present(hook)) log_locals(k)%after_hook => hook
    end subroutine set_vc_log_after_operation_hook

    !> Are we in a directory browser buffer?  Port of
    !> vc-dispatcher.el:vc-dispatcher-browsing.
    logical function vc_dispatcher_browsing() result(browsing)
        use derived_fm, only: derived_mode_p
        browsing = derived_mode_p([character(len=13) :: "vc_dir_mode", "dired_mode", "diff_mode", "log_view_mode"])
    end function vc_dispatcher_browsing

    !> Set the current buffer's `vc_parent_buffer' to PARENT and its
    !> `vc_parent_buffer_name' to NAME.
    subroutine set_vc_parent_buffer(parent, name)
        type(handle_type), intent(in) :: parent
        character(len=*), intent(in) :: name
        integer :: k
        k = parent_index(.true.)
        if (k == 0) return
        parents(k)%parent = parent
        parents(k)%name = name
    end subroutine set_vc_parent_buffer

    ! ------------------------------------------------------------------
    ! Common command execution logic
    ! ------------------------------------------------------------------

    !> An alternative output filter for async process PROC.
    !> One difference with the default filter is that this inserts S after markers.
    !> Another is that undo information is not kept.  Port of
    !> vc-dispatcher.el:vc-process-filter.
    recursive subroutine vc_process_filter(proc, s, status)
        type(handle_type), intent(in) :: proc
        character(len=*), intent(in) :: s
        type(status_type), intent(out) :: status
        type(handle_type) :: buffer, old, mark
        type(value_type) :: saved
        type(status_type) :: st
        integer(int64) :: opoint, mpos
        status = status_type()
        buffer = process_buffer(proc)
        if (.not. buffer_live_p(buffer)) return
        old = current_buffer()
        call set_buffer(buffer, st)
        opoint = point()
        mark = process_mark(proc)
        call marker_position(mark, mpos, st)
        call symbol_value(sym("inhibit_read_only"), saved, st)
        call set(sym("inhibit_read_only"), t_value(), st)
        call goto_char(mpos)
        call insert(s, status)
        call set_marker(mark, point(), st)
        call set(sym("inhibit_read_only"), saved, st)
        ! (save-excursion): point stays, after the text if it was at the mark.
        if (opoint > mpos) opoint = opoint + len(s, int64)
        call goto_char(min(opoint, point_max()))
        call set_buffer(old, st)
    end subroutine vc_process_filter

    !> Prepare BUF for executing a slave command and make it current.
    !> Port of vc-dispatcher.el:vc-setup-buffer.
    subroutine vc_setup_buffer(buf, status)
        type(handle_type), intent(in) :: buf
        type(status_type), intent(out) :: status
        type(handle_type) :: camefrom, oldproc
        type(value_type) :: saved
        type(status_type) :: st
        character(len=:), allocatable :: olddir
        camefrom = current_buffer()
        olddir = "./"
        if (cur /= 0) then
            if (allocated(buffers(cur)%directory)) olddir = buffers(cur)%directory
        end if
        call set_buffer(buf, status)
        if (.not. status%ok()) return
        ! If there's some previous async process still running, just kill it.
        oldproc = get_buffer_process(buf)
        ! Use `delete-process' rather than `kill-process' because we don't
        ! want any of its output to appear from now on.
        if (oldproc%index /= 0) call delete_process(oldproc, st)
        call kill_all_local_variables()
        ! If we are refreshing an existing view,
        ! don't throw away where we really came from (bug#59457).
        if (camefrom%index /= cur .and. buffer_live_p(camefrom)) then
            call set_vc_parent_buffer(camefrom, " from "//buffers(camefrom%index)%name)
        end if
        buffers(cur)%directory = olddir
        call symbol_value(sym("inhibit_read_only"), saved, st)
        call set(sym("inhibit_read_only"), t_value(), st)
        call erase_buffer(st)
        call set(sym("inhibit_read_only"), saved, st)
    end subroutine vc_setup_buffer

    !> " [waiting...]" in the mode line.  Port of
    !> vc-dispatcher.el:vc-set-mode-line-busy-indicator.
    subroutine vc_set_mode_line_busy_indicator()
        type(mode_line_part_type) :: parts(2)
        parts(1)%text = " "
        parts(2)%text = "[waiting...]"
        parts(2)%face = "mode_line_emphasis"
        call set_mode_line_process(parts)
    end subroutine vc_set_mode_line_busy_indicator

    !> Execute ACTION when PROC, or the current buffer's process, is done.
    !>
    !> ACTION's OKSTATUS, if not VC_OKSTATUS_NIL, is a non-negative integer.
    !> Run ACTION only if PROC (or, if PROC is absent, the current buffer's
    !> process) exits normally (i.e. does not die to a signal) with exit
    !> status not exceeding OKSTATUS.
    !>
    !> The optional PROC argument specifies the process to wait for
    !> before executing ACTION.  It defaults to the current buffer's process.
    !> If PROC is absent and the current buffer has no process, just run
    !> ACTION.  Otherwise, add ACTION to the process's sentinel.  Port of
    !> vc-dispatcher.el:vc-exec-after (vc-run-delayed and
    !> vc-run-delayed-success are this with OKSTATUS nil or given).
    recursive subroutine vc_exec_after(action, status, proc)
        type(vc_action_type), intent(in) :: action
        type(status_type), intent(out) :: status
        type(handle_type), intent(in), optional :: proc
        type(handle_type) :: p, buf
        type(status_type) :: st
        character(len=:), allocatable :: pstatus
        logical :: got
        status = status_type()
        if (present(proc)) then
            p = proc
        else
            p = get_buffer_process(current_buffer())
        end if
        if (p%index == 0) then
            ! If there's no background process, just execute the code.
            call run_action(action, p, status)
            return
        end if
        pstatus = process_status(p)
        if (pstatus == "exit" .or. pstatus == "signal") then
            ! Nonblocking call in case we are ourselves called from a
            ! process sentinel.
            call accept_process_output(got, st, p, 0.0)
            call run_action(action, p, status)
        else if (pstatus == "run") then
            buf = process_buffer(p)
            if (buffer_live_p(buf)) call in_buffer_busy(buf)
            call queue_action(p, action)
        else
            call set_error(status, FM_GENERIC_ERROR, "Unexpected process state")
        end if
    end subroutine vc_exec_after

    !> Show the busy indicator in buffer BUF.
    subroutine in_buffer_busy(buf)
        type(handle_type), intent(in) :: buf
        type(handle_type) :: old
        type(status_type) :: st
        old = current_buffer()
        call set_buffer(buf, st)
        call vc_set_mode_line_busy_indicator()
        call set_buffer(old, st)
    end subroutine in_buffer_busy

    !> Run ACTION if its OKSTATUS allows the exit of process P (null for
    !> none).
    recursive subroutine run_action(action, p, status)
        type(vc_action_type), intent(in) :: action
        type(handle_type), intent(in) :: p
        type(status_type), intent(out) :: status
        status = status_type()
        if (.not. associated(action%run)) return
        if (action%okstatus /= VC_OKSTATUS_NIL .and. p%index /= 0) then
            if (process_status(p) /= "exit") return
            if (action%okstatus < process_exit_status(p)) return
        end if
        call action%run(action, status)
    end subroutine run_action

    !> Add ACTION to those run when process P is done.
    subroutine queue_action(p, action)
        type(handle_type), intent(in) :: p
        type(vc_action_type), intent(in) :: action
        type(vc_pending_type), allocatable :: grown(:)
        type(vc_action_type), allocatable :: more(:)
        type(status_type) :: st
        integer :: k, n
        if (.not. allocated(pending)) allocate (pending(0))
        do k = 1, size(pending)
            if (pending(k)%process%index == p%index .and. pending(k)%process%generation == p%generation) exit
        end do
        if (k > size(pending)) then
            n = size(pending)
            allocate (grown(n + 1))
            grown(1:n) = pending
            grown(n + 1)%process = p
            allocate (grown(n + 1)%actions(0))
            call move_alloc(grown, pending)
            k = n + 1
            call set_process_sentinel(p, st, vc_sentinel)
        end if
        n = size(pending(k)%actions)
        allocate (more(n + 1))
        more(1:n) = pending(k)%actions
        more(n + 1) = action
        call move_alloc(more, pending(k)%actions)
    end subroutine queue_action

    !> The sentinel of VC's processes: when one is done, run the actions
    !> waiting for it in its buffer, from its mark, as vc-exec-after's
    !> sentinel function does.
    recursive subroutine vc_sentinel(proc, event, status)
        type(handle_type), intent(in) :: proc
        character(len=*), intent(in) :: event
        type(status_type), intent(out) :: status
        type(vc_action_type), allocatable :: actions(:)
        type(handle_type) :: buf, old, m
        type(status_type) :: st
        character(len=:), allocatable :: pstatus
        integer(int64) :: opoint, mpos
        integer :: k, j
        status = status_type()
        if (len(event) < 0) return
        do k = 1, size(pending)
            if (pending(k)%process%index == proc%index .and. pending(k)%process%generation == proc%generation) exit
        end do
        if (k > size(pending)) return
        pstatus = process_status(proc)
        if (pstatus /= "exit" .and. pstatus /= "signal") return
        call move_alloc(pending(k)%actions, actions)
        call drop_pending(k)
        buf = process_buffer(proc)
        ! Impatient users sometime kill "slow" buffers; check liveness to
        ! avoid "error in process sentinel: Selecting deleted buffer".
        if (.not. buffer_live_p(buf)) return
        old = current_buffer()
        call set_buffer(buf, st)
        call clear_mode_line_process()
        do j = 1, size(actions)
            vc_sentinel_movepoint = 0
            m = process_mark(proc)
            call marker_position(m, mpos, st)
            ! Normally, we want async code such as sentinels to not move
            ! point.
            opoint = point()
            call goto_char(mpos)
            ! Each sentinel may move point and the next one should be run
            ! from that new position.
            call run_action(actions(j), proc, status)
            if (.not. buffer_live_p(buf)) exit
            call set_buffer(buf, st)
            call set_marker(m, point(), st)
            call goto_char(min(opoint, point_max()))
            ! But sometimes the sentinels really want to move point.
            if (vc_sentinel_movepoint /= 0) call goto_char(vc_sentinel_movepoint)
            if (.not. status%ok()) exit
        end do
        call set_buffer(old, st)
    end subroutine vc_sentinel

    ! Remove PENDING(K) (without an emptying array constructor).
    subroutine drop_pending(k)
        integer, intent(in) :: k
        type(vc_pending_type), allocatable :: rest(:)
        integer :: i, j
        allocate (rest(size(pending) - 1))
        j = 0
        do i = 1, size(pending)
            if (i == k) cycle
            j = j + 1
            rest(j) = pending(i)
        end do
        call move_alloc(rest, pending)
    end subroutine drop_pending

    !> Smash a FILELIST into a file list string suitable for info messages.
    !> Port of vc-dispatcher.el:vc-delistify.
    function vc_delistify(filelist) result(s)
        type(vc_string_type), intent(in) :: filelist(:)
        character(len=:), allocatable :: s
        integer :: k
        if (size(filelist) == 0) then
            s = "."
            return
        end if
        s = ""
        do k = 1, size(filelist)
            if (k > 1) s = s//" "
            if (allocated(filelist(k)%s)) s = s//filelist(k)%s
        end do
    end function vc_delistify

    !> Call `message' on TEXT taking into account relevant VC variables.
    !> Port of vc-dispatcher.el:vc--command-message.
    subroutine vc_command_message(text)
        character(len=*), intent(in) :: text
        if (vc_command_messages /= VC_COMMAND_MESSAGES_NIL) call message(text)
    end subroutine vc_command_message

    !> Execute an inferior command, notifying user and checking for errors.
    !>
    !> DESTINATION specifies what to do with COMMAND's output: VC_DEST_CURRENT
    !> for the current buffer, VC_DEST_DISCARD to discard output, or (with
    !> BUFFER) a buffer to insert output there.
    !> STDERR false means to discard standard error output, true (the
    !> default) to mix it with standard output.
    !> If the destination for standard output is a buffer that is not the
    !> current buffer, set up the buffer properly and erase it.
    !>
    !> OKSTATUS VC_OKSTATUS_ASYNC means not to wait for termination of the
    !> subprocess and give the process object as PROCESS.  Otherwise, OKSTATUS
    !> determines when to signal an error instead of giving a numeric exit
    !> status as EXIT_STATUS (or a signal description).  OKSTATUS an integer
    !> means to signal an error if the command's exit status exceeds that
    !> value or the command is killed by a signal, VC_OKSTATUS_NIL means to
    !> signal an error only if the command is killed by a signal, and
    !> VC_OKSTATUS_T means never to signal an error.
    !>
    !> FILES are the working files (none to execute commands that don't
    !> expect a file name or set of files).  FLAGS are inserted into the
    !> command line before the filename (an unallocated one is nil and
    !> passes nothing).  ENVIRONMENT are "NAME=VALUE" entries put first in
    !> the process's environment (as `process_environment').  Port of
    !> vc-dispatcher.el:vc-do-command.
    recursive subroutine vc_do_command(destination, okstatus, command, files, flags, status, buffer, stderr, &
                                       exit_status, process, environment)
        integer, intent(in) :: destination
        integer, intent(in) :: okstatus
        character(len=*), intent(in) :: command
        type(vc_string_type), intent(in) :: files(:), flags(:)
        type(status_type), intent(out) :: status
        type(handle_type), intent(in), optional :: buffer
        logical, intent(in), optional :: stderr
        integer, intent(out), optional :: exit_status
        type(handle_type), intent(out), optional :: process
        type(vc_string_type), intent(in), optional :: environment(:)
        type(handle_type) :: old, proc
        type(vc_string_type), allocatable :: rel(:)
        type(value_type), allocatable :: argv(:), envv(:)
        type(process_arg_type), allocatable :: pargs(:), penv(:)
        type(value_type) :: result, saved, dest
        type(status_type) :: st
        character(len=:), allocatable :: full_command, cmd, expanded, desc
        logical :: to_buffer, mix
        integer :: k, n, ne, code
        status = status_type()
        if (present(exit_status)) exit_status = 0
        if (present(process)) process = null_handle()
        mix = .true.
        if (present(stderr)) mix = stderr
        old = current_buffer()
        to_buffer = destination /= VC_DEST_DISCARD
        if (present(buffer)) then
            to_buffer = .true.
            if (buffer%index /= cur) then
                call vc_setup_buffer(buffer, status)
                if (.not. status%ok()) return
            end if
        end if
        ! FIXME: file-relative-name can return a bogus result because it
        ! doesn't look at the actual file-system to see if symlinks come
        ! into play.
        allocate (rel(size(files)))
        do k = 1, size(files)
            call expand_file_name(files(k)%s, expanded, st)
            if (.not. st%ok()) expanded = files(k)%s
            rel(k)%s = file_relative_name(expanded)
        end do
        cmd = command
        if (len(cmd) > 0) then
            if (cmd(len(cmd):len(cmd)) == achar(10)) cmd = cmd(1:len(cmd) - 1)
        end if
        full_command = cmd//" "//flag_list(flags)
        if (size(rel) > 0) full_command = full_command//" "//vc_delistify(rel)
        ! The squeezed arguments: the flags without nil, then the files.
        n = 0
        do k = 1, size(flags)
            if (allocated(flags(k)%s)) n = n + 1
        end do
        allocate (argv(n + size(rel)))
        n = 0
        do k = 1, size(flags)
            if (.not. allocated(flags(k)%s)) cycle
            n = n + 1
            argv(n) = make_string(flags(k)%s)
        end do
        do k = 1, size(rel)
            argv(n + k) = make_string(rel(k)%s)
        end do
        ! Since some functions need to parse the output from external
        ! commands, set LC_MESSAGES to C.
        ne = 1
        if (present(environment)) ne = ne + size(environment)
        allocate (envv(ne))
        envv(1) = make_string("LC_MESSAGES=C")
        if (present(environment)) then
            do k = 1, size(environment)
                envv(k + 1) = make_string(environment(k)%s)
            end do
        end if
        call symbol_value(sym("inhibit_read_only"), saved, st)
        call set(sym("inhibit_read_only"), t_value(), st)
        if (okstatus == VC_OKSTATUS_ASYNC) then
            ! Run asynchronously.
            allocate (pargs(size(argv) + 1))
            pargs(1)%s = cmd
            do k = 1, size(argv)
                pargs(k + 1)%s = xstring(argv(k))
            end do
            allocate (penv(ne))
            do k = 1, ne
                penv(k)%s = xstring(envv(k))
            end do
            if (to_buffer) then
                call make_process(cmd, pargs, proc, status, buffer=current_buffer(), &
                                  connection_type=PROCESS_CONNECTION_PIPE, environment=penv)
            else
                call make_process(cmd, pargs, proc, status, connection_type=PROCESS_CONNECTION_PIPE, environment=penv)
            end if
            if (status%ok()) then
                call set_process_filter(proc, st, vc_process_filter)
                call vc_command_message("Running in background: "//full_command)
                if (present(process)) process = proc
            end if
        else
            ! Run synchronously
            call vc_command_message("Running in foreground: "//full_command)
            dest = t_value()
            if (.not. to_buffer) then
                call call_process(cmd, result, status, args=make_list(argv), environment=make_list(envv))
            else if (mix) then
                call call_process(cmd, result, status, destination=dest, args=make_list(argv), &
                                  environment=make_list(envv))
            else
                call call_process(cmd, result, status, destination=dest, args=make_list(argv), &
                                  error_file="/dev/null", environment=make_list(envv))
            end if
            if (status%ok()) then
                code = 0
                if (fixnump(result)) code = int(xfixnum(result))
                if (present(exit_status)) exit_status = code
                if (okstatus /= VC_OKSTATUS_T .and. (.not. fixnump(result) .or. &
                                                     (okstatus >= 0 .and. okstatus < code))) then
                    if (buffers(cur)%name(1:min(1, len(buffers(cur)%name))) /= " ") then
                        call pop_to_buffer(current_buffer(), st)
                        call goto_char(point_min())
                        call shrink_window_if_larger_than_buffer()
                    end if
                    if (fixnump(result)) then
                        desc = "status "//itoa(code)
                    else
                        desc = xstring(result)
                    end if
                    call set_error(status, FM_GENERIC_ERROR, "Failed ("//desc//"): "//full_command)
                else
                    call vc_command_message("Done (status="//itoa(code)//"): "//full_command)
                end if
            end if
        end if
        call set(sym("inhibit_read_only"), saved, st)
        if (present(buffer) .or. destination /= VC_DEST_CURRENT) call set_buffer(old, st)
    contains

        !> The flags as `vc_delistify' smashes them, a nil one as "".
        function flag_list(list) result(s)
            type(vc_string_type), intent(in) :: list(:)
            character(len=:), allocatable :: s
            integer :: i
            s = ""
            do i = 1, size(list)
                if (i > 1) s = s//" "
                if (allocated(list(i)%s)) s = s//list(i)%s
            end do
        end function flag_list
    end subroutine vc_do_command

    !> N as a decimal string.
    function itoa(n) result(s)
        integer, intent(in) :: n
        character(len=:), allocatable :: s
        character(len=16) :: buf
        write (buf, "(i0)") n
        s = trim(buf)
    end function itoa

    ! ------------------------------------------------------------------
    ! Keeping the view up to date
    ! ------------------------------------------------------------------

    !> Save a bit of the text around POSN in the current buffer.
    !> Used to help us find the corresponding position again later
    !> if markers are destroyed or corrupted.  Port of
    !> vc-dispatcher.el:vc-position-context.
    function vc_position_context(posn) result(context)
        integer(int64), intent(in) :: posn
        type(vc_context_type) :: context
        type(status_type) :: st
        context%posn = posn
        context%size = point_max() - point_min()
        call buffer_substring(posn, min(point_max(), posn + 100), context%text, st)
        if (.not. st%ok()) context%text = ""
        context%valid = .true.
    end function vc_position_context

    !> Return the position of CONTEXT in the current buffer (0 if
    !> CONTEXT cannot be found).  Port of
    !> vc-dispatcher.el:vc-find-position-by-context.
    integer(int64) function vc_find_position_by_context(context) result(pos)
        type(vc_context_type), intent(in) :: context
        integer(int64) :: diff, saved, found
        type(status_type) :: st
        if (len(context%text) == 0) then
            pos = point_max()
            return
        end if
        saved = point()
        pos = 0
        diff = context%size - (point_max() - point_min())
        if (diff < 0) diff = -diff
        call goto_char(context%posn)
        call search_forward(context%text, found, st, noerror=SEARCH_NOERROR)
        if (found == 0) then
            ! Can't use search-backward since the match may continue after
            ! point.
            call goto_char(point() - diff - len(context%text, int64))
            ! goto-char doesn't signal an error at beginning of buffer like
            ! backward-char would
            call search_forward(context%text, found, st, noerror=SEARCH_NOERROR)
        end if
        ! to beginning of OSTRING
        if (found /= 0) pos = point() - text_chars(context%text)
        call goto_char(saved)
    end function vc_find_position_by_context

    !> The number of characters of UTF-8 TEXT.
    integer(int64) function text_chars(text) result(n)
        character(len=*), intent(in) :: text
        integer :: k
        n = 0
        do k = 1, len(text)
            if (iand(iachar(text(k:k)), 192) /= 128) n = n + 1
        end do
    end function text_chars

    !> Return true if POSN matches CONTEXT, false otherwise.  Port of
    !> vc-dispatcher.el:vc-context-matches-p.
    logical function vc_context_matches_p(posn, context) result(r)
        integer(int64), intent(in) :: posn
        type(vc_context_type), intent(in) :: context
        character(len=:), allocatable :: s
        type(status_type) :: st
        integer(int64) :: endp
        r = .false.
        endp = posn + text_chars(context%text)
        if (endp > point_max() - point_min() + 1 + 1) return
        if (endp > point_max()) return
        call buffer_substring(posn, endp, s, st)
        if (.not. st%ok()) return
        r = s == context%text .and. len(s) == len(context%text)
    end function vc_context_matches_p

    !> Revert buffer keeping point and the mark where the user expects them.
    !> Try to be clever in the face of changes due to expanded VCS
    !> keywords (cf., e.g., Info node `(cvs)Keyword substitution').
    !> This is important for typeahead to work as expected.
    !> ARG and NO_CONFIRM are passed on to `revert_buffer'.  Port of
    !> vc-dispatcher.el:vc-revert-buffer-internal (with vc-buffer-context
    !> and vc-restore-buffer-context).
    recursive subroutine vc_revert_buffer_internal(status, arg, no_confirm)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: arg, no_confirm
        type(vc_context_type) :: point_context, mark_context
        integer(int64) :: saved, newpos, m
        call widen()
        point_context = vc_position_context(point())
        ! Use mark-marker to avoid confusion in transient-mark-mode.
        m = mark_position()
        if (m /= 0) mark_context = vc_position_context(m)
        ! Use save-excursion here, because it may be able to restore point
        ! and mark properly even in cases where vc-restore-buffer-context
        ! would fail.  However, save-excursion might also get it wrong --
        ! in this case, vc-restore-buffer-context gives it a second try.
        saved = point()
        ! t means don't call normal-mode; that's to preserve various minor
        ! modes.
        call revert_buffer(status, ignore_auto=arg, noconfirm=no_confirm, preserve_modes=.true.)
        if (.not. status%ok()) return
        call goto_char(min(max(saved, point_min()), point_max()))
        ! if necessary, restore point and mark
        if (.not. vc_context_matches_p(point(), point_context)) then
            newpos = vc_find_position_by_context(point_context)
            if (newpos /= 0) call goto_char(newpos)
        end if
        if (mark_context%valid .and. buffers(cur)%mark_active) then
            m = mark_position()
            if (.not. vc_context_matches_p(m, mark_context)) then
                newpos = vc_find_position_by_context(mark_context)
                if (newpos /= 0) call set_mark_at(newpos)
            end if
        end if
    end subroutine vc_revert_buffer_internal

    !> (set-mark POS)
    subroutine set_mark_at(pos)
        use simple_fm, only: set_mark
        integer(int64), intent(in) :: pos
        call set_mark(pos)
    end subroutine set_mark_at

    !> Run `vc_mode_line_hook' with FILE.
    subroutine vc_mode_line_hook_run(file)
        character(len=*), intent(in) :: file
        if (associated(vc_mode_line_hook)) call vc_mode_line_hook(file)
    end subroutine vc_mode_line_hook_run

    !> The name FILE is visited under: itself if absolute, else expanded
    !> from ROOT.
    function resynch_name(file, root) result(name)
        character(len=*), intent(in) :: file, root
        character(len=:), allocatable :: name
        type(status_type) :: st
        if (file_name_absolute_p(file)) then
            name = file
        else
            call expand_file_name(file, name, st, root)
            if (.not. st%ok()) name = file
        end if
    end function resynch_name

    !> If FILE is in the current buffer, either revert or unvisit it.
    !> The choice between revert (to see expanded keywords) and unvisit
    !> depends on KEEP.  NOQUERY if true inhibits confirmation for
    !> reverting.  NOQUERY should be true *only* if it is known the only
    !> difference between the buffer and the file is due to
    !> modifications by the dispatcher client code, rather than user
    !> editing!  ROOT is `vc_root_dir', for relative names.  Port of
    !> vc-dispatcher.el:vc-resynch-window.
    recursive subroutine vc_resynch_window(file, root, status, keep, noquery, reset_vc_info)
        character(len=*), intent(in) :: file, root
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: keep, noquery, reset_vc_info
        logical :: k
        status = status_type()
        if (cur == 0) return
        if (.not. allocated(buffers(cur)%filename)) return
        if (buffers(cur)%filename /= resynch_name(file, root)) return
        k = .false.
        if (present(keep)) k = keep
        if (.not. k) then
            call kill_buffer(current_buffer(), status)
        else if (file_exists_p(file)) then
            if (present(reset_vc_info)) then
                if (reset_vc_info) call vc_file_clearprops(file)
            end if
            call vc_revert_buffer_internal(status, .true., noquery)
            if (.not. status%ok()) return
            call vc_mode_line_hook_run(buffers(cur)%filename)
        end if
    end subroutine vc_resynch_window

    !> Resync all buffers that visit files in DIRECTORY.  Port of
    !> vc-dispatcher.el:vc-resynch-buffers-in-directory.
    recursive subroutine vc_resynch_buffers_in_directory(directory, root, status, keep, noquery, reset_vc_info)
        character(len=*), intent(in) :: directory, root
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: keep, reset_vc_info
        integer, intent(in), optional :: noquery
        type(handle_type), allocatable :: all(:)
        type(handle_type) :: old
        type(status_type) :: st
        character(len=:), allocatable :: fname
        integer :: k
        status = status_type()
        all = buffer_list()
        old = current_buffer()
        do k = 1, size(all)
            if (.not. buffer_live_p(all(k))) cycle
            if (.not. allocated(buffers(all(k)%index)%filename)) cycle
            fname = buffers(all(k)%index)%filename
            if (index(fname, directory) /= 1) cycle
            call set_buffer(all(k), st)
            call vc_resynch_buffer(fname, root, status, keep, noquery, reset_vc_info)
            if (.not. status%ok()) exit
        end do
        if (buffer_live_p(old)) call set_buffer(old, st)
    end subroutine vc_resynch_buffers_in_directory

    !> If FILE is currently visited, resynch its buffer.
    !> If NOQUERY is VC_NOQUERY_UNLESS_MODIFIED then silently skip reverting or
    !> unvisiting if the buffer is modified.  VC_NOQUERY_T inhibits
    !> getting confirmation before reverting.  NOQUERY should be VC_NOQUERY_T
    !> only if it is known that the only
    !> difference between the buffer and the file is due to modifications by
    !> the dispatcher client code, rather than user editing.  ROOT is
    !> `vc_root_dir', for relative names.  Port of
    !> vc-dispatcher.el:vc-resynch-buffer (no *vc-dir* buffers).
    recursive subroutine vc_resynch_buffer(file, root, status, keep, noquery, reset_vc_info)
        character(len=*), intent(in) :: file, root
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: keep, reset_vc_info
        integer, intent(in), optional :: noquery
        type(handle_type) :: buffer, old
        type(status_type) :: st
        integer :: nq
        logical :: here
        status = status_type()
        nq = VC_NOQUERY_NIL
        if (present(noquery)) nq = noquery
        here = .false.
        if (cur /= 0) then
            if (allocated(buffers(cur)%filename)) here = buffers(cur)%filename == resynch_name(file, root)
        end if
        if (here) then
            if (nq == VC_NOQUERY_UNLESS_MODIFIED .and. modified()) return
            call vc_resynch_window(file, root, status, keep, nq /= VC_NOQUERY_NIL, reset_vc_info)
        else if (file_directory_p(file)) then
            call vc_resynch_buffers_in_directory(file, root, status, keep, noquery, reset_vc_info)
        else
            buffer = get_file_buffer(resynch_name(file, root))
            if (buffer%index == 0) return
            old = current_buffer()
            call set_buffer(buffer, st)
            if (.not. (nq == VC_NOQUERY_UNLESS_MODIFIED .and. modified())) then
                call vc_resynch_window(file, root, status, keep, nq /= VC_NOQUERY_NIL, reset_vc_info)
            end if
            if (buffer_live_p(old)) call set_buffer(old, st)
        end if
    contains

        logical function modified()
            modified = buffers(cur)%text%save_modiff < buffers(cur)%text%modiff
        end function modified
    end subroutine vc_resynch_buffer

    !> Make sure the current buffer and its working file are in sync.
    !> NOT_ESSENTIAL means it is okay to continue if the user says not to save.
    !> Port of vc-dispatcher.el:vc-buffer-sync.
    recursive subroutine vc_buffer_sync(status, not_essential)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: not_essential
        logical :: missing, need, yes, ne
        status = status_type()
        ne = .false.
        if (present(not_essential)) ne = not_essential
        missing = .false.
        need = buffers(cur)%text%save_modiff < buffers(cur)%text%modiff
        if (.not. need) then
            if (allocated(buffers(cur)%filename)) then
                missing = .not. file_exists_p(buffers(cur)%filename)
                need = missing
            end if
        end if
        if (.not. need) return
        yes = vc_suppress_confirm
        if (.not. yes) then
            if (missing) then
                call y_or_n_p("Buffer "//buffers(cur)%name//" is missing on disk; save it? ", yes, status)
            else
                call y_or_n_p("Buffer "//buffers(cur)%name//" modified; save it? ", yes, status)
            end if
            if (.not. status%ok()) return
        end if
        if (yes) then
            call save_buffer(1_int64, .false., status)
        else if (.not. ne) then
            call set_error(status, FM_GENERIC_ERROR, "Aborted")
        end if
    end subroutine vc_buffer_sync
end module vc_dispatcher_fm
