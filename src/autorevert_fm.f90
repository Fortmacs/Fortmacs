! autorevert_fm.f90 -- revert buffers when files on disk change.
! Ported from GNU Emacs 31.1 lisp/autorevert.el.
!
! Copyright (C) 1997-1999, 2001-2026 Free Software Foundation, Inc.
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

!> Auto-Revert Mode, Auto-Revert Tail Mode and Global Auto-Revert Mode:
!> reverting buffers when their files change on disk, by polling every
!> `auto_revert_interval' seconds and through file notifications
!> (filenotify_fm).
!>
!> The buffer-local internal variables (the watch descriptor, the
!> notification state, the lockout timer, `auto_revert_tail_pos') are
!> kept per buffer in this module; the watch descriptor survives
!> `kill_all_local_variables', as its permanent-local property makes it
!> upstream.
!>
!> Deferred: remote files (`auto_revert_remote_files' has no effect),
!> a predicate as `global_auto_revert_ignore_buffer', the :set functions
!> of `auto_revert_interval', `auto_revert_use_notify' and
!> `auto_revert_avoid_polling' (Customize), `auto_revert_debug'
!> messages, indirect buffers (there are none), `auto_revert_load_hook'
!> (obsolete; nothing is loaded).
module autorevert_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64, real64
    use error_fm, only: status_type, set_error, format_error, FM_GENERIC_ERROR
    use handle_fm, only: handle_type, null_handle
    use lisp_fm, only: value_type, t_value
    use lread_fm, only: sym
    use data_fm, only: logical_setting, forward_setting, add_local_variables_killer, set, symbol_value
    use fns_fm, only: put
    use eval_fm, only: hook_type
    use commands_fm, only: commands
    use modes_fm, only: minor_modes
    use buffer_fm, only: buffers, cur, current_buffer, is_buffer_live, buffer_list, buffer_handle, set_buffer_internal, &
                         restore_buffer_modified
    use window_fm, only: windows, window_leaves, window_point, set_window_point, same_handle
    use frame_fm, only: selected_frame
    use editfns_fm, only: point, point_max, point_min, goto_char, widen, narrow_to_region
    use undo_fm, only: undo_boundary
    use search_fm, only: string_match
    use keyboard_fm, only: is_input_pending
    use timer_fm, only: run_with_timer, free_timer, is_timer, is_timer_active
    use xdisp_fm, only: message
    use fileio_fm, only: expand_file_name, is_file_readable, is_file_symlink, file_attributes, file_stat_type, &
                         file_name_nondirectory, insert_file_contents, set_visited_file_modtime, verify_visited_file_modtime
    use files_fm, only: revert_buffer, find_file_hook, kill_buffer_hook, after_set_visited_file_name_hook, &
                        before_revert_hook, after_revert_hook, revert_buffer_in_progress, buffer_stale_function, &
                        buffer_stale_proc, buffer_stale_default_function, buffer_auto_revert_by_notification, &
                        mounted_file_systems, BUFFER_STALE_NO, BUFFER_STALE_YES, BUFFER_STALE_FAST
    use subr_fm, only: after_change_major_mode_hook, read_y_or_n
    use filenotify_fm, only: file_notify_event_type, file_notify_add_watch, file_notify_rm_watch, &
                             FILE_NOTIFY_CHANGE, FILE_NOTIFY_ATTRIBUTE_CHANGE, FILE_NOTIFY_CREATED, FILE_NOTIFY_DELETED, &
                             FILE_NOTIFY_CHANGED, FILE_NOTIFY_RENAMED, FILE_NOTIFY_ATTRIBUTE_CHANGED, FILE_NOTIFY_STOPPED
    use inotify_fm, only: watch_descriptor_type, operator(==)
    use vc_hooks_fm, only: vc_refresh_state
    implicit none(type, external)
    private

    public :: syms_of_autorevert, auto_revert_mode, auto_revert_tail_mode, global_auto_revert_mode, &
              turn_on_auto_revert_mode, turn_on_auto_revert_tail_mode, auto_revert_set_timer, auto_revert_buffers, &
              auto_revert_buffer, auto_revert_handler, is_auto_revert_active, inhibit_auto_revert, &
              auto_revert_notify_rm_watch, auto_revert_notify_add_watch, auto_revert_notify_watch_descriptor, &
              is_auto_revert_notify_modified, auto_revert_tail_pos, is_auto_revert_timer_active, &
              is_auto_revert_buffer_list

    !> Time, in seconds, between Auto-Revert Mode file checks.
    !> The value may be an integer or floating point number.
    !>
    !> If a timer is already active, there are two ways to make sure
    !> that the new value will take effect immediately.  You can set
    !> this variable through Custom or you can call the command
    !> `auto_revert_set_timer' after setting the variable.  Otherwise,
    !> the new value will take effect the first time Auto-Revert Mode
    !> calls `auto_revert_set_timer' for internal reasons or in your
    !> next editing session.  Port of autorevert.el auto-revert-interval.
    real, public :: auto_revert_interval = 5.0

    !> When true, user input temporarily interrupts Auto-Revert Mode.
    !> With this setting, Auto-Revert Mode checks for user input after
    !> handling each buffer and does not process any further buffers
    !> (until the next run of the timer) if user input is available.
    !> When false, Auto-Revert Mode checks files and reverts buffers,
    !> with quitting disabled, without paying attention to user input.
    !> Thus, with this setting, Emacs might be non-responsive at times.
    !> Port of autorevert.el auto-revert-stop-on-user-input.
    logical, public :: auto_revert_stop_on_user_input = .true.

    !> When false, Auto-Revert Mode does not generate any messages.
    !> When true, a message is generated whenever a buffer is reverted.
    !> Port of autorevert.el auto-revert-verbose.
    logical, public :: auto_revert_verbose = .true.

    !> String to display in the mode line when Auto-Revert Mode is active.
    !> (When the string is not empty, make sure that it has a leading space.)
    !> Port of autorevert.el auto-revert-mode-text.
    character(len=:), allocatable, public :: auto_revert_mode_text

    !> String to display in the mode line when Auto-Revert Tail Mode is active.
    !> (When the string is not empty, make sure that it has a leading space.)
    !> Port of autorevert.el auto-revert-tail-mode-text.
    character(len=:), allocatable, public :: auto_revert_tail_mode_text

    !> Functions to run when Auto-Revert Mode is activated.  Port of
    !> autorevert.el auto-revert-mode-hook.
    type(hook_type), target, public :: auto_revert_mode_hook = hook_type(name="auto_revert_mode_hook")

    !> Hook run after entering or leaving `auto_revert_tail_mode'.  Port
    !> of the hook define-minor-mode makes for autorevert.el's
    !> auto-revert-tail-mode.
    type(hook_type), target, public :: auto_revert_tail_mode_hook = hook_type(name="auto_revert_tail_mode_hook")

    !> String to display when Global Auto-Revert Mode is active.
    !>
    !> The default is nothing since when this mode is active this text doesn't
    !> vary over time, or between buffers.  Hence mode line text
    !> would only waste precious space.  Port of autorevert.el
    !> global-auto-revert-mode-text.
    character(len=:), allocatable, public :: global_auto_revert_mode_text

    !> Hook called when Global Auto-Revert Mode is activated.  Port of
    !> autorevert.el global-auto-revert-mode-hook.
    type(hook_type), target, public :: global_auto_revert_mode_hook = hook_type(name="global_auto_revert_mode_hook")

    !> When false, Global Auto-Revert Mode operates only on file-visiting buffers.
    !>
    !> When true, both file buffers and buffers with a custom
    !> `revert_buffer_function' and a `buffer_stale_function' are
    !> reverted by Global Auto-Revert Mode.  These include the Buffer
    !> List buffer displayed by `buffer_menu', and Dired buffers showing
    !> complete local directories.  The Buffer List buffer reverts every
    !> `auto_revert_interval' seconds; Dired buffers when the file list of
    !> the main directory changes.  Dired buffers do not auto-revert as
    !> a result of changes in subdirectories, or in the contents, size,
    !> modes, etc., of files.  You may still sometimes want to revert
    !> them manually.
    !>
    !> Use this option with care since it could lead to excessive auto-reverts.
    !> For more information, see Info node `(emacs)Auto Revert'.  Port of
    !> autorevert.el global-auto-revert-non-file-buffers.
    logical, public :: global_auto_revert_non_file_buffers = .false.

    !> List of major modes Global Auto-Revert Mode should not check.
    !> (Each name is used without its trailing blanks.)  Port of
    !> autorevert.el global-auto-revert-ignore-modes.
    character(len=:), allocatable, public :: global_auto_revert_ignore_modes(:)

    !> If true Auto-Revert Mode reliably updates version control info.
    !> Auto-Revert Mode updates version control info whenever the buffer
    !> needs reverting, regardless of the value of this variable.
    !> However, the version control state can change without changes to
    !> the work file.  If the change is made from the current Emacs
    !> session, all info is updated.  But if, for instance, a new
    !> version is checked in from outside the current Emacs session, the
    !> version control number in the mode line, as well as other version
    !> control related information, may not be properly updated.  If you
    !> are worried about this, set this variable to true.
    !>
    !> This currently works by automatically updating the version
    !> control info every `auto_revert_interval' seconds.  Nevertheless,
    !> it should not cause excessive CPU usage on a reasonably fast
    !> machine, if it does not apply to too many version controlled
    !> buffers.  CPU usage depends on the version control system.  Port
    !> of autorevert.el auto-revert-check-vc-info.
    logical, public :: auto_revert_check_vc_info = .false.

    !> When true, Global Auto-Revert Mode will not revert this buffer.
    !> Port of autorevert.el global-auto-revert-ignore-buffer (no
    !> predicate function; buffer-local when set).
    type(logical_setting), public :: global_auto_revert_ignore_buffer = &
                                     logical_setting(name="global_auto_revert_ignore_buffer")

    !> If false remote files are not reverted in Auto Revert modes.
    !> Port of autorevert.el auto-revert-remote-files.
    logical, public :: auto_revert_remote_files = .false.

    !> If true Auto-Revert Mode uses file notification functions.
    !> You should set this variable through Custom.  Port of
    !> autorevert.el auto-revert-use-notify.
    type(logical_setting), public :: auto_revert_use_notify = &
                                     logical_setting(name="auto_revert_use_notify", value=.true.)

    !> Regular expression of directories to be excluded from file notifications.
    !> Port of autorevert.el auto-revert-notify-exclude-dir-regexp.
    character(len=:), allocatable, public :: auto_revert_notify_exclude_dir_regexp

    !> True to avoid polling files when notification is available.
    !>
    !> Set this variable to true to save power by avoiding
    !> polling when possible.  Files on file-systems that do not support
    !> change notifications must match `auto_revert_notify_exclude_dir_regexp'
    !> for Auto-Revert to work properly in this case.  This typically
    !> includes files on network file systems on Unix-like machines,
    !> when those files are modified from another computer.
    !>
    !> When false, buffers in Auto-Revert Mode will always be polled for
    !> changes to their files on disk every `auto_revert_interval'
    !> seconds, in addition to using notification for those files.  Port
    !> of autorevert.el auto-revert-avoid-polling.
    logical, public :: auto_revert_avoid_polling = .false.

    !> True if a `auto_revert_buffer' operation is in progress, false otherwise.
    !> Port of autorevert.el auto-revert-buffer-in-progress.
    logical, public :: auto_revert_buffer_in_progress = .false.

    !> Duration, in seconds, of the Auto-Revert Mode notification lockout.
    !> This is the quiescence after each notification of a file being
    !> changed during which no automatic reverting takes place, to
    !> prevent many updates in rapid succession from overwhelming the
    !> system.  Port of autorevert.el auto-revert--lockout-interval.
    real, public :: auto_revert_lockout_interval = 2.5

    !> Timer used by Auto-Revert Mode (auto-revert-timer; the null handle
    !> for none).
    type(handle_type) :: auto_revert_timer

    !> List of buffers in Auto-Revert Mode.
    !>
    !> Note that only Auto-Revert Mode, never Global Auto-Revert Mode, adds
    !> buffers to this list.
    !>
    !> The timer function `auto_revert_buffers' is responsible for purging
    !> the list of old buffers.  Port of autorevert.el
    !> auto-revert-buffer-list (newest first).
    type(handle_type), allocatable :: auto_revert_buffer_list(:)

    !> Buffers not checked when user input stopped execution.  Port of
    !> autorevert.el auto-revert-remaining-buffers.
    type(handle_type), allocatable :: auto_revert_remaining_buffers(:)

    !> A list of buffers with suppressed auto-revert.  Port of
    !> autorevert.el inhibit-auto-revert-buffers.
    type(handle_type), allocatable :: inhibit_auto_revert_buffers(:)

    !> The default `auto_revert_tail_pos': position of last known end of
    !> file.
    integer(int64), parameter :: TAIL_POS_DEFAULT = 0

    !> One buffer's Auto-Revert variables.
    type :: local_type
        logical :: set = .false.
        integer(int64) :: serial = 0
        !> auto-revert-notify-watch-descriptor (permanent): `has_desc'
        !> for non-nil.
        logical :: has_desc = .false.
        type(watch_descriptor_type) :: desc
        !> auto-revert-notify-modified-p: the file has been modified on
        !> the file system, as reported by a file notification event.
        logical :: modified_p = .false.
        !> auto-revert--last-time: the last time the buffer was
        !> reverted, in seconds of `now_seconds' (never: very long ago).
        real(real64) :: last_time = -huge(1.0_real64)
        !> auto-revert--lockout-timer: the timer awaiting the end of the
        !> notification lockout interval (null for none).
        type(handle_type) :: lockout_timer
        !> auto-revert--global-mode: the buffer is handled by Global
        !> Auto-Revert mode.
        logical :: global_mode = .false.
        !> auto-revert-tail-pos, when it has a buffer-local value.
        logical :: tail_pos_local = .false.
        integer(int64) :: tail_pos = TAIL_POS_DEFAULT
    end type local_type
    type(local_type), allocatable :: locals(:)

    !> auto-revert--buffer-by-watch-descriptor: which buffer uses each
    !> notification descriptor.
    type :: watch_buffer_type
        type(watch_descriptor_type) :: desc
        type(handle_type) :: buffer
    end type watch_buffer_type
    type(watch_buffer_type), allocatable :: buffer_by_watch_descriptor(:)

    !> While true, `auto_revert_mode' leaves `auto_revert_tail_mode' on
    !> (the let-binding of auto-revert-tail-mode in auto-revert-tail-mode).
    logical :: tail_mode_binding = .false.

    abstract interface
        !> The BODY of `inhibit_auto_revert'.  Not a port.
        recursive subroutine inhibit_body_proc(status)
            import :: status_type
            implicit none(type, external)
            type(status_type), intent(out) :: status
        end subroutine inhibit_body_proc
    end interface

contains

    ! ------------------------------------------------------------------
    ! Buffer-local variables
    ! ------------------------------------------------------------------

    !> The index of the current buffer's entry in `locals', made if MAKE
    !> (0 if none).  Not a port.
    integer function local_index(make) result(k)
        logical, intent(in) :: make
        type(local_type), allocatable :: grown(:)
        integer :: i, n

        k = 0
        if (cur == 0) return
        if (.not. allocated(locals)) allocate (locals(0))

        do i = 1, size(locals)
            if (locals(i)%set .and. locals(i)%serial == buffers(cur)%serial) then
                k = i
                return
            end if
        end do

        if (.not. make) return

        ! Reuse the entry of a dead buffer.
        do i = 1, size(locals)
            if (locals(i)%set) then
                if (serial_live(locals(i)%serial)) cycle
            end if

            locals(i) = local_type()
            locals(i)%set = .true.
            locals(i)%serial = buffers(cur)%serial
            k = i
            return
        end do

        n = size(locals)
        allocate (grown(n + 1))
        grown(1:n) = locals
        call move_alloc(grown, locals)
        locals(n + 1)%set = .true.
        locals(n + 1)%serial = buffers(cur)%serial
        k = n + 1
    end function local_index

    !> Whether a buffer with SERIAL is live.  Not a port.
    logical function serial_live(serial)
        integer(int64), intent(in) :: serial
        integer(int64) :: b

        serial_live = .false.

        do b = 1, size(buffers, kind=int64)
            if (buffers(b)%serial == serial) then
                serial_live = is_buffer_live(buffer_handle(b))
                return
            end if
        end do
    end function serial_live

    !> kill-all-local-variables: the variables that are not permanent.
    !> Not a port.
    subroutine kill_autorevert_locals()
        integer :: k

        k = local_index(.false.)
        if (k == 0) return
        locals(k)%modified_p = .false.
        locals(k)%last_time = -huge(1.0_real64)
        call free_timer(locals(k)%lockout_timer)
        locals(k)%lockout_timer = null_handle()
        locals(k)%global_mode = .false.
        locals(k)%tail_pos_local = .false.
        locals(k)%tail_pos = TAIL_POS_DEFAULT
    end subroutine kill_autorevert_locals

    !> The file watch descriptor active for the current buffer, in DESC;
    !> false for none.  Port of autorevert.el
    !> auto-revert-notify-watch-descriptor.
    logical function auto_revert_notify_watch_descriptor(desc) result(has)
        type(watch_descriptor_type), intent(out), optional :: desc
        integer :: k

        has = .false.
        k = local_index(.false.)
        if (k == 0) return
        has = locals(k)%has_desc
        if (has .and. present(desc)) desc = locals(k)%desc
    end function auto_revert_notify_watch_descriptor

    !> True when file has been modified on the file system.
    !> This has been reported by a file notification event.  Port of
    !> autorevert.el auto-revert-notify-modified-p.
    logical function is_auto_revert_notify_modified()
        integer :: k

        is_auto_revert_notify_modified = .false.
        k = local_index(.false.)
        if (k /= 0) is_auto_revert_notify_modified = locals(k)%modified_p
    end function is_auto_revert_notify_modified

    !> Position of last known end of file.  Port of autorevert.el
    !> auto-revert-tail-pos.
    integer(int64) function auto_revert_tail_pos()
        integer :: k

        auto_revert_tail_pos = TAIL_POS_DEFAULT
        k = local_index(.false.)
        if (k /= 0) auto_revert_tail_pos = locals(k)%tail_pos
    end function auto_revert_tail_pos

    !> Whether the Auto-Revert timer is active (auto-revert-timer).
    !> Not a port.
    logical function is_auto_revert_timer_active()
        is_auto_revert_timer_active = is_timer(auto_revert_timer)
    end function is_auto_revert_timer_active

    !> Whether BUFFER is in `auto_revert_buffer_list'.  Not a port.
    logical function is_auto_revert_buffer_list(buffer)
        type(handle_type), intent(in) :: buffer

        is_auto_revert_buffer_list = memq(buffer, auto_revert_buffer_list)
    end function is_auto_revert_buffer_list

    !> The size of FILE (file-attribute-size of file-attributes); 0 if
    !> it can't be read.  Not a port.
    integer(int64) function file_size(file)
        character(len=*), intent(in) :: file
        type(file_stat_type) :: st
        logical :: found

        file_size = 0
        call file_attributes(file, st, found, nofollow=.true.)
        if (found) file_size = st%size
    end function file_size

    !> The current time in seconds (current-time, as float-time).  Not a port.
    real(real64) function now_seconds()
        integer(int64) :: count, rate

        call system_clock(count, rate)
        now_seconds = real(count, real64)/real(max(rate, 1_int64), real64)
    end function now_seconds

    ! ------------------------------------------------------------------
    ! Lists of buffers
    ! ------------------------------------------------------------------

    ! Not a port.
    logical function memq(buffer, list)
        type(handle_type), intent(in) :: buffer
        type(handle_type), allocatable, intent(in) :: list(:)
        integer :: k

        memq = .false.
        if (.not. allocated(list)) return

        do k = 1, size(list)
            if (same_handle(list(k), buffer)) then
                memq = .true.
                return
            end if
        end do
    end function memq

    !> (delq BUFFER LIST)  Not a port.
    subroutine delq(buffer, list)
        type(handle_type), intent(in) :: buffer
        type(handle_type), allocatable, intent(inout) :: list(:)
        type(handle_type), allocatable :: kept(:)
        integer :: k, n

        if (.not. allocated(list)) return
        allocate (kept(size(list)))
        n = 0

        do k = 1, size(list)
            if (same_handle(list(k), buffer)) cycle
            n = n + 1
            kept(n) = list(k)
        end do

        list = kept(1:n)
    end subroutine delq

    !> (push BUFFER LIST)  Not a port.
    subroutine push(buffer, list)
        type(handle_type), intent(in) :: buffer
        type(handle_type), allocatable, intent(inout) :: list(:)
        type(handle_type), allocatable :: grown(:)

        if (.not. allocated(list)) allocate (list(0))
        allocate (grown(size(list) + 1))
        grown(1) = buffer
        grown(2:) = list
        call move_alloc(grown, list)
    end subroutine push

    !> (with-current-buffer BUFFER ...): make BUFFER current, keeping the
    !> current buffer in SAVED.  Not a port.
    subroutine enter_buffer(buffer, saved)
        type(handle_type), intent(in) :: buffer
        integer(int64), intent(out) :: saved

        saved = cur
        call set_buffer_internal(buffer%index)
    end subroutine enter_buffer

    !> The end of `enter_buffer'.  Not a port.
    subroutine leave_buffer(saved)
        integer(int64), intent(in) :: saved

        if (saved > 0) then
            if (is_buffer_live(buffer_handle(saved))) call set_buffer_internal(saved)
        end if
    end subroutine leave_buffer

    !> Remove BUFFER from `auto_revert_buffer_list'.
    !> BUFFER defaults to `current_buffer'.  Port of
    !> autorevert.el:auto-revert-remove-current-buffer.
    subroutine auto_revert_remove_current_buffer(buffer)
        type(handle_type), intent(in), optional :: buffer

        if (present(buffer)) then
            call delq(buffer, auto_revert_buffer_list)
        else
            call delq(current_buffer(), auto_revert_buffer_list)
        end if
    end subroutine auto_revert_remove_current_buffer

    !> `auto_revert_remove_current_buffer' as a kill-buffer-hook function.
    !> Not a port.
    recursive subroutine remove_current_buffer_hook(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call auto_revert_remove_current_buffer()
    end subroutine remove_current_buffer_hook

    ! ------------------------------------------------------------------
    ! The modes
    ! ------------------------------------------------------------------

    !> The find-file-hook function: remember the size of the file.  Port of
    !> autorevert.el:auto-revert-find-file-function.
    recursive subroutine auto_revert_find_file_function(status)
        type(status_type), intent(out) :: status
        integer :: k

        status = status_type()
        if (.not. allocated(buffers(cur)%filename)) return
        k = local_index(.true.)
        locals(k)%tail_pos_local = .true.
        locals(k)%tail_pos = file_size(buffers(cur)%filename)
    end subroutine auto_revert_find_file_function

    !> Toggle reverting buffer when the file changes (Auto-Revert Mode).
    !>
    !> Auto-Revert Mode is a minor mode that affects only the current
    !> buffer.  When enabled, it reverts the buffer when the file on
    !> disk changes.
    !>
    !> When a buffer is reverted, a message is generated.  This can be
    !> suppressed by setting `auto_revert_verbose' to false.
    !>
    !> Reverting can sometimes fail to preserve all the markers in the buffer.
    !> To avoid that, set `revert_buffer_insert_file_contents_function' to
    !> the slower function `revert_buffer_insert_file_contents_delicately'.
    !>
    !> Use `global_auto_revert_mode' to automatically revert all buffers.
    !> Use `auto_revert_tail_mode' if you know that the file will only grow
    !> without being changed in the part that is already in the buffer.
    !> Port of autorevert.el:auto-revert-mode.
    recursive subroutine auto_revert_mode(status)
        type(status_type), intent(out) :: status

        call minor_modes%auto_revert_mode%command(status)
    end subroutine auto_revert_mode

    !> The body of Auto-Revert Mode.
    !> Port of autorevert.el:auto-revert-mode (its body).
    recursive subroutine auto_revert_mode_body(enable, status)
        logical, intent(in) :: enable
        type(status_type), intent(out) :: status

        status = status_type()

        if (enable) then
            if (.not. memq(current_buffer(), auto_revert_buffer_list)) then
                call push(current_buffer(), auto_revert_buffer_list)
                call kill_buffer_hook%add(remove_current_buffer_hook, local=.true.)
            end if
        else
            if (auto_revert_notify_watch_descriptor()) call auto_revert_notify_rm_watch()
            call auto_revert_remove_current_buffer()
        end if

        call auto_revert_set_timer(status)
        if (.not. status%ok()) return

        if (enable) then
            call auto_revert_buffer(current_buffer())
            if (.not. tail_mode_binding) call minor_modes%auto_revert_tail_mode%set_state(.false.)
        end if
    end subroutine auto_revert_mode_body

    !> Turn on Auto-Revert Mode.
    !>
    !> This function is designed to be added to hooks, for example:
    !>   call c_mode_hook%add(turn_on_auto_revert_mode)
    !> Port of autorevert.el:turn-on-auto-revert-mode.
    recursive subroutine turn_on_auto_revert_mode(status)
        type(status_type), intent(out) :: status

        call minor_modes%auto_revert_mode%enable(status)
    end subroutine turn_on_auto_revert_mode

    !> Toggle reverting tail of buffer when the file grows.
    !>
    !> When Auto-Revert Tail Mode is enabled, the tail of the file is
    !> constantly followed, as with the shell command `tail -f'.  This
    !> means that whenever the file grows on disk (presumably because
    !> some background process is appending to it from time to time),
    !> this is reflected in the current buffer.
    !>
    !> You can edit the buffer and turn this mode off and on again as
    !> you please.  But make sure the background process has stopped
    !> writing before you save the file!
    !>
    !> When a buffer is reverted, a message is generated.  This can be
    !> suppressed by setting `auto_revert_verbose' to false.
    !>
    !> Use `auto_revert_mode' for changes other than appends!  Port of
    !> autorevert.el:auto-revert-tail-mode.
    recursive subroutine auto_revert_tail_mode(status)
        type(status_type), intent(out) :: status

        call minor_modes%auto_revert_tail_mode%command(status)
    end subroutine auto_revert_tail_mode

    !> The body of Auto-Revert Tail Mode.
    !> Port of autorevert.el:auto-revert-tail-mode (its body).
    recursive subroutine auto_revert_tail_mode_body(enable, status)
        logical, intent(in) :: enable
        type(status_type), intent(out) :: status
        type(status_type) :: st
        logical :: proceed, modified, full
        integer :: k

        status = status_type()
        if (.not. enable) return

        if (.not. allocated(buffers(cur)%filename)) then
            call minor_modes%auto_revert_tail_mode%disable(st)
            call set_error(status, FM_GENERIC_ERROR, "This buffer is not visiting a file")
            return
        end if

        modified = buffers(cur)%text%save_modiff < buffers(cur)%text%modiff
        proceed = .true.

        ! The library was loaded only after finding the file.
        if (modified .and. auto_revert_tail_pos() == 0) then
            call read_y_or_n("Buffer is modified, so tail offset may be wrong.  Proceed? ", proceed, status)
            if (.not. status%ok()) return
        end if

        if (.not. proceed) then
            call minor_modes%auto_revert_tail_mode%disable(status)
            return
        end if

        if (auto_revert_tail_pos() == 0 .and. .not. verify_visited_file_modtime()) then
            call read_y_or_n("File changed on disk, content may be missing.  Perform a full revert? ", full, status)
            if (.not. status%ok()) return
            if (full) call auto_revert_buffer(current_buffer())
        end if

        call before_save_tail_off_hook_add()
        ! Don't lose prior position.
        k = local_index(.true.)

        if (.not. locals(k)%tail_pos_local) then
            locals(k)%tail_pos_local = .true.
            locals(k)%tail_pos = file_size(buffers(cur)%filename)
        end if

        if (.not. minor_modes%auto_revert_mode%enabled()) then
            tail_mode_binding = .true.
            call minor_modes%auto_revert_mode%enable(status)
            tail_mode_binding = .false.
            if (.not. status%ok()) return
        end if

        call minor_modes%auto_revert_mode%set_state(.false.)
    end subroutine auto_revert_tail_mode_body

    !> (add-hook 'before-save-hook (lambda () (auto-revert-tail-mode 0)) nil t)
    !> Not a port.
    subroutine before_save_tail_off_hook_add()
        use files_fm, only: before_save_hook

        call before_save_hook%add(tail_mode_off, local=.true.)
    end subroutine before_save_tail_off_hook_add

    !> (lambda () (auto-revert-tail-mode 0))  Not a port.
    recursive subroutine tail_mode_off(status)
        type(status_type), intent(out) :: status

        call minor_modes%auto_revert_tail_mode%disable(status)
    end subroutine tail_mode_off

    !> Turn on Auto-Revert Tail Mode.
    !>
    !> This function is designed to be added to hooks, for example:
    !>   call my_logfile_mode_hook%add(turn_on_auto_revert_tail_mode)
    !> Port of autorevert.el:turn-on-auto-revert-tail-mode.
    recursive subroutine turn_on_auto_revert_tail_mode(status)
        type(status_type), intent(out) :: status

        call minor_modes%auto_revert_tail_mode%enable(status)
    end subroutine turn_on_auto_revert_tail_mode

    !> Toggle Global Auto-Revert Mode.
    !>
    !> Global Auto-Revert Mode is a global minor mode that reverts any
    !> buffer associated with a file when the file changes on disk.  Use
    !> `auto_revert_mode' to revert a particular buffer.
    !>
    !> If `global_auto_revert_non_file_buffers' is true, this mode
    !> may also revert some non-file buffers, as described in the
    !> documentation of that variable.  It ignores buffers with modes
    !> matching `global_auto_revert_ignore_modes', and buffers with a
    !> true value of `global_auto_revert_ignore_buffer'.
    !>
    !> When a buffer is reverted, a message is generated.  This can be
    !> suppressed by setting `auto_revert_verbose' to false.
    !>
    !> This function calls the hook `global_auto_revert_mode_hook'.
    !> It displays the text that `global_auto_revert_mode_text'
    !> specifies in the mode line.  Port of
    !> autorevert.el:global-auto-revert-mode.
    recursive subroutine global_auto_revert_mode(status)
        type(status_type), intent(out) :: status

        call minor_modes%global_auto_revert_mode%command(status)
    end subroutine global_auto_revert_mode

    !> The body of Global Auto-Revert Mode.
    !> Port of autorevert.el:global-auto-revert-mode (its body).
    recursive subroutine global_auto_revert_mode_body(enable, status)
        logical, intent(in) :: enable
        type(status_type), intent(out) :: status
        type(handle_type), allocatable :: list(:)
        integer(int64) :: saved
        integer :: i, k

        call auto_revert_set_timer(status)
        if (.not. status%ok()) return
        list = buffer_list()

        if (enable) then
            do i = 1, size(list)
                if (.not. is_buffer_live(list(i))) cycle
                call enter_buffer(list(i), saved)
                call auto_revert_global_add_current_buffer()
                call leave_buffer(saved)
            end do

            call find_file_hook%add(auto_revert_global_adopt_current_buffer)
            call after_change_major_mode_hook%add(auto_revert_global_possibly_adopt_current_buffer)
            call auto_revert_buffers(status)
        else
            call after_change_major_mode_hook%remove(auto_revert_global_possibly_adopt_current_buffer)
            call find_file_hook%remove(auto_revert_global_adopt_current_buffer)

            do i = 1, size(list)
                if (.not. is_buffer_live(list(i))) cycle
                call enter_buffer(list(i), saved)
                k = local_index(.false.)

                if (k /= 0) then
                    if (locals(k)%global_mode) then
                        locals(k)%global_mode = .false.

                        if (locals(k)%has_desc .and. .not. (minor_modes%auto_revert_mode%enabled() .or. &
                                                            minor_modes%auto_revert_tail_mode%enabled())) then
                            call auto_revert_notify_rm_watch()
                        end if
                    end if
                end if

                call leave_buffer(saved)
            end do
        end if
    end subroutine global_auto_revert_mode_body

    !> Set current buffer to be tracked by Global Auto-Revert if appropriate.
    !> Port of autorevert.el:auto-revert--global-add-current-buffer.
    subroutine auto_revert_global_add_current_buffer()
        procedure(buffer_stale_proc), pointer :: fn
        integer :: k
        logical :: eligible

        k = local_index(.false.)

        if (k /= 0) then
            if (locals(k)%global_mode) return
        end if

        eligible = allocated(buffers(cur)%filename)

        if (.not. eligible .and. global_auto_revert_non_file_buffers) then
            fn => buffer_stale_function%get()
            eligible = buffers(cur)%name(1:min(1, len(buffers(cur)%name))) /= " " .and. associated(fn)
            if (eligible) eligible = .not. associated(fn, buffer_stale_default_function)
        end if

        if (.not. eligible) return

        if (allocated(global_auto_revert_ignore_modes) .and. allocated(buffers(cur)%major_mode)) then
            if (any(global_auto_revert_ignore_modes == buffers(cur)%major_mode)) return
        end if

        if (global_auto_revert_ignore_buffer%value) return
        k = local_index(.true.)
        locals(k)%global_mode = .true.
    end subroutine auto_revert_global_add_current_buffer

    !> Consider tracking current buffer in a running Global Auto-Revert mode.
    !> This tracks buffers if `global_auto_revert_non_file_buffers' is true.
    !> Port of autorevert.el:auto-revert--global-possibly-adopt-current-buffer.
    recursive subroutine auto_revert_global_possibly_adopt_current_buffer(status)
        type(status_type), intent(out) :: status

        status = status_type()
        if (global_auto_revert_non_file_buffers) call auto_revert_global_adopt_current_buffer(status)
    end subroutine auto_revert_global_possibly_adopt_current_buffer

    !> Consider tracking current buffer in a running Global Auto-Revert mode.
    !> Port of autorevert.el:auto-revert--global-adopt-current-buffer.
    recursive subroutine auto_revert_global_adopt_current_buffer(status)
        type(status_type), intent(out) :: status

        call auto_revert_global_add_current_buffer()
        call auto_revert_set_timer(status)
    end subroutine auto_revert_global_adopt_current_buffer

    !> Update Auto-Revert management of the current buffer.
    !> Called after `set_visited_file_name'.  Port of
    !> autorevert.el:auto-revert-set-visited-file-name.
    recursive subroutine auto_revert_set_visited_file_name(status)
        type(status_type), intent(out) :: status

        status = status_type()
        if (auto_revert_notify_watch_descriptor()) call auto_revert_notify_rm_watch()

        if (minor_modes%global_auto_revert_mode%enabled()) then
            call auto_revert_global_adopt_current_buffer(status)
        else if (minor_modes%auto_revert_mode%enabled() .or. minor_modes%auto_revert_tail_mode%enabled()) then
            call auto_revert_set_timer(status)
        end if
    end subroutine auto_revert_set_visited_file_name

    ! ------------------------------------------------------------------
    ! Polling
    ! ------------------------------------------------------------------

    !> Whether buffer slot B is in Auto-Revert Mode, Auto-Revert Tail Mode
    !> or tracked by Global Auto-Revert Mode, and whether it has a watch
    !> descriptor.  Not a port.
    subroutine buffer_state(b, tracked, has_desc)
        integer(int64), intent(in) :: b
        logical, intent(out) :: tracked, has_desc
        integer(int64) :: saved
        integer :: k

        saved = cur
        call set_buffer_internal(b)
        k = local_index(.false.)
        tracked = minor_modes%auto_revert_mode%enabled() .or. minor_modes%auto_revert_tail_mode%enabled()
        has_desc = .false.

        if (k /= 0) then
            tracked = tracked .or. locals(k)%global_mode
            has_desc = locals(k)%has_desc
        end if

        if (saved > 0) call set_buffer_internal(saved)
    end subroutine buffer_state

    !> List of buffers that need to be polled.  Port of
    !> autorevert.el:auto-revert--polled-buffers.
    subroutine auto_revert_polled_buffers(list)
        type(handle_type), allocatable, intent(out) :: list(:)
        type(handle_type), allocatable :: all(:)
        logical :: tracked, has_desc
        integer :: i, n

        if (minor_modes%global_auto_revert_mode%enabled()) then
            all = buffer_list()
        else
            if (.not. allocated(auto_revert_buffer_list)) allocate (auto_revert_buffer_list(0))
            all = auto_revert_buffer_list

            if (.not. auto_revert_avoid_polling) then
                list = all
                return
            end if
        end if

        allocate (list(size(all)))
        n = 0

        do i = 1, size(all)
            if (.not. is_buffer_live(all(i))) then
                ! (buffer-local-value VAR BUFFER) of a dead buffer errs;
                ! auto-revert-buffer-list may hold one.
                if (.not. minor_modes%global_auto_revert_mode%enabled()) then
                    n = n + 1
                    list(n) = all(i)
                end if

                cycle
            end if

            call buffer_state(all(i)%index, tracked, has_desc)

            if (minor_modes%global_auto_revert_mode%enabled()) then
                if (auto_revert_avoid_polling .and. has_desc) cycle
                if (.not. tracked) cycle
            else
                if (has_desc) cycle
            end if

            n = n + 1
            list(n) = all(i)
        end do

        list = list(1:n)
    end subroutine auto_revert_polled_buffers

    !> Whether periodic polling is required.  Port of
    !> autorevert.el:auto-revert--need-polling-p.
    logical function is_auto_revert_need_polling() result(need)
        type(handle_type), allocatable :: all(:)
        logical :: tracked, has_desc
        integer :: i

        if (minor_modes%global_auto_revert_mode%enabled()) then
            need = .true.
            if (.not. auto_revert_avoid_polling) return
            need = .false.
            all = buffer_list()

            do i = 1, size(all)
                if (.not. is_buffer_live(all(i))) cycle
                call buffer_state(all(i)%index, tracked, has_desc)

                if (.not. has_desc .and. tracked) then
                    need = .true.
                    return
                end if
            end do
        else
            need = .false.
            if (.not. allocated(auto_revert_buffer_list)) return
            if (size(auto_revert_buffer_list) == 0) return
            need = .true.
            if (.not. auto_revert_avoid_polling) return
            need = .false.

            do i = 1, size(auto_revert_buffer_list)
                if (.not. is_buffer_live(auto_revert_buffer_list(i))) then
                    need = .true.
                    return
                end if

                call buffer_state(auto_revert_buffer_list(i)%index, tracked, has_desc)

                if (.not. has_desc) then
                    need = .true.
                    return
                end if
            end do
        end if
    end function is_auto_revert_need_polling

    !> Restart or cancel the timer used by Auto-Revert Mode.
    !> If such a timer is active, cancel it.  Start a new timer if
    !> Global Auto-Revert Mode is active or if Auto-Revert Mode is active
    !> in some buffer.  Restarting the timer ensures that Auto-Revert Mode
    !> will use an up-to-date value of `auto_revert_interval'.  Port of
    !> autorevert.el:auto-revert-set-timer.
    recursive subroutine auto_revert_set_timer(status)
        type(status_type), intent(out) :: status

        status = status_type()
        if (is_timer(auto_revert_timer)) call free_timer(auto_revert_timer)
        auto_revert_timer = null_handle()

        if (is_auto_revert_need_polling()) then
            call run_with_timer(auto_revert_timer, real(auto_revert_interval, real64), auto_revert_buffers, status, &
                                real(auto_revert_interval, real64))
        end if
    end subroutine auto_revert_set_timer

    ! ------------------------------------------------------------------
    ! Notification
    ! ------------------------------------------------------------------

    !> Disable file notification for current buffer's associated file.
    !> Port of autorevert.el:auto-revert-notify-rm-watch.
    recursive subroutine auto_revert_notify_rm_watch()
        type(watch_buffer_type), allocatable :: kept(:)
        type(watch_descriptor_type) :: desc
        integer :: k, i, n

        k = local_index(.false.)
        if (k == 0) return

        if (locals(k)%has_desc) then
            desc = locals(k)%desc

            if (allocated(buffer_by_watch_descriptor)) then
                allocate (kept(size(buffer_by_watch_descriptor)))
                n = 0

                do i = 1, size(buffer_by_watch_descriptor)
                    if (buffer_by_watch_descriptor(i)%desc == desc) cycle
                    n = n + 1
                    kept(n) = buffer_by_watch_descriptor(i)
                end do

                buffer_by_watch_descriptor = kept(1:n)
            end if

            call file_notify_rm_watch(desc)
            call kill_buffer_hook%remove(notify_rm_watch_hook, local=.true.)
        end if

        k = local_index(.false.)
        if (k == 0) return
        locals(k)%has_desc = .false.
        locals(k)%modified_p = .false.
    end subroutine auto_revert_notify_rm_watch

    !> `auto_revert_notify_rm_watch' as a kill-buffer-hook function.
    !> Not a port.
    recursive subroutine notify_rm_watch_hook(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call auto_revert_notify_rm_watch()
    end subroutine notify_rm_watch_hook

    !> Enable file notification for current buffer's associated file.
    !> Port of autorevert.el:auto-revert-notify-add-watch.
    recursive subroutine auto_revert_notify_add_watch()
        type(watch_descriptor_type) :: desc
        type(watch_buffer_type), allocatable :: grown(:)
        type(status_type) :: st
        character(len=:), allocatable :: dir, file
        integer :: k, n, flags

        if (auto_revert_notify_watch_descriptor()) return
        call expand_file_name(buffers(cur)%directory, dir, st)
        if (.not. st%ok()) return
        if (string_match(auto_revert_notify_exclude_dir_regexp, dir, st, inhibit_modify=.true.) > 0) return

        if (allocated(buffers(cur)%filename)) then
            if (is_file_symlink(buffers(cur)%filename)) return
            call expand_file_name(buffers(cur)%filename, file, st, buffers(cur)%directory)
            flags = ior(FILE_NOTIFY_CHANGE, FILE_NOTIFY_ATTRIBUTE_CHANGE)
        else
            if (is_file_symlink(buffers(cur)%directory)) return
            file = dir
            flags = FILE_NOTIFY_CHANGE
        end if

        if (.not. st%ok()) return
        ! ignore-errors
        call file_notify_add_watch(file, flags, auto_revert_notify_handler, desc, st)

        if (.not. st%ok()) then
            call auto_revert_use_notify%set_local(.false.)
            return
        end if

        k = local_index(.true.)
        locals(k)%has_desc = .true.
        locals(k)%desc = desc
        locals(k)%modified_p = .true.
        if (.not. allocated(buffer_by_watch_descriptor)) allocate (buffer_by_watch_descriptor(0))
        n = size(buffer_by_watch_descriptor)
        allocate (grown(n + 1))
        grown(1)%desc = desc
        grown(1)%buffer = current_buffer()
        grown(2:) = buffer_by_watch_descriptor
        call move_alloc(grown, buffer_by_watch_descriptor)
        call kill_buffer_hook%add(notify_rm_watch_hook, local=.true.)
    end subroutine auto_revert_notify_add_watch

    !> Handle an EVENT returned from file notification.  Port of
    !> autorevert.el:auto-revert-notify-handler.
    recursive subroutine auto_revert_notify_handler(event, status)
        type(file_notify_event_type), intent(in) :: event
        type(status_type), intent(out) :: status
        type(handle_type) :: buffer
        type(status_type) :: st
        integer(int64) :: saved
        integer :: i, k
        logical :: relevant

        status = status_type()
        buffer = null_handle()

        if (allocated(buffer_by_watch_descriptor)) then
            do i = 1, size(buffer_by_watch_descriptor)
                if (buffer_by_watch_descriptor(i)%desc == event%descriptor) then
                    buffer = buffer_by_watch_descriptor(i)%buffer
                    exit
                end if
            end do
        end if

        if (.not. is_buffer_live(buffer)) return
        call enter_buffer(buffer, saved)

        if (event%action == FILE_NOTIFY_STOPPED) then
            relevant = .not. allocated(buffers(cur)%filename)
            if (.not. relevant) relevant = file_name_nondirectory(event%file) == file_name_nondirectory(buffers(cur)%filename)

            if (relevant) then
                call auto_revert_notify_rm_watch()
                if (.not. is_timer(auto_revert_timer)) call auto_revert_set_timer(st)
            end if
        else
            if (allocated(buffers(cur)%filename)) then
                relevant = .false.

                if (event%action == FILE_NOTIFY_ATTRIBUTE_CHANGED .or. event%action == FILE_NOTIFY_CHANGED .or. &
                    event%action == FILE_NOTIFY_CREATED) then
                    relevant = file_name_nondirectory(event%file) == file_name_nondirectory(buffers(cur)%filename)
                else if (event%action == FILE_NOTIFY_RENAMED .and. allocated(event%file1)) then
                    relevant = file_name_nondirectory(event%file1) == file_name_nondirectory(buffers(cur)%filename)
                end if
            else
                relevant = event%action == FILE_NOTIFY_CREATED .or. event%action == FILE_NOTIFY_RENAMED .or. &
                           event%action == FILE_NOTIFY_DELETED
            end if

            if (relevant) then
                k = local_index(.true.)
                locals(k)%modified_p = .true.

                if (.not. is_timer(locals(k)%lockout_timer)) then
                    call run_with_timer(locals(k)%lockout_timer, real(auto_revert_lockout_interval, real64), &
                                        auto_revert_end_lockout, st)

                    if (now_seconds() - locals(k)%last_time > auto_revert_lockout_interval) then
                        call auto_revert_buffer(buffer)
                    end if
                end if
            end if
        end if

        call leave_buffer(saved)
    end subroutine auto_revert_notify_handler

    !> End the lockout period after a notification.
    !> If the buffer needs to be reverted, do it now.  Port of
    !> autorevert.el:auto-revert--end-lockout (a timer function has no
    !> arguments: it ends the lockouts whose timers have run).
    recursive subroutine auto_revert_end_lockout(status)
        type(status_type), intent(out) :: status
        integer(int64) :: saved, b
        integer :: k

        status = status_type()
        if (.not. allocated(locals)) return

        do k = 1, size(locals)
            if (.not. locals(k)%set) cycle
            if (.not. is_timer(locals(k)%lockout_timer)) cycle
            if (is_timer_active(locals(k)%lockout_timer)) cycle
            call free_timer(locals(k)%lockout_timer)
            locals(k)%lockout_timer = null_handle()
            b = buffer_of_serial(locals(k)%serial)
            if (b == 0) cycle
            call enter_buffer(buffer_handle(b), saved)
            if (locals(k)%modified_p) call auto_revert_buffer(buffer_handle(b))
            call leave_buffer(saved)
        end do
    end subroutine auto_revert_end_lockout

    !> The live buffer with SERIAL (0 for none).  Not a port.
    integer(int64) function buffer_of_serial(serial) result(b)
        integer(int64), intent(in) :: serial
        integer(int64) :: i

        b = 0

        do i = 1, size(buffers, kind=int64)
            if (buffers(i)%serial == serial) then
                if (is_buffer_live(buffer_handle(i))) b = i
                return
            end if
        end do
    end function buffer_of_serial

    ! ------------------------------------------------------------------
    ! Reverting
    ! ------------------------------------------------------------------

    !> Deactivate auto-reverting of current buffer temporarily.
    !> Run BODY.  Port of autorevert.el:inhibit-auto-revert.
    recursive subroutine inhibit_auto_revert(body, status)
        procedure(inhibit_body_proc) :: body
        type(status_type), intent(out) :: status
        type(handle_type), allocatable :: list(:)
        type(handle_type) :: buf
        logical :: added
        integer :: k

        if (allocated(inhibit_auto_revert_buffers)) then
            list = inhibit_auto_revert_buffers

            do k = 1, size(list)
                if (.not. is_buffer_live(list(k))) call delq(list(k), inhibit_auto_revert_buffers)
            end do
        end if

        buf = current_buffer()
        added = .not. memq(buf, inhibit_auto_revert_buffers)
        if (added) call push(buf, inhibit_auto_revert_buffers)
        call body(status)
        if (added) call delq(buf, inhibit_auto_revert_buffers)
    end subroutine inhibit_auto_revert

    !> Check if auto-revert is active in current buffer.  Port of
    !> autorevert.el:auto-revert-active-p.
    logical function is_auto_revert_active()
        integer :: k

        is_auto_revert_active = minor_modes%auto_revert_mode%enabled() .or. minor_modes%auto_revert_tail_mode%enabled()

        if (.not. is_auto_revert_active) then
            k = local_index(.false.)
            if (k /= 0) is_auto_revert_active = locals(k)%global_mode
        end if

        if (is_auto_revert_active) is_auto_revert_active = .not. memq(current_buffer(), inhibit_auto_revert_buffers)
    end function is_auto_revert_active

    !> Revert current buffer, if appropriate.
    !> This is an internal function used by Auto-Revert Mode.  Port of
    !> autorevert.el:auto-revert-handler.
    recursive subroutine auto_revert_handler()
        procedure(buffer_stale_proc), pointer :: fn
        type(handle_type) :: buffer
        type(handle_type), allocatable :: wins(:), eoblist(:)
        type(status_type) :: st
        integer(int64) :: fsize
        integer :: revert, k, i, n
        logical :: eob, saved_read_only, saved_in_progress, tail

        buffer = current_buffer()
        fsize = 0
        tail = minor_modes%auto_revert_tail_mode%enabled()
        k = local_index(.true.)
        revert = BUFFER_STALE_NO

        if (.not. locals(k)%has_desc .or. locals(k)%modified_p) then
            if (.not. memq(buffer, inhibit_auto_revert_buffers)) then
                if (allocated(buffers(cur)%filename) .and. tail) then
                    if (is_file_readable(buffers(cur)%filename)) then
                        fsize = file_size(buffers(cur)%filename)
                        if (locals(k)%tail_pos /= fsize) revert = BUFFER_STALE_YES
                    end if
                else if (minor_modes%auto_revert_mode%enabled() .or. locals(k)%global_mode) then
                    fn => buffer_stale_function%get()
                    if (.not. associated(fn)) fn => buffer_stale_default_function
                    revert = fn(.true.)
                    k = local_index(.true.)
                end if
            end if
        end if

        if (is_timer(locals(k)%lockout_timer)) call free_timer(locals(k)%lockout_timer)
        locals(k)%lockout_timer = null_handle()

        if (revert /= BUFFER_STALE_NO) then
            if (auto_revert_verbose .and. revert /= BUFFER_STALE_FAST) then
                call message("Reverting buffer `"//buffers(cur)%name//"'")
            end if

            locals(k)%modified_p = .false.
            locals(k)%last_time = now_seconds()
            eob = .false.
            n = 0

            if (allocated(buffers(cur)%filename)) then
                eob = point() == point_max()
                allocate (wins(0))
                call window_leaves(selected_frame%root_window, wins)
                allocate (eoblist(size(wins)))

                do i = 1, size(wins)
                    if (.not. same_handle(windows(wins(i)%index)%buffer, buffer)) cycle
                    if (window_point_of(wins(i)) /= point_max()) cycle
                    n = n + 1
                    eoblist(n) = wins(i)
                end do
            end if

            if (tail) then
                call auto_revert_tail_handler(fsize)
            else
                saved_read_only = buffers(cur)%read_only
                ! ignore-errors
                call revert_buffer(st, ignore_auto=.true., noconfirm=.true., preserve_modes=.true.)
                buffers(cur)%read_only = saved_read_only
            end if

            if (allocated(buffers(cur)%filename)) then
                if (eob) call goto_char(point_max())

                do i = 1, n
                    call set_window_point_of(eoblist(i), point_max())
                end do
            end if
        end if

        if (.not. tail .and. (revert /= BUFFER_STALE_NO .or. auto_revert_check_vc_info)) then
            saved_in_progress = revert_buffer_in_progress
            revert_buffer_in_progress = .true.
            call vc_refresh_state(st)
            revert_buffer_in_progress = saved_in_progress
        end if
    end subroutine auto_revert_handler

    !> The point of window W (window-point: the buffer's point in the
    !> selected window).  Not a port.
    integer(int64) function window_point_of(w) result(pos)
        type(handle_type), intent(in) :: w

        if (same_handle(w, selected_frame%selected_window)) then
            pos = point()
        else
            pos = window_point(windows(w%index))
        end if
    end function window_point_of

    !> Set the point of window W to POS (set-window-point).  Not a port.
    subroutine set_window_point_of(w, pos)
        type(handle_type), intent(in) :: w
        integer(int64), intent(in) :: pos
        type(status_type) :: st

        if (same_handle(w, selected_frame%selected_window)) then
            call goto_char(pos)
        else
            call set_window_point(windows(w%index), pos, st)
        end if
    end subroutine set_window_point_of

    !> Append the new tail of the visited file, of SIZE bytes now.  Port of
    !> autorevert.el:auto-revert-tail-handler.
    recursive subroutine auto_revert_tail_handler(fsize)
        integer(int64), intent(in) :: fsize
        character(len=:), allocatable :: file
        type(value_type) :: saved_inhibit
        type(status_type) :: st
        integer(int64) :: inserted, opoint, obegv, ozv
        logical :: modified, narrowed
        integer :: k

        k = local_index(.true.)
        modified = buffers(cur)%text%save_modiff < buffers(cur)%text%modiff
        call symbol_value(sym("inhibit_read_only"), saved_inhibit, st)
        call set(sym("inhibit_read_only"), t_value(), st)
        ! Ignore that file has changed.
        call move_alloc(buffers(cur)%filename, file)

        if (locals(k)%tail_pos /= fsize) then
            call before_revert_hook%run(st)
            call undo_boundary()
            ! save-restriction, save-excursion
            opoint = point()
            obegv = point_min()
            ozv = point_max()
            narrowed = obegv /= 1 .or. ozv /= buffers(cur)%text%z
            call widen()
            call goto_char(point_max())

            if (locals(k)%tail_pos < fsize) then
                call insert_file_contents(file, .false., inserted, st, beg_byte=locals(k)%tail_pos, end_byte=fsize)
            else
                call insert_file_contents(file, .false., inserted, st, end_byte=fsize)
            end if

            if (narrowed) call narrow_to_region(obegv, ozv, st)
            call goto_char(opoint)
            call after_revert_hook%run(st)
            call undo_boundary()
            k = local_index(.true.)
            locals(k)%tail_pos = fsize
            locals(k)%tail_pos_local = .true.
            call restore_buffer_modified(modified)
        end if

        call move_alloc(file, buffers(cur)%filename)
        call set(sym("inhibit_read_only"), saved_inhibit, st)
        call set_visited_file_modtime()
    end subroutine auto_revert_tail_handler

    !> Return a prioritized list of buffers to maybe auto-revert.
    !> The differences between this return value and the reference
    !> variable `auto_revert_buffer_list' include: 1) this has more
    !> entries when in `global_auto_revert_mode'; 2) this prioritizes
    !> buffers not reverted last time due to user interruption.  Port of
    !> autorevert.el:auto-revert--buffer-candidates.
    subroutine auto_revert_buffer_candidates(list)
        type(handle_type), allocatable, intent(out) :: list(:)
        type(handle_type), allocatable :: bufs(:), remaining(:), new(:)
        integer :: i, n

        call auto_revert_polled_buffers(bufs)
        allocate (list(size(bufs)))
        n = 0

        do i = 1, size(bufs)
            if (.not. is_buffer_live(bufs(i))) cycle
            n = n + 1
            list(n) = bufs(i)
        end do

        bufs = list(1:n)
        allocate (remaining(0), new(0))

        if (allocated(auto_revert_remaining_buffers)) then
            do i = 1, size(auto_revert_remaining_buffers)
                if (memq(auto_revert_remaining_buffers(i), bufs)) call push(auto_revert_remaining_buffers(i), remaining)
            end do
        end if

        do i = 1, size(bufs)
            if (.not. memq(bufs(i), remaining)) call push(bufs(i), new)
        end do

        ! (nreverse (nconc new remaining))
        deallocate (list)

        if (size(remaining) + size(new) == 0) then
            ! (An empty array constructor would reallocate to size 0.)
            allocate (list(0))
        else
            list = [remaining(size(remaining):1:-1), new(size(new):1:-1)]
        end if
    end subroutine auto_revert_buffer_candidates

    !> Revert a single buffer BUF.
    !>
    !> This is performed as specified by Auto-Revert and Global
    !> Auto-Revert Modes.  Port of autorevert.el:auto-revert-buffer.
    recursive subroutine auto_revert_buffer(buf)
        type(handle_type), intent(in) :: buf
        logical :: saved_in_progress, use_notify
        integer(int64) :: saved

        saved_in_progress = auto_revert_buffer_in_progress
        auto_revert_buffer_in_progress = .true.

        if (.not. is_buffer_live(buf)) then
            call auto_revert_remove_current_buffer(buf)
        else
            call enter_buffer(buf, saved)

            if (.not. minor_modes%auto_revert_mode%enabled() .and. .not. minor_modes%auto_revert_tail_mode%enabled()) then
                call auto_revert_remove_current_buffer()
            end if

            if (is_auto_revert_active()) then
                use_notify = auto_revert_use_notify%value .and. .not. auto_revert_notify_watch_descriptor()
                if (use_notify) use_notify = allocated(buffers(cur)%filename) .or. &
                                             buffer_auto_revert_by_notification%value
                if (use_notify) call auto_revert_notify_add_watch()
                call auto_revert_handler()
            end if

            call leave_buffer(saved)
        end if

        auto_revert_buffer_in_progress = saved_in_progress
    end subroutine auto_revert_buffer

    !> Revert buffers as specified by Auto-Revert and Global Auto-Revert Mode.
    !>
    !> Should `global_auto_revert_mode' be active all file buffers are checked.
    !>
    !> Should `auto_revert_mode' be active in some buffers, those buffers
    !> are checked.
    !>
    !> Non-file buffers that have a custom `revert_buffer_function' and
    !> `buffer_stale_function' are reverted either when Auto-Revert
    !> Mode is active in that buffer, or when the variable
    !> `global_auto_revert_non_file_buffers' is true and Global
    !> Auto-Revert Mode is active.
    !>
    !> This function stops whenever there is user input.  The buffers not
    !> checked are stored in the variable `auto_revert_remaining_buffers'.
    !>
    !> To avoid starvation, the buffers in `auto_revert_remaining_buffers'
    !> are checked first the next time this function is called.
    !>
    !> This function is also responsible for removing buffers no longer in
    !> Auto-Revert Mode from `auto_revert_buffer_list', and for canceling
    !> the timer when no buffers need to be checked.  Port of
    !> autorevert.el:auto-revert-buffers.
    recursive subroutine auto_revert_buffers(status)
        type(status_type), intent(out) :: status
        type(handle_type), allocatable :: bufs(:)
        integer :: i

        status = status_type()
        call auto_revert_buffer_candidates(bufs)
        i = 1

        do while (i <= size(bufs))
            if (auto_revert_stop_on_user_input) then
                if (is_input_pending()) exit
            end if

            call auto_revert_buffer(bufs(i))
            i = i + 1
        end do

        auto_revert_remaining_buffers = bufs(i:)

        if (.not. is_auto_revert_need_polling()) then
            if (is_timer(auto_revert_timer)) call free_timer(auto_revert_timer)
            auto_revert_timer = null_handle()
        end if
    end subroutine auto_revert_buffers

    !> The interactive form of `auto_revert_set_timer'.
    recursive subroutine cmd_auto_revert_set_timer(status)
        type(status_type), intent(out) :: status

        call auto_revert_set_timer(status)
    end subroutine cmd_auto_revert_set_timer

    !> Define the Auto-Revert modes, hooks and commands.  Not a port.
    subroutine syms_of_autorevert()
        type(status_type) :: st

        if (.not. allocated(auto_revert_mode_text)) auto_revert_mode_text = " ARev"
        if (.not. allocated(auto_revert_tail_mode_text)) auto_revert_tail_mode_text = " Tail"
        if (.not. allocated(global_auto_revert_mode_text)) global_auto_revert_mode_text = ""
        if (.not. allocated(global_auto_revert_ignore_modes)) allocate (character(len=1) :: global_auto_revert_ignore_modes(0))
        if (.not. allocated(mounted_file_systems)) mounted_file_systems = &
            "^\(?:/\(?:afs/\|m\(?:edia/\|nt\)\|\(?:ne\|tmp_mn\)t/\)\)"

        if (.not. allocated(auto_revert_notify_exclude_dir_regexp)) then
            auto_revert_notify_exclude_dir_regexp = mounted_file_systems
            if (.not. auto_revert_remote_files) auto_revert_notify_exclude_dir_regexp = &
                auto_revert_notify_exclude_dir_regexp//"\|^/[^/:|][^/|]+:"
        end if

        call forward_setting(auto_revert_use_notify)
        call forward_setting(global_auto_revert_ignore_buffer, local_if_set=.true.)
        call forward_setting(buffer_auto_revert_by_notification, local_if_set=.true.)
        call minor_modes%auto_revert_mode%define("auto_revert_mode", auto_revert_mode_text, hook=auto_revert_mode_hook, &
                                                 body=auto_revert_mode_body)
        call minor_modes%auto_revert_tail_mode%define("auto_revert_tail_mode", auto_revert_tail_mode_text, &
                                                      hook=auto_revert_tail_mode_hook, body=auto_revert_tail_mode_body)
        call minor_modes%global_auto_revert_mode%define("global_auto_revert_mode", global_auto_revert_mode_text, &
                                                        global=.true., hook=global_auto_revert_mode_hook, &
                                                        body=global_auto_revert_mode_body)
        ! (put 'auto-revert-mode 'permanent-local t), and the same for
        ! auto-revert-tail-mode.
        call put(sym("auto_revert_mode"), sym("permanent_local"), t_value(), st)
        call put(sym("auto_revert_tail_mode"), sym("permanent_local"), t_value(), st)
        call add_local_variables_killer(kill_autorevert_locals)
        call find_file_hook%add(auto_revert_find_file_function)
        call after_set_visited_file_name_hook%add(auto_revert_set_visited_file_name)
        commands%auto_revert_mode%run => auto_revert_mode
        commands%auto_revert_tail_mode%run => auto_revert_tail_mode
        commands%global_auto_revert_mode%run => global_auto_revert_mode
        commands%auto_revert_set_timer%run => cmd_auto_revert_set_timer
    end subroutine syms_of_autorevert
end module autorevert_fm
