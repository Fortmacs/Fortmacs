! vc_log_fm.f90 -- the log commands of VC.
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

!> `vc_print_log' (C-x v l), `vc_print_root_log' (C-x v L) and
!> `vc_print_log_internal': a submodule because they use log_view_fm,
!> which uses vc_fm.
!>
!> Deferred: the buttons `vc_print_log_setup_buttons' inserts at the end
!> of a log ("Show 4X entries", "Show unlimited entries" and the renamed
!> file buttons, needing button.el and the backend's
!> `file-name-changes'), `log-view-vc-prev-fileset', the
!> `region-history-mode' of logs with diffs, `vc_print_change_log',
!> `vc_print_root_change_log' and the branch logs.
submodule(vc_fm) vc_log_fm
    use log_view_fm, only: log_view_mode, set_log_view_vc_backend, set_log_view_vc_fileset
    use callint_fm, only: PREFIX_NUMBER
    use vc_dispatcher_fm, only: vc_exec_after, vc_action_type, vc_parent_buffer, set_vc_parent_buffer, vc_sentinel_movepoint
    use lisp_fm, only: value_type
    use callint_fm, only: current_prefix_arg
    use fileio_fm, only: is_file_directory
    implicit none(type, external)

contains

    module procedure vc_print_log_internal
    logical :: dir_present, shortlog, is_start
    integer :: k, the_type
    type(vc_log_limit_type) :: lim

    is_start = .false.
    if (present(is_start_revision)) is_start = is_start_revision
    if (present(limit)) lim = limit
    ! Don't switch to the output buffer before running the command,
    ! so that any buffer-local settings in the vc-controlled
    ! buffer can be accessed by the command.
    dir_present = .false.

    do k = 1, size(files)
        if (is_file_directory(files(k)%s)) dir_present = .true.
    end do

    if (dir_present) then
        shortlog = vc_log_short_style%directory
    else
        shortlog = vc_log_short_style%file
    end if

    the_type = VC_LOG_VIEW_LONG
    if (shortlog) the_type = VC_LOG_VIEW_SHORT

    if (present(type)) then
        if (type /= VC_LOG_VIEW_NONE) the_type = type
    end if

    call vc_log_internal_common(backend, "*vc-change-log*", files, the_type, shortlog, working_revision, is_start, &
                                lim, status)
    end procedure vc_print_log_internal

    ! The common part of the log commands: make BUFFER_NAME, insert the log
    ! with the backend's `print_log', set up its Log View mode and show it,
    ! then when the log is complete go to WORKING_REVISION and run
    ! `vc_log_finish_functions'.  Port of vc.el:vc-log-internal-common
    ! with the BACKEND-FUNC, SETUP-BUTTONS-FUNC, GOTO-LOCATION-FUNC and
    ! REV-BUFF-FUNC of `vc-print-log-internal'.
    recursive subroutine vc_log_internal_common(backend, buffer_name, files, type, shortlog, working_revision, &
                                                is_start_revision, limit, status)
        use window_fm, only: pop_to_buffer
        use files_fm, only: revert_buffer_function
        integer, intent(in) :: backend
        character(len=*), intent(in) :: buffer_name
        type(vc_string_type), intent(in) :: files(:)
        integer, intent(in) :: type
        logical, intent(in) :: shortlog, is_start_revision
        character(len=*), intent(in) :: working_revision
        type(vc_log_limit_type), intent(in) :: limit
        type(status_type), intent(out) :: status
        type(handle_type) :: buffer, old
        type(value_type) :: saved
        type(vc_action_type) :: action
        type(status_type) :: st
        logical :: limit_unsupported
        integer :: k

        call get_buffer_create(buffer_name, buffer, status)
        if (.not. status%ok()) return
        old = current_buffer()
        call set_buffer(buffer, st)
        k = log_buffer_index(.true.)
        log_buffers(k)%view_type = type
        ! (lambda (bk buf _type-arg files-arg) ...): the backend function.
        log_buffers(k)%shortlog = shortlog
        call set_buffer(old, st)
        limit_unsupported = .false.

        if (.not. associated(vc_backends(backend)%print_log)) then
            call set_error(status, FM_VC_NOT_SUPPORTED, "No print-log function for "//vc_backends(backend)%name)
            return
        end if

        if (is_start_revision) then
            call vc_backends(backend)%print_log(files, buffer, shortlog, working_revision, limit, status, &
                                                limit_unsupported)
        else
            call vc_backends(backend)%print_log(files, buffer, shortlog, "", limit, status, limit_unsupported)
        end if

        if (.not. status%ok()) return
        old = current_buffer()
        call set_buffer(buffer, st)
        ! log-view-mode used to be called with inhibit-read-only bound
        ! to t, so let's keep doing it, just in case.
        call symbol_value(value_type(name="inhibit_read_only"), saved, st)
        call set(value_type(name="inhibit_read_only"), value_type(.true.), st)

        if (associated(vc_backends(backend)%log_view_mode)) then
            call vc_backends(backend)%log_view_mode(status)
        else
            call log_view_mode(status)
        end if

        call set(value_type(name="inhibit_read_only"), saved, st)

        if (status%ok()) then
            call set_log_view_vc_backend(backend)
            call set_log_view_vc_fileset(files)
            k = log_buffer_index(.true.)
            log_buffers(k)%has_args = .true.
            log_buffers(k)%backend = backend
            log_buffers(k)%files = files
            log_buffers(k)%working_revision = working_revision
            log_buffers(k)%is_start_revision = is_start_revision
            log_buffers(k)%limit = limit
            log_buffers(k)%type = type
            call revert_buffer_function%set_local(log_revert)
        end if

        call set_buffer(old, st)
        if (.not. status%ok()) return
        ! Display after setting up major-mode, so display-buffer-alist can know
        ! the major-mode.
        call pop_to_buffer(buffer, status)
        if (.not. status%ok()) return
        action%execute => log_finish
        action%buffer = buffer
        action%n = backend
        action%s1 = working_revision
        action%flag = limit_unsupported
        call vc_exec_after(action, status)
    end subroutine vc_log_internal_common

    ! (vc-run-delayed ...) of `vc-log-internal-common': set up the buttons
    ! (deferred), go to the working revision, and run
    ! `vc_log_finish_functions'.  Port of the delayed part of
    ! vc.el:vc-log-internal-common.
    recursive subroutine log_finish(action, status)
        type(vc_action_type), intent(in) :: action
        type(status_type), intent(out) :: status
        type(handle_type) :: old
        type(value_type) :: saved
        type(status_type) :: st

        status = status_type()
        if (.not. is_buffer_live(action%buffer)) return
        old = current_buffer()
        call set_buffer(action%buffer, st)
        call symbol_value(value_type(name="inhibit_read_only"), saved, st)
        call set(value_type(name="inhibit_read_only"), value_type(.true.), st)

        ! When it's nil, point really shouldn't move (bug#15322).
        if (len(action%s1) > 0) then
            if (associated(vc_backends(action%n)%show_log_entry)) then
                call vc_backends(action%n)%show_log_entry(action%s1)
            end if

            vc_sentinel_movepoint = point()
        end if

        call restore_buffer_modified(.false.)
        call vc_log_finish_functions%execute(status)
        call set(value_type(name="inhibit_read_only"), saved, st)
        if (is_buffer_live(old)) call set_buffer(old, st)
    end subroutine log_finish

    ! The `revert_buffer_function' of a log buffer: show the log again.
    ! Port of the revert-buffer-function lambda of
    ! vc.el:vc-log-internal-common.
    recursive subroutine log_revert(ignore_auto, noconfirm, status)
        logical, intent(in) :: ignore_auto, noconfirm
        type(status_type), intent(out) :: status
        type(vc_log_buffer_type) :: args
        integer :: k

        status = status_type()
        if (ignore_auto .and. noconfirm .and. .false.) return
        k = log_buffer_index(.false.)
        if (k == 0) return
        if (.not. log_buffers(k)%has_args) return
        args = log_buffers(k)
        call vc_print_log_internal(args%backend, args%files, args%working_revision, status, &
                                   args%is_start_revision, args%limit, args%type)
    end subroutine log_revert

    module procedure vc_print_log
    type(vc_fileset_type) :: fs
    type(vc_log_limit_type) :: lim
    character(len=:), allocatable :: rev

    call vc_deduce_fileset(fs, status, .true.)
    if (.not. status%ok()) return
    rev = ""
    if (present(working_revision)) rev = working_revision
    if (len(rev) == 0) rev = vc_buffer_revision()

    if (present(limit)) then
        if (limit > 0) then
            lim%kind = VC_LIMIT_NUMBER
            lim%n = limit
        end if
    end if

    call vc_print_log_internal(fs%backend, fs%files, rev, status, .false., lim)
    end procedure vc_print_log

    module procedure vc_print_root_log
    type(vc_fileset_type) :: fs
    type(vc_log_limit_type) :: lim
    type(vc_log_short_style_type) :: saved_style
    character(len=:), allocatable :: saved_dir, rev
    type(handle_type) :: old
    type(status_type) :: st
    logical :: with_diff
    integer :: the_type

    call with_backend_in_rootdir("VC revision log", fs, saved_dir, status)

    if (.not. status%ok()) then
        buffers(cur)%directory = saved_dir
        return
    end if

    rev = ""
    if (present(revision)) rev = revision

    if (present(limit)) then
        if (limit > 0) then
            lim%kind = VC_LIMIT_NUMBER
            lim%n = limit
        end if
    end if

    with_diff = lim%kind == VC_LIMIT_NUMBER .and. lim%n == 1 .and. len(rev) > 0
    saved_style = vc_log_short_style
    if (with_diff) vc_log_short_style = vc_log_short_style_type(directory=.false., file=.false.)
    the_type = VC_LOG_VIEW_NONE
    if (with_diff) the_type = VC_LOG_VIEW_WITH_DIFF
    old = current_buffer()
    call vc_print_log_internal(fs%backend, fs%files, rev, status, len(rev) > 0, lim, the_type)
    vc_log_short_style = saved_style

    if (is_buffer_live(old)) then
        call set_buffer(old, st)
        buffers(cur)%directory = saved_dir
        call set_buffer(get_buffer("*vc-change-log*"), st)
    end if

    if (.not. status%ok()) return
    ! We're looking at the root, so displaying " from <some-file>" in
    ! the mode line isn't helpful.
    call set_vc_parent_buffer(vc_parent_buffer(), "")
    end procedure vc_print_root_log

    ! Read a LIMIT argument for a VC log command.  Port of
    ! vc.el:vc--read-limit.
    recursive subroutine read_limit(limit, status)
        use minibuf_fm, only: read_from_minibuffer
        integer(int64), intent(out) :: limit
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: answer
        character(len=24) :: buf
        integer :: ios

        limit = 0
        write (buf, "(i0)") vc_log_show_limit
        call read_from_minibuffer("Limit display (0 for unlimited): ", answer, status, initial=trim(buf))
        if (.not. status%ok()) return
        ! (string-to-number ...): 0 when not a number.
        read (answer, *, iostat=ios) limit
        if (ios /= 0) limit = 0
    end subroutine read_limit

    module procedure cmd_vc_print_log
    use minibuf_fm, only: read_from_minibuffer
    character(len=:), allocatable :: rev
    integer(int64) :: lim

    if (current_prefix_arg%kind /= PREFIX_NONE) then
        call read_from_minibuffer("Leave point at revision (default last revision): ", rev, status)
        if (.not. status%ok()) return
        call read_limit(lim, status)
        if (.not. status%ok()) return
        call vc_print_log(status, rev, max(lim, 0_int64))
    else
        call vc_print_log(status, "", int(max(vc_log_show_limit, 0), int64))
    end if
    end procedure cmd_vc_print_log

    module procedure cmd_vc_print_root_log
    use lisp_fm, only: value_type
    use minibuf_fm, only: read_string
    character(len=:), allocatable :: default, revision, prompt
    integer(int64) :: lim

    if (current_prefix_arg%kind == PREFIX_NUMBER) then
        if (current_prefix_arg%value == 1) then
            default = word_at_point()
            prompt = "Revision to show: "
            if (len(default) > 0) prompt = "Revision to show (default "//default//"): "
            call read_string(prompt, revision, status, default=default)
            if (.not. status%ok()) return
            call vc_print_root_log(status, 1_int64, revision)
        else
            call vc_print_root_log(status, current_prefix_arg%value)
        end if
    else if (current_prefix_arg%kind /= PREFIX_NONE) then
        call read_limit(lim, status)
        if (.not. status%ok()) return
        call vc_print_root_log(status, max(lim, 0_int64))
    else
        call vc_print_root_log(status, int(max(vc_log_show_limit, 0), int64))
    end if
    end procedure cmd_vc_print_root_log

    ! (thing-at-point 'word t): the word around point, or "".  Not a port.
    function word_at_point() result(word)
        use syntax_fm, only: skip_syntax_forward, skip_syntax_backward
        use editfns_fm, only: buffer_substring
        character(len=:), allocatable :: word
        integer(int64) :: opoint, beg, moved
        type(status_type) :: st

        opoint = point()
        moved = skip_syntax_backward("w")
        beg = point()
        moved = skip_syntax_forward("w")
        word = ""
        if (point() > beg) call buffer_substring(beg, point(), word, st)
        call goto_char(opoint)
    end function word_at_point
end submodule vc_log_fm
