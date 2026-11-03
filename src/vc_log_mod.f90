! vc_log_mod.f90 -- the log commands of VC.
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
!> `vc_print_log_internal': a submodule because they use log_view_mod,
!> which uses vc_mod.
!> The branch logs (C-x v b l, C-x v b L) and the incoming and outgoing
!> logs (C-x v I, C-x v O) are here too.
!>
!> Deferred: see the vc_mod row of PORTING.md.
submodule(vc_mod) vc_log_mod
    use log_view_mod, only: log_view_mode, set_log_view_vc_backend, set_log_view_vc_fileset
    use callint_mod, only: PREFIX_NUMBER
    use vc_dispatcher_mod, only: vc_exec_after, vc_action_type, vc_parent_buffer, set_vc_parent_buffer, vc_sentinel_movepoint
    use value_mod, only: value_type
    use callint_mod, only: current_prefix_arg
    use fileio_mod, only: is_file_directory
    use string_type_mod, only: string_type
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
        if (is_file_directory(files(k)%text)) dir_present = .true.
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
    !> With UPSTREAM_LOCATION, the log is an incoming or outgoing one
    !> (TYPE VC_LOG_VIEW_LOG_INCOMING or VC_LOG_VIEW_LOG_OUTGOING), made
    !> by the backend's `log_incoming' or `log_outgoing' with the
    !> SETUP-BUTTONS-FUNC, GOTO-LOCATION-FUNC and REV-BUFF-FUNC of
    !> vc.el:vc-incoming-outgoing-internal.
    recursive subroutine vc_log_internal_common(backend, buffer_name, files, type, shortlog, working_revision, &
                                                is_start_revision, limit, status, upstream_location)
        use window_mod, only: pop_to_buffer
        use files_mod, only: revert_buffer_function
        integer, intent(in) :: backend
        character(len=*), intent(in) :: buffer_name
        type(string_type), intent(in) :: files(:)
        integer, intent(in) :: type
        logical, intent(in) :: shortlog, is_start_revision
        character(len=*), intent(in) :: working_revision
        type(vc_log_limit_type), intent(in) :: limit
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: upstream_location
        type(handle_type) :: buffer, old
        type(value_type) :: saved
        type(vc_action_type) :: action
        type(status_type) :: st
        logical :: limit_unsupported
        integer :: k

        call get_or_make_buffer(buffer_name, buffer, status)
        if (.not. status%ok()) return
        old = current_buffer()
        call set_buffer(buffer, st)
        call make_log_buffer_entry(k)
        log_buffers(k)%view_type = type
        ! (lambda (bk buffer _type-arg files-arg) ...): the backend function.
        if (.not. present(upstream_location)) log_buffers(k)%shortlog = shortlog
        call set_buffer(old, st)
        limit_unsupported = .false.

        if (present(upstream_location)) then
            ! (vc-call-backend bk type-arg buffer upstream-location)
            if (type == VC_LOG_VIEW_LOG_INCOMING) then
                call vc_default_log_incoming(backend, buffer, upstream_location, status)
            else
                call vc_default_log_outgoing(backend, buffer, upstream_location, status)
            end if
        else if (.not. associated(vc_backends(backend)%print_log)) then
            call set_error(status, FM_VC_NOT_SUPPORTED, "No print-log function for "//vc_backends(backend)%name)
            return
        else if (is_start_revision) then
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
            call make_log_buffer_entry(k)
            log_buffers(k)%has_args = .true.
            log_buffers(k)%backend = backend
            log_buffers(k)%files = files
            log_buffers(k)%working_revision = working_revision
            log_buffers(k)%is_start_revision = is_start_revision
            log_buffers(k)%limit = limit
            log_buffers(k)%type = type
            if (present(upstream_location)) log_buffers(k)%upstream_location = upstream_location
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
        k = log_buffer_index()
        if (k == 0) return
        if (.not. log_buffers(k)%has_args) return
        args = log_buffers(k)

        if (allocated(args%upstream_location)) then
            call incoming_outgoing_internal(args%backend, args%upstream_location, buffers(current)%name, args%type, &
                                            status)
        else
            call vc_print_log_internal(args%backend, args%files, args%working_revision, status, &
                                       args%is_start_revision, args%limit, args%type)
        end if
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
        buffers(current)%directory = saved_dir
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
        buffers(current)%directory = saved_dir
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
        use minibuf_mod, only: read_from_minibuffer
        integer(int64), intent(out) :: limit
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: answer
        character(len=24) :: buffer
        integer :: ios

        limit = 0
        write (buffer, "(i0)") vc_log_show_limit
        call read_from_minibuffer("Limit display (0 for unlimited): ", answer, status, initial_input=trim(buffer))
        if (.not. status%ok()) return
        ! (string-to-number ...): 0 when not a number.
        read (answer, *, iostat=ios) limit
        if (ios /= 0) limit = 0
    end subroutine read_limit

    module procedure cmd_vc_print_log
    use minibuf_mod, only: read_from_minibuffer
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
    use value_mod, only: value_type
    use minibuf_mod, only: read_string
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
        use syntax_mod, only: skip_syntax_forward, skip_syntax_backward
        use editfns_mod, only: buffer_substring
        character(len=:), allocatable :: word
        integer(int64) :: opoint, beginning, moved
        type(status_type) :: st

        opoint = point()
        moved = skip_syntax_backward("w")
        beginning = point()
        moved = skip_syntax_forward("w")
        word = ""
        if (point() > beginning) call buffer_substring(beginning, point(), word, st)
        call goto_char(opoint)
    end function word_at_point

    ! ------------------------------------------------------------------
    ! Branch logs
    ! ------------------------------------------------------------------

    !> Read the name of a branch to log.
    !> FILESET, if true, means to pass the current VC fileset to
    !> `vc_read_revision'.  Port of vc.el:vc--read-branch-to-log.
    recursive subroutine read_branch_to_log(branch, status, fileset)
        character(len=:), allocatable, intent(out) :: branch
        type(status_type), intent(out) :: status
        logical, intent(in) :: fileset
        type(vc_fileset_type) :: fs
        type(string_type) :: none(0)

        if (fileset) then
            call vc_deduce_fileset(fs, status, .true.)
            if (.not. status%ok()) return
            call vc_read_revision("Branch to log: ", branch, status, fs%files, fs%backend)
        else
            call vc_read_revision("Branch to log: ", branch, status, none)
        end if

        if (.not. status%ok()) return
        if (len(branch) == 0) call set_error(status, FM_USER_ERROR, "No branch specified")
    end subroutine read_branch_to_log

    !> `vc_log_show_limit' as a log's LIMIT (none when not positive).
    !> Not a port.
    function show_limit() result(lim)
        type(vc_log_limit_type) :: lim

        if (vc_log_show_limit > 0) then
            lim%kind = VC_LIMIT_NUMBER
            lim%n = vc_log_show_limit
        end if
    end function show_limit

    module procedure vc_print_fileset_branch_log
    type(vc_fileset_type) :: fs

    ! Currently the prefix argument is conserved.  Possibly it could be
    ! used to prompt for a LIMIT argument like C-x v l has.  Though
    ! now we have "Show 4X entries" and "Show unlimited entries" that
    ! might be a waste of the prefix argument to this command.  --spwhitton
    call vc_deduce_fileset(fs, status, .true.)
    if (.not. status%ok()) return
    call vc_print_log_internal(fs%backend, fs%files, branch, status, .true., show_limit())
    end procedure vc_print_fileset_branch_log

    module procedure vc_print_root_branch_log
    type(vc_fileset_type) :: fs
    character(len=:), allocatable :: saved_dir
    type(handle_type) :: old
    type(status_type) :: st

    ! Prefix argument conserved; see previous command.  --spwhitton
    call with_backend_in_rootdir("VC branch log", fs, saved_dir, status)

    if (.not. status%ok()) then
        buffers(current)%directory = saved_dir
        return
    end if

    old = current_buffer()
    call vc_print_log_internal(fs%backend, fs%files, branch, status, .true., show_limit())

    if (is_buffer_live(old)) then
        call set_buffer(old, st)
        buffers(current)%directory = saved_dir
        call set_buffer(get_buffer("*vc-change-log*"), st)
    end if
    end procedure vc_print_root_branch_log

    module procedure cmd_vc_print_fileset_branch_log
    character(len=:), allocatable :: branch

    call read_branch_to_log(branch, status, .true.)
    if (.not. status%ok()) return
    call vc_print_fileset_branch_log(branch, status)
    end procedure cmd_vc_print_fileset_branch_log

    module procedure cmd_vc_print_root_branch_log
    character(len=:), allocatable :: branch

    call read_branch_to_log(branch, status, .false.)
    if (.not. status%ok()) return
    call vc_print_root_branch_log(branch, status)
    end procedure cmd_vc_print_root_branch_log

    ! ------------------------------------------------------------------
    ! Incoming and outgoing logs
    ! ------------------------------------------------------------------

    !> Show the incoming or outgoing log (TYPE) of BACKEND's repository for
    !> UPSTREAM_LOCATION in the buffer BUFFER_NAME.  Port of
    !> vc.el:vc-incoming-outgoing-internal.
    recursive subroutine incoming_outgoing_internal(backend, upstream_location, buffer_name, type, status)
        integer, intent(in) :: backend
        character(len=*), intent(in) :: upstream_location, buffer_name
        integer, intent(in) :: type
        type(status_type), intent(out) :: status
        type(string_type), allocatable :: files(:)
        type(vc_log_limit_type) :: none
        character(len=:), allocatable :: root
        logical :: found

        call vc_root_dir(root, found, status)
        if (.not. status%ok()) return
        call append_string(files, root)
        call vc_log_internal_common(backend, buffer_name, files, type, .false., "", .false., none, status, &
                                    upstream_location)
    end subroutine incoming_outgoing_internal

    !> Read the upstream location if there is a prefix argument, else
    !> give "" (none; also for an empty answer).  Port of
    !> vc.el:vc--maybe-read-upstream-location.
    recursive subroutine maybe_read_upstream_location(location, status)
        use minibuf_mod, only: read_string
        character(len=:), allocatable, intent(out) :: location
        type(status_type), intent(out) :: status

        status = status_type()
        location = ""
        if (current_prefix_arg%kind == PREFIX_NONE) return
        call read_string("Upstream location/branch (empty for default): ", location, status, &
                         history="vc_remote_location_history")
    end subroutine maybe_read_upstream_location

    !> Show the incoming or outgoing log (TYPE) in BUFFER_NAME of the
    !> current VC tree.  The body of vc.el:vc-root-log-incoming and
    !> vc.el:vc-root-log-outgoing.  Not a port.
    recursive subroutine root_log_incoming_outgoing(upstream_location, buffer_name, type, status)
        character(len=*), intent(in) :: upstream_location, buffer_name
        integer, intent(in) :: type
        type(status_type), intent(out) :: status
        type(vc_fileset_type) :: fs
        character(len=:), allocatable :: saved_dir
        type(handle_type) :: old
        type(status_type) :: st

        call with_backend_in_rootdir("VC root-log", fs, saved_dir, status)

        if (.not. status%ok()) then
            buffers(current)%directory = saved_dir
            return
        end if

        old = current_buffer()
        call incoming_outgoing_internal(fs%backend, upstream_location, buffer_name, type, status)

        if (is_buffer_live(old)) then
            call set_buffer(old, st)
            buffers(current)%directory = saved_dir
            if (status%ok()) call set_buffer(get_buffer(buffer_name), st)
        end if
    end subroutine root_log_incoming_outgoing

    module procedure vc_root_log_incoming
    if (present(upstream_location)) then
        call root_log_incoming_outgoing(upstream_location, "*vc-incoming*", VC_LOG_VIEW_LOG_INCOMING, status)
    else
        call root_log_incoming_outgoing("", "*vc-incoming*", VC_LOG_VIEW_LOG_INCOMING, status)
    end if
    end procedure vc_root_log_incoming

    module procedure vc_root_log_outgoing
    if (present(upstream_location)) then
        call root_log_incoming_outgoing(upstream_location, "*vc-outgoing*", VC_LOG_VIEW_LOG_OUTGOING, status)
    else
        call root_log_incoming_outgoing("", "*vc-outgoing*", VC_LOG_VIEW_LOG_OUTGOING, status)
    end if
    end procedure vc_root_log_outgoing

    module procedure cmd_vc_root_log_incoming
    character(len=:), allocatable :: location

    call maybe_read_upstream_location(location, status)
    if (.not. status%ok()) return
    call vc_root_log_incoming(status, location)
    end procedure cmd_vc_root_log_incoming

    module procedure cmd_vc_root_log_outgoing
    character(len=:), allocatable :: location

    call maybe_read_upstream_location(location, status)
    if (.not. status%ok()) return
    call vc_root_log_outgoing(status, location)
    end procedure cmd_vc_root_log_outgoing

    !> Insert in BUFFER the log of the changes that will be received when
    !> pulling from UPSTREAM_LOCATION: from the merge base with the
    !> incoming revision (fetched again) to that revision.  Port of
    !> vc.el:vc-default-log-incoming.
    recursive subroutine vc_default_log_incoming(backend, buffer, upstream_location, status)
        integer, intent(in) :: backend
        type(handle_type), intent(in) :: buffer
        character(len=*), intent(in) :: upstream_location
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: incoming

        call vc_incoming_revision(backend, upstream_location, incoming, status, .true.)
        if (.not. status%ok()) return
        call print_log_from_mergebase(backend, buffer, incoming, incoming, status)
    end subroutine vc_default_log_incoming

    !> Insert in BUFFER the log of the changes that will be sent when
    !> pushing to UPSTREAM_LOCATION: from the merge base with the incoming
    !> revision to the working revision.  Port of
    !> vc.el:vc-default-log-outgoing.
    recursive subroutine vc_default_log_outgoing(backend, buffer, upstream_location, status)
        integer, intent(in) :: backend
        type(handle_type), intent(in) :: buffer
        character(len=*), intent(in) :: upstream_location
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: incoming

        call vc_incoming_revision(backend, upstream_location, incoming, status)
        if (.not. status%ok()) return
        call print_log_from_mergebase(backend, buffer, "", incoming, status)
    end subroutine vc_default_log_outgoing

    !> Insert in BUFFER the short log of BACKEND's repository root from
    !> START_REVISION ("" for the working revision) back to the merge base
    !> with INCOMING.  The common part of vc.el:vc-default-log-incoming and
    !> vc.el:vc-default-log-outgoing.  Not a port.
    recursive subroutine print_log_from_mergebase(backend, buffer, start_revision, incoming, status)
        integer, intent(in) :: backend
        type(handle_type), intent(in) :: buffer
        character(len=*), intent(in) :: start_revision, incoming
        type(status_type), intent(out) :: status
        type(string_type), allocatable :: files(:)
        type(vc_log_limit_type) :: limit
        character(len=:), allocatable :: root, saved_dir
        logical :: found, limit_unsupported

        call vc_root_dir(root, found, status, backend)
        if (.not. status%ok()) return
        saved_dir = buffers(current)%directory
        buffers(current)%directory = root

        if (.not. associated(vc_backends(backend)%mergebase)) then
            call vc_not_supported(status, "mergebase", backend)
        else if (.not. associated(vc_backends(backend)%print_log)) then
            call vc_not_supported(status, "print-log", backend)
        else
            limit%kind = VC_LIMIT_REVISION
            call vc_backends(backend)%mergebase(incoming, "", limit%revision, status)
        end if

        if (status%ok()) then
            call append_string(files, root)
            call vc_backends(backend)%print_log(files, buffer, .true., start_revision, limit, status, limit_unsupported)
        end if

        buffers(current)%directory = saved_dir
    end subroutine print_log_from_mergebase
end submodule vc_log_mod
