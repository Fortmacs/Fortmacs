! vc_svn_mod.f90 -- non-resident support for Subversion version-control.
! Ported from GNU Emacs 31.1 lisp/vc/vc-svn.el.
!
! Copyright (C) 2003-2026 Free Software Foundation, Inc.
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

!> The Subversion backend of VC: whether a file is registered and its
!> state (from `svn status -v'), its working revision, the root of a
!> working copy, creating a repository (svnadmin create and a checkout),
!> registering, checking out, reverting, deleting and renaming files,
!> diffs, other revisions (svn cat), the previous revision, revision
!> completion, committing, logs (`vc_svn_log_view_mode') and the VC-Dir
!> states (`svn status -u') and headers.
!>
!> Deferred: see the vc_svn_mod row of PORTING.md.
module vc_svn_mod
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use data_mod, only: number_to_string
    use error_mod, only: status_type, set_error, FM_GENERIC_ERROR
    use handle_mod, only: handle_type
    use buffer_mod, only: buffers, current, current_buffer, set_buffer, get_or_make_buffer, close_buffer, &
                          generate_new_buffer_name, is_buffer_live, get_file_buffer
    use editfns_mod, only: point, goto_char, point_min, point_max, buffer_substring, line_end_position, &
                           line_beginning_position, char_after
    use insdel_mod, only: insert
    use search_mod, only: re_search_forward, search_forward, match_beginning, SEARCH_NOERROR
    use subr_mod, only: match_string
    use fileio_mod, only: expand_file_name, file_name_directory, file_exists, is_file_accessible_directory, &
                          file_attributes, file_stat_type
    use files_mod, only: file_relative_name
    use xdisp_mod, only: message
    use window_mod, only: pop_to_buffer, shrink_window_if_larger_than_buffer
    use vc_hooks_mod, only: vc_backends, vc_string_type, append_vc_string, vc_switches_type, register_vc_backend, &
                            vc_find_root, vc_backend_named, vc_working_revision, vc_mode_line, vc_dir_entry_type, &
                            vc_dir_update_procedure, vc_file_set_state, vc_file_get_state, vc_file_set_working_revision, &
                            vc_file_get_working_revision, vc_file_set_checkout_time, vc_log_limit_type, &
                            VC_SWITCHES_NONE, VC_SWITCHES_UNSPECIFIED, VC_SWITCHES_LIST, VC_LIMIT_NUMBER, &
                            VC_LIMIT_REVISION, VC_MODEL_IMPLICIT, VC_STATE_UNKNOWN, VC_UP_TO_DATE, VC_EDITED, &
                            VC_NEEDS_UPDATE, VC_NEEDS_MERGE, VC_ADDED, VC_REMOVED, VC_CONFLICT, VC_IGNORED, &
                            VC_UNREGISTERED
    use vc_dispatcher_mod, only: vc_do_command, vc_setup_buffer, vc_exec_after, vc_action_type, &
                                 VC_OKSTATUS_UNLESS_SIGNAL, VC_OKSTATUS_ASYNC, VC_DEST_CURRENT
    use vc_mod, only: vc_switches, vc_log_view_type, VC_LOG_VIEW_WITH_DIFF
    use ewoc_mod, only: ewoc_part_type
    use hooks_mod, only: hook_type
    use commands_mod, only: commands
    use modes_mod, only: major_modes
    use log_edit_mod, only: log_edit_extract_headers, log_edit_string_type, log_edit_header_type
    implicit none(type, external)
    private

    public :: init_vc_svn, vc_svn_registered, vc_svn_state, vc_svn_parse_status, vc_svn_working_revision, &
              vc_svn_root, vc_svn_command, vc_svn_previous_revision, vc_svn_revision_table, vc_svn_log_view_mode

    !> Name of the SVN executable.  Port of vc-svn.el vc-svn-program.
    character(len=:), allocatable, public :: vc_svn_program

    !> Global switches to pass to any SVN command.
    !> The option "--non-interactive" is often needed to prevent SVN
    !> hanging while prompting for authorization.  Port of vc-svn.el
    !> vc-svn-global-switches (VC_SWITCHES_LIST: the switches; any other
    !> kind: none).
    type(vc_switches_type), public :: vc_svn_global_switches

    !> Switches for registering a file into SVN.
    !> A string or list of strings passed to the checkin program by
    !> \[vc_register].  If VC_SWITCHES_UNSPECIFIED, use the value of `vc_register_switches'.
    !> If VC_SWITCHES_NONE, use no switches.  Port of vc-svn.el vc-svn-register-switches.
    type(vc_switches_type), target, public :: vc_svn_register_switches

    !> String or list of strings specifying extra switches for svn diff under VC.
    !> If VC_SWITCHES_UNSPECIFIED, use the value of `vc_diff_switches' (or `diff_switches'),
    !> together with "-x --diff-cmd="`diff_command' (since `svn diff'
    !> does not support the default "-c" value of `diff_switches').
    !> If you want to force an empty list of arguments, use VC_SWITCHES_NONE.  Port of
    !> vc-svn.el vc-svn-diff-switches.
    type(vc_switches_type), target, public :: vc_svn_diff_switches = vc_switches_type(kind=VC_SWITCHES_NONE)

    !> The name of the ".svn" subdirectory or its equivalent.  Port of
    !> vc-svn.el vc-svn-admin-directory.
    character(len=*), parameter, public :: vc_svn_admin_directory = ".svn"

    !> Hook run after entering SVN-Log-View mode.
    type(hook_type), target, public :: vc_svn_log_view_mode_hook = hook_type(name="vc_svn_log_view_mode_hook")

    !> The keymap of `vc_svn_log_view_mode'.
    type(handle_type), public :: vc_svn_log_view_mode_map

    ! A `dir_status_files' computation in progress: its update function
    ! (vc-svn-after-dir-status's CALLBACK) and VC-Dir buffer.
    type :: dir_status_state_type
        logical :: live = .false.
        procedure(vc_dir_update_procedure), pointer, nopass :: update_function => null()
        type(handle_type) :: buffer
    end type dir_status_state_type

    ! The computations of `vc_svn_dir_status_files' in progress.
    type(dir_status_state_type), allocatable :: dir_states(:)

    character, parameter :: LF = achar(10)

contains

    ! ------------------------------------------------------------------
    ! Internal functions
    ! ------------------------------------------------------------------

    !> A wrapper around `vc_do_command' for use in vc_svn_mod.
    !> The difference to `vc_do_command' is that this function always invokes `svn',
    !> and that it passes `vc_svn_global_switches' to it before FLAGS.
    !> BUFFER is the output buffer (*vc* if absent and CURRENT false; the
    !> current buffer with CURRENT).  Port of vc-svn.el:vc-svn-command.
    recursive subroutine vc_svn_command(okstatus, files, flags, status, buffer, current, exit_status, process)
        integer, intent(in) :: okstatus
        type(vc_string_type), intent(in) :: files(:), flags(:)
        type(status_type), intent(out) :: status
        type(handle_type), intent(in), optional :: buffer
        logical, intent(in), optional :: current
        integer, intent(out), optional :: exit_status
        type(handle_type), intent(out), optional :: process
        type(vc_string_type), allocatable :: all(:)
        type(handle_type) :: buffer_handle
        logical :: in_current

        status = status_type()
        allocate (all(0))

        if (vc_svn_global_switches%kind == VC_SWITCHES_LIST) then
            if (allocated(vc_svn_global_switches%list)) all = vc_svn_global_switches%list
        end if

        call append_all(all, flags)
        in_current = .false.
        if (present(current)) in_current = current

        if (in_current) then
            call vc_do_command(VC_DEST_CURRENT, okstatus, vc_svn_program, files, all, status, exit_status=exit_status, &
                               process=process)
        else
            if (present(buffer)) then
                buffer_handle = buffer
            else
                call get_or_make_buffer("*vc*", buffer_handle, status)
                if (.not. status%ok()) return
            end if

            call vc_do_command(VC_DEST_CURRENT, okstatus, vc_svn_program, files, all, status, buffer=buffer_handle, &
                               exit_status=exit_status, process=process)
        end if
    end subroutine vc_svn_command

    !> Append the strings MORE to LIST.  Not a port.
    subroutine append_all(list, more)
        type(vc_string_type), allocatable, intent(inout) :: list(:)
        type(vc_string_type), intent(in) :: more(:)
        type(vc_string_type), allocatable :: grown(:)
        integer :: n

        if (.not. allocated(list)) allocate (list(0))
        n = size(list)
        allocate (grown(n + size(more)))
        grown(1:n) = list
        grown(n + 1:) = more
        call move_alloc(grown, list)
    end subroutine append_all

    !> The strings given, as a list (for the short lists of svn
    !> arguments).  Not a port.
    function strings(a1, a2, a3, a4) result(list)
        character(len=*), intent(in) :: a1
        character(len=*), intent(in), optional :: a2, a3, a4
        type(vc_string_type), allocatable :: list(:)

        allocate (list(0))
        call append_vc_string(list, a1)
        if (present(a2)) call append_vc_string(list, a2)
        if (present(a3)) call append_vc_string(list, a3)
        if (present(a4)) call append_vc_string(list, a4)
    end function strings

    !> The current buffer's `default_directory'.  Not a port.
    function buffer_directory() result(dir)
        character(len=:), allocatable :: dir

        dir = "./"
        if (current == 0) return
        if (allocated(buffers(current)%directory)) dir = buffers(current)%directory
    end function buffer_directory

    !> Make a temporary buffer current (with-temp-buffer), with DIR as its
    !> `default_directory'; OLD is the buffer to go back to.  Not a port.
    subroutine temp_begin(old, tmp, dir)
        type(handle_type), intent(out) :: old, tmp
        character(len=*), intent(in) :: dir
        type(status_type) :: st

        old = current_buffer()
        call get_or_make_buffer(generate_new_buffer_name(" *temp*"), tmp, st)
        call set_buffer(tmp, st)
        buffers(current)%directory = dir
    end subroutine temp_begin

    !> Kill the temporary buffer TMP and make OLD current again.  Not a port.
    subroutine temp_end(old, tmp)
        type(handle_type), intent(in) :: old, tmp
        type(status_type) :: st

        call close_buffer(tmp, st)
        call set_buffer(old, st)
    end subroutine temp_end

    !> The index of the SVN backend.  Not a port.
    integer function svn_backend_index() result(b)
        b = vc_backend_named("SVN")
    end function svn_backend_index

    !> S as a number (string-to-number: 0 when it does not start with
    !> one).  Not a port.
    integer(int64) function string_to_number(s) result(n)
        character(len=*), intent(in) :: s
        integer :: k, ios

        n = 0
        k = verify(s, "0123456789")

        if (k == 0) then
            k = len(s) + 1
        end if

        if (k == 1) return
        read (s(1:k - 1), *, iostat=ios) n
        if (ios /= 0) n = 0
    end function string_to_number

    ! ------------------------------------------------------------------
    ! State-querying functions
    ! ------------------------------------------------------------------

    !> Check if FILE is SVN registered.  Port of vc-svn.el:vc-svn-registered.
    logical function vc_svn_registered(file) result(registered)
        character(len=*), intent(in) :: file
        character(len=:), allocatable :: abs, root, dir
        type(handle_type) :: old, tmp
        type(status_type) :: st
        logical :: found
        integer :: code, parsed

        registered = .false.
        call expand_file_name(file, abs, st)
        if (.not. st%ok()) return
        call vc_svn_root(abs, root, found)
        if (.not. found) return
        dir = file_name_directory(abs)
        if (.not. is_file_accessible_directory(dir)) return
        call temp_begin(old, tmp, dir)
        ! Ignore all errors.
        call vc_svn_command(VC_OKSTATUS_UNLESS_SIGNAL, strings(abs), strings("status", "-v"), st, current=.true., &
                            exit_status=code)

        ! Some problem happened.  E.g. We can't find an `svn' executable.
        if (st%ok() .and. code == 0) then
            parsed = vc_svn_parse_status(abs)
            registered = parsed /= VC_STATE_UNKNOWN .and. parsed /= VC_IGNORED .and. parsed /= VC_UNREGISTERED
        end if

        call temp_end(old, tmp)
    end function vc_svn_registered

    !> SVN-specific version of `vc_state'.  Port of vc-svn.el:vc-svn-state.
    integer function vc_svn_state(file) result(state)
        character(len=*), intent(in) :: file
        type(handle_type) :: old, tmp
        type(status_type) :: st
        character(len=:), allocatable :: dir

        dir = file_name_directory(file)
        if (len(dir) == 0) dir = buffer_directory()
        call temp_begin(old, tmp, dir)
        call vc_svn_command(0, strings(file), strings("status", "-v"), st, current=.true.)
        state = vc_svn_parse_status(file)
        call temp_end(old, tmp)
    end function vc_svn_state

    !> Parse output of "svn status" command in the current buffer.
    !> Set file properties accordingly.  If FILENAME is present, return its
    !> status (VC_STATE_UNKNOWN otherwise).  Port of
    !> vc-svn.el:vc-svn-parse-status (Svn's own status, the
    !> `vc-svn-status' property, is not kept: only the deferred
    !> `vc-svn-find-file-hook' reads it).
    integer function vc_svn_parse_status(filename) result(state)
        character(len=*), intent(in), optional :: filename
        character(len=:), allocatable :: file, rest
        type(file_stat_type) :: attr
        type(status_type) :: st
        integer(int64) :: found
        integer(int32) :: status, propstat, flag
        logical :: multifile, has, has_attr

        state = VC_STATE_UNKNOWN
        multifile = .false.
        call goto_char(point_min())

        do
            call re_search_forward("^\(?:\?\|[ ACDGIMR!~][ MC][ L][ +][ SX]..\([ *]\) +\([-0-9]+\) +\([0-9?]+\) " &
                                   //"+\([^ ]+\)\) +", found, st, noerror=SEARCH_NOERROR)
            if (found == 0) exit
            ! If the username contains spaces, the output format is ambiguous,
            ! so don't trust the output's filename unless we have to.
            file = ""

            if (present(filename) .and. .not. multifile) then
                file = filename
            else
                call buffer_substring(point(), line_end_position(), rest, st)
                call expand_file_name(rest, file, st, buffer_directory())
            end if

            ! If we are parsing the result of running status on a directory,
            ! there could be multiple files in the output.
            ! We assume that filename, if supplied, applies to the first
            ! listed file (ie, the directory).  Bug#15322.
            multifile = .true.
            call char_after(status, has, line_beginning_position())
            ! Status of the item's properties ([ MC]).
            call char_after(propstat, has, line_beginning_position() + 1)

            if (status == iachar("?")) then
                call vc_file_set_state(file, VC_UNREGISTERED)
                cycle
            end if

            ! Use the last-modified revision, so that searching in vc-print-log
            ! output works.
            call vc_file_set_working_revision(file, match_string(3))
            call char_after(flag, has, match_beginning(1, st))

            if (status == iachar(" ") .and. propstat == iachar(" ")) then
                if (flag == iachar("*")) then
                    call vc_file_set_state(file, VC_NEEDS_UPDATE)
                else
                    call file_attributes(file, attr, has_attr)

                    if (has_attr) then
                        call vc_file_set_checkout_time(file, attr%mtime_sec, attr%mtime_nsec)
                    else
                        call vc_file_set_checkout_time(file)
                    end if

                    call vc_file_set_state(file, VC_UP_TO_DATE)
                end if
            else if (status == iachar("A")) then
                ! If the file was actually copied, (match-string 2) is "-".
                call vc_file_set_working_revision(file, "0")
                call vc_file_set_checkout_time(file, 0_int64, 0_int32)
                call vc_file_set_state(file, VC_ADDED)
            else if (status == iachar("C") .or. propstat == iachar("C")) then
                ! Conflict in contents or properties.
                call vc_file_set_state(file, VC_CONFLICT)
            else if (status == iachar("M") .or. propstat == iachar("M")) then
                ! Modified contents or properties.
                if (flag == iachar("*")) then
                    call vc_file_set_state(file, VC_NEEDS_MERGE)
                else
                    call vc_file_set_state(file, VC_EDITED)
                end if
            else if (status == iachar("I")) then
                call vc_file_set_state(file, VC_IGNORED)
            else if (status == iachar("D") .or. status == iachar("R")) then
                call vc_file_set_state(file, VC_REMOVED)
            else
                call vc_file_set_state(file, VC_EDITED)
            end if
        end do

        if (present(filename)) state = vc_file_get_state(filename)
    end function vc_svn_parse_status

    !> SVN-specific version of `vc_working_revision'.  Port of
    !> vc-svn.el:vc-svn-working-revision.
    subroutine vc_svn_working_revision(file, rev, found)
        character(len=*), intent(in) :: file
        character(len=:), allocatable, intent(out) :: rev
        logical, intent(out) :: found
        character(len=:), allocatable :: abs
        type(status_type) :: st
        logical :: registered

        ! There is no need to consult RCS headers under SVN, because we
        ! get the workfile version for free when we recognize that a file
        ! is registered in SVN.
        registered = vc_svn_registered(file)
        call expand_file_name(file, abs, st)
        if (.not. st%ok()) abs = file
        call vc_file_get_working_revision(abs, rev, found)
    end subroutine vc_svn_working_revision

    !> The revision before REV ("" and FOUND false for none).  Port of
    !> vc-svn.el:vc-svn-previous-revision.
    subroutine vc_svn_previous_revision(file, rev, prev, found)
        character(len=*), intent(in) :: file, rev
        character(len=:), allocatable, intent(out) :: prev
        logical, intent(out) :: found
        integer(int64) :: newrev

        prev = ""
        if (len(file) < 0) prev = ""
        newrev = string_to_number(rev) - 1
        found = 0 < newrev
        if (found) prev = number_to_string(newrev)
    end subroutine vc_svn_previous_revision

    !> The revisions of FILES, from `svn log -q'.  Port of
    !> vc-svn.el:vc-svn-revision-table (as vc-svn-revision-completion-table).
    subroutine vc_svn_revision_table(files, table)
        type(vc_string_type), intent(in) :: files(:)
        type(vc_string_type), allocatable, intent(out) :: table(:)
        type(vc_string_type), allocatable :: one(:)
        type(handle_type) :: old, vc
        type(status_type) :: st
        integer(int64) :: found

        allocate (table(0), one(1))
        old = current_buffer()
        call get_or_make_buffer("*vc*", vc, st)
        if (.not. st%ok()) return
        call set_buffer(vc, st)
        call vc_svn_command(0, files, strings("log", "-q"), st)
        call set_buffer(vc, st)
        call goto_char(point_min())

        do
            call re_search_forward("^r\([0-9]+\) ", found, st, noerror=SEARCH_NOERROR)
            if (found == 0) exit
            ! (push (match-string 1 loglines) vc-svn-revisions)
            one(1)%s = match_string(1)
            table = [one, table]
        end do

        call set_buffer(old, st)
    end subroutine vc_svn_revision_table

    !> The root of FILE's working copy (the directory with .svn).  Port of
    !> vc-svn.el:vc-svn-root.
    subroutine vc_svn_root(file, root, found)
        character(len=*), intent(in) :: file
        character(len=:), allocatable, intent(out) :: root
        logical, intent(out) :: found

        call vc_find_root(file, vc_svn_admin_directory, root, found)
    end subroutine vc_svn_root

    !> SVN's checkout model: `implicit'.  Port of
    !> vc-svn.el:vc-svn-checkout-model.
    integer function vc_svn_checkout_model(files) result(model)
        type(vc_string_type), intent(in) :: files(:)

        model = VC_MODEL_IMPLICIT
        if (size(files) < 0) model = 0
    end function vc_svn_checkout_model

    ! ------------------------------------------------------------------
    ! State-changing functions
    ! ------------------------------------------------------------------

    !> Create a new SVN repository.  Port of vc-svn.el:vc-svn-create-repo.
    recursive subroutine vc_svn_create_repo(status)
        type(status_type), intent(out) :: status
        type(vc_string_type) :: none(0)
        type(handle_type) :: vc
        character(len=:), allocatable :: defdir, url

        call get_or_make_buffer("*vc*", vc, status)
        if (.not. status%ok()) return
        call vc_do_command(VC_DEST_CURRENT, 0, "svnadmin", none, strings("create", "SVN"), status, buffer=vc)
        if (.not. status%ok()) return
        ! Expand default-directory because svn gets confused by eg
        ! file://~/path/to/file.  (Bug#15446).
        call expand_file_name(buffer_directory(), defdir, status)
        if (.not. status%ok()) return
        ! MS-Windows files d:/foo/bar need to begin with 3 leading slashes.
        url = "file:///"

        if (len(defdir) > 0) then
            if (defdir(1:1) == "/") url = "file://"
        end if

        url = url//defdir//"SVN"
        call vc_svn_command(0, strings("."), strings("checkout", url), status, buffer=vc)
    end subroutine vc_svn_create_repo

    !> Register FILES into the SVN version control system.
    !> The COMMENT argument is ignored  This does an add but not a commit.
    !> Passes either `vc_svn_register_switches' or `vc_register_switches'
    !> to the SVN command.  Port of vc-svn.el:vc-svn-register.
    recursive subroutine vc_svn_register(files, status, comment)
        type(vc_string_type), intent(in) :: files(:)
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: comment
        type(vc_string_type), allocatable :: flags(:), sw(:)

        if (present(comment)) continue
        flags = strings("add")
        call vc_switches(svn_backend_index(), "register", sw)
        call append_all(flags, sw)
        call vc_svn_command(0, files, flags, status)
    end subroutine vc_svn_register

    !> SVN-specific version of `vc_backend_checkin'.  Port of
    !> vc-svn.el:vc-svn-checkin.
    recursive subroutine vc_svn_checkin(files, comment, status, rev)
        use help_mod, only: substitute_command_keys
        type(vc_string_type), intent(in) :: files(:)
        character(len=*), intent(in) :: comment
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: rev
        type(log_edit_header_type) :: no_headers(0)
        type(log_edit_string_type), allocatable :: extracted(:)
        type(vc_string_type), allocatable :: flags(:), sw(:)
        character(len=:), allocatable :: msg
        type(handle_type) :: old, vc
        type(status_type) :: st
        integer(int64) :: found
        integer :: code, k

        if (present(rev)) continue
        call log_edit_extract_headers(no_headers, comment, msg, extracted, status)
        if (.not. status%ok()) return
        flags = strings("ci", "-m", msg)

        do k = 1, size(extracted)
            call append_vc_string(flags, extracted(k)%s)
        end do

        call vc_switches(svn_backend_index(), "checkin", sw)
        call append_all(flags, sw)
        old = current_buffer()
        call vc_svn_command(1, files, flags, status, exit_status=code)
        if (.not. status%ok()) return
        call get_or_make_buffer("*vc*", vc, status)
        if (.not. status%ok()) return
        call set_buffer(vc, st)
        call goto_char(point_min())

        if (code /= 0) then
            ! Check checkin problem.
            call search_forward("Transaction is out of date", found, st, noerror=SEARCH_NOERROR)

            if (found /= 0) then
                do k = 1, size(files)
                    call vc_file_set_state(files(k)%s, VC_NEEDS_MERGE)
                end do

                call set_error(status, FM_GENERIC_ERROR, &
                               substitute_command_keys("Up-to-date check failed: type \[vc_next_action] to merge in changes"))
            else
                call pop_to_buffer(current_buffer(), st)
                call goto_char(point_min())
                call shrink_window_if_larger_than_buffer()
                call set_error(status, FM_GENERIC_ERROR, "Check-in failed")
            end if

            return
        end if

        call set_buffer(old, st)
    end subroutine vc_svn_checkin

    !> SVN-specific retrieval of a specified version into a buffer.  Port of
    !> vc-svn.el:vc-svn-find-revision.
    recursive subroutine vc_svn_find_revision(file, rev, buffer, status)
        character(len=*), intent(in) :: file, rev
        type(handle_type), intent(in) :: buffer
        type(status_type), intent(out) :: status
        type(vc_string_type), allocatable :: flags(:), sw(:)
        type(vc_string_type) :: empty

        flags = strings("cat")

        if (len(rev) > 0) then
            call append_vc_string(flags, "-r"//rev)
        else
            flags = [flags, empty]
        end if

        call vc_switches(svn_backend_index(), "checkout", sw)
        call append_all(flags, sw)
        call vc_svn_command(0, strings(file), flags, status, buffer=buffer)
    end subroutine vc_svn_find_revision

    !> Check out REV of FILE (the base revision if absent; the head if "").
    !> Port of vc-svn.el:vc-svn-checkout.
    recursive subroutine vc_svn_checkout(file, status, rev)
        character(len=*), intent(in) :: file
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: rev
        type(vc_string_type), allocatable :: sw(:)
        type(handle_type) :: old, buffer
        type(status_type) :: st

        call message("Checking out "//file//"...")
        old = current_buffer()
        buffer = get_file_buffer(file)
        if (is_buffer_live(buffer)) call set_buffer(buffer, st)
        call vc_switches(svn_backend_index(), "checkout", sw)
        call vc_svn_update(file, sw, status, rev)
        call set_buffer(old, st)
        if (.not. status%ok()) return
        call vc_mode_line(file, svn_backend_index())
        call message("Checking out "//file//"...done")
    end subroutine vc_svn_checkout

    !> Update FILE to REV with SWITCHES (nothing to do for an existing file
    !> when REV is absent).  Port of vc-svn.el:vc-svn-update.
    recursive subroutine vc_svn_update(file, switches, status, rev)
        character(len=*), intent(in) :: file
        type(vc_string_type), intent(in) :: switches(:)
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: rev
        type(vc_string_type), allocatable :: flags(:)
        type(vc_string_type) :: empty

        status = status_type()
        ! If no revision was specified, there's nothing to do.
        if (file_exists(file) .and. .not. present(rev)) return
        ! Check out a particular version (or recreate the file).
        call vc_file_set_working_revision(file)
        flags = strings("update")

        if (.not. present(rev)) then
            call append_vc_string(flags, "-rBASE")
        else if (len(rev) == 0) then
            flags = [flags, empty]
        else
            call append_vc_string(flags, "-r"//rev)
        end if

        call append_all(flags, switches)
        call vc_svn_command(0, strings(file), flags, status)
    end subroutine vc_svn_update

    !> Delete FILE with `svn remove'.  Port of vc-svn.el:vc-svn-delete-file.
    recursive subroutine vc_svn_delete_file(file, status)
        character(len=*), intent(in) :: file
        type(status_type), intent(out) :: status

        call vc_svn_command(0, strings(file), strings("remove"), status)
    end subroutine vc_svn_delete_file

    !> Rename OLD to NEW with `svn move'.  Port of vc-svn.el:vc-svn-rename-file.
    recursive subroutine vc_svn_rename_file(old, new, status)
        character(len=*), intent(in) :: old, new
        type(status_type), intent(out) :: status

        call vc_svn_command(0, strings(new), strings("move", file_relative_name(old)), status)
    end subroutine vc_svn_rename_file

    !> Revert FILE to the version it was based on.  Port of
    !> vc-svn.el:vc-svn-revert.
    recursive subroutine vc_svn_revert(file, status, contents_done)
        character(len=*), intent(in) :: file
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: contents_done

        status = status_type()
        if (.not. present(contents_done)) call vc_svn_command(0, strings(file), strings("revert"), status)
    end subroutine vc_svn_revert

    ! ------------------------------------------------------------------
    ! History functions
    ! ------------------------------------------------------------------

    !> Major mode for SVN logs.  Port of vc-svn.el:vc-svn-log-view-mode.
    recursive subroutine vc_svn_log_view_mode(status)
        use log_view_mod, only: set_log_view_per_file_logs
        type(status_type), intent(out) :: status
        logical :: saved

        if (.not. associated(major_modes%vc_svn_log_view_mode%execute)) call define_log_view_mode()
        call major_modes%vc_svn_log_view_mode%begin(saved, status)
        if (.not. status%ok()) return
        call set_log_view_per_file_logs(.false.)
        call major_modes%vc_svn_log_view_mode%end(saved, status)
    end subroutine vc_svn_log_view_mode

    ! Define SVN's Log View mode.  Not a port.
    subroutine define_log_view_mode()
        use log_view_mod, only: init_log_view
        use add_log_mod, only: init_add_log

        ! (require 'add-log)
        if (.not. associated(major_modes%change_log_mode%execute)) call init_add_log()
        if (.not. associated(major_modes%log_view_mode%execute)) call init_log_view()
        call major_modes%vc_svn_log_view_mode%define("vc_svn_log_view_mode", "SVN-Log-View", vc_svn_log_view_mode, &
                                                     vc_svn_log_view_mode_hook, parent=major_modes%log_view_mode)
        vc_svn_log_view_mode_map = major_modes%vc_svn_log_view_mode%map
        commands%vc_svn_log_view_mode%execute => vc_svn_log_view_mode
    end subroutine define_log_view_mode

    !> Print commit log associated with FILES into specified BUFFER.
    !> SHORTLOG is ignored.
    !> If START_REVISION is not "", it is the newest revision to show.
    !> If LIMIT is given, show no more than this many entries.  Port of
    !> vc-svn.el:vc-svn-print-log.
    recursive subroutine vc_svn_print_log(files, buffer, shortlog, start_revision, limit, status, limit_unsupported)
        type(vc_string_type), intent(in) :: files(:)
        type(handle_type), intent(in) :: buffer
        logical, intent(in) :: shortlog
        character(len=*), intent(in) :: start_revision
        type(vc_log_limit_type), intent(in) :: limit
        type(status_type), intent(out) :: status
        logical, intent(out) :: limit_unsupported
        type(vc_string_type), allocatable :: flags(:)
        type(vc_string_type) :: none(0)
        type(handle_type) :: old
        type(status_type) :: st
        logical :: saved_read_only
        integer :: k

        limit_unsupported = .false.
        if (shortlog) continue
        old = current_buffer()
        call vc_setup_buffer(buffer, status)
        if (.not. status%ok()) return
        ! Writable meanwhile, as upstream binds inhibit-read-only.
        saved_read_only = buffers(current)%read_only
        buffers(current)%read_only = .false.
        call goto_char(point_min())

        if (size(files) > 0) then
            do k = 1, size(files)
                call insert("Working file: "//files(k)%s//LF, status)
                if (.not. status%ok()) exit

                if (len(start_revision) > 0) then
                    flags = strings("log", "-r"//start_revision//":1")
                else
                    ! By default Subversion only shows the log up to the
                    ! working revision, whereas we also want the log of the
                    ! subsequent commits.  At least that's what the
                    ! vc-cvs.el code does.
                    flags = strings("log", "-rHEAD:0")
                end if

                call append_options(flags)
                call vc_svn_command(VC_OKSTATUS_ASYNC, files(k:k), flags, status, buffer=buffer)
                if (.not. status%ok()) exit
            end do
        else
            ! Dump log for the entire directory.
            if (len(start_revision) > 0) then
                flags = strings("log", "-r"//start_revision)
            else
                flags = strings("log", "-rHEAD:0")
            end if

            call append_options(flags)
            call vc_svn_command(0, none, flags, status, buffer=buffer)
        end if

        buffers(current)%read_only = saved_read_only
        call set_buffer(old, st)

    contains

        ! The --diff and --limit options.
        subroutine append_options(list)
            type(vc_string_type), allocatable, intent(inout) :: list(:)

            if (vc_log_view_type() == VC_LOG_VIEW_WITH_DIFF) call append_vc_string(list, "--diff")

            if (limit%kind == VC_LIMIT_NUMBER) then
                call append_vc_string(list, "--limit")
                call append_vc_string(list, number_to_string(limit%n))
            else if (limit%kind == VC_LIMIT_REVISION) then
                call append_vc_string(list, "--limit")
                call append_vc_string(list, limit%revision)
            end if
        end subroutine append_options
    end subroutine vc_svn_print_log

    !> Get a difference report using SVN between two revisions of fileset FILES.
    !> Port of vc-svn.el:vc-svn-diff.
    recursive subroutine vc_svn_diff(files, buffer, async, status, rev1, rev2)
        use diff_mod, only: diff_command
        type(vc_string_type), intent(in) :: files(:)
        type(handle_type), intent(in) :: buffer
        logical, intent(in) :: async
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: rev1, rev2
        type(vc_string_type), allocatable :: flags(:), sw(:)
        character(len=:), allocatable :: oldvers, rev, joined
        logical :: has_old, use_async, same, found
        integer :: k

        has_old = present(rev1)
        if (has_old) oldvers = rev1

        if (has_old .and. .not. present(rev2) .and. size(files) > 0) then
            same = .true.

            do k = 1, size(files)
                call vc_working_revision(files(k)%s, rev, found)

                if (.not. found) then
                    same = .false.
                else if (rev /= oldvers) then
                    same = .false.
                end if

                if (.not. same) exit
            end do

            ! Use no revision rather than the current one because svn handles
            ! it better (i.e. locally).  Note that if _any_ of the files
            ! has a different revision, we fetch the lot, which is
            ! obviously sub-optimal.
            if (same) has_old = .false.
        end if

        ! Svn diffs those locally.
        use_async = async .and. (has_old .or. present(rev2))
        flags = strings("diff")

        if (vc_svn_diff_switches%kind /= VC_SWITCHES_UNSPECIFIED) then
            call vc_switches(svn_backend_index(), "diff", sw)
            call append_all(flags, sw)
        else
            call vc_switches(0, "diff", sw)
            joined = ""

            do k = 1, size(sw)
                if (k > 1) joined = joined//" "
                joined = joined//sw(k)%s
            end do

            if (.not. allocated(diff_command)) diff_command = "diff"
            call append_vc_string(flags, "--diff-cmd="//diff_command)
            call append_vc_string(flags, "-x")
            call append_vc_string(flags, joined)
        end if

        if (has_old) then
            call append_vc_string(flags, "-r")

            if (present(rev2)) then
                call append_vc_string(flags, oldvers//":"//rev2)
            else
                call append_vc_string(flags, oldvers)
            end if
        end if

        ! For some reason `svn diff' does not return a useful status
        ! w.r.t whether the diff was empty or not: `vc_diff_internal' looks
        ! at the size of BUFFER.
        if (use_async) then
            call vc_svn_command(VC_OKSTATUS_ASYNC, files, flags, status, buffer=buffer)
        else
            call vc_svn_command(0, files, flags, status, buffer=buffer)
        end if
    end subroutine vc_svn_diff

    ! ------------------------------------------------------------------
    ! VC-Dir
    ! ------------------------------------------------------------------

    !> Parse the output of `svn status' (REMOTE: `svn status -u') in the
    !> current buffer and pass the states found to the update function of
    !> the computation of ACTION.  Port of vc-svn.el:vc-svn-after-dir-status.
    recursive subroutine vc_svn_after_dir_status(action, status)
        type(vc_action_type), intent(in) :: action
        type(status_type), intent(out) :: status
        type(vc_dir_entry_type), allocatable :: result(:), one(:)
        character(len=:), allocatable :: re, flag
        type(handle_type) :: old
        type(status_type) :: st
        integer(int64) :: found
        integer :: state, propstat, k

        status = status_type()
        k = action%n

        if (.not. is_buffer_live(action%buffer)) then
            dir_states(k)%live = .false.
            return
        end if

        old = current_buffer()
        call set_buffer(action%buffer, st)

        if (action%flag) then
            re = "^\(.\)\(.\).....? \([ *]\) +\(?:[-0-9]+\)?   \(.*\)$"
        else
            ! Subexp 3 is a dummy in this case, so the numbers match.
            re = "^\(.\)\(.\)...\(.\).? \(.*\)$"
        end if

        allocate (result(0), one(1))
        call goto_char(point_min())

        do
            call re_search_forward(re, found, st, noerror=SEARCH_NOERROR)
            if (found == 0) exit
            state = state_of(match_string(1))
            propstat = state_of(match_string(2))
            flag = match_string(3)
            one(1)%name = match_string(4)
            ! conflict always wins
            if ((propstat == VC_CONFLICT .or. propstat == VC_EDITED) .and. state /= VC_CONFLICT) state = propstat

            if (action%flag .and. flag == "*") then
                ! FIXME are there other possible combinations?
                if (state == VC_EDITED) then
                    state = VC_NEEDS_MERGE
                else if (state == VC_STATE_UNKNOWN) then
                    state = VC_NEEDS_UPDATE
                end if
            end if

            if (state /= VC_STATE_UNKNOWN) then
                one(1)%state = state
                result = [one, result]
            end if
        end do

        call set_buffer(old, st)
        dir_states(k)%live = .false.
        call dir_states(k)%update_function(result, dir_states(k)%buffer, .false.)

    contains

        ! The state of the status character C (state-map).
        integer function state_of(c) result(s)
            character(len=*), intent(in) :: c

            select case (c)
            case ("A")
                s = VC_ADDED
            case ("C")
                s = VC_CONFLICT
            case ("I")
                s = VC_IGNORED
            case ("M")
                s = VC_EDITED
            case ("D", "R")
                s = VC_REMOVED
            case ("!")
                s = VC_NEEDS_UPDATE
            case ("?")
                s = VC_UNREGISTERED
            case ("~")
                ! This is what vc-svn-parse-status does.
                s = VC_EDITED
            case default
                s = VC_STATE_UNKNOWN
            end select
        end function state_of
    end subroutine vc_svn_after_dir_status

    !> Run "svn status" for DIR and update BUFFER via UPDATE.
    !> UPDATE is called with the entries (FILE, STATE) for directory DIR.
    !> Port of vc-svn.el:vc-svn-dir-status-files.
    recursive subroutine vc_svn_dir_status_files(dir, files, update, buffer, status)
        character(len=*), intent(in) :: dir
        type(vc_string_type), intent(in) :: files(:)
        procedure(vc_dir_update_procedure) :: update
        type(handle_type), intent(in) :: buffer
        type(status_type), intent(out) :: status
        type(dir_status_state_type) :: state
        type(vc_string_type), allocatable :: flags(:)
        type(vc_string_type) :: none(0)
        type(vc_action_type) :: action
        integer :: k

        if (len(dir) < 0) return
        if (.not. allocated(dir_states)) allocate (dir_states(0))

        do k = 1, size(dir_states)
            if (.not. dir_states(k)%live) exit
        end do

        if (k > size(dir_states)) dir_states = [dir_states, state]
        dir_states(k)%live = .true.
        dir_states(k)%update_function => update
        dir_states(k)%buffer = buffer
        ! FIXME shouldn't this rather default to all the files in dir?
        flags = strings("status", "-u")
        call append_all(flags, files)
        call vc_svn_command(VC_OKSTATUS_ASYNC, none, flags, status, current=.true.)

        if (.not. status%ok()) then
            dir_states(k)%live = .false.
            return
        end if

        ! (vc-run-delayed (vc-svn-after-dir-status callback t))
        action%execute => vc_svn_after_dir_status
        action%n = k
        action%flag = .true.
        action%buffer = current_buffer()
        call vc_exec_after(action, status)
    end subroutine vc_svn_dir_status_files

    !> Generate extra status headers for a Subversion working copy.  Port of
    !> vc-svn.el:vc-svn-dir-extra-headers.
    recursive subroutine vc_svn_dir_extra_headers(dir, parts)
        type(ewoc_part_type), allocatable, intent(out) :: parts(:)
        character(len=*), intent(in) :: dir
        type(vc_string_type) :: none(0)
        type(handle_type) :: old, vc
        type(status_type) :: st
        character(len=:), allocatable :: repo
        integer(int64) :: found

        allocate (parts(0))
        if (len(dir) < 0) return
        call vc_svn_command(0, none, strings("info"), st)
        old = current_buffer()
        call get_or_make_buffer("*vc*", vc, st)
        call set_buffer(vc, st)
        call goto_char(point_min())
        call re_search_forward("Repository Root: *\(.*\)", found, st, noerror=SEARCH_NOERROR)
        if (found /= 0) repo = match_string(1)
        call set_buffer(old, st)

        if (allocated(repo)) then
            call append_part("Repository : ", "vc_dir_header")
            call append_part(repo, "vc_dir_header_value")
        end if

    contains

        ! Append TEXT with FACE to PARTS.
        subroutine append_part(text, face)
            character(len=*), intent(in) :: text, face
            type(ewoc_part_type) :: part

            part%text = text
            part%face = face
            parts = [parts, part]
        end subroutine append_part
    end subroutine vc_svn_dir_extra_headers

    ! ------------------------------------------------------------------
    ! Setup
    ! ------------------------------------------------------------------

    !> Make the SVN backend known to VC.  Not a port.
    subroutine init_vc_svn()
        integer :: k

        if (.not. allocated(vc_svn_program)) vc_svn_program = "svn"

        if (.not. allocated(vc_svn_global_switches%list)) then
            vc_svn_global_switches%kind = VC_SWITCHES_LIST
            allocate (vc_svn_global_switches%list(0))
            call append_vc_string(vc_svn_global_switches%list, "--non-interactive")
        end if

        call define_log_view_mode()
        call register_vc_backend("SVN", k)
        if (k == 0) return

        associate (b => vc_backends(k))
            b%registered => vc_svn_registered
            b%state => vc_svn_state
            b%working_revision => vc_svn_working_revision
            ! vc-svn-mode-line-string doesn't exist because the default
            ! implementation works just fine.
            b%root => vc_svn_root
            ! (defalias 'vc-svn-responsible-p #'vc-svn-root)
            b%responsible_p => vc_svn_root
            b%checkout_model => vc_svn_checkout_model
            b%create_repo => vc_svn_create_repo
            b%register => vc_svn_register
            b%register_switches => vc_svn_register_switches
            b%checkin => vc_svn_checkin
            b%find_revision => vc_svn_find_revision
            b%checkout => vc_svn_checkout
            b%delete_file => vc_svn_delete_file
            b%rename_file => vc_svn_rename_file
            b%revert => vc_svn_revert
            b%previous_revision => vc_svn_previous_revision
            b%revision_completion_table => vc_svn_revision_table
            b%diff => vc_svn_diff
            b%diff_switches => vc_svn_diff_switches
            b%print_log => vc_svn_print_log
            b%log_view_mode => vc_svn_log_view_mode
            b%dir_status_files => vc_svn_dir_status_files
            b%dir_extra_headers => vc_svn_dir_extra_headers
        end associate
    end subroutine init_vc_svn
end module vc_svn_mod
