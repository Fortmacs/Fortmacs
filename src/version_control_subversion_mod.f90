! version_control_subversion_mod.f90 -- non-resident support for Subversion version-control.
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

!> The Subversion backend of VERSION_CONTROL: whether a file is registered and its
!> state (from `svn status -v'), its working revision, the root of a
!> working copy, creating a repository (svnadmin create and a checkout),
!> registering, checking out, reverting, deleting and renaming files,
!> diffs, other revisions (svn cat), the previous revision, revision
!> completion, committing, logs (`version_control_subversion_log_view_mode') and the VC-Dir
!> states (`svn status -u') and headers.
!>
!> Deferred: see docs/status/version_control_subversion_mod.md.
module version_control_subversion_mod
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use error_mod, only: status_type, set_error, GENERIC_ERROR_CODE
    use data_mod, only: string_to_number
    use handle_mod, only: handle_type
    use buffer_mod, only: buffers, current, current_buffer, set_buffer, get_or_make_buffer, close_buffer, &
                          generate_new_buffer_name, is_buffer_live, get_file_buffer
    use editing_functions_mod, only: point, goto_character, point_minimum, point_maximum, buffer_substring, line_end_position, &
                                     line_beginning_position, character_after
    use insert_delete_mod, only: insert
    use search_mod, only: regular_expression_search_forward, search_forward, match_beginning, SEARCH_NOERROR
    use basic_subprograms_mod, only: match_string
    use file_input_output_mod, only: expand_file_name, file_name_directory, file_exists, is_file_accessible_directory, &
                                     file_attributes, file_status_type
    use files_mod, only: file_relative_name
    use display_engine_mod, only: message
    use window_mod, only: pop_to_buffer, shrink_window_if_larger_than_buffer
    use version_control_hooks_mod, only: version_control_backends, version_control_switches_type, &
                                         register_version_control_backend, version_control_find_root, &
                                         version_control_backend_named, version_control_working_revision, &
                                         version_control_mode_line, version_control_directory_entry_type, &
                                         version_control_directory_update_procedure, version_control_file_set_state, &
                                         version_control_file_get_state, version_control_file_set_working_revision, &
                                         version_control_file_get_working_revision, version_control_file_set_checkout_time, &
                                         version_control_log_limit_type, VERSION_CONTROL_SWITCHES_NONE, &
                                         VERSION_CONTROL_SWITCHES_UNSPECIFIED, VERSION_CONTROL_SWITCHES_LIST, &
                                         VERSION_CONTROL_LIMIT_NUMBER, VERSION_CONTROL_LIMIT_REVISION, &
                                         VERSION_CONTROL_MODEL_IMPLICIT, VERSION_CONTROL_STATE_UNKNOWN, &
                                         VERSION_CONTROL_UP_TO_DATE, VERSION_CONTROL_EDITED, VERSION_CONTROL_NEEDS_UPDATE, &
                                         VERSION_CONTROL_NEEDS_MERGE, VERSION_CONTROL_ADDED, VERSION_CONTROL_REMOVED, &
                                         VERSION_CONTROL_CONFLICT, VERSION_CONTROL_IGNORED, VERSION_CONTROL_UNREGISTERED
    use version_control_dispatcher_mod, only: version_control_do_command, version_control_setup_buffer, &
                                              version_control_exec_after, version_control_action_type, &
                                              VERSION_CONTROL_OKSTATUS_UNLESS_SIGNAL, VERSION_CONTROL_OKSTATUS_ASYNCHRONOUS, &
                                              VERSION_CONTROL_DESTINATION_CURRENT
    use version_control_mod, only: version_control_switches, version_control_log_view_type, VERSION_CONTROL_LOG_VIEW_WITH_DIFF
    use fortmacs_widget_for_object_collections_mod, only: fortmacs_widget_for_object_collections_part_type
    use hooks_mod, only: hook_type
    use commands_mod, only: commands
    use modes_mod, only: major_modes
    use log_edit_mod, only: log_edit_extract_headers, log_edit_header_type
    use string_type_mod, only: string_type, append_string, prepend_string
    implicit none(type, external)
    private

    public :: initialize_version_control_subversion, version_control_subversion_registered, version_control_subversion_state, &
              version_control_subversion_parse_status, version_control_subversion_working_revision, &
              version_control_subversion_root, version_control_subversion_command, version_control_subversion_previous_revision, &
              version_control_subversion_revision_table, version_control_subversion_log_view_mode

    !> Name of the SUBVERSION executable.  Port of vc-svn.el vc-svn-program.
    character(len=:), allocatable, public :: version_control_subversion_program

    !> Global switches to pass to any SUBVERSION command.
    !> The option "--non-interactive" is often needed to prevent SUBVERSION
    !> hanging while prompting for authorization.  Port of vc-svn.el
    !> vc-svn-global-switches (VERSION_CONTROL_SWITCHES_LIST: the switches; any other
    !> kind: none).
    type(version_control_switches_type), public :: version_control_subversion_global_switches

    !> Switches for registering a file into SUBVERSION.
    !> A string or list of strings passed to the checkin program by
    !> \[version_control_register].  If VERSION_CONTROL_SWITCHES_UNSPECIFIED, use the value of `version_control_register_switches'.
    !> If VERSION_CONTROL_SWITCHES_NONE, use no switches.  Port of vc-svn.el vc-svn-register-switches.
    type(version_control_switches_type), target, public :: version_control_subversion_register_switches

    !> String or list of strings specifying extra switches for svn diff under VERSION_CONTROL.
    !> If VERSION_CONTROL_SWITCHES_UNSPECIFIED, use the value of `version_control_diff_switches' (or `diff_switches'),
    !> together with "-x --diff-cmd="`diff_command' (since `svn diff'
    !> does not support the default "-c" value of `diff_switches').
    !> If you want to force an empty list of arguments, use VERSION_CONTROL_SWITCHES_NONE.  Port of
    !> vc-svn.el vc-svn-diff-switches.
    type(version_control_switches_type), target, public :: version_control_subversion_diff_switches = &
                                                           version_control_switches_type(kind=VERSION_CONTROL_SWITCHES_NONE)

    !> The name of the ".svn" subdirectory or its equivalent.  Port of
    !> vc-svn.el vc-svn-admin-directory.
    character(len=*), parameter, public :: version_control_subversion_admin_directory = ".svn"

    !> Hook run after entering SVN-Log-View mode.
    type(hook_type), target, public :: version_control_subversion_log_view_mode_hook = hook_type( &
                                       name="version_control_subversion_log_view_mode_hook")

    !> The keymap of `version_control_subversion_log_view_mode'.
    type(handle_type), public :: version_control_subversion_log_view_mode_map

    ! A `directory_status_files' computation in progress: its update function
    ! (vc-svn-after-dir-status's CALLBACK) and VC-Dir buffer.
    type :: directory_status_state_type
        logical :: live = .false.
        procedure(version_control_directory_update_procedure), pointer, nopass :: update_function => null()
        type(handle_type) :: buffer
    end type directory_status_state_type

    ! The computations of `version_control_subversion_directory_status_files' in progress.
    type(directory_status_state_type), allocatable :: directory_states(:)

    character, parameter :: LF = achar(10)

contains

    ! ------------------------------------------------------------------
    ! Internal functions
    ! ------------------------------------------------------------------

    !> A wrapper around `version_control_do_command' for use in version_control_subversion_mod.
    !> The difference to `version_control_do_command' is that this function always invokes `subversion',
    !> and that it passes `version_control_subversion_global_switches' to it before FLAGS.
    !> BUFFER is the output buffer (*vc* if absent and CURRENT false; the
    !> current buffer with CURRENT).  Port of vc-svn.el:vc-svn-command.
    recursive subroutine version_control_subversion_command(okstatus, files, flags, status, buffer, current, exit_status, process)
        integer, intent(in) :: okstatus
        type(string_type), intent(in) :: files(:), flags(:)
        type(status_type), intent(out) :: status
        type(handle_type), intent(in), optional :: buffer
        logical, intent(in), optional :: current
        integer, intent(out), optional :: exit_status
        type(handle_type), intent(out), optional :: process
        type(string_type), allocatable :: all(:)
        type(handle_type) :: buffer_handle
        logical :: in_current

        status = status_type()
        allocate (all(0))

        if (version_control_subversion_global_switches%kind == VERSION_CONTROL_SWITCHES_LIST) then
            if (allocated(version_control_subversion_global_switches%list)) all = version_control_subversion_global_switches%list
        end if

        call append_all(all, flags)
        in_current = .false.
        if (present(current)) in_current = current

        if (in_current) then
            call version_control_do_command(VERSION_CONTROL_DESTINATION_CURRENT, okstatus, version_control_subversion_program, &
                                            files, all, status, exit_status=exit_status, &
                                            process=process)
        else
            if (present(buffer)) then
                buffer_handle = buffer
            else
                call get_or_make_buffer("*vc*", buffer_handle, status)
                if (.not. status%ok()) return
            end if

            call version_control_do_command(VERSION_CONTROL_DESTINATION_CURRENT, okstatus, version_control_subversion_program, &
                                            files, all, status, buffer=buffer_handle, &
                                            exit_status=exit_status, process=process)
        end if
    end subroutine version_control_subversion_command

    !> Append the strings MORE to LIST.  Not a port.
    subroutine append_all(list, more)
        type(string_type), allocatable, intent(inout) :: list(:)
        type(string_type), intent(in) :: more(:)
        type(string_type), allocatable :: grown(:)
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
        type(string_type), allocatable :: list(:)

        call append_string(list, a1, a2, a3, a4)
    end function strings

    !> The current buffer's `default_directory'.  Not a port.
    function buffer_directory() result(directory)
        character(len=:), allocatable :: directory

        directory = "./"
        if (current == 0) return
        if (allocated(buffers(current)%directory)) directory = buffers(current)%directory
    end function buffer_directory

    !> Make a temporary buffer current (with-temp-buffer), with DIRECTORY as its
    !> `default_directory'; OLD is the buffer to go back to.  Not a port.
    subroutine temporary_begin(old, temporary, directory)
        type(handle_type), intent(out) :: old, temporary
        character(len=*), intent(in) :: directory
        type(status_type) :: st

        old = current_buffer()
        call get_or_make_buffer(generate_new_buffer_name(" *temp*"), temporary, st)
        call set_buffer(temporary, st)
        buffers(current)%directory = directory
    end subroutine temporary_begin

    !> Kill the temporary buffer TEMPORARY and make OLD current again.  Not a port.
    subroutine temporary_end(old, temporary)
        type(handle_type), intent(in) :: old, temporary
        type(status_type) :: st

        call close_buffer(temporary, st)
        call set_buffer(old, st)
    end subroutine temporary_end

    !> The index of the SUBVERSION backend.  Not a port.
    integer function subversion_backend_index() result(b)
        b = version_control_backend_named("SVN")
    end function subversion_backend_index

    ! ------------------------------------------------------------------
    ! State-querying functions
    ! ------------------------------------------------------------------

    !> Check if FILE is SUBVERSION registered.  Port of vc-svn.el:vc-svn-registered.
    logical function version_control_subversion_registered(file) result(registered)
        character(len=*), intent(in) :: file
        character(len=:), allocatable :: abs, root, directory
        type(handle_type) :: old, temporary
        type(status_type) :: st
        logical :: found
        integer :: code, parsed

        registered = .false.
        call expand_file_name(file, abs, st)
        if (.not. st%ok()) return
        call version_control_subversion_root(abs, root, found)
        if (.not. found) return
        directory = file_name_directory(abs)
        if (.not. is_file_accessible_directory(directory)) return
        call temporary_begin(old, temporary, directory)
        ! Ignore all errors.
        call version_control_subversion_command(VERSION_CONTROL_OKSTATUS_UNLESS_SIGNAL, strings(abs), strings("status", "-v"), st, &
                                                current=.true., &
                                                exit_status=code)

        ! Some problem happened.  E.g. We can't find an `subversion' executable.
        if (st%ok() .and. code == 0) then
            parsed = version_control_subversion_parse_status(abs)
            registered = parsed /= VERSION_CONTROL_STATE_UNKNOWN .and. parsed /= VERSION_CONTROL_IGNORED .and. parsed &
                         /= VERSION_CONTROL_UNREGISTERED
        end if

        call temporary_end(old, temporary)
    end function version_control_subversion_registered

    !> SVN-specific version of `version_control_state'.  Port of vc-svn.el:vc-svn-state.
    integer function version_control_subversion_state(file) result(state)
        character(len=*), intent(in) :: file
        type(handle_type) :: old, temporary
        type(status_type) :: st
        character(len=:), allocatable :: directory

        directory = file_name_directory(file)
        if (len(directory) == 0) directory = buffer_directory()
        call temporary_begin(old, temporary, directory)
        call version_control_subversion_command(0, strings(file), strings("status", "-v"), st, current=.true.)
        state = version_control_subversion_parse_status(file)
        call temporary_end(old, temporary)
    end function version_control_subversion_state

    !> Parse output of "svn status" command in the current buffer.
    !> Set file properties accordingly.  If FILENAME is present, return its
    !> status (VERSION_CONTROL_STATE_UNKNOWN otherwise).  Port of
    !> vc-svn.el:vc-svn-parse-status (Svn's own status, the
    !> `vc-svn-status' property, is not kept: only the deferred
    !> `vc-svn-find-file-hook' reads it).
    integer function version_control_subversion_parse_status(filename) result(state)
        character(len=*), intent(in), optional :: filename
        character(len=:), allocatable :: file, rest
        type(file_status_type) :: attribute
        type(status_type) :: st
        integer(int64) :: found
        integer(int32) :: status, propstat, flag
        logical :: multifile, has, has_attribute

        state = VERSION_CONTROL_STATE_UNKNOWN
        multifile = .false.
        call goto_character(point_minimum())

        do
            call regular_expression_search_forward("^\(?:\?\|[ ACDGIMR!~][ MC][ L][ +][ SX]..\([ *]\) +\([-0-9]+\) +\([0-9?]+\) " &
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
            call character_after(status, has, line_beginning_position())
            ! Status of the item's properties ([ MC]).
            call character_after(propstat, has, line_beginning_position() + 1)

            if (status == iachar("?")) then
                call version_control_file_set_state(file, VERSION_CONTROL_UNREGISTERED)
                cycle
            end if

            ! Use the last-modified revision, so that searching in vc-print-log
            ! output works.
            call version_control_file_set_working_revision(file, match_string(3))
            call character_after(flag, has, match_beginning(1, st))

            if (status == iachar(" ") .and. propstat == iachar(" ")) then
                if (flag == iachar("*")) then
                    call version_control_file_set_state(file, VERSION_CONTROL_NEEDS_UPDATE)
                else
                    call file_attributes(file, attribute, has_attribute)

                    if (has_attribute) then
                        call version_control_file_set_checkout_time(file, attribute%mtime_second, attribute%mtime_nsec)
                    else
                        call version_control_file_set_checkout_time(file)
                    end if

                    call version_control_file_set_state(file, VERSION_CONTROL_UP_TO_DATE)
                end if
            else if (status == iachar("A")) then
                ! If the file was actually copied, (match-string 2) is "-".
                call version_control_file_set_working_revision(file, "0")
                call version_control_file_set_checkout_time(file, 0_int64, 0_int32)
                call version_control_file_set_state(file, VERSION_CONTROL_ADDED)
            else if (status == iachar("C") .or. propstat == iachar("C")) then
                ! Conflict in contents or properties.
                call version_control_file_set_state(file, VERSION_CONTROL_CONFLICT)
            else if (status == iachar("M") .or. propstat == iachar("M")) then
                ! Modified contents or properties.
                if (flag == iachar("*")) then
                    call version_control_file_set_state(file, VERSION_CONTROL_NEEDS_MERGE)
                else
                    call version_control_file_set_state(file, VERSION_CONTROL_EDITED)
                end if
            else if (status == iachar("I")) then
                call version_control_file_set_state(file, VERSION_CONTROL_IGNORED)
            else if (status == iachar("D") .or. status == iachar("R")) then
                call version_control_file_set_state(file, VERSION_CONTROL_REMOVED)
            else
                call version_control_file_set_state(file, VERSION_CONTROL_EDITED)
            end if
        end do

        if (present(filename)) state = version_control_file_get_state(filename)
    end function version_control_subversion_parse_status

    !> SVN-specific version of `version_control_working_revision'.  Port of
    !> vc-svn.el:vc-svn-working-revision.
    subroutine version_control_subversion_working_revision(file, revision, found)
        character(len=*), intent(in) :: file
        character(len=:), allocatable, intent(out) :: revision
        logical, intent(out) :: found
        character(len=:), allocatable :: abs
        type(status_type) :: st
        logical :: registered

        ! There is no need to consult RCS headers under SUBVERSION, because we
        ! get the workfile version for free when we recognize that a file
        ! is registered in SUBVERSION.
        registered = version_control_subversion_registered(file)
        call expand_file_name(file, abs, st)
        if (.not. st%ok()) abs = file
        call version_control_file_get_working_revision(abs, revision, found)
    end subroutine version_control_subversion_working_revision

    !> The revision before REVISION ("" and FOUND false for none).  Port of
    !> vc-svn.el:vc-svn-previous-revision.
    subroutine version_control_subversion_previous_revision(file, revision, previous, found)
        character(len=*), intent(in) :: file, revision
        character(len=:), allocatable, intent(out) :: previous
        logical, intent(out) :: found
        integer(int64) :: newrev
        character(len=20) :: digits

        previous = ""
        if (len(file) < 0) previous = ""
        call string_to_number(revision, newrev)
        newrev = newrev - 1
        found = 0 < newrev

        if (found) then
            write (digits, "(i0)") newrev
            previous = trim(digits)
        end if
    end subroutine version_control_subversion_previous_revision

    !> The revisions of FILES, from `svn log -q'.  Port of
    !> vc-svn.el:vc-svn-revision-table (as vc-svn-revision-completion-table).
    subroutine version_control_subversion_revision_table(files, table)
        type(string_type), intent(in) :: files(:)
        type(string_type), allocatable, intent(out) :: table(:)
        type(handle_type) :: old, version_control
        type(status_type) :: st
        integer(int64) :: found

        allocate (table(0))
        old = current_buffer()
        call get_or_make_buffer("*vc*", version_control, st)
        if (.not. st%ok()) return
        call set_buffer(version_control, st)
        call version_control_subversion_command(0, files, strings("log", "-q"), st)
        call set_buffer(version_control, st)
        call goto_character(point_minimum())

        do
            call regular_expression_search_forward("^r\([0-9]+\) ", found, st, noerror=SEARCH_NOERROR)
            if (found == 0) exit
            ! (push (match-string 1 loglines) vc-svn-revisions)
            call prepend_string(table, match_string(1))
        end do

        call set_buffer(old, st)
    end subroutine version_control_subversion_revision_table

    !> The root of FILE's working copy (the directory with .svn).  Port of
    !> vc-svn.el:vc-svn-root.
    subroutine version_control_subversion_root(file, root, found)
        character(len=*), intent(in) :: file
        character(len=:), allocatable, intent(out) :: root
        logical, intent(out) :: found

        call version_control_find_root(file, version_control_subversion_admin_directory, root, found)
    end subroutine version_control_subversion_root

    !> SUBVERSION's checkout model: `implicit'.  Port of
    !> vc-svn.el:vc-svn-checkout-model.
    integer function version_control_subversion_checkout_model(files) result(model)
        type(string_type), intent(in) :: files(:)

        model = VERSION_CONTROL_MODEL_IMPLICIT
        if (size(files) < 0) model = 0
    end function version_control_subversion_checkout_model

    ! ------------------------------------------------------------------
    ! State-changing functions
    ! ------------------------------------------------------------------

    !> Create a new SUBVERSION repository.  Port of vc-svn.el:vc-svn-create-repo.
    recursive subroutine version_control_subversion_create_repository(status)
        type(status_type), intent(out) :: status
        type(string_type) :: none(0)
        type(handle_type) :: version_control
        character(len=:), allocatable :: default_directory, url

        call get_or_make_buffer("*vc*", version_control, status)
        if (.not. status%ok()) return
        call version_control_do_command(VERSION_CONTROL_DESTINATION_CURRENT, 0, "svnadmin", none, strings("create", "SVN"), &
                                        status, buffer=version_control)
        if (.not. status%ok()) return
        ! Expand default-directory because svn gets confused by eg
        ! file://~/path/to/file.  (Bug#15446).
        call expand_file_name(buffer_directory(), default_directory, status)
        if (.not. status%ok()) return
        ! MS-Windows files d:/foo/bar need to begin with 3 leading slashes.
        url = "file:///"

        if (len(default_directory) > 0) then
            if (default_directory(1:1) == "/") url = "file://"
        end if

        url = url//default_directory//"SVN"
        call version_control_subversion_command(0, strings("."), strings("checkout", url), status, buffer=version_control)
    end subroutine version_control_subversion_create_repository

    !> Register FILES into the SUBVERSION version control system.
    !> The COMMENT argument is ignored  This does an add but not a commit.
    !> Passes either `version_control_subversion_register_switches' or `version_control_register_switches'
    !> to the SUBVERSION command.  Port of vc-svn.el:vc-svn-register.
    recursive subroutine version_control_subversion_register(files, status, comment)
        type(string_type), intent(in) :: files(:)
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: comment
        type(string_type), allocatable :: flags(:), sw(:)

        if (present(comment)) continue
        flags = strings("add")
        call version_control_switches(subversion_backend_index(), "register", sw)
        call append_all(flags, sw)
        call version_control_subversion_command(0, files, flags, status)
    end subroutine version_control_subversion_register

    !> SVN-specific version of `version_control_backend_checkin'.  Port of
    !> vc-svn.el:vc-svn-checkin.
    recursive subroutine version_control_subversion_checkin(files, comment, status, revision)
        use help_mod, only: substitute_command_keys
        type(string_type), intent(in) :: files(:)
        character(len=*), intent(in) :: comment
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: revision
        type(log_edit_header_type) :: no_headers(0)
        type(string_type), allocatable :: extracted(:)
        type(string_type), allocatable :: flags(:), sw(:)
        character(len=:), allocatable :: message
        type(handle_type) :: old, version_control
        type(status_type) :: st
        integer(int64) :: found
        integer :: code, k

        if (present(revision)) continue
        call log_edit_extract_headers(no_headers, comment, message, extracted, status)
        if (.not. status%ok()) return
        flags = strings("ci", "-m", message)

        do k = 1, size(extracted)
            call append_string(flags, extracted(k)%text)
        end do

        call version_control_switches(subversion_backend_index(), "checkin", sw)
        call append_all(flags, sw)
        old = current_buffer()
        call version_control_subversion_command(1, files, flags, status, exit_status=code)
        if (.not. status%ok()) return
        call get_or_make_buffer("*vc*", version_control, status)
        if (.not. status%ok()) return
        call set_buffer(version_control, st)
        call goto_character(point_minimum())

        if (code /= 0) then
            ! Check checkin problem.
            call search_forward("Transaction is out of date", found, st, noerror=SEARCH_NOERROR)

            if (found /= 0) then
                do k = 1, size(files)
                    call version_control_file_set_state(files(k)%text, VERSION_CONTROL_NEEDS_MERGE)
                end do

                call set_error(status, GENERIC_ERROR_CODE, &
                               substitute_command_keys( &
                               "Up-to-date check failed: type \[version_control_next_action] to merge in changes"))
            else
                call pop_to_buffer(current_buffer(), st)
                call goto_character(point_minimum())
                call shrink_window_if_larger_than_buffer()
                call set_error(status, GENERIC_ERROR_CODE, "Check-in failed")
            end if

            return
        end if

        call set_buffer(old, st)
    end subroutine version_control_subversion_checkin

    !> SVN-specific retrieval of a specified version into a buffer.  Port of
    !> vc-svn.el:vc-svn-find-revision.
    recursive subroutine version_control_subversion_find_revision(file, revision, buffer, status)
        character(len=*), intent(in) :: file, revision
        type(handle_type), intent(in) :: buffer
        type(status_type), intent(out) :: status
        type(string_type), allocatable :: flags(:), sw(:)
        type(string_type) :: empty

        flags = strings("cat")

        if (len(revision) > 0) then
            call append_string(flags, "-r"//revision)
        else
            flags = [flags, empty]
        end if

        call version_control_switches(subversion_backend_index(), "checkout", sw)
        call append_all(flags, sw)
        call version_control_subversion_command(0, strings(file), flags, status, buffer=buffer)
    end subroutine version_control_subversion_find_revision

    !> Check out REVISION of FILE (the base revision if absent; the head if "").
    !> Port of vc-svn.el:vc-svn-checkout.
    recursive subroutine version_control_subversion_checkout(file, status, revision)
        character(len=*), intent(in) :: file
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: revision
        type(string_type), allocatable :: sw(:)
        type(handle_type) :: old, buffer
        type(status_type) :: st

        call message("Checking out "//file//"...")
        old = current_buffer()
        buffer = get_file_buffer(file)
        if (is_buffer_live(buffer)) call set_buffer(buffer, st)
        call version_control_switches(subversion_backend_index(), "checkout", sw)
        call version_control_subversion_update(file, sw, status, revision)
        call set_buffer(old, st)
        if (.not. status%ok()) return
        call version_control_mode_line(file, subversion_backend_index())
        call message("Checking out "//file//"...done")
    end subroutine version_control_subversion_checkout

    !> Update FILE to REVISION with SWITCHES (nothing to do for an existing file
    !> when REVISION is absent).  Port of vc-svn.el:vc-svn-update.
    recursive subroutine version_control_subversion_update(file, switches, status, revision)
        character(len=*), intent(in) :: file
        type(string_type), intent(in) :: switches(:)
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: revision
        type(string_type), allocatable :: flags(:)
        type(string_type) :: empty

        status = status_type()
        ! If no revision was specified, there's nothing to do.
        if (file_exists(file) .and. .not. present(revision)) return
        ! Check out a particular version (or recreate the file).
        call version_control_file_set_working_revision(file)
        flags = strings("update")

        if (.not. present(revision)) then
            call append_string(flags, "-rBASE")
        else if (len(revision) == 0) then
            flags = [flags, empty]
        else
            call append_string(flags, "-r"//revision)
        end if

        call append_all(flags, switches)
        call version_control_subversion_command(0, strings(file), flags, status)
    end subroutine version_control_subversion_update

    !> Delete FILE with `svn remove'.  Port of vc-svn.el:vc-svn-delete-file.
    recursive subroutine version_control_subversion_delete_file(file, status)
        character(len=*), intent(in) :: file
        type(status_type), intent(out) :: status

        call version_control_subversion_command(0, strings(file), strings("remove"), status)
    end subroutine version_control_subversion_delete_file

    !> Rename OLD to NEW with `svn move'.  Port of vc-svn.el:vc-svn-rename-file.
    recursive subroutine version_control_subversion_rename_file(old, new, status)
        character(len=*), intent(in) :: old, new
        type(status_type), intent(out) :: status

        call version_control_subversion_command(0, strings(new), strings("move", file_relative_name(old)), status)
    end subroutine version_control_subversion_rename_file

    !> Revert FILE to the version it was based on.  Port of
    !> vc-svn.el:vc-svn-revert.
    recursive subroutine version_control_subversion_revert(file, status, contents_done)
        character(len=*), intent(in) :: file
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: contents_done

        status = status_type()
        if (.not. present(contents_done)) call version_control_subversion_command(0, strings(file), strings("revert"), status)
    end subroutine version_control_subversion_revert

    ! ------------------------------------------------------------------
    ! History functions
    ! ------------------------------------------------------------------

    !> Major mode for SUBVERSION logs.  Port of vc-svn.el:vc-svn-log-view-mode.
    recursive subroutine version_control_subversion_log_view_mode(status)
        use log_view_mod, only: log_view_per_file_logs
        type(status_type), intent(out) :: status
        logical :: saved

        if (.not. associated(major_modes%version_control_subversion_log_view_mode%execute)) call define_log_view_mode()
        call major_modes%version_control_subversion_log_view_mode%begin(saved, status)
        if (.not. status%ok()) return
        call log_view_per_file_logs%set_local(.false.)
        call major_modes%version_control_subversion_log_view_mode%end(saved, status)
    end subroutine version_control_subversion_log_view_mode

    ! Define SUBVERSION's Log View mode.  Not a port.
    subroutine define_log_view_mode()
        use log_view_mod, only: initialize_log_view
        use add_log_mod, only: initialize_add_log

        ! (require 'add-log)
        if (.not. associated(major_modes%change_log_mode%execute)) call initialize_add_log()
        if (.not. associated(major_modes%log_view_mode%execute)) call initialize_log_view()
        call major_modes%version_control_subversion_log_view_mode%define("version_control_subversion_log_view_mode", &
                                                                         "SVN-Log-View", version_control_subversion_log_view_mode, &
                                                                         version_control_subversion_log_view_mode_hook, &
                                                                         parent=major_modes%log_view_mode)
        version_control_subversion_log_view_mode_map = major_modes%version_control_subversion_log_view_mode%map
        commands%version_control_subversion_log_view_mode%execute => version_control_subversion_log_view_mode
    end subroutine define_log_view_mode

    !> Print commit log associated with FILES into specified BUFFER.
    !> SHORTLOG is ignored.
    !> If START_REVISION is not "", it is the newest revision to show.
    !> If LIMIT is given, show no more than this many entries.  Port of
    !> vc-svn.el:vc-svn-print-log.
    recursive subroutine version_control_subversion_print_log(files, buffer, shortlog, start_revision, limit, status, &
                                                              limit_unsupported)
        type(string_type), intent(in) :: files(:)
        type(handle_type), intent(in) :: buffer
        logical, intent(in) :: shortlog
        character(len=*), intent(in) :: start_revision
        type(version_control_log_limit_type), intent(in) :: limit
        type(status_type), intent(out) :: status
        logical, intent(out) :: limit_unsupported
        type(string_type), allocatable :: flags(:)
        type(string_type) :: none(0)
        type(handle_type) :: old
        type(status_type) :: st
        logical :: saved_read_only
        integer :: k

        limit_unsupported = .false.
        if (shortlog) continue
        old = current_buffer()
        call version_control_setup_buffer(buffer, status)
        if (.not. status%ok()) return
        ! Writable meanwhile, as upstream binds inhibit-read-only.
        saved_read_only = buffers(current)%read_only
        buffers(current)%read_only = .false.
        call goto_character(point_minimum())

        if (size(files) > 0) then
            do k = 1, size(files)
                call insert("Working file: "//files(k)%text//LF, status)
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
                call version_control_subversion_command(VERSION_CONTROL_OKSTATUS_ASYNCHRONOUS, files(k:k), flags, status, &
                                                        buffer=buffer)
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
            call version_control_subversion_command(0, none, flags, status, buffer=buffer)
        end if

        buffers(current)%read_only = saved_read_only
        call set_buffer(old, st)

    contains

        ! The --diff and --limit options.
        subroutine append_options(list)
            type(string_type), allocatable, intent(inout) :: list(:)
            character(len=20) :: digits

            if (version_control_log_view_type() == VERSION_CONTROL_LOG_VIEW_WITH_DIFF) call append_string(list, "--diff")

            if (limit%kind == VERSION_CONTROL_LIMIT_NUMBER) then
                call append_string(list, "--limit")
                write (digits, "(i0)") limit%n
                call append_string(list, trim(digits))
            else if (limit%kind == VERSION_CONTROL_LIMIT_REVISION) then
                call append_string(list, "--limit")
                call append_string(list, limit%revision)
            end if
        end subroutine append_options
    end subroutine version_control_subversion_print_log

    !> Get a difference report using SUBVERSION between two revisions of fileset FILES.
    !> Port of vc-svn.el:vc-svn-diff.
    recursive subroutine version_control_subversion_diff(files, buffer, asynchronous, status, rev1, rev2)
        use diff_mod, only: diff_command
        type(string_type), intent(in) :: files(:)
        type(handle_type), intent(in) :: buffer
        logical, intent(in) :: asynchronous
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: rev1, rev2
        type(string_type), allocatable :: flags(:), sw(:)
        character(len=:), allocatable :: oldvers, revision, joined
        logical :: has_old, use_asynchronous, same, found
        integer :: k

        has_old = present(rev1)
        if (has_old) oldvers = rev1

        if (has_old .and. .not. present(rev2) .and. size(files) > 0) then
            same = .true.

            do k = 1, size(files)
                call version_control_working_revision(files(k)%text, revision, found)

                if (.not. found) then
                    same = .false.
                else if (revision /= oldvers) then
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
        use_asynchronous = asynchronous .and. (has_old .or. present(rev2))
        flags = strings("diff")

        if (version_control_subversion_diff_switches%kind /= VERSION_CONTROL_SWITCHES_UNSPECIFIED) then
            call version_control_switches(subversion_backend_index(), "diff", sw)
            call append_all(flags, sw)
        else
            call version_control_switches(0, "diff", sw)
            joined = ""

            do k = 1, size(sw)
                if (k > 1) joined = joined//" "
                joined = joined//sw(k)%text
            end do

            if (.not. allocated(diff_command)) diff_command = "diff"
            call append_string(flags, "--diff-cmd="//diff_command)
            call append_string(flags, "-x")
            call append_string(flags, joined)
        end if

        if (has_old) then
            call append_string(flags, "-r")

            if (present(rev2)) then
                call append_string(flags, oldvers//":"//rev2)
            else
                call append_string(flags, oldvers)
            end if
        end if

        ! For some reason `svn diff' does not return a useful status
        ! w.r.t whether the diff was empty or not: `version_control_diff_internal' looks
        ! at the size of BUFFER.
        if (use_asynchronous) then
            call version_control_subversion_command(VERSION_CONTROL_OKSTATUS_ASYNCHRONOUS, files, flags, status, buffer=buffer)
        else
            call version_control_subversion_command(0, files, flags, status, buffer=buffer)
        end if
    end subroutine version_control_subversion_diff

    ! ------------------------------------------------------------------
    ! VC-Dir
    ! ------------------------------------------------------------------

    !> Parse the output of `svn status' (REMOTE: `svn status -u') in the
    !> current buffer and pass the states found to the update function of
    !> the computation of ACTION.  Port of vc-svn.el:vc-svn-after-dir-status.
    recursive subroutine version_control_subversion_after_directory_status(action, status)
        type(version_control_action_type), intent(in) :: action
        type(status_type), intent(out) :: status
        type(version_control_directory_entry_type), allocatable :: result(:), one(:)
        character(len=:), allocatable :: regular_expression, flag
        type(handle_type) :: old
        type(status_type) :: st
        integer(int64) :: found
        integer :: state, propstat, k

        status = status_type()
        k = action%n

        if (.not. is_buffer_live(action%buffer)) then
            directory_states(k)%live = .false.
            return
        end if

        old = current_buffer()
        call set_buffer(action%buffer, st)

        if (action%flag) then
            regular_expression = "^\(.\)\(.\).....? \([ *]\) +\(?:[-0-9]+\)?   \(.*\)$"
        else
            ! Subexp 3 is a dummy in this case, so the numbers match.
            regular_expression = "^\(.\)\(.\)...\(.\).? \(.*\)$"
        end if

        allocate (result(0), one(1))
        call goto_character(point_minimum())

        do
            call regular_expression_search_forward(regular_expression, found, st, noerror=SEARCH_NOERROR)
            if (found == 0) exit
            state = state_of(match_string(1))
            propstat = state_of(match_string(2))
            flag = match_string(3)
            one(1)%name = match_string(4)
            ! conflict always wins
            if ((propstat == VERSION_CONTROL_CONFLICT .or. propstat == VERSION_CONTROL_EDITED) .and. state &
                /= VERSION_CONTROL_CONFLICT) state = propstat

            if (action%flag .and. flag == "*") then
                ! FIXME are there other possible combinations?
                if (state == VERSION_CONTROL_EDITED) then
                    state = VERSION_CONTROL_NEEDS_MERGE
                else if (state == VERSION_CONTROL_STATE_UNKNOWN) then
                    state = VERSION_CONTROL_NEEDS_UPDATE
                end if
            end if

            if (state /= VERSION_CONTROL_STATE_UNKNOWN) then
                one(1)%state = state
                result = [one, result]
            end if
        end do

        call set_buffer(old, st)
        directory_states(k)%live = .false.
        call directory_states(k)%update_function(result, directory_states(k)%buffer, .false.)

    contains

        ! The state of the status character C (state-map).
        integer function state_of(c) result(s)
            character(len=*), intent(in) :: c

            select case (c)
            case ("A")
                s = VERSION_CONTROL_ADDED
            case ("C")
                s = VERSION_CONTROL_CONFLICT
            case ("I")
                s = VERSION_CONTROL_IGNORED
            case ("M")
                s = VERSION_CONTROL_EDITED
            case ("D", "R")
                s = VERSION_CONTROL_REMOVED
            case ("!")
                s = VERSION_CONTROL_NEEDS_UPDATE
            case ("?")
                s = VERSION_CONTROL_UNREGISTERED
            case ("~")
                ! This is what vc-svn-parse-status does.
                s = VERSION_CONTROL_EDITED
            case default
                s = VERSION_CONTROL_STATE_UNKNOWN
            end select
        end function state_of
    end subroutine version_control_subversion_after_directory_status

    !> Run "svn status" for DIRECTORY and update BUFFER via UPDATE.
    !> UPDATE is called with the entries (FILE, STATE) for directory DIRECTORY.
    !> Port of vc-svn.el:vc-svn-dir-status-files.
    recursive subroutine version_control_subversion_directory_status_files(directory, files, update, buffer, status)
        character(len=*), intent(in) :: directory
        type(string_type), intent(in) :: files(:)
        procedure(version_control_directory_update_procedure) :: update
        type(handle_type), intent(in) :: buffer
        type(status_type), intent(out) :: status
        type(directory_status_state_type) :: state
        type(string_type), allocatable :: flags(:)
        type(string_type) :: none(0)
        type(version_control_action_type) :: action
        integer :: k

        if (len(directory) < 0) return
        if (.not. allocated(directory_states)) allocate (directory_states(0))

        do k = 1, size(directory_states)
            if (.not. directory_states(k)%live) exit
        end do

        if (k > size(directory_states)) directory_states = [directory_states, state]
        directory_states(k)%live = .true.
        directory_states(k)%update_function => update
        directory_states(k)%buffer = buffer
        ! FIXME shouldn't this rather default to all the files in dir?
        flags = strings("status", "-u")
        call append_all(flags, files)
        call version_control_subversion_command(VERSION_CONTROL_OKSTATUS_ASYNCHRONOUS, none, flags, status, current=.true.)

        if (.not. status%ok()) then
            directory_states(k)%live = .false.
            return
        end if

        ! (vc-run-delayed (vc-svn-after-dir-status callback t))
        action%execute => version_control_subversion_after_directory_status
        action%n = k
        action%flag = .true.
        action%buffer = current_buffer()
        call version_control_exec_after(action, status)
    end subroutine version_control_subversion_directory_status_files

    !> Generate extra status headers for a Subversion working copy.  Port of
    !> vc-svn.el:vc-svn-dir-extra-headers.
    recursive subroutine version_control_subversion_directory_extra_headers(directory, parts)
        type(fortmacs_widget_for_object_collections_part_type), allocatable, intent(out) :: parts(:)
        character(len=*), intent(in) :: directory
        type(string_type) :: none(0)
        type(handle_type) :: old, version_control
        type(status_type) :: st
        character(len=:), allocatable :: repository
        integer(int64) :: found

        allocate (parts(0))
        if (len(directory) < 0) return
        call version_control_subversion_command(0, none, strings("info"), st)
        old = current_buffer()
        call get_or_make_buffer("*vc*", version_control, st)
        call set_buffer(version_control, st)
        call goto_character(point_minimum())
        call regular_expression_search_forward("Repository Root: *\(.*\)", found, st, noerror=SEARCH_NOERROR)
        if (found /= 0) repository = match_string(1)
        call set_buffer(old, st)

        if (allocated(repository)) then
            call append_part("Repository : ", "version_control_directory_header")
            call append_part(repository, "version_control_directory_header_value")
        end if

    contains

        ! Append TEXT with FACE to PARTS.
        subroutine append_part(text, face)
            character(len=*), intent(in) :: text, face
            type(fortmacs_widget_for_object_collections_part_type) :: part

            part%text = text
            part%face = face
            parts = [parts, part]
        end subroutine append_part
    end subroutine version_control_subversion_directory_extra_headers

    ! ------------------------------------------------------------------
    ! Setup
    ! ------------------------------------------------------------------

    !> Make the SUBVERSION backend known to VERSION_CONTROL.  Not a port.
    subroutine initialize_version_control_subversion()
        integer :: k

        if (.not. allocated(version_control_subversion_program)) version_control_subversion_program = "svn"

        if (.not. allocated(version_control_subversion_global_switches%list)) then
            version_control_subversion_global_switches%kind = VERSION_CONTROL_SWITCHES_LIST
            call append_string(version_control_subversion_global_switches%list, "--non-interactive")
        end if

        call define_log_view_mode()
        call register_version_control_backend("SVN", k)
        if (k == 0) return

        associate (b => version_control_backends(k))
            b%registered => version_control_subversion_registered
            b%state => version_control_subversion_state
            b%working_revision => version_control_subversion_working_revision
            ! vc-svn-mode-line-string doesn't exist because the default
            ! implementation works just fine.
            b%root => version_control_subversion_root
            ! (defalias 'vc-svn-responsible-p #'vc-svn-root)
            b%responsible_p => version_control_subversion_root
            b%checkout_model => version_control_subversion_checkout_model
            b%create_repository => version_control_subversion_create_repository
            b%register => version_control_subversion_register
            b%register_switches => version_control_subversion_register_switches
            b%checkin => version_control_subversion_checkin
            b%find_revision => version_control_subversion_find_revision
            b%checkout => version_control_subversion_checkout
            b%delete_file => version_control_subversion_delete_file
            b%rename_file => version_control_subversion_rename_file
            b%revert => version_control_subversion_revert
            b%previous_revision => version_control_subversion_previous_revision
            b%revision_completion_table => version_control_subversion_revision_table
            b%diff => version_control_subversion_diff
            b%diff_switches => version_control_subversion_diff_switches
            b%print_log => version_control_subversion_print_log
            b%log_view_mode => version_control_subversion_log_view_mode
            b%directory_status_files => version_control_subversion_directory_status_files
            b%directory_extra_headers => version_control_subversion_directory_extra_headers
        end associate
    end subroutine initialize_version_control_subversion
end module version_control_subversion_mod
