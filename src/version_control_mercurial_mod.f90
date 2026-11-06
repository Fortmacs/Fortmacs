! version_control_mercurial_mod.f90 -- VERSION_CONTROL backend for the mercurial version control system.
! Ported from GNU Emacs 31.1 lisp/vc/vc-hg.el.
!
! Copyright (C) 2006-2026 Free Software Foundation, Inc.
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

!> The Mercurial backend of VERSION_CONTROL: whether a file is registered, its state
!> (from `hg status'), working revision and symbolic revision for the
!> mode line, registering, unregistering, reverting, checking out,
!> deleting and renaming files, diffs, other revisions (`hg cat'), the
!> previous and next revisions, revision completion, the ignore file,
!> creating a repository, committing with its Log Edit mode, the logs
!> and their Log View mode, and the VC-Dir status, headers and printer.
!> The Deferred list is in docs/status/version_control_mercurial_mod.md.
module version_control_mercurial_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use error_mod, only: status_type, set_error, GENERIC_ERROR_CODE
    use handle_mod, only: handle_type
    use value_mod, only: value_type, make_list, xfixnum, vtag, V_INTEGER
    use buffer_mod, only: buffers, current, current_buffer, set_buffer, get_or_make_buffer, close_buffer, &
                          generate_new_buffer_name, get_file_buffer, is_buffer_live
    use editing_functions_mod, only: point, goto_character, point_minimum, point_maximum, buffer_string, is_end_of_buffer, &
                                     position_end_of_line, buffer_substring
    use editing_commands_mod, only: forward_line
    use search_mod, only: string_match, regular_expression_search_forward, looking_at, SEARCH_NOERROR
    use basic_subprograms_mod, only: match_string, version, operator(<=)
    use file_input_output_mod, only: expand_file_name, is_file_directory, file_exists, file_name_directory, rename_file, &
                                     insert_file_contents
    use files_mod, only: file_relative_name, file_truename, delete_file, delete_directory
    use execute_process_mod, only: execute_process
    use string_type_mod, only: string_type, append_string
    use version_control_hooks_mod, only: version_control_backends, version_control_switches_type, &
                                         register_version_control_backend, version_control_backend_named, &
                                         version_control_find_root, version_control_state, version_control_mode_line_state, &
                                         version_control_working_revision, version_control_file_set_state, &
                                         version_control_display_status, version_control_use_short_revision, &
                                         version_control_directory_entry_type, version_control_directory_fileinfo_type, &
                                         version_control_directory_update_procedure, VERSION_CONTROL_DISPLAY_STATUS_NO_BACKEND, &
                                         VERSION_CONTROL_SWITCHES_NONE, VERSION_CONTROL_SWITCHES_LIST, VERSION_CONTROL_UP_TO_DATE, &
                                         VERSION_CONTROL_EDITED, VERSION_CONTROL_ADDED, VERSION_CONTROL_REMOVED, &
                                         VERSION_CONTROL_MISSING, VERSION_CONTROL_IGNORED, VERSION_CONTROL_UNREGISTERED, &
                                         VERSION_CONTROL_STATE_UNKNOWN, VERSION_CONTROL_MODEL_IMPLICIT, &
                                         VERSION_CONTROL_RENAME_COPY, VERSION_CONTROL_RENAME_RENAME
    use version_control_dispatcher_mod, only: version_control_do_command, VERSION_CONTROL_OKSTATUS_UNLESS_SIGNAL, &
                                              VERSION_CONTROL_OKSTATUS_ASYNCHRONOUS, VERSION_CONTROL_DESTINATION_CURRENT
    use fortmacs_widget_for_object_collections_mod, only: fortmacs_widget_for_object_collections_part_type
    use version_control_mod, only: version_control_switches
    use hooks_mod, only: hook_type
    use commands_mod, only: commands
    use keymap_mod, only: keymap_set
    use modes_mod, only: major_modes
    use font_core_mod, only: font_lock_keyword_type, font_lock_highlight_type
    use font_lock_mod, only: font_lock_keyword, font_lock_highlight
    use log_edit_mod, only: log_edit_extract_headers, log_edit_toggle_amend, &
                            log_edit_header_type, initialize_log_edit, LOG_EDIT_HEADER_FN
    implicit none(type, external)
    private

    public :: initialize_version_control_mercurial, version_control_mercurial_registered, version_control_mercurial_state, &
              version_control_mercurial_state_slow, version_control_mercurial_working_revision, &
              version_control_mercurial_mode_line_string, version_control_mercurial_root, version_control_mercurial_command, &
              version_control_mercurial_previous_revision, version_control_mercurial_next_revision, &
              version_control_mercurial_revision_table, version_control_mercurial_diff_flags, &
              version_control_mercurial_after_directory_status, version_control_mercurial_parse_program_version

    !> String or list of strings: global switches to pass to any Hg
    !> command (VERSION_CONTROL_SWITCHES_LIST; the other kinds pass none).  Port of
    !> vc-hg.el vc-hg-global-switches.
    type(version_control_switches_type), public :: version_control_mercurial_global_switches

    !> String or list of strings specifying switches for Hg diff under VERSION_CONTROL.
    !> If VERSION_CONTROL_SWITCHES_UNSPECIFIED, use the value of `version_control_diff_switches'.  If VERSION_CONTROL_SWITCHES_NONE,
    !> use no switches.
    !> Port of vc-hg.el vc-hg-diff-switches (Hg doesn't support common
    !> arguments like -u).
    type(version_control_switches_type), target, public :: version_control_mercurial_diff_switches = &
                                                           version_control_switches_type(kind=VERSION_CONTROL_SWITCHES_NONE)

    !> String or list of strings specifying switches for hg revert under VERSION_CONTROL
    !> (VERSION_CONTROL_SWITCHES_LIST; the other kinds pass none).  Port of vc-hg.el
    !> vc-hg-revert-switches.
    type(version_control_switches_type), public :: version_control_mercurial_revert_switches

    !> Name of the Mercurial executable (excluding any arguments).  Port of
    !> vc-hg.el vc-hg-program.
    character(len=:), allocatable, public :: version_control_mercurial_program

    !> The value of `version_control_mercurial_root_log_format': the "--template" argument
    !> string, a regular expression matching the resulting Mercurial output,
    !> and the highlights of its match for the Log View buffer.
    type, public :: version_control_mercurial_root_log_format_type
        character(len=:), allocatable :: template, regular_expression
        type(font_lock_highlight_type), allocatable :: highlights(:)
    end type version_control_mercurial_root_log_format_type

    !> Mercurial log template for `version_control_mercurial_print_log' short format.
    !> This should be a TEMPLATE, a REGULAR_EXPRESSION and KEYWORDS, where TEMPLATE
    !> is the "--template" argument string to pass to Mercurial,
    !> REGULAR_EXPRESSION is a regular expression matching the resulting Mercurial
    !> output, and KEYWORDS is a list of `font_lock_keywords' for
    !> highlighting the Log View buffer.  Port of vc-hg.el
    !> vc-hg-root-log-format.
    type(version_control_mercurial_root_log_format_type), public :: version_control_mercurial_root_log_format

    !> The kinds of an entry of `version_control_mercurial_symbolic_revision_styles': the active
    !> bookmark (VERSION_CONTROL_MERCURIAL_REVISION_STYLES_ACTIVE_BOOKMARK) or a
    !> Mercurial template (VERSION_CONTROL_MERCURIAL_SYMBOLIC_REVISION_STYLES_TEMPLATE).
    integer, parameter, public :: VERSION_CONTROL_MERCURIAL_REVISION_STYLES_ACTIVE_BOOKMARK = 1, &
                                  VERSION_CONTROL_MERCURIAL_SYMBOLIC_REVISION_STYLES_TEMPLATE = 2

    !> One way to present versions symbolically: its kind and, for
    !> VERSION_CONTROL_MERCURIAL_SYMBOLIC_REVISION_STYLES_TEMPLATE, the template (trailing
    !> blanks are not part of it).
    type, public :: version_control_mercurial_revision_style_type
        integer :: kind = VERSION_CONTROL_MERCURIAL_SYMBOLIC_REVISION_STYLES_TEMPLATE
        character(len=128) :: template = ""
    end type version_control_mercurial_revision_style_type

    !> List of ways to present versions symbolically.
    !> The version that we use is the first one that successfully
    !> produces a string.
    !>
    !> Each entry in the list can be either:
    !>
    !> - VERSION_CONTROL_MERCURIAL_REVISION_STYLES_ACTIVE_BOOKMARK, which indicates
    !> that we should use the active bookmark if one exists.  A template can
    !> supply this information as well, but this kind is handled entirely
    !> inside Fortmacs and so is more efficient than using the generic
    !> Mercurial mechanism.
    !>
    !> - VERSION_CONTROL_MERCURIAL_SYMBOLIC_REVISION_STYLES_TEMPLATE, with the Mercurial
    !> template to supply to "hg parent".  "hg help template" may be useful
    !> reading.
    !>
    !> If no list entry produces a useful revision, "???" is shown.  Port
    !> of vc-hg.el vc-hg-symbolic-revision-styles.
    type(version_control_mercurial_revision_style_type), allocatable :: version_control_mercurial_symbolic_revision_styles(:)
    public :: version_control_mercurial_symbolic_revision_styles

    !> When enabled, the modeline contains revision information for the visited file.
    !> When not, the revision in the modeline is for the repository
    !> working copy.  .false. is the much faster setting for
    !> large repositories.  Port of vc-hg.el
    !> vc-hg-use-file-version-for-mode-line-version.
    logical, public :: version_control_mercurial_use_file_version_for_mode_line = .false.

    !> String or list of strings specifying switches for hg log under VERSION_CONTROL
    !> (VERSION_CONTROL_SWITCHES_LIST; the other kinds pass none).  Port of vc-hg.el
    !> vc-hg-log-switches.
    type(version_control_switches_type), public :: version_control_mercurial_log_switches

    !> If true, use `--graph' in the short log output.  Port of vc-hg.el
    !> vc-hg-log-graph.
    logical, public :: version_control_mercurial_log_graph = .false.

    !> Mercurial log template for `version_control_mercurial_print_log' long format.  Port of
    !> vc-hg.el vc-hg-log-format.
    character(len=:), allocatable, public :: version_control_mercurial_log_format

    !> Hook run after entering Hg-Log-View mode.
    type(hook_type), target, public :: version_control_mercurial_log_view_mode_hook = hook_type( &
                                       name="version_control_mercurial_log_view_mode_hook")

    !> The keymap of `version_control_mercurial_log_view_mode'.
    type(handle_type), public :: version_control_mercurial_log_view_mode_map

    !> Hook run by `version_control_mercurial_log_edit_mode'.
    type(hook_type), target, public :: version_control_mercurial_log_edit_mode_hook = hook_type( &
                                       name="version_control_mercurial_log_edit_mode_hook")

    !> Keymap of `version_control_mercurial_log_edit_mode'.  Port of vc-hg.el
    !> vc-hg-log-edit-mode-map.
    type(handle_type), public :: version_control_mercurial_log_edit_mode_map

    ! A `version_control_mercurial_directory_status_files' computation waiting for its process: the
    ! update function and the VC-Dir buffer it updates.
    type :: directory_status_type
        logical :: live = .false.
        procedure(version_control_directory_update_procedure), pointer, nopass :: update_function => null()
        type(handle_type) :: buffer
    end type directory_status_type

    ! The computations of `version_control_mercurial_directory_status_files' in progress.
    type(directory_status_type), allocatable :: directory_states(:)

    !> The version of Mercurial (vc-hg--program-version), unallocated until
    !> known.
    character(len=:), allocatable :: program_version

    ! (AMPERSAND: an ampersand in a string literal confuses fprettify.)
    character, parameter :: LF = achar(10), TAB = achar(9), AMPERSAND = achar(38)

contains

    ! ------------------------------------------------------------------
    ! Helpers
    ! ------------------------------------------------------------------

    !> The strings given, as a list (the short lists of hg arguments).
    !> Not a port.
    function strings(a1, a2, a3, a4, a5, a6, a7) result(list)
        character(len=*), intent(in) :: a1
        character(len=*), intent(in), optional :: a2, a3, a4, a5, a6, a7
        type(string_type), allocatable :: list(:)

        allocate (list(0))
        call append_string(list, a1)
        if (present(a2)) call append_string(list, a2)
        if (present(a3)) call append_string(list, a3)
        if (present(a4)) call append_string(list, a4)
        if (present(a5)) call append_string(list, a5)
        if (present(a6)) call append_string(list, a6)
        if (present(a7)) call append_string(list, a7)
    end function strings

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

    !> Append the switches of SWITCHES to FLAGS: a list, or none for the
    !> other kinds (a switches option of a single kind, as (vc-switches
    !> 'hg 'revert) where there is no `version_control_revert_switches').  Not a port.
    subroutine append_switches(flags, switches)
        type(string_type), allocatable, intent(inout) :: flags(:)
        type(version_control_switches_type), intent(in) :: switches

        if (switches%kind == VERSION_CONTROL_SWITCHES_LIST) then
            if (allocated(switches%list)) call append_all(flags, switches%list)
        end if
    end subroutine append_switches

    !> The current buffer's `default_directory'.  Not a port.
    function buffer_directory() result(directory)
        character(len=:), allocatable :: directory

        directory = "./"
        if (current == 0) return
        if (allocated(buffers(current)%directory)) directory = buffers(current)%directory
    end function buffer_directory

    !> Set the current buffer's `default_directory' to DIRECTORY.  Not a port.
    subroutine set_buffer_directory(directory)
        character(len=*), intent(in) :: directory

        if (current == 0) return
        buffers(current)%directory = directory
    end subroutine set_buffer_directory

    !> Make a temporary buffer TEMPORARY current (with-temp-buffer); OLD is the
    !> buffer to go back to.  Not a port.
    subroutine temporary_begin(old, temporary)
        type(handle_type), intent(out) :: old, temporary
        type(status_type) :: st
        character(len=:), allocatable :: directory

        old = current_buffer()
        directory = buffer_directory()
        call get_or_make_buffer(generate_new_buffer_name(" *temp*"), temporary, st)
        call set_buffer(temporary, st)
        call set_buffer_directory(directory)
    end subroutine temporary_begin

    !> Kill the temporary buffer TEMPORARY and make OLD current again.  Not a port.
    subroutine temporary_end(old, temporary)
        type(handle_type), intent(in) :: old, temporary
        type(status_type) :: st

        call close_buffer(temporary, st)
        call set_buffer(old, st)
    end subroutine temporary_end

    !> The index of the Hg backend.  Not a port.
    integer function mercurial_backend_index() result(b)
        b = version_control_backend_named("Hg")
    end function mercurial_backend_index

    !> The words of S separated by whitespace (split-string with its
    !> default separators), appended to LIST.  Not a port.
    subroutine append_words(list, s)
        type(string_type), allocatable, intent(inout) :: list(:)
        character(len=*), intent(in) :: s
        character(len=*), parameter :: WHITESPACE = " "//achar(12)//achar(9)//achar(10)//achar(13)//achar(11)
        integer :: b, e

        b = 1

        do while (b <= len(s))
            e = verify(s(b:), WHITESPACE)
            if (e == 0) exit
            b = b + e - 1
            e = scan(s(b:), WHITESPACE)

            if (e == 0) then
                e = len(s) + 1
            else
                e = b + e - 1
            end if

            call append_string(list, s(b:e - 1))
            b = e
        end do
    end subroutine append_words

    ! ------------------------------------------------------------------
    ! Internal functions
    ! ------------------------------------------------------------------

    !> A wrapper around `version_control_do_command' for use in version_control_mercurial_mod.
    !> This function differs from `version_control_do_command' in that
    !> - BUFFER absent means use a buffer called "*vc*" (IN_CURRENT: the
    !>   current buffer; STDERR false discards the standard error)
    !> - it invokes `version_control_mercurial_program' and passes `version_control_mercurial_global_switches' to it
    !>   before FLAGS.
    !> Port of vc-hg.el:vc-hg-command (with vc-hg--command-1: HGPLAIN=1
    !> disables the pager).
    recursive subroutine version_control_mercurial_command(okstatus, files, flags, status, buffer, in_current, stderr, exit_status)
        integer, intent(in) :: okstatus
        type(string_type), intent(in) :: files(:), flags(:)
        type(status_type), intent(out) :: status
        type(handle_type), intent(in), optional :: buffer
        logical, intent(in), optional :: in_current, stderr
        integer, intent(out), optional :: exit_status
        type(string_type), allocatable :: all(:), environment_variables(:)
        type(handle_type) :: buffer_handle
        logical :: to_current

        status = status_type()
        ! Disable pager.
        environment_variables = strings("HGPLAIN=1")
        all = strings("--config", "ui.report_untrusted=0")
        call append_switches(all, version_control_mercurial_global_switches)
        call append_all(all, flags)
        to_current = .false.
        if (present(in_current)) to_current = in_current

        if (to_current) then
            call version_control_do_command(VERSION_CONTROL_DESTINATION_CURRENT, okstatus, version_control_mercurial_program, &
                                            files, all, status, stderr=stderr, &
                                            exit_status=exit_status, environment=environment_variables)
        else
            if (present(buffer)) then
                buffer_handle = buffer
            else
                call get_or_make_buffer("*vc*", buffer_handle, status)
                if (.not. status%ok()) return
            end if

            call version_control_do_command(VERSION_CONTROL_DESTINATION_CURRENT, okstatus, version_control_mercurial_program, &
                                            files, all, status, buffer=buffer_handle, &
                                            stderr=stderr, exit_status=exit_status, environment=environment_variables)
        end if
    end subroutine version_control_mercurial_command

    !> The root of FILE's working tree (the directory with .hg).  Port of
    !> vc-hg.el:vc-hg-root.
    subroutine version_control_mercurial_root(file, root, found)
        character(len=*), intent(in) :: file
        character(len=:), allocatable, intent(out) :: root
        logical, intent(out) :: found

        call version_control_find_root(file, ".hg", root, found)
    end subroutine version_control_mercurial_root

    !> The output of `hg FLAGS' in a temporary buffer, in OUT; OK is false
    !> when the command fails (an exit status above OKSTATUS).  Not a port.
    subroutine command_output(okstatus, flags, out, ok)
        integer, intent(in) :: okstatus
        type(string_type), intent(in) :: flags(:)
        character(len=:), allocatable, intent(out) :: out
        logical, intent(out) :: ok
        type(string_type) :: none(0)
        type(handle_type) :: old, temporary
        type(status_type) :: st

        call temporary_begin(old, temporary)
        call version_control_mercurial_command(okstatus, none, flags, st, in_current=.true.)
        ok = st%ok()
        out = buffer_string()
        call temporary_end(old, temporary)
    end subroutine command_output

    !> The version of Mercurial, "0" if unknown.  Port of
    !> vc-hg.el:vc-hg--program-version (the parsing is
    !> `version_control_mercurial_parse_program_version').
    function version_control_mercurial_program_version() result(version)
        character(len=:), allocatable :: version
        character(len=:), allocatable :: out
        logical :: ok

        if (allocated(program_version)) then
            version = program_version
            return
        end if

        call command_output(0, strings("version"), out, ok)

        if (ok) then
            version = version_control_mercurial_parse_program_version(out)
        else
            version = "0"
        end if

        program_version = version
    end function version_control_mercurial_program_version

    !> The version in VERSION_STRING, the output of `hg version' ("0" if
    !> none), without a trailing dot.  Port of vc-hg.el:vc-hg--program-version,
    !> in part.
    function version_control_mercurial_parse_program_version(version_string) result(version)
        character(len=*), intent(in) :: version_string
        character(len=:), allocatable :: version
        type(status_type) :: st
        integer :: k

        version = "0"

        if (string_match("Mercurial Distributed SCM (version \([0-9][0-9.]+\)", version_string, st) > 0) then
            version = match_string(1, version_string)
            ! (string-trim-right ... "\\."): one trailing dot.
            k = len(version)
            if (version(k:k) == ".") version = version(1:k - 1)
        end if
    end function version_control_mercurial_parse_program_version

    ! ------------------------------------------------------------------
    ! Properties of the backend
    ! ------------------------------------------------------------------

    !> Hg's checkout model: `implicit'.  Port of
    !> vc-hg.el:vc-hg-checkout-model.
    integer function version_control_mercurial_checkout_model(files) result(model)
        type(string_type), intent(in) :: files(:)

        model = VERSION_CONTROL_MODEL_IMPLICIT
        if (size(files) < 0) model = 0
    end function version_control_mercurial_checkout_model

    !> The working revision's symbolic name: ".".  Port of vc-hg.el
    !> vc-hg-working-revision-symbol.
    subroutine version_control_mercurial_working_revision_symbol(s)
        character(len=:), allocatable, intent(out) :: s

        s = "."
    end subroutine version_control_mercurial_working_revision_symbol

    ! ------------------------------------------------------------------
    ! State querying functions
    ! ------------------------------------------------------------------

    !> Return true if FILE is registered with hg.  Port of
    !> vc-hg.el:vc-hg-registered (modeled after the similar function in
    !> vc-bzr.el).
    logical function version_control_mercurial_registered(file) result(registered)
        character(len=*), intent(in) :: file
        character(len=:), allocatable :: root
        logical :: found
        integer :: state

        registered = .false.
        ! Short cut.
        call version_control_mercurial_root(file, root, found)
        if (.not. found) return
        ! Expensive.
        state = version_control_state(file, mercurial_backend_index())

        if (state == VERSION_CONTROL_IGNORED .or. state == VERSION_CONTROL_UNREGISTERED .or. state &
            == VERSION_CONTROL_STATE_UNKNOWN) then
            ! Clear the cache for proper fallback to another backend.
            call version_control_file_set_state(file, VERSION_CONTROL_STATE_UNKNOWN)
        else
            registered = .true.
        end if
    end function version_control_mercurial_registered

    !> Hg-specific version of `version_control_state'.  Port of vc-hg.el:vc-hg-state
    !> (`version_control_mercurial_state_slow' always: the fast path, vc-hg-state-fast, is
    !> deferred).
    integer function version_control_mercurial_state(file) result(state)
        character(len=*), intent(in) :: file

        state = VERSION_CONTROL_STATE_UNKNOWN
        if (.not. is_file_directory(file)) state = version_control_mercurial_state_slow(file)
    end function version_control_mercurial_state

    !> Determine status of FILE by running hg.  Port of
    !> vc-hg.el:vc-hg-state-slow.
    integer function version_control_mercurial_state_slow(file) result(state)
        character(len=*), intent(in) :: file
        character(len=:), allocatable :: name, root, relative, saved, out
        type(value_type), allocatable :: argv(:), envv(:)
        type(value_type) :: result
        type(handle_type) :: old, temporary
        type(status_type) :: st
        logical :: found
        integer :: code

        state = VERSION_CONTROL_STATE_UNKNOWN
        call expand_file_name(file, name, st)
        if (.not. st%ok()) return
        call version_control_mercurial_root(name, root, found)
        if (.not. found) root = buffer_directory()
        relative = file_relative_name(name, root)
        call temporary_begin(old, temporary)
        saved = buffer_directory()
        call set_buffer_directory(root)
        ! One element at a time: function results in an array constructor
        ! of value_type leak.
        allocate (argv(9), envv(3))
        argv(1) = value_type(text="--config")
        argv(2) = value_type(text="ui.report_untrusted=0")
        argv(3) = value_type(text="--config")
        argv(4) = value_type(text="alias.status=status")
        argv(5) = value_type(text="--config")
        argv(6) = value_type(text="defaults.status=")
        argv(7) = value_type(text="status")
        argv(8) = value_type(text="-A")
        argv(9) = value_type(text=file_relative_name(relative))
        ! Avoid localization of messages so we can parse the output.
        ! Disable pager.
        envv(1) = value_type(text="TERM=dumb")
        envv(2) = value_type(text="LANGUAGE=C")
        envv(3) = value_type(text="HGPLAIN=1")
        ! Ignore all errors (e.g. we can't find an `mercurial' executable).
        call execute_process(version_control_mercurial_program, result, st, destination=value_type(.true.), &
                             arguments=make_list(argv), &
                             environment=make_list(envv))
        code = -1

        if (st%ok()) then
            if (vtag(result) == V_INTEGER) code = int(xfixnum(result))
        end if

        out = buffer_string()
        call set_buffer_directory(saved)
        call temporary_end(old, temporary)
        if (code /= 0 .or. len(out) == 0) return
        if (string_match(".*: No such file or directory$", out, st) > 0) return

        select case (out(1:1))
        case ("=")
            state = VERSION_CONTROL_UP_TO_DATE
        case ("A")
            state = VERSION_CONTROL_ADDED
        case ("M")
            state = VERSION_CONTROL_EDITED
        case ("I")
            state = VERSION_CONTROL_IGNORED
        case ("R")
            state = VERSION_CONTROL_REMOVED
        case ("!")
            state = VERSION_CONTROL_MISSING
        case ("?")
            state = VERSION_CONTROL_UNREGISTERED
        case ("C")
            ! Older mercurial versions use this.
            state = VERSION_CONTROL_UP_TO_DATE
        case default
            state = VERSION_CONTROL_UP_TO_DATE
        end select
    end function version_control_mercurial_state_slow

    !> Hg-specific version of `version_control_working_revision'.  Port of
    !> vc-hg.el:vc-hg-working-revision.
    subroutine version_control_mercurial_working_revision(file, revision, found)
        character(len=*), intent(in) :: file
        character(len=:), allocatable, intent(out) :: revision
        logical, intent(out) :: found
        character(len=:), allocatable :: template

        revision = ""
        found = .false.
        if (len(file) < 0) return

        if (version_control_use_short_revision) then
            template = "{node|short}"
        else
            template = "{node}"
        end if

        call command_output(0, strings("log", "-r", ".", "--template", template), revision, found)
        if (.not. found) revision = ""
    end subroutine version_control_mercurial_working_revision

    !> The active bookmark of REVISION, if REVISION is "." and a bookmark is active.
    !> Port of vc-hg.el:vc-hg--active-bookmark-internal.
    subroutine version_control_mercurial_active_bookmark_internal(revision, bookmark, found)
        character(len=*), intent(in) :: revision
        character(len=:), allocatable, intent(out) :: bookmark
        logical, intent(out) :: found
        character(len=:), allocatable :: root, current_bookmarks_file
        type(handle_type) :: old, temporary
        type(status_type) :: st
        integer(int64) :: inserted
        logical :: has_root

        bookmark = ""
        found = .false.
        if (revision /= ".") return
        call version_control_mercurial_root(buffer_directory(), root, has_root)
        if (.not. has_root) root = buffer_directory()
        call expand_file_name(".hg/bookmarks.current", current_bookmarks_file, st, root)
        if (.not. st%ok()) return
        if (.not. file_exists(current_bookmarks_file)) return
        call temporary_begin(old, temporary)
        call insert_file_contents(current_bookmarks_file, .false., inserted, st)
        found = st%ok()
        if (found) bookmark = buffer_string()
        call temporary_end(old, temporary)
    end subroutine version_control_mercurial_active_bookmark_internal

    !> The output of `hg log' with TEMPLATE for REVISION, or for the file PATH
    !> (following its history) when given; FOUND is false on errors.
    !> Port of vc-hg.el:vc-hg--run-log.
    subroutine version_control_mercurial_execute_log(template, revision, out, found, path)
        character(len=*), intent(in) :: template, revision
        character(len=:), allocatable, intent(out) :: out
        logical, intent(out) :: found
        character(len=*), intent(in), optional :: path

        if (present(path)) then
            call command_output(0, strings("log", "-f", "-l1", "--template", template, path), out, found)
        else
            call command_output(0, strings("log", "-r", revision, "-l1", "--template", template), out, found)
        end if
    end subroutine version_control_mercurial_execute_log

    !> Make a Mercurial revision human-readable.
    !> REVISION is a Mercurial revision.  `default_directory' is assumed to
    !> be in the repository root of interest.  PATH, if present, is a
    !> specific file to query.  FOUND is false if no style of
    !> `version_control_mercurial_symbolic_revision_styles' produced one.  Port of
    !> vc-hg.el:vc-hg--symbolic-revision.
    subroutine version_control_mercurial_symbolic_revision(revision, symbolic_revision, found, path)
        character(len=*), intent(in) :: revision
        character(len=:), allocatable, intent(out) :: symbolic_revision
        logical, intent(out) :: found
        character(len=*), intent(in), optional :: path
        integer :: k

        symbolic_revision = ""
        found = .false.
        if (.not. allocated(version_control_mercurial_symbolic_revision_styles)) return

        do k = 1, size(version_control_mercurial_symbolic_revision_styles)
            associate (style => version_control_mercurial_symbolic_revision_styles(k))
                select case (style%kind)
                case (VERSION_CONTROL_MERCURIAL_REVISION_STYLES_ACTIVE_BOOKMARK)
                    if (.not. present(path)) call version_control_mercurial_active_bookmark_internal(revision, symbolic_revision, &
                                                                                                     found)
                case (VERSION_CONTROL_MERCURIAL_SYMBOLIC_REVISION_STYLES_TEMPLATE)
                    call version_control_mercurial_execute_log(trim(style%template), revision, symbolic_revision, found, path)
                case default
                    continue
                end select
            end associate

            if (found) return
        end do
    end subroutine version_control_mercurial_symbolic_revision

    !> Hg-specific version of `version_control_mode_line_string'.  Port of
    !> vc-hg.el:vc-hg-mode-line-string (no faces or help-echo).
    subroutine version_control_mercurial_mode_line_string(file, s, found)
        character(len=*), intent(in) :: file
        character(len=:), allocatable, intent(out) :: s
        logical, intent(out) :: found
        character(len=:), allocatable :: truename, echo, face, indicator, revision, root, directory, saved
        type(status_type) :: st
        logical :: has_revision, has_root
        integer :: state

        call file_truename(file, truename, st)
        if (.not. st%ok()) truename = file
        state = version_control_state(truename)
        call version_control_mode_line_state(state, echo, face, indicator)
        has_revision = .false.

        if (state /= VERSION_CONTROL_STATE_UNKNOWN) then
            call version_control_mercurial_root(truename, root, has_root)
            if (.not. has_root) root = buffer_directory()
            call expand_file_name(root, directory, st)
            if (.not. st%ok()) directory = root
            saved = buffer_directory()
            call set_buffer_directory(directory)

            if (version_control_mercurial_use_file_version_for_mode_line) then
                call version_control_mercurial_symbolic_revision(".", revision, has_revision, truename)
            else
                call version_control_mercurial_symbolic_revision(".", revision, has_revision)
            end if

            call set_buffer_directory(saved)
        end if

        if (.not. has_revision) revision = "???"
        s = ""
        if (version_control_display_status%choice /= VERSION_CONTROL_DISPLAY_STATUS_NO_BACKEND) s = "Hg"
        s = s//indicator//revision
        found = .true.
    end subroutine version_control_mercurial_mode_line_string

    ! ------------------------------------------------------------------
    ! History functions
    ! ------------------------------------------------------------------

    !> Print commit log associated with FILES into specified BUFFER.
    !> If SHORTLOG is true, use a short format based on `version_control_mercurial_root_log_format'.
    !> If LIMIT is a positive integer, show no more than that many entries.
    !> If LIMIT is a non-empty string, use it as a base revision.
    !>
    !> If START_REVISION is "", print the commit log starting from the working
    !> directory parent (revset ".").  Otherwise, print the log starting
    !> from that revision.  Port of vc-hg.el:vc-hg-print-log.
    recursive subroutine version_control_mercurial_print_log(files, buffer, shortlog, start_revision, limit, status, &
                                                             limit_unsupported)
        use version_control_dispatcher_mod, only: version_control_setup_buffer
        use version_control_mod, only: version_control_log_view_type, VERSION_CONTROL_LOG_VIEW_WITH_DIFF, &
                                       VERSION_CONTROL_LOG_VIEW_LOG_OUTGOING, VERSION_CONTROL_LOG_VIEW_LOG_UNINTEGRATED
        use version_control_hooks_mod, only: version_control_log_limit_type, VERSION_CONTROL_LIMIT_NUMBER, &
                                             VERSION_CONTROL_LIMIT_REVISION
        type(string_type), intent(in) :: files(:)
        type(handle_type), intent(in) :: buffer
        logical, intent(in) :: shortlog
        character(len=*), intent(in) :: start_revision
        type(version_control_log_limit_type), intent(in) :: limit
        type(status_type), intent(out) :: status
        logical, intent(out) :: limit_unsupported
        type(string_type), allocatable :: flags(:)
        type(handle_type) :: old
        type(status_type) :: st
        character(len=:), allocatable :: start
        character(len=21) :: limit_text
        integer :: view

        limit_unsupported = .false.
        ! `vc-do-command' creates the buffer, but we need it before running
        ! the command.
        call version_control_setup_buffer(buffer, status)
        if (.not. status%ok()) return
        ! If the buffer exists from a previous invocation it might be
        ! read-only (`version_control_do_command' binds `inhibit_read_only').
        ! Normalize START-REVISION parameter.
        start = start_revision
        if (len(start) == 0) start = "."
        view = version_control_log_view_type()
        old = current_buffer()
        call set_buffer(buffer, st)
        ! With Mercurial logs there are are, broadly speaking, two
        ! kinds of ranges of revisions for the log to show:
        !   - ranges by revision number:   -rN:M
        !   - ranges according to the DAG: -rN::M or -rN..M
        ! Note that N and M can be revision numbers or changeset
        ! IDs (hashes).  In either case a revision number range
        ! includes those commits with revision numbers between the
        ! revision numbers of the commits identified by N and M.
        ! See <https://repo.mercurial-scm.org/hg/help/revsets>.
        !
        ! DAG ranges are not the same as Git's double-dot ranges.
        ! Git's 'x..y' is more like Mercurial's 'only(y, x)' than
        ! it is like Mercurial's x::y.  In addition, with -rN::M,
        ! commits from other branches aren't included in the log.
        !
        ! VERSION_CONTROL has always used ranges by revision numbers, such that
        ! commits from all branches are included in the log.
        flags = strings("log")

        if (limit%kind /= VERSION_CONTROL_LIMIT_REVISION) then
            call append_string(flags, "-r"//start//":0")
        else if (view == VERSION_CONTROL_LOG_VIEW_LOG_OUTGOING .or. view == VERSION_CONTROL_LOG_VIEW_LOG_UNINTEGRATED) then
            call append_string(flags, "-rreverse(only("//start//", "//limit%revision//"))")
        else
            call append_string(flags, "-r"//start//":"//limit%revision//" "//AMPERSAND//" !"//limit%revision)
        end if

        if (limit%kind == VERSION_CONTROL_LIMIT_NUMBER) then
            write (limit_text, "(i0)") limit%n
            call append_all(flags, strings("-l", trim(limit_text)))
        end if

        if (view == VERSION_CONTROL_LOG_VIEW_WITH_DIFF) call append_string(flags, "-p")

        if (shortlog) then
            if (version_control_mercurial_log_graph) call append_string(flags, "--graph")
            call append_all(flags, strings("--template", version_control_mercurial_root_log_format%template))
        else
            call append_all(flags, strings("--template", version_control_mercurial_log_format))
        end if

        call append_switches(flags, version_control_mercurial_log_switches)
        call version_control_mercurial_command(VERSION_CONTROL_OKSTATUS_ASYNCHRONOUS, files, flags, status, buffer=buffer)
        call set_buffer(old, st)
    end subroutine version_control_mercurial_print_log

    !> Major mode for Hg logs.  Port of vc-hg.el:vc-hg-log-view-mode.
    recursive subroutine version_control_mercurial_log_view_mode(status)
        use version_control_mod, only: version_control_log_view_type, version_control_shortlog, &
                                       VERSION_CONTROL_LOG_VIEW_LOG_UNINTEGRATED, VERSION_CONTROL_LOG_VIEW_SHORT, &
                                       VERSION_CONTROL_LOG_VIEW_LOG_INCOMING, VERSION_CONTROL_LOG_VIEW_LOG_OUTGOING
        use log_view_mod, only: set_log_view_file_regular_expression, set_log_view_message_regular_expression, &
                                set_log_view_per_file_logs, set_log_view_expanded_log_entry_function, &
                                set_log_view_font_lock_keywords, log_view_font_lock_keywords
        use display_engine_mod, only: tab_width
        type(status_type), intent(out) :: status
        type(font_lock_keyword_type), allocatable :: keywords(:), more(:)
        character(len=*), parameter :: EMAIL = "[A-Za-z0-9_.+-]+@[A-Za-z0-9_.-]+"
        logical :: saved, shortp
        integer :: view

        if (.not. associated(major_modes%version_control_mercurial_log_view_mode%execute)) call define_log_view_mode()
        call major_modes%version_control_mercurial_log_view_mode%begin(saved, status)
        if (.not. status%ok()) return
        view = version_control_log_view_type()

        if (view == VERSION_CONTROL_LOG_VIEW_LOG_UNINTEGRATED) then
            shortp = version_control_shortlog()
        else
            shortp = view == VERSION_CONTROL_LOG_VIEW_SHORT .or. view == VERSION_CONTROL_LOG_VIEW_LOG_INCOMING .or. view &
                     == VERSION_CONTROL_LOG_VIEW_LOG_OUTGOING
        end if

        ! A file regexp that matches nothing (regexp-unmatchable).
        call set_log_view_file_regular_expression("\`a\`")
        call set_log_view_per_file_logs(.false.)

        if (shortp) then
            call set_log_view_message_regular_expression(version_control_mercurial_root_log_format%regular_expression)
        else
            call set_log_view_message_regular_expression("^changeset:[ "//TAB//"]*\([0-9]+\):\(.+\)")
        end if

        call tab_width%set_local(2_int64)
        ! Allow expanding short log entries (upstream also truncates the
        ! lines: `truncate_lines' is not ported).
        if (shortp) call set_log_view_expanded_log_entry_function(version_control_mercurial_expanded_log_entry)

        if (shortp) then
            allocate (keywords(1))
            keywords(1) = font_lock_keyword(version_control_mercurial_root_log_format%regular_expression, &
                                            highlights=version_control_mercurial_root_log_format%highlights)
        else
            call log_view_font_lock_keywords(keywords)
            allocate (more(5))
            ! Handle the case:
            ! user: FirstName LastName <foo@bar>
            more(1) = font_lock_keyword("^user:[ "//TAB//"]+\([^<(]+?\)[ "//TAB//"]*[(<]\("//EMAIL//"\)[>)]", &
                                        highlights=two("change_log_name", "change_log_email"))
            ! Handle the cases:
            ! user: foo@bar
            ! and
            ! user: foo
            more(2) = font_lock_keyword("^user:[ "//TAB//"]+\([A-Za-z0-9_.+-]+\(?:@[A-Za-z0-9_.-]+\)?\)", &
                                        face="change_log_email", subexp=1)
            more(3) = font_lock_keyword("^date: \(.+\)", face="change_log_date", subexp=1)
            more(4) = font_lock_keyword("^tag: +\([^ ]+\)$", face="highlight", subexp=1)
            more(5) = font_lock_keyword("^summary:[ "//TAB//"]+\(.+\)", face="log_view_message", subexp=1)
            keywords = [keywords, more]
        end if

        call set_log_view_font_lock_keywords(keywords)
        call major_modes%version_control_mercurial_log_view_mode%end(saved, status)

    contains

        ! The highlights (1 'FACE1) (2 'FACE2).
        function two(face1, face2) result(h)
            character(len=*), intent(in) :: face1, face2
            type(font_lock_highlight_type) :: h(2)

            h(1) = font_lock_highlight(1, face=face1)
            h(2) = font_lock_highlight(2, face=face2)
        end function two
    end subroutine version_control_mercurial_log_view_mode

    !> The expanded log entry of REVISION: its long log, indented.  Port of
    !> vc-hg.el:vc-hg-expanded-log-entry.
    recursive subroutine version_control_mercurial_expanded_log_entry(revision, text, found)
        use indent_mod, only: indent_region
        character(len=*), intent(in) :: revision
        character(len=:), allocatable, intent(out) :: text
        logical, intent(out) :: found
        type(string_type) :: none(0)
        type(handle_type) :: old, temporary
        type(status_type) :: st

        found = .false.
        text = ""
        call temporary_begin(old, temporary)
        call version_control_mercurial_command(VERSION_CONTROL_OKSTATUS_UNLESS_SIGNAL, none, strings("log", "-r", revision, &
                                                                                                     "--template", &
                                                                       version_control_mercurial_log_format), st, in_current=.true.)
        call goto_character(point_minimum())

        if (.not. is_end_of_buffer()) then
            ! Indent the expanded log entry.
            call indent_region(point_minimum(), point_maximum(), st, 2_int64)
            call goto_character(point_maximum())
            text = buffer_string()
            found = .true.
        end if

        call temporary_end(old, temporary)
    end subroutine version_control_mercurial_expanded_log_entry

    ! Define Hg's Log View mode.  Not a port.
    subroutine define_log_view_mode()
        use log_view_mod, only: initialize_log_view
        use add_log_mod, only: initialize_add_log

        ! (require 'add-log): we need the add-log faces.
        if (.not. associated(major_modes%change_log_mode%execute)) call initialize_add_log()
        if (.not. associated(major_modes%log_view_mode%execute)) call initialize_log_view()
        call major_modes%version_control_mercurial_log_view_mode%define("version_control_mercurial_log_view_mode", "Hg-Log-View", &
                                                                        version_control_mercurial_log_view_mode, &
                                                     version_control_mercurial_log_view_mode_hook, parent=major_modes%log_view_mode)
        version_control_mercurial_log_view_mode_map = major_modes%version_control_mercurial_log_view_mode%map
        commands%version_control_mercurial_log_view_mode%execute => version_control_mercurial_log_view_mode
    end subroutine define_log_view_mode

    ! The defaults of `version_control_mercurial_root_log_format' and `version_control_mercurial_log_format'.
    ! Not a port.
    subroutine initialize_log_formats()
        if (.not. allocated(version_control_mercurial_root_log_format%template)) then
            version_control_mercurial_root_log_format%template = "{rev}:{ifeq(branch, 'default','', '{branch}')}" &
                                                                 //":{bookmarks}:{tags}:{author|person}" &
                                                                 //" {date|shortdate} {desc|firstline}\n"
            ! Graph data, then the fields.
            version_control_mercurial_root_log_format%regular_expression = "^\(?:[+@o x|-]*\)" &
                                                                           //"\([0-9]+\):\([^:]*\)" &
                                                                           //":\([^:]*\):\([^:]*\):\(.*?\)" &
                                                                           //"[ "//TAB//"]+\([0-9]\{4\}-[0-9]\{2\}-[0-9]\{2\}\)"
            allocate (version_control_mercurial_root_log_format%highlights(6))
            version_control_mercurial_root_log_format%highlights(1) = font_lock_highlight(1, face="log_view_message")
            version_control_mercurial_root_log_format%highlights(2) = font_lock_highlight(2, face="change_log_file")
            version_control_mercurial_root_log_format%highlights(3) = font_lock_highlight(3, face="change_log_list")
            version_control_mercurial_root_log_format%highlights(4) = font_lock_highlight(4, face="change_log_conditionals")
            version_control_mercurial_root_log_format%highlights(5) = font_lock_highlight(5, face="change_log_name")
            version_control_mercurial_root_log_format%highlights(6) = font_lock_highlight(6, face="change_log_date")
        end if

        if (.not. allocated(version_control_mercurial_log_format)) then
            version_control_mercurial_log_format = "changeset:   {rev}:{node|short}"//LF &
                                                   //"{tags % 'tag:         {tag}"//LF//"'}" &
                                                   //"{if(parents, 'parents:     {parents}"//LF//"')}" &
                                                   //"user:        {author}"//LF &
                                                   //"Date:        {date|date}"//LF &
                                                   //"summary:     {desc|tabindent}"//LF//LF
        end if

        if (.not. allocated(version_control_mercurial_symbolic_revision_styles)) then
            allocate (version_control_mercurial_symbolic_revision_styles(2))
            version_control_mercurial_symbolic_revision_styles(1)%kind = VERSION_CONTROL_MERCURIAL_REVISION_STYLES_ACTIVE_BOOKMARK
            version_control_mercurial_symbolic_revision_styles(2)%kind = VERSION_CONTROL_MERCURIAL_SYMBOLIC_REVISION_STYLES_TEMPLATE
            version_control_mercurial_symbolic_revision_styles(2)%template = &
                "{if(bookmarks,sub(' ',',',bookmarks),if(phabdiff,phabdiff,shortest(node,6)))}"
        end if
    end subroutine initialize_log_formats

    ! ------------------------------------------------------------------
    ! Diff and revisions
    ! ------------------------------------------------------------------

    !> The flags of `version_control_mercurial_diff' for FILES between OLDVERS and NEWVERS:
    !> "diff", the switches and the revisions.  Not a port (the arguments
    !> part of vc-hg.el:vc-hg-diff).
    subroutine version_control_mercurial_diff_flags(files, flags, oldvers, newvers)
        type(string_type), intent(in) :: files(:)
        type(string_type), allocatable, intent(out) :: flags(:)
        character(len=*), intent(in), optional :: oldvers, newvers
        type(string_type), allocatable :: sw(:)
        character(len=:), allocatable :: working, old
        logical :: has_working, has_old

        has_working = .false.
        if (size(files) > 0) call version_control_working_revision(files(1)%text, working, has_working, mercurial_backend_index())
        has_old = present(oldvers)
        if (has_old) old = oldvers

        if (.not. present(newvers) .and. has_old) then
            if (old == ".") then
                has_old = .false.
            else if (has_working) then
                if (old == working) has_old = .false.
            end if
        end if

        if (present(newvers) .and. .not. has_old) then
            old = "null"
            has_old = .true.
        end if

        flags = strings("diff")
        call version_control_switches(mercurial_backend_index(), "diff", sw)
        call append_all(flags, sw)

        if (has_old) then
            if (present(newvers)) then
                call append_all(flags, strings("-r", old, "-r", newvers))
            else
                call append_all(flags, strings("-r", old))
            end if
        end if
    end subroutine version_control_mercurial_diff_flags

    !> Get a difference report using hg between two revisions of FILES.
    !> Port of vc-hg.el:vc-hg-diff.
    recursive subroutine version_control_mercurial_diff(files, buffer, asynchronous, status, rev1, rev2)
        type(string_type), intent(in) :: files(:)
        type(handle_type), intent(in) :: buffer
        logical, intent(in) :: asynchronous
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: rev1, rev2
        type(string_type), allocatable :: flags(:)
        integer :: okstatus

        call version_control_mercurial_diff_flags(files, flags, rev1, rev2)
        okstatus = 1
        if (asynchronous) okstatus = VERSION_CONTROL_OKSTATUS_ASYNCHRONOUS
        call version_control_mercurial_command(okstatus, files, flags, status, buffer=buffer)
    end subroutine version_control_mercurial_diff

    !> The revisions Hg can complete: the branches, bookmarks and tags.
    !> Port of vc-hg.el:vc-hg-revision-table (as
    !> vc-hg-revision-completion-table).
    subroutine version_control_mercurial_revision_table(files, table)
        type(string_type), intent(in) :: files(:)
        type(string_type), allocatable, intent(out) :: table(:)
        type(string_type) :: none(0)
        type(handle_type) :: old, temporary
        type(status_type) :: st

        allocate (table(0))
        call temporary_begin(old, temporary)
        if (size(files) > 0) call set_buffer_directory(file_name_directory(files(1)%text))
        call version_control_mercurial_command(VERSION_CONTROL_OKSTATUS_UNLESS_SIGNAL, none, strings("branches", "-q"), st, &
                                               in_current=.true.)
        call version_control_mercurial_command(VERSION_CONTROL_OKSTATUS_UNLESS_SIGNAL, none, strings("bookmarks", "-q"), st, &
                                               in_current=.true.)
        call version_control_mercurial_command(VERSION_CONTROL_OKSTATUS_UNLESS_SIGNAL, none, strings("tags", "-q"), st, &
                                               in_current=.true.)
        call append_words(table, buffer_string())
        call temporary_end(old, temporary)
    end subroutine version_control_mercurial_revision_table

    !> Hg-specific version of `version_control_previous_revision'.  Port of
    !> vc-hg.el:vc-hg-previous-revision.
    subroutine version_control_mercurial_previous_revision(file, revision, previous, found)
        character(len=*), intent(in) :: file, revision
        character(len=:), allocatable, intent(out) :: previous
        logical, intent(out) :: found
        type(string_type) :: none(0)
        type(handle_type) :: old, temporary
        type(status_type) :: st
        character(len=:), allocatable :: digits
        character(len=16) :: text
        integer :: n, code, ios

        previous = ""
        found = .false.
        if (len(file) < 0) return

        ! Prefer to return values with tildes not carets because that's more
        ! compatible with MS-Windows (see `version_control_git_previous_revision').
        !
        ! See <https://repo.mercurial-scm.org/hg/help/revsets> for reference.
        if (string_match("\`\.\(\^*\)\'", revision, st) > 0) then
            write (text, '(".~", i0)') 1 + len(match_string(1, revision))
            previous = trim(text)
            found = .true.
        else if (string_match("\`\.~\([0-9]+\)\'", revision, st) > 0) then
            digits = match_string(1, revision)
            read (digits, *, iostat=ios) n
            if (ios /= 0) n = 0
            write (text, '(".~", i0)') 1 + n
            previous = trim(text)
            found = .true.
        else
            ! We can't simply decrement by 1, because that revision might
            ! be e.g. on a different branch (bug#22032).
            call temporary_begin(old, temporary)
            call version_control_mercurial_command(VERSION_CONTROL_OKSTATUS_UNLESS_SIGNAL, none, strings("id", "-n", "-r", &
                                                                                                         revision//"~1"), st, &
                                                   in_current=.true., exit_status=code)

            if (st%ok() .and. code == 0 .and. point_maximum() > point_minimum()) then
                ! Trim the trailing newline.
                call buffer_substring(point_minimum(), point_maximum() - 1, previous, st)
                found = st%ok()
            end if

            call temporary_end(old, temporary)
        end if
    end subroutine version_control_mercurial_previous_revision

    !> Hg-specific version of `version_control_next_revision': the revision number
    !> after REVISION, if it does not exceed the tip.  Port of
    !> vc-hg.el:vc-hg-next-revision.
    subroutine version_control_mercurial_next_revision(file, revision, next, found)
        character(len=*), intent(in) :: file, revision
        character(len=:), allocatable, intent(out) :: next
        logical, intent(out) :: found
        type(string_type) :: none(0)
        type(handle_type) :: old, temporary
        type(status_type) :: st
        integer(int64) :: position, newrev, tip_revision
        character(len=:), allocatable :: digits
        character(len=21) :: revision_text
        integer :: ios

        next = ""
        found = .false.
        if (len(file) < 0) return
        read (revision, *, iostat=ios) newrev
        if (ios /= 0) newrev = 0
        newrev = newrev + 1
        tip_revision = -1
        call temporary_begin(old, temporary)
        call version_control_mercurial_command(0, none, strings("tip", "--style=default"), st, in_current=.true.)

        if (st%ok()) then
            call goto_character(point_minimum())
            call regular_expression_search_forward("^changeset:[ "//TAB//"]*\([0-9]+\):", position, st, noerror=SEARCH_NOERROR)

            if (position > 0) then
                digits = match_string(1)
                read (digits, *, iostat=ios) tip_revision
                if (ios /= 0) tip_revision = -1
            end if
        end if

        call temporary_end(old, temporary)

        ! We don't want to exceed the maximum possible revision number, ie
        ! the tip revision.
        if (newrev <= tip_revision) then
            write (revision_text, "(i0)") newrev
            next = trim(revision_text)
            found = .true.
        end if
    end subroutine version_control_mercurial_next_revision

    ! ------------------------------------------------------------------
    ! State-changing functions
    ! ------------------------------------------------------------------

    !> Create a new Mercurial repository.  Port of
    !> vc-hg.el:vc-hg-create-repo.
    recursive subroutine version_control_mercurial_create_repository(status)
        type(status_type), intent(out) :: status
        type(string_type) :: none(0)

        call version_control_mercurial_command(0, none, strings("init"), status)
    end subroutine version_control_mercurial_create_repository

    !> Register FILES under hg.  COMMENT is ignored.  Port of
    !> vc-hg.el:vc-hg-register.
    recursive subroutine version_control_mercurial_register(files, status, comment)
        type(string_type), intent(in) :: files(:)
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: comment

        if (present(comment)) continue
        call version_control_mercurial_command(0, files, strings("add"), status)
    end subroutine version_control_mercurial_register

    !> Unregister FILE from hg.  Port of vc-hg.el:vc-hg-unregister.
    recursive subroutine version_control_mercurial_unregister(file, status)
        character(len=*), intent(in) :: file
        type(status_type), intent(out) :: status

        call version_control_mercurial_command(0, strings(file), strings("forget"), status)
    end subroutine version_control_mercurial_unregister

    !> Delete FILE and delete it in the Mercurial repository.  Port of
    !> vc-hg.el:vc-hg-delete-file.
    recursive subroutine version_control_mercurial_delete_file(file, status)
        character(len=*), intent(in) :: file
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: root, relative, saved
        type(status_type) :: st
        logical :: found

        ! Ensure we invoke hg in the repository root because the
        ! subdirectories containing each of FILES may no longer exist.
        call version_control_mercurial_root(file, root, found)
        if (.not. found) root = buffer_directory()
        relative = file_relative_name(file, root)
        saved = buffer_directory()
        call set_buffer_directory(root)
        ! Errors deleting the file are ignored.
        call delete_file(relative, st)
        call version_control_mercurial_command(0, strings(relative), strings("remove", "--after", "--force"), status)
        call set_buffer_directory(saved)
    end subroutine version_control_mercurial_delete_file

    !> Delete each of FILES and delete them in the Mercurial repository.
    !> Should be called with `default_directory' equal to the repository root.
    !> Port of vc-hg.el:vc-hg-delete-files.
    recursive subroutine version_control_mercurial_delete_files(files, status)
        type(string_type), intent(in) :: files(:)
        type(status_type), intent(out) :: status
        type(status_type) :: st
        integer :: k

        ! Errors deleting the files are ignored.
        do k = 1, size(files)
            if (is_file_directory(files(k)%text)) then
                call delete_directory(files(k)%text, st, .true.)
            else
                call delete_file(files(k)%text, st)
            end if
        end do

        call version_control_mercurial_command(0, files, strings("remove", "--after", "--force"), status)
    end subroutine version_control_mercurial_delete_files

    !> Rename file from OLD to NEW using `hg mv'.  Port of
    !> vc-hg.el:vc-hg-rename-file.
    recursive subroutine version_control_mercurial_rename_file(old, new, status)
        character(len=*), intent(in) :: old, new
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: old_name, new_name

        ! Do the rename ourselves then update hg.  Otherwise only registered
        ! files are moved.  ('git mv' moves both registered and unregistered
        ! files which seems more useful.)
        call rename_file(old, new, status)
        if (.not. status%ok()) return
        call expand_file_name(new, new_name, status)
        if (.not. status%ok()) return
        call expand_file_name(old, old_name, status)
        if (.not. status%ok()) return
        call version_control_mercurial_command(0, strings(new_name), strings("mv", "--after", old_name), status)
    end subroutine version_control_mercurial_rename_file

    !> Get revision REVISION of FILE into BUFFER (the working revision when REVISION
    !> is "").  Port of vc-hg.el:vc-hg-find-revision.
    recursive subroutine version_control_mercurial_find_revision(file, revision, buffer, status)
        character(len=*), intent(in) :: file, revision
        type(handle_type), intent(in) :: buffer
        type(status_type), intent(out) :: status

        if (len(revision) > 0) then
            call version_control_mercurial_command(0, strings(file), strings("cat", "-r", revision), status, buffer=buffer)
        else
            call version_control_mercurial_command(0, strings(file), strings("cat"), status, buffer=buffer)
        end if
    end subroutine version_control_mercurial_find_revision

    !> Return the ignore file of the repository of FILE.  Port of
    !> vc-hg.el:vc-hg-find-ignore-file.
    subroutine version_control_mercurial_find_ignore_file(file, ignore, found)
        character(len=*), intent(in) :: file
        character(len=:), allocatable, intent(out) :: ignore
        logical, intent(out) :: found
        character(len=:), allocatable :: root
        type(status_type) :: st

        call version_control_mercurial_root(file, root, found)
        if (.not. found) root = buffer_directory()
        call expand_file_name(".hgignore", ignore, st, root)
        found = st%ok()
    end subroutine version_control_mercurial_find_ignore_file

    !> Retrieve a revision of FILE.
    !> REVISION is the revision to check out into the buffer of FILE (or the
    !> current buffer).  Port of vc-hg.el:vc-hg-checkout (modeled after the
    !> similar function in vc-bzr.el).
    recursive subroutine version_control_mercurial_checkout(file, status, revision)
        character(len=*), intent(in) :: file
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: revision
        type(handle_type) :: old, buffer
        type(status_type) :: st

        old = current_buffer()
        buffer = get_file_buffer(file)
        if (is_buffer_live(buffer)) call set_buffer(buffer, st)

        if (present(revision)) then
            call version_control_mercurial_command(0, strings(file), strings("cat", "-r", revision), status, in_current=.true.)
        else
            call version_control_mercurial_command(0, strings(file), strings("cat"), status, in_current=.true.)
        end if

        call set_buffer(old, st)
    end subroutine version_control_mercurial_checkout

    !> Revert FILE (`hg revert' with `version_control_mercurial_revert_switches') unless
    !> CONTENTS_DONE.  Port of vc-hg.el:vc-hg-revert (modeled after the
    !> similar function in vc-bzr.el).
    recursive subroutine version_control_mercurial_revert(file, status, contents_done)
        character(len=*), intent(in) :: file
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: contents_done

        status = status_type()
        if (present(contents_done)) return
        call version_control_mercurial_revert_files(strings(file), status)
    end subroutine version_control_mercurial_revert

    !> Revert FILES (`hg revert' with `version_control_mercurial_revert_switches').  Port of
    !> vc-hg.el:vc-hg-revert-files.
    recursive subroutine version_control_mercurial_revert_files(files, status)
        type(string_type), intent(in) :: files(:)
        type(status_type), intent(out) :: status
        type(string_type), allocatable :: flags(:)
        type(handle_type) :: old, temporary

        flags = strings("revert")
        call append_switches(flags, version_control_mercurial_revert_switches)
        call temporary_begin(old, temporary)
        call version_control_mercurial_command(0, files, flags, status, in_current=.true.)
        call temporary_end(old, temporary)
    end subroutine version_control_mercurial_revert_files

    ! ------------------------------------------------------------------
    ! Checking in
    ! ------------------------------------------------------------------

    ! The message of the last commit, for `version_control_mercurial_log_edit_toggle_amend'.
    ! Port of the message function vc-hg.el:vc-hg-log-edit-toggle-amend gives.
    recursive subroutine last_message(message, status)
        character(len=:), allocatable, intent(out) :: message
        type(status_type), intent(out) :: status
        type(string_type) :: none(0)
        type(handle_type) :: old, temporary

        call temporary_begin(old, temporary)
        call version_control_mercurial_command(1, none, strings("log", "--limit=1", "--template", "{desc}"), status, &
                                               in_current=.true.)
        message = buffer_string()
        call temporary_end(old, temporary)
    end subroutine last_message

    !> Toggle whether this will amend the previous commit.
    !> If toggling on, also insert its message into the buffer.  Port of
    !> vc-hg.el:vc-hg-log-edit-toggle-amend.
    recursive subroutine version_control_mercurial_log_edit_toggle_amend(status)
        type(status_type), intent(out) :: status

        call log_edit_toggle_amend(last_message, status)
    end subroutine version_control_mercurial_log_edit_toggle_amend

    !> Major mode for editing Hg log messages.
    !> It is based on `log_edit_mode', and has Hg-specific extensions.
    !> Port of vc-hg.el:vc-hg-log-edit-mode.
    recursive subroutine version_control_mercurial_log_edit_mode(status)
        type(status_type), intent(out) :: status
        logical :: saved

        if (.not. associated(major_modes%version_control_mercurial_log_edit_mode%execute)) call define_log_edit_mode()
        call major_modes%version_control_mercurial_log_edit_mode%begin(saved, status)
        if (.not. status%ok()) return
        call major_modes%version_control_mercurial_log_edit_mode%end(saved, status)
    end subroutine version_control_mercurial_log_edit_mode

    ! Define Hg's Log Edit mode and its keymap.  Not a port.
    subroutine define_log_edit_mode()
        type(status_type) :: st

        if (.not. associated(major_modes%log_edit_mode%execute)) call initialize_log_edit()
        call major_modes%version_control_mercurial_log_edit_mode%define("version_control_mercurial_log_edit_mode", "Log-Edit/hg", &
                                                                        version_control_mercurial_log_edit_mode, &
                                                     version_control_mercurial_log_edit_mode_hook, parent=major_modes%log_edit_mode)
        version_control_mercurial_log_edit_mode_map = major_modes%version_control_mercurial_log_edit_mode%map
        commands%version_control_mercurial_log_edit_mode%execute => version_control_mercurial_log_edit_mode
        commands%version_control_mercurial_log_edit_toggle_amend%execute => version_control_mercurial_log_edit_toggle_amend
        call keymap_set(version_control_mercurial_log_edit_mode_map, "C-c C-e", "version_control_mercurial_log_edit_toggle_amend", &
                        st)
    end subroutine define_log_edit_mode

    ! The argument "--amend" if the Amend header's VALUE is "yes".
    ! Not a port.
    subroutine amend_argument(value, arguments)
        character(len=*), intent(in) :: value
        type(string_type), allocatable, intent(out) :: arguments(:)

        if (value == "yes") then
            allocate (arguments(1))
            arguments(1)%text = "--amend"
        else
            allocate (arguments(0))
        end if
    end subroutine amend_argument

    ! The log message MESSAGE of COMMENT and the command line arguments ARGUMENTS
    ! of its headers.  Port of vc-hg.el:vc-hg--extract-headers.
    recursive subroutine extract_headers(comment, message, arguments, status)
        character(len=*), intent(in) :: comment
        character(len=:), allocatable, intent(out) :: message
        type(string_type), allocatable, intent(out) :: arguments(:)
        type(status_type), intent(out) :: status
        type(log_edit_header_type) :: headers(3)

        headers(1)%name = "Author"
        headers(1)%cmdarg = "--user"
        headers(2)%name = "Date"
        headers(2)%cmdarg = "--date"
        headers(3)%name = "Amend"
        headers(3)%kind = LOG_EDIT_HEADER_FN
        headers(3)%fn => amend_argument
        call log_edit_extract_headers(headers, comment, message, arguments, status)
    end subroutine extract_headers

    !> Hg-specific version of `version_control_backend_checkin'.
    !> REVISION is ignored.  Port of vc-hg.el:vc-hg-checkin with
    !> vc-hg.el:vc-hg--checkin (checking in a patch, the asynchronous
    !> commit of `version_control_asynchronous_checkin' and the message file of MS-Windows are
    !> deferred or not applicable).
    recursive subroutine version_control_mercurial_checkin(files, comment, status, revision)
        type(string_type), intent(in) :: files(:)
        character(len=*), intent(in) :: comment
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: revision
        type(string_type), allocatable :: extracted(:)
        type(string_type), allocatable :: arguments(:)
        character(len=:), allocatable :: message
        integer :: k

        status = status_type()
        if (present(revision)) continue

        if (size(files) == 0) then
            call set_error(status, GENERIC_ERROR_CODE, "Invalid call to `version_control_mercurial_checkin'")
            return
        end if

        call extract_headers(comment, message, extracted, status)
        if (.not. status%ok()) return
        arguments = strings("commit", "-A", "-m", message)

        do k = 1, size(extracted)
            call append_string(arguments, extracted(k)%text)
        end do

        call version_control_mercurial_command(0, files, arguments, status)
    end subroutine version_control_mercurial_checkin

    ! ------------------------------------------------------------------
    ! VC-Dir
    ! ------------------------------------------------------------------

    !> Pretty-printer for the vc-dir-fileinfo structure.  Port of
    !> vc-hg.el:vc-hg-dir-printer.
    recursive subroutine version_control_mercurial_directory_printer(info)
        use version_control_directory_mod, only: version_control_default_directory_printer, insert_with_face
        type(version_control_directory_fileinfo_type), intent(in) :: info
        character(len=:), allocatable :: rename_state

        call version_control_default_directory_printer(info)
        if (.not. info%entry%has_extra) return

        if (info%entry%rename_state == VERSION_CONTROL_RENAME_COPY) then
            rename_state = "copied from"
        else if (info%entry%state == VERSION_CONTROL_REMOVED) then
            rename_state = "renamed to"
        else
            rename_state = "renamed from"
        end if

        call insert_with_face("   ("//rename_state//" "//info%entry%original_name//")", "font_lock_comment_face")
    end subroutine version_control_mercurial_directory_printer

    !> Pass the states of `hg status -C' in the current buffer to UPDATE
    !> for the VC-Dir buffer BUFFER.  The extra information of a copy is
    !> VERSION_CONTROL_RENAME_COPY with its original name (`copied'); of a rename,
    !> VERSION_CONTROL_RENAME_RENAME with the original name on the added file
    !> (`renamed-from') and the new name on the removed one
    !> (`renamed-to').  Port of vc-hg.el:vc-hg-after-dir-status.
    recursive subroutine version_control_mercurial_after_directory_status(update, buffer)
        procedure(version_control_directory_update_procedure) :: update
        type(handle_type), intent(in) :: buffer
        type(version_control_directory_entry_type), allocatable :: result(:), reversed(:)
        type(version_control_directory_entry_type) :: entry
        type(string_type), allocatable :: copy_names(:)
        integer, allocatable :: copy_entries(:)
        character(len=:), allocatable :: line, file
        integer(int64) :: shortage
        type(status_type) :: st
        integer :: k, last, n

        allocate (result(0), copy_names(0), copy_entries(0))
        call goto_character(point_minimum())

        do while (.not. is_end_of_buffer())
            call buffer_substring(point(), position_end_of_line(), line, st)
            file = ""
            if (len(line) > 2) file = line(3:)
            entry = version_control_directory_entry_type()
            entry%name = file

            if (len(line) == 0) then
                entry%state = VERSION_CONTROL_STATE_UNKNOWN
                result = [result, entry]
            else if (line(1:1) == " ") then
                ! For copied files the output looks like this:
                !     A COPIED_FILE_NAME
                !       ORIGINAL_FILE_NAME
                ! For renamed files the output looks like this:
                !     A NEW_FILE_NAME
                !       ORIGINAL_FILE_NAME
                !     R ORIGINAL_FILE_NAME
                ! but the last line can come arbitrarily later in the output.
                ! So we have to remember the entry for the copy in RESULT to
                ! potentially modify later.
                last = size(result)

                if (last > 0) then
                    result(last)%has_extra = .true.
                    result(last)%rename_state = VERSION_CONTROL_RENAME_COPY
                    result(last)%original_name = file
                    k = copy_index(file)

                    if (k == 0) then
                        call append_string(copy_names, file)
                        copy_entries = [copy_entries, last]
                    else
                        copy_entries(k) = last
                    end if
                end if
            else
                entry%state = translate(line(1:1))
                k = 0
                if (entry%state == VERSION_CONTROL_REMOVED) k = copy_index(file)

                if (k > 0) then
                    last = copy_entries(k)
                    ! `renamed-from' on the copy.
                    result(last)%rename_state = VERSION_CONTROL_RENAME_RENAME
                    ! `renamed-to' on the removed file.
                    entry%has_extra = .true.
                    entry%rename_state = VERSION_CONTROL_RENAME_RENAME
                    entry%original_name = result(last)%name
                end if

                result = [result, entry]
            end if

            shortage = forward_line(1_int64)
        end do

        ! The entries were pushed: the last one comes first.
        n = size(result)
        allocate (reversed(n))

        do k = 1, n
            reversed(k) = result(n + 1 - k)
        end do

        call update(reversed, buffer, .false.)

    contains

        ! The index in COPY_NAMES of NAME (0 if none).
        integer function copy_index(name) result(i)
            character(len=*), intent(in) :: name

            do i = 1, size(copy_names)
                if (copy_names(i)%text == name) return
            end do

            i = 0
        end function copy_index

        ! The state of the status character C (none for an unknown one).
        integer function translate(c) result(state)
            character, intent(in) :: c

            select case (c)
            case ("=", "C")
                state = VERSION_CONTROL_UP_TO_DATE
            case ("A")
                state = VERSION_CONTROL_ADDED
            case ("R")
                state = VERSION_CONTROL_REMOVED
            case ("M")
                state = VERSION_CONTROL_EDITED
            case ("I")
                state = VERSION_CONTROL_IGNORED
            case ("!")
                state = VERSION_CONTROL_MISSING
            case ("?")
                state = VERSION_CONTROL_UNREGISTERED
            case default
                state = VERSION_CONTROL_STATE_UNKNOWN
            end select
        end function translate
    end subroutine version_control_mercurial_after_directory_status

    ! The action run after the `hg status' of `version_control_mercurial_directory_status_files':
    ! `version_control_mercurial_after_directory_status' in the process buffer.  Port of the
    ! delayed part of vc-hg.el:vc-hg-dir-status-files.
    recursive subroutine after_directory_status_action(action, status)
        use version_control_dispatcher_mod, only: version_control_action_type
        type(version_control_action_type), intent(in) :: action
        type(status_type), intent(out) :: status
        type(handle_type) :: old
        type(status_type) :: st
        integer :: k

        status = status_type()
        k = action%n
        directory_states(k)%live = .false.
        if (.not. is_buffer_live(action%buffer)) return
        old = current_buffer()
        call set_buffer(action%buffer, st)
        call version_control_mercurial_after_directory_status(directory_states(k)%update_function, directory_states(k)%buffer)
        if (is_buffer_live(old)) call set_buffer(old, st)
    end subroutine after_directory_status_action

    !> Compute the states of FILES (all when empty) in DIRECTORY asynchronously
    !> and pass them to UPDATE for BUFFER.  Port of
    !> vc-hg.el:vc-hg-dir-status-files.
    recursive subroutine version_control_mercurial_directory_status_files(directory, files, update, buffer, status)
        use version_control_dispatcher_mod, only: version_control_action_type, version_control_exec_after
        character(len=*), intent(in) :: directory
        type(string_type), intent(in) :: files(:)
        procedure(version_control_directory_update_procedure) :: update
        type(handle_type), intent(in) :: buffer
        type(status_type), intent(out) :: status
        type(directory_status_type) :: state
        type(version_control_action_type) :: action
        type(string_type), allocatable :: flags(:)
        character(len=:), allocatable :: saved, options
        integer :: k

        if (.not. allocated(directory_states)) allocate (directory_states(0))

        do k = 1, size(directory_states)
            if (.not. directory_states(k)%live) exit
        end do

        if (k > size(directory_states)) directory_states = [directory_states, state]
        directory_states(k)%live = .true.
        directory_states(k)%update_function => update
        directory_states(k)%buffer = buffer
        ! XXX: We can't pass DIRECTORY directly to 'hg status' because that
        ! returns all ignored files if FILES is not empty (bug#22481).
        saved = buffer_directory()
        call set_buffer_directory(directory)
        options = "-mardu"
        if (size(files) > 0) options = options//"i"
        flags = strings("status", options, "-C")

        ! Invalid version syntax, an error condition upstream, counts as
        ! a lower version.
        if (version("4.2") <= version(version_control_mercurial_program_version())) then
            call append_all(flags, strings("--config", "commands.status.relative=1"))
        else
            call append_all(flags, strings("re:", "-I", "."))
        end if

        call version_control_mercurial_command(VERSION_CONTROL_OKSTATUS_ASYNCHRONOUS, files, flags, status, in_current=.true., &
                                               stderr=.false.)
        call set_buffer_directory(saved)

        if (.not. status%ok()) then
            directory_states(k)%live = .false.
            return
        end if

        ! (vc-run-delayed-success 0 ...)
        action%execute => after_directory_status_action
        action%n = k
        action%buffer = current_buffer()
        action%okstatus = 0
        call version_control_exec_after(action, status)
    end subroutine version_control_mercurial_directory_status_files

    ! Append TEXT with FACE to PARTS.  Not a port.
    subroutine append_part(parts, text, face)
        type(fortmacs_widget_for_object_collections_part_type), allocatable, intent(inout) :: parts(:)
        character(len=*), intent(in) :: text, face
        type(fortmacs_widget_for_object_collections_part_type) :: part

        part%text = text
        part%face = face
        parts = [parts, part]
    end subroutine append_part

    !> Generate extra status headers for a repository in DIRECTORY.
    !> This runs the command "hg summary".  Port of
    !> vc-hg.el:vc-hg-dir-extra-headers.
    recursive subroutine version_control_mercurial_directory_extra_headers(directory, parts)
        use casefiddle_mod, only: capitalize_string
        character(len=*), intent(in) :: directory
        type(fortmacs_widget_for_object_collections_part_type), allocatable, intent(out) :: parts(:)
        type(string_type) :: none(0)
        type(handle_type) :: old, temporary
        type(status_type) :: st
        character(len=:), allocatable :: key, value
        character(len=11) :: padded
        integer(int64) :: shortage

        allocate (parts(0))
        call temporary_begin(old, temporary)
        call set_buffer_directory(directory)
        call version_control_mercurial_command(0, none, strings("summary"), st, in_current=.true.)
        call goto_character(point_minimum())

        do while (.not. is_end_of_buffer())
            if (looking_at("\([^ ].*\): \(.*\)", st)) then
                key = capitalize_string(match_string(1))
                value = match_string(2)
            else
                key = ""
                call buffer_substring(point(), position_end_of_line(), value, st)
            end if

            if (size(parts) > 0) call append_part(parts, LF, "")

            if (len(key) >= 11) then
                call append_part(parts, key//": ", "version_control_directory_header")
            else
                padded = key
                call append_part(parts, padded//": ", "version_control_directory_header")
            end if

            call append_part(parts, value, "version_control_directory_header_value")
            shortage = forward_line(1_int64)
        end do

        call temporary_end(old, temporary)
    end subroutine version_control_mercurial_directory_extra_headers

    ! ------------------------------------------------------------------
    ! Initialization
    ! ------------------------------------------------------------------

    !> Make the Hg backend known to VERSION_CONTROL.  Not a port.
    subroutine initialize_version_control_mercurial()
        integer :: k

        if (.not. allocated(version_control_mercurial_program)) version_control_mercurial_program = "hg"
        call initialize_log_formats()
        call define_log_edit_mode()
        call define_log_view_mode()
        call register_version_control_backend("Hg", k)
        if (k == 0) return

        associate (b => version_control_backends(k))
            b%registered => version_control_mercurial_registered
            b%state => version_control_mercurial_state
            b%working_revision => version_control_mercurial_working_revision
            b%working_revision_symbol => version_control_mercurial_working_revision_symbol
            b%mode_line_string => version_control_mercurial_mode_line_string
            b%root => version_control_mercurial_root
            ! (defalias 'vc-hg-responsible-p #'vc-hg-root)
            b%responsible_p => version_control_mercurial_root
            b%find_ignore_file => version_control_mercurial_find_ignore_file
            b%checkout_model => version_control_mercurial_checkout_model
            b%register => version_control_mercurial_register
            b%unregister => version_control_mercurial_unregister
            b%revert => version_control_mercurial_revert
            b%revert_files => version_control_mercurial_revert_files
            b%diff => version_control_mercurial_diff
            b%diff_switches => version_control_mercurial_diff_switches
            b%find_revision => version_control_mercurial_find_revision
            b%checkout => version_control_mercurial_checkout
            b%delete_file => version_control_mercurial_delete_file
            b%delete_files => version_control_mercurial_delete_files
            b%rename_file => version_control_mercurial_rename_file
            b%previous_revision => version_control_mercurial_previous_revision
            b%next_revision => version_control_mercurial_next_revision
            b%revision_completion_table => version_control_mercurial_revision_table
            b%create_repository => version_control_mercurial_create_repository
            b%checkin => version_control_mercurial_checkin
            b%log_edit_mode => version_control_mercurial_log_edit_mode
            b%print_log => version_control_mercurial_print_log
            b%log_view_mode => version_control_mercurial_log_view_mode
            b%directory_status_files => version_control_mercurial_directory_status_files
            b%directory_extra_headers => version_control_mercurial_directory_extra_headers
            b%directory_printer => version_control_mercurial_directory_printer
        end associate
    end subroutine initialize_version_control_mercurial
end module version_control_mercurial_mod
